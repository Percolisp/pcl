#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# facts-overlay-01.t -- task #2878: THE FACTS OVERLAY.
#
# A sub, or a prototype, that only BEGIN-time CODE installs (a glob-assign
# loop, an eval-built sub, a computed export list) is invisible to a static
# parse.  The fact is supplied by a per-module file of FORWARD DECLARATIONS,
# the module PCL::Facts::<Module>, resolved under every root of the search
# list.  The fixture Pl/t/lib/FactsFixture.pm installs everything by running
# code; its overlay Pl/t/lib/PCL/Facts/FactsFixture.pm states the facts.
#
# Rows 1, 3, 4 and 5 FAIL on the base (no overlay reader): the constant below
# the `use` read as a list-operator call, the `:all` block form dropped, the
# export-only name read as a string, the module's own constant misread.
# Every expectation is the live `perl` answer.  docs/shipped-modules.md
# "Facts overlays".

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my $fixlib       = "$RealBin/lib";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 18;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub both_agree {
    my ($code, $desc) = @_;
    $code = "use lib '$fixlib';\n$code";
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# 1. Below the `use`, the overlay's `()` makes ZERO a nullary constant (ZERO,
# not ONE: as a list operator `ONE(+1 == 2)` would still answer true).
both_agree(<<'PL', 'a `()` constant BELOW the use is a constant');
use FactsFixture qw(ZERO);
if (ZERO + 1 == 1) { print "one\n" } else { print "not\n" }
PL

# 2. Above the `use` the name is unknown to perl: the entry is POSITIONAL,
# so the bareword is the STRING there and the constant below.
both_agree(<<'PL', 'the same constant ABOVE the use stays a bareword string');
no strict;
no warnings;
my $s = ONE;
use FactsFixture qw(ONE);
my $t = ONE;
print "$s $t\n";
PL

# 3. `:all` expands through the overlay's %EXPORT_TAGS; blk is (&;@).
both_agree(<<'PL', 'a (&;@) block form imported through the overlay tag');
use FactsFixture ':all';
my $r = blk { 42 } 1, 2;
print "$r\n";
PL

# 4. greet is callable only because the overlay's @EXPORT names it.
both_agree(<<'PL', 'a name made callable only by the overlay export list');
use FactsFixture;
print join(",", greet, 2), "\n";
PL

# 5. The module's OWN unit: `ONE + 1` inside FactsFixture.pm.
both_agree(<<'PL', "the module's own use of its constant");
use FactsFixture ();
print FactsFixture::own_test(), "\n";
PL

# 6. A conflict between overlay and source dies naming BOTH files.
{
    my $pl = write_pl("use lib '$fixlib';\nuse FactsConflict;\nprint 1;\n");
    my $err = `$pl2cl $pl 2>&1 >/dev/null`;
    ok($? != 0 && $err =~ m{conflict for FactsConflict::CONST}
               && $err =~ m{PCL/Facts/FactsConflict\.pm}
               && $err =~ m{lib/FactsConflict\.pm declares \(\)},
       'a conflicting overlay dies naming both files')
      or diag $err;
}

# 6b. The same conflict on the UNIT path (#2954): transpiling the module file
# ITSELF dies too.  Its check used to run before the unit's own subs were
# registered and never fired (the cloud review's finding 2).
{
    my $err = `PCL_NO_PROTO_CACHE=1 $pl2cl $fixlib/FactsConflict.pm 2>&1 >/dev/null`;
    ok($? != 0 && $err =~ m{conflict for FactsConflict::CONST}
               && $err =~ m{PCL/Facts/FactsConflict\.pm declares \(\$\)}
               && $err =~ m{lib/FactsConflict\.pm declares \(\)},
       'a conflicting overlay dies on the UNIT path too, naming both files')
      or diag $err;
}

# 7. A sub WITH A BODY in an overlay dies naming the file and line.
{
    my $pl = write_pl("use lib '$fixlib';\nuse FactsBody;\nprint 1;\n");
    my $err = `$pl2cl $pl 2>&1 >/dev/null`;
    ok($? != 0 && $err =~ m{facts overlay \S*PCL/Facts/FactsBody\.pm line 8: .*sub y \{ 2 \}},
       'an overlay holding code dies naming file and line')
      or diag $err;
}

# 8-10: the reader, in-process.
{
    local @INC = ("$project_root", @INC);
    require Pl::Parser;
    require Pl::Environment;
    require Pl::ProtoCache;
    require Data::Dumper;
    my $p = Pl::Parser->new(code => "1;", environment => Pl::Environment->new,
                            inc_paths => [$fixlib]);
    my $facts = sub {
        my ($env) = @_;
        local $Data::Dumper::Sortkeys = 1;
        local $Data::Dumper::Indent = 0;
        my %p = map { my %r = %{ $env->prototypes->{$_} }; delete $r{at}; ($_ => \%r) }
                keys %{ $env->prototypes };
        return Data::Dumper::Dumper([\%p, $env->export_names, $env->import_sets]);
    };
    # 8. A module with no overlay: its facts are exactly its source walk's.
    local $ENV{PCL_NO_PROTO_CACHE} = 1;
    my $via = $p->_extract_module_prototypes('FactsPlain');
    my $walk = $p->_walk_module_prototypes('FactsPlain', "$fixlib/FactsPlain.pm");
    ok(!defined $p->_facts_overlay_path('FactsPlain')
       && $facts->($via) eq $facts->($walk),
       'a module without an overlay: facts identical to its source walk');

    # 9. The ProtoCache key carries the overlay's BYTES (or its absence).
    my $dir = tempdir(CLEANUP => 1);
    my $ov = "$dir/ov.pm";
    open my $fh, '>', $ov or die; print $fh "package X;\nsub a ();\n1;\n"; close $fh;
    my $k1 = Pl::ProtoCache::_key('/m.pm', 1, 2, $ov);
    open $fh, '>', $ov or die; print $fh "package X;\nsub a ($);\n1;\n"; close $fh;
    my $k2 = Pl::ProtoCache::_key('/m.pm', 1, 2, $ov);
    my $k0 = Pl::ProtoCache::_key('/m.pm', 1, 2, undef);
    ok($k1 ne $k2 && $k0 ne $k1 && $k0 ne $k2,
       'the ProtoCache key differs when the overlay bytes differ, and without one');

    # 10. The merge: the overlay fills absences and unions the export data.
    my $env = $p->_extract_module_prototypes('FactsFixture');
    ok($env->prototypes->{blk} && ($env->prototypes->{blk}{proto_string} // '') eq '&;@'
       && $env->export_names->{greet}
       && grep({ $_ eq 'blk' } @{ $env->import_sets->{':all'} || [] }),
       'the overlay facts are merged into the module facts');
}

# 12-18: THE IMPORT-DIRECTIVE FACT (task #4881, ruled s541).  The fixture
# Pl/t/lib/DirectiveFix.pm's import_from(MODULE, LIST) installs MODULE's
# names into its caller -- plain Perl, Test::More's use_ok shape -- and its
# overlay declares `sub import_from :pcl_import_directive;` (the source
# declares the same prototype-less sub: the overlay ADDS the fact).  A call of
# it directly in a BEGIN body is `use MODULE LIST` for the parse, so a `()`
# constant it imports is a TERM after it.  Rows 12-15 and 18 FAIL on the base
# (the constant read as a list-operator call that swallowed `+ 1 ..` /
# `- 1, 7`).  Row 17 is the negative: outside a BEGIN nothing changes.

# 12. The parenthesised call, no list: DirectiveConsts' @EXPORT (TWO).
both_agree(<<'PL', 'BEGIN { DIRECTIVE(LIT) }: an imported () constant is a term');
use DirectiveFix qw(import_from);
BEGIN { import_from('DirectiveConsts') }
my $n = 0;
for (TWO + 1 .. 5) { $n++ }
print "$n\n";
PL

# 13. The paren-less call with an explicit list.
both_agree(<<'PL', 'BEGIN { DIRECTIVE LIT, LIST }: the paren-less form');
use DirectiveFix qw(import_from);
BEGIN { import_from 'DirectiveConsts', qw(THREE) }
print THREE + 1, "\n";
PL

# 14. The call as the first operand of `||` (always run).
both_agree(<<'PL', 'BEGIN { DIRECTIVE(LIT, LIST) || die }: the call is still a use');
use DirectiveFix qw(import_from);
BEGIN { import_from('DirectiveConsts', 'TWO') || die "no" }
my @l = (TWO - 1, 7);
print "@l\n";
PL

# 15. A NON-literal module name is computed: no parse effect, and no die.
both_agree(<<'PL', 'BEGIN { DIRECTIVE($var) }: no effect and no die');
use DirectiveFix qw(import_from);
my $m;
BEGIN { $m = 'DirectiveConsts'; import_from($m, 'TWO') }
print TWO() + 1, "\n";
PL

# 16. The real shape: Test::More's use_ok carries the fact (lib/Test/More.pm).
both_agree(<<'PL', 'BEGIN { use_ok(LIT, LIST) } imports for the parse');
use Test::More tests => 2;
BEGIN { use_ok('DirectiveConsts', 'TWO') }
my $n = 0;
$n++ for TWO + 1 .. 5;
is($n, 3, 'the loop runs from TWO + 1');
PL

# 17. OUTSIDE a BEGIN the call runs after the parse, so the parse sees no
# import (perl reads TWO as an unknown word there).  The compiler's own record
# of the call site -- the #2610 detector table in the emission -- says how it
# read TWO: unknown after the plain call, a `()` term after the BEGIN form.
{
    my $src = "use lib '$fixlib';\nuse DirectiveFix qw(import_from);\n%s\n"
            . "my \@l = (TWO, 7);\nprint \"\@l\\n\";\n";
    my $plain = PCLCore::transpile("$pl2cl " . write_pl(sprintf $src,
                  "import_from('DirectiveConsts', 'TWO');"));
    my $begin = PCLCore::transpile("$pl2cl " . write_pl(sprintf $src,
                  "BEGIN { import_from('DirectiveConsts', 'TWO') }"));
    ok($plain =~ /\("main" "TWO" 4 "unknown"\)/
       && $begin =~ /\("main" "TWO" 4 "proto:"\)/,
       'outside a BEGIN the directive call leaves the parse alone')
      or diag "plain: " . join("|", $plain =~ /(\("main" "TWO"[^)]*\))/g)
            . " begin: " . join("|", $begin =~ /(\("main" "TWO"[^)]*\))/g);
}

# 18. The no-list use_ok imports the module's default list.
both_agree(<<'PL', 'BEGIN { use_ok(LIT) }: the default import list');
use Test::More tests => 2;
BEGIN { use_ok('DirectiveConsts') }
my @l = (TWO - 1, 7);
is("@l", "1 7", 'TWO is a term');
PL

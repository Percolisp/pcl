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

plan tests => 10;

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

# 2. Above the `use` the name is unknown to perl: the entry is POSITIONAL.
# Asserted on the EMISSION: the statement above the `use` lowers exactly as
# it does with no `use` at all, and differently from the same statement
# below it.  (Not a perl-output row: PCL calls a sub that exists at run time
# for an unknown bareword in this slot even for a plain module's constant --
# task #3021, filed s513g.)
{
    my $stmt = 'my $s = ONE + 1;';
    my $emit = sub {
        my ($code) = @_;
        my $cl = `$pl2cl @{[ write_pl("use lib '$fixlib';\nno strict;\n$code") ]} 2>/dev/null`;
        return $cl =~ /(\(p-\+ \(p-scalar-ctx \(pl-ONE\)\) 1\)|\(pl-ONE [^()]*\))/ ? $1 : '';
    };
    my $above = $emit->("$stmt\nuse FactsFixture qw(ONE);\n");
    my $none  = $emit->("$stmt\n");
    my $below = $emit->("use FactsFixture qw(ONE);\n$stmt\n");
    ok(length($above) && $above eq $none && $above ne $below,
       'the constant ABOVE the use lowers as if the use were absent')
      or diag "above: $above\nnone:  $none\nbelow: $below";
}

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

#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# feature-pragma-01.t — the core feature-enabling pragmas (#360).
#
# Whether `try` / `signatures` are IN EFFECT at a token is PPI's decision, and
# PPI got two of perl's three spellings wrong: `use experimental 'try'` came
# back as `signatures => 0`, and a VERSION BUNDLE (`use v5.40`) answered
# signatures only — so `use v5.40; try {…} catch ($e) {…}` lexed like the
# no-feature case and the whole statement was DROPPED (announced, rc 0).
# `Pl::Parser::_pcl_feature_include_cb` is the table that answers instead,
# through PPI's own `custom_feature_include_cb` hook.
#
# Three layers here, and each catches a different kind of drift:
#
#   1. THE TABLE vs the RUNNING PERL.  The bundle thresholds are static on
#      purpose — PCL must compile `use v5.40; try` the same way whatever perl
#      it runs under — so they are re-derived here from `%feature::feature_bundle`
#      and a perl that disagrees fails a row instead of drifting silently.
#   2. THE CALLBACK's answers, per spelling, including the `no` forms.
#   3. END TO END vs perl: the same source run both ways.  This is the layer
#      that would have caught #360 in the first place; the callback can be
#      right and the lowering still wrong.
#
# The INVERSE matters as much as the feature: with `try` off, `try {…} catch
# {…};` is Try::Tiny's ordinary sub call and must stay one.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use lib "$RealBin/../..";
use PCLCore;
use PPI;
use Pl::Parser;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 32;

# ---- 1. the table is perl's -----------------------------------------------

SKIP: {
    no warnings 'once';   # feature.pm is required, so the hash is read-only here
    my %bundle = do { local $@; eval { require feature; %feature::feature_bundle } };
    skip "this perl exposes no %feature::feature_bundle", 2 if !%bundle;
    for my $feat (qw(signatures try)) {
        # The lowest NUMBERED bundle that contains the feature — "all" and the
        # unnumbered aliases are not thresholds.
        my @have = sort { $a <=> $b }
                   map  { /^([0-9]+\.[0-9]+)$/ ? $1 : () }
                   grep { grep { $_ eq $feat } @{ $bundle{$_} } } keys %bundle;
        skip "this perl has no bundle carrying $feat", 1 if !@have;
        # 5.39 -> 5.039: perl spells its bundles major.minor, the table
        # spells the same version as one comparable number.
        my ($maj, $min) = split /\./, $have[0];
        is(sprintf("%.3f", $Pl::Parser::PCL_FEATURE_BUNDLE{$feat}),
           sprintf("%.3f", $maj + $min / 1000),
           "$feat threshold matches this perl's lowest bundle ($have[0])");
    }
}

# ---- 2. the callback, per spelling ----------------------------------------

sub mods_for {
    my ($src) = @_;
    my $doc = PPI::Document->new(\$src,
        custom_feature_include_cb => \&Pl::Parser::_pcl_feature_include_cb);
    my ($inc) = @{ $doc->find('PPI::Statement::Include') || [] } or return undef;
    return $inc->feature_mods;
}

is_deeply(mods_for('use v5.40;'),  { signatures => 'perl', try => 'perl' },
    'use v5.40 enables try AND signatures');
is_deeply(mods_for('use v5.36;'),  { signatures => 'perl', try => 0 },
    'use v5.36 enables signatures and leaves try OFF (its bundle has no try)');
is_deeply(mods_for('use 5.036;'),  { signatures => 'perl', try => 0 },
    'the decimal spelling of a bundle answers the same');
is_deeply(mods_for('use 5.010;'),  { signatures => 0, try => 0 },
    'an old bundle turns both OFF — a version bundle REPLACES the feature set');
is_deeply(mods_for(q{use feature 'try';}), { try => 'perl' },
    'use feature try');
is_deeply(mods_for(q{use experimental 'try';}), { try => 'perl' },
    'use experimental try (PPI alone answered signatures => 0 here)');
is_deeply(mods_for('use experimental qw(signatures try);'),
    { signatures => 'perl', try => 'perl' }, 'both, in one list');
is_deeply(mods_for(q{no feature 'try';}), { try => 0 }, 'no feature try');
is_deeply(mods_for(q{no experimental 'try';}), { try => 0 }, 'no experimental try');
is(mods_for('use strict;'), undef, 'an unrelated pragma is left to PPI');
is(mods_for('require v5.40;'), undef, 'require enables nothing');
is_deeply(mods_for(q{use experimental 'defer';}), {},
    'a feature outside the table changes NOTHING — falling through to PPI here '
  . 'would answer signatures => 0 and silently disable signatures');

# ---- 3. end to end, against perl -------------------------------------------

sub run_cl {
    my ($pl_file) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl $pl_file");
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

# The ORACLE is the host perl.  Two facts about the HOST, not about PCL, can
# leave a row without one (the stock-machine rehearsal, s440: CI's perl is
# 5.38): the program's `use v5.NN` is newer than the host perl, or the
# program's CPAN fixture (Try::Tiny) is not installed.  A row that needs a
# newer perl carries the output PROBED on one ($probed, perl 5.40.3, s440)
# and is compared against that -- the #360 shape stays asserted on every
# host; a row whose fixture is missing SKIPS, naming the module (CI installs
# the fixture, so the row runs there).
sub test_src {
    my ($name, $code, $probed) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $perl_out = `perl $pl_file 2>&1`;
    my $expected = $perl_out;
    my $via      = 'perl';
    if ($perl_out =~ /^Perl v([\d.]+) required--this is only v([\d.]+)/m) {
        if (!defined $probed) {
            SKIP: { skip "$name: the host perl is $2, the program needs $1, and no probed output is given", 1 }
            return;
        }
        ($expected, $via) = ($probed, "the output probed on perl >= $1 (host perl is $2)");
    }
    elsif ($perl_out =~ /^Can't locate (\S+)\.pm in \@INC/m) {
        (my $mod = $1) =~ s{/}{::}g;
        SKIP: { skip "$name: $mod not installed (the CPAN module is this row's oracle fixture)", 1 }
        return;
    }
    my $cl_out = run_cl($pl_file);
    is($cl_out, $expected, $name) or diag("$via: [$expected]\nPCL:  [$cl_out]");
}

my $TRY = qq{try { die "boom\\n" } catch (\$e) { print "caught: \$e" }\nprint "after\\n";\n};

test_src('use v5.40 + try/catch — the #360 shape, a whole-statement DROP before',
    qq{use v5.40;\nno warnings;\n$TRY}, "caught: boom\nafter\n");
test_src('use experimental "try" + try/catch',
    qq{use experimental 'try';\nno warnings;\n$TRY});
test_src('use feature "try" + try/catch (the spelling that already worked)',
    qq{use feature 'try';\nno warnings;\n$TRY});

# The INVERSE: with the feature off, `try`/`catch` are ordinary subs.
my $TINY = qq{use Try::Tiny;\ntry { die "boom\\n" } catch { print "caught: \$_" };\nprint "after\\n";\n};

test_src('no pragma at all: Try::Tiny keeps working', $TINY);
test_src('use v5.36: try is NOT in that bundle, so Try::Tiny keeps working',
    qq{use v5.36;\n$TINY});
test_src('no feature "try" after use v5.40 turns it back off',
    qq{use v5.40;\nno feature 'try';\n$TINY}, "caught: boom\nafter\n");
test_src('signatures still come from the bundle',
    qq{use v5.36;\nsub add (\$x, \$y) { \$x + \$y }\nprint add(2,3), "\\n";\n});

# ---- 4. a STRING EVAL inherits the site's features (#364) ------------------
#
# perl's feature pragmas are lexical and `eval STRING` inherits them.  PCL
# compiles the eval text in a separate process, on the bare string, so the SITE
# has to say — the features ride the server request next to the capture alist
# and join the eval cache key.

test_src('eval STRING inherits `use feature "try"` from its site', <<'PERL');
use feature 'try';
no warnings;
my $r = eval q{ try { die "boom\n" } catch ($e) { "caught:$e" } };
print "r=[$r] err=[$@]\n";
PERL

test_src('…and inherits it from a version bundle', <<'PERL', "r=[caught:boom\n] err=[]\n");
use v5.40;
no warnings;
my $r = eval q{ try { die "boom\n" } catch ($e) { "caught:$e" } };
print "r=[$r] err=[$@]\n";
PERL

test_src('an eval with NO feature leaves `try` an ordinary sub call', <<'PERL');
no warnings;
sub try { my ($blk) = @_; "called-try:" . $blk->() }
my $r = eval q{ try(sub { 7 }) };
print "r=[$r] err=[$@]\n";
PERL

# THE CACHE: the same eval TEXT under two different feature scopes must not
# share an entry.  Both orders, because a cache bug shows in only one of them.
test_src('same eval text, two feature scopes — feature scope first', <<'PERL');
no warnings;
my $code = 'try { 42 } catch ($e) { 0 }';
my $with    = do { use feature 'try'; eval $code };
my $without = eval $code;
print "with=", (defined $with ? $with : "undef"),
      " without=", (defined $without ? $without : "undef"), "\n";
PERL

test_src('same eval text, two feature scopes — plain scope first', <<'PERL');
no warnings;
my $code = 'try { 42 } catch ($e) { 0 }';
my $without = eval $code;
my $with    = do { use feature 'try'; eval $code };
print "with=", (defined $with ? $with : "undef"),
      " without=", (defined $without ? $without : "undef"), "\n";
PERL

# ---- 4. the core-pragma NAME TABLES, and the REAL experimental.pm ----------
#
# `lib/experimental.pm` used to be a PCL shim, standing on two blockers.  Both
# are gone: #817 made `$_ = f($_) for values %h` alias (the module's
# `$_ = version->new($_) for values %min_version`), and #840 populated the
# three tables the module builds its whole dispatch from —
# `keys %warnings::Offsets`, `keys %warnings::NoOp`, `keys %feature::feature`.
# With them empty every pragma fell past the "is this a known feature /
# category" arms into a version check and croaked "Need perl 5.34.0 or later
# for feature try" on a perl whose $] is 5.040003.  The shim is DELETED, so
# these rows guard the real module.

{
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh qq{my %h = (a => 2);\n\$_ = \$_ * 10 for values %h;\nprint \$h{a}, "\\n";\n};
    close $fh;
    my $got = run_cl($pl_file);
    chomp(my $g = $got);
    is($g, '20', '`$_ = ... for values %h` aliases into the hash (#817)');
}

# The tables as PCL answers them, dumped from a PCL run: one "KIND\tkey\tvalue"
# line each.  A hash READ cannot trigger the lazy extension load a CALL does,
# so these live in the runtime rather than a lib/ shim — which is exactly what
# this dump measures (nothing here calls a warnings:: sub first).
#
# The oracle is the SAME FILE run by perl, in its own process, never this
# harness's `%warnings::Offsets` — `warnings::register` ADDS a category per
# registering package, so by the time PPI and File::Temp are loaded the
# harness's copy carries entries (Tie::Hash => 160, …) a fresh perl has not.
my ($dump_out, $dump_file);
my %pcl_tables;
{
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh <<'PERL';
require feature; require warnings;
print "F\t$_\t$feature::feature{$_}\n"   for sort keys %feature::feature;
print "O\t$_\t$warnings::Offsets{$_}\n"  for sort keys %warnings::Offsets;
print "N\t$_\t$warnings::NoOp{$_}\n"     for sort keys %warnings::NoOp;
PERL
    close $fh;
    $dump_file = $pl_file;
    $dump_out  = run_cl($pl_file);
    for my $line (split /\n/, $dump_out) {
        next if $line !~ /^([FON])\t([^\t]*)\t(.*)$/;
        $pcl_tables{$1}{$2} = $3;
    }
}

# Load-bearing names, asserted on every host: these are the ones the real
# experimental.pm dispatches on, and the ones a `no warnings '...'` names.
is(join(",", map { exists $pcl_tables{F}{$_} ? $_ : "MISSING($_)" }
              qw(try signatures say state switch class current_sub)),
   "try,signatures,say,state,switch,class,current_sub",
   '%feature::feature carries perl\'s feature names (#840)');
is(join(",", map { exists $pcl_tables{O}{$_} ? $_ : "MISSING($_)" }
              qw(all uninitialized experimental experimental::try experimental::class)),
   "all,uninitialized,experimental,experimental::try,experimental::class",
   '%warnings::Offsets carries perl\'s warning categories (#840)');
is(join(",", map { exists $pcl_tables{N}{$_} ? $_ : "MISSING($_)" }
              qw(experimental::signatures experimental::smartmatch)),
   "experimental::signatures,experimental::smartmatch",
   '%warnings::NoOp carries the no-longer-warning categories (#840)');

# THE DRIFT ROW.  The tables are static on purpose — PCL answers for its
# target perl whatever perl it runs under — so the comparison is only
# meaningful against a 5.40.x host (CI's is 5.38, s440).
SKIP: {
    my $hostv = sprintf("%.6f", $]);
    skip "the host perl is $], not 5.40.x — the tables are 5.40's", 1
        if $hostv !~ /^5\.040/;
    my $perl_out = `perl $dump_file 2>&1`;
    is($dump_out, $perl_out,
        'the three tables are line-for-line this 5.40 perl\'s (a drift fails here)');
}

# The shim is gone: what loads is the REAL module, from perl's own lib.
{
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh qq{require experimental;\n}
            . qq{print "V=\$experimental::VERSION IN=", }
            . qq{(\$INC{"experimental.pm"} =~ m{/lib/experimental\\.pm\$} ? "pcl-shim" : "real"), "\\n";\n};
    close $fh;
    chomp(my $g = run_cl($pl_file));
    my $perl_g = `perl $pl_file 2>&1`;
    chomp $perl_g;
    is($g, $perl_g,
        'the REAL experimental.pm loads under PCL, same VERSION as perl (#840)');
}

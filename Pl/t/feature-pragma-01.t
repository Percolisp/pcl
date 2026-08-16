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

plan tests => 22;

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

sub test_src {
    my ($name, $code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $perl_out = `perl $pl_file 2>&1`;
    my $cl_out   = run_cl($pl_file);
    is($cl_out, $perl_out, $name) or diag("perl: [$perl_out]\nPCL:  [$cl_out]");
}

my $TRY = qq{try { die "boom\\n" } catch (\$e) { print "caught: \$e" }\nprint "after\\n";\n};

test_src('use v5.40 + try/catch — the #360 shape, a whole-statement DROP before',
    qq{use v5.40;\nno warnings;\n$TRY});
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
    qq{use v5.40;\nno feature 'try';\n$TINY});
test_src('signatures still come from the bundle',
    qq{use v5.36;\nsub add (\$x, \$y) { \$x + \$y }\nprint add(2,3), "\\n";\n});

# DELETE-WHEN trigger for lib/experimental.pm: the shim exists only because
# `for values %h` does not alias, which is what makes the real module die at
# load.  When this row starts FAILING, `values` aliases — drop the shim and
# let the real experimental.pm load.
{
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh qq{my %h = (a => 2);\n\$_ = \$_ * 10 for values %h;\nprint \$h{a}, "\\n";\n};
    close $fh;
    my $got = run_cl($pl_file);
    chomp(my $g = $got);
    isnt($g, '20', 'STILL not aliasing through `for values %h` — lib/experimental.pm '
                 . 'is still needed (when this row fails, delete the shim)');
}

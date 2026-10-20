#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# tap-exit-01.t — A Test::More RUN ENDS AS Test::Builder ENDS IT (task #3651).
#
# perl's Test::Builder sets the program's EXIT STATUS at its END: the number of
# failed rows (at most 254), else 255 when the plan was not met (too few, too
# many, none at all: "# No tests run!"), else 254 for rows run with no plan and
# no done_testing -- and a non-zero status the program was ALREADY leaving with
# (a die, `exit N', an END that set $?) is kept and announced: "# Looks like
# your test exited with N just after M."  PCL's TAP layer (cl/pcl-test.lisp)
# never set the status, so every failing Test::More file exited 0, and two of
# its closing lines were not Test::Builder's ("planned 1 tests", "planned 3
# tests but ran 0" where perl says "No tests run!").
#
# A file driven by perl's t/test.pl keeps that harness's ending, which sets NO
# status (perl-tests/ under the sweep): the last two rows load the TAP layer
# directly, the way the sweep does, and assert rc 0 for a failing row and the
# PCL-EXIT fact line for an explicit exit (tools/lib/PCLTap.pm reads it).
#
# perl 5.40.3 IS the oracle for the Test::More rows: the same program both
# ways; the closing lines are compared as perl writes them (on STDERR) and PCL
# writes them (on stdout, with the rest of the TAP -- %tap-out's #3241 rule).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my $testlib      = "$project_root/cl/pcl-test.lisp";
my @sbcl_rt      = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

my $workdir = tempdir(CLEANUP => 1);

# The closing lines Test::Builder writes, and only those.
sub ending { join "\n", grep { /^# (?:Looks like|No tests run|Tests were run)/ } split /\n/, $_[0] }

sub slurp { my ($p) = @_; open(my $f, '<', $p) or return ''; local $/; my $c = <$f>; $c // '' }

sub run_pcl {
    my ($tag, $body, @preload) = @_;
    my $src = "$workdir/$tag.pl";
    open(my $s, '>', $src) or die; print {$s} $body; close $s;
    my $cl_code = `$pl2cl $src 2>/dev/null`;
    my $cl_file = "$workdir/$tag.lisp";
    open(my $c, '>', $cl_file) or die; print {$c} $cl_code; close $c;
    my $pre = join ' ', map { "--load $_" } @preload;
    system("sbcl @sbcl_rt $pre --load $cl_file >$workdir/$tag.out 2>$workdir/$tag.err");
    return ($? >> 8, slurp("$workdir/$tag.out"));
}

my @cases = (
  ['f2',       "use Test::More tests => 3; ok(1,'a'); ok(0,'b'); ok(0,'c');\n"],
  ['f1m',      "use Test::More tests => 5; ok(1,'a'); ok(0,'b');\n"],
  ['extra',    "use Test::More tests => 1; ok(1,'a'); ok(1,'b');\n"],
  ['dtf',      "use Test::More; ok(1,'a'); ok(0,'b'); done_testing;\n"],
  ['dtn',      "use Test::More; ok(1,'a'); done_testing(3);\n"],
  ['zero',     "use Test::More tests => 3;\n"],
  ['die',      "use Test::More tests => 3; ok(1,'a'); die \"dead\\n\";\n"],
  ['endq',     "use Test::More tests => 1; ok(1,'a'); END { \$? = 7 }\n"],
  ['ex3',      "use Test::More tests => 2; ok(1,'a'); ok(1,'b'); exit 3;\n"],
  ['exb',      "use Test::More tests => 3; exit 4;\n"],
  ['noplan',   "use Test::More; ok(1,'a'); ok(0,'b');\n"],
  ['noplanok', "use Test::More; ok(1,'a');\n"],
  ['nop',      "use Test::More 'no_plan'; ok(1,'a'); ok(0,'b');\n"],
  ['todo',     "use Test::More tests => 2; ok(1,'a'); TODO: { local \$TODO = 'x'; ok(0,'b'); }\n"],
  ['many',     "use Test::More tests => 300; ok(0) for 1..300;\n"],
  ['endset',   "use Test::More tests => 2; ok(0,'a'); ok(1,'b'); END { print \"end sees \$?\\n\" }\n"],
  ['bail',     "use Test::More tests => 3; ok(1); BAIL_OUT('stop');\n"],
  ['skip',     "use Test::More skip_all => 'why'; ok(1);\n"],
  ['ok',       "use Test::More tests => 2; ok(1,'a'); ok(1,'b');\n"],
);

for my $c (@cases) {
    my ($tag, $body) = @$c;
    my $src = "$workdir/$tag.perl.pl";
    open(my $s, '>', $src) or die; print {$s} $body; close $s;
    system("$^X $src >$workdir/$tag.perl.out 2>$workdir/$tag.perl.err");
    my $perl_rc = $? >> 8;
    my $perl_end = ending(slurp("$workdir/$tag.perl.out") . slurp("$workdir/$tag.perl.err"));
    my ($pcl_rc, $pcl_out) = run_pcl($tag, $body);
    is($pcl_rc, $perl_rc, "$tag: exit status is Test::Builder's ($perl_rc)");
    is(ending($pcl_out), $perl_end, "$tag: the closing lines are Test::Builder's");
}

# The t/test.pl ending (the TAP layer preloaded, no `use Test::More`): no status
# of its own, and a non-zero entry status announced by the PCL fact line.
{
    my ($rc, $out) = run_pcl('h-fail', "plan(2); ok(1, 'a'); ok(0, 'b');\n", $testlib);
    is($rc, 0, 't/test.pl mode: a failing row leaves the exit status 0, as t/test.pl does');
    unlike($out, qr/^# Looks like you failed/m, 't/test.pl mode: no Test::Builder failed-line');
    ($rc, $out) = run_pcl('h-exit', "plan(2); ok(1, 'a'); exit 3;\n", $testlib);
    is($rc, 3, 't/test.pl mode: an explicit exit keeps its status');
    like($out, qr/^# PCL-EXIT status=3 after=1$/m, 't/test.pl mode: the entry status is announced by the PCL fact line');
}

done_testing;

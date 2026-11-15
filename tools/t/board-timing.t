#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# tools/board-timing.pl (task #4787, s540a) times a board file under perl and
# under PCL and splits PCL's time into transpile / load / run with a hooks
# fasl that ENCAPSULATES runtime entry points by name.  A renamed entry point
# would leave its bucket at ZERO, silently -- so these rows are the positive
# control: a one-file fixture dist whose run MUST land time in each bucket it
# exercises (a `use`d module of the dist, the Test::More layer, a string
# eval), the comparison class, and the report over the written TSV.
#
# NOT part of the Pl/t gate (it spawns SBCL a few times).  Run it directly:
#   prove tools/t/board-timing.t
use strict;
use warnings;
use Test::More tests => 32;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);

my $tool = "$RealBin/../board-timing.pl";
my $tmp = tempdir(CLEANUP => 1);
my $dist = "$tmp/Fixture-Timing-1.0";
mkdir $_ or die "$_: $!" for $dist, "$dist/t", "$dist/lib";
open my $m, '>', "$dist/lib/BtMod.pm" or die $!;
print $m "package BtMod;\nsub f { return 42 }\n1;\n";
close $m;
open my $t, '>', "$dist/t/a.t" or die $!;
print $t "use strict;\nuse Test::More tests => 3;\nuse BtMod;\n"
       . "is(BtMod::f(), 42, 'module');\nmy \$x = eval \"1 + 1\";\nis(\$x, 2, 'eval');\nok(1, 'three');\n";
close $t;
# b.t (s541a, --run): what the RUN PHASE must see -- a module first required
# at run time, a run-time string eval, a CPU-burning child, a loop.
open my $lm, '>', "$dist/lib/BtLate.pm" or die $!;
print $lm "package BtLate;\nsub g { return 7 }\n1;\n";
close $lm;
open my $bt, '>', "$dist/t/b.t" or die $!;
print $bt <<'B';
use strict;
use Test::More tests => 4;
sub late { require BtLate; return BtLate::g() }
is(late(), 7, 'late require');
my $y = eval "2 * 3";
is($y, 6, 'run-time eval');
my $rc = system($^X, '-e', 'my $x = 0; $x++ for 1 .. 3_000_000');
is($rc, 0, 'child');
my $s = 0; $s += $_ for 1 .. 200_000;
is($s, 20000100000, 'loop');
B
close $bt;
# An hour old: the runtime's module cache is valid only when its entry is
# NEWER than the source, in whole seconds -- a module written in the same
# second as the cold run's entry would be transpiled again by the warm run.
my $old = time - 3600;
utime $old, $old, map { "$dist/$_" } qw(lib/BtMod.pm lib/BtLate.pm t/a.t t/b.t) or die "utime: $!";
open my $l, '>', "$tmp/list.tsv" or die $!;
print $l "$dist\t-\ts540a\tthe fixture\n";
close $l;

my $out = "$tmp/out.tsv";
my $log = `"$^X" "$tool" --list "$tmp/list.tsv" --out "$out" --cache-root "$tmp/cache" --modes cold,warm --only a.t 2>&1`;
is($?, 0, 'the run exits 0') or diag($log);

my (@cols, %row);
open my $fh, '<', $out or die "no $out: $!";
while (<$fh>) {
  chomp;
  next if /^#/;
  my @v = split /\t/, $_, -1;
  if ($v[0] eq 'sha') { @cols = @v; next }
  my %r;
  @r{@cols} = @v;
  $row{$r{mode}} = \%r;
}
close $fh;
my ($cold, $warm) = @row{qw(cold warm)};
ok($cold && $warm, 'one cold and one warm row');
is($warm->{cmp}, 'C', 'the file is COMPARED: PCL ran its plan with perl\'s ok count');
is("$warm->{p_ok}/$warm->{c_ok}", '3/3', 'perl and PCL both pass 3');
is($warm->{rec}, 'last', 'the hooks wrote their record after the END phase');
# Rows 6-9 rewritten s541a: the two s540a rows read "the TAP layer is compiled"
# and "the dist module loads from its text" -- both untrue since s540d (#4962:
# cl/pcl-test.lisp loads from its fasl; #4986: a -I module is compiled once),
# and row 7 failed on main (d2bc17d4); row 6 passed there only on SBCL's CLOS
# constructor compiles during the TAP load, which are bucket rcompile now.
cmp_ok($warm->{tap}, '>', 0, 'tap: the Test::More layer is loaded');
is($warm->{tcompile}, 0, 'tcompile 0: it loads from its fasl (#4962), nothing of it is compiled');
cmp_ok($cold->{cfile}, '>', 0, q{cfile: the cold run builds the dist module's fasl (#4986)});
is($warm->{mcompile}, 0, 'mcompile 0: the warm run loads that fasl, not the text');
cmp_ok($warm->{ecompile}, '>', 0, 'ecompile: the string eval is compiled');
cmp_ok($cold->{xpile}, '>', 0, 'xpile: the cold run transpiles the module');
cmp_ok($cold->{n_xpile}, '>=', 1, 'n_xpile counts it');
is($warm->{n_xpile}, 0, 'the warm run transpiles no module');
my $sum = $warm->{A_wall} + $warm->{B_wall} + $warm->{C_wall};
cmp_ok(abs($sum - $warm->{c_wall}), '<', 0.02, 'A + B + C = the whole PCL wall');
cmp_ok($warm->{x_wall}, '>', 0, 'the test file transpile is timed');

my $rep = `"$^X" "$tool" --report "$out" 2>&1`;
like($rep, qr/compared \(C\) 1, partial-equal \(PE\) 0/, 'the report counts the compared file');

# ---- --run (s541a, task #4791): the RUN PHASE, on b.t ------------------------
# perl's clock is a -M helper (INIT first, END last); PCL's starts at the main
# program's CHECK drain.  Each component must land in its own column: the
# late require + the run-time eval in LATE, the child in SUBPROCESS (never in
# the run), the text run's own form compiles in the subtracted load work.
my $rout = "$tmp/run.tsv";
$log = `"$^X" "$tool" --run --perl-late --perl-c --list "$tmp/list.tsv" --only b.t --out "$rout" --cache-root "$tmp/cache" --xp-dir "$tmp/xp" --modes cold,warm 2>&1`;
is($?, 0, '--run exits 0') or diag($log);
my %rr = rows_by_mode($rout);
my $rw = $rr{warm} // {};
is($rw->{cmp}, 'C', '--run: b.t is compared');
is($rw->{q_init}, 1, 'perl: the INIT clock started (the helper module loaded)');
cmp_ok($rw->{q_cpu} // 0, '>', 0, 'perl: a run-phase CPU');
cmp_ok($rw->{q_c_wall} // 0, '>', 0, '--perl-c: perl -c timed');
cmp_ok($rw->{q_late_n} // 0, '>=', 1, q{--perl-late: perl's run-time require counted});
cmp_ok($rw->{q_sub} // 0, '>', 0, q{perl: the child's CPU is the SUBPROCESS column});
cmp_ok($rw->{q_cpu} // 1, '<', $rw->{q_sub} // 0, 'perl: ... and not inside the run number');
like($rw->{r_late_n} // '', qr{^[1-9]\d*/[1-9]\d*$}, 'PCL: a late module load and a run-time eval after the boundary');
cmp_ok($rw->{r_late_cpu} // 0, '>', 0, 'PCL: the late load / compile has seconds');
cmp_ok($rw->{r_lw_cpu} // 0, '>', 0, 'PCL: the text run compiles its run forms (subtracted)');
cmp_ok($rw->{r_sub} // 0, '>', 0.02, 'PCL: the child is the SUBPROCESS column');
cmp_ok($rw->{pr_cpu} // 1, '<', $rw->{r_cpu} // 0, 'PCL: the run number is the window minus the load work');
like($rw->{run_how} // '', qr/^text\+xp$/, 'the warm run reused the kept transpile');
$log = `"$^X" "$tool" --fasl --list "$tmp/list.tsv" --only b.t --out "$tmp/fasl.tsv" --cache-root "$tmp/cache" --xp-dir "$tmp/xp" --modes warm 2>&1`;
my %fr = rows_by_mode("$tmp/fasl.tsv");
like(($fr{warm} // {})->{run_how} // '', qr/^fasl/, '--fasl: the program ran from a fasl') or diag($log);
$rep = `"$^X" "$tool" --report --run "$rout" 2>&1`;
like($rep, qr/compared \(C, perl reached INIT\) 1;/, 'report --run counts the compared file');

sub rows_by_mode {
  my ($file) = @_;
  my (@c, %by);
  open my $h, '<', $file or return;
  while (<$h>) {
    chomp;
    next if /^#/;
    my @v = split /\t/, $_, -1;
    if ($v[0] eq 'sha') { @c = @v; next }
    my %r;
    @r{@c} = @v;
    $by{$r{mode}} = \%r;
  }
  return %by;
}

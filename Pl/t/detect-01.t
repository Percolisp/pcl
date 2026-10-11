#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# detect-01.t -- task #2610: the DETECTOR (s513d an instrument, s514d the FLAG).
#
# perl parses a statement after the one before it has RUN, so a sub or a
# prototype that BEGIN-time code installs changes how LATER statements parse;
# PCL parsed the whole unit before anything ran.  The compiler writes into
# every program and module unit (s514d; s513d: only under PCL_DETECT_TABLE) how it read each bareword call name (and marks
# each BEGIN / `use`); under PCL_DETECT_LOG the runtime writes one line per
# install that disagrees with a call site below it:
#   unit  name  install-line  install-kind  call-line  assumed-kind  installed-prototype
# That LOG mode (PCL_DETECT_LOG, or PCL_DETECT=log) is the measurement: the
# rows below check that the programs' own output is untouched.  The s514d rows
# at the end are the production FLAG (USER ruling, DECIDED ## s513): one stderr
# line per site per process under PCL_DETECT=announce (the default until
# 2026-10-11; now OFF until #3184 closes), PCL_DETECT=die dies trappably,
# PCL_DETECT=off is silent, the harmless nullary-constant shape is never
# announced, and a string eval carries no table.
#
# The three shapes are the family's named examples: brian d foy's conditional
# glob assignment in BEGIN, merlyn's `BEGIN { eval "sub zany ();" }`, and
# Scalar::Util::set_prototype in BEGIN (#2610's own reproducer).  No 5.40
# syntax: CI's perl is 5.38.

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
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 19;

# The instrument's emission must never reach the shared module cache.
my $cache = tempdir(CLEANUP => 1);
chmod 0700, $cache;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile and run with the detector ON; return (stdout, [log lines]).
sub detect_run {
    my ($code) = @_;
    my $pl = write_pl($code);
    my ($lfh, $log) = tempfile(SUFFIX => '.log', UNLINK => 1);
    close $lfh;
    unlink $log;
    local $ENV{PCL_CACHE_DIR}    = $cache;
    local $ENV{PCL_DETECT_TABLE} = 1;
    local $ENV{PCL_DETECT_LOG}   = $log;
    local $ENV{PCL_DETECT}       = 'log';   # robust to a PCL_DETECT in the caller's environment
    my $cl = PCLCore::transpile("$pl2cl $pl");
    my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl;
    close $cfh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    my @lines;
    if (open my $lh, '<', $log) { @lines = <$lh>; chomp @lines }
    s/\A[^\t]*\t// for @lines;       # the unit is a temp file name
    return ($out, \@lines);
}

my $FOY = <<'PL';
BEGIN {
  my $subname = 'do_it';
  no strict 'refs';
  *{$subname} = (time > 0) ? sub ($) { "A: $_[0]" } : sub ($$) { "B: $_[0] \U$_[1]" };
}
print do_it('a', 'b'), "\n";
PL

my ($out, $log) = detect_run($FOY);
is($out, "A: a\n", 'foy: the program output is untouched (the wrong parse still runs)');
is_deeply($log, ["main::do_it\t5\tglob\t6\tunknown\t(\$)"],
          'foy: a conditional glob assignment in BEGIN is ONE event, at the call below it');

my $MERLYN = <<'PL';
BEGIN { eval((time > 0) ? "sub zany ();" : "sub zany (\@);") }
sub zany { 42 }
print zany + 1, "\n";
PL

($out, $log) = detect_run($MERLYN);
is($out, "42", 'merlyn: the program output is untouched');
is_deeply($log, ["main::zany\t1\tdeclare\t3\tlist-call\t()"],
          'merlyn: an eval-made forward declaration in BEGIN is ONE event');

my $SETP = <<'PL';
sub sp { scalar(@_) }
my @l = (1, 2, 3);
BEGIN { require Scalar::Util; Scalar::Util::set_prototype(\&sp, q($$)); }
print "c ", sp(@l, 5), "\n";
PL

($out, $log) = detect_run($SETP);
is($out, "c 4\n", 'set_prototype: the program output is untouched');
is_deeply($log, ["main::sp\t3\tset-prototype\t4\tlist-call\t(\$\$)"],
          'set_prototype in BEGIN is ONE event, at the call below it');

my $PLAIN = <<'PL';
use Scalar::Util qw(blessed);
sub f ($) { "f:$_[0]" }
BEGIN { *g = sub ($) { "g:$_[0]" } }
my $o = bless {}, 'K';
print f(1), g(2), (blessed $o), "\n";
PL

($out, $log) = detect_run($PLAIN);
is($out, "f:1g:2K\n", 'a plain program runs unchanged under the detector');
is_deeply($log, [], '... and logs nothing: every install is one the compiler saw');

# ---- the table is emitted by default (s514d) ----------------------------
# s513d emitted it only under PCL_DETECT_TABLE; since the FLAG ruling every
# program and module unit carries it, with or without that variable.

{
    local $ENV{PCL_DETECT_TABLE};
    delete $ENV{PCL_DETECT_TABLE};
    my $cl = PCLCore::transpile("$pl2cl " . write_pl($FOY));
    like($cl, qr/\(pcl::p-detect-table /, 'the table is emitted by default (no switch) ...');
    like($cl, qr/\(pcl::p-detect-at 5\)/, '... and the BEGIN block is marked at its last line');
}

# ---- production mode: the FLAG (USER ruling, DECIDED ## s513) -----------
# Run with PCL_DETECT set (or unset = off, USER 2026-10-11) and NO log; return
# (stdout, [the PCL: detector lines of stderr], the whole stderr text).
sub flag_run {
    my ($code, $mode) = @_;
    my $pl = write_pl($code);
    local $ENV{PCL_CACHE_DIR} = $cache;
    local $ENV{PCL_DETECT_LOG};
    delete $ENV{PCL_DETECT_LOG};
    local $ENV{PCL_DETECT};
    if (defined $mode) { $ENV{PCL_DETECT} = $mode } else { delete $ENV{PCL_DETECT} }
    my $cl = PCLCore::transpile("$pl2cl $pl");
    my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl;
    close $cfh;
    my ($efh, $err) = tempfile(SUFFIX => '.err', UNLINK => 1);
    close $efh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>$err`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    my (@lines, $all);
    if (open my $eh, '<', $err) { local $/; $all = <$eh>; @lines = grep { /^PCL: / } split /^/m, $all; s/\n\z// for @lines }
    s/^PCL: \S+ line/PCL: FILE line/ for @lines;    # the unit is a temp file name
    return ($out, \@lines, $all // '');
}

# Two call sites below the install, one of them in a loop; a nullary constant
# read as a bareword term (shape 1, harmless); a string eval (no table).
my $FLAG = <<'PL';
BEGIN {
  my $subname = 'do_it';
  no strict 'refs';
  *{$subname} = (time > 0) ? sub ($) { "A:$_[0]" } : sub ($$) { "B:$_[0]" };
}
print do_it 'a', 'b';
print "\n";
for (1 .. 2) { print do_it 'c', 'd'; print "\n" }
BEGIN { no strict 'refs'; *{"main::KONST"} = sub () { 0 } }
print "k\n" if !KONST;
my $r = eval q{ BEGIN { *main::ev = sub ($) { "E:$_[0]" } } ev 'x', 'y' };
print "eval [$r]\n";
PL

my $SITE = "was parsed as a call of a sub it did not know but code that ran at line 5 "
         . "installed a sub with prototype (\$); set PCL_DETECT=die to stop here";
($out, $log) = flag_run($FLAG, 'announce');
is($out, "A:a\nA:c\nA:c\nk\neval [y]\n", 'PCL_DETECT=announce: the run continues, output untouched');
is_deeply($log, ["PCL: FILE line 6: 'main::do_it' $SITE", "PCL: FILE line 8: 'main::do_it' $SITE"],
          'announce: two call sites are two lines, the loop is one; the harmless constant and the eval are silent');

my ($out_off, $log_off) = flag_run($FLAG, 'off');
is($out_off, $out, 'PCL_DETECT=off: the same output ...');
is_deeply($log_off, [], '... and not one line');

# THE DEFAULT IS OFF (USER ruling 2026-10-11, s515: off until #3184, the
# export-tag import gap, closes).  Inverse: on main 403009eb the unset default
# announces, so the stderr row fails there.
my ($out_def, undef, $err_def) = flag_run($FLAG);
is($out_def, $out, 'PCL_DETECT unset (the default, off): the same output ...');
is($err_def, '', '... and stderr EMPTY');

my $DIE = <<'PL';
BEGIN {
  no strict 'refs';
  my $ok = eval { *{"main::do_it"} = sub ($) { "A:$_[0]" }; 1 };
  print "trapped: $@" if !$ok;
}
print do_it 'a', 'b';
print "\n";
PL

($out, $log) = flag_run($DIE, 'die');
like($out, qr/^trapped: 'main::do_it' was parsed as a call of a sub it did not know but code that ran at line 5 installed a sub with prototype \(\$\) at \S+ line 6\.\n/,
     'PCL_DETECT=die: dies perl-shaped and trappably, $@ naming the site');
is_deeply($log, [], '... and announces nothing');

($out, $log) = flag_run($DIE, 'bogus');
unlike($out, qr/A:a/, 'an unknown PCL_DETECT mode stops the program ...');

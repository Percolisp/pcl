#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# detect-01.t -- task #2610 (s513d): the DETECTOR as an instrument.
#
# perl parses a statement after the one before it has RUN, so a sub or a
# prototype that BEGIN-time code installs changes how LATER statements parse;
# PCL parsed the whole unit before anything ran.  Under PCL_DETECT_TABLE the
# compiler writes into the unit how it read each bareword call name (and marks
# each BEGIN / `use`); under PCL_DETECT_LOG the runtime writes one line per
# install that disagrees with a call site below it:
#   unit  name  install-line  install-kind  call-line  assumed-kind  installed-prototype
# It is a MEASUREMENT: nothing dies and nothing announces (the rows below check
# that the programs' own output is untouched), and with neither variable set
# the emission carries no trace of it.
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

plan tests => 11;

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

# ---- off by default: no trace in the emission --------------------------

{
    local $ENV{PCL_DETECT_TABLE};
    delete $ENV{PCL_DETECT_TABLE};
    my $cl = PCLCore::transpile("$pl2cl " . write_pl($FOY));
    unlike($cl, qr/p-detect/, 'without PCL_DETECT_TABLE the emission carries no detector form');
}
{
    local $ENV{PCL_DETECT_TABLE} = 1;
    my $cl = PCLCore::transpile("$pl2cl " . write_pl($FOY));
    like($cl, qr/\(pcl::p-detect-table /, 'with it, the table is emitted ...');
    like($cl, qr/\(pcl::p-detect-at 5\)/, '... and the BEGIN block is marked at its last line');
}

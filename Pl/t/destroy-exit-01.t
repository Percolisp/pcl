#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# destroy-exit-01.t — DESTROY AT PROGRAM EXIT (task #2371, s538b; Stage 0 of
# docs/scope-exit-destroy-design.md; ir-spec §7.5).
#
# perl destroys every object still alive when the program ends, AFTER the END
# blocks, with ${^GLOBAL_PHASE} = DESTRUCT.  PCL has no reference counts, so
# that is the one DESTROY moment it can know: while the program has a
# non-empty DESTROY or AUTOLOAD, every bless notes its referent in a WEAK
# registry and the exit hook calls DESTROY on each one still alive.
#
# perl 5.40.3 IS the oracle: every row runs the same program both ways and
# compares STDOUT and the exit status (and STDERR where the row says so).
# perl does not specify the ORDER of global destruction, so a row with more
# than one object compares its lines SORTED.  Rows never assert scope-exit
# timing: PCL calls DESTROY at exit only (Stage 2 = #2370, parked).
#
# MEASURED perl facts these rows pin (s538b probes): a $? set inside DESTROY
# becomes the exit status; $@ and $! are NOT localized around DESTROY (a later
# destructor sees an earlier one's values); a die inside DESTROY is silent
# without `use warnings` and keeps the status; `exit` inside DESTROY ends the
# whole destruction; AUTOLOAD is called for DESTROY when no DESTROY exists,
# and an empty `sub DESTROY {}` suppresses it.

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
my @sbcl_rt      = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 30;

my $workdir = tempdir(CLEANUP => 1);

# The SAME program under perl and under PCL: (stdout, status, stderr) each.
sub run_both {
    my ($tag, $body) = @_;
    my $src = "$workdir/$tag.pl";
    open(my $s, '>', $src) or die; print {$s} $body; close $s;
    my $perl_out = `$^X $src 2>$workdir/$tag.perr`;
    my $perl = $? >> 8;
    my $cl_code = `$pl2cl $src 2>/dev/null`;
    my $cl_file = "$workdir/$tag.lisp";
    open(my $c, '>', $cl_file) or die; print {$c} $cl_code; close $c;
    my $pcl_out = `sbcl @sbcl_rt --load $cl_file 2>$workdir/$tag.cerr`;
    my $pcl = $? >> 8;
    my $slurp = sub { open(my $h, '<', $_[0]) or return ''; local $/; <$h> // '' };
    return ([$perl_out, $perl, $slurp->("$workdir/$tag.perr")],
            [$pcl_out, $pcl, $slurp->("$workdir/$tag.cerr")]);
}

sub sorted_lines { join '', sort split /^/m, $_[0] }

# A class whose DESTROY prints its object's name.
my $G = 'package G; sub new { my ($c, $n) = @_; bless { n => $n }, $c } '
      . 'sub DESTROY { print "DESTROY $_[0]{n}\n" } package main; $| = 1; ';

# ROWS: [tag, program, mode]; mode `exact` compares stdout + status as they
# are, `sorted` compares stdout sorted (several objects, order unspecified).
my @rows = (
  ['01-end-first',  $G . 'our $g = G->new("global"); END { print "END\n" } print "main\n";', 'exact'],
  ['02-file-lex',   $G . 'my $o = G->new("lexical"); print "main\n";', 'exact'],
  ['03-closure',    $G . 'my $f = do { my $o = G->new("closure"); sub { $o } }; print "main\n";', 'exact'],
  ['04-containers', $G . 'our @k = (G->new("a1")); our %h = (x => G->new("h1")); print "main\n";', 'sorted'],
  ['05-nested',     $G . 'our $p = G->new("parent"); $p->{c} = G->new("child"); print "main\n";', 'sorted'],
  ['06-inherited',  'package B1; sub DESTROY { print "B1::DESTROY ", ref($_[0]), "\n" } package C1; our @ISA = ("B1"); sub new { bless {}, shift } package main; our $c = C1->new; print "main\n";', 'exact'],
  ['07-autoload',   'package A1; our $AUTOLOAD; sub new { bless {}, shift } sub AUTOLOAD { print "AUTOLOAD $AUTOLOAD\n"; return } package main; our $a1 = A1->new; print "main\n";', 'exact'],
  # INVERSE GUARD of the empty-body mark: the AUTOLOAD-guard idiom calls nothing.
  ['08-empty-guard', 'package A2; our $AUTOLOAD; sub new { bless {}, shift } sub DESTROY {} sub AUTOLOAD { print "AUTOLOAD $AUTOLOAD\n" } package main; our $a2 = A2->new; print "main\n";', 'exact'],
  # The class is read AT EXIT: a re-bless into a class with no DESTROY ...
  ['09-rebless-out', $G . 'package N; package main; our $o = G->new("rb"); bless $o, "N"; print "main ", ref($o), "\n";', 'exact'],
  # ... and a DESTROY installed after the bless (the program already had one).
  ['10-late-glob',  'package Z; sub DESTROY { print "Z\n" } package L; sub new { bless {}, shift } package main; our $l = L->new; *L::DESTROY = sub { print "late DESTROY ", ref($_[0]), "\n" }; print "main\n";', 'exact'],
  ['11-explicit',   $G . 'our $o = G->new("expl"); $o->DESTROY; print "main\n";', 'exact'],
  ['12-kinds',      'package K; sub DESTROY { print "DESTROY ", Scalar::Util::reftype($_[0]), " ", ref($_[0]), "\n" } package main; use Scalar::Util (); our $c = bless sub { 1 }, "K"; our $a = bless [], "K"; our $s = bless \my $x, "K"; our $g = bless \*FOO, "K"; print "main\n";', 'sorted'],
  ['13-phase',      'package D; sub new { bless {}, shift } sub DESTROY { print "phase ${^GLOBAL_PHASE}\n" } package main; our $d = D->new; print "main ${^GLOBAL_PHASE}\n";', 'exact'],
  ['14-method-in',  'package D; sub new { bless {v=>[1,2,3]}, shift } sub sum { my $s = 0; $s += $_ for @{$_[0]{v}}; $s } sub DESTROY { print "sum ", $_[0]->sum, " ref ", ref($_[0]), "\n" } package main; our $d = D->new; print "main\n";', 'exact'],
  ['15-eval-class', 'eval q{package E; sub new { bless {}, shift } sub DESTROY { print "E::DESTROY\n" } 1} or die $@; our $e = E->new; print "main\n";', 'exact'],
  # Objects a DESTROY creates are destroyed too (the drain).
  ['16-new-in-destroy', 'package M; sub new { bless { i => $_[1] }, $_[0] } sub DESTROY { print "DESTROY $_[0]{i}\n"; $main::m2 = M->new($_[0]{i} + 1) if $_[0]{i} < 3 } package main; our $m = M->new(1); print "main\n";', 'exact'],
  # THE STATUS: a $? set in DESTROY is the exit status; a die keeps it; exit
  # in END then DESTROY; a top-level die still destroys.
  ['17-status-set', 'package D; sub new { bless {}, shift } sub DESTROY { $? = 9; print "DESTROY sees ", $?, "\n" } package main; our $d = D->new; print "main\n"; exit 3;', 'exact'],
  ['18-die-kept',   'package D; sub new { bless {}, shift } sub DESTROY { die "boom\n" } package main; $| = 1; our $d = D->new; print "main\n"; exit 4;', 'exact'],
  ['19-exit-in-end', $G . 'our $o = G->new("eie"); END { print "END\n"; exit 5 } print "main\n";', 'exact'],
  ['20-top-die',    $G . 'our $o = G->new("died"); print "main\n"; die "top\n";', 'exact'],
  ['21-sig-handlers', 'package D; sub new { bless {}, shift } sub DESTROY { warn "w in destroy\n"; die "d in destroy\n" } package main; $| = 1; $SIG{__WARN__} = sub { print "WARN: $_[0]" }; $SIG{__DIE__} = sub { print "DIEH: $_[0]" }; our $d = D->new; print "main\n";', 'exact'],
  # PROCESS BOUNDARIES: a fork child destroys its copy; exec and _exit run nothing.
  ['22-fork',       'package F; sub new { bless { pid => $$ }, shift } sub DESTROY { print "DESTROY in ", ($_[0]{pid} == $$ ? "parent" : "child"), "\n" } package main; $| = 1; our $f = F->new; my $pid = fork; if (!$pid) { print "child exits\n"; exit 0 } waitpid $pid, 0; print "parent after child\n";', 'exact'],
  ['23-exec',       $G . 'our $o = G->new("exec"); print "main\n"; exec "/bin/echo", "execd";', 'exact'],
  ['24-posix-exit', $G . 'use POSIX (); our $o = G->new("px"); print "main\n"; POSIX::_exit(4);', 'exact'],
  # Test::More's plan line and exit code do not move; the DESTROY line follows.
  ['25-test-more',  $G . 'use Test::More; our $o = G->new("tm"); ok(1, "one"); done_testing;', 'exact'],
  # INVERSE GUARD: no DESTROY / AUTOLOAD anywhere -- nothing runs, nothing prints.
  ['26-no-destroy', 'package P; sub new { bless {}, shift } package main; our @o = map { P->new } 1 .. 5; print "main\n";', 'exact'],
);

for my $r (@rows) {
    my ($tag, $body, $mode) = @$r;
    my ($p, $c) = run_both($tag, $body);
    my ($po, $co) = ($p->[0], $c->[0]);
    ($po, $co) = (sorted_lines($po), sorted_lines($co)) if $mode eq 'sorted';
    is("$co|rc=$c->[1]", "$po|rc=$p->[1]", "global destruction $tag: stdout and status are perl's");
}

# A die inside DESTROY is SILENT without `use warnings` -- perl's `(in
# cleanup)` is a default-off warning (W misc) -- so stderr is empty both ways.
{
    my ($p, $c) = run_both('27-die-silent',
        'package D; sub new { bless {}, shift } sub DESTROY { die "boom\n" } package main; our $d = D->new; print "main\n";');
    is("$c->[0]|$c->[2]|rc=$c->[1]", "$p->[0]|$p->[2]|rc=$p->[1]",
       'a die in DESTROY prints nothing and keeps status 0, as perl without warnings');
}

# `exit` inside DESTROY ends the WHOLE destruction with its status: one of the
# two destructors runs (which one is unspecified), then the process exits 7.
{
    my ($p, $c) = run_both('28-exit-in-destroy',
        'package X; sub new { bless { n => $_[1] }, $_[0] } sub DESTROY { print "X::DESTROY\n"; exit 7 } package main; $| = 1; our $x1 = X->new(1); our $x2 = X->new(2); print "main\n";');
    is("$c->[0]|rc=$c->[1]", "$p->[0]|rc=$p->[1]",
       'exit in DESTROY runs one destructor and exits with its status');
}

# $@ and $! are NOT localized around DESTROY: whichever destructor runs second
# sees what the first one set (perl, probed).  Order-free comparison.
{
    my ($p, $c) = run_both('29-errsv-shared',
        'package D; sub new { bless {n=>$_[1]}, $_[0] } sub DESTROY { my $e = $@; $e =~ s/\d//g; print "sees [$e] [", (($!+0) == 5 ? 5 : "x"), "]\n"; $@ = "set$_[0]{n}"; $! = 5 } package main; $| = 1; our $a1 = D->new(1); our $b1 = D->new(2); $@ = "orig"; $! = 0; print "main\n";');
    is(sorted_lines($c->[0]), sorted_lines($p->[0]),
       'the second DESTROY sees the first one\'s $@ and $!: nothing is localized');
}

# The resurrection guard: an object a DESTROY stores in a global is destroyed
# ONCE.  perl itself dies here ("DESTROY created new reference to dead object",
# exit 22) -- PCL cannot see a new reference, so this row asserts only the
# part both agree on: the destructor's line appears exactly once.
{
    my ($p, $c) = run_both('30-resurrect-once',
        'package R; our @keep; my $n = 0; sub new { bless {}, shift } sub DESTROY { $n++; print "DESTROY $n\n"; push @keep, $_[0] } package main; $| = 1; our $r = R->new; print "main\n";');
    my $count = () = $c->[0] =~ /^DESTROY \d/mg;
    is($count, 1, 'a resurrected object is destroyed once (perl: once, then its own fatal)');
}

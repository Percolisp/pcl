#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perf-levers-13.t — perf round 44 (s513h, docs/faster-codegen-suggestions.md
# "Round 44 movers").  MECHANISM rows (the runtime's own answer), a TIMED row
# (bound RELATIVE to perl's own time measured in the run, DECIDED ## s513) and
# ANSWER rows: programs whose expected output below IS perl's own (probed on
# 5.40.3; nothing in them needs perl >= 5.38).
#
#   #2771 print/say with one to three list items take a FIXED-ARITY entry
#         (%p-print-1/2/3, chosen by a compiler macro from the form's arity)
#         that builds the item list on the stack and runs the SAME resolver
#         and tail as p-print; the general entry's &rest list is
#         dynamic-extent.
use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use lib "$RealBin/../..";
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl   = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);
plan skip_all => "pl2cl not found" if !-x $pl2cl;
plan skip_all => "sbcl not found"  if !`which sbcl 2>/dev/null`;

sub src_file {
    my ($src) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $src;
    close $fh;
    return $file;
}

sub transpile { my ($src) = @_; return PCLCore::transpile(qq{$pl2cl } . src_file($src)) }

sub run_pl {
    my ($src) = @_;
    my $cl = transpile($src);
    my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl;
    close $cfh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^(?:caught |compilation unit|-->|==>|PCL Runtime loaded).*\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

sub answers {
    my ($src, $expected, $what) = @_;
    is(run_pl($src), $expected, $what);
}

# The runtime's own answers to a few forms (the MECHANISM rows).
sub lisp_out {
    my ($forms) = @_;
    my ($lfh, $lfile) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $lfh "(in-package :pcl)\n$forms";
    close $lfh;
    return scalar `sbcl @sbcl_rt --load $lfile 2>&1`;
}

use Time::HiRes qw(time);

# ─────────────────────────────────────────────────────────────────────────────
# #2771 MECHANISM: a print/say form with one to three list items compiles to
# a fixed-arity entry (the compiler macro decides from the ARITY alone); a
# longer one stays the general &rest call.
like(lisp_out(<<'END_LISP'),
(let ((*print-pretty* nil)) (format t "~S" (list (funcall (compiler-macro-function 'p-print) '(p-print :fh $fh "x") nil) (funcall (compiler-macro-function 'p-print) '(p-print "x" $y) nil) (funcall (compiler-macro-function 'p-say) '(p-say :fh 'stderr "a" "b" "c") nil) (funcall (compiler-macro-function 'p-print) '(p-print :fh $fh 1 2 3 4) nil) (funcall (compiler-macro-function 'p-print) '(p-print :fh $fh @a) nil))))
END_LISP
     do { my $e = q{((%p-print-1 nil $fh "x") (%p-print-2 nil nil "x" $y) (%p-print-3 t (quote stderr) "a" "b" "c") (p-print :fh $fh 1 2 3 4) (%p-print-1 nil $fh @a))}; qr/\Q$e\E/i },
     '#2771: print/say with 1-3 items -> %p-print-N (say = T, no handle = NIL); 4+ items stay p-print');

# #2771 MECHANISM: neither entry allocates its argument list.  A print of two
# items consed 6.4 MB per 100 000 calls on the base (the &rest list); the
# fixed entry and the general entry (dynamic-extent rest) cons nothing.
like(lisp_out(<<'END_LISP'),
(with-open-file (s "/dev/null" :direction :output :if-exists :append)
  (let ((f (compile nil '(lambda (s) (dotimes (i 100000) (p-print :fh s "x" "y") (p-print :fh s "a" "b" "c" "d" "e"))))))
    (funcall f s)
    (let ((b0 (sb-ext:get-bytes-consed)))
      (funcall f s)
      (format t "CONSED=~D~%" (if (< (- (sb-ext:get-bytes-consed) b0) 100000) "small" (- (sb-ext:get-bytes-consed) b0))))))
END_LISP
     qr/CONSED=small/, '#2771: 200 000 prints (2 and 5 items) allocate no argument lists');

{
    # 2 000 000 `print $fh "x\n"`: 0.32 s here, 0.37 s on the base, perl 0.07 s
    # (whole run incl. transpile and start-up).  The bound is RELATIVE to perl's own time on the same program,
    # measured in this run (DECIDED ## s513: 8x perl + 0.5 s), the gross-
    # regression bound; the inverse guard of the lever is the two rows above.
    my $src = q{my $f = "/tmp/pcl-pl13-t-$$"; open(my $fh, '>', $f) or die; for my $i (1 .. 2000000) { print $fh "x\n" } close $fh; my $sz = -s $f; unlink $f; print "$sz\n";};
    my ($pfh, $pfile) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $pfh $src;
    close $pfh;
    my $p0 = time;
    my $perl_out = `$^X $pfile 2>&1`;
    my $pdt = time - $p0;
    die "perl's own answer is wrong: $perl_out" if $perl_out ne "4000000\n";
    my $t0 = time;
    my $out = run_pl($src);
    my $dt = time - $t0;
    my $bound = 8 * $pdt + 0.5;
    is($out, "4000000\n", '#2771 timed: the answer');
    cmp_ok($dt, '<', $bound, sprintf('#2771 timed: 2 000 000 prints within 8x perl + 0.5 s = %.2f s (took %.2f s, perl %.2f s)', $bound, $dt, $pdt));
}

answers(<<'END_SRC', <<'END_EXP', '#2771 answers: 1-5 items, spreading items through a fixed entry, $, and $\ (set, 0, undef), say, in-memory, select, closed and read-only handles, numbers / undef / refs / overload / tied scalar, a ternary glob block, :utf8, printf, print in list/scalar context, a list-returning call');
use strict; no warnings;
my $f = "/tmp/pcl-pl13-$$";
open(my $fh, '>', $f) or die;
my $x = "v"; my @a = (1, 2, 3); my %h = (k => 1); my $i = 7;
print $fh "x\n"; print {$fh} "x\n"; print $fh $x; print $fh "x", "\n"; print $fh "v=$i\n";
print $fh @a, "\n"; print $fh @a; print $fh %h, "\n"; print $fh "a", @a, "b"; print $fh 1, 2, 3, 4, "\n";
{ local $, = "-"; local $\ = "!\n"; print $fh "a", "b"; print $fh "c"; print $fh @a; }
{ local $, = 0; local $\ = 0; print $fh "a", "b"; }
{ local $, = undef; local $\ = undef; print $fh "a", "b", "\n"; }
close($fh);
open(my $r, '<', $f) or die; my $c = do { local $/; <$r> }; close $r; unlink $f;
$c =~ s/\n/|/g; print "FILE[$c]\n";
{ use feature 'say'; local $\ = "<O>"; say "s1"; say "s2", "s3"; local $, = ","; say @a; say $x, $i, "z"; }
{ my $buf = ''; open(my $m, '>', \$buf) or die; print $m "m1", "m2"; print {$m} 42; close $m; print "MEM[$buf]\n"; }
{ my $buf = ''; open(my $m, '>', \$buf) or die; my $old = select($m); print "sel"; print "a", "b"; select($old); close $m; print "SEL[$buf]\n"; }
{ open(my $cl, '>', "/dev/null") or die; close $cl; my $rv = print $cl "x"; print "CLOSED[", defined $rv ? "<$rv>" : 'undef', "][", ($!+0), "]\n"; }
{ open(my $ro, '<', "/dev/null") or die; my $rv = print $ro "x"; print "RO[", defined $rv ? "<$rv>" : 'undef', "][", ($!+0), "]\n"; }
{ print 3.5, "\n"; print 1e21, "\n"; my $u; print "u=", $u, "\n"; my $ar = [1]; my $s = "$ar"; print(($s =~ /^ARRAY\(0x/ ? "ref-ok" : "ref-bad"), "\n"); }
{ package OV; use overload '""' => sub { "OVL" }; } { my $o = bless {}, 'OV'; print $o, "\n"; print "o=$o\n"; }
{ package TS; my $n = 0; sub TIESCALAR { bless [], shift } sub FETCH { $n++; "F$n" } } { tie my $t, 'TS'; print $t, "\n"; print $t; print "\n"; }
{ my $ok = 1; print { $ok ? *STDOUT : *STDERR } "tern\n"; }
{ my $f2 = "$f.u"; open(my $u8, '>:utf8', $f2) or die; print $u8 "\x{263A}"; close $u8; print "U8[", -s $f2, "]\n"; unlink $f2; }
{ printf("%s-%d\n", "pf", 3); my @l = (print("L1\n")); print "LIST[@l]\n"; my $sv = print "S1\n"; print "SC[$sv]\n"; print("P1\n") or die; print("pa", "rens"), print "\n"; }
sub three { return ("t1", "t2") } print three(), "\n"; print "q", three(), "\n";
END_SRC
FILE[x|x|vx|v=7|123|123k1|a123b1234|a-b!|c!|1-2-3!|a0b0ab|]
s1
s2s3
1,2,3
v,7,z
MEM[m1m242]
SEL[selab]
CLOSED[undef][9]
RO[undef][9]
3.5
1e+21
u=
ref-ok
OVL
o=OVL
F1
F2
tern
U8[3]
pf-3
L1
LIST[1]
S1
SC[1]
P1
parens
t1t2
qt1t2
END_EXP

done_testing();

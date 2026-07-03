#!/usr/bin/env perl
# Pl/t/parser2-01.t — v2 pipeline prototype (Pl::Parser2 / Pl::ExprToCL2 /
# Pl::VarAnnotator / Pl::CLForm).  Shape checks on generated CL + one
# end-to-end run.  The v2 pipeline is opt-in (PCL_V2=1); these tests pin the
# north-star shapes from docs/codegen-rewrite-spec.md so they can't silently
# regress while v2 grows.
use v5.30;
use strict;
use warnings;
use Test::More;
use FindBin;
use lib "$FindBin::Bin/../..";
use Pl::Parser2;

my $fib = <<'EOF';
sub fib { my ($n) = @_; my $a = 0; my $b = 1; for my $i (2..$n) { my $c = $a + $b; $a = $b; $b = $c; } return $b; }
print fib(30), "\n";
EOF

my $cl = Pl::Parser2->parse_code($fib);

# Spec #3: real lambda list, no p-list-= arg destructuring.
like($cl, qr/\(p-sub pl-fib\s*\n?\s*\(&optional \(\$n \(p-undef\)\) &rest %_args\)/,
     'fixed-arity &optional lambda list for my (LIST) = @_');
unlike($cl, qr/p-list-= \(vector \$n\)/, 'no p-list-= param destructuring');

# Spec #1: no dead hoisted boxes for the loop-body vars.
unlike($cl, qr/\(\$i \(make-p-box/, 'no dead hoisted loop-var box');
my $c_boxes = () = $cl =~ /\(\$c \(make-p-box/g;
is($c_boxes, 0, '$c is unboxed (single arith write)');
like($cl, qr/\(let \(\(\$c \(p-\+ \$a \$b\)\)\)/, '$c bound raw at its declaration site');

# Spec #2: no VOID *wantarray* wrap around the sub body or plain assignments.
unlike($cl, qr/\(let \(\(\*wantarray\* :void\)\)/, 'no VOID context wrap');
like($cl, qr/\(p-my-= \$a \$b\)/, 'boxed var copy is plain p-my-= (no wrap, no p-scalar-=)');
unlike($cl, qr/p-scalar-=/, 'no special-proclaiming p-scalar-= anywhere');

# Foreach list is list-context: a range, not a flip-flop.
like($cl, qr/\(p-foreach \(\$i \(p-\.\. 2 \$n\)\)/, 'foreach range in list context');

# Unboxed accumulator: my $sum = 0; $sum = $sum + ...;
my $cl2 = Pl::Parser2->parse_code(
  'my $sum = 0; for my $i (0..9) { $sum = $sum + $i * 2; } print "$sum\n";');
like($cl2, qr/\(let \(\(\$sum 0\)\)/, 'unboxable scalar bound raw');
like($cl2, qr/\(setf \$sum \(p-\+ \$sum \(p-\* \$i 2\)\)\)/, 'native setf write, native op forms');

# Gate-1 fallbacks: a ref-taken scalar stays boxed.
my $cl3 = Pl::Parser2->parse_code(
  'my $x = 1; my $r = \$x; print "$x\n";');
like($cl3, qr/\(\$x \(make-p-box nil\)\)/, 'ref-taken scalar stays boxed');

# Native funcalls + R2 caller half: context-insensitive callee → direct call,
# no *wantarray* bind; context-sensitive callee keeps the bind.
my $rec = Pl::Parser2->parse_code(
  'sub fib { my ($n) = @_; return $n if $n < 2; return fib($n-1) + fib($n-2); } print fib(10), "\n";');
like($rec, qr/\(p-\+ \(pl-fib \(p-- \$n 1\)\) \(pl-fib \(p-- \$n 2\)\)\)/,
     'insensitive callee: direct native calls, no *wantarray* bind');
my $ctx = Pl::Parser2->parse_code(
  'sub pair { my ($x) = @_; return ($x, $x); } sub inc { my ($x) = @_; return $x + 1; } my $s = 0; $s = inc(pair(7) + 0) + 100;');
like($ctx, qr/\(let \(\(\*wantarray\* nil\)\) \(pl-pair 7\)\)/,
     'sensitive callee (returns a list) keeps the *wantarray* bind');
like($ctx, qr/\(pl-inc \(p-\+/,
     'insensitive callee called directly even with nested sensitive arg');

# --- Session-265 growth: strings, funcall-in-arith unboxing, elsif, C-for ---

# String literals + `.` concat are native, and string slots unbox.
my $str = Pl::Parser2->parse_code(
  q{my $s = 'hello'; my $t = $s . " world"; print "$t\n";});
like($str, qr/\(let \(\(\$s "hello"\)\)/, 'bare string literal binds raw');
like($str, qr/\(let \(\(\$t \(p-\. \$s " world"\)\)\)/, 'native p-. concat, raw slot');

# A known-sub call under a top-level operator unboxes (`my $x = f() + 1`).
my $fca = Pl::Parser2->parse_code(
  'sub add { my ($a, $b) = @_; return $a + $b; } my $x = add(2, 3) + 1; print "$x\n";');
like($fca, qr/\(let \(\(\$x \(p-\+ \(pl-add 2 3\) 1\)\)\)/,
     'funcall under arith op: raw slot, direct native call');

# A BARE known-sub call must NOT unbox (could return a box).
my $fcb = Pl::Parser2->parse_code(
  'sub give { my ($a) = @_; return $a; } my $x = give(2); print "$x\n";');
like($fcb, qr/\(\$x \(make-p-box nil\)\)/, 'bare funcall RHS stays boxed');

# elsif chains lower to nested p-if.
my $eif = Pl::Parser2->parse_code(
  'my $x = 5; if ($x > 9) { print "a\n"; } elsif ($x > 4) { print "b\n"; } else { print "c\n"; }');
like($eif, qr/\(p-if \(p-> \$x 9\)/, 'if head of elsif chain');
like($eif, qr/\(p-if \(p-> \$x 4\)/, 'elsif lowered as nested p-if');

# C-style for: arith step → raw counter; ++ step → boxed counter.
my $cfor = Pl::Parser2->parse_code(
  'for (my $i = 0; $i < 3; $i = $i + 1) { print "$i\n"; }');
like($cfor, qr/\(let \(\(\$i 0\)\)/, 'C-for arith-step counter binds raw');
like($cfor, qr/\(\(setf \$i \(p-\+ \$i 1\)\)\)/, 'C-for raw counter native setf step');
# ++-step carve-out (session 269): a PURE `$j++` step is position-known (its
# value is discarded), so it lowers as a native setf and the counter unboxes.
my $cfor2 = Pl::Parser2->parse_code(
  'for (my $j = 0; $j < 3; $j++) { print "$j\n"; }');
like($cfor2, qr/\(let \(\(\$j 0\)\)/, 'C-for pure ++ step: counter unboxes (carve-out)');
like($cfor2, qr/\(\(setf \$j \(p-\+ \$j 1\)\)\)/, 'C-for pure ++ step lowered as native setf');
# … but a ++ anywhere ELSE (body) still forces the boxed path.
my $cfor3 = Pl::Parser2->parse_code(
  'for (my $j = 0; $j < 6; $j++) { $j++; print "$j\n"; }');
like($cfor3, qr/\(\$j \(make-p-box nil\)\)/, 'C-for with body ++ keeps counter boxed');
like($cfor3, qr/\(p-post\+\+ \$j\)/, 'C-for boxed step through p-post++');

# Lean p-sub: a body that never reads @_ skips the p-args-body prologue and
# stack-allocates the unused &rest.
like($rec, qr/\(declare \(ignore %_args\) \(dynamic-extent %_args\)\)/,
     'no-@_ sub: rest ignored + dynamic-extent, no p-args-body');
unlike($rec, qr/p-args-body/, 'no p-args-body when @_ is unused');

# goto &sub forwards the LIVE @_: a sub containing goto must keep the boxed
# @_ convention (p-args-body) so p-goto-sub has the full argument list.
my $goto = Pl::Parser2->parse_code(
  'sub target { my ($x, $y) = @_; print "$x $y\n"; } sub fwd { my ($a) = @_; goto &target; } fwd(7, 9);');
like($goto, qr/\(p-sub pl-fwd\s*\n?\s*\(&rest %_args\)\s*\n?\s*\(p-args-body/s,
     'goto-containing sub keeps p-args-body (@_ live for forwarding)');
like($goto, qr/\(p-goto-sub #'pl-target\)/, 'goto &sub lowers via p-goto-sub');

# next/last lower through the fallback inside v2 loops.
my $brk = Pl::Parser2->parse_code(
  'for my $i (1..10) { next if $i == 2; last if $i > 4; print "$i\n"; }');
like($brk, qr/p-next/, 'next lowers');
like($brk, qr/p-last/, 'last lowers');

# --- Session-269 growth: non-scalar my, use/require seam, native interp ---

# my @a / my %h / my (LIST) let-bind fresh containers; the assignment lowers
# through the original expression machinery (p-array-= / p-hash-= / p-list-=).
my $agg = Pl::Parser2->parse_code(
  'my @a = (1,2,3); my %h = (x => 9); my ($p, $q) = (4, 5); print $a[0]+$h{x}+$p+$q, "\n";');
like($agg, qr/\(let \(\(\@a \(make-array 0 :adjustable t :fill-pointer 0\)\)\)/,
     'my @a binds a fresh adjustable vector');
like($agg, qr/\(p-array-= \@a \(vector 1 2 3\)\)/, 'array init via p-array-=');
like($agg, qr/\(let \(\(%h \(make-hash-table :test 'equal\)\)\)/,
     'my %h binds a fresh hash table');
like($agg, qr/\(p-list-= \(vector \$p \$q\)/, 'my (LIST) init via p-list-=');

# use/require lower through the statement-level fallback seam: declarations
# hoisted to the top, runtime effects in statement position.
my $use = Pl::Parser2->parse_code(
  'use constant PI => 3; print PI() + 1, "\n";');
like($use, qr/\(p-sub pl-PI/, 'use constant: captured declaration hoisted');
# (v1 parity: `require` is an eval-always definition, hoisted ABOVE runtime
# statements — same bucket ordering as the v1 assembly.)
my $ord = Pl::Parser2->parse_code(
  'print "a\n"; require POSIX; print "b\n";');
like($ord, qr/\(p-eval-always\s*\n?\s*\(p-require "POSIX"\)\).*\(p-print "a/s,
     'require hoisted as eval-always declaration (v1 parity)');

# Native interpolated strings: plain $name scalars → p-string-concat form
# (a raw root — the slot unboxes); fancy interpolations stay on the fallback.
my $interp = Pl::Parser2->parse_code(
  'my $x = 5; my $msg = "x is $x!\n"; print $msg;');
like($interp, qr/\(p-string-concat "x is " \$x "!\s*\n?"\)/,
     'simple $name interpolation lowers natively to p-string-concat');
like($interp, qr/\(\$msg\s*\n?\s*\(p-string-concat/, 'interpolated-string slot binds raw');

# Unary ! is native and raw-rooted.
my $bang = Pl::Parser2->parse_code('my $x = 0; my $y = !$x; print "y=[$y]\n";');
like($bang, qr/\(let \(\(\$y \(p-! \$x\)\)\)/, 'unary ! native, raw slot');

# End-to-end: v2 output runs and matches perl.
SKIP: {
  skip 'sbcl not available', 1 unless grep { -x "$_/sbcl" } split /:/, $ENV{PATH};
  my $root = "$FindBin::Bin/../..";
  my $tmp = "/tmp/parser2-01-$$.lisp";
  open my $fh, '>', $tmp or die $!;
  my $out = $cl;
  $out =~ s/\(in-package :pcl\)/(in-package :pcl)\n(p-defpackage :main)\n(in-package :main)/;
  print $fh $out;
  close $fh;
  my $got = `sbcl --control-stack-size 512 --noinform --non-interactive --load "$root/cl/pcl-runtime.lisp" --eval "(setf pcl::*pcl-skip-cache* t)" --load "$tmp" 2>/dev/null | tail -1`;
  chomp $got;
  is($got, '832040', 'v2-transpiled loop-fib(30) runs correctly');
  unlink $tmp;
}

done_testing();

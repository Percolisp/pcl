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

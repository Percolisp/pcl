#!/usr/bin/env perl
# Pl/t/parser2-01.t — v2 pipeline prototype (Pl::Parser2 / Pl::ExprToCL2 /
# Pl::VarAnnotator / Pl::CLForm).  Shape checks on generated CL + one
# end-to-end run.  The v2 pipeline is the DEFAULT (W9; PCL_V1=1 = v1 escape
# hatch); these tests call Parser2 directly regardless of env and pin the
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

# --- Session-270 growth: `package` statements (section splitting) ---

# Statement-form `package Foo;` opens a new output SECTION whose (in-package)
# preamble puts the READER in the right CL package (mirrors v1's model).
my $pkg = Pl::Parser2->parse_code(<<'EOF');
sub hi { return "main-hi"; }
print hi(), "\n";
package Foo;
sub hi { return "foo-hi"; }
print hi(), "\n";
package main;
print hi(), "\n";
EOF
like($pkg, qr/^\(pcl:p-defpackage :Foo\)/m, 'later packages predeclared at file top');
like($pkg, qr/\(p-defpackage :Foo\)\n\(in-package :Foo\)/, 'package section preamble enters :Foo');
like($pkg, qr/\(defclass plc-foo \(\) \(\)\)/, 'CLOS class for MRO in the section preamble');
like($pkg, qr/\(p-set-current-package :Foo "Foo"\)/, 'runtime current-package tracking');
like($pkg, qr/\(in-package :main\)/, 'package main section returns the reader to :main');
my $hi_defs = () = $pkg =~ /\(p-sub pl-hi /g;
is($hi_defs, 2, 'same-named sub defined once per package section');

# W10: a qualifying my-lexical spanning a package boundary is renamed to a
# package-level cell (defvar'd in the declaring section; package-qualified in
# later sections) instead of gating — v1 CRASHES on this shape (s270 bug).
# A non-qualifying shape (interpolated use — the token rename can't reach it)
# still dies → v1.  When the name has exactly ONE binding file-wide, the cell
# keeps the PLAIN name (no __file__N mangle) — there is no sibling `let` to
# poison, and the unmangled global stays visible to a dynamic string eval that
# references the bare name (s278c).
my $span = eval { Pl::Parser2->parse_code(qq{my \$x = 1;\npackage Foo;\nprint \$x;\n}) };
is($@, '', 'W10: qualifying my across a package boundary lowers natively');
like($span, qr/\(defvar \$x \(make-p-box nil\)\)/,
     'W10: spanning lexical gets a defvar cell in the declaring section');
unlike($span, qr/\$x__file__\d+/,
     'W10: a file-unique spanning name is NOT mangled (plain $Pkg::name)');
like($span, qr/main::\$x\b/,
     'W10: later section reads the package-qualified cell');
my $span_interp = eval { Pl::Parser2->parse_code(qq{my \$x = 1;\npackage Foo;\nprint "\$x";\n}) };
like($@, qr/spans a package boundary/,
     'W10: interpolated spanning lexical still dies to v1');

# W5: a single scalar file lexical captured by a NAMED sub is rewritten to a
# package-level cell — defvar'd, NOT let-bound — so the hoisted sub and
# in-place code share the box.  A FILE-UNIQUE name keeps its own name
# (identity, like the span pass — no __file__N mangle), so string eval and
# interp text keep resolving.
my $capt = Pl::Parser2->parse_code(q{my $n = 1; sub bump { $n + 1 } print bump(), "\n";});
like($capt, qr/\(defvar \$n \(make-p-box nil\)\)/,
     'W5: captured file-unique scalar gets an identity defvar cell');
unlike($capt, qr/\$n__file__\d+/, 'W5: file-unique captured name is NOT mangled');
unlike($capt, qr/\(let \(\(\$n\b/, 'W5: promoted cell is NOT let-bound');
# A NON-unique name (block-nested shadow elsewhere) takes the mangled path;
# the shadow scope keeps its own name (M-C shadow-aware count + rewrite).
my $captm = Pl::Parser2->parse_code(
  q{my $n = 1; sub bump { $n + 1 } { my $n = 9; print $n; } print bump(), "\n";});
like($captm, qr/\(defvar \$n__file__\d+ \(make-p-box nil\)\)/,
     'W5: shadowed captured lexical gets a MANGLED defvar cell');
like($captm, qr/\(p-scalar-= \$n__file__\d+ 1\)/, 'W5: renamed cell assigned in place');
like($captm, qr/\(let \(\(\$n (?:9|\(make-p-box)/,
     'W5: the block shadow keeps its own let-bound $n');

# …but a case OUTSIDE the subset still gates → whole-file v1: an interpolated
# use ($n inside a string) can't be rewritten by token content, so it must die.
my $capt_i = eval { Pl::Parser2->parse_code(q{my $n = 1; sub bump { "got $n" } print bump(), "\n";}) };
like($@, qr/captured by sub/, 'interpolated captured lexical still dies to v1');

# …and two declarations of the same name (shadowing) stay gated.
my $capt_sh = eval { Pl::Parser2->parse_code(q{my $n = 1; sub bump { $n + 1 } my $n = 2; print $n;}) };
like($@, qr/captured by sub/, 'shadowed captured lexical still dies to v1');

# … but the same name confined to top-level statements is still v2-lowered.
my $nocapt = Pl::Parser2->parse_code(q{my $n = 1; sub bump { my ($m) = @_; return $m + 1 } print bump($n), "\n";});
like($nocapt, qr/\(let \(\(\$n 1\)\)/, 'sub with its own params does not block v2');

# Octal literals must NOT enter the native subset (CL reads 0100 as 100).
my $oct = Pl::Parser2->parse_code(q{my $o = 0100; print "$o\n";});
like($oct, qr/\(\$o #o100\)/, 'octal literal routed to fallback (#o100), not bare 0100');

# --- `our` declarations: package vars — defvar hoisted to the section top,
# no let, assignment through the ordinary machinery.
my $our = Pl::Parser2->parse_code(<<'EOF');
our $count = 3;
our @list = (1, 2);
our ($p, $q) = (7, 8);
sub show { return $count + $p + $q; }
print show(), "\n";
package Dog;
our @ISA = ('Animal');
EOF
like($our, qr/^\(defvar \$count \(make-p-box nil\)\)/m, 'our $x defvar hoisted');
like($our, qr/^\(defvar \@list /m, 'our @a defvar hoisted');
like($our, qr/\(p-scalar-= \$count 3\)/, 'our $x init via p-scalar-= (box-set clears sv/nv caches; a raw p-box-value setf reads back stale — D23)');
like($our, qr/\(p-list-= \(vector \$p \$q\)/, 'our (LIST) init via p-list-=');
unlike($our, qr/\(let \(\(\$count/, 'our vars are not let-bound');
like($our, qr/\(in-package :Dog\).*\(defvar \@ISA /s, 'our @ISA defvar lands in its package section');
my $ourshadow = eval { Pl::Parser2->parse_code(q{my $x = 1; our $x; print $x;}) };
like($@, qr/shadows a my-lexical/, 'our shadowing a my-lexical dies to v1');

# --- W1 (session 272): package block form `package Foo { … }` + versions ---

# Block form: full preamble ONCE for the block's package, then a short-form
# RETURN section that only re-enters the enclosing package's reader.
my $blk = Pl::Parser2->parse_code(<<'EOF');
package Animal { sub speak { return "generic" } }
print __PACKAGE__, "\n";
EOF
my $animal_pre = () = $blk =~ /\(p-defpackage :Animal\)\n\(in-package :Animal\)/g;
is($animal_pre, 1, 'block-form package emits its full preamble exactly once');
like($blk, qr/;;; back to package main\n\(in-package :main\)/,
     'return section uses only (in-package) — no re-defpackage of main');
unlike($blk, qr/;;; back to package main\n\(p-defpackage/,
       'return section does NOT re-emit p-defpackage for the enclosing package');

# Versioned package: $VERSION defvar (eval-when) + a source-order assignment.
my $ver = Pl::Parser2->parse_code(qq{package Counter 1.5;\nprint \$Counter::VERSION;\n});
like($ver, qr/\(defvar Counter::\$VERSION \(make-p-box nil\)\)/,
     'versioned package defvars $VERSION');
like($ver, qr/\(p-scalar-= Counter::\$VERSION 1\.5\)/,
     'versioned package assigns $VERSION in source order');

# --- W2 (session 272): string-eval gate is now a PPI walk, not a text scan ---

# eval BLOCK is fine (no lexical-capture problem) — must lower natively.
my $eb = eval { Pl::Parser2->parse_code(q{my $x = eval { 1 + 2 }; print "$x\n";}) };
ok(!$@, 'eval BLOCK does not trip the string-eval gate') or diag($@);
# eval STRING now lowers natively (W3): it flows through the expression
# fallback seam and emits (p-eval STR (list (cons "$x" $x) …)) — no gate.
my $es = eval { Pl::Parser2->parse_code(q{my $x = 1; print eval '$x + 1', "\n";}) };
ok(!$@, 'eval STRING no longer gates (W3)') or diag($@);
like($es, qr/\(p-eval "\$x \+ 1" \(list \(cons "\$x" \$x\)\)\)/,
     'eval STRING captures the in-scope lexical $x in the alist');
# A closed sibling scope's lexical must NOT appear in the alist (it would be a
# free CL symbol → unbound at load).  _let_bound_vars is scoped for this (W3).
my $ecs = Pl::Parser2->parse_code(
  q[{ my $dead = 5; print "$dead\n"; } my $y = 2; print eval '$y + 1', "\n";]);
like($ecs, qr/\(p-eval "\$y \+ 1" \(list \(cons "\$y" \$y\)\)\)/,
     'eval alist captures $y only — not the closed-scope $dead');
unlike($ecs, qr/cons "\$dead"/, 'closed-scope lexical excluded from eval alist');
# The old text scan tripped on `eval` mentions that are not string eval; the
# PPI walk excludes a `=>` hash key and a `->eval` method call.
my $ek = eval { Pl::Parser2->parse_code(q{my %h = (eval => 1); print $h{k};}) };
ok(!$@, 'eval as a fat-comma hash key does not gate') or diag($@);
my $em = eval { Pl::Parser2->parse_code(q{my $r = 0; my $v = $r->eval();}) };
ok(!$@, 'eval as a method call does not gate') or diag($@);

# Bodyless forward declaration `sub foo;` — v1 emits (p-declare-sub); v2 must
# too (not crash on $sub->block), with NO definition.
my $fwd = Pl::Parser2->parse_code(q{sub t1; sub u; print "hi\n";});
like($fwd, qr/\(p-declare-sub pl-t1\)/, 'forward-declared sub emits p-declare-sub');
unlike($fwd, qr/\(p-sub pl-t1\b/, 'forward declaration emits no definition');

# W3 regression: a foreach loop variable must be SCOPED to the loop body.  If it
# leaked into _let_bound_vars, a later sibling eval's capture alist would list
# the (now-unbound) loop var → unbound-variable at load (cmpchain.t crash).
my $fev = Pl::Parser2->parse_code(
  q[foreach my $e (1,2) { print $e; } my $z = 9; print eval '$z';]);
like($fev, qr/\(p-eval "\$z" \(list \(cons "\$z" \$z\)\)\)/,
     'foreach loop var does not leak into a later sibling eval alist');
unlike($fev, qr/cons "\$e"/, 'closed foreach var excluded from later eval alist');

# W3 regression: the native attempt runs PExpr's cleanup_for_parsing, which
# mutates the shared `=>` token to `,`.  _lower_expr must restore token content
# so the fallback still sees `=>` and auto-quotes the bareword (else `(N=>1)`
# lowers N as a call → undefined-function crash, seen in tr.t).
my $fc = Pl::Parser2->parse_code(q{%h = (N=>1); print "hi\n";});
like($fc, qr/\(vector "N" 1\)/, 'fat-comma bareword before => stays a string, not a call');
unlike($fc, qr/pl-N\b/, 'single-char bareword before => is not lowered as a funcall');

# --- W4 (session 272d): prototype/signature subs ---

# A prototyped sub is lowered by v1 via the fallback (imposed context); its
# definition lands in the captured-decls / definitions region, not a native
# (p-sub) from _lower_sub.  The `($)` prototype imposes scalar context on the arg.
my $pr = Pl::Parser2->parse_code(
  q{sub takes_scalar ($) { my ($n) = @_; return $n; } my @l=(7,8,9); print takes_scalar(@l);});
like($pr, qr/\(p-sub pl-takes_scalar/, 'prototyped sub definition still emitted (via v1)');
like($pr, qr/\(p-scalar .*pl-takes_scalar|pl-takes_scalar.*p-scalar|p-scalar/,
     'prototype `($)` imposes scalar context at the call site');
# A signature sub (PPI::Structure::Signature, ->prototype undef) also routes to
# v1 — which emits the arity check + @_ binding, not a bare (&rest %_args).
my $sig = Pl::Parser2->parse_code(
  qq{use feature 'signatures';\nsub greet (\$name, \$g = "hi") { return "\$g \$name"; }\nprint greet("bob");});
like($sig, qr/p-check-arity/, 'signature sub gets v1 arity-check binding (not a bare &rest)');

# W4 regression (arith.t): a top-level `my` nests its whole block in one `let`;
# an oversized body must be wrapped in (locally (declare (notinline …))) so the
# R1 inline hot ops do not blow up SBCL's compiler (v1's _cap_inlining_if_huge).
my $big = "my \$T = 0;\n" . ("\$T = \$T + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9;\n" x 400);
my $bigcl = Pl::Parser2->parse_code($big);
like($bigcl, qr/\(locally \(declare \(notinline/,
     'oversized top-level my-block is wrapped to cap inline expansion');

# --- A3 (session 271): local / statement modifiers / for(;;) ---

# `local` lowers through v1's machinery; the opened save/restore scope wraps
# the lowered block remainder (raw_wrap), and the printer's appended closes
# keep the whole output balanced.
my $loc = Pl::Parser2->parse_code(q{local $/ = undef; my $s = 1; print "$s\n";});
like($loc, qr/\(let \(\(\$\/ \(p-box-for-local/, 'local $/ = … opens v1 p-box-for-local let');
like($loc, qr/\(p-box-for-local[^\n]*\n.*\(p-print/s, 'block remainder nested inside the local scope');
is(paren_balance($loc), 0, 'raw_wrap closes balance the local open');

# Degenerate local that opens no scope embeds as a plain statement.
my $loclen = Pl::Parser2->parse_code(q{my @a = (1,2,3); local $#a = 1; print "$a[1]\n";});
is(paren_balance($loclen), 0, 'local $#a (no scope opened) stays balanced');

# while/until/for/foreach statement modifiers route through the per-statement
# fallback; the written var stays boxed (fallback writes are box-ops).
my $fe = Pl::Parser2->parse_code(q{my $t = 0; $t = $t + $_ foreach 1..3; print "$t\n";});
like($fe, qr/\(p-foreach \(\$_ \(p-\.\. 1 3\)\) \(p-my-= \$t/, 'foreach modifier via per-statement fallback');
unlike($fe, qr/\(let \(\(\$t 0\)\)/, 'modifier-written var stays boxed');
my $dw = Pl::Parser2->parse_code(q{my $x = 5; do { $x--; } while ($x > 3); print "$x\n";});
like($dw, qr/p-do-while/, 'do-while via per-statement fallback');

# for(;;) — empty sections are native: no init/step, cond defaults to t.
my $inf = Pl::Parser2->parse_code(q{for (;;) { last; } print "done\n";});
like($inf, qr/\(p-for \(\) \(t\) \(\)/, 'for(;;) native: empty init/step, cond t');
my $nostep = Pl::Parser2->parse_code(q{for (my $i = 0; $i < 3;) { $i = $i + 1; } print "ok\n";});
like($nostep, qr/\(let \(\(\$i 0\)\)\s*\(p-for \(\) \(\(p-< \$i 3\)\) \(\)/,
     'for with empty step: raw counter, empty step slot');

# Loop-condition auto-defined rewrites (v1's _auto_defined_cond at the raw
# seam): bare <FH> assigns $_ implicitly; my-scalar readline gets p-defined.
my $rl = Pl::Parser2->parse_code(q{while (<STDIN>) { print; }});
like($rl, qr/\(progn \(p-setf \$_ .*p-readline.*\(p-defined \$_\)\)/s,
     'bare <FH> while cond: implicit $_ assign + defined wrap');
my $rl2 = Pl::Parser2->parse_code(q{while (my $l = <STDIN>) { print $l; }});
like($rl2, qr/\(p-defined \$l\)/, 'while (my $l = <FH>) cond terminates on undef');

# W6: while/foreach continue blocks lower natively to a :continue (progn …)
# loop key, placed AFTER the body (parse-loop-keys finds it by position).
my $cont = Pl::Parser2->parse_code(
  q{my $i = 0; while ($i < 3) { print $i; } continue { $i = $i + 1; }});
like($cont, qr/\(p-while .*:continue \(progn/s, 'while continue block → :continue key');
# …but a BARE-block continue (LABEL: { … } continue { … }) stays gated → v1
# (v1 runs it after the tagbody — different shape).
my $bcont = eval { Pl::Parser2->parse_code(
  q{L: { print "x"; } continue { print "y"; }}) };
like($@, qr/continue/, 'bare-block continue block still dies to v1');

# --- A2 (session 271): bare blocks (loop-once) + labels + nested subs ---

# Unlabeled bare block = single-iteration loop: last/next/redo must work.
my $bare = Pl::Parser2->parse_code(q[{ my $y = 5; print "$y\n"; last; } print "after\n";]);
like($bare, qr/\(block nil\s*\n?\s*\(tagbody :redo/, 'bare block lowers to loop-once tagbody');
like($bare, qr/\(tagbody :redo.*\(let \(\(\$y.*:next/s, 'my inside bare block nests inside the tagbody');

# Labeled bare block gets the LAST/NEXT/REDO catch tags for dynamic throws.
my $skip = Pl::Parser2->parse_code(q[SKIP: { last SKIP; print "no\n"; } print "yes\n";]);
like($skip, qr/\(block SKIP/, 'labeled bare block: (block LABEL)');
like($skip, qr/\(catch \(pcl::%pcl-loop-tag "LAST" 'SKIP\)/, 'LAST catch tag present');
like($skip, qr/\(catch \(pcl::%pcl-loop-tag "REDO" 'SKIP\)/, 'REDO catch tag present');

# Loop labels ride into the loop macros as :label keys.
my $lw = Pl::Parser2->parse_code(q[OUTER: while (1) { last OUTER; }]);
like($lw, qr/\(p-while 1 :label OUTER/, 'labeled while: :label key');
my $lf = Pl::Parser2->parse_code(q[LOOP: for my $i (1..3) { next LOOP; }]);
like($lf, qr/\(p-foreach \(\$i \(p-\.\. 1 3\)\) :label LOOP/, 'labeled foreach: :label key');

# Named subs nested in blocks are package-global: hoisted to the defs bucket.
my $nest = Pl::Parser2->parse_code(
  q[{ sub geta { return getb() + 0; } sub getb { return 7; } my $x = geta(); print "$x\n"; }]);
like($nest, qr/\(p-sub pl-geta/, 'nested named sub hoisted to definitions');
like($nest, qr/\(p-declare-sub pl-geta\)/, 'hoisted sub gets p-declare-sub');

# W5: the static-variable idiom (block lexical captured by a nested sub) is
# rewritten to a shared package cell — no gate.
my $capt2 = Pl::Parser2->parse_code(q[{ my $x = 0; sub bump2 { $x = $x + 1; return $x; } } print bump2(), "\n";]);
like($capt2, qr/\(defvar \$x__file__\d+ \(make-p-box nil\)\)/,
     'W5: static-variable idiom gets a defvar cell');
like($capt2, qr/\(p-sub pl-bump2/, 'W5: capturing nested sub still hoisted');

# …and an array block lexical captured by a nested sub (with init) is now
# promoted too (M-D, task #50): sigil-aware rename + init kept as a
# write-through assignment.  This shape used to gate → v1, which MISCOMPILED
# it (s282b edge probes).
my $capt2a = Pl::Parser2->parse_code(q[{ my @x = (0); sub bump2a { push @x, 1 } } print "ok\n";]);
like($capt2a, qr/\(defvar \@x__file__\d+ /,
     'M-D: array block lexical captured by nested sub gets a defvar container');
like($capt2a, qr/\(p-array-= \@x__file__\d+ /,
     'M-D: promoted container keeps its init as a write-through assignment');

# …and a SIBLING scope's same-named lexical must NOT block the hoist.
my $sib = Pl::Parser2->parse_code(
  q[{ my $u = 1; print "$u\n"; } { sub getu { my $u = 2; return $u; } print getu(), "\n"; }]);
like($sib, qr/\(p-sub pl-getu/, 'closed sibling-scope lexical does not block sub hoist');

# VarAnnotator: a list-assignment LHS is a write — the var must stay boxed
# (a raw slot in (vector $a $b) would silently drop the p-list-= write).
my $la = Pl::Parser2->parse_code(q[my $a = 7; my $b = 0; ($a, $b) = (1, 2); print "$a $b\n";]);
unlike($la, qr/\(let \(\(\$a 7\)\)/, 'list-assigned scalar stays boxed');

# CLForm: a raw `;;` comment chunk must never be flattened onto one line
# with following siblings (the comment would swallow them).
my $cmt = Pl::Parser2->parse_code(q[{ no warnings 'syntax'; print "x\n"; }]);
is(paren_balance($cmt), 0, 'raw comment chunk inside a form stays balanced');
unlike($cmt, qr/;;[^\n]*\(p-print/, 'no code swallowed after a raw comment');

# in_subroutine: bare shift inside a v2-lowered sub body defaults to @_.
# The remainder reads $_[0] so the W14 coalesce is disqualified and the
# statement still lowers through the seam (a plain `my $r = shift;` run now
# coalesces to a lambda list and emits no p-shift at all — see parser2-02.t).
my $shf = Pl::Parser2->parse_code(
  q[sub take { my $r = shift; return $r + $_[0]; } print take(5, 2), "\n";]);
like($shf, qr/\(p-shift \@_\)/, 'bare shift in sub body defaults to @_ (not @ARGV)');

# End-to-end: v2 output runs and matches perl.
SKIP: {
  skip 'sbcl not available', 7 unless grep { -x "$_/sbcl" } split /:/, $ENV{PATH};
  my $root = "$FindBin::Bin/../..";
  my $run = sub {
    my ($src) = @_;
    my $tmp = "/tmp/parser2-01-$$.lisp";
    open my $fh, '>', $tmp or die $!;
    (my $out = $src) =~ s/\(in-package :pcl\)/(in-package :pcl)\n(p-defpackage :main)\n(in-package :main)/;
    print $fh $out;
    close $fh;
    # Set the pl2cl path so `p-eval` (string eval) can spawn the transpiler.
    my $p2c = "$root/pl2cl";
    my $got = `sbcl --control-stack-size 512 --noinform --non-interactive --load "$root/cl/pcl-runtime.lisp" --eval "(setf pcl::*pcl-skip-cache* t)" --eval "(setf pcl::*pcl-pl2cl-path* #P\\"$p2c\\")" --load "$tmp" 2>/dev/null`;
    unlink $tmp;
    return $got;
  };
  my $got = $run->($cl);
  chomp(my ($last) = (split /\n/, $got)[-1]);
  is($last, '832040', 'v2-transpiled loop-fib(30) runs correctly');
  is($run->($pkg), "main-hi\nfoo-hi\nmain-hi\n",
     'package sections run end-to-end: per-package sub dispatch');
  # local: dynamic binding visible through a call, restored after return.
  my $locprog = Pl::Parser2->parse_code(<<'EOF');
sub g { return $val; }
sub f { local $val = "in"; return g(); }
$val = "out";
print f(), "-", g(), "\n";
EOF
  is($run->($locprog), "in-out\n", 'local end-to-end: dynamic scope + restore');
  # Bare blocks + labels: last-in-block, last LABEL, next OUTER, redo count.
  my $blocks = Pl::Parser2->parse_code(<<'EOF');
my $x = 0;
{ $x = $x + 1; last if $x > 0; $x = 100; }
SKIP: { last SKIP; $x = 200; }
my $n = 0;
OUTER: for (my $i = 0; $i < 3; $i++) {
  for (my $j = 0; $j < 3; $j++) { next OUTER if $j == 1; $n = $n + 1; }
}
my $r = 0;
{ $r = $r + 1; redo if $r < 3; }
print "$x $n $r\n";
EOF
  is($run->($blocks), "1 3 3\n", 'bare blocks + labels end-to-end');
  # W1: block-form package + return-to-main + cross-package dispatch.
  my $blkprog = Pl::Parser2->parse_code(<<'EOF');
package Animal { sub speak { return "generic" } }
package Dog { our @ISA = ('Animal'); sub name { return "rex" } }
print Dog->speak(), " ", Dog->name(), "\n";
print __PACKAGE__, "\n";
EOF
  is($run->($blkprog), "generic rex\nmain\n",
     'block-form packages run end-to-end: dispatch + return to main');
  # W3: string eval reads AND writes back through the shared captured boxes.
  my $evprog = Pl::Parser2->parse_code(<<'EOF');
my $x = 1; my @a = (1,2,3);
eval '$x = $x + 10; push @a, 9;';
print "$x @a\n";
EOF
  is($run->($evprog), "11 1 2 3 9\n",
     'string eval reads + writes back through captured lexicals');
  # W5: static-variable idiom — a block lexical captured by a nested named sub
  # runs through the shared package cell.
  my $w5prog = Pl::Parser2->parse_code(
    q[{ my $count = 0; sub bump { $count = $count + 1; return $count; } } print bump(), bump(), bump(), "\n";]);
  is($run->($w5prog), "123\n", 'W5: captured file lexical runs end-to-end');
}

# s285 — foreach over a single aliasable ELEMENT ($h{k} / $a[i]) binds the loop
# var to the box-returning form so a write persists.  Element shapes de-gate
# natively; the MAGIC-lvalue shape (substr/pos/vec) still gates to v1.
{
  my $he = Pl::Parser2->parse_code(q[my %h = (k=>1); for ($h{k}) { $_++ }]);
  like($he, qr/\(p-foreach \(\$_ \(p-gethash-box %h "k"\)\)/,
       'hash-element foreach aliases via p-gethash-box');
  my $ae = Pl::Parser2->parse_code(q[my @a = (1,2,3); for ($a[1]) { $_++ }]);
  like($ae, qr/\(p-foreach \(\$_ \(p-aref-box \@a 1\)\)/,
       'array-element foreach aliases via p-aref-box');
  # substr/pos/vec magic-lvalue aliasing is still gated → dies to v1.
  my $sub = eval { Pl::Parser2->parse_code(q[my $s="hi"; for (substr($s,0,1)) { $_="J" }]); };
  ok(!defined $sub, 'magic-lvalue foreach (substr) still gates to v1');
  # Bare `return;` is the zero-arg (p-return) (context-sensitive empty/undef),
  # never (p-return (p-undef)) which leaks a 1-element list in list context.
  my $br = Pl::Parser2->parse_code(q[sub f { return; }]);
  like($br, qr/\(p-return\)/, 'bare return emits zero-arg (p-return)');
  unlike($br, qr/\(p-return \(p-undef\)\)/, 'bare return is not (p-return (p-undef))');
}

# CLAUDE.md's paren checker (handles strings, ;-comments, #\( char literals).
# $in_str persists across lines: generated string literals contain newlines.
sub paren_balance {
  my ($s) = @_;
  my ($d, $in_str, $ahb) = (0, 0, 0);
  for my $line (split /\n/, $s) {
    my @c = split //, $line;
    my $i = 0;
    while ($i < @c) {
      my $ch = $c[$i];
      if ($in_str) { if ($ch eq "\\") { $i += 2; next } $in_str = 0 if $ch eq '"' }
      elsif ($ahb) { $ahb = 0 }
      elsif ($ch eq '"') { $in_str = 1 }
      elsif ($ch eq '#' && $i + 1 < @c && $c[$i + 1] eq "\\") { $ahb = 1; $i += 2; next }
      elsif ($ch eq ';') { last }
      elsif ($ch eq '(') { $d++ }
      elsif ($ch eq ')') { $d-- }
      $i++;
    }
  }
  return $d;
}

done_testing();

#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Pl/t/parser2-02.t — v2 pipeline prototype (Pl::Parser2), continuation of
# parser2-01.t.  New work items add their guards HERE to keep each file a
# manageable size.  Same conventions: shape checks call Pl::Parser2->parse_code
# directly (so gates DIE — catch with eval), runtime checks run in the SKIP
# block via $run.  Starts at W6.
use v5.30;
use strict;
use warnings;
use Test::More;
use FindBin;
use lib "$FindBin::Bin/../..";
use Pl::Parser2;

# ---- W6: continue blocks on while/foreach; `my $scalar <non-'=' trailing>` ----

# while continue → :continue (progn …) key, AFTER the body.
my $wc = Pl::Parser2->parse_code(
  q{my $i = 0; while ($i < 3) { print $i; } continue { $i = $i + 1; }});
like($wc, qr/\(p-while .*\(p-print \$i\).*:continue \(progn/s,
     'while continue: :continue key follows the body');

# foreach continue → :continue key too.
my $fc = Pl::Parser2->parse_code(
  q{my @a = (1,2); foreach my $y (@a) { print $y; } continue { print "-"; }});
# Either foreach arm (`print $y` is a pure read → #862 ARM A's raw arm);
# this row's subject is the :continue key, not the loop-variable binding.
like($fc, qr/\(p-foreach(?:-raw)? .*:continue \(progn/s, 'foreach continue: :continue key');

# `my $aa, $bb, $cc;` — Perl declares only $aa (a lexical); $bb/$cc are package
# vars.  Lower as a boxed `my $aa` let + the comma expression discarded.
my $md = Pl::Parser2->parse_code(q{my $aa, $bb, $cc; $bb = 1; print $aa;});
like($md, qr/\(p-let \(\(\$aa :box \(make-p-box nil\)\)\)/, 'my $aa,… : only $aa let-bound');
unlike($md, qr/\(p-let \(\(\$bb/, 'my $aa,… : $bb is NOT let-bound (package var)');
like($md, qr/\(p-defcell \$bb /, 'my $aa,… : $bb forward-declared as a package global');

# `my $a . $foo;` — declares $a, concatenation discarded.
my $dc = Pl::Parser2->parse_code(q{my $foo = "f"; my $a . $foo; print $a;});
like($dc, qr/\(p-let \(\(\$a__excl__\d+ :box \(make-p-box nil\)\)\)/,
     'my $a . $foo : $a let-bound');   # $a is exception-partition → #296 rename

# ---- W8 tail (s273): VarAnnotator write shapes + self-ref my init + our-init ----

# Bitwise compound assigns are writes; since task #62 a coercing compound op
# at native statement root stores to a RAW slot via its -raw macro twin (same
# p-bit-and computation as the boxed macro, so bop-01 t17/18 string-bitwise
# semantics are preserved) — the write must lower through the twin, never a
# box-set on the raw slot.
my $bw = Pl::Parser2->parse_code(q{my $s = "zzzzz"; $s &= "AAAAA"; print $s;});
like($bw, qr/\(p-let \(\(\$s :scalar "zzzzz"\)\)/, 'bitwise &= target goes raw (task #62)');
like($bw, qr/\(p-bit-and=-raw \$s "AAAAA"\)/, '&= lowers via the raw macro twin');

# Paren-less \substr $t: magic write-through ref needs the box (misc-fixes-02 t27).
my $sb = Pl::Parser2->parse_code(q{my $t = ""; ${\substr $t, 0} = "X"; print $t;});
like($sb, qr/\(\$t :box \(make-p-box nil\)\)/, 'paren-less \substr target stays boxed');

# Handle-vivifying builtin writes its FH arg: open($h,…) keeps $h boxed
# (fileio-02 t25 — a raw slot loses the handle association).
my $oh = Pl::Parser2->parse_code(q{my $h = "Log123"; open($h, '<', "/dev/null") or die; print $h;});
like($oh, qr/\(\$h :box \(make-p-box nil\)\)/, 'open($h,…) FH arg stays boxed');

# Self-referencing init `my $i = $i` reads the OUTER $i: the init moves into
# the let BINDING via p-box-init (evaluated in the outer env), never a
# body-position p-my-= that would read the fresh nil box (closure-01 t17).
my $sh = Pl::Parser2->parse_code(q{my $i = 7; { my $i = $i; print $i; } print $i;});
like($sh, qr/\(p-let \(\(\$i :box \(p-box-init \$i\)\)\)/, 'my $i = $i: init in the let binding (outer scope)');

# Scalar our-init emits p-scalar-= (box-set clears sv/nv caches) — a raw
# (setf (p-box-value …)) reads back a stale string cache after a BEGIN wrote
# the box (D20 reverted, D23; pinned end-to-end by begin-end-01 t13/14).
my $oi = Pl::Parser2->parse_code(q{our $config = "default"; print $config;});
like($oi, qr/\(p-scalar-= \$config "default"\)/, 'our $x = RHS init via p-scalar-= (cache-safe)');
unlike($oi, qr/\(setf \(p-box-value \$config\)/, 'our $x = RHS init is NOT a raw p-box-value setf');

# ---- W8.5: shadow renames (seam my-shadow + poisoned condition-my) ----

# A my-shadow of a live lexical inside a fallback block is RENAMED to
# $x__shadow__N (not gated) so the seam gets a fresh non-colliding name.
my $sr = Pl::Parser2->parse_code(
  q{my $x = "outer"; my @r = map { my $x = $_ * 2; $x } (1,2,3); print "@r $x";});
like($sr, qr/\$x__shadow__\d+/, 'seam my-shadow renamed, file stays native');

# An INTERPOLATED use of the shadow inside the block FOLLOWS the rename
# (M-A: _rename_decl_within rewrites interp text via _interp_fixer) — the
# file stays native instead of gating.  Interp text outside the shadow's
# block keeps the OUTER name.
my $si = Pl::Parser2->parse_code(
  q{my $e = "o"; my @r = map { my $e = $_; "($e)" } (1,2); print "@r $e";});
like($si, qr/"\(" \$e__shadow__\d+ "\)"/,
     'interpolated seam-shadow use follows the rename (M-A)');
like($si, qr/\(p-join \|\$"\| \@r\) " " \$e\)/,
     'interp text outside the shadow block keeps the outer $e');

# A condition-my whose name is ALSO a package global (defins.t crash): the two
# roles now share the name — the construct's `my` is a plain `let`, which
# lexically shadows the global's p-defcell symbol macro.  Until #291 the
# lexical was renamed to $name__cond__N, because a `defvar` of the global
# would have made that `let` a dynamic rebind.
my $pc = Pl::Parser2->parse_code(<<'EOF');
my @l = (1,2);
while (my $name = shift @l) { print $name; }
$name = 9;
print $name;
EOF
unlike($pc, qr/__cond__/, 'condition-my keeps its name (the rename is gone)');
like($pc, qr/\(p-let \(\(\$name /, 'the condition-my is let-bound — a lexical shadow');
like($pc, qr/\(p-defcell \$name /, 'the package global $name still gets its declaration');

# A self-contained condition-my (no outside use) is unchanged too.
my $cc = Pl::Parser2->parse_code(
  q{my @l = (1,2); while (my $line = shift @l) { print "$line\n"; }});
unlike($cc, qr/__cond__/, 'self-contained condition-my not renamed');

# ---- W11: native hash/array element access on let-bound containers ----

# Element READ in an operator RHS lowers natively and the accumulator becomes
# a raw slot (the write is arith-shaped; the element read coerces via p-+).
my $ea = Pl::Parser2->parse_code(
  q{my %h; my @a; my $s = 0; $h{x} = 1; $a[3] = 2; $s = $s + $h{x} + $a[3]; print $s;});
like($ea, qr/\(setf \$s \(p-\+ \(p-\+ \$s \(p-gethash %h "x"\)\) \(p-aref \@a 3\)\)\)/,
     'W11: element reads native in arith RHS; accumulator unboxed');
# Plain setf, not the p-setf macro: _elem_place guarantees a let-bound
# container, so the macro's boundp/auto-declare arm is skipped (W15.1).
like($ea, qr/\(setf \(p-gethash %h "x"\) 1\)/, 'W11: hash element write = bare setf (no boundp arm)');
like($ea, qr/\(setf \(p-aref \@a 3\) 2\)/, 'W11: array element write = bare setf (no boundp arm)');
unlike($ea, qr/\(p-setf \(p-gethash/, 'W11: no p-setf macro on let-bound hash writes');

# A BARE element read as a my-init: the element value can itself be a
# reference box, and a raw slot must never receive a box (class-5) — so the
# B-str freeze (all uses stringify) stores its STRING through the strict
# coercer instead.  With an opaque use the variable still boxes.
my $eb = Pl::Parser2->parse_code(q{my %h; $h{k} = 5; my $v = $h{k}; print $v;});
like($eb, qr/\(\$v :str \(%pcl-to-string-strict \(p-gethash %h "k"\) "\$v"\)\)/,
     'W11/B-str: element-read init freezes raw when every use stringifies');
my $eb2 = Pl::Parser2->parse_code(
  q{my %h; $h{k} = 5; my $v = $h{k}; my $w = $v; print $v;});
like($eb2, qr/\(\$v :box \(make-p-box nil\)\)/,
     'W11: element-read init stays boxed once an opaque use exists');

# A non-let-bound (package) container falls back — v1 owns the boundp/
# auto-declare arm.
my $ec = Pl::Parser2->parse_code(q{$G::h{k} = 1; print $G::h{k};});
unlike($ec, qr/\(p-gethash %h /, 'W11: package-hash element access not native-lowered');

# ---- W14: leading `my $x = shift;` run coalesces into the @_ fast path ----

# Single leading shift, clean body → real lambda list, no p-args-body.
my $s1 = Pl::Parser2->parse_code(q{sub f { my $x = shift; return $x + 1; } print f(1);});
like($s1, qr/\(p-raw-params \(\$x\)/,
     'W14: single my $x = shift → p-raw-params fast path');
unlike($s1, qr/pl-f[\s\S]*p-args-body/, 'W14: coalesced sub skips p-args-body');

# Multi-statement run → one slot per shift, in order.
my $s2 = Pl::Parser2->parse_code(
  q{sub f { my $x = shift; my $z = shift; return $x + $z; } print f(1,2);});
like($s2, qr/\(p-raw-params \(\$x \$z\)/,
     'W14: shift run coalesces in order');

# Remainder reading @_ → the rewrite is illegal (shift mutated @_) → old path.
my $s3 = Pl::Parser2->parse_code(
  q{sub g { my $a = shift; return join(",", @_); } print g(1,2);});
like($s3, qr/\(&rest %_args\)[\s\S]*p-args-body/, 'W14: remainder reads @_ → p-args-body kept');

# Remainder with string eval (can observe @_ / lexicals invisibly) → old path.
my $s4 = Pl::Parser2->parse_code(
  q{sub e2 { my $a = shift; return eval "1"; } print e2(1);});
like($s4, qr/\(&rest %_args\)[\s\S]*p-args-body/, 'W14: string eval in remainder → p-args-body kept');

# Interleaved run (`my $t = 1` between shifts): the trailing shift is in the
# remainder → whole rewrite disqualified (conservative version).
my $s5 = Pl::Parser2->parse_code(
  q{sub h3 { my $x = shift; my $t = 1; my $z = shift; return $x + $t + $z; } print h3(1,2);});
like($s5, qr/\(&rest %_args\)[\s\S]*p-args-body/, 'W14: interleaved shift run stays on old path');

# ---- block-form-arg bodies re-hosted IN PLACE (Try-Tiny `catch { $caught = $_ }`) ----

# A block-form-prototype arg's body used to go through v1's
# parse_block_as_function, which emits a top-level `--anon-block-N--` defun
# that the expression seam then HOISTS to the section top, OUTSIDE every
# lexical `let` — so a body referencing a live lexical read an unbound global,
# and the whole file had to gate to v1 (the #26 gate).  Task #78 re-hosts the
# body as an inline lambda AT THE CALL SITE, inside the enclosing let, where
# it closes over the lexical.  These are the INVERSE guards of the old gate
# assertions: what must hold now is "no hoisted defun, and the lambda is
# lexically inside the $caught binding".
{
  my $src_cap = q{
    sub try2 (&;@) { my ($t, @h) = @_; my @r = eval { $t->() }; if ($@) { for my $h (@h) { return $h->($@) } } return @r; }
    sub catch2 (&;@) { my ($b, @rest) = @_; return ($b, @rest); }
    my $caught = "none";
    try2 { die "boom\n" } catch2 { $caught = $_[0] };
    print $caught;
  };
  my $cl = eval { Pl::Parser2->parse_code($src_cap) };
  ok(defined $cl && $cl !~ /--anon-block-/,
     'capturing block-form arg lowers natively — no hoisted defun') or diag($@);
  # The write to $caught must sit INSIDE the `(let (($caught …))` that binds
  # it — the property the hoist destroyed.  Checked by real paren nesting:
  # count depth from the binding let to the assignment and require it never
  # returns to 0.  Strings and char literals are skipped, because a
  # `die "boom\n"` puts a genuine newline inside a CL string and a
  # column/line-based scan would misread its closing line as top level.
  {
    my $open  = index($cl // '', "\n(p-let ((\$caught ");
    my $write = index($cl // '', '(p-my-= $caught (p-aref @_ 0))');
    my ($depth, $min) = (0, 0);
    if ($open >= 0 && $write > $open) {
      my $span = substr($cl, $open + 1, $write - $open - 1);
      while ($span =~ /\G(?:"(?:[^"\\]|\\.)*"|\#\\.|(\()|(\))|.)/gs) {
        $depth++ if $1;
        if ($2) { $depth--; $min = $depth if $depth < $min }
      }
    }
    ok($open >= 0 && $write > $open && $min >= 0 && $depth > 0,
       'the re-hosted lambda sits inside the lexical let it captures')
      or diag("open=$open write=$write depth=$depth min=$min");
  }

  # Same shape with NO lexical capture in the block → same in-place lambda.
  my $src_ok = q{
    sub try2 (&;@) { my ($t, @h) = @_; my @r = eval { $t->() }; if ($@) { for my $h (@h) { return $h->($@) } } return @r; }
    sub catch2 (&;@) { my ($b, @rest) = @_; return ($b, @rest); }
    try2 { die "boom\n" } catch2 { print "caught" };
  };
  my $ok = eval { Pl::Parser2->parse_code($src_ok) };
  ok(defined $ok && $ok !~ /--anon-block-/ && $ok =~ /\(p-sub-frame \(block nil \(p-print "caught"\)\)\)/,
     'non-capturing block-form arg lowers to the same in-place lambda') or diag($@);

  # DECLINE RESIDUE: bodies the embed hook refuses (a `package` statement needs
  # v1's revert wrapper; a named sub / `use` hoists) must ALSO stay in place —
  # they take v1's $return_lambda route, which produces the lambda as text
  # rather than emitting a `--anon-block-N--` defun for the seam to hoist.
  # This is what makes the #26 gate unreachable rather than merely narrower;
  # the gate itself stays as the drain's backstop until E4.1's reachability
  # pass retires it.  Runtime parity for these three is checked against perl
  # in the sweep/board, not here (this file is shape-only outside SKIP).
  for my $r (['package stmt',  'package Foo; $caught = 1'],
             ['named sub',     'sub helper { 7 } $caught = helper();'],
             ['use statement', 'use List::Util qw(sum); $caught = sum(1,2,3);']) {
    my ($what, $body) = @$r;
    my $src = qq{
      sub try2 (&;\@) { my (\$t) = \@_; return \$t->(); }
      my \$caught = "none";
      try2 { $body };
      print \$caught;
    };
    my $out = eval { Pl::Parser2->parse_code($src) };
    ok(defined $out && $out !~ /--anon-block-/,
       "declining body ($what) still lowers in place — no hoisted defun")
      or diag($@);
  }
}

# ---- #226: eval-mode leading `package X;` lowers AS section X ---------------

# An eval string whose first statement is `package X;` used to gate the whole
# eval to v1 (audit family F1, 24 events).  It now stays on v2: the package
# statement is left IN the statement stream for _lower_block's D1-lite path,
# which pushes X onto the Environment while the SECTION package stays the
# eval's root — the `current ne cur_pkg` condition every QUALIFIED-emission
# site keys on.  Runtime parity (X::f callable, our-global, method dispatch,
# __PACKAGE__, capture) is probed against perl in the board/sweep; these are
# the shape guards.
{
  my $cl = eval { Pl::Parser2->parse_code(q{package X1; sub f { 42 } 1},
                                          eval_mode => 1, eval_pkg => 'main') };
  ok(defined $cl, 'eval-mode leading `package X;` no longer gates to v1') or diag($@);
  # The sub must be defined QUALIFIED: the eval body is read in :pcl, so a bare
  # `pl-f` would intern in main while the caller looks up X1::pl-f (the s342g
  # silent-wrong that got that attempt reverted).  This is its INVERSE guard.
  like($cl // '', qr/\(p-sub X1::pl-f\b/,
       '#226: the region\'s sub is defined qualified into X, not bare in main');
  unlike($cl // '', qr/\(p-sub pl-f\b/, '#226: and NOT bare (s342g inverse guard)');
  # The package switch must lead the BODY, ahead of the defs/sched interleave:
  # a `use` in the region lowers into sched, and its import records the package
  # in effect (Role-Tiny create-hook.t got 'main' when the switch came after).
  my $u = eval { Pl::Parser2->parse_code(q{package X2; use List::Util qw(sum); 1},
                                         eval_mode => 1, eval_pkg => 'main') };
  ok(defined $u && $u =~ /p-set-current-package :X2[\s\S]*p-use "List::Util"/,
     '#226: p-set-current-package precedes the region\'s `use`') or diag($@);
  # ... and that `use` must name its import target, because the reader package
  # is still :pcl (v1's existing `:into` branch, fed by the seam).
  like($u // '', qr/:into "X2"/, '#226: the region\'s `use` imports :into X');

  # #240 step 2 (RULED s349 §2c): NOTHING in a single-region eval is refused
  # any more.  The three shapes that used to gate (or to be silently wrong)
  # were one bug — an unqualified name inside the region resolved in the
  # CALLER's package — and p-eval-thunk's region-package argument binds
  # *package* to X around both the free-name resolution and the body.  These
  # rows assert the MECHANISM; the VALUES are Pl/t/transpile-test-09.t's
  # 'eval package-region: unqualified names resolve in X' row, against perl.
  my $o = eval { Pl::Parser2->parse_code(q{package X3; our $Z = 5; $Z * 2},
                                         eval_mode => 1, eval_pkg => 'main') };
  ok(defined $o, '#240 step 2: an `our` READ BACK collapses natively') or diag($@);
  # (Since s411 Phase A the one generator resolves `$Z` to X3::$Z at compile
  # time, so the free-name list is EMPTY here — the deleted native generator
  # left the bare `$Z` for the thunk to resolve at run time.  The designator
  # is still carried: it binds *package* around the body for the names that
  # DO stay free — a symbolic reference, an eval-in-eval.)
  like($o // '', qr/p-eval-thunk \(list ?\)[\s\S]*X3::\$Z[\s\S]*\) :X3\)/,
       '#240 step 2: the thunk carries the region package designator (and $Z resolves in X3 at compile time)');
  # INVERSE guard: the region package must not leak into an eval that has NO
  # region — there *package* stays the caller's, which is what perl says.
  my $nr = Pl::Parser2->parse_code(q{my $qq; $qq + 1},
                                   eval_mode => 1, eval_pkg => 'main');
  unlike($nr, qr/p-eval-thunk[\s\S]*\) :\w+\)/,
         '#240 step 2: a region-less eval gets NO region argument');
  # A region with NO free names emits the thunk anyway — the binding's main
  # job is the BODY (a `use`, a bare sub install, a symbolic write), and that
  # shape is the board's dominant one (s350 §2b: 108 region events).
  my $nf = eval { Pl::Parser2->parse_code(q{package X9; sub f { 1 } 1},
                                          eval_mode => 1, eval_pkg => 'main') };
  like($nf // '', qr/p-eval-thunk \(list \)[\s\S]*\) :X9\)/,
       '#240 step 2: a region with no free names still gets the thunk + X') or diag($@);
  # Write-only `our`, a later WRITE of an `our` name, and a symbolic deref in
  # an `our` region: all three were s348 gate rows, all now native.
  for my $w (q{package X4; our $VERSION = "1.25"; 1},
             q{package X5; our @ISA = ("Exporter"); 1},
             q{package X6; our $Z; $Z = 5; 1},
             q{package X7; our $Z = 5; my $n = "Z"; ${$n}},
             q{package X8; my $n = "Z"; ${$n}}) {
    my $ok = eval { Pl::Parser2->parse_code($w, eval_mode => 1,
                                            eval_pkg => 'main') };
    ok(defined $ok, "#240 step 2: collapses natively [$w]") or diag($@);
  }

  # The multi-switch shape stays refused (zero measured events).
  eval { Pl::Parser2->parse_code(q{package A1; sub a {1} package B1; sub b {2} 1},
                                 eval_mode => 1, eval_pkg => 'main') };
  # The refusal text became perl-shaped at the E4.1 flip (#242): it now reaches
  # `$@` as an ordinary trappable Perl error, not a "Parser2 TODO:" note that
  # used to key a silent v1 retry.  The assertion — still refused — is unchanged.
  like($@, qr/^PCL: unsupported in string eval: multiple package sections/,
       '#226: two package sections still refused');
}

# ---- state in named subs: native per-sub cell (rename family __state__N) ----

{
  my $st = Pl::Parser2->parse_code(
    q{use feature 'state'; sub c { state $n = 0; $n = $n + 1; return $n; } print c();});
  like($st, qr/\(p-defcell \$n__state__0 \(make-p-box nil\)\)/,
       'state: per-sub cell hoisted as a declared box');
  like($st, qr/\(unless \$n__state__0__init \(box-set \$n__state__0 0\) \(setf \$n__state__0__init t\)\)/,
       'state: guarded once-init in v1 shape');
  unlike($st, qr/\(p-let \(\(\$n__state__0/, 'state: cell is never let-bound');

  # No init → cell only, no flag.
  my $sp = Pl::Parser2->parse_code(
    q{use feature 'state'; sub t2 { state $k; return $k; } print t2();});
  like($sp, qr/\(p-defcell \$k__state__0 /, 'state without init: cell declared');
  unlike($sp, qr/__init/, 'state without init: no once-flag');

  # s415 (#401 half): a state shadowing a my in the same sub no longer gates —
  # _rename_decl_within is shadow-aware (#254 B-ii) and region-limited
  # (#296-B2), so the named-sub route passes shadow_ok and the shape lowers
  # natively (probed vs perl 5.40.3: prints 22, the state masks the my).
  my $g1 = eval { Pl::Parser2->parse_code(
    q{use feature 'state'; sub s2 { my $n = 1; state $n = 2; return $n; } print s2();}) };
  is($@, '', 'state shadowing a my in the same sub no longer gates');
  like($g1, qr/\(p-defcell \$n__state__\d+ /,
       'state shadowing a my: state cell declared');
  # #401's eval half SHIPPED (s418c): a top-level-of-sub SCALAR state decl
  # now rides the eval-site capture alist (_eval_state_captures), so THAT
  # shape lowers — behavior guarded in Pl/t/state-eval-01.t.  What still
  # gates is the out-of-subset residue: a decl nested in an INNER block
  # (its region ends with the block, which the sub-scoped map cannot
  # express) with a string eval in the sub.
  my $g1b = eval { Pl::Parser2->parse_code(
    q{use feature 'state'; sub s3 { state $n = 2; return eval q($n); } print s3();}) };
  is($@, '', 'state + string eval in a named sub no longer gates (#401)');
  my $g1c = eval { Pl::Parser2->parse_code(
    q{use feature 'state'; sub s4 { { state $n = 2; } return eval q($n); } print s4();}) };
  like($@, qr/state \$n in named sub \(string eval\)/,
       'inner-block state decl + string eval in the sub still gates');
  # s296: state OUTSIDE a named sub no longer gates — it lowers natively to a
  # package-cell defvar + __init once-guard (the classic-pass container/scalar
  # route).
  my $g2 = eval { Pl::Parser2->parse_code(
    q{use feature 'state'; for (1..3) { state $x = 0; } print 1;}) };
  is($@, '', 'file-level state no longer gates to v1');
  like($g2, qr/\(p-defcell \$x__state__\d+ /, 'file-level state: cell declared');
  like($g2, qr/__state__\d+__init/, 'file-level state: once-init guard');
}

# ---- runtime ----

SKIP: {
  skip 'sbcl not available', 11 unless grep { -x "$_/sbcl" } split /:/, $ENV{PATH};
  my $root = "$FindBin::Bin/../..";
  my $run = sub {
    my ($src) = @_;
    my $tmp = "/tmp/parser2-02-$$.lisp";
    open my $fh, '>', $tmp or die $!;
    (my $out = $src) =~ s/\(in-package :pcl\)/(in-package :pcl)\n(p-defpackage :main)\n(in-package :main)/;
    print $fh $out;
    close $fh;
    my $got = `sbcl --control-stack-size 512 --noinform --non-interactive --load "$root/cl/pcl-runtime.lisp" --load "$tmp" 2>/dev/null`;
    unlink $tmp;
    return $got;
  };

  # while continue runs the continue block each iteration (incl. after next).
  my $wcp = Pl::Parser2->parse_code(<<'EOF');
my $i = 0; my $sum = 0;
while ($i < 5) { $sum = $sum + $i; } continue { $i = $i + 1; }
print "$sum\n";
EOF
  is($run->($wcp), "10\n", 'while continue: increments run between iterations');

  # foreach continue block runs after each element.
  my $fcp = Pl::Parser2->parse_code(<<'EOF');
my @a = (1,2,3); my $s = "";
foreach my $y (@a) { $s = $s . $y; } continue { $s = $s . "-"; }
print "$s\n";
EOF
  is($run->($fcp), "1-2-3-\n", 'foreach continue: separator runs after each element');

  # my $aa,$bb,$cc: $aa is an undef lexical, $bb a settable package var.
  my $mdp = Pl::Parser2->parse_code(<<'EOF');
my $aa, $bb, $cc;
$bb = 7;
print defined($aa) ? "def" : "undef", " $bb\n";
EOF
  is($run->($mdp), "undef 7\n", 'my $aa,$bb,$cc: only $aa lexical, $bb package var');

  # my $a . $foo: $a is declared empty; the concatenation is discarded.
  my $dcp = Pl::Parser2->parse_code(<<'EOF');
my $foo = "foo";
my $a . $foo;
print "[$a]\n";
EOF
  is($run->($dcp), "[]\n", 'my $a . $foo: $a declared empty');

  # my $i = $i shadow in a NATIVE block: the inner copy captures the OUTER
  # value; mutating it leaves the outer untouched (closure-01 t17 distilled).
  # (The same shadow inside a fallback-seam block is handled by the W8.5
  # rename — guarded above and by the runtime map test below.)
  my $shp = Pl::Parser2->parse_code(<<'EOF');
my $i = 7;
my $f;
{ my $i = $i; $f = sub { $i = $i + 1; $i }; }
print $f->(), "\n";
print $f->(), "\n";
print $i, "\n";
EOF
  is($run->($shp), "8\n9\n7\n", 'my $i = $i: inner copy independent of outer');

  # W8.5 seam-shadow rename, end-to-end: perl-correct (v1 gives "2 4 6 6" via
  # its defvar-shadow bug — v2-native is deliberately BETTER here; proof in
  # v2-completion-plan W8.5 / decisions D28).
  my $map = Pl::Parser2->parse_code(<<'EOF');
my $x = "outer";
my @r = map { my $x = $_ * 2; $x } (1,2,3);
print "@r $x\n";
EOF
  is($run->($map), "2 4 6 outer\n", 'map my-shadow: outer lexical untouched (matches perl)');

  # W8.5 poisoned condition-my, end-to-end: the loop lexical and the later
  # package global coexist (defins.t distilled — was an unbound-variable crash).
  my $pcm = Pl::Parser2->parse_code(<<'EOF');
my @l = ("a","b");
my $seen = 0;
while (my $name = shift @l) { $seen++ if $name eq "b"; }
$name = "G";
print "$seen $name\n";
EOF
  is($run->($pcm), "1 G\n", 'poisoned condition-my: lexical loop var + package global coexist');

  # W10 spanning lexical, end-to-end: declared in main, written in Foo,
  # captured by Foo's sub, read back in reopened main (v1 CRASHES here —
  # the s270 unbound-variable bug; v2's rename is deliberately better).
  my $spn = Pl::Parser2->parse_code(<<'EOF');
my $c = 1;
package Foo;
$c = $c + 2;
sub get { return $c; }
package main;
print $c, " ", Foo::get(), "\n";
EOF
  is($run->($spn), "3 3\n", 'W10: my spanning package boundaries reads/writes one cell');

  # W11 element access, end-to-end: var keys, ref-valued elements through a
  # bare (boxed) read, no autoviv on read, negative index — must match perl.
  my $elm = Pl::Parser2->parse_code(<<'EOF');
my %h; my @a; my $y = 9;
$h{x} = 5; $a[2] = 7; $a[0] = $a[2] * 2;
my $v = $h{x};
$h{$v} = $h{x} + 1;
$h{r} = \$y; my $g = $h{r};
my $z = $h{nope} + 1;
print $v, " ", $h{5}, " ", $a[0], " ", $a[-1], " ", $$g, " ", $z, " ",
      (exists $h{nope} ? "viv" : "noviv"), "\n";
EOF
  is($run->($elm), "5 6 14 7 9 1 noviv\n", 'W11: element read/write semantics match perl');

  # W14 end-to-end: coalesced multi-shift sub, missing arg = undef, extra
  # args ignored; a shift-then-@_ sub keeps mutation semantics (join must NOT
  # see the shifted "h").
  my $shf = Pl::Parser2->parse_code(<<'EOF');
sub f { my $x = shift; my $z = shift; return $x + $z; }
sub g { my $a = shift; return $a . "|" . join(",", @_); }
print f(1,2), " ", f(5) + 0, " ", g("h","r1","r2"), "\n";
EOF
  is($run->($shf), "3 5 h|r1,r2\n", 'W14: coalesced and non-coalesced shift subs match perl');

  # state end-to-end: counter persists across calls; no-init state starts
  # undef; a state inside a loop inits once (not per iteration or per call);
  # init RHS reads the outer scope.
  my $ste = Pl::Parser2->parse_code(<<'EOF');
use feature 'state';
sub c { state $n = 0; $n = $n + 1; return $n; }
sub tally { state $k; $k = ($k // 0) + 2; return $k; }
sub looped { my ($x) = @_; for my $i (1..3) { state $acc = 100; $acc = $acc + $x; } return 1; }
our $g = "outer";
sub echo { state $s = $g; $g = "changed"; return $s; }
print c(), c(), c(), " ", tally(), tally(), " ", looped(5), " ", echo(), echo(), "\n";
EOF
  is($run->($ste), "123 24 1 outerouter\n", 'state: persistence, once-init, outer-reading init match perl');
}

# ---- task #78: embedded map/grep/sort/eval{} blocks lower structurally ----
# The lambda is a CLForm (one flat form, no ";; echo" comment lines, no
# fixed multiline layout); declined shapes keep v1's text (body_cl).
{
  my $mp = Pl::Parser2->parse_code(
    q{my @a = (1,2,3); my @b = map { $_ + 1 } @a;});
  like($mp, qr/\(p-map \(lambda \(\$_\) \(p-\+ \$_ 1\)\) \@a\)/,
       '#78: map block emits one structured (lambda ($_) …) form');
  unlike($mp, qr/\(lambda \(\$_\)\n/,
         '#78: no v1 multiline lambda layout for a native block');

  my $st = Pl::Parser2->parse_code(
    q{my @a = (3,1); my @s = sort { $a <=> $b } @a;});
  # (\s+ between subforms: since the E2.final root flip the structural printer
  # may break long forms across lines — layout is free, the shape is pinned.)
  like($st,
       qr/\(p-sort\s+\(p-sort-cmp \(\$a \$b\)\s+\(p-scalar-ctx \(p-<=> \$a \$b\)\)\)\s+\@a\)/,
       '#78: sort block is ONE p-sort-cmp over the pair, scalar-context body');

  my $ev = Pl::Parser2->parse_code(q{my $e = eval { 42 };});
  like($ev, qr/\(p-eval-block 42\)/, '#78: eval block body is structural');

  # local inside the block → raw_wrap → decline → v1 text route (multiline).
  my $lc = Pl::Parser2->parse_code(
    q{my @a = (1); my @q = map { local $_ = 5; $_ } @a;});
  like($lc, qr/\(lambda \(\$_\)\n/,
       '#78: local-in-block declines to the v1 text body');

  # package inside the block → decline (v1 owns the revert wrapper).
  my $pk = Pl::Parser2->parse_code(
    q{my @r = map { package XM; $_ * 2 } (1, 2, 3);});
  like($pk, qr/\(\*pcl-current-package\* \*pcl-current-package\*\)/,
       '#78: package-in-block keeps v1 revert wrapper');

  # Bare `...` statement lowers natively (was a PARSE ERROR raw at file level).
  my $yy = Pl::Parser2->parse_code(q{...;});
  like($yy, qr/\(p-die "Unimplemented" :loc/,
       '#78: bare yada-yada statement lowers to p-die Unimplemented');

  # -- step 2: do{} / anonymous sub raw_lambda re-host --
  my $do = Pl::Parser2->parse_code(q{my $d = do { my $y = 5; $y + 1 };});
  like($do, qr/\(funcall \(lambda \(\) \(progn \(p-let \(\(\$y :scalar 5\)\) \(p-\+ \$y 1\)\)\)\)\)/,
       '#78: do{} body is structural — (funcall (lambda () (progn …)))');

  my $hc = Pl::Parser2->parse_code(q{my @h = map { { k => $_ } } (1);});
  like($hc, qr/\(p-map \(lambda \(\$_\) \(make-p-box \(p-hash "k" \$_\)\)\)/,
       '#78: hash-constructor map block body is structural');

  # The wrapper's THIRD binding is the anon sub's home package (task #515):
  # an anon lambda has no name to read one off, so the emitter supplies the
  # package in force at the `sub {` as a compile-time constant — the same
  # thing p-sub rebinds per call for a NAMED sub.  Asserted here as part of
  # the wrapper's shape, and differentially against perl in
  # Pl/t/decl-ordering-02.t.
  # The frame itself is `p-sub-frame`, not a bare `catch :p-return`: a SUB
  # frame applies perl's leave rule (pp_leavesub) to the value it exits with,
  # and an anon sub's wrapper is EMITTED, so the copy would be lost here if
  # this site drifted back (task #964; Pl/t/return-copy-01.t owns the
  # behaviour, this row owns the spelling).
  my $an = Pl::Parser2->parse_code(q{my $s = sub { 42 };});
  like($an,
       qr/\(lambda \(&rest %_args\)\s+\(let\s+\(\(\@_ \(p-flatten-args %_args\)\)\s+\(\*pcl-current-package\* "main"\)\s+\(\*pcl-caller-wantarray\* \*wantarray\*\)\)\s+\(p-sub-frame \(block nil 42\)\)\)\)/,
       '#78: anon sub emits v1 wrapper shape as one structured form');
  # Layout is no longer the v1-vs-native discriminator (the structural printer
  # breaks lines too); v1's text body is marked by its `;; <src>` echo lines.
  unlike($an, qr/;; 42/,
         '#78: no v1 source-echo comment in a native anon-sub body');
}

# ---- s316: shadow-aware interp rewrite in capture promotion (task #125) ----
# A file `my $x` captured by a named sub is promoted to a package cell; the
# promotion used to REFUSE (→ whole-file v1) whenever the name was also used
# in interpolated text and some other scope declared the same name, because
# the interp rewrite was scope-blind.  It now runs the same shadow predicate
# as the symbol rewrite: outer uses (symbol AND interpolated) take the cell,
# the shadow's own scope keeps the plain name.
{
  my $code = q{sub tempfile { "t1" }
               my $tmpfile = tempfile();
               sub fresh { print "outer=$tmpfile\n"; }
               sub other { my $tmpfile = tempfile(); print "inner=$tmpfile\n"; }
               fresh(); other();};
  my $cl = eval { Pl::Parser2->parse_code($code) };
  is($@, '', 'captured file lexical with an interpolated use + shadow no longer gates');
  like($cl, qr/\(p-defcell \$tmpfile__file__\d+ /,
       'promotion happened: the captured lexical became a package cell');
  # The OUTER sub's interpolated read takes the renamed cell …
  like($cl, qr/"outer="\s*\$tmpfile__file__\d+/,
       'interpolated use outside the shadow is rewritten to the cell');
  # … and the shadow's own interpolated read keeps the let-bound name.
  like($cl, qr/"inner="\s*\$tmpfile\b/,
       'interpolated use inside the shadow keeps the original name');
}

# ---- #184 (s334): that shadow predicate is asked only about REWRITE
# CANDIDATES, never about every token ----
# The predicate above walks the parent chain and each parent's preceding
# siblings, so its cost is proportional to the file, not to the token.  s316b
# called it once per PPI::Token, which made the cost quadratic: pack.t's
# transpile went 5.8 s → 74 s for BYTE-IDENTICAL output.  The symbol loop had
# always filtered (`eq $canon`) before asking; the interp loop now does the
# same by handing the predicate down to _fix_interp_token, which consults it
# only after the fixer has matched the name.
#
# Guard the invariant, not the seconds: count the calls with the same file
# padded by unrelated statements.  Two calls here (one interpolated use, one
# symbol use); the per-token version made 610 on this input, and that number
# grows with the padding.
{
  my $noise = join('', map { "my \$q$_ = $_ + 1; \$q$_ = \$q$_ * 2;\n" } 1..30);
  my $code = qq{sub tempfile { "t1" }
my \$tmpfile = tempfile();
$noise
sub fresh { print "outer=\$tmpfile\\n"; }
sub other { my \$tmpfile = tempfile(); print "inner=\$tmpfile\\n"; }
fresh(); other();};
  my $calls = 0;
  my $cl;
  {
    no warnings 'redefine';
    my $orig = \&Pl::Parser2::_ref_shadowed;
    local *Pl::Parser2::_ref_shadowed = sub { $calls++; $orig->(@_) };
    $cl = eval { Pl::Parser2->parse_code($code) };
  }
  is($@, '', '#184: padded capture-promotion file still parses');
  cmp_ok($calls, '<=', 20,
         "#184: shadow predicate asked $calls times, not once per token");
  # …and the padding did not change what the rewrite DID (same three verdicts
  # as the unpadded case above) — the speed-up must not cost the scoping.
  like($cl, qr/\(p-defcell \$tmpfile__file__\d+ /, '#184: promotion still happens');
  like($cl, qr/"outer="\s*\$tmpfile__file__\d+/,
       '#184: outer interpolated use still takes the cell');
  like($cl, qr/"inner="\s*\$tmpfile\b/,
       '#184: shadowed interpolated use still keeps the original name');
}

# ---- #184 residue (s335): the W10 spanning-lexical rename loops obey the
# same rule — the shadow predicate is asked about REWRITE CANDIDATES only ----
# _promote_captured was converted in s334, but the two W10 cross-package
# rename loops still asked _ref_shadowed about every PPI::Token of every
# later-segment statement.  Measured before this fix: a two-package file
# with ONE spanning lexical and 200 noise statements transpiled in 86 s
# (0.98 s after).  Same guard shape: count the calls on a padded file, and
# re-assert the scoping the predicate exists to protect.
{
  my $noise = join('', map { "my \$w$_ = $_ + 1; print \"w=\$w$_\\n\" if \$w$_ > $_;\n" } 1..30);
  my $code = qq{package A;
my \$tmpx = 5;
print "A=\$tmpx\\n";
package B;
$noise
{ my \$tmpx = 9; print "inner=\$tmpx\\n"; }
print "B=\$tmpx\\n";};
  my $calls = 0;
  my $cl;
  {
    no warnings 'redefine';
    my $orig = \&Pl::Parser2::_ref_shadowed;
    local *Pl::Parser2::_ref_shadowed = sub { $calls++; $orig->(@_) };
    $cl = eval { Pl::Parser2->parse_code($code) };
  }
  is($@, '', '#184/W10: padded spanning-lexical file still parses');
  cmp_ok($calls, '<=', 40,
         "#184/W10: shadow predicate asked $calls times, not once per token");
  like($cl, qr/"B="\s*A::\$tmpx__file__\d+/,
       '#184/W10: cross-package interpolated use still takes the qualified cell');
  like($cl, qr/"inner="\s*\$tmpx\b/,
       '#184/W10: shadowed interpolated use still keeps the original name');
}

# ---- E4.1 pre-work (s341): two v1 fallbacks that the cold-sweep cache found ----

# (1) `our $x OP= …` — perl's own Exporter.pm opens with `our $Verbose ||= 0;`.
# The initialiser may use ANY assignment operator; the gate used to accept `=`
# alone and route the whole file to v1.
my $ourc = eval { Pl::Parser2->parse_code(q{our $Verbose ||= 0; print $Verbose;}) };
is($@, '', 'our-compound: `our $x ||= 0` lowers natively (no Parser2 TODO)');
like($ourc, qr/\(p-defcell \$Verbose /, 'our-compound: the package cell is still declared');
like($ourc, qr/\(p-or-assign \$Verbose 0\)/, 'our-compound: the ||= write is emitted');
like(eval { Pl::Parser2->parse_code(q{our $n //= 3; our $m += 2; print $n;}) } // '',
     qr/\(p-defcell \$m /, 'our-compound: //= and += declare their cells too');
# A NON-assignment operator after the name is the SAME statement shape, not a
# different one — perl declares the cell and evaluates `NAMES <tail>` as an
# ordinary expression.  Probed against perl 5.40.3 (s395, #314 family F-B):
#   perl -e '$x=1; our $x, $y; print $x'            -> 1
#   perl -e 'sub f {print "called\n"} our $z, f();' -> called
# so the tail's side effects DO run, and only `use strict` rejects the
# spelling (invalid perl there, principle 9 — not our business).  This used to
# be pinned as a REFUSAL here; op/inccode.t's tied `sub FETCH { our $count++ }`
# and op/repeat.t's `our $Tiecount++` are the same shape and were both whole
# TRANSPILE-FAIL files because of it.
my $ourt = eval { Pl::Parser2->parse_code(q{our $x, $y; print $x;}) };
is($@, '', 'our-tail: `our $x, $y` lowers natively (no Parser2 TODO)');
like($ourt, qr/\(p-defcell \$x /, 'our-tail: the declared cell is still emitted');
my $ourinc = eval { Pl::Parser2->parse_code(q{our $count++; print $count;}) } // '';
like($ourinc, qr/\(p-defcell \$count /, 'our-tail: `our $count++` declares the cell');
like($ourinc, qr/\(p-post\+\+ \$count\)/, 'our-tail: …and the increment is emitted');

# (2) Condition-my poison test: a `foreach my $i (…)` BINDS the name, so its
# uses say nothing about a global.  Math::BigInt has five C-for `my $i` loops
# and four `for my $i` loops; counting the latter as outside-uses poisoned the
# name, and a poisoned construct holding a string eval gated the file to v1.
my $cm = Pl::Parser2->parse_code(<<'EOF');
my @r;
for my $i (1 .. 2) { push @r, $i }
for (my $i = 0 ; $i < 2 ; $i++) { my $v = eval "1+$i"; push @r, $v }
print "@r\n";
EOF
unlike($cm, qr/\$i__cond__/,
       'cond-my: foreach-my siblings do not poison the name (no rename)');
# The case that used to rename: a genuine use OUTSIDE every binding construct.
# Both roles keep the name now — the C-for head binds a `let`, and the global
# gets its cell for the trailing `print $i` (#291).
my $cm2 = Pl::Parser2->parse_code(<<'EOF');
$i = "GLOBAL";
for (my $i = 0 ; $i < 2 ; $i++) { print $i }
print $i;
EOF
unlike($cm2, qr/\$i__cond__/,
       'cond-my: a global use outside the construct no longer renames it');
like($cm2, qr/\(p-let \(\(\$i /, 'the C-for head my is let-bound — a lexical shadow');
like($cm2, qr/\(p-defcell \$i /, 'the package global $i still gets its declaration');

# ---- E4.1 pre-work (s342d, task #229): the two perl CORE modules the s342c
# ---- live-v1 audit found still gating (docs/v1-live-share-audit.md, F5).

# (3) A my-init that mentions an ELEMENT of a same-named container is NOT
# self-referential: $attrs{$_} is a slot of %attrs, $a[0] of @a — different
# variables.  ExtUtils::MM_Unix gated its whole file on
# `my $attrs = join " ", map { qq[$_="$attrs{$_}"] } sort keys %attrs;`.
my $mm = eval { Pl::Parser2->parse_code(
  q{my %attrs=(a=>1); my $attrs = join " ", map { qq[$_="$attrs{$_}"] } sort keys %attrs; print $attrs;}) };
is($@, '', 'element-init: $attrs{...} in the init of `my $attrs` is not self-reference');
like($mm // '', qr/p-join/, 'element-init: the join is actually emitted');
is(eval { Pl::Parser2->parse_code(
     q{my @a=(1,2); my $a = join ",", map { $a[$_] } 0..1; print $a;}) } && $@, '',
   'element-init: the array form ($a[$_] inside `my $a`) lowers too');
# INVERSE: a REAL self-reference with a below-assignment tail is still refused.
eval { Pl::Parser2->parse_code(q{our $x = "O"; { my $x = $x, 1; print $x; }}) };
like($@, qr/Parser2 TODO: self-referential my-init/,
     'element-init INVERSE: `my $x = $x, 1` is still refused');
eval { Pl::Parser2->parse_code(q{our $x = "O"; { my $x = "<$x>", 1; print $x; }}) };
like($@, qr/Parser2 TODO: self-referential my-init/,
     'element-init INVERSE: the INTERPOLATED self-read is still refused');

# (4) A plain `my`/`state` declaration BINDS the name, so it is not evidence
# that a package global of that name is live.  CPAN::Meta::Requirements::Range
# gated on one `my ($vobj, $err);` while every other $err sat inside an
# `if (my $err = $@)`.
my $cm3 = Pl::Parser2->parse_code(<<'EOF');
sub f {
  my ($vobj, $err);
  eval { die "boom\n" };
  if (my $err = $@) { my $v = eval "1"; return "caught" }
  return defined $vobj ? "V" : "-";
}
print f(), "\n";
EOF
unlike($cm3, qr/\$err__cond__/,
       'cond-my: a plain `my ($vobj, $err)` decl does not poison the name');
# INVERSE 1: a REAL use of the name outside every construct.  This used to
# rename (and, before #254 B-i, gate the whole file when the construct held a
# STRING EVAL).  Since #291 there is no rename at all: the condition-my is a
# `let`, which lexically shadows the global's cell, and the eval inside still
# sees it through _let_bound_vars under its own name.
my $cm4 = eval { Pl::Parser2->parse_code(<<'EOF') };
sub f {
  my ($vobj, $err);
  if (my $err = $@) { my $v = eval "1"; return "caught" }
  return defined $err ? "def" : "undef";
}
EOF
is($@, '', 'cond-my: a string eval in the construct no longer gates the file');
unlike($cm4 // 'x', qr/__cond__/,
       'cond-my: a real use outside the constructs no longer renames');
like($cm4 // '', qr/\(p-let \(\(\$err /,
     'cond-my: the construct binds a lexical $err over the global');
# INVERSE 2: an `our` declaration DOES create the global — both roles, one name.
my $cm5 = Pl::Parser2->parse_code(<<'EOF');
our $err;
if (my $err = "INNER") { print $err }
print $err;
EOF
unlike($cm5, qr/__cond__/, 'cond-my: `our $err` beside a condition-my needs no rename');
like($cm5, qr/\(p-defcell \$err /, 'cond-my: the `our` global still gets its cell');
like($cm5, qr/\(p-let \(\(\$err /,   'cond-my: the condition-my still gets its lexical');

# ---- E4.1 pre-work (s342f, task #227): eval-mode trailing declarations.
# The audit's CPAN half of family F2 was not syntax probes at all — it was
# `our $VERSION = '1.01'`, a routine module idiom, gating every time.

# (5) `our NAMES [OP= RHS]` as an eval's last statement.  _lower_our_decl
# already returns the assignment expression as its only form, so the value is
# the tail — the gate simply never let `our` through.
for my $src ('our $V = "1.01"', 'our $W ||= 3', 'our ($P,$Q) = (7,8)',
             'our @R = (4,5,6)', 'our $N') {
  my $out = eval { Pl::Parser2->parse_code($src, eval_mode => 1) };
  is($@, '', "eval-tail our: `$src` lowers natively");
  ok(defined $out && length $out, "eval-tail our: `$src` produced output");
}
# The no-init form must emit the READ as its tail value (v1 answered with the
# emitted variable NAME — a silent-wrong).
like(eval { Pl::Parser2->parse_code('our $N', eval_mode => 1) } // '',
     qr/\(p-defcell \$N /, 'eval-tail our: the no-init decl still declares its cell');

# (6) `my ()` — legal, declares nothing (perl #113554); my.t asserts
# `eval "my ()"` leaves $@ EMPTY, so it must not gate in either position.
is(eval { Pl::Parser2->parse_code('my ();') } && $@, '',
   'empty-my: `my ();` lowers natively in statement position');
my $em = eval { Pl::Parser2->parse_code('my ()', eval_mode => 1) };
is($@, '', 'empty-my: `my ()` lowers natively as an eval tail');
like($em // '', qr/\(progn\)/,
     'empty-my: the eval tail is the empty-list form, not (p-undef)');

# (7) A bare MULTI decl as the tail is the LIST of its names.
is(eval { Pl::Parser2->parse_code('my ($c,$d)', eval_mode => 1) } && $@, '',
   'multi-tail: `my ($c,$d)` lowers natively');
# INVERSE: a non-parenthesised multi (`my @a` is single) and the shapes the
# gate still declines must keep declining — a declaration buried in a comma
# expression is the #138 family, not this one.
eval { Pl::Parser2->parse_code('my($a,$b),$x,my($c,$d)', eval_mode => 1) };
like($@, qr/^PCL: unsupported in string eval: trailing declaration has no value/,
     'multi-tail INVERSE: a decl inside a comma expression still refuses');

# s412 (Phase C): v1's `local` machinery caps inlining — `(locally (declare
# (notinline …)))` / a `(declare (notinline …))` at the head of its let — only
# for a `local` that wraps the REST OF THE FILE (a huge cold form: local.t OOM,
# s268).  Under Parser2's seam every v1-routed statement lowers at indent 0,
# so the indent-keyed discriminator called a `local` in a file-level LOOP body
# top-level too and suppressed the fast-path inlining in that hot body; the
# seam's block_depth is the real-nesting fact and now takes part.
{
  my $top = Pl::Parser2->parse_code(q{our $x = 1; local $x = 2; sub f { $x } print f();});
  like($top, qr/notinline/, 'a true top-level local (wraps the rest of the file) keeps the inlining cap');
  my $loop = Pl::Parser2->parse_code(
    q{our $x = 5; sub f { $x } for my $i (1..2) { local $x = $i; print f(); }});
  unlike($loop, qr/notinline/, 'a local inside a file-level loop body gets NO inlining cap (hot code)');
  like($loop, qr/\(p-local-cell \$x \(p-box-for-local \$i\)/, '…and still lowers as p-local-cell');
  my $ev = Pl::Parser2->parse_code(
    q{our $x = 5; sub f { $x } my $r = eval { local $x = 1; my @l = map { $_ } 1..2; f() }; print $r;});
  unlike($ev, qr/notinline/, 'a local inside an eval body: no inlining cap');
  like($ev, qr/p-eval-block/, 'the eval body lowers');
  like($ev, qr/\(p-foreach-range-raw|\(p-map \(lambda/, 'the eval body with a local is lowered STRUCTURALLY (map goes through the structural route)');
}

done_testing();

#!/usr/bin/env perl
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
like($fc, qr/\(p-foreach .*:continue \(progn/s, 'foreach continue: :continue key');

# `my $aa, $bb, $cc;` — Perl declares only $aa (a lexical); $bb/$cc are package
# vars.  Lower as a boxed `my $aa` let + the comma expression discarded.
my $md = Pl::Parser2->parse_code(q{my $aa, $bb, $cc; $bb = 1; print $aa;});
like($md, qr/\(let \(\(\$aa \(make-p-box nil\)\)\)/, 'my $aa,… : only $aa let-bound');
unlike($md, qr/\(let \(\(\$bb/, 'my $aa,… : $bb is NOT let-bound (package var)');
like($md, qr/\(defvar \$bb /, 'my $aa,… : $bb forward-declared as a package global');

# `my $a . $foo;` — declares $a, concatenation discarded.
my $dc = Pl::Parser2->parse_code(q{my $foo = "f"; my $a . $foo; print $a;});
like($dc, qr/\(let \(\(\$a \(make-p-box nil\)\)\)/, 'my $a . $foo : $a let-bound');

# ---- W8 tail (s273): VarAnnotator write shapes + self-ref my init + our-init ----

# Bitwise compound assigns are writes; since task #62 a coercing compound op
# at native statement root stores to a RAW slot via its -raw macro twin (same
# p-bit-and computation as the boxed macro, so bop-01 t17/18 string-bitwise
# semantics are preserved) — the write must lower through the twin, never a
# box-set on the raw slot.
my $bw = Pl::Parser2->parse_code(q{my $s = "zzzzz"; $s &= "AAAAA"; print $s;});
like($bw, qr/\(let \(\(\$s "zzzzz"\)\)/, 'bitwise &= target goes raw (task #62)');
like($bw, qr/\(p-bit-and=-raw \$s "AAAAA"\)/, '&= lowers via the raw macro twin');

# Paren-less \substr $t: magic write-through ref needs the box (misc-fixes-02 t27).
my $sb = Pl::Parser2->parse_code(q{my $t = ""; ${\substr $t, 0} = "X"; print $t;});
like($sb, qr/\(\$t \(make-p-box nil\)\)/, 'paren-less \substr target stays boxed');

# Handle-vivifying builtin writes its FH arg: open($h,…) keeps $h boxed
# (fileio-02 t25 — a raw slot loses the handle association).
my $oh = Pl::Parser2->parse_code(q{my $h = "Log123"; open($h, '<', "/dev/null") or die; print $h;});
like($oh, qr/\(\$h \(make-p-box nil\)\)/, 'open($h,…) FH arg stays boxed');

# Self-referencing init `my $i = $i` reads the OUTER $i: the init moves into
# the let BINDING via p-box-init (evaluated in the outer env), never a
# body-position p-my-= that would read the fresh nil box (closure-01 t17).
my $sh = Pl::Parser2->parse_code(q{my $i = 7; { my $i = $i; print $i; } print $i;});
like($sh, qr/\(let \(\(\$i \(p-box-init \$i\)\)\)/, 'my $i = $i: init in the let binding (outer scope)');

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

# Poisoned condition-my (same name later used as a package global) is renamed
# to $x__cond__N so the global gets its forward defvar (defins.t crash).
my $pc = Pl::Parser2->parse_code(<<'EOF');
my @l = (1,2);
while (my $name = shift @l) { print $name; }
$name = 9;
print $name;
EOF
like($pc, qr/\$name__cond__\d+/, 'poisoned condition-my renamed');
like($pc, qr/\(defvar \$name /, 'the package global $name still gets its defvar');

# A self-contained condition-my (no outside use) keeps its name — zero churn.
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
like($eb, qr/\(\$v \(%pcl-to-string-strict \(p-gethash %h "k"\) "\$v"\)\)/,
     'W11/B-str: element-read init freezes raw when every use stringifies');
my $eb2 = Pl::Parser2->parse_code(
  q{my %h; $h{k} = 5; my $v = $h{k}; my $w = $v; print $v;});
like($eb2, qr/\(\$v \(make-p-box nil\)\)/,
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

# ---- block-form-arg capture gate (Try-Tiny `catch { $caught = $_ }` class) ----

# A block-form-prototype arg's body hoists as a top-level --anon-block-N--
# defun, OUTSIDE the lexical lets; if it references a live lexical, the file
# must gate to v1 (v2 would read an unbound global from the hoisted defun).
{
  my $src_cap = q{
    sub try2 (&;@) { my ($t, @h) = @_; my @r = eval { $t->() }; if ($@) { for my $h (@h) { return $h->($@) } } return @r; }
    sub catch2 (&;@) { my ($b, @rest) = @_; return ($b, @rest); }
    my $caught = "none";
    try2 { die "boom\n" } catch2 { $caught = $_[0] };
    print $caught;
  };
  my $cl = eval { Pl::Parser2->parse_code($src_cap) };
  like($@, qr/block-form arg body captures live lexical 'caught'/,
       'block-form arg body capturing a live lexical gates to v1');

  # Same shape with NO lexical capture in the block → still lowers natively.
  my $src_ok = q{
    sub try2 (&;@) { my ($t, @h) = @_; my @r = eval { $t->() }; if ($@) { for my $h (@h) { return $h->($@) } } return @r; }
    sub catch2 (&;@) { my ($b, @rest) = @_; return ($b, @rest); }
    try2 { die "boom\n" } catch2 { print "caught" };
  };
  my $ok = eval { Pl::Parser2->parse_code($src_ok) };
  ok(defined $ok && $ok =~ /--anon-block-/, 'non-capturing block-form arg still lowers natively')
    or diag($@);
}

# ---- state in named subs: native per-sub cell (rename family __state__N) ----

{
  my $st = Pl::Parser2->parse_code(
    q{use feature 'state'; sub c { state $n = 0; $n = $n + 1; return $n; } print c();});
  like($st, qr/\(defvar \$n__state__0 \(make-p-box nil\)\)/,
       'state: per-sub cell hoisted as a defvar box');
  like($st, qr/\(unless \$n__state__0__init \(box-set \$n__state__0 0\) \(setf \$n__state__0__init t\)\)/,
       'state: guarded once-init in v1 shape');
  unlike($st, qr/\(let \(\(\$n__state__0/, 'state: cell is never let-bound');

  # No init → cell only, no flag.
  my $sp = Pl::Parser2->parse_code(
    q{use feature 'state'; sub t2 { state $k; return $k; } print t2();});
  like($sp, qr/\(defvar \$k__state__0 /, 'state without init: cell defvar');
  unlike($sp, qr/__init/, 'state without init: no once-flag');

  # Out-of-subset shapes still gate.
  my $g1 = eval { Pl::Parser2->parse_code(
    q{use feature 'state'; sub s2 { my $n = 1; state $n = 2; return $n; } print s2();}) };
  like($@, qr/state \$n in named sub \(multiple declarations\)/,
       'state shadowing a my in the same sub gates to v1');
  # s296: state OUTSIDE a named sub no longer gates — it lowers natively to a
  # package-cell defvar + __init once-guard (the classic-pass container/scalar
  # route).
  my $g2 = eval { Pl::Parser2->parse_code(
    q{use feature 'state'; for (1..3) { state $x = 0; } print 1;}) };
  is($@, '', 'file-level state no longer gates to v1');
  like($g2, qr/\(defvar \$x__state__\d+ /, 'file-level state: cell defvar');
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
       qr/\(p-sort\s+\(lambda \(\$a \$b\)\s+\(catch :p-return \(block nil \(let \(\(\*wantarray\* nil\)\) \(p-<=> \$a \$b\)\)\)\)\)\s+\@a\)/,
       '#78: sort block keeps catch/block/wantarray wrappers, structurally');

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
  like($do, qr/\(funcall \(lambda \(\) \(progn \(let \(\(\$y 5\)\) \(p-\+ \$y 1\)\)\)\)\)/,
       '#78: do{} body is structural — (funcall (lambda () (progn …)))');

  my $hc = Pl::Parser2->parse_code(q{my @h = map { { k => $_ } } (1);});
  like($hc, qr/\(p-map \(lambda \(\$_\) \(make-p-box \(p-hash "k" \$_\)\)\)/,
       '#78: hash-constructor map block body is structural');

  my $an = Pl::Parser2->parse_code(q{my $s = sub { 42 };});
  like($an,
       qr/\(lambda \(&rest %_args\)\s+\(let \(\(\@_ \(p-flatten-args %_args\)\) \(\*pcl-caller-wantarray\* \*wantarray\*\)\)\s+\(catch :p-return \(block nil 42\)\)\)\)/,
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
  like($cl, qr/\(defvar \$tmpfile__file__\d+ /,
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
  like($cl, qr/\(defvar \$tmpfile__file__\d+ /, '#184: promotion still happens');
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
like($ourc, qr/\(defvar \$Verbose /, 'our-compound: the package cell is still defvar\'d');
like($ourc, qr/\(p-or-assign \$Verbose 0\)/, 'our-compound: the ||= write is emitted');
like(eval { Pl::Parser2->parse_code(q{our $n //= 3; our $m += 2; print $n;}) } // '',
     qr/\(defvar \$m /, 'our-compound: //= and += declare their cells too');
# INVERSE: a non-assignment operator after the name is still not an `our` decl.
eval { Pl::Parser2->parse_code(q{our $x, $y; print $x;}) };
like($@, qr/Parser2 TODO: unsupported our declaration/,
     'our-compound INVERSE: `our $x, $y` is still refused (not an assignment)');

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
# INVERSE: a genuine use OUTSIDE every binding construct still poisons → rename.
my $cm2 = Pl::Parser2->parse_code(<<'EOF');
$i = "GLOBAL";
for (my $i = 0 ; $i < 2 ; $i++) { print $i }
print $i;
EOF
like($cm2, qr/\$i__cond__/,
     'cond-my INVERSE: a global use outside the construct still renames it');

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
# INVERSE 1: a REAL use of the name outside every construct still poisons — and
# because one construct holds a string eval, that is still a whole-file gate.
eval { Pl::Parser2->parse_code(<<'EOF') };
sub f {
  my ($vobj, $err);
  if (my $err = $@) { my $v = eval "1"; return "caught" }
  return defined $err ? "def" : "undef";
}
EOF
like($@, qr/Parser2 TODO: poisoned condition-my \$err/,
     'cond-my INVERSE: a real use outside the constructs still poisons');
# INVERSE 2: an `our` declaration DOES create the global, so it still poisons.
like(Pl::Parser2->parse_code(<<'EOF'), qr/\$err__cond__/,
our $err;
if (my $err = "INNER") { print $err }
print $err;
EOF
     'cond-my INVERSE: `our $err` still counts as a live global (renames)');

done_testing();

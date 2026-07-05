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

# Bitwise compound assigns are writes: $s must stay boxed (bop-01 t17/18).
my $bw = Pl::Parser2->parse_code(q{my $s = "zzzzz"; $s &= "AAAAA"; print $s;});
like($bw, qr/\(\$s \(make-p-box nil\)\)/, 'bitwise &= target stays boxed');

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

# An INTERPOLATED use of the shadow inside the block still gates → v1.
my $si = eval { Pl::Parser2->parse_code(
  q{my $e = "o"; my @r = map { my $e = $_; "($e)" } (1,2); print "@r $e";}) };
like($@, qr/my-shadow of live lexical \$e.*interpolated/,
     'interpolated seam shadow still dies to v1');

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

# ---- runtime ----

SKIP: {
  skip 'sbcl not available', 8 unless grep { -x "$_/sbcl" } split /:/, $ENV{PATH};
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
}

done_testing();

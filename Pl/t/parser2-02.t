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

# ---- runtime ----

SKIP: {
  skip 'sbcl not available', 4 unless grep { -x "$_/sbcl" } split /:/, $ENV{PATH};
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
}

done_testing();

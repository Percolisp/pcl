#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# raw-verdict-01.t - B-regime scan-licensed freeze verdicts (task #62,
# docs/raw-numeric-verdict.md): raw-numeric / raw-string slots for variables
# whose writes are unproven but whose USES all license one family, with every
# native write routed through the strict freeze coercers.
#
# Transpile checks pin the verdict (wrapper present/absent); runtime checks
# pin value fidelity vs perl for the trap cases ("0.0" truthiness, ref
# stable-ID identity, aggregate scalar-context collapse).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use lib "$RealBin/../..";
use PCLCore;
use Pl::Parser2;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 48;

sub run_cl {
    my ($code, $env) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `${\ ($env // '')} $pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected, $env) = @_;
    is(run_cl($code, $env), $expected, $name);
}

# ---- verdict shape checks (transpile only) --------------------------------

# The bench shape: element-seeded loop bound, all uses numeric → raw-numeric.
my $cl = Pl::Parser2->parse_code(
  q{my %h=(k=>5); my $n = $h{k}; my $s=0; for (my $i=0; $i<$n; $i++) { $s+=$i } print "$s\n";});
like($cl, qr/\(\$n \(%pcl-to-number-strict /, 'element-seeded numeric bound: B-num freeze');

# All-string uses (interpolation, length, bool) → raw-string; bool licenses str.
$cl = Pl::Parser2->parse_code(
  q{my %h=(k=>"x"); my $m = $h{k}; if ($m) { print "got $m\n"; } print length($m),"\n";});
like($cl, qr/\(\$m \(%pcl-to-string-strict /, 'string/bool uses: B-str freeze');

# Boolean context DISQUALIFIES raw-numeric ("0.0"/"00"/" " are true strings
# that numify false) — a bool + num mix stays boxed.
$cl = Pl::Parser2->parse_code(
  q{my %h=(k=>"0.0"); my $n = $h{k}; print "T\n" if $n; print $n+1,"\n";});
like($cl, qr/\(\$n \(make-p-box nil\)\)/, 'bool+num mixed uses stay boxed (the "0.0" trap)');

# defined() is a call arg → opaque → boxed (freeze would make undef defined).
$cl = Pl::Parser2->parse_code(
  q{my %h; my $n = $h{k}; print defined($n)?"d":"u"; print $n+1,"\n";});
like($cl, qr/\(\$n \(make-p-box nil\)\)/, 'defined() use disqualifies');

# Dereference → opaque → boxed (freeze would break the ref).
$cl = Pl::Parser2->parse_code(q{my %h=(k=>[1]); my $r = $h{k}; print $r->[0],"\n";});
like($cl, qr/\(\$r \(make-p-box nil\)\)/, 'deref use disqualifies');

# Range endpoint is TYPE-SENSITIVE (magical string range) → opaque → boxed.
$cl = Pl::Parser2->parse_code(
  q{my %h=(k=>3); my $n = $h{k}; for my $i (1..$n) { print $i } print "\n";});
like($cl, qr/\(\$n \(make-p-box nil\)\)/, 'range endpoint use disqualifies');

# & | ^ are TYPE-SENSITIVE (string bitwise on two strings) → opaque → boxed.
$cl = Pl::Parser2->parse_code(
  q{my %h=(k=>6); my $n = $h{k}; print $n & 3, "\n";});
like($cl, qr/\(\$n \(make-p-box nil\)\)/, 'bitwise & use disqualifies');

# Unary minus is TYPE-SENSITIVE (-"abc" eq "-abc") → opaque → boxed.
$cl = Pl::Parser2->parse_code(
  q{my %h=(k=>3); my $n = $h{k}; print -$n, "\n"; print $n+1,"\n";});
like($cl, qr/\(\$n \(make-p-box nil\)\)/, 'unary minus use disqualifies');

# A read hidden in a regex PATTERN interpolates (stringify) — licenses B-str,
# disqualifies B-num even when every visible use is numeric.
$cl = Pl::Parser2->parse_code(
  q{my %h=(k=>3); my $n = $h{k}; print $n+1,"\n"; print "y\n" if "x3" =~ /x$n/;});
like($cl, qr/\(\$n \(make-p-box nil\)\)/, 'regex-pattern interpolation blocks B-num');

# `use overload` anywhere in the file disables both B-verdicts.
$cl = Pl::Parser2->parse_code(
  q{package O; use overload '""' => sub {"o"}; package main; my %h=(k=>5); my $n=$h{k}; print $n+1,"\n";});
unlike($cl, qr/%pcl-to-number-strict/, 'use overload in file disables freezes');

# Sub params never freeze (caller-bound initial value).
$cl = Pl::Parser2->parse_code(
  q{sub f { my ($n) = @_; return $n + 1; } print f(2), "\n";});
unlike($cl, qr/%pcl-to-\w+-strict \$n/, 'sub params never freeze');

# ---- runtime fidelity ------------------------------------------------------

test_cl('B-num loop bound computes identically',
    'my %h=(k=>4); my $n=$h{k}; my $s=0; for (my $i=0; $i<$n; $i++){ $s+=$i } print "$s\n";',
    "6\n");

test_cl('B-str prints the exact element string',
    'my %h=(k=>"0.0"); my $m=$h{k}; print "T\n" if $m; print "[$m]\n";',
    "T\n[0.0]\n");

test_cl('ref stable-ID: frozen key matches a later live read',
    'my %h=(k=>{x=>1}); my $r=$h{k}; my %seen; $seen{$r}=1;
     my $r2=$h{k}; print exists $seen{$r2} ? "same" : "diff", "\n";',
    "same\n");

test_cl('aggregate scalar-context collapse inside the freeze',
    'our @a; my $n = @a = split(/,/, "a,b,c"); print "$n\n";',
    "3\n");

test_cl('undef freeze: numeric slot sees 0, like perl at first numeric use',
    'my %h; my $n=$h{nope}; print $n+1,"\n";',
    "1\n");

# ---- S1 str-buffer (fill-pointer append) ----------------------------------

# Accumulator with only `.=` writes and transient uses → buffer.
$cl = Pl::Parser2->parse_code(
  q{my $s = ""; for (my $i=0; $i<10; $i++) { $s .= "ab"; } print "$s\n";});
like($cl, qr/\(\$s \(%pcl-str-buffer ""\)\)/, 'S1: accumulator init becomes a buffer');
like($cl, qr/\(%pcl-str-append \$s "ab"\)/,   'S1: .= appends in place');

# A bare-copy alias escape (opaque use) blocks the buffer (the alias must
# not observe later in-place appends).
$cl = Pl::Parser2->parse_code(
  q{my $s=""; $s .= "a"; my $t = $s; print "$t$s\n";});
unlike($cl, qr/%pcl-str-buffer/, 'S1: alias escape blocks buffer');

# A hash-key use is RETAINED by the table → blocks buffer (still B-str ok).
$cl = Pl::Parser2->parse_code(
  q{my $s=""; $s .= "ab"; my %h; $h{$s}=1; print "$s\n";});
unlike($cl, qr/%pcl-str-buffer/, 'S1: hash-key use blocks buffer');

# Any non-.= compound write (x=) blocks buffer.
$cl = Pl::Parser2->parse_code(q{my $s="x"; $s .= "y"; $s x= 2; print "$s\n";});
unlike($cl, qr/%pcl-str-buffer/, 'S1: x= write blocks buffer');

# foreach range var never buffers (bound by the loop macro, not an init).
$cl = Pl::Parser2->parse_code(q{for my $i (1..3) { $i .= "x"; print "$i\n"; }});
unlike($cl, qr/%pcl-str-buffer/, 'S1: foreach range var never buffers');

test_cl('S1 runtime: append loop matches perl',
    'my $s = ""; for (my $i=0; $i<5; $i++) { $s .= "ab"; }
     print "$s\n"; print length($s), "\n"; print "T\n" if $s;',
    "ababababab\n10\nT\n");

test_cl('S1 runtime: self-append is safe',
    'my $s = "ab"; $s .= $s; $s .= $s; print "$s\n"; print "eq\n" if $s eq "abababab";',
    "abababab\neq\n");

# ---- the runtime DECLINE (task #890) --------------------------------------
#
# The B-regime licence is a compile-time SCAN of this file's text for
# `use overload`, so an overloaded object arriving from a MODULE is invisible
# to it.  The strict coercers therefore decline at the write and store what
# the general form would have stored (p-box-init), instead of dying.  The
# classes below live in a temp module dir precisely so the file under test
# contains no `use overload` text and the verdict really does fire.

my $modlib = tempdir(CLEANUP => 1);
sub write_mod {
    my ($name, $body) = @_;
    open my $mfh, '>', "$modlib/$name.pm" or die $!;
    print $mfh $body;
    close $mfh;
}
write_mod('AoCmp', <<'MOD');
package AoCmp;
use overload '<=>' => sub { my ($a,$b,$sw)=@_; my $bv = ref($b) ? $b->{v} : $b;
                            $sw ? ($bv <=> $a->{v}) : ($a->{v} <=> $bv) };
sub new { my ($c,$v)=@_; return bless { v => $v }, $c }
1;
MOD
write_mod('AoKid', "package AoKid;\nuse AoCmp ();\nour \@ISA=('AoCmp');\n1;\n");
write_mod('AoNum', <<'MOD');
package AoNum;
use overload '0+' => sub { $_[0]{v} }, '""' => sub { 'N(' . $_[0]{v} . ')' },
             'fallback' => 1;
sub new { my ($c,$v)=@_; return bless { v => $v }, $c }
1;
MOD
write_mod('AoPlain', "package AoPlain;\nsub new { return bless { v => \$_[1] }, \$_[0] }\n1;\n");

my $USE = qq{use lib '$modlib';\n};

# The VERDICT still fires — the decline is a RUNTIME de-optimization, so the
# compile-time freeze must still be emitted (this is what keeps the fix from
# quietly giving back the raw-slot win on every method-call initializer).
$cl = Pl::Parser2->parse_code(
  qq{${USE}use AoNum (); my \$q = AoNum->new(7); print \$q + 1, "\\n";});
like($cl, qr/%pcl-to-number-strict/, '#890: the freeze wrapper is still emitted');

# A class that overloads ONLY `<=>` (no "" and no 0+) — every numeric
# comparison is autogenerated from it, so freezing compared the ref's
# stable ID and answered the opposite of perl.  Silent, not fatal.
test_cl('#890: <=>-only overload declines the freeze',
    qq{${USE}use AoCmp (); my \$c = AoCmp->new(42);
       print((\$c > 10 ? 1 : 0), (\$c < 10 ? 1 : 0), (\$c == 42 ? 1 : 0), "\\n");},
    "101\n");

test_cl('#890: <=>-only, same answer with the verdict off',
    qq{${USE}use AoCmp (); my \$c = AoCmp->new(42);
       print((\$c > 10 ? 1 : 0), (\$c < 10 ? 1 : 0), (\$c == 42 ? 1 : 0), "\\n");},
    "101\n", 'PCL_OPT=none');

# The reported shape: a 0+/"" object from a module used only numerically.
# This DIED before ("re-box the variable"), losing the whole program.
test_cl('#890: a module-built 0+ object in a numeric-only slot runs',
    qq{${USE}use AoNum (); my \$w = AoNum->new(30);
       print((5 < \$w ? 1 : 0), (\$w == 30 ? 1 : 0), "\\n");},
    "11\n");

# Inheritance is part of the question: perl's own overload::Overloaded
# answers true for a subclass of an overloaded parent (probed 5.40.3).
test_cl('#890: an INHERITED overload declines the freeze too',
    qq{${USE}use AoKid (); my \$k = AoKid->new(42);
       print((\$k > 10 ? 1 : 0), (overload::Overloaded(\$k) ? 1 : 0), "\\n");},
    "11\n");

# A plain blessed ref has no per-use code, so it still FREEZES to its stable
# ID — the accepted footnote in raw-numeric-verdict.md, and the negative that
# keeps the decline from widening into "never freeze an object".
test_cl('#890: a plain blessed ref still freezes (identity preserved)',
    qq{${USE}use AoPlain (); my \$p = AoPlain->new(3); my \$r = \$p;
       print((\$r == \$p ? 1 : 0), (\$r > 0 ? 1 : 0), "\\n");},
    "11\n");

# A genuine dualvar declines for the same reason (freezing drops one side).
test_cl('#890: a dualvar in a numeric-only slot keeps both sides',
    'open(my $fh, "<", "/no/such/file/pcl-890") or 1;
     my $e = $!; my $n = $e + 0; print(($n > 0 ? 1 : 0), "\n");',
    "1\n");

# ---- #77: return-family transfer through sub_info -------------------------
# A funcall ROOT is normally unproven (a sub may hand back a BOX), but when
# every value a named sub can return is operator-coerced or literal, its
# result is a fresh raw CL value by the SAME proof that licenses
# `$x = $a + $b`.  Parser2::_sub_return_facts records `returns => num/str`;
# the call-site write is then PROVEN — a plain raw slot, no freeze wrapper.

$cl = Pl::Parser2->parse_code(
  q{sub f { my $a = shift; return $a + 1 } my $x = f(1); g($x); print "$x\n";});
like($cl, qr/\(\$x \(pl-f 1\)\)/,
     '#77 proven-num return: plain raw slot, no freeze wrapper');

$cl = Pl::Parser2->parse_code(
  q{sub f { my $a = shift; return $a . "x" } my $x = f(1); g($x); print "$x\n";});
like($cl, qr/\(\$x \(pl-f 1\)\)/, '#77 proven-str return: plain raw slot');

# The gate switches it off — the general form comes back.
{
  local $ENV{PCL_OPT} = '-raw-return-family';
  Pl::Passes::_parse_env();
  my $off = Pl::Parser2->parse_code(
    q{sub f { my $a = shift; return $a + 1 } my $x = f(1); g($x); print "$x\n";});
  like($off, qr/\(\$x \(make-p-box nil\)\)/,
       '#77 PCL_OPT=-raw-return-family: the general (boxed) form');
}
Pl::Passes::_parse_env();

# The NEGATIVES — each is a way a sub can hand back a box or an unknown, and
# each must leave the call site unproven.
for my $neg (
  ['bare variable return' => q{our $g; sub f { $g } my $x = f(); g($x); print "$x\n";}],
  ['mixed families'       => q{sub f { my $n=shift; return $n+1 if $n; return "s"."t" }
                               my $x = f(1); g($x); print "$x\n";}],
  ['bare return;'         => q{sub f { my $n=shift; return if $n; return $n+1 }
                               my $x = f(1); g($x); print "$x\n";}],
  ['conditional tail return (falls off the end)'
                          => q{sub f { my $n=shift; return $n+1 if $n }
                               my $x = f(1); g($x); print "$x\n";}],
  ['compound tail'        => q{sub f { my $n=shift; if ($n) { $n+1 } else { 0 } }
                               my $x = f(1); g($x); print "$x\n";}],
  ['unknown sub'          => q{my $x = nosuchsub(1); g($x); print "$x\n";}],
  ['method call'          => q{sub f { my $a=shift; $a+1 } my $o = bless {}, "C";
                               my $x = $o->f(1); g($x); print "$x\n";}],
) {
  my ($what, $src) = @$neg;
  like(Pl::Parser2->parse_code($src), qr/\(\$x \(make-p-box nil\)\)/,
       "#77 negative: $what stays boxed");
}

# The shape where the two native-root models disagreed: PExpr folds the comma
# into the parenless list-op CALL (so the parse root is the `=`), while
# Parser2's token split sees a depth-0 comma and reroutes the whole statement
# through the seam, where the write is a box-set that cannot reach a raw slot.
# Proving the call's family made that reachable — `$c` printed nothing.
$cl = Pl::Parser2->parse_code(
  q{sub two { $_[0] + $_[1] } my $c; $c = two 1, 2; print "$c\n";});
like($cl, qr/\(\$c \(make-p-box nil\)\)/,
     '#77 negative: a below-assignment tail is not a native root write');
test_cl('#77: `$c = f 1, 2` still assigns the call result',
    'sub two { $_[0] + $_[1] } my $c; $c = two 1, 2; print "$c\n";',
    "3\n");

# Unary PLUS is a no-op in perl, so `+$y` IS `$y` — a BOX, not a raw value.
# Calling it 'num' with `-` and `!` was a silent wrong of the bare-`$y` kind
# (`my $b = +$h; $h = 77` then read 77); it is value-TRANSPARENT now.
$cl = Pl::Parser2->parse_code(q{our $h; my $b = +$h; print "$b\n";});
unlike($cl, qr/\(\$b\w* \$h\)/,
       q{unary + over a variable never seeds the slot with that variable's BOX});
like($cl, qr/\(\$b\w* \(%pcl-to-string-strict /,
     q{... it is an unproven write, so the B-regime freeze COPIES instead});
$cl = Pl::Parser2->parse_code(q{my $b = +5; print $b+1,"\n";});
like($cl, qr/\(\$b\w* 5\)/, 'unary + over a literal is still proven num');
$cl = Pl::Parser2->parse_code(q{our $h; my $b = -$h; print $b+1,"\n";});
like($cl, qr/\(\$b\w* \(p-- \$h\)\)/, 'unary - still computes a raw value');

test_cl('#77 + the unary-plus fix: values are COPIED, never aliased',
    'our $g = 1; sub uplus { +$g } my $a = uplus(); $g = 99;
     our $h = 5; my $b = +$h; $h = 77;
     our $k = 2; sub plus0 { $k + 0 } my $c = plus0(); $k = 42;
     sub bare { $g } my $d = bare(); $g = 1000;
     print "$a $b $c $d\n";',
    "1 5 2 99\n");

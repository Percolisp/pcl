#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perf-levers-07.t — perf round 38 (s501q, docs/faster-codegen-suggestions.md
# §0.2u).  Three EMISSION levers and the copies they stand on:
#
#   #2514 sig-classic      a plain signature sub is lowered as the classic
#                          `my (PARAMS) = @_;` spelling + its arity check;
#   #2515 args-copy        a callee that reads @_ only by copying it builds @_
#                          from VALUES (`(p-args-body :copy …)'), and p-map's
#                          block-value slice reads values (%p-aslice-copy);
#   #2114 list-decl-split  `my ($a, $b) = (L1, L2);` is lowered as its
#                          declarations apart;
#   and the three silent wrongs found on the way, each a COPY that was an
#   alias: #2536 (a signature slurpy), #2570 (a p-raw-params parameter), and
#   the crash #2571 (a :str-buffer parameter bound to a raw argument).
#
# SHAPE rows assert the emission and that `PCL_OPT=-name' takes it away (the
# registry's rule, Pl/t/passes-01.t); ANSWER rows run the program and compare
# with perl 5.40.3's own output (probed; the expected text below IS perl's).
# Every ANSWER block also runs under PCL_OPT=none and must print the same.
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

sub transpile {
    my ($src, $opt) = @_;
    my $file = src_file($src);
    local $ENV{PCL_OPT} = $opt if defined $opt;
    delete local $ENV{PCL_OPT} if !defined $opt;
    return PCLCore::transpile(qq{$pl2cl $file});
}

sub run_pl {
    my ($src, $opt) = @_;
    my $cl = transpile($src, $opt);
    my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl;
    close $cfh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>/dev/null`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^(?:caught |compilation unit|-->|==>|PCL Runtime loaded).*\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

# Every line of EXPECTED must appear, whole, in OUT; then the same program
# under PCL_OPT=none must print exactly what the default run printed.
sub answers {
    my ($src, $expected, $what) = @_;
    my $out = run_pl($src);
    for my $line (split /\n/, $expected) {
        my $q = quotemeta $line;
        like($out, qr/^$q$/m, "$what: $line");
    }
    is(run_pl($src, 'none'), $out, "$what: PCL_OPT=none prints the same");
}

# ─────────────────────────────────────────────────────────────────────────────
# SHAPES
# ─────────────────────────────────────────────────────────────────────────────
my $SIG = <<'PERL';
use feature 'signatures'; no warnings;
sub add ($x, $y) { my $s = 0; $s += $x * $y; $s }
sub rest ($x, @r) { scalar(@r) }
print add(2, 3), " ", rest(1, 2, 3), "\n";
PERL
my $cl = transpile($SIG);
like($cl, qr/\(p-raw-params \(\(\$x :scalar\) \(\$y :scalar\)\)\s*\(:arity "main::add" 2 2 nil nil\)/,
     '#2514: a plain scalar signature takes p-raw-params with its arity clause');
like($cl, qr/\(p-args-body :copy\s*\(p-check-arity "main::rest" \(length \@_\) 1 nil t nil\)/,
     '#2514/#2515: a slurpy signature is the classic copy + arity, on a :copy @_');
unlike($cl, qr/p-copy-scalar-arg|p-sig-rest-array/, '#2514: no v1 signature binding left');
my $cl_off = transpile($SIG, '-sig-classic');
like($cl_off, qr/p-sig-rest-array/, 'PCL_OPT=-sig-classic keeps the v1 signature binding');

my $COPY = <<'PERL';
sub first2 { my (@l) = @_; "$l[0]$l[1]" }
sub writer { my ($x) = @_; $_[0] = 9; $x }
my @a = (1, 2); print first2(@a), writer(@a), "@a\n";
PERL
$cl = transpile($COPY);
like($cl, qr/\(p-sub pl-first2.*?\(p-args-body :copy/s, '#2515: a copying callee builds @_ from values');
unlike($cl, qr/\(p-sub pl-writer[^\n]*\n[^\n]*\n[^\n]*\n\s*\(p-args-body :copy/,
       '#2515: a callee that writes $_[0] keeps the aliasing @_');
unlike(transpile($COPY, '-args-copy'), qr/:copy/, 'PCL_OPT=-args-copy removes the :copy marker');

# #2515 (ii) is a compiler macro (runtime-only): read its expansion out of the
# loaded runtime.  The ITEM-side rewrite (#1010) is the control.
my ($lfh, $lfile) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $lfh <<'LISP';
(in-package :pcl)
(format t "blk ~a~%"
  (and (search "%p-aslice-copy"
         (string-downcase (prin1-to-string (funcall (compiler-macro-function 'p-map)
                                   '(p-map (lambda ($_) (p-aslice @d $_ 1)) (p-.. 0 3)) nil))))
       t))
(format t "item ~a~%"
  (and (search "%p-aslice-viv"
         (string-downcase (prin1-to-string (funcall (compiler-macro-function 'p-map)
                                   '(p-map (lambda ($_) $_) (p-aslice @d 0 1)) nil))))
       t))
(format t "short ~a~%" (and (fboundp '%p-case-map-short) (equal (%p-case-map-short "aBc1" t) "ABC1") (null (%p-case-map-short (coerce (list #\a (code-char 233)) 'string) t)) t))
(format t "nottail ~a~%"
  (and (search "%p-aslice-copy"
         (string-downcase (prin1-to-string (funcall (compiler-macro-function 'p-map)
                                   '(p-map (lambda ($_) (p-aslice @d $_) 1) (p-.. 0 3)) nil))))
       t))
LISP
close $lfh;
my $mech = `sbcl @sbcl_rt --load $lfile 2>&1`;
like($mech, qr/^blk T$/mi,  '#2515 (ii): a map block whose value is a slice reads it with %p-aslice-copy');
like($mech, qr/^item T$/mi, 'control: a slice among the map ITEMS still vivifies (#1010)');
like($mech, qr/^nottail NIL$/mi, '#2515 (ii): a slice that is not the block value is left alone');
like($mech, qr/^short T$/mi, '#2535: the short pure-ASCII one-pass map exists, maps, and declines at a high char');

my $LD = <<'PERL';
my ($s, $i) = ("", 0);
while ($i++ < 3) { $s .= "ab" }
print "$s\n";
PERL
$cl = transpile($LD);
like($cl, qr/\(\$s :str-buffer/, '#2114: the split declaration takes the str-buffer verdict');
unlike($cl, qr/p-list-= \(vector \$s \$i\)/, '#2114: no list assignment left');
like(transpile($LD, '-list-decl-split'), qr/p-list-= \(vector \$s \$i\)/,
     'PCL_OPT=-list-decl-split keeps the list declaration');

# ─────────────────────────────────────────────────────────────────────────────
# ANSWERS — #2536: a signature's slurpy parameter is a COPY (perl's rule).
# ─────────────────────────────────────────────────────────────────────────────
answers(<<'PERL', <<'EXPECTED', '#2536 slurpy copy');
use strict; use feature 'signatures'; no warnings;
sub s1 ($x, @r) { $r[0] = "y"; $x = "z"; scalar(@r) }
my @a = (1, 2, 3); s1(@a); print "1 @a\n";
sub s2 ($x, %h) { $h{k} = "changed"; $h{n} = 1; 1 } my %o = (k => "v"); s2(1, %o); print "3 $o{k} ", scalar(keys %o), "\n";
sub s3 (@l) { $_ *= 2 for @l; push @l, 0; "@l" } my @b = (1, 2); print "4 ", s3(@b), " | @b\n";
{ package P; use feature 'signatures'; sub new ($c) { bless {}, $c } sub m ($self, @l) { $l[0] = "meth"; 1 } } my @h = (1, 2); P->new->m(@h); print "10 @h\n";
sub s6 ($x, @r) { $r[0] = "lit"; 1 } my %hh = (a => 5); s6(1, $hh{a}, values %hh); print "11 $hh{a}\n";
my @nested = ([1], [2]); sub s7 (@l) { $l[0] = "gone"; $l[1][0] = "deep"; 1 } s7(@nested); print "12 ", ref($nested[0]), " $nested[1][0]\n";
sub s8 ($x, $y = 1, @r) { $r[0] = "d"; 1 } my @c = (1, 2, 3); s8(@c); print "13 @c\n";
PERL
1 1 2 3
3 v 1
4 2 4 0 | 1 2
10 1 2
11 5
12 ARRAY deep
13 1 2 3
EXPECTED

# #2570: a parameter of the raw fast path is a COPY taken at the call.
answers(<<'PERL', <<'EXPECTED', '#2570 raw parameter copy');
my @a = (1, 2);
sub f { my ($x) = @_; $a[0] = 5; $x + 0 } print "1 ", f(@a), "\n";
my $g = 7;
sub h { my ($x, $y) = @_; $g = 8; $x * 1 } print "2 ", h($g, 1), "\n";
my @b = (3, 4);
sub k { my $x = shift; $b[0] = 9; $x + 0 } print "3 ", k(@b), "\n";
my %h = (a => 1);
sub m1 { my ($k, $v) = @_; $h{a} = 2; $v + 0 } print "4 ", m1(%h), "\n";
PERL
1 1
2 7
3 3
4 1
EXPECTED

# #2571: a :str-buffer parameter starts as a buffer.
answers(<<'PERL', <<'EXPECTED', '#2571 str-buffer parameter');
my @a = (1, 2, 3);
sub c1 { my ($x, $y) = @_; $x .= "!"; "$x$y" } print "3 ", c1(@a), " | @a\n";
sub c3 { my ($x) = @_; $x .= "!"; $x } print c3("a"), "\n";
PERL
3 1!2 | 1 2 3
a!
EXPECTED

# #2572: a body that reaches @_ IMPLICITLY (`&name;`, a bare `pop`) is not on
# the raw path that drops @_.
answers(<<'PERL', <<'EXPECTED', '#2572 implicit @_');
sub w0 { $_[0] = 9 }
sub am { my ($x) = @_; &w0; $x } my @a = (1, 2); am(@a); print "1 @a\n";
sub ev { my ($x) = @_; eval q{$_[1] = "E"}; $x } ev(@a); print "2 @a\n";
sub pp { my ($x) = @_; my $l = pop; "$x$l" } print "3 ", pp(1, 2), "\n";
sub sp { my $x = shift; &w0; $x } my @b = (5, 6); sp(@b); print "4 @b\n";
PERL
1 9 2
2 9 E
3 12
4 5 9
EXPECTED

# #2515: the args-copy licence — the callees that COPY and the ones that do not.
answers(<<'PERL', <<'EXPECTED', '#2515 args-copy');
use strict; use warnings; no warnings 'redefine';
my @a = (1, 2, 3);
sub w0 { $_[0] = 9 } w0(@a); print "1 @a\n";
sub w1 { my ($x) = @_; $_[1]++; $x } w1(@a); print "2 @a\n";
sub c2 { my (@l) = @_; $l[0] = "z"; $_ .= "q" for @l; "@l" } print "4 ", c2(@a), " | @a\n";
my $r = \&c2; print "5 ", &$r(@a), " | @a\n";
{ package O; sub new { bless {}, shift } sub m1 { my ($s, @l) = @_; $l[0] = "m"; "@l" } }
print "7 ", O->new->m1(@a), " | @a\n";
my @h = (1, 2, 3, 4); print "10 ", c2(@h[1, 2]), " | @h\n";
my %hh = (k => "v"); sub hc { my (%x) = @_; $x{k} = "w"; join ",", %x } print "11 ", hc(%hh), " | $hh{k}\n";
sub rd { my ($x) = @_; $a[0] = "changed"; $x } @a = (1, 2, 3); print "13 ", rd(@a), " | @a\n";
my @holes; $holes[2] = 5; sub hl { my (@l) = @_; scalar(@l) . ":" . join(",", map { defined $_ ? $_ : "u" } @l) } print "14 ", hl(@holes), " | ", scalar(@holes), " ", (exists $holes[0] ? "e" : "ne"), "\n";
sub ev { my ($x) = @_; eval '$_[0] = "E"'; $x } print "17 ", ev(@a), " | @a\n";
sub am { my ($x) = @_; &w0; } am(@a); print "19 @a\n";
my @big = (1 .. 5); sub mut { my (@l) = @_; push @l, 1; $l[0]++; scalar(@l) } for (1 .. 3) { mut(@big) } print "22 @big\n";
PERL
1 9 2 3
2 9 3 3
4 zq 3q 3q | 9 3 3
5 zq 3q 3q | 9 3 3
7 m 3 3 | 9 3 3
10 zq 3q | 1 2 3 4
11 k,w | v
13 1 | changed 2 3
14 3:u,u,5 | 3 ne
17 changed | E 2 3
19 9 2 3
22 1 2 3 4 5
EXPECTED

# #2515 (ii): a map block whose VALUE is a slice reads values — no promotion,
# no vivification, and the result is still a copy.
answers(<<'PERL', <<'EXPECTED', '#2515 map slice');
my @a = (1, 2, 3); $_++ for map { @a[0, 1] } 1; print "1 @a\n";
my @b = (1, 2, 3); for my $x (map { @b[0, 5] } 1) { $x = 9 } print "2 @b ", scalar(@b), "\n";
my @c = (1, 2); my @r = map { @c[$_, $_ + 1] } 0 .. 0; $r[0] = 7; print "3 @c @r\n";
my %h = (a => 1, b => 2); $_ .= "x" for map { @h{qw(a b)} } 1; print "4 $h{a} $h{b}\n";
my @d = ([1], [2]); my @e = map { @d[0, 1] } 1; $e[0][0] = "deep"; print "5 $d[0][0]\n";
my @f = (1, 2, 3); my @g = map { @f[1 .. 9] } 1; print "6 ", scalar(@f), " ", scalar(@g), "\n";
sub ps { my @deck = @_; my $m = @deck / 2; map { @deck[$_, $_ + $m] } 0 .. $m - 1 } print "7 ", join(",", ps(1 .. 6)), "\n";
PERL
1 1 2 3
2 1 2 3 3
3 1 2 7 2
4 1 2
5 deep
6 3 9
7 1,4,2,5,3,6
EXPECTED

# #2514: the normalised signature subs answer as the v1 binding did (and as perl does).
answers(<<'PERL', <<'EXPECTED', '#2514 sig-classic');
use strict; use warnings; use feature qw(signatures say); no warnings qw(experimental::signatures);
sub t { my ($n, $c) = @_; my @r = eval { $c->() }; print "$n ", (@r || !$@ ? join(",", map { $_ // "undef" } @r) : "DIED " . ($@ =~ /^(.{0,50})/)[0]), "\n" }
sub sb ($x, $y) { $x .= "!"; $x .= $y; $x } t(1, sub { (sb("a", "b"), sb(1, 2)) });
my $x = "FILE"; sub shadow ($x) { $x . "!" } t(3, sub { (shadow("p"), $x) });
sub outer ($n) { sub inner ($m) { $m * 3 } inner($n) + 1 } t(4, sub { (outer(2), inner(5)) });
{ package Q; use feature 'signatures'; sub new ($c, %a) { bless {%a}, $c } sub v ($s) { $s->{v} } } t(5, sub { Q->new(v => 7)->v });
t(7, sub { Q->new(v => 1)->v(2) });
sub hs ($k, %h) { join ",", map { "$_=$h{$_}" } sort keys %h } t(11, sub { (hs(1, a => 1, b => 2), hs(1)) });
sub rec ($n, @acc) { return "@acc" if $n == 0; rec($n - 1, @acc, $n) } t(13, sub { rec(4) });
my @arr = (1, 2, 3); sub mut ($x, @r) { $x = 0; $_ = 0 for @r; scalar(@r) } t(14, sub { (mut(@arr), "@arr") });
sub lst ($x) { ($x, $x + 1) } t(17, sub { my @l = lst(5); my $s = lst(5); "@l|$s" });
sub refp ($r) { $$r = "set"; 1 } my $tgt = "orig"; t(18, sub { (refp(\$tgt), $tgt) });
sub add ($a, $b) { $a + $b } t(19, sub { add(1) }); t(20, sub { add(1, 2, 3) });
sub fact ($n) { $n <= 1 ? 1 : $n * fact($n - 1) } t(21, sub { fact(10) });
my $v = 5; sub mod ($y) { $y++; $y .= "!"; $y } t(22, sub { (mod($v), $v) });
PERL
1 a!b,1!2
3 p!,FILE
4 7,15
5 7
7 DIED Too many arguments for subroutine 'Q::v' (got 2; e
11 a=1,b=2,
13 4 3 2 1
14 2,1 2 3
17 5 6|6
18 1,set
19 DIED Too few arguments for subroutine 'main::add' (got 
20 DIED Too many arguments for subroutine 'main::add' (got
21 3628800
22 6!,5
EXPECTED

# #2114: the list declarations that split answer as before; the ones that must
# not split (a non-literal element, unequal counts, a value that is read) too.
answers(<<'PERL', <<'EXPECTED', '#2114 list-decl-split');
use strict; use warnings; no warnings qw(misc once);
sub p { print join(" ", map { $_ // "U" } @_), "\n" }
my ($s, $i) = ("", 0); while ($i++ < 5) { $s .= "ab" } p(1, $s, $i, length $s);
my ($a1, $b1, $c1) = (1, "two", 3.5); $a1 += 2; $b1 .= "!"; $c1 *= 2; p(2, $a1, $b1, $c1);
my $outer = "O"; { my ($outer, $y) = (1, $outer); p(3, $outer, $y); }
my ($x1, $x2) = (1); p(5, $x1, $x2);
my $n = (my ($c1a, $c2a) = (7, 8, 9)); p(11, $n, $c1a, $c2a);
my ($f1, $f2) = ("a$i", 'b$i'); p(14, $f1, $f2); my ($q1, $q2) = (undef, 0); p(15, $q1, $q2);
sub tail { my ($p1, $q3) = (1, 2) } my @t = tail(); my $ts = tail(); p(16, "@t", $ts);
my ($k1, $k2) = (1, 2); { my ($k1, $k2) = ($k2, $k1); p(18, $k1, $k2); } p(19, $k1, $k2);
my ($cl, $cn) = ("c", 0); my $closure = sub { $cl .= "x"; ++$cn }; $closure->() for 1 .. 2; $cl .= "y"; p(25, $cl, $cn);
my ($j1, $j2) = (1, 2), my $j3 = 3; p(32, $j1, $j2, $j3);
PERL
1 ababababab 6 10
2 3 two! 7
3 1 O
5 1 U
11 3 7 8
14 a6 b$i
15 U 0
16 1 2 2
18 2 1
19 1 2
25 cxxy 2
32 1 2 3
EXPECTED

# #2535: lc/uc in one pass — the answers of every shape the fast paths meet:
# short and long ASCII, a buffer (`.=` slot) and a capture (displaced) as the
# SOURCE, undecoded UTF-8 bytes (ASCII rules) vs decoded text, the empty
# string, a high char only at the end, the first-only pair, fc, and the
# interpolation escapes that lower to the same calls.  Rows 4 and 6 (decoded text at a /d site)
# are the ruled divergence "The per-scalar UTF-8 flag" (not-supported.md): they
# are checked only for PCL_OPT=none agreement, not against perl.
answers(<<'PERL', <<'EXPECTED', '#2535 case map');
no warnings; use feature "fc";
my $buf = ""; $buf .= "MiXeD" for 1 .. 2; print "1 ", lc($buf), " ", uc($buf), "\n";
"Hello World and more text here" =~ /(\w+ \w+)/; print "2 ", uc($1), " ", lc($1), "\n";
my $bytes = "h\xc3\xa9llo W\xc3\xb6rld"; print "3 ", join(",", map { sprintf "%vd", $_ } lc($bytes), uc($bytes)), "\n";
my $text = "h\x{e9}llo W\x{f6}rld"; print "4 ", join(",", map { sprintf "%vd", $_ } lc($text), uc($text)), "\n";
print "5 [", lc(""), "][", uc(""), "][", ucfirst(""), "]\n";
print "6 ", join(",", map { sprintf "%vd", $_ } uc("abc\x{e9}"), lc("ABC\x{c9}"), uc("ab\xc3\xa9")), "\n";
print "7 ", ucfirst("abc"), " ", lcfirst("ABC"), " ", (ucfirst("zt\x{e9}") eq "Zt\x{e9}" ? "u" : "b"), "\n";
print "8 ", fc("HeLLo"), " ", "\LABC\E \Uabc\E \labc \uxyz \FDEF", "\n";
my $long = "The Quick Brown Fox Jumps Over The Lazy Dog 0123456789"; print "9 ", uc($long), "\n";
print "10 ", join("", map { lc } qw(A b C)), uc("a1-b2_c3"), "\n";
PERL
1 mixedmixed MIXEDMIXED
2 HELLO WORLD hello world
3 104.195.169.108.108.111.32.119.195.182.114.108.100,72.195.169.76.76.79.32.87.195.182.82.76.68
5 [][][]
7 Abc aBC u
8 hello abc ABC abc Xyz def
9 THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG 0123456789
10 abcA1-B2_C3
EXPECTED

done_testing();

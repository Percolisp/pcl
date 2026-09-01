#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# aassign-01.t — AASSIGN_COMMON self-assignment regression tests
# Covers the p-array-= fix that snapshots the RHS before clearing the LHS.
# Without the fix, @a = @a clears @a and then reads from the (now empty) array.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 42;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
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
    my ($name, $code, $expected) = @_;
    my $got = run_cl($code);
    is($got, $expected, $name);
}

# ── Self-assignment: @a = @a ──────────────────────────────────────────────────
# Without the AASSIGN_COMMON fix, p-array-= clears fill-pointer before reading,
# so @a = @a emptied the array.

test_cl('@a = @a preserves elements',
    'our @a = (1, 2, 3); @a = @a; print "@a\n";',
    "1 2 3\n");

test_cl('@a = @a with strings',
    'our @a = ("foo", "bar", "burbl", "blah"); @a = @a; print "@a\n";',
    "foo bar burbl blah\n");

# ── Embedding self: @a = (x, @a, y) ─────────────────────────────────────────
# The nested @a inside (vector x @a y) was not snapshotted before clearing.

test_cl('@a = (100, @a, 200) embeds self correctly',
    'our @a = ("bar", "burbl", "blah"); @a = (100, @a, 200); print "@a\n";',
    "100 bar burbl blah 200\n");

test_cl('@a = ("X", @a, "Y") with string context',
    'our @a = ("bar", "burbl", "blah"); @a = ("XXX", @a, "YYY"); print "@a\n";',
    "XXX bar burbl blah YYY\n");

# ── List assignment with self-reference: (undef, @a) = @a ───────────────────
# p-list-= uses %p-flatten-list which already snapshots; ensure it still works.

test_cl('(undef, @a) = @a shifts first element',
    'our @a = ("foo", "bar", "burbl", "blah"); (undef, @a) = @a; print "@a\n";',
    "bar burbl blah\n");

# ── Chained: @a = @a; then embed ─────────────────────────────────────────────

test_cl('sequential self-assigns stay consistent',
    'our @a = (1,2,3); @a = @a; @a = (0, @a); print "@a\n";',
    "0 1 2 3\n");

# ── nil (deleted element) preservation through p-array-= ─────────────────────
# The snapshot must not drop nil slots (which mark deleted elements).

test_cl('delete then @a = @a preserves deleted slot',
    'my @a = (1,2,3,4); delete $a[1]; @a = @a;
     print exists($a[1]) ? "exists" : "deleted", "\n";',
    "deleted\n");

test_cl('delete then @a = reverse @a preserves nil slot',
    'my @a = (1,2,3,4); @a = reverse @a;
     delete $a[1];
     @a = reverse @a;
     print exists($a[2]) ? "exists" : "deleted", "\n";',
    "deleted\n");

# ── our (...) = (1..N) — RHS is LIST context, so .. is a range not a flip-flop ──
# Was generating (p-flipflop-num ...) for the RHS, yielding all-empty values.

test_cl('our (...) = (1..3) is a range',
    'our ($x, $y, $z) = (1..3); print "$x $y $z\n";',
    "1 2 3\n");

# ── AASSIGN_COMMON via our: RHS snapshotted before the LHS is overwritten ──────

test_cl('our (...) self-assign snapshots RHS',
    'our ($x,$y,$z) = (1..3); our ($y,$z) = ($x,$y); print "$x $y $z\n";',
    "1 1 2\n");

# ── Logical && in list context returns the list, not a scalar ──────────────────
# `$cond && ($x,$y)` in list context must yield ($x,$y); previously forced scalar.

test_cl('&& propagates list context to its value operand',
    'our ($x,$y,$z) = (1..3); my $t = 1; (our $y, our $z) = $t && ($x,$y); print "$x $y $z\n";',
    "1 1 2\n");

# ── #570: an `undef` PLACEHOLDER in a sub's leading `my (LIST) = @_` ───────────
# The signature fast path (Parser2::_extract_params) is POSITIONAL — the Nth
# name binds @_[N-1] — and it used to build the list by GREPPING the Symbol
# tokens out of it.  An `undef` placeholder is a Word, so it vanished and every
# later name moved one slot LEFT: `my (undef, $x) = @_` bound $_[0].  Silent
# wrong, and only in that one position: the same list with any other RHS, or
# after any other statement, was already right (rows 4-6 below).  The fix makes
# _extract_params DECLINE a list that is not exactly names and commas, so the
# statement lowers through the ordinary `my (LIST) = @_` path.
# Every expected string is real perl 5.40.3's output for the same program;
# rows 1-3 print A / A/B / A on the base tree (measured before the fix).

test_cl('#570 leading undef placeholder binds $_[1]',
    'sub f { my (undef, $x) = @_; return $x }
     print f(qw(A B)), "\n";',
    "B\n");

test_cl('#570 interior + repeated placeholders',
    'sub f2 { my ($a, undef, $c) = @_; return "$a/$c" }
     sub f3 { my (undef, undef, $z) = @_; return $z }
     sub f4 { my (undef, $x, undef, $y) = @_; return "$x$y" }
     print f2(qw(A B C)), " ", f3(qw(A B C)), " ", f4(qw(A B C D)), "\n";',
    "A/C C BD\n");

test_cl('#570 the placeholder does not disturb @_ or a later write',
    'sub g { my (undef, $x) = @_; $x .= "!"; return "$x:" . scalar(@_) . ":$_[0]" }
     print g(qw(A B)), "\n";',
    "B!:2:A\n");

# The shapes that were ALREADY right and must stay right (the discriminator:
# these prove the general list-assign path handles placeholders, which is why
# declining is a correct fix rather than a workaround).

test_cl('#570 regression: no placeholder, trailing placeholder, other RHS',
    'sub h1 { my ($a, $b) = @_; return "$a-$b" }
     sub h2 { my ($p, undef) = @_; return $p }
     sub h3 { my @c = @_; my (undef, $x) = @c; return $x }
     sub h4 { my (undef, $x) = @_[0..1]; return $x }
     print h1(qw(A B)), " ", h2(qw(A B)), " ", h3(qw(A B)), " ", h4(qw(A B)), "\n";',
    "A-B A B B\n");

test_cl('#570 insignificant tokens keep the fast path (nested parens, comment)',
    'sub k1 { my (($a), $b) = @_; return "$a$b" }
     sub k2 { my ($a, # a comment
                   $b) = @_; return "$a$b" }
     sub k3 { my ($a => $b) = @_; return "$a$b" }
     sub k4 { my ($a, $b,) = @_; return "$a$b" }
     print k1(qw(A B)), " ", k2(qw(A B)), " ", k3(qw(A B)), " ", k4(qw(A B)), "\n";',
    "AB AB AB AB\n");

# The EMISSION shape: the ordinary two-name list still takes the fast path (the
# decline must not fire on it), and the placeholder list must not.
{
    my ($fh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "sub ok1 { my (\$p, \$q) = \@_; return \$p }\n"
            . "sub no1 { my (undef, \$x) = \@_; return \$x }\n"
            . "print ok1(1,2), no1(1,2), \"\\n\";\n";
    close $fh;
    my $cl = `$pl2cl $pl 2>/dev/null`;
    like($cl, qr/p-raw-params \(\$p \$q\)/,
         '#570 shape: a plain name list still takes the params fast path');
    unlike($cl, qr/p-raw-params \(\$x\)/,
           '#570 shape: a placeholder list does NOT reach p-raw-params');
}

# ── #610: `my (undef) = LIST` — a list that declares NO NAME ──────────────────
# #570's residue, and a LOUD one: `Pl::Parser2::_multi_decl` reports "no names"
# for an all-placeholder list and `_lower_block`'s
# `die "Parser2 TODO: unsupported declaration"` took the WHOLE FILE, anywhere in
# the program.  The MIXED list is fine (that is #570 above), so the refusal was
# exactly "this list declares nothing".
#
# `my (undef) = LIST` declares nothing and evaluates LIST — once, in LIST
# context — discarding it; it is perl's idiomatic "consume and ignore".  It is
# NOT the `my ()` no-op (task #227), which has no RHS to run.  Every expected
# string below is real perl 5.40.3's output for the same program.

test_cl('#610 the three repro shapes: all-placeholder my-lists',
    'sub f7 { my (undef) = @_; return defined($_[0]) ? "def" : "undef" }
     sub g7 { my (undef, undef) = @_; "g" }
     my (undef) = (1,2);
     print f7(qw(A B)), " ", g7(1,2), " ok\n";',
    "def g ok\n");

# The RHS is a real list assignment: it RUNS, exactly once, in LIST context.
test_cl('#610 the RHS is evaluated ONCE, in LIST context',
    'my $n = 0; my @seen;
     sub h7 { $n++; push @seen, (wantarray ? "LIST" : defined(wantarray) ? "SCALAR" : "VOID"); return (7,8,9) }
     my (undef) = h7();
     print "$n @seen\n";',
    "1 LIST\n");

test_cl('#610 the statement value in SCALAR context is the RHS count',
    'sub v7 { my (undef) = (10,20,30) }
     sub w7 { my (undef, undef) = @_; return scalar(@_) }
     print v7(), " ", w7(1,2,3), "\n";',
    "3 3\n");

# `my (undef)` reaches STRING EVAL too — the refusal used to escape as $@.
test_cl('#610 ... and inside a string eval',
    'my $r = eval "my (undef) = (1,2,3); q(ok)";
     print defined($r) ? $r : "ERR:$@", "\n";',
    "ok\n");

test_cl('#610 an ARRAY on the RHS is read, not consumed',
    'my @a = (1,2,3);
     my (undef) = @a;
     my %h = (a=>1);
     my (undef) = %h;
     print "@a ", join(",", map {"$_=$h{$_}"} sort keys %h), "\n";',
    "1 2 3 a=1\n");

# The inverses: the mixed list (#570), the `my ()` no-op (#227) and a
# placeholder list with NO initialiser must all be untouched.  (No
# all-placeholder decl WITH an init here — that is the positive shape above;
# this row must pass on the base tree.)
test_cl('#610 inverse: mixed list, my (), and a no-init placeholder list',
    'sub m7 { my (undef, $x) = @_; return $x }
     sub n7 { my ($y, undef) = @_; return $y }
     my (); my (undef);
     print m7(qw(A B)), " ", n7(qw(A B)), "\n";',
    "B A\n");

# ── #721: a list assignment used as a VALUE ──────────────────────────────────
# perl: SCALAR/void context → the number of RHS elements; LIST context → the
# LHS lvalues AFTER the assignment.  PCL froze the SCALAR answer for the
# DECLARATION spellings (`my (LIST) =`, `my %h =`, `our … =` all lowered their
# assignment in scalar context, so the count leaked out as a one-element list)
# and never collected the undef placeholders or the element lvalues in
# p-list-='s list-context result.  Every expected string below is perl
# 5.40.3's own output.

my $SHOW = 'sub show { my $t = shift; my @l = @_;
                       print "$t=", scalar(@l), ":", join("|", map { defined $_ ? $_ : "U" } @l), "\n" }
            ';

test_cl('#721 a declaration as a sub tail yields the LHS in list context',
    $SHOW . 'sub d1 { my ($a, $b) = (10,20,30) }
             sub d2 { my ($a, undef) = (10,20,30) }
             sub d3 { my %h = (a=>1) }
             sub d4 { our ($O1,$O2) = (40,50,60) }
             show("d1", d1()); show("d2", d2()); show("d3", d3()); show("d4", d4());
             print "s=", scalar(d1()), ",", scalar(d2()), ",", scalar(d3()), ",", scalar(d4()), "\n";',
    "d1=2:10|20\nd2=2:10|20\nd3=2:a|1\nd4=2:40|50\ns=3,3,2,3\n");

# The same statements one line down in a MULTI-statement body: the sub-body
# :void regime is active there, so the tail declaration has to restore the
# CALLER's context exactly as an ordinary tail expression statement does.
test_cl('#721 ... and under the sub-body void regime (multi-statement body)',
    $SHOW . 'sub v1 { my $p = 1; my ($a, $b) = (10,20,30) }
             sub v2 { my $p = 1; my %h = (a=>1,b=>2) }
             sub v3 { my $p = 1; our ($P1,$P2) = (7,8,9) }
             show("v1", v1()); show("v2", v2()); show("v3", v3());
             print "s=", scalar(v1()), ",", scalar(v2()), ",", scalar(v3()), "\n";',
    "v1=2:10|20\nv2=4:a|1|b|2\nv3=2:7|8\ns=3,4,3\n");

# The undef placeholder is an LVALUE that takes its slot's value, not a hole:
# it consumes a slot AND contributes it to the list-context result.
test_cl('#721 undef placeholders are collected, `()` still yields nothing',
    $SHOW . 'our ($a1,$b1,$c1);
             show("u1", ((undef) = (10,20,30)));
             show("u2", ((undef,undef) = (10,20,30)));
             show("mid", (($a1,undef,$c1) = (1,2,3,4)));
             show("trail", (($a1,$b1,undef) = (1,2)));
             show("empty", (() = (10,20,30)));
             my $n = () = (1,2,3,4); print "countof=$n\n";',
    "u1=1:10\nu2=2:10|20\nmid=3:1|2|3\ntrail=3:1|2|U\nempty=0:\ncountof=4\n");

# Element / repeat lvalues: the generic p-setf arm and p-list-x collected
# nothing, so every such LHS returned a SHORT list.
test_cl('#721 element and repeat lvalues are collected too',
    $SHOW . 'our (%h, @arr, $z1, $z2);
             show("elem", (($h{a},$h{b}) = (1,2)));
             show("aelem", (($arr[0],$arr[1]) = (3,4)));
             show("mix", (($z1,$h{c},$arr[2]) = (5,6,7)));
             show("repeat", ((($z1,$z2) x 2) = (1,2,3,4)));',
    "elem=2:1|2\naelem=2:3|4\nmix=3:5|6|7\nrepeat=4:3|4|3|4\n");

# Every other block whose value is the declaration.
test_cl('#721 do / eval / map / ternary blocks ending in a declaration',
    $SHOW . 'show("do", do { my ($a,$b) = (1,2,3) });
             show("eval", eval { my ($a,$b) = (4,5,6) });
             show("map", map { my ($p,$q) = ($_, $_*2) } (1,2));
             sub t1 { my $c = shift; $c ? (my ($a,$b) = (1,2)) : (my ($x,$y) = (3,4)) }
             show("tern1", t1(1)); show("tern0", t1(0));
             my $ds = do { my ($a,$b) = (1,2,3) }; print "do_scalar=$ds\n";',
    "do=2:1|2\neval=2:4|5\nmap=4:1|2|2|4\ntern1=2:1|2\ntern0=2:3|4\ndo_scalar=3\n");

# INVERSES — the shapes that must NOT move.  A declaration in a CONDITION or
# a non-tail statement stays in its own (boolean/void) context, and the
# container spellings answered correctly before this change.
test_cl('#721 inverse: condition heads and non-tail statements are unchanged',
    'my %h = (a=>1);
     if (my ($k,$v) = %h) { print "if=$k\n" } else { print "if=no\n" }
     if (my ($z) = ()) { print "if2=yes\n" } else { print "if2=no\n" }
     print "grep0=", scalar(grep { my ($g) = (0) } (1,2,3)), "\n";
     print "grepE=", scalar(grep { my ($g) = () } (1,2,3)), "\n";
     sub vv { my ($v1,$v2) = (1,2); return "ok" }
     print "void=", vv(), "\n";',
    "if=a\nif2=no\ngrep0=3\ngrepE=0\nvoid=ok\n");

test_cl('#721 inverse: plain and container LHS keep their answers',
    $SHOW . 'our (@A, %H, $X, $Y);
             sub c1 { @A = (1,2,3) }  sub c2 { %H = (a=>1,b=>2) }
             sub c3 { ($X,$Y) = (10,20,30) }  sub c4 { my @z = (5,6) }
             show("c1", c1()); show("c2", c2()); show("c3", c3()); show("c4", c4());
             print "s=", scalar(c1()), ",", scalar(c2()), ",", scalar(c3()), ",", scalar(c4()), "\n";',
    "c1=3:1|2|3\nc2=4:a|1|b|2\nc3=2:10|20\nc4=2:5|6\ns=3,4,3,2\n");

# The lvalues perl hands back are WRITABLE for the named-scalar targets, and
# that must survive the collect (op/hashassign.t's `$_++ foreach (…) = (…)`
# family already depends on it for the greedy-starved tail).
test_cl('#721 the collected scalar lvalues are still writable',
    'our ($w1,$w2);
     for (($w1,$w2) = (1,2)) { $_ *= 10 }
     print "$w1 $w2\n";',
    "10 20\n");

# ── #720: an lvalue SLICE whose container must be AUTOVIVIFIED ───────────────
# A slice is a SUBSCRIPTED dereference, and perl vivifies the base of one —
# `@{ $h{k} }[0,1] = (11,12)` makes $h{k} an ARRAY ref and writes through it,
# and it vivifies even in RVALUE position (`my @y = @{ $h{k} }[0,1]` leaves
# the key present, while the BARE `@{ $h{k} }` does not).  The four slice
# emitters generated their container in RVALUE context, so `p-gethash` handed
# back the throwaway box a missing key reads as, `p-cast-@` autovivified into
# THAT and the write went nowhere (silent wrong); the hash spelling DIED in
# (setf p-gethash) on :undef, because `p-cast-%` was ALSO missing the
# autovivification arm its `p-cast-@` twin has.  Expected strings are perl
# 5.40.3's.

test_cl('#720 an lvalue slice autovivifies its container element',
    'use v5.24;
     my %a1; @{ $a1{k} }[0,1] = (11,12);
     my %a2; @{ ($a2{k}) }[0,1] = (21,22);
     my @ar; @{ $ar[2] }[0,1] = (31,32);
     my %a3; @{ $a3{k} }{qw(x y)} = (1,2);
     print "a1=", join(",",@{$a1{k}}), ":", ref($a1{k}), "\n";
     print "a2=", join(",",@{$a2{k}}), ":", ref($a2{k}), "\n";
     print "ar=", join(",",@{$ar[2]}), ":", ref($ar[2]), "\n";
     print "a3=", join(",", map {"$_=$a3{k}{$_}"} sort keys %{$a3{k}}), ":", ref($a3{k}), "\n";',
    "a1=11,12:ARRAY\na2=21,22:ARRAY\nar=31,32:ARRAY\na3=x=1,y=2:HASH\n");

# The postfix spelling and a MULTI-element parenthesised base reach the same
# emitters, and a kv-slice through an element must stop leaving the key absent.
test_cl('#720 postfix, parenthesised and kv spellings agree',
    'use v5.24;
     my %p1; $p1{k}->@[0,1] = (41,42);
     my %p2; @{ (0,$p2{k}) }{qw(x y)} = (1,2);
     my %p3; my @kv = %{ $p3{k} }{"x"};
     print "p1=", join(",",@{$p1{k}}), ":", ref($p1{k}), "\n";
     print "p2=", join(",", map {"$_=$p2{k}{$_}"} sort keys %{$p2{k}}), ":", ref($p2{k}), "\n";
     print "p3=", scalar(@kv), ":", (defined $p3{k} ? ref($p3{k}) : "undef"), "\n";',
    "p1=41,42:ARRAY\np2=x=1,y=2:HASH\np3=2:HASH\n");

# INVERSE — the containers that were already right must stay right: a slice
# through a scalar holding a ref, through an UNDEF scalar (which vivifies),
# through a bare aggregate, and the element sibling `@{ $h{k} } = (…)` whose
# `@` cast has always set the lvalue context this fix gives the slices.
test_cl('#720 inverse: ref, undef-scalar, bare and whole-aggregate containers',
    'use v5.24;
     my $r = []; @{ $r }[0,1] = (51,52);
     my $u;      @{ $u }[0,1] = (61,62);
     my $hr = {}; @{ ($hr) }{qw(x y)} = (7,8);
     my @bare = (0,0,0); @bare[0,1] = (71,72);
     my %bh; @bh{qw(a b)} = (81,82);
     my %w; @{ $w{k} } = (5,6);
     print "r=", join(",",@$r), " u=", join(",",@$u), ":", ref($u), "\n";
     print "hr=", join(",", map {"$_=$hr->{$_}"} sort keys %$hr), "\n";
     print "bare=", join(",",@bare), " bh=", join(",", map {"$_=$bh{$_}"} sort keys %bh), "\n";
     print "w=", join(",",@{$w{k}}), ":", ref($w{k}), "\n";',
    "r=51,52 u=61,62:ARRAY\nhr=x=7,y=8\nbare=71,72,0 bh=a=81,b=82\nw=5,6:ARRAY\n");

# ── A SLICE inside a list-assignment LHS absorbs its width (s452 review fix) ──
# The #736 marker arm in %p-flatten-list accidentally REPLACED the cons-splice
# arm, so a spliced value group collapsed to ONE element and the slice consumed
# one RHS value instead of three (perl-tests/range.t row 4 caught it: got
# a:b:::c for a:b:c:d:e).  Differential against real perl by construction.
test_cl("LHS slice absorbs its width in a list assignment",
    'my ($a, @bcd, $e);
     ($a, @bcd[0..2], $e) = ("a","b","c","d","e");
     print join(":", $a, @bcd[0..2], $e), "\n";',
    "a:b:c:d:e\n");

# ── #891: a swap whose values are REFERENCES ─────────────────────────────────
# `($x,$y) = ($y,$x)` was CORRECT for plain scalars and SILENTLY WRONG for
# references: %p-assign-snapshot returned the LIVE box for a ref/blessed/dualvar
# payload, so store 1 read $x back AFTER store 0 had overwritten it and both
# names ended up holding $y's referent.  #423 had already found this for the
# typeglob arm; the rule is the same for every payload that travels as a box.
# Every expectation below is real perl's output (probed, perl 5.40.3).

test_cl('#891 reference swap: array, hash, blessed, 3-cycle',
    'my $x=[1,2]; my $y=[9]; ($x,$y)=($y,$x);
     my $h1={a=>1}; my $h2={b=>2,c=>3}; ($h1,$h2)=($h2,$h1);
     my $o1=bless({},"A"); my $o2=bless({},"B"); ($o1,$o2)=($o2,$o1);
     my $c1=[1]; my $c2=[2]; my $c3=[3]; ($c1,$c2,$c3)=($c3,$c1,$c2);
     print scalar(@$x), ",", scalar(@$y), "\n";
     print scalar(keys %$h1), ",", scalar(keys %$h2), "\n";
     print ref($o1), ref($o2), "\n";
     print $$c1[0], $$c2[0], $$c3[0], "\n";',
    "1,2\n2,1\nBA\n312\n");

test_cl('#891 reference swap: scalar refs, mixed ref/plain, sub params',
    'my $s1="one"; my $s2="two"; my $r1=\$s1; my $r2=\$s2; ($r1,$r2)=($r2,$r1);
     my $m=[7]; my $n="N"; ($m,$n)=($n,$m);
     sub sw { my ($u,$v)=@_; ($u,$v)=($v,$u); return "$$u[0]$$v[0]" }
     print "$$r1,$$r2\n";
     print $m, ",", ref($n), $n->[0], "\n";
     print sw([4],[6]), "\n";',
    "two,one\nN,ARRAY7\n64\n");

test_cl('#891 reference swap through element places and slices',
    'my @e=([10],[20]);    ($e[0],$e[1])=($e[1],$e[0]);
     my %hh=(a=>[1],b=>[2]); ($hh{a},$hh{b})=($hh{b},$hh{a});
     my @a=([1],[2,3]);    @a[0,1] = @a[1,0];
     my %h=(x=>[1],y=>[2,3]); @h{qw(x y)} = @h{qw(y x)};
     print $e[0][0], ",", $e[1][0], "\n";
     print $hh{a}[0], ",", $hh{b}[0], "\n";
     print scalar(@{$a[0]}), ",", scalar(@{$a[1]}), "\n";
     print scalar(@{$h{x}}), ",", scalar(@{$h{y}}), "\n";',
    "20,10\n2,1\n2,1\n2,1\n");

# INVERSE — what the fresh CONTAINER box must not disturb.  The copy carries
# the class, the glob is-ref discriminator and a dualvar's cached NV, and it
# shares the REFERENT, which is what identity (== / refaddr / "ARRAY(0x..)")
# is keyed on.  #423's glob swap must stay fixed, and refaliasing must still
# resolve to the referent BOX through the copy.
test_cl('#891 inverse: identity, class, dualvar, glob swap, refaliasing',
    'use Scalar::Util qw(refaddr);
     no warnings "experimental::refaliasing"; use feature "refaliasing";
     my $r=[1]; my ($r2)=($r);
     my $o=bless([1],"Cls"); my ($o2)=($o);
     $!=13; my ($e9)=($!);
     our @G1=(1); our @G2=(2,3);
     my $g1=\*G1; my $g2=\*G2; ($g1,$g2)=($g2,$g1);
     our $ra; my $src="S"; \$ra = \$src; $src="T";
     push @$r2, 2;
     print refaddr($r)==refaddr($r2) ? "same" : "diff", " ", "$r" eq "$r2" ? "str" : "STR", " ", scalar(@$r), "\n";
     print ref($o2), " ", ($o==$o2 ? "same" : "diff"), "\n";
     print $e9+0, "|$e9\n";
     print scalar(@{*$g1{ARRAY}}), ",", scalar(@{*$g2{ARRAY}}), "\n";
     print "$ra\n";',
    "same str 2\nCls same\n13|Permission denied\n2,1\nT\n");

# ── #910: the fast path that keeps a container box LIVE ──────────────────────
# `my ($self,$x,$r) = @_` used to copy every reference-valued RHS element into a
# fresh box up front (#891's rule, two struct allocations per OO method entry).
# It now hands the LIVE box through and applies perl's COMMONALITY test instead
# — an element that IS one of this assignment's own target boxes is snapshotted,
# nothing else is.  The test is a RUNTIME eq against the targets on purpose:
# the shapes below are exactly the ones a compile-time LHS/RHS name comparison
# would miss, because the caller's boxes arrive through @_ and the RHS form
# mentions neither target name.  Every expectation is real perl's (probed 5.40.3).

test_cl('#910 common assignment through @_ (names invisible to the RHS form)',
    'our ($p,$q); sub f { ($p,$q) = @_ }
     $p=[1]; $q=[2,2]; f($q,$p);
     sub g { my ($u,$v)=@_; ($u,$v)=($v,$u); return "$$u[0]$$v[0]" }
     my $h={k=>1}; my ($a,$b) = ($h,$h);
     my $x={a=>[5,6]}; my $y="y0"; ($x,$y) = ($x->{a}, $x);
     print scalar(@$p), ",", scalar(@$q), "\n";
     print g([4],[6]), "\n";
     print(($a == $b ? "same" : "diff"), $a->{k}, $b->{k}, "\n");
     print scalar(@$x), " ", ref($y), " ", scalar(@{$y->{a}}), "\n";',
    "2,1\n64\nsame11\n2 HASH 2\n");

test_cl('#910 inverse: a live container box must not leak into the target',
    'use Scalar::Util qw(refaddr dualvar);
     my $cv = "orig"; sub h { my ($only) = @_; $only = "changed"; } h($cv);
     my $hr = {n=>1}; sub h2 { my ($o) = @_; $o = {n=>2}; } h2($hr);
     my $o = bless [1], "Cls";
     sub k { my ($self) = @_; return (ref($self), refaddr($self)) }
     my ($cls,$ad) = k($o);
     my $d1 = dualvar(11,"eleven"); my $d2 = dualvar(22,"twenty-two");
     ($d1,$d2) = ($d2,$d1);
     my $c1 = sub { "one" }; my $c2 = sub { "two" }; ($c1,$c2) = ($c2,$c1);
     print "$cv ", $hr->{n}, "\n";
     print "$cls ", ($ad == refaddr($o) ? "same" : "diff"), "\n";
     print $d1+0, "|$d1 ", $d2+0, "|$d2\n";
     print $c1->(), $c2->(), "\n";',
    "orig 1\nCls same\n22|twenty-two 11|eleven\ntwoone\n");

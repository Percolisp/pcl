#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# classic-sort-01.t — task #996 half A5: the `classic-sort' optimization
# (Pl/ClassicSort.pm, a Kind-B pass in the registry; runtime %p-sort-classic).
#
# `sort LIST' and the four classic comparator blocks lower to a path that
# reads VALUES instead of boxing every element and funcalling a comparator per
# comparison — and, as much to the point, without %p-collect-list's MONOTONE
# promotion of the source array's raw slots.
#
# THE LICENCE IS ABOUT THE CONSUMER, NOT THE COMPARATOR.  `sort' returns
# ALIASES in perl and in PCL — `$_++ for sort { $a <=> $b } @a' writes back
# into @a — so a path that sorts values is legal only where nobody can observe
# which box came back.  Two licences, either one enough:
#   A  a COPYING consumer (array/list assignment, [ … ], push, join/print/
#      say/printf, return, and a `foreach' whose loop variable is only ever
#      READ — the `foreach-raw' verdict);
#   B  every top-level argument is a fresh-value producer (a literal, keys,
#      map, split, readdir, glob, a range, a user sub call).
# EVERY expectation below is the live perl 5.40.3 answer, and the aliasing
# rows are the ones that say WHY the negatives must stay generic.
#
# `values' IS NOT FRESH — measured, and the reason there is a row for it:
# `for my $v (sort { $a <=> $b } values %h) { $v .= "!" }' writes back into
# the hash, in perl and in PCL alike.  So do `grep' and a nested `sort'.
#
# NOR IS A `do { … }' TAIL A COPYING CONSUMER (merge review): perl hands its
# aliases through, so it keeps the general form; `eval { }' and a sub tail do
# copy.  Both are rows below.
#
# THE (A) `foreach-raw' MEMBER'S BOUNDARY, WHICH IS NOW EXACT (s470bj, task
# #1140): that verdict says the loop variable is only ever READ, which does
# not by itself cover a write to the ARRAY by another path during the loop.
# VarAnnotator's array facts supply the other half, so Parser2 emits
# `p-foreach-raw' only when every array NAMED in the list is a non-escaping
# `my @a' the body does not write — and this pass inherits the fix by reading
# that head.  `for my $x (sort { $a <=> $b } @fs) { $fs[1] = 99; print $x;
# last }' prints perl's 99 now; the row lives in Pl/t/array-facts-01.t
# (`classic-sort follows: the sorted SOURCE array is written in the body'),
# beside its plain twin, because both are one fact.

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

plan tests => 29;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub emitted { return PCLCore::transpile("$pl2cl " . write_pl($_[0])) }

# Transpile (a dropped statement fails the row, via PCLCore) and run.  OPT, if
# given, is the PCL_OPT setting for the TRANSPILE — the runtime reads none.
sub run_cl {
    my ($code, $opt) = @_;
    my $pre = defined $opt ? "PCL_OPT=$opt " : '';
    my $cl_code = PCLCore::transpile($pre . "$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

my $HEAD = "my \@src = (3,1,2); my \%h = (bb=>2, aa=>1); my \$cmp = sub { \$_[0] <=> \$_[1] };\n"
         . "sub byname { \$a <=> \$b }\n";

# ── 1. THE FIVE LICENSED SHAPES, licence A (an array assignment copies) ──────
my @positive = (
  ['my @x = sort { $a <=> $b } @src;', ':num-asc',  '{ $a <=> $b }'],
  ['my @x = sort { $b <=> $a } @src;', ':num-desc', '{ $b <=> $a }'],
  ['my @x = sort { $a cmp $b } @src;', ':str-asc',  '{ $a cmp $b }'],
  ['my @x = sort { $b cmp $a } @src;', ':str-desc', '{ $b cmp $a }'],
  ['my @x = sort @src;',               ':default',  'sort LIST (no block)'],
);
for my $p (@positive) {
    my ($code, $mode, $what) = @$p;
    like(emitted($HEAD . $code), qr/\Q(%p-sort-classic $mode\E\s/,
         "licence A + $what lowers to %p-sort-classic $mode");
}

# ── 2. LICENCE B: a fresh source licenses a NON-copying consumer ─────────────
# `map' does not copy its input list (its block sees the aliases), so these two
# rows are licence B alone: `keys' and a user sub call make fresh values.
like(emitted($HEAD . 'my @x = map { $_ } sort { $a cmp $b } keys %h;'),
     qr/\Q(%p-sort-classic :str-asc\E\s/,
     'licence B: `keys` is a fresh source, so a non-copying consumer is fine');
like(emitted($HEAD . 'sub gen { return (3,1,2) } my @x = map { $_ } sort { $a <=> $b } gen();'),
     qr/\Q(%p-sort-classic :num-asc\E\s/,
     'licence B: a user sub call is a fresh source (the call boundary copies)');

# The `foreach-raw' verdict IS the licence for a foreach list: the loop
# variable is only ever read, so the alias is unobservable.  The negative twin
# is row 4 below — the same loop that WRITES $x keeps the general form.
like(emitted($HEAD . 'my @o; for my $x (sort { $a <=> $b } @src) { push @o, $x }'),
     qr/p-foreach-raw.*\Q(%p-sort-classic :num-asc\E/s,
     'a foreach-raw list is a copying consumer (the same fact, not a second walk)');

# ── 3. THE NEGATIVES — each must keep (p-sort …) ─────────────────────────────
my @negative = (
  ['my @x = sort byname @src;',                       'sort NAME LIST'],
  ['my @x = sort $cmp @src;',                         'sort $subref LIST'],
  ['my @x = sort { $a->{k} <=> $b->{k} } @src;',      'a key extractor'],
  ['my @x = sort { lc($a) cmp lc($b) } @src;',        'a function of $a/$b'],
  ['my @x = sort { $a <=> $b || $a cmp $b } @src;',   'an || chain'],
  ['my @x = scalar(sort @src);',                      'sort in scalar context'],
  ['my @x = map { $_ } sort { $a <=> $b } @src;',     'no licence: array source + non-copying consumer'],
  ['my @x = map { $_ } sort { $a <=> $b } values %h;','`values` hands aliases through'],
  ['my @x = map { $_ } sort { $a <=> $b } grep { 1 } @src;', '`grep` hands aliases through'],
  ['my @o; for my $x (sort { $a <=> $b } @src) { $x .= "!" }', 'a foreach that WRITES the loop variable'],
);
for my $n (@negative) {
    my ($code, $what) = @$n;
    unlike(emitted($HEAD . $code), qr/%p-sort-classic/, "NOT licensed: $what");
}

# A `do { … }' TAIL IS NOT A COPYING CONSUMER (s470a5 merge review).  perl
# hands its aliases through — `$_++ for do { sort { $a <=> $b } @d }' turns
# (3,1,2) into (4,2,3), probed — so a do-block around a sort keeps the general
# form, while `eval { }' and a sub tail (which DO copy) may license it.  The
# runtime rows below carry the do-block's own value answer.
unlike(emitted('my @d = (3,1,2); $_++ for do { sort { $a <=> $b } @d };'),
       qr/%p-sort-classic/,
       'NOT licensed: a `do { … }` tail hands the aliases through');
like(emitted('our @f = (3,1,2); sub s1 { return sort { $a <=> $b } @f }'),
     qr/p-tail-value \(\Q%p-sort-classic :num-asc\E/,
     'a sub TAIL is a copying consumer (perl copies a returned list)');

# The reverse twin: `reverse' hands aliases through, so it is NOT a fresh
# source (licence B fails) — but a COPYING consumer licenses the sort anyway,
# and `reverse' is transparent to that licence in the other direction too.
like(emitted($HEAD . 'my @x = sort { $b <=> $a } reverse @src;'),
     qr/\Q(%p-sort-classic :num-desc\E/,
     'a copying consumer licenses even an aliasing `reverse` source');
unlike(emitted($HEAD . 'my @x = map { $_ } sort { $b <=> $a } reverse @src;'),
     qr/%p-sort-classic/,
     'NOT licensed: `reverse` is not a fresh source for licence B');

# ── 4. VALUES: the fast path answers exactly what the general path answers ───
# perl 5.40.3, verbatim.  numification ("abc"/undef -> 0, "3x" -> 3), cmp on
# codepoints, NaN in both directions, stability with equal keys, and $a/$b
# untouched after the sort.
my $VALUES = <<'PL';
no warnings;
my @src = (3, 1, 2);
my @a = sort { $a <=> $b } @src;
my @b = sort { $b <=> $a } @src;
my @c = sort { $a cmp $b } @src;
my @d = sort { $b cmp $a } @src;
my @e = sort @src;
print "asc:@a desc:@b cmpa:@c cmpd:@d def:@e\n";
my %h = (bb => 2, aa => 1, cc => 3);
my @k;
for my $key (sort keys %h) { push @k, $key }
print "keys:@k\n";
print "join:", join("|", sort { $a <=> $b } @src), "\n";
my $ar = [ sort { $b <=> $a } @src ];
print "anon:@$ar\n";
sub ret { return sort { $a <=> $b } @src }
print "ret:", join(",", ret()), "\n";
my @mixed = ("10", 9, "abc", undef, "3x");
print "mixnum:", join(",", map { defined $_ ? $_ : "U" } sort { $a <=> $b } @mixed), "\n";
print "mixstr:", join(",", map { defined $_ ? $_ : "U" } sort @mixed), "\n";
my @nan = (3, 9**9**9/9**9**9, 1, 2);
print "nan:", join(",", sort { $a <=> $b } @nan), " nand:", join(",", sort { $b <=> $a } @nan), "\n";
my @pairs = ([2,'a'],[1,'b'],[2,'c'],[1,'d']);
my @st = map { $_->[1] } sort { $a->[0] <=> $b->[0] } @pairs;
print "stable:@st\n";
my @ties = (2, 1, 2, 1);
print "eqkeys:", join(",", sort { $a <=> $b } @ties), "\n";
print "ab:", (defined($a) ? "D" : "U"), (defined($b) ? "D" : "U"), "\n";
print "srcsame:@src\n";
PL
my $VALUES_WANT = <<'OUT';
asc:1 2 3 desc:3 2 1 cmpa:1 2 3 cmpd:3 2 1 def:1 2 3
keys:aa bb cc
join:1|2|3
anon:3 2 1
ret:1,2,3
mixnum:abc,U,3x,9,10
mixstr:U,10,3x,9,abc
nan:1,2,3,NaN nand:3,NaN,2,1
stable:b d a c
eqkeys:1,1,2,2
ab:UU
srcsame:3 1 2
OUT
is(run_cl($VALUES), $VALUES_WANT, 'the fast path answers perl on every value shape');
is(run_cl($VALUES, 'none'), $VALUES_WANT,
   'PCL_OPT=none runs identically — the registry contract');

# ── 5. ALIASING: what the licences exist to protect ──────────────────────────
# Every row here must keep the general form, and the run proves why: the write
# through the loop variable / $_ reaches the SOURCE, as perl does.
my $ALIAS = <<'PL';
no warnings;
my @a = (3,1,2);
$_++ for sort { $a <=> $b } @a;
print "topic:@a\n";
my @b = ("b","a");
for my $x (sort { $a cmp $b } @b) { $x .= "!" }
print "namedwrite:@b\n";
my @c = ("b","a");
my @r = map { \$_ } sort @c;
print "ident:", (($r[0] == \$c[1]) ? "same" : "copy"), "\n";
my %h = (b=>1, a=>2);
for my $v (sort { $a <=> $b } values %h) { $v .= "!" }
print "values:", join(",", map { $h{$_} } sort keys %h), "\n";
my @g = (3,1,2);
for my $x (sort { $a <=> $b } grep { 1 } @g) { $x .= "!" }
print "grep:@g\n";
my @rv = (3,1,2);
for my $x (reverse sort { $a <=> $b } @rv) { $x .= "!" }
print "reverse:@rv\n";
my @ro = (3,1,2);
my @out;
for my $x (sort { $a <=> $b } @ro) { push @out, $x }
print "readonly:@ro|@out\n";
my @d = (3,1,2);
$_++ for do { sort { $a <=> $b } @d };
print "doblock:@d\n";
my @ev = (3,1,2);
$_++ for eval { sort { $a <=> $b } @ev };
print "evalblock:@ev\n";
our @st = (3,1,2);
sub s1 { return sort { $a <=> $b } @st }
$_++ for s1();
print "subtail:@st\n";
PL
my $ALIAS_WANT = <<'OUT';
topic:4 2 3
namedwrite:b! a!
ident:same
values:2!,1!
grep:3! 1! 2!
reverse:3! 1! 2!
readonly:3 1 2|1 2 3
doblock:4 2 3
evalblock:3 1 2
subtail:3 1 2
OUT
is(run_cl($ALIAS), $ALIAS_WANT, 'sort returns ALIASES where the licence does not hold');
is(run_cl($ALIAS, 'none'), $ALIAS_WANT, 'the aliasing answers are the same under PCL_OPT=none');

# ── 6. THE RUNTIME FALLBACK: an overloaded, tied or dualvar element ──────────
# Each of these is licensed at COMPILE time (an array assignment) and must be
# handed back to p-sort at RUN time, because reading the value runs user code.
# `Num' reverses `cmp' on purpose: a fallback that stringified instead of
# dispatching the handler would answer N1,N2,N3 here.
my $MAGIC = <<'PL';
package Num;
use overload '<=>' => sub { $_[0]{v} <=> $_[1]{v} },
             'cmp' => sub { $_[1]{v} cmp $_[0]{v} },
             '""'  => sub { "N" . $_[0]{v} },
             fallback => 0;
sub new { my ($c,$v) = @_; return bless {v=>$v}, $c }
package TS;
sub TIESCALAR { my ($c,$v) = @_; my $x = $v; return bless \$x, $c }
sub FETCH { my $s = shift; $main::fetches++; return $$s }
sub STORE { my ($s,$v) = @_; $$s = $v }
package main;
no warnings;
my @o = (Num->new(3), Num->new(1), Num->new(2));
my @s1 = sort { $a <=> $b } @o;
print "ovnum:", join(",", map { "$_" } @s1), "\n";
my @s2 = sort { $a cmp $b } @o;
print "ovcmp:", join(",", map { "$_" } @s2), "\n";
our $fetches = 0;
my ($t, $u, $v);
tie $t, 'TS', 3; tie $u, 'TS', 1; tie $v, 'TS', 2;
my @s3 = sort { $a <=> $b } ($t, $u, $v);
print "tie:", join(",", @s3), " fetched:", ($fetches > 0 ? "yes" : "no"), "\n";
my @s4 = sort ($t, $u, $v);
print "tiedef:", join(",", @s4), "\n";
use Scalar::Util qw(dualvar);
my @d = (dualvar(3, "aaa"), dualvar(1, "zzz"));
print "dual:", join(",", map { $_+0 } sort { $a <=> $b } @d),
      "|", join(",", map { "$_" } sort { $a cmp $b } @d), "\n";
PL
my $MAGIC_WANT = <<'OUT';
ovnum:N1,N2,N3
ovcmp:N3,N2,N1
tie:1,2,3 fetched:yes
tiedef:1,2,3
dual:1,3|aaa,zzz
OUT
is(run_cl($MAGIC), $MAGIC_WANT,
   'overload / tie / dualvar elements fall back to p-sort at run time');
is(run_cl($MAGIC, 'none'), $MAGIC_WANT, 'the fallback answers are PCL_OPT=none`s own');

# ── 7. RULE 12: the mode set is closed ───────────────────────────────────────
{
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh qq{(handler-case (pcl::%p-sort-classic :no-such-mode 1 2)\n}
               . qq{  (error (e) (format t "DIED: ~A~%" e)))\n};
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    like($out, qr/DIED:.*unknown mode :NO-SUCH-MODE/i,
         'an unknown mode DIES naming the value (rule 12)');
}

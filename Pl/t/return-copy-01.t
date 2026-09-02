#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# return-copy-01.t — THE SUB-FRAME LEAVE RULE (task #964, s464a).
#
# perl's pp_leavesub returns a mortal COPY of every value a non-lvalue sub
# returns; only a `:lvalue` sub returns the variable itself.  PCL used to hand
# the tail expression's BOX straight out of the frame, so every aliasing
# consumer — foreach, the `for` statement modifier, map/grep's $_, `\f()`, an
# @_-writing callee — wrote through it into the RETURNED VARIABLE:
#
#     my $x = 1; sub f { $x }
#     for my $v (f()) { $v = 5 }      # perl leaves $x = 1; PCL printed 5
#
# 24 of the 38 probe rows below leaked that way, identically under the default
# optimizer and under PCL_OPT=none, so it was the return PROTOCOL and not an
# optimizer regime.  The fix is ONE runtime rule applied ONCE at the frame exit
# (pcl::%p-leavesub, reached through the p-sub-frame macro that p-sub uses and
# the anon-sub wrapper emits), never N copies of the rule at the consumers.
#
# EVERY expected string in this file was probed against perl 5.40.3 first: run
# the same program with `perl` and with `./runpcl` and the two are byte-equal.
# The four programs are grouped so the file costs FOUR SBCL launches, not 38:
#   A  the scalar family        — tail / return / \f() / @_-writer / refs
#   B  the list family          — (x,y) / @a / @_ / shift / slices / %h / $_
#   C  anon, &f, goto, string eval, methods — the frames that are NOT p-sub
#   D  the shapes that must NOT change — the non-leaking family, referent
#      sharing through the copy, object identity, wantarray, undef/empty edges
# D is the inverse guard: an over-eager copy (of the REFERENT rather than the
# reference, or of a blessed box's identity) fails there, not in A–C.
#
# The single-element foreach spelling of the same rule lives with its family in
# Pl/t/foreach-aliasing-01.t ('for(f()) does not write back').

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

plan tests => 6;

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
    is(run_cl($code), $expected, $name);
}

sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return scalar `$pl2cl $pl_file 2>/dev/null`;
}

# ─────────────────────────────────────────────────────────────────────────────
# A — the SCALAR family.  Twelve ways to reach the returned scalar; on the base
# compiler ten of them wrote through into $x (only `return $x`, which
# p-return-value already unboxed, and the explicit scalar() spelling held).
# ─────────────────────────────────────────────────────────────────────────────
test_cl('scalar family: a returned scalar is a copy at every consumer', <<'PL', <<'OUT');
my $x = 1; my $rr = {k=>1}; our $g = 1;
sub f    { $x }
sub fr   { return $x }
sub fr2  { return $x if 1; 0 }
sub fref { $rr }
sub frefr { return $rr }
sub fg   { $g }
sub mod  { $_[0] = 12 }
sub r { $x = 1; $g = 1; $rr = {k=>1} }
r(); for my $v (f())  { $v = 5 }  print "tail-foreach=$x\n";
r(); for my $v (fr()) { $v = 5 }  print "return-foreach=$x\n";
r(); for my $v (fr2()){ $v = 5 }  print "returnif-foreach=$x\n";
r(); $_ = 7 for f();              print "modifier=$x\n";
r(); my @m = map { $_ = 8 } f();  print "map=$x\n";
r(); my @q = grep { $_ = 9 } f(); print "grep=$x\n";
r(); my $r1 = \f();  $$r1 = 10;   print "backslash-tail=$x\n";
r(); my $r2 = \fr(); $$r2 = 11;   print "backslash-return=$x\n";
r(); mod(f());                    print "argwriter=$x\n";
r(); my $r3 = \(scalar f()); $$r3 = 13; print "scalar-ctx=$x\n";
r(); for my $v (fref())  { $v = 0 } print "ref-tail=", (ref($rr)||"scalar:$rr"), "\n";
r(); for my $v (frefr()) { $v = 0 } print "ref-return=", (ref($rr)||"scalar:$rr"), "\n";
r(); for my $v (fg()) { $v = 'W' }  print "our-scalar=$g\n";
PL
tail-foreach=1
return-foreach=1
returnif-foreach=1
modifier=1
map=1
grep=1
backslash-tail=1
backslash-return=1
argwriter=1
scalar-ctx=1
ref-tail=HASH
ref-return=HASH
our-scalar=1
OUT

# ─────────────────────────────────────────────────────────────────────────────
# B — the LIST family.  A list-context frame copies PER ELEMENT, into a FRESH
# vector: the `(vector $x $s)` temp of a literal list, the element cells of a
# flattened array, the caller's own @_ boxes (`&f;` and `goto &f` SHARE @_, so
# the copy can never be made in place), a slice's cells, and a raw hash's
# values.  `shift-ref=copied` is [perl #91844] — perl's own t/op/sub.t row 9.
# ─────────────────────────────────────────────────────────────────────────────
test_cl('list family: every element of a returned list is a copy', <<'PL', <<'OUT');
my $x = 1; my $s = "abc"; my @a = (1,2,3); my %h = (a=>1); my @big = (1,2,3,4);
sub fl    { ($x, $s) }
sub flr   { return ($x, $s) }
sub fa    { @a }
sub far   { return @a }
sub fargs { @_ }
sub fsh   { shift }
sub fst   { $s }
sub fu    { $_ }
sub fhash { %h }
sub fslice { @big[1,2] }
sub r { $x = 1; $s = "abc"; @a = (1,2,3); %h = (a=>1); @big = (1,2,3,4); $_ = "topic" }
r(); for my $v (fl())  { $v = 'W' } print "list-tail=$x $s\n";
r(); for my $v (flr()) { $v = 'W' } print "list-return=$x $s\n";
r(); for my $v (fa())  { $v = 'W' } print "array-tail=@a\n";
r(); for my $v (far()) { $v = 'W' } print "array-return=@a\n";
r(); my @m = map { $_ = 'M' } fa(); print "array-map=@a\n";
r(); for my $v (fargs($x, $s)) { $v = 'W' } print "args=$x $s\n";
r(); for my $v (fsh($x)) { $v = 'W' } print "shift=$x\n";
r(); print "shift-ref=", ((\fsh($x) == \$x) ? "SAME" : "copied"), "\n";
r(); for (fst()) { s/a/Z/ } print "string=$s\n";
r(); for my $v (fu()) { $v = 'W' } print "topic=$_\n";
r(); for my $v (fhash()) { $v = 'W' if $v eq '1' } print "hash=", join(",", map { "$_=$h{$_}" } sort keys %h), "\n";
r(); for my $v (fslice()) { $v = 'S' } print "slice=@big\n";
r(); my @c = fa(); $c[0] = 'C'; print "array-copy=@a / @c\n";
r(); my %cp = fhash(); $cp{a} = 'C'; print "hash-copy=", join(",", map { "$_=$h{$_}" } sort keys %h), " / $cp{a}\n";
PL
list-tail=1 abc
list-return=1 abc
array-tail=1 2 3
array-return=1 2 3
array-map=1 2 3
args=1 abc
shift=1
shift-ref=copied
string=abc
topic=topic
hash=a=1
slice=1 2 3 4
array-copy=1 2 3 / C 2 3
hash-copy=a=1 / C
OUT

# ─────────────────────────────────────────────────────────────────────────────
# C — the frames that are NOT p-sub.  An anon sub's wrapper is EMITTED (two
# sites in Pl/Parser2.pm's _lower_embedded_anon, one in Pl/Parser.pm's v1 seam),
# so it must emit p-sub-frame too or `sub { $x }->()` keeps leaking; `&f()`,
# `goto &f`, a sub defined inside a string eval and both method spellings all
# reach the same rule.  method-referent pins the other half: the copy is of the
# REFERENCE, so the object behind it is still shared and still ==.
# ─────────────────────────────────────────────────────────────────────────────
test_cl('anon / ampersand / goto / string-eval / method frames copy too', <<'PL', <<'OUT');
my $x = 1;
sub f     { $x }
sub fanon { my $c = sub { $x }; $c->() }
sub gf    { goto &f }
package O;
sub new   { bless {v=>1}, shift }
sub self  { $_[0] }
sub selfm { my $self = shift; $self }
package main;
sub r { $x = 1 }
r(); for my $v (fanon()) { $v = 'W' } print "anon-closure=$x\n";
r(); my $c = sub { $x }; for my $v ($c->()) { $v = 'W' } print "anon-direct=$x\n";
r(); for my $v (&f()) { $v = 'W' } print "ampersand=$x\n";
r(); for my $v (gf()) { $v = 'W' } print "goto=$x\n";
r(); eval 'sub fe { $x }'; for my $v (fe()) { $v = 'W' } print "string-eval=$x\n";
r(); my $o = O->new; my $rf = \$o->self; $$rf = 'X'; print "method-argbox=", (ref($o) || $o), "\n";
r(); my $o2 = O->new; for my $v ($o2->selfm) { $v = 'X' } print "method-my-self=", (ref($o2) || $o2), "\n";
r(); my $o3 = O->new; my $o4 = $o3->selfm; $o4->{v} = 7;
     print "method-referent=$o3->{v} ", (($o4 == $o3) ? "same" : "diff"), "\n";
r(); my $mk = sub { my $n = shift; sub { $n } }; my $c1 = $mk->(7); my $c2 = $mk->(8);
     print "closure-capture=", $c1->(), $c2->(), "\n";
PL
anon-closure=1
anon-direct=1
ampersand=1
goto=1
string-eval=1
method-argbox=O
method-my-self=O
method-referent=7 same
closure-capture=78
OUT

# ─────────────────────────────────────────────────────────────────────────────
# D — THE INVERSE GUARD.  Everything the copy must leave alone: the shapes that
# never leaked (a temp, a sub-local `my`, a raw aggregate element), the fact
# that a copied REFERENCE still reaches its shared referent, a blessed box's
# class and identity, wantarray, and the undef/empty-list edges where unboxing
# a box holding undef would otherwise make the scalar VANISH from a list.
# ─────────────────────────────────────────────────────────────────────────────
test_cl('inverse: the copy leaves referents, identity and the edges alone', <<'PL', <<'OUT');
my $x = 1; my $s = "abc"; my @a = (1,2,3); my $rr = {k=>1};
sub fc  { $x > 0 ? $x : $s }
sub for_ { $x || $s }
sub fdo { do { $x } }
sub fmy { my $t = $x; $t }
sub fel { $a[0] }
sub fh  { $rr->{k} }
sub fst { $s }
sub r { $x = 1; $s = "abc"; @a = (1,2,3); $rr = {k=>1} }
r(); for my $v (fc())  { $v = 'W' } print "ternary=$x\n";
r(); for my $v (for_()) { $v = 'W' } print "or=$x\n";
r(); for my $v (fdo()) { $v = 'W' } print "do-block=$x\n";
r(); for my $v (fmy()) { $v = 'W' } print "sub-local-my=$x\n";
r(); for my $v (fel()) { $v = 'W' } print "element=@a\n";
r(); for my $v (fh())  { $v = 'W' } print "hash-elem=$rr->{k}\n";
r(); my $c1 = fst(); $s .= "d"; print "copy-src=$c1\n";
r(); my $c2 = fst(); $c2 .= "d"; print "copy-dst=$s\n";
my @refs = ({k=>1},{k=>2});
sub frefs { @refs }
for my $h (frefs()) { $h->{k} = 9 }
print "referent=$refs[0]{k},$refs[1]{k}\n";
package Ov; use overload '""' => sub { "OV" }, '==' => sub { 1 }, fallback => 1;
sub new { bless {}, shift }
package main;
my $ov = Ov->new; sub fov { $ov }
my $ov2 = fov();
print "overload=$ov2 ", (($ov2 == $ov) ? "eq" : "ne"), " ", (ref($ov2) || '?'), "\n";
sub fwa { wantarray ? "L" : defined(wantarray) ? "S" : "V" }
my @w = fwa(); my $w = fwa();
print "wantarray=$w[0] $w\n";
sub fcount { @a } my $n = fcount();
print "scalar-count=$n\n";
sub fempty { return } my @e = fempty();
print "empty=", scalar(@e), "\n";
sub fundef { my $u; $u } my @u = fundef();
print "undef=", scalar(@u), " ", (defined($u[0]) ? 1 : 0), "\n";
sub fmulti { return (1, undef, 3) } my @m = fmulti();
print "undef-in-list=", scalar(@m), " ", join(",", map { defined($_) ? $_ : 'u' } @m), "\n";
sub frec { my $k = shift; return $k <= 1 ? 1 : $k * frec($k-1) }
print "recursion=", frec(5), "\n";
PL
ternary=1
or=1
do-block=1
sub-local-my=1
element=1 2 3
hash-elem=1
copy-src=abc
copy-dst=abc
referent=9,9
overload=OV eq Ov
wantarray=L S
scalar-count=3
empty=0
undef=1 0
undef-in-list=3 1,u,3
recursion=120
OUT

# ─────────────────────────────────────────────────────────────────────────────
# The SHAPE of the frame, at the one site the compiler writes out.  A named sub
# gets its frame from the p-sub macro (nothing to assert in the emission); an
# anonymous one is written by the emitter, and that is the site the copy would
# be lost at again.  The inverse guard is the second row: the frame belongs to
# a SUB, not to `eval { }` or a sort comparator, both of which keep the bare
# catch (perl copies at eval exit too — pp_leaveeval — but that is a sibling
# rule with its own probe set, task #987).
# ─────────────────────────────────────────────────────────────────────────────
like(transpile('my $x = 1; my $c = sub { $x }; print $c->(), "\n";'),
    qr/\(p-sub-frame\b/,
    'an anon sub is emitted with the p-sub-frame leave rule (#964)');
unlike(transpile('my $x = 1; my @s = sort { $a <=> $b } (3,1); my $e = eval { $x }; print "$e\n";'),
    qr/\(p-sub-frame\b/,
    'a sort comparator and eval { } keep the bare catch — not sub frames (#964 inverse)');

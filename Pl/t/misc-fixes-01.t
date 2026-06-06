#!/usr/bin/env perl
# misc-fixes-01.t - Small targeted fixes: auto-increment, splice scalar

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 55;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
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

# ── auto.t test 32: ++("99a") → numeric 100 ──────────────────────────────
# Perl string increment only applies to strings matching /^[a-zA-Z]*[0-9]*$/
# "99a" (digits then letter) does NOT match → numeric increment → 100

test_cl('++("99a") gives 100 (numeric, not string)',
    'my $foo; my $r = ++($foo = "99a"); print $r, "\n";',
    "100\n");

test_cl('++("99\\0a") gives 100 (NUL breaks string increment)',
    'my $foo; my $r = ++($foo = "99\0a"); print $r, "\n";',
    "100\n");

# Regression: pure-alpha strings still do string increment
test_cl('++("az") stays string: "ba"',
    'my $foo; my $r = ++($foo = "az"); print $r, "\n";',
    "ba\n");

test_cl('++("zz") stays string: "aaa"',
    'my $foo; my $r = ++($foo = "zz"); print $r, "\n";',
    "aaa\n");

test_cl('++("A99") stays string: "B00"',
    'my $foo; my $r = ++($foo = "A99"); print $r, "\n";',
    "B00\n");

test_cl('++("99") numeric: 100',
    'my $foo; my $r = ++($foo = "99"); print $r, "\n";',
    "100\n");

# ── splice.t test 21: splice in scalar context returns last removed element

test_cl('splice scalar: returns last removed element',
    'my @a = qw(red green blue);
     my $foo = splice @a, 1, 2;
     print $foo, "\n";',
    "blue\n");

test_cl('splice scalar: returns single removed element',
    'my @a = qw(a b c d);
     my $x = splice @a, 1, 1;
     print $x, "\n";',
    "b\n");

test_cl('splice scalar: array modified correctly',
    'my @a = qw(red green blue);
     splice @a, 1, 2;
     print join(",", @a), "\n";',
    "red\n");

test_cl('splice list context: returns all removed elements',
    'my @a = qw(red green blue);
     my @r = splice @a, 1, 2;
     print join(",", @r), "\n";',
    "green,blue\n");

test_cl('splice scalar: remove and replace',
    'my @a = (1,2,3,4,5);
     my $r = splice @a, 1, 2, 10, 20;
     print "$r ", join(",", @a), "\n";',
    "3 1,10,20,4,5\n");

test_cl('splice list: returns removed with replacement',
    'my @a = (1,2,3,4,5);
     my @r = splice @a, 1, 2, 10, 20;
     print join(",", @r), " ", join(",", @a), "\n";',
    "2,3 1,10,20,4,5\n");

# ── *_{ARRAY} typeglob slot access ────────────────────────────────────────
# *_{SLOT} accesses the SLOT of the current _ typeglob. Inside a sub,
# *_{ARRAY} returns a reference to @_.

test_cl('*_{ARRAY} returns array ref',
    'sub foo { *_{ARRAY} }
     my $aref = foo(1,2,3);
     print ref($aref), "\n";',
    "ARRAY\n");

test_cl('*_{ARRAY} contains correct arguments',
    'sub foo { *_{ARRAY} }
     my $aref = foo(10,20,30);
     print join(",", @$aref), "\n";',
    "10,20,30\n");

test_cl('*_{HASH} returns hash ref to %_ typeglob slot',
    '
     %_ = (a => 1, b => 2);
     my $href = *_{HASH};
     print ref($href), "\n";',
    "HASH\n");

# ── %{$ref}{"keys"} KV hash slice via block-deref ───────────────────────
# PPI gives Cast(%) + Block({ref}) + Block({"keys"}) for this form.

test_cl('%{$href}{"keys"} kv hash slice via block-deref',
    'my $h = {c=>3, d=>4, e=>5};
     my @a = %{$h}{"c","d","e"};
     print join(":", @a), "\n";',
    "c:3:d:4:e:5\n");

test_cl('%{$href}{key} single-key kv hash slice via block-deref',
    'my $h = {x => 42};
     my @kv = %{$h}{"x"};
     print "$kv[0]:$kv[1]\n";',
    "x:42\n");

# ── low-precedence prefix `not` as an assignment RHS ────────────────────
# 'not' (prec 3) is the loosest prefix op, so as the RHS of '=' it used to be
# grabbed as a bare token by the higher-prec '=' → "unknown node" parse crash,
# notably inside sub bodies ('my $x = not eval {...}' in Try::Tiny).
test_cl('my $x = not 5 inside a sub (not-as-RHS parse fix)',
    'sub f { my $x = not 5; return $x ? "T" : "F"; }
     print f(), "\n";',
    "F\n");

test_cl('my $x = not 0 yields true',
    'my $x = not 0; print $x ? "T\n" : "F\n";',
    "T\n");

# 'not 5' is the empty string (false), not undef
test_cl('$x = not 5 (no my) gives empty-string false',
    'my $y; $y = not 5; print "[", (defined $y ? $y : "U"), "]\n";',
    "[]\n");

# `not` must still bind looser than `==`: not $a == $b  ==  not ($a == $b)
test_cl('not binds looser than == (semantics preserved)',
    'my $a = 3; my $b = 3; my $r = not $a == $b; print $r ? "T\n" : "F\n";',
    "F\n");

# ── slurpy (@) prototype tail forces LIST context on those args ──────────
# A sub declared (&;@) runs its trailing @-slurped arguments in list context,
# so a context-sensitive call there sees wantarray true. (Found via Try::Tiny:
# catch croaks 'Useless bare catch()' unless wantarray.)
test_cl('arg in a (&;@) slurpy position runs in list context',
    'sub want { wantarray ? "LIST" : "SCALAR" }
     sub mytry (&;@) { my ($blk, @rest) = @_; return $rest[0]; }
     my $r = mytry { 0 } want();
     print "$r\n";',
    "LIST\n");

# Guard: a ($$) prototype does NOT force list context — its args inherit the
# call context (here scalar), matching Test::More is($$;$) keeping unpack scalar.
test_cl('($$) prototype arg is not forced to list context',
    'sub want { wantarray ? "LIST" : "SCALAR" }
     sub two ($$) { return "$_[0]"; }
     my $r = two(want(), 9);
     print "$r\n";',
    "SCALAR\n");

# Guard: a plain scalar assignment still gives scalar context.
test_cl('scalar assignment gives scalar context',
    'sub inner { return wantarray ? "LIST" : "SCALAR"; }
     my $s = inner();
     print "$s\n";',
    "SCALAR\n");

# ── return inside eval { } exits the eval, not the enclosing sub ──────────
# perldoc -f return: "return can also be used to exit an eval block". The
# eval evaluates to the returned value and the sub keeps running.
test_cl('return inside eval { } exits only the eval block',
    'sub f { my $x = eval { return 1; }; return "after x=$x"; }
     print f(), "\n";',
    "after x=1\n");

test_cl('eval { ...; return V } yields V to the assignment',
    'my $v = eval { 2 + 2; return 7; 99 };
     print "v=$v\n";',
    "v=7\n");

# ── &-prototype block argument is an anon sub (accepts @_) ────────────────
# A sub declared (&;@) receives its block as an anonymous sub, which must
# accept call arguments via @_ (Try::Tiny's catch is called with $error).
test_cl('block arg of a (&;@) sub accepts call arguments via @_',
    'sub mytry (&;@) { my ($blk, @rest) = @_; return $blk->("ARG"); }
     my $r = mytry { "got:$_[0]" };
     print "$r\n";',
    "got:ARG\n");

# ── use Carp imports croak/carp/confess; croak dies catchably ────────────
test_cl('use Carp; croak dies with the message (caught by eval)',
    'use Carp;
     eval { croak "boom" };
     print $@ =~ /^boom/ ? "caught\n" : "no\n";',
    "caught\n");

# ── glob-assign into a multi-segment package preserves case ──────────────
# *Foo::Bar::name = sub {...} must install into package Foo::Bar (case
# preserved), matching how codegen emits the qualified call.
test_cl('glob-assign a sub into a multi-segment package, then call it',
    '*Foo::Bar::greet = sub { "hi" };
     print Foo::Bar::greet(), "\n";',
    "hi\n");

# ── use constant value goes through the full number codegen path ─────────
# The "single literal" fast path used to emit the raw token, which mishandles
# octal (0777 -> CL 777) and crashes compile-file on float literals that
# overflow double range (POSIX::LDBL_MAX 1.18e+4932 — unreadable to SBCL).
test_cl('use constant octal literal is parsed as octal',
    'use constant FOO => 0777;
     my $v = FOO;
     print "$v\n";',
    "511\n");

test_cl('use constant overflowing float literal becomes Inf (not a crash)',
    'use constant BIG => 1.1897314953572317e+4932;
     my $v = BIG;
     print "$v\n";',
    "Inf\n");

# ── import list qw// with non-bracket delimiter ──────────────────────────
# The qw// handler only stripped bracket delimiters ([{< — qw/.../ passed
# through as the literal token "qw/sum/" so nothing imported.
test_cl('use Module qw/.../ (slash delimiter) imports correctly',
    'use List::Util qw/sum/;
     print sum(1..5), "\n";',
    "15\n");

# ── import a %hash variable into a non-main package ──────────────────────
# `use Config qw/%Config/` in package Foo used shadowing-import, which
# orphaned the already-compiled package-local %Config symbol (#:%CONFIG
# unbound). Now the local symbol shares the source value.
test_cl('use Config qw/%Config/ in a non-main package binds %Config',
    'package Foo;
     use Config qw/%Config/;
     sub kind { return ref(\\%Config); }
     package main;
     print Foo::kind(), "\n";',
    "HASH\n");

# ── mro: require mro + C3 get_linear_isa (C3-only shim) ───────────────────
# require mro in expression context used to misparse to (p-require (pl-mro))
# and crash; lib/mro.pm provides C3 get_linear_isa. See docs/mro-plan.md.
test_cl('require mro in expression context loads (no crash)',
    'my $ok = (1 && require mro) ? "yes" : "no";
     print "$ok\n";',
    "yes\n");

test_cl('mro::get_linear_isa returns C3 order for a diamond',
    'package A; our @ISA=();
     package B; our @ISA=("A");
     package C; our @ISA=("A");
     package D; our @ISA=("B","C");
     package main;
     require mro;
     print "@{mro::get_linear_isa(q(D))}\n";',
    "D B C A\n");

test_cl('\\&mro::get_linear_isa is a usable coderef',
    'package P; our @ISA=();
     package Q; our @ISA=("P");
     package main;
     require mro;
     my $code = \\&mro::get_linear_isa;
     print "@{$code->(q(Q))}\n";',
    "Q P\n");

# ── package-qualified hash/array ELEMENT ops ─────────────────────────────
# $Pkg::H{k}/$Pkg::A[i]: the qualified symbol contains "::%"/"::@" which the
# assignment dispatch and the exists/defined/delete sigil-rewrites used to
# mishandle (whole-hash p-hash-= on an element; wrong $-sigil container).
test_cl('qualified hash element: assign/read/exists/defined/delete',
    '$Pk::Q::H{k} = 7;
     my @o;
     push @o, $Pk::Q::H{k};
     push @o, (exists $Pk::Q::H{k} ? 1 : 0);
     push @o, (defined $Pk::Q::H{k} ? 1 : 0);
     push @o, delete $Pk::Q::H{k};
     push @o, (exists $Pk::Q::H{k} ? 1 : 0);
     print "@o\n";',
    "7 1 1 7 0\n");

test_cl('qualified array element: assign/read/exists/delete',
    '$Pk::Q::A[2] = "v";
     my @o;
     push @o, $Pk::Q::A[2];
     push @o, (exists $Pk::Q::A[2] ? 1 : 0);
     push @o, delete $Pk::Q::A[2];
     print "@o\n";',
    "v 1 v\n");

# ── package-qualified hash/array SLICES ──────────────────────────────────
# @Pkg::H{...} -> %Pkg::H and %Pkg::A[...] -> @Pkg::A sigil rewrites must
# handle the "::"-qualified position, not just a leading sigil.
test_cl('qualified hash slice and array slice read',
    '%Pk::Q::H = (a=>1, b=>2, c=>3);
     @Pk::Q::A = (10, 20, 30);
     my @hs = @Pk::Q::H{qw(a c)};
     my @as = @Pk::Q::A[0, 2];
     print "@hs | @as\n";',
    "1 3 | 10 30\n");

test_cl('qualified delete hash slice',
    '%Pk::Q::H = (a=>1, b=>2, c=>3);
     my @d = delete @Pk::Q::H{qw(a b)};
     print "@d | ", join(",", sort keys %Pk::Q::H), "\n";',
    "1 2 | c\n");

# ── CPAN survey bugs (Data::Dump) ─────────────────────────────────────────
# do { } inside an elsif condition used to emit its defun BETWEEN the p-if
# branches, producing a malformed p-if (too many args).  Now inline.
test_cl('do {} block in elsif condition',
    'my $x = 5; my $rval = \$x; my $out;
     if (!defined $$rval)            { $out = "undef"; }
     elsif (do { $$rval + 0 eq $$rval }) { $out = $$rval; }
     else                            { $out = "other"; }
     print "$out\n";',
    "5\n");

# Bare %hash in boolean context: true iff non-empty (was always true).
test_cl('empty %hash is false in boolean context',
    'my %h; print((%h ? "T" : "F"), "\n"); %h=(a=>1); print((%h ? "T":"F"),"\n");',
    "F\nT\n");

# A reference held in a scalar is ALWAYS true, even for an empty referent.
test_cl('empty array/hash refs are true',
    'my $ar = []; my $hr = {}; my @a; my $ar2 = \@a;
     print(($ar ? "T":"F"), ($hr ? "T":"F"), ($ar2 ? "T":"F"), "\n");',
    "TTT\n");

# Deref of an aggregate in boolean context still tests element count.
test_cl('empty deref is false in boolean context',
    'my @a; my $ar = \@a; print((@$ar ? "T" : "F"), "\n");',
    "F\n");

# Bare &foo (no parens) re-uses the caller's @_ (was passing empty list).
test_cl('&foo (no parens) passes current @_',
    'sub inner { return "GOT:$_[0]"; }
     sub outer { my $x = &inner; return $x; }
     print outer("hi"), "\n";',
    "GOT:hi\n");

# local $_ = &quote pattern from Data::Dump str()/quote().
test_cl('&sub via local $_ assignment threads @_',
    'sub q1 { local($_) = $_[0]; return qq("$_"); }
     sub s1 { local $_ = &q1; return $_; }
     print s1("hi"), "\n";',
    "\"hi\"\n");

# Data::Dump end-to-end: string values must survive the str/quote chain.
test_cl('Data::Dump dumps strings and nested structures',
    'use Data::Dump qw(dump);
     print dump({x=>[1,2], y=>"hi"}), "\n";',
    "{ x => [1, 2], y => \"hi\" }\n");

# ── do {} loop-control transparency (Perl semantics) ─────────────────────
# do{} is not a loop; an unlabeled last/next/redo inside it must escape to
# the ENCLOSING loop (do{} body is wrapped in progn, not block nil).
test_cl('last inside do{} breaks the enclosing loop',
    'my @o; for my $i (1..5) { push @o, $i; do { last if $i == 3; }; }
     print "@o\n";',
    "1 2 3\n");

test_cl('next inside do{} continues the enclosing loop',
    'my @o; for my $i (1..5) { do { next if $i == 2; }; push @o, $i; }
     print "@o\n";',
    "1 3 4 5\n");

# return inside do{} exits the enclosing sub (not just the do block).
test_cl('return inside do{} exits the enclosing sub',
    'sub f { my $x = do { return "from-do"; 99 }; return "after:$x"; }
     print f(), "\n";',
    "from-do\n");

# A use-constant value tolerates being called with args (every Perl sub
# accepts @_); a bare &CONST re-uses the caller @_ and must not arity-error.
test_cl('&CONSTANT (no parens) does not arity-error',
    'use constant { K => 7 };
     print "direct:", &K, " eval:", eval("&K"), "\n";',
    "direct:7 eval:7\n");

# ── $obj->$coderef and method-arg flattening ─────────────────────────────
# $obj->$coderef(@args) invokes the coderef directly as $coderef->($obj,@args),
# bypassing method-name lookup (Safe::Isa $_isa/$_can idiom, dispatch tables).
test_cl('method call via coderef invokes it with invocant first',
    'package Foo; sub new { bless {}, shift }
     sub greet { my ($self, $n) = @_; "hi $n from " . ref($self) }
     package main;
     my $o = Foo->new; my $m = \&Foo::greet;
     print $o->$m("bob"), "\n";',
    "hi bob from Foo\n");

# $obj->$name(...) where $name is a string still dispatches by method name.
test_cl('method call via string name dispatches by name',
    'package Foo; sub new { bless {}, shift } sub hi { "HI:$_[1]" }
     package main;
     my $o = Foo->new; my $n = "hi";
     print $o->$n("x"), "\n";',
    "HI:x\n");

# Method arguments flatten like any Perl call: $o->isa(@a) spreads @a.
test_cl('method arguments flatten (array arg spread)',
    'my $o = bless {}, "Foo"; my @a = ("Foo");
     print(($o->isa(@a) ? "Y" : "N"), "\n");',
    "Y\n");

test_cl('user method receives flattened array arguments',
    'package C; sub new { bless {}, shift }
     sub take { my ($s, @rest) = @_; join(",", @rest) }
     package main;
     my $o = C->new; my @a = (1,2,3);
     print $o->take(@a, 4), "\n";',
    "1,2,3,4\n");

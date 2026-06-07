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

plan tests => 102;

# Run transpiled code capturing stdout and stderr SEPARATELY (the normal
# run_cl merges them with 2>&1).  Returns ($stdout, $stderr) with SBCL/PCL
# noise lines filtered from both.
sub run_cl_split {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my ($ef, $err_file) = tempfile(SUFFIX => '.err', UNLINK => 1);
    close $ef;
    my $out = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>$err_file`;
    my $err = do { local $/; open(my $f, '<', $err_file) or return ($out, ''); <$f> };
    for my $s ($out, $err) {
        $s =~ s/^;.*\n//gm;
        $s =~ s/^PCL Runtime loaded\n//gm;
        $s =~ s/^\s*\n//gm;
    }
    return ($out, $err);
}

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

# --- caller() reports the original-case package of the calling frame ---
# (was always "main"; broke modules like Class::Method::Modifiers / Exporter
# that capture scalar(caller) at import time to find the target package).
test_cl('caller() returns the calling frame package, not always main',
    'package Foo; sub who { return scalar caller } package main;
     print Foo::who(), "\n";',
    "main\n");

test_cl('caller() preserves single-segment package case',
    'package Foo; sub who { scalar caller } package Foo::Bar; sub call_who { Foo::who() }
     package main;
     print Foo::Bar::call_who(), "\n";',
    "Foo::Bar\n");

# --- method dispatch on a scalar holding a class-name string ---
# (a *boxed* string invocant used to dispatch against "main" instead of the
# class the string names; only a literal "Foo"->m worked).
test_cl('method call via class-name string in a scalar dispatches to that class',
    'package Foo; sub greet { "hi from Foo" } package main;
     my $cls = "Foo"; print $cls->greet(), "\n";',
    "hi from Foo\n");

test_cl('can() via class-name in a scalar finds the sub',
    'package Foo; sub greet { "x" } package main;
     my $cls = "Foo"; print((defined($cls->can("greet")) ? "Y" : "N"), "\n");',
    "Y\n");

# --- compound conditional-assignment on hash/array element places ---
# (||=, &&=, //= used to box-set the *read* result, which is undef for an
# absent key, so nothing was stored; nested places were not autovivified).
test_cl('||= autovivifies and stores into an absent hash element',
    'my %h; $h{a} ||= 5; print "$h{a}\n";',
    "5\n");

test_cl('||= on a nested hash element autovivifies the intermediate',
    'my %h; my $into = "Foo"; my $name = "greet";
     my $c = $h{$into}{$name} ||= { around => [] };
     push @{ $c->{around} }, "x";
     print ref($c), ":", scalar(@{$h{$into}{$name}{around}}), "\n";',
    "HASH:1\n");

test_cl('//= stores into an absent hash element when undef',
    'my %h; $h{k} //= 7; $h{k} //= 99; print "$h{k}\n";',
    "7\n");

test_cl('&&= updates an existing element only when true',
    'my %h = (a => 1, b => 0); $h{a} &&= 10; $h{b} &&= 20;
     print "$h{a},$h{b}\n";',
    "10,0\n");

test_cl('||= on an absent array element stores the value',
    'my @a; $a[2] ||= "z"; print "$a[2]\n";',
    "z\n");

# --- can()/isa() honour runtime @ISA without crashing on an unfinalized class ---
# (p-can read the CLOS class-precedence-list directly; a class whose @ISA was set
#  at runtime was never finalized -> UNBOUND-SLOT %CLASS-PRECEDENCE-LIST crash.
#  Now both walk @ISA, like the method-call path already does.)
test_cl('can() finds an inherited method via runtime @ISA',
    'package Foo; sub greet { "hi" } package Bar; our @ISA = ("Foo");
     package main; print((Bar->can("greet") ? "Y" : "N"), "\n");',
    "Y\n");

test_cl('isa() honours runtime @ISA',
    'package Foo; sub new { bless {}, shift } package Bar; our @ISA = ("Foo");
     package main; my $o = Bar->new;
     print(($o->isa("Foo") ? "Y" : "N"), ($o->isa("Nope") ? "Y" : "N"), "\n");',
    "YN\n");

test_cl('can()/isa() walk a diamond @ISA hierarchy',
    'package A; sub new { bless {}, shift } sub am { "A::am" }
     package B; our @ISA=("A"); package C; our @ISA=("A"); sub cm { "c" }
     package D; our @ISA=("B","C");
     package main; my $o = D->new;
     print(($o->isa("A")?"Y":"N"), ($o->can("cm")?"Y":"N"),
           ($o->can("nope")?"Y":"N"), " ", $o->am(), "\n");',
    "YYN A::am\n");

# --- bareword constant as an ARRAY subscript is evaluated, not autoquoted ---
# (Perl autoquotes only HASH {bareword}; [bareword] is a numeric expression, so a
#  use-constant index like $self->[P_FOO] must call the constant. Was quoted to
#  "P_FOO" -> non-integer index crash. JSON::PP's array-backed objects need this.)
test_cl('use-constant as array index is evaluated',
    'use constant P_FOO => 7; my @a; $a[P_FOO] = "hi"; print "$a[P_FOO]\n";',
    "hi\n");

test_cl('use-constant as arrayref index is evaluated',
    'use constant P_FOO => 7; my $r = []; $r->[P_FOO] = "yo"; print "$r->[P_FOO]\n";',
    "yo\n");

# Forward reference: the constant is used (in a sub) BEFORE its `use constant`.
# The two-pass parse_file collects the prototype in pass 1, so the subscript in
# pass 2 still knows P_FOO is a constant (not an autoquoted string).
test_cl('forward-referenced constant array index resolves (two-pass)',
    'sub set { my @a; $a[P_FOO] = "hi"; return $a[P_FOO]; }
     use constant P_FOO => 3;
     print set(), "\n";',
    "hi\n");

# An UNKNOWN bareword array index is the string "bar" (no strict subs) -> index 0,
# so the element is defined; it must NOT be evaluated as an undefined sub call.
test_cl('unknown bareword array index is a string (not an undefined sub call)',
    'my @a = (10,20,30); my $x = $a[bar]; print defined($x) ? "ok" : "undef", "\n";',
    "ok\n");

# Hash subscripts still autoquote a bareword (must NOT be treated as a sub call).
test_cl('hash bareword subscript still autoquotes (constant name not called)',
    'use constant FOO => "xyz"; my %h = (FOO => 1); my $r = { FOO => 9 };
     print "$h{FOO},$r->{FOO}\n";',
    "1,9\n");

# overload::import / overload::unimport install/remove for the caller package
# (JSON::PP::Boolean drives these directly).
test_cl('overload::import installs stringify for the caller package',
    'package MyB; use overload ();
     overload::import("overload", q("") => sub { ${$_[0]} ? "T" : "F" });
     sub new { my $v = $_[1]; bless \$v, $_[0] }
     package main; my $t = MyB->new(1); print "$t\n";',
    "T\n");

# --- per-iteration closure capture in map/grep/sort blocks ---
# A `my` declared in the block and captured by a nested anon sub must be a fresh
# lexical per element (the block is a lambda called once per element), not one
# shared file-level global. Was "ccc"; Perl is "abc".
test_cl('map { my $x=$_; sub {$x} } captures per-iteration',
    'my @s = map { my $x = $_; sub { $x } } qw(a b c);
     print $s[0]->(), $s[1]->(), $s[2]->(), "\n";',
    "abc\n");

# The captured var used only via string interpolation must also be detected
# (it is not a PPI::Token::Symbol; it lives inside the quote token).
test_cl('map closure captures a var used only in string interpolation',
    'my @s = map { my $x = $_; sub { "v=$x" } } qw(a b);
     print $s[0]->(), $s[1]->(), "\n";',
    "v=av=b\n");

test_cl('map closure captures multiple per-iteration vars',
    'my @s = map { my $k=$_; my $v=$_*10; sub { "$k:$v" } } (1,2);
     print $s[0]->(), " ", $s[1]->(), "\n";',
    "1:10 2:20\n");

# A plain map block (no closure) is unaffected, and sort still sees $a/$b.
test_cl('plain map/sort blocks unaffected by closure-capture handling',
    'my @a = map { $_ + 1 } (1,2,3);
     my @b = sort { $b <=> $a } (3,1,2);
     print join(",", @a), " ", join(",", @b), "\n";',
    "2,3,4 3,2,1\n");

# --- builtin:: namespace (core pragma, Perl 5.36+) ---
# Always available without `use` (a generated builtin::NAME(...) call must
# resolve). PCL provides the flag-free functions faithfully.
test_cl('builtin::true/false/ceil/floor/trim',
    'no warnings;
     printf "%s %s %d %d [%s]\n",
       (builtin::true() ? "T":"F"), (builtin::false() ? "T":"F"),
       builtin::ceil(2.1), builtin::floor(2.9), builtin::trim("  hi  ");',
    "T F 3 2 [hi]\n");

test_cl('builtin::blessed/reftype on a blessed ref vs a plain string',
    'no warnings; my $o = bless {}, "Foo";
     print builtin::blessed($o)//"u", " ", builtin::reftype($o)//"u", " ",
           builtin::blessed("x")//"u", "\n";',
    "Foo HASH u\n");

# created_as_number/created_as_string use the box value type as a proxy for the
# SV IOK/NOK/POK flags PCL cannot see.
test_cl('builtin::created_as_number/created_as_string proxy via value type',
    'no warnings;
     print((builtin::created_as_number(42) ? "N":"-"),
           (builtin::created_as_string("hi") ? "S":"-"),
           (builtin::created_as_number("hi") ? "N":"-"), "\n");',
    "NS-\n");

# --- ${ EXPR } block dereference in string interpolation (Sub::Quote) ---
# ${$ref} derefs as a scalar; ${\ EXPR} is the "interpolate an expression"
# idiom; ${N} is the numbered capture var $N. Were emitting SCALAR(0x..)/
# REF(0x..)/literal N respectively.
test_cl('${$ref} scalar-deref in interpolation',
    'my $r = \"hello"; print "v=${$r}\n";',
    "v=hello\n");

test_cl('${\ EXPR} expression-interpolation idiom',
    'my @a = (10,20); print "j=${\ join(q{,},@a)} s=${\ (1+2)}\n";',
    "j=10,20 s=3\n");

test_cl('${N} numbered capture var in interpolation',
    'my $s = "abc"; $s =~ /(b)(c)/; print "c=${1}${2}\n";',
    "c=bc\n");

# --- print STDERR routes to *error-output*, not stdout ---
# Generated code runs in a user package whose STDERR symbol is NOT the :pcl
# STDERR registered in *p-filehandles* (an `eq` miss).  p-get-filehandle-stream
# now falls back to a by-name lookup so `print STDERR ...` reaches *error-output*
# rather than silently going to stdout (which broke Carp/warn diagnostics for
# CPAN modules).  Without reopening STDERR, the only observable difference is
# which stream the bytes land on — so capture them separately.
{
    my ($out, $err) = run_cl_split(
        'print STDOUT "OUTMARK"; print STDERR "ERRMARK";');
    like($out, qr/OUTMARK/, 'print STDOUT reaches stdout');
    like($err, qr/ERRMARK/,
        'print STDERR reaches stderr (foreign-package STDERR symbol)');
}

# --- our $var inside a sub in a multi-segment package ---
# `_process_our_declaration` emitted the inner defvar with a raw package prefix
# (Foo::Bar::$thing), which the CL reader rejects as too many colons.  Must
# pipe-quote multi-segment names (|Foo::Bar|::$thing).  Hit via Moo::sification.
test_cl('our $var inside a sub in a multi-segment package',
    'package Foo::Bar; sub setit { our $thing = 42; return $thing; }'
    . ' package main; print Foo::Bar::setit(), "\n";',
    "42\n");

# --- glob-ref install/inspect: *{\*glob} = code and *{\*glob}{CODE} ---
# Moo's _install_coderef does _getglob = \*{$name}, then `if (*{$glob}{CODE})`
# and `*{$glob} = $code`.  p-glob-assign-dynamic / p-dynamic-typeglob
# stringified the glob ref (→ GLOB(0x..)) instead of recognizing the
# p-typeglob it wraps, so the install was lost and the slot read returned
# nothing.  Also *{EXPR}{SLOT} (dynamic glob-slot) used to be a parse error.
test_cl('*{$globref} = sub installs a callable sub',
    'no strict "refs"; my $gr = \*{"main::zap"};'
    . ' *{$gr} = sub { return "zapped" }; print zap(), "\n";',
    "zapped\n");

test_cl('*{$globref}{CODE} reflects an installed sub',
    'no strict "refs"; *{"main::has_code"} = sub { 1 };'
    . ' my $gr = \*{"main::has_code"};'
    . ' print( (*{$gr}{CODE} ? "yes" : "no"), "\n");',
    "yes\n");

test_cl('*{$globref}{CODE} is false for an empty glob',
    'no strict "refs"; my $gr = \*{"main::no_code"};'
    . ' print( (*{$gr}{CODE} ? "yes" : "no"), "\n");',
    "no\n");

# --- scalar ref placed directly in a %hash = (...) literal/copy ---
# `%h = (k => \$x)` and `%c = %$href` lost the scalar-ref-ness: %p-make-hash-entry
# unboxed the ref one level and p-gethash then stripped it to a plain scalar, so
# ref()='' and ${$h{k}} read empty. Sub::Quote builds captures as {'$x'=>\$v} and
# copies them via `my %captures = %$captures`, so this blocked Sub::Quote entirely.
test_cl('scalar ref as direct value in %h = (k => \\$x) literal',
    'my $v = "AAA"; my %h = (k => \$v);'
    . ' print ref($h{k}), ":", ${$h{k}}, "\n";',
    "SCALAR:AAA\n");

test_cl('scalar ref survives %copy = %$href',
    'my $v = "BBB"; my $href = { s => \$v }; my %c = %$href;'
    . ' print ref($c{s}), ":", ${$c{s}}, "\n";',
    "SCALAR:BBB\n");

# writing through the copied ref reaches the original scalar
test_cl('writeback through a hash-copied scalar ref reaches the original',
    'my $v = "old"; my %h = (r => \$v); ${$h{r}} = "new"; print $v, "\n";',
    "new\n");

# --- return of a scalar ref must not strip it ---
# p-return-value preserved hash/array/code refs (inner is hash/vector/function)
# but a scalar ref's inner is a p-box, so `return \$x` (and `return $_[0]` when
# $_[0] is a directly-passed \$x) unboxed it to a plain scalar. Keyed the fix on
# the is-ref flag. Needed by Sub::Quote's unquote_sub.
test_cl('return \\$x preserves the scalar ref',
    'my $x = "V"; sub r { return \$x } my $s = r();'
    . ' print ref($s), ":", ${$s}, "\n";',
    "SCALAR:V\n");

test_cl('return $_[0] preserves a directly-passed scalar ref',
    'my $x = "V"; sub a { return $_[0] } my $s = a(\$x);'
    . ' print ref($s), ":", ${$s}, "\n";',
    "SCALAR:V\n");

# --- top-level my ($x) = LIST is a LIST assignment (first element) ---
# my ($x) is parenthesized → list context → $x gets the FIRST element. The
# top-level my handler dropped the parens and emitted a scalar box-set, so
# my ($x)=(a,b) gave the last element and my ($x)=@a gave the element count.
test_cl('my ($x) = (a, b) takes the first element',
    'my ($x) = ("A", "B"); print $x, "\n";',
    "A\n");

test_cl('my ($x) = @array takes the first element (not the count)',
    'my @a = ("P","Q","R"); my ($x) = @a; print $x, "\n";',
    "P\n");

# --- dereference slices (prefix and postfix) ---
# @$ar[i,j] (array-ref slice, square brackets) wrongly routed to p-hslice (hash
# slice) → p-gethash on a vector → crash; the bracket type must pick the slice
# kind. And the postfix forms $ref->@[...], ->@{...}, ->%{...}, ->%[...] were
# parse errors. keys/values $ref->%* parsed as (keys $ref)->%*.
test_cl('prefix array-ref slice @$ar[0,2]',
    'my $ar=[10,20,30]; my @s=@$ar[0,2]; print "@s\n";',
    "10 30\n");

test_cl('postfix array-ref slice $ar->@[0,2]',
    'my $ar=[10,20,30]; my @s=$ar->@[0,2]; print "@s\n";',
    "10 30\n");

test_cl('postfix hash-ref slice $hr->@{...}',
    'my $hr={a=>1,b=>2,c=>3}; my @s=$hr->@{qw(a c)}; print "@s\n";',
    "1 3\n");

test_cl('postfix kv hash-ref slice $hr->%{...}',
    'my $hr={a=>1,c=>3}; my @s=$hr->%{qw(a c)}; print "@s\n";',
    "a 1 c 3\n");

test_cl('keys on a postfix hash deref: keys $hr->%*',
    'my $hr={a=>1,b=>2,c=>3}; print join(",",sort keys $hr->%*), "\n";',
    "a,b,c\n");

# nested exists on a missing intermediate must not crash (returns false)
test_cl('exists $h{a}{b} on empty hash does not crash',
    'my %h; print((exists $h{a}{b}) ? "T":"F", "\n");',
    "F\n");

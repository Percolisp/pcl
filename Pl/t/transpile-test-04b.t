#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Transpile tests part 4b: regex advanced, captures, state, builtins

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

# Path to pl2cl and runtime
my $project_root = "$RealBin/../..";
my $pl2cl = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
# Optional saved-core fast path (PCL_TEST_CORE=1); source-load otherwise.
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

# Check dependencies
plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

# Run a Perl snippet and return output
sub run_perl {
    my ($code) = @_;
    # Add common 'use' statements for features we support
    my $full_code = 'use feature "state"; use Cwd; ' . $code;
    my $output = `perl -e '$full_code' 2>&1`;
    return $output;
}

# Transpile and run CL, return output
sub run_cl {
    my ($code) = @_;

    # Write Perl code to temp file
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    # Transpile
    my $cl_code = PCLCore::transpile(qq{$pl2cl $pl_file});

    # Write CL to temp file
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    # Run with sbcl (saved core if PCL_TEST_CORE is set, else --load the runtime)
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;

    # Filter out warnings and "PCL Runtime loaded"
    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;

    return $output;
}

# Test helper: compare Perl and CL output
sub test_transpile {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL: $cl_out");
}

# ============ REGEX: ALTERNATION ============

test_transpile("regex: alternation first", '
my $str = "cat";
if ($str =~ /cat|dog/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: alternation second", '
my $str = "dog";
if ($str =~ /cat|dog/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: alternation none", '
my $str = "bird";
if ($str =~ /cat|dog/) { print "yes\n"; } else { print "no\n"; }
');

# ============ REGEX: QUANTIFIERS ============

test_transpile("regex: exact count", '
my $str = "aaa";
if ($str =~ /a{3}/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: range count", '
my $str = "aaaa";
if ($str =~ /a{2,4}/) { print "yes\n"; } else { print "no\n"; }
');

# ============ REGEX: MODIFIERS ============

test_transpile("regex: single-line mode (dot matches newline)", '
my $str = "a\nb";
if ($str =~ /a.b/s) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: multi-line mode (caret matches after newline)", '
my $str = "hello\nworld";
if ($str =~ /^world/m) { print "yes\n"; } else { print "no\n"; }
');

# ============ REGEX: SUBSTITUTION PATTERNS ============

test_transpile("regex: subst with digit pattern", '
my $str = "abc123def";
$str =~ s/\d+/XXX/;
print $str, "\n";
');

test_transpile("regex: subst global with pattern", '
my $str = "a1b2c3";
$str =~ s/\d/X/g;
print $str, "\n";
');

test_transpile("regex: subst anchored", '
my $str = "hello world";
$str =~ s/^hello/hi/;
print $str, "\n";
');

test_transpile("regex: subst with character class", '
my $str = "hello";
$str =~ s/[aeiou]/X/g;
print $str, "\n";
');

# ============ TR: MODIFIERS ============

test_transpile("regex: tr delete modifier", '
my $str = "hello123world";
$str =~ tr/0-9//d;
print $str, "\n";
');

test_transpile("regex: tr squash modifier", '
my $str = "heeellooo";
$str =~ tr/a-z//s;
print $str, "\n";
');

test_transpile("regex: tr complement modifier", '
my $str = "hello123";
$str =~ tr/a-z//cd;
print $str, "\n";
');

test_transpile("regex: tr count characters", '
my $str = "hello world";
my $count = ($str =~ tr/l//);
print $count, "\n";
');

# ============ REGEX: WORD BOUNDARIES ============

test_transpile("regex: word boundary match", '
my $str = "hello world";
if ($str =~ /\bworld\b/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: word boundary no match", '
my $str = "helloworld";
if ($str =~ /\bworld\b/) { print "yes\n"; } else { print "no\n"; }
');

# ============ REGEX: ESCAPE SEQUENCES ============

test_transpile("regex: literal dot", '
my $str = "file.txt";
if ($str =~ /\.txt$/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: literal backslash", '
my $str = "path\\\\file";
if ($str =~ /\\\\/) { print "yes\n"; } else { print "no\n"; }
');

# ============ CAPTURE GROUPS ============

test_transpile("regex: capture group simple", '
my $str = "hello world";
if ($str =~ /(\\w+) (\\w+)/) {
    print $1, "-", $2, "\n";
}
');

test_transpile("regex: capture group three groups", '
my $str = "a-b-c";
if ($str =~ /(\\w)-(\\w)-(\\w)/) {
    print $1, $2, $3, "\n";
}
');

test_transpile("regex: capture group clears on fail", '
my $str = "hello world";
$str =~ /(\\w+) (\\w+)/;
my $saved1 = $1;
$str =~ /xyz/;
if (defined($1)) { print "defined\n"; } else { print "undef\n"; }
');

test_transpile("regex: capture group in subst", '
my $str = "hello world";
$str =~ s/(\\w+) (\\w+)/$2 $1/;
print $str, "\n";
');

test_transpile("regex: capture with mixed text in replacement", '
my $str = "fooABC bar123";
$str =~ s/foo([A-Z]+) bar([\\d]+)/DUH$2 Dah$1/;
print $str, "\n";
');

test_transpile("regex: capture global with backrefs", '
my $str = "cat1 dog2 bird3";
$str =~ s/(\\w+)(\\d)/[$1:$2]/g;
print $str, "\n";
');

test_transpile("regex: capture case insensitive", '
my $str = "HELLO world";
$str =~ s/(hello) (world)/[$1] [$2]/i;
print $str, "\n";
');

test_transpile("regex: capture with digits", '
my $str = "order-12345-item";
if ($str =~ /order-(\\d+)-item/) {
    print "id:", $1, "\\n";
}
');

test_transpile("regex: multiple captures sequential", '
my $str = "2024-01-15";
if ($str =~ /(\\d{4})-(\\d{2})-(\\d{2})/) {
    print $1, "/", $2, "/", $3, "\\n";
}
');

test_transpile("regex: capture repeated in replacement", '
my $str = "abc";
$str =~ s/(\\w+)/$1$1$1/;
print $str, "\n";
');

test_transpile("regex: capture with anchors", '
my $str = "hello world";
if ($str =~ /^(\\w+)/) {
    print "first:", $1, "\\n";
}
');

test_transpile("regex: capture at end", '
my $str = "hello world";
if ($str =~ /(\\w+)$/) {
    print "last:", $1, "\\n";
}
');

test_transpile("regex: nested parens capture", '
my $str = "abc123def";
if ($str =~ /([a-z]+)(\\d+)([a-z]+)/) {
    print $1, "-", $2, "-", $3, "\\n";
}
');

test_transpile("regex: empty capture on no match", '
my $str = "hello";
if ($str =~ /(\\d+)/) {
    print "found:", $1, "\\n";
} else {
    print "none\\n";
}
');

test_transpile("regex: capture preserves across success", '
my $str = "abc123";
$str =~ /(\\w+)/;
my $first = $1;
$str =~ /(\\d+)/;
print $first, "-", $1, "\\n";
');

test_transpile("regex: subst global multiple captures", '
my $str = "a1b2c3";
$str =~ s/([a-z])(\\d)/$2$1/g;
print $str, "\n";
');

# ============ SPECIAL VARIABLES ============

test_transpile("special: \$\$ is numeric", '
if ($$ > 0) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("special: \$\$ in string interpolation", '
my $msg = "pid:$$";
if ($msg =~ /^pid:\\d+$/) { print "ok\n"; } else { print "fail\n"; }
');

test_transpile("special: \$\$ assigned to variable", '
my $pid = $$;
if ($pid > 0) { print "ok\n"; } else { print "fail\n"; }
');

# ============ STATE VARIABLES ============

test_transpile("state: counter increments", '
sub counter {
    state $count = 0;
    $count++;
    return $count;
}
print counter(), "-", counter(), "-", counter(), "\n";
');

test_transpile("state: multiple state vars", '
sub pair {
    state $a = 10;
    state $b = 20;
    $a++;
    $b += 2;
    return $a + $b;
}
print pair(), "-", pair(), "-", pair(), "\n";
');

test_transpile("state: preserves across calls", '
sub accum {
    state $total = 0;
    my $n = shift;
    $total += $n;
    return $total;
}
print accum(5), "-", accum(3), "-", accum(2), "\n";
');

test_transpile("state: string initialization", '
sub greeter {
    state $prefix = "Hello";
    my $name = shift;
    return $prefix . " " . $name;
}
print greeter("World"), "-", greeter("Perl"), "\n";
');

# ============ BUILT-IN FUNCTIONS ============

test_transpile("builtin: getcwd returns string", '
my $cwd = getcwd();
if (length($cwd) > 0) { print "ok\n"; } else { print "fail\n"; }
');

test_transpile("builtin: time returns number", '
my $t = time();
if ($t > 1700000000) { print "ok\n"; } else { print "fail\n"; }
');

test_transpile("builtin: localtime array access", '
my @lt = localtime();
my $year = $lt[5] + 1900;
if ($year >= 2024 && $year <= 2030) { print "ok\n"; } else { print "fail\n"; }
');

test_transpile("builtin: localtime hour/min/sec", '
my @lt = localtime();
my $hour = $lt[2];
if ($hour >= 0 && $hour <= 23) { print "ok\n"; } else { print "fail\n"; }
');

test_transpile("builtin: mkdir and rmdir", '
my $dir = "/tmp/pcl_test_dir_$$";
if (mkdir($dir)) {
    if (rmdir($dir)) { print "ok\n"; } else { print "rmdir fail\n"; }
} else { print "mkdir fail\n"; }
');

test_transpile("builtin: chdir and getcwd", '
my $orig = getcwd();
chdir("/tmp");
my $tmp = getcwd();
chdir($orig);
if ($tmp eq "/tmp") { print "ok\n"; } else { print "fail\n"; }
');

test_transpile("builtin: ENV read HOME", '
my $home = $ENV{HOME};
if (length($home) > 0) { print "ok\n"; } else { print "fail\n"; }
');

test_transpile("builtin: ENV write and read", '
$ENV{PCL_TEST_VAR} = "test_value";
if ($ENV{PCL_TEST_VAR} eq "test_value") { print "ok\n"; } else { print "fail\n"; }
');

test_transpile("builtin: ENV in string", '
my $user = $ENV{USER};
my $msg = "user:$user";
if ($msg =~ /^user:/) { print "ok\n"; } else { print "fail\n"; }
');

# ============ OO: INHERITANCE (runtime execution) ============

test_transpile("OO: basic method inheritance", '
package Animal;
sub new { bless { name => $_[1] }, $_[0] }
sub speak { "I am " . $_[0]->{name} }

package Dog;
our @ISA = ("Animal");
sub speak { $_[0]->SUPER::speak() . " and I bark" }

package main;
my $d = Dog->new("Rex");
print $d->speak(), "\n";
');

test_transpile("OO: inherited method called directly", '
package Base;
sub new { bless {}, $_[0] }
sub greet { "hello from base" }

package Child;
our @ISA = ("Base");

package main;
my $c = Child->new();
print $c->greet(), "\n";
');

test_transpile("OO: SUPER:: call", '
package A;
sub new { bless {}, $_[0] }
sub name { "A" }

package B;
our @ISA = ("A");
sub name { "B+" . $_[0]->SUPER::name() }

package main;
my $b = B->new();
print $b->name(), "\n";
');

test_transpile("OO: multiple inheritance", '
package X;
sub new { bless {}, $_[0] }
sub hello { "X" }

package Y;
sub world { "Y" }

package Z;
our @ISA = ("X", "Y");

package main;
my $z = Z->new();
print $z->hello(), $z->world(), "\n";
');

# T-A1 regression (s278b): a top-level `{ package X; … }` helper block is
# flattened into the segment stream.  An embedded `my` there (`tie my $t`) is
# forward-defvar'd as X::$t; the forward-decl exclusion must be PACKAGE-AWARE,
# so a `let`-bound `$t` in a DIFFERENT package block does not suppress it (the
# join.t 42–43 unbound-$t miscompile).
test_transpile("T-A1: embedded-my in flattened package block, name reused as let elsewhere", '
{ package X;
  sub TIESCALAR { my $x = 7; bless \\$x }
  sub FETCH { my $y = shift; $$y += 5 }
  tie my $t, "X";
  my $r = join ":", $t, 99, $t, 99;
  print "$r\n";
}
{ package main;
  my $t = "own-lexical";
  print "$t\n";
}
');

# W10 span-detection is a PPI decision (s278b): these go v2-NATIVE and must
# match perl.  Each targets a soundness boundary of _canon_refs_in.
test_transpile("W10: genuine my-lexical span across a package boundary", '
my $x = 10;
{ package Foo; sub g { $x + 1 } }
print Foo::g(), "\n";
');
test_transpile("W10-ext-4: string eval that does NOT name the spanning var is safe", '
my $x = 7;
{ package Q; eval q(my $z = 1;); sub g { $x } }
print Q::g(), "\n";
');
test_transpile("W10 sigil-distinct: my %h (main) vs interpolated \$h (block) are different vars", '
my %h = (a => 1);
print "$h{a}\n";
{ package Bar; my $h = "S"; print "$h\n"; }
');

# D1/E1.5 (session 281): nested `package` statements — v1-shape transplant in
# _lower_block.  Statement form scopes to the enclosing block remainder;
# block form to its own block; subs after the switch install qualified.
test_transpile("nested package: statement form in sub scopes to sub end, block restores", '
sub f { package X; return __PACKAGE__; }
sub g { { package Y; } return __PACKAGE__; }
print f(), "-", g(), "\n";
');
test_transpile("nested package: sub defined after nested package installs qualified (bless.t F shape)", '
{ { package F; sub hello { return "F-hello" } } }
print F::hello(), "\n";
');
test_transpile("nested package: block form inside a sub body (index.t shape)", '
sub f { package P2 { sub m2 { return "m2v" } } return P2::m2(); }
print f(), "\n";
');
test_transpile("nested package in BEGIN gets a p-defpackage pre-declaration (Moo idiom)", '
BEGIN { package Foo::_Private; sub hidden { return 42 } }
print Foo::_Private::hidden(), "\n";
');

# delete-local inside a my-init opens a restore scope (local.t shape); the
# whole statement routes through the local seam (_is_local_stmt).
test_transpile("my = delete local restores the deleted elements at block end", '
our @a = ("a", "b", "c");
$a[4] = "d";
{
    my $c = delete local $a[2];
    my ($d, $zzz) = delete local @a[4, 999];
    print "in=$c$d\n";
}
print "out=$a[2]$a[4]\n";
');

# $^S eval-state variable (was unbound: missing from %SPECIAL_VARS + runtime).
test_transpile("\$^S is 0 at runtime and 1 inside eval", '
print $^S, ";", eval { $^S }, "\n";
');

# `our` inside a nested-package region declares (and uses) the SWITCHED
# package cell — v1 mechanism: add_our_variable/is_our_variable qualification.
test_transpile("our inside nested package region binds the switched package cell", '
sub f { package X; our $v; $v = 5; return $X::v // "X-undef"; }
print f(), "\n";
');

# Task #49 (s282): `package NAME;` inside an EXPRESSION block (do/eval/anon
# sub/map-grep-sort) is block-scoped.  parse_block_as_function leaked the
# Environment push (pre-passes parse the block 3x, so calls BEFORE the
# statement were qualified too); parse_block_to_cl_string dropped the whole
# body (the top-level package path opens a new section the string collector
# never reads).  Runtime *package*/*pcl-current-package* now revert via a
# let binding that passes the tail value through.
test_transpile("do-block package is block-scoped: calls before/after stay unqualified", '
sub f5 { return 7 }
print f5(), "-";
my $r = do { package X8; 1 };
print f5(), "-", $r, "\n";
');
test_transpile("eval-block with package statement keeps its body (was (p-eval-block nil))", '
my $v = eval { package XV; bless {} };
print ref($v), "\n";
');
test_transpile("runtime current package reverts after do/eval blocks", '
sub who { return (caller())[0]; }
my $c = do { package XC; main::who() };
my $e = eval { package XE; eval q{__PACKAGE__} };
print $c, "-", $e, "-", main::who(), "-", eval(q{__PACKAGE__}), "\n";
');
test_transpile("named sub after package inside do-block installs qualified", '
my $o = do { package XD; sub mk { bless {}, "XD" } XD::mk(); };
print ref($o), "\n";
');
test_transpile("anon sub with package statement: switch scoped to sub body", '
my $s = sub { package XS; __PACKAGE__ };
print $s->(), "-", eval(q{__PACKAGE__}), "\n";
');
test_transpile("map/grep blocks with package statement keep body and revert package", '
my @r = map { package XM; $_ * 2 } (1, 2, 3);
my @g = grep { package XG; $_ > 2 } @r;
print "@r|@g|", eval(q{__PACKAGE__}), "\n";
');
test_transpile("last unwinds out of a do-block that switched package, binding reverts", '
for my $i (1..3) {
    my $x = do { package XL; last if $i == 2; $i };
    print "x=$x;";
}
print eval(q{__PACKAGE__}), "\n";
');

# s295b (#63/t183): `our` declared under an in-block `package` stays aliased
# to the DECLARING package for the rest of the block — a later in-block
# `package` switch must not re-home the bare name (requalify pre-pass).
# Also guards `$#a++` extending with real HOLES (exists false), not boxes.
test_transpile("our-alias survives an in-block package switch", '
{
    package tmp2; our @qa; $#qa++; $qa[1] = 5; package main;
    my @b = @qa;
    print 0+@b, ";", (exists $qa[0] ? "viv" : "hole"), ";", $qa[1], "\n";
}
print 0+@tmp2::qa, "\n";
');

# M7 (s355): an our-alias runs to the end of the block OR to the next
# declaration of the same name — so a re-declaration ENDS the alias instead of
# defeating it.  This is the Role-Tiny subclass.t shape: one bare block
# declaring `our @ISA` in each of four successive packages, which used to route
# the whole file to v1.
test_transpile("our-alias per package: successive our \@ISA in one block", '
{
    package M7::Top; sub m0 { __PACKAGE__ }
    package M7::Left;  our @ISA = qw(M7::Top);
    package M7::Right; our @ISA = qw(M7::Top);
    package M7::Bottom; our @ISA = qw(M7::Left M7::Right);
}
print "L=@M7::Left::ISA;R=@M7::Right::ISA;B=@M7::Bottom::ISA\n";
print "m=", M7::Bottom->m0, "\n";
');
# INVERSE GUARDS, one snippet because each row costs an SBCL run:
#  - uses BEFORE the re-declaration must still requalify to the DECLARING
#    package (`pre:1 2`, not main::v) — truncation must not start early;
#  - `foreach my $v` binds the SCALAR and must NOT end the `@v` alias
#    (`loop:1 2`) — the re-declaration test is sigil-exact;
#  - after `our @v` the alias is the NEW package's (`post:9` = main::v, with
#    M7a::v still 1 2), and after a top-level `my @w` it is the lexical
#    (`my:5`, M7b::w still 3 4).
# Still REFUSED (correct — the alias resumes after it, which a truncation
# cannot express): a re-declaration nested in an inner block or sub.
test_transpile("our-alias re-declaration boundaries: sigil-exact, before/after", '
{ package M7a; our @v = (1,2);
  package main;
  print "pre:@v;";
  foreach my $v (7,8) { }
  print "loop:@v;";
  our @v = (9);
  print "post:@v;";
}
{ package M7b; our @w = (3,4);
  package main;
  print "pre:@w;";
  my @w = (5);
  print "my:@w;";
}
print "\nM7a=@M7a::v main=@main::v M7b=@M7b::w\n";
');

# s299/#45: interpolated postfix deref ("$r->@*" etc.) is gated on the
# lexical postderef_qq feature; without it the ->@* stays literal text.
test_transpile(q{interp postderef: ->@* / ->@[slice] / ->@{kslice} / ->$#* with feature on}, '
use feature "postderef_qq";
my $r = [7,8,9];
my $h = {foo=>"oof"};
print "$r->@*|$r->@[0,1]|$h->@{q(foo)}|$r->$#*\n";
');
test_transpile(q{interp postderef: scalar deref ->$* with feature on}, '
use feature "postderef_qq";
my $x = 43;
my $s = \$x;
print "$s->$*\n";
');
test_transpile("interp postderef stays literal without the feature", '
my $r = "V";
print "$r->@*\n";
');
test_transpile("interp postderef feature is block-scoped: off again after scope exit", '
my $r = [7,8,9];
my $p = "V";
{
    use feature "postderef_qq";
    print "$r->@*;";
}
print "$p->@*\n";
');

# s299/#45: implicit arrow after a call — $cr->(){k} / $cr->()[i] chain like
# ->()->{k} / ->()->[i]; PPI tags the braces Block/Constructor, not Subscript.
test_transpile("implicit arrow subscript after call: ->(){k} and ->()[i]", '
my $cr = sub { {k=>42} };
my $ca = sub { [5,6,7] };
print $cr->(){k}, ";", $ca->()[1], "\n";
');
# ... and a lone bareword in such a brace autoquotes even when it names a sub.
test_transpile("bareword key autoquotes in ->(){key} even when a sub of that name exists", '
sub ppp { "qqq" }
my $cr = sub { {ppp=>31} };
print $cr->(){ppp}, "\n";
');

# s299/#45 (child_context): an interpolated slice passed as a scalar-imposing
# funcall arg must still join ALL elements, not collapse to the last one.
test_transpile("interp postderef slice as scalar-prototype funcall arg keeps all elements", '
use feature "postderef_qq";
sub takeit ($) { return $_[0] }
my $r = [7,8,9];
print takeit("$r->@[0,1]"), "\n";
');

# s300/#55: BEGIN blocks interleave with sub defs at SOURCE POSITION — a
# BEGIN sees exactly the subs defined above it and none below (perl compiles
# in source order; sub-existence introspection must not see later subs).
test_transpile("BEGIN sub-existence introspection sees earlier subs only", '
sub early { "e" }
BEGIN {
    print defined &early ? "early-yes;" : "early-no;";
    print defined &late  ? "late-yes;"  : "late-no;";
}
sub late { "l" }
print "run\n";
');
test_transpile("BEGIN calls a sub defined before it", '
sub greet { "hi" }
BEGIN { print greet(), ";"; }
print "run\n";
');

# s300c (signatures gate): a BLOCK-NESTED signatured sub used to lose its
# whole signature in v2 (native _lower_sub ignored it — params fell through
# to file globals, defaults never ran); it now routes through the v1 seam
# like top-level signatured subs.
test_transpile("block-nested signatured sub keeps its signature and defaults", '
use feature q(signatures); no warnings;
$a = 123;
{
    sub t9 ($a = 222, $b = 7) { "$a/$b" }
}
print t9(), ";", t9(1), "\n";
');
# ... and a named sub nested INSIDE the signatured sub hoists so the
# signature default can call it before the outer sub ever runs (the former
# whole-file "named sub nested in a prototyped/signatured sub" gate).
test_transpile("named sub nested in signatured sub is callable from the default", '
use feature q(signatures); no warnings;
my $file_lex = "leak-me";
{
    sub t146 ($a = t146x()) {
        sub t146x { $a = "abc"; 1 }
        $a;
    }
    print t146(), "\n";
}
');

# s299/#45: a bare-block my whose name is also a package global elsewhere is
# renamed (shadow) so the block sees the lexical and the file keeps the global.
test_transpile("bare-block my shadowing a package global leaves the global intact", '
@a = (1,2,3);
{
    my ($s, @a) = ("x", 7, 8);
    print "@a;";
}
print "@a\n";
');

# s300d/#70: fork-pipe opens.  Command pipes (2-arg spellings), the bare
# "-|" / "|-" fork forms with an in-process child, close-reaps-child ($?),
# and the closure.t child shape (pipe + bare |- + ">&" dup-open + exec).
test_transpile("command read-pipe: open FH, \"cmd |\"", '
open(FH, "echo hello |") or die "nope";
while (<FH>) { print "got: $_"; }
my $ok = close FH;
print "close: ", ($ok ? "true" : "false"), " status: $?\n";
');
test_transpile("command write-pipe: open FH, \"| cmd\"", '
open(FH, "| tr a-z A-Z") or die "nope";
print FH "shout\n";
close FH;
print "done\n";
');
test_transpile("bare -| fork-pipe: parent reads in-process child, close sets \$?", '
my $pid = open CHILD, "-|";
die "fork failed" unless defined $pid;
if ($pid) {
    while (<CHILD>) { print "parent-got: $_"; }
    close CHILD;
    print "reaped status $?\n";
} else {
    print "hello from child\n";
    exit 0;
}
');
test_transpile("bare |- fork-pipe: child reads rewired STDIN, exit status through close", '
my $pid = open KID, "|-";
die "fork failed" unless defined $pid;
if ($pid) {
    print KID "one\n";
    print KID "two\n";
    close KID;
    print "parent done, status $?\n";
} else {
    while (<STDIN>) { chomp; print "child-saw: [$_]\n"; }
    exit 3;
}
');
test_transpile("closure.t child shape: pipe + bare |- + >&dup + exec, parent captures", '
pipe READ, WRITE or die "no pipe";
my $pid = open PERL, "|-";
die "no fork" unless defined $pid;
unless ($pid) {
    close READ;
    open STDOUT, ">&WRITE" or die "no redirect: $!";
    exec "cat", "-" or die "no exec";
} else {
    close WRITE;
    print PERL "through the child\n";
    close PERL;
    local $/;
    my $out = <READ>;
    close READ;
    print "captured: [$out]";
}
');

# s301/#70: ">&=" fdopen-style dup — same underlying handle, works on an
# in-memory (fd-less) filehandle too (scalar.t [perl #113764] shape).
test_transpile(">&= dup onto an in-memory handle", '
open FILE, ">", \my $content or die "no open";
open my $fh, ">&=FILE" or die "no dup: $!";
print $fh "Foo-Bar\n";
close $fh;
close FILE;
print "content=<$content>";
');

# s301/#70: an interpolated heredoc whose EVERY sigil is escaped still gets
# its escapes collapsed (\$ -> $, \\ -> \) — closure.t END_MARK_ONE/TWO shape.
# Covers both the my-decl RHS (v2-native) and .= (seam) parse paths.
test_transpile("escape-only interpolated heredoc collapses \\\$ and \\\\", '
my $c = <<"END_MARK";
BEGIN { \$SIG{__WARN__} = sub {
    my \$msg = \$_[0];
END_MARK
$c .= <<"END_TWO";
tail \$x and a literal \\\\n here
END_TWO
print $c;
');

# ============ CALLING CONVENTION: AGGREGATE ARGS TO SIGNATURE SUBS ============
# s304 task #80: the v2 signature fast path (p-raw-params, formerly a bare
# &optional lambda list) must honour Perl argument flattening — f(@args) and
# the f(@_) delegation idiom pass the CONTAINER raw and the callee spreads it.
# The &optional form bound the whole vector to the first param (broke Moo via
# Moo::_Utils::_name_coderef -> Sub::Util::set_subname(@_)).

test_transpile("signature sub called with an array flattens", '
sub f { my ($x, $y) = @_; return "$x/$y"; }
my @args = (1, 2);
print f(@args), "\n";
');

test_transpile("signature sub via @_ delegation flattens", '
sub f { my ($x, $y) = @_; return "$x/$y"; }
sub g { return f(@_); }
print g(3, 4), "\n";
');

test_transpile("signature sub with hash arg flattens to kv pairs", '
sub f { my ($k, $v) = @_; return "$k=$v"; }
my %h = (a => 5);
print f(%h), "\n";
');

test_transpile("signature sub mixed scalar + array args", '
sub f { my ($a, $b, $c) = @_; return join(":", $a, $b, $c); }
my @rest = (20, 30);
print f(10, @rest), "\n";
');

test_transpile("shift-run sub called through a code ref with @_", '
sub f { my $n = shift; my $m = shift; return $n * 10 + $m; }
sub d { my $r = \&f; return $r->(@_); }
print d(4, 2), "\n";
');

# ============ GLOB-INSTALLED CONSTANT SUBS (s304 E4.0 fuzzer find) ============
# `*NAME = sub () {...}` gives NAME an empty prototype perl knows via the live
# stash; _premerge_glob_const_prototypes registers it so the bareword is a
# zero-arg call (not a swallowed operand or a string) in every parse branch.

test_transpile("glob constant sub does not swallow + operand", '
use strict;
BEGIN { *_kn = sub () { 42 }; }
my $r = _kn + 1;
print "$r\n";
');

test_transpile("glob constant sub as regex operand in ternary", '
use strict;
BEGIN { my $rx = qr/^\w+$/; *_krx = sub () { $rx }; }
my $r = "Pt" =~ _krx ? "match" : "nomatch";
print "$r\n";
');

test_transpile("glob constant sub before concat", '
use strict;
BEGIN { *_kv = sub () { "CV" }; }
my $r = _kv . "-tail";
print "$r\n";
');

done_testing();

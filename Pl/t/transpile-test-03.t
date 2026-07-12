#!/usr/bin/env perl
# Transpile tests part 3: variable scoping

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
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

# Check dependencies
plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

# Run a Perl snippet and return output
sub run_perl {
    my ($code) = @_;
    my $output = `perl -e '$code' 2>&1`;
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
    my $cl_code = `$pl2cl $pl_file 2>&1`;

    # Write CL to temp file
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    # Run with sbcl
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

# ============ SCOPING TESTS ============

# if condition scoping - variable visible inside if/else but not after
test_transpile("scope: my in if condition - visible in then block", '
my $result = "";
if (my $x = 5) { $result = "x=$x"; }
print "$result\n";
');

test_transpile("scope: my in if condition - visible in else block", '
my $result = "";
if (my $x = 0) { $result = "then"; } else { $result = "else:x=$x"; }
print "$result\n";
');

test_transpile("scope: my in if condition - not visible after if", '
my $x = "outer";
if (my $x = 5) { }
print "after:$x\n";
');

test_transpile("scope: nested if with my in condition", '
my $r = "";
if (my $a = 1) {
    if (my $b = 2) {
        $r = "a=$a,b=$b";
    }
}
print "$r\n";
');

test_transpile("scope: my in elsif condition", '
my $r = "";
my $test = 0;
if ($test) { $r = "if"; }
elsif (my $x = 10) { $r = "elsif:x=$x"; }
print "$r\n";
');

# Inline declaration in expression
test_transpile("scope: inline my in expression", '
my $z = 2;
my $result = (my $y = 5) * $z;
print "result=$result,y=$y\n";
');

# Multiple declarations in same condition
test_transpile("scope: assignment chain with my", '
my $r;
if (my $a = my $b = 3) { $r = "a=$a,b=$b"; }
print "$r\n";
');

test_transpile("scope: chained my in if - not visible after", '
my $x = "outer_x";
my $y = "outer_y";
if (my $x = my $y = 3) { }
print "after:$x,$y\n";
');

# while loop scoping
test_transpile("scope: my in while condition - visible inside", '
my $r = "";
my $n = 3;
while (my $i = $n--) { $r .= "$i,"; }
print "$r\n";
');

test_transpile("scope: my in while - not visible after", '
my $i = "outer";
my $n = 1;
while (my $i = $n--) { }
print "after:$i\n";
');

test_transpile("scope: chained my in while - not visible after", '
my $x = "outer_x";
my $y = "outer_y";
my $n = 1;
while (my $x = my $y = $n--) { }
print "after:$x,$y\n";
');

# for loop scoping
test_transpile("scope: my in for init - visible inside loop", '
my $r = "";
for (my $i = 0; $i < 3; $i++) { $r .= "$i,"; }
print "$r\n";
');

test_transpile("scope: my in for init - not visible after loop", '
my $i = "outer";
for (my $i = 0; $i < 2; $i++) { }
print "after:$i\n";
');

# foreach loop scoping - use push since @arr = (1,2,3) has a bug
test_transpile("scope: foreach loop variable", '
my $r = "";
my @arr; push @arr, 1; push @arr, 2; push @arr, 3;
foreach my $x (@arr) { $r .= "$x,"; }
print "$r\n";
');

# Multiple variable declarations
test_transpile("scope: multiple vars in for init", '
my $r = "";
for (my $i = 0, my $j = 10; $i < 3; $i++, $j++) { $r .= "$i:$j,"; }
print "$r\n";
');

test_transpile("scope: multiple vars in for - not visible after", '
my $i = "outer_i";
my $j = "outer_j";
for (my $i = 0, my $j = 0; $i < 2; $i++) { }
print "after:$i,$j\n";
');

# List declaration scoping - my ($x, $y) in if
test_transpile("scope: list declaration in if", '
my $p = "outer_p";
my $q = "outer_q";
if (my ($p, $q) = (5, 10) and 1) { }
print "after:$p,$q\n";
');

# ============ LABELED BLOCK EXIT (last LABEL) ============

test_transpile("last LABEL exits bare block", '
my $x = 0;
SKIP: { last SKIP; $x = 1; }
print "$x\n";
');

test_transpile("last LABEL with other label name", '
my $x = 0;
OUTER: { last OUTER; $x = 1; }
print "$x\n";
');

test_transpile("SKIP block: code after last LABEL does not run", '
my @ran;
SKIP: {
  last SKIP;
  push @ran, "inner";
}
push @ran, "after";
print join(",", @ran), "\n";
');

# ============ CONTROL CHARACTER ESCAPE \cX ============

test_transpile("control char \\c@ is chr(0)", '
my $c = "\c@";
print ord($c), "\n";
');

test_transpile("control char \\c? is chr(127)", '
my $c = "\c?";
print ord($c), "\n";
');

test_transpile("control char \\cA-\\cZ range", '
print ord("\cA"), " ", ord("\cZ"), "\n";
');

test_transpile("control char \\c@ in string concat", '
my $s = "a\c@b";
print length($s), "\n";
');

# \&funcname - references to named subs
test_transpile("funcref: \\&foo stored and called",
    'sub foo { print "hello\n"; } my $r = \&foo; $r->();');

test_transpile("funcref: \\&foo called with args",
    'sub add { my ($a, $b) = @_; print $a + $b, "\n"; } my $r = \&add; $r->(3, 4);');

test_transpile("funcref: \\&foo ref()",
    'sub foo {} my $r = \&foo; print ref($r), "\n";');

# &funcname(args) - explicit call with & sigil
test_transpile("amp call: &foo(args)",
    'sub double { my ($n) = @_; print $n * 2, "\n"; } &double(7);');

# ============ BARE BLOCK PACKAGE SCOPING ============

# Bare blocks with package declarations must not leak *package* after the block.
# Before fix: (in-package :Foo) inside block leaked, causing subsequent
# top-level forms to read symbols in wrong package.

test_transpile("bare block package: __PACKAGE__ reverts after block", '
{ package Foo; }
print __PACKAGE__, "\n";
');

test_transpile("bare block package: my var visible outside block, in correct pkg", '
{ package Foo; my $x = 1; }
our $y = 2;
print $y, "\n";
');

test_transpile("bare block package: multiple packages in one block", '
{ package P1; package P2; }
print __PACKAGE__, "\n";
');

test_transpile("bare block package: outer package preserved after inner block", '
package Bar;
{ package Foo; }
print __PACKAGE__, "\n";
');

# ============ exists &sub / defined &sub ============

# Forward declaration only — exists is true, defined is false
test_transpile("exists &sub — forward declared only", '
sub t1;
my $e = exists &t1 ? "yes" : "no";
print $e, "\n";
');

test_transpile("defined &sub — forward declared only (not defined)", '
sub t1;
my $d = defined &t1 ? "yes" : "no";
print $d, "\n";
');

# Full definition — exists and defined both true
test_transpile("exists &sub — with body", '
sub t5 { 1; }
my $e = exists &t5 ? "yes" : "no";
print $e, "\n";
');

test_transpile("defined &sub — with body", '
sub t5 { 1; }
my $d = defined &t5 ? "yes" : "no";
print $d, "\n";
');

# ============ PACKAGE IN BARE BLOCK (task #79) ============

# Sub defined inside bare block with inline package change
# must end up in the correct CL package, not :main
test_transpile("bare block pkg: sub defined in inner package exists", '
{ package P1; sub tmc { 1; } }
my $e = exists &P1::tmc ? "yes" : "no";
print $e, "\n";
');

test_transpile("bare block pkg: sub defined in inner package is callable", '
{ package P1; sub tmc { 42; } }
print P1::tmc(), "\n";
');

test_transpile("bare block pkg: multiple packages in block", '
{ package P1; sub p1_sub { 1; } package P2; sub p2_sub { 2; } }
my $e1 = exists &P1::p1_sub ? "y" : "n";
my $e2 = exists &P2::p2_sub ? "y" : "n";
print "$e1 $e2\n";
');

# Regression: print UPPERCASE->chain() — uppercase/single-letter class name
# was mistakenly consumed as a print filehandle, leaving -> at position 0.
test_transpile("chained method: uppercase class name not treated as filehandle", '
package B; sub new { bless {v => $_[1]}, $_[0] } sub val { $_[0]->{v} }
package main;
print B->new("ok")->val(), "\n";
', "ok\n");

test_transpile("chained method: ALLCAPS class name not treated as filehandle", '
package DB; sub new { bless {}, $_[0] } sub ping { "pong" }
package main;
print DB->new()->ping(), "\n";
', "pong\n");

test_transpile("chained method: real filehandle still works", '
print STDOUT "yes\n";
', "yes\n");

# Regression: bless sub { ... }, "Class" — anonymous sub in arg position
# splice bug in PExpr.pm was eating the comma after sub { }, causing parse error.
test_transpile("bless: anon sub blessed into class", '
my $e1 = bless sub { 42 }, "E";
print ref($e1), "\n";
');

test_transpile("bless: anon sub called after bless", '
my $e1 = bless sub { return "hello" }, "E";
print $e1->(), "\n";
');

# Regression: package NAME; inside a sub body must not open a new CL section.
# Previously, package _foo; inside sub broke the section/bucket structure and
# caused all subsequent top-level code to be emitted inside the sub body.
test_transpile("package inside sub: inline switch does not leak", '
sub make_foo {
    package Foo;
    return bless {}, "Foo";
}
my $f = make_foo();
print ref($f), "\n";
print "ok\n";
');

# Regression: ref() on blessed array/scalar refs passed through function args
# %pl-flatten-list was stripping the class by extracting the inner value.
test_transpile("bless: ref() on blessed array survives function call", '
my $b = bless [], "MyArr";
sub check_ref { my ($x) = @_; print ref($x), "\n"; }
check_ref($b);
');

test_transpile("bless: ref() on blessed scalar survives function call", '
my $tmp = "hello";
my $s = \$tmp;
my $b = bless $s, "MyScl";
sub check_ref2 { my ($x) = @_; print ref($x), "\n"; }
check_ref2($b);
');

# sprintf %g must not strip significant integer-part trailing zeros: 100000
# (which %g prints in fixed notation, exp 5 < precision 6) was becoming "1".
test_transpile("sprintf %g keeps integer trailing zeros", '
printf "%g %g %g %g\n", 100000, 200000, 500000, 120000;
');
test_transpile("sprintf %g fixed vs exponential boundary", '
printf "%g %g %g\n", 999999, 1000000, 0.00001;
');

# Postfix -- on undef returns undef (NOT 0, unlike ++); then sets to -1.
test_transpile("postdec of undef returns undef", '
my $a; my $y = $a--;
printf "y=%s a=%s\n", (defined $y ? $y : "undef"), $a;
my @x; my $z = $x[0]--;
printf "z=%s x0=%s\n", (defined $z ? $z : "undef"), $x[0];
');

# Nested compound-assignment lvalue chains: each compound op returns its LHS as
# an lvalue, so the place form is shared.  The runtime must evaluate the place
# exactly once per op — otherwise the inner assignment re-runs and the result
# grows exponentially.  (opbasic/concat.t test 242.)
test_transpile("nested .= lvalue chain evaluates place once", '
my $a = "a";
(($a .= $a) .= $a) .= $a;
print "$a\n";
');
test_transpile("nested += lvalue chain evaluates place once", '
my $n = 1;
(($n += $n) += $n) += $n;
print "$n\n";
');

# CORE::<declarator> in expression context (CORE::my / CORE::state) names the
# bare declarator (PCL has no overridable builtins).  Without normalization it
# parsed as a function call and crashed.  (opbasic/concat.t.)
test_transpile("CORE::my declarator in expression context", '
my $h = { a => 1 };
print ref(CORE::my $x = $h), "\n";
');
test_transpile("CORE::state declarator in expression context", '
use feature "state";
sub f { my $r = (CORE::state $y = 7); $y }
print f(), f(), "\n";
');

# $^T (BASETIME) is the program start time in Unix seconds — must be a sane
# positive epoch value, not an unbound variable.  (op/lex_assign.t.)
test_transpile("\$^T is a positive epoch value", '
print(($^T > 1000000000 ? "yes" : "no"), "\n");
print((localtime($^T))[5] + 1900 >= 2020 ? "ok\n" : "bad\n");
');

# `undef` placeholder in a my-list LHS occupies a position; with a single declared
# var the (vector $x) shortcut used to DROP a leading/middle undef and misalign
# the assignment (my (undef, $b) = (10,20) wrongly gave $b=10).  Found while
# writing Pl/t/socket-01.t (getpeername unpack).
test_transpile("my-list leading undef placeholder", '
my (undef, $b) = (10, 20);
my (undef, undef, $c) = (1, 2, 3);
my @a = (5, 6, 7); my (undef, $x) = @a;
print "$b $c $x\n";
');
test_transpile("my-list interleaved undef placeholders and array slurp", '
my ($a, undef, $c, undef, $e) = (1 .. 5);
my (undef, @rest) = (9, 8, 7);
print "$a$c$e | @rest\n";
');

# A user sub with an old-style scalar prototype ($) imposes SCALAR context on
# the corresponding argument: an array yields its element count, a hash its
# bucket-ratio-ish scalar, and a wantarray-sensitive builtin (each/keys) its
# scalar value — rather than flattening into the arg list.  perl's t/test.pl
# `sub is ($$@)` relies on this so `is(@a, 3)` / `is(each @h, 0)` work.
test_transpile("scalar prototype (\$) imposes scalar context on array arg", '
sub takes ($) { return $_[0]; }
my @a = (10, 20, 30);
print takes(@a), "\n";
');
test_transpile("scalar prototype (\$\$) imposes scalar ctx on each slot", '
sub two ($$) { return "$_[0]|$_[1]"; }
my @a = (10, 20, 30);
print two(@a, 99), "\n";
');
test_transpile("scalar prototype (\$) collapses keys() to its count", '
sub one ($) { return $_[0]; }
my %h = (a => 1, b => 2, c => 3);
print one(keys %h), "\n";
');
# Guard: when a call passes FEWER args than the prototype mandates, an array
# argument is flattening to fill the slots — it must NOT be scalarized.  This
# is perl test.pl`s `sub like ($$@) { like_yn(0, @_) }` / `like_yn ($$$@)`
# pattern; collapsing @_ to its count breaks every test.pl-based file.
test_transpile("scalar prototype does not collapse a flattening \@_", '
sub f ($$@) { return g(0, @_); }
sub g ($$$@) { my ($flip, undef, $expected, $name, @mess) = @_;
               return "$expected|$name|@mess"; }
print f("got", "EXP", "nm", "x", "y"), "\n";
');


# ---- method-call invocant / package-resolution / indirect-object fixes ----

# A parenthesised method-call result used as an invocant in LIST context must
# stay a scalar invocant, not be wrapped in (vector ...) → "unblessed reference".
test_transpile("paren method invocant in list context", '
package Widget; sub new { bless {}, shift } sub name { "wid" }
package main;
sub take { print "got=$_[0]\n" }
take( ("Widget"->new)->name );
my @r = ( ("Widget"->new)->name );
print "arr=@r\n";
');

# main::Foo names the same package as Foo (main:: is the root-stash prefix).
test_transpile("main::Class->method resolves like Class->method", '
package Foo; sub new { bless {}, shift } sub hi { "hello" }
package main;
print "main::Foo->new->hi = ", ("main::Foo"->new)->hi, "\n";
print "isa = ", (("main::Foo"->new)->isa("Foo") ? "yes" : "no"), "\n";
');

# `is Qualified::name(ARGS)` is a function call argument, not the indirect
# object `Qualified::name->is(ARGS)`.
test_transpile("qualified-name(args) after a word is a funcall, not indirect obj", '
sub check { print "check: $_[0]\n" }
package Util; sub thing { "T:@_" }
package main;
check Util::thing(1, 2);
');


# A parenthesised my(...) declaration used as a list-operator argument must not
# be mistaken for the call's own argument parens: `f my($y), LIST` is
# f($y, LIST), not f($y).  (Same shape as `tie my($x), "Class".)
test_transpile("my(\$x) as a list-operator funcall argument", '
sub f { print "n=", scalar(@_), " v=@_\n" }
f my($a), "two";
f(my($b), "three");
my @c = (my($d), "four");
print "c=@c\n";
');

# `close F, LIST` / `fileno F, LIST`: a bareword filehandle passed to a
# single-filehandle function (close/fileno/eof) must consume ONLY the
# filehandle — the trailing comma belongs to the enclosing list, e.g.
# `ok(close F, 'desc')` is ok(close(F), 'desc'), NOT ok(close(F, 'desc')).
# Previously `close F, ...` grabbed the comma list → p-close got 2 args → a
# compile-time macro error that aborted the whole file (io/open.t).
test_transpile("close/fileno bareword FH consumes only the FH, not the comma list", '
sub note2 { print "n=", scalar(@_), " last=$_[-1]\n" }
open(F, ">", "/tmp/pcl_close_reg_$$") or die;
print F "x\n";
note2( close F, "desc-close" );
open(F, "<", "/tmp/pcl_close_reg_$$") or die;
note2( fileno F, "desc-fileno" );
close F;
unlink "/tmp/pcl_close_reg_$$";
');

# fork()/waitpid()/exec()/kill() — real process control via sb-posix.
# The parent gets the child PID, the child gets 0, both continue; waitpid sets
# $? and exec runs a program in the child.  (PCL-behaviour test: real perl forks
# too, but the child PID differs, so we assert the structural outcome, not exact
# text vs perl.)
{
    my $out = run_cl(<<'PERL');
$| = 1;
my $pid = fork();
die "fork failed: $!" unless defined $pid;
if ($pid == 0) { exec("echo", "child-exec-ok"); die "exec failed"; }
my $reaped = waitpid($pid, 0);
print "reaped-ok\n" if $reaped == $pid;
print "exit-status=", ($? >> 8), "\n";
PERL
    like($out, qr/child-exec-ok/,  'fork(): child runs and exec() launches a program');
    like($out, qr/reaped-ok/,      'waitpid() reaps the forked child');
    like($out, qr/exit-status=0/,  'waitpid() sets $? from the child exit');
}

# ============ M-C/M-D CAPTURE-PROMOTION TESTS (session 284) ============
# Shadow-aware, position-aware promotion of lexicals captured by named subs
# (Pl/Parser2.pm _promote_captured); these shapes previously gated whole
# files to v1 (closure.t/index.t/undef.t/hashassign.t families).

# A re-decl of the same name inside another named sub is a distinct shadow —
# it must not block promoting the file-level $i captured by foo.
test_transpile("capture: shadow decl in sibling sub does not block promotion", '
my $i = 1;
sub foo { $i = shift if @_; $i }
sub foo2 { my $i = shift; return $i }
foo(2);
print foo(), " ", foo2(9), "\n";
');

# Perl visibility: uses BEFORE the decl and the decl RHS read the global.
test_transpile("capture: pre-decl use and decl RHS read the package global", '
$v = "G";
print "pre=$v\n";
my $v = $v . "L";
sub getv { $v }
print "post=$v get=", getv(), "\n";
');

# Container decl WITH init captured by named subs (chdir.t %Saved_Env shape).
test_transpile("capture: hash with init captured by named subs", '
my %saved = (a => 1);
sub stash { $saved{$_[0]} = $_[1] }
sub fetch { $saved{$_[0]} }
stash("b", 2);
print "a=$saved{a} b=", fetch("b"), " n=", scalar(keys %saved), "\n";
');

# Multi-container list decl (undef.t/hashassign.t shape).
test_transpile("capture: multi-container list decl captured by named sub", '
my (%names, %copy);
%names = (x => "X");
%copy = %names;
sub inm { return $names{$_[0]} . $copy{$_[0]} }
print "r=", inm("x"), "\n";
');

# Mixed scalar+array list decl, sub called BEFORE the decl statement runs,
# plus "@a" / "$a[i]" / "$#a" interpolation following the rename (aassign.t).
test_transpile("capture: mixed list decl + array interpolation follows rename", '
my $ra = f1();
my ($x, @a) = @$ra;
sub f1 { $x = 1; @a = 2..4; return \\@a }
print "x=$x a=@a first=$a[0] last=$#a\n";
');

# Hash element and slice interpolation follow a container rename.
test_transpile("capture: hash element/slice interpolation follows rename", '
my %h = (k => "V", j => "W");
sub geth { $h{$_[0]} }
my @ks = ("k","j");
print "el=$h{k} slice=@h{@ks} get=", geth("j"), "\n";
');

# M-D (index.t shape): a lexical inside a named sub captured by a NESTED
# named sub shares one cell with the enclosing body.
test_transpile("capture: nested named sub captures enclosing sub lexical", '
sub run {
  my $store = 100;
  sub setter { $store = $_[0] }
  setter(7);
  print "store=$store\n";
}
run();
');

# Identity promotion: a file-unique name keeps its own name, so a DYNAMIC
# string eval still resolves it (hashassign.t %names + eval $tempval shape).
test_transpile("capture: file-unique name stays visible to dynamic string eval", '
my %names = (a => 1, b => 2);
sub geta { $names{a} }
my $tv = q{$names{b}};
print "e=", eval($tv), " g=", geta(), " k=", scalar(keys %names), "\n";
');

# Expression-embedded my (weaken(my $p = \%tb)) is block-scoped and boxed via
# p-my-= — reference identity must survive (hashassign.t 217/218).
test_transpile("embedded my decl: weak ref keeps hash identity", '
{
    my %tb;
    no warnings;
    use builtin qw(weaken);
    weaken(my $p = \\%tb);
    %tb = ();
    print "eq1=", ($p eq \\%tb ? "Y" : "N"), "\n";
    undef %tb;
    print "eq2=", ($p eq \\%tb ? "Y" : "N"), "\n";
}
');

# foreach over a single aliasable ELEMENT ($h{k} / $a[i]) binds the loop var as
# an ALIAS to the live container slot, so a write through $_ persists (v2 native
# via p-gethash-box / p-aref-box, s285 — was gated to v1).
test_transpile("foreach-alias: hash element writes through", '
my %h = (k => 1);
for ($h{k}) { $_ += 40 }
print $h{k}, "\n";
');
test_transpile("foreach-alias: array element writes through", '
my @a = (1, 2, 3);
for ($a[1]) { $_ *= 10 }
print "@a\n";
');
test_transpile("foreach-alias: array element regex subst in place", '
my @a = ("foo", "bar");
for ($a[0]) { s/o/0/g }
print "@a\n";
');

# Bare `return;` (no value) is context-sensitive: 0 elements in list context,
# undef in scalar/void (v2 emitted (p-return (p-undef)) → a spurious 1-element
# list; now emits the zero-arg (p-return) — sub.t check_ret(-1) list, s285).
test_transpile("bare return: empty list in list context", '
sub f { return; }
my @r = f();
print "count=", scalar(@r), "\n";
');
test_transpile("bare return: undef in scalar context", '
sub f { return; }
my $s = f();
print "def=", (defined $s ? "Y" : "N"), "\n";
');
test_transpile("bare return from nested for in list context", '
sub g { for ("x") { return if $_[0] < 0; } return 99; }
my @r = g(-1);
print "count=", scalar(@r), "\n";
');

done_testing();

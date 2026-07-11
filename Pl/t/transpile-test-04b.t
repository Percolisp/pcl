#!/usr/bin/env perl
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
    my $cl_code = `$pl2cl $pl_file 2>&1`;

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

done_testing();

#!/usr/bin/env perl
# Transpile tests part 4: math and string functions

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

# ============ MATH FUNCTIONS ============

# Use int() to avoid float formatting differences between Perl and CL
test_transpile("math: int(sin(0))", 'print int(sin(0)), "\n";');
test_transpile("math: int(cos(0))", 'print int(cos(0)), "\n";');
test_transpile("math: int(sqrt(16))", 'print int(sqrt(16)), "\n";');
test_transpile("math: sqrt(2) comparison", 'print sqrt(2) > 1.41 && sqrt(2) < 1.42 ? "ok" : "fail", "\n";');
test_transpile("math: int(exp(0))", 'print int(exp(0)), "\n";');
test_transpile("math: exp(1) comparison", 'print exp(1) > 2.71 && exp(1) < 2.72 ? "ok" : "fail", "\n";');
test_transpile("math: int(log(1))", 'print int(log(1)), "\n";');
test_transpile("math: log(exp(1)) comparison", 'print log(exp(1)) > 0.99 && log(exp(1)) < 1.01 ? "ok" : "fail", "\n";');
test_transpile("math: atan2(1,1) comparison", 'print atan2(1,1) > 0.78 && atan2(1,1) < 0.79 ? "ok" : "fail", "\n";');
test_transpile("math: int(atan2(0,1))", 'print int(atan2(0,1)), "\n";');
test_transpile("math: int(3.7)", 'print int(3.7), "\n";');
test_transpile("math: int(-3.7)", 'print int(-3.7), "\n";');
test_transpile("math: abs(-42)", 'print abs(-42), "\n";');
test_transpile("math: abs(42)", 'print abs(42), "\n";');

# ============ STRING FUNCTIONS ============

test_transpile("string: chr(65)", 'print chr(65), "\n";');
test_transpile("string: chr(97)", 'print chr(97), "\n";');
test_transpile("string: ord A", 'print ord("A"), "\n";');
test_transpile("string: ord a", 'print ord("a"), "\n";');
test_transpile("string: hex 0xff", 'print hex("ff"), "\n";');
test_transpile("string: hex 0x10", 'print hex("10"), "\n";');
test_transpile("string: oct 0777", 'print oct("777"), "\n";');
test_transpile("string: oct 0o10", 'print oct("10"), "\n";');
test_transpile("string: lcfirst", 'print lcfirst("HELLO"), "\n";');
test_transpile("string: ucfirst", 'print ucfirst("hello"), "\n";');
test_transpile("string: lc", 'print lc("HELLO"), "\n";');
test_transpile("string: uc", 'print uc("hello"), "\n";');
test_transpile("string: length", 'print length("hello"), "\n";');
test_transpile("string: substr 2 args", 'print substr("hello", 2), "\n";');
test_transpile("string: substr 3 args", 'print substr("hello", 1, 3), "\n";');
test_transpile("string: index", 'print index("hello", "l"), "\n";');
test_transpile("string: index with offset", 'print index("hello", "l", 3), "\n";');
test_transpile("string: rindex", 'print rindex("hello", "l"), "\n";');

# ============ SPRINTF ============

test_transpile("sprintf: string %s", 'print sprintf("%s", "hello"), "\n";');
test_transpile("sprintf: integer %d", 'print sprintf("%d", 42), "\n";');
test_transpile("sprintf: float %f", 'print sprintf("%.2f", 3.14159), "\n";');
test_transpile("sprintf: multiple args", 'print sprintf("%s is %d", "answer", 42), "\n";');

# ============ FILE I/O ============

test_transpile("file I/O: write and read back", '
my $file = "/tmp/pcl-test-file.txt";

# Write to file using print and say
open(FH, ">", $file);
print FH "line one";
print FH "\n";
say FH "line two";
print FH "line three\n";
close(FH);

# Read back and verify
my $content = "";
open(FH, "<", $file);
while (my $line = <FH>) {
    $content = $content . $line;
}
close(FH);

# Clean up
unlink($file);

# Print what we read
print $content;
');

test_transpile("file I/O: readline returns undef at EOF", '
my $file = "/tmp/pcl-test-eof.txt";
open(FH, ">", $file);
print FH "only line\n";
close(FH);

open(FH, "<", $file);
my $line1 = <FH>;
my $line2 = <FH>;
close(FH);
unlink($file);

print "line1 defined: ", defined($line1) ? "yes" : "no", "\n";
print "line2 defined: ", defined($line2) ? "yes" : "no", "\n";
');

# ============ REGEX ============

test_transpile("regex: simple match true", '
my $str = "hello world";
if ($str =~ /world/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: simple match false", '
my $str = "hello world";
if ($str =~ /foo/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: case insensitive", '
my $str = "Hello World";
if ($str =~ /hello/i) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: negation", '
my $str = "hello world";
if ($str !~ /foo/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: substitution single", '
my $str = "foo bar foo";
$str =~ s/foo/baz/;
print $str, "\n";
');

test_transpile("regex: substitution global", '
my $str = "foo bar foo";
$str =~ s/foo/baz/g;
print $str, "\n";
');

test_transpile("regex: substitution case insensitive", '
my $str = "Hello World";
$str =~ s/hello/hi/i;
print $str, "\n";
');

test_transpile("regex: tr lowercase to uppercase", '
my $str = "hello";
$str =~ tr/a-z/A-Z/;
print $str, "\n";
');

test_transpile("regex: tr uppercase to lowercase", '
my $str = "HELLO";
$str =~ tr/A-Z/a-z/;
print $str, "\n";
');

test_transpile("regex: tr single chars", '
my $str = "abc";
$str =~ tr/abc/xyz/;
print $str, "\n";
');

# ============ REGEX: METACHARACTERS ============

test_transpile("regex: dot matches any char", '
my $str = "abc";
if ($str =~ /a.c/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: dot does not match newline by default", '
my $str = "a\nc";
if ($str =~ /a.c/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: star quantifier", '
my $str = "goooal";
if ($str =~ /go*al/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: star matches zero", '
my $str = "gal";
if ($str =~ /go*al/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: plus quantifier", '
my $str = "goooal";
if ($str =~ /go+al/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: plus requires one", '
my $str = "gal";
if ($str =~ /go+al/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: question mark optional", '
my $str = "color";
if ($str =~ /colou?r/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: question mark with char", '
my $str = "colour";
if ($str =~ /colou?r/) { print "yes\n"; } else { print "no\n"; }
');

# ============ REGEX: ANCHORS ============

test_transpile("regex: caret anchor match", '
my $str = "hello world";
if ($str =~ /^hello/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: caret anchor no match", '
my $str = "say hello";
if ($str =~ /^hello/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: dollar anchor match", '
my $str = "hello world";
if ($str =~ /world$/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: dollar anchor no match", '
my $str = "world hello";
if ($str =~ /world$/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: both anchors", '
my $str = "hello";
if ($str =~ /^hello$/) { print "yes\n"; } else { print "no\n"; }
');

# ============ REGEX: CHARACTER CLASSES ============

test_transpile("regex: character class", '
my $str = "cat";
if ($str =~ /[cb]at/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: negated character class", '
my $str = "cat";
if ($str =~ /[^d]at/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: digit class", '
my $str = "abc123";
if ($str =~ /\d+/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: word class", '
my $str = "hello_world";
if ($str =~ /^\w+$/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("regex: whitespace class", '
my $str = "hello world";
if ($str =~ /\s/) { print "yes\n"; } else { print "no\n"; }
');

test_transpile("&\$scalar() — code ref call via & sigil", '
my $foo = sub { return 42 };
my $x = &$foo();
print $x, "\n";
');

test_transpile("&\$scalar(args) — code ref call with args", '
my $foo = sub { return $_[0] + 1 };
my $x = &$foo(5);
print $x, "\n";
');

test_transpile("&{\$arr[N]}() — code ref call from array element", '
my @subs;
$subs[0] = sub { return 7 };
my $x = &{$subs[0]}();
print $x, "\n";
');

test_transpile("&\$foo() inside print args", '
my $foo = sub { return "hi" };
print(&$foo(), "\n");
');

test_transpile("map({key=>\$_}, LIST) — hash constructor in paren-form map", '
my @res = map({a => $_}, ("chobb"));
print $res[0]->{a}, "\n";
');

test_transpile("map {key=>\$_}, LIST — hash constructor in block-form map", '
my @res = (map {a => $_}, ("chobb"));
print $res[0]->{a}, "\n";
');

test_transpile("map fat-comma returns key-value pairs", '
my @pairs = map { $_ => uc $_ } "a".."c";
print join(":", @pairs), "\n";
', "a:A:b:B:c:C\n");

test_transpile("map fat-comma builds hash", '
my %h = map { $_ => uc $_ } "a".."c";
print $h{b}, "\n";
', "B\n");

test_transpile("map simple block returns one element per input", '
my @r = map { $_ } ("yoyodyne");
print scalar(@r), "\n";
print $r[0], "\n";
', "1\nyoyodyne\n");

# ============ LIST CONTEXT / ASSIGNMENT REGRESSIONS ============

test_transpile("return 1..4 gives range not flip-flop", '
sub f { return 1..4 }
my ($a,$b) = f();
print "$a:$b\n";
');

test_transpile("(func())[0,0] list-slice repeats element", '
sub f { ("x", "y", "z") }
my @r = (f())[0,0];
print "$r[0]:$r[1]\n";
');

test_transpile("greedy array in list assignment clears later array", '
my (@a, @b) = (1..5);
@a = 1..10; @b = 100..110;
(@a, @b) = qw(x y);
print scalar(@a), " ", scalar(@b), "\n";
');

test_transpile('string interpolation: $a[0][1] nested subscript', '
my @a;
$a[0] = [10, 20, 30];
print "$a[0][1]\n";
');

test_transpile('string interpolation: $h{a}{b} nested subscript', '
my %h;
$h{a} = {b => "found"};
print "$h{a}{b}\n";
');

# ============ ANON-HASH vs BARE-BLOCK DISAMBIGUATION ============
# PPI mis-tokenizes `{ LITERAL , ... }` (string/number literal then comma) as a
# bare block; Perl treats it as an anon-hash constructor in term context.
# (Barewords/variables stay blocks; map-block `{ 'a', $_ }` stays a code block.)
# (keys are double-quoted so the code survives the run_perl `perl -e '...'` wrapper)
test_transpile('anon-hash: eval string literal-comma is HASH',
    q{$a = eval "{ \"a\" , \"foo\" }"; print ref($a), "\n";});
test_transpile('anon-hash: eval "{ 1 , 2 }" is HASH',
    q{$a = eval "{ 1 , 2 }"; print ref($a), "\n";});
test_transpile('anon-hash: eval comma chain is HASH and keeps pairs',
    q{$a = eval "{ \"a\" , \"b\" , \"c\" , \"d\" }"; print ref($a), ":", $a->{a}, $a->{c}, "\n";});

# ============ do BLOCK while/until — POST-test loops ============
# `do {} while/until COND` must run the body at least once (condition tested
# afterwards), unlike the pre-test while/until statement modifier.
test_transpile('do-while: false cond still runs body once',
    q{my $x=0; do { ++$x } while 0; print "$x\n";});
test_transpile('do-until: true cond still runs body once',
    q{my $y=0; do { ++$y } until 1; print "$y\n";});
test_transpile('do-while: iterates until cond false',
    q{my $z=0; do { ++$z } while $z<3; print "$z\n";});
test_transpile('do-while: post-increment cond fills array incl. last',
    q{my $x=0; my @a; do { $a[$x]=$x } while ($x++)<5; print join(" ",@a), "\n";});

# ============ package NAME VERSION + __PACKAGE__ in string eval ============
test_transpile('package VERSION (non-block) sets $Pkg::VERSION',
    q{package Foo 3.5; package main; print "$Foo::VERSION\n";});
test_transpile('package VERSION (block) sets $Pkg::VERSION',
    q{package Foo 11 { } print "$Foo::VERSION\n";});
# string eval inside a package block sees the enclosing package (not main).
test_transpile('eval "__PACKAGE__" inside package block resolves to that package',
    q{package Foo { my $p = eval("__PACKAGE__"); print "$p\n"; }});
test_transpile('nested package blocks: __PACKAGE__ via string eval',
    q{$main::r=""; package Foo { $main::r.=eval("__PACKAGE__"); package Bar::Baz { $main::r.=eval("__PACKAGE__"); } } print "$main::r\n";});

# A main-package global ($::x / $main::x) referenced ONLY inside a sub must get
# a forward defvar (undef when unset), not crash with an unbound variable.
test_transpile('main-package global used only in a sub is undef, not unbound',
    q{sub f { return $::TODO ? "y" : "n"; } print f(), "\n";});
test_transpile('$main::x set at top level, read only inside a sub',
    q{$main::g = 42; sub get { return $main::g; } print get(), "\n";});

# `$#[0]` is the removed `$#` magic taking a subscript = element 0 of @#.
# An undeclared @# must read as empty (undef), not crash unbound.
test_transpile('$#[0] on empty @# is undef, not a crash',
    q{$x = $#[0]; print "[$x]\n";});
test_transpile('$#[N] indexes array @#',
    q{@{"#"}=(10,20,30); print $#[0], " ", $#[2], "\n";});

# ============ chained deref-write autoviv keeps the root scalar boxed ======
# s278 VarAnnotator fix (write-deref-viv): `$r->{A}[0] = V` autovivifies
# THROUGH $r — an unboxed $r made every deref re-vivify a fresh container
# (exists_sub.t t13: the stored coderef "vanished").
test_transpile('chained deref write $r->{A}[0] persists through the root scalar',
    q{my $r; $r->{A}[0] = 5; print $r->{A}[0], "\n";});
test_transpile('coderef stored via chained deref write is found again',
    q{sub t4; my $r; $r->{A}[0] = \&t4; print( (exists &{$r->{A}[0]}) ? "yes\n" : "no\n");});
test_transpile('array-of-hash chained write',
    q{my $r; $r->[1]{k} = "v"; print $r->[1]{k}, "\n";});
test_transpile('plain container element writes stay correct beside chained ones',
    q{my $h; my %t; $t{x} = 1; $h->{a}{b} = 2; my $n = $t{x} + 1; print "$h->{a}{b} $n\n";});

# Typed lexicals (`my Foo $f`): the class name is runtime-inert and must be
# dropped, leaving a plain scalar lexical (regression: v2 died "unsupported
# declaration" in _multi_decl — see multideref.t).
test_transpile('typed lexical my Foo $f is a plain scalar',
    q{package Foo; sub new { bless {}, shift } package main; my Foo $f = Foo->new; $f->{c} = 7; print $f->{c}, "\n";});
test_transpile('typed lexical without init',
    q{package Dog; package main; my Dog $spot; $spot = "rex"; print "$spot\n";});

# W10 unmangle-when-unique (s278c): a file-unique my-lexical spanning a package
# boundary is renamed to the PLAIN $Pkg::name global (no __file__N mangle), so a
# dynamic string eval that names the bare var — in the declaring package — sees
# the same cell.  Regression: the old unconditional mangle forced the whole file
# to v1 whenever any dynamic eval was in scope.
test_transpile('spanning lexical stays visible to a dynamic eval that names it',
    q{my $strval = "init"; { package Bar; sub show { $strval } } $strval = "changed"; my $code = q<$strval>; print eval($code), "\n"; print Bar::show(), "\n";});

# Self-referential container init (`my @a = (@a, …)` / `my %h = (%h, …)`): the
# RHS must see the OUTER var (Perl scopes the new lexical from the NEXT
# statement).  v2 now binds the container to (p-copy-array/-hash <RHS>) in the
# let BINDING position, where the outer var is still visible — mirroring v1's
# "init in let binding" dance.  Regression: v2 died "self-referential init".
test_transpile('self-ref array init reads the outer array',
    q{my @bee = (1,2,3); { my @bee = (@bee, 4); print "@bee\n"; } print "@bee\n";});
test_transpile('self-ref array init with element inserted around it',
    q{my @bee = ('bar','burbl'); { my @bee = ('XXX',@bee,'YYY'); print "@bee\n"; }});
test_transpile('self-ref hash init reads the outer hash',
    q{my %h = (a=>1); { my %h = (%h, c=>3); print join(",", map {"$_=$h{$_}"} sort keys %h), "\n"; }});
# Copy semantics: the inner container is a COPY — mutating it must NOT leak into
# the outer one (regression: p-copy-hash's hash-table branch shared value boxes,
# so `$h{a}=…` mutated the source hash; hit v1's `local %h = %h` too).
test_transpile('self-ref array init is a copy, not an alias',
    q{my @a = (1,2,3); { my @a = @a; push @a, 99; $a[0] = 'X'; } print "@a\n";});
test_transpile('self-ref hash init is a copy, not an alias',
    q{my %g = (a=>1); { my %g = %g; $g{a} = 42; $g{z} = 9; } print "$g{a}\n";});

# CORE:: declarator prefix (my/our/state/local): the prefix forces the core
# builtin over a same-named user sub, but a declarator can't be shadowed, so
# CORE::my ≡ my.  Normalized at source level (Pl::Parser::_preprocess_source)
# because PPI mis-structures `for CORE::my $v (@l) {…}` otherwise.  Regression:
# v2 died "CORE:: declarator prefix" / "foreach without list"; v1 dropped the
# whole for-loop.
test_transpile('for CORE::my loop variable',
    q{my @o = (1,2,3); my @r; for CORE::my $v (@o) { push @r, $v } print "@r\n";});
test_transpile('CORE::my list declaration',
    q{CORE::my ($a,$b) = (7,8); print "$a,$b\n";});
test_transpile('literal CORE::my inside a string is not rewritten',
    q{my $s = "keep CORE::my literal"; print "$s\n";});
test_transpile('CORE::our scalar and $CORE:: package var coexist',
    q{CORE::our $y = 9; $CORE::keep = 5; print "$y $CORE::keep\n";});

# Container captured by a named sub (the `{ my %cache; sub get{} sub set{} }`
# encapsulated-state idiom): promoted to a shared defvar container cell, so the
# hoisted subs and in-place code share one @/%.  Regression: v2 gated the whole
# file ("possibly captured by nested sub").  Sigil-aware rename via the shared
# _rewrite_var_uses helper.
test_transpile('array captured by named subs (static-var idiom)',
    q{{ my @stack; sub push_it { push @stack, $_[0] } sub all { return @stack } } push_it($_) for (10,20,30); print join(",", all()), "\n";});
test_transpile('hash captured by named subs',
    q{{ my %seen; sub mark { $seen{$_[0]}++ } sub cnt { scalar keys %seen } } mark("a"); mark("b"); mark("a"); print cnt(), "\n";});
test_transpile('captured array element + $#array follow the promoted cell',
    q{{ my @q; sub add { push @q, $_[0] } sub last_i { return $#q } } add(5); add(6); print "$q[0] $q[1] ", last_i(), "\n";});
# A scalar $x beside a captured @x must NOT be conflated (sigil-aware): here the
# captured @data and an unrelated scalar $data coexist.
test_transpile('captured @x does not conflate a sibling scalar $x',
    q{my $data = "S"; { my @data; sub feed { push @data, $_[0] } sub dump_it { "@data" } } feed(1); feed(2); print "$data ", dump_it(), "\n";});
# Block-local static vars: the SAME name declared in two separate blocks are
# DISTINCT variables — each promotion is scoped to its own block's extent, so
# they must not merge (regression: file-wide decl_count gated both — do.t idiom).
test_transpile('same-name captured scalar in two blocks stays distinct',
    q{{ my $n; sub a1 { $n++ } sub g1 { $n } } { my $n; sub a2 { $n += 10 } sub g2 { $n } } a1(); a1(); a2(); print g1(), " ", g2(), "\n";});
test_transpile('same-name captured array in two blocks stays distinct',
    q{{ my @x; sub p1 { push @x, $_[0] } sub s1 { join(",",@x) } } { my @x; sub p2 { push @x, $_[0] } sub s2 { join(",",@x) } } p1(1); p1(2); p2(9); print s1(), "|", s2(), "\n";});
# An interpolated container ("@x") can't be reached by a token rewrite → the
# interp guard must REFUSE promotion (regression: only $-sigil interpolation was
# detected, so a bare @x read stayed unrenamed → split from the renamed writes).
test_transpile('interpolated captured array is not promoted (stays correct)',
    q{{ my @y; sub q1 { push @y, $_[0] } sub d1 { "@y" } } q1(3); q1(4); print d1(), "\n";});

# s287 — bare-block continue (loopctl.t): continue runs after normal exit or
# next, is skipped by last, and redo re-runs the body without it; the labeled
# form keeps its continue inside the compound, the unlabeled form is joined
# from PPI's orphan sibling statement (with a trailing glommed statement).
test_transpile('bare-block continue: labeled/unlabeled, last/next/redo paths',
    q{my $ok = 0;
{ print "x"; } continue { print "c"; } $ok = 1; print "t$ok";
L1: { print "A"; last L1; } continue { print "NO"; }
my $first = 1;
L2: { print "B"; if ($first) { $first = 0; redo L2; } } continue { print "C"; }
L3: { print "D"; next L3; print "NO2"; } continue { print "E"; }
OUT: { L4: { print "F"; } continue { print "G"; last OUT; } print "NO3"; }
print "\n";});

# s287 — list-form self-referential my init (array.t bug 70171 family) + the
# chained-declarator forms: the self-referenced container binds to a copy of
# its outer self; chained `my ... = my ... = LIST` collapses to one binding.
test_transpile('list self-ref my init and chained my declarators',
    q{my @bee = qw(foo bar burbl blah);
{ my (undef,@bee) = @bee; print "1:@bee\n"; }
{ my ($x, @bee) = ('X', @bee); print "2:$x|@bee\n"; }
my ($p, $q) = (1, 2);
{ my ($p, $q) = ($q, $p); print "3:$p$q\n"; }
print "4:$p$q\n";
{ my @bee = my @bee = qw(fee fie); print "5:@bee\n"; }
{ my (@bim) = my(@bee) = (7, 8); print "6:@bee|@bim\n"; }
print "7:@bee\n";});

# s287 — standalone label as a backward goto target (array.t/my.t goto
# variants): (tagbody :label ...) over the block remainder; a my jumped back
# over re-initializes.
test_transpile('standalone label with backward goto',
    q{{ my ($i, $ra);
  again:
    my @a = @$ra;
    @a = (1, 2, 3, 4);
    $ra = \@a;
    goto again unless $i++;
    print "@a\n"; }});

# s288 — sub-body :void regime (task #60): *wantarray* bound :void once
# around a multi-statement body, tail restores the caller's context via
# *pcl-caller-wantarray*.  Covers: context-sensitive builtin tail (keys),
# wantarray() mid-body and in tail, a void-position call observing :void,
# tail `EXPR if COND`, a compound (if/else) tail's branch leaves, and a
# g-match in void statement position (must not see the caller's list ctx).
test_transpile('sub-body void regime: contexts across tails and void calls',
    q{our @CTX;
my %H = (a=>1, b=>2, c=>3);
sub tailkeys { my $x = shift; $x++; keys %H }
my $s = tailkeys(1); my @l = sort(tailkeys(1));
print "tail: $s|@l\n";
sub wa { my $d = shift; $d++; wantarray ? "L" : defined(wantarray) ? "S" : "V" }
my @wl = wa(0);
print "wa: ", scalar(wa(0)), " $wl[0]\n";
sub inner { push @CTX, wantarray ? "L" : defined(wantarray) ? "S" : "V"; return }
sub voidwa { my $q = shift; $q++; inner(); return "ok" }
my @r = voidwa(5);
sub single { inner() }
single(); my $sx = single(); my @sy = single();
print "ctx: @CTX\n";
sub tmod { my $c = shift; $c += 0; 5 if $c }
my @t2 = tmod(1);
print "tmod: ", scalar(tmod(1)), " @t2\n";
sub condtail { my $c = shift; $c++; if ($c > 1) { keys %H } else { "no" } }
my $cs = condtail(1); my @cl = sort(condtail(1));
print "condtail: $cs|@cl\n";
sub gm { my ($str) = @_; $str =~ /(.)/g; return $1 }
my @g = gm("xyz");
print "gmatch: $g[0]\n";});

done_testing();

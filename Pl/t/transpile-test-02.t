#!/usr/bin/env perl
# Transpile tests part 2: string operations, arrays, hashes, refs, objects

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
    # Use -E to enable say and other modern features
    my $output = `perl -E '$code' 2>&1`;
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

# ============ TESTS ============

# chomp and chop
test_transpile("chomp: basic", 'my $s = "hello\n"; chomp($s); say "[$s]";');
test_transpile("chomp: no newline", 'my $s = "hello"; chomp($s); say "[$s]";');
test_transpile("chomp: return value", 'my $s = "hi\n"; my $n = chomp($s); say $n;');

test_transpile("chop: basic", 'my $s = "hello"; chop($s); say "[$s]";');
test_transpile("chop: return value", 'my $s = "world"; my $c = chop($s); say "removed: $c";');

# Arrays - use variable workaround for print $a[n] limitation
test_transpile("array: create and access", 'my @a; push @a, 1; push @a, 2; push @a, 3; my $x = $a[1]; say $x;');
test_transpile("array: negative index", 'my @a; push @a, 10; push @a, 20; push @a, 30; my $x = $a[-1]; say $x;');
test_transpile("array: push/pop", 'my @a; push @a, 1; push @a, 2; my $x = pop @a; say $x;');
test_transpile("array: shift/unshift", 'my @a; push @a, 2; push @a, 3; unshift @a, 1; my $x = shift @a; say $x;');
test_transpile("array: scalar context (length)", 'my @a; push @a, 1; push @a, 2; push @a, 3; my $len = scalar(@a); say $len;');

# Hashes - use variable workaround for print $h{k} limitation
test_transpile("hash: create and access", 'my %h; $h{name} = "Alice"; my $x = $h{name}; say $x;');
test_transpile("hash: keys count", 'my %h; $h{a} = 1; $h{b} = 2; my @k = keys %h; my $n = scalar(@k); say $n;');

# References
test_transpile("ref: scalar ref", 'my $x = 42; my $ref = \\$x; my $v = $$ref; say $v;');
test_transpile("ref: modify through ref", 'my $x = 10; my $ref = \\$x; $$ref = 20; say $x;');
test_transpile("ref: array ref", 'my $aref = [1, 2, 3]; my $v = $aref->[1]; say $v;');
test_transpile("ref: hash ref", 'my $href = {a => 1, b => 2}; my $v = $href->{b}; say $v;');

# Objects
test_transpile("object: bless and ref", '
package Counter;
sub new { my $class = shift; my $self = {count => 0}; bless $self, $class; return $self; }
package main;
my $c = Counter->new();
say ref($c);
');

test_transpile("object: method call", '
package Counter;
sub new { my $class = shift; my $self = {count => 0}; bless $self, $class; return $self; }
sub get { my $self = shift; return $self->{count}; }
sub incr { my $self = shift; $self->{count}++; }
package main;
my $c = Counter->new();
$c->incr();
$c->incr();
my $v = $c->get();
say $v;
');

# wantarray
test_transpile("wantarray: scalar context", '
sub ctx { if (wantarray()) { return (1, 2, 3); } else { return "scalar"; } }
my $s = ctx();
say $s;
');

test_transpile("wantarray: list context", '
sub ctx { if (wantarray()) { return (1, 2, 3); } else { return "scalar"; } }
my @a = ctx();
say "@a";
');

# ============ MULTIPLE PACKAGES ============

test_transpile("packages: switch between two", '
package Foo;
sub greet { return "Hello from Foo"; }

package Bar;
sub greet { return "Hello from Bar"; }

package main;
my $f = Foo::greet();
my $b = Bar::greet();
say $f;
say $b;
');

test_transpile("packages: three packages", '
package Alpha;
sub value { return 1; }

package Beta;
sub value { return 2; }

package Gamma;
sub value { return 3; }

package main;
my $sum = Alpha::value() + Beta::value() + Gamma::value();
say $sum;
');

test_transpile("packages: call across packages", '
package Math;
sub double { my $n = shift; return $n * 2; }
sub triple { my $n = shift; return $n * 3; }

package Compute;
sub calc { my $n = shift; return Math::double($n) + Math::triple($n); }

package main;
my $r = Compute::calc(5);
say $r;
');

# ============ MULTIPLE CLASSES ============

test_transpile("classes: two classes", '
package Dog;
sub new { my $class = shift; bless { name => shift }, $class; }
sub speak { my $self = shift; return $self->{name} . " says woof"; }

package Cat;
sub new { my $class = shift; bless { name => shift }, $class; }
sub speak { my $self = shift; return $self->{name} . " says meow"; }

package main;
my $d = Dog->new("Rex");
my $c = Cat->new("Whiskers");
say $d->speak();
say $c->speak();
');

test_transpile("classes: class with multiple methods", '
package Point;
sub new {
    my $class = shift;
    my $self = { x => shift, y => shift };
    bless $self, $class;
    return $self;
}
sub x { my $self = shift; return $self->{x}; }
sub y { my $self = shift; return $self->{y}; }
sub move {
    my $self = shift;
    my $dx = shift;
    my $dy = shift;
    $self->{x} = $self->{x} + $dx;
    $self->{y} = $self->{y} + $dy;
}
sub to_string {
    my $self = shift;
    return "(" . $self->{x} . "," . $self->{y} . ")";
}

package main;
my $p = Point->new(3, 4);
say $p->to_string();
$p->move(2, -1);
say $p->to_string();
');

test_transpile("classes: objects in array", '
package Item;
sub new { my $class = shift; bless { val => shift }, $class; }
sub val { my $self = shift; return $self->{val}; }

package main;
my @items;
push @items, Item->new(10);
push @items, Item->new(20);
push @items, Item->new(30);
my $sum = 0;
foreach my $i (@items) {
    $sum = $sum + $i->val();
}
say $sum;
');

test_transpile("classes: objects in hash", '
package Person;
sub new { my $class = shift; bless { name => shift }, $class; }
sub name { my $self = shift; return $self->{name}; }

package main;
my %people;
$people{alice} = Person->new("Alice");
$people{bob} = Person->new("Bob");
say $people{alice}->name();
say $people{bob}->name();
');

# ============ COMPLEX CONTROL FLOW ============

test_transpile("complex: nested loops with last/next", '
my $count = 0;
foreach my $i (1, 2, 3) {
    foreach my $j (1, 2, 3, 4) {
        if ($j == 2) { next; }
        if ($j == 4) { last; }
        $count++;
    }
}
say $count;
');

test_transpile("complex: while with multiple conditions", '
my $x = 0;
my $y = 10;
while ($x < 5 && $y > 5) {
    $x++;
    $y--;
}
say "$x $y";
');

test_transpile("complex: chained method calls", '
package Builder;
sub new { my $class = shift; bless { parts => [] }, $class; }
sub add {
    my $self = shift;
    my $p = shift;
    push @{$self->{parts}}, $p;
    return $self;
}
sub build {
    my $self = shift;
    return join("-", @{$self->{parts}});
}

package main;
my $b = Builder->new();
$b->add("A");
$b->add("B");
$b->add("C");
say $b->build();
');

test_transpile("complex: recursive function", '
sub fib {
    my $n = shift;
    if ($n <= 1) { return $n; }
    return fib($n - 1) + fib($n - 2);
}
say fib(10);
');

test_transpile("complex: mutual recursion", '
sub is_even {
    my $n = shift;
    if ($n == 0) { return 1; }
    return is_odd($n - 1);
}
sub is_odd {
    my $n = shift;
    if ($n == 0) { return 0; }
    return is_even($n - 1);
}
say is_even(10);
say is_odd(7);
');

# ============ DATA STRUCTURES ============

test_transpile("data: array of arrays", '
my @matrix;
push @matrix, [1, 2, 3];
push @matrix, [4, 5, 6];
my $v = $matrix[1]->[2];
say $v;
');

test_transpile("data: hash of hashes", '
my %data;
$data{user1} = { name => "Alice", age => 30 };
$data{user2} = { name => "Bob", age => 25 };
say $data{user1}->{name};
say $data{user2}->{age};
');

test_transpile("data: mixed nested structure", '
my $data = {
    users => [
        { name => "Alice" },
        { name => "Bob" }
    ],
    count => 2
};
say $data->{users}->[0]->{name};
say $data->{count};
');

# ============ STRING OPERATIONS ============

test_transpile("string: uc and lc", '
my $s = "Hello World";
say uc($s);
say lc($s);
');

test_transpile("string: substr", '
my $s = "Hello World";
say substr($s, 0, 5);
say substr($s, 6);
');

test_transpile("string: index", '
my $s = "Hello World";
say index($s, "o");
say index($s, "o", 5);
');

test_transpile("string: concatenation chain", '
my $a = "Hello";
my $b = " ";
my $c = "World";
my $d = "!";
my $result = $a . $b . $c . $d;
say $result;
');

test_transpile("string: repetition", '
my $s = "ab";
my $r = $s x 4;
say $r;
');

# ============ REFERENCES ============

test_transpile('double deref $$ref', '
my $y = 42;
my $ref = \$y;
my $got = $$ref;
print "$got\n";
');

test_transpile('double deref ${$ref} block form', '
my $y = 42;
my $ref = \$y;
my $got = ${$ref};
print "$got\n";
');

# Triple dereference via explicit block form.
# Note: $$$refref (no braces) is misread by PPI as $$ (PID magic var) + $refref,
# so PCL requires the explicit block form ${$$refref} for triple dereference.
test_transpile('triple deref ${$$refref} block form', '
my $y = 42;
my $ref_y = \$y;
my $refref = \$ref_y;
my $got = ${$$refref};
print "$got\n";
');

# ============ NUMERIC OPERATIONS ============

test_transpile("numeric: int and abs", '
say int(3.7);
say int(-3.7);
say abs(-42);
');

test_transpile("numeric: modulo", '
say 17 % 5;
say -17 % 5;
');

test_transpile("numeric: compound assignment", '
my $x = 10;
$x += 5;
say $x;
$x -= 3;
say $x;
$x *= 2;
say $x;
');

# ============ FILETEST OPS AS LIST-OP ARGS ============
# A filetest operator (-e/-f/-d/...) leading the args of a list operator
# (print/say/return) was mistaken for a binary op, making the list op zero-arg
# and falling through to a PARSE ERROR.  Filetests are always unary prefix.
test_transpile('filetest -e as sole print arg',
    'print((-e "/") ? "y" : "n"); print " "; print -e "/" ? "yes" : "no";');
test_transpile('filetest -d leads print args',
    'print -d "/" ? "dir" : "nodir";');
test_transpile('filetest -e with concat precedence',
    'print -e "/" . "zzz" ? "exists" : "absent";');
# A prefix !/~ immediately before a filetest must reduce inner-first:
# `!-e $f` is `!(-e $f)`.  (Was a PARSE ERROR — also blocked Perl's test.pl.)
test_transpile('negated filetest !-e',
    'my $f="/nonexistent_zzz"; print( (!-e $f) ? "absent" : "present" );');
test_transpile('negated filetest ! -d with space',
    'my $f="/nonexistent_zzz"; print( (! -d $f) ? "notdir" : "dir" );');

# ============ R1 CRASH REGRESSION: top-level `local` caps inlining ============
# A top-level `local $x = ...` wraps the entire rest of the file in one giant
# CL `let`.  R1 declaims the hot fast-path operators `inline`; inlining them
# into a function that large blows up SBCL's constraint propagation and
# OOM-crashes compilation (perl-tests/local.t, session 268).  Codegen must emit
# a `(declare (notinline ...))` at the head of a top-level local's let body to
# suppress inlining there (cold, runs-once code) — but must NOT do so for a
# `local` inside a sub, where hot code has to keep open-coding the fast paths.
sub transpile_only {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return scalar `$pl2cl $pl_file 2>/dev/null`;
}
# W12 raw-slot seam writes: the text-scan VarAnnotator missed embedded writes
# (no statement-root `$x = RHS` shape), so the name stayed a raw let slot while
# the write lowered through the v1 seam as (p-my-= ...) = box-set on a raw
# value — a silent no-op.  The W12 tree annotator boxes them (write-embedded).
test_transpile("W12: chained assignment writes the inner var",
    'my $y = 0; my $x = 0; $x = $y = 5; print "y=", $y+1, " x=", $x+1, "\n";');
test_transpile("W12: write inside do-block reaches the outer var",
    'my $x = 0; my $z = do { $x = 5 }; print "x=", $x+1, " z=", $z+1, "\n";');
test_transpile("W12: write inside map-block reaches the outer var",
    'my $y = 3; my $x = 0; map { $x = $y * 2 } (1,2,3); print "x=", $x+1, "\n";');
# tie attaches magic to the box: a tied my-scalar must never unbox, even when
# the only visible write is a shape-clean `$x = literal` (STORE doubles it).
test_transpile("W12: tie on a pre-declared my scalar keeps FETCH/STORE magic",
    'package Counter; sub TIESCALAR { my $c = shift; my $v = 0; bless \$v, $c }'
  . ' sub FETCH { my $s = shift; $$s } sub STORE { my ($s, $v) = @_; $$s = $v * 2 }'
  . ' package main; my $x; tie $x, "Counter"; $x = 21; print "$x\n";');
# The tree annotator's analysis parses run BEFORE `use constant` registers the
# constant; PExpr marked the then-unknown bareword `_bareword_string` ON the
# shared PPI token, and the stale flag made the real parse emit the constant
# call as (pl-"nought") — an undefined-function crash (found via split.t).
test_transpile("W12: constant used after use-constant survives analysis parses",
    'use constant nought => 0; my $w = nought; my $z = nought + 1;'
  . ' print "w=", $w+1, " z=$z\n";');

like(transpile_only('local $x = 5; my $y = $x + 1; print $y;'),
     qr/\(declare \(notinline pcl::p-\+/,
     'top-level local emits notinline declare (R1 OOM guard)');
unlike(transpile_only('sub f { local $x = 5; my $y = $x + 1; return $y; } print f();'),
       qr/\(declare \(notinline pcl::p-\+/,
       'local inside a sub keeps inlining (no notinline declare)');

# Session 280: shadow-aware capture gates + multi-scalar capture promotion.
# A nested named sub whose $a is its OWN `my` must not false-positive the
# capture gate against the outer file lexicals $a/@a (wantarray.t's inline).
test_transpile("capture: sub-local my shadows outer file lexical",
    'my $a = 5; my @a = (9); { sub inline2 { my $a = 7; return $a; } }'
  . ' print inline2(), " ", $a, "\n";');
# The RHS of the shadowing decl still sees the OUTER variable — that IS a
# capture.  s284 (M-C): the shadow-aware promotion lowers this NATIVELY and
# CORRECTLY — the outer $x promotes to a cell, the sub's shadow keeps its own
# let, and the shadow's RHS follows the rename to the outer cell.  perl
# prints 6; the old "gates to v1" expectation preserved v1's pre-existing
# MISCOMPILE (it printed 1).
test_transpile("capture: shadowing decl RHS reads the outer (promoted) lexical",
    'my $x = 5; { sub g2 { my $x = $x + 1; return $x } } print g2(), "\n";');
# push.t idiom: multi-scalar list decl captured by a named sub, with the
# scalars ALSO read via interpolation (the interp rewrite must follow).
test_transpile("capture: multi-scalar list decl captured by named sub",
    '{ my ($first, $second) = ([1], [2]);'
  . ' sub two_things2 { return +($first, $second) }'
  . ' push @{ two_things2() }, 3;'
  . ' print join(":", @$first), " [@$second]", "\n"; }');
# A promoted captured scalar interpolated in qr// follows the rename
# (QuoteLike::Regexp is covered by the interpolation rewriter).
test_transpile("capture: renamed scalar followed into qr// interpolation",
    '{ my $pat = "ab+c";'
  . ' sub matches2 { my $s = shift; return "u" unless defined $pat;'
  . '   return $s =~ qr/$pat/ ? "y" : "n"; }'
  . ' print matches2("xabbc"), matches2("xac"), "\n"; }');
# @a/@b sat in _forward_global_decls' runtime-owned exclusion but nothing
# defines them (only $a/$b are; v1's list never had them): package @a taken
# by reference BEFORE any assignment was unbound at load (postfixderef.t).
test_transpile("forward decl: ref to package \@a before any assignment",
    'no strict; $r = \@a; push @$r, 7; print "@a\n";');
# Interpolated postfix deref emits literal text in the v2 string lowering —
# must gate the whole file to v1 (postfixderef.t cascade guard).
like(transpile_only('use feature q(postderef_qq); my $r = [1,2,3]; print "$r->@*\n";'),
     qr/pipeline=v1/,
     'interpolated postfix deref gates the file to v1');

# E1.1 / W10-ext-3 (s282): containers (%h/@a) spanning a package boundary are
# span-renamed to a defvar package cell (was a whole-file v1 gate).
test_transpile("container spanning: %h element access from another package", '
my %h;
{ package X; sub g { $h{a} } }
$h{a} = 5;
print X::g(), "\n";
');
test_transpile('container spanning: @list whole/last-index across a package boundary', '
my @list;
push @list, "a", "b";
{ package Y; sub n { $#list + scalar(@list) } }
print Y::n(), "\n";
');

# PPI splits `sub main::::flomp` into two Word tokens; v2 read only the first
# (emitted unreadable pl-main::, aborting the section — method.t test 122).
test_transpile("sub main::::flomp — PPI-split name reassembled", '
sub flomp { "flimp" }
sub main::::flomp { "flump" }
print "::"->flomp, "-", "::main"->flomp, "\n";
');

# Guard edges of the container-spanning rename (each `next` in the loop):
# an edge the rename must REFUSE still runs correctly via the v1 fallback.
test_transpile('container spanning guard: interpolated "@list" refuses rename, stays correct', '
my @ilist = (1,2,3);
{ package EI; sub s1 { "@ilist" } }
print "interp: ", EI::s1(), "\n";
');
test_transpile('container spanning guard: $mix scalar + %mix hash share the bare name — refused', '
my $mix = "s";
my %mix = (k => "h");
{ package E5; sub g5 { $mix . $mix{k} } }
print "mix: ", E5::g5(), "\n";
');
test_transpile("container spanning: hash and array SLICES across the boundary", '
my %sh = (a => 1, b => 2);
my @sa = (10, 20, 30);
{ package E6; sub g6 { my @v = @sh{qw(a b)}; my @w = @sa[0,2]; "@v|@w" } }
print "slice: ", E6::g6(), "\n";
');

# Forward declaration with a PPI-split name must merge too.
test_transpile("sub main::::fwd; forward decl then definition", '
sub main::::fwd;
sub main::::fwd { "F" }
print "fwd: ", "::"->fwd, "\n";
');

# Indirect-object SUPER block forms (method.t 120-122): block list and
# trailing LIST concatenate; first element of the combined list = invocant.
test_transpile("SUPER::m indirect block forms incl. trailing LIST", '
package egakacp {
  our @ISA = q(ASI);
  sub ASI::m { shift; "@_" };
  my @a = (bless([]), q(arg));
  my $r = SUPER::m{@a};
  print "r1=$r\n";
  $r = SUPER::m{}@a;
  print "r2=$r\n";
  $r = SUPER::m{@a}"b";
  print "r3=$r\n";
}
');
test_transpile("SUPER indirect: scalar-invocant block + multi-element trailing LIST", '
package ASI2; sub m2 { shift; join("+", @_) }
package egak2; our @ISA = "ASI2";
my @a = (bless([], "egak2"), "z");
my $r = SUPER::m2{$a[0]} "x", "y";
print "multi: $r\n";
');

# foreach over a SINGLE range lowers to the counting-loop macro
# p-foreach-range(-raw) — endpoints evaluated once, numeric ranges never
# materialize the vector (s286, docs/bench-exec-investigation.md).  These
# cover the semantic edges of the new path; emission guards in parser2-01.t.
test_transpile("range foreach: sum + loop control", '
my $s = 0;
for my $i (1..10) { next if $i % 2; last if $i > 8; $s += $i }
print "$s\n";
');
test_transpile("range foreach: labeled next/last from inner loop", '
OUTER: for my $i (1..3) {
  for my $j (1..3) { next OUTER if $j == 2; print "$i$j " }
}
print "\n";
');
test_transpile("range foreach: continue block runs per iteration", '
my $log = "";
for my $i (1..3) { $log .= "b$i " } continue { $log .= "c$i " }
print "$log\n";
');
test_transpile("range foreach: endpoint side effect evaluated once", '
my $calls = 0;
sub f { $calls++; return 3 }
my $n = 0;
for my $i (1..f()) { $n++ }
print "n=$n calls=$calls\n";
');
test_transpile("range foreach: closure captures per-iteration value", '
my @subs;
for my $i (1..3) { push @subs, sub { $i } }
print join(",", map { $_->() } @subs), "\n";
');
test_transpile("range foreach: magic string range falls back at runtime", '
my $out = "";
for my $c ("ay".."bc") { $out .= "$c " }
print "$out\n";
');
test_transpile("range foreach: reversed bounds iterate zero times", '
my $n = 0;
for my $i (5..1) { $n++ }
print "n=$n\n";
');
test_transpile("range foreach: float endpoints truncate like perl", '
my $out = "";
for my $i (1.7..4.2) { $out .= "$i " }
print "$out\n";
');
test_transpile("range foreach: \$_ form with regex on \$_ stays boxed", '
my $out = "";
for (1..3) { $out .= $_ }
print "$out\n";
');
test_transpile("reverse range still iterates descending (not split)", '
my $out = "";
for my $i (reverse 1..4) { $out .= "$i " }
print "$out\n";
');
test_transpile("range foreach: \\\$i in body reads through (var stays boxed)", '
for my $i (1..2) { my $r = \$i; print $$r, " " }
print "\n";
');

done_testing();

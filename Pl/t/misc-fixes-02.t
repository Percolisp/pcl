#!/usr/bin/env perl
# misc-fixes-02.t - Continuation of misc-fixes-01.t (which became the largest .t).
# Operator-semantics bugs surfaced by the differential fuzzer (tools/difftest-ops.pl).

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

plan tests => 16;

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

# ── difftest-ops fuzzer finds (session 241): operator bugs perl-tests/ missed ──

# [10] Chained string comparison crashed: the chained-comparison expander mapped
# `eq`/`ne`/`lt`/... to a non-existent p-eq (only the numeric p-== family exists);
# string ops live in the p-str-* family.  `'2' eq '10' eq '3'` → both compares
# false → "" (perl prints empty).  Was: UNDEFINED-FUNCTION PCL::P-EQ at load.
test_cl('chained string comparison (eq/lt) does not crash',
    'my $r = "2" eq "10" eq "3"; $r = "F" unless $r; print "[$r]\n";'
    . 'my $s = "a" lt "b" lt "c"; print "[$s]\n";',
    "[F]\n[1]\n");

# [9] Numeric bitwise & | ^ treat operands as UNSIGNED 64-bit in Perl, so a
# negative operand wraps to its two's-complement value.  `(2-3) | 4` = -1|4 =
# 2**64-1.  PCL was doing signed CL logior → -1.
test_cl('numeric bitwise | ^ treat operands as unsigned 64-bit',
    'my $a = (2 - 3) | 4; my $b = (2 - 3) ^ 4; print "$a $b\n";',
    "18446744073709551615 18446744073709551611\n");

# [2] Relational < > <= >= bind TIGHTER than equality == != eq ne, so
# `2 != 3 > 4` is `2 != (3 > 4)` = `2 != "" ` = 1.  The chained-comparison
# left-scan was crossing precedence tiers and chaining `(2!=3) && (3>4)` = "".
test_cl('relational binds tighter than equality (no cross-tier chaining)',
    'my $a = 2 != 3 > 4; my $b = 2 != 3 >= 4; print "[$a][$b]\n";',
    "[1][1]\n");

# Braced block-deref + subscript: PPI tags the `[...]` after a `${BLOCK}`/`@{BLOCK}`
# as a Constructor (anon array) not a Subscript, so `${$ar}[1]` / `@{$ar}[0,2]`
# fell through to the "Missing case" die and silently became undef.  The arrow
# (`$ar->[1]`), $$ (`$$ar[1]`), and hash (`${$hr}{a}`) forms always worked.
# Found by tools/difftest-ops.pl deref axis (session 241).
test_cl('braced array block-deref with index/slice subscript',
    'my @a=(10,20,30); my $ar=\@a;'
    . ' my $x = ${$ar}[1]; my @s = @{$ar}[0,2];'
    . ' print "[$x][@s]\n";',
    "[20][10 30]\n");

# Float stringification must match Perl's %.15g (15 significant digits, then
# strip trailing zeros), NOT SBCL's shortest-round-trip form.  `0.1+0.2` is the
# canonical case: perl prints 0.3, PCL was printing 0.30000000000000004.
# Found by tools/difftest-ops.pl numeric axis (session 242).  Covers the
# fixed-notation branch (0.1+0.2, 1/3, 2**0.5) and the exponential branch
# (the 1e+16 rounding-bump) of stringify-value.
test_cl('float stringification matches Perl %.15g',
    'print "[", 0.1+0.2, "][", 1/3, "][", 2**0.5, "][",'
    . ' 9.999999999999999e15, "]\n";',
    "[0.3][0.333333333333333][1.4142135623731][1e+16]\n");

# Compound /= and **= leaked a CL ratio: `$x /= 2` gave "7/2" and `$x **= -1`
# gave "1/2" instead of Perl's 3.5 / 0.5.  The macros divided/exponentiated
# raw (CL int/int -> ratio) while plain p-/ and p-** coerce ratio -> float.
# Fixed by delegating p-/= -> p-/ and p-**= -> p-**.  Found by the difftest-ops
# compound-assignment axis (session 242).
test_cl('compound /= and **= coerce ratio to float like Perl',
    'my $a=7; $a /= 2; my $b=2; $b **= -1;'
    . ' my $c=10; $c /= 4; print "[$a][$b][$c]\n";',
    "[3.5][0.5][2.5]\n");

# Array/hash SLICE inside string interpolation must join its elements with $"
# (list context), regardless of the context the string is used in.  Bug: a
# single-slice string "@a[1..2]" bypassed the string_concat/join wrapper, and
# even wrapped slices inherited the outer scalar context and reduced to the
# LAST element (`my $s = "@a[1..2]"` gave "3" not "2 3"; mid-string gave "x3y").
# Whole-array "@a" interpolation was always fine.  Found by the difftest-ops
# slice axis (session 242).
test_cl('array/hash slice in string interpolation joins with $" (list ctx)',
    'my @a=(1,2,3,4,5); my %h=(a=>1,b=>2,c=>3);'
    . ' my $s = "@a[1..2]"; my $t = "x@a[-2,-1]y"; my $u = "@h{qw(a c)}";'
    . ' print "[$s][$t][$u]\n";',
    "[2 3][x4 5y][1 3]\n");

# Anonymous array constructor [ ... ] evaluates its contents in LIST context and
# is never the enclosing sub's tail call.  Bug: a wantarray-sensitive builtin
# inside [...] (e.g. reverse) skipped its (let ((*wantarray* t)) ...) wrapper in
# tail position, so a scalar context leaked in — `do { ...; "@{[reverse @a]}" }`
# reversed the joined string ("123" -> "321") instead of the list (3,2,1).
# Found by the difftest-ops babycart axis (session 242).  sort/map (always-list)
# were unaffected; reverse exposed it.
test_cl('anon arrayref [ ] forces list context on contents (reverse in do-tail)',
    'my $r = do { my @a=(1,2,3); "@{[reverse @a]}" };'
    . ' my $d = "@{[reverse 1,2,3]}";'
    . ' sub f { return [ reverse @_ ] } my $t = "@{f(7,8,9)}";'
    . ' print "[$r][$d][$t]\n";',
    "[3 2 1][3 2 1][9 8 7]\n");

# Postfix conditional whose condition starts with a parenthesised group followed
# by an operator — `return X if (A) || (B)` — made PPI mislabel the leading `(A)`
# as a Structure::Condition (its `if (...)` bracket type) instead of a
# Structure::List, which PExpr's parse() did not handle ("unknown type
# PPI::Structure::Condition" -> the whole sub failed to transpile).  Now a
# Structure::Condition in expression position is parsed as a parenthesised expr,
# like Structure::List.  Found in Math::BigInt via the CPAN test-suite survey.
test_cl('postfix if with leading parenthesised condition (A) || (B)',
    'sub f { my ($a,$b)=@_; return "yes" if ($a == 1) || ($b == 2); return "no" }'
    . ' print "[", f(1,9), "][", f(9,2), "][", f(9,9), "]\n";',
    "[yes][yes][no]\n");

# List::Util block-prototype functions (first/any/all/none/reduce, pair*) must
# parse the `{ BLOCK } LIST` form like grep/map.  Two bugs (session 244):
# (1) List::Util was in _extract_module_prototypes's skip-list, so the shim's
#     `sub first (&@)` prototype was never read -> "Missing case: [" parse error.
#     Fixed by declaring the (&@) prototypes in lib/List/Util.pm (the data) and
#     un-skipping List::Util (the generic mechanism reads the prototype).
# (2) The slurpy @ tail of (&@) must force LIST context on the list args, else a
#     scalar-context call (my $x = first {...} (1,2,3,4)) collapsed the paren
#     list to its last element.  reduce/pair* set the CALLER's $a/$b by symbolic
#     ref in the shim (lib/List/Util.pm) — $a/$b are package-scoped, so the block
#     {$a+$b}, compiled in the caller, reads the caller's globals (no parser hack).
test_cl('List::Util first/reduce block form parses and respects list context',
    'use List::Util qw(first reduce);'
    . ' my $f = first { $_ > 2 } (1,2,3,4);'
    . ' my $r = reduce { $a + $b } 1,2,3,4;'
    . ' print "[$f][$r]\n";',
    "[3][10]\n");

# A 'my @arr'/'my %hash' captured by a closure is renamed to a let-bound LEXICAL
# (@a__lex__N) so the closure sees per-instance state.  Bug (session 244): the
# init went through p-my-= (box-set), which is a NO-OP on a non-box array/hash
# place — so the captured aggregate was never populated.  Whole-aggregate reads
# (scalar(@a), keys %h, "@a") saw an empty array; element reads ($a[0]) happened
# to work only when the var dodged the rename.  Fix: fill the (already adjustable)
# lexical in place via p-array-fill / p-hash-fill (extracted from p-array-=/p-hash-=
# without their proclaim-special guard), and force LIST context on the RHS.
test_cl('closure capturing a my-array/my-hash populates the lexical aggregate',
    'my $r = do { my @a=(1,2,3); my $f=sub { my $s=0; $s+=$_ for @a; "@a|$s|".scalar(@a) }; $f->() };'
    . ' my $h = do { my %h=(a=>1,b=>2); my $g=sub { $h{c}=3; join(",",sort keys %h) }; $g->() };'
    . ' print "[$r][$h]\n";',
    "[1 2 3|6|3][a,b,c]\n");

# sprintf %.Nf must round half-to-EVEN (C/Perl printf), not half-away-from-zero.
# Bug (session 244, found by the fuzzer numeric axis): SBCL's ~F (and scaling a
# float before ROUND) rounds 2.5->3, 0.5->1; C/Perl give 2 and 0.  Fixed by
# rounding the EXACT rational of the double with CL ROUND (itself half-to-even).
test_cl('sprintf %.Nf rounds half-to-even like C/Perl printf',
    'printf "%.0f %.0f %.0f %.0f|%.2f|%.0f\n", 2.5, 3.5, 0.5, 1.5, 9.999, -2.5;',
    "2 4 0 2|10.00|-2\n");

# In-memory string filehandles: open my $fh, MODE, \$scalar.  Writes append into
# the scalar live (a p-string-output-stream Gray stream over the box's adjustable
# string); reads come from a string-input-stream.  Previously open got the scalar
# ref stringified to a junk filename and the scalar stayed empty.  Found by the
# fuzzer special-variable axis (session 244).
test_cl('in-memory string filehandles (open my $fh, ">", \\$scalar)',
    'my $w=""; open my $o,">",\$w; print {$o} "a","b"; printf {$o} "%d",7; close $o;'
    . ' my $a="Z"; open my $ap,">>",\$a; print {$ap} "!"; close $ap;'
    . ' my $d="p\nq\nr\n"; open my $i,"<",\$d; my $n=0; while(<$i>){$n++} close $i;'
    . ' print "[$w][$a][$n]\n";',
    "[ab7][Z!][3]\n");

# $, (output field separator) is printed between print's arguments; $\ (output
# record separator) after.  Bug (session 244, fuzzer special-var axis): p-print
# honored $\ but ignored $,.  `local $,=","; print 1,2,3` should give "1,2,3".
test_cl('print honors $, (output field separator) between arguments',
    'local $, = ","; local $\\ = "!\n"; print 1,2,3;',
    "1,2,3!\n");

# my @a = <$fh> must read ALL records (list context), not one.  Bug (session 244):
# p-array-= did not bind *wantarray* t, so the readline RHS saw the ambient scalar
# context and read a single line.  Array-assignment RHS is always list context.
test_cl('my @lines = <$fh> reads all records (list context)',
    'my $d = "a\nb\nc\n"; open my $fh, "<", \$d; my @lines = <$fh>; close $fh;'
    . ' my $one; open my $g, "<", \$d; $one = <$g>; close $g; chomp $one;'
    . ' print scalar(@lines), "|$one\n";',
    "3|a\n");

# printf/print take a LIST: a bare array arg must flatten so the format comes
# from the first element.  Bug (from Perl t/io/print.t, session 244): p-printf
# grabbed the whole @a vector as the format -> "ARRAY(0x..)".  p-print already
# flattened; p-printf now does too.
test_cl('printf @a flattens the array (format from first element)',
    'my @a = ("%s=%d\n", "x", 5); printf @a; printf STDOUT @a;',
    "x=5\nx=5\n");

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

plan tests => 92;

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

# Transpile only — return the generated CL string (for codegen-shape assertions).
sub transpile_to_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return scalar `$pl2cl $pl_file 2>/dev/null`;
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

# Symbolic-ref array slice (session 245).  @{NAME}[i,j] under no-strict-refs is a
# symbolic reference: NAME (a string, or any expr giving one) names a package
# array.  PExpr now builds a slice_a_acc node whenever a Cast('@') + Block is
# followed by a TRAILING subscript, whatever the block contains (the subscript's
# position disambiguates slice-vs-deref at parse time — see
# docs/symbolic-ref-slice-parse-fix.md).  Every spelling compiles to the SAME
# one path (p-aslice), whose p-aref resolves ref-vs-string at runtime (was:
# (setf p-aref) into the string's chars → CHARACTER type-error for $scalar,
# inverted (p-cast-@ (p-aref-box ..)) crash for literal/expr).
# Write via one spelling, read via another.
test_cl('symbolic-ref array slice: scalar / literal / computed expr all agree',
    'no strict;'
    . ' my $f = "marr"; @{$f}[1,2] = ("A","B");'          # write via $scalar
    . ' @{"marr"}[3] = "C";'                               # write via literal
    . ' @{"m"."arr"}[4,5] = ("D","E");'                    # write via expr
    . ' print join(",", map { defined $_ ? $_ : "u" } @{"marr"}[0,1,2,3,4,5]), "\n";',
    "u,A,B,C,D,E\n");

# Same symbolic slice with a VARIABLE / computed index (single-subscript form):
# the subscript is a bare expression, not a comma list, so it exercises the
# single-child (non-progn) path of the slice node — @{EXPR}[$i] = ... must work
# for any deref spelling.
test_cl('symbolic-ref array slice with a variable/computed index',
    'no strict; my $f = "narr"; my $i = 2;'
    . ' @{$f}[$i] = "X"; @{"narr"}[$i+1] = "Y"; @{"n"."arr"}[$i+2] = "Z";'
    . ' print join(",", map { defined $_ ? $_ : "u" } @{"narr"}[0,1,2,3,4]), "\n";',
    "u,u,X,Y,Z\n");

# Symbolic-ref HASH slice (fuzzer axis-22 find, session 245): @{EXPR}{keys} is a
# hash value-slice (the @ sigil + {} subscript), %{EXPR}{keys} a key/value slice.
# The literal/computed-expr deref forms used to invert to (p-cast-@ (p-gethash-
# box ..)) and crash (CL-ERROR); the parse-time slice rule routes them to
# (p-hslice / p-kv-hslice EXPR keys), the same path @{$scalar}{..} already used.
test_cl('symbolic-ref hash slice: @{}=values, %{}=key/value, all spellings',
    'no strict; %hh=(a=>1,b=>2,c=>3);'
    . ' my $n="hh";'
    . ' print join(",", @{$n}{qw(a c)}), "|",'        # via scalar  -> 1,3
    . '       join(",", @{"hh"}{qw(a c)}), "|",'      # via literal -> 1,3
    . '       join(",", @{"h"."h"}{qw(a c)}), "|",'   # via expr    -> 1,3
    . '       join(",", %{"hh"}{qw(a c)}), "\n";',    # kv-slice    -> a,1,c,3
    "1,3|1,3|1,3|a,1,c,3\n");

# GUARD for the session-245 regression: @{$a[0]} / @{$h{k}} / @{$obj->{x}} are
# DEREFS of a container element (the subscript is INSIDE the braces) and must
# stay plain casts — the codegen rewrite of 2bc25da turned them into slices
# because both compile to the same (p-cast-@ (p-aref-box ..)) string shape
# (scalar @{$a[0]} gave 1, not 3).  The parse-time rule keys on a subscript
# AFTER the block, so these never match; the last case (@{$h{a}}[0,2]) has
# BOTH — an inner element subscript and a trailing slice — and is a genuine
# slice of the deref'd element.
test_cl('deref of container element stays a deref; trailing subscript slices it',
    'my @a=([1,2,3]); my %h=(a=>[4,5,6]); my $o={x=>[7,8,9]};'
    . ' print scalar(@{$a[0]}), scalar(@{$h{a}}), scalar(@{$o->{x}}), "|",'
    . '       join(",", @{$h{a}}[0,2]), "\n";',
    "333|4,6\n");

# ── Moo-driven fixes (session 247) ──────────────────────────────────────────

# Closure-shadow rename bug: an anon sub that re-declares `my $c` with the SAME
# name as the outer var the sub is being assigned to (Moo install_delayed:
# my $c = defer_sub ... sub { my $c = gen(); $c }).  The closure-capture
# renamer renamed the inner decl's ASSIGNMENT target to the outer __lex__ var
# while the inner body read its own plain let binding -> undef.  Fixed in
# _process_variable_statement (rename path strips the var from
# _current_scope_new_renames for the RHS parse) + _with_declarations (shadowed
# my-vars drop outer renames for the body).
test_cl('my $c = sub { my $c = ...; $c } shadow returns inner value',
    'sub inner { sub { "INNER" } }'
    . ' sub mf { my ($p) = @_; my $c = sub { my $c = inner($p); $c }; return $c }'
    . ' my $made = mf("X")->();'
    . ' print defined $made ? "ok ".$made->() : "UNDEF", "\n";'
    . ' my $o = 5; my $bump = sub { $o + 1 };'
    . ' my $f = sub { my $o = inner(); $o };'
    . ' print $bump->(), " ", $f->()->(), "\n";',
    "ok INNER\n6 INNER\n");

# Stash delete write-through: delete $Pkg::{name} (via \%{"Pkg::"}) must really
# remove the sub so *{Pkg::name}{CODE}, defined &, and ->can stop seeing it.
# Moo's Method::Generate::Constructor bootstrap `sub new` deletes itself from
# the stash on first call; the old read-only-snapshot p-stash lost the delete
# and assert_constructor croaked "Unknown constructor ... already exists".
test_cl('delete from package stash removes the sub (write-through)',
    'no strict "refs"; package Foo; sub hello { "hi" } package main;'
    . ' my $st = \%{"Foo::"};'
    . ' print defined &Foo::hello ? "y":"n", exists $st->{hello} ? "y":"n";'
    . ' delete $st->{hello};'
    # NB: perl keeps `defined &Foo::hello` true here (the compiled reference
    # pins the glob); only the live ->can lookup sees the deletion.
    . ' print defined &Foo::hello ? "y":"n", Foo->can("hello") ? "y":"n", "\n";',
    "yyyn\n");

# Typeglob deref slots: @{*{globref}} (and %{...}) must resolve to the glob's
# ARRAY/HASH slot, live and lvalue-capable.  Moo's _set_superclasses does
# @{*{_getglob("${target}::ISA")}} = @_ — the write was silently lost, so
# `extends` never changed @ISA.
test_cl('@{*{globref}} reads and writes the glob ARRAY slot',
    'no strict "refs"; @Bar::ISA = ("X");'
    . ' sub _gg { no strict "refs"; \*{$_[0]} }'
    . ' @{*{_gg("Bar::ISA")}} = ("Foo");'
    . ' print "(@Bar::ISA)(@{*{_gg(q(Bar::ISA))}})\n";',
    "(Foo)(Foo)\n");

# local $h{k} = {} must store the init with ordinary assignment box shape:
# %p-lhe-init used raw make-p-box, so the localized hashref defeated
# p-autoviv-gethash's unboxing — a later nested write clobbered the elem with
# a RAW hash and scalar reads returned the COUNT (Moo: local $self->{captures}
# = {} then $self->{captures}{$k} = \$v in generate_method).
test_cl('local hash-elem init holds a hashref usable by nested writes',
    'my $self = { captures => undef };'
    . ' sub fill { my $s = shift; $s->{captures}{q($x)} = \42; }'
    . ' { local $self->{captures} = {}; fill($self); my $c = $self->{captures};'
    . '   print ref($c), " ", scalar(keys %$c), " ", ${$c->{q($x)}}, "\n"; }'
    . ' print defined $self->{captures} ? "kept" : "restored", "\n";',
    "HASH 1 42\nrestored\n");

# ── List flattening in return (session 248) ─────────────────────────────────

# p-return's list-context multi-value branch built (vector v1 v2 ...) without
# splicing array-valued elements, so `return ($i, map ...)` handed the caller
# a NESTED vector — join() stringified it as ARRAY(0x...).  List assignment
# happened to deep-flatten, hiding the bug.  Now flattened via p-flatten-args
# (raw vectors/hashes spread; boxes/refs/blessed stay intact).
test_cl('return ($i, map ...) flattens; refs in returned list stay refs',
    'sub f { my $i = 5; my @subs = (sub { 6 }, sub { 6 });'
    . '   return ($i, map { $_->() } @subs); }'
    . ' sub g { my $i = 1; return ($i, [2,3], ()); }'
    . ' print join(",", f()), "|";'
    . ' my @g = g(); print scalar(@g), ",", $g[1][1], "\n";',
    "5,6,6|2,3\n");

# gen_tree_val emitted a bare (progn ...) for a multi-value parenthesized list
# in non-list static context, so a ternary branch inside return — context only
# known at runtime — ran as the comma OPERATOR and dropped all but the last
# value.  Now it dispatches on *wantarray* like gen_progn does.
test_cl('parenthesized list in ternary return: list vs scalar by context',
    'sub t { my @a = (7,8); return wantarray ? (0, @a) : -1 }'
    . ' sub u { my $i = 5; my @s = (sub { 6 });'
    . '   return wantarray ? ($i, map { $_->() } @s) : 0 }'
    . ' print join(",", t()), ",", scalar(t()), "|", join(",", u()), "\n";',
    "0,7,8,-1|5,6\n");

# box-set class rules (substr.t #378/#383 regressions, session 248):
# (1) overwriting a box that held a blessed REF with a plain value clears the
#     stale class (else overloaded "" keeps firing on the new string and
#     p-aref's symbolic-ref arm turns it into undef);
# (2) a blessed object assigned into a substr lvalue reaches the magic-cell
#     setter still boxed, so its "" overload is used (exactly once);
# (3) a blessed scalar REFERENT keeps its class on assignment (Perl SV stash).
test_cl('box-set class: clear on ref-holder overwrite, keep on referent write',
    'package o { use overload q("") => sub { ++our $count; $_[0][0] } }'
    . ' my $refee = bless ["Za"], "o";'
    . ' my $substr = \substr $refee, -2; $$substr = "b";'
    . ' print "[$refee]", (ref($refee) eq "" ? "plain" : "CLASSY"), "|";'
    . ' my $t = ""; $o::count = 0;'
    . ' ${\substr $t, 0} = bless ["X"], "o";'
    . ' print "[$t]$o::count|";'
    . ' my $x = 1; my $r = bless \$x, "C"; $x = 5;'
    . ' print ref($r), "\n";',
    "[b]plain|[X]1|C\n");

# ── Moo bootstrap fixes (session 248b) ──────────────────────────────────────

# Named sub inside a block must close over the block's `my` lexicals.  The
# closure-capture renamer skipped named subs, so the block's let of the
# defvar'd name dynamically shadowed the global the defun read — the sub saw
# nil.  This is Sub::Quote's eval'd shape: { my $default_for_b = ...;
# sub new { ... $default_for_b->($new) ... } } — the Moo coderef-default wall.
test_cl('named sub in a block closes over the block lexical',
    '{ my $x = sub { "OK" }; sub callit { $x->() } }'
    . ' print callit(), "|";'
    . ' { my $n = 41; sub bump { $n + 1 } }'
    . ' print bump(), "\n";',
    "OK|42\n");

# SUPER:: through a multi-segment parent package: p-super-call did
# (find-package (string-upcase ...)), which misses case-preserved
# |My::Base|-style packages — the walk dead-ended with "No SUPER::new found"
# (Moo: Animal's SUPER::new -> Moo::Object::new).  Now uses %pcl-find-package
# like the rest of dispatch.
test_cl('SUPER:: resolves through multi-segment package names',
    'package My::Base; sub new { my $c = shift; bless {ok=>1}, (ref($c)||$c) }'
    . ' package Kid::Sub; our @ISA = ("My::Base");'
    . ' sub new { my $c = shift; my $o = $c->SUPER::new; $o->{kid}=1; $o }'
    . ' package main;'
    . ' my $o = Kid::Sub->new;'
    . ' print $o->{ok}, $o->{kid}, " ", ref($o), "\n";',
    "11 Kid::Sub\n");

# ── session 249: unblocking Moo's lazy/subclass constructor bootstrap ──

# `defined &Pkg::sub` for a MULTI-SEGMENT package: p-sub-defined did
# (find-package (string-upcase pkg)), which can't see a case-preserved
# |Sub::Util|-style CL package, so it always reported false.  Sub::Defer's
# BEGIN gate `defined &Sub::Util::set_subname` then mis-detected _CAN_SUBNAME.
# Now resolves via %pcl-find-package.  (Single-segment was already fine.)
test_cl('defined &Pkg::sub works for multi-segment package names',
    'package Sub::Util; sub set_subname { $_[1] }'
    . ' package main;'
    . ' print((defined &Sub::Util::set_subname ? 1 : 0),'
    . '       (defined &Sub::Util::missing ? 1 : 0), "\n");',
    "10\n");

# Coderef identity must be STABLE across allocation/GC.  object-address used
# the raw moving pointer, so a coderef stringified into a hash key (CODE(0x..))
# could diverge after the GC relocated it -> hash lookup miss (broke
# Sub::Defer's %DEFERRED).  A coderef key must still be found after heavy
# allocation, and refaddr must be invariant.
test_cl('coderef hash key is stable across allocation (object identity)',
    'my %h; my $c = sub { 1 }; $h{$c} = "v";'
    . ' my @j; for (1..50000) { push @j, [$_, {a=>$_}] } @j = ();'
    . ' print((exists $h{$c} ? "HIT" : "MISS"), "\n");',
    "HIT\n");

test_cl('stringified ref identity is invariant across allocation',
    'my $r = {}; my $s1 = "$r";'
    . ' my @j; for (1..50000) { push @j, [$_] } @j = ();'
    . ' my $s2 = "$r";'
    . ' print(($s1 eq $s2 ? "SAME" : "MOVED"), "\n");',
    "SAME\n");

# refaddr: implemented via ref-numification over the stable object identity.
# Distinct refs differ; the same ref is invariant across allocation; a non-ref
# is undef; and refaddr agrees with the hex of the stringified ref.
test_cl('refaddr: distinct/stable/undef + agrees with stringification',
    'use Scalar::Util qw(refaddr);'
    . ' my $r = {}; my $r2 = {};'
    . ' my $a = refaddr($r);'
    . ' my @j; for (1..50000) { push @j, [$_] } @j = ();'
    . ' my $hex = ("$r" =~ /0x([0-9a-f]+)/) ? hex($1) : -1;'
    . ' print(($a == refaddr($r) ? "S" : "M"),'
    . '       (refaddr($r) != refaddr($r2) ? "D" : "d"),'
    . '       (defined refaddr(5) ? "x" : "u"),'
    . '       ($a == $hex ? "H" : "h"), "\n");',
    "SDuH\n");

# refaddr of a non-ref is undef — including numeric/hex-looking STRINGS, which
# must NOT be numified (the `ref` guard short-circuits before `0 + $r`).
test_cl('refaddr returns undef for non-ref scalars (strings included)',
    'use Scalar::Util qw(refaddr);'
    . ' print((map { defined refaddr($_) ? "x" : "u" }'
    . '            ("hello", "42", "0xff", 7, undef)), "\n");',
    "uuuuu\n");

# ── Case-sensitive identifiers must not collide on the CL reader ──
# Perl is case-sensitive; PCL reads its generated code under (readtable-case
# :invert) so $base_len and $BASE_LEN map to DISTINCT CL symbols (without it the
# lexical would shadow the file-`my`, which is what broke Math::BigInt::Calc).
# More case-sensitivity regression coverage lives in Pl/t/case-invert-01.t.
test_cl('case-colliding scalars stay distinct (plain refs)',
    'my $base_len = 1; my $BASE_LEN = 2;'
    . ' print "$base_len $BASE_LEN\n";',
    "1 2\n");

# The collision also has to survive string interpolation (interpolation builds
# fresh Symbol nodes from raw text, a separate code path from the PPI mutation).
test_cl('case-colliding scalars stay distinct (interpolation + reassign)',
    'my $val = 10; my $VAL = 20;'
    . ' $val += 5; $VAL += 5;'
    . ' print "$val/$VAL\n";',
    "15/25\n");

# The Math::BigInt::Calc shape: a file-scoped `my` written from inside a sub
# whose lexical param differs only in case. The file-`my` must keep the value
# after the sub returns.
test_cl('file-my written via same-case-collision lexical param persists',
    'my $WIDTH;'
    . ' sub setw { my ($width) = @_; $WIDTH = $width; }'
    . ' setw(9);'
    . ' print "WIDTH=$WIDTH\n";',
    "WIDTH=9\n");

# Non-colliding code is left completely untouched (common path unchanged).
test_cl('no rename when there is no case collision',
    'my $count = 3; my $total = $count * 2;'
    . ' print "$count $total\n";',
    "3 6\n");

# ── &NAME(...) calls the USER sub, even over a builtin (Perl's & sigil) ──
# A user `sub length` is reachable only via &length(...); the bareword still
# calls the builtin. (The real-world case: `sub connect` imported into main::
# called as `&connect()`.)
test_cl('&NAME(args) calls user sub overriding a builtin',
    'sub length { return "U:@_" }'
    . ' print &length("ab"), "|", length("ab"), "\n";',
    "U:ab|2\n");

# Empty parens: &NAME() passes an empty list (not the caller @_).
test_cl('&NAME() empty parens calls user sub with no args',
    'sub connect { return "C:@_" } print &connect(), "\n";',
    "C:\n");

# &NAME(args) followed by more of the expression must keep those elements
# (regression guard for the funcall splice).
test_cl('&NAME(args) in a larger expression keeps trailing tokens',
    'sub ucfirst { return "U:@_" } print &ucfirst("a") . "Z", "\n";',
    "U:aZ\n");

# Package-qualified &Pkg::sub(args).
test_cl('&Pkg::sub(args) calls the qualified user sub',
    'package Foo; sub bar { return "B:@_" } package main;'
    . ' print &Foo::bar("x"), "\n";',
    "B:x\n");

# The ref-taking form \&foo stays a code ref (NOT a call) — gated on no list.
test_cl('\\&foo remains a code ref, not a call',
    'sub foo { return "F:@_" } my $r = \&foo;'
    . ' print ref($r), ":", $r->("a","b"), "\n";',
    "CODE:F:a b\n");

# \&NAME references the sub slot, never the builtin — even when NAME is a
# builtin name. With a user `sub length`, \&length calls the user sub.
test_cl('\\&NAME refs the user sub slot, not the builtin operator',
    'sub length { return "UL:@_" } my $r = \&length;'
    . ' print $r->("zz"), "\n";',
    "UL:zz\n");

# ── Compound assignment must write back through array/hash element places ──
# `$a[i] OP= v` / `$h{k} OP= v` previously used box-set on the value (a no-op on
# a non-box deref), so the store was lost. (Surfaced by Math::BigInt::Calc's
# `$xv->[0] *= $yv->[0]`.) Mirrors what += already did.
test_cl('compound assign on array/hashref elements writes back',
    'my $xv=[100]; my $yv=[3]; $xv->[0] *= $yv->[0];'
    . ' my $h={a=>"x"}; $h->{a} .= "Y";'
    . ' my @a=(20,30); $a[1] /= 3;'
    . ' print "$xv->[0] $h->{a} $a[1]\n";',
    "300 xY 10\n");

# Plain boxed scalar compound assign is unchanged.
test_cl('compound assign on a plain scalar still works',
    'my $x = 100; $x *= 3; $x -= 50; print "$x\n";',
    "250\n");

# Empty array flattens to nothing (baseline list-flatten sanity).
test_cl('empty array flattens to nothing (not a spurious undef)',
    'my @a; my @b = (@a, 1); print scalar(@b), ":@b\n";',
    "1:1\n");

# A `X < Y > Z` comparison chain must parse as operators, not a <...> readline/glob.
# This used to PARSE-ERROR — PPI misread `< … >` as a glob and dropped the
# statement (docs/ppi-glob-disambiguation.md).  PPI 1.291 tokenizes it correctly
# and PCL handles it; this guards against either side regressing.  (Fixed
# upstream, so docs/ppi-bug-report.t no longer carries it.)
test_cl('chained < > comparison is not misparsed as a glob/readline',
    'my $x=[1,5,3]; print join(",", "a", ($x->[0] < $x->[1] > $x->[2]), "b"), "\n";',
    "a,1,b\n");

# `scalar <$fh>` / `print <$fh>` (a scalar-filehandle readline after a bareword):
# PPI misparses `<$fh>` as the two operators `< $fh >` whenever it follows a word
# that could take an operand (print/return/scalar/sort).  The glob-fixup pass now
# reconstructs the readline token.  (YAML::PP's `return scalar <$fh>` hit this:
# the unhandled `<`/`>` operand became an undef "single node of unknown type".)
like(transpile_to_cl('my $line = scalar <$fh>;'), qr/\(p-scalar \(p-readline \$fh\)\)/,
    'scalar <$fh> parses as a readline, not < > operators');
like(transpile_to_cl('print <$fh>;'), qr/\(p-print \(p-readline \$fh\)\)/,
    'print <$fh> parses as a readline');
# Guard the comparison sibling: `$a < $b` (no closing >) stays a less-than.
like(transpile_to_cl('my $r = $a < $b;'), qr/\(p-< \$a \$b\)/,
    '$a < $b stays a comparison (not reconstructed as readline)');

# A paren-less list operator in a ternary TRUE branch must stop its argument
# list at the ':' that closes the enclosing ternary — `cond ? join "-", @a : $fb`
# is `cond ? (join "-", @a) : $fb`, NOT join swallowing `: $fb` (which orphaned
# the colon → "Missing case" parse-error → empty result).  This is the root
# cause of the YAML::PP "dynamic require" wall: Module::Load's `_to_file` does
# `$^O eq 'MSWin32' ? join "/", @parts : File::Spec->catfile(@parts)`.
test_cl('paren-less list-op in ternary true-branch stops at enclosing colon',
    'my @p=("L","U"); my $y = 0 ? join "-", @p : "FB"; my $z = 1 ? join "-", @p : "FB"; print "$y|$z\n";',
    "FB|L-U\n");
# A NESTED ternary inside the list-op args must still be consumed (its own '?'
# raises the depth so its ':' is not mistaken for the enclosing boundary).
test_cl('nested ternary inside list-op args is not cut at its own colon',
    'my $c=1; my $v = join "-", $c ? "A" : "B", "Z"; print "$v\n";',
    "A-Z\n");

# $$ref->() must parse as (${$ref})->() — deref the scalar-ref-to-coderef, THEN
# call — not ${ $ref->() }.  Was mis-associated in PExpr (the leading scalar Cast
# swallowed the postfix call).  Class::Method::Modifiers' $$wrapped->(@_) needs it.
test_cl('$$ref->() derefs then calls (scalar-ref-to-coderef)',
    'my $cv = sub { return "OK" }; my $r = \$cv; print $$r->(), "\n";',
    "OK\n");

# \$ref->{k} / \$ref->[i] must be a LIVE reference to the slot, so a later write
# to that element is visible through the ref (stacked Moo `around` relies on
# \$cache->{wrapped} seeing reassignment).  Direct \$h{k} was already live; the
# arrow-deref form used to snapshot.
test_cl('\\$href->{k} is a live ref (tracks later writes)',
    'my $c = { k => "A" }; my $r = \$c->{k}; $c->{k} = "B"; print "$$r\n";',
    "B\n");
test_cl('\\$aref->[i] is a live ref (tracks later writes)',
    'my $a = [10, 20]; my $r = \$a->[1]; $a->[1] = 99; print "$$r\n";',
    "99\n");

# Postfix deref X->$#* is the last index of an arrayref (= $#{X}). The other
# postfix derefs (->@*/->%*/->$*) already worked; ->$#* was unrecognised
# (PPI tokenises it as a single Cast '$#*') and yielded undef.
# Found by tools/difftest-ops.pl axis 23.
test_cl('$ar->$#* is the last index of an arrayref',
    'my $ar = [10, 20, 30]; print $ar->$#*, "\n";',
    "2\n");
test_cl('$ar->$#* in arithmetic (named-unary-free) gives count',
    'my $ar = [1, 2, 3, 4]; print $ar->$#* + 1, "\n";',
    "4\n");

# ── session 255b: the six sweep-crash fixes (see docs/session-log.md) ──────────

# A lone bareword in a deref block is a symbolic ref to the package variable,
# NOT a sub call.  @{foo} / "$x->@{foo}" used to emit (pl-foo) → UNDEFINED-FUNCTION
# (crashed postfixderef.t and magic.t at *@{HASH}).
test_cl('@{bareword} is the symbolic array @bareword (interpolated)',
    'our @foo = (7,8,9); $_ = "foo"; print "$_->@{foo}\n";',
    "foo->7 8 9\n");
test_cl('@{bareword} is the symbolic array @bareword (non-interpolated)',
    'our @foo = (7,8,9); print join(",", @{foo}), "\n";',
    "7,8,9\n");

# $^H (hint bits) and %^H (hints hash) are inert always-bound empties — \%^H,
# keys %^H, $^H & MASK no longer crash with an unbound variable (eval.t RT 63110).
test_cl('$^H / %^H are inert, always-bound specials',
    'print "h=", ($^H & 0x20000), " k=", scalar(keys %^H), " r=", ref(\%^H), "\n";',
    "h=0 k=0 r=HASH\n");

# An embedded `our $var` in a use-constant value (\our $referent) declares the
# package global — previously dropped, leaving $referent unbound (index.t).
test_cl('embedded `our $var` in a use constant value is declared',
    'use constant riffraff => \our $referent; $referent = 42;'
  . ' print ref(riffraff), " ", ${riffraff()}, "\n";',
    "SCALAR 42\n");

# A bareword filehandle argument to select is a filehandle, not a value — was
# emitted as an unbound bareword symbol (scalar.t `select STDERR`).
test_cl('select BAREWORD does not evaluate an unbound symbol',
    'select STDERR; select STDOUT; print "ok\n";',
    "ok\n");

# A parenthesised scalar arrow-deref base — ($r//0)->[i]... = v — must not be
# treated as a list (rendered (vector ...)); it autovivified into a bogus
# 1-element vector → p-autoviv-aref-for-hash TYPE-ERROR (multideref.t).
test_cl('parenthesised scalar deref base autovivifies through the referent',
    'my $r = [[0]]; ($r // 0)->[0][0] = 9; print $r->[0][0], "\n";',
    "9\n");

# ── session 256: scalar(eval{die}) is a single undef element, not empty list ──
# scalar(EXPR) always produces exactly one scalar.  scalar(eval{die}) was
# returning raw nil, which p-flatten-args (used by `return (LIST)` in list
# context) splices away — so `return (scalar(eval{...}), $@)` dropped the undef
# and shifted the list.  This is the Try::Tiny / Test helper `_eval` idiom.
test_cl('scalar(eval{die}) contributes one undef element in a returned list',
    'sub f { return ( scalar(eval { die "boom\n"; 1 }), $@ ); }'
  . ' my @x = f(); print "n=", scalar(@x), " d0=", (defined $x[0]?1:0), " e=$x[1]";',
    "n=2 d0=0 e=boom\n");

# ── session 256: a ($)/($$) prototype imposes SCALAR context on its arguments ──
# child_context honors the scalar/ref slots of an old-style prototype, so an
# argument that lands in a $ slot is evaluated in scalar context even when the
# call is at statement level (void).  Without this, wantarray() reports void/
# undef inside the callee — e.g. Test::More's is($$;$) made `is(try {42}, 42)`
# evaluate try in void context and return undef.  Self-contained repro (no
# Test::More): the arg in a $ slot sees scalar context, the slurpy @ sees list.
test_cl('a ($$;@) prototype imposes scalar context on its $ args, list on @ tail',
    'sub wa { wantarray ? "L" : defined(wantarray) ? "S" : "V" }'
  . ' sub probe ($$;@) { print "a=$_[0] b=$_[1] c=$_[2]\n" }'
  . ' probe( wa(), wa(), wa() );',
    "a=S b=S c=L\n");

# Edge cases of prototype-driven argument context (all verified vs perl 5.40):

# A ($) unary-scalar prototype forces scalar context on its single argument.
test_cl('($) prototype forces scalar context on the argument',
    'sub wa { wantarray ? "L" : defined(wantarray) ? "S" : "V" }'
  . ' sub p1 ($) { print "$_[0]\n" } p1( wa() );',
    "S\n");

# ($@): the leading $ is scalar, the slurpy @ tail is list — within one call.
test_cl('($@) prototype: scalar head arg, list slurpy tail',
    'sub wa { wantarray ? "L" : defined(wantarray) ? "S" : "V" }'
  . ' sub psl ($@) { print "a=$_[0] rest=", join(",", @_[1..$#_]), "\n" }'
  . ' psl( wa(), wa(), wa() );',
    "a=S rest=L,L\n");

# ($$$;$) — every mandatory/optional scalar slot is scalar context (cmp_ok shape).
test_cl('($$$;$) prototype forces scalar context on all $ slots',
    'sub wa { wantarray ? "L" : defined(wantarray) ? "S" : "V" }'
  . ' sub p3 ($$$;$) { print join(",", @_), "\n" } p3( wa(), wa(), wa() );',
    "S,S,S\n");

# A (\@) reference prototype: the argument is taken as a ref (scalar slot).
test_cl('(\\@) reference prototype takes the argument as an array ref',
    'sub pref (\@) { print ref($_[0]), "\n" } my @b = (1, 2); pref(@b);',
    "ARRAY\n");

# ── session 256: die preserves ANY reference (blessed OR not) in $@ ──────────
# Perl's `die REF` keeps the reference verbatim as the exception — no
# stringification, no " at FILE line N." suffix.  p-die used to preserve only
# BLESSED refs; an unblessed hashref (`die { prev => $@ }`, Try::Tiny basic.t)
# fell to the string branch and became "HASH(0x..) at line N".
test_cl('die with an unblessed hashref preserves it as $@ (ref, not string)',
    'eval { die { prev => "bar\n" } }; print ref($@), " ", $@->{prev};',
    "HASH bar\n");
test_cl('die with an unblessed arrayref preserves it as $@',
    'eval { die [10, 20, 30] }; print ref($@), " ", join("-", @{$@});',
    "ARRAY 10-20-30");

# ── session 256: `my $x = EXPR if COND` = `my $x; $x = EXPR if COND` ─────────
# A statement modifier on a `my` declaration: the lexical is declared
# unconditionally (its let is opened by the block scanner), only the
# initializer assignment is conditional, and the lexical is re-bound per call
# (no stale carryover).  Was: the modifier tokens stayed in the RHS — a bare
# list-op RHS (`my $c = shift if @_>1`) crashed with a malformed (p-if ...),
# and a literal RHS dropped the initializer.  Found in File::Spec via
# Class::Inspector.  (verified vs perl 5.40)
test_cl('my $x = shift if COND — assigns when true (list-op RHS, no crash)',
    'sub f { my $c = shift if @_ > 1; return defined $c ? "c=$c" : "u" }'
  . ' print f(10, 20);',
    "c=10");
test_cl('my $x = shift if COND — undef when false',
    'sub f { my $c = shift if @_ > 1; return defined $c ? "c=$c" : "u" }'
  . ' print f();',
    "u");
test_cl('my $x = LITERAL if COND — initializer kept when true',
    'sub f { my $c = 5 if @_ > 1; return defined $c ? "c=$c" : "u" }'
  . ' print f(10, 20);',
    "c=5");
# Re-bound per call: a false COND on a later call must NOT see the earlier value.
test_cl('my $x = EXPR if COND re-binds per call (no stale carryover)',
    'sub f { my $c = "set" if $_[0]; return defined $c ? $c : "undef" }'
  . ' print f(1), ",", f(0), ",", f(1);',
    "set,undef,set");

# ── session 256: interpolated @ISA element (runtime parent class name) ───────
# `our @ISA = ("Base::$impl")` names a parent only known at run time.  It must
# NOT be baked into the compile-time CLOS defclass (it would store the literal
# "Base::$impl"); instead it is pushed onto @ISA at run time and resolved by the
# %pcl-isa-ancestry method-dispatch walk.  This is the mechanism the real (pure-
# Perl) File::Spec uses to dispatch to File::Spec::Unix.
test_cl('interpolated @ISA parent (runtime class name) resolves inherited methods',
    'package Base::Impl; sub greet { "hi from impl" }'
  . ' package Front; my $impl = "Impl"; our @ISA = ("Base::$impl");'
  . ' package main; print Front->greet, "\n";',
    "hi from impl\n");

# ── session 257: block-scoped `{ package X; our @ISA = (...) }` inheritance ──
# Two bugs made a block-scoped package's @ISA inheritance silently break (while
# the same code at file scope worked).  (1) The package's bare `(defclass X ())`
# is emitted INLINE inside the runtime block, but the parented "Redefine"
# defclass was hoisted to the package PREAMBLE — emitted earlier, then clobbered
# by the inline bare one.  (2) The `(defvar @ISA ...)` was unqualified → landed
# in MAIN::@ISA, but the `(p-push @ISA ...)` ran under `(in-package :X)` → wrote
# X::@ISA; %pcl-isa-ancestry reads X::@ISA, which stayed empty.  Both now key off
# _block_depth: parented defclass emitted inline, @ISA package-qualified.  This
# was the wall blocking Class::Inspector / Safe::Isa.  (verified vs perl 5.40)
test_cl('block-scoped package @ISA — inherited method dispatch',
    '{ package Animal; sub new { bless {}, shift } sub speak { "generic" } }'
  . '{ package Dog; our @ISA = ("Animal"); sub bark { "woof" } }'
  . ' my $d = Dog->new; print $d->speak, "-", $d->bark, "\n";',
    "generic-woof\n");
test_cl('block-scoped package @ISA — isa() and can()',
    '{ package Animal; sub new { bless {}, shift } sub legs { 4 } }'
  . '{ package Dog; our @ISA = ("Animal"); }'
  . ' my $d = Dog->new;'
  . ' print +($d->isa("Animal") ? "y" : "n"), ($d->can("legs") ? "y" : "n"), "\n";',
    "yy\n");
test_cl('block-scoped package @ISA — SUPER:: dispatch',
    '{ package Animal; sub new { bless {}, shift } sub speak { "generic" } }'
  . '{ package Dog; our @ISA = ("Animal");'
  . '  sub speak { my $s = shift; "woof+" . $s->SUPER::speak() } }'
  . ' print Dog->new->speak, "\n";',
    "woof+generic\n");

# ── session 257: method call on undef / unblessed ref is a fatal error ───────
# `$ref->method` where $ref is an unblessed reference dies "Can't call method X
# on unblessed reference"; on undef it dies "...on an undefined value".  Was: the
# invocant yielded a nil class which fell through to "main", so the call silently
# dispatched against main (lived).  Safe::Isa's $_isa/$_can guard non-objects by
# relying on this death under eval.  (verified vs perl 5.40)
test_cl('method call on unblessed ref / undef dies (caught by eval)',
    'my $aref = [42]; my $undef;'
  . ' my $a = eval { $aref->isa("Foo"); 1 };'
  . ' my $b = eval { $undef->can("x"); 1 };'
  . ' print defined($a) ? "lived" : "died", ",",'
  . '       defined($b) ? "lived" : "died", "\n";',
    "died,died\n");

# ── session 257: method-call arguments are LIST context ──────────────────────
# A Perl method call passes its args as a flat list (methods can't have
# prototypes), so a list-returning arg must run in LIST context.  Was: a
# method-call arg inherited the caller's (scalar) context, so
# `$obj->m(split /::/, $s)` evaluated split in scalar context → it returned the
# field COUNT (2) instead of the fields.  This was the Class::Inspector
# ->filename bug: File::Spec->catfile(split /::/, $name) gave "2".  (Plain
# unprototyped FUNCTION calls are deliberately NOT changed here — forcing LIST on
# all user-sub args regressed the sweep; only the method path is fixed.)
test_cl('method-call arg gets list context (split returns fields, not count)',
    'package Cat; sub j { shift; return join("/", @_) }'
  . ' package main; my $name = "Class::Inspector";'
  . ' print Cat->j(split /(?:\'|::)/, $name), "\n";',
    "Class/Inspector\n");

# ── session 258: unprototyped user-function args are LIST context ────────────
# Sibling of the s257 methodcall rule: Perl flattens a function call's args into
# @_, so a list-returning arg (split, etc.) must run in LIST context.  Was: such
# args inherited the caller's (scalar/void) context → split returned its field
# COUNT.  Made safe by extracting the TAP assertions' real ($$@) prototypes from
# test.pl/Test::More so is(unpack(...), …) etc. stay scalar.  (verified vs perl 5.40)
test_cl('unprototyped user-function arg gets list context (split → fields)',
    'sub myf { return scalar(@_) }'
  . ' print myf(split /,/, "a,b,c"), "\n";',
    "3\n");

test_cl('user-function arg list-context preserves all split fields',
    'sub cat { shift; return join("/", @_) }'
  . ' print cat("x", split /::/, "Foo::Bar::Baz"), "\n";',
    "Foo/Bar/Baz\n");

# Bit-shift and bitwise operators force SCALAR context on their operands.  This
# latent bug surfaced once list context could reach them: `($x || 255) << 8`
# evaluated the `||` RHS in list context and the shift/bitwise op yielded 0.
# << >> & | ^ are numeric scalar operators — operands must be scalar even when
# the op sits in an unprototyped funcall's (now list-context) argument slot.
test_cl('bitwise/shift operators force scalar context on operands',
    'my $z = 0;'
  . ' sub g { return join(",", @_) }'
  . ' print g(($z||255)<<8, ($z||7)&3, ($z||4)|1, ($z||4096)>>4), "\n";',
    "65280,3,5,256\n");

# ── session 258b: %INC populated with single-slash keys (was Foo//Bar.pm) ─────
# p-module-to-path replaced each ':' with '/', so 'Foo::Bar' -> 'Foo//Bar.pm'.
# The OS tolerates the double slash when opening the file (so loading worked),
# but %INC's key was wrong, and $INC{"Foo/Bar.pm"} / loaded_filename-style
# lookups missed.  Fix: collapse '::' to a single '/'.  Also gave keys/values
# %INC their missing %INC-MARKER% case (they returned empty).  Found via the
# Class::Inspector test suite (loaded_filename).  (verified vs perl 5.40)
test_cl('%INC key uses single slash and keys %INC enumerates loaded modules',
    'use File::Spec;'
  . ' print exists $INC{"File/Spec.pm"} ? "Y" : "N";'
  . ' my @f = grep { m{/} } keys %INC;'
  . ' print scalar(@f) ? "K" : "k"; print "\n";',
    "YK\n");

# ── session 258b: @ISA = qw/.../ with a NON-bracket delimiter ────────────────
# _extract_parent_classes stripped only bracket qw delimiters ( [ { < — so
# `our @ISA = qw/ Parent /` (slash, or !|#, etc.) kept its "qw/" prefix and "/"
# suffix and split into bogus parent names ("qw/", "Parent", "/"), emitting a
# (defclass ... (qw/::plc-qw/ ... /::plc-/)) that failed to READ ("Package QW/
# does not exist").  qw(Parent) always worked.  Found in YAML::PP::Reader
# (`our @ISA = qw/ YAML::PP::Reader /`).  Fix: strip qw + ANY delimiter.
test_cl('@ISA = qw/.../ (slash/non-bracket delimiter) sets inheritance',
    'package P; sub new { bless {}, shift } sub hi { "hi" }'
  . ' package C; our @ISA = qw/ P /;'
  . ' package D; our @ISA = qw! P !;'
  . ' package main;'
  . ' print ref(C->new), ":", C->new->hi, ":", ref(D->new), "\n";',
    "C:hi:D\n");

# ── PPI bug workaround: 7%-3 mis-tokenized as the magic hash %- ───────────────
# PPI 1.291 reads `%-`/`%+` glued to a preceding term as the named-capture magic
# hash, dropping the modulo operator (PARSE ERROR).  _fix_modulo_magic re-splits
# `%-`/`%+` after a term into `% -`/`% +`.  See docs/ppi-bug-modulo-magic.md.
test_cl('modulo with glued negative/positive operand (PPI %-/%+ workaround)',
    'my $y = 3; my @a = (7, 8);'
  . ' print join(",", 7%-3, 7 %-3, 7%+3, $a[0]%-2, (5+5)%-3, 8%-$y), "\n";',
    "-2,-2,1,-1,-2,-1\n");

# The genuine magic hashes %-/%+ must still parse as hashes (not be rewritten):
# `keys %-` keeps `%-` because it follows the list-op word `keys`, not a term.
test_cl('named-capture %+ hash element still works after the %- workaround',
    '"foo42" =~ /(?<n>\d+)/; print "$+{n}\n";',
    "42\n");

# ── runtime warning strings used CL "...\n", but \n in a CL string literal is a
# bare 'n' (backslash only escapes " and \), so the join uninit warning rendered
# as "...joinn at unknown line 0." (no real newline → the location suffix was
# appended too). Fixed by emitting a real newline (~%). PCL can't produce a line
# number, so the message just ends after the text — matching Perl's wording.
test_cl('uninitialized-in-join warning renders cleanly (no "joinn" / no bogus line)',
    'print join(",", 1, undef, 3), "\n";',
    "Use of uninitialized value in join or string\n1,,3\n");

# ── undef regex capture vars vanished in a list assignment ───────────────────
# Non-participating capture groups were raw nil, but %p-flatten-list (used by
# p-list-= `my (...) = (...)`) drops raw nil as an array-hole/empty-list marker,
# so `my ($a,$b)=($3,$4,'Z')` shifted 'Z' into $a. Fixed: capture-group undef is
# now *p-undef* (survives flattening). Found via Text::ParseWords::parse_line.
test_cl('undef regex capture in a list assignment keeps its slot',
    '"ab" =~ /(a)(b)/;'                       # $1=a $2=b, $3/$4 do not participate
  . ' my ($p,$q,$r) = ($3, $4, "Z");'
  . ' print "p=",(defined $p?$p:"U")," q=",(defined $q?$q:"U")," r=",(defined $r?$r:"U"),"\n";',
    "p=U q=U r=Z\n");

# Same, after a substitution (s/// sets undef captures via a different path).
test_cl('undef capture after s/// keeps its slot in a list (ternary-list form)',
    'my $s = "ab"; $s =~ s/(a)(b)/x/;'
  . ' my ($p,$q,$r,$t) = (($9 ? (1,2) : ($3,$4)), "A", "B");'
  . ' print join(",", map { defined $_ ? $_ : "U" } $p,$q,$r,$t), "\n";',
    "U,U,A,B\n");

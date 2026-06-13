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

plan tests => 34;

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

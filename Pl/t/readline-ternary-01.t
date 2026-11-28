#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# readline-ternary-01.t — tasks #479 and #563: the CASCADE of PPI's `<FH>`
# mis-lex, in both of its shapes.
#
# docs/ppi-upstream-bugs.md §14: in TERM position PPI 1.291 reads `<$f>` as
# `Operator(<) Symbol($f) Operator(>)`.  PCL has lived with that since s404
# because Pl::PExpr::_fix_ppi_glob_after_block rebuilds the token — but the
# rebuild cannot reach the CONSEQUENCE: with `>` taken for an operator, the
# next `/` is in term position too, so PPI reads it as a MATCH:
#
#     $ok ? <$f> // "" : ""    ->  … Operator(>) Regexp::Match(//) …
#     $ok ? <$f> / 2 : ""      ->  … Operator(>) Regexp::Match(/ 2 : "";)
#
# The first dropped the statement (#138); the second swallowed the REST of the
# file up to the next `;`.  Pl::Parser2::_repair_readline_cascade rewrites the
# diamond as the `readline(...)` perlop says it is and reparses.
#
# WHY IT IS WORTH A FILE: the shape occurs in ZERO sites of all four in-repo
# populations (measured s446k over 1329 files), so nothing in the sweep, the
# corpus or the companion suite can guard it — these rows are the guard.  It is
# not hypothetical: perl-tests/t/test.pl's runperl_and_capture carried two of
# them, re-transpiled by every one of the 108 sweep files, and it was the FIRST
# thing the #472 child-drop instrument found.
#
# The negatives are as load-bearing as the positives: the repair's condition is
# the family's negative (`_ends_term` — the `<` must be in TERM position) plus
# the cascade itself (a Regexp token must follow the `>`), so a genuine
# comparison chain and a genuine match must come through untouched.  Every
# expectation below is the live `perl` answer.
#
# THE SECOND HALF (task #563, §14c) is the same cascade one step further out.
# When the body is a GLOB PATTERN rather than a readline body, the `/` inside
# the pattern starts the match, so the `>` is swallowed TOO:
#
#     my @f = sort <./nope-*-xyz>;
#         -> Word(sort) Operator(<) Operator(.) Regexp::Match(/nope-*-xyz>;)
#
# and the Regexp then eats until the next `/` — which is usually lines away, so
# this member takes whole statements, sometimes the rest of the file, with it.
# `Pl::Parser2::_repair_glob_pattern_cascade` rewrites the diamond as the
# `glob("...")` perlop says it is.  It too occurs in ZERO files of every
# population (emission-ab over 921: SAME 921), so these rows are its only guard.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 39;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (PCLCore::transpile FAILS the row on a dropped statement) and run.
sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# Both sides of every row: PCL's answer must be PERL's answer, so a future
# change to either can only agree or fail.
sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# Each snippet makes its own data file, so perl's run and PCL's run never
# share one; $$ differs between them.
my $PRELUDE = <<'PL';
my $F = "/tmp/pcl-readline-ternary-$$.dat";
open(my $w, '>', $F) or die "open: $!"; print $w "hello\nsecond\n"; close $w;
my $ok = 1;
PL
my $EPILOGUE = qq{\nunlink "/tmp/pcl-readline-ternary-\$\$.dat";\n};

sub agree_with_file {
    my ($body, $desc) = @_;
    both_agree($PRELUDE . $body . $EPILOGUE, $desc);
}

# ---- the bug ---------------------------------------------------------------

agree_with_file(<<'PL', 'the #479 shape: COND ? <$fh> // "" : ""');
local $/; open(my $f, '<', $F) or die;
my $so = $ok ? <$f> // "" : "";
print "[$so]\n";
PL

agree_with_file(<<'PL', 'the false branch does not read the handle');
local $/; open(my $f, '<', $F) or die;
my $so = 0 ? <$f> // "" : "X";
print "[$so]", (defined(<$f>) ? "unread" : "eof-or-unread"), "\n";
PL

agree_with_file(<<'PL', 'an undef handle: // supplies the default');
my $g;
my $so = $ok ? <$g> // "Z" : "";
print "[$so]\n";
PL

agree_with_file(<<'PL', 'the swallowing spelling: `<$f> / 2` ate the rest of the file');
open(my $f, '<', $F) or die; my $line = <$f>; close $f;
open(my $n, '>', "$F.n") or die; print $n "8\n"; close $n;
open(my $h, '<', "$F.n") or die;
my $q = $ok ? <$h> / 2 : 0;
print "[$q][$line]";
unlink "$F.n";
PL

agree_with_file(<<'PL', 'a BAREWORD handle in the same shape');
local $/; open(FH, '<', $F) or die;
my $so = $ok ? <FH> // "" : "";
close FH;
print "[$so]\n";
PL

# A `(...)` rewrite of the diamond would have turned this into perl's
# "print (...) interpreted as function" gotcha — hence readline(), not parens.
agree_with_file(<<'PL', 'inside a print argument list');
local $/; open(my $f, '<', $F) or die;
print "[", ($ok ? <$f> // "" : ""), "]\n";
PL

# ---- the neighbours that ALREADY lowered and must keep lowering ------------

agree_with_file(<<'PL', 'neighbour: no // — a plain readline branch');
local $/; open(my $f, '<', $F) or die;
my $so = $ok ? <$f> : "";
print "[$so]\n";
PL

agree_with_file(<<'PL', 'neighbour: readline($f) // "" spelled out');
local $/; open(my $f, '<', $F) or die;
my $so = $ok ? readline($f) // "" : "";
print "[$so]\n";
PL

agree_with_file(<<'PL', 'neighbour: <$f> // "" with no ternary');
local $/; open(my $f, '<', $F) or die;
my $so = <$f> // "";
print "[$so]\n";
PL

# ---- the negatives: `<` that is really less-than ---------------------------

both_agree('my ($a1,$b1,$c1) = (1,5,3); print (($a1 < $b1) ? 1 : 0), print (($b1 > $c1) ? 1 : 0), print "\n";',
           'negative: two genuine comparisons keep their meaning');

both_agree('my $s = "abc"; my $n = 2; print(($n > 1 && $s =~ /b/) ? "y" : "n"); print "\n";',
           'negative: a real match after a `>` comparison');

both_agree('my $q = 10; my $ok = 1; print $ok ? $q / 2 : 0; print "\n";',
           'negative: division after an ordinary term');

# ---- #563: the GLOB-PATTERN half of the same cascade ------------------------

# One fixture directory with two files, and every snippet chdir()s into it, so
# perl's run and PCL's run glob the same relative patterns whatever cwd prove
# was started from.
my $globdir = tempdir(CLEANUP => 1);
for my $n (qw(a b)) {
    open my $fh, '>', "$globdir/g-$n.t" or die "create: $!";
    close $fh;
}

sub agree_in_globdir {
    my ($body, $desc) = @_;
    both_agree(qq{chdir "$globdir" or die "chdir: \$!";\n} . $body, $desc);
}

agree_in_globdir(<<'PL', 'the #563 shape: a `./`-relative pattern after sort');
my @f = sort <./g-*.t>;
print "[@f]\n";
PL

agree_in_globdir(<<'PL', 'the task repro: the pattern matches nothing');
my @f = sort <./nope-*-xyz>;
print "n=", scalar(@f), "\n";
PL

# #651 (closed by s543f): a pattern whose only metacharacter is a BRACKET CLASS,
# and #2384 (closed by s543f): a pattern after a grep BLOCK whose directory name
# holds a digit then a quote-like letter (`a9m`) -- both one Readline since
# PCL's PPI group AH (s527d / s527g, §14 §14d); Pl/t/glob-01.t's grep rows 29-30
# used a random File::Temp name of that kind and flaked.
agree_in_globdir(<<'PL', '#651 / #2384: <./g-[ab].t> after sort, and <./a9m/*.txt> after a grep block');
mkdir "a9m"; for my $f ("a9m/x.txt", "a9m/y.txt") { open my $h, ">", $f or die; close $h }
my @f = sort <./g-[ab].t>;
my @g = grep { /x/ } <./a9m/*.txt>;
my @s = sort { $a cmp $b } <a9m/*.txt>;
print "[@f] [@g] [@s]\n";
PL

agree_in_globdir(<<'PL', 'as a list-operator argument: print <./g-*.t>');
print sort <./g-*.t>;
print "\n";
PL

agree_in_globdir(<<'PL', 'after a BLOCK, the other §14 trigger');
my @f = grep { 1 } <./g-*.t>;
print scalar(@f), "\n";
PL

agree_in_globdir(<<'PL', 'after reverse, and the pattern interpolates a scalar');
my $d = ".";
my @f = reverse sort <$d/g-*.t>;
print "[@f]\n";
PL

# The Regexp the mis-lex creates eats until the next `/`, which is normally
# LINES away — so this row is about the two prints, not about the glob.
agree_in_globdir(<<'PL', 'the swallow: the statements AFTER the diamond still run');
my @f = sort <./g-*.t>;
print "one\n";
print "two\n";
PL

# TWO glob statements, no other `/` between them: the first derail's Regexp
# swallows the second glob's own `/`, so that `<` is not even a token until
# the first repair's reparse frees it — a single-pass repair fixed glob one
# and dropped glob two (s449 review fix: the repair runs to FIXPOINT).
agree_in_globdir(<<'PL', 'a second cascade REVEALED by the first repair is repaired too');
my @f = sort <./g-*.t>;
my @g = sort <./nope-*-xyz>;
print "n1=", scalar(@f), " n2=", scalar(@g), " alive\n";
PL

# ---- #563 neighbours: shapes PExpr's rebuild already owned ------------------

agree_in_globdir(<<'PL', 'neighbour: the `>` survives (no leading `./`) — PExpr rebuilds');
my @f = sort <g-*.t>;
print "[@f]\n";
PL

# #650 (s543w): perl's diamond may contain whitespace -- it is a LIST of
# patterns, and a pattern without a wildcard is itself, so `<a b>` is a word
# list.  PCL's PPI reads a spaced `<...>` where a term is expected as one
# Readline (group BK, docs/ppi-upstream-bugs.md §77), and after `use Module`
# (group BJ, §76).  The repair above still takes only ONE CONTIGUOUS WORD (the
# belt to `_ends_term`'s braces); it is reached only after a user's sub now.
agree_in_globdir(<<'PL', '#650: sort < ./g-*.t >, and two patterns <./g-*.t ./g-a.t>');
my @f = sort < ./g-*.t >;
my @g = sort <./g-*.t ./g-a.t>;
my @s = < ./g-a.t >;
print "[@f] [@g] [@s]\n";
PL
both_agree('use List::Util <max sum>; print max(3, 9, 4), " ", sum(1, 2, 3), "\n";',
           '#650 §76: use Module <a b> imports both names (a word list written as a glob)');
both_agree('use List::Util <first sum0>; my $f = first { $_ > 1 } 1, 2, 3; print $f, " ", sum0(4, 5), "\n";',
           '#650 §76: the word list reaches the prototype merge (first BLOCK LIST)');
both_agree('no warnings; use builtin <floor ceil>; print floor(2.5), ceil(2.5), "\n";',
           '#650 §76: use builtin <a b> aliases both builtins');
both_agree('my @r = ([< 4 5 >], [< 6 7 >]); my $h = { < a b > }; print "@{$r[1]} ", join(",", %$h), "\n";',
           '#650 §77: a spaced word list after [ and {');
both_agree('my %h = map { $_ => 1 } < a b >; my @g = grep { 1 } < c d >; print join(",", sort keys %h), " @g\n";',
           '#650 §77: a spaced word list after a map / grep block');
both_agree('sub f { return < a b > } my %k = (k => < x >); my @c = (0 ? < p > : < q r >); print f(), " $k{k} @c\n";',
           '#650 §77: after return, =>, ? and :');
agree_in_globdir(<<'PL', '#650: after a user sub the rebuild keeps the space between two patterns');
sub g { @_ }
my @x = g <g-*.t g-a.t>;
print "[@x]\n";
PL
# The reverse error: PExpr's rebuild took `< 2*$n and 3 >` for a glob after
# a subscript, a call, a method name, `time` or a declared constant -- only a
# user's word can stand before a glob PPI did not lex (s543w).
both_agree('my @a = (1); my $n = 1; sub f { 1 } print "a\n" if $a[0] < 2*$n and 3 > 1; print "b\n" if f() < 2*$n and 3 > 1; print "c\n" if time < 2**40 and 1 > 0;',
           'negative: `X < 2*$n and 3 > 1` compares after a subscript, a call and time');

agree_in_globdir(<<'PL', 'neighbour: after `=`, where PPI gets the Readline token right');
my @f = sort(<./g-*.t>);
print "[@f]\n";
PL

# ---- #563 negatives: a `<` that is really less-than ------------------------

# `$#a` is a VALUE, so this `<` is a comparison — but it is not a
# PPI::Token::Symbol, which is how it fell out of `_ends_term` (s449s).  Spelled
# with NO whitespace, because the repair's contiguity guard would otherwise hide
# the miss: without the `_ends_term` arm this transpiled to
# `my $r=($#aglob("3&&$s=~/a")b/)?"y":"n";` and dropped.
both_agree('my @a=(1,2);my $s="a>b";my $r=($#a<3&&$s=~/a>b/)?"y":"n";print "$r\n";',
           'negative: $#a ENDS A TERM, so `<` is less-than (match holds a `>`)');
both_agree('my @a=(1,2,3,4,5);my $s="a>b";my $r=($#a<3&&$s=~/a>b/)?"y":"n";print "$r\n";',
           'negative: the same comparison the other way round');

# A zero-arity builtin is a TERM after which `<` compares — the same predicate
# `_repair_word_x_call` uses, and the reason an ALL-CAPS word is NOT consulted
# (before a diamond it is a filehandle, not a constant).
both_agree('my $r = (time < 2 && "ab" =~ /a>b/) ? "y" : "n"; print "$r\n";',
           'negative: `time` is a term, so `time < 2` is a comparison');

# ---- #2090: <FH> on the RHS of a LIST ASSIGNMENT is LIST context -----------
#
# `p-readline` used to consult a second variable besides *wantarray*:
# `*p-in-list-assign-rhs*`, bound T by `p-list-=` around its whole RHS, forced
# SCALAR mode there.  So `my ($header, @rows) = <$fh>` left @rows EMPTY and
# `my $n = () = <$fh>` was 1 — silently, on the commonest CSV/report idiom
# there is.  The flag was a patch over a different bug: the shape it was added
# for, `while (($seen ? $dummy : $name) = <FILE>)`, is a SCALAR assignment in
# perl (a ternary is not a list: `($s ? $d : $n) = @a` stores the COUNT), and
# ExprToCL's `_sole_ternary_lvalue_id` arm has lowered it as one since.
# Every expectation is the live `perl` answer, including the one that surprises:
# `while (my ($x) = <FH>)` iterates ONCE, because the first list-context read
# slurps the handle.

agree_with_file(<<'PL', '#2090: my ($first, @rest) = <$fh> reads every line');
open(my $f, '<', $F) or die;
my ($first, @rest) = <$f>;
print "first=$first", "rest=", scalar(@rest), "\n";
PL

agree_with_file(<<'PL', '#2090: my $n = () = <$fh> counts the lines');
open(my $f, '<', $F) or die;
my $n = () = <$f>;
print "n=$n\n";
PL

agree_with_file(<<'PL', '#2090: while (my ($x) = <$fh>) iterates ONCE (perl slurps)');
open(my $f, '<', $F) or die;
my $it = 0; my $last = "";
while (my ($x) = <$f>) { $it++; $last = $x; last if $it > 20 }
print "it=$it last=$last";
PL

agree_with_file(<<'PL', '#2090 negative: a ternary LHS is a SCALAR assignment, one line per loop');
open(FILE, '<', $F) or die;
my ($seen, $dummy, $name) = (0, '', '');
while (($seen ? $dummy : $name) = <FILE>) { $seen++; last if $seen > 20 }
print "seen=$seen\n";
close FILE;
PL

agree_with_file(<<'PL', '#2090 negative: a scalar `my $line = <$fh>` still reads one line');
open(my $f, '<', $F) or die;
my $line = <$f>;
my $left = 0; $left++ while <$f>;
print "line=$line", "left=$left\n";
PL

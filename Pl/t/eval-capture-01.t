#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

#          -*-Mode: CPerl -*-

# Lexical capture in string eval (session 250).
#
# Perl's `eval "CODE"` can see the enclosing scope's `my` lexicals; PCL bridges
# this by passing the caller's in-scope lexicals into p-eval and turning the
# eval body's free variables into lambda parameters.  See
# docs/eval-lexical-capture.md.
#
# Every expected value below was verified against real `perl` (the snippets live
# in /tmp batteries during development).  To keep the suite fast, all scenarios
# run in ONE transpiled program that prints "tag=value" lines; we assert one
# per line.  Three deliberate divergences from Perl are documented (and probed)
# at the end rather than asserted as correct.

use v5.30;
use strict;
use warnings;
use lib ".";
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

use Test::More;

my $pl2cl   = "./pl2cl";
# The sbcl command line comes from the ONE builder every runner shares
# (tools/lib/PCLSbcl.pm via PCLCore::sbcl_prefix, task #344): the saved core
# with the runtime already compiled in, and the 512 MB control stack.  This
# file used to keep its own `my $runtime = 'cl/pcl-runtime.lisp'` and `--load`
# it, which recompiles the whole runtime on EVERY row -- 2.89 CPU-s a row
# against 0.007 s from the core (measured s473u, #1544) -- ran on the default
# 2 MB stack, and depended on prove's cwd for that relative path.
my @sbcl_rt = PCLCore::sbcl_prefix("$FindBin::Bin/../../cl/pcl-runtime.lisp");

plan skip_all => "pl2cl not found"  unless -x $pl2cl;
plan skip_all => "sbcl not found"   unless `which sbcl 2>/dev/null`;

# One program exercising every capture scenario.  Each scenario is isolated in
# its own sub so `my` declarations don't leak between cases.
my $driver = <<'PERL';
our $LVG;
sub basic_read    { my $z = 5; return eval '$z + 1'; }
sub write_back    { my $z = 10; eval '$z = 99'; return $z; }
sub closure_eval  { my $c = "X"; my $g = eval 'sub { "got:$c" }'; return $g->(); }
sub array_elem    { my @a = (11,22,33); return eval '$a[1]'; }
sub hash_elem     { my %h = (k=>7); return eval '$h{k}'; }
sub ab_lexical    { my $a = 5; return eval '$a + 1'; }
sub ab_ordinary   { my $a = 3; my $b = 4; return eval '$a + $b'; }
sub user_my       { return eval 'my $z = 7; $z + 1'; }
sub user_my_a     { return eval 'my $a = 7; $a + 1'; }
sub eval_shadow   { my $z = 1; return eval 'my $z = 5; $z'; }
sub return_eval   { my $r = eval 'return 7; 99'; return $r; }
sub two_lex       { my $p = 2; my $q = 3; return eval '$p * $q'; }
sub nested_block  { my $p = 1; { my $q = 2; return eval '$p + $q'; } }
sub many_vars     { my ($a1,$b2,$c3,$d4) = (1,2,3,4); return eval '$a1+$b2+$c3+$d4'; }
sub array_full    { my @a = (1,2,3); return join(",", eval '@a'); }
sub array_set     { my @a = (1,2,3); eval '$a[0] = 99'; return "@a"; }
sub hash_full     { my %h = (a=>1,b=>2); return join(",", sort eval 'keys %h'); }
sub mixed_global  { our $G1 = 100; my $z = 5; return eval '$z + $G1'; }
sub list_return   { my $x = 2; return join(",", eval '($x, $x*2, $x*3)'); }
sub recursion     { my $n = shift; return 1 if $n <= 1; return $n * eval 'recursion($n-1)'; }
sub two_evals     { my $x = 1; my $y = 2; my $a = eval '$x'; my $b = eval '$y'; return "$a$b"; }
sub mod_then_read { my $x = 1; eval '$x = $x + 10'; return eval '$x * 2'; }
sub deep_closure  { my $msg = "hi"; my $g = eval 'sub { my $h = sub { $msg }; $h->() }'; return $g->(); }
sub closure_rename { my $c = 5; my $cl = sub { $c + 100 }; my $e = eval '$c + 1'; return "$e/" . $cl->(); }
sub foreach_var   { my @r; for my $x (1..3) { push @r, eval '$x * 10' } return "@r"; }
sub local_var     { local $LVG = 42; return eval '$LVG'; }
sub magic_under   { return join(",", map { eval '$_ * 2' } (1,2,3)); }
sub magic_args    { return eval 'join ",", @_'; }
sub magic_cap     { "abc" =~ /(b)/; return eval '$1'; }
sub eval_in_sort  { return eval 'join ",", sort { $a <=> $b } (3,1,2)'; }
# s304 regression (eval.t #39): a sub DEFINED by the eval builds a closure over
# its own lexical.  The enclosing file's forward-decl scan used to defvar $s9
# from the string literal's innards, proclaiming it special and collapsing the
# closure to a dynamic read (empty after return).
sub sub_in_eval   { eval 'sub cgc9 { my $s9 = shift; return sub { $s9 } }'; return cgc9("good")->(); }
# s304: a package global used ONLY inside eval strings must persist across
# evals (p-eval-lex-lookup installs the autovivified container).
sub cross_eval    { eval '$xg9 = 5'; return eval '$xg9 + 1'; }
# s501b (#2285): a `package` statement inside the eval'd text does not re-home
# a CAPTURED lexical -- core Benchmark builds exactly pkg_bench's string.  The
# last row is the inverse: a name the eval does NOT capture still goes to X.
sub pkg_amp       { my $c = sub { "ran" }; my $s = eval 'sub { package main; &$c; }'; return $s->(); }
sub pkg_arrow     { my $c = sub { "ran" }; my $s = eval 'sub { package main; $c->(); }'; return $s->(); }
sub pkg_other     { my $c = sub { "ran" }; my $s = eval 'sub { package Other; &$c; }'; return $s->(); }
sub pkg_bench     { my $r = ""; my $c = sub { $r .= "b" }; my ($n, $pack) = (2, "main");
                    my $s = eval "sub { for (1 .. $n) { local \$_; package $pack; &\$c; } }"; $s->(); return $r; }
sub pkg_scalar    { my $x = "lex"; $Foo::x = "foo"; my $s = eval 'sub { package Foo; $x }'; return $s->(); }
sub pkg_array     { my @arr = (1,2,3); my $s = eval 'sub { package Foo; scalar(@arr) . $arr[1] . $#arr }'; return $s->(); }
sub pkg_global    { my $s = eval 'sub { package Foo; $yy9 = 7; $Foo::yy9 }'; return $s->(); }
# ...and a FILE lexical a named sub captures (promoted to a package cell under
# its own name) is still a lexical to the eval'd text: scalar, array, and a
# `package` statement at the START of the eval string defining a named sub.
my $cap9 = sub { "cap(@_)" }; my @arr9 = (4, 5);
sub use_cap9      { return ($cap9, @arr9) }
sub pkg_promoted  { my $s = eval 'sub { package Other; $cap9->(1) . scalar(@arr9) }'; return $s->(); }
sub pkg_made      { eval 'package Made9; sub mk { __PACKAGE__ . ":" . $cap9->(2) } 1' or return "ERR"; return Made9::mk(); }
# #4796 (s543b): an eval that RE-DECLARES a captured lexical and reads it -- the
# initializer (and a use before the declaration) is the CALLER's variable: the
# sub-level lexical, a nested block, a sub argument, a write, nested evals, a
# list declaration, a loop (fresh each time), Sub::Quote's shape, a file lexical.
sub reread_init   { my $x = 7; return eval 'my $x = $x + 1; $x'; }
sub reread_block  { my $r; { my $z = 3; $r = eval 'my $z = $z * 2; my $w = $z + 1; "$z $w"'; } return $r; }
sub reread_arg    { my $p = shift; return eval 'my $p = $p . "!"; $p'; }
sub reread_write  { my $y = 7; eval '$y = 5; { my $y = 3; } 1'; return $y; }
sub reread_nested { my $n = 1; return eval 'my $n = $n + 10; eval q{my $n = $n + 100; $n}'; }
sub reread_list   { my ($p1, $p2) = (1, 2); return eval('my ($p1, $p2) = ($p2, $p1); "$p1 $p2"') . " $p1 $p2"; }
sub reread_loop   { my $l = 0; my @r; for (1 .. 3) { push @r, eval 'my $l = $l + $_; $l'; $l += 10; } return "@r"; }
sub reread_quote  { my $captures = { '$v' => \"V" }; return eval 'my $v = ${$captures->{q{$v}}}; my $captures = "own"; "$v $captures"'; }
my $fl9 = 100;
print "reread_file=",   eval('my $fl9 = $fl9 + 1; $fl9'), " $fl9\n";
print "reread_init=",   reread_init(),   "\n";
print "reread_block=",  reread_block(),  "\n";
print "reread_arg=",    reread_arg("a"), "\n";
print "reread_write=",  reread_write(),  "\n";
print "reread_nested=", reread_nested(), "\n";
print "reread_list=",   reread_list(),   "\n";
print "reread_loop=",   reread_loop(),   "\n";
print "reread_quote=",  reread_quote(),  "\n";
print "pkg_amp=",       (eval { pkg_amp() } // "DIED"),       "\n";
print "pkg_arrow=",     (eval { pkg_arrow() } // "DIED"),     "\n";
print "pkg_other=",     (eval { pkg_other() } // "DIED"),     "\n";
print "pkg_bench=",     (eval { pkg_bench() } // "DIED"),     "\n";
print "pkg_scalar=",    (eval { pkg_scalar() } // "DIED"),    "\n";
print "pkg_array=",     (eval { pkg_array() } // "DIED"),     "\n";
print "pkg_global=",    (eval { pkg_global() } // "DIED"),    "\n";
print "pkg_promoted=",  (eval { pkg_promoted() } // "DIED"),  "\n";
print "pkg_made=",      (eval { pkg_made() } // "DIED"),      "\n";
# #2630 (s543h): a file lexical a named sub captures, whose name the file ALSO
# spells qualified (the #470 mangle), is still the caller's lexical to a
# package-switching eval -- in the capturing sub, in a sub that does not name
# it, at file level, a block lexical; an eval BEFORE the declaration and one in
# a sub defined above it see the package global; the package global keeps its
# own value (the alias wrote the lexical into it).
$main::qd9 = "glob";
print "qual_before=", eval(q{$qd9}), "\n";
sub qual_early { eval q{package Other; $qd9} }
my $qd9 = "lex";
$Other::qd9 = "pkg";
sub qual_cap  { my $x = $qd9; eval q{package Other; $qd9} }
sub qual_free { eval q{package Other; $qd9} }
print "qual_cap=",   qual_cap(),   "\n";
print "qual_free=",  qual_free(),  "\n";
print "qual_file=",  eval(q{package Other; $qd9}), "\n";
print "qual_early=", qual_early(), "\n";
print "qual_main=$main::qd9\n";
{ my $qb9 = "blk"; $Other::qb9 = "pkgb"; sub qual_bsub { $qb9 }
  print "qual_block=", eval(q{package Other; $qb9}), "\n"; }
# #2557 (s543h): the caller's `our` alias names its DECLARING package's
# variable inside a package-switching eval -- a block region, a single region,
# an array (and `$#`), `local our`, a third package, a nested eval, a write,
# the interpolated spellings, a `package` section of the file, the same text
# at a site with and one without the alias (the cache); a closed block's
# `our` and an inner `my` are the controls.
our $oa9 = "main-o"; $Foo::oa9 = "foo-o"; $Other::oa9 = "other-o";
print "our_block=",  eval('sub { package Foo; $oa9 }')->(), "\n";
print "our_region=", eval('package Foo; $oa9'), "\n";
our @ob9 = (1, 2); @Foo::ob9 = (7, 8, 9);
print "our_array=",  eval('package Foo; scalar(@ob9) . $#ob9'), "\n";
sub our_local { local our $oc9 = "loc"; eval 'package Foo; $oc9' }
$Foo::oc9 = "foo-oc";
print "our_local=",  our_local(), "\n";
package A9; our $od9 = "a-od"; package main; $C9::od9 = "c-od";
print "our_third=",  eval('package C9; $od9'), "\n";
print "our_nested=", eval(q{eval 'package Foo; $oa9'}), "\n";
eval 'package Foo; $oa9 = "written"'; print "our_write=$oa9 $Foo::oa9\n"; $oa9 = "main-o";
print "our_interp=", eval('package Foo; "${oa9}|$oa9"'), "\n";
{ our $oe9; } $main::oe9 = "main-oe"; $Foo::oe9 = "foo-oe";
print "our_closed=", eval('package Foo; $oe9'), "\n";
{ my $oa9 = "my-o"; print "our_myshadow=", eval('package Foo; $oa9'), "\n"; }
sub our_wo { eval 'package Foo; $of9' }
$Foo::of9 = "foo-of"; $main::of9 = "main-of";
our $of9;
sub our_w  { eval 'package Foo; $of9' }
print "our_cache=", our_w(), " ", our_wo(), " ", our_w(), "\n";
package Other; print "our_section=", eval('$oa9'), "\n"; package main;

print "basic_read=",    basic_read(),    "\n";
print "write_back=",    write_back(),    "\n";
print "closure_eval=",  closure_eval(),  "\n";
print "array_elem=",    array_elem(),    "\n";
print "hash_elem=",     hash_elem(),     "\n";
print "ab_lexical=",    ab_lexical(),    "\n";
print "ab_ordinary=",   ab_ordinary(),   "\n";
print "user_my=",       user_my(),       "\n";
print "user_my_a=",     user_my_a(),     "\n";
print "eval_shadow=",   eval_shadow(),   "\n";
print "return_eval=",   return_eval(),   "\n";
print "two_lex=",       two_lex(),       "\n";
print "nested_block=",  nested_block(),  "\n";
print "many_vars=",     many_vars(),     "\n";
print "array_full=",    array_full(),    "\n";
print "array_set=",     array_set(),     "\n";
print "hash_full=",     hash_full(),     "\n";
print "mixed_global=",  mixed_global(),  "\n";
print "list_return=",   list_return(),   "\n";
print "recursion=",     recursion(4),    "\n";
print "two_evals=",     two_evals(),     "\n";
print "mod_then_read=", mod_then_read(), "\n";
print "deep_closure=",  deep_closure(),  "\n";
print "closure_rename=",closure_rename(),"\n";
print "foreach_var=",   foreach_var(),   "\n";
print "local_var=",     local_var(),     "\n";
print "magic_under=",   magic_under(),   "\n";
print "magic_args=",    magic_args(1,2,3),"\n";
print "magic_cap=",     magic_cap(),     "\n";
print "eval_in_sort=",  eval_in_sort(),  "\n";
print "sub_in_eval=",   sub_in_eval(),   "\n";
print "cross_eval=",    cross_eval(),    "\n";
PERL

# (tag => expected) — each verified against real perl.
my %expect = (
    basic_read    => '6',
    write_back    => '99',
    closure_eval  => 'got:X',
    array_elem    => '22',
    hash_elem     => '7',
    ab_lexical    => '6',
    ab_ordinary   => '7',
    user_my       => '8',
    user_my_a     => '8',
    eval_shadow   => '5',
    return_eval   => '7',
    two_lex       => '6',
    nested_block  => '3',
    many_vars     => '10',
    array_full    => '1,2,3',
    array_set     => '99 2 3',
    hash_full     => 'a,b',
    mixed_global  => '105',
    list_return   => '2,4,6',
    recursion     => '24',
    two_evals     => '12',
    mod_then_read => '22',
    deep_closure  => 'hi',
    closure_rename => '6/105',
    foreach_var   => '10 20 30',
    local_var     => '42',
    magic_under   => '2,4,6',
    magic_args    => '1,2,3',
    magic_cap     => 'b',
    eval_in_sort  => '1,2,3',
    sub_in_eval   => 'good',
    cross_eval    => '6',
    pkg_amp       => 'ran',
    pkg_arrow     => 'ran',
    pkg_other     => 'ran',
    pkg_bench     => 'bb',
    pkg_scalar    => 'lex',
    pkg_array     => '322',
    pkg_global    => '7',
    pkg_promoted  => 'cap(1)2',
    pkg_made      => 'Made9:cap(2)',
    qual_before   => 'glob',
    qual_cap      => 'lex',
    qual_free     => 'lex',
    qual_file     => 'lex',
    qual_early    => 'pkg',
    qual_main     => 'glob',
    qual_block    => 'blk',
    our_block     => 'main-o',
    our_region    => 'main-o',
    our_array     => '21',
    our_local     => 'loc',
    our_third     => 'a-od',
    our_nested    => 'main-o',
    our_write     => 'written foo-o',
    our_interp    => 'main-o|main-o',
    our_closed    => 'foo-oe',
    our_myshadow  => 'my-o',
    our_cache     => 'main-of foo-of main-of',
    our_section   => 'main-o',
    reread_file   => '101 100',
    reread_init   => '8',
    reread_block  => '6 7',
    reread_arg    => 'a!',
    reread_write  => '5',
    reread_nested => '111',
    reread_list   => '2 1 1 2',
    reread_loop   => '1 12 23',
    reread_quote  => 'V own',
);

plan tests => scalar(keys %expect);

# Transpile + run the combined driver once.
my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
print $fh $driver;
close $fh;
my $cl_code = PCLCore::transpile(qq{$pl2cl --no-cache $pl_file});
my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $cl_fh $cl_code;
close $cl_fh;
my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;

# Parse "tag=value" lines into a hash.
my %got;
for my $line (split /\n/, $out) {
    $got{$1} = $2 if $line =~ /^(\w+)=(.*)$/;
}

for my $tag (sort keys %expect) {
    is($got{$tag} // '<missing>', $expect{$tag},
       "capture: $tag => $expect{$tag}");
}

# --- Deliberate divergences from Perl (documented, not asserted as correct) ---
# These are probed during development and recorded in docs/eval-lexical-capture.md:
#   1. A lexical `my $a` masking a sort block inside the same eval: Perl yields a
#      broken order (the lexical shadows the sort var); PCL sorts correctly.
#   2. Nested string eval (eval 'eval "$x"'): the inner eval cannot capture the
#      outer eval's free variable.
#   3. eval inside a returned closure referencing a var ONLY through the eval
#      string: Perl's closure optimizer never closes over it (sees undef); PCL
#      captures it (more permissive).

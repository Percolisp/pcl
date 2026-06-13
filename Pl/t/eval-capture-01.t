#!/usr/bin/env perl
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

use Test::More;

my $pl2cl   = "./pl2cl";
my $runtime = "cl/pcl-runtime.lisp";

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
);

plan tests => scalar(keys %expect);

# Transpile + run the combined driver once.
my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
print $fh $driver;
close $fh;
my $cl_code = `$pl2cl --no-cache $pl_file 2>&1`;
my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $cl_fh $cl_code;
close $cl_fh;
my $out = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;

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

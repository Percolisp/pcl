#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# method-name-word-01.t — tasks #481, #482 and the block-form arrow.
#
# ONE FACT, three places that used to disagree with it: **a WORD after `->` is
# a METHOD NAME**.  perl allows any identifier there, keywords included, and
# nothing else can stand in that position — so no pass may read such a word as
# something else.  Two passes did, and each one LOST the whole statement:
#
#   * `extract_declarations` stripped `state`/`my`/`our`/`local` as a
#     DECLARATOR, so `$one->state` reached the postfix-`->` handler with
#     nothing after the arrow and the compiler died inside PPI's API
#     ("Can't call method \"content\" on an undefined value") — task #482,
#     Test-Simple's Test2/API/InterceptResult(.t and Event.pm) and, on the
#     board, Test2::Util::Grabber;
#   * the fat-comma pass AUTOQUOTED it, so `is $csv->module => 'M'` turned the
#     method name into a string and left the arrow with no word — task #481,
#     Text-CSV-2.04/t/01_is_pp.t.
#
# Both now ask ONE predicate, `Pl::PExpr::_word_is_method_name`.
#
# The third row family is the same shape one level out: a `(&…)`-prototype
# BLOCK call followed by `->`.  The slurpy `@` consumes juxtaposed TERMS and
# `->` is not one — it is a postfix operator binding to the call's RESULT — so
# `intercept { … }->upgrade` is `intercept(sub{…})->upgrade` in perl.  PCL
# swallowed the arrow into the argument list, which left it at the head of a
# list with nothing before it: "WTF? :-) Expr starts with ->/brace??", the two
# remaining Test-Simple census drops (t/Legacy/Regression/637.t and
# t/Test2/modules/API/InterceptResult.t).
#
# Every expectation below is the live `perl` answer, so a future change to
# either side can only agree or fail.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 20;

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

sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

my $CLS = <<'PL';
use feature 'state';
package C;
sub new    { my $c = shift; bless {}, ref($c) || $c }
sub module { "M" }
sub state  { "S" }
sub my     { "MY" }
sub local  { "LO" }
sub our    { "OU" }
sub print  { "PR" }
sub self   { my $s = shift; $s }
package main;
sub is { print "is(", join("|", @_), ")\n" }
my $csv = C->new;
PL

# ---- #482: a KEYWORD is a legal method name --------------------------------

both_agree($CLS . 'print $csv->state, "\n";',
           '#482: `->state` is a method call, not a state declaration');

both_agree($CLS . 'print join("|", $csv->my, $csv->our, $csv->local), "\n";',
           '#482: `->my`, `->our`, `->local` are method calls');

both_agree($CLS . 'print $csv->self->state, "\n";',
           '#482: a keyword method name in the MIDDLE of a chain');

# ---- #481: the fat comma does not autoquote a method name ------------------

both_agree($CLS . 'is $csv->module => "M";',
           '#481: `is $o->module => "M"` passes the method RESULT');

both_agree($CLS . 'is $csv->state => "S";',
           '#481 + #482 together: a keyword method name before `=>`');

both_agree($CLS . 'my @l = (a => $csv->module); print "@l\n";',
           '#481: a method call on the RIGHT of a fat comma');

# ---- the negatives the two fixes must NOT change ---------------------------

both_agree($CLS . 'is FOO => 1;',
           'negative: a plain bareword before `=>` still autoquotes');

both_agree($CLS . 'my %h = (-b => 2, key => 3);'
                . ' print join(",", map { "$_=$h{$_}" } sort keys %h), "\n";',
           'negative: `-b =>` and `key =>` still autoquote');

both_agree($CLS . 'my %g = (module => 9, state => 10, my => 11);'
                . ' print "$g{module} $g{state} $g{my}\n";',
           'negative: keyword-shaped fat-comma KEYS still autoquote');

both_agree($CLS . 'sub st { state $n = 0; return ++$n } print st(), st(), "\n";',
           'negative: `state $n` in a sub body is still a declaration');

both_agree($CLS . 'my $z = 5; our $w = 6; { local $\ = undef; }'
                . ' print "$z $w\n";',
           'negative: my / our / local declarations still work');

# ---- the block-form arrow --------------------------------------------------

my $BLK = <<'PL';
package X;
sub new     { my $c = shift; bless { n => (shift // 0) }, ref($c)||$c }
sub upgrade { "UP" }
sub n       { $_[0]{n} }
package main;
sub one  (&)   { my $c = shift; return X->new(scalar(@_)) }
sub many (&;@) { my $c = shift; return X->new(scalar(@_)) }
sub arr  (&;@) { my $c = shift; return [10, 20] }
sub hsh  (&;@) { my $c = shift; return { a => 30 } }
sub blk  (&;@) { my ($c, @r) = @_; return "blk(" . scalar(@r) . ")" }
PL

both_agree($BLK . 'print one { 5 }->upgrade, "\n";',
           'block-arrow: `(&)` call then `->method`');

both_agree($BLK . 'print many { 5 }->n, "\n";',
           'block-arrow: `(&;@)` call then `->method` — the slurp STOPS at `->`');

both_agree($BLK . 'print arr { 5 }->[1], "\n"; print hsh { 5 }->{a}, "\n";',
           'block-arrow: `->[0]` and `->{k}` after a block call');

# The inverse: the slurpy @ still slurps a juxtaposed TERM, and a comma still
# ends it — neither may change.
both_agree($BLK . 'print blk { 5 } 1, 2, "\n";',
           'inverse: a juxtaposed list is still slurped');

both_agree($BLK . 'print( (many { 5 }, 1, 2)[0]->n, "\n");',
           'inverse: a comma still ends the slurp');

# s501b (task #2083): a METHOD whose name is a named-unary builtin, called
# WITH parens.  `$x->length()` got the builtin's `$_` default as an argument
# (and `$o->shift()` its @ARGV, `$o->length(1,2)` the comma collapse), so
# Math::BigInt::length saw a stray argument and carped "Rounding is not
# supported"; Math::BigFloat's bdiv printed it on every call.
both_agree('package Foo; sub new { bless {}, shift }'
         . ' sub length { print "length n=", scalar(@_), " last=", (ref $_[-1] ? "obj" : $_[-1]), "\n"; 3 }'
         . ' sub lc { print "lc n=", scalar(@_), "\n" } sub shift { print "shift n=", scalar(@_), "\n" }'
         . ' sub split { print "split n=", scalar(@_), "\n" }'
         . ' package main; $_ = "dflt"; my $o = Foo->new;'
         . ' $o->length(); $o->length; $o->lc(); $o->shift(); $o->split(); $o->length(1, 2); Foo->length();'
         . ' print length(), " ", length("abcd"), "\n";',
           '#2083: $o->length() / ->lc() / ->shift() / ->split() get no builtin default or collapse');

# s543c (task #333, closed; fixed by #481's `_word_is_method_name`, verified on
# 7ea827a6): a CLASS-method name before `=>` -- `(Foo->x => 1)` -- is a call, as
# the instance spelling above is; the plain and the qualified bareword are the
# negatives (`x =>` autoquotes, `Foo::x =>` is a call in perl too).
both_agree('package Foo; sub x { "X" } package main;'
         . ' my %h = (Foo->x => 1); print join(",", %h), "|\n";'
         . ' my @l = (Foo->x => 1); print "@l|\n";'
         . ' my $o = bless {}, "Foo"; my @m = ($o->x => 2); print "@m|\n";'
         . ' my @q = (x => 1); my @r = (Foo::x => 1); print "@q|@r|\n";',
           '#333: `(Foo->x => 1)` calls the class method before a fat comma');

# s543x (#5515): the same fact for the word BEFORE `=>` -- the fat comma quotes
# the identifier on its left whatever it spells, so a declarator word there is a
# STRING.  extract_declarations (and v1's expression statement, for `state`)
# stripped it as a declarator, which dropped the key and moved the fat comma
# onto the word before it: `f 1, state => 5` passed (1, 5), and `f state => 7;`
# quoted `f` itself -- the call became a void list, gone with no message.  ONE
# predicate, `Pl::PExpr::_fat_comma_quotes_word`.
both_agree('use feature "state"; sub f { print join(",", @_), "|\n" }'
         . ' f 1, state => 5; f 1, my => 6; f our => 7; f local => 8, 1;'
         . ' f state => 1, my => 2, our => 3, local => 4;'
         . ' print 1, state => 8, "|\n"; my @a; push @a, state => 1; print "@a|\n";'
         . ' my @m = map { $_ } 1, state => 3; print "@m|\n";'
         . ' sub r { return 1, state => 4 } print join(",", r()), "|\n";'
         . ' sub rq { return state => 4 } print join(",", rq()), "|\n";'
         . ' my $t = 1; f state => 9 if $t; f 1, state'
         . "\n" . ' => 2;',
           '#5515: a declarator word before `=>` in a paren-less list is the string');
# The other side: a REAL declaration in a list keeps its reading (control).
both_agree('use feature "state"; sub f { print join(",", @_), "|\n" } our $g = 2;'
         . ' f 1, my $x = 5; print "$x|\n"; my @b; push @b, my $y; print scalar(@b), "|\n";'
         . ' print state $n = 3; print "|\n"; f(local $_ = 1); f 1, local $g = 3; print "$g|\n";'
         . ' f 1, our $og = 4; print "$og|\n"; my %h = (state => 1, my => 2);'
         . ' print join(",", map { "$_=$h{$_}" } sort keys %h), "|$h{state}|\n";',
           '#5515: a real declaration in a list, and `state =>` in parens, unchanged (control)');

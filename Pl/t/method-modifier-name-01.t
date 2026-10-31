#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# method-modifier-name-01.t -- task #4083: a METHOD named like a statement
# modifier (`$factory->foreach(...)`, `$f->if`, `->while`, `->unless`,
# `->until`, `->for`) is a method call, never `STATEMENT MODIFIER COND`.
#
# perl (toke.c): a word after `->` is read in XOPERATOR state as a METHOD name
# -- no keyword lookup -- and a word before `=>` is a string (the fat-arrow
# test runs before the keyword lookup).  PCL's modifier splits (twelve sites in
# Pl/Parser.pm, Pl/Parser2.pm, Pl/VarAnnotator.pm) took the first top-level Word
# whose CONTENT was one of the six, so `print $f->foreach(1, 2)` was split as
# `foreach (1, 2) { print $f-> }` and dropped, and `my @b = (1), while => 2;`
# was an endless loop.  Every site now asks ONE token predicate,
# Pl::PExpr::Config::is_statement_modifier_word, which answers NO after `->`
# and before `=>`.
#
# NOT HERE (a PPI row, filed for the PPI batch): `$f->if(1)` / `->while(` /
# `->unless(` / `->until(` / `->elsif(` -- PPI's Lexer builds the parenthesis
# after those words as a Structure::Condition even after `->`, and PCL's
# method-call lowering takes a List.  The rows below use `->foreach(` /
# `->for(` (a List to PPI) and the paren-less spellings, which reach the split
# alone.  Every expectation is the live `perl` answer to the same source.

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

plan tests => 9;

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

# The class every row calls: AUTOLOAD answers any method with its own name.
my $F = <<'PL';
package F;
sub AUTOLOAD { our $AUTOLOAD; my $n = $AUTOLOAD; $n =~ s/.*:://;
  return if $n eq 'DESTROY'; "$n(" . join(",", @_[1 .. $#_]) . ")" }
package main;
my $f = bless {}, 'F';
PL

both_agree($F . <<'PL', 'paren-less ->if / ->while / ->unless / ->until in a print list');
print $f->if, "|", $f->while, "|", $f->unless, "|", $f->until, "\n";
PL

both_agree($F . <<'PL', '->foreach(LIST) and ->for(LIST) in a print list');
print $f->foreach(1, 2), " ", $f->for(3), "\n";
PL

both_agree($F . <<'PL', 'the Template::Grammar shape: ->foreach(SLICE) as an anon sub body');
my @a = (0 .. 5);
my $s = sub { $f->foreach(@a[2, 4, 5]) };
print $s->(), "\n";
PL

both_agree($F . <<'PL', 'a keyword-named method call followed by a REAL modifier');
my $x = 1;
print $f->foreach(1), "\n" if $x;
print $f->for(2), "\n" unless !$x;
print $f->while, "\n" foreach 1 .. 2;
my $c = 0; $c++ while $f->until && $c < 3; print "c=$c\n";
PL

both_agree(<<'PL', 'the real modifiers stay modifiers');
my $x = 0;
print "no\n" if $x; print "yes\n" unless $x;
print "e$_\n" for 1 .. 2; print "f$_\n" foreach 3;
my $i = 0; do { $i++ } while ($i < 3); print "i=$i\n";
my $j = 0; $j++ while $j < 4; my $k = 9; $k-- until $k < 5; print "j=$j k=$k\n";
PL

both_agree($F . <<'PL', 'a class method and a method inside a map block');
print F->foreach(4), "\n";
my @r = map { $f->for($_) } 1 .. 2; print "@r\n";
PL

both_agree($F . <<'PL', 'the declaration paths: in-sub my with a modifier, our, return');
sub g { my $v = $f->foreach(7) if $_[0]; return defined $v ? $v : 'undef' }
our $o = $f->for(8);
sub h { return $f->until }
print g(1), " ", g(0), " $o ", h(), "\n";
PL

both_agree(<<'PL', 'a modifier word before => is a string');
push my @p, if => 5; print "@p\n";
my @q = (unless => 1, until => 2); print "@q\n";
my %h = (if => 3, foreach => 4); print join(",", map { "$_=$h{$_}" } sort keys %h), "\n";
PL

both_agree(<<'PL', 'a modifier word as a hash key and a qualified sub name');
sub if { "IF(@_)" }
my %h; $h{while} = 5; print main::if(2), " $h{while}\n";
PL

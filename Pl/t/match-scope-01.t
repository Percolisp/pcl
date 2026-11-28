#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# match-scope-01.t -- task #5545 (s543t2): the capture scope of a sub frame
# (#932's sub half, s543t) is built only for a body that can WRITE the match
# state inline.  Three rows:
#   1. THE REGISTRY: every EXPORTED runtime op from which the one writer,
#      %p-match-record, is reachable (cl/pcl-runtime.lisp's call graph, read
#      statically) is listed in Pl::Manifest::%MATCH_STATE_HEAD -- a new
#      writer that the compiler's walk would not see fails here, not as a
#      silent wrong at run time;
#   2. THE EMISSION: a body without a match carries no scope (a named sub's
#      `:needs` lacks `:match`, an anonymous sub's frame is `:no-match`), a
#      body with one keeps it;
#   3. THE RUN: a sub without a match that calls one with a match leaves the
#      caller's $1, and a sub that only READS $1 reads the caller's (perl
#      oracle).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use lib "$RealBin/../..";
use PCLCore;
use Pl::Manifest ();

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 3;

# ---- 1. the registry against the runtime's call graph ---------------------
{
  open my $h, '<', $runtime or die "$runtime: $!";
  my $src = do { local $/; <$h> };
  close $h;
  my %exported;
  $exported{lc $1} = 1 while $src =~ /#:([^\s()#|]+)/g;
  # Strings (docstrings), comments and character literals name functions
  # without calling them: one left-to-right pass removes all three (a `;'
  # inside a docstring is not a comment), keeping the newlines.
  $src =~ s{("(?:[^"\\]|\\.)*")|(\#\|.*?\|\#)|(;[^\n]*)|(\#\\.)}
           { defined $1 ? '""' . ("\n" x ($1 =~ tr/\n//))
           : defined $2 ? ("\n" x ($2 =~ tr/\n//)) : defined $3 ? '' : 'CHAR' }gse;
  my (%body, $cur);
  for my $l (split /^/, $src) {
    if ($l =~ /^\((?:defun|defmacro|define-compiler-macro|defmethod)\s+(\(setf\s+)?([^\s()]+)/) {
      $cur = ($1 ? 'setf ' : '') . lc $2;
    }
    elsif ($l =~ /^\S/) { $cur = undef }
    next if !defined $cur;
    $body{$cur} .= $l;
  }
  my %w = map { $_ => 1 }
          grep { $_ ne '%p-match-record' && $body{$_} =~ /\(%p-match-record\s/ } keys %body;
  for (my $changed = 1; $changed; ) {
    $changed = 0;
    for my $n (keys %body) {
      next if $w{$n};
      for my $c (keys %w) {
        next if $c =~ /\Asetf /;
        if ($body{$n} =~ /[(\s'`,]\Q$c\E[\s)]/) { $w{$n} = 1; $changed = 1; last }
      }
    }
  }
  my @reach = sort grep { $exported{$_} } keys %w;
  my @missing = grep { !$Pl::Manifest::MATCH_STATE_HEAD{$_} } @reach;
  ok(@reach && !@missing,
     "#5545: every exported op reaching %p-match-record is in Pl::Manifest::%MATCH_STATE_HEAD (reach: @reach)"
     . (@missing ? " -- MISSING: @missing" : ''));
}

# ---- 2. the emission ------------------------------------------------------
{
  my $prog = <<'PL';
sub plain { $_[0] + 1 }
sub matches { my $s = shift; $s =~ /(\d)/; $1 }
my $a1 = sub { 2 };
my $a2 = sub { "x" =~ /(x)/; $1 };
print plain(1), matches("a7"), $a1->(), $a2->(), "\n";
PL
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
  print $fh $prog; close $fh;
  my $cl = PCLCore::strip_loc(PCLCore::transpile("$pl2cl $pl_file"));
  my ($plain)   = $cl =~ /\(p-sub pl-plain\s*\n[^\n]*\n\s*(\([^\n]*\))/;
  my ($matches) = $cl =~ /\(p-sub pl-matches\s*\n[^\n]*\n\s*(\([^\n]*\))/;
  my @frames = $cl =~ /\(p-sub-frame(\s+:no-match)?/g;
  ok(defined $plain && $plain =~ /:needs \(\)/ && $plain !~ /:match/
     && defined $matches && $matches =~ /:needs \([^)]*:match\)/
     && @frames == 2 && defined $frames[0] && !defined $frames[1],
     '#5545: a named sub without a match has no :match in its :needs, one with a match has it; an anonymous sub without one is (p-sub-frame :no-match ...), with one the plain frame'
     . " (plain: " . ($plain // 'none') . "; matches: " . ($matches // 'none') . ")");
}

# ---- 3. the run (perl oracle) --------------------------------------------
{
  my $prog = <<'PL';
sub inner { "zz" =~ /(z)/; $1 }
sub nomatch { my $r = inner(); "nm:$r" }
sub reads { "rd:" . (defined $1 ? $1 : "u") }
my $an = sub { my $r = inner(); "an:$r" };
"ABC" =~ /(B)/;
my $v = nomatch(); print "$v [$1]\n";
$v = reads();      print "$v [$1]\n";
$v = $an->();      print "$v [$1]\n";
sub inmap { my @l = map { /(\d)/; $1 } @_; "@l" }
$v = inmap("a1", "b2"); print "$v [$1]\n";
PL
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
  print $fh $prog; close $fh;
  my $want = `perl $pl_file 2>&1`;
  my $cl = PCLCore::transpile("$pl2cl $pl_file");
  my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
  print $cfh $cl; close $cfh;
  my $got = `sbcl @sbcl_rt --load $cl_file 2>&1`;
  $got =~ s/^;.*\n//gm;
  $got =~ s/^PCL Runtime loaded\n//gm;
  $got =~ s/^\s*\n//gm;
  is($got, $want, '#5545: a sub without a match that calls one with a match, a sub that only reads $1, an anon sub, a match inside map: the caller keeps its capture (perl oracle)');
}

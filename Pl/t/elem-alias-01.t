#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# THE ELEMENT-ALIASING BATTERY (docs/boxed-aggregates-design-s455.md phase 0,
# task #816).  Array/hash ELEMENTS are stored as boxes today so that some OTHER
# holder can write through them; the design replaces that with raw storage plus
# ONE in-place promotion at the alias event.  Every phase of that work must
# leave THIS file green, which is what makes the early phases' zero-change bar
# checkable at the semantic level rather than only at the sweep level.
#
# The catalogue is the design's E1-E12 (plus E13, added s458ak), both
# spellings each:
#   E1  f($a[0]) / f($h{k})            -- @_ aliasing of ONE element
#   E2  f(@a) / f(%h)                  -- @_ aliasing of every element
#   E3  for (@a) / for my $x (@a)      -- loop var aliases each element
#   E4  \$a[0] / \$h{k}                -- explicit element ref
#   E5  local $a[0] / local $h{k}      -- save/restore through the slot
#   E6  sort comparator $a/$b          -- READS (perl calls writes undefined)
#   E7  map / grep $_                  -- same family as E3
#   E8  array hole                     -- read does NOT vivify, write DOES
#   E10 readonly array                 -- element write still legal
#   E11 values %h / values @a          -- aliases (task #817)
#   E12 @a[0,1] / @h{...}              -- slice aliases (task #818)
#   E13 @$obj{k} / @$obj[0] on a BLESSED container -- aliases exactly like a
#       plain one, and the class survives it (task #841)
#   plus the NEGATIVES: every COPY position must BREAK aliasing.
# E9 (tie/magic containers) is out of scope by design -- those keep today's
# fully-boxed representation, and their own guards cover them.
#
# The oracle is REAL PERL run on the same program at test time.

use strict;
use warnings;
use Test::More tests => 21;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $pl2cl = './pl2cl';
my $runtime = 'cl/pcl-runtime.lisp';
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

my $prog = <<'PROG';
sub w0 { $_[0] = "W" }
sub w1 { $_[1] = "X" }

# --- E1: one element in @_ position ---------------------------------------
my @a1 = (1,2,3);  w0($a1[1]);                 print "e1a=@a1\n";
my %h1 = (k => 1); w0($h1{k});                 print "e1b=$h1{k}\n";

# --- E2: a whole aggregate flattened into @_ ------------------------------
my @a2 = (1,2,3);  w1(@a2);                    print "e2a=@a2\n";
my %h2 = (k => 1); w1(%h2);                    print "e2b=$h2{k}\n";

# --- E3: foreach loop variable --------------------------------------------
my @a3 = (1,2);        for (@a3) { $_ *= 10 }         print "e3a=@a3\n";
my @b3 = (1,2);        for my $x (@b3) { $x *= 10 }   print "e3b=@b3\n";

# --- E4: explicit element ref ---------------------------------------------
my @a4 = (1,2);   my $r4 = \$a4[0];  $$r4 = "R";      print "e4a=@a4\n";
my %h4 = (k => 1); my $s4 = \$h4{k}; $$s4 = "R";      print "e4b=$h4{k}\n";

# --- E5: local on an element, restored at scope exit ----------------------
our @a5 = (1,2);
sub peek5 { "$a5[0]" }
my $in5 = do { local $a5[0] = "L"; peek5() };
print "e5a=$in5/$a5[0]\n";
our %h5 = (k => 1);
sub peek5h { "$h5{k}" }
my $in5h = do { local $h5{k} = "L"; peek5h() };
print "e5b=$in5h/$h5{k}\n";

# --- E6: sort comparator reads --------------------------------------------
my @a6 = (3,1,2);
my @s6 = sort { $a <=> $b } @a6;          print "e6a=@s6/@a6\n";
my %h6 = (x=>3, y=>1, z=>2);
my @s6b = sort { $h6{$a} <=> $h6{$b} } keys %h6;  print "e6b=@s6b\n";

# --- E7: map / grep $_ ----------------------------------------------------
my @a7 = (1,2);  my @m7 = map { $_ .= "M"; $_ } @a7;   print "e7a=@a7/@m7\n";
my @b7 = (1,2);  my @g7 = grep { $_ .= "G"; 1 } @b7;   print "e7b=@b7/@g7\n";

# --- E8: array holes ------------------------------------------------------
my @a8; $a8[3] = "end";
my $seen8 = 0; for (@a8) { $seen8++ }                  # a READ must not vivify
print "e8a=", scalar(@a8), "/", (exists $a8[1] ? 1 : 0), "/$seen8\n";
my @b8; $b8[3] = "end";
for (@b8) { $_ = "V" if !defined $_ }                  # a WRITE must vivify
print "e8b=", (exists $b8[1] ? 1 : 0), "/", (defined $b8[1] ? $b8[1] : "u"), "\n";

# --- E10: readonly array, element write still legal -----------------------
my @a10 = (1,2);
Internals::SvREADONLY(@a10, 1);
my $ok10 = eval { $a10[0] = "Q"; 1 } ? 1 : 0;
print "e10=$ok10/$a10[0]\n";
Internals::SvREADONLY(@a10, 0);

# --- E11: values ----------------------------------------------------------
my %h11 = (k => 1);  for (values %h11) { $_ .= "V" }     print "e11a=$h11{k}\n";
my @a11 = (1,2);     for (values @a11) { $_ *= 10 }      print "e11b=@a11\n";

# --- E12: slices ----------------------------------------------------------
my @a12 = (1,2,3);   for (@a12[0,1]) { $_ *= 10 }        print "e12a=@a12\n";
my %h12 = (x=>1, y=>2);
for (@h12{'x','y'}) { $_ += 100 }                        print "e12b=$h12{x} $h12{y}\n";
my @a12c = (1); for (@a12c[0,2]) { $_ = 9 }              # slice hole vivifies
print "e12c=", scalar(@a12c), "/", join(",", map { defined $_ ? $_ : "u" } @a12c), "\n";
my %h12d = (x=>1); for (@h12d{'x','zz'}) { $_ = 8 }      # missing key vivifies
print "e12d=", scalar(keys %h12d), "/$h12d{zz}\n";
my $r12 = [1,2,3]; for (@{$r12}[0,1]) { $_ *= 2 }        print "e12e=@$r12\n";

# --- E13: BLESSING DOES NOT CHANGE ELEMENT ALIASING (task #841) -----------
# A blessed hash is an ordinary hash with a stash attached; its ELEMENTS are
# ordinary lvalues.  PCL used to refuse to alias them (the :__class__ guard),
# which was a silent wrong.  The class key must stay invisible throughout.
my $ob13 = bless { k => "v" }, "C13";
w0(@$ob13{"k"});                                         print "e13a=$ob13->{k}\n";
my $oc13 = bless { a => "x", b => "y" }, "C13";
for my $v (@$oc13{'a','b'}) { $v .= "!" }                print "e13b=$oc13->{a} $oc13->{b}\n";
my $od13 = bless { a => "x" }, "C13";
for my $v (values %$od13) { $v .= "!" }                  print "e13c=$od13->{a}\n";
my $oe13 = bless [ "p", "q" ], "C13";
w0(@$oe13[0]);                                           print "e13d=$oe13->[0]\n";
my $of13 = bless { a => "x" }, "C13";
my @cb13 = @$of13{'a'};  $cb13[0] = "C";                 # a COPY still breaks it
print "e13e=", ref($ob13), "/", join(",", sort keys %$ob13), "/$of13->{a}\n";

# --- NEGATIVES: every copy position breaks aliasing -----------------------
my @an = (1,2,3);
my @c1 = @an;             $c1[0] = "C";
my @c2; push @c2, @an;    $c2[0] = "C";
my @c3 = sort { $a <=> $b } @an;  $c3[0] = "C";
my ($c4) = @an;           $c4 = "C";
my %hn = (k => 1);
my @c5 = values %hn;      $c5[0] = "C";
my @c6 = @an[0,1];        $c6[0] = "C";
my @c7 = @hn{'k'};        $c7[0] = "C";
my %hn2 = (k => 1);
while (my ($k, $v) = each %hn2) { $v = "C" }
my @c9 = keys %hn;        $c9[0] = "C";
print "neg=@an/$hn{k}/$hn2{k}/", join(",", sort keys %hn), "\n";

# --- the RHS is evaluated BEFORE the first store (perl's aassign copies) ---
my ($sa, $sb) = (1,2);  ($sa, $sb) = ($sb, $sa);
my @sw = (1,2);         @sw[0,1] = @sw[1,0];
my %sh = (p=>1, q=>2);  @sh{'p','q'} = @sh{'q','p'};
my @sy = (1,2);         @sy = @sy[1,0];
my @sz = (1,2,3);       @sz[0,1] = @sz[1,2];
print "swap=$sa:$sb/@sw/$sh{p}:$sh{q}/@sy/@sz\n";

# read positions must not vivify
my @nv = (1);  my @rv = @nv[0,5];
my %nvh = (k=>1); my @rvh = @nvh{'k','nope'};
print "nov=", scalar(@nv), "/", scalar(keys %nvh), "\n";

print "done\n";
PROG

my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
print $fh $prog;
close $fh;

# Oracle: real perl on the same program.
my $perl_out = `perl $pl_file 2>&1`;

# PCL side.
my $cl_code = PCLCore::transpile(qq{$pl2cl --no-cache $pl_file});
my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp');
print $cl_fh $cl_code;
close $cl_fh;
my $pcl_out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
$pcl_out =~ s/^;.*\n//gm;
$pcl_out =~ s/PCL Runtime loaded\n?//g;
$pcl_out =~ s/STYLE-WARNING.*\n//g;
unlink $pl_file, $cl_file;

# BOTH sides must have run to completion, or a keyed compare could pass
# vacuously on undef == undef.
is(($perl_out =~ /^done$/m ? 1 : 0) . ($pcl_out =~ /^done$/m ? 1 : 0), '11',
   'perl oracle AND PCL both ran the battery to completion')
  or diag("perl:\n$perl_out\nPCL:\n$pcl_out");

my %perl = map { /^([^=]+)=(.*)$/ ? ($1 => $2) : () } split /\n/, $perl_out;
my %pcl  = map { /^([^=]+)=(.*)$/ ? ($1 => $2) : () } split /\n/, $pcl_out;

is($pcl{e1a},  $perl{e1a},  'E1 $_[0] writes through to an ARRAY element');
is($pcl{e1b},  $perl{e1b},  'E1 $_[0] writes through to a HASH element');
is($pcl{e2a},  $perl{e2a},  'E2 f(@a) -- @_ aliases every array element');
is($pcl{e2b},  $perl{e2b},  'E2 f(%h) -- @_ aliases the hash VALUE halves');
is($pcl{e3a},  $perl{e3a},  'E3 for (@a) writes through $_');
is($pcl{e3b},  $perl{e3b},  'E3 for my $x (@a) writes through $x');
is($pcl{e4a},  $perl{e4a},  'E4 \\$a[0] stays live after the ref is taken');
is($pcl{e4b},  $perl{e4b},  'E4 \\$h{k} stays live after the ref is taken');
is($pcl{e5a},  $perl{e5a},  'E5 local $a[0] is seen by a callee and restored');
is($pcl{e5b},  $perl{e5b},  'E5 local $h{k} is seen by a callee and restored');
is("$pcl{e6a}|$pcl{e6b}", "$perl{e6a}|$perl{e6b}", 'E6 sort comparator element reads');
is("$pcl{e7a}|$pcl{e7b}", "$perl{e7a}|$perl{e7b}", 'E7 map/grep $_ writes through');
is("$pcl{e8a}|$pcl{e8b}", "$perl{e8a}|$perl{e8b}", 'E8 a hole reads without vivifying and vivifies on write');
is($pcl{e10},  $perl{e10}, 'E10 an element write into a readonly array');
is("$pcl{e11a}|$pcl{e11b}", "$perl{e11a}|$perl{e11b}", 'E11 values %h / values @a alias (#817)');
is("$pcl{e12a}|$pcl{e12b}|$pcl{e12c}|$pcl{e12d}|$pcl{e12e}",
   "$perl{e12a}|$perl{e12b}|$perl{e12c}|$perl{e12d}|$perl{e12e}",
   'E12 array/hash/ref slices alias, holes and missing keys vivify (#818)');
is("$pcl{e13a}|$pcl{e13b}|$pcl{e13c}|$pcl{e13d}",
   "$perl{e13a}|$perl{e13b}|$perl{e13c}|$perl{e13d}",
   'E13 a BLESSED hash/array aliases its elements exactly like a plain one (#841)');
is($pcl{e13e}, $perl{e13e},
   'E13 the bless class survives element aliasing and is never a visible key');
is("$pcl{neg}|$pcl{nov}", "$perl{neg}|$perl{nov}",
   'NEGATIVES: every copy position breaks aliasing; reads never vivify');
is($pcl{swap}, $perl{swap},
   'a self-referential list/slice assignment reads its whole RHS before storing');

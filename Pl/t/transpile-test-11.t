#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Transpile tests part 11 -- s543c, the verification batch (2026-11-21).
# Every row is the OWN REPRODUCER of a task the batch closed because it now
# agrees with perl, and that had no Pl/t row of its own: the fix came from an
# earlier change (named in each task's s543c block), so these rows are the
# guard that keeps it.  Rows are grouped by area, one program (= one SBCL
# launch) per group, and every row compares PCL against perl run on the same
# file at test time (no 5.40-only syntax: CI's perl is 5.38).
# NEW TESTS GO HERE -- the next transpile-test file after -10.

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

plan tests => 8;

my $DIR = tempdir(CLEANUP => 1);

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(DIR => $DIR, SUFFIX => '.pl');
    binmode $fh;
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (PCLCore::transpile FAILS the row on a dropped statement) and run,
# from $DIR so the programs' relative files land there.
sub run_cl {
    my ($pl_file) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl $pl_file");
    my ($cl_fh, $cl_file) = tempfile(DIR => $DIR, SUFFIX => '.lisp');
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `cd $DIR && sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub both_agree {
    my ($code, $desc) = @_;
    my $pl_file = write_pl($code);
    my $perl = `cd $DIR && perl $pl_file 2>&1`;
    $perl =~ s/^\s*\n//gm;
    my $pcl = run_cl($pl_file);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- handles: #1425 print {$$fh}; #2780 a handle `my` embedded in an
#      expression survives a package block --------------------------------------
both_agree(<<'PL', '#1425 print {$$fh} / printf {$$fh} write; #2780 open my $fa survives { package O; }');
my $F = "t11-1425.out";
open(my $fh, ">", $F) or die; my $r = print {$$fh} "x\n"; close $fh;
open my $in, '<', $F or die; my $c = <$in>; close $in; unlink $F;
print "1 r=", (defined $r ? $r : "undef"), " file=[", ($c // ''), "]";
open(my $g, ">", $F) or die; my $r2 = printf {$$g} "%s\n", "y"; close $g;
open $in, '<', $F or die; $c = <$in>; close $in; unlink $F;
print "2 r=", ($r2 // 'undef'), " file=[", ($c // ''), "]";
open my $fa, "<", $0 or die;
{ package O; }
my $cc = 1;
print "3 [", (ref(\$fa) eq 'REF' || ref($fa) eq 'GLOB' ? "handle" : "lost"), "] ", (defined(scalar <$fa>) ? "reads" : "empty"), "\n";
PL

# ---- tie and local: #1483 a tied $_ fires FETCH / STORE for chop, a bare
#      filetest and a plain assignment; #2112 a `local` embedded in an
#      expression statement is restored at the end of its scope ----------------
both_agree(<<'PL', '#1483 tied $_ under chop / -x / assignment; #2112 an embedded local is restored');
package UnderScore;
sub TIESCALAR { bless \my $self, shift }
sub FETCH { die "read  \$_ forbidden\n" }
sub STORE { die "write \$_ forbidden\n" }
package main;
tie $_, 'UnderScore';
for my $t (['Reading', sub { print }], ['Matching', sub { my $x = /badness/ }], ['Concat', sub { $_ .= "a" }],
           ['Chop', sub { chop }], ['Filetest', sub { -x }], ['Assignment', sub { $_ = "Bad" }]) {
  eval { $t->[1]->() };
  printf "%-12s died=%d\n", $t->[0], ($@ ? 1 : 0);
}
untie $_;
our $r = "o"; sub rr { $r } my @s; (local $r = $_), push(@s, rr()) for 1..2; print "@s $r\n";
our $t = "o"; sub tt { $t } my @u; for (1..2) { (local $t = $_), push(@u, tt()); } print "@u $t\n";
PL

# ---- name resolution: #2195 an embedded `my` in a sub beside a later file
#      `my`; #2781 a file `my` masks an earlier `our` inside a named sub;
#      #3490 an embedded `my` is not visible later in its own statement;
#      #3610 "${e}[1]" in a sub is the scalar $e ---------------------------------
both_agree(<<'PL', '#2195 embedded my in a sub; #2781 our/my masking; #3490 own-statement my; #3610 "${e}[1]"');
sub op { open(my $h, "<", $0) or die; $h }
my $h = 5; print "1 ok $h\n";
sub cond { if ((my $g = 1) > 0) { return $g } } my $g = 7; print "2 g=$g ", cond(), "\n";
sub id { $_[0] } sub argm { id(my $k = 2); $k } my $k = 9; print "3 k=$k ", argm(), "\n";
our $x = "G";
my $x = "X";
sub f2 { "$x-" . $x }
print "4 ", f2(), "\n";
sub X::n { shift; $_ = "y" for @_; 1 }
for my $kk (1..2) { print "5 f ", X->n(my $t), " $kk ", ($t // ''), "\n" }
my $xx = 1; for (1..2) { my $l = (my $xx = $xx + 1); print "6 $l $xx\n" }
my $e = 1;
sub rf { return "${e}[1]" }
sub rh { return "${e}{k}" }
print "7 ", rf(), " ", rh(), " ${e}[1]\n";
PL

# ---- eval, construction, references: #1943 a string eval's last expression
#      takes the eval's context; #2540 `[ { g(@l) } ]` is ONE anon hash;
#      #3827 a ref-to-ref chain `$$$a` unwraps to the right level -------------
both_agree(<<'PL', '#1943 scalar eval "%h{...}"; #2540 [ { g(@l) } ]; #3827 $$$a of a ref chain');
my %h = map { $_ => uc $_ } 'a'..'z';
print "1 ", scalar(eval "%h{'c','d','e'}"), " ", scalar(eval "\@h{'c','d','e'}"), "\n";
my $s = eval "(4,5,6)"; my @l = eval "(4,5,6)"; print "2 $s ", scalar(@l), "\n";
sub g { map { $_ => 1 } @_ }   my @gl = qw(a b);
print "3 ", scalar(@{ [ { g(@gl) } ] }), " ", ref([ { g(@gl) } ]->[0]), "\n";
use Scalar::Util qw(refaddr);
my $a = \\\\\'foo'; my $b = $$$a;
my @lv = ($a, $$a, $$$a, $$$$a);
print "4 ", join(" ", map { refaddr($lv[$_]) == refaddr($b) ? "L$_=b" : "L$_" } 0..3), "\n";
PL

# ---- #4302 a CR inside a string literal is a character of the program
#      (PPI's tokenizer used to rewrite CR LF / CR to LF): string-eval text
#      and a raw-CR source file ----------------------------------------------------
both_agree(<<'PL', '#4302 CR LF / CR inside quotes survive a string eval');
my $s = eval "'a\r\nb'";   my $t = eval "\"a\r\nb\""; my $v = eval "q{a\r\r\nb}"; my $w = eval "'c\rd'";
print join(" ", length($s), length($t), length($v), length($w), ($w =~ /\r/ ? "cr" : "nocr")), "\n";
PL
both_agree("my \$x = q{a\r\nb}; my \$y = \"c\rd\";\nprint length(\$x), \" \", length(\$y), \" \", (\$y =~ /\\r/ ? \"cr\" : \"nocr\"), \"\\n\";\n"
         . "my \@d = <DATA>; print scalar(\@d), \" \", (\$d[0] =~ /\\r\\n\\z/ ? \"crlf\" : \"lf\"), \"\\n\";\n__DATA__\nd1\r\nd2\n",
           '#4302 a raw CR in the SOURCE: q{} / "" literals and __DATA__ keep it');

# ---- modules: #1617 core File::Copy loads and copies; #2012 File::Temp's
#      tempdir(CLEANUP => 1) without a template ----------------------------------
both_agree(<<'PL', '#1617 core File::Copy: copy / move / cp');
require File::Copy;
print "loaded\n";
open my $o, '>', "t11-1617.src" or die; print $o "data\n"; close $o;
print "copy=", (File::Copy::copy("t11-1617.src", "t11-1617.dst") ? 1 : 0), "\n";
open my $i, '<', "t11-1617.dst" or die; print "dst=", scalar <$i>; close $i;
print "move=", (File::Copy::move("t11-1617.dst", "t11-1617.mv") ? 1 : 0), " exists=", (-e "t11-1617.mv" ? 1 : 0), "\n";
print "cp=", (File::Copy::cp("t11-1617.src", "t11-1617.cp") ? 1 : 0), "\n";
unlink "t11-1617.src", "t11-1617.mv", "t11-1617.cp";
PL
both_agree(<<'PL', '#2012 File::Temp tempdir(CLEANUP => 1) with no template');
use File::Temp qw(tempdir tempfile);
my $d = tempdir(CLEANUP => 1);
print "dir=", (-d $d ? "ok" : "FAIL"), " abs=", ($d =~ m{^/} ? 1 : 0), "\n";
my ($fh, $fn) = tempfile(DIR => $d); print "file=", (-f $fn ? "ok" : "FAIL"), "\n";
PL

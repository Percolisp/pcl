#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# bind-operand-ctx-01.t — task #2102: the BOUND OPERAND of `=~` / `!~` is
# evaluated in SCALAR context, whatever context the match itself is in.
#
# perlop says so, and PCL's `child_context` had no arm for the binding
# operator, so the operand INHERITED whatever context the `=~` sat in.  Inside
# a print argument list or on the RHS of a list assignment that is LIST
# context, and a context-sensitive operand then handed the match an ARRAY:
#
#     print "[", `echo hello` =~ s/\n//r, "]"      ->  [ARRAY(0x1)]
#     my ($w) = `echo one two` =~ /(\w+)$/;        ->  $w empty
#
# — silently, on two everyday shapes.  The whole operand family was affected,
# not just backticks: a list-returning sub call, `<$fh>`, `localtime`,
# `reverse`, `join`.  One arm at the binding operator's own node fixes all of
# them; nothing keys on the operand's KIND.
#
# Child 1 (the pattern / substitution) keeps the match's own context, so a
# list-context `m//g` still collects every match and `my %h = $s =~ /(k)=(v)/g`
# still builds the hash — rows 6-9 are those negatives.
#
# Every expectation is the live `perl` answer.  Inverse-verified on a
# 5ce155cb extraction: rows 1-5 fail there, 6-9 pass.

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
    is($pcl, $perl, $desc);
}

my $PRELUDE = <<'P';
sub ctx { wantarray ? "L" : defined(wantarray) ? "S" : "V" }
sub lst { ("one two", "x") }
P

# ---- the bug: a context-sensitive operand in a LIST-context position -------

both_agree($PRELUDE . <<'P', 'a sub-call operand of =~ runs in scalar context inside a print list');
print "[", ctx() =~ s/S/s/r, "]\n";
print "[", lst() =~ s/one/1/r, "]\n";
print "[", reverse("ab") =~ s/b/B/r, "]\n";
P

both_agree($PRELUDE . <<'P', 'and on the RHS of a LIST assignment');
my ($a1) = ctx() =~ /(\w)/;   print "$a1\n";
my ($a2) = lst() =~ /(\w+)$/; print "$a2\n";
my ($a3) = reverse("abc") =~ /(\w)/; print "$a3\n";
P

both_agree(<<'P', 'a BACKTICK operand: the reported shape');
print "[", `echo hello` =~ s/\n//r, "]\n";
my ($w) = `echo one two` =~ /(\w+)$/; print "$w\n";
P

both_agree(<<'P', 'a <$fh> operand reads ONE record, not the file');
my $f = "/tmp/pcl-bindctx-$$.dat";
open(my $w, ">", $f) or die; print $w "hello world\nsecond\n"; close $w;
open(my $h, "<", $f) or die; print "[", <$h> =~ s/hello/HI/r, "]"; close $h;
open(my $h2, "<", $f) or die; my ($c) = <$h2> =~ /(\w+)/; print "$c\n"; close $h2;
unlink $f;
P

both_agree($PRELUDE . <<'P', '!~ and tr///r bind their operand the same way');
print "[", (ctx() !~ /zz/ ? "nomatch" : "match"), "]\n";
print "[", ctx() =~ tr/SL/sl/r, "]\n";
P

# ---- the negatives: the PATTERN side keeps the match's own context --------

both_agree(<<'P', 'negative: a list-context m//g still collects every match');
my $s = "a1b2c3";
my @all = $s =~ /(\d)/g;        print scalar(@all), " @all\n";
my $cnt = () = $s =~ /\d/g;     print "$cnt\n";
my @caps = "pq" =~ /(p)(q)/;    print "@caps\n";
P

both_agree(<<'P', 'negative: a hash built from a list-context m//g');
my %h = ("k1=v1;k2=v2" =~ /(\w+)=(\w+)/g);
print join(",", map { "$_=$h{$_}" } sort keys %h), "\n";
P

both_agree(<<'P', 'negative: an in-place s/// and tr/// on a plain scalar');
my $t = "xyz"; $t =~ s/y/Y/; print "$t\n";
my $u = "AbC"; print $u =~ tr/a-z//, "\n";
P

both_agree(<<'P', 'negative: a match inside grep, and a list-slice of a match');
my @l = ("a".."c"); my $n = grep { $_ =~ /b/ } @l; print "$n\n";
my @arr = ("aa bb", "zz");     print +($arr[0] =~ /(\w+)/)[0], "\n";
P

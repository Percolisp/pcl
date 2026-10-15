#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# stable-bool-01.t - perl 5.36+ STABLE BOOLEANS (task #3242,
# docs/stable-booleans-design-s516.md).  perl's boolean is its shared "1" / ""
# string buffer: `builtin::is_bool` is true for what a boolean producer returns
# and for every copy that shares it, false for anything that makes a new
# value.  PCL's boolean is the same thing -- two interned strings returned by
# p-bool.  Every row is compared with perl's own output (the oracle runs the
# same program; stderr is not compared: 5.36-5.38 warn "experimental" on
# builtin::).  JSON::PP's true/false is the consumer that matters: the rows run
# the CORE JSON::PP and the board dist's 4.18.

use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $root    = "$RealBin/../..";
my $pl2cl   = "$root/pl2cl";
my @sbcl_rt = PCLCore::sbcl_prefix("$root/cl/pcl-runtime.lisp");
plan skip_all => "perl $] has no builtin::is_bool" if $] < 5.036;

plan tests => 10;

sub run_both {
    my ($code) = @_;
    my ($fh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "use v5.36; no warnings; use builtin qw(is_bool true false);\n$code";
    close $fh;
    my $perl = `$^X $pl 2>/dev/null`;
    my $cl = `$pl2cl $pl 2>/dev/null`;
    my ($cfh, $clf) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl;
    close $cfh;
    my $pcl = `sbcl @sbcl_rt --load $clf 2>/dev/null`;
    $pcl =~ s/^;.*\n//gm;
    $pcl =~ s/^PCL Runtime loaded\n//gm;
    return ($pcl, $perl);
}

sub same {
    my ($name, $code) = @_;
    my ($got, $want) = run_both($code);
    is($got, $want, $name);
}

my $B = 'sub b { is_bool($_[0]) ? 1 : 0 } ';

same('producers: ! not, the comparisons, defined, exists, isa, true/false, is_bool, xor, !~',
  $B . 'my ($x, $u, %h, @a) = (3); %h = (k => 1); @a = (1, 0); sub f { 1 }
   print join(",", b(!1), b(!0), b(not 1), b($x == 4), b($x == 3), b($x != 4), b($x < 4), b($x > 4),
     b($x <= 4), b($x >= 4), b("a" eq "b"), b("a" ne "b"), b("a" lt "b"), b("a" gt "b"),
     b("a" le "b"), b("a" ge "b"), b(defined $x), b(defined $u), b(exists $h{k}), b(exists $h{q}),
     b(exists $a[1]), b(exists &f), b(bless({}, "Foo") isa Foo), b(bless({}, "Foo") isa Bar),
     b(true), b(false), b(is_bool(1)), b(1 xor 0), b("abc" !~ /b/)), "\n";');

same('non-producers: && || // <=> cmp, a scalar grep, delete, a number, a string',
  $B . 'my ($u, %h) = (undef, z => 0); my @a = (1, 0, 2);
   print join(",", b(1 && 0), b(0 || 1), b($u // 1), b(1 <=> 2), b("a" cmp "b"),
     b(scalar(grep { $_ } @a)), b(delete $h{z}), b(1), b("1"), b(""), b(0)), "\n";');

same('a boolean survives a copy: scalar, element, hash value, return, local, list assign',
  $B . 'my $b = !!1; my @e = ($b); my %hh = (v => $b); our $g = 5; my $x = 3;
   sub rb { return $x == 3 } my ($l1) = (rb()); my $lg; { local $g = $b; $lg = b($g) }
   my @c = (1 == 1, 1 == 0);
   print join(",", b($b), b($e[0]), b($hh{v}), b(rb()), b($l1), $lg, b($c[0]), b($c[1])), "\n";');

same('and is lost by every new value: +0, . "", .=, ++, sprintf, interpolation, substr',
  $B . 'my $b = !!1; my $c = $b; $c .= ""; my $d = $b; $d++;
   print join(",", b($b + 0), b($b . ""), b($c), b($d), b(sprintf("%s", $b)), b("$b"), b(substr($b, 0))), "\n";');

same('!! re-booleanises; the TEXT of a boolean is unchanged (print, join, sprintf, numeric use)',
  $B . 'print b(!!"x"), b(!!0), "\n"; print "[", !!1, "][", !!0, "] ", join("|", !!1, !!0), " ",
     sprintf("%s/%d/%s/%d", !1, !1, !0, !0), " ", (1 == 1) + 1, " ", (1 == 0) * 5, "\n";
   my $n = 0; $n += ($_ % 3 == 0) for 1 .. 30; my $m = 0; for my $i (1 .. 30) { my $ok = $i % 2 == 0; $m++ if $ok }
   print "n=$n m=$m\n";');

same('JSON::PP (core): booleans encode as true / false, 1 / "" / 0 as themselves',
  'use JSON::PP; print JSON::PP->new->canonical->encode([!!1, !!0, 1, "", 0, (1 == 1), (1 == 0)]), "\n";');

my $jpp = "$ENV{HOME}/.cpan/build/JSON-PP-4.18-0/lib";
SKIP: {
  skip "the board dist JSON-PP-4.18 is not built here", 1 if !-d $jpp;
  same('JSON::PP 4.18 (the board dist): the same',
  'use lib "' . $jpp . '"; use JSON::PP;
   print JSON::PP->VERSION, " ", JSON::PP->new->canonical->encode([!!1, !!0, 1, "", 0]), "\n";');
}

same('JSON::PP decode with core_bools: true / false come back as perl booleans',
  $B . 'use JSON::PP; my $d = JSON::PP->new->core_bools->decode("[true,false]");
   print join(",", b($d->[0]), b($d->[1]), "[$d->[0]]", "[$d->[1]]"), "\n";');

same('a boolean was created as neither a number nor a string (builtin::created_as_*)',
  'use builtin qw(created_as_number created_as_string);
   print join(",", map { $_ ? 1 : 0 } created_as_number(!!1), created_as_string(!!1), created_as_number(1 == 0),
     created_as_string(1 == 0), created_as_string("1"), created_as_number(1)), "\n";');

# The truth twins (p-true-p's compiler macro on a comparison, s517a): a test
# of a comparison in a condition answers the CL truth directly -- an overload
# handler's OWN value still decides (perl returns it verbatim), and a NaN
# compares false.
same('a comparison in a condition: plain, string, NaN, an overload handler returning "0" / "PQR"',
  'package O; use overload "==" => sub { $_[0][0] }, "lt" => sub { $_[0][0] }, q("") => sub { "o" };
   package main; my @r; my $nan = "nan" + 0; my ($a, $b) = (3, 4);
   push @r, ($a < $b ? "y" : "n"), ($a == $b ? "y" : "n"), ("a" lt "b" ? "y" : "n"), ($nan == $nan ? "y" : "n");
   my $z = bless ["0"], "O"; my $p = bless ["PQR"], "O";
   push @r, ($z == 1 ? "y" : "n"), ($p == 1 ? "y" : "n"), ($z lt "x" ? "y" : "n"), ($p lt "x" ? "y" : "n"), scalar($p == 1);
   my $i = 0; $i++ while $i < 5; push @r, $i;
   print "@r\n";');

#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# bigint-01.t -- task #2874: `use bigint` / `use bignum` turn numeric LITERALS
# into Math::BigInt / Math::BigFloat objects in their lexical scope.
#
# perl does it with compile-time constant handlers (overload::constant): the
# tokenizer hands every literal's source text to the handler.  PCL cannot run
# a handler while it parses, so the shims lib/bigint.pm and lib/bignum.pm name
# their handlers in the one shape the transpiler reads statically
# (`overload::constant KIND => \&NAME` in the import sub), and every literal
# in scope is compiled as `NAME('TEXT')` -- the TEXT, so `2**100` never passes
# through a double.  Before this, `use bigint; print 2**100` printed
# 1.26765060022823e+30, silently.  bigrat is ANNOUNCED (Math::BigRat does not
# run under PCL yet).
#
# Each row compares against perl's STDOUT.  No 5.40 syntax: CI's perl is 5.38.

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

plan tests => 16;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "no warnings;\n$code";
    close $fh;
    return $pl_file;
}

# Transpile (PCLCore::transpile FAILS the row on a dropped statement) and run.
sub run_cl {
    my ($code, $stderr_too) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $redir = $stderr_too ? '2>&1' : '2>/dev/null';
    my $output = `sbcl @sbcl_rt --load $cl_file $redir`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub both_agree {
    my ($code, $desc) = @_;
    my $file = write_pl($code);
    my $perl = scalar `perl $file 2>/dev/null`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

both_agree(q{use bigint; print 2**100, "\n";},
           '`2**100` is exact (the task probe)');

both_agree(q{use bigint; print 7 / 2, " ", -7 / 2, " ", 7 % 3, " ", 9 <=> 10, " ", ref(5), "\n";
print 12345678901234567890 * 98765432109876543210, "\n";},
           'integer division, modulus, comparison, a Math::BigInt per literal');

both_agree(q{use bigint; print 1e3, " ", ref(1e3), " ", 2.5 + 1, "\n";},
           'a float literal is truncated to an integer');

both_agree(q{use bigint; print 0x10 + 1, " ", ref(0x10), " ", hex("ff"), " ", ref(hex("ff")), " ", inf, " ", NaN, "\n";},
           'a hex literal, the hex override, inf and NaN');

both_agree(q{use bigint; print ref("7" * "2"), "|", "7" * "2", "|", "7" + 1, " ", ref("7" + 1), "\n";},
           'a STRING is not a literal: "7" * "2" stays a plain number');

both_agree(q{use bigint; sub dbl { return 2 * $_[0] } print dbl(2**70), "\n";
{ no bigint; print 2**70, " ", ref(5), "\n"; }
print 2**70, "\n";},
           'a sub body in scope; `no bigint` in a nested block ends it there only');

both_agree(q{{ use bigint; print 2**70, "\n"; } print 2**70, "\n";},
           '`use bigint` inside a block: plain numbers after it');

both_agree(q{use bigint; my $s = 0; $s += $_ for 1 .. 10; print $s, " ", ref($s), "\n";
my @r = (2 .. 4); print "@r\n";},
           'a range with Math::BigInt endpoints numifies through the overload');

both_agree(q{use bignum; print ref(1), " ", ref(1.5), " ", 1/3, " ", 0.1 + 0.2, " ", 2**100, "\n";},
           '`use bignum`: BigInt literals upgrade to BigFloat');

both_agree(q{use bigint (); print 2**70, "\n"; use bigint; print 2**70, "\n";},
           '`use bigint ()` (an EMPTY import list) converts nothing');

# #5465 (s543t): Math::BigInt's `/' handler (bdiv) answers (quotient,
# remainder) in LIST context; perl calls every overload handler in scalar
# context, so a division in a list slot is ONE value.
both_agree(q{use Math::BigInt; my $y = Math::BigInt->new(7);
my @q = ($y / 2); print scalar(@q), " [@q]\n";
sub half { my $v = shift; $v / 2 } my @t = half($y); print scalar(@t), " [@t]\n";
my @m = map { my $q; $q = $_ / 7; $q } Math::BigInt->new(1000); print "[@m]\n";
my @bd = $y->copy->bdiv(2); print scalar(@bd), " [@bd]\n";},
           '#5465: a Math::BigInt division in list context is one value; the bdiv METHOD keeps (quo, rem)');

both_agree(q{use bignum; print "[$_]\n" for 9, do { my $q = 1/3; $q }; my @l = (1/3, 10/4); print scalar(@l), "\n";},
           '#5465: `use bignum` literal divisions in a for list and a list assignment');

# #5468 (s543t): under `use bigint` every literal is an object, so `@i - 1`
# is an overloaded `-' with an ARRAY operand -- its count, not its first
# element; `@rows == 2' over array references died in Math::BigInt's objectify.
both_agree(q{use bigint; my @i = (3, 5, 4, 2, 1); my $k = @i - 1; print "$k\n";
my @rows = ([4], [5]); print "two\n" if @rows == 2; my @e; print @e + 1, " ", 10 - @i, "\n";},
           '#5468: `use bigint` -- an array operand of an overloaded operator is its count');

# #5467 (s543t): `int' of a Math::BigInt / BigFloat calls the class's `int'
# handler and stays an OBJECT, so the arithmetic after it stays exact.
both_agree(q{use Math::BigInt; use Math::BigFloat; my $b = Math::BigInt->new("12345678901234567890123");
print ref(int($b)), " ", int($b) / 7, " ", int(Math::BigFloat->new("-2.5")), "\n";
my $n = Math::BigInt->new("98765432109876543210"); my @d;
while ($n > 0) { push @d, $n % 7; $n = int($n / 7) } print "@d\n";},
           '#5467: int() of a big-number object is the class\'s int (an object)');

# #2560 / #3000 (s543t): Math::BigRat's "N/D" string constructor -- its
# Math::BigFloat::new($1) call ran matches of its own, whose $2 used to reach
# the caller (#932's sub half): "49/4" was 1.
both_agree(q{use Math::BigRat; print join(" ", map { Math::BigRat->new($_) } "3", "49/4", "1/3", "0.5", "-7/2", "5/-10"), "\n";
print Math::BigRat->new("1/3") + Math::BigRat->new("1/6"), " ", Math::BigRat->new("3/4") * 2, " ", Math::BigRat->new("3/4")->numify, "\n";},
           '#2560: Math::BigRat->new("N/D") and its arithmetic');

my $out = run_cl(q{use bigrat; print 1 + 2, "\n";}, 1);
like($out, qr/\APCL: use bigrat is not supported \(task #2874\)[^\n]*\n3\n\z/,
     '`use bigrat` is ANNOUNCED once on stderr, literals stay plain');

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

plan tests => 10;

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

my $out = run_cl(q{use bigrat; print 1 + 2, "\n";}, 1);
like($out, qr/\APCL: use bigrat is not supported \(task #2874\)[^\n]*\n3\n\z/,
     '`use bigrat` is ANNOUNCED once on stderr, literals stay plain');

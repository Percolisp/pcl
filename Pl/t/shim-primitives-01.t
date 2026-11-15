#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# shim-primitives-01.t -- s540e (#4989 #4988 #4990): the runtime primitives
# three XS stand-ins sit on since s540e, as a CONTRACT of their own.
#
# `lib/Storable.pm` dclone, `lib/Digest/MD5.pm` and `lib/MIME/Base64.pm` call
# PCL-only names in the `builtin::` namespace (the blessed shim-dispatch
# seam, docs/DECIDED.md): `deep_copy` (a value graph copied, the classes it
# blessed, the EMPTY LIST for any kind it does not copy as data), `md5_digest`
# / `md5_update` (sb-md5: a byte string's digest; whole blocks into the four
# chaining words) and `base64_encode` / `base64_decode`.  The shims' own
# guard files hold them to perl's XS modules; this file holds the primitives
# to what docs/ir-spec.md says they answer -- above all the DECLINE answers
# (CODE, a glob, a regexp, a non-reference, a structure past the depth bound,
# a character above 255), which a shim's fallback depends on.  perl has none
# of these names, so the expectations are PCL's documented answers; every
# row fails on the base (`Undefined subroutine &BUILTIN::deep_copy`).

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

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = PCLCore::transpile("$pl2cl $pl_file");
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

my @lines = split /\n/, run_cl(<<'PERL');
my ($c, @cl) = builtin::deep_copy(bless { a => [1, bless([], 'In')] }, 'Out');
print ref($c), ' ', ref($c->{a}[1]), ' ', join(',', sort @cl), "\n";
print scalar(my @n = builtin::deep_copy({ c => sub { 1 } })), ' ', scalar(my @g = builtin::deep_copy([\*STDOUT])),
      ' ', scalar(my @q = builtin::deep_copy([qr/x/])), ' ', scalar(my @p = builtin::deep_copy(5)), "\n";
my $cyc = []; push @$cyc, $cyc; my ($cc) = builtin::deep_copy($cyc);
print(($cc->[0] == $cc && $cc != $cyc) ? "cycle\n" : "broken\n");
my $deep = []; my $cur = $deep; for (1 .. 1001) { my $nx = []; push @$cur, $nx; $cur = $nx }
print scalar(my @d = builtin::deep_copy($deep)), "\n";
print unpack('H*', builtin::md5_digest('abc')), ' ', (defined builtin::md5_digest("\x{100}") ? 'def' : 'undef'), "\n";
my @r = builtin::md5_update(0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, '', 'x' x 70);
my @w = builtin::md5_update(1, 2, 3, 4, '', "\x{263a}");
print scalar(@r), ' ', length($r[4]), ' ', scalar(@w), "\n";
print builtin::base64_encode('abc' x 30, '|'), ' ', builtin::base64_decode('YW J=jZA'), ' ',
      (defined builtin::base64_encode("\x{263a}", '') ? 'def' : 'undef'), "\n";
PERL

plan tests => 7;
is($lines[0], 'Out In In,Out', 'deep_copy: (COPY, CLASS...) -- blessings kept, each class named once');
is($lines[1], '0 0 0 0', 'deep_copy declines (empty list) on CODE, a glob, a regexp, a non-reference');
is($lines[2], 'cycle', 'deep_copy: a cycle is copied once and stays a cycle, sharing nothing');
is($lines[3], '0', 'deep_copy declines a structure nested past its depth bound (1,000)');
is($lines[4], '900150983cd24fb0d6963f7d28e17f72 undef',
   'md5_digest: the RFC 1321 "abc" digest; undef for a character above 255');
is($lines[5], '5 6 0', 'md5_update: four words + the 6 bytes left after one block of 70; () for a wide character');
is($lines[6], ('YWJj' x 19) . '|' . ('YWJj' x 11) . '| ab undef',
   'base64_encode: EOL after 76 characters and after the last; base64_decode skips and stops at =; undef when wide');

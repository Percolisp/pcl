#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pack-u0-01.t -- a template that BEGINS with U is U0 character mode (#3650).
#
# perlfunc pack: a template whose first item is U packs into a CHARACTER
# string and unpacks the string's characters; a U later in the template (or
# after C0) works in UTF-8 bytes.  The rule is in cl/pack-impl.pl (p_pack /
# p_unpack, the two copies the USER lifted out of the pack park, s523).  Perl
# 5.40.3 is the oracle; every row FAILS on 0f7fc8bd (the bytes rule always).
# Not covered here (outside the rule, still parked): the UTF-8 FLAG of a
# C-led result, malformed-UTF-8 answers of a white-space-led unpack.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt      = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

my $workdir = tempdir(CLEANUP => 1);

sub both {
    my ($tag, $body) = @_;
    my $src = "$workdir/$tag.pl";
    open(my $s, '>', $src) or die; print {$s} $body; close $s;
    my $perl = `$^X $src 2>&1`;
    my $cl_code = `$pl2cl $src 2>/dev/null`;
    my $cl_file = "$workdir/$tag.lisp";
    open(my $c, '>', $cl_file) or die; print {$c} $cl_code; close $c;
    my $pcl = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $pcl =~ s/^;.*\n//gm;
    $pcl =~ s/^PCL Runtime loaded\n//gm;
    $pcl =~ s/^\s*\n//gm;
    return ($perl, $pcl);
}

my ($perl, $pcl) = both('packu', <<'PL');
sub show { my ($s) = @_; print length($s), ":", join(",", map { ord } split //, $s), "\n" }
show(pack('U', 0x83));
show(pack('U*', 0x1F600));
show(pack('U*', 65, 0xe9, 0x100));
show(pack('U0U', 0xe9));
show(pack(' U', 0xe9));
show(pack('U a*', 0xe9, "z"));
show(pack('U N', 0xe9, 1));
show(pack('C U', 65, 0xe9));
show(pack('U'));
PL
is($pcl, $perl, 'pack: a leading U packs characters, a later U packs UTF-8 bytes');

($perl, $pcl) = both('unpacku', <<'PL');
print join(",", unpack('U*', "\x01\x41\x83\xe9\xff")), "\n";
print join(",", unpack('U*', "\xc3\xa9")), "\n";
print join(",", unpack('U*', chr(300))), "\n";
print join(",", unpack('U0U*', "\xc3\xa9")), "\n";
print join(",", unpack('a U*', "x\xc3\xa9")), "\n";
print join(",", unpack('U C*', "\xe9\xe9")), "\n";
print join(",", unpack('U2 W', chr(300) . "\xe9z")), "\n";
print join(",", unpack('U*', pack('U*', 0x41, 0xe9, 0x263a))), "\n";
PL
is($pcl, $perl, 'unpack: a leading U reads characters, a later U reads UTF-8 bytes');

# s538a: pack 'w' of a DIGIT STRING above UV_MAX is perl's w_string (div128 on
# the decimal digits).  cl/pack-impl.pl's `$v >>= 7` loop was exact on a
# bignum only while the compound shift was signed; #3781 made `>>=` read a UV
# as perl's does and pack.t row 10 failed.  Every 'w' site (a count, a '/'
# length, the elements) is the one encoder _pack_ber now.
($perl, $pcl) = both('packw', <<'PL');
for my $x (0, 130, 268435455, 2**33, '4503599627365785', 18446744073709551615,
           '18446744073709551616', '23728385234614992549757750638446',
           ' +340282366920938463463374607431768211456 ', '00012') {
  print unpack("H*", pack("w", $x)), "\n" }
print unpack("H*", pack("w*", 1, '99999999999999999999999', 2)), "\n";
print unpack("H*", pack("w/a*", "abc")), "\n";
PL
is($pcl, $perl, "pack 'w': a digit string above UV_MAX is encoded exactly (s538a)");

done_testing;

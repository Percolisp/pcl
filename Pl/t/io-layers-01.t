#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Task #1115 — a filehandle carries OCTETS unless a layer says otherwise.
#
# perl's default I/O discipline is BYTES: `open $fh,'<',$f` reads one CHARACTER
# PER OCTET, so `length` on the slurp is the file's size.  Before #1115 PCL
# opened every handle with SBCL's default (UTF-8), so every read of a non-ASCII
# file measured characters — the widest silent divergence PCL had left, since
# any byte-oriented use of the result (a size, a seek offset, a checksum, a
# re-write) was then wrong.
#
# Every expectation here is perl 5.40.3's own answer, measured on the fixture
# this file writes (15 octets: "abc" U+00E9 "def" U+2019 "ghi\n").  The INVERSE
# guard: on an `f728637` worktree rows 1/2/6/7/8 fail (len 12 where perl says
# 15, and the round-trip writes double-encoded octets).

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

# The fixture: 15 OCTETS, two of the characters multi-byte in UTF-8.
my $dir  = tempdir(CLEANUP => 1);
my $data = "$dir/data.txt";
{
    open my $fh, '>:raw', $data or die "open $data: $!";
    print $fh "abc\xc3\xa9def\xe2\x80\x99ghi\n";
    close $fh;
}
is(-s $data, 15, 'fixture is 15 octets on disk');

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = PCLCore::transpile(qq{$pl2cl $pl_file});
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^WARNING:.*\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# Slurp $data through MODE (plus an optional binmode call) and report the
# length and the 4th character's ordinal — the two numbers that separate
# octets from characters.
sub slurp_report {
    my ($mode, $binmode) = @_;
    my $bm = defined $binmode ? "binmode(\$fh, $binmode);" : '';
    return run_cl(<<"PL");
open my \$fh, '$mode', '$data' or die "open: \$!";
$bm
my \$s = do { local \$/; <\$fh> };
close \$fh;
printf "len=%d ord3=%d\\n", length(\$s), ord(substr(\$s,3,1));
PL
}

is(slurp_report('<'),      "len=15 ord3=195\n", 'default open reads OCTETS');
is(slurp_report('<:raw'),  "len=15 ord3=195\n", ':raw reads octets');
is(slurp_report('<:utf8'), "len=12 ord3=233\n", ':utf8 in the mode DECODES');
is(slurp_report('<:encoding(UTF-8)'), "len=12 ord3=233\n",
   ':encoding(UTF-8) in the mode decodes');
is(slurp_report('<', "':utf8'"), "len=12 ord3=233\n",
   "binmode(\$fh,':utf8') decodes a handle opened as bytes");
is(slurp_report('<', "':encoding(UTF-8)'"), "len=12 ord3=233\n",
   "binmode(\$fh,':encoding(UTF-8)') decodes");
is(slurp_report('<:encoding(UTF-8)', undef), "len=12 ord3=233\n",
   'the decoding open on its own still decodes');
is(slurp_report('<:encoding(UTF-8)', "':raw'"), "len=15 ord3=195\n",
   "binmode(\$fh,':raw') pops a decoding handle back to octets");

# `binmode($fh)` with no layer POPS back to bytes (probed 5.40.3).
is(run_cl(<<"PL"), "len=15 ord3=195\n", 'bare binmode() pops back to OCTETS');
open my \$fh, '<:encoding(UTF-8)', '$data' or die;
binmode(\$fh);
my \$s = do { local \$/; <\$fh> };
close \$fh;
printf "len=%d ord3=%d\\n", length(\$s), ord(substr(\$s,3,1));
PL

# A byte-for-byte round trip: read default, write default.  This is the shape
# every CPAN module that rewrites a file has, and it was silently corrupting
# the two multi-byte characters before #1115.
is(run_cl(<<"PL"), "out=15 same=1\n", 'read-then-write round-trips byte for byte');
open my \$r, '<', '$data' or die; my \$s = do { local \$/; <\$r> }; close \$r;
open my \$w, '>', '$dir/rt.bin' or die; print \$w \$s; close \$w;
open my \$c, '<:raw', '$dir/rt.bin' or die; my \$d = do { local \$/; <\$c> }; close \$c;
open my \$o, '<:raw', '$data' or die; my \$e = do { local \$/; <\$o> }; close \$o;
printf "out=%d same=%d\\n", length(\$d), (\$d eq \$e ? 1 : 0);
PL

# tell/-s/read/getc all count the same units as length now.
is(run_cl(<<"PL"), "tell=15 size=15 read=5 ord4=195\n", q{tell, -s, read and getc all count OCTETS on a byte handle});
open my \$fh, '<', '$data' or die;
my \$line = <\$fh>;
my \$t = tell(\$fh);
close \$fh;
open my \$g, '<', '$data' or die; my \$buf = ''; my \$n = read(\$g, \$buf, 5); close \$g;
printf "tell=%d size=%d read=%d ord4=%d\\n", \$t, -s '$data', \$n, ord(substr(\$buf,3,1));
PL

# `use open qw(:std :utf8)` moves the default for BOTH directions and the
# standard handles.  A wide character then reaches STDOUT without a warning.
is(run_cl(<<"PL"), "len=12 ord3=233\n", 'use open qw(:std :utf8) makes the default DECODE');
use open qw( :std :utf8 );
open my \$fh, '<', '$data' or die;
my \$s = do { local \$/; <\$fh> };
close \$fh;
printf "len=%d ord3=%d\\n", length(\$s), ord(substr(\$s,3,1));
PL

is(run_cl(<<"PL"), "len=15 ord3=195\n", 'an explicit :raw still overrides use open');
use open qw( :std :utf8 );
open my \$fh, '<:raw', '$data' or die;
my \$s = do { local \$/; <\$fh> };
close \$fh;
printf "len=%d ord3=%d\\n", length(\$s), ord(substr(\$s,3,1));
PL

# A WIDE character on a byte handle: perl warns once per ARGUMENT and writes
# the string's whole UTF-8 encoding — the \x{e9} becomes TWO octets although
# one would hold it, because perl decides per SV, not per character.
is(run_cl(<<"PL"), "bytes=c3a9e28099 warns=2\n", q{a wide character upgrades the WHOLE string and warns once per argument});
my \@w;
local \$SIG{__WARN__} = sub { push \@w, \$_[0] };
my \$s = "\\x{e9}\\x{2019}";
open my \$fh, '>', '$dir/wide.bin' or die;
print \$fh \$s, \$s;
close \$fh;
open my \$r, '<:raw', '$dir/wide.bin' or die; my \$d = do { local \$/; <\$r> }; close \$r;
printf "bytes=%s warns=%d\\n", join('', map { sprintf '%02x', ord } split //, substr(\$d,0,5)),
       scalar(grep { /Wide character/ } \@w);
PL

# ...and a string that FITS in octets is written as octets, with no warning.
is(run_cl(<<"PL"), "bytes=e9 warns=0\n", q{a string that fits in octets is written as octets and does not warn});
my \@w;
local \$SIG{__WARN__} = sub { push \@w, \$_[0] };
open my \$fh, '>', '$dir/nar.bin' or die;
print \$fh "\\x{e9}";
close \$fh;
open my \$r, '<:raw', '$dir/nar.bin' or die; my \$d = do { local \$/; <\$r> }; close \$r;
printf "bytes=%s warns=%d\\n", join('', map { sprintf '%02x', ord } split //, \$d),
       scalar(grep { /Wide character/ } \@w);
PL

# perl makes syswrite FATAL where print warns (probed 5.40.3).
like(run_cl(<<"PL"), qr/died=Wide character in syswrite/, q{syswrite of a wide character DIES, as perl does});
open my \$fh, '>', '$dir/sw.bin' or die;
my \$n = eval { syswrite(\$fh, "a\\x{2019}b") };
print "died=\$@" if \$@;
close \$fh;
PL

# The emission: `use open` is the one never-loaded pragma with a runtime effect.
{
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "use open qw(:std :utf8);\nprint \"x\\n\";\n";
    close $fh;
    my $cl = PCLCore::transpile(qq{$pl2cl $pl_file});
    like($cl, qr/\(p-use-open \(vector ":std" ":utf8"\)\)/,
         'use open lowers to a p-use-open call with its list');
}

done_testing();

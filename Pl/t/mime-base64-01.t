#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# mime-base64-01.t — s492c: `lib/MIME/Base64.pm`, plain Perl.
#
# The real module is XS, so `use MIME::Base64;` died "Can't locate
# MIME/Base64.pm in @INC (the module is XS and has no PCL build)" — and base64
# is in every script that talks to an HTTP API, stores a binary blob in a text
# field or reads a Basic-Auth header.
#
# perl is the ORACLE for every row.  The one rule a from-scratch encoder gets
# wrong is the DECODER's: perl stops at the first `=` wherever it sits, so
# `decode_base64("aa=aa")` is ONE byte, not three — MIME-Base64's own
# t/length.t rows 22-25 are exactly that, and they are row 5 here.
#
# The dist's own tests go 0 -> 437 of 495 (t/base64.t, t/base64url.t,
# t/unicode.t and t/length.t all fully match); the residue is
# MIME::QuotedPrint, which is a separate XS module (filed #2059).

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

plan tests => 8;

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
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

my $HDR = 'use MIME::Base64 qw(encode_base64 decode_base64 encode_base64url'
        . ' decode_base64url encoded_base64_length decoded_base64_length);' . "\n";

both_agree($HDR . 'print encode_base64("Hello, World!");',
           'encode_base64 of a short string, with its default newline');

both_agree($HDR . 'print "[", encode_base64("a"), "][", encode_base64("ab"),'
                . ' "][", encode_base64("abc"), "][", encode_base64("", "\n"),'
                . ' "][", encode_base64("x", ""), "]\n";',
           '... and every padding length, the empty string and an empty $eol');

both_agree($HDR . 'my $bin = join("", map { chr($_) } 0 .. 255);'
                . ' my $e = encode_base64($bin);'
                . ' print length($e), " ", (decode_base64($e) eq $bin ? "ok" : "BAD"), "\n";',
           'all 256 byte values round-trip, wrapped at 76 columns');

both_agree($HDR . 'print "[", decode_base64("SGVsbG8sIFdvcmxkIQ=="), "][",'
                . ' decode_base64("SGVs bG8s\nIFdvcmxkIQ=="), "][", decode_base64("aGk"),'
                . ' "][", decode_base64(""), "]\n";',
           'decode ignores whitespace and tolerates missing padding');

both_agree($HDR . 'print join(" ", map { decoded_base64_length($_) }'
                . ' "=aaaa", "a=aaa", "aa=aa", "aaa=a", "aaaa=", "a\na\na a"), "\n";'
                . ' print "[", decode_base64("aa=aa"), "]\n";',
           'the decoder STOPS at the first "=" (t/length.t rows 22-25)');

both_agree($HDR . 'print encoded_base64_length("x" x 100), " ", length(encode_base64("x" x 100)),'
                . ' " ", encoded_base64_length("aaa", ""), " ", encoded_base64_length(""), "\n";',
           'encoded_base64_length agrees with the encoder it predicts');

both_agree($HDR . 'my $bin = join("", map { chr($_) } 0 .. 255);'
                . ' print "[", encode_base64url("\xfb\xff\xfe"), "][",'
                . ' (decode_base64url(encode_base64url($bin)) eq $bin ? "ok" : "BAD"), "]\n";',
           'the url-safe alphabet, unpadded, round-trips');

both_agree($HDR . 'my $r = eval { encode_base64("\x{263A}"); 1 };'
                . ' print +($r ? "silent" : "loud"), "\n";',
           'a character above 255 is LOUD, as perl is');

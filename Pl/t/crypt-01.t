#!/usr/bin/env perl
# crypt-01.t: crypt(PLAINTEXT, SALT) via FFI to the system crypt(3).
# PCL calls the same crypt(3) Perl does, so output is byte-identical on the
# same platform.  We compare PCL's output to this perl's crypt() directly.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;
plan skip_all => "no crypt() in this perl" unless eval { my $c = crypt("ab","cd"); 1 };

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>&1`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

plan tests => 4;

# 1. DES hash matches Perl's own crypt()
is run_cl('print crypt("ab","cd"), "\n";'),
   crypt("ab","cd") . "\n",
   'crypt DES output matches Perl';

# 2. Salt makes a difference
is run_cl('print((crypt("ab","cd") ne crypt("ab","ce")) ? "diff\n" : "same\n");'),
   "diff\n",
   'salt changes the hash';

# 3. Wide character dies
like run_cl('eval { crypt("a\x{100}", "cd") }; print $@;'),
     qr/Wide character in crypt/,
     'wide character in crypt dies';

# 4. Eight-bit (latin-1) plaintext round-trips identically to Perl
is run_cl('print crypt("a\xFF", "cd"), "\n";'),
   crypt("a\xFF","cd") . "\n",
   'eight-bit plaintext matches Perl';

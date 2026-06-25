#!/usr/bin/env perl
# binmode-01.t: binmode() return value + EBADF on an unopened handle, and the
# PerlIO::Layer->find introspection shim.
#
# Regression for the t/io/binmode.t crash (2026-06-25): `find PerlIO::Layer
# 'perlio'` (an indirect method call on a core package PCL did not ship) died
# with an uncaught "Can't locate object method", aborting the whole file.  PCL
# now auto-requires a minimal lib/PerlIO/Layer.pm shim on first dispatch.  Also:
# binmode on a filehandle that is not open must fail with errno EBADF, not
# silently succeed.

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

plan tests => 6;

# binmode on an open handle returns true.
is run_cl('open(my $fh, ">", "/tmp/pcl_binmode_t.txt") or die;
           print binmode($fh) ? "ok\n" : "no\n"; close $fh;'),
   "ok\n", 'binmode(open handle) is true';

# binmode with a layer string on a standard handle returns true.
is run_cl('print binmode(STDOUT, ":raw") ? "ok\n" : "no\n";'),
   "ok\n", 'binmode(STDOUT, ":raw") is true';

# binmode on an UNOPENED bareword handle fails and sets $! to EBADF (9).
is run_cl('$! = 0; my $r = binmode(NOPE);
           printf "r=%s errno=%d\n", ($r ? "T" : "F"), ($!+0);'),
   "r=F errno=9\n", 'binmode(unopened) returns false and sets $! = EBADF';

# PerlIO::Layer->find must not crash, and reports known core layers.
is run_cl('print( (find PerlIO::Layer "perlio") ? "yes\n" : "no\n");'),
   "yes\n", 'find PerlIO::Layer "perlio" is true (no crash)';

is run_cl('print( (find PerlIO::Layer "nosuchlayer") ? "yes\n" : "no\n");'),
   "no\n", 'find PerlIO::Layer on an unknown layer is false';

# The whole program continues after the introspection call (no abort).
is run_cl('my $ok = find PerlIO::Layer "perlio";
           print "after=", ($ok ? 1 : 0), "\n";
           print "still running\n";'),
   "after=1\nstill running\n", 'program continues past PerlIO::Layer->find';

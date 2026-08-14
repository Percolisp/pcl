#!/usr/bin/env perl
# version-shim-01.t — lib/version.pm, the shim for perl's `version` module.
#
# Task #319: every t/op/packagev.t row that validates a version string calls
# version::is_strict / version::is_lax, and the shim had neither, so the file
# died with `undef-fn:version::pl-is_strict` after 5 of its 307 rows.
#
# Perl ships these two in version/regex.pm, where each pattern is composed by
# interpolating qr// objects into other qr// objects.  The shim spells each as
# ONE literal pattern instead (no qr-in-qr), so the grammars have to be
# expanded by hand — which is exactly the kind of transcription that needs a
# guard.  Every expectation below is the answer the REAL `version::` gives
# (perl 5.40.3); the strings are packagev.t's own table.

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

plan tests => 2;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# The acceptance table, verbatim from t/op/packagev.t's __DATA__ (the STRICT
# column is the `package NAME VERSION` grammar; the LAX column is what
# version->new accepts), plus the two `undef`/`v` oddities.
my $STRICT_CASES = <<'PL';
require version;
for my $v (qw(1.00 1.00001 0.123 12.345 42 0 0.0 v1.2.3 v1.2.3.4 v0.1.2 v0.0.0
              01 01.0203 v01 v01.02.03 1.2.3 v1.2 v0 v1 v1.2345.6 undef 1a bar _)) {
  print version::is_strict($v) ? 1 : 0;
}
print "\n";
PL
is(run_cl($STRICT_CASES), "111111111110000000000000\n",
   "is_strict: the eleven strict spellings pass, the thirteen packagev.t rejects fail");

my $LAX_CASES = <<'PL';
require version;
for my $v (qw(1.00 42 v1.2.3 01 01.0203 v01 v01.02.03 1.2.3 v1.2 v0 v1
              1.02_03 v1.2_3 v1.02_03 v1.2345.6 undef
              1.a 1._ 0_ 1_ 1_. 1.1_ 1.02_03_04 v.1.2.3 v 1a 1.2a3 bar _)) {
  print version::is_lax($v) ? 1 : 0;
}
print "\n";
PL
is(run_cl($LAX_CASES), "11111111111111110000000000000\n",
   "is_lax: the sixteen lax spellings pass, the thirteen non-versions fail");

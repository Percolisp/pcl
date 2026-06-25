#!/usr/bin/env perl
# system-block-01.t: indirect-object block form  system { PROG } LIST  /
# exec { PROG } LIST, in both the bare and paren-wrapped shapes.
#
# Perl's `system { PROG } argv0, args...` runs PROG with the LIST as argv (so
# argv[0] can differ from PROG).  PCL lowers this to the ordinary list form
# system(PROG, LIST) — the argv[0]-override nuance is dropped, but the program
# and its arguments are correct.  Before this fix the leading brace block fell
# through the parser with "Missing case: [".

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
plan skip_all => "no /bin/echo"    unless -x "/bin/echo";

sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return scalar `$pl2cl $pl_file 2>&1`;
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = transpile($code);
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

plan tests => 5;

# --- transpile (codegen) checks: the block lowers to a plain program arg ---
like transpile('system { "/bin/echo" } "argv0", "x";'),
     qr/\(p-system "\/bin\/echo" "argv0" "x"\)/,
     'bare block form lowers to (p-system PROG LIST)';

like transpile('my $rc = system({ "/bin/echo" } "argv0", "x");'),
     qr/\(p-system "\/bin\/echo" "argv0" "x"\)/,
     'paren block form lowers to (p-system PROG LIST)';

unlike transpile('system { "/bin/echo" } "a";'),
       qr/PARSE ERROR/,
       'bare block form does not parse-error';

# --- runtime: the program actually runs and prints its arguments ---
is run_cl('system { "/bin/echo" } "echo", "hello", "world"; print "done\n";'),
   "echo hello world\ndone\n",
   'bare block form runs /bin/echo with the LIST as argv';

is run_cl('my $rc = system({ "/bin/echo" } "p", "a", "b"); print "rc=$rc\n";'),
   "p a b\nrc=0\n",
   'paren block form runs and returns 0';

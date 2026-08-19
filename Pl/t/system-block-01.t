#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

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
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;
plan skip_all => "no /bin/echo"    unless -x "/bin/echo";

sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return scalar PCLCore::transpile(qq{$pl2cl $pl_file});
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = transpile($code);
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

plan tests => 11;

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

# ── #369: every qx spelling is the same term as the backtick form ────────────
# PPI gives `…` its own token class and hands EVERY qx spelling a
# PPI::Token::QuoteLike::Command, and the term walker's primary set accepted
# only the first — so `my $c = qx{echo hi}` had no primary at all and the
# statement was DROPPED (8 drops over perl's own t/, and $c silently undef in
# any program using the shape).  The delimiter decides interpolation exactly as
# it does for q// vs qq//: qx'…' is literal, everything else interpolates.
is run_cl('my $c = qx{/bin/echo hi}; print "c=$c";'), "c=hi\n",
   'qx{} runs the command';
is run_cl('my $c = qx(/bin/echo ho); print "c=$c";'), "c=ho\n",
   'qx() runs the command';
is run_cl('my $c = qx[/bin/echo he]; print "c=$c";'), "c=he\n",
   'qx[] runs the command';
is run_cl('my $c = qx!/bin/echo ha!; print "c=$c";'), "c=ha\n",
   'qx!! runs the command';
is run_cl('my $w = "yes"; my $c = qx{/bin/echo $w}; print "c=$c";'), "c=yes\n",
   'qx{} interpolates like the backtick form';
is run_cl(q{my $w = "yes"; my $c = qx'/bin/echo $w'; print "c=$c";}), "c=\n",
   q{qx'' does NOT interpolate (perl-probed: the shell sees a literal $w)};

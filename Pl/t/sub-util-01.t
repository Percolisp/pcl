#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# sub-util-01.t -- Sub::Util::subname / set_subname (#3529, s523b).
#
# A sub's NAME is ONE runtime record: the one `sub NAME` writes, B's GV reads
# and subname reads; set_subname rewrites it (perl gives the CV a new GV and
# clears CVf_ANON) without installing the sub anywhere.  caller(0)[3] INSIDE an
# anonymous sub is the call frame, #233's deferral -- not asserted here.
# Every expected string was probed against perl 5.40.3 first.

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

plan tests => 12;

my $prog = <<'PL';
use B;
use Scalar::Util qw(refaddr);
use Sub::Util qw(set_subname subname);
my $c = sub { 1 };
my $t = set_subname("Foo::plain", $c);
print "U1 ", subname($t), "\n";
print "U2 ", (refaddr($t) == refaddr($c) ? "same" : "new"), "\n";
print "U3 ", (defined &Foo::plain ? "installed" : "not-installed"), "\n";
print "U4 ", subname(sub { 2 }), "\n";
sub f { 3 }
print "U5 ", subname(\&f), "\n";
my $n = set_subname("main::x\x00y", sub { 4 });
print "U6 ", subname($n), "\n";
(my $gn = B::svref_2object($n)->GV->NAME) =~ s/\x00/<NUL>/g;
print "U7 $gn\n";
print "U8 ", subname(set_subname("bare", sub { 5 })), "\n";
print "U9 ", subname(set_subname("q'r", sub { 6 })), "\n";
print "U10 ", (B::svref_2object($t)->CvFLAGS & B::CVf_ANON()) ? "anon" : "named", "\n";
my $g = B::svref_2object(set_subname("P::Q::SOME_:_NAME", sub { 7 }))->GV;
print "U11 ", $g->STASH->NAME, " | ", $g->NAME, "\n";
package Bar; print "U12 ", Sub::Util::subname(sub { 8 }), "\n";
PL

my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
print $fh $prog;
close $fh;
my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $cl_fh $cl_code;
close $cl_fh;
my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
my %got;
$got{$1} = $2 while $out =~ /^(U\d+) (.*)$/mg;

sub line_is {
    my ($key, $want, $name) = @_;
    is($got{$key} // "(missing: $out)", $want, $name);
}

line_is('U1',  'Foo::plain',     'set_subname renames: subname reads the new name');
line_is('U2',  'same',           'set_subname returns the SAME code reference');
line_is('U3',  'not-installed',  'set_subname does not install the sub');
line_is('U4',  'main::__ANON__', 'an anonymous sub is PKG::__ANON__');
line_is('U5',  'main::f',        'a named sub answers its defining name');
line_is('U6',  'main::x',        "subname stops at a NUL (perl formats a C string)");
line_is('U7',  'x<NUL>y',        "B's GV keeps the whole name, NUL included");
line_is('U8',  'main::bare',     'a name with no package is in main');
line_is('U9',  'q::r',           "the old ' separator is ::");
line_is('U10', 'named',          'a renamed sub is no longer CVf_ANON');
line_is('U11', 'P::Q | SOME_:_NAME', "a lone ':' is a name byte: the GV splits at the LAST ::");
line_is('U12', 'Bar::__ANON__',  'an anonymous sub carries its compiling package');

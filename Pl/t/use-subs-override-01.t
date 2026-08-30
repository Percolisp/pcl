#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Task #732: a user sub displaces a core builtin ONLY after a compile-time
# predeclaration (`use subs`/an import), only for a WEAK keyword
# (Perl_keyword()'s negative half — Environment::builtin_is_overridable), and
# only at use sites AFTER the predeclaration.  `CORE::NAME` names the builtin
# unconditionally (the _core_qualified marker survives PExpr's prefix strip).
# The one emission seam is gen_funcall_form's override lookup before cl_name.
# Sibling guards: Pl/t/system-block-01.t (#703/#734, the backtick spellings).
#
# The oracle is REAL PERL run on the same program at test time.

use strict;
use warnings;
use Test::More tests => 13;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $pl2cl = './pl2cl';
my $runtime = 'cl/pcl-runtime.lisp';
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

my $prog = <<'PROG';
package p1;
sub length { return "PL:" . pop }
print "plain=[", length("abcde"), "]\n";
package p2;
my $before = length("abcde");
use subs qw(length uc chdir readpipe);
sub length { return "L:" . pop }
sub uc { return "U:" . pop }
sub chdir { return "C:" . pop }
sub readpipe { return "R:" . pop }
print "before=[$before]\n";
print "after=[", length("abcde"), "]\n";
my $np = length "xyz";
print "noparen=[$np]\n";
print "core=[", CORE::length("abcde"), "]\n";
print "uc=[", uc("lower"), "]\n";
print "chdir=[", chdir("/tmp"), "]\n";
print "rp=[", readpipe("echo user"), "]\n";
print "corerp=[", CORE::readpipe("echo shell"), "]\n";
my $amp = &length("hello");
print "amp=[$amp]\n";
package p3;
print "other=[", length("abcde"), "]\n";
package p4;
use subs qw(time);
sub time { return "T:fixed" }
print "weaktime=[", time(), "]\n";
print "done\n";
PROG

my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
print $fh $prog;
close $fh;

my $perl_out = `perl $pl_file 2>&1`;

my $cl_code = PCLCore::transpile(qq{$pl2cl --no-cache $pl_file});
my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp');
print $cl_fh $cl_code;
close $cl_fh;
my $pcl_out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
$pcl_out =~ s/^;.*\n//gm;
$pcl_out =~ s/PCL Runtime loaded\n?//g;
$pcl_out =~ s/STYLE-WARNING.*\n//g;
unlink $pl_file, $cl_file;

like($perl_out, qr/^done$/m, 'perl oracle ran the program to completion');

my %perl = map { /^(\w+)=\[(.*?)\]$/s ? ($1 => $2) : () } split /\n(?=\w+=\[|done)/s, $perl_out;
my %pcl  = map { /^(\w+)=\[(.*?)\]$/s ? ($1 => $2) : () } split /\n(?=\w+=\[|done)/s, $pcl_out;

is($pcl{plain},  $perl{plain},  'plain sub length (no use subs) does NOT displace the builtin');
is($pcl{before}, $perl{before}, 'a use site BEFORE the use subs still gets the builtin');
is($pcl{after},  $perl{after},  'use subs "length" + sub length displaces the builtin');
is($pcl{noparen}, $perl{noparen}, 'the paren-less single-operand spelling is the user sub too');
is($pcl{core},   $perl{core},   'CORE::length names the builtin inside the overriding package');
is($pcl{uc},     $perl{uc},     'use subs "uc" displaces uc');
is($pcl{chdir},  $perl{chdir},  'use subs "chdir" displaces chdir');
is($pcl{rp},     $perl{rp},     'readpipe NAME spelling reaches the user sub (#734 interplay)');
is($pcl{corerp}, $perl{corerp}, 'CORE::readpipe runs the shell inside the overriding package');
is($pcl{other},  $perl{other},  'the next package sees the builtin again (package-scoped)');
is($pcl{weaktime}, $perl{weaktime}, 'zero-arg weak keyword (time) displaced by use subs');

is($pcl_out, $perl_out, 'full override battery matches perl byte for byte');

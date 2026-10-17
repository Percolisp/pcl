#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# #2636's copy-back wraps a raw-slot scalar argument of an UNSEEN callee;
# a callee in one of perl's CORE namespaces (builtin::, utf8::, Internals::,
# CORE::) is the runtime's and is exempt unless it is a builtin that writes
# its argument (task #3348).  Rows 1-3 read the emission, row 4 runs vs perl.

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

sub run_perl {
    my ($code) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return `perl $file 2>&1`;
}

# Transpile and run CL, return output
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
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^WARNING:.*\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub cl_of {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return PCLCore::transpile(qq{$pl2cl $pl_file});
}

# 1-3. #3348: a callee in a CORE namespace is not an unseen user sub -- no
#      copy-back box around its raw-slot argument; a module-qualified callee
#      still gets one (#2636).
my $cl = cl_of(<<'PERL');
my $x = 1; my $y = 2; my $z = 3;
my $t = builtin::reftype($x);
my $u = utf8::native_to_unicode($y);
my $v = Foo::bar($z);
print defined $t ? "d" : "u", " $u\n";
PERL
unlike($cl, qr/--pcl-cb--\d+ \$x\b/, 'builtin:: callee: no copy-back of a raw-slot argument');
unlike($cl, qr/--pcl-cb--\d+ \$y\b/, 'utf8:: callee: no copy-back of a raw-slot argument');
like($cl, qr/--pcl-cb--\d+ \$z\b/, 'a module-qualified callee is still an unseen callee (#2636)');

# 4. the core builtins that DO write their argument still do.
{
    my $code = <<'PERL';
my $s = "\x{e9}"; utf8::encode($s); print length($s), "\n";
my $b = "\xc3\xa9"; utf8::decode($b); print length($b), "\n";
my $n = 65; print utf8::native_to_unicode($n), " $n\n";
PERL
    is(run_cl($code), run_perl($code), 'utf8:: writers still write, readers still read');
}

done_testing();

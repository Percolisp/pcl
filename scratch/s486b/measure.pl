#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# s486b scratch instrument: run ONE perl-tests file the way the sweep does
# (p-load-with-recovery, the registry loaded) and print its
# (pass, fail, skip, registry, stale) tuple plus every REGISTRY-STALE line.
# Usage: perl scratch/s486b/measure.pl NAME.t [...]
use strict;
use warnings;
use FindBin;
use lib "$FindBin::RealBin/../../tools/lib";
use PCLSbcl ();
use PCLPaths ();
use File::Basename qw(basename);
use Cwd qw(getcwd);

my $root = PCLPaths::root("$FindBin::RealBin/../../tools");
my $runtime = "$root/cl/pcl-runtime.lisp";
my $testlib = "$root/cl/pcl-test.lisp";
my $registry = "$root/cl/skip-registry.lisp";
my $pt = "$root/perl-tests";
my $out_dir = "$FindBin::RealBin/out";
mkdir $out_dir;

for my $name (map { basename($_) } @ARGV) {
    my $cl = "$out_dir/$name.lisp";
    my $orig = getcwd();
    chdir $pt or die "chdir: $!";
    unless ($ENV{PCL_REUSE_CL} && -s $cl) {
        system("perl -I\Q$root\E \Q$root/pl2cl\E --no-cache --lenient-ppi \Q$name\E > \Q$cl\E 2>$out_dir/$name.transpile.err") == 0
            or die "transpile $name failed\n";
    }
    my $cmd = PCLSbcl::sbcl_prefix_str(runtime => $runtime, quote => 0)
        . " --eval \"(setf pcl::*pcl-skip-cache* t)\" --load $testlib" . ($ENV{PCL_NO_REGISTRY} ? "" : " --load $registry")
        . " --eval \"(setf pcl::*current-test-file* \\\"$name\\\")\""
        . " --eval \"(pcl::p-load-with-recovery \\\"$cl\\\")\"";
    my $tap = "$out_dir/$name" . ($ENV{PCL_NO_REGISTRY} ? ".noreg" : "") . ".tap";
    system("timeout 300 $cmd > \Q$tap\E 2>&1");
    chdir $orig;

    open my $fh, '<', $tap or die "read $tap: $!";
    my ($pass, $fail, $skip, $reg, $stale, @stale_lines) = (0, 0, 0, 0, 0);
    while (my $l = <$fh>) {
        if ($l =~ /^#\s*REGISTRY-STALE:(.*)$/) { $stale++; push @stale_lines, $1; next }
        next unless $l =~ /^(not ok|ok) \d+([^\n]*)$/;
        my ($verb, $rest) = ($1, $2);
        if    ($rest =~ /#\s*skip/i) { $skip++; $reg++ if $rest =~ /#\s*skip\s+\[registry\]/i }
        elsif ($rest =~ /#\s*todo/i) { $verb eq 'not ok' ? $skip++ : $pass++ }
        elsif ($verb eq 'not ok')    { $fail++ }
        else                         { $pass++ }
    }
    close $fh;
    printf "%-14s pass=%-5d fail=%-4d skip=%-4d registry=%-4d stale=%d\n",
        $name, $pass, $fail, $skip, $reg, $stale;
    print "    STALE:$_\n" for @stale_lines;
}

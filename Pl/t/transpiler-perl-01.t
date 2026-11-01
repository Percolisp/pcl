#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later
# transpiler-perl-01.t — A PROGRAM THAT SETS $ENV{PATH} KEEPS ITS RUN-TIME
# TRANSPILES (task #3822, s531d).
#
# A run-time `require` of a module not yet in the cache, and every string
# eval, spawn pl2cl under perl.  The runtime used to find that perl with a
# PATH search AT SPAWN TIME, and a program's `$ENV{PATH} = ...` is a real
# setenv -- so URI's t/cwd.t (PATH=/bin:/usr/bin) ran the SYSTEM perl
# against the perlbrew perl's Config.pm and every later module load died
# "Failed to transpile".  The perl is now resolved once, at process start
# (*p-transpiler-perl*).  The program below points PATH at a directory whose
# only `perl` is a stub that fails, so the row means the same thing on every
# machine (CI's /usr/bin/perl IS its perl).  Expected = perl 5.40.3.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use File::Spec;
use FindBin qw($RealBin);

my $root = File::Spec->rel2abs("$RealBin/../..");
my $pcl  = "$root/pcl";

plan skip_all => "pcl not found"  if !-x $pcl;
plan skip_all => "sbcl not found" if !`which sbcl 2>/dev/null`;

my $dir   = tempdir(CLEANUP => 1);
my $cache = tempdir(CLEANUP => 1);
sub write_file {
    my ($name, $body, $mode) = @_;
    open my $fh, '>', "$dir/$name" or die "$dir/$name: $!";
    print $fh $body;
    close $fh;
    chmod $mode, "$dir/$name" if $mode;
}
mkdir "$dir/fakebin" or die "mkdir: $!";
write_file('fakebin/perl', "#!/bin/sh\necho 'stub perl' >&2\nexit 7\n", 0755);
write_file('PM3822.pm', "package PM3822; sub v { 7 } 1;\n");
write_file('p.pl', qq{use lib "."; \$ENV{PATH} = "$dir/fakebin";\n}
                 . qq{my \$v = eval { require PM3822; PM3822::v() };\n}
                 . qq{print defined \$v ? "mod \$v\\n" : "died: \$\@";\n}
                 . qq{my \$e = eval "40 + 2"; print "eval \$e\\n";\n});

my $want = `cd '$dir' && perl p.pl 2>&1`;
is($want, "mod 7\neval 42\n", 'the oracle: perl runs the program');
for my $run (1, 2) {
    my $out = `cd '$dir' && PCL_CACHE_DIR='$cache' '$pcl' p.pl 2>&1 < /dev/null`;
    is($out, "mod 7\neval 42\n",
       "run $run: a require and a string eval after \$ENV{PATH} = <a dir without the real perl>");
}

done_testing();

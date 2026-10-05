#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# module-load-die-01.t -- a module whose LOAD dies prints only the program's own
# output (task #2764, s508a).
#
# `eval { require Optional::Thing; 1 } or fallback()` is the standard
# optional-dependency idiom.  When the module exists but its load dies, PCL
# loaded its cached CL TEXT through LOAD on a pathname, and SBCL's source loader
# printed its "While evaluating the form starting at line N, column 0 / of
# #P\"…\":" herald to STDERR for the die -- even though the caller CAUGHT it,
# on every run (a module that dies is never stored compiled).  The text is now
# loaded from a plain stream, where SBCL prints no herald.  Two runs, because
# the first builds the cache and the second reuses it.  INVERSE: main fee16466
# printed the two-line herald on both runs.

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
    my ($name, $body) = @_;
    open my $fh, '>', "$dir/$name" or die "$dir/$name: $!";
    print $fh $body;
    close $fh;
}
write_file('D.pm', qq{package D; print "D body\\n"; die "D died\\n"; 1;\n});
write_file('p.pl', qq{use lib "."; print "run1\\n"; eval { require D };\n}
                 . qq{print "caught: ", (\$@ =~ /^D died/ ? "yes" : "no:\$@"), "\\n"; print "run2\\n";\n});

for my $run (1, 2) {
    my $out = `cd '$dir' && PCL_CACHE_DIR='$cache' '$pcl' p.pl 2>'$dir/err' < /dev/null`;
    is($out, "run1\nD body\ncaught: yes\nrun2\n", "run $run: the program's output");
    open my $e, '<', "$dir/err" or die;
    my $err = do { local $/; <$e> };
    is($err, '', "run $run: nothing on STDERR for the caught die of a module's load");
}

done_testing();

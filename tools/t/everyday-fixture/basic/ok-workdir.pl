#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: tools/t fixture -- the work directory must be FRESH every run
use strict;
use warnings;
open my $fh, '>>', 'grow.txt' or die "grow.txt: $!";
print $fh "one more\n";
close $fh;
open my $rh, '<', 'grow.txt' or die "grow.txt: $!";
my @l = <$rh>;
close $rh;
print "grow.txt has ", scalar(@l), " line(s)\n";

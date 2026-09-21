#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: tools/t fixture -- script arguments must reach the program
# args: alpha "beta gamma"
use strict;
use warnings;
print "argc: ", scalar(@ARGV), "\n";
print "argv: ", join('|', @ARGV), "\n";

#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: tools/t fixture -- admissible: the answer is a fact of the program
use strict;
use warnings;
my %h = map { ("k$_" => $_) } 1 .. 40;
print join(',', sort keys %h), "\n";

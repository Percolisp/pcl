#!/usr/bin/perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Fixture for tools/t/cpan-scoreboard.t — NOT a PCL test.  A file that DIES
# after its first row, short of its plan (#3241): before the fix the board
# called it PASS 1/0.  It must read PARTIAL with a `*PLAN*` row.
use strict;
use warnings;
use Test::More tests => 3;

ok(1, 'fixture before the death');
die "fixture: dies after row 1\n";

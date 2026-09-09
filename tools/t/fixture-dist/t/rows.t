#!/usr/bin/perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Fixture for tools/t/cpan-scoreboard.t — NOT a PCL test.  It is a two-file
# "dist" the board instrument is run against, so the row extractor is exercised
# on a named failure (got/expected diagnostics), an UNNAMED failure (#1041's
# key) and a passing row.  Its verdicts are asserted there; do not "fix" it.
use strict;
use warnings;
use Test::More tests => 4;

is(1 + 1, 2, 'fixture named pass');
is(1 + 1, 3, 'fixture named fail');
ok(0);
ok(1, 'fixture last');

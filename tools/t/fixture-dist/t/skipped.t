#!/usr/bin/perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Fixture for tools/t/cpan-scoreboard.t — NOT a PCL test.  A file that skips
# itself whole: zero ok, zero not-ok, which the board's "zero ok" rule calls
# FAIL.  It is here so the `*FILE*` synthetic row (the only way such a file can
# appear in a file of failing assertions) and the PERL-SKIP cause are covered.
use strict;
use warnings;
use Test::More;

plan skip_all => 'fixture: nothing to run here';

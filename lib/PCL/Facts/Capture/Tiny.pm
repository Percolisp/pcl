# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# PCL FACTS OVERLAY for Capture::Tiny (task #2878; docs/shipped-modules.md
# "Facts overlays").  Declarations only: PCL reads this beside the real
# module's source; the real module still runs and installs the real subs.
package Capture::Tiny;

# Capture::Tiny builds its eight API subs with a STRING eval in a loop over
# `keys %api` (`eval "sub $sub(&;@) {...}"`), so a static parse sees neither
# the subs nor their `(&;@)` prototype -- and without it `capture { ... }`
# is not a block-form call.
sub capture (&;@);
sub capture_stdout (&;@);
sub capture_stderr (&;@);
sub capture_merged (&;@);
sub tee (&;@);
sub tee_stdout (&;@);
sub tee_stderr (&;@);
sub tee_merged (&;@);

# Its export lists are computed (`@EXPORT_OK = keys %api`,
# `%EXPORT_TAGS = (all => \@EXPORT_OK)`), so the export scan reads nothing.
our @EXPORT_OK = qw(capture capture_stdout capture_stderr capture_merged
                    tee tee_stdout tee_stderr tee_merged);
our %EXPORT_TAGS = (all => [qw(capture capture_stdout capture_stderr capture_merged
                               tee tee_stdout tee_stderr tee_merged)]);

1;

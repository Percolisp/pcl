# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Fixture for Pl/t/facts-overlay-01.t (task #2878): its overlay declares
# CONST with a DIFFERENT prototype than this source does -- a conflict.
package FactsConflict;
sub CONST () { 1 }
1;

#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: tools/t fixture -- a deliberate non-zero exit status
# expect-rc: 2
use strict;
use warnings;
print "leaving with 2\n";
exit 2;

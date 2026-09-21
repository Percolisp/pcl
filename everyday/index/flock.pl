# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s493 perlfunc index walk / pf-flock.pl
use Fcntl qw(:flock); open my $h, ">", "/tmp/s493-flock.$$" or die; print flock($h, LOCK_EX) ? "locked\n" : "no\n"; print flock($h, LOCK_UN) ? "un\n" : "no\n"; close $h; unlink "/tmp/s493-flock.$$";

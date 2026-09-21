# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-19-Sys-Hostname-Env-English.pl
use strict; use warnings; use Sys::Hostname; use Env qw(HOME @PATH); use English qw(-no_match_vars);
my $h = hostname(); "abc" =~ /b/; print join(" ", (length($h) > 0 ? "host" : "nohost"), ($HOME eq $ENV{HOME} ? "home" : "nohome"), (scalar(@PATH) > 1 ? "path-list" : "path:" . scalar(@PATH)), ($PROCESS_ID == $$ ? "pid" : "nopid"), ($PROGRAM_NAME eq $0 ? "name" : "noname"), (defined $OS_ERROR ? "oserr" : "x"), $OSNAME, ($EFFECTIVE_USER_ID == $> ? "euid" : "x"), do { local $LIST_SEPARATOR = "-"; my @l = (1, 2); "@l" }, do { local $OUTPUT_FIELD_SEPARATOR = ","; "ofs" }), "\n";

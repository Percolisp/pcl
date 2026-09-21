# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-17-IO-File-Handle.pl
use strict; use warnings; use IO::File; use IO::Handle;
my $p = "/tmp/pcl-mw-io-$$"; my $w = IO::File->new($p, "w") or die; $w->autoflush(1); $w->print("l1\n"); $w->printf("%s\n", "l2"); $w->say("l3") if $w->can("say"); $w->close;
my $r = IO::File->new($p, "r") or die; my $first = $r->getline; my @rest = $r->getlines; my $eof = $r->eof ? "eof" : "more"; $r->seek(0, 0); my $c = $r->getc; $r->close; chomp($first, @rest);
STDOUT->autoflush(1); STDERR->printf("%s", ""); print join(" ", $first, scalar(@rest), $eof, $c, ($r->opened ? "open" : "closed"), (IO::File->new("/nonexistent/x", "r") ? "opened" : "undef"), ref($w)), "\n"; unlink $p;

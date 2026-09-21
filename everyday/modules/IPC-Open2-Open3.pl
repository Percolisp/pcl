# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-24-IPC-Open2-Open3.pl
use strict; use warnings; use IPC::Open2; use IPC::Open3; use Symbol qw(gensym);
my $pid = open2(my $out, my $in, "tr", "a-z", "A-Z"); print $in "hello\n"; close $in; my $up = <$out>; waitpid($pid, 0); chomp $up;
my $err = gensym; my $pid3 = open3(my $w, my $r, $err, "sh", "-c", "echo out; echo err 1>&2; exit 4"); close $w; my $o = <$r>; my $e = <$err>; waitpid($pid3, 0); chomp($o, $e); print "$up $o $e rc=", $? >> 8, "\n";

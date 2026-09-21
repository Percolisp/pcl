# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-20-Fcntl-Errno.pl
use strict; use warnings; use Fcntl qw(:DEFAULT :flock :seek :mode); use Errno qw(ENOENT EEXIST EACCES);
my $p = "/tmp/pcl-mw-fcntl-$$"; sysopen(my $fh, $p, O_WRONLY | O_CREAT | O_EXCL, 0600) or die "sysopen: $!"; my $lk = flock($fh, LOCK_EX | LOCK_NB) ? "locked" : "nolock"; syswrite($fh, "abcdef"); sysseek($fh, 2, SEEK_SET); syswrite($fh, "XY"); close $fh;
my $again = sysopen(my $f2, $p, O_WRONLY | O_CREAT | O_EXCL) ? "created" : ($! == EEXIST ? "EEXIST" : "other:$!"); open(my $r, "<", $p) or die; my $txt = <$r>; close $r; my $mode = (stat $p)[2]; unlink $p;
open(my $no, "<", "/nonexistent-$$") or my $err = ($! == ENOENT && $!{ENOENT} ? "ENOENT" : "other"); print join(" ", $lk, $again, $txt, sprintf("%04o", S_IMODE($mode)), (S_ISREG($mode) ? "reg" : "notreg"), $err, (exists $!{EACCES} ? "errno-hash" : "nohash")), "\n";

# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-08-File-Temp.pl
use strict; use warnings; use File::Temp qw(tempfile tempdir);
my $dir = tempdir(CLEANUP => 1); my ($fh, $name) = tempfile(DIR => $dir, SUFFIX => ".tmp"); print $fh "payload"; close $fh;
my $obj = File::Temp->new(DIR => $dir); print $obj "obj"; $obj->flush; print join(" ", (-d $dir ? "dir" : "nodir"), (-s $name), ($name =~ /\.tmp$/ ? "suffix" : "nosuffix"), (-s $obj->filename), (index($name, $dir) == 0 ? "inside" : "outside")), "\n";

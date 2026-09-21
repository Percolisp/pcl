# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-25-Archive-Tar-Compress.pl
use strict; use warnings; use Archive::Tar;
my $d = "/tmp/pcl-mw-tar-$$"; mkdir $d; open(my $o, ">", "$d/a.txt") or die; print $o "alpha\n"; close $o; my $tar = Archive::Tar->new; $tar->add_data("b.txt", "beta\n"); $tar->add_files("$d/a.txt"); $tar->write("$d/t.tar") or die $tar->error;
my $rd = Archive::Tar->new("$d/t.tar") or die; my @names = sort map { $_ =~ s{.*/}{}r } $rd->list_files; print "@names ", $rd->get_content("b.txt") =~ s/\n//r, " ", (-s "$d/t.tar") % 512 == 0 ? "blocked" : "odd", "\n"; unlink "$d/a.txt", "$d/t.tar"; rmdir $d;

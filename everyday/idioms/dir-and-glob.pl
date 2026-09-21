# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-15-dir-and-glob.pl
no warnings;
mkdir "/tmp/pcl-s491d-$$"; for my $n (qw(a.txt b.txt c.log)) { open my $o, ">", "/tmp/pcl-s491d-$$/$n"; close $o } opendir(my $d, "/tmp/pcl-s491d-$$"); my @f = sort grep { !/^\./ } readdir $d; closedir $d; my @g = map { s{.*/}{}r } sort glob("/tmp/pcl-s491d-$$/*.txt"); unlink glob("/tmp/pcl-s491d-$$/*"); rmdir "/tmp/pcl-s491d-$$"; print "@f | @g\n";

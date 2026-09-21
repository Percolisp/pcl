# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-14-file-io.pl
no warnings;
my $f = "/tmp/pcl-s491-$$.txt"; open(my $o, ">", $f) or die; print $o "l1\nl2\nl3\n"; close $o; open(my $i, "<", $f) or die; my @l = <$i>; close $i; open($i, "<", $f); my $n = 0; while (my $x = <$i>) { chomp $x; $n++ if $x =~ /l[23]/ } close $i; open(my $a, ">>", $f); print $a "l4\n"; close $a; print scalar(@l), " $n ", -s $f, " ", (-e $f ? "e" : "ne"); unlink $f; print " ", (-e $f ? "e" : "gone"), "\n";

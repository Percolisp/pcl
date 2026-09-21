# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-30-getopt-long.pl
no warnings;
use Getopt::Long; local @ARGV = ("--name=bob", "-v", "-v", "--num", "3", "rest"); my ($name, $v, $num) = ("", 0, 0); GetOptions("name=s" => \$name, "v+" => \$v, "num=i" => \$num) or die; print "$name $v $num @ARGV\n";

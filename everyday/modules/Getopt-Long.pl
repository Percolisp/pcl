# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-02-Getopt-Long.pl
use strict; use warnings; use Getopt::Long qw(GetOptionsFromArray :config no_ignore_case bundling);
my @args = qw(-v -v --name=ann --num 42 --list a --list b --define k=v --no-color -x file1 file2);
my %o = (color => 1); my @list; my %def; my $verbose = 0;
my $ok = GetOptionsFromArray(\@args, "v+" => \$verbose, "name=s" => \$o{name}, "num=i" => \$o{num}, "list=s" => \@list, "define=s" => \%def, "color!" => \$o{color}, "x" => \$o{x}, "help|h" => \$o{help});
print "$ok $verbose $o{name} $o{num} @list $def{k} $o{color} $o{x} ", (defined $o{help} ? "help" : "nohelp"), " rest=@args\n";

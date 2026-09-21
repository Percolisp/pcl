# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-07-File-Path-Find.pl
use strict; use warnings; use File::Path qw(make_path remove_tree); use File::Find;
my $root = "/tmp/pcl-mw-fp-$$"; my @made = make_path("$root/a/b", "$root/c"); for my $f ("$root/a/1.txt", "$root/a/b/2.txt", "$root/c/3.log") { open(my $o, ">", $f) or die; close $o }
my @found; find(sub { push @found, $File::Find::name =~ s/^\Q$root\E//r if -f }, $root); my @dirs; finddepth({ wanted => sub { push @dirs, $_ if -d && $_ ne "." }, no_chdir => 0 }, $root);
print scalar(@made), " ", join(",", sort @found), " ", join(",", sort @dirs), " ", remove_tree($root) > 0 ? "removed" : "kept", " ", (-e $root ? "exists" : "gone"), "\n";

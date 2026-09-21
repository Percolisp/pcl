# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-28-real-dir-walker.pl
use strict; use warnings; use File::Find; use File::Path qw(make_path remove_tree); use File::Basename; use File::Spec; use File::Temp qw(tempdir); use Cwd qw(getcwd abs_path);
my $root = tempdir("pcl-b2-walk-XXXXXX", TMPDIR => 1, CLEANUP => 1);
make_path("$root/src/lib", "$root/src/t", "$root/docs");
my %files = ("src/main.pl" => 120, "src/lib/A.pm" => 300, "src/lib/B.pm" => 45, "src/t/a.t" => 80, "docs/README.md" => 999, "docs/notes.txt" => 10);
for my $rel (sort keys %files) { open(my $o, ">", "$root/$rel") or die; print $o "x" x $files{$rel}; close $o }
my (%by_ext, @big); my $dirs = 0;
find({ no_chdir => 1, wanted => sub { if (-d $_) { $dirs++; return } my ($name, $dir, $ext) = fileparse($_, qr/\.[^.]*/); $by_ext{$ext}{n}++; $by_ext{$ext}{bytes} += -s $_; push @big, File::Spec->abs2rel($_, $root) if -s _ > 100 } }, $root);
print "dirs=$dirs ", join(" ", map { "$_:$by_ext{$_}{n}/$by_ext{$_}{bytes}" } sort keys %by_ext), "\n", "big: @{[ sort @big ]}\n";
my @pm = sort map { basename($_) } glob("$root/src/lib/*.pm"); my $cat = File::Spec->catfile("a", "b", "c.txt"); my ($v, $d, $f) = File::Spec->splitpath("/x/y/z.txt");
print "@pm $cat $d $f ", dirname("/x/y/z.txt"), " ", (File::Spec->file_name_is_absolute($root) ? "abs" : "rel"), " ", (abs_path($root) ? "resolved" : "unresolved"), " ", (getcwd() ? "cwd" : "nocwd"), "\n";
remove_tree("$root/src"); print((-d "$root/src" ? "still" : "removed"), " ", (-d "$root/docs" ? "docs-kept" : "docs-gone"), "\n");

# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s493 perlfunc index walk / pf-chop-chomp.pl
my $s = "line\n"; my $r = chomp($s); print "$r [$s]\n"; my @l = ("a\n", "b\n"); chomp(@l); print "@l\n"; { local $/ = "xx"; my $t = "fooxx"; chomp $t; print "$t\n" } my $c = "abc"; chop $c; print "$c\n"; my ($p, $q) = ("x\n", "y\n"); chomp($p, $q); print "$p$q\n"; print lc, "\n" for "ABC"; $_ = "  trim  "; s/^\s+|\s+$//g; print "[$_]\n"; my $str = "aXbXc"; (my $cp = $str) =~ s/X/-/g; print "$cp $str\n"; my $cnt = () = $str =~ /X/g; print "$cnt\n"; my $new = $str =~ s/X/+/gr; print "$new\n"; print join(",", "a1b22c333" =~ /(\d+)/g), "\n"; print "yes\n" if "foobar" =~ /^foo(?=bar)/; $_ = "x=1,y=2"; my %kv = /(\w)=(\d)/g; print join(",", map {"$_$kv{$_}"} sort keys %kv), "\n"; (my $t = "hello") =~ tr/a-y/b-z/; print "$t\n"; my $up = ($str =~ tr/a-z//); print "$up\n";

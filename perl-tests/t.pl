my $s = "aabbcc"; my $n = $s =~ s/b/X/rg; print "$s\n$n\n";
my $t = "hello"; my $u = $t =~ s/xyz/Q/r; print "$t\n$u\n";

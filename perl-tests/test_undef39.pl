my $a = "old_a"; my $b = "old_b";
(undef, $a, undef, $b) = qw(1 2 3 4);
print "a=$a b=$b\n";
print "expected: a=2 b=4\n";

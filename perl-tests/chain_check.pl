my @data = ([5,3,2], [5,5,4]);
foreach my $item (@data) {
    my ($a,$b,$c) = ($item->[0], $item->[1], $item->[2]);
    my $chain = ($a == $b != $c);
    my $explicit = !!($a == $b && $b != $c);
    print "[$a,$b,$c] chain=$chain explicit=$explicit match=",
          ($chain eq $explicit ? "YES" : "NO"), "\n";
}

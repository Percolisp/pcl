my @data = ([5,3,2], [5,5,4]);
foreach my $item (@data) {
    my $r = $item->[0] == $item->[1] != $item->[2];
    print "got=[$r]\n";
}

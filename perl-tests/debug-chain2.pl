BEGIN {
    chdir 't' if -d 't';
    require "./test.pl";
}

foreach (
    [5,3,2],
) {
    my $part1 = join(",", "x", $_->[0] == $_->[1] != $_->[2], "y");
    my $part2 = join(",", "x", !!($_->[0] == $_->[1] && $_->[1] != $_->[2]), "y");
    print "part1=[$part1]\n";
    print "part2=[$part2]\n";
    is($part1, $part2, "chain cmp");
}

done_testing();

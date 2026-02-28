BEGIN {
    chdir 't' if -d 't';
    require "./test.pl";
}

foreach (
    [5,3,2],
) {
    my $a = $_->[0];
    my $b = $_->[1];
    my $c = $_->[2];
    my $ab_eq = ($a == $b);
    my $bc_ne = ($b != $c);
    my $ab_and_bc = ($ab_eq && $bc_ne);
    my $bb = !!$ab_and_bc;
    my $bb2 = !!($a == $b && $b != $c);
    my $bb3 = !!($_->[0] == $_->[1] && $_->[1] != $_->[2]);
    print "a=$a b=$b c=$c\n";
    print "ab_eq=[$ab_eq] bc_ne=[$bc_ne]\n";
    print "ab_and_bc=[$ab_and_bc]\n";
    print "bb=[$bb]\n";
    print "bb2=[$bb2]\n";
    print "bb3=[$bb3]\n";
    my $join_bb3 = join(",", "x", $bb3, "y");
    print "join_bb3=[$join_bb3]\n";
    my $inline = join(",", "x", !!($_->[0] == $_->[1] && $_->[1] != $_->[2]), "y");
    print "inline=[$inline]\n";
}

done_testing();

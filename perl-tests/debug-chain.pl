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
    print "a=$a b=$b c=$c\n";
    my $eq_ab = ($a == $b);
    my $ne_bc = ($b != $c);
    print "eq_ab=[$eq_ab] ne_bc=[$ne_bc]\n";
    my $and_result = ($eq_ab && $ne_bc);
    print "and_result=[$and_result]\n";
    my $bang_bang = !!$and_result;
    print "bang_bang=[$bang_bang]\n";
    my $direct = !!($a == $b && $b != $c);
    print "direct=[$direct]\n";
    my $aref_direct = !!($_->[0] == $_->[1] && $_->[1] != $_->[2]);
    print "aref_direct=[$aref_direct]\n";
}

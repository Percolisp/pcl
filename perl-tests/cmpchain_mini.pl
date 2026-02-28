BEGIN {
    require "./test.pl";
}
use feature "isa";
my @data = ([5,3,2], [5,5,4]);

foreach my $item (@data) {
    my $v0 = $item->[0];
    my $v1 = $item->[1];
    my $v2 = $item->[2];
    my $got = join(",", "x", $v0 == $v1 != $v2, "y");
    my $exp = join(",", "x", !!($v0 == $v1 && $v1 != $v2), "y");
    if ($got eq $exp) {
        print "ok - $v0 == $v1 != $v2\n";
    } else {
        print "not ok - $v0 == $v1 != $v2 got='$got' exp='$exp'\n";
    }
}

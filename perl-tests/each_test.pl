my @a = ("x", "y", "z");
my ($k, $v);
for (; ($k, $v) = each @a ;) {
    print "$k=$v\n";
}

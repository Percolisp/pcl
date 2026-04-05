my @a = qw(crunch zam bloop);
my ($k, $v) = (0, 0);
while ($k = each @a) {
    print "k=$k\n";
    $v++;
}
print "ran $v iterations\n";

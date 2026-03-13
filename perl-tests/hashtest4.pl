my @arr = ("key", "value");
my $r = \@arr;
print "last_idx = ", $#$r, "\n";
print "last_idx2 = ", $#{$r}, "\n";
for my $i (0..$#$r) {
    print "i=$i val=", $r->[$i], "\n";
}

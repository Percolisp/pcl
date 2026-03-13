use strict;
my @comma = ("key", "value");
my %comma = @comma;
my @temp = %comma;
# simulate eq_array
my $a = \@comma;
my $b = \@temp;
print "ref a: ", ref($a), "\n";
print "ref b: ", ref($b), "\n";
my $len_ok = (@$a == @$b);
print "len_ok: ", $len_ok ? "1" : "0", "\n";
for my $i (0..$#$a) {
    print "i=$i a[$i]=", $a->[$i]//"undef", " b[$i]=", $b->[$i]//"undef", "\n";
}

BEGIN {
    chdir 't' if -d 't';
    require './test.pl';
}
plan tests => 6;
my @a = ("key", "value");
my @b = ("key", "value");
my $ra = \@a;
my $rb = \@b;
print "# last_idx_a=", $#$ra, "\n";
print "# last_idx_b=", $#$rb, "\n";
ok(ref($ra) eq 'ARRAY', "ref is ARRAY");
ok(@$ra == @$rb, "same length");
for my $i (0..$#$ra) {
    print "# i=$i a=", $ra->[$i], " b=", $rb->[$i], "\n";
    ok($ra->[$i] eq $rb->[$i], "elem $i matches");
}

my $c = 0;
my @a = split /-(?{ $c++ })/, "a-b-c";
print scalar(@a), "\n";
print "@a\n";

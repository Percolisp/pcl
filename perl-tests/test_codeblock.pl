use Test::More;
my $c = 0;
my @a = split /-(?{ $c++ })/, "a-b-c";
print scalar(@a), "\n";
done_testing;

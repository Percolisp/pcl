use Test::More tests => 3;
my ($a, $b);
# like in split.t before test 38/39
$_ = join '|', split(/,|(-)/, "1-10,20,,,");
is($_, "1|-|10|undef|20");   # test 34
my @ary = split(/,|(-)/, "1-10,20,,,");
my $cnt = split(/,|(-)/, "1-10,20,,,");
is($cnt, scalar(@ary));     # test 35

# now the undef assignment  
(undef, $a, undef, $b) = qw(1 2 3 4);
is("$a|$b", "2|4");

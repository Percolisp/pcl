use Test::More tests => 2;
my $x;
is($x, undef, "undeclared my");
my $y = undef;
is($y, undef, "explicit undef");

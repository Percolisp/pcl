sub check_pos { pos $_[0] = 3; return pos $_[0]; }
my $x = "hello"; print check_pos($x), "\n";

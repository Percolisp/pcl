my $pattern = "\x{E4}";
my $str = "a\x{E4}b";
my @parts = split /$pattern/, $str;
print "count: ", scalar(@parts), "\n";
print "parts: ", join(":", @parts), "\n";

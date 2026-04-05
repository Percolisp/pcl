# This is the exact test from concat.t
my $dx = "\x{10f2}";
$_ = $dx;
s/($dx)/$dx$1/;
print "test4 got:      ", join(" ", map { ord($_) } split //, $_), "\n";
print "test4 expected: ", join(" ", map { ord($_) } split //, "$dx$dx"), "\n";
print "test4 match: ", ($_ eq "$dx$dx") ? "ok" : "not ok", "\n";

my $dx = "\x{10f2}";
$_ = $dx;
s/($dx)/$dx$1/;
print "got:      ", length($_), " chars\n";
print "expected: ", length("$dx$dx"), " chars\n";
print "ok: ", ($_ eq "$dx$dx") ? "yes" : "no", "\n";

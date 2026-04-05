my @x;
push @x, length reverse for split "-", "A--B";
print "x0=", defined($x[0]) ? $x[0] : "undef", "\n";
print "x1=", defined($x[1]) ? $x[1] : "undef", "\n";
print "x2=", defined($x[2]) ? $x[2] : "undef", "\n";

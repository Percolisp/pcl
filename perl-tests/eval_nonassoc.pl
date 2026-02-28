my ($a,$b,$c) = (1,2,3);
my $r1 = eval('sub { $a <=> $b == $c }');
my $r2 = eval('sub { $a <=> $b cmp $c }');
my $r3 = eval('sub { $a == $b == $c }');
my $r4 = eval('sub { $a isa $b == $c }');
print "r1=", defined($r1) ? "defined" : "undef", " err='$@'\n";
print "r2=", defined($r2) ? "defined" : "undef", " err='$@'\n";
print "r3=", defined($r3) ? "defined" : "undef", " err='$@'\n";
print "r4=", defined($r4) ? "defined" : "undef", " err='$@'\n";

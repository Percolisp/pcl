my $r1 = eval("sub { my (\$a,\$b,\$c) = (1,2,3); \$a <=> \$b == \$c }");
my $r2 = eval("sub { my (\$a,\$b,\$c) = (1,2,3); \$a == \$b == \$c }");
print "r1=", defined($r1) ? "defined" : "undef", "\n";
print "r2=", defined($r2) ? "defined" : "undef", "\n";

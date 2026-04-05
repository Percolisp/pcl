my $flag = "--";
# Standalone: works?
print "standalone /-/: ", ($flag =~ /-/ ? "yes" : "no"), "\n";

# In ternary false branch (the actual failing pattern):
my $r1 = $flag =~ /0/ && $flag !~ /-/
    ? "zero-pad"
    : $flag =~ /-/
        ? "left"
        : "right";
print "ternary result: $r1\n";

# Break it apart:
my $c1 = $flag =~ /0/;       print "c1 (/0/): $c1\n";
my $c2 = $flag !~ /-/;       print "c2 (!~/-/): $c2\n";
my $c3 = $c1 && $c2;         print "c3 (c1&&c2): $c3\n";
my $c4 = $flag =~ /-/;       print "c4 (/-/ after &&): $c4\n";

# Direct PCL test to understand the substitution behavior
my $dx = "\x{10f2}";
$_ = $dx;
# Test if the literal pattern ($dx) matches \x{10f2}
if (/(\$dx)/) {
    print "literal pattern matched, \$1=", ord($1), "\n";
} else {
    print "literal pattern did not match\n";
}
# Now test variable-interpolated pattern
if (/$dx/) {
    print "var pattern matched\n";
} else {
    print "var pattern did not match\n";
}

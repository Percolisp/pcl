"abc" =~ /(a)(b)/;
my $cap = $1;
print "before: cap='$cap' \$1='$1'\n";

# Now do another match that fails (or succeeds with different captures)
"xyz" =~ /(p)/;   # fails — should reset $1 to undef

print "after failed match: cap='$cap' \$1='", defined($1) ? $1 : "undef", "'\n";

# The real pattern from sprintf2.t:
"--" =~ /^([-+ 0]+)$/;
my $flag = $1;
print "flag before match: '$flag'\n";
$flag =~ /\+/;   # fails
print "flag after /+/ match: '$flag'\n";
$flag =~ /\ /;   # fails  
print "flag after / / match: '$flag'\n";
print "dash: ", ($flag =~ /-/ ? "yes" : "no"), "\n";

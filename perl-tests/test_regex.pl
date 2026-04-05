my $fmt = "%--4d";
if ($fmt =~ /^%([-+ 0]+)([1-9][0-9]*)d$/) {
    print "matched: flag='$1' width='$2'\n";
} else {
    print "no match\n";
}
# Also test simpler cases
my $s = "--";
print "dash in class: ", ($s =~ /^[-+ 0]+$/ ? "yes" : "no"), "\n";
print "dash direct:   ", ($s =~ /^[-]+$/    ? "yes" : "no"), "\n";
print "dash escaped:  ", ($s =~ /^\-+$/     ? "yes" : "no"), "\n";

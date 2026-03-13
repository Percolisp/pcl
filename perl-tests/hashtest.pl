use strict;
my @comma = ("key", "value");
my %comma = @comma;
my @temp = %comma;
print "temp has ", scalar(@temp), " elems\n";
print "temp[0]=", $temp[0] // "undef", " temp[1]=", $temp[1] // "undef", "\n";
# eq_array check
my $ok = (@comma == @temp);
print "same length: ", $ok ? "yes" : "no", "\n";

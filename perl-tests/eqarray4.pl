BEGIN {
    chdir 't' if -d 't';
    require './test.pl';
}
plan tests => 3;
my @comma = ("key", "value");
my %comma = @comma;
my @temp = %comma;
# Exactly what hashassign.t test 6 does:
ok(eq_array(\@comma, \@temp), 'list from comma hash');
# And what eq_array does internally:
my $a = \@comma;
my $b = \@temp;
print "# ref(a)=", ref($a), " ref(b)=", ref($b), "\n";
print "# len_a=", scalar(@$a), " len_b=", scalar(@$b), "\n";
ok(ref($a) eq 'ARRAY' && ref($b) eq 'ARRAY', "refs ok");
ok(scalar(@$a) == scalar(@$b), "lengths ok");

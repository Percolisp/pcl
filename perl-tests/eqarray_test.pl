BEGIN {
    chdir 't' if -d 't';
    require './test.pl';
}
plan tests => 3;
my @a = ("key", "value");
my %h = @a;
my @b = %h;
print "# a=[@a] b=[@b]\n";
print "# ref_a=", ref(\@a), " ref_b=", ref(\@b), "\n";
my $r = eq_array(\@a, \@b);
print "# eq_array result=$r\n";
ok($r, "eq_array works");
ok(eq_array(\@a, \@b), "eq_array inline");
ok(scalar(@a) == scalar(@b), "same length");

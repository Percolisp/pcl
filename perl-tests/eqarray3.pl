BEGIN {
    chdir 't' if -d 't';
    require './test.pl';
}
plan tests => 8;
my @comma = ("key", "value");
my %h = @comma;
my @temp = %h;
print "# comma=[@comma] temp=[@temp]\n";
my $rc = \@comma;
my $rt = \@temp;
print "# last_idx_c=", $#$rc, " last_idx_t=", $#$rt, "\n";
ok(ref($rc) eq 'ARRAY', "ref comma is ARRAY");
ok(ref($rt) eq 'ARRAY', "ref temp is ARRAY");
ok(@$rc == @$rt, "same length");
for my $i (0..$#$rc) {
    my $ce = $rc->[$i];
    my $te = $rt->[$i];
    print "# i=$i c=", $ce//"undef", " t=", $te//"undef", "\n";
    ok(defined($ce) && defined($te) && ($ce eq $te), "elem $i matches");
}
ok(eq_array($rc, $rt), "eq_array");

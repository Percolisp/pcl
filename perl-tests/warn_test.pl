my @warnings;
my $wa = [];
$SIG{__WARN__} = sub { push @warnings, $_[0] };

@warnings = ();
warn $wa;
print "count: ", scalar(@warnings), "\n";
print "ref: ", ref($warnings[0]), "\n";
print "same: ", ($warnings[0] == $wa ? "yes" : "no"), "\n";

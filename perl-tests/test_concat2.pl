my $dx = "\x{10f2}";
$_ = $dx;
s/($dx)/$dx$1/;
use Data::Dumper;
my @ords_got = map { ord($_) } split //, $_;
my @ords_exp = map { ord($_) } split //, "$dx$dx";
print "got ords: @ords_got\n";
print "exp ords: @ords_exp\n";

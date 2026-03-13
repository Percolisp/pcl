use strict;
use Test::More tests => 5;

my @subs;
for my $n (0..4) {
    push @subs, sub { $n };
}
is($subs[0]->(), 0, "foreach closure n=0");
is($subs[1]->(), 1, "foreach closure n=1");
is($subs[2]->(), 2, "foreach closure n=2");
is($subs[3]->(), 3, "foreach closure n=3");
is($subs[4]->(), 4, "foreach closure n=4");

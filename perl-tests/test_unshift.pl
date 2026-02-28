use strict;
use Test::More tests => 2;
my @a = (10);
unshift @a, (1,2,3);
is(scalar(@a), 4, 'unshift list literal - count');
is(join(' ', @a), '1 2 3 10', 'unshift list literal - values');

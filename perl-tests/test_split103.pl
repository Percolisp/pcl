use strict;
use warnings;
use Test::More;

my @split;
my $p="";
my $n;
my @a;

for my $pattern ("\x{E4}", "\x{437}") {
    utf8::upgrade $pattern;
    my @res;
    for my $str ("a${pattern}b", "axb", "a${pattern}b") {
        @split = split /$pattern/, $str;
        push @res, scalar(@split);
    }
    print "res: @res\n";
    is($res[0], 2);
    is($res[1], 1);
    is($res[2], 2, '#123469 - split with utf8 pattern after handling non-utf8 EXPR');
}

done_testing;

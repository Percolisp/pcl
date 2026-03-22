use strict;
use warnings;
# No our @split - it's used without declaration in the test
for my $pattern ("\x{E4}", "\x{437}") {
    utf8::upgrade $pattern;
    my @res;
    for my $str ("a${pattern}b", "axb", "a${pattern}b") {
        @split = split /$pattern/, $str;
        push @res, scalar(@split);
    }
    print join(",", @res), "\n";
}

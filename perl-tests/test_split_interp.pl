for my $pattern ("\x{E4}", "\x{437}") {
    my @res;
    for my $str ("a${pattern}b", "axb", "a${pattern}b") {
        my @split = split /$pattern/, $str;
        push @res, scalar(@split);
    }
    print join(",", @res), "\n";
}

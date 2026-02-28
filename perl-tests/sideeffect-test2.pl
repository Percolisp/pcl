BEGIN {
    chdir 't' if -d 't';
    require "./test.pl";
}

foreach ([5,3,2]) {
    my $e = "";
    my $r = ($e .= "a", $_->[0]) == ($e .= "b", $_->[1]) != ($e .= "c", $_->[2]);
    print "r=[$r] e=[$e]\n";
    is $e, "ab", "eval order for [5,3,2]";
}

done_testing();

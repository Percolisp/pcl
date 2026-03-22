my @tests = (["%s", [42], "42", "int"]);
for (@tests) {
    my ($tmpl, $evalData, $result, $cmt) = @$_;
    my $x = sprintf($tmpl, @$evalData);
    print "x=$x result=$result\n";
}

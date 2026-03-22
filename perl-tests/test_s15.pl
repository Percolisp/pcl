BEGIN { chdir 't' if -d 't'; require './test.pl'; }
plan(2);
my ($template, $data, $result, $comment) = ("%G", "12345.6789", "12345.7", "");
my $evalData = eval $data;
$evalData = ref $evalData ? $evalData : [$evalData];
my @tests = ([$template, $evalData, $result, $comment, $data]);
my ($w, $x, $y);
for (@tests) {
    ($template, $evalData, $result, $comment, $data) = @$_;
    $w = undef;
    $x = sprintf($template, @$evalData);
    $x = ">$x<" if defined $x;
    $y = $x;
    if ($y =~ s/([Ee][-+])0(\d)/$1$2/) {}
    if ($x eq ">$result<") {
        ok(1, "PASS x='$x' result='$result'");
    } else {
        ok(0, "FAIL x='$x' result='$result'");
    }
}
ok(1, "done");

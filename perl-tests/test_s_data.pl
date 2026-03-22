BEGIN { chdir 't' if -d 't'; require './test.pl'; }
my @tests;
my ($template, $data, $result, $comment);
while (<DATA>) {
    s/<\s*$//;
    s/^\s*>//;
    ($template, $data, $result, $comment) = split(/<\s*>/, $_, 4);
    my $evalData = eval $data;
    $evalData = ref $evalData ? $evalData : [$evalData];
    push @tests, [$template, $evalData, $result, $comment, $data];
}
plan(scalar @tests);
my ($w, $x, $y);
for (@tests) {
    ($template, $evalData, $result, $comment, $data) = @$_;
    $w = undef;
    $x = sprintf($template, @$evalData);
    $x = ">$x<" if defined $x;
    $y = $x;
    if ($y =~ s/([Ee][-+])0(\d)/$1$2/) {}
    if ($x eq ">$result<" || $y eq ">$result<") {
        ok(1, "ok: $template data=$data");
    } else {
        ok(0, "FAIL: $template data=$data got=$x exp=>$result<");
    }
}
__DATA__
>%.0f<    >1.5<        >2<
>%d<      >42<         >42<
>%s<      >'hello'<    >hello<
>%G<      >12345.6789< >12345.7<
>%F<      >123456.789< >123456.789000<

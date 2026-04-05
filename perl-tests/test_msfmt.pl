sub mysprintf_int_flags {
    my ($fmt, $num) = @_;
    die "wrong format $fmt" if $fmt !~ /^%([-+ 0]+)([1-9][0-9]*)d\z/;
    my $flag  = $1;
    my $width = $2;
    print "flag='$flag' width='$width'\n";
    my $sign  = $num < 0 ? '-' :
        $flag =~ /\+/ ? '+' :
        $flag =~ /\ / ? ' ' :
        '';
    my $abs   = abs($num);
    my $padlen = $width - length($sign.$abs);
    print "sign='$sign' abs=$abs padlen=$padlen\n";
    print "0-flag: ", ($flag =~ /0/ ? "yes" : "no"), "\n";
    print "dash-flag: ", ($flag =~ /-/ ? "yes" : "no"), "\n";
    return
        $flag =~ /0/ && $flag !~ /-/
            ? $sign . '0' x $padlen . $abs
            : $flag =~ /-/
                ? $sign . $abs . ' ' x $padlen
                : ' ' x $padlen . $sign . $abs;
}
print mysprintf_int_flags("%--4d", 0), "\n";

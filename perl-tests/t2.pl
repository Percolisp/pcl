my @records = (
    { separator => '0', effective => '',  text => 'ab' },
    { separator => ';', effective => ';', text => 'a;b' },
);
for (@records) {
    my ($separator, $effective, $text) = @$_{qw(separator effective text)};
    print "sep='$separator' eff='$effective' text='$text'\n";
}

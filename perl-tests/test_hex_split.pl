use Test::More tests => 4;
my @ary = split(/\x{FE}/, "\x{FF}\x{FE}\x{FD}");
is(scalar @ary, 2, "split on \\x{FE}");
ok($ary[0] eq "\xFF" && $ary[1] eq "\xFD", "correct pieces");
my @ary2 = split(/\x{FE}\xFE/, "\xFF\x{FF}\xFE\x{FE}\xFD\x{FD}");
is(scalar @ary2, 3, "split on \\x{FE}\\xFE");
ok($ary2[0] eq "\xFF\xFF" && $ary2[1] eq "\xFE\xFE" && $ary2[2] eq "\xFD\xFD", "correct pieces");

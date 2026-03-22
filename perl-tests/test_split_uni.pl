my @ary = split(/\x{FE}/, "\x{FF}\x{FE}\x{FD}");
my $cnt = split(/\x{FE}/, "\x{FF}\x{FE}\x{FD}");
print "count: ", scalar(@ary), "\n";
print "cnt: $cnt\n";
print "ok: ", (@ary == 2 && $ary[0] eq "\xFF" && $ary[1] eq "\xFD" ? "yes" : "no"), "\n";
print "cnt eq arr: ", ($cnt == scalar(@ary) ? "yes" : "no"), "\n";

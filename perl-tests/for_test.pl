my ($sp) = grep /\s/u, map chr, reverse 128..255;
if ($sp) {
    print "sp found: ord=" . ord($sp) . "\n";
    for (["${sp}${sp}. /", "leading"],
         [".${sp}${sp}/",  "separator"],
         [". /${sp}${sp}", "trailing"]) {
        my ($str, $desc) = @$_;
        print "str=[$str] desc=[$desc]\n";
    }
}

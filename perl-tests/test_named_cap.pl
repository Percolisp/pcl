"2024-03-21" =~ /(?<year>\d{4})-(?<month>\d{2})-(?<day>\d{2})/;
print "$+{year} $+{month} $+{day}\n";
print $1, " ", $2, " ", $3, "\n";

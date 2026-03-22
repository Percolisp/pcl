my @r = split(/,|(-)/, "1-10,20,,,");
print join("|", map { defined $_ ? $_ : "undef" } @r), "\n";
print scalar(@r), "\n";

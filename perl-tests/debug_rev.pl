my @x;
push @x, length reverse for split "-", "\x{100}--0";
print scalar(@x), ": ";
print join(", ", @x), "\n";

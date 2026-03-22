our ($a, $b);
$a = "init_a"; $b = "init_b";
{
  local(undef, $a, undef, $b) = qw(1 2 3 4);
  print "inside: $a|$b\n";
}
print "outside: $a|$b\n";

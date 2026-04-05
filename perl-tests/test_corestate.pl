use feature ':5.10';
my $r = eval('CORE::state $x = 1;');
print "result: ", defined($r) ? $r : "(undef)", "\n";
print "err: '$@'\n";

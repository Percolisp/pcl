# Simplified from Perl's t/op/dor.t
# Test // (defined-or) and //=

my $x;

# // operator
$x = 1;
is($x // 0, 1, '// : left-hand operand defined');

$x = undef;
is($x // 1, 1, '// : left-hand operand undef');

$x = '';
is($x // 0, '', '// : left-hand operand defined but empty');

$x = 0;
is($x // 1, 0, '// : left-hand operand is zero');

# //= operator
$x = undef;
$x //= 1;
is($x, 1, '//=: left-hand operand undefined');

$x //= 0;
is($x, 1, '//=: left-hand operand defined');

$x = '';
$x //= 0;
is($x, '', '//=: left-hand operand defined but empty');

$x = 0;
$x //= 5;
is($x, 0, '//=: left-hand operand is zero');

# Chained //
$x = undef;
my $y = undef;
my $z = 3;
is($x // $y // $z, 3, 'chained //');

$y = 2;
is($x // $y // $z, 2, 'chained // stops at first defined');

# // with expressions
my @arr = (undef, 0, 3);
is($arr[0] // 7, 7, 'array element undef // default');
is($arr[1] // 7, 0, 'array element zero // default');
is($arr[2] // 7, 3, 'array element defined // default');

done_testing();

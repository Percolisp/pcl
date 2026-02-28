# Simple test to verify PCL test infrastructure

plan(5);

ok(1, "true is ok");
ok(1 == 1, "1 == 1");

is(2 + 2, 4, "addition works");
isnt(2 + 2, 5, "2 + 2 is not 5");

cmp_ok(10, '>', 5, "10 > 5");

#!/usr/bin/perl
BEGIN { require "./test.pl" }
plan 2;
local $! = 1;
my $string = "$!";
print "# string=$string\n";
is($string, "Operation not permitted", "errno 1");
$! = 1; my $s2 = "$!";
is($s2, "Operation not permitted", "errno 1 again");

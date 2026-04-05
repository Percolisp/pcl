package MyNum;
use overload "+" => \&add, "0+" => \&numify, q("") => \&stringify;
sub new { bless { val => $_[1] }, $_[0] }
sub add { MyNum->new($_[0]{val} + (ref $_[1] ? $_[1]{val} : $_[1])) }
sub numify { $_[0]{val} }
sub stringify { "MyNum(" . $_[0]{val} . ")" }
package main;
my $a = MyNum->new(5);
my $b = MyNum->new(3);
my $c = $a + $b;
print "$c\n";
print $a + 10, "\n";

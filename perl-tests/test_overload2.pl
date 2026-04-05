package MyStr;
use overload q("") => \&stringify;
sub new { bless { s => $_[1] }, $_[0] }
sub stringify { "<<" . $_[0]{s} . ">>" }
package main;
my $obj = MyStr->new("hello");
print "$obj\n";
print $obj . " world\n";

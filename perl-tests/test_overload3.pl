package MyNum;
use overload
  "+"   => \&add,
  "0+"  => \&numify,
  q("") => \&stringify,
  "<=>" => \&spaceship;

sub new       { bless { val => $_[1] }, $_[0] }
sub add       { my ($self, $other, $rev) = @_;
                MyNum->new($self->{val} + (ref $other ? $other->{val} : $other)) }
sub numify    { $_[0]{val} }
sub stringify { "MyNum(" . $_[0]{val} . ")" }
sub spaceship { my ($a, $b, $rev) = @_;
                $rev ? $b->{val} <=> $a->{val} : $a->{val} <=> $b->{val} }

package main;
my $a = MyNum->new(5);
my $b = MyNum->new(3);
my $c = $a + $b;
print "$c\n";              # MyNum(8)
print $a + 10, "\n";      # MyNum(15) ... wait, the handler returns a MyNum
my $n = $a + 0;
print $n + 0, "\n";       # 5 (numify)
my @sorted = sort { $a <=> $b } (MyNum->new(3), MyNum->new(1), MyNum->new(2));
print join(", ", @sorted), "\n";  # MyNum(1), MyNum(2), MyNum(3)

package List::Util;
use strict;
use warnings;
use Exporter 'import';

our @EXPORT_OK = qw(
    min max sum sum0 reduce first any all none notall product
    shuffle uniq uniqstr uniqnum uniqint
    pairs unpairs pairkeys pairvalues pairfirst pairgrep pairmap
    head tail zip mesh
);

our $VERSION = "1.63";

sub min {
    return undef unless @_;
    my $min = shift;
    for (@_) {
        $min = $_ if $_ < $min;
    }
    return $min;
}

sub max {
    return undef unless @_;
    my $max = shift;
    for (@_) {
        $max = $_ if $_ > $max;
    }
    return $max;
}

sub sum {
    return undef unless @_;
    my $sum = 0;
    $sum += $_ for @_;
    return $sum;
}

sub sum0 {
    my $sum = 0;
    $sum += $_ for @_;
    return $sum;
}

sub product {
    return undef unless @_;
    my $prod = 1;
    $prod *= $_ for @_;
    return $prod;
}

sub reduce {
    my $code = shift;
    return undef unless @_;
    my $acc = shift;
    for (@_) {
        local $a = $acc;
        local $b = $_;
        $acc = $code->($a, $b);
    }
    return $acc;
}

sub first {
    my $code = shift;
    for (@_) {
        return $_ if $code->($_);
    }
    return undef;
}

sub any {
    my $code = shift;
    for (@_) {
        return 1 if $code->($_);
    }
    return '';
}

sub all {
    my $code = shift;
    for (@_) {
        return '' unless $code->($_);
    }
    return 1;
}

sub none {
    my $code = shift;
    for (@_) {
        return '' if $code->($_);
    }
    return 1;
}

sub notall {
    my $code = shift;
    for (@_) {
        return 1 unless $code->($_);
    }
    return '';
}

sub shuffle {
    my @list = @_;
    for (my $i = $#list; $i > 0; $i--) {
        my $j = int(rand($i + 1));
        @list[$i, $j] = @list[$j, $i];
    }
    return @list;
}

sub uniq {
    my %seen;
    return grep { !$seen{"$_"}++ } @_;
}

sub uniqstr { uniq(@_) }
sub uniqnum {
    my %seen;
    return grep { !$seen{$_+0}++ } @_;
}
sub uniqint { uniqnum(@_) }

sub head {
    my ($n, @list) = @_;
    return @list[0..$n-1] if $n >= 0;
    return @list[0..$#list+$n];
}

sub tail {
    my ($n, @list) = @_;
    return @list[-$n..-1] if $n >= 0;
    return @list[-$n..$#list];
}

sub pairs {
    my @out;
    while (@_) {
        push @out, [shift, shift];
    }
    return @out;
}

sub unpairs {
    map { @$_ } @_;
}

sub pairkeys {
    my @out;
    while (@_) {
        push @out, shift;
        shift;
    }
    return @out;
}

sub pairvalues {
    my @out;
    while (@_) {
        shift;
        push @out, shift;
    }
    return @out;
}

sub pairfirst {
    my $code = shift;
    while (@_) {
        my ($k, $v) = (shift, shift);
        local $a = $k; local $b = $v;
        return ($k, $v) if $code->($k, $v);
    }
    return ();
}

sub pairgrep {
    my $code = shift;
    my @out;
    while (@_) {
        my ($k, $v) = (shift, shift);
        local $a = $k; local $b = $v;
        push @out, $k, $v if $code->($k, $v);
    }
    return @out;
}

sub pairmap {
    my $code = shift;
    my @out;
    while (@_) {
        my ($k, $v) = (shift, shift);
        local $a = $k; local $b = $v;
        push @out, $code->($k, $v);
    }
    return @out;
}

sub zip {
    my @arrays = @_;
    my $max = 0;
    $max = @$_ > $max ? @$_ : $max for @arrays;
    map { my $i = $_; map { $_->[$i] } @arrays } 0..$max-1;
}

sub mesh { zip(@_) }

1;

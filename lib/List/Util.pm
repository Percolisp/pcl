# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

package List::Util;
use strict;
use warnings;
use Exporter 'import';

our @EXPORT_OK = qw(
    min max sum sum0 reduce first any all none notall product
    shuffle uniq uniqstr uniqnum uniqint
    pairs unpairs pairkeys pairvalues pairfirst pairgrep pairmap
    head tail zip mesh
    maxstr minstr reductions sample
    zip_longest zip_shortest mesh_longest mesh_shortest
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

sub reduce (&@) {
    my $code = shift;
    return undef unless @_;
    # $a/$b belong to the CALLER's package (like sort), not List::Util's, so the
    # block `{ $a + $b }` — a plain anon sub compiled in the caller — reads the
    # caller's globals.  Real List::Util (XS) sets them via glob magic; the
    # pure-Perl path sets them by symbolic reference, saving/restoring so we
    # don't clobber the caller's $a/$b.
    my $caller = caller;
    no strict 'refs';
    no warnings 'once';
    my ($sa, $sb) = (${"${caller}::a"}, ${"${caller}::b"});
    my $acc = shift;
    for (@_) {
        ${"${caller}::a"} = $acc;
        ${"${caller}::b"} = $_;
        $acc = $code->();
    }
    (${"${caller}::a"}, ${"${caller}::b"}) = ($sa, $sb);
    return $acc;
}

sub first (&@) {
    my $code = shift;
    for (@_) {
        return $_ if $code->($_);
    }
    return undef;
}

sub any (&@) {
    my $code = shift;
    for (@_) {
        return 1 if $code->($_);
    }
    return '';
}

sub all (&@) {
    my $code = shift;
    for (@_) {
        return '' unless $code->($_);
    }
    return 1;
}

sub none (&@) {
    my $code = shift;
    for (@_) {
        return '' if $code->($_);
    }
    return 1;
}

sub notall (&@) {
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

# pair* expose each pair's key/value as the caller's $a/$b (see reduce above).
sub pairfirst (&@) {
    my $code = shift;
    my $caller = caller;
    no strict 'refs';
    no warnings 'once';
    my ($sa, $sb) = (${"${caller}::a"}, ${"${caller}::b"});
    my @found = ();
    while (@_) {
        my ($k, $v) = (shift, shift);
        ${"${caller}::a"} = $k;
        ${"${caller}::b"} = $v;
        if ($code->()) { @found = ($k, $v); last; }
    }
    (${"${caller}::a"}, ${"${caller}::b"}) = ($sa, $sb);
    return @found;
}

sub pairgrep (&@) {
    my $code = shift;
    my $caller = caller;
    no strict 'refs';
    no warnings 'once';
    my ($sa, $sb) = (${"${caller}::a"}, ${"${caller}::b"});
    my @out;
    while (@_) {
        my ($k, $v) = (shift, shift);
        ${"${caller}::a"} = $k;
        ${"${caller}::b"} = $v;
        push @out, $k, $v if $code->();
    }
    (${"${caller}::a"}, ${"${caller}::b"}) = ($sa, $sb);
    return @out;
}

sub pairmap (&@) {
    my $code = shift;
    my $caller = caller;
    no strict 'refs';
    no warnings 'once';
    my ($sa, $sb) = (${"${caller}::a"}, ${"${caller}::b"});
    my @out;
    while (@_) {
        my ($k, $v) = (shift, shift);
        ${"${caller}::a"} = $k;
        ${"${caller}::b"} = $v;
        push @out, $code->();
    }
    (${"${caller}::a"}, ${"${caller}::b"}) = ($sa, $sb);
    return @out;
}

sub zip {
    my @arrays = @_;
    my $max = 0;
    $max = @$_ > $max ? @$_ : $max for @arrays;
    map { my $i = $_; map { $_->[$i] } @arrays } 0..$max-1;
}

sub mesh { zip(@_) }

# Not yet implemented in PCL — exported as dying stubs so `use List::Util
# qw(...)` succeeds (the real Exporter hard-dies on a missing export, which kills
# the whole importing file); only an actual CALL fails.
sub maxstr       { die "List::Util::maxstr is not yet implemented in PCL\n" }
sub minstr       { die "List::Util::minstr is not yet implemented in PCL\n" }
sub reductions   { die "List::Util::reductions is not yet implemented in PCL\n" }
sub sample       { die "List::Util::sample is not yet implemented in PCL\n" }
sub zip_longest  { die "List::Util::zip_longest is not yet implemented in PCL\n" }
sub zip_shortest { die "List::Util::zip_shortest is not yet implemented in PCL\n" }
sub mesh_longest { die "List::Util::mesh_longest is not yet implemented in PCL\n" }
sub mesh_shortest{ die "List::Util::mesh_shortest is not yet implemented in PCL\n" }

1;

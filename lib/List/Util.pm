# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

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

# product() of NO arguments is 1, the multiplicative identity — unlike sum(),
# which is undef on an empty list.  (Scalar-List-Utils t/product.t row 1.)
sub product {
    my $prod = 1;
    $prod *= $_ for @_;
    return $prod;
}

sub maxstr {
    return undef unless @_;
    my $max = shift;
    for (@_) { $max = $_ if $_ gt $max }
    return $max;
}

sub minstr {
    return undef unless @_;
    my $min = shift;
    for (@_) { $min = $_ if $_ lt $min }
    return $min;
}

# Every block-taking function here takes its first argument as CODE, and perl
# distinguishes the two ways that can be wrong: something that is not a code ref
# at all, and a code ref naming a sub that was never defined.  One helper so all
# ten answer identically — t/undefined-block.t checks every one of them, and a
# per-function copy would drift the moment one message changed.
sub _need_code {
    my ($code, $name) = @_;
    ref($code) eq 'CODE' or die "Not a subroutine reference";
    defined(&$code)      or die "Undefined subroutine in $name";
    return $code;
}

sub reduce (&@) {
    my $code = shift;
    _need_code($code, "reduce");
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
    _need_code($code, "first");
    for (@_) {
        return $_ if $code->($_);
    }
    return undef;
}

sub any (&@) {
    my $code = shift;
    _need_code($code, "any");
    for (@_) {
        return 1 if $code->($_);
    }
    return '';
}

sub all (&@) {
    my $code = shift;
    _need_code($code, "all");
    for (@_) {
        return '' unless $code->($_);
    }
    return 1;
}

sub none (&@) {
    my $code = shift;
    _need_code($code, "none");
    for (@_) {
        return '' if $code->($_);
    }
    return 1;
}

sub notall (&@) {
    my $code = shift;
    _need_code($code, "notall");
    for (@_) {
        return 1 unless $code->($_);
    }
    return '';
}

# $List::Util::RAND is List::Util's documented randomness hook: when it holds a
# CODE ref, shuffle() and sample() call it instead of rand().  Both dists' tests
# `local`ise it to a constant and then assert that two calls agree — so honouring
# it is what makes those rows a real determinism check instead of a coin flip.
our $RAND;

sub _rand_below {
    my ($n) = @_;
    return int(($RAND ? $RAND->() : rand()) * $n);
}

sub shuffle {
    my @list = @_;
    for (my $i = $#list; $i > 0; $i--) {
        my $j = _rand_below($i + 1);
        @list[$i, $j] = @list[$j, $i];
    }
    return @list;
}

# The four uniq* differ in HOW they judge equality and in WHAT they hand back:
#   uniqstr  string equality; undef is coerced to "" and RETURNED as ""
#   uniq     string equality too, but undef is its OWN value, returned as undef
#   uniqint  the values TRUNCATED to integers — compared and returned as such
#   uniqnum  numeric equality at full NV precision
# uniqnum's key is the raw 8-byte double, not its stringification: perl prints
# an NV with ~15 significant digits, which collapses 1.4142135623730951 and
# 1.4142135623730954 into one key even though they are different doubles
# (Scalar-List-Utils t/uniqnum.t).  pack "d" is exactly the bits, so equal keys
# mean equal numbers and nothing else.
# All four are the count in scalar context — that is just grep's own behaviour.
sub uniq {
    my %seen;
    my $saw_undef = 0;
    return grep { defined($_) ? !$seen{"$_"}++ : !$saw_undef++ } @_;
}

sub uniqstr {
    my %seen;
    return grep { !$seen{$_}++ } map { defined($_) ? "$_" : "" } @_;
}

sub uniqnum {
    my %seen;
    return grep { !$seen{ pack "d", (defined($_) ? $_ + 0 : 0) }++ } @_;
}

sub uniqint {
    my %seen;
    return grep { !$seen{$_}++ } map { defined($_) ? int($_) : 0 } @_;
}

# head/tail take a COUNT then a list — the `($@)` prototype is load-bearing:
# it is what makes `head` with no arguments die "Not enough arguments for
# List::Util::head" the way perl's does (t/head-tail.t).
#
# A negative count means "all but the last/first |n|", and BOTH ends clamp:
# `head 999, (4,5,6)` is the whole list (not 999 slots with undefs in them) and
# `head -999, (4,5,6)` is empty (not a reversed range).  Computing the count
# first and slicing once is what makes both clamps fall out.
sub head ($@) {
    @_ or die "Not enough arguments for List::Util::head";
    my $n = shift;
    $n = @_ + $n if $n < 0;
    $n = 0  if $n < 0;
    $n = @_ if $n > @_;
    return @_[0 .. $n-1];
}

sub tail ($@) {
    @_ or die "Not enough arguments for List::Util::tail";
    my $n = shift;
    $n = @_ + $n if $n < 0;
    $n = 0  if $n < 0;
    $n = @_ if $n > @_;
    return @_[@_-$n .. $#_];
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
    _need_code($code, "pairfirst");
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
    # Scalar context is a FOUND/NOT-FOUND answer, not the key/value pair's
    # element count — `scalar(pairfirst {...} ...)` is 1 or "" (t/pair.t).
    return @found if wantarray;
    return @found ? 1 : '';
}

sub pairgrep (&@) {
    my $code = shift;
    _need_code($code, "pairgrep");
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
    # Scalar context counts the PAIRS that matched, not the elements pushed —
    # so it is half the flat list (t/pair.t).  pairmap is the other way round:
    # its block may return any number of items, so its scalar answer IS the
    # element count, which `return @out` already gives.
    return @out if wantarray;
    return scalar(@out) / 2;
}

sub pairmap (&@) {
    my $code = shift;
    _need_code($code, "pairmap");
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

# zip/mesh take ARRAYREFS and differ only in their RESULT shape:
#   zip  ([1,2,3],[4,5,6])  ->  ([1,4], [2,5], [3,6])    — one arrayref per index
#   mesh ([1,2,3],[4,5,6])  ->  (1,4, 2,5, 3,6)          — the same, flattened
# `zip`/`mesh` are the _longest variants (short lists pad with undef); the
# _shortest ones stop at the shortest input.  PCL had zip returning the FLAT
# list and mesh aliased to it, so zip's rows compared arrayrefs against plain
# scalars (t/zip.t).
# Every argument must be an ARRAY reference; anything else (a plain scalar, a
# hashref) is an error, not something to coerce.  Checking once, up front,
# keeps the two zip bodies free of per-element guards.
sub _zip_check {
    for (@_) {
        ref($_) eq 'ARRAY' or die "Expected an ARRAY reference to zip";
    }
}

sub zip_longest {
    my @arrays = @_;
    _zip_check(@arrays);
    my $max = 0;
    for (@arrays) { $max = @$_ if @$_ > $max }
    return map { my $i = $_; [ map { $_->[$i] } @arrays ] } 0 .. $max-1;
}

sub zip_shortest {
    my @arrays = @_;
    _zip_check(@arrays);
    return () unless @arrays;
    my $min = scalar @{$arrays[0]};
    for (@arrays) { $min = @$_ if @$_ < $min }
    return map { my $i = $_; [ map { $_->[$i] } @arrays ] } 0 .. $min-1;
}

sub zip           { zip_longest(@_) }
sub mesh_longest  { map { @$_ } zip_longest(@_) }
sub mesh_shortest { map { @$_ } zip_shortest(@_) }
sub mesh          { mesh_longest(@_) }

# reductions() is reduce() that keeps every intermediate: the accumulator after
# each step, starting with the first element itself.  Same caller-$a/$b
# protocol as reduce (see there) — an empty list gives an empty list.
sub reductions (&@) {
    my $code = shift;
    _need_code($code, "reductions");
    return () unless @_;
    my $caller = caller;
    no strict 'refs';
    no warnings 'once';
    my ($sa, $sb) = (${"${caller}::a"}, ${"${caller}::b"});
    my $acc = shift;
    my @out = ($acc);
    for (@_) {
        ${"${caller}::a"} = $acc;
        ${"${caller}::b"} = $_;
        $acc = $code->();
        push @out, $acc;
    }
    (${"${caller}::a"}, ${"${caller}::b"}) = ($sa, $sb);
    return @out;
}

# sample($count, @values) — up to $count values picked without replacement, in
# random order.  A partial Fisher-Yates: swap a random survivor into each of the
# first $count slots, then take that prefix.  Asking for more than there are
# gives all of them (shuffled), never padding.
sub sample ($@) {
    my $n = shift;
    $n = @_ if $n > @_;
    return () if $n <= 0;
    my @list = @_;
    for my $i (0 .. $n-1) {
        my $j = $i + _rand_below(scalar(@list) - $i);
        @list[$i, $j] = @list[$j, $i];
    }
    return @list[0 .. $n-1];
}

1;

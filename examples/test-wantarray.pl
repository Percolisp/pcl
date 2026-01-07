#!/usr/bin/env perl
use strict;
use warnings;

# Test wantarray for regular subs
sub context_sub {
    if (wantarray()) {
        return (1, 2, 3);
    } else {
        return "scalar";
    }
}

# Test wantarray for methods
package Ctx;

sub new {
    my $class = shift;
    my $self = {};
    bless $self, $class;
    return $self;
}

sub context_method {
    my $self = shift;
    if (wantarray()) {
        return (10, 20, 30);
    } else {
        return "method-scalar";
    }
}

package main;

# Scalar context for sub
my $s = context_sub();
print "sub scalar: $s\n";

# List context for sub
my @a = context_sub();
print "sub list: @a\n";

# Scalar context for method
my $obj = Ctx->new();
my $ms = $obj->context_method();
print "method scalar: $ms\n";

# List context for method
my @ma = $obj->context_method();
print "method list: @ma\n";

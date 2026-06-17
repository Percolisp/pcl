# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.
#
# Pure-Perl shim for Sub::Util.  The real module is XS (it borrows
# List::Util's compiled core).  PCL cannot rename a compiled CL closure or
# read a Perl prototype off a code ref, but the *return contract* of these
# functions is what callers actually depend on:
#
#   set_subname / set_prototype return the SAME code ref back, so they can be
#   chained (`set_subname name => set_prototype '&@' => sub {...}`).
#
# Critically, merely *defining* set_subname here flips Sub::Defer's
# `_CAN_SUBNAME` to true, which makes defer_sub use its pure-Perl anonymous
# closure branch instead of the string-eval branch.  PCL's string eval runs
# in a subprocess and cannot capture the enclosing lexicals
# ($undeferred / $deferred_info), so the eval branch silently produces a
# broken deferred stub.  The closure branch captures them correctly, which is
# what makes Moo's lazy/sub-classed constructor bootstrap work.

package Sub::Util;
use strict;
use warnings;
use Exporter 'import';

our @EXPORT_OK = qw(
    prototype set_prototype
    subname set_subname
);

our $VERSION = "1.63";

sub prototype {
    my ($code) = @_;
    return CORE::prototype($code);
}

# Renaming a CL closure is not supported; the name is cosmetic (it only
# affects how the sub reports itself to caller()/debuggers).  Return the
# code ref unchanged so chaining works.
sub set_prototype {
    my ($proto, $code) = @_;
    return $code;
}

sub subname {
    my ($code) = @_;
    return '__ANON__';
}

sub set_subname {
    my ($name, $code) = @_;
    return $code;
}

1;

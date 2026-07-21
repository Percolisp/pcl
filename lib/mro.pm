# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.
#
# mro - PCL shim (C3-only).
#
# The real `mro` is a core, partly-XS facility PCL cannot transpile.  PCL's
# object system is CLOS-backed and dispatches in C3 *always*, so this shim
# provides only what real consumers need — `mro::get_linear_isa` computed in C3
# from @ISA — and accepts `use mro` / `require mro`.  Perl's DFS default, the
# ability to switch ordering, and the rest of the API (set_mro/get_mro/
# get_isarev/next::method/...) are intentionally NOT emulated; see
# docs/not-supported.md ("mro pragma") and docs/mro-plan.md.  This is provisional
# and will be revisited if a real module is shown to depend on the missing parts.

package mro;
use strict;
use warnings;

our $VERSION = '1.29_01';

# `use mro;` / `use mro 'c3';` — no-op (PCL is C3 already).  Not exported.
sub import { }
sub unimport { }

# get_linear_isa($class [, $type]) -> arrayref of class names, $class first,
# in C3 order.  $type is accepted and ignored (PCL is always C3; a DFS request
# still returns C3 — documented divergence).
sub get_linear_isa {
    my ($class, $type) = @_;
    return _c3_linearize($class);
}

# --- Tier 2, C3-only no-crash versions (t/mro files call these bare; an
# --- undefined-function abort would kill the whole file).  Values are
# --- honest for a C3-only world; DFS answers are a documented divergence.
sub get_mro { return 'c3' }             # PCL is always C3
sub set_mro { return }                  # ordering is not switchable — no-op

# Reverse-ISA would need to enumerate every known package (live stash
# support, deferred — docs/not-supported.md §symbol-table hashes); an empty
# list keeps callers running.
sub get_isarev { return [] }

sub is_universal {
    my ($class) = @_;
    return 1 if $class eq 'UNIVERSAL';
    no strict 'refs';
    for my $u (@{"UNIVERSAL::ISA"}) {
        return 1 if $u eq $class;
    }
    return 0;
}

# CLOS has no perl-style method cache to poke.
sub invalidate_all_method_caches { return }
sub method_changed_in { return }

# Standard C3 linearization over @ISA:
#   L[C] = C  followed by  merge( L[P1], ..., L[Pn], [P1, ..., Pn] )
sub _c3_linearize {
    my ($class, $seen) = @_;
    no strict 'refs';

    # Recursive @ISA (a class inheriting from itself, directly or through a
    # cycle) must die like perl, not exhaust the stack (t/mro/recursion_c3.t).
    $seen = { %{ $seen || {} } };
    if ($seen->{$class}++) {
        die "Recursive inheritance detected in package '$class'\n";
    }

    my @parents = @{"${class}::ISA"};

    # Leaf class: just itself.
    return [$class] unless @parents;

    my @seqs = map { [ @{ _c3_linearize($_, $seen) } ] } @parents;
    push @seqs, [@parents];

    my @result = ($class);
    while (1) {
        @seqs = grep { scalar @$_ } @seqs;   # drop emptied sequences
        last unless @seqs;

        # Pick the first head that does not appear in the tail of any sequence.
        my $cand;
        for my $seq (@seqs) {
            my $head = $seq->[0];
            my $in_tail = 0;
            for my $s (@seqs) {
                for my $i (1 .. $#$s) {
                    if ($s->[$i] eq $head) { $in_tail = 1; last; }
                }
                last if $in_tail;
            }
            unless ($in_tail) { $cand = $head; last; }
        }

        die "Inconsistent hierarchy during C3 merge of '$class'\n"
            unless defined $cand;

        push @result, $cand;

        # Remove the chosen class from the head of every sequence.
        for my $seq (@seqs) {
            shift @$seq if @$seq && $seq->[0] eq $cand;
        }
    }

    return \@result;
}

1;

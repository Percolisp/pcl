# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-shim: must-win -- the real module is XS.  This shim is found BEFORE @INC is
# searched, so a PERL5LIB or -I copy of the real module cannot shadow it
# (task #2462, docs/ir-spec.md 9, docs/shipped-modules.md).
#
# Hash::Util without XS (task #3163, batch s514e).  The real module is XS, so
# `use Hash::Util` died "Can't locate loadable object" -- and with it every
# program that restricts a hash (fields.pm's objects, lock_keys in tests).
#
# A RESTRICTED HASH IS A TIE HERE.  perl keeps the restriction in the hash
# itself (SvREADONLY + placeholders); PCL has no such bit, and adding one to
# every hash write would cost every program.  PCL's tie already sits only in
# the MISS branch of the element fast paths (a tied hash is an empty shell,
# docs/tie-aggregates.md), so a program that never restricts a hash pays
# nothing, and a restricted one pays a method call per access.  The tie is
# HIDDEN (builtin::hide_tie): tied(%h) answers undef for it, as in perl.
#
# What is real: lock_keys / unlock_keys / lock_keys_plus / lock_value /
# unlock_value / lock_hash / unlock_hash / the _ref_ and _recurse forms /
# legal_keys / hidden_keys / all_keys / hash_locked / hash_unlocked (and the
# ref forms) / hv_store; the deaths are perl's ("Attempt to access disallowed
# key 'k' in a restricted hash", "Attempt to delete readonly key 'k' from a
# restricted hash", "Modification of a read-only value attempted").
#
# What ANNOUNCES (docs/not-supported.md "Hash::Util: perl's hash internals"):
# bucket_ratio / num_buckets / used_buckets answer a plausible "used/total"
# shape, bucket_info / bucket_array / bucket_stats an empty answer,
# hash_seed / hash_value / hash_traversal_mask a fixed one -- PCL's hashes are
# SBCL hash tables with no perl buckets, seed or traversal order to report.
# Each says so on stderr, once.  The "Cannot usefully lock values in an
# unlocked hash" warning is not given (warnings-gated diagnostics, #221).

package Hash::Util;

use strict;
use warnings;
use Exporter 'import';

our $VERSION = '0.32';

our @EXPORT_OK = qw(
    all_keys
    lock_keys unlock_keys
    lock_value unlock_value
    lock_hash unlock_hash
    lock_keys_plus
    hash_locked hash_unlocked
    hashref_locked hashref_unlocked
    hidden_keys legal_keys
    lock_ref_keys unlock_ref_keys
    lock_ref_value unlock_ref_value
    lock_hashref unlock_hashref
    lock_ref_keys_plus
    hidden_ref_keys legal_ref_keys
    hash_seed hash_value hv_store
    bucket_stats bucket_stats_formatted bucket_info bucket_array
    lock_hash_recurse unlock_hash_recurse
    lock_hashref_recurse unlock_hashref_recurse
    hash_traversal_mask
    bucket_ratio
    used_buckets
    num_buckets
);

sub _croak { require Carp; goto &Carp::croak }

# The restriction of HASHREF: its hidden tie's state, or undef.
sub _state { builtin::hidden_tie_object($_[0]) }

# Make HASHREF restrictable (idempotent): move its contents into the state
# and tie it, hidden.
sub _restrict {
    my ($h) = @_;
    my $s = _state($h);
    return $s if $s;
    my %data = %$h;
    tie %$h, 'Hash::Util::_Restricted', \%data;
    builtin::hide_tie($h);
    return _state($h);
}

# Drop the restriction when nothing is restricted any more.
sub _maybe_release {
    my ($h) = @_;
    my $s = _state($h) or return;
    return if $s->{locked} || %{ $s->{ro} };
    my %data = %{ $s->{d} };
    untie %$h;
    %$h = %data;
}

sub lock_ref_keys {
    my ($h, @keys) = @_;
    my $s = _state($h);
    my @present = $s ? keys %{ $s->{d} } : keys %$h;
    if (@keys) {
        my %new = map { $_ => 1 } @keys;
        for my $k (@present) {
            _croak("Hash has key '$k' which is not in the new key set")
                if !$new{$k};
        }
    }
    $s = _restrict($h);
    $s->{legal} = { map { $_ => 1 } @present, @keys };
    $s->{locked} = 1;
    return $h;
}

sub unlock_ref_keys {
    my ($h) = @_;
    my $s = _state($h) or return $h;
    $s->{locked} = 0;
    $s->{legal} = {};
    _maybe_release($h);
    return $h;
}

sub lock_ref_keys_plus {
    my ($h, @extra) = @_;
    my $s = _state($h);
    my @present = $s ? keys %{ $s->{d} } : keys %$h;
    my @hidden = $s && $s->{locked} ? grep { !exists $s->{d}{$_} } keys %{ $s->{legal} } : ();
    return lock_ref_keys($h, @present, @hidden, @extra);
}

sub lock_ref_value {
    my ($h, $k) = @_;
    my $s = _restrict($h);
    $s->{ro}{$k} = 1;
    return $h;
}

sub unlock_ref_value {
    my ($h, $k) = @_;
    my $s = _state($h) or return $h;
    delete $s->{ro}{$k};
    _maybe_release($h);
    return $h;
}

sub lock_hashref {
    my ($h) = @_;
    lock_ref_keys($h);
    my $s = _state($h);
    $s->{ro}{$_} = 1 for keys %{ $s->{d} };
    return $h;
}

sub unlock_hashref {
    my ($h) = @_;
    my $s = _state($h) or return $h;
    $s->{ro} = {};
    return unlock_ref_keys($h);
}

sub lock_hashref_recurse {
    my ($h) = @_;
    lock_hashref($h);
    for my $v (values %$h) {
        lock_hashref_recurse($v) if ref $v eq 'HASH';
    }
    return $h;
}

sub unlock_hashref_recurse {
    my ($h) = @_;
    for my $v (values %$h) {
        unlock_hashref_recurse($v) if ref $v eq 'HASH';
    }
    return unlock_hashref($h);
}

sub hashref_locked   { my $s = _state($_[0]); return $s && $s->{locked} ? 1 : '' }
sub hashref_unlocked { return hashref_locked($_[0]) ? '' : 1 }

sub legal_ref_keys {
    my ($h) = @_;
    my $s = _state($h);
    return keys %$h if !$s || !$s->{locked};
    return (keys %{ $s->{d} }, grep { !exists $s->{d}{$_} } keys %{ $s->{legal} });
}

sub hidden_ref_keys {
    my ($h) = @_;
    my $s = _state($h);
    return () if !$s || !$s->{locked};
    return grep { !exists $s->{d}{$_} } keys %{ $s->{legal} };
}

sub   lock_keys (\%;@)      { lock_ref_keys(@_) }
sub unlock_keys (\%)        { unlock_ref_keys(@_) }
sub lock_keys_plus (\%;@)   { lock_ref_keys_plus(@_) }
sub   lock_value (\%$)      { lock_ref_value(@_) }
sub unlock_value (\%$)      { unlock_ref_value(@_) }
sub   lock_hash (\%)        { lock_hashref(@_) }
sub unlock_hash (\%)        { unlock_hashref(@_) }
sub   lock_hash_recurse (\%)   { lock_hashref_recurse(@_) }
sub unlock_hash_recurse (\%)   { unlock_hashref_recurse(@_) }
sub hash_locked (\%)        { hashref_locked(@_) }
sub hash_unlocked (\%)      { hashref_unlocked(@_) }
sub legal_keys (\%)         { legal_ref_keys(@_) }
sub hidden_keys (\%)        { hidden_ref_keys(@_) }

sub all_keys (\%\@\@) {
    my ($h, $keys, $hidden) = @_;
    @$keys = keys %$h;
    @$hidden = hidden_ref_keys($h);
    return $h;
}

# hv_store: perl stores the value without copying it; a store is a store here.
sub hv_store (\%$$) {
    my ($h, $k, $v) = @_;
    $h->{$k} = $v;
    return 1;
}

# --- perl's hash INTERNALS: announced, never computed ------------------------

my %announced;
sub _internal {
    my ($name) = @_;
    print STDERR "PCL: Hash::Util::$name answers a placeholder: PCL's hashes have "
        . "no perl buckets, seed or traversal order (see docs/not-supported.md)\n"
        if !$announced{$name}++;
}

sub _buckets_for { my $n = shift; my $t = 8; $t *= 2 while $t < $n; return $t }

sub bucket_ratio (\%) {
    _internal('bucket_ratio');
    my $n = scalar keys %{ $_[0] };
    return 0 if !$n;
    my $t = _buckets_for($n);
    return ($n < $t ? $n : $t) . "/$t";
}
sub num_buckets (\%)  { _internal('num_buckets');  return _buckets_for(scalar keys %{ $_[0] }) }
sub used_buckets (\%) {
    _internal('used_buckets');
    my $n = scalar keys %{ $_[0] };
    my $t = _buckets_for($n);
    return $n < $t ? $n : $t;
}
sub bucket_info  { _internal('bucket_info');  return () }
sub bucket_array { _internal('bucket_array'); return [] }
sub bucket_stats { _internal('bucket_stats'); return () }
sub bucket_stats_formatted { _internal('bucket_stats_formatted'); return '' }
sub hash_seed    { _internal('hash_seed');    return "\0" x 8 }
sub hash_value   { _internal('hash_value');   return 0 }
sub hash_traversal_mask { _internal('hash_traversal_mask'); return 0 }

# --- the restriction itself ---------------------------------------------------

package Hash::Util::_Restricted;

sub _croak { require Carp; goto &Carp::croak }

sub TIEHASH {
    my ($class, $data) = @_;
    return bless { d => $data, legal => {}, ro => {}, locked => 0 }, $class;
}

sub FETCH {
    my ($s, $k) = @_;
    return $s->{d}{$k} if exists $s->{d}{$k};
    _croak("Attempt to access disallowed key '$k' in a restricted hash")
        if $s->{locked} && !$s->{legal}{$k};
    return undef;
}

sub STORE {
    my ($s, $k, $v) = @_;
    _croak("Attempt to access disallowed key '$k' in a restricted hash")
        if $s->{locked} && !$s->{legal}{$k};
    _croak("Modification of a read-only value attempted") if $s->{ro}{$k};
    $s->{d}{$k} = $v;
}

sub DELETE {
    my ($s, $k) = @_;
    if ($s->{locked} && !$s->{legal}{$k}) {
        _croak("Attempt to delete disallowed key '$k' from a restricted hash");
    }
    _croak("Attempt to delete readonly key '$k' from a restricted hash")
        if $s->{ro}{$k} && exists $s->{d}{$k};
    return delete $s->{d}{$k};
}

sub CLEAR {
    my ($s) = @_;
    for my $k (keys %{ $s->{d} }) {
        _croak("Attempt to delete readonly key '$k' from a restricted hash")
            if $s->{ro}{$k};
    }
    %{ $s->{d} } = ();
}

sub EXISTS   { exists $_[0]{d}{$_[1]} }
sub FIRSTKEY { my $s = shift; my $n = keys %{ $s->{d} }; return scalar each %{ $s->{d} } }
sub NEXTKEY  { return scalar each %{ $_[0]{d} } }
sub SCALAR   { return scalar %{ $_[0]{d} } }

1;

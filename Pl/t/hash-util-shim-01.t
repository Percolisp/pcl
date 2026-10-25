#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# hash-util-shim-01.t -- task #3163 (s514e): `use Hash::Util` works without
# XS.  A restricted hash is a HIDDEN tie (lib/Hash/Util.pm's own class, tied()
# answers undef as in perl), so the element fast paths pay nothing -- the tie
# test already sits in their miss branch.  Before it, `use Hash::Util` died
# "Can't locate loadable object" -- so every row here fails on the base.  Each
# program prints LABELLED lines, compared with perl (nothing needs 5.40).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" if !-x $pl2cl;
plan skip_all => "sbcl not found"  if !`which sbcl 2>/dev/null`;

my $HELPERS = <<'H';
use strict; no warnings;
sub row { my ($l, @v) = @_; print "$l: ", join(' | ', @v), "\n" }
sub trap { my $c = shift; my $r = eval { $c->(); 1 };
           return $r ? 'lived' : 'died:' . ($@ =~ s/ at \S+ line \d+\.?\n?//sr =~ s/\n\z//r) }
H

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $HELPERS, $code;
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($file) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl $file");
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>/dev/null`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    return $output;
}

sub compare_lines {
    my ($code, $what) = @_;
    my $file = write_pl($code);
    my @perl = split /\n/, scalar `perl $file 2>/dev/null`;
    my %pcl;
    for my $l (split /\n/, run_cl($file)) {
        $pcl{$1} = $l if $l =~ /^(.+?): /;
    }
    for my $l (@perl) {
        my ($label) = $l =~ /^(.+?): /;
        is($pcl{$label} // "(no '$label' line)", $l, "$what: $label");
    }
}

# 1. lock_keys: the key set is fixed; reads, writes, deletes of other keys die.
compare_lines(<<'P', 'lock_keys');
use Hash::Util qw(lock_keys unlock_keys lock_keys_plus legal_keys hidden_keys all_keys
                  hash_locked hash_unlocked lock_ref_keys);
my %h = (a => 1, b => 2);
lock_keys(%h);
row('locked', hash_locked(%h) ? 1 : 0, hash_unlocked(%h) ? 1 : 0, defined(tied %h) ? 'tied' : 'untied');
row('legal', sort(legal_keys(%h)));
$h{a} = 10; row('allowed write', $h{a});
row('disallowed write', trap(sub { $h{c} = 3 }));
row('disallowed read', trap(sub { my $x = $h{c} }));
row('exists', exists $h{c} ? 1 : 0, exists $h{a} ? 1 : 0);
row('keys', sort keys %h);
delete $h{b};
row('deleted', join(',', sort keys %h), join(',', hidden_keys(%h)), defined $h{b} ? 'def' : 'undef');
$h{b} = 22; row('revive', $h{b});
row('delete disallowed', trap(sub { delete $h{zz} }));
my (@k, @hid); delete $h{b}; all_keys(%h, @k, @hid); row('all_keys', join(',', sort @k), join(',', sort @hid));
my @pairs; while (my ($k, $v) = each %h) { push @pairs, "$k=$v" } row('each', sort @pairs);
row('count', scalar(keys %h));
unlock_keys(%h); $h{c} = 3;
row('unlocked', join(',', sort keys %h), hash_locked(%h) ? 1 : 0);
my %p = (x => 1); lock_keys_plus(%p, 'y', 'z'); $p{y} = 2;
row('plus', join(',', sort(legal_keys(%p))), trap(sub { $p{w} = 1 }));
my %q = (a => 1, b => 2);
row('key set', trap(sub { lock_keys(%q, 'a') }));
lock_keys(%q, qw(a b c)); $q{c} = 3; row('explicit set', join(',', sort keys %q));
my $r = { k => 1 }; lock_ref_keys($r); row('ref', trap(sub { $r->{m} = 1 }), $r->{k});
P

# 2. lock_value / lock_hash: read-only values.
compare_lines(<<'P', 'lock_value');
use Hash::Util qw(lock_keys unlock_keys lock_value unlock_value lock_hash unlock_hash
                  lock_hash_recurse unlock_hash_recurse hv_store);
my %h = (a => 1, b => 2);
lock_keys(%h); lock_value(%h, 'a');
row('ro write', trap(sub { $h{a} = 99 }), $h{a});
row('ro incr', trap(sub { $h{a}++ }), $h{a});
row('ro delete', trap(sub { delete $h{a} }));
row('other write', trap(sub { $h{b} = 5 }), $h{b});
unlock_value(%h, 'a'); $h{a} = 7; row('unlock value', $h{a});
unlock_keys(%h);
lock_hash(%h);
row('lock_hash set', trap(sub { $h{a} = 6 }), trap(sub { delete $h{b} }),
    # which key a clear names first is hash ORDER, random in perl itself
    trap(sub { %h = () }) =~ s/key '\w+'/key K/r);
unlock_hash(%h); $h{a} = 8; $h{n} = 1; row('unlock_hash', $h{a}, join(',', sort keys %h));
my %deep = (in => { x => 1 }, v => 2);
lock_hash_recurse(%deep);
row('recurse', trap(sub { $deep{in}{x} = 2 }), trap(sub { $deep{in}{y} = 2 }));
unlock_hash_recurse(%deep); $deep{in}{y} = 3; row('unrecurse', $deep{in}{y});
my %s; hv_store(%s, 'q', 'val'); row('hv_store', $s{q});
my %big; $big{$_} = $_ for 1 .. 500; lock_keys(%big); my $sum = 0; $sum += $big{$_} for 1 .. 500;
row('big read', $sum);
P

# 3. A blessed restricted hash (fields.pm's shape).
compare_lines(<<'P', 'blessed');
use Hash::Util qw(lock_keys);
package Obj; sub new { my $s = bless { name => 'n', age => 1 }, shift; Hash::Util::lock_keys(%$s); $s }
sub name { $_[0]{name} }
package main;
my $o = Obj->new;
row('method', $o->name, ref $o);
row('typo', trap(sub { $o->{nmae} = 1 }));
P

# 4. The mechanism the blessed case needed: a DEREFERENCE argument fills a
# `\@` / `\%` prototype slot by reference (it was flattened).
compare_lines(<<'P', 'ref slot');
sub f (\@) { scalar @{$_[0]} } sub g (\%) { join ',', sort keys %{$_[0]} }
sub h (\@) { push @{$_[0]}, 9 }
my $r = [1, 2, 3]; my $hr = { a => 1, b => 2 };
row('array', f(@$r), f(@{$r}));
row('hash', g(%$hr), g(%{$hr}));
h(@$r); row('writes through', "@$r");
P

# 5. CLEAR of a restricted hash (s527b, PPI §55): the shim's CLEAR ends in
# `%{ $s->{d} } = ();` right after a `for` block, and PPI lexed that `%` as
# modulo (its back-scan took the block's last subscript `}` for the term end)
# -- the statement was dropped and `%h = ()` on a locked hash DIED.
compare_lines(<<'P', 'clear');
use Hash::Util qw(lock_keys);
my %h = (a => 1, b => 2); lock_keys(%h);
%h = (); row('cleared', scalar(keys %h));
$h{a} = 3; row('legal key after clear', $h{a});
row('illegal key after clear', trap(sub { $h{zz} = 1 }));
P

done_testing;

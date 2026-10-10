#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# attributes-shim-01.t -- task #3162 (s514e): `use attributes` works without
# XS.  lib/attributes.pm is perl's Perl half over the runtime's ONE sub-
# attribute record (builtin::sub_attributes / set_sub_attribute / sub_stash),
# written where a sub is defined (the p-sub :attributes fact, an anonymous
# sub's __pcl_set_attributes wrap).  Before it, perl's attributes.pm died at
# its XS _guess_stash, and an attribute nothing accepts was silently dropped --
# so every row here fails on the base.  Each program prints LABELLED lines,
# compared with the same program under perl (nothing needs perl 5.40).

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

# 1. attributes::get reads the built-in attributes, never the prototype.
compare_lines(<<'P', 'get');
use attributes;
sub plain { 1 } sub lv :lvalue { my $x } sub meth :method { 1 }
sub pro :prototype($) { 1 } sub both :lvalue :method { my $y }
row('plain', scalar(() = attributes::get(\&plain)));
row('lvalue', attributes::get(\&lv));
row('method', attributes::get(\&meth));
row('prototype', scalar(() = attributes::get(\&pro)), prototype(\&pro));
row('both', sort(attributes::get(\&both)));
my $anon = sub :lvalue { my $z };
row('anon', attributes::get($anon));
my $am = sub :method { 1 };
row('anon method', attributes::get($am));
row('not code', scalar(() = attributes::get(\my @a)), scalar(() = attributes::get(\my %h)));
row('reftype', attributes::reftype(\&plain), attributes::reftype({}), attributes::reftype([]),
    attributes::reftype(\my $s));
row('usage', trap(sub { attributes::get("x") }) =~ /Usage/ ? 'usage' : 'other');
P

# 2. Setting attributes at run time: `use attributes PKG, \&f, LIST`.
compare_lines(<<'P', 'set');
use attributes;
sub late { 1 }
attributes->import(__PACKAGE__, \&late, 'prototype($$)');
row('prototype set', prototype(\&late), scalar(() = attributes::get(\&late)));
sub m2 { 1 }
attributes->import(__PACKAGE__, \&m2, 'method');
row('method set', attributes::get(\&m2));
row('invalid', trap(sub { attributes->import(__PACKAGE__, \&late, 'nosuch') }));
row('invalid two', trap(sub { attributes->import(__PACKAGE__, \&late, 'nos1', 'nos2') }));
row('compile invalid', eval 'sub bad2 :nosuch2 { 1 } 1' ? 'lived' : ($@ =~ /Invalid CODE attribute: nosuch2/ ? 'dies-right' : "other: $@"));
P

# 3. MODIFY_ / FETCH_CODE_ATTRIBUTES: a package's own attributes.
compare_lines(<<'P', 'handlers');
package H;
our @seen;
sub MODIFY_CODE_ATTRIBUTES { my ($pkg, $code, @a) = @_; push @seen, @a; return grep { !/^(?:Mine|Other\(.*\))\z/ } @a }
sub FETCH_CODE_ATTRIBUTES { return ('Mine') }
sub f :Mine { 1 }
sub g :Mine :lvalue { my $q }
my $an = sub :Other(1 2) { 1 };
package main;
row('modify saw', @H::seen);
row('fetch', sort(attributes::get(\&H::f)));
row('fetch both', sort(attributes::get(\&H::g)));
row('rejected', eval 'package H; sub h :Bogus { 1 } 1' ? 'lived' : ($@ =~ /Invalid CODE attribute: Bogus/ ? 'dies-right' : "other: $@"));
P

done_testing;

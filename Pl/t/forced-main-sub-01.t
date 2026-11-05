#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# forced-main-sub-01.t -- task #4163.
#
# perl's gv.c (S_find_default_stash) resolves an unqualified identifier in
# `_ INC ENV SIG ARGV STDIN STDOUT STDERR ARGVOUT` in main:: from EVERY
# package -- a SUB as much as a variable: `package Q; sub INC {...}` defines
# main::INC, and a call `ENV()` in package Q calls main::ENV.  PCL defined and
# called Q::INC, so an @INC hook OBJECT whose class wrote `sub INC` worked in
# PCL where perl dies "Can't locate object method INC" (real code writes
# `sub Foo::INC`).  The one list is Pl::Environment::forced_main_name; the
# definition (Parser::_qualified_sub_to_cl) and the call / \&NAME / defined
# &NAME (ExprToCL::cl_name, _split_func_sym) ask it.  perl is the oracle at
# test time.  Every row FAILS on 59317afd.

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

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 3;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `$^X @{[ write_pl($code) ]} 2>/dev/null`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

both_agree(<<'PL', 'a sub named like a forced-main identifier, defined in another package, is main::\'s');
package Q; sub INC { 1 } sub ENV { 2 } sub STDIN { 3 } sub qq1 { 4 } sub ARGV { 5 } sub SIG { 6 } sub STDERR { 7 } sub ARGVOUT { 8 } sub STDOUT { 9 }
package main;
print join(',', map { no strict 'refs'; defined &{$_} ? 1 : 0 } qw(main::INC Q::INC main::ENV Q::ENV main::STDIN Q::STDIN Q::qq1)), "\n";
print join(',', map { no strict 'refs'; defined &{$_} ? 1 : 0 } qw(main::ARGV Q::ARGV main::SIG Q::SIG main::STDERR main::ARGVOUT main::STDOUT)), "\n";
print main::ENV(), main::INC(), "\n";
sub Foo::INC { 11 } print defined &Foo::INC ? "explicit Foo::INC\n" : "none\n";
PL

both_agree(<<'PL', 'a call, \&NAME and defined &NAME of such a name in another package reach main::\'s');
package Q; sub ENV { 2 } sub qq1 { 4 }
print ENV(), qq1(), "\n";
my $r = \&ENV; print &$r(), "\n";
print defined &ENV ? "d" : "u", defined &Q::ENV ? "d" : "u", "\n";
PL

both_agree(<<'PL', 'an @INC hook OBJECT whose class wrote `sub INC` has no INC method (perl dies)');
package Hk; sub new { bless {}, shift } sub INC { my ($self, $f) = @_; return unless $f eq 'HkMod.pm'; open my $fh, '<', \"package HkMod; sub v {7} 1;"; $fh }
package main;
unshift @INC, Hk->new;
my $ok = eval { require HkMod; 1 };
print $ok ? "loaded " . HkMod::v() . "\n" : "err: " . ($@ =~ /Can't locate object method "INC"/ ? "no INC method" : $@) . "\n";
PL

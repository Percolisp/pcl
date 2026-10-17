#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# A user sub named like a Test::More function (ok, is, explain, note, diag,
# pass, plan ...) is its CLASS's method and leaves the TAP layer's own
# function alone (#3405: the reader read `sub explain` in package E as the
# inherited pcl: TAP function and the install redefined it program-wide).
# Every row compares against perl (5.38-safe: core only).

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

# Run a Perl file and return output
sub run_perl {
    my ($code) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return `perl $file 2>&1`;
}

# Transpile and run CL, return output
sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    my $cl_code = PCLCore::transpile(qq{$pl2cl $pl_file});

    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^WARNING:.*\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_mc {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out   = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL:   $cl_out");
}

test_mc('#3405 subs named ok / is / explain / note / diag / pass / plan are methods of their class', <<'PERL');
package E; sub new { bless {}, shift }
sub explain { "ex" } sub note { "no" } sub diag { "di" } sub is { "is" }
sub ok { "ok" } sub pass { "pa" } sub plan { "pl" }
package main;
my $e = E->new;
print join(",", map { $e->$_ } qw(explain note diag is ok pass plan)), "\n";
print E::ok(), " ", E->can("explain") ? "can" : "cannot", "\n";
PERL

test_mc('#3405 a class with sub ok / is / explain beside `use Test::More`: both answer their own', <<'PERL');
use Test::More;
package C; sub new { bless {}, shift } sub ok { "C-ok" } sub is { "C-is" } sub explain { "C-ex" }
package main;
ok(1, "a"); is(2, 2, "b");
print "# ", C->new->ok, " ", C->new->is, " ", C->new->explain, "\n";
note explain [1];
done_testing;
PERL

test_mc('#3405 the Error::TypeTiny::Assertion shape: to_string calls $self->explain through an overload', <<'PERL');
package Err;
use overload '""' => sub { $_[0]->to_string }, fallback => 1;
sub new { my ($c, %a) = @_; bless {%a}, $c }
sub explain { my $s = shift; [ "value " . $s->{value} . " failed", "  check: Int" ] }
sub to_string { my $s = shift; join "\n", @{ $s->explain } }
package main;
my $e = Err->new(value => "x");
print "$e\n";
PERL

done_testing;

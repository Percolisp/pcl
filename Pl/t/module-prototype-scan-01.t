#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# module-prototype-scan-01.t — task #478: the prototype pre-scan reads EVERY
# module, not every module except a list of names.
#
# Pl::Parser::_extract_module_prototypes used to return undef for any module
# matching /^Test2::/ or /^Test::/ (except Test::More) — a compile-time cost
# measure that cost correctness, because a `(&)` prototype such a module
# declares never reached the block-form parser:
#
#     use Test2::Whatever;  my $r = blk { 42; };   # statement DROPPED
#     use Test2::Whatever;  my $r = blk { 43 };    # (pl-blk 43) — the block's
#                                                  # VALUE where perl passes a
#                                                  # CODE REF: silent wrong
#
# That was 79 of the 83 drops in the cpan-t census population, and a CPAN
# module name inside Pl/ is CLAUDE.md 9a's hard stop besides.
#
# THE ROWS ARE THE GUARD, and they are keyed on the MECHANISM, not on a real
# dist: each builds its own two-line module in a temp lib and differs from the
# next ONLY in the package name.  That is the discriminating measurement the
# task was filed from — `Test2::Fake` dropped and `My::Blk2` ran — so a
# re-introduced name list fails here whatever names it lists.  Every
# expectation is the live `perl` answer.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use File::Path qw(make_path);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 8;

my $libdir = tempdir(CLEANUP => 1);

# One fixture module per package name, identical but for the name.
sub make_module {
    my ($pkg) = @_;
    (my $rel = $pkg) =~ s{::}{/}g;
    my $path = "$libdir/$rel.pm";
    my ($dir) = $path =~ m{^(.*)/[^/]+$};
    make_path($dir);
    open my $fh, '>', $path or die "$path: $!";
    print $fh <<"PM";
package $pkg;
use strict; use warnings;
require Exporter;
our \@ISA = qw(Exporter);
our \@EXPORT = qw(blk twice);
sub blk (&)   { return "<" . \$_[0]->() . ">" }
sub twice (&) { my \$c = shift; return \$c->() + \$c->() }
1;
PM
    close $fh;
    return;
}

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (PCLCore::transpile FAILS the row on a dropped statement) and run.
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
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# The two spellings, against a module whose name the old list matched.
sub both_spellings {
    my ($pkg, $why) = @_;
    make_module($pkg);
    both_agree(<<"PL", "$pkg: block body ENDING IN A SEMICOLON ($why)");
use lib '$libdir';
use $pkg;
my \$r = blk { 42; };
print "r=\$r\\n";
PL
    both_agree(<<"PL", "$pkg: block body with NO semicolon ($why)");
use lib '$libdir';
use $pkg;
my \$s = twice { 21 };
print "s=\$s\\n";
PL
}

both_spellings('Test2::PclGuard',       'the /^Test2::/ half of the old skip');
both_spellings('Test::PclGuard',        'the /^Test::/ half of the old skip');
both_spellings('Test2::Deep::PclGuard', 'a deeper Test2:: name');
both_spellings('My::PclGuard',          'the CONTROL: never skipped, must not move');

#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# import-tag-01.t -- task #3184: an import through an export TAG, and a
# `use` inside a literal BEGIN-time string eval, carry the imported subs'
# prototypes (and builtin overrides) to the PARSE.
#
# Two static readings were missing (Pl::Parser::module_import_sets and
# Pl::Parser::use_statements_of):
#   (a) a tag list is read the way the module BUILDS it -- `@ARRAY` members
#       (Encode's `all => [ @EXPORT, @EXPORT_OK ]`), a tag from other tags,
#       a tag set inside BEGIN, and a leading `&` stripped (Text::Balanced's
#       `ALL => [ qw{ &extract_delimited ... } ]`);
#   (b) `BEGIN { eval q{ use M LIST } }` runs that `use` at BEGIN time, so
#       its imports are in force from there (File::Copy's Time::HiRes line).
# Before the fix each call below parsed as a call of an UNKNOWN sub: `f0 "a",
# "\n"` passed both arguments, `f2 { uc } "p", "q"` was DROPPED, `h2 @arr`
# passed the elements, and the run-time detector (#2610) announced each site.
# Every row compares against perl itself; the fixtures live in Pl/t/lib.

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
my $libdir  = "$RealBin/lib";

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 14;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "use lib '$libdir';\n$code";
    close $fh;
    return $pl_file;
}

# Transpile (PCLCore::transpile FAILS the row on a dropped statement) and run
# with the detector ON; returns (stdout, the detector's stderr lines).
sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my (undef, $err_file) = tempfile(SUFFIX => '.err', UNLINK => 1);
    local $ENV{PCL_DETECT} = 'announce';
    my $output = `sbcl @sbcl_rt --load $cl_file 2>$err_file`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    open my $e, '<', $err_file or die;
    my $detect = join '', grep { /^PCL: .*was parsed as/ } <$e>;
    return ($output, $detect);
}

sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my ($pcl, $detect) = run_cl($code);
    is($pcl . $detect, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- (a) a tag built from the module's own ARRAYS -------------------------

both_agree('use TagMod qw(:all); print f0 "a", "\n";',
           'a ($) sub through a tag of @EXPORT/@EXPORT_OK takes ONE argument');
both_agree('use TagMod qw(:all); print g1, "|", g1("z"), "\n";',
           '... a (;$) sub before a comma is a call with no argument');
both_agree('use TagMod qw(:all); my @r = f2 { uc } "p", "q"; print "@r\n";',
           '... a (&@) sub takes a BLOCK (was a dropped statement)');

# ---- (a) a tag set in BEGIN, its names spelled `&name` ---------------------

both_agree('use TagAmp qw(:ALL); print h1("m", "n"), "\n";',
           'a `&name` tag member imports the sub');
both_agree('use TagAmp qw(:ALL); my @arr = (1, 2, 3); print h2 @arr, "\n";',
           '... with its (\@) prototype: the array passes BY REFERENCE');

# ---- (a) a tag from other tags; a tag naming an overridable builtin -------

both_agree('use TagNest qw(:all); print n1 "a", "\n"; print n2, "|\n";',
           'a tag assembled from @{ $EXPORT_TAGS{...} } of earlier tags');
both_agree('use TagNest qw(:ov); print stat("x"), "\n";',
           'a tag that imports `stat` displaces the builtin');

# ---- (b) a literal `use` in a BEGIN-time string eval ----------------------

both_agree('BEGIN { eval q{ use TagMod qw(f0) } } print f0 "a", "\n";',
           'BEGIN { eval q{ use M LIST } } imports at BEGIN time');
both_agree(q{BEGIN { eval 'use TagNest qw(stat)' } print stat("x"), "\n";},
           "... a single-quoted body; an imported builtin name overrides it");
both_agree('BEGIN { eval q{ use NoSuchModXyz qw(f0) } } print "ok:", (defined &f0 ? 1 : 0), "\n";',
           '... a module that is not there: the eval guards it, nothing imported');
# ---- negatives ------------------------------------------------------------

# A string eval OUTSIDE BEGIN runs after the line below was parsed: f0 is
# unknown there, so perl passes @x as a LIST (f0(a,b)) -- a ($) prototype
# read from the eval would pass its COUNT (f0(2)).
both_agree('eval q{ use TagMod qw(f0) }; my @x = ("a", "b"); print f0(@x), "\n";',
           'NEGATIVE: a run-time eval of `use` is not an import at parse time');
both_agree('BEGIN { eval q{ use TagMod qw(f0) } } my @x = ("a", "b"); print f0(@x), "\n";',
           '... the same call after the BEGIN-time spelling passes the COUNT');

# A COMPUTED eval body is not read statically (perl imports it at BEGIN time
# all the same; task #3200 holds that divergence).  A call whose parse does
# not depend on the prototype still agrees.
both_agree('BEGIN { my $m = "TagMod"; eval "use $m qw(f0)" } print f0("a"), "\n";',
           'NEGATIVE: a computed eval body is unread; a paren call still runs');

# ---- a module the prototype scan SKIPS for cost (#3205) ---------------------
# Every IO:: module is skipped by name; a facts overlay states its prototypes.
# `isaFilename($)`: perl passes ONE argument, so the list has 2 elements.
both_agree('use IO::Compress::Base::Common 2.212; my @r = (isaFilename "x", "y"); print scalar(@r), " ", STATUS_ERROR + 1, "\n";',
           'a skipped module\'s prototypes come from its facts overlay');

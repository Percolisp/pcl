#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# proto-position-01.t -- task #2871: a PROTOTYPE applies only to calls parsed
# AFTER the declaration that carries it.
#
# perl compiles top-down: a call parsed before `sub f ($) {…}` is a plain
# list-operator call ("main::f() called too early to check prototype" under
# warnings), so `print f(@a); sub f ($) { $_[0] }` prints the FIRST ELEMENT,
# not the count.  PCL's prototype table is filled for the whole file before
# anything is lowered, so every call used to see every prototype -- a silent
# wrong in the common shape "main code at the top, prototyped helpers at the
# bottom".  Each record now carries the site of the statement that introduced
# it (a definition, a forward declaration, a `:prototype` attribute, a `use
# constant`, a module import at its `use`), and the lookup answers for the
# statement being lowered (Pl::Environment parse_site / reg_site; ir-spec §5).
#
# Every row compares against perl's STDOUT (warnings off).  No 5.40 syntax:
# CI's perl is 5.38.

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

plan tests => 12;

# A module exporting a `(\@)` sub, for the import-position rows.
my $libdir = tempdir(CLEANUP => 1);
make_path("$libdir/T2871");
open(my $mfh, '>', "$libdir/T2871/M.pm") or die "fixture: $!";
print $mfh <<'PM';
package T2871::M;
use strict; use warnings;
require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(mx);
sub mx (\@) { ref($_[0]) ? "ref:" . scalar(@{$_[0]}) : "val:$_[0]" }
1;
PM
close $mfh;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "use lib '$libdir';\nno warnings;\n$code";
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
    my $file = write_pl($code);
    my $perl = `perl $file 2>/dev/null`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- a call ABOVE the definition: no prototype --------------------------

both_agree('my @a = (10, 20, 30); print f(@a), "\n"; print g(@a), "\n";
            sub f ($) { $_[0] } sub g ($) { $_[0] }',
           '`($)` below the call: the list is passed, not its count');

both_agree('my @a = (1, 2, 3); print cnt(@a), "\n";
            sub cnt (\@) { ref($_[0]) ? "ref:" . scalar(@{$_[0]}) : "val:$_[0]" }',
           '`(\@)` below the call: the list, not a reference');

both_agree('my @a = (10, 20, 30); sub top { f(@a) } print top(), "\n";
            sub f ($) { $_[0] }',
           'a call inside a sub defined ABOVE the prototyped one');

both_agree('my @a = (10, 20, 30); print g(@a), "\n";
            sub g :prototype($) { $_[0] } print g(@a), "\n";',
           'a `:prototype` attribute: in force from its sub only');

both_agree('my @a = (10, 20, 30); sub f; print f(@a), "\n"; sub f ($) { $_[0] }',
           'a forward declaration WITHOUT the prototype does not carry it');

# ---- at or after the declaration: unchanged ----------------------------

both_agree('my @a = (10, 20, 30); sub f ($) { $_[0] } print f(@a), "\n";',
           '`($)` above the call: the count (unchanged)');

both_agree('my @a = (10, 20, 30); sub f ($); print f(@a), "\n"; sub f ($) { $_[0] }',
           'a forward declaration `sub f ($);` carries it from there');

# ---- the negative: a `(&@)` sub called with parens -----------------------

both_agree('my @r = blk(sub { "b" }, 2, 3); print "@r[1..2] ", $r[0]->(), "\n";
            sub blk (&@) { @_ } my @s = blk { "c" } 4, 5; print "@s[1..2] ", $s[0]->(), "\n";',
           '`(&@)` with parens above its definition: no difference');

# ---- use subs, use constant, a module import ---------------------------

both_agree('use subs qw(sh); my @r = (sh 1, 2); print scalar(@r), "\n";
            sub sh ($) { 9 } my @q = (sh 1, 2); print scalar(@q), "\n";',
           '`use subs` declares the name WITHOUT the later prototype');

both_agree('my $y = k; print "$y\n"; my $z = k . "x"; print "$z\n";
            use constant k => 3; print k . "x", "\n"; my $q = k; print "$q\n";',
           '`use constant` below: the bareword is the string above it');

both_agree('my @a = (1, 2, 3); print mx(@a), "\n"; use T2871::M; print mx(@a), "\n";',
           'an imported prototype is in force from its `use`');

both_agree('my @a = (1, 2, 3); use T2871::M; print mx(@a), "\n";',
           '... and a `use` above every call (unchanged)');

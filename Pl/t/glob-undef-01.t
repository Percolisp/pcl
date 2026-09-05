#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# glob-undef-01.t — s470bi, task #1020: `undef *GLOB` CLEARS EVERY SLOT.
#
# It used to clear NOTHING, in silence.  `p-undef`'s glob arm handed
# `p-glob-undef-name` the CL package NAME ("MAIN") where the callee wanted a
# PERL one; it case-inverted that to "main", found no such package, and its
# `(when pkg …)` swallowed the whole body.  So the statement ran and did
# nothing — the #964 failure mode, a plausible answer that is silently the old
# one — and the only row that tested it had been replaced by an inline SKIP.
#
# The fix routes `undef` through the SAME clear the glob-COPY path uses for a
# slot the source lacks (task #602's rule, rule 11): %p-glob-clear-{var,code,
# io}-slot, taking the PACKAGE OBJECT and the already-inverted name that a
# p-typeglob carries, so nothing is re-derived from strings.
#
# Every expectation below is the live perl 5.40.3 answer (probed s470bi,
# scratch/s470bi/p1020/).  The one spelling that still diverges is
# introspection — perl REMOVES an aggregate slot, PCL empties it, so
# `*a{ARRAY}` is undef in perl and a ref here (task #1117, and the reason
# perl-tests/sub.t row 24 stays failing).  Asserted nowhere in this file: a
# row that encoded the divergence would have to be rewritten when #1117 lands.

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

plan tests => 5;

# Transpile (a DROPPED statement fails the row, via PCLCore) and run.
sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = PCLCore::transpile("$pl2cl $pl_file");
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# ---- 1. the task's own reproducer: all four VALUE slots ---------------------
is(run_cl(<<'PERL'), "scalar: cleared\narray: cleared\nhash: cleared\ncode: cleared\n",
our $s = 5; our @a = (9); our %h = (k=>1);
sub c { "code" }
undef *s; undef *a; undef *h; undef *c;
print "scalar: ", (defined $s ? "STILL $s" : "cleared"), "\n";
print "array: ",  (@a ? "STILL @a" : "cleared"), "\n";
print "hash: ",   (%h ? "STILL " . join(",",%h) : "cleared"), "\n";
print "code: ",   (defined &c ? "STILL " . c() : "cleared"), "\n";
PERL
   'undef *GLOB clears the scalar, array, hash and code slots (#1020)');

# ---- 2. the CODE slot is *p-declared-subs* too ------------------------------
# `defined &name` reads that table, and a FORWARD declaration (`sub d;`) lives
# only there — perl answers 0 for `exists &c` and `exists &d` after the undef,
# so the entry has to go whether or not the symbol was fbound.
is(run_cl(<<'PERL'), "pre: d&c=1 e&c=1 d&d=0 e&d=1\npost: e&c=0 e&d=0\n",
sub c { "code" }
sub d;
print "pre: d&c=", (defined &c ?1:0), " e&c=", (exists &c ?1:0),
      " d&d=", (defined &d ?1:0), " e&d=", (exists &d ?1:0), "\n";
undef *c; undef *d;
print "post: e&c=", (exists &c ?1:0), " e&d=", (exists &d ?1:0), "\n";
PERL
   '... including the declared/defined table, so `exists &c` goes false too');

# ---- 3. the IO slot -------------------------------------------------------
# perl's `undef *FH` makes `*FH{IO}` undef and the handle unusable: a later
# `print FH` returns undef and sets $! to EBADF.  Losing the *p-filehandles*
# registration is what gives that here.
is(run_cl(<<'PERL'), "before: io=yes\nafter: io=undef\nprint: undef\n",
my $f = "/tmp/pcl_glob_undef_$$.txt";
open(FH, '>', $f) or die "open: $!";
print FH "one\n";
print "before: io=", (defined *FH{IO} ? "yes" : "undef"), "\n";
undef *FH;
print "after: io=", (defined *FH{IO} ? "yes" : "undef"), "\n";
my $r = print FH "two\n";
print "print: ", (defined $r ? $r : "undef"), "\n";
unlink $f;
PERL
   '... and the IO slot, so a later `print FH` fails as perl does');

# ---- 4. INVERSE: the glob-COPY path the clear was extracted from -----------
# `*A = *B` must still copy the slots B has and clear the ones it does not —
# the #602 behaviour whose helpers this change re-used.
is(run_cl(<<'PERL'), "x=7 s1=B2 z=cleared created=no\n",
our $x = 5; our %z = (k=>1); sub s1 { "S1" }
our $b2 = 7; sub b2 { "B2" }
*x = *b2;
*s1 = *b2;
*z = *neverdefinedglob;
print "x=", $x, " s1=", s1(),
      " z=", (%z ? "STILL" : "cleared"),
      " created=", (defined $main::neverdefinedglob ? "yes" : "no"), "\n";
PERL
   'inverse: *A = *B still copies bound slots and clears unbound ones (#602)');

# ---- 5. INVERSE: clearing never CREATES ------------------------------------
is(run_cl(<<'PERL'), "created=no arr=no\n",
undef *nosuchthing;
print "created=", (defined $main::nosuchthing ? "yes" : "no"),
      " arr=", (defined *nosuchthing{ARRAY} ? "yes" : "no"), "\n";
PERL
   'inverse: undef of a glob with no slots brings nothing into being');

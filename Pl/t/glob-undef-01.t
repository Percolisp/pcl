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
# scratch/s470bi/p1020/, and s484b, scratch/s484b/probe/).
#
# s484b, task #1117: the INTROSPECTION spelling agrees now too.  perl REMOVES
# an aggregate slot where PCL can only EMPTY it, so `*a{ARRAY}` after
# `undef *a` used to be a ref where perl says undef.  The cell still holds the
# fresh empty container — a makunbound cell cannot be re-vivified on read, and
# guarding every cell expansion costs 3-5% on element access (s473h) — but the
# container is REGISTERED in `*p-removed-agg-slots*`, and `*G{ARRAY}` /
# `*G{HASH}` answer undef while a registered container is still empty.  Rows
# 6-9 below are that table.
#
# TWO residue halves are NOT modelled and are documented, with the probe
# lines, in docs/not-supported.md "`undef *GLOB` leaves an EMPTY aggregate
# slot where perl REMOVES it": (a) a READ of @a (or an empty write) re-vivifies
# the slot in perl and not here, and (b) a vivifying write is noticed at the
# next READ of the slot, so a write undone before anything asks reads as absent
# where perl keeps it present.  Neither is asserted here: a row encoding a
# divergence would have to be rewritten when the divergence goes.

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

plan tests => 9;

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

# ---- 6. task #1117: the INTROSPECTION table, in ORDER -----------------------
# ORDER MATTERS and is the reason the first probe of this looked like
# agreement: in perl a READ of @a re-vivifies the slot, so the four slots have
# to be asked BEFORE anything touches them.  perl's answers (5.40.3): the
# SCALAR slot survives an undef as a ref to undef, the two AGGREGATE slots are
# GONE, and the CODE slot is gone.  A write then brings the slot back.
is(run_cl(<<'PERL'), "1 SCALAR=def ARRAY=undef HASH=undef CODE=undef\n2 after-push ARRAY=def\n3 after-hashwrite HASH=def\n",
our $s = 5; our @a = (9); our %h = (k=>1);
sub c { "code" }
undef *s; undef *a; undef *h; undef *c;
printf "1 SCALAR=%s ARRAY=%s HASH=%s CODE=%s\n",
  (defined *s{SCALAR} ? "def":"undef"), (defined *a{ARRAY} ? "def":"undef"),
  (defined *h{HASH} ? "def":"undef"),   (defined *c{CODE} ? "def":"undef");
push @a, 42;
printf "2 after-push ARRAY=%s\n", (defined *a{ARRAY} ? "def":"undef");
%h = (k=>1);
printf "3 after-hashwrite HASH=%s\n", (defined *h{HASH} ? "def":"undef");
PERL
   '#1117: after undef *G the aggregate slots are ABSENT, and a write brings them back');

# ---- 7. the other ways a slot arrives, and the other way it goes -----------
# `*a = \@o` installs a DIFFERENT container, so the slot is present AND is
# that very array; `*a2 = *b2` where b2 has only a scalar slot clears a2's
# array slot (#602's rule), so it is absent; a second `undef` of an already
# cleared glob is idempotent, and a push after it still vivifies.
is(run_cl(<<'PERL'), "1 ARRAY=def same=yes\n2 ARRAY=undef\n3 double-undef ARRAY=undef then=def\n",
our @a = (1,2); undef *a;
our @o = (7,8);
*a = \@o;
printf "1 ARRAY=%s same=%s\n", (defined *a{ARRAY} ? "def":"undef"),
   (\@o == *a{ARRAY} ? "yes":"no");
our @a2 = (1,2); undef *a2;
our $b2 = 5;
*a2 = *b2;
printf "2 ARRAY=%s\n", (defined *a2{ARRAY} ? "def":"undef");
our @d = (1); undef *d; undef *d;
my $before = (defined *d{ARRAY} ? "def":"undef");
push @d, 1;
printf "3 double-undef ARRAY=%s then=%s\n", $before, (defined *d{ARRAY} ? "def":"undef");
PERL
   '#1117: *a = \@o installs a present slot, *a = *b (no array) clears it, undef is idempotent');

# ---- 8. INVERSE: EMPTY is not ABSENT ---------------------------------------
# The mechanism keys on the CONTAINER a clear installed, never on emptiness:
# an array or hash that was created and is merely EMPTY has a slot, in perl and
# here, and so does a cleared glob that had an EMPTY foreign array assigned in.
is(run_cl(<<'PERL'), "1 empty-created ARRAY=def HASH=def\n2 assigned-empty ARRAY=def same=yes\n3 scalar-only ARRAY=undef SCALAR=def val=undef\n",
our @o = (); our %e = ();
printf "1 empty-created ARRAY=%s HASH=%s\n", (defined *o{ARRAY} ? "def":"undef"),
   (defined *e{HASH} ? "def":"undef");
our @c = (1); undef *c; our @e2 = ();
*c = \@e2;
printf "2 assigned-empty ARRAY=%s same=%s\n", (defined *c{ARRAY} ? "def":"undef"),
   (\@e2 == *c{ARRAY} ? "yes":"no");
our $s = 5; undef *s;
printf "3 scalar-only ARRAY=%s SCALAR=%s val=%s\n",
   (defined *s{ARRAY} ? "def":"undef"), (defined *s{SCALAR} ? "def":"undef"),
   (defined ${*s{SCALAR}} ? "def":"undef");
PERL
   'inverse: an EMPTY created aggregate still HAS its slot (and undef *s keeps SCALAR)');

# ---- 9. the two consumers that made this a bug -----------------------------
# perl-tests/sub.t row 24 (`goto &xsub when @_ does not exist`) is the row this
# task owns: `undef *_` then an xsub call through `&`, and *_{ARRAY} must be
# undef.  And Carp's guard idiom (Carp.pm:34/:122/:124, the only non-test
# consumer of the slot) asks exactly the first conjunct below — perl answers
# 1/0/0 for a written / undef'd / never-created hash slot.
#
# NOTE: Carp's FULL guard, `*$_{HASH} && exists $$_{$sub}`, still answers 0 in
# PCL for the WRITTEN glob, at its OTHER conjunct: `exists $$_{k}` on a
# GLOB-valued $_ is false here and true in perl (task #1726, pre-existing and
# unrelated to the slot table).  Only the slot conjunct is asserted here.
is(run_cl(<<'PERL'), "1 _ARRAY=undef\n2 written=1 undefd=0 never=0\n",
undef *_;
eval { &utf8::encode };
printf "1 _ARRAY=%s\n", (defined *_{ARRAY} ? "def":"undef");
our %written = (sub1 => 1);
our %undefd  = (sub1 => 1); undef *undefd;
my @out;
for my $g (*written, *undefd, *nevercreated) {
    local $_ = $g;
    push @out, (*$_{HASH} ? 1 : 0);
}
printf "2 written=%d undefd=%d never=%d\n", @out;
PERL
   '#1117: sub.t row 24s shape answers undef, and Carps hash-slot guard matches perl');

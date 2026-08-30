#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# glob-sub-alias-01.t — s454ae: CODE ASSIGNED TO A TYPEGLOB IS A SUB
# DECLARATION, prototype included, from the moment the assignment RUNS
# (Test2::Util's `BEGIN { *try = \&_manual_try }` family), and A MODULE'S
# SUBS ARE IN ITS STASH whatever the import list says (a package-QUALIFIED
# call site parses with the declared prototype:
# `List::Util::first { … } @list` unimported).
#
# Mechanism: Pl::Parser::glob_sub_alias_fact / glob_sub_alias_stmts /
# _glob_alias_sig_info (consumed by the module facts walk for every
# load-time assignment, and by Parser2's pre-scan for same-file BEGIN
# blocks only), Environment::add_pkg_prototype + _proto_entry's
# qualified-first read.  Every expectation below is the live perl 5.40
# answer (probed s454ae/s455).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
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

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (a DROPPED statement fails the row, via PCLCore) and run.
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

# ── 1. Same-file BEGIN, \&TARGET spelling: the alias carries the target's
# (&;@) prototype, so the block-form call parses ────────────────────────────
is(run_cl(<<'PL'),
sub _t (&;@) { my ($cb, @r) = @_; return ($cb->(), @r) }
BEGIN { *tt = \&_t; }
my @x = tt { 42 } 7, 8;
print "x=@x\n";
PL
   "x=42 7 8\n",
   'BEGIN { *tt = \&_t } makes tt a (&;@) block-form call');

# ── 2. The HAVE_PERLIO shape: agreeing anon-sub candidates across a ternary
# give a zero-arg prototype, so the name is a TERM (`HP ? … : …` must not
# read HP as a list operator that swallows the `?`) ─────────────────────────
is(run_cl(<<'PL'),
BEGIN { *HP = 1 ? sub () { 5 } : sub () { 6 }; }
my $v = HP ? "yes" : "no";
print "v=$v hp=", HP + 1, "\n";
PL
   "v=yes hp=6\n",
   '*HP = COND ? sub(){5} : sub(){6} makes HP a zero-arg term');

# ── 3. MODULE side: a file-scope alias in a use'd module runs at load, so
# it is a compile-time fact for the user file (Test2::Util's spelling) ──────
my $moddir = tempdir(CLEANUP => 1);
mkdir "$moddir/My" or die "mkdir: $!";
open my $mfh, '>', "$moddir/My/AliasMod.pm" or die "module fixture: $!";
print $mfh <<'MOD';
package My::AliasMod;
use strict; use warnings;
require Exporter; our @ISA = ('Exporter');
our @EXPORT = ('mtry', 'mkon');
sub _mt (&;@) { my ($cb, @r) = @_; return ($cb->(), @r) }
*mtry = \&_mt;
BEGIN { *mkon = sub () { 7 } }
1;
MOD
close $mfh;
is(run_cl(<<PL),
use lib "$moddir";
use My::AliasMod;
my \@x = mtry { 40 } 1;
print "x=\@x\\n";
print "k=", mkon + 1, "\\n";
PL
   "x=40 1\nk=8\n",
   'a use\'d module\'s file-scope *mtry = \&_mt is a (&;@) fact; BEGIN sub() a term');

# ── 4. QUALIFIED, UNIMPORTED: the module's stash answers the prototype even
# when the name was never imported (and the flat table must NOT change —
# List::Util is use'd empty here) ───────────────────────────────────────────
is(run_cl(<<'PL'),
use List::Util ();
my $f = List::Util::first { $_ > 1 } (1,2,3);
print "f=$f\n";
my $s = List::Util::reduce { $a + $b } (1,2,3,4);
print "s=$s\n";
PL
   "f=2\ns=10\n",
   'List::Util::first { … } LIST parses block-form without an import');

# ── 5. INVERSE: candidates that DISAGREE contribute no fact (perl's answer
# depends on which branch ran; guessing is how silent-wrongs start) — the
# parenthesized call still works ────────────────────────────────────────────
is(run_cl(<<'PL'),
BEGIN { *amb = 0 ? sub (&;@) { 1 } : sub () { 2 }; }
print "amb=", amb(), "\n";
PL
   "amb=2\n",
   'disagreeing candidates register nothing; amb() with parens still right');

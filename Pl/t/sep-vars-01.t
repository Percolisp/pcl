#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# sep-vars-01.t — task #465: `$\` and `$,` are UNDEF until set.
#
# perl's defaults are asymmetric and the asymmetry is load-bearing: `$/` is
# "\n", `$;` is "\034", `$!` is the errno object — all DEFINED — while `$\`
# (output record separator) and `$,` (output field separator) are UNDEF.  PCL
# initialised all five to a defined value, which is invisible on the WRITE side
# (an empty separator and an absent one print the same nothing) and decisive on
# the READ side: `defined($,)`, `if ($\)`, `$, // ","`, `length($\)`.
#
# The shape that found it is Test2::Formatter::TAP's
#     local($\, $,) = (undef, '') if $\ || $,;
# — PCL took the branch perl skips.  `$\ || $,` is FALSE either way ("" is
# false), so the guard that actually diverges is `defined`, which is why every
# row below asks the question in a way an empty string cannot answer.
#
# Every row is differential against real perl.

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
my @sbcl_rt      = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 4;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    binmode($fh, ':raw');
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    binmode($cl_fh, ':raw');
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub run_perl { my $f = write_pl($_[0]); return scalar `perl $f 2>&1` }

sub both_agree {
    my ($name, $code) = @_;
    my $p = run_perl($code);
    my $c = run_cl($code);
    is($c, $p, $name) or diag "perl=[$p] pcl=[$c]";
}

# ── 1. the READ side: which separators are defined out of the box ──────────
# `$/`, `$;` and `$!` must stay DEFINED — the fix is not "make them all undef".
both_agree('#465 $\\ and $, are undef, $/ $; $! $" are defined (perl oracle)', <<'PL');
printf("def: ors=%s ofs=%s irs=%s subsep=%s errno=%s listsep=%s\n",
       map { defined($_) ? "d" : "u" } ($\, $,, $/, $;, $!, $"));
printf("guards: or=%s dslash-ors=%s dslash-ofs=%s\n",
       (($\ || $,) ? "T" : "F"), ($\ // "FALLBACK"), ($, // "FALLBACK"));
printf("len: ors=%s ofs=%s eq-empty=%s\n",
       (defined($\) ? length($\) : "undef"),
       (defined($,) ? length($,) : "undef"),
       ((defined($\) && $\ eq "") ? "T" : "F"));
my @seen;
push @seen, "ORS" if defined $\;
push @seen, "OFS" if defined $,;
printf("seen: [%s]\n", join(",", @seen));
PL

# ── 2. the WRITE side must be untouched ────────────────────────────────────
# print's readers test "a non-empty string", so an undef separator flows
# through them exactly as the empty string did.  say uses "\n" INSTEAD of `$\`
# (perldoc -f say) and printf never appends it.
both_agree('#465 the write path is unchanged: $, between args, $\\ after', <<'PL');
$, = ":";  print 1, 2, 3;  print "\n";
$, = undef; print 4, 5, 6; print "\n";
$\ = "!\n"; print "x";
$\ = undef; print "y\n";
$\ = "[Z]"; printf("%s", "pf"); print "\n";
$\ = undef; $, = undef;
print "j:", join(",", 1, 2), "\n";
my @a = (1,2,3);
print "interp:[@a]\n";
PL

# ── 3. local, both spellings, and the restore ──────────────────────────────
# `local $\;` with no initialiser must read UNDEF inside the block, and the
# outer value must come back — including when the outer value is the
# pristine undef this task installs.
both_agree('#465 local $\\ / local $, — bare, assigned, and restored', <<'PL');
{ local $\ = "<END>\n"; print "in-block"; }
print "after-block\n";
{ local $, = "-"; print "a", "b"; print "\n"; }
print "c", "d"; print "\n";
{ local $\; print "bare-ors:", (defined($\) ? "d" : "u"), "\n"; }
{ local $,; print "bare-ofs:", (defined($,) ? "d" : "u"), "\n"; }
sub outer { local $\ = "!"; return inner() }
sub inner { print "dyn"; return 1 }
outer(); print "\n";
print "restored:", (defined($\) ? "d" : "u"), (defined($,) ? "d" : "u"), "\n";
PL

# ── 4. the shape that found it, without the statement modifier ─────────────
# (`local(…) = (…) if COND;` is a separate census drop, task #464 — the guard
# spells the same test as an if BLOCK so it exercises THIS bug and not that
# one.)  Also the `//` default idiom, which reads the wrong way round when the
# separator is a defined empty string.
both_agree('#465 the Test2::Formatter::TAP guard takes perl\'s branch', <<'PL');
sub tapish { if ($\ || $,) { return "RESET" } return "NOOP" }
sub defaulty { return ($, // ",") . "|" . ($\ // "\n") }
print "tapish=", tapish(), "\n";
printf("defaulty=%s", defaulty());
$, = ""; $\ = "";
print "after-set: tapish=", tapish(), " def=",
      (defined($\) ? "d" : "u"), (defined($,) ? "d" : "u"), "\n";
PL

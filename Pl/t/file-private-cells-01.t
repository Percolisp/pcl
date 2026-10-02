#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# file-private-cells-01.t - a promoted file lexical belongs to its OWN
# compilation unit (task #2633).
#
# A file-level `my` that a named sub (or a BEGIN block) captures is promoted to
# a package CELL.  The program names that cell by the lexical's own name; every
# other unit -- a string eval, a `do FILE` (eval mode), a `require`d file or a
# module (module mode) -- shares its packages with the program and with the
# other units, so its cell takes the `$name__file__N` mangle with a number
# derived from a digest of the unit's text.  Before that, a do-file's
# `my $file; BEGIN { $file = shift }` WAS the caller's `$main::file` -- which,
# with the caller's foreach aliasing a literal, aborted perl's eight
# t/re/regexp_*.t wrappers ("Modification of a read-only value") -- and two
# string evals declaring `my $n` shared one variable.
#
# Every fixture is written to a temp dir at run time; each unit kind is ONE
# program printing labelled lines (one SBCL launch per kind, the file's wall
# time is the constraint), and every expectation is perl 5.40.3's own output.

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

plan skip_all => "pl2cl not found" if !-x $pl2cl;
plan skip_all => "sbcl not found"  if !`which sbcl 2>/dev/null`;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub write_file {
    my ($dir, $name, $body) = @_;
    open my $fh, '>', "$dir/$name" or die "$dir/$name: $!";
    print $fh $body;
    close $fh;
}

# One launch, one row per expected line (labelled by the line's own text, so
# a failing row names what diverged).
sub check_lines {
    my ($kind, $code, @want) = @_;
    my @got = split /\n/, run_cl($code);
    for my $i (0 .. $#want) {
        is($got[$i] // '(missing)', $want[$i], "$kind: $want[$i]");
    }
    is(scalar(@got), scalar(@want), "$kind: no extra output")
        or diag(join "\n", @got[@want .. $#got]);
}

my $D = tempdir(CLEANUP => 1);

# ---- EVAL-MODE units: `do FILE` and `eval STRING` --------------------------
write_file($D, 'v2.pl', <<'PL');
my $file; BEGIN { $file = 5; } print "v2 f=$file\n"; 1;
PL
# t/re/regexp.t's head, which the wrappers `do` from inside a foreach whose
# GLOBAL loop variable $file aliases a LITERAL.
write_file($D, 'b2.pl', <<'PL');
my ($file, $iters);
BEGIN { $iters = shift || 1; $file = shift; }
print "b2 body file=", defined $file ? $file : "undef", " iters=$iters\n";
1;
PL
# Breaking cases for the renamed cells: interpolation (scalar, array, hash
# element, $#), a heredoc, a regex, a same-named `my` shadow in a sub.
write_file($D, 'i1.pl', <<'PL');
my $x = "X"; my @a = (1, 2); my %h = (k => "v");
sub f1 { "$x @a $h{k} " . scalar(@a) . ":$a[1]:$#a" }
my $p = "ab";
sub f3 { my $s = <<"E";
hd $p
E
  chomp $s; $s . ("xaby" =~ /$p/ ? "/m" : "/n") }
my $y = 1; sub f8 { my $y = 2; $y } sub g8 { $y }
print "i1 f1=", f1(), " f3=", f3(), " f8=", f8(), " g8=", g8(), "\n";
1;
PL
write_file($D, 'l1.pl', <<'PL');
my $q = "lex"; sub gq { $q } print "l1 gq=", gq(), " main::q=$main::q\n"; 1;
PL

check_lines('eval-mode', <<"PL",
chdir "$D" or die;
\$file = "outer";
do "./v2.pl" or die \$@;
print "outer file=\$file\\n";
for \$file ('./b2.pl', './nope.pl') {
    if (-r \$file) { do \$file or die \$@; print "after wrapper: \$file\\n"; last }
}
\$x = "gx"; \@a = (9); \%h = (k => "g"); \$p = "gp"; \$y = "gy";
do "./i1.pl" or die \$@;
print "main: \$x \@a \$h{k} \$p \$y\\n";
our \$q = "g";
{ local \$q = "L"; do "./l1.pl" or die \$@; print "in local: \$q\\n"; }
print "after local: \$q\\n";
eval q{ my \$n = 1; sub ea { \$n } 1 } or die \$@;
eval q{ my \$n = 2; sub eb { \$n } 1 } or die \$@;
print "ea=", ea(), " eb=", eb(), "\\n";
our \$g = "global";
eval q{ my \$g = "lex"; sub eg { \$g } 1 } or die \$@;
print "g=\$g eg=", eg(), "\\n";
\$v = "gv";
eval q{ my \$v = "e1"; sub s11a { \$v } 1 } or die \$@;
eval q{ my \$v = "e1"; sub s11b { \$v } 1 } or die \$@;
print "s11a=", s11a(), " s11b=", s11b(), " v=\$v\\n";
PL
    'v2 f=5',
    'outer file=outer',
    'b2 body file=undef iters=1',
    'after wrapper: ./b2.pl',
    'i1 f1=X 1 2 v 2:2:1 f3=hd ab/m f8=2 g8=1',
    'main: gx 9 g gp gy',
    'l1 gq=lex main::q=L',
    'in local: L',
    'after local: g',
    'ea=1 eb=2',
    'g=global eg=lex',
    's11a=e1 s11b=e1 v=gv',
);

done_testing();

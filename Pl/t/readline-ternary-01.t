#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# readline-ternary-01.t — task #479: the CASCADE of PPI's `<FH>` mis-lex.
#
# docs/ppi-upstream-bugs.md §14: in TERM position PPI 1.291 reads `<$f>` as
# `Operator(<) Symbol($f) Operator(>)`.  PCL has lived with that since s404
# because Pl::PExpr::_fix_ppi_glob_after_block rebuilds the token — but the
# rebuild cannot reach the CONSEQUENCE: with `>` taken for an operator, the
# next `/` is in term position too, so PPI reads it as a MATCH:
#
#     $ok ? <$f> // "" : ""    ->  … Operator(>) Regexp::Match(//) …
#     $ok ? <$f> / 2 : ""      ->  … Operator(>) Regexp::Match(/ 2 : "";)
#
# The first dropped the statement (#138); the second swallowed the REST of the
# file up to the next `;`.  Pl::Parser2::_repair_readline_cascade rewrites the
# diamond as the `readline(...)` perlop says it is and reparses.
#
# WHY IT IS WORTH A FILE: the shape occurs in ZERO sites of all four in-repo
# populations (measured s446k over 1329 files), so nothing in the sweep, the
# corpus or the companion suite can guard it — these rows are the guard.  It is
# not hypothetical: perl-tests/t/test.pl's runperl_and_capture carried two of
# them, re-transpiled by every one of the 108 sweep files, and it was the FIRST
# thing the #472 child-drop instrument found.
#
# The negatives are as load-bearing as the positives: the repair's condition is
# the family's negative (`_ends_term` — the `<` must be in TERM position) plus
# the cascade itself (a Regexp token must follow the `>`), so a genuine
# comparison chain and a genuine match must come through untouched.  Every
# expectation below is the live `perl` answer.

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

plan tests => 12;

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

# Both sides of every row: PCL's answer must be PERL's answer, so a future
# change to either can only agree or fail.
sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# Each snippet makes its own data file, so perl's run and PCL's run never
# share one; $$ differs between them.
my $PRELUDE = <<'PL';
my $F = "/tmp/pcl-readline-ternary-$$.dat";
open(my $w, '>', $F) or die "open: $!"; print $w "hello\nsecond\n"; close $w;
my $ok = 1;
PL
my $EPILOGUE = qq{\nunlink "/tmp/pcl-readline-ternary-\$\$.dat";\n};

sub agree_with_file {
    my ($body, $desc) = @_;
    both_agree($PRELUDE . $body . $EPILOGUE, $desc);
}

# ---- the bug ---------------------------------------------------------------

agree_with_file(<<'PL', 'the #479 shape: COND ? <$fh> // "" : ""');
local $/; open(my $f, '<', $F) or die;
my $so = $ok ? <$f> // "" : "";
print "[$so]\n";
PL

agree_with_file(<<'PL', 'the false branch does not read the handle');
local $/; open(my $f, '<', $F) or die;
my $so = 0 ? <$f> // "" : "X";
print "[$so]", (defined(<$f>) ? "unread" : "eof-or-unread"), "\n";
PL

agree_with_file(<<'PL', 'an undef handle: // supplies the default');
my $g;
my $so = $ok ? <$g> // "Z" : "";
print "[$so]\n";
PL

agree_with_file(<<'PL', 'the swallowing spelling: `<$f> / 2` ate the rest of the file');
open(my $f, '<', $F) or die; my $line = <$f>; close $f;
open(my $n, '>', "$F.n") or die; print $n "8\n"; close $n;
open(my $h, '<', "$F.n") or die;
my $q = $ok ? <$h> / 2 : 0;
print "[$q][$line]";
unlink "$F.n";
PL

agree_with_file(<<'PL', 'a BAREWORD handle in the same shape');
local $/; open(FH, '<', $F) or die;
my $so = $ok ? <FH> // "" : "";
close FH;
print "[$so]\n";
PL

# A `(...)` rewrite of the diamond would have turned this into perl's
# "print (...) interpreted as function" gotcha — hence readline(), not parens.
agree_with_file(<<'PL', 'inside a print argument list');
local $/; open(my $f, '<', $F) or die;
print "[", ($ok ? <$f> // "" : ""), "]\n";
PL

# ---- the neighbours that ALREADY lowered and must keep lowering ------------

agree_with_file(<<'PL', 'neighbour: no // — a plain readline branch');
local $/; open(my $f, '<', $F) or die;
my $so = $ok ? <$f> : "";
print "[$so]\n";
PL

agree_with_file(<<'PL', 'neighbour: readline($f) // "" spelled out');
local $/; open(my $f, '<', $F) or die;
my $so = $ok ? readline($f) // "" : "";
print "[$so]\n";
PL

agree_with_file(<<'PL', 'neighbour: <$f> // "" with no ternary');
local $/; open(my $f, '<', $F) or die;
my $so = <$f> // "";
print "[$so]\n";
PL

# ---- the negatives: `<` that is really less-than ---------------------------

both_agree('my ($a1,$b1,$c1) = (1,5,3); print (($a1 < $b1) ? 1 : 0), print (($b1 > $c1) ? 1 : 0), print "\n";',
           'negative: two genuine comparisons keep their meaning');

both_agree('my $s = "abc"; my $n = 2; print(($n > 1 && $s =~ /b/) ? "y" : "n"); print "\n";',
           'negative: a real match after a `>` comparison');

both_agree('my $q = 10; my $ok = 1; print $ok ? $q / 2 : 0; print "\n";',
           'negative: division after an ordinary term');

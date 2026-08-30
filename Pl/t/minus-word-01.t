#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# minus-word-01.t — task #457: `)-name` with NO SPACE.
#
# PPI 1.291 glues the `-` to the identifier and hands PCL one
# `PPI::Token::Word('-length')` — the negative-bareword string — where perl
# reads binary minus, because after `)` a term has ENDED.  PExpr has no case
# for `Word(-name) List`, so the WHOLE STATEMENT was dropped (#138 family).
# Upstream report: docs/ppi-upstream-bugs.md §25 + docs/ppi-bug-report.t.
#
# WHY IT IS WORTH A FILE: the shape occurs in ZERO files of the four in-repo
# populations and TWICE in one board dist, Text-Balanced-2.07-0 (lines 118 and
# 397).  Line 118 sits in `gen_delimited_pat`, which that module's own top
# level CALLS at line 308 — so after the s435 announce→DIE flip
# `use Text::Balanced` DIED and the dist went from 958 passing rows to zero.
# Nothing in the repo populations can guard it, so these rows are the guard.
#
# THE CONDITION IS A NEGATIVE, and that is what makes the repair safe: perl's
# `-bareword` string form can only start where a TERM can, so every legitimate
# spelling follows `(`, `{` or `,` — none of which ends a term.  The negatives
# below are as load-bearing as the positives; each expectation is the live
# `perl` answer.

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

plan tests => 20;

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

# ---- the bug ---------------------------------------------------------------

both_agree('my $z = length("abc")-length("a"); print "$z\n";',
           'a call, `)-`, a call: binary minus');

both_agree('my @a = (10,20); print $a[1]-length("ab"), "\n";',
           'a subscript `]` ends a term too');

both_agree('my %h = (a => 9); print $h{a}-length("abc"), "\n";',
           'a subscript `}` ends a term');

# Text::Balanced line 118 and line 397, standalone.
both_agree(<<'PL', 'Text::Balanced:118 — .= with a `)-` inside a repetition count');
my ($dels, $escs) = ('()[]', '\\');
$escs .= substr($escs,-1) x (length($dels)-length($escs));
print "[$escs]\n";
PL

both_agree(<<'PL', 'Text::Balanced:397 — pos()-length()');
my $text = "hello <tag> world"; pos($text) = 11;
my $closetagpos = pos($text)-length("<tag>");
print "$closetagpos\n";
PL

# ---- the negatives: `-bareword` where a term has NOT ended -----------------

both_agree('my %h = (-foo => 1, -bar => 2); print join(",", map { "$_=$h{$_}" } sort keys %h), "\n";',
           'negative: `(-foo =>` after `(` is the string "-foo"');

both_agree('sub tag { return "[@_]" } print tag(-baz), "\n";',
           'negative: `foo(-bar)` passes the string');

both_agree('my %o; $o{-x} = 3; print $o{-x}, "\n";',
           'negative: `$h{-x}` is a hash key, not a subtraction');

both_agree('sub f { "F" } my @l = (1, -bar, 2); print scalar(@l), "$l[1]\n";',
           'negative: after a comma, `-bar` is the string');

both_agree('print "x" . -foo . "y", "\n";',
           'negative: after a `.` operator, `-foo` is the string');

# ---- task #480: `$x.2` — the SAME rule, on the CONCATENATION spelling -------
#
# PPI 1.291 absorbs the `.` into a Number::Float, so there is no operator token
# at all and the statement was dropped ("Missing case: [Symbol, Number::Float]").
# Upstream report: docs/ppi-upstream-bugs.md §28 + docs/ppi-bug-report.t bug 28.
# The one census site is Text-CSV-2.04/t/78_fragment.t:101.

both_agree('local $_ = "a"; my $x = $_.2; print "$x\n";',
           '#480: `$_.2` is a concatenation');

both_agree('my @a = (7,8); my $w = $a[1].3; print "$w\n";',
           '#480: `].3` is a concatenation');

both_agree('my %h = (k => "v"); my $v = $h{k}.4; print "$v\n";',
           '#480: a subscript `}` then `.4` is a concatenation');

both_agree('sub f { "F" } my $u = f().6; print "$u\n";',
           '#480: `).6` is a concatenation');

both_agree('my @a = (1,2,3); my $t = $#a.7; print "$t\n";',
           '#480: `$#a.7` is a concatenation');

# ---- the negatives: a TERM is expected, so `.5` really is one half ----------

both_agree('my $n = .5; print "$n\n";',
           'negative: `= .5` is the number 0.5');

both_agree('my @l = (1, .5); print "@l\n";',
           'negative: after a comma, `.5` is the number');

both_agree('my %g = (a => .5); print "$g{a}\n";',
           'negative: after a fat comma, `.5` is the number');

# The ONE position the shape oracle cannot see: a print FILEHANDLE looks like a
# completed term and is not one — perl is still waiting for the argument list,
# so `.5` there is the NUMBER (task #405's family; the guard is
# Pl::Parser2::_is_print_filehandle_slot).
both_agree('my $x = "A"; print $x .5, "\n"; print "done\n";',
           'negative: `print $x .5` treats $x as the FILEHANDLE, .5 as a number');

both_agree('print STDOUT .5, "\n";',
           'negative: `print STDOUT .5` writes 0.5');

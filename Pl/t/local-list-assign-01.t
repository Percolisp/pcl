#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# local-list-assign-01.t — `local(LIST) = LIST` is a LIST assignment (#1243 (a)).
#
# perl reads list-vs-scalar assignment off the PARENTHESES alone, exactly as it
# does for `my`: `local $x = @_` is a scalar assignment (the argument COUNT) and
# `local($x) = @_` is a list assignment (the first argument).  PCL built its own
# target list in Pl::Parser::_process_local_declaration and forgot the parens,
# so every single-scalar `local($x) = LIST` — the classic pre-`my` idiom, still
# everywhere in old code — stored the count.  Two targets already worked, which
# is why it went unnoticed: `local($x,$y) = @_` took the list path all along.
#
# EVERY expectation below is perl 5.40.3, probed s473a.  Rows 1 and 3 FAIL on
# the 41ca2496 extraction (H=1 and X=30 there); the rest pass on both trees and
# are here so the fix cannot be bought with a regression elsewhere.
#
# NOT covered, deliberately: the SUBSCRIPTED spelling `local($a[1]) = (7,8)`,
# which is still a scalar assignment (8, not 7) and still runs its RHS in
# scalar context — task #1339 carries the measurement that stopped it being
# fixed with this one (it costs the p-local-*-elem-init fast path).

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

plan tests => 14;

# One transpile + one SBCL run for the whole family; one `is` per line.
my $fixture = <<'PL';
our ($H, $X, $Y, @A, %Hh, $R);
sub f  { local($H) = @_;            return "H=$H" }
sub g  { local($X, $Y) = @_;        return "X=$X Y=$Y" }
sub lst { return (10,20,30) }
sub h  { local($X) = lst();         return "X=$X" }
sub sc { local $H = @_;             return "H=$H" }
sub ar { local(@A) = (1,2);         return "A=@A" }
sub hs { local(%Hh) = (k=>1);       return "Hh=$Hh{k}" }
sub my1 { my($H) = @_;              return "my=$H" }
sub r  { local($R) = ("inner");     return "in=$R" }
sub cnt { my $n = (local($X) = (7,8,9)); return "n=$n" }
sub nolist { local $X = (4,5,6);    return "X=$X" }
$R = "outer";
print "1 ", f("beach"), "\n";
print "2 ", g("a","b"), "\n";
print "3 ", h(), "\n";
print "4 ", sc("p","q"), "\n";
print "5 ", ar(), "\n";
print "6 ", hs(), "\n";
print "7 ", my1("zzz"), "\n";
print "8 ", r(), " after=$R\n";
print "9 ", cnt(), "\n";
print "10 ", nolist(), "\n";
our @B;
sub ret  { local(@B) = (1,2) }
sub ret2 { local @B = (1,2) }
my @r1 = ret();  print "11 list=@r1\n";
my $n1 = ret();  print "12 scalar=$n1\n";
my @r2 = ret2(); print "13 list=@r2\n";
my $n2 = ret2(); print "14 scalar=$n2\n";
our @bee = (5,6);
sub sw { local(@bee) = reverse @bee; return "@bee" }
print "15 ", sw(), " after=@bee\n";
sub two { local($X, $Y) = (1); return "X=$X Y=", (defined $Y ? $Y : "undef") }
print "16 ", two(), "\n";
PL

my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
print $fh $fixture;
close $fh;
my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $cl_fh $cl_code;
close $cl_fh;
my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
$out =~ s/^;.*\n//gm;
$out =~ s/^PCL Runtime loaded\n//gm;
$out =~ s/^\s*\n//gm;

my %got;
$got{$1} = $2 while $out =~ /^(\d+) (.*)$/mg;

sub line_is {
    my ($key, $want, $name) = @_;
    is($got{$key} // "(missing; full output: $out)", $want, $name);
}

# ── the bug: ONE parenthesised scalar target is a LIST assignment ────────────
line_is(1, 'H=beach',
        'local($H) = @_ assigns the first ARGUMENT, not the count');
line_is(3, 'X=10',
        'local($X) = f() calls f in LIST context and takes the first element');

# ── the sibling shapes that already worked and must keep working ─────────────
line_is(2,  'X=a Y=b',   'local($X,$Y) = @_ (two targets) still a list assignment');
line_is(4,  'H=2',       'local $H = @_ WITHOUT parens is still the count');
line_is(5,  'A=1 2',     'local(@A) = (1,2) — an array lvalue is list context either way');
line_is(6,  'Hh=1',      'local(%Hh) = (k=>1)');
line_is(7,  'my=zzz',    'my($H) = @_ — the sibling that was always right');
line_is(8,  'in=inner after=outer', 'the localization is still restored at scope exit');
line_is(9,  'n=3',       'the list assignment yields the RHS count in scalar context');
line_is(10, 'X=6',       'local $X = (4,5,6) WITHOUT parens is the comma operator');
line_is(16, 'X=1 Y=undef', 'a short RHS leaves the extra target undef');

# ── the value of `local(@a) = LIST` as a sub's last statement ────────────────
line_is(11, 'list=1 2', 'local(@B) = (1,2) as the last statement returns the list');
line_is(12, 'scalar=2', '…and its count in scalar context');
line_is(15, '6 5 after=5 6',
        'local(@bee) = reverse @bee evaluates the RHS against the OLD @bee');

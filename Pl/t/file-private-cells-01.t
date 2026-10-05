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

# ---- MODULE-MODE units: `require "PATH"` and `use Module` -----------------
write_file($D, 'm2.pl', <<'PL');
my $count = 0;
sub bump { $count++ }
sub b { $count }
1;
PL
# Two modules reopening ONE package, each with its own `my $n`.
write_file($D, 'ShA.pm', <<'PL');
package Shared; my $n = "A"; sub ga { $n } 1;
PL
write_file($D, 'ShB.pm', <<'PL');
package Shared; my $n = "B"; sub gb { $n } 1;
PL
# The module's file lexical is ALSO spelled `$ModC::v` by its USER: two
# variables in perl.
write_file($D, 'ModC.pm', <<'PL');
package ModC; my $v = "lex"; sub gv { $v } 1;
PL
# A module compiled into the PROGRAM's package, with the program's own
# captured file lexical of the same name.
write_file($D, 'ModF.pm', <<'PL');
package main; my $count2 = 7; sub fcount { $count2 } 1;
PL

check_lines('module-mode', <<"PL",
use lib "$D";
my \$count = 10;
sub a { \$count }
require "$D/m2.pl";
bump(); bump();
print "a=", a(), " b=", b(), "\\n";
use ShA; use ShB;
print "ga=", Shared::ga(), " gb=", Shared::gb(), "\\n";
use ModC;
\$ModC::v = "user";
print "gv=", ModC::gv(), " v=\$ModC::v\\n";
my \$count2 = 1; sub mc { \$count2 }
use ModF;
\$main::count2 = "g";
print "mc=", mc(), " f=", fcount(), " g=\$main::count2\\n";
PL
    'a=10 b=2',
    'ga=A gb=B',
    'gv=lex v=user',
    'mc=1 f=7 g=g',
);

# ---- The unit BASE's width (Fable ruling s505/s506) ------------------------
# The PROPERTY, not the digits: two different unit texts draw different bases,
# one text always draws the same names (the eval / module caches are keyed by
# text), and on a 64-bit IV the base carries ~52 digest bits -- more than 13
# decimal digits -- because a 30-bit base gives thousands of generated eval
# texts (Sub::Quote, Moo) a real chance of a silently shared cell.  No SBCL.
{
    local @INC = ($project_root, @INC);
    require Pl::Parser2;
    my $cells = sub {
        my $cl = Pl::Parser2->parse_code($_[0], eval_mode => 1,
                                         eval_pkg => 'main');
        my %u;
        return join ',', grep { !$u{$_}++ } $cl =~ /(\$n__file__\d+)/g;
    };
    my ($c1, $c2, $c1b) = map { $cells->($_) }
        q{my $n = 1; sub wa { $n } 1}, q{my $n = 2; sub wb { $n } 1},
        q{my $n = 1; sub wa { $n } 1};
    my ($digits) = $c1 =~ /__file__(\d+)$/;
    my $wide = ~0 > 0xFFFFFFFF;
    ok($c1 ne '' && $c2 ne '' && $c1 ne $c2 && $c1 eq $c1b
         && (!$wide || length($digits // '') > 13),
       'unit base: distinct texts -> distinct cells, same text -> same names, '
         . '> 13 digits on a 64-bit IV')
        or diag("c1=$c1 c2=$c2 c1b=$c1b");
}

# ---- s508a (#2645): the BRACED spelling `${x}` / `@{x}` / `%{x}` / `$#{x}` /
# `${x}[i]` / `${x}{k}` of a file lexical inside a NAMED sub, in code and in a
# string, beside a same-named package variable; a block lexical; a sub's own
# shadow.  The capture promotion used to refuse the spelling and the sub read
# the PACKAGE variable.  Program mode, then the same text as a `do` file.
# INVERSE: main fee16466 printed `01 G-G` / `02 G|G|1|GA||GH|0` / `07 ` (empty).
my $BRACE = <<'PL';
$main::x = "G"; @main::a = ("GA"); %main::h = (k => "GH");
my $x = "X";
my @a = (1, 2, 3);
my %h = (k => "HV");
sub f2 { "${x}-" . ${x} }
sub f3 { "${ x }|" . ${ x } . "|" . scalar(@{a}) . "|" . join(",", @{a}) . "|" . ${a}[1] . "|" . ${h}{k} . "|" . $#{a} }
sub f4 { my @k = keys %{h}; "@k|@{a}[0,1]|@{h}{k}|" . join(",", @{a}[0, 1]) . "|" . join(",", @{h}{k}) }
sub f5 { ${x} = "Y"; ${a}[0] = 9; ${h}{k} = "NV"; "set" }
my $anon = sub { "${x}+" . ${x} };
print "01 ", f2(), "\n";
print "02 ", f3(), "\n";
print "03 ", f4(), "\n";
print "04 ", $anon->(), "\n";
print "05 ", f5(), " $x $a[0] $h{k} / $main::x $main::a[0] $main::h{k}\n";
print "06 ", f2(), " ", f3(), "\n";
{ my $y = "BY"; sub g1 { "${y}" . ${ y } } print "07 ", g1(), "\n"; }
my $z = "Z"; sub g2 { my $z = "inner"; "${z}" . ${z} } print "08 ", g2(), " $z\n";
PL
my @BRACE_WANT = ('01 X-X', '02 X|X|3|1,2,3|2|HV|2', '03 k|1 2 3[0,1]|{k}|1,2|HV',
                  '04 X+X', '05 set Y 9 NV / G GA GH', '06 Y-Y Y|Y|3|9,2,3|2|NV|2',
                  '07 BYBY', '08 innerinner Z');
check_lines('braced capture', $BRACE, @BRACE_WANT);
write_file($D, 'br.pl', $BRACE . "1;\n");
check_lines('braced capture, do FILE', qq{chdir "$D" or die;\ndo "./br.pl" or die \$@;\n},
            @BRACE_WANT);

done_testing();

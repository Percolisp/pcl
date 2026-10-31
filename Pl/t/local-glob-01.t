#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# local-glob-01.t — `local *foo = RHS` typeglob localization, RHS eval order,
# and the deprecated conditional form `local *foo = RHS if COND`.
#
# Two bugs fixed (session 262):
#  1. RHS eval order: localizing *_ clears ALL slots of `_` (including @_), so an
#     RHS that reads @_ (e.g. local *_ = \join('', @_)) must be evaluated BEFORE
#     the slots are cleared.  PCL now binds the RHS in a wrapping let.
#  2. Conditional form: `local *_ = RHS if @_` (Text::ParseWords::old_shellwords).
#     When COND is false Perl does NOT localize at all (the rest of the scope sees
#     the outer slots); when true it localizes+assigns.  p-local-glob-if handles
#     both, evaluating RHS while the slots are still intact.

use v5.30;
use strict;
use warnings;

use Test::More;
use File::Temp qw(tempfile);

use lib ".";
use Pl::Parser2;
use FindBin;
use lib "$FindBin::Bin";
use PCLCore;

# The sbcl command line comes from the ONE builder every runner shares
# (tools/lib/PCLSbcl.pm via PCLCore::sbcl_prefix, task #344): the saved core
# with the runtime already compiled in, and the 512 MB control stack.  This
# file used to spell `sbcl --noinform --non-interactive --load
# cl/pcl-runtime.lisp` itself, which recompiles the whole runtime on EVERY row
# -- 2.89 CPU-s a row against 0.007 s from the core (measured s473u, #1544) --
# and ran on the default 2 MB stack, which is exactly the drift #344 exists to
# stop.
my @sbcl_rt = PCLCore::sbcl_prefix("$FindBin::Bin/../../cl/pcl-runtime.lisp");

sub run_pl {
    my $code = shift;
        my $cl_code = Pl::Parser2->parse_code($code);
    my ($fh, $filename) = tempfile(SUFFIX => '.lisp');
    print $fh $cl_code;
    close $fh;
    my $output = `sbcl @sbcl_rt --load "$filename" 2>&1`;
    unlink $filename;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^\s*\n//gm;
    $output =~ s/PCL Runtime loaded\n?//g;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s+//;
    return $output;
}

plan tests => 17;

# 1. RHS reads @_, which *_ localization would otherwise clear first.
is(run_pl(<<'PL'), "foobar\n", 'local *_ = \join("",@_) sees old @_');
sub t { local *_ = \join('', @_); print "$_\n"; }
t("foo","bar");
PL

# 2. \$_[0] as the RHS (also reads @_).
is(run_pl(<<'PL'), "foo\n", 'local *_ = \$_[0] sees old @_');
sub t { local *_ = \$_[0]; print "$_\n"; }
t("foo","bar");
PL

# 3. \"@_" interpolation RHS.
is(run_pl(<<'PL'), "a b\n", 'local *_ = \"@_" sees old @_');
sub t { local *_ = \"@_"; print "$_\n"; }
t("a","b");
PL

# 4. Plain literal RHS still works (no @_ involved).
is(run_pl(<<'PL'), "hi\n", 'local *_ = \"literal" still works');
sub t { local *_ = \"hi"; print "$_\n"; }
t();
PL

# 5. Conditional, COND true: localize + assign.
is(run_pl(<<'PL'), "Z\n", 'local *_ = RHS if COND (true) localizes');
sub t { local *_ = \"Z" if $_[0]; print "$_\n"; }
$_ = "outer";
t(1);
PL

# 6. Conditional, COND false: do NOT localize — rest of scope sees outer $_.
is(run_pl(<<'PL'), "outer\n", 'local *_ = RHS if COND (false) keeps outer $_');
sub t { local *_ = \"Z" if $_[0]; print "$_\n"; }
$_ = "outer";
t(0);
PL

# 7. The old_shellwords idiom: local *_ = \join('', @_) if @_.
is(run_pl(<<'PL'), "foobar|done\n", 'conditional local *_ = \join if @_ (with args)');
sub t {
    local *_ = \join('', @_) if @_;
    print "$_|done\n";
}
t("foo","bar");
PL

# 8. Same idiom, no args: falls through to the caller's $_.
is(run_pl(<<'PL'), "preset|done\n", 'conditional local *_ if @_ (no args) uses outer $_');
sub t {
    local *_ = \join('', @_) if @_;
    print "$_|done\n";
}
$_ = "preset";
t();
PL

# ── A glob STRINGIFIES to its perl spelling (#316, s395) ────────────────────
# Single-segment package names are upcased into CL packages and a glob's name
# is stored case-INVERTED (the symbol spelling), so both halves came back
# upcased: `print *plain` gave `*MAIN::PLAIN` where perl gives `*main::plain`.
# %pcl-invert-case is its own inverse, so one call restores each half — except
# for the package, where the inversion is applied on the way in only to names
# WITHOUT "::", so an all-lowercase multi-segment package must be left alone.
# All five expectations are the live perl answers.
is(run_pl(<<'PL'), "*main::plain\n*Foo::bar\n*main::STDOUT\n", 'globs stringify in perl case');
print *plain, "\n";
print *Foo::bar, "\n";
print *STDOUT, "\n";
PL

is(run_pl(<<'PL'), "*version::regex::thing\n", 'an all-lowercase multi-segment package is not upcased');
package version::regex;
sub q1 { return *thing }
print q1(), "\n";
PL

is(run_pl(<<'PL'), "STDOUT|main\n", '*FOO{NAME} and {PACKAGE} still answer the same way');
print *STDOUT{NAME}, "|", *STDOUT{PACKAGE}, "\n";
PL

# ── `local *FH` gives the scope a FRESH glob, and a glob VALUE taken in the
# scope keeps it (#3868, s529a) ─────────────────────────────────────────────
# A PCL typeglob is a NAME, so every `*FH` named main::FH and two handles opened
# through `do { local *FH; *FH }` shared one stream (Log::Log4perl's
# Appender::File wrote every appender into one file).  The scope's glob values
# are now handed the scope's slots and IO at the restore; a REFERENCE `\*FH`
# taken in the scope is a ref to the GV and follows the NAME; the IO slot is
# localized too, and a handle the scope opened is closed at its end unless a
# glob value kept it.  Every expected line is perl 5.40.3's output.
my $lg_dir = ($ENV{TMPDIR} || "/tmp") . "/pcl-local-glob-$$";
mkdir $lg_dir;
# the programs name their scratch files __DIR__; rd() reads one back as a line
sub lg { my $s = shift; $s =~ s/__DIR__/$lg_dir/g;
         'sub rd { open my $r, "<", $_[0] or return "none"; local $/; my $c = <$r>; $c =~ s/\n/;/g; $c }' . "\n" . $s }
# These rows run a whole PROGRAM (pl2cl), not a fragment: run_pl's
# parse_code emits into package PCL, where a bareword FH is PCL::FH while
# `local *FH` names main::FH -- two globs a real program never mixes.
sub run_file {
    my $code = shift;
    my ($fh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$FindBin::Bin/../../pl2cl $pl 2>/dev/null`;
    my ($cfh, $filename) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl_code;
    close $cfh;
    my $output = `sbcl @sbcl_rt --load "$filename" 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^\s*\n//gm;
    $output =~ s/PCL Runtime loaded\n?//g;
    return $output;
}

is(run_file(lg(<<'PL')), "1=[A;] 2=[B;]\nsame-name: eq *main::FH\n", 'do { local *FH; *FH } hands out DISTINCT handles');
my $d = "__DIR__/a";
sub mk { my $fh = do { local *FH; *FH; }; return $fh }
my $a = mk(); my $b = mk();
open $a, '>', "$d.1" or die; open $b, '>', "$d.2" or die;
print $a "A\n"; print $b "B\n"; close $a; close $b;
print "1=[", rd("$d.1"), "] 2=[", rd("$d.2"), "]\n";
print "same-name: ", ($a eq $b ? "eq" : "ne"), " ", "$a", "\n";
PL

is(run_file(lg(<<'PL')), "outer=[out;via-ref;] inner=[]\nio before==after: same inside==before: diff\nl1\nl1\nl2\n", 'the IO slot is localized; \*FH follows the name, return *FH keeps the scope glob');
my $d = "__DIR__/b";
open FH, '>', "$d.outer" or die;
sub pr { print FH $_[0] }
my ($io_before, $io_inside, $io_after, $ref);
$io_before = *FH{IO};
{ local *FH; open FH, '>', "$d.inner" or die; pr("in\n"); $io_inside = *FH{IO}; $ref = \*FH; }
pr("out\n");
$io_after = *FH{IO};
print {$ref} "via-ref\n";
close FH;
print "outer=[", rd("$d.outer"), "] inner=[", rd("$d.inner"), "]\n";
print "io before==after: ", ($io_before == $io_after ? "same" : "diff"), " inside==before: ", ($io_inside == $io_before ? "same" : "diff"), "\n";
sub openit { local *FH; open FH, '<', $_[0] or die; return *FH }
open my $w, '>', "$d.x" or die; print $w "l1\nl2\n"; close $w;
my $g1 = openit("$d.x"); my $g2 = openit("$d.x");
my $l1 = <$g1>; my $l2 = <$g2>; my $l3 = <$g1>;
print "$l1$l2$l3";
PL

is(run_file(lg(<<'PL')), "2000\nalias\nstdout alive\nboom|K;\n4.0 open\n4.1 closed\nC;\n", 'a scope-opened handle closes at the scope end; aliases and the outer handle survive');
my $d = "__DIR__/c";
open my $w, '>', "$d.y" or die; print $w "Y\n"; close $w;
sub slurp { local *FH; open FH, '<', $_[0] or die "slurp: $!"; local $/; my $c = <FH>; $c }
my $n = 0; for (1..2000) { $n++ if slurp("$d.y") eq "Y\n" }
print "$n\n";
{ local *FH = *STDOUT; print FH "alias\n"; }
print "stdout alive\n";
open FH, '>', "$d.k" or die;
eval { local *FH; open FH, '>', "$d.k2" or die; die "boom\n" };
print FH "K\n"; close FH;
chomp(my $e = $@); print "$e|", rd("$d.k"), "\n";
open FH, '>', "$d.c" or die;
for my $c (0, 1) { local *FH if $c; print "4.$c ", (defined fileno(FH) ? "open" : "closed"), "\n"; }
print FH "C\n"; close FH;
print rd("$d.c"), "\n";
PL

is(run_file(lg(<<'PL')), "o=[O;] i=[I;]\nsv hv *main::FH\nouter-undef code\n", 'nested scopes, and the scope glob carries its scalar / hash / code slots');
my $d = "__DIR__/d";
my ($o, $i);
{ local *FH; $o = *FH; { local *FH; $i = *FH; } }
open $o, '>', "$d.o" or die; open $i, '>', "$d.i" or die;
print $o "O\n"; print $i "I\n"; close $o; close $i;
print "o=[", rd("$d.o"), "] i=[", rd("$d.i"), "]\n";
sub mkh { local *FH; ${*FH} = "sv"; ${*FH}{k} = "hv"; return *FH }
my $h = mkh(); print ${*$h}, " ", ${*$h}{k}, " ", "$h", "\n";
sub mkc { local *FH; *FH = sub { "code" }; return *FH }
my $cg = mkc(); print((defined &FH ? "outer-defined" : "outer-undef"), " ", *{$cg}{CODE}->(), "\n");
PL

is(run_file(<<'PL'), "1 undef\n1b boo\n2 undef\n2b b\n3 undef\n3b boo\n4 9\n4b 5\n5 7\n5b 1\n6 G\n6b E\n7 Fx\n7b undef\n8 0\n8b 2\n9 undef\n9b U\n", 'local *NAME hides the scalar / array an enclosing local $NAME / @NAME set; the restore brings it back (#3963, t/op/for.t NULL GvSV)');
our $foo = "boo"; { local *foo; print "1 ", (defined $foo ? "def [$foo]" : "undef"), "\n"; } print "1b $foo\n";
local $bar = "b"; { local *bar; print "2 ", (defined $bar ? "def [$bar]" : "undef"), "\n"; $bar = "in"; } print "2b $bar\n";
{ local $baz = "boo"; { local *baz; for $baz (1,2) {} print "3 ", (defined $baz ? "def" : "undef"), "\n"; } print "3b $baz\n"; }
{ local $q = 5; { local *q; $q = 9; print "4 $q\n"; } print "4b $q\n"; }
{ local $w = 1; sub rw { $w } { local *w; $w = 7; print "5 ", rw(), "\n"; } print "5b ", rw(), "\n"; }
{ local $e = "E"; { local *e = \"G"; print "6 $e\n"; } print "6b $e\n"; }
{ local $f = "F"; $f .= "x"; print "7 $f\n"; } print "7b ", (defined $f ? $f : "undef"), "\n";
{ local @arr = (1,2); { local *arr; print "8 ", scalar(@arr), "\n"; } print "8b ", scalar(@arr), "\n"; }
{ local $t = "T"; my $r = \$t; { local *t; $$r = "U"; print "9 ", (defined $t ? $t : "undef"), "\n"; } print "9b $t\n"; }
PL

is(run_file(<<'PL'), "1 120 5\n2 L L\n3 E E\n4 G\n5 9 8\n6 0\n7 9 8\n8 1 2 3\n9 0\n10 j\n11 1\n11 2\n11 3\n12 undef\n13 2\n14 undef\n15 1\n16 4\n17 3\n", 'local $NAME bodies read the CELL: string eval, closures, local-if, %/@ globs (#3963)');
$foo = 5;
$fact = 'local($foo)=$foo; $foo <= 1 ? 1 : $foo-- * (eval $fact)';
print "1 ", eval($fact), " $foo\n";
our $g = "G"; sub show { $g } { local $g = "L"; print "2 ", show(), " ", eval('$g'), "\n"; eval 'local $g = "E"; print "3 ", show(), " $g\n"'; } print "4 $g\n";
our @a = (1,2,3); { local @a = (9); push @a, 8; print "5 @a\n"; { local *a; print "6 ", scalar(@a), "\n"; } print "7 @a\n"; } print "8 @a\n";
our %h = (k => 1); { local %h = (j => 2); { local *h; print "9 ", scalar(keys %h), "\n"; } print "10 ", join(",", keys %h), "\n"; }
our $c = 0; for my $i (1..3) { local $c = $c + $i; print "11 $c\n"; }
our $x = 1; { local $x = 2 if 1; { local *x; print "12 ", (defined $x ? $x : "undef"), "\n"; } print "13 $x\n"; }
our $y = 1; { local $y = 2 if 0; { local *y; print "14 ", (defined $y ? $y : "undef"), "\n"; } print "15 $y\n"; }
our $z = 1; { local $z = 3; my $f = sub { $z }; { local *z; $z = 4; print "16 ", $f->(), "\n"; } print "17 ", $f->(), "\n"; }
PL

system('rm', '-rf', $lg_dir);

# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ctx-bind-01.t -- the *wantarray* bind at a BUILT-IN call site (s510c).
#
# Since #2775 a built-in call is bound to its static context ONLY when the
# built-in is in Pl::ExprToCL's %WANTARRAY_SENSITIVE: every other built-in is
# emitted bare in every slot (the old list-only fallback bind is gone).  That
# makes the table load-bearing, so the eight built-ins it was missing (#2801:
# getpw*/getgr*, the CALL spellings glob(...) / readline(...)) must be in it.
#
# Two inverse groups, each with a perl-PROBED expected output (perl 5.40.3;
# nothing here needs > 5.38):
#   S-*  a SCALAR slot inside a list-context call's argument list -- FAILS on
#        main before s510c (the outer list bind leaked in: `ARRAY(0x1)`).
#   L-*  a LIST slot no enclosing macro binds -- FAILS when the fallback is
#        removed WITHOUT the table addition (each answers 1).
# The fixture directory makes every count independent of the machine.

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

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

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

# One SBCL run per group; one row per output line, keyed by its label.
sub rows_agree {
    my ($code, $probed, $what) = @_;
    my %got = map { /^(\S+) (.*)$/ ? ($1 => $2) : () } split /\n/, run_cl($code);
    for my $line (split /\n/, $probed) {
        my ($label, $want) = $line =~ /^(\S+) (.*)$/;
        is($got{$label}, $want, "$what: $label");
    }
}

my $D = tempdir(CLEANUP => 1);
for my $f (qw(a1 a2 a3)) { open my $t, '>', "$D/$f" or die; close $t }
{ open my $t, '>', "$D/lines.txt" or die; print $t "l$_\n" for 1 .. 5; close $t }

# ── #2775 + #2801: the eight built-ins the table was missing ──────────────
rows_agree(qq{my \$D = "$D";\n} . <<'PL', <<'PROBED', '#2801 table');
sub id { @_ }
sub cnt { scalar @_ }
sub fh { open my $h, "<", "$D/lines.txt" or die; $h }
sub bad { $_[0] =~ /^ARRAY/ ? "bad" : "ok" }
print "S-pwuid ",  bad(id("" . getpwuid 0)), "\n";
print "S-pwnam ",  bad(id("" . getpwnam "root")), "\n";
print "S-grgid ",  bad(id("" . getgrgid 0)), "\n";
print "S-glob ",   (id("" . glob "$D/a*"))[0] =~ s{.*/}{}r, "\n";
{ my $h = fh(); my ($v) = id("" . readline($h)); chomp $v; print "S-readline $v\n"; }
{ my $c = cnt(getpwuid 0); print "L-pwuid ", ($c > 1 ? "list" : "scalar"), "\n"; }
{ my $c = cnt(getgrgid 0); print "L-grgid $c\n"; }
{ my $c = cnt(glob "$D/a*"); print "L-glob $c\n"; }
{ my $h = fh(); my $c = cnt(readline($h)); print "L-readline $c\n"; }
my @p; push @p, getgrgid 0; print "L-push ", scalar @p, "\n";
my $r = [glob "$D/a*"]; print "L-anon ", scalar @$r, "\n";
my %h = (k => [glob "$D/a*"]); print "L-hash ", scalar @{$h{k}}, "\n";
sub inS { my $z = cnt(glob "$D/a*"); $z } my $q = inS(); print "L-inS ", $q, "\n";
PL
S-pwuid ok
S-pwnam ok
S-grgid ok
S-glob a1
S-readline l1
L-pwuid list
L-grgid 4
L-glob 3
L-readline 5
L-push 4
L-anon 3
L-hash 3
L-inS 3
PROBED

# ── #2803: a hash assignment binds its OWN context, whatever its right-hand
# side (the literal-list form always did; `(1) x 8`, `f()`, `@list` did not, so
# `print %h = (1) x 8` printed the scalar count 8, perl 11).  `@a =` rows are
# the control.  Rows P-H1..3, S-H1..3 and PU-H1 FAIL on main before s510c.
rows_agree(<<'PL', <<'PROBED', '#2803 hash assignment');
sub f { (1) x 6 }
sub cnt { scalar @_ }
my (%h, %g, @a, @list);
@list = (1) x 6;
%g = (k => 2);
# H1 %h = (1) x 8      H2 %h = f()      H3 %h = @list
# H4 %h = (%g, k => 1) H5 %h = ()
# A1..A5: @a = the same right-hand sides (the control)
my $c; my $n; my @l;
print "P-H1 ", %h = (1) x 8, "\n";
print "P-H2 ", %h = f(), "\n";
print "P-H3 ", %h = @list, "\n";
print "P-H4 ", %h = (%g, k => 1), "\n";
print "P-H5 ", %h = (), "\n";
print "P-A1 ", @a = (1) x 8, "\n";
print "P-A2 ", @a = f(), "\n";
print "J-H1 ", join(":", %h = (1) x 8), "\n";
print "J-H2 ", join(":", %h = f()), "\n";
print "J-H3 ", join(":", %h = @list), "\n";
print "J-H4 ", join(":", %h = (%g, k => 1)), "\n";
print "J-H5 ", join(":", %h = ()), "\n";
print "J-A1 ", join(":", @a = (1) x 8), "\n";
$c = cnt(%h = (1) x 8);       print "S-H1 $c\n";
$c = cnt(%h = f());           print "S-H2 $c\n";
$c = cnt(%h = @list);         print "S-H3 $c\n";
$c = cnt(%h = (%g, k => 1));  print "S-H4 $c\n";
$c = cnt(%h = ());            print "S-H5 $c\n";
$c = cnt(@a = (1) x 8);       print "S-A1 $c\n";
$n = (%h = (1) x 8);          print "N-H1 $n\n";
$n = (%h = f());              print "N-H2 $n\n";
$n = (%h = @list);            print "N-H3 $n\n";
$n = (%h = (%g, k => 1));     print "N-H4 $n\n";
$n = (%h = ());               print "N-H5 $n\n";
$n = (@a = (1) x 8);          print "N-A1 $n\n";
@l = (%h = (1) x 8);          print "L-H1 ", scalar(@l), "\n";
@l = (%h = f());              print "L-H2 ", scalar(@l), "\n";
@l = (%h = @list);            print "L-H3 ", scalar(@l), "\n";
@l = (%h = (%g, k => 1));     print "L-H4 ", scalar(@l), "\n";
@l = (%h = ());               print "L-H5 ", scalar(@l), "\n";
@l = (@a = (1) x 8);          print "L-A1 ", scalar(@l), "\n";
print "B-H1 ", ((%h = (1) x 8) ? "T" : "F"), "\n";
print "B-H2 ", ((%h = f()) ? "T" : "F"), "\n";
print "B-H3 ", ((%h = @list) ? "T" : "F"), "\n";
print "B-H4 ", ((%h = (%g, k => 1)) ? "T" : "F"), "\n";
print "B-H5 ", ((%h = ()) ? "T" : "F"), "\n";
print "B-A5 ", ((@a = ()) ? "T" : "F"), "\n";
sub inS { my $z = (%h = (1) x 8); $z } my @q = inS(); print "U-H1 @q\n";
sub inL { my @z = (%h = (1) x 8); scalar @z } my $q = inL(); print "V-H1 $q\n";
my @y; push @y, %h = (3) x 4; print "PU-H1 ", scalar(@y), "\n";
my @w; push @w, @a = (3) x 4; print "PU-A1 ", scalar(@w), "\n";
PL
P-H1 11
P-H2 11
P-H3 11
P-H4 k1
P-H5 
P-A1 11111111
P-A2 111111
J-H1 1:1
J-H2 1:1
J-H3 1:1
J-H4 k:1
J-H5 
J-A1 1:1:1:1:1:1:1:1
S-H1 2
S-H2 2
S-H3 2
S-H4 2
S-H5 0
S-A1 8
N-H1 8
N-H2 6
N-H3 6
N-H4 4
N-H5 0
N-A1 8
L-H1 2
L-H2 2
L-H3 2
L-H4 2
L-H5 0
L-A1 8
B-H1 T
B-H2 T
B-H3 T
B-H4 T
B-H5 F
B-A5 F
U-H1 8
V-H1 2
PU-H1 2
PU-A1 4
PROBED

# ── #2800: `eval STRING` is bound by its own context exactly as eval BLOCK
# is (it used to inherit the surrounding bind: `print 1, eval "(3,4)", 2` printed
# 142).  E-print, E-subL-scalar, E-arg and E-wa-void FAIL on main before s510c;
# E-join is the row join's call-wide bind used to hide.
rows_agree(<<'PL', <<'PROBED', '#2800 eval STRING');
our $g;
sub cnt { scalar @_ }
print "E-print ", 1, eval "(3,4)", 2, "\n";
sub s1 { my $x = eval "(5,6,7)"; $x } my @r = s1(); print "E-subL-scalar @r\n";
my @e = eval "(1,2)"; print "E-list ", scalar(@e), "\n";
my $c = () = eval "(1,2,3)"; print "E-countof $c\n";
print "E-join ", join(",", eval "(1,2,3)"), "\n";
$c = cnt(eval "(1,2,3)"); print "E-arg $c\n";
sub r { return eval "(1,2,9)" } my @x = r(); my $y = r(); print "E-ret ", scalar(@x), " $y\n";
sub t { eval "(4,5)" } my @z = t(); my $w = t(); print "E-tail ", scalar(@z), " $w\n";
my $v = eval "wantarray ? 1 : defined(wantarray) ? 0 : 2"; print "E-wa-scalar $v\n";
my @u = eval "wantarray ? 1 : 0"; print "E-wa-list @u\n";
eval "\$g = defined(wantarray) ? 1 : 2"; print "E-wa-void $g\n";
PL
E-print 1342
E-subL-scalar 7
E-list 2
E-countof 3
E-join 1,2,3
E-arg 3
E-ret 3 9
E-tail 2 5
E-wa-scalar 0
E-wa-list 1
E-wa-void 2
PROBED

# A-* (#2861): `&NAME` / `&$code` / `&{EXPR}` with no argument list is a CALL and is
# bound by its own context.  A-join FAILED on the tree before #2861 (join's
# call-wide bind was what it read, member 3 removed it: cmd/subval.t 24/26);
# A-print/-scalar/-hash/-sprintf/-anon FAIL on main.
rows_agree(<<'PL', <<'PROBED', '#2861 &NAME call context');
sub a1 { wantarray ? "L" : defined(wantarray) ? "S" : "V" }
my $r = \&a1;
print "A-join ", join(":", &a1), " ", join(":", &$r), " ", join(":", &{$r}), "\n";
print "A-print ", &a1, "\n";
my @x = &a1; my $s = &a1; print "A-assign @x $s\n";
print "A-scalar ", scalar(&a1), "\n";
my @y = (1, &a1); print "A-listlit @y\n";
my %h = (k => &a1); print "A-hash $h{k}\n";
print "A-sprintf ", sprintf("%s", &a1), " uc ", uc(&a1), " lc ", lc(join "", &a1), "\n";
print "A-anon ", [&a1]->[0], "\n";
sub w { return &a1 } my @z = w(); my $z = w(); print "A-ret @z $z\n";
sub t { &a1 } my @q = t(); my $p = t(); print "A-tail @q $p\n";
PL
A-join L L L
A-print L
A-assign L S
A-scalar S
A-listlit 1 L
A-hash L
A-sprintf L uc S lc l
A-anon L
A-ret L S
A-tail L S
PROBED

done_testing();

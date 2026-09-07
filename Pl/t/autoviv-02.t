#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# autoviv-02.t — NESTED-ELEMENT autovivification, the write side.
#
# autoviv-01.t is the gate's slowest file, so this is its second home (the
# metric is a file's WALL time, not its row count).
#
# #1058 (s470bk): `$h{a}{b}++` on a fresh hash SILENTLY LOST the increment.
# The chain lowers to (p-gethash-box (p-gethash-box %h "a") "b"); the inner
# call creates key "a" with an undef box and hands it to the outer call — a
# place to vivify INTO — and the outer call returned a fresh DETACHED box
# instead, so `++` incremented a box nobody could reach.  `exists $h{a}` was
# still 1, which is what made it invisible.  The array-slot spelling of the
# same undef is NIL, and that reached SBCL's GETHASH and crashed.
#
# #1057 (s470bk): the coercing compound assigns (`.=` `+=` `*=` `x=` …) build
# their read-modify-write over the place with CL's SETF, whose container
# subform is the plain READ accessor — `(setf (p-gethash (p-gethash %h "a")
# "b") …)` handed :UNDEF to GETHASH and died with an SBCL type error.  Both
# halves are now one rule: an undef container that is a WRITABLE PLACE is
# dereferenced-and-created, which is perl's.
#
# Every expectation below is the live `perl` 5.40.3 answer (probed s470bk,
# scratch/s470bk/p1058/).

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

# ── 1. #1058: the counting idiom.  The value must be READABLE afterwards,
# through the container and through an alias, and a second ++ must see the
# first — a detached box passes none of those. ─────────────────────────────
is(run_cl(<<'PL'), "1\n2\n1\n-1\n1\n", '#1058 nested ++/-- through an undef container is not lost');
my %h; $h{a}{b}++;               print $h{a}{b}, "\n";
my %g; $g{x}{y}++; $g{x}{y}++;   print $g{x}{y}, "\n";
my %i; $i{p}{q}{r}++;            print $i{p}{q}{r}, "\n";
my %j; $j{m}{n}--;               print $j{m}{n}, "\n";
my $r; $r->{p}{q}++;             print $r->{p}{q}, "\n";
PL

# ── 2. #1058: the array-shaped intermediate.  `$a[0]{k}++` CRASHED ("nil is
# not of type hash-table") because a promoted array HOLE is a box of NIL,
# not of *p-undef*; `$a[0][1]++` and `$h{a}[0]++` lost the write silently. ──
is(run_cl(<<'PL'), "1\n1\n1\nHASH\nARRAY\n", '#1058 array-shaped intermediates vivify to the right kind');
my @a; $a[0]{k}++;   print $a[0]{k}, "\n";
my @b; $b[0][1]++;   print $b[0][1], "\n";
my %h; $h{a}[0]++;   print $h{a}[0], "\n";
my @c; $c[0]{k}++;   print ref($c[0]), "\n";
my @d; $d[0][1]++;   print ref($d[0]), "\n";
PL

# ── 3. #1058: the vivified slot is the CONTAINER's own, so an alias taken
# before the write sees it and one taken after tracks it.  (A detached box
# gave inner=0 here where perl gives 1.) ───────────────────────────────────
is(run_cl(<<'PL'), "1 1\n1\nyes\n", '#1058 the vivified slot is live, not detached');
my %h; my $ref = \$h{a}{b};
print((exists $h{a} ? 1 : 0), " ", (exists $h{a}{b} ? 1 : 0), "\n");
$$ref = 1; print $h{a}{b}, "\n";
my %g; $g{a}{b}++; my $r2 = \$g{a}{b}; $$r2 += 0;
print(($g{a}{b} == 1 ? "yes" : "no"), "\n");
PL

# ── 4. #1057: the coercing compound assigns over a nested element.  Every row
# below CRASHED ("Value of pcl::h in (sb-impl::gethash3 k h nil) is :undef")
# because the store was built as CL SETF over the plain READ accessor, whose
# container subform cannot vivify.  `**=` and `x=` are the two whose perl
# answer is not the obvious one (0 and the empty string).
#
# THE LAST ROW IS THE ALIAS, and it is why the fix binds the vivified
# container and stores through the ORDINARY `(setf (p-gethash …))` write rule
# rather than routing the whole place through `p-setf`: p-setf's nested arm
# used to REPLACE the slot (raw `(setf (gethash …))`), which would answer "x"
# here.  perl answers "xy" — probed.  (That raw store was #1151; row 9 below
# is its guard, and p-setf's nested arm now writes through the box too.)
is(run_cl(<<'PL'), "x\n1\n0\n[]\n0\n-3\n3\n0\n0\nx\nx\nx\nxy\n", '#1057 compound assigns over a nested element');
my %a; $a{a}{b} .= "x";   print $a{a}{b}, "\n";
my %b; $b{a}{b} += 1;     print $b{a}{b}, "\n";
my %c; $c{a}{b} **= 2;    print $c{a}{b}, "\n";
my %d; $d{a}{b} x= 3;     print "[", $d{a}{b}, "]\n";
my %e; $e{a}{b} *= 3;     print $e{a}{b}, "\n";
my %f; $f{a}{b} -= 3;     print $f{a}{b}, "\n";
my %g; $g{a}{b} |= 3;     print $g{a}{b}, "\n";
my %n; $n{a}{b} <<= 2;    print $n{a}{b}, "\n";
my %o; $o{a}{b} %= 3;     print $o{a}{b}, "\n";
my @i; $i[0]{k} .= "x";   print $i[0]{k}, "\n";
my @j; $j[0][1] .= "x";   print $j[0][1], "\n";
my $r; $r->{p}{q} .= "x"; print $r->{p}{q}, "\n";
my %m; $m{a}{b} = "x"; my $al = \$m{a}{b}; $m{a}{b} .= "y"; print $$al, "\n";
PL

# ── 5. #1057, the string-bitwise third: `&.=` `|.=` `^.=` reached the same
# crash through a SECOND spelling of the store decision (`%p-store-back`,
# now deleted — they go through `%store-back-form` like the other thirteen).
is(run_cl(<<'PL'), "x\ny\n240\n", '#1057 &.= |.= ^.= over a nested element');
use feature 'bitwise';
no warnings 'experimental::bitwise';
my %k; $k{a}{b} = "\xff"; $k{a}{b} &.= "x"; print $k{a}{b}, "\n";
my %l; $l{a}{b} .= "\x01"; $l{a}{b} |.= "x"; print $l{a}{b}, "\n";
my @m; $m[0]{z} = "\xff"; $m[0]{z} ^.= "\x0f"; printf "%vd\n", $m[0]{z};
PL

# ── 6. #1273 (s471a): a NEGATIVE subscript on the CONTAINER of a nested
# element.  Every row here died at LOAD with SBCL's "Invalid index -1 for
# (vector t N)" — uncatchable in Perl terms — because the negative-subscript
# rebase `(if (< i 0) (+ len i) i)` was a COPIED idiom that three store-side
# accessors never got (p-autoviv-aref-for-hash, p-autoviv-aref-for-array,
# p-array-set): they truncated the index and handed it straight to AREF.
# Latent until #1057 routed a compound assign's container through the chain
# walker.  All ten array accessors now resolve through ONE %p-array-index.
#
# THE LAST ROW IS t/run/fresh_perl.t's `sub NewShell` verbatim
# (`my($m2) = $#Shells++; $Shells[$m2]{HOST} = $Host`) — the file died at
# load on it and produced NO TAP AT ALL, a 59-row coverage loss.
is(run_cl(<<'PL'), "zx\n8\n4z\nzy\n9 2\nbeach 1\n", '#1273 a negative subscript on a nested element container');
my @a = (1,2,{k=>"z"}); $a[-1]{k} .= "x";   print $a[-1]{k}, "\n";
my @b = (1,2,[3,4]);    $b[-1][0] += 5;      print $b[-1][0], "\n";
my @c = ([1,2],[3,4]);  $c[-1][-1] .= "z";   print $c[-1][-1], "\n";
my $r = [1,2,{k=>"z"}]; $r->[-1]{k} .= "y";  print $r->[-1]{k}, "\n";
my @d = ({k=>1},{k=>2}); $d[-1]{k} = 9;      print $d[-1]{k}, " ", scalar(@d), "\n";
my @e; $#e++; $e[-1]{HOST} = "beach";        print $e[0]{HOST}, " ", scalar(@e), "\n";
PL

# ── 7. #1273: the negative-subscript paths #1057 did NOT route — a FLAT
# element compound assign, ++, ||=, plain =, a variable index, and the READ
# side.  They worked before and must keep working: the fix moved them onto
# the shared helper, so a regression here is the helper disagreeing with the
# idiom it replaced.  The last two rows are the two quiet answers perl gives
# for a BELOW-start subscript (probed): a read is undef, `exists` is false,
# `delete` is a no-op — none of them is the fatal that row 8 asserts.
is(run_cl(<<'PL'), "1 2 3x\n1 2 4\n1 2 3\n1 2 9\n1 2 3x\n1 4 1\nu0\n1 2 3\n", '#1273 the negative-subscript paths that already worked still do');
my @a = (1,2,3); $a[-1] .= "x";  print "@a\n";
my @b = (1,2,3); $b[-1]++;       print "@b\n";
my @c = (1,2,3); $c[-1] ||= 7;   print "@c\n";
my @d = (1,2,3); $d[-1] = 9;     print "@d\n";
my @e = (1,2,3); my $i = -1; $e[$i] .= "x"; print "@e\n";
my %h = (k => [1,2]); $h{k}[-1] *= 2; print "@{$h{k}} $#{$h{k}}\n";
my @f = (1,2,3); print((defined $f[-4] ? "d" : "u"), (exists $f[-4] ? 1 : 0), "\n");
my @g = (1,2,3); delete $g[-4]; print "@g\n";
PL

# ── 8. #1273 / CLAUDE.md rule 12: a subscript that lands BEFORE the start of
# the array is perl's fatal on every LVALUE use — assignment, a coercing
# compound assign, ++, an autovivifying `{k}`, `\$a[-4]`, a write to an EMPTY
# array, and the deref spelling.  PCL used to DROP the write and carry on, so
# the program read the old value back: the #138 silent-wrong one level down.
# The message is perl's leading text (it names the subscript AS WRITTEN), so
# `$@ =~ /^Modification of non-creatable/` — which real code greps — matches,
# and the last row is the one that proves it is a TRAPPABLE perl death and
# not a host abort: the program survives the eval with @a untouched.
is(run_cl(<<'PL'), "die -4\ndie -4\ndie -4\ndie -4\ndie -4\ndie -4\ndie -1\ndie -4\nsurvived 1 2 3\n", '#1273 a below-start subscript is perl fatal on every lvalue use');
for my $t (
  sub { my @a = (1,2,3); $a[-4] = 9 },
  sub { my @a = (1,2,3); $a[-4] .= "x" },
  sub { my @a = (1,2,3); $a[-4] += 1 },
  sub { my @a = (1,2,3); $a[-4]++ },
  sub { my @a = (1,2,3); $a[-4]{k} = 1 },
  sub { my @a = (1,2,3); my $x = \$a[-4]; $$x = 1 },
  sub { my @a; $a[-1] = 5 },
  sub { my $r = [1,2,3]; $r->[-4] = 9 },
) {
  if (eval { $t->(); 1 }) { print "NO-DIE\n" }
  elsif ($@ =~ /^Modification of non-creatable array value attempted, subscript (-\d+)/) { print "die $1\n" }
  else { print "OTHER: $@" }
}
my @a = (1,2,3); eval { $a[-4] = 9 }; print "survived @a\n";
PL

# ── 9. #1151 (s473b): a plain `=` to a NESTED element must WRITE THROUGH the
# slot's box, not replace it.  `p-setf`'s nested arm expanded to a raw
# `(setf (gethash (to-string KEY) H) VAL)` — the ONE element-write entry path
# in the runtime that did not go through the write rule
# (docs/boxed-aggregates-design-s455.md §4.1) — so every alias taken before
# the assignment went stale: rows 1–5 printed the OLD value, row 6 kept
# reporting the overwritten undef as defined, row 7's foreach alias froze.
# Its array twin `p-autoviv-aref-set` already stored through `p-array-set`,
# which writes the box through; that disagreement is what identified this as
# a bug (rule 11).  Rows 8–9 are the two spellings that were already right
# and must stay so: the FLAT element, and the array-shaped nested slot.
# Every expectation is the live perl 5.40.3 answer (probed s473b).
is(run_cl(<<'PL'), "2\n2\n2\n2\n9 1\n01\n7\n2\n2\n", '#1151 a plain = to a nested element writes through the slot box');
my %a; $a{a}{b} = 1; my $r1 = \$a{a}{b}; $a{a}{b} = 2;       print $$r1, "\n";
my %b; $b{a}{b}{c} = 1; my $r2 = \$b{a}{b}{c}; $b{a}{b}{c} = 2; print $$r2, "\n";
my @c; $c[0]{k} = 1; my $r3 = \$c[0]{k}; $c[0]{k} = 2;       print $$r3, "\n";
my $d = {}; $d->{a}{b} = 1; my $r4 = \$d->{a}{b}; $d->{a}{b} = 2; print $$r4, "\n";
my %e; $e{1}{2} = 1; my $r5 = \$e{1}{2}; $e{1}{2} = 9;       print $$r5, " ", scalar(keys %{$e{1}}), "\n";
my %f; $f{a}{b} = 1; my $r6 = \$f{a}{b}; $f{a}{b} = undef;   print((defined $$r6 ? 1 : 0), (exists $f{a}{b} ? 1 : 0), "\n");
my %g; $g{a}{b} = 1; for my $v ($g{a}{b}) { $g{a}{b} = 7; print $v, "\n"; }
my %h; $h{a} = 1; my $r7 = \$h{a}; $h{a} = 2;                print $$r7, "\n";
my %i; $i{a}[0] = 1; my $r8 = \$i{a}[0]; $i{a}[0] = 2;       print $$r8, "\n";
PL

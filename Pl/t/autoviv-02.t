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

plan tests => 15;

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

# ── 10. #1241 (= #1150, s473b): the READ path vivifies every INTERMEDIATE.
# perl autovivifies whenever an undefined value is DEREFERENCED — on a pure
# read exactly as on a write; that is the gotcha the CPAN `autovivification`
# pragma exists to switch off.  PCL vivified NONE of them, so a program that
# tested `exists $h{a}` after reading `$h{a}{b}` saw a different hash: every
# row below printed a 0 where perl prints 1 (the ir-conform corpus pinned
# seven of them, cases 045/046/047/054/055/059/060-autoviv, now dropped from
# known-fail.tsv).  Row 7 CRASHED outright — `delete $f{a}{b}` handed :UNDEF
# to SBCL's GETHASH.  The vivified slot is a REFERENCE, so `ref` answers
# HASH/ARRAY per the NEXT subscript's sigil, which is what rows 1/5/9-11/14
# check.  Every expectation is the live perl 5.40.3 answer (probed s473b).
is(run_cl(<<'PL'), "11HASH\n01\n01\n110\n1HASH1\n1U\n1\n1\n1ARRAY\n1HASH\n1ARRAY\n1\n1\n1ARRAY\n", '#1241 a nested READ vivifies every intermediate');
my %a; my $v1 = $a{a}{b};        print scalar(keys %a), (exists $a{a} ?1:0), ref($a{a}), "\n";
my %b; my $e = exists $b{a}{b};  print(($e?1:0), scalar(keys %b), "\n");
my %c; my $d = defined $c{a}{b}; print(($d?1:0), scalar(keys %c), "\n");
my %d; my $v2 = $d{a}{b}{c};     print scalar(keys %d), scalar(keys %{$d{a}}), (exists $d{a}{b}{c} ?1:0), "\n";
my $r; my $v3 = $r->{p}{q};      print((defined $r ?1:0), ref($r), scalar(keys %$r), "\n");
my %e; $e{a}{b} &&= 5;           print((exists $e{a} ?1:0), (defined $e{a}{b} ? $e{a}{b} : "U"), "\n");
my %f; delete $f{a}{b};          print((exists $f{a} ?1:0), "\n");
my %g; if ($g{a}{b}) { }         print((exists $g{a} ?1:0), "\n");
my %h; my $v4 = $h{a}[0];        print scalar(keys %h), ref($h{a}), "\n";
my @i; my $v5 = $i[0]{k};        print scalar(@i), ref($i[0]), "\n";
my @j; my $v6 = $j[0][1];        print scalar(@j), ref($j[0]), "\n";
my %k; my $v7 = ${$k{a}}{b};     print((exists $k{a} ?1:0), "\n");
my %l; my $kk = "a"; my $v8 = $l{$kk}{b}; print((exists $l{a} ?1:0), "\n");
my $m; my $v9 = $m->[0][1];      print((defined $m ?1:0), ref($m), "\n");
PL

# ── 11. #1241, THE NEGATIVES — the direction that would over-create.  Only
# an INTERMEDIATE dereference vivifies: the LAST level of a chain creates
# nothing, and a FLAT element access is all last level.  Making a read
# vivify unconditionally would put keys in %h that perl never puts there,
# so these rows are the other half of the acceptance and they must keep
# printing 0.  Row 7 is the three-level `exists`: perl creates levels 1 and
# 2 and not the key itself (1,1,0).
is(run_cl(<<'PL'), "0\n0\n0\n0\n1\n0\n110\n0\n0\n", '#1241 the last level and a flat access create nothing');
my %a; my $v1 = $a{a};            print scalar(keys %a), "\n";
my %b; my $e = exists $b{a};      print scalar(keys %b), "\n";
my %c; my $d = defined $c{a};     print scalar(keys %c), "\n";
my %d; my $v2 = $d{a} // 1;       print scalar(keys %d), "\n";
my %e; $e{a}{b} = 1; my $v3 = $e{a}{zz}; print scalar(keys %{$e{a}}), "\n";
my @f; my $v4 = $f[0];            print scalar(@f), "\n";
my %g; my $v5 = exists $g{a}{b}{c}; print scalar(keys %g), scalar(keys %{$g{a}}), scalar(keys %{$g{a}{b}}), "\n";
my %h; my @k = keys %h;           print scalar(keys %h), "\n";
my %i; my $v6 = $i{a}{b};         print((exists $i{a}{b} ?1:0), "\n");
PL

# ── 12. #1456 (closed by #1241's fix): an intermediate slot holding a DEFINED
# non-reference is NOT a vivification site.  It is a SYMBOLIC REFERENCE —
# `$h{a} = "zz"; $h{a}{b} = 1` writes `$zz{b}` — and PCL used to CLOBBER the
# string with a fresh hash instead, which is neither of perl's two answers
# (under `use strict refs` perl dies; PCL models no strict hint, see
# docs/not-supported.md).  The four autoviv accessors now route a defined
# slot through `p-ensure-hashref`/`p-ensure-arrayref`, the SAME pair
# `$ref->{k}` has always used, so the twins cannot disagree again (rule 11).
# Rows 4-6: a READ-ONLY literal list in container position — the vivifying
# accessor reads the slot BEFORE extending, so `("a","b",{q=>7})[2]{q}` is 7
# and not "Modification of a read-only value"; and a list-slice container is
# normalized by the one helper p-aref-deref's own read uses.
# Rows 7-8: the chain ROOT may be a RAW (unboxed) slot — a `my ($self) = @_`
# the body only READS — and it still vivifies, through its PLACE.  That is
# what keeps the accessor shape fast: boxing such roots instead cost +97 % on
# this very loop (bench-emission-ab, s473b).
is(run_cl(<<'PL'), "zz 1\nww U 0\nyy 5\n7\ndeep\n5\n1U\nHASH1\n", '#1456 a defined non-ref intermediate is a symbolic ref, not a clobber');
no strict 'refs'; no warnings;
our %zz; my %h; $h{a} = "zz"; $h{a}{b} = 1;
print "$h{a} ", (defined $zz{b} ? $zz{b} : "U"), "\n";
our %ww; my %g; $g{a} = "ww"; my $v = $g{a}{c};
print "$g{a} ", (defined $v ? $v : "U"), " ", (exists $ww{c} ?1:0), "\n";
our @yy; my @a; $a[0] = "yy"; $a[0][2] = 5;
print "$a[0] ", (defined $yy[2] ? $yy[2] : "U"), "\n";
sub f { return ({k=>"v", j=>{d=>"deep"}}, {k=>"w"}) }
print (("a","b",{q=>7})[2]{q}); print "\n";
print( (f())[0]{j}{d} ); print "\n";
print( ([{x=>5}])[0][0]{x} ); print "\n";
sub get { my ($self, $k) = @_; return $self->{opt}{$k} }
my $o = { opt => { a => 1 } };
print get($o, "a"), (defined get($o, "zz") ? "D" : "U"), "\n";
sub viv { my ($s) = @_; my $x = $s->{p}{q}; return ref($s) . (exists $s->{p} ? 1 : 0) }
my $u; print viv($u), "\n";
PL

# ── 13. #1152 (s473b): a compound assignment over a FLAT element evaluated its
# container and its KEY TWICE.  `%store-back-form` builds the read-modify-write
# as `(setf PLACE (op PLACE VALUE))`, so the syntactic place appears twice —
# and #1057 had bound temps for the NESTED spelling only, which left one
# program giving two answers: `$h{a}{k()} .= 1` called k() once and
# `$h{k()} .= 1` twice, where perl calls it once in both.  ONE predicate now
# answers "is this an element place" instead of "is this a NESTED element
# place", so the temps bind for the whole family; the container still goes
# through the chain walker, which is the IDENTITY on a flat container, so
# #1057's vivification is unchanged (row 3).
#
# The SHORT-CIRCUIT assigns are in the same family and were in it (rows 4, 5,
# 10): `p-or-assign`/`p-//=`/`p-and-assign` read the place and then hand the
# SYNTACTIC place to `p-setf`, and the comment above them used to call that
# "harmless for the variable/constant subscripts that occur in practice" — it
# was not.  They share the seam now.
#
# The last four rows are the properties the temps must not break: the alias
# rule (a write goes THROUGH the slot's box, #1151/#1057), for the flat, the
# array, the short-circuit-then-coerce and the nested spellings.
# Every expectation is the live perl 5.40.3 answer (probed s473b).
is(run_cl(<<'PL'), "1q\n13\n11\n17\n18\n10\n1[]\n1y\n1U\npq\n6\nx!\nnm\n", '#1152 an element compound assign evaluates its container and key ONCE');
my $n = 0; sub k { $n++; return "kk" }
my $m = 0; sub i { $m++; return 1 }
my %a; $a{a} = "z"; $a{k()} .= "q";   print "$n$a{kk}\n"; $n = 0;
my @b = (1,2,3); $b[i()] += 1;        print "$m$b[1]\n"; $m = 0;
my %c; $c{a}{k()} .= "1";             print "$n$c{a}{kk}\n"; $n = 0;
my %f; $f{k()} ||= 7;                 print "$n$f{kk}\n"; $n = 0;
my %g; $g{k()} //= 8;                 print "$n$g{kk}\n"; $n = 0;
my %h; $h{a} = 2; $h{k()} *= 3;       print "$n", (defined $h{kk} ? $h{kk} : "U"), "\n"; $n = 0;
my %i2; $i2{k()} x= 2;                print $n, "[", (defined $i2{kk} ? $i2{kk} : "U"), "]\n"; $n = 0;
my @j; $j[i()] .= "y";                print "$m", (defined $j[1] ? $j[1] : "U"), "\n"; $m = 0;
my %p; $p{a} = 3; $p{k()} &&= 9;      print "$n", (defined $p{kk} ? $p{kk} : "U"), "\n"; $n = 0;
my %l; $l{a} = "p"; my $al = \$l{a}; $l{a} .= "q"; print "$$al\n";
my @o = (1,2); my $a2 = \$o[0]; $o[0] += 5; print "$$a2\n";
my %q; $q{a} = "x"; my $a3 = \$q{a}; $q{a} ||= "z"; $q{a} .= "!"; print "$$a3\n";
my %r; $r{a}{b} = "n"; my $a4 = \$r{a}{b}; $r{a}{b} .= "m"; print "$$a4\n";
PL

# ── 14. #1010 (s473b): a SLICE consumed by something that ALIASES its elements
# vivifies the missing ones, as perl does — and a slice consumed by something
# that COPIES still creates nothing (row 15).  perl's slice is a list of
# LVALUES: pp_hslice/pp_aslice CREATE the element when the op is in lvalue
# context, which is what the @_ of a sub or method call, a foreach, a map/grep
# block (they alias $_) and `\(…)` put it in.  PCL had the rule for a single
# ELEMENT already — `for ($h{zz}) {}` emits the eager `-box' accessor and
# `f($h{zz})` the LAZY `-argbox' one, which is perl's own asymmetry — but the
# slice emitters never consulted it, so every one of these read without
# creating.  ONE rewrite rule now (%p-aliasing-slice-form), five consumers:
# the two foreach macros, the p-map / p-grep / p-refgen-list compiler macros,
# and the emitter's `p-viv-slice' marker on a user sub's slice argument (a
# called function cannot see its argument's form).
#
# Every expectation is the live perl 5.40.3 answer (scratch/s473b/probe/
# m1010b.pl, probed row by row).  The array rows say `scalar(@a)`: perl grows
# the array to the index, which is the same fact as creating a hash key.
is(run_cl(<<'PL'), "1\n1\n1\n1\n1W\n1\n1\n1\n4\n4\n4\n4\n1\n1\n11\nV\n11\n8\n", '#1010 a slice in an ALIASING consumer vivifies its missing slots');
package C; sub new { bless {}, shift } sub m1 { return scalar(@_) }
package main;
sub s1 { return scalar(@_) }
sub s2 { $_[0] = "W"; return 1 }
sub sc ($) { return $_[0] }
{ my %h=(a=>1); my $x = s1(@h{'a','zz'});        print +(exists $h{zz}?1:0), "\n"; }
{ my %h=(a=>1); for (@h{'a','zz'}) { }           print +(exists $h{zz}?1:0), "\n"; }
{ my %h=(a=>1); for my $v (@h{'a','zz'}) { }     print +(exists $h{zz}?1:0), "\n"; }
{ my %h=(a=>1); my @r = \(@h{'a','zz'});         print +(exists $h{zz}?1:0), "\n"; }
{ my %h=(a=>1); my $x = s2(@h{'zz','a'});        print +(exists $h{zz}?1:0), $h{zz}, "\n"; }
{ my %h=(a=>1); my @m = map { $_ } @h{'a','zz'}; print +(exists $h{zz}?1:0), "\n"; }
{ my %h=(a=>1); my @g = grep { 1 } @h{'a','zz'}; print +(exists $h{zz}?1:0), "\n"; }
{ my %h=(a=>1); my @k=('zz'); for (@h{@k}) { }   print +(exists $h{zz}?1:0), "\n"; }
{ my @a=(1); my $x = s1(@a[0,3]);                print scalar(@a), "\n"; }
{ my @a=(1); for (@a[0,3]) { }                   print scalar(@a), "\n"; }
{ my @a=(1); my @m = map { $_ } @a[0,3];         print scalar(@a), "\n"; }
{ my @a=(1); my @r = \(@a[0,3]);                 print scalar(@a), "\n"; }
{ my %h=(a=>1); my $o = C->new; my $x = $o->m1(@h{'a','zz'}); print +(exists $h{zz}?1:0), "\n"; }
{ my %h=(a=>1); my $x = s1(@h{'zz'});            print +(exists $h{zz}?1:0), "\n"; }
{ my %h; my %g=(a=>1); for (@h{'x'}, @g{'y'}) { } print +(exists $h{x}?1:0), (exists $g{y}?1:0), "\n"; }
{ my %h=(a=>1); for (@h{'a','zz'}) { $_ = "V" }  print $h{zz}, "\n"; }
# A `($)` PROTOTYPE imposes scalar context on the argument, and perl vivifies
# there too — EVERY key of the slice, not just the one the scalar read keeps
# (probed).  The emitter wraps the slice in `(p-list-scalar …)` before the
# marker sees it, which is why the marker descends through a closed set of
# value wrappers instead of only matching a bare slice.
{ my %h=(a=>1); my $x = sc(@h{'zz','yy'});       print +(exists $h{zz}?1:0), (exists $h{yy}?1:0), "\n"; }
{ my @r=(3,4); my $x = sc(@r[5,7]);              print scalar(@r), "\n"; }
PL

# ── 15. #1010's NEGATIVES: a consumer that COPIES the values creates nothing,
# and that is what stops the rule from being "a slice always vivifies".  These
# all passed BEFORE the fix and are the ones it must not move; the two-slice
# foreach in row 14 is why the foreach rewrite has to reach INSIDE the
# emitter's `(p-flatten-args (list …))' list — with only the single-slice
# shape handled, one program answered differently written two ways.
is(run_cl(<<'PL'), "0\n0\n0\n0\n0\n0\n0\n0\n1\n0\n", '#1010 NEGATIVE: a slice in a COPYING consumer creates nothing');
{ my %h=(a=>1); my @v = @h{'a','zz'};            print +(exists $h{zz}?1:0), "\n"; }
{ my %h=(a=>1); my $c = () = @h{'a','zz'};       print +(exists $h{zz}?1:0), "\n"; }
{ my %h=(a=>1); my $j = join(",", @h{'a','zz'}); print +(exists $h{zz}?1:0), "\n"; }
{ my %h=(a=>1); my @s = sort @h{'a','zz'};       print +(exists $h{zz}?1:0), "\n"; }
{ my %h=(a=>1); my @k2; push @k2, @h{'a','zz'};  print +(exists $h{zz}?1:0), "\n"; }
{ my %h=(a=>1); my $s = "@h{'a','zz'}";          print +(exists $h{zz}?1:0), "\n"; }
{ my %h=(a=>1); my %kv = %h{'zz'};               print +(exists $h{zz}?1:0), "\n"; }
{ my %h=(a=>1); my ($p,$q) = @h{'a','zz'};       print +(exists $h{zz}?1:0), "\n"; }
{ my @a=(1); my @c = @a[0,3];                    print scalar(@a), "\n"; }
{ my %h=(a=>1); my $n = @h{'a','zz'};            print +(exists $h{zz}?1:0), "\n"; }
PL

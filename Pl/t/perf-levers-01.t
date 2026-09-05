#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perf-levers-01.t — the round-27 speed levers (docs/plan-speed-and-ir-s470.md
# §A.2), each guarded the way passes-01.t guards the older Kind-A names: the
# fast shape appears by default, `PCL_OPT=-name` brings the general form back
# (where the lever HAS a name), the LICENCE's negatives do NOT take the fast
# shape, and the program prints perl's answer under every setting.
#
# WHY A FILE OF ITS OWN and not more rows in passes-01.t: passes-01.t's own
# header says its subject is the REGISTRY (does a name gate anything at all);
# these rows are about each lever's LICENCE — which operand shapes qualify and
# which must not — and each needs its own snippet.  The gate's cost metric is a
# file's WALL time (CLAUDE.md rule 6), and this file's runtime rows are a
# handful of SBCL runs, not one per row.
#
#   symref-const (task #1180, Kind-A): a CONSTANT-string operand of a symbolic
#     dereference resolves to its symbol ONCE per site.
#   numeric-slot (task #1183, Kind-A): a raw slot every one of whose writes
#     stores a compile-time NUMBER drops the compound-assign overload guard.
#   foreach-arrays (task #1184, Kind-A): `for my $x (@a, @b)` over BARE named
#     arrays iterates each in turn, with no flattened temporary.
#   the array-assignment BULK FILL (task #1181, runtime only — no PCL_OPT name
#     because there is no emission to switch): `@x = LIST` is ONE block copy
#     when every element would be stored as itself.
use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use lib "$RealBin/../..";
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl   = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);
plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;
plan tests => 54;

sub write_pl {
    my ($src) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $src;
    close $fh;
    return $file;
}
sub transpile_with {
    my ($file, $opt) = @_;
    local $ENV{PCL_OPT} = $opt if defined $opt;
    delete local $ENV{PCL_OPT} unless defined $opt;
    return PCLCore::transpile(qq{$pl2cl $file});
}
sub run_with {
    my ($file, $opt) = @_;
    local $ENV{PCL_OPT} = $opt if defined $opt;
    delete local $ENV{PCL_OPT} unless defined $opt;
    my $cl = PCLCore::transpile(qq{$pl2cl $file});
    my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl;
    close $cfh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^(?:caught |compilation unit|-->|==>|PCL Runtime loaded).*\n//gm;
    $out =~ s/^\s*Undefined.*\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

# ─────────────────────────────────────────────────────────────────────────────
# symref-const — task #1180
# ─────────────────────────────────────────────────────────────────────────────
# The three sigils with a CONSTANT name, plus the four negatives: a name built
# at run time, an interpolated one, a hard reference (no name at all), and the
# `-symref-const` switch.
my $sr = write_pl(<<'PERL');
no strict 'refs';
our $g = 2; our @ga = (1,2,3); our %gh = (a => 1);
my $pk = 'main';
my $r = \$g;
my $s = ${'main::g'} + ${'g'} + scalar(@{'main::ga'}) + scalar(keys %{'main::gh'});
$s += ${"${pk}::g"} + ${$r};
print "$s\n";
PERL

my $sr_def = transpile_with($sr, undef);
like($sr_def, qr/\(p-cast-\$ "main::g" \(p-symref-site\)\)/,
     'symref-const: a QUALIFIED constant scalar name carries its site cache');
like($sr_def, qr/\(p-cast-\$ "g" \(p-symref-site\)\)/,
     'symref-const: an UNQUALIFIED constant scalar name too (the site records its package)');
like($sr_def, qr/\(p-cast-\@ "main::ga" \(p-symref-site\)\)/,
     'symref-const: the ARRAY sigil');
like($sr_def, qr/\(p-cast-% "main::gh" \(p-symref-site\)\)/,
     'symref-const: the HASH sigil');
# NEGATIVE 1 — the name is built at run time, so no site may be attached.
like($sr_def, qr/\(p-cast-\$ \(p-string-concat \$pk "::g"\)\)/,
     'symref-const NEGATIVE: a name built at run time keeps the generic path');
# NEGATIVE 2 — a hard reference is not a name at all.
like($sr_def, qr/\(p-cast-\$ \$r\)/,
     'symref-const NEGATIVE: a hard reference operand gets no site');

# NEGATIVE 3 — the switch: every site goes back to the two-element form.
my $sr_off = transpile_with($sr, '-symref-const');
unlike($sr_off, qr/p-symref-site/,
       '-symref-const: no site cache anywhere');
like($sr_off, qr/\(p-cast-\$ "main::g"\)/,
     '-symref-const: the general form is the bare cast');
like(transpile_with($sr, 'none'), qr/\(p-cast-\$ "main::g"\)/,
     'PCL_OPT=none: the bare cast too');

# NEGATIVE 4 — a name that could hide a NUL or a control character keeps the
# generic path: the runtime's readers answer those with undef / a fresh empty
# container, a decision the site cache is never asked to hold.
my $sr_esc = write_pl(<<'PERL');
no strict 'refs';
our $g = 1;
print defined(${"main\0::g"}) ? "d" : "u", "\n";
PERL
unlike(transpile_with($sr_esc, undef), qr/p-symref-site/,
       'symref-const NEGATIVE: an ESCAPED character in the name declines the licence');

# --- runtime: the same answers under every setting -------------------------
# The three things a per-site symbol cache could get wrong, in one program:
#   (a) `local` installs a FRESH box in the cell, so a cached BOX would be
#       stale — only the SYMBOL may be cached;
#   (b) an UNQUALIFIED name resolves in the perl-level current package, and
#       ONE site can see several (a string eval carrying a `package X;` region
#       is the way that happens);
#   (c) a name whose package does not exist yet must not cache the MISS.
my $sr_run = write_pl(<<'PERL');
no strict 'refs';
package A; our $v = "A"; sub get { return ${"v"} }
package B; our $v = "B"; sub get { return ${"v"} }
package main;
our $v = "M";
our $g = 1; our @ga = (1,2,3); our %gh = (a => 1);
sub sg { return ${'main::g'} }
print sg();                                   # 1
{ local $main::g = 7; print sg() }            # 7   (a): a fresh box in the cell
print sg();                                   # 1
print A::get(), B::get();                     # AB  (b): two packages, two sites
sub e { my $p = shift; return eval "package $p; no strict 'refs'; \${'v'}" }
print e('A'), e('B'), e('main'), e('A');      # ABMA (b): ONE site, four packages
sub rd { my $x = ${'Nope::Later::x'}; return defined($x) ? $x : "u" }
print rd(), rd();                             # uu  (c): a miss stays a miss
${'Nope::Later::x'} = 5;
print rd(), rd();                             # 55
print scalar(@{'main::ga'}), scalar(keys %{'main::gh'});
{ local @main::ga = (4,5); print scalar(@{'main::ga'}) }
print scalar(@{'main::ga'}), "\n";
PERL
my $want = "171ABABMAuu553123\n";   # perl 5.40.3 is the oracle
my $got_def = run_with($sr_run, undef);
is($got_def, $want, 'symref-const: default setting is perl\'s answer for local / package / vivification order');
is(run_with($sr_run, '-symref-const'), $want, '-symref-const: the same answers');
is(run_with($sr_run, 'none'), $want, 'PCL_OPT=none: the same answers');

# The site cache must not leak ACROSS sites that share a name but not a
# package, nor across sigils with the same name.
my $sr_mix = write_pl(<<'PERL');
no strict 'refs';
package P; our $n = "Ps"; our @n = ("Pa"); our %n = (k => "Ph");
package main; our $n = "Ms"; our @n = ("Ma"); our %n = (k => "Mh");
print ${'P::n'}, ${'main::n'}, ${'n'};
print $n[0] eq "Ma" ? "" : "X", "@{'P::n'}", "@{'main::n'}";
print ${'P::n'} eq 'Ps' ? "" : "X";
print $n{k} eq 'Mh' ? "" : "X", ${'main::n'}, "\n";
PERL
is(run_with($sr_mix, undef), "PsMsMsPaMaMs\n",
   'symref-const: sites sharing a NAME across packages and sigils stay separate');
is(run_with($sr_mix, '-symref-const'), "PsMsMsPaMaMs\n", '-symref-const: same');

# A WRITE through a constant symbolic name goes to the cell the cache names,
# and the compound-assignment / ++ places (which key on the `p-cast-$` HEAD,
# so the extra site argument must not disturb them) still resolve.
my $sr_wr = write_pl(<<'PERL');
no strict 'refs';
our $w = 1; our @wa = (1,2); our %wh = (a => 1);
${'main::w'} = 5;      print $w;
${'main::w'} += 3;     print $w;
${'main::w'}++;        print $w;
${'main::w'} .= "z";   print $w;
@{'main::wa'} = (7,8); print "@wa";
%{'main::wh'} = (b=>2); print join(',', %wh);
print "\n";
PERL
my $want_wr = "589" . "9z" . "7 8" . "b,2" . "\n";
is(run_with($sr_wr, undef), $want_wr, 'symref-const: writes and compound assignments reach the cell');
is(run_with($sr_wr, '-symref-const'), $want_wr, '-symref-const: same writes');
like(transpile_with($sr_wr, undef), qr/\(p-setf \(p-cast-\$ "main::w" \(p-symref-site\)\)/,
     'symref-const: the p-setf PLACE keeps the p-cast-$ head (its place tables key on it)');
like(transpile_with($sr_wr, undef), qr/\(p-post\+\+ \(p-cast-\$ "main::w" \(p-symref-site\)\)\)/,
     'symref-const: the ++ place too');

# ─────────────────────────────────────────────────────────────────────────────
# the array-assignment BULK FILL — task #1181 (runtime only, no PCL_OPT name:
# there is no emission to switch, the way %p-vpush and %p-vec-data have none)
# ─────────────────────────────────────────────────────────────────────────────
# `@x = LIST' is one block copy when every element would be stored AS ITSELF.
# Every row below is a case the block copy could get wrong, and each is perl
# 5.40.3's own answer: the COPY semantics a shared box would break (1, 8, 13),
# self-assignment and embedding (2, 3), flattening (4, 12), a hole (5), the
# container kinds that are NOT stored as themselves — a blessed box, a scalar
# ref, a dualvar, a code ref, a glob (6, 7, 16, 17) — shrink and grow (10, 11),
# the empty source (9), a mixed list with undef (14), and the each() iterator
# an assignment must reset (15).
{
    my $bulk = write_pl(<<'PERL');
use strict; use warnings;
my @src = (1,2,3);
my @c = @src; $c[0] = 99;
print "1:@src|@c\n";
my @a = (1,2,3); @a = @a;
print "2:@a\n";
my @b = (1,2,3); @b = (0, @b, 4);
print "3:@b\n";
my @d = (1,2); my @e = (3,4); my @f = (@d, @e);
print "4:@f\n";
my @g = (1,2,3); delete $g[1]; my @h = @g;
print "5:", join(',', map { defined($_) ? $_ : 'u' } @h), " ", scalar(@h), "\n";
my $o = bless { v => 7 }, 'K'; my @i = ($o, 5); my @j = @i;
print "6:", ref($j[0]), " ", $j[0]{v}, "\n";
my $r = \my $x; $x = 3; my @k = ($r, 1); my @l = @k; ${$l[0]} = 8;
print "7:$x ", ${$l[0]}, "\n";
my @m = ("a","b"); my @n = @m; $n[1] .= "!";
print "8:@m|@n\n";
my @p; my @q = @p;
print "9:", scalar(@q), "\n";
my @big = (1..5); my @small = (7); @big = @small;
print "10:@big ", scalar(@big), "\n";
my @grow = (1); @grow = (1..6);
print "11:@grow\n";
my %hh = (a=>1); my @flat = %hh; my @fc = @flat;
print "12:", scalar(@fc), "\n";
my @s2 = (3,1,2); my @sorted = sort { $a <=> $b } @s2; $sorted[0] = 42;
print "13:@s2|@sorted\n";
my @mixed = (1, "x", undef, 2.5); my @mc = @mixed;
print "14:", join(',', map { defined($_) ? $_ : 'u' } @mc), "\n";
my @it = (1,2,3); my ($idx) = each @it; my @it2 = (9,8); @it = @it2;
my ($idx2) = each @it; print "15:$idx2\n";
my @dv; { local $! = 2; @dv = ($!); }
my @dvc = @dv; printf "16:%d %s\n", ($dvc[0]+0 == 2 ? 1 : 0), ($dvc[0] ne '' ? 'y' : 'n');
my @cr = (sub { 11 }, \*STDOUT); my @crc = @cr;
print "17:", $crc[0]->(), " ", ref($crc[1]), "\n";
PERL
    my $want_bulk = <<'OUT';
1:1 2 3|99 2 3
2:1 2 3
3:0 1 2 3 4
4:1 2 3 4
5:1,u,3 3
6:K 7
7:8 8
8:a b|a b!
9:0
10:7 1
11:1 2 3 4 5 6
12:2
13:3 1 2|42 2 3
14:1,x,u,2.5
15:0
16:1 y
17:11 GLOB
OUT
    is(run_with($bulk, undef), $want_bulk,
       'bulk fill: seventeen array-assignment shapes are perl 5.40.3\'s answers');
}

# ─────────────────────────────────────────────────────────────────────────────
# numeric-slot — task #1183 (the guard-free raw compound assign)
# ─────────────────────────────────────────────────────────────────────────────
# The licence is about the SLOT, not the site: a raw slot every one of whose
# writes stores a compile-time NUMBER can never hold a blessed box, so its
# `+=`/`-=`/`*=`/`%=`/`++`/`--` drop %compound-arith-form's overload guard.
# THE NEGATIVE THAT MATTERS is a POISONED slot — `$s += $obj` puts an object
# in a raw slot (measured, task #1153), and a later `$s *= 3` has a literal
# delta and a BLESSED current value.  Licensing on the delta alone would
# numify it: the silent wrong this file exists to prevent.
{
    my $ns = write_pl(<<'PERL');
my $o = bless {}, 'K';
my $lic = 2;  $lic *= 3; $lic %= 100; $lic += 1; $lic--; $lic++;
my $poison = 0; $poison += $o; $poison *= 3;
my $arith = 1;  $arith = $lic + 1; $arith *= 2;
my $strfam = 1; $strfam += 2; $strfam .= "x";
sub par { my ($p) = @_; $p *= 2; return $p }
my $cond = 1; $cond = $o if $lic; $cond *= 2;
print "$lic $poison $arith $strfam $cond ", par(3), "\n";
PERL
    my $ns_def = transpile_with($ns, undef);
    like($ns_def, qr/\(p-\*=-raw \$lic 3 :numeric\)/,  'numeric-slot: `*=` with a literal delta on a literal-only slot');
    like($ns_def, qr/\(p-%=-raw \$lic 100 :numeric\)/, 'numeric-slot: `%=` too');
    like($ns_def, qr/\(p-incf-raw \$lic 1 :numeric\)/, 'numeric-slot: `+=` too');
    like($ns_def, qr/\(p-decf-raw \$lic :numeric\)/,   'numeric-slot: root `--` (no delta)');
    like($ns_def, qr/\(p-incf-raw \$lic :numeric\)/,   'numeric-slot: root `++`');
    # NEGATIVE 1 — the POISONED slot: an object reached it through a NON-literal
    # `+=`, so the later literal `*=` keeps the guard.
    like($ns_def, qr/\(p-incf-raw \$poison \$o\)/,     'numeric-slot NEGATIVE: a non-literal delta is not licensed');
    like($ns_def, qr/\(p-\*=-raw \$poison 3\)\n?/,     'numeric-slot NEGATIVE: and it POISONS the slot for the literal site after it');
    # NEGATIVE 2 — an ARITHMETIC RHS is num-FAMILY and can still be an object
    # (`$x = $a + $obj` runs the class's `+` handler).
    unlike($ns_def, qr/\$arith \d+ :numeric/,          'numeric-slot NEGATIVE: an arithmetic RHS is not a literal');
    # NEGATIVE 3 — a `.=` write makes the slot string-family.
    unlike($ns_def, qr/\$strfam \d+ :numeric/,         'numeric-slot NEGATIVE: a str-family write declines');
    # NEGATIVE 4 — a sub PARAMETER's value is caller-supplied.
    unlike($ns_def, qr/\$p \d+ :numeric/,              'numeric-slot NEGATIVE: a sub parameter declines');
    # NEGATIVE 5 — a CONDITIONAL write is not a proven one.
    unlike($ns_def, qr/\$cond \d+ :numeric/,           'numeric-slot NEGATIVE: a conditional write declines');
    # NEGATIVE 6 — the switch.
    my $ns_off = transpile_with($ns, '-numeric-slot');
    unlike($ns_off, qr/:numeric/,                      '-numeric-slot: no marker anywhere');
    like($ns_off, qr/\(p-\*=-raw \$lic 3\)/,           '-numeric-slot: the guarded raw twin is back');
    unlike(transpile_with($ns, 'none'), qr/:numeric/,  'PCL_OPT=none: no marker either');
}
# The runtime half: every route by which an OBJECT can reach a raw slot, and
# the licensed shapes beside them — perl 5.40.3's answers, unchanged by the
# switch.  The class carries its own `++` handler, which perl asks BEFORE
# autogenerating from `+`.
{
    my $ns_run = write_pl(<<'PERL');
package P;
use overload
  '+'  => sub { P->new($_[0]{v} + (ref $_[1] ? $_[1]{v} : $_[1])) },
  '-'  => sub { P->new($_[0]{v} - (ref $_[1] ? $_[1]{v} : $_[1])) },
  '*'  => sub { P->new($_[0]{v} * (ref $_[1] ? $_[1]{v} : $_[1])) },
  '%'  => sub { P->new($_[0]{v} % (ref $_[1] ? $_[1]{v} : $_[1])) },
  '++' => sub { $_[0]{v}++; $_[0] },
  '""' => sub { "P(" . $_[0]{v} . ")" };
sub new { bless { v => $_[1] }, $_[0] }
package main;
my $o = P->new(5);
sub c1 { my $n = 1; my $f = sub { $n = $o }; $f->(); $n *= 2; return "$n" }
sub c2 { my $n = 1; my $r = \$n; $$r = $o; $n *= 2; return "$n" }
sub c3 { my ($n) = @_; $n *= 2; return "$n" }
sub c4 { my $n = 1; $n = $o if 1; $n *= 2; return "$n" }
sub c5 { my $n = 1; $n = $o + 0; $n *= 2; return "$n" }
sub c6 { my $n = 1; eval '$n = $o'; $n *= 2; return "$n" }
sub c7 { my $n = 1; $n += $o; $n++; return "$n" }
sub c8 { my $n = 2; for (1..3) { $n *= 3; $n %= 100; $n += 1; $n--; $n++ } return "$n" }
sub c9 { my $n = 10; $n *= -2; $n += 0.5; return "$n" }
sub c10 { my $n = 1; $n += 2; $n .= "x"; return "$n" }
sub c11 { my $n; $n += 5; $n *= 2; return "$n" }
print join(' ', c1(), c2(), c3($o), c4(), c5(), c6(), c7(), c8(), c9(), c10(), c11()), "\n";
PERL
    my $want_ns = "P(10) P(10) P(10) P(10) P(10) P(10) P(7) 67 -19.5 3x 10\n";
    is(run_with($ns_run, undef), $want_ns,
       'numeric-slot: every route an OBJECT reaches a raw slot by is perl 5.40.3\'s answer');
    is(run_with($ns_run, '-numeric-slot'), $want_ns, '-numeric-slot: the same answers');
}

# ─────────────────────────────────────────────────────────────────────────────
# foreach-arrays — task #1184 (the multi-array run, no flattened temporary)
# ─────────────────────────────────────────────────────────────────────────────
# Each case is its OWN sub so the #1140 array facts are scoped to it: an
# `escapes` anywhere in a region disqualifies the array for every loop in it,
# which is the licence working, not the emission missing.
{
    my $fa = write_pl(<<'PERL');
use strict; use warnings;
sub c1  { my @a=(1,2,3); my @b=(4,5); my $s=0; for my $x (@a,@b) { $s+=$x } return $s }
sub c2  { my @a=(1,2,3); my @e=(); my @b=(4,5); my $s=0; for my $x (@a,@e,@b) { $s+=$x } return $s }
sub c3  { my @e=(); my @f=(); my $s=0; for my $x (@e,@f) { $s+=$x } return "$s" }
sub c5  { my @a=(1,2,3); my @b=(4,5); my $s=0; OUT: for my $x (@a,@b) { next OUT if $x==2; last OUT if $x==5; $s+=$x } return $s }
sub c7  { my @a=(1,2,3); my @b=(4,5); my $s=0; my $r=0; for my $x (@a,@b) { $r++; if ($x==3 && $r<10) { redo } $s+=$x } return "$s $r" }
sub c8  { my @a=(1,2,3); my @b=(4,5); my $s=0; for my $x (@a,@b) { $s+=$x } continue { $s+=100 } return $s }
sub c9  { my @a=(1,2,3); my @b=(4,5); my $s=0; for my $x (@a[0,1],@b) { $s+=$x } return $s }
sub c10 { my @a=(1,2,3); my $ar=\@a; my @b=(4,5); my $s=0; for my $x (@$ar,@b) { $s+=$x } return $s }
sub c11 { my @a=(1,2,3); my $s=0; for my $x (@a,99) { $s+=$x } return $s }
sub c13 { my @w=(1,2,3); my @v=(4,5); my $s=0; for my $x (@w,@v) { $w[2]=99; $s+=$x } return "$s @w" }
sub c14 { my @a=(1,2,3); my @b=(4,5); my @c=(6); my $s=0; for my $x (@a,@b) { for my $y (@b,@c) { $s+=$x*$y } } return $s }
sub c15 { my @p=(1,2); my @q=(3,4); for my $x (@p,@q) { $x*=10 } return "@p @q" }
sub c16 { my @c=(6); my %h=(k=>1); my $s=0; for my $x (@c,%h) { $s += ($x=~/^\d+$/ ? $x : 0) } return $s }
sub c17 { my @a=(1,2,3); my @b=(4,5); my $s=0; for my $x (@a,@b) { push @a, 9 if @a < 5; $s+=$x } return "$s ".scalar(@a) }
print join(' ', c1(),c2(),c3(),c5(),c7(),c8(),c9(),c10(),c11(),c13(),c14(),c15(),c16(),c17()), "\n";
PERL
    my $fa_def = transpile_with($fa, undef);
    # A `(vector @a @b)` list plus `:arrays t` — the printer may wrap the keys
    # onto their own lines, so match with \s+.
    my $runs = () = ($fa_def =~ /\(p-foreach-raw \(\$\w+ \(vector [^)]*\)\)\s+(?::label\s+\S+\s+)?:arrays\s+t\b/g);
    is($runs, 8, 'foreach-arrays: eight licensed loops take the run (c1 c2 c3 c5 c7 c8 and c14\'s two nested ones)')
      or diag("runs=$runs");
    like($fa_def, qr/\(p-foreach-raw \(\$x \(vector \@a \@b\)\)\s+:arrays\s+t/,
         'foreach-arrays: two bare arrays');
    like($fa_def, qr/\(p-foreach-raw \(\$x \(vector \@a \@e \@b\)\)\s+:arrays\s+t/,
         'foreach-arrays: three, one of them empty');
    like($fa_def, qr/\(p-foreach-raw \(\$x \(vector \@a \@b\)\)\s+:label\s+OUT\s+:arrays\s+t/,
         'foreach-arrays: a LABELLED loop keeps its label and takes the run');
    # NEGATIVES — every one of them is a shape the run must not take.
    like($fa_def, qr/\(p-aslice \@a 0 1\)/,      'foreach-arrays NEGATIVE: a SLICE is not a bare array');
    like($fa_def, qr/p-flatten-args \(list \(p-cast-\@ \$ar\)/, 'foreach-arrays NEGATIVE: a DEREF is not a bare array');
    like($fa_def, qr/p-flatten-args \(list \@a 99\)/,           'foreach-arrays NEGATIVE: a scalar in the list');
    like($fa_def, qr/\(p-foreach \(\$x \(p-flatten-args \(list \@w \@v\)/, 'foreach-arrays NEGATIVE: an array WRITTEN in the body (#1140)');
    like($fa_def, qr/\(p-foreach \(\$x \(p-flatten-args \(list \@p \@q\)/, 'foreach-arrays NEGATIVE: a WRITTEN loop variable declines the raw arm outright');
    like($fa_def, qr/\(p-foreach \(\$x \(p-flatten-args \(list \@c %h\)/,  'foreach-arrays NEGATIVE: a HASH in the list');
    like($fa_def, qr/\(p-foreach \(\$x \(p-flatten-args \(list \@a \@b\)\)\)\s+:my\s+t\s+\(p-if\s+\(p-< \@a 5\)/,
         'foreach-arrays NEGATIVE: a push into a source array (#1140 written_in) — and the BOXED foreach, since the write also revokes the read-only arm');
    # NEGATIVE — the switch.
    my $fa_off = transpile_with($fa, '-foreach-arrays');
    unlike($fa_off, qr/:arrays/, '-foreach-arrays: no run anywhere');
    like($fa_off, qr/\(p-foreach-raw \(\$x \(p-flatten-args \(list \@a \@b\)/,
         '-foreach-arrays: the flattened list is back, still the raw arm');
    unlike(transpile_with($fa, 'none'), qr/:arrays/, 'PCL_OPT=none: no run either');
    # RUNTIME: perl 5.40.3's answers, unchanged by the switch.  `last`, `next`,
    # `redo` and a `continue` block all cross an array boundary here.
    my $want_fa = "15 15 0 8 15 12 515 12 15 105 111 1 2 99 225 10 20 30 40 7 15 5\n";
    is(run_with($fa, undef), $want_fa, 'foreach-arrays: fourteen loop shapes are perl 5.40.3\'s answers');
    is(run_with($fa, '-foreach-arrays'), $want_fa, '-foreach-arrays: the same answers');
}

# The registry's own contract: the name is known, and a typo still dies.
{
    require Pl::Passes;
    ok(exists $Pl::Passes::KIND_A{q(symref-const)} && %Pl::Passes::KIND_A, 'symref-const is a registered Kind-A name');
}
{
    my (undef, $err, $rc) = PCLCore::transpile_raw(qq{PCL_OPT=symref-konst $pl2cl $sr});
    ok($rc != 0 && $err =~ /unknown optimization name\(s\) in PCL_OPT: symref-konst/,
       'a near-miss of the new name dies naming the known list');
}

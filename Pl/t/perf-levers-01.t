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
plan tests => 22;

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

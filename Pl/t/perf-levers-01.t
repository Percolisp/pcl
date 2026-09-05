#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perf-levers-01.t — the round-27 Kind-A speed levers (docs/plan-speed-and-ir-s470.md
# §A.2 rows 1-3 + L4), each guarded the way passes-01.t guards the older names:
# the fast emission appears by default, `PCL_OPT=-name` brings the general form
# back, the LICENCE's negatives do NOT take the fast shape, and the program
# prints the same thing under every setting.
#
# WHY A FILE OF ITS OWN and not more rows in passes-01.t: passes-01.t's own
# header says its subject is the REGISTRY (does a name gate anything at all);
# these rows are about each lever's LICENCE — which operand shapes qualify and
# which must not — and each needs its own snippet.  The gate's cost metric is a
# file's WALL time (CLAUDE.md rule 6), and this file's runtime rows are three
# SBCL runs, not one per row.
#
#   symref-const (task #1180): a CONSTANT-string operand of a symbolic
#     dereference resolves to its symbol ONCE per site.
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
plan tests => 21;

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

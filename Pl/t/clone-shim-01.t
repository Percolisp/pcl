#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# clone-shim-01.t -- task #3826: lib/Clone.pm, the pure-Perl Clone.
#
# The real Clone (0.47) is XS, so `use Clone qw(clone)` died at load under PCL
# and took HTTP::Headers -- and with it 18 of HTTP-Message's 23 test files --
# along.  The shim walks the structure the way Clone.xs's rv_clone does: deep
# copy, re-bless, CODE / GLOB / Regexp shared, a referent seen twice copied
# once (cycles stay cycles), and clone($ref, $depth) shares below $depth.
#
# The expected text below was produced by the REAL XS Clone 0.47 under perl
# 5.40.3; the row also compares against perl live when Clone is installed
# (CI's stock machine has no Clone, so the fixed text is the bar there).
use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $runpcl = "$RealBin/../../runpcl";
plan skip_all => "runpcl not found" if !-x $runpcl;
plan skip_all => "sbcl not found"   if !`which sbcl 2>/dev/null`;
plan tests => 2;

my $prog = <<'P';
use Clone qw(clone);
my $code = sub { 1 };
my $h = { a => [1, 2, { b => 3 }], c => \"s", d => $code, o => bless({ x => 1 }, 'Foo'), r => qr/x/ };
$h->{self} = $h; $h->{twice} = $h->{a};
my $c = clone($h);
print join(" ", ($c != $h ? 1 : 0), ($c->{a} != $h->{a} ? 1 : 0), $c->{a}[2]{b}, ${$c->{c}},
  ($c->{d} == $code ? 1 : 0), ref($c->{o}), ($c->{o} != $h->{o} ? 1 : 0),
  ($c->{self} == $c ? 1 : 0), ($c->{twice} == $c->{a} ? 1 : 0), ref($c->{r}), ($c->{r} == $h->{r} ? 1 : 0)), "\n";
$c->{a}[0] = 9; print "$h->{a}[0]\n";
my $d = clone($h, 1); print join(" ", ($d != $h ? 1:0), ($d->{a} == $h->{a} ? 1 : 0)), "\n";
print clone("plain"), " ", (defined clone(undef) ? "def" : "undef"), "\n";
my @l = (1, [2]); my $l2 = clone(\@l); print scalar(@$l2), " ", ($l2->[1] != $l[1] ? 1 : 0), "\n";
my $s = \\"deep"; my $s2 = clone($s); print $$$s2, " ", ($$s2 != $$s ? 1 : 0), "\n";
P
my $want = <<'W';
1 1 3 s 1 Foo 1 1 1 Regexp 1
1
1 1
plain undef
2 1
deep 1
W
my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
print $fh $prog;
close $fh;
my $got = `$runpcl $file 2>&1`;
is($got, $want, 'clone: deep copy, re-bless, shared CODE/Regexp, cycles kept, depth limit (#3826)');
SKIP: {
    skip 'Clone is not installed for perl', 1 if system($^X, '-MClone', '-e', '1') != 0;
    is(scalar(`$^X $file 2>&1`), $want, 'perl with the real XS Clone prints the same text');
}

#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# re-pragma-flags-01.t -- task #4042: `use re '/FLAGS'` sets the DEFAULT
# modifiers of every pattern compiled in its lexical scope (re.pm's bits():
# letters turn on with `use`, off with `no`; a second x is /xx; the charset
# letters are exclusive; `no re` with no argument turns everything off).
# PCL read only `use re 'eval'`; the flags reached neither the pattern
# compiler nor the interpolation scanner (#3856's `x => 1`), so every row
# below printed "no" where perl prints "ok".  The fix is ONE scope walk
# (Pl::Parser2::re_pragma_at, shared with `use re 'eval'`) whose flags are
# merged into each m// / qr// / s/// literal's own, and into a RUN-TIME
# pattern operand of =~ / !~ (perl compiles that with the op's defaults too).
# Every expected value is perl's own: the program runs under perl AND pcl and
# each row compares its line.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);

my $project_root = "$RealBin/../..";
my $pcl_bin      = "$project_root/pcl";

plan skip_all => "pcl not found"  unless -x $pcl_bin;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

my @rows = (
  ['a', '/x from the pragma: a `#` comment in the pattern',
   q{{ use re "/x"; print "a ", ("q" =~ /q # c
/ ? "ok" : "no"), "\n"; }}],
  ['b', '/i from the pragma',
   q{{ use re "/i"; print "b ", ("Q" =~ /q/ ? "ok" : "no"), "\n"; }}],
  ['c', 'the scanner half: a pragma /x comment naming a newline-valued variable',
   q{{ use re "/x"; my $nl = "\nZ"; print "c ", ("q" =~ /q # $nl
/ ? "ok" : "no"), "\n"; }}],
  ['d', 'outside the scope nothing changes',
   q{print "d ", ("Q" =~ /q/ ? "ok" : "no"), "\n";}],
  ['e', '`no re \'/i\'` ends it',
   q{{ use re '/i'; no re '/i'; print "e ", ("Q" =~ /q/ ? "ok" : "no"), "\n"; }}],
  ['f', 'qr// carries the default in its stringification',
   q{{ use re '/i'; my $q = qr/q/; print "f $q\n"; }}],
  ['g', 's/// takes /x from the pragma',
   q{{ use re '/x'; my $s = "ab"; $s =~ s/a b/Z/; print "g $s\n"; }}],
  ['h', 'a RUN-TIME string pattern takes the default too',
   q{{ use re '/i'; my $p = "Q"; print "h ", ("q" =~ $p ? "ok" : "no"), "\n"; }}],
  ['i', 'an interpolated pattern takes the default',
   q{{ use re '/i'; my $p = "Q"; print "i ", ("q" =~ /$p/ ? "ok" : "no"), "\n"; }}],
  ['j', 'split\'s pattern',
   q{{ use re '/i'; print "j ", join("|", split /b/, "aBc"), "\n"; }}],
  ['l', '/s', q{{ use re '/s'; print "l ", ("a\nb" =~ /a.b/ ? "ok" : "no"), "\n"; }}],
  ['m', '/m', q{{ use re '/m'; print "m ", ("a\nb" =~ /^b/ ? "ok" : "no"), "\n"; }}],
  ['o', 'an inline (?-i) still turns it off',
   q{{ use re '/i'; print "o ", ("Q" =~ /(?-i)q/ ? "ok" : "no"), "\n"; }}],
  ['p', 'lexical, not dynamic: a sub defined outside the scope',
   q{sub f { "Q" =~ /q/ ? "ok" : "no" } { use re '/i'; print "p ", f(), "\n"; }}],
  ['q', 'an anon sub inside the scope',
   q{{ use re '/i'; my $c = sub { "Q" =~ /q/ ? "ok" : "no" }; print "q ", $c->(), "\n"; }}],
  ['r', '`no re` with no argument turns everything off',
   q{{ use re '/i'; no re; print "r ", ("Q" =~ /q/ ? "ok" : "no"), "\n"; }}],
  ['t', 'the pattern\'s own flags and the default together',
   q{{ use re '/i'; my $q = qr/q/x; print "t $q\n"; }}],
  ['u', 'an explicit charset on the pattern replaces the default charset',
   q{{ use re '/u'; my $q = qr/q/a; print "u $q\n"; }}],
  ['w', 'tr/// is not a pattern',
   q{{ use re '/i'; my $s = "aAa"; (my $t = $s) =~ tr/a/b/; print "w $t\n"; }}],
  ['x1', '/xx from the pragma: a space in a class is ignored',
   q{{ use re '/xx'; print "x1 ", (("a " =~ /^[a b]{2}$/) ? "ok" : "no"), "\n"; }}],
  ['x2', '... and /x alone keeps it',
   q{{ use re '/x'; print "x2 ", (("a " =~ /^[a b]{2}$/) ? "ok" : "no"), "\n"; }}],
  ['n1', 'an inner `no re` block, then the outer scope again',
   q{{ use re '/i'; { no re '/i'; print "n1 ", ("Q" =~ /q/ ? "ok" : "no"), "\n"; } print "n2 ", ("Q" =~ /q/ ? "ok" : "no"), "\n"; }}],
  ['n2', '... the outer scope after the inner `no re` keeps /i', q{}],
  ['qw', 'qw(/i /s)',
   q{{ use re qw(/i /s); print "qw ", ("A\nB" =~ /a.b/ ? "ok" : "no"), "\n"; }}],
  ['nm', '!~ with a run-time pattern',
   q{{ use re '/i'; my $p = "Q"; print "nm ", ("q" !~ $p ? "ok" : "no"), "\n"; }}],
  ['qo', 'a qr operand keeps its OWN flags (it was compiled outside)',
   q{my $q0 = qr/Q/; { use re '/i'; print "qo ", ("q" =~ $q0 ? "ok" : "no"), "\n"; }}],
  ['sx', 'split under /x',
   q{{ use re '/x'; my @w = split / , /, "a,b , c"; print "sx @w\n"; }}],
);

plan tests => scalar(@rows) + 1;

my ($fh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
print $fh join("\n", map { $_->[2] } @rows), "\n";
close $fh;
my $perl = `perl $pl 2>&1 </dev/null`;
my $pcl  = `$pcl_bin $pl 2>&1 </dev/null`;

sub line_of {
  my ($out, $label) = @_;
  my ($l) = $out =~ /^(\Q$label\E\b.*)$/m;
  return $l // "<no line for $label>";
}

unlike($pcl, qr/statement dropped|refused|error/i, 'the program transpiles and runs whole');
for my $r (@rows) {
  my ($label, $desc) = @$r;
  my $want = line_of($perl, $label);
  is(line_of($pcl, $label), $want, "$desc (perl: $want)");
}

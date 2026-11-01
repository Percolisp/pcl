#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Unit tests for the expect-state CHECKER (Pl::PPIExpect + tools/ppi-expect-check.pl,
# s525p): the catalogued mis-lexes PPI still makes are FLAGGED, and their correct
# spellings are NOT -- the breaking-case rule (a widened rule must be probed on
# the case it would break).
#
# The flagged shapes come from the REGISTRY (Pl/PPIBugs.pm), not from a list
# written here (#3980, s531e): the rows PCL's vendored PPI still mis-lexes
# (`fixed_in` undef, a `when.check` kind) must each be flagged with that kind
# and section, and each needs a CONTROL spelling below.  A row fixed on PCL's
# branch drops out of that set by itself; its shape is then asserted two ways:
# clean on the vendored PPI (the fix and the checker agree), and still FLAGGED
# on the installed PPI when that is the version the row was confirmed on
# (PCL_SYSTEM_PPI=1, a child process -- one process loads one PPI), so every
# detector keeps a shape it is tested on.  The pre-s531e list fixed its shapes
# by hand and went red when s527a/b fixed §4 §12 §25 §28 on the branch.
#
# NOT part of the Pl/t gate (it measures a measurement tool).  Run it directly:
#   prove tools/t/ppi-expect-check.t
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib "$RealBin/../..";
use Pl::PPIBugs;
use Pl::PPIExpect qw(check_source);
use Pl::VendorPPI;

# The partition below reads `fixed_in => 'pcl'` as "fixed on the PPI this process
# loaded", which holds only for the vendored lib; the installed PPI is reached by
# the child process further down.
plan skip_all => 'runs on the vendored PPI (the installed one is its child-process leg); unset PCL_SYSTEM_PPI'
  unless Pl::VendorPPI::is_vendored();

# The correct spelling of each row's shape, keyed by registry section.
my %CONTROL = (
   4 => 'print 7 % -3, "\n";',
  11 => 'sub ok { print "ok $_[1]\n" } $_ = "x"; ok(/x/, "d");',
  12 => 'my ($s, $k) = (0, "abc"); $s += length($k) * length($k); print "$s\n";',
  19 => 'sub x { "PKG" } print &x(), "|\n";',
  25 => 'my $z = length("abc") - length("a"); print "$z\n";',
  28 => '$_ = 1; my $x = $_ . 2; print "$x\n";',
  40 => 'print "hello world\n"; print "after\n";',
  48 => 'my @x = @{[1, 2, 3]}[1]; print "@x\n";',
);

sub _show { join("\n", map { "$_->{kind} '$_->{text}' \@$_->{state} §$_->{sec}" } @_) }
sub _flagged {
  my ($r, @rows) = @_;
  return grep { $_->{kind} eq $r->{when}{check} && $_->{sec} eq $r->{id} && !$_->{cascade} } @rows;
}

my @checked = grep { defined $_->{when}{check} } @Pl::PPIBugs::BUGS;
my @open    = grep { !defined $_->{ppi}{fixed_in} } @checked;
my @fixed   = grep {  defined $_->{ppi}{fixed_in} } @checked;
ok(scalar(@open), 'the registry holds rows the vendored PPI still mis-lexes, with a checker kind')
  or diag 'every checked row is fixed: the checker has nothing left to flag on the vendored PPI';

for my $r (@open) {
  my @r = check_source($r->{repro});
  ok(_flagged($r, @r), "§$r->{id} flagged ($r->{when}{check}): registry repro") or diag _show(@r);
  my $good = $CONTROL{$r->{id}};
  ok(defined $good, "§$r->{id} has a control spelling") or next;
  my @g = check_source($good);
  is(scalar(@g), 0, "§$r->{id} control is clean: $good") or diag _show(@g);
}

# Fixed on PCL's branch: the vendored PPI lexes the repro right, so the checker is quiet.
for my $r (@fixed) {
  my @r = check_source($r->{repro});
  ok(!_flagged($r, @r), "§$r->{id} fixed on the vendored PPI: its repro is not flagged") or diag _show(@r);
}

# ... and the detector still fires on the PPI the row was confirmed on.
# A kind several rows share (glob-for-multiply: §12 §55) is attributed by the checker to
# one of them, so the section matched is any row registered with that kind.
my %by_kind;
push @{ $by_kind{$_->{when}{check}} }, $_->{id} for @checked;
{
  my $child = sub {
    my @in = @_;
    my $prog = 'use lib $ARGV[0]; use PPI; use Pl::PPIExpect qw(check_source); binmode STDIN, q{:encoding(UTF-8)}; local $/; '
             . 'my @s = split /\0/, <STDIN>; print "VERSION $PPI::VERSION\n"; '
             . 'for my $i (0..$#s) { print join("\t", $i, @{$_}{qw(kind sec cascade)}), "\n" for check_source($s[$i]) }';
    local $ENV{PCL_SYSTEM_PPI} = 1;
    my ($fh, $f) = tempfile(UNLINK => 1);
    binmode $fh, ':encoding(UTF-8)'; print $fh join("\0", @in); close $fh;
    return `$^X -e '$prog' "$RealBin/../.." < "$f" 2>/dev/null`;
  };
  my @out = $child->(map { $_->{repro} } @fixed);
  my ($ver) = map { /^VERSION (\S+)/ ? $1 : () } @out;
  SKIP: {
    skip 'no installed PPI to run the fixed rows on (PCL_SYSTEM_PPI=1)', scalar(@fixed) unless defined $ver;
    for my $i (0 .. $#fixed) {
      my $r = $fixed[$i];
      SKIP: {
        skip "§$r->{id}: the installed PPI is $ver, the row was confirmed on $r->{ppi}{confirmed}", 1
          if $ver ne $r->{ppi}{confirmed};
        my %sec = map { $_ => 1 } @{ $by_kind{$r->{when}{check}} };
        my @hit = grep { my @c = split /\t/; chomp $c[3];
                         $c[0] eq $i && $c[1] eq $r->{when}{check} && !$c[3]
                           && grep { $sec{$_} } split m{/}, $c[2] } @out;
        ok(scalar(@hit), "§$r->{id} flagged ($r->{when}{check}) on the installed PPI $ver");
      }
    }
  }
}

# The command: a file in, one TSV row per contradiction out, nothing for a clean file.
{
  my ($case) = grep { my @r = check_source($_->{repro}); @r == 1 && _flagged($_, @r) && $CONTROL{$_->{id}} } @open;
  SKIP: {
    skip 'no open registry row raises exactly one contradiction', 2 unless $case;
    my ($row) = check_source($case->{repro});
    my ($fh, $f) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    my $first = $case->{repro}; $first =~ s/\n+\z//;
    print $fh "$first\n$CONTROL{$case->{id}}\n"; close $fh;
    my @out = `$^X $RealBin/../ppi-expect-check.pl $f`;
    is(scalar(@out), 1, "tools/ppi-expect-check.pl: one row for one mis-lex (§$case->{id})");
    like($out[0] // '', qr/\t$row->{line}\t\d+\t\Q$row->{text}\E\t\Q$row->{class}\E\t$row->{state}\t.*\t$row->{kind}\t$case->{id}\t0\t/,
         'the row: line, text, class, state, kind, §, primary');
  }
}

done_testing();

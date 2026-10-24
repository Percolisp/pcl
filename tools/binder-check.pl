#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# binder-check.pl -- run the BINDER's dual-run (Pl::Binder, PCL_BINDER_CHECK)
# over files or a named population and merge its log rows (s525q;
# docs/binder-dual-run-s525.md).
#
# INERT: each file is transpiled by this tree's ./pl2cl (a `.pm` with
# --module) with PCL_BINDER_CHECK=log; the emitted CL is discarded.
#
#   tools/binder-check.pl FILE...
#   tools/binder-check.pl --population perl-tests|perl-t|everyday|cpan|all
#   options: --out FILE (default stdout)  --jobs N (default 2)  --summary
#            --summary-of LOG   (print the D3 numbers of an existing merged log)
#            --timeout S (default 120)
#   tools/binder-check.pl --dump FILE   every reference -> its binding, one
#            line each, from a PCL_BINDER_CHECK=die PCL_BINDER_TRACE=1
#            transpile (exit status = the transpile's: a disagreement DIES)
#
# Log rows (TSV, first column = row kind):
#   S file refs= bindings= kinds= via= captures= fragments= build_ms= tokens= eval=
#   D file line col token resolver legacy binder kind note pass
#   R file why pass resolver tokens= build_ms=        (a rebuild)
#   C file <resolver>=<calls>... rebuilds=            (per transpile)
#   E file line col token resolver legacy - - error   (a binder error)
use strict;
use warnings;
use FindBin qw($RealBin);
use lib "$RealBin/lib";
use Getopt::Long;
use PCLPopulations;

my %o = (jobs => 2, timeout => 120);
GetOptions(\%o, 'population=s', 'out=s', 'jobs=i', 'summary', 'summary-of=s', 'dump=s', 'timeout=i') or exit 2;
my $root = "$RealBin/..";

if (defined $o{dump}) { exit dump_file($o{dump}) }
if (defined $o{'summary-of'}) { summary($o{'summary-of'}); exit 0 }

my @files = @ARGV;
push @files, PCLPopulations::files($o{population}, $root) if $o{population};
@files or die "binder-check: no files (give FILE... or --population NAME)\n";

sub rel { my $p = shift; $p =~ s{^\Q$root\E/}{}; return $p }

sub dump_file {
  my ($f) = @_;
  local $ENV{PCL_BINDER_CHECK} = 'die';
  local $ENV{PCL_BINDER_TRACE} = 1;
  my @mod = $f =~ /\.pm\z/ ? ('--module') : ();
  my $pid = open(my $fh, '-|') // die "fork: $!";
  if (!$pid) {
    open STDERR, '>&', \*STDOUT or die;
    open my $null, '>', '/dev/null' or die;
    # stdout (the CL) to /dev/null, stderr (the trace) to the pipe
    open STDOUT, '>&', $null or die;
    exec "$root/pl2cl", @mod, $f;
    exit 127;
  }
  my @err;
  while (my $l = <$fh>) {
    if ($l =~ /^PCL-BINDER\t/) {
      chomp $l;
      my (undef, $pos, $tok, $canon, $via, $id, $kind, $name, $decl) = split /\t/, $l;
      printf "%-7s %-24s %-8s %-11s -> %-7s %-18s %s %s\n", $pos, $tok, $canon, $via, $kind, $name, $decl, $id;
    }
    else { push @err, $l }
  }
  close $fh;
  my $st = $? >> 8;
  print STDERR grep { /PCL binder|^D\t|^E\t/ } @err;
  return $st;
}

my $out;
if ($o{out}) { open $out, '>:utf8', $o{out} or die "$o{out}: $!\n" } else { $out = \*STDOUT; binmode $out, ':utf8' }

my $tmpdir = "/tmp/pcl-binder-check.$$";
mkdir $tmpdir or die "$tmpdir: $!\n";
my (%kid, %status);
my $n = 0;
sub reap {
  my $pid = waitpid(-1, 0);
  return unless $pid > 0 && $kid{$pid};
  my ($f, $log) = @{ delete $kid{$pid} };
  my ($st, $rc) = ($?, $? >> 8);
  $status{ $rc == 124 ? q{timeout} : $st ? q{transpile-failed} : q{ok} }++;
  if (open my $h, q{<:utf8}, $log) { print $out $_ while <$h>; close $h }
  print $out join("\t", q{X}, rel($f), $rc == 124 ? q{timeout} : "failed rc=$rc"), "\n" if $st;
  unlink $log;
}
for my $f (@files) {
  reap() while keys(%kid) >= $o{jobs};
  my $log = "$tmpdir/" . $n++ . '.tsv';
  my $pid = fork() // die "fork: $!";
  if (!$pid) {
    $ENV{PCL_BINDER_CHECK} = 'log';
    $ENV{PCL_BINDER_LOG}   = $log;
    open STDOUT, '>', '/dev/null' or die;
    open STDERR, '>', '/dev/null' or die;
    chdir $root;
    exec q{timeout}, $o{timeout}, "$root/pl2cl", ($f =~ /\.pm\z/ ? (q{--module}) : ()), rel($f);
    exit 127;
  }
  $kid{$pid} = [$f, $log];
}
reap() while keys %kid;
rmdir $tmpdir;
close $out if $o{out};
printf STDERR "binder-check: %d files (%s)\n", scalar(@files),
  join(', ', map { "$_ $status{$_}" } sort keys %status);
summary($o{out}) if $o{summary} && $o{out};

# --summary: the D3 numbers, read back from the merged log.
sub summary {
  my ($file) = @_;
  open my $h, '<:utf8', $file or die "$file: $!\n";
  my (%s, %kinds, %via, %d, %r, %calls, %e, %files);
  while (<$h>) {
    chomp;
    my @f = split /\t/;
    my $t = $f[0] // '';
    if ($t eq 'S') {
      $files{$f[1]}++;
      for my $kv (@f[2 .. $#f]) {
        my ($k, $v) = split /=/, $kv, 2;
        next unless defined $v;
        if ($k eq 'kinds' || $k eq 'via') {
          for (split /,/, $v) { my ($kk, $vv) = split /:/; ($k eq q{kinds} ? \%kinds : \%via)->{$kk} += $vv }
        }
        elsif ($v =~ /^\d+$/) { $s{$k} += $v }
      }
      $s{builds}++;
    }
    elsif ($t eq 'D') { $d{"$f[5]\t$f[6]->$f[7]\t$f[8]"}++; $s{disagreements}++ }
    elsif ($t eq 'R') { $r{"$f[2]\t$f[3]"}++; $s{rebuilds}++ }
    elsif ($t eq 'C') { for my $kv (@f[2 .. $#f]) { my ($k, $v) = split /=/, $kv; next if $k eq q{rebuilds}; $calls{$k} += $v // 0 } }
    elsif ($t eq 'E') { (my $m = $f[9] // '') =~ s/ at \S+ line \d+.*//; $e{"$f[5]\t" . substr($m, 0, 90)}++ }
  }
  printf STDERR "files with a binder: %d  builds: %d  refs: %d  bindings: %d  captures: %d  fragments: %d  build_ms: %d  tokens: %d\n",
    scalar(keys %files), $s{builds} // 0, $s{refs} // 0, $s{bindings} // 0, $s{captures} // 0,
    $s{fragments} // 0, $s{build_ms} // 0, $s{tokens} // 0;
  print STDERR "bindings by kind: ", join(' ', map { "$_=$kinds{$_}" } sort keys %kinds), "\n";
  print STDERR "refs by via:      ", join(' ', map { "$_=$via{$_}" } sort keys %via), "\n";
  print STDERR "resolver calls:   ", join(' ', map { "$_=$calls{$_}" } sort keys %calls), "\n";
  print STDERR "disagreements (distinct sites) by resolver / legacy->binder / binder kind:\n";
  printf STDERR "  %6d  %s\n", $d{$_}, $_ for sort { $d{$b} <=> $d{$a} || $a cmp $b } keys %d;
  print STDERR "rebuilds by why / pass:\n";
  printf STDERR "  %6d  %s\n", $r{$_}, $_ for sort { $r{$b} <=> $r{$a} || $a cmp $b } keys %r;
  print STDERR "binder errors:\n";
  printf STDERR "  %6d  %s\n", $e{$_}, $_ for sort { $e{$b} <=> $e{$a} || $a cmp $b } keys %e;
}

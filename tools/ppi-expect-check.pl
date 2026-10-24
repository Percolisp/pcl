#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ppi-expect-check.pl -- run the expect-state CHECKER (Pl::PPIExpect) over
# files or a named population and write one TSV row per token whose PPI class
# contradicts perl's PL_expect state (s525p; docs/expect-state-checker-s525.md).
#
# INERT: it parses with PPI only (no transpile, nothing is modified).
#
#   tools/ppi-expect-check.pl FILE...
#   tools/ppi-expect-check.pl --population perl-tests|perl-t|everyday|cpan|all
#   options: --out FILE (default stdout)  --jobs N (default 1)
#            --mode raw|pcl  (raw = PPI::Document->new, the default;
#                             pcl = Pl::Parser's document: _ppi_new + the
#                             in-place passes of _ppi_parse, i.e. what is left
#                             after the Parser.pm repairs)
#            --summary  print the per-kind counts to stderr
#
# TSV columns: file line col text class state expect kind sec cascade note
# (`sec` = the docs/ppi-upstream-bugs.md section, `cascade` = 1 when an
# earlier row of the same statement exists).  A file PPI cannot lex gives one
# `lexer-dies` row.
use strict;
use warnings;
use FindBin qw($RealBin);
use lib "$RealBin/..";
use lib "$RealBin/lib";
use Getopt::Long;
use File::Find;
use Pl::PPIExpect qw(check_document check_source);

my %o = (jobs => 1, mode => 'raw');
GetOptions(\%o, 'population=s', 'out=s', 'jobs=i', 'mode=s', 'summary') or exit 2;
my $root = "$RealBin/..";

my @files = @ARGV;
push @files, population($o{population}) if $o{population};
@files or die "ppi-expect-check: no files (give FILE... or --population NAME)\n";

sub population {
  my ($name) = @_;
  require PCLPopulations;            # the ONE population list (s525q)
  return PCLPopulations::files($name, $root);
}

my $parser;
sub doc_of {
  my ($src) = @_;
  return PPI::Document->new(\$src) if $o{mode} eq 'raw';
  require Pl::Parser;
  $parser //= Pl::Parser->new;
  return $parser->_ppi_parse($src);
}

sub run_files {
  my ($list, $fh, $stats) = @_;
  for my $f (@$list) {
    my $src = do { open my $h, '<:raw', $f or do { warn "$f: $!\n"; next }; local $/; <$h> };
    # `use utf8`: the source is UTF-8, decoded exactly as PCL decodes it before PPI
    # (Pl::Parser::_maybe_decode_utf8) -- PPI lexes raw bytes >127 as errors.
    if ($src =~ /\buse\s+utf8\b/) { require Pl::Parser; $src = Pl::Parser::_maybe_decode_utf8($src) }
    $stats->{files}++;
    (my $name = $f) =~ s{^\Q$root\E/}{};
    my @rows;
    my $doc = eval { doc_of($src) };
    if (!$doc) {
      @rows = check_source($src, file => $name, stats => $stats);   # PPI could not build it: the lexer-dies row
      @rows = ({ file => $name, line => 0, col => 0, text => '', class => '', state => '',
                 expect => 'a document', kind => 'lexer-dies', sec => '', cascade => 0,
                 note => ($@ || 'no document') =~ s/\s+/ /gr }) if !@rows;
    }
    else {
      @rows = eval { check_document($doc, file => $name, stats => $stats) };
      if ($@) { die "ppi-expect-check: $name: $@" }
    }
    $stats->{rows}++ for @rows;
    for my $r (@rows) {
      print $fh join("\t", map { my $v = $r->{$_} // ''; $v =~ s/[\t\n]/ /g; $v }
                     qw(file line col text class state expect kind sec cascade note)), "\n";
      $stats->{kind}{"$r->{kind}\t$r->{class}\t$r->{state}\t" . ($r->{sec} // '')}++ unless $r->{cascade};
      $stats->{cascade}++ if $r->{cascade};
    }
  }
}

my $out;
if ($o{out}) { open $out, '>', $o{out} or die "$o{out}: $!\n" } else { $out = \*STDOUT }
binmode $out, ':utf8';

my %stats;
if ($o{jobs} <= 1) { run_files(\@files, $out, \%stats) }
else {
  my @chunks; push @{ $chunks[$_ % $o{jobs}] }, $files[$_] for 0 .. $#files;
  my @kids;
  for my $j (0 .. $#chunks) {
    my $tmp = ($o{out} // "/tmp/ppi-expect-$$") . ".part$j";
    my $pid = fork // die "fork: $!";
    if (!$pid) {
      open my $fh, '>', $tmp or die; binmode $fh, ':utf8';
      my %s; run_files($chunks[$j], $fh, \%s); close $fh;
      open my $sf, '>', "$tmp.stats" or die;
      print $sf join("\t", 'files', $s{files} // 0), "\n", join("\t", 'tokens', $s{tokens} // 0), "\n",
                join("\t", 'rows', $s{rows} // 0), "\n", join("\t", 'cascade', $s{cascade} // 0), "\n";
      print $sf "kind\t$_\t$s{kind}{$_}\n" for keys %{ $s{kind} || {} };
      exit 0;
    }
    push @kids, [$pid, $tmp];
  }
  for my $k (@kids) {
    waitpid $k->[0], 0;
    die "ppi-expect-check: a worker failed\n" if $?;
    open my $h, '<:utf8', $k->[1] or die; print $out $_ while <$h>; close $h; unlink $k->[1];
    open my $sf, '<', "$k->[1].stats" or die;
    while (<$sf>) { chomp; my @f = split /\t/;
      if ($f[0] eq 'kind') { $stats{kind}{join "\t", @f[1 .. $#f - 1]} += $f[-1] } else { $stats{$f[0]} += $f[1] } }
    close $sf; unlink "$k->[1].stats";
  }
}
close $out if $o{out};

if ($o{summary}) {
  printf STDERR "files %d  tokens %d  rows %d (primary %d, cascade %d)\n",
    $stats{files} // 0, $stats{tokens} // 0, $stats{rows} // 0,
    ($stats{rows} // 0) - ($stats{cascade} // 0), $stats{cascade} // 0;
  my $k = $stats{kind} || {};
  printf STDERR "%6d  %s\n", $k->{$_}, $_ for sort { $k->{$b} <=> $k->{$a} || $a cmp $b } keys %$k;
}

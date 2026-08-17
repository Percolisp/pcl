#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later
#
# sub-call-census.pl -- how often is each compiler sub CALLED, and what does
# it cost, when PCL transpiles a corpus?  (task #379, s411)
#
# The s386 duplication review measured this with a wrap-all-subs tracer that
# lived in a scratchpad and is gone; this is the reproducible version, on
# Devel::NYTProf (exact, no wrapping, so `caller`-based logic in the compiler
# is not disturbed).  Output is a TSV of
#
#     sub<TAB>calls<TAB>incl_seconds<TAB>excl_seconds
#
# sorted by calls, for every sub in a Pl::* package (plus pl2cl's own).  Two
# consumers:
#   - tools/dup-census.pl --calls FILE tags each duplicate cluster's members
#     hot/cold, so an extraction on a per-token path ships with a compile-time
#     measurement and a cold one is free;
#   - a "where does compile time go" question (#213 material) reads the
#     excl_seconds column directly.
#
# Usage:
#   tools/sub-call-census.pl [--out calls.tsv] [--min-calls N] FILE.t ...
#   tools/sub-call-census.pl --sample 12         # every 9th perl-tests file
#
# Each file is transpiled ONCE by `perl -d:NYTProf pl2cl FILE` (stdout
# discarded); the per-file profiles are read one by one and summed per sub
# through Devel::NYTProf::Data.  Needs Devel::NYTProf (cpanm Devel::NYTProf).

use v5.20;
use strict;
use warnings;
use Getopt::Long;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);
use Cwd qw(abs_path);

my $root = abs_path("$RealBin/..");
my $out = '-';
my $min_calls = 1;
my $sample = 0;
GetOptions('out=s' => \$out, 'min-calls=i' => \$min_calls, 'sample=i' => \$sample)
  or die "usage: $0 [--out FILE] [--min-calls N] [--sample N] FILES...\n";

my @files = @ARGV;
if ($sample) {
  my @all = sort glob("$root/perl-tests/*.t");
  my $step = int(@all / $sample) || 1;
  @files = map { $all[$_ * $step] } grep { $_ * $step < @all } 0 .. $sample - 1;
}
die "$0: no files (give FILES or --sample N)\n" if !@files;

eval { require Devel::NYTProf::Data; 1 }
  or die "$0: Devel::NYTProf is not installed (cpanm Devel::NYTProf)\n";

my $tmp = tempdir(CLEANUP => 1);
my @profiles;
for my $i (0 .. $#files) {
  my $f = $files[$i];
  my $prof = "$tmp/nytprof.$i.out";
  local $ENV{NYTPROF} = "file=$prof:start=init:addpid=0:calls=1:slowops=0:stmts=0";
  # stdout is the emitted CL, which nobody wants here; stderr shows drops.
  my $rc = system("perl -d:NYTProf '$root/pl2cl' '$f' > /dev/null");
  if ($rc != 0 || !-s $prof) {
    warn "$0: skipped $f (rc=" . ($rc >> 8) . ")\n";
    next;
  }
  push @profiles, $prof;
  print STDERR "profiled " . ($i + 1) . "/" . @files . "  $f\n";
}
die "$0: nothing profiled\n" if !@profiles;

# Sum per sub across the per-file profiles in Perl (nytprofmerge on a dozen
# 200 MB profiles is not reliable; the sums are all this needs).
my %sum;
for my $prof (@profiles) {
  my $data = Devel::NYTProf::Data->new({ filename => $prof, quiet => 1 });
  my $map = $data->subname_subinfo_map;
  for my $si (values %$map) {
    my $name = $si->subname;
    next if $name !~ /^(?:Pl::|main::)/ || $name =~ /::CORE:/ || $name =~ /::BEGIN$/;
    my $r = $sum{$name} //= [0, 0, 0];
    $r->[0] += $si->calls; $r->[1] += $si->incl_time; $r->[2] += $si->excl_time;
  }
  unlink $prof;
}
my @rows = map { [$_, @{ $sum{$_} }] } grep { $sum{$_}[0] >= $min_calls } keys %sum;
@rows = sort { $b->[1] <=> $a->[1] } @rows;

my $fh;
if ($out eq '-') { $fh = \*STDOUT }
else { open $fh, '>', $out or die "$0: cannot write $out: $!\n" }
print $fh "# sub-call-census over " . scalar(@profiles) . " files: " . join(' ', map { s{^\Q$root\E/}{}r } @files) . "\n";
print $fh join("\t", qw(sub calls incl_s excl_s)), "\n";
printf $fh "%s\t%d\t%.4f\t%.4f\n", @$_ for @rows;
close $fh if $out ne '-';
print STDERR "wrote $out (" . scalar(@rows) . " subs)\n" if $out ne '-';

# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-27-real-csv-munger.pl
use strict; use warnings;
# CSV-ish munging with quoted fields, a hash of arrays, sorting, a formatted report.
sub parse_csv_line { my $l = shift; my @f; while ($l =~ /\G(?:"((?:[^"]|"")*)"|([^,]*))(?:,|$)/g) { my $v = defined $1 ? $1 : $2; last if !defined $v && pos($l) >= length($l); $v =~ s/""/"/g if defined $1; push @f, $v; last if pos($l) >= length($l) } @f }
my $hdr = <DATA>; chomp $hdr; my @cols = parse_csv_line($hdr); my (%dept, @recs);
while (<DATA>) { chomp; next if !length; my %r; @r{@cols} = parse_csv_line($_); $r{salary} =~ s/[^\d.]//g; push @recs, \%r; push @{ $dept{$r{dept}} }, \%r }
for my $d (sort keys %dept) { my @m = sort { $b->{salary} <=> $a->{salary} || $a->{name} cmp $b->{name} } @{ $dept{$d} }; my $sum = 0; $sum += $_->{salary} for @m;
  printf "%-6s n=%d avg=%9.2f top=%s\n", $d, scalar @m, $sum / @m, $m[0]{name}; }
my @quoted = grep { $_->{name} =~ /[,"]/ } @recs; print "quoted: ", join(" | ", map { $_->{name} } @quoted), "\n";
my %years; $years{ (split /-/, $_->{start})[0] }++ for @recs; print join(" ", map { "$_:$years{$_}" } sort { $a <=> $b } keys %years), "\n";
print join(",", map { my $n = $_; $n =~ /,/ ? qq("$n") : $n } map { $_->{name} } sort { $a->{start} cmp $b->{start} } @recs), "\n";
__DATA__
name,dept,salary,start
"Smith, Anna",eng,"$5,200.50",2019-03-01
Bob Stone,eng,4100,2021-07-15
"Carl ""CJ"" Jones",ops,3900.00,2019-11-30
Dana White,ops,3900,2020-01-02
Eve Black,sales,2800.75,2021-02-14

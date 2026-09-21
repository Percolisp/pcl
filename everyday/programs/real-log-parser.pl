# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-26-real-log-parser.pl
use strict; use warnings;
# A ~60-line "real" script: parse an access-log-like text, aggregate, report.
my %by_status; my %by_path; my %bytes_by_host; my @slow; my ($first_ts, $last_ts); my $bad = 0;
my %mon = do { my $i = 0; map { $_ => ++$i } qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec) };
while (my $line = <DATA>) {
  chomp $line; next if $line =~ /^\s*(#|$)/;
  my ($host, $ts, $method, $path, $status, $bytes, $ms) =
    $line =~ m{^(\S+) \S+ \S+ \[([^\]]+)\] "(\w+) (\S+) [^"]*" (\d{3}) (\d+|-) (\d+)ms$} or do { $bad++; next };
  $bytes = 0 if $bytes eq "-";
  my ($d, $mname, $y, $H, $M, $S) = $ts =~ m{^(\d+)/(\w+)/(\d+):(\d+):(\d+):(\d+)} or do { $bad++; next };
  my $key = sprintf "%04d-%02d-%02d %02d:%02d:%02d", $y, $mon{$mname}, $d, $H, $M, $S;
  $first_ts = $key if !defined $first_ts || $key lt $first_ts; $last_ts = $key if !defined $last_ts || $key gt $last_ts;
  $by_status{$status}++; (my $p = $path) =~ s/\?.*//; $by_path{$p}{hits}++; $by_path{$p}{ms} += $ms; $bytes_by_host{$host} += $bytes;
  push @slow, { path => $p, ms => $ms, host => $host } if $ms >= 500;
}
print "period: $first_ts .. $last_ts; malformed: $bad\n";
print "status: ", join(", ", map { "$_ x$by_status{$_}" } sort keys %by_status), "\n";
for my $p (sort { $by_path{$b}{hits} <=> $by_path{$a}{hits} || $a cmp $b } keys %by_path) {
  printf "%-12s %2d hits  avg %6.1f ms\n", $p, $by_path{$p}{hits}, $by_path{$p}{ms} / $by_path{$p}{hits};
}
my ($top) = sort { $bytes_by_host{$b} <=> $bytes_by_host{$a} } keys %bytes_by_host;
printf "top host: %s (%.1f KB)\n", $top, $bytes_by_host{$top} / 1024;
print "slow: ", join("; ", map { "$_->{path}\@$_->{host}=$_->{ms}" } sort { $b->{ms} <=> $a->{ms} } @slow), "\n";
my $total = 0; $total += $_ for values %by_status; my $err = 0; $err += $by_status{$_} for grep { /^[45]/ } keys %by_status;
printf "error rate: %.1f%% of %d\n", 100 * $err / $total, $total;
__DATA__
# host ident user [ts] "request" status bytes time
10.0.0.1 - - [19/Sep/2026:10:00:01 +0300] "GET /index.html HTTP/1.1" 200 5120 12ms
10.0.0.2 - bob [19/Sep/2026:10:00:03 +0300] "GET /api/items?id=7 HTTP/1.1" 200 20480 730ms
10.0.0.1 - - [19/Sep/2026:10:00:04 +0300] "POST /api/items HTTP/1.1" 201 64 95ms
this line is malformed
10.0.0.3 - - [19/Sep/2026:09:59:58 +0300] "GET /missing HTTP/1.1" 404 - 3ms
10.0.0.2 - bob [19/Sep/2026:10:01:10 +0300] "GET /api/items?id=8 HTTP/1.1" 500 128 1204ms
10.0.0.1 - - [19/Sep/2026:10:02:00 +0300] "GET /index.html HTTP/1.1" 304 0 2ms

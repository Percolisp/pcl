# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-32-string-ops-misc.pl
use strict; use warnings;
my $s = "  The quick  brown fox  "; (my $t = $s) =~ s/^\s+|\s+$//g; my @w = split ' ', $s; my $sq = $t =~ s/\s+/ /gr; my $cnt = () = $t =~ /o/g; my $tc = join " ", map { ucfirst lc } @w;
print "[$t] ", scalar(@w), " [$sq] $cnt [$tc] ", sprintf("%s", "abc" x 2), " ", "=" x 10, " ", lc("ABC") . uc("def"), " ", ucfirst(join("", reverse(split //, "olleh"))), " ", "a" lt "b" ? "lt" : "ge", " ", "abc" x -1, "|\n";
my $p = "key = value # comment"; my ($k, $v) = $p =~ /^\s*(\w+)\s*=\s*(.*?)\s*(?:#.*)?$/; my $csv = join ",", map { qq("$_") } qw(a b); my $padded = sprintf "%-8s|%8s|%08.3f", "l", "r", 3.14159; my $rep = "aaa" =~ s/a/b/r; my $n = ($p =~ tr/a-z//); my $pos = index($p, "="); my $sub = substr($p, $pos + 2, 5); my $centered = " " x ((20 - length $t) / 2) . $t;
print "$k=$v $csv $padded $rep $n $pos $sub [$centered] ", join("|", "a1b22c333" =~ /(\d+)/g), " ", join("|", split /\s*;\s*/, "x ; y;z ;"), " ", "hello world" =~ /^(\w+)\s+(\w+)$/ ? "$2 $1" : "nomatch", " ", sprintf("%vd", "1.22.333"), " ", join(",", map { sprintf "%s:%d", $_, length } qw(a bb)), " ", quotemeta("a.b*c"), " ", "x" . 1 + 2, "\n";

# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 290-sprintf -- harvested from s470/bo-agent-a4964c8ec026d7882/s470bo/d4.pl

my $ns = "plain";
my $u;
my $r = { a => 1 };
printf "exists non-ref     => %s\n", (defined(exists $ns->{k}) ? "[".(exists $ns->{k})."]" : 'undef');
printf "exists undef       => %s\n", (defined(exists $u->{k})  ? "[".(exists $u->{k})."]"  : 'undef');
printf "exists ref present => %s\n", (defined(exists $r->{a}) ? "[".(exists $r->{a})."]" : 'undef');
printf "exists ref absent  => %s\n", (defined(exists $r->{z}) ? "[".(exists $r->{z})."]" : 'undef');
my %h = (a=>1);
printf "exists hash absent => %s\n", (defined(exists $h{z}) ? "[".(exists $h{z})."]" : 'undef');

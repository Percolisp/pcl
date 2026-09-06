# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 263-regex -- harvested from s470/fable/bo/probe.pl

package A; sub hi { "A" } package B; sub hi { "B" } package main;
{ package C1; use parent qw(-norequire A); } { package C2; use parent -norequire, 'B'; } { package C3; use base ('A','B'); } { package C4; use parent qw(-norequire A B); }
print "isa: ", join(",", @C1::ISA), "|", join(",", @C2::ISA), "|", join(",", @C3::ISA), "|", join(",", @C4::ISA), " ", (C1->isa('-norequire') ? "BAD" : "ok"), " ", C3->hi, "\n";
sub takes { scalar @_ } my $n = takes((open my $fh, "+>", undef)); print "openundef: n=$n fh=", (defined $fh ? "def" : "undef"), "\n";
my $t = takes((my $q = 3)); print "myq: $t $q\n";
eval { my $x = 1/0 }; print "div: ", ($@ =~ /^Illegal division by zero/ ? "perl-text" : "other:$@"), "\n";
eval { my $x = 5 % 0 }; print "mod: ", ($@ =~ /^Illegal modulus zero/ ? "perl-text" : "other:$@"), "\n";
my %h = (a=>1); my $e = exists $h{nope}; print "exists: def=", (defined $e ? 1 : 0), " [", $e, "] ", (exists $h{a} ? 1 : 0), "\n";
my $s = "abc"; my $sub = substr($s, 10); print "substr: ", (defined $sub ? "def" : "undef"), "\n";
my @a = (1,2); my $ea = exists $a[5]; print "aexists: def=", (defined $ea ? 1 : 0), "\n";
print "data: ", scalar(<DATA>);
__DATA__
line1

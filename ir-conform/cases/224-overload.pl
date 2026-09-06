# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 224-overload -- harvested from s470/bk-agent-ad0b34477151560d8/s470bk/pov/m1004b.pl

package AoMut;
use overload '0+' => sub { $_[0]{v} }, '""' => sub { 'M(' . $_[0]{v} . ')' },
             '++' => sub { $_[0]{v} += 10; 'INCED' },
             '--' => sub { $_[0]{v} -= 10; 'DECED' }, fallback => 1;
sub new { return bless { v => $_[1] }, $_[0] }
package AoPlus;
use overload '0+' => sub { $_[0]{v} }, '""' => sub { 'P(' . $_[0]{v} . ')' },
             '+'  => sub { AoPlus->new($_[0]{v} + (ref($_[1]) ? $_[1]{v} : $_[1])) },
             '-'  => sub { my ($a,$b,$sw)=@_; my $o = ref($b) ? $b->{v} : $b;
                           AoPlus->new($sw ? $o - $a->{v} : $a->{v} - $o) },
             fallback => 1;
sub new { return bless { v => $_[1] }, $_[0] }
package main;
my $a = AoMut->new(5); $a++;
print ref($a), ":$a\n";
my $b = AoMut->new(5); my $c = ++$b;
print ref($b), ":$b c=$c\n";
my $d = AoMut->new(5); $d--;
print ref($d), ":$d\n";
my $g = AoPlus->new(5); $g++;
print ref($g), ":$g\n";

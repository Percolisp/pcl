# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 295-string-eval -- harvested from s470/fable/legs/evalblk.pl

my $curr_test = 56;
eval {
    print "ok $curr_test\n";
    die sprintf "ok %d\n", $curr_test + 2;
    1;
} || printf "ok %d\n$@", $curr_test + 1;
print "after: [", (defined $@ ? $@ : 'undef'), "]\n";
my @a = qw(a b c d);
my @b = eval @a;
print "b=@b err=[$@]\n";
my $r = eval { 42 } || 7; print "r=$r\n";
my $s = eval { die "boom\n"; 1 } || "caught"; print "s=$s\n";
sub f { eval { return 5 }; 9 } print "f=", f(), "\n";

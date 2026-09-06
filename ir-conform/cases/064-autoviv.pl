# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 064-autoviv -- harvested from s470/s471a-agent-ab8307152aef20823/guard6.pl

my @a = (1,2,{k=>"z"}); $a[-1]{k} .= "x";   print $a[-1]{k}, "\n";
my @b = (1,2,[3,4]);    $b[-1][0] += 5;      print $b[-1][0], "\n";
my @c = ([1,2],[3,4]);  $c[-1][-1] .= "z";   print $c[-1][-1], "\n";
my $r = [1,2,{k=>"z"}]; $r->[-1]{k} .= "y";  print $r->[-1]{k}, "\n";
my @d = ({k=>1},{k=>2}); $d[-1]{k} = 9;      print $d[-1]{k}, " ", scalar(@d), "\n";
my @e; $#e++; $e[-1]{HOST} = "beach";        print $e[0]{HOST}, " ", scalar(@e), "\n";

# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 292-sprintf -- harvested from s470/bo-agent-a4964c8ec026d7882/s470bo/d7.pl

my $s = "abc";
printf "d(10,2)=%d d(10)=%d d(-10,2)=%d d(3)=%d v(3)=[%s] v(1,-1)=[%s]\n",
  (defined substr($s,10,2)  ? 1 : 0),
  (defined substr($s,10)    ? 1 : 0),
  (defined substr($s,-10,2) ? 1 : 0),
  (defined substr($s,3)     ? 1 : 0),
  substr($s,3), substr($s,1,-1);

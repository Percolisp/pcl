# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 241-refs -- harvested from s470/s471a-agent-ab8307152aef20823/probes/C.pl

my %h = (k => [1,2]); $h{k}[-1] *= 2; print "h=@{$h{k}} #=$#{$h{k}}\n";

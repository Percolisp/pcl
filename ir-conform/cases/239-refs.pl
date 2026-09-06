# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 239-refs -- harvested from s470/bn-agent-a28fa0a7a0844c434/s470bn/p8-wr.pl

no strict 'refs';
our $w = 1; our @wa = (1,2); our %wh = (a => 1);
${'main::w'} = 5;      print $w;
${'main::w'} += 3;     print $w;
${'main::w'}++;        print $w;
${'main::w'} .= "z";   print $w;
@{'main::wa'} = (7,8); print "@wa";
%{'main::wh'} = (b=>2); print join(',', %wh);
print "\n";
PERL

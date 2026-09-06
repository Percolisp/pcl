# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 133-local -- harvested from s470/bn-agent-a28fa0a7a0844c434/s470bn/p3-local.pl

no strict 'refs';
our $g = 1;
sub show { print "in=", ${'main::g'}, " "; }
show();
{
  local $main::g = 7;
  show();
}
show();
# local through the symbolic spelling itself
{
  local ${'main::g'} = 9;
  show();
}
show();
our @ga = (1,2,3);
sub showa { print "a=", scalar(@{'main::ga'}), ":", join(',', @{'main::ga'}), " "; }
showa();
{
  local @main::ga = (4,5);
  showa();
}
showa();
our %gh = (a=>1);
sub showh { print "h=", join(',', map { "$_=$gh{$_}" } sort keys %{'main::gh'}), " "; }
showh();
{
  local %main::gh = (b=>2);
  showh();
}
showh();
print "\n";

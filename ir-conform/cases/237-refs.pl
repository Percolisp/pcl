# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 237-refs -- harvested from s470/bn-agent-a28fa0a7a0844c434/s470bn/p2.pl

no strict 'refs';
our $g = 2; our @ga = (1,2,3); our %gh = (a=>1);
sub foo { 42 }
my $s = 0;
$s += ${'main::g'};
$s += ${'g'};
$s += scalar(@{'main::ga'});
$s += ${'main::ga'}[1];
$s += ${'main::gh'}{'a'};
$s += $#{'main::ga'};
$s += (keys %{'main::gh'});
$s += &{'main::foo'}();
${'main::g'} = 5;
@{'main::ga'} = (9,8);
%{'main::gh'} = (b=>2);
*{'main::alias'} = \&foo;
my $pkg = 'main';
$s += ${"${pkg}::g"};
print "$s\n";

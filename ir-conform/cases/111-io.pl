# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 111-io -- harvested from s470/bo-agent-a4964c8ec026d7882/s470bo/dp2.pl

use Exporter;
my @all = <DATA>;
chomp @all;
print "n=", scalar(@all), " first=[$all[0]] last=[$all[-1]]\n";
__END__
alpha
beta

# comment
>%6. 6s<    >''<          >%6. 6s INVALID<
gamma
delta
epsilon
zeta
omega

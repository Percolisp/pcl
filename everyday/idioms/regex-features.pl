# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-12-regex-features.pl
no warnings;
my $s = "2026-09-19 key=val; k2=v2"; my ($y, $m) = $s =~ /^(\d+)-(\d+)/; my %kv = $s =~ /(\w+)=(\w+)/g; (my $t = $s) =~ s/(\d+)/<$1>/g; "ab" =~ /(?<first>a)(?<second>b)/; print "$y $m $kv{k2} $t $+{second} ", ($s =~ /KEY/i ? 1 : 0), " ", join("|", split /[;=\s]+/, "a=1; b=2"), "\n";

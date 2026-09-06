# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 324-string-eval -- harvested from s470/bi-agent-a4b9fedc2175a6513/s469bi/p1083/b.pl

sub probe {
    my ($name, $maker) = @_;
    my @s = ($maker->(), $maker->());
    print "$name: ", ($s[0] == $s[1] ? "same" : "different"), "\n";
}
# NOTE: each maker is itself a sub, so the inner sub is re-evaluated per call.
probe('plain',            sub { sub { 1 } });
probe('my-only',          sub { sub { my $x; $x } });
probe('subst-runtime-pat',sub { sub { my $x; s/$x/1/ } });
probe('re-eval-pragma',   sub { sub { use re "eval"; 1 } });
probe('re-eval+subst',    sub { sub { use re "eval"; my $x; s/$x/1/ } });
probe('ee',               sub { sub { s/1/1/ee } });
probe('e-only',           sub { sub { s/1/1/e } });
probe('str-eval',         sub { sub { eval "1" } });
probe('block-eval',       sub { sub { eval { 1 } } });

# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 259-regex -- harvested from s470/br-agent-ae6b0c4523801002e/s470br/br/warnre2.pl

my $n = 0;
$SIG{__WARN__} = sub {
    $n++;
    die "RUNAWAY at $n\n" if $n > 5;
    return if $_[0] =~ /^nothing/;
    print "H: \x{2019}\n";      # the handler prints a WIDE char to a byte STDOUT
};
print "W: \x{2019}\n";
print "handler calls: $n\n";

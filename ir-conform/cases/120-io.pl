# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 120-io -- harvested from s470/br-agent-ae6b0c4523801002e/s470br/p1116/probe.pl

print "1 before: ", (exists $INC{"./t.pl"} ? "yes" : "no"), "\n";
my $r = do "./t.pl";
print "2 do ret=", (defined $r ? $r : "U"), " v=$main::v\n";
print "3 INC{./t.pl}=", (exists $INC{"./t.pl"} ? ($INC{"./t.pl"} // "UNDEF") : "ABSENT"), "\n";
print "4 keys matching t.pl: ", join('|', grep { /t\.pl/ } keys %INC), "\n";
$main::v = 0;
my $q = require "./t.pl";
print "5 require ret=", (defined $q ? $q : "U"), " v=$main::v\n";
# a FAILED do
my $s = do "./nope-xyz.pl";
print "6 faildo ret=", (defined $s ? $s : "U"), " INC=", (exists $INC{"./nope-xyz.pl"} ? ($INC{"./nope-xyz.pl"} // "UNDEF") : "ABSENT"), "\n";
# a do of a file that COMPILES but dies
open my $f, '>', './dies.pl' or die; print $f "die \"boom\\n\";\n"; close $f;
my $u = do "./dies.pl";
print "7 diedo ret=", (defined $u ? $u : "U"), " err=[$@] INC=", (exists $INC{"./dies.pl"} ? ($INC{"./dies.pl"} // "UNDEF") : "ABSENT"), "\n";
# a do of a file that returns FALSE
open my $g, '>', './false.pl' or die; print $g "0;\n"; close $g;
my $w = do "./false.pl";
print "8 falsedo ret=", (defined $w ? $w : "U"), " INC=", (exists $INC{"./false.pl"} ? ($INC{"./false.pl"} // "UNDEF") : "ABSENT"), "\n";
my $w2 = eval { require "./false.pl" };
print "9 require-after-false ret=", (defined $w2 ? $w2 : "U"), " err=[$@]\n";

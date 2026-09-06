# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 258-regex -- harvested from s470/br-agent-ae6b0c4523801002e/s470br/br/pipebm.pl

my ( $in, $out );
pipe $in, $out;
binmode $out;
binmode $in,  ':utf8';
syswrite $out, "...\n";
my $line = 'ascii';
$line .= readline $in;
print "got=[$line]\n";

# and the other direction, which readline.t tests just above
my ( $in2, $out2 );
pipe $in2, $out2;
binmode $out2, ':utf8';
binmode $in2;
syswrite $out2, "abc\n";
my $l2 = readline $in2;
print "got2=[$l2]";
print "\n" unless $l2 =~ /\n\z/;

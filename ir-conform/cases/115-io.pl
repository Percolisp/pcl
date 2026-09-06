# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-conform case 115-io -- harvested from s470/br-agent-ae6b0c4523801002e/s470br/br/pipe3.pl

my $n = 0;
sub note { $n++; print "ok $n - $_[0]\n" }

{
    my $line = 'ascii';
    my ( $in, $out );
    pipe $in, $out;
    binmode $in;
    binmode $out;
    syswrite $out, "...\n";
    $line .= readline $in;
    note("ascii to ascii = [$line]");
}
{
    my $line = "\x{2080} utf8";
    my ( $in, $out );
    pipe $in, $out;
    binmode $out;
    binmode $in;
    syswrite $out, "...\n";
    $line .= readline $in;
    note("ascii to utf8 len=" . length($line));
}
{
    my $line = 'ascii';
    my ( $in, $out );
    pipe $in, $out;
    binmode $out;
    binmode $in,  ':utf8';
    syswrite $out, "...\n";
    $line .= readline $in;
    note("utf8 to ascii = [$line]");
}
print "done $n\n";

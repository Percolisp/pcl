# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-19-lc-uc-nonascii.pl
use strict; use warnings; use utf8; binmode(STDOUT, ":encoding(UTF-8)");
my $s = "Ärger Straße ÉCOLE ñandú"; print lc($s), "|", uc($s), "|", ucfirst(lc("élan")), "|", length($s), "|", scalar reverse("añb"), "\n";
print join(",", sort { lc($a) cmp lc($b) } qw(zebra Äpfel apple Zoo)), " ", ("é" =~ /^\w$/ ? "word" : "nonword"), " ", ("straße" =~ /STRASSE/i ? "fold" : "nofold"), " ", sprintf("%-6s|%3s|", "añ", "é"), " ", index("naïve", "ï"), " ", substr("日本語テキスト", 2, 3), " ", join("", map { sprintf "%04x", ord } split //, "aé€"), "\n";
my %h = ("ключ" => "значение"); print "$_=$h{$_}\n" for keys %h; print "\x{263A} ", chr(0x1F600), " ", ord("€"), " ", "caf\x{e9}" eq "café" ? "same" : "diff", " ", lc("ΣΑΣ"), "\n";

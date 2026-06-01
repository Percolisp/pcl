#!/usr/bin/perl

# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

use strict;
use warnings;

# Split a .lisp file into one /tmp/defun-FUNCNAME.lisp per top-level defun.
# Splits on lines starting with (defun at column 0. Does NOT count parens.
# Usage: perl split-lisp.pl FILENAME.lisp

my $file = shift or die "Usage: $0 FILE.lisp\n";
open my $fh, '<', $file or die "Cannot open $file: $!\n";

my ($name, @buf, @written);

while (<$fh>) {
    if (/^\(def\w+\s+(%?\S+)/) {
        # Flush previous chunk
        if ($name && @buf) {
            my $out = "/tmp/defun-$name.lisp";
            open my $wh, '>', $out or die "Cannot write $out: $!\n";
            print $wh @buf;
            push @written, $out;
        }
        ($name = $1) =~ s/[^A-Za-z0-9_+-]/_/g;
        @buf = ($_);
    } elsif ($name) {
        push @buf, $_;
    }
    # lines before any defun are silently skipped
}

# Flush last chunk
if ($name && @buf) {
    my $out = "/tmp/defun-$name.lisp";
    open my $wh, '>', $out or die "Cannot write $out: $!\n";
    print $wh @buf;
    push @written, $out;
}

print "Written:\n";
print "  $_\n" for @written;

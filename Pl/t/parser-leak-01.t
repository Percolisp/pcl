#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# The compiler must not leak across transpiles (task #128).
#
# `pl2cl --server` is a LONG-LIVED process: it answers one string-eval request
# per `eval "..."` in a running program, for the life of that program.  Anything
# a single transpile retains is therefore multiplied by the request count —
# s321/s365 caught servers holding 4.9 GB and 5.8 GB.  Measured s406, the cause
# was one self-referential closure in Pl/Parser2.pm's seam fix:
#
#     my $w;  $w = sub { … $w->(…) … };     # $w holds a CV that holds $w
#
# a reference cycle perl never collects, which leaked the closure AND its pad
# on every seam expression: ~8.5 kB per 40-character eval string, ~150 kB per
# 1.4 kB one, linear with no plateau over 600 transpiles.  `__SUB__` (feature
# current_sub, on under `use v5.30`) recurses without the self-reference.
#
# Two rows: the SHAPE must not come back anywhere in the compiler, and the
# BEHAVIOUR (a flat process after the warm-up) is checked directly.

use strict;
use warnings;
use Test::More tests => 2;
use FindBin qw($RealBin);
use lib "$RealBin/../..";

my $root = "$RealBin/../..";

# --- 1. the shape, everywhere in the compiler ------------------------------
# The COMPILER only — Pl/ and pl2cl, the code that runs under perl in the
# long-lived server.  NOT lib/: those shims are Perl that PCL transpiles and
# SBCL runs, where a closure cycle is collected like anything else, and where
# `__SUB__` inside an anonymous sub is a documented no-op stub
# (not-supported.md "__SUB__") — so the remedy this row names would turn a
# harmless idiom into a silent wrong there (s407 review).
{
    my @hits;
    for my $rel (glob("$root/Pl/*.pm"), glob("$root/Pl/*/*.pm"),
                 "$root/pl2cl") {
        open my $fh, '<', $rel or next;
        my @l = <$fh>;
        close $fh;
        for my $i (0 .. $#l) {
            next unless $l[$i] =~ /^\s*my\s+(\$\w+)\s*;\s*$/;
            my $var = $1;
            for my $j ($i + 1 .. ($i + 3 > $#l ? $#l : $i + 3)) {
                next unless $l[$j] =~ /^\s*\Q$var\E\s*=\s*sub\b/;
                # Self-referential only: the body must actually name it.  A
                # closure that never mentions the variable holding it is not a
                # cycle, so it is not this bug.
                my $body = join '', @l[$j .. ($j + 40 > $#l ? $#l : $j + 40)];
                push @hits, "$rel:" . ($i + 1) . " ($var)"
                    if $body =~ /\Q$var\E\s*->\s*\(/;
                last;
            }
        }
    }
    is_deeply(\@hits, [],
        'no self-referential closure (`my $w; $w = sub {… $w->() …}`) — use __SUB__')
        or diag("reference cycle, leaks the closure and its pad per call:\n  "
                . join("\n  ", @hits));
}

# --- 2. the behaviour: a warm compiler does not grow -----------------------
SKIP: {
    skip "no /proc/self/status (not Linux)", 1 unless -r "/proc/$$/status";
    require Pl::Parser2;

    my $code = 'my $v = 1; my @l = (1..3); my %h = (k => $v);'
             . ' my @o = map { $_ * 2 } @l; $o[0]';
    my $rss = sub {
        open my $f, '<', "/proc/$$/status" or return -1;
        while (<$f>) { return $1 if /^VmRSS:\s+(\d+)/ }
        return -1;
    };

    # pl2cl writes the program to stdout; the parse is what is under test.
    open my $devnull, '>', '/dev/null' or skip "no /dev/null", 1;
    my $old = select($devnull);
    Pl::Parser2->new(code => $code)->parse for 1 .. 50;   # warm: arenas settle
    my $before = $rss->();
    Pl::Parser2->new(code => $code)->parse for 1 .. 300;
    my $after = $rss->();
    select($old);
    close $devnull;

    # The bug leaked ~8.5 kB per transpile = ~2.5 MB over 300; a fixed compiler
    # is flat.  The bound is generous so ordinary allocator noise cannot fail
    # the row — anything that trips it is retention, not noise.
    cmp_ok($after - $before, '<', 1000,
        "300 transpiles grow the process by < 1 MB (got @{[ $after - $before ]} kB)");
}

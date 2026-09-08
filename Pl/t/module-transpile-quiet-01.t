#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# module-transpile-quiet-01.t — the compiler's OWN stderr during a transpile
# must be silent (task #1531).
#
# The runtime transpiles a `use`d module in a `pl2cl --module` child, and that
# child's stderr is part of the OBSERVED OUTPUT of whatever program is running:
# a gate row that captures `2>&1` fails on it, and a real program sees a
# warning it did not cause.  s473c split `_lower_block` into `_lower_block` +
# `_lower_block_1`; perl's "Deep recursion on subroutine" warning (depth 100,
# `warnings 'recursion'`, judged at the CALL SITE's lexical scope) then fired
# at the new call site, which sat outside the `no warnings 'recursion'` the
# original sub carried.  `_lower_block_1` recurses once PER STATEMENT (each
# `my` nests the block remainder), so any block of more than ~100 statements
# crossed it.  Two shipped modules do — lib/Fcntl.pm and lib/Errno.pm — and
# CI, whose module cache is always COLD, went red on fileio-02.t row 15 while
# every warm-cache gate stayed green (a warm cache never re-transpiles the
# module, so the warning never fires; that is why this file transpiles the
# modules DIRECTLY instead of relying on a cold cache).
#
# Rows 5–6 then found that the split was only the FIRST such site: three
# more per-statement recursions fire on the same input in program mode —
# `Pl::ClassicSort::_walk`, `Pl::Manifest::_walk` and `Pl::CLForm::_flat` +
# `to_string` — all tree walkers over the emitted form, whose nesting is one
# level per `my`.  Each carries the pragma at its own head now (the s403
# ruling's form: `no warnings 'recursion'` at the narrowest scope, never a
# blanket handler); probed silent up to 1000 statements in one block.
#
# Rows 1–4: the two real modules — exit 0 and a stderr with no perl-runtime
# diagnostic from the compiler (the `at .../Pl/....pm line N.` shape catches
# any warning or die of the compiler's own, not only this one).  Rows 5–6:
# the MECHANISM without the modules — a program of 150 top-level `my`
# statements, and the same inside a sub body (the two lowering paths), in
# program mode.  Inverse-verified on main aa58b3b7 (pre-fix): rows 2, 4, 5
# and 6 fail there, each naming the "Deep recursion" line.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";

plan skip_all => "pl2cl not found" if ! -x $pl2cl;
plan tests => 6;

# The first line of ERR that is a diagnostic of the COMPILER itself: perl's
# `... at <path>/Pl/<Module>.pm line N.` shape, or the recursion warning's
# own wording.  Returns '' when there is none.
sub compiler_noise {
    my ($err) = @_;
    for my $line (split /\n/, $err) {
        return $line if $line =~ /^Deep recursion on subroutine/;
        return $line if $line =~ m{ at \S*/Pl/\S+\.pm line \d+\.?$};
    }
    return '';
}

for my $mod (qw(Fcntl Errno)) {
    my $file = "$project_root/lib/$mod.pm";
    my ($cl, $err, $rc) = PCLCore::transpile_raw("$pl2cl --module $file");
    is($rc, 0, "lib/$mod.pm transpiles as a module (exit 0)");
    is(compiler_noise($err), '', "lib/$mod.pm: the compiler wrote no diagnostic of its own to stderr");
}

# The mechanism, module-free: N statements in one block = N nested
# `_lower_block_1` frames.  150 is safely past perl's depth-100 warning.
sub many_statements {
    my ($n, $wrap) = @_;
    my $body = join "\n", map { "my \$v$_ = $_;" } 1 .. $n;
    return $wrap
        ? "sub f {\n$body\nreturn \$v$n;\n}\nprint f(), \"\\n\";\n"
        : "$body\nprint \$v$n, \"\\n\";\n";
}

for my $case ([0, 'file top level'], [1, 'a sub body']) {
    my ($wrap, $label) = @$case;
    my ($fh, $src) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh many_statements(150, $wrap);
    close $fh;
    my ($cl, $err, $rc) = PCLCore::transpile_raw("$pl2cl < $src");
    is(compiler_noise($err), '',
       "150 statements in one block at $label: no deep-recursion (or other compiler) diagnostic")
        or diag("exit $rc");
}

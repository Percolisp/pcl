#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# gate-cost-01.t — the gate's own per-row cost machinery (s473u, #1544).
#
# The gate is this project's throughput constraint: a batch runs it six to nine
# times, and one run is ~2900 CPU-s.  The mechanisms that keep the per-row price
# down are the kind that stop working SILENTLY — the rows keep passing, they
# just cost hundreds of times more — so they are gated here rather than trusted.
#
# THE SAVED CORE.  Every sbcl command line comes from the ONE builder,
# tools/lib/PCLSbcl.pm via PCLCore::sbcl_prefix (task #344), which supplies the
# content-keyed core with the runtime already compiled in, plus the 512 MB
# control stack.  A file that spells `sbcl … --load cl/pcl-runtime.lisp` itself
# recompiles the whole runtime on every row: 2.89 CPU-s against 0.007 s from
# the core, measured s473u.  Fifteen files did, and they were 974 of the gate's
# 2933 CPU-s for 8 % of its rows — and they ran on the default 2 MB control
# stack, which is exactly the drift #344 exists to stop (#324 spent months
# measuring PCL on a 2 MB stack for the same reason).

use v5.30;
use strict;
use warnings;
use Test::More;
use FindBin;
use lib "$FindBin::Bin";
use PCLCore;

my @drift;
for my $f (sort glob("$FindBin::Bin/*.t")) {
    open my $fh, '<', $f or next;
    my @l = <$fh>;
    close $fh;
    for my $i (0 .. $#l) {
        next if $l[$i] =~ /^\s*#/;               # a comment may quote the shape
        next unless $l[$i] =~ /sbcl\b[^\n]*--load[^\n]*pcl-runtime\.lisp/;
        push @drift, (split m{/}, $f)[-1] . ':' . ($i + 1);
    }
}
is_deeply(\@drift, [],
    'no gate file spells its own sbcl runtime-source load')
    or diag("these recompile the runtime on every row (2.89 CPU-s each) and run\n"
          . "on the default 2 MB control stack.  Use the shared builder:\n"
          . "  my \@sbcl_rt = PCLCore::sbcl_prefix(\"\$FindBin::Bin/../../cl/pcl-runtime.lisp\");\n"
          . "  my \$out = `sbcl \@sbcl_rt --load \$file 2>&1`;\n  "
          . join("\n  ", @drift));

# The row above can only fail; this one says it was actually looking at files.
my @all_t = glob("$FindBin::Bin/*.t");
ok(scalar(@all_t) > 100, 'the scan saw the gate directory (not an empty glob)');

done_testing();

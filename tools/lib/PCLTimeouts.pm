package PCLTimeouts;
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# PCLTimeouts — the per-file TIMEOUT ALLOWANCE registry, shared by every
# measurement runner that kills a child on the clock.
#
# WHY A REGISTRY AT ALL: a file that TIMEOUTs contributes NO rows, so a file
# which merely needs longer than the default reads as a total loss and its
# passing rows evaporate invisibly — the #176 pack.t lesson.  The sweep answers
# that with a blind retry at 3x; where the need is KNOWN per file it belongs
# written down WITH ITS CAUSE, so the default run honours it and the allowance
# is reviewable.  The effective timeout is max(registry, --timeout), so raising
# --timeout on the command line still works.
#
# Registering a file is NOT a skip: it still runs, still reports, still fails
# the run if it diverges.  It is also NOT for hangs — this registry promises
# "give the file the time and it finishes", and a file that returns the SAME
# rows at 10x the budget is stuck, not slow (task #326/#345).
#
# FILE SHAPE (one per population, both read by this module):
#   rel <TAB> seconds <TAB> cause
#   baselines/perl-suite-timeouts.tsv   rel = t/op/foo.t          (companion)
#   baselines/cpan-board-timeouts.tsv   rel = <dist>/<t-file>     (CPAN board)
#
# A missing file is an EMPTY registry, and every caller reads "no entry" as
# "no information" — never as zero.

use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(read_timeouts timeout_for);

# path -> { rel => { secs => N, cause => TEXT } }
sub read_timeouts {
    my ($path) = @_;
    my %reg;
    return \%reg unless defined $path && -e $path;
    open my $fh, '<', $path or return \%reg;
    while (my $line = <$fh>) {
        chomp $line;
        next if $line =~ /^\s*(?:#|$)/;
        my ($rel, $secs, $cause) = split /\t/, $line, 3;
        next unless defined $secs && $secs =~ /^\d+$/;
        $reg{$rel} = { secs => $secs, cause => (defined $cause ? $cause : '') };
    }
    close $fh;
    return \%reg;
}

# The effective per-file timeout: the MAX of the registered allowance and the
# run's default, so a bigger --timeout still raises every file.
sub timeout_for {
    my ($reg, $rel, $default) = @_;
    my $e = $reg->{$rel} or return $default;
    return $e->{secs} > $default ? $e->{secs} : $default;
}

1;

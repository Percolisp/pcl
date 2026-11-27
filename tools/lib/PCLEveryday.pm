# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package PCLEveryday;
# PCLEveryday -- THE ONE READING OF A PROGRAM'S RESULT for tools/everyday-smoke.pl
# (task #2099) and the Rosetta classifier, tools/rosetta-corpus classify
# (task #2104, s543o).
#
# A run of tools/everyday-smoke.pl leaves, per program, <outdir>/<area>/<name>.out,
# .err and .rc; the corpus holds <corpus>/<area>/<name>.expect and, when perl
# exits non-zero, .rc.  The runner turns those into a VERDICT (same / DIFF /
# RC / TIMEOUT) and a first differing line; the classifier asks the same
# question of a finished run's outdir.  Two readers, one definition (CLAUDE.md
# rule 11): these subs were the runner's own until s543o and moved here
# unchanged, so the battery's verdicts cannot move by the move.
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(first_diff_line verdict_for expected_rc_of first_complaint TIMEOUT_RC);

# The exit status the runner WRITES into <outdir>/<area>/<name>.rc for a program
# it killed at --timeout (no process exits -1, so it cannot be a real status).
use constant TIMEOUT_RC => -1;

sub _slurp { my ($p) = @_; open my $h, '<:raw', $p or return undef; local $/; my $t = <$h>; close $h; return defined $t ? $t : '' }

# The 1-based number of the first STDOUT line that differs.  When one side is
# a prefix of the other, that is the first line the shorter one does not have.
sub first_diff_line {
    my ($want, $got) = @_;
    my @w = split /(?<=\n)/, $want;
    my @g = split /(?<=\n)/, $got;
    my $n = @w < @g ? @w : @g;
    for my $i (0 .. $n - 1) { return $i + 1 if $w[$i] ne $g[$i] }
    return $n + 1;
}

# The expected exit status of the program whose files are BASE.* (BASE.rc, or 0).
sub expected_rc_of { my ($base) = @_; my $t = _slurp("$base.rc"); return defined $t && $t =~ /(\d+)/ ? $1 : 0 }

sub verdict_for {
    my ($want, $want_rc, $got, $got_rc, $timed_out) = @_;
    return ('TIMEOUT', '-') if $timed_out;
    return ('DIFF', first_diff_line($want, $got)) if $got ne $want;
    return ('RC', '-') if $got_rc != $want_rc;
    return ('same', '-');
}

# PCL's first REAL complaint: the banner, the compiler's notes and SBCL's
# style warnings are not one.
sub first_complaint {
    my ($text) = @_;
    return '' if !defined $text;
    for my $l (split /\n/, $text) {
        next if $l !~ /\S/;
        next if $l =~ /^PCL Runtime loaded/ || $l =~ /^;/ || $l =~ /^WARNING/ || $l =~ /STYLE-WARNING/;
        $l =~ s/\s+$//;
        return $l;
    }
    return '';
}

1;

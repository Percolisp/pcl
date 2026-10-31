#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# verdict-register-01.t -- THE CAUSE COLUMN'S KEYS ARE CHECKED (s531b; Fable
# rulings s531 #2 and #3; docs/failure-cause-classes.md "The grammar").
#
# Every blessed failing row carries a CAUSE whose leading marker has a KEY:
# a task (`#N`, `UNREDUCED #N`, `HARNESS #N`), a not-supported id (`NS:<id>`),
# a parking key (`PARKED:<id>`).  A key that points at a CLOSED task or at a
# section that no longer exists is a WRONG RECORD -- the 13 JSON-PP rows that
# cited #1575 for five weeks after it closed were found by a reader's memory.
# This row finds them the day the key goes stale.
#
# WHAT IS CHECKED -- the KEY only, never every `#NNNN` in the note (ruling 2:
# 432 board cells mention a closed task somewhere, mostly as history; 21 had a
# bad KEY):
#   * every cause parses into the grammar (PCLCauses::parse_cause, no legacy);
#   * every NS:/PARKED: id exists in docs/not-supported.md and is not RETIRED
#     (tools/ns-ids.pl --check is clean);
#   * every task key is a file in the TREE's task store
#     (dot-claude-in-home-dir/tasks/pcl) with an `id`, whose status is not
#     completed / done;
#   * an UNREDUCED key's task names the t-file (the dumping-ground guard).
#
# GATE for baselines/cpan-board14-fails.tsv; REPORT (counts via note(), never
# a failing row) for perl-suite-fails.tsv, fail-baseline.tsv and
# row-shortfall.tsv until #1836's batch moves them (ruling 8).
#
# CLOSE-WITH-ROWS (ruling 3) is what makes this safe: a task is closed only in
# a commit that also leaves or re-causes every row keyed on it.
#
# WHERE IT IS AUTHORITATIVE: the task checks read the TREE's store, which in
# a git WORKTREE (an agent's, a review's) is a checkout as old as its branch
# -- so there they are ADVISORY (reported as TODO rows); in a clone (CI, the
# merge) they are the gate.  PCL_VERDICT_STRICT=1 makes them the gate
# anywhere; PCL_TASK_STORE=DIR reads another store (the live one:
# ~/.claude/tasks/pcl).  The grammar and id checks need no store and are the
# gate everywhere.  No SBCL: pure perl, ~1 s.

use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use lib "$RealBin/../../tools/lib";
use JSON::PP ();
use File::Basename qw(basename);
use PCLCauses qw(parse_cause baseline_causes);
use PCLNsIds qw(read_ids);

my $root   = "$RealBin/../..";
my $store  = $ENV{PCL_TASK_STORE} // "$root/dot-claude-in-home-dir/tasks/pcl";
my $strict = $ENV{PCL_VERDICT_STRICT} || !-f "$root/.git";   # .git is a FILE in a worktree

# ── the keys' three sources ─────────────────────────────────────────────────
my $ns = read_ids("$root/docs/not-supported.md");
is_deeply($ns->{problems}, [], 'docs/not-supported.md: every ## / ### heading has a unique stable id (tools/ns-ids.pl --check)')
  or diag(join "\n", @{ $ns->{problems} });

my %decided;
if (open my $fh, '<:raw', "$root/docs/DECIDED.md") {
    while (<$fh>) { $decided{$1} = 1 if /^## (s\d+[a-z0-9]*)\b/ }
}

# Task status, normalised: the store spells six statuses.
my %CLOSED = map { ($_ => 1) } qw(completed done);
my %OPEN   = map { ($_ => 1) } qw(pending open in_progress in-progress);
my %task;
sub task_problem {
    my ($n) = @_;
    return $task{$n} if exists $task{$n};
    my $path = "$store/$n.json";
    return $task{$n} = "task #$n does not exist ($path)" unless -e $path;
    my $j = eval { local $/; open my $f, '<:raw', $path or die "$!\n"; JSON::PP->new->utf8->decode(<$f>) };
    return $task{$n} = "task #$n is unreadable: $@" unless $j;
    return $task{$n} = "task #$n has no id field" unless defined $j->{id} && $j->{id} eq $n;
    my $st = $j->{status} // '(none)';
    return $task{$n} = "task #$n is $st (closed): re-cause the row (CLOSE-WITH-ROWS)" if $CLOSED{$st};
    return $task{$n} = "task #$n has an unknown status '$st'" unless $OPEN{$st};
    $task{"text$n"} = join ' ', $j->{subject} // '', $j->{description} // '';
    return $task{$n} = '';
}

sub id_problem {
    my ($id) = @_;
    my $s = $ns->{ids}{$id} or return "NS id $id does not exist in docs/not-supported.md";
    return "NS id $id is RETIRED" . (defined $s->{successor} ? " -> $s->{successor}" : '') if $s->{retired};
    return '';
}

# One row's problems, by kind: grammar / id / task / unreduced.
sub check_row {
    my ($cause, $tfile) = @_;
    my ($class, $key, $note, $legacy) = parse_cause($cause);
    my $short = substr($cause // '', 0, 90);
    return (grammar => "not in the grammar ($class, legacy spelling): $short") if $legacy;
    my %p;
    if ($class eq 'not-supported') { $p{id} = id_problem($key) }
    elsif ($class eq 'parked') {
        $p{id} = $key =~ /^ns-/ ? id_problem($key)
               : $decided{$key} ? '' : "PARKED key $key is not a DECIDED section label";
    }
    elsif ($class =~ /^(?:bug|unreduced|harness)$/) {
        $p{task} = task_problem($key);
        if ($class eq 'unreduced' && !$p{task} && defined $tfile) {
            my $b = basename($tfile);
            $p{unreduced} = "UNREDUCED #$key does not name $b in its task"
                unless index($task{"text$key"}, $b) >= 0;
        }
    }
    return map { $p{$_} ? ($_ => "$p{$_}: $short") : () } keys %p;
}

# ── run one population ──────────────────────────────────────────────────────
sub scan {
    my ($path, $format, $tcol) = @_;
    my $rows = baseline_causes($path, $format);
    return undef unless $rows;
    my (%prob, %keys);
    for my $r (@$rows) {
        my %p = check_row($r->{cause}, defined $tcol ? $r->{fields}[$tcol] : undef);
        for my $k (keys %p) {
            push @{ $prob{$k} }, "$r->{key}: $p{$k}";
            $keys{$k}{$1}++ if $p{$k} =~ /^(task #\d+|NS id \S+)/;
        }
    }
    return { rows => scalar(@$rows), prob => \%prob, keys => \%keys };
}

sub show { my ($list) = @_; my @l = @{ $list || [] }; return join("\n", @l[0 .. ($#l < 24 ? $#l : 24)]) . (@l > 25 ? "\n... and " . (@l - 25) . " more" : '') }

# ── GATE: the CPAN board ────────────────────────────────────────────────────
my $board = scan("$root/baselines/cpan-board14-fails.tsv", 'board', 1);
ok($board, 'the board baseline is readable') or BAIL_OUT('no board baseline');
note sprintf 'board: %d rows; the task checks are %s', $board->{rows},
             $strict ? 'AUTHORITATIVE' : 'ADVISORY (a git worktree: the tree\'s store is a stale checkout)';
is(scalar @{ $board->{prob}{grammar} || [] }, 0, 'board: every cause is in the grammar (a leading marker with its key)')
  or diag(show($board->{prob}{grammar}));
is(scalar @{ $board->{prob}{id} || [] }, 0, 'board: every NS: / PARKED: key is a live not-supported id (or a DECIDED label)')
  or diag(show($board->{prob}{id}));
TODO: {
    local $TODO = $strict ? undef : 'advisory in a git worktree: the task store is a checkout as old as the branch';
    is(scalar @{ $board->{prob}{task} || [] }, 0, 'board: every task key is an OPEN task in the store (CLOSE-WITH-ROWS)')
      or diag(show($board->{prob}{task}));
    is(scalar @{ $board->{prob}{unreduced} || [] }, 0, 'board: every UNREDUCED key\'s task names the t-file')
      or diag(show($board->{prob}{unreduced}));
}

# ── REPORT: the three other baselines (ruling 8: they move with #1836) ──────
for my $p (['companion (perl-suite-fails.tsv)', 'perl-suite-fails.tsv', 'companion'],
           ['perl-tests sweep (fail-baseline.tsv)', 'fail-baseline.tsv', 'sweep'],
           ['shortfall (row-shortfall.tsv)', 'row-shortfall.tsv', 'shortfall']) {
    my ($label, $file, $fmt) = @$p;
    my $s = scan("$root/baselines/$file", $fmt);
    if (!$s) { note "REPORT $label: baseline absent"; next }
    my %n = map { ($_ => scalar @{ $s->{prob}{$_} || [] }) } qw(grammar id task unreduced);
    my $closed  = grep { /closed/ }         @{ $s->{prob}{task} || [] };
    my $missing = grep { /does not exist/ } @{ $s->{prob}{task} || [] };
    my $idless  = grep { /has no id field/ } @{ $s->{prob}{task} || [] };
    my %tk; for (@{ $s->{prob}{task} || [] }) { $tk{$1}++ if /: task #(\d+) is \S+ \(closed\)/ }
    note sprintf "REPORT %s: %d rows -- %d not in the grammar, %d bad NS/PARKED keys, %d keyed on a CLOSED task "
               . "(%d tasks), %d on a MISSING task, %d on an id-less task file, %d other task problems",
               $label, $s->{rows}, $n{grammar}, $n{id}, $closed, scalar(keys %tk), $missing, $idless,
               $n{task} - $closed - $missing - $idless;
    my @top = (sort { $tk{$b} <=> $tk{$a} || $a <=> $b } keys %tk)[0 .. 9];
    note '  closed keys: ' . join(', ', map { "#$_ $tk{$_}" } grep { defined } @top) if %tk;
}
pass('the REPORT populations are counted, never a failing row (ruling 2)');

done_testing();

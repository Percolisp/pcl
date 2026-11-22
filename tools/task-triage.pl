#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# task-triage.pl -- count the open tasks by their TRIAGE class against
# docs/supported-subset.md (section 7), and print the number exit criterion 1
# asks for: open tasks that are IN the subset and marked silent wrong.
#
#   tools/task-triage.pl                  the counts
#   tools/task-triage.pl --list CLASS     the tasks of one class
#                                         (in | out | tooling | decision | untriaged | in-silent
#                                          | silent: every open task marked silent wrong)
#
# A task file carries two fields, written by the reviewing session:
#   "triage"         in | out | tooling | decision
#   "triage_reason"  one line
#
#   in        a bug or gap a program inside the subset can meet (silent wrongs,
#             dropped statements, loud failures, tidy errors, exit status)
#   out       outside the subset or outside the claim: section 4 of the subset
#             file, docs/not-supported.md, speed, message text, warnings, XS,
#             parked work, the board's widening
#   tooling   the project's own machinery: runners, baselines, harness,
#             instruments, structure work no program observes
#   decision  the USER's to rule before it can be IN or OUT
#
# "Marked silent wrong" is read from the task's TEXT, so a silent wrong whose
# task does not say so is not counted: the label is part of filing one.  The
# text is read by its CURRENT label, not by its history (task #5162): the
# SUBJECT first; then the NEWEST `LABEL:` line of the description -- each
# dated verdict block carries one since s543c (`LABEL: silent ...`, `LABEL:
# loud ...`, `LABEL: ... OUT ...`) -- and only a description with no LABEL
# line is searched whole for SILENT WRONG, as before.  So a task whose history
# says SILENT WRONG above a newer LOUD verdict is not counted.  An open task with no triage field
# is UNTRIAGED and is listed, never guessed.
#
# The store is dot-claude-in-home-dir/tasks/pcl in the tree (PCL_TASK_STORE
# overrides it, as in Pl/t/verdict-register-01.t).

use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/lib";
use PCLPaths;
use JSON::PP;

my $root  = PCLPaths::root($FindBin::Bin);
my $store = $ENV{PCL_TASK_STORE} // "$root/dot-claude-in-home-dir/tasks/pcl";
my %CLASS = map { $_ => 1 } qw(in out tooling decision);

my $list;
while (@ARGV) {
    my $a = shift @ARGV;
    if ($a eq '--list') {
        $list = shift @ARGV;
        die "task-triage.pl: --list takes in | out | tooling | decision | untriaged | in-silent | silent\n"
            if !defined $list || !($CLASS{$list} || $list =~ /^(?:untriaged|in-silent|silent)\z/);
    }
    else {
        die "task-triage.pl: unknown argument '$a'\n";
    }
}

opendir my $dh, $store or die "task-triage.pl: cannot read $store: $!\n";
my @files = sort { $a <=> $b } map { /^(\d+)\.json$/ ? $1 : () } readdir $dh;
closedir $dh;

my (%count, %rows, @bad);
for my $n (@files) {
    my $file = "$store/$n.json";
    open my $fh, '<:raw', $file or die "task-triage.pl: $file: $!\n";
    my $raw = do { local $/; <$fh> };
    close $fh;
    my $t = eval { JSON::PP->new->utf8->decode($raw) };
    if (!$t) { push @bad, $n; next }
    next if ($t->{status} // '') =~ /^(?:completed|done)$/;

    my $subject = $t->{subject} // $t->{title} // '';
    $subject =~ s/\s+/ /g;
    my $class = $t->{triage};
    if (defined $class && !$CLASS{$class}) {
        die "task-triage.pl: task $n has triage '$class' (legal: in, out, tooling, decision)\n";
    }
    $class //= 'untriaged';
    my $silent = silent_marked($subject, $t->{description} // '');

    $count{open}++;
    $count{$class}++;
    my $row = [ $n, $subject, $t->{triage_reason} // '' ];
    push @{ $rows{$class} }, $row;
    push @{ $rows{silent} }, $row if $silent;
    if ($class eq 'in' && $silent) {
        $count{'in-silent'}++;
        push @{ $rows{'in-silent'} }, $row;
    }
}

# Is the task marked silent wrong by its CURRENT label?  (See the header.)
sub silent_marked {
    my ($subject, $desc) = @_;
    return 1 if $subject =~ /silent[- ]wrong/i;
    my @labels = $desc =~ /^LABEL:[ \t]*(.*)$/mg;
    return $labels[-1] =~ /^silent\b/i ? 1 : 0 if @labels;
    return $desc =~ /SILENT[- ]WRONG/ ? 1 : 0;
}

binmode STDOUT, ':encoding(UTF-8)';
if (defined $list) {
    for my $r (@{ $rows{$list} || [] }) {
        printf "#%s\t%s\t%s\n", $r->[0], $r->[2], substr($r->[1], 0, 140);
    }
    exit 0;
}

printf "TRIAGE: %d open -- in %d, out %d, tooling %d, decision %d, untriaged %d\n",
    map { $count{$_} // 0 } qw(open in out tooling decision untriaged);
printf "IN and marked silent wrong (exit criterion 1): %d\n", $count{'in-silent'} // 0;
print "unreadable task files: @bad\n" if @bad;

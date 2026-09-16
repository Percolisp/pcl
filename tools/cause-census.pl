#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# cause-census.pl — "how many of the failures are from what we don't support?"
# (USER, s486; task #1782).  The failure-cause CLASS census over every blessed
# population PCL keeps: the perl-tests sweep, the companion (perl's own t/), the
# CPAN board, the companion's registered XDIFF divergences, and the row
# shortfall (rows a population expected and PCL never produced).
#
# Every row is classified by the ONE rule — PCLCauses::cause_class, six classes,
# recorded verbatim in docs/DECIDED.md §s486a and explained in
# docs/failure-cause-classes.md.  This tool adds no reading of its own: it only
# decides which rows to hand that function, and how to weight them.
#
#   tools/cause-census.pl             # the table
#   tools/cause-census.pl --markdown  # the same table for docs/STATUS.md
#   tools/cause-census.pl --hygiene   # every `other` row: population, key, cause
#
# WEIGHTING.  The sweep's baseline is a set of KEYS — (file, description) — and
# one key can stand for several failing rows of a run, so the honest count needs
# a run: when `.faillog/*.fails.tsv` is present its rows are joined to the
# baseline on that key and the census is ROW-weighted (the log's date is
# printed, because it is the date of the measurement).  Without a log the census
# is KEY-weighted and SAYS SO.  Both are legitimate; neither may be silently
# substituted for the other.
#
# THE SKIP REGISTRY (cl/skip-registry.lisp) relabels a matched failing row as
# `ok # skip`, so those rows are not-supported FAILURES that the sweep's
# headline fail count does not contain.  The sweep's denominator is therefore
# fails + registry-relabelled rows, read from `.faillog/_status.tsv`'s registry
# column when that column exists and printed as NOT COUNTED otherwise — never
# inferred, never zero by default.
#
# The baselines carry NUL bytes and non-UTF-8 text: every read is `:raw`, and
# `grep` on one needs `-a`.

use strict;
use warnings;
use FindBin;
use lib "$FindBin::RealBin/lib";
use PCLPaths ();
use PCLCauses qw(cause_class class_census census_line);
use Getopt::Long;

my ($MARKDOWN, $HYGIENE, $FAILLOG, $ROOT);
GetOptions('markdown' => \$MARKDOWN, 'hygiene' => \$HYGIENE,
           'faillog=s' => \$FAILLOG, 'root=s' => \$ROOT)
    or die "usage: $0 [--markdown] [--hygiene] [--faillog DIR] [--root DIR]\n";

my $root = $ROOT // PCLPaths::root($FindBin::RealBin);
my $bl   = "$root/baselines";
$FAILLOG //= "$root/.faillog";

# ── readers ────────────────────────────────────────────────────────────────
# One reader shape: hand back [ [key, cause], ... ] so the classifier and the
# hygiene list see the same rows.  A missing file is NO ROWS and says so at the
# report, never a silent zero.
sub rows_of {
    my ($path, $split) = @_;
    my @rows;
    open my $fh, '<:raw', $path or return undef;
    while (my $line = <$fh>) {
        next if $line =~ /^\s*#/ || $line !~ /\S/;
        chomp $line;
        my $r = $split->($line);
        push @rows, $r if $r;
    }
    close $fh;
    return \@rows;
}

# sweep: file <TAB> num <TAB> description <TAB> got <TAB> expected <TAB> cause
sub sweep_baseline {
    my @rows;
    my $r = rows_of("$bl/fail-baseline.tsv", sub {
        my ($f, $num, $d, $got, $exp, $c) = split /\t/, $_[0], 6;
        return undef unless defined $d;
        return ["$f\t$d", $c];
    });
    return $r;
}

# ── the sweep, row-weighted when a run's log is there ──────────────────────
# The join key is (file, description), the same one tools/sweep-diff.pl uses;
# a run row with no baseline row is DRIFT between the log and the baseline and
# is reported on its own, never folded into a class.
sub sweep_population {
    my $base = sweep_baseline() or return { missing => "$bl/fail-baseline.tsv" };
    my %cause; my %dups;
    for my $r (@$base) { $cause{$r->[0]} = $r->[1]; $dups{$r->[0]}++ }
    my @logs = sort glob "$FAILLOG/*.fails.tsv";
    if (!@logs) {
        return { weight => 'key', rows => $base, note =>
                 "no run log under $FAILLOG — KEY-weighted (one blessed line each)" };
    }
    my ($mtime) = (stat $logs[0])[9];
    for my $l (@logs) { my $m = (stat $l)[9]; $mtime = $m if $m > $mtime }
    my @t = localtime $mtime;
    my $when = sprintf "%04d-%02d-%02d", $t[5]+1900, $t[4]+1, $t[3];
    my (@rows, $orphan);
    for my $l (@logs) {
        my $lr = rows_of($l, sub {
            my ($f, $num, $d) = split /\t/, $_[0];
            return undef unless defined $d;
            return ["$f\t$d", undef];
        }) or next;
        for my $r (@$lr) {
            if (exists $cause{$r->[0]}) { push @rows, [$r->[0], $cause{$r->[0]}] }
            else { $orphan++ }
        }
    }
    return { weight => 'row', rows => \@rows, orphan => $orphan, when => $when,
             logs => scalar(@logs), keys => scalar(@$base),
             note => "ROW-weighted: " . scalar(@logs) . " .faillog files of "
                   . "$when joined to " . scalar(@$base) . " blessed lines"
                   . ($orphan ? "; $orphan run row(s) have no baseline row"
                              . " (log/baseline drift)" : '') };
}

# ── the registry column of .faillog/_status.tsv (s486b adds it) ────────────
# NEVER inferred: a column that is not there reads as NOT COUNTED, because a
# skip-registry relabel is a not-supported FAILURE the sweep's fail count does
# not contain, and guessing zero would understate the not-supported share.
sub registry_count {
    my $sf = "$FAILLOG/_status.tsv";
    return (undef, "no $sf") unless -e $sf;
    open my $fh, '<:raw', $sf or return (undef, "cannot read $sf");
    my @head;
    my $total = 0; my $seen = 0;
    while (my $line = <$fh>) {
        chomp $line;
        if ($line =~ /^#/) {
            # the header names its columns; the registry one is named there
            @head = split /\t/, ($line =~ s/^#\s*//r) if $line =~ /registry/i;
            next;
        }
        next unless $line =~ /\S/;
        my @f = split /\t/, $line, -1;
        if (@head) {
            my ($i) = grep { ($head[$_] // '') =~ /registry/i } 0 .. $#head;
            if (defined $i && defined $f[$i] && $f[$i] =~ /^\d+$/) {
                $total += $f[$i]; $seen++;
            }
        }
    }
    close $fh;
    return ($total, "$seen file(s)") if $seen;
    return (undef, "no registry column in $sf");
}

# ── the populations ────────────────────────────────────────────────────────
sub companion_population {
    my $r = rows_of("$bl/perl-suite-fails.tsv", sub {
        my ($rel, $num, $pv, $cv, $key, $c) = split /\t/, $_[0], 6;
        return undef unless defined $key;
        return ["$rel\t$key", $c];
    });
    return $r ? { weight => 'row', rows => $r,
                  note => 'one blessed line per diverging TAP row' }
              : { missing => "$bl/perl-suite-fails.tsv" };
}

sub board_population {
    my ($assert, $file) = (0, 0);
    my $r = rows_of("$bl/cpan-board14-fails.tsv", sub {
        my ($dist, $t, $num, $desc, $got, $exp, $c) = split /\t/, $_[0], 7;
        return undef unless defined $desc;
        (defined $desc && $desc eq '*FILE*') ? $file++ : $assert++;
        return ["$dist $t\t" . (length $desc ? $desc : "#$num"), $c];
    });
    return { missing => "$bl/cpan-board14-fails.tsv" } unless $r;
    return { weight => 'row', rows => $r,
             note => "$assert failing assertion(s) + $file *FILE* row(s)" };
}

# The registered XDIFF divergences: every one cites a not-supported.md section
# BY CONSTRUCTION (the reason column's rule — see the baseline's header), so the
# population is counted whole and each row carries its FILE's reason.
sub xdiff_population {
    my %reason;
    my $reg = rows_of("$bl/perl-suite-expected.tsv", sub {
        my ($rel, $why) = split /\t/, $_[0], 2;
        return undef unless defined $rel;
        $reason{$rel} = $why;
        return [$rel, $why];
    });
    return { missing => "$bl/perl-suite-expected.tsv" } unless $reg;
    my %files;
    my $r = rows_of("$bl/perl-suite-expected-rows.tsv", sub {
        my ($rel, $key) = split /\t/, $_[0], 2;
        return undef unless defined $key;
        $files{$rel}++;
        return ["$rel\t$key", $reason{$rel}];
    });
    return { missing => "$bl/perl-suite-expected-rows.tsv" } unless $r;
    return { weight => 'row', rows => $r,
             note => scalar(keys %files) . " file(s) with blessed rows, "
                   . scalar(@$reg) . " registered in perl-suite-expected.tsv" };
}

# The shortfall is weighted by its COUNT column: the unit is a row that was
# never produced, not a file.
sub shortfall_population {
    my ($half) = @_;
    my (@rows, %files);
    my $r = rows_of("$bl/row-shortfall.tsv", sub {
        my ($key, $n, $c) = split /\t/, $_[0], 3;
        return undef unless defined $n && $n =~ /^\d+$/;
        my $which = $key =~ m{^perl-tests/} ? 'perl-tests' : 't';
        return undef unless $which eq $half;
        $files{$key}++;
        push @rows, [$key, $c] for 1 .. $n;
        return undef;
    });
    return { missing => "$bl/row-shortfall.tsv" } unless defined $r;
    return { weight => 'row', rows => \@rows,
             note => scalar(keys %files) . " file(s)" };
}

# ── the report ─────────────────────────────────────────────────────────────
my $sweep = sweep_population();
my @pop = (
  ['perl-tests sweep'        => $sweep],
  ["companion (perl's own t/)"=> companion_population()],
  ['CPAN board (14 dists)'   => board_population()],
  ['companion XDIFF rows'    => xdiff_population()],
  ['shortfall: perl-tests'   => shortfall_population('perl-tests')],
  ["shortfall: perl's t/"    => shortfall_population('t')],
);

sub commify { my $n = reverse shift; $n =~ s/(\d{3})(?=\d)/$1,/g; return scalar reverse $n }

my (@table, @hygiene, %grand);
for my $p (@pop) {
    my ($label, $d) = @$p;
    if ($d->{missing}) {
        push @table, { label => $label, missing => $d->{missing} };
        next;
    }
    my @causes = map { $_->[1] } @{ $d->{rows} };
    my $n = class_census(\@causes);
    $grand{$_} += $n->{$_} for keys %$n;
    push @table, { label => $label, n => $n, note => $d->{note},
                   total => scalar(@causes) };
    for my $r (@{ $d->{rows} }) {
        push @hygiene, [$label, $r->[0], $r->[1]] if cause_class($r->[1]) eq 'other';
    }
}

# The SHARE: not-supported + parked over everything that is a PCL failure.
# perl-skip is excluded from the denominator — perl skips the file too, so the
# row is not PCL's failure to answer.
sub share {
    my ($n) = @_;
    my $denom = 0;
    $denom += $n->{$_} // 0 for grep { $_ ne 'perl-skip' } @PCLCauses::CLASSES;
    return ('n/a', 0) unless $denom;
    return (sprintf("%.1f%%", 100 * (($n->{'not-supported'} // 0)
                                   + ($n->{parked} // 0)) / $denom), $denom);
}

my ($reg, $reg_note) = registry_count();

if ($MARKDOWN) {
    print "| population | rows | not-supported | parked | bug | other | unexplained | perl-skip | not-supported + parked |\n";
    print "|---|---:|---:|---:|---:|---:|---:|---:|---:|\n";
    for my $t (@table) {
        if ($t->{missing}) {
            printf "| %s | *baseline absent: %s* | | | | | | | |\n", $t->{label}, $t->{missing};
            next;
        }
        my ($sh) = share($t->{n});
        printf "| %s | %s | %s | %s | %s | %s | %s | %s | %s |\n", $t->{label},
            commify($t->{total}), map({ commify($t->{n}{$_} // 0) }
              qw(not-supported parked bug other unexplained perl-skip)), $sh;
    }
    my ($gsh, $gden) = share(\%grand);
    my $gtot = 0; $gtot += $grand{$_} // 0 for @PCLCauses::CLASSES;
    printf "| **all populations** | **%s** | **%s** | **%s** | **%s** | **%s** | **%s** | **%s** | **%s** |\n",
        commify($gtot), map({ commify($grand{$_} // 0) }
          qw(not-supported parked bug other unexplained perl-skip)), $gsh;
    print "\nSkip-registry relabels in the sweep: ",
          defined $reg ? commify($reg) . " row(s) ($reg_note)"
                       : "NOT COUNTED ($reg_note)", ".\n";
}
else {
    print "FAILURE-CAUSE CLASS CENSUS — docs/failure-cause-classes.md (the rule: DECIDED \xc2\xa7s486a)\n";
    print "root: $root\n\n";
    for my $t (@table) {
        if ($t->{missing}) {
            printf "%-26s BASELINE ABSENT: %s\n\n", $t->{label}, $t->{missing};
            next;
        }
        my ($sh) = share($t->{n});
        printf "%s\n  %s", $t->{label}, census_line('  CAUSES', $t->{n});
        printf "  not-supported + parked: %s of the rows that are PCL's to answer\n", $sh;
        printf "  (%s)\n\n", $t->{note} if $t->{note};
    }
    my ($gsh) = share(\%grand);
    printf "ALL POPULATIONS\n  %s", census_line('  CAUSES', \%grand);
    printf "  not-supported + parked: %s of the rows that are PCL's to answer\n\n", $gsh;
    print "registry: ", defined $reg
        ? commify($reg) . " skip-registry relabel(s) in the sweep ($reg_note)"
          . " — not-supported FAILURES the sweep's fail count does not contain\n"
        : "NOT COUNTED ($reg_note)\n";
}

if ($HYGIENE) {
    printf "\nHYGIENE — %d `other` row(s): a cause citing no not-supported.md"
         . " section and no task.\nEach should either gain the NS: section it"
         . " rests on or a task number.\n\n", scalar(@hygiene);
    my %seen;
    for my $h (@hygiene) {
        my $k = join "\t", $h->[0], $h->[2] // '';
        next if $seen{$k}++;
        printf "  %-26s %s\n      %s\n", $h->[0], $h->[1], $h->[2] // '(none)';
    }
    printf "\n  (%d distinct cause text(s) over %d row(s))\n",
           scalar(keys %seen), scalar(@hygiene);
}

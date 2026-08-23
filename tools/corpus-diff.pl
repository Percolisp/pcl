#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# corpus-diff.pl — standing-rule #1 verification: byte-diff the emitted CL
# of the WORKING TREE against a git ref (default HEAD) over the perl-tests
# corpus, and report exactly which files' emission changed.
#
# Usage:
#   tools/corpus-diff.pl                    # working tree vs HEAD, whole corpus
#   tools/corpus-diff.pl HEAD~1             # vs another ref
#   tools/corpus-diff.pl HEAD f1.t f2.t     # subset of perl-tests files
#   tools/corpus-diff.pl --show ...         # also print the diff hunks
#                                           # (first 60 lines/file; --show=all
#                                           # for everything, --show=N to cap)
#   tools/corpus-diff.pl --ws ...           # compare with ALL whitespace
#                                           # stripped — the verifier for
#                                           # whitespace-only emission steps
#                                           # (E2 layout changes).  Caveat:
#                                           # also equates whitespace-only
#                                           # changes INSIDE string literals,
#                                           # so back it up with the test gate.
#
# For an E1 de-gate session the acceptance is: ONLY the de-gated files
# appear in the output, and each diff is explained.  Exit status: 0 when
# identical, 1 when files differ (so it can gate scripts).
#
# Every run also prints the SILENT-DROP count on both sides — the number of
# `(progn ;; PARSE ERROR: … nil)` forms, i.e. statements the compiler could
# not lower and replaced with nil.  They are invisible at run time, so a
# change that adds one otherwise shows up nowhere; task #343 and
# baselines/parse-error-drop-census-s399.tsv have the population-wide census.
#
# What it gets right that ad-hoc reruns keep getting wrong (s287):
#   - the SAME input files (this tree's perl-tests/) are fed to BOTH
#     compilers — a ref that changed perl-tests must not pollute the diff;
#   - BOTH repo roots are normalized to `ROOT` — the ref runs from a temp
#     worktree whose own absolute path is embedded in @INC/pl2cl-path
#     forms, and the working tree embeds this repo's path; missing either
#     mapping makes every file "differ";
#   - the pipeline marker line (`;;; pcl: pipeline=… gen=…`) is stripped
#     in perl, not grep (emitted CL can contain NUL bytes, which silence
#     grep) — the cache generation string lives there and always differs
#     across an emission-changing commit;
#   - the temp worktree is always removed (END block), even on die.
#
# THE SHAPES POPULATION (task #496, s440): after the corpus verdict, the same
# two compilers run over Pl/t/shapes/*.pl -- deliberately-awkward files that
# exist only to exercise grammar no real population contains (lifted from the
# s438 guard files; README there).  Reported on ITS OWN line, never mixed into
# the corpus count; a shapes diff fails the run like a corpus diff.  A file
# list given on the command line skips the shapes (a subset run is a probe).
use strict;
use warnings;
use File::Temp   qw(tempdir);
use File::Path   qw(make_path);
use FindBin      qw($RealBin);
use Cwd          qw(abs_path);

my $root = abs_path("$RealBin/..");
chdir $root or die "chdir $root: $!";

my $show;   # undef = filenames only; 0 = unlimited; N = first N diff lines
my $strip_ws = 0;
for my $i (reverse 0 .. $#ARGV) {
    if ($ARGV[$i] eq '--ws') { $strip_ws = 1; splice @ARGV, $i, 1; next }
    next unless $ARGV[$i] =~ /^--show(?:=(\w+))?$/;
    $show = !defined $1 ? 60 : $1 eq 'all' ? 0 : $1 + 0;
    splice @ARGV, $i, 1;
}

my $ref = (@ARGV && $ARGV[0] !~ /\.t$/) ? shift @ARGV : 'HEAD';
system("git rev-parse --verify --quiet \Q$ref\E^{commit} >/dev/null") == 0
    or die "not a commit: $ref\n";

my @files = @ARGV ? map { m{/} ? $_ : "perl-tests/$_" } @ARGV
                  : sort glob("perl-tests/*.t");
-f $_ or die "no such file: $_\n" for @files;

my $tmp = tempdir("pcl-corpus-diff-XXXXXX", TMPDIR => 1, CLEANUP => 1);
my $wt  = "$tmp/ref-tree";
system("git worktree add --quiet \Q$wt\E \Q$ref\E") == 0
    or die "git worktree add failed\n";
END {
    # local $? — a system() in an END block otherwise overwrites the
    # process exit status (perlmod: END and $?).
    local $?;
    if (defined $wt && -d $wt) {
        system("git -C \Q$root\E worktree remove --force \Q$wt\E >/dev/null 2>&1");
    }
}

make_path("$tmp/new", "$tmp/ref");

sub transpile {
    my ($pl2cl_dir, $out_dir) = @_;
    for my $f (@files) {
        (my $base = $f) =~ s{.*/}{};
        # Inputs always come from THIS tree; only the compiler differs.
        system("cd \Q$pl2cl_dir\E && ./pl2cl < \Q$root/$f\E > \Q$out_dir/$base\E.lisp 2>/dev/null");
    }
}
transpile($root, "$tmp/new");
transpile($wt,   "$tmp/ref");

my $norm = sub {
    my ($path) = @_;
    open my $fh, '<', $path or return '';
    local $/;
    my $t = <$fh>;
    $t =~ s/^;;; pcl: pipeline=.*\n//m;
    $t =~ s/\Q$wt\E/ROOT/g;
    $t =~ s/\Q$root\E/ROOT/g;
    if ($strip_ws) {
        # `;` line comments must go BEFORE whitespace-stripping: their only
        # terminator is the newline, and layout changes move/merge them.
        # (Comment text inside string literals is over-stripped — acceptable
        # for a whitespace-step verifier; the test gate backs this up.)
        $t =~ s/^\s*;.*//mg;       # whole-line comments
        $t =~ s/\s;;.*//g;         # trailing comments
        $t =~ s/\s+//g;            # all whitespace
        $t =~ s/\)\(/)\n(/g;       # quasi-lines so --show diffs stay readable
    }
    return $t;
};

my @changed;
for my $f (@files) {
    (my $base = $f) =~ s{.*/}{};
    push @changed, $base
        if $norm->("$tmp/new/$base.lisp") ne $norm->("$tmp/ref/$base.lisp");
}

# SILENT-DROP counter (task #343).  A `(progn ;; PARSE ERROR: … nil)` is a
# statement the compiler could not lower and replaced with nil — invisible at
# RUN time, so neither the gate nor the sweep notices when a change adds one
# (perl-tests/bless.t carries one today and it is a test row that never runs,
# in a file the sweep reports as passing).  Both emissions are already in
# hand here, so counting them is free; the population-wide version is
# tools/drop-census.pl against baselines/parse-error-drop-census-s399.tsv.
my $drops = sub {
    my ($dir) = @_;
    my $n = 0;
    for my $f (@files) {
        (my $base = $f) =~ s{.*/}{};
        open my $fh, '<', "$dir/$base.lisp" or next;
        local $/;
        my $t = <$fh> // '';
        $n += () = $t =~ /;; PARSE ERROR:/g;
    }
    return $n;
};
my ($d_ref, $d_new) = ($drops->("$tmp/ref"), $drops->("$tmp/new"));
if ($d_ref != $d_new) {
    printf "SILENT DROPS: %d -> %d (%+d) — each is a statement replaced by nil;"
         . " explain every added one (task #343)\n", $d_ref, $d_new, $d_new - $d_ref;
} else {
    printf "silent drops: %d, unchanged\n", $d_new;
}

if (@changed) {
    printf "%d of %d files differ vs %s:\n", scalar @changed, scalar @files, $ref;
    print "  $_\n" for @changed;
    if (defined $show) {
        for my $base (@changed) {
            for my $side (qw(ref new)) {
                open my $fh, '>', "$tmp/$side.norm" or die "write $side.norm: $!";
                print $fh $norm->("$tmp/$side/$base.lisp");
                close $fh;
            }
            # -a: emitted CL can contain NUL bytes; without it diff calls the
            # files binary and prints no hunks (same trap as grep, see header).
            my @lines = qx(diff -a -u \Q$tmp\E/ref.norm \Q$tmp\E/new.norm);
            splice @lines, 0, 2;   # drop the +++/--- tempfile header
            my $n = @lines;
            splice @lines, $show if $show && $n > $show;
            print "\n=== $base (-ref +new, $n diff lines"
                . ($show && $n > $show ? ", first $show shown" : '') . ")\n";
            print @lines;
        }
    } else {
        print "(inspect: diff the normalized outputs; every changed file must be explained)\n";
    }
}
else {
    printf "emission identical to %s across %d files\n", $ref, scalar @files;
}

# ---- the SHAPES population (own line; skipped when a file subset was given)
my $shapes_differ = 0;
if (!@ARGV) {
    my @shapes = sort glob("Pl/t/shapes/*.pl");
    make_path("$tmp/new-shapes", "$tmp/ref-shapes");
    my %seen;
    for my $f (@shapes) {
        (my $base = $f) =~ s{.*/}{};
        die "shapes: duplicate basename $base" if $seen{$base}++;
        system("cd \Q$root\E && ./pl2cl < \Q$root/$f\E > \Q$tmp/new-shapes/$base\E.lisp 2>/dev/null");
        system("cd \Q$wt\E   && ./pl2cl < \Q$root/$f\E > \Q$tmp/ref-shapes/$base\E.lisp 2>/dev/null");
    }
    my @sch = grep { $norm->("$tmp/new-shapes/$_.lisp") ne $norm->("$tmp/ref-shapes/$_.lisp") }
              map { (my $b = $_) =~ s{.*/}{}; $b } @shapes;
    if (@sch) {
        $shapes_differ = 1;
        printf "SHAPES: %d of %d files differ vs %s (Pl/t/shapes, task #496): %s\n",
            scalar @sch, scalar @shapes, $ref, join(' ', @sch);
        if (defined $show) {
            for my $base (@sch) {
                for my $side (qw(ref new)) {
                    open my $sfh, '>', "$tmp/$side.norm" or die "write $side.norm: $!";
                    print $sfh $norm->("$tmp/$side-shapes/$base.lisp");
                    close $sfh;
                }
                my @lines = qx(diff -a -u \Q$tmp\E/ref.norm \Q$tmp\E/new.norm);
                splice @lines, 0, 2;
                my $nl = @lines;
                splice @lines, $show if $show && $nl > $show;
                print "\n=== shapes/$base (-ref +new, $nl diff lines"
                    . ($show && $nl > $show ? ", first $show shown" : '') . ")\n";
                print @lines;
            }
        }
    } else {
        printf "shapes: %d files identical (Pl/t/shapes)\n", scalar @shapes;
    }
}
exit((@changed || $shapes_differ) ? 1 : 0);

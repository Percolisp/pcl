#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-doc-examples-01.t -- the one-liners the user-facing documents print
# are what `pcl` prints, and what perl prints.  So the documents cannot drift
# from the command.
#
# THE MARKER.  An example is checked when the line before its fenced block
# starts with
#     <!-- doc-example
# (the rest of the comment is free text; GitHub does not render it).  Two
# shapes follow a marker:
#   * a ```console block: each line `$ COMMAND` is run, and the lines after
#     it, up to the next `$ ` line or the fence, are its expected STDOUT;
#   * a ```bash block followed by a ```console block (the README's quick
#     start): the bash lines that start with `pcl ` or `./pcl ` are run, and
#     the console block is their expected STDOUT, together.
# Each COMMAND runs under `sh -c` in an empty temporary directory with STDIN
# from /dev/null, once with this tree's `pcl` first on PATH and once with a
# `pcl` that IS perl ($^X), so an example where PCL and perl differ fails
# here.  `./pcl` is read as `pcl`.  Each document must carry at least the
# number of checked commands named below, so removing a marker fails too.

use v5.30;
use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use File::Temp qw(tempdir);
use Cwd qw(abs_path);

my $root = abs_path("$RealBin/../..");
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

# document => the least number of checked commands it must hold
my %DOCS = ('docs/pcl-commands.md' => 5, 'README.md' => 2);

sub slurp {
    my ($f) = @_;
    open my $h, '<:raw', $f or die "$f: $!";
    local $/;
    return scalar <$h>;
}

# The fenced blocks after each marker -> list of [command, expected stdout].
sub examples {
    my ($text) = @_;
    my @lines = split /\n/, $text, -1;
    my @ex;
    for (my $i = 0; $i < @lines; $i++) {
        next if $lines[$i] !~ /^<!-- doc-example/;
        my ($lang, $body, $next) = fence(\@lines, $i + 1);
        die "marker at line " . ($i + 1) . " is not followed by a fenced block\n"
            if !defined $lang;
        if ($lang eq 'console') {
            my $cur;
            for my $l (@$body) {
                if ($l =~ /^\$ (.*)/) { push @ex, $cur = [$1, ''] ; next }
                die "console block at line " . ($i + 1) . " has output before a command\n" if !$cur;
                $cur->[1] .= "$l\n";
            }
        }
        elsif ($lang eq 'bash') {
            my ($lang2, $out) = fence(\@lines, $next);
            die "bash example at line " . ($i + 1) . " is not followed by a console block\n"
                if !defined $lang2 || $lang2 ne 'console';
            my @cmds = grep { /^(?:\.\/)?pcl / } @$body;
            die "bash example at line " . ($i + 1) . " runs no pcl line\n" if !@cmds;
            push @ex, [join(' && ', @cmds), join('', map { "$_\n" } @$out)];
        }
        else { die "marker at line " . ($i + 1) . ": a ```$lang block cannot be checked\n" }
    }
    for my $e (@ex) {
        $e->[0] =~ s{^\./pcl }{pcl };
        $e->[0] =~ s{&& \./pcl }{&& pcl }g;
    }
    return @ex;
}

# fence(\@lines, $from) -> (lang, [body lines], index after the closing fence)
# for the first fenced block at or after $from (only blank lines may precede it).
sub fence {
    my ($lines, $i) = @_;
    $i++ while $i < @$lines && $lines->[$i] =~ /^\s*$/;
    return if $i >= @$lines || $lines->[$i] !~ /^```(\w+)\s*$/;
    my $lang = $1;
    my @body;
    for ($i++; $i < @$lines; $i++) {
        return ($lang, \@body, $i + 1) if $lines->[$i] =~ /^```\s*$/;
        push @body, $lines->[$i];
    }
    return;
}

# A directory whose `pcl` is perl: the oracle leg.
my $perlbin = tempdir(CLEANUP => 1);
symlink($^X, "$perlbin/pcl") or die "symlink: $!";

sub run_with {
    my ($bin, $cmd) = @_;
    my $dir = tempdir(CLEANUP => 1);
    local $ENV{PATH} = "$bin:$ENV{PATH}";
    my $pid = open(my $h, '-|') // die "fork: $!";
    if (!$pid) {
        chdir $dir or die;
        open STDIN, '<', '/dev/null' or die;
        open STDERR, '>', '/dev/null' or die;
        exec 'sh', '-c', $cmd or die "exec sh: $!";
    }
    local $/;
    my $out = <$h> // '';
    close $h;
    return ($out, $? >> 8);
}

for my $doc (sort keys %DOCS) {
    my @ex = examples(slurp("$root/$doc"));
    ok(@ex >= $DOCS{$doc}, "$doc carries at least $DOCS{$doc} checked commands (found " . scalar(@ex) . ")");
    for my $e (@ex) {
        my ($cmd, $want) = @$e;
        my ($got, $st) = run_with($root, $cmd);
        is($got, $want, "$doc: pcl prints what the document says: $cmd");
        my ($pgot) = run_with($perlbin, $cmd);
        is($pgot, $want, "$doc: perl prints it too: $cmd");
    }
}

done_testing();

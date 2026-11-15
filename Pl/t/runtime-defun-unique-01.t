#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# runtime-defun-unique-01.t -- no function or macro of the hand-written
# runtime is DEFINED TWICE in one package (s540c).
#
# The runtime is one 40,000-line file plus the TAP layer and the XS bridge,
# loaded into the same image.  A second `(defun NAME ...)` in the same package
# silently REPLACES the first for every caller -- SBCL loads it without a word.
# s540c's flock helper `%p-file-id (fd)` (a file's device and inode) and
# s539d's `%p-file-id (path)` (the interned id of a source file name) were
# written in parallel and met at the rebase: the later one won, so every
# `p-file' form died with a raw type error (Pl/t/die-location-01.t and
# caller-fidelity-01.t were the only rows that noticed).  Two batches picking
# the same name is ordinary; this row makes the meeting loud at once.
#
# The scan reads top-level `(defun NAME' / `(defmacro NAME' / `(defgeneric
# NAME' lines (NAME may be `(setf X)'), keyed on the package the last
# `(in-package ...)' selected -- the same short name in two packages (the
# `pl-import' stubs of the core-module packages) is two definitions.  NAMES
# compare exactly: the runtime is read with readtable-case :invert, under
# which `%p--s-impl' (-s) and `%p--S-impl' (-S) are two symbols.

use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);

my $root  = "$RealBin/../..";
my @files = map { "$root/cl/$_" } qw(pcl-runtime.lisp pcl-test.lisp pcl-xs.lisp);

# Returns the list of "PKG NAME at FILE:LINE (first at FILE:LINE)" duplicates
# in the given (label, text) pairs, read in order as one image.
sub duplicates {
    my @src = @_;
    my (%first, @dups);
    while (my ($label, $text) = splice @src, 0, 2) {
        my $pkg  = 'pcl';
        my $line = 0;
        for (split /\n/, $text) {
            $line++;
            $pkg = lc $1 if /^\(in-package :?"?([^\s")]+)/;
            next if !/^\(def(?:un|macro|generic) (\(setf \S+?\)|\S+)/;
            my $key = "$pkg $1";
            if ($first{$key}) { push @dups, "$key at $label:$line (first at $first{$key})" }
            else              { $first{$key} = "$label:$line" }
        }
    }
    return @dups;
}

# 1: the scanner itself finds a same-package duplicate and nothing else (a
# row that could never fail would prove nothing about row 2).
my @probe = duplicates(
    'a', "(defun %x (fd) fd)\n(defun (setf %y) (v o) v)\n(in-package :carp)\n(defun %x () nil)\n",
    'b', "(in-package :pcl)\n(defmacro %z () nil)\n(defun (setf %y) (v o) o)\n(defun %x (path) path)\n");
is_deeply(\@probe,
          ['pcl (setf %y) at b:3 (first at a:2)', 'pcl %x at b:4 (first at a:1)'],
          'the scanner finds a duplicate in one package, not across packages');

# 2: the runtime as it is.
my @src;
for my $f (@files) {
    open my $h, '<', $f or die "$f: $!";
    local $/;
    (my $label = $f) =~ s{^.*/}{};
    push @src, $label, scalar <$h>;
}
my @dups = duplicates(@src);
is(scalar(@dups), 0, 'no function or macro of cl/pcl-{runtime,test,xs}.lisp is defined twice in one package')
    or diag(join "\n", @dups);

done_testing;

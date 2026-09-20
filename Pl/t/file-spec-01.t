#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# file-spec-01.t — s481b: `File::Spec->canonpath` in PCL's shim.
#
# PCL ships `lib/File/Spec.pm`, a Unix-only subset of the real module (its own
# header says "keep it in sync with the real File::Spec::Unix as methods are
# needed").  `canonpath` was one of the missing ones, and it is not a corner:
# `Path::Tiny::_path` — the constructor EVERY Path::Tiny object goes through —
# calls it, so all 30 of that dist's test files died at load with
#
#     Can't locate object method "canonpath" via package "File::Spec"
#
# where perl runs 29 of them for 1779 assertions (#1607's measurement, s481b).
#
# canonpath TIDIES a path textually and resolves nothing: `..` stays put except
# at the very front of an absolute path, where perl collapses it because `/`
# has no parent.  The expectations below are real perl 5.40.3's File::Spec,
# case for case, and the two `..` rows are the ones a "simplify the path"
# guess gets wrong.

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
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 7;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = PCLCore::transpile("$pl2cl $pl_file");
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# One launch, twelve cases: every line is real perl's answer for that input.
is(run_cl(<<'PERL'), <<'OUT',
use File::Spec;
for my $c ('a//b', 'a/./b', './a/b', '/../../x', '/..', 'a/b/', '/', './',
           'foo/../bar', '', 'a///./b//', '/a/./../b') {
  printf "[%s] -> [%s]\n", $c, File::Spec->canonpath($c);
}
PERL
[a//b] -> [a/b]
[a/./b] -> [a/b]
[./a/b] -> [a/b]
[/../../x] -> [/x]
[/..] -> [/]
[a/b/] -> [a/b]
[/] -> [/]
[./] -> [.]
[foo/../bar] -> [foo/../bar]
[] -> []
[a///./b//] -> [a/b]
[/a/./../b] -> [/a/../b]
OUT
   'canonpath agrees with real File::Spec 5.40.3 on twelve paths');

is(run_cl(<<'PERL'), "undef\n",
use File::Spec;
my $r = File::Spec->canonpath(undef);
print defined $r ? "defined" : "undef", "\n";
PERL
   'canonpath(undef) returns undef, as File::Spec::Unix does (`return unless defined`)');

# The Path::Tiny shape that found this: a constructor calling canonpath.
is(run_cl(<<'PERL'), "a/b\n",
use File::Spec;
package Tiny;
sub new { my ($c, $p) = @_; bless { canon => File::Spec->canonpath($p) }, $c }
sub canon { $_[0]{canon} }
package main;
print Tiny->new('./a//./b/')->canon, "\n";
PERL
   'a class constructor may call it as a class method, which is how Path::Tiny does');

# Inverse: the methods that were already there still answer the same.
is(run_cl(<<'PERL'), "/ . .. a/b 1 \n",
use File::Spec;
print File::Spec->rootdir, " ", File::Spec->curdir, " ", File::Spec->updir,
      " ", File::Spec->catfile('a', 'b'),
      " ", File::Spec->file_name_is_absolute('/x'),
      " ", File::Spec->file_name_is_absolute('x'), "\n";
PERL
   'inverse: rootdir / curdir / updir / catfile / file_name_is_absolute unchanged');

# ── s492c, task #2007: catdir DROPPED the leading empty component ──────────
# `catdir("", "tmp")` was "tmp" where perl says "/tmp" — the empty part at the
# front is what says ROOT.  File::Temp builds a tempdir's parent as
# `catdir($volume, @dirs[0..$#dirs-1])` over a split absolute path, so the
# commonest call in the module, `tempdir(CLEANUP => 1)`, died "Parent
# directory (tmp) does not exist".  catdir/catfile are now File::Spec::Unix's
# own shape (join with a trailing empty part, then canonpath), and abs2rel is
# the real algorithm.  Expectations are real perl 5.40.3's.
is(run_cl(<<'PERL'), "[/tmp] [/tmp] [a/b] [/] [] [/etc/passwd] [a/b/c]\n",
use File::Spec;
my @d = File::Spec->splitdir("/tmp/XXXX");
print "[", File::Spec->catdir("", "", "tmp"), "] [", File::Spec->catdir("", @d[0..$#d-1]),
      "] [", File::Spec->catdir("a", "", "b"), "] [", File::Spec->catdir(""),
      "] [", File::Spec->catdir(), "] [", File::Spec->catfile("", "etc", "passwd"),
      "] [", File::Spec->catfile("a", "b", "c"), "]\n";
PERL
   'catdir keeps the LEADING empty component, which is root (perl-probed)');

is(run_cl(<<'PERL'), "[b/c] [../c] [.] [b]\n",
use File::Spec;
print "[", File::Spec->abs2rel("/a/b/c", "/a"), "] [", File::Spec->abs2rel("/a/c", "/a/b"),
      "] [", File::Spec->abs2rel("/a", "/a"), "] [", File::Spec->abs2rel("/a/b", "/a"), "]\n";
PERL
   'abs2rel exists and is File::Spec::Unix\'s algorithm');

is(run_cl(<<'PERL'), "dir file name-ok\n",
use File::Temp qw(tempdir tempfile);
my $d = tempdir(CLEANUP => 1);
my ($fh, $fn) = tempfile("pcl-gXXXXXX", TMPDIR => 1, UNLINK => 1, SUFFIX => ".txt");
print +(-d $d ? "dir" : "no-dir"), " ", (-f $fn ? "file" : "no-file"), " ",
      ($fn =~ m{^/tmp/pcl-g\w{6}\.txt$} ? "name-ok" : $fn), "\n";
PERL
   'File::Temp::tempdir/tempfile work again (they died in catdir)');

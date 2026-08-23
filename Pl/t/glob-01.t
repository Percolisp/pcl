#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Tests for file glob <*.txt> and glob() function
# Glob expands file patterns and returns matching files

use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $pl2cl = './pl2cl';
my $runtime = 'cl/pcl-runtime.lisp';
# Cached-core prefix (#518): loading the runtime SOURCE per row recompiles it
# (~1.2 s per spawn) and is the one variable the CI flake pointed at — every
# sibling runs the saved core through this same one-place prefix.
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

# Create temp directory with test files
my $tmpdir = tempdir(CLEANUP => 1);
for my $name (qw(a.txt b.txt c.log d.txt sub)) {
  if ($name eq 'sub') {
    mkdir "$tmpdir/$name";
  } else {
    open my $fh, '>', "$tmpdir/$name" or die $!;
    close $fh;
  }
}

# Helper to run transpiled code and capture output
sub run_pcl {
  my ($code) = @_;

  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl_code = PCLCore::transpile(qq{$pl2cl --no-cache $pl_file});

  my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp');
  print $cl_fh $cl_code;
  close $cl_fh;

  my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;

  # Filter SBCL noise
  $output =~ s/^;.*\n//gm;
  $output =~ s/^\s*\n//gm;
  $output =~ s/PCL Runtime loaded\n?//g;
  $output =~ s/STYLE-WARNING.*\n//g;

  unlink $pl_file, $cl_file;

  return $output;
}

# ============================================================
# Transpilation Tests
# ============================================================

# Test: Basic glob transpilation
{
  my $code = 'my @f = <*.txt>;';
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl = PCLCore::transpile(qq{$pl2cl $pl_file});

  like($cl, qr/p-glob.*"\*\.txt"/, 'glob <*.txt> generates p-glob call');

  unlink $pl_file;
}

# Test: Glob with path
{
  my $code = 'my @f = </tmp/*.log>;';
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl = PCLCore::transpile(qq{$pl2cl $pl_file});

  like($cl, qr/p-glob.*"\/tmp\/\*\.log"/, 'glob </tmp/*.log> generates p-glob with path');

  unlink $pl_file;
}

# Test: Readline still works (not confused with glob)
{
  my $code = 'my $line = <STDIN>;';
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl = PCLCore::transpile(qq{$pl2cl $pl_file});

  like($cl, qr/p-readline/, '<STDIN> still generates p-readline');
  unlike($cl, qr/p-glob/, '<STDIN> does NOT generate p-glob');

  unlink $pl_file;
}

# Test: Variable filehandle still works
{
  my $code = 'my $line = <$fh>;';
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl = PCLCore::transpile(qq{$pl2cl $pl_file});

  like($cl, qr/p-readline.*\$fh/, '<$fh> generates p-readline');
  unlike($cl, qr/p-glob/, '<$fh> does NOT generate p-glob');

  unlink $pl_file;
}

# ============================================================
# Runtime Tests
# ============================================================

# Test: Basic glob in list context
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/*.txt>;
print scalar(\@files);
END_CODE

  like($output, qr/3/, 'glob *.txt returns 3 .txt files');
}

# Test: Glob returns sorted list
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/*.txt>;
print join(",", sort \@files);
END_CODE

  like($output, qr/a\.txt.*b\.txt.*d\.txt/s, 'glob returns files (sorted)');
}

# Test: Glob with no matches returns empty
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/*.xyz>;
print "count:" . scalar(\@files);
END_CODE

  like($output, qr/count:0/, 'glob with no matches returns empty array');
}

# Test: Glob with ? wildcard
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/?.txt>;
print scalar(\@files);
END_CODE

  like($output, qr/3/, 'glob with ? wildcard works');
}

# Test: Multiple wildcards
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/*.*>;
print scalar(\@files);
END_CODE

  # Should match a.txt, b.txt, c.log, d.txt = 4 files
  like($output, qr/4/, 'glob *.* matches all files with extensions');
}

# Test: Glob in scalar context (returns one file)
{
  my $output = run_pcl(<<"END_CODE");
my \$file = <$tmpdir/*.txt>;
print defined \$file ? "got one" : "undef";
END_CODE

  like($output, qr/got one/, 'glob in scalar context returns first match');
}

# ============================================================
# Corner Cases
# ============================================================

# Test: Glob with brackets []
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/[ab].txt>;
print scalar(\@files);
END_CODE

  like($output, qr/2/, 'glob with [ab] character class works');
}

# Test: Distinguish glob from readline by content
{
  my $code = q{
    my $x = <STDIN>;   # readline
    my @f = <*.pm>;    # glob
  };
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl = PCLCore::transpile(qq{$pl2cl $pl_file});

  like($cl, qr/p-readline.*STDIN/, 'STDIN recognized as readline');
  like($cl, qr/p-glob.*\*\.pm/, '*.pm recognized as glob');

  unlink $pl_file;
}

# Test: Empty <> is readline (not glob)
{
  my $code = 'my $line = <>;';
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl = PCLCore::transpile(qq{$pl2cl $pl_file});

  like($cl, qr/p-readline\)/, 'empty <> is readline');
  unlike($cl, qr/p-glob/, 'empty <> is NOT glob');

  unlink $pl_file;
}

# ============================================================
# Additional Edge Cases
# ============================================================

# Test: Variable interpolation in glob pattern
{
  my $output = run_pcl(<<"END_CODE");
my \$dir = "$tmpdir";
my \@files = <\$dir/*.txt>;
print scalar(\@files);
END_CODE

  like($output, qr/3/, 'glob with variable interpolation works');
}

# Test: Hidden files (dotfiles)
{
  # Create a dotfile
  open my $fh, '>', "$tmpdir/.hidden" or die $!;
  close $fh;

  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/.*>;
print scalar(\@files);
END_CODE

  # Should match .hidden (and possibly . and ..)
  like($output, qr/[123]/, 'glob .* matches hidden files');

  unlink "$tmpdir/.hidden";
}

# Test: glob() function form (not angle brackets)
{
  my $code = 'my @f = glob("*.txt");';
  my ($fh, $pl_file) = tempfile(SUFFIX => '.pl');
  print $fh $code;
  close $fh;

  my $cl = PCLCore::transpile(qq{$pl2cl $pl_file});

  like($cl, qr/p-glob.*"\*\.txt"/, 'glob() function generates p-glob call');

  unlink $pl_file;
}

# Test: glob() function runtime
{
  my $output = run_pcl(<<"END_CODE");
my \@files = glob("$tmpdir/*.txt");
print scalar(\@files);
END_CODE

  like($output, qr/3/, 'glob() function returns correct count');
}

# Test: Relative path glob
{
  # Run from tmpdir context
  my $output = run_pcl(<<"END_CODE");
chdir("$tmpdir");
my \@files = <*.txt>;
print scalar(\@files);
END_CODE

  like($output, qr/3/, 'glob with relative path works');
}

# Test: No wildcards - literal filename
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/a.txt>;
print scalar(\@files);
END_CODE

  like($output, qr/1/, 'glob with literal filename returns 1 match');
}

# Test: No wildcards - nonexistent file.
#
# EXPECTATION REWRITTEN s438i (task #450), because it asserted PCL's old
# behaviour rather than perl's.  perl returns a pattern that holds no glob
# METACHARACTER as ITSELF, whether or not anything of that name exists —
# probed on 5.40.3:
#
#     perl -e 'my @f = </tmp/nonexistent-xyz.txt>; print scalar(@f), " @f"'
#     1 /tmp/nonexistent-xyz.txt
#
# so `count:0` was the one answer perl never gives.  The row now asserts the
# COUNT and the VALUE, which is strictly more than it asserted before.
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/nonexistent.txt>;
print "count:" . scalar(\@files) . " [\@files]";
END_CODE

  my $want = "count:1 [$tmpdir/nonexistent.txt]";
  like($output, qr/\Q$want\E/,
       'glob with a nonexistent LITERAL file returns the pattern itself (perl)');
}

# Test: Glob used directly in foreach
{
  my $output = run_pcl(<<"END_CODE");
my \$count = 0;
for my \$f (<$tmpdir/*.txt>) {
    \$count++;
}
print \$count;
END_CODE

  like($output, qr/3/, 'glob works directly in foreach');
}

# Test: Glob in boolean context (if)
{
  my $output = run_pcl(<<"END_CODE");
if (<$tmpdir/*.txt>) {
    print "found";
} else {
    print "empty";
}
END_CODE

  like($output, qr/found/, 'glob in boolean context works');
}

# Test: Glob with no matches in boolean context
{
  my $output = run_pcl(<<"END_CODE");
if (<$tmpdir/*.xyz>) {
    print "found";
} else {
    print "empty";
}
END_CODE

  like($output, qr/empty/, 'glob with no matches is false');
}

# Test: Multiple glob calls in same expression
{
  my $output = run_pcl(<<"END_CODE");
my \@all = (<$tmpdir/*.txt>, <$tmpdir/*.log>);
print scalar(\@all);
END_CODE

  like($output, qr/4/, 'multiple globs can be combined');
}

# Test: Glob result used with grep
{
  my $output = run_pcl(<<"END_CODE");
my \@files = grep { /\\/a\\.txt\$/ } <$tmpdir/*.txt>;
print scalar(\@files);
END_CODE

  # Should match only a.txt (ends with /a.txt).  EXACT assertion (#518): the
  # CI flake showed qr/1/ can only say "some 1 appeared somewhere in
  # stdout+stderr" — when it failed there was no telling what the output WAS.
  is($output, "1", 'glob result can be filtered with grep');
}

# Test: Character range in brackets (now expanded by p-glob)
{
  # Create c.txt for this test
  open my $fh, '>', "$tmpdir/c.txt" or die $!;
  close $fh;

  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/[a-c].txt>;
print scalar(\@files);
END_CODE

  # Should match a.txt, b.txt, c.txt = 3 files
  like($output, qr/3/, 'glob with [a-c] character range works');

  unlink "$tmpdir/c.txt";
}

# Test: Negated character class [!d] (handled at transpile time)
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/[!d].txt>;
print scalar(\@files);
END_CODE

  # Should match a.txt, b.txt (not d.txt) - 2 files since c.txt was cleaned up
  # Wait, we have a.txt, b.txt, d.txt from setup. c.log not c.txt.
  # So [!d].txt should match a.txt, b.txt = 2 files
  like($output, qr/2/, 'glob with [!d] negated class works');
}

# Test: Negated class with caret [^d]
{
  my $output = run_pcl(<<"END_CODE");
my \@files = <$tmpdir/[^d].txt>;
print scalar(\@files);
END_CODE

  like($output, qr/2/, 'glob with [^d] negated class works');
}

# Regression (defins.t test 16): a list-assignment whose RHS is a lone glob, used
# as a while-condition, must run glob as a SCALAR iterator (one file per loop) not
# list context (all files at once).  glob is wrapped (p-list-ctx ...) on
# a p-list-= RHS, so p-glob must fall back to scalar mode when *p-in-list-assign-rhs*
# is set — mirroring p-readline's handling of while (($x) = <FH>).
{
  my $output = run_pcl(<<"END_CODE");
my (\$seen, \$dummy, \$name) = (0, '', '');
while ((\$seen ? \$dummy : \$name) = glob("$tmpdir/*.txt")) { \$seen++; last if \$seen > 99 }
print \$seen;
END_CODE

  # a.txt, b.txt, d.txt = 3 .txt files; iterator visits each once.
  like($output, qr/\b3\b/, 'while (($x)=glob) iterates one file per loop, not all at once');
}

# Companion: @a = glob stays LIST context (p-array-=, no scalar fallback).
{
  my $output = run_pcl(<<"END_CODE");
my \@a = glob("$tmpdir/*.txt");
print scalar(\@a);
END_CODE

  like($output, qr/\b3\b/, '@a = glob still returns all matches at once');
}


# Test: bare `*` matches names WITH extensions and directories (CL's pathname
# `*` wrongly requires no extension — regression for the directory-listing fix).
{
  my $output = run_pcl(<<"END_CODE");
my \@all = glob("$tmpdir/*");
print scalar(\@all);
END_CODE

  like($output, qr/\b5\b/, 'glob(*) matches extensioned files + dirs (a.txt b.txt c.log d.txt sub)');
}

# Test: bare `glob` (no argument) defaults to \$_.
{
  my $output = run_pcl(<<"END_CODE");
\$_ = "$tmpdir/*.txt";
my \@f = glob;
print scalar(\@f);
END_CODE

  like($output, qr/\b3\b/, 'bare glob defaults to \$_');
}

# Task #499 (s440, found by the SHAPES corpus, Pl/t/shapes/punct-arrays-glob.pl):
# a LIST-context glob is never stateful -- perl returns the full list on EVERY
# call (perlfunc glob; only scalar context iterates).  PCL kept a per-pattern
# :list-done mark and answered full, EMPTY, full, ... so `glob($p)` in a loop
# or in a sub called twice lost every second result.  Three call shapes, and
# the expectation is the live perl answer for the same source (both_agree).
{
  my $code = <<"END_CODE";
my \@n; for (1..3) { my \@f = glob("$tmpdir/*.txt"); push \@n, scalar(\@f); }
sub cnt { my \@f = glob("$tmpdir/*.txt"); scalar \@f }
my \@x = glob("$tmpdir/*.txt"); my \@y = glob("$tmpdir/*.txt");
my \@v = <$tmpdir/*.txt>; my \@w = <$tmpdir/*.txt>;
print "loop=\@n sub=", cnt(), cnt(), " sites=", scalar(\@x), scalar(\@y),
      " angle=", scalar(\@v), scalar(\@w), "\\n";
END_CODE
  my ($pfh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
  print $pfh $code; close $pfh;
  my $perl = `perl $pl 2>&1`;
  like($perl, qr/^loop=3 3 3 sub=33 sites=33 angle=33$/m, 'the oracle: perl answers the full list every time');
  is(run_pcl($code), $perl, '#499: a list-context glob returns the full list on every call (loop, sub, two sites, <>)');
}

# ... and the SCALAR-context iterator is untouched by a list glob of the same
# pattern in the loop body (perl keys the iterator by call site, #489; the
# list call must not disturb it).
{
  my $code = <<"END_CODE";
my \$c = 0; while (my \$f = glob("$tmpdir/*.txt")) { \$c++; my \@i = glob("$tmpdir/*.txt"); print scalar(\@i); }
print " c=\$c\\n";
END_CODE
  my ($pfh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
  print $pfh $code; close $pfh;
  my $perl = `perl $pl 2>&1`;
  like($perl, qr/^333 c=3$/m, 'the oracle: perl iterates 3 times, the inner list glob is full each time');
  is(run_pcl($code), $perl, '#499: a scalar-context glob loop survives a list glob of the same pattern inside it');
}


done_testing();

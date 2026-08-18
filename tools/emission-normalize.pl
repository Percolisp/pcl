#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later
#
# emission-normalize.pl — read emitted CL, rewrite the shapes an emission
# change is EXPECTED to move, and print one canonical flat form per top-level
# form; two emissions that differ only by those shapes normalize to identical
# text (s410 ask 7.7 (a); the Phase A bar of docs/plan-one-compiler-s411.md).
#
# Usage:
#   tools/emission-normalize.pl [--rule NAME]... FILE.lisp > out.txt
#   tools/emission-normalize.pl --diff A.lisp B.lisp     # normalize both, diff
#   tools/emission-normalize.pl --corpus REF [--rule …]  # every perl-tests
#        file: working tree vs REF, both normalized; prints the files that
#        still differ (exit 1) — corpus-diff.pl for a known emission change
#
# Rules (default: all):
#   elem-setf         (setf (p-gethash|p-aref C K) V)  →  (p-setf (… C K) V)
#   insensitive-call  (let ((*wantarray* X)) (pl-F …)) →  (pl-F …)
#                     — a *wantarray* bind whose whole body is ONE user-sub
#                     call (head `pl-…`, optionally package-qualified) is
#                     dropped on BOTH sides, so a bind that only one side
#                     emits cannot be told apart from one both emit; the
#                     sweep is the oracle for whether a dropped bind was
#                     legal, this tool only proves that NOTHING ELSE moved.
#   ctx-macros        (p-list-ctx F…) / (p-scalar-ctx F…) / (p-void-ctx F…) /
#                     (p-caller-ctx F…) → the `let` they abbreviate (#281 item 1)
#   sort-cmp          (p-sort-cmp PARAMS [decls…] BODY…) → the lambda/catch/
#                     block it abbreviates (#281 item 6)
#   dup-decls         a (defvar|p-defcell) repeated for the same (PACKAGE,
#                     NAME) is dropped — on BOTH sides, like insensitive-call
#                     (#281 item 2).  The package matters: in-package is
#                     READ-time per top-level form, so the same text under two
#                     packages is two different symbols
#   notinline-locally (locally (declare (notinline …)) B…) → (progn B…) — the
#                     top-level-`local` inlining cap; its discriminator moved
#                     at s412 (Phase C), a wrap-only change normalizes away
#
# The reader is a small S-expression reader for PCL's OWN emission: strings
# with backslash escapes, `|…|` symbol segments (may contain parens and
# spaces), `#\x` character literals, `;` comments, the `'` and `#'` prefixes.
# It is not a Common Lisp reader; a form it cannot read stops the file with
# a loud error rather than a silent skip.

use v5.20;
use strict;
use warnings;
no warnings 'recursion';   # the reader, rewrite and flat are tree walkers
use Getopt::Long;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);
use Cwd qw(abs_path);

my $root = abs_path("$RealBin/..");
my @rules;
my ($do_diff, $corpus_ref);
GetOptions('rule=s' => \@rules, 'diff' => \$do_diff, 'corpus=s' => \$corpus_ref)
  or die "usage: $0 [--rule NAME]... FILE | --diff A B | --corpus REF\n";
my %RULE = map { $_ => 1 } (@rules ? @rules
                          : qw(elem-setf insensitive-call ctx-macros sort-cmp
                               dup-decls notinline-locally));

if ($corpus_ref) { exit corpus_mode($corpus_ref) }
if ($do_diff) {
  die "--diff needs two files\n" unless @ARGV == 2;
  my ($a, $b) = map { normalize_text(slurp($_)) } @ARGV;
  if ($a eq $b) { print "identical after normalization\n"; exit 0 }
  my $dir = tempdir(CLEANUP => 1);
  spit("$dir/a", $a); spit("$dir/b", $b);
  system("diff", "-u", "$dir/a", "$dir/b");
  exit 1;
}
die "usage: $0 FILE\n" unless @ARGV == 1;
print normalize_text(slurp($ARGV[0]));
exit 0;

# ------------------------------------------------------------------ driver
sub corpus_mode {
  my ($ref) = @_;
  chdir $root or die;
  system("git rev-parse --verify --quiet \Q$ref\E^{commit} >/dev/null") == 0 or die "not a commit: $ref\n";
  my @files = sort glob("perl-tests/*.t");
  my $tmp = tempdir("pcl-emnorm-XXXXXX", TMPDIR => 1, CLEANUP => 1);
  my $wt = "$tmp/ref";
  system("git worktree add --quiet \Q$wt\E \Q$ref\E") == 0 or die "worktree add failed\n";
  my @differ;
  eval {
    for my $f (@files) {
      (my $base = $f) =~ s{.*/}{};
      my $new = `cd \Q$root\E && ./pl2cl < \Q$root/$f\E 2>/dev/null`;
      my $old = `cd \Q$wt\E   && ./pl2cl < \Q$root/$f\E 2>/dev/null`;
      for ($new, $old) { s/^;;; pcl: pipeline=.*\n//m; s/\Q$wt\E/ROOT/g; s/\Q$root\E/ROOT/g }
      my ($n, $o) = (eval { normalize_text($new) }, eval { normalize_text($old) });
      if (!defined $n || !defined $o) { push @differ, "$base (reader failed: $@)"; next }
      if ($n ne $o) {
        push @differ, $base;
        spit("$tmp/$base.new", $n); spit("$tmp/$base.old", $o);
        print STDERR "=== $base\n" . `diff -u \Q$tmp/$base.old\E \Q$tmp/$base.new\E | head -40`;
      }
    }
  };
  system("git -C \Q$root\E worktree remove --force \Q$wt\E >/dev/null 2>&1");
  die $@ if $@;
  if (@differ) { print "STILL DIFFER after normalization (" . scalar(@differ) . "):\n  " . join("\n  ", @differ) . "\n"; return 1 }
  print "identical after normalization across " . scalar(@files) . " files\n";
  return 0;
}

sub slurp { my ($p) = @_; open my $fh, '<:raw', $p or die "$p: $!"; local $/; my $t = <$fh>; close $fh; $t }
sub spit  { my ($p, $t) = @_; open my $fh, '>:raw', $p or die "$p: $!"; print $fh $t; close $fh }

# ------------------------------------------------------------------ reader
# Returns a list of top-level forms.  A form is a string (atom, printed
# verbatim) or an arrayref [head, @args] of forms.  The prefixes ' and #'
# become one-element wrappers ["'", form] / ["#'", form] so they print back
# without a space.
sub read_forms {
  my ($text) = @_;
  my @out;
  my $pos = 0;
  my $len = length $text;
  my $read; $read = sub {
    while ($pos < $len) {
      my $c = substr($text, $pos, 1);
      if ($c =~ /\s/) { $pos++; next }
      if ($c eq ';') { $pos = index($text, "\n", $pos); $pos = $len if $pos < 0; next }
      if ($c eq '(') {
        $pos++;
        my @kids;
        while (1) {
          $pos++ while $pos < $len && substr($text, $pos, 1) =~ /\s/;
          if ($pos < $len && substr($text, $pos, 1) eq ';') { my $nl = index($text, "\n", $pos); $pos = $nl < 0 ? $len : $nl; next }
          die "unterminated list at end of input\n" if $pos >= $len;
          if (substr($text, $pos, 1) eq ')') { $pos++; last }
          push @kids, $read->();
        }
        return \@kids;
      }
      if ($c eq ')') { die "unexpected ) at offset $pos\n" }
      if ($c eq '"') {
        my $start = $pos++;
        while ($pos < $len) {
          my $d = substr($text, $pos, 1);
          if ($d eq '\\') { $pos += 2; next }
          if ($d eq '"') { $pos++; last }
          $pos++;
        }
        return substr($text, $start, $pos - $start);
      }
      if ($c eq "'" || substr($text, $pos, 2) eq "#'") {
        my $pfx = $c eq "'" ? "'" : "#'";
        my $save = $pos;
        $pos += length $pfx;
        $pos++ while $pos < $len && substr($text, $pos, 1) =~ /\s/;
        if ($pos < $len && substr($text, $pos, 1) eq '(') { return [$pfx, $read->()] }
        $pos = $save;   # a quoted ATOM: read as one atom below
      }
      # atom: run of non-delimiters, with |…| segments and #\x literals
      my $start = $pos;
      while ($pos < $len) {
        my $d = substr($text, $pos, 1);
        if ($d eq '|') {                       # |…| segment: backslash escapes the next char (|$\||)
          $pos++;
          while ($pos < $len) {
            my $e = substr($text, $pos, 1);
            if ($e eq '\\') { $pos += 2; next }
            if ($e eq '|') { last }
            $pos++;
          }
          die "unterminated | at $start\n" if $pos >= $len;
          $pos++;
          next;
        }
        if ($d eq '#' && substr($text, $pos + 1, 1) eq '\\') { $pos += 3; next }   # #\x  (x may be a paren/space)
        last if $d =~ /[\s()]/ || $d eq '"' || $d eq ';';
        $pos++;
      }
      die "empty atom at $pos" if $pos == $start;
      return substr($text, $start, $pos - $start);
    }
    return undef;
  };
  while ($pos < $len) {
    my $f = $read->();
    push @out, $f if defined $f;
  }
  return \@out;
}

# ------------------------------------------------------------------ rules
sub is_list { ref $_[0] eq 'ARRAY' }
sub head    { my ($f) = @_; is_list($f) && @$f && !ref $f->[0] ? $f->[0] : '' }

sub rewrite {
  my ($f) = @_;
  return $f unless is_list($f);
  # bottom-up
  my @k = map { rewrite($_) } @$f;
  $f = \@k;
  my $h = head($f);
  # THE MACRO SPELLINGS EXPAND FIRST, and their result falls through to the
  # rules below rather than returning: (p-list-ctx (pl-F …)) must still be
  # seen by insensitive-call as the `let` it abbreviates, or a side that emits
  # the macro and a side that emits the let stop comparing equal.
  if ($RULE{'ctx-macros'} && $h =~ /^p-(list|scalar|void|caller)-ctx$/) {
    my $bind = { list => 't', scalar => 'nil', void => ':void', caller => '*pcl-caller-wantarray*' }->{$1};
    $f = ['let', [['*wantarray*', $bind]], @$f[1 .. $#$f]];
    $h = head($f);
  }
  # (p-sort-cmp PARAMS [decls…] BODY…) → the lambda/catch/block it abbreviates
  # (#281 item 6).  Leading (declare …) forms stay at the lambda head, exactly
  # as the macro puts them.
  if ($RULE{'sort-cmp'} && $h eq 'p-sort-cmp' && @$f >= 2) {
    my @body = @$f[2 .. $#$f];
    my @decl;
    push @decl, shift @body while @body && head($body[0]) eq 'declare';
    $f = ['lambda', $f->[1], @decl, ['catch', ':p-return', ['block', 'nil', @body]]];
    $h = head($f);
  }
  if ($RULE{'elem-setf'} && $h eq 'setf' && @$f == 3
      && (head($f->[1]) eq 'p-gethash' || head($f->[1]) eq 'p-aref')) {
    return ['p-setf', $f->[1], $f->[2]];
  }
  if ($RULE{'insensitive-call'} && $h eq 'let' && @$f == 3
      && is_list($f->[1]) && @{$f->[1]} == 1 && is_list($f->[1][0])
      && @{$f->[1][0]} == 2 && !ref $f->[1][0][0] && $f->[1][0][0] eq '*wantarray*'
      && head($f->[2]) =~ /^(?:[\w:|]+::)?pl-/) {
    return $f->[2];
  }

  # (locally (declare (notinline …)) BODY…) → (progn BODY…): v1's `local`
  # machinery wraps a "top-level" local's remainder in it to cap inlining
  # in a huge cold form; the discriminator moved at s412 (Phase C), so an
  # emission change that only adds/removes this wrap normalizes away.
  if ($RULE{'notinline-locally'}) {
    my $is_ni = sub { is_list($_[0]) && head($_[0]) eq 'declare'
                      && is_list($_[0][1]) && head($_[0][1]) eq 'notinline' };
    if ($h eq 'locally' && @$f >= 2 && $is_ni->($f->[1])) {
      return @$f == 3 ? $f->[2] : ['progn', @$f[2 .. $#$f]];
    }
    # the same declaration at the head of a `let` body (a top-level `local`
    # of an exception global carries it inside its own let)
    if (($h eq 'let' || $h eq 'let*') && @$f >= 3 && $is_ni->($f->[2])) {
      return [$h, $f->[1], @$f[3 .. $#$f]];
    }
  }
  return $f;
}

# ------------------------------------------------------------------ printer
sub flat {
  my ($f) = @_;
  return $f unless is_list($f);
  if (@$f == 2 && !ref $f->[0] && ($f->[0] eq "'" || $f->[0] eq "#'") && is_list($f->[1])) {
    return $f->[0] . flat($f->[1]);
  }
  return '(' . join(' ', map { flat($_) } @$f) . ')';
}

# A global DECLARATION repeated for the same (package, name) is dropped — on
# BOTH sides, so a side that stopped emitting the repeat cannot be told from
# one that never did (#281 item 2, the same "drop on both sides" shape as
# insensitive-call).  The key must carry the package: `in-package` is READ-time
# per top-level form, so a bare `$a` under :Foo and one under :Bar are
# DIFFERENT symbols and both declarations are real.
sub drop_dup_decls {
  my ($forms) = @_;
  my ($pkg, %seen, @out) = ('pcl');
  for my $f (@$forms) {
    my $h = head($f);
    if ($h eq 'in-package' && @$f == 2 && !ref $f->[1]) {
      ($pkg = $f->[1]) =~ s/^://;
    }
    elsif (($h eq 'defvar' || $h eq 'p-defcell') && @$f >= 2 && !ref $f->[1]) {
      my $key = $f->[1] =~ /::/ ? $f->[1] : "$pkg\0$f->[1]";
      next if $seen{$key}++;
    }
    push @out, $f;
  }
  return \@out;
}

sub normalize_text {
  my ($text) = @_;
  my $forms = read_forms($text);
  $forms = drop_dup_decls($forms) if $RULE{'dup-decls'};
  return join("\n", map { flat(rewrite($_)) } @$forms) . "\n";
}


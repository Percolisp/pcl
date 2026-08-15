package PCLLicense;
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# The ONE definition of "a PCL code file" and of the license tag it carries
# (USER, s401: "License — same as Perl.  Tag all code files."  And: "Don't
# tag code files straight from the Perl distro!").  Two consumers, so the
# rules live here and nowhere else:
#   tools/tag-license          — inserts / normalises the tag (idempotent)
#   Pl/t/license-tag-01.t      — the gate row: every code file carries it,
#                                every named exclusion still exists.
#
# WHAT IS A CODE FILE: under the roots below, a file whose extension is
# .pm/.pl/.t/.lisp/.sh, or an extensionless file whose first line is a
# shebang; plus the named extras.  Data (.json/.md/.tsv/xs-pin) is not code.
#
# WHAT IS EXCLUDED, BY NAME AND WITH A REASON: whole trees that are not ours
# (perl's own t/ under perl-tests/, the CPAN dists under cpan-tests/) and the
# lib/ files that are perl-core / CPAN code carried with local edits — their
# authors' notices stay, and they are under the same terms already.

use strict;
use warnings;
use File::Find ();

our $HOLDER = 'the PCL authors';
our $YEARS  = '2025-2026';
our @TAG_LINES = (
  "Copyright (c) $YEARS $HOLDER",
  "This is free software; you can redistribute it and/or modify it under the",
  "same terms as the Perl 5 programming language system itself.",
  "SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later",
);
our $SPDX = $TAG_LINES[3];

# Roots scanned (relative to the checkout), and root-level extras.
our @ROOTS  = qw(Pl cl lib tools examples .claude/hooks);
our @EXTRAS = qw(docs/ppi-bug-report.t);

# Named exclusions => reason.  Every one must EXIST (the gate checks) so an
# exclusion cannot outlive the file it excuses.
our %EXCLUDE = (
  'lib/IO/Handle.pm'        => 'perl core IO/Handle.pm with two subs changed (see its header); perl\'s notice, same terms',
  'lib/Math/BigInt/Calc.pm' => 'CPAN Math::BigInt::Calc 2.003002 carried verbatim; its authors\' notice, same terms',
);
# Whole trees never scanned (not ours): perl-tests/ (perl\'s own t/ files and
# test.pl), cpan-tests/ (CPAN distributions), docs/ (prose; the one .t is an
# EXTRA), memory/, .suitelog*/.

sub _has_shebang {
  my ($path) = @_;
  open my $fh, '<:raw', $path or return 0;
  my $l = <$fh> // '';
  return $l =~ /^#!/ ? 1 : 0;
}

# Returns (\@code_files, \%excluded_seen) with paths relative to $root.
sub code_files {
  my ($root) = @_;
  my (@files, %seen_excl);
  # root-level scripts: extensionless with a shebang, or *.pl
  opendir my $dh, $root or die "opendir $root: $!";
  for my $e (sort readdir $dh) {
    next if $e =~ /^\./;
    my $p = "$root/$e";
    next unless -f $p;
    push @files, $e if $e =~ /\.pl$/ || ($e !~ /\./ && _has_shebang($p));
  }
  closedir $dh;
  for my $r (@ROOTS) {
    next unless -d "$root/$r";
    File::Find::find({ no_chdir => 1, wanted => sub {
      return unless -f $_;
      my $rel = substr($_, length($root) + 1);
      return if $rel =~ /~$/;
      my $is_code = $rel =~ /\.(?:pm|pl|t|lisp|sh)$/
                 || ($rel !~ m{[^/]*\.[^/]*$} && _has_shebang($_));
      return unless $is_code;
      if ($EXCLUDE{$rel}) { $seen_excl{$rel} = 1; return }
      push @files, $rel;
    } }, "$root/$r");
  }
  push @files, grep { -f "$root/$_" } @EXTRAS;
  my %u; @files = sort grep { !$u{$_}++ } @files;
  return (\@files, \%seen_excl);
}

# Comment prefix for a file: Lisp gets ";;;; ", everything else "# ".
sub prefix_for {
  my ($path) = @_;
  return $path =~ /\.lisp$/ ? ';;;; ' : '# ';
}

# Where the tag block starts: after a shebang, an emacs mode line, or a
# transpiled-artifact gen stamp (";;; pcl: pipeline=… gen=…", line 1 by
# contract — Pl/t/artifact-staleness-01.t keys on it); else line 0.
sub insert_index {
  my ($lines) = @_;
  return 0 unless @$lines;
  my $l0 = $lines->[0];
  return 1 if $l0 =~ /^#!/;
  return 1 if $l0 =~ /^\s*(?:#|;+)\s*-\*-.*-\*-/;
  return 1 if $l0 =~ /^;;; pcl: pipeline=/;
  return 0;
}

# True when the file carries the exact current tag block within its first
# 8 lines.
sub has_tag {
  my ($path) = @_;
  open my $fh, '<:raw', $path or return 0;
  my @head; while (defined(my $l = <$fh>)) { push @head, $l; last if @head >= 8 }
  close $fh;
  my $pfx = prefix_for($path);
  my $want = join '', map { "$pfx$_\n" } @TAG_LINES;
  my $text = join '', @head;
  return index($text, $want) >= 0 ? 1 : 0;
}

1;

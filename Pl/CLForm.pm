# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::CLForm;

# CLForm — structured CL emission for the v2 pipeline (prototype).
#
# A *form* is one of:
#   - a plain string          → an atom already in CL syntax ("$x", "3", "\"s\"")
#   - a Raw object            → a pre-generated CL string (from the ORIGINAL
#                               Pl::ExprToCL) embedded opaquely; the escape
#                               hatch that lets v2 fall back per-expression
#   - [ $head, @args ]        → the list form ($head @args)
#   - [ 'list', @elems ]      → a headless parenthesized list (@elems) — used
#                               for let-bindings, lambda lists, (var list) pairs
#
# The printer is the ONLY place text is produced: parens always balance and
# indentation encodes depth (2 spaces/level) by construction. No downstream
# pass ever re-parses emitted text (the whole point of the v2 rewrite).

use v5.30;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(raw raw_wrap is_raw is_raw_wrap to_string to_flat to_program);

use Scalar::Util ();
use constant ONE_LINE_MAX => 95;

our %RAW_PROV;
sub raw {
  my ($s) = @_;
  my $r = bless \$s, 'Pl::CLForm::Raw';
  if ($ENV{PCL_E2_RAW_CENSUS}) {
    # Provenance for the census: which frame MADE this raw.  Inside-out
    # (keyed by refaddr) so the object itself stays a plain scalar ref for
    # every other consumer.
    my @c1 = caller(1);
    $RAW_PROV{Scalar::Util::refaddr($r)} =
      ($c1[3] // 'main') . ':' . ((caller(0))[2] // '?');
  }
  return $r;
}

sub is_raw { ref($_[0]) eq 'Pl::CLForm::Raw' }

# A raw OPEN chunk from the original generator that deliberately leaves
# $closes forms unclosed (v1's `local` machinery emits `(let ((…))` /
# `(p-local-… …` opens and defers the closes to block end, counted in
# _local_let_depth), wrapping @body — the lowered remainder of the enclosing
# v2 block — in their dynamic extent.  The printer appends exactly $closes
# `)`s after the body, so balance still holds by construction: the open
# text's depth surplus comes from the SAME counter v1 itself uses to close
# these scopes.
sub raw_wrap {
  my ($open, $closes, @body) = @_;
  return bless { open => $open, closes => $closes, body => [@body] },
    'Pl::CLForm::RawWrap';
}

sub is_raw_wrap { ref($_[0]) eq 'Pl::CLForm::RawWrap' }

# One-line rendering, or undef if the form contains a multi-line raw chunk
# or a string atom with an embedded newline (CL has no \n escape — a literal
# newline inside a string atom makes the "one line" lie to the indenter).
# A chunk containing a `;` comment (outside a string literal) can never be
# joined onto one line either: the comment would swallow every sibling that
# follows it on that line (v1's fallback emits `;; source-echo` raws).
sub _no_flat {
  my ($s) = @_;
  return 1 if $s =~ /\n/;
  return 0 unless $s =~ /;/;
  my @c = split //, $s;
  my ($in_str, $i) = (0, 0);
  while ($i < @c) {
    my $ch = $c[$i];
    if ($in_str) { if ($ch eq "\\") { $i += 2; next } $in_str = 0 if $ch eq '"' }
    elsif ($ch eq '"') { $in_str = 1 }
    elsif ($ch eq ';') { return 1 }
    $i++;
  }
  return 0;
}

# Does the text END inside a line comment?  Tracks string literals and #\
# character literals like the paren checker; comment state resets at newline.
sub _ends_in_comment {
  my ($s) = @_;
  # A `;` comment ends at the next newline, so the text can only END inside
  # one if a `;` occurs AFTER the last newline.  No `;` there → 0, without
  # the char scan.  This guard is load-bearing for compile time, not just a
  # shortcut: _close asks about the WHOLE accumulated subtree text at every
  # nesting level, so on a deeply nested file the full scan is quadratic —
  # 93% of a 200-statement file's transpile wall (#213, s335).  When a `;`
  # IS present the full scan still runs: whether it opens a comment depends
  # on string state carried from the start of the text.
  return 0 if index($s, ';', rindex($s, "\n") + 1) < 0;
  my @c = split //, $s;
  my ($in_str, $com, $ahb, $i) = (0, 0, 0, 0);
  while ($i < @c) {
    my $ch = $c[$i];
    if ($ch eq "\n") { $com = 0; $i++; next }
    if ($com) { $i++; next }
    if ($in_str) { if ($ch eq "\\") { $i += 2; next } $in_str = 0 if $ch eq '"' }
    elsif ($ahb) { $ahb = 0 }
    elsif ($ch eq '"') { $in_str = 1 }
    elsif ($ch eq '#' && $i + 1 < @c && $c[$i + 1] eq "\\") { $ahb = 1; $i += 2; next }
    elsif ($ch eq ';') { $com = 1 }
    $i++;
  }
  return $com;
}

sub _flat {
  my ($f) = @_;
  if (!ref $f) { return _no_flat($f) ? undef : $f }
  if (is_raw($f)) { return _no_flat($$f) ? undef : $$f }
  return undef if is_raw_wrap($f);
  my ($head, @args) = @$f;
  my @parts = $head eq 'list' ? () : ($head);
  for my $a (@args) {
    my $p = _flat($a);
    return undef unless defined $p;
    push @parts, $p;
  }
  return '(' . join(' ', @parts) . ')';
}

# EXACT flat rendering — the E2 emitter-conversion boundary.  One line,
# single spaces, no length limit, raw atoms embedded verbatim (even
# multi-line ones: v1's text emitters interpolate child text the same
# way).  A converted (form-producing) ExprToCL emitter inside a text
# context is printed with THIS, so byte-parity with the old text emitter
# is checkable per step (tools/corpus-diff.pl).  Unlike _flat above this
# never declines — flat is the contract, not an optimization.  raw_wrap
# is a statement-level device and cannot appear inside an expression
# form; a loud die beats silently mangling its deferred closers.
sub to_flat {
  my ($f) = @_;
  return $f unless ref $f;
  return $$f if is_raw($f);
  die "CLForm::to_flat: raw_wrap inside an expression form" if is_raw_wrap($f);
  my ($head, @args) = @$f;
  my @parts = ($head eq 'list' ? () : $head);
  push @parts, map { to_flat($_) } @args;
  return '(' . join(' ', @parts) . ')';
}

sub to_string {
  my ($f, $depth) = @_;
  $depth //= 0;
  _raw_census($f) if !$depth && $ENV{PCL_E2_RAW_CENSUS};
  return $f unless ref $f;
  # Raw chunks pass through verbatim: re-indenting would corrupt string
  # literals that contain newlines, and alignment is only cosmetic.
  return $$f if is_raw($f);
  if (is_raw_wrap($f)) {
    my $ind1 = '  ' x ($depth + 1);
    my $out = $f->{open}
      . join('', map { "\n$ind1" . to_string($_, $depth + 1) } @{ $f->{body} });
    # Closers appended to text that ends inside a `;` comment are swallowed
    # by the comment (v1's fallback echoes skipped statements as `;; …` raw
    # lines — local.t's stash delete-local) — drop them to their own line
    # then.  Byte-identical everywhere else.
    $out .= "\n" . ('  ' x $depth) if _ends_in_comment($out);
    return $out . (')' x $f->{closes});
  }
  my $flat = _flat($f);
  return $flat if defined $flat && length($flat) + 2 * $depth <= ONE_LINE_MAX;

  my ($head, @args) = @$f;
  my $ind1 = '  ' x ($depth + 1);
  if ($head eq 'list') {
    return _close('(' . join("\n$ind1", map { to_string($_, $depth + 1) } @args),
                  $depth);
  }
  # Keep short scrutinee args (var/list pairs of let/foreach) on the head line
  # when the first arg fits flat; body args go one per line.
  my $first = @args ? _flat($args[0]) : undef;
  if (defined $first && length($head) + length($first) + 2 * $depth < ONE_LINE_MAX) {
    my @rest = @args[1 .. $#args];
    return "($head $first)" unless @rest;
    return _close("($head $first\n"
      . join("\n", map { $ind1 . to_string($_, $depth + 1) } @rest), $depth);
  }
  return _close("($head\n"
    . join("\n", map { $ind1 . to_string($_, $depth + 1) } @args), $depth);
}

# Append a form's closing paren, dropping it to its own line when the body
# text ENDS inside a `;` comment — a raw residue chunk with a trailing
# comment would otherwise swallow the paren (the guard raw_wrap always had;
# with the E2.final root flip raw chunks can sit anywhere in a tree).
# Byte-identical output whenever the last line is comment-free.
sub _close {
  my ($out, $depth) = @_;
  $out .= "\n" . ('  ' x $depth) if _ends_in_comment($out);
  return $out . ')';
}

sub to_program {
  my (@forms) = @_;
  if ($ENV{PCL_E2_RAW_CENSUS}) { _raw_census($_) for @forms }
  return join("\n", map { ref($_) || $_ ne '' ? to_string($_, 0) : '' } @forms) . "\n";
}

# E2.final distance meter (task #78): count the raw text actually PRINTED,
# classified by shape.  Counting at emit sites overcounts — analysis parses
# and the native attempt build trees that are discarded; only what reaches
# to_program is real.
sub _raw_census {
  my ($f) = @_;
  return unless ref $f;
  if (is_raw($f)) {
    my $t = $$f // '';
    # Shape classes are secondary — a "lambda-looking" raw is usually just a
    # whole-expression fallback whose text starts with (lambda (s314b).  The
    # PROVENANCE (which frame called raw()) is the signal that locates work.
    my $cls = $t =~ /^\s*;;/ ? 'raw:comment-echo'
            : $t =~ /^\s*\(/ ? 'raw:form-text'
            : 'raw:atom';
    my $prov = $RAW_PROV{Scalar::Util::refaddr($f)} // '?';
    (my $snip = substr($t, 0, 100)) =~ s/\s+/ /g;
    warn "pcl-rawout\t$cls\t$prov\t$snip\n";
    return;
  }
  if (is_raw_wrap($f)) {
    warn "pcl-rawout\traw_wrap\n";
    _raw_census($_) for @{ $f->{body} };
    return;
  }
  _raw_census($_) for @$f;
}

# A form the FLAT printer cannot safely embed in an expression position
# (task #78 embed-safety scan, shared by Parser2 and PExpr): raw_wrap
# (statement-level device, to_flat dies on it) or a raw chunk that ENDS
# inside a line comment (a sibling or closing paren printed after it on the
# same line would be swallowed).  Interior newlines/comments are fine —
# to_flat embeds raw text verbatim, as v1's text emitters always did.
sub embed_unsafe {
  my ($f) = @_;
  return 0 unless ref $f;
  return 1 if is_raw_wrap($f);
  return _ends_in_comment($$f) if is_raw($f);
  return 0 unless ref $f eq 'ARRAY';
  for my $sub (@$f) { return 1 if embed_unsafe($sub) }
  return 0;
}

1;

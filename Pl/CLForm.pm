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
our @EXPORT_OK = qw(raw raw_wrap is_raw is_raw_wrap to_string to_flat to_program ctx_bind
                    cl_sym cl_pkg cl_unquote needs_pipes);

use Scalar::Util ();
use constant ONE_LINE_MAX => 95;

our %RAW_PROV;
# THE CONTEXT BIND, built in ONE place (task #281 item 1, s414).  Perl's
# calling context is a dynamic binding of *wantarray*, and every emitter that
# needs one asks here instead of spelling the `let` itself — seventeen sites
# did.  CTX is the CL form the binding takes: 't' (list), 'nil' (scalar),
# ':void' (void) or '*pcl-caller-wantarray*' (propagate the caller's).  The
# macros expand to exactly that let (cl/pcl-runtime.lisp), so this renames the
# emission without changing it.
#
# An unknown CTX DIES naming the value (CLAUDE.md rule 12): the set is closed,
# and a silent fallthrough here would emit a bind that reads as one context
# and behaves as another.
my %CTX_MACRO = (
  't'                      => 'p-list-ctx',
  'nil'                    => 'p-scalar-ctx',
  ':void'                  => 'p-void-ctx',
  '*pcl-caller-wantarray*' => 'p-caller-ctx',
);

sub ctx_bind {
  my ($ctx, @body) = @_;
  my $macro = $CTX_MACRO{$ctx // ''}
    or die "PCL internal: no context macro for '"
         . (defined $ctx ? $ctx : 'undef')
         . "' (known: " . join(' ', sort keys %CTX_MACRO) . ")\n";
  return [$macro, @body];
}

# ── THE NAME-SPELLING RULE (task #418, s423) ──────────────────────────────
# How a Perl NAME is spelled as a CL token.  ONE rule, one place: every
# emitter that turns a package name, a variable name, a sub name, a CLOS
# class name, a loop label or a bareword filehandle into CL text asks here.
#
# WHY.  SBCL's reader does TWO things to a BARE token before it becomes a
# symbol name, and generated code is read under (readtable-case :invert):
#   1. it NFKC-normalizes the characters — the fullwidth Ｘ (U+FF38) folds to
#      an ASCII X, so `%Ｘ` and `%X` are the SAME symbol (measured s423:
#      (read-from-string ":ＦＯＯ") has symbol-name "foo");
#   2. it inverts the case — an all-uppercase token down-cases.
# Neither happens to the runtime's side of the same name: `(p-stash "ＦＯＯ")`
# carries the perl characters verbatim.  So a non-ASCII name that is emitted
# BARE both collides with its ASCII twin and misses every runtime string
# path.  Inside |…| the reader takes the characters exactly as written
# (`:|ＦＯＯ|` → "ＦＯＯ"), which defeats both transforms at once.
#
# THE INVERSE HALF IS AS LOAD-BEARING: an ASCII name that is bare today MUST
# stay bare.  Under :invert a bare `$foo` reads as the symbol `$FOO` while
# `|$foo|` reads as `$foo` — quoting an ASCII name silently renames it.  So
# cl_sym is the IDENTITY on ASCII, and the acceptance bar for this rule is
# byte-identical emission over every ASCII file (tools/emission-ab.pl).
#
# The runtime half of the agreement is `%pcl-invert-case` in
# cl/pcl-runtime.lisp, which is likewise the identity on a name carrying a
# non-ASCII character — that is what makes "pipe-quoted" mean "verbatim" on
# both sides of the seam.
sub needs_pipes {
  my ($name) = @_;
  return 0 if !defined $name;
  return $name =~ /[^\x00-\x7F]/ ? 1 : 0;
}

# A `|` in the input means the caller is holding a CL SPELLING, not a perl
# name: no perl identifier, package name or bareword can contain one, while
# the emitters routinely pass a token something already spelled — the
# punctuation-variable rule (|$"|, |${^CAPTURE}|), and, less obviously, a
# PPI Word whose content was rewritten in place to a qualified CL symbol
# (|ＦＯＯ|::pl-two, from Parser::_qualified_sub_to_cl).  Spelling such a
# token again names a DIFFERENT symbol — measured s423: the second pass
# produced |\|ＦＯＯ|::pl-two| and the file no longer READ.  The test is
# ASCII-neutral by construction: cl_sym only ever acts on a name carrying a
# non-ASCII character, so nothing that is bare today can be caught by it.
sub _already_cl { return index($_[0], '|') >= 0 }

# The token for a NAME with no package half (a variable with its sigil, a
# `pl-`/`plc-`-prefixed symbol, a label, a bareword filehandle).
sub cl_sym {
  my ($name) = @_;
  return $name if !needs_pipes($name);
  return $name if _already_cl($name);
  return _pipe($name);
}

# The token for a name that must read as ONE symbol even though it contains a
# package marker.  A multi-segment name has always been quoted — the '::'
# would otherwise read as a package marker, naming the symbol `STDOUT` in the
# package `FOO`, which is a different object from the symbol whose NAME is
# "Foo::STDOUT" — and the non-ASCII rule joins it.  Any colon counts: a single
# stray ':' in a name reads as a package marker just as '::' does.
sub cl_whole_sym {
  my ($name) = @_;
  return $name if !defined $name;
  return $name if _already_cl($name);
  return _pipe($name) if $name =~ /:/;
  return cl_sym($name);
}

# The PACKAGE half of a qualified CL symbol (`Foo::$x`, `Foo::pl-bar`) — the
# same rule, named for the caller that has always had it.
sub cl_pkg { return cl_whole_sym($_[0]) }

sub _pipe {
  my ($s) = @_;
  $s =~ s/([|\\])/\\$1/g;
  return "|$s|";
}

# The inverse: the NAME a |…| token spells.  Used by the consumers that read
# emitted text back (the global partition, the cross-package forward-decl
# scanner) — they must see the same name the reader will.
sub cl_unquote {
  my ($tok) = @_;
  return $tok if !defined $tok;
  return $tok if $tok !~ /\A\|(.*)\|\z/s;
  my $inner = $1;
  $inner =~ s/\\(.)/$1/g;
  return $inner;
}

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
# never declines — flat is the contract, not an optimization.
# A raw_wrap (a v1 `local` inside a structurally lowered eval/do/sub body,
# Phase C of docs/plan-one-compiler-s411.md) renders as its open text, the
# body forms, and exactly $closes `)`s — balanced by construction, the same
# closers to_string appends; a segment that ENDS inside a `;` comment gets
# the next piece on a fresh line so nothing is swallowed.
sub to_flat {
  my ($f) = @_;
  return $f unless ref $f;
  return $$f if is_raw($f);
  if (is_raw_wrap($f)) {
    my $out = $f->{open};
    for my $b (@{ $f->{body} }) {
      $out .= (_ends_in_comment($out) ? "\n" : ' ') . to_flat($b);
    }
    $out .= "\n" if _ends_in_comment($out);
    return $out . (')' x $f->{closes});
  }
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
# (task #78 embed-safety scan, shared by Parser2 and PExpr): a raw chunk
# that ENDS inside a line comment (a sibling or closing paren printed after
# it on the same line would be swallowed).  Interior newlines/comments are
# fine — to_flat embeds raw text verbatim, as v1's text emitters always did.
# A raw_wrap is safe when its body is: to_flat prints its open text and its
# closers with the same end-of-comment care as to_string (until Phase C it
# was refused outright — every eval/do/sub body holding a `local` took the
# v1 text route for that alone).
sub embed_unsafe {
  my ($f) = @_;
  return 0 unless ref $f;
  if (is_raw_wrap($f)) {
    for my $sub (@{ $f->{body} }) { return 1 if embed_unsafe($sub) }
    return 0;
  }
  return _ends_in_comment($$f) if is_raw($f);
  return 0 unless ref $f eq 'ARRAY';
  for my $sub (@$f) { return 1 if embed_unsafe($sub) }
  return 0;
}

1;

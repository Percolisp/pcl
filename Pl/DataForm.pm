# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::DataForm;
# THE IR AS DATA — `pl2cl --emit-sexp` (task #1215; docs/plan-speed-and-ir-s470.md
# Part B item B5, docs/generated-cl-ir-review.md §3.1 item 7, grammar in
# docs/ir-spec.md §12b).
#
# THE PROBLEM.  PCL's output is Common Lisp source, so a consumer that wants
# the PROGRAM has to implement a CL reader: the five rules of ir-spec §11b —
# `|…|` verbatim symbols, an empty `||` contributing nothing, `#\c`
# characters, `#x`/`#o`/`#b` numbers, and string literals that may contain a
# raw newline.  Those are not hard, but they are five chances to be subtly
# wrong before a backend has evaluated a single form.
#
# THE ANSWER is a SECOND PRINTER over the same CLForm tree (never a text
# transform — the tree is the authority, `Pl::CLForm::to_string` is its CL
# printer and this is its data printer), emitting a form of S-expression that
# needs a ~50-line reader in any language:
#
#   * every symbol is pipe-quoted, always: `|p-+|` `|$x|` `|:class|` — a
#     keyword's colon is INSIDE the bars, so a reader never has to know CL's
#     package or keyword syntax;
#   * strings carry exactly the escapes `\\ \" \n \t \r \uXXXX` and nothing
#     else, so the file is 7-bit and LINE-ORIENTED;
#   * numbers are decimal (a `#x41` radix literal is converted);
#   * a character literal is `(|char| CODE)`, a quoted symbol `(|quote| |X|)`;
#   * one top-level form per line; no comments at all.
#
# WHAT IS AND IS NOT IN IT — stated, because the honest boundary is the point
# (review §3.1: "a sexp dump with embedded text islands is worse than no
# dump").  The data form carries the program's LOWERED FORMS.  Two things it
# does not carry, both DECLARED rather than silently dropped:
#
#   1. A v1-seam text chunk — a `Raw` leaf, i.e. CL the old text generator
#      produced and that never became a tree — is printed as
#      `(|p-cl-text| "…")`.  It is CL source a consumer must handle by other
#      means, and the trailer form COUNTS them, so "how much of this program
#      is structured?" is a number and not a guess.
#   2. The file's environment preamble and the CL-reader bookkeeping the
#      assembly writes as text (`in-package`, `p-defpackage`, `defclass`, the
#      per-package `$a`/`$b` defvars, the forward-global `defvar`s,
#      `p-run-compile-phase-blocks`, `p-set-current-package`) are not lowered
#      forms and do not appear.  ir-spec §11 already says the preamble is
#      ignorable; §12b says what a consumer must supply for the rest, and the
#      `(|p-bucket| …)` markers name each run's package and phase so it can.
#
# ORDER IS THE PROGRAM (ir-spec §1), so the forms are printed in the order the
# assembly emits them: every section's COMPILE-phase buckets first, then every
# section's RUN bucket (the #469 phase model).  The collector buffers by
# (section, bucket) and replays in that order — the two-pass rule applied to
# the TREES, not re-derived from the text.
use v5.20;
use strict;
use warnings;
use Pl::CLForm ();

our $VERSION = '1.0';

my $ON = 0;
my (@RUNS, $ISLANDS, $FORMS, $SECTION, $LAST_BUCKET);

# The buckets, in the order the assembly emits them within one section, and
# which PHASE each belongs to.  `run` is the only run-phase bucket; everything
# else is compile phase (Parser2::_lower_sections + the phase model, #469).
my %BUCKET_PHASE = (
  decls     => 'compile',
  pkg_enter => 'compile',
  defs      => 'compile',
  run       => 'run',
);
my @BUCKET_ORDER = qw(decls pkg_enter defs run);

sub enable {
  $ON = 1;
  reset_all();
  require Pl::Passes;
  Pl::Passes::set_form_hook(\&note_form);
  Pl::Passes::set_text_hook(\&note_text);
  return;
}
sub enabled { return $ON }

sub reset_all {
  # Explicit assignments, not a list one: `(@RUNS, $x, …) = (…)` would flatten
  # every scalar INTO @RUNS (an array in a list-assignment target eats the
  # rest), which is exactly the bug this comment now prevents from returning.
  @RUNS = ();
  ($ISLANDS, $FORMS, $SECTION, $LAST_BUCKET) = (0, 0, -1, '');
  return;
}

# Pl::Passes::run hands over each finished top-level form with the BUCKET it
# belongs to.  A `decls` bucket after anything else starts a new SECTION — the
# buckets cycle once per section, so the boundary is derivable and needs no
# extra plumbing through Parser2.
sub note_form {
  return unless $ON;
  my ($form, $bucket) = @_;
  $bucket = 'run' unless defined $bucket && $BUCKET_PHASE{$bucket};
  _open_bucket($bucket);
  push @{ $RUNS[-1]{forms} }, $form;
  return;
}

# The two v1-TEXT buckets (`captured`, `sched`) are not trees at all; they join
# the compile phase as ISLANDS, at the position the assembly gives them.
sub note_text {
  return unless $ON;
  my ($text) = @_;
  return unless defined $text && length $text;
  _open_bucket('captured');
  push @{ $RUNS[-1]{forms} }, Pl::CLForm::raw($text);
  return;
}

sub _open_bucket {
  my ($bucket) = @_;
  my $phase = $BUCKET_PHASE{$bucket} // 'compile';
  # A NEW section starts when the bucket cycle RE-ENTERS `decls` from another
  # bucket — the buckets run decls → pkg_enter → defs → run once per section
  # (Parser2::_lower_sections), so the boundary is derivable and Parser2 needs
  # no extra plumbing.  "…from another bucket" is load-bearing: the decls of
  # one section arrive as several separate forms.
  $SECTION++ if $bucket eq 'decls' && $LAST_BUCKET ne '' && $LAST_BUCKET ne 'decls';
  $SECTION = 0 if $SECTION < 0;
  if (!@RUNS || $RUNS[-1]{bucket} ne $bucket || $RUNS[-1]{section} != $SECTION) {
    push @RUNS, { section => $SECTION, bucket => $bucket, phase => $phase,
                  forms => [] };
  }
  $LAST_BUCKET = $bucket;
  return;
}

# ── The printer ───────────────────────────────────────────────────────────

# The data form of the whole program, as text.  Provenance first, then the
# runs in phase order, then the census trailer.
sub program {
  my (%o) = @_;
  my @out;
  push @out, _form(['p-data-form', 1,
                    _str($o{file} // '-'), _str($o{mode} // 'program'),
                    _str($o{generation} // '?')]);
  ($ISLANDS, $FORMS) = (0, 0);
  for my $phase (qw(compile run)) {
    for my $r (sort { $a->{section} <=> $b->{section} } grep { $_->{phase} eq $phase } @RUNS) {
      push @out, _form(['p-bucket', $r->{section}, _sym(":$r->{bucket}"),
                        _sym(":$phase")]);
      for my $f (@{ $r->{forms} }) {
        $FORMS++;
        push @out, _form($f);
      }
    }
  }
  push @out, _form(['p-data-form-end', $FORMS, $ISLANDS]);
  return join("\n", @out) . "\n";
}

sub island_count { return $ISLANDS }
sub form_count   { return $FORMS }

# One form, on one line.  `_form` takes a CLForm; the pieces below take
# already-rendered text.
sub _form {
  my ($f) = @_;
  return _atom($f) unless ref $f;
  if (Pl::CLForm::is_raw($f)) { $ISLANDS++; return '(' . _sym('p-cl-text') . ' ' . _str($$f) . ')' }
  if (Pl::CLForm::is_raw_wrap($f)) {
    # A raw_wrap is an OPEN text chunk with lowered forms inside its dynamic
    # extent and N closing parens implied.  It cannot be one island (the body
    # IS structured), so it becomes a marked form: the open text, the body,
    # and the closer count a consumer must apply.
    $ISLANDS++;
    return '(' . _sym('p-cl-text-wrap') . ' ' . _str($f->{open}) . ' '
         . $f->{closes} . ' '
         . join(' ', map { _form($_) } @{ $f->{body} }) . ')';
  }
  return _atom($f) unless ref $f eq 'ARRAY';
  my ($head, @args) = @$f;
  # `(p-esc "payload")` COLLAPSES to a plain data-form string.  p-esc exists
  # because CL string syntax has no `\n` (task #1212); the data form has one,
  # so carrying the wrapper here would be noise a consumer has to undo.
  return _str(unesc(read_cl_string($args[0])))
    if !ref($head) && $head eq 'p-esc' && @args == 1 && !ref($args[0])
       && $args[0] =~ /\A"/;
  my @parts = (!ref($head) && $head eq 'list') ? () : ($head);
  return '(' . join(' ', map { _form($_) } (@parts, @args)) . ')';
}

# An ATOM's data form.  The tree's atoms are CL TEXT (that is what the CL
# printer needs), so each shape is recognised and re-spelled.
sub _atom {
  my ($a) = @_;
  return _sym('nil') unless defined $a;
  return _sym('nil') if $a eq '';
  # A number, in any of the spellings the emitter writes.  Radix literals
  # become decimal: `#x41` is a NUMBER, not a symbol (ir-spec §11b rule 4).
  return $a           if $a =~ /\A-?(?:[0-9]+|[0-9]*\.[0-9]+(?:[eE][-+]?[0-9]+)?|[0-9]+\.[0-9]*(?:[eE][-+]?[0-9]+)?|[0-9]+[eE][-+]?[0-9]+)\z/;
  return hex($1)      if $a =~ /\A#[xX]([0-9a-fA-F]+)\z/;
  return oct("0$1")   if $a =~ /\A#[oO]([0-7]+)\z/;
  return oct("0b$1")  if $a =~ /\A#[bB]([01]+)\z/;
  # A string literal in CL syntax → the data form's own escaping.
  return _str(read_cl_string($a)) if $a =~ /\A"/;
  # A quoted symbol: `'DATA`, `'>` (ir-spec §11b's three meanings — the data
  # form keeps the quote explicit and lets the consumer decide).
  return '(' . _sym('quote') . ' ' . _atom(substr($a, 1)) . ')' if $a =~ /\A'/;
  # A character literal: `#\a`, `#\Newline`.
  if ($a =~ /\A#\\(.+)\z/s) {
    my $n = $1;
    my %named = (Newline => 10, Space => 32, Tab => 9, Return => 13, Nul => 0,
                 Null => 0, Linefeed => 10, Page => 12, Backspace => 8,
                 Rubout => 127, Escape => 27);
    my $cp = length($n) == 1 ? ord($n) : $named{ucfirst lc $n};
    return '(' . _sym('char') . ' ' . $cp . ')' if defined $cp;
    return '(' . _sym('char-name') . ' ' . _str($n) . ')';
  }
  # AN ATOM THAT IS REALLY CL TEXT.  A few emitters put a pre-spelled CL form
  # into the tree as a plain string — `(make-p-box nil)` as a `p-let` INIT is
  # the common one — and that is a v1-seam residue exactly like a `Raw` leaf,
  # not a symbol.  Declared as an island (and COUNTED) rather than spelled
  # `|(make-p-box nil)|`, which would be a symbol whose name happens to
  # contain a paren.
  if ($a =~ /\A\(/) {
    $ISLANDS++;
    return '(' . _sym('p-cl-text') . ' ' . _str($a) . ')';
  }
  return _sym($a);
}

# A SYMBOL, always pipe-quoted, with `|` and `\` escaped inside the bars —
# exactly CL's own `|…|` rule, so the data form's symbols are also readable by
# a CL reader.  A token the emitter already pipe-quoted is unquoted first, so
# the same NAME never gets two spellings.
sub _sym {
  my ($s) = @_;
  $s = _unpipe($s);
  $s =~ s/([|\\])/\\$1/g;
  return "|$s|";
}

# The NAME a possibly-pipe-quoted CL token spells.  A qualified token keeps its
# `::` inside the name: `|Foo::$x|` is one data-form symbol whose name says
# which package it belongs to (ir-spec §12b).
sub _unpipe {
  my ($t) = @_;
  return $t unless $t =~ /\|/;
  my $out = '';
  my @c = split //, $t;
  my ($i, $in) = (0, 0);
  while ($i < @c) {
    my $ch = $c[$i];
    if ($in && $ch eq "\\") { $out .= $c[$i + 1] // ''; $i += 2; next }
    if ($ch eq '|') { $in = !$in; $i++; next }
    $out .= $ch;
    $i++;
  }
  return $out;
}

# ── Strings ───────────────────────────────────────────────────────────────

# THE VALUE a CL string literal denotes.  `\\` and `\"` are CL's only escapes,
# so everything else — including a raw newline — is itself.  A `(p-esc "…")`
# payload is NOT read here: the caller collapses that form first (see
# `_form`'s caller in ir-spec §12b's grammar note).
sub read_cl_string {
  my ($tok) = @_;
  $tok =~ s/\A"//;
  $tok =~ s/"\z//;
  $tok =~ s/\\(.)/$1/gs;
  return $tok;
}

# THE DECODER for the data form's own escape alphabet — the Perl twin of the
# runtime's `%p-esc-decode`, and the reference implementation ir-spec §12b
# quotes.  It is here rather than in two places because `p-esc`'s payload and a
# data-form string are the SAME alphabet by design (rule 11).
sub unesc {
  my ($s) = @_;
  my $out = '';
  my @c = split //, $s;
  my ($i, $n) = (0, scalar @c);
  my @pend;
  while ($i < $n) {
    if ($c[$i] ne "\\") { $out .= $c[$i]; $i++; next }
    my $d = $c[$i + 1];
    die "Pl::DataForm: payload ends in a lone backslash: $s\n" unless defined $d;
    if    ($d eq '\\') { $out .= "\\";   $i += 2 }
    elsif ($d eq '"')  { $out .= '"';    $i += 2 }
    elsif ($d eq 'n')  { $out .= "\n";   $i += 2 }
    elsif ($d eq 't')  { $out .= "\t";   $i += 2 }
    elsif ($d eq 'r')  { $out .= "\r";   $i += 2 }
    elsif ($d eq 'u') {
      die "Pl::DataForm: \\u needs four hex digits: $s\n" if $i + 6 > $n;
      my $cp = hex(join '', @c[$i + 2 .. $i + 5]);
      $i += 6;
      # A SURROGATE PAIR is one code point (JSON's rule) — combine it.
      if ($cp >= 0xD800 && $cp <= 0xDBFF && $i + 6 <= $n
          && $c[$i] eq "\\" && $c[$i + 1] eq 'u') {
        my $lo = hex(join '', @c[$i + 2 .. $i + 5]);
        if ($lo >= 0xDC00 && $lo <= 0xDFFF) {
          $cp = 0x10000 + (($cp - 0xD800) << 10) + ($lo - 0xDC00);
          $i += 6;
        }
      }
      $out .= chr($cp);
    }
    else { die "Pl::DataForm: unknown escape \\$d in $s\n" }
  }
  return $out;
}

my %ESC = ("\\" => '\\\\', '"' => '\\"', "\n" => '\\n', "\t" => '\\t',
           "\r" => '\\r');

# A STRING in the data form: 7-bit, one line, with exactly the six escapes.
# A code point above the BMP becomes a SURROGATE PAIR, which is JSON's rule —
# so a JavaScript consumer's own string literal parser is already correct and
# a Perl one needs three lines (ir-spec §12b has both).
sub _str {
  my ($s) = @_;
  $s = '' unless defined $s;
  my $out = '"';
  for my $ch (split //, $s) {
    if (my $e = $ESC{$ch}) { $out .= $e; next }
    my $cp = ord $ch;
    if ($cp >= 0x20 && $cp < 0x7F) { $out .= $ch; next }
    if ($cp > 0xFFFF) {
      my $v = $cp - 0x10000;
      $out .= sprintf('\\u%04X\\u%04X', 0xD800 + ($v >> 10), 0xDC00 + ($v & 0x3FF));
      next;
    }
    $out .= sprintf('\\u%04X', $cp);
  }
  return $out . '"';
}

1;

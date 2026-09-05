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
# THE SECOND REASON A NAME CANNOT BE BARE (task #449, s449s): a character the
# CL READER does not read as part of a token.  A punctuation-named container —
# `@,`, `@;`, `@|`, `%(` — is a perl global like any other, but `,` ends a
# token, `|` opens a quoted symbol, `;` `#` start a comment, `(` `)` `'` "`"
# `"` and `\` are all reader macros or escapes, so a BARE `@,` is not a symbol
# at all: `(p-array-= @, (vector 1 2))` fails to READ, taking the whole file
# with it (measured; that is what #449 recorded as "the CL-unsafe punctuation
# arrays need a pipe-quoted emission").  Inside |…| every one of them is an
# ordinary character.
#
# It CANNOT collide with the inverse half above, and the reason is arithmetic:
# a perl identifier, package name, bareword filehandle or `pl-`-prefixed sub
# name cannot contain any of these characters, so the only names this arm can
# reach are punctuation variables — which carry no letters, so bare and quoted
# would read as the same symbol even if one existed.  `:` is deliberately NOT
# in the set: it is the package marker, `cl_whole_sym` already owns it, and
# `cl_sym`'s callers pass qualified names it must not swallow.
my $CL_UNSAFE = qr/[,;|'"`#()\\\s]/;

sub needs_pipes {
  my ($name) = @_;
  return 0 if !defined $name;
  return 1 if $name =~ /[^\x00-\x7F]/;
  return $name =~ $CL_UNSAFE ? 1 : 0;
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
# WHERE the pipe is decides, not whether there is one (s449s): a CL spelling's
# pipe always OPENS a token — at the start of the string, or right after the
# `::` that ends the package half (`|$"|`, `|ＦＯＯ|::pl-two`).  A perl
# PUNCTUATION NAME can contain one too, and there it is the name: `@|` and `%|`
# are globals like `@?` and `%?`, and with the old "contains a pipe" test they
# were handed back BARE and the file failed to read (task #449).  `$|` never
# hit it because its CL spelling is written out in %SPECIAL_VARS.
sub _already_cl { return $_[0] =~ /\A\||::\|/ ? 1 : 0 }

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

# THE ONE READING of the IR verification switch (task #1035, s466/s469bg;
# docs/ir-spec.md 2b.2a).  PCL_IR_PLAIN=1 makes every printer that gained a
# #1035 spelling -- the `p-let` declaration form, the `p-raw-params` per-name
# class, the `p-sub` facts plist -- emit the spelling that stood before it, so
# that `PCL_IR_PLAIN=1 tools/corpus-diff.pl <base>` reading IDENTICAL proves a
# step touched nothing but syntax.  It is a VERIFICATION switch, not an
# optimization: not in Pl/Passes.pm, and it changes no runtime behaviour.  Both
# emitters ask here, so the answer cannot drift between them.
#
# ITS OUTPUT IS FOR COMPARISON, NOT FOR RUNNING (s469bg).  Since #1035 step 3
# made `p-sub`'s facts plist a POSITIONAL slot, a plist-less `p-sub` -- which
# is exactly what this switch prints -- does not load on THIS runtime: the
# macro would bind its `facts` parameter to the first body form.  That is the
# price of an unambiguous lambda list, and it costs nothing, because the only
# consumer of this switch is a byte-diff against a tree that predates the step.
sub ir_plain { return $ENV{PCL_IR_PLAIN} ? 1 : 0 }

# TRUE when @$args is a keyword PLIST: every even position is a non-ref atom
# spelled as a CL keyword.  The one shape test for the plist layout above; a
# list that merely STARTS with a keyword (an argument run, say) is not one.
sub _is_plist {
  my ($args) = @_;
  for (my $i = 0; $i < @$args; $i += 2) {
    return 0 if ref $args->[$i] || $args->[$i] !~ /^:/;
  }
  return 1;
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
    # A declaration entry `(NAME CLASS INIT …)` (p-let, ir-spec §2b.2a) keeps
    # NAME and its keyword CLASS on the head line when the entry does not fit
    # flat: the class is what a reader looks for, and a line of its own would
    # separate it from the name it describes.  Shape-keyed (an atom followed
    # by a keyword atom), not name-keyed.
    #
    # A declaration entry's FIRST element is a NAME, never a keyword, and that
    # test is load-bearing (#1118): the OTHER headless list this printer sees
    # is `p-sub`'s FACTS PLIST (ir-spec §5.1), whose first two atoms
    # `:returns :str` fit the shape exactly.  Claimed as an entry, its tail
    # went through `join` — so a nested `:captures (CELLS…)`, an ARRAY REF,
    # was stringified into the emitted CL as `ARRAY(0x…)`.  That made the
    # output NON-DETERMINISTIC (a fresh address per run, so every A/B read
    # DIFF) and the plist ODD-LENGTH, and `%p-check-facts` then killed the
    # file at LOAD: "facts of pl-f are not keyword pairs".  Reachable
    # whenever a sub has a proven `:returns` family AND a capture manifest
    # AND a plist too long to print flat — t/opbasic/concat.t and t/op/try.t
    # both died on it.
    if (@args >= 3 && !ref $args[0] && $args[0] !~ /^:/
        && !ref $args[1] && $args[1] =~ /^:/) {
      my ($n, $c, $init, @facts) = @args;
      # The FACTS tail (`:perl "$x" :why :FAMILY :captured t`, ir-spec 2b.2a)
      # is short atoms describing the SAME binding, so it goes on ONE line
      # after the init rather than one atom per line -- `:perl` and `"$x"` on
      # separate lines would be unreadable and is not what a plist looks like.
      my $tail = @facts ? "\n$ind1" . join(' ', @facts) : '';
      return _close("($n $c\n" . $ind1 . to_string($init, $depth + 1) . $tail, $depth);
    }
    # A PLIST that did not fit flat (`p-sub`'s facts, ir-spec §5.1) breaks
    # between PAIRS, never between a key and its value: a `:captures` alone on
    # one line and its cell list on the next is not what a plist looks like,
    # and it is the same argument the declaration tail above makes.  Keyed on
    # the shape a plist has and nothing else has here — a leading keyword atom
    # and an even length, with every key position a keyword atom.
    if (@args && @args % 2 == 0 && _is_plist(\@args)) {
      my @pairs;
      for (my $i = 0; $i < @args; $i += 2) {
        push @pairs, $args[$i] . ' ' . to_string($args[$i + 1], $depth + 1);
      }
      return _close('(' . join("\n$ind1", @pairs), $depth);
    }
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

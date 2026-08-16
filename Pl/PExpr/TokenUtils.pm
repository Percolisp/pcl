# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::PExpr::TokenUtils;

use v5.20;
use strict;
use warnings;

use Moo;
use Scalar::Util ();

sub is_atomic {
  my $self      = shift;
  my $stmt      = shift;

  return 1
      if $self->is_string($stmt) || $self->is_number($stmt)
      || $self->is_var($stmt);

  # Note: s/// and tr/// are NOT atomic - they need a target ($_)
  # They're handled by is_regexp() and wrapped with '$_ =~' if standalone

  # $#arr - array last index
  return 1 if ref($stmt) eq 'PPI::Token::ArrayIndex';

  return undef;
}

sub is_regexp {
  my $self      = shift;
  my $stmt      = shift;

  # Match, Substitute (s///), or Transliterate (tr///, y///)
  return 1
      if ref($stmt) =~ /PPI::Token::Regexp::(Match|Substitute|Transliterate)/;

  return undef;
}

sub is_string {
  my $self      = shift;
  my $stmt      = shift;

  return 1 # , $stmt->content)
      if ref($stmt) =~ /PPI::Token::Quote::Single/;

  # q{...} - literal quoting, no interpolation (like single-quoted)
  return 1
      if ref($stmt) eq 'PPI::Token::Quote::Literal';

  if (ref($stmt) =~ /PPI::Token::Quote::Double/) {
    # Check if interpolation is needed
    my $content = $stmt->content();
    # Remove quotes and check for $ or @
    my $inner = $content;
    $inner =~ s/^"//;
    $inner =~ s/"$//;

    # Return 2 if interpolation needed, 1 if plain string.
    # Strip \\ pairs first so \\$var is seen as interpolatable
    # (two backslashes = one literal backslash, $ is still a variable).
    (my $tmp = $inner) =~ s/\\\\/\x00\x00/g;
    if ($tmp =~ /(?<!\\)[\$\@]/) {
      return 2;  # Needs interpolation
    }
    return 1; # Plain double-quoted string
  }

  # Handle qq{} and other Quote::Interpolate forms
  if (ref($stmt) eq 'PPI::Token::Quote::Interpolate') {
    # ->string, not a hand-strip: `qq {…}` (whitespace before the delimiter)
    # made the old `s/^qq.//` take the SPACE as the delimiter and leave the
    # braces in the content — see the note in StringInterpolation.pm.
    my $content = $stmt->can('string') ? $stmt->string : do {
      my $c = $stmt->content();
      $c =~ s/^qq.//;
      $c =~ s/.$//;
      $c;
    };

    # Return 2 if interpolation needed, 1 if plain
    (my $tmp = $content) =~ s/\\\\/\x00\x00/g;
    if ($tmp =~ /(?<!\\)[\$\@]/) {
      return 2;  # Needs interpolation
    }
    return 1;
  }

  return undef;
}

sub is_number {
  my $self      = shift;
  my $stmt      = shift;

  return 1 # , $stmt->content)
      if ref($stmt) =~ /PPI::Token::Number/;
  return undef;
}

sub is_var {
  my $self      = shift;
  my $stmt      = shift;

  # Handle filehandes, *foo etc too.
  # Also handle magic variables like $/, $_, $1, etc.
  return 1 # , $stmt->content)
      if ref($stmt) =~ /PPI::Token::(Symbol|Magic)/;
  return undef;
}

# "->"
sub is_arrow_op {
  my $self      = shift;
  my $stmt      = shift;

  return 1
      if ref($stmt) eq 'PPI::Token::Operator' && $stmt->content() eq '->';
  return undef;
}

sub is_arr_or_hash_braces {
  my $self      = shift;
  my $stmt      = shift;

  return 1
      if ref($stmt) eq 'PPI::Structure::Subscript'
      && ($stmt->start() eq '[' || $stmt->start() eq '{');
  return undef;
}

sub is_arr_braces {
  my $self      = shift;
  my $stmt      = shift;

  return 1
      if ref($stmt) eq 'PPI::Structure::Subscript'
      && $stmt->start() eq '[';
  return undef;
}

# Not for code. :-)
sub is_hash_braces {
  my $self      = shift;
  my $stmt      = shift;

  return 1
      if ref($stmt) eq 'PPI::Structure::Subscript'
      && $stmt->start() eq '{';
  return undef;
}

sub is_inline_hash {
  my $self      = shift;
  my $stmt      = shift;

  return 1
      if ref($stmt) eq 'PPI::Structure::Constructor'
      && $stmt->start() eq '{';
  return undef;
}

sub is_inline_arr {
  my $self      = shift;
  my $stmt      = shift;

  return 1
      if ref($stmt) eq 'PPI::Structure::Constructor'
      && $stmt->start() eq '[';
  return undef;
}

sub is_token_operator {
  my $self      = shift;
  my $stmt      = shift;

  # Handle standard operators
  if (ref($stmt) eq 'PPI::Token::Operator') {
    return $stmt->content();
  }

  # Handle Cast tokens (deref operators: $, @, %, &, *)
  # These act as prefix operators for dereference
  if (ref($stmt) eq 'PPI::Token::Cast') {
    return $stmt->content();
  }

  # Handle word-form binary operators (PPI::Token::Word)
  # PPI tokenizes 'isa' as a Word, not an Operator.
  if (ref($stmt) eq 'PPI::Token::Word') {
    my $word = $stmt->content();
    return $word if $word eq 'isa';
  }

  return undef;
}

sub is_list {
  my $self      = shift;
  my $stmt      = shift;

  return 1
      if ref($stmt) eq 'PPI::Structure::List';
  return undef;
}

# This can be both a sub name and a bareword as a filehandle (won't
# work from v5.30!)
sub is_word {
  my $self      = shift;
  my $stmt      = shift;

  return 1
      if ref($stmt) eq 'PPI::Token::Word';
  return undef;
}

# Instead of PPI Expr, represents a packed node tree.
sub is_internal_node_type {
  my $self      = shift;
  my $node      = shift;

  return (defined $node->{type} ? $node->{type} : 1)
      if ref($node) eq 'PPIreference';
  return undef;
}

# ---- the ONE below-assignment precedence table (task #138) ----------------
#
# Plain subs, not methods: both statement parsers (Pl::Parser and
# Pl::Parser2) need this classification, and neither holds a TokenUtils
# object.  It lives here because it is token-classification knowledge, and
# because the review (docs/v2-code-review.md §2) found it hand-rolled in
# three places and MISSING from five more — each copy a separate bug.
#
# Perl parses `$x = A, B` / `$x = A or B` as `($x = A), B` / `($x = A) or B`:
# assignment (and OP=) binds TIGHTER than `,`/`=>`/`or`/`and`/`xor`.  Any
# statement handler that slices "everything after the `=`" as the initializer
# folds such a tail into the value (op/lex_assign.t: `$a = readlink 'x','y'`
# must leave $a undef).
#
# lowprec_idx: index of the first DEPTH-0 operator below assignment
# precedence at or after $from, or undef when the run is clean.  Structures
# (parens / braces / subscripts) are single PPI children, so a scan over a
# statement's schildren only ever sees depth-0 operators.
sub lowprec_idx {
  my ($toks, $from) = @_;
  for my $i (($from // 0) .. $#$toks) {
    my $t = $toks->[$i];
    return $i if $t->isa('PPI::Token::Operator')
      && $t->content =~ /^(?:,|=>|or|and|xor)$/;
  }
  return undef;
}

# May the run $toks[$from .. $idx-1] be a PARENLESS list operator whose
# argument list swallows the operator at $idx?  Only `,`/`=>` are ambiguous:
# a rightward list operator's arguments run to the closing paren, the `;`, or
# an operator BELOW list-operator precedence -- i.e. `not`/`and`/`or`/`xor`,
# which are therefore always a safe split point.  For a comma the answer is
# conservative: a bare Word not immediately followed by its own paren list may
# be a list operator eating the comma (`state $c = \substr $s, 0, 1`), so the
# split is declined.  A caller that can hand the WHOLE run to the expression
# parser should do that instead of splitting -- PExpr owns the ambiguity, and
# only a caller that must interpose code between head and tail needs to split.
sub lowprec_split_safe {
  my ($toks, $from, $idx) = @_;
  return 1 if $toks->[$idx]->content =~ /^(?:or|and|xor)$/;
  for my $i ($from .. $idx - 1) {
    my $t = $toks->[$i];
    next unless $t->isa('PPI::Token::Word');
    my $nx = $toks->[$i + 1];
    return 0 unless $nx && $nx->isa('PPI::Structure::List');
  }
  return 1;
}

# ---- the ONE compound-assignment operator set (task #140) -----------------
#
# Perl's `OP=` operators, complete.  This existed as FOUR hand-rolled copies
# and two of them omitted the string-bitwise trio `&.= |.= ^.=` (feature
# 'bitwise'); both omissions were live divergences (s316v):
#   - %RANGE_SPLIT_STOP (VarAnnotator): `for ($y |.= "a" .. 3)` split at the
#     `..` and ran 4 iterations — but assignment binds LOOSER than `..`, so
#     the range is the assignment's RHS and perl runs ONE;
#   - the state-decl normalization (Parser2): `state ($u) |.= "a"` never got
#     rewritten to `state $u ; $u |.= "a"`, and yielded undef.
# Order matters for text matching: longest first, so `**=` is tried before
# `*=` and `&.=` before `&=`.
my @COMPOUND_ASSIGN = sort { length($b) <=> length($a) || $a cmp $b }
  qw(+= -= *= /= %= **= x= .=
     ||= &&= //=
     <<= >>= &= |= ^= &.= |.= ^.=);
my %COMPOUND_ASSIGN = map { $_ => 1 } @COMPOUND_ASSIGN;
my $COMPOUND_ASSIGN_ALT = join '|', map { quotemeta } @COMPOUND_ASSIGN;

# The set itself, longest-first — for callers that need to build their own
# lookup table (a stop-set, a hash keyed on more than these).
sub compound_assign_ops { return @COMPOUND_ASSIGN }

# Is this token content an `OP=` operator?  (`=` itself is NOT included —
# ask is_assign_op for that.)
sub is_compound_assign { return defined $_[0] && $COMPOUND_ASSIGN{ $_[0] } ? 1 : 0 }

# Any assignment operator: `=` or an `OP=`.
sub is_assign_op {
  return 0 unless defined $_[0];
  return 1 if $_[0] eq '=';
  return is_compound_assign($_[0]);
}

# Alternation for scanning raw SOURCE TEXT (where there are no PPI tokens to
# ask).  Callers must still exclude `==` themselves with a `(?!=)` tail — an
# `OP=` never legally abuts another `=`.
sub compound_assign_text_re { return qr/(?:$COMPOUND_ASSIGN_ALT)/ }

# --- Source SITE of an element: "where does this sit, and in which file?" ----
#
# Perl's compile-time decisions are top-down: a bareword is a call only if the
# name is already known where the call site is COMPILED (task #266).  Answering
# that needs two positions and the knowledge that they are comparable at all —
# positions from two different PPI documents (a bundled module, an eval string)
# are not.  So the document's identity is part of the site.
#
# Returns undef for a synthesized or detached element with no location; every
# caller must read that as "unknown", never as "before".
sub decl_site {
  my ($elem) = @_;
  return undef unless ref($elem) && Scalar::Util::blessed($elem)
                   && $elem->can('location') && $elem->can('top');
  my $loc = $elem->location or return undef;
  my $top = $elem->top      or return undef;
  return { doc => Scalar::Util::refaddr($top), pos => [ $loc->[0], $loc->[1] ] };
}

# Is this PPI::Token::HereDoc a RAW (non-interpolating) heredoc?  THE one
# predicate — every site that asks "does this heredoc interpolate?" calls it
# (#301).  A heredoc is raw exactly when its terminator is SINGLE-quoted, and
# perl allows both `~` (indented form) and whitespace between `<<` and a
# QUOTED terminator: all of `<<'E'`, `<< 'E'`, `<<~'E'`, `<<~ 'E'` are raw.
# Four hand-written regexes used to answer this and every one of them was
# narrower than perl: PExpr's `/^<<'/` missed both `~` and the space, the three
# Parser2 copies missed the space.  A miss is SILENT-WRONG — the text is run
# through string interpolation, so `$x`/`@y` vanish and `\n` collapses to a
# newline, with no diagnostic (closure.t RT #23265's fresh_perl source).
#
# Reads `{_heredoc_content}` first: the rename passes rewrite a heredoc's
# marker there, and content() still holds the original.
sub heredoc_is_raw {
  my ($t) = @_;
  return 0 unless ref($t) && Scalar::Util::blessed($t)
               && $t->isa('PPI::Token::HereDoc');
  return (($t->{_heredoc_content} // $t->content) =~ /^<<~?\s*'/) ? 1 : 0;
}

# Does site A sit at or before site B?  1 / 0, or undef when the two are not
# comparable (either site unknown, or they come from different documents).
sub site_precedes {
  my ($a, $b) = @_;
  return undef unless $a && $b && defined $a->{doc} && defined $b->{doc};
  return undef unless $a->{doc} == $b->{doc};
  return 1 if $a->{pos}[0] < $b->{pos}[0];
  return 0 if $a->{pos}[0] > $b->{pos}[0];
  return $a->{pos}[1] <= $b->{pos}[1] ? 1 : 0;
}

1;

package Pl::PExpr::TokenUtils;

# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

use v5.20;
use strict;
use warnings;

use Moo;

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
    my $content = $stmt->content();
    # Extract inner content (remove qq and delimiters)
    # qq{...}, qq(...), qq[...], qq/.../ etc.
    $content =~ s/^qq.//;
    $content =~ s/.$//;

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

sub is_list_parentheses {
  my $self      = shift;
  my $stmt      = shift;

  # XXXX Add test for { ... } or [ ... ]. Needed?
  return 1
      if ref($stmt) eq 'PPI::Structure::List';
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

sub _is_block {
  my $self      = shift;
  my $stmt      = shift;

  return 1
      if ref($stmt) eq 'PPI::Structure::Block';

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

1;

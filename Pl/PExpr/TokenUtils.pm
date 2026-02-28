package Pl::PExpr::TokenUtils;

# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

use v5.30;
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

1;

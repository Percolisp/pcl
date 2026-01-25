package Pl::PExpr::StringInterpolation;

# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

use v5.30;
use strict;
use warnings;

use Moo;

use PPI;
use PPI::Document;

# This module handles parsing of interpolated strings for Pl::PExpr
# It takes a string token and returns a node ID for the parsed result

# Parse a double-quoted string with variable interpolation
# "Hello $name" => string_concat("Hello ", $name)
# "Value: $x" => string_concat("Value: ", $x)
# "Array: @arr" => string_concat("Array: ", join($", @arr))
#
# Parameters:
#   $parser - The Pl::PExpr object (for calling make_node, parse, etc.)
#   $str_token - PPI::Token::Quote::Double token
#
# Returns:
#   $node_id - ID of created node (string_concat or simple node)
sub parse_interpolated_string {
  my $self      = shift;
  my $parser    = shift;  # Pl::PExpr object
  my $str_token = shift;

  my $content   = $str_token->content();

  say "parse_interpolated_string: Input: $content" if $parser->DEBUG & 32;

  # Remove surrounding quotes/delimiters based on type
  if (ref($str_token) eq 'PPI::Token::Quote::Interpolate') {
    # qq{...}, qq(...), qq[...], qq/.../, qq!...! etc.
    $content =~ s/^qq(.)//;
    my $open_delim = $1;
    # Handle paired delimiters
    my %pairs = ('{' => '}', '(' => ')', '[' => ']', '<' => '>');
    my $close_delim = $pairs{$open_delim} // $open_delim;
    $content =~ s/\Q$close_delim\E$//;
  } else {
    # Standard double-quoted string
    $content =~ s/^"//;
    $content =~ s/"$//;
  }
  
  my @parts;
  my $pos = 0;
  
  # Process the string character by character, looking for variables
  while ($pos < length($content)) {
    # Find next variable or end of string
    if ($content =~ /\G(.*?)(?:([\$\@])|$)/gc) {
      my $literal = $1;
      my $sigil = $2;
      
      # Add literal part if not empty
      if (length($literal) > 0) {
        # Unescape common escape sequences
        $literal = $self->unescape_string($literal);
        push @parts, $self->make_string_literal_node($parser, $literal);
      }
      
      last unless defined $sigil;
      
      # Parse the variable starting at current position
      my $var_start = pos($content);
      my ($var_node_id, $new_pos) = $self->parse_interpolated_variable(
          $parser, \$content, $var_start - 1
      );
      
      if (defined $var_node_id) {
        push @parts, $var_node_id;
        pos($content) = $new_pos;
      } else {
        # Failed to parse variable, treat $ or @ as literal
        push @parts, $self->make_string_literal_node($parser, $sigil);
      }
    }
  }
  
  # If no parts, return empty string
  if (@parts == 0) {
    return $self->make_string_literal_node($parser, "");
  }

  # If only one part, check if it's a simple string literal
  # Array variables need string_concat wrapper for proper join handling
  if (@parts == 1) {
    my $part_node = $parser->get_a_node($parts[0]);
    # Only return directly if it's a string literal, not an @array
    if (ref($part_node) ne 'PPI::Token::Symbol' || $part_node->content() !~ /^@/) {
      return $parts[0];
    }
    # Fall through to create string_concat for array variables
  }

  # Build string_concat node with all parts
  my ($concat_node, $concat_id) = $parser->make_node_insert('string_concat');
  # Add the original string to the node:
  $parser->_add_tag_to_node($concat_id, $str_token->content());

  for my $part_id (@parts) {
    $parser->add_child_to_node($concat_id, $part_id);
  }
  
  say "parse_interpolated_string: Created concat node $concat_id with ", 
      scalar(@parts), " parts" if $parser->DEBUG & 32;
  
  return $concat_id;
}


# Parse a variable or expression within an interpolated string
# Returns: ($node_id, $new_position) or (undef, $old_position)
sub parse_interpolated_variable {
  my $self      = shift;
  my $parser    = shift;
  my $content_ref = shift;  # Reference to string
  my $pos       = shift;     # Position of $ or @
  
  my $content   = $$content_ref;
  my $sigil     = substr($content, $pos, 1);
  
  say "parse_interpolated_variable: Starting at pos $pos, sigil: $sigil" 
      if $parser->DEBUG & 32;
  
  # Handle ${expr}
  if ($sigil eq '$' && substr($content, $pos + 1, 1) eq '{') {
    return $self->parse_braced_expression($parser, $content_ref, $pos);
  }
  
  # Set position for \G anchor
  pos($content) = $pos;

  # Handle special variable $$  (process ID)
  if ($sigil eq '$' && substr($content, $pos + 1, 1) eq '$') {
    # Make sure it's not $$var (scalar deref)
    my $after = substr($content, $pos + 2, 1);
    if ($after eq '' || $after !~ /\w/) {
      my $var_token = PPI::Token::Symbol->new('$$');
      my $var_id = $parser->make_node($var_token);
      return ($var_id, $pos + 2);
    }
  }

  # Handle magic variables starting with $
  if ($sigil eq '$') {
    my $next_char = substr($content, $pos + 1, 1);

    # Handle caret variables FIRST: $^O, $^V, $^W, $^X, etc.
    # (must check before single-punct to avoid matching just $^)
    if ($next_char eq '^' && substr($content, $pos + 2, 1) =~ /^[A-Z]$/) {
      my $caret_letter = substr($content, $pos + 2, 1);
      my $var_token = PPI::Token::Magic->new('$^' . $caret_letter);
      my $var_id = $parser->make_node($var_token);
      return ($var_id, $pos + 3);
    }

    # Handle single-punctuation magic variables: $! $? $. $@ $/ $\ $& $' $` $+
    # $; $, $| $: $% $= $- $< $> $( $) $[ $] $~ $"
    # Note: $^ alone (format top name) is rare, skip it to avoid ambiguity
    if ($next_char =~ /^[!\?\.\@\/\\&\'\`\+;\,\|:\%=\-<>\(\)\[\]~"]$/) {
      my $var_token = PPI::Token::Magic->new('$' . $next_char);
      my $var_id = $parser->make_node($var_token);
      return ($var_id, $pos + 2);
    }
  }

  # Handle simple variable name: $var or @var
  if ($content =~ /\G[\$\@](\w+)/gc) {
    my $var_name = $1;
    my $full_var = "$sigil$var_name";
    my $end_pos = pos($content);
    
    say "parse_interpolated_variable: Found simple var: $full_var" 
        if $parser->DEBUG & 32;
    
    # Check for array/hash subscript: $var[...] or $var{...}
    if (substr($content, $end_pos, 1) eq '[') {
      return $self->parse_array_subscript($parser, $content_ref, $pos,
					  $full_var);
    }
    elsif (substr($content, $end_pos, 1) eq '{') {
      return $self->parse_hash_subscript($parser, $content_ref, $pos,
					 $full_var);
    }
    
    # Simple variable - create token and node
    my $var_token = PPI::Token::Symbol->new($full_var);
    my $var_id = $parser->make_node($var_token);
    
    return ($var_id, $end_pos);
  }
  
  # Failed to parse
  return (undef, $pos);
}


# Parse ${expression} in interpolated string
sub parse_braced_expression {
  my $self      = shift;
  my $parser    = shift;
  my $content_ref = shift;
  my $pos       = shift;  # Position of $
  
  my $content   = $$content_ref;
  
  # Find matching }
  my $brace_start = $pos + 2;  # After ${
  my $depth = 1;
  my $i = $brace_start;
  
  while ($i < length($content) && $depth > 0) {
    my $ch = substr($content, $i, 1);
    $depth++ if $ch eq '{';
    $depth-- if $ch eq '}';
    $i++;
  }
  
  if ($depth != 0) {
    # Unmatched braces
    return (undef, $pos);
  }
  
  my $expr_str = substr($content, $brace_start, $i - $brace_start - 1);
  say "parse_braced_expression: expr_str: '$expr_str'" if $parser->DEBUG & 32;
  
  # Parse the expression using PPI
  my $doc = PPI::Document->new(\$expr_str);
  my @stmts = $doc->children();
  if (@stmts == 0) {
    return (undef, $pos);
  }
  
  my @parts = $stmts[0]->children();
  my $expr_id = $parser->parse(\@parts);
  
  return ($expr_id, $i);
}


# Parse $var[index] in interpolated string
sub parse_array_subscript {
  my $self      = shift;
  my $parser    = shift;
  my $content_ref = shift;
  my $pos       = shift;
  my $var_name  = shift;  # e.g., "$foo"
  
  my $content   = $$content_ref;
  my $bracket_start = index($content, '[', $pos);
  
  # Find matching ]
  my $depth = 1;
  my $i = $bracket_start + 1;
  
  while ($i < length($content) && $depth > 0) {
    my $ch = substr($content, $i, 1);
    $depth++ if $ch eq '[';
    $depth-- if $ch eq ']';
    $i++;
  }
  
  if ($depth != 0) {
    # Unmatched brackets - treat as simple variable
    my $var_token = PPI::Token::Symbol->new($var_name);
    my $var_id = $parser->make_node($var_token);
    return ($var_id, $bracket_start);
  }
  
  my $index_str = substr($content, $bracket_start+1, $i - $bracket_start - 2);
  
  # Trim whitespace
  $index_str =~ s/^\s+//;
  $index_str =~ s/\s+$//;
  
  say "parse_array_subscript: var=$var_name, index='$index_str'" 
      if $parser->DEBUG & 32;
  
  # Parse index expression
  my $doc = PPI::Document->new(\$index_str);
  my @stmts = $doc->children();
  if (@stmts == 0) {
    # Empty index - treat as simple variable
    my $var_token = PPI::Token::Symbol->new($var_name);
    my $var_id = $parser->make_node($var_token);
    return ($var_id, $bracket_start);
  }

  # Clone parts to preserve content after $doc goes out of scope
  # (PPI tokens lose content when their parent document is garbage collected)
  my @parts = map { $_->clone() } $stmts[0]->children();
  my $index_id = $parser->parse(\@parts);
  
  # Create array access node
  my ($acc_node, $acc_id) = $parser->make_node_insert('a_acc');
  
  my $arr_token = PPI::Token::Symbol->new($var_name);
  my $arr_id = $parser->make_node($arr_token);
  
  $parser->add_child_to_node($acc_id, $arr_id);
  $parser->add_child_to_node($acc_id, $index_id);
  
  return ($acc_id, $i);
}


# Parse $hash{key} in interpolated string
sub parse_hash_subscript {
  my $self      = shift;
  my $parser    = shift;
  my $content_ref = shift;
  my $pos       = shift;
  my $var_name  = shift;  # e.g., "$foo"
  
  my $content   = $$content_ref;
  my $brace_start = index($content, '{', $pos);
  
  # Find matching }
  my $depth = 1;
  my $i = $brace_start + 1;
  
  while ($i < length($content) && $depth > 0) {
    my $ch = substr($content, $i, 1);
    $depth++ if $ch eq '{';
    $depth-- if $ch eq '}';
    $i++;
  }
  
  if ($depth != 0) {
    # Unmatched braces - treat as simple variable
    my $var_token = PPI::Token::Symbol->new($var_name);
    my $var_id = $parser->make_node($var_token);
    return ($var_id, $brace_start);
  }
  
  my $key_str = substr($content, $brace_start + 1, $i - $brace_start - 2);
  
  # Trim whitespace
  $key_str =~ s/^\s+//;
  $key_str =~ s/\s+$//;
  
  say "parse_hash_subscript: var=$var_name, key='$key_str'" 
      if $parser->DEBUG & 32;
  
  # Parse key - could be bareword, variable, or expression
  my $key_id;
  
  # Check if it's a simple bareword (no quotes, no special chars)
  if ($key_str =~ /^[a-zA-Z_]\w*$/) {
    # Bareword key - convert to string
    my $str_token = PPI::Token::Quote::Double->new('"' . $key_str . '"');
    $str_token->{separator} = '"';
    $key_id = $parser->make_node($str_token);
  } else {
    # Parse as expression
    my $doc = PPI::Document->new(\$key_str);
    my @stmts = $doc->children();
    if (@stmts == 0) {
      # Empty key - treat as simple variable
      my $var_token = PPI::Token::Symbol->new($var_name);
      my $var_id = $parser->make_node($var_token);
      return ($var_id, $brace_start);
    }

    # Clone parts to preserve content after $doc goes out of scope
    my @parts = map { $_->clone() } $stmts[0]->children();
    $key_id = $parser->parse(\@parts);
  }
  
  # Create hash access node
  my ($acc_node, $acc_id) = $parser->make_node_insert('h_acc');
  
  my $hash_token = PPI::Token::Symbol->new($var_name);
  my $hash_id = $parser->make_node($hash_token);
  
  $parser->add_child_to_node($acc_id, $hash_id);
  $parser->add_child_to_node($acc_id, $key_id);
  
  return ($acc_id, $i);
}


# Create a string literal node
sub make_string_literal_node {
  my $self      = shift;
  my $parser    = shift;
  my $str       = shift;
  
  # Create a PPI string token
  my $str_token = PPI::Token::Quote::Double->new('"' . $str . '"');
  $str_token->{separator} = '"';
  
  return $parser->make_node($str_token);
}


# Unescape common escape sequences in strings
sub unescape_string {
  my $self      = shift;
  my $str       = shift;
  
  # Handle common escape sequences
  $str =~ s/\\n/\n/g;
  $str =~ s/\\t/\t/g;
  $str =~ s/\\r/\r/g;
  $str =~ s/\\\$/\$/g;
  $str =~ s/\\\@/\@/g;
  $str =~ s/\\\\/\\/g;
  $str =~ s/\\"/"/g;
  
  return $str;
}


1;

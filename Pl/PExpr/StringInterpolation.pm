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

  # Stack for case-changing escapes: each entry is { mode => 'U'|'L'|'Q'|'F', parts => [...] }
  my @case_stack;
  # Target list: either the top of the case stack or @parts
  my $cur_parts = \@parts;
  # Pending single-char transform: 'u' or 'l' (applies to next part only)
  my $pending_char_transform;

  # Process the string, looking for variables and case-changing escapes
  while ($pos < length($content)) {
    # Find next variable, case escape, or end of string
    if ($content =~ /\G((?:[^\$\@\\]|\\(?:c.?|[^ULulQFEc]))*?)(?:([\$\@])|\\([ULulQFE])|$)/gc) {
      my $literal = $1;
      my $sigil = $2;
      my $case_cmd = $3;

      # Add literal part if not empty
      if (length($literal) > 0) {
        $literal = $self->unescape_string($literal);
        my $lit_id = $self->make_string_literal_node($parser, $literal);
        if ($pending_char_transform) {
          $lit_id = $self->_wrap_case_func($parser,
            $pending_char_transform eq 'u' ? 'ucfirst' : 'lcfirst', $lit_id);
          $pending_char_transform = undef;
        }
        push @$cur_parts, $lit_id;
      }

      # Handle case-changing escape
      if (defined $case_cmd) {
        if ($case_cmd eq 'E') {
          # Close the current case group
          if (@case_stack) {
            my $group = pop @case_stack;
            $cur_parts = @case_stack ? $case_stack[-1]{parts} : \@parts;
            # Wrap group's parts in the appropriate function
            my $wrapped = $self->_wrap_case_group($parser, $group);
            if ($pending_char_transform) {
              $wrapped = $self->_wrap_case_func($parser,
                $pending_char_transform eq 'u' ? 'ucfirst' : 'lcfirst', $wrapped);
              $pending_char_transform = undef;
            }
            push @$cur_parts, $wrapped;
          }
          # \E also cancels any pending \u or \l with no content
          $pending_char_transform = undef;
        } elsif ($case_cmd eq 'u' || $case_cmd eq 'l') {
          $pending_char_transform = $case_cmd;
        } else {
          # \U, \L, \Q, \F — push a new group
          my $new_parts = [];
          push @case_stack, { mode => $case_cmd, parts => $new_parts };
          $cur_parts = $new_parts;
        }
        next;
      }

      last unless defined $sigil;

      # Parse the variable starting at current position
      my $var_start = pos($content);
      my ($var_node_id, $new_pos) = $self->parse_interpolated_variable(
          $parser, \$content, $var_start - 1
      );

      if (defined $var_node_id) {
        if ($pending_char_transform) {
          $var_node_id = $self->_wrap_case_func($parser,
            $pending_char_transform eq 'u' ? 'ucfirst' : 'lcfirst', $var_node_id);
          $pending_char_transform = undef;
        }
        push @$cur_parts, $var_node_id;
        pos($content) = $new_pos;
      } else {
        push @$cur_parts, $self->make_string_literal_node($parser, $sigil);
      }
    }
  }

  # Close any unclosed case groups (implicit \E at end of string)
  while (@case_stack) {
    my $group = pop @case_stack;
    $cur_parts = @case_stack ? $case_stack[-1]{parts} : \@parts;
    my $wrapped = $self->_wrap_case_group($parser, $group);
    push @$cur_parts, $wrapped;
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

  # ${identifier} is equivalent to $identifier — create Symbol token directly
  # (We can't use PPI::Document->new here because the document would be GC'd
  # when this function returns, invalidating the token's content.)
  if ($expr_str =~ /^[a-zA-Z_]\w*$/) {
    my $sym = PPI::Token::Symbol->new('$' . $expr_str);
    my $expr_id = $parser->make_node($sym);
    return ($expr_id, $i);
  }

  # Complex expression (e.g., ${$ref}) — parse via PPI but keep doc alive
  my $doc = PPI::Document->new(\$expr_str);
  $self->{_ppi_docs} //= [];
  push @{$self->{_ppi_docs}}, $doc;  # prevent GC
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

  # The content is already decoded (actual chars).  Re-encode as a valid
  # Perl double-quoted string literal so convert_perl_string will handle it.
  my $encoded = $str;
  $encoded =~ s/\\/\\\\/g;   # \ -> \\
  $encoded =~ s/"/\\"/g;     # " -> \"
  $encoded =~ s/\n/\\n/g;    # real newline -> \n sequence
  $encoded =~ s/\r/\\r/g;
  $encoded =~ s/\t/\\t/g;

  # Create a PPI string token
  my $str_token = PPI::Token::Quote::Double->new('"' . $encoded . '"');
  $str_token->{separator} = '"';

  return $parser->make_node($str_token);
}


# Unescape common escape sequences in strings
sub unescape_string {
  my $self      = shift;
  my $str       = shift;

  # Single-pass escape processing (reuses _process_dq_escape from ExprToCL)
  $str =~ s!\\(x\{[^}]*\}|x[0-9A-Fa-f]{1,2}|x|o\{[^}]*\}|[0-7]{1,3}|c.|[ntreafd"\\\$\@]|.)!
    Pl::ExprToCL::_process_dq_escape($1)
  !ge;

  return $str;
}


# Wrap a single node in a case-changing function call (ucfirst, lcfirst, etc.)
sub _wrap_case_func {
  my ($self, $parser, $func_name, $node_id) = @_;

  # Create: (pl-ucfirst ...) or (pl-lcfirst ...)
  my $func_token = PPI::Token::Word->new($func_name);
  my ($funcall_node, $funcall_id) = $parser->make_node_insert('funcall');
  my $name_id = $parser->make_node($func_token);
  $parser->add_child_to_node($funcall_id, $name_id);
  $parser->add_child_to_node($funcall_id, $node_id);

  return $funcall_id;
}

# Wrap a case group's parts in the appropriate function
# Group: { mode => 'U'|'L'|'Q'|'F', parts => [...] }
sub _wrap_case_group {
  my ($self, $parser, $group) = @_;

  my $mode = $group->{mode};
  my $parts = $group->{parts};

  # Map mode to function name
  my %mode_func = (
    'U' => 'uc',
    'L' => 'lc',
    'F' => 'fc',
    'Q' => 'quotemeta',
  );
  my $func_name = $mode_func{$mode} // 'uc';

  # If no parts, return empty string
  if (@$parts == 0) {
    return $self->make_string_literal_node($parser, "");
  }

  # Build the content node: single part or string_concat of multiple parts
  my $content_id;
  if (@$parts == 1) {
    $content_id = $parts->[0];
  } else {
    my ($concat_node, $concat_id) = $parser->make_node_insert('string_concat');
    for my $part_id (@$parts) {
      $parser->add_child_to_node($concat_id, $part_id);
    }
    $content_id = $concat_id;
  }

  # Wrap in function call
  return $self->_wrap_case_func($parser, $func_name, $content_id);
}

1;

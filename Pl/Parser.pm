package Pl::Parser;

use v5.30;
use strict;
use warnings;

use Moo;

use PPI;
use Data::Dump qw/dump/;

use Pl::PExpr;
use Pl::ExprToCL;
use Pl::Environment;

# Statement-level parser prototype.
# Iterates over PPI statements, delegates expressions to PExpr,
# generates Common Lisp via ExprToCL.

has filename => (
  is        => 'ro',
  predicate => 'has_filename',
);

has code => (
  is        => 'ro',
  predicate => 'has_code',
);

has ppi_doc => (
  is        => 'lazy',
);

has indent_level => (
  is        => 'rw',
  default   => 0,
);

has output => (
  is        => 'rw',
  default   => sub { [] },
);

has environment => (
  is        => 'lazy',
);

sub _build_environment {
  return Pl::Environment->new();
}


sub _build_ppi_doc {
  my $self = shift;

  if ($self->has_filename) {
    my $doc = PPI::Document->new($self->filename);
    die "Failed to parse file: " . $self->filename unless $doc;
    return $doc;
  }
  elsif ($self->has_code) {
    my $code = $self->code;
    my $doc = PPI::Document->new(\$code);
    die "Failed to parse code" unless $doc;
    return $doc;
  }
  else {
    die "Must provide either 'filename' or 'code'";
  }
}


# Main entry point: parse and generate CL
sub parse {
  my $self = shift;

  my $doc = $self->ppi_doc;
  $self->output([]);

  # Always start with in-package :pcl
  $self->_emit("(in-package :pcl)");
  $self->_emit("");

  $self->_process_children($doc);

  return join("\n", @{$self->output});
}


# Process children of a PPI node (Document or Block)
sub _process_children {
  my $self     = shift;
  my $parent   = shift;

  for my $child ($parent->children) {
    $self->_process_element($child);
  }
}


# Process a single PPI element
sub _process_element {
  my $self    = shift;
  my $element = shift;

  my $ref = ref($element);

  # Skip whitespace and POD
  return if $ref eq 'PPI::Token::Whitespace';
  return if $ref eq 'PPI::Token::Pod';

  # Emit Perl comments as Lisp comments
  if ($ref eq 'PPI::Token::Comment') {
    my $comment = $element->content;
    chomp $comment;
    $self->_emit(";; $comment");
    return;
  }

  # Handle different statement types
  if ($ref eq 'PPI::Statement') {
    # Simple expression statement
    $self->_process_expression_statement($element);
  }
  elsif ($ref eq 'PPI::Statement::Expression') {
    $self->_process_expression_statement($element);
  }
  elsif ($ref eq 'PPI::Statement::Variable') {
    # Variable declaration: my $x = 10;
    $self->_process_variable_statement($element);
  }
  elsif ($ref eq 'PPI::Statement::Compound') {
    # Control flow: if, while, for, etc.
    $self->_process_compound_statement($element);
  }
  elsif ($ref eq 'PPI::Statement::Sub') {
    # Subroutine declaration
    $self->_process_sub_statement($element);
  }
  elsif ($ref eq 'PPI::Statement::Package') {
    # Package declaration
    $self->_process_package_statement($element);
  }
  elsif ($ref eq 'PPI::Statement::Include') {
    # use/require
    $self->_process_include_statement($element);
  }
  elsif ($ref =~ /^PPI::Statement/) {
    # Other statement types - treat as expression for now
    $self->_process_expression_statement($element);
  }
  else {
    # Unknown - emit comment
    $self->_emit(";; UNKNOWN: $ref");
  }
}


# Process a simple expression statement
sub _process_expression_statement {
  my $self = shift;
  my $stmt = shift;

  my $perl_code = $stmt->content;
  $perl_code =~ s/;\s*$//;  # Remove trailing semicolon
  $perl_code =~ s/\n/ /g;   # Collapse newlines

  # Get the expression parts (skip semicolon, whitespace, and comments)
  my @parts = grep {
    my $ref = ref($_);
    $ref ne 'PPI::Token::Whitespace'
      && $ref ne 'PPI::Token::Comment'
      && !($ref eq 'PPI::Token::Structure' && $_->content eq ';')
  } $stmt->children;

  return unless @parts;

  # Check for statement modifiers: EXPR if/unless/while/until/for COND
  my $modifier_idx = -1;
  my $modifier;
  for my $i (0 .. $#parts) {
    if (ref($parts[$i]) eq 'PPI::Token::Word') {
      my $word = $parts[$i]->content;
      if ($word =~ /^(if|unless|while|until|for|foreach)$/) {
        $modifier_idx = $i;
        $modifier = $word;
        last;
      }
    }
  }

  my $cl_code;
  if ($modifier_idx > 0) {
    # Split into expression and condition
    my @expr_parts = @parts[0 .. $modifier_idx - 1];
    my @cond_parts = @parts[$modifier_idx + 1 .. $#parts];

    my $expr_cl = $self->_parse_expression(\@expr_parts, $stmt);
    my $cond_cl = $self->_parse_expression(\@cond_parts, $stmt);

    # Generate: (pl-if/unless/while cond expr)
    $cl_code = "(pl-$modifier $cond_cl $expr_cl)";
  }
  else {
    # No modifier - parse normally
    $cl_code = $self->_parse_expression(\@parts, $stmt);
  }

  # Emit as comment + code
  $self->_emit(";; $perl_code");
  $self->_emit($cl_code) if defined $cl_code;
  $self->_emit("");
}


# Process variable declaration: my $x = 10;
sub _process_variable_statement {
  my $self = shift;
  my $stmt = shift;

  my $perl_code = $stmt->content;
  $perl_code =~ s/;\s*$//;
  $perl_code =~ s/\n/ /g;

  # Get expression parts (skip semicolon and whitespace)
  my @parts = grep {
    my $ref = ref($_);
    $ref ne 'PPI::Token::Whitespace'
      && !($ref eq 'PPI::Token::Structure' && $_->content eq ';')
  } $stmt->children;

  return unless @parts;

  # Check if this is a state declaration inside a sub
  my $is_state = (ref($parts[0]) eq 'PPI::Token::Word' && $parts[0]->content eq 'state');
  my $state_vars = $self->{_current_state_vars} // {};

  if ($is_state && %$state_vars) {
    # State declaration inside a sub - generate init guard
    $self->_process_state_declaration($stmt, \@parts, $perl_code);
    return;
  }

  # Check for bare declaration without assignment (my $x; or my @arr;)
  # These have: declarator, variable, no operator
  my $has_operator = grep { ref($_) eq 'PPI::Token::Operator' } @parts;
  if (!$has_operator) {
    # Bare declaration - just emit as comment, runtime will auto-declare
    # For state, we also need init guard for bare declarations
    if ($is_state && %$state_vars) {
      $self->_process_state_declaration($stmt, \@parts, $perl_code);
      return;
    }
    $self->_emit(";; $perl_code (bare declaration)");
    $self->_emit("");
    return;
  }

  # Parse with PExpr (handles declarator extraction)
  my $cl_code = $self->_parse_expression(\@parts, $stmt);

  $self->_emit(";; $perl_code");
  $self->_emit($cl_code) if defined $cl_code;
  $self->_emit("");
}

# Process state variable declaration with init guard
sub _process_state_declaration {
  my $self = shift;
  my $stmt = shift;
  my $parts = shift;
  my $perl_code = shift;

  # Find the variable name(s) and initializer
  my @vars;
  my $init_expr;
  my $found_eq = 0;
  my @init_parts;

  for my $part (@$parts) {
    my $ref = ref($part);

    if ($ref eq 'PPI::Token::Word' && $part->content eq 'state') {
      next;  # Skip 'state' keyword
    }
    elsif ($ref eq 'PPI::Token::Symbol' && !$found_eq) {
      push @vars, $part->content;
    }
    elsif ($ref eq 'PPI::Token::Operator' && $part->content eq '=') {
      $found_eq = 1;
    }
    elsif ($found_eq) {
      push @init_parts, $part;
    }
  }

  # Parse the initializer expression
  my $init_cl = 'nil';
  if (@init_parts) {
    $init_cl = $self->_parse_expression(\@init_parts, $stmt) // 'nil';
  }

  $self->_emit(";; $perl_code");

  # Generate init guard for each state variable
  for my $var (@vars) {
    my $init_flag = "$var--init";
    $self->_emit("(unless $init_flag");
    $self->indent_level($self->indent_level + 1);
    $self->_emit("(setf $var (ensure-boxed $init_cl))");
    $self->_emit("(setf $init_flag t))");
    $self->indent_level($self->indent_level - 1);
  }

  $self->_emit("");
}


# Process compound statement (if/while/for/bare block)
sub _process_compound_statement {
  my $self = shift;
  my $stmt = shift;

  # Get the first keyword to determine statement type
  # Also detect any label (LABEL:) before the keyword
  my $first_word;
  my $first_block;
  my $label;
  for my $child ($stmt->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Token::Label') {
      # Label like "OUTER:" - extract just the name
      $label = $child->content;
      $label =~ s/:$//;  # Remove trailing colon
    }
    elsif ($ref eq 'PPI::Token::Word') {
      $first_word = $child->content;
      last;
    }
    elsif ($ref eq 'PPI::Structure::Block' && !$first_block) {
      $first_block = $child;
    }
  }

  if (!$first_word && $first_block) {
    # Bare block: { ... }
    $self->_process_bare_block($first_block);
  }
  elsif ($first_word eq 'if' || $first_word eq 'unless') {
    $self->_process_if_statement($stmt, $first_word);
  }
  elsif ($first_word eq 'while' || $first_word eq 'until') {
    $self->_process_while_statement($stmt, $first_word, $label);
  }
  elsif ($first_word eq 'for' || $first_word eq 'foreach') {
    $self->_process_for_statement($stmt, $first_word, $label);
  }
  else {
    # Unknown compound - emit as comment
    my $perl_code = $stmt->content;
    $perl_code =~ s/\n/ /g;
    $self->_emit(";; COMPOUND ($first_word) not yet implemented:");
    $self->_emit(";; $perl_code");
    $self->_emit("");
  }
}


# Process a bare block: { ... }
sub _process_bare_block {
  my $self  = shift;
  my $block = shift;

  $self->_emit(";; { ... }");
  $self->_emit("(progn");
  $self->indent_level($self->indent_level + 1);
  $self->_process_block($block);
  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");
  $self->_emit("");
}


# Process if/elsif/else statement
sub _process_if_statement {
  my $self     = shift;
  my $stmt     = shift;
  my $keyword  = shift;  # 'if' or 'unless'

  # Emit the original Perl as comment
  my $perl_code = $stmt->content;
  $perl_code =~ s/\n/ /g;
  $self->_emit(";; $perl_code");

  # Collect the if/elsif/else chain and conditions for declaration scanning
  my @clauses;  # Each: { type => 'if'|'elsif'|'else', cond => ..., block => ... }
  my @conditions;  # All condition elements for declaration scanning

  my $current_type;
  my $current_cond;

  for my $child ($stmt->children) {
    my $ref = ref($child);

    if ($ref eq 'PPI::Token::Word') {
      my $word = $child->content;
      if ($word eq 'if' || $word eq 'elsif' || $word eq 'unless') {
        $current_type = $word;
      }
      elsif ($word eq 'else') {
        $current_type = 'else';
      }
    }
    elsif ($ref eq 'PPI::Structure::Condition') {
      $current_cond = $child;
      push @conditions, $child;
    }
    elsif ($ref eq 'PPI::Structure::Block') {
      push @clauses, {
        type  => $current_type,
        cond  => $current_cond,
        block => $child,
      };
      $current_cond = undef;
    }
  }

  # Use common helper to wrap with declarations
  $self->_with_declarations(\@conditions, sub {
    $self->_generate_if_clauses(\@clauses);
  });

  $self->_emit("");
}


# Generate CL for if/elsif/else chain
sub _generate_if_clauses {
  my $self    = shift;
  my $clauses = shift;

  return unless @$clauses;

  my $first = $clauses->[0];
  my $rest  = [@$clauses[1 .. $#$clauses]];

  # Generate condition
  my $cond_cl = $self->_parse_condition($first->{cond});

  # Emit comment for this clause
  my $cond_perl = $first->{cond} ? $first->{cond}->content : "";
  $cond_perl =~ s/^\s*\(\s*//;  # Remove leading paren
  $cond_perl =~ s/\s*\)\s*$//;  # Remove trailing paren
  $cond_perl =~ s/\n/\n;; /g;   # Add ;; to continuation lines
  $self->_emit(";; $first->{type} ($cond_perl)");

  # Handle 'unless' by negating
  if ($first->{type} eq 'unless') {
    $cond_cl = "(pl-not $cond_cl)";
  }

  $self->_emit("(pl-if $cond_cl");
  $self->indent_level($self->indent_level + 1);

  # Then block
  $self->_emit("(progn");
  $self->indent_level($self->indent_level + 1);
  $self->_process_block($first->{block});
  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");

  # Else/elsif part
  if (@$rest) {
    my $next = $rest->[0];
    if ($next->{type} eq 'else') {
      # Simple else
      $self->_emit(";; else");
      $self->_emit("(progn");
      $self->indent_level($self->indent_level + 1);
      $self->_process_block($next->{block});
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")");
    }
    else {
      # elsif - recursive
      $self->_generate_if_clauses($rest);
    }
  }
  else {
    # No else clause - emit nil
    $self->_emit("nil");
  }

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");
}


# Process a block's contents
sub _process_block {
  my $self  = shift;
  my $block = shift;

  # Enter new scope for filehandles
  $self->environment->push_scope();

  for my $child ($block->children) {
    my $ref = ref($child);
    next if $ref eq 'PPI::Token::Whitespace';
    next if $ref eq 'PPI::Token::Comment';

    $self->_process_element($child);
  }

  # Leave scope - removes filehandles added in this block
  $self->environment->pop_scope();
}


# Counter for anonymous block functions
my $anon_block_counter = 0;

# Parse a block as a named function for grep/map/sort/eval/sub blocks
# Returns the generated function name
# $params is arrayref: ['$_'] for grep/map, ['$a','$b'] for sort, [] for eval/sub
sub parse_block_as_function {
  my $self   = shift;
  my $block  = shift;  # PPI::Structure::Block
  my $params = shift // [];  # Parameter names

  # Generate unique function name
  my $func_name = sprintf("--anon-block-%d--", ++$anon_block_counter);

  # Build parameter list
  my $params_cl = join(' ', @$params);

  # Emit the function definition
  $self->_emit("(defun $func_name ($params_cl)");
  $self->indent_level($self->indent_level + 1);

  # Enter new scope for filehandles
  $self->environment->push_scope();

  # Process block contents
  my $has_content = 0;
  for my $child ($block->children) {
    my $ref = ref($child);
    next if $ref eq 'PPI::Token::Whitespace';
    next if $ref eq 'PPI::Token::Comment';

    $self->_process_element($child);
    $has_content = 1;
  }

  # Leave scope - removes filehandles added in this block
  $self->environment->pop_scope();

  # Emit nil if block was empty
  $self->_emit("nil") unless $has_content;

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");
  $self->_emit("");  # Blank line after function

  return $func_name;
}


# Find all variable declarations recursively in a PPI element
# Returns arrayref of { type => 'my'|'our'|..., var => '$x' }
sub _find_all_declarations {
  my $self = shift;
  my $elem = shift;
  my @decls;

  # Handle arrays of elements
  my @children;
  if (ref($elem) eq 'ARRAY') {
    @children = @$elem;
  } elsif (ref($elem) && $elem->can('children')) {
    @children = $elem->children;
  } else {
    return [];
  }

  my $pending_decl;
  for my $child (@children) {
    my $ref = ref($child);

    # Found a declarator keyword
    if ($ref eq 'PPI::Token::Word' && $child->content =~ /^(my|our|state|local)$/) {
      $pending_decl = $1;
    }
    # Found a variable after declarator
    elsif ($pending_decl && $ref eq 'PPI::Token::Symbol') {
      push @decls, { type => $pending_decl, var => $child->content };
      $pending_decl = undef;
    }
    # Found a list after declarator: my ($x, $y)
    elsif ($pending_decl && $ref eq 'PPI::Structure::List') {
      # Find all symbols inside the list
      my @list_vars = $self->_find_symbols_in_list($child);
      for my $var (@list_vars) {
        push @decls, { type => $pending_decl, var => $var };
      }
      $pending_decl = undef;
      # Don't recurse into this list - we already processed it
      next;
    }
    # Not a symbol or list after declarator - reset
    elsif ($pending_decl && $ref !~ /Whitespace/) {
      $pending_decl = undef;
    }

    # Recurse into children
    if ($ref && $child->can('children')) {
      push @decls, @{$self->_find_all_declarations($child)};
    }
  }

  return \@decls;
}

# Helper: find all symbol names in a list structure like ($x, $y, @z)
sub _find_symbols_in_list {
  my $self = shift;
  my $list = shift;
  my @vars;

  for my $child ($list->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Token::Symbol') {
      push @vars, $child->content;
    }
    elsif ($ref && $child->can('children')) {
      push @vars, $self->_find_symbols_in_list($child);
    }
  }

  return @vars;
}

# Common helper: wrap emitted code with let for any 'my' declarations
# Usage: $self->_with_declarations($ppi_elements, sub { ... emit code ... });
# $ppi_elements can be a single PPI element or arrayref of elements to scan
sub _with_declarations {
  my $self = shift;
  my $elements = shift;  # PPI element(s) to scan for declarations
  my $emit_body = shift; # Callback to emit the body code

  # Collect declarations from all elements
  my @all_decls;
  my @elems = ref($elements) eq 'ARRAY' ? @$elements : ($elements);
  for my $elem (@elems) {
    next unless defined $elem;
    push @all_decls, @{$self->_find_all_declarations($elem)};
  }

  # Get unique 'my' variables (skip state vars - they're handled at sub level)
  my $state_vars = $self->{_current_state_vars} // {};
  my %seen;
  my @my_vars = grep { !$seen{$_}++ }
                grep { !$state_vars->{$_} }  # skip state vars
                map { $_->{var} }
                grep { $_->{type} eq 'my' } @all_decls;

  # Wrap in let if we have declarations
  if (@my_vars) {
    my $bindings = join(" ", map { "($_ (make-pl-box nil))" } @my_vars);
    $self->_emit("(let ($bindings)");
    $self->indent_level($self->indent_level + 1);
  }

  # Emit the body
  $emit_body->();

  # Close let if we opened it
  if (@my_vars) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  }
}

# Parse a condition (from PPI::Structure::Condition)
# Returns: ($cl_code, $declarations_arrayref) in list context
#          $cl_code in scalar context
sub _parse_condition {
  my $self = shift;
  my $cond = shift;

  # Find ALL declarations recursively (including nested ones like my $x = my $y = 3)
  my $all_decls = $self->_find_all_declarations($cond);

  # Get the expression inside the condition
  my @parts;
  for my $child ($cond->children) {
    my $ref = ref($child);
    next if $ref eq 'PPI::Token::Whitespace';

    if ($ref eq 'PPI::Statement::Expression') {
      # Get children of the expression
      push @parts, grep {
        ref($_) ne 'PPI::Token::Whitespace'
      } $child->children;
    }
    else {
      push @parts, $child;
    }
  }

  my ($result, $decls) = $self->_parse_expression(\@parts, $cond);
  $result //= "nil";

  # Merge: use our recursive findings (which catches nested decls)
  return wantarray ? ($result, $all_decls) : $result;
}


# Process while/until loop
sub _process_while_statement {
  my $self    = shift;
  my $stmt    = shift;
  my $keyword = shift;
  my $label   = shift;  # Optional loop label

  my $perl_code = $stmt->content;
  $perl_code =~ s/\n/ /g;
  $self->_emit(";; $perl_code");

  # Find condition and block
  my ($cond, $block);
  for my $child ($stmt->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Structure::Condition') {
      $cond = $child;
    }
    elsif ($ref eq 'PPI::Structure::Block') {
      $block = $child;
      last;  # Take first block only
    }
  }

  # Get condition CL code
  my $cond_cl = $cond ? $self->_parse_condition($cond) : "t";
  $cond_cl //= "t";

  # Handle 'until' by negating
  if ($keyword eq 'until') {
    $cond_cl = "(pl-not $cond_cl)";
  }

  # Build the loop form with optional label
  my $label_arg = $label ? " :label $label" : "";

  # Use common helper to wrap with declarations
  $self->_with_declarations($cond, sub {
    $self->_emit("(pl-while $cond_cl$label_arg");
    $self->indent_level($self->indent_level + 1);
    $self->_process_block($block) if $block;
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  });

  $self->_emit("");
}


# Process for/foreach loop
sub _process_for_statement {
  my $self    = shift;
  my $stmt    = shift;
  my $keyword = shift;
  my $label   = shift;  # Optional loop label

  my $perl_code = $stmt->content;
  $perl_code =~ s/\n/ /g;
  $self->_emit(";; $perl_code");

  # Check for C-style for vs foreach style
  my $c_style_for;
  my $block;

  for my $child ($stmt->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Structure::For') {
      $c_style_for = $child;
    }
    elsif ($ref eq 'PPI::Structure::Block') {
      $block = $child;
    }
  }

  if ($c_style_for) {
    $self->_process_c_style_for($c_style_for, $block, $stmt, $label);
  }
  else {
    $self->_process_foreach_loop($stmt, $block, $label);
  }
}


# Process C-style for loop: for (init; cond; incr) { }
sub _process_c_style_for {
  my $self   = shift;
  my $for_struct = shift;
  my $block  = shift;
  my $stmt   = shift;
  my $label  = shift;  # Optional loop label

  # Collect the three statements from the for structure
  my @statements;
  for my $child ($for_struct->children) {
    my $ref = ref($child);
    next if $ref eq 'PPI::Token::Whitespace';
    if ($ref =~ /^PPI::Statement/) {
      push @statements, $child;
    }
  }

  my $init_cl = '';
  my $cond_cl = 't';  # Default to true
  my $incr_cl = '';

  # Process init statement (may be variable declaration or expression)
  if (@statements >= 1) {
    my $init_stmt = $statements[0];
    my @parts = grep {
      ref($_) ne 'PPI::Token::Whitespace' &&
      !(ref($_) eq 'PPI::Token::Structure' && $_->content eq ';')
    } $init_stmt->children;

    if (@parts) {
      # Strip 'my'/'our'/'local' keywords for parsing (declarations handled by _with_declarations)
      if (ref($init_stmt) eq 'PPI::Statement::Variable') {
        @parts = grep { !(ref($_) eq 'PPI::Token::Word' && $_->content =~ /^(my|our|local)$/) } @parts;
      }
      $init_cl = $self->_parse_expression(\@parts, $stmt) // '' if @parts;
    }
  }

  # Process condition
  if (@statements >= 2) {
    my @parts = grep {
      ref($_) ne 'PPI::Token::Whitespace' &&
      !(ref($_) eq 'PPI::Token::Structure' && $_->content eq ';')
    } $statements[1]->children;
    $cond_cl = $self->_parse_expression(\@parts, $stmt) // 't' if @parts;
  }

  # Process increment
  if (@statements >= 3) {
    my @parts = grep {
      ref($_) ne 'PPI::Token::Whitespace' &&
      !(ref($_) eq 'PPI::Token::Structure' && $_->content eq ';')
    } $statements[2]->children;
    $incr_cl = $self->_parse_expression(\@parts, $stmt) // '' if @parts;
  }

  # Build label argument if present
  my $label_arg = $label ? " :label $label" : "";

  # Use common helper - scan init and condition for declarations
  my @decl_sources = grep { defined } @statements[0..1];
  $self->_with_declarations(\@decl_sources, sub {
    $self->_emit("(pl-for ($init_cl)");
    $self->_emit("        ($cond_cl)");
    $self->_emit("        ($incr_cl)$label_arg");
    $self->indent_level($self->indent_level + 1);
    $self->_process_block($block) if $block;
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  });

  $self->_emit("");
}


# Process foreach-style loop: for/foreach VAR (LIST) { }
sub _process_foreach_loop {
  my $self  = shift;
  my $stmt  = shift;
  my $block = shift;
  my $label = shift;  # Optional loop label

  my $loop_var;
  my @list_parts;

  for my $child ($stmt->children) {
    my $ref = ref($child);

    if ($ref eq 'PPI::Token::Symbol' && !$loop_var) {
      $loop_var = $child->content;
    }
    elsif ($ref eq 'PPI::Structure::List') {
      # The list expression
      for my $lc ($child->children) {
        next if ref($lc) eq 'PPI::Token::Whitespace';
        if (ref($lc) eq 'PPI::Statement::Expression') {
          push @list_parts, grep {
            ref($_) ne 'PPI::Token::Whitespace'
          } $lc->children;
        }
        else {
          push @list_parts, $lc;
        }
      }
    }
  }

  $loop_var //= '$_';
  my $list_cl = @list_parts
    ? ($self->_parse_expression(\@list_parts, $stmt) // "(list)")
    : "(list)";

  # Convert (progn ...) to (vector ...) for foreach list context
  # This handles: foreach (1, 2, 3) which parses as a progn
  # Use \s* to handle potential leading whitespace from indentation
  $list_cl =~ s/^\s*\(progn\b/(vector/;

  # Wrap single scalar values in a vector for foreach
  # This handles: foreach (42) or foreach ($x) where $x is a scalar
  # Skip if it's already a vector, array, hash, range, or function call
  if ($list_cl !~ /^\s*\(/ && $list_cl !~ /^[@%]/) {
    $list_cl = "(vector $list_cl)";
  }

  # Build label argument if present
  my $label_arg = $label ? " :label $label" : "";

  $self->_emit("(pl-foreach ($loop_var $list_cl)$label_arg");
  $self->indent_level($self->indent_level + 1);
  $self->_process_block($block) if $block;
  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");
  $self->_emit("");
}


# Process subroutine declaration
sub _process_sub_statement {
  my $self = shift;
  my $stmt = shift;

  my $name = '';
  my $prototype = '';
  my $block;

  for my $child ($stmt->children) {
    my $ref = ref($child);

    if ($ref eq 'PPI::Token::Word' && $child->content ne 'sub') {
      $name = $child->content unless $name;
    }
    elsif ($ref eq 'PPI::Token::Prototype') {
      $prototype = $child->content;
    }
    elsif ($ref eq 'PPI::Structure::Signature') {
      # Perl 5.20+ signature (when 'use feature "signatures"' is used)
      $prototype = $child->content;
    }
    elsif ($ref eq 'PPI::Structure::Block') {
      $block = $child;
    }
  }

  # Emit Perl code as comment
  my $perl_code = $stmt->content;
  $perl_code =~ s/\{.*\}$/{ ... }/s;  # Abbreviate body
  $perl_code =~ s/\n/ /g;
  $self->_emit(";; $perl_code");

  # Parse prototype/signature
  my $sig_info = { params => [], min_params => 0, is_proto => 0 };
  if ($prototype) {
    $sig_info = $self->parse_prototype_or_signature($prototype, $stmt);
  }

  # Store in environment for later use by PExpr
  if ($name) {
    $self->environment->add_prototype($name, $sig_info);
  }

  # Build parameter list for defun
  my @param_names;
  my @optional_params;
  my $in_optional = 0;

  for my $param (@{$sig_info->{params}}) {
    my $pname = $param->{name};

    # For old-style prototypes, skip non-variable sigils
    next if $sig_info->{is_proto} && $pname !~ /^[\$\@\%]/;

    if (defined $param->{default_cl}) {
      # Parameter with default goes to &optional
      push @optional_params, { name => $pname, default => $param->{default_cl} };
      $in_optional = 1;
    }
    elsif ($pname =~ /^[\@\%]/) {
      # Slurpy parameter - use &rest
      push @optional_params, { name => $pname, rest => 1 };
    }
    elsif ($in_optional) {
      # After seeing optional, all are optional
      push @optional_params, { name => $pname, default => 'nil' };
    }
    else {
      # Required parameter
      push @param_names, $pname;
    }
  }

  # Build the parameter string
  # CL order: required &optional &rest &key
  my $params_cl = join(' ', @param_names);

  if (@optional_params) {
    my @opt_strs;
    my $rest_param;

    for my $opt (@optional_params) {
      if ($opt->{rest}) {
        $rest_param = $opt->{name};
      }
      else {
        push @opt_strs, "($opt->{name} $opt->{default})";
      }
    }

    # &optional comes before &rest
    if (@opt_strs) {
      $params_cl .= ' &optional ' . join(' ', @opt_strs);
    }

    # &rest before &key
    if ($rest_param) {
      $params_cl .= ' &rest ' . $rest_param;
    }
  }

  # If no explicit parameters, add &rest %_args to capture arguments
  # Then convert to @_ vector so shift/pop work correctly
  # wantarray is handled via *wantarray* dynamic variable (set by caller)
  my $needs_args_conversion = 0;
  if (!@param_names && !@optional_params) {
    $params_cl = '&rest %_args';
    $needs_args_conversion = 1;
  }

  # Find state declarations in the block (they need special handling)
  my @state_vars;
  if ($block) {
    my @all_decls = @{$self->_find_all_declarations($block)};
    my %seen;
    @state_vars = grep { !$seen{$_}++ }
                  map { $_->{var} }
                  grep { $_->{type} eq 'state' } @all_decls;
  }

  # If we have state vars, wrap defun in a let for persistent storage
  if (@state_vars) {
    # Create bindings: ($var nil) ($var--init nil) for each state var
    my @bindings;
    for my $var (@state_vars) {
      push @bindings, "($var nil)";
      push @bindings, "($var--init nil)";
    }
    $self->_emit("(let (" . join(" ", @bindings) . ")");
    $self->indent_level($self->indent_level + 1);
  }

  # User-defined subs get pl- prefix to avoid conflicts with CL built-ins
  # Wrap body in (block nil ...) so pl-return works
  $self->_emit("(defun pl-$name ($params_cl)");
  $self->indent_level($self->indent_level + 1);

  # If using %_args, convert to @_ vector
  if ($needs_args_conversion) {
    $self->_emit("(let ((\@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))");
    $self->indent_level($self->indent_level + 1);
  }

  $self->_emit("(block nil");
  $self->indent_level($self->indent_level + 1);

  if ($block) {
    # Wrap sub body with let for local variable declarations
    # Pass state_vars so _with_declarations knows to skip them
    local $self->{_current_state_vars} = { map { $_ => 1 } @state_vars };
    $self->_with_declarations($block, sub {
      $self->_process_block($block);
    });
  }
  else {
    $self->_emit("nil");
  }

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");  # close block

  if ($needs_args_conversion) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");  # close let
  }

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");  # close defun

  # Close state vars let
  if (@state_vars) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  }

  $self->_emit("");
}


# Process package declaration
sub _process_package_statement {
  my $self = shift;
  my $stmt = shift;

  my $pkg_name = $stmt->namespace // 'main';

  # Register package as a known class/package for method call resolution
  $self->environment->add_package($pkg_name);

  # Check for block form: package Foo { ... }
  my $block;
  for my $child ($stmt->schildren) {
    if (ref($child) eq 'PPI::Structure::Block') {
      $block = $child;
      last;
    }
  }

  if ($block) {
    # Block form: push package, process block, pop
    $self->_emit_package_preamble($pkg_name);
    $self->environment->push_package($pkg_name);

    # Process the block contents
    for my $child ($block->schildren) {
      $self->_process_element($child);
    }

    $self->environment->pop_package();
    # Switch back to previous package
    my $prev_pkg = $self->environment->current_package();
    $self->_emit("(in-package :$prev_pkg)");
    $self->_emit(";;; end package $pkg_name");
    $self->_emit("");
  }
  else {
    # Simple form: package Foo;
    # This changes the current package until another package declaration
    $self->_emit_package_preamble($pkg_name);
    $self->environment->push_package($pkg_name);
    # Note: no pop - package remains active until next package declaration
  }
}


# Emit CL package preamble (defpackage + in-package)
sub _emit_package_preamble {
  my $self     = shift;
  my $pkg_name = shift;

  $self->_emit(";;; package $pkg_name");
  $self->_emit("(defpackage :$pkg_name");
  $self->_emit("  (:use :cl :pcl))");
  $self->_emit("(in-package :$pkg_name)");
  $self->_emit("");
}


# PCL runtime symbols that might conflict with user sub names
my %PCL_SYMBOLS = map { $_ => 1 } qw(
  pl-x pl-y pl-print pl-say pl-length pl-push pl-pop pl-shift pl-unshift
  pl-keys pl-values pl-exists pl-delete pl-sort pl-reverse pl-map pl-grep
  pl-join pl-split pl-ref pl-bless pl-die pl-warn pl-open pl-close
  pl-read pl-write pl-int pl-abs pl-substr pl-index pl-lc pl-uc
);

# Check if a sub name conflicts with PCL runtime
sub _is_pcl_symbol {
  my $self = shift;
  my $name = shift;
  return exists $PCL_SYMBOLS{"pl-$name"};
}


# Process use/require statements
sub _process_include_statement {
  my $self = shift;
  my $stmt = shift;

  my $perl_code = $stmt->content;
  $perl_code =~ s/;\s*$//;

  # Check if this is 'use constant'
  my $module = $stmt->module // '';
  if ($module eq 'constant') {
    $self->_process_use_constant($stmt, $perl_code);
    return;
  }

  # Other includes - not yet implemented
  $self->_emit(";; $perl_code");
  $self->_emit(";; (include handling not yet implemented)");
  $self->_emit("");
}


# Process 'use constant' declarations
sub _process_use_constant {
  my $self      = shift;
  my $stmt      = shift;
  my $perl_code = shift;

  # Get children after 'constant'
  my @children = $stmt->schildren;
  # Skip: 'use', 'constant'
  my $i = 0;
  while ($i < @children && $children[$i]->content ne 'constant') {
    $i++;
  }
  $i++;  # Skip 'constant' itself

  # What's next determines the form
  return if $i >= @children;

  my $next = $children[$i];

  if ($next->isa('PPI::Structure::Constructor')) {
    # Hash-style: use constant { PI => 3.14, E => 2.71 };
    $self->_process_constant_hash($next, $perl_code);
  }
  elsif ($next->isa('PPI::Token::Word')) {
    # Single: use constant PI => 3.14;
    my $name = $next->content;
    # Get the value (skip => operator)
    my @value_parts;
    for my $j ($i+1 .. $#children) {
      my $child = $children[$j];
      next if $child->isa('PPI::Token::Operator') && $child->content eq '=>';
      next if $child->isa('PPI::Token::Structure');  # Skip ;
      push @value_parts, $child;
    }
    $self->_process_single_constant($name, \@value_parts, $perl_code);
  }
}


# Process hash-style constant declaration
sub _process_constant_hash {
  my $self      = shift;
  my $struct    = shift;
  my $perl_code = shift;

  $self->_emit(";; $perl_code");

  # Get the expression inside the braces
  my @contents = $struct->schildren;

  for my $content (@contents) {
    next unless $content->isa('PPI::Statement::Expression');

    # Parse the expression children for name => value pairs
    my @parts = $content->schildren;
    my $i = 0;
    while ($i < @parts) {
      # Get name
      my $name_tok = $parts[$i];
      last unless $name_tok && $name_tok->isa('PPI::Token::Word');
      my $name = $name_tok->content;
      $i++;

      # Skip =>
      $i++ while $i < @parts && $parts[$i]->isa('PPI::Token::Operator') && $parts[$i]->content eq '=>';

      # Collect value tokens until , or end
      my @value_parts;
      while ($i < @parts) {
        my $part = $parts[$i];
        last if $part->isa('PPI::Token::Operator') && $part->content eq ',';
        push @value_parts, $part;
        $i++;
      }

      # Skip comma
      $i++ if $i < @parts && $parts[$i]->isa('PPI::Token::Operator') && $parts[$i]->content eq ',';

      # Process this constant
      $self->_emit_constant($name, \@value_parts);
    }
  }
  $self->_emit("");
}


# Process single constant declaration
sub _process_single_constant {
  my $self        = shift;
  my $name        = shift;
  my $value_parts = shift;
  my $perl_code   = shift;

  $self->_emit(";; $perl_code");
  $self->_emit_constant($name, $value_parts);
  $self->_emit("");
}


# Emit a single constant definition
# Constants are implemented as zero-arg functions (like Perl does internally)
sub _emit_constant {
  my $self        = shift;
  my $name        = shift;
  my $value_parts = shift;

  # Compile the value expression to CL
  my $cl_value = $self->_compile_constant_value($value_parts);

  # Emit as a function (Perl implements constants as subs)
  $self->_emit("(defun pl-$name () $cl_value)");

  # Register as a zero-arg prototype so bareword is recognized as function call
  $self->environment->add_prototype($name, {
    params     => [],
    min_params => 0,
    is_proto   => 0,
  });
}


# Compile a constant's value expression to CL
sub _compile_constant_value {
  my $self  = shift;
  my $parts = shift;

  # Simple case: single literal
  if (@$parts == 1) {
    my $tok = $parts->[0];
    if ($tok->isa('PPI::Token::Number')) {
      return $tok->content;
    }
    elsif ($tok->isa('PPI::Token::Quote')) {
      # String - get the actual string value
      my $str = $tok->string // $tok->content;
      return '"' . $str . '"';
    }
  }

  # Complex expression: use PExpr to parse
  my $result;
  eval {
    my $expr_o = Pl::PExpr->new(
      e           => $parts,
      environment => $self->environment,
      parser      => $self,
    );
    my $node_id = $expr_o->parse_expr_to_tree($parts);
    my $gen = Pl::ExprToCL->new(
      expr_o       => $expr_o,
      environment  => $self->environment,
      indent_level => 0,
    );
    $result = $gen->generate($node_id);
  };

  return $result // '0';  # Fallback
}


# Parse an expression using PExpr and generate CL
sub _parse_expression {
  my $self  = shift;
  my $parts = shift;
  my $stmt  = shift;  # Original statement for full_PPI

  # Call the internal version that returns declarations too
  my ($result, $decls) = $self->_parse_expression_internal($parts, $stmt);

  # In scalar context, just return result (backwards compatible)
  return $result unless wantarray;

  # In list context, return result and declarations
  return ($result, $decls);
}

# Internal: parse expression and return both CL code and declarations
sub _parse_expression_internal {
  my $self  = shift;
  my $parts = shift;
  my $stmt  = shift;

  my $result;
  my @decls;

  eval {
    my $expr_o = Pl::PExpr->new(
      e           => $parts,
      full_PPI    => $stmt,
      environment => $self->environment,
      parser      => $self,
    );

    # Capture declarations in list context
    my ($node_id, $decl_list) = $expr_o->parse_expr_to_tree($parts);
    @decls = @{$decl_list // []};

    # Annotate AST with context information (scalar/list)
    $expr_o->annotate_contexts($node_id);

    my $gen = Pl::ExprToCL->new(
      expr_o       => $expr_o,
      environment  => $self->environment,
      indent_level => $self->indent_level,
    );

    $result = $gen->generate($node_id);
  };

  if ($@) {
    my $error = $@;
    $error =~ s/ at \/.*//s;  # Remove file/line info
    $error =~ s/\n.*//s;      # First line only
    return (";; PARSE ERROR: $error", []);
  }

  return ($result // ";; (no output)", \@decls);
}


# Emit a line to output
sub _emit {
  my $self = shift;
  my $line = shift;

  my $indent = "  " x $self->indent_level;
  push @{$self->output}, $indent . $line;
}


# Convenience class methods
sub parse_file {
  my $class    = shift;
  my $filename = shift;

  my $parser = $class->new(filename => $filename);
  return $parser->parse;
}


sub parse_code {
  my $class = shift;
  my $code  = shift;

  my $parser = $class->new(code => $code);
  return $parser->parse;
}


# ============================================================
# Parse a subroutine prototype or signature string.
#
# Input: prototype string like "($x, $y = 10, @rest)" or "($$;$)"
# Returns: {
#   params     => [ { name => '$x', default_cl => undef },
#                   { name => '$y', default_cl => '10' },
#                   { name => '@rest', default_cl => undef } ],
#   min_params => 1,   # minimum required parameters
#   is_proto   => 0,   # 1 if old-style prototype, 0 if signature
# }
#
# This is a separate sub so it can be moved to its own module later.
# ============================================================
sub parse_prototype_or_signature {
  my $self      = shift;
  my $proto_str = shift;
  my $context   = shift;  # PPI context for parsing defaults (e.g., the sub statement)

  # Remove surrounding parens and whitespace
  $proto_str =~ s/^\s*\(\s*//;
  $proto_str =~ s/\s*\)\s*$//;

  return { params => [], min_params => 0, is_proto => 0 } if $proto_str eq '';

  # Detect if this is an old-style prototype (no variable names, just sigils)
  # Old-style: ($$), (\@$), ($;$@)
  # New-style: ($x, $y), ($x = 10)
  my $is_proto = ($proto_str !~ /[\$\@\%]\w/);

  if ($is_proto) {
    return $self->_parse_old_prototype($proto_str);
  } else {
    return $self->_parse_signature($proto_str, $context);
  }
}


# Parse old-style prototype like "$$", "\@$", "$;$$"
sub _parse_old_prototype {
  my $self      = shift;
  my $proto_str = shift;

  my @params;
  my $min_params = 0;
  my $in_optional = 0;

  # Split into characters, handling backslash escapes
  my $i = 0;
  while ($i < length($proto_str)) {
    my $char = substr($proto_str, $i, 1);

    if ($char eq ';') {
      # Semicolon marks start of optional parameters
      $in_optional = 1;
      $i++;
      next;
    }
    elsif ($char eq '\\') {
      # Reference type: \@, \$, \%, \*
      my $next = substr($proto_str, $i + 1, 1);
      push @params, { name => "\\$next", default_cl => undef };
      $min_params++ unless $in_optional;
      $i += 2;
    }
    elsif ($char =~ /[\$\@\%\&\*_]/) {
      push @params, { name => $char, default_cl => undef };
      $min_params++ unless $in_optional || $char eq '@' || $char eq '%';
      $i++;
    }
    else {
      # Skip unknown/whitespace
      $i++;
    }
  }

  return {
    params     => \@params,
    min_params => $min_params,
    is_proto   => 1,
  };
}


# Parse new-style signature like "$x, $y = 10, @rest"
sub _parse_signature {
  my $self      = shift;
  my $sig_str   = shift;
  my $context   = shift;

  my @params;
  my $min_params = 0;
  my $seen_optional = 0;

  # Split on commas, but be careful with nested parens in defaults
  my @param_strs = $self->_split_signature_params($sig_str);

  for my $param_str (@param_strs) {
    $param_str =~ s/^\s+//;
    $param_str =~ s/\s+$//;
    next if $param_str eq '';

    my ($name, $default_expr);

    if ($param_str =~ /^([\$\@\%]\w+)\s*=\s*(.+)$/) {
      # Parameter with default: $x = 10
      $name = $1;
      $default_expr = $2;
      $seen_optional = 1;
    }
    elsif ($param_str =~ /^([\$\@\%]\w+)$/) {
      # Simple parameter: $x
      $name = $1;
      $default_expr = undef;
    }
    else {
      # Unknown format, skip
      next;
    }

    my $default_cl = undef;
    if (defined $default_expr) {
      # Compile the default expression to CL
      $default_cl = $self->_compile_default_expr($default_expr, $context);
    }

    push @params, {
      name       => $name,
      default_cl => $default_cl,
    };

    # Count mandatory params (before any optional, and not slurpy)
    if (!$seen_optional && !defined $default_expr && $name !~ /^[\@\%]/) {
      $min_params++;
    }
  }

  return {
    params     => \@params,
    min_params => $min_params,
    is_proto   => 0,
  };
}


# Split signature string on commas, respecting nested parens
sub _split_signature_params {
  my $self = shift;
  my $str  = shift;

  my @result;
  my $current = '';
  my $depth = 0;

  for my $char (split //, $str) {
    if ($char eq ',' && $depth == 0) {
      push @result, $current;
      $current = '';
    }
    else {
      $depth++ if $char eq '(' || $char eq '[' || $char eq '{';
      $depth-- if $char eq ')' || $char eq ']' || $char eq '}';
      $current .= $char;
    }
  }
  push @result, $current if $current ne '';

  return @result;
}


# Compile a default expression to CL
sub _compile_default_expr {
  my $self    = shift;
  my $expr    = shift;
  my $context = shift;

  my $result;
  eval {
    my $doc = PPI::Document->new(\$expr);
    my @stmts = $doc->children;
    return undef unless @stmts;

    my @parts = grep {
      ref($_) ne 'PPI::Token::Whitespace'
    } $stmts[0]->children;

    return undef unless @parts;

    my $expr_o = Pl::PExpr->new(
      e           => \@parts,
      full_PPI    => $doc,
      environment => $self->environment,
      parser      => $self,
    );

    my $node_id = $expr_o->parse_expr_to_tree(\@parts);

    my $gen = Pl::ExprToCL->new(
      expr_o       => $expr_o,
      environment  => $self->environment,
      indent_level => 0,
    );

    $result = $gen->generate($node_id);
  };

  if ($@) {
    warn "Failed to compile default expression '$expr': $@";
    return undef;
  }

  return $result;
}


1;

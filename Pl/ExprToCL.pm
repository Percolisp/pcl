package Pl::ExprToCL;

# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

use v5.30;
use strict;
use warnings;

use Moo;

use Scalar::Util qw/looks_like_number/;

# Code generator that transforms Pl::PExpr AST into Common Lisp code.
# Follows conventions from CODEGEN_DESIGN.md:
# - Variables keep Perl sigils ($x, @arr, %hash)
# - All operators/functions use pl- prefix
# - Pretty-printed output

has expr_o => (
  is       => 'ro',
  required => 1,
  doc      => 'Pl::PExpr object with parsed tree',
);

has environment => (
  is       => 'ro',
  doc      => 'Pl::Environment for package/class info',
);

has indent_level => (
  is       => 'rw',
  default  => 0,
);

has indent_str => (
  is       => 'ro',
  default  => '  ',
);

# L-value context tracking for array/hash element access
# When true, array/hash access should return boxes instead of values
has lvalue_context => (
  is       => 'rw',
  default  => 0,
);


has handlers => (
  is      => 'ro',
  lazy    => 1,
  builder => '_build_handlers',
);

sub _build_handlers {
  my $self = shift;
  return {
    'funcall'       => \&gen_funcall,
    'methodcall'    => \&gen_methodcall,
    'ref_funcall'   => \&gen_ref_funcall,
    'ternary'       => \&gen_ternary,
    'prefix_op'     => \&gen_prefix_op,
    'postfix_op'    => \&gen_postfix_op,
    'a_acc'         => \&gen_array_access,
    'h_acc'         => \&gen_hash_access,
    'a_ref_acc'     => \&gen_array_ref_access,
    'h_ref_acc'     => \&gen_hash_ref_access,
    'slice_a_acc'   => \&gen_array_slice,
    'slice_h_acc'   => \&gen_hash_slice,
    'kv_slice_h_acc' => \&gen_kv_hash_slice,
    'arr_init'      => \&gen_array_init,
    'hash_init'     => \&gen_hash_init,
    'progn'         => \&gen_progn,
    'tree_val'      => \&gen_tree_val,
    'filehandle'    => \&gen_filehandle,
    'readline'      => \&gen_readline,
    'glob'          => \&gen_glob,
    'backtick'      => \&gen_backtick,
    'anon_sub'      => \&gen_anon_sub,
    'func_ref'      => \&gen_func_ref,
    'inline_lambda' => \&gen_inline_lambda,
    'string_concat' => \&gen_string_concat,
  };
}

# Only exceptions that need different CL names than pl-<perl-op>
# If not listed here, the CL name is just pl-<perl-name>
my %OP_EXCEPTIONS = (
  # Bitwise operators - avoid confusion with CL's & and |
  '&'   => 'pl-bit-and',
  '|'   => 'pl-bit-or',
  '^'   => 'pl-bit-xor',
  '~'   => 'pl-bit-not',

  # Assignment variants with clearer names
  '='   => 'pl-setf',
  '+='  => 'pl-incf',
  '-='  => 'pl-decf',

  # Compound assignment - bitwise
  '&='  => 'pl-bit-and=',
  '|='  => 'pl-bit-or=',
  '^='  => 'pl-bit-xor=',

  # Compound assignment - logical
  '&&=' => 'pl-and-assign',
  '||=' => 'pl-or-assign',

  # Reference operator
  '\\'  => 'pl-backslash',

  # Note: Sigil cast operators (@, %, $) are handled in gen_prefix_op
  # They can't be in OP_EXCEPTIONS because % is also the modulo operator

  # Operators with names that could conflict with user subs
  # (these are valid Perl identifiers, so code can define sub x, sub eq, etc.)
  'x'   => 'pl-str-x',
  'x='  => 'pl-str-x=',
  'lt'  => 'pl-str-lt',
  'gt'  => 'pl-str-gt',
  'le'  => 'pl-str-le',
  'ge'  => 'pl-str-ge',
  'eq'  => 'pl-str-eq',
  'ne'  => 'pl-str-ne',
  'cmp' => 'pl-str-cmp',
);

# Magic/special variables that need specific CL output
# Maps Perl variable name to its CL representation
my %SPECIAL_VARS = (
  '$!'  => '(pl-errno-string)',
  '$?'  => '$?',
  '$.'  => '|$.|',
  '$0'  => '$0',
  '$@'  => '$@',
  '$^O' => '|$^O|',
  '$^V' => '|$^V|',
  '$^X' => '|$^X|',
  '$/'  => '|$/|',
  '$\\' => '|$\\|',
  '$"'  => '|$"|',
  '$|'  => '|$\||',
  '$;'  => '|$;|',
  '$,'  => '|$,|',
  '$]'  => '|$]|',
);

# Generate CL operator/function name from Perl name
# - Package-qualified names (Foo::bar) → Foo:bar (CL package syntax)
# - Operator exceptions → from %OP_EXCEPTIONS
# - Built-in functions → pl-<name>
# - User-defined functions → <name> (in current CL package)
sub cl_name {
  my $self      = shift;
  my $perl_name = shift;

  # Guard against undefined input
  return 'pl-UNDEFINED' unless defined $perl_name && length($perl_name);

  # Check for operator exceptions first
  return $OP_EXCEPTIONS{$perl_name} if exists $OP_EXCEPTIONS{$perl_name};

  # Check for package-qualified name (Foo::bar or Foo::Bar::baz)
  if ($perl_name =~ /^(.+)::(.+)$/) {
    my ($pkg, $func) = ($1, $2);
    # Record package reference for pre-declaration
    $self->environment->add_referenced_package($pkg) if $self->environment;
    # Use pipe-quoting if package contains :: (e.g., |Foo::Bar|::pl-func)
    my $cl_pkg = $pkg =~ /::/ ? "|$pkg|" : $pkg;
    return "${cl_pkg}::pl-${func}";
  }

  # All functions (built-in and user-defined) get pl- prefix
  return "pl-$perl_name";
}


# Get context keyword for a node (:scalar or :list)
sub get_context_keyword {
  my $self    = shift;
  my $node_id = shift;

  my $ctx = $self->expr_o->get_node_context($node_id);
  # SCALAR_CTX = 0, LIST_CTX = 1
  return $ctx == 1 ? ':list' : ':scalar';
}


# Main entry point: generate CL code from AST
sub generate {
  my $self    = shift;
  my $node_id = shift // $self->expr_o->root;

  return ($self->indent_str x $self->indent_level) . $self->gen_node($node_id);
}


# Generate code for a single node
sub gen_node {
  my $self    = shift;
  my $node_id = shift;

  my $node    = $self->expr_o->get_a_node($node_id);
  my $kids    = $self->expr_o->get_node_children($node_id);

  # Internal node (PPIreference with type)
  if ($self->expr_o->is_internal_node_type($node)) {
    return $self->gen_internal_node($node, $node_id, $kids);
  }

  # Binary operator: PPI::Token::Operator with children
  # (Binary ops are stored as operator tokens, not PPIreference)
  if (ref($node) eq 'PPI::Token::Operator' && @$kids) {
    my $op = $node->content();
    return $self->gen_binary_op($op, $kids, $node_id);
  }

  # Leaf node (PPI token)
  return $self->gen_leaf($node);
}


# Generate code for internal nodes (operators, funcalls, etc.)
sub gen_internal_node {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $type    = $node->{type};

  # Dispatch based on node type
  my $handler = $self->handlers->{$type};

  if ($handler) {
    return $handler->($self, $node, $node_id, $kids);
  }

  # Assume it's a binary operator (operators stored with operator as type)
  return $self->gen_binary_op($type, $kids, $node_id);
}


# Generate code for leaf nodes (literals, variables)
sub gen_leaf {
  my $self = shift;
  my $node = shift;

  my $ref  = ref($node);

  # Variable (like $x, @arr, %hash)
  if ($ref eq 'PPI::Token::Symbol' || $ref eq 'PPI::Token::Magic') {
    my $content = $node->content() // '';
    # Handle magic/special variables via dispatch table
    return $SPECIAL_VARS{$content} if exists $SPECIAL_VARS{$content};
    # Handle package-qualified variables: $Pkg::var -> Pkg::$var
    # Perl: $Config::debug  ->  CL: Config::$debug
    # Also: $::foo means $main::foo (empty package = main)
    # Note: Use [^:]+ at the end to avoid matching stash refs like $Pkg::Sub::
    if ($content =~ /^([\$\@\%])(.*)::([^:]+)$/) {
      my ($sigil, $pkg, $name) = ($1, $2, $3);
      # Empty package means main (e.g., $::foo = $main::foo)
      $pkg = 'main' if $pkg eq '';
      # Track referenced package
      $self->environment->add_referenced_package($pkg) if $self->environment;
      # Use pipe quoting for nested packages
      my $cl_pkg = $pkg =~ /::/ ? "|$pkg|" : $pkg;
      return "${cl_pkg}::${sigil}${name}";
    }
    # Handle package-qualified typeglobs: *Pkg::var -> |Pkg|::*var
    # Perl: *Scalar::Util::refaddr  ->  CL: |Scalar::Util|::*refaddr
    # Also: *::foo means *main::foo (empty package = main)
    if ($content =~ /^\*(.*)::([^:]+)$/) {
      my ($pkg, $name) = ($1, $2);
      # Empty package means main (e.g., *::foo = *main::foo)
      $pkg = 'main' if $pkg eq '';
      # Track referenced package
      $self->environment->add_referenced_package($pkg) if $self->environment;
      # Use pipe quoting for package (which may contain ::)
      my $cl_pkg = "|$pkg|";
      return "${cl_pkg}::*${name}";
    }
    # Handle package stash access: $Pkg::Sub:: or %Pkg::Sub::
    # Perl: $YAML::Tiny:: or %YAML::Tiny:: -> CL: (pl-stash "YAML::Tiny")
    # Also: $:: or %:: means main stash
    if ($content =~ /^([\$\%])(.*)::$/) {
      my ($sigil, $pkg) = ($1, $2);
      # Empty package means main (e.g., $:: = main stash)
      $pkg = 'main' if $pkg eq '';
      # Track referenced package
      $self->environment->add_referenced_package($pkg) if $self->environment;
      return "(pl-stash \"$pkg\")";
    }
    # Handle &subname - call subroutine
    # &foo -> (pl-foo) - calls without passing @_ through
    # Note: &foo(@args) would be handled as funcall, not here
    if ($content =~ /^&(.+)$/) {
      my $func_name = $1;
      my $cl_func = $self->cl_name($func_name);
      return "($cl_func)";
    }
    return $content;
  }

  # Array last index ($#arr)
  if ($ref eq 'PPI::Token::ArrayIndex') {
    my $content = $node->content();
    # $#arr -> (pl-array-last-index @arr)
    $content =~ s/^\$#/@/;
    return "(pl-array-last-index $content)";
  }

  # Number literal - convert Perl format to CL format
  # (includes subclasses: ::Hex, ::Binary, ::Octal, ::Float, ::Exp, ::Version)
  if ($ref =~ /^PPI::Token::Number/) {
    my $num = $node->content();

    # Version strings: v1.20.300 -> string of chr values
    if ($num =~ /^v(\d[\d.]*)$/) {
      my @parts = split /\./, $1;
      my $str = join('', map { chr($_) } @parts);
      # Escape for CL string literal
      $str =~ s/\\/\\\\/g;
      $str =~ s/"/\\"/g;
      # For non-printable chars, use CL character escapes or just embed
      # Convert to a runtime call for safety with high codepoints
      my $args = join(' ', @parts);
      return "(pl-version-string $args)";
    }

    # Hex: 0x1234 or 0X1234 -> #x1234 (with optional leading -)
    if ($num =~ /^(-?)0[xX]([0-9a-fA-F_]+)$/) {
      my ($sign, $hex) = ($1, $2);
      $hex =~ s/_//g;  # Remove underscores
      return $sign ? "(- #x$hex)" : "#x$hex";
    }
    # Binary: 0b1010 or 0B1010 -> #b1010 (with optional leading -)
    if ($num =~ /^(-?)0[bB]([01_]+)$/) {
      my ($sign, $bin) = ($1, $2);
      $bin =~ s/_//g;
      return $sign ? "(- #b$bin)" : "#b$bin";
    }
    # Octal: 0o777 or 0O777 -> #o777 (Perl 5.34+ syntax, with optional leading -)
    if ($num =~ /^(-?)0[oO]([0-7_]+)$/) {
      my ($sign, $oct) = ($1, $2);
      $oct =~ s/_//g;
      return $sign ? "(- #o$oct)" : "#o$oct";
    }
    # Legacy octal: 0777 (but not 0 alone) -> #o777 (with optional leading -)
    if ($num =~ /^(-?)0([0-7_]+)$/ && $num ne '0') {
      my ($sign, $oct) = ($1, $2);
      $oct =~ s/_//g;
      return $sign ? "(- #o$oct)" : "#o$oct";
    }
    # Remove underscores from regular numbers (Perl allows 1_000_000)
    $num =~ s/_//g;
    return $num;
  }

  # Compiled regex qr// (check before Quote to avoid catching QuoteLike::Regexp)
  if ($ref eq 'PPI::Token::QuoteLike::Regexp') {
    my $content = $node->content();
    # Escape backslashes and double quotes for CL string literal
    $content =~ s/\\/\\\\/g;
    $content =~ s/"/\\"/g;
    return qq{(pcl::pl-qr "$content")};
  }

  # Heredoc <<'EOF' or <<"EOF" or <<EOF
  if ($ref eq 'PPI::Token::HereDoc') {
    # PPI::Token::HereDoc has heredoc() method to get the content lines
    my @lines = $node->heredoc();
    my $content = join('', @lines);
    # Escape backslashes and double quotes for CL string literal
    $content =~ s/\\/\\\\/g;
    $content =~ s/"/\\"/g;
    # Remove trailing newline if present (heredocs include it)
    chomp $content;
    return qq{"$content"};
  }

  # String literals
  if ($ref =~ /^PPI::Token::Quote/) {
    my $content = $node->content();
    # Convert Perl escape sequences to actual characters for CL
    return $self->convert_perl_string($content);
  }

  # Bareword (function name, etc.)
  if ($ref eq 'PPI::Token::Word') {
    my $content = $node->can('content') ? ($node->content() // '') : '';
    # Handle __FILE__ and __LINE__ compile-time tokens
    if ($content eq '__FILE__') {
      my $source_file = $self->environment ? $self->environment->source_file : '-';
      $source_file //= '-';
      return qq{"$source_file"};
    }
    if ($content eq '__LINE__') {
      my $line = $node->line_number // 0;
      return $line;
    }
    return $content;
  }

  # Operator token (used as child of prefix_op, etc.)
  if ($ref eq 'PPI::Token::Operator') {
    return $node->content();
  }

  # Substitution s///
  if ($ref eq 'PPI::Token::Regexp::Substitute') {
    return $self->gen_substitution($node);
  }

  # Transliteration tr/// or y///
  if ($ref eq 'PPI::Token::Regexp::Transliterate') {
    return $self->gen_transliteration($node);
  }

  # Match regex m// or //
  if ($ref =~ /^PPI::Token::Regexp/) {
    my $content = $node->content();
    # Escape backslashes and double quotes for CL string literal
    $content =~ s/\\/\\\\/g;
    $content =~ s/"/\\"/g;
    return qq{(pl-regex "$content")};
  }

  # Cast (deref sigil)
  if ($ref eq 'PPI::Token::Cast') {
    return $node->content();
  }

  # Fallback
  if ($node->can('content')) {
    return $node->content();
  }

  return "(pl-UNKNOWN-LEAF)";
}


# Binary operator: (pcl:pl-OP left right)
# Operators always use pcl: prefix to avoid conflicts with user-defined subs
sub gen_binary_op {
  my $self    = shift;
  my $op      = shift;
  my $kids    = shift;
  my $node_id = shift;  # Optional: for context-dependent operators like 'x'

  my $cl_op = $self->cl_op_name($op);

  # Special case: 'x' operator - use list repeat when LHS is parenthesized and in list context
  if ($op eq 'x' && defined $node_id) {
    my $lhs_node = $self->expr_o->get_a_node($kids->[0]);
    my $lhs_is_paren = $self->expr_o->is_internal_node_type($lhs_node) &&
                       ($lhs_node->{type} eq 'tree_val' || $lhs_node->{type} eq 'progn');
    my $ctx = $self->expr_o->get_node_context($node_id);
    if ($lhs_is_paren && $ctx == 1) {  # LIST_CTX = 1
      # List repeat: (@x) x 4 in list context
      my $left  = $self->gen_node($kids->[0]);
      my $right = $self->gen_node($kids->[1]);
      return "(pl-list-x $left $right)";
    }
  }

  my $left  = $self->gen_node($kids->[0]);

  # Special case: hash assignment with list
  # %h = () or %h = (a => 1) should use (pl-hash ...), not (vector ...)
  if ($op eq '=' && $left =~ /^%/) {
    my $rhs_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($rhs_node)) {
      my $rhs_type = $rhs_node->{type};
      if ($rhs_type eq 'tree_val' || $rhs_type eq 'progn') {
        my $rhs_kids = $self->expr_o->get_node_children($kids->[1]);
        if (@$rhs_kids == 0) {
          # Empty hash
          return "(pl-hash-= $left (pl-hash))";
        } else {
          # Hash with initial values - generate (pl-hash k1 v1 k2 v2 ...)
          my @parts = map { $self->gen_node($_) } @$rhs_kids;
          my $hash_init = "(pl-hash " . join(" ", @parts) . ")";
          return "(pl-hash-= $left $hash_init)";
        }
      }
    }
  }

  my $right = $self->gen_node($kids->[1]);

  # Special case: keys(%h) = N is hash pre-sizing - no-op in CL
  # CL hash tables auto-resize, so just evaluate the RHS for side effects
  if ($op eq '=' && $left =~ /^\(pl-keys /) {
    return "$right";
  }

  # Special case: $#arr = N  →  (pl-set-array-length @arr N)
  if ($op eq '=' && $left =~ /^\(pl-array-last-index (.+)\)$/) {
    my $arr = $1;
    return "(pl-set-array-length $arr $right)";
  }

  # Special case: typeglob assignment with function reference
  # *freeze = \&Dump  →  (setf (symbol-function 'pl-freeze) #'pl-Dump)
  if ($op eq '=' && $left =~ /^\*(\w+)$/) {
    my $glob_name = $1;
    if ($right =~ /^#'/) {
      my $cl_func_name = $self->cl_name($glob_name);
      return "(setf (symbol-function '$cl_func_name) $right)";
    }
  }

  # For assignment, dispatch to type-specific forms based on LHS sigil
  if ($op eq '=') {
    if ($left =~ /^\(vector /) {
      return "(pl-list-= $left $right)";
    } elsif ($left =~ /^[\@]/) {
      return "(pl-array-= $left $right)";
    } elsif ($left =~ /^%/) {
      return "(pl-hash-= $left $right)";
    } elsif ($left =~ /^\$/) {
      return "(pl-scalar-= $left $right)";
    }
    # Element access, slices, etc. - keep using pl-setf
  }

  return "($cl_op $left $right)";
}

# Generate CL name for an OPERATOR
# No prefix needed - pcl is in use list of all generated packages
sub cl_op_name {
  my $self = shift;
  my $op   = shift;

  # Check for operator exceptions first
  if (exists $OP_EXCEPTIONS{$op}) {
    return $OP_EXCEPTIONS{$op};
  }

  return "pl-$op";
}


# String concatenation with multiple parts
# Handles array interpolation: "@arr" joins elements with $" (default: space)
sub gen_string_concat {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my @parts;
  for my $kid_id (@$kids) {
    my $kid_node = $self->expr_o->get_a_node($kid_id);
    my $generated = $self->gen_node($kid_id) // '';

    # Check if this is an array variable (@arr) - needs to be joined
    my $kid_content = (ref($kid_node) eq 'PPI::Token::Symbol' && $kid_node->can('content'))
                      ? ($kid_node->content() // '') : '';
    if ($kid_content =~ /^@/) {
      # In Perl, "@arr" in string interpolation joins with $" (default space)
      # Use |$"| which is the CL variable for Perl's $" list separator
      push @parts, '(pl-join |$"| ' . $generated . ')';
    } else {
      push @parts, $generated;
    }
  }
  return "(pl-string-concat " . join(" ", @parts) . ")";
}


# Function call: (pl-FUNC args...)
sub gen_funcall {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # Check for __FILE__ and __LINE__ as zero-arg "functions"
  # (Parser wraps them in funcall when followed by operators)
  if (@$kids == 1) {
    my $func_node = $self->expr_o->get_a_node($kids->[0]);
    if (ref($func_node) eq 'PPI::Token::Word' && $func_node->can('content')) {
      my $content = $func_node->content() // '';
      if ($content eq '__FILE__') {
        my $source_file = $self->environment
	    ? $self->environment->source_file : '-';
        $source_file //= '-';
        return qq{"$source_file"};
      }
      if ($content eq '__LINE__') {
        my $line = $func_node->line_number // 0;
        return $line;
      }
      if ($content eq '__PACKAGE__') {
        my $pkg = $self->environment
	    ? $self->environment->current_package : 'main';
        $pkg //= 'main';
        return qq{"$pkg"};
      }
      # Perl: -bareword produces string "-bareword"
      # PPI tokenizes this as a single PPI::Token::Word
      if ($content =~ /^-[A-Za-z_]\w*$/) {
        return qq{"$content"};
      }
    }
  }

  # First child is function name
  my $func_name = $self->gen_node($kids->[0]);
  my $cl_func   = $self->cl_name($func_name);

  # Special handling for next/last/redo with label argument
  if (($func_name eq 'next' || $func_name eq 'last' || $func_name eq 'redo') && @$kids == 2) {
    # Check if the argument is a bareword label (funcall with single word child)
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'funcall') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids == 1) {
        my $label_node = $self->expr_o->get_a_node($arg_kids->[0]);
        if (ref($label_node) eq 'PPI::Token::Word') {
          my $label = $label_node->content();
          return "($cl_func $label)";
        }
      }
    }
  }

  # Special handling for eval { } blocks
  # eval { block } catches exceptions and sets $@
  if ($func_name eq 'eval' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($arg_node)) {
      if ($arg_node->{type} eq 'anon_sub') {
        # eval { block } with inline anon_sub - generate pl-eval-block with body
        my $block_kids = $self->expr_o->get_node_children($kids->[1]);
        my @body_parts;
        for my $kid_id (@$block_kids) {
          push @body_parts, $self->gen_node($kid_id);
        }
        my $body = join(' ', @body_parts);
        return "(pl-eval-block $body)";
      }
      elsif ($arg_node->{type} eq 'func_ref') {
        # eval { block } with named function (from Parser callback)
        # Generate pl-eval-block that calls the function
        my $func_ref = $self->gen_node($kids->[1]);
        return "(pl-eval-block (funcall $func_ref))";
      }
    }
  }

  # Special handling for grep/map expression form (without block)
  # grep EXPR, LIST  →  (pl-grep (lambda ($_) EXPR) LIST)
  # The EXPR typically uses $_ which should be the lambda parameter
  if (($func_name eq 'grep' || $func_name eq 'map') && @$kids >= 2) {
    my $first_arg_node = $self->expr_o->get_a_node($kids->[1]);
    my $is_lambda_form = $self->expr_o->is_internal_node_type($first_arg_node) &&
                         ($first_arg_node->{type} eq 'inline_lambda' ||
                          $first_arg_node->{type} eq 'func_ref' ||
                          $first_arg_node->{type} eq 'anon_sub');

    if (!$is_lambda_form) {
      # Expression form: wrap first arg in lambda
      my $expr_cl = $self->gen_node($kids->[1]);
      my @rest_args;
      for my $i (2 .. $#$kids) {
        push @rest_args, $self->gen_node($kids->[$i]);
      }
      my $list_str = join(' ', @rest_args);
      return "($cl_func (lambda (\$_) $expr_cl) $list_str)";
    }
  }

  # Special handling for bless(REF, CLASSNAME)
  # The classname can be a bareword like MyClass or o:: which should be a string
  if ($func_name eq 'bless' && @$kids >= 2) {
    my $ref_arg = $self->gen_node($kids->[1]);
    my $class_arg = '"main"';  # Default class is caller's package

    if (@$kids >= 3) {
      my $class_node = $self->expr_o->get_a_node($kids->[2]);
      my $is_bareword = 0;

      # Check if it's a bareword (funcall with single word child that's just a Word)
      if ($self->expr_o->is_internal_node_type($class_node) &&
          $class_node->{type} eq 'funcall') {
        my $class_kids = $self->expr_o->get_node_children($kids->[2]);
        # Bareword funcalls have exactly 1 child (the word itself, no arguments)
        if (@$class_kids == 1) {
          my $word_node = $self->expr_o->get_a_node($class_kids->[0]);
          if (ref($word_node) eq 'PPI::Token::Word') {
            my $classname = $word_node->content();
            # Handle special tokens: __PACKAGE__, __FILE__, __LINE__
            if ($classname eq '__PACKAGE__') {
              my $pkg = $self->environment
                  ? $self->environment->current_package : 'main';
              $pkg //= 'main';
              $class_arg = qq{"$pkg"};
              $is_bareword = 1;
            } else {
              # Regular bareword class name - remove trailing :: if present (o:: -> o)
              $classname =~ s/::$//;
              $class_arg = qq{"$classname"};
              $is_bareword = 1;
            }
          }
        }
      }

      # Not a bareword - generate normally (could be string, shift, or other expression)
      if (!$is_bareword) {
        $class_arg = $self->gen_node($kids->[2]);
      }
    }
    return "(pl-bless $ref_arg $class_arg)";
  }

  # Special handling for push/unshift: flatten @array arguments
  # In Perl, push(@x, @y) flattens @y, but push(@x, [1,2,3]) doesn't flatten the anon array
  # We detect @-sigiled expressions at code-gen time and wrap them with pl-flatten
  if (($func_name eq 'push' || $func_name eq 'unshift') && @$kids >= 2) {
    my $target = $self->gen_node($kids->[1]);  # First arg is target array
    my @items;
    for my $i (2 .. $#$kids) {
      my $arg_node = $self->expr_o->get_a_node($kids->[$i]);
      my $arg = $self->gen_node($kids->[$i]);
      my $should_flatten = 0;

      # Check if this is an @-sigiled variable (e.g., @arr)
      if (ref($arg_node) eq 'PPI::Token::Symbol') {
        my $sigil = substr($arg_node->content(), 0, 1);
        $should_flatten = 1 if $sigil eq '@';
      }
      # Check if this is an array deref (e.g., @$ref, @{expr})
      # These are prefix_op nodes with @ Cast as first child
      elsif ($self->expr_o->is_internal_node_type($arg_node) &&
             $arg_node->{type} eq 'prefix_op') {
        my $arg_kids = $self->expr_o->get_node_children($kids->[$i]);
        if (@$arg_kids >= 1) {
          my $cast_node = $self->expr_o->get_a_node($arg_kids->[0]);
          if (ref($cast_node) eq 'PPI::Token::Cast' && $cast_node->content() eq '@') {
            $should_flatten = 1;
          }
        }
      }

      if ($should_flatten) {
        # Wrap with pl-flatten to expand array elements
        $arg = "(pl-flatten $arg)";
      }
      push @items, $arg;
    }
    my $items_str = @items ? ' ' . join(' ', @items) : '';
    return "($cl_func $target$items_str)";
  }

  # Special handling for delete on arrays: delete $a[idx]
  # Need to pass array and index separately, not the dereferenced value
  if ($func_name eq 'delete' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($arg_node) &&
        $arg_node->{type} eq 'a_acc') {
      # Array access: delete $a[idx] -> (pl-delete-array @arr idx)
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $arr_node = $self->expr_o->get_a_node($arg_kids->[0]);
        my $arr = $self->gen_node($arg_kids->[0]);
        # Convert $a to @a for array
        if (ref($arr_node) eq 'PPI::Token::Symbol' && $arr =~ /^\$/) {
          $arr =~ s/^\$/\@/;
        }
        my $idx = $self->gen_node($arg_kids->[1]);
        return "(pl-delete-array $arr $idx)";
      }
    }
    # Hash access: delete $h{key} -> (pl-delete %h key)
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'h_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $hash_node = $self->expr_o->get_a_node($arg_kids->[0]);
        my $hash = $self->gen_node($arg_kids->[0]);
        # Convert $h to %h for hash
        if (ref($hash_node) eq 'PPI::Token::Symbol' && $hash =~ /^\$/) {
          $hash =~ s/^\$/\%/;
        }
        my $key = $self->gen_node($arg_kids->[1]);
        return "(pl-delete $hash $key)";
      }
    }
    # Hash slice: delete @foo{4,5} -> (pl-delete-hash-slice %hash key1 key2 ...)
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'slice_h_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $hash_node = $self->expr_o->get_a_node($arg_kids->[0]);
        my $hash = $self->gen_node($arg_kids->[0]);
        # Convert @ to % for hash access (@ is context sigil, % is container sigil)
        if (ref($hash_node) eq 'PPI::Token::Symbol' && $hash =~ /^\@/) {
          $hash =~ s/^\@/\%/;
        }
        my @keys;
        for my $i (1 .. $#$arg_kids) {
          push @keys, $self->gen_node($arg_kids->[$i]);
        }
        my $keys_str = join(' ', @keys);
        return "(pl-delete-hash-slice $hash $keys_str)";
      }
    }
    # Array slice: delete @arr[1,2,3] -> (pl-delete-array-slice @arr idx1 idx2 ...)
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'slice_a_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $arr = $self->gen_node($arg_kids->[0]);
        my @indices;
        for my $i (1 .. $#$arg_kids) {
          push @indices, $self->gen_node($arg_kids->[$i]);
        }
        my $idx_str = join(' ', @indices);
        return "(pl-delete-array-slice $arr $idx_str)";
      }
    }
    # KV slice delete: delete %foo{6,7} -> (pl-delete-kv-hash-slice %hash key1 key2 ...)
    elsif ($self->expr_o->is_internal_node_type($arg_node) &&
           $arg_node->{type} eq 'kv_slice_h_acc') {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        my $hash = $self->gen_node($arg_kids->[0]);
        my @keys;
        for my $i (1 .. $#$arg_kids) {
          push @keys, $self->gen_node($arg_kids->[$i]);
        }
        my $keys_str = join(' ', @keys);
        return "(pl-delete-kv-hash-slice $hash $keys_str)";
      }
    }
  }

  # Special handling for exists on arrays and hashes
  # Need to pass container and key/index separately
  if ($func_name eq 'exists' && @$kids == 2) {
    my $arg_node = $self->expr_o->get_a_node($kids->[1]);
    if ($self->expr_o->is_internal_node_type($arg_node)) {
      my $arg_kids = $self->expr_o->get_node_children($kids->[1]);
      if (@$arg_kids >= 2) {
        if ($arg_node->{type} eq 'a_acc') {
          # Array access: exists $a[idx] -> (pl-exists-array @arr idx)
          my $arr_node = $self->expr_o->get_a_node($arg_kids->[0]);
          my $arr = $self->gen_node($arg_kids->[0]);
          if (ref($arr_node) eq 'PPI::Token::Symbol' && $arr =~ /^\$/) {
            $arr =~ s/^\$/\@/;
          }
          my $idx = $self->gen_node($arg_kids->[1]);
          return "(pl-exists-array $arr $idx)";
        }
        elsif ($arg_node->{type} eq 'h_acc') {
          # Hash access: exists $h{key} -> (pl-exists %h key)
          my $hash_node = $self->expr_o->get_a_node($arg_kids->[0]);
          my $hash = $self->gen_node($arg_kids->[0]);
          if (ref($hash_node) eq 'PPI::Token::Symbol' && $hash =~ /^\$/) {
            $hash =~ s/^\$/\%/;
          }
          my $key = $self->gen_node($arg_kids->[1]);
          return "(pl-exists $hash $key)";
        }
      }
    }
  }

  # Check for reference prototype that requires auto-boxing
  my $proto = $self->environment ? $self->environment->get_prototype($func_name) : undef;
  my @ref_params;
  if ($proto && $proto->{is_proto} && $proto->{params}) {
    # Use proto_type for auto-boxing checks (contains original like \@, \%, $, etc.)
    @ref_params = map { $_->{proto_type} // $_->{name} } @{$proto->{params}};
  }

  # Functions that modify their arguments (need l-value access to array/hash elements)
  my %lvalue_funcs = map { $_ => 1 } qw(chop chomp);
  my $needs_lvalue = $lvalue_funcs{$func_name} // 0;

  # Rest are arguments
  my @args;
  for my $i (1 .. $#$kids) {
    # Set l-value context for functions that modify their arguments
    my $saved_lvalue = $self->lvalue_context;
    $self->lvalue_context(1) if $needs_lvalue;
    my $arg = $self->gen_node($kids->[$i]);
    $self->lvalue_context($saved_lvalue);

    # Check if this position has a reference prototype (\@, \%, \$)
    my $param_idx = $i - 1;  # 0-based index for params
    if ($param_idx < @ref_params) {
      my $param_type = $ref_params[$param_idx];
      if ($param_type =~ /^\\([@%\$])$/) {
        my $expected_sigil = $1;
        # Check if arg is an unref'd array/hash/scalar that needs wrapping
        my $arg_node = $self->expr_o->get_a_node($kids->[$i]);
        if (ref($arg_node) eq 'PPI::Token::Symbol') {
          my $arg_sigil = substr($arg_node->content(), 0, 1);
          # If arg sigil matches expected and it's not already a reference
          if ($arg_sigil eq $expected_sigil) {
            $arg = "(pl-backslash $arg)";
          }
        }
      }
    }
    push @args, $arg;
  }

  my $args_str = @args ? ' ' . join(' ', @args) : '';
  my $call = "($cl_func$args_str)";

  # Wrap in dynamic wantarray binding for list context
  my $ctx = $self->expr_o->get_node_context($node_id);
  if ($ctx == 1) {  # LIST_CTX = 1
    return "(let ((*wantarray* t)) $call)";
  }

  # split in scalar context returns number of fields
  if ($func_name eq 'split' && $ctx == 0) {
    return "(length $call)";
  }

  return $call;
}


# Method call: (pl-method-call obj 'method args...)
sub gen_methodcall {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # First child is object, second is method name
  my $obj_node = $self->expr_o->get_a_node($kids->[0]);
  my $obj;

  # Check if object is a class name (funcall with no args = bare word like Counter)
  # Disambiguation at compile time when possible, otherwise defer to runtime:
  #   - Known package (e.g., Counter->new()) → string "Counter"
  #   - Known function (e.g., foo->method() where foo is a sub) → function call
  #   - Unknown bareword → use pl-resolve-invocant for runtime dispatch
  #     (In Perl, Foo->bar() checks if sub Foo exists before treating as class)
  if ($self->expr_o->is_internal_node_type($obj_node) &&
      $obj_node->{type} eq 'funcall') {
    my $obj_kids = $self->expr_o->get_node_children($kids->[0]);
    if (@$obj_kids == 1) {
      # Single child = potential bare class name or function call
      my $class_node = $self->expr_o->get_a_node($obj_kids->[0]);
      if (ref($class_node) eq 'PPI::Token::Word') {
        my $name = $class_node->content();
        if ($self->environment && $self->environment->is_package($name)) {
          # Known package → class name string
          $obj = '"' . $name . '"';
        } elsif ($self->environment && $self->environment->has_prototype($name)) {
          # Known function → function call
          $obj = $self->gen_node($kids->[0]);
        } else {
          # Unknown bareword → runtime dispatch (checks for sub first, then class)
          $obj = '(pl-resolve-invocant "' . $name . '")';
        }
      } else {
        $obj = $self->gen_node($kids->[0]);
      }
    } else {
      $obj = $self->gen_node($kids->[0]);
    }
  } else {
    $obj = $self->gen_node($kids->[0]);
  }

  my $method_node = $self->expr_o->get_a_node($kids->[1]);
  my $method  = $self->gen_node($kids->[1]);

  # Check if method name is a variable (dynamic method call)
  my $is_dynamic_method = 0;
  if (ref($method_node) eq 'PPI::Token::Symbol' && $method_node->content() =~ /^\$/) {
    $is_dynamic_method = 1;
  }

  # Rest are arguments
  my @args;
  for my $i (2 .. $#$kids) {
    push @args, $self->gen_node($kids->[$i]);
  }

  my $args_str = @args ? ' ' . join(' ', @args) : '';

  # Check for SUPER:: method call
  my $call;
  if ($method =~ /^SUPER::(.+)$/) {
    my $real_method = $1;
    # Need current package for SUPER:: lookup
    my $current_pkg = $self->environment ? $self->environment->current_package : 'main';
    $call = "(pl-super-call $obj '$real_method \"$current_pkg\"$args_str)";
  } elsif ($is_dynamic_method) {
    # Dynamic method call: $obj->$method_var
    # Method name is in a variable, pass the variable value
    $call = "(pl-method-call $obj $method$args_str)";
  } else {
    $call = "(pl-method-call $obj '$method$args_str)";
  }

  # Wrap in dynamic wantarray binding for list context
  my $ctx = $self->expr_o->get_node_context($node_id);
  if ($ctx == 1) {  # LIST_CTX = 1
    return "(let ((*wantarray* t)) $call)";
  }
  return $call;
}


# Code ref call: (pl-funcall-ref ref args...)
sub gen_ref_funcall {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # First child is the code reference
  my $ref = $self->gen_node($kids->[0]);

  # Rest are arguments
  my @args;
  for my $i (1 .. $#$kids) {
    push @args, $self->gen_node($kids->[$i]);
  }

  my $args_str = @args ? ' ' . join(' ', @args) : '';
  my $call = "(pl-funcall-ref $ref$args_str)";

  # Wrap in dynamic wantarray binding for list context
  my $ctx = $self->expr_o->get_node_context($node_id);
  if ($ctx == 1) {  # LIST_CTX = 1
    return "(let ((*wantarray* t)) $call)";
  }
  return $call;
}


# Ternary: (pl-if cond then else)
sub gen_ternary {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # Check if condition is wantarray - if so, the 'then' branch should be in list context
  my $cond_node = $self->expr_o->get_a_node($kids->[0]);
  my $is_wantarray_cond = 0;

  if ($self->expr_o->is_internal_node_type($cond_node) &&
      $cond_node->{type} eq 'funcall') {
    my $cond_kids = $self->expr_o->get_node_children($kids->[0]);
    if (@$cond_kids) {
      my $func_node = $self->expr_o->get_a_node($cond_kids->[0]);
      if (!$self->expr_o->is_internal_node_type($func_node) &&
          $func_node->can('content') &&
          $func_node->content eq 'wantarray') {
        $is_wantarray_cond = 1;
      }
    }
  }

  my $cond = $self->gen_node($kids->[0]);

  # If condition is wantarray, set list context on 'then' branch
  if ($is_wantarray_cond) {
    $self->expr_o->set_node_context($kids->[1], 1);  # LIST_CTX = 1
  }

  my $then  = $self->gen_node($kids->[1]);
  my $else  = $self->gen_node($kids->[2]);

  return "(pl-if $cond $then $else)";
}


# Prefix operator: (pl-OP operand) or (pl-OP-pre operand)
sub gen_prefix_op {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # First child is the operator, second is the operand
  my $op_node = $self->expr_o->get_a_node($kids->[0]);
  my $op      = $op_node->content();

  # Special case: \&func (reference to function)
  if ($op eq '\\') {
    my $operand_node = $self->expr_o->get_a_node($kids->[1]);
    if (ref($operand_node) eq 'PPI::Token::Symbol' &&
        $operand_node->content() =~ /^&(.+)$/) {
      my $func_name = $1;
      my $cl_func = $self->cl_name($func_name);
      return "#'$cl_func";
    }
  }


  # ++, --, and \ need l-value context for array/hash elements
  # \ needs l-value to get a reference to the box, not a copy of the value
  my $needs_lvalue = ($op eq '++' || $op eq '--' || $op eq '\\');
  my $saved_lvalue = $self->lvalue_context;
  $self->lvalue_context(1) if $needs_lvalue;
  my $operand = $self->gen_node($kids->[1]);
  $self->lvalue_context($saved_lvalue);

  # Get CL name for the operator
  my $cl_op = $self->cl_name($op);

  # For ++ and --, distinguish prefix from postfix
  if ($op eq '++' || $op eq '--') {
    # Special case: $#array lvalue - emit setter form
    if ($operand =~ /^\(pl-array-last-index (.+)\)$/) {
      my $arr = $1;
      my $delta_op = ($op eq '++') ? '1+' : '1-';
      return "(pl-set-array-length $arr ($delta_op (pl-array-last-index $arr)))";
    }
    $cl_op = "pl-pre" . $op;
  }
  # Sigil cast operators (dereference) - use pl-cast-X
  elsif ($op eq '@' || $op eq '%' || $op eq '$') {
    $cl_op = "pl-cast-$op";
  }

  return "($cl_op $operand)";
}


# Postfix operator: (pl-OP-post operand)
sub gen_postfix_op {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # Check if this is a chained comparison (has 5 children)
  if (scalar(@$kids) == 5) {
    return $self->gen_chained_comparison($kids);
  }

  # Get operator first to check if we need l-value context
  my $op_node = $self->expr_o->get_a_node($kids->[1]);
  my $op      = $op_node->content();

  # ++ and -- modify their operand, need l-value context for array/hash elements
  my $needs_lvalue = ($op eq '++' || $op eq '--');
  my $saved_lvalue = $self->lvalue_context;
  $self->lvalue_context(1) if $needs_lvalue;
  my $operand = $self->gen_node($kids->[0]);
  $self->lvalue_context($saved_lvalue);

  # For ++ and --, use pl-post++ / pl-post-- naming
  my $cl_op;
  if ($op eq '++' || $op eq '--') {
    # Special case: $#array lvalue - emit setter form (return old value)
    if ($operand =~ /^\(pl-array-last-index (.+)\)$/) {
      my $arr = $1;
      my $delta_op = ($op eq '++') ? '1+' : '1-';
      return "(let ((_prev (pl-array-last-index $arr))) (pl-set-array-length $arr ($delta_op _prev)) _prev)";
    }
    $cl_op = "pl-post" . $op;
  } else {
    $cl_op = $self->cl_name($op) . '-post';
  }

  return "($cl_op $operand)";
}


# Chained comparison: $x < $y < $z -> (pl-chain-cmp $x '< $y '< $z)
sub gen_chained_comparison {
  my $self = shift;
  my $kids = shift;

  # Kids: term1, op1, term2, op2, term3
  my $t1  = $self->gen_node($kids->[0]);
  my $op1 = $self->expr_o->get_a_node($kids->[1])->content();
  my $t2  = $self->gen_node($kids->[2]);
  my $op2 = $self->expr_o->get_a_node($kids->[3])->content();
  my $t3  = $self->gen_node($kids->[4]);

  return "(pl-chain-cmp $t1 '$op1 $t2 '$op2 $t3)";
}


# Array access: (pl-aref arr idx) or (pl-aref-box arr idx) in l-value context
# In Perl, $arr[0] accesses @arr, so we convert $sigil to @sigil
sub gen_array_access {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $arr = $self->gen_node($kids->[0]);
  my $idx = $self->gen_node($kids->[1]);

  # Convert $varname to @varname (Perl $arr[i] accesses @arr)
  # Handle both plain $arr and package-qualified Pkg::$arr
  $arr =~ s/(^|::)\$/$1@/;

  # Use pl-aref-box in l-value context for modifying operations
  my $func = $self->lvalue_context ? 'pl-aref-box' : 'pl-aref';
  return "($func $arr $idx)";
}


# Hash access: (pl-gethash hash key) or (pl-gethash-box hash key) in l-value context
# In Perl, $hash{key} accesses %hash, so we convert $sigil to %sigil
sub gen_hash_access {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $hash = $self->gen_node($kids->[0]);
  my $key  = $self->gen_node($kids->[1]);

  # Convert $varname to %varname (Perl $hash{k} accesses %hash)
  # Handle both plain $hash and package-qualified Pkg::$hash
  $hash =~ s/(^|::)\$/$1%/;

  # Use pl-gethash-box in l-value context for modifying operations
  my $func = $self->lvalue_context ? 'pl-gethash-box' : 'pl-gethash';
  return "($func $hash $key)";
}


# Array ref access: (pl-aref-deref ref idx)
sub gen_array_ref_access {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $ref = $self->gen_node($kids->[0]);
  my $idx = $self->gen_node($kids->[1]);

  return "(pl-aref-deref $ref $idx)";
}


# Hash ref access: (pl-gethash-deref ref key)
sub gen_hash_ref_access {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $ref = $self->gen_node($kids->[0]);
  my $key = $self->gen_node($kids->[1]);

  return "(pl-gethash-deref $ref $key)";
}


# Array slice: (pl-aslice arr idx1 idx2 ...)
sub gen_array_slice {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $arr = $self->gen_node($kids->[0]);
  my @indices;
  for my $i (1 .. $#$kids) {
    push @indices, $self->gen_node($kids->[$i]);
  }

  my $idx_str = join(' ', @indices);
  return "(pl-aslice $arr $idx_str)";
}


# Hash slice: (pl-hslice hash key1 key2 ...)
# Note: @foo{keys} accesses %foo, so we convert @ sigil to %
sub gen_hash_slice {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $hash_node = $self->expr_o->get_a_node($kids->[0]);
  my $hash = $self->gen_node($kids->[0]);
  # Convert @ to % for hash access (@ is context sigil, % is container sigil)
  if (ref($hash_node) eq 'PPI::Token::Symbol' && $hash =~ /^\@/) {
    $hash =~ s/^\@/\%/;
  }
  my @keys;
  for my $i (1 .. $#$kids) {
    push @keys, $self->gen_node($kids->[$i]);
  }

  my $key_str = join(' ', @keys);
  return "(pl-hslice $hash $key_str)";
}

# KV hash slice: %hash{keys} - returns key-value pairs
sub gen_kv_hash_slice {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $hash = $self->gen_node($kids->[0]);
  my @keys;
  for my $i (1 .. $#$kids) {
    push @keys, $self->gen_node($kids->[$i]);
  }

  my $key_str = join(' ', @keys);
  return "(pl-kv-hslice $hash $key_str)";
}


# Array initializer: (pl-array-init ...)
# Uses pl-array-init to flatten nested arrays (handles [(@x) x 2] etc.)
sub gen_array_init {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my @elements;
  for my $kid_id (@$kids) {
    push @elements, $self->gen_node($kid_id);
  }

  # Use pl-array-init which flattens nested arrays
  # Wrap in make-pl-box because [...] creates a REFERENCE to an anonymous array
  # (a scalar value), not the array itself. Without boxing, pl-setf @arr
  # would flatten the inner array instead of storing it as a reference.
  if (@elements) {
    my $elem_str = join(' ', @elements);
    return "(make-pl-box (pl-array-init $elem_str))";
  } else {
    return "(make-pl-box (make-array 0 :adjustable t :fill-pointer 0))";
  }
}


# Hash initializer: (pl-hash ...)
sub gen_hash_init {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my @pairs;
  for my $kid_id (@$kids) {
    push @pairs, $self->gen_node($kid_id);
  }

  my $pairs_str = join(' ', @pairs);
  # Wrap in make-pl-box because {...} creates a REFERENCE to an anonymous hash
  return "(make-pl-box (pl-hash $pairs_str))";
}


# Progn (comma-separated list): (progn ...)
sub gen_progn {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my @forms;
  for my $kid_id (@$kids) {
    push @forms, $self->gen_node($kid_id);
  }

  my $forms_str = join(' ', @forms);

  # In list context, generate a vector instead of progn
  # This handles: foreach (1,2,3), @a = (1,2,3), etc.
  my $ctx = $self->expr_o->get_node_context($node_id);
  if ($ctx == 1) {  # LIST_CTX = 1
    return "(vector $forms_str)";
  }

  return "(progn $forms_str)";
}


# Tree value (parenthesized expression): just generate the content
sub gen_tree_val {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $ctx = $self->expr_o->get_node_context($node_id);

  # If single child in scalar context, just return it
  # But in list context, we need (vector $x) for proper list assignment
  if (scalar(@$kids) == 1) {
    my $child = $self->gen_node($kids->[0]);
    if ($ctx == 1) {  # LIST_CTX = 1
      # Special case: regex match already returns captures in list context
      # Don't wrap in vector, just ensure *wantarray* is set
      if ($child =~ /\(pl-=~\s/) {
        return "(let ((*wantarray* t)) $child)";
      }
      return "(vector $child)";
    }
    return $child;
  }

  # Multiple values
  my @forms;
  for my $kid_id (@$kids) {
    push @forms, $self->gen_node($kid_id);
  }

  my $forms_str = join(' ', @forms);

  # In list context, generate a vector instead of progn
  # This handles: @a = (1, 2, 3), foreach (1, 2, 3), etc.
  if ($ctx == 1) {  # LIST_CTX = 1
    return "(vector $forms_str)";
  }

  return "(progn $forms_str)";
}


# Generate filehandle marker for print/say
sub gen_filehandle {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # Filehandle has one child - the actual handle name/variable
  if (@$kids) {
    my $fh = $self->gen_node($kids->[0]);
    # Quote bareword filehandles (FH, STDOUT, etc.) so CL doesn't try to evaluate them
    # Barewords are uppercase words without sigils
    if ($fh =~ /^[A-Z][A-Z0-9_]*$/) {
      return ":fh '$fh";
    }
    return ":fh $fh";
  }
  return ":fh nil";
}


# Generate readline operator <FH> or <$fh>
sub gen_readline {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # Readline may have a filehandle child, or none (for <>)
  if (@$kids) {
    my $fh = $self->gen_node($kids->[0]);
    # Quote bareword filehandles
    if ($fh =~ /^[A-Z][A-Z0-9_]*$/) {
      return "(pl-readline '$fh)";
    }
    return "(pl-readline $fh)";
  }
  # Empty <> reads from ARGV or STDIN
  return "(pl-readline)";
}


# Generate file glob <*.txt> or <$pattern>
# Handles negated character classes [!x] by generating grep filter,
# since SBCL's pathname wildcards don't support negation properly.
sub gen_glob {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # Glob has a pattern child (string or interpolated)
  my $pattern_str;
  my $call;

  if (@$kids == 1) {
    $pattern_str = $self->gen_node($kids->[0]);
    $call = "(pl-glob $pattern_str)";
  } elsif (@$kids > 1) {
    # Interpolated pattern - concatenate parts
    my @parts = map { $self->gen_node($_) } @$kids;
    my $concat = "(pl-. " . join(' ', @parts) . ")";
    $pattern_str = $concat;
    $call = "(pl-glob $concat)";
  } else {
    $pattern_str = '"*"';
    $call = "(pl-glob)";
  }

  # Check for negated character class [!...] or [^...] in literal patterns
  # For these, generate a glob + filter since SBCL doesn't handle negation
  my $needs_filter = 0;
  my $negated_chars = '';
  my $modified_pattern = $pattern_str;

  if ($pattern_str =~ /^"([^"]*)"$/) {
    my $pat = $1;
    # Look for [!chars] or [^chars] - negated character class
    if ($pat =~ /\[([!\^])([^\]]+)\]/) {
      $needs_filter = 1;
      my $neg_marker = $1;
      $negated_chars = $2;
      # Replace negated class with ? wildcard for the glob
      my $simple_pat = $pat;
      $simple_pat =~ s/\[[!\^][^\]]+\]/?/g;
      $modified_pattern = qq{"$simple_pat"};
      $call = "(pl-glob $modified_pattern)";
    }
  }

  # Wrap in dynamic wantarray binding for list context
  my $ctx = $self->expr_o->get_node_context($node_id);
  my $is_list_ctx = ($ctx == 1);  # LIST_CTX = 1

  if ($needs_filter) {
    # Generate: (remove-if (lambda (f) (find (char basename 0) "negated")) (pl-glob pattern))
    # More Perl-like: filter files where the matched char is NOT in the negated set
    # Extract just the filename part for matching
    my $filter = qq{(remove-if (lambda (--f--) }
               . qq{(let ((--name-- (file-namestring (pathname --f--)))) }
               . qq{(and (> (length --name--) 0) }
               . qq{(find (char --name-- 0) "$negated_chars")))) }
               . qq{$call)};
    if ($is_list_ctx) {
      return "(let ((*wantarray* t)) $filter)";
    }
    return $filter;
  }

  if ($is_list_ctx) {
    return "(let ((*wantarray* t)) $call)";
  }
  return $call;
}


# Generate backtick command execution `command`
sub gen_backtick {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # Backtick has a string child containing the command
  my $cmd = $self->gen_node($kids->[0]);
  return "(pl-backtick $cmd)";
}


# Generate anonymous sub (for grep/map blocks)
# Output: (lambda () body)
sub gen_anon_sub {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # Generate body from children
  my @body_parts;
  for my $kid_id (@$kids) {
    push @body_parts, $self->gen_node($kid_id);
  }
  my $body = join(' ', @body_parts);

  return "(lambda () $body)";
}


# Generate function reference for block callbacks
# Output: #'func-name
sub gen_func_ref {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $func_name = $node->{func_name};
  return "#'$func_name";
}

# Generate inline lambda for grep/map/sort blocks
# Output: (lambda (params) body)
sub gen_inline_lambda {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $params = join(' ', @{$node->{params} // []});
  my $body = $node->{body_cl} // 'nil';

  return "(lambda ($params)\n$body)";
}


# Generate substitution s///
# Output: (pl-subst "pattern" "replacement" :g :i ...)
sub gen_substitution {
  my $self = shift;
  my $node = shift;

  my $match = $node->get_match_string;
  my $subst = $node->get_substitute_string;
  my $mods  = $node->get_modifiers;

  # Escape backslashes and quotes for CL string literal
  $match =~ s/\\/\\\\/g;
  $match =~ s/"/\\"/g;
  $subst =~ s/\\/\\\\/g;
  $subst =~ s/"/\\"/g;

  my @mod_strs;
  for my $mod (sort keys %$mods) {
    push @mod_strs, ":$mod";
  }

  my $mods_str = @mod_strs ? ' ' . join(' ', @mod_strs) : '';
  return qq{(pl-subst "$match" "$subst"$mods_str)};
}


# Generate transliteration tr/// or y///
# Output: (pl-tr "from" "to" :c :d :s ...)
sub gen_transliteration {
  my $self = shift;
  my $node = shift;

  my $from = $node->get_match_string;
  my $to   = $node->get_substitute_string;
  my $mods = $node->get_modifiers;

  # Escape quotes
  $from =~ s/"/\\"/g;
  $to   =~ s/"/\\"/g;

  my @mod_strs;
  for my $mod (sort keys %$mods) {
    push @mod_strs, ":$mod";
  }

  my $mods_str = @mod_strs ? ' ' . join(' ', @mod_strs) : '';
  return qq{(pl-tr "$from" "$to"$mods_str)};
}


# Helper: \x{HHHH} - parse hex with spaces/underscores, convert to chr
# Rules: spaces stripped, leading underscores stripped,
# single underscores between digits ok, double underscores stop parsing
sub _hex_brace_escape {
  my $inner = shift;
  $inner =~ s/\s//g;   # strip spaces
  $inner =~ s/^_+//;   # strip leading underscores
  return chr(0) if $inner eq '';  # \x{} = chr(0)
  # Parse: hex digits with optional single underscores between them
  my $hex = '';
  while ($inner =~ /\G([0-9A-Fa-f]+)(_(?=[0-9A-Fa-f]))?/gc) {
    $hex .= $1;
  }
  return chr(0) if $hex eq '';
  return chr(hex($hex));
}

# Helper: \o{OOO} - parse octal with spaces/underscores, convert to chr
sub _octal_brace_escape {
  my $inner = shift;
  $inner =~ s/\s//g;  # strip spaces
  return chr(0) if $inner eq '';
  # Parse: octal digits with optional single underscores between them
  my $oct = '';
  while ($inner =~ /\G([0-7]+)(_(?=[0-7]))?/gc) {
    $oct .= $1;
  }
  return chr(0) if $oct eq '';
  return chr(oct($oct));
}

# Single-pass escape sequence processor for double-quoted strings
# Handles all \X sequences including unknown ones (\. → .)
sub _process_dq_escape {
  my $esc = shift;
  return "\n" if $esc eq 'n';
  return "\t" if $esc eq 't';
  return "\r" if $esc eq 'r';
  return "\a" if $esc eq 'a';
  return "\e" if $esc eq 'e';
  return "\f" if $esc eq 'f';
  return "\$" if $esc eq '$';
  return '@'  if $esc eq '@';
  return '"'  if $esc eq '"';
  return "\\" if $esc eq '\\';
  # \cX - control character
  if ($esc =~ /^c(.)$/) {
    return chr(ord(uc($1)) ^ 64);
  }
  # \x{HHHH} - hex with braces
  if ($esc =~ /^x\{([^}]*)\}$/) {
    return _hex_brace_escape($1);
  }
  # \xHH - hex 1-2 digits
  if ($esc =~ /^x([0-9A-Fa-f]{1,2})$/) {
    return chr(hex($1));
  }
  # \x alone - chr(0)
  return chr(0) if $esc eq 'x';
  # \o{OOO} - octal with braces
  if ($esc =~ /^o\{([^}]*)\}$/) {
    return _octal_brace_escape($1);
  }
  # \NNN - octal digits
  if ($esc =~ /^([0-7]{1,3})$/) {
    return chr(oct($1));
  }
  # Case-changing escapes: preserve as markers for _apply_case_escapes
  if ($esc =~ /^[ULulQFE]$/) {
    return "\\$esc";  # keep as \U, \L, etc. for later processing
  }
  # Unknown escape: \X → X (Perl drops the backslash)
  return $esc;
}

# Apply \U, \L, \u, \l, \Q, \F ... \E case transformations to a string
# These are processed after escape sequences, on the final text
sub _apply_case_escapes {
  my $str = shift;
  # Quick check: if no case escapes, return unchanged
  return $str unless $str =~ /\\/;

  my $result = '';
  my @modes;  # stack of active modes

  while ($str =~ /\G(.*?)\\([ULulQFE])/gc) {
    my ($text, $cmd) = ($1, $2);
    # Apply current mode to the text before this escape
    $result .= _apply_mode(\@modes, $text);

    if ($cmd eq 'E') {
      pop @modes if @modes;
    } elsif ($cmd eq 'u' || $cmd eq 'l') {
      # Single-char transforms: apply to next char only
      # Grab one char after the escape
      if ($str =~ /\G(.)/gc) {
        my $ch = $1;
        $ch = $cmd eq 'u' ? uc($ch) : lc($ch);
        $result .= _apply_mode(\@modes, $ch);
      }
    } else {
      # \U, \L, \Q, \F — push mode, affects until \E
      push @modes, $cmd;
    }
  }
  # Remaining text after last escape
  my $rest = substr($str, pos($str) // 0);
  $result .= _apply_mode(\@modes, $rest);

  return $result;
}

# Apply the current mode stack to a piece of text
sub _apply_mode {
  my ($modes, $text) = @_;
  return $text unless @$modes && length($text);
  for my $mode (@$modes) {
    if ($mode eq 'U') { $text = uc($text); }
    elsif ($mode eq 'L') { $text = lc($text); }
    elsif ($mode eq 'F') { $text = lc($text); }  # fc ≈ lc for ASCII
    elsif ($mode eq 'Q') { $text = quotemeta($text); }
  }
  return $text;
}

# Convert Perl string with escapes to CL string
# Perl "\n" -> actual newline in CL string
sub convert_perl_string {
  my $self = shift;
  my $str = shift;

  # Determine quote type and extract content
  my $quote_char;
  my $content;

  if ($str =~ /^'(.*)'$/s) {
    # Single-quoted: no escape processing except \\ and \'
    $content = $1;
    $content =~ s/\\'/'/g;
    $content =~ s/\\\\/\\/g;
    # For CL, escape backslashes and quotes
    $content =~ s/\\/\\\\/g;
    $content =~ s/"/\\"/g;
    return qq{"$content"};
  }
  elsif ($str =~ /^"(.*)"$/s) {
    # Double-quoted: process Perl escapes
    $content = $1;
  }
  elsif ($str =~ /^qq\{(.*)\}$/s || $str =~ /^qq\((.*)\)$/s ||
         $str =~ /^qq\[(.*)\]$/s) {
    # qq{}, qq(), qq[] style
    $content = $1;
  }
  elsif ($str =~ /^qq(.)(.*)\1$/s) {
    # qq/.../  style - content is in $2
    $content = $2;
  }
  elsif ($str =~ /^q\{(.*)\}$/s || $str =~ /^q\((.*)\)$/s ||
         $str =~ /^q\[(.*)\]$/s) {
    # q{}, q(), q[] style - like single-quoted, no interpolation
    $content = $1;
    $content =~ s/\\\\/\\/g;    # only \\ is special in q{}
    # For CL, escape backslashes and quotes
    $content =~ s/\\/\\\\/g;
    $content =~ s/"/\\"/g;
    return qq{"$content"};
  }
  elsif ($str =~ /^q(.)(.*)\1$/s) {
    # q/.../ style - content is in $2
    $content = $2;
    $content =~ s/\\\\/\\/g;
    $content =~ s/\\/\\\\/g;
    $content =~ s/"/\\"/g;
    return qq{"$content"};
  }
  else {
    # Unknown format, return as-is
    return $str;
  }

  # Process Perl escape sequences in single pass to handle \\ correctly
  $content =~ s!\\(x\{[^}]*\}|x[0-9A-Fa-f]{1,2}|x|o\{[^}]*\}|[0-7]{1,3}|c.|[ntreafd"\\\$\@]|.)!
    _process_dq_escape($1)
  !ge;

  # Apply \U, \L, \u, \l, \Q, \F ... \E transformations (non-interpolated strings)
  $content = _apply_case_escapes($content);

  # Now escape for CL output: backslashes and quotes
  $content =~ s/\\/\\\\/g;
  $content =~ s/"/\\"/g;

  return qq{"$content"};
}


1;

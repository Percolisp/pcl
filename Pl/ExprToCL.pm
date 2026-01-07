package Pl::ExprToCL;

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
    'arr_init'      => \&gen_array_init,
    'hash_init'     => \&gen_hash_init,
    'progn'         => \&gen_progn,
    'tree_val'      => \&gen_tree_val,
    'filehandle'    => \&gen_filehandle,
    'readline'      => \&gen_readline,
    'backtick'      => \&gen_backtick,
    'anon_sub'      => \&gen_anon_sub,
    'func_ref'      => \&gen_func_ref,
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
  # (these are valid Perl identifiers, so users could define sub x, sub eq, etc.)
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


# Generate CL operator/function name from Perl name
# - Package-qualified names (Foo::bar) → Foo:bar (CL package syntax)
# - Operator exceptions → from %OP_EXCEPTIONS
# - Built-in functions → pl-<name>
# - User-defined functions → <name> (in current CL package)
sub cl_name {
  my $self      = shift;
  my $perl_name = shift;

  # Check for operator exceptions first
  return $OP_EXCEPTIONS{$perl_name} if exists $OP_EXCEPTIONS{$perl_name};

  # Check for package-qualified name (Foo::bar)
  if ($perl_name =~ /^(.+)::(.+)$/) {
    my ($pkg, $func) = ($1, $2);
    # Use CL's package::symbol syntax with pl- prefix
    return "${pkg}::pl-${func}";
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
    return $self->gen_binary_op($op, $kids);
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
  return $self->gen_binary_op($type, $kids);
}


# Generate code for leaf nodes (literals, variables)
sub gen_leaf {
  my $self = shift;
  my $node = shift;

  my $ref  = ref($node);

  # Variable ($x, @arr, %hash)
  if ($ref eq 'PPI::Token::Symbol' || $ref eq 'PPI::Token::Magic') {
    return $node->content();
  }

  # Array last index ($#arr)
  if ($ref eq 'PPI::Token::ArrayIndex') {
    my $content = $node->content();
    # $#arr -> (pl-array-last-index @arr)
    $content =~ s/^\$#/@/;
    return "(pl-array-last-index $content)";
  }

  # Number literal
  if ($ref eq 'PPI::Token::Number') {
    return $node->content();
  }

  # String literals
  if ($ref =~ /^PPI::Token::Quote/) {
    my $content = $node->content();
    # Convert Perl escape sequences to actual characters for CL
    return $self->convert_perl_string($content);
  }

  # Bareword (function name, etc.)
  if ($ref eq 'PPI::Token::Word') {
    return $node->content();
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
    # Escape backslashes for CL string literal
    $content =~ s/\\/\\\\/g;
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
  my $self = shift;
  my $op   = shift;
  my $kids = shift;

  my $cl_op = $self->cl_op_name($op);
  my $left  = $self->gen_node($kids->[0]);
  my $right = $self->gen_node($kids->[1]);

  return "($cl_op $left $right)";
}

# Generate CL name for an OPERATOR (always qualified with pcl:)
sub cl_op_name {
  my $self = shift;
  my $op   = shift;

  # Check for operator exceptions first - add pcl: prefix
  if (exists $OP_EXCEPTIONS{$op}) {
    return "pcl:" . $OP_EXCEPTIONS{$op};
  }

  # Operators always use pcl: prefix
  return "pcl:pl-$op";
}


# String concatenation with multiple parts
sub gen_string_concat {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my @parts = map { $self->gen_node($_) } @$kids;
  return "(pl-string_concat " . join(" ", @parts) . ")";
}


# Function call: (pl-FUNC args...)
sub gen_funcall {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  # First child is function name
  my $func_name = $self->gen_node($kids->[0]);
  my $cl_func   = $self->cl_name($func_name);

  # Special handling for next/last with label argument
  if (($func_name eq 'next' || $func_name eq 'last') && @$kids == 2) {
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

  # Rest are arguments
  my @args;
  for my $i (1 .. $#$kids) {
    push @args, $self->gen_node($kids->[$i]);
  }

  my $args_str = @args ? ' ' . join(' ', @args) : '';
  my $call = "($cl_func$args_str)";

  # Wrap in dynamic wantarray binding for list context
  my $ctx = $self->expr_o->get_node_context($node_id);
  if ($ctx == 1) {  # LIST_CTX = 1
    return "(let ((*wantarray* t)) $call)";
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

  my $method  = $self->gen_node($kids->[1]);

  # Rest are arguments
  my @args;
  for my $i (2 .. $#$kids) {
    push @args, $self->gen_node($kids->[$i]);
  }

  my $args_str = @args ? ' ' . join(' ', @args) : '';
  my $call = "(pl-method-call $obj '$method$args_str)";

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

  my $cond  = $self->gen_node($kids->[0]);
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
  my $operand = $self->gen_node($kids->[1]);

  # Get CL name for the operator
  my $cl_op = $self->cl_name($op);

  # For ++ and --, distinguish prefix from postfix
  if ($op eq '++' || $op eq '--') {
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

  # Normal postfix: first child is operand, second is operator
  my $operand = $self->gen_node($kids->[0]);
  my $op_node = $self->expr_o->get_a_node($kids->[1]);
  my $op      = $op_node->content();

  # For ++ and --, use pl-post++ / pl-post-- naming
  my $cl_op;
  if ($op eq '++' || $op eq '--') {
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


# Array access: (pl-aref arr idx)
# In Perl, $arr[0] accesses @arr, so we convert $sigil to @sigil
sub gen_array_access {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $arr = $self->gen_node($kids->[0]);
  my $idx = $self->gen_node($kids->[1]);

  # Convert $varname to @varname (Perl $arr[i] accesses @arr)
  $arr =~ s/^\$/@/;

  return "(pl-aref $arr $idx)";
}


# Hash access: (pl-gethash hash key)
# In Perl, $hash{key} accesses %hash, so we convert $sigil to %sigil
sub gen_hash_access {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my $hash = $self->gen_node($kids->[0]);
  my $key  = $self->gen_node($kids->[1]);

  # Convert $varname to %varname (Perl $hash{k} accesses %hash)
  $hash =~ s/^\$/%/;

  return "(pl-gethash $hash $key)";
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
sub gen_hash_slice {
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
  return "(pl-hslice $hash $key_str)";
}


# Array initializer: (list ...)
sub gen_array_init {
  my $self    = shift;
  my $node    = shift;
  my $node_id = shift;
  my $kids    = shift;

  my @elements;
  for my $kid_id (@$kids) {
    push @elements, $self->gen_node($kid_id);
  }

  # Generate an adjustable vector for Perl array semantics
  # This allows push, pop, shift, unshift to work correctly
  if (@elements) {
    my $elem_str = join(' ', @elements);
    return "(make-array " . scalar(@elements) .
           " :adjustable t :fill-pointer t :initial-contents (list $elem_str))";
  } else {
    return "(make-array 0 :adjustable t :fill-pointer 0)";
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
  return "(pl-hash $pairs_str)";
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

  # If single child, just return it
  if (scalar(@$kids) == 1) {
    return $self->gen_node($kids->[0]);
  }

  # Multiple values
  my @forms;
  for my $kid_id (@$kids) {
    push @forms, $self->gen_node($kid_id);
  }

  my $forms_str = join(' ', @forms);

  # In list context, generate a vector instead of progn
  # This handles: @a = (1, 2, 3), foreach (1, 2, 3), etc.
  my $ctx = $self->expr_o->get_node_context($node_id);
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
         $str =~ /^qq\[(.*)\]$/s || $str =~ /^qq(.)(.*)\1$/s) {
    # qq{} style
    $content = $1 // $2;
  }
  else {
    # Unknown format, return as-is
    return $str;
  }

  # Process Perl escape sequences
  $content =~ s/\\n/\n/g;      # newline
  $content =~ s/\\t/\t/g;      # tab
  $content =~ s/\\r/\r/g;      # carriage return
  $content =~ s/\\0/\0/g;      # null
  $content =~ s/\\a/\a/g;      # bell
  $content =~ s/\\e/\e/g;      # escape
  $content =~ s/\\f/\f/g;      # form feed
  $content =~ s/\\"/"/g;       # escaped quote
  $content =~ s/\\\$/\$/g;     # escaped dollar
  $content =~ s/\\\@/\@/g;     # escaped at
  $content =~ s/\\\\/\\/g;     # escaped backslash (must be last)

  # Now escape for CL output: backslashes and quotes
  $content =~ s/\\/\\\\/g;
  $content =~ s/"/\\"/g;

  return qq{"$content"};
}


1;

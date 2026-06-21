package Pl::PExpr;

# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

use v5.20;
use strict;
use warnings;

use Moo;
# use strictures 2;

use Scalar::Util qw/looks_like_number/;

use PPI;
use PPI::Dumper;

use Data::Dump qw/dump/;

use Pl::OpcodeTree;
use Pl::PExpr::Config;
use Pl::PExpr::TokenUtils;
use Pl::PExpr::StringInterpolation;


# Context constants
use constant {
    SCALAR_CTX  => 0,
    LIST_CTX    => 1,
    VOID_CTX    => 2,
    INHERIT_CTX => 3,  # inherit *wantarray* from dynamic scope; emit no binding
};

# Export for use in tests/other modules
use Exporter 'import';
our @EXPORT_OK = qw(SCALAR_CTX LIST_CTX VOID_CTX INHERIT_CTX);

# XXXX Unary ops have a different prio compared to list ops, se page 106.

# XXXX From 5.36 can do sub foo :lvalue ($x, $y = 1, @z) { .... }
#      https://perldoc.perl.org/perlsub#Signatures
#      Can PPI handle that?
#      Sub specs aren't relevant in expr parsing, of course. But do
#      need to know about L-value subs:
#      https://perldoc.perl.org/perlsub#Lvalue-subroutines
#      foo_lvalue_sub($bar) = some_expr(42);
#      Need to send them in to sub declares? Is it illegal syntax for
#      non L-value subs to do that?


# use constant DEBUG => 8;
my $DEBUG_VAL  = 0; # Stupid to not have a constant for DEBUG during dev
sub DEBUG { $DEBUG_VAL; }
sub SET_DEBUG { $DEBUG_VAL = shift; }



# Expression:
has e => (
  is        => 'ro',
  required  => 0,
);

# Parts of PPI get GC:ed, if there are no references to the PPI
# object!  So keep the PPI::Document alive to prevent tokens from
# becoming empty.  (This is probably only a problem when writing
# tests.)
has full_PPI => (
  is        => 'ro',
  required  => 0,
);


has node_tree => (
  is        => 'rw',
  default   => sub {return Pl::OpcodeTree->new(); },
);

has root => (
  is        => 'rw',
  default => sub { 0; },
);

# Declarations found during parsing (my, our, state, local)
# Each entry: { type => 'my'|'our'|'state'|'local', var => '$x' }
has declarations => (
  is        => 'rw',
  default   => sub { [] },
);

# String interpolation handler
has str_interpol => (
  is       => 'ro',
  default  => sub { Pl::PExpr::StringInterpolation->new() },
);


# Configuration object for operator precedence, function parameters, etc.
has config => (
  is       => 'ro',
  default  => sub { Pl::PExpr::Config->new() },
);

# Environment for declared subs, constants, etc.
# Optional - if not provided, only built-in functions are known.
has environment => (
  is        => 'ro',
  predicate => 'has_environment',
);

# Delegate configuration accessors
sub postfix { shift->config->postfix }
sub prefix { shift->config->prefix }
sub precedences { shift->config->precedences }
sub known_no_of_params { shift->config->known_no_of_params }
sub named_unary { shift->config->named_unary }

sub is_named_unary {
  my $self = shift;
  my $name = shift;
  return $self->named_unary->{$name};
}

# Token utilities for type checking
has token_utils => (
  is       => 'ro',
  default  => sub { Pl::PExpr::TokenUtils->new() },
);

# Parser reference for recursive block parsing
has parser => (
  is        => 'ro',
  required  => 0,
  predicate => 'has_parser',
);


# Delegate token utility methods
sub is_atomic { shift->token_utils->is_atomic(@_) }
sub is_regexp { shift->token_utils->is_regexp(@_) }
sub is_string { shift->token_utils->is_string(@_) }
sub is_number { shift->token_utils->is_number(@_) }
sub is_var { shift->token_utils->is_var(@_) }
sub is_arrow_op { shift->token_utils->is_arrow_op(@_) }
sub is_arr_or_hash_braces { shift->token_utils->is_arr_or_hash_braces(@_) }
sub is_arr_braces { shift->token_utils->is_arr_braces(@_) }
sub is_hash_braces { shift->token_utils->is_hash_braces(@_) }
sub is_inline_hash { shift->token_utils->is_inline_hash(@_) }
sub is_inline_arr { shift->token_utils->is_inline_arr(@_) }
sub is_token_operator { shift->token_utils->is_token_operator(@_) }
sub is_list_parentheses { shift->token_utils->is_list_parentheses(@_) }
sub is_list { shift->token_utils->is_list(@_) }
sub is_word { shift->token_utils->is_word(@_) }
sub is_internal_node_type { shift->token_utils->is_internal_node_type(@_) }
sub _is_block { shift->token_utils->_is_block(@_) }


# ----------------------------------------------------------------------
# See PPI tree:

#$ pd -E 'use PPI; use PPI::Dumper;  \
#         $doc = PPI::Document->new(\"(5 + 3) * 10) + \$q[1][3];"); \
#         $dmp = PPI::Dumper->new( $doc ); $dmp->print; $cs=$doc->children();'


# ----------------------------------------------------------------------
# External API:
sub parse_expr_to_tree {
  my $self      = shift;
  my $e         = shift // $self->e;

  # Clear declarations from any previous parse
  $self->declarations([]);

  my @exprs     = @$e;          # Copy

  # Handle declarators (my, our, state, local)
  @exprs = $self->extract_declarations(\@exprs);

  # XXXX Clear any stored temporary stuff??
  # Clear node tree here?
  my $root_id   = $self->parse(\@exprs);
  # say "--------- Root id: $root_id"   if 1 & DEBUG;
  $self->set_top_node_id($root_id);

  # Return declarations too in list context (for proper scope handling)
  # Usage: my ($tree, $decls) = $parser->parse_expr_to_tree($expr);
  return wantarray ? ($root_id, $self->declarations) : $root_id;
}


# Extract declarations (my, our, state, local) from expression.
# Returns modified expression with declarators stripped.
# Records declarations in $self->declarations for later retrieval.
#
# This implements proper Perl 5.10+ semantics where:
#   my $x = 10 if $condition;
# is equivalent to:
#   my $x;                    # Declaration always happens
#   $x = 10 if $condition;    # Assignment is conditional
sub extract_declarations {
  my $self  = shift;
  my $exprs = shift;

  my @result;

  for my $item (@$exprs) {
    # Check for PPI::Statement::Variable (wraps 'my $x = ...' etc)
    if (ref($item) eq 'PPI::Statement::Variable') {
      my @children = $item->children();
      my $decl_type;
      my @vars;
      my @rest;
      my $in_decl = 1;
      my $decl_list;  # The original Structure::List for list-form declarations

      for my $child (@children) {
        # Skip whitespace
        next if ref($child) =~ /::Whitespace$/;

        if ($in_decl && ref($child) eq 'PPI::Token::Word') {
          my $word = $child->content();
          if ($word =~ /^(my|our|state|local)$/) {
            $decl_type = $word;
            next;
          }
        }

        if ($in_decl && ref($child) eq 'PPI::Token::Symbol') {
          push @vars, $child->content();
          next;
        }

        # Handle list declarations: my ($x, $y) = ...
        if ($in_decl && ref($child) eq 'PPI::Structure::List') {
          $decl_list = $child;  # Remember the original list structure
          # Extract all Symbol tokens from inside the list
          my @list_children = $child->children();
          for my $lc (@list_children) {
            # Skip whitespace
            next if ref($lc) =~ /::Whitespace$/;
            # Handle Statement::Expression wrapper
            if (ref($lc) eq 'PPI::Statement::Expression'
                || ref($lc) eq 'PPI::Statement') {
              for my $sc ($lc->children()) {
                if (ref($sc) eq 'PPI::Token::Symbol'
                    || ref($sc) eq 'PPI::Token::Magic') {
                  push @vars, $sc->content();
                }
              }
            }
            # Direct Symbol in list (less common)
            elsif (ref($lc) eq 'PPI::Token::Symbol'
                   || ref($lc) eq 'PPI::Token::Magic') {
              push @vars, $lc->content();
            }
          }
          next;
        }

        # Once we hit an operator or anything else, we're past declarations
        $in_decl = 0;
        push @rest, $child;
      }

      # Record the declarations
      if ($decl_type && @vars) {
        for my $var (@vars) {
          push @{$self->declarations}, { type => $decl_type, var => $var };
          # For 'our' declarations, also register in the environment so that
          # ExprToCL can emit package-qualified names when needed (e.g. in
          # lambdas inside inline package blocks where in-package is not in
          # effect at read time).
          if ($decl_type eq 'our' && $self->environment) {
            my $pkg = $self->environment->current_package // 'main';
            $self->environment->add_our_variable($pkg, $var);
          }
        }
        say "extract_declarations: Found $decl_type for: ", join(", ", @vars)
            if 1 & DEBUG;
      }

      # Add the remaining expression parts (without the declarator)
      # The variable itself stays - just the 'my'/'our'/etc is stripped
      if (@vars) {
        if ($decl_list) {
          # List-form: my ($k,$v) = expr → keep Structure::List intact so
          # the binary-op parser sees ($k,$v) as a single LHS unit.
          push @result, $decl_list;
        } else {
          # Scalar-form: my $x = expr → single Symbol token
          for my $var (@vars) {
            push @result, PPI::Token::Symbol->new($var);
          }
        }
      }
      push @result, @rest;
    }
    # Check for standalone declarator word at start of expression
    elsif (ref($item) eq 'PPI::Token::Word'
           && $item->content() =~ /^(my|our|state|local)$/) {
      my $decl_type = $item->content();

      # Look ahead for the variable in the next items
      # (This handles cases where expression is a flat array of tokens)
      # Skip this token; next iteration should find the variable
      # We just record that we saw a declarator
      $self->{_pending_decl} = $decl_type;
      say "extract_declarations: Pending declarator: $decl_type"
          if 1 & DEBUG;
    }
    elsif ($self->{_pending_decl}
           && (ref($item) eq 'PPI::Token::Symbol'
               || ref($item) eq 'PPI::Token::Magic')) {
      # Found variable after declarator (Symbol or Magic like $/)
      my $var = $item->content();
      my $decl = $self->{_pending_decl};
      push @{$self->declarations}, { type => $decl, var => $var };
      say "extract_declarations: Found ", $decl, " $var"
          if 1 & DEBUG;
      # Register our vars in environment so gen_leaf can qualify them.
      if ($decl eq 'our' && $self->environment) {
        my $pkg = $self->environment->current_package // 'main';
        $self->environment->add_our_variable($pkg, $var);
      }
      delete $self->{_pending_decl};
      push @result, $item;  # Keep the variable, just stripped the declarator
    }
    # Handle list after declarator: my ($x, $y) = ...
    elsif ($self->{_pending_decl}
           && ref($item) eq 'PPI::Structure::List') {
      my $decl_type = $self->{_pending_decl};
      my @vars;
      # Extract all Symbol tokens from inside the list
      for my $lc ($item->children()) {
        next if ref($lc) =~ /::Whitespace$/;
        if (ref($lc) eq 'PPI::Statement::Expression'
            || ref($lc) eq 'PPI::Statement') {
          for my $sc ($lc->children()) {
            if (ref($sc) eq 'PPI::Token::Symbol'
                || ref($sc) eq 'PPI::Token::Magic') {
              push @vars, $sc->content();
            }
          }
        }
        elsif (ref($lc) eq 'PPI::Token::Symbol'
               || ref($lc) eq 'PPI::Token::Magic') {
          push @vars, $lc->content();
        }
      }
      # Record declarations
      for my $var (@vars) {
        push @{$self->declarations}, { type => $decl_type, var => $var };
      }
      say "extract_declarations: Found $decl_type for list: ", join(", ", @vars)
          if 1 & DEBUG;
      delete $self->{_pending_decl};
      push @result, $item;  # Keep the list structure
    }
    elsif (ref($item) =~ /::Whitespace$/) {
      # Keep whitespace, don't reset pending declarator
      push @result, $item;
    }
    else {
      delete $self->{_pending_decl} if exists $self->{_pending_decl};
      push @result, $item;
    }
  }

  return @result;
}


# ----------------------------------------------------------------------

sub parse {
  my $self      = shift;
  my $e         = shift // $self->e;

  if ($self->is_list($e)) {
    # if (ref($e) eq 'PPI::Structure::List') {
    my @list    = $self->children();
    $e          = \@list;
  }

  $e            = $self->cleanup_for_parsing($e);
  # Collapse dynamic typeglob-slot *{EXPR}{SLOT} into a single glob_slot node
  # BEFORE handle_subcalls, so a preceding named unary grabs the whole glob-slot
  # as its argument (e.g. `defined *{$g}{CODE}` in Sub::Override) instead of just
  # the Cast '*'.  Without parens, handle_subcalls would otherwise orphan the
  # trailing {EXPR}{SLOT} blocks and the parse would fall through.
  $self->_retag_braced_deref_subscript($e);
  $self->_precollapse_dyn_glob_slots($e);
  $self->handle_subcalls($e);
  say "parse: //////  After calling handle_subcalls, in param:"  if 1 & DEBUG;
  say dump($e)      if 1 & DEBUG;

  # Empty expression: () or empty list — generate an empty progn node.
  # In list context this becomes (vector), in scalar context (progn).
  if (scalar(@$e) == 0) {
    my ($node, $id) = $self->make_node_insert('progn');
    return $id;
  }

  # - - - Handle just one item:
  if (scalar(@$e) == 1) {
    my $e1      = $e->[0];

    if (ref($e1) eq "PPI::Statement::Expression"
        || ref($e1) eq "PPI::Statement"
        || ref($e1) eq "PPI::Statement::Break") {
      # Usually puts an expression object around the items in expr list.
      # Also handles Statement::Break (return/last/next)
      my $kids  = $self->remove_expression_object_around($e1);
      return $self->parse($kids);
    }

    if (ref($e1) eq "PPI::Statement::Variable") {
      # Statement::Variable wraps 'my $x = ...' - need to strip the declarator
      my $kids  = $self->remove_expression_object_around($e1);
      # Extract declarations (strips 'my'/'our'/etc, keeps variable)
      my @stripped = $self->extract_declarations($kids);
      return $self->parse(\@stripped);
    }

    # A parenthesised expression.  PPI normally labels `(...)` as Structure::List,
    # but in a postfix conditional whose condition STARTS with a parenthesised
    # group followed by an operator — `return X if (A) || (B)` — PPI mislabels the
    # leading `(A)` as a Structure::Condition (the bracket type it uses for
    # `if (...)`).  Both are just a parenthesised expression here, so treat them
    # identically: parse the inner children.  (Found in Math::BigInt via the CPAN
    # test-suite survey.)
    if (ref($e1) eq 'PPI::Structure::List'
        || ref($e1) eq 'PPI::Structure::Condition') {
      my @list    = $e1->children();
      $e          = \@list;
      return $self->parse($e);
    }

    # Handle Block structures (used in braced derefs like ${$ref}, @{$expr})
    if (ref($e1) eq 'PPI::Structure::Block') {
      # Perl: a LONE bareword in a deref block is autoquoted — ${foo}/@{foo}/
      # %{foo} mean the symbolic refs ${"foo"}/@{"foo"}/%{"foo"} (the package
      # variable named foo), NEVER a sub call (you must write {foo()} to call).
      # PPI would otherwise parse the bareword as a function call (pl-foo), which
      # is undefined at runtime.  Replace it with a string literal so the cast
      # (p-cast-@ / p-cast-$ / p-cast-%) does the symbolic deref.
      if (defined(my $bw = _block_sole_bareword($e1))) {
        my $str = PPI::Token::Quote::Single->new("'$bw'");
        return $self->make_node($str);
      }
      # If the block looks like a hash constructor ({ key => val }), treat as hash_init
      if (_block_is_hash_constructor($e1)) {
        my @list    = $e1->children();
        if (@list == 1 && ref($list[0]) eq 'PPI::Statement') {
          @list = $list[0]->children();
        }
        my $e_list  = $self->cleanup_for_parsing(\@list);
        $e_list     = $self->remove_expression_object_around($e_list);
        my $x = $self->parse_list($e_list);
        my($top_node, $top_id) = $self->make_node_insert('hash_init');
        for (@$x) { $self->add_child_to_node($top_id, $_) }
        return $top_id;
      }
      my @list    = $e1->children();
      $e          = \@list;
      return $self->parse($e);
    }

    say "parse: ///// Just 1! Ref: ", ref($e1)     if 1 & DEBUG;

    # - - - Trivial cases:
    if ($self->is_atomic($e1)) {
      # Check if it's a string that needs interpolation
      my $str_type = $self->is_string($e1);
      if ($str_type && $str_type == 2) {
        # String needs interpolation
        say "parse(): String needs interpolation"      if 1 & DEBUG;
        return $self->str_interpol->parse_interpolated_string($self, $e1);
      }
      
      # Simple atomic value
      my $id    = $self->make_node($e1);
      say "parse(): Made node $id of atomic."      if 1 & DEBUG;
      return $id;
    }

    # - - - We have already made a tree out of all the Expr!
    return $self->id_of_internal_node($e1)
        if $self->is_internal_node_type($e1);

    # - - - Regular expression?
    if ($self->is_regexp($e1)) {
      my $id    = $self->make_node($e1);
      
      my $node  = $self->get_a_node($id);
      # Does regex have match context (from =~ or !~)
      if ($node->{_has_match_context}) {
        say "parse(): Regexp has match context, no $_ wrapping"  if 1 & DEBUG;
        return $id;
      }

      # Standalone regex. Wrap with '$_ =~'
      say "parse(): Found standalone regexp, add '\$_ =~'"       if 1 & DEBUG;
      # Create =~ operator node
      my ($match_node, $match_id) = $self->make_node_insert('=~');

      # Create $_ as left operand
      my $underscore = PPI::Token::Symbol->new('$_');
      my $underscore_id = $self->make_node($underscore);

      # Create regex as right operand
      my $regex_id = $self->make_node($e1);

      # Build tree: =~($_, /pattern/)
      $self->add_child_to_node($match_id, $underscore_id);
      $self->add_child_to_node($match_id, $regex_id);
      
      say "parse(): Made $match_id of \$_ =~ regexp." if 1 & DEBUG;
      return $match_id;
    }

    # - - - Hash constant?
    if ($self->is_inline_hash($e1) || $self->is_inline_arr($e1)) {
      my @list    = $e1->children();
      # Seems to be different for hash/arr constants??
      if (scalar(@list) == 1 && ref($list[0]) eq 'PPI::Statement')  {
        # XXXX Is this PPI change???
        @list     = $list[0]->children();
      }
      my $e_list  = $self->cleanup_for_parsing(\@list);
      $e_list     = $self->remove_expression_object_around($e_list);
      my $x = $self->parse_list($e_list);

      my $type    =  ($self->is_inline_arr($e1)) ? 'arr_init' : 'hash_init';
      my($top_node, $top_id) = $self->make_node_insert($type);
      for(@$x) {
        $self->add_child_to_node($top_id, $_);
      }
      return $top_id;

    }

    # - - - Quote-like words (qw)?
    if (ref($e1) eq 'PPI::Token::QuoteLike::Words') {
      say "parse(): Found qw() - converting to list"         if 1 & DEBUG;

      # qw(a b c) becomes a list of string literals
      my @words = $e1->literal();  # PPI extracts the words

      # Create a progn node with word children
      my ($progn_node, $progn_id) = $self->make_node_insert('progn');

      for my $word (@words) {
        # Create a string token for each word
        my $str_token = PPI::Token::Quote::Single->new("'$word'");
        my $word_id = $self->make_node($str_token);
        $self->add_child_to_node($progn_id, $word_id);
      }

      say "parse(): Made qw() progn node $progn_id with ",
           scalar(@words), " words"                           if 1 & DEBUG;
      return $progn_id;
    }


    # - - - Readline operator <FH> or <$fh>, or file glob <*.txt>?
    if (ref($e1) eq 'PPI::Token::QuoteLike::Readline') {
      say "parse(): Found readline/glob operator"    if 1 & DEBUG;
      my $content = $e1->content;
      # Extract the content from <...>
      $content =~ /^<(.*)>$/;
      my $inner = $1;

      # Distinguish between readline and file glob:
      # - Glob: contains *, ?, [, ], {, } or looks like a path with /
      # - Readline: bareword filehandle (STDIN), variable ($fh), or empty
      my $is_glob = 0;
      if (defined $inner && $inner ne '') {
        # Check for glob metacharacters or path-like content
        if ($inner =~ /[\*\?\[\]\{\}]/ ||           # glob metacharacters
            ($inner =~ /\// && $inner !~ /^\$/)) {  # path with / (not variable)
          $is_glob = 1;
        }
      }

      if ($is_glob) {
        # File glob: <*.txt>, </path/*.log>, etc.
        say "parse(): Treating as file glob"         if 1 & DEBUG;
        my ($node, $node_id) = $self->make_node_insert('glob');

        # Store the pattern - handle interpolation if contains $var
        if ($inner =~ /[\$\@]/) {
          # Contains variable - needs interpolation at runtime
          # Create a fake double-quoted string token for the interpolation parser
          my $fake_str = PPI::Token::Quote::Double->new(qq{"$inner"});
          my $interp_id = $self->str_interpol->parse_interpolated_string($self, $fake_str);
          # The interpolation returns a string_concat node, add its children
          my $interp_node = $self->get_a_node($interp_id);
          if ($self->is_internal_node_type($interp_node) && $interp_node->{type} eq 'string_concat') {
            my $interp_kids = $self->get_node_children($interp_id);
            for my $part_id (@$interp_kids) {
              $self->add_child_to_node($node_id, $part_id);
            }
          } else {
            $self->add_child_to_node($node_id, $interp_id);
          }
        } else {
          # Static pattern - store as literal string
          my $str_token = PPI::Token::Quote::Double->new(qq{"$inner"});
          my $str_id = $self->make_node($str_token);
          $self->add_child_to_node($node_id, $str_id);
        }

        say "parse(): Made glob node $node_id"       if 1 & DEBUG;
        return $node_id;
      }

      # Create a readline node with the filehandle
      my ($node, $node_id) = $self->make_node_insert('readline');

      if (defined $inner && $inner ne '') {
        # Has a filehandle - could be bareword (STDIN) or variable ($fh)
        if ($inner =~ /^\$/) {
          # Variable filehandle like $fh
          my $sym_token = PPI::Token::Symbol->new($inner);
          my $fh_id = $self->make_node($sym_token);
          $self->add_child_to_node($node_id, $fh_id);
        } else {
          # Bareword filehandle like STDIN, FH
          my $word_token = PPI::Token::Word->new($inner);
          my $fh_id = $self->make_node($word_token);
          $self->add_child_to_node($node_id, $fh_id);
        }
      }
      # If empty (<>), no children - means read from ARGV/STDIN

      say "parse(): Made readline node $node_id"     if 1 & DEBUG;
      return $node_id;
    }

    # - - - Backtick command execution `command`?
    if (ref($e1) eq 'PPI::Token::QuoteLike::Backtick') {
      say "parse(): Found backtick command"         if 1 & DEBUG;
      my $content = $e1->content;
      # Extract the command from `...`
      $content =~ /^`(.*)`$/s;
      my $cmd = $1;

      # Create a backtick node with the command as a string child
      my ($node, $node_id) = $self->make_node_insert('backtick');

      # Backticks interpolate like double-quoted strings
      my $str_token = PPI::Token::Quote::Double->new(qq{"$cmd"});
      my $cmd_id;

      # Check if interpolation is needed (has $ or @)
      if ($cmd =~ /[\$\@]/) {
        say "parse(): Backtick needs interpolation"  if 1 & DEBUG;
        $cmd_id = $self->str_interpol->parse_interpolated_string($self,
								 $str_token);
      } else {
        $cmd_id = $self->make_node($str_token);
      }
      $self->add_child_to_node($node_id, $cmd_id);

      say "parse(): Made backtick node $node_id"     if 1 & DEBUG;
      return $node_id;
    }

    # - - - Bareword (like filehandle FH, constant, or other bareword)?
    if (ref($e1) eq 'PPI::Token::Word') {
      my $name = $e1->content;

      # Check if this is a known zero-arg function (e.g., constant)
      if ($self->has_environment && $self->environment->has_prototype($name)) {
        my $sig = $self->environment->get_prototype($name);
        if ($sig->{min_params} == 0 && @{$sig->{params}} == 0) {
          # Zero-arg function - create funcall node
          my $func_id = $self->make_node($e1);
          my $call_id = $self->add_node({
            type     => 'funcall',
            function => $func_id,
            args     => [],
          });
          say "parse(): Made funcall node $call_id for zero-arg function $name"
              if 1 & DEBUG;
          return $call_id;
        }
      }

      # Regular bareword (filehandle, etc.)
      my $id = $self->make_node($e1);
      say "parse(): Made node $id of bareword."      if 1 & DEBUG;
      return $id;
    }

    # - - - Compiled regex qr//
    if (ref($e1) eq 'PPI::Token::QuoteLike::Regexp') {
      say "parse(): Found qr// regex"                if 1 & DEBUG;
      my $id = $self->make_node($e1);
      say "parse(): Made qr node $id"                if 1 & DEBUG;
      return $id;
    }

    # - - - Heredoc <<'EOF' or <<"EOF" or <<EOF
    if (ref($e1) eq 'PPI::Token::HereDoc') {
      say "parse(): Found heredoc"                   if 1 & DEBUG;
      # For interpolated heredocs (<<"..." or <<BARE but not <<'...'),
      # route through the string interpolation system so $var/@arr are expanded.
      my $marker = $e1->content;   # e.g. <<'' or <<"" or <<EOF
      if ($marker !~ /^<<'/) {
        my $inner = join('', $e1->heredoc());
        # Only use interpolation path if there's actually something to interpolate
        (my $tmp = $inner) =~ s/\\\\/\x00\x00/g;
        if ($tmp =~ /(?<!\\)[\$\@]/) {
          my $fake_str = PPI::Token::Quote::Double->new(qq{"$inner"});
          return $self->str_interpol->parse_interpolated_string($self, $fake_str);
        }
      }
      my $id = $self->make_node($e1);
      say "parse(): Made heredoc node $id"           if 1 & DEBUG;
      return $id;
    }

    # - - - What else can it be?? :-)
    warn "Handle single node of unknown type: ref='" . ref($e1) . "'\n";
    die "Handle single node of unknown type. Dump:\n" . dump($e1);
  }


  # - - - Are there any ","? Make it a (progn ... ) case.
  # XXXXX Bad? Not compatible with what () code do!!
  if (grep { my $tmp = $self->is_token_operator($_) // '';
             $tmp eq ',' ? 1 : undef;
           } @$e) {
    my $parts   = $self->parse_list($e);
    my($fakenode, $node_id) = $self->make_node_insert('progn');

    for my $c_id (@$parts) {
      $self->add_child_to_node($node_id, $c_id);
    }

    return $node_id;
  }


  # - - - Find any "()" sets and create a node:
  # (Subcalls has been done at top of sub.)
  for(my $i=0; $i < scalar(@$e); $i++) {
    my $e_l     = $e->[$i];
    next
        if !$self->is_list($e_l);
    say "parse: Replaces ()."           if 2 & DEBUG;
    $e_l        = $self->remove_expression_object_around($e_l);
    my @list    = $e_l->children();

    my $parts   = $self->parse_list(\@list);
    # The tree_val is if there are multiple values, so value is just
    # the last.
    my($pars_node, $node_id) = $self->make_node_insert('tree_val');

    for my $c_id (@$parts) {
      $self->add_child_to_node($node_id, $c_id);
    }
    $e->[$i]    = $pars_node;
  }


  # - - - Handle array/hash indexes and method calls:
  # There are 4 types of arrows:
  # Case 0:  []->[]      (same for hashes, It is a noop.)
  # Case 1:  X->foo()    (method call)
  # Case 1B: X->$foo()   (method call, named method.)
  # Case 2:  X->(...)    (^ to fun)
  # Case 3:  X->[], X->{}

  # Handle Case 0 of "->", just remove syntax sugar:
  for(my $i=0; $i < scalar(@$e); $i++) {
    my $term    = $e->[$i];
    next
        if !$self->is_arrow_op($term);

    # Case 0: A noop, just syntactic sugar?
    if ($i > 0
        && $self->is_arr_or_hash_braces($e->[$i-1])
        && $self->is_arr_or_hash_braces($e->[$i+1])) {
      splice @$e, $i, 1;
      # $i--; Not needed, we know the skipped one is [] or {}.
    }
  }

  for(my $i=0; $i < scalar(@$e); $i++) {
    my $term    = $e->[$i];
    # Check if term is something we need to process in this loop:
    # - arrow operator (->)
    # - array/hash subscript ([] or {})
    # - Constructor [ ] following an internal node (subscript after method call)
    my $is_constructor_subscript = ref($term) eq 'PPI::Structure::Constructor'
        && $term->start() eq '['
        && $i > 0
        && $self->is_internal_node_type($e->[$i-1]);
    # KV slice: %hash{keys} - PPI parses this as Symbol '%h' + Block '{keys}'
    my $is_kv_slice_block = ref($term) eq 'PPI::Structure::Block'
        && $term->start() eq '{'
        && $i > 0
        && !$self->is_internal_node_type($e->[$i-1])
        && $self->is_var($e->[$i-1])
        && $e->[$i-1]->content() =~ /^%/;
    # KV array slice: %arr[indices] - PPI parses as Symbol '%arr' + Constructor '[indices]'
    my $is_kv_arr_constructor = ref($term) eq 'PPI::Structure::Constructor'
        && $term->start() eq '['
        && $i > 0
        && !$self->is_internal_node_type($e->[$i-1])
        && $self->is_var($e->[$i-1])
        && $e->[$i-1]->content() =~ /^%/;
    # KV array slice via block-deref: %{$ref}[indices] - Cast('%') + Block('{ref}') + Constructor '[indices]'
    my $is_kv_arr_deref_constructor = ref($term) eq 'PPI::Structure::Constructor'
        && $term->start() eq '['
        && $i >= 2
        && ref($e->[$i-1]) eq 'PPI::Structure::Block'
        && $e->[$i-1]->start() eq '{'
        && ref($e->[$i-2]) eq 'PPI::Token::Cast'
        && $e->[$i-2]->content() eq '%';
    # KV hash slice via block-deref: %{$ref}{"keys"} - Cast('%') + Block('{ref}') + Block('{"keys"}')
    # PPI gives two Blocks (not Subscript) when sigil is %
    my $is_kv_hash_deref_block = ref($term) eq 'PPI::Structure::Block'
        && $term->start() eq '{'
        && $i >= 2
        && ref($e->[$i-1]) eq 'PPI::Structure::Block'
        && $e->[$i-1]->start() eq '{'
        && ref($e->[$i-2]) eq 'PPI::Token::Cast'
        && $e->[$i-2]->content() eq '%';
    # qw[...][idx] — subscript on a qw word list literal
    my $is_qw_subscript = ref($term) eq 'PPI::Structure::Constructor'
        && $term->start() eq '['
        && $i > 0
        && ref($e->[$i-1]) eq 'PPI::Token::QuoteLike::Words';
    # Typeglob slot access: *name{SLOT} — PPI gives Symbol '*name' + Block '{SLOT}'
    my $is_typeglob_slot = ref($term) eq 'PPI::Structure::Block'
        && $term->start() eq '{'
        && $i > 0
        && !$self->is_internal_node_type($e->[$i-1])
        && $self->is_var($e->[$i-1])
        && $e->[$i-1]->content() =~ /^\*/;
    # Dynamic typeglob slot access: *{EXPR}{SLOT} — Cast('*') + Block('{EXPR}') +
    # Block('{SLOT}').  e.g. *{$glob}{CODE}, used by Moo's _install_coderef.
    # SLOT must be a known glob-slot bareword so we don't misread *{$x}{$y}.
    my $is_dyn_typeglob_slot = ref($term) eq 'PPI::Structure::Block'
        && $term->start() eq '{'
        && $i >= 2
        && ref($e->[$i-1]) eq 'PPI::Structure::Block'
        && $e->[$i-1]->start() eq '{'
        && ref($e->[$i-2]) eq 'PPI::Token::Cast'
        && $e->[$i-2]->content() eq '*'
        && $self->_block_is_glob_slot($term);
    next
        if !$self->is_arrow_op($term)
        && !$self->is_arr_or_hash_braces($term)
        && !$is_constructor_subscript
        && !$is_kv_slice_block
        && !$is_kv_arr_constructor
        && !$is_kv_arr_deref_constructor
        && !$is_kv_hash_deref_block
        && !$is_qw_subscript
        && !$is_typeglob_slot
        && !$is_dyn_typeglob_slot;

    die "WTF? :-) Expr starts with ->/brace??\n" . dump($e) . "\n"
        if $i == 0;

    my $pre     = $e->[$i-1];
    my $nxt     = $e->[$i+1];
    my $nxt_2   = $e->[$i+2];   # (So 'undef' if after the last.(
    my $nxt_is_brace;
    $nxt_is_brace++
        if $nxt && $self->is_arr_or_hash_braces($nxt);

    my $is_reference;
    if ($self->is_arrow_op($term)) {
      if ($nxt_is_brace) {
        # Handle Case 3 (X->[]) like X[], with a special flag:
        # Just remove the '->' part and fall through, handled with X[].
        splice @$e, $i, 1;
        $term   = $nxt;
        $nxt    = $e->[$i+1];
        $is_reference++;
      } elsif ($self->is_internal_node_type($nxt)
               && $nxt->{type} eq 'tree_val') {
        # Case 2: X->(..) Apply a fun call.
        # Need to make $pre a funref call to the parameters in the
        # parentheses.

        # That can look like: '$q[3][4]->(5, 6)', we need to handle []s/{}s
        # first. Tree should be like:
        #            ref_funcall
        #         a_acc     5     6
        #     a_acc   4
        #    $q   3

        # (This '($q...)->(5)' should work the same.)
        #
        # A leading scalar deref binds WITH the ref target: $$r->() means
        # (${$r})->() — deref $r to the coderef, THEN call.  PPI hands us a flat
        # Cast('$') + Symbol/Block before the '->', and without consuming the
        # cast here it would wrap the whole funcall instead — ${ $r->() }.  So
        # if $e->[$i-2] is a scalar Cast, parse it together with $pre as the ref.
        my @pre_toks      = ($pre);
        my $cast_consumed = 0;
        if ($i >= 2
            && ref($e->[$i-2]) eq 'PPI::Token::Cast'
            && $e->[$i-2]->content eq '$') {
          unshift @pre_toks, $e->[$i-2];
          $cast_consumed = 1;
        }
        my $pre_id = $self->parse(\@pre_toks);
        my $pst_id = $nxt->{id};
        my $kids   = $self->get_node_children($pst_id);

        my($node, $id) = $self->make_node_insert('ref_funcall');
        $self->add_child_to_node($id, $pre_id);   # Fun ref
        for my $kid_id (@$kids) {
          $self->add_child_to_node($id, $kid_id); # Parameters
        }

        if ($cast_consumed) {
          $e->[$i-2] = $node;
          splice @$e, $i-1, 3;   # remove the ref term, '->', and the param list
          $i -= 2;
        } else {
          $e->[$i-1] = $node;
          splice @$e, $i, 2;     # remove '->' and the param list
          $i--;
        }
        next;
      } elsif ($self->is_internal_node_type($nxt)
               && $nxt->{type} eq 'funcall') {
        # Should really have a check for if it is the first.

        # Case 1: X->foo(...) (method call)
        # The 'f00->(...)' part has been compiled by handle_subcalls()
        # at the top of parse().

        # Need to change that to 'methodcall' and the first parameter
        # of that ref_funcall to whatever is to the left of this.

        # So: '$barf[2]->foobar(1,2)' becomes:
        #             methodcall
        #    a_acc    foobar     1  2
        #  $barf  2

        $nxt->{type}= 'methodcall';

        my $pre_id  = $self->parse([$pre]);
        my $pst_id  = $nxt->{id};
        $self->prepend_child_to_node($pst_id, $pre_id);
        splice @$e, $i-1, 2;
        $i--;  # Adjust for removed elements so recheck for following subscript
        next;
      } elsif (!$self->is_internal_node_type($nxt)
               && $nxt->content() =~ /^\$/
               && $nxt_2 && $self->is_internal_node_type($nxt_2)
               && $nxt_2->{type} eq 'tree_val') {
        # Case 1B: X->$foo(...)
        my $pre_id = $self->parse([$pre]);
        my $meth_id= $self->parse([$nxt]); # Variable name with method
        my $pars_id= $nxt_2->{id};
        my $params = $self->get_node_children($pars_id);

        # Create a node 'methodcall', add X, variable ref and params
        my($node, $id) = $self->make_node_insert('methodcall');
        $self->add_child_to_node($id, $pre_id);  # Object
        $self->add_child_to_node($id, $meth_id); # Method (name in $variable)
        for my $kid_id (@$params) {
          $self->add_child_to_node($id, $kid_id);
        }

        $e->[$i-1] = $node;
        splice @$e, $i, 3;
        $i--;
        next;
      } elsif (ref($nxt) eq 'PPI::Token::Cast'
               && $nxt->content() eq '$'
               && ref($nxt_2) eq 'PPI::Structure::Block') {
        # Case 1E: X->${ EXPR }(...) — method whose name (or coderef) is the
        # scalar deref of EXPR.  e.g. Moo::Object's $self->${\(...)}(@_).
        # Build the ${ EXPR } deref node as a (computed/dynamic) method, with
        # optional trailing argument list (already a tree_val node).
        my $pre_id  = $self->parse([$pre]);
        my $meth_id = $self->parse([$nxt, $nxt_2]);  # ${ EXPR } scalar deref
        my ($node, $id) = $self->make_node_insert('methodcall');
        $self->add_child_to_node($id, $pre_id);   # Object
        $self->add_child_to_node($id, $meth_id);  # Method (computed)
        my $count  = 3;  # remove ->, Cast, Block
        my $params = $e->[$i+3];
        if ($params && $self->is_internal_node_type($params)
            && $params->{type} eq 'tree_val') {
          for my $kid_id (@{ $self->get_node_children($params->{id}) }) {
            $self->add_child_to_node($id, $kid_id);
          }
          $count++;  # also consume the params node
        }
        $e->[$i-1] = $node;
        splice @$e, $i, $count;
        $i--;
        next;
      } elsif ($self->is_word($nxt)) {
        # Case 1C: X->method (no parentheses)
        # Method call without arguments, e.g., $obj->DEBUG or $self->nodes
        my $pre_id = $self->parse([$pre]);
        my $meth_id = $self->make_node($nxt);  # Method name as node

        my($node, $id) = $self->make_node_insert('methodcall');
        $self->add_child_to_node($id, $pre_id);  # Object
        $self->add_child_to_node($id, $meth_id); # Method name

        $e->[$i-1] = $node;
        splice @$e, $i, 2;  # Remove -> and method name
        $i--;
        next;
      } elsif (ref($nxt) eq 'PPI::Token::Cast'
               && $nxt->content() =~ /^([\$@%])\*$/) {
        # Postfix deref: X->$* (scalar), X->@* (array), X->%* (hash) — Perl 5.20+
        # Equivalent to $$X, @$X, %$X respectively.
        my $sigil    = $1;
        my $pre_id   = $self->parse([$pre]);
        my $cast_tok = PPI::Token::Cast->new($sigil);
        my ($node, $id) = $self->make_node_insert('prefix_op');
        my $op_id    = $self->make_node($cast_tok);
        $self->add_child_to_node($id, $op_id);   # Cast sigil ($, @, or %)
        $self->add_child_to_node($id, $pre_id);  # Ref being dereferenced
        $e->[$i-1] = $node;
        splice @$e, $i, 2;  # Remove -> and Cast($*/\@*/\%*)
        $i--;
        next;
      } elsif (ref($nxt) eq 'PPI::Token::Cast'
               && $nxt->content() eq '$#*') {
        # Postfix deref: X->$#* — last index of an arrayref (Perl 5.20+).
        # Equivalent to $#{X}; build the same $# prefix_op the braced form uses
        # ($# op token + ref operand) so codegen emits (p-array-last-index X).
        my $pre_id   = $self->parse([$pre]);
        my $cast_tok = PPI::Token::Cast->new('$#');
        my ($node, $id) = $self->make_node_insert('prefix_op');
        my $op_id    = $self->make_node($cast_tok);
        $self->add_child_to_node($id, $op_id);   # $# operator
        $self->add_child_to_node($id, $pre_id);  # Arrayref being dereferenced
        $e->[$i-1] = $node;
        splice @$e, $i, 2;  # Remove -> and Cast($#*)
        $i--;
        next;
      } elsif (ref($nxt) eq 'PPI::Token::Cast'
               && $nxt->content() =~ /^([@%])$/
               && defined($nxt_2)
               && (ref($nxt_2) eq 'PPI::Structure::Subscript'
                   || ref($nxt_2) eq 'PPI::Structure::Block')) {
        # Postfix deref slice: X->@[i,j] / X->@{k,l} / X->%[i,j] / X->%{k,l}
        # (Perl 5.20+).  Equivalent to @{X}[i,j], @{X}{k,l}, %{X}[i,j], %{X}{k,l}
        # — build the same slice node the prefix forms use.
        my $sigil  = $1;
        my $is_arr = ($nxt_2->start() eq '[');
        my $type   = $sigil eq '@'
                     ? ($is_arr ? 'slice_a_acc'    : 'slice_h_acc')
                     : ($is_arr ? 'kv_slice_a_acc' : 'kv_slice_h_acc');
        my $pre_id = $self->parse([$pre]);
        my ($node, $id) = $self->make_node_insert($type);
        $self->add_child_to_node($id, $pre_id);
        my @ix    = $nxt_2->children();
        my $ix_id = $self->_parse_subscript_ix(\@ix, $is_arr);
        my $n     = $self->get_a_node($ix_id);
        if ($self->is_internal_node_type($n) && $n->{type} eq 'progn') {
          # Flatten comma-separated indices/keys into separate children
          my $kids = $self->get_node_children($ix_id);
          $self->add_child_to_node($id, $_) for @$kids;
        } else {
          $self->add_child_to_node($id, $ix_id);
        }
        $e->[$i-1] = $node;
        splice @$e, $i, 3;  # Remove ->, Cast(@/%), and the subscript
        $i--;
        next;
      } elsif (!$self->is_internal_node_type($nxt)
               && $nxt->content() =~ /^\$/) {
        # Case 1D: X->$foo (variable method name, no parentheses)
        # Method call with method name in a variable, no arguments
        # e.g., $obj->$method or $_[0]->$probe
        my $pre_id = $self->parse([$pre]);
        my $meth_id = $self->parse([$nxt]);  # Variable containing method name

        my($node, $id) = $self->make_node_insert('methodcall');
        $self->add_child_to_node($id, $pre_id);  # Object
        $self->add_child_to_node($id, $meth_id); # Method (name in $variable)

        $e->[$i-1] = $node;
        splice @$e, $i, 2;  # Remove -> and $variable
        $i--;
        next;
      } else {
        my $fn = eval { $self->parser->filename } // '(unknown)';
        die "PExpr: unhandled postfix '->' term in $fn: "
          . "term=" . dump($term) . " next=" . dump($nxt)
          . " next2=" . dump($nxt_2) . "\n";
      }
    }

    if ($self->is_arr_or_hash_braces($term)) {
      # X[] or X{}
      my $pre_id= $self->parse([$pre]);
      my $pre_n = $self->get_a_node($pre_id);

      my $type  = ($self->is_arr_braces($term) ? "a_acc" : "h_acc");
      # If it was X->[] or X->{}:
      if ($is_reference) {
        $type   = ($self->is_arr_braces($term) ? "a_ref_acc" : "h_ref_acc");
      } elsif (ref($pre) eq 'PPI::Structure::Block'
               && $pre->start() eq '{'
               && $i >= 2
               && ref($e->[$i-2]) eq 'PPI::Token::Cast'
               && ($e->[$i-2]->content() eq '@'
                   || $e->[$i-2]->content() eq '$')) {
        # Braced deref with a TRAILING subscript: @{EXPR}[..] / @{EXPR}{..} are
        # slices of the deref'd EXPR; ${EXPR}[..] / ${EXPR}{..} are element
        # accesses.  The subscript's position disambiguates at parse time: a
        # subscript AFTER the block makes a slice/element node here, while
        # @{$a[0]} / @{$h{k}} (subscript INSIDE the block, no trailing one)
        # never enter this branch and stay plain casts.  EXPR is an arbitrary
        # expression — an array/hash ref or a symbolic-ref name string;
        # p-aref/p-gethash resolve ref-vs-string at runtime.  The %-sigil kv
        # forms have their own raw-token patterns above ($is_kv_*_deref_*).
        # See docs/symbolic-ref-slice-parse-fix.md.
        $type = $e->[$i-2]->content() eq '@'
              ? ($self->is_arr_braces($term) ? "slice_a_acc" : "slice_h_acc")
              : ($self->is_arr_braces($term) ? "a_ref_acc"   : "h_ref_acc");
      } elsif ($self->is_var($pre_n)
               && $pre_n->content() =~ /^\$/) {
        # Check for $$scalar[n] / $$scalar{key} (Cast '$') or
        # @{$hashref}{keys} / @$scalar{keys} (Cast '@') patterns.
        my $cast_before = ($i >= 2) ? $e->[$i-2] : undef;
        if ($cast_before
            && ref($cast_before) eq 'PPI::Token::Cast'
            && $cast_before->content() eq '$') {
          # $$scalar[n] or $$scalar{key} — dereference ref
          $type = ($self->is_arr_braces($term) ? "a_ref_acc" : "h_ref_acc");
        } elsif ($cast_before
                 && ref($cast_before) eq 'PPI::Token::Cast'
                 && $cast_before->content() eq '@') {
          # @$ref[indices] — ARRAY ref slice (square brackets);
          # @$ref{keys} / @{$hashref}{keys} — HASH ref slice (curly braces).
          # The bracket type decides, NOT the ref type.  (Was always slice_h_acc,
          # so @$ar[0,2] wrongly hit p-hslice → p-gethash on a vector → crash.)
          $type = $self->is_arr_braces($term) ? "slice_a_acc" : "slice_h_acc";
        } elsif ($cast_before
                 && ref($cast_before) eq 'PPI::Token::Cast'
                 && $cast_before->content() eq '%'
                 && $self->is_arr_braces($term)) {
          # %$ref[indices] — KV array slice of array ref
          $type = "kv_slice_a_acc";
        } elsif ($cast_before
                 && ref($cast_before) eq 'PPI::Token::Cast'
                 && $cast_before->content() eq '%'
                 && !$self->is_arr_braces($term)) {
          # %$ref{keys} — KV hash ref slice
          $type = "kv_slice_h_acc";
        }
      } elsif ($self->is_var($pre_n)
               && $pre_n->content() =~ /^@/) {
        $type   = "slice_$type";
      }
      my($node, $id) = $self->make_node_insert($type);

      my @ix    = $term->children();
      my $ix_id = $self->_parse_subscript_ix(\@ix, $self->is_arr_braces($term));

      # Add $pre as child 1
      $self->add_child_to_node($id, $pre_id);

      # Add index to arr or hash:
      if ($type =~ /^slice_/ || $type eq 'kv_slice_h_acc') {
        my $n   = $self->get_a_node($ix_id);
        if ($self->is_internal_node_type($n) && $n->{type} eq 'progn') {
          # Skip the 'progn' for slices:
          my $kids = $self->get_node_children($ix_id);
          for my $param_id (@$kids) {
            $self->add_child_to_node($id, $param_id);
          }
        } else {
          $self->add_child_to_node($id, $ix_id);
        }
      } else {
        $self->add_child_to_node($id, $ix_id);
      }

      # Replace $pre with the new node, remove the subscript term.
      $e->[$i-1] = $node;
      splice @$e, $i, 1;         # Remove $term (subscript)

      if (($type eq 'slice_a_acc' || $type eq 'slice_h_acc'
           || $type eq 'kv_slice_a_acc' || $type eq 'kv_slice_h_acc')
          && $i >= 2
          && ref($e->[$i-2]) eq 'PPI::Token::Cast'
          && ($e->[$i-2]->content() eq '@' || $e->[$i-2]->content() eq '%')) {
        # Also remove the Cast '@'/'%' that precedes the ref symbol
        splice @$e, $i-2, 1;
        $i -= 2;
      } elsif (($type eq 'a_ref_acc' || $type eq 'h_ref_acc')
               && $i >= 2
               && ref($e->[$i-2]) eq 'PPI::Token::Cast'
               && $e->[$i-2]->content() eq '$') {
        # $$scalar[n] / $$scalar{key}: also remove the leading Cast '$'
        # so it doesn't get applied again as a prefix p-cast-$ on the result.
        splice @$e, $i-2, 1;
        $i -= 2;
      } else {
        $i--;
      }
      next;
    }

    # Handle KV slice: %hash{keys} - PPI gives Symbol '%h' + Block '{keys}'
    # (unlike @h{keys} which gives Subscript)
    if (ref($term) eq 'PPI::Structure::Block'
        && $term->start() eq '{'
        && !$self->is_internal_node_type($pre)
        && $self->is_var($pre)
        && $pre->content() =~ /^%/) {
      my $pre_id = $self->parse([$pre]);
      my($node, $id) = $self->make_node_insert('kv_slice_h_acc');

      my @ix    = $term->children();
      my $ix_id = $self->parse(\@ix);

      $self->add_child_to_node($id, $pre_id);

      # Flatten progn children (comma-separated keys)
      my $n = $self->get_a_node($ix_id);
      if ($self->is_internal_node_type($n) && $n->{type} eq 'progn') {
        my $kids = $self->get_node_children($ix_id);
        for my $param_id (@$kids) {
          $self->add_child_to_node($id, $param_id);
        }
      } else {
        $self->add_child_to_node($id, $ix_id);
      }

      $e->[$i-1] = $node;
      splice @$e, $i, 1;
      $i--;
      next;
    }

    # Handle KV array slice: %arr[indices] - PPI gives Symbol '%arr' + Constructor '[...]'
    if ($is_kv_arr_constructor) {
      my $pre_id = $self->parse([$pre]);
      my($node, $id) = $self->make_node_insert('kv_slice_a_acc');

      my @ix    = $term->children();
      my $ix_id = $self->parse(\@ix);

      $self->add_child_to_node($id, $pre_id);

      # Flatten progn children (comma-separated indices)
      my $n = $self->get_a_node($ix_id);
      if ($self->is_internal_node_type($n) && $n->{type} eq 'progn') {
        my $kids = $self->get_node_children($ix_id);
        for my $param_id (@$kids) {
          $self->add_child_to_node($id, $param_id);
        }
      } else {
        $self->add_child_to_node($id, $ix_id);
      }

      $e->[$i-1] = $node;
      splice @$e, $i, 1;
      $i--;
      next;
    }

    # Handle KV array slice via block-deref: %{$ref}[indices]
    if ($is_kv_arr_deref_constructor) {
      my @block_kids = $e->[$i-1]->children();
      my $ref_id = $self->parse(\@block_kids);
      my($node, $id) = $self->make_node_insert('kv_slice_a_acc');

      my @ix    = $term->children();
      my $ix_id = $self->parse(\@ix);

      $self->add_child_to_node($id, $ref_id);

      # Flatten progn children (comma-separated indices)
      my $n = $self->get_a_node($ix_id);
      if ($self->is_internal_node_type($n) && $n->{type} eq 'progn') {
        my $kids = $self->get_node_children($ix_id);
        for my $param_id (@$kids) {
          $self->add_child_to_node($id, $param_id);
        }
      } else {
        $self->add_child_to_node($id, $ix_id);
      }

      $e->[$i-2] = $node;   # Replace Cast '%' position with node
      splice @$e, $i-1, 2;  # Remove Block and Constructor
      $i -= 2;
      next;
    }

    # Handle KV hash slice via block-deref: %{$ref}{"keys"} - Cast('%') + Block('{ref}') + Block('{"keys"}')
    # e.g., %{$h}{"c","d"} -> (p-kv-hslice $h "c" "d")
    if ($is_kv_hash_deref_block) {
      my @block_kids = $e->[$i-1]->children();
      my $ref_id = $self->parse(\@block_kids);
      my($node, $id) = $self->make_node_insert('kv_slice_h_acc');

      my @ix    = $term->children();
      my $ix_id = $self->parse(\@ix);

      $self->add_child_to_node($id, $ref_id);

      # Flatten progn children (comma-separated keys)
      my $n = $self->get_a_node($ix_id);
      if ($self->is_internal_node_type($n) && $n->{type} eq 'progn') {
        my $kids = $self->get_node_children($ix_id);
        for my $param_id (@$kids) {
          $self->add_child_to_node($id, $param_id);
        }
      } else {
        $self->add_child_to_node($id, $ix_id);
      }

      $e->[$i-2] = $node;   # Replace Cast '%' position with node
      splice @$e, $i-1, 2;  # Remove Block and Block
      $i -= 2;
      next;
    }

    # Handle dynamic typeglob slot access: *{EXPR}{SLOT} — Cast('*') +
    # Block('{EXPR}') + Block('{SLOT}'), e.g. *{$glob}{CODE}.
    # Parse the Cast+Block pair into the (p-dynamic-typeglob ...) node, then
    # wrap it in a glob_slot node — same shape as the static *name{SLOT} below.
    if ($is_dyn_typeglob_slot) {
      my $glob_id = $self->parse([$e->[$i-2], $e->[$i-1]]);
      my($node, $id) = $self->make_node_insert('glob_slot');
      $self->add_child_to_node($id, $glob_id);
      $self->_attach_glob_slot($id, $node, $term);
      $e->[$i-2] = $node;   # Replace Cast '*' position with node
      splice @$e, $i-1, 2;  # Remove Block(EXPR) and Block(SLOT)
      $i -= 2;
      next;
    }

    # Handle typeglob slot access: *name{SLOT} — PPI gives Symbol '*name' + Block '{SLOT}'
    # e.g., *_{ARRAY} -> (p-glob-slot (p-make-typeglob "main" "_") "ARRAY")
    if ($is_typeglob_slot) {
      my $glob_id = $self->parse([$pre]);
      my($node, $id) = $self->make_node_insert('glob_slot');
      $self->add_child_to_node($id, $glob_id);
      # Slot: literal bareword (*name{CODE}), scalar var, string, or expression.
      $self->_attach_glob_slot($id, $node, $term);
      $e->[$i-1] = $node;
      splice @$e, $i, 1;
      $i--;
      next;
    }

    # Handle Constructor [ ] after funcall/methodcall - PPI uses Constructor
    # Handle qw[...][idx] — subscript on a qw word list literal
    # qw[void scalar list][1] → (p-aref-deref (vector "void" "scalar" "list") 1)
    if ($is_qw_subscript) {
      my $pre_id = $self->parse([$pre]);
      my($node, $id) = $self->make_node_insert('a_ref_acc');
      my @ix = $term->children();
      my $ix_id = $self->parse(\@ix);
      $self->add_child_to_node($id, $pre_id);
      $self->add_child_to_node($id, $ix_id);
      $e->[$i-1] = $node;
      splice @$e, $i, 1;
      $i--;
      next;
    }

    # instead of Subscript when subscript follows a method call
    # e.g., $obj->method()[$i] has [$i] as Constructor, not Subscript
    if (ref($term) eq 'PPI::Structure::Constructor'
        && $term->start() eq '['
        && $self->is_internal_node_type($pre)) {
      # Treat as array subscript on the result of the previous expression.
      # Mark as list-context subscript: (EXPR)[N] / method()[N] forces list
      # context on the expression, unlike $arr->[N] which is a scalar deref.
      my $pre_id = $pre->{id};
      my($node, $id) = $self->make_node_insert('a_ref_acc');
      $self->node_tree->set_metadata($id, 'list_ctx_subscript', 1);

      my @ix    = $term->children();
      my $ix_id = $self->parse(\@ix);

      $self->add_child_to_node($id, $pre_id);
      $self->add_child_to_node($id, $ix_id);

      $e->[$i-1] = $node;
      splice @$e, $i, 1;
      $i--;
      next;
    }
  }


  # - - - handle ops:

  # Loop, replacing highest precedence 'op' with small tree:
  while(1) {
    my $hi_ix;
    my $hi_prio = -1;
    my($op, $op_name, $op_info);

    for (my $i=0; $i < scalar(@$e); $i++) {
      my $term  = $e->[$i];
      my $info  = $self->op_info($term);
      next
          if !defined $info;

      # Skip marker-only operators (like ':' which is handled by ternary)
      next
          if $info->{no} == 0;

      # Check for unary minus/plus: if '-' or '+' has no operand before it
      # (or previous item is an operator), treat as unary with high precedence
      my $op_str = $self->is_token_operator($term) // '';
      if ($op_str eq '-' || $op_str eq '+') {
        my $is_unary = 0;
        if ($i == 0) {
          $is_unary = 1;  # Nothing before it
        } elsif ($self->is_token_operator($e->[$i-1])) {
          $is_unary = 1;  # Previous is an operator
        }
        if ($is_unary) {
          $info = { assoc => 'r', no => 1, prec => 90 };  # Unary precedence
        }
      }

      if ($info->{prec} >= $hi_prio) {
        # Need to look at left and right associative, for the previous op.
        if ($hi_ix && $info->{prec} == $op_info->{prec}) {
          # Right associative 'x = y = z' should do y=z first.
          # So select the rightmost for replacement first. That is,
          # don't replace.

          next
              if $info->{assoc} eq 'l';

        }

        $hi_ix  = $i;
        $hi_prio= $info->{prec};
        $op     = $term;
        $op_info= $info;
        $op_name= $self->is_token_operator($op);
      }
    }
    last
        if ! defined $hi_ix;

    # Low-precedence prefix 'not' deadlock: 'not' (prec 3) is the loosest prefix
    # operator, so when it is the right operand of a higher-precedence binary op
    # (e.g. '$x = not 5', 'my @a = not $y') it is never selected on its own and
    # the binary op would grab the bare 'not' token.  Reduce the 'not' first.
    # This is safe because, 'not' being looser than everything except and/or/xor,
    # its own right operand is already a single reduced term by now — so it grabs
    # exactly one term, matching Perl ('$x = not 5' => '$x = (not 5)', while
    # 'not $a == $b' still parses as 'not ($a == $b)' since '==' reduced earlier).
    if ($hi_ix + 1 < scalar(@$e)) {
      my $rn      = $e->[$hi_ix + 1];
      my $rn_str  = $self->is_token_operator($rn) // '';
      my $rn_info = $self->op_info($rn);
      if ($rn_str eq 'not' && defined $rn_info && $rn_info->{prec} < $hi_prio) {
        $hi_ix++;
        $op       = $rn;
        $op_info  = $rn_info;
        $op_name  = $rn_str;
        $hi_prio  = $rn_info->{prec};
      }
    }

    say "++++++ Found an op to replace. Got ", $op->content(),
        ", precedence: $hi_prio"                     if 2 & DEBUG;

    # Create the tree:
    my $no_pars = $op_info->{no};

    # Handle chained comparison (e.g. 1 < $x < 10, or a == b != c == d):
    # With assoc='r', hi_ix is the rightmost chained op.  Scan left to find
    # the leftmost chained op in this run, then build a single flat chain node
    # covering all N terms and N-1 operators.
    if ($self->op_is_chained($op_info)) {
      # Only operators of the SAME precedence chain together.  Perl parses
      # `2 != 3 > 4` as `2 != (3 > 4)` (relational `>` is tighter than `!=`),
      # NOT as a chain `(2 != 3) && (3 > 4)`.  Restrict the left-scan to ops
      # whose precedence equals this op's precedence.
      my $left = $hi_ix;
      while ($left >= 2) {
        my $prev_op   = $e->[$left - 2];
        my $prev_info = $self->op_info($prev_op);
        last unless defined $prev_info && $self->op_is_chained($prev_info);
        last unless $prev_info->{prec} == $op_info->{prec};
        $left -= 2;
      }

      if ($left < $hi_ix) {
        # Chain of 2+ operators spanning positions $left-1 .. $hi_ix+1.
        # Positions alternate: term at even offset, op at odd offset from $left-1.
        my @chain_kids;
        for my $pos (($left - 1) .. ($hi_ix + 1)) {
          my $offset = $pos - ($left - 1);
          if ($offset % 2 == 0) {
            push @chain_kids, $self->parse([$e->[$pos]]);   # term
          } else {
            push @chain_kids, $self->make_node($e->[$pos]); # op
          }
        }

        my($top_node, $top_id) = $self->make_node_insert('postfix_op');
        $self->add_child_to_node($top_id, $_) for @chain_kids;

        $e->[$left - 1] = $top_node;
        splice @$e, $left, ($hi_ix + 1) - $left + 1;
        next;
      }
      # else: single isolated chained op — fall through to binary node
    }

# say dump $e; say "---"; say dump $op_info; say dump $self->node_tree; exit 0;


    if ($no_pars == 2) {
      my $prev  = $e->[$hi_ix-1];
      my $post  = $e->[$hi_ix+1];
      my $id_bef= $self->parse([$prev]);

      # Ugly. Set flag for parsing this, so it doesn't add '$_ =~' to regexp:
      my $match_op = ($op_name eq '=~' || $op_name eq '!~');
      if ($match_op && ref($post) =~ /PPI::Token::Regexp/) {
        $post->{_has_match_context}++;
      }

      # Special case for 'isa': RHS bareword class name must stay as a bareword,
      # not be treated as a function call by handle_subcalls inside parse().
      # Convert it to a string token so parse() doesn't call it.
      if ($op_name eq 'isa' && ref($post) eq 'PPI::Token::Word') {
        my $class_name = $post->content();
        $post = PPI::Token::Quote::Single->new("'$class_name'");
      }

      my $id_aft= $self->parse([$post]);

      say "=========   OP replace 2 params for ", $op->content(),
          ", ix $hi_ix.\nParam before:", dump($prev),
          "\nParam after:", dump($post), "\n======"
                                                     if 2 & DEBUG;
      my $n_id  = $self->make_node($op);

      $e->[$hi_ix] = $self->make_subtree_item($n_id);
      $self->add_child_to_node($n_id, $id_bef);
      $self->add_child_to_node($n_id, $id_aft);
      splice @$e, $hi_ix+1, 1;
      splice @$e, $hi_ix-1, 1;

      next;
    } elsif ($no_pars == 3) {
      # Ternary operator (? :)
      # hi_ix points to '?', need to find matching ':'
      my $ternary_prec = $op_info->{prec};  # 15
      my $colon_pos = $self->find_matching_colon($e, $hi_ix + 1);

      if (!defined $colon_pos) {
        die "Ternary operator: Found '?' but no matching ':'\n" . dump($e);
      }

      # Find cond start: scan backwards from ? to find lower-prec op or ':'
      my $cond_start = 0;
      for (my $i = $hi_ix - 1; $i >= 0; $i--) {
        my $info = $self->op_info($e->[$i]);
        if ($info && $info->{prec} <= $ternary_prec) {
          # Stop at lower-precedence operators OR at ':' (which marks
          # outer ternary boundary)
          $cond_start = $i + 1;
          last;
        }
      }

      # Find false end: scan forward from : to find lower-prec operator, OR a
      # ':' marking the boundary of an ENCLOSING ternary.  The latter matters
      # for a nested ternary in the true branch: `A ? B ? C : D : E` reduces the
      # inner `?` first (right-assoc picks the rightmost `?` as hi_ix), and its
      # false branch (`D`) must stop at the outer `:` — which has the same prec
      # 15, so a strict `prec < ternary_prec` test would wrongly swallow `D : E`.
      my $false_end = $#{$e};
      for (my $i = $colon_pos + 1; $i <= $#{$e}; $i++) {
        my $tok_op = $self->is_token_operator($e->[$i]) // '';
        my $info = $self->op_info($e->[$i]);
        if ($tok_op eq ':' || ($info && $info->{prec} < $ternary_prec)) {
          $false_end = $i - 1;
          last;
        }
      }

      say "Ternary: cond_start=$cond_start, ?=$hi_ix, :=$colon_pos, ",
	  "false_end=$false_end"
          if 2 & DEBUG;

      # Extract the three parts
      my @condition  = @$e[$cond_start .. $hi_ix - 1];
      my @true_expr  = @$e[$hi_ix + 1 .. $colon_pos - 1];
      my @false_expr = @$e[$colon_pos + 1 .. $false_end];

      # Parse each part recursively
      my $cond_id  = $self->parse(\@condition);
      my $true_id  = $self->parse(\@true_expr);
      my $false_id = $self->parse(\@false_expr);

      # Build ternary node
      my($ternary_node, $ternary_id) = $self->make_node_insert('ternary');
      $self->add_child_to_node($ternary_id, $cond_id);
      $self->add_child_to_node($ternary_id, $true_id);
      $self->add_child_to_node($ternary_id, $false_id);

      # Replace the ternary portion (cond_start to false_end) with ternary node
      splice @$e, $cond_start, $false_end - $cond_start + 1, $ternary_node;

      next;
    } elsif ($no_pars == 1) {
      # Hiighest prio op is for one param.
      my $prev  = $hi_ix ? $e->[$hi_ix-1] : undef;
      my $post  = ($hi_ix < scalar(@$e)-1) ? $e->[$hi_ix+1] : undef;

      my $postfix;
      my $can_be_postfix = $self->postfix->{$op_name} // 0;
      if ($can_be_postfix == 2) {
        $postfix++;          # Always postfix isn't in Perl (right? :-) )
      } elsif ($can_be_postfix == 1 && $prev) {
        # Might be postfix.
        # t1 op t2. So it must be 't1 <pfix> op t2', or 't1 op t2 <pfix>'.
        # Previous must be a term, next must be an op or end.

        if (! $self->is_token_operator($prev)
            && (! defined $post
                || $self->is_token_operator($post))) {
          $postfix++;
        }
      }

      if ($postfix) {
        # XXXXX Test:
        my $id_bef= $self->parse([$prev]);
        my($node, $id) = $self->make_node_insert('postfix_op');
        my $op_id      = $self->make_node($op);
        $self->add_child_to_node($id, $id_bef); # Expr.
        $self->add_child_to_node($id, $op_id);  # Postfix fun

        $e->[$hi_ix-1] = $node;
        splice @$e, $hi_ix, 1;
        next;
      } else {
        die "Got op '$op_name', not postfix. But there is nothing after it??"
            if ! $post;
        my $id_term    = $self->parse([$post]);
        # Mark \(LIST) so code-gen can distribute refs over list elements.
        # By the time we reach here, Structure::List has been converted to a
        # 'tree_val' PPIreference by the ()→node pass above (lines 704-723).
        if ($op_name eq '\\' && ref($post) eq 'PPIreference'
                             && ($post->{type} // '') eq 'tree_val') {
            $self->node_tree->set_metadata($id_term, 'backslash_paren_list', 1);
        }
        my($node, $id) = $self->make_node_insert('prefix_op');
        my $op_id      = $self->make_node($op);
        $self->add_child_to_node($id, $op_id);     # Prefix operand
        $self->add_child_to_node($id, $id_term);   # Expr.

        $e->[$hi_ix] = $node;
        splice @$e, $hi_ix+1, 1;
        next;
      }

    }

    die "Unknown. Bug. op=" . dump($op) . " info=" . dump($op_info);
  }

  if (scalar(@$e) == 1 && $self->is_internal_node_type($e->[0])) {
    return $self->id_of_internal_node($e->[0]);
  }

  # Single atomic element (number, string, variable, etc.)
  if (scalar(@$e) == 1) {
    return $self->make_node($e->[0]);
  }

  die "Bug. Fell through. Missing case: " . dump($e);
}


# Makes a list of nodes of the children of an expr:
sub make_nodes_from_list {
  my $self      = shift;
  my $list      = shift;

  my @children  = $list->children();
  my $c_ids     = $self->parse_list(\@children);
  return $c_ids;
}

# Gets a list of "," separated parameters to a fun and call parse() on them.
# (Not for qw/foo bar ../, etc.)

# In parameters: array of expr objects, offset to start and to end.
# Returns: Array with list of IDs.
sub parse_list {
  my $self      = shift;
  my $e_list    = shift;        # Won't be changed.
  my $from      = shift;
  my $to        = shift;

  # Copy data structure, since it will be modified in some places.
  if ($from || $to) {
    # Just working with part of that array?
    $to         = scalar(@$e_list)-1 # Default is the rest of expr.
        if ! defined $to;
    my @work    = @$e_list[$from .. $to];
    $e_list     = \@work;
  }

  say "Starting parse_list:\n"                 if 4 & DEBUG;
  $e_list       = $self->cleanup_for_parsing($e_list); # Needed??
  $e_list       = $self->remove_expression_object_around($e_list);
  $e_list       = $self->cleanup_for_parsing($e_list);
  # Strip declarators (my/our/state/local) - they may have been unwrapped above
  my @stripped  = $self->extract_declarations($e_list);
  $e_list       = \@stripped;
  $self->handle_subcalls($e_list, 1); # If a funcall w/o () in the list.

  # 1. Split into list with ","-separated. Eval them
  say "Parts in list:\n", dump $e_list         if 4 & DEBUG;
  my $parts     = $self->parse_comma_separated_list($e_list);
  say "Split into list:\n", dump $parts        if 4 & DEBUG;

  # 2. Call the parts recursively to parse()
  my @node_ids;
  for my $e_part (@$parts) {
    # Skip empty parts (can happen with leading commas)
    next if !@$e_part;
    say "Parse this:", dump $e_part            if 4 & DEBUG;
    my $id      = $self->parse($e_part);
    say "  ==> id: ", dump $id                 if 4 & DEBUG;
    push @node_ids, $id;
  }

  say "Got node ids:", join(", ", @node_ids)   if 4 & DEBUG;
  return \@node_ids;
}

# Returns true if a PPI::Structure::Block looks like a hash constructor:
# first significant token is a bareword followed by =>
# e.g., {a => $_, b => $x}
# Extend an operand-boundary index over a trailing POSTFIX chain, returning the
# new (inclusive) end index.  This is the one place that knows the postfix grammar
#   postfix := [subscript] | {subscript}
#            | -> [..] | -> {..}              (arrow subscript)
#            | -> @* | -> %* | -> $*          (postfix deref)
#            | -> @[..] | -> @{..} | -> %[..] | -> %{..}   (postfix slice)
#            | -> method                      (method name; args are bounded elsewhere)
# It replaces five hand-rolled, subtly-divergent copies of this walk that used to
# live in the named-unary / 1-arg-function operand-boundary logic (some handled
# `-> subscript` but not `-> @*`, etc.).  $end is the index of the last token of
# the term so far; the walk looks at $e->[$end+1] onward.  See
# docs/pexpr-term-parsing-review.md (Option A) for the rationale and Option B for
# the eventual two-phase replacement.
sub _extend_postfix_chain {
  my ($self, $e, $end) = @_;
  my $n = scalar(@$e);
  while ($end + 1 < $n) {
    my $nx = $e->[$end + 1];
    if (ref($nx) eq 'PPI::Structure::Subscript') {
      $end++;                                       # [..] or {..}
      next;
    }
    last unless ref($nx) eq 'PPI::Token::Operator'
             && $nx->content() eq '->'
             && $end + 2 < $n;
    my $after = $e->[$end + 2];
    if (ref($after) eq 'PPI::Structure::Subscript') {
      $end += 2;                                    # -> [..] / -> {..}
    } elsif (ref($after) eq 'PPI::Token::Cast'
             && $after->content() =~ /^[\$\@%]\*$/) {
      $end += 2;                                    # -> @* / %* / $*
    } elsif (ref($after) eq 'PPI::Token::Cast'
             && $after->content() =~ /^[\@%]$/
             && $end + 3 < $n
             && ($e->[$end + 3]->isa('PPI::Structure::Subscript')
                 || $e->[$end + 3]->isa('PPI::Structure::Block'))) {
      $end += 3;                                    # -> @[..]/@{..}/%[..]/%{..}
    } elsif (ref($after) eq 'PPI::Token::Word'
             || ref($after) eq 'PPI::Token::Symbol'
             || ref($after) eq 'PPI::Token::Magic') {
      $end += 2;                                    # -> method (name)
    } else {
      last;
    }
  }
  return $end;
}

# Pre-pass (runs before handle_subcalls): collapse a dynamic typeglob-slot into a
# single glob_slot node, for both spellings:
#   *{EXPR}{SLOT}  — Cast('*') + Block('{EXPR}') + {SLOT}
#   *$var{SLOT}    — Cast('*') + Symbol('$var')  + {SLOT}   (Perl: == *{$var}{SLOT})
# SLOT must be a known glob-slot bareword (CODE/SCALAR/…); it arrives as a Block
# (after a Block glob-name) or a Subscript (after a Symbol glob-name).  Doing this
# early — before handle_subcalls and before $var{SLOT} is read as a hash access —
# lets a preceding named unary (`defined *{$g}{CODE}` in Sub::Override) grab the
# whole glob-slot as one argument.  The in-loop handler (~line 1234) still covers
# the Block/Block form reached via later recursion.
# PPI mis-tokenizes the subscript after a braced array/scalar deref: in
# `${$ref}[idx]` and `@{$ref}[i,j]` the `[...]` arrives as a
# PPI::Structure::Constructor (an anonymous-array literal) rather than a
# PPI::Structure::Subscript — only because it follows a Block `}` rather than a
# Symbol.  (The hash form `${$ref}{key}` is correctly a Subscript, which is why
# it already works.)  Left alone, the Cast+Block+Constructor triple matches no
# case in the main loop and the parse falls through to the "Missing case" die,
# which degrades to a silent `undef`.  Re-tag the Constructor as a Subscript so
# the existing Cast+Block+Subscript machinery (the same path `${$ref}{key}`
# uses) handles it.  `%`-cast (KV array slice `%{$ref}[i]`) and `*`-cast (glob)
# are left as Constructors — they have their own dedicated handlers.
sub _retag_braced_deref_subscript {
  my ($self, $e) = @_;
  for (my $i = 2; $i < scalar(@$e); $i++) {
    my $term = $e->[$i];
    next unless ref($term) eq 'PPI::Structure::Constructor'
             && $term->start() eq '[';
    my $block = $e->[$i-1];
    my $cast  = $e->[$i-2];
    next unless ref($block) eq 'PPI::Structure::Block'
             && $block->start() eq '{'
             && ref($cast) eq 'PPI::Token::Cast'
             && ($cast->content() eq '$' || $cast->content() eq '@');
    bless $term, 'PPI::Structure::Subscript';   # correct PPI's misclassification
  }
}

sub _precollapse_dyn_glob_slots {
  my ($self, $e) = @_;
  for (my $i = 2; $i < scalar(@$e); $i++) {
    my $term = $e->[$i];
    next unless (ref($term) eq 'PPI::Structure::Block'
                 || ref($term) eq 'PPI::Structure::Subscript')
             && $term->start() eq '{'
             && $self->_block_is_glob_slot($term);
    my $cast = $e->[$i-2];
    my $name = $e->[$i-1];
    next unless ref($cast) eq 'PPI::Token::Cast' && $cast->content() eq '*';
    my $name_ok = (ref($name) eq 'PPI::Structure::Block' && $name->start() eq '{')
               || (ref($name) eq 'PPI::Token::Symbol'    && $name->content() =~ /^\$/);
    next unless $name_ok;
    my $glob_id = $self->parse([$cast, $name]);
    my ($node, $id) = $self->make_node_insert('glob_slot');
    $self->add_child_to_node($id, $glob_id);
    $self->_attach_glob_slot($id, $node, $term);
    $e->[$i-2] = $node;     # replace Cast '*' position with the glob_slot node
    splice @$e, $i-1, 2;    # remove glob-name and SLOT tokens
    $i -= 2;
  }
}

# Classify the SLOT block of a dynamic glob-slot access *{EXPR}{SLOT}.
# Returns () if BLOCK is not a glob slot, otherwise a (kind, value) pair:
#   ('lit',  "CODE")     — a literal bareword slot ({CODE}); Perl's glob-slot
#                          autoquote means the bareword is the *string* "CODE",
#                          not a call to sub CODE, so it is recorded verbatim.
#   ('expr', \@tokens)   — anything else: a scalar var ({$type}), a string
#                          ({"CODE"}), or a full expression ({uc $x}, {"CO".$s}).
#                          Parsed and evaluated at runtime; p-glob-slot stringifies
#                          the result.  Moo's glob-copy loop uses the {$type} form.
# Only ever consulted in a `*`-cast-guarded context (every caller requires a
# preceding Cast '*'), and a glob has no hash-element semantics, so accepting an
# arbitrary expression here cannot make ordinary hash access $h{$k} mis-parse.
sub _glob_slot_spec {
  my ($self, $block) = @_;
  my @ch = grep { ref($_) !~ /Whitespace|Comment/ } $block->children();
  if (@ch == 1 && $ch[0]->isa('PPI::Statement')) {
    @ch = grep { ref($_) !~ /Whitespace|Comment/ } $ch[0]->children();
  }
  return () unless @ch;
  # Lone bareword slot — restricted to the known slot names so an unknown bareword
  # isn't silently swallowed as a glob slot (it falls through to normal parsing).
  if (@ch == 1 && $ch[0]->isa('PPI::Token::Word')) {
    my $name = $ch[0]->content;
    return $name =~ /^(?:SCALAR|ARRAY|HASH|CODE|IO|GLOB|NAME|PACKAGE|FORMAT)$/
         ? ('lit', $name) : ();
  }
  # Everything else (scalar/string/expression) is computed at runtime.
  return ('expr', \@ch);
}

# True if BLOCK is a glob slot: {CODE}, {SCALAR}, ..., {$type}, or any expression.
# Used to recognize the SLOT block of *{EXPR}{SLOT} dynamic glob-slot access.
sub _block_is_glob_slot {
  my ($self, $block) = @_;
  return scalar($self->_glob_slot_spec($block)) ? 1 : 0;
}

# Attach the SLOT of a glob_slot NODE (whose glob is already child 0) from BLOCK:
# a literal bareword sets {slot_name}; anything else is parsed as a child
# expression and flagged {slot_is_expr} (codegen reads child 1, runtime stringifies).
sub _attach_glob_slot {
  my ($self, $id, $node, $block) = @_;
  my ($kind, $slot) = $self->_glob_slot_spec($block);
  if ($kind eq 'expr') {
    $self->add_child_to_node($id, $self->parse($slot));  # $slot = \@tokens
    $node->{slot_is_expr} = 1;
  } elsif ($kind eq 'lit') {
    $node->{slot_name} = $slot;
  } else {
    # Unrecognized lone bareword (e.g. *name{SOMEWORD}): keep its text verbatim,
    # defaulting to SCALAR for an empty block — matches the historical static
    # *name{SLOT} behavior (p-glob-slot returns undef for an unknown slot).
    my @ch = grep { ref($_) !~ /Whitespace|Comment/ } $block->children();
    @ch = grep { ref($_) !~ /Whitespace|Comment/ } $ch[0]->children()
        if @ch == 1 && $ch[0]->isa('PPI::Statement');
    $node->{slot_name} = @ch ? $ch[0]->content() : 'SCALAR';
  }
}

sub _block_is_hash_constructor {
  my $block = shift;
  my @ch = grep { ref($_) !~ /Whitespace|Comment/ } $block->children();
  if (@ch == 1 && $ch[0]->isa('PPI::Statement')) {
    @ch = grep { ref($_) !~ /Whitespace|Comment/ } $ch[0]->children();
  }
  return @ch >= 2
      && ref($ch[0]) eq 'PPI::Token::Word'
      && ref($ch[1]) eq 'PPI::Token::Operator'
      && $ch[1]->content() eq '=>';
}

# If a deref BLOCK contains exactly one bareword identifier (e.g. the `foo` in
# ${foo} / @{foo} / %{foo}), return that identifier — Perl autoquotes it into a
# symbolic ref to the package variable of that name.  Returns undef for anything
# else (a sub call `foo()` has a trailing List; `$ref`/`[...]`/`\ ...` are not a
# lone Word; multi-token blocks are expressions), so those keep their normal
# parse.
sub _block_sole_bareword {
  my $block = shift;
  my @ch = grep { ref($_) !~ /Whitespace|Comment/ } $block->children();
  if (@ch == 1 && $ch[0]->isa('PPI::Statement')) {
    @ch = grep { ref($_) !~ /Whitespace|Comment/ } $ch[0]->children();
  }
  return undef unless @ch == 1 && ref($ch[0]) eq 'PPI::Token::Word';
  my $w = $ch[0]->content();
  return ($w =~ /\A\w+(?:::\w+)*\z/ && $w !~ /\A\d/) ? $w : undef;
}


# This replaces all sub calls in an expression.
# It use known number of parameters for subs and priorities.

sub handle_subcalls {
  my $self       = shift;
  my $e          = shift;
  my $in_arglist = shift // 0;  # 1 when called from parse_list (inside explicit parens)

  say "---- handle_subcalls. Incoming expr:\n", dump($e)     if 8 & DEBUG;

  # - - - Pre-pass: normalize CORE::<builtin> to the bare builtin name.
  # `CORE::foo` explicitly names Perl's builtin (bypassing any override).  PCL
  # has no overridable builtins, so CORE::foo == foo.  Rewriting the token here
  # makes ALL downstream logic — named-unary detection, param specs, funcall
  # recognition — treat it as the builtin (codegen already maps both to p-foo).
  # Without this, `CORE::ref $x` / `CORE::shift` (no parens) parse as barewords.
  for my $tok (@$e) {
    next unless ref($tok) eq 'PPI::Token::Word';
    my $c = $tok->content();
    if ($c =~ /^CORE::(\w+)$/ && exists $self->known_no_of_params->{$1}) {
      $tok->set_content($1);
    }
  }

  # - - - Pre-pass (BEFORE fun(list) loop): Handle general indirect object syntax
  # "METHOD ClassName ARGS" → ClassName->METHOD(ARGS)
  # "METHOD $obj ARGS"      → $obj->METHOD(ARGS)
  # Must run BEFORE the fun(list) loop that transforms ClassName(ARGS) into a funcall,
  # which would prevent us from detecting the pattern.
  # Only triggers for non-builtin, non-keyword method names followed by uppercase Word or Symbol.
  {
    my %_indirect_skip = map { $_ => 1 } qw(
      my our local state
      return next last redo goto
      if unless elsif else
      while until for foreach do
      eval sub package use require no
      BEGIN END CHECK UNITCHECK INIT
      and or not xor CORE new
    );
    for (my $i = 0; $i < scalar(@$e) - 1; $i++) {
      my $now  = $e->[$i];
      my $next = $e->[$i+1];

      # First token must be a plain Word token (method name)
      next unless ref($now) eq 'PPI::Token::Word';
      next if $self->is_token_operator($now);
      my $method_name = $now->content;

      # Skip control keywords and "new" (handled by later pre-pass)
      next if $_indirect_skip{$method_name};

      # Skip known builtin functions
      next if exists $self->known_no_of_params->{$method_name};

      # Skip all-uppercase words: they are filehandles (STDIN/STDOUT/STDERR)
      # or constants, never method names in indirect-object syntax
      next if $method_name =~ /^[A-Z][A-Z0-9_]*$/;

      # Skip if preceded by -> (this is a method name, not an invocant position)
      if ($i > 0 && $self->is_arrow_op($e->[$i-1])) {
        next;
      }

      # Determine invocant: next token must be uppercase-starting Word (class name)
      # or a Symbol ($var) as object reference
      my $invocant = $next;
      my $invocant_is_class = 0;
      if (ref($invocant) eq 'PPI::Token::Word'
          && !$self->is_token_operator($invocant)
          && $invocant->content =~ /^[A-Z]/) {
        # Skip all-uppercase invocants unless they are known declared packages:
        # unqualified all-caps words are typically filehandles (STDIN/STDOUT),
        # special blocks (BEGIN/END), or constants — not class names.
        # Exception: if the name is a known package, allow it as indirect invocant.
        if ($invocant->content =~ /^[A-Z][A-Z0-9_]*$/) {
          my $is_known_pkg = $self->has_environment
              && $self->environment->is_package($invocant->content);
          next unless $is_known_pkg;
        }
        $invocant_is_class = 1;
      } elsif (ref($invocant) eq 'PPI::Token::Symbol'
               && $invocant->content =~ /^\$/) {
        # If the token right after the symbol is ++ or --, this is $var++
        # (postfix operator on the invocant), not indirect object syntax
        if ($i + 2 <= scalar(@$e) - 1) {
          my $after_inv = $e->[$i+2];
          if (ref($after_inv) eq 'PPI::Token::Operator') {
            my $op = $after_inv->content;
            next if $op eq '++' || $op eq '--';
          }
        }
        $invocant_is_class = 0;
      } else {
        next;
      }

      # Find end of arg span: stop before low-priority operators (and/or/xor)
      # Find end of arg span.
      # Cases:
      # 1. Args in explicit parens: METHOD INV (args) → i+2 is a Structure::List.
      #    Stop at next ',' so we don't grab outer expression elements.
      # 2. No args (next is ',' separator or end of array): METHOD INV, other → no args.
      # 3. Bare args: METHOD INV a, b, c → grab all args until and/or/xor.
      my $args_explicit_parens = ($i + 2 <= scalar(@$e) - 1
                                  && ref($e->[$i+2]) eq 'PPI::Structure::List');
      # has_no_args: true when the invocant is the last token OR is immediately followed
      # by a comma (which is an outer-call arg separator, not a method arg).
      # e.g.  "method Pack, extra"  → Pack at i+2 is ',', so has_no_args=1 → only Pack
      my $has_no_args = 0;
      if ($i + 2 > scalar(@$e) - 1) {
        $has_no_args = 1;
      } elsif (!$args_explicit_parens) {
        my $first_arg_op = $self->is_token_operator($e->[$i+2]);
        if (defined $first_arg_op && $first_arg_op eq ',') {
          $has_no_args = 1;
        }
      }

      # For $variable invocants: require explicit parens around args OR be inside
      # an explicit arg list where the invocant is immediately followed by a comma
      # (meaning the comma is an outer separator, not part of the method's args).
      # "func $var, args" is ambiguous in standalone context — almost always a
      # normal function call (ok $x, 'desc', cmp_ok $a, '==', $b, etc.).
      # Exception: inside explicit parens (in_arglist=1), "method $obj, outer_arg"
      # with comma right after invocant is unambiguously "method($obj), outer_arg".
      # e.g. is(method $obj, "expected") → (p-method-call $obj 'method), "expected"
      my $comma_after_invocant = $has_no_args && ($i + 2 <= scalar(@$e) - 1);
      my $var_invocant_ok = $args_explicit_parens
          || ($in_arglist && $comma_after_invocant);
      next if !$invocant_is_class && !$var_invocant_ok;

      my $end_pars = $i + 1;  # default: just invocant, no args
      unless ($has_no_args) {
        $end_pars = scalar(@$e) - 1;
        for my $j ($i + 2 .. scalar(@$e) - 1) {
          my $op = $self->is_token_operator($e->[$j]);
          if (defined $op) {
            if ($op eq 'and' || $op eq 'or' || $op eq 'xor') {
              $end_pars = $j - 1;
              last;
            }
            if ($args_explicit_parens && $op eq ',') {
              $end_pars = $j - 1;
              last;
            }
          }
        }
      }

      # Build methodcall node: kids[0]=invocant, kids[1]=method, kids[2+]=args
      my($mc_node, $mc_id) = $self->make_node_insert('methodcall');

      # kids[0]: invocant
      if ($invocant_is_class) {
        # Class name bareword: wrap in funcall (gen_methodcall expects this shape)
        my($class_fc_node, $class_fc_id) = $self->make_node_insert('funcall');
        $self->add_child_to_node($class_fc_id, $self->make_node($invocant));
        $self->add_child_to_node($mc_id, $class_fc_id);
      } else {
        # $variable object: parse directly
        my $inv_id = $self->parse([$invocant]);
        $self->add_child_to_node($mc_id, $inv_id);
      }

      # kids[1]: method name
      $self->add_child_to_node($mc_id, $self->make_node($now));

      # kids[2+]: args (if any, after the invocant)
      if ($end_pars >= $i + 2) {
        my $arg_ids = $self->parse_list($e, $i + 2, $end_pars);
        for my $arg_id (@$arg_ids) {
          $self->add_child_to_node($mc_id, $arg_id);
        }
      }

      # Replace "METHOD INVOCANT [ARGS]" span with the single methodcall node
      splice @$e, $i, $end_pars - $i + 1, $mc_node;
    }
  }

  # - - - Pre-pass: Handle indirect object syntax "new ClassName ARGS"
  # Equivalent to ClassName->new(ARGS).  Must run BEFORE the fun(list) loop
  # below, which would otherwise collapse "ClassName(ARGS)" into a plain
  # funcall(ClassName, ARGS) and hide the indirect pattern (so `new Foo(1,2)`
  # mis-parsed as `new(Foo(1,2))`).  This is a dedicated handler for `new`
  # because the general indirect pre-pass skips all-caps invocants unless they
  # are known packages — but after the keyword `new`, even an all-caps bareword
  # (`new CGI`) is unambiguously a class name, never a filehandle.
  # Detects: Word("new") followed by a bare Word class name (not an operator).
  for (my $i = 0; $i < scalar(@$e) - 1; $i++) {
    my $now  = $e->[$i];
    my $next = $e->[$i+1];
    next unless $self->is_word($now) && $now->content() eq 'new';
    next unless ref($next) eq 'PPI::Token::Word';
    next if $self->is_token_operator($next);

    my $class_word = $next;

    # Find end of args.
    # Explicit parens — `new Foo(ARGS)` — the args are exactly the single
    # Structure::List at i+2; do NOT grab trailing tokens (`new Foo(1), $x`).
    # Bare args — `new Foo 1, 2` — grab everything up to a low-priority operator.
    my $end_pars;
    if (ref($e->[$i+2] // '') eq 'PPI::Structure::List') {
      $end_pars = $i + 2;
    } else {
      $end_pars = scalar(@$e) - 1;
      for my $j ($i + 2 .. scalar(@$e) - 1) {
        my $op = $self->is_token_operator($e->[$j]);
        if (defined $op && ($op eq 'and' || $op eq 'or' || $op eq 'xor')) {
          $end_pars = $j - 1;
          last;
        }
      }
    }

    # Build methodcall node: kids[0]=funcall{ClassName}, kids[1]=Word("new"), kids[2+]=args
    my($mc_node, $mc_id) = $self->make_node_insert('methodcall');

    # kids[0]: funcall wrapping the class name word (shape gen_methodcall expects)
    my($class_fc_node, $class_fc_id) = $self->make_node_insert('funcall');
    $self->add_child_to_node($class_fc_id, $self->make_node($class_word));
    $self->add_child_to_node($mc_id, $class_fc_id);

    # kids[1]: the method name "new"
    $self->add_child_to_node($mc_id, $self->make_node($e->[$i]));

    # kids[2..N]: arguments (if any)
    if ($end_pars >= $i + 2) {
      my $arg_ids = $self->parse_list($e, $i + 2, $end_pars);
      for my $arg_id (@$arg_ids) {
        $self->add_child_to_node($mc_id, $arg_id);
      }
    }

    # Replace "new ClassName ARGS" span with the single methodcall node
    splice @$e, $i, $end_pars - $i + 1, $mc_node;
  }

  # - - - Handle: `fun(...)`:
  # (Yes, loops to all but last.)
  for(my $i=0; $i < scalar(@$e)-1; $i++) {
    my $now     = $e->[$i];
    my $next    = $e->[$i+1];
    say "handle_subcalls: Look for subname(..) in:\n", dump $now  if 8 & DEBUG;

    # Handle &funcname( list ) - direct function call with & sigil
    # e.g., &foo(1, 2) -> (pl-foo 1 2), &Pkg::foo(1,2) -> (Pkg::pl-foo 1 2)
    # The `&` sigil forces the user sub even when the name is a builtin (Perl
    # semantics: `&connect()` calls a user `sub connect`, not the builtin), via
    # force_user_sub. Gated on a trailing list, so `\&NAME` (no list) stays a
    # code-ref refgen and never reaches here.
    if (ref($now) eq 'PPI::Token::Symbol'
        && $now->content() =~ /^&(.+)$/
        && $self->is_list($next)) {
      my $func_name = $1;
      my $word_token = PPI::Token::Word->new($func_name);
      my($top_node, $top_id) = $self->make_node_insert('funcall');
      $top_node->{force_user_sub} = 1;
      my $c_ids = $self->make_nodes_from_list($next);
      my $node_id = $self->make_node($word_token);
      $self->add_child_to_node($top_id, $node_id);
      for my $c_id (@$c_ids) {
        $self->add_child_to_node($top_id, $c_id);
      }
      splice @$e, $i, 2, $top_node;
      next;
    }

    # Handle &$scalar(args) and &{expr}(args) — code ref call via & sigil
    # e.g., &$foo(1, 2)      -> (pl-funcall-ref $foo 1 2)
    # e.g., &{$arr[0]}(args) -> (pl-funcall-ref ($arr[0]) args...)
    if (ref($now) eq 'PPI::Token::Cast' && $now->content() eq '&'
        && $i + 2 < scalar(@$e) && $self->is_list($e->[$i+2])) {
      my $operand = $next;
      my $list    = $e->[$i+2];
      my $ref_id;
      if (ref($operand) eq 'PPI::Token::Symbol' && $operand->content() =~ /^\$/) {
        # &$scalar(args)
        $ref_id = $self->parse([$operand]);
      } elsif (ref($operand) eq 'PPI::Structure::Block') {
        # &{expr}(args) — parse the expression inside the braces
        my @blk_ch = grep { ref($_) !~ /Whitespace/ } $operand->children();
        if (@blk_ch == 1 && $blk_ch[0]->isa('PPI::Statement')) {
          @blk_ch = grep { ref($_) !~ /Whitespace/ } $blk_ch[0]->children();
        }
        $ref_id = @blk_ch ? $self->parse(\@blk_ch) : undef;
      }
      if (defined $ref_id) {
        my($top_node, $top_id) = $self->make_node_insert('ref_funcall');
        $self->add_child_to_node($top_id, $ref_id);
        my $c_ids = $self->make_nodes_from_list($list);
        for my $c_id (@$c_ids) {
          $self->add_child_to_node($top_id, $c_id);
        }
        # 4-arg splice: replace 3 elements (Cast+Symbol+List) with 1 node,
        # preserving any elements after $i+2 (e.g. comma and more args).
        splice @$e, $i, 3, $top_node;
        next;
      }
    }

    next
        if !$self->is_word($now); # Only want function calls.

    # Strip PPI::Token::Prototype after 'sub' keyword for anonymous subs.
    # e.g., sub (&) { ... } → sub { ... }
    # The prototype has no effect on generated CL code; removing it lets
    # the normal sub { BLOCK } handler below fire correctly.
    if ($now->content() eq 'sub'
        && $i + 1 < scalar(@$e)
        && ref($e->[$i+1]) eq 'PPI::Token::Prototype') {
      splice @$e, $i+1, 1;  # drop the prototype token
      $next = ($i+1 < scalar(@$e)) ? $e->[$i+1] : undef;
    }

    say "handle_subcalls() Look for subname(..), was word. Is next list ",
        ($self->is_list($next) ? "Yes" : "No"), ". Dump:", dump $next
        if 8 & DEBUG;

    # Handle grep/map( { BLOCK } LIST ) — paren form
    # e.g., map({$_} @list) — PPI gives Structure::List wrapping a Statement
    #   Structure::List → Statement → [Block, rest...]
    if ($self->is_list($next)) {
      my $func_name = $now->content();
      if ($func_name eq 'grep' || $func_name eq 'map' || $func_name eq 'sort') {
        # PPI wraps the list content in a Statement — unwrap it
        my @outer_ch = grep { ref($_) !~ /Whitespace/ } $next->children();
        my @inner_ch;
        if (@outer_ch == 1 && $outer_ch[0]->isa('PPI::Statement')) {
          @inner_ch = grep { ref($_) !~ /Whitespace/ } $outer_ch[0]->children();
        } else {
          @inner_ch = @outer_ch;
        }
        # sort( NAME LIST ) — named comparator in paren form
        # e.g. sort( Backwards @arr ) where Backwards is a sub name
        if ($func_name eq 'sort'
            && @inner_ch
            && $inner_ch[0]->isa('PPI::Token::Word')) {
          my $comp_name = $inner_ch[0]->content();
          my $is_builtin = exists $self->known_no_of_params->{$comp_name};
          # If NAME is followed immediately by (...), it's a function call: sort(func(args))
          # not a comparator: sort(NAME LIST).
          my $is_funcall = (@inner_ch >= 2 && ref($inner_ch[1]) eq 'PPI::Structure::List');
          unless ($is_builtin || $is_funcall
                  || $comp_name =~ /^(?:CORE|my|our|local|sub|if|else|elsif|unless|while|until|for|foreach|do|return|use|package|BEGIN|END|not|and|or|eq|ne|lt|gt|le|ge|cmp|x)$/
                  || $comp_name =~ /^CORE::/) {
            my($top_node, $top_id) = $self->make_node_insert('funcall');
            my $node_id = $self->make_node($now);
            $self->add_child_to_node($top_id, $node_id);
            my($lambda_node, $lambda_id) = $self->make_node_insert('inline_lambda');
            $lambda_node->{params}          = ['$a', '$b'];
            $lambda_node->{body_cl}         = 'nil';
            $lambda_node->{for_func}        = 'sort';
            $lambda_node->{comparator_name} = $comp_name;
            $self->add_child_to_node($top_id, $lambda_id);
            # Rest of inner_ch (after NAME, skip optional leading comma)
            my @rest_ch = @inner_ch[1..$#inner_ch];
            if (@rest_ch && ref($rest_ch[0]) eq 'PPI::Token::Operator'
                && $rest_ch[0]->content eq ',') {
              shift @rest_ch;
            }
            if (@rest_ch) {
              my $rest_expr = $self->cleanup_for_parsing(\@rest_ch);
              my $rest_ids  = $self->parse_list($rest_expr);
              for my $rid (@$rest_ids) {
                $self->add_child_to_node($top_id, $rid);
              }
            }
            splice @$e, $i, 2, $top_node;
            next;
          }
        }

        if (@inner_ch && ref($inner_ch[0]) eq 'PPI::Structure::Block') {
          my $block = $inner_ch[0];
          # Rest: children after the block; strip only the optional leading comma
          # (grep({ block }, LIST) has a comma between block and list, but
          # grep({ block } 1, 2, 3) needs the inner commas for parse_list).
          my @rest_ch = @inner_ch[1..$#inner_ch];
          if (@rest_ch && ref($rest_ch[0]) eq 'PPI::Token::Operator' && $rest_ch[0]->content eq ',') {
            shift @rest_ch;
          }
          # If rest is a single Structure::List, expand its children
          if (@rest_ch == 1 && ref($rest_ch[0]) eq 'PPI::Structure::List') {
            @rest_ch = grep { ref($_) !~ /Whitespace/ } $rest_ch[0]->children();
            # Unwrap inner Statement if present
            if (@rest_ch == 1 && $rest_ch[0]->isa('PPI::Statement')) {
              @rest_ch = grep { ref($_) !~ /Whitespace/ } $rest_ch[0]->children();
            }
          }

          my($top_node, $top_id) = $self->make_node_insert('funcall');
          my $node_id = $self->make_node($now);
          $self->add_child_to_node($top_id, $node_id);

          if ($self->has_parser) {
            my $params = ($func_name eq 'sort') ? ['$a', '$b'] : ['$_'];
            my $body_cl = _block_is_hash_constructor($block)
              ? $self->parser->parse_hash_block_to_cl_string($block)
              : $self->parser->parse_block_to_cl_string($block);

            # Handle -> deref chain after block in paren form: grep({HASH}->{key}, LIST)
            # @rest_ch starts with -> subscript pairs; consume them into body_cl.
            while (@rest_ch >= 2
                   && ref($rest_ch[0]) eq 'PPI::Token::Operator'
                   && $rest_ch[0]->content eq '->'
                   && ref($rest_ch[1]) eq 'PPI::Structure::Subscript') {
              my $sub = $rest_ch[1];
              my $start = $sub->start->content;
              my $key_cl = _subscript_to_cl_str($sub, $self, $start eq '[');
              last unless defined $key_cl;
              $body_cl = ($start eq '{')
                  ? "(p-gethash-deref $body_cl $key_cl)"
                  : "(p-aref-deref $body_cl $key_cl)";
              splice @rest_ch, 0, 2;
            }

            my($lambda_node, $lambda_id) = $self->make_node_insert('inline_lambda');
            $lambda_node->{params}   = $params;
            $lambda_node->{body_cl}  = $body_cl;
            $lambda_node->{for_func} = $func_name;
            $self->add_child_to_node($top_id, $lambda_id);
          } else {
            my @bc = $block->children();
            my $be = $self->cleanup_for_parsing(\@bc);
            my $bid = $self->parse($be);
            my($sub_node, $sub_id) = $self->make_node_insert('anon_sub');
            $self->add_child_to_node($sub_id, $bid);
            $self->add_child_to_node($top_id, $sub_id);
          }

          if (@rest_ch) {
            my $rest_expr = $self->cleanup_for_parsing(\@rest_ch);
            my $rest_ids  = $self->parse_list($rest_expr);
            for my $rid (@$rest_ids) {
              $self->add_child_to_node($top_id, $rid);
            }
          }

          splice @$e, $i, 2, $top_node;
          next;
        }
      }
    }

    # Handle grep/map { BLOCK } LIST pattern
    # Uses Parser.pm callback for multi-statement blocks
    # Also handles: sub { ... } (anonymous subs)
    # Also handles: any function with & prototype (e.g., try { } from Try::Tiny)
    if (ref($next) eq 'PPI::Structure::Block') {
      my $func_name = $now->content();

      # Check if this function has & prototype (block arg)
      my $has_block_proto = 0;
      if ($self->environment) {
        my $proto = $self->environment->get_prototype($func_name);
        $has_block_proto = $proto && $proto->{has_block_arg};
      }

      if ($func_name eq 'grep' || $func_name eq 'map' || $func_name eq 'sort'
          || $func_name eq 'eval' || $func_name eq 'do' || $has_block_proto) {

        # Create funcall with block as first param
        my($top_node, $top_id) = $self->make_node_insert('funcall');
        my $node_id = $self->make_node($now);
        $self->add_child_to_node($top_id, $node_id);

        # Use parser callback if available (handles multi-statement blocks)
        my $deref_skip = 0;  # extra elements consumed by -> deref chain after block
        if ($self->has_parser) {
          # Determine parameters based on function type
          my $params = ($func_name eq 'sort') ? ['$a', '$b']
                     : ($func_name eq 'eval') ? []
                     : ($func_name eq 'grep' || $func_name eq 'map') ? ['$_']
                     : [];  # Other & prototype functions: no implicit params

          # For grep/map/sort/eval, use inline lambda (cleaner, avoids emission issues)
          # eval { } in expression context must use inline form — defun side-effect would
          # corrupt the surrounding p-if argument list (e.g. eval{} inside elsif condition).
          # For other blocks, use named function (may need to be called separately)
          if ($func_name eq 'grep' || $func_name eq 'map' || $func_name eq 'sort'
              || $func_name eq 'eval') {
            # Parse block body as CL string
            my $body_cl = _block_is_hash_constructor($next)
              ? $self->parser->parse_hash_block_to_cl_string($next)
              : $self->parser->parse_block_to_cl_string($next);

            # Handle -> deref chain after block: grep {HASH}->{key}, LIST
            # Consume any leading '-> subscript' pairs from @$e[$i+2..], wrapping body_cl.
            while ($i + 2 + $deref_skip < @$e
                   && ref($e->[$i + 2 + $deref_skip]) eq 'PPI::Token::Operator'
                   && $e->[$i + 2 + $deref_skip]->content eq '->'
                   && $i + 3 + $deref_skip < @$e
                   && ref($e->[$i + 3 + $deref_skip]) eq 'PPI::Structure::Subscript') {
              my $sub = $e->[$i + 3 + $deref_skip];
              my $start = $sub->start->content;
              my $key_cl = _subscript_to_cl_str($sub, $self, $start eq '[');
              last unless defined $key_cl;
              if ($start eq '{') {
                $body_cl = "(p-gethash-deref $body_cl $key_cl)";
              } else {
                $body_cl = "(p-aref-deref $body_cl $key_cl)";
              }
              $deref_skip += 2;
            }

            # Create inline_lambda node
            my($lambda_node, $lambda_id) = $self->make_node_insert('inline_lambda');
            $lambda_node->{params}   = $params;
            $lambda_node->{body_cl}  = $body_cl;
            $lambda_node->{for_func} = $func_name;
            $lambda_node->{deref_skip} = $deref_skip;
            $self->add_child_to_node($top_id, $lambda_id);
          } elsif ($func_name eq 'do') {
            # do { } : emit an INLINE lambda (return_lambda=1) rather than a
            # named defun.  A defun side-effect would be written into the output
            # stream at the current position, which corrupts a surrounding p-if
            # when the do{} sits in an elsif condition (the defun lands between
            # the p-if branches).  Unlike parse_block_to_cl_string, the
            # return_lambda path runs the block through _process_block, so the
            # bare-if tail-return semantics (`do { 1 if $x }` returns the
            # condition value when the modifier suppresses the expression) are
            # preserved.  do{} is a plain 0-arg block (is_anon_sub=0).  The
            # loop_transparent flag (5th arg) wraps the body in (progn ...) not
            # (block nil ...), so an unlabeled last/next/redo inside the do{}
            # escapes to the enclosing loop, matching Perl.
            my $lambda_str =
              $self->parser->parse_block_as_function($next, [], 0, 1, 1);
            my($ref_node, $ref_id) = $self->make_node_insert('func_ref');
            $ref_node->{raw_lambda} = $lambda_str;
            $self->add_child_to_node($top_id, $ref_id);
          } else {
            # Parse block as a named function and get its name.
            # A &-prototype sub (e.g. try/catch) receives the block as an
            # anonymous sub: it must accept call arguments via @_, since the
            # caller may invoke it with args (Try::Tiny's catch passes $error).
            my $block_func_name =
              $self->parser->parse_block_as_function($next, $params, $has_block_proto);

            # Create a func_ref node that holds the function name
            my($ref_node, $ref_id) = $self->make_node_insert('func_ref');
            $ref_node->{func_name} = $block_func_name;
            $self->add_child_to_node($top_id, $ref_id);
          }
        } else {
          # Fallback: parse block as expression (single statement only)
          my @block_children = $next->children();
          my $block_expr = $self->cleanup_for_parsing(\@block_children);
          my $block_id = $self->parse($block_expr);

          # Add a sub wrapper for the block
          my($sub_node, $sub_id) = $self->make_node_insert('anon_sub');
          $self->add_child_to_node($sub_id, $block_id);
          $self->add_child_to_node($top_id, $sub_id);
        }

        # For grep/map/sort: parse remaining elements as the list to process.
        # For eval/do: the block is the only argument; don't consume what follows.
        # $deref_skip: number of extra elements already consumed by -> deref chain.
        #
        # For a user (&;@)-prototype sub (Try::Tiny's try/catch/finally), the
        # slurpy @ consumes only JUXTAPOSED trailing terms; a comma immediately
        # after the block terminates the slurp and belongs to the enclosing list.
        # Perl: `try {42}, 42, "d"` → try gets ONLY the block (the 42,"d" are
        # siblings), whereas `try {} catch {}` (no comma) → catch{} is slurped.
        # grep/map/sort are true list-ops whose list starts juxtaposed and then
        # continues across commas, so this only applies to $has_block_proto subs.
        my $next_after = $e->[$i + 2 + $deref_skip];
        my $comma_stops = $has_block_proto
          && $next_after
          && $next_after->isa('PPI::Token::Operator')
          && ($next_after->content eq ',' || $next_after->content eq '=>');

        if ($func_name ne 'eval' && $func_name ne 'do' && !$comma_stops
            && $i + 2 + $deref_skip < scalar(@$e)) {
          my @rest = @$e[$i + 2 + $deref_skip .. $#$e];
          my $rest_list = $self->cleanup_for_parsing(\@rest);
          # Parse rest as comma-separated list (usually just one element)
          my $rest_ids = $self->parse_list($rest_list);
          for my $rest_id (@$rest_ids) {
            $self->add_child_to_node($top_id, $rest_id);
          }
          # Remove all processed elements and replace start with result node
          splice @$e, $i, scalar(@$e) - $i;
          $e->[$i] = $top_node;
        } else {
          # Replace eval+block (2 elements) with result node in-place
          splice @$e, $i, 2, $top_node;
        }
        next;
      }

      # Handle anonymous sub: sub { ... }
      if ($func_name eq 'sub') {
        # Use parser callback if available (handles multi-statement blocks)
        if ($self->has_parser) {
          # Anonymous subs receive call arguments via @_ (like named subs)
          my $lambda_str = $self->parser->parse_block_as_function($next, [], 1, 1);

          # Create a func_ref node that holds the lambda string inline
          my($ref_node, $ref_id) = $self->make_node_insert('func_ref');
          $ref_node->{raw_lambda} = $lambda_str;

          # Replace sub { } with the function reference (4-arg splice preserves comma)
          splice @$e, $i, 2, $ref_node;
        } else {
          # Fallback: parse block as expression (single statement only)
          my @block_children = $next->children();
          my $block_expr = $self->cleanup_for_parsing(\@block_children);
          my $block_id = $self->parse($block_expr);

          # Create anon_sub node
          my($sub_node, $sub_id) = $self->make_node_insert('anon_sub');
          $self->add_child_to_node($sub_id, $block_id);

          # Replace sub { } with the anon_sub (4-arg splice preserves comma)
          splice @$e, $i, 2, $sub_node;
        }
        next;
      }
    }

    # Handle sort NAME LIST — named comparator sub (not a block form)
    # e.g. sort compare @list  →  (p-sort (lambda ($a $b) (pl-compare)) @list)
    # The lambda params $a/$b create dynamic bindings (since defvar makes them special),
    # so named comparator subs that read $a/$b as globals see the values.
    if ($now->isa('PPI::Token::Word') && $now->content() eq 'sort'
        && $next->isa('PPI::Token::Word')) {
      my $comp_name = $next->content();
      # Only treat as comparator if NOT a known built-in (reverse, etc.)
      # and NOT a keyword (my, if, etc.)
      my $is_builtin = exists $self->known_no_of_params->{$comp_name};
      unless ($is_builtin || $comp_name =~ /^(?:CORE|my|our|local|sub|if|else|elsif|unless|while|until|for|foreach|do|return|use|package|BEGIN|END|not|and|or|eq|ne|lt|gt|le|ge|cmp|x)$/ || $comp_name =~ /^CORE::/) {
        my($top_node, $top_id) = $self->make_node_insert('funcall');
        my $sort_id = $self->make_node($now);
        $self->add_child_to_node($top_id, $sort_id);

        # Inline lambda that wraps the named comparator call
        # body_cl is a placeholder; comparator_name drives ExprToCL codegen
        my($lambda_node, $lambda_id) = $self->make_node_insert('inline_lambda');
        $lambda_node->{params}          = ['$a', '$b'];
        $lambda_node->{body_cl}         = 'nil';
        $lambda_node->{for_func}        = 'sort';
        $lambda_node->{comparator_name} = $comp_name;
        $self->add_child_to_node($top_id, $lambda_id);

        # Parse remaining elements (after sort + NAME) as the list
        if ($i + 2 < scalar(@$e)) {
          my @rest = @$e[$i + 2 .. $#$e];
          my $rest_list = $self->cleanup_for_parsing(\@rest);
          my $rest_ids  = $self->parse_list($rest_list);
          for my $rest_id (@$rest_ids) {
            $self->add_child_to_node($top_id, $rest_id);
          }
          splice @$e, $i, scalar(@$e) - $i;
          $e->[$i] = $top_node;
        } else {
          splice @$e, $i, 2, $top_node;
        }
        next;
      }
    }

    # Handle sort $scalar LIST — scalar variable as comparator (coderef, string, glob, glob ref)
    # e.g. sort $sortsub 4,1,3,2  →  (p-sort (lambda ($a $b) (funcall (p-sort-get-fn $sortsub) $a $b)) ...)
    # But a scalar immediately followed by -> is one term (method call / postfix
    # deref), NOT a bare comparator: sort $ar->@* sorts the elements of $ar, with
    # no comparator. Skip the comparator form so it falls through to list parsing.
    if ($now->isa('PPI::Token::Word') && $now->content() eq 'sort'
        && $next->isa('PPI::Token::Symbol')
        && substr($next->content(), 0, 1) eq '$'
        && !($i + 2 <= $#$e
             && $e->[$i + 2]->isa('PPI::Token::Operator')
             && $e->[$i + 2]->content() eq '->')) {
      my($top_node, $top_id) = $self->make_node_insert('funcall');
      my $sort_id = $self->make_node($now);
      $self->add_child_to_node($top_id, $sort_id);

      my($lambda_node, $lambda_id) = $self->make_node_insert('inline_lambda');
      $lambda_node->{params}     = ['$a', '$b'];
      $lambda_node->{body_cl}    = 'nil';
      $lambda_node->{for_func}   = 'sort';
      $lambda_node->{scalar_cmp} = 1;  # flag: scalar comparator
      # Parse the scalar as a child of the lambda (ExprToCL generates it)
      my @scalar_tok = ($next);
      my $scalar_clean = $self->cleanup_for_parsing(\@scalar_tok);
      my $scalar_ids   = $self->parse_list($scalar_clean);
      $self->add_child_to_node($lambda_id, $scalar_ids->[0]) if @$scalar_ids;
      $self->add_child_to_node($top_id, $lambda_id);

      if ($i + 2 < scalar(@$e)) {
        my @rest = @$e[$i + 2 .. $#$e];
        my $rest_list = $self->cleanup_for_parsing(\@rest);
        my $rest_ids  = $self->parse_list($rest_list);
        for my $rest_id (@$rest_ids) {
          $self->add_child_to_node($top_id, $rest_id);
        }
        splice @$e, $i, scalar(@$e) - $i;
        $e->[$i] = $top_node;
      } else {
        splice @$e, $i, 2, $top_node;
      }
      next;
    }

    next
        if !$self->is_list($next);

    # - - - open
    # Special handling: register bareword filehandle BEFORE parsing args
    my $func_name = $now->can('content') ? $now->content() : '';
    if ($func_name eq 'open' && $self->has_environment) {
      # Peek at first argument - if it's a bareword, register it as filehandle
      my @list_children = $next->children();
      if (@list_children) {
        my $first_child = $list_children[0];
        # Unwrap PPI::Statement::Expression if present
        if (ref($first_child) eq 'PPI::Statement::Expression') {
          my @expr_children = $first_child->children();
          $first_child = $expr_children[0] if @expr_children;
        }
        # Check if first arg is a bareword (not a variable)
        if (ref($first_child) eq 'PPI::Token::Word') {
          my $fh_name = $first_child->content();
          # Register as filehandle (skip 'my' keyword)
          if ($fh_name ne 'my') {
            $self->environment->add_filehandle($fh_name);
          }
        }
      }
    }

    # Special handling for split with regex pattern: mark regex before parsing
    if ($func_name eq 'split') {
      my @list_children = $next->children();
      for my $child (@list_children) {
        my @check = ref($child) eq 'PPI::Statement::Expression'
                  ? $child->children() : ($child);
        for my $item (@check) {
          if (ref($item) =~ /^PPI::Token::Regexp/) {
            $item->{_has_match_context} = 1;
            last;
          }
        }
      }
    }

    # Paren-form print/say/printf with a leading filehandle inside the parens:
    #   print($fh LIST)  print(STDERR LIST)  print({EXPR} LIST)
    # Extract the filehandle from the front of the list (it has no separating
    # comma) and prepend it as the funcall's first child.
    my $paren_fh_id;
    if ($func_name eq 'print' || $func_name eq 'say' || $func_name eq 'printf') {
      $paren_fh_id = $self->_extract_paren_filehandle($next);
    }

    # Replace the two items in expr with a subtree:
    my($top_node, $top_id) = $self->make_node_insert('funcall');

    my $c_ids   = $self->make_nodes_from_list($next);
    my $node_id = $self->make_node($now);

    $self->add_child_to_node($top_id, $node_id);
    if (defined $paren_fh_id) {
      $self->add_child_to_node($top_id, $paren_fh_id);
    }
    for my $c_id (@$c_ids) {
      $self->add_child_to_node($top_id, $c_id);
    }

    # Special handling for split: ensure pattern and string are always provided
    # split()        -> split(" ", $_)
    # split(/pat/)   -> split(/pat/, $_)
    if ($func_name eq 'split') {
      my $arg_count = scalar(@$c_ids);
      if ($arg_count == 0) {
        # No args: add " " pattern and $_
        my $space = PPI::Token::Quote::Double->new('" "');
        my $space_id = $self->make_node($space);
        $self->add_child_to_node($top_id, $space_id);
        my $underscore = PPI::Token::Symbol->new('$_');
        my $underscore_id = $self->make_node($underscore);
        $self->add_child_to_node($top_id, $underscore_id);
      } elsif ($arg_count == 1) {
        # One arg (pattern): add $_
        my $underscore = PPI::Token::Symbol->new('$_');
        my $underscore_id = $self->make_node($underscore);
        $self->add_child_to_node($top_id, $underscore_id);
      }
    }

    # Add implicit $_ if function defaults to it
    $self->add_implicit_default_param($func_name, $top_id);

    # So it is marked as finished.
    $e->[$i]    = $top_node;
    splice @$e, $i+1, 1;        # Remove parameters.
  }

  say "---- handle_subcalls: Before main loop. Has ", dump $e   if 8 & DEBUG;

  # - - - Look for remaining funcalls without () around parameters:
  my $last_low_prio_op;       # Store index to lower prio op than ","
  for(my $i=scalar(@$e)-1; $i >= 0; $i--) {
    my $now     = $e->[$i];

    # - - - Find lower prio op than "," -- that will end param list to fun:
    # foo a, b, c or d etc ==> foo(a,b,c) or d etc.

    # XXXXX This is different for 1 param subs that are built in ops???
    my $op_name = $self->is_token_operator($now);
    if (defined $op_name && ($op_name eq 'and'
                             || $op_name eq 'or'
                             || $op_name eq 'xor')) {
      $last_low_prio_op = $i;
      next;
    }

    # - - - Make certain it is a fun name:
    # Note: is_word() returns 1 if word, undef otherwise (NOT the word content)
    next unless $self->is_word($now);
    my $sub_name = $now->content;

    # - - - Skip if this word is a binary operator (e.g. 'isa')
    # These are recognized by is_token_operator and handled in the binary op parser.
    next if $self->is_token_operator($now);

    # - - - Skip if preceded by a word-form binary operator (e.g. 'isa')
    # e.g. '$obj isa BaseClass' — BaseClass is a class name bareword, not a function call
    if ($i > 0) {
      my $prev_elem = $e->[$i - 1];
      if (ref($prev_elem) eq 'PPI::Token::Word' && $self->is_token_operator($prev_elem)) {
        next;  # Skip - RHS of a word-form binary operator, not a function name
      }
    }

    # - - - Skip if this word is followed by -> (class method call; Foo->new)
    # The word is a class/package name, not a function call
    if ($i + 1 < scalar(@$e)) {
      my $next_elem = $e->[$i + 1];
      if ($self->is_arrow_op($next_elem)) {
        next;  # Skip - will be handled as method call in parse()
      }
    }

    # - - - Skip if this word is preceded by -> (method name like $obj->method)
    # The word is a method name, not a function call
    if ($i > 0) {
      my $prev_elem = $e->[$i - 1];
      if ($self->is_arrow_op($prev_elem)) {
        next;  # Skip - will be handled as method call in parse()
      }
    }

    # - - - Skip if this is a bareword filehandle for a function with * prototype:
    # open FH, ...; print STDERR "hello" - FH/STDERR are filehandles, not functions
    # Functions like open, close have * as first param prototype.
    # print/say/printf are handled specially (no prototype) but also take filehandles.
    if ($i > 0 && $sub_name =~ /^[A-Z][A-Z0-9_]*$/) {
      my $prev = $e->[$i - 1];
      if ($self->is_word($prev)) {
        my $prev_name = $prev->content;
        my $is_fh_func = 0;

        # print/say/printf have special handling, not prototypes
        if ($prev_name eq 'print' || $prev_name eq 'say' || $prev_name eq 'printf') {
          $is_fh_func = 1;
        }
        # Check if previous function takes * (filehandle) as first param
        elsif ($self->has_environment) {
          my $proto = $self->environment->get_prototype($prev_name);
          if ($proto && $proto->{is_proto} && @{$proto->{params}}) {
            my $first_param_type = $proto->{params}[0]{proto_type} // '';
            $is_fh_func = 1 if $first_param_type eq '*';
          }
        }

        next if $is_fh_func;  # Skip - will be handled when processing the function
      }
    }

    # - - - Check if this is a known filehandle:
    if ($self->has_environment && $self->environment->is_filehandle($sub_name)) {
      # Leave as bareword - don't treat as funcall
      # It will be emitted as-is by ExprToCL
      next;
    }

    # - - - Does it have zero parameters:
    # Simple case, e.g. time(), wantarray().

    my $no_pars = $self->no_params_of_sub($sub_name);

    # Check if function takes 0 params, or can default to $_ or @_
    # and is followed by an operator (meaning no explicit args given)
    my $is_zero_param = 0;
    if (defined $no_pars) {
      if ($no_pars == 0 || $no_pars == -2 || $no_pars == -3) {
        $is_zero_param = 1;
      } elsif (ref($no_pars) eq 'ARRAY') {
        # Array spec like [1, -3] - check if it can take 0 params
        # -2 = default $_, -3 = default @_, 0 = explicit 0 params
        for my $spec (@$no_pars) {
          if ($spec == 0 || $spec == -2 || $spec == -3) {
            $is_zero_param = 1;
            last;
          }
        }
      }
    }

    # If can be zero-param, check if next token is an operator
    # (but NOT a Cast token like @, $, %, etc. which are deref operators for arguments)
    # Also NOT if the operator can be a unary prefix (like ~, !, +, -, not, \)
    # because then it's likely the start of an argument expression, not a binary op.
    # e.g., `length ~0` → length(~0), not length() followed by ~0
    if ($is_zero_param && $i + 1 < scalar(@$e)) {
      my $next = $e->[$i + 1];
      my $next_op = $self->is_token_operator($next);
      my %can_be_prefix = map { $_ => 1 } ('+', '-', '!', '~', '\\', 'not');
      if ($next_op && ref($next) ne 'PPI::Token::Cast'
          && !$can_be_prefix{$next_op}) {
        # Function followed by binary-only operator - treat as zero params
        my($top_node, $top_id) = $self->make_node_insert('funcall');
        my $node_id = $self->make_node($now);
        $self->add_child_to_node($top_id, $node_id);
        $self->add_implicit_default_param($sub_name, $top_id);
        $e->[$i] = $self->make_subtree_item($top_id, 'funcall');
        next;
      }
    }

    if (defined $no_pars && $no_pars == 0) {
      my($top_node, $top_id) = $self->make_node_insert('funcall');
      my $node_id = $self->make_node($now);
      $self->add_child_to_node($top_id, $node_id);
      $e->[$i] = $self->make_subtree_item($top_id, 'funcall');
      next;
    }

    # - - - If bareword is followed by binary-only operator, treat as zero-arg:
    # e.g., PI/2 should be PI() / 2, not PI(/2) which fails
    # Operators that CAN be unary prefix: + - ! ~ \ not
    # All other operators are binary-only and should terminate parameter list
    if ($i + 1 < scalar(@$e)) {
      my $next = $e->[$i + 1];
      my $next_op = $self->is_token_operator($next);
      if (defined $next_op) {
        # Check if this is a binary-only operator (cannot be unary prefix)
        # Cast tokens (@, $, %, &, *) are always unary deref operators
        my $is_cast = ref($next) eq 'PPI::Token::Cast';
        # Operators that can be unary prefix: + - ! ~ ~. \ not
        my %can_be_unary_op = map { $_ => 1 } ('+', '-', '!', '~', '~.', '\\', 'not', '++', '--');
        my $is_unary = $is_cast || $can_be_unary_op{$next_op};
        if (!$is_unary) {
          # Binary-only operator - treat bareword as zero-arg function.
          # BUT: if the word is not a known function (not in known_no_of_params,
          # not declared in Environment), it's an unknown bareword string literal.
          # e.g., !Bare || $x — Bare is the string "Bare", not a function call.
          # Unknown barewords before binary operators are strings in no-strict Perl;
          # in strict Perl they'd be a compile error (so CPAN modules never have them).
          my $is_known_bop = exists $self->known_no_of_params->{$sub_name}
              || ($self->has_environment
                  && $self->environment->has_prototype($sub_name));
          # ALL-CAPS words (DIR, FILE, STDIN, MAXSIZE, etc.) are filehandles or
          # constants — leave them as funcalls so %p-fh-arg can identify them.
          # Only mixed-case unknown words (like Bare in !Bare) are string literals.
          my $is_all_caps_bop = ($sub_name =~ /^[A-Z][A-Z0-9_]*$/);
          unless ($is_known_bop || $is_all_caps_bop) {
            $now->{_bareword_string} = 1;
            next;
          }
          my($top_node, $top_id) = $self->make_node_insert('funcall');
          my $node_id = $self->make_node($now);
          $self->add_child_to_node($top_id, $node_id);
          $e->[$i] = $self->make_subtree_item($top_id, 'funcall');
          next;
        }
      }
    }

    # - - - Parse parameters of fun:
    my $end_pars= scalar(@$e)-1;
    $end_pars   = $last_low_prio_op-1
        if defined $last_low_prio_op;

    # A ternary ':' that closes an ENCLOSING ternary terminates this list
    # operator's argument list: `cond ? join "-", @a : $fb` must parse as
    # `cond ? (join "-", @a) : $fb`, not let join swallow `: $fb` (which then
    # orphans the colon and the whole expression falls through).  Walk the arg
    # region tracking ternary depth so a NESTED ternary's own ':' (whose '?' is
    # inside the args, e.g. `join "-", $c ? @a : @b`) is NOT treated as a
    # boundary and stays part of the args.
    {
      my $tern_depth = 0;
      for (my $j = $i + 1; $j <= $end_pars; $j++) {
        my $jop = $self->is_token_operator($e->[$j]) // '';
        if ($jop eq '?') {
          $tern_depth++;
        } elsif ($jop eq ':') {
          if ($tern_depth == 0) { $end_pars = $j - 1; last; }
          $tern_depth--;
        }
      }
    }

    # Named unary operators only take the next single term
    # But Cast + Symbol (like @$list) counts as one term
    # And Symbol + Subscript (like $h{key} or $a[0]) counts as one term
    my $func_name_for_unary = $now->content();
    if ($self->is_named_unary($func_name_for_unary) && $end_pars > $i + 1) {
        my $next_term = $e->[$i + 1];
        if (ref($next_term) eq 'PPI::Token::Cast' && $end_pars >= $i + 2) {
            # Cast followed by Symbol is a single dereference term
            $end_pars = $i + 2;
            # Also include a trailing Subscript: @$h{keys}, $$h{key}, @$a[idx]
            # so delete/exists/keys on ref-slices see the full lvalue
            if ($end_pars + 1 <= scalar(@$e) - 1
                && ref($e->[$end_pars + 1]) eq 'PPI::Structure::Subscript') {
                $end_pars++;
                $end_pars = $self->_extend_postfix_chain($e, $end_pars);
            }
        } elsif ((ref($next_term) eq 'PPI::Token::Symbol'
                  || ref($next_term) eq 'PPI::Token::Magic') && $end_pars >= $i + 2) {
            # Check if symbol is followed by subscript (hash/array access)
            my $after_symbol = $e->[$i + 2];
            if (ref($after_symbol) eq 'PPI::Structure::Subscript') {
                # Symbol + Subscript chain: consume all chained subscripts and
                # arrow-subscript chains (e.g., $h{a}{b}[c] or $h{a}->{b}->[c])
                $end_pars = $i + 2;
                $end_pars = $self->_extend_postfix_chain($e, $end_pars);
            } elsif (ref($after_symbol) eq 'PPI::Structure::Block'
                     && $after_symbol->start() eq '{'
                     && $next_term->content() =~ /^%/) {
                # %hash + Block is one term (KV slice: %h{keys})
                $end_pars = $i + 2;
            } elsif (ref($after_symbol) eq 'PPI::Structure::Constructor'
                     && $after_symbol->start() eq '['
                     && $next_term->content() =~ /^%/) {
                # %arr + Constructor is one term (KV array slice: %arr[indices])
                $end_pars = $i + 2;
            } elsif (ref($after_symbol) eq 'PPI::Token::Operator'
                     && $after_symbol->content() eq '->'
                     && $end_pars >= $i + 3) {
                # $r->{key} or $r->[idx]: consume full arrow-subscript chain
                # so exists/delete/defined can see the whole lvalue
                $end_pars = $i + 3;  # symbol + -> + subscript/block
                $end_pars = $self->_extend_postfix_chain($e, $end_pars);
            } else {
                $end_pars = $i + 1;
            }
        } elsif (ref($next_term) =~ /^PPI::Structure::/
                 && $end_pars >= $i + 3
                 && ref($e->[$i + 2]) eq 'PPI::Token::Operator'
                 && $e->[$i + 2]->content() eq '->'
                 && ref($e->[$i + 3]) =~ /^PPI::Structure::/) {
            # Block/Constructor + -> + Subscript: e.g. exists { hash }->{key}
            # Consume full arrow-subscript chain as the named-unary argument
            $end_pars = $i + 3;
            $end_pars = $self->_extend_postfix_chain($e, $end_pars);
        } elsif (ref($next_term) eq 'PPI::Token::Operator'
                 && grep { $next_term->content() eq $_ } ('~', '!')) {
            # Unary prefix operator (~, !) — include operator and its operand as the argument
            if ($end_pars >= $i + 2) {
                $end_pars = $i + 2;
                # Handle chained prefix operators: ~~0, !!$x, etc.
                while ($end_pars < scalar(@$e) - 1) {
                    my $nx = $e->[$end_pars];
                    last unless ref($nx) eq 'PPI::Token::Operator'
                             && grep { $nx->content() eq $_ } ('~', '!');
                    $end_pars++;
                }
            } else {
                $end_pars = $i + 1;
            }
        } else {
            # Named unary with a literal/word/subtree first arg (not Cast, Symbol/Magic,
            # or Structure+arrow). Consume through high-prec binary ops (prec >= 55:
            # . + - * / % x ** =~ !~ << >>), stop before comparison/logical/assignment.
            # E.g.: eval 'a' . $x . 'b' → eval('a' . $x . 'b'), not (eval 'a') . $x . 'b'
            my $j = $i + 1;
            while ($j + 1 < scalar(@$e)) {
                my $nxt = $e->[$j + 1];
                if (ref($nxt) eq 'PPI::Token::Operator') {
                    my $op_str = $nxt->content();
                    unless ($op_str eq '->') {
                        my $op_info = $self->config->precedences->{$op_str};
                        last unless defined $op_info && $op_info->{prec} >= 55;
                    }
                }
                $j++;
            }
            $end_pars = $j;
        }

        # Named unary operators bind LOOSER than the high-precedence binary ops
        # (. + - * / % x ** =~ !~ << >>, all prec >= 55) but TIGHTER than
        # comparison/logical/assignment. Whichever branch above set $end_pars to
        # the end of the first operand term (symbol, cast, subscript chain, or
        # literal), keep consuming through any prec>=55 binary operator and its
        # right operand, stopping before comparison/comma/etc. So `length $s + 1`
        # => length($s + 1) and `uc $x . "y"` => uc($x . "y"), matching Perl's
        # named-unary precedence. (Idempotent for the literal branch above, which
        # already extended; this fixes the symbol/cast/subscript branches, which
        # previously stopped at the first term.)
        {
            my $j = $end_pars;
            while ($j + 1 < scalar(@$e)) {
                my $nxt = $e->[$j + 1];
                if (ref($nxt) eq 'PPI::Token::Operator') {
                    my $op_str = $nxt->content();
                    unless ($op_str eq '->') {
                        my $op_info = $self->config->precedences->{$op_str};
                        last unless defined $op_info && $op_info->{prec} >= 55;
                    }
                }
                $j++;
            }
            $end_pars = $j;
        }
    }

    # Functions taking EXACTLY 1 param need Cast+Symbol handling (e.g., shift @$arr)
    # Check if this is a strictly 1-param function with Cast+Symbol as argument
    # NOTE: Don't apply this to functions with variable params like bless([1,2])
    #       as they may take more arguments after the Cast+Symbol
    if (defined $no_pars && $end_pars > $i + 1) {
      # Only limit to single term if function takes EXACTLY 1 param (max is 1)
      my $is_strictly_single = 0;
      if ($no_pars == 1) {
        $is_strictly_single = 1;
      } elsif (ref($no_pars) eq 'ARRAY') {
        # For array specs, only if max is 1 (all values are 1 or less)
        # Skip negative values (defaults like -2, -3) when finding max
        my @positive = grep { $_ > 0 } @$no_pars;
        my $max = @positive ? (sort { $b <=> $a } @positive)[0] : 0;
        $is_strictly_single = ($max == 1);
      }
      if ($is_strictly_single && !$self->is_named_unary($func_name_for_unary)) {
        # Only apply for non-named-unary 1-param functions
        # Named unary already handled above with proper term detection
        my $next_term = $e->[$i + 1];
        if (ref($next_term) eq 'PPI::Token::Cast' && $end_pars >= $i + 2) {
          # Cast followed by Symbol is a single dereference term
          $end_pars = $i + 2;
        } elsif (ref($next_term) eq 'PPI::Token::Symbol'
                 || ref($next_term) eq 'PPI::Token::Magic') {
          # Symbol or Magic (%hash, @arr, $var, $_, @_, …)
          if ($i + 2 <= $end_pars) {
            my $after = $e->[$i + 2];
            if (ref($after) eq 'PPI::Structure::Block'
                && $after->start() eq '{'
                && $next_term->content() =~ /^%/) {
              # %hash + Block is one term (KV slice: %h{keys}) — not a postfix chain
              $end_pars = $i + 2;
            } else {
              # Subscript chain, -> subscript, -> deref (@*/%*), -> slice
              # (@[..]/%{..}), or just the bare symbol — all bounded by the walk.
              # (Subsumes the old keys $hr->%* / values $ar->@* special cases.)
              $end_pars = $self->_extend_postfix_chain($e, $i + 1);
            }
          } else {
            $end_pars = $i + 1;
          }
        } elsif ($self->is_internal_node_type($next_term)) {
          # Already-parsed node (e.g., from previous handle_subcalls)
          $end_pars = $i + 1;
        }
      }
    }

    # - - - Limit args for user-sub old-style prototypes (e.g., sub foo($)):
    # If the function has a fixed-count prototype with no @ or % param, limit
    # $end_pars so that only that many arguments are consumed, leaving the rest
    # for the surrounding expression.
    # e.g., `is _and 0, '0', 'str'` with _and($) -> `is(_and(0), '0', 'str')`
    # _proto_max_args returns undef for built-in prototypes (no min_params set),
    # so this only fires for user-defined subs with old-style prototypes.
    if ($self->has_environment) {
      my $proto = $self->environment->get_prototype($sub_name);
      my $max_args = $self->_proto_max_args($proto);
      if (defined $max_args) {
        if ($max_args == 0) {
          $end_pars = $i;
        } else {
          my $comma_count = 0;
          for my $j ($i + 1 .. $end_pars) {
            my $tok = $e->[$j];
            if (ref($tok) eq 'PPI::Token::Operator' && $tok->content() eq ',') {
              $comma_count++;
              if ($comma_count == $max_args) {
                $end_pars = $j - 1;
                last;
              }
            }
          }
        }
      }
    }

    # - - - Special handling for print/say with filehandle:
    # print FILEHANDLE LIST  (no comma between filehandle and list)
    # print $fh LIST         (variable filehandle)
    my $filehandle_id;
    if (($sub_name eq 'print' || $sub_name eq 'say' || $sub_name eq 'printf') && $i + 1 <= $end_pars) {
      my $maybe_fh = $e->[$i + 1];
      my $is_fh = 0;

      # Track filehandle expression end position (for multi-token expressions)
      my $fh_end = $i + 1;  # Start at first token after print/say

      # Check for uppercase bareword (STDERR, STDOUT, FH, etc.)
      if ($self->is_word($maybe_fh)) {
        my $fh_name = $maybe_fh->content;
        if ($fh_name =~ /^[A-Z][A-Z0-9_]*$/) {
          # Not a filehandle if followed by -> (class method call: Foo->bar())
          my $after_fh = $e->[$fh_end + 1];
          $is_fh = 1 unless $after_fh && $self->is_arrow_op($after_fh);
        }
      }
      # Check for block filehandle syntax: print {$expr} LIST
      elsif (ref($maybe_fh) eq 'PPI::Structure::Block') {
        $is_fh = 1;
        # Block is always a filehandle - contents will be parsed below
      }
      # Check for variable filehandle: print $scalar TERM
      # Only SIMPLE scalars can be filehandles (not $hash{key}, $arr[0])
      # Complex expressions need block form: print {$expr} LIST
      elsif (ref($maybe_fh) eq 'PPI::Token::Symbol'
             && $maybe_fh->content =~ /^\$/) {
        if ($fh_end + 1 <= $end_pars) {
          my $after = $e->[$fh_end + 1];
          $is_fh = $self->_is_print_term_start($after);
        }
        # Nothing follows → it's an argument, not a filehandle
      }

      if ($is_fh) {
        my($fh_node, $fh_id) = $self->make_node_insert('filehandle');

        # Handle block syntax: parse block contents
        if (ref($maybe_fh) eq 'PPI::Structure::Block') {
          my @block_children = $maybe_fh->children();
          # Filter to just the expression (skip whitespace)
          @block_children = grep { ref($_) !~ /Whitespace/ } @block_children;
          # Unwrap PPI::Statement if present
          if (@block_children == 1 && ref($block_children[0]) eq 'PPI::Statement') {
            my @stmt_children = $block_children[0]->children();
            @stmt_children = grep { ref($_) !~ /Whitespace/ } @stmt_children;
            @block_children = @stmt_children if @stmt_children;
          }
          # Check if single bareword is a known filehandle
          if (@block_children == 1 && $self->is_word($block_children[0])) {
            my $name = $block_children[0]->content;
            if ($self->has_environment && $self->environment->is_filehandle($name)) {
              # Known filehandle - treat as bareword (don't parse as funcall)
              my $fh_name_id = $self->make_node($block_children[0]);
              $self->add_child_to_node($fh_id, $fh_name_id);
            } else {
              # Not a known filehandle - parse it (might be a sub call)
              my $fh_expr_id = $self->parse(\@block_children);
              $self->add_child_to_node($fh_id, $fh_expr_id);
            }
          } elsif (@block_children) {
            # Complex expression - parse it
            my $fh_expr_id = $self->parse(\@block_children);
            $self->add_child_to_node($fh_id, $fh_expr_id);
          }
        }
        # Handle simple variable or bareword: just make node from token
        else {
          my $fh_name_id = $self->make_node($maybe_fh);
          $self->add_child_to_node($fh_id, $fh_name_id);
        }

        $filehandle_id = $fh_id;
        # Remove the filehandle token from expression list
        splice @$e, $i + 1, 1;
        $end_pars -= 1;
      }
    }

    # - - - Special handling for split with regex pattern:
    # split /pattern/, LIST - the regex should not be wrapped with $_ =~
    if ($sub_name eq 'split' && $i + 1 <= $end_pars) {
      my $maybe_regex = $e->[$i + 1];
      # Direct regex: split /pattern/
      if (ref($maybe_regex) =~ /^PPI::Token::Regexp/) {
        $maybe_regex->{_has_match_context} = 1;
      }
      # Regex in parentheses: split(/pattern/)
      elsif (ref($maybe_regex) eq 'PPI::Structure::List') {
        # Look inside the list for the regex
        my @list_children = $maybe_regex->children();
        for my $child (@list_children) {
          # May be wrapped in PPI::Statement::Expression
          my @check = ref($child) eq 'PPI::Statement::Expression'
                    ? $child->children() : ($child);
          for my $item (@check) {
            if (ref($item) =~ /^PPI::Token::Regexp/) {
              $item->{_has_match_context} = 1;
              last;
            }
          }
        }
      }
    }

    # If no parameters would be consumed and the word is not a known function,
    # treat it as a bareword string literal instead of a zero-arg function call.
    # This handles e.g. parse([!, Bare]) where Bare is the operand of !, not a func,
    # and also the RHS of binary ops: "a .. c" — c after .. is also a bareword.
    # Decision: use strict_subs pragma to gate. Without strict, unknown standalone
    # words in operator context are strings. With strict, leave as funcall (may
    # fail at runtime, which is correct Perl behavior for typo'd sub names).
    if ($end_pars < $i + 1) {
      my $is_known_fb = exists $self->known_no_of_params->{$sub_name}
          || ($self->has_environment
              && $self->environment->has_prototype($sub_name));
      # ALL-CAPS words are filehandles/constants — leave as funcalls.
      my $is_all_caps_fb = ($sub_name =~ /^[A-Z][A-Z0-9_]*$/);
      unless ($is_known_fb || $is_all_caps_fb) {
        my $prev_is_unary = 0;
        my $prev_is_binary = 0;
        if ($i > 0) {
          my $prev_tok = $e->[$i - 1];
          my $prev_op  = $self->is_token_operator($prev_tok);
          if (defined $prev_op) {
            my %unary_ops = map { $_ => 1 } ('+', '-', '!', '~', '\\', 'not');
            if ($unary_ops{$prev_op}) {
              $prev_is_unary = 1;
            } elsif ($prev_op ne ',' && $prev_op ne '=>') {
              # Commas and fat-commas are argument/list separators, not value-combining
              # binary operators. A bareword after ',' could be a class name (bless \$x, Foo::)
              # or a sub call — don't force it to a string based on separator position.
              # For actual binary operators (.. + - eq etc.), treat RHS bareword as string.
              $prev_is_binary = 1;
            }
          }
        }
        # In strict-subs mode: only unary context and already-flagged words → string.
        # In no-strict mode: any value-operator context (unary OR binary, not separators) → string.
        my $strict_subs = $self->has_environment
                          && $self->environment->has_pragma('strict_subs');
        my $is_op_context = $strict_subs
            ? ($prev_is_unary || $now->{_bareword_string})
            : ($prev_is_unary || $prev_is_binary || $now->{_bareword_string});
        if ($is_op_context) {
          $now->{_bareword_string} = 1;
          next;
        }
      }
    }

    # Everything to the right of the Expr seems to be parameter(s).
    my($top_node, $top_id) = $self->make_node_insert('funcall');
    my $c_ids   = $self->parse_list($e, $i+1, $end_pars);
    my $node_id = $self->make_node($e->[$i]);

    # - - - Post-process for * (filehandle) prototype:
    # If first arg is a zero-param funcall of uppercase bareword, it's a filehandle
    if (@$c_ids && $self->has_environment) {
      my $proto = $self->environment->get_prototype($sub_name);
      if ($proto && $proto->{is_proto} && @{$proto->{params}}) {
        my $first_param_type = $proto->{params}[0]{proto_type} // '';
        if ($first_param_type eq '*') {
          my $first_arg_id = $c_ids->[0];
          my $first_arg = $self->get_a_node($first_arg_id);
          # Check if it's a funcall (zero-param bareword becomes funcall)
          if ($self->is_internal_node_type($first_arg) && $first_arg->{type} eq 'funcall') {
            my $arg_kids = $self->get_node_children($first_arg_id);
            # Zero-param funcall has exactly 1 child (the function name)
            if (@$arg_kids == 1) {
              my $name_node = $self->get_a_node($arg_kids->[0]);
              if (ref($name_node) eq 'PPI::Token::Word') {
                my $name = $name_node->content;
                if ($name =~ /^[A-Z][A-Z0-9_]*$/) {
                  # It's a bareword filehandle - replace funcall with just the word node
                  $c_ids->[0] = $arg_kids->[0];
                  # Register as filehandle
                  $self->environment->add_filehandle($name);
                }
              }
            }
          }
        }
      }
    }

    $self->add_child_to_node($top_id, $node_id);
    # Add filehandle as first parameter if present
    # Note: use 'defined' because ID 0 is valid but falsy
    if (defined $filehandle_id) {
      $self->add_child_to_node($top_id, $filehandle_id);
    }
    for my $c_id (@$c_ids) {
      $self->add_child_to_node($top_id, $c_id);
    }

    # Special handling for split: ensure pattern and string are always provided
    if ($sub_name eq 'split') {
      my $arg_count = scalar(@$c_ids);
      if ($arg_count == 0) {
        # No args: add " " pattern and $_
        my $space = PPI::Token::Quote::Double->new('" "');
        my $space_id = $self->make_node($space);
        $self->add_child_to_node($top_id, $space_id);
        my $underscore = PPI::Token::Symbol->new('$_');
        my $underscore_id = $self->make_node($underscore);
        $self->add_child_to_node($top_id, $underscore_id);
      } elsif ($arg_count == 1) {
        # One arg (pattern): add $_
        my $underscore = PPI::Token::Symbol->new('$_');
        my $underscore_id = $self->make_node($underscore);
        $self->add_child_to_node($top_id, $underscore_id);
      }
    }

    # Add implicit $_ if function defaults to it
    $self->add_implicit_default_param($sub_name, $top_id);

    $e->[$i]    = $top_node; # $self->make_subtree_item($node_id, 'funcall');

    splice @$e, $i+1, $end_pars-$i; # Should be correct, right? :-)
  }

  say "handle_subcalls: End"        if 8 & DEBUG;
}


# Add implicit $_ or @_ parameter to functions that default to it
# Call this after creating a funcall node
sub add_implicit_default_param {
  my $self      = shift;
  my $func_name = shift;
  my $node_id   = shift;

  return unless defined $func_name;

  # CORE::foo explicitly names the builtin (bypassing any override), so it must
  # inherit the builtin's param spec — e.g. CORE::shift()/CORE::pop() default to
  # @_ just like shift/pop.  Strip the prefix for the spec lookup.
  $func_name =~ s/^CORE:://;

  my $param_spec = $self->known_no_of_params->{$func_name};
  return unless defined $param_spec;

  # Check if function uses $_ as default (-2) or @_ as default (-3)
  my @specs = ref($param_spec) eq 'ARRAY' ? @$param_spec : ($param_spec);
  my $has_scalar_default = grep { $_ == -2 } @specs;
  my $has_array_default  = grep { $_ == -3 } @specs;

  return unless $has_scalar_default || $has_array_default;

  # Check how many parameters the funcall currently has
  my $children = $self->get_node_children($node_id);
  # First child is the function name, rest are parameters
  my $param_count = scalar(@$children) - 1;

  # If no parameters provided, add implicit $_ or @_/@ARGV
  if ($param_count == 0) {
    my $default_var;
    if ($has_array_default) {
      # @_ in subs, @ARGV in main
      my $in_sub = $self->has_environment && $self->environment->in_subroutine > 0;
      if ($in_sub) {
        $default_var = '@_';
        say "add_implicit_default_param: Adding \@_ to $func_name (in sub)" if 8 & DEBUG;
      } else {
        $default_var = '@ARGV';
        say "add_implicit_default_param: Adding \@ARGV to $func_name (at top level)" if 8 & DEBUG;
      }
    } else {
      $default_var = '$_';
      say "add_implicit_default_param: Adding \$_ to $func_name" if 8 & DEBUG;
    }

    # Create default variable token and node
    my $var_token = PPI::Token::Symbol->new($default_var);
    my $var_id = $self->make_node($var_token);
    $self->add_child_to_node($node_id, $var_id);
  }
}

# Return the maximum fixed number of arguments for a user-defined old-style
# prototype, or undef if:
#  - no prototype / not is_proto
#  - no min_params key (built-in prototypes from _builtin_prototypes lack this)
#  - prototype has @ or % or * param (unbounded / filehandle)
sub _proto_max_args {
  my ($self, $proto) = @_;
  return undef unless $proto && $proto->{is_proto};
  return undef unless defined $proto->{min_params};  # built-ins have no min_params
  my $params = $proto->{params} // [];
  for my $p (@$params) {
    my $pt = $p->{proto_type} // '';
    return undef if $pt eq '@' || $pt eq '%' || $pt eq '*';
  }
  return scalar(@$params);
}


# Find the matching : for a ? at given position
# Handles nested ternaries by counting ? and : depth
sub find_matching_colon {
  my $self      = shift;
  my $e         = shift;
  my $start_pos = shift;

  my $depth     = 1;  # We're looking for the : that matches our ?

  for(my $i = $start_pos; $i < scalar(@$e); $i++) {
    next unless $self->is_token_operator($e->[$i]);
    my $op = $e->[$i]->content;

    if ($op eq '?') {
      $depth++;
    } elsif ($op eq ':') {
      $depth--;
      return $i if $depth == 0;
    }
  }

  return undef;  # No matching : found
}


# Should handle Perl std ones and prototype declared ones.

# Note that if declares parameters like a '&', expects code block or
# ref to a sub. So not the start of a hash. XXXXX This must change how
# parameters are handled above!
sub no_params_of_sub {
  my $self = shift;
  my $name = shift;

  # First check environment for declared subs
  if ($self->has_environment) {
    my $min = $self->environment->get_min_params($name);
    return $min if defined $min;
  }

  # Fall back to built-in function table
  return $self->known_no_of_params->{$name} // -1;
}



sub op_info {
  my $self      = shift;
  my $op        = shift;

  my $name      = $self->is_token_operator($op) // '';

  # Cast tokens (deref: $$ref, @$ref, %$ref, &$ref, *$ref) are always
  # unary prefix operators with high precedence. Handle them specially
  # to avoid conflict with binary * (multiplication) in precedences hash.
  if (ref($op) eq 'PPI::Token::Cast') {
    return { assoc => 'r', no => 1, prec => 90 };
  }

  my $operands  = $self->precedences();

  return $operands->{lc $name};
}

sub is_op_prefix {
  my $self      = shift;
  my $op        = shift;

  my $name      = $self->is_token_operator($op) // '';
  my $prefix    = $self->prefix();

  return $prefix->{lc $name};
}

# After "print $var TOKEN", determine if TOKEN starts a new term
# (making $var a filehandle) or is an operator (making $var an argument).
sub _is_print_term_start {
  my ($self, $token) = @_;
  my $ref = ref($token);

  # Binary operators → $var is part of an expression, NOT a filehandle
  # Exception: ! and ~ are unary-only and always start a new term
  if ($ref eq 'PPI::Token::Operator') {
    my $op = $token->content;
    return 1 if $op eq '!' || $op eq '~' || $op eq 'not';
    return 0;  # All others: , . + - * / == && || etc.
  }

  # Subscript {key}/[idx] means it's $var{key} or $var[idx], NOT a filehandle
  return 0 if $ref eq 'PPI::Structure::Subscript';

  # Everything else IS a term start:
  #   Symbol ($x, @arr), Magic ($_), Quote ("str"), Number (42),
  #   Cast (\, @{), Word (func), Regexp (/pat/), HereDoc (<<EOF),
  #   QuoteLike (qw()), Structure::List ((expr)), Constructor ([]),
  #   and already-parsed internal nodes
  return 1;
}


# Detect and extract a filehandle from the front of a paren-form print:
#   print(FH LIST)   print($fh LIST)   print({EXPR} LIST)
# The filehandle is the first significant token inside the parens, separated
# from the first real argument by whitespace (no comma). On success this prunes
# the filehandle token from the PPI list (so the remaining tokens parse as the
# args) and returns the new filehandle node id; otherwise returns undef and
# leaves the list untouched (it's an ordinary parenthesised argument list).
sub _extract_paren_filehandle {
  my ($self, $list) = @_;

  # Find the inner expression node that actually holds the tokens.
  my ($expr) = grep { ref($_) =~ /^PPI::Statement/ } $list->children;
  $expr ||= $list;
  my @kids = grep { ref($_) !~ /Whitespace/ } $expr->schildren;
  return undef unless @kids >= 2;

  my $first  = $kids[0];
  my $second = $kids[1];

  # The first token must look like a filehandle, and the second must start a
  # new term (no separating comma → not a normal argument list).
  my $is_fh = 0;
  if ($self->is_word($first) && $first->content =~ /^[A-Z][A-Z0-9_]*$/) {
    $is_fh = 1;            # bareword: print(STDERR ...)
  }
  elsif (ref($first) eq 'PPI::Token::Symbol' && $first->content =~ /^\$/) {
    $is_fh = 1;            # scalar: print($fh ...)
  }
  elsif (ref($first) eq 'PPI::Structure::Block') {
    $is_fh = 1;            # block: print({EXPR} ...)
  }
  return undef unless $is_fh && $self->_is_print_term_start($second);

  # Build the filehandle node.
  my ($fh_node, $fh_id) = $self->make_node_insert('filehandle');
  if (ref($first) eq 'PPI::Structure::Block') {
    my @bk = grep { ref($_) !~ /Whitespace/ } $first->schildren;
    if (@bk == 1 && ref($bk[0]) =~ /^PPI::Statement/) {
      @bk = grep { ref($_) !~ /Whitespace/ } $bk[0]->schildren;
    }
    my $inner = $self->parse([@bk]);
    $self->add_child_to_node($fh_id, $inner);
  }
  else {
    my $name_id = $self->make_node($first);
    $self->add_child_to_node($fh_id, $name_id);
  }

  # Prune the filehandle token from the PPI list so the rest parses as args.
  $first->remove;
  return $fh_id;
}


# ----------------------------------------------------------------------
# Context handling

# Annotate the tree with contexts after parsing is complete.
# Call this with the root node ID after parse_expr_to_tree().
#
# Usage: $expr_o->annotate_contexts($root_id, SCALAR_CTX);
sub annotate_contexts {
  my $self          = shift;
  my $node_id       = shift;
  my $context       = shift // SCALAR_CTX;

  # Use iterative approach with explicit stack to avoid deep recursion
  # warnings on long expression chains (e.g., many concatenations)
  my @stack = ([$node_id, $context]);

  while (@stack) {
    my ($current_id, $current_ctx) = @{pop @stack};

    say "annotate_contexts: node $current_id, context ",
        $self->context_name($current_ctx)
        if 16 & DEBUG;

    # Store context on this node
    $self->set_node_context($current_id, $current_ctx);

    my $node     = $self->get_a_node($current_id);
    my $children = $self->get_node_children($current_id);

    # Push children onto stack in reverse order (so first child is processed first)
    for my $i (reverse 0 .. $#{$children}) {
      my $child_id  = $children->[$i];
      my $child_ctx = $self->child_context($node, $current_id, $i, $current_ctx);
      push @stack, [$child_id, $child_ctx];
    }
  }
}


# Determine what context a child should be evaluated in.
# This is where we encode the rules about context propagation.
sub child_context {
  my $self          = shift;
  my $parent_node   = shift;
  my $parent_id     = shift;
  my $child_index   = shift;
  my $parent_ctx    = shift;

  # - - - Internal node (has type field)
  if ($self->is_internal_node_type($parent_node)) {
    my $type        = $parent_node->{type};

    # Assignment: RHS context depends on LHS, LHS ctxt depends on lvalue type
    if ($type eq '=') {
      my $children  = $self->get_node_children($parent_id);
      my $lhs_id    = $children->[0];
      my $lhs       = $self->get_a_node($lhs_id);
      
      if ($child_index == 0) {
        # LHS: context based on what kind of lvalue it is
        return $self->lvalue_context($lhs);
      } elsif ($child_index == 1) {
        # RHS: context based on what LHS expects
        return $self->assignment_rhs_context($lhs, $lhs_id);
      }
    }

    # Function calls: check if function imposes context
    if ($type eq 'funcall') {
      my $children  = $self->get_node_children($parent_id);
      my $func_node = $self->get_a_node($children->[0]);
      my $func_name = $func_node->content() if $func_node->can('content');

      # List operators force list context on their list argument
      if ($func_name && $func_name =~ /^(map|grep|sort|keys|values|each)$/) {
        # child 0 = function name, child 1 = comparator/block (for sort/grep/map),
        # child 2+ = list to process.
        # For sort without a comparator (e.g. sort LIST, sort &f()), child 1 IS
        # the list — detect this by checking if child 1 is an inline_lambda.
        if ($func_name eq 'sort' && $child_index == 1 && @$children >= 2) {
          my $c1_node = $self->get_a_node($children->[1]);
          # inline_lambda means there IS a comparator — child 1 is NOT the list
          return LIST_CTX
              unless $self->is_internal_node_type($c1_node)
                  && $c1_node->{type} eq 'inline_lambda';
        }

        # Standard: second parameter (index 2 in children) is the list
        return LIST_CTX
            if $child_index == 2;
      }

      # join forces list context on all arguments after separator
      if ($func_name && $func_name eq 'join') {
        return LIST_CTX
            if $child_index >= 2;  # All arguments after function name and separator
      }

      # Functions that always take lists
      if ($func_name && $func_name =~ /^(push|unshift|splice|reverse)$/) {
        return LIST_CTX
            if $child_index >= 2;  # List argument(s)
      }

      # chop/chomp operate on a LIST of lvalues (chop @array, chop @h{@keys},
      # chop($a,$b)) and must evaluate their argument(s) in list context — even
      # when the call itself sits in scalar context (e.g. is(chop(@slice), 't'),
      # where Test::More's $-proto forces the result scalar).  Without this the
      # slice collapses via p-list-scalar and chop sees a single string.
      # NB: this lives here (a context-only hint) rather than as a (@) entry in
      # _builtin_prototypes because the prototype table is also read by codegen
      # paths, and giving chomp a prototype there changes how `chomp @a`
      # compiles (breaks chop.t).
      if ($func_name && $func_name =~ /^(chop|chomp)$/) {
        return LIST_CTX
            if $child_index >= 1;
      }

      # print/say force list context on all arguments
      if ($func_name && $func_name =~ /^(print|say)$/) {
        return LIST_CTX
            if $child_index > 0;  # All arguments after function name
      }

      # scalar forces scalar context on its argument
      if ($func_name && $func_name eq 'scalar') {
        return SCALAR_CTX
            if $child_index >= 1;  # Argument is scalar context
      }

      # Scalar-argument named-unary operators impose SCALAR context on their
      # argument even when the operator itself is in list context — e.g.
      # `print ucfirst(reverse $s)` must reverse the STRING, not the list, and
      # `push @a, length reverse $s` counts characters.  Without this the arg
      # inherits the caller's list context and a context-sensitive callee like
      # reverse/sort returns a list.
      if ($func_name && $func_name =~ /^(length|uc|lc|ucfirst|lcfirst|fc
                                         |ord|chr|hex|oct|quotemeta
                                         |abs|int|sqrt|sin|cos|exp|log
                                         |defined|ref)$/x) {
        return SCALAR_CTX
            if $child_index >= 1;
      }

      # split takes scalar arguments (pattern, string, limit) even though it
      # returns a list.  In list context (e.g. join ':', split('a'=~/b/, $s)) the
      # pattern arg must stay scalar — otherwise `'a' =~ /b/` returns the list (1)
      # instead of the scalar 1 used as the pattern.
      if ($func_name && $func_name eq 'split') {
        return SCALAR_CTX
            if $child_index >= 1;
      }

      # Functions that take a filehandle as their first argument.
      # The FH arg must be SCALAR_CTX: bareword FHs become (pl-NAME) funcalls,
      # and wrapping them in (let ((*wantarray* t)) ...) prevents %p-fh-arg
      # from recognising them, causing an UNDEFINED-FUNCTION crash.
      if ($func_name && $func_name =~ /^(readdir|opendir|closedir|seekdir|telldir|rewinddir|eof|getc|read|sysread|syswrite|fileno|binmode|truncate)$/) {
        return SCALAR_CTX if $child_index == 1;  # First arg is the filehandle
      }

      # return: the value expression inherits *wantarray* from the caller's
      # dynamic scope — emit no binding so context propagates through.
      if ($func_name && $func_name eq 'return') {
        return INHERIT_CTX;
      }

      # Force LIST_CTX for '..'/'...' operators in function argument position so
      # they generate a range, not a flip-flop.  Other arguments inherit the
      # parent's context — this lets prototype-forced scalar context (e.g.
      # Test::More's is($$;$)) work correctly via wantarray propagation.
      # NOTE: '..' nodes are PPI::Token::Operator (not PPIreference), so we
      # must check the PPI token content, not is_internal_node_type.
      if ($child_index >= 1) {
        my $child_id = $children->[$child_index];
        if (defined $child_id) {
          my $child_node = $self->get_a_node($child_id);
          my $cop;
          if ($self->is_internal_node_type($child_node)) {
            $cop = $child_node->{type};
          } elsif (ref($child_node) eq 'PPI::Token::Operator') {
            $cop = $child_node->content();
          }
          return LIST_CTX if defined($cop) && ($cop eq '..' || $cop eq '...');
        }
      }

      # A sub declared with an explicit prototype evaluates the arguments that
      # land in its slurpy (@/%) tail in LIST context — e.g. try (&;@) runs the
      # trailing catch/finally blocks in list context, so Try::Tiny's catch
      # (croak unless wantarray) is happy.  We act ONLY on the slurpy tail of a
      # KNOWN prototype; unprototyped subs and $-proto positions (e.g.
      # Test::More's is($$;$)) are left to inherit, matching Perl when the
      # prototype isn't known to us — this keeps is(unpack(...), ...) scalar.
      if ($func_name && $child_index >= 1 && $self->environment) {
        my $proto = $self->environment->get_prototype($func_name);
        if ($proto && $proto->{is_proto} && $proto->{params}) {
          my @p = @{$proto->{params}};
          my $slurpy_at;
          for my $j (0 .. $#p) {
            my $pt = $p[$j]{proto_type} // '';
            if ($pt eq '@' || $pt eq '%') { $slurpy_at = $j; last; }
          }
          return LIST_CTX
            if defined($slurpy_at) && ($child_index - 1) >= $slurpy_at;

          # A scalar ($) — or reference (\$, \@, \%, \&, \*) — prototype slot
          # imposes SCALAR context on that argument, even when the call sits in
          # void/list context.  This is what makes Test::More's is($$;$) /
          # ok($;$) / like($$;$) evaluate `is(try {42}, 42)` with try in scalar
          # context (so it returns 42, not undef).  Without it the arg inherits
          # the caller's context — VOID at statement level — and wantarray()
          # reports undef inside the callee.
          my $pidx = $child_index - 1;
          if (!defined($slurpy_at) || $pidx < $slurpy_at) {
            my $pt = ($pidx <= $#p) ? ($p[$pidx]{proto_type} // '') : '';
            return SCALAR_CTX if $pt eq '$' || $pt =~ /^\\/;
          }
        }
      }

      # A call to an unprototyped, non-builtin (user) function evaluates its
      # arguments in LIST context: Perl flattens the argument list into @_, so a
      # context-sensitive argument — myfunc(split /::/, $name), catfile(split …)
      # — must run as a list, not collapse to a scalar (e.g. split's field
      # count).  This is the sibling of the methodcall rule below.  It is SAFE
      # only because prototyped subs are handled by the block above: the TAP
      # assertions (is/ok/like/…) carry real ($$@)-style prototypes — extracted
      # from test.pl (require) or the Test::More shim (use) — so their leading
      # scalar slots still impose SCALAR context (keeping is(unpack(...), …)
      # scalar).  Builtins are excluded via known_no_of_params (they have their
      # own context rules above).
      if ($func_name && $child_index >= 1
          && !exists $self->known_no_of_params->{$func_name}) {
        return LIST_CTX;
      }
    }

    # Method-call arguments are always LIST context: a Perl method call passes
    # its args as a flat list (methods cannot have prototypes), so a
    # context-sensitive arg — `$obj->m(split /,/, $s)`, File::Spec->catfile(split
    # /::/, $name) — must run in list context.  kids[0]=invocant, kids[1]=method,
    # kids[2+]=args.
    if ($type eq 'methodcall') {
      return LIST_CTX if $child_index >= 2;
    }
    # progn (comma operator) forces list context
    if ($type eq 'progn') {
      return LIST_CTX;
    }

    # Array/hash constructors are list context
    if ($type eq 'arr_init' || $type eq 'hash_init') {
      return LIST_CTX;
    }

    # Ternary: condition is scalar, branches inherit parent
    if ($type eq 'ternary') {
      return SCALAR_CTX if $child_index == 0;  # Condition
      return $parent_ctx;  # True/false branches inherit
    }

    # Prefix operators: child[0]=op token, child[1]=operand.
    # Boolean/arithmetic ops force scalar context on their operand.
    # Without this, !!($a && $b) inside join() produces (vector ...) wrapper.
    if ($type eq 'prefix_op' && $child_index == 1) {
      my $children  = $self->get_node_children($parent_id);
      my $op_node   = $self->get_a_node($children->[0]);
      my $op        = $op_node->can('content') ? $op_node->content() : '';
      if ($op =~ /^(!|not|~|\\|[+\-])$/) {
        return SCALAR_CTX;
      }
    }

    # Chained comparison (postfix_op with 5+ alternating term/op children).
    # e.g. $a == $b != $c  =>  ['postfix_op', $a, '==', $b, '!=', $c]
    # Term children (even indices 0,2,4,...) are comparison operands —
    # they must be scalar even when pl-chain-cmp appears inside join().
    if ($type eq 'postfix_op') {
      my $children = $self->get_node_children($parent_id);
      if (scalar(@$children) >= 5 && scalar(@$children) % 2 == 1) {
        return SCALAR_CTX if $child_index % 2 == 0;  # term child
      }
    }
  }

  # - - - Token operator nodes
  if ($parent_node->can('content')) {
    my $op          = $parent_node->content();

    # Assignment operator
    if ($op eq '=') {
      my $children  = $self->get_node_children($parent_id);
      my $lhs_id    = $children->[0];
      my $lhs       = $self->get_a_node($lhs_id);

      if ($child_index == 0) {
        # LHS: context based on lvalue type
        return $self->lvalue_context($lhs);
      } elsif ($child_index == 1) {
        # RHS: context based on what LHS expects
        return $self->assignment_rhs_context($lhs, $lhs_id);
      }
    }

    # String concatenation always forces scalar context on both operands.
    # Without this, parens inside concat inherit list context from outer
    # constructs (e.g. [...]) and produce unwanted (vector ...) wrappers.
    if ($op eq '.' || $op eq '.=') {
      return SCALAR_CTX;
    }

    # Logical NOT always forces scalar context: !expr, not expr.
    # !!($a && $b) passed as join() arg must not produce (vector ...) wrapper.
    if ($op eq '!' || $op eq 'not') {
      return SCALAR_CTX;
    }

    # Short-circuit logical ops (&&, and, ||, //, or): the LHS is always
    # evaluated in scalar (boolean) context — even in list context, a true LHS
    # is returned as a scalar (`@a = (@x || @y)` yields the count of @x, not its
    # elements).  The RHS is the value returned when the LHS short-circuits, so
    # it inherits the surrounding context (`() || (1,2)` -> (1,2) in list ctx).
    if ($op eq '&&' || $op eq 'and'
        || $op eq '||' || $op eq '//' || $op eq 'or') {
      return $child_index == 0 ? SCALAR_CTX : $parent_ctx;
    }

    # xor is purely boolean — always scalar.
    if ($op eq 'xor') {
      return SCALAR_CTX;
    }

    # Comparison operators always produce scalar results.
    if ($op =~ /^(==|!=|<|>|<=|>=|eq|ne|lt|gt|le|ge|<=>|cmp)$/) {
      return SCALAR_CTX;
    }

    # Arithmetic operators produce scalar results.
    if ($op =~ /^([+\-*\/%]|\*\*|x)$/) {
      return SCALAR_CTX;
    }

    # Bit-shift and bitwise operators are numeric/string scalar operators:
    # their operands must be scalar even when the operator sits in list
    # context (e.g. an unprototyped funcall arg).  Without this,
    # `($x || 255) << 8` evaluates the `||` RHS in list context and the shift
    # yields 0.  Includes the bitwise-string variants (&. |. ^.).
    if ($op =~ /^(<<|>>|&|\||\^|&\.|\|\.|\^\.)$/) {
      return SCALAR_CTX;
    }
  }

  # Default: children inherit parent's context
  return $parent_ctx;
}


# Determine what context an lvalue should have (indicates type of lvalue)
sub lvalue_context {
  my $self          = shift;
  my $lhs           = shift;

  # Simple variable
  if ($lhs->can('content')) {
    my $content     = $lhs->content();
    return LIST_CTX if $content =~ /^[@%]/;  # Array or hash
    return SCALAR_CTX;  # Scalar variable
  }

  # Complex lvalue - check the type
  if ($self->is_internal_node_type($lhs)) {
    my $type        = $lhs->{type};
    
    # Array operations return list context
    return LIST_CTX if $type =~ /^(a_ref_acc|a_acc|slice_a_acc)$/;
    
    # Hash operations return list context
    return LIST_CTX if $type =~ /^(h_ref_acc|h_acc|slice_h_acc|kv_slice_h_acc|kv_slice_a_acc)$/;
    
    # List of lvalues: ($a, $b, $c)
    return LIST_CTX if $type eq 'progn' || $type eq 'tree_val';
    
    # Everything else (scalar deref, etc.)
    return SCALAR_CTX;
  }

  # Default: scalar
  return SCALAR_CTX;
}


# Determine RHS context based on LHS of assignment
sub assignment_rhs_context {
  my $self          = shift;
  my $lhs           = shift;
  my $lhs_id        = shift;

  # Simple variable
  if ($lhs->can('content')) {
    my $content     = $lhs->content();
    return LIST_CTX if $content =~ /^[@%]/;  # Array or hash assignment
    return SCALAR_CTX;  # Scalar assignment
  }

  # Complex lvalue - check the type
  if ($self->is_internal_node_type($lhs)) {
    my $type        = $lhs->{type};
    
    # List of lvalues: ($a, $b, $c) = ...
    return LIST_CTX if $type eq 'progn' || $type eq 'tree_val';
    
    # Array/hash slices take lists
    return LIST_CTX if $type =~ /^slice_/;

    # @{...} or %{...} deref: prefix_op with @ or % cast operator
    if ($type eq 'prefix_op') {
      my $kids   = $self->get_node_children($lhs_id);
      my $op_node = $self->get_a_node($kids->[0]) if @$kids;
      my $op = ($op_node && $op_node->can('content')) ? $op_node->content() : '';
      return LIST_CTX if $op =~ /^[@%]/;
    }

    # Single element access is scalar
    # (even if it's an array/hash element: $arr[0] = ..., $hash{key} = ...)
    return SCALAR_CTX;
  }

  # Default: scalar context
  return SCALAR_CTX;
}


# Helper: Set context metadata on a node
sub set_node_context {
  my $self          = shift;
  my $node_id       = shift;
  my $context       = shift;

  $self->node_tree->set_metadata($node_id, 'context', $context);
}

# Helper: Get context metadata from a node
sub get_node_context {
  my $self          = shift;
  my $node_id       = shift;

  return $self->node_tree->get_metadata($node_id, 'context') // SCALAR_CTX;
}

# Like get_node_context but returns undef when no context was ever annotated
# (instead of defaulting to SCALAR_CTX).  Used where the default-scalar fallback
# would be wrong — e.g. a slice in an unannotated position is list-natural, not
# scalar, so it must not be reduced to its last element.
sub get_node_context_raw {
  my $self    = shift;
  my $node_id = shift;
  return $self->node_tree->get_metadata($node_id, 'context');
}

# Helper: Get context name for debugging
sub context_name {
  my $self          = shift;
  my $ctx           = shift;

  return 'SCALAR' if $ctx == SCALAR_CTX;
  return 'LIST'   if $ctx == LIST_CTX;
  return 'VOID'   if $ctx == VOID_CTX;
  return 'UNKNOWN';
}


# ----------------------------------------------------------------------
# Debug:

sub debug_dump_tree {
  my $self      = shift;
  my $node_id   = shift;
  my $indent    = shift // 0;

  say "Dump of parse tree:"   if $indent == 0;

  my($node)     = $self->get_nodes($node_id);
  my $kids      = $self->get_node_children($node_id);

  my $ind_str   = ". " x $indent;
  if ($self->is_internal_node_type($node)) {
    say $ind_str, $node->{type};
  } else {
    my $ref     = ref $node;
    my $text    = $node->content() // '????';
    say "${ind_str}id $node_id, Class $ref, value: $text";
  }

  $indent++;
  for my $id (@$kids) {
    $self->debug_dump_tree($id, $indent);
  }
}


# ----------------------------------------------------------------------
# Operands on item queue:


# Unused routines. Should probably have been used.
sub _is_empty {
  my $self      = shift;
  my $e         = shift;

  return 1
      if scalar(@$e) == 0;

  return undef;
}


sub _peek {
  my $self      = shift;
  my $e         = shift;

  return undef
      if scalar(@$e) == 0;

  return $e->[0];
}


sub _peek_next {
  my $self      = shift;
  my $e         = shift;

  return undef
      if scalar(@$e) < 2;

  return $e->[1];
}

sub _peek_next_next {
  my $self      = shift;
  my $e         = shift;

  return undef
      if scalar(@$e) < 3;

  return $e->[2];
}

sub _peek_next_next_next {
  my $self      = shift;
  my $e         = shift;

  return undef
      if scalar(@$e) < 4;

  return $e->[3];
}


sub _pop {
  my $self      = shift;
  my $e         = shift;

  return shift @$e;
}

sub _unpop {
  my $self      = shift;
  my $e         = shift;
  my $item      = shift;

  unshift @$e, $item;
}


# Find priority of an operand
sub _get_prio {
  
}

# ----------------------------------------------------------------------

sub get_nodes {
  my $self      = shift;
  my @node_ids  = @_;

  my $node_tree = $self->node_tree();
  my @out;
  for my $id (@node_ids) {
    push @out, $node_tree->node_data($id);
  }

  return @out;
}

sub get_a_node {
  my $self      = shift;
  my $node_id   = shift;

  my $node_tree = $self->node_tree();
  # say "---- get_a_node(): Before calling node_data()";
  my $node      = $node_tree->node_data($node_id);
  # say "---- get_a_node(): After calaling node_data()"; say dump  $node;
  return  $node;
}


sub get_node_children {
  my $self      = shift;
  my $node_id   = shift;

  my $node_tree = $self->node_tree();
  return $node_tree->children_ids($node_id);
}



# Parse a subscript @ix list, handling bareword subscripts.
# In $a[bar] / $h{bar}, PPI gives a Statement::Expression wrapping a Token::Word.
# handle_subcalls would turn that into a funcall — wrong for barewords.
# We detect the pattern and return a string-literal node instead.
# Whether a lone bareword subscript should be autoquoted to a string.
# HASH subscripts ($h{bar}) always autoquote.  ARRAY subscripts ($a[bar]) are
# numeric expressions: Perl evaluates the bareword as a function/constant call
# IF one of that name is known at this point (e.g. a use-constant index like
# $self->[P_ALLOW_NONREF]); otherwise (no strict subs) an unknown bareword is
# just the string "bar" → numeric 0.  So we autoquote unless it's a known
# callable, mirroring Perl's compile-time decision.
sub _bareword_subscript_autoquotes {
  my ($self, $name, $is_array) = @_;
  return 1 unless $is_array;                       # hash subscript: always quote
  return 1 unless $self->has_parser;               # no environment: fall back to quote
  my $env = $self->parser->environment;
  return 0 if $env->has_prototype($name);          # constant or prototyped/known sub
  for my $s (@{ $env->get_declared_subs || [] }) {
    return 0 if defined $s->{name} && $s->{name} eq $name;
  }
  # In eval-string mode the prototype table is empty, but constants/subs from the
  # enclosing program DO exist at runtime as zero-arg subs. Perl only autoquotes
  # barewords in HASH subscripts — `$a[FOO]` is always a sub call — so an ALL-CAPS
  # bareword here (the convention for constants, matching the handle_subcalls
  # heuristic) must stay callable, not be stringified to a 0 index.
  return 0 if $self->parser->eval_mode && $name =~ /^[A-Z][A-Z0-9_]*$/;
  return 1;                                         # unknown bareword: string index
}

sub _subscript_to_cl_str {
  my ($subscript, $self, $is_array) = @_;
  my @kids = grep { !$_->isa('PPI::Token::Whitespace') } $subscript->children();
  my @inner = @kids;
  if (@inner == 1 && $inner[0]->isa('PPI::Statement::Expression')) {
    @inner = grep { !$_->isa('PPI::Token::Whitespace') } $inner[0]->children();
  }
  if (@inner == 1) {
    my $k = $inner[0];
    if (ref($k) eq 'PPI::Token::Word') {
      return '"' . $k->content . '"'
        if $self->_bareword_subscript_autoquotes($k->content, $is_array);
      # else: known callable in an array subscript — evaluate it (fall through)
    } elsif (ref($k) eq 'PPI::Token::Number') {
      return $k->content;
    }
  }
  return $self->parser->_parse_expression(\@inner, undef) if $self->has_parser;
  return undef;
}

sub _parse_subscript_ix {
  my ($self, $ix, $is_array) = @_;
  my @sig = grep { !$_->isa('PPI::Token::Whitespace') } @$ix;
  if (@sig == 1 && $sig[0]->isa('PPI::Statement::Expression')) {
    my @ekids = grep { !$_->isa('PPI::Token::Whitespace') } $sig[0]->children();
    if (@ekids == 1 && ref($ekids[0]) eq 'PPI::Token::Word') {
      my $word = $ekids[0]->content();
      if ($self->_bareword_subscript_autoquotes($word, $is_array)) {
        my $str_tok = PPI::Token::Quote::Single->new("'$word'");
        return $self->make_node($str_tok);
      }
      # else: known callable in an array subscript — parse as an expression below
    }
  }
  return $self->parse($ix);
}

sub make_node {
  my $self      = shift;
  my $node      = shift;

  my $node_tree = $self->node_tree();
  return $node_tree->add_node($node);
}

sub add_child_to_node {
  my $self      = shift;
  my $node_id   = shift;
  my $child_id  = shift;

  die "Tried to add child to non-numeric node ID ($node_id)"
      if ! looks_like_number($node_id);
  die "Tried to add non-numeric child ($child_id) to node ID $node_id"
      if ! looks_like_number($child_id);

  my $node_tree = $self->node_tree();
  $node_tree->add_child_id($node_id, $child_id);
}


sub prepend_child_to_node {
  my $self      = shift;
  my $node_id   = shift;
  my $child_id  = shift;

  my $node_tree = $self->node_tree();
  $node_tree->unshift_child_id($node_id, $child_id);
}


sub set_top_node_id {
  my $self      = shift;
  my $top_node  = shift;

  $self->root($top_node);

  my $node_tree = $self->node_tree();
  $node_tree->node_top($top_node);
}


sub _add_tag_to_node {
  my $self      = shift;
  my $node_id   = shift;
  my $extra     = shift;

  # XXXX Needed??
  my $node_tree = $self->node_tree();
  $node_tree->add_extra($node_id, $extra);
  

}


# ----------------------------------------------------------------------
# Util:


# Removes whitespace, makes "=>" into "," and makes strings out of
# some keywords. Done to simpllify the rest of the parsing.

sub cleanup_for_parsing {
  my $self      = shift;
  my $stmts     = shift;

  # Filter out whitespace and comments
  my @no_ws     = grep {
    ref($_) !~ /Token::Whitespace/ && ref($_) ne 'PPI::Token::Comment'
  } @$stmts;

  # PPI BUG WORKAROUND: PPI parses "expr)-1" as "expr)" followed by negative
  # number "-1", but Perl actually interprets this as subtraction "expr) - 1".
  # We detect negative numbers following expression-ending tokens and split
  # them into minus operator + positive number.
  # TODO: File bug report with PPI project.
  @no_ws = $self->_fix_ppi_negative_number_bug(\@no_ws);

  # PPI BUG WORKAROUND: PPI parses "word :" in ternary as Label instead of
  # Word + Operator. Split labels back into their components when preceded by "?".
  @no_ws = $self->_fix_ppi_ternary_label_bug(\@no_ws);

  # PPI BUG WORKAROUND: Perl 5.40+ '^^' (logical XOR) is tokenized by PPI as
  # two consecutive '^' operators.  Merge them into a single '^^' token.
  @no_ws = $self->_fix_ppi_logical_xor_bug(\@no_ws);

  # PPI BUG WORKAROUND: After blocks, PPI parses <*.txt> as separate tokens
  # instead of a glob. Reconstruct glob tokens from < PATTERN > sequences.
  @no_ws = $self->_fix_ppi_glob_after_block(\@no_ws);

  for(my $i=0; $i < scalar(@no_ws); $i++) {
    my $part    = $no_ws[$i];

    # - - - Make $foo{bar} into $foo{"bar"}
    if ($self->is_hash_braces($part)) {
      my(@h_ix) = $part->children;

      # XXXX This is destructive, redo sometime.
      # Remove any whitespace:
      while(scalar(@h_ix) && ref($h_ix[0]) =~ /Token::Whitespace/) {
        shift @h_ix;
      }
      while(scalar(@h_ix) && ref($h_ix[-1]) =~ /Token::Whitespace/) {
        pop @h_ix;
      }
      if (scalar(@h_ix) == 1
          && ref($h_ix[0]) eq 'PPI::Statement::Expression') {
        my(@items)    = $h_ix[0]->children;
        if (scalar(@items) == 1 && ref($items[0]) eq 'PPI::Token::Word') {
          my $str     = $self->_make_string_of_token_word($items[0]);
          $h_ix[0]->replace_child($items[0], $str);
        }
      }
    }
    # - - - Replace foo => "bar" with "foo" => "bar":
    if (ref($part) eq 'PPI::Token::Operator' && $part->content() eq '=>') {
      $part->set_content(",");

      # Need to check previous too, so it isn't a string constant
      # without quotes:
      next
          if $i == 0;
      my $prev  = $no_ws[$i-1];
      $no_ws[$i-1] = $self->_make_string_of_token_word($prev)
          if ref($prev) eq "PPI::Token::Word";
    }
  }

  return \@no_ws;
}


# PPI BUG WORKAROUND: Split negative numbers into minus operator + positive number
# when they follow expression-ending tokens. PPI incorrectly parses "foo()-1" as
# the number -1 rather than subtraction. Perl's actual parser treats this as "- 1".
# See cleanup_for_parsing() for context.
sub _fix_ppi_logical_xor_bug {
  my $self   = shift;
  my $tokens = shift;

  # PPI tokenizes Perl 5.40's '^^' (logical XOR) as two separate '^' operators.
  # Merge consecutive '^' '^' into a single '^^' operator token so the
  # precedence loop sees it at the same level as '||' (not bitwise '^').
  my @result;
  for (my $i = 0; $i < @$tokens; $i++) {
    if (ref($tokens->[$i]) eq 'PPI::Token::Operator'
        && $tokens->[$i]->content eq '^'
        && $i + 1 < @$tokens
        && ref($tokens->[$i+1]) eq 'PPI::Token::Operator'
        && $tokens->[$i+1]->content eq '^') {
      push @result, PPI::Token::Operator->new('^^');
      $i++;  # consume the second '^'
    } else {
      push @result, $tokens->[$i];
    }
  }
  return @result;
}

sub _fix_ppi_negative_number_bug {
  my $self   = shift;
  my $tokens = shift;

  my @result;
  for (my $i = 0; $i < @$tokens; $i++) {
    my $token = $tokens->[$i];

    # Check if this is a negative number
    if (ref($token) eq 'PPI::Token::Number' && $token->content =~ /^-(.+)$/) {
      my $positive_part = $1;

      # ** has higher precedence than unary minus in Perl.
      # If a negative literal is followed by **, always split: -3**2 = -(3**2).
      my $next_is_pow = ($i + 1 < @$tokens &&
                         ref($tokens->[$i+1]) eq 'PPI::Token::Operator' &&
                         $tokens->[$i+1]->content eq '**');

      # Check if previous token ends an expression (where subtraction makes sense)
      my $is_expr_end = 0;
      if ($i > 0) {
        my $prev = $result[-1];  # Use result array since we may have inserted
        my $prev_ref = ref($prev);

        # Expression-ending tokens: ) ] } or symbols/words/numbers
        # Named unary functions (chr, abs, uc, etc.) are NOT expression-enders:
        # "chr -1" means chr(-1), not chr() - 1.
        my $prev_is_named_unary = ($prev_ref eq 'PPI::Token::Word'
                                   && $self->is_named_unary($prev->content));
        $is_expr_end = (
          $prev_ref eq 'PPI::Structure::List'        ||  # (...)
          $prev_ref eq 'PPI::Structure::Subscript'   ||  # [...]
          $prev_ref eq 'PPI::Structure::Block'       ||  # {...}
          $prev_ref eq 'PPI::Token::Symbol'          ||  # $foo
          ($prev_ref eq 'PPI::Token::Word'
           && !$prev_is_named_unary)                 ||  # bareword/const (not named unary)
          $prev_ref eq 'PPI::Token::Number'          ||  # number
          $prev_ref eq 'PPI::Token::Quote::Double'   ||  # "string"
          $prev_ref eq 'PPI::Token::Quote::Single'   ||  # 'string'
          $prev_ref =~ /^PPI::Token::Quote/               # other quotes
        );
      }

      if ($is_expr_end || $next_is_pow) {
        # Split into minus operator and positive number
        my $minus_op = bless { content => '-' }, 'PPI::Token::Operator';
        my $pos_num  = bless { content => $positive_part }, 'PPI::Token::Number';
        push @result, $minus_op, $pos_num;
        next;
      }
    }

    push @result, $token;
  }

  return @result;
}


# PPI BUG WORKAROUND: In ternary expressions like "cond ? foo : bar",
# PPI sometimes parses "foo :" as a Label instead of Word + Operator.
# This happens when there's a space before the colon.
# We detect Labels that follow "?" and split them back into Word + ":".
sub _fix_ppi_ternary_label_bug {
  my $self   = shift;
  my $tokens = shift;

  my @result;
  my $seen_question = 0;

  for (my $i = 0; $i < @$tokens; $i++) {
    my $token = $tokens->[$i];

    # Track if we've seen a ? (ternary operator)
    if (ref($token) eq 'PPI::Token::Operator' && $token->content eq '?') {
      $seen_question = 1;
    }

    # Check if this is a Label after a ? (likely part of ternary)
    if (ref($token) eq 'PPI::Token::Label' && $seen_question) {
      my $content = $token->content;
      # Label content is like "word :" or "word:" - extract the word
      if ($content =~ /^(\w+)\s*:\s*$/) {
        my $word = $1;
        # Split into Word and : operator
        my $word_token  = bless { content => $word }, 'PPI::Token::Word';
        my $colon_token = bless { content => ':' }, 'PPI::Token::Operator';
        push @result, $word_token, $colon_token;
        $seen_question = 0;  # Reset after finding the colon
        next;
      }
    }

    # Reset seen_question after we've processed a complete ternary
    if (ref($token) eq 'PPI::Token::Operator' && $token->content eq ':') {
      $seen_question = 0;
    }

    push @result, $token;
  }

  return @result;
}


# PPI BUG WORKAROUND: After a block (e.g., grep { } or map { }), PPI fails to
# recognize <*.txt> as a file glob (PPI::Token::QuoteLike::Readline). Instead,
# it parses it as separate tokens: < (operator), * (operator), . (operator),
# txt (word), > (operator).
#
# This happens because PPI's tokenizer doesn't have enough context after a
# closing brace to know that < starts a glob rather than a comparison operator.
#
# Example: "grep { /a/ } <*.txt>" is misparsed as:
#   grep, {/a/}, <, *, ., txt, >
# Instead of:
#   grep, {/a/}, <*.txt>
#
# This workaround detects sequences like < TOKENS > that look like glob patterns
# and reconstructs them into a single PPI::Token::QuoteLike::Readline token.
sub _fix_ppi_glob_after_block {
  my $self   = shift;
  my $tokens = shift;

  my @result;
  my $i = 0;

  while ($i < @$tokens) {
    my $token = $tokens->[$i];

    # Look for < that might start a broken glob
    if (ref($token) eq 'PPI::Token::Operator' && $token->content eq '<') {
      # Scan ahead to find matching > and check if it looks like a glob
      my $j = $i + 1;
      my $glob_content = '';
      my $has_glob_chars = 0;
      my $found_close = 0;

      while ($j < @$tokens) {
        my $t = $tokens->[$j];
        my $c = $t->can('content') ? $t->content : '';

        # Found closing >
        if (ref($t) eq 'PPI::Token::Operator' && $c eq '>') {
          $found_close = 1;
          last;
        }

        # Accumulate content
        $glob_content .= $c;

        # Check for glob metacharacters — only count actual token content,
        # NOT the content of structure nodes (PPI::Structure::Subscript [1]
        # contains '[1]' which would falsely match \[ or \]).
        $has_glob_chars = 1
            if ref($t) !~ /^PPI::Structure/
            && $c =~ /[\*\?\[\]]/;

        # Stop if we hit something that can't be part of a glob
        last if ref($t) eq 'PPI::Token::Operator' && $c =~ /^(==|!=|<=|>=|<=>|&&|\|\|)$/;
        last if ref($t) eq 'PPI::Token::Operator' && $c eq '->'; # $ref->[n] not a glob
        last if ref($t) eq 'PPI::Structure::List';  # Parentheses

        $j++;
      }

      # Also detect bare filehandle readline: < BAREWORD > when not preceded
      # by a value token (symbol/number/string/structure) — those indicate < is
      # the less-than operator, not the readline diamond.
      my $is_bare_fh = ($glob_content =~ /^[A-Za-z_][A-Za-z0-9_:]*$/);
      # Scalar filehandle readline <$fh>: a single scalar variable between < and >.
      # PPI misparses this as `< $fh >` (two operators) whenever it follows a
      # bareword that could take an operand — print/return/scalar/sort <$fh>.
      my $is_scalar_fh = ($glob_content =~ /^\$[A-Za-z_]\w*$/);
      my $prev = @result ? $result[-1] : undef;
      # A simple value (symbol/number/string) before < means it's definitely lt, not glob.
      # e.g. $a<$b?1:$a>$b: the < is less-than, not a glob opener.
      # PPI::Structure (block/subscript) before < can still be a glob (e.g. sort { } <*.txt>).
      my $prev_is_simple_value = $prev && ref($prev) =~ /^PPI::Token::(Symbol|Number|Quote)/;
      my $prev_is_value = $prev_is_simple_value || ($prev && ref($prev) =~ /^PPI::Structure/);

      # If we found a valid-looking glob pattern or bare/scalar filehandle, reconstruct it
      if ($found_close && !$prev_is_simple_value && ($has_glob_chars || (($is_bare_fh || $is_scalar_fh) && !$prev_is_value)) && $glob_content ne '') {
        # Create a proper readline token
        my $glob_token = bless {
          content => "<$glob_content>"
        }, 'PPI::Token::QuoteLike::Readline';
        push @result, $glob_token;
        $i = $j + 1;  # Skip past all the consumed tokens
        next;
      }
    }

    push @result, $token;
    $i++;
  }

  return @result;
}


# (Expects a PPI::Token::Word object, but just use the content() method.)
sub _make_string_of_token_word {
  my $self      = shift;
  my $tokenword = shift;

  my $str       = $tokenword->content();
  my $strobj    = PPI::Token::Quote::Double->new('"' . $str . '"');
  $strobj->{separator} = '"';      # Can't do that in the API?
  return $strobj;
}



# Util:

# Operator utilities
sub op_is_chained {
  my $self      = shift;
  my $op_info   = shift;        # The info in $self->precedences.

  return $op_info->{chained};
}

sub remove_expression_object_around {
  my $self      = shift;
  my $e_list    = shift;

  # Handle any PPI::Statement* wrapper by extracting children
  if (ref($e_list) =~ /^PPI::Statement/) {
    my @kids    = $e_list->children();
    return \@kids;
  }

  if (ref($e_list) eq "ARRAY" && scalar(@$e_list) == 1) {
    if (ref($e_list->[0]) =~ /^PPI::Statement/) {
      my @kids  = $e_list->[0]->children();
      return \@kids;
    }
  }

  return $e_list;
}

# XXXXX Extend this, to be able to see where a list ends.
# 1. Need number of expected parameters in list (if known).
# 2. Need to know precedence.

# Used by fun calls, etc. Typically used for list of parameters.
# Expects a cleaned up list w/out comments and whitespace.
sub parse_comma_separated_list {
  my $self      = shift;
  my $stmts     = shift;

  if (ref($stmts) eq 'PPI::Statement::Expression') {
    # Usually puts an exprssion object around the items in expr list.
    $stmts      = $stmts->children();
  }

  my @out;
  my $present   = [];
  for my $s (@$stmts) {
    my $comma   = $self->is_token_operator($s);
    if ($comma && $comma eq ',') {
      push @out, $present;
      $present  = [];
    } else {
      push @$present, $s;
    }
  }

  if (scalar @$present) {
    # Skip empty () — a single empty Structure::List contributes nothing to a list.
    # e.g. unshift(@a, ()) or push(@a, ()) should pass no extra arguments.
    my @non_ws = grep { ref($_) !~ /::Whitespace$/ } @$present;
    unless (scalar(@non_ws) == 1
            && ref($non_ws[0]) eq 'PPI::Structure::List'
            && !scalar($non_ws[0]->children())) {
      push @out, $present;
    }
  }

  return \@out;
}


# Instead of PPI Expr, represents a packed node tree.

# These are stored as nodes, when need to create new. Otherwise just
# data structures in our list of expression ops/data.

# These are stored as nodes, when need to create new. Otherwise just
# data structures in our list of expression ops/data.
sub make_subtree_item {
  my $self      = shift;
  my $node_id   = shift;
  my $type      = shift;

  # Make recognizable object and return. It should have tags for
  # postfix subs too.
  # Need to update is_word() etc to handle this too?

  my $tmp_node  = { id => $node_id };
  $tmp_node->{type} = $type
      if $type;
  bless $tmp_node, 'PPIreference';
  return $tmp_node;
}

# A less painful call for the previous sub:
sub make_node_insert {
  my $self      = shift;
  my $type      = shift;

  my $node      = $self->make_subtree_item(-1, $type);
  my $id        = $self->make_node( $node );
  $self->id_of_internal_node($node, $id);

  return ($node, $id);
}


sub id_of_internal_node {
  my $self      = shift;
  my $node      = shift;

  my $id        = $node->{id};
  $node->{id}   = shift
      if @_;

  return $id;
}


1;

__END__

=head1 NAME

Pl::PExpr - Expression parser that extends PPI with operator precedence

=head1 SYNOPSIS

    use Pl::PExpr;
    use PPI;

    # Example 1: Parse a simple expression
    my $doc    = PPI::Document->new(\'$x + $y * 2');
    my @tokens = $doc->children->[0]->children;

    my $parser = Pl::PExpr->new(
        e        => \@tokens,
        full_PPI => $doc,    # Prevents GC of tokens during parsing
    );

    my $root_id = $parser->parse_expr_to_tree();

    # Access the AST
    my $tree      = $parser->node_tree;
    my $root_node = $tree->node_data($root_id);
    my $children  = $tree->children_ids($root_id);


    # Example 2: Extract variable declarations from expressions
    # (e.g., '$x' from 'if (my $x = foo()) { ... }')
    my ($root_id, $declarations) = $parser->parse_expr_to_tree();
    for my $decl (@$declarations) {
        say "$decl->{type} $decl->{var}";  # e.g., "my $x"
    }

=head1 DESCRIPTION

Pl::PExpr is an expression parser that extends L<PPI>. It takes PPI
token arrays as input and produces an Abstract Syntax Tree (AST)
with correct operator precedence, suitable for code generation.

PPI parses Perl source into tokens and basic structure, but does not
build expression trees with operator precedence. Pl::PExpr fills this
gap.

The parser can optionally receive information about subroutine
prototypes, filehandles, and constants (zero-parameter subs) via the
Environment object. Parsing assumes C<use strict> is in effect, so
unknown barewords are treated as subroutine names.

The original motivation was to build a Perl-to-Common-Lisp transpiler
(compiled Lisp is fast, and S-expressions are easy to transform
further). The distribution includes B<PCL> (Perl to Common Lisp), a
prototype transpiler demonstrating Pl::PExpr usage. Many tests
transpile Perl code, execute it in both Perl and SBCL, and compare the
output.

Pl::PExpr handles:

=over 4

=item * Operator precedences

=item * All Perl operators, with ternary C<?:> etc

=item * Function and method calls (C<< $obj->method() >>, C<< Class->new() >>)

=item * Array and hash access (C<$a[0]>, C<$h{key}>, slices)

=item * References and dereferences (C<\$x>, C<$$ref>, C<< $aref->[0] >>)

=item * String interpolation

=item * Anonymous subs (C<sub { ... }>)

=item * Variable declarations (C<my>, C<our>, C<state>, C<local>)

=item * Context annotation (scalar vs list)

=item * Regex: C<m//>, C<s///>, C<tr///> with modifiers

=item * Diamond operator C<< <FH> >>, C<< <$fh> >>

=back

=head1 CONSTRUCTOR

Pl::PExpr uses L<Moo> for object construction.

    my $parser = Pl::PExpr->new(
        e           => \@ppi_tokens,      # Required
        full_PPI    => $ppi_document,     # Recommended
        environment => $env_object,       # Optional
        parser      => $statement_parser, # Optional
    );

=head2 Attributes

=over 4

=item e

ArrayRef of PPI tokens representing the expression to parse.
Can also be passed directly to C<parse_expr_to_tree()>.

=item full_PPI

The L<PPI::Document> object containing the tokens. Keeping a reference
prevents PPI from garbage-collecting tokens during parsing. This is
mainly relevant for test code where the original document may go out
of scope. Without it, you may see mysterious errors when the underlying
data structures are freed.

=item environment

Optional L<Pl::Environment> object that tracks declared constants,
subroutine prototypes, and package information. When provided, the
parser recognizes functions with known parameter counts and handles
prototypes correctly.

=item parser

Optional L<Pl::Parser> object for recursive parsing of nested blocks
containing statements (e.g., C<grep { BLOCK } @list> or anonymous
subs). Required for multi-statement block support.

=item node_tree

L<Pl::OpcodeTree> object that stores the AST nodes. Created
automatically. Use this to access parsed nodes.

=item declarations

ArrayRef of variable declarations found during parsing. Each entry
is a hashref: C<< { type => 'my', var => '$x' } >>. Populated by
C<parse_expr_to_tree()>.

=back

=head1 METHODS

=head2 parse_expr_to_tree

    my $root_id = $parser->parse_expr_to_tree();
    my $root_id = $parser->parse_expr_to_tree(\@tokens);

    # List context returns declarations too
    my ($root_id, $declarations) = $parser->parse_expr_to_tree();

Main entry point. Parses the expression and returns the AST root node ID.
In list context, also returns an arrayref of variable declarations found.

The returned ID can be used with C<< $parser->node_tree >> to access
the AST structure.

=head1 CONTEXT CONSTANTS

Exported on request:

    use Pl::PExpr qw(SCALAR_CTX LIST_CTX VOID_CTX);

=over 4

=item SCALAR_CTX (0)

Expression evaluated in scalar context.

=item LIST_CTX (1)

Expression evaluated in list context.

=item VOID_CTX (2)

Expression evaluated in void context (result discarded).

=back

=head1 AST NODE TYPES

The parser produces an AST stored in L<Pl::OpcodeTree>. Each node has
a C<type> field indicating its kind.

=head2 Operators

=over 4

=item Binary operators (binop)

Binary operators like C<+>, C<*>, C<=>, C<< < >>, etc. are stored as
the raw PPI::Token::Operator object with two children (left and right
operands). The C<dump_tree> utility displays these as C<binop(op)>.

    $a + $b      -> binop(+)[$a, $b]
    $x * $y      -> binop(*)[$x, $y]
    $a = $b      -> binop(=)[$a, $b]

=item prefix_op

Prefix unary operator. Children: [operator_token, operand].

    !$x          -> prefix_op[!, $x]
    -$n          -> prefix_op[-, $n]
    \$ref        -> prefix_op[\, $ref]

=item postfix_op

Postfix unary operator. Children: [operand, operator_token].

    $x++         -> postfix_op[$x, ++]
    $y--         -> postfix_op[$y, --]

=item ternary

Ternary conditional operator. Children: [condition, if_true, if_false].

    $a ? $b : $c -> ternary[$a, $b, $c]

=item =~

Regex match/substitution. Children: [string, pattern].

    $s =~ /pat/  -> binop(=~)[$s, /pat/]

=back

=head2 Function and Method Calls

=over 4

=item funcall

Function call. First child is the function name; remaining children
are arguments.

    print("hello")   -> funcall(print, "hello")
    push @arr, $x    -> funcall(push, @arr, $x)

=item methodcall

Method call on an object. Children: [object, method_name, args...].

    $obj->method($x) -> methodcall($obj, 'method', $x)

=item ref_funcall

Call through a code reference.

    &$subref()       -> ref_funcall($subref)
    $code->($arg)    -> ref_funcall($code, $arg)

=back

=head2 Data Structures

=over 4

=item a_ref_acc

Array reference access (arrow notation).

    $arr->[0]        -> a_ref_acc($arr, 0)
    $arr->[0][1]     -> a_ref_acc(a_ref_acc($arr, 0), 1)

=item progn

Sequence of expressions (comma operator or list context).

    ($a, $b, $c)     -> progn($a, $b, $c)

=item string_concat

Interpolated strings are decomposed into parts for concatenation. The
original string is preserved in the node's C<extras()> field (see
L<Pl::OpcodeTree>). Note: the C<.> operator produces a regular binop.

    "Foo $bar baz"   -> string_concat("Foo ", $bar, " baz")

=item readline

Diamond operator for reading from a filehandle.

    <FH>             -> readline('FH')
    <$fh>            -> readline($fh)

=back

=head2 Special Forms

=over 4

=item anon_sub

Anonymous subroutine.

    sub { $x + 1 }   -> anon_sub(BLOCK)

=item func_ref

Reference to a named subroutine.

    \&mysub          -> func_ref('mysub')

=item filehandle

Filehandle argument for print/say.

    print STDERR $x  -> funcall(print, filehandle(STDERR), $x)

=back

=head1 EXPRESSION EXAMPLES

=head2 Arithmetic with Precedence

    # Input
    $a + $b * $c

    # AST (multiplication binds tighter)
    binop(+)
        $a
        binop(*)
            $b
            $c

=head2 Method Chain

    # Input
    $obj->foo->bar($x)

    # AST
    methodcall
        methodcall
            $obj
            'foo'
        'bar'
        $x

=head2 Ternary Operator

    # Input
    $x > 0 ? "positive" : "non-positive"

    # AST
    ternary
        binop(>)
            $x
            0
        "positive"
        "non-positive"

=head2 Array and Hash Access

    # Input: $data->{users}[0]{name}

    # AST (nested access)
    h_ref_acc
        a_ref_acc
            h_ref_acc
                $data
                'users'
            0
        'name'

=head2 Anonymous Sub

    # Input
    my $add = sub { $_[0] + $_[1] };

    # AST (declarations + assignment)
    # Declarations: [{type => 'my', var => '$add'}]
    binop(=)
        $add
        anon_sub(BLOCK)

=head2 Regex Match

    # Input
    $str =~ /^\d+$/

    # AST
    binop(=~)
        $str
        /^\d+$/

=head2 List Operations

    # Input
    grep { $_->is_valid } @items

    # AST
    funcall
        grep
        anon_sub(BLOCK)
        @items

=head2 Complex Expression

    # Input
    my ($x, $y) = $hash{key} // [0, 0];

    # Declarations: [{type => 'my', var => '$x'}, {type => 'my', var => '$y'}]
    # AST for assignment:
    binop(=)
        progn
            $x
            $y
        binop(//)
            gethash
                $hash
                'key'
            progn
                0
                0

=head1 DUMPING THE PARSE TREE

To inspect the AST structure, see the C<dump_tree> function in
C<examples/parse_expr.pl>. Key methods for traversing the tree:

    my $tree     = $parser->node_tree;
    my $node     = $tree->node_data($node_id);      # Get node data
    my $children = $tree->children_ids($node_id);   # Get child IDs

=head2 Example Output

For the expression C<$x + $y * 2>:

    [4] binop(+)
      [3] Token::Symbol: $x
      [2] binop(*)
        [0] Token::Symbol: $y
        [1] Token::Number: 2

For the expression C<$obj-E<gt>method($a, $b)>:

    [0] methodcall
      [4] Token::Symbol: $obj
      [3] Token::Word: method
      [1] Token::Symbol: $a
      [2] Token::Symbol: $b

For the expression C<$x ? $a : $b>:

    [3] ternary
      [0] Token::Symbol: $x
      [1] Token::Symbol: $a
      [2] Token::Symbol: $b

=head1 COMPLETE WORKING EXAMPLE

See C<examples/parse_expr.pl> in the distribution for a complete script
that parses any Perl expression and dumps the AST.

Usage:

    $ perl examples/parse_expr.pl '$x > 0 ? "yes" : "no"'
    AST for: $x > 0 ? "yes" : "no"
    [6] ternary
      [2] binop(>)
        [0] Token::Symbol: $x
        [1] Token::Number: 0
      [3] Token::Quote::Double: "yes"
      [4] Token::Quote::Double: "no"

=head1 DEPENDENCIES

=over 4

=item * L<Moo> - Object system

=item * L<PPI> - Perl parser (provides input tokens)

=item * L<Pl::OpcodeTree> - AST storage

=item * L<Pl::PExpr::Config> - Operator precedence and function specs

=item * L<Pl::PExpr::TokenUtils> - Token classification

=item * L<Pl::PExpr::StringInterpolation> - String interpolation handling

=back

=head1 SEE ALSO

=over 4

=item * L<Pl::Parser> - Statement-level parser that uses Pl::PExpr for
expressions, generates Common Lisp

=item * L<Pl::ExprToCL> - Code generator that transforms the Pl::PExpr
AST into Common Lisp

=item * L<Pl::Environment> - Tracks constants, prototypes, and packages

=item * L<PPI> - The Perl parser that provides the input tokens

=back

=head1 AUTHOR

Bernt Budde

=head1 LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

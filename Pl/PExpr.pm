package Pl::PExpr;

use v5.30;
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
    SCALAR_CTX => 0,
    LIST_CTX   => 1,
    VOID_CTX   => 2,
};

# Export for use in tests/other modules
use Exporter 'import';
our @EXPORT_OK = qw(SCALAR_CTX LIST_CTX VOID_CTX);

# Expects a statement/expression in PPI.

# Keep configuration for a scope. Keep code that should be added after
# scope.

# XXXX Postfix 'if' will look like a word. What more cases do we need
#      to recognize?? That should be recognized before calling this.
#      p-fix 'if' isn't expr, must be full statement. But is expr in PPI.

# XXXX This also need to keep track ofcontexts, scalar och list!
#      All calls get that as an extra parameter? Also, need to keep
#      stack for uses of caller().

# XXXX Handle default $_. (Just add automagic param for chomp etc?)
#      Mark ops which can take that default $_ in op list??
#      So needs to support varying no of params for ops.

# XXXX Logic here for @_? Or do in destination?

# XXXX Range '..'.

# XXXX ':?'

# XXXX &sub(), &$subref(), ref to the running sub with __SUB__->().
#      Note that &foo(...) ignores prototype checks! Page 326.

# XXXX Tests for 'isa'.

# XXXX Reread about prototypes.

# XXXX 'grep { ... } <expr>' etc.

# XXXX Unary ops have a different prio compared to list ops, se page 106.

# XXXX Assignments '+=', '-=', '*=', '**=', etc.

# XXXX Does the '//' op work, like '$x // $y'?

# State like use v5.20, known cases with subs + parameter specs, etc.

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



# Functions with known number of parameters (and types).
# Should also have declarations.

# XXXX Also need to rething subs with declared prototypes etc. (Does
#      constants for e.g. tcp/ip integratiion need special handling??)

# XXXX Need to be extensible, for Prototypes.

# XXXXX Need to flag for if it returns different in scalar/list contexts.

# XXXXX Would it be shorter to list all funs not defaulting to $_?

# -1 means list. -1x means x parameters before a list.
# -2 means use $_ as default.

# perldoc perlfun:


# Expression:
has e => (
  is        => 'ro',
  required  => 0,
);

# Parts get GC:ed, if there are no references to the PPI object!
# So keep the PPI::Document alive to prevent tokens from becoming empty.
# (This is probably only a problem when writing tests.)
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
has string_interpolator => (
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

  # XXXX Get list of filehandles? Of subs with know no of params?
  #      Can prototypes get FH as params? If so, add proto for open/close/etc

  # XXXX Clear any stored temporary stuff??
  # Clear node tree here?

  # XXXX Find any postfix 'if' before calling this?

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
        }
        say "extract_declarations: Found $decl_type for: ", join(", ", @vars)
            if 1 & DEBUG;
      }

      # Add the remaining expression parts (without the declarator)
      # The variable itself stays - just the 'my'/'our'/etc is stripped
      if (@vars) {
        # Recreate just the variable(s) and rest of expression
        for my $var (@vars) {
          push @result, PPI::Token::Symbol->new($var);
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
      push @{$self->declarations}, { type => $self->{_pending_decl}, var => $var };
      say "extract_declarations: Found ", $self->{_pending_decl}, " $var"
          if 1 & DEBUG;
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

# XXXX Replace comparing ->{type} to 'progn', 'funcall', etc... :-(
# XXXX Handle chained ops.
# XXXX Handle ?:.

sub parse {
  my $self      = shift;
  my $e         = shift // $self->e;

  if ($self->is_list($e)) {
    # if (ref($e) eq 'PPI::Structure::List') {
    my @list    = $self->children();
    $e          = \@list;
  }

  $e            = $self->cleanup_for_parsing($e);
  $self->handle_subcalls($e);
  say "parse: //////  After calling handle_subcalls, in param:"  if 1 & DEBUG;
  say dump($e)      if 1 & DEBUG;


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

    if (ref($e1) eq 'PPI::Structure::List') {
      my @list    = $e1->children();
      $e          = \@list;
      return $self->parse($e);
    }

    # Handle Block structures (used in braced derefs like ${$ref}, @{$expr})
    if (ref($e1) eq 'PPI::Structure::Block') {
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
        return $self->string_interpolator->parse_interpolated_string($self,$e1);
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


    # - - - Readline operator <FH> or <$fh>?
    if (ref($e1) eq 'PPI::Token::QuoteLike::Readline') {
      say "parse(): Found readline operator"         if 1 & DEBUG;
      my $content = $e1->content;
      # Extract the filehandle from <...>
      $content =~ /^<(.*)>$/;
      my $fh_name = $1;

      # Create a readline node with the filehandle
      my ($node, $node_id) = $self->make_node_insert('readline');

      if (defined $fh_name && $fh_name ne '') {
        # Has a filehandle - could be bareword (STDIN) or variable ($fh)
        if ($fh_name =~ /^\$/) {
          # Variable filehandle like $fh
          my $sym_token = PPI::Token::Symbol->new($fh_name);
          my $fh_id = $self->make_node($sym_token);
          $self->add_child_to_node($node_id, $fh_id);
        } else {
          # Bareword filehandle like STDIN, FH
          my $word_token = PPI::Token::Word->new($fh_name);
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
        $cmd_id = $self->string_interpolator->parse_interpolated_string($self, $str_token);
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

    # - - - What else can it be?? :-)
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
    next
        if !$self->is_arrow_op($term)
        && !$self->is_arr_or_hash_braces($term)
        && !$is_constructor_subscript;

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
        my $pre_id = $self->parse([$pre]);
        my $pst_id = $nxt->{id};
        my $kids   = $self->get_node_children($pst_id);

        my($node, $id) = $self->make_node_insert('ref_funcall');
        $self->add_child_to_node($id, $pre_id);   # Fun ref
        for my $kid_id (@$kids) {
          $self->add_child_to_node($id, $kid_id); # Parameters
        }

        $e->[$i-1] = $node;
        splice @$e, $i, 2;
        $i--;
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
        $i--;  # Adjust for removed elements so we recheck for following subscript
        next;
      } elsif (!$self->is_internal_node_type($nxt)
               && $nxt->content() =~ /^\$/
               && $nxt_2 && $nxt_2->{type} eq 'tree_val') {
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
      } else {
        say "??? Term:", dump($term), "\nNext is:", dump $nxt;
        say " Next 2:", dump $nxt_2;
        exit 0;
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
      } elsif ($self->is_var($pre_n)
               && $pre_n->content() =~ /^@/) {
        $type   = "slice_$type";
      }
      my($node, $id) = $self->make_node_insert($type);

      my @ix    = $term->children();
      my $ix_id = $self->parse(\@ix);

      # Add $pre as child 1
      $self->add_child_to_node($id, $pre_id);

      # Add index to arr or hash:
      if ($type =~ /^slice_/) {
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

      # XXXX Remove extra in #e and replace with array access:
      $e->[$i-1]= $node;
      splice @$e, $i, 1;

      $i--;                     # ??
      next;
    }

    # Handle Constructor [ ] after funcall/methodcall - PPI uses Constructor
    # instead of Subscript when subscript follows a method call
    # e.g., $obj->method()[$i] has [$i] as Constructor, not Subscript
    if (ref($term) eq 'PPI::Structure::Constructor'
        && $term->start() eq '['
        && $self->is_internal_node_type($pre)) {
      # Treat as array subscript on the result of the previous expression
      my $pre_id = $pre->{id};
      my($node, $id) = $self->make_node_insert('a_ref_acc');

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

  # XXXX Can a code block be declared as a parameter in an expr,
  #      except for prototype declarations?? Read up if we can get
  #      that.  Note that a 'sub { ... } returns a 'CODE'. There are
  #      no operations on that.
  #      Like: foobar(4, sub { ... }, 5);

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

    say "++++++ Found an op to replace. Got ", $op->content(),
        ", precedence: $hi_prio"                     if 2 & DEBUG;

    # Create the tree:
    my $no_pars = $op_info->{no};

    # Handle chained compared:
    if ($self->op_is_chained($op_info) && $hi_ix >= 3) {
      # The chained ops are specified as "r" associative, so we will
      # get the last of the chain here.
      # t1 op t2 op t3, like "17 <= $foo <= 42".
      my $prev_2= $e->[$hi_ix-2];
      my $info_2= $self->op_info($prev_2);

      if (defined $info_2 && $self->op_is_chained($info_2)) {
        # Chained.
        my $prev_3 = $e->[$hi_ix-3];            # 1st term is 3 back from op
        my $id_3   = $self->parse([$prev_3]);

        my $id_op2 = $self->make_node($prev_2); # 1st op is 2 back.

        my $prev   = $e->[$hi_ix-1];            # Prev is 2nd term.
        my $id_prev= $self->parse([$prev]);

        my $id_op  = $self->make_node($op);     # The 1st op is at ix.

        my $post   = $e->[$hi_ix+1];            # Prev is 2nd term.
        my $id_post= $self->parse([$post]);

        my($top_node, $top_id) = $self->make_node_insert('postfix_op');
        $self->add_child_to_node($top_id, $id_3);    # First term in chain
        $self->add_child_to_node($top_id, $id_op2);  # First op in chain
        $self->add_child_to_node($top_id, $id_prev); # Second term in chain
        $self->add_child_to_node($top_id, $id_op);   # Second op in chain
        $self->add_child_to_node($top_id, $id_post); # Third term in chain

        $e->[$hi_ix-3] = $top_node;
        splice @$e, $hi_ix-2, 4;
        next;
      }
    }

# say dump $e; say "---"; say dump $op_info; say dump $self->node_tree; exit 0;


    if ($no_pars == 2) {
      my $prev  = $e->[$hi_ix-1];
      my $post  = $e->[$hi_ix+1];
      my $id_bef= $self->parse([$prev]);

      # Ugly. Set flag for parsing this, so it doesn't add '$_ =~' to regexp:
      my $match_op = ($op_name eq '=~' || $op_name eq '!~');
      if ($match_op && ref($post) =~ /Regexp::Match/) {
        $post->{_has_match_context}++;
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

      # Find condition start: scan backwards from ? to find lower-prec operator or ':'
      my $cond_start = 0;
      for (my $i = $hi_ix - 1; $i >= 0; $i--) {
        my $info = $self->op_info($e->[$i]);
        if ($info && $info->{prec} <= $ternary_prec) {
          # Stop at lower-prec operators OR at ':' (which marks outer ternary boundary)
          $cond_start = $i + 1;
          last;
        }
      }

      # Find false end: scan forward from : to find lower-prec operator
      my $false_end = $#{$e};
      for (my $i = $colon_pos + 1; $i <= $#{$e}; $i++) {
        my $info = $self->op_info($e->[$i]);
        if ($info && $info->{prec} < $ternary_prec) {
          $false_end = $i - 1;
          last;
        }
      }

      say "Ternary: cond_start=$cond_start, ?=$hi_ix, :=$colon_pos, false_end=$false_end"
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
        my($node, $id) = $self->make_node_insert('prefix_op');
        my $op_id      = $self->make_node($op);
        $self->add_child_to_node($id, $op_id);     # Prefix operand
        $self->add_child_to_node($id, $id_term);   # Expr.

        $e->[$hi_ix] = $node;
        splice @$e, $hi_ix+1, 1;
        next;
      }

    }

    die "Yabba dabba doh! op=" . dump($op) . " info=" . dump($op_info);
  }

  if (scalar(@$e) == 1 && $self->is_internal_node_type($e->[0])) {
    return $self->id_of_internal_node($e->[0]);
  }

  # Single atomic element (number, string, variable, etc.)
  if (scalar(@$e) == 1) {
    return $self->make_node($e->[0]);
  }

  die "Fell through. Missing case: " . dump($e);


  # XXXX There is an extra object over old parenthese exprs We could
  #      remove that? E.g. 5 * (4 + 7)
  #      Is there an extra level also for progn?
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
  $self->handle_subcalls($e_list); # If a funcall w/o () in the list.

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


# This replaces all sub calls in an expression.
# It use known number of parameters for subs and priorities.

# XXXXX This needs to handle `open FH, ... and ... <FH> ...`??
# Both so doesn't try to define FH as a sub when declaring/closing it
# in open/close and also when referencing it.

# XXXXX Need to look at sub name, parameters are different for
# different subs (Prototypes and built in).

# XXXXX Now it is:
#        foo
#   par1  par2  par3 ...
# That looks bad with empty param list. Instead generate this:
#         funcall
#   foo     par1 par2 par3 ...
sub handle_subcalls {
  my $self      = shift;
  my $e         = shift;

  say "---- handle_subcalls. Incoming expr:\n", dump($e)     if 8 & DEBUG;
  # XXXX Special case for open/close/<FH>/etc, so knows next param is a FH.
  #      Declare that in sub specs above?? That would be the easy way.

  # - - - Handle: `fun(...)`:
  # (Yes, loops to all but last.)
  for(my $i=0; $i < scalar(@$e)-1; $i++) {
    my $now     = $e->[$i];
    my $next    = $e->[$i+1];
    say "handle_subcalls: Look for subname(..) in:\n", dump $now  if 8 & DEBUG;
    next
        if !$self->is_word($now); # Only want function calls.

    say "handle_subcalls() Look for subname(..), was word. Is next list ",
        ($self->is_list($next) ? "Yes" : "No"), ". Dump:", dump $next
        if 8 & DEBUG;

    # XXXX Here, handle subs that have known params, also 0 (default to $_.)??
    # e.g.  foo xxx, yyy, chdir z, u, v;
    #  pars:    ___  ___  _______  _  _
    #            1    2      3     3  4.

    # Handle grep/map { BLOCK } LIST pattern
    # Uses Parser.pm callback for multi-statement blocks
    # Also handles: sub { ... } (anonymous subs)
    if (ref($next) eq 'PPI::Structure::Block') {
      my $func_name = $now->content();
      if ($func_name eq 'grep' || $func_name eq 'map' || $func_name eq 'sort'
          || $func_name eq 'eval') {

        # Create funcall with block as first param
        my($top_node, $top_id) = $self->make_node_insert('funcall');
        my $node_id = $self->make_node($now);
        $self->add_child_to_node($top_id, $node_id);

        # Use parser callback if available (handles multi-statement blocks)
        if ($self->has_parser) {
          # Determine parameters based on function type
          my $params = ($func_name eq 'sort') ? ['$a', '$b']
                     : ($func_name eq 'eval') ? []
                     : ['$_'];  # grep, map

          # Parse block as a named function and get its name
          my $block_func_name = $self->parser->parse_block_as_function($next, $params);

          # Create a func_ref node that holds the function name
          my($ref_node, $ref_id) = $self->make_node_insert('func_ref');
          $ref_node->{func_name} = $block_func_name;
          $self->add_child_to_node($top_id, $ref_id);
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

        # Parse remaining elements as the list to process
        # Note: parse as a whole expression since $arr[0]->method is one expression
        if ($i + 2 < scalar(@$e)) {
          my @rest = @$e[$i + 2 .. $#$e];
          my $rest_list = $self->cleanup_for_parsing(\@rest);
          # Parse rest as comma-separated list (usually just one element)
          my $rest_ids = $self->parse_list($rest_list);
          for my $rest_id (@$rest_ids) {
            $self->add_child_to_node($top_id, $rest_id);
          }
          # Remove all processed elements
          splice @$e, $i, scalar(@$e) - $i;
        } else {
          splice @$e, $i, 2;
        }
        $e->[$i] = $top_node;
        next;
      }

      # Handle anonymous sub: sub { ... }
      if ($func_name eq 'sub') {
        # Use parser callback if available (handles multi-statement blocks)
        if ($self->has_parser) {
          # Anonymous subs take no implicit parameters
          my $block_func_name = $self->parser->parse_block_as_function($next, []);

          # Create a func_ref node that holds the function name
          my($ref_node, $ref_id) = $self->make_node_insert('func_ref');
          $ref_node->{func_name} = $block_func_name;

          # Replace sub { } with the function reference
          splice @$e, $i, 2;
          $e->[$i] = $ref_node;
        } else {
          # Fallback: parse block as expression (single statement only)
          my @block_children = $next->children();
          my $block_expr = $self->cleanup_for_parsing(\@block_children);
          my $block_id = $self->parse($block_expr);

          # Create anon_sub node
          my($sub_node, $sub_id) = $self->make_node_insert('anon_sub');
          $self->add_child_to_node($sub_id, $block_id);

          # Replace sub { } with the anon_sub
          splice @$e, $i, 2;
          $e->[$i] = $sub_node;
        }
        next;
      }
    }

    next
        if !$self->is_list($next);

    # - - - Special handling for open: register bareword filehandle BEFORE parsing args
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

    # Replace the two items in expr with a subtree:
    my($top_node, $top_id) = $self->make_node_insert('funcall');

    my $c_ids   = $self->make_nodes_from_list($next);
    my $node_id = $self->make_node($now);

    $self->add_child_to_node($top_id, $node_id);
    for my $c_id (@$c_ids) {
      $self->add_child_to_node($top_id, $c_id);
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

    # - - - Skip if this word is followed by -> (class method call like Foo->new)
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

    # - - - Skip if this is a filehandle for print/say:
    # print STDERR "hello" - STDERR is a filehandle, not a function
    if ($i > 0) {
      my $prev = $e->[$i - 1];
      if ($self->is_word($prev)) {
        my $prev_name = $prev->content;
        if ($prev_name eq 'print' || $prev_name eq 'say') {
          # Check if this looks like a filehandle (uppercase bareword)
          if ($sub_name =~ /^[A-Z][A-Z0-9_]*$/) {
            next;  # Skip - will be handled when processing print/say
          }
        }
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

    # XXXX Should return list of possible no of  parameters.
    #      Update this.
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
    if ($is_zero_param && $i + 1 < scalar(@$e)) {
      my $next = $e->[$i + 1];
      if ($self->is_token_operator($next)) {
        # Function followed by operator - treat as zero params
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
        # Operators that can be unary prefix: + - ! ~ \ not
        my %can_be_unary_op = map { $_ => 1 } ('+', '-', '!', '~', '\\', 'not');
        my $is_unary = $is_cast || $can_be_unary_op{$next_op};
        if (!$is_unary) {
          # Binary-only operator - treat bareword as zero-arg function
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

    # Named unary operators only take the next single term
    # But Cast + Symbol (like @$list) counts as one term
    # And Symbol + Subscript (like $h{key} or $a[0]) counts as one term
    my $func_name_for_unary = $now->content();
    if ($self->is_named_unary($func_name_for_unary) && $end_pars > $i + 1) {
        my $next_term = $e->[$i + 1];
        if (ref($next_term) eq 'PPI::Token::Cast' && $end_pars >= $i + 2) {
            # Cast followed by Symbol is a single dereference term
            $end_pars = $i + 2;
        } elsif (ref($next_term) eq 'PPI::Token::Symbol' && $end_pars >= $i + 2) {
            # Check if symbol is followed by subscript (hash/array access)
            my $after_symbol = $e->[$i + 2];
            if (ref($after_symbol) eq 'PPI::Structure::Subscript') {
                # Symbol + Subscript is one term (e.g., $h{key}, $a[0])
                $end_pars = $i + 2;
            } else {
                $end_pars = $i + 1;
            }
        } else {
            $end_pars = $i + 1;
        }
    }

    # Functions taking 1 param also need Cast+Symbol handling (e.g., shift @$arr)
    # Check if this is a 1-param function with Cast+Symbol as argument
    if (defined $no_pars && $end_pars > $i + 1) {
      my $is_single_param = ($no_pars == 1)
                         || (ref($no_pars) eq 'ARRAY' && grep { $_ == 1 } @$no_pars);
      if ($is_single_param) {
        my $next_term = $e->[$i + 1];
        if (ref($next_term) eq 'PPI::Token::Cast' && $end_pars >= $i + 2) {
          # Cast followed by Symbol is a single dereference term
          $end_pars = $i + 2;
        }
      }
    }

    # - - - Special handling for print/say with filehandle:
    # print FILEHANDLE LIST  (no comma between filehandle and list)
    # print $fh LIST         (variable filehandle)
    my $filehandle_id;
    if (($sub_name eq 'print' || $sub_name eq 'say') && $i + 1 <= $end_pars) {
      my $maybe_fh = $e->[$i + 1];
      my $is_fh = 0;

      # Check for uppercase bareword (STDERR, STDOUT, FH, etc.)
      # Note: is_word() returns 1/undef, not content - use ->content
      if ($self->is_word($maybe_fh)) {
        my $fh_name = $maybe_fh->content;
        if ($fh_name =~ /^[A-Z][A-Z0-9_]*$/) {
          $is_fh = 1;
        }
      }
      # Check for variable filehandle ($fh)
      # For variable filehandles, there MUST be more content after them
      # print $fh "hello" - $fh is filehandle (more content after)
      # print $x          - $x is the thing to print (nothing after)
      # print $d->method() - $d is object, NOT filehandle (-> follows)
      # print $h{k}->method() - hash access, NOT filehandle ({} follows)
      elsif (ref($maybe_fh) eq 'PPI::Token::Symbol') {
        # Only treat as filehandle if there's more content after
        # AND that content is not a method call (->) or subscript ({}[])
        if ($i + 2 <= $end_pars) {
          my $after_var = $e->[$i + 2];
          # If next element is -> it's a method call, not filehandle
          my $is_arrow = $after_var
            && ref($after_var) eq 'PPI::Token::Operator'
            && $after_var->content eq '->';
          # If next element is {} or [] it's a subscript, not filehandle
          my $is_subscript = $after_var
            && ref($after_var) =~ /^PPI::Structure::/;
          $is_fh = 1 unless ($is_arrow || $is_subscript);
        }
      }

      if ($is_fh) {
        # Check next element is NOT a comma (filehandle syntax has no comma)
        my $after_fh = $e->[$i + 2] if $i + 2 <= $end_pars;
        my $is_comma = $after_fh
          && ref($after_fh) eq 'PPI::Token::Operator'
          && $after_fh->content eq ',';
        if (!$is_comma) {
          # It's a filehandle - create node and remove from param list
          my($fh_node, $fh_id) = $self->make_node_insert('filehandle');
          my $fh_name_id = $self->make_node($maybe_fh);
          $self->add_child_to_node($fh_id, $fh_name_id);
          $filehandle_id = $fh_id;
          # Remove filehandle from expression list
          splice @$e, $i + 1, 1;
          $end_pars--;
        }
      }
    }

    # - - - Special handling for split with regex pattern:
    # split /pattern/, LIST - the regex should not be wrapped with $_ =~
    if ($sub_name eq 'split' && $i + 1 <= $end_pars) {
      my $maybe_regex = $e->[$i + 1];
      if (ref($maybe_regex) =~ /^PPI::Token::Regexp/) {
        # Mark regex as having match context so it's not wrapped with $_ =~
        $maybe_regex->{_has_match_context} = 1;
      }
    }

    # Everything to the right of the Expr seems to be parameter(s).
    my($top_node, $top_id) = $self->make_node_insert('funcall');
    my $c_ids   = $self->parse_list($e, $i+1, $end_pars);
    my $node_id = $self->make_node($e->[$i]);

    $self->add_child_to_node($top_id, $node_id);
    # Add filehandle as first parameter if present
    # Note: use 'defined' because ID 0 is valid but falsy
    if (defined $filehandle_id) {
      $self->add_child_to_node($top_id, $filehandle_id);
    }
    for my $c_id (@$c_ids) {
      $self->add_child_to_node($top_id, $c_id);
      # $self->add_child_to_node($node_id, $c_id);
    }

    # Add implicit $_ if function defaults to it
    my $func_name = $e->[$i]->content() if $e->[$i]->can('content');
    $self->add_implicit_default_param($func_name, $top_id);

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

  # If no parameters provided, add implicit $_ or @_
  if ($param_count == 0) {
    my $default_var;
    if ($has_array_default) {
      # @_ in subs, @ARGV in main (but we don't distinguish at parse time)
      $default_var = '@_';
      say "add_implicit_default_param: Adding \@_ to $func_name" if 8 & DEBUG;
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

  say "annotate_contexts: node $node_id, context ",
      $self->context_name($context)
      if 16 & DEBUG;

  # Store context on this node
  $self->set_node_context($node_id, $context);

  my $node          = $self->get_a_node($node_id);
  my $children      = $self->get_node_children($node_id);

  # Determine context for each child
  for my $i (0 .. $#{$children}) {
    my $child_id    = $children->[$i];
    my $child_ctx   = $self->child_context($node, $node_id, $i, $context);
    $self->annotate_contexts($child_id, $child_ctx);
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
        # First arg (block/code) inherits, second arg (list) is LIST_CTX

        # Second parameter (index 2 in children)
        return LIST_CTX
            if $child_index == 2;
      }

      # print/say force list context on all arguments
      if ($func_name && $func_name =~ /^(print|say)$/) {
        return LIST_CTX
            if $child_index > 0;  # All arguments after function name
      }
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
    return LIST_CTX if $type =~ /^(h_ref_acc|h_acc|slice_h_acc)$/;
    
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
sub _fix_ppi_negative_number_bug {
  my $self   = shift;
  my $tokens = shift;

  my @result;
  for (my $i = 0; $i < @$tokens; $i++) {
    my $token = $tokens->[$i];

    # Check if this is a negative number
    if (ref($token) eq 'PPI::Token::Number' && $token->content =~ /^-(.+)$/) {
      my $positive_part = $1;

      # Check if previous token ends an expression (where subtraction makes sense)
      if ($i > 0) {
        my $prev = $result[-1];  # Use result array since we may have inserted
        my $prev_ref = ref($prev);

        # Expression-ending tokens: ) ] } or symbols/words/numbers
        my $is_expr_end = (
          $prev_ref eq 'PPI::Structure::List'        ||  # (...)
          $prev_ref eq 'PPI::Structure::Subscript'   ||  # [...]
          $prev_ref eq 'PPI::Structure::Block'       ||  # {...}
          $prev_ref eq 'PPI::Token::Symbol'          ||  # $foo
          $prev_ref eq 'PPI::Token::Word'            ||  # bareword/func
          $prev_ref eq 'PPI::Token::Number'          ||  # number
          $prev_ref eq 'PPI::Token::Quote::Double'   ||  # "string"
          $prev_ref eq 'PPI::Token::Quote::Single'   ||  # 'string'
          $prev_ref =~ /^PPI::Token::Quote/               # other quotes
        );

        if ($is_expr_end) {
          # Split into minus operator and positive number
          my $minus_op = bless { content => '-' }, 'PPI::Token::Operator';
          my $pos_num  = bless { content => $positive_part }, 'PPI::Token::Number';
          push @result, $minus_op, $pos_num;
          next;
        }
      }
    }

    push @result, $token;
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

  push @out, $present
      if scalar @$present;

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

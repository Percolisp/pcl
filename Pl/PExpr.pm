# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::PExpr;

use v5.20;
use strict;
use warnings;

use Moo;
# use strictures 2;

use Scalar::Util qw/looks_like_number/;

use PPI;
use PPI::Dumper;
use Pl::CLForm ();
# For fh_bareword_shape — THE shape test for a bareword filehandle name, asked
# by four parse sites here and by the `defined FH` emitter.  Pl::Environment
# has no Pl:: dependencies of its own, so this cannot be circular.
use Pl::Environment ();

use Data::Dumper ();

# Diagnostic dumper for die() texts and DEBUG traces.  Core Data::Dumper, not
# Data::Dump: the compiler must run on a stock perl where only PPI and Moo were
# installed (the installer, CI, a user machine) -- Data::Dump is not core.
sub _dd {
  local $Data::Dumper::Terse    = 1;
  local $Data::Dumper::Indent   = 0;   # ONE line: a drop announcement keeps only the first line of its reason
  local $Data::Dumper::Sortkeys = 1;
  my $s = Data::Dumper::Dumper(map { _dd_compact($_) } @_);
  chomp $s;
  return $s;
}

# A PPI element dumps as `Class<content>` instead of its whole blessed hash
# (locations, separators ...): the token stream IS the diagnosis, the rest is
# noise in a `;; PARSE ERROR` comment.  One level into arrays and hashes, which
# is the shape every die() here hands over (the operand list being parsed).
sub _dd_compact {
  my ($v) = @_;
  return $v unless ref $v;
  if (Scalar::Util::blessed($v) && $v->isa('PPI::Element')) {
    (my $c = ref $v) =~ s/^PPI:://;
    my $txt = $v->can('content') ? $v->content : '';
    $txt = substr($txt, 0, 60) . '...' if length($txt) > 63;
    return "$c<$txt>";
  }
  return [ map { _dd_compact($_) } @$v ] if ref $v eq 'ARRAY';
  return { map { $_ => _dd_compact($v->{$_}) } keys %$v } if ref $v eq 'HASH';
  return $v;
}

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


# Debug tracing, as a COMPILE-TIME constant (#303 step 0, ruled s391): the
# ~50 `… if N & DEBUG` guards below are on the hottest paths in the compiler,
# and while DEBUG was a sub reading a file lexical it never inlined — the s386
# call trace measured 4.3M DEBUG calls per corpus transpile, all of them
# returning 0.  As a constant every guard folds away at compile time.
#
# Turning tracing on is therefore an ENV var, read once when this module is
# compiled, not a runtime setter:  PCL_PEXPR_DEBUG=N prove …
# The bits: 1 parse, 2 operator replacement, 4 parse_list, 8 handle_subcalls,
# 16 annotate_contexts, 32 string interpolation (StringInterpolation.pm).
use constant DEBUG => $ENV{PCL_PEXPR_DEBUG} // 0;



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
sub control_flow_ops   { shift->config->control_flow_ops }
sub statement_keywords { shift->config->statement_keywords }
sub named_unary { shift->config->named_unary }

# A NAMED UNARY OPERATOR, in perl's sense (toke.c): an operator whose operand
# is one term, binding LOOSER than the high-precedence binary ops and TIGHTER
# than comparison — so `f $x + 1` is f($x + 1) and `f "a" . "b"` is f("ab").
#
# TWO SOURCES, and they answer about different things (task #453):
#   * Config's `named_unary` table — the BUILTIN named unaries (defined, ref,
#     length, lc, uc, …).  Authoritative, and asked first: a builtin's name
#     means the builtin here even where a sub of that name is also declared,
#     which is the routing this site has always had.
#   * a DECLARED sub whose PROTOTYPE makes it one.  perl decides that from the
#     prototype alone, and `_proto_parse_spec` is already this file's ONE
#     reading of that shape: 1 for `($)`/`(*)`/`(_)`/`(\@)`, [0,1] for the
#     `;`-led `(;$)` — exactly perl's named-unary set — and -1 for every list
#     operator (`($;$)`, `(@)`, `($;)`, a signature, a plain sub).
#
# Before #453 the second source did not exist here, so a user `($)` sub took
# the strictly-single operand site instead — which stops at the first term and
# has no `_extend_high_prec`.  `f "a" . "b"` was therefore f("a")."b" and
# (for `(*)`, whose `_proto_max_args` declines to narrow) `g + 1, "\n"` was
# g(1, "\n").  Routing the unary-class prototypes here makes the two operand
# sites ONE mechanism for the shape they share, and leaves the strictly-single
# site to Config's non-named-unary 1-arg builtins (shift, close, fileno, eof),
# whose bareword-filehandle branch is the reason it still exists.
#
# The environment record is the ONLY second source: `known_no_of_params` is
# NOT consulted, because its 1 covers Config builtins (shift, close…) that are
# deliberately not named unaries.
sub is_named_unary {
  my $self = shift;
  my $name = shift;
  return 1 if $name && $self->named_unary->{$name};
  return $self->_declared_named_unary($name);
}

# The declared half of is_named_unary: a sub whose prototype's parse class is
# perl's named-unary one.  Kept separate so the two sources stay legible and
# so a caller that must ask only about BUILTINS can still read `named_unary`.
sub _declared_named_unary {
  my ($self, $name) = @_;
  return 0 if !defined $name || !length $name || !$self->has_environment;
  my $rec = $self->environment->get_prototype($name);
  return 0 if !$rec || !defined $rec->{min_params};
  my $spec = $self->_proto_parse_spec($rec);
  return 1 if !ref($spec) && $spec == 1;                       # ($) (*) (_) (\@)
  return 1 if ref($spec) eq 'ARRAY' && @$spec == 2
           && $spec->[0] == 0 && $spec->[1] == 1;              # (;$)
  return 0;
}

# True if TOKEN is an operator that acts as a unary PREFIX operator: the prefix
# set (! ~ \ ++ -- the DEREF CASTS $ @ % & * and the file-test ops -e/-f/-d/…)
# — i.e. operators whose operand is to their right.  Used to reduce a run of
# adjacent prefix operators inner-first (`!-e $x` => `!(-e $x)`).
#
# A Cast is one of them (#463 item 2).  op_info already calls every Cast a
# right-associative one-operand operator of precedence 90, and `\` — a Cast
# token too — has always answered YES here through the prefix table.  The
# other casts answered NO, so the ONE prefix op that outranks a cast (`++` /
# `--`, precedence 92) was reduced FIRST and then took the bare `$` cast token
# as its whole operand: `parse([Cast])` has no case for a lone cast, so the
# term walker declined and the STATEMENT WAS DROPPED (`++$$r`, `++${$r}`,
# `++${"name"}` and their `--` twins; every other prefix operator is at 90 and
# ties resolve rightmost, which is why `\$$r` / `!$$r` / `-$$r` always worked).
sub _is_prefix_op_token {
  my ($self, $tok) = @_;
  my $name = $self->is_token_operator($tok);
  return 0 unless defined $name;
  return 1 if ref($tok) eq 'PPI::Token::Cast';  # $ @ % & * \ — a deref cast
  my $info = $self->op_info($tok);
  return 0 unless $info && ($info->{no} // 0) == 1;
  return 1 if exists $self->prefix->{$name};   # ! ~ \ ++ -- -e -f …
  return 1 if $name =~ /^-[A-Za-z]$/;          # any file-test operator
  return 0;
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

# Phase B1 (docs/plan-one-compiler-s411.md): an ANALYSIS parse wants the
# expression's SHAPE — VarAnnotator's tree walk, Parser2's
# _expr_scalar_rooted — never its emission.  Under analysis_only the block
# sites (map/grep/sort/eval/do/&-proto/anon sub) build their lambda nodes
# with NO body: neither the structural route (the parser's `_v2_embed` hook)
# nor v1's text compile of the block runs, so an analysis parse compiles
# nothing and emits nothing — the discarded ~900 block compiles per corpus
# measured s411, and with them the reason those callers had to save/restore
# the parser's emission buckets.  A body-less lambda node reaching an
# emitter DIES there (rule 12): analysis trees never reach emission.
has analysis_only => (
  is      => 'ro',
  default => sub { 0 },
);


# Delegate token utility methods
sub is_atomic { shift->token_utils->is_atomic(@_) }
sub is_regexp { shift->token_utils->is_regexp(@_) }
sub is_string { shift->token_utils->is_string(@_) }
sub is_number { shift->token_utils->is_number(@_) }
sub is_var { shift->token_utils->is_var(@_) }
sub is_arrow_op { shift->token_utils->is_arrow_op(@_) }
# The five brace predicates are stateless one-liners in TokenUtils and never
# read their invocant, so they are called as FUNCTIONS — no accessor, no
# method dispatch — is_hash_braces is the compiler's hottest predicate
# (#387 family 34, s413).  The rest keep the delegating shape: some of them
# call sibling predicates on $self.
sub is_arr_or_hash_braces { Pl::PExpr::TokenUtils::is_arr_or_hash_braces(@_) }
sub is_arr_braces { Pl::PExpr::TokenUtils::is_arr_braces(@_) }
sub is_hash_braces { Pl::PExpr::TokenUtils::is_hash_braces(@_) }
sub is_inline_hash { Pl::PExpr::TokenUtils::is_inline_hash(@_) }
sub is_inline_arr { Pl::PExpr::TokenUtils::is_inline_arr(@_) }
sub is_token_operator { shift->token_utils->is_token_operator(@_) }
sub is_list { shift->token_utils->is_list(@_) }
sub is_word { shift->token_utils->is_word(@_) }
sub is_internal_node_type { shift->token_utils->is_internal_node_type(@_) }

# Is NAME a zero-argument function — `use constant NAME => …`, `sub NAME () {…}`,
# a `*NAME = sub () {…}` glob install?  Perl calls such a word a TERM, and that
# one fact decides two things in this file: the word lowers to a call with no
# arguments (the bareword branch of parse()), and it is NOT a print filehandle
# (`print FOO . "b"` is a concatenation, `print STDOUT -1` is a handle plus an
# argument).  ONE copy, because the two readings must never disagree — a word
# that is a term for one of them and a handle for the other is how
# `print FOO x 3` came to be dropped whole (task #361).
sub _is_zero_arg_func {
  my ($self, $name) = @_;
  return 0 unless $self->has_environment && $self->environment->has_prototype($name);
  # ONE reading of the record shape (Pl::Environment::proto_is_zero_arg): the
  # same question decides whether a module's prototype crosses a `use`
  # (Parser::_merge_module_prototypes), and the two tests had drifted apart
  # into a bug — see #365.
  return $self->environment->proto_is_zero_arg(
           $self->environment->get_prototype($name));
}

# The maximal run of Cast tokens immediately before position $p in @$e.
# Returns the index of the FIRST (outermost) cast, or $p itself when the
# token before $p is not a Cast.  The caller takes @$e[$start .. $p-1] as the
# casts and splices that same span.  See the "THE CAST RUN (#305)" comment at
# the Case 0-3 dispatch for what the run means; both cast-consuming sites
# read it through this ONE helper.
sub _cast_run_start {
  my ($self, $e, $p) = @_;
  my $s = $p;
  $s-- while $s >= 1 && ref($e->[$s-1]) eq 'PPI::Token::Cast';
  return $s;
}

# True when every cast in @$e[$from .. $to] is a scalar deref '$'.  An inner
# non-'$' cast is not a deref level (there is no slice-of-a-slice), so a run
# that fails this keeps the pre-#305 single-cast handling rather than being
# folded into the base.
sub _all_scalar_casts {
  my ($self, $e, $from, $to) = @_;
  return 0 if $from > $to;
  for my $k ($from .. $to) {
    return 0 if ref($e->[$k]) ne 'PPI::Token::Cast'
             || $e->[$k]->content ne '$';
  }
  return 1;
}


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
          # the binary-op parser sees ($k,$v) as a single LHS unit.  Tag it so
          # the funcall-paren detector won't mistake a stripped `my(...)` for a
          # call's own argument parens (`f my($y), LIST` → f($y, LIST), not f($y)).
          $decl_list->{_pcl_decl_list} = 1;
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
    # Check for standalone declarator word at start of expression.
    # Accept an explicit CORE:: prefix (CORE::state $y = ...) — PCL has no
    # overridable builtins, so CORE::<declarator> is the bare declarator.
    elsif (ref($item) eq 'PPI::Token::Word'
           && $item->content() =~ /^(?:CORE::)?(my|our|state|local)$/) {
      my $decl_type = $1;

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
      $item->{_pcl_decl_list} = 1;  # see note at the Statement::Variable branch
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
  $self->_merge_split_qualified_words($e);
  $self->_split_pid_magic_cast_run($e);
  $self->_fold_braced_punct_magic($e);
  $self->_retag_magic_array_index($e);
  $self->_fuse_print_filehandle_filetest($e);
  $self->_default_filetest_operand($e);
  # Collapse dynamic typeglob-slot *{EXPR}{SLOT} into a single glob_slot node
  # BEFORE handle_subcalls, so a preceding named unary grabs the whole glob-slot
  # as its argument (e.g. `defined *{$g}{CODE}` in Sub::Override) instead of just
  # the Cast '*'.  Without parens, handle_subcalls would otherwise orphan the
  # trailing {EXPR}{SLOT} blocks and the parse would fall through.
  $self->_retag_braced_deref_subscript($e);
  $self->_retag_list_slice_subscripts($e);
  $self->_insert_elided_call_arrows($e);
  $self->_precollapse_dyn_glob_slots($e);
  $self->handle_subcalls($e);
  say "parse: //////  After calling handle_subcalls, in param:"  if 1 & DEBUG;
  say _dd($e)      if 1 & DEBUG;

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
      # An EMPTY `{}` in term position is an anonymous HASH, never a block —
      # perl's toke.c decides that on the very next character after the brace,
      # with no ambiguity to resolve.  PPI already labels `{}` a Constructor
      # everywhere it can (`my $x = {}`, `return {}`, `[ {} ]`, `f({})`); the
      # one place it still says Block is after a bareword in paren-less
      # list-operator position — `f {}`, `explain {}` — which is exactly this
      # arm.  Falling through to the block-body parse below produced an empty
      # ARRAY there (`ref` said "" and the call lost an argument), task #276.
      #
      # This is NOT folded into _block_is_hash_constructor: that predicate
      # answers the map/grep/(&@) BODY question, where a bare `{}` is not valid
      # perl at all (`map {} (1,2)` is a syntax error), so widening it would be
      # a claim about input principle 9 says we do not have to read.
      if (_block_is_hash_constructor($e1) || _block_is_empty($e1)) {
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

      # Readline or file glob — perlop states the rule as a WHITELIST, and it
      # is the readline side that is narrow: "If what's within the angle
      # brackets is neither a filehandle nor a simple scalar variable
      # containing a filehandle name, typeglob, or typeglob reference, it is
      # interpreted as a filename pattern to be globbed."  So `<>` (ARGV), a
      # bareword handle and `<$fh>` are readline; EVERYTHING else globs.
      #
      # This test used to be the inverse — a blacklist of glob metacharacters
      # — so `<~>` was read as a readline on a filehandle named `~` and
      # emitted `(p-readline ~)`, an unbound CL symbol that killed the whole
      # file at load (task #415, t/op/glob.t:110 `ok <~>, '~ works'`), and so
      # did `<foo.txt>` and every other metacharacter-free pattern.  perl
      # agrees about the whitespace too: probed, `< $fh >` globs the string
      # "GLOB(0x…)" rather than reading a line, so the spellings are compared
      # untrimmed.
      # The spelling tested here is the one in the token by the time PExpr
      # runs, and Parser2's rename passes have already rewritten the symbol
      # INSIDE it: perl-tests/scalar.t's `<$fh>` arrives as
      # `<$main::fh__file__0>` once `$fh` is promoted to a file-level global.
      # A package-QUALIFIED scalar is still a simple scalar to perl — probed,
      # `<$main::fh>` reads a line — so the scalar test allows a qualifier.
      # `<<>>` is perl 5.22's DOUBLE DIAMOND — `<>` without magic open — and
      # PPI hands it over as one Readline token whose inner text is `<>`.  It
      # is a readline, not a pattern: the old blacklist rule crashed on it
      # (an unbound CL symbol `<>`, io/argv.t's first failure note) and a
      # whitelist without this line would silently glob the string "<>".
      # A handle NAME is a perl identifier, and under `use utf8` that means
      # unicode word characters — `[^\W\d]\w*`, not `[A-Za-z_]\w*`.  An
      # ASCII-only test reads `<ＦＨ>` as a filename pattern and silently
      # globs it (caught by Pl/t/utf8-source-01.t's #418 bareword-filehandle
      # row, which is exactly the case this rule change could break).
      my $is_glob = defined $inner && $inner ne '' && $inner ne '<>'
                 && $inner !~ /\A[^\W\d]\w*(?:::[^\W\d]\w*)*\z/       # bareword handle
                 && $inner !~ /\A\$(?:[^\W\d]\w*::)*[^\W\d]\w*\z/;    # scalar handle

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
          $self->add_child_flattening($node_id, $interp_id, 'string_concat');
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

      # `<<>>` (the double diamond) reads ARGV like `<>`; the only difference
      # is that it does not honour a magic-open filename, which PCL's ARGV
      # readline does not do either.  Same node as `<>`: no children.
      if (defined $inner && $inner ne '' && $inner ne '<>') {
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

    # - - - Command execution: `command` or its qx// spellings?
    # PPI gives the backtick form its own token class and EVERY qx spelling a
    # PPI::Token::QuoteLike::Command, so accepting only the first left
    # `my $c = qx{echo hi}` with no primary at all — the statement was dropped
    # whole (task #369: 8 drops over the companion population, and $c silently
    # undef in any program that used the shape).  They are ONE term.
    #
    # The delimiter decides interpolation exactly as it does for q// vs qq//:
    # `qx'…'` does NOT interpolate, every other delimiter does.  PPI records it
    # in the token's section (`type` is the delimiter pair), which is where the
    # body comes from too — no re-lexing of the content string.
    if (ref($e1) eq 'PPI::Token::QuoteLike::Backtick'
     || ref($e1) eq 'PPI::Token::QuoteLike::Command') {
      say "parse(): Found backtick command"         if 1 & DEBUG;
      my ($cmd, $interpolating) = _command_body($e1);

      # Create a backtick node with the command as a string child
      my ($node, $node_id) = $self->make_node_insert('backtick');

      my $cmd_id;
      # Backticks interpolate like double-quoted strings
      if ($interpolating && $cmd =~ /[\$\@]/) {
        say "parse(): Backtick needs interpolation"  if 1 & DEBUG;
        my $str_token = PPI::Token::Quote::Double->new(qq{"$cmd"});
        $cmd_id = $self->str_interpol->parse_interpolated_string($self,
								 $str_token);
      } elsif ($interpolating) {
        $cmd_id = $self->make_node(PPI::Token::Quote::Double->new(qq{"$cmd"}));
      } else {
        # qx'…': the body is literal, so it must not travel as a double-quoted
        # token — a `$` in it would be interpolated by whoever reads it next.
        (my $esc = $cmd) =~ s/([\\'])/\\$1/g;
        $cmd_id = $self->make_node(PPI::Token::Quote::Single->new("'$esc'"));
      }
      $self->add_child_to_node($node_id, $cmd_id);

      say "parse(): Made backtick node $node_id"     if 1 & DEBUG;
      return $node_id;
    }

    # - - - Bareword (like filehandle FH, constant, or other bareword)?
    if (ref($e1) eq 'PPI::Token::Word') {
      my $name = $e1->content;

      # Check if this is a known zero-arg function (e.g., constant)
      if ($self->_is_zero_arg_func($name)) {
          # Zero-arg function — a funcall node whose ONLY child is the name.
          # That is the shape the rest of this file reads back ("Zero-param
          # funcall has exactly 1 child (the function name)", the `*`
          # filehandle-prototype post-pass below), and it is built the way
          # every other internal node is: make_node_insert + add_child_to_node.
          # It used to call a nonexistent `$self->add_node({type=>…})` — an
          # OpcodeTree method, never a PExpr one — so this branch ALWAYS died
          # with "Can't locate object method", the caller caught it, and the
          # statement was silently dropped (task #343: `sub FILE1 () { 1 }` +
          # `sub dummy { tell FILE1 }` — t/comp/parser.t).
          my ($node, $call_id) = $self->make_node_insert('funcall');
          my $func_id = $self->make_node($e1);
          $self->add_child_to_node($call_id, $func_id);
          say "parse(): Made funcall node $call_id for zero-arg function $name"
              if 1 & DEBUG;
          return $call_id;
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
      # For interpolated heredocs (<<"..." or <<BARE but not any of the raw
      # single-quoted spellings), route through the string interpolation system
      # so $var/@arr are expanded.  heredoc_is_raw is THE shared predicate —
      # this test used to be `/^<<'/` here, which read `<< 'E'` and `<<~'E'` as
      # interpolating and silently ate their variables (#301).
      if (! Pl::PExpr::TokenUtils::heredoc_is_raw($e1)) {
        my $inner = join('', $e1->heredoc());
        # Route through interpolation when there's something to interpolate OR
        # any escape sequence to collapse (\$ \@ \\ \n ...): the raw-literal
        # fallback below keeps backslashes verbatim, which is only correct for
        # escape-free text (closure.t END_MARK_ONE: all-escaped \$SIG heredoc).
        (my $tmp = $inner) =~ s/\\\\/\x00\x00/g;
        if ($tmp =~ /(?<!\\)[\$\@]/ or $inner =~ /\\/) {
          my $fake_str = PPI::Token::Quote::Double->new(qq{"$inner"});
          # Pass the real HereDoc token as origin so lexical feature lookup
          # (postderef_qq) sees the document position.
          return $self->str_interpol->parse_interpolated_string($self, $fake_str, $e1);
        }
      }
      my $id = $self->make_node($e1);
      say "parse(): Made heredoc node $id"           if 1 & DEBUG;
      return $id;
    }

    # - - - What else can it be?? :-)
    # NO `warn` here (task #339, ruled fable-answers-s400.md §6.2): this die is
    # a routine DECLINE — the term walker declines bare words, prefix operators
    # and a few token classes BY DESIGN, and callers catch it and take another
    # route.  An unconditional warn made 25 companion files print an
    # error-shaped line while compiling fine, which is the opposite of the
    # rule-12 discipline.  The EVENT worth announcing is the one place a
    # decline actually costs the program a statement: Pl/Parser.pm's two
    # `PARSE ERROR` emitters (_announce_dropped_statement) say so there, with
    # the file, line and source text this site does not have.
    die "Handle single node of unknown type. Dump:\n" . _dd($e1);
  }


  # - - - Pre-pass: collapse ${name}/@{name}/%{name}/$#{name} (a Cast sigil
  # followed by a Block whose sole content is a bare identifier) into the plain
  # variable token $name/@name/%name/$#name.  In Perl a *bareword* in a deref
  # block is NOT a symbolic ref — `${name}` is exactly `$name` (the lexical, or
  # the package var if no lexical), and `${name}[0]` is `$name[0]`.  Only a
  # quoted string (`${"name"}`) or an expression (`${$ref}`) is a real deref.
  # Rewriting the two tokens into one Symbol/ArrayIndex routes the whole thing
  # through the ordinary variable path, so lexicals resolve correctly and any
  # trailing subscript is handled by the normal element-access machinery (rather
  # than emitting a package-only symbolic `(p-cast-$ "name")`).
  $self->_collapse_braced_bareword_derefs($e);


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


  # - - - #153 FOLD (chunks 1-3): reduce embedded postfix-bearing terms to
  # nodes BEFORE the ()-replacement (the walker's `-> method ( args )` step
  # needs raw Lists) and before the arrow/subscript machinery below — which,
  # after this pass, reduces only WHOLE terms (its own recursive parse of a
  # folded term) plus the by-design word-led / block-led residue.  The
  # PCL_NO_FOLD A/B switch and the PCL_FOLD_PROBE instrument were deleted at
  # the chunk-3 flip (s398) once emission measured byte-identical with the
  # fold on and off over all four populations.
  $self->_fold_terms($e);

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
  #
  # THE CAST RUN (#305).  PPI hands a prefixed term as a FLAT run of Cast
  # tokens before the Symbol/Block: `@$$arr[0]` is Cast(@) Cast($) Symbol.
  # Perl's rule for such a run, in front of a subscript:
  #
  #   * the OUTERMOST (leftmost) cast decides the ACCESS KIND — `@` slice,
  #     `%` kv-slice, `$` element;
  #   * every INNER cast is one more deref applied to the BASE;
  #   * and with a real `->` arrow, the arrow supplies the access kind, so
  #     ALL the casts are derefs on the base:
  #     `$$$rrr->{k}` == `${${$rrr}}->{k}`.
  #
  # Both cast-consuming sites below used to look at exactly ONE token
  # (`$e->[$i-2]`).  With two or more casts the extra ones were left in the
  # stream, matched no case, and the "Missing case" die dropped the WHOLE
  # statement (`$$$rr{k}`, `$$$rrr->{k}`) or crashed the load (`@$$arr[0]`).

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

    die "WTF? :-) Expr starts with ->/brace??\n" . _dd($e) . "\n"
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
        # Leading scalar derefs bind WITH the ref target: $$r->() means
        # (${$r})->() — deref $r to the coderef, THEN call.  PPI hands us a flat
        # run of Cast('$') before the Symbol/Block, and without consuming it
        # here the casts would wrap the whole funcall instead — ${ $r->() }.
        # ALL of them belong to the target (#305): `$$$crr->(1)` is
        # `${${$crr}}->(1)`, so parse the whole run together with $pre.
        my $cast_s  = $self->_cast_run_start($e, $i-1);
        $cast_s     = $i-1
          if !$self->_all_scalar_casts($e, $cast_s, $i-2);
        my @pre_toks = (@$e[$cast_s .. $i-2], $pre);
        my $pre_id   = $self->parse(\@pre_toks);
        my $pst_id = $nxt->{id};
        my $kids   = $self->get_node_children($pst_id);

        my($node, $id) = $self->make_node_insert('ref_funcall');
        $self->add_child_to_node($id, $pre_id);   # Fun ref
        for my $kid_id (@$kids) {
          $self->add_child_to_node($id, $kid_id); # Parameters
        }

        # Replace [casts…, ref term, '->', param list] with the new node.
        # With no casts $cast_s == $i-1, so this is the plain two-token splice.
        $e->[$cast_s] = $node;
        splice @$e, $cast_s+1, ($i + 1) - $cast_s;
        my $np = $cast_s;        # the new node's position in @$e
        $i = $cast_s;
        # Implicit arrow after a call: $cr->(){k} / $cr->()[i] mean
        # $cr->()->{k} / $cr->()->[i].  PPI cannot tell these braces are
        # subscripts (they follow ')'), so it tags {k} as a Block and [i] as
        # a Constructor; left alone they match no case and the parse falls
        # through to the "Missing case" die.  Re-tag each DIRECTLY following
        # brace as a Subscript so the X[]/X{} machinery below chains them
        # exactly like the Subscripts in $x->[0]{k} (same fix pattern as
        # _retag_braced_deref_subscript).
        for (my $j = $np + 1; $j < scalar(@$e); $j++) {
          my $t = $e->[$j];
          if ((ref($t) eq 'PPI::Structure::Block'
               || ref($t) eq 'PPI::Structure::Constructor')
              && $t->start && $t->start->content =~ /^[\[{]$/) {
            bless $t, 'PPI::Structure::Subscript';
          } else {
            last;
          }
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

        _reduce_pre($e, \$i, $node, 3);

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
        _reduce_pre($e, \$i, $node, $count);
        next;
      } elsif ($self->is_word($nxt)) {
        # Case 1C: X->method (no parentheses)
        # Method call without arguments, e.g., $obj->DEBUG or $self->nodes
        my $pre_id = $self->parse([$pre]);
        my $meth_id = $self->make_node($nxt);  # Method name as node

        my($node, $id) = $self->make_node_insert('methodcall');
        $self->add_child_to_node($id, $pre_id);  # Object
        $self->add_child_to_node($id, $meth_id); # Method name

        _reduce_pre($e, \$i, $node, 2);  # Remove -> and method name

        next;
      } elsif (ref($nxt) eq 'PPI::Token::Cast'
               && $nxt->content() =~ /^([\$@%])\*$/) {
        # Postfix deref: X->$* (scalar), X->@* (array), X->%* (hash) — Perl 5.20+
        # Equivalent to $$X, @$X, %$X respectively.
        my $sigil = $1;
        my $node  = $self->_prefix_op_node(PPI::Token::Cast->new($sigil),   # Cast sigil ($, @, or %)
                                           $self->parse([$pre]));           # Ref being dereferenced
        _reduce_pre($e, \$i, $node, 2);  # Remove -> and Cast($*/\@*/\%*)
        next;
      } elsif (ref($nxt) eq 'PPI::Token::Cast'
               && $nxt->content() eq '$#*') {
        # Postfix deref: X->$#* — last index of an arrayref (Perl 5.20+).
        # Equivalent to $#{X}; build the same $# prefix_op the braced form uses
        # ($# op token + ref operand) so codegen emits (p-array-last-index X).
        my $node = $self->_prefix_op_node(PPI::Token::Cast->new('$#'),   # $# operator
                                          $self->parse([$pre]));         # Arrayref being dereferenced
        _reduce_pre($e, \$i, $node, 2);  # Remove -> and Cast($#*)
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
        # Flatten comma-separated indices/keys into separate children
        $self->add_child_flattening($id, $ix_id, 'progn');
        _reduce_pre($e, \$i, $node, 3);  # Remove ->, Cast(@/%), and the subscript
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

        _reduce_pre($e, \$i, $node, 2);  # Remove -> and $variable

        next;
      } else {
        my $fn = eval { $self->parser->filename } // '(unknown)';
        die "PExpr: unhandled postfix '->' term in $fn: "
          . "term=" . _dd($term) . " next=" . _dd($nxt)
          . " next2=" . _dd($nxt_2) . "\n";
      }
    }

    if ($self->is_arr_or_hash_braces($term)) {
      # X[] or X{}
      # #211: with a REAL arrow ($is_reference), a leading scalar-deref cast
      # binds WITH the ref target — `$$rr->{k}` == `(${$rr})->{k}`, deref
      # $rr FIRST, then the arrow derefs THAT value (same rule Case 2 gives
      # `$$r->()`).  Parse [Cast, $pre] together as the base; the cast-removal
      # after the node build then consumes a cast that is genuinely part of
      # the base, instead of silently swallowing a deref level (the arrow
      # splice had turned `$$rr->{k}` into `$$rr{k}`, one level short).
      # Without the arrow the cast IS the one deref the *_ref_acc node
      # encodes ($$scalar{key}), so nothing changes there.
      # The cast run before the base (#305).  $n_casts == 0 or 1 reproduces
      # the pre-#305 behaviour exactly; 2+ is what used to be dropped.
      my $cast_s   = $self->_cast_run_start($e, $i-1);
      my $n_casts  = $i - 1 - $cast_s;
      my $base_ok  = (!$self->is_internal_node_type($pre)
                      && $self->is_var($pre) && $pre->content() =~ /^\$/)
                  || (ref($pre) eq 'PPI::Structure::Block'
                      && $pre->start() eq '{');

      # How many of the casts fold INTO the base parse as deref levels:
      #   arrow    — all of them (the arrow supplies the access kind);
      #   no arrow — all but the OUTERMOST, which is the access kind and is
      #              the one deref the *_ref_acc / slice node itself encodes.
      my $base_casts = 0;
      if ($base_ok && $n_casts) {
        if ($is_reference) {
          $base_casts = $n_casts
            if $self->_all_scalar_casts($e, $cast_s, $i-2);
        } elsif ($n_casts >= 2) {
          $base_casts = $n_casts - 1
            if $self->_all_scalar_casts($e, $cast_s+1, $i-2);
        }
      }
      my $pre_id = $base_casts
                 ? $self->parse([@$e[$i-1-$base_casts .. $i-2], $pre])
                 : $self->parse([$pre]);
      my $pre_n = $self->get_a_node($pre_id);

      # The cast that DECIDES the access kind is the outermost one.  When the
      # run is 0 or 1 long this is the same token the pre-#305 code read at
      # $i-2.
      my $outer_cast = $n_casts ? $e->[$cast_s] : undef;

      my $type  = ($self->is_arr_braces($term) ? "a_acc" : "h_acc");
      # If it was X->[] or X->{}:
      if ($is_reference) {
        $type   = ($self->is_arr_braces($term) ? "a_ref_acc" : "h_ref_acc");
      } elsif (ref($pre) eq 'PPI::Structure::Block'
               && $pre->start() eq '{'
               && $outer_cast
               && ($outer_cast->content() eq '@'
                   || $outer_cast->content() eq '$')) {
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
        $type = $outer_cast->content() eq q{@}
              ? ($self->is_arr_braces($term) ? "slice_a_acc" : "slice_h_acc")
              : ($self->is_arr_braces($term) ? "a_ref_acc"   : "h_ref_acc");
      } elsif ($base_casts
               || ($self->is_var($pre_n) && $pre_n->content() =~ /^\$/)) {
        # Check for $$scalar[n] / $$scalar{key} (Cast '$') or
        # @{$hashref}{keys} / @$scalar{keys} (Cast '@') patterns.
        #
        # $base_casts alone also qualifies: once inner casts are folded into
        # the base (`@$$arr[0,1]`), $pre_n is a cast node, not the Symbol the
        # is_var test wants — but the OUTERMOST cast still decides the access
        # kind by exactly this mapping.  Without this the type silently fell
        # back to a plain a_acc and the slice returned one element (#305).
        my $cast_before = $outer_cast;
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
        # Skip the 'progn' for slices:
        $self->add_child_flattening($id, $ix_id, 'progn');
      } else {
        $self->add_child_to_node($id, $ix_id);
      }

      # Replace $pre with the new node, remove the subscript term.
      $e->[$i-1] = $node;
      splice @$e, $i, 1;         # Remove $term (subscript)

      # Remove the casts this node ACCOUNTED for, so none is applied again as
      # a prefix p-cast-$ on the result.  That is the ones folded into the
      # base plus, when the outermost cast chose the access kind, that one
      # too.  A cast the node did NOT account for (a run whose inner casts
      # were not all '$', so $base_casts stayed 0) is deliberately left in
      # the stream to fail loudly rather than silently lose a deref level.
      # $base_casts > 0 means a widened path ran, and both of those account
      # for the WHOLE run: the arrow path folds every cast into the base, the
      # no-arrow path folds all but the outermost and spends that one on the
      # access kind.  Otherwise it is the pre-#305 single-cast test.
      my $consumed = $base_casts ? $n_casts
        : ($outer_cast
           && ((($type eq 'slice_a_acc' || $type eq 'slice_h_acc'
                 || $type eq 'kv_slice_a_acc' || $type eq 'kv_slice_h_acc')
                && ($outer_cast->content() eq '@'
                    || $outer_cast->content() eq '%'))
               || (($type eq 'a_ref_acc' || $type eq 'h_ref_acc')
                   && $outer_cast->content() eq '$'))) ? 1 : 0;
      splice @$e, $i-1-$consumed, $consumed if $consumed;
      $i = $i - 1 - $consumed;
      next;
    }

    # Handle KV slice: %hash{keys} - PPI gives Symbol '%h' + Block '{keys}'
    # (unlike @h{keys} which gives Subscript)
    if (ref($term) eq 'PPI::Structure::Block'
        && $term->start() eq '{'
        && !$self->is_internal_node_type($pre)
        && $self->is_var($pre)
        && $pre->content() =~ /^%/) {
      my $node = $self->_kv_slice_node('kv_slice_h_acc', $self->parse([$pre]), $term);
      _reduce_pre($e, \$i, $node, 1);
      next;
    }

    # Handle KV array slice: %arr[indices] - PPI gives Symbol '%arr' + Constructor '[...]'
    if ($is_kv_arr_constructor) {
      my $node = $self->_kv_slice_node('kv_slice_a_acc', $self->parse([$pre]), $term);
      _reduce_pre($e, \$i, $node, 1);
      next;
    }

    # Handle KV array slice via block-deref: %{$ref}[indices]
    if ($is_kv_arr_deref_constructor) {
      my @block_kids = $e->[$i-1]->children();
      my $node = $self->_kv_slice_node('kv_slice_a_acc', $self->parse(\@block_kids), $term);
      $e->[$i-2] = $node;   # Replace Cast '%' position with node
      splice @$e, $i-1, 2;  # Remove Block and Constructor
      $i -= 2;
      next;
    }

    # Handle KV hash slice via block-deref: %{$ref}{"keys"} - Cast('%') + Block('{ref}') + Block('{"keys"}')
    # e.g., %{$h}{"c","d"} -> (p-kv-hslice $h "c" "d")
    if ($is_kv_hash_deref_block) {
      my @block_kids = $e->[$i-1]->children();
      my $node = $self->_kv_slice_node('kv_slice_h_acc', $self->parse(\@block_kids), $term);
      $e->[$i-2] = $node;   # Replace Cast '%' position with node
      splice @$e, $i-1, 2;  # Remove Block and Block
      $i -= 2;
      next;
    }

    # Dynamic typeglob slot *{EXPR}{SLOT} — Cast('*') + Block + Block(SLOT).
    # UNREACHABLE: _precollapse_dyn_glob_slots reduces exactly this triple (and
    # its `*$var{SLOT}` sibling) at parse() entry, before this loop, and nothing
    # in the loop manufactures a fresh Block; measured ZERO firings over the
    # perl-tests corpus, perl's t/, the 14-dist board, lib/ and every `*{` file
    # of 108 CPAN dists (#153 FOLD chunk 3, s398).  A firing here would mean the
    # pre-pass and this predicate disagree — say so (rule 12); never reduce the
    # same shape a second way.
    if ($is_dyn_typeglob_slot) {
      die "PCL: internal: dynamic glob-slot *{EXPR}{SLOT} reached the arrow "
        . "loop unreduced (pre-pass missed it): "
        . _dd([ @$e[$i-2 .. $i] ]) . "\n";
    }

    # Handle typeglob slot access: *name{SLOT} — PPI gives Symbol '*name' + Block '{SLOT}'
    # e.g., *_{ARRAY} -> (p-glob-slot (p-make-typeglob "main" "_") "ARRAY")
    if ($is_typeglob_slot) {
      my $glob_id = $self->parse([$pre]);
      my($node, $id) = $self->make_node_insert('glob_slot');
      $self->add_child_to_node($id, $glob_id);
      # Slot: literal bareword (*name{CODE}), scalar var, string, or expression.
      $self->_attach_glob_slot($id, $node, $term);
      _reduce_pre($e, \$i, $node, 1);
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
      _reduce_pre($e, \$i, $node, 1);
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

      _reduce_pre($e, \$i, $node, 1);

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

    # Adjacent prefix operators must reduce INNER-first, regardless of their
    # precedence numbers: `!-e $x` is `!(-e $x)` and `! -d $x` is `!(-d $x)`.
    # The loop picks the highest-precedence op, which for `! -e` is `!` (90) —
    # but `!`'s operand is the not-yet-reduced `-e $x`, so reducing `!` first
    # strands `$x` and the parse falls through.  When the selected op is a
    # prefix operator whose right neighbour is ALSO a prefix operator, walk to
    # the RIGHTMOST operator in that consecutive run and reduce it first.
    if ($self->_is_prefix_op_token($op)) {
      my $j = $hi_ix;
      while ($j + 1 < scalar(@$e)
             && $self->_is_prefix_op_token($e->[$j + 1])) {
        $j++;
      }
      if ($j != $hi_ix) {
        $hi_ix   = $j;
        $op      = $e->[$j];
        $op_info = $self->op_info($op);
        $op_name = $self->is_token_operator($op);
        $hi_prio = $op_info->{prec};
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

# say _dd $e; say "---"; say _dd $op_info; say _dd $self->node_tree; exit 0;


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
          ", ix $hi_ix.\nParam before:", _dd($prev),
          "\nParam after:", _dd($post), "\n======"
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
        die "Ternary operator: Found '?' but no matching ':'\n" . _dd($e);
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
        my $node = $self->_filetest_prefix_node($op, $op_name, $post, $id_term)
                // $self->_prefix_op_node($op, $id_term);

        $e->[$hi_ix] = $node;
        splice @$e, $hi_ix+1, 1;
        next;
      }

    }

    die "Unknown. Bug. op=" . _dd($op) . " info=" . _dd($op_info);
  }

  if (scalar(@$e) == 1 && $self->is_internal_node_type($e->[0])) {
    return $self->id_of_internal_node($e->[0]);
  }

  # Single atomic element (number, string, variable, etc.)
  if (scalar(@$e) == 1) {
    return $self->make_node($e->[0]);
  }

  die "Bug. Fell through. Missing case: " . _dd($e);
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

  # Perl precedence: ',' binds TIGHTER than the low-precedence logical
  # operators (perlop: not > and > or/xor, and all of them are looser than
  # ',').  These streams reach us comma-first, so a top-level and/or/xor owns
  # the whole list: `A and B, C` is `A and (B, C)`, and f(1, 2 and 3, 4)
  # calls f(3, 4).  Reduce the loosest such operator (rightmost among equals
  # — left assoc) BEFORE any comma split; each side re-enters parse(), where
  # remaining commas become the usual progn/list handling.  A bare prefix
  # `not` with commas after it swallows the tail the same way — (1, not 0, 2)
  # is (1, not(0, 2)) — EXCEPT when its operand is parenthesized: `not(0), 5`
  # is a func-call-style not((0)) followed by 5.
  {
    my %LOGICAL_PREC = (or => 1, xor => 1, and => 2);
    my ($lo_ix, $lo_prec, $not_ix);
    for (my $i = 0; $i < scalar(@$e_list); $i++) {
      my $tok = $e_list->[$i];
      next if ref($tok) ne 'PPI::Token::Operator';
      my $op  = $tok->content();
      # `and => 1` — the fat comma auto-quotes the word; a key, not an op.
      my $next = $e_list->[$i+1];
      next if $next && ref($next) eq 'PPI::Token::Operator'
                    && $next->content() eq '=>';
      if (exists $LOGICAL_PREC{$op} && $i > 0) {
        if (!defined $lo_prec || $LOGICAL_PREC{$op} <= $lo_prec) {
          ($lo_ix, $lo_prec) = ($i, $LOGICAL_PREC{$op});
        }
      } elsif ($op eq 'not' && !defined $not_ix && !$self->is_list($next)) {
        $not_ix = $i;
      }
    }
    if (defined $lo_ix) {
      my @left  = @$e_list[0 .. $lo_ix - 1];
      my @right = @$e_list[$lo_ix + 1 .. $#$e_list];
      my $id_l  = $self->parse(\@left);
      my $id_r  = $self->parse(\@right);
      my $n_id  = $self->make_node($e_list->[$lo_ix]);
      $self->add_child_to_node($n_id, $id_l);
      $self->add_child_to_node($n_id, $id_r);
      return [$n_id];
    }
    if (defined $not_ix
        && grep { ref($_) eq 'PPI::Token::Operator' && $_->content() eq ',' }
                @$e_list[$not_ix + 1 .. $#$e_list]) {
      my @operand = @$e_list[$not_ix + 1 .. $#$e_list];
      my $id_term = $self->parse(\@operand);
      my $node    = $self->_prefix_op_node($e_list->[$not_ix], $id_term);
      splice @$e_list, $not_ix, scalar(@$e_list) - $not_ix, $node;
      # fall through to the ordinary comma split with the reduced tail
    }
  }

  # 1. Split into list with ","-separated. Eval them
  say "Parts in list:\n", _dd $e_list         if 4 & DEBUG;
  my $parts     = $self->parse_comma_separated_list($e_list);
  say "Split into list:\n", _dd $parts        if 4 & DEBUG;

  # 2. Call the parts recursively to parse()
  my @node_ids;
  for my $e_part (@$parts) {
    # Skip empty parts (can happen with leading commas)
    next if !@$e_part;
    say "Parse this:", _dd $e_part            if 4 & DEBUG;
    my $id      = $self->parse($e_part);
    say "  ==> id: ", _dd $id                 if 4 & DEBUG;
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
#            | -> <funcall>                    (method call whose `name(args)` handle_subcalls
#                                                already reduced — #153 chunk 3, W1)
#            | -> ( args )                       (coderef call: raw List, or the <tree_val> the
#                                                ()-replacement makes of it — chunk 3, W2)
#            | -> $#*                             (postfix last-index — chunk 3, W3)
#            | -> ${ EXPR }                        (computed method name — chunk 3, W7;
#                                                its `( args )` are taken by _term_extent)
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
    } elsif ($self->is_internal_node_type($after)
             && ($after->{type} // '') eq 'funcall') {
      $end += 2;                                    # -> <funcall> (method + args, W1)
    } elsif (ref($after) eq 'PPI::Structure::List'
             || ($self->is_internal_node_type($after)
                 && ($after->{type} // '') eq 'tree_val')) {
      $end += 2;                                    # -> ( args )  coderef call (W2)
    } elsif (ref($after) eq 'PPI::Token::Cast'
             && $after->content() eq '$'
             && $end + 3 < $n
             && ref($e->[$end + 3]) eq 'PPI::Structure::Block') {
      $end += 3;                                    # -> ${ EXPR }  computed method (W7)
    } elsif (ref($after) eq 'PPI::Token::Cast'
             && $after->content() eq '$#*') {
      $end += 2;                                    # -> $#*  (W3)
    } else {
      last;
    }
  }
  return $end;
}

# ---------------------------------------------------------------------------
# #153 / E5.0 (Option B, docs/pexpr-term-parsing-review.md) — phase 1 pieces.
#
# _term_extent(\@e, $start, $limit) is the ONE walker that knows the term
# grammar:
#
#   term := cast* primary postfix*
#
# It returns the (inclusive) index of the last element of the term starting
# at $start — never past $limit — or undef when the walker cannot bound the
# term CONFIDENTLY.  undef is a first-class answer, not an error: a bare
# word (list operator? filehandle? class name? constant?), a prefix
# operator, or anything else outside the grammar above stays with the call
# site's legacy derivation.  The walker answers only when the tokens match
# the grammar; it never guesses, so switching a site to it can only replace
# hand-derived boundaries with grammar-derived ones, never widen coverage
# silently.
sub _term_extent {
  my ($self, $e, $start, $limit) = @_;
  my $n = scalar(@$e);
  $limit = $n - 1 if !defined($limit) || $limit > $n - 1;
  return undef if $start < 0 || $start > $limit;

  # cast*: sigil/reference casts ($ @ % & * \ $#) before the primary.
  my $i = $start;
  $i++ while $i < $limit && ref($e->[$i]) eq 'PPI::Token::Cast';
  return undef if ref($e->[$i]) eq 'PPI::Token::Cast';  # cast, no primary

  my $p = $e->[$i];
  my $r = ref($p);
  my $end;
  if ($r eq 'PPI::Token::Symbol'
      || $r eq 'PPI::Token::Magic'
      || $r eq 'PPI::Token::ArrayIndex'
      || $r =~ /^PPI::Token::Number/
      || $r =~ /^PPI::Token::Quote::/
      || $r =~ /^PPI::Token::QuoteLike::/
      || $r eq 'PPI::Token::HereDoc'
      || $r eq 'PPI::Structure::Block'
      || $r eq 'PPI::Structure::Constructor'
      || $r eq 'PPI::Structure::List'
      # PPI labels the leading `(…)` of a postfix-conditional CONDITION a
      # Structure::Condition (`return X if (A)->[0] ne 'tag'`); in an
      # expression it is just a parenthesised group — the same equivalence the
      # single-element site draws (#153 chunk 3, W8).
      || $r eq 'PPI::Structure::Condition') {
    $end = $i;
  } elsif ($self->is_internal_node_type($p)) {
    $end = $i;                       # already-reduced node
  } elsif ($self->is_word($p)) {
    # A word is a self-bounded term ONLY as `name(...)` — a call with
    # parens — or as `Name->…` — a word DIRECTLY followed by an arrow is the
    # invocant / callee of that arrow (class name, `shift->m`, `__PACKAGE__->m`,
    # `foo->[0]`) and nothing else: not a filehandle, not a list operator, not
    # an `=>` autoquote (#153 chunk 3, W5 — measured the second-largest
    # embedded population the fold left to the legacy loop).  WHAT the word
    # means is still decided by the reduction (the same branches as before);
    # the walker only bounds it.  Anything else about barewords is not this
    # walker's call.
    if ($i + 1 <= $limit && ref($e->[$i + 1]) eq 'PPI::Structure::List') {
      $end = $i + 1;
    } elsif ($i + 1 <= $limit && $self->is_arrow_op($e->[$i + 1])) {
      $end = $i;
    } else {
      return undef;
    }
  } else {
    return undef;                    # operator, regex-op, anything else
  }

  # LIST SLICE: a `[..]` group directly after a parenthesised List or a qw()
  # word list is a slice of that list — `(f())[0]`, `(1,2,3)[1]`, `qw(a b)[1]`.
  # PPI hands it over as a Constructor (it classifies `[..]` by predecessor),
  # so the postfix walker below, which knows only Subscripts, would stop
  # before it.  One group, then the ordinary chain continues (`(…)[0]->{k}`).
  # (#153 chunk 3, W4 — CtorSub firings over four populations were all this
  # shape; perl rejects `f()[0]` and `$o->m()[0]`, so a Constructor after a
  # funcall/methodcall node is deliberately NOT taken.)
  if ($end == $i && ($r eq 'PPI::Structure::List'
                    || $r eq 'PPI::Structure::Condition'
                    || $r eq 'PPI::Token::QuoteLike::Words')
      && $end + 1 <= $limit
      && ref($e->[$end + 1]) eq 'PPI::Structure::Constructor'
      && $e->[$end + 1]->start() eq '[') {
    $end++;
    # …and any further ARROW-LESS `[j]` groups on the slice, which PPI also
    # labels Constructor by predecessor — `([…])[0][1]` (W9; the ref.t
    # list-slice deref rows).  A `{k}` after the `]` arrives as a Block and is
    # re-labelled a Subscript by _retag_list_slice_subscripts before the
    # walker runs, so the ordinary chain below takes it.
    while ($end + 1 <= $limit
           && ref($e->[$end + 1]) eq 'PPI::Structure::Constructor'
           && $e->[$end + 1]->start() eq '[') {
      $end++;
    }
  }

  # GLOB SLOT: *name{SLOT} arrives as Symbol(*name) + Block({SLOT}) — PPI does
  # not attach the group as a Subscript after a glob sigil.  One postfix group,
  # and the chain CONTINUES: a slot yields a value (`*STDOUT{IO}->autoflush`).
  # (#153 chunk 3, W6 — the Glob branch's embedded firings over four
  # populations were all this shape.  `*{EXPR}{SLOT}` / `*$v{SLOT}` never
  # reach here: _precollapse_dyn_glob_slots reduces them before the loop.)
  if ($end == $i && $r eq 'PPI::Token::Symbol' && $p->content() =~ /^\*/
      && $end + 1 <= $limit
      && ref($e->[$end + 1]) eq 'PPI::Structure::Block'
      && $e->[$end + 1]->start() eq '{') {
    $end++;
  }

  # KV-slice postfix: %h{a,b} / %h[0,1] arrive as Symbol + Block/Constructor
  # (PPI does not attach these as Subscript).  One group, no further chain —
  # a KV slice yields a list; nothing postfixes it.
  if ($end == $i && $i == $start && $r eq 'PPI::Token::Symbol'
      && $p->content() =~ /^%/ && $end + 1 <= $limit) {
    my $nx = $e->[$end + 1];
    if ((ref($nx) eq 'PPI::Structure::Block' && $nx->start() eq '{')
        || (ref($nx) eq 'PPI::Structure::Constructor' && $nx->start() eq '[')) {
      return $end + 1;
    }
  }

  # postfix*: subscripts and -> groups, via the single shared chain walker.
  # The chain walker's `-> method` step consumes only the NAME — the args of a
  # method call are a separate postfix step, taken here (#153 step 4a) because
  # the walker is the only caller that wants them: a parenthesised List
  # directly after a method name IS that call's argument list, and the chain
  # continues past it (`$o->m(1)->n(2)[0]`).  Before step 4 this shape made the
  # walker DECLINE rather than stop in the middle of a method call.
  while (1) {
    my $next = $self->_extend_postfix_chain($e, $end);
    if ($next > $end
        && $next + 1 <= $limit
        && ref($e->[$next + 1]) eq 'PPI::Structure::List'
        && ($self->is_arrow_op($e->[$next - 1])
            # -> ${ EXPR } ( args ): the name step is Cast+Block (W7)
            || ($next >= 2
                && ref($e->[$next]) eq 'PPI::Structure::Block'
                && ref($e->[$next - 1]) eq 'PPI::Token::Cast'
                && $self->is_arrow_op($e->[$next - 2])))) {
      $end = $next + 1;                # -> method ( args )
      next;
    }
    $end = $next;
    last;
  }
  return undef if $end > $limit;

  # A Block/Constructor group directly after a cast-deref term is PPI's
  # spelling of a SLICE on the deref (`@{$r}[0]`, `%{$h}{a}`) — a Subscript
  # everywhere else, but PPI classifies it by what precedes it, and a `}` or a
  # cast-deref does not qualify.  It is one postfix group and the term ends
  # there: a slice yields a list, and nothing postfixes a list.  (#153 step 4b;
  # the walker used to decline here rather than stop inside the term.)
  if ($i > $start && $end + 1 <= $limit
      && (ref($e->[$end + 1]) eq 'PPI::Structure::Constructor'
          || ref($e->[$end + 1]) eq 'PPI::Structure::Block')) {
    return $end + 1;
  }
  return $end;
}

# Extend an operand end index rightward through high-precedence binary
# operators (prec >= 55: . + - * / % x ** =~ !~ << >>) and their operands,
# stopping before comparison/logical/comma/assignment.  This is the
# named-unary precedence rule (`length $s + 1` == length($s + 1)); it is
# OPERATOR knowledge, deliberately separate from _term_extent's term
# grammar.  Factored from the two identical in-line walks at the
# named-unary operand site.
sub _extend_high_prec {
  my ($self, $e, $j) = @_;
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
  return $j;
}

# MEASUREMENT (#153, PCL_TERM_DECL=1): report every operand-site consultation
# the walker DECLINED, with the token run.  That inventory is what says which
# shapes still reach a site's fallback — the argument behind step 5's deletions
# and their `die` guards — so it stays live for whoever widens the walker next.
# Read it with tools/term-diff-sweep.pl over BOTH populations (the 111-file
# corpus alone is not enough; s361).
sub _term_probe {
  my ($self, $site, $fn, $e, $i, $ceiling, $ans) = @_;
  return if defined $ans;
  warn sprintf "PCL_TERM_DECL %s fn=%s toks=[%s]\n", $site, $fn,
      $self->_tok_run_desc($e, $i, $ceiling);
}

# Debug/probe helper: describe a token run compactly for PCL_TERM_DIFF logs.
sub _tok_run_desc {
  my ($self, $e, $from, $to) = @_;
  $to = scalar(@$e) - 1 if $to > scalar(@$e) - 1;
  my @out;
  for my $t (@$e[$from .. $to]) {
    if ($self->is_internal_node_type($t)) {
      push @out, '<' . ($t->{type} // 'node') . '>';
    } else {
      my $c = eval { $t->content } // '?';
      $c = substr($c, 0, 20) . '…' if length($c) > 21;
      my ($short) = (ref($t) =~ /([^:]+)$/);
      push @out, "$short($c)";
    }
  }
  return join(' ', @out);
}

# _reduce_term(\@e, $start, $limit) — reduce the term starting at $start to
# ONE parsed node.  Returns ($node_id, $next_index), or the empty list when
# _term_extent cannot bound the term.  The slice is reduced by the same
# recursive parse the operand sites use for their ranges today, so the node
# is identical to what the legacy derivations produce for the same range.
sub _reduce_term {
  my ($self, $e, $start, $limit) = @_;
  my $end = $self->_term_extent($e, $start, $limit);
  return () if !defined $end;
  my $node_id = $self->parse([ @$e[$start .. $end] ]);
  return ($node_id, $end + 1);
}

# --- #153 FOLD, chunk 1: phase-1 term reduction in the main loop -----------
#
# Reduce every POSTFIX-BEARING term whose primary the walker can claim into
# ONE node, in place, BEFORE the opportunistic arrow/subscript machinery
# runs.  After this pass the legacy loop sees a single already-reduced node
# where it used to hand-walk `$h -> {a} [2]` token by token; the reduction
# itself is _reduce_term's recursive parse of exactly the term's tokens, so
# the node is built by the SAME builder branches the in-place walk uses for
# the same tokens — byte-identical emission is the measured expectation, not
# a hope (corpus + suite emission compare, s375).
#
# Deliberately NOT folded here (they stay with the legacy loop, later
# chunks):
#   * a WORD-led term — indirect-object syntax, list operators, filehandles
#     and `=>` autoquote all read the raw word and its neighbours;
#   * a BLOCK-led term — a `{…}` after grep/map/sort/eval/do/sub is that
#     word's BLOCK ARGUMENT, not a hash-constructor primary; the
#     block-vs-constructor question belongs to handle_subcalls
#     (_ctor_deref_verdict, chunk 2 — s389);
#   * a term followed by a Block/Constructor group — the legacy loop has
#     combining rules that read the raw pair (indirect method args
#     `$o->SUPER::m{@a}`, kv-slice spellings); folding the left side would
#     blind them.
#
# The whole-array guard is load-bearing: _reduce_term parses the subrange
# through this same function, so claiming ALL of @$e would recurse forever.
# A term that IS the whole expression is exactly what the legacy machinery
# already reduces top-down; the fold's job is only the embedded case.
sub _fold_terms {
  my ($self, $e) = @_;
  for (my $i = 0; $i < scalar(@$e); $i++) {
    my $t = $e->[$i];
    my $r = ref($t);
    my $is_start =
         $r eq 'PPI::Token::Cast'
      || $r eq 'PPI::Token::Symbol'
      || $r eq 'PPI::Token::Magic'
      || $r eq 'PPI::Token::ArrayIndex'
      || $r eq 'PPI::Structure::List'
      || $r eq 'PPI::Structure::Condition'      # PPI's postfix-if paren label (W8)
      || $r eq 'PPI::Structure::Constructor'
      || $self->is_internal_node_type($t)
      # #153 chunk 3 (W5): a quoted string or a qw() list can head a postfix
      # chain (`"Class"->new`, `qw(a b)[1]`), and so can a WORD that is
      # DIRECTLY followed by an arrow (`Foo->new(...)`, `shift->m`,
      # `__PACKAGE__->m`) — the arrow makes the word an invocant, never a
      # filehandle / list operator / `=>` key.  All other words stay with the
      # main loop, by design (s364).
      || $r =~ /^PPI::Token::Quote::/
      || $r eq 'PPI::Token::QuoteLike::Words'
      || ($self->is_word($t) && $i + 1 < scalar(@$e)
          && $self->is_arrow_op($e->[$i + 1]));
    next if !$is_start;
    # A position PRECEDED by a cast or an arrow is the middle of some term,
    # never a start: folding the tail alone re-binds the subscript under the
    # wrong reading (`$$r[0]` would become `${ $r[0] }`; `$o->$m(...)`'s $m
    # would become an @m element).  The proper start (the cast / the chain
    # head) either claimed this position already or was guarded off.
    if ($i > 0) {
      my $prev = $e->[$i - 1];
      next if ref($prev) eq 'PPI::Token::Cast'
           || $self->is_arrow_op($prev);
      # A Constructor is a genuine anon-array primary only where nothing
      # before it could own it as a POSTFIX group.  PPI classifies `[...]`
      # by its predecessor, and after a `)` (list slice `(...)[0]`), a qw
      # list, or an already-reduced node it hands over a Constructor that is
      # really a SUBSCRIPT — folding `[0]->{k}` alone would orphan the term
      # it subscripts (found live: ref.t list-slice rows, grep.t deref-map).
      # Only an operator (or expression start) guarantees primary position.
      next if $r eq 'PPI::Structure::Constructor'
           && ref($prev) ne 'PPI::Token::Operator';
    }
    my $end = $self->_term_extent($e, $i, undef);
    next if !defined $end;
    next if $i == 0 && $end == scalar(@$e) - 1;      # whole array: recursion guard
    # Postfix-bearing only: a bare `$x` / `@$x` / `(…)` term is left as raw
    # tokens — the machinery reads those shapes (content, sigils) all over.
    my $p = $i;
    $p++ while ref($e->[$p]) eq 'PPI::Token::Cast';
    next if $end <= $p;
    # Combining-rule guard: raw Block/Constructor after the term.
    if ($end + 1 < scalar(@$e)) {
      my $after = ref($e->[$end + 1]);
      next if $after eq 'PPI::Structure::Block'
           || $after eq 'PPI::Structure::Constructor';
    }
    my ($node_id) = $self->_reduce_term($e, $i, undef);
    next if !defined $node_id;
    # The tree stores an internal node's PPIreference wrapper as its data —
    # that wrapper (with its {type}) is what belongs in the token array.  A
    # non-wrapper result (the parse reduced to a raw token) gets a fresh one.
    my $node = $self->get_a_node($node_id);
    $node = $self->make_subtree_item($node_id)
        if !$self->is_internal_node_type($node);
    splice @$e, $i, $end - $i + 1, $node;
  }
  return;
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

# The SAME PPI habit one group further along: after a LIST SLICE `(…)[i]` /
# `qw(…)[i]` — where the `[i]` is itself a Constructor by predecessor (see
# W4 in _term_extent) — a following arrow-less `{k}` arrives as a Structure::
# BLOCK, again only because a `]` precedes it.  It is a hash SUBSCRIPT on the
# slice (`({foo=>"bar"})[0]{foo}`, `(f())[0]{k}`), and left as a Block it
# matched no case: the statement fell through to the "Missing case" die and
# became a PARSE ERROR comment (a #138-family silent drop, s398).  Re-tag it
# a Subscript so the ordinary `<node>{k}` chain machinery (the path
# `$h{a}{b}` takes) handles it, and the term walker sees a plain chain.  A
# following `[j]` Constructor is left alone: CtorSub already reduces
# `<node>[j]` and it works today (`([qw/a b/])[0][1]`); the walk continues
# past it so `(…)[0][1]{k}` is caught too.
sub _retag_list_slice_subscripts {
  my ($self, $e) = @_;
  for (my $i = 1; $i < scalar(@$e); $i++) {
    next unless ref($e->[$i]) eq 'PPI::Structure::Constructor'
             && $e->[$i]->start() eq '[';
    my $r = ref($e->[$i-1]);
    next unless $r eq 'PPI::Structure::List'
             || $r eq 'PPI::Structure::Condition'
             || $r eq 'PPI::Token::QuoteLike::Words';
    my $j = $i + 1;
    while ($j < scalar(@$e)) {
      my $g = $e->[$j];
      if (ref($g) eq 'PPI::Structure::Block' && $g->start() eq '{') {
        bless $g, 'PPI::Structure::Subscript';
      } elsif (!(ref($g) eq 'PPI::Structure::Constructor' && $g->start() eq '[')) {
        last;
      }
      $j++;
    }
    $i = $j - 1;
  }
}

# #411 (task #153 / B3.1): a `(args)` list DIRECTLY after a completed postfix
# element is an ELIDED-ARROW CALL of that element's result — perl lets you drop
# the `->` between chain links, so `$a[0](1)`, `$s2->()()`, `(sub{})[0]()` and
# `$r->{m}()` all call the coderef the left side yields.  PPI hands the trailing
# `(...)` over as a plain Structure::List with no operator before it, so the
# arrow/subscript reducer (Case 2, `X->(...)`) never fires and the statement
# DROPPED ("Bug. Fell through. Missing case: [").  Rather than teach the reducer
# a second call spelling, this pass makes the elided arrow EXPLICIT — exactly as
# _retag_* normalize PPI's predecessor-classified braces — so the ONE existing
# `-> ( args )` path (walker W2 + reduction Case 2) handles every shape.
#
# A List is an elided call iff its predecessor is a COMPLETED postfix element
# that yields a value, never the bare primary: a Subscript (`$a[0]`, `$h{k}`,
# `$x->{m}`), a `-> ( )` call result (a List whose own predecessor is `->`), or
# a list-slice (a Constructor `[` preceded by a List/Condition/qw primary — the
# same discriminator _retag_list_slice_subscripts and _term_extent W4 use).  A
# List after a bare Symbol (`$foo(1)` — not a call in perl), a Word (`func(1)` —
# the word's own args), an arrow (`$x->(1)` — already explicit) or a Cast is
# left alone.  Building a fresh list makes the insertion cascade: `$a[0]()(0)`
# becomes `$a[0]->()->(0)` because the `->` inserted before the first call is
# the predecessor the second call's rule then sees.
sub _insert_elided_call_arrows {
  my ($self, $e) = @_;
  my @out;
  for my $t (@$e) {
    if (ref($t) eq 'PPI::Structure::List' && $t->start && $t->start->content eq '('
        && @out && $self->_is_elided_call_prev(\@out)) {
      push @out, PPI::Token::Operator->new('->');
    }
    push @out, $t;
  }
  @$e = @out;
}

# Does the tail of the already-emitted run end in a completed postfix element a
# following `(...)` would CALL?  (See _insert_elided_call_arrows.)
sub _is_elided_call_prev {
  my ($self, $out) = @_;
  my $prev  = $out->[-1];
  my $prev2 = @$out >= 2 ? $out->[-2] : undef;
  my $rp = ref($prev);
  return 1 if $rp eq 'PPI::Structure::Subscript';                 # $a[0](  $h{k}(  $x->{m}(
  return 1 if $rp eq 'PPI::Structure::List'                       # $s2->()(
           && $prev2 && $self->is_arrow_op($prev2);
  if ($rp eq 'PPI::Structure::Constructor' && $prev->start        # (sub{})[0](
      && $prev->start->content eq '[' && $prev2) {                #   a list-slice
    my $r2 = ref($prev2);
    return 1 if $r2 eq 'PPI::Structure::List'
             || $r2 eq 'PPI::Structure::Condition'
             || $r2 eq 'PPI::Token::QuoteLike::Words';
  }
  return 0;
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

# An expression-embedded block (map/grep/sort/eval body, do{}, a &-proto
# call's block, anon sub) is compiled by the PARSER — one call, one route
# (Phase B3, docs/plan-one-compiler-s411.md): Pl::Parser::embed_block answers
# through Parser2's `_v2_embed` hook when one is installed (structural forms,
# or v1's text as one raw form when the structural route declines — decided
# and captured there, never here) and with v1's own text otherwise (no
# Parser2 above this parse).  $kind 'map'|'grep'|'sort'|'eval' → an arrayref
# of BODY forms for an inline_lambda; 'do'|'sub' → the whole LAMBDA form for
# a func_ref.  Under analysis_only the block is not compiled at all and the
# node stays body-less (an emitter reaching it dies).
sub _embedded_block {
  my ($self, $block, $kind) = @_;
  return undef if $self->analysis_only;
  return $self->parser->embed_block($block, $kind);
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

# `{}` / `{ }` / `{ # comment\n }` — braces with no content at all.  Same
# whitespace/comment-stripping and same lone-Statement unwrapping as its
# sibling above, so the two answer the same question about the same shape.
sub _block_is_empty {
  my $block = shift;
  my @ch = grep { ref($_) !~ /Whitespace|Comment/ } $block->children();
  if (@ch == 1 && $ch[0]->isa('PPI::Statement')) {
    @ch = grep { ref($_) !~ /Whitespace|Comment/ } $ch[0]->children();
  }
  return scalar(@ch) == 0;
}

# #153 FOLD chunk 2 — perl's intuit_curly boundary for a brace group after
# grep/map/sort that is FOLLOWED by a `->` subscript (probed vs perl 5.40,
# s389): a hash-constructor-shaped `{…}` (or an empty `{}`) is not a block
# at all — it is an anon-hash EXPR term.  `grep {a=>1}->{a}, LIST` selects;
# `sort {a=>1}->{a}, (3,1)` prints "1 1 3" — the deref'd value is simply a
# LIST ELEMENT, sort has no expr-comparator form; `grep {}->{a}, LIST` is
# the empty-hash spelling.  A block-SHAPED `{…}` followed by `->` is a perl
# COMPILE-TIME syntax error (`near "}->"`) for all three words.  Returns
# 'ctor', 'err', or undef (no `->` subscript follows — not this boundary's
# question; the brace group keeps its block reading).
sub _ctor_deref_verdict {
  my ($block, $t1, $t2) = @_;
  return undef unless $t1 && $t2
    && ref($t1) eq 'PPI::Token::Operator' && $t1->content() eq '->'
    && ref($t2) eq 'PPI::Structure::Subscript';
  return (_block_is_hash_constructor($block) || _block_is_empty($block))
    ? 'ctor' : 'err';
}

# If a deref BLOCK contains exactly one bareword identifier (e.g. the `foo` in
# ${foo} / @{foo} / %{foo}), return that identifier — Perl autoquotes it into a
# symbolic ref to the package variable of that name.  Returns undef for anything
# else (a sub call `foo()` has a trailing List; `$ref`/`[...]`/`\ ...` are not a
# lone Word; multi-token blocks are expressions), so those keep their normal
# parse.
# Collapse a Cast sigil + Block-of-sole-bareword into a single variable token.
# ${name} → $name, @{name} → @name, %{name} → %name, $#{name} → $#name.
# Operates in place on the token list $e.  See the call site for rationale.
sub _collapse_braced_bareword_derefs {
  my ($self, $e) = @_;
  for (my $i = 0; $i < scalar(@$e) - 1; $i++) {
    my $cast = $e->[$i];
    next unless ref($cast) eq 'PPI::Token::Cast';
    my $sigil = $cast->content();
    next unless $sigil eq '$' || $sigil eq '@' || $sigil eq '%'
             || $sigil eq '$#';
    my $blk = $e->[$i+1];
    next unless ref($blk) eq 'PPI::Structure::Block' && $blk->start() eq '{';
    my $bw = _block_sole_bareword($blk);
    # `$#{^CAPTURE}`: a CARET name in the block.  PPI lexes `${^CAPTURE}` and
    # `@{^CAPTURE}` as one Magic token but has no token for the `$#` spelling,
    # so it alone arrives here as Cast + Block (task #412).
    $bw = _block_caret_name($blk) if !defined $bw;
    next unless defined $bw;
    my $tok = $sigil eq '$#'
      ? PPI::Token::ArrayIndex->new('$#' . $bw)
      : PPI::Token::Symbol->new($sigil . $bw);
    splice @$e, $i, 2, $tok;
  }
}


# A caret-name deref block, `{^CAPTURE}` → the braced name.  Kept SEPARATE from
# _block_sole_bareword on purpose: that predicate also drives the symbolic-ref
# autoquote (`${foo}` → `${"foo"}`), where a caret name is not a package
# variable name at all.  Returning the braced form lets the collapse build the
# very `$#{^CAPTURE}` ArrayIndex token the ordinary `$#name` path already
# lowers — no new emission case (rule 11).
sub _block_caret_name {
  my $block = shift;
  my @ch = grep { ref($_) !~ /Whitespace|Comment/ } $block->children();
  if (@ch == 1 && $ch[0]->isa('PPI::Statement')) {
    @ch = grep { ref($_) !~ /Whitespace|Comment/ } $ch[0]->children();
  }
  return undef unless @ch == 2
                   && ref($ch[0]) eq 'PPI::Token::Operator'
                   && $ch[0]->content() eq '^'
                   && ref($ch[1]) eq 'PPI::Token::Word';
  my $w = $ch[1]->content();
  return $w =~ /\A[A-Za-z_]\w*\z/ ? '{^' . $w . '}' : undef;
}


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


# The two symbols a `sort` comparator lambda must BIND (#287, s380).
#
# Perl sets the `$a`/`$b` of the package the sort was COMPILED in.  v2 emits a
# whole block as ONE top-level CL form, so inside a block-level `package X;`
# region the reader has already interned every bare name in the ENCLOSING
# package — which is why Parser2's requalifier rewrites the region's source
# spellings ($a → $X::a) in the first place.  The comparator body therefore
# reads X's pair there and the section's bare pair everywhere else (a
# FILE-level `package X;` is split into its own top-level form, so a bare $a
# is already read as X's), and the lambda must bind whichever the body reads.
#
# The region test is Parser2's ONE package-in-effect walk (rule 11 — never a
# fresh scope loop here).  Without the v2 seam there is no requalifier either,
# so the bare pair is right by construction; that is what has_parser gates.
# The result is CL text, not perl text: package X's `$a` is the symbol `X::$a`.
sub _sort_pair {
  my ($self, $tok) = @_;
  return ['$a', '$b'] unless $self->has_parser;
  return ['$a', '$b'] unless ref $tok && $tok->isa('PPI::Element');
  my $pkg = Pl::Parser2::_pkg_region_at($tok);
  return ['$a', '$b'] unless defined $pkg;
  (my $sym = $self->parser->_cl_pkg_designator($pkg)) =~ s/^://;
  return ["${sym}::\$a", "${sym}::\$b"];
}

# This replaces all sub calls in an expression.
# It use known number of parameters for subs and priorities.

sub handle_subcalls {
  my $self       = shift;
  my $e          = shift;
  my $in_arglist = shift // 0;  # 1 when called from parse_list (inside explicit parens)

  say "---- handle_subcalls. Incoming expr:\n", _dd($e)     if 8 & DEBUG;

  # - - - Pre-pass: normalize CORE::<builtin> to the bare builtin name.
  # `CORE::foo` explicitly names Perl's builtin (bypassing any override).  PCL
  # has no overridable builtins, so CORE::foo == foo.  Rewriting the token here
  # makes ALL downstream logic — named-unary detection, param specs, funcall
  # recognition — treat it as the builtin (codegen already maps both to p-foo).
  # Without this, `CORE::ref $x` / `CORE::shift` (no parens) parse as barewords.
  for my $tok (@$e) {
    next unless ref($tok) eq 'PPI::Token::Word';
    my $c = $tok->content();
    if ($c =~ /^CORE::(\w+)$/
        && (exists $self->known_no_of_params->{$1}
            || $1 =~ /^(?:my|our|state|local)$/)) {
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

      # Skip anything Perl already knows is CALLABLE here — a sub declared in
      # this file, a constant, an imported/prototyped name.  Perl resolves the
      # bareword at compile time and a known sub wins: `divide $text => 4` is
      # `divide($text, 4)`, never `$text->divide(4)`.  Without this the same
      # call parsed one way at statement level and the OTHER way inside a
      # `( … )` or `[ … ]` (only the nested form reaches this pre-pass), so
      # `[ divide $stdtext => 4 ]` died with "Can't locate object method
      # "divide" via package "<the string value>"" — Text-Balanced 05_extmul.t.
      next if $self->_is_known_callable($method_name, 1);

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
        # A qualified name (Foo::bar) immediately followed by parens is a
        # function call — `is UNIVERSAL::isa($x,$y)` is `is(UNIVERSAL::isa(...))`,
        # NOT the indirect-object `UNIVERSAL::isa->is(...)`.  (The `new Foo::Bar(...)`
        # indirect-with-parens form is handled by the dedicated `new` pre-pass.)
        if ($invocant->content =~ /::/
            && $i + 2 <= scalar(@$e) - 1
            && ref($e->[$i+2]) eq 'PPI::Structure::List') {
          next;
        }
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
    say "handle_subcalls: Look for subname(..) in:\n", _dd $now  if 8 & DEBUG;

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

    # Strip the PROTOTYPE and/or ATTRIBUTES that may sit between the `sub`
    # keyword and an anonymous sub's block:
    #     sub (&) { … }        sub :lvalue { … }        sub :lvalue :method { … }
    #     sub ($x) :lvalue { … }
    # PPI spells an attribute as Operator(':') + Token::Attribute (the
    # attribute token carries its own parens, e.g. `prototype($$)`), so the
    # two forms interleave; consume whichever comes next until neither does.
    # Neither affects the generated CL — the named-sub path drops them the
    # same way — and removing them lets the ordinary `sub { BLOCK }` handler
    # below fire.  Without the ATTRIBUTE half, `sub :lvalue { … }` in
    # expression position fell through to "Missing case: [" and the whole
    # statement was replaced by a PARSE ERROR comment (op/sub_lval.t, #268).
    if ($now->content() eq 'sub') {
      my $drop = 0;
      while ($i + 1 + $drop < scalar(@$e)) {
        my $t = $e->[$i + 1 + $drop];
        if (ref($t) eq 'PPI::Token::Prototype') { $drop++; next }
        last unless ref($t) eq 'PPI::Token::Operator' && $t->content eq ':'
          && $i + 2 + $drop < scalar(@$e)
          && ref($e->[$i + 2 + $drop]) eq 'PPI::Token::Attribute';
        # A LIVE `:prototype(…)` normally never reaches here: the named-sub
        # pass (Pl::Parser::_extract_prototype_attributes) has already turned
        # it into a runtime `__pcl_set_prototype` wrap.  But that pass has
        # silent bail-outs, and in those shapes the attribute arrives intact
        # and would be dropped without a word.  Announce it (#270 §8) — same
        # family as the anon-sub announce; the drop itself is effect-only.
        my $attr = $e->[$i + 2 + $drop];
        warn "PCL: attribute `:" . $attr->content . "` on an anonymous sub "
           . "is dropped (see docs/not-supported.md)\n"
          if $attr->content =~ /^prototype\(/;
        $drop += 2;
      }
      if ($drop) {
        splice @$e, $i + 1, $drop;
        $next = ($i + 1 < scalar(@$e)) ? $e->[$i + 1] : undef;
      }
    }

    say "handle_subcalls() Look for subname(..), was word. Is next list ",
        ($self->is_list($next) ? "Yes" : "No"), ". Dump:", _dd $next
        if 8 & DEBUG;

    # Handle grep/map( { BLOCK } LIST ) — paren form
    # e.g., map({$_} @list) — PPI gives Structure::List wrapping a Statement
    #   Structure::List → Statement → [Block, rest...]
    if ($self->is_list($next)) {
      my $func_name = $now->content();
      # Paren-wrapped indirect-object block form:
      #   system({ PROG } LIST) / exec({ PROG } argv...)
      # The leading brace block is the program path, the rest is argv.  Lower to
      # the ordinary list form system(PROG, LIST) (argv[0]-override nuance lost).
      if ($func_name eq 'system' || $func_name eq 'exec') {
        my @outer_ch = grep { ref($_) !~ /Whitespace/ } $next->children();
        my @inner_ch;
        if (@outer_ch == 1 && $outer_ch[0]->isa('PPI::Statement')) {
          @inner_ch = grep { ref($_) !~ /Whitespace/ } $outer_ch[0]->children();
        } else {
          @inner_ch = @outer_ch;
        }
        # Inside parens, PPI tokenises `{ PROG }` as an anon-hash Constructor,
        # not a Block — accept either, as long as it opens with `{`.
        if (@inner_ch
            && ref($inner_ch[0]) =~ /^PPI::Structure::(?:Block|Constructor)$/
            && $inner_ch[0]->start && $inner_ch[0]->start->content eq '{') {
          my $block = $inner_ch[0];
          my @rest_ch = @inner_ch[1 .. $#inner_ch];
          if (@rest_ch && ref($rest_ch[0]) eq 'PPI::Token::Operator'
              && $rest_ch[0]->content eq ',') {
            shift @rest_ch;
          }

          my($top_node, $top_id) = $self->make_node_insert('funcall');
          my $node_id = $self->make_node($now);
          $self->add_child_to_node($top_id, $node_id);

          # Block inner expression → program argument.
          my @bc = grep { ref($_) !~ /Whitespace/ } $block->children();
          if (@bc == 1 && ref($bc[0]) eq 'PPI::Statement') {
            my @sc = grep { ref($_) !~ /Whitespace/ } $bc[0]->children();
            @bc = @sc if @sc;
          }
          my $prog_expr = $self->cleanup_for_parsing(\@bc);
          $self->add_child_to_node($top_id, $self->parse($prog_expr));

          if (@rest_ch) {
            my $rest_expr = $self->cleanup_for_parsing(\@rest_ch);
            my $rest_ids  = $self->parse_list($rest_expr);
            $self->add_child_to_node($top_id, $_) for @$rest_ids;
          }

          splice @$e, $i, 2, $top_node;
          next;
        }
      }
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

        # #153 FOLD chunk 2, paren spelling: grep({a=>1}->{a}, LIST) — the
        # brace group is an anon-hash TERM (see _ctor_deref_verdict), so
        # re-bless it into the Constructor it is and fall through to the
        # ordinary funcall path, which parses the List contents as plain
        # arguments.  The block-shaped spelling dies exactly as perl does.
        if (@inner_ch >= 3 && ref($inner_ch[0]) eq 'PPI::Structure::Block') {
          my $v = _ctor_deref_verdict(@inner_ch[0, 1, 2]);
          if (defined $v) {
            die 'PCL: syntax error near "}->" (a ' . $func_name
              . " BLOCK cannot be dereferenced)\n" if $v eq 'err';
            bless $inner_ch[0], 'PPI::Structure::Constructor';
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
            my $params = ($func_name eq 'sort') ? $self->_sort_pair($now) : ['$_'];
            # (A following `->` deref chain never reaches this branch: the
            # chunk-2 normalizer above re-routed the ctor-shaped spelling
            # and died on the block-shaped one.)
            my $body_form = $self->_embedded_block($block, $func_name);

            my($lambda_node, $lambda_id) = $self->make_node_insert('inline_lambda');
            $lambda_node->{params}    = $params;
            $lambda_node->{body_form} = $body_form if $body_form;
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

    # #153 FOLD chunk 2, block spelling: `grep {a=>1}->{a}, LIST` — the brace
    # group followed by a `->` subscript is an anon-hash TERM, not a block
    # argument (see _ctor_deref_verdict).  Re-bless it into the Constructor
    # it is, so the branch below never fires and the ordinary word/args
    # machinery parses the term; die perl-shaped on the block-shaped
    # spelling.  This replaces the $deref_skip text-wrap route, which was
    # also silently WRONG for sort (it swallowed the deref'd value, which
    # perl treats as a plain LIST ELEMENT) and for eval (double-applied
    # deref — the wrapped chain was left in the stream and bound again).
    if (ref($next) eq 'PPI::Structure::Block'
        && $now->content() =~ /^(?:grep|map|sort)$/) {
      my $v = _ctor_deref_verdict($next, $e->[$i + 2], $e->[$i + 3]);
      if (defined $v) {
        die 'PCL: syntax error near "}->" (a ' . $now->content()
          . " BLOCK cannot be dereferenced)\n" if $v eq 'err';
        bless $next, 'PPI::Structure::Constructor';
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

      # Indirect-object block form: system { PROG } LIST / exec { PROG } LIST.
      # Here the brace block is NOT a code block — it is the real program path,
      # while LIST supplies the argv (whose first element may differ from PROG,
      # a nuance we drop).  Lower to the ordinary list form system(PROG, LIST)
      # so the builtin's normal arg machinery handles it.
      if ($func_name eq 'system' || $func_name eq 'exec') {
        my($top_node, $top_id) = $self->make_node_insert('funcall');
        my $node_id = $self->make_node($now);
        $self->add_child_to_node($top_id, $node_id);

        # Parse the block's inner expression as the program argument.
        my @bc = grep { ref($_) !~ /Whitespace/ } $next->children();
        if (@bc == 1 && ref($bc[0]) eq 'PPI::Statement') {
          my @sc = grep { ref($_) !~ /Whitespace/ } $bc[0]->children();
          @bc = @sc if @sc;
        }
        my $prog_expr = $self->cleanup_for_parsing(\@bc);
        my $prog_id   = $self->parse($prog_expr);
        $self->add_child_to_node($top_id, $prog_id);

        # Parse the remaining elements (the LIST → argv) and append them.
        if ($i + 2 < scalar(@$e)) {
          my @rest = @$e[$i + 2 .. $#$e];
          my $rest_list = $self->cleanup_for_parsing(\@rest);
          my $rest_ids = $self->parse_list($rest_list);
          $self->add_child_to_node($top_id, $_) for @$rest_ids;
        }

        splice @$e, $i, scalar(@$e) - $i;
        $e->[$i] = $top_node;
        next;
      }

      # Indirect-object block form with a SUPER::-qualified method:
      #   SUPER::m {@a}       — block list = (invocant, args...)
      #   SUPER::m {} @a      — trailing LIST appended to the (empty) block list
      #   SUPER::m {@a} "b"   — both concatenate
      # Perl semantics (verified vs perl 5.40): the block's list value and the
      # trailing LIST concatenate, and the INVOCANT is the first element of
      # the combined list — exactly %pcl-super-indirect's calling convention
      # (ExprToCL's SUPER:: funcall special case), so lower all three shapes
      # to funcall(SUPER::m, BLOCK-ARGS..., LIST...).  Without this, a
      # trailing element after the block fell through to the parse-error die.
      if ($func_name =~ /^SUPER::\w+$/) {
        my($top_node, $top_id) = $self->make_node_insert('funcall');
        my $node_id = $self->make_node($now);
        $self->add_child_to_node($top_id, $node_id);

        my @bc = grep { ref($_) !~ /Whitespace|Comment/ } $next->children();
        if (@bc == 1 && ref($bc[0]) eq 'PPI::Statement') {
          my @sc = grep { ref($_) !~ /Whitespace|Comment/ } $bc[0]->children();
          @bc = @sc if @sc;
        }
        if (@bc) {
          my $blk_expr = $self->cleanup_for_parsing(\@bc);
          my $blk_ids  = $self->parse_list($blk_expr);
          $self->add_child_to_node($top_id, $_) for @$blk_ids;
        }
        if ($i + 2 < scalar(@$e)) {
          my @rest = @$e[$i + 2 .. $#$e];
          my $rest_list = $self->cleanup_for_parsing(\@rest);
          my $rest_ids  = $self->parse_list($rest_list);
          $self->add_child_to_node($top_id, $_) for @$rest_ids;
        }
        splice @$e, $i, scalar(@$e) - $i;
        $e->[$i] = $top_node;
        next;
      }

      if ($func_name eq 'grep' || $func_name eq 'map' || $func_name eq 'sort'
          || $func_name eq 'eval' || $func_name eq 'do' || $has_block_proto) {

        # Create funcall with block as first param
        my($top_node, $top_id) = $self->make_node_insert('funcall');
        my $node_id = $self->make_node($now);
        $self->add_child_to_node($top_id, $node_id);

        # Use parser callback if available (handles multi-statement blocks)
        if ($self->has_parser) {
          # Determine parameters based on function type
          my $params = ($func_name eq 'sort') ? $self->_sort_pair($now)
                     : ($func_name eq 'eval') ? []
                     : ($func_name eq 'grep' || $func_name eq 'map') ? ['$_']
                     : [];  # Other & prototype functions: no implicit params

          # For grep/map/sort/eval, use inline lambda (cleaner, avoids emission issues)
          # eval { } in expression context must use inline form — defun side-effect would
          # corrupt the surrounding p-if argument list (e.g. eval{} inside elsif condition).
          # For other blocks, use named function (may need to be called separately)
          if ($func_name eq 'grep' || $func_name eq 'map' || $func_name eq 'sort'
              || $func_name eq 'eval') {
            # (A `->` deref chain after the block never reaches this branch
            # for grep/map/sort — the chunk-2 normalizer above re-routed or
            # died.  For eval/do the chain stays IN the token stream and is
            # bound onto the funcall node by the ordinary postfix machinery,
            # derefing the block's RESULT exactly as perl does.)
            my $body_form = $self->_embedded_block($next, $func_name);

            # Create inline_lambda node
            my($lambda_node, $lambda_id) = $self->make_node_insert('inline_lambda');
            $lambda_node->{params}    = $params;
            $lambda_node->{body_form} = $body_form if $body_form;
            $lambda_node->{for_func} = $func_name;
            $self->add_child_to_node($top_id, $lambda_id);
          } elsif ($func_name eq 'do') {
            # do { } : an INLINE lambda (funcall (lambda () (progn …))) — never
            # a named defun, whose emission would land between the branches of
            # a surrounding p-if when the do{} sits in an elsif condition.  The
            # progn (not (block nil)) keeps it loop-transparent: an unlabeled
            # last/next/redo inside the do{} escapes to the enclosing loop, as
            # in perl.  The parser answers with the whole lambda form.
            my($ref_node, $ref_id) = $self->make_node_insert('func_ref');
            $ref_node->{lambda_form} = $self->_embedded_block($next, 'do');
            $self->add_child_to_node($top_id, $ref_id);
          } else {
            # A &-prototype sub (e.g. try/catch, first/reduce) receives the
            # block as an anonymous sub: it must accept call arguments via @_,
            # since the caller may invoke it with args (Try::Tiny's catch
            # passes $error).  The whole lambda arrives as one form IN PLACE
            # (the same anon-sub wrapper as `sub {…}` below), so it stays
            # inside the enclosing lexical `let` and closes over it — never a
            # top-level `(defun --anon-block-N--)` hoisted out of that let,
            # the bug the #26 gate guards against (fable-answers-s345.md §3).
            my($ref_node, $ref_id) = $self->make_node_insert('func_ref');
            $ref_node->{lambda_form} = $self->_embedded_block($next, 'sub');
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
        #
        # For a user (&;@)-prototype sub (Try::Tiny's try/catch/finally), the
        # slurpy @ consumes only JUXTAPOSED trailing terms; a comma immediately
        # after the block terminates the slurp and belongs to the enclosing list.
        # Perl: `try {42}, 42, "d"` → try gets ONLY the block (the 42,"d" are
        # siblings), whereas `try {} catch {}` (no comma) → catch{} is slurped.
        # grep/map/sort are true list-ops whose list starts juxtaposed and then
        # continues across commas, so this only applies to $has_block_proto subs.
        my $next_after = $e->[$i + 2];
        my $comma_stops = $has_block_proto
          && $next_after
          && $next_after->isa('PPI::Token::Operator')
          && ($next_after->content eq ',' || $next_after->content eq '=>');

        if ($func_name eq 'eval' || $func_name eq 'do' || $comma_stops) {
          # Replace eval+block (2 elements) with result node in-place
          splice @$e, $i, 2, $top_node;
        } else {
          $self->_take_rest_as_args($e, $i, $top_id, $top_node);
        }
        next;
      }

      # Handle anonymous sub: sub { ... }
      if ($func_name eq 'sub') {
        # Use parser callback if available (handles multi-statement blocks)
        if ($self->has_parser) {
          # Anonymous subs receive call arguments via @_ (like named subs);
          # the parser answers with the whole lambda form.
          my($ref_node, $ref_id) = $self->make_node_insert('func_ref');
          $ref_node->{lambda_form} = $self->_embedded_block($next, 'sub');

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
        $lambda_node->{params}          = $self->_sort_pair($now);
        $lambda_node->{body_cl}         = 'nil';
        $lambda_node->{for_func}        = 'sort';
        $lambda_node->{comparator_name} = $comp_name;
        $self->add_child_to_node($top_id, $lambda_id);

        # Parse remaining elements (after sort + NAME) as the list
        $self->_take_rest_as_args($e, $i, $top_id, $top_node);
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
      $lambda_node->{params}     = $self->_sort_pair($now);
      $lambda_node->{body_cl}    = 'nil';
      $lambda_node->{for_func}   = 'sort';
      $lambda_node->{scalar_cmp} = 1;  # flag: scalar comparator
      # Parse the scalar as a child of the lambda (ExprToCL generates it)
      my @scalar_tok = ($next);
      my $scalar_clean = $self->cleanup_for_parsing(\@scalar_tok);
      my $scalar_ids   = $self->parse_list($scalar_clean);
      $self->add_child_to_node($lambda_id, $scalar_ids->[0]) if @$scalar_ids;
      $self->add_child_to_node($top_id, $lambda_id);

      $self->_take_rest_as_args($e, $i, $top_id, $top_node);
      next;
    }

    next
        if !$self->is_list($next);

    # A stripped `my(...)`/`our(...)` declaration list is NOT this call's
    # argument parens: `f my($y), LIST` must parse as f($y, LIST).  Leave it for
    # the bare list-operator pass below (which grabs args until a low-prio op).
    next
        if ref($next) eq 'PPI::Structure::List' && $next->{_pcl_decl_list};

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
    my ($paren_fh_id, $fh_heal);
    if ($func_name eq 'print' || $func_name eq 'say' || $func_name eq 'printf') {
      ($paren_fh_id, $fh_heal) = $self->_extract_paren_filehandle($next);
    }

    # Replace the two items in expr with a subtree:
    my($top_node, $top_id) = $self->make_node_insert('funcall');

    my $c_ids   = $self->make_nodes_from_list($next);
    # Args are built (make_nodes_from_list copies before mutating): re-attach the
    # pruned filehandle token so the shared PPI tree is pristine for any re-parse.
    if ($fh_heal) {
      my ($fh_el, $anchor) = @$fh_heal;
      if ($anchor && $anchor->parent) { $anchor->insert_before($fh_el) }
      elsif ($next->isa('PPI::Node')) {
        my ($expr) = grep { $_->isa('PPI::Statement') } $next->children;
        ($expr || $next)->add_element($fh_el);
      }
    }
    my $node_id = $self->make_node($now);

    # A bareword in a `*` slot means the same thing with parens as without —
    # `star(FOO, sub{…})` and `star FOO, sub{…}` are one call in perl and both
    # pass "FOO" (t/comp/proto.t asserts it).  The paren form used to get the
    # right answer only by ACCIDENT: the paren-less site registered FOO as a
    # handle, and the leaf emitter then spelled every later FOO as a bareword.
    $self->_read_star_slot_bareword($func_name, $c_ids)
      if @$c_ids && $self->has_environment;

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

  say "---- handle_subcalls: Before main loop. Has ", _dd $e   if 8 & DEBUG;

  # - - - Look for remaining funcalls without () around parameters:
  for(my $i=scalar(@$e)-1; $i >= 0; $i--) {
    my $now     = $e->[$i];

    # - - - A lower prio op than "," ends a param list to a fun:
    # foo a, b, c or d etc ==> foo(a,b,c) or d etc.
    # Its position is derived at the point of use (see "Parse parameters"
    # below), NEVER cached here: this loop's own reductions splice @$e and
    # shift positions, so a saved index goes stale (#343).
    my $op_name = $self->is_token_operator($now);
    if (defined $op_name && ($op_name eq 'and'
                             || $op_name eq 'or'
                             || $op_name eq 'xor')) {
      next;   # a boundary operator, never a call word
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
    # The shape is asked of the NAME, so a package-qualified spelling is a
    # handle here too (#491): this scan runs RIGHT to LEFT, so without it
    # `print Foo::H1 "x"` reduced `Foo::H1` to a funcall before `print` was
    # ever looked at, and the `:fh` site downstream never saw a Word.
    if ($i > 0 && Pl::Environment::fh_bareword_shape($sub_name)) {
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
      # Filetest operators (-e, -f, -d, …) are always unary prefix: they start
      # an argument expression, not a binary op (e.g. `print -e $f`).
      if ($next_op && ref($next) ne 'PPI::Token::Cast'
          && !$can_be_prefix{$next_op}
          && $next_op !~ /^-[A-Za-z]$/) {
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
        # Filetest operators (-e, -f, -d, …) are always unary prefix.
        my $is_unary = $is_cast || $can_be_unary_op{$next_op}
            || $next_op =~ /^-[A-Za-z]$/;
        if (!$is_unary) {
          # Binary-only operator - treat bareword as zero-arg function.
          # BUT: if the word is not a known function (not in known_no_of_params,
          # not declared in Environment), it's an unknown bareword string literal
          # in NO-STRICT code: e.g., !Bare || $x — Bare is the string "Bare".
          my $is_known_bop =
              $self->_bareword_callable_here($sub_name, $now) eq 'yes';
          # ALL-CAPS words (DIR, FILE, STDIN, MAXSIZE, etc.) are filehandles or
          # constants — leave them as funcalls so %p-fh-arg can identify them.
          # Only mixed-case unknown words (like Bare in !Bare) are string literals.
          my $is_all_caps_bop = ($sub_name =~ /^[A-Z][A-Z0-9_]*$/);
          # Under strict-subs an undeclared bareword is a COMPILE ERROR, so by
          # principle 9 anything that compiles here is a CALL, never a string —
          # a sub installed through a dynamic glob in a BEGIN loop is invisible
          # to the transpiler but real at runtime (task #193: File::Path's
          # `_IS_MSWIN32`, which also slips the ALL-CAPS escape via its leading
          # underscore).  Two operators autoquote the word to their left even
          # under strict and keep the string reading: `=>` (fat comma) and
          # `->` (class-name invocant).  This is the same strict_subs gate the
          # end-of-expression branch below already applies.
          my $strict_call_bop = $next_op ne '=>' && $next_op ne '->'
              && $self->has_environment
              && $self->environment->has_pragma('strict_subs');
          if (!($is_known_bop || $is_all_caps_bop || $strict_call_bop)) {
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
    # The argument list of a paren-less list operator ends before the nearest
    # and/or/xor AT THIS LEVEL.  Derive that boundary from the CURRENT @$e,
    # never from an index saved earlier in the scan: this loop's own
    # reductions splice @$e and shift everything right of the reduction
    # leftward, so a saved position no longer means what it meant — the
    # arguments then swallow the operator that should end them (#343;
    # docs/b2-stale-operand-ceiling-s417.md has the trace and the
    # population scan, docs/b2-ceiling-fix-s418.md the equivalence argument).
    my $end_pars= scalar(@$e)-1;
    for (my $j = $i + 1; $j <= $end_pars; $j++) {
      my $jop = $self->is_token_operator($e->[$j]) // '';
      if ($jop eq 'and' || $jop eq 'or' || $jop eq 'xor') {
        $end_pars = $j - 1;
        last;
      }
    }

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
    # The operand CEILING (list boundary from low-prec ops / enclosing
    # ternary) before any term derivation narrows it — the #153 walker and
    # the legacy branches must both be bounded by it.
    my $term_ceiling = $end_pars;
    if ($self->is_named_unary($func_name_for_unary) && $end_pars > $i + 1) {
        # #153 step 3: EVERY named unary takes its operand extent from the
        # ONE term-grammar walker (_term_extent) when the walker ANSWERS;
        # a decline (undef) falls through to the legacy hand-derived
        # branches below, which cover the two shapes the walker declines BY
        # DESIGN: bare words and prefix-op runs.  (`->method(args)` and
        # cast-block slice groups were declines too until s363 step 4 widened
        # the walker to them.)
        # Measured before the flip (PCL_TERM_DIFF, s359 + s361): zero
        # disagreements over the 111-file census corpus AND over all 604
        # files of perl's own t/*/*.t — at this site, in both populations.
        my $walker_end = $self->_term_extent($e, $i + 1, $term_ceiling);
        $self->_term_probe('unary', $sub_name, $e, $i, $term_ceiling, $walker_end)
          if $ENV{PCL_TERM_DECL};
        if (defined $walker_end) {
            $end_pars = $walker_end;
        } else {
        # #153 step 5: what is left here handles ONLY the shapes the walker
        # deliberately declines — a bare WORD primary (call? filehandle? class
        # name? constant? — not the term grammar's call, see _term_extent) and
        # PREFIX operators (`~`/`!`, unary `+`/`-`, filetests).  The
        # hand-derived Symbol / cast-deref-chain / Structure-arrow operand
        # branches that used to live here are DELETED: the walker claims every
        # one of those shapes, so they were unreachable.  Measured to be
        # unreachable too — over the 111-file corpus and all 604 files of
        # perl's own t/*/*.t, every decline that reached this chain had a Word,
        # an Operator or a Cast as its first operand token, never a Symbol or
        # a Structure (PCL_TERM_DECL inventory, s363).
        my $next_term = $e->[$i + 1];
        if (ref($next_term) eq 'PPI::Token::Cast' && $end_pars >= $i + 2) {
            # The only cast shape the walker declines is a cast over a bare
            # WORD (`ref \select`) — the cast plus that word is the operand.
            $end_pars = $i + 2;
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
        } elsif ($self->is_word($next_term)
                 || ref($next_term) eq 'PPI::Token::Operator'
                 || ref($next_term) eq 'PPI::Token::Cast') {
            # A bare WORD, a prefix operator the branch above did not take
            # (unary `+`/`-`, a filetest `-t`), or a cast run that reaches the
            # ceiling with no primary.  Consume through high-prec binary ops
            # (prec >= 55: . + - * / % x ** =~ !~ << >>), stopping before
            # comparison/logical/assignment — `eval 'a' . $x . 'b'` is
            # eval('a' . $x . 'b'), not (eval 'a') . $x . 'b'.
            $end_pars = $self->_extend_high_prec($e, $i + 1);
        } else {
            # Unreachable by construction: every other operand shape is inside
            # `cast* primary postfix*`, so the walker answered and this chain
            # never ran.  A decline here would mean the term crossed the
            # operand CEILING — which cannot happen, because the ceiling only
            # ever falls at a top-level low-precedence operator or a ternary
            # `:`, and a postfix chain contains neither.  Say so loudly rather
            # than leave $end_pars at the ceiling: that would silently hand the
            # operator a wrong-sized operand (CLAUDE.md rule 12).
            die sprintf "PExpr: term walker declined a %s operand to '%s' "
                      . "(#153 step 5 believed this unreachable): %s\n",
                ref($next_term), $func_name_for_unary,
                $self->_tok_run_desc($e, $i, $term_ceiling);
        }
        }  # end legacy operand branches (walker declined, or non-`defined` unary)

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
        $end_pars = $self->_extend_high_prec($e, $end_pars);
        # (The step-1→2 PCL_TERM_DIFF probe that used to sit here is gone
        # with the flip above: once the walker's answer IS $end_pars, the
        # comparison can only ever report equality.  The probe at the
        # strictly-1-arg site below is still live.)
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
      # For a DECLARED sub, $no_pars IS its parse class (no_params_of_sub →
      # _proto_parse_spec): 1 / [0, 1] exactly for perl's named-unary
      # prototypes (`($)`, `(;$)`, `(*)`, `(\@)`…), -1 for a list operator —
      # so `($;$)`, `($;)`, `(;$;)`, `(@)` never narrow here (s361 probed
      # `sub f ($;$)`: `f $a, $b` must stay f($a, $b); t/comp/proto.t's
      # `unilist 0 || 5` is unilist(0 || 5)), and `(;$)` / `(*)` do.
      if ($is_strictly_single && !$self->is_named_unary($func_name_for_unary)) {
        # Only apply for non-named-unary 1-param functions
        # Named unary already handled above with proper term detection
        # #153 step 3b: the operand extent comes from the ONE term-grammar
        # walker when it answers; a decline falls through to the legacy
        # branches below (bare-word filehandles are the live one).
        # Measured before the flip, over the 111-file corpus AND all 604
        # files of perl's t/*/*.t: ONE disagreement, `getc $$_[0]`, where
        # the walker takes the whole `$$_[0]` element and legacy stopped at
        # `$$_` — a silent-wrong this flip fixes (io/utf8.t:397 is the live
        # case).  The two other shapes the wider population found were the
        # prototype-arity bug fixed in the previous commit.
        my $walker_end = $self->_term_extent($e, $i + 1, $term_ceiling);
        $self->_term_probe('single', $sub_name, $e, $i, $term_ceiling, $walker_end)
          if $ENV{PCL_TERM_DECL};
        if (defined $walker_end) {
          $end_pars = $walker_end;
        } else {
        # #153 step 5, same deletion as the named-unary site above: the Symbol
        # / Magic / already-parsed-node branches are gone — the walker claims
        # all three, so they were unreachable (and measured so: every decline
        # reaching this chain across both populations had a Word, an Operator
        # or a Cast first).  A prefix operator keeps this site's OLD answer,
        # which is to leave $end_pars at the ceiling: there never was a branch
        # for one here, and `one_args + 5` relies on that.
        my $next_term = $e->[$i + 1];
        if (ref($next_term) eq 'PPI::Token::Cast' && $end_pars >= $i + 2) {
          # A cast over a bare WORD — the only cast shape the walker declines.
          $end_pars = $i + 2;
        } elsif ($self->is_word($next_term)) {
          # A STANDALONE bareword next-term to a strictly-single (max-1-arg)
          # function is a single argument — typically a bareword filehandle:
          # `close F, ...` / `fileno F, ...` / `eof FH, ...`.  The operand ends
          # where perl says it ends, and perl's answer is PRECEDENCE, not a
          # token list: `close`/`fileno`/`eof` are named unary operators, so
          # the operand runs through everything that binds TIGHTER than named
          # unary (`.` `+` `x` `->`) and stops at everything looser (`,` `?`
          # `<` `==` `&&` `and`).  `_extend_high_prec` is that rule, and it is
          # the SAME helper the named-unary operand site uses — which is the
          # point: this site had a hand-rolled three-case list (Structure::List
          # → i+2, comma-or-nothing → i+1, any other operator → leave the
          # operand at the CEILING) and the third case was wrong.  Measured vs
          # perl 5.40.3 (#495 shape (c)): `print close G ? "a" : "b"` is
          # `close(G) ? …` in perl and was `close(G ? …)` here, so PCL passed
          # the ternary's VALUE to close and read an unbound `G`; while
          # `close G . "x"` really is `close(G . "x")` in perl, which a
          # "stop at any operator" rule would have broken.  The two surviving
          # cases fall out of the same walk: a Structure::List is not an
          # operator so it is consumed (`close foo()`), and `,`/end-of-run
          # stop it.
          # Clamped to the existing ceiling — the argument-run boundary
          # (#343/B2) is computed for this call and can only shrink the answer.
          my $ext = $self->_extend_high_prec($e, $i + 1);
          $end_pars = $ext if $ext < $end_pars;
        } elsif (ref($next_term) ne 'PPI::Token::Operator') {
          # Unreachable by construction — see the named-unary site's `die` for
          # the argument (a decline on any other shape would mean the term
          # crossed the operand ceiling, which the ceiling's own definition
          # forbids).  Loud beats a silently wrong-sized operand.
          die sprintf "PExpr: term walker declined a %s operand to '%s' "
                    . "(#153 step 5 believed this unreachable): %s\n",
              ref($next_term), $sub_name,
              $self->_tok_run_desc($e, $i, $term_ceiling);
        }
        }  # end legacy operand branches (walker declined)
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

    # (The strictly-1-arg PCL_TERM_DIFF probe that used to sit here is gone
    # with the flip above — like the named-unary one, it can no longer
    # report anything about the walker: its answer IS $end_pars.  Both
    # operand sites are now on the walker; `_tok_run_desc` stays as the
    # shared describe-a-token-run helper for the next site's measurement.)

    # - - - Special handling for print/say with filehandle:
    # print FILEHANDLE LIST  (no comma between filehandle and list)
    # print $fh LIST         (variable filehandle)
    my $filehandle_id;
    if (($sub_name eq 'print' || $sub_name eq 'say' || $sub_name eq 'printf') && $i + 1 <= $end_pars) {
      my $maybe_fh = $e->[$i + 1];
      my $is_fh = 0;

      # Track filehandle expression end position (for multi-token expressions)
      my $fh_end = $i + 1;  # Start at first token after print/say

      # Check for uppercase bareword (STDERR, STDOUT, FH, etc.) or a
      # lower/mixed-case bareword already registered as a filehandle via
      # open(foo, ...) — Perl allows `print foo LIST` for any-case handles.
      if ($self->is_word($maybe_fh)) {
        my $fh_name = $maybe_fh->content;
        # A word this document declared as a zero-arg sub — `use constant FOO
        # => …`, `sub FOO () {…}` — is a TERM, and perl reads it as one:
        # `print FOO . "b"`, `print FOO - 1`, `print FOO x 3` all print the
        # constant's value combined with the operand, while `print STDOUT -1`
        # prints -1 to STDOUT.  The discriminator is the WORD (declared or
        # not), not what follows it (probed s406, task #361).  PCL took every
        # ALL-CAPS bareword as a handle, so those three statements were DROPPED
        # whole ("Fell through. Missing case: []") — the operator had no left
        # operand once the word was pruned as a filehandle.  A name that IS a
        # registered filehandle stays one: `open(FOO, …)` wins over a same-named
        # constant, because that is the only reading that can be intended.
        my $registered_fh = $self->has_environment
                         && $self->environment->is_filehandle($fh_name);
        # A package-QUALIFIED spelling is the same handle, so the ALL-CAPS
        # shape is asked of the NAME, not of the qualifier (#491): perl reads
        # `print main::STDOUT "a"` and `print Foo::H1 "x"` as handles, and
        # PCL read them as CALLS to undefined subs.  The lower-case negative
        # is unchanged and is perl's own: `sub main::f {…}; print main::f "a"`
        # CALLS f (probed), because `f` fails this test either way.
        if ((Pl::Environment::fh_bareword_shape($fh_name) || $registered_fh)
            && ($registered_fh || !$self->_is_zero_arg_func($fh_name))) {
          # Not a filehandle if followed by -> (class method call: Foo->bar())
          my $after_fh = $e->[$fh_end + 1];
          # A comma/fat-comma right after the bareword means it is a LIST
          # element, not a filehandle: `print FOO, $x` (FOO is a constant/
          # string).  The filehandle form has NO separator between the handle
          # and the list (`print FH LIST`).
          my $after_op = $after_fh ? $self->is_token_operator($after_fh) : undef;
          my $blocks_fh = $after_fh
              && ($self->is_arrow_op($after_fh)
                  || (defined($after_op) && ($after_op eq ',' || $after_op eq '=>')));
          $is_fh = 1 unless $blocks_fh;
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
      elsif ($self->_is_scalar_fh_token($maybe_fh)) {
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
      my $callable_fb = $self->_bareword_callable_here($sub_name, $now);
      # ALL-CAPS words are filehandles/constants — leave as funcalls.
      my $is_all_caps_fb = ($sub_name =~ /^[A-Z][A-Z0-9_]*$/);
      unless ($callable_fb eq 'yes' || $is_all_caps_fb) {
        my $prev_is_unary     = 0;
        my $prev_is_value_op  = 0;
        my $prev_is_separator = 0;
        if ($i > 0) {
          my $prev_tok = $e->[$i - 1];
          my $prev_op  = $self->is_token_operator($prev_tok);
          if (defined $prev_op) {
            my %unary_ops = map { $_ => 1 } ('+', '-', '!', '~', '\\', 'not');
            if    ($unary_ops{$prev_op})                 { $prev_is_unary     = 1 }
            elsif ($prev_op eq ',' || $prev_op eq '=>')  { $prev_is_separator = 1 }
            else                                         { $prev_is_value_op  = 1 }
          }
        }
        # In strict-subs mode: only unary context and already-flagged words → string.
        # In no-strict mode: any OPERATOR context → string, SEPARATORS INCLUDED.
        # Separators used to be excluded, on the grounds that a bareword after
        # `,` could be a class name or a sub call — but a name that is not
        # callable here and takes no arguments is neither: `print "x=", nosuch;`
        # fell through to a funcall and crashed at load with an undefined
        # function, where perl prints "nosuch" (probed, task #266).
        # NOT widened on 'no': the no-previous-token case ($i == 0).  A word
        # that starts its own run is `next;`, a `goto LABEL` operand, or a sub
        # this compiler cannot see (defined in a `require`d file, imported by a
        # `:DEFAULT` tag).  PCL's compile-time name knowledge is incomplete, so
        # an ABSENCE of knowledge keeps answering CALL there — measured:
        # widening that far turned `next`, `goto again` and File::Spec's
        # `curdir` into strings.  'not-yet' is different: the file declares the
        # name BELOW, so perl provably does not know it here either, and the
        # string reading holds with no previous token at all.
        my $strict_subs = $self->has_environment
                          && $self->environment->has_pragma('strict_subs');
        my $is_op_context = $strict_subs
            ? ($prev_is_unary || $now->{_bareword_string})
            : ($prev_is_unary || $prev_is_value_op || $prev_is_separator
               || $callable_fb eq 'not-yet' || $now->{_bareword_string});
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
    # A bareword in a `*` slot is a HANDLE NAME, not a call.  It reaches here
    # in one of two shapes — a zero-param funcall (the usual classification of
    # an unplaceable bareword) or a plain Word (when the name is already a
    # REGISTERED handle, which `handle_subcalls` leaves alone) — and both are
    # the same fact, so both are read here.
    $self->_read_star_slot_bareword($sub_name, $c_ids)
      if @$c_ids && $self->has_environment;

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
# The parser's ONE question about a name: how does a call to it PARSE?  The
# answer is in known_no_of_params' convention (0 = a term that takes no
# arguments, 1 = named unary, [0, 1] = named unary with an optional operand,
# -1 = list operator, -2/-3 = builtin $_/@_ defaults).  A DECLARED sub —
# prototype, signature, plain, `use constant` — answers from its record via
# _proto_parse_spec; the environment's builtin records carry no min_params
# and Config's table stays authoritative for those and for undeclared names.
# It used to return a declared sub's min_params, an ARITY fact — and the
# callers read 0 as "takes no arguments", so `(%)`, `(@)`, `(;$)`, `(;$;)`
# subs and all-defaulted signatures were called with ZERO arguments and the
# statement fell through and was dropped (task #259, t/comp/proto.t ×3).
sub no_params_of_sub {
  my $self = shift;
  my $name = shift;

  if ($self->has_environment) {
    my $rec = $self->environment->get_prototype($name);
    return $self->_proto_parse_spec($rec)
      if $rec && defined $rec->{min_params};
  }

  return $self->known_no_of_params->{$name} // -1;
}

# perl decides how a declared sub's call parses from its PROTOTYPE alone
# (toke.c, just_a_word): an empty prototype makes a TERM that never takes
# arguments; a prototype that is, after its leading `;`s, exactly one of
# `$` `_` `*` `+` or one `\X` / `\[…]` group makes a NAMED UNARY operator
# (optional operand when a `;` led); everything else — `($$)`, `(@)`, `(%)`,
# `(&@)`, and `($;)` / `(;$;)`, whose TRAILING `;` keeps them list
# operators — is a LIST operator.  A signature is not a prototype and does
# not affect parsing; a plain sub is a list operator.  The `()` shape arrives
# as is_proto 0 / min_params 0 / no params (parse_prototype_or_signature's
# empty case, `use constant`'s registration) as well as is_proto 1 / '' —
# both are the term.  This is THE one reading of a prototype's shape for the
# parser; arity (min_params, _proto_max_args) is a different fact.
sub _proto_parse_spec {
  my ($self, $rec) = @_;
  if (!$rec->{is_proto}) {
    return (($rec->{min_params} // -1) == 0 && !@{ $rec->{params} // [] })
      ? 0 : -1;
  }
  my $ps = $rec->{proto_string} // '';
  $ps =~ s/\s+//g;
  return 0 if $ps eq '';
  my $optional = ($ps =~ s/\A;+//) ? 1 : 0;
  return -1 if $ps !~ /\A(?:[\$_*+]|\\(?:[\$\@\%\&*]|\[[^\]]*\]))\z/;
  return $optional ? [0, 1] : 1;
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

  # Exact match first: filetest operators are case-SENSITIVE (-M/-A have no
  # lowercase twin, and -C/-T/-S/... only worked because the lc lookup hit
  # the OTHER filetest's identical prec-52 entry).  lc stays as the fallback
  # for whatever case-insensitive word-operator lookups relied on it.
  return $operands->{$name} // $operands->{lc $name};
}

# After "print $var TOKEN", determine if TOKEN starts a new term
# (making $var a filehandle) or is an operator (making $var an argument).
# Filetest operators (-e, -f, -d, …) default their operand to $_ when no term
# follows: `grep { -e } @files`, `print -e ? ...`, bare `-e`.  Perl treats them
# like the named-unary functions that default to $_ (uc/lc/length/…); the only
# difference is PPI tokenises them as Operators, not Words, so the normal
# default-$_ machinery (the [1,-2] spec) never sees them.  Insert an explicit
# `$_` token after such a bare filetest so both the single-element and the
# operator-precedence parse paths handle it uniformly with no special-casing.
# PPI splits a call into a trailing-:: package (`Bear::::baz`, perl: sub baz
# in package "Bear::") into TWO Words, `Bear::` + `::baz`.  Merge them back
# into one Word so every downstream path sees the qualified name whole.
sub _merge_split_qualified_words {
  my ($self, $e) = @_;
  for (my $i = 0; $i < scalar(@$e) - 1; $i++) {
    my ($a, $b) = ($e->[$i], $e->[$i + 1]);
    next unless ref($a) eq 'PPI::Token::Word' && ref($b) eq 'PPI::Token::Word'
             && $a->content =~ /::$/ && $b->content =~ /^::/;
    splice @$e, $i, 2, PPI::Token::Word->new($a->content . $b->content);
    $i--;   # re-examine: the merged word may join a further ::-fragment
  }
}

# Perl's `Bareword::` form: a bareword ENDING in :: evaluates to the package
# name as a string ("Foo::" eq "Foo", "Foo::Bar::" eq "Foo::Bar") — used as
# `tie $x, Pkg::` or `Pkg::->method` to name a class unambiguously.  Left
# alone, handle_subcalls turns the Word into an (undefined) function call
# pl-Foo::.  Replace it with the string literal up front; a method call on
# the string ("Foo"->method) is exactly perl's semantics for `Foo::->method`.
sub _stringify_trailing_colon_package {
  my ($self, $e) = @_;
  for (my $i = 0; $i < scalar(@$e); $i++) {
    my $tok = $e->[$i];
    next unless ref($tok) eq 'PPI::Token::Word'
             && $tok->content =~ /^(\w+(?:::\w+)*)::$/;
    my $name = $1;
    # Statement heads (sub/package Foo::) never parse through here, but a
    # quoted class after them would be nonsense — guard anyway.
    my $prev = $i > 0 ? $e->[$i - 1] : undef;
    next if defined $prev && ref($prev) eq 'PPI::Token::Word'
         && $prev->content =~ /^(?:sub|package)$/;
    # `Bear::::baz` reaches PPI as Word "Bear::" + Word "::baz" — that pair
    # is a CALL into the ::-suffixed package, glued later by
    # _merge_split_qualified_words (which runs after cleanup).  Leave it.
    my $next = $e->[$i + 1];
    next if defined $next && ref($next) eq 'PPI::Token::Word'
         && $next->content =~ /^::/;
    splice @$e, $i, 1, PPI::Token::Quote::Single->new("'$name'");
  }
}

# PPI upstream bug (docs/ppi-upstream-bugs.md §8, #305): `$$` is lexed as the
# PID magic variable whenever it is NOT directly followed by an identifier —
# so `$$rr` is Cast($) Symbol($rr), correctly, but `$$$rr` comes through as
# Magic($$) Symbol($rr) and `$$$$rr` as Magic($$) Cast($) Symbol($rr).  The
# stray Magic matched no case and the "Missing case" die then dropped the
# whole statement (`$$$rr{k}`, `$$$rrr->{k}` printed NOTHING).
#
# Perl's rule is positional: `$$` immediately before another deref sigil, a
# scalar, or a brace block is two scalar-deref casts; `$$` before an operator,
# a comma, `;`, `}` … is the PID.  Rewriting it here — ONE pre-pass, before
# any of the term machinery runs — means both cast-consuming sites below see
# the ordinary Cast run they already understand, instead of each growing a
# Magic special case (rule 11).
#
# Source ADJACENCY is the test, read from PPI's own sibling links rather than
# from @$e (which has whitespace filtered out): `$$ $x` is not a deref run.
sub _split_pid_magic_cast_run {
  my ($self, $e) = @_;
  for (my $i = 0; $i < scalar(@$e); $i++) {
    my $tok = $e->[$i];
    next unless ref($tok) eq 'PPI::Token::Magic' && $tok->content eq '$$';
    my $nxt = $tok->next_sibling;
    next if !$nxt || $nxt->isa('PPI::Token::Whitespace');
    # A MAGIC scalar is a scalar (#507, the #466 finding again): `$$$_` comes
    # through as Magic($$) Magic($_) and `ref() eq 'Symbol'` answered no for
    # the second half, so the run was left unrepaired and the whole statement
    # dropped — while the one-cast spelling `$$_` (Cast + Magic) always worked.
    # PPI::Token::Magic IS-A PPI::Token::Symbol, so `isa` is the whole fix; the
    # `^\$` test still keeps `@_` out (`$$@_` is a syntax error in perl).
    next unless ref($nxt) eq 'PPI::Token::Cast'
             || ($nxt->isa('PPI::Token::Symbol') && $nxt->content =~ /^\$/)
             || (ref($nxt) eq 'PPI::Structure::Block'    && $nxt->start eq '{')
             || (ref($nxt) eq 'PPI::Structure::Subscript' && $nxt->start eq '{');
    splice @$e, $i, 1, PPI::Token::Cast->new('$'), PPI::Token::Cast->new('$');
    # `$${EXPR}` (#463 item 1): PPI calls the braces a SUBSCRIPT because they
    # follow a variable token — but once the Magic is two casts there is no
    # primary between them and the braces, so they cannot be a subscript at
    # all.  They are the inner deref's BLOCK: `$${$r}` is `${ ${$r} }`, and
    # `$${$_[0]}` (t/op/gv.t:911, t/uni/gv.t:805) is `${ ${$_[0]} }`.  Left as
    # a Subscript the cast-consuming loop below tried to build a hash-element
    # access with no base and DECLINED, dropping the whole statement.  Re-class
    # it to exactly the shape `${$r}` arrives in — the established repair for a
    # PPI misclassification (Parser.pm's `_merge_unicode_symbols` does the
    # mirror image).  `$$x{k}` is untouched: its `$$` is Cast+Symbol, PPI never
    # makes it a Magic.
    if (ref($nxt) eq 'PPI::Structure::Subscript') {
      bless $nxt, 'PPI::Structure::Block';
      for my $kid ($nxt->children) {
        bless $kid, 'PPI::Statement'
          if ref($kid) eq 'PPI::Statement::Expression';
      }
    }
    $i++;   # skip past the pair just written
  }
}

# `@{+}` / `${!}` / `%{+}` — perl's BRACE spelling of a PUNCTUATION variable.
# `${ NAME }` accepts a punctuation name as readily as an identifier, so `@{+}`
# is the match-end array `@+` and `${!}` is `$!` — a variable, not a
# dereference of anything.
#
# ONE decision function, asked by both consumers (rule 11): the token-level
# fold below, and Pl::PExpr::StringInterpolation's `@{…}` scanner, which only
# ever holds the brace TEXT.  Returns the variable's ordinary spelling, or
# undef when the content is not a single punctuation character.
sub braced_punct_magic_name {
  my ($sigil, $inner) = @_;
  return undef unless defined $inner && $inner =~ /\A\s*([^\w\s])\s*\z/;
  return $sigil . $1;
}

# The token-level half.  PPI produces a single Token::Magic for the identifier
# and CARET spellings (`${^CAPTURE}`), but for the punctuation ones it produces
# Cast + Structure::Block holding a lone Operator, because `+` is an operator
# everywhere else it appears.
#
# A deref block holding exactly ONE Operator token can never be an expression —
# `+` alone is not one — so the reading is unambiguous and this is a pure
# re-tokenization: fold the pair into the Magic token PPI itself makes for the
# bare spelling.  ONE pre-pass, so both consumers see an ordinary magic
# variable (rule 11): the term machinery below, and the INTERPOLATION path,
# which compiles a reference's source text back through this same parser.
#
# Before this, `@{+}` in code silently produced an EMPTY list, and inside a
# regex it died in ExprToCL::_interp_ref_form ("cannot compile interpolated
# regex reference '@{+}'") — which was re/pat_rt_report.t's whole file, 2513
# rows held hostage by four assertions that spell @+ and @- that way (#314).
sub _fold_braced_punct_magic {
  my ($self, $e) = @_;
  for (my $i = 0; $i + 1 < scalar(@$e); $i++) {
    my $cast = $e->[$i];
    next unless ref($cast) eq 'PPI::Token::Cast'
             && $cast->content =~ /^[\$\@\%]$/;
    my $blk = $e->[$i + 1];
    next unless ref($blk) eq 'PPI::Structure::Block' && $blk->start eq '{';
    my @inner = grep { !$_->isa('PPI::Token::Whitespace') }
                map  { $_->isa('PPI::Statement') ? $_->schildren : $_ } $blk->children;
    next unless @inner == 1 && ref($inner[0]) eq 'PPI::Token::Operator';
    my $name = braced_punct_magic_name($cast->content, $inner[0]->content)
      or next;
    splice @$e, $i, 2, PPI::Token::Magic->new($name);
  }
}

# `$#-` / `$#+` — the last index of the magic arrays @- and @+.  PPI lexes
# `$#foo` as a Token::ArrayIndex but these two as a single Token::Magic, so
# they never reached the `$#…` machinery: the leaf emitter had no case and they
# came out as the literal CL symbols `|$#-|` / `|$#+|`, unbound at load.  That
# is what re/pat_rt_report.t died on the moment the @{+} fold let it start
# (#314).  Retag to the ArrayIndex token the ordinary path already lowers —
# `(p-array-last-index @-)` — so there is no new emission case (rule 11).
sub _retag_magic_array_index {
  my ($self, $e) = @_;
  for (my $i = 0; $i < scalar(@$e); $i++) {
    my $tok = $e->[$i];
    next unless ref($tok) eq 'PPI::Token::Magic'
             && $tok->content =~ /^\$#[-+]$/;   # @- and @+ are the only magic ARRAYS
    $e->[$i] = PPI::Token::ArrayIndex->new($tok->content);
  }
}

# PPI BUG (1.291, docs/ppi-upstream-bugs.md): after a SCALAR or BLOCK
# filehandle, a leading filetest is split in two.
#
#     print $fh -e $f    →  Word(print) Symbol($fh) Operator(-) Word(e) Symbol($f)
#     print STDERR -e $f →  Word(print) Word(STDERR) Operator(-e)  Symbol($f)
#
# The bareword form is right and the scalar/block form is wrong: perl reads
# `-e` as ONE filetest in both (deparse agrees), and after a term `-e` cannot
# be a binary minus at all — `$n -e $b` is a perl SYNTAX ERROR, so there is no
# competing reading to protect.  Repair the tokens here and every consumer
# downstream — the print-filehandle oracle, `_default_filetest_operand`, the
# prefix-run reduction — sees the `-X` Operator PPI should have produced, with
# no new case anywhere (rule 11).
#
# ADJACENCY is the discriminator and perl honours it too: `print $fh - e $f`
# really is negation of a call to sub `e` (probed, deparse: `-(e('...'))`).
# `next_sibling` — not `snext_sibling` — answers it: with a space between, the
# dash's immediate sibling is the Whitespace token.
sub _fuse_print_filehandle_filetest {
  my ($self, $e) = @_;
  for (my $i = 0; $i + 3 < scalar(@$e); $i++) {
    my $word = $e->[$i];
    next unless $self->is_word($word)
             && $word->content =~ /^(?:print|printf|say)$/;
    my $fh = $e->[$i + 1];
    next unless $self->_is_scalar_fh_token($fh)
             || ref($fh) eq 'PPI::Structure::Block';
    my ($dash, $letter) = @$e[$i + 2, $i + 3];
    next unless ref($dash) eq 'PPI::Token::Operator' && $dash->content eq '-';
    next unless $self->is_word($letter) && $letter->content =~ /^[A-Za-z]$/;
    # The prefix table IS the filetest list — no second copy of the letters.
    next unless $self->prefix->{'-' . $letter->content};
    next unless $dash->can('next_sibling')
             && ($dash->next_sibling // 0) == $letter;
    splice @$e, $i + 2, 2,
           PPI::Token::Operator->new('-' . $letter->content);
  }
}

sub _default_filetest_operand {
  my ($self, $e) = @_;
  for (my $i = 0; $i < scalar(@$e); $i++) {
    my $tok = $e->[$i];
    next unless ref($tok) eq 'PPI::Token::Operator'
             && $tok->content =~ /^-[A-Za-z]$/;
    my $next = $e->[$i + 1];   # undef past end
    next if defined $next && $self->_is_print_term_start($next);
    splice @$e, $i + 1, 0, PPI::Token::Symbol->new('$_');
    $i++;   # skip the token we just inserted
  }
}

# A SCALAR VARIABLE in the filehandle slot of print/printf/say — the other
# half of the `print $fh LIST` decision, whose second half is
# _is_print_term_start (does what follows begin a new term?).  ONE predicate,
# three call sites: the operator-loop print path, the `print $fh -e $f` filetest
# repair, and the paren form `print($fh LIST)` (#466).
#
# perl's grammar is `listop: LSTOP indirob listexpr` with
# `indirob: WORD | scalar | block`, and `scalar` is ANY scalar variable — a
# punctuation or digit one as readily as a named one.  Probed 5.40.3:
# `local $_ = \*STDOUT; print $_ "x\n"` writes x to STDOUT, `$0 = "STDOUT";
# print $0 "x\n"` writes through the symbolic handle named by $0, and
# `print $, "x\n"` is the same reading.  PPI hands the punctuation/digit/caret
# spellings over as PPI::Token::Magic, which IS a subclass of
# PPI::Token::Symbol — so the old exact-class `ref eq 'PPI::Token::Symbol'`
# test answered "not a scalar" for every one of them, no filehandle was
# extracted, and the leftover `$_ "x\n"` run had no operator between its two
# terms: the WHOLE statement was dropped ("Bug. Fell through. Missing case").
# Test::Builder::NoOutput's `print $_ @_ for @$self;` is the wild case.
# ->isa is the test; the `$` guard is what keeps `@_`/`%ENV` out, and
# $#array is a PPI::Token::ArrayIndex (not a Symbol subclass), so it stays out
# by construction.
sub _is_scalar_fh_token {
  my ($self, $tok) = @_;
  return 0 unless ref($tok) && $tok->isa('PPI::Token::Symbol');
  return $tok->content =~ /^\$/ ? 1 : 0;
}

sub _is_print_term_start {
  my ($self, $token) = @_;
  my $ref = ref($token);

  # Binary operators → $var is part of an expression, NOT a filehandle
  # Exception: ! and ~ are unary-only and always start a new term
  if ($ref eq 'PPI::Token::Operator') {
    my $op = $token->content;
    return 1 if $op eq '!' || $op eq '~' || $op eq 'not';
    # A filetest `-X` is unary-only too, so it ALWAYS starts a term.  The
    # letter is what discriminates: plain `-` is binary minus (`print $x - 3`)
    # and must keep answering 0.  All three callers want perl's answer here —
    # `print STDERR -e $f` reads STDERR as the handle BECAUSE `-e` starts a
    # term, and `_default_filetest_operand` must not splice `$_` into the
    # middle of a stacked run (`-f -d $x` is not `-f $_ -d $x`).
    return 1 if $op =~ /^-[A-Za-z]$/;
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
  if ($self->is_word($first)
      && Pl::Environment::fh_bareword_shape($first->content)) {
    $is_fh = 1;            # bareword: print(STDERR ...), print(main::STDOUT ...)
  }
  elsif ($self->_is_scalar_fh_token($first)) {
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
  # This mutates the SHARED PPI tree, which is a problem because v2 parses the
  # same statement twice (VarAnnotator's analysis pass, then the emission pass):
  # after the first prune the second parse no longer sees the filehandle and
  # drops it (`print($fh …)` → `(p-print …)` with no fh).  So return a heal token
  # `[$first, $anchor]` the caller re-inserts once it has built the arg nodes —
  # leaving the tree pristine after each parse.  ($anchor is the sibling the fh
  # preceded, or undef if it was last.)
  my $anchor = $first->next_sibling || undef;
  $first->remove;
  return ($fh_id, [$first, $anchor]);
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

    # Interpolated array/deref/slice ("@arr", "@{...}", "$r->@[...]"): the
    # joined expression is always a LIST — a slice child left to inherit a
    # scalar context would yield only its last element.
    return LIST_CTX if $type eq 'array_str_interp';

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
    # Code-ref call arguments likewise: $f->(...) / &$f(...) ignores
    # prototypes, so every arg flattens into @_ and a context-sensitive arg
    # (1..$n, split) must run as a list — without this, `..` inherited a
    # non-list context and parsed as a FLIP-FLOP (op/signatures.t via
    # coderef).  kids[0]=the code reference, kids[1+]=args.
    if ($type eq 'ref_funcall') {
      return LIST_CTX if $child_index >= 1;
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
  # say "---- get_a_node(): After calaling node_data()"; say _dd  $node;
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
# Is NAME something Perl already knows is callable at this point — a declared
# sub, a constant, or a prototyped/imported one?  Perl's compile-time decision
# for a bareword hangs on exactly this, so the two places that need the answer
# (bareword array subscripts, and the indirect-object pre-pass) must ask it the
# same way.
# SAME_PKG_ONLY restricts the answer to subs declared in the package the call
# sits in.  `Widget::show` is NOT visible as a bare `show` from main, so
# `[ show $w ]` there really IS indirect method syntax — probed, and the
# unqualified answer broke it.  The PROTOTYPE table cannot answer that
# question at all: it is keyed by bare name with no package, so `Widget::show`
# registers as plain `show`.  Under SAME_PKG_ONLY it is therefore not
# consulted, which makes the qualified answer conservative — an IMPORTED sub
# (`use List::Util qw(first); [ first $obj ]`) still reads as indirect method
# syntax, exactly as it did before.  Narrowing that needs the import list as a
# per-package fact, not the flat prototype table.
sub _is_known_callable {
  my ($self, $name, $same_pkg_only) = @_;
  return 0 unless $self->has_parser;
  my $env = $self->parser->environment;
  return 1 if !$same_pkg_only && $env->has_prototype($name);
  my $cur = $same_pkg_only ? ($env->current_package // 'main') : undef;
  for my $s (@{ $env->get_declared_subs || [] }) {
    next unless defined $s->{name} && $s->{name} eq $name;
    return 1 if !defined $cur;
    return 1 if defined $s->{package} && $s->{package} eq $cur;
  }
  return 0;
}

# --- The bareword-vs-string question, asked in ONE place (task #266) --------
#
# A bare `foo` with no parens and no arguments is a CALL in Perl only if the
# name is already known to be callable where the call site is COMPILED; with no
# `strict subs` in force anything else is simply the string "foo".  Two
# branches of handle_subcalls need that answer — the binary-operator branch
# (`foo , …`, `foo . $x`) and the end-of-expression branch (`…, foo;`) — and
# each used to carry its own copy of a name test that could not answer either
# half of the question:
#
#   * a QUALIFIED name never matched.  Both the prototype table and
#     declared_subs are keyed by the BARE name (plus a package), so `Foo::init`
#     read as a string even with `sub init` in package Foo above it — perl
#     calls (probed).
#   * the answer was position-blind.  Parser2's pre-scan registers every sub in
#     the FILE before anything is lowered, so a call site ABOVE `sub foo {…}`
#     was told the name is callable; perl, compiling top-down, does not know it
#     yet and reads the string "foo" (probed).  When the name was qualified the
#     two errors compounded into a silent wrong: the tail-position branch
#     emitted a call to a sub not yet defined, which returned EMPTY.
#
# Everything the compiler cannot place in this file — a core builtin, a
# constant, a name imported by a `use` — stays callable, which is the old
# whole-file answer.  Positions from two documents (a bundled module, an eval
# string) are not comparable, and answer callable for the same reason.
# Three-valued, and the two negatives are NOT interchangeable:
#   'yes'     — callable here.
#   'not-yet' — this FILE declares it, but BELOW this point.  Positive
#               knowledge: perl, compiling top-down, has not seen it either, so
#               the bareword is the string, wherever it sits.
#   'no'      — nothing this compiler can see.  That is an ABSENCE of
#               knowledge, not evidence: the name may be a builtin missing from
#               the table, a `goto` label, or a sub from a `require`d file.
sub _bareword_callable_here {
  my ($self, $name, $tok) = @_;
  return 'yes' if exists $self->known_no_of_params->{$name};
  return 'yes' if $self->control_flow_ops->{$name};
  # `CORE::length` / `CORE::GLOBAL::foo`: the core builtin under an explicit
  # namespace, never a sub this file could have declared.
  if ((my $core = $name) =~ s/^CORE::(?:GLOBAL::)?//) {
    return 'yes' if exists $self->known_no_of_params->{$core};
  }
  return 'no' unless $self->has_environment;
  my $env = $self->environment;
  my ($pkg, $base) = $name =~ /^(.+)::([^:]+)\z/ ? ($1, $2) : (undef, $name);
  my $site = Pl::PExpr::TokenUtils::decl_site($tok);
  my $declared_below = 0;
  for my $s (@{ $env->get_declared_subs || [] }) {
    next unless defined $s->{name} && $s->{name} eq $base;
    # A qualified call site names its package; an unqualified one stays
    # package-blind, as the prototype table it replaces here always was.
    next if defined $pkg && (($s->{package} // '') ne $pkg);
    my $before = Pl::PExpr::TokenUtils::site_precedes($s, $site);
    return 'yes' if !defined $before || $before;
    $declared_below = 1;
  }
  return 'not-yet' if $declared_below;
  # Not declared in this file at all: a prototype entry means an import or a
  # constant, which perl knows at compile time.  That table cannot answer for a
  # qualified name (it is keyed bare), so a qualified unknown is a string.
  return 'yes' if !defined $pkg && $env->has_prototype($name);
  return 'no';
}

# THE reading of a bareword in the `*` (filehandle) slot of a prototyped call
# — `open FH, …`, `opendir DH, …`, and a user `sub fh (*) {…}` alike.
#
# perl's rule, probed against 5.40.3 — and the two halves of the `*` family do
# NOT agree, which is the whole reason this reads the prototype record and not
# just the word:
#   * for a BUILTIN handle slot the bareword is ALWAYS the handle, even when a
#     sub of that name is declared: `sub FILE1 () {42}; tell FILE1` is -1, the
#     unopened handle, not 42 (t/comp/parser.t's very shape);
#   * for a USER `(*)` sub a DECLARED name is CALLED (`sub FOO {…}; fh FOO`
#     is `fh("FOO-called")`), and any other bareword arrives as its NAME in a
#     plain string — even when the handle is open (`open(G,…); fh G` is
#     SCALAR "G", never a glob).
#
# The two also differ in what PCL must EMIT, because the consumers differ: a
# builtin slot is a runtime macro that quotes the bareword itself
# (`%p-fh-arg`), so the node stays a Word and the name is registered as a
# handle; a USER sub's argument list quotes nothing, so the bareword reached
# SBCL as an unbound variable and killed the run (#495 shape (a)) — it becomes
# the string perl passes.  The discriminator is `min_params`, which only a
# DECLARED prototype carries (Pl::Environment's builtin table has none — the
# same test `_proto_max_args` uses).
sub _read_star_slot_bareword {
  my ($self, $sub_name, $c_ids) = @_;

  my $proto = $self->environment->get_prototype($sub_name);
  return if !$proto || !$proto->{is_proto} || !@{$proto->{params}};
  return if ($proto->{params}[0]{proto_type} // '') ne '*';

  # The bareword arrives either as a plain Word — which happens when the name
  # is ALREADY a registered handle, so `handle_subcalls` left it alone — or
  # wrapped in a zero-param funcall (exactly one child, the name), the usual
  # classification of a bareword the compiler cannot place.
  my $first = $self->get_a_node($c_ids->[0]);
  my ($name_id, $name_node, $from_funcall);
  if (ref($first) eq 'PPI::Token::Word') {
    ($name_id, $name_node, $from_funcall) = ($c_ids->[0], $first, 0);
  } elsif ($self->is_internal_node_type($first) && $first->{type} eq 'funcall') {
    my $kids = $self->get_node_children($c_ids->[0]);
    return if @$kids != 1;
    my $n = $self->get_a_node($kids->[0]);
    return if ref($n) ne 'PPI::Token::Word';
    ($name_id, $name_node, $from_funcall) = ($kids->[0], $n, 1);
  } else {
    return;
  }

  my $name = $name_node->content;
  # ALL-CAPS is asked of the NAME, not of the qualifier (#491) — the same
  # widening the print `:fh` site takes.  Without it `opendir main::DH, …`
  # stayed a funcall and reached the runtime as the STRING "main::DH" while
  # `readdir(main::DH)` reached it as a SYMBOL: the dirhandle was registered
  # under one key and read under another, so readdir silently returned nothing
  # (probed vs perl s443f; the PARENTHESISED `opendir(…)` spelling arrives by
  # a different route and is NOT fixed here).
  return if !Pl::Environment::fh_bareword_shape($name);

  if (defined $proto->{min_params}) {
    # perl CALLS a declared name in a USER sub's `*` slot — the builtin half
    # below must NOT ask this (see the probe in the header).
    return if $self->_bareword_callable_here($name, $name_node) eq 'yes';
    my $str = PPI::Token::Quote::Single->new("'$name'");
    $c_ids->[0] = $self->make_node($str);
  } elsif ($from_funcall) {
    # A builtin handle slot, and the word was classified as a CALL: unwrap it
    # to the bareword the runtime's `%p-fh-arg` quotes, and register the name.
    # An arg that is ALREADY a Word needs neither — and must not be
    # re-registered, because `add_filehandle` re-stamps the SCOPE LEVEL and a
    # `{ close WRITER; }` inside a block would then take the handle out of the
    # table when the block pops (measured: io/pipe.t's later `pipe(READER,
    # WRITER)`).
    $c_ids->[0] = $name_id;
    $self->environment->add_filehandle($name);
  }
}

sub _bareword_subscript_autoquotes {
  my ($self, $name, $is_array) = @_;
  return 1 unless $is_array;                       # hash subscript: always quote
  return 1 unless $self->has_parser;               # no environment: fall back to quote
  return 0 if $self->_is_known_callable($name);    # constant or prototyped/known sub
  # In eval-string mode the prototype table is empty, but constants/subs from the
  # enclosing program DO exist at runtime as zero-arg subs. Perl only autoquotes
  # barewords in HASH subscripts — `$a[FOO]` is always a sub call — so an ALL-CAPS
  # bareword here (the convention for constants, matching the handle_subcalls
  # heuristic) must stay callable, not be stringified to a 0 index.
  return 0 if $self->parser->eval_mode && $name =~ /^[A-Z][A-Z0-9_]*$/;
  return 1;                                         # unknown bareword: string index
}

# The ONE answer to "does this LONE subscript token autoquote, and to what
# text?".  Two sites ask it — the interpolation path (_subscript_to_cl_str) and
# the expression path (_parse_subscript_ix) — and they had drifted into two
# copies of the Word case, which is why the filetest key below was fixed in one
# and still broken in the other.
sub _subscript_autoquote_text {
  my ($self, $tok, $is_array) = @_;
  my $ref = ref($tok);
  if ($ref eq 'PPI::Token::Word') {
    return $tok->content
      if $self->_bareword_subscript_autoquotes($tok->content, $is_array);
    return undef;      # known callable in an array subscript — evaluate it
  }
  # `$h{-f}`: perl autoquotes a `-BAREWORD` HASH key, so this is the string
  # "-f".  A SINGLE-letter one arrives from PPI as the FILETEST operator token,
  # so the Word case never saw it and the key came out EMPTY — the filetest ran
  # on $_ instead — while `$h{-foo}` and `$h{-1}`, which tokenize differently,
  # were always right (probed).  Same root as the fat-comma case in
  # cleanup_for_parsing (task #234).  An ARRAY subscript is not autoquoted in
  # perl, so it keeps evaluating.
  return $tok->content
    if !$is_array && $ref eq 'PPI::Token::Operator'
       && $tok->content =~ /^-[A-Za-z]$/;
  return undef;
}

sub _parse_subscript_ix {
  my ($self, $ix, $is_array) = @_;
  my @sig = grep { !$_->isa('PPI::Token::Whitespace') } @$ix;
  # A native Subscript wraps its content in a Statement::Expression; a brace
  # PPI mis-tagged as a Block (re-blessed to Subscript by the implicit-arrow
  # retag, or the KV-slice Block forms) wraps it in a plain Statement.  A
  # lone bareword autoquotes in both — Perl's rule is positional.
  if (@sig == 1 && ($sig[0]->isa('PPI::Statement::Expression')
                    || ref($sig[0]) eq 'PPI::Statement')) {
    my @ekids = grep { !$_->isa('PPI::Token::Whitespace') } $sig[0]->children();
    if (@ekids == 1) {
      my $q = $self->_subscript_autoquote_text($ekids[0], $is_array);
      if (defined $q) {
        my $str_tok = PPI::Token::Quote::Single->new("'$q'");
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

# Attach the node at $child_id under $node_id — SPLICING ITS CHILDREN IN
# instead when it is an internal node of $flat_type (a `progn` of
# comma-separated slice keys/indices; a `string_concat` of interpolation
# parts), so they become $node_id's own children.  The one copy of the
# flatten idiom the postfix-`->` loop spelled seven times (#387 family 3,
# s413); once per reduction, never per token.
sub add_child_flattening {
  my ($self, $node_id, $child_id, $flat_type) = @_;
  my $n = $self->get_a_node($child_id);
  if ($self->is_internal_node_type($n) && $n->{type} eq $flat_type) {
    $self->add_child_to_node($node_id, $_) for @{ $self->get_node_children($child_id) };
  } else {
    $self->add_child_to_node($node_id, $child_id);
  }
  return;
}

# The KV-slice node for `%X[…]` / `%X{…}` and their `%{REF}[…]` / `%{REF}{…}`
# twins: a $type node whose first child is the (already parsed) base and
# whose remaining children are the parsed indices/keys of $term (Constructor
# or Block), comma list flattened.  Returns the node; the caller splices it
# into the operand list (the splice width differs per shape).  Same family.
sub _kv_slice_node {
  my ($self, $type, $base_id, $term) = @_;
  my ($node, $id) = $self->make_node_insert($type);
  my @ix    = $term->children();
  my $ix_id = $self->parse(\@ix);
  $self->add_child_to_node($id, $base_id);
  $self->add_child_flattening($id, $ix_id, 'progn');
  return $node;
}

# A `prefix_op` node: operator token $op_tok (a PPI token — a Cast, an
# Operator, `not`) applied to the already-parsed operand at $operand_id.
# Returns the node.  The operand is parsed BEFORE the call by every caller,
# which keeps node-id allocation order — and so the emission — identical
# to the hand-rolled sites this replaced (#387 family 4, s413).
sub _prefix_op_node {
  my ($self, $op_tok, $operand_id) = @_;
  my ($node, $id) = $self->make_node_insert('prefix_op');
  $self->add_child_to_node($id, $self->make_node($op_tok));   # operator
  $self->add_child_to_node($id, $operand_id);                 # operand
  return $node;
}

# STACKED FILETESTS are the `_`-chain, not a nest (perldoc -f -X):
#
#     -f -w -x $file   ==   -x $file && -w _ && -f _
#
# The RIGHTMOST test runs first on the real operand; each earlier test then
# re-uses the stat buffer `_`, `&&`-short-circuited, so a false inner value IS
# the result.  Nesting them instead (`-f` applied to `-d`'s "1"/undef) is
# SILENT WRONG and answers the opposite of perl on the common case:
# `-e -f "/etc/passwd"` is 1 in perl, undef nested.
#
# The prefix loop above already reduces a consecutive prefix run RIGHTMOST
# first, so this is called once per outer filetest with the inner run already
# reduced.  The discriminator is a mark on the node the inner reduction left:
# only a filetest reduction of ours chains, so `-f $x` (nothing to chain to),
# `! -e $f` (the `!` node is unmarked) and `-e $t` where `$t` merely holds a
# filetest's VALUE all keep today's shape — which is what perl does too
# (probed: `my $t = -f "/etc/passwd"; -e $t` is FALSE, no stacking).
#
# Returns the chain node, or undef when this is not a stacked filetest and the
# caller should build the ordinary prefix node.
sub _filetest_prefix_node {
  my ($self, $op_tok, $op_name, $post, $operand_id) = @_;
  return undef unless ($op_name // '') =~ /^-[A-Za-z]$/;

  # Innermost filetest of a run: ordinary node, but marked so an enclosing
  # filetest knows to chain onto it rather than consume its value.
  if (! $self->_is_filetest_reduction($post)) {
    my $node = $self->_prefix_op_node($op_tok, $operand_id);
    $self->node_tree->set_metadata($self->id_of_internal_node($node),
                                   'filetest_reduction', 1);
    return $node;
  }

  # `INNER && -X _` — built by re-parsing the documented spelling, so the
  # ordinary `&&` and filetest machinery lowers it (rule 11: no new emission
  # case, and `p-&&`'s value semantics already carry the false value through).
  # `_` is a PPI::Token::Magic, NOT a Word — that is how the lexer spells it
  # (`PPI::Document->new(\"-f _")` gives Operator + Magic), and it is what
  # makes the stat-cache filehandle emit as the bare symbol `_` instead of the
  # string "_" that handle_subcalls turns an unknown bareword into.
  my $chain_id = $self->parse([ $post,
                                PPI::Token::Operator->new('&&'),
                                PPI::Token::Operator->new($op_name),
                                PPI::Token::Magic->new('_') ]);
  $self->node_tree->set_metadata($chain_id, 'filetest_reduction', 1);
  return $self->make_subtree_item($chain_id);
}

sub _is_filetest_reduction {
  my ($self, $item) = @_;
  return 0 unless defined $item && $self->is_internal_node_type($item);
  return $self->node_tree->get_metadata($self->id_of_internal_node($item),
                                        'filetest_reduction') ? 1 : 0;
}

# THE reduction step of parse()'s postfix loop (#387 family 4, s413): the
# operand list element BEFORE position $$i (the PRE of `PRE -> NXT …`) is
# replaced by $node, the $width elements from $$i on are removed, and $$i
# steps back onto the new node so the loop re-examines it — a postfix chain
# (`$x->[0]->{k}->m()`) reduces one link per iteration.  The caller `next`s.
sub _reduce_pre {
  my ($e, $i, $node, $width) = @_;
  $e->[$$i-1] = $node;
  splice @$e, $$i, $width;
  $$i--;
  return;
}

# `WORD BLOCK LIST…` / `sort NAME LIST…` / `sort $cmp LIST…` (#387 family 8,
# s413): everything after the two head elements at $i, $i+1 is the call's
# LIST — parsed as a comma list, each item attached to $top_id, and the whole
# run from $i replaced by $top_node; with nothing after the head the two
# elements are replaced in place.  Once per block-form call.
sub _take_rest_as_args {
  my ($self, $e, $i, $top_id, $top_node) = @_;
  if ($i + 2 < scalar(@$e)) {
    my @rest = @$e[$i + 2 .. $#$e];
    my $rest_list = $self->cleanup_for_parsing(\@rest);
    my $rest_ids  = $self->parse_list($rest_list);   # usually one element
    $self->add_child_to_node($top_id, $_) for @$rest_ids;
    splice @$e, $i, scalar(@$e) - $i;
    $e->[$i] = $top_node;
  } else {
    splice @$e, $i, 2, $top_node;
  }
  return;
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


# (_add_tag_to_node deleted #303/s392 — its own "XXXX Needed??" answered NO.
# It was the sole writer of OpcodeTree's {xa} slot, which nothing read.)


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

  # `Bareword::` = the package-name string; normalize here so EVERY parse
  # route (parse() and parse_list()) sees the string literal.
  $self->_stringify_trailing_colon_package(\@no_ws);

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
    # - - - Replace foo => "bar" with "foo" , "bar":
    if (ref($part) eq 'PPI::Token::Operator' && $part->content() eq '=>') {
      # A NEW comma token in the LOCAL array — never set_content on the
      # shared tree token.  The old destructive rewrite made this pass
      # non-idempotent: the key stringification below lives only in the
      # local array, so a SECOND parse of the same region (shadow-rename
      # re-lowering, seam retries) saw `,` + a bare Word key, which
      # strict-subs then compiled as a zero-arg CALL (E4.1 M2 residue,
      # s353: Moo's `{ no_install => 1 }` became `(pl-no_install)`).
      $no_ws[$i] = PPI::Token::Operator->new(',');

      # Need to check previous too, so it isn't a string constant
      # without quotes:
      next
          if $i == 0;
      my $prev  = $no_ws[$i-1];
      if (ref($prev) eq "PPI::Token::Word") {
        $no_ws[$i-1] = $self->_make_string_of_token_word($prev);
      }
      # A FILETEST letter is autoquoted by the fat comma too, and the string
      # reading WINS over the operator one: `my %h = (-f => 4)` is the key
      # "-f" in perl, not a filetest (probed).  PPI hands `-f` over as one
      # Operator token, so it never looked like a Word here; the filetest's
      # `$_` default then turned the pair into `-f($_), 4` and the result ate
      # the next element — a SILENT WRONG (task #234).  This is the one place
      # that knows "the fat comma quotes what is on its left", so the letter is
      # settled here rather than in the `$_`-default pre-pass, which by then
      # sees the element ALREADY SPLIT and has no `=>` left to key on.
      # The narrow key matters: only a letter IMMEDIATELY before `=>` is a
      # string.  `f(-e $file => 1)` is a real filetest whose RESULT the fat
      # comma follows, and perl keeps it (probed).
      elsif (ref($prev) eq 'PPI::Token::Operator'
             && $prev->content =~ /^-[A-Za-z]$/) {
        $no_ws[$i-1] = PPI::Token::Quote::Single->new("'" . $prev->content . "'");
      }
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
        # `~` is one of them: bsd_glob expands a leading tilde, which is the
        # whole content of `<~>` (t/op/glob.t:110 `ok <~>, '~ works'`, a #415
        # census drop).  It is LESS ambiguous than the `*` already in the
        # class — a `~` can only be bitwise-not where a term is expected, and
        # the guard below already refuses to rebuild after a simple value.
        $has_glob_chars = 1
            if ref($t) !~ /^PPI::Structure/
            && $c =~ /[\*\?\[\]~]/;

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
  # `Bareword::` is the package-name string even where autoquoting applies:
  # $h{Foo::} and (Foo:: => 1) both mean the key "Foo", not "Foo::".
  $str          =~ s/^(\w+(?:::\w+)*)::$/$1/;
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



# The command text of a `…` / qx…  token, and whether it interpolates.
# The body comes from PPI's own section record (position/size within the
# token), so every delimiter — qx{} qx() qx[] qx// qx## qx'' — is read the same
# way and no delimiter has to be spelled here.  A `''` section is perl's
# non-interpolating form (task #369).
sub _command_body {
  my ($tok) = @_;
  if (ref($tok) eq 'PPI::Token::QuoteLike::Backtick') {
    my ($body) = $tok->content =~ /^`(.*)`\z/s;
    return ($body // '', 1);
  }
  my $sec = $tok->{sections} && $tok->{sections}[0]
    or die "PCL internal: qx token without a section: " . $tok->content . "\n";
  my $body = substr($tok->content, $sec->{position}, $sec->{size});
  return ($body, (($sec->{type} // '') eq q{''}) ? 0 : 1);
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

Interpolated strings are decomposed into parts for concatenation. Note: the
C<.> operator produces a regular binop.

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

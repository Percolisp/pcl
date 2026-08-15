# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::ExprToCL2;

# ExprToCL2 (prototype) — form-producing expression codegen for the v2
# pipeline.  SAME API surface as Pl::ExprToCL (expr_o / environment /
# indent_level, generate($node_id) → string) but generate() renders a CLForm.
#
# STRICT native subset: gen_form returns a form only when the WHOLE subtree
# is in the supported set (scalars, decimal numbers, binary arithmetic /
# comparison, parens); otherwise it returns undef and the caller falls back
# to the ORIGINAL Pl::ExprToCL for the whole expression.  All-or-nothing per
# expression: no mid-tree fallback, so the original generator always runs
# from an expression root (its context/lvalue state stays coherent).
#
# Extending v2 = adding node types here, one at a time, while everything
# unhandled keeps flowing through the old generator.

use v5.30;
use strict;
use warnings;
use Moo;
use Pl::CLForm ();

has expr_o       => (is => 'ro', required => 1);
has environment  => (is => 'ro');
has indent_level => (is => 'rw', default => 0);

# Live let-bound lexical set at this expression's position (sigiled names:
# '$x', '@a', '%h') — Parser2 passes its own _let_bound_vars registry
# (owner-homed since #153 chunk 0).
# W11 element access is native ONLY for let-bound containers: they are
# guaranteed bound (no boundp/auto-declare arm needed) and are never
# state-renamed package cells.
has lexicals     => (is => 'ro', default => sub { {} });

# Per-sub facts collected by Parser2's pre-pass:
#   { perl_name => { cl_name => 'pl-foo', insensitive => 0|1 } }
# `insensitive` = the sub provably never observes its caller's context
# (no wantarray, every return value scalar-shaped), so the call site may
# skip the (let ((*wantarray* …)) …) dynamic bind — the R2 caller half.
has sub_info     => (is => 'ro', default => sub { {} });

# Perl binary op → CL runtime function.  All of these coerce their operands
# and return a RAW CL value (number / string / 1-or-""), which is what makes
# them safe roots for unboxed slots (see Pl::VarAnnotator).
my %BINOP = (
  (map { $_ => "p-$_" } qw(+ - * / % ** < > <= >= == != <=>)),
  '.'   => 'p-.',
  'eq'  => 'p-str-eq', 'ne' => 'p-str-ne',
  'lt'  => 'p-str-lt', 'gt' => 'p-str-gt',
  'le'  => 'p-str-le', 'ge' => 'p-str-ge',
  'cmp' => 'p-str-cmp',
);

# (generate() deleted #303/s392.  It was the string-returning twin of
# gen_form, kept from the prototype days when this class had to be a drop-in
# for Pl::ExprToCL.  Both construction sites — Parser2.pm's seam and its
# census walker — call gen_form and consume the FORM; audited s392: all eight
# `->generate(` call sites in the repo construct `Pl::ExprToCL->new(...)` on
# the adjacent lines, so no receiver could ever be an ExprToCL2.  The two
# classes are unrelated (no `extends`), so nothing inherits either.)

# $ctx describes the POSITION of this expression, for the funcall bind only:
#   undef / 'nil' → scalar (default), 't' → list, ':void' → statement
#   position, 'inherit' → return/sub-tail position (the callee must see the
#   CALLER's context, so no bind at all).  Only funcall roots consume it;
#   operators impose scalar context on their operands (no ctx forwarded),
#   parens are transparent.
sub gen_form {
  my ($self, $node_id, $ctx) = @_;
  my $node = $self->expr_o->get_a_node($node_id);
  my $kids = $self->expr_o->get_node_children($node_id);

  if ($self->expr_o->is_internal_node_type($node)) {
    my $type = $node->{type} // '';
    # Parenthesized sub-expression: transparent (context flows through).
    if ($type eq 'tree_val' && @$kids == 1) {
      return $self->gen_form($kids->[0], $ctx);
    }
    # W11: element READ — $h{k} / $a[i] on a let-bound container →
    # (p-gethash %h KEY) / (p-aref @a IDX).  Both runtime fns return the
    # UNBOXED element value (v1 emits the same forms in rvalue position), so
    # the result is safe as an operand anywhere a value is expected.  NB: the
    # value can still BE a p-box when the element holds a reference — which
    # is why VarAnnotator counts element reads as `others` (a bare
    # `my $x = $h{k}` stays boxed; only an operator-coerced RHS may unbox).
    if ($type eq 'h_acc' || $type eq 'a_acc') {
      return $self->_elem_place($node_id);
    }
    # Native funcall: a KNOWN user sub called with static scalar args →
    # direct (pl-f a b).  The &optional/&rest calling convention makes any
    # static arity legal.  Context-insensitive callee → no *wantarray* bind.
    if ($type eq 'funcall' && @$kids >= 1) {
      my $fnode = $self->expr_o->get_a_node($kids->[0]);
      return undef unless ref($fnode) eq 'PPI::Token::Word';
      # A bare call to a BUILTIN name is the builtin even when an in-file
      # `sub NAME` shadows it (Perl overrides builtins only via import /
      # &NAME / Pkg::NAME) — never direct-call the user sub.  Falls back to
      # the seam, whose cl_name prefers the builtin (v1 parity: fixes the
      # lib/Sub/Util.pm `CORE::prototype($code)` self-call, task #81).
      # The predicate is Config's builtin param-spec table — the LANGUAGE
      # surface — NOT %RUNTIME_NAMES, which also lists internal p-* helpers
      # (aslice, gethash, …) that are legal user-sub names.
      return undef
        if exists $self->expr_o->known_no_of_params->{ $fnode->content };
      my $info = $self->sub_info->{ $fnode->content } or return undef;
      my @args;
      for my $kid (@$kids[1 .. $#$kids]) {
        # An argument expression is flattened into @_ → LIST context.
        my $f = $self->gen_form($kid, 't');
        return undef unless defined $f;
        # An element arg to a user sub aliases through @_ (defelem, #131):
        # live slot box when the element exists, lazy defelem cell when
        # not.  The seam paths do this via lvalue_context 'argbox'; here
        # swap the lowered head (the _alias_box_form sibling).
        $f = [$f->[0] . '-argbox', @{$f}[1 .. $#$f]]
          if ref($f) eq 'ARRAY' && !ref($f->[0])
             && ($f->[0] eq 'p-gethash' || $f->[0] eq 'p-aref');
        push @args, $f;
      }
      my $call = [$info->{cl_name}, @args];
      return $call if $info->{insensitive};
      return $call if ($ctx // '') eq 'inherit';   # callee sees caller's ctx
      # Under the sub-body :void regime the ambient *wantarray* is already
      # :void — skip the per-statement re-bind (task #60; the seam's
      # _ctx_wrap does the same).
      return $call if ($ctx // '') eq ':void'
        && $self->environment && $self->environment->wa_void_active;
      my $bind = (!defined $ctx || $ctx eq 'nil') ? 'nil' : $ctx;
      return ['let', ['list', ['list', '*wantarray*', $bind]], $call];
    }
    return undef;
  }

  if (ref($node) eq 'PPI::Token::Operator' && @$kids) {
    my $op = $node->content;
    # W11: element WRITE — `$h{k} = RHS` / `$a[i] = RHS` → plain CL
    # (setf (p-gethash %h K) RHS).  v1's p-setf macro adds only a per-write
    # `(unless (boundp 'CONTAINER) proclaim+vivify)` arm around this exact
    # setf — dead weight here because _elem_place already guarantees the
    # container is LET-BOUND (and skipping it avoids the arm's latent
    # special-proclaim of a lexical name on first write — W15 item 1).  The
    # (setf p-gethash)/(setf p-aref) functions own box-or-create / tie /
    # autoviv semantics.  Only a direct single-element place: list-assign
    # LHS and chained/deref places fall back.
    if ($op eq '=' && @$kids == 2) {
      my $place = $self->_elem_place($kids->[0]);
      return undef unless defined $place;
      my $rhs = $self->gen_form($kids->[1]);
      return undef unless defined $rhs;
      return ['setf', $place, $rhs];
    }
    my @forms;
    for my $kid (@$kids) {
      my $f = $self->gen_form($kid);
      return undef unless defined $f;
      push @forms, $f;
    }
    return ['p-!', $forms[0]] if $op eq '!' && @forms == 1;
    return undef unless $BINOP{$op};
    return undef unless @forms == 2 || ($op eq '-' && @forms == 1);
    return [ $BINOP{$op}, @forms ];
  }

  if (ref($node) && $node->isa('PPI::Token::Quote') && !@$kids) {
    return _string_literal_form($node);
  }

  if (ref($node) && $node->isa('PPI::Token::Number')) {
    my $c = $node->content;
    # No leading zero: Perl reads 0100 as OCTAL (64), the CL reader as 100.
    return $c if $c =~ /^(?:0|[1-9]\d*)$/;        # plain decimal integer
    return $c if $c =~ /^(?:0|[1-9]\d*)\.\d+$/;   # simple float
    return undef;                                 # octal/hex/exp → old pipeline
  }

  if (ref($node) eq 'PPI::Token::Symbol' && !@$kids) {
    my $c = $node->content;
    return $c if $c =~ /^\$\w+$/;
    return undef;
  }

  return undef;
}

# W11: `(p-gethash %h KEY)` / `(p-aref @a IDX)` form for an h_acc/a_acc node,
# or undef (→ fallback).  Native subset: the container is a PLAIN Symbol
# (`$h{...}` — not a chained/deref/qualified base) whose %h/@a is a let-bound
# lexical, and the key/index lowers natively (PExpr has already auto-quoted
# bareword hash keys into Quote nodes; multi-key `$h{a,b}` arrives as a progn
# → gen_form undef → fallback).
sub _elem_place {
  my ($self, $node_id) = @_;
  my $node = $self->expr_o->get_a_node($node_id);
  return undef unless $self->expr_o->is_internal_node_type($node);
  my $type = $node->{type} // '';
  return undef unless $type eq 'h_acc' || $type eq 'a_acc';
  my $kids = $self->expr_o->get_node_children($node_id);
  return undef unless $kids && @$kids == 2;
  my $base = $self->expr_o->get_a_node($kids->[0]);
  return undef unless ref($base) eq 'PPI::Token::Symbol'
    && $base->content =~ /^\$(\w+)$/;
  my $container = ($type eq 'h_acc' ? '%' : '@') . $1;
  return undef unless $self->lexicals->{$container};
  # state-renamed containers must go through v1's rename map.
  return undef if $self->environment
    && exists +($self->environment->state_var_renames // {})->{$container};
  my $key = $self->gen_form($kids->[1]);
  return undef unless defined $key;
  return [$type eq 'h_acc' ? 'p-gethash' : 'p-aref', $container, $key];
}

# String literal → CL form, or undef (old pipeline).
# Single quotes: unescape \' and \\.  Double quotes: the STRICT native subset —
# escapes limited to \n \t \\ \" \$ \@ \', interpolations limited to plain
# `$name` scalars (no subscripts / deref / method chains / `@` arrays); a
# variable-bearing string lowers to (p-string-concat piece …), which coerces
# every piece and returns a RAW CL string (a legit raw-slot root, same as the
# arithmetic ops).  Anything fancier → undef → the original interpolator.
sub _string_literal_form {
  my ($node) = @_;
  my $r = ref $node;
  if ($r eq 'PPI::Token::Quote::Single') {
    my $s = $node->string;
    $s =~ s/\\([\\'])/$1/g;
    return _cl_string($s);
  }
  return undef unless $r eq 'PPI::Token::Quote::Double';
  my $s = $node->string;
  my (@pieces, $lit);
  $lit = '';
  while (length $s) {
    if ($s =~ s/^\\(.)//s) {                       # escape sequence
      my $e = $1;
      if    ($e eq 'n') { $lit .= "\n" }
      elsif ($e eq 't') { $lit .= "\t" }
      elsif ($e eq '\\' || $e eq '"' || $e eq '$' || $e eq '@' || $e eq "'") { $lit .= $e }
      else { return undef }                        # \x{}, \0, \l/\u/\L/\U/… → old pipeline
    } elsif ($s =~ s/^\$([A-Za-z_]\w*)//) {        # plain scalar interpolation
      my $name = $1;                               # ($1-style captures → old pipeline)
      # A following subscript / deref / package qualifier changes meaning.
      return undef if $s =~ /^(?:\[|\{|->|::|')/;
      push @pieces, _cl_string($lit) if length $lit;
      $lit = '';
      push @pieces, "\$$name";
    } elsif ($s =~ /^[\$\@]/) {                    # $. $" ${…} @arr … → old pipeline
      return undef;
    } else {
      $lit .= substr($s, 0, 1, '');
    }
  }
  push @pieces, _cl_string($lit) if length $lit;
  return '""' unless @pieces;
  return $pieces[0] if @pieces == 1 && $pieces[0] =~ /^"/;   # pure literal
  return ['p-string-concat', @pieces];             # ≥1 variable → raw string
}

sub _cl_string {
  my ($s) = @_;
  $s =~ s/(["\\])/\\$1/g;
  return '"' . $s . '"';
}

1;

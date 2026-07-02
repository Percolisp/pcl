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

# Per-sub facts collected by Parser2's pre-pass:
#   { perl_name => { cl_name => 'pl-foo', insensitive => 0|1 } }
# `insensitive` = the sub provably never observes its caller's context
# (no wantarray, every return value scalar-shaped), so the call site may
# skip the (let ((*wantarray* …)) …) dynamic bind — the R2 caller half.
has sub_info     => (is => 'ro', default => sub { {} });

my %BINOP = map { $_ => 1 } qw(+ - * / % ** < > <= >= == !=);

# Same entry as Pl::ExprToCL::generate — returns a string (or undef when the
# expression is outside the native subset; caller then uses the old pipeline).
sub generate {
  my ($self, $node_id) = @_;
  $node_id //= $self->expr_o->root;
  my $form = $self->gen_form($node_id);
  return undef unless defined $form;
  return ('  ' x $self->indent_level) . Pl::CLForm::to_string($form, $self->indent_level);
}

sub gen_form {
  my ($self, $node_id) = @_;
  my $node = $self->expr_o->get_a_node($node_id);
  my $kids = $self->expr_o->get_node_children($node_id);

  if ($self->expr_o->is_internal_node_type($node)) {
    my $type = $node->{type} // '';
    # Parenthesized sub-expression in scalar position: transparent.
    if ($type eq 'tree_val' && @$kids == 1) {
      return $self->gen_form($kids->[0]);
    }
    # Native funcall: a KNOWN user sub called with static scalar args →
    # direct (pl-f a b).  The &optional/&rest calling convention makes any
    # static arity legal.  Context-insensitive callee → no *wantarray* bind.
    if ($type eq 'funcall' && @$kids >= 1) {
      my $fnode = $self->expr_o->get_a_node($kids->[0]);
      return undef unless ref($fnode) eq 'PPI::Token::Word';
      my $info = $self->sub_info->{ $fnode->content } or return undef;
      my @args;
      for my $kid (@$kids[1 .. $#$kids]) {
        my $f = $self->gen_form($kid);
        return undef unless defined $f;
        push @args, $f;
      }
      my $call = [$info->{cl_name}, @args];
      return $call if $info->{insensitive};
      return ['let', ['list', ['list', '*wantarray*', 'nil']], $call];
    }
    return undef;
  }

  if (ref($node) eq 'PPI::Token::Operator' && @$kids) {
    my $op = $node->content;
    return undef unless $BINOP{$op};
    my @forms;
    for my $kid (@$kids) {
      my $f = $self->gen_form($kid);
      return undef unless defined $f;
      push @forms, $f;
    }
    return undef unless @forms == 2 || ($op eq '-' && @forms == 1);
    return [ "p-$op", @forms ];
  }

  if (ref($node) && $node->isa('PPI::Token::Number')) {
    my $c = $node->content;
    return $c if $c =~ /^\d+$/;             # plain decimal integer
    return $c if $c =~ /^\d+\.\d+$/;        # simple float
    return undef;                           # octal/hex/exp → old pipeline
  }

  if (ref($node) eq 'PPI::Token::Symbol' && !@$kids) {
    my $c = $node->content;
    return $c if $c =~ /^\$\w+$/;
    return undef;
  }

  return undef;
}

1;

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

# Same entry as Pl::ExprToCL::generate — returns a string (or undef when the
# expression is outside the native subset; caller then uses the old pipeline).
sub generate {
  my ($self, $node_id) = @_;
  $node_id //= $self->expr_o->root;
  my $form = $self->gen_form($node_id);
  return undef unless defined $form;
  return ('  ' x $self->indent_level) . Pl::CLForm::to_string($form, $self->indent_level);
}

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
    # Native funcall: a KNOWN user sub called with static scalar args →
    # direct (pl-f a b).  The &optional/&rest calling convention makes any
    # static arity legal.  Context-insensitive callee → no *wantarray* bind.
    if ($type eq 'funcall' && @$kids >= 1) {
      my $fnode = $self->expr_o->get_a_node($kids->[0]);
      return undef unless ref($fnode) eq 'PPI::Token::Word';
      my $info = $self->sub_info->{ $fnode->content } or return undef;
      my @args;
      for my $kid (@$kids[1 .. $#$kids]) {
        # An argument expression is flattened into @_ → LIST context.
        my $f = $self->gen_form($kid, 't');
        return undef unless defined $f;
        push @args, $f;
      }
      my $call = [$info->{cl_name}, @args];
      return $call if $info->{insensitive};
      return $call if ($ctx // '') eq 'inherit';   # callee sees caller's ctx
      my $bind = (!defined $ctx || $ctx eq 'nil') ? 'nil' : $ctx;
      return ['let', ['list', ['list', '*wantarray*', $bind]], $call];
    }
    return undef;
  }

  if (ref($node) eq 'PPI::Token::Operator' && @$kids) {
    my $op = $node->content;
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

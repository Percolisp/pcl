package Pl::VarAnnotator;

# VarAnnotator (prototype) — Gate-1 "may this my-scalar leave the box?" for
# the v2 pipeline.  Deliberately LIGHT and conservative: any doubt → boxed
# (byte-identical semantics to today).  The full design is
# docs/type-flow-and-codegen-plan.md §(s)/(e); this version is name-keyed and
# text-scan based, which is safe because every false positive only *keeps* a
# box that wasn't needed.
#
#   my $vi = Pl::VarAnnotator->analyze(\@stmts);   # PPI statements of a block
#   $vi->{'$sum'}{unboxable}   # 1 → emit raw let + setf; writes proven arith
#
# Unboxable requires ALL of (prototype gates — a subset of the real Gate 1/2):
#   - declared exactly once in the region as a single `my $x` (shadowing → box)
#   - never: \$x, $x++/--, compound-assign, =~ target, local, pos($x),
#     referenced inside any nested sub block, foreach loop variable
#   - no string `eval` anywhere in the region (session-250 lexical capture)
#   - the decl init and EVERY `$x = RHS;` write are arithmetic-shaped
#     (numbers / $vars / + - * / % ** comparisons) with at least one operator,
#     or a pure numeric literal — so every stored value is a raw CL number
#     produced by a p-op (never a box that could alias).

use v5.30;
use strict;
use warnings;

my %ARITH_OP = map { $_ => 1 } qw(+ - * / % ** < > <= >= == !=);

sub analyze {
  my ($class, $stmts, $extra_params) = @_;
  my @stmts = grep { ref $_ } @$stmts;
  my $text = join("\n", map { $_->content } @stmts);
  my %vi;

  # 1. Collect single-scalar `my` declarations (anywhere in the region).
  # Sub parameters (bound by the lambda list, not by a `my` in this region)
  # are seeded as known one-shot declarations so the same gates apply to them.
  my %decl_count;
  my %decl_init_ok;
  for my $p (@{ $extra_params // [] }) {
    $decl_count{$p} = 1;
    $decl_init_ok{$p} = 1;
  }
  for my $stmt (@stmts) {
    my $vars = $stmt->find('PPI::Statement::Variable') || [];
    my @all = (($stmt->isa('PPI::Statement::Variable') ? ($stmt) : ()), @$vars);
    for my $d (@all) {
      next unless ref $d;
      my @kids = $d->schildren;
      next unless @kids >= 2
        && $kids[0]->isa('PPI::Token::Word') && $kids[0]->content eq 'my';
      if ($kids[1]->isa('PPI::Token::Symbol') && $kids[1]->content =~ /^\$\w+$/) {
        my $name = $kids[1]->content;
        $decl_count{$name}++;
        # init = everything after '='
        my @rhs;
        my $seen_eq = 0;
        for my $k (@kids[2 .. $#kids]) {
          if (!$seen_eq) {
            $seen_eq = 1 if $k->isa('PPI::Token::Operator') && $k->content eq '=';
            next;
          }
          push @rhs, $k;
        }
        $decl_init_ok{$name} = !$seen_eq || _arith_rhs(\@rhs);
      } else {
        # my (LIST) — mark every scalar in it as multi-declared (→ boxed)
        my $syms = $d->find('PPI::Token::Symbol') || [];
        $decl_count{$_->content} += 2
          for grep { $_->content =~ /^\$\w+$/ } @$syms;
      }
    }
  }

  # 2. Region-wide disqualifiers.
  my $has_eval = $text =~ /\beval\b/;
  my %in_nested_sub;
  for my $stmt (@stmts) {
    my $blocks = $stmt->find(sub {
      $_[1]->isa('PPI::Structure::Block') && do {
        my $prev = $_[1]->sprevious_sibling;
        $prev && $prev->isa('PPI::Token::Word') && $prev->content eq 'sub';
      };
    }) || [];
    for my $b (@$blocks) {
      $in_nested_sub{$_}++ for ($b->content =~ /(\$\w+)/g);
    }
  }

  # 3. Per-name gates.
  for my $name (keys %decl_count) {
    my $bare = quotemeta $name;
    my $unboxable =
         $decl_count{$name} == 1
      && $decl_init_ok{$name}
      && !$has_eval
      && !$in_nested_sub{$name}
      && $text !~ /\\\s*$bare\b/                                   # \$x
      && $text !~ /$bare\s*(?:\+\+|--)/                            # $x++
      && $text !~ /(?:\+\+|--)\s*$bare\b/                          # ++$x
      && $text !~ /$bare\s*(?:[-+*\/.%x]|\*\*|\|\||&&|\/\/|<<|>>)=(?!=)/  # $x +=
      && $text !~ /$bare\s*=~/                                     # $x =~ ...
      && $text !~ /\blocal\b[^;]*$bare\b/                          # local $x
      && $text !~ /\bpos\s*\(?\s*$bare\b/                          # pos($x)
      && $text !~ /\bforeach?\s+my\s+$bare\b/                      # loop var
      ? 1 : 0;
    $vi{$name} = { unboxable => $unboxable };
  }

  # 4. Every plain `$x = RHS;` write must be arith-shaped too.
  for my $stmt (@stmts) {
    my @assigns = ($stmt, @{ $stmt->find('PPI::Statement') || [] });
    for my $s (@assigns) {
      next unless ref $s && $s->isa('PPI::Statement') && !$s->isa('PPI::Statement::Variable');
      my @k = $s->schildren;
      next unless @k >= 3
        && $k[0]->isa('PPI::Token::Symbol') && $k[0]->content =~ /^\$\w+$/
        && $k[1]->isa('PPI::Token::Operator') && $k[1]->content eq '=';
      my $name = $k[0]->content;
      next unless $vi{$name} && $vi{$name}{unboxable};
      $vi{$name}{unboxable} = 0 unless _arith_rhs([@k[2 .. $#k]]);
    }
  }

  return \%vi;
}

# True when the token list is numbers/$scalars/arith-operators/parens only,
# AND (contains an operator OR is a bare numeric literal).  Such an expression
# lowers to p-ops that return raw CL numbers — safe to store in a raw slot.
sub _arith_rhs {
  my ($elems) = @_;
  my ($ok, $ops, $syms, $nums) = (1, 0, 0, 0);
  my $walk;
  $walk = sub {
    for my $e (@_) {
      next unless ref $e && $e->significant;
      my $r = ref $e;
      if    ($e->isa('PPI::Token::Number'))   { $nums++ }
      elsif ($r eq 'PPI::Token::Symbol')      { $e->content =~ /^\$\w+$/ ? $syms++ : ($ok = 0) }
      elsif ($r eq 'PPI::Token::Operator')    { $ARITH_OP{$e->content} ? $ops++ : ($ok = 0) }
      elsif ($r eq 'PPI::Token::Structure')   { $ok = 0 unless $e->content eq ';' }
      elsif ($r eq 'PPI::Structure::List' || $e->isa('PPI::Statement')) { $walk->($e->schildren) }
      else                                    { $ok = 0 }
      return unless $ok;
    }
  };
  $walk->(@$elems);
  return 0 unless $ok;
  return 1 if $ops;                       # $i * 3 + 7
  return 1 if $nums == 1 && !$syms;       # my $sum = 0;
  return 0;                               # bare `$x = $y` would alias a box
}

1;

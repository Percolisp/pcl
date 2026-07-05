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
#   - the decl init and EVERY `$x = RHS;` write are RAW-VALUE-shaped: every
#     p-op below coerces its operands and returns a raw CL number or string
#     (never a box), so an RHS with at least one top-level operator stores a
#     raw value no matter what its operands are — including calls to KNOWN
#     user subs (`my $x = f() + 1`), whose result the operator coerces.
#     Without an operator only a bare number/string literal qualifies (a bare
#     `$y` or bare `f()` could alias/return a box).
#
# ── MAINTENANCE NOTE: the disqualifier list is a growing smell (W12 replaces it)
#
# The per-name gates in step 3 are TEXT-SCAN regexes.  Every time a new syntactic
# shape is found that writes/aliases a scalar without matching an existing
# pattern (e.g. an embedded lvalue-assignment `++($x = 5)`, a slice write, a
# `substr($x,…) = …` target), the safe fix is to ADD another disqualifier — it
# only ever KEEPS a box, so it can never make output wrong, only slightly slower.
# But that means the list grows one special-case regex at a time, and there is no
# upper bound on the shapes; we could accumulate dozens.  That is acceptable as a
# *stopgap* but is NOT the end state.
#
# The principled replacement is **W12** (docs/v2-completion-plan.md): a VarAnnotator
# that walks the PExpr OpcodeTree ExprToCL2 already builds and collects real
# per-name events (read / write / ref-taken / magic-target) instead of guessing
# from source text.  A structural "is this scalar ever a write/lvalue target?"
# fact subsumes EVERY regex here in one pass.  When adding a disqualifier, log it
# in the list below so W12 has a checklist of shapes to reproduce structurally;
# do NOT invent a clever new regex that tries to cover several shapes at once
# (that is how the text-scan `_preprocess_source` bugs happened — see memory
# project_preprocess_source_strings).  Over-fire narrowly and move on.
#
# Disqualifiers added AFTER the s272 prototype (each = one shape W12 must cover):
#   - s272h: `($x = …)` parenthesized lvalue-assignment — used as an lvalue by a
#     following ++/--/.=/op (`++($x = 5)`, `($x = 5)++`).  Regex is anchored on
#     `(` immediately before the name so `for (my $i = 0; …)` counters (which
#     have `my` between `(` and the name) stay unboxable.

use v5.30;
use strict;
use warnings;

# Operators whose p-functions return raw CL values (number / string / 1-or-"").
my %ARITH_OP = map { $_ => 1 } qw(+ - * / % ** < > <= >= == != <=>
                                  . eq ne lt gt le ge cmp !);

sub analyze {
  my ($class, $stmts, $extra_params, $known_subs) = @_;
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
        $decl_init_ok{$name} = !$seen_eq || _arith_rhs(\@rhs, $known_subs);
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
      && $text !~ /\(\s*$bare\s*=[^=~]/                            # ($x = …) lvalue-assign (++($x=5), ($x=5).=…)
      && $text !~ /\blocal\b[^;]*$bare\b/                          # local $x
      && $text !~ /\bpos\s*\(?\s*$bare\b/                          # pos($x)
      && $text !~ /\bforeach?\s+my\s+$bare\b/                      # loop var
      && $text !~ /\bforeach?\s+$bare\b/                           # for $x (…) aliases
      && $text !~ /\([^=]*$bare\b[^=]*\)\s*=[^=]/                  # ($x,…) = list-assign (any nesting: (($x)xN,$y)=…)
      && $text !~ /\b(?:chomp|chop|undef|read|sysread|recv)\b[^;]*$bare\b/  # mutating builtin arg
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
      $vi{$name}{unboxable} = 0 unless _arith_rhs([@k[2 .. $#k]], $known_subs);
    }
  }

  return \%vi;
}

# True when the RHS provably stores a RAW CL value (never a box) in the slot:
#   - it contains at least one TOP-LEVEL %ARITH_OP operator (every such p-op
#     coerces its operands — boxes, strings, sub results — and returns a raw
#     number/string), or
#   - it is a single bare number/string literal.
# Operands may be numbers, string literals, $scalars, parenthesized
# subexpressions, and calls to KNOWN user subs (`f(...)` with args of ANY
# shape — the args only feed the call; the top-level operator coerces its
# result).  Operators inside call parens do not count as top-level: a bare
# `f($a + 1)` could still return a box.
sub _arith_rhs {
  my ($elems, $known_subs) = @_;
  my ($ok, $ops, $lits, $others) = _scan($elems, $known_subs);
  return 0 unless $ok;
  return 1 if $ops;                                # $i * 3 + 7 / $s . "x"
  return 1 if $lits == 1 && !$others;              # my $sum = 0; my $s = 'a';
  return 0;                        # bare `$x = $y` / `$x = f()` may alias a box
}

# Walk one nesting level; returns (ok, top_level_ops, literals, other_values).
sub _scan {
  my ($elems, $known_subs) = @_;
  my @e = grep { ref $_ && $_->significant } @$elems;
  my ($ops, $lits, $others) = (0, 0, 0);
  for (my $i = 0; $i <= $#e; $i++) {
    my $e = $e[$i];
    my $r = ref $e;
    if ($e->isa('PPI::Statement')) {                 # transparent wrapper
      my ($ok, $o, $l, $v) = _scan([$e->schildren], $known_subs);
      return 0 unless $ok;
      $ops += $o; $lits += $l; $others += $v;
    }
    elsif ($e->isa('PPI::Token::Number'))            { $lits++ }
    elsif ($r eq 'PPI::Token::Quote::Single'
        || $r eq 'PPI::Token::Quote::Double')        { $lits++ }
    elsif ($r eq 'PPI::Token::Symbol') {
      return 0 unless $e->content =~ /^\$\w+$/;
      $others++;
    }
    elsif ($r eq 'PPI::Token::Operator') {
      return 0 unless $ARITH_OP{$e->content};
      $ops++;
    }
    elsif ($r eq 'PPI::Token::Structure') {
      return 0 unless $e->content eq ';';
    }
    elsif ($r eq 'PPI::Token::Word'
           && $known_subs && $known_subs->{$e->content}
           && $i < $#e && ref($e[$i+1]) eq 'PPI::Structure::List') {
      $i++;                                          # skip the arg list
      $others++;                                     # call result: a value
    }
    elsif ($r eq 'PPI::Structure::List') {           # (subexpression)
      my ($ok, $o, $l, $v) = _scan([$e->children], $known_subs);
      return 0 unless $ok;
      $ops += $o; $lits += $l; $others += $v;
    }
    else { return 0 }
  }
  return (1, $ops, $lits, $others);
}

1;

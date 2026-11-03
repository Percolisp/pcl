# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::Stmt;

# Pl::Stmt -- THE STATEMENT CLASSIFIER (s533a; parser program step 1d / C1,
# task #3694; design `docs/codegen-review-s526.md` §12 row C1 and
# `docs/v2-target-architecture.md` §2.2).
#
# ONE reader of statement SHAPE.  The statement lowering in Pl::Parser2
# (`_lower_block_1`, `_lower_stmt`, `_lower_compound` and their arm helpers)
# calls `classify` once per statement and switches on its `kind`; the arms
# receive the spans and fields of the records below and never index a
# statement's token array or test `isa` / `content` for shape themselves.
# A grammar bug in statement recognition can therefore live in ONE place.
#
# PURITY RULE: every function here reads the PPI statement it is handed (and
# the facts the caller passes in) and returns data.  It never emits, never
# mutates the document, never calls the lowering.  Where the lowering MUTATES
# a statement (the foreach head's For->List rebless) the record names the
# node and the arm does the mutation.
#
# This module ABSORBED grammar knowledge that was already in the lowering; it
# adds none.  A shape the lowering handled wrongly before s533a is classified
# identically wrongly here (file it, do not fix it in the classifier).
#
# ---------------------------------------------------------------------------
# THE KIND TABLE (the schema: `classify` returns { kind => K, FIELDS })
#
#   kind       statement (PPI)                         lowering (Parser2 arm)
#   ---------  --------------------------------------  -------------------------
#   refused    any statement the caller's REFUSAL fact  raw refused-statement
#              names (`feature_statement_refusal`:      form (perl-shaped die at
#              `defer`, class syntax, `format NAME =`   run time); field: why
#              ... rule 11 s533 -- a format declaration
#              PCL's PPI reads as one (§40, s532e) is
#              taken out of the document before lowering
#              (Pl::Parser::_elide_format_declarations),
#              so the refusal is how any OTHER `format`
#              statement arrives; #3370
#              local-term refusals)
#   local      `local …;`, `delete local …;`,           _lower_local (v1 seam)
#              `my (…) = delete local …;`
#   decl       Statement::Variable (my / our / state)   the declaration arms;
#                                                       sub-kind: decl_shape()
#   scheduled  Statement::Scheduled (BEGIN END …)       _fallback_stmt sched
#   use        Statement::Include (use / no / require)  _fallback_stmt sched
#   end        Statement::End / Statement::Data         _fallback_stmt
#   sub        Statement::Sub with a NAME               hoist / v1 for proto+sig;
#                                                       fields: name, block,
#                                                       proto_or_sig
#   package    Statement::Package                       package switch; fields:
#                                                       name, version, block
#   label      Compound holding ONLY a Label            tagbody; field: label
#   compound   any other Compound                       _lower_compound;
#                                                       sub-kind: compound_shape()
#   return     Statement::Break whose word is `return`  the return arm;
#                                                       spans: break_shape()
#   loopctl    any other Statement::Break               next/last/redo/goto;
#              (`next` `last` `redo` `goto`)            spans: break_shape()
#   expr       PPI::Statement, ::Expression, an         the expression statement;
#              unnamed ::Sub, ::Given, ::When,          spans: expr_shape();
#              ::Unknown, ::UnmatchedBrace, ::Null      field: plain (exactly
#                                                       Statement / Expression:
#                                                       the embedded-`my` arm)
#   (none)     any other class                          DIES (rule 12)
#
# decl_shape($stmt) -> { decl => D, sk => [tokens], ... }
#   state      `state …`            fields: name (the renamed cell)
#   our        `our …`              (the lowering is _lower_our_decl)
#   my-scalar  `my $x [= INIT | if/unless COND]`  fields: name, init, declmod
#   my-lead    `my VAR <non-= operator> …`        field:  name
#   my-empty   `my ();`
#   my-noname  `my (undef …) = RHS`
#   my-list    every other `my` (containers, lists): read by decl_list_view
#
# compound_shape($stmt) -> { ckind => C, label, k => [children after label] }
#   empty      a label and nothing else (the lowering dies)
#   block      bare block: block, cont (continue block), cont_missing
#   if         if / unless chains: clauses [{kw, cond, block}]
#   while      while / until: cond, block
#   for-c      for(;;): for_s, block, has_continue, sections [3 or more]
#   foreach    foreach: list, block, var, loop_my, cast, rebless
#   try        try / catch / finally: blocks, clist, has_catch, fin_block,
#              catch_vars
#   other      any other keyword (the lowering dies naming it)
#   Field loop = 1 when kw is a loop keyword (a label is legal on it).
#
# break_shape($stmt) -> { kw, expr, mod, cond, top_comma (return only) }
# expr_shape($stmt)  -> { k, expr, mod, cond, yada, line, assign, incdec,
#                         bareword } -- see the sub.
# ---------------------------------------------------------------------------

use v5.30;
use strict;
use warnings;
use Pl::PExpr::Config ();
use Pl::PExpr::TokenUtils ();
use Pl::Environment ();

# The statement classes whose lowering is the expression statement.
my %EXPR_CLASS = map { ("PPI::Statement$_" => 1) }
  ('', qw(::Expression ::Sub ::Given ::When ::Unknown ::UnmatchedBrace ::Null));

# ---- token helpers (moved from Pl::Parser2; it aliases them) --------------

sub strip_semi {
  return grep { !($_->isa('PPI::Token::Structure') && $_->content eq ';') } @_;
}

# Split "EXPR if COND" style trailing statement modifiers at the top level.
sub split_modifier {
  my ($parts) = @_;
  for my $i (1 .. $#$parts) {
    my $p = $parts->[$i];
    if (Pl::PExpr::Config::is_statement_modifier_word($p)) {
      return ([@$parts[0 .. $i - 1]], $p->content, [@$parts[$i + 1 .. $#$parts]]);
    }
  }
  return ($parts, undef, undef);
}

sub modifier_needs_fallback {
  my ($mod) = @_;
  return $mod && $mod !~ /^(?:if|unless)$/;
}

# A token run's TEXT mentions NAME as a word (the self-init text scan).
sub text_mentions {
  my ($toks, $name) = @_;
  return join('', map { $_->content } @$toks) =~ /\Q$name\E\b/ ? 1 : 0;
}

# A node's text holds `goto LABEL` (the forward-goto scans).
sub mentions_goto {
  my ($node, $label) = @_;
  return $node->content =~ /\bgoto\s+\Q$label\E\b/ ? 1 : 0;
}

# ---- the classifier ---------------------------------------------------------

sub classify {
  my ($stmt, %fact) = @_;
  return { kind => 'refused', why => $fact{refusal} } if defined $fact{refusal};
  if ($stmt->isa('PPI::Statement::Compound')) {
    my @lk = $stmt->schildren;
    if (@lk && $lk[0]->isa('PPI::Token::Label')) {
      (my $l = $lk[0]->content) =~ s/\s*:\s*$//;
      return { kind => 'label', label => $l } if @lk == 1;
      return { kind => 'compound', label => $l };
    }
    return { kind => 'compound' };
  }
  return { kind => 'local', %{ local_shape($stmt) } } if is_local_stmt($stmt);
  return { kind => 'decl' } if $stmt->isa('PPI::Statement::Variable');
  return { kind => 'scheduled' } if $stmt->isa('PPI::Statement::Scheduled');
  return { kind => 'use' } if $stmt->isa('PPI::Statement::Include');
  return { kind => 'end' }
    if $stmt->isa('PPI::Statement::End') || $stmt->isa('PPI::Statement::Data');
  if ($stmt->isa('PPI::Statement::Sub') && $stmt->name) {
    return { kind => 'sub', name => $stmt->name, block => $stmt->block,
             proto_or_sig => proto_or_sig_str($stmt) };
  }
  return package_record($stmt) if $stmt->isa('PPI::Statement::Package');
  if ($stmt->isa('PPI::Statement::Break')) {
    my ($w) = $stmt->schildren;
    return { kind => ($w && $w->content eq 'return') ? 'return' : 'loopctl' };
  }
  my $class = ref $stmt;
  return { kind => 'expr',
           plain => ($class eq 'PPI::Statement'
                     || $class eq 'PPI::Statement::Expression') ? 1 : 0 }
    if $EXPR_CLASS{$class};
  die "Pl::Stmt: no statement kind for a $class: " . $stmt->content . "\n";
}

sub package_record {
  my ($stmt) = @_;
  my $version = eval { $stmt->version };
  # PPI quirk: ->version returns the BLOCK text for an unversioned block
  # form -- accept only real version literals.
  undef $version unless defined $version && $version =~ /^v?\d+(?:[._]\d+)*$/;
  my ($blk) = grep { $_->isa('PPI::Structure::Block') } $stmt->schildren;
  return { kind => 'package', name => $stmt->namespace // 'main',
           version => $version, block => $blk };
}

# The prototype/signature string of a named sub, or undef if it is plain.
# PPI represents an old-style prototype as a PPI::Token::Prototype (reachable
# via ->prototype), but a real signature (when `use feature 'signatures'` is in
# the document) as a PPI::Structure::Signature child — for which ->prototype is
# undef.  Both must route the sub's DEFINITION through v1's _process_sub_statement
# (imposed context / signature binding + arity checks).
sub proto_or_sig_str {
  my ($sub) = @_;
  my $p = $sub->prototype;
  return $p if defined $p;
  my ($sig) = grep { $_->isa('PPI::Structure::Signature') } $sub->children;
  return $sig ? $sig->content : undef;
}

# A statement v1's local machinery owns: `local …;` (Statement::Variable with
# declarator `local`) or standalone `delete local …;` (plain Statement).
sub is_local_stmt {
  my ($stmt) = @_;
  my @k = $stmt->schildren;
  return 0 unless @k && $k[0]->isa('PPI::Token::Word');
  return 1 if $stmt->isa('PPI::Statement::Variable') && $k[0]->content eq 'local';
  return 1 if $k[0]->content eq 'delete'
    && $k[1] && $k[1]->isa('PPI::Token::Word') && $k[1]->content eq 'local';
  # `my (...) = delete local ...`: the INIT opens a local scope (the deleted
  # element is restored at block end) — v2's plain `my` path would lower the
  # init as a self-contained expression and drop the restore.  v1's statement
  # machinery owns the open-scope bookkeeping (defvar'd my-vars +
  # p-local-*-elem wrapping the block remainder), so route the whole
  # statement through the local seam.  Top-level adjacent `delete local`
  # tokens only: a delete-local nested deeper in the init expression (e.g.
  # as a call argument) is not detected — same-shape residue as before.
  if ($stmt->isa('PPI::Statement::Variable') && $k[0]->content =~ /^(?:my|our)$/) {
    for my $i (0 .. $#k - 1) {
      return 1 if $k[$i]->isa('PPI::Token::Word')   && $k[$i]->content eq 'delete'
        && $k[$i + 1]->isa('PPI::Token::Word') && $k[$i + 1]->content eq 'local';
    }
  }
  return 0;
}

# ---- the `local` statement's shape (s533c, C2 kind 1) ----------------------
#
# local_shape($stmt) -> { lk => LK, FIELDS }, the sub-kind the structured
# `local` lowering (Pl::Parser2::_local_plan) dispatches on.  The order of the
# tests IS the old v1 text lowering's branch order (Pl::Parser's
# _process_local_declaration, deleted s533c), so a statement takes the same
# lowering family it always took.
#
#   lk         statement                                       fields
#   ---------  ----------------------------------------------  ---------------
#   delete     `delete local $h{k}` / `$a[i]` / `$r->{k}` /    sym, arrow, sub
#              `$r->[i]` (a plain statement)
#   seam       `my (…) = delete local …`, any other            why
#              `delete local …` -- still the v1 seam
#   loopmod    `local … for/foreach/while/until COND`, no `=`  mod, cond
#   glob       `local *NAME [= RHS]`                           sym, rhs
#   arylen     `local $#a = N`                                 (nonws)
#   elem       an element / slice target: `$h{k}` `@h{…}`      ebase (sym |
#              `$r->{k}` `${$r}{k}` `@{EXPR}{…}` `@$r[…]`       arrow | cast),
#                                                              sub, sub_idx,
#                                                              slice, base_expr
#   globdyn    `local *{EXPR}` / `*$x` / a punctuation glob     punct
#   deref      `local ${EXPR}` / `$$x` / `@{…}` / `%$x`         sigil
#   items      variables, `local (LIST)`, `local our …`         our
#
# Every record also carries: parts (the significant tokens, `;` and white
# space removed, the trailing modifier cut off -- v1's @parts), nonws (parts
# without the `local` word, the paren pre-unwrap applied), mod ('' or the
# modifier word), cond (its tokens), assign (a top-level `=` before the
# split), deref_paren (an unsubscripted cast in parens: `local($$r) = …`).
sub local_shape {
  my ($stmt) = @_;
  my @parts = grep {
    my $r = ref $_;
    $r ne 'PPI::Token::Whitespace' && !($r eq 'PPI::Token::Structure' && $_->content eq ';')
  } $stmt->children;
  my $w0 = $parts[0] && $parts[0]->isa('PPI::Token::Word') ? $parts[0]->content : '';
  # (a plain statement's run drops comments as well -- the expression
  # statement's filter; a `local` declaration's run keeps them, as v1's did)
  return local_delete_shape([grep { ref($_) ne 'PPI::Token::Comment' } @parts])
    if $w0 eq 'delete';
  return { lk => 'seam', why => 'my-delete-local' } if $w0 ne 'local';
  my $assign = grep { ref($_) eq 'PPI::Token::Operator' && $_->content eq '=' } @parts;
  my ($mod, $cond) = ('', []);
  for my $i (0 .. $#parts) {
    next unless ref($parts[$i]) eq 'PPI::Token::Word'
      && Pl::PExpr::Config::is_statement_modifier_word($parts[$i]);
    my $m = $parts[$i]->content;
    # WITH an assignment only if/unless split (a loop modifier must leave the
    # run whole: the RHS parse then drops the statement LOUDLY -- the open
    # scope cannot restore once per iteration); with none, all six do.
    last if $assign && $m !~ /^(?:if|unless)$/;
    ($mod, $cond) = ($m, [@parts[$i + 1 .. $#parts]]);
    splice(@parts, $i);
    last;
  }
  my %r = (parts => \@parts, mod => $mod, cond => $cond, assign => $assign ? 1 : 0);
  return { %r, lk => 'loopmod' } if $mod ne '' && $mod !~ /^(?:if|unless)$/;
  my @nonws = grep {
    my $x = ref $_;
    $x ne 'PPI::Token::Whitespace' && !($x eq 'PPI::Token::Word' && $_->content eq 'local')
  } @parts;
  $r{deref_paren} = local_paren_unwrap(\@nonws);
  $r{nonws} = \@nonws;
  my ($n0, $n1, $n2) = @nonws;
  my $c0 = $n0 ? $n0->content : '';
  if ($n0 && ref($n0) eq 'PPI::Token::Symbol' && $c0 =~ /^\*(.+)$/
      && !defined Pl::Environment::punct_glob_name($c0)) {
    return { %r, lk => 'glob', sym => $n0, rhs => local_rhs_after(\@nonws, 0) };
  }
  return { %r, lk => 'arylen' } if $n0 && ref($n0) eq 'PPI::Token::ArrayIndex';
  my $e = local_elem_shape(\@nonws);
  return { %r, lk => 'elem', %$e } if $e;
  my $punct = ($n0 && ref($n0) eq 'PPI::Token::Symbol')
            ? Pl::Environment::punct_glob_name($c0) : undef;
  my $block_or_sym = $n1 && (ref($n1) eq 'PPI::Structure::Block'
                             || ref($n1) eq 'PPI::Token::Symbol');
  if (defined $punct || (ref($n0 // '') eq 'PPI::Token::Cast' && $c0 eq '*' && $block_or_sym)) {
    return { %r, lk => 'globdyn', punct => $punct };
  }
  if (ref($n0 // '') eq 'PPI::Token::Cast' && $c0 =~ /^[\$\@%]$/ && $block_or_sym) {
    return { %r, lk => 'deref', sigil => $c0 };
  }
  return { %r, lk => 'items',
           our => ($n0 && ref($n0) eq 'PPI::Token::Word' && $c0 eq 'our') ? 1 : 0 };
}

# The pre-unwrap of a parenthesised single target -- `local(*foo)`,
# `local($x)`, `local($h{k})`, `local($r->{k})`, `local(@$r{…})`, `local($$r)`
# -- IN PLACE on @$nonws.  Returns 1 for an UNSUBSCRIPTED cast (its parens
# still make the assignment a LIST one, #3365), else 0.  The parens of a
# SUBSCRIPTED single element are discarded here, and that is #1339 (perl
# reads list-vs-scalar assignment off them); a lone variable keeps them in
# `parts`, which the items path reads (#1243 (a)).
sub local_paren_unwrap {
  my ($nonws) = @_;
  return 0 unless @$nonws && ref($nonws->[0]) eq 'PPI::Structure::List';
  my @flat = flat_children($nonws->[0]);
  my $has_comma = grep { ref($_) eq 'PPI::Token::Operator' && $_->content eq ',' } @flat;
  if (@flat == 1 && ref($flat[0]) eq 'PPI::Token::Symbol') {
    splice(@$nonws, 0, 1, @flat);
    return 0;
  }
  return 0 unless @flat >= 2 && !$has_comma
    && (ref($flat[0]) eq 'PPI::Token::Symbol' || ref($flat[0]) eq 'PPI::Token::Cast');
  my $deref_paren = (ref($flat[0]) eq 'PPI::Token::Cast'
    && !grep { ref($_) eq 'PPI::Structure::Subscript'
               || (ref($_) eq 'PPI::Token::Operator' && $_->content eq '->') } @flat) ? 1 : 0;
  splice(@$nonws, 0, 1, @flat);
  return $deref_paren;
}

# The significant children of a bracketed structure with an expression
# Statement inside flattened -- the ( ) / { } tokens dropped (v1's walk, which
# two `local` readers and the deref-block reader share).
sub flat_children {
  my ($node) = @_;
  my @out;
  for my $child ($node->children) {
    my $cr = ref $child;
    next if $cr eq 'PPI::Token::Whitespace' || $cr eq 'PPI::Token::Structure';
    if ($cr =~ /^PPI::Statement/) {
      push @out, grep { ref($_) ne 'PPI::Token::Whitespace' } $child->children;
    } else {
      push @out, $child;
    }
  }
  return @out;
}

# A subscript's key groups: its flattened tokens split on top-level commas
# (`{a, b}` two keys, `[1..3]` one group).
sub subscript_key_groups {
  my ($sub) = @_;
  return split_groups([flat_children($sub)], qr/^,$/);
}

# The target groups of a `local (…)` list: split on `,` and `=>` (task #509;
# `undef` stays a group of its own -- a skip slot).
sub local_list_groups {
  my ($list) = @_;
  return split_groups([flat_children($list)], qr/^(?:,|=>)$/);
}

# A token run split on top-level operators matching $sep; empty groups dropped.
sub split_groups {
  my ($toks, $sep) = @_;
  my (@groups, @cur);
  for my $t (@$toks) {
    if (ref($t) eq 'PPI::Token::Operator' && $t->content =~ $sep) {
      push @groups, [@cur] if @cur;
      @cur = ();
    } else {
      push @cur, $t;
    }
  }
  push @groups, [@cur] if @cur;
  return @groups;
}

# The tokens after the first top-level `=` at index >= $from, or undef when
# there is no `=`.
sub local_rhs_after {
  my ($toks, $from) = @_;
  for my $i ($from .. $#$toks) {
    next unless ref($toks->[$i]) eq 'PPI::Token::Operator' && $toks->[$i]->content eq '=';
    return [@$toks[$i + 1 .. $#$toks]];
  }
  return undef;
}

# An element / slice `local` target's fields, or undef.  Three bases: a named
# container (`$h{k}`, `@a[…]`), an arrow off a scalar (`$r->{k}`), a cast
# (`${$r}{k}`, `@{EXPR}{…}`, `@$r[…]`).
sub local_elem_shape {
  my ($nw) = @_;
  my ($n0, $n1, $n2) = @$nw;
  return undef unless $n0 && $n1;
  my $e = local_elem_base($nw) or return undef;
  # An EMPTY subscript is not an element target (v1's branch fell through).
  return subscript_key_groups($e->{sub}) ? $e : undef;
}

sub local_elem_base {
  my ($nw) = @_;
  my ($n0, $n1, $n2) = @$nw;
  my $sub = 'PPI::Structure::Subscript';
  if (ref($n0) eq 'PPI::Token::Symbol' && ref($n1) eq $sub) {
    return { ebase => 'sym', sub => $n1, sub_idx => 1,
             slice => ($n0->content =~ /^\@/ ? 1 : 0) };
  }
  return undef unless $n2 && ref($n2) eq $sub;
  if (ref($n0) eq 'PPI::Token::Symbol'
      && ref($n1) eq 'PPI::Token::Operator' && $n1->content eq '->') {
    return { ebase => 'arrow', sub => $n2, sub_idx => 2, slice => 0 };
  }
  if (ref($n0) eq 'PPI::Token::Cast' && $n0->content =~ /^[\$\@]$/
      && (ref($n1) eq 'PPI::Structure::Block' || ref($n1) eq 'PPI::Token::Symbol')) {
    return { ebase => 'cast', sub => $n2, sub_idx => 2,
             slice => ($n0->content eq '@' ? 1 : 0),
             base_expr => (ref($n1) eq 'PPI::Structure::Block' ? 1 : 0) };
  }
  return undef;
}

# Standalone `delete local SYM SUBSCRIPT` / `delete local $r->SUBSCRIPT`.
sub local_delete_shape {
  my ($parts) = @_;
  my ($d, $l, $s, $x, $y) = @$parts;
  return { lk => 'seam', why => 'delete-local-other' }
    unless $l && ref($l) eq 'PPI::Token::Word' && $l->content eq 'local'
        && $s && ref($s) eq 'PPI::Token::Symbol' && $x;
  my $sub = 'PPI::Structure::Subscript';
  return { lk => 'delete', parts => $parts, sym => $s, arrow => 0, sub => $x }
    if ref($x) eq $sub;
  return { lk => 'delete', parts => $parts, sym => $s, arrow => 1, sub => $y }
    if ref($x) eq 'PPI::Token::Operator' && $x->content eq '->' && $y && ref($y) eq $sub;
  return { lk => 'seam', why => 'delete-local-other' };
}

# The label of a standalone-label statement, or undef.
sub standalone_label {
  my ($stmt) = @_;
  return undef unless $stmt->isa('PPI::Statement::Compound');
  my $r = classify($stmt);
  return $r->{kind} eq 'label' ? $r->{label} : undef;
}

# A declaration or a local statement (whose scope a goto wrap must not cut).
sub opens_scope {
  my ($stmt) = @_;
  return $stmt->isa('PPI::Statement::Variable') || is_local_stmt($stmt);
}


# The statements a block lowers: every significant statement but `;`.
sub lowered_statements {
  my ($stmts) = @_;
  return grep { ref $_ && !$_->isa('PPI::Statement::Null') } @$stmts;
}

# A PPI NODE among a label's earlier siblings whose text holds `goto LABEL`.
sub node_mentions_goto {
  my ($node, $label) = @_;
  return ref $node && $node->isa('PPI::Node') && mentions_goto($node, $label) ? 1 : 0;
}

# A token run's text, space-joined (the self-reference scans read it).
sub joined_text {
  my ($toks) = @_;
  return join ' ', map { $_->content } @$toks;
}

# ---- declarations -------------------------------------------------------------

sub decl_shape {
  my ($stmt) = @_;
  my @sk = strip_semi($stmt->schildren);
  my %r = (sk => \@sk, nsk => scalar(@sk), rest => [@sk[1 .. $#sk]]);
  if (@sk >= 2 && $sk[0]->isa('PPI::Token::Word')) {
    return { %r, decl => 'our' } if $sk[0]->content eq 'our';
    return _state_shape(\%r, \@sk) if $sk[0]->content eq 'state';
  }
  my ($name, $init, $declmod) = single_scalar_decl($stmt);
  if ($name) {
    my %i;
    if (defined $init) {
      my $lp = Pl::PExpr::TokenUtils::lowprec_idx($init, 0);
      @i{qw(init_expr init_mod init_cond)} = split_modifier($init);
      $i{tail} = [@$init[$lp .. $#$init]] if defined $lp;
    }
    return { %r, %i, decl => 'my-scalar', name => $name, init => $init,
             declmod => $declmod };
  }
  my $lead = lead_decl_with_expr_tail($stmt);
  return { %r, decl => 'my-lead', name => $lead } if defined $lead;
  return { %r, decl => 'my-empty' } if is_empty_my_decl($stmt);
  return { %r, decl => 'my-noname' } if is_no_name_my_decl($stmt);
  return { %r, decl => 'my-list' };
}

# `state NAME [= INIT]`: a SCALAR's below-assignment tail (`state $s = 1, f();`)
# is split off when the comma is the statement's own and not a parenless list
# operator's (Pl::PExpr::TokenUtils::lowprec_split_safe): init = the tokens
# between `=` and the tail, tail_op + tail = the rest.
sub _state_shape {
  my ($r, $sk) = @_;
  my $name = $sk->[1]->content;
  my $lp;
  if ($name =~ /^\$/
      && defined($lp = Pl::PExpr::TokenUtils::lowprec_idx($sk, 3))
      && Pl::PExpr::TokenUtils::lowprec_split_safe($sk, 3, $lp)) {
    return { %$r, decl => 'state', name => $name,
             init => [@$sk[3 .. $lp - 1]], tail_op => $sk->[$lp]->content,
             tail => [@$sk[$lp + 1 .. $#$sk]] };
  }
  return { %$r, decl => 'state', name => $name, init => [@$sk[3 .. $#$sk]] };
}

sub single_scalar_decl {
  my ($stmt) = @_;
  my @k = strip_semi($stmt->schildren);
  return () unless @k >= 2
    && $k[0]->isa('PPI::Token::Word') && $k[0]->content eq 'my'
    && $k[1]->isa('PPI::Token::Symbol') && $k[1]->content =~ /^\$\w+$/;
  my $name = $k[1]->content;
  return ($name, undef) if @k == 2;
  # `my $x if COND;` / `unless COND` (no init) — the legal non-constant-cond
  # stale-var idiom (closure.t mosquito/staleval).  Perl declares $x
  # unconditionally at COMPILE time; at runtime only COND is evaluated (with
  # the OUTER $x still visible — my-visibility starts after the statement).
  # The accidental cross-call value persistence when COND is false is
  # perl-undefined behaviour and not emulated (a fresh per-entry binding is
  # what the let gives; the tests only assert same-variable consistency).
  # Return the condition tokens (3rd value) so the caller void-evaluates them
  # BEFORE the let.  while/until/for stay unmatched → whole-file gate.
  if (@k >= 4 && $k[2]->isa('PPI::Token::Word')
      && $k[2]->content =~ /^(?:if|unless)$/) {
    return ($name, undef, [@k[3 .. $#k]]);
  }
  return () unless $k[2]->isa('PPI::Token::Operator') && $k[2]->content eq '=';
  return ($name, [@k[3 .. $#k]]);
}

# `my VAR <non-'=' trailing>;` — `my $aa, $bb, $cc;`, `my @raw, @up, @utf8;`,
# `my $a . $foo;`.  Perl declares ONLY VAR here and evaluates the rest as an
# ordinary expression, so the other names are PACKAGE variables (it warns
# "Parenthesize").  Returns VAR, or undef when the statement is not that shape.
#
# ONE predicate, two consumers (CLAUDE.md 11): the lowering in _lower_block and
# the lexical-name collector below.  They must not disagree about what a
# statement declares — while the collector answered "all of them" (its
# conservative unknown-shape branch), `my $a1, $b1; sub g { $b1 }` refused with
# "file lexical 'b1' captured by sub g" even though $b1 is a package global
# there and perl prints it happily (probed s393, #314).
sub lead_decl_with_expr_tail {
  my ($stmt) = @_;
  my @kd = strip_semi($stmt->schildren);
  return undef unless @kd >= 3
    && $kd[0]->isa('PPI::Token::Word') && $kd[0]->content eq 'my'
    && $kd[1]->isa('PPI::Token::Symbol') && $kd[1]->content =~ /^[\$\@\%]\w+$/
    && $kd[2]->isa('PPI::Token::Operator') && $kd[2]->content ne '=';
  return $kd[1]->content;
}

# `my ();` — a declaration with an EMPTY name list.  Legal Perl, declares
# nothing (perl #113554).
sub is_empty_my_decl {
  my ($stmt) = @_;
  my @k = strip_semi($stmt->schildren);
  return @k == 2
      && $k[0]->isa('PPI::Token::Word') && $k[0]->content eq 'my'
      && $k[1]->isa('PPI::Structure::List')
      && !@{ $k[1]->find('PPI::Token::Symbol') || [] };
}

# `my (undef) = RHS` — the same "declares no name" list, but WITH an
# initialiser, so it is NOT the `my ()` no-op: the RHS is a real list
# assignment that runs (#610).  Sibling of _is_empty_my_decl and deliberately
# disjoint from it (that one requires exactly two children, i.e. no `=`).
# `find` returns 0, not undef, when nothing matches — hence the `|| []`.
sub is_no_name_my_decl {
  my ($stmt) = @_;
  my @k = strip_semi($stmt->schildren);
  return @k >= 3
      && $k[0]->isa('PPI::Token::Word') && $k[0]->content eq 'my'
      && $k[1]->isa('PPI::Structure::List')
      && !@{ $k[1]->find('PPI::Token::Symbol') || [] }
      && $k[2]->isa('PPI::Token::Operator') && $k[2]->content eq '=';
}

# `my @a` / `my %h` / `my ($p, @q)` [= INIT] → (\@var_names, $has_init); else ().
# $KTOKS (#620) lets a caller that has already split a trailing statement
# modifier off the run pass the DECLARATION half in: `my @a = (1,2) if $c`
# declares exactly what `my @a = (1,2)` declares, but the raw token run puts
# `if` where this matcher wants `=`.  Callers that do not split are unchanged.
sub multi_decl {
  my ($stmt, $ktoks) = @_;
  my @k = $ktoks ? @$ktoks : strip_semi($stmt->schildren);
  return () unless @k >= 2
    && $k[0]->isa('PPI::Token::Word') && $k[0]->content eq 'my';
  my @vars;
  if ($k[1]->isa('PPI::Token::Symbol') && $k[1]->content =~ /^[\@\%]\w+$/) {
    @vars = ($k[1]->content);
  } elsif ($k[1]->isa('PPI::Structure::List')) {
    @vars = map  { $_->content }
            grep { $_->isa('PPI::Token::Symbol') } map { $_->tokens } $k[1];
    return () if !@vars || grep { !/^[\$\@\%]\w+$/ } @vars;
  } else {
    return ();
  }
  return (\@vars, 0) if @k == 2;
  return () unless $k[2]->isa('PPI::Token::Operator') && $k[2]->content eq '=';
  return (\@vars, 1);
}

# The `my-list` declaration's token run K (after any modifier split):
#   eq          index of the first depth-0 `=`, or undef
#   container   K[1] is a single `@x` / `%x` symbol
#   paren       K[1] is a parenthesised name list
#   names       K[1] itself (the declared name or name list)
sub decl_list_view {
  my ($k) = @_;
  my ($eq) = grep { $k->[$_]->isa('PPI::Token::Operator')
                    && $k->[$_]->content eq '=' } 0 .. $#$k;
  return {
    eq        => $eq,
    container => ($k->[1] && $k->[1]->isa('PPI::Token::Symbol')
                  && $k->[1]->content =~ /^[\@\%]\w+$/) ? 1 : 0,
    paren     => ($k->[1] && $k->[1]->isa('PPI::Structure::List')) ? 1 : 0,
    names     => $k->[1],
  };
}

# A chained declaration on a `my-list` RHS: `my ($a) = my $b = my @c = INIT`.
# Returns (\@names, \@rest): the names every leading `my NAMES =` link
# declares, and the run after the last link.  No link: ([], RHS).
sub decl_chain {
  my ($rhs) = @_;
  my @r = @$rhs;
  my @chain;
  while (@r >= 3
         && $r[0]->isa('PPI::Token::Word') && $r[0]->content eq 'my') {
    my @syms = $r[1]->isa('PPI::Token::Symbol')   ? ($r[1])
             : $r[1]->isa('PPI::Structure::List') ? (grep { $_->isa('PPI::Token::Symbol') }
                                                     map { $_->tokens } $r[1])
             : ();
    my @names = grep { /^[\$\@\%]\w+$/ } map { $_->content } @syms;
    last unless @names == @syms && @names
      && $r[2]->isa('PPI::Token::Operator') && $r[2]->content eq '=';
    push @chain, @names;
    @r = @r[3 .. $#r];
  }
  return (\@chain, \@r);
}

# ---- compound statements -------------------------------------------------------

sub compound_shape {
  my ($stmt) = @_;
  my @k = $stmt->schildren;
  my $label;
  if (@k && $k[0]->isa('PPI::Token::Label')) {
    ($label = (shift @k)->content) =~ s/\s*:\s*$//;
  }
  my %r = (label => $label, k => \@k);
  return { %r, ckind => 'empty' } unless @k;
  if ($k[0]->isa('PPI::Structure::Block')) {
    my ($cont, $missing);
    my ($ci) = grep { $k[$_]->isa('PPI::Token::Word')
                      && $k[$_]->content eq 'continue' } 0 .. $#k;
    if (defined $ci) {
      ($cont) = grep { $_->isa('PPI::Structure::Block') } @k[$ci + 1 .. $#k];
      $missing = $cont ? 0 : 1;
    }
    return { %r, ckind => 'block', block => $k[0], cont => $cont,
             cont_missing => $missing };
  }
  my $kw = $k[0]->content;
  $r{kw}   = $kw;
  $r{loop} = $kw =~ /^(?:while|until|for|foreach)$/ ? 1 : 0;
  return { %r, ckind => 'if', clauses => _if_clauses(\@k) }
    if $kw eq 'if' || $kw eq 'unless';
  if ($kw eq 'while' || $kw eq 'until') {
    my ($cond_s) = grep { $_->isa('PPI::Structure::Condition') } @k;
    my ($block)  = grep { $_->isa('PPI::Structure::Block') } @k;
    return { %r, ckind => 'while', cond => $cond_s, block => $block };
  }
  return _for_shape(\%r, \@k) if $kw eq 'for' || $kw eq 'foreach';
  return _try_shape(\%r, \@k) if $kw eq 'try';
  return { %r, ckind => 'other' };
}

sub _if_clauses {
  my ($k) = @_;
  my (@clauses, $cur_kw, $cur_cond);
  for my $el (@$k) {
    if    ($el->isa('PPI::Token::Word'))            { $cur_kw = $el->content }
    elsif ($el->isa('PPI::Structure::Condition'))   { $cur_cond = $el }
    elsif ($el->isa('PPI::Structure::Block')) {
      push @clauses, { kw => $cur_kw, cond => $cur_cond, block => $el };
      $cur_cond = undef;
    }
  }
  return \@clauses;
}

# for / foreach.  PPI builds `for my $x (LIST)` / `for (LIST)` with a
# Structure::For when the list holds no `;` -- that is a FOREACH list, and the
# lowering re-blesses it (the node is named in `rebless`; the arm blesses).
sub _for_shape {
  my ($r, $k) = @_;
  my ($mis) = grep { $_->isa('PPI::Structure::For') } @$k;
  my $rebless;
  if ($mis) {
    my $has_var  = grep { $_->isa('PPI::Token::Symbol') } @$k;
    my $has_semi = grep { $_->isa('PPI::Token::Structure') && $_->content eq ';' }
                   $mis->tokens;
    $rebless = $mis if $has_var || !$has_semi;
  }
  if ($mis && !$rebless) {
    my ($block) = grep { $_->isa('PPI::Structure::Block') } @$k;
    my $has_cont = grep { $_->isa('PPI::Token::Word') && $_->content eq 'continue' } @$k;
    my @sect = grep { $_->isa('PPI::Statement') } $mis->children;
    my ($init_s, $cond_s, $step_s) =
      map { $_ && !$_->isa('PPI::Statement::Null') ? $_ : undef } @sect[0 .. 2];
    return { %$r, ckind => 'for-c', for_s => $mis, block => $block,
             has_continue => $has_cont ? 1 : 0, nsections => scalar(@sect),
             init => $init_s, cond => $cond_s, step => $step_s };
  }
  my $cast = grep { $_->isa('PPI::Token::Cast') } @$k;
  my ($list)  = grep { $_->isa('PPI::Structure::List')
                       || ($rebless && $_ == $rebless) } @$k;
  my ($block) = grep { $_->isa('PPI::Structure::Block') } @$k;
  my ($var)   = grep { $_->isa('PPI::Token::Symbol') } @$k;
  my $loop_my = $var && do {
    my $p = $var->sprevious_sibling;
    $p && $p->isa('PPI::Token::Word') && $p->content eq 'my' ? 1 : 0;
  };
  my @parts = $list
    ? (map { $_->schildren } grep { $_->isa('PPI::Statement') } $list->children) : ();
  return { %$r, ckind => 'foreach', rebless => $rebless, cast => $cast ? 1 : 0,
           list => $list, block => $block, var => $var,
           name => ($var ? $var->content : '$_'), loop_my => $loop_my,
           list_parts => \@parts };
}

sub _try_shape {
  my ($r, $k) = @_;
  my (@blocks, $clist, $has_catch, $after_finally, $fin_block);
  for my $el (@$k[1 .. $#$k]) {
    if ($el->isa('PPI::Structure::Block')) {
      if ($after_finally) { $fin_block = $el } else { push @blocks, $el }
    }
    elsif ($el->isa('PPI::Structure::List'))  { $clist = $el }
    elsif ($el->isa('PPI::Token::Word')) {
      $has_catch = 1 if $el->content eq 'catch';
      $after_finally = 1 if $el->content eq 'finally';
    }
  }
  my @vs = $clist ? @{ $clist->find('PPI::Token::Symbol') || [] } : ();
  return { %$r, ckind => 'try', blocks => \@blocks, clist => $clist,
           has_catch => $has_catch, fin_block => $fin_block,
           catch_var => ((@vs == 1 && $vs[0]->content =~ /^\$\w+\z/)
                         ? $vs[0]->content : undef) };
}

# ---- break statements (return / next / last / redo / goto) -------------------

sub break_shape {
  my ($stmt) = @_;
  my @k = strip_semi($stmt->schildren);
  my $kw = $k[0]->content;
  if ($kw eq 'return') {
    shift @k;
    my ($expr, $mod, $cond);
    if (@k && Pl::PExpr::Config::is_statement_modifier_word($k[0])) {
      ($expr, $mod, $cond) = ([], $k[0]->content, [@k[1 .. $#k]]);
    } else {
      ($expr, $mod, $cond) = split_modifier(\@k);
    }
    my $is_comma = sub { $_[0]->isa('PPI::Token::Operator')
                         && ($_[0]->content eq ',' || $_[0]->content eq '=>') };
    my $top_comma = grep { $is_comma->($_) } @$expr;
    if (!$top_comma && @$expr == 1 && $expr->[0]->isa('PPI::Structure::List')) {
      my ($inner) = grep { $_->isa('PPI::Statement') } $expr->[0]->schildren;
      $top_comma = $inner && grep { $is_comma->($_) } $inner->schildren;
    }
    return { kw => $kw, expr => $expr, mod => $mod, cond => $cond,
             top_comma => $top_comma ? 1 : 0 };
  }
  my ($expr, $mod, $cond) = split_modifier(\@k);
  return { kw => $kw, expr => $expr, mod => $mod, cond => $cond };
}

# ---- expression statements -------------------------------------------------------
#
#   k         the statement's tokens without the `;`
#   yada      `...` alone (field line = its line number)
#   expr/mod/cond   the modifier split of k
#   assign    { name, op, rhs } -- `$name OP RHS` with no modifier, a plain
#             scalar on the left, a depth-0 operator second, and no
#             below-assignment tail (`$x = A, B` keeps the generic path)
#   incdec    { name, op, post } -- `$x++` / `--$x` with no modifier
#   bareword  the Word token, when the statement is one bare word

sub expr_shape {
  my ($stmt) = @_;
  my @k = strip_semi($stmt->schildren);
  if (@k == 1 && $k[0]->isa('PPI::Token::Operator') && $k[0]->content eq '...') {
    return { k => \@k, yada => 1, line => $k[0]->line_number // 0 };
  }
  my ($expr, $mod, $cond) = split_modifier(\@k);
  my %r = (k => \@k, expr => $expr, mod => $mod, cond => $cond);
  return \%r if $mod;
  if (@$expr >= 3
      && $expr->[0]->isa('PPI::Token::Symbol') && $expr->[0]->content =~ /^\$\w+$/
      && $expr->[1]->isa('PPI::Token::Operator')
      && !defined Pl::PExpr::TokenUtils::lowprec_idx($expr, 2)) {
    $r{assign} = { name => $expr->[0]->content, op => $expr->[1]->content,
                   rhs => [@$expr[2 .. $#$expr]] };
  }
  if (@$expr == 2) {
    my ($a, $b) = @$expr;
    my ($name, $opc, $post);
    if ($a->isa('PPI::Token::Symbol') && $b->isa('PPI::Token::Operator')) {
      ($name, $opc, $post) = ($a->content, $b->content, 1);
    } elsif ($a->isa('PPI::Token::Operator') && $b->isa('PPI::Token::Symbol')) {
      ($name, $opc, $post) = ($b->content, $a->content, 0);
    }
    $r{incdec} = { name => $name, op => $opc, post => $post }
      if defined $name && $name =~ /^\$\w+$/ && ($opc eq '++' || $opc eq '--');
  }
  $r{bareword} = $expr->[0] if @$expr == 1 && $expr->[0]->isa('PPI::Token::Word');
  return \%r;
}

1;

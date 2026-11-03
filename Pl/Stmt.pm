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
  return { kind => 'local' } if is_local_stmt($stmt);
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

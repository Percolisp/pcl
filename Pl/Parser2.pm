# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::Parser2;

# Parser2 (prototype) — the v2 statement-level pipeline: structured emission
# (Pl::CLForm) instead of an interleaved text stream.  SAME external API as
# Pl::Parser (parse_file / parse_code / parse → CL text with the
# "(in-package :pcl)" marker pl2cl rewrites), so the two pipelines are
# switchable: pl2cl picks this class when PCL_V2=1.
#
# Division of labour while v2 is being written:
#   - statement/scope/sub STRUCTURE is lowered natively to forms here —
#     nested `let`s at declaration sites (scoping by construction), real
#     lambda lists for `my (LIST) = @_` subs, no VOID `*wantarray*` wraps,
#     VarAnnotator-gated unboxed scalars;
#   - every EXPRESSION is parsed ONCE by PExpr and generated ONCE by
#     ExprToCL (the one expression compiler since s411, Phase A of
#     docs/plan-one-compiler-s411.md), reached through the v1 Pl::Parser
#     instance (`fallback_parser`) whose `_parse_expression_form` owns the
#     parse-error handling; the result is a CLForm tree whose raw residue is
#     only the genuinely-declining subtrees.  The two per-scope facts the
#     generator's Kind-A rules read (sub_info, let-bound lexicals) travel
#     with the call.
#
# Prototype boundary: unsupported statement kinds die with "Parser2 TODO:".

use v5.30;
use strict;
use warnings;
use Scalar::Util qw(refaddr weaken);
use Moo;
use PPI;
use Pl::Parser;
use Pl::Environment;
use Pl::PExpr;
use Pl::InterpScan ();
use Pl::VarAnnotator;
use Pl::Passes ();
use Pl::CLForm qw(raw raw_wrap);
use Pl::GlobalPartition qw(global_decl_form);

has filename => (is => 'ro', predicate => 1);
has code     => (is => 'ro', predicate => 1);

# E3 eval-mode (docs/v2-opus48-execution-plan.md §E3): set when transpiling a
# string for runtime `eval "..."` (pl2cl --eval-pkg / --server).  Free
# variables become p-eval-thunk lambda params instead of forward-decl defvars
# (docs/eval-lexical-capture.md); eval_pkg seeds the package so __PACKAGE__
# resolves to the call site's package.  Multi-segment eval strings (top-level
# `package` statements) die → the per-eval v1 retry (kept until E4).
has eval_mode => (is => 'ro', default => sub { 0 });
has eval_pkg  => (is => 'ro', default => sub { undef });
# #296-B1: the names of the caller's in-scope lexicals (the capture alist's
# keys, sent by p-transpile-string).  Read by exactly one pass —
# _rename_free_eval_captures — which needs to know whether a free `$a` in this
# eval string is the caller's `my $a` (compile it as that captured lexical) or
# the package's dynamic `$a` (leave today's special path alone).  perl draws
# the line in the same place, so the list is a FACT about the eval site, not a
# heuristic.  Empty/absent = "no captures known", i.e. today's behaviour.
has eval_captures => (is => 'ro', default => sub { [] });

# #364: the perl FEATURES in effect at the eval SITE.  perl's feature pragmas
# are lexical and a string eval inherits them, but PCL compiles the eval text
# in a separate process on the bare string — so `use feature 'try'; eval q{try
# {…} catch ($e) {…}}` parsed with no features at all and the construct became
# one swallowing statement.  The site knows the answer (PPI's
# ->presumed_features, fed by the #360 table), so it rides the server request
# next to eval_captures and seeds PPI's lexer here.  Like eval_captures, this
# is compiler INPUT and therefore part of the eval CACHE key (s387).
has eval_features => (is => 'ro', default => sub { [] });

# Pre-pass result, keyed by package: { pkg => { perl sub name →
# { cl_name, insensitive } } } (read by ExprToCL's `insensitive-call` rule).
# Bareword sub resolution is package-scoped in Perl, so ExprToCL/VarAnnotator
# only ever see the CURRENT package's slice (_cur_sub_info).
has sub_info => (is => 'rw', default => sub { {} });

# Package of the segment currently being lowered (see parse()'s segment split).
has cur_pkg => (is => 'rw', default => 'main');

sub _cur_sub_info {
  my $self = shift;
  return $self->sub_info->{ $self->cur_pkg } //= {};
}

has environment => (is => 'lazy');
sub _build_environment {
  my $self = shift;
  return Pl::Environment->new(
    source_file => $self->has_filename ? $self->filename : '-');
}

# The original parser, used purely as the expression-codegen engine.
has fallback_parser => (is => 'lazy');
sub _build_fallback_parser {
  my $self = shift;
  my $p = Pl::Parser->new(
    ($self->has_filename ? (filename => $self->filename) : ()),
    ($self->has_code     ? (code     => $self->code)     : ()),
    environment => $self->environment,
    # E3: mirror v1's eval-mode error contract in the expression seam — e.g.
    # assignment to a non-lvalue sub must DIE (failing the whole eval, the
    # CMM lvalue-probe idiom) instead of degrading to a PARSE ERROR comment.
    ($self->eval_mode         ? (eval_mode => 1)               : ()),
    (defined $self->eval_pkg  ? (eval_pkg  => $self->eval_pkg) : ()),
  );
  # The lexical registry lives HERE, on the owner (#153 chunk 0); the seam
  # parser reads it through lex_home.
  $self->{_let_bound_vars} //= {};
  # The seam's emission state and its (weak) owner back-reference are set by
  # the parser itself (Pl::Parser::become_seam): v1 text is produced on it only
  # inside capture_v1, whose drain hands the lines back by bucket name —
  # Parser2 never touches the sections.
  return $p->become_seam($self);
}

# EVAL MODE: the features the SITE told us about, in PPI's own shape, ready to
# seed the lexer (PPI::Lexer consults the document's feature_mods before the
# first token).  Empty in file mode — a file's own pragmas are in its text.
sub _eval_feature_seed {
  my ($self) = @_;
  my @feats = @{ $self->eval_features // [] } or return ();
  return (feature_mods => { map +($_ => 'perl'), @feats });
}

# The features in effect AT each string-eval site in this document, recorded
# against every enclosing statement so the lowering can look one up by the
# statement it is handed (refaddr, O(1)).  One find + one walk per eval; eval
# sites are rare, and nothing here runs per token (the #184 lesson).
#
# perl's answer is exact and lexical — a `no feature 'try'` in an inner block
# turns it off for an eval there and back on after — and PPI's
# ->presumed_features gives exactly that, now that the #360 table teaches it
# every spelling.  Only ENABLED features are recorded: they are what changes
# how the eval TEXT lexes, and carrying the disabled ones would put noise in
# the request and the cache key.
sub _scan_eval_site_features {
  my ($doc) = @_;
  my %by_stmt;
  for my $w (@{ $doc->find(sub {
                  $_[1]->isa('PPI::Token::Word') && $_[1]->content eq 'eval' }) || [] }) {
    my $f = eval { $w->presumed_features } or next;
    my @on = sort grep { $f->{$_} } keys %$f;
    next if !@on;
    for (my $p = $w->parent; $p; $p = $p->parent) {
      next if !$p->isa('PPI::Statement');
      # Union when one statement holds two eval sites with different answers:
      # the alternative is to pick one, and enabling is the direction that
      # keeps a program working.  No real code does this.
      my %seen = map +($_ => 1), @{ $by_stmt{ refaddr $p } || [] }, @on;
      $by_stmt{ refaddr $p } = [sort keys %seen];
    }
  }
  return \%by_stmt;
}

sub parse_file { my ($class, $fn, %opts) = @_; return $class->new(filename => $fn)->parse }
sub parse_code {
  my ($class, $code, %opts) = @_;
  return $class->new(
    code => $code,
    map { $_ => $opts{$_} } grep { defined $opts{$_} }
      qw(eval_mode eval_pkg eval_captures eval_features),
  )->parse;
}

# Pre-pass: `goto LABEL` (plain-word label form) whose nearest sub-like
# barrier — named sub body, anon `sub {}` block, or a `sort {}` comparator
# block — contains NO such label can never reach it: Perl raises the runtime
# error "Can't find label LABEL" (a sort block additionally forbids the jump
# outright).  Rewrite the two tokens to `die "Can't find label LABEL"` —
# byte-equivalent Perl semantics (the " at FILE line N" suffix is documented
# not-supported) that every lowering path consumes without special cases.
# map/grep/eval blocks are NOT barriers: Perl allows goto to leave those, so
# such gotos are genuine (the standalone-label pass gates the forward ones).
# Only the v2 PPI document is touched; a later gate re-parses the source.
sub _rewrite_unreachable_gotos {
  my ($self, $doc) = @_;
  for my $w (@{ $doc->find(sub { $_[1]->isa('PPI::Token::Word')
                                 && $_[1]->content eq 'goto' }) || [] }) {
    my $nx = $w->snext_sibling or next;
    next unless $nx->isa('PPI::Token::Word')
      && $nx->content =~ /^\w+$/ && $nx->content ne 'sub';
    my $lbl = $nx->content;
    my $barrier;
    for (my $p = $w->parent; $p; $p = $p->parent) {
      if ($p->isa('PPI::Statement::Sub')) { $barrier = $p; last }
      if ($p->isa('PPI::Structure::Block')) {
        my $prev = $p->sprevious_sibling;
        if ($prev && $prev->isa('PPI::Token::Word')
            && $prev->content =~ /^(?:sub|sort)$/) { $barrier = $p; last }
      }
    }
    next unless $barrier;
    next if $barrier->find_any(sub { $_[1]->isa('PPI::Token::Label')
                                     && $_[1]->content =~ /^\Q$lbl\E\s*:/ });
    $w->set_content('die');
    $nx->insert_before(PPI::Token::Quote::Double->new(qq{"Can't find label $lbl"}));
    $nx->delete;
  }
}

# Pre-pass (#63/t183): Perl's `our @a` binds the bare name to the variable of
# the package CURRENT AT THE DECLARATION, for the rest of the enclosing
# lexical scope — a later `package` statement in the same block does NOT
# re-home it.  The emission resolves bare names by section package, so a use
# after an in-block package switch would silently read the wrong package's
# variable (array.t #8910 block: `package tmp; (\our @a)->$#*++;
# package main; my @b = @a;` must copy tmp::a).  Requalify the uses in the
# switched region to the declaring package's Perl spelling (`@a` → `@tmp::a`,
# `$a[0]` → `$tmp::a[0]`, `$#a` → `$#tmp::a`), reusing the rename passes'
# family-aware rewriter.  Narrow activation — everything else keeps today's
# behavior: the our-decl must sit inside a Structure::Block, AFTER an
# in-block `package` statement (an our that inherits the block's outer
# package is out of scope here), with a later same-block package switch.
# Conservative dies (→ whole-file v1) on shapes the flat rewrite cannot
# honor: a re-declaration of the name, or a nested package statement, in the
# switched region.
sub _requalify_block_our_after_pkg_switch {
  my ($self, $doc) = @_;
  for my $w (@{ $doc->find(sub { $_[1]->isa('PPI::Token::Word')
                                 && $_[1]->content eq 'our' }) || [] }) {
    my $stmt = $w->statement or next;
    # An our embedded in a subexpression — `(\our @a)->$#*++` — reports the
    # INNER Statement::Expression; climb to the statement whose parent is
    # the enclosing block/document.
    while ($stmt->parent && !($stmt->parent->isa('PPI::Structure::Block')
                              || $stmt->parent->isa('PPI::Document'))) {
      my $up = $stmt->parent->statement or last;
      last if $up == $stmt;
      $stmt = $up;
    }
    my $blk = $stmt->parent;
    next unless $blk && $blk->isa('PPI::Structure::Block');
    my $nx = $w->snext_sibling or next;
    my @names = $nx->isa('PPI::Token::Symbol') ? ($nx->content)
              : $nx->isa('PPI::Structure::List')
                ? (map  { $_->content }
                   grep { $_->isa('PPI::Token::Symbol') } map { $_->tokens } $nx)
              : ();
    next unless @names && !grep { !/^[\$\@\%]\w+$/ } @names;
    my $decl_pkg;
    for (my $p = $stmt->sprevious_sibling; $p; $p = $p->sprevious_sibling) {
      next unless ref $p;
      if ($p->isa('PPI::Statement::Package')) { $decl_pkg = $p->namespace; last }
    }
    next unless defined $decl_pkg && $decl_pkg =~ /^\w+(?:::\w+)*$/;
    my ($cur, @switched) = ($decl_pkg);
    for (my $p = $stmt->snext_sibling; $p; $p = $p->snext_sibling) {
      next unless ref $p;
      if ($p->isa('PPI::Statement::Package')) { $cur = $p->namespace; next }
      next if $cur eq $decl_pkg;
      # RULED REFUSAL (M7 residue, #251; rephrased at the E4.1 flip, #242).
      # docs/not-supported.md: 'An our-alias whose requalified region contains
      # a nested package statement or an inner-scope re-declaration'.
      die "PCL: unsupported: `our` alias followed by a nested package"
        . " statement in the same block\n"
        if $p->isa('PPI::Node')
        && $p->find_any(sub { $_[1]->isa('PPI::Statement::Package') });
      push @switched, $p;
    }
    next unless @switched;
    for my $canon (@names) {
      my $sig = substr($canon, 0, 1);
      (my $bare = $canon) =~ s/^[\$\@\%]//;
      # An alias runs to the end of the block OR to the next declaration of
      # the same name, whichever comes first (M7, s355).  So a block-level
      # re-declaration does not defeat the requalification — it ENDS it: the
      # statements before it still belong to this alias, and the ones after
      # belong to the new binding, which gets its own turn in the outer loop
      # with its own decl_pkg.  Truncate rather than refuse.  (Role-Tiny's
      # subclass.t is the shape: one bare block declaring `our @ISA` in each
      # of four successive packages — ordinary Perl, and the whole file was
      # routing to v1 for it.)
      #
      # local is deliberately NOT a re-binder: it never re-binds the bare
      # name lexically — `local $a[3]` in the switched region still operates
      # on the requalified `@tmp::a` element (probe-verified vs perl), so the
      # flat rewrite stays correct.  Only my/our/state create new bindings.
      my @region;
      for my $s (@switched) {
        last if _block_level_redecl($s, $canon);
        # Any OTHER re-declaring shape keeps the conservative die, because
        # its binding ENDS before the block does and the outer alias then
        # resumes — a truncation would silently stop requalifying uses that
        # still belong to this alias.  Two live examples: a Compound HEAD
        # (`foreach my $a (…) {…}`, scoped to the loop) and an embedded decl
        # (`(\our @a)->$#*++`, not a Statement::Variable at all).
        #
        # SIGIL-EXACT (M7, s355 — the same lesson as M4): only a declaration
        # of THIS canonical variable re-binds it.  `foreach my $d (…)` binds
        # the SCALAR $d and leaves the `@d` alias alone, so the old
        # sigil-blind `[\$\@\%]` refused a requalification that was never
        # ambiguous — and the v1 fallback it dropped into then produced the
        # empty list where perl gives (1,2), probe-verified.
        # RULED REFUSAL (M7 residue, #251; rephrased at the E4.1 flip, #242).
        # docs/not-supported.md: 'An our-alias whose requalified region
        # contains a nested package statement or an inner-scope
        # re-declaration'.
        die "PCL: unsupported: `our` alias for '$sig$bare' re-declared in an"
          . " inner scope of the same block\n"
          if $s->isa('PPI::Node')
          && $s->content =~ /\b(?:my|our|state)\b[^;=]*\Q$sig$bare\E\b/;
        push @region, $s;
      }
      next unless @region;
      $self->_rewrite_var_uses(\@region, $canon, "${decl_pkg}::${bare}");
    }
  }
}

# Is $stmt a plain block-level `my`/`our`/`state` declaration STATEMENT that
# (re)binds $canon?  This is the one re-declaration shape
# _requalify_block_our_after_pkg_switch can honour by truncating: such a
# binding runs from here to the end of the enclosing block, so it cleanly
# partitions the block into "before = the old alias, after = this one".
# Every other shape (a Compound head, an embedded `our`, a decl nested in an
# inner block or sub) binds for a SHORTER extent than the rest of the block,
# which a truncation cannot express — those keep the caller's die.
sub _block_level_redecl {
  my ($stmt, $canon) = @_;
  return 0 unless ref $stmt && $stmt->isa('PPI::Statement::Variable');
  my $kw = ($stmt->schildren)[0];
  return 0 unless $kw && $kw->isa('PPI::Token::Word')
               && $kw->content =~ /^(?:my|our|state)$/;
  my @k = _strip_semi($stmt->schildren);
  return 0 unless @k >= 2;
  my @declared = $k[1]->isa('PPI::Token::Symbol') ? ($k[1]->content)
               : $k[1]->isa('PPI::Structure::List')
                 ? (map  { $_->content }
                    grep { $_->isa('PPI::Token::Symbol') } map { $_->tokens } $k[1])
               : ();
  return scalar grep { $_ eq $canon } @declared;
}

# Variables an in-block `package X;` must NOT re-home: perl itself keeps these
# in main whatever package is in effect, so a switch cannot move them ($_, @_,
# @ARGV/$ARGV/@ARGVOUT, %ENV, @INC/%INC, %SIG; %_args is PCL's own arg vector,
# same status).  This answers a DIFFERENT question from
# _forward_global_decls's %runtime_vars ("does this name need a defvar?"),
# which is why the two lists are separate and neither is derived from the
# other — keep them in step by cause, not by copy.
#
# `$a`/`$b` are NOT here (#287, s380).  They were, between s378 and s380, for
# a reason that has since been fixed at its own layer: the sort lowering used
# to hard-code `(lambda ($a $b) …)`, i.e. the SECTION's two symbols, so
# requalifying a `sort { $a <=> $b }` inside a switched region left the body
# reading X::$a while the lambda still bound the section's $a — a working sort
# turned silently wrong.  The lowering now binds the pair the comparator block
# actually reads (Pl::PExpr::_sort_pair, keyed on _pkg_region_at below), which
# is also what perl does: sort sets the $a/$b of the package the sort was
# COMPILED in.  @a/@b are ordinary globals and were never in this set.
my %PKG_SWITCH_IMMUNE_VARS = map { $_ => 1 }
  qw($_ @_ %_args @ARGV $ARGV @ARGVOUT @INC %ENV %INC %SIG);

# (#239) Perl's `package X;` inside a BLOCK re-homes every unqualified
# package variable for the REST OF THAT BLOCK — the switch is lexical, ends
# with the block, and reaches nested blocks and nested sub bodies inside the
# region exactly as a file-level `package X;` reaches the rest of the file.
#
# v2 emits a block as ONE top-level CL form, and CL's reader interns every
# symbol in a top-level form BEFORE evaluation, so an `(in-package :X)`
# nested inside that form cannot change how the symbols around it were read:
# every bare `$z` in the region had already been interned in the ENCLOSING
# package.  A file-level `package X;` works precisely because D1-lite splits
# it into SEPARATE top-level forms — which is also why the plain bare block,
# the one shape D1-lite splits, was the only one measuring correct: probed
# s378 across nine block kinds, eval/do/sub/BEGIN/if/while/label/sort all
# wrote the ENCLOSING package's variable where perl writes X's.
#
# So requalify the region's bare names to X's Perl spelling up front, reusing
# the `our` trigger's family-aware rewriter ($z → $X::z, @a → @X::a,
# $a[0] → $X::a[0], $#a → $#X::a, and every interpolated spelling of those).
# Each bare name is classified four ways (RULED s377,
# docs/fable-answers-s376.md §7), from the scope walk below rather than a
# fresh regex walk:
#   (a) lexical — a my/state binding in scope, or an `our` ALIAS in scope,
#       whose home is the DECLARING package, never X and never the enclosing
#       one.  A my/state is left alone; an `our` is requalified to ITS home,
#       because leaving it alone would let the section package answer (the
#       s377 §9.1 finding: `our $x; { package Bar; $x = "X" }` wrote Bar::x
#       where perl writes main::x — the same family diverging in the
#       OPPOSITE direction in the bare-block path this pass now also covers);
#   (b) magic/special — %PKG_SWITCH_IMMUNE_VARS above, plus every name that
#       is not word-shaped ($1, $@, ${^WARNING_BITS}, …);
#   (c) already qualified — including a name _requalify_block_our_after_
#       pkg_switch has already rewritten, which is why this pass runs AFTER
#       it, and a name an inner switched region already claimed, which is
#       why the blocks are processed deepest-first;
#   (d) none of the above → a global of X → requalify, sigil-family-aware.
# A name whose declarator the resolver cannot READ dies (rule 12) rather
# than receiving a silently-wrong package home.
#
# SCOPE: variable symbols only.  Sub definitions, `*glob` installs and
# bareword CALLS inside such a region were probed s378 and are ALREADY homed
# to X correctly — they resolve through the package stack at lowering time,
# not through the CL reader, so they never had this bug.
sub _requalify_block_globals_after_pkg_switch {
  my ($self, $doc) = @_;
  my @blocks;
  for my $blk (@{ $doc->find('PPI::Structure::Block') || [] }) {
    # `package X { … }` (the block form) does not switch its ENCLOSING
    # scope, so it is not a region opener; its body is an ordinary section.
    next unless grep { $_->isa("PPI::Statement::Package") && !_pkg_stmt_has_block($_) }
                $blk->schildren;
    my $depth = 0;
    $depth++ for _ancestors($blk);
    push @blocks, [$depth, $blk];
  }
  for my $e (sort { $b->[0] <=> $a->[0] } @blocks) {
    $self->_requalify_switched_regions($e->[1]);
  }
  return;
}

# `package NAME { … }` (the block form) confines its package to the block and
# does NOT switch the enclosing scope, so it opens no region.  PPI 1.291's
# Statement::Package has no ->block accessor — ask the children.
sub _pkg_stmt_has_block {
  my ($stmt) = @_;
  return scalar grep { $_->isa('PPI::Structure::Block') } $stmt->schildren;
}

sub _ancestors {
  my ($node) = @_;
  my @a;
  for (my $p = $node->parent; $p; $p = $p->parent) { push @a, $p }
  return @a;
}

# Split one block into its package-switched regions and requalify each.  A
# region runs from a `package X;` statement to the next one (or the end of
# the block); the statements BEFORE the first switch keep the enclosing
# package and are never touched.
sub _requalify_switched_regions {
  my ($self, $blk) = @_;
  my ($pkg, @region, @regions);
  for my $s ($blk->schildren) {
    if ($s->isa("PPI::Statement::Package") && !_pkg_stmt_has_block($s)) {
      push @regions, [$pkg, [@region]] if defined $pkg && @region;
      ($pkg, @region) = ($s->namespace);
      next;
    }
    push @region, $s if defined $pkg;
  }
  push @regions, [$pkg, [@region]] if defined $pkg && @region;
  for my $r (@regions) {
    my ($p, $stmts) = @$r;
    next unless defined $p && $p =~ /^\w+(?:::\w+)*$/;
    $self->_requalify_region($stmts, $p);
  }
  return;
}

sub _requalify_region {
  my ($self, $stmts, $pkg) = @_;
  # Candidate names: every variable the region MENTIONS, in any of the three
  # spellings a name can reach the emitter through — a Symbol token, an
  # ArrayIndex ($#a), or interpolating text with no token of its own
  # ("$v" inside a string is the only appearance some globals have).
  my %cand;
  for my $stmt (@$stmts) {
    next unless ref $stmt && $stmt->isa('PPI::Node');
    $cand{ $_->symbol } = 1 for @{ $stmt->find('PPI::Token::Symbol') || [] };
    for my $ai (@{ $stmt->find('PPI::Token::ArrayIndex') || [] }) {
      $cand{ '@' . substr($ai->content, 2) } = 1 if $ai->content =~ /^\$\#\w+$/;
    }
    _interp_canon($stmt, undef, undef, \%cand);   # undef $live = collect all
  }
  for my $canon (sort keys %cand) {
    my $sig = substr($canon, 0, 1);
    (my $bare = $canon) =~ s/^[\$\@\%]//;
    next unless $sig =~ /^[\$\@\%]$/;          # &sub / *glob: not this axis
    next if $bare =~ /::/;                     # (c) already qualified
    next unless $bare =~ /^[A-Za-z_]\w*$/;     # (b) $1, $@, ${^X}, …
    next if $PKG_SWITCH_IMMUNE_VARS{$canon};   # (b)
    my %homes;                                 # (a) `our` homes seen, ne $pkg
    my $binding = sub {
      my ($tok) = @_;
      # A my/state/our DECLARATOR names a new binding, not a use of the
      # package global.  `local` is deliberately absent: it never re-binds
      # the bare name lexically, so `local $v` in the region localizes the
      # REQUALIFIED global and its symbol must be rewritten with the rest
      # (the same rule the `our` trigger above states and probe-verified).
      return 'lex' if $tok->isa('PPI::Token::Symbol')
        && $self->_symbol_is_declarator($tok, qr/^(?:my|state|our)$/);
      return $self->_binding_at($tok, $canon);
    };
    $self->_rewrite_var_uses($stmts, $canon, "${pkg}::${bare}", undef, sub {
      my $b = $binding->($_[0]);
      return 0 if !defined $b;                 # (d) global of X → rewrite
      return 1 if $b eq 'lex';                 # (a) my/state → leave alone
      return 0 if $b eq $pkg;                  # (a) our alias already homed here
      $homes{$b} = 1;                          # (a) our alias elsewhere → pass 2
      return 1;
    });
    # Pass 2 — the `our` aliases, each to its OWN declaring package.  It runs
    # second because pass 1's $skip is what discovers them: every token that
    # could match this canon is offered to it exactly once.
    for my $home (sort keys %homes) {
      $self->_rewrite_var_uses($stmts, $canon, "${home}::${bare}", undef, sub {
        my $b = $binding->($_[0]);
        return defined($b) && $b eq $home ? 0 : 1;
      });
    }
  }
  return;
}

# What binds $canon at this token: 'lex' (a my/state in scope), a PACKAGE
# NAME (an `our` alias in scope, whose home is the package in effect at the
# DECLARATION), or undef (nothing — so it is a global of whatever package is
# in effect here).  The walk is the ordinary Perl scope walk: at every
# enclosing scope, the declarations that precede us in it, plus the scope's
# own HEAD (`sub f($x)`, `sub ($x) {…}`, `foreach my $x (…)`,
# `if (my $x = …)`, `for (my $i = 0; …)`) — which binds for the whole block.
# Unlike _ref_shadowed (which answers a narrower, segment-local question for
# the span gates and deliberately stops at the segment parent) this one must
# climb all the way to the Document, because a FILE lexical declared outside
# the block is in scope inside it.
sub _binding_at {
  my ($self, $node, $canon) = @_;
  my $child = $node;
  for (my $p = $node->parent; $p; $child = $p, $p = $p->parent) {
    next unless $p->isa('PPI::Structure::Block') || $p->isa('PPI::Document');
    # LAST declaration before the use wins, not the first: a scope may
    # re-declare the name (`our @v = (1,2); … our @v = (9);`) and Perl binds
    # the most recent one.  Taking the first made a use after the second
    # `our` read the FIRST one's package (probed live: gate row
    # "our-alias re-declaration boundaries" printed M7a's value for main's).
    my $found;
    for my $sib ($p->schildren) {
      last if $sib == $child;
      my $b = $self->_stmt_binding($sib, $canon);
      $found = $b if defined $b;
    }
    return $found if defined $found;
    next unless $p->isa('PPI::Structure::Block');
    my $b = $self->_head_binding($p, $canon);
    return $b if defined $b;
  }
  return undef;
}

# The binding a STATEMENT at scope level contributes (see _binding_at).
sub _stmt_binding {
  my ($self, $stmt, $canon) = @_;
  return 'lex' if $self->_stmt_declares_canon($stmt, $canon);
  return undef unless ref $stmt && $stmt->isa('PPI::Statement::Variable');
  my $kw = ($stmt->schildren)[0];
  return undef unless $kw && $kw->isa('PPI::Token::Word') && $kw->content eq 'our';
  return undef unless grep { $_ eq $canon } $self->_declared_names($stmt);
  return _pkg_in_effect_at($stmt);
}

# The binding a block's HEAD contributes to the block: everything written
# before the `{` inside the same construct.  A head declaration is scoped to
# the construct, so it binds for the whole block and nothing after it.
sub _head_binding {
  my ($self, $blk, $canon) = @_;
  my $par = $blk->parent or return undef;
  my @head;
  for my $sib ($par->schildren) {
    last if $sib == $blk;
    push @head, $sib;
  }
  return $self->_decl_binding_in(\@head, $canon);
}

# Scan a run of elements for a my/state/our declaration of $canon.  Used for
# block heads only, where the declarators are loose TOKENS of the enclosing
# statement (`foreach my $x (…) {…}`) as often as they are nested in a list
# (`if (my $x = …) {…}`, `sub f($x) {…}`).
sub _decl_binding_in {
  my ($self, $elems, $canon) = @_;
  my $found;                                   # last one wins, as in _binding_at
  for my $e (@$elems) {
    # A SIGNATURE (`sub f ($x, $y = 0) {…}`) binds its parameters for the
    # whole block with no my/state/our keyword at all (s379 review probe:
    # the body's $x was requalified to $X::x while the seam still bound the
    # plain lexical — silent wrong value).  This PPI tokenizes prototypes
    # AND signatures as Token::Prototype, so the named-params discriminator
    # is the same textual one _is_pure_prototype uses, and the param NAMES
    # come from the same splitter the v1 seam binds them with — a Symbol
    # inside a DEFAULT expression is a use, not a parameter, and stays
    # eligible for requalification.  A pure prototype binds nothing.
    if ($e->isa('PPI::Token::Prototype') || $e->isa('PPI::Structure::Signature')) {
      my $str = $e->content;
      next if $str !~ /[\$\@\%]\w/;
      $str =~ s/^\s*\(\s*//;                   # the splitter expects the parens
      $str =~ s/\s*\)\s*$//;                   # already peeled, as its caller does
      $found = 'lex' if grep { ($_->{name} // '') eq $canon }
        @{ $self->fallback_parser->_signature_param_specs($str) };
      next;
    }
    my @words = $e->isa('PPI::Token::Word') ? ($e)
              : $e->isa('PPI::Node')
                ? @{ $e->find(sub { $_[1]->isa('PPI::Token::Word') }) || [] }
              : ();
    for my $w (@words) {
      next unless $w->content =~ /^(?:my|state|our)$/;
      # Only a real DECLARATOR position counts.  The same three words are
      # legal perl elsewhere and must neither bind nor die (s379 review
      # probe: `if ($h{my}) { package P; … }` died "unclassifiable"):
      #   - a method name (`$obj->my(...)`);
      #   - a fat-comma key (`my => 1`) — always the string;
      #   - a bare hash-subscript key (`$h{my}`) — no next sibling;
      #   - a lexical sub (`my sub f {…}`) — declares no VARIABLE, and its
      #     name can never collide with $canon's symbol space.
      my $pv = $w->sprevious_sibling;
      next if $pv && $pv->isa('PPI::Token::Operator') && $pv->content eq '->';
      my $nx = $w->snext_sibling;
      next if !$nx;
      next if $nx->isa('PPI::Token::Operator') && $nx->content eq '=>';
      next if $nx->isa('PPI::Token::Word') && $nx->content eq 'sub';
      my @syms = $nx->isa('PPI::Token::Symbol')  ? ($nx)
               : $nx->isa('PPI::Structure::List')
                 ? @{ $nx->find('PPI::Token::Symbol') || [] }
               : undef;
      # RULE 12: a declarator whose declared names we cannot READ would make
      # every use of $canon in the region a guess between "lexical, leave
      # alone" and "global of X, requalify" — and the wrong guess is a
      # silently mis-homed value, not a crash.  Say so instead.
      die "PCL: unclassifiable `" . $w->content . "` declarator in a"
        . " package-switched block (resolving $canon)\n"
        if grep { !defined } @syms;
      next unless grep { $_->symbol eq $canon } @syms;
      $found = $w->content eq 'our' ? _pkg_in_effect_at($w) : 'lex';
    }
  }
  return $found;
}

# The package a `package NAME;` statement has put in effect at this node:
# the nearest one that PRECEDES it in its own scope or any enclosing one.
# A `package` inside a preceding sibling BLOCK never leaks out of it, which
# is exactly why the walk looks at siblings and never inside them.
sub _pkg_in_effect_at {
  my ($node) = @_;
  my $stmt = _pkg_stmt_in_effect_at($node);
  return $stmt ? $stmt->namespace : 'main';
}

# The `package NAME;` STATEMENT itself, or undef when none is in effect.  ONE
# walk with two readers — the namespace above and the switched-region test
# below (rule 11: do not grow a third package-in-effect resolver).
sub _pkg_stmt_in_effect_at {
  my ($node) = @_;
  my $child = $node;
  for (my $p = $node->parent; $p; $child = $p, $p = $p->parent) {
    for (my $s = $child->sprevious_sibling; $s; $s = $s->sprevious_sibling) {
      return $s
        if $s->isa("PPI::Statement::Package") && !_pkg_stmt_has_block($s);
    }
  }
  return undef;
}

# The package of the BLOCK-LEVEL switched region this node sits in, or undef.
#
# The distinction matters to every consumer that has to know whether the CL
# READER already interned the surrounding bare names in this package or in
# the enclosing one.  A FILE-level `package X;` is split by D1-lite into its
# own top-level form, so a bare `$a` there is read as X's `$a` and nothing
# needs rewriting; a BLOCK-level one is not, which is the whole reason
# _requalify_block_globals_after_pkg_switch rewrites the region's source
# spellings.  So "undef" here means exactly "this node's bare names were NOT
# requalified", and the qualifying condition is deliberately the same one
# that pass uses: a `package X;` STATEMENT (never the `package X { … }` block
# form) whose parent is a Block, with a word-shaped namespace.
sub _pkg_region_at {
  my ($node) = @_;
  my $stmt = _pkg_stmt_in_effect_at($node) or return undef;
  my $par  = $stmt->parent                 or return undef;
  return undef unless $par->isa('PPI::Structure::Block');
  my $ns = $stmt->namespace;
  return undef unless defined $ns && $ns =~ /^\w+(?:::\w+)*$/;
  return $ns;
}

# Merge prototypes declared by every use/require in the document (nested ones
# included — pack.t's `BEGIN { require './test.pl' }`) into the shared
# Environment, mirroring v1's statement-time extraction (use →
# _extract_module_prototypes with the import list; require → module form, or
# literal non-interpolating file path via _extract_file_prototypes).  See the
# parse() call-site comment for why this must run before expression parsing.
# E4.1 M1 (RULED s353, fable-answers-s352.md §1.2): may segment 0's leading
# statements be swept INTO the eval package region?  True only when every
# one is a single-scalar `my` declaration whose initializer is built from
# WHITELISTED material: scalars declared by an EARLIER leading `my`, magic
# variables (`$_[1]`, `@_`, `$_` — runtime-special, never *package*-interned),
# literals with no live sigil, operators/casts/subscripts/parens over those.
# The swept-in statements then run inside the region thunk, i.e. under
# `*package*` = X; the whitelist exists precisely to make that binding
# unobservable — a free package name, an interpolated global, or a bareword
# call would resolve in X where perl resolves it in the CALLER's package
# (the #240 silent-wrong, one statement earlier).  WHITELIST, never a
# blacklist: anything unrecognized refuses (rule 12; the residue risk is a
# scanner missing a spelling — the s332 §9 lesson).  This is Sub::Quote's
# accessor/constructor shape, Moo's code generator:
#   { my $x = ${$_[1]->{"\$x"}}; …; package X; sub NAME { … } }
sub _eval_safe_leading_stmts {
  my ($self, $stmts) = @_;
  my %own;   # scalars declared by earlier leading statements
  for my $stmt (@$stmts) {
    return 0 unless $stmt->isa('PPI::Statement::Variable');
    my @k = grep { $_->significant } $stmt->children;
    pop @k if @k && $k[-1]->isa('PPI::Token::Structure')
                 && $k[-1]->content eq ';';
    return 0 unless @k >= 2
      && $k[0]->isa('PPI::Token::Word') && $k[0]->content eq 'my'
      && $k[1]->isa('PPI::Token::Symbol')
      && $k[1]->content =~ /^\$\w+$/;
    my $target = $k[1]->content;
    # Shape: `my $x;` or `my $x = INIT;` — attributes, declarator
    # modifiers, list targets all refuse.
    my @init;
    if (@k > 2) {
      return 0 unless $k[2]->isa('PPI::Token::Operator')
        && $k[2]->content eq '=';
      @init = @k[3 .. $#k];
      return 0 unless @init;
    }
    for my $el (@init) {
      for my $t ($el->isa('PPI::Node') ? $el->tokens : ($el)) {
        next unless $t->significant;
        next if $t->isa('PPI::Token::Magic');    # BEFORE Symbol: Magic ISA Symbol
        if ($t->isa('PPI::Token::Symbol')) {
          return 0 unless $own{ $t->content };   # own target NOT allowed: in
          next;                                  # `my $x = $x` the RHS is the
        }                                        # caller's — unknowable here
        next if $t->isa('PPI::Token::Number');
        next if $t->isa('PPI::Token::Quote::Single')
             || $t->isa('PPI::Token::Quote::Literal');
        if ($t->isa('PPI::Token::Quote::Double')
            || $t->isa('PPI::Token::Quote::Interpolate')) {
          # Interpolation resolves free names too — allow only when no LIVE
          # sigil survives escape stripping ("\$x" is literal text).
          (my $body = $t->content) =~ s/\\.//g;
          return 0 if $body =~ /[\$\@]/;
          next;
        }
        next if $t->isa('PPI::Token::Operator');
        next if $t->isa('PPI::Token::Cast');
        next if $t->isa('PPI::Token::Structure');
        return 0;   # Words (bareword calls), heredocs, ArrayIndex, anything new
      }
    }
    $own{$target} = 1;
  }
  return 1;
}

# M1's companion for the TRAILING statements of the flattened-block shape:
# true when every statement is built from literals/operators/structure only
# (`1;` — the routine eval-truth tail).  No symbols, no words: a trailing
# expression with a name in it would resolve in the region package X where
# perl, back outside the block, resolves it in the CALLER's package.
sub _eval_literal_only_stmts {
  my ($stmts) = @_;
  for my $stmt (@$stmts) {
    return 0 unless $stmt->isa('PPI::Statement');
    for my $t ($stmt->tokens) {
      next unless $t->significant;
      next if $t->isa('PPI::Token::Number');
      next if $t->isa('PPI::Token::Quote::Single')
           || $t->isa('PPI::Token::Quote::Literal');
      next if $t->isa('PPI::Token::Operator');
      next if $t->isa('PPI::Token::Structure');
      return 0;
    }
  }
  return 1;
}

# Pre-seed the strict_subs pragma from the document's use/no strict includes
# (see the parse() call site).  Mirrors v1's per-statement rule — bare
# `use strict` or an arg list mentioning 'subs' counts; `no strict 'refs'`
# does not.  v1 flips the flag linearly as statements process; up front we
# can only pick ONE state, so: strict iff a qualifying `use strict` exists
# and no qualifying `no strict` does (a file that mixes both keeps today's
# lenient default, exactly what its no-strict regions require).  The
# in-stream statement fallbacks still re-set the flag at their positions.
sub _premerge_strict_pragma {
  my ($self, $doc) = @_;
  my ($use_strict, $no_strict);
  for my $inc (@{ $doc->find('PPI::Statement::Include') || [] }) {
    next if ($inc->module // '') ne 'strict';
    my @args = map { $_->string }
               grep { $_->isa('PPI::Token::Quote') } $inc->children;
    next if @args && !grep { /\bsubs\b/ } @args;
    if (($inc->type // '') eq 'no') { $no_strict = 1 } else { $use_strict = 1 }
  }
  $self->environment->set_pragma('strict_subs', 1) if $use_strict && !$no_strict;
}

# Register `*NAME = sub () { ... }` glob-installed constant subs as zero-arg
# prototypes (see the parse() call site).  Only the EXPLICIT empty prototype
# registers — a plain `*NAME = sub {...}` or `*NAME = \&other` has no arity
# contract to learn.  `local *NAME = ...` is skipped (temporary rebind, not a
# declaration).  Package-qualified targets register under the bare name too:
# PExpr's known-word checks look up the spelling at the call site.
sub _premerge_glob_const_prototypes {
  my ($self, $doc) = @_;
  for my $stmt (@{ $doc->find('PPI::Statement') || [] }) {
    my @k = grep { $_->significant } $stmt->children;
    next if @k < 4;
    next if !($k[0]->isa('PPI::Token::Symbol')
              && $k[0]->content =~ /^\*(?:\w+(?:::\w+)*::)?(\w+)$/);
    my $name = $1;
    next if !($k[1]->isa('PPI::Token::Operator') && $k[1]->content eq '=');
    # `*NAME = \&OTHER` inside BEGIN (task #83): the glob alias runs during
    # compilation, so perl treats a later bare `NAME` as a sub CALL, not a
    # string.  Register NAME with the default prototype-less signature (the
    # same shape the sub pre-pass gives a plain `sub NAME`); the runtime
    # glob-assign installs the function the call resolves to.  BEGIN-gated
    # on purpose: a plain runtime `*NAME = \&OTHER` leaves later barewords
    # as strings in perl too (no compile-time knowledge) — current behavior
    # is already correct there.
    if ($k[2]->isa('PPI::Token::Cast') && $k[2]->content eq '\\'
        && $k[3]->isa('PPI::Token::Symbol') && $k[3]->content =~ /^&/) {
      my $in_begin = 0;
      for (my $p = $stmt->parent; $p; $p = $p->parent) {
        if ($p->isa('PPI::Statement::Scheduled')
            && ($p->type // '') eq 'BEGIN') { $in_begin = 1; last }
      }
      next unless $in_begin;
      $self->environment->add_prototype($name,
        { params => [], min_params => -1, is_proto => 0 });
      next;
    }
    next if !($k[2]->isa('PPI::Token::Word') && $k[2]->content eq 'sub');
    next if !($k[3]->isa('PPI::Token::Prototype') && $k[3]->content eq '()');
    $self->environment->add_prototype($name, {
      params     => [],
      min_params => 0,
      max_params => 0,
      is_proto   => 1,
      proto_string => '',
    });
  }
}

sub _premerge_include_prototypes {
  my ($self, $doc) = @_;
  my $fp = $self->fallback_parser;
  # Modules v1's use-branch short-circuits BEFORE its extraction call —
  # never extract these (v1 order: version pragma / overload / base+parent /
  # the pragma list / lib).  `use feature` in particular reaches PPI shapes
  # the module transpiler warns about ("Handle single node of unknown
  # type") — and pl2cl's stderr must stay clean: test harnesses capture it.
  my $skip = qr/^(?:overload|base|parent|lib|strict|warnings|warnings::register|
                   feature|utf8|open|bytes|locale|integer|builtin|overloading|
                   XSLoader|DynaLoader|re)$/x;
  # `use lib "dir"` paths must reach the TRANSPILE-TIME search list before
  # the extraction loop below, or a module the file itself puts on @INC is
  # never found and its prototypes (block-form `(&@)` etc.) are silently
  # lost.  v1 got this for free from statement ORDER (its use-lib branch
  # unshifts onto inc_paths as it walks); this pre-pass runs before any
  # statement, so seed it here.  Literal strings and qw() only — an
  # interpolated path is runtime-computed (the #235 family) and stays out.
  for my $inc (@{ $doc->find('PPI::Statement::Include') || [] }) {
    next unless ($inc->type // '') eq 'use' && ($inc->module // '') eq 'lib';
    for my $child ($inc->schildren) {
      if ($child->isa('PPI::Token::Quote')) {
        my $path = $child->string;
        unshift @{ $fp->inc_paths }, $path
          if $path !~ /[\$\@]/ || $child->isa('PPI::Token::Quote::Single');
      } elsif ($child->isa('PPI::Token::QuoteLike::Words')) {
        (my $c = $child->content) =~ s/^qw\s*[\(\[\{<]//;
        $c =~ s/[\)\]\}>]$//;
        unshift @{ $fp->inc_paths }, grep { length } split /\s+/, $c;
      }
    }
  }
  for my $inc (@{ $doc->find('PPI::Statement::Include') || [] }) {
    my $type = $inc->type // '';
    if ($type eq 'use') {
      my $module = $inc->module or next;
      next if $module =~ $skip;
      my $menv = $fp->_extract_module_prototypes($module) or next;
      $fp->_merge_module_prototypes($menv, [ $fp->_parse_use_import_list($inc) ]);
    } elsif ($type eq 'require') {
      if (my $module = $inc->module) {
        my $menv = $fp->_extract_module_prototypes($module) or next;
        $fp->_merge_module_prototypes($menv, undef);
        next;
      }
      my ($q) = grep { $_->isa('PPI::Token::Quote') } $inc->schildren;
      next unless $q;
      my $interpolating = $q->isa('PPI::Token::Quote::Double')
                       || $q->isa('PPI::Token::Quote::Interpolate');
      my $path = $q->string;
      next if $interpolating && $path =~ /[\$\@]/;   # runtime-computed → v1 rule: skip
      my $fenv = $fp->_extract_file_prototypes($path) or next;
      $fp->_merge_module_prototypes($fenv, undef);
    }
  }
}

sub _source {
  my $self = shift;
  return $self->code if $self->has_code;
  open my $fh, '<', $self->filename or die "Parser2: cannot read " . $self->filename . ": $!";
  local $/;
  return scalar <$fh>;
}

# D7 (extended): a PExpr parse mutates shared PPI state in TWO ways — token
# CONTENT (the fat-comma `=>` → `,` rewrite) and ad-hoc parse-state keys
# stored on the PPI elements themselves.  `_bareword_string` is the toxic
# one: it means "this word was UNKNOWN at parse time, emit it as a string",
# so an analysis parse that runs BEFORE a `use constant`/sub registration
# poisons the later real parse (found via split.t: `my $w = nought;` after
# `use constant nought => 0;` emitted `(pl-"nought")`).  Snapshot both;
# restore puts back the exact prior state, deleting keys that did not exist.
# Used by _lower_expr's native attempt AND VarAnnotator's analysis parses.
our @PPI_ADHOC_KEYS = qw(_bareword_string _has_match_context _pcl_decl_list);

sub _ppi_state_snapshot {
  my @parts = grep { ref $_ } @_;
  my @elems = map { $_->isa('PPI::Node') ? ($_, @{ $_->find(sub { 1 }) || [] }) : $_ }
              @parts;
  return [ map {
    my $el = $_;
    [ $el,
      $el->isa('PPI::Node') ? undef : $el->content,
      { map { exists $el->{$_} ? ($_ => $el->{$_}) : () } @PPI_ADHOC_KEYS } ]
  } @elems ];
}

sub _ppi_state_restore {
  my ($snap) = @_;
  for my $s (@$snap) {
    my ($el, $content, $keys) = @$s;
    $el->set_content($content) if defined $content;
    for my $k (@PPI_ADHOC_KEYS) {
      if (exists $keys->{$k}) { $el->{$k} = $keys->{$k} }
      else                    { delete $el->{$k} }
    }
  }
}

sub parse {
  my $self = shift;
  # PCL_OPT names are validated HERE, once every module is loaded (a Kind-B
  # pass may register from any of them) — a typo dies with the known list
  # before any parse, instead of surfacing from inside an analysis walk.
  Pl::Passes::check_env();
  # (The PCL_V1_FILES bisect hook lived here — it forced named files through
  # the whole-file v1 fallback to isolate a diverging module, task #80.  Both
  # it and the fallback were removed at E4.1 step 2, #242: with one pipeline
  # there is nothing to bisect against.)
  my $src = Pl::Parser::_preprocess_source(Pl::Parser::_maybe_decode_utf8($self->_source));
  # Route through v1's _ppi_parse so the shared PPI-bug workarounds apply — most
  # importantly _fix_modulo_magic (`7%-3` mis-tokenized as the magic hash %-,
  # dropping the modulo → PARSE ERROR).  A bare PPI::Document->new skipped it.
  # NAME THE FILE (§5a.4, E4.1 step 2).  Before the flip a PPI failure landed
  # in v1, where `--lenient-ppi` truncated the source at the unparseable line
  # and the run continued on a silently-shortened program.  There is no
  # lenient mode on this pipeline, so the failure has to say WHICH file — an
  # unattributed "PPI parse failed" in a sweep of 100 files is not a report.
  my $doc = $self->fallback_parser->_ppi_parse($src, $self->_eval_feature_seed)
    or die "PCL: cannot parse " . ($self->has_filename ? $self->filename : "(inline code)")
           . ": PPI failed to tokenize it\n";

  # B-regime flag (docs/raw-numeric-verdict.md §flag): a `use overload` in
  # THIS file means blessed values with per-use conversion handlers can flow
  # into scalars, so the scan-licensed freeze verdicts (raw-numeric/-string)
  # are disabled file-wide.  Overloaded objects arriving from OTHER files
  # (modules, string eval) are caught at runtime by the strict write
  # coercers — the designed backstop, loud die instead of silent corruption.
  $self->{_overload_in_file} = 1 if $src =~ /\buse\s+overload\b/;

  # Declaration decorations — the typed-lexical class word (`my Dog $spot;`)
  # and the attribute list (`my $x : shared = 1;`).  Both sit between the
  # declarator and the rest of the statement, neither survives into the
  # generated code, and both break every downstream decl-shape matcher if
  # left in place.  Strip them here, before any other pass sees the document.
  $self->_strip_decl_decorations($doc);

  # Statement-level `tie my $y, ARGS;` embeds its declaration inside a plain
  # statement, where the lexical facts scan (and so capture promotion) cannot
  # see it — a nested sub capturing $y then gates the file.  Normalize to the
  # equivalent `my $y; tie $y, ARGS;` so $y is an ordinary block-level decl
  # (state.t countfetches).  Statement position only: the two forms are
  # identical there (same decl scope, tie applied to the same variable).
  $doc = $self->_normalize_tie_my($doc);

  # PPI LEXER BUG: an anon sub with an ATTRIBUTE at the START of an expression
  # is tokenized as a LABEL — see _normalize_anon_sub_attrs.
  $doc = $self->_normalize_anon_sub_attrs($doc);

  # PPI LEXER BUG: a `for` whose loop variable is a \-cast (refaliasing) or a
  # non-scalar swallows the rest of the file — see _repair_alias_foreach.
  # Its n-at-a-time sibling (`for my ($q,$r) (LIST)`, perl 5.36) mis-lexes the
  # same way and is repaired the same way, and it runs FIRST because its
  # rewrite EMITS a `\my $q = …` the alias repair must not see as a loop head.
  $doc = $self->_repair_nary_foreach($doc);
  $doc = $self->_repair_alias_foreach($doc);

  # PPI LEXER BUGS in the OPERATOR-vs-TERM decision, each of which eats a whole
  # statement: `)*name` lexed as a glob (#354), `/PATTERN/` after a paren-less
  # call lexed as division (#351), and a call to a sub named `x` after a list
  # operator lexed as the repetition operator (#361).  All three are repaired
  # on the raw token stream with perl's own rule — the word before must not be
  # a TERM — see _repair_glob_multiply, _repair_word_match, _repair_word_x_call.
  $doc = $self->_repair_glob_multiply($doc);
  $doc = $self->_repair_word_match($doc);
  $doc = $self->_repair_word_x_call($doc);
  $doc = $self->_repair_term_initial_complement($doc);

  # PPI LEXER BUG: a `finally { … }` block is not part of the try Compound PPI
  # built, and the orphan statement it starts swallows the rest of the block —
  # see _repair_try_finally.
  $doc = $self->_repair_try_finally($doc);

  # `state` outside the classic subset (scalar statement-decl in a named sub)
  # is rewritten at SOURCE level into plain Perl the existing machinery
  # already lowers, then the document is reparsed — see _rewrite_state_prepass.
  $doc = $self->_rewrite_state_prepass($doc);

  # PPI splits an oddly-spelled qualified sub name into ADJACENT Word tokens
  # (`sub main::::flomp` → 'main::' + '::flomp'), and Statement::Sub->name
  # returns only the FIRST — every downstream consumer (prototype registry,
  # sub_info, p-declare-sub, p-sub emission) would see the truncated name and
  # emit an unreadable symbol (`pl-main::` — a read error that aborts the
  # section; method.t stopped at test 122 on this).  Merge each run into ONE
  # Word token here; v1's own name-concatenation loop sees the single token
  # and behaves identically (v1 handles the split by concatenating).
  for my $sub (@{ $doc->find('PPI::Statement::Sub') || [] }) {
    next if $sub->isa('PPI::Statement::Scheduled');
    my @words;
    for my $child ($sub->children) {
      last if $child->isa('PPI::Structure::Block')
           || $child->isa('PPI::Token::Prototype')
           || $child->isa('PPI::Token::Attribute');
      push @words, $child
        if $child->isa('PPI::Token::Word')
        && $child->content !~ /^(?:sub|my|our|state)$/;
    }
    next unless @words > 1;
    $words[0]->set_content(join '', map { $_->content } @words);
    $_->delete for @words[1 .. $#words];
  }

  # `my sub NAME {…}` / `state sub NAME {…}` are LEXICALS, but every named sub
  # compiles to a PACKAGE sub — so two same-named lexical subs in different
  # scopes clobbered each other and every reference resolved to the LAST one,
  # silently (#337).  Give each declaration a scope-unique name and rewrite
  # the uses its region owns, the way the _rename_* family does for `my $x`.
  # Runs BEFORE every name-keyed pass below (prototype registry, term scan,
  # sub_info, hoisting), so all of them see one consistent set of names.
  $self->_rename_lexical_subs($doc);

  # #364: which features each string-eval site inherits.  Computed here, once,
  # while the document is whole; _lower_stmt publishes the answer for the
  # statement it is lowering and ExprToCL puts it in the (p-eval …) call.
  $self->{_eval_features_by_stmt} = _scan_eval_site_features($doc);

  # Cross-file prototypes must be in the shared Environment BEFORE any
  # expression parses: v2 lowers named subs ahead of the statement stream and
  # pre-parses the stream through VarAnnotator, both of which run before the
  # use/require statement FALLBACKS through which v1 learns prototypes.  A
  # `sub is ($$@)` from a required test.pl otherwise never imposes scalar
  # context inside v2-lowered code (`is($be, reverse($le))` list-reversed —
  # pack.t s289).  Mirror v1's two extraction sites up front; extraction is
  # memoized and add_prototype idempotent, so the later statement-fallback
  # re-merge is harmless.
  $self->_premerge_include_prototypes($doc);

  # `use strict` must be visible BEFORE the ahead-of-stream parses for the
  # same reason as the prototypes above: PExpr's bareword disambiguation is
  # gated on the strict_subs pragma (an unknown bareword after a binary
  # operator is a FUNCALL under strict, a string without it — PExpr.pm
  # ~3590), and v1 learns the pragma from the `use strict` statement before
  # it reaches any sub body.  v2's named-sub lowering and VarAnnotator
  # pre-parse run first — without this, Moo::_Utils's
  # `$module =~ _module_name_rx` (a glob-installed constant sub) parsed as
  # the literal string "_module_name_rx" and _load_module croaked on every
  # class name (task #80).  The annotator parse also STAMPS
  # _bareword_string on the shared PPI token, so the mis-decision would
  # survive into the real parse even after the statement fallback sets the
  # pragma (the s276 stale-stamp family).
  $self->_premerge_strict_pragma($doc);

  # The constant-sub idiom `*NAME = sub () { ... }` (BEGIN-glob-installed,
  # Moo::_Utils's _module_name_rx/_CAN_SUBNAME family) gives NAME an empty
  # prototype perl KNOWS at parse time via the live stash.  Register it up
  # front so PExpr treats the bareword as a zero-arg call everywhere —
  # without it `_cnum + 1` swallowed the operand (`_cnum(+1)`) and
  # `X =~ _cnst ? ...` strung the bareword (E4.0 fuzzer, axis 19/22).
  $self->_premerge_glob_const_prototypes($doc);

  # `goto LABEL` cannot leave the enclosing subroutine in Perl (and a sort
  # comparator counts: "Can't goto out of a pseudo block") — when no such
  # label exists inside that barrier, the goto is a GUARANTEED runtime error
  # ("Can't find label LABEL").  Rewrite it to the equivalent `die` up front,
  # so every lowering path (named-sub body, anon sub, sort block, seam)
  # consumes plain Perl and the standalone-label pass never mistakes it for
  # a genuine forward goto (sort.t lines 809/813; v1 emits a naked (go :tag)
  # here that only survives because the calls sit under eval).
  $self->_rewrite_unreachable_gotos($doc);

  # #63/t183: `our @a` declared inside a block AFTER an in-block `package`
  # statement stays aliased to the DECLARING package's variable until the
  # block ends — including across a later `package` switch in the same
  # block.  The emission resolves bare names by the current section package,
  # so uses in the switched region would silently read the WRONG package's
  # variable.  Requalify them to the declaring package's spelling up front.
  $self->_requalify_block_our_after_pkg_switch($doc);

  # (#239) …and the same block's UNDECLARED globals: a `package X;` inside a
  # block re-homes every bare name for the rest of that block, which a
  # nested `(in-package :X)` cannot express because the CL reader has
  # already interned those symbols.  Requalify them too — AFTER the `our`
  # pass, whose output this one reads as "already qualified".
  $self->_requalify_block_globals_after_pkg_switch($doc);

  # (s300d) The s295c "bare fork pipe-open with die-on-failure" gate is gone:
  # %p-open-fork-pipe now implements the 2-arg `open FH, "|-"` / "-|" fork
  # forms (plus command pipes and ">&" dup-opens) in the runtime, so the
  # die-on-failure idiom no longer fires and the load-abort blast radius the
  # gate protected against cannot be reached.

  # String eval (`eval EXPR`) captures enclosing my-lexicals (session-250
  # mechanism).  W3: it lowers through the ordinary expression fallback seam —
  # v1's gen_funcall emits (p-eval STR (list (cons "$x" $x) …)) reading the
  # SCOPED _let_bound_vars, so the capture alist reflects the call site's live
  # scope.  The VarAnnotator's region-wide $has_eval keeps every captured var
  # boxed (do not narrow it).  No gate here anymore.
  # (`CORE::my`/`CORE::our`/`CORE::state`/`CORE::local` are normalised to the
  # bare declarator in Pl::Parser::_preprocess_source, before PPI parses — a
  # token rename here is too late, PPI already mis-structures the for-head.)
  # (state decls outside the classic named-sub statement subset were rewritten
  # away by _rewrite_state_prepass above; _rename_state_vars owns the rest.)
  # (Bare `$#` magic + subscript — `$#[0]` on the oddly-named `@#` array —
  # lowers as element access; `@#` is forward-declared by the %punct bucket
  # in _forward_global_decls.  E4.1 M3, s353: the v1 gate that used to sit
  # here is gone.  `$#array` is a distinct ArrayIndex token, never affected.)
  # E3 eval-mode: the segment package is the CALL SITE's package, so
  # __PACKAGE__ (and bare sub/global resolution) inside the eval string
  # matches the caller.
  my $root_pkg = $self->eval_mode ? ($self->eval_pkg // 'main') : 'main';
  $self->environment->package_stack([$root_pkg]);
  $self->environment->state_var_renames({});
  $self->{_referenced_pkgs} = {};
  # The forward-declaration exclusion set is PER SECTION (_seg_lex, reset in
  # the segment loop, accumulated by _reg_lex, never shrinks within its
  # section): a section must not defvar a name it let-binds itself (a defvar
  # proclaims the symbol special and poisons the section's own `let`s), but a
  # name let-bound only in some OTHER section must still get this section's
  # defvar when used as a package global here (sort.t @a — see the segment
  # loop comment).  Formerly a file-wide pkg-keyed accumulator (_all_lex);
  # the per-package keying (join.t: a main `my $t` must not suppress an
  # `X::$t` defvar) is subsumed — a section has exactly one package.  This is
  # deliberately separate from the now-SCOPED _let_bound_vars, which must
  # reflect the call site's live scope so the string-eval capture alist
  # (_eval_lexical_alist) doesn't list a closed sibling scope's name.
  $self->{_if_ret_counter} = 0;   # unique --pcl-if-ret--N per tail bare-if

  # ---- Split the top level into PACKAGE SEGMENTS at statement-form
  # `package Foo;`.  v2 mirrors v1's section model: each segment becomes its
  # own output section whose (in-package …) preamble puts the READER in the
  # right CL package — an in-package nested inside a form is a no-op for the
  # rest of that already-read form, so the switch must happen between
  # top-level forms.  A `package` nested deeper than the positions handled
  # here dies in _lower_stmt (whole-file v1 fallback).
  my @segments = ({ pkg => $root_pkg, stmts => [], reopen => 0 });
  my $cur_pkg = 'main';
  my %opened  = (main => 1);   # packages whose full preamble was already emitted
  # T-A1 block extents: segments born from a flattened bare block carry
  # `blk => ID`.  A my-lexical declared in a blk-tagged segment is live only
  # while later segments carry the SAME blk (its Perl scope ends at the
  # block) — the spanning check/rename passes use this to (a) not flag names
  # that merely recur after the block, (b) stop the qualified rewrite at the
  # block end.  Flattened-block segment runs are contiguous by construction.
  my $cur_blk;
  my $blk_counter = 0;
  # One consumer for `package` statements, shared by the top level and the
  # T-A1 bare-block flattening below (both forms, identical semantics).
  my $consume_pkg = sub {
    my ($child) = @_;
    return 0 unless $child->isa('PPI::Statement::Package');
    my ($block) = grep { $_->isa('PPI::Structure::Block') } $child->schildren;
    my $pkg = $child->namespace // 'main';
    my $version = eval { $child->version };
    # PPI quirk (see v1 _emit_package_version): ->version returns the BLOCK
    # text for an unversioned block form — accept only real version literals.
    undef $version unless defined $version && $version =~ /^v?\d+(?:[._]\d+)*$/;
    if ($block) {
      # Block form `package Foo { … }`: a section for Foo, then a short-form
      # RETURN section that puts the reader back in the enclosing package
      # (which already has its own section — hence reopen).
      push @segments, { pkg => $pkg, stmts => [$block->schildren],
                        reopen => ($opened{$pkg}++ ? 1 : 0), version => $version,
                        blockform => 1, blk => $cur_blk };
      push @segments, { pkg => $cur_pkg, stmts => [], reopen => 1, blk => $cur_blk };
      return 1;                             # $cur_pkg unchanged
    }
    push @segments, { pkg => $pkg, stmts => [],
                      reopen => ($opened{$pkg}++ ? 1 : 0), version => $version,
                      blk => $cur_blk, pkg_stmt => $child };
    $cur_pkg = $pkg;
    return 1;
  };
  my %pre_my;   # file-lexical bare names declared by top-level my/state so far
  for my $child ($doc->schildren) {
    next if $consume_pkg->($child);
    if ($child->isa('PPI::Statement::Variable')) {
      my $kw = ($child->schildren)[0];
      if ($kw && $kw->isa('PPI::Token::Word') && $kw->content =~ /^(?:my|state)$/) {
        for ($self->_declared_names($child)) {
          (my $b = $_) =~ s/^[\$\@\%]//;
          $pre_my{$b} = 1 if /^[\$\@\%]\w+$/;
        }
      }
    }
    # T-A1: a top-level bare block with direct-child `package` statements —
    # the helper-class idiom `{ package Foo; sub new {…} … }`.  Flatten its
    # children into the segment stream so the normal machinery applies; then
    # restore the enclosing package (statement-form `package` inside a block
    # is block-scoped).  Lexical safety: flattening runs BEFORE the rename
    # passes, so a `my` spanning the intra-block segments is renamed (W10
    # subset) or dies → v1, exactly like a file lexical — and the blk tags
    # bound both the check and the rewrite to the block's extent.
    #
    # DEFAULT ON since s278b (was gated behind PCL_V2_PKGBLOCK until the
    # join.t miscompile — an X::$t forward-defvar wrongly suppressed by a
    # main-package `let` of the same bare name — was fixed via the
    # package-aware (now per-section _seg_lex) exclusion).  Full-sweep parity vs v1 on all 18
    # package-in-block files, and corpus-wide (identical fully-passing set).
    my $inner = $self->_flattenable_pkg_block($child);
    # Flatten refusal (s296): a my/state decl at the BLOCK's own statement
    # level (incl. embedded ones like `tie my $y, …`) that shadows a file
    # lexical declared BEFORE the block would become an unscopeable
    # segment-top-level re-decl under flattening — the span engine's hard-decl
    # rule can neither rename nor skip it (state.t: file `my (…, $y, …)` +
    # `tie my $y` inside the countfetches block).  Lower the block IN PLACE
    # instead (D1-lite nested-package path), which keeps every lexical's
    # scope intact by construction.  Files that flatten today cannot contain
    # this shape (they would be gated), so their output is unchanged.
    undef $inner
      if $inner && $self->_pkgblock_shadows_file_lexical($child, \%pre_my);
    if ($inner) {
      my $outer_pkg = $cur_pkg;
      $cur_blk = ++$blk_counter;
      # The block's leading statements (before its first `package`) get their
      # own blk-tagged segment so their `my`s die at the block end too.
      push @segments, { pkg => $outer_pkg, stmts => [], reopen => 1, blk => $cur_blk };
      for my $c (@$inner) {
        next if $consume_pkg->($c);
        push @{ $segments[-1]{stmts} }, $c;
      }
      $cur_blk = undef;
      # Restore segment: plain enclosing scope (no blk).
      push @segments, { pkg => $outer_pkg, stmts => [], reopen => 1 };
      $cur_pkg = $outer_pkg;
      next;
    }
    push @{ $segments[-1]{stmts} }, $child;
  }
  $self->{_file_lex_counter} = 0;
  $self->{_file_lex_renamed} = {};
  # M-F: renamed-cell declarations emit a p-alias-eval-cell only when the
  # file contains a string eval somewhere — the alias can only be observed
  # through an eval in THIS file (lexicals never cross files), and skipping
  # the inert call keeps eval-free files byte-identical.  Non-eval uses of
  # the word (`->eval`, `eval =>`, hash keys) over-fire harmlessly.
  # _str_eval_in_named_sub is the same scan's second question (#296-B1): a
  # string eval inside a NAMED sub can only reach a file lexical through the
  # promotion-to-cell path, because the sub is hoisted out of the file-level
  # `let` that the eval-site capture alist is built from.
  $self->{_file_has_str_eval} = 0;
  $self->{_str_eval_in_named_sub} = 0;
  for my $w (@{ $doc->find(sub { $_[1]->isa('PPI::Token::Word')
                                 && $_[1]->content eq 'eval' }) || [] }) {
    my $nx = $w->snext_sibling;
    next if $nx && $nx->isa('PPI::Structure::Block');
    $self->{_file_has_str_eval} = 1;
    $self->{_str_eval_in_named_sub} = 1 if _inside_named_sub($w);
    last if $self->{_str_eval_in_named_sub};
  }

  # W10: a file lexical declared in one segment and used in a later one spans
  # a package boundary.  v1 has an OPEN BUG here (it defvars the name under
  # the declaring package; the later segment reads Pkg::$name → unbound), so
  # gating to v1 crashes at runtime.  When the W5 subset holds, rewrite the
  # lexical to a fresh package-level cell instead: unqualified $x__file__N in
  # the declaring segment (defvar'd box via _file_lex_renamed), and the
  # package-qualified $Pkg::x__file__N in later segments.  Must run BEFORE
  # _check_my_spanning (renamed names no longer span) and before the W5 pass
  # (which skips already-renamed names).
  # E3: an eval string that switches package at top level would need the full
  # multi-section assembly (head/body split across in-package switches) — the
  # per-eval v1 retry owns that shape until it proves common.
  #
  # s342g (task #226) narrowed WHY, so the next attempt starts from facts:
  # 23 of the family's 24 measured events are ONE LEADING `package X;` and no
  # further switch, i.e. an EMPTY first segment plus X's — not a real
  # multi-section assembly.  Dropping the empty head makes it a single segment
  # and the assembler accepts it, but the result is SILENTLY WRONG: the body's
  # `sub f` still emits as `pl-f` (read in :pcl) while the caller looks up
  # `X::pl-f`.  The missing piece is making the section's symbols resolve in X.
  # `(in-package |X|)` only works when the body is at top level — with free
  # variables it sits inside the thunk's lambda, where a reader-level switch
  # cannot reach.  The reuse path is the OTHER mechanism, already built for
  # nested packages by E1.5/D1-lite: emit the symbols QUALIFIED (see
  # _lower_our_decl's "Inside a nested-`package X;` region … qualify it").
  #
  # s346 (#226, RULED s345 §2): the fix is to STOP CONSUMING the leading
  # `package X;` at segment level and hand it to the D1-lite nested-package
  # path in _lower_block instead.  That path pushes X onto the Environment for
  # the remainder of the block while the SECTION package (cur_pkg) stays the
  # eval's root — which is exactly the `current ne cur_pkg` condition the
  # QUALIFIED emission keys on (_sub_name_for_emission, _lower_our_decl's
  # qualify branch), so every symbol the region defines is spelled `X::…` and
  # resolves where the caller looks.  No new mechanism, and file mode is
  # untouched (there a top-level `package X;` still opens its own section,
  # whose in-package does the same job at read time).
  # The segment-level sub extraction must be skipped for such a segment
  # (eval_pkg_region): it runs BEFORE lowering, so it would name the subs
  # unqualified — leaving them in the statement stream lets _lower_block's
  # nested-sub hoist name them through _sub_name_for_emission.
  #
  # NOTHING IS REFUSED HERE ANY MORE (#240 step 2, RULED s349 §2c).  Two
  # shapes used to keep the v1 retry, and both were the SAME bug seen from
  # two sides: an unqualified name inside the region resolved in the CALLER's
  # package.  `_lower_our_decl`'s qualify branch writes `F1::$Z` while the
  # read emits a bare `$Z` that eval mode's free-var scan bound to the
  # caller's container (`eval 'package F1; our $Z = 5; $Z * 2'` → 0, perl
  # 10); and a symbolic deref (`${$n}`) interned its runtime name in
  # `*package*`, the caller again.  p-eval-thunk now binds `*package*` to the
  # region package around the free-name resolution and the body, so both
  # spellings — and the wider hole the s348 gate could not cover at all
  # (`eval 'package F2; $Zz = 5; 1'` setting `$main::Zz`) — resolve in X.
  # E4.1 M1 (s353): the collapse also accepts LEADING statements that pass
  # the `_eval_safe_leading_stmts` whitelist.  Verified live: Moo's
  # Sub::Quote population lowers natively (moo-01.t 15/15, gate audit
  # events 15 → 0), and the faithful write-through probe (capture value =
  # \$lexical holding a ref, `$$q = …` inside the region) round-trips.
  # A LITERAL `\\$x` ref-of-ref capture still mis-derefs — but identically
  # under the v1 fallback (pre-existing, the #163 referent-kind-tag
  # residue; probed at HEAD in file mode too), so this arm changes nothing
  # for that shape.
  if ($self->eval_mode && @segments == 2
      && $self->_eval_safe_leading_stmts($segments[0]{stmts})
      && $segments[1]{pkg_stmt} && !$segments[1]{blockform}
      && !defined $segments[1]{version}) {
    $self->{_eval_pkg_stmt} = $segments[1]{pkg_stmt};
    @segments = ({ pkg => $root_pkg, reopen => 0, eval_pkg_region => 1,
                   stmts => [ @{ $segments[0]{stmts} },
                              $segments[1]{pkg_stmt}, @{ $segments[1]{stmts} } ] });
  }
  # E4.1 M1 (s353): the same collapse for the FLATTENED bare-block spelling.
  # Sub::Quote wraps everything in one `{ … }` with a trailing `1;`, which
  # the T-A1 flattening renders as [empty main | blk leading-`my`s | pkg X
  # (blk) | blk restore + the trailing literal].  Accept when the leading
  # statements pass the M1 whitelist and the trailing statements are
  # literal-only (their package/scope is unobservable), and collapse to the
  # one region segment the #226 machinery already lowers.  The block's
  # lexical scoping is erased, which the two whitelists make unobservable.
  if ($self->eval_mode && @segments == 4
      && !@{ $segments[0]{stmts} } && !$segments[0]{pkg_stmt}
      && defined $segments[1]{blk} && !$segments[1]{pkg_stmt}
      && $self->_eval_safe_leading_stmts($segments[1]{stmts})
      && $segments[2]{pkg_stmt} && !$segments[2]{blockform}
      && !defined $segments[2]{version}
      && !$segments[3]{pkg_stmt}
      && _eval_literal_only_stmts($segments[3]{stmts})) {
    $self->{_eval_pkg_stmt} = $segments[2]{pkg_stmt};
    @segments = ({ pkg => $root_pkg, reopen => 0, eval_pkg_region => 1,
                   stmts => [ @{ $segments[1]{stmts} },
                              $segments[2]{pkg_stmt}, @{ $segments[2]{stmts} },
                              @{ $segments[3]{stmts} } ] });
  }
  # RULED REFUSAL (fable-answers-s345.md §2, amended s353; rephrased in the
  # E4.1 step-2 commit, #242).  Two residual multi-segment shapes: a true
  # multi-switch (`package A; …; package B; …`) and leading statements the M1
  # whitelist declines.  §5a.3 requires the text to reach `$@` perl-shaped —
  # this is an ordinary Perl-level error the program can trap, not a compiler
  # note.  docs/not-supported.md: 'String eval with multiple package sections'.
  die "PCL: unsupported in string eval: multiple package sections\n"
    if $self->eval_mode && @segments > 1;
  # E3: `eval '...; my $x = EXPR'` — the eval's VALUE is the trailing
  # declaration's statement value.  Eval mode lowers the top level with
  # tail_ctx='inherit' (s308b), so the $decl_tail machinery covers the same
  # shapes it covers in sub bodies.
  #
  # RULED REFUSAL, rephrased at the flip (#242).  What still lands here is
  # a declaration shape whose VALUE the lowering cannot produce.  Measured
  # over the whole sweep (s354): all five events are perl-INVALID input —
  # `my $$x`, `my $$$x`, `my @$x`, `my($a,$b),$x,my($c,$d)` from lex.t/my.t
  # rows that assert perl REJECTS them (CLAUDE.md principle 9).  Refusing
  # makes `$@` non-empty, which is what those rows want; DECIDED.md records
  # the expectation that this GAINS four eval.t rows rather than losing any.
  # docs/not-supported.md: 'String eval ending in an unconvertible declaration'.
  if ($self->eval_mode) {
    my ($last) = grep { $_->significant && !$_->isa('PPI::Statement::Null') }
                 reverse @{ $segments[0]{stmts} };
    die "PCL: unsupported in string eval: trailing declaration has no value\n"
      if $last && $last->isa('PPI::Statement::Variable')
      && !$self->_tail_decl_convertible($last);
    # (E4.1 pre-work, s352: the bareword-ARRAY-subscript refusal that used to
    # sit here is GONE.  It claimed "v2's native lowering strings it", but
    # PExpr's `_bareword_subscript_autoquotes` already keeps an ALL-CAPS
    # bareword callable under eval_mode — the carve-out predates this gate —
    # and Parser2 answers `eval_mode` for both the native and the seam route.
    # Measured: with the gate removed the JSON::PP ->canonical shape lowers
    # natively and agrees with perl, and the one shape it does NOT cover, a
    # LOWERCASE sub name, is wrong IDENTICALLY under v1 — so the fallback was
    # buying nothing.  That divergence is task #246, not a v1 dependency.)
  }
  $self->_rename_spanning_lexicals(\@segments) if @segments > 1;
  $self->_check_my_spanning(\@segments) if @segments > 1;

  # state in a NAMED sub: rename to a per-sub package cell `$x__state__N`
  # (+ once-flag) — see _rename_state_vars.  Runs FIRST among the segment-
  # local rename passes so the later passes' facts scans (decl_count in
  # _scan_lex_facts counts my AND state) see state decls already off the
  # bare name.  Anything outside the subset dies → v1; the pre-pass is
  # authoritative — no un-renamed declarator-shaped `state` survives into
  # lowering.
  $self->_rename_state_vars($_) for @segments;

  # #296: a `my`/`state` of an EXCEPTION-partition name ($a/$b/@ARGV/…) must
  # not be let-bound under its own symbol — that symbol is a proclaimed
  # special, so the `let` is a DYNAMIC rebind.  Runs HERE, before the capture
  # promotion below and before the three poisoned-my passes, so every one of
  # them sees the final name: a file-level `my $a` captured by a named sub has
  # to reach _rename_captured_file_lexicals already renamed, or that pass
  # promotes `$a` and this one then renames the promoted decl out from under
  # it (probed: "Parser2 TODO: file lexical 'a__excl__0' captured by sub foo").
  $self->{_excl_rename_counter} = 0;
  $self->_rename_exception_mys($_) for @segments;

  # #296-B1: the eval-mode half — a FREE exception name this eval captures
  # from the caller (`eval q{"[$a]"}` inside a `my $a` scope) compiles as that
  # captured lexical.  Runs right after the decl pass, so the only `$a` tokens
  # left are genuinely free ones, and before every consumer of the names.
  $self->{_evalcap_counter} = 0;
  $self->_rename_free_eval_captures($doc) if $self->eval_mode;

  # W5: rewrite file lexicals captured by named subs to fresh package-level
  # cells (see _rename_captured_file_lexicals).  Runs BEFORE the pre-pass so
  # every downstream reader (sub_info, _sub_ctx_insensitive, VarAnnotator,
  # _lower_block) sees the renamed tokens; _file_lex_renamed drives the
  # defvar-not-let lowering and un-fires the capture gates.
  # File-wide my/state declaration counts (post-span tree): a bare name with
  # exactly ONE declaration in the whole file is promoted under its OWN name
  # (identity — no __file__N mangle), which keeps string eval, interp and
  # ${x} text resolving; see _promote_captured's identity branch.
  {
    my %fdc;
    for my $seg (@segments) {
      my $ff = {};
      $self->_scan_lex_facts($seg->{stmts}, $ff);
      $fdc{$_} += $ff->{decl_count}{$_} for keys %{ $ff->{decl_count} };
    }
    $self->{_file_decl_count} = \%fdc;
  }
  $self->_rename_captured_file_lexicals($_) for @segments;

  # (#291: the two W8.5 passes that renamed a poisoned `my` — the condition-my
  # `$x__cond__N` and the nested-bare-block `$x__shadow__N` — are GONE.  Both
  # existed because a `defvar` of the package global would have poisoned the
  # construct's own `let`, and because the exclusion that avoided THAT left the
  # global undeclared; since the flip a `p-defcell` symbol macro and a `let` of
  # the same name simply coexist.  `_shadow_rename_counter` still numbers the
  # v1-SEAM shadow rename, _gate_seam_my_shadow, whose cause is the seam's
  # _let_bound_vars contract and not the declaration model.)
  $self->{_shadow_rename_counter} = 0;

  # Pre-pass: collect per-sub facts BEFORE lowering anything, so call sites
  # that precede (or recurse into) a sub's definition see them.  Register each
  # sub in the SHARED Environment too — the fallback expression machinery
  # decides bareword-vs-string ("foo" vs (pl-foo)) from declared_subs, and
  # Parser2 never runs v1's _process_sub_statement which normally does this.
  # Keyed by the segment's package: `sub hi` after `package Foo;` is Foo::hi.
  for my $seg (@segments) {
    $self->_set_cur_package($seg->{pkg});
    for my $child (@{ $seg->{stmts} }) {
      # Named subs anywhere in the statement — top-level or nested inside a
      # block (Perl subs are package-global regardless of nesting; the nested
      # ones hoist via _hoist_nested_sub during lowering).
      # NB: PPI::Statement::Scheduled (BEGIN/END/…) ISA Statement::Sub — those
      # are runnable blocks, not sub definitions; they lower via _fallback_stmt.
      # NB2: PPI find returns 0 (not undef) when nothing matches → `|| []`.
      # $child itself (when a named sub) AND any subs nested INSIDE it: a named
      # sub nested in another named sub (`sub run_tests { sub bar {…} }`,
      # substr.t) is still package-global, so its bareword call sites must
      # resolve to (pl-bar), not the string "bar".  `find` returns descendants
      # only, so a top-level `sub` $child needs `($child, @{find})`.
      my @subs = ($child->isa('PPI::Statement::Sub') ? ($child) : (),
                  @{ $child->find('PPI::Statement::Sub') || [] });
      for my $sub (@subs) {
        next unless $sub->name && !$sub->isa('PPI::Statement::Scheduled');
        # A prototype/signature changes how CALL SITES parse (arity, imposed
        # context like `($)` → scalar, block-form `(&@)`).  Register it so the
        # fallback PExpr parses call sites correctly; the DEFINITION is lowered
        # by v1 via _fallback_stmt (signature binding + arity checks).  NO
        # sub_info: the `insensitive-call` rule (once ExprToCL2's direct-call
        # path) ignores imposed context, so call sites to a prototyped sub
        # must keep the general funcall path.
        if (defined(my $proto = $self->_proto_or_sig_str($sub))) {
          # (s300c) The former whole-file gate for a NAMED sub nested inside a
          # prototyped/signatured sub is gone: the seam lowering now hoists the
          # nested sub correctly — top-level prototyped subs lower with the
          # per-segment fresh _let_bound_vars, and the block-nested route
          # clears leaked let-bound names it never references (see the
          # Statement::Sub branch in _lower_block), so v1's nested-named hoist
          # (`sub t152x` callable before `t152` runs) engages as in whole-file
          # v1.  signatures.t: 796+182, fail rows identical to v1.
          # FACTS ONLY.  v1's signature parser is a statement-level helper
          # that also EMITS (an `our`/`state` inside a default declares its
          # cell — _parse_signature); here only its prototype record is
          # wanted, and the sub statement's own lowering (v1-routed: sub-with-
          # signature is one of the 12 classes) emits those declarations in
          # their place.  So the call runs inside capture_v1 and its drain is
          # deliberately DISCARDED — the one such discard in Parser2; it goes
          # when the class is ported (E5.3).
          my $fp = $self->fallback_parser;
          my $sig_info = $fp->capture_v1(
            sub { $fp->parse_prototype_or_signature($proto, $sub) })->{result};
          $self->environment->add_prototype($sub->name, $sig_info);
          $self->environment->add_declared_sub($sub->name, $self->_effective_pkg($sub, $seg->{pkg}),
                                             Pl::PExpr::TokenUtils::decl_site($sub));
          next;
        }
        $self->environment->add_declared_sub($sub->name, $self->_effective_pkg($sub, $seg->{pkg}),
                                             Pl::PExpr::TokenUtils::decl_site($sub));
        # Same default signature v1's _process_sub_statement registers for a
        # prototype-less sub: PExpr consults get_prototype() to decide that a
        # bareword `foo` is a CALL (pl-foo), not the string "foo".  A
        # :prototype-attribute proto (from_attr) is compile-time in perl —
        # never clobber it with the default.
        my $prev_proto = $self->environment->get_prototype($sub->name);
        if (!($prev_proto && $prev_proto->{from_attr})) {
          $self->environment->add_prototype($sub->name,
                                            { params => [], min_params => -1, is_proto => 0 });
        }
        # Forward declaration `sub foo;` (no block) reserves the name only — v1
        # emits (p-declare-sub) and no definition.  No sub_info: there is
        # nothing to direct-call, so any call takes the fallback funcall path.
        next unless $sub->block;
        # A sub living in a NESTED package (D1/E1.5) is emitted with a
        # qualified name (_sub_name_for_emission); the unqualified cl_name
        # convention below would direct-call the wrong symbol → no sub_info,
        # calls take the fallback funcall path.
        next if $self->_effective_pkg($sub, $seg->{pkg}) ne $seg->{pkg};
        # cl_name stays UNQUALIFIED (pl-foo) for a plain name: the section's
        # in-package makes the reader intern it in the segment's package —
        # exactly v1's per-section convention.
        $self->sub_info->{ $seg->{pkg} }{ $sub->name } = {
          cl_name     => $self->fallback_parser->_qualified_sub_to_cl($sub->name),
          insensitive => $self->_sub_ctx_insensitive($sub),
          # #189: this sub writes through @_ to its caller's variables, so
          # every argument handed to it must be a BOX (VarAnnotator turns the
          # fact into an `arg-to-writer` boxing event at the call sites).
          writes_args => $self->_sub_writes_args($sub),
        };
      }
    }
  }

  # ---- Lower each segment into a section record.
  my @sections;
  for my $seg (@segments) {
    $self->_check_sub_captures($seg->{stmts});
    $self->cur_pkg($seg->{pkg});
    $self->_set_cur_package($seg->{pkg});
    $self->{_captured_decls} = [];
    $self->{_sched_defs}     = [];   # BEGIN/END p-BEGIN blocks (after defs, before run)
    $self->{_sched_lines}    = [];   # source position per _sched_defs entry (#55 interleave)
    $self->{_hoisted_decls}  = [];
    $self->{_hoisted_defs}   = [];
    $self->{_hoisted_def_lines} = [];  # source position per _hoisted_defs entry
    $self->{_live_lex}       = {};
    $self->{_seg_lex}        = {};   # every name let-bound in THIS section (forward-decl exclusion)
    # #226: collector for the eval region's leading-package enter forms — see
    # the hoist in _lower_block's Statement::Package branch.
    $self->{_eval_pkg_enter} = $seg->{eval_pkg_region} ? [] : undef;
    $self->{_eval_pkg_enter_cl} = undef;
    # Named subs + Scheduled blocks of this segment — the embedded-my
    # let-hoist consults these (a sub referencing the name vetoes the hoist).
    $self->{_seg_named_subs} = _collect_named_subs($seg->{stmts});
    # `my` lexicals do not cross a segment (package) boundary in v2 — each
    # segment is its own top-level section, and a genuine cross-boundary my is
    # gated by _check_my_spanning.  So the fallback machinery's let-bound set
    # must start empty per segment; otherwise an earlier segment's file
    # lexical (e.g. `package Foo { my @a; … }`) leaks into a later segment's
    # string-eval capture alist as a free (→ unbound) symbol.  The forward-
    # decl exclusion (_seg_lex) is per-section too: a name let-bound ONLY in
    # another section must still get this section's defvar when used as a
    # package global here (sort.t: top-level `sort {…} @a` beside later
    # block-scoped `my @a`s — the suppressed defvar left @a unbound at load).
    # The defvar makes the other sections' uncaptured lets dynamic, which is
    # exactly v1's file-lexical model for the colliding name; captured
    # lexicals are renamed (promotion passes / the seam's __lex__N) or gated,
    # so the per-iteration-closure hazard cannot reach this path.
    $self->{_let_bound_vars} = {};
    # M-F: this segment's span-mangled cells, visible to string evals by
    # their ORIGINAL name via the capture alist (_eval_lexical_alist).
    # These per-site pairs cover CROSS-PACKAGE sites (the eval-time lookup
    # interns the name in the SITE's package, where a cell aliased in the
    # declaring package is invisible).  Same-package resolution — promoted
    # cells, and nested/late evals with no site alist at all — goes through
    # the alias rule instead (p-alias-eval-cell at the decl's run position;
    # ir-spec §9.1).
    $self->{_eval_span_captures} = $seg->{eval_span_captures} // {};
    my (@decls, @defs, @def_lines, @top);
    for my $child (@{ $seg->{stmts} }) {
      # eval_pkg_region (#226): this segment leads with a `package X;` that is
      # lowered IN the statement stream, so a sub after it belongs to X — but
      # this loop runs BEFORE lowering, where the Environment still says the
      # section package and the name would emit unqualified (the s342g
      # silent-wrong: `pl-f` read in :pcl while the caller looks up X::pl-f).
      # Leave them in @top; _lower_block's nested-sub hoist names them through
      # _sub_name_for_emission, after the D1-lite push.
      if (!$seg->{eval_pkg_region}
          && $child->isa('PPI::Statement::Sub') && $child->name
          && !$child->isa('PPI::Statement::Scheduled')) {
        # SIGNATURED sub: v1 owns the whole definition (param binding,
        # defaults, arity).  _fallback_stmt runs v1's _process_sub_statement,
        # which emits the p-declare-sub + p-sub into its declaration/
        # definition buckets (→ _captured_decls).  Any runtime raw it returns
        # (rare) goes to @defs.  A PURE prototype (`($;$)`) binds nothing —
        # its definition lowers natively below (task #126; the prototype
        # itself was already registered for call-site parsing in the
        # pre-pass, and call sites still take the fallback funcall path —
        # no sub_info).
        if (defined $self->_proto_or_sig_str($child)
            && !$self->_is_pure_prototype($child)) {
          my @raw = $self->_fallback_stmt($child);
          push @defs, @raw;
          push @def_lines, (_src_pos($child)) x @raw;
          next;
        }
        push @decls, ['p-declare-sub', $self->fallback_parser->_qualified_sub_to_cl($child->name)];
        # Forward declaration `sub foo;` reserves the name only (no definition).
        if ($child->block) {
          push @defs, $self->_lower_sub($child);
          push @def_lines, _src_pos($child);
        }
        next;
      }
      # `use strict`-family pragmas are pure no-ops; real use/require/BEGIN
      # statements stay IN the statement stream (position matters: a module
      # must load before the code that follows it runs) and lower through the
      # statement fallback in _lower_block.
      next if $child->isa('PPI::Statement::Null');
      push @top, $child;
    }
    # File top level has no consumer of the last statement's value — except
    # EVAL MODE, where the eval's value IS the tail statement's value:
    # 'inherit' turns on the tail machinery ($decl_tail for trailing
    # declarations, the ret-var transform for tail if-modifiers/compounds)
    # exactly as in a sub body (s308b, drops the E3 trailing-decl retry).
    my @runtime = $self->_lower_block(\@top,
        Pl::VarAnnotator->analyze(\@top, undef, $self->_cur_sub_info, $self),
        $self->eval_mode ? 'inherit' : undef);
    # Named subs found nested inside blocks during lowering (package-global
    # in Perl) hoist into the same decl/def buckets as top-level subs.
    push @decls, @{ $self->{_hoisted_decls} };
    push @defs,  @{ $self->{_hoisted_defs} };
    push @def_lines, @{ $self->{_hoisted_def_lines} };
    push @sections, {
      pkg      => $seg->{pkg},
      reopen   => $seg->{reopen},
      version  => $seg->{version},
      # Every top-level form passes through the Kind-B optimization registry
      # (Pl::Passes::run — the identity until a pass is registered) HERE, the
      # one place a lowered tree becomes text.  Captured/sched entries are v1
      # TEXT already and are not trees; they stay outside the registry.
      decls    => [map { Pl::CLForm::to_string(Pl::Passes::run($_), 0) } @decls],
      defs     => [map { Pl::CLForm::to_string(Pl::Passes::run($_), 0) } @defs],
      # A top-level `my` nests its whole block remainder in ONE `let` form; for
      # a large block (arith.t's ~180 `$T++` calls) that single form exhausts
      # SBCL's compiler heap when the R1 hot ops open-code inline.  v1 caps this
      # by wrapping any oversized top-level runtime form in a
      # `(locally (declare (notinline …)))` — reuse that mechanism verbatim.
      # Past ~3x that size even the notinline form OOMs the compiler's
      # register allocator outright (pack.t s289: one 162k-char form died at
      # the sweep's 1 GB heap; the corpus' largest WORKING form is ~55k).
      # _oversized_top_decls flattens the common cause pre-emptively; this
      # gate is the by-construction backstop — die → v1, never an OOM crash.
      # Both size deciders get the whitespace-COLLAPSED length: the structural
      # printer's depth indentation would otherwise inflate the measure past
      # thresholds calibrated on v1-flat text (compile cost tracks content,
      # not layout) — three files spuriously gated at the E2.final root flip.
      run      => [map {
        my $text = Pl::CLForm::to_string(Pl::Passes::run($_), 0);
        (my $collapsed = $text) =~ s/\s+/ /g;
        $self->_gate_oversized_run_form(
          Pl::Parser::_cap_inlining_if_huge($text, length $collapsed),
          length $collapsed);
      } @runtime],
      captured => [@{ $self->{_captured_decls} }],
      sched    => [@{ $self->{_sched_defs} }],
      # #226: eval region's leading-package enter forms — emitted at the head
      # of the eval BODY, ahead of the defs/sched interleave.
      pkg_enter => [map { Pl::CLForm::to_string(Pl::Passes::run($_), 0) }
                    @{ $self->{_eval_pkg_enter} // [] }],
      # #240 step 2: the region's CL package designator, recorded where the
      # enter forms were built (never derived a second time) — see
      # _assemble_eval_mode's p-eval-thunk argument.
      pkg_enter_cl => $self->{_eval_pkg_enter_cl},
      # Source positions parallel to defs/sched — the #55 interleave assembly
      # merges the two streams by these (perl compiles subs and runs BEGIN
      # blocks in source order; a BEGIN must not see a sub defined below it).
      def_lines   => [@def_lines],
      sched_lines => [@{ $self->{_sched_lines} }],
      seg_lex  => $self->{_seg_lex},
    };
  }

  # M-F backstop: every promoted cell whose eval refusals were waived must
  # have registered its capture pair at its defvar lowering.  A decl that
  # instead lowered inside a v1-seam expression never registered — the eval
  # would silently read the wrong variable → die → whole-file v1.
  if (my ($miss) = sort keys %{ $self->{_pending_eval_caps} // {} }) {
    die "Parser2 TODO: promoted cell $miss never registered for eval capture\n";
  }

  # Phase B3 backstop: v1 text is produced on the seam parser ONLY inside
  # capture_v1, whose drain hands it back by bucket — its standing scratch
  # section must be EMPTY here, or some emission was never drained (lost
  # output, rule 12).
  $self->fallback_parser->assert_seam_clean;

  # E3 eval-mode: single anonymous segment, head/body split, p-eval-thunk
  # wrap — no per-package section assembly.
  return $self->_assemble_eval_mode($sections[0], $doc) if $self->eval_mode;

  # ---- Assemble the sections.
  my @body;
  my $phase_boundary_emitted = 0;
  for my $i (0 .. $#sections) {
    my $sec = $sections[$i];
    my $pkg = $sec->{pkg};
    my $cl_pkg = $self->fallback_parser->_cl_pkg_designator($pkg);
    if ($i > 0 && !$sec->{reopen}) {
      # First section for this package: v1's full package-section preamble —
      # create/enter the CL package, CLOS class for MRO, original-case
      # registration, per-package $a/$b specials (sort comparator lambdas bind
      # them dynamically).
      my $cl_class = $self->fallback_parser->_pkg_to_clos_class($pkg);
      push @body, ";;; package $pkg",
                  "(p-defpackage $cl_pkg)",
                  "(in-package $cl_pkg)",
                  "(defclass $cl_class () ())",
                  "(p-register-pkg-name $cl_pkg \"$pkg\")", '';
    } elsif ($i > 0) {
      # Reopened (return) section: the package's section already exists, so
      # just put the CL reader back into it (v1's block-form return branch).
      push @body, ";;; back to package $pkg",
                  "(in-package $cl_pkg)", '';
    }
    push @body, @{ $sec->{decls} };
    # Versioned `package Foo 1.5;`: $VERSION defvar in decls, assignment at the
    # front of run (v1's _emit_package_version — set in source order, not BEGIN).
    my @ver_run;
    if (defined $sec->{version}) {
      (my $prefix = $cl_pkg) =~ s/^://;
      my $sym    = "$prefix\::\$VERSION";
      my $ver_cl = ($sec->{version} =~ /^\d+(?:\.\d+)?$/) ? $sec->{version}
                                                          : "\"$sec->{version}\"";
      push @body, "(eval-when (:compile-toplevel :load-toplevel :execute)",
                  "  " . global_decl_form($sym, '(make-p-box nil)') . ")";
      @ver_run = ("(p-scalar-= $sym $ver_cl)", '');
    }
    # Per-package $a/$b specials: once per package (not on reopen — duplicate
    # defvars are noisy).
    push @body, '(defvar $a (make-p-box nil))', '(defvar $b (make-p-box nil))', ''
      unless $sec->{reopen};
    # Undeclared package globals referenced in this section (v1's forward-
    # declaration pass) — defvar'd so first use isn't an unbound-variable
    # crash.  Per-section: the same unqualified $x names DIFFERENT vars in
    # different packages, and the defvar must be read under this section's
    # in-package.
    push @body, $self->_forward_global_decls(join("\n", @{ $sec->{captured} },
                                                        @{ $sec->{defs} },
                                                        @{ $sec->{sched} },
                                                        @{ $sec->{run} }),
                                             $pkg, $sec->{seg_lex}), '';
    # Declarations captured by _fallback_stmt during lowering (defvar/
    # defconstant/eval-when from use/require/BEGIN) — before the definitions
    # that may reference them.
    push @body, @{ $sec->{captured} }, '';
    # #55 interleave: merge sub defs and BEGIN/END forms by SOURCE POSITION —
    # perl compiles subs and runs BEGIN blocks in source order, so a BEGIN
    # sees exactly the subs defined above it and none below (sub-existence
    # introspection: chdir.t).  Index tie-break keeps the merge stable for
    # entries with equal positions.
    push @body, map { ($_, '') } $self->_interleaved_defs($sec);
    # Runtime current-package tracking (caller()/__PACKAGE__) in execution
    # order — after this section's definitions load, before its code runs.
    push @body, "(p-set-current-package $cl_pkg \"$pkg\")", '' if $i > 0;
    push @body, @ver_run;
    # The compile->run boundary: UNITCHECK/CHECK (reverse) then INIT (source
    # order) run once before the first runtime code — perl's phase order
    # (v1's _assemble_output emits the same call at the same seam).
    if (!$phase_boundary_emitted && grep { /\S/ } @{ $sec->{run} }) {
      push @body, "(p-run-compile-phase-blocks)", '';
      $phase_boundary_emitted = 1;
    }
    push @body, map { ($_, '') } @{ $sec->{run} };
  }
  push @body, "(p-run-compile-phase-blocks)", '' unless $phase_boundary_emitted;

  my @out = ('(in-package :pcl)', '');
  # Pre-declare every package a later section opens or a qualified symbol
  # references: load reads+evaluates one top-level form at a time, so a
  # p-defpackage up top guarantees Pkg::sym forms further down are readable.
  my %pre = map { ($_->{pkg} => 1) } @sections[1 .. $#sections];
  $pre{$_} = 1 for keys %{ $self->{_referenced_pkgs} };
  # A package DECLARED below the top level (nested `package X;` in a
  # sub/block — D1/E1.5 — or inside a BEGIN lowered whole through v1) has no
  # section, but its emission carries X-qualified symbols that must be
  # READABLE when the enclosing top-level form is read → pre-declare every
  # Statement::Package namespace in the document (top-level ones dedup here).
  $pre{ $_->namespace // 'main' } = 1
    for @{ $doc->find('PPI::Statement::Package') || [] };
  # Packages referenced via qualified CALLS (PerlIO::get_layers($fh)) register
  # in the shared Environment as the fallback expressions parse — v1 pre-
  # declares these the same way (get_undeclared_packages).
  $pre{$_} = 1 for @{ $self->environment->get_undeclared_packages() };
  delete @pre{qw(main pcl)};
  push @out, map { "(pcl:p-defpackage " . $self->fallback_parser->_cl_pkg_designator($_) . ")" }
             sort keys %pre;
  push @out, '' if %pre;
  # Cross-section forward sub calls: Perl compiles every sub before any
  # top-level code runs, so an earlier section's load-time code may call a
  # sub a LATER section defines (sort.t bug-36430: main's comparator calls
  # A::min from the flattened block's package-A segment).  v1 puts every
  # p-declare-sub no-op stub at the file top; mirror that ON DEMAND — only
  # for declare-subs whose bare cl-name is mentioned in an earlier section's
  # emitted text — so files without such calls keep byte-identical output.
  # Qualified read (Pkg::pl-name) under :pcl; the packages exist (%pre, and
  # :main via the idempotent defpackage below).
  {
    my (@stubs, %stub_seen, $need_main, @earlier);
    for my $i (0 .. $#sections) {
      my $sec = $sections[$i];
      my $earlier = join "\n", @earlier;
      (my $prefix = $self->fallback_parser->_cl_pkg_designator($sec->{pkg})) =~ s/^://;
      for my $line (@{ $sec->{decls} }, @{ $sec->{captured} }) {
        next unless $i > 0 && $line =~ /^\(p-declare-sub\s+(\S+?)\)\s*$/;
        my $name = $1;
        (my $bare = $name) =~ s/^.*:://;
        next unless index($earlier, $bare) >= 0;
        my $q = $name =~ /::/ ? $name : "$prefix\::$name";
        next if $stub_seen{$q}++;
        $need_main = 1 if $q =~ /^main::/;
        push @stubs, "(pcl:p-declare-sub $q)";
      }
      push @earlier, @{ $sec->{$_} } for qw(decls captured defs sched run);
    }
    if (@stubs) {
      push @out, '(pcl:p-defpackage :main)' if $need_main;
      push @out, @stubs, '';
    }
  }
  push @out, @body;
  $self->_seam_census_dump if _seam_census();
  return join("\n", @out);
}

# T-A1 (docs/v2-transfer-plan.md): is this top-level statement a bare block
# whose direct children include `package` statements?  Returns the block's
# significant children for flattening, or undef when the block must stay a
# single statement (its nested `package` then dies in _lower_stmt →
# whole-file v1, exactly as before this feature).
#
# Refusals — all conservative, cost is only the v2 lowering, never
# correctness:
#   - labeled block (a label makes it a by-name loop-control target);
#   - `last`/`next`/`redo`/`goto` that could target the block itself: any
#     LABELED one anywhere in the subtree, or an unlabeled one with no
#     intervening loop compound / sub body between it and the block — a
#     bare block IS a one-iteration loop in Perl, and flattening dissolves
#     the (block nil) exit that v1 gives loop control to land on;
#   - a direct-child `local` (its restore scope is the block end, which
#     flattening dissolves; deeper `local`s keep their own inner blocks).
sub _flattenable_pkg_block {
  my ($self, $child) = @_;
  return undef unless $child->isa('PPI::Statement::Compound');
  my @k = $child->schildren;
  return undef unless @k == 1 && $k[0]->isa('PPI::Structure::Block');
  my $block = $k[0];
  my @inner = grep { $_->significant } $block->children;
  return undef unless grep { $_->isa('PPI::Statement::Package') } @inner;
  return undef if grep { $self->_is_local_stmt($_) } @inner;
  for my $br (@{ $block->find('PPI::Statement::Break') || [] }) {
    my @bk = $br->schildren;
    next unless @bk && $bk[0]->isa('PPI::Token::Word')
      && $bk[0]->content =~ /^(?:last|next|redo|goto)$/;
    return undef if @bk > 1 && $bk[1]->isa('PPI::Token::Word');   # labeled
    my $safe = 0;
    for (my $p = $br->parent; $p && $p != $block; $p = $p->parent) {
      if ($p->isa('PPI::Statement::Sub')) { $safe = 1; last }     # named sub body
      if ($p->isa('PPI::Statement::Compound')
          && ($p->type // '') =~ /^(?:for|foreach|while|until)$/) {
        $safe = 1; last;                                          # real loop
      }
      if ($p->isa('PPI::Structure::Block')) {
        my $prev = $p->sprevious_sibling;                          # anon sub body
        if ($prev && $prev->isa('PPI::Token::Word') && $prev->content eq 'sub') {
          $safe = 1; last;
        }
      }
    }
    return undef unless $safe;
  }
  return \@inner;
}

# True when a my/state declaration at the pkg-block's own statement level
# (its nearest enclosing block is the pkg block itself — this includes decls
# embedded in a plain statement like `tie my $y, …`) declares a bare name in
# %$pre (file lexicals declared before the block) AND that shadowed lexical
# is still referenced after the block (_lex_referenced_after).  Only then is
# the file lexical truly SPANNING: under flattening the block's decl becomes
# a segment-top-level re-declaration of a live spanning lexical, which the
# span engine's rename machinery cannot scope (state.t: file `my (…,$y,…)` +
# `tie my $y` in the countfetches block + `$y = 0` after it).  Without a
# post-block reference the span engine handles the flattened shape exactly
# as it did before this refusal existed (rename or die → v1, never a
# miscompile) — and firing anyway rerouted eval.t's DB/Eval1 blocks to
# in-place lowering, regressing the whole file to v1 via the capture gate
# (s296's one open regression, fixed s299).
sub _pkgblock_shadows_file_lexical {
  my ($self, $child, $pre) = @_;
  return 0 unless %$pre;
  my ($block) = grep { $_->isa('PPI::Structure::Block') } $child->schildren;
  return 0 unless $block;
  for my $d (_decl_syms_under($block, sub_bounds => 1, plain => 1)) {
    my $s = $d->[1];
    (my $bare = $s->content) =~ s/^[\$\@\%]//;
    return 1 if $pre->{$bare}
      && $self->_lex_referenced_after($child, $s->content);
  }
  return 0;
}

# True when canonical lexical $canon (e.g. '$y') is REFERENCED in the
# file-level statements after $child (the pkg-block statement) — i.e. the
# pre-block file lexical of that name is still live past the block, so it
# would have to span the block's flattened segments.  Symbol/ArrayIndex uses
# are declarator- and shadow-discounted via the span engine's own predicates
# (a post-block `my $x = 3` re-decl and its scoped uses are a NEW variable,
# not a reference); interpolated mentions count sigil-aware via _interp_canon.
# Deliberately NO string-eval conservatism here (unlike _canon_refs_in): an
# eval-string mention post-block (eval.t's `eval '$x'`) is exactly what the
# rename machinery's M-F alias rule handles under flattening — counting it
# would over-refuse.  Under-firing is always safe: no refusal = the pre-s296
# flattening path, whose span engine renames correctly or dies to v1.
sub _lex_referenced_after {
  my ($self, $child, $canon) = @_;
  my @after;
  for (my $sib = $child->snext_sibling; $sib; $sib = $sib->snext_sibling) {
    push @after, $sib;
  }
  return 0 unless @after;
  (my $bare = $canon) =~ s/^[\$\@\%]//;
  my $seg_parent = $child->parent;
  for my $stmt (@after) {
    next unless ref $stmt && $stmt->isa('PPI::Node');
    for my $s (@{ $stmt->find('PPI::Token::Symbol') || [] }) {
      next unless ($s->symbol // '') eq $canon;
      next if $self->_symbol_is_declarator($s);
      next if $self->_ref_shadowed($s, $canon, \@after, $seg_parent);
      return 1;
    }
    if ($canon =~ /^\@/) {
      for my $ai (@{ $stmt->find('PPI::Token::ArrayIndex') || [] }) {
        (my $b = $ai->content) =~ s/^\$#//;
        next unless $b eq $bare;
        next if $self->_ref_shadowed($ai, $canon, \@after, $seg_parent);
        return 1;
      }
    }
    my %hit;
    _interp_canon($stmt, { $canon => 1 }, { $bare => [$canon] }, \%hit);
    return 1 if $hit{$canon};
  }
  return 0;
}

# Reset the shared Environment's notion of the current package to a segment's
# package (v1's statement-form `package` pushes with no pop — same effect).
sub _set_cur_package {
  my ($self, $pkg) = @_;
  $self->environment->package_stack(['main']);
  return if $pkg eq 'main';
  $self->environment->push_package($pkg);
  $self->environment->add_package($pkg);
}

# v2 `let`s CLOSE at a package boundary (each segment is its own top-level
# section), but Perl file lexicals stay in scope across `package`.  A my-name
# from an earlier segment mentioned in a later one → die → whole-file v1
# fallback (v1's file-scope my-vars are defvar'd specials, which DO span
# sections).  Text-scan conservative: a redeclaration of the same name or a
# mention in a comment also dies — that only costs the v2 lowering, never
# correctness.
sub _check_my_spanning {
  my ($self, $segments) = @_;
  # Keyed by CANONICAL (sigil-qualified) variable — `my %h` and `my $h` are
  # different variables, so the span check is a PPI decision (uses ->symbol to
  # resolve $h{k}→%h, $a[0]→@a), not a bare-name text scan.  See _canon_refs_in.
  my %live;       # canonical name → 1, declared in earlier segments
  my %live_blk;   # canonical name → blk id of its declaring segment (undef = file)
  for my $i (0 .. $#$segments) {
    my $blk = $segments->[$i]{blk};
    # A lexical declared inside a flattened bare block (blk-tagged segment)
    # scopes to the BLOCK: once the segment run leaves that blk, the name is
    # dead — a later same-name reference is a different variable.
    for my $c (keys %live) {
      delete $live{$c}
        if defined $live_blk{$c}
        && !(defined $blk && $blk == $live_blk{$c});
    }
    if ($i && %live) {
      my $hit = $self->_canon_refs_in($segments->[$i]{stmts}, \%live);
      for my $c (sort keys %$hit) {
        # An IDENTITY-unmangled spanning lexical (unique name, promoted to a
        # defvar under its OWN name by _rename_spanning_lexicals) is handled:
        # later-segment uses — including interpolated ones the rename could
        # not rewrite — resolve to the defvar'd global.  Mangled renames never
        # match here (the tokens carry the new name).
        next if $self->{_file_lex_renamed}{$c};
        (my $bare = $c) =~ s/^[\$\@\%]//;
        die "Parser2 TODO: my-lexical '$bare' (canon $c) spans a package boundary\n";
      }
    }
    # A BLOCK-FORM package segment (`package Foo { … }`) is a scope of its
    # own: its statements ARE the block's, so a `my` declared there is dead
    # the moment the segment ends and can never span (#254 A-iii).  Skipping
    # its declarations is the whole rule — and it must be a SKIP of the
    # declarations, not a blk-style kill on entry, because the outer lexicals
    # live ON through the block (`{ my $x; package Foo { print $x } }` is a
    # real span perl resolves to the outer $x, and dropping it here would
    # turn a die into a silently free read).  op/sub_lval.t's `$x` is the
    # opposite case: declared inside `package _102486 { … }` and merely
    # RE-USED as a different variable 40 lines later.
    next if $segments->[$i]{blockform};
    my %seg_lex;
    $self->_collect_lexical_canon($segments->[$i]{stmts}, \%seg_lex);
    for my $c (keys %seg_lex) {
      # A name already live as a FILE lexical keeps the wider scope even when
      # redeclared inside a block (shadowing) — over-checks, never under.
      $live_blk{$c} = $blk unless $live{$c} && !defined $live_blk{$c};
      $live{$c} = 1;
    }
  }
}

# Canonical (sigil-qualified) names of my/state-declared vars among the given
# TOP-LEVEL statements, added to %$live (e.g. $x, %h, @a).  The sigil-aware
# sibling of _collect_lexical_names: the span check must not conflate the
# distinct variables $h and %h.  `our`/`local` are skipped (package vars).
sub _collect_lexical_canon {
  my ($self, $stmts, $live) = @_;
  for my $stmt (@$stmts) {
    next unless ref $stmt && $stmt->isa('PPI::Statement::Variable');
    my $kw = ($stmt->schildren)[0];
    next unless $kw && $kw->isa('PPI::Token::Word') && $kw->content =~ /^(?:my|state)$/;
    my @names = grep { /^[\$\@\%]\w+$/ } $self->_declared_names($stmt);
    if (@names) { $live->{$_} = 1 for @names }
    else {
      # Unrecognized declaration shape — every symbol, canonicalized.
      my $syms = $stmt->find('PPI::Token::Symbol') || [];
      $live->{ $_->symbol } = 1 for grep { $_->content =~ /^[\$\@\%]\w+$/ } @$syms;
    }
  }
  return;
}

# Which of the live CANONICAL variables %$live are REFERENCED by the segment's
# statements — a PPI decision.  A Symbol's ->symbol resolves $h{k}→%h and
# $a[0]→@a, so $h and %h are never conflated; $#x references @x.  A reference
# can still hide from tokenisation in two places, matched conservatively by
# bare name against the live set (over-gates, never under):
#   - interpolation inside strings/regex/heredoc (_interp_names), and
#   - a string eval's (possibly single-quoted) literal, which defers a
#     reference no Symbol token covers (eval '$x').
sub _canon_refs_in {
  my ($self, $stmts, $live) = @_;
  my (%hit, %live_bare);
  for my $c (keys %$live) { (my $b = $c) =~ s/^[\$\@\%]//; push @{ $live_bare{$b} }, $c }
  # The segment's statements share one PPI parent (the Document, or the `{}` of
  # a flattened block).  Shadow analysis stops there: a `my` in a DIFFERENT
  # segment under the same parent is the span source, not a shadow.
  my ($seg_parent) = map { $_->parent } grep { ref && $_->isa('PPI::Node') } @$stmts;
  for my $stmt (@$stmts) {
    next unless ref $stmt && $stmt->isa('PPI::Node');
    # String eval anywhere in the statement → conservative bare-name scan of
    # the whole statement (a single-quoted eval body is a real deferred use).
    my $has_str_eval = $stmt->find_any(sub {
      $_[1]->isa('PPI::Token::Word') && $_[1]->content eq 'eval'
        && do { my $n = $_[1]->snext_sibling;
                $n && ($n->isa('PPI::Token::Quote')
                       || ($n->isa('PPI::Structure::List')
                           && $n->find_any('PPI::Token::Quote'))) } });
    if ($has_str_eval) {
      my $txt = $stmt->content;
      for my $b (keys %live_bare) {
        next unless $txt =~ /(?:[\$\@\%]|\$\#)\Q$b\E\b/;
        $hit{$_} = 1 for @{ $live_bare{$b} };
        warn sprintf("SPANHIT eval-scan %s line=%s stmt=%.70s\n", $b,
                     ($stmt->location||['?'])->[0], $stmt->content =~ s/\s+/ /gr)
          if $ENV{PCL_SPAN_DEBUG};
      }
      next;
    }
    for my $s (@{ $stmt->find('PPI::Token::Symbol') || [] }) {
      my $canon = $s->symbol;
      next unless $live->{$canon};
      next if $self->_symbol_is_declarator($s);              # a decl, not a use
      next if $self->_ref_shadowed($s, $canon, $stmts, $seg_parent);
      $hit{$canon} = 1;
      warn sprintf("SPANHIT symbol %s line=%s stmt=%.70s\n", $canon,
                   ($s->location||['?'])->[0], $stmt->content =~ s/\s+/ /gr)
        if $ENV{PCL_SPAN_DEBUG};
    }
    for my $ai (@{ $stmt->find('PPI::Token::ArrayIndex') || [] }) {
      (my $b = $ai->content) =~ s/^\$#//;
      next unless $live->{"\@$b"} && !$self->_ref_shadowed($ai, "\@$b", $stmts, $seg_parent);
      $hit{"\@$b"} = 1;
      warn sprintf("SPANHIT arylen \@%s line=%s\n", $b, ($ai->location||['?'])->[0])
        if $ENV{PCL_SPAN_DEBUG};
    }
    # `${x}` at CODE level is a use of $x with no Symbol token — see
    # _brace_name_refs.  Missing it here is what let a spanning `${x}` compile
    # to a read of an unbound name instead of gating (#264).
    for my $p (@{ _brace_name_refs($stmt) }) {
      my ($tok, $canon) = @$p;
      next unless $live->{$canon};
      next if $self->_ref_shadowed($tok, $canon, $stmts, $seg_parent);
      $hit{$canon} = 1;
      warn sprintf("SPANHIT brace %s line=%s stmt=%.70s\n", $canon,
                   ($tok->location||['?'])->[0], $stmt->content =~ s/\s+/ /gr)
        if $ENV{PCL_SPAN_DEBUG};
    }
    my %before = %hit;
    _interp_canon($stmt, $live, \%live_bare, \%hit);
    if ($ENV{PCL_SPAN_DEBUG}) {
      for my $c (keys %hit) {
        warn sprintf("SPANHIT interp %s line=%s stmt=%.70s\n", $c,
                     ($stmt->location||['?'])->[0], $stmt->content =~ s/\s+/ /gr)
          unless $before{$c};
      }
    }
  }
  return \%hit;
}

# Canonical variables referenced by interpolation inside a string/regex/heredoc,
# added to %$hit if live.  Sigil-AWARE (unlike _interp_names, which only grabs
# the bare name and would conflate $h/%h): "$h"→$h, "$h{k}"→%h, "$h[0]"→@h,
# "@h"→@h, "$#h"→@h.  Only the ambiguous brace-deref forms (${…}, @{…}) fall
# back to bare-name matching against the live set (over-gates, never under).
sub _interp_canon {
  my ($node, $live, $live_bare, $hit) = @_;
  for my $t (@{ $node->find('PPI::Token') || [] }) {
    my $c;
    if ($t->isa('PPI::Token::HereDoc')) {
      next if Pl::PExpr::TokenUtils::heredoc_is_raw($t);  # #301: THE shared predicate
      $c = join '', $t->heredoc;
    } elsif ($t->isa('PPI::Token::Quote::Double')
          || $t->isa('PPI::Token::Quote::Interpolate')
          || $t->isa('PPI::Token::QuoteLike::Backtick')
          || $t->isa('PPI::Token::QuoteLike::Command')
          || $t->isa('PPI::Token::QuoteLike::Regexp')      # qr/$x/
          || $t->isa('PPI::Token::QuoteLike::Readline')    # <$fh>
          || $t->isa('PPI::Token::Regexp::Match')
          || $t->isa('PPI::Token::Regexp::Substitute')) {
      $c = $t->content;
    } else { next; }
    # The name captures run to the END of a package-qualified name: perl
    # reads "$Foo::bar" as the QUALIFIED global, never as $Foo followed by
    # literal "::bar".  Stopping at the first \w+ named a variable the
    # string does not mention — harmless for a live-set lookup (qualified
    # names are never live), fatal for the requalifier, which took the
    # "@M7a" out of an already-requalified "@M7a::v" for a fresh global.
    while ($c =~ /(?<!\\)\$#(\w+(?:::\w+)*)/g) { $hit->{"\@$1"} = 1 if !$live || $live->{"\@$1"} }
    while ($c =~ /(?<!\\)[\$\@]\s*\{\s*(\w+(?:::\w+)*)/g) {          # ${x}/@{x} deref
      # Collect-all mode has no live set to disambiguate the brace form
      # against, so it names all three families — over-collection is inert
      # (a canon with no matching spelling rewrites nothing) where a miss
      # would be a silently unrequalified global.
      if (!$live) { $hit->{"\$$1"} = $hit->{"\@$1"} = $hit->{"\%$1"} = 1 }
      else { $hit->{$_} = 1 for @{ $live_bare->{$1} || [] } }
    }
    while ($c =~ /(?<!\\)\@(\w+(?:::\w+)*)/g) { $hit->{"\@$1"} = 1 if !$live || $live->{"\@$1"} }
    while ($c =~ /(?<!\\)\$(\w+(?:::\w+)*)(\s*[\[\{])?/g) {          # $x / $x[.] / $x{.}
      # Copy captures FIRST: the inner `=~ /\[/` on success resets $1 to undef
      # (no groups), which silently dropped every interpolated "$x[i]" hit.
      my ($nm, $br) = ($1, $2);
      my $sig = !$br ? '$' : $br =~ /\[/ ? '@' : '%';
      $hit->{"$sig$nm"} = 1 if !$live || $live->{"$sig$nm"};
    }
  }
  return;
}

# Is this Symbol token the variable being DECLARED by an enclosing
# my/state/our/local (the declarator), rather than a use of it?
# $kw_rx narrows WHICH declarators count.  The default is all four; the
# package-switch requalifier passes the my/state/our subset because `local`
# is not a binder — `local $v` names the very global the caller is about to
# requalify, so its symbol must be rewritten, not skipped.
sub _symbol_is_declarator {
  my ($self, $sym, $kw_rx) = @_;
  $kw_rx //= qr/^(?:my|state|our|local)$/;
  # Expression-embedded declaration (`open my $fh, …`, `func(my $x)`,
  # `foreach my $i`): the declarator keyword is the symbol's immediately
  # preceding significant sibling — there is no Statement::Variable wrapper
  # for the statement-walk below to find (M2, scalar.t's false span hits).
  my $prev = $sym->sprevious_sibling;
  return 1 if $prev && $prev->isa('PPI::Token::Word')
    && $prev->content =~ $kw_rx;
  my $stmt = $sym;
  $stmt = $stmt->parent while $stmt && !$stmt->isa('PPI::Statement');
  # The contents of `my (LIST)`'s parens parse as a nested
  # Statement::Expression — the walk above stops there and the
  # Statement::Variable test below failed for EVERY list-decl symbol
  # (caller.t's `my ($pkg, $file, $line) = caller` gated the file).
  # Climb through Expression wrappers to the enclosing statement.
  # EXACT class match: PPI::Statement::Variable ISA Statement::Expression,
  # so an isa() climb would walk OUT of genuine decl statements (that bug
  # gated do.t/each.t/vec.t/sprintf2.t when first tried with isa).
  while ($stmt && ref($stmt) eq q{PPI::Statement::Expression}) {
    my $p = $stmt->parent or last;
    $p = $p->parent while $p && !$p->isa(q{PPI::Statement});
    last unless $p;
    $stmt = $p;
  }
  return 0 unless $stmt && $stmt->isa('PPI::Statement::Variable');
  my $kw = ($stmt->schildren)[0];
  return 0 unless $kw && $kw->isa('PPI::Token::Word')
    && $kw->content =~ $kw_rx;
  # Declared names occupy the tokens BEFORE the statement's top-level `=`.
  for my $child ($stmt->schildren) {
    last if $child->isa('PPI::Token::Operator') && $child->content eq '=';
    return 1 if $child == $sym;                              # my $x
    return 1 if $child->isa('PPI::Structure::List')          # my ($x, $y)
      && grep { $_ == $sym } @{ $child->find('PPI::Token::Symbol') || [] };
  }
  return 0;
}

# Is the reference $sym (canonical $canon) shadowed by an earlier `my`/`state`
# declaration of $canon in an enclosing block of the SAME segment?  Same-
# segment is the crux: under flattening a `my` in an earlier segment sharing
# the PPI block parent is the span SOURCE, not a shadow (method.t's `my $o`).
# Only the clear case returns true; when unsure it returns false so the span
# gate still fires (soundness: never wrongly clear a genuine span).
sub _ref_shadowed {
  my ($self, $sym, $canon, $stmts, $seg_parent) = @_;
  my $node = $sym;
  while (my $parent = $node->parent) {
    my $at_seg = defined $seg_parent && $parent == $seg_parent;
    if ($parent->isa('PPI::Structure::Block')
        || $parent->isa('PPI::Statement::Sub') || $at_seg) {
      for my $sib ($parent->schildren) {
        last if $sib == $node;
        # At the shared parent, only THIS segment's own declarations shadow.
        next if $at_seg && !grep { $_ == $sib } @$stmts;
        return 1 if $self->_stmt_declares_canon($sib, $canon);
      }
    }
    last if $at_seg;                     # do not climb above the segment
    $node = $parent;
  }
  return 0;
}

# THE my/state declaration walk (#387 family 5, s413) — the one copy of what
# eight predicates each hand-rolled: find every `my`/`state` word under $root,
# locate its nearest enclosing Block AT OR BELOW $root, take the declarator
# symbol(s) after the word (`my $x` → the Symbol; `my ($p, $q)` → the Symbols
# in the List; anything else → none).  Returns [$word, $sym, $block] triples
# in document order; $block is undef when no Block lies between $word and
# $root (and $root is not itself a Block).  %opt:
#   words  => 'my' | 'state' | 'my|state' (default: both)
#   nested => 1 reports every declaration under $root (the rename blockers
#             count re-shadows); 0 (default) SKIPS a declaration whose
#             enclosing block is strictly below $root — it belongs to that
#             block, not to $root's scope.
#   sub_bounds => 1 also treats a PPI::Statement::Sub strictly below $root
#             as a nesting boundary (the pkg-block shadow test).
#   plain  => 1 keeps only `[$@%]\w+` symbols (drops `${...}` spellings).
# One walk per call — the census's "no new scope walk" rule: every caller
# below replaced its own find()+climb with this one, none added a second.
sub _decl_syms_under {
  my ($root, %opt) = @_;
  my $words = $opt{words} // 'my|state';
  my $rx    = qr/^(?:$words)$/;
  my @out;
  for my $w (@{ $root->find(sub { $_[1]->isa('PPI::Token::Word')
                                  && $_[1]->content =~ $rx }) || [] }) {
    my ($p, $block, $deep) = ($w->parent, undef, 0);
    while ($p) {
      if ($p->isa('PPI::Structure::Block')) { $block = $p; last }
      last if $p == $root;
      $deep = 1, last if $opt{sub_bounds} && $p->isa('PPI::Statement::Sub');
      $p = $p->parent;
    }
    $deep = 1 if $block && $block != $root;
    next if $deep && !$opt{nested};
    my $nx = $w->snext_sibling or next;
    my @syms = $nx->isa('PPI::Token::Symbol')   ? ($nx)
             : $nx->isa('PPI::Structure::List') ? @{ $nx->find('PPI::Token::Symbol') || [] }
             : ();
    @syms = grep { $_->content =~ /^[\$\@\%]\w+$/ } @syms if $opt{plain};
    push @out, map { [$w, $_, $block] } @syms;
  }
  return @out;
}

# Does $stmt declare canonical $canon (sigil-qualified) via my/state?
sub _stmt_declares_canon {
  my ($self, $stmt, $canon) = @_;
  return 0 unless ref $stmt;
  if ($stmt->isa('PPI::Statement::Variable')) {
    my $kw = ($stmt->schildren)[0];
    return 0 unless $kw && $kw->isa('PPI::Token::Word') && $kw->content =~ /^(?:my|state)$/;
    return scalar grep { $_ eq $canon } $self->_declared_names($stmt);
  }
  # Expression-embedded `my`/`state` in a PLAIN expression statement
  # (`open my $fh, …`, `func(my $x)`) declares into the enclosing scope
  # exactly like a my-statement (M2).  Exact-class check: a Compound
  # statement's head decl (`foreach my $x`) is scoped to the LOOP and must
  # NOT shadow later same-name references at the sibling level.  A `my`
  # nested inside a block within the statement is likewise block-scoped —
  # skipped by the nested-block climb.
  return 0 unless ref($stmt) eq 'PPI::Statement'
               || ref($stmt) eq 'PPI::Statement::Expression';
  return 1 if grep { $_->[1]->content eq $canon } _decl_syms_under($stmt);
  return 0;
}

# Bare names (sigil stripped) of my/state-declared vars among the given
# TOP-LEVEL statements, added to %$live.  `our`/`local` are skipped — they
# name package vars, which resolve per-package.  When $canons is given, also
# records the sigil-qualified canonical symbols per bare name
# ($canons->{bare}{'$x'} = 1) — the capture gates use this to test only the
# DECLARED variables: a file `my @x` must not gate on a sub that touches only
# the package global $x (bare-name text matching conflated them — my.t).
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
sub _lead_decl_with_expr_tail {
  my ($stmt) = @_;
  my @kd = _strip_semi($stmt->schildren);
  return undef unless @kd >= 3
    && $kd[0]->isa('PPI::Token::Word') && $kd[0]->content eq 'my'
    && $kd[1]->isa('PPI::Token::Symbol') && $kd[1]->content =~ /^[\$\@\%]\w+$/
    && $kd[2]->isa('PPI::Token::Operator') && $kd[2]->content ne '=';
  return $kd[1]->content;
}

sub _collect_lexical_names {
  my ($self, $stmts, $live, $canons) = @_;
  my $add = sub {
    my ($canon) = @_;
    my $bare = substr($canon, 1);
    $live->{$bare} = 1;
    $canons->{$bare}{$canon} = 1 if $canons;
  };
  for my $stmt (@$stmts) {
    next unless ref $stmt && $stmt->isa('PPI::Statement::Variable');
    my $kw = ($stmt->schildren)[0];
    next unless $kw && $kw->isa('PPI::Token::Word') && $kw->content =~ /^(?:my|state)$/;
    my ($n)    = $self->_single_scalar_decl($stmt);
    my ($vars) = $self->_multi_decl($stmt);
    if ($n)        { $add->($n) }
    elsif ($vars)  { $add->($_) for @$vars }
    elsif (defined(my $lead = _lead_decl_with_expr_tail($stmt))) { $add->($lead) }
    else {
      # Unrecognized declaration shape — take every symbol conservatively.
      my $syms = $stmt->find('PPI::Token::Symbol') || [];
      $add->($_->content)
        for grep { $_->content =~ /^[\$\@\%]\w+$/ } @$syms;
    }
  }
}

# Named top-level subs are HOISTED into the definitions bucket, OUTSIDE the
# nested `let`s that bind the file's my-lexicals — a sub body that captures
# one would compile a free (→ unbound) symbol.  v1 defvar's file lexicals, so
# the capture works there → die → whole-file v1 fallback.  (Anonymous subs
# are fine: they are lowered in place, inside the lets.)  Conservative text
# scan: a sub that re-declares the same name with its own `my` also dies —
# that only costs the v2 lowering, never correctness.
# The prototype/signature string of a named sub, or undef if it is plain.
# PPI represents an old-style prototype as a PPI::Token::Prototype (reachable
# via ->prototype), but a real signature (when `use feature 'signatures'` is in
# the document) as a PPI::Structure::Signature child — for which ->prototype is
# undef.  Both must route the sub's DEFINITION through v1's _process_sub_statement
# (imposed context / signature binding + arity checks).
sub _proto_or_sig_str {
  my ($self, $sub) = @_;
  my $p = $sub->prototype;
  return $p if defined $p;
  my ($sig) = grep { $_->isa('PPI::Structure::Signature') } $sub->children;
  return $sig ? $sig->content : undef;
}

# A PURE old-style prototype (`($;$)`, `(&@)`, `()`) never binds parameters —
# the body is a plain @_-consuming body, identical to a prototype-less sub's
# (v1's _process_sub_statement skips ALL params when is_proto).  Only a
# SIGNATURE (named params: `($x, $y = 5)`) needs v1's binding/default/arity
# machinery, so only signatures keep the v1 fallback (task #126: v1's
# _wrap_runtime_labels loses the goto tagbody in t/test.pl's
# `sub watchdog ($;$)`; the v2-native lowering handles the same body
# correctly).  The discriminator mirrors parse_prototype_or_signature's
# is_proto test — this PPI tokenizes both shapes as Token::Prototype, so the
# test is textual: a sigil followed by an identifier char means named params.
# A real PPI::Structure::Signature (feature-signatures parse) is never pure,
# even for anonymous shapes like `($)` — those carry arity checks.
sub _is_pure_prototype {
  my ($self, $sub) = @_;
  return 0 if grep { $_->isa('PPI::Structure::Signature') } $sub->children;
  my $p = $sub->prototype;
  return 0 unless defined $p;
  (my $inner = $p) =~ s/^\s*\(\s*//;
  $inner =~ s/\s*\)\s*$//;
  return $inner !~ /[\$\@\%]\w/;
}

# (The s280 `_check_interp_postderef` gate was removed in s299: postderef_qq
# interpolation is now implemented in Pl/PExpr/StringInterpolation.pm —
# lexically feature-scoped, shared by both pipelines.)

sub _check_sub_captures {
  my ($self, $stmts) = @_;
  my (%lex, %canons);
  $self->_collect_lexical_names($stmts, \%lex, \%canons);
  return unless %lex;
  for my $child (@$stmts) {
    # BEGIN/END/CHECK/UNITCHECK/INIT blocks are hoisted to compile time
    # (eval-when, via v1's p-BEGIN) OUTSIDE the runtime `let`s, so a file `my`
    # var they reference is unbound there.  v1 defvar's file lexicals for exactly
    # this compile-time visibility (CLAUDE.md §3) → die → whole-file v1.
    if ($child->isa('PPI::Statement::Scheduled') && $child->block) {
      for my $bare (sort keys %lex) {
        next if grep { $self->{_file_lex_renamed}{"$_$bare"} } '$', '@', '%';
        die "Parser2 TODO: file lexical '$bare' referenced in a "
            . $child->type . " block\n"
          if $self->_block_captures_name($child->block, $bare, $canons{$bare});
      }
      next;
    }
    next unless $child->isa('PPI::Statement::Sub') && $child->name && $child->block
      && !$child->isa('PPI::Statement::Scheduled');
    for my $bare (sort keys %lex) {
      # W5: a name already rewritten to a package-level cell ($x__file__N) is
      # legitimately captured — the hoisted sub and in-place code share the
      # one defvar'd box, so it must NOT gate.  Any sigil: container
      # promotions record '@x__file__N'/'%x__file__N', and every promotion
      # path guarantees the bare name denotes only that one variable.
      next if grep { $self->{_file_lex_renamed}{"$_$bare"} } '$', '@', '%';
      die "Parser2 TODO: file lexical '$bare' captured by sub " . $child->name . "\n"
        if $self->_block_captures_name($child->block, $bare, $canons{$bare});
    }
  }
}

# Shadow-aware capture test — the precise replacement for the raw text scan
# both capture gates used (`sub f { my $a = …; $a }` must NOT gate on an outer
# $a: the sub's $a is its own shadow, not a capture).  True iff $block contains
# a use of bare name $bare that resolves OUTSIDE the block:
#   - a Symbol/ArrayIndex use is DISCOUNTED when a my/state declaration of the
#     SAME canonical symbol inside the block strictly precedes it and the
#     declaring statement's parent scope contains the use (Perl's shadowing
#     rule; the RHS of the shadowing decl itself still sees the OUTER variable,
#     so `my $a = $a + 1` inside the sub still counts as a capture);
#   - declaration targets themselves are not uses;
#   - any occurrence inside quoted/regex/heredoc text counts —
#     interpolation and string eval reach names invisibly to token analysis
#     (this includes non-interpolating quotes, which can feed `eval`);
#   - `foreach my $x` loop-var decls are NOT recognized as shadows (they are
#     Compound-statement tokens, not Statement::Variable) — over-fires → gate
#     stays → v1; conservative, never a miscompile.
# $canons (optional hashref of '$x'/'@x'/'%x'): restrict token uses to those
# canonical symbols — a use of @a matters only when the live lexical IS @a.
# Text matches are restricted by SIGIL SHAPE per canon (a text `$x->` can only
# denote scalar $x; `$x[` an @x element; `$x{`/`@x{` the hash %x), so a
# single-quoted '$x…' no longer gates a file whose only lexical is @x (my.t).
# Whitespace is allowed before the subscript bracket: interpolation never has
# it, but eval-fed single-quoted code can — over-fire keeps the gate (safe).
sub _block_captures_name {
  my ($self, $block, $bare, $canons) = @_;
  my $re = qr/(?:[\$\@\%]|\$\#)\Q$bare\E\b/;
  # Per-CANON text patterns, so a string/heredoc/regex mention can be
  # attributed to one canonical variable and shadow-checked like a Symbol
  # use (M-F: `eval('$zzz')` under the sub's own preceding `my $zzz` refers
  # to the shadow — the eval capture alist binds it let-bound-first — not a
  # capture of the file lexical).  Without canon info the mention stays
  # unattributable and is conservatively a capture, as before.
  my %canon_pat;
  if ($canons) {
    if ($canons->{"\$$bare"}) {
      my $u = join '|', qr/\$\Q$bare\E\b(?!\s*[\[\{])/, qr/\$\{\s*\Q$bare\E\s*\}(?![\[\{])/;
      $canon_pat{"\$$bare"} = qr/$u/;
    }
    if ($canons->{"\@$bare"}) {
      my $u = join '|',
        qr/\@\Q$bare\E\b(?!\s*\{)/, qr/\$\Q$bare\E\s*\[/, qr/\$\#\Q$bare\E\b/,
        qr/[\$\@]\{\s*\Q$bare\E\s*\}\s*\[/, qr/\@\{\s*\Q$bare\E\s*\}(?!\s*\{)/,
        qr/\$\#\{\s*\Q$bare\E\s*\}/;
      $canon_pat{"\@$bare"} = qr/$u/;
    }
    if ($canons->{"\%$bare"}) {
      my $u = join '|',
        qr/\%\Q$bare\E\b/, qr/[\$\@]\Q$bare\E\s*\{/,
        qr/[\$\@]\{\s*\Q$bare\E\s*\}\s*\{/, qr/\%\{\s*\Q$bare\E\s*\}/;
      $canon_pat{"\%$bare"} = qr/$u/;
    }
  } else {
    $canon_pat{''} = $re;   # canon unknown → unattributable, never discounted
  }
  my @heredocs = @{ $block->find('PPI::Token::HereDoc') || [] };
  # Cheap early-out: the bare name appears nowhere in the text (common case).
  return 0 unless $block->content =~ $re
    || grep { join('', $_->heredoc) =~ $re } @heredocs;

  # my/state declarations of the bare name inside the block:
  # canon → [shadowing scope, ord of the decl's last token]; plus the
  # declaring Symbol tokens themselves (declaration targets, not uses).
  my (%decl, %decl_tok, %ord);
  my $i = 0;
  $ord{ refaddr $_ } = $i++ for $block->tokens;
  for my $d (@{ $block->find('PPI::Statement::Variable') || [] }) {
    my @k = _strip_semi($d->schildren);
    next unless @k >= 2 && $k[0]->isa('PPI::Token::Word')
      && $k[0]->content =~ /^(?:my|state)$/;
    my @tgt = $k[1]->isa('PPI::Token::Symbol')   ? ($k[1])
            : $k[1]->isa('PPI::Structure::List')
              ? (grep { $_->isa('PPI::Token::Symbol') } map { $_->tokens } $k[1])
            : ();
    my $last = $ord{ refaddr( ($d->tokens)[-1] ) };
    for my $t (@tgt) {
      next unless substr($t->content, 1) eq $bare;
      $decl_tok{ refaddr $t } = 1;
      push @{ $decl{ $t->content } }, [$d->parent, $last];
    }
  }
  # `for/foreach my $x (LIST) BLOCK` — the loop-head `my $x` shadows for the
  # BLOCK only (the LIST still sees the outer variable, so a use there stays
  # a capture).  E4.1 M6, s353: without this the head decl was invisible
  # (it is Compound tokens, not a Statement::Variable) and a sub whose loop
  # var merely shares a file lexical's name gated the whole file to v1.
  for my $c (@{ $block->find('PPI::Statement::Compound') || [] }) {
    my @k = $c->schildren;
    for my $j (0 .. $#k - 2) {
      next unless $k[$j]->isa('PPI::Token::Word')
        && $k[$j]->content =~ /^(?:for|foreach)$/
        && $k[$j+1]->isa('PPI::Token::Word') && $k[$j+1]->content eq 'my'
        && $k[$j+2]->isa('PPI::Token::Symbol');
      my $t = $k[$j+2];
      last unless substr($t->content, 1) eq $bare;
      my ($blk) = grep { $_->isa('PPI::Structure::Block') } @k[$j+3 .. $#k];
      last unless $blk;
      $decl_tok{ refaddr $t } = 1;
      push @{ $decl{ $t->content } }, [$blk, $ord{ refaddr $t }];
      last;
    }
  }

  # A `my`/`state` declaration EMBEDDED in some other statement.  Perl allows
  # one anywhere an expression goes, and two spellings matter here:
  #   `die "…" unless my ($how, $first) = /…/;`  — a statement MODIFIER: the
  #      names live from the end of that STATEMENT to the end of the block;
  #   `if (my $x = f()) { … }` / `while (my $l = <$fh>) { … }` — the names
  #      live in the compound statement (its blocks), not after it.
  # Neither is a PPI::Statement::Variable, so the scan above was blind to
  # both, and a sub's OWN lexical then read as a capture of a same-named file
  # lexical — op/getppid.t gated its whole file on exactly that (#254 A-i).
  # Same class of blind spot M6 closed for the `for my $x (…)` loop head, and
  # the same rule as everywhere else: the pass that DETECTS a capture and the
  # scoping the program actually has must agree.
  for my $w (@{ $block->find('PPI::Token::Word') || [] }) {
    next unless $w->content =~ /^(?:my|state)$/;
    my $pv = $w->sprevious_sibling;
    # `for my $x (…)` — handled above, with the narrower block-only scope.
    next if $pv && $pv->isa('PPI::Token::Word')
      && $pv->content =~ /^(?:for|foreach)$/;
    my $nx = $w->snext_sibling or next;
    my @tgt = $nx->isa('PPI::Token::Symbol') ? ($nx)
            : $nx->isa('PPI::Structure::List')
              ? (grep { $_->isa('PPI::Token::Symbol') } $nx->tokens)
            : ();
    next unless grep { substr($_->content, 1) eq $bare } @tgt;
    # The statement the declaration sits in, innermost-first.
    my $stmt;
    for (my $p = $w->parent; $p; $p = $p->parent) {
      if ($p->isa('PPI::Statement')) { $stmt = $p; last }
      last if refaddr($p) == refaddr($block);
    }
    next unless $stmt;
    next if $stmt->isa('PPI::Statement::Variable');   # already scanned above
    # In a compound statement's HEAD the names scope to that statement (its
    # blocks); anywhere else they scope to the rest of the enclosing block.
    my $head = $stmt->parent;
    my ($scope, $last) =
      ($head && ($head->isa('PPI::Structure::Condition')
                 || $head->isa('PPI::Structure::For')) && $head->parent)
        ? ($head->parent,  $ord{ refaddr( ($head->tokens)[-1] ) })
        : ($stmt->parent,  $ord{ refaddr( ($stmt->tokens)[-1] ) });
    next unless $scope && defined $last;
    for my $t (@tgt) {
      next unless substr($t->content, 1) eq $bare;
      $decl_tok{ refaddr $t } = 1;
      push @{ $decl{ $t->content } }, [$scope, $last];
    }
  }

  my $shadowed = sub {
    my ($tok, $canon) = @_;
    my $u = $ord{ refaddr $tok };
    return 0 unless defined $u;
    for my $dd (@{ $decl{$canon} || [] }) {
      my ($scope, $dlast) = @$dd;
      next unless $dlast < $u;                    # decl strictly precedes use
      next unless $scope;                         # decl's enclosing scope
      for (my $p = $tok->parent; $p; $p = $p->parent) {
        return 1 if refaddr($p) == refaddr($scope);
        last if refaddr($p) == refaddr($block);
      }
    }
    return 0;
  };

  for my $t ($block->tokens) {
    if ($t->isa('PPI::Token::Symbol')) {
      next if $decl_tok{ refaddr $t };
      my $canon = $t->symbol;
      next unless $canon =~ /^[\$\@\%]\Q$bare\E$/;
      next if $canons && !$canons->{$canon};
      return 1 unless $shadowed->($t, $canon);
    } elsif ($t->isa('PPI::Token::ArrayIndex')) {
      next unless $t->content eq "\$#$bare";
      next if $canons && !$canons->{"\@$bare"};
      return 1 unless $shadowed->($t, "\@$bare");
    } elsif ($t->isa('PPI::Token::HereDoc')) {
      my $txt = join('', $t->heredoc);
      for my $c (sort keys %canon_pat) {
        next unless $txt =~ $canon_pat{$c};
        return 1 if !length($c) || !$shadowed->($t, $c);
      }
    } elsif ($t->isa('PPI::Token::Quote')
          || $t->isa('PPI::Token::QuoteLike')
          || $t->isa('PPI::Token::Regexp')) {
      for my $c (sort keys %canon_pat) {
        next unless $t->content =~ $canon_pat{$c};
        return 1 if !length($c) || !$shadowed->($t, $c);
      }
    }
  }
  return 0;
}


# Shared fact scan for the W5/W10 lexical-rename passes.  Accumulates into
# $f (so callers can scan one segment or the whole file):
#   decl_count{bare}   — count of my/state declarations of the bare name;
#   scalar_decl{bare}  — the `my $x` single-scalar declaration STATEMENTS
#                        outside named subs (rename candidates);
#   disq{bare}         — names unrenameable by Symbol-token content: used in
#                        array/hash family form (@x, %x, $#x, $x[…], $x{…})
#                        or interpolated in a string/regex/heredoc.
# Every STATEMENT in a fact set that declares the scalar `$bare` as its own
# lexical: the single-scalar form (`my $x [= …]`, recorded in scalar_decl) and
# the plain LIST form (`my ($x, $y)`, recorded per name in mlist_decl).  perl
# declares each name in a list form exactly as the single form does, and the
# span rename touches ONE symbol inside the declaring statement either way, so
# for that pass the two shapes are interchangeable — which is why they are
# merged HERE rather than by conflating the two facts (the capture-promotion
# pass reads them separately on purpose: it promotes per CANON and needs the
# sigil-carrying name, and it would otherwise process a list decl twice).
#
# This is #314 family F-D: without it `my ($fetch, $store) = (0, 0);` gave
# `sdecls=0 dc=1` — the declaration counted, but no statement was found to
# rename — so a package-spanning list-form lexical refused and the CHECKER
# killed the whole file (io/shm.t, op/taint.t).
sub _scalar_decl_stmts {
  my ($facts, $bare) = @_;
  return [ @{ $facts->{scalar_decl}{$bare} || [] },
           map  { $_->[0] }
           grep { $_->[1] =~ /^\$/ }
           @{ $facts->{mlist_decl}{$bare} || [] } ];
}

sub _scan_lex_facts {
  my ($self, $stmts, $f) = @_;
  $f->{$_} //= {} for qw(decl_count canon_decl_count scalar_decl disq
                         container_decl interp family mlist_decl);
  for my $stmt (@$stmts) {
    my @vstmts = $stmt->isa('PPI::Statement::Variable') ? ($stmt) : ();
    push @vstmts, @{ $stmt->find('PPI::Statement::Variable') || [] };
    for my $v (@vstmts) {
      my $kw = ($v->schildren)[0];
      next unless $kw && $kw->isa('PPI::Token::Word') && $kw->content =~ /^(?:my|state)$/;
      for my $dn ($self->_declared_names($v)) {
        (my $bare = $dn) =~ s/^[\$\@\%]//;
        $f->{decl_count}{$bare}++;
        # Sigil-exact count: `my @x` and `my $x` are different variables AND
        # different CL symbols — the container span path's file-uniqueness
        # rule needs the count of THIS canon only (nested shadows included).
        $f->{canon_decl_count}{$dn}++;
      }
      my ($name) = $self->_single_scalar_decl($v);
      if ($name && $kw->content eq 'my') {
        (my $bare = $name) =~ s/^\$//;
        push @{ $f->{scalar_decl}{$bare} }, $v;
      }
      # A single-container `my %h` / `my @a` [= INIT] is a promotion candidate
      # — recorded WITH its sigil-carrying symbol so the sigil-aware rewrite
      # can follow element/whole/slice uses to the new name, and with its
      # has-init flag (the SPAN pass still refuses init'd decls; the capture
      # promotion lowers the init as a write-through assignment).
      my ($cvars, $chas_init) = $self->_multi_decl($v);
      if ($cvars && @$cvars == 1 && $cvars->[0] =~ /^[\@\%]\w+$/
          && $kw->content eq 'my') {
        (my $bare = $cvars->[0]) =~ s/^[\@\%]//;
        push @{ $f->{container_decl}{$bare} }, [$v, $cvars->[0], $chas_init];
      }
      # A list decl of plain names `my ($a, @b, %c) [= INIT]`: each name is
      # its own per-name promotion candidate, recorded with its canonical
      # (sigil-carrying) symbol (push.t's `my ($first,$second)=…;
      # sub two_things { ($first,$second) }`; undef.t's `my (%hash,%mirror)`).
      # The ONE-element spelling `my ($x) = @_;` is the SAME declaration with
      # the same promotion, and it used to fall between every branch here: it
      # is not _single_scalar_decl's shape (a list assignment), the container
      # branch above wants a [@%] sigil, and this one wanted two names — so
      # the promoter never saw the commonest way a sub takes a parameter, and
      # a nested named sub capturing it killed the whole FILE with "lexical
      # 'x' possibly captured".  `my ($x, $y) = @_` two lines away promoted
      # fine (#377; the N=1-is-the-N=k rule again, cf. #267).  The single
      # CONTAINER spelling `my (@a) = …` stays the container branch's.
      if ($cvars && !(grep { !/^[\$\@\%]\w+$/ } @$cvars)
          && $kw->content eq 'my'
          && (@$cvars > 1 || $cvars->[0] =~ /^\$/)) {
        for my $mv (@$cvars) {
          (my $bare = $mv) =~ s/^[\$\@\%]//;
          push @{ $f->{mlist_decl}{$bare} }, [$v, $mv];
        }
      }
      # NOTE (M-D): decls INSIDE named subs are candidates too — a nested
      # named sub capturing an enclosing sub's lexical needs the same
      # promotion (index.t's tie-handler STORE).  Harmless for ordinary sub
      # locals: promotion requires capture by a named sub inside the extent.
    }
    # Any symbol whose canonical form is @x / %x (incl. $x[…] / $x{…} element
    # access, whose ->symbol resolves to the container) disqualifies bare `x`.
    for my $s (@{ $stmt->find('PPI::Token::Symbol') || [] }) {
      my $canon = $s->symbol;
      (my $bare = $canon) =~ s/^[\$\@\%]//;
      if (substr($canon, 0, 1) ne '$') {
        $f->{disq}{$bare}   = 1;
        $f->{family}{$bare} = 1;   # family-form use, independent of interp
      }
    }
    for my $ai (@{ $stmt->find('PPI::Token::ArrayIndex') || [] }) {
      (my $bare = $ai->content) =~ s/^\$#//;
      $f->{disq}{$bare}   = 1;
      $f->{family}{$bare} = 1;
    }
    # A name INTERPOLATED inside a string/regex/heredoc is not a Symbol token,
    # so a token rewrite can't reach it.  Recorded in BOTH `disq` (the scalar
    # path's blanket blocker) and `interp` (the container path, which treats
    # container-family uses as renameable and so cannot use `disq` — it checks
    # interpolation on its own).
    my %ih;
    _interp_names($stmt, \%ih);
    for my $n (keys %ih) { $f->{disq}{$n} = 1; $f->{interp}{$n} = 1; }
    # A CONTAINER also interpolates as @x / @{x} / @x{…} — NOT caught by the
    # scalar-sigil scan above, and a token rewrite cannot reach inside a string.
    # Record it in `interp` (the container-promotion guard) so a name used that
    # way is never renamed; leave `disq` (the scalar guard) untouched so the
    # scalar path's behaviour — and its byte-for-byte output — is unchanged.
    my %ah;
    _interp_names($stmt, \%ah, '\@');
    for my $n (keys %ah) { $f->{interp}{$n} = 1 }
  }
}

# W10: my-lexical spanning a package boundary (see the parse() comment).
# Subset (anything outside it keeps the _check_my_spanning gate → v1):
#   - exactly ONE my/state declaration of the bare name in the whole file,
#     and it is a top-level `my $x` scalar declaration (same as W5);
#   - never used as a ${x} deref-block (text the interp fixer cannot
#     attribute).  Sibling @x/%x family uses are fine (M-F): Symbol rewrites
#     key on ->symbol and the interp fixer skips `$x[`/`$x{`.  Interpolated
#     `$x` uses are rewritten by the fixer (M-A);
#   - the declaring segment is not a package-BLOCK segment (a block-scoped
#     `my` does NOT span in Perl — later same-name uses are package globals);
#   - a string eval naming the original `$x` is handled (M-F): each extent
#     segment records original→cell in eval_span_captures, and the capture
#     alist carries the pair (see _eval_lexical_alist).
# References BEFORE the declaration (earlier segments, or earlier statements
# of the declaring segment, or the decl's own RHS) are package globals of a
# DIFFERENT variable — left untouched, exactly Perl's visibility rule.
# The last segment index in the same flattened-block run as segment $di (its
# blk tag), i.e. the extent a block-scoped `my` from $di stays live over.  For
# a file-level decl (no blk) the extent is all later segments.
sub _blk_extent {
  my ($segments, $di) = @_;
  my $blk = $segments->[$di]{blk};
  return $#$segments unless defined $blk;
  my $last = $di;
  $last++ while $last < $#$segments
    && defined $segments->[$last + 1]{blk}
    && $segments->[$last + 1]{blk} == $blk;
  return $last;
}

# Declarations of bare name $bare among a segment's statements that must count
# against the span-rename's "sole binding in its extent" rule (M3):
#   - a decl at the segment's TOP LEVEL (same scope level as the span decl:
#     a genuine same-level re-binding);
#   - a decl in a Compound statement's HEAD (`foreach my $x`, `if (my $x…)`)
#     — its scope is the construct, which _ref_shadowed cannot delimit, so it
#     cannot be safely skipped by the rewrite.
# NOT counted: a decl nested inside a Structure::Block or a named sub — a
# distinct shadowing variable whose scope the rewrite skips via
# _symbol_is_declarator + _ref_shadowed.  Both Statement::Variable decls and
# expression-embedded ones (`open my $fh`) are recognized.
sub _hard_decl_count {
  my ($self, $stmts, $bare, $sig) = @_;
  # $sig: count declarations of ONE canonical variable (e.g. '%' → only
  # `my %x`).  Default counts every sigil of the bare name — the conflated
  # mode the scalar-promotion ambiguity gate relies on.
  my $sigpat = defined $sig ? quotemeta($sig) : '[\$\@\%]';
  my $hard = 0;
  for my $stmt (@$stmts) {
    next unless ref $stmt && $stmt->isa('PPI::Node');
    for my $w (@{ $stmt->find(sub {
          $_[1]->isa('PPI::Token::Word')
          && $_[1]->content =~ /^(?:my|state)$/ }) || [] }) {
      # Declared names directly following this declarator keyword.
      my $nx = $w->snext_sibling or next;
      my @names = $nx->isa('PPI::Token::Symbol')   ? ($nx->content)
                : $nx->isa('PPI::Structure::List')
                  ? (map { $_->content } @{ $nx->find('PPI::Token::Symbol') || [] })
                : ();
      next unless grep { /^$sigpat\Q$bare\E$/ } @names;
      # Classify by enclosure: block/sub-nested → scopeable shadow (skip);
      # Compound-enclosed (head position, no block in between) → hard;
      # else (statement top level of the segment) → hard.
      my ($p, $kind) = ($w->parent, 'top');
      while ($p && $p != $stmt) {
        if ($p->isa('PPI::Structure::Block'))    { $kind = 'shadow'; last }
        if ($p->isa('PPI::Statement::Sub'))      { $kind = 'shadow'; last }
        $p = $p->parent;
      }
      $kind = 'hard'
        if $kind eq 'top' && $stmt->isa('PPI::Statement::Compound');
      $hard++ unless $kind eq 'shadow';
    }
  }
  return $hard;
}

sub _rename_spanning_lexicals {
  my ($self, $segments) = @_;

  # Spanning names, detected exactly as _check_my_spanning does: declared in
  # an earlier segment, textually used in a later one — with the same
  # blk-extent rule (a block lexical dies at its block's end, so it can only
  # span within its own blk run).
  my (%decl_seg, %spanning, %live, %live_blk);
  for my $i (0 .. $#$segments) {
    my $blk = $segments->[$i]{blk};
    for my $bare (keys %live) {
      delete $live{$bare}
        if defined $live_blk{$bare}
        && !(defined $blk && $blk == $live_blk{$bare});
    }
    if ($i && %live) {
      my $txt = join "\n", map { $_->content } @{ $segments->[$i]{stmts} };
      for my $bare (keys %live) {
        # `\{?`: the BRACED spelling `"${x}"` is a use of $x, and missing it
        # was an invariant break, not a conservative refusal — this pre-filter
        # decides which names the pass CONSIDERS, so a name it skips is never
        # renamed, prints no SPANREFUSE, and then dies in the CHECKER, which
        # resolves uses properly (`_canon_refs_in` reads interpolation).  The
        # pass's contract is that it never refuses a name the checker will die
        # on (M4, s354); op/exec.t:215 `qq{${quote}…}` broke it.  Same braced
        # blind spot M2 fixed for #226's collapse in s353.
        # OVER-matching is safe BY DESIGN and stays that way: this is only a
        # pre-filter, and every name it admits still has to pass the CANON
        # span test ($scalar_spans / the SPANSCAN below, both `_canon_refs_in`)
        # before anything is renamed — which is what already throws out a
        # sibling `my @x` marking `x`, and now also throws out `'${x}'` inside
        # a NON-interpolating literal.
        $spanning{$bare} = 1 if $txt =~ /(?:[\$\@\%]|\$\#)\{?\Q$bare\E\b/;
      }
    }
    # Block-form package segment: its `my`s die with the block, so they are
    # not span candidates — the same skip the CHECKER does, in the same place,
    # so the two passes agree about what spans (#254 A-iii).
    next if $segments->[$i]{blockform};
    my %seg_lex;
    $self->_collect_lexical_names($segments->[$i]{stmts}, \%seg_lex);
    for my $bare (keys %seg_lex) {
      $decl_seg{$bare} //= $i;
      $live_blk{$bare} = $blk unless $live{$bare} && !defined $live_blk{$bare};
      $live{$bare} = 1;
    }
  }
  return unless %spanning;

  my $f = {};
  $self->_scan_lex_facts($_->{stmts}, $f) for @$segments;
  # Per-segment facts too (W10-ext-1): a `my $x` in a flattened block scopes to
  # that block, so the eligibility facts for a block-declared span need only
  # hold over the block's segment run — a same-name re-declaration in a
  # DIFFERENT block (method.t's second `my $o`) is a distinct variable the
  # block-bounded rewrite never touches, so it must not inflate decl_count.
  my @sf;
  for my $i (0 .. $#$segments) {
    my $g = {};
    $self->_scan_lex_facts($segments->[$i]{stmts}, $g);
    $sf[$i] = $g;
  }
  my $alltxt = join "\n", map { map { $_->content } @{ $_->{stmts} } } @$segments;

  # (W10-ext-4's per-segment eval scan and its 'eval-unsafe (non-unique)'
  # refusal were REMOVED in M-F: a mangled rename now registers an
  # original-name → cell pair on each extent segment (see the registration
  # at the bottom of the instance loop), so a string eval naming the
  # original `$x` — literal or dynamic — captures the renamed cell through
  # the s250 alist.)

  for my $bare (sort keys %spanning) {
    # M-B (per-declaration span tracking): a spanning bare name can carry
    # SEVERAL declaration instances — the file lexical plus a same-name
    # re-decl at the TOP of a flattened blk segment (its shadow itself spans
    # segments within its blk run, e.g. sort.t's second `my $answer`).  The
    # bare-name model conflated them (dc=2 → refuse → die → v1).  Instead,
    # enumerate every top-level single-scalar decl instance and process them
    # INNERMOST/LATEST FIRST: renaming the later instance consumes its uses
    # (its extent is its blk run), so when the earlier instance's facts are
    # re-scanned it sees only its own decl and uses — every eligibility rule
    # below then applies per instance unchanged.  A refused spanning name
    # previously always died (whole-file v1), so multi-instance promotion can
    # only change files that were gated — never a v2-native file's emission.
    my @inst;
    for my $i (0 .. $#$segments) {
      my $top = $segments->[$i]{stmts};
      for my $v (@{ _scalar_decl_stmts($sf[$i], $bare) }) {
        my ($ix) = grep { $top->[$_] == $v } 0 .. $#$top;
        push @inst, [$i, $ix, $v] if defined $ix;
      }
    }
    @inst = sort { $b->[0] <=> $a->[0] || $b->[1] <=> $a->[1] } @inst;
    @inst = ([$decl_seg{$bare}, undef, undef]) unless @inst;  # keep bare-name refusal traces
    my $multi = @inst > 1;
    # CANON-exact span test for the SCALAR `$bare` (M4, s354).  %spanning is a
    # bare-name TEXT pre-filter, so a sibling `my @x` used in a later segment
    # marks `x` as spanning even when the scalar never crosses the boundary —
    # and the loop below would then rename a variable the CHECKER
    # (_check_my_spanning) never had a complaint about.  Ask the checker's own
    # resolver instead: _canon_refs_in is the shared primitive that reads
    # `$x[i]`/`$#x`/`"@x"` as the ARRAY and only `$x`/`"$x"` as the scalar
    # (same shape as the container loop's SPANSCAN below).  Keeping the two in
    # step is what makes "the rename never refuses a name the checker will die
    # on" hold; the whole point of the pass.
    my $scalar_spans = sub {
      my ($lo, $up) = @_;
      for my $j ($lo .. $up) {
        return 1
          if $self->_canon_refs_in($segments->[$j]{stmts}, { "\$$bare" => 1 })->{"\$$bare"};
      }
      return 0;
    };
    # Multi-instance: if ANY instance spans, EVERY instance must be renamed —
    # a sibling left under the original name would (a) keep the spanning
    # instance's dc at 2 and (b) sit exposed to its qualified rewrite (it is
    # segment-top-level, invisible to the block-shadow skip).  If none spans
    # (%spanning was a text false positive from this bare name's other
    # sigils/packages), leave everything byte-untouched.  (scalar.t: file
    # `my $fh` spans; a second `my $fh` at the top of a flattened blk run
    # doesn't — both rename, each within its own extent.)
    if ($multi) {
      my $any = 0;
      for my $in (@inst) {
        my $ihi = _blk_extent($segments, $in->[0]);
        $any ||= $scalar_spans->($in->[0] + 1, $ihi);
        last if $any;
      }
      next unless $any;
    }
    # Facts view for the eligibility checks: the shared pre-scan for the
    # single-instance case (byte-identical to the bare-name model), a fresh
    # local re-scan per instance in the multi case (must see prior renames).
    my ($cf, $csf, $ctxt) = ($f, \@sf, $alltxt);
    for my $in (@inst) {
    my ($di, $inst_idx, $inst_decl) = @$in;
    if ($multi) {
      $cf = {}; $self->_scan_lex_facts($_->{stmts}, $cf) for @$segments;
      $csf = [];
      for my $i (0 .. $#$segments) {
        my $g = {};
        $self->_scan_lex_facts($segments->[$i]{stmts}, $g);
        $csf->[$i] = $g;
      }
      $ctxt = join "\n", map { map { $_->content } @{ $_->{stmts} } } @$segments;
    }
    # A name with exactly ONE my/state binding file-wide can be renamed to the
    # PLAIN package global $Pkg::name (no __file__N mangle): there is no other
    # `let $name` in the file for the defvar to poison.  The unmangle also
    # neutralises the string-eval hazard — a dynamic `eval $var` whose runtime
    # text references the bare `$name`, running in the declaring package,
    # resolves to $Pkg::name (the same cell), so the mangle-driven eval guard
    # is unnecessary here (a cross-package eval matches v1, which likewise
    # defvars file lexicals — not a regression).  Only the NON-unique case
    # must mangle (to protect the sibling `let`) and therefore keep the guard.
    my $unique = (($cf->{decl_count}{$bare} // 0) == 1);
    my $refuse = sub {
      warn "SPANREFUSE $bare\@seg$di: $_[0]\n" if $ENV{PCL_SPAN_DEBUG};
      return 1;
    };
    # Facts scoped to the declaration's live extent: the block's segment run for
    # a flattened-block decl, else all later segments (file lexical).  Decls or
    # disqualifying uses outside that range belong to a different variable.
    # Shadow-aware (M3): a re-declaration NESTED in a block (or sub) within the
    # extent is a DISTINCT shadowing variable — it does not block the rename;
    # the rewrite below skips its scope instead (declarator skip +
    # _ref_shadowed).  Only a same-level (segment top-level) re-decl, or a
    # decl form the shadow machinery cannot scope (a Compound head like
    # `foreach my $x`), refuses.
    my $hi = _blk_extent($segments, $di);
    # Single instance: rename only if the SCALAR itself crosses (M4).  The
    # multi case already answered this above, and there it is all-or-nothing —
    # a sibling instance left under the original name would re-inflate the
    # spanning one's decl count and sit exposed to its qualified rewrite.
    next if !$multi && !$scalar_spans->($di + 1, $hi)
      && $refuse->('scalar does not cross the boundary (canon)');
    my ($dc, $family, $interp, @sdecls) = (0, 0, 0);
    for my $j ($di .. $hi) {
      my $top = $segments->[$j]{stmts};
      # CANON-exact (M4, s354): `my %mix` beside `my $mix` is a different
      # variable and a different CL symbol — counting it as a re-declaration
      # of `$mix` refused a rename that is not ambiguous at all.  This is the
      # same canonical resolution _check_my_spanning already uses (->symbol),
      # and the last conflated site in this pass: the M-F comment below
      # already established that family USES are safe here (Symbol rewrites
      # key on ->symbol; the `$x` interp fixer skips `$x[`/`$x{`), so the
      # matching family DECL must not refuse either.  (The capture-promotion
      # pass keeps its conflated count on purpose — see _promote_captured.)
      $dc     += $self->_hard_decl_count($top, $bare, '$');
      $family ||= $csf->[$j]{family}{$bare};
      $interp ||= $csf->[$j]{interp}{$bare};
      push @sdecls, grep { my $v = $_; grep { $_ == $v } @$top }
                    @{ _scalar_decl_stmts($csf->[$j], $bare) };
    }
    # (The old blanket 'family use (@x/%x/$#x)' refusal was REMOVED in M-F:
    # Symbol rewrites key on ->symbol (a sibling @x/%x is never touched),
    # $#x is an ArrayIndex token the scalar loops never rewrite, and the
    # interp fixer's scalar pattern skips `$x[`/`$x{` — which is also
    # Perl-correct, since "$x[0]" interpolates @x's element, never the
    # scalar.  The ${x} deref-block refusal below still guards the one text
    # shape the fixer cannot attribute.)
    # An interpolated use is text a rename cannot rewrite.  It is safe ONLY on
    # the identity-unmangle path (the name is unchanged, so interpolation
    # keeps resolving to the defvar'd global) and only where the interpolating
    # segment is the DECLARING package — a bare `$x` interpolated in another
    # package's segment would read THAT package's symbol, not the cell.
    # STAGED (M-B session 3): the rename loops below already carry the M-A
    # interp fixer (mangled + cross-package identity), so this refusal can be
    # DROPPED once the scalar.t divergence it exposed is fixed — de-gated
    # scalar.t ran 78+36/128 PARTIAL (early stop after t126, new fail t64
    # "new value preserved") vs the v1 baseline 81/35/12 complete.  Debug
    # that first; sort.t needs no interp and is unaffected either way.
    # STAGED DROP (M-B session 3): the interp fixer in the rename loops below
    # rewrites interpolated uses (mangled + cross-package identity), so the old
    # blanket 'interpolated use' refusal is removed.  See the block comment
    # above.
    next if !(@sdecls == 1 && $dc == 1)
      && $refuse->('sdecls=' . scalar(@sdecls) . " dc=$dc");
    next if $multi && $sdecls[0] != $inst_decl
      && $refuse->('extent sole decl is not this instance');
    # (The `${x} deref-block` refusal that used to sit here is GONE, #264: the
    # rewrite loops below now set the Word inside the Cast's Block, so the one
    # shape it guarded — a CODE-level `${x}` of THIS scalar — is handled.  The
    # other sigils it also refused (`@{x}`, `%{x}`, `$#{x}`) are DIFFERENT
    # canonical variables that renaming `$x` never touches: the refusal was
    # sigil-blind, the same complaint as the capture path's family-use rule.)
    next if $segments->[$di]{blockform} && $refuse->('blockform decl segment');
    my $decl  = $sdecls[0];
    my $stmts = $segments->[$di]{stmts};
    my ($idx) = grep { $stmts->[$_] == $decl } 0 .. $#$stmts;
    next if !defined $idx && $refuse->('decl not top-level in its segment');
    my ($sym) = grep { $_->content eq "\$$bare" }
                @{ $decl->find('PPI::Token::Symbol') || [] };
    next if !$sym && $refuse->('decl symbol not found');

    my $newbare = $unique ? $bare
                          : $bare . '__file__' . $self->{_file_lex_counter}++;
    # A use inside a shadowing scope (block-nested re-decl of the same bare
    # name — a DISTINCT variable) must keep its original name (M3): skip the
    # shadow decl's own declarator symbol and every use _ref_shadowed
    # attributes to it.  The extent dc above counted only same-level /
    # unscopeable re-decls, so everything skipped here is a genuine shadow.
    my $skip_shadowed = sub {
      my ($s, $seg_stmts, $seg_parent) = @_;
      return 1 if $self->_symbol_is_declarator($s);
      return 1 if $self->_ref_shadowed($s, "\$$bare", $seg_stmts, $seg_parent);
      return 0;
    };
    # Declaring segment: the decl symbol itself (its RHS reads the outer
    # global — _rename_decl_within's rule), then every use in later
    # statements of the segment.
    $self->_rename_decl_within($decl, $sym, "\$$newbare");
    my ($dsp) = map { $_->parent } grep { ref && $_->isa('PPI::Node') } @$stmts;
    my $decl_fix = $unique ? undef : _interp_fixer("\$$bare", $newbare);
    for my $j ($idx + 1 .. $#$stmts) {
      next unless ref $stmts->[$j] && $stmts->[$j]->isa('PPI::Node');
      for my $s (@{ $stmts->[$j]->find('PPI::Token::Symbol') || [] }) {
        next unless $s->symbol eq "\$$bare";
        next if $skip_shadowed->($s, $stmts, $dsp);
        $s->set_content("\$$newbare");
      }
      # The same use spelled `${x}` — a Word inside a Cast's Block, invisible
      # to the Symbol loop above (#264).  The DETECTOR counts it, so the
      # rewrite must reach it or the rename would leave a live mention behind.
      for my $p (@{ _brace_name_refs($stmts->[$j]) }) {
        next unless $p->[1] eq "\$$bare";
        next if $self->_ref_shadowed($p->[0], "\$$bare", $stmts, $dsp);
        $p->[0]->set_content($newbare);
      }
      # Mangled path: interp text must follow the rename (identity keeps the
      # name, so same-package interp needs nothing).  Shadow scopes keep the
      # outer text: their $bare is a different variable.  The shadow
      # predicate is handed DOWN so it is asked only about tokens the fixer
      # matched — per token it is a tree walk, i.e. quadratic (#184).
      if ($decl_fix) {
        my $dskip = sub { $self->_ref_shadowed($_[0], "\$$bare", $stmts, $dsp) };
        for my $t (@{ $stmts->[$j]->find('PPI::Token') || [] }) {
          next unless _interp_token_candidate($t);
          _fix_interp_token($t, $decl_fix, $dskip);
        }
      }
    }
    # Later segments: the package-qualified form — their sections' reader
    # sits in THEIR package; the qualified symbol reaches the declaring
    # section's defvar (which has already loaded — sections load in order).
    # A block lexical's rewrite stops at its blk run's end: text after the
    # block is out of the lexical's Perl scope (a same-name mention there is
    # a different variable) and must NOT be rewritten — the same extent the
    # eligibility facts above were scoped to.
    my $last_j = $hi;
    my $qual = '$' . $segments->[$di]{pkg} . '::' . $newbare;
    my $qual_fix = _interp_fixer("\$$bare", $segments->[$di]{pkg} . '::' . $newbare);
    for my $j ($di + 1 .. $last_j) {
      my $seg_stmts = $segments->[$j]{stmts};
      my ($sp) = map { $_->parent } grep { ref && $_->isa('PPI::Node') } @$seg_stmts;
      # Identity path + same package: interp already resolves to the defvar'd
      # global under the unchanged name — leave the text byte-untouched.
      my $do_interp = !$unique
        || $segments->[$j]{pkg} ne $segments->[$di]{pkg};
      # As in _rewrite_var_uses (#184): the shadow predicate goes DOWN to
      # _fix_interp_token so only matched tokens pay its tree walk.
      my $qskip = sub { $self->_ref_shadowed($_[0], "\$$bare", $seg_stmts, $sp) };
      for my $stmt (@$seg_stmts) {
        next unless ref $stmt && $stmt->isa('PPI::Node');
        for my $s (@{ $stmt->find('PPI::Token::Symbol') || [] }) {
          next unless $s->symbol eq "\$$bare";
          next if $skip_shadowed->($s, $seg_stmts, $sp);
          $s->set_content($qual);
        }
        # `${x}` in a later segment → `${Pkg::newbare}`, the brace spelling of
        # the qualified symbol the loop above writes (#264).
        for my $p (@{ _brace_name_refs($stmt) }) {
          next unless $p->[1] eq "\$$bare";
          next if $self->_ref_shadowed($p->[0], "\$$bare", $seg_stmts, $sp);
          $p->[0]->set_content($segments->[$di]{pkg} . '::' . $newbare);
        }
        if ($do_interp) {
          for my $t (@{ $stmt->find('PPI::Token') || [] }) {
            next unless _interp_token_candidate($t);
            _fix_interp_token($t, $qual_fix, $qskip);
          }
        }
      }
    }
    $self->{_file_lex_renamed}{"\$$newbare"} = 1;
    # M-F: a MANGLED rename is invisible to a string eval that names the
    # original `$x` (the s250 capture alist finds lexicals by name).  Record
    # original→cell on every segment of the extent; the section driver
    # publishes the current segment's map to the fallback parser, and
    # _eval_lexical_alist appends the pairs (after let-bound ones, so a live
    # shadow wins by assoc order).  The package-QUALIFIED spelling is valid
    # in every segment (same symbol the later-segment token rewrite uses).
    # `//=` keeps the innermost instance's cell where extents overlap (the
    # instance loop runs innermost-first).  Identity renames need no pair:
    # the unchanged name resolves to the defvar'd global (see the $unique
    # comment above).
    if (!$unique) {
      my $pkg = $segments->[$di]{pkg};
      my $cl_pkg = $pkg =~ /::/ ? "|$pkg|" : $pkg;
      for my $j ($di .. $hi) {
        $segments->[$j]{eval_span_captures}{"\$$bare"} //= "${cl_pkg}::\$$newbare";
      }
    }
    }   # per-declaration instance
  }

  # W10-ext-3: containers (%h / @a) spanning a package boundary.  Same span
  # rename as the scalar loop above, but container-family uses (%h, $h{k},
  # @h{@ks}, $#a, $a[i]) resolve via ->symbol to the container while carrying
  # DIFFERENT leading sigils in their token content — the rewrite preserves
  # each token's sigil and replaces only the NAME.  File-unique only, so the
  # cell keeps the PLAIN (unmangled) name: a container is never the sibling
  # `let` the mangle exists to protect, and — as with unique scalars — the
  # identity name neutralises the string-eval hazard, so no eval guard is
  # needed.  The decl lowers via _file_lex_renamed as a hoisted defvar
  # container (see _lower_block's no-init single-container branch).
  for my $bare (sort keys %spanning) {
    # Per-declaration instances (M-B), like the scalar loop: every top-level
    # single-container decl of the bare name, each with its OWN segment — the
    # bare-keyed %decl_seg could point at a sibling `my $x` scalar's segment.
    # Uniqueness is per CANON (sigil-exact): a sibling `my $x` is a different
    # variable and a different CL symbol — it neither poisons the identity
    # defvar nor is touched by the sigil-aware rewrite, so it must not block
    # promoting @x/%x.  Nested same-canon shadows DO block (canon_decl_count
    # counts them): a nested `let @x` under the defvar'd special is the
    # poison the file-uniqueness rule exists to prevent.
    my @cinst;
    for my $i (0 .. $#$segments) {
      my $top = $segments->[$i]{stmts};
      for my $cd (@{ $sf[$i]{container_decl}{$bare} || [] }) {
        push @cinst, [$i, $cd] if grep { $top->[$_] == $cd->[0] } 0 .. $#$top;
      }
    }
    for my $ci (@cinst) {
    my ($di, $cd) = @$ci;
    my ($decl, $csym) = @$cd;              # $csym e.g. '%methods' / '@list'
    next unless ($f->{canon_decl_count}{$csym} // 0) == 1;   # sole binding of THIS canon
    my $hi = _blk_extent($segments, $di);
    # Canon-exact span test (%spanning is bare-keyed TEXT — a sibling `$x`
    # in a later segment must not promote an un-spanning @x): promote only
    # when a later extent segment really uses THIS container (->symbol
    # resolves $x[i]/@x{…} to it; $#x via ArrayIndex).  Canon-unique
    # file-wide, so any such use is this variable, shadows impossible.
    my $spans = 0;
  SPANSCAN:
    for my $j ($di + 1 .. $hi) {
      for my $stmt (@{ $segments->[$j]{stmts} }) {
        next unless ref $stmt && $stmt->isa('PPI::Node');
        last SPANSCAN if $spans =
          grep { $_->symbol eq $csym } @{ $stmt->find('PPI::Token::Symbol') || [] };
        last SPANSCAN if $spans = ($csym =~ /^\@/)
          && grep { $_->content eq '$#' . $bare } @{ $stmt->find('PPI::Token::ArrayIndex') || [] };
        # Interp-ONLY spans count too ("h:[@h]" in a later package was the
        # container's sole cross-segment use — s305, task #84): the same
        # sigil-aware detector _check_my_spanning uses.
        my %ihit;
        _interp_canon($stmt, { $csym => 1 }, { $bare => [$csym] }, \%ihit);
        last SPANSCAN if $spans = $ihit{$csym};
      }
    }
    next unless $spans;
    # Interpolated uses in a NON-declaring package are rewritten to the
    # package-qualified name by the fixer below (same M-A fixer the scalar
    # loop carries) — no refusal needed; same-package interp resolves via
    # the identity name untouched.
    next if $alltxt =~ /[\$\@\%]\{\s*\Q$bare\E\s*\}/;   # ${x}/@{x}/%{x} deref-block
    next if $segments->[$di]{blockform};
    my $stmts = $segments->[$di]{stmts};
    next if $self->{_file_lex_renamed}{$csym};

    # Identity-unmangled: the decl stays `my %methods` and defvar-lowers via
    # _file_lex_renamed; same-package uses already resolve to that cell, so
    # only later segments need the package-qualified form (harmless where the
    # package already matches).
    my $qname = $segments->[$di]{pkg} . '::' . $bare;
    my $qfix  = _interp_fixer($csym, $qname);
    for my $j ($di + 1 .. $hi) {
      # Interp text needs the qualified name only where the reading package
      # DIFFERS from the declaring one (same package: the identity name
      # already resolves to the defvar'd cell — leave text byte-untouched,
      # like the scalar identity path).  Canon-unique file-wide, so no
      # shadow skip is needed.
      my $cross = $segments->[$j]{pkg} ne $segments->[$di]{pkg};
      for my $stmt (@{ $segments->[$j]{stmts} }) {
        next unless ref $stmt && $stmt->isa('PPI::Node');
        for my $s (@{ $stmt->find('PPI::Token::Symbol') || [] }) {
          # Canon-exact: only uses of THIS container — a sibling %x beside a
          # promoted @x is a different, unpromoted variable.
          next unless $s->symbol eq $csym;
          (my $c = $s->content) =~ s/^([\$\@\%])\Q$bare\E\b/$1 . $qname/e;
          $s->set_content($c);
        }
        if ($csym =~ /^\@/) {   # $#x belongs to the array only
          for my $ai (@{ $stmt->find('PPI::Token::ArrayIndex') || [] }) {
            my $c = $ai->content;
            $ai->set_content($c) if $c =~ s/^(\$\#)\Q$bare\E\b/$1 . $qname/e;
          }
        }
        if ($cross) {
          _fix_interp_token($_, $qfix) for @{ $stmt->find('PPI::Token') || [] };
        }
      }
    }
    $self->{_file_lex_renamed}{$csym} = 1;
    }   # per-declaration instance
  }
}

# Sigil-aware rename of every use of ONE variable within @$stmts.  $canon is the
# canonical symbol ('$x' | '@a' | '%h'); a Symbol token is rewritten iff its
# ->symbol resolves to $canon, so @a, $a[i] and @a{…} all follow the ONE array
# (and $a / %a — different variables that share the bare name — are left alone,
# because ->symbol distinguishes them by sigil).  Each token KEEPS its own
# leading sigil; only the NAME becomes $newbare.  For an array the $#a length
# token (a PPI::Token::ArrayIndex, not a Symbol) is rewritten too.  This is the
# ONE rewrite primitive shared by the scalar/container promotion passes — the
# thing that must be sigil-correct, so it lives in a single place.
# True when $elem is (transitively) inside $anc.
sub _elem_within {
  my ($elem, $anc) = @_;
  my $p = $elem->parent;
  while ($p) { return 1 if $p == $anc; $p = $p->parent }
  return 0;
}

# Nearest enclosing Structure::Block of $elem (its lexical scope), or undef when
# $elem sits at segment top level (scope = the whole segment).
sub _enclosing_block {
  my ($elem) = @_;
  my $p = $elem->parent;
  while ($p) { return $p if $p->isa('PPI::Structure::Block'); $p = $p->parent }
  return undef;
}

# Sigil-aware rename of every use of ONE variable.  When $within (a block node)
# is given, only tokens inside it are rewritten — the variable's lexical extent,
# so a same-name variable in a sibling block is untouched.  When $skip (a
# coderef) is given, Symbol/ArrayIndex tokens it accepts are left alone — the
# promotion pass uses it to keep shadow-scope uses on their original name.
# Interpolation rewriter: uses of the SAME variable inside interpolating
# text must follow a rename.  Backslash-parity guard (an escaped \$x is
# literal text; \\$x interpolates).  Sigil-aware per canon:
#   $x  → `$x` not followed by [ / { (those are @x/%x ELEMENT interpolations
#         — different variables);
#   @a  → `@a` (join) and `@a[…]` (slice) but not `@a{…}` (%a slice);
#         `$a[` (element — immediate `[`: `$a [` does not interpolate as an
#         element, and `$a->[` is a deref of scalar $a); `$#a`;
#   %h  → `$h{` (element) and `@h{` (slice); bare %h never interpolates.
# The `${x}` deref-block form never reaches here (every caller's blocker
# refuses it first — it is invisible to these regexes).
# Returns a closure over ONE text argument; truthy result = text changed.
sub _interp_fixer {
  my ($canon, $newbare) = @_;
  my $sigil = substr($canon, 0, 1);
  (my $bare = $canon) =~ s/^[\$\@\%]//;
  # Each arm rewrites the plain spelling AND the braced-interpolation
  # spelling ("${x}" / "@{x}[…]" / "${x}{k}" — E4.1 M2, s353): braces are
  # kept in the output so adjacency stays unambiguous.  The `(?:^|[^\\])`
  # prefix skips escaped sigils, as before.
  # `(?!::)` on the UNBRACED arms: "$x::y" interpolates the qualified global
  # $x::y, not $x followed by the text "::y", so a rename of $x must not
  # reach into it (the braced form "${x}::y" DOES mean $x then text, which
  # is why those arms carry no such guard).
  return
    $sigil eq '$' ? sub {
      my $n = 0;
      $n += $_[0] =~ s/((?:^|[^\\])(?:\\\\)*)\$\Q$bare\E\b(?![\[\{])(?!::)/$1\$$newbare/g;
      $n += $_[0] =~ s/((?:^|[^\\])(?:\\\\)*)\$\{\s*\Q$bare\E\s*\}(?![\[\{])/$1\${$newbare}/g;
      return $n;
    }
  : $sigil eq '@' ? sub {
      my $n = 0;
      $n += $_[0] =~ s/((?:^|[^\\])(?:\\\\)*)\@\Q$bare\E\b(?!\{)(?!::)/$1\@$newbare/g;
      $n += $_[0] =~ s/((?:^|[^\\])(?:\\\\)*)\$\Q$bare\E(?=\[)/$1\$$newbare/g;
      $n += $_[0] =~ s/((?:^|[^\\])(?:\\\\)*)\$\#\Q$bare\E\b(?!::)/$1\$#$newbare/g;
      $n += $_[0] =~ s/((?:^|[^\\])(?:\\\\)*)\@\{\s*\Q$bare\E\s*\}(?!\{)/$1\@{$newbare}/g;
      $n += $_[0] =~ s/((?:^|[^\\])(?:\\\\)*)\$\{\s*\Q$bare\E\s*\}(?=\[)/$1\${$newbare}/g;
      $n += $_[0] =~ s/((?:^|[^\\])(?:\\\\)*)\$\#\{\s*\Q$bare\E\s*\}/$1\$#{$newbare}/g;
      return $n;
    }
  : sub {
      my $n = 0;
      $n += $_[0] =~ s/((?:^|[^\\])(?:\\\\)*)([\$\@])\Q$bare\E(?=\{)/$1$2$newbare/g;
      $n += $_[0] =~ s/((?:^|[^\\])(?:\\\\)*)([\$\@])\{\s*\Q$bare\E\s*\}(?=\{)/$1$2\{$newbare\}/g;
      return $n;
    };
}

# Could this token's text possibly BE interpolating text?  A cheap superset of
# the exact class list in _fix_interp_token (four isa calls, no tree walk), so
# a caller can drop the ~95% of tokens that are whitespace/operators/words
# before doing anything expensive to them.
sub _interp_token_candidate {
  my ($t) = @_;
  return $t->isa('PPI::Token::Quote')
      || $t->isa('PPI::Token::QuoteLike')
      || $t->isa('PPI::Token::Regexp')
      || $t->isa('PPI::Token::HereDoc');
}

# Apply an _interp_fixer closure to ONE token, iff it is interpolating text
# (double/qq/backtick/qx/regex/readline, or an interpolating heredoc's body).
#
# $skip (optional, same predicate as the symbol loop's) is consulted ONLY once
# the token is known to be a real rewrite candidate — the fixer has already
# matched the name.  That ordering is load-bearing for compile time, not just
# tidiness: $skip is position-based and walks the parent chain and its
# preceding siblings, so asking it about every token in the file is quadratic
# (#184 — it cost pack.t 5.8 s → 74 s of transpile when s316b added it).
sub _fix_interp_token {
  my ($t, $fix, $skip) = @_;
  if ($t->isa('PPI::Token::HereDoc')) {
    return if Pl::PExpr::TokenUtils::heredoc_is_raw($t);  # #301: THE shared predicate
    my $lines = $t->{_heredoc} || [];
    my @new = @$lines;                       # rewrite a copy, so $skip can veto
    my $hit = 0;
    $hit += ($fix->($_) || 0) for @new;
    return unless $hit;
    return if $skip && $skip->($t);
    @$lines = @new;
  } elsif ($t->isa('PPI::Token::Quote::Double')
        || $t->isa('PPI::Token::Quote::Interpolate')
        || $t->isa('PPI::Token::QuoteLike::Backtick')
        || $t->isa('PPI::Token::QuoteLike::Command')
        || $t->isa('PPI::Token::QuoteLike::Regexp')
        || $t->isa('PPI::Token::QuoteLike::Readline')
        || $t->isa('PPI::Token::Regexp::Match')
        || $t->isa('PPI::Token::Regexp::Substitute')) {
    my $c = $t->content;
    return unless $fix->($c);
    return if $skip && $skip->($t);
    $t->set_content($c);
  }
  return;
}

sub _rewrite_var_uses {
  my ($self, $stmts, $canon, $newbare, $within, $skip) = @_;
  my $sigil = substr($canon, 0, 1);
  (my $bare = $canon) =~ s/^[\$\@\%]//;
  my $interp_fix = _interp_fixer($canon, $newbare);
  for my $stmt (@$stmts) {
    next unless ref $stmt && $stmt->isa('PPI::Node');
    for my $s (@{ $stmt->find('PPI::Token::Symbol') || [] }) {
      next unless $s->symbol eq $canon;
      next if $within && !_elem_within($s, $within);
      next if $skip && $skip->($s);
      (my $c = $s->content) =~ s/^([\$\@\%])\Q$bare\E\b/$1$newbare/;
      $s->set_content($c);
    }
    for my $t (@{ $stmt->find('PPI::Token') || [] }) {
      # Interpolated text obeys the SAME scoping as a symbol use: a string
      # inside a shadow's scope names the shadow, not the variable being
      # renamed (s316).  $skip is position-based (_ref_shadowed climbs the
      # parents of any node, _symbol_is_declarator answers 0 for a string),
      # so the one predicate serves both loops — before this, the interp
      # rewrite was scope-blind, which is why _promote_captured had to
      # refuse the whole promotion whenever an interpolated use met a shadow.
      # It is handed DOWN rather than applied here so that it is asked only
      # about tokens the fixer actually matched: the symbol loop above filters
      # on `eq $canon` before calling it, and this loop needs the same
      # discipline — a per-token $skip call is a tree walk per token (#184).
      next unless _interp_token_candidate($t);
      next if $within && !_elem_within($t, $within);
      _fix_interp_token($t, $interp_fix, $skip);
    }
    next unless $sigil eq '@';
    for my $ai (@{ $stmt->find('PPI::Token::ArrayIndex') || [] }) {
      next if $within && !_elem_within($ai, $within);
      next if $skip && $skip->($ai);
      my $c = $ai->content;
      $ai->set_content($c) if $c =~ s/^(\$\#)\Q$bare\E\b/$1$newbare/;
    }
  }
  return;
}

# W5: file lexicals captured by named subs.  A named sub hoists into the
# definitions bucket OUTSIDE the lexical `let`s that bind file `my`-vars, so a
# sub body that reads such a var would compile a free (→ unbound) symbol —
# today that gates the whole file to v1 (_check_sub_captures / _hoist_nested_sub).
# When the capture meets the conservative preconditions below, rewrite the
# lexical to a fresh package-level name and lower it as a defvar'd box (the
# `our` shape — no let), so both the hoisted sub and the in-place code share
# the single cell.  This is exactly what v1 does (it defvar's file lexicals);
# the fresh NAME additionally avoids proclaiming a common symbol special
# file-wide, which would poison a true `let $x` elsewhere in the file.
#
# Subset (anything outside it keeps the gate → whole-file v1; the full
# per-candidate rules live in _promote_captured):
#   - the decl is the sole HARD (same-level) my/state declaration of the bare
#     name in its extent — deeper-nested re-decls are distinct shadows whose
#     scopes the rewrite skips (M-C);
#   - scalars: the name is never used in array/hash family form (@x, %x, $#x,
#     $x[…], $x{…}) — one bare name would denote >1 variable; nobody uses the
#     deref-block form (${x}), which token rewrites can't reach;
#   - the name is referenced inside some named sub body AFTER the decl.
sub _rename_captured_file_lexicals {
  my ($self, $seg) = @_;
  my $stmts = $seg->{stmts};

  # Named subs (with a body) anywhere in the segment — Perl subs are
  # package-global regardless of block nesting.  BEGIN/END/… Scheduled
  # blocks count as capturers too (s295c): their p-BEGIN forms hoist to the
  # section's compile-phase position OUTSIDE the runtime `let`s (via
  # _sched_defs), so a lexical they reference needs the same package-cell
  # promotion as one captured by a named sub — the classic
  # `my $x; BEGIN { $x = … }` idiom (closure.t newsub block).
  my @subs;
  for my $child (@$stmts) {
    push @subs, $child if $child->isa('PPI::Statement::Sub');
    push @subs, @{ $child->find('PPI::Statement::Sub') || [] };
  }
  @subs = grep { $_->block
                 && ($_->isa('PPI::Statement::Scheduled') || $_->name) } @subs;

  # Oversized-extent flattening (v1's defvar model, size-triggered): a
  # segment-top-level `my` nests the WHOLE segment remainder in one `let`,
  # and SBCL compiles that as a single function — past ~60k chars of source
  # remainder the register allocator's memory use grows superlinearly and
  # exhausts the default 1 GB heap even under the notinline sandwich
  # (pack.t: 63k source → one 162k-char form → compiler OOM).  Any top-level
  # decl whose post-decl runtime remainder (hoisted subs excluded — they
  # leave the run bucket) exceeds $RUN_NEST_MAX MUST therefore be promoted
  # to a defvar cell exactly like a captured lexical; a decl the promotion
  # machinery refuses dies → v1 (never a silent giant form; the assembly
  # gate on $RUN_FORM_MAX is the backstop).
  my %forced = map { (refaddr($_) => 1) } $self->_oversized_top_decls($stmts);
  return unless @subs || %forced;

  # Tally declarations (my/state, by bare name) and disqualify any bare name
  # ever used in array/hash family form.
  my $f = {};
  $self->_scan_lex_facts($stmts, $f);
  my %scalar_decl    = %{ $f->{scalar_decl}    };
  my %container_decl = %{ $f->{container_decl} };
  my %mlist_decl     = %{ $f->{mlist_decl}     };

  # ONE promotion mechanism for scalars and containers, PER-DECLARATION and
  # EXTENT-scoped.  The extent of a `my` is its nearest enclosing block (or the
  # whole segment at top level); a same-name `my` in a DIFFERENT block is a
  # distinct variable.  So a candidate is promoted iff, WITHIN ITS OWN EXTENT, it
  # is the sole declaration of the bare name and is captured by a named sub in
  # that extent — and the rewrite is confined to that extent (block-scoped), so
  # a sibling-block same-name variable and a post-block package global of the
  # same name are never touched.  This is what makes the block-local static-var
  # idiom (`{ my $n; sub inc{$n++} } { my $n; sub dec{$n--} }`) safe.  Scalars
  # first, then list decls, then containers, in bare-sorted /
  # source order → deterministic __file__N numbering (matters for the cache
  # key).  Scalars refuse extent-scoped FAMILY use (@x/%x/$#x in the same extent
  # — one bare name would denote >1 variable); interpolated uses follow via
  # _rewrite_var_uses's interpolation rewriter — containers too ("@a" /
  # "$a[i]" element interpolations are rewritten sigil-aware; M-A).
  # sorted keys: hash order is per-process random — unsorted made cached
  # transpiles churn.
  my $promote = sub {
    my ($decl, $canon, $bare) = @_;
    my $force = $forced{ refaddr($decl) };
    my $ok = $self->_promote_captured($stmts, \@subs, $decl, $canon, $bare,
                                      $force);
    die "Parser2 TODO: oversized top-level my extent: $canon not promotable\n"
      if $force && !$ok;
  };
  for my $bare (sort keys %scalar_decl) {
    next if $self->{_file_lex_renamed}{"\$$bare"};          # already promoted by the spanning pass
    for my $decl (@{ $scalar_decl{$bare} }) {
      $promote->($decl, "\$$bare", $bare);
    }
  }
  for my $bare (sort keys %mlist_decl) {
    for my $md (@{ $mlist_decl{$bare} }) {
      my ($decl, $msym) = @$md;
      next if $self->{_file_lex_renamed}{$msym};            # already promoted by the spanning pass
      $promote->($decl, $msym, $bare);
    }
  }
  for my $bare (sort keys %container_decl) {
    for my $cd (@{ $container_decl{$bare} }) {
      my ($decl, $csym) = @$cd;
      next if $self->{_file_lex_renamed}{$csym};            # already promoted by the spanning pass
      $promote->($decl, $csym, $bare);
    }
  }
}

# The segment-top-level my/state declarations whose post-decl runtime
# remainder (statement source length, minus hoisted named-sub bodies)
# exceeds $RUN_NEST_MAX — each such decl would nest an oversized `let`.
# Returns the decl statement NODES (callers key sets by refaddr — PPI
# stringification is overloaded to content and must not be a hash key).
# Emitted CL runs 2.2–3.2x the statement source across the corpus, so the
# source-side trigger must sit at ~$RUN_FORM_MAX / 3.2 for the flattening to
# actually keep every emitted form under the gate (pack.t s289: a 37k source
# remainder emitted a 96k form).
our $RUN_NEST_MAX = 20_000;   # chars of post-decl source remainder
sub _oversized_top_decls {
  my ($self, $stmts) = @_;
  my @only = grep { ref $_ && $_->isa('PPI::Statement') } @$stmts;
  # Runtime source weight per top-level statement: named-sub definitions
  # hoist out of the run bucket entirely; subs nested deeper hoist too.
  my @w;
  for my $st (@only) {
    if ($st->isa('PPI::Statement::Sub') && !$st->isa('PPI::Statement::Scheduled')) {
      push @w, 0;
      next;
    }
    my $len = length($st->content);
    for my $sub (@{ $st->find('PPI::Statement::Sub') || [] }) {
      $len -= length($sub->content)
        unless $sub->isa('PPI::Statement::Scheduled');
    }
    push @w, $len;
  }
  my @tail;   # tail[i] = sum of w[i+1 ..]
  my $acc = 0;
  for my $i (reverse 0 .. $#only) { $tail[$i] = $acc; $acc += $w[$i] }
  my @forced;
  for my $i (0 .. $#only) {
    next unless $only[$i]->isa('PPI::Statement::Variable');
    my $kw = $only[$i]->schild(0);
    next unless $kw && $kw->content =~ /^(?:my|state)$/;
    push @forced, $only[$i] if $tail[$i] > $RUN_NEST_MAX;
  }
  return @forced;
}

# By-construction backstop for the compiler-heap OOM class: a single emitted
# top-level runtime form past $RUN_FORM_MAX chars is not loadable under the
# standard 1 GB heap (SB-REGALLOC grows superlinearly; pack.t's 162k-char
# form OOMed, the corpus' largest passing form is ~55k).  Refusing beats
# emitting a form that crashes SBCL at load.
#
# RULED REFUSAL (F6/#230, fable-answers-s346.md §2.3; rephrased at the E4.1
# flip, #242).  $RUN_FORM_MAX is never raised.  The one measured event is a
# torture-scale generated source, where one honest loud row is the accepted
# outcome — a pre-flip effort to compile an arbitrarily huge single form is
# not required by any target.
# docs/not-supported.md: 'A single generated top-level form above 64k chars'.
our $RUN_FORM_MAX = 64_000;
sub _gate_oversized_run_form {
  my ($self, $text, $size) = @_;
  # $size: layout-invariant (whitespace-collapsed) length — see the caller.
  $size //= length($text);
  if ($size > $RUN_FORM_MAX) {
    (my $head = substr($text, 0, 120)) =~ s/\s+/ /g;
    die "PCL: unsupported: a single generated top-level form of $size chars"
      . " exceeds the $RUN_FORM_MAX-char limit"
      . " (it would exhaust the SBCL compiler heap at load): $head\n";
  }
  return $text;
}

# Refusal diagnostic for the capture-promotion path (CAPREFUSE, the analogue of
# the span loop's SPANREFUSE).  Always returns 1 so it can gate a `next if`.
sub _caprefuse {
  my ($canon, $why) = @_;
  warn "CAPREFUSE $canon: $why\n" if $ENV{PCL_SPAN_DEBUG};
  return 1;
}

# Promote ONE captured lexical declaration ($decl, canonical symbol $canon —
# '$x' | '@a' | '%h') to a defvar'd cell, IF it is the sole HARD declaration of
# the bare name within its lexical extent and is captured by a named sub AFTER
# it in that extent.  Perl visibility is honoured positionally: uses BEFORE the
# declaration (earlier statements, the decl's own RHS) denote the OUTER
# variable — package global or an outer lexical — and are left untouched; only
# the declarator symbol and post-declaration uses are renamed.  A same-name
# re-declaration NESTED in a deeper block or named sub is a DISTINCT shadowing
# variable (M-C, the span loop's M3 model): it does not block the rename, and
# the rewrite skips its scope (_symbol_is_declarator + _ref_shadowed).  No-op
# when the guards fail (→ the file keeps its capture gate → v1).
sub _promote_captured {
  my ($self, $stmts, $subs, $decl, $canon, $bare, $force) = @_;
  my $sig    = substr($canon, 0, 1);
  my $extent = _enclosing_block($decl);                     # block, or undef = segment
  my $estmts = $extent ? [ grep { $_->isa('PPI::Statement') } $extent->schildren ]
                       : $stmts;
  my ($di) = grep { ref $estmts->[$_] && $estmts->[$_] == $decl } 0 .. $#$estmts;
  return if !defined $di && _caprefuse($canon, 'decl not top-level in its extent');
  my @post = @{$estmts}[$di + 1 .. $#$estmts];
  # The promotion is needed only when a named sub AFTER the decl captures it —
  # a sub textually before the decl does not close over it in Perl.
  my @psubs;
  for my $sub (@$subs) {
    push @psubs, $sub
      if grep { ref $_ && ($sub == $_ || _elem_within($sub, $_)) } @post;
  }
  # A $force decl (oversized top-level extent — see _oversized_top_decls)
  # must be promoted regardless of capture; every other safety rule below
  # still applies, and the caller gates the file when we return falsy.
  return if !$force && !$self->_captured_in_subs(\@psubs, $canon, $extent)
    && _caprefuse($canon, 'not captured by a named sub after the decl');
  # File-unique name declared at segment top level: promote under its OWN
  # name (the span pass's identity-unmangle rule).  The defvar cannot poison
  # a sibling `let` (no other declaration of the name file-wide), and keeping
  # the name neutralises every text hazard at once — interpolation, ${x}
  # deref-blocks and string eval all keep resolving to the same symbol
  # (exactly v1's defvar-under-original-name model).  Block-extent decls stay
  # on the mangled path: an identity defvar would outlive the block and
  # capture post-block package-global uses of the name.
  if (!$extent && ($self->{_file_decl_count}{$bare} // 0) == 1) {
    $self->{_file_lex_renamed}{$canon} = 1;
    return 1;
  }
  # Sole HARD declaration of the bare name in this extent: only a same-level
  # (extent top-level) re-decl, or one the shadow machinery cannot scope (a
  # Compound head like `foreach my $x`), blocks the rename.  For a SCALAR
  # canon, count ALL sigils — that also refuses the ambiguous case where the
  # one name denotes >1 variable at the same level ($x beside @x), where the
  # interp rewrite for `$x` text cannot be trusted.  A CONTAINER canon counts
  # only its own sigil: its rewrite shapes (`%x`, `$x{`, `@x{` resp. `@x`,
  # `$x[`, `$#x`) are syntactically disjoint from a sibling scalar/other-
  # family variable (token rewrites key on ->symbol), so `my $x` beside
  # `my %x` does not block promoting %x (array.t bug-70171 block).
  my $hard = $self->_hard_decl_count($estmts, $bare, $sig eq '$' ? undef : $sig);
  return if $hard != 1 && _caprefuse($canon, "hard-decls=$hard in extent");
  # (#254 A-iv, s365: the blanket "family use (@x/%x/$#x) in extent" refusal
  # for a SCALAR promotion is GONE.  It predated the sigil-exact rewriter and
  # duplicated its knowledge as a veto: `$x` and `@x` are two variables, and
  # every rewrite this promotion performs already keys on the CANONICAL symbol
  # — the Symbol loop tests `$s->symbol eq '$x'`, which PPI answers `@x` for
  # the `$x[0]` element spelling; the ArrayIndex loop runs for `@`-canons only;
  # and the interp fixer's scalar arm carries `(?![\[\{])`, so `"$x[0]"` /
  # `"$x{k}"` in a string are left alone too.  Nothing in the rewrite was
  # relying on the veto, so keeping it only refused correct promotions — most
  # of them where the container is a DIFFERENT variable declared inside a sub
  # (`my ($name, $ref, @attrs) = @_;` beside a file `my $attrs`, op/attrproto.t).
  # The genuinely unreachable text shape, `${x}`, keeps its own refusal below.
  # Same sigil-exactness argument the CONTAINER path has used since array.t's
  # bug-70171 block, applied to the scalar side.)
  # (s316: the interp rewrite is shadow-aware — _rewrite_var_uses runs the
  # same $skip predicate over interpolated tokens as over symbols — so an
  # interpolated use alongside a shadow no longer refuses the promotion.
  # The `${x}` deref-block refusal below still stands: that shape is a
  # *text* form the token rewrites cannot reach at all, shadows or not.)
  my $etxt = $extent ? $extent->content : join("\n", map { $_->content } @$stmts);
  return if $etxt =~ /[\$\@\%]\{\s*\Q$bare\E\s*\}/          # ${x}/@{x}/%{x} deref-block → can't rewrite
    && _caprefuse($canon, '${x} deref-block');
  # M-F: a promoted SCALAR cell becomes eval-visible when its renamed decl
  # LOWERS — _reg_eval_capture at the defvar branches emits the alias call
  # (p-alias-eval-cell, ir-spec §9.1) at the decl's run position, and string
  # eval — literal AND dynamic — then reaches the cell by its original name
  # through the lookup's global fall-through.  So no eval refusal is needed,
  # EXCEPT for: containers (only scalar cells are aliased/alist-carried) and
  # the enclosing-outer-lexical shape (an outer `my $x` around the extent —
  # the site alist's let-bound pair precedes the global in lookup order, so
  # the deeper cell could never win; aliasing skips it, so keep the
  # refusal).  For those, a renamed cell stays invisible to string eval's
  # by-name lexical capture — refuse when a post-decl string eval could name
  # it.  eval BLOCKS are fine; a literal eval that never mentions the name
  # is fine.
  my $eval_pair = $sig eq '$' && !$self->_enclosing_lex_decl($extent, $bare);
  my $post_eval = 0;
  my $site_pair = 0;
  for my $st (@post) {
    next unless ref $st && $st->isa('PPI::Node');
    for my $w (@{ $st->find(sub { $_[1]->isa('PPI::Token::Word')
                                  && $_[1]->content eq 'eval' }) || [] }) {
      my $nx = $w->snext_sibling;
      next if !$nx || $nx->isa('PPI::Structure::Block');    # eval { } — fine
      if ($eval_pair) { $post_eval = 1; next }
      # E4.1 M5 (s353): the enclosing-outer-lexical SCALAR shape — the
      # static-variable idiom `my $x; { my $x; sub f { $x } eval q{$x} }` —
      # no longer refuses.  The promoted cell reaches these evals through a
      # PER-SITE capture-alist pair under the original name: registered
      # when the renamed decl lowers (see the _file_lex_renamed my-branch),
      # block-scoped for free by the existing _let_bound_vars save/restore,
      # and emitted innermost-first by the alist builder's __file__N strip
      # rule — so inside the block it precedes the outer let-bound pair,
      # and after the block it is gone.  Containers keep the refusal below:
      # the alist carries scalar cells only.
      if ($sig eq '$') { $site_pair = 1; next }
      my @q = $nx->isa('PPI::Token::Quote') ? ($nx)
            : $nx->isa('PPI::Structure::List')
              ? @{ $nx->find('PPI::Token::Quote') || [] } : ();
      return if !@q && _caprefuse($canon, 'dynamic string eval after decl');
      return if (grep { $_->content =~ /[\$\@\%]\s*\{?\s*\Q$bare\E\b/ } @q)
        && _caprefuse($canon, 'string eval names the lexical');
    }
  }
  my ($dsym) = grep { $_->content eq $canon }
               @{ $decl->find('PPI::Token::Symbol') || [] };
  return if !$dsym && _caprefuse($canon, 'decl symbol not found');
  my $newbare = $bare . '__file__' . $self->{_file_lex_counter}++;
  my $sp = $extent
    // (map { $_->parent } grep { ref $_ && $_->isa('PPI::Node') } @$stmts)[0];
  my $skip = sub {
    my ($s) = @_;
    return 1 if $self->_symbol_is_declarator($s);
    return 1 if $self->_ref_shadowed($s, $canon, $estmts, $sp);
    return 0;
  };
  # Declarator first (its RHS keeps the original name — it reads the OUTER
  # variable), then every post-declaration use, skipping shadow scopes.
  $self->_rename_decl_within($decl, $dsym, $sig . $newbare);
  $self->_rewrite_var_uses(\@post, $canon, $newbare, $extent, $skip);
  $self->{_file_lex_renamed}{ $sig . $newbare } = 1;             # drives the defvar lowering
  # M-F backstop: the eval refusals above were waived on the promise that
  # _reg_eval_capture runs when this decl lowers to its defvar.  If the decl
  # instead lowers inside a v1-seam expression (do-block, anon-sub body, …)
  # the defvar branch never runs and the promise breaks — the end-of-parse
  # check dies (→ v1) on any name still pending.  Only armed when a post-decl
  # string eval actually exists.
  $self->{_pending_eval_caps}{ $sig . $newbare } = 1 if $eval_pair && $post_eval;
  # M5 (s353): the enclosing-outer shape's waiver carries the same promise —
  # the per-site pair is registered only by the native decl lowering, which
  # also clears the pending flag.
  if ($site_pair) {
    $self->{_eval_block_cells}{ $sig . $newbare } = 1;
    $self->{_pending_eval_caps}{ $sig . $newbare } = 1;
  }
  warn "CAPPROMOTE $canon -> $sig$newbare (extent="
    . ($extent ? "block@" . $extent->location->[0] : 'segment')
    . ", eval_pair=$eval_pair, post_eval=$post_eval)\n" if $ENV{PCL_SPAN_DEBUG};
  return 1;
}

# True when $canon (sigil-aware, via ->symbol) is used inside a NAMED sub whose
# body lies within $extent (any sub in the segment when $extent is undef).  A
# block-scoped lexical can only be captured by a sub textually inside its block.
sub _captured_in_subs {
  my ($self, $subs, $canon, $extent) = @_;
  (my $bare = $canon) =~ s/^[\$\@\%]//;
  for my $sub (@$subs) {
    next if $extent && !_elem_within($sub, $extent);
    # ONE capture test, the same one the GATE uses (`_check_sub_captures`):
    # Symbol uses, `$#name`, and quoted/heredoc/regex mentions — the last
    # because string eval reaches a lexical by NAME (`eval '$yyy'`), M-F.
    #
    # This function used to run its own Symbol and ArrayIndex loops FIRST,
    # a shadow-BLIND duplicate of what _block_captures_name does two lines
    # later: `$s->symbol eq $canon` counted a sub's OWN `my $x` uses as a
    # capture of a same-named file lexical.  For a `my` the block lowers as
    # a let that shadows the promoted cell, so the extra promotion was
    # merely wasteful — but for a `my` EMBEDDED in another statement
    # (`… if my $x = …`, `++my $x->{k}`) the embedded-my let is skipped for
    # promoted names, and the sub then WROTE THE FILE LEXICAL: state leaked
    # across calls and the outer variable was clobbered (#265, silent wrong).
    # Deleting the blind loops makes the promoter and the gate agree, which
    # is the standing rule (detector and rewriter share one resolver).
    return 1 if $self->_block_captures_name($sub->block, $bare, { $canon => 1 });
  }
  return 0;
}

# M-F: any my/state declaration of $bare at a scope ENCLOSING $extent — an
# outer lexical the promoted cell would nest inside.  The flat eval-capture
# alist puts let-bound pairs first, so a cell nested inside the outer
# lexical's scope could never win the by-name lookup; the caller then keeps
# the eval refusals for this (rare) shape instead of registering a pair.
# Only DIRECT children of enclosing blocks/document count (same-level decls);
# a decl inside a sibling block is a different, non-overlapping scope.
# An enclosing decl ALREADY RENAMED by an earlier pass instance (its content
# is now `$x__file__0`/`$x__lex__2`/…) still counts: it declares the same
# bare name in an enclosing scope, and the outer/inner precedence problem is
# identical.  Without the suffix-strip, promotion order decides whether the
# refusal fires (outer-promotes-first hid the decl from this scan — the encl
# probe's silent "2 2" miscompile, s295).
sub _enclosing_lex_decl {
  my ($self, $extent, $bare) = @_;
  return 0 unless $extent;
  for (my $p = $extent->parent; $p; $p = $p->parent) {
    next unless $p->isa('PPI::Structure::Block') || $p->isa('PPI::Document');
    for my $st (grep { $_->isa('PPI::Statement::Variable') } $p->schildren) {
      my @k = _strip_semi($st->schildren);
      next unless @k >= 2 && $k[0]->isa('PPI::Token::Word')
        && $k[0]->content =~ /^(?:my|state)$/;
      my @syms = $k[1]->isa('PPI::Token::Symbol') ? ($k[1])
               : $k[1]->isa('PPI::Structure::List')
                 ? (grep { $_->isa('PPI::Token::Symbol') } map { $_->tokens } $k[1])
               : ();
      return 1 if grep {
        (my $n = $_->content) =~ s/__(?:file|lex|shadow|cond)__\d+$//;
        $n eq "\$$bare"
      } @syms;
    }
  }
  return 0;
}

# M-F (s295 ALIAS rule, ir-spec §9.1): make a renamed (mangled) SCALAR cell
# reachable by string eval under its ORIGINAL name — including from code the
# eval transpile itself emitted (a sub defined inside an eval string whose
# nested eval names it), which no codegen-site alist can ever know about.
# Emits (p-alias-eval-cell '$x $x__file__N) for the decl's RUN position: the
# quoted UNQUALIFIED symbol is interned by the reader under the section's
# in-package = the declaring package, so the alias writes the original-name
# global there — the very slot p-eval-lex-lookup's global fall-through (and
# any plain un-renamed defvar'd lexical) uses.  One storage location per
# name, TIME-ORDERED exactly like v1's defvar model: a later same-name
# declaration takes over the name from the moment it executes.  Returns the
# CLForms.
sub _reg_eval_capture {
  my ($self, @vars) = @_;
  my @forms;
  for my $var (@vars) {
    next unless $var =~ /^\$\w+__file__\d+$/;
    (my $orig = $var) =~ s/__file__\d+$//;
    push @forms, ['p-alias-eval-cell', "'$orig", $var];
    delete $self->{_pending_eval_caps}{$var} if $self->{_pending_eval_caps};
  }
  return @forms;
}

# ---- W8.5 shared shadow-rename machinery ----------------------------------
# A `my $x` whose scope is a subtree we can delimit (a fallback block, a
# condition-my's construct) can be renamed to a fresh name so it stops
# colliding with an outer lexical or a package global of the same name.
# Perl visibility rule honoured: uses in the SAME statement after the `my`
# (`my $x = $x`) still see the OUTER variable, so only the declared symbol
# itself and symbols AFTER the declaration statement are renamed.

# A CODE-level brace-deref of the bare name — `${x}` / `@{x}` / `%{x}` /
# `$#{x}` spelled as Cast (or the `$#` Magic) + Block whose sole content is
# the bare word — references the variable through a Word token the symbol
# rewrite never sees, so a rename would split the variable.  The blockers
# refuse on THIS shape only (E4.1 M2, s353); the old whole-content text scan
# also tripped on "${x}" inside string literals, which _interp_fixer now
# rewrites (both live Moo events were that spelling, probed to their source
# lines in Method::Generate::{Constructor,Accessor}).
# `${name}` / `@{name}` / `%{name}` / `$#{name}` written at CODE level is NOT a
# deref of an expression — it is a fancy spelling of `$name` / `@name` /
# `%name` / `$#name`.  PPI spells it Cast + Block-containing-one-Word, so there
# is no Symbol token and every Symbol-driven pass is blind to it: that blindness
# is what made a spanning `${x}` read EMPTY instead of gating (#264).  ONE
# helper answers "which canonical variables does this node mention that way",
# and both the span DETECTOR (_canon_refs_in) and the span RENAMER consume it —
# they must agree about what a use is, which is the whole point.
# Returns [ $word_token, $canon ] pairs; the token is the rename target.
# `${ $ref }` / `${\ …}` / `@{[ … ]}` hold a Symbol or an expression, not a
# lone Word, so they are correctly not claimed.
sub _brace_name_refs {
  my ($node) = @_;
  my @out;
  for my $b (@{ $node->find('PPI::Structure::Block') || [] }) {
    my $prev = $b->sprevious_sibling or next;
    my $sig = $prev->isa('PPI::Token::Cast') ? $prev->content
            : ($prev->isa('PPI::Token::Magic') && $prev->content eq '$#')
              ? '$#' : next;
    my @kids = grep { $_->significant } $b->children;
    next unless @kids == 1 && $kids[0]->isa('PPI::Statement');
    my @t = grep { $_->significant } $kids[0]->children;
    next unless @t == 1 && $t[0]->isa('PPI::Token::Word')
             && $t[0]->content =~ /^\w+$/;      # a qualified name is no lexical
    my $canon = $sig eq '$#' ? '@' . $t[0]->content
              : $sig =~ /^[\$\@\%]$/ ? $sig . $t[0]->content
              : next;                            # `\{name}`, `&{name}`: not ours
    push @out, [ $t[0], $canon ];
  }
  return \@out;
}

sub _has_code_brace_deref {
  my ($root, $bare) = @_;
  return scalar grep { $_->[1] =~ /^[\$\@\%]\Q$bare\E$/ }
                @{ _brace_name_refs($root) };
}

# How many `my`/`state` DECLARATION events of $name (sigil'd) sit anywhere
# under $root — nested ones included: two is a re-shadow.  Counts the
# declaring WORDS, so `my ($x, $x)` is one event, as the two blockers always
# counted it.
sub _count_decls_of {
  my ($root, $name) = @_;
  my %seen_word;
  return scalar grep { $_->[1]->content eq $name && !$seen_word{ $_->[0] }++ }
                _decl_syms_under($root, nested => 1);
}

# Reasons renaming `my $x` within $root is NOT safe; undef when safe.
sub _shadow_rename_blocker {
  my ($self, $root, $sym, $eval_ok, $shadow_ok) = @_;
  my $old = $sym->content;
  return "non-scalar" unless $old =~ /^\$\w+$/;
  (my $bare = $old) =~ s/^\$//;
  # More than one my/state declaring this name in $root → nested re-shadow;
  # a single positional rename would merge the scopes — unless the caller
  # renames SHADOW-AWARE ($shadow_ok, #254 B-ii): _rename_decl_within now
  # leaves an inner declaration's target and its scope alone, so the two
  # variables stay two variables.
  my $decls = _count_decls_of($root, $old);
  return "multiple declarations" if $decls != 1 && !$shadow_ok;
  return "no declaration" if $decls == 0;
  # Interpolated uses ("$x" / /$x/ / "${x}" / heredoc / <$x>) are handled:
  # the rename (_rename_decl_within) rewrites them via _interp_fixer (M-A;
  # braced spellings since M2, s353).  Only a CODE-level brace-deref
  # (`${x}` as Cast+Block — the name lives in a Word token the symbol
  # rewrite never sees) keeps the refusal; the old whole-content text scan
  # also tripped on "${x}" inside string literals (both live Moo events).
  return "brace-deref" if _has_code_brace_deref($root, $bare);
  # String eval captures lexicals BY NAME (session-250 alist) — the eval'd
  # code would look for the original name.  eval-BLOCKS are fine.  $eval_ok
  # waives this (M-F): the seam my-shadow rename produces a LET-BOUND
  # `$x__shadow__N`, which _eval_lexical_alist strips back to the original
  # key (innermost-first), so string eval — literal and dynamic — still
  # reaches the shadow.  state/cond renames produce cells the alist does not
  # carry, so they keep the refusal.
  unless ($eval_ok) {
    for my $w (@{ $root->find(sub { $_[1]->isa('PPI::Token::Word')
                                    && $_[1]->content eq 'eval' }) || [] }) {
      my $nx = $w->snext_sibling;
      return "string eval" unless $nx && $nx->isa('PPI::Structure::Block');
    }
  }
  return undef;
}

# Container (`state @x` / `state %h`) variant of _shadow_rename_blocker: same
# multiple-declaration and brace-deref refusals (keyed on the full sigil'd
# name), plus the classic string-eval refusal.  Interpolated `@x` text is
# handled by the rename's interp fixer (M-A), like scalars.
sub _state_container_blocker {
  my ($self, $root, $sym, $eval_ok) = @_;
  my $old = $sym->content;
  (my $bare = $old) =~ s/^[\@\%]//;
  my $decls = _count_decls_of($root, $old);
  return "multiple declarations" if $decls != 1;
  return "brace-deref" if _has_code_brace_deref($root, $bare);
  # $eval_ok (s299, __shadow__ renames only): the eval capture alist strips
  # the __shadow__N suffix back to the original name sigil-agnostically, so
  # string eval still reaches a container shadow.  state renames produce
  # cells the alist does not carry, so they keep the refusal.
  unless ($eval_ok) {
    for my $w (@{ $root->find(sub { $_[1]->isa('PPI::Token::Word')
                   && $_[1]->content eq 'eval' }) || [] }) {
      my $nx = $w->snext_sibling;
      return "string eval" unless $nx && $nx->isa('PPI::Structure::Block');
    }
  }
  return undef;
}

# ---------------------------------------------------------------------------
# LEXICAL SUBS — `my sub NAME {…}` / `state sub NAME {…}` (#337).
#
# A lexical sub is a LEXICAL: its name is visible from the declaration to the
# end of the enclosing block, two declarations of the same name in different
# scopes are two DIFFERENT subs, and a call (or `\&NAME`) resolves to the one
# whose scope it sits in.  PCL compiles every named sub as a PACKAGE sub, so
# before this pass the second `my sub x` OVERWROTE the first and every
# reference — including one captured in a closure built before it — resolved
# to whichever was defined LAST.  Silent: no warning, no die, a wrong value
# (probed vs perl 5.40.3: `8 3` there, `3 3` here).
#
# The fix is what the _rename_* family already does for `my $x`: give the
# declaration a scope-unique name and rewrite the uses its region owns.  The
# region runs from the DECLARATION — not from the top of the block: a call
# BEFORE it still reaches the package sub (probed) — to the end of the
# enclosing scope, and STOPS at a sibling redeclaration of the same name in
# that scope (the #296-B2 rule).  Nesting needs no separate shadow test: a use
# is claimed by the covering declaration with the LATEST start, which is by
# construction the innermost one in scope there.
#
# DELIBERATELY unchanged (the last four registered in docs/not-supported.md):
#   * `our sub` — that IS a package sub, and it already matches perl.
#   * a body's call to its own name (`my sub rec { rec(…) }`): perl DIES
#     there (a `my sub` is not in scope inside itself), PCL resolves it to
#     the sub — principle 9, PCL is the more permissive one.
#   * a lexical sub reached from a STRING eval: perl finds it in the pad, PCL
#     cannot see into the string.
#   * a fresh closure per loop iteration (`for my $i (…) { my sub g {…} }`) —
#     the hoisted-CV residue of #347's "will not stay shared" family, sized
#     separately; a rename cannot express it.
#
# A lexical sub named after a KEYWORD (`state sub if () { 44 }`, which perl
# allows and t/op/lexsub.t asserts) IS renamed, and that turns `my $x = if if
# if` from a keyword parse into three juxtaposed zero-arg calls the term
# grammar cannot lower — an announced DROP where the keyword parse used to
# emit a zero-argument `(p-if)`, i.e. the very form whose macroexpansion
# error crashes that file.  A counted drop is the better of the two failures
# and it is deliberate; the shape belongs to Option B phase 2's term-grammar
# track (task #374), not here.
sub _rename_lexical_subs {
  my ($self, $doc) = @_;
  my @decls;
  for my $st (@{ $doc->find('PPI::Statement::Sub') || [] }) {
    next if $st->isa('PPI::Statement::Scheduled');
    my $type = $st->type // '';
    next if $type ne 'my' && $type ne 'state';
    my $name = $st->name;
    next if !defined $name || $name !~ /^\w+\z/;      # never qualified
    # `my sub b;` declares the name but defines nothing — it OPENS a region
    # like a bodied declaration, and the `sub b {…}` that fills it in later is
    # renamed by _lexsub_renamable's package-sub arm, because that is what
    # perl does with it.  This is perlsub's own idiom for mutually recursive
    # lexical subs, and leaving the pair alone left BOTH halves package subs,
    # so two scopes clobbered each other exactly as #337's core bug did
    # (probed: `{ my sub c; sub c {"c1"} print c() } { my sub c; sub c {"c2"}
    # print c() }` — perl `c1 c2`, PCL `c2 c2`).  Task #376(a).
    my $scope = _lexsub_scope($st) or next;
    push @decls, { st => $st, name => $name, scope => $scope };
  }
  return if !@decls;

  my %idx;
  my $n = 0;
  $idx{ refaddr $_ } = $n++ for $doc->tokens;
  for my $d (@decls) {
    $d->{start} = $idx{ refaddr( ($d->{st}->tokens)[0] ) };
    $d->{stop}  = $idx{ refaddr( _last_token($d->{scope}) ) };
    $d->{new}   = sprintf '%s__lexsub__%d', $d->{name}, ++$self->{_lexsub_counter};
  }
  # A sibling redeclaration in the SAME scope ends the earlier one's claim
  # (#296-B2).  It ends it AT the declarator, not after the statement: unlike
  # `my $a = "[$a]"`, a sub declaration's own body is the new sub's, which is
  # also what keeps a self-recursive body resolving to itself.
  for my $d (@decls) {
    for my $o (@decls) {
      next if refaddr($o) == refaddr($d) || $o->{name} ne $d->{name};
      next if refaddr($o->{scope}) != refaddr($d->{scope});
      next if $o->{start} <= $d->{start};
      $d->{stop} = $o->{start} - 1 if $o->{start} - 1 < $d->{stop};
    }
  }
  my %by_name;
  push @{ $by_name{ $_->{name} } }, $_ for @decls;

  # The declaration covering a use of $name at token index $i: the one with the
  # LATEST start among those whose region contains it — the innermost in scope.
  my $covering = sub {
    my ($name, $t, $i) = @_;
    my $win;
    for my $d (@{ $by_name{$name} || [] }) {
      next if $i < $d->{start} || $i > $d->{stop};
      next if !_has_ancestor($t, $d->{scope});
      $win = $d if !$win || $d->{start} > $win->{start};
    }
    return $win;
  };

  for my $t ($doc->tokens) {
    my ($name, $sigil) = _lexsub_use_name($t);
    if (defined $name) {
      next if !$by_name{$name};
      my $win = $covering->($name, $t, $idx{ refaddr $t }) or next;
      next if !_lexsub_renamable($t, $win->{st});
      $t->set_content($sigil . _lexsub_spelling($win, $t));
      next;
    }
    next if !_interp_token_candidate($t);
    my $i = $idx{ refaddr $t };
    my %ren;
    for my $nm (keys %by_name) {
      my $win = $covering->($nm, $t, $i) or next;
      $ren{$nm} = _lexsub_spelling($win, $t);
    }
    next if !%ren;
    _fix_interp_token($t, sub { _fix_lexsub_interp($_[0], \%ren) });
  }
  return;
}

# Interpolated CODE — `"@{[ f() ]}"`, `"${\ f() }"`, the same inside a heredoc
# or a regex — is compiled from the STRING's TEXT, not from these tokens, so a
# rename that only rewrote the token stream left the embedded call pointing at
# a package sub that no longer exists (loud: "the function main::pl-f is
# undefined" — found by a probe, this pass caused it).  Rewrite the embedded
# code too.  The spans come from Pl::InterpScan (standing rule §8: new
# interpolation behavior lives there or nowhere), and the code inside a span is
# classified by the SAME predicate as the token stream — by parsing it as what
# it is, Perl, rather than by matching the name in text where it could equally
# be a string, a hash key or a method name.
#
# Signature is _interp_fixer's: rewrite $_[0] in place, return a hit count.
sub _fix_lexsub_interp {
  my $ren = $_[1];                          # $_[0] is rewritten IN PLACE
  return 0 if !grep { $_[0] =~ /\b\Q$_\E\b/ } keys %$ren;
  my @ev = grep { ($_->{form} // '') eq 'expr' && $_->{expr_span} }
           @{ Pl::InterpScan::scan($_[0]) };
  return 0 if !@ev;
  my $hits = 0;
  for my $ev (reverse @ev) {                # right to left: spans stay valid
    my ($s, $e) = @{ $ev->{expr_span} };
    my $new = _rename_lexsub_in_code(substr($_[0], $s, $e - $s), $ren);
    next if !defined $new;
    substr($_[0], $s, $e - $s) = $new;
    $hits++;
  }
  return $hits;
}

# One embedded-code span, renamed.  Returns the new text, or undef when
# nothing in it was a use of a lexical sub.
sub _rename_lexsub_in_code {
  my ($code, $ren) = @_;
  my $mini = eval { PPI::Document->new(\$code) } or return undef;
  my $hit = 0;
  for my $t ($mini->tokens) {
    my ($name, $sigil) = _lexsub_use_name($t);
    next if !defined $name;
    my $new = $ren->{$name} or next;
    next if !_lexsub_renamable($t, undef);
    $t->set_content($sigil . $new);
    $hit = 1;
  }
  return $hit ? $mini->serialize : undef;
}

# The renamed name as THIS token has to spell it.  A lexical sub is scoped to
# the file or block, never to a package — but PCL resolves a bare name in the
# package in effect at the token, so a use (or the definition) written under a
# different `package NAME;` must name the declaring package explicitly, or it
# reaches a sub that does not exist there (probed: `my sub helper {"H"};
# package Other; sub go { helper() }` → perl H, PCL "the function
# Other::pl-helper__lexsub__N is undefined").  The resolver is the one the
# variable-rename family already uses — rule 11: no second package walk.
# Task #376(c).
sub _lexsub_spelling {
  my ($d, $t) = @_;
  my $decl_pkg = _pkg_in_effect_at($d->{st});
  return $d->{new} if $decl_pkg eq _pkg_in_effect_at($t);
  return $decl_pkg . '::' . $d->{new};
}

# The lexical scope a declaration lives in: the nearest enclosing block, or
# the document for a file-level one.
sub _lexsub_scope {
  my ($st) = @_;
  for (my $p = $st->parent; $p; $p = $p->parent) {
    return $p if $p->isa('PPI::Document') || $p->isa('PPI::Structure::Block');
  }
  return undef;
}

sub _has_ancestor {
  my ($t, $node) = @_;
  for (my $p = $t; $p; $p = $p->parent) {
    return 1 if refaddr($p) == refaddr($node);
  }
  return 0;
}

# The sub name a token could be naming, and the sigil to put back: a bare Word
# (`f()`, `\&{…}`-free call, `sort f @l`, the declaration's own name) or an
# unqualified `&f` Symbol (`\&f`, `&f()`, `defined &f`, `goto &f`).
sub _lexsub_use_name {
  my ($t) = @_;
  return ($t->content, '')
    if $t->isa('PPI::Token::Word') && $t->content =~ /^\w+\z/;
  return ($1, '&')
    if $t->isa('PPI::Token::Symbol') && $t->content =~ /^&(\w+)\z/;
  return ();
}

# Is this token in the region really a USE of the lexical sub?  A `&NAME`
# Symbol always is.  A Word is not when it is a method name, a fat-comma or
# bare-subscript hash key, a module name in a `use`/`require`, or the name of
# some OTHER sub declaration — a package `sub f {…}` written inside the
# region still defines the package sub, and a nested `my sub f` is renamed by
# its own declaration, never by this one.
sub _lexsub_renamable {
  my ($t, $decl) = @_;
  return 1 if $t->isa('PPI::Token::Symbol');
  return 0 if _is_method_name_word($t);
  my $stmt = $t->statement;
  return 0 if $stmt && $stmt->isa('PPI::Statement::Include');
  my $nx = _next_sig_token($t);
  return 0 if $nx && $nx->isa('PPI::Token::Operator') && $nx->content eq '=>';
  my $par = $t->parent;
  # `$h{f}` — the Word is the sole content of a subscript's Expression.
  return 0 if $par && $par->isa('PPI::Statement') && $par->parent
           && $par->parent->isa('PPI::Structure::Subscript')
           && !grep { refaddr($_) != refaddr($t) } $par->schildren;
  my $pv = _prev_sig_token($t);
  if ($pv && $pv->isa('PPI::Token::Word')
      && $pv->content =~ /^(?:sub|package|require)\z/) {
    return 1 if $decl && $par && refaddr($par) == refaddr($decl);
    return 0 if $pv->content ne 'sub';      # `package NAME` / `require NAME`
    # A plain `sub NAME …` written INSIDE the region DEFINES THE LEXICAL in
    # perl — that is exactly what makes the forward-declaration idiom work —
    # and no package sub of that name comes into existence.  Probed vs perl
    # 5.40.3: `my sub f {"L"} { package O; sub f {"M"} } print f()` prints M,
    # and `O->can("f")` is false; the same in one package prints M with no
    # main::g.  Task #376(b).  A `my`/`state sub` statement is not this
    # declaration's to rename — its own declaration covers it (the covering
    # rule picks the innermost), and _rename_lexical_subs never reaches here
    # for one, since that token IS its decl.
    return $par && $par->isa('PPI::Statement::Sub')
        && !$par->isa('PPI::Statement::Scheduled')
        && (($par->type // '') eq '') ? 1 : 0;
  }
  return 1;
}

# Rename the declaration $sym (a `my $x` Symbol) and every post-declaration
# scalar use of it within $root to $new — including uses inside interpolating
# text ("$x", /$x/, heredocs, <$fh>; via _interp_fixer, M-A).  Symbols whose
# ->symbol resolves to a container (`$x[0]` → @x, `$x{k}` → %x) are left
# alone — they are element accesses of DIFFERENT variables; the interp fixer
# skips those forms for the same reason.  The positional walk keeps interp
# text BEFORE the decl (and in the decl's own RHS) on the outer variable.
# Every caller's blocker guarantees a single declaration of the name in
# $root, so there is no shadow scope to skip.
sub _rename_decl_within {
  my ($self, $root, $sym, $new, $decl_override) = @_;
  my $old  = $sym->content;
  my $canon = $sym->symbol;
  # $decl is the region evaluated in the OUTER scope, skipped by the rewrite.
  # It is the declaring STATEMENT for an ordinary decl (`my $x = $x` reads the
  # outer $x), but a caller whose ROOT *is* that statement must say which
  # sub-region to skip instead — a `for my $x (LIST)` roots at the Compound,
  # where the default would skip the whole construct (#296).
  my $decl = $decl_override // $sym->statement;
  # A NESTED re-declaration of the same name inside $root is a different
  # variable: its own decl target and every use in its scope must keep the
  # original name (#254 B-ii — op/while.t's `while (my $i = …) { … my $i = 0 }`).
  # Without this the rename merged the two scopes, which is why the callers'
  # blocker refused the shape outright; the reducer that decides "is this use
  # shadowed" is the same one the span pass uses, so the two agree.
  my $shadowed = sub {
    my ($t) = @_;
    return 1 if $t != $sym && _is_lexical_decl_name($t);
    return $self->_ref_shadowed($t, $canon, [], $root);
  };
  (my $newbare = $new) =~ s/^[\$\@\%]//;
  my $interp_fix = _interp_fixer($old, $newbare);
  my ($seen_sym, $past_decl) = (0, 0);
  my ($stop_tok, $skip_end, $skip_in) = (undef) x 3;
  for my $t ($root->tokens) {
    if (!$seen_sym) {
      next unless $t == $sym;
      $t->set_content($new);
      $seen_sym = 1;
      next;
    }
    if (!$past_decl) {
      my ($p, $inside) = ($t->parent, 0);
      while ($p) { if ($p == $decl) { $inside = 1; last } $p = $p->parent; }
      next if $inside;   # decl RHS: `my $x = $x` reads the OUTER $x
      $past_decl = 1;
    }
    # A LATER declaration of the same name ends this one's claim on the uses it
    # covers (#296-B2).  Only a `my`/`state` word can open one, so the test
    # costs nothing on ordinary tokens.
    if (!$stop_tok && !$skip_end && $t->isa('PPI::Token::Word')
        && $t->content =~ /^(?:my|state)$/) {
      my ($kind, $end, $inner) = _redecl_region($root, $t, $sym, $canon);
      if    (($kind // '') eq 'stop') { $stop_tok = $end }
      elsif (defined $kind)           { ($skip_end, $skip_in) = ($end, $inner) }
    }
    if ($skip_end) {
      my $out = 0;                      # in the construct's OUTER-evaluated part
      for (my $p = $t; $p; $p = $p->parent) {
        if (refaddr($p) == refaddr($skip_in)) { $out = 1; last }
      }
      $skip_end = undef if refaddr($t) == refaddr($skip_end);
      next if !$out;
    }
    _rename_use_token($t, $old, $new, $interp_fix, $shadowed);
    last if $stop_tok && refaddr($t) == refaddr($stop_tok);
  }
  return $new;
}

# Where an earlier declaration's rewrite region gives way to a LATER `my`/`state`
# word $w declaring the same $canon.  Returns:
#
#   ('stop', TOKEN)         $w re-declares in the SAME scope $root — perl's
#                           later `my` is a NEW variable and every use from
#                           there on is ITS use, rewritten by its own pass.
#                           TOKEN is the LAST TOKEN OF THE STATEMENT, not the
#                           declarator: perl does not introduce the new name
#                           until the current statement finishes, so the
#                           redeclaration's own initializer still reads the
#                           EARLIER variable (probed: `my $a = "X";
#                           my $a = "[$a]"` prints `[X]`).
#   ('skip', TOKEN, NODE)   $w declares into a CONSTRUCT (`for my $x (LIST)`,
#                           `while (my $x = …)`, `for (my $x = 0; …)`), whose
#                           whole extent is the new variable's scope.  Rewriting
#                           resumes after TOKEN (the construct's last token);
#                           inside it only NODE — the region _lexical_decl_scope
#                           says is evaluated in the OUTER scope — is still ours.
#   ()                      not a redeclaration of $canon, or one scoped to a
#                           nested BLOCK: _ref_shadowed decides those, and it is
#                           positionally exact there (statements before the inner
#                           `my` still belong to us), which a whole-region skip
#                           would not be.
#
# Both cases were live on this branch and correct before it: an earlier decl's
# region ran straight through the later declarator, renaming uses the later
# declaration's own pass then could not find — split.t's three same-block
# `my ($a,$b) = split …` statements all read the FIRST split's values, and a
# `while (my $a = …)` body read the enclosing block's `my $a`.  _ref_shadowed
# cannot see either: it inspects Block/Sub parents, and a construct's head is a
# sibling of neither.
sub _redecl_region {
  my ($root, $w, $sym, $canon) = @_;
  my $nx = $w->snext_sibling or return ();
  my @syms = $nx->isa('PPI::Token::Symbol')   ? ($nx)
           : $nx->isa('PPI::Structure::List') ? @{ $nx->find('PPI::Token::Symbol') || [] }
           : ();
  for my $s (@syms) {
    next if $s == $sym || $s->symbol ne $canon;
    my ($r, $d) = _lexical_decl_scope($w, $s);
    next unless $r;
    if (refaddr($r) == refaddr($root)) {
      # The declaring STATEMENT as $root sees it — the child of $root holding
      # the declarator.  (Not $w->statement: a `my` embedded in an argument
      # list — `func(my $x)` — sits in an inner Statement::Expression, and
      # perl's "not until the statement ends" is about the whole statement.)
      my $top = $w;
      $top = $top->parent while $top->parent && refaddr($top->parent) != refaddr($root);
      return ('stop', _last_token($top));
    }
    next unless $r->isa('PPI::Statement::Compound');
    return ('skip', _last_token($r), $d // $w->parent);
  }
  return ();
}

sub _last_token {
  my ($node) = @_;
  return $node unless $node->isa('PPI::Node');
  return ($node->tokens)[-1];
}

# Rewrite ONE token that USES the variable $old (code symbol, `$#x` array
# index, or interpolating text mentioning it) to $new — the shared body of
# every use-rewrite in the file.  $shadowed answers "does this token belong to
# a different variable of the same name" for its caller's scope model; the
# positional/scope logic stays there, this is only the rewrite.
# (Extracted from _rename_decl_within so the free-capture rename
# — _rename_free_eval_captures, #296-B1 — cannot drift from it.)
sub _rename_use_token {
  my ($t, $old, $new, $interp_fix, $shadowed) = @_;
  my $sigil = substr($old, 0, 1);
  (my $bare    = $old) =~ s/^[\$\@\%]//;
  (my $newbare = $new) =~ s/^[\$\@\%]//;
  _fix_interp_token($t, $interp_fix, $shadowed);
  # `$#x` last-index of a renamed @x (container decls: state @x family)
  if ($sigil eq '@' && $t->isa('PPI::Token::ArrayIndex')
      && $t->content eq "\$#$bare") {
    $t->set_content("\$#$newbare") unless $shadowed->($t);
    return;
  }
  return unless $t->isa('PPI::Token::Symbol') && $t->symbol eq $old;
  return if $shadowed->($t);
  # Sigil-preserving rewrite: a container's element/slice uses keep their
  # access sigil (`$x[0]`/`$x{k}`/`@x{…}` for @x/%x) — only the NAME part
  # changes.  For scalars this is the identity replacement (element tokens
  # of a same-bare container resolve their ->symbol to @x/%x and never
  # match a scalar $old).
  (my $c = $t->content) =~ s/^([\$\@\%])\Q$bare\E\b/$1$newbare/ or return;
  $t->set_content($c);
  return;
}

# ---- state source rewrite (E1-e, s296) -------------------------------------
# Perl `state` outside the classic subset (a scalar/container statement-decl
# at block level in a named sub — _rename_state_vars' territory) is rewritten
# AT SOURCE LEVEL into plain Perl the existing machinery already lowers
# correctly, and the document is reparsed.  Decl sites become guarded-init
# do-blocks over fresh cells:
#     state $s = EXPR   →  do { unless ($FLAG) { $CELL = EXPR; $FLAG = 1 } $CELL }
#     state $s          →  $CELL          (bare cell — undef box, ref-stable)
# with every post-decl use of $s in its scope renamed to $CELL.  Cell
# OWNERSHIP decides where the cells live:
#   - nearest enclosing CV is an ANON SUB → the `sub {…}` is wrapped
#     `do { my $CELL; my $FLAG; sub {…} }`.  The do runs per sub{} EVALUATION,
#     so each closure instance gets fresh cells (perl's per-instance state
#     semantics: `push @f, sub { state $x } for 1..2` yields independent
#     cells) — riding entirely on the proven anon-sub-closes-over-block-my
#     seam mechanism.  Scalar decls only; containers in anon subs die → v1.
#   - named sub / file CV (including map/grep/sort blocks, which are NOT CVs)
#     → no wrap: the fresh unique names are left undeclared and become
#     defvar'd package globals via _forward_global_decls (boxed cell, raw-nil
#     flag is a boxed global too — the source-level `unless ($FLAG)` unboxes).
#     One shared cell = per-CV semantics for single-instance CVs.  This also
#     covers expression-position decls (`++state $y`, `\state $x`,
#     `my $x = state $y = 42`, `goto state $l = $f`) and decls inside
#     Given/When conditions.
# Not touched here (left for the classic pass or the seam):
#   - the classic named-sub statement subset (see _rename_state_vars);
#   - decls inside a SIGNATURED named sub (or inside a signature default):
#     v1 owns the whole definition (_fallback_stmt), and its own state
#     machinery handles them (signatures.t t126/t127);
#   - `state sub NAME` lexical subs (snext is not a Symbol).
# String eval residue: a renamed cell is NOT reachable by its original name
# from eval'd code (same gap as v1's own state renames — v1 parity).
sub _rewrite_state_prepass {
  my ($self, $doc) = @_;
  my $has = $doc->find(sub {
    $_[1]->isa('PPI::Token::Word') && $_[1]->content eq 'state' }) || [];
  return $doc unless ref $has && @$has;
  $doc = $self->_reparse_doc($doc) if $self->_state_normalize_decls($doc);
  $doc = $self->_reparse_doc($doc) if $self->_state_rewrite_routes($doc);
  return $doc;
}

# Reparse the document from its own (source-level rewritten) text.  Shared by
# the state prepass and the #270 prototype repair.
sub _reparse_doc {
  my ($self, $doc) = @_;
  # The trailing-tail trim for PPI's global-state bug lives in _ppi_parse (the
  # ONE place either pipeline turns source into a document), so a reparse here
  # inherits it — see the note there and docs/ppi-upstream-bugs.md §13.
  my $new = $self->fallback_parser->_ppi_parse($doc->serialize)
    or die "Parser2: PPI reparse after source-level rewrite failed\n";
  return $new;
}

# `tie my $y, ARGS;` → `my $y; tie $y, ARGS;` (see the parse() comment).
sub _normalize_tie_my {
  my ($self, $doc) = @_;
  my $changed = 0;
  for my $stmt (@{ $doc->find('PPI::Statement') || [] }) {
    next unless ref($stmt) eq 'PPI::Statement';   # plain statements only
    my @k = _strip_semi($stmt->schildren);
    next unless @k >= 3
      && $k[0]->isa('PPI::Token::Word') && $k[0]->content eq 'tie'
      && $k[1]->isa('PPI::Token::Word') && $k[1]->content eq 'my'
      && $k[2]->isa('PPI::Token::Symbol') && $k[2]->content =~ /^\$\w+$/;
    my $sym = $k[2]->content;
    $k[0]->set_content('my');
    $k[1]->delete;
    $k[2]->set_content("$sym ; tie $sym");
    $changed++;
  }
  return $changed ? $self->_reparse_doc($doc) : $doc;
}

# PPI LEXER BUG (1.291, #268).  At the START of an expression — right after
# `(`, `[`, `{` or a `,` that opens a fresh Statement::Expression — an
# anonymous sub carrying an ATTRIBUTE is tokenized as a LABEL:
#
#     (sub :lvalue { 1 })       Label('sub :')  Word('lvalue')  Block
#     (sub :lvalue :method {})  Label('sub :')  Label('lvalue :') Word('method')
#     (sub :prototype($$) {})   Label('sub :')  Word('prototype') List('($$)')
#
# Mid-expression the SAME text tokenizes correctly (Word/Operator/Attribute),
# which is why `my $f = sub :lvalue {…}` parsed and `(sub :lvalue {…})` did
# not: the token run never reached the anon-sub handler, the expression fell
# through to "Missing case: [", and the whole statement was replaced by a
# PARSE ERROR comment — silently dropping code (op/sub_lval.t).
#
# Rewrite the mis-lexed run back into the plain `sub` Word the ordinary
# anon-sub path already consumes.  The attributes are DROPPED, exactly as the
# named-sub path and PExpr's sibling strip drop them (:lvalue/:method carry no
# CL meaning here; a :prototype on an ANON sub cannot be consulted by the
# caller-side parser, which keys on a declared name).  Registered upstream in
# docs/ppi-upstream-bugs.md §7.
sub _normalize_anon_sub_attrs {
  my ($self, $doc) = @_;
  # #270 first: a prototype whose text ends in `$` mangles the run one layer
  # deeper than the repair below can see, and it is repaired on the raw token
  # stream before any of the tree surgery here runs.
  $doc = $self->_repair_swallowing_prototypes($doc);
  for my $lab (@{ $doc->find('PPI::Token::Label') || [] }) {
    next unless $lab->content =~ /^sub\s*:$/;
    # Inside a `for`/`foreach` LIST the lexer goes one step further and puts
    # each mis-lexed label in a STATEMENT OF ITS OWN (a label statement is a
    # complete statement), splitting one expression across two or three
    # siblings.  Merge them back into the label's statement first, so the run
    # below sees the same token sequence it sees inside an ordinary list.
    my $stmt = $lab->parent;
    if ($stmt && $stmt->isa('PPI::Statement')) {
      my $merged = 0;
      while (1) {
        my @sig = $stmt->schildren;
        last unless @sig && $sig[-1]->isa('PPI::Token::Label');
        my $next = $stmt->snext_sibling;
        last unless $next && $next->isa('PPI::Statement');
        for my $c ($next->children) {
          $next->remove_child($c);
          $stmt->add_element($c);
        }
        $next->delete;
        $merged++;
      }
      # It was only a statement because of the label; it is an expression.
      bless $stmt, 'PPI::Statement::Expression'
        if $merged && ref($stmt) ne 'PPI::Statement::Expression';
    }
    my @drop;
    my $t = $lab->snext_sibling;
    while ($t) {
      if ($t->isa('PPI::Token::Label') && $t->content =~ /^\w+\s*:$/) {
        push @drop, $t;                       # a chained attribute
        $t = $t->snext_sibling;
        next;
      }
      last unless $t->isa('PPI::Token::Word');
      push @drop, $t;                         # the final attribute's name
      $t = $t->snext_sibling;
      if ($t && $t->isa('PPI::Structure::List')) {
        push @drop, $t;                       # `:prototype($$)`'s own parens
        $t = $t->snext_sibling;
      }
      last;
    }
    # The run must end at the sub's block.  A `sub :` Label is only ever
    # produced by this mis-lex, so a run that does NOT end at a Block is
    # known-mangled input in a shape the repair does not cover: die naming it
    # rather than fall through, which would silently drop the statement
    # (#270 — that silence is what made `:prototype($)` vanish at exit 0).
    if (! (@drop && $t && $t->isa('PPI::Structure::Block'))) {
      my $shape = join '', map { $_->content } $lab, @drop;
      die "Parser2: unrepaired mis-lexed anon-sub attribute run `$shape` "
        . "(PPI lexes `sub :ATTR` at the start of an expression as a Label; "
        . "see docs/ppi-upstream-bugs.md \x{a7}7) at line "
        . $lab->line_number . "\n";
    }
    # `:prototype(…)` normally survives as a runtime `__pcl_set_prototype`
    # wrap (Pl::Parser::_extract_prototype_attributes), but that pass runs on
    # the token stream PPI produced — where this one is a Word, invisible to
    # it — and it cannot be re-run here without a reparse that would just
    # re-create the mis-lex.  Dropping it is EFFECT-ONLY (an anon sub has no
    # name for the call-site parser to consult; that is already true of the
    # correctly-lexed spelling, whose wrap is a runtime call), so it
    # ANNOUNCES and continues per the s329 boundary rather than dying or
    # going quiet.  docs/not-supported.md carries the entry.
    # The attribute NAME and its parens are separate tokens here (Word +
    # Structure::List), so read the run as text rather than per token.
    my $run   = join '', map { $_->content } @drop;
    my $proto = $run =~ /(prototype\([^)]*\))/ ? $1 : '';
    warn "PCL: attribute `:$proto` on an anonymous sub at the start "
       . "of an expression is dropped (PPI lexes it as a label; see "
       . "docs/ppi-upstream-bugs.md \x{a7}7)\n"
      if $proto ne '';
    $_->delete for @drop;
    bless $lab, 'PPI::Token::Word';
    $lab->set_content('sub');
  }
  return $doc;
}

# #270, the second layer of the §7 mis-lex, repaired on the RAW TOKEN STREAM.
#
# `prototype($)` — and every prototype whose text ends in `$`: `(;$)`,
# `($;$)`, … — has its closing paren eaten by the magic variable `$)`, so PPI
# never closes the attribute's paren group there.  It closes it on the sub's
# OWN closing paren instead, swallowing the block in between; the tree that
# results has the ENCLOSING structure left unfinished, so no local tree edit
# can restore it (and inside a `for` LIST the damage spreads across sibling
# statements).  `($$)`, `(\@)` etc. lex correctly and never come here.
#
# So this runs BEFORE any tree surgery and does not read the tree at all: it
# walks the token stream from each `sub :` Label, and when the run spells
# `:[attr:]*prototype(` … `$)` it blanks exactly those tokens.  The document
# text then reads `sub { … }` as if the attribute had never been written, and
# one reparse yields the tree PPI would have built for the plain anon sub.
#
# Dropping the prototype is EFFECT-ONLY and ANNOUNCES, per the s329 boundary
# and the #268 entry in docs/not-supported.md: an anon sub has no name for a
# call-site parser to consult, so nothing downstream consumes the value.
sub _repair_swallowing_prototypes {
  my ($self, $doc) = @_;
  my $repaired = 0;
  for my $lab (@{ $doc->find('PPI::Token::Label') || [] }) {
    next unless $lab->content =~ /^sub\s*:$/;
    my @blank;                                # tokens to erase from the source
    my $t = _next_sig_token($lab);
    while ($t && $t->isa('PPI::Token::Label') && $t->content =~ /^\w+\s*:$/) {
      push @blank, $t;                        # a chained attribute
      $t = _next_sig_token($t);
    }
    next unless $t && $t->isa('PPI::Token::Word') && $t->content eq 'prototype';
    push @blank, $t;
    $t = _next_sig_token($t);
    next unless $t && $t->isa('PPI::Token::Structure') && $t->content eq '(';
    push @blank, $t;
    # Everything up to the mis-lexed `$)`, which must all be prototype text —
    # anything else means this is not the shape, and the run is left alone for
    # the caller's die to name.
    my @proto;
    while (1) {
      $t = _next_sig_token($t);
      last unless $t && $t->isa('PPI::Token');
      last if $t->isa('PPI::Token::Magic') && $t->content eq '$)';
      last unless $t->content =~ /^[\\\$\@\%\&\*\[\]\;\+\_]+$/;
      push @proto, $t;
    }
    next unless $t && $t->isa('PPI::Token::Magic') && $t->content eq '$)';
    warn "PCL: attribute `:prototype("
       . join('', map { $_->content } @proto) . "\$)` on an anonymous sub at "
       . "the start of an expression is dropped (PPI lexes it as a label; see "
       . "docs/ppi-upstream-bugs.md \x{a7}7)\n";
    $lab->set_content('sub');
    $_->set_content('') for @blank, @proto, $t;
    $repaired = 1;
  }
  return $repaired ? $self->_reparse_doc($doc) : $doc;
}

# PPI LEXER BUG (task #327): `for \my %e (@list) {…}` — the refaliasing
# foreach.  PPI only lexes `foreach [my] $scalar (LIST) BLOCK`; both the
# `\`-cast and a non-scalar loop variable break it, and the damage is not
# local — the Statement::Compound comes out holding ONLY the word `for`, with
# the rest of the construct AND EVERY FOLLOWING STATEMENT swallowed into one
# flat sibling.  So no tree edit can repair it: like #270, this runs on the RAW
# TOKEN STREAM (unaffected by the lexing) and reparses.
#
# The rewrite is a pure RE-SPELLING into the alias assignment the compiler
# already has (p-setf's \-cast place, task #325):
#
#     for \my %e (@list) { BODY }
#       ⇒  for my $__PCL_RA0 (@list) { \my %e = $__PCL_RA0; BODY }
#
# so there is no new foreach macro and no VarAnnotator work: %e is an ordinary
# block-level declaration, scoped to the body and fresh per iteration, which is
# exactly perl's scoping.  A PACKAGE loop variable needs no save/restore —
# probed on 5.40.3, perl does NOT restore an aliased package loop variable
# after the loop (`our $s; for \$::s (\"a",\"b") {} print $s` prints "b"), so
# the alias persisting IS the perl behaviour.
sub _repair_alias_foreach {
  my ($self, $doc) = @_;
  my ($n, $repaired) = (0, 0);
  for my $w (@{ $doc->find('PPI::Token::Word') || [] }) {
    next unless $w->content =~ /^for(?:each)?$/;
    my $cast = _next_sig_token($w);
    next unless $cast && $cast->isa('PPI::Token::Cast') && $cast->content eq '\\';
    my $t = _next_sig_token($cast);
    my $decl;
    if ($t && $t->isa('PPI::Token::Word') && $t->content =~ /^(?:my|our|state)$/) {
      $decl = $t;
      $t = _next_sig_token($t);
    }
    next unless $t && $t->isa('PPI::Token::Symbol');
    my $var  = $t;
    my $open = _next_sig_token($var);
    next unless $open && $open->isa('PPI::Token::Structure') && $open->content eq '(';
    # The list and block structures ARE built (only the compound statement is
    # not), so the block's opening brace is reachable from the list's finish.
    my $list = $open->parent;
    next unless $list && $list->isa('PPI::Structure::List') && $list->finish;
    my $brace = _next_sig_token($list->finish);
    next unless $brace && $brace->isa('PPI::Token::Structure') && $brace->content eq '{';
    my $tmp  = '$__PCL_RA' . $n++;
    my $name = $var->content;
    my $dw   = $decl ? $decl->content : '';
    # `our` spells the cast INSIDE the declaration (`our \$T = …`); the other
    # declarators spell it outside (`\my %e = …`, `\state @a = …`).
    my $alias = $dw eq 'our' ? "our \\$name = $tmp;"
              : $dw          ? "\\$dw $name = $tmp;"
              :                "\\$name = $tmp;";
    $cast->set_content('my ');       # the trailing space matters: `for \%_ (…)`
    $decl->set_content('') if $decl; # has no whitespace of its own to reuse
    $var->set_content($tmp);
    $brace->set_content("{ $alias");
    $repaired = 1;
  }
  return $repaired ? $self->_reparse_doc($doc) : $doc;
}

# PPI LEXER BUG (task #329): `for my ($q, $r) (LIST) {…}` — perl 5.36's
# n-at-a-time foreach.  Same mis-lex as _repair_alias_foreach (PPI only knows
# `foreach [my] $scalar (LIST) BLOCK`, so the compound keeps just `for my` and
# the rest lands in a flat sibling), and the same cure: repair the RAW TOKEN
# STREAM, then reparse.
#
# The re-spelling is a `while` loop over a list of REFERENCES, which is what
# makes the loop variables ALIAS the list elements the way perl's do:
#
#     for my ($q, $r) (@a) { BODY } continue { CONT }
#       ⇒  my @L = map \$_, (@a);               # one ref per ELEMENT
#          my @PD; push @L, \$PD[scalar @PD] while @L % 2;   # short last chunk
#          my $I = 0;
#          while ($I < @L) { \my $q = $L[$I]; \my $r = $L[$I+1]; BODY }
#          continue { CONT; $I += 2 }
#
# Every piece is a mechanism PCL already has: `map \$_, LIST` gives one
# write-through ref per element because map ALIASES $_ to each element (probed
# against perl for arrays, several arrays, a hash, literals and `reverse` — all
# six identical), and `\my $q = REF` is the refaliasing declaration from #325.
# NOT `\(LIST)`: that distributes over the list's TERMS, so `\(@Q, @A)` is two
# ARRAY refs (perl's own answer, probed) — right for `\(…)`, wrong here.
#
# `while`, not a C-style `for`: perl allows a `continue` block on this loop
# (op/for-many.t uses one together with redo/next) and a C-style for cannot
# carry one.  The three non-local exits then land where perl puts them —
# `next` runs the continue block and the step, `redo` runs neither, `last`
# leaves — because the step IS in the continue block.
#
# The PAD array gives the short final chunk its own writable slots, one per
# missing variable, which is perl's answer (`for my ($q,$r,$s) (@a)` with 7
# elements leaves $s undef on the last pass) — a single shared pad would make
# two loop variables the same variable.
sub _repair_nary_foreach {
  my ($self, $doc) = @_;
  my ($n, $repaired) = (0, 0);
  for my $w (@{ $doc->find('PPI::Token::Word') || [] }) {
    next unless $w->content =~ /^for(?:each)?$/;
    my $my = _next_sig_token($w);
    next unless $my && $my->isa('PPI::Token::Word') && $my->content eq 'my';
    my $o1 = _next_sig_token($my);
    next unless $o1 && $o1->isa('PPI::Token::Structure') && $o1->content eq '(';
    my $vars = $o1->parent;
    next unless $vars && $vars->isa('PPI::Structure::List') && $vars->finish;
    my @names = map { $_->content } @{ $vars->find('PPI::Token::Symbol') || [] };
    # Scalars only — perl rejects every other spelling of this list, so a
    # non-scalar here is not this construct and must not be rewritten.
    next unless @names && !grep { !/^\$\w+$/ } @names;
    my $o2 = _next_sig_token($vars->finish);
    next unless $o2 && $o2->isa('PPI::Token::Structure') && $o2->content eq '(';
    my $list = $o2->parent;
    next unless $list && $list->isa('PPI::Structure::List') && $list->finish;
    my $brace = _next_sig_token($list->finish);
    next unless $brace && $brace->isa('PPI::Token::Structure') && $brace->content eq '{';
    my $block = $brace->parent;
    next unless $block && $block->isa('PPI::Structure::Block') && $block->finish;

    my $k = @names;
    my ($L, $PD, $I) = ("\@__PCL_FL$n", "\@__PCL_PD$n", "\$__PCL_FI$n");
    (my $Ls = $L) =~ s/^\@/\$/;
    my $setup = "my $L = map \\\$_, " . $list->content . "; my $PD; "
              . "push $L, \\$PD\[scalar $PD] while $L % $k; my $I = 0; ";
    # A label belongs to the LOOP, not to the setup statements in front of it.
    my $label = '';
    if (my $prev = _prev_sig_token($w)) {
      if ($prev->isa('PPI::Token::Label')) {
        $label = $prev->content . ' ';
        $prev->set_content('');
      }
    }
    $w->set_content($setup . $label . 'while ');
    $my->set_content('');
    $_->set_content('') for $vars->tokens;
    $_->set_content('') for $list->tokens;
    $o2->set_content("($I < $L)");
    $brace->set_content('{ ' . join('', map {
      "\\my $names[$_] = $Ls\[$I" . ($_ ? " + $_" : '') . "]; " } 0 .. $k - 1));
    # The step goes in the continue block so `next` reaches it; an existing
    # continue block keeps its own statements and gains the step after them,
    # which is the order perl runs them in.
    my $cont_w = _next_sig_token($block->finish);
    my $cont_b = $cont_w && $cont_w->isa('PPI::Token::Word')
              && $cont_w->content eq 'continue' ? $cont_w->snext_sibling : undef;
    if ($cont_b && $cont_b->isa('PPI::Structure::Block') && $cont_b->finish) {
      $cont_b->finish->set_content("; $I += $k; }");
    } else {
      $block->finish->set_content("} continue { $I += $k; }");
    }
    $n++;
    $repaired = 1;
  }
  return $repaired ? $self->_reparse_doc($doc) : $doc;
}

# PPI LEXER BUG (task #354, docs/ppi-upstream-bugs.md §12).  After a token that
# ENDS A TERM, `*name` written with no space is lexed as a GLOB — a
# PPI::Token::Symbol — instead of the multiplication it can only be:
#
#     $s += length($k)*length($k);   =>  Word List Symbol(*length) List
#
# PExpr has no case for that shape, so the WHOLE STATEMENT is dropped (#138
# family).  Data::Dump line 325 is exactly this, in any program that uses it.
# What makes it easy to miss: a space (`) * length`) lexes correctly, and so
# does a NUMBER on the left (`2*length($k)`) — measured, both.
#
# The repair is perl's rule on the raw token stream: a `*` cannot open a glob
# where a term has just ended, so split the token back into operator + word and
# reparse.  Everything the predicate leans on is in _ends_term.
sub _repair_glob_multiply {
  my ($self, $doc) = @_;
  my $repaired = 0;
  for my $sym (@{ $doc->find('PPI::Token::Symbol') || [] }) {
    my ($name) = $sym->content =~ /^\*(\w+(?:::\w+)*)\z/ or next;
    next unless _ends_term(_prev_sig_token($sym));
    $sym->set_content("* $name");
    $repaired = 1;
  }
  return $repaired ? $self->_reparse_doc($doc) : $doc;
}

# PPI LEXER BUG (task #351, docs/ppi-upstream-bugs.md §11).  A bare `/PATTERN/`
# as the first argument of a PAREN-LESS call is tokenized as DIVISION:
#
#     ok /foo/, "desc";   =>  Word(ok) Operator(/) Word(foo) Operator(/) …
#
# so the statement is dropped whole, and with a modifier letter (`ok /foo/x`)
# it compiles to real division and dies at run time.  PPI gets it right after
# `grep`, `return`, `(` and `=`, and wrong after every other Word — including
# core list operators (`print /foo/`).
#
# THE CONDITION IS PERL'S, and it is a NEGATIVE, which is what makes it safe:
# perl reads `/` after a bareword as division only when the word is a TERM (a
# constant, a `()`-prototyped sub, or a 0-ary builtin).  For anything else perl
# does not fall back to division — it is a SYNTAX ERROR (measured: `ok /foo/`
# with no `sub ok` above it, and with `ok` declared BELOW, are both syntax
# errors; `sub f {…} print f / 2` reads the `/` as a match; `sub g () {…}` and
# `use constant PI => 6` read it as division).  PCL assumes valid Perl input
# (principle 9), so "not a term" is exactly the right test.
#
# Measured over both populations (657 files, 28 `WORD /` sites): the shapes are
# `ok`, `while` and `when` — all repairs — plus one that must NOT be touched,
# `map { … } <op/*>`, where PPI derails a GLOB into `< Word / * >`.  Hence the
# `<` guard, which no amount of reading would have suggested.
sub _repair_word_match {
  my ($self, $doc) = @_;
  my @tok = grep { $_->significant } $doc->tokens;
  my $repaired = 0;
  for my $i (1 .. $#tok) {
    my ($prev, $t) = @tok[$i - 1, $i];
    next unless $t->isa('PPI::Token::Operator') && $t->content eq '/';
    next unless $prev->isa('PPI::Token::Word');
    next if $i >= 2 && $tok[$i - 2]->isa('PPI::Token::Operator')
                    && $tok[$i - 2]->content eq '<';       # <op/*> glob run
    next if _is_method_name_word($prev);                # $o->w / $o->h: division
    next if $self->_word_is_term($prev->content, $doc);
    # A match needs a closing delimiter; without one this is not the shape and
    # the statement is left to the ordinary error path to name.
    my $close = 0;
    for my $j ($i + 1 .. $#tok) {
      last if $tok[$j]->isa('PPI::Token::Structure') && $tok[$j]->content eq ';';
      ($close = 1), last if $tok[$j]->isa('PPI::Token::Operator')
                         && $tok[$j]->content eq '/';
    }
    next unless $close;
    $t->set_content('m/');
    $repaired = 1;
  }
  return $repaired ? $self->_reparse_doc($doc) : $doc;
}


# PPI LEXER BUG (task #370, docs/ppi-upstream-bugs.md §20).  `~~` is ONE token
# to PPI and always the smart-match operator, but perl reads it that way only
# where an OPERATOR may stand.  Where a TERM is expected it is two complements:
#
#     is(~~$y, 3);   perl: ~(~$y) — the classic "numify" idiom, bop.t asserts it
#                    PPI:  Structure( … Operator(~~) Symbol($y) … )
#
# and the main loop then sees a binary operator with no left operand, so the
# whole statement is DROPPED ("Fell through").  perl-tests/bop.t and t/op/bop.t
# each carry two of them.
#
# THE CONDITION IS THE SAME NEGATIVE the other repairs use, and it is exact:
# `~~` is the operator only after something that ENDS A TERM.  Nothing else can
# follow a term-ending token here, and nothing else can precede a prefix
# complement — perl has no third reading.  So an occurrence that is not after a
# term becomes two `~` tokens and the ordinary prefix path handles it; an infix
# one is left alone, and Track A (task #371) refuses it perl-shaped, because
# smart match was removed in perl 5.42.
sub _repair_term_initial_complement {
  my ($self, $doc) = @_;
  my $repaired = 0;
  for my $op (@{ $doc->find('PPI::Token::Operator') || [] }) {
    next unless $op->content eq '~~';
    next if _ends_term(_prev_sig_token($op));
    $op->set_content('~ ~');
    $repaired = 1;
  }
  return $repaired ? $self->_reparse_doc($doc) : $doc;
}
# PPI LEXER BUG (task #361, docs/ppi-upstream-bugs.md §19).  `x` is both an
# operator and a legal sub name, and PPI decides which by looking at the token
# before it — a Word counts as a complete term, so:
#
#     sub x { "PKG" }  print x(), "|\n";
#         => Word(print) Operator(x) Structure(() …      WRONG
#
# and the statement compiles to `(print $_) x ()` — the print of $_ repeated
# zero times.  It prints NOTHING and announces nothing: silent wrong, rc 0.
# perl reads it as the call (measured: "PKG|"), because `print` is a list
# operator and cannot be the left operand of `x`.
#
# SAME CONDITION AS _repair_word_match, and the same reason it is safe: the
# previous Word must not be a TERM.  `print FOO x 3` (a constant), `print "-" x
# 5`, `print $s x 3` and `print g() x 3` all keep the operator reading, because
# the token before `x` is a term in each.  `x` is the only member of this
# family that is legal perl at all — every other operator-shaped name (eq, lt,
# cmp, and, …) is a compile error as a sub name, and `not` is a named unary
# that PPI already gets right (both probed s406).
#
# The repair is perl's own disambiguator: a unary `+` in front of the call,
# which is a documented no-op and makes PPI lex the word as a word.  PCL
# already emits `+x()` as a plain call, so the fix costs no emission shape.
sub _repair_word_x_call {
  my ($self, $doc) = @_;
  my @tok = grep { $_->significant } $doc->tokens;
  my $repaired = 0;
  for my $i (1 .. $#tok) {
    my ($prev, $t) = @tok[$i - 1, $i];
    next unless $t->isa('PPI::Token::Operator') && $t->content eq 'x';
    next unless $prev->isa('PPI::Token::Word');
    # A METHOD NAME is a term (`$o->name x 3` repeats the method's value; it
    # was mis-repaired into `$o->name + x(3)` — s407 review, a regression of
    # this repair), and the repair only makes sense at all when this document
    # DECLARES a sub named `x`: without one, valid perl cannot be calling it.
    next if _is_method_name_word($prev);
    next unless $self->_document_declares_sub('x', $doc);
    # DECLARED terms only: an ALL-CAPS word that this document does not declare
    # as a constant is a FILEHANDLE here (`print STDOUT x(), …`), and a handle
    # is not an operand, so perl reads the `x` as the call.  A declared
    # constant (`use constant FOO => "-"; print FOO x 3`) keeps the operator.
    next if $self->_word_is_declared_term($prev->content, $doc);
    $t->set_content('+x');
    $repaired = 1;
  }
  return $repaired ? $self->_reparse_doc($doc) : $doc;
}

# PPI LEXER BUG (task #340, docs/ppi-upstream-bugs.md §18).  With
# `use feature 'try'` in scope PPI 1.291 knows the construct half way: it lexes
# `try {…} catch (VAR) {…}` into a PPI::Statement::Compound and then STOPS.  A
# following `finally {…}` is left out, and since the orphan statement it starts
# has no terminator, it SWALLOWS every following statement up to the next `;`:
#
#     try {…} catch ($e) {…} finally {…}   is one Compound plus
#     is($x, 1, 'desc');                   PPI::Statement: finally {…} is(…);
#
# so the assertion after a finally block simply disappears into it.  (Without
# the pragma PPI does not know `try` at all and the whole construct is one
# swallowing statement — but then perl does not compile the file either, so
# principle 9 leaves that alone.)
#
# The repair terminates the orphan where perl does, by giving the finally
# block's closing brace a `;`.  What comes after is then an ordinary statement
# again, and the `finally {…};` that is left joins its try in _lower_block —
# the same route the unlabeled `continue` block already takes.
sub _repair_try_finally {
  my ($self, $doc) = @_;
  my $repaired = 0;
  for my $w (@{ $doc->find('PPI::Token::Word') || [] }) {
    next unless $w->content eq 'finally';
    my $stmt = $w->parent;
    next unless $stmt && ref($stmt) eq 'PPI::Statement';
    my @k = _strip_semi($stmt->schildren);
    # `finally` must OPEN the statement and be followed by its block; anything
    # else (Try::Tiny's `try {…} catch {…} finally {…};`, where finally is
    # mid-statement) is a different construct and is left alone.
    next unless @k > 2 && $k[0] == $w && $k[1]->isa('PPI::Structure::Block');
    my $prev = $stmt->sprevious_sibling;
    next unless $prev && $prev->isa('PPI::Statement::Compound');
    my @pk = $prev->schildren;
    next unless @pk && $pk[0]->isa('PPI::Token::Word') && $pk[0]->content eq 'try';
    my $close = $k[1]->finish or next;
    $close->set_content($close->content . ';');
    $repaired = 1;
  }
  return $repaired ? $self->_reparse_doc($doc) : $doc;
}

# Is this bareword a TERM — the half of perl's rule that keeps `time / 60` and
# `PI / 2` division?  Three sources, in the order they are cheap:
#   * the 0-ary builtins (time, times, wantarray, __PACKAGE__, …);
#   * a constant or `()`-prototyped sub DECLARED IN THIS DOCUMENT — perl learns
#     both at compile time, and so can we, from the raw tokens;
#   * the ALL-CAPS convention, which is what the compiler already assumes for a
#     constant it cannot place (PExpr::_bareword_subscript_autoquotes).  An
#     imported constant (`use POSIX qw(DBL_MAX); DBL_MAX / 2`) is invisible to
#     a token scan, and this is the cheap way not to break it.
sub _word_is_term {
  my ($self, $name, $doc) = @_;
  return 1 if $name =~ /^[A-Z][A-Z0-9_]*\z/;
  return $self->_word_is_declared_term($name, $doc);
}

# The same question WITHOUT the ALL-CAPS guess, for the callers where an
# ALL-CAPS bareword is more likely a FILEHANDLE than a constant.  `print FOO x
# 3` needs the guess (FOO is a constant if it is anything); `print STDOUT x()`
# needs its absence, because a handle is not an operand and perl reads the `x`
# as a call.  Splitting the predicate keeps ONE copy of each half.
sub _word_is_declared_term {
  my ($self, $name, $doc) = @_;
  my $zero = $self->_zero_arity_builtins;
  return 1 if $zero->{$name};
  my $terms = $self->_doc_term_words($doc);
  return $terms->{$name} ? 1 : 0;
}

# The two per-document name sets ONE token walk produces (_scan_document_terms):
# the words this document makes a TERM, and the subs it DECLARES by any `sub
# NAME` — cached together, because a second scan for the second answer is how
# the two drift apart.
sub _doc_term_words {
  my ($self, $doc) = @_;
  $self->_scan_terms_and_subs($doc) unless $self->{_doc_term_words};
  return $self->{_doc_term_words};
}

# Does this document declare `sub NAME` at all (any prototype, or none)?  The
# question the `x` repair must ask before it turns `WORD x` into a CALL: perl
# reads `x` after a list operator as a term start — a call to sub x — and valid
# perl can only mean that when a sub named x exists.  With no `sub x` in the
# document (an imported one is invisible; accepted), `WORD x N` is the
# repetition operator whatever WORD is, and the repair must not fire — it turned
# `$o->name x 3` into `$o->name + x(3)` (s407 review).
sub _document_declares_sub {
  my ($self, $name, $doc) = @_;
  $self->_scan_terms_and_subs($doc) unless $self->{_doc_sub_words};
  return $self->{_doc_sub_words}{$name} ? 1 : 0;
}

sub _scan_terms_and_subs {
  my ($self, $doc) = @_;
  my ($terms, $subs) = _scan_document_terms($doc);
  $self->{_doc_term_words} = $terms;
  $self->{_doc_sub_words}  = $subs;
  return;
}

# A Word that is a METHOD NAME — the significant token before it is `->` — is a
# TERM in perl's operator-vs-term state (`$o->w / $o->h`, `$o->name x 3`,
# `$o->w*w()`), and none of the three repairs may read the operator after it as
# anything else.  Found in the s407 review: two of the repairs (#351, #361)
# regressed exactly these shapes, and #354's _ends_term never knew the third.
sub _is_method_name_word {
  my ($t) = @_;
  return 0 unless $t && $t->isa('PPI::Token::Word');
  my $prev = _prev_sig_token($t);
  return ($prev && $prev->isa('PPI::Token::Operator') && $prev->content eq '->') ? 1 : 0;
}

sub _zero_arity_builtins {
  my ($self) = @_;
  return $self->{_zero_arity} ||= do {
    # The ONE arity table (Pl::PExpr::Config), read through PExpr as every
    # other consumer does — never a second list of builtin names here.
    my $t = Pl::PExpr->new->known_no_of_params;
    +{ map { $_ => 1 }
       grep { defined $t->{$_} && !ref $t->{$_} && $t->{$_} == 0 } keys %$t };
  };
}

# Names this DOCUMENT makes a term: `use constant NAME =>`, the hash form
# `use constant { A => …, B => … }`, and `sub NAME ()` (the empty prototype IS
# what makes perl read the next `/` as division — measured).  Returns that set
# AND the set of every `sub NAME` the document declares, from the one walk.
sub _scan_document_terms {
  my ($doc) = @_;
  my (%term, %sub);
  my @tok = grep { $_->significant } $doc->tokens;
  for my $i (0 .. $#tok) {
    my $t = $tok[$i];
    next unless $t->isa('PPI::Token::Word');
    if ($t->content eq 'constant' && $i > 0
        && $tok[$i - 1]->isa('PPI::Token::Word') && $tok[$i - 1]->content eq 'use') {
      for my $j ($i + 1 .. $#tok) {
        last if $tok[$j]->isa('PPI::Token::Structure') && $tok[$j]->content eq ';';
        $term{ $tok[$j]->content } = 1
          if $tok[$j]->isa('PPI::Token::Word')
          && $tok[$j + 1] && $tok[$j + 1]->isa('PPI::Token::Operator')
          && $tok[$j + 1]->content eq '=>';
      }
    }
    elsif ($t->content eq 'sub' && $tok[$i + 1]
           && $tok[$i + 1]->isa('PPI::Token::Word')) {
      # `sub NAME` / `my sub NAME` / `sub NAME ($$)`: a declared sub of any
      # arity (the `x` repair asks this); with an EMPTY prototype it is also a
      # term (the `/` repair asks that).
      $sub{ $tok[$i + 1]->content } = 1;
      $term{ $tok[$i + 1]->content } = 1
        if $tok[$i + 2] && $tok[$i + 2]->isa('PPI::Token::Prototype')
        && $tok[$i + 2]->prototype =~ /^\s*\z/;
    }
  }
  return (\%term, \%sub);
}

# Does this token END A TERM?  That is perl's own operator-vs-term state, and
# it is what decides both repairs below: after a term, `*` and `/` are
# ARITHMETIC; before one, they start a glob or a match.
#
# `}` is deliberately ambiguous and is only counted when it closes a SUBSCRIPT:
# `$h{x}*foo()` is multiplication, but `sub f {…} *bar = \&f;` is a real glob,
# and the tree is what tells them apart (measured, both spellings).
sub _ends_term {
  my ($t) = @_;
  return 0 unless $t;
  return 1 if _is_method_name_word($t);              # $o->w*w(): multiplication
  return 1 if $t->isa('PPI::Token::Symbol')          # $x @a %h *glob, and Magic
           || $t->isa('PPI::Token::Number')
           || $t->isa('PPI::Token::Quote')           # '…' "…" q qq
           || $t->isa('PPI::Token::QuoteLike::Words')
           # A match, a substitution, a heredoc, `…`/qx… and <FH> all YIELD a
           # value, so what follows them is an operator (s415, found by the
           # drop census: without the Regexp arm, #370's repair read
           # `/X/ ~~ @a` as a term-initial `~~` and split perl's smart match
           # into two complements — a silent wrong in t/op/smartmatch.t).
           || $t->isa('PPI::Token::Regexp')
           || $t->isa('PPI::Token::HereDoc')
           || $t->isa('PPI::Token::QuoteLike::Backtick')
           || $t->isa('PPI::Token::QuoteLike::Command')
           || $t->isa('PPI::Token::QuoteLike::Readline');
  if ($t->isa('PPI::Token::Structure')) {
    return 1 if $t->content eq ')' || $t->content eq ']';
    return 1 if $t->content eq '}'
             && $t->parent && $t->parent->isa('PPI::Structure::Subscript');
  }
  return 0;
}

# The previous significant token in DOCUMENT order — the raw stream twin of
# _next_sig_token.
sub _prev_sig_token {
  my ($t) = @_;
  while ($t = $t->previous_token) {
    return $t if $t->significant;
  }
  return undef;
}

# The next significant token in DOCUMENT order (structure starts and finishes
# included) — the raw stream, not the sibling chain.
sub _next_sig_token {
  my ($t) = @_;
  while ($t = $t->next_token) {
    return $t if $t->significant;
  }
  return undef;
}

# True for a `state` Word that heads a declaration (mirrors the classic
# pass's exclusions: `->state`, `state =>`, `sub state`, hash-key uses).
sub _state_declarator_word {
  my ($w) = @_;
  my $prev = $w->sprevious_sibling;
  return 0 if $prev && $prev->isa('PPI::Token::Operator')
           && $prev->content =~ /^(?:->|=>)$/;
  return 0 if $prev && $prev->isa('PPI::Token::Word') && $prev->content eq 'sub';
  return 1;
}

# Shape normalizations so downstream passes see only `state $x [= INIT];`:
#   - attributes:  `state $a :shared = 3`  →  `state $a = 3`   (attr dropped —
#     single-threaded PCL has no shared semantics to honour)
#   - single-scalar list + compound assign:  `state ($t) //= 3;`
#       →  `state $t ; $t //= 3;`   (decl + per-call defaulting, perl's rule)
#   - trailing ++/--:  `state $z ++;`  →  `state $z ; $z ++;`
sub _state_normalize_decls {
  my ($self, $doc) = @_;
  my $changed = 0;
  for my $stmt (@{ $doc->find('PPI::Statement::Variable') || [] }) {
    my @k = _strip_semi($stmt->schildren);
    next unless @k >= 2 && $k[0]->isa('PPI::Token::Word')
             && $k[0]->content eq 'state' && _state_declarator_word($k[0]);
    # attribute strip: `: word` pairs directly after the declared symbol/list
    my $t = $k[1]->snext_sibling;
    while ($t && $t->isa('PPI::Token::Operator') && $t->content eq ':'
           && $t->snext_sibling && $t->snext_sibling->isa('PPI::Token::Word')) {
      my $attr = $t->snext_sibling;
      my $next = $attr->snext_sibling;
      $t->delete; $attr->delete;
      $t = $next; $changed++;
    }
    if ($k[1]->isa('PPI::Structure::List')) {
      # single scalar in parens?
      my @syms = @{ $k[1]->find('PPI::Token::Symbol') || [] };
      next unless @syms == 1 && $syms[0]->content =~ /^\$\w+$/
        && !grep { $_->isa('PPI::Token::Operator') && $_->content eq ',' }
            map { $_->tokens } $k[1];
      my $op = $k[1]->snext_sibling;
      # #140: the one `OP=` set — the hand-rolled regex here omitted
      # `&.= |.= ^.=`, so `state ($u) |.= "a"` was never normalized and
      # yielded undef instead of perl's "a".
      if ($op && $op->isa('PPI::Token::Operator')
          && Pl::PExpr::TokenUtils::is_compound_assign($op->content)) {
        $op->set_content('; ' . $syms[0]->content . ' ' . $op->content);
      }
      $k[1]->start->set_content('');
      $k[1]->finish->set_content('') if $k[1]->finish;
      $changed++;
    } elsif ($k[1]->isa('PPI::Token::Symbol') && @k >= 3
             && $k[2]->isa('PPI::Token::Operator')
             && $k[2]->content =~ /^(?:\+\+|--)$/) {
      $k[2]->set_content('; ' . $k[1]->content . ' ' . $k[2]->content);
      $changed++;
    }
  }
  return $changed;
}

# Route a state decl by ownership.  Returns:
#   { route => 'skip' }                      — signatured sub / sig default: v1 owns
#   { route => 'classic' }                   — _rename_state_vars' subset
#   { route => 'anon', cv_block, subword }   — nearest CV is an anon sub
#   { route => 'expr' }                      — rewrite with package cells
sub _state_route {
  my ($self, $w) = @_;
  my $stmt = $w->parent;
  my $stmt_shaped = $stmt && $stmt->isa('PPI::Statement::Variable')
    && do { my @k = _strip_semi($stmt->schildren); @k && $k[0] == $w }
    && $stmt->parent
    && ($stmt->parent->isa('PPI::Structure::Block')
        || $stmt->parent->isa('PPI::Document'));
  my $in_mgs = 0;
  for (my $n = $w->parent; $n && !$n->isa('PPI::Document'); $n = $n->parent) {
    return { route => 'skip' }
      if $n->isa('PPI::Structure::List') && $n->parent
      && $n->parent->isa('PPI::Statement::Sub');
    if ($n->isa('PPI::Structure::Block')) {
      my $p = $n->sprevious_sibling;
      $p = $p->sprevious_sibling
        while $p && ($p->isa('PPI::Token::Prototype')
                  || $p->isa('PPI::Token::Attribute')
                  || ($p->isa('PPI::Token::Operator') && $p->content eq ':'));
      if ($p && $p->isa('PPI::Token::Word')) {
        my $c = $p->content;
        return { route => 'anon', cv_block => $n, subword => $p } if $c eq 'sub';
        $in_mgs = 1 if $c =~ /^(?:map|grep|sort)$/;
      }
    }
    if ($n->isa('PPI::Statement::Sub') && !$n->isa('PPI::Statement::Scheduled')
        && $n->name) {
      # 'skip' = the sub definition is v1-owned (signature); a pure-prototype
      # sub lowers natively (#126), so its state decls route like any other.
      return { route => 'skip' }
        if defined $self->_proto_or_sig_str($n) && !$self->_is_pure_prototype($n);
      return { route => ($stmt_shaped && !$in_mgs) ? 'classic' : 'expr' };
    }
  }
  return { route => 'expr' };
}

# Reasons the source rewrite of one state decl is unsafe; undef when safe.
# Unlike _shadow_rename_blocker: no string-eval refusal (the cell is a
# package global the eval could never have reached under v1 either), and
# sibling STATE re-decls of the name are tolerated — the caller processes
# decls in REVERSE document order, so an earlier decl's walk finds the later
# scope already renamed (perl's masking semantics fall out positionally).
sub _state_rw_blocker {
  my ($self, $root, $sym) = @_;
  my $old = $sym->content;
  (my $bare = $old) =~ s/^\$//;
  # NO `my`/`our`/`local` re-declaration refusal any more (s415, task #401).
  # It existed because a positional rename of every `$y` in $root would merge
  # two different variables, but _rename_decl_within has been SHADOW-AWARE
  # since #254 B-ii and region-limited since #296-B2: it starts at the state
  # declaration, leaves a nested re-declaration's own scope alone, and stops
  # at a LATER declaration of the same name.  So the three orderings all come
  # out as perl reads them (probed s415: an unrelated `my $y` later in the
  # file, a `my $y` before the state decl, and an inner `my $y` inside a block
  # under it).  Keeping the refusal cost t/opbasic/concat.t its whole run
  # (248 rows) the moment `CORE::state` began normalising.
  return "brace-deref" if $root->content =~ /[\$\@\%]\{\s*\Q$bare\E\s*\}/;
  return undef;
}

# The last token of the init expression that starts after `$sym = ` — the
# sibling run up to a `;`, a low-precedence / statement-modifier word, or the
# end of the enclosing node (PPI keeps statements flat, so nested structures
# are single siblings).  A top-level `,` ends the init only in EXPRESSION
# position (`foo(state $x = 5, $y)`); in a statement-shaped decl the commas
# belong to a paren-less list operator's arguments and are part of the init
# (`state $c = \substr $tintin, $x, 1;`).
sub _state_init_end {
  my ($self, $sym, $stmt_shaped) = @_;
  # Only statement-modifier WORDS are matched here; `or`/`and`/`xor`/`,`/`=>`
  # are PPI Operators and come from the one shared below-assignment table
  # (#138 — this scan used to list them as words, which never matched).  In a
  # statement-shaped decl a depth-0 comma ends the init only when it is not a
  # parenless list operator's argument separator; in expression position it
  # always does (`foo(state $x = 5, $y)`).
  my %stop = map { $_ => 1 } qw(if unless while until for foreach when);
  my @toks;
  my $t = $sym->snext_sibling;                 # the `=`
  $t = $t && $t->snext_sibling;                # first init token
  while ($t) {
    last if $t->isa('PPI::Token::Structure') && $t->content eq ';';
    last if $t->isa('PPI::Token::Word') && $stop{ $t->content };
    push @toks, $t;
    $t = $t->snext_sibling;
  }
  if (defined(my $lp = _lowprec_idx(\@toks, 0))) {
    splice(@toks, $lp)
      if !$stmt_shaped || _lowprec_split_safe(\@toks, 0, $lp);
  }
  die "Parser2 TODO: empty state initializer\n" unless @toks;
  return $toks[-1];
}

sub _state_rewrite_routes {
  my ($self, $doc) = @_;
  my (@sites, %anon);
  for my $w (@{ $doc->find(sub { $_[1]->isa('PPI::Token::Word')
                 && $_[1]->content eq 'state' }) || [] }) {
    next unless _state_declarator_word($w);
    my $sym = $w->snext_sibling;
    next unless $sym && $sym->isa('PPI::Token::Symbol');
    my $route = $self->_state_route($w);
    next if $route->{route} eq 'skip' || $route->{route} eq 'classic';
    die "Parser2 TODO: non-scalar state declaration outside a named-sub statement: "
      . $sym->content . "\n" unless $sym->content =~ /^\$\w+$/;
    push @sites, [$w, $sym, $route];
  }
  return 0 unless @sites;
  for my $site (reverse @sites) {
    my ($w, $sym, $route) = @$site;
    my $root = _enclosing_block($w) // $doc;
    if (my $why = $self->_state_rw_blocker($root, $sym)) {
      die "Parser2 TODO: state " . $sym->content . " rewrite ($why)\n";
    }
    (my $bare = $sym->content) =~ s/^\$//;
    my $n    = $self->_state_disambig . $self->{_state_rename_counter}++;
    my $cell = "\$${bare}__state__${n}";
    my $flag = "${cell}__init";
    $self->_rename_decl_within($root, $sym, $cell);
    my $nx = $sym->snext_sibling;
    my $has_init = $nx && $nx->isa('PPI::Token::Operator') && $nx->content eq '=';
    if ($has_init) {
      my $stmt = $w->parent;
      my $stmt_shaped = $stmt && $stmt->isa('PPI::Statement::Variable')
        && do { my @sk = _strip_semi($stmt->schildren); @sk && $sk[0] == $w };
      my $end = $self->_state_init_end($sym, $stmt_shaped);
      $w->set_content("do { unless ($flag) { ");
      $end->insert_after(PPI::Token::Word->new(" ; $flag = 1 } $cell }"));
    } else {
      $w->delete;
    }
    if ($route->{route} eq 'anon') {
      my $a = $anon{ refaddr $route->{cv_block} } //=
        { subword => $route->{subword}, block => $route->{cv_block}, cells => [] };
      unshift @{ $a->{cells} }, $cell, ($has_init ? ($flag) : ());
    }
  }
  for my $a (values %anon) {
    my $fin = $a->{block}->finish
      or die "Parser2 TODO: unterminated anon sub in state rewrite\n";
    $a->{subword}->set_content(
      'do { ' . join('', map { "my $_; " } @{ $a->{cells} }) . 'sub');
    $fin->set_content('} }');
  }
  return 1;
}

# state in a NAMED sub (the common case): `state $x [= INIT];` at block level
# becomes a defvar'd package cell `$x__state__N` — one instance per named sub,
# which IS Perl's named-sub state semantics (named subs are single-instance).
# An initialized decl additionally gets a raw once-flag `$x__state__N__init`;
# the decl statement lowers to v1's exact guarded-init shape
# (`(unless FLAG (box-set CELL INIT) (setf FLAG t))` + the bare cell as the
# statement value).  Every post-decl use in the sub is token-renamed — the
# same machinery as the other rename families (_rename_decl_within), so the
# decl's own RHS still reads the OUTER variable (`state $x = $x`).
#
# Containers (`state @x [= LIST]`, `state %h [= LIST]`) take the same route:
# a _fresh_container defvar cell plus a guarded whole-assignment through the
# expression seam.
#
# The pass is AUTHORITATIVE for the segment: every declarator-shaped `state`
# token it sees is either renamed here, deliberately skipped (signatured
# named subs — v1 owns those definitions wholesale), or dies → v1.  Outside
# the subset:
#   - list state (`state ($x, $y)` — the initialized forms are invalid perl);
#   - the blocker set: a second my/state declaration of the name in the sub,
#     `${x}` brace-deref, string eval in the sub (the eval capture alist
#     finds lexicals BY SOURCE NAME — a renamed cell would be invisible to
#     eval'd code).
# Everything OUTSIDE the named-sub statement subset (anon subs, map/grep/sort
# blocks, file level, expression position) was already rewritten into plain
# Perl by _rewrite_state_prepass and never reaches this pass.
sub _rename_state_vars {
  my ($self, $seg) = @_;
  my @words;
  for my $top (@{ $seg->{stmts} }) {
    next unless ref $top && $top->isa('PPI::Node');
    push @words, @{ $top->find(sub {
      $_[1]->isa('PPI::Token::Word') && $_[1]->content eq 'state' }) || [] };
  }
  for my $w (@words) {
    # Non-declarator uses of the word (hash key `state =>`, method ->state,
    # `sub state`): same exclusions as the W2 eval walk.
    my $prev = $w->sprevious_sibling;
    next if $prev && $prev->isa('PPI::Token::Operator')
         && $prev->content =~ /^(?:->|=>)$/;
    next if $prev && $prev->isa('PPI::Token::Word') && $prev->content eq 'sub';
    my $nx = $w->snext_sibling;
    next unless $nx && ($nx->isa('PPI::Token::Symbol')
                     || $nx->isa('PPI::Structure::List'));
    # Signatured named sub (or a decl inside a signature default): v1 owns the
    # whole definition via _fallback_stmt — leave the tokens for its state
    # machinery (signatures.t t126/t127).
    my ($sanc, $insig) = ($w->parent, 0);
    while ($sanc && !$sanc->isa('PPI::Document')) {
      $insig = 1 if $sanc->isa('PPI::Structure::List') && $sanc->parent
                 && $sanc->parent->isa('PPI::Statement::Sub');
      last if $sanc->isa('PPI::Statement::Sub');
      $sanc = $sanc->parent;
    }
    next if $insig;
    # v1 owns SIGNATURED sub definitions, so their state decls are v1's
    # problem; a pure-prototype sub lowers natively (#126) — no exemption.
    next if $sanc && $sanc->isa('PPI::Statement::Sub')
         && !$sanc->isa('PPI::Statement::Scheduled') && $sanc->name
         && defined $self->_proto_or_sig_str($sanc)
         && !$self->_is_pure_prototype($sanc);
    my $stmt = $w->parent;
    die "Parser2 TODO: state outside a block-level declaration\n"
      unless $stmt && $stmt->isa('PPI::Statement::Variable')
          && $stmt->parent
          && ($stmt->parent->isa('PPI::Structure::Block')
              || $stmt->parent->isa('PPI::Document'));
    my ($anc, $sub) = ($stmt->parent, undef);
    while ($anc) {
      if ($anc->isa('PPI::Statement::Sub') && !$anc->isa('PPI::Statement::Scheduled')
          && $anc->name) { $sub = $anc; last }
      $anc = $anc->parent;
    }
    die "Parser2 TODO: state outside a named sub\n" unless $sub;
    my @k = _strip_semi($stmt->schildren);
    die "Parser2 TODO: unsupported state declaration shape: " . $stmt->content . "\n"
      unless @k >= 2 && $k[1]->isa('PPI::Token::Symbol')
          && $k[1]->content =~ /^[\$\@\%]\w+$/
          && (@k == 2
              || ($k[2]->isa('PPI::Token::Operator') && $k[2]->content eq '='));
    my $why = $k[1]->content =~ /^\$/
      ? $self->_shadow_rename_blocker($sub->block, $k[1], undef, 'shadow_ok')
      : $self->_state_container_blocker($sub->block, $k[1]);
    die "Parser2 TODO: state " . $k[1]->content . " in named sub ($why)\n" if $why;
    my $new = $k[1]->content . '__state__' . $self->_state_disambig
            . $self->{_state_rename_counter}++;
    $self->_rename_decl_within($sub->block, $k[1], $new);
    $self->{_state_renamed}{$new} = @k > 2 ? 'init' : 'plain';
    # Both symbols are defvar'd via _captured_decls at lowering time; the
    # marker keeps _forward_global_decls from emitting a competing defvar
    # (its box-shaped default would load FIRST and leave the flag truthy).
    $self->{_file_lex_renamed}{$new} = 1;
    $self->{_file_lex_renamed}{ $new . '__init' } = 1;
  }
}

# True when SYM is one of the names DECLARED by a plain `my`/`state`
# statement — `my $err;`, `my ($vobj, $err);` — as opposed to a use of the
# name.  The initialiser after `=` is a real use (`my $x = $err;`), so the
# scan stops there.  `our` deliberately does not qualify: it creates the
# package global.
#
# Residual (unchanged in kind by this test): a global reachable only through a
# NON-interpolating string (`eval '$err = 1'`) is invisible to both this and
# the interpolation check beside it — the same hole the pass has always had.
sub _is_lexical_decl_name {
  my ($sym) = @_;
  # Climb to the nearest enclosing Statement::Variable — NOT merely the nearest
  # Statement: in `my ($vobj, $err);` the name sits in the Expression inside the
  # parens, so stopping at the first Statement finds that and misses the
  # declaration.  Climbing past a block into an unrelated outer declaration is
  # harmless: the name is then in that one's initialiser, which the scan below
  # rejects.
  my $st = $sym->parent;
  $st = $st->parent while $st && !$st->isa('PPI::Statement::Variable');
  return 0 unless $st;
  my @k = $st->schildren;
  return 0 unless @k && $k[0]->isa('PPI::Token::Word')
                     && $k[0]->content =~ /^(?:my|state)$/;
  for my $k (@k) {
    last if $k->isa('PPI::Token::Operator') && $k->content eq '=';
    return 1 if $k == $sym;
    return 1 if $k->isa('PPI::Node') && $k->find_first(sub { $_[1] == $sym });
  }
  return 0;
}

# #296: a `my`/`state` declaring an EXCEPTION-partition name.
#
# Since the direction-D flip an ORDINARY global is a symbol macro over a cell,
# so `my $x` near one is a plain lexical `let` that shadows it.  An EXCEPTION
# name (Pl::GlobalPartition — $a/$b plus the runtime-owned set) is still a
# `defvar`, i.e. PROCLAIMED SPECIAL, so `(let (($a …)) …)` is a DYNAMIC
# rebinding: a closure made inside loses the value at scope exit, and a sub
# called from the scope sees it.  Measured on plain main:
#
#     sub mk { my $a = shift; return sub { $a } }
#     print mk("F")->(), mk("G")->();      # perl FG, PCL empty
#
# CL cannot lexically bind a proclaimed special, so no emission fixes this
# while the name stays `$a` — the declaration must get a different SYMBOL.
# And the partition must keep these names dynamic: the sort lowering binds the
# $a/$b pair (#287) and the runtime binds $_/@_/%ENV/… by name, so shrinking
# the partition instead would cost every sort CALL a p-local-cell (~41 ns,
# plan §2) and still leave `my %ENV` broken.
#
# THIS IS NOT THE POISONED-MY FAMILY (#291 deletes those three; this one
# stays).  There is no poison test and no analysis deciding WHETHER: the
# trigger is the NAME — one Pl::GlobalPartition call — and the only scope work
# is finding the declaration's own root, which perl states syntactically.
# Corpus census (perl-tests + lib + pack-impl, 133 files): 147 sites, every
# one `$a` or `$b`; 132 block-scoped, 12 file-level, 3 foreach loop variables.
#
# A blocked site is left untouched — exactly today's (broken) emission, never
# worse — because a partial rename would split one variable in two.
sub _rename_exception_mys {
  my ($self, $seg) = @_;
  for my $top (@{ $seg->{stmts} }) {
    next unless ref $top && $top->isa('PPI::Node');
    for my $d (_decl_syms_under($top, nested => 1, plain => 1)) {
      my ($w, $s) = @$d;
      next unless Pl::GlobalPartition::is_exception_global($s->content);
      my ($root, $decl) = _lexical_decl_scope($w, $s);
      next unless $root;
      # A FILE-level decl in a file whose string eval sits inside a NAMED SUB
      # is left to the capture/promotion machinery (task #296's second
      # reproducer: `my $a = "FILE"; sub g { eval '$a' }`).  A named sub is
      # HOISTED out of the file-level `let`, so the eval-site capture alist
      # cannot carry the lexical to it — only promotion to a package cell
      # reaches, and that pass finds the declaration by its PERL name in the
      # eval text.  Renaming first hides it and strands the decl in a let.
      # A file-level eval is not affected: the alist carries the name there
      # (do.t/qr.t emit `(cons "$a" $a__excl__0)`), so those keep the rename
      # — and a promoted cell is not a `let`, so this pass has nothing to fix
      # in the skipped case anyway.
      next if $root->isa('PPI::Document') && $self->{_str_eval_in_named_sub};
      my $why = $s->content =~ /^\$/
        ? $self->_shadow_rename_blocker($root, $s, 'eval_ok', 'shadow_ok')
        : $self->_state_container_blocker($root, $s, 'eval_ok');
      next if $why;
      $self->_rename_decl_within($root, $s,
        $s->content . '__excl__' . $self->{_excl_rename_counter}++, $decl);
    }
  }
  return;
}

# #296-B1, the eval-mode HALF of the same fact.  A string eval compiled inside
# a scope that declared `my $a` must read THAT lexical, and perl says so:
#
#     { my $a = "IN"; print eval q{"[$a]"} }        # perl: [IN]
#     my $cmp = eval q{sub { $a <=> $b }};          # no `my $a` in scope:
#     sort $cmp @list;                              #   sort's dynamic $a
#
# The two cases differ ONLY in whether a `my $a` was in scope at the eval site
# — which is exactly what the capture alist records, so `eval_captures`
# membership IS the perl rule, not a heuristic (ruled s386,
# fable-answers-s385.md §2a; the progv/dynamic-extent shapes were OVERRULED
# there — they lose the capture the moment the eval returns, which case 2 of
# that table, an escaping closure, measures).
#
# The rewrite: rename the eval body's free `$a` to a fresh symbol that is NOT
# proclaimed special, and keep "$a" as the p-eval-thunk's LOOKUP name.  The
# thunk then binds a plain lexical to the captured container, which is the
# ordinary `$x__shadow__N` capture path — read, write, and #295's pad chain
# come with it.  With no alist key nothing is renamed and the special path is
# untouched, so a comparator eval'd outside a `my $a` scope still reads sort's
# dynamic binding.
#
# Skipped when the eval body DECLARES the name itself: a `my $a` inside the
# string is its own variable, and if _rename_exception_mys could not rename it
# (a blocker fired) the remaining `$a` tokens are that declaration's, not the
# caller's.  Leaving them is today's emission — never worse.
sub _rename_free_eval_captures {
  my ($self, $doc) = @_;
  my @caps = grep { /^[\$\@\%]\w+$/ && Pl::GlobalPartition::is_exception_global($_) }
             @{ $self->eval_captures // [] };
  return unless @caps;
  my $never_shadowed = sub { 0 };
  for my $old (@caps) {
    # Own declaration in the eval body → not the caller's variable.  (A `my`
    # the decl pass above renamed is already off the bare name, so what is
    # left here is a decl it could not rename.)
    next if grep { $_->symbol eq $old && _is_lexical_decl_name($_) }
            @{ $doc->find('PPI::Token::Symbol') || [] };
    # No occurrence test: the use may be INTERPOLATED (`eval q{"[$a]"}` — the
    # commonest shape), which no Symbol-token scan sees.  A rename that hits
    # nothing costs one unused counter value and emits nothing.
    my $new = $old . '__evalcap__' . $self->{_evalcap_counter}++;
    (my $newbare = $new) =~ s/^[\$\@\%]//;
    my $interp_fix = _interp_fixer($old, $newbare);
    _rename_use_token($_, $old, $new, $interp_fix, $never_shadowed)
      for $doc->tokens;
    $self->{_evalcap_names}{$new} = $old;
  }
  return;
}

# The SCOPE of a `my`/`state` declaration, as perl states it syntactically:
# returns (ROOT, DECL) for _rename_decl_within — ROOT is the region whose
# tokens may be rewritten, DECL the sub-region evaluated in the OUTER scope
# (the initializer / the foreach list), which must not be.  DECL undef means
# "the declaring statement", which is _rename_decl_within's own default.
#
#   for my $x (LIST) {…}   construct-scoped; LIST runs outside it
#   if/while (my $x = …)   construct-scoped; the head runs in it, so DECL
#                          stays the declaring statement
#   for (…; my $x…; …)     ditto, via Structure::For
#   my $x inside a block   block-scoped (sub body, bare block, loop body)
#   my $x at file level    to end of document — the decl's own top node
sub _lexical_decl_scope {
  my ($w, $s) = @_;
  my $par = $w->parent;
  if ($par && $par->isa('PPI::Statement::Compound')) {   # foreach loop variable
    my ($list) = grep { $_->isa('PPI::Structure::List') } $par->schildren;
    return ($par, $list // $s);
  }
  for (my $p = $w->parent; $p; $p = $p->parent) {
    if ($p->isa('PPI::Structure::Condition') || $p->isa('PPI::Structure::For')) {
      my $c = $p->parent;
      return $c && $c->isa('PPI::Statement::Compound') ? ($c, undef) : ();
    }
    return ($p, undef) if $p->isa('PPI::Structure::Block');
  }
  return ($s->top, undef);
}

# The nearest enclosing SUB BODY of $el, if any: a NAMED sub definition, or
# the BLOCK of an anonymous `sub { … }` (with its prototype/attributes
# skipped, the same way _state_decl_route recognises one).  A Scheduled block
# — BEGIN/END — is neither: it has no call boundary, it runs once in place.
#
# WHY the body and not the NAME (#272, s372): the condition the embedded-`my`
# rename needs is "this declaration is inside SOME sub's body", because then
# no other sub can possibly see the lexical it declares — which is exactly
# why the file-level premise of the veto (another sub genuinely SHARES a
# file-level cell: Capture-Tiny's Utils.pm, #199) does not apply.  Whether
# that body has a name is irrelevant to the scope question, and keying on the
# name left the ANON spelling scope-blind: the decl fell back to the package
# global, so `my $anon = sub { ++my $x->{foo} }` read the global that another
# sub had written and died on it.
sub _enclosing_sub_body {
  my ($el) = @_;
  for (my $p = $el->parent; $p; $p = $p->parent) {
    return $p if $p->isa('PPI::Statement::Sub')
              && !$p->isa('PPI::Statement::Scheduled') && $p->name;
    next unless $p->isa('PPI::Structure::Block');
    my $w = $p->sprevious_sibling;
    $w = $w->sprevious_sibling
      while $w && ($w->isa('PPI::Token::Prototype')
                || $w->isa('PPI::Token::Attribute')
                || ($w->isa('PPI::Token::Operator') && $w->content eq ':'));
    return $p if $w && $w->isa('PPI::Token::Word') && $w->content eq 'sub';
  }
  return undef;
}

# Mark (in %$disq) every bare name that appears interpolated ($name / ${name})
# inside an interpolating token (double-quote, qq, backtick/qx, interpolating
# heredoc, or a regex match/substitution) anywhere under $node.  Single quotes,
# q(), qw(), and tr/// do not interpolate and are skipped.
sub _interp_names {
  my ($node, $disq, $sigils) = @_;
  $sigils //= '\$';   # default: scalar-sigil forms ($name, ${name}, $name[…])
  for my $t (@{ $node->find('PPI::Token') || [] }) {
    my $c;
    if ($t->isa('PPI::Token::HereDoc')) {
      next if Pl::PExpr::TokenUtils::heredoc_is_raw($t);  # #301: THE shared predicate
      $c = join '', $t->heredoc;
    } elsif ($t->isa('PPI::Token::Quote::Double')
          || $t->isa('PPI::Token::Quote::Interpolate')
          || $t->isa('PPI::Token::QuoteLike::Backtick')
          || $t->isa('PPI::Token::QuoteLike::Command')
          || $t->isa('PPI::Token::QuoteLike::Regexp')      # qr/$x/
          || $t->isa('PPI::Token::QuoteLike::Readline')    # <$fh>
          || $t->isa('PPI::Token::Regexp::Match')
          || $t->isa('PPI::Token::Regexp::Substitute')) {
      $c = $t->content;
    } else {
      next;
    }
    while ($c =~ /(?<!\\)[$sigils]\{?\s*([A-Za-z_]\w*)/g) { $disq->{$1} = 1 }
  }
  return;
}

# True when $node is lexically inside a NAMED sub's body (so a `my` there is
# sub-local, not a file lexical).
sub _inside_named_sub {
  my ($node) = @_;
  my $p = $node->parent;
  while ($p) {
    return 1 if $p->isa('PPI::Statement::Sub') && $p->name;
    $p = $p->parent;
  }
  return 0;
}

# The LHS names a `my`/`state`/`our`/`local` statement DECLARES (before `=`) —
# a bare Symbol or the Symbols of a parenthesised list.  Excludes RHS uses.
sub _declared_names {
  my ($self, $v) = @_;
  my @k = _strip_semi($v->schildren);
  return () unless @k >= 2 && $k[0]->isa('PPI::Token::Word');
  return $k[1]->content if $k[1]->isa('PPI::Token::Symbol');
  if ($k[1]->isa('PPI::Structure::List')) {
    return map { $_->content }
           grep { $_->isa('PPI::Token::Symbol') } map { $_->tokens } $k[1];
  }
  return ();
}

# v2 twin of v1's _insert_variable_forward_declarations, with the key v2
# difference: any name Parser2 let-binds is a TRUE lexical and must NOT be
# defvar'd (proclaiming it special would poison every let of that name —
# closures would capture the symbol, raw slots would break).  So: defvar
# exactly the referenced sigil-vars that are neither let-bound anywhere in
# the file nor runtime-owned.
# E3: state cells minted while transpiling an EVAL STRING need a
# disambiguating tag — the eval's counter restarts at 0, so a bare
# `$s__state__0` could collide with the ENCLOSING file's cell of the same
# name (whose __init flag is already set → the eval's initializer never
# runs; state.t #148/149).  The tag is a hash of the eval source, so it is
# deterministic (stable across the p-eval transpile cache) and unique per
# distinct eval string.  Empty outside eval mode — file emissions unchanged.
sub _state_disambig {
  my $self = shift;
  return '' if !$self->eval_mode;
  return $self->{_state_eval_tag} //= do {
    require Digest::MD5;
    my $src = $self->has_code ? $self->code : '';
    utf8::encode($src) if utf8::is_utf8($src);
    'e' . substr(Digest::MD5::md5_hex($src), 0, 8) . '_';
  };
}

# #55 interleave: merge a section's sub defs and BEGIN/END forms by SOURCE
# POSITION — perl compiles subs and runs BEGIN blocks in source order, so a
# BEGIN sees exactly the subs defined above it and none below (sub-existence
# introspection: chdir.t).  Index tie-break keeps the merge stable for
# entries with equal positions.
sub _interleaved_defs {
  my ($self, $sec) = @_;
  my @tagged;
  push @tagged, [$sec->{def_lines}[$_]   // 0, scalar(@tagged), $sec->{defs}[$_]]
    for 0 .. $#{ $sec->{defs} };
  push @tagged, [$sec->{sched_lines}[$_] // 0, scalar(@tagged), $sec->{sched}[$_]]
    for 0 .. $#{ $sec->{sched} };
  return map { $_->[2] }
         sort { $a->[0] <=> $b->[0] || $a->[1] <=> $b->[1] } @tagged;
}

# E3 eval-mode assembly (docs/v2-opus48-execution-plan.md §E3): one anonymous
# segment; HEAD = package pre-declares + declarations + $a/$b + the genuine
# global forward-decls + captured decls; BODY = defs/sched interleave + run.
# Free variables (AST scope scan of the source ∪ the text-scan candidates
# _forward_global_decls would have defvar'd) become the p-eval-thunk lambda's
# parameters — v1's exact wrapper shape (docs/eval-lexical-capture.md), so the
# runtime binds them to the caller's live containers via p-eval-lex-lookup.
sub _assemble_eval_mode {
  my ($self, $sec, $doc) = @_;
  my $fb = $self->fallback_parser;

  # Free vars, AST-scan half: scope-aware walk of the source (descends into
  # named subs — the modifier idiom's captures).  Reuses v1's engine.
  my %free = %{ $fb->_eval_free_vars_from_ppi($doc) };

  # Text-scan half: what _forward_global_decls would defvar.  In eval mode
  # those names go to %free instead (the defvar would proclaim them special
  # and defeat the lambda's lexical binding); cross-package + caret vars
  # still get their defvars here.
  my @fwd = $self->_forward_global_decls(
    join("\n", @{ $sec->{captured} }, @{ $sec->{defs} },
               @{ $sec->{sched} },    @{ $sec->{run} }),
    $sec->{pkg}, $sec->{seg_lex}, \%free);

  # Names the promotion machinery renamed to defvar'd cells are NOT free —
  # they resolve through the capture alist / alias rule (ir-spec §9.1).
  delete $free{$_} for keys %{ $self->{_file_lex_renamed} // {} };

  # M1 (s353): `\&name` where the sub is DEFINED in this eval (Sub::Quote's
  # `$$_UNQUOTED = \&NAME` inside the region) is not a caller capture — the
  # body already resolves it statically.  Leaving it free made the thunk
  # look "&name" up in the CALLER and bind an empty cell over the real sub.
  for my $sub (@{ $doc->find('PPI::Statement::Sub') || [] }) {
    next unless $sub->name && !$sub->isa('PPI::Statement::Scheduled');
    delete $free{ '&' . $sub->name };
  }

  # $a/$b: kept defvar'd (sort comparators need them special) but ALSO listed
  # as params when referenced, so a caller's lexical `my $a`/`my $b` is
  # captured; the param is then a dynamic rebinding (v1's rule).
  # A name #296-B1 renamed is NOT here — its tokens are `$a__evalcap__N` now,
  # so this find sees nothing and the capture takes the lexical param path
  # below instead (which is the whole point: a dynamic rebind cannot outlive
  # the eval, and perl's does).
  for my $ab ('$a', '$b') {
    $free{$ab} = 1
      if @{ $doc->find(sub {
             ref($_[1]) eq 'PPI::Token::Symbol' && $_[1]->content eq $ab;
           }) || [] };
  }

  my @head;
  # Pre-declare referenced/nested packages so qualified symbols are readable.
  my %pre;
  $pre{$_} = 1 for keys %{ $self->{_referenced_pkgs} };
  $pre{ $_->namespace // 'main' } = 1
    for @{ $doc->find('PPI::Statement::Package') || [] };
  $pre{$_} = 1 for @{ $self->environment->get_undeclared_packages() };
  delete @pre{qw(main pcl)};
  push @head, map { "(pcl:p-defpackage " . $fb->_cl_pkg_designator($_) . ")" }
              sort keys %pre;
  push @head, @{ $sec->{decls} };
  push @head, '(defvar $a (make-p-box nil))', '(defvar $b (make-p-box nil))';
  push @head, @fwd;
  push @head, @{ $sec->{captured} };

  # #226: a leading `package X;` must be in effect for EVERYTHING the region
  # emits — including the hoisted sub defs and the sched bucket's `use`
  # statements, whose import records the caller's package.
  my @body = (@{ $sec->{pkg_enter} // [] },
              $self->_interleaved_defs($sec), @{ $sec->{run} });

  # #295: some eval site in this body appended %p-eval-env% (see
  # _eval_lexical_alist) — bind it here, at body entry, to the alist the
  # enclosing p-eval is dynamically holding RIGHT NOW.  A lexical, not a
  # dynamic rebind: a named sub defined by this eval closes over it, so its
  # own eval sites still see the enclosing scope when called after this eval
  # has returned (eval.t's fred1/fred2).  One form, so it wraps the whole
  # body whether or not the thunk lambda is emitted around it.
  if ($fb->{_eval_env_used}) {
    @body = ('(let ((%p-eval-env% pcl:*p-eval-lex-alist*))', @body, ')');
  }

  my @out = ('(in-package :pcl)', '', grep { length } @head);
  push @out, '' if @head;
  my @names = sort keys %free;
  # #240 step 2 (RULED s349 §2c): a `package X; …` eval region runs with X as
  # the current package, so p-eval-thunk binds *package* to X around BOTH the
  # free-name resolution and the body — which is what makes an unqualified
  # global land in X (the lookup's miss path, %p-symref-box, p-use's import
  # target, p-bless's default class all ask *package* for exactly that).
  # The thunk is therefore emitted whenever a region package is present, even
  # with NO free names: the body is where the binding does most of its work,
  # and an empty parameter list is the only difference.  Measured s350
  # (docs/eval-region-measurements-s350.md §6): forcing the wrap over
  # Role-Tiny + Class-Method-Modifiers + Try-Tiny, 48 files, is row-identical
  # — the _cap_inlining_if_huge no-wrap rule is about FILE-mode top-level
  # forms (an eval-when that a later BEGIN must see at compile-file time), and
  # eval mode has no compile-file phase to lose.
  my $region_cl = $sec->{pkg_enter_cl};
  if (@names || defined $region_cl) {
    # The lambda PARAMETER is the name the body reads; the LOOKUP name is what
    # p-eval-lex-lookup asks the caller's alist for.  They differ for exactly
    # one family: a #296-B1 renamed exception capture, whose body symbol had
    # to become non-special (`$a__evalcap__0`) while the alist still keys the
    # caller's lexical under its perl spelling (`$a`).
    my $caps = $self->{_evalcap_names} // {};
    my $names_str = join(' ', map { '"' . ($caps->{$_} // $_) . '"' } @names);
    my $params    = join(' ', @names);
    push @out, "(pcl:p-eval-thunk (list $names_str)",
               " (lambda ($params)",
               @body,
               defined $region_cl ? " ) $region_cl)" : " ))";
  } else {
    push @out, @body;
  }
  return join("\n", @out);
}

# Blank the INNARDS of CL string literals and line comments in emitted text
# (delimiters and newlines kept) so the forward-decl scans below cannot match
# variable names inside DATA: an eval'd Perl source embedded as a string
# literal would otherwise defvar its own lexicals in the enclosing file —
# proclaiming them special and silently breaking closures the eval builds
# (found by E3: eval.t #39) — and a sprintf "%x" format minted a phantom %x
# (task #66).  Pipe symbols (|$;|, |${^MPE}|) pass through whole (they are
# code, and a raw `;`/`"` inside one must not start a comment/string);
# #\X char literals are skipped so #\" cannot toggle string state.
sub _blank_string_innards {
  my ($text) = @_;
  my @c = split //, $text;
  my ($in_str, $in_pipe) = (0, 0);
  for (my $i = 0; $i < @c; $i++) {
    my $ch = $c[$i];
    if ($in_str) {
      if ($ch eq '\\') {
        $c[$i] = ' ';
        if ($i + 1 < @c) { $c[$i+1] = ' ' if $c[$i+1] ne "\n"; $i++ }
        next;
      }
      if ($ch eq '"') { $in_str = 0; next }
      $c[$i] = ' ' if $ch ne "\n";
      next;
    }
    if ($in_pipe) {
      $in_pipe = 0 if $ch eq '|';
      next;
    }
    if ($ch eq '"') { $in_str = 1; next }
    if ($ch eq '|') { $in_pipe = 1; next }
    if ($ch eq '#' && $i + 1 < @c && $c[$i+1] eq '\\') { $i += 2; next }
    if ($ch eq ';') {
      while ($i < @c && $c[$i] ne "\n") { $c[$i] = ' '; $i++ }
      next;
    }
  }
  return join '', @c;
}

# $free_out (E3 eval-mode only): when given, the plain undeclared sigil-vars
# (%seen) are recorded there as p-eval-thunk capture candidates INSTEAD of
# being defvar'd — a defvar would proclaim the name special and defeat the
# thunk lambda's lexical binding (docs/eval-lexical-capture.md).  Cross-package
# and caret vars still get their defvars (they are genuine globals, never
# capturable lexicals).
sub _forward_global_decls {
  my ($self, $text, $pkg, $seg_lex, $free_out) = @_;
  $pkg //= 'main';
  $text = _blank_string_innards($text // '');
  # $a/$b are runtime-owned: the section head defvars the BARE pair
  # unconditionally, so a second defvar here would only be noise.  (This is a
  # different question from %PKG_SWITCH_IMMUNE_VARS' "can a package switch
  # re-home this name" — keep the two in step by cause, not by copy.  Since
  # #287 the sort lowering binds a REGION's qualified pair where one applies;
  # those symbols are defvar'd by the region's own entry forms, and being
  # qualified they never reach this unqualified list at all.)  @a/@b are NOT
  # runtime-owned — nothing defines them, so excluding them left `\@a` before
  # any assignment unbound at load (postfixderef.t; v1's list never had them).
  my %runtime_vars = map { $_ => 1 } qw($_ @_ %_args @ARGV @INC %ENV %INC %SIG
                                        $a $b);
  # Names let-bound in THIS section ($seg_lex, recorded by _reg_lex during the
  # section's lowering — NOT the now-scoped _let_bound_vars, which has shrunk
  # back to top-level scope by the time this assembly-phase pass runs).
  #
  # In FILE mode this set no longer excludes anything (#291, direction D).  The
  # exclusion existed because a `defvar` PROCLAIMS its symbol special, so
  # declaring a name the section also let-binds turned that `let` into a
  # dynamic rebinding — the whole poisoned-`my` rename family (`__shadow__`,
  # `__cond__`, `__emb__`) existed to dodge it by giving the LEXICAL a fresh
  # name so the GLOBAL could keep its declaration.  Since the flip an ordinary
  # global is a `p-defcell` symbol-macro, which a `let` of the same name simply
  # SHADOWS — declaration and lexical coexist, and the renames are gone.  The
  # cost is a dead cell for a name that is only ever lexical here; the
  # alternative (guessing which of the two roles a name plays from an
  # assembled-text scan) is what the renames were, and it was scope-blind three
  # times over (#205, #265, #272).
  #
  # In EVAL mode the set still excludes, because there this list is not
  # declarations at all: $free_out routes it to the p-eval-thunk's capture
  # PARAMETERS, bound from the CALLER's lexicals.  A name the eval region
  # declares itself is not a caller capture, and the flip says nothing about
  # that question.
  my $lb = $free_out ? ($seg_lex // {}) : {};
  my %skip_pkg = map { $_ => 1 } qw(ENV INC SIG pcl);
  my (%seen, %cross, %caret, %punct);
  for my $line (split /\n/, $text) {
    next if $line =~ /^\s*;;/;
    # A declaration line is not a USE — skip both spellings (a p-defcell line
    # that reached the scan would re-declare its own name, and for a renamed
    # cell that second declaration lands in the wrong bucket).
    next if $line =~ /^\s*\((?:defvar|p-defcell)\s/;
    # The punctuation array `@#` (bare `$#` magic + subscript lowers to
    # `(p-aref @# …)`) — the [A-Za-z_] scan below can't match it, same as the
    # caret specials.  A genuine global, never a capturable lexical, so it is
    # defvar'd even in eval mode (like %caret, unlike %seen).
    $punct{'@#'} = 1 if $line =~ /(?<![\w:|])\@\#(?!\w)/;
    # Caret specials (${^MPE}, ${^WARNING_BITS}, …) compile to the pipe-delimited
    # CL symbol |${^MPE}| — the [A-Za-z_] scan below can't match the `{^`.  They
    # are user-writable globals; defvar any that appear.  Keyed on the full
    # pipe-wrapped symbol; sigil (for container choice) is the char after `|`.
    $caret{$1} = 1 while $line =~ /(\|[\$\@\%]\{\^[A-Za-z_]\w*\}\|)/g;
    # (?<![\w:|]) skips pkg-qualified Foo::$x / |P|::$x; (?!-) skips runtime
    # internals like %pcl-cl-sub-name — with a POSSESSIVE \w*+ so the scan
    # cannot backtrack into a shorter match that dodges the lookahead
    # (`%pcl-str-buffer` used to shed two chars and defvar a phantom `%pc`).
    while ($line =~ /(?<![\w:|])([\$\@\%][A-Za-z_]\w*+)(?!-)/g) {
      my $v = $1;
      next if $runtime_vars{$v} || $lb->{$v};
      # W5-renamed cells are defvar'd via _captured_decls — don't double-declare.
      next if $self->{_file_lex_renamed}{$v};
      # `$x__lex__N` is v1's per-scope closure-capture RENAME (emitted by the
      # fallback's _with_declarations for a `my` captured by a nested sub, e.g.
      # in a map/grep block).  It is always a TRUE lexical, let-bound inside the
      # generated code — defvar'ing it would proclaim it special and collapse
      # every per-iteration let into one shared dynamic cell (map closures all
      # saw the LAST value).  Never forward-declare it.
      next if $v =~ /__lex__\d+$/;
      $seen{$v} = 1;
    }
    # Cross-package refs (main::$IS_ASCII from a required harness, Foo::@bar)
    # get a defvar in THEIR package — v1's %cross_pkg_vars behaviour.
    while ($line =~ /(?:\b([a-zA-Z_]\w*)|\|([^|]+)\|)::([\$\@\%][A-Za-z_]\w*+)(?!-)/g) {
      my ($pkg, $var) = (defined($1) ? $1 : "|$2|", $3);
      next if $skip_pkg{ defined($1) ? $1 : $2 };
      # Referenced packages must exist when the qualified symbol is READ —
      # parse() pre-declares them at the top of the file.
      $self->{_referenced_pkgs}{ defined($1) ? $1 : $2 } = 1;
      next if $pkg eq 'main' && $runtime_vars{$var};
      $cross{"$pkg\::$var"} = 1;
    }
  }
  if ($free_out) {
    $free_out->{$_} = 1 for keys %seen;
    %seen = ();
  }
  return () if !(%seen || %cross || %caret || %punct);
  # Every one of the four buckets goes through Pl::GlobalPartition, which
  # decides defvar-vs-p-defcell (task #289 direction D).  %punct and %caret are
  # all-exception by construction — routed anyway, so the partition stays the
  # single authority and a future caret name that IS word-shaped cannot drift.
  # ONE declaration per (package, name) per FILE (#281 item 2, s414).  These
  # blocks are emitted per SECTION, so a name referenced from several sections
  # of the SAME package was declared once per section — 16 such repeats in
  # sort.t, 4 in hash.t, 2 in closure.t.  Dropping a repeat is safe because the
  # survivor is an EARLIER top-level form and both declarers are define-once
  # (defvar; p-defcell via its boundp guard), so the later one was already a
  # no-op at load.
  #
  # The key is (package, name), NEVER the text.  A section emits its own
  # `in-package`, and `in-package` is READ-time per top-level form, so a bare
  # `$x` under package Foo and one under Bar are DIFFERENT symbols and both
  # declarations are needed — which is why sort.t's ten `(defvar $a …)` lines
  # are not duplicates at all (they are ten packages), and why a text-level
  # de-duplication here would be a silent wrong.  A name that carries its own
  # `::` is package-independent and keyed by itself.
  my @decls;
  my $emit = sub {
    my ($name, $init) = @_;
    my $key = $name =~ /::/ ? $name : "$pkg\0$name";
    return if $self->{_declared_globals}{$key}++;
    push @decls, global_decl_form($name, $init);
  };
  $emit->($_, _fresh_container($_))            for sort keys %seen;
  $emit->($_, _fresh_container($_))            for sort keys %punct;
  $emit->($_, _fresh_container(substr($_, 1))) for sort keys %caret;
  for my $qv (sort keys %cross) {
    (my $var = $qv) =~ s/^.*:://;
    $emit->($qv, _fresh_container($var));
  }
  return () unless @decls;
  return (';; Forward declarations for undeclared package globals', @decls);
}

# ---------------------------------------------------------------- subs

# The shared Environment's in_subroutine counter drives fallback-expression
# decisions (bare shift/pop default to @_ inside a sub, @ARGV at top level;
# my-var qualification; the top-level notinline guard) — v1 bumps it around
# sub bodies, so v2 must too.
sub _lower_sub {
  my ($self, $sub) = @_;
  my $env = $self->environment;
  $env->in_subroutine($env->in_subroutine + 1);
  my %saved_lex = %{ $self->{_live_lex} // {} };
  # Scope _let_bound_vars across the sub body too (W3): params + body lexicals
  # must not leak into a later call site's string-eval capture alist.
  # _eval_extra_captures likewise (M-F): a promoted cell declared inside the
  # sub body dies with it.
  my %saved_lb  = %{ $self->{_let_bound_vars} // {} };
  # A named sub is HOISTED outside the file's lexical `let`s, so an OUTER
  # let-bound name is unbound inside its body — it must not reach a body
  # eval's capture alist (eval.t recurse: the alist's `(cons "$curr_test"
  # $curr_test)` crashed unbound at call time; the eval never even named
  # it).  Start the body from an empty set: params + body lexicals
  # re-register during lowering; promoted/span cells are defvars — globally
  # bound — reachable via the span pairs and the alias rule (ir-spec §9.1).
  # A sub that genuinely REFERENCES an outer lexical is the capture family:
  # promoted or gated before lowering ever gets here.
  # EXCEPT the M5 block cells (s353): those carry no global alias (see the
  # _eval_block_cells branch), so a body eval can only reach them through
  # the per-site pair — and they are defvars, so the pair can never be the
  # unbound-at-call-time crash the wipe defends against.  A sub textually
  # inside the block closed over the cell; keep it.
  $self->{_let_bound_vars} =
    { map { ($_ => 1) }
      grep { $self->{_eval_block_cells} && $self->{_eval_block_cells}{$_} }
      keys %saved_lb };
  my $form = eval { $self->_lower_sub_inner($sub) };
  my $err = $@;
  $self->{_live_lex} = \%saved_lex;
  $self->{_let_bound_vars} = \%saved_lb;
  $env->in_subroutine($env->in_subroutine - 1);
  die $err if $err;
  return $form;
}

# After a nested `package X;` switch (D1/E1.5) the Environment's current
# package differs from the segment package whose section this output is read
# in — an unqualified sub name would intern in the SECTION's package, so
# qualify it against the Environment package first.  Top-level lowering has
# current == segment (via _set_cur_package), where this is the identity.
sub _sub_name_for_emission {
  my ($self, $name) = @_;
  my $cur = $self->environment->current_package // 'main';
  $name = "${cur}::${name}"
    if $name !~ /::|'/ && $cur ne ($self->cur_pkg // 'main');
  return $self->fallback_parser->_qualified_sub_to_cl($name);
}

# The Perl package an element sits in, honouring NESTED `package` statements
# (D1/E1.5): the innermost win is either a block-form `package X { … }`
# ancestor, or the nearest PRECEDING statement-form `package X;` sibling at
# any ancestor level (a statement-form switch scopes to the remainder of its
# enclosing block).  Used by the pre-pass, which runs before lowering and so
# cannot read the Environment's live package stack.
sub _effective_pkg {
  my ($self, $elem, $default) = @_;
  for (my $n = $elem; $n && !$n->isa('PPI::Document'); $n = $n->parent) {
    if ($n->isa('PPI::Structure::Block') && $n->parent
        && $n->parent->isa('PPI::Statement::Package')) {
      return $n->parent->namespace // $default;
    }
    for (my $p = $n->sprevious_sibling; $p; $p = $p->sprevious_sibling) {
      next unless $p->isa('PPI::Statement::Package');
      next if grep { $_->isa('PPI::Structure::Block') } $p->schildren;
      return $p->namespace // $default;
    }
  }
  return $default;
}

sub _lower_sub_inner {
  my ($self, $sub) = @_;
  my $clname = $self->_sub_name_for_emission($sub->name);
  my @stmts = $sub->block->schildren;

  my $params = $self->_extract_params($stmts[0]);
  my @body_stmts = @stmts;
  my $tail_param;
  shift @body_stmts if $params;
  if (!$params) {
    # W14: coalesce a contiguous LEADING run of `my $x = shift;` statements
    # into the same `my (LIST) = @_` fast path (the Nth bare shift binds
    # @_[N-1] = the Nth list slot).  _leading_shift_params owns the guards —
    # the rewrite is legal only when the remainder provably never observes
    # @_, because the shifts MUTATED it and the list assign does not.
    my ($sp, $n) = $self->_leading_shift_params(\@stmts);
    if ($sp) {
      $params = $sp;
      splice(@body_stmts, 0, $n);
      # The run consumed the WHOLE body (`sub f { my $x = shift; }`): the
      # tail decl's statement value — the last param — is the sub's return
      # value; an empty (block nil) would lose it (s307 $decl_tail family).
      $tail_param = $params->[-1] if !@body_stmts;
    }
  }
  # #377: a PROMOTED name (`…__file__N`) is not this sub's private lexical any
  # more — a nested named sub was hoisted out and reads it as a free special —
  # so its declaration has to publish a CELL.  Both param bindings below are
  # LEXICAL (p-raw-params binds the name raw; the boxed convention binds a
  # `let`), so a promoted param may take neither: put the declaration back into
  # the body, where _lower_block's _file_lex_renamed branch emits the
  # global_decl_form + the plain assignment.  Measured: `sub outer { my $x =
  # shift; my sub inner { $x * 2 } inner() }` crashed on an unbound
  # $x__file__0, while the same sub with one more use of @_ — which already
  # defeats the shift coalescing — ran and matched perl.  The sub then answers
  # like every other member of the "will not stay shared" family
  # (docs/not-supported.md): perl's plain-`sub` twin keeps the FIRST instance,
  # PCL's shared cell reads the last write; for the `my sub` spelling, which
  # is what this shape is written as, the two agree.
  if ($params && grep { $self->{_file_lex_renamed}{$_} } @$params) {
    $params     = undef;
    @body_stmts = @stmts;
    $tail_param = undef;
  }
  $self->_reg_lex(@{ $params // [] });

  if ($params) {
    my $rest_txt = join("\n", map { $_->content } @body_stmts);
    my $body_uses_args = $rest_txt =~ /\@_|\$_\[|\bshift\b|\bgoto\b|\bwantarray\b/;
    my $vi = Pl::VarAnnotator->analyze(\@body_stmts, $params, $self->_cur_sub_info, $self);
    if (!$body_uses_args && !grep { !$vi->{$_}{unboxable} } @$params) {
      # Signature fast path (#3): my ($a,$b) = @_ untouched afterwards, params
      # never written un-arithmetically / ref-taken → p-raw-params binds them
      # raw (no boxes, no p-list-=), with the callee-side Perl argument
      # flattening the uniform calling convention requires.  A plain &optional
      # lambda list here misbound every f(@args)/f(@_) call — the args vector
      # landed whole in the first param (task #80: Moo's Sub::Util shim,
      # reached through _Utils's _subname(@_) delegation).  All-scalar calls
      # take p-raw-params' no-allocation fast path.
      return ['p-sub', $clname, ['list', '&rest', '%_args'],
              ['p-raw-params', ['list', @$params],
                ['block', 'nil', $self->_lower_body_regime(\@body_stmts, $vi),
                  ($tail_param ? ($tail_param) : ())]]];
    }
    # Old convention with boxed params + synthesized list-assign binding.
    $vi->{$_} = { unboxable => 0 } for @$params;
    return ['p-sub', $clname, ['list', '&rest', '%_args'],
            ['p-args-body', ['block', 'nil',
              ['let', ['list', map { ['list', $_, '(make-p-box nil)'] } @$params],
                Pl::CLForm::ctx_bind('nil',
                  ['p-list-=', ['vector', @$params], '@_']),
                $self->_lower_body_regime(\@body_stmts, $vi),
                ($tail_param ? ($tail_param) : ())]]]];
  }

  my $vi = Pl::VarAnnotator->analyze(\@stmts, undef, $self->_cur_sub_info, $self);
  return ['p-sub', $clname, ['list', '&rest', '%_args'],
          ['p-args-body', ['block', 'nil', $self->_lower_body_regime(\@stmts, $vi)]]];
}

# Sub-body :void regime (task #60 — v1's wa_void_active model): bind
# *wantarray* to :void ONCE around the body instead of wrapping every
# void-position statement.  wa_void_active=1 tells every void emitter the
# ambient is already :void so it may skip its own bind (the seam's
# _ctx_wrap / funcall bind, _lower_stmt's g-match wrap); the
# tail statement restores the caller's context via *pcl-caller-wantarray*
# (bound at entry by the p-sub macro) — the leaf-level wrap in _lower_stmt.
# Without the hoist, SBCL's compiler exhausts the sweep's 1GB heap on large
# sub bodies (substr.t run_tests: 425 per-statement dynamic binds vs v1's
# 30).  A body that is a single non-compound statement has no non-tail
# statements at any depth, so the regime would be two pure-overhead dynamic
# binds per call (accessors!) — it keeps the direct per-expression scheme.
sub _lower_body_regime {
  my ($self, $stmts, $vi) = @_;
  my @live = grep { ref $_ && !$_->isa('PPI::Statement::Null') } @$stmts;
  if (@live == 0
      || (@live == 1 && !$live[0]->isa('PPI::Statement::Compound'))) {
    return $self->_lower_block($stmts, $vi, 'inherit');
  }
  local $self->environment->{wa_void_active} = 1;
  return Pl::CLForm::ctx_bind(':void',
          $self->_lower_block($stmts, $vi, 'inherit'));
}

# Register a `my`-declared name: for the fallback machinery's my-vs-package
# decisions (`_let_bound_vars`, never shrinks — mirrors v1's accumulator) and
# for the LIVE lexical-scope set (`_live_lex`, restored at scope exit by
# _lower_scope/_lower_sub) that the nested-sub capture check reads.
sub _reg_lex {
  my ($self, @names) = @_;
  for my $n (@names) {
    # Package-qualified names are never lexicals (`my $Foo::x` is a perl
    # compile error) — registering one (e.g. `for $main::x` loop var) makes
    # the string-eval capture alist emit the raw unreadable perl-order
    # symbol.  Globals are reachable inside eval by name anyway.
    next if $n =~ /::/;
    $self->{_let_bound_vars}{$n} = 1;
    $self->{_live_lex}{$n} = 1;
    # Per-section accumulator — drives the forward-decl exclusion (a name
    # let-bound in THIS section is never defvar'd by this section; other
    # sections decide for themselves — see the parse() _seg_lex comment).
    $self->{_seg_lex}{$n} = 1;
  }
  return;
}

# Lower a Structure::Block's statements: a lexical scope — names declared
# inside stop being "live" when it closes.
sub _lower_scope {
  my ($self, $stmts, $vi, $tail_ctx) = @_;
  # Same structural recursion as _lower_block (which already silences this):
  # the per-declaration re-scan recurses once per decl statement, so a long
  # block legitimately crosses perl's depth-100 report threshold (array.t).
  no warnings 'recursion';
  my %saved     = %{ $self->{_live_lex} // {} };
  # Scope _let_bound_vars too (W3): a name declared inside this block must not
  # leak into the string-eval capture alist at a call site AFTER the block
  # closes.  _seg_lex (cumulative within the section) still guards the
  # forward-decl pass.
  my %saved_lb  = %{ $self->{_let_bound_vars} // {} };
  # Push an Environment scope frame around the block so LEXICAL PRAGMAS set by a
  # fallback-emitted statement inside the block (`use integer` / `no integer`)
  # are saved and restored — v1 relies on push_scope/pop_scope for this, and
  # without it a nested `no integer` leaks out to the enclosing scope
  # (transpile-test-01 "no integer restores float division").
  $self->environment->push_scope;
  my @forms = $self->_lower_block($stmts, $vi, $tail_ctx);
  $self->environment->pop_scope;
  $self->{_live_lex} = \%saved;
  $self->{_let_bound_vars} = \%saved_lb;
  return @forms;
}

# A named sub nested inside a block hoists to the section's definitions
# bucket, OUTSIDE any lexical `let`s — safe only when its body provably
# references no lexical LIVE at this point (a real capture, e.g. the
# `{ my $x = 0; sub X::DESTROY { $x++ } }` static-variable idiom, must fall
# back to v1, whose defvar'd my-vars make the capture work).  Conservative
# text scan; over-firing only costs the v2 lowering, never correctness.
sub _hoist_nested_sub {
  my ($self, $sub) = @_;
  # Group the live lexicals by bare name; a sub-body use gates only when its
  # canonical symbol is actually live (and is not the sub's own shadow —
  # _block_captures_name).
  my %by_bare;
  for my $var (keys %{ $self->{_live_lex} // {} }) {
    (my $bare = $var) =~ s/^[\$\@\%]//;
    $by_bare{$bare}{$var} = 1;
  }
  for my $bare (sort keys %by_bare) {
    # W5's exemption, which this sibling scan was missing (task #347): a name
    # already PROMOTED to a package-level cell is legitimately captured — the
    # hoisted sub and the in-place code share the one defvar'd box, which is
    # the whole point of the promotion.  The identical `next` guards the
    # file-lexical scan in _check_sub_captures; without it here, the
    # promotion happened and the gate fired anyway.  Any sigil: container
    # promotions record '@x__file__N' / '%x__file__N', and a promotion may be
    # IDENTITY (the name keeps its spelling), so both are looked up.
    next if grep { $self->{_file_lex_renamed}{"$_$bare"} } '$', '@', '%';
    die "Parser2 TODO: lexical '$bare' possibly captured by nested sub " . $sub->name . "\n"
      if $self->_block_captures_name($sub->block, $bare, $by_bare{$bare});
  }
  push @{ $self->{_hoisted_decls} },
    ['p-declare-sub', $self->_sub_name_for_emission($sub->name)];
  push @{ $self->{_hoisted_defs} }, $self->_lower_sub($sub);
  push @{ $self->{_hoisted_def_lines} }, _src_pos($sub);
  return;
}

# Sortable source position of a statement (line major, column minor) — used
# by the #55 interleave to merge sub defs and scheduled blocks in source
# order.  Columns break same-line ties (`BEGIN { f() } sub f {…}`).
sub _src_pos {
  my ($stmt) = @_;
  my $line = $stmt->line_number   // 0;
  my $col  = $stmt->column_number // 0;
  return $line * 100000 + ($col < 100000 ? $col : 99999);
}

# `my ($a, $b) = @_;` → ['$a','$b'] | undef
#
# EXACT arity (task #138): the caller DELETES the whole statement, so a `>= 4`
# match silently deleted anything after `@_` — `my ($a) = @_, g();` lost the
# g() call entirely (perl runs it; assignment binds tighter than the comma).
# Anything longer falls through to ordinary statement lowering.
sub _extract_params {
  my ($self, $stmt) = @_;
  return undef unless $stmt && $stmt->isa('PPI::Statement::Variable');
  my @k = _strip_semi($stmt->schildren);
  return undef unless @k == 4
    && $k[0]->content eq 'my'
    && $k[1]->isa('PPI::Structure::List')
    && $k[2]->isa('PPI::Token::Operator') && $k[2]->content eq '='
    && $k[3]->isa('PPI::Token::Magic') && $k[3]->content eq '@_';
  my @params = map { $_->content }
               grep { $_->isa('PPI::Token::Symbol') }
               map { $_->tokens } $k[1];
  return undef unless @params && !grep { !/^\$\w+$/ } @params;
  return \@params;
}

# W14: detect a contiguous LEADING run of exactly `my $scalar = shift;` at the
# top of a sub body → (\@param_names, $run_length), or () when the rewrite is
# not provably safe.  Guards (all conservative — any doubt keeps today's
# p-args-body path):
#   - each statement is `my $x = shift;` with a BARE shift — no `shift @arr`,
#     no `shift()`, no `// $default`, no modifier, nothing else in the stmt;
#   - the names are distinct (a duplicate would be an illegal CL lambda list);
#   - the REMAINDER never observes @_: bare `shift` mutated @_ (dropped the
#     first element) where `my (LIST) = @_` does not, so any later @_ / $_[i]
#     / shift / goto (forwards @_) disqualifies (wantarray rides along in the
#     shared scan — harmless over-fire);
#   - the remainder has no string eval: eval'd code can read @_ (and captures
#     lexicals by name) invisibly to the text scan.  eval-BLOCKS are fine.
sub _leading_shift_params {
  my ($self, $stmts) = @_;
  my (@params, %seen);
  my $n = 0;
  for my $s (@$stmts) {
    last unless ref $s && $s->isa('PPI::Statement::Variable');
    my @k = _strip_semi($s->schildren);
    last unless @k == 4
      && $k[0]->isa('PPI::Token::Word')     && $k[0]->content eq 'my'
      && $k[1]->isa('PPI::Token::Symbol')   && $k[1]->content =~ /^\$\w+$/
      && $k[2]->isa('PPI::Token::Operator') && $k[2]->content eq '='
      && $k[3]->isa('PPI::Token::Word')     && $k[3]->content eq 'shift';
    return () if $seen{ $k[1]->content }++;
    push @params, $k[1]->content;
    $n++;
  }
  return () unless $n;
  my @rest = @$stmts[$n .. $#$stmts];
  my $rest_txt = join("\n", map { ref $_ ? $_->content : '' } @rest);
  return () if $rest_txt =~ /\@_|\$_\[|\bshift\b|\bgoto\b|\bwantarray\b/;
  for my $s (@rest) {
    next unless ref $s && $s->isa('PPI::Node');
    for my $w (@{ $s->find(sub { $_[1]->isa('PPI::Token::Word')
                                 && $_[1]->content eq 'eval' }) || [] }) {
      my $nx = $w->snext_sibling;
      return () unless $nx && $nx->isa('PPI::Structure::Block');
    }
  }
  return (\@params, $n);
}

# ---------------------------------------------------------------- blocks

# Lower a statement list to forms.  A `my` declaration NESTS the remainder of
# the block inside its `let` — scoping is a property of the tree, never of
# emitted text (the R3 fix).
#
# $tail_ctx: context of the block's LAST statement value.  A sub body passes
# 'inherit' (its tail is the sub's return value, so a tail call must see the
# CALLER's *wantarray*); everything else leaves it undef → statement position
# is void.  Threaded through `my`-let nesting and tail if/unless branches.
sub _lower_block {
  # One recursion level PER STATEMENT (each `my` nests the block remainder):
  # a few hundred top-level statements is normal, not runaway recursion.
  no warnings 'recursion';
  my ($self, $stmts, $vi, $tail_ctx) = @_;
  my @s = grep { ref $_ && !$_->isa('PPI::Statement::Null') } @$stmts;
  return () unless @s;

  # -- #63 forward `goto LABEL` to a standalone label among these statements:
  # a lexical `(go)` cannot reach a tag whose tagbody opens LATER, and Perl
  # allows the goto to sit inside a map/grep block (a pseudo-block the jump
  # may leave) — i.e. inside a LAMBDA here, where only a dynamic transfer
  # works.  Lowering: wrap the statements before the label in
  # (catch :pcl-goto-LBL …) falling through to the label point, and lower
  # every `goto LBL` in that prefix as (throw :pcl-goto-LBL nil) — the
  # `_catch_labels` registry on this object (ExprToCL's goto branch reads it
  # through the seam parser's lex_home) is `local`ized around the prefix
  # lowering so backward
  # gotos (in the tagbody remainder) keep the lexical (go).  Only the FIRST
  # standalone label of the list is considered (a later label's prefix
  # would cross this one's tagbody; the recursion inside the tagbody
  # re-scans).  The wrap is skipped — leaving the label case's forward-goto
  # gate as the safety net — when the prefix contains a declaration/local
  # statement (its let/save-restore scope must enclose the label, which the
  # catch would cut) or when the goto textually precedes such a statement.
  if (@s >= 2) {
    # ALL top-level standalone labels, in order.  The first drives the classic
    # single-label wraps below; the full list drives the #252 general wrap.
    my @labels;    # [index, name]
    for my $i (1 .. $#s) {
      next unless $s[$i]->isa('PPI::Statement::Compound');
      my @lk = $s[$i]->schildren;
      next unless @lk == 1 && $lk[0]->isa('PPI::Token::Label');
      (my $l = $lk[0]->content) =~ s/\s*:\s*$//;
      push @labels, [$i, $l];
    }
    my ($k, $lbl) = @labels ? @{ $labels[0] } : ();
    if (defined $k && $lbl =~ /^\w+$/) {
      my @prefix = @s[0 .. $k - 1];
      my $has_goto = grep { $_->content =~ /\bgoto\s+\Q$lbl\E\b/ } @prefix;
      # A goto in this prefix to a LATER label needs the #252 general wrap
      # below: the single-label wraps open no catch for that label, so its
      # goto would lower to a lexical (go) with no tag in scope.
      my $cross = 0;
      if (@labels > 1) {
        PFX: for my $p (@prefix) {
          for my $j (1 .. $#labels) {
            if ($p->content =~ /\bgoto\s+\Q$labels[$j][1]\E\b/) { $cross = 1; last PFX; }
          }
        }
      }
      my $scope_stmt = grep {
        $_->isa('PPI::Statement::Variable') || $self->_is_local_stmt($_)
      } @prefix;
      if (!$cross && $has_goto && !$scope_stmt) {
        my $tag = ':pcl-goto-' . $lbl;
        my @pre_forms = do {
          local $self->{_catch_labels}{$lbl} = $tag;
          $self->_lower_block(\@prefix, $vi, undef);
        };
        $self->{_goto_caught}{ refaddr($s[$k]) } = 1;
        return (['catch', $tag, @pre_forms],
                $self->_lower_block([@s[$k .. $#s]], $vi, $tail_ctx));
      }
      # Decl-hoist variant (#126, t/test.pl watchdog): the prefix DOES contain
      # declarations, whose let scope must survive past the label (the plain
      # catch-wrap would cut it).  When every one of them is a simple
      # single-scalar `my $x [= INIT];` (no local, no modifiers, no self-init,
      # no re-declaration), pre-bind them all as nil boxes in ONE let around
      # both the catch and the label point — v1's flat-decl model, structured —
      # and lower each decl in place as its bare assignment (the
      # _goto_hoisted flag, consulted by the Statement::Variable branch).  A
      # goto that jumps over a hoisted decl leaves its box nil = undef,
      # perl's jumped-over-my behaviour.  Anything outside the subset keeps
      # the standalone-label forward-goto gate as the safety net.
      elsif (!$cross && $has_goto) {
        my (%seen, @hoist);
        my $ok = 1;
        for my $st (@prefix) {
          next unless $st->isa('PPI::Statement::Variable')
                   || $self->_is_local_stmt($st);
          if ($self->_is_local_stmt($st)) { $ok = 0; last }
          my $kw = $st->schild(0);
          if (!$kw || $kw->content ne 'my') { $ok = 0; last }
          my ($nm, $init, $dmc) = $self->_single_scalar_decl($st);
          if (!$nm || $dmc || $seen{$nm}++) { $ok = 0; last }
          # a promoted file lexical is a DEFVAR — a let binding here would
          # dynamically rebind the special; leave it to its own branch
          if ($self->{_file_lex_renamed}{$nm}) { $ok = 0; last }
          if (defined $init) {
            my (undef, $imod) = _split_modifier($init);
            if (defined $imod) { $ok = 0; last }
            if (join('', map { $_->content } @$init) =~ _reads_name_rx($nm)) { $ok = 0; last }
          }
          push @hoist, $nm;
        }
        if ($ok && @hoist) {
          my $tag = ':pcl-goto-' . $lbl;
          my @pre_forms = do {
            local $self->{_catch_labels}{$lbl} = $tag;
            local $self->{_goto_hoisted} = { map { $_ => 1 } @hoist };
            $self->_lower_block(\@prefix, $vi, undef);
          };
          $self->{_goto_caught}{ refaddr($s[$k]) } = 1;
          my $vi2 = { %$vi, map { $_ => { unboxable => 0 } } @hoist };
          return (['let',
                   ['list', map { ['list', $_, '(make-p-box nil)'] } @hoist],
                   ['catch', $tag, @pre_forms],
                   $self->_lower_block([@s[$k .. $#s]], $vi2, $tail_ctx)]);
        }
      }
      # -- #252 GENERAL forward-goto shape (Text::Balanced's _match_tagged):
      # several standalone labels and gotos that CROSS an intervening label.
      # Preconditions: label names distinct, every goto to every label is
      # strictly FORWARD (textually before its label statement), and NO
      # top-level declaration/local before the LAST label.  Declarations do
      # not defeat the branch — a leading `my`/`local` is consumed by its
      # ordinary statement branch, which NESTS the block remainder (labels
      # included) inside its let/save-restore and recurses back here, so the
      # declaration's scope encloses every segment and this branch fires at
      # the level where only code remains.  (A decl BETWEEN labels would need
      # cross-catch hoisting — rare, still gated below; a goto that jumps
      # over a declaration keeps the classic #126 single-label hoist.)
      # Lowering: nest catches so the catch for EACH label encloses
      # everything before that label:
      #   (catch :L3 (catch :L2 (catch :L1 P0…) P1…) P2…) P3…
      # A (throw :Li) from any depth in any earlier segment unwinds to Li's
      # catch, whose next sibling form is Li's segment — exactly perl's jump
      # — and normal completion falls through the segments in source order.
      # The block value is the LAST segment's tail.  Backward or mixed gotos
      # never fire this branch; they keep the classic tagbody machinery and
      # its forward-goto gate as the safety net.
      if (@labels && !grep { $_->[1] !~ /^\w+$/ } @labels) {
        my %ln;
        my $dup = grep { $ln{ $_->[1] }++ } @labels;
        my ($backward, $any_goto) = (0, 0);
        LBL: for my $L (@labels) {
          my ($idx, $name) = @$L;
          for my $j (0 .. $#s) {
            next if $j == $idx;
            next unless $s[$j]->content =~ /\bgoto\s+\Q$name\E\b/;
            if ($j > $idx) { $backward = 1; last LBL; }
            $any_goto = 1;
          }
        }
        my $decl_stmt = grep {
          $_->isa('PPI::Statement::Variable') || $self->_is_local_stmt($_)
        } @s[0 .. $labels[-1][0] - 1];
        if (!$dup && !$backward && $any_goto && !$decl_stmt) {
          my @tags = map { ':pcl-goto-' . $_->[1] } @labels;
          my (@segs, $start);
          $start = 0;
          for my $L (@labels) {
            push @segs, [ @s[$start .. $L->[0] - 1] ];
            $start = $L->[0] + 1;
          }
          push @segs, [ @s[$start .. $#s] ];
          my ($wrap, @tail_forms);
          {
            local @{ $self->{_catch_labels} }{ map { $_->[1] } @labels } = @tags;
            $wrap = ['catch', $tags[0], $self->_lower_block($segs[0], $vi, undef)];
            $wrap = ['catch', $tags[$_], $wrap,
                     $self->_lower_block($segs[$_], $vi, undef)]
              for 1 .. $#labels;
            @tail_forms = $self->_lower_block($segs[-1], $vi, $tail_ctx);
          }
          return ($wrap, @tail_forms);
        }
      }
    }
  }

  my ($first, @rest) = @s;
  my $first_tail = @rest ? undef : $tail_ctx;

  # #364: publish the perl features this statement's string evals inherit, for
  # the (p-eval …) emission in ExprToCL.  HERE rather than in _lower_stmt
  # because the declaration paths below (`my $r = eval "…"` — the commonest
  # eval statement there is) never reach it.  Set unconditionally, so an eval
  # can only ever see its OWN site's answer.
  $self->fallback_parser->lex_home->{_eval_site_features} =
    $self->{_eval_features_by_stmt}{ refaddr $first };

  # -- local …;  → v1's local machinery via the fallback seam; the opened
  # save/restore scope wraps the lowered block remainder (see _lower_local).
  # Standalone `delete local $h{k};` is a plain PPI::Statement with the same
  # scope-to-block-end behaviour — same route.
  if ($self->_is_local_stmt($first)) {
    return $self->_lower_local($first, \@rest, $vi, $tail_ctx);
  }

  # -- my $x [= INIT];  → (let (($x …)) rest...)
  if ($first->isa('PPI::Statement::Variable')) {
    # A declaration in TAIL position (last statement of a value-position
    # block: sub body, embedded map/grep/sort/eval block, do{}, eval-mode)
    # must yield its statement value — perl's `sub f { my $x = 5 }` returns
    # 5.  Branches whose last form is the assignment (p-my-=/p-array-=/…
    # return the place) are already correct; the ones that bury the init in
    # the let BINDING (unboxable, self-init) or have no init append the
    # declared variable itself.  File top-level passes tail_ctx=undef, so
    # statement-position bytes are untouched.
    my $decl_tail = !@rest && defined $tail_ctx;
    # -- state $x [= INIT];  (renamed to $x__state__N by _rename_state_vars)
    #    → hoisted cell defvar (+ raw once-flag defvar) and v1's exact
    #    guarded-init shape in place; the bare cell is the statement value
    #    (`sub { state $n }` returns undef via the nil box).
    my @sk = _strip_semi($first->schildren);
    if (@sk >= 2 && $sk[0]->isa('PPI::Token::Word') && $sk[0]->content eq 'state') {
      my $name = $sk[1]->content;
      my $kind = $self->{_state_renamed}{$name}
        or die "Parser2 TODO: unrenamed state declaration: " . $first->content . "\n";
      push @{ $self->{_captured_decls} },
        global_decl_form($name, _fresh_container($name));
      my @forms;
      if ($kind eq 'init') {
        my $flag = $name . '__init';
        push @{ $self->{_captured_decls} }, global_decl_form($flag, 'nil');
        # Scalar: guarded box-set.  Container (@x/%x): the whole assignment
        # `@x__state__N = LIST` through the expression seam — v1 emits the
        # p-array-= / p-hash-= form (same path the my-container branch uses).
        #
        # #138: only the ASSIGNMENT is once-guarded, so a below-assignment
        # tail (`state $s = 1, f();`) must sit OUTSIDE the guard and run on
        # every call.  Unlike the `my` decl, this one cannot hand the whole
        # run to PExpr — the guard has to be interposed between head and tail
        # — so it splits, and _lowprec_split_safe decides whether the comma is
        # ours or a parenless list operator's.
        # When the comma is AMBIGUOUS (a parenless list operator may own it —
        # `state $c = \substr $tintin, $x, 1`, state.t) the split is declined
        # and the whole run stays the initializer, exactly as before: that is
        # right whenever the operator really does own the comma, which is the
        # only shape seen in practice.  The residual (`state $s = f 1, $t = 2`)
        # needs PExpr's arity knowledge to resolve and is recorded in #138.
        my ($lp, $tail_op, @tail_toks);
        if ($name =~ /^\$/
            && defined($lp = _lowprec_idx(\@sk, 3))
            && _lowprec_split_safe(\@sk, 3, $lp)) {
          $tail_op   = $sk[$lp]->content;
          @tail_toks = @sk[$lp + 1 .. $#sk];
        } else {
          undef $lp;
        }
        my $init_end = defined $lp ? $lp - 1 : $#sk;
        my $assign = $name =~ /^\$/
          ? ['box-set', $name, $self->_lower_expr([@sk[3 .. $init_end]], $first)]
          : $self->_lower_expr([@sk[1 .. $#sk]], $first);
        push @forms, ['unless', $flag, $assign, ['setf', $flag, 't']];
        if (defined $tail_op) {
          # `,`/`=>`: the tail is a separate statement — the cell is the head's
          # value, the tail runs unconditionally and supplies the statement
          # value.  `or`/`and`/`xor`: the tail is CONDITIONAL on the cell's
          # current value (perl re-tests it on every call, init or not).
          my $tail = $self->_lower_expr(\@tail_toks, $first,
                                        ($decl_tail ? () : ':void'));
          if ($tail_op =~ /^(?:or|and|xor)$/) {
            return (['p-' . $tail_op, ['progn', @forms, $name], $tail],
                    $self->_lower_block(\@rest, $vi, $tail_ctx));
          }
          # In TAIL position the comma expression is the sub's return value,
          # and a comma yields BOTH operands in list context — the same shape
          # PExpr emits for a bare `A, B` tail statement.
          if ($decl_tail) {
            return (['if', '*wantarray*',
                     ['vector', ['progn', @forms, $name], $tail],
                     ['progn', @forms, $name, $tail]],
                    $self->_lower_block(\@rest, $vi, $tail_ctx));
          }
          return (@forms, $tail, $self->_lower_block(\@rest, $vi, $tail_ctx));
        }
      }
      push @forms, $name;
      return (@forms, $self->_lower_block(\@rest, $vi, $tail_ctx));
    }
    my $our = $self->_lower_our_decl($first);
    if ($our) {
      # A no-init `our` in TAIL position still has a value in perl — the
      # declared variable, read in the tail's context (probed: `our $A` → 5,
      # `our ($C,$D)` → 2, `our @E` → 2, `our %H` → 1).  _lower_our_decl
      # contributes no form for that shape (the defvar is a declaration), so
      # append the read.  Statement position appends nothing, so emission
      # outside an eval tail is byte-identical.  (Task #227: v1 answered this
      # shape with the emitted variable NAME — a silent-wrong.)
      my @ok = _strip_semi($first->schildren);
      push @$our, $self->_lower_expr([@ok[1 .. $#ok]], $first, 'inherit')
        if $decl_tail && @ok == 2;
      return (@$our, $self->_lower_block(\@rest, $vi, $tail_ctx));
    }
    my ($name, $init, $declmod_cond) = $self->_single_scalar_decl($first);
    # Decl-level modifier (`my $x if @_;`): evaluate the condition for its
    # side effects, in void, BEFORE the let (the outer $x is what it sees).
    my @declmod_eval = $declmod_cond
      ? ($self->_lower_expr($declmod_cond, $first, ':void')) : ();
    # -- `my $x = A, B;` / `my $x = A or B;` (task #138).  Assignment binds
    # TIGHTER than `,`/`=>`/`or`/`and`/`xor`, so the tail is NOT part of the
    # init — perl runs `(my $x = A), B`, leaving $x = A.  Splitting the tokens
    # here would mean re-deciding the parenless list-operator ambiguity
    # (`my $c = h 1, 2` DOES pass both args to h), and that knowledge lives in
    # PExpr — so hand the WHOLE `$x = …` run to the expression machinery, the
    # same move the `$x = RHS` statement fast path makes via
    # _tail_below_assign_prec, and the same shape the `my $x <non-'='
    # trailing>` branch below already uses.  The declaration then contributes
    # only the (boxed) binding.  $x stays BOXED in the remainder: a comma/or
    # write is not a native-root arithmetic event, so no raw slot is lost.
    my ($lowprec_run, $selfref_run);
    if (defined $init && defined _lowprec_idx($init, 0)
        && !(_split_modifier($init))[1]) {
      # Self-referencing init (`my $x = $x, …`): the run is lowered INSIDE the
      # fresh let below, where $x already denotes the new nil box, so the RHS
      # would read the binding instead of the shadowed outer one.  Flag it —
      # the `if ($name)` branch below re-routes the shapes it can (#298) and
      # refuses the rest.  (Unlike the $self_init text scan further down, where
      # an over-fire is harmless, a false positive here costs the whole file,
      # so NON-INTERPOLATING literals are excluded: sprintf2.t's
      # `my $s = sprintf '%*2$s', "abc", $i` has `$s` inside a single-quoted
      # format, and perl does not read it.)
      $selfref_run = $self->_init_reads_scalar($init, $name);
      # The two paths that return BEFORE that branch bind no `let` of their
      # own (a promoted cell / a goto-hoisted box), so the re-route has nothing
      # to move the init into: they keep the refusal.
      die "Parser2 TODO: self-referential my-init with a below-assignment tail: "
        . $first->content . "\n"
        if $selfref_run
        && ($self->{_file_lex_renamed}{$name}
            || ($self->{_goto_hoisted} && $self->{_goto_hoisted}{$name}));
      my @kd = _strip_semi($first->schildren);
      $lowprec_run = [@kd[1 .. $#kd]];
    }
    if ($name && $self->{_file_lex_renamed}{$name}) {
      # W5: a captured file lexical, rewritten to a fresh package-level name —
      # lower as `our` does: a defvar'd box hoisted to the section top (so the
      # hoisted named sub that captures it sees the same special symbol) plus a
      # plain package-var assignment in place.  No let, not let-bound.
      push @{ $self->{_captured_decls} },
        global_decl_form($name, '(make-p-box nil)');
      # Register AFTER the assignment: the decl's own RHS (incl. an eval in
      # it) still resolves the original name to the OUTER variable.
      my @reg;
      if ($self->{_eval_block_cells} && $self->{_eval_block_cells}{$name}) {
        # M5 (s353), enclosing-outer shape: NO global alias — a later
        # p-alias-eval-cell would clobber the OUTER promotion's alias for
        # the same original name, and post-block evals must keep resolving
        # the outer cell.  Instead the cell joins the eval-site capture
        # alists under its ORIGINAL name (the alist builder strips
        # __file__N, innermost-first), scoped to this block by the
        # enclosing save/restore of _let_bound_vars.  Deliberately NOT
        # _reg_lex: _live_lex would re-trip the nested-sub hoist gate, and
        # _seg_lex is unneeded (_forward_global_decls already skips
        # _file_lex_renamed names).
        $self->{_let_bound_vars}{$name} = 1;
        delete $self->{_pending_eval_caps}{$name} if $self->{_pending_eval_caps};
      } else {
        @reg = $self->{_file_has_str_eval} ? $self->_reg_eval_capture($name) : ();
      }
      return (@declmod_eval,
              ($lowprec_run
                ? ($self->_lower_expr($lowprec_run, $first, ':void'))
                : defined $init
                  ? (['p-scalar-=', $name, $self->_lower_expr($init, $first)]) : ()),
              @reg,
              $self->_lower_block(\@rest, $vi, $tail_ctx),
              ($decl_tail ? ($name) : ()));
    }
    if ($name && $self->{_goto_hoisted} && $self->{_goto_hoisted}{$name}) {
      # #126 decl-hoist: the binding was pre-opened by the forward-goto
      # catch-wrap's outer let (a nil box) — emit only the assignment here.
      # The name stays BOXED for the whole remainder (the binding cannot
      # carry a raw verdict); the wrap's selection already refused
      # modifiers, self-init, and re-declaration.
      $self->_reg_lex($name);
      my $vi2 = { %$vi, $name => { unboxable => 0 } };
      return (@declmod_eval,
              ($lowprec_run
                ? ($self->_lower_expr($lowprec_run, $first, ':void'))
                : defined $init
                  ? (['p-my-=', $name, $self->_lower_expr($init, $first)]) : ()),
              $self->_lower_block(\@rest, $vi2, $tail_ctx));
    }
    if ($name) {
      # A postfix modifier on the init (`my $c = shift if @_ > 1`): Perl declares
      # $c ALWAYS and makes the ASSIGNMENT conditional (undef when the cond is
      # false; re-bound fresh each call, which a fresh boxed let already gives).
      # Split the modifier off the init; while/until/for/foreach → whole-stmt v1.
      my ($imod, $icond);
      if (defined $init) {
        (my $iexpr, $imod, $icond) = _split_modifier($init);
        $init = @$iexpr ? $iexpr : undef if defined $imod;
      }
      if ($imod && _modifier_needs_fallback($imod)) {
        return ($self->_fallback_stmt($first), $self->_lower_block(\@rest, $vi, $tail_ctx));
      }
      # Self-referencing init (`my $i = $i;` / `my $s = "$s-x";`): the RHS must
      # read the OUTER (shadowed) variable.  The boxed emission runs p-my-=
      # INSIDE the new let, where $i already refers to the fresh nil box — so
      # move the init into the let BINDING (a CL let init-form is evaluated in
      # the OUTER environment) via (p-box-init INIT).  Lower the init BEFORE
      # _reg_lex so the fallback sees the outer scope's registration state.
      # Text-scan over-fire is harmless: p-box-init is semantically identical
      # to (make-p-box nil)+(p-my-=).  (The unboxable raw-slot path below is
      # already correct — its init sits in the let binding.)  Combined with a
      # modifier, `my $x = $x if C` is perl-undefined behaviour — not handled.
      my $self_init;
      if (defined $init && !$imod
          && join('', map { $_->content } @$init) =~ /\Q$name\E\b/) {
        $self_init = ['p-box-init', $self->_lower_expr($init, $first)];
      }
      # #298: a self-referential init whose only depth-0 low-prec token is a
      # LIST-OPERATOR argument separator (`my $c = bless $c, "C3"`) has no
      # statement tail at all — and PExpr, which owns that ambiguity, is the
      # only thing that can say so.  Ask it: lower the run HERE, still in the
      # outer scope (before _reg_lex), so every `$c` in it denotes the
      # SHADOWED variable perl reads.  If the answer is exactly one assignment
      # to $name, there was no tail, and its RHS goes into the p-box-init let
      # binding — the same shape the no-tail path above builds.  Anything else
      # is a genuine `my $x = A, B` whose tail must run inside the new
      # binding, which this shape cannot express: refuse, as before.
      # (Lowered exactly once either way — PExpr's cleanup mutates the shared
      # tokens destructively, so a speculative re-lowering is not available.)
      if ($lowprec_run && $selfref_run) {
        my $rhs = _sole_assign_rhs($self->_lower_expr($lowprec_run, $first), $name);
        die "Parser2 TODO: self-referential my-init with a below-assignment tail: "
          . $first->content . "\n" if !defined $rhs;
        $self_init = ['p-box-init', $rhs];
        $lowprec_run = undef;
      }
      $self->_reg_lex($name);
      # #138: the whole `$x = A, B` run as one expression inside the fresh
      # boxed binding — see the $lowprec_run note above.  In tail position the
      # expression IS the sub's return value and must see the CALLER's context
      # ('inherit'): `sub f { my $t = 1, $u = 2 }` returns 2 in scalar context
      # but the LIST (1, 2) in list context, because the comma operator does.
      if ($lowprec_run) {
        my $vi2 = { %$vi, $name => { unboxable => 0 } };
        return (@declmod_eval,
                ['let', ['list', ['list', $name, '(make-p-box nil)']],
                 $self->_lower_expr($lowprec_run, $first,
                                    ($decl_tail ? 'inherit' : ':void')),
                 $self->_lower_block(\@rest, $vi2, $tail_ctx)]);
      }
      # A conditional init MUST keep $c boxed (the assignment writes through the
      # box; a false cond leaves it undef) — never the unboxable raw-slot path.
      if (!$imod && $vi->{$name} && $vi->{$name}{unboxable}) {
        my $initform = defined $init ? $self->_lower_expr($init, $first) : '(p-undef)';
        $initform = _wrap_freeze($vi->{$name}, $name, $initform);
        return (@declmod_eval,
                ['let', ['list', ['list', $name, $initform]],
                 $self->_lower_block(\@rest, $vi, $tail_ctx),
                 ($decl_tail ? ($name) : ())]);
      }
      if ($self_init) {
        return (['let', ['list', ['list', $name, $self_init]],
                 $self->_lower_block(\@rest, $vi, $tail_ctx),
                 ($decl_tail ? ($name) : ())]);
      }
      my @assign;
      if (defined $init) {
        my $set = ['p-my-=', $name, $self->_lower_expr($init, $first)];
        if ($imod) {
          my $cf = $self->_lower_expr($icond, $first);
          $cf = ['p-!', $cf] if $imod eq 'unless';
          @assign = (['p-if', $cf, $set]);
        } else {
          @assign = ($set);
        }
      }
      return (@declmod_eval,
              ['let', ['list', ['list', $name, '(make-p-box nil)']],
               @assign,
               $self->_lower_block(\@rest, $vi, $tail_ctx),
               (($decl_tail && !@assign) ? ($name) : ())]);
    }
    # -- my VAR <non-'=' trailing>;  (`my $aa, $bb, $cc;` / `my $a . $foo;` /
    #    `my @raw, @upgraded, @utf8;`)
    #    Perl declares ONLY the first variable and evaluates the rest as an
    #    ordinary (void) expression whose first operand is the fresh lvalue; the
    #    other names are package vars (it warns "Parenthesize").  Lower as a
    #    `my VAR` let + the whole `VAR <trailing>` expression discarded.  Keep a
    #    SCALAR BOXED in the remainder (a later `$x = …` must not hit the setf
    #    raw-slot path — VarAnnotator may have marked it unboxable); a container
    #    binds the same fresh container `my @a;` alone binds.
    #    The container spelling was a `Parser2 TODO: unsupported declaration`
    #    refusal until s393/#314 — one predicate too narrow, and it was
    #    opbasic/cmp.t's whole file (12078 rows) in the companion suite.
    my @kd = _strip_semi($first->schildren);
    if (defined(my $lead = _lead_decl_with_expr_tail($first))) {
      my $sname = $lead;
      $self->_reg_lex($sname);
      my $vi2 = { %$vi, $sname => { unboxable => 0 } };
      return (['let', ['list', ['list', $sname, _fresh_container($sname)]],
               $self->_lower_expr([@kd[1 .. $#kd]], $first, ':void'),
               $self->_lower_block(\@rest, $vi2, $tail_ctx),
               ($decl_tail ? ($sname) : ())]);
    }

    # -- my @a / my %h / my (LIST) [= INIT];  → fresh containers in a let,
    # the assignment lowered by the ORIGINAL expression machinery (v1 parses
    # the whole `my … = …` statement as an expression and produces the
    # p-array-= / p-hash-= / p-list-= form — same path as Parser.pm's
    # _process_variable_statement).  All these vars stay boxed/containers.
    # `my ();` declares nothing.  It is legal Perl and a no-op — perl #113554,
    # and my.t asserts `eval "my ()"` leaves $@ EMPTY, so refusing it is not an
    # option once the v1 fallback is gone (task #227).  No binding, no form; in
    # tail position the value is the EMPTY LIST — emitted as the `(progn)` a
    # bare `()` already lowers to, which yields 0 elements in list context and
    # undef in scalar.  `(p-undef)` would wrongly be a 1-element list (the same
    # trap the bare `return;` lowering documents above) and `(vector)` an empty
    # ARRAY ref; both were measured against perl before settling here.  Must
    # precede _multi_decl, which reports an empty name list as "unsupported".
    if (_is_empty_my_decl($first)) {
      return ($self->_lower_block(\@rest, $vi, $tail_ctx),
              ($decl_tail ? ('(progn)') : ()));
    }
    my ($vars, $has_init) = $self->_multi_decl($first);
    die "Parser2 TODO: unsupported declaration: " . $first->content unless $vars;
    # A single container promoted to a package cell by a rename pass
    # (spanning OR captured-by-named-sub) lowers as `our` does: a defvar'd
    # container hoisted to the section top (so a hoisted named sub that captures
    # it, or a later package segment, shares the one cell) — NOT a let.  An
    # init form (`my %h__file__N = (…)`) stays as a write-through assignment
    # via the whole-statement expression machinery (same form the multi-decl
    # branch below emits).
    my @k = _strip_semi($first->schildren);
    if (@$vars == 1 && $self->{_file_lex_renamed}{$vars->[0]}) {
      push @{ $self->{_captured_decls} },
        global_decl_form($vars->[0], _fresh_container($vars->[0]));
      my @assign = $has_init ? ($self->_lower_expr([@k], $first)) : ();
      return (@assign, $self->_lower_block(\@rest, $vi, $tail_ctx));
    }
    if ($has_init) {
      # Self-referential init (`my @a = (@a, 1)` — RHS must see the OUTER
      # `@a`, not the freshly-declared one).  Perl's rule: the new lexical is
      # not in scope until AFTER the statement, so the RHS names the enclosing
      # scope's variable.  The fresh-container-then-`p-array-=` shape below
      # binds `@a` to an EMPTY container first, so the RHS would read that
      # empty one — wrong.  v1's fix is the "init in let binding" dance:
      # bind `@a` directly to `(p-copy-array <RHS>)`, with the RHS lowered in
      # the let's BINDING position where CL's parallel-let still resolves `@a`
      # to the outer scope.  We mirror that for the simple single-container
      # form `my @x = …@x…` / `my %h = …%h…`; the list form (`my (undef,@a)`)
      # and a nested declarator in the RHS (`my @a = my @a = …`) still need
      # v1's fuller machinery → fall back the whole file.
      my ($eq_i) = grep { $k[$_]->isa('PPI::Token::Operator') && $k[$_]->content eq '=' } 0 .. $#k;
      my @rhs     = @k[$eq_i + 1 .. $#k];
      # Chained declarators (`my @bee = my @bee = qw(…)`, `my (@bim) = my(@bee)
      # = LIST`, array.t): a nested `my` in the RHS declares its names in the
      # SAME enclosing scope (perl warns "masks earlier declaration" for a
      # duplicate but keeps one variable).  v1 collapses the chain into ONE
      # let binding every name fresh — the declarators reduce to chained
      # assignments in the expression machinery.  Consume the chain head(s),
      # collect their names, and require the FINAL RHS to be self-ref-free.
      my @chain_names;
      {
        my @r = @rhs;
        while (@r >= 3
               && $r[0]->isa('PPI::Token::Word') && $r[0]->content eq 'my') {
          my @syms = $r[1]->isa('PPI::Token::Symbol')   ? ($r[1])
                   : $r[1]->isa('PPI::Structure::List') ? (grep { $_->isa('PPI::Token::Symbol') }
                                                           map { $_->tokens } $r[1])
                   : ();
          my @names = grep { /^[\$\@\%]\w+$/ } map { $_->content } @syms;
          last unless @names == @syms && @names
            && $r[2]->isa('PPI::Token::Operator') && $r[2]->content eq '=';
          push @chain_names, @names;
          @r = @r[3 .. $#r];
        }
        if (@chain_names) {
          my $final_txt = join ' ', map { $_->content } @r;
          my %seen;
          my @all = grep { !$seen{$_}++ } (@$vars, @chain_names);
          die "Parser2 TODO: self-referential init: " . $first->content
            if $final_txt =~ /(?<![-\w])(?:my|our|local|state)\b/
            || (grep { $final_txt =~ _reads_name_rx($_) } @all)
            || (grep { $self->{_file_lex_renamed}{$_} } @all);
          $self->_reg_lex(@all);
          return (['let', ['list', map { ['list', $_, _fresh_container($_)] } @all],
                   $self->_lower_expr([@k], $first),
                   $self->_lower_block(\@rest, $vi, $tail_ctx)]);
        }
      }
      my $rhs_txt = join ' ', map { $_->content } @rhs;
      my @self_ref = grep { $rhs_txt =~ _reads_name_rx($_) } @$vars;
      if (@self_ref) {
        # A nested declarator in the RHS (`my @a = my @a = …`) or a var already
        # promoted to a renamed package cell still needs v1's fuller machinery.
        die "Parser2 TODO: self-referential init: " . $first->content
          if $rhs_txt =~ /(?<![-\w])(?:my|our|local|state)\b/
          || grep { $self->{_file_lex_renamed}{$_} } @$vars;
        if (@$vars == 1
            && $k[1]->isa('PPI::Token::Symbol') && $k[1]->content =~ /^[\@\%]\w+$/) {
          my $var  = $vars->[0];
          my $copy = substr($var, 0, 1) eq '@' ? 'p-copy-array' : 'p-copy-hash';
          $self->_reg_lex($var);
          return (['let', ['list', ['list', $var, [$copy, $self->_lower_expr(\@rhs, $first, 1)]]],
                   $self->_lower_block(\@rest, $vi, $tail_ctx),
                   ($decl_tail ? ($var) : ())]);
        }
        # LIST form (`my (undef,@bee) = @bee`, `my ($x,@a) = ($a[0],@a)`): the
        # same dance per variable — every SELF-REFERENCED name binds to a copy
        # of its outer self (container copy / fresh box with the outer value;
        # a let init-form evaluates in the OUTER environment), the rest bind
        # fresh, and the ordinary whole-statement assignment below reads the
        # copies, which hold the outer values.  A name mentioned only inside
        # interpolated RHS text also reads the copy — still correct.  (A var
        # read only via `$#name` is missed by the text scan — the same rare
        # limitation as the single-container path above.)
        $self->_reg_lex(@$vars);
        my %sref = map { $_ => 1 } @self_ref;
        my @binds;
        for my $v (@$vars) {
          my $init = !$sref{$v}             ? _fresh_container($v)
                   : substr($v, 0, 1) eq '@' ? ['p-copy-array', $v]
                   : substr($v, 0, 1) eq '%' ? ['p-copy-hash',  $v]
                   :                           ['p-box-init',   $v];
          push @binds, ['list', $v, $init];
        }
        return (['let', ['list', @binds],
                 $self->_lower_expr([@k], $first),
                 $self->_lower_block(\@rest, $vi, $tail_ctx)]);
      }
    }
    # Promoted (captured) names in a MULTI-decl lower as defvar'd package
    # cells hoisted to the section top, like the single-scalar/-container
    # branches above; unpromoted siblings keep their let.  The assignment is
    # the whole `my (...) = (...)` statement through the expression machinery
    # (same form the plain path emits inside its let) — it writes THROUGH the
    # boxes, so defvar'd cells and let-bound cells both receive their values.
    my @renamed = grep { $self->{_file_lex_renamed}{$_} } @$vars;
    if (@renamed) {
      push @{ $self->{_captured_decls} },
        global_decl_form($_, _fresh_container($_)) for @renamed;
      # Register AFTER the assignment (the decl's RHS reads the outer vars).
      my @reg = $self->{_file_has_str_eval} ? $self->_reg_eval_capture(@renamed) : ();
      my @unren  = grep { !$self->{_file_lex_renamed}{$_} } @$vars;
      $self->_reg_lex(@unren);
      my @assign = $has_init ? ($self->_lower_expr([@k], $first)) : ();
      return (@assign, @reg, $self->_lower_block(\@rest, $vi, $tail_ctx)) unless @unren;
      return (['let', ['list', map { ['list', $_, _fresh_container($_)] } @unren],
               @assign, @reg,
               $self->_lower_block(\@rest, $vi, $tail_ctx)]);
    }
    $self->_reg_lex(@$vars);
    my @binds = map { ['list', $_, _fresh_container($_)] } @$vars;
    # Tail value: with an init the assignment form (last) returns the place;
    # a bare single container is its own value.  A bare MULTI decl
    # (`my ($c,$d);` as tail) is the LIST of the declared names — perl gives 2
    # elements in list context and undef (the comma operator's last operand) in
    # scalar — so lower the name list through the ordinary expression
    # machinery, which already has both context rules, rather than inventing a
    # form here (task #227).
    my @tailval = ();
    if ($decl_tail && !$has_init) {
      @tailval = @$vars == 1 ? ($vars->[0])
               : $k[1]->isa('PPI::Structure::List')
                 ? ($self->_lower_expr([$k[1]], $first, 'inherit')) : ();
    }
    return (['let', ['list', @binds],
             ($has_init ? ($self->_lower_expr([@k], $first)) : ()),
             $self->_lower_block(\@rest, $vi, $tail_ctx),
             @tailval]);
  }

  # -- BEGIN/END/CHECK/… blocks: v1's p-BEGIN goes to the definitions bucket,
  # then the #55 assembly interleaves it with the sub defs at its source
  # position, so a BEGIN sees exactly the subs defined above it — perl-correct
  # by construction (sub-existence introspection: chdir.t).
  if ($first->isa('PPI::Statement::Scheduled')) {
    return ($self->_fallback_stmt($first, sched => 1),
            $self->_lower_block(\@rest, $vi, $tail_ctx));
  }
  # -- use/require/no: whole-statement fallback through the ORIGINAL parser,
  # but their definitions join the SAME source-ordered compile-time stream as
  # BEGIN blocks and sub defs (the #55 interleave, sched => 1).  Perl runs
  # use and BEGIN in source order; the plain route sent a use's definitions
  # to _captured_decls (the section HEAD), hoisting EVERY use above EVERY
  # earlier BEGIN — which inverted the %INC-seeding idiom
  # `BEGIN { package My::X; $INC{'My/X.pm'} = 1 }  use My::X;`
  # (Role-Tiny role-basic-basic.t) and violates the compile-stream invariant
  # of docs/declaration-ordering-fix-plan.md.
  if ($first->isa('PPI::Statement::Include')) {
    return ($self->_fallback_stmt($first, sched => 1),
            $self->_lower_block(\@rest, $vi, $tail_ctx));
  }
  # -- __END__/__DATA__: whole-statement fallback (declarations hoisted).
  if ($first->isa('PPI::Statement::End')
      || $first->isa('PPI::Statement::Data')) {
    return ($self->_fallback_stmt($first), $self->_lower_block(\@rest, $vi, $tail_ctx));
  }

  # -- a named sub nested inside a block: package-global in Perl (the block
  # only bounds lexical capture, which _hoist_nested_sub gates on) — hoist
  # the definition into the section's decl/def buckets.  A bodyless forward
  # declaration (`sub foo;`) hoists only a p-declare-sub (no definition).
  if ($first->isa('PPI::Statement::Sub') && $first->name) {
    # Prototyped/signatured: v1 owns the whole definition, same as the
    # top-level route (s300c) — the native _lower_sub used to swallow these
    # and DROP the signature (params fell through to file globals; defaults
    # never ran: signatures.t t131/t144/t146–t161, sig-r3 probe).  The
    # p-declare-sub + p-sub land in v1's buckets (→ _captured_decls);
    # rare runtime raws stay in place here.
    #
    # v1 keys its sub bucketing on _let_bound_vars: non-empty forces IN-PLACE
    # runtime emission (so a closure can capture a let lexical) and — the
    # trap — suppresses the hoist of named subs NESTED in this sub's body, so
    # `sub t146 ($a = t146x()) { sub t146x {…} … }` leaves t146x undefined
    # until t146 first RUNS while the default already calls it (undef).  The
    # let-bound set here is usually just leakage from earlier file lexicals
    # this sub never touches: when the sub's text references NONE of the
    # let-bound names, lower it with an EMPTY set (definitions bucket +
    # nested-sub hoist, the whole-file-v1 shape); a genuine reference keeps
    # the in-place capture behavior.
    if ($first->block && defined $self->_proto_or_sig_str($first)
        && !$self->_is_pure_prototype($first)) {
      my $lb   = $self->{_let_bound_vars} // {};
      my $text = $first->content;
      my $refs_letbound = 0;
      for my $lv (keys %$lb) {
        (my $bare = $lv) =~ s/^[\$\@\%]//;
        $bare =~ s/__(?:lex|file|shadow)__\d+$//;
        if ($text =~ /[\$\@\%]\s*\{?\s*\Q$bare\E\b/) { $refs_letbound = 1; last }
      }
      my @out;
      if ($refs_letbound) {
        @out = $self->_fallback_stmt($first);
      } else {
        local $self->{_let_bound_vars} = {};
        @out = $self->_fallback_stmt($first);
      }
      return (@out, $self->_lower_block(\@rest, $vi, $tail_ctx));
    }
    if ($first->block) {
      $self->_hoist_nested_sub($first);
    } else {
      push @{ $self->{_hoisted_decls} },
        ['p-declare-sub', $self->_sub_name_for_emission($first->name)];
    }
    return $self->_lower_block(\@rest, $vi, $tail_ctx);
  }

  # -- nested `package NAME;` / `package NAME { … }` (D1/E1.5,
  # docs/v2-endgame-plan.md): a package switch below the top level never
  # opens a section.  v1's shape: track the package in the shared Environment
  # so everything Environment-driven (hoisted sub names, 1-arg bless,
  # __PACKAGE__, `use overload`) attributes to NAME, and reflect the switch
  # at runtime (caller()) via p-set-current-package.  The statement form
  # scopes to the REMAINDER OF THE ENCLOSING BLOCK — lowering @rest under the
  # push gives Perl's scoping for free; the block form scopes to its own
  # block.  The package + CLOS class are (re)created inline each execution —
  # cheap enough for this rare construct.  defclass is QUALIFIED: the reader
  # consumes the whole enclosing top-level form before any of it runs, so a
  # bare class name would intern in the READING (section) package; NAME
  # itself is readable because parse() pre-declares every Statement::Package
  # namespace in the document.  Unqualified GLOBALS after the switch keep the
  # section package — v1's exact (documented) divergence.
  if ($first->isa('PPI::Statement::Package')) {
    my $pkg = $first->namespace // 'main';
    my $version = eval { $first->version };
    # PPI quirk (see $consume_pkg): ->version returns the BLOCK text for an
    # unversioned block form — accept only real version literals.
    undef $version unless defined $version && $version =~ /^v?\d+(?:[._]\d+)*$/;
    die "Parser2 TODO: versioned nested package statement\n" if defined $version;
    my ($blk) = grep { $_->isa('PPI::Structure::Block') } $first->schildren;
    my $env  = $self->environment;
    my $fp   = $self->fallback_parser;
    $env->add_package($pkg);
    my $prev    = $env->current_package;
    my $cl_pkg  = $fp->_cl_pkg_designator($pkg);
    my $cl_prev = $fp->_cl_pkg_designator($prev);
    (my $sym = $cl_pkg) =~ s/^://;
    # Per-package $a/$b specials (sort comparators) — v1 emits these from its
    # package preamble; hoisted to the section's declarations like `our`.
    push @{ $self->{_captured_decls} },
      "(defvar ${sym}::\$a (make-p-box nil))",
      "(defvar ${sym}::\$b (make-p-box nil))";
    my @enter = (['p-defpackage', $cl_pkg],
                 ['defclass', "${sym}::" . $fp->_pkg_to_clos_class($pkg),
                  ['list'], ['list']],
                 ['p-set-current-package', $cl_pkg, "\"$pkg\""]);
    my $restore = ['p-set-current-package', $cl_prev, "\"$prev\""];
    if ($blk) {
      $env->push_package($pkg);
      my @inner = $self->_lower_scope([grep { $_->significant } $blk->children], $vi, undef);
      $env->pop_package;
      return (@enter, @inner, $restore,
              $self->_lower_block(\@rest, $vi, $tail_ctx));
    }
    # #226: the eval region's LEADING `package X;` must take effect before the
    # body's hoisted forms, not at its own position in the run stream.  `use`
    # statements lower via _fallback_stmt(sched => 1) into the sched bucket,
    # which _assemble_eval_mode emits BEFORE the run forms — so leaving the
    # p-set-current-package here ran `use Role::Tiny` in main and its import
    # recorded the wrong package (Role-Tiny create-hook.t: got 'main', wanted
    # 'MyRole').  Hoist the enter forms to the head of the BODY (still inside
    # the thunk lambda, so p-eval-thunk's dynamic bind still unwinds them).
    if ($self->{_eval_pkg_enter} && $self->{_eval_pkg_stmt}
        && $first == $self->{_eval_pkg_stmt}) {
      push @{ $self->{_eval_pkg_enter} }, @enter;
      # #240 step 2: p-eval-thunk binds *package* to X around the free-name
      # resolution AND the body, so an unqualified package global inside the
      # region resolves in X the way perl says it does.  The designator is
      # $cl_pkg — the same spelling the enter forms use — recorded rather than
      # re-derived at assembly time.
      $self->{_eval_pkg_enter_cl} = $cl_pkg;
      $env->push_package($pkg);
      my @rest_only = $self->_lower_block(\@rest, $vi, $tail_ctx);
      $env->pop_package;
      return @rest_only;
    }
    $env->push_package($pkg);
    my @rest_forms = $self->_lower_block(\@rest, $vi, $tail_ctx);
    $env->pop_package;
    # The runtime restore would REPLACE the block's tail value — when the
    # remainder's value is used (sub-body tail), skip it and rely on p-sub's
    # dynamic *pcl-current-package* binding instead (v1's shape).
    push @rest_forms, $restore unless defined $tail_ctx;
    return (@enter, @rest_forms);
  }

  # -- expression-embedded `my` in a plain statement (`open my $fh, …`,
  # `weaken(my $p = \%tb)`): the lexical scopes to the enclosing BLOCK — let-
  # bind fresh containers around the statement AND the block remainder,
  # registered via _reg_lex so both the native and seam assignment paths emit
  # p-my-= (box-set), never p-scalar-= (whose box-in-box ref storage broke
  # weak-ref identity — hashassign.t 217/218).  VETO when a named sub
  # elsewhere in the segment references the name: those shapes relied on the
  # old forward-defvar'd global as the shared cell, and lexicalizing the decl
  # would strand the sub's reference (the statement's own enclosing sub does
  # not veto — its body IS the let's scope).
  if (ref($first) eq 'PPI::Statement' || ref($first) eq 'PPI::Statement::Expression') {
    my @emb = grep { !$self->{_file_lex_renamed}{$_}
                  && !$self->{_let_bound_vars}{$_} }
              $self->_embedded_my_names($first);
    if (@emb) {
      # INSIDE A SUB BODY the veto's premise is false and the question is not
      # even asked (#265/#272, re-shaped by #291): another sub cannot possibly
      # share a lexical declared inside THIS sub's body, so there is nothing to
      # strand — and the package global of that name keeps its own cell either
      # way, since the forward-decl pass no longer excludes let-bound names.
      # (Until #291 this was reached by RENAMING the decl to `$x__emb__N`, a
      # name no sub mentions, so that the veto below would not fire.  Same
      # outcome, one mechanism fewer.)  At FILE level the premise holds and the
      # veto stands: Capture-Tiny's Utils.pm really does share the cell (#199).
      my $vetoed = !_enclosing_sub_body($first)
                && $self->_embedded_my_veto_names(
                     $self->{_seg_named_subs} // [], $first, \@emb) ? 1 : 0;
      if (!$vetoed) {
        $self->_reg_lex(@emb);
        return (['let', ['list', map { ['list', $_, _fresh_container($_)] } @emb],
                 $self->_lower_stmt($first, $vi, $first_tail),
                 $self->_lower_block(\@rest, $vi, $tail_ctx)]);
      }
    }
  }

  # -- standalone label `AGAIN:` (a goto target, PPI: a Compound holding ONLY
  # the Label; the labeled statements are its SIBLINGS): lower the block
  # remainder inside (tagbody :again …) so a later `goto AGAIN` — lowered to
  # a raw `(go :again)` by the expression machinery — reaches the tag
  # lexically.  Perl re-executes `my` declarations jumped back over; the
  # remainder's nested lets sit inside the tagbody, so a backward go re-binds
  # them fresh — the same semantics.  Kept gated: a label in VALUE position
  # (tagbody yields nil, the remainder's tail value would be lost) and a
  # FORWARD goto (its `(go …)` was already emitted before the tagbody opens,
  # where CL cannot reach the tag).  Atom forms in the remainder (a void bare
  # read like `$x;` / a state-decl cell name) become tagbody TAGS — skipped
  # unevaluated, which for a void variable read is equivalent.
  if ($first->isa('PPI::Statement::Compound')) {
    my @lk = $first->schildren;
    if (@lk == 1 && $lk[0]->isa('PPI::Token::Label')) {
      (my $lbl = $lk[0]->content) =~ s/\s*:\s*$//;
      die "Parser2 TODO: standalone label\n" if $lbl !~ /^\w+$/;
      # Forward gotos handled by the #63 catch-wrap are marked; anything
      # else that textually targets this label from an earlier sibling is
      # an unhandled forward shape (goto before a decl outside the
      # decl-hoist subset, goto from an outer level across an intervening
      # label, …) → gate.
      unless (delete $self->{_goto_caught}{ refaddr($first) }) {
        for (my $p = $first->sprevious_sibling; $p; $p = $p->sprevious_sibling) {
          die "Parser2 TODO: forward goto to a standalone label\n"
            if ref $p && $p->isa('PPI::Node')
            && $p->content =~ /\bgoto\s+\Q$lbl\E\b/;
        }
      }
      return (['tagbody', ':' . $lbl,
               $self->_lower_block(\@rest, $vi, undef)])
        if !defined $tail_ctx;
      # Tail position (sub body / value block): a tagbody yields NIL, so
      # bracket the remainder in (setf RET (progn …)) and read RET after —
      # the labeled-loop/bare-block branch's task-#64 regime (#126: this die
      # used to send every sub body containing a standalone label to v1).
      $self->{_blk_ret_counter} //= 0;
      my $ret = '--pcl-blk-ret--' . $self->{_blk_ret_counter}++;
      return (['let', ['list', ['list', $ret, 'nil']],
               ['tagbody', ':' . $lbl,
                ['setf', $ret,
                 ['progn', $self->_lower_block(\@rest, $vi, $tail_ctx)]]],
               $ret]);
    }
    # -- `LBL: while (…) {…}` / `LBL: { … }` targeted by a `goto LBL`: PPI glues
    # a label onto a LOOP or a BARE BLOCK (only those two — `LBL: if (…)` and
    # `LBL: $x++;` leave the label standalone), so the branch above never sees
    # this shape and the `(go :LBL)` the expression machinery emits had no tag
    # at all.  Perl's `goto LABEL` jumps to the labeled STATEMENT, i.e. re-runs
    # the loop from the top: open the tagbody just before it and lower the run
    # unchanged inside (the mark makes the re-entry fall through to the normal
    # statement path).  The label's own loop-control block/catch tags are a
    # different namespace, so `last LBL` inside is untouched.
    if (@lk >= 2 && $lk[0]->isa('PPI::Token::Label')
        && !delete $self->{_goto_tagged}{ refaddr($first) }) {
      (my $lbl = $lk[0]->content) =~ s/\s*:\s*$//;
      if ($lbl =~ /^\w+$/
          && grep { $_->content =~ /\bgoto\s+\Q$lbl\E\b/ } @s) {
        $self->{_goto_tagged}{ refaddr($first) } = 1;
        my @inner = $self->_lower_block(\@s, $vi, $tail_ctx);
        # A tagbody yields NIL, so in value position bracket the run in
        # (setf RET (progn …)) and read RET after it — the same regime as the
        # bare block's task-#64 tail handling.
        return (['tagbody', ':' . $lbl, ['progn', @inner]])
          if !defined $tail_ctx;
        $self->{_blk_ret_counter} //= 0;
        my $ret = '--pcl-blk-ret--' . $self->{_blk_ret_counter}++;
        return (['let', ['list', ['list', $ret, 'nil']],
                 ['tagbody', ':' . $lbl, ['setf', $ret, ['progn', @inner]]],
                 $ret]);
      }
    }
  }

  # -- unlabeled `{ … } continue { … }`: PPI splits the continue off into an
  # ORPHAN sibling PPI::Statement (a labeled bare block keeps it inside the
  # compound) and may glom the NEXT statement's tokens into that orphan after
  # the continue block (there is no `;` to end it).  Join the continue back
  # onto the compound and lower the glommed trailing tokens as a synthetic
  # statement — v1's _find_continue_sibling + _process_trailing_tokens.
  if ($first->isa('PPI::Statement::Compound') && @rest
      && ref($rest[0]) eq 'PPI::Statement') {
    my @ck = $first->schildren;
    shift @ck if @ck && $ck[0]->isa('PPI::Token::Label');
    my @ok = $rest[0]->schildren;
    if (@ck && $ck[0]->isa('PPI::Structure::Block')
        && !Pl::Parser::_bare_block_is_anon_hash($ck[0])
        && @ok >= 2
        && $ok[0]->isa('PPI::Token::Word') && $ok[0]->content eq 'continue'
        && $ok[1]->isa('PPI::Structure::Block')) {
      my @och = $rest[0]->children;
      my ($bi) = grep { $och[$_] == $ok[1] } 0 .. $#och;
      my @trail = @och[$bi + 1 .. $#och];
      my @tsig  = grep { $_->significant
                         && !($_->isa('PPI::Token::Structure') && $_->content eq ';') } @trail;
      my @rest2 = @rest[1 .. $#rest];
      my @tforms;
      if (@tsig) {
        # Only a plain expression statement can ride here — a declarator or
        # compound keyword needs its real statement machinery → v1.
        die "Parser2 TODO: non-expression statement after bare-block continue\n"
          if join(' ', map { $_->content } @tsig)
             =~ /(?<![-\$\@\%\w])(?:my|our|local|state|sub|package|use|no|require|if|unless|while|until|for|foreach)\b/;
        my $synth = PPI::Statement->new;
        $synth->add_element($_->clone) for @trail;
        @tforms = $self->_lower_stmt($synth, $vi, @rest2 ? undef : $tail_ctx);
      }
      return ($self->_lower_compound($first, $vi, undef, $ok[1]),
              @tforms,
              $self->_lower_block(\@rest2, $vi, $tail_ctx));
    }
  }

  # -- `try {…} catch (VAR) {…}` followed by its `finally {…}`: PPI leaves the
  # finally block OUT of the Compound (see _repair_try_finally, which has by now
  # terminated it), so it arrives as the next sibling statement.  Join it back
  # on, exactly as the unlabeled `continue` block above is joined.
  if ($first->isa('PPI::Statement::Compound') && @rest
      && ref($rest[0]) eq 'PPI::Statement') {
    my @ck = $first->schildren;
    my @ok = _strip_semi($rest[0]->schildren);
    if (@ck && $ck[0]->isa('PPI::Token::Word') && $ck[0]->content eq 'try'
        && @ok == 2
        && $ok[0]->isa('PPI::Token::Word') && $ok[0]->content eq 'finally'
        && $ok[1]->isa('PPI::Structure::Block')) {
      my @rest2 = @rest[1 .. $#rest];
      return ($self->_lower_compound($first, $vi,
                                     (@rest2 ? undef : $tail_ctx), $ok[1]),
              $self->_lower_block(\@rest2, $vi, $tail_ctx));
    }
  }

  # -- everything else appends a form and continues at the same depth.
  return ($self->_lower_stmt($first, $vi, $first_tail), $self->_lower_block(\@rest, $vi, $tail_ctx));
}

# Declarator SYMBOLS of an expression-embedded `my` at the top level of a
# PLAIN statement (`open my $fh, …`, `weaken(my $p = \%tb)`, `func(my @a)`,
# `++my $x->{k}`) — they scope to the enclosing BLOCK (M2).  A `my` nested
# inside a block within the statement belongs to that block — skipped.
# (`state` never reaches lowering un-renamed — the state pre-pass is
# authoritative.)
sub _embedded_my_syms {
  my ($self, $stmt) = @_;
  return map { $_->[1] } _decl_syms_under($stmt, words => 'my', plain => 1);
}

# The same declarations, as a de-duplicated list of NAMES.
sub _embedded_my_names {
  my ($self, $stmt) = @_;
  my %seen;
  return grep { !$seen{$_}++ } map { $_->content } $self->_embedded_my_syms($stmt);
}

# Named subs + Scheduled blocks under @$stmts.  Shared by the segment driver
# and the pre-pass below so both read the same population.
sub _collect_named_subs {
  my ($stmts) = @_;
  my @ns;
  for my $child (@$stmts) {
    next unless ref $child && $child->isa('PPI::Node');
    push @ns, $child if $child->isa('PPI::Statement::Sub');
    push @ns, @{ $child->find('PPI::Statement::Sub') || [] };
  }
  return [ grep { $_->name || $_->isa('PPI::Statement::Scheduled') } @ns ];
}

# Which of @$names does a named sub OTHER than the one holding $stmt reference?
# That is exactly the condition under which _lower_block refuses to let-bind an
# expression-embedded `my` — the sub is presumed to share the forward-defvar'd
# global as its cell, so lexicalizing the decl would strand it.  ONE predicate,
# read by the refusal AND by the pre-pass that removes the need for it, so the
# two can never disagree (the s363 detector/rewriter rule).
sub _embedded_my_veto_names {
  my ($self, $subs, $stmt, $names) = @_;
  my %bad;
  SUBSCAN: for my $sub (@$subs) {
    next if _elem_within($stmt, $sub);
    my $t = $sub->content;
    for my $n (@$names) {
      next if $bad{$n};
      my $b = substr($n, 1);
      next unless $t =~ /(?:[\$\@\%]|\$\#)\Q$b\E\b/;
      # A sub that DECLARES the same name has its own lexical and cannot be
      # sharing this statement's cell — vetoing on it left BOTH sides unbound,
      # because the "old forward-defvar'd global" the veto falls back to is
      # never emitted when every mention is a declaration.
      #   sub s1 { my $fh; … }                 # its own lexical
      #   sub nf { open my $fh, '>', …; … }    # vetoed → free var, CRASH
      # (Capture-Tiny's t/lib/Utils.pm, task #199: `Utils::$fh is unbound`.)
      # The declaration must cover EVERY use, though: a sub can reference the
      # file-level cell AND declare its own shadow in an inner block —
      # `sub t { my $l = <$fh>; { my $fh; } }` — and exempting on the shadow
      # alone strands the outer reference (s329 review probe).
      next if $self->_sub_declares_name($sub, $n)
           && !$self->_sub_freely_references_name($sub, $n);
      $bad{$n} = 1;
      last SUBSCAN if keys(%bad) == @$names;
    }
  }
  return grep { $bad{$_} } @$names;
}

# Does named sub $sub declare $name (sigil-qualified) itself — i.e. does it
# own a lexical of that name rather than referring to someone else's cell?
# Used by the embedded-`my` veto above; deliberately checks the EXACT name, so
# a sub that mentions a different sigil of the same base ($fh vs @fh) still
# vetoes.
sub _sub_declares_name {
  my ($self, $sub, $name) = @_;
  for my $sym (@{ $sub->find(sub {
        $_[1]->isa('PPI::Token::Symbol') && $_[1]->content eq $name }) || [] }) {
    return 1 if $self->_symbol_is_declarator($sym);
  }
  return 0;
}

# Does $sub contain a FREE reference to $name — a use not covered by any of
# the sub's own declarations of that name?  A use is covered when some
# declarator of the exact name precedes it in document order AND the block the
# declaration scopes over contains the use (a declarator in a compound header
# — `foreach my $fh` — scopes over the compound's BODY only, not the block
# holding the compound).  Perl scopes a `my` from the END of its statement, so
# a same-statement use (`my $fh = f($fh)`) really refers to the outer cell;
# that shape warns in perl and is not distinguished here — it stays covered,
# which errs toward exempting.  Everything else errs toward VETO, the old
# conservative behavior.
sub _sub_freely_references_name {
  my ($self, $sub, $name) = @_;
  my $sigil = substr($name, 0, 1);
  my $base  = substr($name, 1);
  my $pat   = qr/\Q$sigil\E\Q$base\E\b/;
  my @decls;    # find() returns document order, so earlier index = earlier decl
  my @refs;     # [$token, #decls seen before it] — only EARLIER decls can cover
  for my $tok (@{ $sub->find('PPI::Token') || [] }) {
    if ($tok->isa('PPI::Token::Symbol')) {
      next if $tok->content ne $name;
      if ($self->_symbol_is_declarator($tok)) { push @decls, $tok }
      else { push @refs, [$tok, scalar @decls] }
    }
    else {
      # The name can hide inside tokens that are not Symbols: `<$fh>` is one
      # QuoteLike::Readline token, and "$fh"/regex bodies interpolate.  Those
      # are always USES, never declarators.  Single-quoted strings don't
      # interpolate; comments/POD are noise.
      next if $tok->isa('PPI::Token::Quote::Single')
           || $tok->isa('PPI::Token::Comment')
           || $tok->isa('PPI::Token::Pod');
      next if $tok->content !~ $pat;
      push @refs, [$tok, scalar @decls];
    }
  }
  REF: for my $r (@refs) {
    my ($tok, $ndecl) = @$r;
    for my $i (0 .. $ndecl - 1) {
      my $scope = $self->_decl_scope_block($decls[$i], $sub);
      my $b = $tok;
      while ($b && $b != $sub) {
        next REF if $b == $scope;
        $b = $b->parent;
      }
    }
    return 1;
  }
  return 0;
}

# The block a declarator's `my` scopes over, within $sub: the nearest
# enclosing Structure::Block — unless the declarator sits in a compound
# HEADER (`foreach my $x (…)`, `if (my $x = …)`), in which case it scopes
# over that compound's body block only.
sub _decl_scope_block {
  my ($self, $decl, $sub) = @_;
  my $node = $decl;
  while ($node && $node != $sub) {
    my $parent = $node->parent or last;
    return $parent if $parent->isa('PPI::Structure::Block');
    if ($parent->isa('PPI::Statement::Compound')) {
      my ($blk) = grep { $_->isa('PPI::Structure::Block') } $parent->schildren;
      return $blk if $blk;
    }
    $node = $parent;
  }
  return $sub;
}

sub _lower_stmt {
  my ($self, $stmt, $vi, $tail_ctx) = @_;
  # #364: the same publication as in _lower_block, for the statements that
  # reach here on their own (a compound's parts, a for-head's init/step).
  $self->fallback_parser->lex_home->{_eval_site_features} =
    $self->{_eval_features_by_stmt}{ refaddr $stmt };

  # `class NAME ;` in a file that has actually switched the feature on: the
  # indirect-object reading PCL would otherwise emit is a silent method call
  # where the author declared a class.  Refuse perl-shaped, like the Track A
  # families — but on code that COMPILES, so the key is the STRICT one (no
  # version-bundle evidence).  See Pl::Parser::class_statement_refusal.
  if (my $refusal = $self->fallback_parser->class_statement_refusal($stmt)) {
    my $where = $self->fallback_parser->eval_mode
              ? '(eval)'
              : ($self->fallback_parser->has_filename
                   ? $self->fallback_parser->filename : '-');
    die "PCL: $refusal, at $where line " . ($stmt->line_number // 0) . "\n";
  }

  if ($stmt->isa('PPI::Statement::Compound')) {
    return $self->_lower_compound($stmt, $vi, $tail_ctx);
  }

  if ($stmt->isa('PPI::Statement::Break')) {
    my @k = _strip_semi($stmt->schildren);
    my $kw = $k[0]->content;
    if ($kw eq 'return') {
      shift @k;
      # `return if COND` / `return unless COND`: after shifting `return` the
      # modifier keyword is at index 0, which _split_modifier (scans from 1)
      # would miss — mis-lowering to (p-return (p-if COND)).  Detect a leading
      # modifier here; the value expr is then empty (return undef/()).
      my ($expr, $mod, $cond);
      if (@k && $k[0]->isa('PPI::Token::Word')
          && Pl::PExpr::Config::is_statement_modifier($k[0]->content)) {
        ($expr, $mod, $cond) = ([], $k[0]->content, [@k[1 .. $#k]]);
      } else {
        ($expr, $mod, $cond) = _split_modifier(\@k);
      }
      return $self->_fallback_stmt($stmt) if _modifier_needs_fallback($mod);
      # A multi-element return list gates to v1, whose expression machinery
      # knows list-operator arity and spreads true top-level commas as
      # separate p-return args.  v2's native lowering gets both variants
      # wrong: a LIST-VALUED element makes the native list construction emit
      # (vector … (p-flatten @a) …), which p-return does NOT splice
      # (`return (0,@a)` leaked the marker); an all-scalar comma list emits
      # one (if *wantarray* (vector …) (progn …)) arg, which wrongly treats
      # a :void caller context — toplevel, sort comparator — as list where
      # the macro's spread dispatch is (eq *wantarray* t) (sort.t "Ret: blk
      # ret", s308).  Detection is STRUCTURAL — a comma among the statement's
      # own tokens (`return $a, $b` and `return bless \$x, "C"` both gate;
      # only the expression parser could tell them apart) or inside a lone
      # parenthesized list (`return ($a, $b)`).  Commas nested in call
      # parens (`return f($a, $b)`) stay native.  Whole-stmt gate, so a
      # trailing if/unless modifier rides along.  Single list-valued returns
      # (`return @a` / `return map …`) are NOT wrapped in a vector and work.
      my $is_comma = sub { $_[0]->isa('PPI::Token::Operator')
                           && ($_[0]->content eq ',' || $_[0]->content eq '=>') };
      my $top_comma = grep { $is_comma->($_) } @$expr;
      if (!$top_comma && @$expr == 1 && $expr->[0]->isa('PPI::Structure::List')) {
        my ($inner) = grep { $_->isa('PPI::Statement') } $expr->[0]->schildren;
        $top_comma = $inner && grep { $is_comma->($_) } $inner->schildren;
      }
      return $self->_fallback_stmt($stmt) if $top_comma;
      # A returned call must see the CALLER's context (no *wantarray* bind).
      # Bare `return;` must be a ZERO-arg (p-return): in list context it
      # contributes 0 elements (`()`), scalar/void → undef.  `(p-return
      # (p-undef))` would wrongly yield a 1-element list in list context
      # (v1 emits the bare `(p-return)` — sub.t check_ret(-1) list).
      my $form = @$expr
        ? ['p-return', $self->_lower_expr($expr, $stmt, 'inherit')]
        : ['p-return'];
      return _apply_modifier($form, $mod, $cond, $self, $stmt);
    }
    # goto/next/last/redo: keep the keyword and let the ORIGINAL expression
    # machinery lower the whole thing (goto &sub tail-calls with the LIVE @_;
    # the $body_uses_args gate in _lower_sub has already kept the @_ binding
    # for any sub whose body mentions goto, so the forwarded @_ exists).
    my ($expr, $mod, $cond) = _split_modifier(\@k);
    return $self->_fallback_stmt($stmt) if _modifier_needs_fallback($mod);
    my $form = $self->_lower_expr($expr, $stmt);
    return _apply_modifier($form, $mod, $cond, $self, $stmt);
  }

  if ($stmt->isa('PPI::Statement::Scheduled') || $stmt->isa('PPI::Statement::Package')) {
    die "Parser2 TODO: " . ref($stmt);
  }

  # -- plain expression statement
  my @k = _strip_semi($stmt->schildren);

  # Bare `...` (yada yada): dies "Unimplemented at $0 line N." — a STATEMENT
  # form v1 handles above its expression parse (Parser.pm), so the seam
  # cannot lower it.  Native mirror of v1's emission; also fixes the file-
  # level bare `...` (broken PARSE ERROR raw before task #78).
  if (@k == 1 && $k[0]->isa('PPI::Token::Operator') && $k[0]->content eq '...') {
    my $line = $k[0]->line_number // 0;
    return ['p-die', '"Unimplemented"', ':loc',
            ['format', 'nil', '"~A line ~D"', ['to-string', ['unbox', '$0']], $line]];
  }

  my ($expr, $mod, $cond) = _split_modifier(\@k);
  # while/until/for/foreach statement modifiers (and do{}while, which splits
  # the same way) are outside the native subset — whole-statement fallback
  # through v1, which owns their loop/do-while semantics.
  return $self->_fallback_stmt($stmt) if _modifier_needs_fallback($mod);
  # A postfix `EXPR if/unless COND` whose value is the sub's return ($tail_ctx
  # defined) yields the COND value when the body is skipped (`sub f { 5 if 0 }`
  # → 0), like a block bare-if — same ret-var transform as the block form.
  # The body lowers in the TAIL's context like a plain expression tail would
  # (s308b): a map tail is LIST, so `map { (A, B) if C }` keeps both
  # elements — the default lowering flattened the list to its last element.
  if (defined $tail_ctx && $mod && $mod =~ /^(?:if|unless)$/) {
    my $ret = '--pcl-if-ret--' . $self->{_if_ret_counter}++;
    my $test = ['setf', $ret, $self->_lower_expr($cond, $stmt)];
    $test = ['p-!', $test] if $mod eq 'unless';
    return $self->_restore_caller_wa($tail_ctx,
           ['let', ['list', ['list', $ret, 'nil']],
            ['p-if', $test, ['setf', $ret, $self->_lower_expr($expr, $stmt, $tail_ctx)], 'nil'],
            $ret]);
  }

  # `$x = RHS;` on a let-bound scalar → native form: `setf` when $x is a raw
  # (unboxed) slot — RHS proven arithmetic by VarAnnotator, so the stored
  # value is a raw number — else `p-my-=` (box-set; never the special-
  # proclaiming p-scalar-=).
  if (!$mod && @$expr >= 3
      && $expr->[0]->isa('PPI::Token::Symbol') && $expr->[0]->content =~ /^\$\w+$/
      && !$self->{_file_lex_renamed}{ $expr->[0]->content }
      && $expr->[1]->isa('PPI::Token::Operator') && $expr->[1]->content eq '='
      && !_tail_below_assign_prec($expr)) {
    my $name = $expr->[0]->content;
    my $rhs = [@$expr[2 .. $#$expr]];
    if ($vi->{$name} && $vi->{$name}{unboxable}) {
      return ['setf', $name,
              _wrap_freeze($vi->{$name}, $name, $self->_lower_expr($rhs, $stmt))];
    }
    if ($self->{_let_bound_vars}{$name}) {
      return ['p-my-=', $name, $self->_lower_expr($rhs, $stmt)];
    }
  }

  # `$x OP= RHS;` (coercing compound op) on a RAW slot → the op's -raw macro
  # twin, e.g. (p-incf-raw $x …) = (setf $x (+ (to-number $x) …)).
  # VarAnnotator leaves $x unboxable only when EVERY compound write is a
  # coercing op at native statement root (its %RAW_COMPOUND regime), so this
  # branch and the verdict key on the same shape; a boxed $x falls through to
  # the generic path (p-incf … = box-set), unchanged from before task #62.
  if (!$mod && @$expr >= 3
      && $expr->[0]->isa('PPI::Token::Symbol') && $expr->[0]->content =~ /^\$\w+$/
      && !$self->{_file_lex_renamed}{ $expr->[0]->content }
      && $expr->[1]->isa('PPI::Token::Operator')
      && !_tail_below_assign_prec($expr)) {
    my $name   = $expr->[0]->content;
    my $rawmac = Pl::VarAnnotator::raw_compound_macro($expr->[1]->content);
    if ($rawmac && $vi->{$name} && $vi->{$name}{unboxable}) {
      # S1 str-buffer slot: `.=` appends in place (fill-pointer extend)
      # instead of the allocate-a-fresh-concatenation -raw twin.
      return ['%pcl-str-append', $name,
              $self->_lower_expr([@$expr[2 .. $#$expr]], $stmt)]
        if $vi->{$name}{strbuf} && $expr->[1]->content eq '.=';
      return [$rawmac, $name,
              $self->_lower_expr([@$expr[2 .. $#$expr]], $stmt)];
    }
  }

  # `$x++;` / `++$x;` / `--$x;` / `$x--;` as its own statement on a RAW slot
  # (A-num): VarAnnotator allows the root-incdec event only when every other
  # write is numeric-valued, so the numeric -raw twin matches perl (magical
  # string increment is unreachable).  A postfix incdec in TAIL position
  # returns the OLD value — prog1.
  if (!$mod && @$expr == 2) {
    my ($a, $b) = @$expr;
    my ($name, $opc, $post);
    if ($a->isa('PPI::Token::Symbol') && $b->isa('PPI::Token::Operator')) {
      ($name, $opc, $post) = ($a->content, $b->content, 1);
    } elsif ($a->isa('PPI::Token::Operator') && $b->isa('PPI::Token::Symbol')) {
      ($name, $opc, $post) = ($b->content, $a->content, 0);
    }
    if (defined $name && $name =~ /^\$\w+$/ && ($opc eq '++' || $opc eq '--')
        && !$self->{_file_lex_renamed}{$name}
        && $vi->{$name} && $vi->{$name}{unboxable}) {
      my $form = [$opc eq '++' ? 'p-incf-raw' : 'p-decf-raw', $name];
      return ($post && defined $tail_ctx) ? ['prog1', $name, $form] : $form;
    }
  }

  # Statement position: the value is discarded (void) — except for a block
  # tail whose value the enclosing sub returns ($tail_ctx = 'inherit').
  my $vctx = $tail_ctx // ':void';
  my $form = $self->_lower_expr($expr, $stmt, $vctx);
  # A void FALLBACK `m//g` match must bind *wantarray* :void explicitly: v1 adds
  # this wrap at the statement level, but _parse_expression(VOID_CTX) does not, so
  # the /g match would inherit the CALLER's list context dynamically and match
  # GLOBALLY (`$a =~ /(.)/g;` in a list-called sub advanced through the whole
  # string, so `$1` was the LAST char, not the first).  Narrowed to g-matches
  # only — wrapping EVERY void statement is both wrong (over-scopes wantarray)
  # and needless overhead (it perturbed `print $i;` shapes and every call).
  # Under an active sub-body :void regime the ambient is already :void.
  if ($vctx eq ':void'
      # (every expression lowers through the one generator now — the old
      # native/fallback distinction that lived here is gone, Phase A)
      && _stmt_has_global_match($stmt)
      && !$self->environment->wa_void_active) {
    $form = Pl::CLForm::ctx_bind(':void', $form);
  }
  return $self->_restore_caller_wa($tail_ctx,
         _apply_modifier($form, $mod, $cond, $self, $stmt));
}

# Perl parses `$x = A, B` / `$x = A or B` as `($x = A), B` / `($x = A) or B`
# — assignment (and OP=) binds TIGHTER than `,`/`=>`/`or`/`and`/`xor`.
# Splitting the statement tokens at the operator would fold such a tail into
# the RHS (op/lex_assign.t: `$a = readlink 'x', 'y'` must leave $a undef; a
# folded `$a = 0 or f()` assigned f()'s value).  A depth-0 operator below
# assignment precedence therefore disqualifies the native token split; the
# generic expression machinery owns the whole statement instead — including
# the parenless list-op ambiguity (`$x = f 1, 2`, where the comma DOES belong
# to the call).  Structures (parens/braces) are single PPI children, so a
# scan over the statement's schildren only sees depth-0 operators.  An
# unboxable (raw-slot) $x cannot be stranded by this reroute: a comma/or
# statement write is not an arithmetic native-root event under either token
# association, so VarAnnotator has already left such variables boxed.
sub _tail_below_assign_prec {
  my ($expr) = @_;
  return defined _lowprec_idx($expr, 2) ? 1 : 0;
}

# The below-assignment precedence table itself lives in
# Pl::PExpr::TokenUtils (task #138) — v1's `local` handler needs the same
# classification, and neither statement parser may depend on the other.
# These two are thin local names for it.
sub _lowprec_idx        { Pl::PExpr::TokenUtils::lowprec_idx(@_) }

# Is $form exactly ONE scalar assignment to $name, and nothing else?  Then the
# depth-0 low-prec token _lowprec_idx saw was a list-operator argument
# separator, not a statement tail — PExpr consumed it into the RHS.  Returns
# the RHS form, or undef for every other shape (a comma expression, a raw
# seam string, an assignment to something else).  Used by the #298 re-route in
# _lower_block; deliberately exact, since the caller's fallback is a refusal.
sub _sole_assign_rhs {
  my ($form, $name) = @_;
  return undef unless ref($form) eq 'ARRAY' && @$form == 3;
  my ($head, $target) = @{$form}[0, 1];
  return undef if ref $head || ref $target;
  return undef unless defined $head && defined $target;
  return undef unless $head eq 'p-scalar-=' || $head eq 'p-my-=';
  return undef unless $target eq $name;
  return $form->[2];
}
sub _lowprec_split_safe { Pl::PExpr::TokenUtils::lowprec_split_safe(@_) }

# Leaf-level tail wrap under the sub-body :void regime (task #60): the body
# bound *wantarray* to :void once, but a tail (implicit-return) statement's
# value must be computed in the CALLER's context — restore it from
# *pcl-caller-wantarray* for this one statement.  Applied at the innermost
# expression statement (compound tails thread $tail_ctx down to their branch
# leaves), never around a whole compound — its non-tail inner statements must
# stay in the :void ambient.  Explicit `return` never reaches here (Break
# branch; the p-return macro restores the caller context itself).
sub _restore_caller_wa {
  my ($self, $tail_ctx, @forms) = @_;
  return @forms
    unless defined $tail_ctx && "$tail_ctx" eq 'inherit'
    && $self->environment->wa_void_active && @forms;
  return Pl::CLForm::ctx_bind('*pcl-caller-wantarray*', @forms);
}

# True if the statement contains an `m//g` match (list-vs-scalar context
# sensitive).  s///g / tr///g are not context-sensitive (they act globally in
# any context), so they are excluded.
sub _stmt_has_global_match {
  my ($stmt) = @_;
  for my $t (@{ $stmt->find('PPI::Token::Regexp::Match') || [] }) {
    my %m = $t->get_modifiers;
    return 1 if $m{g};
  }
  return 0;
}

sub _lower_compound {
  my ($self, $stmt, $vi, $tail_ctx, $sib_cont) = @_;
  my @k = $stmt->schildren;

  # Optional leading `LABEL:` — rides along on loops (`OUTER: while …`) and
  # bare blocks (`SKIP: { … }`).  Standalone labels (goto targets) and
  # labeled if/unless stay TODO → v1.
  my $label;
  if (@k && $k[0]->isa('PPI::Token::Label')) {
    ($label = (shift @k)->content) =~ s/\s*:$//;
  }
  die "Parser2 TODO: standalone label\n" unless @k;

  # Bare block { … } = a loop-once: last/next/redo all work inside it.
  if ($k[0]->isa('PPI::Structure::Block')) {
    # `LABEL: { … } continue { … }` keeps the continue INSIDE the compound;
    # the unlabeled form arrives as an ORPHAN sibling statement instead,
    # joined back by _lower_block's lookahead into $sib_cont.
    my $cont = $sib_cont;
    my ($ci) = grep { $k[$_]->isa('PPI::Token::Word')
                      && $k[$_]->content eq 'continue' } 0 .. $#k;
    if (defined $ci) {
      ($cont) = grep { $_->isa('PPI::Structure::Block') } @k[$ci + 1 .. $#k];
      die "Parser2 TODO: continue without a block\n" unless $cont;
    }
    # PPI mis-tokenizes an anon-hash constructor statement `{ LITERAL , … };`
    # as a bare block — v1's detector + statement fallback handle it.
    return $self->_fallback_stmt($stmt)
      if Pl::Parser::_bare_block_is_anon_hash($k[0]);
    return $self->_lower_bare_block($k[0], $label, $vi, $cont, $tail_ctx);
  }

  my $kw = $k[0]->content;
  die "Parser2 TODO: label on non-loop compound '$kw'\n"
    if defined $label && $kw !~ /^(?:while|until|for|foreach)$/;

  if ($kw eq 'if' || $kw eq 'unless') {
    # Collect (keyword, condition, block) clauses: if/unless, elsif*, else?.
    my (@clauses, $cur_kw, $cur_cond);
    for my $el (@k) {
      if    ($el->isa('PPI::Token::Word'))            { $cur_kw = $el->content }
      elsif ($el->isa('PPI::Structure::Condition'))   { $cur_cond = $el }
      elsif ($el->isa('PPI::Structure::Block')) {
        push @clauses, { kw => $cur_kw, cond => $cur_cond, block => $el };
        $cur_cond = undef;
      }
    }
    # A `my` declared in any condition head scopes to the whole construct —
    # register the names as let-bound around cond+body lowering, then wrap the
    # result in a fresh boxed let (lexical shadow of any outer same-named var).
    my @cond_mys = $self->_cond_my_names(map { $_->{cond} } @clauses);
    my (%sv_live, %sv_lb);
    if (@cond_mys) {
      %sv_live = %{ $self->{_live_lex} // {} };
      %sv_lb   = %{ $self->{_let_bound_vars} // {} };
      $self->_reg_lex(@cond_mys);
    }
    my $result;
    # A bare if/unless (no else) whose value is the enclosing sub's return
    # ($tail_ctx defined) returns, in Perl, the CONDITION value when false
    # (`sub f { if(0){5} }` → 0) and the body value when true.  Replicate v1's
    # `--pcl-if-ret--` ret-var transform natively: each cond is captured into
    # RET *and* used as the test (so a false chain leaves RET = the last cond),
    # each taken branch overwrites RET with its body value, and the whole form
    # yields RET.  (An empty true branch → RET = nil = undef, matching perl; v1
    # wrongly keeps the cond there — the documented not-supported corner.)
    if (defined $tail_ctx && (!@clauses || $clauses[-1]{kw} ne 'else')) {
      my $ret = '--pcl-if-ret--' . $self->{_if_ret_counter}++;
      my $chain = 'nil';
      for my $c (reverse @clauses) {
        my $test = ['setf', $ret, $self->_lower_expr([_cond_parts($c->{cond})], $stmt)];
        $test = ['p-!', $test] if $c->{kw} eq 'unless';
        $chain = ['p-if', $test,
                  ['setf', $ret, ['progn', $self->_lower_scope([$c->{block}->schildren], $vi, $tail_ctx)]],
                  $chain];
      }
      $result = ['let', ['list', ['list', $ret, 'nil']], $chain, $ret];
    }
    else {
      # Build nested p-if forms from the tail (else innermost) outward.
      # A tail if/unless is the enclosing block's VALUE — its branch blocks
      # inherit $tail_ctx (loop/plain compounds don't propagate values).
      my $form;
      if (@clauses && $clauses[-1]{kw} eq 'else') {
        my $c = pop @clauses;
        $form = ['progn', $self->_lower_scope([$c->{block}->schildren], $vi, $tail_ctx)];
      }
      while (my $c = pop @clauses) {
        my $cond = $self->_lower_expr([_cond_parts($c->{cond})], $stmt);
        $cond = ['p-!', $cond] if $c->{kw} eq 'unless';
        $form = ['p-if', $cond,
                 ['progn', $self->_lower_scope([$c->{block}->schildren], $vi, $tail_ctx)],
                 (defined $form ? ($form) : ())];
      }
      $result = $form;
    }
    if (@cond_mys) {
      $self->{_live_lex} = \%sv_live;
      $self->{_let_bound_vars} = \%sv_lb;
      $result = $self->_wrap_cond_mys($result, @cond_mys);
    }
    return $result;
  }

  if ($kw eq 'while' || $kw eq 'until') {
    my ($cond_s) = grep { $_->isa('PPI::Structure::Condition') } @k;
    # The FIRST Structure::Block is the loop body; a second (after `continue`)
    # is the continue block, handled by _continue_keys.
    my ($block)  = grep { $_->isa('PPI::Structure::Block') } @k;
    # `my` in the loop condition scopes to the loop — register + wrap (as for if).
    my @cond_mys = $self->_cond_my_names($cond_s);
    my (%sv_live, %sv_lb);
    if (@cond_mys) {
      %sv_live = %{ $self->{_live_lex} // {} };
      %sv_lb   = %{ $self->{_let_bound_vars} // {} };
      $self->_reg_lex(@cond_mys);
    }
    my $cond = $self->_lower_expr([_cond_parts($cond_s)], $stmt);
    # Perl loop conditions whose value comes from each/readline/readdir/glob
    # terminate on *undef*, not false-but-defined ("0" line, each's index 0),
    # and a bare `<FH>` implicitly assigns to $_ — v1's _auto_defined_cond,
    # applied at the raw seam (native conds can't contain these calls).
    # v1 skips the rewrite for `until`, matching perl.
    $cond = $self->_auto_defined_raw($cond) if $kw eq 'while';
    $cond = ['p-!', $cond] if $kw eq 'until';
    my $result = ['p-while', $cond, _label_keys($label),
                  $self->_lower_scope([$block->schildren], $vi),
                  $self->_continue_keys(\@k, $vi)];
    if (@cond_mys) {
      $self->{_live_lex} = \%sv_live;
      $self->{_let_bound_vars} = \%sv_lb;
      $result = $self->_wrap_cond_mys($result, @cond_mys);
    }
    return $result;
  }

  # PPI 1.291 sometimes hands a foreach's parenthesised LIST a
  # PPI::Structure::For instead of a Structure::List — measured on
  # `for my $sub (sub :lvalue {$_}, sub :lvalue {return $_})` (op/sub_lval.t,
  # #268), where the anon-sub blocks inside the parens confuse its lexer.  The
  # source shape decides, not PPI's class name: a C-style `for` NEVER has a
  # loop VARIABLE before the parens, and it always has `;` separators.  Fix it
  # here, before either branch reads @k, so the whole foreach path (range
  # split, aliasing, loop-var scoping) sees the list it would have seen.
  # Same lexer, same class of failure as #253 — see docs/ppi-upstream-bugs.md.
  if (($kw eq 'for' || $kw eq 'foreach')
      && (my ($mis) = grep { $_->isa('PPI::Structure::For') } @k)) {
    my $has_var  = grep { $_->isa('PPI::Token::Symbol') } @k;
    my $has_semi = grep { $_->isa('PPI::Token::Structure') && $_->content eq ';' }
                   $mis->tokens;
    bless $mis, 'PPI::Structure::List' if $has_var || !$has_semi;
  }

  if (($kw eq 'for' || $kw eq 'foreach')
      && (my ($for_s) = grep { $_->isa('PPI::Structure::For') } @k)) {
    # C-style for (INIT; COND; STEP) BLOCK.  Sections are POSITIONAL: an
    # empty one is a PPI::Statement::Null (`;`) placeholder, and trailing
    # empties are simply absent.  Empty init/step → no form; empty cond →
    # constant true (v1's _process_c_style_for defaults).
    my ($block) = grep { $_->isa('PPI::Structure::Block') } @k;
    # p-for IGNORES :continue and C-style-for + continue is invalid Perl anyway
    # — gate defensively (never legitimately reached).
    die "Parser2 TODO: C-style for with continue block\n"
      if grep { $_->isa('PPI::Token::Word') && $_->content eq 'continue' } @k;
    my @sect = grep { $_->isa('PPI::Statement') } $for_s->children;
    die "Parser2 TODO: for(;;) with extra sections" if @sect > 3;
    my ($init_s, $cond_s, $step_s) =
      map { $_ && !$_->isa('PPI::Statement::Null') ? $_ : undef } @sect[0 .. 2];

    # The whole C-for HEAD is one lexical scope, and every `my` in it scopes to
    # the loop — exactly like `while (my $x = …)`.  Save the lexical registries
    # before ANY of it registers and restore at every exit: a leak puts the
    # (unbound-after-the-let) name into a later sibling's string-eval capture
    # alist (bop.t %res section abort).
    my %saved_lb  = %{ $self->{_let_bound_vars} // {} };
    my %saved_lex = %{ $self->{_live_lex} // {} };

    # Multiple `my` decls in the init (`for (my $i = 0, my $j = 10; …)`) are the
    # comma operator: `(my $i = 0), (my $j = 10)`.  _single_scalar_decl would
    # misparse the whole comma-list as $i's RHS, so bind ALL declared counters in
    # a boxed let and lower the init as one expression (a progn of assignments,
    # matching v1) — no unboxing carve-out for the multi-counter case.
    my @init_mys = $init_s ? $self->_cond_my_names($init_s) : ();
    if (@init_mys >= 2) {
      my @head_mys = $self->_cond_my_names($cond_s, $step_s);
      $self->_reg_lex(@head_mys, @init_mys);
      my $initform = ['list', $self->_lower_expr([_strip_semi($init_s->schildren)], $stmt)];
      my $cond = $cond_s
        ? ['list', $self->_auto_defined_raw(
                     $self->_lower_expr([_strip_semi($cond_s->schildren)], $stmt))]
        : ['list', 't'];
      my $step = $step_s ? ['list', $self->_lower_stmt($step_s, $vi)] : ['list'];
      my @body = $self->_lower_scope([$block->schildren], $vi);
      my $form = ['let', ['list', map { ['list', $_, '(make-p-box nil)'] } @init_mys],
                  ['p-for', $initform, $cond, $step, _label_keys($label), @body]];
      $self->{_let_bound_vars} = \%saved_lb;
      $self->{_live_lex} = \%saved_lex;
      return $self->_wrap_cond_mys($form, @head_mys);
    }

    # A `my $i = INIT` init binds the counter in a let AROUND the p-for —
    # register the name BEFORE lowering cond/step/body so fallback expressions
    # see it as let-bound.  Unboxable (e.g. step `$i = $i + 1`) → raw slot;
    # else boxed (a `$i++` step keeps VarAnnotator conservative).
    # The counter is scoped to the loop (head + body), like the foreach branch
    # below — see the registry save above.
    my ($name, $init) = $init_s ? $self->_single_scalar_decl($init_s) : ();

    # #297: every OTHER `my` in the head — one in the CONDITION or the STEP, and
    # an init `_single_scalar_decl` declined (`my ($x) = …`, `my @a = …`) — gets
    # ONE fresh boxed/container let around the whole construct (_wrap_cond_mys),
    # exactly as a `while`/`if` condition-my does; the section itself lowers to
    # the per-iteration assignment into it.  Without the let the declaration was
    # a bare write into the package cell, so the name stayed defined (and shared
    # with the global) after the loop.  $name is excluded: its own let is below,
    # and it may take a raw/unboxed slot a `(make-p-box nil)` wrap would defeat.
    my @head_mys = grep { !defined $name || $_ ne $name }
                   $self->_cond_my_names($cond_s, $step_s, $init_s);
    $self->_reg_lex(@head_mys) if @head_mys;
    $self->_reg_lex($name) if $name;
    # #138: a SINGLE `my` with a comma tail (`for (my $i = 0, $j = 9; …)`) —
    # the tail is not part of $i's init (perl: `(my $i = 0), ($j = 9)`), and
    # folding it made $i start at 9, so the loop ran zero times.  The >= 2
    # branch above already documents the shape; this is its one-`my` sibling.
    # As in the `my` statement branch, hand the whole `$i = …` run to the
    # expression machinery instead of re-deciding the parenless list-operator
    # ambiguity here, and pin the counter BOXED (a comma write is not a
    # native-root arithmetic event, so no raw-slot verdict is applicable).
    my $lowprec_run;
    if ($name && defined $init && defined _lowprec_idx($init, 0)) {
      my @ik = _strip_semi($init_s->schildren);
      $lowprec_run = [@ik[1 .. $#ik]];
      $vi = { %$vi, $name => { unboxable => 0 } };
    }

    # (The s286b ++-step carve-out — re-analyze WITHOUT the step, emit
    # `(setf $i (p-± $i 1))` — is gone: the A-num root-incdec regime (task
    # #62) approves the same counters in the VarAnnotator pass itself, and
    # its numeric-write-family gate also fixes the carve-out's latent bug:
    # a string-seeded counter (`my $i = "aa"; …; $i++`) now stays boxed so
    # perl's MAGICAL string increment still runs — the carve-out numified
    # it, hanging `for (my $i = "aa"; $i ne "ad"; $i++)`.)
    #
    # RENAMED counters (an `$x__file__N` promotion, say) are
    # invisible to the block-level $vi (keyed by ORIGINAL names), so re-run
    # the annotator over just this loop's statements — init/cond/STEP/body,
    # step INCLUDED (A-num classifies it; the old carve-out had to exclude
    # it) — and adopt an approving verdict.  The loop var is loop-scoped
    # (let around p-for), so the loop region is the whole visibility span.
    if ($name && !$lowprec_run && !($vi->{$name} && $vi->{$name}{unboxable})) {
      my $vi2 = Pl::VarAnnotator->analyze(
        [(grep { defined } $init_s, $cond_s, $step_s), $block->schildren],
        undef, $self->_cur_sub_info, $self);
      if ($vi2->{$name} && $vi2->{$name}{unboxable}) {
        # adopt the WHOLE entry: a B-verdict (coerce => num/str) must keep
        # its strict-wrap marker or the writes would store unfrozen values
        $vi = { %$vi, $name => $vi2->{$name} };
      }
    }
    my $cond = $cond_s
      ? ['list', $self->_auto_defined_raw(
                   $self->_lower_expr([_strip_semi($cond_s->schildren)], $stmt))]
      : ['list', 't'];
    my $step = $step_s ? ['list', $self->_lower_stmt($step_s, $vi)] : ['list'];
    my @body = $self->_lower_scope([$block->schildren], $vi);

    my $form;
    if ($name) {
      my $initval = defined $init && !$lowprec_run
        ? $self->_lower_expr($init, $stmt) : '(p-undef)';
      if ($lowprec_run) {
        $form = ['let', ['list', ['list', $name, '(make-p-box nil)']],
                 ['p-for', ['list', $self->_lower_expr($lowprec_run, $stmt, ':void')],
                  $cond, $step, _label_keys($label), @body]];
      } elsif ($vi->{$name} && $vi->{$name}{unboxable}) {
        # a B-verdict/str-buffer counter must freeze/bufferize its init too
        $form = ['let', ['list', ['list', $name,
                                  _wrap_freeze($vi->{$name}, $name, $initval)]],
                 ['p-for', ['list'], $cond, $step, _label_keys($label), @body]];
      } else {
        $form = ['let', ['list', ['list', $name, '(make-p-box nil)']],
                 ['p-for', ['list', ['p-my-=', $name, $initval]], $cond, $step,
                  _label_keys($label), @body]];
      }
    } else {
      $form = ['p-for',
               ['list', ($init_s ? ($self->_lower_stmt($init_s, $vi)) : ())],
               $cond, $step, _label_keys($label), @body];
    }
    $self->{_let_bound_vars} = \%saved_lb;
    $self->{_live_lex} = \%saved_lex;
    return $self->_wrap_cond_mys($form, @head_mys);
  }

  if ($kw eq 'for' || $kw eq 'foreach') {
    my ($list) = grep { $_->isa('PPI::Structure::List') } @k;
    my ($block) = grep { $_->isa('PPI::Structure::Block') } @k;
    my ($var) = grep { $_->isa('PPI::Token::Symbol') } @k;
    die "Parser2 TODO: foreach without list" unless $list && $block;
    my $name = $var ? $var->content : '$_';
    # `foreach MY $x` declares a fresh lexical, so the loop must bind one — no
    # matter what a package variable of the same name is doing.  The loop macro
    # cannot see the declaration (the name is not bound in its macroexpansion
    # environment yet), so say it: `:my t` overrides %p-cell-loop-var-p, which
    # would otherwise localize the global's cell and leak the loop value into
    # every sub the body calls (#294).  `foreach our $x` / `foreach $x` carry no
    # key — there the macro's environment reading IS the right answer.
    my $loop_my = $var && do {
      my $p = $var->sprevious_sibling;
      $p && $p->isa('PPI::Token::Word') && $p->content eq 'my' ? 1 : 0;
    };
    # `for $Pkg::x (...)`: the BINDING must be the CL-ordered global
    # (Pkg::$x) — the raw perl order is unreadable ($MAIN package error) —
    # while $name stays the perl name for $vi/_reg_lex bookkeeping.
    my $cl_name = Pl::ExprToCL::qualified_var_to_cl($name, $self->environment);
    # The LIST is evaluated in the OUTER scope (the loop var is not yet bound).
    my @list_parts = map { $_->schildren } grep { $_->isa('PPI::Statement') } $list->children;
    # A single aliasable lvalue ELEMENT (`for ($a[i])`, `for ($h{k})`) must bind
    # the loop var to a box that WRITES THROUGH to the live container.
    # _foreach_alias_rewrite gives (FROM-HEAD, TO-HEAD), AST-guarded on the
    # sole-element shape; swap the lowered element's call head to its box-
    # returning form (v1's mechanism, docs/foreach-aliasing.md).  The container
    # (%h/@a) is already a box, so no extra VarAnnotator work is needed.
    #
    # The MAGIC-lvalue shape (`for (substr($s,..))` / pos / vec) additionally
    # needs the scalar arg force-boxed and is used only by substr.t — which also
    # over-taxes the compiler's default heap through the per-statement void-wrap
    # (a v2-wide issue, CLAUDE.md #8 / s285).  Keep it gated to v1 until both are
    # addressed (E2 void-wrap hoist).
    my @alias_hd = Pl::Parser::_foreach_alias_rewrite(\@list_parts);
    # A list that is EXACTLY one range (`for $v (A..B)`) lowers to the
    # counting-loop macro p-foreach-range: endpoints evaluated once, numeric
    # ranges never materialize the vector (perl's own foreach-range
    # optimization; docs/bench-exec-investigation.md).  Detection is AST-level
    # (_foreach_range_split over the top-level PPI tokens) and must run
    # BEFORE whole-list lowering — PExpr's cleanup mutates the shared tokens
    # destructively, so each token list is lowered exactly once.  The
    # numeric-vs-magical-string decision happens at RUNTIME inside the macro
    # (%p-range-classify), so 'a'..'e' takes the same route and falls back
    # internally.  Shapes the guard rejects (`reverse 1..9`, `1..3, 7`,
    # `1..$x ? 3 : 5`) and endpoints the native lowerer can't do (eval-guard)
    # keep the p-foreach path — a skip is a missed optimization, never a
    # miscompile.  (Postfix `EXPR for A..B` is a different lowering site,
    # not covered here.)
    my ($from_form, $to_form, $range_raw);
    # Kind-A gate (Pl::Passes, PCL_OPT): off, the range materializes through
    # the general p-foreach path — a skip is a missed optimization, never a
    # miscompile (the same contract as the guard-rejected shapes above).
    my @range = (@alias_hd || !Pl::Passes::enabled('foreach-range'))
              ? () : _foreach_range_split(\@list_parts);
    if (@range) {
      ($from_form, $to_form) = eval {
        ($self->_lower_expr($range[0], $stmt), $self->_lower_expr($range[1], $stmt));
      };
      # RAW loop var (no per-iteration box) when the segment VarAnnotator
      # proved the name unboxable — the same name-keyed verdict every other
      # raw-slot decision consults ($vi is conservative: a capture/\$i/local
      # of the name ANYWHERE in the segment vetoes it).  Never for $_: the
      # global must stay a box (s///, chomp write through it).
      $range_raw = 1
        if defined $to_form && $var && $vi->{$name} && $vi->{$name}{unboxable};
    }
    my $list_form;
    unless (defined $to_form) {
      # A list whose every DEPTH-0 element is a single SCALAR operand has a
      # statically known length, so it must not go through the run-time
      # flattener: at runtime a box wrapping a vector is indistinguishable
      # from an @array box, so %p-flatten-for-list spread the referent
      # (`for ($r)` ran once per element of @$r).  The sigil is compile-time
      # knowledge — emit the `(vector …)` shape instead, which keeps each
      # box as one element (so `for ($x) { $_ = 1 }` writes through).  Same
      # rule at k=1 and k>1; resolver + rationale:
      # Pl::Parser::_foreach_scalar_elements.
      my @el = Pl::Parser::_foreach_scalar_elements(\@list_parts);
      if (@el > 1) {
        # Each element's token run is lowered EXACTLY ONCE and the whole
        # list never is — PExpr's cleanup mutates the shared tokens
        # destructively (same discipline as the range split above).  The
        # alias VERDICTS are therefore taken over the untouched tokens,
        # BEFORE any lowering runs; the head swap then maps onto the lowered
        # elements BY POSITION.  This is the sole-element rewrite applied per
        # element — one verdict function, one head-swapper, k of them.
        my @hd = map { [ Pl::Parser::_foreach_alias_rewrite($_) ] } @el;
        my @forms;
        for my $i (0 .. $#el) {
          my $f = $self->_lower_expr($el[$i], $stmt, 1);
          $f = _alias_box_form($f, @{ $hd[$i] })
            // die "Parser2 TODO: foreach over an aliasable lvalue element\n"
            if @{ $hd[$i] };
          push @forms, $f;
        }
        $list_form = ['vector', @forms];
      }
      else {
        $list_form = $self->_lower_expr(\@list_parts, $stmt, 1);
        if (@alias_hd) {
          $list_form = _alias_box_form($list_form, @alias_hd)
            // die "Parser2 TODO: foreach over an aliasable lvalue element\n";
        }
        $list_form = ['vector', $list_form] if @el;
      }
    }
    # The loop variable is scoped to the BODY only: register it, lower the
    # body (and a continue block, which sees the loop var), then restore
    # _let_bound_vars/_live_lex so it does not leak into sibling statements.
    # Without this, a later sibling's string-eval capture alist would list the
    # (now-unbound) loop var → unbound-variable at load (W3 regression seen in
    # cmpchain.t).  _seg_lex keeps it (forward-decl).
    my %saved_lb  = %{ $self->{_let_bound_vars} // {} };
    my %saved_lex = %{ $self->{_live_lex} // {} };
    $self->_reg_lex($name);
    my @body = $self->_lower_scope([$block->schildren], $vi);
    my @cont = $self->_continue_keys(\@k, $vi);
    $self->{_let_bound_vars} = \%saved_lb;
    $self->{_live_lex} = \%saved_lex;
    my @my_keys = $loop_my ? (':my', 't') : ();
    return defined $to_form
      ? [($range_raw ? 'p-foreach-range-raw' : 'p-foreach-range'),
         ['list', $cl_name, $from_form, $to_form],
         _label_keys($label), @my_keys, @body, @cont]
      : ['p-foreach', ['list', $cl_name, $list_form],
         _label_keys($label), @my_keys, @body, @cont];
  }

  # `try BLOCK catch (VAR) BLOCK [finally BLOCK]` — perl 5.34's feature 'try'.
  # The finally block is NOT in the Compound (PPI stops after catch); it rides
  # in through $sib_cont, joined by _lower_block.  Everything else about the
  # construct is in the p-try macro's docstring — this end only has to hand it
  # three lowered blocks and the catch variable's CL name.
  if ($kw eq 'try') {
    my (@blocks, $clist, $has_catch);
    for my $el (@k[1 .. $#k]) {
      if    ($el->isa('PPI::Structure::Block')) { push @blocks, $el }
      elsif ($el->isa('PPI::Structure::List'))  { $clist = $el }
      elsif ($el->isa('PPI::Token::Word'))      { $has_catch = 1 if $el->content eq 'catch' }
    }
    # A missing/odd shape DIES naming itself rather than lowering half a
    # construct: perl requires the catch block, and a `try` whose catch we
    # cannot see would silently swallow every exception.
    die "Parser2 TODO: try without a catch (VAR) BLOCK\n"
      unless $has_catch && @blocks == 2 && $clist;
    my @vs = @{ $clist->find('PPI::Token::Symbol') || [] };
    die "Parser2 TODO: catch variable is not a single scalar\n"
      unless @vs == 1 && $vs[0]->content =~ /^\$\w+\z/;
    my $name   = $vs[0]->content;
    my $cl_var = Pl::ExprToCL::qualified_var_to_cl($name, $self->environment);
    # Both blocks are VALUE positions (`do { try {…} catch ($e) {…} }` yields
    # the executed one's last value), so both inherit $tail_ctx.  The finally
    # block never yields a value — perl discards it.
    my @try = $self->_lower_scope([$blocks[0]->schildren], $vi, $tail_ctx);
    # The catch variable is scoped to the catch block only — register, lower,
    # restore, as the foreach loop variable does.
    my %saved_lb  = %{ $self->{_let_bound_vars} // {} };
    my %saved_lex = %{ $self->{_live_lex} // {} };
    $self->_reg_lex($name);
    my @catch = $self->_lower_scope([$blocks[1]->schildren], $vi, $tail_ctx);
    $self->{_let_bound_vars} = \%saved_lb;
    $self->{_live_lex} = \%saved_lex;
    my @fin = defined $sib_cont
      ? $self->_lower_scope([$sib_cont->schildren], $vi) : ();
    return ['p-try', ['progn', @try], ['list', $cl_var, ['progn', @catch]],
            (@fin ? (['progn', @fin]) : ())];
  }

  die "Parser2 TODO: compound '$kw'";
}

# Sole-range foreach-list detection lives in Pl::VarAnnotator
# (foreach_range_split) — the annotator uses the SAME test to decide that the
# loop var aliases nothing (→ raw-slot candidate), so there is exactly one
# definition of "the list is one range".
sub _foreach_range_split { Pl::VarAnnotator::foreach_range_split(@_) }

# Rewrite a lowered foreach-list element's call head FROM → TO (its box-
# returning form) so the loop var aliases the live container slot.  The two
# representations `_lower_expr` produces for these shapes:
#   - native list form `['p-gethash', @args]` / `['p-aref', @args]` — pure
#     AST head-swap (the box head takes the identical args);
#   - a Raw seam chunk for substr/pos/vec (wrapped in a `(let ((*wantarray* t))
#     …)`) — swap the FIRST `(FROM ` head token, exactly as v1 does (the outer
#     call precedes its args in prefix notation, so the first is the right one).
# Returns undef if the lowering was neither shape (the AST check disagreed with
# the emission) so the caller can gate cleanly rather than miscompile.
sub _alias_box_form {
  my ($form, $from, $to) = @_;
  if (ref($form) eq 'ARRAY' && @$form && !ref $form->[0] && $form->[0] eq $from) {
    return [$to, @{$form}[1 .. $#$form]];
  }
  if (Pl::CLForm::is_raw($form)) {
    my $text = $$form;
    return undef unless $text =~ s/\(\Q$from\E /($to /;
    return Pl::CLForm::raw($text);
  }
  # Since the E2.final root flip the seam returns TREES, so the call head for
  # substr/pos/vec sits NESTED under the (let ((*wantarray* t)) …) context
  # wrap.  Preorder, leftmost-first descent = v1's first-text-occurrence swap
  # in prefix notation.  Rebuilds only the spine above the swapped call.
  if (ref($form) eq 'ARRAY') {
    for my $i (1 .. $#$form) {
      next unless ref $form->[$i];
      my $sub = _alias_box_form($form->[$i], $from, $to);
      next unless defined $sub;
      my @copy = @$form;
      $copy[$i] = $sub;
      return \@copy;
    }
  }
  return undef;
}

# `:label NAME` pair for the loop macros' parse-loop-keys (must come first in
# the body-and-keys tail), or nothing.
sub _label_keys {
  my ($label) = @_;
  return () unless defined $label;
  return (':label', $label);
}

# `:continue (progn …)` pair for a while/foreach `continue { … }` block, placed
# AFTER the loop body (parse-loop-keys finds :continue by position; v1 emits it
# last), or nothing.  The continue block is its own lexical scope.
sub _continue_keys {
  my ($self, $k, $vi) = @_;
  my ($ci) = grep { $k->[$_]->isa('PPI::Token::Word')
                    && $k->[$_]->content eq 'continue' } 0 .. $#$k;
  return () unless defined $ci;
  my ($cont_block) = grep { $_->isa('PPI::Structure::Block') } @{$k}[$ci + 1 .. $#$k];
  die "Parser2 TODO: continue without a block\n" unless $cont_block;
  return (':continue', ['progn', $self->_lower_scope([$cont_block->schildren], $vi)]);
}

# A bare block is a single-iteration loop: last/next/redo work inside it.
# The shapes replicate v1's _process_bare_block exactly — unlabeled uses the
# plain (block nil (tagbody :redo … :next)) that unlabeled p-last/p-next/
# p-redo target; labeled adds the LAST/NEXT/REDO catch tags that the
# *-dynamic throws (e.g. Test::More's skip() doing `last SKIP` from inside a
# called sub) unwind to.  The (let ((*package* *package*))) wrapper stops an
# (in-package …) reached inside the block (module loads via the statement
# fallback) from leaking to subsequent top-level forms.  `my` scoping needs
# no bookkeeping: _lower_block's nested lets sit inside the tagbody, and a
# (go …) legally jumps out of them.
sub _lower_bare_block {
  my ($self, $block, $label, $vi, $cont, $tail_ctx) = @_;
  # `{ … } continue { … }` — the continue block (its own lexical scope) runs
  # after normal completion or `next`, is skipped by `last` (which exits the
  # enclosing block/LAST catch), and is not re-run by `redo` (which re-enters
  # the tagbody).  v1's placement: after the tagbody, inside the block (or
  # inside the LAST catch, after the NEXT catch, for the labeled shape).
  my @cont = defined $cont
    ? (['progn', $self->_lower_scope([$cont->schildren], $vi)])
    : ();
  # Tail position (task #64): tagbody always yields NIL, which dropped the
  # block's value — Perl returns the last statement's value from a loop-once
  # bare block in sub-tail position.  Same regime as v1's text emitter:
  # unlabeled, no continue → bracket the body in (setf RET (progn …)) inside
  # the tagbody and read RET after the block.  `last` return-from's past the
  # setf (RET stays nil), `redo` re-runs and re-assigns, `next` skips to
  # :next — loop-once semantics unchanged.
  my $value_tail = !defined $label && !@cont && defined $tail_ctx;
  my @body = $self->_lower_scope([$block->schildren], $vi,
                                 $value_tail ? $tail_ctx : undef);
  my $inner;
  if (defined $label) {
    $inner =
      ['block', $label,
        ['catch', "(pcl::%pcl-loop-tag \"LAST\" '$label)",
          ['catch', "(pcl::%pcl-loop-tag \"NEXT\" '$label)",
            ['tagbody', ':redo',
              ['catch', "(pcl::%pcl-loop-tag \"REDO\" '$label)",
                ['progn', @body, '(go :next)']],
              '(go :redo)',
              ':next']],
          @cont]];
  } elsif ($value_tail) {
    $self->{_blk_ret_counter} //= 0;
    my $ret = '--pcl-blk-ret--' . $self->{_blk_ret_counter}++;
    $inner = ['let', ['list', ['list', $ret, 'nil']],
               ['block', 'nil',
                 ['tagbody', ':redo',
                   ['setf', $ret, ['progn', @body]],
                   ':next']],
               $ret];
  } else {
    $inner = ['block', 'nil', ['tagbody', ':redo', @body, ':next'], @cont];
  }
  return ['let', ['list', ['list', '*package*', '*package*']], $inner];
}

# ---------------------------------------- embedded blocks (task #78, E2)

# lower_embedded_block — THE answer to "how is this expression-embedded block
# compiled" while Parser2 is lowering (Phase B3 of
# docs/plan-one-compiler-s411.md).  PExpr's block sites ask their parser
# (Pl::Parser::embed_block), which forwards to this through the `_v2_embed`
# hook that capture_v1 / _lower_expr install around every lowering parse.
# The hook ALWAYS answers: the structural route when it can — the
# inline_lambda node then carries `body_form` (an arrayref of CLForms for the
# lambda body) and ExprToCL's gen_inline_lambda_form emits the whole lambda
# structurally — and otherwise v1's text compile of the block, run HERE inside
# capture_v1 (its hoists drained to _captured_decls at this one place) and
# returned as ONE raw form.  Nothing declines to PExpr any more; a hook-less
# parser (no Parser2 above it) answers with v1's text itself.
# Kinds: 'map' | 'grep' | 'sort' | 'eval' answer BODY forms; 'do' | 'sub'
# answer the whole LAMBDA form.
# A tail Statement::Variable whose _lower_block value semantics are known
# correct (the $decl_tail machinery): `state` (appends its cell, or dies →
# clean decline via the eval), single-scalar `my` (any init shape), and
# container/list `my` with an init (the assignment form returns the place)
# or a bare SINGLE container (appended).  NOT convertible: `our` (no-init
# returns nil, should be the var) and bare multi decls (`my ($a,@b);` —
# value would need list construction).  Pure token read, no side effects.
# `my ();` — a declaration with an EMPTY name list.  Legal Perl, declares
# nothing (perl #113554).
sub _is_empty_my_decl {
  my ($stmt) = @_;
  my @k = _strip_semi($stmt->schildren);
  return @k == 2
      && $k[0]->isa('PPI::Token::Word') && $k[0]->content eq 'my'
      && $k[1]->isa('PPI::Structure::List')
      && !@{ $k[1]->find('PPI::Token::Symbol') || [] };
}

sub _tail_decl_convertible {
  my ($self, $stmt) = @_;
  return 1 if _is_empty_my_decl($stmt);
  my @sk = _strip_semi($stmt->schildren);
  return 0 unless @sk && $sk[0]->isa('PPI::Token::Word');
  my $kw = $sk[0]->content;
  return 1 if $kw eq 'state';
  # `our NAMES OP= RHS` as the eval's last statement: _lower_our_decl returns
  # the assignment EXPRESSION as its only form, and _lower_block returns ()
  # for the empty remainder, so the assignment is already the tail value —
  # nothing to convert.  `eval "our \$VERSION = '1.01'"` is a routine module
  # idiom and was the whole CPAN-board half of audit family F2 (task #227).
  # A no-init `our` converts too — _lower_block appends the declared
  # variable's read in tail position, which is the value perl gives.
  if ($kw eq 'our') {
    return 1 if @sk == 2;
    # Any operator tail, not only an assignment: `our $x++` lowers to the
    # post-increment expression exactly as `our $x = 1` lowers to the
    # assignment, and in both cases that expression IS _lower_our_decl's only
    # form, hence already the tail value.  (Same widening as there.)
    return $sk[2]->isa('PPI::Token::Operator') ? 1 : 0;
  }
  return 0 unless $kw eq 'my';
  my ($name) = $self->_single_scalar_decl($stmt);
  return 1 if $name;
  my ($vars, $has_init) = $self->_multi_decl($stmt);
  return 0 unless $vars;
  return 1 if $has_init || @$vars == 1;
  # A bare MULTI decl's tail value is the LIST of its names, which the
  # lowering now emits by running the name list through the expression
  # machinery — but only when the names are parenthesised (`my ($c,$d)`).
  my @k = _strip_semi($stmt->schildren);
  return @k >= 2 && $k[1]->isa('PPI::Structure::List') ? 1 : 0;
}

sub lower_embedded_block {
  my ($self, $block, $kind) = @_;
  my $forms = $kind eq 'sub' ? $self->_lower_embedded_anon($block)
                             : $self->_lower_embedded_body($block, $kind);
  if ($forms) {
    # do{} is a plain 0-arg lambda; progn — not (block nil) — keeps it
    # loop-transparent (an unlabeled last/next inside reaches the enclosing
    # loop, as in perl).
    return ['lambda', ['list'], ['progn', @$forms]] if $kind eq 'do';
    return $forms;
  }
  return $self->_embed_via_v1($block, $kind);
}

# The v1 route for a block the structural lowering declined: v1's block
# compiler runs on the seam parser inside capture_v1 — parse_block_as_function
# pushes the hoists it finds inside the block (a `use`, a BEGIN, an `our`
# defvar) into the current section, and the capture's drain hoists them to
# _captured_decls = the section TOP, outside every lexical `let`.  A hoisted
# body that references a lexical LIVE here would read an unbound global from
# up there — the same conservative text scan _hoist_nested_sub uses; over-
# firing only costs a die, never correctness.
sub _embed_via_v1 {
  my ($self, $block, $kind) = @_;
  warn "pcl-raw\tdecl:hook-declined\t$kind\n" if $ENV{PCL_E2_RAW_CENSUS};
  my $p = $self->fallback_parser;
  my $cap = $p->capture_v1(sub { $p->embed_block_v1($block, $kind) },
                           bucket => 'definitions', hook => $self->_embed_hook);
  my @drained = (@{ $cap->{decls} }, @{ $cap->{defs} }, @{ $cap->{runtime} });
  if (@drained && %{ $self->{_live_lex} // {} }) {
    my $txt = join "\n", @drained;
    for my $var (sort keys %{ $self->{_live_lex} }) {
      (my $bare = $var) =~ s/^[\$\@\%]//;
      die "Parser2 TODO: embedded block's hoisted text captures live lexical '$bare'\n"
        if $txt =~ /(?:[\$\@\%]|\$\#)\Q$bare\E\b/;
    }
  }
  push @{ $self->{_captured_decls} }, @drained;
  return $cap->{result};
}

# The structural lowering of a map/grep/sort/eval/do block BODY: an arrayref
# of CLForms, or undef to decline (the caller takes the v1 route).
sub _lower_embedded_body {
  my ($self, $block, $for_func) = @_;
  # `map { {k => $_} } …` / `map({k => $_}, LIST)`: the braces are one
  # hash-constructor EXPRESSION, not a statement list — the form twin of the
  # v1 helper suffices.  Only map/grep: perl reads `{ WORD => …` after them
  # as an anon hash (the paren form); after eval/do/sub the braces are
  # always a BLOCK whose statement is a LIST (`eval { k => 1 }` is (k, 1)),
  # so the hash route is wrong there — it was applied to every kind until
  # s412 (`sub { a => 1 }` printed a garbage lambda and crashed; `do { b => 2 }`
  # gave a hashref).  A grep/map hash block that fails the form route falls
  # to v1's text route below.
  if (($for_func eq 'map' || $for_func eq 'grep')
      && Pl::PExpr::_block_is_hash_constructor($block)) {
    my $f = $self->fallback_parser->parse_hash_block_to_cl_form($block);
    if (!$f || _embed_form_unsafe($f)) {
      warn "pcl-raw\tdecl:hash-ctor\n" if $ENV{PCL_E2_RAW_CENSUS};
      return undef;
    }
    return [$f];
  }
  my @stmts = grep { ref $_ && $_->significant && !$_->isa('PPI::Statement::Null') }
              $block->schildren;
  return ['nil'] unless @stmts;

  # A `package` statement ANYWHERE in the block: v1's machinery wraps the
  # body in the (*package* / *pcl-current-package*) revert let — the native
  # nested-package branch instead relies on p-sub's dynamic bind for the
  # runtime restore, which a bare lambda does not have (the switch leaked
  # out of `map { package XM; … }`).  Keep v1's route.  (PPI find returns 0,
  # not undef, when nothing matches.)
  if (@{ $block->find('PPI::Statement::Package') || [] }) {
    warn "pcl-raw\tbody-decl:package\n" if $ENV{PCL_E2_RAW_CENSUS};
    return undef;
  }

  # Tail shapes v1 lowers differently: scheduled/sub tails still decline.
  # A tail DECLARATION converts when its value semantics are covered by the
  # $decl_tail machinery in _lower_block (s307).  A tail COMPOUND converts
  # (s308): _lower_compound threads $tail_ctx to its branch leaves.  A tail
  # if/unless statement MODIFIER converts (s308b): _lower_stmt's
  # defined-tail_ctx ret-var transform yields the cond value when the body
  # is skipped — v1's plain (p-if COND EXPR) wrongly dropped it (`map
  # { X if C }` lost the "" element perl produces); loop modifiers take the
  # statement-level _fallback_stmt inside the converted block.  A tail
  # INCLUDE converts (Phase C, s412): _lower_block routes it through
  # _fallback_stmt, whose runtime raw IS the tail value — `(p-require "X")`
  # for `eval { require X }` (require's value, as perl), and a `use`/`no`
  # emits no runtime form at all, so the block's value is the previous
  # statement's — which is perl's too (a `use` runs nothing at run time).
  my $tail = $stmts[-1];
  if (!$tail->isa('PPI::Statement')
      || ($tail->isa('PPI::Statement::Variable')
          && !$self->_tail_decl_convertible($tail))
      || $tail->isa('PPI::Statement::Scheduled')
      || $tail->isa('PPI::Statement::Sub')
      || $tail->isa('PPI::Statement::Package')) {
    warn "pcl-raw\tbody-decl:tail-" . (ref($tail) =~ s/^PPI::Statement:*//r || 'stmt') . "\n"
      if $ENV{PCL_E2_RAW_CENSUS};
    return undef;
  }

  my $env = $self->environment;
  # Decline must be side-effect-free: snapshot everything a partial lowering
  # can touch, restore on failure so v1's re-lowering of the same block does
  # not duplicate hoists or see fat-comma-rewritten tokens.
  my $ppi_snap    = _ppi_state_snapshot(@stmts);
  my %saved_lex   = %{ $self->{_live_lex} // {} };
  my %saved_lb    = %{ $self->{_let_bound_vars} // {} };
  my $scope_depth = scalar @{ $env->scope_stack };
  my @side_snaps  = map { [$_, scalar @{ $self->{$_} // [] }] }
                    qw(_captured_decls _hoisted_decls _hoisted_defs
                       _hoisted_def_lines _sched_defs _sched_lines);

  # Tail context.  v1 suppresses EVERY dynamic *wantarray* wrap on the tail
  # statement's spine call (the env tail_position flag) so the ENCLOSING
  # binding flows: p-map binds t, p-grep nil, the sort lambda's own
  # (*wantarray* nil) wrapper nil, and an eval{} inherits its call site's
  # context (perl: eval BLOCK propagates context — a ':void' tail bind here
  # broke `my @x = eval { f() }`).  'inherit' reproduces that: no bind
  # native or seam.  map instead passes LIST (v1 parses map tails LIST_CTX
  # for the range/flatten structure); its explicit t bind is dynamically
  # identical to p-map's own.  The enclosing macro rebinds *wantarray*, so
  # the sub-body :void regime does not reach in here (v1 clears
  # wa_void_active the same way).
  my $tail_ctx = $for_func eq 'map' ? 1 : 'inherit';
  my $forms = eval {
    local $env->{wa_void_active} = 0;
    my $vi = Pl::VarAnnotator->analyze(\@stmts, undef, $self->_cur_sub_info, $self);
    $env->push_scope;
    [ $self->_lower_block(\@stmts, $vi, $tail_ctx) ];
  };
  pop @{ $env->scope_stack } while @{ $env->scope_stack } > $scope_depth;
  $self->{_live_lex} = \%saved_lex;
  $self->{_let_bound_vars} = \%saved_lb;

  if (!$forms || !@$forms || grep { _embed_form_unsafe($_) } @$forms) {
    if ($ENV{PCL_E2_RAW_CENSUS}) {
      my $why = !$forms ? 'lower-died: ' . substr(($@ // '') =~ s/\s+/ /gr, 0, 60)
              : !@$forms ? 'empty-forms'
              : 'embed-unsafe';
      warn "pcl-raw\tbody-decl:$why\n";
    }
    for my $s (@side_snaps) {
      splice @{ $self->{$s->[0]} }, $s->[1] if $self->{$s->[0]};
    }
    _ppi_state_restore($ppi_snap);
    return undef;
  }
  return $forms;
}

# task #78 step 2: anonymous `sub { … }` in expression position — the
# raw_lambda sibling of the map/grep/sort/eval re-host.  Returns v1's exact
# lambda WRAPPER as one CLForm (`&rest %_args` + `@_` flatten +
# *pcl-caller-wantarray* snapshot + the :p-return catch), with the body
# lowered like a named sub body (_lower_body_regime: void regime + tail
# caller-context restore).  Unlike a NAMED sub (hoisted, so _lower_sub
# clears _let_bound_vars), an anon sub CLOSES OVER the enclosing lexicals —
# the live sets are kept, only body-local additions are unwound.  undef =
# decline (v1's parse_block_as_function text as before).
sub _lower_embedded_anon {
  my ($self, $block) = @_;
  my $census = $ENV{PCL_E2_RAW_CENSUS};
  my @stmts = grep { ref $_ && $_->significant && !$_->isa('PPI::Statement::Null') }
              $block->schildren;
  if (!@stmts) {
    # Empty sub {}: v1's wrapper with the empty :void body (a body-less let
    # prints "(let ((*wantarray* :void)))" — same nil value, whitespace-only
    # vs v1's text).  Normalized structurally per task #78 E2.final.
    return ['lambda', ['list', '&rest', '%_args'],
            ['let', ['list',
                     ['list', '@_', ['p-flatten-args', '%_args']],
                     ['list', '*pcl-caller-wantarray*', '*wantarray*']],
             ['catch', ':p-return',
              ['block', 'nil',
               Pl::CLForm::ctx_bind(':void')]]]];
  }

  # Same conservative declines as the block form: package switches need v1's
  # revert wrapper (the native nested-package branch relies on p-sub's
  # dynamic bind, absent in a bare lambda); a tail declaration converts when
  # the $decl_tail machinery covers its value semantics (s307).
  if (@{ $block->find('PPI::Statement::Package') || [] }) {
    warn "pcl-raw\tanon-decl:package\n" if $census;
    return undef;
  }
  if ($stmts[-1]->isa('PPI::Statement::Variable')
      && !$self->_tail_decl_convertible($stmts[-1])) {
    warn "pcl-raw\tanon-decl:tail-decl\n" if $census;
    return undef;
  }

  my $env = $self->environment;
  my $ppi_snap    = _ppi_state_snapshot(@stmts);
  my %saved_lex   = %{ $self->{_live_lex} // {} };
  my %saved_lb    = %{ $self->{_let_bound_vars} // {} };
  my $scope_depth = scalar @{ $env->scope_stack };
  my @side_snaps  = map { [$_, scalar @{ $self->{$_} // [] }] }
                    qw(_captured_decls _hoisted_decls _hoisted_defs
                       _hoisted_def_lines _sched_defs _sched_lines);

  $env->in_subroutine($env->in_subroutine + 1);
  my $forms = eval {
    # Fresh context: the enclosing sub's :void regime does not reach into
    # the lambda (its dynamic *wantarray* at call time is the CALLER's).
    local $env->{wa_void_active} = 0;
    my $vi = Pl::VarAnnotator->analyze(\@stmts, undef, $self->_cur_sub_info, $self);
    $env->push_scope;
    [ $self->_lower_body_regime(\@stmts, $vi) ];
  };
  $env->in_subroutine($env->in_subroutine - 1);
  pop @{ $env->scope_stack } while @{ $env->scope_stack } > $scope_depth;
  $self->{_live_lex} = \%saved_lex;
  $self->{_let_bound_vars} = \%saved_lb;

  if (!$forms || !@$forms || grep { _embed_form_unsafe($_) } @$forms) {
    if ($census) {
      my $why = !$forms ? 'lower-died: ' . substr(($@ // '') =~ s/\s+/ /gr, 0, 60)
              : !@$forms ? 'empty-forms'
              : 'embed-unsafe';
      warn "pcl-raw\tanon-decl:$why\n";
    }
    for my $s (@side_snaps) {
      splice @{ $self->{$s->[0]} }, $s->[1] if $self->{$s->[0]};
    }
    _ppi_state_restore($ppi_snap);
    return undef;
  }
  return ['lambda', ['list', '&rest', '%_args'],
          ['let', ['list',
                   ['list', '@_', ['p-flatten-args', '%_args']],
                   ['list', '*pcl-caller-wantarray*', '*wantarray*']],
           ['catch', ':p-return',
            ['block', 'nil', @$forms]]]];
}

# Embed-safety scan — shared implementation lives in Pl::CLForm (PExpr's
# hash-constructor route needs it too and must not depend on Parser2).
sub _embed_form_unsafe { Pl::CLForm::embed_unsafe($_[0]) }

# Cached self-weakened closure installed as the fallback parser's `_v2_embed`
# slot (a plain coderef, so PExpr needs no Parser2 knowledge).
sub _embed_hook {
  my ($self) = @_;
  return $self->{_embed_hook} //= do {
    weaken(my $weak = $self);
    sub { $weak ? $weak->lower_embedded_block(@_) : undef };
  };
}

# ---------------------------------------------------------------- expressions

# ONE expression compiler (Phase A of docs/plan-one-compiler-s411.md): parse
# once, lower once.  $ctx describes the expression's position:
#   undef → scalar; 1 → list (a foreach list `2..$n` is a range, not a
#   scalar flip-flop); ':void' → statement position; 'inherit' →
#   return/sub-tail (the callee sees the CALLER's *wantarray*).
# History: until s411 this function first tried a second, strict-subset
# generator (ExprToCL2) on a discarded parse and re-parsed the same tokens
# through v1 when it declined — 88 % of the time.  Measured, the two
# generators' whole emission difference was two rules, both now Kind-A
# gates inside ExprToCL (`insensitive-call`, `elem-setf`); the second parse,
# the token snapshot/restore around it, the string context encoding and the
# native/fallback census went with it.
sub _lower_expr {
  my ($self, $parts, $stmt, $ctx) = @_;
  my @parts = _strip_semi(@$parts);
  die "Parser2: empty expression" unless @parts;
  $self->_seam_note_expr(\@parts) if _seam_census();

  # v2's ctx → PExpr's numeric context (SCALAR_CTX 0, LIST_CTX 1, VOID_CTX 2,
  # INHERIT_CTX 3).  'inherit' MUST map to INHERIT_CTX so a context-sensitive
  # operator emits its runtime *wantarray* check (`return 1..4` → range in
  # list context, flip-flop in scalar); collapsing it to scalar always
  # produced flip-flop.
  my $fb_ctx = !defined $ctx        ? 0
             : "$ctx" eq '1'        ? 1
             : "$ctx" eq 'inherit'  ? 3
             : "$ctx" eq ':void'    ? 2
             :                        0;
  $self->_gate_seam_my_shadow(@parts);
  # ONE parse, ONE generator, no capture: nothing emits v1 text during an
  # expression lowering any more — an embedded block the structural route
  # declines is compiled by v1 INSIDE the hook (lower_embedded_block →
  # _embed_via_v1, under its own capture_v1) — so this parse needs neither a
  # scratch section nor a drain.  Parser2::parse asserts the seam parser's
  # standing section is still empty at the end (assert_seam_clean).
  my $p = $self->fallback_parser;
  my $form = do {
    # The embedded-block hook is live for this whole parse: every block
    # PExpr meets is answered by lower_embedded_block.
    local $p->{_v2_embed} = $self->_embed_hook;
    # E2.final root flip: the form entry (gen_node_form) — the tree's raw
    # residue is only the genuinely-declining subtrees.  The two facts are
    # what the Kind-A rules in ExprToCL read.
    $p->_parse_expression_form(\@parts, $stmt, $fb_ctx,
      sub_info => $self->_cur_sub_info,
      lexicals => $self->{_let_bound_vars} // {});
  };
  die "Parser2: expression fallback failed for: " . join(' ', map { $_->content } @parts)
    unless defined $form;
  $self->_seam_lex_assign_fix($form);
  return $form;
}

# Legacy-boundary parity: the old pipeline rewrites (p-scalar-= $x …) to
# (p-my-= $x …) for let-bound names inside _emit, which the fallback seam
# bypasses (v2-native assignments never need this — they are lowered as
# p-my-=/setf forms).  Structural heads swap in place; raw residue chunks
# (declined subtrees) get v1's own text rewrite.
sub _seam_lex_assign_fix {
  my ($self, $form) = @_;
  my $lb = $self->{_let_bound_vars} // {};
  return unless %$lb;
  # `my $w; $w = sub { … $w->(…) … }` is a REFERENCE CYCLE: the closure
  # captures the very variable that holds it, so perl frees NEITHER — and this
  # runs once per seam expression.  Measured s406 (task #128): two CVs and
  # their pads leaked per transpile, ~8.5 kB for a 40-character eval string and
  # ~150 kB for a 1.4 kB one, growing linearly with no plateau — which is that
  # task's original report, "~6 GB after ~1400 eval requests" in a long-lived
  # `pl2cl --server`.  __SUB__ (feature current_sub, on under `use v5.30`)
  # recurses without naming itself, so the closure dies with its caller.
  my $walk = sub {
    my ($f) = @_;
    if (Pl::CLForm::is_raw($f)) {
      for my $var (keys %$lb) {
        my $pat = quotemeta("(p-scalar-= $var");
        $$f =~ s/$pat(?=[\s)])/(p-my-= $var/g;
      }
      return;
    }
    return unless ref $f eq 'ARRAY';
    $f->[0] = 'p-my-='
      if @$f >= 2 && !ref $f->[0] && $f->[0] eq 'p-scalar-='
      && !ref $f->[1] && $lb->{$f->[1]};
    __SUB__->($_) for @$f;
  };
  $walk->($form);
}

# Apply v1's _auto_defined_cond to a loop condition that lowered through the
# fallback seam: `(p-scalar-= $x (p-each …))` → wrapped in (p-defined $x),
# bare `(p-readline …)` → `(progn (p-setf $_ …) (p-defined $_))`, etc.
# Since the E2.final root flip the fallback returns TREES, so the four text
# rewrites are decided structurally here; a raw root (PARSE ERROR shape)
# still delegates to v1's text matcher.
my %AUTO_DEFINED_HEAD = map { $_ => 1 } qw(p-each p-readdir p-readline p-glob);
sub _auto_defined_raw {
  my ($self, $cond) = @_;
  return raw($self->fallback_parser->_auto_defined_cond($$cond))
    if Pl::CLForm::is_raw($cond);
  return $cond unless ref $cond eq 'ARRAY' && @$cond && !ref $cond->[0];
  my ($head, @a) = @$cond;
  if ($head =~ /^p-(?:scalar|my)-=$/ && @a == 2 && !ref $a[0] && $a[0] =~ /^\$/
      && _auto_defined_call($a[1])) {
    return ['progn', $cond, ['p-defined', $a[0]]];
  }
  # (`setf` as well as `p-setf`: ExprToCL's elem-setf rule writes a let-bound
  # container's element through CL setf directly, s411)
  if (($head eq 'p-setf' || $head eq 'setf') && @a == 2 && ref $a[0] eq 'ARRAY'
      && !ref $a[0][0] && $a[0][0] =~ /^p-(?:gethash|aref)$/
      && _auto_defined_call($a[1])) {
    return ['p-defined', $cond];
  }
  if ($head eq 'p-setf' && @a == 2 && !ref $a[0] && $a[0] eq '$_'
      && _auto_defined_call($a[1])) {
    return ['progn', $cond, ['p-defined', '$_']];
  }
  if ($head eq 'box-set' && @a == 2 && _auto_defined_call($a[1])) {
    # Ternary-lvalue scalar assignment (`($c ? $a : $b) = readdir(D)`,
    # defins.t t10) — box-set returns the TARGET BOX, so defined() reads the
    # just-assigned value.
    return ['p-defined', $cond];
  }
  if (_auto_defined_call($cond)) {
    return ['progn', ['p-setf', '$_', $cond], ['p-defined', '$_']];
  }
  return $cond;
}

# An each/readdir/readline/glob call form, possibly under the emitter's
# CONTEXT BIND — `(p-scalar-ctx …)` since #281 item 1, and still a bare `let`
# where two variables are bound together — or a raw chunk with that text shape
# (a declined subtree in the RHS position).  BOTH spellings must be seen
# through: a context wrap the detector cannot see past silently drops perl's
# implicit defined() and turns `while (my $l = <FH>)` into a loop that stops on
# a "0" line (caught by the corpus A/B when the macros landed, s414).
my %CTX_MACRO_HEAD = map { $_ => 1 } qw(p-list-ctx p-scalar-ctx p-void-ctx p-caller-ctx);
sub _auto_defined_call {
  my ($f) = @_;
  if (Pl::CLForm::is_raw($f)) {
    return $$f =~ /^(?:\(let \(\(\*wantarray\* (?:nil|t)\)\) |\(p-(?:list|scalar|void|caller)-ctx )?\(p-(?:each|readdir|readline|glob)\b/;
  }
  return 0 unless ref $f eq 'ARRAY' && @$f && !ref $f->[0];
  return 1 if $AUTO_DEFINED_HEAD{$f->[0]};
  return _auto_defined_call($f->[-1]) if $f->[0] eq 'let' && @$f >= 3;
  return _auto_defined_call($f->[-1]) if $CTX_MACRO_HEAD{$f->[0]} && @$f >= 2;
  return 0;
}

# -------------------------------------------------- #189: writes through @_

# perl's @_ elements are ALIASES of the caller's arguments, so `$_[0] = …`
# inside a sub assigns the CALLER's variable.  PCL can only honour that when
# the caller handed over a BOX, and boxing every call argument is off the table
# (DECIDED: no blanket boxing of call arguments).  So the rare fact is detected
# where it lives — the callee's body — carried on sub_info, and consumed at
# call sites as one more VarAnnotator boxing event (`arg-to-writer`), exactly
# the way `chomp $x` already forces $x boxed.
#
# CONSERVATIVE BY CONSTRUCTION (fable-answers-s323.md §1.1): every @_ / $_[N]
# occurrence must be a PROVEN read, and the aliasing ESCAPES (\$_[N], \@_,
# `&callee;`, `goto &sub`, handing @_ to an unknown callee) count as writes.
# A false positive costs one boxed argument; a false negative re-creates the
# silent dirname bug with only the runtime's "Cannot modify non-boxed value"
# warning as a witness.  That warning stays as the backstop for what this
# scan cannot see (coderef calls, method dispatch, cross-file callees).

# Builtins that CONSUME values, so handing them @_ or $_[N] copies rather than
# aliases.  Anything not listed is treated as a callee that may alias.
# map/grep are NOT here: they alias $_ to their list elements, so they get the
# same body-scan rule as a foreach (see _map_grep_topic_writes) — listing them
# as value consumers was a probe-found false negative (s332).
my %ARG_VALUE_FN = map { $_ => 1 } qw(
  scalar defined ref exists delete wantarray return
  join sprintf printf print say die warn croak confess carp cluck
  push unshift shift pop splice sort reverse keys values each
  split length uc lc ucfirst lcfirst sprintf index rindex sprintf abs int);

# The Word whose argument list $node sits in — paren form `f(… $node …)` or
# bare form `f $node`.  Undef when $node is not in an argument position.
sub _arg_owner_word {
  my ($node) = @_;
  for (my $p = $node->parent; $p; $p = $p->parent) {
    last if $p->isa('PPI::Structure::Block');       # a block is not an arg list
    if ($p->isa('PPI::Structure::List')) {
      my $w = $p->sprevious_sibling;
      return ($w && $w->isa('PPI::Token::Word')) ? $w : undef;
    }
  }
  # Bare form: walk left over the comma-separated list to its leading Word.
  my $prev = $node->sprevious_sibling;
  while ($prev) {
    return $prev if $prev->isa('PPI::Token::Word');
    last if $prev->isa('PPI::Token::Operator') && $prev->content ne ',';
    $prev = $prev->sprevious_sibling;
  }
  return undef;
}

my $ASSIGN_OP_RE = qr/^(?:=|\*\*=|\|\|=|&&=|\/\/=|x=|<<=|>>=|[-+.*\/%|&^]=)$/;

# `$_[N]` (SYM is the '$_' Symbol, SUB its '[…]' subscript): written?
sub _args_elem_written {
  my ($sym, $subscript) = @_;
  my $next = $subscript->snext_sibling;
  if ($next && $next->isa('PPI::Token::Operator')) {
    my $c = $next->content;
    return 1 if $c =~ $ASSIGN_OP_RE || $c eq '++' || $c eq '--' || $c eq '=~';
  }
  my $prev = $sym->sprevious_sibling;
  if ($prev) {
    return 1 if $prev->isa('PPI::Token::Cast') && $prev->content eq '\\';   # \$_[0]
    return 1 if $prev->isa('PPI::Token::Operator')
             && ($prev->content eq '++' || $prev->content eq '--');
  }
  my $w = _arg_owner_word($sym) or return 0;
  my $n = $w->content;
  return 1 if Pl::VarAnnotator::arg_writing_builtin($n);
  return 1 if $n eq 'substr' || $n eq 'vec' || $n eq 'pos';   # lvalue-capable
  return _map_grep_topic_writes($w, $sym) if $n eq 'map' || $n eq 'grep';
  return 0 if $ARG_VALUE_FN{$n};
  return 1;                       # unknown callee: the alias travels onward
}

# `@_` itself: does this occurrence let the aliases escape?
sub _args_array_escapes {
  my ($sym) = @_;
  my $prev = $sym->sprevious_sibling;
  return 1 if $prev && $prev->isa('PPI::Token::Cast') && $prev->content eq '\\';
  # RHS of an assignment COPIES (`my ($a,$b) = @_`), and so does a bare use in
  # a numeric/boolean position (`@_ == 2`, `if (@_)`, `"@_"`).
  return 0 if $prev && $prev->isa('PPI::Token::Operator')
           && $prev->content =~ $ASSIGN_OP_RE;
  my $w = _arg_owner_word($sym) or return 0;
  my $n = $w->content;
  return 1 if Pl::VarAnnotator::arg_writing_builtin($n);
  return _map_grep_topic_writes($w, $sym) if $n eq 'map' || $n eq 'grep';
  return 0 if $ARG_VALUE_FN{$n};
  return 1;
}

# A write to $_ that carries NO '$_' Symbol token: a bare `s///` or `tr///`
# (binding implicitly to $_) or an argument-less chomp/chop.  Without this,
# `s/x/y/ for @_` — the most common spelling of the aliasing loop — read as a
# no-write body (probe-found, s332).  /r never writes; a form bound with
# =~/!~ writes its explicit target, which the Symbol scan owns.
sub _implicit_topic_write {
  my ($root) = @_;
  my @toks = $root->isa('PPI::Token') ? ($root)
           : $root->can('find')       ? @{ $root->find('PPI::Token') || [] }
           :                            ();
  for my $t (@toks) {
    if ($t->isa('PPI::Token::Regexp::Substitute')
        || $t->isa('PPI::Token::Regexp::Transliterate')) {
      my $prev = $t->sprevious_sibling;
      next if $prev && $prev->isa('PPI::Token::Operator')
           && ($prev->content eq '=~' || $prev->content eq '!~');
      my $mods = eval { $t->get_modifiers } || {};
      next if $mods->{r};
      return 1;
    }
    if ($t->isa('PPI::Token::Word')
        && ($t->content eq 'chomp' || $t->content eq 'chop')) {
      my $next = $t->snext_sibling;
      return 1 if !$next
               || $next->isa('PPI::Token::Structure')
               || ($next->isa('PPI::Token::Word')
                   && $next->content =~ /^(?:for|foreach|if|unless|while|until|and|or)$/);
    }
  }
  return 0;
}

# map/grep ALIAS $_ to their list elements, so handing them @_ (or an alias
# variable) is a write exactly when the block/expr before the list writes $_ —
# the same rule as a foreach over @_.  SYM is the list argument; the nodes
# between the owner word and SYM are the block (block form) or the expression
# (expr form), in both the bare and the parenthesised spelling.
sub _map_grep_topic_writes {
  my ($w, $sym) = @_;
  my @before;
  for (my $s = $sym->sprevious_sibling; $s && $s != $w; $s = $s->sprevious_sibling) {
    push @before, $s;
  }
  return _nodes_write_var(\@before, '$_');
}

# A `foreach` over @_ ALIASES its loop variable to the arguments, so a write to
# the loop variable is a write to the caller's variable.  Returns true when the
# body writes it; a read-only iteration (`for my $x (@_) { push @o, $x }`) is
# the common case and stays unflagged.
sub _nodes_write_var {
  my ($nodes, $var) = @_;
  for my $root (@$nodes) {
    next unless ref $root;
    my @syms = $root->isa('PPI::Token::Symbol') ? ($root)
             : $root->can('find')               ? @{ $root->find('PPI::Token::Symbol') || [] }
             :                                    ();
    for my $sym (@syms) {
      next unless $sym->content eq $var;
      my $next = $sym->snext_sibling;
      if ($next && $next->isa('PPI::Token::Operator')) {
        my $c = $next->content;
        return 1 if $c =~ $ASSIGN_OP_RE || $c eq '++' || $c eq '--' || $c eq '=~';
      }
      my $prev = $sym->sprevious_sibling;
      if ($prev) {
        return 1 if $prev->isa('PPI::Token::Cast') && $prev->content eq '\\';
        return 1 if $prev->isa('PPI::Token::Operator')
                 && ($prev->content eq '++' || $prev->content eq '--');
      }
      my $w = _arg_owner_word($sym) or next;
      my $n = $w->content;
      return 1 if Pl::VarAnnotator::arg_writing_builtin($n)
               || $n eq 'substr' || $n eq 'vec' || $n eq 'pos';
      if ($n eq 'map' || $n eq 'grep') {
        return 1 if _map_grep_topic_writes($w, $sym);
        next;
      }
      return 1 unless $ARG_VALUE_FN{$n};
    }
    return 1 if $var eq '$_' && _implicit_topic_write($root);
  }
  return 0;
}

sub _foreach_over_args_writes {
  my ($list) = @_;
  my $compound = $list->parent or return 1;
  return 1 unless $compound->isa('PPI::Statement::Compound');
  my $block;
  for my $c ($compound->schildren) { $block = $c if $c->isa('PPI::Structure::Block') }
  return 1 unless $block;                       # shape not understood → write
  # Loop variable: the Symbol before the list, else the implicit $_.
  my $var = '$_';
  for my $c ($compound->schildren) {
    last if $c == $list;
    $var = $c->content if $c->isa('PPI::Token::Symbol');
  }
  return _nodes_write_var([$block], $var);
}

# THE FACT: does SUB write through @_ to its caller's variables?
sub _sub_writes_args {
  my ($self, $sub) = @_;
  my $block = $sub->block or return 0;

  # (1) @_ PASSTHROUGH.  `&callee;` and `goto &sub` hand our LIVE @_ to another
  #     sub, so ITS writes land on OUR caller.  `\&foo` (a code ref) and
  #     `&foo(…)` (an explicit arg list) do not.
  for my $sym (@{ $block->find('PPI::Token::Symbol') || [] }) {
    next unless $sym->raw_type eq '&';
    my $prev = $sym->sprevious_sibling;
    next if $prev && $prev->isa('PPI::Token::Cast') && $prev->content eq '\\';
    my $next = $sym->snext_sibling;
    next if $next && $next->isa('PPI::Structure::List');
    return 1;
  }
  for my $cast (@{ $block->find('PPI::Token::Cast') || [] }) {
    next unless $cast->content eq '&';           # &$code / &{$code}
    my $target = $cast->snext_sibling or next;
    my $after  = $target->snext_sibling;
    next if $after && $after->isa('PPI::Structure::List');
    return 1;
  }
  for my $w (@{ $block->find('PPI::Token::Word') || [] }) {
    next unless $w->content eq 'goto';
    my $n = $w->snext_sibling or next;
    return 1 unless $n->isa('PPI::Token::Word');   # `goto LABEL` carries nothing
  }

  # (2) Every @_ / $_[N] occurrence must be a proven read.  ($#_ is a count.)
  for my $sym (@{ $block->find('PPI::Token::Symbol') || [] }) {
    my $c = $sym->content;
    if ($c eq '@_') {
      my $p = $sym->parent;
      # foreach list gets the alias rule, not the escape rule.
      if ($p && $p->isa('PPI::Statement') && $p->parent
          && $p->parent->isa('PPI::Structure::List')
          && $p->parent->parent
          && $p->parent->parent->isa('PPI::Statement::Compound')) {
        return 1 if _foreach_over_args_writes($p->parent);
        next;
      }
      # Statement-modifier form `EXPR for @_` — same aliasing, no block: the
      # implicit $_ is the alias, so only a write to $_ in EXPR is a write.
      my $prev = $sym->sprevious_sibling;
      if ($prev && $prev->isa('PPI::Token::Word')
          && ($prev->content eq 'for' || $prev->content eq 'foreach')) {
        my @before;
        for my $sib ($p ? $p->schildren : ()) {
          last if $sib == $prev;
          push @before, $sib;
        }
        return 1 if _nodes_write_var(\@before, '$_');
        next;
      }
      return 1 if _args_array_escapes($sym);
      next;
    }
    next unless $c eq '$_';
    my $next = $sym->snext_sibling;
    next unless $next && $next->isa('PPI::Structure::Subscript')
             && ($next->start // '') eq '[';
    return 1 if _args_elem_written($sym, $next);
  }
  return 0;
}

# ------------------------------------------------------- context sensitivity

# A sub is context-INSENSITIVE when its caller's *wantarray* provably cannot
# be observed: no `wantarray` in the body, and every value it can return —
# each explicit `return EXPR` and the implicit last-statement value — is
# scalar-shaped.  Conservative: any doubt → sensitive (call sites keep the
# dynamic bind, exactly today's behaviour).
sub _sub_ctx_insensitive {
  my ($self, $sub) = @_;
  my $txt = $sub->block->content;
  return 0 if $txt =~ /\bwantarray\b/;

  my $breaks = $sub->block->find('PPI::Statement::Break') || [];
  for my $b (@$breaks) {
    my @k = _strip_semi($b->schildren);
    my $kw = shift @k;
    next unless $kw && $kw->content eq 'return';
    return 0 unless @k;                        # bare `return;` = () vs undef
    my ($expr) = _split_modifier(\@k);
    return 0 unless $self->_expr_scalar_rooted($expr);
  }

  my @stmts = grep { $_->significant && !$_->isa('PPI::Statement::Null') }
              $sub->block->schildren;
  return 0 unless @stmts;
  my $last = $stmts[-1];
  return 1 if $last->isa('PPI::Statement::Break');   # checked above
  if (ref($last) eq 'PPI::Statement' || $last->isa('PPI::Statement::Variable')) {
    my @k = _strip_semi($last->schildren);
    shift @k if @k && $k[0]->isa('PPI::Token::Word') && $k[0]->content eq 'my';
    my ($expr) = _split_modifier(\@k);
    # for `my $x = INIT` the statement value is the assignment value
    return $self->_expr_scalar_rooted($expr) ? 1 : 0;
  }
  return 0;                                   # compound/other tail → sensitive
}

# The expression's ROOT forces scalar shape: an arithmetic/comparison operator
# (coerces its operands, yields one scalar), a scalar variable, a number, or a
# string literal.  A funcall root propagates the caller's context → NOT scalar.
# NOT included: && || // (context-transparent to their right operand) and
# x (repeats lists in list context).
my %SCALAR_ROOT_OP = map { $_ => 1 } qw(+ - * / % ** < > <= >= == != .
                                        eq ne lt gt le ge <=> cmp !);
sub _expr_scalar_rooted {
  my ($self, $parts) = @_;
  my @parts = _strip_semi(@{ $parts // [] });
  return 0 unless @parts;
  # parse_expr_to_tree runs cleanup_for_parsing, which DESTRUCTIVELY rewrites the
  # `=>` operator to `,` (fat-comma auto-quote) on the SHARED PPI tokens.  This
  # analysis runs during sub pre-registration, BEFORE the body is lowered — if we
  # don't restore, the later _lower_expr sees `,` instead of `=>` and the fat
  # comma's key auto-quote never fires (`bless { key => shift }` lowers `key` as a
  # funcall — every OO constructor broke).  Snapshot + restore token content,
  # exactly as _lower_expr does around its own native attempt.
  my @snap = map { [$_, $_->content] }
             map { $_->isa('PPI::Node') ? $_->tokens : $_ } @parts;
  my $ok = eval {
    # ANALYSIS-ONLY parse (PExpr `analysis_only`, Phase B1): embedded block
    # bodies stay uncompiled, so this parse neither lowers through the
    # `_v2_embed` hook nor emits v1 text.  The `local $SIG{__WARN__} = sub {}`
    # that used to sit here silenced ONE line, PExpr's "Handle single node of
    # unknown type" warn (`sub u { ... }` triggered it); that warn is gone
    # since task #339, so the workaround left with its cause.
    my $expr_o = Pl::PExpr->new(
      e             => \@parts,
      environment   => $self->environment,
      parser        => $self->fallback_parser,
      analysis_only => 1,
    );
    my ($id) = $expr_o->parse_expr_to_tree(\@parts);
    while (1) {
      my $node = $expr_o->get_a_node($id);
      my $kids = $expr_o->get_node_children($id);
      if ($expr_o->is_internal_node_type($node)) {
        my $t = $node->{type} // '';
        if ($t eq 'tree_val' && @$kids == 1) { $id = $kids->[0]; next }
        return 0;
      }
      return 1 if ref($node) eq 'PPI::Token::Operator' && @$kids
                  && $SCALAR_ROOT_OP{ $node->content };
      return 1 if ref($node) && $node->isa('PPI::Token::Number');
      return 1 if ref($node) && $node->isa('PPI::Token::Quote');
      return 1 if ref($node) eq 'PPI::Token::Symbol' && !@$kids
                  && $node->content =~ /^\$\w+$/;
      return 0;
    }
  };
  $_->[0]->set_content($_->[1]) for @snap;   # restore pristine tokens (cleanup mutates =>→,)
  return $ok ? 1 : 0;
}

# ---------------------------------------------------------------- small helpers

sub _strip_semi {
  return grep { !($_->isa('PPI::Token::Structure') && $_->content eq ';') } @_;
}

# B-regime write freeze (docs/raw-numeric-verdict.md): a raw slot licensed by
# the USE-proof (coerce => 'num'/'str') stores every native write through the
# STRICT eager coercer — %pcl-to-number-strict / %pcl-to-string-strict die
# loudly on overload-capable refs and genuine dualvars instead of freezing
# them.  Uniform on every root write (a proven-arith RHS passes the check for
# one cheap typecheck at the rare write; uses stay unconditional raw reads).
# Plain unboxable entries (no coerce) pass through untouched.
sub _wrap_freeze {
  my ($vi_entry, $name, $form) = @_;
  if (my $c = $vi_entry->{coerce}) {
    $form = [$c eq 'num' ? '%pcl-to-number-strict' : '%pcl-to-string-strict',
             $form, "\"$name\""];
  }
  # S1 str-buffer slot: every plain write REPLACES the buffer with a fresh
  # adjustable fill-pointer string ((%pcl-str-append …) handles `.=`).
  $form = ['%pcl-str-buffer', $form] if $vi_entry->{strbuf};
  return $form;
}

# Split "EXPR if COND" style trailing statement modifiers at the top level.
sub _split_modifier {
  my ($parts) = @_;
  for my $i (1 .. $#$parts) {
    my $p = $parts->[$i];
    if ($p->isa('PPI::Token::Word') && Pl::PExpr::Config::is_statement_modifier($p->content)) {
      return ([@$parts[0 .. $i - 1]], $p->content, [@$parts[$i + 1 .. $#$parts]]);
    }
  }
  return ($parts, undef, undef);
}

sub _modifier_needs_fallback {
  my ($mod) = @_;
  return $mod && $mod !~ /^(?:if|unless)$/;
}

sub _apply_modifier {
  my ($form, $mod, $cond, $self, $stmt) = @_;
  return $form unless $mod;
  my $condform = $self->_lower_expr($cond, $stmt);
  return ['p-if', $condform, $form]          if $mod eq 'if';
  return ['p-if', ['p-!', $condform], $form] if $mod eq 'unless';
  die "Parser2 TODO: statement modifier '$mod'";   # unreachable (callers gate)
}

# `our $x` / `our @a` / `our (LIST)` [= INIT] → arrayref of runtime forms
# (possibly empty), or undef when the statement is not an `our` declaration.
# `our` names PACKAGE vars: no let — a defvar is hoisted to the section top
# (`_captured_decls`, read under the section's in-package), and the
# assignment lowers as a plain package-var assignment through the ordinary
# expression machinery (p-scalar-= / p-array-= / p-hash-= / p-list-=).
# NB: like v1, the `our` alias's lexical VISIBILITY is not modelled — the
# name simply resolves per-package (an `our $x` followed by `package Foo;
# print $x;` reads Foo::$x in both pipelines; Perl reads the alias).
sub _lower_our_decl {
  my ($self, $stmt) = @_;
  my @k = _strip_semi($stmt->schildren);
  return undef unless @k >= 2
    && $k[0]->isa('PPI::Token::Word') && $k[0]->content eq 'our';
  # `our \$T = \$::TODO;` (declared_refs) — the `\` belongs to the ASSIGNMENT,
  # not to the declaration: the statement declares the package cell for $T and
  # then aliases it, so the names are read past the cast and the cast stays in
  # the expression tail, where p-setf's \-cast place turns it into an alias.
  # Same DECLARE-then-lower-the-tail shape as `our $count++` (family F-B).
  my $ni = 1;
  $ni = 2 if @k > 2 && $k[1]->isa('PPI::Token::Cast') && $k[1]->content eq '\\';
  my @names;
  if ($k[$ni]->isa('PPI::Token::Symbol')) {
    @names = ($k[$ni]->content);
  } elsif ($k[$ni]->isa('PPI::Structure::List')) {
    @names = map  { $_->content }
             grep { $_->isa('PPI::Token::Symbol') } map { $_->tokens } $k[$ni];
  }
  # A trailing statement modifier belongs to the STATEMENT, not to the
  # declaration: perl declares the package cell unconditionally (it is a
  # compile-time act) and makes only the TAIL conditional — `our $ok++,
  # return if $depth == 2` (op/sub.t's [perl #122845] closure recursion)
  # increments and returns only at depth 2, but $ok exists either way.  The
  # whole run used to go to _lower_expr with the modifier still attached,
  # which PExpr answered with "Fell through. Missing case: []" — i.e. the
  # statement was DROPPED (#138 family, silent-wrong: the assignment simply
  # never happened), and the no-tail spelling `our $z if C;` died outright
  # on the $bad check below.  Split it with the same helper the `my` path
  # uses and re-apply it to the lowered tail.  The split is only accepted
  # PAST the declared names, so a word that merely looks like a modifier in
  # declarator position (`our sub if() {…}`, op/lexsub.t) still reaches the
  # $bad check unchanged.  while/until/for/foreach are left attached, so
  # they keep today's announced drop (task #380; zero occurrences in
  # perl-tests + perl's own t/ + lib) rather than gaining a half-modelled
  # loop here.
  my ($mod, $cond);
  {
    my ($head, $m, $c) = _split_modifier(\@k);
    if ($m && !_modifier_needs_fallback($m) && @$head > $ni) {
      ($mod, $cond) = ($m, $c);
      @k = @$head;
    }
  }
  # The tail may be ANY operator, not only an assignment: perl's
  # `our $Verbose ||= 0;` (Exporter.pm) declares the package cell and then
  # runs a compound assignment on it, and `our $count++;` (op/inccode.t's
  # tied FETCH, op/repeat.t) declares it and then evaluates `$count++` as an
  # ordinary expression.  Both are the same statement shape — DECLARE, then
  # lower `NAMES <tail>` through the expression machinery — and both are the
  # `our` twin of `my VAR <non-'=' tail>` (_lead_decl_with_expr_tail).  The
  # only thing that must be rejected here is a tail that is not an operator
  # at all (an unrecognised declaration shape), so ask that and nothing more.
  # NB the operator-vs-assignment distinction still matters to ONE consumer,
  # _tail_decl_convertible: in eval-tail position the value of `our $x = 1`
  # is the assignment's, of `our $x++` the post-increment's — both are simply
  # this function's last form, so it accepts both.
  my $bad = !@names
    || (grep { !/^[\$\@\%]\w+$/ } @names)
    || (@k > $ni + 1 && !$k[$ni + 1]->isa('PPI::Token::Operator'));
  die "Parser2 TODO: unsupported our declaration: " . $stmt->content if $bad;
  for my $n (@names) {
    # A defvar of a let-bound name would proclaim it special and poison the
    # lexical lets (see _forward_global_decls) — shadowing our/my → v1.
    die "Parser2 TODO: our '$n' shadows a my-lexical\n"
      if $self->{_let_bound_vars}{$n};
    # Inside a nested-`package X;` region (D1/E1.5) the declared cell belongs
    # to X, but this defvar is read in the SECTION's package — qualify it, and
    # register the our-variable so the fallback expression path qualifies
    # unqualified USES identically (v1's mechanism: ExprToCL "Qualify `our`
    # variables", keyed on the Environment's current package, which pops back
    # at the block end).  Segment-level `our` stays bare — the section's
    # in-package already reads it into the right package — so emission
    # everywhere else is byte-identical.
    my $cur = $self->environment->current_package // 'main';
    my $prefix = '';
    if ($cur ne ($self->cur_pkg // 'main')) {
      $self->environment->add_our_variable($cur, $n);
      $prefix = ($cur =~ /::/ ? "|$cur|" : $cur) . '::';
    }
    push @{ $self->{_captured_decls} },
      global_decl_form("${prefix}${n}", _fresh_container($n));
  }
  # Declaration only.  With a modifier (`our $z if C;`) the condition still
  # RUNS — perl evaluates it at runtime for its side effects, exactly as the
  # `my` path's @declmod_eval does — it just has nothing to guard.
  return [ $mod ? ($self->_lower_expr($cond, $stmt, ':void')) : () ]
    if @k == $ni + 1;
  # `NAMES = RHS` minus the `our` keyword is a plain (list) assignment.
  # NOTE (D20 reverted, D23): a single-scalar init MUST go through p-scalar-=
  # (box-set invalidates the box's sv/nv caches), NOT `(setf (p-box-value …))`.
  # The raw setf bypasses cache invalidation, so a value a BEGIN block set at
  # compile time was still read back from the stale string cache
  # (begin-end-01 t13/14: `our $c = "default"; BEGIN { $c = "x" }` printed "x";
  # real perl prints "default" — the runtime our-init runs in source order and
  # clobbers the BEGIN value).  v1 emits the raw setf and has this stale-cache
  # divergence; v2 deliberately matches perl here, not v1.
  my $form = $self->_lower_expr([@k[1 .. $#k]], $stmt);
  return [ $mod ? _apply_modifier($form, $mod, $cond, $self, $stmt) : $form ];
}

# `my @a` / `my %h` / `my ($p, @q)` [= INIT] → (\@var_names, $has_init); else ().
sub _multi_decl {
  my ($self, $stmt) = @_;
  my @k = _strip_semi($stmt->schildren);
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

sub _fresh_container {
  my ($var) = @_;
  my $sigil = substr($var, 0, 1);
  return '(make-array 0 :adjustable t :fill-pointer 0)' if $sigil eq '@';
  return "(make-hash-table :test 'equal)"               if $sigil eq '%';
  return '(make-p-box nil)';
}

# Textual "does this code read NAME" scan.  For a SCALAR the match must not
# fire on `$name[…]` / `$name{…}` — those are elements of @name/%name, not
# the scalar.  The list-decl self-ref path binds a flagged name to
# `(p-box-init $name)` — a direct READ of the outer variable — so a false
# positive there is not the harmless single-scalar kind: with no outer
# scalar in existence it emits an unbound package-var read (measured:
# Text::Balanced's `my ($class, $func) = ($class[$i], $func[$i]);` crashed
# `|Text::Balanced|::$class is unbound` the moment v2 lowered the module).
sub _reads_name_rx {
  my ($n) = @_;
  return substr($n, 0, 1) eq '$' ? qr/\Q$n\E\b(?![\[\{])/ : qr/\Q$n\E\b/;
}

# Whole-statement fallback: run one statement through the ORIGINAL parser's
# _process_element into a scratch section, then split the buckets — preamble/
# declarations/definitions lines are hoisted to the file top (they carry
# defvar/defconstant/eval-when forms that need top-level-ness), runtime lines
# embed at the statement's position as a raw form.  This is the statement-level
# twin of the _lower_expr fallback seam.
#
# Statements whose v1 lowering opens a scope spanning to block end (`local`,
# `delete local` — counted in _local_let_depth) leave the runtime text with
# open parens; _fallback_stmt_capture reports that surplus so _lower_block can
# nest the block remainder inside (raw_wrap).  _fallback_stmt itself is for
# self-contained statements only and treats a surplus as an unsupported shape.
# A `my` INSIDE a fallback expression/statement that re-declares a name
# already live in v2's scope must NOT reach the seam under that name: v1's
# seam machinery consults _let_bound_vars (which v2 pre-populated with the
# outer name), so the inner `my $x` emits a plain (p-my-= $x …) ASSIGNMENT
# instead of a fresh binding — the "shadow" writes through the OUTER lexical.
# Observed: `my $x = "outer"; my @r = map { my $x = $_ * 2; $x } (1,2,3);`
# left $x == 6; same for `do { my $x = …; }` and anon-sub bodies.
# W8.5: RENAME the shadow to a fresh `$x__shadow__N` within its Block (its
# whole Perl scope) when safe; otherwise die → v1 (interpolated uses,
# re-shadows, string eval — see _shadow_rename_blocker).  A same-level `my`
# in a fallback statement stays untouched: that is the sanctioned seam
# contract (v2 registered the name; v1 assigns into the existing binding).
sub _gate_seam_my_shadow {
  my ($self, @parts) = @_;
  my $live = $self->{_live_lex};
  return unless $live && %$live;
  for my $part (@parts) {
    next unless ref $part && $part->isa('PPI::Node');
    # nested => 1 and the triple's own $block: a `my` directly under a
    # NON-block $part has no block and is skipped — that is the sanctioned
    # same-level seam contract; the fallback root itself counts as a Block.
    for my $d (_decl_syms_under($part, nested => 1)) {
      my ($w, $s, $block) = @$d;
      next unless $block;
      next unless $live->{$s->content};
      # `state` has per-instance semantics driven by state_var_renames —
      # renaming the token would bypass that machinery; always gate.
      my $why = $w->content eq 'state' ? 'state'
              : $self->_shadow_rename_blocker($block, $s, 'eval_ok');
      die "Parser2 TODO: my-shadow of live lexical " . $s->content
        . " inside fallback block ($why)\n" if $why;
      $self->_rename_decl_within($block, $s,
        $s->content . '__shadow__' . $self->{_shadow_rename_counter}++);
    }
  }
}

sub _fallback_stmt {
  my ($self, $stmt, %opt) = @_;
  $self->_gate_seam_my_shadow($stmt);
  my ($text, $opens) = $self->_fallback_stmt_capture($stmt, %opt);
  die "Parser2 TODO: statement fallback left $opens open scope(s): " . $stmt->content
    if $opens;
  return () unless defined $text;
  # A COMMENT-ONLY raw — pragma markers like ";; use integer (pragma)" /
  # ";; no strict (no-op)" — carries no code; the pragma's compile-side
  # effect happened during the capture above.  Drop it entirely: the
  # comment is v1 cosmetics, and a comment-tail raw is embed-unsafe, so
  # these markers declined 27 embedded blocks on the corpus (s308b).
  {
    (my $code = $text) =~ s/^\s*;;[^\n]*$//mg;
    return () if $code !~ /\S/;
  }
  return raw($text);
}

# A statement v1's local machinery owns: `local …;` (Statement::Variable with
# declarator `local`) or standalone `delete local …;` (plain Statement).
sub _is_local_stmt {
  my ($self, $stmt) = @_;
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

# `local` scopes to the end of the enclosing block: v1 emits the save/restore
# form(s) OPEN and counts them in _local_let_depth (closed at block end by
# emit-order bookkeeping).  v2's tree structure does the same thing by
# construction — nest the lowered block remainder inside the open text via
# raw_wrap.  Degenerate locals that open no scope (`local $#a = N`, skipped
# stash locals) come back with 0 opens and embed as a plain raw statement.
sub _lower_local {
  my ($self, $stmt, $rest, $vi, $tail_ctx) = @_;
  my ($text, $opens) = $self->_fallback_stmt_capture($stmt);
  my @rest_forms = $self->_lower_block($rest, $vi, $tail_ctx);
  return ((defined $text ? (raw($text)) : ()), @rest_forms) unless $opens;
  return raw_wrap($text, $opens, @rest_forms);
}

sub _fallback_stmt_capture {
  my ($self, $stmt, %opt) = @_;
  $self->_seam_note_stmt($stmt) if _seam_census();
  my $p = $self->fallback_parser;
  # A Compound statement (for/foreach/while/if/bare block) confines every
  # `my` inside it — Perl scopes even a loop-head decl (`for (my $i = …;…)`)
  # to the statement.  v1's _process_element registers such decls in its
  # never-shrinking _let_bound_vars accumulator; without a restore the name
  # leaks into later seam lowerings at this level, and the string-eval
  # capture alist would reference a `let` variable whose binding has closed
  # (unbound-variable abort at load — bop.t's %res section).  Non-Compound
  # statements (`my $x = …;` at this level) must keep their registrations.
  my $confines = $stmt->isa('PPI::Statement::Compound');
  my %saved_lb;
  %saved_lb = %{ $self->{_let_bound_vars} // {} } if $confines;
  # Reflect the statement's REAL block-nesting into the isolated capture: v1's
  # _process_element keys several bucket decisions on `_block_depth > 0` — most
  # importantly a bareword `require Module` nested in a block/sub stays INLINE
  # (runtime `(p-require …)`) instead of being hoisted to the definitions bucket
  # as `(p-eval-always (p-require …))`.  Hoisting is fatal for a `require` guarded
  # by an enclosing `SKIP:`/`if` block (scalar.t's `require B` / `require threads`)
  # — the module then loads unconditionally at top level and dies (XS).  A fresh
  # capture context would reset _block_depth to 0, so the nesting is passed in.
  my $in_block = 0;
  for (my $a = $stmt->parent; $a; $a = $a->parent) {
    ($in_block = 1), last if $a->isa('PPI::Structure::Block');
  }
  # #226: inside an eval region's `package X;` the READER package is still the
  # section's (an eval body is read in `:pcl` — the thunk lambda cannot switch
  # it), so a `use` must name its import target EXPLICITLY or Role::Tiny's
  # import records `main` (Role-Tiny create-hook.t).  v1 already emits that
  # `:into` — _process_include_statement's branch keyed on _block_depth plus
  # _seam_outer_pkg, written for the same reader-vs-Perl package split inside a
  # do{}/eval{} block — so SUPPLY THOSE TWO FACTS rather than adding a second
  # predicate.  The condition is the one every other qualification site uses:
  # the Environment's package differs from the section's.
  my $seam_pkg_region =
    ($self->environment->current_package // 'main') ne ($self->cur_pkg // 'main');
  local $p->{_seam_outer_pkg} = $seam_pkg_region ? ($self->cur_pkg // 'main')
                                                 : $p->{_seam_outer_pkg};
  # THE seam call: v1 lowers the statement on the fallback parser inside
  # Pl::Parser::capture_v1 (its emission state isolated, the embedded-block
  # hook installed so blocks inside the statement go structural), and the
  # capture hands back the drained buckets by name.
  my $cap = $p->capture_v1(sub { $p->_process_element($stmt) },
                           bucket      => 'runtime',
                           block_depth => ($in_block || $seam_pkg_region) ? 1 : 0,
                           hook        => $self->_embed_hook);
  my $opens = $cap->{opens};
  # A BEGIN/END/… block's p-BEGIN lands in v1's `definitions` bucket, alongside
  # sub definitions.  v2 emits native sub defs to @defs and this fallback's
  # `definitions` to _captured_decls, which is assembled BEFORE @defs — so a
  # plain route would run the BEGIN before the subs it calls exist.  For a
  # scheduled block, route its definitions to _sched_defs (assembled AFTER defs,
  # before runtime), so every sub is defined before any BEGIN runs and every
  # BEGIN runs before the runtime code (matching v1/Perl).  preamble/
  # declarations (defvars from an inner `our`, etc.) still go to _captured_decls.
  my $defs_target = $opt{sched} ? $self->{_sched_defs} : $self->{_captured_decls};
  push @{ $self->{_captured_decls} }, @{ $cap->{decls} };
  push @$defs_target, @{ $cap->{defs} };
  push @{ $self->{_sched_lines} }, (_src_pos($stmt)) x @{ $cap->{defs} } if $opt{sched};
  my @runtime = @{ $cap->{runtime} };
  $self->{_let_bound_vars} = \%saved_lb if $confines;
  return (@runtime ? join("\n", @runtime) : undef, $opens);
}

# `my $x` / `my $x = INIT` → ($name, \@init_parts | undef); else ().
# Does the initialiser of `my $x = …` actually READ $x?  Asked at the TOKEN
# level, because a text scan cannot tell a scalar from an ELEMENT of the
# same-named container: `$attrs{$_}` is a slot of %attrs and `$a[0]` a slot of
# @a — different variables, which perl reads without touching $attrs/$a.
# PPI's ->symbol does exactly that canonicalisation, so ask it.  Interpolating
# quotes carry no Symbol tokens, so they keep a text scan — with the same
# subscript exclusion spelled out.  Non-interpolating literals are skipped
# entirely (sprintf2.t's `my $s = sprintf '%*2$s', …`: perl does not read $s).
# A false positive here costs a WHOLE-FILE gate — it cost ExtUtils::MM_Unix
# one, on `my $attrs = join " ", map { qq[$_="$attrs{$_}"] } sort keys %attrs;`
# (audit family F5, task #229).
# Tokens inside a BLOCK in the init (an anon-sub body, a map/grep block) go
# through _block_captures_name instead, so an INNER `my $x` shadow discounts
# its own uses (E4.1 M2 residue, s353: Moo's Method::Generate::Constructor
# declares an inner `my $constructor` inside the deferred sub — perl never
# reads the outer one).  A genuine closure capture of the outer $x still
# answers 1: the use resolves outside the block.
sub _init_reads_scalar {
  my ($self, $init, $name) = @_;
  (my $bare = $name) =~ s/^\$//;
  my (%inblk, @blocks);
  for my $el (@$init) {
    next unless $el->isa('PPI::Node');
    push @blocks, $el->isa('PPI::Structure::Block') ? $el : ();
    push @blocks, @{ $el->find('PPI::Structure::Block') || [] };
  }
  for my $b (@blocks) {
    # Only TOP-level blocks: a nested block's own capture test would call a
    # use shadowed one level up "resolves outside me" and false-refuse.
    my $nested = 0;
    for (my $p = $b->parent; $p && !$p->isa('PPI::Statement::Variable');
         $p = $p->parent) {
      $nested = 1, last if $p->isa('PPI::Structure::Block');
    }
    next if $nested;
    return 1 if $self->_block_captures_name($b, $bare, { $name => 1 });
    $inblk{ refaddr $_ } = 1 for $b->tokens;
  }
  for my $el (@$init) {
    for my $t ($el->isa('PPI::Node') ? $el->tokens : ($el)) {
      next if $inblk{ refaddr $t };
      if ($t->isa('PPI::Token::Symbol')) {
        return 1 if $t->symbol eq $name;
      } elsif ($t->isa('PPI::Token::Quote::Single')
            || $t->isa('PPI::Token::Quote::Literal')) {
        next;
      } elsif ($t->content =~ /(?<!\\)\$\{?\s*\Q$bare\E\b\}?\s*(?![\{\[])/) {
        return 1;
      }
    }
  }
  return 0;
}

sub _single_scalar_decl {
  my ($self, $stmt) = @_;
  my @k = _strip_semi($stmt->schildren);
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

sub _cond_parts {
  my ($cond) = @_;
  return map { $_->schildren } grep { $_->isa('PPI::Statement') } $cond->children;
}

# Remove the two DECORATIONS a declaration statement may carry between the
# declarator and the rest of the statement, so that every downstream
# decl-shape matcher (_single_scalar_decl / _multi_decl /
# _lead_decl_with_expr_tail / the span and capture scans) sees a plain
# declaration.  One walk, two independent halves:
#
#   (a) the typed-lexical CLASS word — `my Foo $f` → `my $f`, `our Foo $g = …`.
#       In a Variable statement `<my|our|state> <Word> <Symbol>` is
#       unambiguously a typed lexical (the bare Word can only be the class),
#       and it is runtime-inert (v1 discards it too).
#
#   (b) the ATTRIBUTE list — `my $x : shared = 1`, `my ($c,@g,%b) : teapots =
#       …`, `my $x : switch(10,foo(7,3)) : expensive`.  PPI does NOT spell
#       these as Token::Attribute inside a Statement::Variable (it does for
#       subs): they arrive as Operator(':') followed by a run of Words and
#       parenthesised argument Lists, terminated by `=`, `;` or end.
#
# Why (b) must happen HERE and not in a decl matcher: without it the ':' is
# just "some operator after the name", so `my $x : shared = 1;` matched
# _lead_decl_with_expr_tail — the `my VAR <non-'=' tail>` shape — and lowered
# as a bare `my $x` plus the void expression `$x : shared = 1`.  That printed
# an EMPTY $x where perl prints 1: a silent wrong, live in the tree since the
# scalar branch shipped and inherited by the container spelling at s393d.
# One pre-pass fixes every path instead of each matcher growing a ':' case
# (CLAUDE.md 11), and it is also the whole of #314 family F-A2 (op/attrs.t,
# uni/attrs.t — both TRANSPILE-FAIL on `my (…) : teapots = …`).
#
# The DROP is announced, not silent: an attribute on a lexical is never inert
# in perl — it calls MODIFY_<TYPE>_ATTRIBUTES in the declaring package, and
# perl makes it a compile error when nothing handles it.  Ignoring it is
# rule 12's effect-only ANNOUNCE case (the declaration still binds the right
# variable; only the hook does not run), so it says so once per distinct
# attribute per file.  See docs/not-supported.md §"Attributes on variable
# declarations".
sub _strip_decl_decorations {
  my ($self, $doc) = @_;
  for my $v (@{ $doc->find('PPI::Statement::Variable') || [] }) {
    my @k = $v->schildren;
    next unless @k >= 2
      && $k[0]->isa('PPI::Token::Word') && $k[0]->content =~ /^(?:my|our|state)$/;
    # (a) typed-lexical class word
    if (@k >= 3 && $k[1]->isa('PPI::Token::Word') && $k[2]->isa('PPI::Token::Symbol')) {
      $k[1]->remove;
      @k = $v->schildren;
    }
    # (b) attribute list: from the ':' up to (not including) the first token
    # that is neither an attribute name, its argument list, nor another ':'.
    next unless @k >= 3
      && ($k[1]->isa('PPI::Token::Symbol') || $k[1]->isa('PPI::Structure::List'))
      && $k[2]->isa('PPI::Token::Operator') && $k[2]->content eq ':';
    my (@drop, @named);
    for my $t (@k[2 .. $#k]) {
      if ($t->isa('PPI::Token::Operator') && $t->content eq ':') { push @drop, $t; next }
      if ($t->isa('PPI::Token::Word')) { push @drop, $t; push @named, $t->content; next }
      # A parenthesised argument list belongs to the attribute BEFORE it.
      if ($t->isa('PPI::Structure::List') && @named) {
        push @drop, $t;
        $named[-1] .= $t->content;
        next;
      }
      last;
    }
    $_->delete for @drop;
    for my $a (@named) {
      next if $self->{_attr_announced}{$a}++;
      warn "PCL: attribute `:$a` on a variable declaration is dropped "
         . "(MODIFY_*_ATTRIBUTES is not called; see docs/not-supported.md)\n";
    }
  }
  return;
}

# Scalar names declared by `my` in a condition head (`if (my $x = …)`,
# `while (my $i = …)`, chained `my $x = my $y`, list `my ($p,$q) = …`).  Perl
# scopes such a declaration to the whole construct (condition + branches/body),
# NOT to the enclosing block — so we wrap the lowered construct in a fresh
# `(let ((name (make-p-box nil)) …) …)` that lexically shadows any outer
# same-named var (v2 uses real lexical lets, so shadowing needs no renaming).
# Returns a deduped list of $names.  A `my` nested inside a block/anon-sub in the
# condition is NOT hoisted.  Dies (→ v1) on an array/hash my (needs a container
# init, not a boxed scalar cell).
sub _cond_my_names {
  my ($self, @conds) = @_;
  my (@names, %seen);
  for my $cond (grep { defined } @conds) {
    # a `my` inside a nested block/anon-sub within the condition is skipped
    # (nested => 0).  Scalar ($x) or container (@a / %h) — both scope to the
    # whole construct and are wrapped in a fresh let by _wrap_cond_mys
    # (_fresh_container picks the box/vector/table by sigil).
    for my $d (_decl_syms_under($cond, words => 'my', plain => 1)) {
      next if $seen{$d->[1]->content}++;
      push @names, $d->[1]->content;
    }
  }
  return @names;
}

# Wrap a lowered construct FORM in a fresh let binding boxed cells for the
# condition-declared @names (empty → FORM unchanged).
sub _wrap_cond_mys {
  my ($self, $form, @names) = @_;
  return $form unless @names;
  return ['let', ['list', map { ['list', $_, _fresh_container($_)] } @names], $form];
}

# ============================================================ seam census
# (docs/v2-transfer-plan.md T0.2, retargeted s411/Phase A.)  Under
# PCL_V2_SEAM_CENSUS=1, count every expression the one generator lowers and
# every STATEMENT that still goes to v1's statement layer whole, keyed by
# construct, and dump a TSV block to STDERR at the end of parse().  The
# statement histogram is the E5.3 port worklist (docs/plan-one-compiler-s411.md
# §4.1); the raw-residue census that says which SUBTREES still print as v1
# text is PCL_E2_RAW_CENSUS (Pl::CLForm::_raw_census).  Zero cost when the
# env var is unset.  (The per-node "blame frontier" that ranked ExprToCL2's
# porting worklist went with ExprToCL2.)

sub _seam_census { $ENV{PCL_V2_SEAM_CENSUS} ? 1 : 0 }

sub _seam_note_stmt {
  my ($self, $stmt) = @_;
  (my $class = ref $stmt) =~ s/^PPI::Statement(?:::)?//;
  $class = 'Plain' if $class eq '';
  my @k = $stmt->schildren;
  my $head = (@k && $k[0]->isa('PPI::Token::Word')) ? ':' . $k[0]->content : '';
  $self->{_seam_stmt}{"$class$head"}++;
}

# One expression through the generator; keyed by its first significant
# token so the histogram still says what SHAPES the corpus is made of.
sub _seam_note_expr {
  my ($self, $parts) = @_;
  $self->{_seam_expr}++;
  my $t = $parts->[0];
  my $desc = !ref $t ? '(?)'
           : $t->isa('PPI::Token::Word')     ? 'word:' . $t->content
           : $t->isa('PPI::Token::Symbol')   ? 'sym:' . substr($t->content, 0, 1)
           : $t->isa('PPI::Token::Magic')    ? 'magic:' . $t->content
           : $t->isa('PPI::Token::Operator') ? 'op:' . $t->content
           : do { (my $s = lc ref $t) =~ s/^ppi::(?:token::|structure::)?//; $s =~ s/::/-/g; $s };
  $self->{_seam_expr_head}{$desc}++;
}

sub _seam_census_dump {
  my ($self) = @_;
  my $tag = $self->has_filename ? $self->filename : '-';
  print STDERR join("\t", 'pcl-seam', 'totals', $tag,
                    'expr='      . ($self->{_seam_expr} // 0),
                    'seam-stmt=' . _hist_total($self->{_seam_stmt})), "\n";
  for my $cat (qw(stmt head)) {
    my $h = { stmt => $self->{_seam_stmt}, head => $self->{_seam_expr_head} }->{$cat} or next;
    print STDERR join("\t", 'pcl-seam', $cat, $_, $h->{$_}), "\n"
      for sort { $h->{$b} <=> $h->{$a} || $a cmp $b } keys %$h;
  }
}

sub _hist_total { my ($h) = @_; my $n = 0; $n += $_ for values %{ $h // {} }; return $n }

1;

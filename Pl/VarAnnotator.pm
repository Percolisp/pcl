package Pl::VarAnnotator;

# VarAnnotator — Gate-1 "may this my-scalar leave the box?" for the v2
# pipeline.  Conservative: any doubt → boxed (byte-identical semantics).
# Full design: docs/type-flow-and-codegen-plan.md §(s)/(e).
#
#   my $vi = Pl::VarAnnotator->analyze(\@stmts, $params, $known_subs, $host);
#   $vi->{'$sum'}{unboxable}   # 1 → emit raw let + setf; writes proven arith
#
# The tree annotator (_analyze_tree) is the DEFAULT since W12 (s276).
# _analyze_text — the s272 name-keyed TEXT-SCAN prototype — remains only as
# the fallback when a statement parse dies inside the tree walk or when no
# $host is supplied (the PCL_W12_OLD=1 escape hatch was deleted in s277).
#
#   _analyze_tree — the W12 annotator: per-statement parse_expr_to_tree
#     (the same OpcodeTree ExprToCL2 consumes) + a structural event walk.
#     Event vocabulary per name (docs/v2-completion-plan.md §W12):
#       write            statement-root `$x = RHS` — the ONE shape Parser2
#                        lowers natively (raw `setf` / let-init), so it is not
#                        a boxing event; only its RHS shape is checked
#       write-embedded   any deeper `=` (chained, in args, in conditions) or
#                        any write inside an expression-embedded block
#                        (do/map/grep/eval{}): these lower through the v1 seam
#                        as `(p-my-= …)` = box-set, which CANNOT store to a
#                        raw slot → the name must stay boxed.  The text scan
#                        missed this class entirely — three live v2
#                        miscompiles found while designing W12:
#                          $x = $y = 5;              ($y raw → box-set no-op)
#                          my $z = do { $x = 5 };
#                          map { $x = $y * 2 } @l;
#       write-cond       root write under a statement modifier (D12): the
#                        native setf path requires !$mod → seam → boxed
#       write-compound   ALL compound assigns incl. bitwise/string-bitwise (D24)
#       write-incdec     ++/-- either side (also covers ++($x=5) via subtree)
#       write-list       any `=` whose LHS is not a plain $scalar/element (D11)
#       ref-taken        \$x — marking the WHOLE `\` operand subtree also
#                        covers \substr($x…)/\vec/\pos, paren-less too (D15/D25)
#       regex-target     `$x =~ …` / `!~` (s///, tr///, m//g pos)
#       local            from parse_expr_to_tree's declarations list
#       pos-arg          pos($x)
#       mutating-builtin-arg   chomp/chop/undef/read/sysread/recv arg; also
#                              4-arg substr($x,…) (in-place replace — a hole
#                              the text scan never covered)
#       handle-viv-arg   open/opendir/sysopen/pipe/socket/socketpair/accept (D26)
#       foreach-alias    foreach loop variable (my and plain)
#     Region facts (not per-event): string-eval reachability = an `eval` WORD
#     token (comments/string innards no longer false-fire — the flagship win);
#     nested-sub capture = $names inside anon-sub blocks, from Symbol tokens
#     plus interpolatable-quote innards, skipping comments.
#     Statements whose parse dies fall back to the text gates on THAT
#     statement's source, plus a bare `$x =` write gate (the tree normally
#     owns write classification, so the text gate list alone is not enough).
#
# Runtime switches (bring-up history: docs/v2-completion-plan.md §W12):
#   default            = tree verdicts
#   PCL_W12_DIFF=1     = run BOTH, print one W12DIFF line per verdict
#                        difference to STDERR, still return tree verdicts
#
# Unboxable requires ALL of:
#   - declared exactly once in the region as a single `my $x` (shadowing → box)
#   - no boxing event (list above), not referenced in a nested sub block,
#     no string `eval` in the region (session-250 lexical capture)
#   - the decl init and EVERY statement-root `$x = RHS;` write are
#     RAW-VALUE-shaped: at least one top-level %ARITH_OP operator (every such
#     p-op coerces its operands — boxes, strings, sub results — and returns a
#     raw CL number or string), or a single bare number/string literal.
#     A bare `$y` or bare `f()` could alias/return a box.

use v5.30;
use strict;
use warnings;

use Pl::PExpr ();

# Operators whose p-functions return raw CL values (number / string / 1-or-"").
my %ARITH_OP = map { $_ => 1 } qw(+ - * / % ** < > <= >= == != <=>
                                  . eq ne lt gt le ge cmp !);

my %COMPOUND_ASSIGN = map { $_ => 1 }
  qw(+= -= *= /= %= **= x= .= ||= &&= //= <<= >>= &= |= ^= &.= |.= ^.=);

my %MUTATING_FN   = map { $_ => 1 } qw(chomp chop undef read sysread recv);
my %HANDLE_VIV_FN = map { $_ => 1 } qw(open opendir sysopen pipe socket
                                       socketpair accept);
# tie attaches magic to the BOX — a tied variable must stay boxed forever
# (a raw slot would bypass FETCH/STORE).  Found via case-invert-01.t during
# W12 bring-up; the text scan had no tie gate either (live bug on
# `my $x; tie $x, …` — the inline `tie my $x, …` form only worked by luck:
# text step 1 never counted the inline decl, so $x wasn't in vi at all).
my %TIE_FN        = map { $_ => 1 } qw(tie untie);

sub analyze {
  my ($class, $stmts, $extra_params, $known_subs, $host) = @_;
  if (!$host) {
    return _analyze_text($stmts, $extra_params, $known_subs);
  }
  my $tree_vi = eval { _analyze_tree($stmts, $extra_params, $known_subs, $host) };
  if (!$tree_vi) {
    # Text fallback on a tree crash.  Warn only under diff mode: pl2cl
    # stderr is merged into generated CL by several test helpers.
    warn "W12DIFF TREE-CRASH: $@" if $ENV{PCL_W12_DIFF};
    return _analyze_text($stmts, $extra_params, $known_subs);
  }
  if ($ENV{PCL_W12_DIFF}) {
    my $text_vi = _analyze_text($stmts, $extra_params, $known_subs);
    _diff_report($stmts, $text_vi, $tree_vi);
  }
  return $tree_vi;
}

# ==========================================================================
# Text annotator (s272 prototype) — kept verbatim during bring-up.
# ==========================================================================

# Step-3 per-name gates, shared with the tree annotator's parse-failure
# fallback.  Returns the tags of every firing gate ('' text → none).
sub _text_gate_tags {
  my ($name, $text, $with_bare_write) = @_;
  my $bare = quotemeta $name;
  my @tags;
  push @tags, 'ref-taken'    if $text =~ /\\\s*$bare\b/;
  push @tags, 'magic-ref'    if $text =~ /\\\s*(?:substr|vec|pos)\s*\(?\s*$bare\b/;
  push @tags, 'write-incdec' if $text =~ /$bare\s*(?:\+\+|--)/
                             || $text =~ /(?:\+\+|--)\s*$bare\b/;
  push @tags, 'write-compound'
    if $text =~ /$bare\s*(?:[-+*\/.%x&|^]|\*\*|\|\||&&|\/\/|<<|>>|[&|^]\.)=(?!=)/;
  push @tags, 'regex-target' if $text =~ /$bare\s*=~/;
  push @tags, 'paren-assign' if $text =~ /\(\s*$bare\s*=[^=~]/;
  push @tags, 'local'        if $text =~ /\blocal\b[^;]*$bare\b/;
  push @tags, 'pos-arg'      if $text =~ /\bpos\s*\(?\s*$bare\b/;
  push @tags, 'foreach-alias' if $text =~ /\bforeach?\s+my\s+$bare\b/
                              || $text =~ /\bforeach?\s+$bare\b/;
  push @tags, 'write-list'   if $text =~ /\([^=]*$bare\b[^=]*\)\s*=[^=]/;
  push @tags, 'mutating-builtin-arg'
    if $text =~ /\b(?:chomp|chop|undef|read|sysread|recv)\b[^;]*$bare\b/;
  push @tags, 'handle-viv-arg'
    if $text =~ /\b(?:open|opendir|sysopen|pipe|socket|socketpair|accept)\s*\(?\s*$bare\b/;
  push @tags, 'tie-target'                  # tie magic lives on the box
    if $text =~ /\b(?:tie|untie)\s*\(?\s*(?:my\s+)?$bare\b/;
  # Only for the tree annotator's parse-failure fallback: a bare assignment in
  # unparsed source cannot be shape-checked or seam-classified → box.
  push @tags, 'write-in-fallback'
    if $with_bare_write && $text =~ /$bare\s*=[^=~]/;
  return @tags;
}

sub _analyze_text {
  my ($stmts, $extra_params, $known_subs) = @_;
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
    my @reasons;
    push @reasons, 'multi-decl'     if $decl_count{$name} != 1;
    push @reasons, 'init-shape'     unless $decl_init_ok{$name};
    push @reasons, 'eval-in-region' if $has_eval;
    push @reasons, 'nested-sub-ref' if $in_nested_sub{$name};
    push @reasons, _text_gate_tags($name, $text);
    $vi{$name} = { unboxable => (@reasons ? 0 : 1),
                   ($ENV{PCL_W12_DIFF} ? (reasons => \@reasons) : ()) };
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
      unless (_arith_rhs([@k[2 .. $#k]], $known_subs)) {
        $vi{$name}{unboxable} = 0;
        push @{ $vi{$name}{reasons} }, 'write-shape' if $ENV{PCL_W12_DIFF};
      }
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
      # W11: element access `$h{k}` / `$a[i]` — the Symbol plus its subscript
      # chain is ONE value (the element).  Consume the trailing Subscript(s)
      # without scanning inside (the key only selects the slot; writes inside
      # it are caught by the step-3 text regexes).  p-gethash/p-aref return
      # the element VALUE, which may itself be a reference box — so this
      # counts as `others` (like a sub call): only an operator-coerced RHS
      # may unbox, a bare `$x = $h{k}` stays boxed.
      $i++ while $i < $#e && ref($e[$i+1]) eq 'PPI::Structure::Subscript';
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

# ==========================================================================
# Tree annotator (W12) — structural events from the PExpr OpcodeTree.
# ==========================================================================

sub _analyze_tree {
  my ($stmts, $extra_params, $known_subs, $host) = @_;
  my @stmts = grep { ref $_ } @$stmts;

  my $ctx = {
    ev             => {},   # $ev->{'$x'}{'write-incdec'}++  (boxing events only)
    decl_count     => {},
    init_bad       => {},   # a root write's RHS shape unproven → box
    fallback_texts => [],   # statements whose parse died → text gates apply
    known_subs     => $known_subs // {},
    host           => $host,
    seam           => 0,    # inside an expression-embedded block (v1-compiled)
    cond           => 0,    # under an if/unless statement modifier
    has_eval       => 0,
    nested_sub     => {},
  };
  $ctx->{decl_count}{$_} = 1 for @{ $extra_params // [] };

  for my $stmt (@stmts) {
    _tw_region_facts($ctx, $stmt);
  }
  _tw_stmts($ctx, \@stmts);

  my %vi;
  for my $name (keys %{ $ctx->{decl_count} }) {
    my @reasons;
    push @reasons, 'multi-decl'     if $ctx->{decl_count}{$name} != 1;
    push @reasons, 'eval-in-region' if $ctx->{has_eval};
    push @reasons, 'nested-sub-ref' if $ctx->{nested_sub}{$name};
    push @reasons, 'write-shape'    if $ctx->{init_bad}{$name};
    push @reasons, sort keys %{ $ctx->{ev}{$name} // {} };
    push @reasons, map { "fallback:$_" }
      map { _text_gate_tags($name, $_, 1) } @{ $ctx->{fallback_texts} };
    $vi{$name} = { unboxable => (@reasons ? 0 : 1),
                   ($ENV{PCL_W12_DIFF} ? (reasons => \@reasons) : ()) };
  }
  return \%vi;
}

sub _ev {
  my ($ctx, $name, $event) = @_;
  return unless defined $name && $name =~ /^\$\w+$/;
  $ctx->{ev}{$name}{$event}++;
}

# Region facts for one top-level statement (find() descends everywhere,
# including nested sub bodies — same coverage as the text region scan).
sub _tw_region_facts {
  my ($ctx, $stmt) = @_;

  # String-eval reachability: an `eval` WORD token.  Structural, so `eval`
  # inside a comment or string literal no longer boxes the whole region —
  # the W12 flagship win.  (Block eval still fires, matching the text scan;
  # narrowing that is a separate, later decision.)
  $ctx->{has_eval} = 1
    if @{ $stmt->find(sub {
            $_[1]->isa('PPI::Token::Word') && $_[1]->content eq 'eval';
          }) || [] };

  # Names captured by nested anon subs (`sub { … }` blocks): Symbol tokens
  # plus $names inside interpolatable quote-likes ("…", qq, regexes,
  # backticks) — a "$x" in a closure body is a capture.  Comments and
  # single-quoted strings no longer count (text-scan false fires).
  my $blocks = $stmt->find(sub {
    $_[1]->isa('PPI::Structure::Block') && do {
      my $prev = $_[1]->sprevious_sibling;
      $prev && $prev->isa('PPI::Token::Word') && $prev->content eq 'sub';
    };
  }) || [];
  for my $b (@$blocks) {
    for my $t ($b->tokens) {
      if ($t->isa('PPI::Token::Symbol')) {
        $ctx->{nested_sub}{$1}++ if $t->content =~ /^(\$\w+)/;
      }
      elsif ($t->isa('PPI::Token::Quote::Double')
          || $t->isa('PPI::Token::Quote::Interpolate')
          || $t->isa('PPI::Token::QuoteLike::Backtick')
          || $t->isa('PPI::Token::QuoteLike::Readline')
          || $t->isa('PPI::Token::QuoteLike::Regexp')
          || $t->isa('PPI::Token::Regexp')
          || $t->isa('PPI::Token::HereDoc')) {
        my $c = $t->content;
        $c .= join '', $t->heredoc if $t->isa('PPI::Token::HereDoc');
        $ctx->{nested_sub}{$1}++ while $c =~ /(\$\w+)/g;
      }
    }
  }
}

# ---------------------------------------------------------- statement walk

sub _tw_stmts {
  my ($ctx, $stmts) = @_;
  for my $s (grep { ref $_ && $_->significant } @$stmts) {
    _tw_stmt($ctx, $s);
  }
}

sub _tw_stmt {
  my ($ctx, $s) = @_;
  return unless $s->isa('PPI::Statement');
  my $r = ref $s;

  # Named sub: its body is its OWN region (analyzed when the sub is lowered);
  # outer names it captures are handled by the nested-sub/file-lexical
  # machinery, and a `my` inside it is invisible at this region's level.
  return if $s->isa('PPI::Statement::Sub')
         && !$s->isa('PPI::Statement::Scheduled');

  if ($s->isa('PPI::Statement::Compound')) {
    my @k = $s->schildren;
    my ($kw) = grep { $_->isa('PPI::Token::Word') } @k;
    if ($kw && $kw->content =~ /^for(?:each)?$/) {
      # foreach loop variable (my or plain) is an ALIAS into the list
      my ($var) = grep { $_->isa('PPI::Token::Symbol') } @k;
      _ev($ctx, $var->content, 'foreach-alias') if $var;
    }
    for my $k (@k) {
      if ($k->isa('PPI::Structure::Condition')
          || $k->isa('PPI::Structure::For')
          || $k->isa('PPI::Structure::List')
          || $k->isa('PPI::Structure::Block')) {
        _tw_stmts($ctx, [$k->schildren]);
      }
    }
    return;
  }

  if ($s->isa('PPI::Statement::Break')) {
    # `return EXPR` (etc.) lowers via _lower_expr — never the native
    # statement-root setf — so root writes here are seam-embedded.
    my @k = grep { !_semi($_) } $s->schildren;
    shift @k if @k && $k[0]->isa('PPI::Token::Word');
    _tw_stmt_expr($ctx, \@k, 0) if @k;
    return;
  }

  if ($r eq 'PPI::Statement'
      || $r eq 'PPI::Statement::Variable'
      || $r eq 'PPI::Statement::Expression') {
    # Statement::Expression appears inside conditions/lists — those lower via
    # _lower_expr (seam for any write), not the native statement-root path.
    my $native_root = $r ne 'PPI::Statement::Expression';
    _tw_stmt_expr($ctx, [$s->schildren], $native_root);
    return;
  }

  return if $s->isa('PPI::Statement::Null')
         || $s->isa('PPI::Statement::End')
         || $s->isa('PPI::Statement::Data');

  # Include/Package/Scheduled/Given/When/unknown: no tree parse — apply the
  # text gates to this statement's source (conservative).
  push @{ $ctx->{fallback_texts} }, $s->content;
}

sub _semi {
  my ($e) = @_;
  return $e->isa('PPI::Token::Structure') && $e->content eq ';';
}

sub _tw_stmt_expr {
  my ($ctx, $parts, $native_root) = @_;
  my @parts = grep { ref $_ && $_->significant && !_semi($_) } @$parts;
  return unless @parts;

  # Trailing statement modifier: `EXPR if COND` — the native setf path
  # requires !$mod (D12), so root writes under a modifier box (write-cond);
  # while/until/for modifiers lower whole-statement via v1 → same treatment.
  my ($expr, $mod, $cond) = Pl::Parser2::_split_modifier(\@parts);
  if ($mod) {
    _tw_stmt_expr($ctx, $cond, 0);
    local $ctx->{cond} = 1;
    _tw_expr_parse($ctx, $expr, 0);
    return;
  }
  _tw_expr_parse($ctx, \@parts, $native_root);
}

sub _tw_expr_parse {
  my ($ctx, $parts, $native_root) = @_;
  my @parts = grep { ref $_ && $_->significant } @$parts;
  return unless @parts;

  # Expression-embedded blocks (do/map/grep/sort/eval{} …): PExpr compiles
  # their bodies to a CL STRING (inline_lambda body_cl) — invisible to the
  # tree walk — and they lower inside the v1 seam, where every write is a
  # box-set.  Walk their statements STRUCTURALLY with the seam flag so all
  # writes count as write-embedded.  Sub-blocks are closures: the nested-sub
  # region fact owns those.
  for my $b (@{ _tw_top_blocks(\@parts) }) {
    my $prev = $b->sprevious_sibling;
    next if $prev && $prev->isa('PPI::Token::Word') && $prev->content eq 'sub';
    local $ctx->{seam} = 1;
    _tw_stmts($ctx, [$b->schildren]);
  }

  # D7 (extended): PExpr parses DESTRUCTIVELY rewrite shared token content
  # (`=>` → `,`) AND store parse-state keys on the elements (the analysis
  # parse runs BEFORE later `use constant`/sub registrations, so a stale
  # `_bareword_string` here poisons the real parse — the split.t `nought`
  # bug).  Snapshot/restore both, via the shared Parser2 helper.
  my $snap = Pl::Parser2::_ppi_state_snapshot(@parts);

  # Analysis-only parse: divert any parser bucket emission (block-form args
  # can emit anon-block defuns DURING parsing) into a scratch section set and
  # DISCARD it — the real lowering emits later.
  my $p = $ctx->{host}->fallback_parser;
  my @sv = ($p->_sections, $p->_cur_bucket, $p->indent_level);
  $p->_sections([]);
  $p->_cur_bucket('definitions');
  $p->_open_section('pcl');
  $p->_cur_bucket('definitions');
  $p->indent_level(0);

  my $ok = eval {
    # Analysis-only parse: PExpr warns before it dies on unsupported shapes
    # ("Handle single node of unknown type…"); those diagnostics will repeat
    # in the REAL lowering if relevant, and several test helpers merge
    # pl2cl's stderr into the generated CL — so silence warns here.
    local $SIG{__WARN__} = sub { };
    my $expr_o = Pl::PExpr->new(
      e           => \@parts,
      environment => $ctx->{host}->environment,
      parser      => $p,
    );
    my ($root, $decls) = $expr_o->parse_expr_to_tree(\@parts);
    for my $d (@{ $decls // [] }) {
      my $var = $d->{var} // '';
      next unless $var =~ /^\$\w+$/;
      if    ($d->{type} eq 'my')    { $ctx->{decl_count}{$var}++ }
      elsif ($d->{type} eq 'local') { _ev($ctx, $var, 'local') }
      # our/state: package/persistent cells — never raw-let candidates here
    }
    _tw_walk($ctx, $expr_o, $root, $native_root && !$ctx->{seam});
    1;
  };

  $p->_sections($sv[0]);
  $p->_cur_bucket($sv[1]);
  $p->indent_level($sv[2]);
  Pl::Parser2::_ppi_state_restore($snap);

  push @{ $ctx->{fallback_texts} }, join(' ', map { $_->content } @parts)
    unless $ok;
}

# Top-most Structure::Block descendants of the expression parts (blocks
# nested inside a found block are reached via the statement walk instead).
sub _tw_top_blocks {
  my ($parts) = @_;
  my @found;
  for my $part (@$parts) {
    next unless $part->isa('PPI::Node');
    if ($part->isa('PPI::Structure::Block')) {
      push @found, $part;
      next;
    }
    my $inner = $part->find(sub {
      my $b = $_[1];
      return 0 unless $b->isa('PPI::Structure::Block');
      # skip blocks contained in another block below $part
      my $up = $b->parent;
      while ($up && $up != $part) {
        return 0 if $up->isa('PPI::Structure::Block');
        $up = $up->parent;
      }
      return 1;
    }) || [];
    push @found, @$inner;
  }
  return \@found;
}

# ------------------------------------------------------------- tree walk

sub _tw_walk {
  my ($ctx, $xo, $id, $root_native) = @_;
  my $node = $xo->get_a_node($id);
  my $kids = $xo->get_node_children($id) || [];

  if ($xo->is_internal_node_type($node)) {
    my $t = $node->{type} // '';
    if ($t eq 'funcall' && @$kids) {
      my $f = $xo->get_a_node($kids->[0]);
      my $fname = (ref($f) eq 'PPI::Token::Word') ? $f->content : '';
      my $mark = $MUTATING_FN{$fname}     ? 'mutating-builtin-arg'
               : $HANDLE_VIV_FN{$fname}   ? 'handle-viv-arg'
               : $TIE_FN{$fname}          ? 'tie-target'
               : $fname eq 'pos'          ? 'pos-arg'
               : ($fname eq 'substr' && @$kids == 5)
                                          ? 'mutating-builtin-arg'  # 4-arg
               :                            undef;
      if ($mark) {
        # handle-viv/tie/pos/4-arg-substr write only their FIRST argument
        # (the mode/list/expression args are reads); chomp/chop/undef/read/
        # sysread/recv can write any/later args → mark them all.
        my @args = $MUTATING_FN{$fname} ? @$kids[1 .. $#$kids]
                 :                        ($kids->[1] // ());
        _tw_mark($ctx, $xo, $_, $mark) for @args;
      }
      _tw_walk($ctx, $xo, $_, 0) for @$kids[1 .. $#$kids];
      return;
    }
    if ($t eq '=~') {
      _tw_mark($ctx, $xo, $kids->[0], 'regex-target') if @$kids;
      _tw_walk($ctx, $xo, $_, 0) for @$kids;
      return;
    }
    if ($t eq 'prefix_op' || $t eq 'postfix_op') {
      my ($op_i, $ex_i) = $t eq 'prefix_op' ? (0, 1) : (1, 0);
      my $op = $xo->get_a_node($kids->[$op_i]);
      my $opc = (ref($op) && !$xo->is_internal_node_type($op)) ? $op->content : '';
      if ($opc eq '++' || $opc eq '--') {
        _tw_mark($ctx, $xo, $kids->[$ex_i], 'write-incdec');
      } elsif ($opc eq '\\') {
        # the WHOLE operand subtree: covers \$x and \substr($x,…)/\vec/\pos
        _tw_mark($ctx, $xo, $kids->[$ex_i], 'ref-taken');
      }
      _tw_walk($ctx, $xo, $kids->[$ex_i], 0) if @$kids > $ex_i;
      return;
    }
    # tree_val, progn, ternary, h_acc/a_acc, slices, hash_init/arr_init,
    # methodcall, string_concat, anon_sub, inline_lambda (list args only —
    # body is opaque body_cl, covered by the seam block walk), readline,
    # glob, backtick, filehandle, func_ref, ref_funcall, …: recurse.
    # root-nativeness never propagates through a wrapper: Parser2's native
    # setf path requires the bare `$x = RHS` token shape at statement root.
    _tw_walk($ctx, $xo, $_, 0) for @$kids;
    return;
  }

  my $r = ref $node;
  if ($r eq 'PPI::Token::Operator' && @$kids) {
    my $op = $node->content;
    if ($op eq '=' && @$kids == 2) {
      my $l = $xo->get_a_node($kids->[0]);
      my $lkids = $xo->get_node_children($kids->[0]) || [];
      my $ltype = $xo->is_internal_node_type($l);
      if (ref($l) eq 'PPI::Token::Symbol' && !@$lkids
          && $l->content =~ /^\$\w+$/) {
        my $name = $l->content;
        if (!$root_native)     { _ev($ctx, $name, 'write-embedded') }
        elsif ($ctx->{cond})   { _ev($ctx, $name, 'write-cond') }
        $ctx->{init_bad}{$name} = 1
          unless _tw_shape_ok($ctx, $xo, $kids->[1]);
      }
      elsif ($ltype && ($ltype eq 'h_acc' || $ltype eq 'a_acc')) {
        # Container element write.  For a PLAIN container base (a Symbol
        # whose ->symbol is %h/@a) no scalar is written — keys are reads.
        # But a deref-CHAIN base ($r->{A}[0]: a_acc over h_ref_acc($r))
        # autovivifies THROUGH the root scalar — the runtime writes the
        # vivified container back into $r's box, so $r must stay boxed
        # (exists_sub.t t13: unboxed $r made every deref re-vivify a fresh
        # hash).  Mark every $scalar under the base as written; a nested
        # plain access ($h{a}{b}) over-marks token '$h' — over-boxing only,
        # never correctness.
        my $base = $lkids->[0];
        my $bn   = defined $base ? $xo->get_a_node($base) : undef;
        if (ref($bn) && !$xo->is_internal_node_type($bn)
            && $bn->isa('PPI::Token::Symbol')
            && $bn->symbol =~ /^[\@\%]/) {
          _tw_walk($ctx, $xo, $kids->[0], 0);
        } else {
          _tw_mark($ctx, $xo, $base, 'write-deref-viv') if defined $base;
          _tw_walk($ctx, $xo, $kids->[0], 0);
        }
      }
      else {
        # list assign / paren-wrapped / lvalue-fn target (D11): every
        # $scalar inside the LHS is written by seam machinery → box
        _tw_mark($ctx, $xo, $kids->[0], 'write-list');
        _tw_walk($ctx, $xo, $kids->[0], 0);
      }
      _tw_walk($ctx, $xo, $kids->[1], 0);
      return;
    }
    if ($COMPOUND_ASSIGN{$op}) {
      _tw_mark($ctx, $xo, $kids->[0], 'write-compound');
      _tw_walk($ctx, $xo, $_, 0) for @$kids;
      return;
    }
    if ($op eq '=~' || $op eq '!~') {
      _tw_mark($ctx, $xo, $kids->[0], 'regex-target');
      _tw_walk($ctx, $xo, $_, 0) for @$kids;
      return;
    }
    _tw_walk($ctx, $xo, $_, 0) for @$kids;
    return;
  }

  # Leaf tokens are reads — except a substitution with /e, whose replacement
  # is CODE the tree cannot see: apply the text gates to its source.
  if ($r eq 'PPI::Token::Regexp::Substitute') {
    my %m = $node->get_modifiers;
    push @{ $ctx->{fallback_texts} }, $node->content if $m{e} || $m{ee};
  }
  _tw_walk($ctx, $xo, $_, 0) for @$kids;
}

# Mark every $scalar Symbol in the subtree with a boxing event.
sub _tw_mark {
  my ($ctx, $xo, $id, $event) = @_;
  my $node = $xo->get_a_node($id);
  if (ref($node) && !$xo->is_internal_node_type($node)
      && $node->isa('PPI::Token::Symbol')) {
    _ev($ctx, $node->content, $event);
  }
  _tw_mark($ctx, $xo, $_, $event) for @{ $xo->get_node_children($id) || [] };
}

# Mirror of _arith_rhs on the tree: the RHS stores a raw CL value when its
# ROOT is an %ARITH_OP operator (which coerces its operands) over operands
# from the allowed vocabulary, or a single number/string literal.
# string_concat (an interpolated "…") returns a raw string, like the text
# scanner's Quote::Double-counts-as-literal rule.
sub _tw_shape_ok {
  my ($ctx, $xo, $id) = @_;
  my $node = $xo->get_a_node($id);
  my $kids = $xo->get_node_children($id) || [];

  if ($xo->is_internal_node_type($node)) {
    my $t = $node->{type} // '';
    return _tw_shape_ok($ctx, $xo, $kids->[0]) if $t eq 'tree_val' && @$kids == 1;
    return 1 if $t eq 'string_concat';
    if ($t eq 'prefix_op' && @$kids == 2) {          # -$y / +$y / !$y roots
      my $op = $xo->get_a_node($kids->[0]);
      my $opc = (ref($op) && !$xo->is_internal_node_type($op)) ? $op->content : '';
      return _tw_operand_ok($ctx, $xo, $kids->[1])
        if $opc eq '-' || $opc eq '+' || $opc eq '!';
    }
    return 0;               # funcall/h_acc/… root: value may be/alias a box
  }
  my $r = ref $node;
  if ($r eq 'PPI::Token::Operator' && @$kids) {
    return 0 unless $ARITH_OP{$node->content};
    for my $k (@$kids) {
      return 0 unless _tw_operand_ok($ctx, $xo, $k);
    }
    return 1;
  }
  return 1 if ref($node) && $node->isa('PPI::Token::Number');
  return 1 if $r eq 'PPI::Token::Quote::Single'
           || $r eq 'PPI::Token::Quote::Double';
  return 0;                 # bare $y / f() / anything else: may alias a box
}

# Allowed vocabulary INSIDE an operator-coerced RHS (mirror of _scan):
# numbers, strings, plain $scalars, parens, arith operators, element reads
# (key shape free — the read returns a value the operator coerces), and
# calls to KNOWN user subs with args of ANY shape.
sub _tw_operand_ok {
  my ($ctx, $xo, $id) = @_;
  my $node = $xo->get_a_node($id);
  my $kids = $xo->get_node_children($id) || [];

  if ($xo->is_internal_node_type($node)) {
    my $t = $node->{type} // '';
    if ($t eq 'tree_val') {
      for my $k (@$kids) {
        return 0 unless _tw_operand_ok($ctx, $xo, $k);
      }
      return 1;
    }
    return 1 if $t eq 'string_concat';
    return 1 if $t eq 'h_acc' || $t eq 'a_acc';      # element read = a value
    if ($t eq 'funcall' && @$kids) {                 # known user sub call
      my $f = $xo->get_a_node($kids->[0]);
      return 1 if ref($f) eq 'PPI::Token::Word'
        && $ctx->{known_subs}{$f->content};
      return 0;
    }
    if ($t eq 'prefix_op' && @$kids == 2) {
      my $op = $xo->get_a_node($kids->[0]);
      my $opc = (ref($op) && !$xo->is_internal_node_type($op)) ? $op->content : '';
      return _tw_operand_ok($ctx, $xo, $kids->[1])
        if $opc eq '-' || $opc eq '+' || $opc eq '!';
    }
    return 0;
  }
  my $r = ref $node;
  if ($r eq 'PPI::Token::Operator') {
    return 0 unless $ARITH_OP{$node->content};
    for my $k (@$kids) {
      return 0 unless _tw_operand_ok($ctx, $xo, $k);
    }
    return 1;
  }
  return 1 if ref($node) && $node->isa('PPI::Token::Number');
  return 1 if $r eq 'PPI::Token::Quote::Single'
           || $r eq 'PPI::Token::Quote::Double';
  # ref() equality (not isa): PPI::Token::Magic ($_, $1, …) stays REJECTED,
  # mirroring _scan — relaxing it is a separate, measured decision.
  return 1 if $r eq 'PPI::Token::Symbol' && !@$kids
           && $node->content =~ /^\$\w+$/;
  return 0;
}

# ------------------------------------------------------------ diff report

sub _diff_report {
  my ($stmts, $text_vi, $tree_vi) = @_;
  my ($first) = grep { ref $_ } @$stmts;
  my $where = '?';
  if ($first) {
    my $line = eval { $first->line_number } // '?';
    $where = "line $line";
  }
  for my $name (sort keys %$text_vi) {
    # names absent from the tree side are declared only inside nested sub
    # bodies — never consulted at this region's level — skip
    next unless exists $tree_vi->{$name};
    my ($t, $w) = ($text_vi->{$name}{unboxable}, $tree_vi->{$name}{unboxable});
    next if $t == $w;
    my $treasons = join(',', @{ $text_vi->{$name}{reasons} // [] }) || '-';
    my $wreasons = join(',', @{ $tree_vi->{$name}{reasons} // [] }) || '-';
    my $line = sprintf "W12DIFF %s %s text=%s(%s) tree=%s(%s)\n",
      $where, $name,
      $t ? 'unboxable' : 'boxed', $treasons,
      $w ? 'unboxable' : 'boxed', $wreasons;
    # PCL_W12_DIFF=/abs/path appends there — STDERR of pl2cl is merged into
    # the generated CL by some test helpers, so warn only when no path given.
    if (($ENV{PCL_W12_DIFF} // '') =~ m{^/}) {
      open my $fh, '>>', $ENV{PCL_W12_DIFF} or next;
      print $fh $line;
      close $fh;
    } else {
      warn $line;
    }
  }
}

1;

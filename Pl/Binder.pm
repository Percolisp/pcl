# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::Binder;

# Pl::Binder -- THE BINDING TABLE (perl's pad model, pad.c) built in ONE walk
# over a PPI document, run in DUAL-RUN beside the three legacy resolvers of
# Pl::Parser2 (s525q; parser-structure program step 0, member c2; design
# `docs/var-handling-review-s379.md` §3 Direction A and
# `docs/parser-review-s525.md` §3 / §4 Option 1c; measurement
# `docs/binder-dual-run-s525.md`).
#
# INERT BY DESIGN.  Nothing reads its answers to decide anything: Parser2
# builds it only under PCL_BINDER_CHECK and its hooks compare the binder's
# verdict with the legacy verdict, never replacing it.  With the variable
# unset this module is not even loaded.
#
#   PCL_BINDER_CHECK=die   a disagreement dies, one line, both verdicts
#   PCL_BINDER_CHECK=log   a disagreement appends one TSV row to
#                          $PCL_BINDER_LOG (STDERR when unset) and continues;
#                          a binder ERROR is logged too, never fatal
#   PCL_BINDER_TRACE=1     print every reference with its binding (STDERR)
#   PCL_BINDER_INJECT=R    self-test: invert the binder's answer for resolver
#                          R (binding_at | ref_shadowed | block_captures) so
#                          the die path can be exercised (Pl/t/binder-01.t)
#
# THE TABLE (one record per binding, perl's PADNAME + COP_SEQ_RANGE):
#   id            counter, 1.. in document order (globals get ids lazily)
#   kind          my | state | our | param | lexsub | outer   (declared)
#                 global | special                            (undeclared)
#   canon         the container in source spelling: '$x' '@x' '%x' '&f'
#   sigil         '$' '@' '%' '&'
#   name          what perl would print: '$x' for a lexical, '$Pkg::x' for
#                 an our alias or a global (`our` = its DECLARING package)
#   perl_package  the package in force at the declaration (globals: at the
#                 use; specials: 'main' -- gv.c S_gv_is_in_main)
#   decl_node     the declaring token (Symbol / Word / Prototype) or undef
#   decl_ord      its token ordinal (-1 for outer/global)
#   scope_node    the Block / Document / Compound / Statement::Sub whose
#                 range closes the binding (pad_leavemy)
#   from, to      the visible range in token ORDINALS: from inclusive (the
#                 first token AFTER the intro point -- COP_SEQ_RANGE_LOW is
#                 exclusive of the declaring statement), to inclusive
#                 (COP_SEQ_RANGE_HIGH, the scope's last token)
#
# THE REF TABLE: refaddr(token) -> [ { canon, via, ord, pkg, cv, binding } ]
#   via  sym | arylen | brace | decl | interp | interp-code | readline
#   An interpolating token (dq string, heredoc, regex, qx, qr, <$fh>) carries
#   one ref per reference Pl::InterpScan::scan reports, all at the token's
#   own ordinal (perl resolves a string's lexicals where the string is lexed).
#
# THE RULES, one sub each (cited to perl 5.40.3's source):
#   _intro_point    a `my` is visible from the statement AFTER its own
#                   (op.c:8653 newSTATEOP -> intro_my, pad.c:1451); in a
#                   compound HEAD from the end of the head expression
#                   (perly.y:754 mexpr, :758 mnexpr, :728 mintro) to the end
#                   of the compound; `for my $x (LIST)` from after LIST
#                   (perly.y:519, the list is mexpr)
#   _resolve        the innermost visible binding wins: the latest `from`
#                   among the bindings whose range holds the position
#                   (pad_findmy_pvn pad.c:961 -> pad_findlex pad.c:1101 walks
#                   the current pad then CvOUTSIDE); none -> a global of the
#                   POSITIONAL package (toke.c:9994 S_pending_ident), or a
#                   special of main:: (gv.c S_gv_is_in_main)
#   _signature      a parameter is visible from the end of its own part
#                   (a default sees the OUTER binding of its own name)
#   _catch_param    `catch ($e) BLOCK` binds $e for BLOCK (perly.y:294)
#   qualified names are always globals (#3080); `local` binds nothing;
#   `foreach $x` with no `my` is a USE (it aliases whatever $x resolves to,
#   toke.c yyl_foreach); strict vars is not enforced (CLAUDE.md rule 9).
#   An unknown variable-token shape DIES naming it (rule 12).
#
# API:  Pl::Binder->new($doc, parser => $p2, outer => [canons], pkg => 'X')
#         ->bind                       returns the object
#       ->binding_of($tok [, $canon])  the record or undef (not a variable)
#       ->resolve($node, $canon)       the record visible at $node for $canon
#       ->captures($block)             OUTER bindings read inside $block
#       Pl::Binder::check(...)         the dual-run comparison (the hooks)

use strict;
use warnings;
use Scalar::Util qw(refaddr blessed);
use Time::HiRes ();
use Pl::InterpScan ();
use Pl::PExpr::TokenUtils ();

my %SPECIAL_MAIN = map { $_ => 1 } qw(_ INC ENV SIG ARGV STDIN STDOUT STDERR ARGVOUT);
my $RENAME_SUFFIX = qr/__(?:file|state|shadow|excl|lex|evalcap|lexsub|cond|family|emb)__\w*\z/;

sub new {
  my ($class, $doc, %opt) = @_;
  return bless { doc => $doc, opt => \%opt, bindings => [], by_canon => {},
                 refs => {}, ord => {}, globals => {}, stats => {} }, $class;
}

sub _first_ord { my ($s, $n) = @_; my $t = $n->isa('PPI::Token') ? $n : $n->first_token; return $t ? $s->{ord}{refaddr $t} : undef }
sub _last_ord  { my ($s, $n) = @_; my $t = $n->isa('PPI::Token') ? $n : $n->last_token;  return $t ? $s->{ord}{refaddr $t} : undef }

sub bind {
  my ($self) = @_;
  my $t0 = Time::HiRes::time();
  my $doc = $self->{doc};
  my $i = 0;
  $self->{ord}{refaddr $_} = $i++ for $doc->tokens;
  $self->{last} = $i - 1;
  $self->{decl_tok} = {};
  # eval-mode: the caller's lexicals (the capture alist) are the outermost
  # pad, visible everywhere a declaration of the eval text does not shadow
  # (pad_findlex's CvOUTSIDE of an eval is the caller's pad; ir-spec §2b).
  for my $c (@{ $self->{opt}{outer} || [] }) {
    $self->_add_binding(kind => 'outer', canon => $c, pkg => undef,
                        decl => undef, decl_ord => -1, scope => $doc,
                        from => -1, to => $self->{last});
  }
  # `${x}` / `@{x}` / `$#{x}` at code level: the ONE helper Parser2's span
  # and rename passes read (rule 11), so the binder sees the same uses.
  $self->{brace} = {};
  if (defined &Pl::Parser2::_brace_name_refs) {
    for my $p (@{ Pl::Parser2::_brace_name_refs($doc) }) {
      $self->{brace}{ refaddr $p->[0] } = $p->[1];
    }
  }
  $self->_walk($doc, $self->{opt}{pkg} // 'main', $doc);
  $self->_resolve_all;
  $self->{stats}{build_ms} = int(1000 * (Time::HiRes::time() - $t0) + 0.5);
  $self->_trace if $ENV{PCL_BINDER_TRACE};
  return $self;
}

sub _add_binding {
  my ($self, %b) = @_;
  my $rec = { id => scalar(@{ $self->{bindings} }) + 1, kind => $b{kind},
              canon => $b{canon}, sigil => substr($b{canon}, 0, 1),
              perl_package => $b{pkg}, decl_node => $b{decl},
              decl_ord => $b{decl_ord}, scope_node => $b{scope},
              from => $b{from}, to => $b{to} };
  (my $bare = $b{canon}) =~ s/^[\$\@\%\&]//;
  $rec->{name} = $b{kind} eq 'our' ? "$rec->{sigil}$b{pkg}::$bare" : $b{canon};
  push @{ $self->{bindings} }, $rec;
  push @{ $self->{by_canon}{ $b{canon} } }, $rec;
  $self->{decl_bind}{ refaddr $b{decl} } //= $rec if $b{decl};
  $self->{stats}{kind}{ $b{kind} }++;
  return $rec;
}

# ---------------------------------------------------------------------------
# THE WALK: document order, carrying the positional package and the CV (the
# sub body whose pad a use belongs to).  `package NAME;` lasts to the end of
# its block; `package NAME BLOCK` holds for the block only.
sub _walk {
  my ($self, $node, $pkg, $cv) = @_;
  for my $c ($node->children) {
    if ($c->isa('PPI::Statement::Package')) {
      my $ns = $c->namespace;
      my ($blk) = grep { $_->isa('PPI::Structure::Block') } $c->schildren;
      if ($blk) { $self->_walk($c, $ns // $pkg, $cv) }
      else      { $pkg = $ns if defined $ns }
      next;
    }
    if ($c->isa('PPI::Token')) { $self->_token($c, $pkg, $cv); next }
    my $cv2 = $cv;
    $cv2 = $c if $c->isa('PPI::Structure::Block') && _is_cv_block($c);
    $self->_head_decls($c, $pkg) if $c->isa('PPI::Statement::Sub') || $c->isa('PPI::Statement::Compound');
    $self->_walk($c, $pkg, $cv2);
  }
}

# A block that is a sub body: a named sub's (Statement::Sub, incl. BEGIN &
# co., which are CVs too) or an anonymous `sub [PROTO|SIG|ATTR]* BLOCK`.
sub _is_cv_block {
  my ($blk) = @_;
  my $par = $blk->parent;
  return 1 if $par && $par->isa('PPI::Statement::Sub');
  for (my $p = $blk->sprevious_sibling; $p; $p = $p->sprevious_sibling) {
    next if $p->isa('PPI::Token::Prototype') || $p->isa('PPI::Token::Attribute')
         || $p->isa('PPI::Structure::Signature')
         || ($p->isa('PPI::Token::Operator') && $p->content eq ':');
    return ($p->isa('PPI::Token::Word') && $p->content eq 'sub') ? 1 : 0;
  }
  return 0;
}

# Declarations a construct's HEAD makes for its block: signature / named
# prototype parameters, `catch ($e)`.  (`for my $x`, `if (my $x = …)` are
# ordinary `my` words and go through _intro_point.)
sub _head_decls {
  my ($self, $st, $pkg) = @_;
  my @k = $st->schildren;
  for my $j (0 .. $#k) {
    my $e = $k[$j];
    if ($e->isa('PPI::Structure::Signature')) { $self->_signature($st, $e, \@k, $j) }
    elsif ($e->isa('PPI::Token::Prototype') && $st->isa('PPI::Statement::Sub')) {
      $self->_proto_params($st, $e, \@k, $j);
    }
    elsif ($e->isa('PPI::Token::Word') && $e->content eq 'catch'
           && $j + 2 <= $#k && $k[$j+1]->isa('PPI::Structure::List')
           && $k[$j+2]->isa('PPI::Structure::Block')) {
      $self->_catch_param($st, $k[$j+1], $k[$j+2]);
    }
  }
}

# perly.y:294 catch_paren: `scalar` then intro_my -- the variable is visible
# in the catch BLOCK and nowhere else.
sub _catch_param {
  my ($self, $st, $list, $blk) = @_;
  my @s = grep { $_->isa('PPI::Token::Symbol') } $list->tokens;
  return unless @s == 1;
  $self->{decl_tok}{ refaddr $s[0] } = 1;
  $self->_add_binding(kind => 'my', canon => $s[0]->content, pkg => undef,
                      decl => $s[0], decl_ord => $self->_first_ord($s[0]),
                      scope => $blk, from => $self->_first_ord($blk),
                      to => $self->_last_ord($blk));
}

# A Structure::Signature: each top-level part's LEADING symbol is a
# parameter, visible from the end of ITS part (a default of a later part sees
# it; its own default sees the outer name -- probed s438, #485) to the end
# of the sub.  perly.y sigsub: the parameters are pad entries introduced as
# the signature is parsed.
sub _signature {
  my ($self, $st, $sig, $k, $j) = @_;
  my ($blk) = grep { $_->isa('PPI::Structure::Block') } @$k[$j .. $#$k];
  return unless $blk;
  my $to = $self->_last_ord($blk);
  my @t = grep { $_->significant } $sig->tokens;
  shift @t if @t && $t[0]->content eq '(';
  pop @t   if @t && $t[-1]->content eq ')';
  my ($depth, $lead, $pending) = (0, 1, undef);
  my $close = sub {
    my ($ord) = @_;
    return unless $pending;
    $self->_add_binding(kind => 'param', canon => $pending->content, pkg => undef,
                        decl => $pending, decl_ord => $self->_first_ord($pending),
                        scope => $st, from => $ord, to => $to);
    $pending = undef;
  };
  for my $t (@t) {
    my $c = $t->content;
    if ($t->isa('PPI::Token::Structure')) { $depth++ if $c =~ /^[\(\[\{]$/; $depth-- if $c =~ /^[\)\]\}]$/; }
    if ($depth == 0 && $t->isa('PPI::Token::Operator') && $c eq ',') {
      $close->($self->_first_ord($t) + 1); $lead = 1; next;
    }
    if ($lead && $t->isa('PPI::Token::Symbol') && $c =~ /^[\$\@\%]\w+$/) {
      $pending = $t; $self->{decl_tok}{ refaddr $t } = 1;
    }
    $lead = 0;
  }
  $close->($self->_first_ord($blk));
}

# A Token::Prototype with NAMED parameters (`sub f ($x, $y)`): a signature
# only where the feature is in force at that sub (the parser's
# head_is_signature, #2872 / #455); otherwise perl reads an (illegal)
# prototype and binds NOTHING -- the body's names are globals.
sub _proto_params {
  my ($self, $st, $proto, $k, $j) = @_;
  my $text = $proto->content;
  return unless defined &Pl::Parser::proto_text_has_named_params
             && Pl::Parser::proto_text_has_named_params($text);
  my ($blk) = grep { $_->isa('PPI::Structure::Block') } @$k[$j .. $#$k];
  return unless $blk;
  my $fp = $self->{opt}{parser} && $self->{opt}{parser}->can('fallback_parser')
         ? $self->{opt}{parser}->fallback_parser : undef;
  my $is_sig = $fp ? $fp->head_is_signature($text, $st) : 1;
  $self->{stats}{proto_named}{ $is_sig ? 'signature' : 'prototype' }++;
  return unless $is_sig;
  for my $canon (Pl::Parser2::_signature_param_canons($text)) {
    $self->_add_binding(kind => 'param', canon => $canon, pkg => undef,
                        decl => $proto, decl_ord => $self->_first_ord($proto),
                        scope => $st, from => $self->_first_ord($blk),
                        to => $self->_last_ord($blk));
  }
}

# ---------------------------------------------------------------------------
# ONE TOKEN: a declarator word, a variable use, or interpolating text.
sub _token {
  my ($self, $t, $pkg, $cv) = @_;
  if ($t->isa('PPI::Token::Word')) {
    my $w = $t->content;
    if ($w eq 'my' || $w eq 'our' || $w eq 'state') { $self->_declare($t, $pkg) }
    elsif (exists $self->{brace}{ refaddr $t }) {
      $self->_ref($t, $self->{brace}{ refaddr $t }, 'brace', $pkg, $cv);
    }
    return;
  }
  if ($t->isa('PPI::Token::Symbol')) {
    my $canon = _sym_canon($t);
    return unless defined $canon;                  # & and * : not a variable
    $self->_ref($t, $canon, $self->{decl_tok}{ refaddr $t } ? 'decl' : 'sym', $pkg, $cv);
    return;
  }
  if ($t->isa('PPI::Token::ArrayIndex')) {
    my $c = $t->content;
    $c =~ /^\$#(.+)\z/s or die "PCL binder: unknown ArrayIndex shape `$c`\n";
    $self->_ref($t, "\@$1", 'arylen', $pkg, $cv);
    return;
  }
  for my $piece (_interp_texts($t)) {
    my ($text, $mode) = @$piece;
    if ($mode eq 'code') { $self->_code_refs($t, $text, $pkg, $cv); next }
    $self->_interp_refs($t, $text, $mode eq 'regex', $pkg, $cv);
  }
}

# A Symbol's canonical container.  PPI's ->symbol reads a following
# subscript (`$x[0]` -> @x) but ALSO after a cast (`*$g{IO}` -> %g, wrong:
# the `$g` there is the scalar holding the glob), so a cast-prefixed symbol
# is its own content.  Rule 12: a shape not in the list dies.
sub _sym_canon {
  my ($t) = @_;
  my $c = $t->content;
  my $sig = substr($c, 0, 1);
  return undef if $sig eq '&' || $sig eq '*';
  return undef if $c eq q{_};                      # the stat filehandle `-f _`
  return undef if $c =~ /^[\$\@\%]\z/;          # an unnamed signature placeholder `($, $y)`
  # `$#+` / `$#-`: PPI lexes the last index of @+ / @- as ONE Token::Magic (an
  # ArrayIndex everywhere else) -- the array @+ / @- (perlvar).
  return "\@$1" if $c =~ /^\$#([+-])\z/;
  my $prev = $t->sprevious_sibling;
  my $canon = ($prev && $prev->isa(q{PPI::Token::Cast}) && $prev->content ne q{\\}) ? $c : $t->symbol;
  return $canon if $canon =~ /^[\$\@\%](?:[^\W\d][\w]*(?:(?:::|')\w+)*(?:::)?|::\w*(?:::\w+)*|\d+|\^\w+|\{\^\w+\}|[^\w\s{])\z/;
  die "PCL binder: unknown variable token shape `$c` (canon `$canon`)\n";
}

# my/our/state WORD: its declarator symbols (perl's `my` takes ONE term:
# `my $x`, `my ($a, $b)`, `my Dog $spot`), each a new binding from the
# intro point to the scope end.  `my sub NAME` is a lexsub.
sub _declare {
  my ($self, $w, $pkg) = @_;
  my $pv = $w->sprevious_sibling;
  return if $pv && $pv->isa('PPI::Token::Operator') && $pv->content eq '->';
  my $nx = $w->snext_sibling or return;           # `$h{my}`
  return if $nx->isa('PPI::Token::Operator') && $nx->content eq '=>';
  my $kind = $w->content;
  my @syms;
  if ($nx->isa('PPI::Token::Word') && $nx->content eq 'sub') {
    my $name = $nx->snext_sibling;
    return unless $name && $name->isa('PPI::Token::Word');
    my ($from, $to, $scope) = $self->_intro_point($w);
    $self->_add_binding(kind => 'lexsub', canon => '&' . $name->content, pkg => $pkg,
                        decl => $name, decl_ord => $self->_first_ord($name),
                        scope => $scope, from => $from, to => $to) if defined $from;
    return;
  }
  $nx = $nx->snext_sibling if $nx->isa('PPI::Token::Word') && $nx->snext_sibling
                             && $nx->snext_sibling->isa('PPI::Token::Symbol');   # my Dog $spot
  if    ($nx->isa('PPI::Token::Symbol'))   { @syms = ($nx) }
  elsif ($nx->isa('PPI::Structure::List')) { @syms = grep { $_->isa('PPI::Token::Symbol') } $nx->tokens }
  else  { return }                                  # `my` as a bareword / hash key
  my ($from, $to, $scope) = $self->_intro_point($w);
  die "PCL binder: no intro point for `" . $w->content . "` at line "
    . ($w->line_number // '?') . "\n" unless defined $from;
  for my $s (@syms) {
    my $canon = _sym_canon($s) // next;
    $self->{decl_tok}{ refaddr $s } = 1;
    $self->_add_binding(kind => $kind eq 'our' ? 'our' : $kind, canon => $canon,
                        pkg => $pkg, decl => $s, decl_ord => $self->_first_ord($s),
                        scope => $scope, from => $from, to => $to);
  }
}

# THE INTRO POINT (intro_my, pad.c:1451) and the scope end (pad_leavemy,
# pad.c:1503), returned as (from_ord, to_ord, scope_node).
sub _intro_point {
  my ($self, $w) = @_;
  my $c = $w;
  for (my $p = $w->parent; $p; $c = $p, $p = $p->parent) {
    if ($p->isa('PPI::Structure::Block') || $p->isa('PPI::Document')) {
      # newSTATEOP (op.c:8653) calls intro_my: visible from the NEXT statement.
      return ($self->_last_ord($c) + 1, $self->_last_ord($p), $p);
    }
    if ($p->isa('PPI::Structure::For')) {
      # `for (my $i = 0; …; …)`: mnexpr (perly.y:758) introduces after the
      # init part; the scope is the whole loop (remember … block_end).
      my $cmp = $p->parent;
      return ($self->_last_ord($c) + 1, $self->_last_ord($cmp), $cmp);
    }
    if ($p->isa('PPI::Statement::Compound')) {
      # `for my $x (LIST) BLOCK` (perly.y:519): LIST is mexpr, so the loop
      # variable is introduced after LIST and lives to the end of the loop.
      for (my $s = $c->snext_sibling; $s; $s = $s->snext_sibling) {
        next unless $s->isa('PPI::Structure::List') || $s->isa('PPI::Structure::For');
        return ($self->_last_ord($s) + 1, $self->_last_ord($p), $p);
      }
      return ($self->_last_ord($c) + 1, $self->_last_ord($p), $p);
    }
    if ($p->isa('PPI::Structure') && $p->parent && $p->parent->isa('PPI::Statement::Compound')) {
      # if/elsif/unless/while/until/given HEAD: mexpr / texpr+mintro
      # (perly.y:464-497, :704) -- visible after the head, to the end of the
      # compound (an elsif's head opens inside the if's `remember`).
      my $cmp = $p->parent;
      return ($self->_last_ord($p) + 1, $self->_last_ord($cmp), $cmp);
    }
  }
  return;
}

sub _ref {
  my ($self, $tok, $canon, $via, $pkg, $cv) = @_;
  my $a = refaddr $tok;
  push @{ $self->{refs}{$a} },
    { canon => $canon, via => $via, ord => $self->{ord}{$a},
      pkg => $pkg, cv => $cv, tok => $tok };
  $self->{sig}{$a} //= _tok_sig($tok);
  return if $self->{rescan};
  $self->{stats}{refs}++;
  $self->{stats}{via}{$via}++;
}

# The text a token's refs were derived from (a heredoc's is its BODY).
sub _tok_sig {
  my ($t) = @_;
  return $t->isa('PPI::Token::HereDoc') ? join('', $t->heredoc) : $t->content;
}

# ---------------------------------------------------------------------------
# THE CURRENT NAMES.  The legacy resolvers are asked mid-rename: a pass has
# already rewritten some tokens (`my $x` -> `my $x__file__0`) and asks about
# the rest by the names they carry NOW.  The table keeps perl's ranges (a
# rename changes no scope) and re-reads only NAMES: a declaration answers for
# the canon its token spells now, a token whose text changed is re-scanned.

# The canon a declaration's token spells now ('' when it no longer declares).
sub _live_canon {
  my ($b) = @_;
  my $d = $b->{decl_node} or return $b->{canon};
  if ($d->isa('PPI::Token::Symbol')) { return eval { _sym_canon($d) } // '' }
  if ($d->isa('PPI::Token::Word'))   { return $b->{sigil} eq '&' ? '&' . $d->content : $b->{canon} }
  return $b->{canon};                       # a Prototype's / an outer name
}

# A token's refs as its CURRENT text gives them.
sub _cur_refs {
  my ($self, $tok) = @_;
  my $a = refaddr $tok;
  my $orig = $self->{refs}{$a} or return [];
  my $sig = _tok_sig($tok);
  return $orig if $sig eq ($self->{sig}{$a} // '');
  my $c = $self->{cur}{$a};
  return $c->[1] if $c && $c->[0] eq $sig;
  my ($pkg, $cv, $via0, $canon0) = @{ $orig->[0] }{qw(pkg cv via canon)};
  local $self->{rescan} = 1;
  local $self->{refs}{$a} = [];
  if ($tok->isa('PPI::Token::Symbol')) {
    my $canon = eval { _sym_canon($tok) };
    $self->_ref($tok, $canon, $via0, $pkg, $cv) if defined $canon;
  }
  elsif ($tok->isa('PPI::Token::Word')) {
    $self->_ref($tok, substr($canon0, 0, 1) . $tok->content, $via0, $pkg, $cv);
  }
  elsif ($tok->isa('PPI::Token::ArrayIndex')) {
    $self->_ref($tok, "\@$1", 'arylen', $pkg, $cv) if $tok->content =~ /^\$#(.+)\z/s;
  }
  else {
    for my $piece (_interp_texts($tok)) {
      my ($text, $mode) = @$piece;
      if ($mode eq 'code') { $self->_code_refs($tok, $text, $pkg, $cv) }
      else { $self->_interp_refs($tok, $text, $mode eq 'regex', $pkg, $cv) }
    }
  }
  my $new = $self->{refs}{$a};
  $self->{cur}{$a} = [$sig, $new];
  return $new;
}

# The binding $asked names at $ord in the CURRENT program: perl's rule over
# the table, a declaration counting only while its token still spells $asked.
sub _resolve_cur {
  my ($self, $ord, $asked, $pkg) = @_;
  (my $orig = $asked) =~ s/$RENAME_SUFFIX//;
  (my $bare = $orig) =~ s/^[\$\@\%\&]//;
  if ($bare !~ /::|'/) {
    my $best;
    for my $b (@{ $self->{by_canon}{$orig} || [] }) {
      next unless $b->{from} <= $ord && $ord <= $b->{to};
      next unless _live_canon($b) eq $asked;
      $best = $b if !$best || $b->{from} > $best->{from}
                 || ($b->{from} == $best->{from} && $b->{decl_ord} > $best->{decl_ord});
    }
    return $best if $best;
  }
  return $self->_resolve_ord($ord, $asked, $pkg, 1);
}

# What $canon at $node names now: the node's own declaration when it is one,
# else _resolve_cur at its position.
sub _binding_cur {
  my ($self, $node, $canon) = @_;
  my $a = refaddr $node;
  if (my $own = $self->{decl_bind}{$a}) {
    return $own if _live_canon($own) eq $canon;
  }
  my $ord = $self->_first_ord($node);
  return undef unless defined $ord;
  my $list = $self->{refs}{$a};
  my $pkg = $list && @$list ? $list->[0]{pkg} : $self->_pkg_at($node);
  return $self->_resolve_cur($ord, $canon, $pkg);
}

# The interpolating texts of a token: [text, 'dq'|'regex'|'code'].  Raw
# heredocs, single-quote delimiters (m'' s''' qr'' qx'') and tr/// do not
# interpolate.  An s///e replacement is CODE.
sub _interp_texts {
  my ($t) = @_;
  if ($t->isa('PPI::Token::HereDoc')) {
    return () if Pl::PExpr::TokenUtils::heredoc_is_raw($t);
    return ([join('', $t->heredoc), 'dq']);
  }
  if ($t->isa('PPI::Token::Quote::Double') || $t->isa('PPI::Token::Quote::Interpolate')) {
    return ([$t->string, 'dq']);
  }
  if ($t->isa('PPI::Token::QuoteLike::Readline')) {
    (my $s = $t->content) =~ s/^<|>\z//g;
    return ([$s, 'dq']);
  }
  if ($t->isa('PPI::Token::QuoteLike::Backtick')) {
    (my $s = $t->content) =~ s/^`|`\z//g;
    return ([$s, 'dq']);
  }
  if ($t->isa('PPI::Token::QuoteLike::Command')
      || $t->isa('PPI::Token::QuoteLike::Regexp') || $t->isa('PPI::Token::Regexp::Match')
      || $t->isa('PPI::Token::Regexp::Substitute')) {
    my $secs = $t->{sections} || [];
    return () unless @$secs;
    return () if ($secs->[0]{type} // '') eq "''";
    my $regex = !$t->isa('PPI::Token::QuoteLike::Backtick') && !$t->isa('PPI::Token::QuoteLike::Command');
    my $content = $t->content;
    my @out = ([substr($content, $secs->[0]{position}, $secs->[0]{size}), $regex ? 'regex' : 'dq']);
    if ($t->isa('PPI::Token::Regexp::Substitute') && $secs->[1]) {
      my %mod = %{ $t->get_modifiers || {} };
      my $rep = substr($content, $secs->[1]{position}, $secs->[1]{size});
      push @out, [$rep, $mod{e} ? 'code' : 'dq'] unless ($secs->[1]{type} // '') eq "''" && !$mod{e};
    }
    return @out;
  }
  return ();
}

sub _interp_refs {
  my ($self, $t, $text, $in_regex, $pkg, $cv) = @_;
  return unless $text =~ /[\$\@]/;
  my $evs = eval { Pl::InterpScan::scan($text, in_regex => $in_regex ? 1 : 0, postderef_qq => 1) } || [];
  for my $ev (@$evs) {
    my $canon = $ev->{canon};
    if (!defined $canon && $ev->{form} eq 'magic' && defined $ev->{name}) {
      $canon = ($ev->{sigil} eq '$#' ? '@' : $ev->{sigil}) . $ev->{name};
    }
    $self->_ref($t, $canon, 'interp', $pkg, $cv) if defined $canon;
    my @code;
    push @code, $ev->{expr_span} if $ev->{expr_span};
    push @code, $_->{guts_span} for grep { $_->{guts_span} } @{ $ev->{chain} || [] };
    push @code, $ev->{postderef}{guts_span} if $ev->{postderef} && $ev->{postderef}{guts_span};
    for my $sp (@code) {
      my $g = substr($text, $sp->[0], $sp->[1] - $sp->[0]);
      next unless $g =~ /[\$\@]/;
      if ($g =~ /^\s*\$(\w+)\s*\z/) { $self->_ref($t, "\$$1", 'interp', $pkg, $cv); next }
      $self->_code_refs($t, $g, $pkg, $cv);
    }
  }
}

# Embedded CODE (`@{[ … ]}`, `${\ …}`, a subscript `$x[$i+1]`, an s///e
# replacement): its variables are uses at the host token's position, in the
# host's scope -- parsed through Pl::Parser::fragment_doc, the s438f pattern.
sub _code_refs {
  my ($self, $host, $code, $pkg, $cv) = @_;
  return unless $code =~ /[\$\@]/;
  my $frag = eval { Pl::Parser::fragment_doc($code) };
  if (!$frag) { $self->{stats}{fragment_fail}++; return }
  $self->{stats}{fragments}++;
  my %brace = map { (refaddr($_->[0]) => $_->[1]) }
              @{ defined &Pl::Parser2::_brace_name_refs ? Pl::Parser2::_brace_name_refs($frag) : [] };
  for my $ft ($frag->tokens) {
    if ($ft->isa('PPI::Token::Word')) {
      $self->{stats}{fragment_decl}++ if $ft->content =~ /^(?:my|our|state)$/;
      $self->_ref($host, $brace{ refaddr $ft }, 'interp-code', $pkg, $cv) if $brace{ refaddr $ft };
    }
    elsif ($ft->isa('PPI::Token::Symbol')) {
      my $c = _sym_canon($ft);
      $self->_ref($host, $c, 'interp-code', $pkg, $cv) if defined $c;
    }
    elsif ($ft->isa('PPI::Token::ArrayIndex') && $ft->content =~ /^\$#(.+)\z/s) {
      $self->_ref($host, "\@$1", 'interp-code', $pkg, $cv);
    }
    else {
      for my $piece (_interp_texts($ft)) {
        my ($text, $mode) = @$piece;
        if ($mode eq 'code') { $self->_code_refs($host, $text, $pkg, $cv) }
        else { $self->_interp_refs($host, $text, $mode eq 'regex', $pkg, $cv) }
      }
    }
  }
}

# ---------------------------------------------------------------------------
# RESOLUTION
sub _resolve_all {
  my ($self) = @_;
  for my $list (values %{ $self->{refs} }) {
    for my $r (@$list) {
      $r->{binding} = ($r->{via} eq q{decl} && $self->{decl_bind}{ refaddr $r->{tok} })
        || $self->_resolve_ord($r->{ord}, $r->{canon}, $r->{pkg});
      my $b = $r->{binding};
      $self->{stats}{captures}++
        if $b->{decl_ord} >= 0 && $r->{cv} && !$r->{cv}->isa('PPI::Document')
        && $b->{kind} =~ /^(?:my|state|our|param|lexsub)$/
        && !$self->_ord_in($b->{decl_ord}, $r->{cv});
    }
  }
}

sub _ord_in {
  my ($self, $ord, $node) = @_;
  my ($a, $z) = ($self->_first_ord($node), $self->_last_ord($node));
  return defined $a && defined $z && $ord >= $a && $ord <= $z;
}

# pad_findmy_pvn / pad_findlex: the innermost-latest visible binding, else a
# global of the positional package (S_pending_ident) or a special of main.
sub _resolve_ord {
  my ($self, $ord, $canon, $pkg, $globals_only) = @_;
  (my $bare = $canon) =~ s/^[\$\@\%\&]//;
  if (!$globals_only && $bare !~ /::|'/) {
    my $best;
    for my $b (@{ $self->{by_canon}{$canon} || [] }) {
      next unless $b->{from} <= $ord && $ord <= $b->{to};
      $best = $b if !$best || $b->{from} > $best->{from}
                 || ($b->{from} == $best->{from} && $b->{decl_ord} > $best->{decl_ord});
    }
    return $best if $best;
  }
  my ($kind, $home, $name);
  if ($bare =~ /^(.*?)(?:::|')(\w*)\z/ && $bare !~ /^\^/) {
    ($kind, $home) = ('global', length($1) ? $1 : 'main');
    $name = substr($canon, 0, 1) . "${home}::$2";
  }
  elsif ($bare !~ /^[^\W\d]/ || $SPECIAL_MAIN{$bare}) { ($kind, $home) = ('special', 'main'); $name = substr($canon, 0, 1) . "main::$bare" }
  else { ($kind, $home) = ('global', $pkg // 'main'); $name = substr($canon, 0, 1) . "${home}::$bare" }
  return $self->{globals}{$name} //= do {
    $self->{stats}{kind}{$kind}++;
    { id => 'g' . (scalar(keys %{ $self->{globals} }) + 1), kind => $kind, canon => $canon,
      sigil => substr($canon, 0, 1), name => $name, perl_package => $home,
      decl_node => undef, decl_ord => -1, scope_node => undef, from => -1, to => $self->{last} };
  };
}

sub binding_of {
  my ($self, $tok, $canon) = @_;
  my $list = $self->{refs}{ refaddr $tok } or return undef;
  for my $r (@$list) { return $r->{binding} if !defined $canon || $r->{canon} eq $canon }
  return undef;
}

# The binding visible for $canon at $node's position.  A canon this compiler
# RENAMED after the table was built (`$x__file__3`) is asked about under the
# name the source wrote -- the binder answers about the program as written.
sub resolve {
  my ($self, $node, $canon) = @_;
  my $ord = $self->_first_ord($node);
  return undef unless defined $ord;
  $canon =~ s/$RENAME_SUFFIX//;
  if (my $list = $self->{refs}{ refaddr $node }) {
    for my $r (@$list) { return $r->{binding} if $r->{canon} eq $canon }
  }
  return $self->_resolve_ord($ord, $canon, $self->_pkg_at($node));
}

# The positional package of a node not in the ref table (the ref table
# carries the walk's package; this is the same rule read upward).
sub _pkg_at {
  my ($self, $node) = @_;
  my $child = $node;
  for (my $p = $node->parent; $p; $child = $p, $p = $p->parent) {
    if ($p->isa('PPI::Statement::Package')) { return $p->namespace }
    for (my $s = $child->sprevious_sibling; $s; $s = $s->sprevious_sibling) {
      return $s->namespace if $s->isa('PPI::Statement::Package')
        && !grep { $_->isa('PPI::Structure::Block') } $s->schildren;
    }
  }
  return $self->{opt}{pkg} // 'main';
}

sub _is_lexical { return $_[0] && $_[0]{kind} =~ /^(?:my|state|our|param|lexsub|outer)$/ ? 1 : 0 }

# The refs inside $block whose binding is declared OUTSIDE it (pad_findlex's
# OUTER entries).  %opt: bare => restrict to one bare name, canons => {...}.
sub captures {
  my ($self, $block, %opt) = @_;
  my ($a, $z) = ($self->_first_ord($block), $self->_last_ord($block));
  return () unless defined $a && defined $z;
  my @out;
  for my $r ($self->_refs_in($a, $z)) {
    my $c = $r->{canon};
    next if defined $opt{bare} && substr($c, 1) ne $opt{bare};
    next if $opt{canons} && !$opt{canons}{$c};
    my $b = $r->{binding};
    next unless _is_lexical($b) && $r->{via} ne 'decl';
    next if $b->{decl_ord} >= $a && $b->{decl_ord} <= $z;
    push @out, $r;
  }
  return @out;
}

# The refs whose ordinal lies in [$a, $z], in order (a binary search over the
# ord-sorted ref list, built once).
sub _refs_in {
  my ($self, $lo0, $z) = @_;
  my $s = $self->{sorted} //= [ sort { $a->{ord} <=> $b->{ord} } map { @$_ } values %{ $self->{refs} } ];
  my ($lo, $hi) = (0, scalar @$s);
  while ($lo < $hi) { my $m = ($lo + $hi) >> 1; if ($s->[$m]{ord} < $lo0) { $lo = $m + 1 } else { $hi = $m } }
  my @out;
  for (my $i = $lo; $i < @$s && $s->[$i]{ord} <= $z; $i++) { push @out, $s->[$i] }
  return @out;
}

sub _trace {
  my ($self) = @_;
  my @all = sort { $a->{ord} <=> $b->{ord} } map { @$_ } values %{ $self->{refs} };
  for my $r (@all) {
    my $b = $r->{binding};
    my $t = $r->{tok};
    my $d = $b->{decl_node};
    printf STDERR "PCL-BINDER\t%s:%s\t%s\t%s\t%s\t#%s\t%s\t%s\t%s\n",
      $t->line_number // '?', $t->column_number // '?', _short($t->content), $r->{canon},
      $r->{via}, $b->{id}, $b->{kind}, $b->{name},
      $d ? 'decl=' . ($d->line_number // '?') . ':' . ($d->column_number // '?') : 'decl=-';
  }
}

sub _short { my $s = shift; $s =~ s/\s+/ /g; return length($s) > 40 ? substr($s, 0, 37) . '...' : $s }

# ===========================================================================
# THE DUAL-RUN (the hooks in Pl::Parser2 call check(); nothing it computes
# reaches the caller).
my %SEEN;

sub _log_fh {
  my $f = $ENV{PCL_BINDER_LOG};
  return \*STDERR unless defined $f && length $f;
  open my $fh, '>>:utf8', $f or return \*STDERR;
  return $fh;
}

sub _row {
  my (@f) = @_;
  local $!;
  my $fh = _log_fh();
  print {$fh} join("\t", map { my $v = $_ // ''; $v =~ s/[\t\n\r]/ /g; $v } @f), "\n";
  close $fh unless $fh == \*STDERR;
}

sub _file_of {
  my ($p) = @_;
  return $p->filename    if $p->can('has_filename') && $p->has_filename;
  return $p->source_name if $p->can('has_source_name') && $p->has_source_name;
  return '-';
}

# Build the main binder (Parser2 calls this at its build point).
sub build_for {
  my ($parser, $doc) = @_;
  local ($!, $@);
  my $mode = $ENV{PCL_BINDER_CHECK} // '';
  my $b = eval {
    Pl::Binder->new($doc, parser => $parser,
                    outer => ($parser->eval_mode ? [ @{ $parser->eval_captures // [] } ] : []),
                    pkg => ($parser->eval_mode ? $parser->eval_pkg : undef))->bind;
  };
  if (!$b) {
    my $err = $@ || 'unknown';
    die $err if $mode ne 'log';
    _row('E', _file_of($parser), 0, 0, '', 'build', '', '', '', $err);
    return undef;
  }
  $b->{file} = _file_of($parser);
  $b->{mode} = $mode;
  _row('S', $b->{file}, 'refs=' . ($b->{stats}{refs} // 0), 'bindings=' . scalar(@{ $b->{bindings} }),
       'kinds=' . join(',', map { "$_:$b->{stats}{kind}{$_}" } sort keys %{ $b->{stats}{kind} || {} }),
       'via=' . join(',', map { "$_:$b->{stats}{via}{$_}" } sort keys %{ $b->{stats}{via} || {} }),
       'captures=' . ($b->{stats}{captures} // 0), 'fragments=' . ($b->{stats}{fragments} // 0),
       'build_ms=' . $b->{stats}{build_ms}, 'tokens=' . ($b->{last} + 1),
       'eval=' . ($parser->eval_mode ? 1 : 0))
    if $mode eq 'log';
  return $b;
}

# The binder that can answer about $node: the main one, or one built for the
# document $node now lives in (a fragment, or the main document after a pass
# inserted tokens -- a REBUILD, counted with the pass that caused it).
sub _binder_for {
  my ($parser, $node, $which, $pass) = @_;
  my $main = $parser->{_binder} or return undef;
  my $top = $node->top;
  return undef unless $top && $top->isa('PPI::Document');
  my $key = refaddr $top;
  my $b = $key == refaddr($main->{doc}) ? $main : $main->{others}{$key};
  my $tok = $node->isa('PPI::Token') ? $node : $node->first_token;
  return $b if $b && $tok && exists $b->{ord}{ refaddr $tok };
  my $why = !$b ? ($key == refaddr($main->{doc}) ? 'main' : 'new-document')
                : 'inserted-token';
  my $caller = $pass // '?';
  my $nb = eval { Pl::Binder->new($top, %{ $main->{opt} })->bind };
  if (!$nb) {
    _row('E', $main->{file}, 0, 0, '', $which, '', '', '', "rebuild: " . ($@ || '?'));
    return undef;
  }
  $nb->{file} = $main->{file};
  $main->{rebuilds}++;
  _row('R', $main->{file}, $why, $caller, $which, 'tokens=' . ($nb->{last} + 1),
       'build_ms=' . $nb->{stats}{build_ms}) if $main->{mode} eq 'log';
  if ($key == refaddr($main->{doc})) { %$main = (%$main, %$nb, file => $main->{file}, mode => $main->{mode}, others => $main->{others}, sorted => undef) }
  else { $main->{others}{$key} = $nb }
  return $key == refaddr($main->{doc}) ? $main : $nb;
}

sub _verdict_str { my $v = shift; return defined $v ? $v : 'undef' }

sub _rec_str {
  my ($b) = @_;
  return '-' unless $b;
  my $d = $b->{decl_node};
  return "$b->{kind} $b->{name}" . ($d ? ' decl=' . ($d->line_number // '?') . ':' . ($d->column_number // '?') : '');
}

# THE COMPARISON.  $which: binding_at | ref_shadowed | block_captures.
sub check {
  my ($parser, $which, $legacy, @args) = @_;
  local ($!, $@);                           # inert: never leak errno / $@ to the parser
  my $mode = $ENV{PCL_BINDER_CHECK} // '';
  my ($node) = @args;
  my $pass = _pass_name();                   # the pass that asked the resolver
  my $res = eval {
    my $b = _binder_for($parser, $node, $which, $pass) or return;
    $parser->{_binder}{calls}{$which}++;
    my ($mine, $kind, $note) = $which eq 'binding_at'     ? $b->_q_binding_at(@args)
                             : $which eq 'ref_shadowed'   ? $b->_q_ref_shadowed(@args)
                             : $which eq 'block_captures' ? $b->_q_block_captures(@args)
                             : die "PCL binder: unknown resolver `$which`\n";
    $mine = _invert($which, $mine) if ($ENV{PCL_BINDER_INJECT} // '') eq $which;
    my $same = _verdict_str($legacy) eq _verdict_str($mine)
            || ($which ne 'binding_at' && !!$legacy == !!$mine);
    return [1] if $same;
    [0, $b, $mine, $kind, $note];
  };
  if (!$res) {
    my $err = $@ or return;
    die $err if $mode ne 'log';
    _row('E', ($parser->{_binder} || {})->{file} // _file_of($parser), _line($node), _col($node),
         _short(eval { $node->content } // ''), $which, _verdict_str($legacy), '', '', $err);
    return;
  }
  return if $res->[0];
  my (undef, $b, $mine, $kind, $note) = @$res;
  my $extra = $which eq 'block_captures' ? $args[1] : $args[1] // '';
  my $key = join "\0", $b->{file}, $which, refaddr($node), $extra, _verdict_str($legacy), _verdict_str($mine);
  return if $SEEN{$key}++;
  my @row = ('D', $b->{file}, _line($node), _col($node), _short($node->content), $which,
             _verdict_str($legacy), _verdict_str($mine), $kind, "asked=$extra $note", $pass // q{?});
  if ($mode eq 'die') {
    die sprintf("PCL binder: %s disagrees at %s line %s col %s `%s` (%s): legacy=%s binder=%s [%s] %s\n",
                $which, $b->{file}, $row[2], $row[3], $row[4], $extra,
                $row[6], $row[7], $kind, $note);
  }
  _row(@row);
}

# The first three NAMED frames above the resolver (closures and the shared
# rewriter are skipped): which pass asked.
sub _pass_name {
  my @n;
  for (my $i = 3; $i < 40 && @n < 3; $i++) {
    my $s = (caller($i))[3] // last;
    next if $s =~ /__ANON__|^Pl::Binder::|::_rewrite_var_uses$|::_fix_interp_token$|\(eval\)/;
    $s =~ s/^Pl::Parser2:://;
    push @n, $s unless @n && $n[-1] eq $s;
  }
  return join '<', @n;
}

sub _invert {
  my ($which, $v) = @_;
  return $which eq 'binding_at' ? (defined $v ? undef : 'lex') : ($v ? 0 : 1);
}

sub _line { my $n = shift; my $t = $n->isa('PPI::Token') ? $n : $n->first_token; return $t ? ($t->line_number // 0) : 0 }
sub _col  { my $n = shift; my $t = $n->isa('PPI::Token') ? $n : $n->first_token; return $t ? ($t->column_number // 0) : 0 }

# _binding_at's question: 'lex' (a my/state/param in scope), the DECLARING
# package of an `our` alias in scope, or undef (a global here).
sub _q_binding_at {
  my ($self, $node, $canon) = @_;
  my $b = $self->_binding_cur($node, $canon);
  my $v = !$b || $b->{kind} eq 'global' || $b->{kind} eq 'special' ? undef
        : $b->{kind} eq 'our' ? $b->{perl_package} : 'lex';
  return ($v, $b ? $b->{kind} : '-', _rec_str($b));
}

# _ref_shadowed's question: does this use resolve to a binding DECLARED
# between the segment boundary and the use -- i.e. inside $seg_parent, at its
# top level only by one of the segment's own statements @$stmts?
sub _q_ref_shadowed {
  my ($self, $sym, $canon, $stmts, $seg_parent) = @_;
  my $b = $self->_binding_cur($sym, $canon);
  return (0, $b ? $b->{kind} : '-', _rec_str($b) . ' (undeclared)')
    unless $b && $b->{decl_node} && $b->{kind} ne 'outer';
  my $d = $b->{decl_node};
  if (!defined $seg_parent) {
    my $file_level = $b->{scope_node} && $b->{scope_node}->isa('PPI::Document');
    return ($file_level ? 0 : 1, $b->{kind}, _rec_str($b) . ($file_level ? ' (file level)' : ''));
  }
  my $c = $d;
  $c = $c->parent while $c && $c->parent && refaddr($c->parent) != refaddr($seg_parent);
  return (0, $b->{kind}, _rec_str($b) . ' (declared outside the segment parent)')
    unless $c && $c->parent && refaddr($c->parent) == refaddr($seg_parent);
  return (1, $b->{kind}, _rec_str($b)) if grep { refaddr($_) == refaddr($c) } @{ $stmts || [] };
  my $u = $sym;
  $u = $u->parent while $u && $u->parent && refaddr($u->parent) != refaddr($seg_parent);
  return (1, $b->{kind}, _rec_str($b) . ' (same top statement)') if $u && refaddr($u) == refaddr($c);
  return (0, $b->{kind}, _rec_str($b) . ' (another segment)');
}

# _block_captures_name's question: does $block read bare name $bare (one of
# $canons when given) through a lexical declared OUTSIDE it?
sub _q_block_captures {
  my ($self, $block, $bare, $canons) = @_;
  my ($a, $z) = ($self->_first_ord($block), $self->_last_ord($block));
  return (0, 'no-range', 'block not in the table') unless defined $a && defined $z;
  my (@why, %done);
  for my $r0 ($self->_refs_in($a, $z)) {
    next if $done{ refaddr $r0->{tok} }++;
    for my $r (@{ $self->_cur_refs($r0->{tok}) }) {
      next if $r->{via} eq 'decl';
      my $c = $r->{canon};
      next unless substr($c, 1) eq $bare;
      next if $canons && !$canons->{$c};
      my $b = $self->_resolve_cur($r->{ord}, $c, $r->{pkg});
      my $outer = _is_lexical($b) && !($b->{decl_ord} >= $a && $b->{decl_ord} <= $z);
      return (1, $b->{kind}, sprintf('%s@%s:%s %s', $c, _line($r->{tok}), $r->{via}, _rec_str($b)))
        if $outer;
      push @why, sprintf('%s@%s:%s=%s', $c, _line($r->{tok}), $r->{via}, $b->{kind});
    }
  }
  return (0, @why ? 'no-outer-ref' : 'no-ref',
          @why ? join(',', @why[0 .. ($#why > 5 ? 5 : $#why)]) : 'no reference of that name (text only?)');
}

# The calls summary of one transpile, when the parser lets the binder go.
sub DESTROY {
  my ($self) = @_;
  local ($!, $@, $?);                    # a destructor runs during die unwinding
  return unless $self->{mode} && $self->{mode} eq 'log' && $self->{calls};
  _row('C', $self->{file}, map({ "$_=$self->{calls}{$_}" } sort keys %{ $self->{calls} }),
       'rebuilds=' . ($self->{rebuilds} // 0));
}

1;

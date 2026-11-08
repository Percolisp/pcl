# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::Binder;

# Pl::Binder -- THE BINDING TABLE (perl's pad model, pad.c) built in ONE walk
# over a PPI document (s525q dual-run; PRODUCTION since s533b; parser-structure program step 0, member c2; design
# `docs/var-handling-review-s379.md` §3 Direction A and
# `docs/parser-review-s525.md` §3 / §4 Option 1c; measurement
# `docs/binder-dual-run-s525.md`).
#
# Parser2 builds it once per document, before every requalify / rename pass.
# PRODUCTION CONSUMER (1c port 1, s533b): the block-package requalifier asks
# requalify_home for every token it might rewrite (docs/binder-dual-run-s525.md
# §6 item 1); the capture gate, the promoter and the nested-sub hoist ask
# captures_name (1c port 2, s533d, §6 item 2); the span pass, the flatten
# refusal and every rename pass's "is this use the declaration's variable"
# ask spans / binding_after / refers_to_decl (1c port 3, s535a, §6 item 3 --
# the last dual-run hook, _ref_shadowed, left with it).
#
#   PCL_BINDER_CHECK=log   the MEASUREMENT rows (S per build, R per rebuild,
#                          C query counts per transpile, E per binder error)
#                          go to $PCL_BINDER_LOG (STDERR when unset); a
#                          binder error is logged, not fatal, there (the
#                          consumer then dies naming the missing binder)
#   PCL_BINDER_TRACE=1     print every reference with its binding (STDERR)
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
#       ->requalify_home($tok, $canon) the requalifier's home (s533b)
#       Pl::Binder::for_node($p2, $n)   the binder answering about $n
#       ->resolve($node, $canon)       the record visible at $node for $canon
#       ->captures($block)             OUTER bindings read inside $block
#       ->captures_name($n, $bare, $canons, eval => 1)  the capture question (s533d)
#       ->has_string_eval($cv), ->eval_sees($b)  the string-eval CV fact (s533d)
#       Pl::Binder::for_captures($p2, $n)  the binder answering the capture question
#       ->spans($ranges, $skip, kinds => {...})  "does B have a ref in segment j" (s535a)
#       ->uses_of($b), ->text_uses_in($b, $lo, $hi)  a binding's uses; its eval-text uses
#       ->refers_to_decl($tok, $canon, $decl)  the shadow question (s535a)
#       ->binding_after($node, $canon), ->decl_binding($tok), ->ord_range($a, $z)
#       Pl::Binder::for_span($p2, $n)  the binder answering the span / shadow question

use strict;
use warnings;
use Scalar::Util qw(refaddr blessed weaken);
use Time::HiRes ();
use Pl::InterpScan ();
use Pl::PExpr::TokenUtils ();

my %SPECIAL_MAIN = map { $_ => 1 } qw(_ INC ENV SIG ARGV STDIN STDOUT STDERR ARGVOUT);
my $RENAME_SUFFIX = qr/__(?:file|state|shadow|excl|lex|evalcap|lexsub|cond|family|emb)__\w*\z/;

sub new {
  my ($class, $doc, %opt) = @_;
  my $self = bless { doc => $doc, opt => \%opt, bindings => [], by_canon => {},
                 refs => {}, ord => {}, globals => {}, stats => {} }, $class;
  # The parser holds the binder ($p2->{_binder}); a strong back-reference
  # would be a cycle that leaks the parser, the binder and the document per
  # transpile (Pl/t/parser-leak-01.t row 2 caught it under PCL_BINDER_CHECK).
  weaken($self->{opt}{parser}) if ref $self->{opt}{parser};
  return $self;
}

sub _first_ord { my ($s, $n) = @_; my $t = $n->isa('PPI::Token') ? $n : $n->first_token; return $t ? $s->{ord}{refaddr $t} : undef }
sub _last_ord  { my ($s, $n) = @_; my $t = $n->isa('PPI::Token') ? $n : $n->last_token;  return $t ? $s->{ord}{refaddr $t} : undef }

sub bind {
  my ($self) = @_;
  my $t0 = Time::HiRes::time();
  my $doc = $self->{doc};
  my $i = 0;
  for my $t ($doc->tokens) {
    $self->{ord}{refaddr $t} = $i;
    push @{ $self->{text_toks} }, [$i, $t] if _literal_texts_kind($t);
    $i++;
  }
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
    $self->_head_decls($c, $pkg, $cv) if $c->isa('PPI::Statement::Sub') || $c->isa('PPI::Statement::Compound');
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
  my ($self, $st, $pkg, $cv) = @_;
  my @k = $st->schildren;
  for my $j (0 .. $#k) {
    my $e = $k[$j];
    if ($e->isa('PPI::Structure::Signature')) { $self->_signature($st, $e, \@k, $j) }
    elsif ($e->isa('PPI::Token::Prototype') && $st->isa('PPI::Statement::Sub')) {
      $self->_proto_params($st, $e, \@k, $j, $pkg, $cv);
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
  my ($self, $st, $proto, $k, $j, $pkg, $cv) = @_;
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
  # A DEFAULT is an expression evaluated in the sub (#485): its uses are refs
  # at the Prototype token -- one token, so read through the fragment path --
  # except a parameter of an EARLIER part, which the default sees (perly.y
  # sigsub; its own part's name it does not).  Resolved at the token's
  # ordinal, before the block, i.e. to the OUTER binding.
  my %left;
  for my $part (Pl::Parser2::_signature_part_defaults($text)) {
    my ($canon, $default) = @$part;
    if (defined $default && $default =~ /[\$\@\%]/) {
      my $a = refaddr $proto;
      my $n0 = scalar @{ $self->{refs}{$a} || [] };
      $self->_code_refs_doc($proto, $default, $pkg, $cv);
      my $list = $self->{refs}{$a} || [];
      @$list = (@$list[0 .. $n0 - 1], grep { !$left{ $_->{canon} } } @$list[$n0 .. $#$list]);
      delete $self->{refs}{$a} unless @$list;
    }
    $left{$canon} = 1 if defined $canon;
  }
}

# ---------------------------------------------------------------------------
# ONE TOKEN: a declarator word, a variable use, or interpolating text.
sub _token {
  my ($self, $t, $pkg, $cv) = @_;
  if ($t->isa('PPI::Token::Word')) {
    my $w = $t->content;
    $self->_note_string_eval($t, $cv) if ($w eq 'eval' || $w eq 'CORE::eval') && _is_string_eval($t);
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
  # `s///ee`: the replacement's VALUE is compiled as perl at run time -- a
  # string eval (perlop "ee").
  $self->_note_string_eval($t, $cv)
    if $t->isa('PPI::Token::Regexp::Substitute') && ($t->get_modifiers || {})->{ee};
  for my $piece (_interp_texts($t)) {
    my ($text, $mode) = @$piece;
    if ($mode eq 'code') { $self->_code_refs($t, $text, $pkg, $cv); next }
    $self->_interp_refs($t, $text, $mode eq 'regex', $pkg, $cv);
  }
}

# ---------------------------------------------------------------------------
# THE STRING-EVAL CV FACT (s533d, 1c port 2; docs/binder-dual-run-s525.md
# §4(c)).  A string `eval` compiles text AT RUN TIME against the pad its CV
# can see (pp_entereval: the eval's CvOUTSIDE is the running CV), so a name
# that occurs only inside TEXT -- `eval '$x'`, `my $c = '$x'; eval $c` --
# can read a visible outer lexical that the compile-time pad never records.
# The binder records every string eval with its CV (`eval EXPR` /
# `CORE::eval EXPR` / `s///ee`; never `eval BLOCK`, a method `->eval` or a
# key `eval =>`): has_string_eval($cv) is the CV fact.  A literal TEXT
# (text_refs: a quote, a heredoc, qw) that names a lexical L counts as a use
# of L exactly when a string eval runs INSIDE L's visible range
# (eval_sees): that eval can see L, and the text can reach it -- passed as an
# argument, returned, stored (`my $c = '$x'; eval $c`; text in one sub,
# the eval in another).  No string eval in L's range: no text can read L.
# captures_name(..., eval => 1) applies it.  This is the conservative eval
# rule of ir-spec §2b, read through the fragment path
# (Pl::Parser::fragment_doc -- no new scanner).
sub _is_string_eval {
  my ($w) = @_;
  my $pv = $w->sprevious_sibling;
  return 0 if $pv && $pv->isa('PPI::Token::Operator') && $pv->content eq '->';
  my $nx = $w->snext_sibling;
  return 0 if $nx && $nx->isa('PPI::Structure::Block');            # eval BLOCK
  return 0 if $nx && $nx->isa('PPI::Token::Operator') && $nx->content eq '=>';
  return 1;
}

sub _note_string_eval {
  my ($self, $t, $cv) = @_;
  push @{ $self->{evals} }, $self->{ord}{ refaddr $t };
  $self->{cv_eval}{ refaddr $cv }++ if $cv;
  $self->{stats}{string_eval}++;
}

# THE CV FACT: does a string eval run in $cv's own body (a sub body block, an
# anonymous sub's block, or the document; nested CVs are their own)?
sub has_string_eval {
  my ($self, $cv) = @_;
  return $self->{cv_eval}{ refaddr $cv } ? 1 : 0;
}

# Can a string eval see binding $b -- does one run inside its visible range?
sub eval_sees {
  my ($self, $b) = @_;
  for my $e (@{ $self->{evals} || [] }) { return 1 if $e >= $b->{from} && $e <= $b->{to} }
  return 0;
}

# The literal TEXTS a token carries that a string eval could compile: a
# quote's raw body (a dq string's too -- `eval "\$x = 1"` names $x through
# the escape), a heredoc's body, qw's words.  Interpolating regex / command
# tokens contribute through their ordinary interp refs.
sub _literal_texts_kind {
  my ($t) = @_;
  return $t->isa('PPI::Token::HereDoc') || $t->isa('PPI::Token::Quote')
      || $t->isa('PPI::Token::QuoteLike::Words');
}

# The literal-text tokens whose ordinal lies in [$a, $z], in order.
sub _text_toks {
  my ($self, $lo0, $z) = @_;
  my $s = $self->{text_toks} || [];
  my ($lo, $hi) = (0, scalar @$s);
  while ($lo < $hi) { my $m = ($lo + $hi) >> 1; if ($s->[$m][0] < $lo0) { $lo = $m + 1 } else { $hi = $m } }
  my @out;
  for (my $i = $lo; $i < @$s && $s->[$i][0] <= $z; $i++) { push @out, $s->[$i][1] }
  return @out;
}

sub _literal_texts {
  my ($t) = @_;
  return (join '', $t->heredoc) if $t->isa('PPI::Token::HereDoc');
  return ($t->string) if $t->isa('PPI::Token::Quote');
  return (join ' ', $t->literal) if $t->isa('PPI::Token::QuoteLike::Words');
  return ();
}

# The uses a token's literal text names when read as code (cached by the
# token's current text): refs with via `evaltext`, at the token's ordinal,
# in the token's package -- resolved like any use there.
sub text_refs {
  my ($self, $t) = @_;
  my $a = refaddr $t;
  my $sig = _tok_sig($t);
  my $c = $self->{textc}{$a};
  return $c->[1] if $c && $c->[0] eq $sig;
  my @texts = grep { /[\$\@\%]/ } _literal_texts($t);
  my $new = [];
  if (@texts) {
    my $orig = $self->{refs}{$a};
    my ($pkg, $cv) = $orig && @$orig ? @{ $orig->[0] }{qw(pkg cv)} : ($self->_pkg_at($t), undef);
    local $self->{rescan} = 1;
    local $self->{refs}{$a} = [];
    $self->_code_refs_doc($t, $_, $pkg, $cv) for @texts;
    $new = [ map { +{ %$_, via => 'evaltext' } } @{ $self->{refs}{$a} } ];
  }
  $self->{textc}{$a} = [$sig, $new];
  return $new;
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
  # A pattern's EXTENDED flag (the token's trailing /x) decides its `#`
  # comments, which hold no references (task #3856).
  my $x = ($in_regex && $t->can('get_modifiers') && ($t->get_modifiers || {})->{x}) ? 1 : 0;
  my $evs = eval { Pl::InterpScan::scan($text, in_regex => $in_regex ? 1 : 0, x => $x,
                                        postderef_qq => 1) } || [];
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
  return $self->_code_refs_doc($host, $code, $pkg, $cv);
}

sub _code_refs_doc {
  my ($self, $host, $code, $pkg, $cv) = @_;
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

# THE REQUALIFIER'S QUESTION (s533b, 1c port 1): which package's spelling the
# block-package requalifier gives this token's $canon.  Returns (HOME, WHY):
# HOME undef = leave the token as written, else the package whose qualified
# spelling it takes; WHY = the binding kind, `decl` for the token's own
# declaration, `no-token` / `no-ref` when the table holds no record (the
# caller dies, rule 12).
#   my | state | param | lexsub | outer  -> leave (a lexical: pad_findmy)
#   our                                  -> the DECLARING package (SvOURSTASH)
#   global                               -> the positional package at the use
#                                           (toke.c S_pending_ident)
#   special                              -> main (gv.c S_gv_is_in_main)
sub requalify_home {
  my ($self, $tok, $canon) = @_;
  my $list = $self->{refs}{ refaddr $tok } or return (undef, 'no-token');
  my ($r) = grep { $_->{canon} eq $canon } @$list;
  return (undef, 'no-ref') unless $r;
  if ($r->{via} eq 'decl') {
    # An `our` DECLARATOR whose statement's own lowering does not qualify it
    # -- a `for our $x` loop head, `local our $x`, an `our` inside an
    # expression -- is spelled in its DECLARING package, as its uses already
    # are (#4440, #4422); a plain `our $x` statement qualifies itself
    # (Parser2::_lower_our_decl), so its declarator is left as written.
    my $b = $r->{binding};
    return ($b->{perl_package}, 'our-decl') if $b && $b->{kind} eq 'our' && !_leads_our_stmt($tok);
    return (undef, 'decl');
  }
  my $b = $r->{binding};
  my $k = $b->{kind};
  return (undef, $k) if $k =~ /^(?:my|state|param|lexsub|outer)$/;
  return ($b->{perl_package}, $k) if $k eq 'our' || $k eq 'global';
  return ('main', $k) if $k eq 'special';
  die "PCL binder: unknown binding kind `$k` for $canon\n";
}

# Is the declarator $tok's `our` word the LEADING word of an `our` statement
# (`our $x …;`, `our ($a, $b) …;`) -- the shape _lower_our_decl lowers?
sub _leads_our_stmt {
  my ($tok) = @_;
  my $w = $tok->sprevious_sibling;
  if (!($w && $w->isa('PPI::Token::Word') && $w->content eq 'our')) {
    my $l = $tok->parent;
    $l = $l->parent while $l && !$l->isa('PPI::Structure::List') && !$l->isa('PPI::Statement::Variable');
    $w = $l && $l->isa('PPI::Structure::List') ? $l->sprevious_sibling : undef;
  }
  return 0 unless $w && $w->isa('PPI::Token::Word') && $w->content eq 'our';
  my $st = $w->parent;
  return ($st && $st->isa('PPI::Statement::Variable') && refaddr($st->schild(0)) == refaddr($w)) ? 1 : 0;
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

# Does a `my` / `state` of $canon declared OUTSIDE $node have a SCOPE that
# encloses $node -- wherever in that scope it is declared, before or after
# $node?  The promoter's eval-pair test (Parser2::_enclosing_lex_decl): such
# a name has a let-bound pair in the eval alists of that scope, so a cell
# nested inside it must take the per-site pair, never the global alias.
# Asked about the program as written (a renamed declaration still counts).
sub enclosing_decl {
  my ($self, $node, $canon) = @_;
  my ($a, $z) = ($self->_first_ord($node), $self->_last_ord($node));
  return 0 unless defined $a && defined $z;
  for my $b (@{ $self->{by_canon}{$canon} || [] }) {
    next unless $b->{kind} eq 'my' || $b->{kind} eq 'state';
    next if $b->{decl_ord} >= $a && $b->{decl_ord} <= $z;
    my $s = $b->{scope_node} or next;
    my ($sa, $sz) = ($self->_first_ord($s), $self->_last_ord($s));
    return 1 if defined $sa && $sa <= $a && $z <= $sz;
  }
  return 0;
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
  my $s = $self->_sorted_refs;
  my ($lo, $hi) = (0, scalar @$s);
  while ($lo < $hi) { my $m = ($lo + $hi) >> 1; if ($s->[$m]{ord} < $lo0) { $lo = $m + 1 } else { $hi = $m } }
  my @out;
  for (my $i = $lo; $i < @$s && $s->[$i]{ord} <= $z; $i++) { push @out, $s->[$i] }
  return @out;
}

sub _sorted_refs {
  my ($self) = @_;
  return $self->{sorted} //= [ sort { $a->{ord} <=> $b->{ord} } map { @$_ } values %{ $self->{refs} } ];
}

# ---------------------------------------------------------------------------
# THE SPAN QUESTION (s535a, 1c port 3; docs/binder-dual-run-s525.md §6 item 3).
# Parser2 splits the top level into SECTIONS at `package NAME;` (each one its
# own CL top-level form, read in its own package); a my-lexical whose USE lies
# in a later section than its declaration must become a package cell.  The
# question is "does binding B have a ref in segment j": the ref table grouped
# by binding, a segment = a range of ordinals.  As written: the table is the
# source's (the span pass is the first rename pass; a token renamed since is
# still the same token).

# The USES of binding $b (its declaration excluded), in ordinal order.
sub uses_of {
  my ($self, $b) = @_;
  my $idx = $self->{by_binding} //= do {
    my %h;
    for my $r (@{ $self->_sorted_refs }) {
      next if $r->{via} eq 'decl' || !$r->{binding};
      push @{ $h{ refaddr $r->{binding} } }, $r;
    }
    \%h;
  };
  return @{ $idx->{ refaddr $b } || [] };
}

# The literal-TEXT uses of $b in [$lo, $hi] -- the string-eval rule of port 2
# (§4(c), ir-spec §2b.4): a quote / heredoc / qw text naming $b counts iff a
# string eval runs inside $b's visible range (eval_sees).  Resolved as written
# at the text's position.
sub text_uses_in {
  my ($self, $b, $lo, $hi) = @_;
  return () unless @{ $self->{evals} || [] } && $self->eval_sees($b);
  (my $bare = $b->{canon}) =~ s/^[\$\@\%\&]//;
  my @out;
  for my $t ($self->_text_toks($lo, $hi)) {
    next if index(_tok_sig($t), $bare) < 0;
    for my $r (@{ $self->text_refs($t) }) {
      next unless $r->{canon} eq $b->{canon};
      my $rb = $self->_resolve_ord($r->{ord}, $r->{canon}, $r->{pkg});
      push @out, $r if $rb && refaddr($rb) == refaddr($b);
    }
  }
  return @out;
}

# Is the literal-text token $t the OPERAND of a string eval (`eval '...'`,
# `eval(q{...})`, `eval <<'EOS'`) -- text this eval compiles, not text that
# merely might reach one?
sub _is_eval_operand {
  my ($t) = @_;
  my $n = $t;
  $n = $n->parent while $n->parent && ($n->parent->isa('PPI::Statement::Expression')
                                      || $n->parent->isa('PPI::Structure::List'))
                    && !$n->sprevious_sibling;
  my $w = $n->sprevious_sibling;
  return $w && $w->isa('PPI::Token::Word') && ($w->content eq 'eval' || $w->content eq 'CORE::eval')
      && _is_string_eval($w) ? 1 : 0;
}

# THE SPANS.  $ranges = [ [lo, hi] | undef, ... ] the ordinal range of each
# segment in order (undef = an empty segment), $skip = { i => 1 } the segments
# whose declarations never span (a `package NAME { }` block's: its `my`s die
# with the block), kinds => { our => 1 } (default my + state) the binding
# kinds asked about.  Returns one record per such binding declared in a
# segment i and USED in a later one -- { b, di, segs => { j => 1 }, first }
# (first = the first later segment with a use; text => 1 when the only later
# uses are POSSIBLE ones: literal texts some string eval in range MIGHT compile,
# none the operand of an eval -- the renamer promotes such a binding when it
# can, and the checker does not refuse it) -- in declaration order.
sub spans {
  my ($self, $ranges, $skip, %opt) = @_;
  my $kinds = $opt{kinds} || { my => 1, state => 1 };
  # the segments are disjoint and in document order: a binary search on lo
  my @nz = grep { $ranges->[$_] } 0 .. $#$ranges;
  my $seg_of = sub {
    my ($ord) = @_;
    my ($l, $h) = (0, scalar @nz);
    while ($l < $h) { my $m = ($l + $h) >> 1; if ($ranges->[ $nz[$m] ][1] < $ord) { $l = $m + 1 } else { $h = $m } }
    return undef if $l >= @nz;
    my $j = $nz[$l];
    return $ord >= $ranges->[$j][0] ? $j : undef;
  };
  my @out;
  for my $b (@{ $self->{bindings} }) {
    next unless $kinds->{ $b->{kind} };
    next unless $b->{decl_ord} >= 0;
    my $di = $seg_of->($b->{decl_ord});
    next if !defined $di || $skip->{$di};
    my (%segs, $code);
    for my $r ($self->uses_of($b)) {
      my $j = $seg_of->($r->{ord});
      $segs{$j} = $code = 1 if defined $j && $j > $di;
    }
    for my $j ($di + 1 .. $#$ranges) {
      next if $segs{$j} || !$ranges->[$j];
      my @t = $self->text_uses_in($b, @{ $ranges->[$j] }) or next;
      $segs{$j} = 1;
      # a text that IS a string eval's operand (`eval '$x'`) is a DEFINITE use
      $code = 1 if grep { _is_eval_operand($_->{tok}) } @t;
    }
    next unless %segs;
    my $first; for (keys %segs) { $first = $_ if !defined $first || $_ < $first }
    push @out, { b => $b, di => $di, segs => \%segs, first => $first, text => $code ? 0 : 1 };
  }
  return @out;
}

# The binding a DECLARATOR token makes (`my $x`'s `$x`), or undef.
sub decl_binding {
  my ($self, $tok) = @_;
  return $self->{decl_bind}{ refaddr $tok };
}

# The binding $tok's use of $canon names AS WRITTEN: the token's own record
# when its text is unchanged since the build, else $canon (rename suffix
# stripped) resolved at the token's ordinal.  0 = the token does not use
# $canon; undef = the table holds no record of the token (the caller dies,
# rule 12).
sub _use_binding {
  my ($self, $tok, $canon) = @_;
  my $a = refaddr $tok;
  return undef unless defined $self->{ord}{$a};   # not in the table at all
  my $list = $self->{refs}{$a} or return 0;       # in it, but no variable use
  (my $orig = $canon) =~ s/$RENAME_SUFFIX//;
  if (_tok_sig($tok) eq ($self->{sig}{$a} // '')) {
    for my $r (@$list) { return $r->{binding} if $r->{canon} eq $orig }
    return 0;
  }
  return $self->_resolve_ord($self->{ord}{$a}, $orig, $list->[0]{pkg});
}

# THE SHADOW QUESTION every rename pass asks (it replaces _ref_shadowed): is
# $tok's use of $canon a use of the variable $decl declares?  1 / 0, or undef
# when the table holds no record of either token (rule 12: the caller dies).
sub refers_to_decl {
  my ($self, $tok, $canon, $decl) = @_;
  my $b = $self->{decl_bind}{ refaddr $decl } or return undef;
  my $u = $self->_use_binding($tok, $canon);
  return undef unless defined $u;
  return ($u && refaddr($u) == refaddr($b)) ? 1 : 0;
}

# The binding of $canon visible right AFTER $node (in $node's scope), or undef
# when nothing follows it in the document.
sub binding_after {
  my ($self, $node, $canon) = @_;
  my $z = $self->_last_ord($node);
  return undef unless defined $z && $z < $self->{last};
  return $self->_resolve_ord($z + 1, $canon, $self->_pkg_at($node));
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
# THE MEASUREMENT ROWS (PCL_BINDER_CHECK=log, tools/binder-check.pl): one S
# row per build, an R row per rebuild, a C row of query counts per transpile,
# an E row per binder error.  Nothing here reaches the caller.  (The D rows
# of the s525q dual-run left with its last hook, 1c port 3.)

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
    _row('E', $main->{file}, 0, 0, '', $which, '', '', '', "rebuild: " . ($@ || '?')) if $main->{mode};
    return undef;
  }
  $nb->{file} = $main->{file};
  $main->{rebuilds}++;
  _row('R', $main->{file}, $why, $caller, $which, 'tokens=' . ($nb->{last} + 1),
       'build_ms=' . $nb->{stats}{build_ms}) if ($main->{mode} // '') eq 'log';
  if ($key == refaddr($main->{doc})) { %$main = (%$main, %$nb, file => $main->{file}, mode => $main->{mode}, others => $main->{others}, sorted => undef, by_binding => undef, textc => undef, held => undef) }
  else { $main->{others}{$key} = $nb }
  return $key == refaddr($main->{doc}) ? $main : $nb;
}

# The binder that answers about $node in PRODUCTION (the block-package
# requalifier, s533b): the main one, or a rebuild when $node lives in a
# document re-parsed after the build or is a token inserted since (the R row
# in log mode; none fired in the four populations + the gate).
sub for_node {
  my ($parser, $node) = @_;
  $parser->{_binder}{calls}{requalify}++ if $parser->{_binder};
  return _binder_for($parser, $node, 'requalify', 'requalify');
}

# The binder that answers the CAPTURE question about $node (1c port 2, s533d):
# the capture gate, the promoter, the nested-sub hoist and the closure tests.
sub for_captures {
  my ($parser, $node) = @_;
  $parser->{_binder}{calls}{captures}++ if $parser->{_binder};
  return _binder_for($parser, $node, 'captures', 'captures');
}

# The binder that answers the SPAN and SHADOW questions about $node (1c port
# 3, s535a): the span pass, the flatten refusal, and every rename pass's
# "is this use the declaration's variable" (the old _ref_shadowed).
sub for_span {
  my ($parser, $node) = @_;
  $parser->{_binder}{calls}{span}++ if $parser->{_binder};
  return _binder_for($parser, $node, 'span', 'span');
}

# The ordinal range [first, last] of the nodes $a .. $z (undef when the table
# does not hold them).
sub ord_range {
  my ($self, $a, $z) = @_;
  my ($lo, $hi) = ($self->_first_ord($a), $self->_last_ord($z));
  return (defined $lo && defined $hi) ? [$lo, $hi] : undef;
}

sub _rec_str {
  my ($b) = @_;
  return '-' unless $b;
  my $d = $b->{decl_node};
  return "$b->{kind} $b->{name}" . ($d ? ' decl=' . ($d->line_number // '?') . ':' . ($d->column_number // '?') : '');
}

sub _line { my $n = shift; my $t = $n->isa('PPI::Token') ? $n : $n->first_token; return $t ? ($t->line_number // 0) : 0 }

# THE CAPTURE QUESTION (1c port 2): does $node (a sub body, a whole
# Statement::Sub -- its signature included -- or any block) read the bare
# name $bare (restricted to the canons in $canons when given) through a
# lexical declared OUTSIDE $node?  pad_findlex's OUTER entries, asked about
# the CURRENT names (a rename pass may have run).  eval => 1: the names the
# node's literal texts mention (text_refs) count too, for a lexical some
# string eval can see (eval_sees) -- the string-eval rule.  Returns
# (VERDICT, KIND, NOTE).
sub captures_name {
  my ($self, $block, $bare, $canons, %opt) = @_;
  my ($a, $z) = ($self->_first_ord($block), $self->_last_ord($block));
  return (0, 'no-range', 'block not in the table') unless defined $a && defined $z;
  # Every token of the node must be one the table holds: a token inserted
  # since the build carries no record, and a use it spells would go uncounted
  # (rule 12: the caller dies on `no-record`).  Checked once per node.
  my $n = $self->{held}{ refaddr $block } //= do { my @t = $block->isa('PPI::Token') ? ($block) : $block->tokens; scalar @t };
  return (0, 'no-record', ($n - ($z - $a + 1)) . ' token(s) the table does not hold')
    if $n != $z - $a + 1;
  my (@why, %done);
  my @toks = grep { !$done{ refaddr $_ }++ } map { $_->{tok} } $self->_refs_in($a, $z);
  my $ntok = @toks;
  push @toks, $self->_text_toks($a, $z) if $opt{eval} && @{ $self->{evals} || [] };
  for my $i (0 .. $#toks) {
    next if $i >= $ntok && index(_tok_sig($toks[$i]), $bare) < 0;       # the text never spells it
    my $refs = $i < $ntok ? $self->_cur_refs($toks[$i]) : $self->text_refs($toks[$i]);
    for my $r (@$refs) {
      next if $r->{via} eq 'decl';
      my $c = $r->{canon};
      next unless substr($c, 1) eq $bare;
      next if $canons && !$canons->{$c};
      my $b = $self->_resolve_cur($r->{ord}, $c, $r->{pkg});
      my $outer = _is_lexical($b) && !($b->{decl_ord} >= $a && $b->{decl_ord} <= $z)
                && ($i < $ntok || $self->eval_sees($b));
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

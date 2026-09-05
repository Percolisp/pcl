# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::PExpr::TokenUtils;

use v5.20;
use strict;
use warnings;

use Moo;
use Scalar::Util ();

sub is_atomic {
  my $self      = shift;
  my $stmt      = shift;

  return 1
      if $self->is_string($stmt) || $self->is_number($stmt)
      || $self->is_var($stmt);

  # Note: s/// and tr/// are NOT atomic - they need a target ($_)
  # They're handled by is_regexp() and wrapped with '$_ =~' if standalone

  # $#arr - array last index
  return 1 if ref($stmt) eq 'PPI::Token::ArrayIndex';

  return undef;
}

sub is_regexp {
  my $self      = shift;
  my $stmt      = shift;

  # Match, Substitute (s///), or Transliterate (tr///, y///)
  return 1
      if ref($stmt) =~ /PPI::Token::Regexp::(Match|Substitute|Transliterate)/;

  return undef;
}

sub is_string {
  my $self      = shift;
  my $stmt      = shift;

  return 1 # , $stmt->content)
      if ref($stmt) =~ /PPI::Token::Quote::Single/;

  # q{...} - literal quoting, no interpolation (like single-quoted)
  return 1
      if ref($stmt) eq 'PPI::Token::Quote::Literal';

  if (ref($stmt) =~ /PPI::Token::Quote::Double/) {
    # Check if interpolation is needed
    my $content = $stmt->content();
    # Remove quotes and check for $ or @
    my $inner = $content;
    $inner =~ s/^"//;
    $inner =~ s/"$//;

    # Return 2 if interpolation needed, 1 if plain string.
    # Strip \\ pairs first so \\$var is seen as interpolatable
    # (two backslashes = one literal backslash, $ is still a variable).
    (my $tmp = $inner) =~ s/\\\\/\x00\x00/g;
    if ($tmp =~ /(?<!\\)[\$\@]/) {
      return 2;  # Needs interpolation
    }
    return 1; # Plain double-quoted string
  }

  # Handle qq{} and other Quote::Interpolate forms
  if (ref($stmt) eq 'PPI::Token::Quote::Interpolate') {
    # ->string, not a hand-strip: `qq {…}` (whitespace before the delimiter)
    # made the old `s/^qq.//` take the SPACE as the delimiter and leave the
    # braces in the content — see the note in StringInterpolation.pm.
    my $content = $stmt->can('string') ? $stmt->string : do {
      my $c = $stmt->content();
      $c =~ s/^qq.//;
      $c =~ s/.$//;
      $c;
    };

    # Return 2 if interpolation needed, 1 if plain
    (my $tmp = $content) =~ s/\\\\/\x00\x00/g;
    if ($tmp =~ /(?<!\\)[\$\@]/) {
      return 2;  # Needs interpolation
    }
    return 1;
  }

  return undef;
}

sub is_number {
  my $self      = shift;
  my $stmt      = shift;

  return 1 # , $stmt->content)
      if ref($stmt) =~ /PPI::Token::Number/;
  return undef;
}

sub is_var {
  my $self      = shift;
  my $stmt      = shift;

  # Handle filehandes, *foo etc too.
  # Also handle magic variables like $/, $_, $1, etc.
  return 1 # , $stmt->content)
      if ref($stmt) =~ /PPI::Token::(Symbol|Magic)/;
  return undef;
}

# "->"
sub is_arrow_op {
  my $self      = shift;
  my $stmt      = shift;

  return 1
      if ref($stmt) eq 'PPI::Token::Operator' && $stmt->content() eq '->';
  return undef;
}

# The five brace predicates (#387 family 34, s413): one line each, the same
# truth table (1 / undef), and deliberately NO shared helper — is_hash_braces
# is the hottest predicate in the compiler (92 k calls in the s413 12-file
# sample, 176 k in s411's); a call layer here is a measurable compile-time
# cost, which is also why Pl::PExpr calls these five as plain functions.
sub is_arr_or_hash_braces {
  my (undef, $stmt) = @_;
  return ref($stmt) eq 'PPI::Structure::Subscript'
      && ($stmt->start() eq '[' || $stmt->start() eq '{') ? 1 : undef;
}

sub is_arr_braces {
  my (undef, $stmt) = @_;
  return ref($stmt) eq 'PPI::Structure::Subscript' && $stmt->start() eq '[' ? 1 : undef;
}

# Not for code. :-)
sub is_hash_braces {
  my (undef, $stmt) = @_;
  return ref($stmt) eq 'PPI::Structure::Subscript' && $stmt->start() eq '{' ? 1 : undef;
}

sub is_inline_hash {
  my (undef, $stmt) = @_;
  return ref($stmt) eq 'PPI::Structure::Constructor' && $stmt->start() eq '{' ? 1 : undef;
}

sub is_inline_arr {
  my (undef, $stmt) = @_;
  return ref($stmt) eq 'PPI::Structure::Constructor' && $stmt->start() eq '[' ? 1 : undef;
}

sub is_token_operator {
  my $self      = shift;
  my $stmt      = shift;

  # Handle standard operators
  if (ref($stmt) eq 'PPI::Token::Operator') {
    return $stmt->content();
  }

  # Handle Cast tokens (deref operators: $, @, %, &, *)
  # These act as prefix operators for dereference
  if (ref($stmt) eq 'PPI::Token::Cast') {
    return $stmt->content();
  }

  # Handle word-form binary operators (PPI::Token::Word)
  # PPI tokenizes 'isa' as a Word, not an Operator.
  if (ref($stmt) eq 'PPI::Token::Word') {
    my $word = $stmt->content();
    return $word if $word eq 'isa';
  }

  return undef;
}

sub is_list {
  my $self      = shift;
  my $stmt      = shift;

  return 1
      if ref($stmt) eq 'PPI::Structure::List';
  return undef;
}

# This can be both a sub name and a bareword as a filehandle (won't
# work from v5.30!)
sub is_word {
  my $self      = shift;
  my $stmt      = shift;

  return 1
      if ref($stmt) eq 'PPI::Token::Word';
  return undef;
}

# Instead of PPI Expr, represents a packed node tree.
sub is_internal_node_type {
  my $self      = shift;
  my $node      = shift;

  return (defined $node->{type} ? $node->{type} : 1)
      if ref($node) eq 'PPIreference';
  return undef;
}

# ---- the ONE below-assignment precedence table (task #138) ----------------
#
# Plain subs, not methods: both statement parsers (Pl::Parser and
# Pl::Parser2) need this classification, and neither holds a TokenUtils
# object.  It lives here because it is token-classification knowledge, and
# because the review (docs/v2-code-review.md §2) found it hand-rolled in
# three places and MISSING from five more — each copy a separate bug.
#
# Perl parses `$x = A, B` / `$x = A or B` as `($x = A), B` / `($x = A) or B`:
# assignment (and OP=) binds TIGHTER than `,`/`=>`/`or`/`and`/`xor`.  Any
# statement handler that slices "everything after the `=`" as the initializer
# folds such a tail into the value (op/lex_assign.t: `$a = readlink 'x','y'`
# must leave $a undef).
#
# lowprec_idx: index of the first DEPTH-0 operator below assignment
# precedence at or after $from, or undef when the run is clean.  Structures
# (parens / braces / subscripts) are single PPI children, so a scan over a
# statement's schildren only ever sees depth-0 operators.
sub lowprec_idx {
  my ($toks, $from) = @_;
  for my $i (($from // 0) .. $#$toks) {
    my $t = $toks->[$i];
    return $i if $t->isa('PPI::Token::Operator')
      && $t->content =~ /^(?:,|=>|or|and|xor)$/;
  }
  return undef;
}

# May the run $toks[$from .. $idx-1] be a PARENLESS list operator whose
# argument list swallows the operator at $idx?  Only `,`/`=>` are ambiguous:
# a rightward list operator's arguments run to the closing paren, the `;`, or
# an operator BELOW list-operator precedence -- i.e. `not`/`and`/`or`/`xor`,
# which are therefore always a safe split point.  For a comma the answer is
# conservative: a bare Word not immediately followed by its own paren list may
# be a list operator eating the comma (`state $c = \substr $s, 0, 1`), so the
# split is declined.  A caller that can hand the WHOLE run to the expression
# parser should do that instead of splitting -- PExpr owns the ambiguity, and
# only a caller that must interpose code between head and tail needs to split.
sub lowprec_split_safe {
  my ($toks, $from, $idx) = @_;
  return 1 if $toks->[$idx]->content =~ /^(?:or|and|xor)$/;
  for my $i ($from .. $idx - 1) {
    my $t = $toks->[$i];
    next unless $t->isa('PPI::Token::Word');
    my $nx = $toks->[$i + 1];
    return 0 unless $nx && $nx->isa('PPI::Structure::List');
  }
  return 1;
}

# ---- the ONE compound-assignment operator set (task #140) -----------------
#
# Perl's `OP=` operators, complete.  This existed as FOUR hand-rolled copies
# and two of them omitted the string-bitwise trio `&.= |.= ^.=` (feature
# 'bitwise'); both omissions were live divergences (s316v):
#   - %RANGE_SPLIT_STOP (VarAnnotator): `for ($y |.= "a" .. 3)` split at the
#     `..` and ran 4 iterations — but assignment binds LOOSER than `..`, so
#     the range is the assignment's RHS and perl runs ONE;
#   - the state-decl normalization (Parser2): `state ($u) |.= "a"` never got
#     rewritten to `state $u ; $u |.= "a"`, and yielded undef.
# Order matters for text matching: longest first, so `**=` is tried before
# `*=` and `&.=` before `&=`.
my @COMPOUND_ASSIGN = sort { length($b) <=> length($a) || $a cmp $b }
  qw(+= -= *= /= %= **= x= .=
     ||= &&= //=
     <<= >>= &= |= ^= &.= |.= ^.=);
my %COMPOUND_ASSIGN = map { $_ => 1 } @COMPOUND_ASSIGN;
my $COMPOUND_ASSIGN_ALT = join '|', map { quotemeta } @COMPOUND_ASSIGN;

# The set itself, longest-first — for callers that need to build their own
# lookup table (a stop-set, a hash keyed on more than these).
sub compound_assign_ops { return @COMPOUND_ASSIGN }

# Is this token content an `OP=` operator?  (`=` itself is NOT included —
# ask is_assign_op for that.)
sub is_compound_assign { return defined $_[0] && $COMPOUND_ASSIGN{ $_[0] } ? 1 : 0 }

# Any assignment operator: `=` or an `OP=`.
sub is_assign_op {
  return 0 unless defined $_[0];
  return 1 if $_[0] eq '=';
  return is_compound_assign($_[0]);
}

# Alternation for scanning raw SOURCE TEXT (where there are no PPI tokens to
# ask).  Callers must still exclude `==` themselves with a `(?!=)` tail — an
# `OP=` never legally abuts another `=`.
sub compound_assign_text_re { return qr/(?:$COMPOUND_ASSIGN_ALT)/ }

# --- Source SITE of an element: "where does this sit, and in which file?" ----
#
# Perl's compile-time decisions are top-down: a bareword is a call only if the
# name is already known where the call site is COMPILED (task #266).  Answering
# that needs two positions and the knowledge that they are comparable at all —
# positions from two different PPI documents (a bundled module, an eval string)
# are not.  So the document's identity is part of the site.
#
# Returns undef for a synthesized or detached element with no location; every
# caller must read that as "unknown", never as "before".
sub decl_site {
  my ($elem) = @_;
  return undef unless ref($elem) && Scalar::Util::blessed($elem)
                   && $elem->can('location') && $elem->can('top');
  my $loc = $elem->location or return undef;
  my $top = $elem->top      or return undef;
  return { doc => Scalar::Util::refaddr($top), pos => [ $loc->[0], $loc->[1] ] };
}

# Is this PPI::Token::HereDoc a RAW (non-interpolating) heredoc?  THE one
# predicate — every site that asks "does this heredoc interpolate?" calls it
# (#301).  A heredoc is raw exactly when its terminator is SINGLE-quoted, and
# perl allows both `~` (indented form) and whitespace between `<<` and a
# QUOTED terminator: all of `<<'E'`, `<< 'E'`, `<<~'E'`, `<<~ 'E'` are raw.
# Four hand-written regexes used to answer this and every one of them was
# narrower than perl: PExpr's `/^<<'/` missed both `~` and the space, the three
# Parser2 copies missed the space.  A miss is SILENT-WRONG — the text is run
# through string interpolation, so `$x`/`@y` vanish and `\n` collapses to a
# newline, with no diagnostic (closure.t RT #23265's fresh_perl source).
#
# Reads `{_heredoc_content}` first: the rename passes rewrite a heredoc's
# marker there, and content() still holds the original.
sub heredoc_is_raw {
  my ($t) = @_;
  return 0 unless ref($t) && Scalar::Util::blessed($t)
               && $t->isa('PPI::Token::HereDoc');
  return (($t->{_heredoc_content} // $t->content) =~ /^<<~?\s*'/) ? 1 : 0;
}

# Is this heredoc a COMMAND heredoc (`<<`TAG``)?  Same shape question as
# heredoc_is_raw and the same spelling latitude: perl allows `~` and
# whitespace between `<<` and the quoted terminator, so `` <<`E` ``,
# `` << `E` `` and `` <<~`E` `` are all one.  A backtick-terminated heredoc is
# `readpipe` with a heredoc body: the text INTERPOLATES like `<<"E"` and is
# then RUN, and its value is the child's stdout.  PCL used to lower it exactly
# like `<<"E"` — the command line itself came back as the value, silently
# (task #702).
sub heredoc_is_command {
  my ($t) = @_;
  return 0 unless ref($t) && Scalar::Util::blessed($t)
               && $t->isa('PPI::Token::HereDoc');
  return (($t->{_heredoc_content} // $t->content) =~ /^<<~?\s*`/) ? 1 : 0;
}

# --- Does this run of code CALL USER CODE?  (task #1022 half (b)) -----------
#
# THE LICENCE for a dynamic-loop-exit frame (ir-spec §6.2, Kind-A gate
# `dyn-loop-exit`).  Perl's unlabelled `last`/`next`/`redo` act on the
# innermost DYNAMICALLY enclosing loop, so a loop whose body can reach perl
# code this compiler cannot see may have to catch one.  A body that cannot
# reach any such code has no dynamic exit to catch, and is emitted EXACTLY as
# it is today — that byte-identity is the licence's whole point, since the
# frame is what a counting loop must not pay for.
#
# CONSERVATIVE IN THE COSTING DIRECTION, deliberately: a bareword this
# predicate has never heard of counts as a CALL, so being wrong costs one
# `catch` plus one special bind per loop ENTRY and never a missing frame.  The
# safe set is therefore the BUILTIN table the compiler already trusts
# (Pl::PExpr::Config::known_no_of_params — PExpr::_bareword_callable_here's
# "core callable word") plus the grammar words PPI hands over as Words, MINUS
# the handful of builtins that reach user code with no other syntactic marker.
#
# WHAT IT CANNOT SEE, and this is the residue docs/not-supported.md names:
# user code reached through OVERLOAD, a TIE handler, DESTROY or a %SIG handler
# carries no syntactic marker at all, so `for (…) { $a + $b }` is licence-free
# even though an overloaded `+` could run a sub that says `last`.  Such an
# exit then takes the perl-shaped "outside a loop block" die, which is loud.
my %DYN_CALLY_BUILTIN = map { $_ => 1 } qw(
  eval do require tie untie tied dbmopen dbmclose
);

# Words PPI hands over as PPI::Token::Word that are not in the builtin table
# and are not calls: perl's grammar, its word-spelled operators, and the
# builtins the table happens not to carry.
my %DYN_SAFE_GRAMMAR = map { $_ => 1 } qw(
  my our local state sub package use no
  if unless while until for foreach else elsif continue
  last next redo return
  and or not xor cmp x eq ne lt gt le ge
  q qq qw qr m s tr y
  unlink select flock study reset lock sleep chomp chop
  dbmclose endhostent endnetent endprotoent endservent
  sethostent setnetent setprotoent setservent
  gethostbyaddr gethostbyname gethostent getnetbyaddr getnetbyname getnetent
  getprotoent getservbyname getservbyport getservent
  msgctl msgget msgrcv msgsnd semctl semget semop shmctl shmget shmread shmwrite
  chroot formline ioctl syscall umask waitpid
);

my %DYN_SAFE_WORD;      # built once, on first ask

sub _dyn_safe_words {
  return \%DYN_SAFE_WORD if %DYN_SAFE_WORD;
  require Pl::PExpr::Config;
  my $cfg = Pl::PExpr::Config->new;
  %DYN_SAFE_WORD = map { $_ => 1 } keys %{ $cfg->known_no_of_params };
  $DYN_SAFE_WORD{$_} = 1 for keys %DYN_SAFE_GRAMMAR;
  delete $DYN_SAFE_WORD{$_} for keys %DYN_CALLY_BUILTIN;
  return \%DYN_SAFE_WORD;
}

# Words that make the NEXT word a name being declared or named, not a call.
my %DYN_DECLARATOR = map { $_ => 1 } qw(package sub use no format);

# A bareword that is not a call: a sole hash-subscript key, a fat-comma left
# side, the NAME in a declarator.  (Getting one of these wrong only costs a
# frame, so the test stays the cheap shapes and does not reach for PExpr's
# full classifier, which needs an environment this predicate has not got.)
sub _dyn_word_autoquotes {
  my ($t) = @_;
  my $pv = $t->sprevious_sibling;
  return 1 if $pv && $pv->isa('PPI::Token::Word') && $DYN_DECLARATOR{$pv->content};
  my $nx = $t->snext_sibling;
  return 1 if $nx && $nx->isa('PPI::Token::Operator') && $nx->content eq '=>';
  # PPI puts a subscript's contents either directly in the Structure or in a
  # Statement::Expression inside it, depending on the key's shape — accept
  # both, and answer YES only when the word is the WHOLE key.
  my $parent = $t->parent           or return 0;
  $parent = $parent->parent
    if $parent->isa('PPI::Statement') && $parent->schildren == 1;
  return 0 unless $parent && $parent->isa('PPI::Structure::Subscript');
  my @sib = $parent->schildren;
  @sib = $sib[0]->schildren if @sib == 1 && !$sib[0]->isa('PPI::Token');
  return @sib == 1 && $sib[0] == $t ? 1 : 0;
}

sub calls_user_code {
  my ($elems) = @_;
  my $safe = _dyn_safe_words();
  for my $el (@$elems) {
    next unless ref($el) && Scalar::Util::blessed($el);
    for my $t ($el->isa('PPI::Token') ? ($el) : $el->tokens) {
      # A code-ref call, a method call, an indirect sub call: `$c->()`,
      # `$o->m`, `Foo->new`, `&foo()`, `&$c()`, `&{$c}()`.
      return 1 if $t->isa('PPI::Token::Operator') && $t->content eq '->';
      return 1 if $t->isa('PPI::Token::Cast')     && $t->content eq '&';
      return 1 if $t->isa('PPI::Token::Symbol')   && $t->content =~ /\A&/;
      # Code hidden INSIDE one token: `s/x/f()/e` (the replacement is perl),
      # a regex with `(?{…})`, and any interpolation with a `${…}`/`@{…}`
      # BLOCK in it (`"@{[ f() ]}"`).  The token stream shows none of these.
      if ($t->isa('PPI::Token::Regexp') || $t->isa('PPI::Token::Quote')
          || $t->isa('PPI::Token::QuoteLike') || $t->isa('PPI::Token::HereDoc')) {
        # NB a HereDoc's BODY is `{_heredoc}` (an arrayref of lines); its
        # `content` is only the `<<TAG` marker, which the rename passes rewrite
        # in `{_heredoc_content}`.  `$t->heredoc` returns a LIST, so reading it
        # through `|| []` yields the COUNT and dies "Can't use string as an
        # ARRAY ref" — the slot is the one thing to read here.
        my $c = $t->isa('PPI::Token::HereDoc')
              ? ($t->{_heredoc_content} // $t->content)
                . join('', @{ $t->{_heredoc} || [] })
              : $t->content;
        # A `${…}`/`@{…}` BLOCK only runs code where the text INTERPOLATES:
        # `'@{[f()]}'`, `q{…}` and a `<<'E'` heredoc are literal (heredoc_is_raw
        # is the one reading of that last question).
        my $literal = $t->isa('PPI::Token::Quote::Single')
                   || $t->isa('PPI::Token::Quote::Literal')
                   || ($t->isa('PPI::Token::HereDoc') && heredoc_is_raw($t));
        return 1 if !$literal && $c =~ /[\$\@]\{/;
        if ($t->isa('PPI::Token::Regexp')) {
          return 1 if $c =~ /\(\?\??\{/;
          # `s/…/EXPR/e` — the REPLACEMENT is perl code, and it is inside this
          # one token, so the walk above cannot see the call in it.
          my %m = $t->get_modifiers;
          return 1 if $m{e};
        }
        next;
      }
      next unless $t->isa('PPI::Token::Word');
      my $n = $t->content;
      unless ($safe->{$n}) {
        next if _dyn_word_autoquotes($t);
        return 1;                       # a user sub, or a word we cannot place
      }
      # `sort $cmp @l` / `map $code, @l`: the comparator/body is a CODE REF IN
      # A SCALAR, which calls user code with no other marker.  The BLOCK
      # spellings need nothing here — their block IS part of this body, so a
      # call inside it is found by this same walk.
      if ($n =~ /\A(?:sort|map|grep)\z/) {
        my $nx = $t->snext_sibling;
        return 1 if $nx && $nx->isa('PPI::Token::Symbol') && $nx->content =~ /\A\$/;
      }
    }
  }
  return 0;
}

# Does site A sit at or before site B?  1 / 0, or undef when the two are not
# comparable (either site unknown, or they come from different documents).
sub site_precedes {
  my ($a, $b) = @_;
  return undef unless $a && $b && defined $a->{doc} && defined $b->{doc};
  return undef unless $a->{doc} == $b->{doc};
  return 1 if $a->{pos}[0] < $b->{pos}[0];
  return 0 if $a->{pos}[0] > $b->{pos}[0];
  return $a->{pos}[1] <= $b->{pos}[1] ? 1 : 0;
}

1;

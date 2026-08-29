# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::PExpr::StringInterpolation;

use v5.20;
use strict;
use warnings;

use Moo;

use PPI;
use PPI::Document;
use Scalar::Util qw(refaddr);

use Pl::InterpScan ();   # THE variable-reference scanner (#237/#388); see
                         # docs/interp-scan.md — this file is consumer 3.

# This module handles parsing of interpolated strings for Pl::PExpr
# It takes a string token and returns a node ID for the parsed result

# Parse a double-quoted string with variable interpolation
# "Hello $name" => string_concat("Hello ", $name)
# "Value: $x" => string_concat("Value: ", $x)
# "Array: @arr" => string_concat("Array: ", join($", @arr))
#
# Parameters:
#   $parser - The Pl::PExpr object (for calling make_node, parse, etc.)
#   $str_token - PPI::Token::Quote::Double token
#
# Returns:
#   $node_id - ID of created node (string_concat or simple node)
# PPI tears a document down RECURSIVELY when it goes out of scope: its DESTROY
# walks the tree and empties every descendant hash, so any token the OpcodeTree
# still points at goes HOLLOW — `content` returns undef and the leaf emitter
# dies ("no form emitter for expression leaf …:?", task #414).  Cloning the
# top-level parts is NOT enough: a clone that is a NODE (the inner `[0]` of
# "$x[$i[0]]", the `{k}` of "$x[$h{k}]") is itself unreferenced the moment the
# caller returns, and takes its own descendants down with it.  So anchor
# everything handed to the parser for this object's lifetime — the same
# `_ppi_docs` anchor the document-parsing sites in this file already use.
sub _anchor {
  my $self = shift;
  push @{ $self->{_ppi_docs} //= [] }, @_;
  return @_;
}

sub parse_interpolated_string {
  my $self      = shift;
  my $parser    = shift;  # Pl::PExpr object
  my $str_token = shift;
  # Optional: the ORIGINAL document token when $str_token is a synthetic
  # Quote::Double built from other source text (heredocs).  Feature scoping
  # (postderef_qq) is looked up from this token's document position; a
  # detached synthetic token without an origin simply gets no features.
  my $origin_tok = shift // $str_token;

  # postderef_qq ("$ref->@*" interpolation) is lexically scoped in Perl;
  # resolve it once per string from the token's enclosing blocks.
  $self->{_postderef_qq} = _postderef_qq_active_for($origin_tok);

  my $content   = $str_token->content();

  say "parse_interpolated_string: Input: $content" if $parser->DEBUG & 32;

  # Remove surrounding quotes/delimiters.  PPI already knows exactly where a
  # quote-like's delimiters are, so ASK IT (CLAUDE.md 11) — the hand-strip this
  # replaced took the character right after `qq` as the delimiter, which for
  # `qq {…}` is a SPACE: the braces stayed in the value and every such string
  # was silently wrong.  It is what made perl-tests/index.t's eval'd
  # assertions unparseable (`{is (index …, } "…")`), found via #363.
  if ($str_token->can('string')) {
    $content = $str_token->string;
  } elsif (ref($str_token) eq 'PPI::Token::Quote::Interpolate') {
    $content =~ s/^qq(.)//;
    my $open_delim = $1;
    my %pairs = ('{' => '}', '(' => ')', '[' => ']', '<' => '>');
    my $close_delim = $pairs{$open_delim} // $open_delim;
    $content =~ s/\Q$close_delim\E$//;
  } else {
    $content =~ s/^"//;
    $content =~ s/"$//;
  }
  
  my @parts;
  my $pos = 0;

  # Stack for case-changing escapes: each entry is { mode => 'U'|'L'|'Q'|'F', parts => [...] }
  my @case_stack;
  # Target list: either the top of the case stack or @parts
  my $cur_parts = \@parts;
  # Pending single-char transform: 'u' or 'l' (applies to next part only)
  my $pending_char_transform;

  # Process the string, looking for variables and case-changing escapes
  while ($pos < length($content)) {
    # Find next variable, case escape, or end of string.
    # Use \z (absolute end) not $ (which stops before a final \n) so that
    # a literal newline at the end of the string is captured as a literal part.
    if ($content =~ /\G((?:[^\$\@\\]|\\(?:c.?|[^ULulQFEc]))*?)(?:([\$\@])|\\([ULulQFE])|\z)/gc) {
      my $literal = $1;
      my $sigil = $2;
      my $case_cmd = $3;

      # Add literal part if not empty
      if (length($literal) > 0) {
        $literal = $self->unescape_string($literal);
        my $lit_id = $self->make_string_literal_node($parser, $literal);
        if ($pending_char_transform) {
          $lit_id = $self->_wrap_case_func($parser,
            $pending_char_transform eq 'u' ? 'ucfirst' : 'lcfirst', $lit_id);
          $pending_char_transform = undef;
        }
        push @$cur_parts, $lit_id;
      }

      # Handle case-changing escape
      if (defined $case_cmd) {
        if ($case_cmd eq 'E') {
          # Close the current case group
          if (@case_stack) {
            my $group = pop @case_stack;
            $cur_parts = @case_stack ? $case_stack[-1]{parts} : \@parts;
            # Wrap group's parts in the appropriate function
            my $wrapped = $self->_wrap_case_group($parser, $group);
            # A \u/\l that opened before this group (\u\L...\E) applies to the
            # group's output.  A \u/\l still pending here appeared with no
            # following content inside the group; apply it to the output too.
            my $oc = $group->{outer_char} || $pending_char_transform;
            if ($oc) {
              $wrapped = $self->_wrap_case_func($parser,
                $oc eq 'u' ? 'ucfirst' : 'lcfirst', $wrapped);
              $pending_char_transform = undef;
            }
            push @$cur_parts, $wrapped;
          }
          # \E also cancels any pending \u or \l with no content
          $pending_char_transform = undef;
        } elsif ($case_cmd eq 'u' || $case_cmd eq 'l') {
          # A \u/\l at the very START of an open \U/\L group ("\L\u$x") modifies
          # the first character of the group's OUTPUT — ucfirst(lc($x)), not
          # lc(ucfirst($x)) (the group's lc would otherwise override the inner
          # ucfirst).  Record it on the group like the \u\L case above.
          # (A mid-group \u still applies to the next character locally.)
          if (@case_stack && @{$case_stack[-1]{parts}} == 0
              && !$case_stack[-1]{outer_char}) {
            $case_stack[-1]{outer_char} = $case_cmd;
          } else {
            $pending_char_transform = $case_cmd;
          }
        } else {
          # \U, \L, \Q, \F — push a new group.  A pending \u/\l (e.g. the common
          # \u\L idiom) applies to the FIRST CHARACTER OF THE GROUP'S OUTPUT, not
          # to the first element inside it: "\u\L$a" is ucfirst(lc($a)), not
          # lc(ucfirst($a)).  Stash it on the group so it wraps the result when
          # the group closes, and clear it so it doesn't leak onto the contents.
          my $new_parts = [];
          push @case_stack, { mode => $case_cmd, parts => $new_parts,
                              outer_char => $pending_char_transform };
          $pending_char_transform = undef;
          $cur_parts = $new_parts;
        }
        next;
      }

      last unless defined $sigil;

      # Parse the variable starting at current position
      my $var_start = pos($content);
      my ($var_node_id, $new_pos) = $self->parse_interpolated_variable(
          $parser, \$content, $var_start - 1
      );

      if (defined $var_node_id) {
        # postderef_qq: the parsed variable is followed by a postfix deref
        # ("$ref->$*", "$ref->$#*", "$ref->@*", "$ref->@[...]", "$ref->@{...}").
        # Only under the lexical feature — without it the arrow stays literal
        # text (and "@{...}" after it interpolates on its own, matching Perl).
        if ($self->{_postderef_qq}
            && substr($content, $new_pos, 2) eq '->') {
          my ($pd_id, $pd_pos) = $self->_parse_postfix_deref(
              $parser, \$content, $var_start - 1, $new_pos);
          ($var_node_id, $new_pos) = ($pd_id, $pd_pos) if defined $pd_id;
        }
        if ($pending_char_transform) {
          $var_node_id = $self->_wrap_case_func($parser,
            $pending_char_transform eq 'u' ? 'ucfirst' : 'lcfirst', $var_node_id);
          $pending_char_transform = undef;
        }
        push @$cur_parts, $var_node_id;
        pos($content) = $new_pos;
      } else {
        push @$cur_parts, $self->make_string_literal_node($parser, $sigil);
      }
    }
  }

  # Close any unclosed case groups (implicit \E at end of string)
  while (@case_stack) {
    my $group = pop @case_stack;
    $cur_parts = @case_stack ? $case_stack[-1]{parts} : \@parts;
    my $wrapped = $self->_wrap_case_group($parser, $group);
    # Apply a \u/\l that opened before this group (\u\L$a with no closing \E).
    if ($group->{outer_char}) {
      $wrapped = $self->_wrap_case_func($parser,
        $group->{outer_char} eq 'u' ? 'ucfirst' : 'lcfirst', $wrapped);
    }
    push @$cur_parts, $wrapped;
  }
  
  # If no parts, return empty string
  if (@parts == 0) {
    return $self->make_string_literal_node($parser, "");
  }

  # If the whole string is ONE part, the only case we may return bare is a plain
  # string literal (no interpolation happened — e.g. "foo"); that needs neither a
  # join nor a stringify.  Every other single part — a scalar/element/expression,
  # a whole array, or a slice — must still flow through string_concat:
  #   * a scalar "$x" must be STRINGIFIED.  Returning the bare variable node drops
  #     the coercion, which matters when $x is an overloaded object (the "" /
  #     stringify overload never fires) or a reference (no "ARRAY(0x..)" text).
  #     E.g. `$x = "$x"` on a version/overloaded object used to leave $x an
  #     object, so a later overloaded `cmp`/`<=>` re-dispatched forever
  #     (BINDING-STACK-EXHAUSTED).  gen_string_concat emits (p-string-concat $x)
  #     → to-string → box-sv, which fires the "" overload exactly like Perl.
  #   * a whole array "@x" / a slice "@a[1..2]" needs the (p-join |$"| ...) join
  #     AND list context; returning the bare node skipped the join and let the
  #     element inherit the surrounding scalar context.
  if (@parts == 1) {
    my $part_node = $parser->get_a_node($parts[0]);
    return $parts[0] if ref($part_node) eq 'PPI::Token::Quote::Double';
    # Fall through to create string_concat for everything else.
  }

  # Build string_concat node with all parts
  my ($concat_node, $concat_id) = $parser->make_node_insert('string_concat');

  for my $part_id (@parts) {
    $parser->add_child_to_node($concat_id, $part_id);
  }
  
  say "parse_interpolated_string: Created concat node $concat_id with ", 
      scalar(@parts), " parts" if $parser->DEBUG & 32;
  
  return $concat_id;
}


# ── The scanner seam: InterpScan consumer 3 (task #388, s426) ─────────────
# Where a reference STARTS and how far it EXTENDS is `Pl::InterpScan`'s
# answer, never a private walk here (standing rule
# `docs/var-handling-review-s379.md` §8; wiring plan `docs/interp-scan.md`
# step 3).  What stays in this file is the NODE BUILDING and the
# case-mod/literal outer loop; what went away is the hand-rolled scanning —
# the `\G` name grabs, the `$#`/`$::`/punct-magic branches, the six-line
# brace-depth walks and the subscript-group counters.
#
# The port IS the fix for task #420: the old scanner stopped after a DEREF
# spelling and left its subscript as literal string text, so `"$$r[1]"`
# printed `ARRAY(0x…)[1]` where perl prints the element.  The event carries
# the chain, so every deref/braced-expression base now continues into it.
# Task #422 item 1 (`"@{^CAPTURE}"`, which used to DROP the statement) comes
# from the same place: the scanner learned the braced caret name.
#
# Returns: ($node_id, $new_position) or (undef, $old_position) when the text
# at $pos is not a reference and the sigil is literal.
sub parse_interpolated_variable {
  my ($self, $parser, $content_ref, $pos) = @_;
  my $content = $$content_ref;
  my $ev = Pl::InterpScan::scan_one($content, $pos);
  return (undef, $pos) unless $ev;

  say "parse_interpolated_variable: $ev->{sigil} form=$ev->{form} "
    . "name=" . (defined $ev->{name} ? $ev->{name} : '-')
    . " span=$ev->{span}[0]..$ev->{span}[1] chain=" . scalar(@{$ev->{chain}})
      if $parser->DEBUG & 32;

  my ($id, $end) =
      $ev->{sigil} eq '$#' ? $self->_interp_array_index($parser, $content, $ev)
    : $ev->{sigil} eq '$'  ? $self->_interp_scalar($parser, $content, $ev)
    :                        $self->_interp_array($parser, $content, $ev);
  return (undef, $pos) unless defined $id;
  return ($id, $end);
}

# The reference's own source text — what the ordinary expression pipeline
# reads when a shape is easier to re-parse than to re-implement.
sub _ev_src {
  my ($content, $ev) = @_;
  return substr($content, $ev->{span}[0], $ev->{span}[1] - $ev->{span}[0]);
}

# Is this reference written with braces right after the sigil?  ${^NAME} and
# ${1} are the same scanner FORM as $^X and $1 but different PPI tokens.
sub _ev_braced {
  my ($content, $ev) = @_;
  return substr($content, $ev->{span}[0] + 1, 1) eq '{';
}

# A fragment lifted out of a double-quoted construct still carries the ESCAPED
# DELIMITER: `"${\ \"L\"}"` hands the block over as `\ \"L\"`.  Perl undoes that
# one escape when it re-lexes the block, AND NOTHING ELSE — `\\` stays a pair
# and `\t` stays a backslash and a t, because the fragment is CODE and a string
# inside it does its own escape processing.  Probed 5.40.3 (task #521):
#
#   "X${\ \"a\tb\"}Y"      Xa<TAB>Y   (the INNER dq string's \t)
#   "X${\ \"a\\tb\"}Y"     Xa\tbY     (so \\ was NOT unescaped here)
#   "X${\ \"a\\\\b\"}Y"    Xa\\bY
#
# Without this the `"` closed the fragment's string early and the leftovers —
# including the block's own `}` — landed inside it: `s/A/${\ "L"}/` emitted
# `(p-cast-$ (p-backslash (p-backslash "L\"})))`, which SBCL cannot even READ,
# so the whole file died at load.  (The s/// replacement reaches this the same
# way: _gen_interp_replacement wraps it in a manufactured `"…"` token, escaping
# the quotes exactly as a dq token has them.)  The delimiter handled here is
# `"`; `\}` in a `qq{…}` block is the same rule with another character and has
# no case yet.
sub _undelimit {
  my ($src) = @_;
  return $src if index($src, '\\') < 0;
  my $out = '';
  my $i = 0;
  my $n = length $src;
  while ($i < $n) {
    my $c = substr($src, $i, 1);
    if ($c eq '\\' && $i + 1 < $n) {
      my $next = substr($src, $i + 1, 1);
      if ($next eq '"') { $out .= '"'; $i += 2; next }
      $out .= $c . $next;            # every other escape stays a PAIR
      $i += 2;
      next;
    }
    $out .= $c;
    $i++;
  }
  return $out;
}

# Compile one fragment of Perl source through the ordinary expression
# pipeline — the move `_parse_postfix_deref` and ExprToCL's regex consumer
# (`_compile_ref_text_form`) already make.  The fragment is un-escaped first
# (_undelimit).  The document is ANCHORED, not
# cloned: PPI's DESTROY empties every descendant, so the tokens must outlive
# this call (task #414).  Returns a node id, or undef when PPI/PExpr cannot
# read the fragment.
sub _interp_reparse {
  my ($self, $parser, $src) = @_;
  # Lazy: this file is loaded FROM Pl::Parser, so a compile-time
  # `use` would be circular; a runtime require is a %INC lookup once loaded.
  require Pl::Parser;
  my $doc = Pl::Parser::fragment_doc(_undelimit($src));
  return undef unless $doc;
  $self->_anchor($doc);
  my $stmt = $doc->find_first('PPI::Statement');
  return undef unless $stmt;
  my @parts = $stmt->children();
  return undef unless @parts;
  return $parser->parse(\@parts);
}

sub _interp_symbol {
  my ($self, $parser, $text) = @_;
  return $parser->make_node(PPI::Token::Symbol->new($text));
}

# A '@'-sigil reference yields a LIST: codegen joins it with $".
sub _interp_join {
  my ($self, $parser, $child_id) = @_;
  return undef unless defined $child_id;
  my (undef, $interp_id) = $parser->make_node_insert('array_str_interp');
  $parser->add_child_to_node($interp_id, $child_id);
  return $interp_id;
}

# ── $#… ───────────────────────────────────────────────────────────────────
# $#name / $#{name} is the last index of the LEXICAL (or package) @name, not
# a symbolic deref, so the braces are dropped and codegen sees
# (p-array-last-index @name).  $#- / $#+ are the same shape with a
# punctuation name (task #417) — PPI hands the CODE spelling over as a
# single Magic token the emitter has no case for, which is why
# Pl::PExpr::_retag_magic_array_index performs the same retag there.
# $#$ref and $#{EXPR} are derefs: compiled from source.
sub _interp_array_index {
  my ($self, $parser, $content, $ev) = @_;
  my $end = $ev->{span}[1];
  if ($ev->{form} eq 'deref' || $ev->{form} eq 'expr') {
    return ($self->_interp_reparse($parser, _ev_src($content, $ev)), $end);
  }
  my $tok = PPI::Token::ArrayIndex->new('$#' . $ev->{name});
  return ($parser->make_node($tok), $end);
}

# ── $… ────────────────────────────────────────────────────────────────────
sub _interp_scalar {
  my ($self, $parser, $content, $ev) = @_;
  my $end   = $ev->{span}[1];
  my $chain = $ev->{chain};

  # A base the element builder cannot name (a deref or a braced expression),
  # a SECOND subscript group, or an explicit arrow: compile the reference's
  # own source text.  `"$$r[1]"`, `"${$r}[1]"`, `"$h{a}{b}[1]"`,
  # `"$r->[0]{k}"` all land here — one path, and it is the same pipeline the
  # equivalent CODE goes through.
  if ($ev->{form} eq 'deref' || $ev->{form} eq 'expr'
      || @$chain > 1 || (@$chain && $chain->[0]{arrow})) {
    return ($self->_interp_reparse($parser, _ev_src($content, $ev)), $end);
  }
  return $self->_interp_element($parser, $content, $ev) if @$chain;

  # A bare name.  ${^NAME} reaches the parser as Cast+Block, so it goes
  # through the pipeline; every other spelling is one leaf token, and WHICH
  # token PPI would have made is the only thing this dispatch decides.
  my $name   = $ev->{name};
  my $braced = _ev_braced($content, $ev);
  if ($ev->{form} eq 'magic' && $braced && $name !~ /^[0-9]+$/) {
    return ($self->_interp_reparse($parser, _ev_src($content, $ev)), $end);
  }
  # ${1}, ${2} — the numbered capture variables, Magic in PPI (a bare $1 is
  # a Symbol; PCL does not otherwise read a braced ${N} as $N).
  return ($parser->make_node(PPI::Token::Magic->new('$' . $name)), $end)
    if $ev->{form} eq 'magic' && $braced;
  # $! $? $^X $+ … — punctuation and caret magic.  A digit name ($1) and the
  # pid ($$) are Symbols, as PPI spells them.
  return ($parser->make_node(PPI::Token::Magic->new('$' . $name)), $end)
    if $ev->{form} eq 'magic' && $name !~ /^(?:[0-9]+|\$)$/;
  return ($self->_interp_symbol($parser, '$' . $name), $end);
}

# ── @… ────────────────────────────────────────────────────────────────────
sub _interp_array {
  my ($self, $parser, $content, $ev) = @_;
  my $end   = $ev->{span}[1];
  my $chain = $ev->{chain};
  my $form  = $ev->{form};

  # @$r[0,1] / @{$r}[1] / @{$h}{'a','b'} — a slice of a dereferenced
  # container.  Source text through the pipeline, then joined (task #420).
  if (@$chain) {
    return $self->_interp_element($parser, $content, $ev)
      if $form eq 'plain' || $form eq 'magic';
    my $id = $self->_interp_reparse($parser, _ev_src($content, $ev));
    return ($self->_interp_join($parser, $id), $end);
  }

  # @{ EXPR } — "@{[ uc $_ ]}", "@{$ref}", "@{$h->{list}}".  The INNER
  # expression is compiled and the join wraps it, so the emission stays
  # (p-join |$"| (p-cast-@ EXPR)) with exactly one cast.  The guts are
  # un-escaped by _interp_reparse, the ONE place a lifted fragment is read
  # (task #521); this arm used to run the full dq `unescape_string` over them
  # instead, which also turned an inner `\t` into a TAB — a `\\` the block's own
  # string literal was supposed to keep (probed against perl).
  if ($form eq 'expr') {
    my ($gs, $ge) = @{ $ev->{expr_span} };
    my $id = $self->_interp_reparse($parser, substr($content, $gs, $ge - $gs));
    return ($self->_interp_join($parser, $id), $end);
  }

  # @{^CAPTURE} and @{+} / @{-} — a braced caret or punctuation name is the
  # magic ARRAY of that name, never an expression (task #422 item 1).  Both
  # reach the parser as Cast+Block, which Pl::PExpr's pre-pass folds back
  # into the Magic token PPI makes for the bare spelling.
  if ($form eq 'magic' && _ev_braced($content, $ev)) {
    my $id = $self->_interp_reparse($parser, _ev_src($content, $ev));
    return ($self->_interp_join($parser, $id), $end);
  }

  # A bare `@name` is handed over as the Symbol itself: gen_string_concat
  # joins an '@'-sigil Symbol with $" already, and wrapping it would put a
  # second (p-cast-@ @name) in front of every array interpolation there is.
  # An all-DIGIT name comes back as form 'magic' (the scanner's rule, for
  # `$1`), but there is no magic `@1`: `"@119797"` in t/op/sub_lval.t is an
  # ordinary array whose name happens to be digits, and it takes this path.
  return ($self->_interp_symbol($parser, '@' . $ev->{name}), $end)
    if $form eq 'plain'
    || ($form eq 'magic' && $ev->{name} =~ /^[0-9]+\z/
        && !_ev_braced($content, $ev));

  # @{name} is the array @name itself — NOT a symbolic ref and NOT a call to
  # name() — and @- / @+ are the two magic arrays.  @$ref's child is the
  # SCALAR that holds the reference, which is what p-cast-@ wants.
  my $text = ($form eq 'deref' ? '$' : '@') . $ev->{name};
  my $id   = $self->_interp_symbol($parser, $text);
  return ($self->_interp_join($parser, $id), $end);
}

# ── One named container, one bracket group ────────────────────────────────
# $x[i] $x{k} $::x[i] $+{k} $-[i] @x[..] @h{..} @-[i] — the shapes whose
# base is a NAME the accessor node can carry.  A '@' sigil makes it a slice
# (gen_string_concat joins slice nodes with $" the same way it joins a bare
# @array).  Everything else went through _interp_reparse above.
sub _interp_element {
  my ($self, $parser, $content, $ev) = @_;
  my $grp = $ev->{chain}[0];
  my ($gs, $ge) = @{ $grp->{guts_span} };
  (my $guts = substr($content, $gs, $ge - $gs)) =~ s/\A\s+|\s+\z//g;
  my $hash = $grp->{open} eq '{';

  # An empty group is not a subscript: leave the brackets as literal text
  # and stop the reference at the name (what the old scanner did too).
  return ($self->_interp_leaf_of($parser, $ev), $grp->{span}[0])
    if $guts eq '';

  my $key_id = $hash ? $self->_interp_hash_key($parser, $guts)
                     : $self->_interp_reparse($parser, $guts);
  return ($self->_interp_leaf_of($parser, $ev), $grp->{span}[0])
    unless defined $key_id;

  my $type = $ev->{sigil} eq '@' ? ($hash ? 'slice_h_acc' : 'slice_a_acc')
                                 : ($hash ? 'h_acc'       : 'a_acc');
  my (undef, $acc_id) = $parser->make_node_insert($type);
  $parser->add_child_to_node($acc_id,
      $self->_interp_symbol($parser, $ev->{sigil} . $ev->{name}));
  $parser->add_child_to_node($acc_id, $key_id);
  return ($acc_id, $ev->{span}[1]);
}

# The reference read as a plain variable, with its subscript abandoned.
sub _interp_leaf_of {
  my ($self, $parser, $ev) = @_;
  return $self->_interp_symbol($parser, $ev->{sigil} . $ev->{name});
}

# A hash subscript's guts.  A leading `-` is part of a BAREWORD key: perl
# autoquotes `-BAREWORD`, so "$h{-f}" is the key "-f".  Without the `-?` the
# key parsed as an EXPRESSION, and a single-letter one is PPI's filetest
# operator — the interpolation then read `-f $_` and produced the empty
# string (task #234).  The token-side twin of this rule is
# Pl::PExpr::_subscript_autoquote_text; a digit key ("$h{-1}") still parses
# as the expression it is.
sub _interp_hash_key {
  my ($self, $parser, $guts) = @_;
  # The identifier class is perl's own `\w` under `use utf8` — `$h{ｋ}` autoquotes
  # the fullwidth ｋ exactly as `$h{k}` does, and the code-side twin takes any
  # PPI Word.  An ASCII-only head class here sent `"$ｈ{ｋ}"` to the expression
  # path, where the bareword became a CALL to sub ｋ (s425 review probe of
  # #418; pre-existing in the old scanner, which had the same class).
  if ($guts =~ /^-?[^\W\d]\w*$/) {
    my $tok = PPI::Token::Quote::Double->new('"' . $guts . '"');
    $tok->{separator} = '"';
    return $parser->make_node($tok);
  }
  return $self->_interp_reparse($parser, $guts);
}


# Is the postderef_qq feature lexically enabled at $tok's source position?
# Perl scopes `use feature` from the statement to the end of its enclosing
# block, so walk up from the token: at each enclosing Block (and the
# Document), scan the PRECEDING sibling statements for an enable/disable —
# the innermost level with a determination wins.  Enablers: `use feature`
# with postderef_qq or a :5.24+ bundle, `use experimental` with postderef,
# `use v5.24`+.  Disabler: `no feature` naming postderef_qq / :all / bare.
# A detached token (no parent — synthetic strings without an origin) gets 0.
sub _postderef_qq_active_for {
  my ($tok) = @_;
  return 0 unless ref $tok && $tok->can('parent');
  my $node = $tok;
  while (my $parent = $node->parent) {
    if ($parent->isa('PPI::Structure::Block')
        || $parent->isa('PPI::Document')) {
      my $verdict;
      for my $sib ($parent->schildren) {
        last if refaddr($sib) == refaddr($node);
        next unless $sib->isa('PPI::Statement::Include');
        my $c = $sib->content;
        if ($c =~ /^\s*use\s/) {
          $verdict = 1 if $c =~ /\bpostderef(?:_qq)?\b/
            || $c =~ /^\s*use\s+feature\b.*:5\.(\d+)/s && $1 >= 24
            || $c =~ /^\s*use\s+v5\.(\d+)/ && $1 >= 24
            || $c =~ /^\s*use\s+5\.0*(\d+)/ && $1 >= 24;
        } elsif ($c =~ /^\s*no\s+feature\b/) {
          $verdict = 0 if $c =~ /\bpostderef_qq\b/
            || $c =~ /:all\b/
            || $c =~ /^\s*no\s+feature\s*;/;
        }
      }
      return $verdict if defined $verdict;
    }
    $node = $parent;
  }
  return 0;
}

# Parse a postfix dereference following an interpolated variable, under
# postderef_qq: ->$* (scalar), ->$#* (last index), ->@* (whole array),
# ->@[...] (array slice), ->@{...} (hash slice).  %-forms never interpolate.
# The WHOLE expression text (variable + postfix) is re-parsed as code via a
# mini PPI document — the code path already handles every postfix-deref form
# — and list-yielding results are wrapped in array_str_interp so codegen
# joins them with $" exactly like any other array interpolation.
# Returns ($node_id, $new_pos), or (undef, $arrow_pos) to leave the arrow as
# literal text.
sub _parse_postfix_deref {
  my ($self, $parser, $content_ref, $expr_start, $arrow_pos) = @_;
  my $content = $$content_ref;
  pos($content) = $arrow_pos;
  return (undef, $arrow_pos)
    unless $content =~ /\G->(\$\#?\*|\@\*|\@\[|\@\{)/gc;
  my $what = $1;
  my $end  = pos($content);
  if ($what eq '@[' || $what eq '@{') {
    my ($open, $close) = $what eq '@[' ? ('[', ']') : ('{', '}');
    my ($depth, $i) = (1, $end);
    while ($i < length($content) && $depth > 0) {
      my $ch = substr($content, $i, 1);
      $depth++ if $ch eq $open;
      $depth-- if $ch eq $close;
      $i++;
    }
    return (undef, $arrow_pos) if $depth;      # unbalanced: stay literal
    $end = $i;
  }
  my $expr_text = substr($content, $expr_start, $end - $expr_start);
  # Lazy: this file is loaded FROM Pl::Parser, so a compile-time
  # `use` would be circular; a runtime require is a %INC lookup once loaded.
  require Pl::Parser;
  my $doc = Pl::Parser::fragment_doc($expr_text);
  $self->{_ppi_docs} //= [];
  push @{$self->{_ppi_docs}}, $doc;
  my $stmt = $doc && $doc->find_first('PPI::Statement');
  return (undef, $arrow_pos) unless $stmt;
  my @parts   = $stmt->children();
  my $expr_id = $parser->parse(\@parts);
  return (undef, $arrow_pos) unless defined $expr_id;
  if ($what =~ /^\@/) {
    my ($interp_node, $interp_id) = $parser->make_node_insert('array_str_interp');
    $parser->add_child_to_node($interp_id, $expr_id);
    return ($interp_id, $end);
  }
  return ($expr_id, $end);
}


# Create a string literal node
sub make_string_literal_node {
  my $self      = shift;
  my $parser    = shift;
  my $str       = shift;

  # The content is already decoded (actual chars).  Re-encode as a valid
  # Perl double-quoted string literal so convert_perl_string will handle it.
  my $encoded = $str;
  $encoded =~ s/\\/\\\\/g;   # \ -> \\
  $encoded =~ s/"/\\"/g;     # " -> \"
  $encoded =~ s/\n/\\n/g;    # real newline -> \n sequence
  $encoded =~ s/\r/\\r/g;
  $encoded =~ s/\t/\\t/g;
  # Sigils too: a decoded literal `$msg` must round-trip as `\$msg`, or the
  # token is not a faithful dq literal and a downstream consumer that honours
  # interpolation (ExprToCL's dq-string emitter; once ExprToCL2::_string_literal_form) re-interpolates it
  # (closure.t END_MARK heredocs: `\$msg` text lost its `$msg`).
  $encoded =~ s/([\$\@])/\\$1/g;

  # Create a PPI string token
  my $str_token = PPI::Token::Quote::Double->new('"' . $encoded . '"');
  $str_token->{separator} = '"';

  return $parser->make_node($str_token);
}


# Unescape common escape sequences in strings
sub unescape_string {
  my $self      = shift;
  my $str       = shift;

  # Single-pass escape processing (reuses _process_dq_escape from ExprToCL)
  $str =~ s!\\(x\{[^}]*\}|x[0-9A-Fa-f]{1,2}|x|o\{[^}]*\}|N\{[^}]*\}|[0-7]{1,3}|c.|[ntreafd"\\\$\@]|.)!
    Pl::ExprToCL::_process_dq_escape($1)
  !ge;

  return $str;
}


# Wrap a single node in a case-changing function call (ucfirst, lcfirst, etc.)
sub _wrap_case_func {
  my ($self, $parser, $func_name, $node_id) = @_;

  # Create: (pl-ucfirst ...) or (pl-lcfirst ...)
  my $func_token = PPI::Token::Word->new($func_name);
  my ($funcall_node, $funcall_id) = $parser->make_node_insert('funcall');
  my $name_id = $parser->make_node($func_token);
  $parser->add_child_to_node($funcall_id, $name_id);
  $parser->add_child_to_node($funcall_id, $node_id);

  return $funcall_id;
}

# Wrap a case group's parts in the appropriate function
# Group: { mode => 'U'|'L'|'Q'|'F', parts => [...] }
sub _wrap_case_group {
  my ($self, $parser, $group) = @_;

  my $mode = $group->{mode};
  my $parts = $group->{parts};

  # Map mode to function name
  my %mode_func = (
    'U' => 'uc',
    'L' => 'lc',
    'F' => 'fc',
    'Q' => 'quotemeta',
  );
  my $func_name = $mode_func{$mode} // 'uc';

  # If no parts, return empty string
  if (@$parts == 0) {
    return $self->make_string_literal_node($parser, "");
  }

  # Build the content node: single part or string_concat of multiple parts
  my $content_id;
  if (@$parts == 1) {
    $content_id = $parts->[0];
  } else {
    my ($concat_node, $concat_id) = $parser->make_node_insert('string_concat');
    for my $part_id (@$parts) {
      $parser->add_child_to_node($concat_id, $part_id);
    }
    $content_id = $concat_id;
  }

  # Wrap in function call
  return $self->_wrap_case_func($parser, $func_name, $content_id);
}

1;

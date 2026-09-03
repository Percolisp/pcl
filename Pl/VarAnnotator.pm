# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::VarAnnotator;

# VarAnnotator — Gate-1 "may this my-scalar leave the box?" for the v2
# pipeline.  Conservative: any doubt → boxed (byte-identical semantics).
# Full design: docs/type-flow-and-codegen-plan.md §(s)/(e).
#
#   my $vi = Pl::VarAnnotator->analyze(\@stmts, $params, $known_subs, $host);
#   $vi->{'$sum'}{unboxable}   # 1 → emit raw let + setf; writes proven arith
#
# The tree annotator (_analyze_tree) is the ONLY annotator.  It became the
# default at W12 (s276); the s272 name-keyed TEXT-SCAN prototype
# (_analyze_text) survived behind two fallbacks — "the tree walk died" and
# "no $host" — until #303/s393 measured BOTH at ZERO over all three
# populations (111-file corpus, 138-file Pl/t gate, full perl-tests sweep)
# and deleted it; those two paths now die (see analyze).  What stayed is
# _text_gate_tags, which the tree annotator calls on its own per-statement
# parse-failure fallback text.  The PCL_W12_OLD=1 escape hatch went in s277,
# PCL_W12_DIFF's dual-run in s393.
#
#   _analyze_tree — the W12 annotator: per-statement parse_expr_to_tree
#     (the same OpcodeTree ExprToCL consumes) + a structural event walk.
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
#       write-deref-viv  the SCALAR ROOT of an element write through a deref
#                        chain ($r->{A}[0] = …): autovivification writes the
#                        new container back into $r's box.  Every write arm
#                        routes its lvalue through _tw_mark_lvalue, which is
#                        the ONE place that knows a subscript is a READ (#995)
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
# Runtime switch (bring-up history: docs/v2-completion-plan.md §W12):
#   PCL_B_DEBUG=1      = one B-DEBUG line per name with its verdict, coerce
#                        class, strbuf flag and the reasons that boxed it
#                        (the reasons are only STORED in the verdict hash
#                        under this switch — nothing else reads them)
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
use Pl::Passes ();

# Operators whose p-functions return raw CL values (number / string / 1-or-"").
my %ARITH_OP = map { $_ => 1 } qw(+ - * / % ** < > <= >= == != <=>
                                  . eq ne lt gt le ge cmp !);

# Split of %ARITH_OP by the VALUE FAMILY of the op's result: a NUMERIC-valued
# write can never seed magical string increment, so a var whose every write is
# num-family may take root `$x++`/`$x--` statements raw (the A-num regime,
# task #62).  Comparisons and `!` return 1/"" — perl's `""++` is numeric (1),
# so they count as num.  String-family: `.` and the string comparisons.
my %NUM_OP = map { $_ => 1 } qw(+ - * / % ** < > <= >= == != <=> !);

# Coercing compound assigns by stored-value family (see %RAW_COMPOUND below).
# The bitwise trio &= |= ^= is 'str': perl dispatches & | ^ to STRING bitwise
# when both operands are strings, so their result is not provably numeric.
my %NUM_COMPOUND = map { $_ => 1 } qw(+= -= *= /= %= **= <<= >>=);

my %COMPOUND_ASSIGN = map { $_ => 1 }
  Pl::PExpr::TokenUtils::compound_assign_ops();   # #140: the one set

# Compound assigns whose macro COERCES the stored value (the new value is an
# operator result — a raw number or string, never a box), so a statement-root
# `$x OP= RHS;` may store to a RAW slot via the macro's -raw twin
# (docs/raw-numeric-verdict.md, task #62 step 1).  ||= &&= //= are absent:
# they store the RHS value unchanged, which may be/alias a box.  Values are
# the runtime raw-twin macro names; Parser2's native lowering consults this
# same table via raw_compound_macro() — one definition of "raw-storable
# compound op".
my %RAW_COMPOUND = (
  '+='  => 'p-incf-raw',         '-='  => 'p-decf-raw',
  '*='  => 'p-*=-raw',           '/='  => 'p-/=-raw',
  '%='  => 'p-%=-raw',           '**=' => 'p-**=-raw',
  '.='  => 'p-.=-raw',           'x='  => 'p-str-x=-raw',
  '&='  => 'p-bit-and=-raw',     '|='  => 'p-bit-or=-raw',
  '^='  => 'p-bit-xor=-raw',     '<<=' => 'p-<<=-raw',
  '>>=' => 'p->>=-raw',          '&.=' => 'p-str-bit-and=-raw',
  '|.=' => 'p-str-bit-or=-raw',  '^.=' => 'p-str-bit-xor=-raw',
);

sub raw_compound_macro { $RAW_COMPOUND{ $_[0] } }

# --- B-regime use-classification tables (docs/raw-numeric-verdict.md, the
# scan-licensed freeze verdicts, task #62).  WHITELISTS: a read of a plain
# $scalar is classified by the operator/position that consumes it; any read
# not proven by a table is 'opaque' and disqualifies both verdicts.
# TYPE-SENSITIVE ops are deliberately absent — freezing would flip a runtime
# dispatch: `& | ^` (string bitwise when both operands are strings), range
# `..` endpoints (magical string range), unary minus ("-abc" string
# negation), `//` (a DEFINED-ness test — frozen undef becomes defined 0/"").
my %USE_NUM_OP = map { $_ => 1 } qw(+ - * / % ** == != < <= > >= <=> << >>);
my %USE_STR_OP = map { $_ => 1 } qw(. eq ne lt gt le ge cmp);
# Truthiness pass-through: operands of these are truth-TESTED when the whole
# expression sits in boolean context; outside bool context their value
# escapes to the consumer → opaque.
my %USE_BOOL_THROUGH_OP = map { $_ => 1 } qw(&& || and or);

# Builtin funcall arg licensing: name => 'str-all' (every non-filehandle arg
# is a stringify use: print/say/join) or [per-position class] (undef slots =
# opaque).  Args of every OTHER callee are opaque — the value escapes.
# substr appears rvalue-3-arg here; the mutating 4-arg form is already
# vetoed via mutating-builtin-arg.  sprintf/printf license only the FORMAT
# (arg values depend on the format string, which this pass does not read).
my %USE_FN = (
  print   => 'str-all',  say => 'str-all',  join => 'str-all',
  length  => ['str'],    lc  => ['str'],    uc   => ['str'],
  lcfirst => ['str'],    ucfirst => ['str'],
  ord     => ['str'],    hex => ['str'],    oct  => ['str'],
  index   => ['str', 'str', 'num'],
  rindex  => ['str', 'str', 'num'],
  substr  => ['str', 'num', 'num'],
  split   => [undef, 'str', 'num'],
  printf  => ['str'],    sprintf => ['str'],
);

my %MUTATING_FN   = map { $_ => 1 } qw(chomp chop undef read sysread recv);
my %HANDLE_VIV_FN = map { $_ => 1 } qw(open opendir sysopen pipe socket
                                       socketpair accept);
# tie attaches magic to the BOX — a tied variable must stay boxed forever
# (a raw slot would bypass FETCH/STORE).  Found via case-invert-01.t during
# W12 bring-up; the text scan had no tie gate either (live bug on
# `my $x; tie $x, …` — the inline `tie my $x, …` form only worked by luck:
# text step 1 never counted the inline decl, so $x wasn't in vi at all).
my %TIE_FN        = map { $_ => 1 } qw(tie untie);

# ONE membership test for "this builtin WRITES the argument you hand it",
# shared with Parser2's #189 writes_args body scan so the two cannot drift.
sub arg_writing_builtin {
  my ($fname, $nargs) = @_;
  return 0 unless defined $fname;
  return 1 if $MUTATING_FN{$fname} || $HANDLE_VIV_FN{$fname} || $TIE_FN{$fname};
  return 1 if $fname eq 'pos';
  return 1 if $fname eq 'substr' && defined $nargs && $nargs >= 4;
  return 0;
}

# The ONE entry.  Both former fallbacks to the text annotator now DIE (#303,
# s393): the annotator decides whether a name may leave its box, so a
# fallback verdict is a VALUE the emitter consumes — a wrong one is a silent
# miscompile, which is rule 12's die case, not its announce case.  Both arms
# measured ZERO across all three populations (111-file corpus, 138-file Pl/t
# gate, full perl-tests sweep) before the text annotator was deleted.
sub analyze {
  my ($class, $stmts, $extra_params, $known_subs, $host) = @_;
  die "PCL internal: VarAnnotator->analyze called with no host (the text "
    . "annotator it used to fall back to was deleted as unreachable, #303)\n"
    if !$host;
  my $tree_vi = eval { _analyze_tree($stmts, $extra_params, $known_subs, $host) };
  die "PCL internal: VarAnnotator tree walk failed (#303 deleted the text "
    . "fallback as unreachable): " . ($@ || "returned no verdicts\n")
    if !$tree_vi;
  return $tree_vi;
}

# ==========================================================================
# Text annotator (s272 prototype) — kept verbatim during bring-up.
# ==========================================================================

# The ONE list of "what makes a name need a box", published for the callers
# outside this file that ask the same question of a source RUN rather than of
# a region: Parser2's #761 topic gate (is anything in this loop body a write /
# ref / local / alias of `$_`?) and #760's closure-body event test above.
# Same convention as foreach_range_split / raw_compound_macro / arg_writing_builtin.
sub text_gate_tags { return _text_gate_tags(@_) }

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
  push @tags, 'write-compound'                   # #140: the one set
    if $text =~ /$bare\s*${\ Pl::PExpr::TokenUtils::compound_assign_text_re() }(?!=)/;
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
    write_obj      => {},   # a root write whose RHS is an OBJECT (qr//) → never
                            # freeze to text: see the B-regime gate below
    fallback_texts => [],   # statements whose parse died → text gates apply
    known_subs     => $known_subs // {},
    host           => $host,
    seam           => 0,    # inside an expression-embedded block (v1-compiled)
    cond           => 0,    # under an if/unless statement modifier
    has_eval       => 0,
    nested_sub     => {},
    use_class      => {},   # $x => {num|str|bool|opaque => count} (B-regimes)
  };
  for my $p (@{ $extra_params // [] }) {
    $ctx->{decl_count}{$p} = 1;
    # A sub parameter's initial value is CALLER-SUPPLIED — an unknown write
    # family.  Record a non-'num' family so the A-num root-incdec gate never
    # fires on a param: `sub b { my ($pack) = @_; …; $pack++ }` may hold a
    # package-name STRING that perl magically increments ('a' -> 'b'); the
    # numeric -raw twin would numify it (caught via sub.t, s302).
    $ctx->{write_fam}{$p}{caller}++;
  }

  for my $stmt (@stmts) {
    _tw_region_facts($ctx, $stmt);
  }
  _tw_stmts($ctx, \@stmts);

  my %vi;
  # Kind-A gates (Pl::Passes; PCL_OPT): 'raw-numeric' is the B-regime freeze
  # below (PCL_NO_RAW_VERDICT is its alias, resolved inside Pl::Passes);
  # 'raw-slot' is the whole unboxable verdict — off, every name is boxed,
  # which is the general form each name with a reason already takes.
  my $no_b = !Pl::Passes::enabled('raw-numeric')
          || ($host && $host->{_overload_in_file});
  my $no_raw_slot = !Pl::Passes::enabled('raw-slot');
  # #862 ARM A: a `for my $v (LIST)` variable is declared BY THE LOOP, so it
  # is NOT in decl_count — only the sole-RANGE arm registers a declaration
  # there, because that arm gives the name a real storage verdict.  ARM A's
  # question is about the CONTAINER's elements, not this name's slot, so the
  # name gets its region reasons computed here and nothing else: the walk runs
  # over both populations, and a foreach-only name is answered `unboxable => 0`
  # (byte-identical to the absent entry every consumer reads today) plus the
  # read-only key.  decl_count is deliberately NOT bumped: that would add a
  # 'multi-decl' reason to any OUTER `my $v` of the same name and silently
  # de-optimize it.
  my %fe_only = map  { $_ => 1 }
                grep { !$ctx->{decl_count}{$_} }
                keys %{ $ctx->{foreach_my_alias} // {} };
  for my $name (keys %{ $ctx->{decl_count} }, keys %fe_only) {
    my @reasons;
    push @reasons, 'multi-decl'
      if !$fe_only{$name} && $ctx->{decl_count}{$name} != 1;
    push @reasons, 'eval-in-region' if $ctx->{has_eval};
    # #760: the veto is capture PLUS a boxing event in the closure body; with
    # `raw-closure-capture` off, _tw_region_facts sets nested_sub_ev for every
    # captured name, so this reads as the pre-s456af categorical veto.
    push @reasons, 'nested-sub-ref'
      if $ctx->{nested_sub}{$name} && $ctx->{nested_sub_ev}{$name};
    push @reasons, 'write-shape'    if $ctx->{init_bad}{$name};
    push @reasons, 'write-object'   if $ctx->{write_obj}{$name};
    # Root `$x++;` statements are allowed on a raw slot ONLY when every
    # other write is numeric-valued (A-num): a str-family write could seed
    # a string, and perl's ++ on a non-numeric string is the MAGICAL
    # increment ("aa" -> "ab"), which the numeric -raw twin cannot do.
    push @reasons, 'write-incdec-root'
      if $ctx->{incdec_root}{$name}
      && ($ctx->{init_bad}{$name}
          || grep { $_ ne 'num' } keys %{ $ctx->{write_fam}{$name} // {} });
    push @reasons, sort keys %{ $ctx->{ev}{$name} // {} };
    push @reasons, map { "fallback:$_" }
      map { _text_gate_tags($name, $_, 1) } @{ $ctx->{fallback_texts} };

    # B-regime (scan-licensed freeze verdicts, docs/raw-numeric-verdict.md):
    # when the ONLY thing keeping $name boxed is an unproven write shape
    # (write-shape, or its incdec corollary), and every USE licenses one
    # family, go raw anyway — Parser2 wraps each native write in the strict
    # freeze coercer (%pcl-to-number-strict / %pcl-to-string-strict), which
    # dies loudly on overload-capable refs and genuine dualvars.  Boolean
    # context licenses raw-string only (perl truthiness is defined on the
    # STRING form: "0.0"/"00"/" " are true but numify to false).  Sub params
    # are excluded (their initial value is caller-bound, not a wrappable
    # write).  A name inside any parse-failure fallback text has uses this
    # walk never classified → no freeze.
    # (a grep inside a && chain would slurp the rest of the chain into its
    # LIST argument — hoist each grep into its own boolean)
    my $only_liftable
      = !grep { $_ ne 'write-shape' && $_ ne 'write-incdec-root' } @reasons;
    my $in_fallback = grep { /\Q$name\E(?!\w)/ } @{ $ctx->{fallback_texts} };
    if (!$no_b && @reasons && $only_liftable
        && !$ctx->{write_fam}{$name}{caller} && !$in_fallback) {
      my $uc = $ctx->{use_class}{$name};
      my @cls = $uc ? keys %$uc : ();
      my $coerce;
      if (@cls) {
        if    (!grep { $_ ne 'num' } @cls) { $coerce = 'num' }
        elsif (!grep { $_ ne 'str' && $_ ne 'bool' && $_ ne 'strkey' } @cls) {
          $coerce = 'str';    # strkey = stringify use; freeze-safe (simple string)
        }
      }
      if ($coerce) {
        $vi{$name} = { unboxable => 1, coerce => $coerce,
                       ($ENV{PCL_B_DEBUG} ? (reasons => ["b-$coerce"]) : ()) };
        _mark_strbuf($ctx, \%vi, $name);
        next;
      }
    }
    if ($fe_only{$name}) {
      # Declared by the loop itself — the binding is the loop macro's, so
      # there is no storage verdict to give and never a str-buffer.
      $vi{$name} = { unboxable => 0,
                     ($ENV{PCL_B_DEBUG} ? (reasons => \@reasons) : ()) };
    }
    else {
      $vi{$name} = { unboxable => (@reasons ? 0 : 1),
                     ($ENV{PCL_B_DEBUG} ? (reasons => \@reasons) : ()) };
      _mark_strbuf($ctx, \%vi, $name) unless @reasons;
    }

    # #862 ARM A — the READ-ONLY foreach-LIST loop variable (Kind-A
    # `foreach-raw`).  A SEPARATE verdict key, deliberately NOT `unboxable`:
    # the loop variable's own storage question is unchanged (every other
    # consumer keeps seeing unboxable => 0, so the BODY's emission is
    # byte-identical), and this key answers only "may p-foreach skip
    # promoting each element to a box".
    #
    # The licence STARTS from the reason list being exactly the alias event
    # (it does not end there — see the write FACTS below): any write
    # (root/embedded/cond/compound/incdec/list), `\$v`, a regex target,
    # `local`, pos/chomp/open, `arg-to-writer` (#189: a known sub that writes
    # through @_), a nested-sub capture WITH an event, a string eval in the
    # region, a redeclaration, or any parse-failure fallback text mentioning
    # the name — each pushes its own reason and revokes this.  What is left is
    # a variable every one of whose uses is a pure READ, and a read never
    # needs the element's identity.  (The B-regime `next` above cannot reach
    # here, and could not qualify anyway: its licence is a WRITE-shape reason.)
    #
    # THE REASON LIST IS NOT THE WHOLE WRITE STORY, and assuming it was cost
    # this arm its first probe battery: the writes Parser2 lowers NATIVELY —
    # a statement-root `$v = RHS`, a root coercing compound `$v *= 2`, a root
    # `$v++` — are deliberately NOT boxing events (a raw slot stores them
    # fine), so they leave no reason at all.  They are recorded as write
    # FACTS instead, and for a foreach ALIAS every one of them is a write that
    # must reach the container.  `for my $o (@a) { for my $i (@$o) { $i *= 2 } }`
    # was the catch: `$i`'s reasons were exactly [foreach-alias], the arm
    # fired, `(p-*= $i 2)` box-set a raw value and the doubling vanished
    # silently.  So the licence tests the facts too — no write of ANY kind.
    $vi{$name}{foreach_ro} = 1
      if $ctx->{foreach_my_alias}{$name}
      && @reasons == 1 && $reasons[0] eq 'foreach-alias'
      && !$ctx->{write_fam}{$name}       # root `$v = …` / `$v OP= …`
      && !$ctx->{incdec_root}{$name}     # root `$v++` / `$v--`
      && !$ctx->{init_bad}{$name}        # root write of unproven shape
      && !$ctx->{write_obj}{$name}       # root write of an object
      && Pl::Passes::enabled('foreach-raw');
  }
  if ($no_raw_slot) {
    for my $v (values %vi) {
      $v->{unboxable} = 0;
      delete @$v{qw(coerce strbuf)};
      push @{ $v->{reasons} }, 'opt-off:raw-slot' if $ENV{PCL_B_DEBUG};
    }
  }
  if ($ENV{PCL_B_DEBUG}) {
    for my $name (sort keys %vi) {
      warn sprintf "B-DEBUG %s unboxable=%d coerce=%s strbuf=%d reasons=[%s] uses={%s}\n",
        $name, $vi{$name}{unboxable}, $vi{$name}{coerce} // '-',
        $vi{$name}{strbuf} // 0,
        join(',', @{ $vi{$name}{reasons} // [] }),
        join(',', map { "$_=$ctx->{use_class}{$name}{$_}" }
                  sort keys %{ $ctx->{use_class}{$name} // {} });
    }
  }
  return \%vi;
}

# S1 str-buffer verdict (task #62; the W15.8 append fix rides on the raw
# slot): an unboxable slot whose ONLY writes are plain roots and `.=`
# compounds, and whose every use is a TRANSIENT stringify/boolean read,
# holds an adjustable fill-pointer buffer — plain writes wrap in
# %pcl-str-buffer, `.=` becomes in-place %pcl-str-append (O(1) amortized
# instead of O(n) per append).  'strkey' (hash-key) uses are RETAINED by
# the table → excluded; foreach range vars are bound by the loop macro,
# not a buffer init → excluded.
sub _mark_strbuf {
  my ($ctx, $vi, $name) = @_;
  return unless Pl::Passes::enabled('str-buffer');   # Kind-A gate (PCL_OPT)
  return unless $ctx->{write_ops}{$name}{'.='};
  return if grep { $_ ne '.=' } keys %{ $ctx->{write_ops}{$name} };
  return if $ctx->{foreach_var}{$name};
  my $uc = $ctx->{use_class}{$name};
  return unless $uc && %$uc;
  return if grep { $_ ne 'str' && $_ ne 'bool' } keys %$uc;
  $vi->{$name}{strbuf} = 1;
}

# Record one classified USE of a plain $scalar (B-regimes).  undef class =
# 'opaque' (the default for every read the whitelist tables do not prove).
sub _use {
  my ($ctx, $name, $class) = @_;
  return unless defined $name && $name =~ /^\$\w+$/;
  $ctx->{use_class}{$name}{$class // 'opaque'}++;
}

sub _ev {
  my ($ctx, $name, $event) = @_;
  return unless defined $name && $name =~ /^\$\w+$/;
  $ctx->{ev}{$name}{$event}++;
}

# Is a foreach LIST exactly one range expression `A..B` / `A...B`?  Takes the
# TOP-LEVEL PPI token list (structures like parens are single nested elements,
# so scanning the list IS the depth-0 scan).  Returns ([FROM tokens], [TO
# tokens]) or the empty list.  Shared oracle: Parser2 uses it to emit
# p-foreach-range(-raw), THIS module uses it to skip the foreach-alias veto —
# one definition of "the list is one range".
#
# Guard: exactly one top-level `..`/`...` operator, non-empty sides, and NO
# other top-level operator of lower-or-equal precedence — comma/fat-comma (a
# multi-element list), `? :` (range binds tighter than ternary: `1..$x ? 3 :
# 5` is `(1..$x) ? 3 : 5`), and assignments.
#
# Top-level WORDS: a bare list operator (`reverse 1..3`, `sort`, `join`) has
# the LOWEST precedence — it swallows the range (`reverse (1..3)`), so
# splitting at `..` would miscompile.  Named unaries (`length $s`) bind
# tighter and would be safe, but telling the two apart needs the param-spec
# table; a Word is accepted only in the two provably-tight shapes — an
# explicit-paren call `scalar(@a)` (Word followed by a Structure::List) or a
# method name (Word preceded by `->`).  Everything else rejects (including
# and/or/not/xor): a bare-word miss is only a skipped optimization.
my %RANGE_SPLIT_STOP = map { $_ => 1 }
  (',', '=>', '?', ':', '=',
   # #140: every `OP=` from the one set.  The hand-listed copy this replaces
   # omitted `&.= |.= ^.=`, so `for ($y |.= "a" .. 3)` split at the `..` and
   # ran 4 iterations where perl runs ONE (the range is the assignment's RHS).
   Pl::PExpr::TokenUtils::compound_assign_ops());
sub foreach_range_split {
  my ($parts) = @_;
  my @p = @$parts;
  my @at;
  for my $i (0 .. $#p) {
    my $e = $p[$i];
    if (ref($e) eq 'PPI::Token::Operator') {
      my $c = $e->content;
      push @at, $i and next if $c eq '..' || $c eq '...';
      return () if $RANGE_SPLIT_STOP{$c};
    }
    elsif (ref($e) eq 'PPI::Token::Word') {
      my $tight_call = $i < $#p && ref($p[$i + 1]) eq 'PPI::Structure::List';
      my $method     = $i > 0
        && ref($p[$i - 1]) eq 'PPI::Token::Operator'
        && $p[$i - 1]->content eq '->';
      return () unless $tight_call || $method;
    }
  }
  return () unless @at == 1 && $at[0] > 0 && $at[0] < $#p;
  return ([@p[0 .. $at[0] - 1]], [@p[$at[0] + 1 .. $#p]]);
}

# Region facts for one top-level statement (find() descends everywhere,
# including nested sub bodies — same coverage as the text region scan).
sub _tw_region_facts {
  my ($ctx, $stmt) = @_;

  # String-eval reachability: an `eval` WORD token.  Structural, so `eval`
  # inside a comment or string literal no longer boxes the whole region —
  # the W12 flagship win.
  #
  # A BLOCK eval is NOT a boxing event (#758, Kind-A `raw-block-eval`).  The
  # mechanism that needs a cell per name is the eval CAPTURE ALIST (#296-B1),
  # and that is a STRING-eval feature: the runtime resolves a free name in the
  # eval'd source against the alist, so the name must have a box to alias.
  # `eval {…}` is plain control flow — its body is compiled in place (walked
  # with the seam flag by `_tw_expr_parse`, so its writes are counted) and a
  # die unwinding past a raw `let` slot leaves the slot holding its last value,
  # which is exactly perl's `my` retention.  A STRING eval nested inside an
  # eval block still fires: find() descends, and that inner `eval` Word has no
  # Block sibling.  PPI is unambiguous here — `eval {` always lexes the brace
  # run as a Structure::Block, never a hash Constructor (probed s456af over
  # nine spellings).
  my $block_eval_free = Pl::Passes::enabled('raw-block-eval');
  $ctx->{has_eval} = 1
    if @{ $stmt->find(sub {
            return '' unless $_[1]->isa('PPI::Token::Word')
                          && $_[1]->content eq 'eval';
            return 1 unless $block_eval_free;
            my $next = $_[1]->snext_sibling;
            return !($next && $next->isa('PPI::Structure::Block'));
          }) || [] };

  # Names captured by nested anon subs (`sub { … }` blocks): Symbol tokens
  # plus $names inside interpolatable quote-likes ("…", qq, regexes,
  # backticks) — a "$x" in a closure body is a capture.  Comments and
  # single-quoted strings no longer count (text-scan false fires).
  # (A NAMED sub's block has its own name as the previous sibling, so only
  # anon subs are collected here — the named-sub case is the file-promotion
  # story, p-defcell, and is deliberately out of this scan.)
  my $blocks = $stmt->find(sub {
    $_[1]->isa('PPI::Structure::Block') && do {
      my $prev = $_[1]->sprevious_sibling;
      $prev && $prev->isa('PPI::Token::Word') && $prev->content eq 'sub';
    };
  }) || [];
  # #760 (Kind-A `raw-closure-capture`): CAPTURE is not itself a boxing event.
  # A CL closure over a `let` binding shares that binding natively — mutable,
  # kept alive — so a raw slot serves a capture-and-read exactly as a box does,
  # and the per-iteration `let` inside %expand-foreach-range means a closure
  # made in a raw range loop still captures a FRESH binding per iteration
  # (probed: 10/20/30, not 30/30/30).  What a capture cannot serve is a real
  # boxing EVENT inside the closure body — `\$x`, `local`, a write, `tie`, a
  # regex target — because the body lowers through the v1 seam, where every
  # write goes through box-set machinery and needs a box to write into.
  # The oracle for "is there an event" is the SHARED per-name gate list
  # `_text_gate_tags` (the same list the parse-failure fallback uses), run on
  # the closure body's source WITH the bare-write gate: text-shaped, therefore
  # over-firing, which is the safe direction for a veto.
  my $capture_needs_event = Pl::Passes::enabled('raw-closure-capture');
  for my $b (@$blocks) {
    my %cap;
    for my $t ($b->tokens) {
      if ($t->isa('PPI::Token::Symbol')) {
        $cap{$1}++ if $t->content =~ /^(\$\w+)/;
      }
      elsif ($t->isa('PPI::Token::Quote::Double')
          || $t->isa('PPI::Token::Quote::Interpolate')
          || $t->isa('PPI::Token::QuoteLike::Backtick')
          || $t->isa('PPI::Token::QuoteLike::Command')   # qx{…}, #369
          || $t->isa('PPI::Token::QuoteLike::Readline')
          || $t->isa('PPI::Token::QuoteLike::Regexp')
          || $t->isa('PPI::Token::Regexp')
          || $t->isa('PPI::Token::HereDoc')) {
        my $c = $t->content;
        $c .= join '', $t->heredoc if $t->isa('PPI::Token::HereDoc');
        $cap{$1}++ while $c =~ /(\$\w+)/g;
      }
    }
    next unless %cap;
    my $body = $capture_needs_event ? $b->content : undef;
    for my $n (keys %cap) {
      $ctx->{nested_sub}{$n} += $cap{$n};
      $ctx->{nested_sub_ev}{$n} = 1
        if !$capture_needs_event || _text_gate_tags($n, $body, 1);
    }
  }
}

# ---------------------------------------------------------- statement walk

# $uctx: use-class for the statements' root expressions ('bool' inside
# if/while/unless conditions) — consumed by the B-regime read classifier.
sub _tw_stmts {
  my ($ctx, $stmts, $uctx) = @_;
  for my $s (grep { ref $_ && $_->significant } @$stmts) {
    _tw_stmt($ctx, $s, $uctx);
  }
}

sub _tw_stmt {
  my ($ctx, $s, $uctx) = @_;
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
      # foreach loop variable (my or plain) is an ALIAS into the list —
      # EXCEPT `for my $v (A..B)`: a sole-range list has nothing to alias
      # (range elements are fresh values, perl-side read-only), so count it
      # as this region's declaration of the name instead of vetoing.  That
      # lets the counting-loop lowering bind the var RAW
      # (p-foreach-range-raw); every other veto (capture, \$v, local,
      # eval-in-region, multi-decl/shadowing) still applies from the body
      # walk.  Plain (non-my) loop vars are dynamically-scoped globals a
      # callee can see — always the alias veto, never raw.
      my ($var) = grep { $_->isa('PPI::Token::Symbol') } @k;
      if ($var) {
        my $is_my = do {
          my $prev;
          for my $e (@k) { last if $e == $var; $prev = $e }
          $prev && $prev->isa('PPI::Token::Word') && $prev->content eq 'my';
        };
        my ($list) = grep { $_->isa('PPI::Structure::List') } @k;
        my @lp = $list
          ? (map { $_->schildren } grep { $_->isa('PPI::Statement') } $list->children)
          : ();
        if ($is_my && @lp && foreach_range_split(\@lp)) {
          $ctx->{decl_count}{$var->content}++;
          # p-foreach-range(-raw) binds this var itself each iteration —
          # never a str-buffer slot (there is no buffer to append into)
          $ctx->{foreach_var}{$var->content} = 1;
        } else {
          _ev($ctx, $var->content, 'foreach-alias');
          # #862 ARM A (the boxed-aggregates design's §4.4 "proven arm"): a
          # `my` loop variable whose ONLY event is this very alias is read
          # ONLY, so the loop never needs the element's IDENTITY and the
          # elements need not be promoted to boxes.  Record the my-spelling;
          # the verdict loop turns "reasons == exactly [foreach-alias]" into
          # foreach_ro.  A PLAIN (non-my) loop var is excluded here and not
          # merely by the verdict: it is a dynamically-scoped global a callee
          # can read AND write, and the cell arm would then store a raw value
          # where the callee's write needs a box.
          $ctx->{foreach_my_alias}{$var->content}++ if $is_my;
        }
      }
      # A magic-lvalue foreach alias — `for (substr($x,…))` / pos / vec — binds
      # the loop var to a write-through cell (p-substr-lvalue-cell), so a write
      # to the loop var mutates the SCALAR ARG $x.  VarAnnotator otherwise sees
      # no write to $x and would raw-slot it, leaving the cell nothing to write
      # back into.  Veto $x's raw slot (force-box) so the write-through works.
      my ($list2) = grep { $_->isa('PPI::Structure::List') } @k;
      my @lp2 = $list2
        ? (map { $_->schildren } grep { $_->isa('PPI::Statement') } $list2->children)
        : ();
      my @ah = @lp2 ? Pl::Parser::_foreach_alias_rewrite(\@lp2) : ();
      if (@ah && $ah[0] =~ /^p-(?:substr|pos|vec)$/) {
        my ($inner) = grep { $_->isa('PPI::Structure::List') } @lp2;
        my $arg = $inner && $inner->find_first('PPI::Token::Symbol');
        _ev($ctx, $arg->content, 'magic-lvalue-arg')
          if $arg && $arg->content =~ /^\$/;
      }
      # Same veto, one shape over: `for ($x) { $_ = … }` — see
      # _ev_foreach_alias_list.  The statement-MODIFIER spelling of the same
      # loop vetoes from _tw_stmt_expr, through the same helper.
      _ev_foreach_alias_list($ctx, \@lp2);
    }
    for my $k (@k) {
      if ($k->isa('PPI::Structure::Condition')) {
        # if/while/unless/until condition: the root expression is truth-
        # tested — 'bool' use class (licenses raw-string, blocks raw-numeric)
        _tw_stmts($ctx, [$k->schildren], 'bool');
      }
      elsif ($k->isa('PPI::Structure::For')
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
    _tw_stmt_expr($ctx, [$s->schildren], $native_root, $uctx);
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

# `for ($x, $y) { $_ = … }` and its statement-modifier spelling
# `$_ = … for ($x)`: the loop var ALIASES each scalar list operand, so a write
# through it must reach that variable itself.  The list is evaluated once into
# a vector, and only a BOX carries the write back; a raw slot silently dropped
# it (probed s361 against perl: `alias=orig`, perl says `alias=written`).  Veto
# the raw slot of every bare `$name` operand — an element or a deref (`$h{k}`,
# `$$r`) needs nothing here, it already arrives as a live box.  BOTH lowering
# sites build the list the same way (see Pl::Parser::_foreach_single_scalar_p
# for the one-operand wrap), so both must veto the same way.
#
# PAIRED WITH Pl::Parser::_foreach_scalar_elements, the qualifier that decides
# the `(vector …)` wrapper and walks the same list for commas with the SHARED
# #138 splitter.  Two walks in one family is deliberate (ruled s371 §3 — read
# that function's comment too before touching either):
#   (a) On every list that QUALIFIES there — only `,`/`=>` at depth 0, both
#       walkers fed the same _foreach_list_unwrap output — the two walks
#       partition the tokens IDENTICALLY, so a qualifying list can never get
#       the vector + boxes while this veto missed one of its raw `$name` slots.
#   (b) This veto is deliberately a SUPERSET on lists that do NOT qualify: it
#       keeps splitting past `or`/`and`/`xor` (where the qualifier declines the
#       whole list) and vetoes slots the qualifier rejected, because
#       `for ($x, @a)` still aliases `$x` through p-flatten-args.  A superset
#       is the only safe direction for a veto — routing this through the
#       qualifier would silently drop those vetoes.
# A THIRD comma walk in this family reopens the shared-primitive question.
sub _ev_foreach_alias_list {
  my ($ctx, $parts) = @_;
  # The list arrives wrapped differently per spelling: the block form hands
  # over the list's Statement children, the modifier form the parens.  One
  # shared peeler, so this veto and the rewrite it protects can never disagree
  # about what the sole element is (#263).
  my @sig = Pl::Parser::_foreach_list_unwrap($parts);
  # One operand per top-level comma; only a lone bare scalar in a slot counts
  # (anything longer is an expression, whose value is already a fresh box).
  my @slot;
  for my $e (@sig, undef) {
    if (!defined $e
        || ($e->isa('PPI::Token::Operator') && $e->content =~ /^(?:,|=>)$/)) {
      _ev($ctx, $slot[0]->content, 'foreach-alias-list')
        if @slot == 1 && $slot[0]->isa('PPI::Token::Symbol')
        && $slot[0]->content =~ /^\$\w+$/;
      @slot = ();
      next;
    }
    push @slot, $e;
  }
}

sub _tw_stmt_expr {
  my ($ctx, $parts, $native_root, $uctx) = @_;
  my @parts = grep { ref $_ && $_->significant && !_semi($_) } @$parts;
  return unless @parts;

  # Trailing statement modifier: `EXPR if COND` — the native setf path
  # requires !$mod (D12), so root writes under a modifier box (write-cond);
  # while/until/for modifiers lower whole-statement via v1 → same treatment.
  my ($expr, $mod, $cond) = Pl::Parser2::_split_modifier(\@parts);
  if ($mod) {
    _tw_stmt_expr($ctx, $cond, 0, 'bool');
    _ev_foreach_alias_list($ctx, $cond) if $mod =~ /^for(?:each)?$/;
    local $ctx->{cond} = 1;
    _tw_expr_parse($ctx, $expr, 0);
    return;
  }
  # Perl parses `$x = A, B` as `($x = A), B`, so Parser2's native token split
  # DECLINES any statement carrying a depth-0 operator below assignment
  # precedence and hands the whole thing to the generic machinery — where
  # every write is a box-set that cannot store into a raw slot.  Such a write
  # is therefore not a native root for this walk either: ONE predicate, both
  # models (Parser2::_tail_below_assign_prec).
  #
  # Until #77 this agreement held by ACCIDENT, and Parser2's own comment said
  # so: the shape where the two models disagree is the parenless list-op call
  # (`$x = f 1, 2` — PExpr folds the comma into the CALL, so the parse root is
  # the `=` and this walk saw a native root, while the token split saw the
  # comma and rerouted).  A funcall root was never a proven write shape, so
  # the name was boxed anyway.  With `raw-return-family` it can be proven, and
  # the write went to a raw slot through p-my-= — storing nothing at all.
  $native_root = 0
    if $native_root && Pl::Parser2::_tail_below_assign_prec(\@parts);
  _tw_expr_parse($ctx, \@parts, $native_root, $uctx);
}

sub _tw_expr_parse {
  my ($ctx, $parts, $native_root, $uctx) = @_;
  my @parts = grep { ref $_ && $_->significant } @$parts;
  return unless @parts;

  # Expression-embedded blocks (do/map/grep/sort/eval{} …): the analysis
  # parse below leaves their bodies UNCOMPILED (opaque lambda nodes, invisible
  # to the tree walk), and they lower inside the v1 seam, where every write
  # is a box-set.  Walk their statements STRUCTURALLY with the seam flag so all
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

  # ANALYSIS-ONLY parse (PExpr `analysis_only`, Phase B1): the embedded
  # blocks' bodies are not compiled at all — no structural lowering through
  # the `_v2_embed` hook, no v1 text compile — so nothing here can emit or
  # recurse into Parser2, and the parser's emission buckets need no
  # save/redirect/restore.  (Before B1 this parse compiled every block body
  # through v1 into a scratch section and threw the text away: ~900 discarded
  # block compiles per corpus, measured s411.)
  my $p = $ctx->{host}->fallback_parser;
  my $ok = eval {
    # (The `local $SIG{__WARN__} = sub {}` that used to sit here silenced ONE
    # line — PExpr's "Handle single node of unknown type" warn before its
    # decline die.  That warn is gone since task #339, so the workaround left
    # with its cause; an analysis parse that has something real to say should
    # say it.)
    my $expr_o = Pl::PExpr->new(
      e             => \@parts,
      environment   => $ctx->{host}->environment,
      parser        => $p,
      analysis_only => 1,
    );
    my ($root, $decls) = $expr_o->parse_expr_to_tree(\@parts);
    for my $d (@{ $decls // [] }) {
      my $var = $d->{var} // '';
      next unless $var =~ /^\$\w+$/;
      if    ($d->{type} eq 'my')    { $ctx->{decl_count}{$var}++ }
      elsif ($d->{type} eq 'local') { _ev($ctx, $var, 'local') }
      # our/state: package/persistent cells — never raw-let candidates here
    }
    _tw_walk($ctx, $expr_o, $root, $native_root && !$ctx->{seam}, $uctx);
    1;
  };

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

# $uctx (B-regimes): the use class this subtree's VALUE is consumed under —
# 'num' / 'str' / 'bool' / undef (= opaque, the conservative default).  It
# reaches plain-$scalar Symbol leaves via _use; every consuming node sets its
# operands' classes from the whitelist tables (its own incoming $uctx only
# passes through value-transparent wrappers: parens/tree_val, unary +,
# ternary branches, and &&/||-in-bool).
sub _tw_walk {
  my ($ctx, $xo, $id, $root_native, $uctx) = @_;
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
        # each is an lvalue: `chomp $h{$k}` writes the ELEMENT (#995)
        _tw_mark_lvalue($ctx, $xo, $_, $mark) for @args;
      }
      # #189: a KNOWN user sub whose body writes through @_ aliases EVERY
      # argument, exactly as chomp aliases its own — same marking, same
      # mechanism, one more reason.  The fact rides sub_info (writes_args),
      # computed once from the callee's body by Parser2's _sub_writes_args.
      elsif ($fname
             && ref $ctx->{known_subs}{$fname}
             && $ctx->{known_subs}{$fname}{writes_args}) {
        _tw_mark_lvalue($ctx, $xo, $_, 'arg-to-writer')         # #995
          for @$kids[1 .. $#$kids];
      }
      _tw_walk_funcall_args($ctx, $xo, $fname, $kids);
      return;
    }
    if ($t eq '=~') {
      # the match/substitution target is an lvalue: `$h{$k} =~ s///` writes
      # the ELEMENT, the key is a read (#995)
      _tw_mark_lvalue($ctx, $xo, $kids->[0], 'regex-target') if @$kids;
      _tw_walk($ctx, $xo, $_, 0) for @$kids;
      return;
    }
    if ($t eq 'prefix_op' || $t eq 'postfix_op') {
      my ($op_i, $ex_i) = $t eq 'prefix_op' ? (0, 1) : (1, 0);
      my $op = $xo->get_a_node($kids->[$op_i]);
      my $opc = (ref($op) && !$xo->is_internal_node_type($op)) ? $op->content : '';
      if ($opc eq '++' || $opc eq '--') {
        my $ex  = $xo->get_a_node($kids->[$ex_i]);
        my $exk = $xo->get_node_children($kids->[$ex_i]) || [];
        if ($root_native && ref($ex) eq 'PPI::Token::Symbol' && !@$exk
            && $ex->content =~ /^\$\w+$/) {
          # `$x++;` / `++$x;` as its OWN statement (A-num regime, task #62):
          # a numeric ±1 write Parser2 lowers via the -raw twin.  Recorded
          # separately, NOT as a boxing event; the verdict allows it only
          # when every other write to $x is NUMERIC-valued (then magical
          # string increment is unreachable and the twin matches perl).
          # For B-num the wrapped writes guarantee a numeric slot, so the
          # same twin applies — recorded as a 'num' use (which also blocks
          # B-str: ++ on a frozen string cannot magically increment).
          $ctx->{incdec_root}{$ex->content}++;
          _use($ctx, $ex->content, 'num');
          return;  # the incdec IS the bare symbol's use — nothing to walk
        }
        # The write lands on the lvalue ROOT, never on a subscript: `$h{$k}++`
        # writes the ELEMENT and READS the key (#995).
        _tw_mark_lvalue($ctx, $xo, $kids->[$ex_i], 'write-incdec');
        # the operand subtree (`++($x = 5)`, `++$a[$i]`) can hold embedded
        # writes/uses that still need the generic walk
        _tw_walk($ctx, $xo, $kids->[$ex_i], 0);
        return;
      }
      if ($opc eq '\\') {
        # \$h{$k} refs the ELEMENT — the key is a read (#995); every other
        # operand shape still marks the WHOLE subtree, which is what covers
        # \$x and \substr($x,…)/\vec/\pos
        _tw_mark_lvalue($ctx, $xo, $kids->[$ex_i], 'ref-taken');
      }
      # Operand use class: `!`/`not` truth-test → bool; unary `+` is value-
      # transparent → pass through; unary `-` is TYPE-SENSITIVE in perl
      # (string negation: -"abc" eq "-abc") → opaque, like everything else.
      my $child_uctx = ($opc eq '!' || $opc eq 'not') ? 'bool'
                     : $opc eq '+'                    ? $uctx
                     :                                  undef;
      _tw_walk($ctx, $xo, $kids->[$ex_i], 0, $child_uctx) if @$kids > $ex_i;
      return;
    }
    if ($t eq 'string_concat') {
      # interpolation stringifies each part → 'str' use of a part's value
      _tw_walk($ctx, $xo, $_, 0, 'str') for @$kids;
      return;
    }
    if ($t eq 'ternary' && @$kids == 3) {
      _tw_walk($ctx, $xo, $kids->[0], 0, 'bool');
      _tw_walk($ctx, $xo, $_, 0, $uctx) for @$kids[1, 2];   # value pass-through
      return;
    }
    if ($t eq 'h_acc' || $t eq 'a_acc') {
      # element access: the key/index position classifies the key var
      # ($h{$q} → strkey, $a[$q] → num) regardless of where the element's
      # value flows.  Hash keys get their OWN class: a stringify use for the
      # B-verdicts, but the hash RETAINS the key object, so it must block
      # the str-buffer regime (an in-place-mutated buffer as a stored key
      # would corrupt the table).  The base walk stays opaque: a $-Symbol
      # base is either a deref-chain root (must disqualify) or the
      # '$a'-content token of an @a element access (over-conservative
      # pollution of a same-named scalar — safe).
      _tw_walk($ctx, $xo, $kids->[0], 0) if @$kids;
      my $key_uctx = $t eq 'h_acc' ? 'strkey' : 'num';
      _tw_walk($ctx, $xo, $_, 0, $key_uctx) for @$kids[1 .. $#$kids];
      return;
    }
    if ($t eq 'tree_val' && @$kids == 1) {           # parens: value-transparent
      _tw_walk($ctx, $xo, $kids->[0], 0, $uctx);
      return;
    }
    # progn, slices, hash_init/arr_init, methodcall, anon_sub, inline_lambda
    # (list args only — body is opaque body_cl, covered by the seam block
    # walk), readline, glob, backtick, filehandle, func_ref, ref_funcall, …:
    # recurse with opaque use class (their operands' values escape).
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
      if (ref($l) eq 'PPI::Token::Symbol' && !@$lkids
          && $l->content =~ /^\$\w+$/) {
        my $name = $l->content;
        if (!$root_native)     { _ev($ctx, $name, 'write-embedded') }
        elsif ($ctx->{cond})   { _ev($ctx, $name, 'write-cond') }
        my $fam = _tw_shape_ok($ctx, $xo, $kids->[1]);
        if ($fam) { $ctx->{write_fam}{$name}{$fam}++ }
        else      { $ctx->{init_bad}{$name} = 1 }
        $ctx->{write_obj}{$name} = 1 if _tw_rhs_is_object($xo, $kids->[1]);
      }
      else {
        # Everything else — a container element ($h{$k} = …), a deref chain,
        # a list/paren/lvalue-fn target — asks the ONE lvalue-root marker
        # which scalar (if any) the write lands on.  'write-list' is this
        # arm's reason for the shapes that need one (D11: every $scalar
        # inside a list LHS is written by seam machinery).
        _tw_mark_lvalue($ctx, $xo, $kids->[0], 'write-list');
        _tw_walk($ctx, $xo, $kids->[0], 0);
      }
      _tw_walk($ctx, $xo, $kids->[1], 0);
      return;
    }
    if ($COMPOUND_ASSIGN{$op}) {
      # A COERCING compound op (%RAW_COMPOUND) writing a plain $scalar at
      # native statement root stores an operator-coerced raw value, exactly
      # like a root `$x = ARITH;` — Parser2 lowers it via the -raw macro twin
      # (raw_compound_macro), so it is NOT a boxing event.  Every other shape
      # (seam/embedded/modifier position, element or list LHS, ||= &&= //=)
      # still lowers through box-set machinery → veto as before.
      my $l  = $xo->get_a_node($kids->[0]);
      my $lk = $xo->get_node_children($kids->[0]) || [];
      my $raw_ok = $root_native && @$kids == 2 && $RAW_COMPOUND{$op}
        && ref($l) eq 'PPI::Token::Symbol' && !@$lk
        && $l->content =~ /^\$\w+$/;
      if ($raw_ok) {
        $ctx->{write_fam}{$l->content}{ $NUM_COMPOUND{$op} ? 'num' : 'str' }++;
        $ctx->{write_ops}{$l->content}{$op}++;   # str-buffer verdict input
        # The compound also READS the old value — classify the LHS use by
        # the op's coercion family.  The bitwise trio (&= |= ^=, and the
        # .-suffixed string forms) is TYPE-SENSITIVE (& | ^ dispatch on
        # both operands' types) → opaque, blocks both B-verdicts.
        _use($ctx, $l->content,
             $op =~ /^[&|^]/     ? undef
           : $NUM_COMPOUND{$op}  ? 'num'
           :                       'str');
      } else {
        # same lvalue-root rule as `=` and `++`: `$h{$k} .= "x"` writes the
        # element, the key is a read (#995)
        _tw_mark_lvalue($ctx, $xo, $kids->[0], 'write-compound');
        _tw_walk($ctx, $xo, $kids->[0], 0);
      }
      # RHS operand class follows the op's coercion: += … <<= → num;
      # .= → str; x= repeat COUNT → num; bitwise/||=/&&=///= → opaque.
      my $ru = $op =~ /^[&|^]/    ? undef
             : $NUM_COMPOUND{$op} ? 'num'
             : $op eq '.='        ? 'str'
             : $op eq 'x='        ? 'num'
             :                      undef;
      _tw_walk($ctx, $xo, $kids->[1], 0, $ru) if @$kids > 1;
      _tw_walk($ctx, $xo, $_, 0) for @$kids[2 .. $#$kids];   # arity safety
      return;
    }
    if ($op eq '=~' || $op eq '!~') {
      _tw_mark_lvalue($ctx, $xo, $kids->[0], 'regex-target');   # #995
      _tw_walk($ctx, $xo, $_, 0) for @$kids;
      return;
    }
    # Generic operator: classify operands from the whitelist tables.
    # `x` splits (string/list LHS, numeric repeat count RHS); &&/||/and/or
    # pass 'bool' through ONLY in bool context (outside it their operand
    # value escapes to the consumer).  Everything else — `..` endpoints,
    # `& | ^`, `//` (a defined-test), `,` — stays opaque.
    if ($op eq 'x' && @$kids == 2) {
      _tw_walk($ctx, $xo, $kids->[0], 0, 'str');
      _tw_walk($ctx, $xo, $kids->[1], 0, 'num');
      return;
    }
    my $child_uctx = ($USE_NUM_OP{$op} && @$kids >= 2) ? 'num'
                   : $USE_STR_OP{$op}                  ? 'str'
                   : ($USE_BOOL_THROUGH_OP{$op}
                      && ($uctx // '') eq 'bool')      ? 'bool'
                   :                                     undef;
    _tw_walk($ctx, $xo, $_, 0, $child_uctx) for @$kids;
    return;
  }

  # Leaf tokens are reads.  A plain $scalar records its consumer's use class
  # (B-regimes; undef = opaque).  Interpolatable quote-likes hide reads the
  # tree has no Symbol node for (regex patterns, backticks, heredocs) —
  # scanned textually.  A substitution with /e has CODE in the replacement
  # the tree cannot see: apply the text gates to its source.
  if ($r eq 'PPI::Token::Symbol' && !@$kids && $node->content =~ /^\$\w+$/) {
    _use($ctx, $node->content, $uctx);
    return;
  }
  if ($r eq 'PPI::Token::Regexp::Substitute') {
    my %m = $node->get_modifiers;
    push @{ $ctx->{fallback_texts} }, $node->content if $m{e} || $m{ee};
  }
  if (ref($node)
      && ($node->isa('PPI::Token::Regexp')
          || $node->isa('PPI::Token::Quote::Double')
          || $node->isa('PPI::Token::Quote::Interpolate')
          || $node->isa('PPI::Token::QuoteLike::Backtick')
          || $node->isa('PPI::Token::QuoteLike::Command')   # qx{…}, #369
          || $node->isa('PPI::Token::HereDoc'))) {
    _tw_scan_quote_leaf($ctx, $node);
  }
  _tw_walk($ctx, $xo, $_, 0) for @$kids;
}

# Reads hidden inside an interpolatable quote-like LEAF token: interpolation
# is a stringify use ('str') — except a deref form ("$q->[0]", "$q->{k}"):
# there the ELEMENT is interpolated and $q's own use is a dereference →
# opaque.  "$q[0]"/"$q{k}" (element of @q/%q) over-fires on the same-named
# SCALAR as opaque — over-conservative, safe.  Escaped \$q also over-fires
# as a str use — same direction.  ${name} spelled with braces is caught.
sub _tw_scan_quote_leaf {
  my ($ctx, $node) = @_;
  my $c = $node->content;
  $c .= join '', $node->heredoc if $node->isa('PPI::Token::HereDoc');
  while ($c =~ /\$\{?(\w+)\}?((?:->)?[\[\{])?/g) {
    _use($ctx, '$' . $1, $2 ? undef : 'str');
  }
}

# Classified walk of funcall ARG subtrees (B-regimes): builtins in %USE_FN
# get their per-position class; args of every other callee — including KNOWN
# user subs — are opaque (the value escapes into the callee).  'filehandle'
# nodes (print $fh …) never consume an arg position and their innards are
# opaque (a handle-carrying scalar must stay boxed).
sub _tw_walk_funcall_args {
  my ($ctx, $xo, $fname, $kids) = @_;
  my $spec = $USE_FN{$fname};
  my $argi = 0;
  for my $kid (@$kids[1 .. $#$kids]) {
    my $n = $xo->get_a_node($kid);
    if ($xo->is_internal_node_type($n) && ($n->{type} // '') eq 'filehandle') {
      _tw_walk($ctx, $xo, $kid, 0);
      next;
    }
    my $class = !$spec     ? undef
              : !ref $spec ? 'str'          # 'str-all'
              :              $spec->[$argi];
    _tw_walk($ctx, $xo, $kid, 0, $class);
    $argi++;
  }
}

# The element/slice access node types.  Every one of them has the shape
# [BASE, SUBSCRIPT...]: the base is the container (or the chain below it) and
# everything after it is a subscript expression.
my %ACCESS_NODE = map { $_ => 1 } qw(h_acc a_acc h_ref_acc a_ref_acc
                                     slice_h_acc slice_a_acc
                                     kv_slice_h_acc kv_slice_a_acc);

# THE lvalue-root marker (#995) — the ONE place that decides which scalar a
# write to a non-plain-scalar lvalue lands on.  Every write arm of the walk
# asks it: `=`, `++`/`--`, the compound assigns, `=~`/`!~`, the mutating
# builtins (chomp/read/tie/pos/4-arg substr/open…), an argument to a known
# @_-writing sub, and `\`.  Before s465bd only `=` had the rule and the other
# arms _tw_mark-ed the WHOLE operand subtree, so `$h{$k}++`, `$h{$k} .= "x"`
# and `chomp $h{$k}` all recorded a WRITE against the hash KEY (and `$a[$i]++`
# against the index) — boxing a variable perl only reads.  That single false
# positive was 15.4 % %make-p-box + 8.1 % box-set of the arrhash bench row.
#
# The rule, which is just perl's: a write to a container element writes the
# ELEMENT, so every subscript on the way down is an ordinary READ (the
# caller's own generic walk classifies them: h_acc key → strkey, a_acc index
# → num).  Only the ROOT of the access chain can be a scalar the write
# reaches, and then only by autovivification:
#   %h / @a plain container root  → NO scalar is written.  ($h{$k}{$j} = 1
#     vivifies into %h's own element box, never into a lexical slot.)
#   $r scalar root ($r->{A}[0], $$r{k}, @{$r}{…})  → the vivified container is
#     written BACK into $r's box, so $r must stay boxed: an unboxed root made
#     every deref re-vivify a fresh hash (exists_sub.t t13) — 'write-deref-viv'.
#   any other root (a funcall, a paren list, `++($x = 5)`, a plain $x, $$r,
#     substr($x,…))  → the whole subtree, with the caller's own reason.
# Marks only; the caller walks the subtree for reads exactly as before.
sub _tw_mark_lvalue {
  my ($ctx, $xo, $id, $event) = @_;
  return if !defined $id;
  my $n = $xo->get_a_node($id);
  my $t = $xo->is_internal_node_type($n) // '';
  my $descended = 0;
  while ($ACCESS_NODE{$t}) {
    my $base = ($xo->get_node_children($id) || [])->[0];
    return if !defined $base;          # subscript-less access: nothing to mark
    $descended = 1;
    $id = $base;
    $n  = $xo->get_a_node($id);
    $t  = $xo->is_internal_node_type($n) // '';
  }
  return if $descended
    && ref($n) && !$xo->is_internal_node_type($n)
    && $n->isa('PPI::Token::Symbol') && $n->symbol =~ /^[\@\%]/;
  _tw_mark($ctx, $xo, $id, $descended ? 'write-deref-viv' : $event);
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
# Returns the stored value's FAMILY — 'num' (numeric-op result / number
# literal) or 'str' (string-op result / quote literal / interpolation) — or
# 0 when the shape is unproven (may alias a box).  Truthiness is the old
# ok/not-ok verdict; the family feeds the A-num root-incdec gate.
# True when a root write's RHS produces an OBJECT whose string form is lossy —
# today exactly `qr//`.  A Regexp carries flags and identity that its
# stringification `(?^flags:…)` cannot round-trip: perl's rule is that a lone
# interpolated qr IS that regex (`qr/$re/` keeps $re's own flags and ignores the
# outer ones), which only holds while the value is still the object.  Freezing
# such a variable to text at the write site turns every later use into a
# re-parse of the wrapper, which is how `qr/$re/` came out double-wrapped and a
# /xx pattern silently reverted to /x (task #181).  This is NOT the same as an
# unproven shape: `write-shape` alone is liftable by the B regime, this is not.
sub _tw_rhs_is_object {
  my ($xo, $id) = @_;
  my $node = $xo->get_a_node($id);
  my $kids = $xo->get_node_children($id) || [];
  if ($xo->is_internal_node_type($node)) {
    return (($node->{type} // '') eq 'tree_val' && @$kids == 1)
      ? _tw_rhs_is_object($xo, $kids->[0]) : 0;
  }
  return ref($node) eq 'PPI::Token::QuoteLike::Regexp' ? 1 : 0;
}

# #759 (Kind-A `raw-op-family`): under an %ARITH_OP root the family is decided
# by the OPERATOR, not by its operands.  That is this file's own stated
# invariant (the header above: "every such p-op coerces its operands — boxes,
# strings, sub results — and returns a raw CL number or string"), and the
# `_tw_operand_ok` walk below contradicts it: it rejected a `PPI::Token::Magic`
# operand and an unknown-sub call, so `$s = $s + $_` was an unproven
# write-shape while `$s += $_` — the same value through the same coercion —
# went raw.  Three spellings of one sum, three verdicts (probed s453/s456af).
#
# What the operand walk still owns is the NO-operator case, and that is the
# real hazard the check was written for: a bare `$y` RHS stores $y's BOX, so
# the slot becomes an alias.  An operator root cannot do that — `p-+` builds a
# fresh CL number.  Overload does not add a class here: a plain `$scalar`
# operand was ALREADY accepted and may hold an overloaded object, so
# `$a + f()` is exactly as exposed as `$a + $b` was before (probed).
sub _op_family_by_operator { return Pl::Passes::enabled('raw-op-family') }

# Task #77 (Kind-A `raw-return-family`), the callee→caller half.  A funcall
# ROOT is unproven because a sub can hand back a BOX — its own lexical's, a
# global's — and storing that in a raw slot would make the slot an ALIAS.  But
# when every value a named sub can return is itself operator-coerced or a
# literal, its result is a fresh raw CL value by the same argument that
# licenses `$x = $a + $b`: the ONE proof, applied to the sub's returns instead
# of to this RHS.  Parser2's sub_info pre-pass records the verdict as
# `returns => 'num'/'str'` (Parser2::_sub_return_family); nothing is recorded
# unless every return is proven and they agree, so a present key IS the proof.
#
# NO new soundness assumption over the direct-call facts already on sub_info
# (`insensitive`, `writes_args`): same closed world, same table, same
# invalidation.  What it does NOT cover is a method / coderef / AUTOLOAD call —
# those are not funcall-with-a-Word roots and never reach here.
sub _tw_known_sub_return_family {
  my ($ctx, $xo, $node, $kids) = @_;
  return 0 unless ($node->{type} // '') eq 'funcall' && @$kids;
  return 0 unless Pl::Passes::enabled('raw-return-family');
  my $f = $xo->get_a_node($kids->[0]);
  return 0 unless ref($f) eq 'PPI::Token::Word';
  my $info = $ctx->{known_subs}{ $f->content };
  return 0 unless ref $info;
  return $info->{returns} || 0;
}

sub _tw_shape_ok {
  my ($ctx, $xo, $id) = @_;
  my $node = $xo->get_a_node($id);
  my $kids = $xo->get_node_children($id) || [];
  my $by_op = _op_family_by_operator();

  if ($xo->is_internal_node_type($node)) {
    my $t = $node->{type} // '';
    return _tw_shape_ok($ctx, $xo, $kids->[0]) if $t eq 'tree_val' && @$kids == 1;
    return 'str' if $t eq 'string_concat';
    if ($t eq 'prefix_op' && @$kids == 2) {          # -$y / +$y / !$y roots
      my $op = $xo->get_a_node($kids->[0]);
      my $opc = (ref($op) && !$xo->is_internal_node_type($op)) ? $op->content : '';
      # perl's unary PLUS is a NO-OP — `+$y` IS `$y`, and `+{…}` is the
      # anon-hash constructor with a disambiguator in front.  It computes
      # nothing, so it cannot make a raw value: it is VALUE-TRANSPARENT and
      # takes its operand's shape, exactly like the parens arm above.  Calling
      # it 'num' with the other two was a SILENT WRONG of the bare-`$y` kind
      # this oracle exists to reject — `my $b = +$h; $h = 77;` stored $h's BOX
      # and then read 77 where perl reads the old value (probed s461aq, wrong
      # on HEAD too; found extending this oracle for #77, which would have
      # transferred it through every `return +$y`).
      return _tw_shape_ok($ctx, $xo, $kids->[1]) if $opc eq '+';
      # `-` and `!` DO compute: p--/p-! return a fresh raw value.  (Perl's
      # unary minus on a non-numeric string yields a STRING — "-foo" — so the
      # 'num' LABEL is imprecise there, but the value is raw either way, which
      # is what the write proof is about; task #921 has the residue.)
      return (($by_op || _tw_operand_ok($ctx, $xo, $kids->[1])) ? 'num' : 0)
        if $opc eq '-' || $opc eq '!';
    }
    # funcall/h_acc/… root: the value may BE or alias a box — except a call to
    # a known sub whose returns are all proven (#77).
    return _tw_known_sub_return_family($ctx, $xo, $node, $kids);
  }
  my $r = ref $node;
  if ($r eq 'PPI::Token::Operator' && @$kids) {
    return 0 unless $ARITH_OP{$node->content};
    if (!$by_op) {
      for my $k (@$kids) {
        return 0 unless _tw_operand_ok($ctx, $xo, $k);
      }
    }
    return $NUM_OP{$node->content} ? 'num' : 'str';
  }
  return 'num' if ref($node) && $node->isa('PPI::Token::Number');
  return 'str' if $r eq 'PPI::Token::Quote::Single'
               || $r eq 'PPI::Token::Quote::Double';
  return 0;                 # bare $y / f() / anything else: may alias a box
}

# The family oracle as a NAMED SEAM for a caller outside this file (task #77):
# Parser2's sub_info pre-pass asks it of every `return` expression, so that the
# proof licensing `my $x = $a + $b` and the proof licensing `my $x = f()` are
# THE SAME proof — the alternative was a second family table in Parser2, which
# is exactly the drift rule 11 forbids.  KNOWN_SUBS is the call table
# _tw_operand_ok consults; a caller with none passes {}.  Returns 'num' / 'str'
# / undef (never 0), because the fact it feeds is an ABSENT-or-proven key.
sub value_family {
  my ($known_subs, $xo, $id) = @_;
  return _tw_shape_ok({ known_subs => $known_subs // {} }, $xo, $id) || undef;
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

1;

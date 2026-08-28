# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::InterpScan;

# InterpScan — THE variable-reference event scanner for interpolating text
# (task #237, shape ruled `docs/fable-answers-s378.md` §3 = (b′); design
# `docs/var-handling-review-s379.md` §4).  One scanner answers "where does
# this text mention a variable, and how far does the reference extend?" for
# every consumer: the dq-string/heredoc parser (StringInterpolation.pm), the
# regex-pattern interpolator (ExprToCL::_gen_interp_regex_pattern — whose
# private walk this scanner exists to DELETE), and the rename/requalify
# machinery (_interp_canon / _interp_names / _interp_fixer, %canon_pat,
# VarAnnotator's quote scans).  Per the s379 standing rule (§8): new
# interpolation behavior is implemented HERE or not at all.
#
# The grammar is perl's own tokenizer (toke.c, read from the 5.40.3 source):
# scan_const's reference-start rules, scan_ident's name forms, and — the
# #237 heart — S_intuit_more + Perl_regcurly deciding whether a bracket
# group after a variable in a PATTERN is a subscript or regex syntax.
# `intuit_more`/`regcurly` below are line-faithful ports and share their
# names with toke.c/regcomp.c for greppability.  Where StringInterpolation.pm
# is narrower than perl, the scanner follows PERL; the divergence table in
# `docs/interp-scan.md` names every such spot for the consumer-wiring
# sessions (probe-and-guard per entry).  Verified against live perl by the
# probe table in Pl/t/interp-scan-01.t.
#
# ── The event ──────────────────────────────────────────────────────────────
# scan()/scan_one() return hashrefs:
#   sigil     '$' | '@' | '$#'
#   form      'plain'  $x, @x, $Foo::bar, $::x        (name = identifier)
#             'braced' ${x}, @{x}, $#{x}, ${ x }      (identifier in braces)
#             'magic'  $1, $!, $^W, ${^NAME}, @{^NAME}, $$, @-, $#-
#                                                    (non-identifier name)
#             'deref'  $$x, @$x, $#$x                 (name = the ref SCALAR)
#             'expr'   ${ EXPR }, @{ EXPR }, $#{EXPR} (no name; see expr_span)
#             A braced NAME of any kind ('braced' and the braced 'magic'
#             spellings) CLOSES the reference; a braced EXPRESSION does not
#             — it takes a chain like `$name[` does.  Both probed; see
#             _scan_braced_dollar.
#   name      the bare name ('x', 'Foo::bar', '::x', '1', '!', '^W', '$');
#             undef for form 'expr'
#   canon     the container the reference READS, in source spelling:
#             '$x' | '@x' | '%x' ($x[i]→@x, $x{k}→%x, $#x→@x, @x{..}→%x,
#             $x->[i]→$x, $$r→$r); undef for magic/expr and all-digit names.
#             This is the join key for the rename machinery.
#   span      [start, end)  the whole reference: sigil through last chain
#             group / postderef.  end is where scanning resumes.
#   name_span [start, end)  the bare name only — the splice target for a
#             rename; undef for form 'expr'
#   expr_span [start, end)  form 'expr'/braced-$#: the text inside the braces
#   slice     1 when an '@'-sigil reference took a subscript group
#   chain     [ { open=>'['|'{', arrow=>0|1, span=>[s,e), guts_span=>[s,e) },
#               ... ]  subscript groups, in order (slices: at most one)
#   postderef undef | { what=>'$*'|'$#*'|'@*'|'@['|'@{', span=>[s,e),
#                       guts_span=>[s,e)|undef }
#
# ── Options (both entry points) ────────────────────────────────────────────
#   in_regex     => 0|1   pattern context: perl's start rules ($ before
#                         `()| \r\n\t`/end is an anchor; @ only before
#                         \w : { $; @+/@- never interpolate) and the
#                         intuit_more classifier on the FIRST bracket group
#   postderef_qq => 0|1   accept ->$* ->$#* ->@* ->@[..] ->@{..}
#   known_name   => sub($name)->bool   the weigher's gv_fetchpvn_flags hook:
#                         "is NAME a package glob already known?"  Default:
#                         always false (the more subscript-leaning answer is
#                         -100 vs -10, so false only ever leans toward
#                         charclass, matching perl on an empty stash).
#
# Deliberate v1 non-goals (regex mode), each recorded in docs/interp-scan.md:
# (?#..) comments, //x #-comments, (?{..}) code blocks, charclass state, and
# the removed-in-5.38 $pkg'var spelling are scanned as ordinary text.

use v5.20;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(scan scan_one intuit_more regcurly);

# perl 5.40.3's always-on keyword set (keywords.h minus the 19
# feature-gated entries keywords.c guards with FEATURE_*_IS_ENABLED —
# say/fc/isa/try/... — which Perl_keyword(name, len, FALSE) reports as
# non-keywords when the feature is off, the state intuit_more runs in for
# plain code).  Consulted only by the weigher's "two alphas spell a
# keyword" rule.
my %KEYWORDS = map { $_ => 1 } qw(
  AUTOLOAD BEGIN CHECK DESTROY END INIT UNITCHECK __DATA__ __END__ __FILE__
  __LINE__ __PACKAGE__ abs accept alarm and atan2 bind binmode bless caller
  chdir chmod chomp chop chown chr chroot close closedir cmp connect
  continue cos crypt dbmclose dbmopen defined delete die do dump each else
  elsif endgrent endhostent endnetent endprotoent endpwent endservent eof
  eq eval exec exists exit exp fcntl fileno flock for foreach fork format
  formline ge getc getgrent getgrgid getgrnam gethostbyaddr gethostbyname
  gethostent getlogin getnetbyaddr getnetbyname getnetent getpeername
  getpgrp getppid getpriority getprotobyname getprotobynumber getprotoent
  getpwent getpwnam getpwuid getservbyname getservbyport getservent
  getsockname getsockopt glob gmtime goto grep gt hex if index int ioctl
  join keys kill last lc lcfirst le length link listen local localtime lock
  log lstat lt m map mkdir msgctl msgget msgrcv msgsnd my ne next no not
  oct open opendir or ord our pack package pipe pop pos print printf
  prototype push q qq qr quotemeta qw qx rand read readdir readline
  readlink readpipe recv redo ref rename require reset return reverse
  rewinddir rindex rmdir s scalar seek seekdir select semctl semget semop
  send setgrent sethostent setnetent setpgrp setpriority setprotoent
  setpwent setservent setsockopt shift shmctl shmget shmread shmwrite
  shutdown sin sleep socket socketpair sort splice split sprintf sqrt srand
  stat study sub substr symlink syscall sysopen sysread sysseek system
  syswrite tell telldir tie tied time times tr truncate uc ucfirst umask
  undef unless unlink unpack unshift untie until use utime values vec wait
  waitpid wantarray warn while write x xor y
);

# The single-punctuation magic scalars ($! $? $. ... $"), exactly
# StringInterpolation.pm's set.  In regex mode ( ) | never reach this test
# (vetoed at the reference start, as perl's scan_const does).
my $PUNCT_MAGIC = '!?.@/\\&\'`+;,|:%=-<>()[]~"';

# The punctuation names whose `[` starts a SUBSCRIPT of the punctuation ARRAY,
# rather than ending the reference (task #451).  perl's rule is the one that
# makes `$-[0]` and `$+[0]` element-of-@-/@+ — it is not special to those two,
# and `@?`/`@!`/`@.` … subscript the same way.  Probed one character at a time
# against perl 5.40.3: `@X = (11,22,33); print "$X[1]"` gives 22 for every
# character below, in the string AND in code.
#
# `^` IS DELIBERATELY ABSENT, and it is the reason this is its own set rather
# than a reference to Pl::Parser's %PUNCT_ARRAY_CHARS.  That set is about the
# CL SYMBOL SPELLING (which characters can be emitted bare), and `@^` is a
# perfectly good array; this one is about perl's SUBSCRIPT rule, and `"$^[1]"`
# does NOT read as an element — `$^` is the format-top-of-page name and the
# `[` after it is not a subscript (probed).  Two sets, two questions.
#
# The CL-unsafe punctuation names (`@,` `@;` `@|` `@'` `@"` …) were absent for
# the reason task #449 recorded: they needed a pipe-quoted emission that did
# not exist.  IT EXISTS SINCE s449s (`Pl::CLForm::needs_pipes` pipes a name
# carrying a CL-unsafe character), so that reason is GONE — but the set is not
# widened here, because this is the SUBSCRIPT question and it has its own
# answer per character.  Measured s449s: `@, = (11,22,33); print "$,[1]"` is
# 22 in perl and the literal `[1]` in PCL for `,`, `|`, `@` and `\` — four
# silent wrongs, and the `\` one needs the escape rule thought through (this
# scanner hides the character after a backslash from reference detection, while
# perl consumes the `\` into the variable NAME first).  Task #653.
my $PUNCT_ARRAY_SUBSCRIPT = '?!./~&%=<>';

# ── Whole-text driver ──────────────────────────────────────────────────────
# Walk $text emitting an event per variable reference.  Escape skipping
# mirrors the tokenizer: a backslash hides the next char from reference
# detection; in dq text \cX also hides the X (toke's string-mode \c eats
# its char), while in a PATTERN \c passes through and the char after it can
# still start an interpolation (scan_const's inpat branch — probed:
# qr/\c$x/ interpolates $x).
sub scan {
  my ($text, %opt) = @_;
  my @events;
  my ($i, $n) = (0, length $text);
  while ($i < $n) {
    my $c = substr($text, $i, 1);
    if ($c eq '\\') {
      $i += (!$opt{in_regex} && substr($text, $i + 1, 1) eq 'c') ? 3 : 2;
      next;
    }
    if ($c eq '$' || $c eq '@') {
      my $ev = scan_one($text, $i, %opt);
      if ($ev) { push @events, $ev; $i = $ev->{span}[1]; next }
    }
    $i++;
  }
  return \@events;
}

# ── Single-reference entry (for callers that run their own outer loop,
#    e.g. StringInterpolation's literal/case-mod walk) ─────────────────────
# $pos must be at the sigil.  Returns the event, or undef when the text at
# $pos is not a variable reference (caller treats the sigil as literal).
sub scan_one {
  my ($text, $pos, %opt) = @_;
  my $n = length $text;
  my $sigil = substr($text, $pos, 1);
  return undef unless $sigil eq '$' || $sigil eq '@';
  my $next = $pos + 1 < $n ? substr($text, $pos + 1, 1) : '';
  return undef if $next eq '';
  if ($opt{in_regex}) {
    if ($sigil eq '$') {
      # "$ might be tail anchor": before ( ) | whitespace or at pattern end
      return undef if index("()| \r\n\t", $next) >= 0;
    } else {
      # @ interpolates only before \w : ' { $ — and @' (perl-4 package
      # separator) is not supported here, matching the rest of PCL
      return undef unless $next =~ /\w/ || $next eq ':' || $next eq '{'
        || $next eq '$';
    }
  }
  return $sigil eq '$' ? _scan_dollar($text, $pos, \%opt)
                       : _scan_snail($text, $pos, \%opt);
}

# Identifier at $pos: \w+ with :: continuations (a qualified name is ONE
# name — the s378 lesson).  Digit-led names are digits-only: perl reads
# "$12abc" as ${12} . "abc" (StringInterpolation's \w+ grab is wider —
# divergence table).  A dangling "::" (not followed by \w) is swallowed
# into the SPAN but not the name: probed, "a$Foo::.b" prints "a.b" — perl
# consumes the colons as part of the reference.  Returns
# (name, name_end, span_end) or the empty list.
sub _scan_name {
  my ($text, $pos) = @_;
  my $rest = substr($text, $pos);
  my $name;
  if    ($rest =~ /^([0-9]+)/)         { $name = $1 }
  elsif ($rest =~ /^(\w+(?:::\w+)*)/)  { $name = $1 }
  else                                 { return () }
  my $name_end = $pos + length $name;
  my $span_end = $name_end;
  $span_end += 2 if substr($text, $name_end, 2) eq '::';
  return ($name, $name_end, $span_end);
}

# All-digit names are magic ($1, $0, ${12}) — form 'magic', canon undef,
# never rename targets.
sub _digits { return $_[0] =~ /^[0-9]+$/ }

# The body of a `${…}` / `@{…}` that names a MAGIC variable rather than an
# identifier or an expression: a caret name (`^CAPTURE`, `^TAINT`) or a
# single punctuation character (`@{+}` is `@+`, `@{-}` is `@-` — the
# spelling re/pat_rt_report.t uses, #314).  `+` alone is not an expression,
# so the reading is unambiguous.  Returns the bare name or ().
# The punctuation rule is Pl::PExpr::braced_punct_magic_name's, which the
# TOKEN-level pre-pass `_fold_braced_punct_magic` applies to the same
# spelling in code — one rule, asked at both levels.
sub _braced_magic_name {
  my ($guts) = @_;
  return $1 if $guts =~ /^(\^\w+)$/;
  return $1 if $guts =~ /^\s*([^\w\s])\s*$/;
  return ();
}

# Parse "{ blank* ident blank* }" with the brace at $pos.  Returns
# (name, name_span, after) for an identifier body, (undef, undef, after)
# for an expression body, or () when the brace is unbalanced.  What an
# identifier vs an expression MEANS is the caller's per-sigil decision.
sub _braced_ident {
  my ($text, $pos) = @_;
  my $after = _match_bracket($text, $pos);
  return () unless $after;
  my $guts_start = $pos + 1;
  my $guts = substr($text, $guts_start, $after - 1 - $guts_start);
  if ($guts =~ /^(\s*)(\w+(?:::\w+)*)(\s*)$/) {
    my ($lead, $name) = ($1, $2);
    my $ns = $guts_start + length $lead;
    return ($name, [$ns, $ns + length $name], $after);
  }
  return (undef, undef, $after);
}

sub _ev {
  my (%f) = @_;
  return { sigil => $f{sigil}, form => $f{form}, name => $f{name},
           canon => $f{canon}, span => $f{span}, name_span => $f{name_span},
           expr_span => $f{expr_span}, slice => 0, chain => [],
           postderef => undef };
}

# Find the matching close bracket for the open at $pos, counting ONLY that
# bracket pair (StringInterpolation's rule; a quoted close inside miscounts
# there and so miscounts here — divergence table).  Returns the index one
# past the close, or 0 when unbalanced.
sub _match_bracket {
  my ($text, $pos) = @_;
  my $open  = substr($text, $pos, 1);
  my $close = $open eq '[' ? ']' : $open eq '{' ? '}' : return 0;
  my $n = length $text;
  my ($depth, $i) = (1, $pos + 1);
  while ($i < $n && $depth > 0) {
    my $ch = substr($text, $i, 1);
    $depth++ if $ch eq $open;
    $depth-- if $ch eq $close;
    $i++;
  }
  return $depth == 0 ? $i : 0;
}

# ONE "read the NAME after a sigil and build its event" (#387 family 38 — the
# scanner's own four copies, a deref one and a plain one per sigil).  START is
# where the name begins, POS where the event's span does.  DEREF ($$name,
# @$name) names the SCALAR that holds the reference, so its canon carries a
# '$' whatever the outer sigil is; otherwise an all-digit name ($1, @1) is
# magic and has no canon.  Binding a subscript chain or a postderef to the
# event afterwards is the caller's business — the two sigils differ there.
sub _name_event {
  my ($text, $pos, $start, $sigil, $deref) = @_;
  my ($name, $name_end, $span_end) = _scan_name($text, $start);
  return undef unless defined $name;
  return _ev(sigil => $sigil,
             form  => ($deref ? 'deref' : _digits($name) ? 'magic' : 'plain'),
             name  => $name,
             canon => ($deref ? '$' . $name
                              : _digits($name) ? undef : $sigil . $name),
             name_span => [$start, $name_end],
             span      => [$pos, $span_end]);
}

# ── $-sigil forms ──────────────────────────────────────────────────────────
sub _scan_dollar {
  my ($text, $pos, $opt) = @_;
  my $next = substr($text, $pos + 1, 1);

  return _scan_braced_dollar($text, $pos, $opt) if $next eq '{';

  # $$name (scalar deref) / bare $$ (pid)
  if ($next eq '$') {
    if (substr($text, $pos + 2, 1) =~ /\w/) {
      my $ev = _name_event($text, $pos, $pos + 2, '$', 1) or return undef;
      _scan_chain($text, $ev, $opt);        # "$$r[0]" is $r->[0] (probed)
      _scan_postderef($text, $ev, $opt);
      return $ev;
    }
    # bare $$ = pid ($$$x scans as pid + $x, StringInterpolation's reading;
    # perl's ${${$x}} nesting is a recorded divergence)
    return _ev(sigil => '$', form => 'magic', name => '$', canon => undef,
               name_span => [$pos + 1, $pos + 2], span => [$pos, $pos + 2]);
  }

  # $^W caret magic (single uppercase letter; braced ${^NAME} is above)
  if ($next eq '^') {
    my $letter = substr($text, $pos + 2, 1);
    return undef unless $letter =~ /^[A-Z]$/;
    return _ev(sigil => '$', form => 'magic', name => '^' . $letter,
               canon => undef, name_span => [$pos + 1, $pos + 3],
               span => [$pos, $pos + 3]);
  }

  return _scan_array_index($text, $pos, $opt) if $next eq '#';

  # $::name — main-package spelling; the source spelling is the name
  if ($next eq ':' && substr($text, $pos + 2, 1) eq ':') {
    my ($base, $name_end, $span_end) = _scan_name($text, $pos + 3);
    return undef unless defined $base;   # bare $:: — literal
    my $name = '::' . $base;
    my $ev = _ev(sigil => '$', form => 'plain', name => $name,
                 canon => '$' . $name, name_span => [$pos + 1, $name_end],
                 span => [$pos, $span_end]);
    _scan_chain($text, $ev, $opt);
    _scan_postderef($text, $ev, $opt);
    return $ev;
  }

  # Single-punctuation magic; $+ / $- subscript as %+/%-/@+/@- elements
  if ($next ne '' && index($PUNCT_MAGIC, $next) >= 0) {
    my $ev = _ev(sigil => '$', form => 'magic', name => $next,
                 canon => undef, name_span => [$pos + 1, $pos + 2],
                 span => [$pos, $pos + 2]);
    # `$+`/`$-` are %+/%-/@+/@- elements; the rest of the punctuation ARRAY
    # family subscripts the same way (#451) — the set above says which.
    _scan_chain($text, $ev, $opt)
      if $next eq '+' || $next eq '-'
      || index($PUNCT_ARRAY_SUBSCRIPT, $next) >= 0;
    return $ev;
  }

  # Plain $name — the workhorse
  my $ev = _name_event($text, $pos, $pos + 1, '$') or return undef;
  _scan_chain($text, $ev, $opt);
  _scan_postderef($text, $ev, $opt);
  return $ev;
}

# ${ ... } family: braced name, braced number, ${^NAME}, or an expression.
# A braced NAME (or caret name) CLOSES the reference — no subscript, no
# postderef, in either mode: probed, "${x}[0]" prints $x then literal "[0]"
# (under strict, "${m}[0]" dies "Global symbol $m" — the SCALAR, never @m),
# "${x}{k}" prints 'SX{k}', "${^CAPTURE}[1]" prints "[1]", and "${ar}->@*"
# stays literal even with postderef_qq on.
# (Consequence recorded in docs/interp-scan.md: _interp_fixer's
# "${x}[ is @x-family" arm has the sigil family WRONG.)
#
# A braced EXPRESSION does NOT close it.  `${ EXPR }` is the deref of a
# value and perl continues into a subscript chain exactly as `$name[` does
# — probed on 5.40.3 (s426, task #420): "${$r}[1]" is 20, "${ $r }[1]" is
# 20 with blanks, "${$aoa}[0][1]" is 2, "${$rr}->[1]" takes the explicit
# arrow, and "${\ $x}[0]" dies "Not an ARRAY reference" (it took the
# group, it did not leave it literal).  The asymmetry with the NAME forms
# above is perl's own, not a simplification.
sub _scan_braced_dollar {
  my ($text, $pos, $opt) = @_;
  my ($name, $name_span, $after) = _braced_ident($text, $pos + 1);
  return undef unless $after;
  my $guts_start = $pos + 2;

  # ${^NAME} caret-string magic and ${+}-style punctuation magic
  if (my ($magic) = _braced_magic_name(
        substr($text, $guts_start, $after - 1 - $guts_start))) {
    return _ev(sigil => '$', form => 'magic', name => $magic, canon => undef,
               name_span => [$guts_start, $after - 1],
               span => [$pos, $after]);
  }
  # ${name} / ${ name } — perl's scan_ident allows blanks around the
  # identifier (StringInterpolation requires the bare spelling — table)
  if (defined $name) {
    return _ev(sigil => '$',
               form => (_digits($name) ? 'magic' : 'braced'),
               name => $name,
               canon => (_digits($name) ? undef : '$' . $name),
               name_span => $name_span,
               span => [$pos, $after]);
  }
  # ${ EXPR } — deref of the block's value; consumers re-parse expr_span
  my $ev = _ev(sigil => '$', form => 'expr', name => undef, canon => undef,
               expr_span => [$guts_start, $after - 1],
               span => [$pos, $after]);
  _scan_chain($text, $ev, $opt);          # "${$r}[1]" is $r->[1] (probed)
  _scan_postderef($text, $ev, $opt);
  return $ev;
}

# $#array / $#{array} / $#{EXPR} / $#$ref / $#- / $#+ — last-index family.
# No chain: the value is a scalar and perl attaches no subscript to it
# (probed: `"$#{$r}[0]"` and `"$#$r[0]"` are perl SYNTAX ERRORS).
sub _scan_array_index {
  my ($text, $pos, $opt) = @_;
  my $c2 = substr($text, $pos + 2, 1);
  # $#- / $#+ — @- and @+ are the only magic ARRAYS, so this is the whole
  # punctuation set (task #417: $#+ is the pattern's group count, $#- stops
  # at the last participant).  StringInterpolation has always read them.
  if ($c2 eq '-' || $c2 eq '+') {
    return _ev(sigil => '$#', form => 'magic', name => $c2, canon => undef,
               name_span => [$pos + 2, $pos + 3], span => [$pos, $pos + 3]);
  }
  if ($c2 eq '{') {
    my ($name, $name_span, $after) = _braced_ident($text, $pos + 2);
    return undef unless $after;
    if (defined $name) {
      return _ev(sigil => '$#', form => 'braced', name => $name,
                 canon => (_digits($name) ? undef : '@' . $name),
                 name_span => $name_span,
                 span => [$pos, $after]);
    }
    return _ev(sigil => '$#', form => 'expr', name => undef, canon => undef,
               expr_span => [$pos + 3, $after - 1],
               span => [$pos, $after]);
  }
  if ($c2 eq '$') {
    my ($name, $name_end, $span_end) = _scan_name($text, $pos + 3);
    return undef unless defined $name;
    return _ev(sigil => '$#', form => 'deref', name => $name,
               canon => '$' . $name, name_span => [$pos + 3, $name_end],
               span => [$pos, $span_end]);
  }
  my ($name, $name_end, $span_end) = _scan_name($text, $pos + 2);
  return undef unless defined $name;
  return _ev(sigil => '$#', form => 'plain', name => $name,
             canon => (_digits($name) ? undef : '@' . $name),
             name_span => [$pos + 2, $name_end],
             span => [$pos, $span_end]);
}

# ── @-sigil forms ──────────────────────────────────────────────────────────
sub _scan_snail {
  my ($text, $pos, $opt) = @_;
  my $next = substr($text, $pos + 1, 1);

  # @{name} / @{^NAME} / @{ EXPR } — a braced NAME closes the reference
  # (probed: "@{x}[0]" prints the whole @x then literal "[0]"), and so does
  # a braced CARET name ("@{^CAPTURE}[0]" prints "a b[0]" — task #422.1);
  # a braced EXPRESSION does not, exactly as on the '$' side above
  # ("@{$r}[1]" is 20, "@{$hr}{'a','b'}" is a hash slice, "@{[1,2]}[0]" is
  # 1 — all probed on 5.40.3, s426/#420).  A digit-led body (`@{12}`) is
  # not an array name; it stays on the expression path.
  if ($next eq '{') {
    my ($name, $name_span, $after) = _braced_ident($text, $pos + 1);
    return undef unless $after;
    my $guts_start = $pos + 2;
    if (my ($magic) = _braced_magic_name(
          substr($text, $guts_start, $after - 1 - $guts_start))) {
      return _ev(sigil => '@', form => 'magic', name => $magic, canon => undef,
                 name_span => [$guts_start, $after - 1],
                 span => [$pos, $after]);
    }
    if (defined $name && $name !~ /^[0-9]/) {
      return _ev(sigil => '@', form => 'braced', name => $name,
                 canon => '@' . $name,
                 name_span => $name_span,
                 span => [$pos, $after]);
    }
    my $ev = _ev(sigil => '@', form => 'expr', name => undef, canon => undef,
                 expr_span => [$guts_start, $after - 1],
                 span => [$pos, $after]);
    _scan_chain($text, $ev, $opt, 1);       # "@{$r}[1]" slices @$r (probed)
    return $ev;
  }

  # @$name — elements of the array referenced by $name
  if ($next eq '$') {
    return undef unless substr($text, $pos + 2, 1) =~ /\w/;
    my $ev = _name_event($text, $pos, $pos + 2, '@', 1) or return undef;
    _scan_chain($text, $ev, $opt, 1);       # "@$r[0]" slices @$r (probed)
    return $ev;
  }

  # @- / @+ match-offset arrays — dq/heredoc only ("in regexp, neither @+
  # nor @- are interpolated", toke.c scan_const; regex mode never gets here:
  # the start rules vetoed non-\w already)
  if ($next eq '-' || $next eq '+') {
    my $ev = _ev(sigil => '@', form => 'magic', name => $next,
                 canon => undef, name_span => [$pos + 1, $pos + 2],
                 span => [$pos, $pos + 2]);
    _scan_chain($text, $ev, $opt, 1);       # "@-[0]" is a slice (probed)
    return $ev;
  }

  # @name — whole array, or a slice when a group follows
  my $ev = _name_event($text, $pos, $pos + 1, '@') or return undef;
  _scan_chain($text, $ev, $opt, 1);
  return $ev;
}

# ── Subscript chain ────────────────────────────────────────────────────────
# Bind bracket groups after a reference: explicit "->[" / "->{", or a bare
# bracket (implicit arrow after the first group).  In regex mode intuit_more
# classifies the FIRST group ONLY — once a reference has bound a group the
# tokenizer is in expression mode and every further group binds
# unconditionally (probed: /$m[0][abc]/ dies on the bareword and
# /$h2{k}{2,3}/ dies "Not a HASH reference" — perl never re-classifies a
# continuation, not even a regcurly-valid one).
# '@'-sigil callers pass $max=1: a slice takes one group; a further bracket
# after a slice is a perl COMPILE ERROR ("syntax error near ][", probed), so
# leaving it unconsumed loses no legal program.
sub _scan_chain {
  my ($text, $ev, $opt, $max) = @_;
  my $p = $ev->{span}[1];
  my $groups = 0;
  while (!defined $max || $groups < $max) {
    my ($arrow, $bpos);
    if (substr($text, $p, 2) eq '->'
        && substr($text, $p + 2, 1) =~ /^[\[\{]$/) {
      ($arrow, $bpos) = (1, $p + 2);
    } elsif (substr($text, $p, 1) =~ /^[\[\{]$/) {
      ($arrow, $bpos) = (0, $p);
    } else {
      last;
    }
    last if $groups == 0 && $opt->{in_regex}
      && !intuit_more($text, $p, %$opt);
    my $after = _match_bracket($text, $bpos);
    last unless $after;                     # unbalanced: group not taken
    my $open = substr($text, $bpos, 1);
    push @{ $ev->{chain} }, { open => $open, arrow => $arrow,
                              span => [$p, $after],
                              guts_span => [$bpos + 1, $after - 1] };
    if (++$groups == 1) {
      # The FIRST group names the container: $x[i] reads @x, $x{k} reads
      # %x, @x{..} reads %x — but $x->[i] reads the scalar $x itself, and
      # deref/expr forms already name their true base.
      if (!$arrow && defined $ev->{canon}
          && ($ev->{form} eq 'plain' || $ev->{form} eq 'braced')) {
        if ($ev->{sigil} eq '$') {
          substr($ev->{canon}, 0, 1) = $open eq '[' ? '@' : '%';
        } elsif ($ev->{sigil} eq '@' && $open eq '{') {
          substr($ev->{canon}, 0, 1) = '%';
        }
      }
      $ev->{slice} = 1 if $ev->{sigil} eq '@';
    }
    $ev->{span}[1] = $p = $after;
  }
  return;
}

# ── Postfix dereference (postderef_qq) ─────────────────────────────────────
# ->$* ->$#* ->@* ->@[..] ->@{..} after a reference or its chain.  %-forms
# never interpolate.  Gated on the option in BOTH modes (toke gates the
# string and pattern cases on the same lexical feature).
# KEEP IN STEP with intuit_more's ->-branch (the port's approval list).
sub _scan_postderef {
  my ($text, $ev, $opt) = @_;
  return unless $opt->{postderef_qq};
  my $p = $ev->{span}[1];
  return unless substr($text, $p, 2) eq '->';
  my $c2 = substr($text, $p + 2, 1);
  my $c3 = substr($text, $p + 3, 1);
  my ($what, $end, $guts);
  if ($c2 eq '$') {
    if    ($c3 eq '*')                                { ($what, $end) = ('$*', $p + 4) }
    elsif ($c3 eq '#' && substr($text, $p + 4, 1) eq '*') { ($what, $end) = ('$#*', $p + 5) }
    else { return }
  } elsif ($c2 eq '@') {
    if ($c3 eq '*') { ($what, $end) = ('@*', $p + 4) }
    elsif ($c3 eq '[' || $c3 eq '{') {
      my $after = _match_bracket($text, $p + 3);
      return unless $after;                 # unbalanced: stays literal
      ($what, $end) = ('@' . $c3, $after);
      $guts = [$p + 4, $after - 1];
    }
    else { return }
  } else { return }
  $ev->{postderef} = { what => $what, span => [$p, $end],
                       guts_span => $guts };
  $ev->{span}[1] = $end;
  return;
}

# ── S_intuit_more, ported line-for-line from perl 5.40.3 toke.c ───────────
# "This is the one truly awful dwimmer necessary to conflate C and sed."
# $i is the position AFTER the variable/group; returns 1 = subscript (more
# expression), 0 = the bracket is regex syntax.  Outside patterns brackets
# are always subscripts.  The known_name option stands in for
# gv_fetchpvn_flags (symbol-table lookup of a multi-char name).
sub intuit_more {
  my ($text, $i, %opt) = @_;
  my $n = length $text;
  my $c0 = $i < $n ? substr($text, $i, 1) : '';

  # KEEP IN STEP with _scan_postderef: this port answers "does more bind?",
  # the scanner binds it — the two spellings of ->$* ->$#* ->@* ->@[ ->@{
  # must agree or a form would be approved here and dropped there.
  if ($c0 eq '-' && substr($text, $i + 1, 1) eq '>') {
    my $c2 = substr($text, $i + 2, 1);
    return 1 if $c2 eq '[' || $c2 eq '{';
    if ($opt{postderef_qq}) {
      my $c3 = substr($text, $i + 3, 1);
      return 1 if $c2 eq '$'
        && ($c3 eq '*' || ($c3 eq '#' && substr($text, $i + 4, 1) eq '*'));
      return 1 if $c2 eq '@' && $c3 ne '' && index('*[{', $c3) >= 0;
    }
  }
  return 0 if $c0 ne '{' && $c0 ne '[';
  return 1 if !$opt{in_regex};

  # In a pattern, {n,m} shapes are quantifiers, everything else in braces
  # is a subscript
  if ($c0 eq '{') {
    return regcurly($text, $i) ? 0 : 1;
  }

  # '[': maybe a character class — examine the guts
  my $s = $i + 1;
  my $first = $s < $n ? substr($text, $s, 1) : '';
  return 0 if $first eq ']' || $first eq '^';
  my $send = index($text, ']', $s);
  return 1 if $send < 0;                    # no ']': has to be an expression

  # Entirely one or two digits: call it a subscript
  if ($first =~ /[0-9]/ && $send - $s <= 2
      && ($send - $s == 1 || substr($text, $s + 1, 1) =~ /[0-9]/)) {
    return 1;
  }

  # The weigher ("this is terrifying, and it mostly works" — GH #16478)
  my $weight = $first eq '$' ? -1 : 2;
  my %seen;
  my $un = '';
  my $known = $opt{known_name};
  my $first_time = 1;
  for (my $p = $s; $p < $send; $p++, $first_time = 0) {
    my $prev = $un;
    my $ch = substr($text, $p, 1);
    $un = $ch;
    if ($ch eq '@' || $ch eq '&' || $ch eq '$') {
      # repeats of these strongly indicate a subscript; a known multi-char
      # name after one very strongly so
      $weight -= ($seen{$ch} // 0) * 10;
      my $nx = substr($text, $p + 1, 1);
      if ($nx =~ /^\w/) {
        # deliberate second copy of _scan_name's grammar: this function is
        # a self-contained C port (it approximates toke's scan_ident, not
        # the scanner's name rule) — keep the two spellings in step
        my ($ident) = substr($text, $p + 1) =~ /^(\w+(?:::\w+)*)/;
        if (length($ident) > 1 && $known && $known->($ident)) {
          $weight -= 100;
        } else {
          $weight -= 10;
        }
      } elsif ($ch eq '$' && $nx ne ''
               && index('[#!%*<>()-=', $nx) >= 0) {
        # a punctuation variable; next-next closing makes it likelier
        my $c2 = substr($text, $p + 2, 1);
        if ($c2 ne '' && index('])} =', $c2) >= 0) { $weight -= 10 }
        else                                       { $weight -= 1 }
      }
    } elsif ($ch eq '\\') {
      my $nx = substr($text, $p + 1, 1);
      if ($nx ne '') {
        if    (index('wds]', $nx) >= 0)          { $weight += 100 }
        elsif ($seen{"'"} || $seen{'"'})         { $weight += 1 }
        elsif (index('abcfnrtvx', $nx) >= 0)     { $weight += 40 }
        elsif ($nx =~ /[0-9]/) {
          $weight += 40;
          $p++ while substr($text, $p + 1, 1) =~ /[0-9]/;
        }
      } else { $weight += 100 }               # terminal backslash
    } elsif ($ch eq '-') {
      $weight += 50 if substr($text, $p + 1, 1) eq '\\';
      $weight += 30 if !$first_time && $prev ne ''
        && index('aA01! ', $prev) >= 0;
      my $nx = substr($text, $p + 1, 1);
      $weight += 30 if $nx ne '' && index('zZ79~', $nx) >= 0;
      $weight -= 5 if $first_time && ($nx =~ /[0-9]/ || $nx eq '$');
    } else {
      my $nx = substr($text, $p + 1, 1);
      if (($first_time
           || ($prev !~ /\w/ && $prev ne '$' && $prev ne '@' && $prev ne '&'))
          && $ch =~ /[A-Za-z]/ && $nx =~ /[A-Za-z]/) {
        # a run of alphas spelling a keyword is almost never a charclass.
        # NOTE the faithful quirk: the run advance skips the char after it
        # from individual weighing, exactly as the C loop does.
        my $q = $p;
        $q++ while substr($text, $q, 1) =~ /[A-Za-z]/;
        $weight -= 150 if $KEYWORDS{ substr($text, $p, $q - $p) };
        $p = $q;
      }
      $weight += 5 if !$first_time && $prev ne ''
        && ord($ch) == ord($prev) + 1;      # consecutive chars: classier
      $weight -= ($seen{$ch} // 0);         # repeats: subscriptier
    }
    $seen{$un}++;
  }
  return $weight >= 0 ? 0 : 1;              # >= 0: probably a charclass
}

# ── Perl_regcurly, ported from perl 5.40.3 regcomp.c ──────────────────────
# Is the text at $i (which must be '{') syntactically a {m,n} quantifier?
#   \{ blank* digits? blank* ( , blank* digits? blank* )? \}
# with at least one number present ({2} {2,} {2,3} {,3} { 2, 3 } yes;
# {} {,} {k} {$k} no).  Returns the index one past the '}' (truthy), or 0.
sub regcurly {
  my ($text, $i) = @_;
  my $n = length $text;
  return 0 if $i >= $n || substr($text, $i, 1) ne '{';
  my $s = $i + 1;
  my $sawnum = 0;
  $s++ while $s < $n && substr($text, $s, 1) =~ /[ \t]/;
  if (substr($text, $s, 1) =~ /[0-9]/) {
    $sawnum = 1;
    $s++ while $s < $n && substr($text, $s, 1) =~ /[0-9]/;
  }
  $s++ while $s < $n && substr($text, $s, 1) =~ /[ \t]/;
  if (substr($text, $s, 1) eq ',') {
    $s++;
    $s++ while $s < $n && substr($text, $s, 1) =~ /[ \t]/;
    if (substr($text, $s, 1) =~ /[0-9]/) {
      $sawnum = 1;
      $s++ while $s < $n && substr($text, $s, 1) =~ /[0-9]/;
    }
  }
  $s++ while $s < $n && substr($text, $s, 1) =~ /[ \t]/;
  return 0 if $s >= $n || substr($text, $s, 1) ne '}' || !$sawnum;
  return $s + 1;
}

1;

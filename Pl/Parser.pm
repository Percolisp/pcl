# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::Parser;

use v5.20;
use strict;
use warnings;

use Moo;

use PPI;
use Scalar::Util qw(refaddr);
use Data::Dump qw/dump/;
use File::Basename;
use File::Spec;
use Cwd qw(abs_path);

use Pl::PExpr qw(SCALAR_CTX LIST_CTX VOID_CTX INHERIT_CTX);
use Pl::CLForm qw(cl_sym cl_pkg);
use Pl::ExprToCL;
use Pl::Environment;
# THE global partition (task #289): every declaration this file emits, and the
# `local` lowering below, ask the same function whether a name keeps today's
# defvar + dynamic-let or becomes a symbol-macro cell.
use Pl::GlobalPartition qw(global_decl_form is_exception_global);

# File-level counters (shared across all Parser instances within a load)
my $anon_block_counter = 0;
my $state_var_counter  = 0;
my $lex_var_counter    = 0;

# Statement-level parser prototype.
# Iterates over PPI statements, delegates expressions to PExpr,
# generates Common Lisp via ExprToCL.

has filename => (
  is        => 'ro',
  predicate => 'has_filename',
);

has code => (
  is        => 'ro',
  predicate => 'has_code',
);

# eval_mode: set when transpiling a string for `eval "..."` (via pl2cl
# --eval-pkg / --server).  In this mode, variables used but not declared
# inside the eval string are the caller's in-scope lexicals; instead of
# emitting forward-declaration defvars for them (which proclaim them special
# and break lexical capture), we record them in _eval_free_vars and wrap the
# eval body in a (p-eval-thunk '(names) (lambda (syms) body)) so the runtime
# can bind them to the caller's containers.  See docs/eval-lexical-capture.md.
has eval_mode => (
  is      => 'ro',
  default => sub { 0 },
);

# eval_pkg: the Perl package in effect at the `eval "..."` call site (e.g.
# "Foo" when the eval runs inside `package Foo { ... }`).  Seeds the
# Environment's package_stack so __PACKAGE__ (and other compile-time package
# references) inside the eval string resolve to the caller's package rather
# than defaulting to "main".  Set via pl2cl --eval-pkg / --server.
has eval_pkg => (
  is      => 'ro',
  default => sub { undef },
);

has ppi_doc => (
  is        => 'lazy',
);

has indent_level => (
  is        => 'rw',
  default   => 0,
);


# Output bucket system (replaces flat @output + post-processing)
# Each section = one package entry point; buckets are assembled in order:
#   preamble → declarations → definitions → runtime
has _sections => (
  is      => 'rw',
  default => sub { [] },
);
has _cur_section => (
  is      => 'rw',
  default => 0,
);
has _cur_bucket => (
  is      => 'rw',
  default => 'runtime',
);
has _block_depth => (
  is      => 'rw',
  default => 0,
);

has environment => (
  is        => 'lazy',
);

# @INC paths for module lookup (transpile-time)
# Computed at compile time: project_root/lib + Perl's @INC
my $_pcl_lib_dir = File::Spec->catdir(dirname(dirname(abs_path(__FILE__))), 'lib');
has inc_paths => (
  is        => 'rw',
  default   => sub { [$_pcl_lib_dir, @INC] },  # Include project lib/ + Perl's @INC
);

# Track modules currently being parsed (cycle detection)
has _parsing_modules => (
  is        => 'rw',
  default   => sub { {} },
);


# Flag to suppress output emission (for prototype-only parsing)
has collect_prototypes_only => (
  is        => 'rw',
  default   => 0,
);

# Where the cross-statement lexical registries live (_let_bound_vars,
# _catch_labels, _eval_span_captures): the v2 driver (Pl::Parser2) OWNS them
# (#153 chunk 0) — one registry, one owner, both compilers read through this
# accessor.  A parser serving as a v2 expression seam reaches its owner via
# the _v2_owner back-ref; a standalone parser (the prototype-collection
# walkers) is its own home.  _v2_owner is weakened, so "was owned but the
# owner is gone" must DIE (rule 12): falling back to $self there would read
# a fresh empty registry — silently dropping every live lexical.
sub lex_home {
  my $self = shift;
  return $self->{_v2_owner} if $self->{_v2_owner};
  die "PCL internal error: seam parser outlived its v2 owner; "
    . "lexical registry unreachable" if $self->{_v2_owned};
  return $self;
}

# Make this parser Pl::Parser2's SEAM: v1's machinery runs on it only through
# capture_v1 (statements it has no arm for, embedded blocks the structural
# route declines) and through the expression compiler (`_parse_expression_form`).
# It is never parse()d, so its emission state is set here: one open scratch
# section, so any v1 helper reached OUTSIDE a capture has a bucket to write to
# (that text is never printed).  The owner back-reference is weak — a strong
# one would keep owner and seam alive for the life of the process — and
# _v2_owned is its non-weak twin so lex_home DIES instead of answering with an
# empty registry when the owner is gone (see above).
sub become_seam {
  my ($self, $owner) = @_;
  $self->_sections([]);
  $self->_open_section('pcl');
  $self->_cur_bucket('runtime');
  $self->{_v2_owner} = $owner;
  Scalar::Util::weaken($self->{_v2_owner});
  $self->{_v2_owned} = 1;
  return $self;
}

# (The lenient_ppi truncate-at-first-unparseable-line flag lived here until
# E4.1 step 3 — it only ever worked by silently dropping code, was retired
# by ruling in s356 (§5a.4: a PPI failure dies naming the file), and
# nothing could construct a Parser with it after pl2cl made the flag inert.)


sub _build_environment {
  my $self = shift;
  return Pl::Environment->new(
    source_file => $self->has_filename ? $self->filename : '-',
  );
}


sub _maybe_decode_utf8 {
  my ($src) = @_;
  # `use utf8` tells Perl the source is UTF-8: multi-byte sequences in string
  # literals (and identifiers) are single CHARACTERS, so length/substr/index/
  # regex see characters, not bytes.  PCL reads the source as raw bytes, so we
  # must decode it here when the pragma is in effect — otherwise "café" is 5
  # bytes, not 4 chars.  Only decode raw byte input (an already-decoded char
  # string is left alone); without `use utf8` Perl treats high bytes as Latin-1,
  # which matches reading the bytes unchanged.  The pl2cl output is already
  # written UTF-8, so the wide chars round-trip into the generated CL.
  if (defined $src && $src =~ /\buse\s+utf8\b/ && !utf8::is_utf8($src)) {
    my $copy = $src;
    utf8::decode($copy) and return $copy;   # leaves $src on invalid bytes
  }
  return $src;
}

sub _preprocess_source {
  my ($src) = @_;
  # hex()/oct() on a hex-float mantissa (below) can exceed 0xffffffff (32-bit),
  # which makes Perl emit spurious 'Hexadecimal number > 0xffffffff non-portable'
  # / 'Integer overflow' warnings *from our own toolchain* while transpiling source
  # that legitimately contains large hex literals (hexfp.t, sprintf.t). These
  # conversions are intentional and correct on 64-bit, so silence the noise.
  no warnings 'portable', 'overflow';
  # Convert C99/Perl hex/binary/octal float literals (0x1.8p-1, 0b10p-2, 010.1p0)
  # to decimal before PPI sees them. PPI doesn't understand the 'p' exponent marker
  # and misparses 0x1.8p-1 as: 0x1 . p - 1 (hex-num, concat, bareword, minus, num).
  # Perl allows underscore separators anywhere in these literals.
  #
  # CRITICAL: only convert *numeric literals*, never text that merely looks like one
  # inside a quoted string (e.g. the string '0x1p+0' must stay '0x1p+0', not become
  # '1'). Each substitution therefore matches a quoted string as its FIRST alternative
  # and passes it through unchanged, so the float pattern is never seen inside a string.
  # A float pattern inside a comment is harmless to convert — PPI discards
  # comments — but a comment must still be CONSUMED by the pass-through
  # alternative, and that is not cosmetic (s415):
  #
  #   AN APOSTROPHE IN A COMMENT OPENS A STRING THAT NEVER CLOSES until the
  #   next one, and everything between the two is passed through untouched.
  #   `# it doesn't matter` … `# that's all` therefore hid a whole `format`
  #   block from the stripper below — measured in t/op/closure.t (267 passing
  #   rows), t/comp/parser.t, t/op/gv.t and t/uni/gv.t, where the unstripped
  #   block then swallowed the following statement (the exact corruption the
  #   stripper exists to prevent).  Every pass here has the same failure mode:
  #   the rewrite it owes silently does not happen, and PPI mis-structures the
  #   statement.
  #
  # So the skip alternative is strings OR a comment.  `$#array` and an escaped
  # `\#` are not comment starts; a `#` inside a string is already covered
  # because the string alternative is tried first at every position.
  my $str_re  = qr{'(?:\\.|[^'\\])*'|"(?:\\.|[^"\\])*"};
  my $skip_re = qr{$str_re|(?<![\$\\])#[^\n]*};
  # Hex float: 0x[hex_][.[hex_]]p[+-][decimal_]
  $src =~ s{($skip_re)|0x([0-9a-fA-F_]*)\.?([0-9a-fA-F_]*)[pP]([+-]?[\d_]+)}{
    if (defined $1) { $1 } else {
      my ($int_str, $frac_str, $exp_str) = ($2, $3, $4);
      $int_str  =~ s/_//g;
      $frac_str =~ s/_//g;
      $exp_str  =~ s/_//g;
      my $mantissa = ($int_str ne '' ? hex($int_str) : 0);
      $mantissa += hex($frac_str) / (16 ** length($frac_str)) if $frac_str ne '';
      sprintf("%.17g", $mantissa * (2 ** $exp_str));
    }
  }gex;
  # Binary float: 0b1.1p0, 0b10p-2
  $src =~ s{($skip_re)|0b([01_]+)(?:\.([01_]*))?[pP]([+-]?[\d_]+)}{
    if (defined $1) { $1 } else {
      my ($int_str, $frac_str, $exp_str) = ($2, $3 // '', $4);
      $int_str  =~ s/_//g;
      $frac_str =~ s/_//g;
      $exp_str  =~ s/_//g;
      my $mantissa = oct("0b$int_str");
      $mantissa += oct("0b$frac_str") / (2 ** length($frac_str)) if $frac_str ne '';
      sprintf("%.17g", $mantissa * (2 ** $exp_str));
    }
  }gex;
  # Octal float: 010.1p0, 00p0.  Lookbehind prevents matching digits inside a larger number.
  $src =~ s{($skip_re)|(?<!\w)0([0-7_]+)(?:\.([0-7_]*))?[pP]([+-]?[\d_]+)}{
    if (defined $1) { $1 } else {
      my ($int_str, $frac_str, $exp_str) = ($2, $3 // '', $4);
      $int_str  =~ s/_//g;
      $frac_str =~ s/_//g;
      $exp_str  =~ s/_//g;
      my $mantissa = oct("0$int_str");
      $mantissa += oct("0$frac_str") / (8 ** length($frac_str)) if $frac_str ne '';
      sprintf("%.17g", $mantissa * (2 ** $exp_str));
    }
  }gex;
  # Perl-4 `'` package separator in a SUB DECLARATION name: `sub x'y {…}`
  # → `sub x::y {…}` (A'B == A::B, valid through the 5.40 oracle).  PPI
  # cannot tokenize the tick form at all — `sub x'y { 1 }` reads as
  # Word(sub) Word(x) Quote::Single("'y { 1 }\nx'"), destroying everything
  # up to the next apostrophe — so this must be normalised before PPI.
  # Deliberately narrow: only after `sub\s+`, so `don't`/`can't` in code,
  # comments, regex or tr content are never touched; the $str_re
  # alternative passes quoted strings through untouched.  Symbolic uses of
  # tick names in STRINGS (`&{"x'y"}`) are normalised at runtime instead
  # (%p-tick-package-seps).  Bareword calls `x'y()` stay unsupported.
  $src =~ s{($skip_re)|\bsub(\s+)([A-Za-z_]\w*(?:'[A-Za-z_]\w*)+)}{
    if (defined $1) { $1 } else {
      my ($ws, $name) = ($2, $3);
      $name =~ s/'/::/g;
      "sub$ws$name";
    }
  }gex;
  # Strip type annotations from foreach loop variables: `for my Dog $spot` → `for my $spot`.
  # Perl allows `for my ClassName $var` but PPI can't parse the ClassName and stops,
  # producing a broken AST. PCL ignores type constraints anyway, so just drop them.
  $src =~ s/\b(for(?:each)?\s+(?:my|our))\s+[A-Za-z_]\w*(?:::[A-Za-z_]\w*)*\s+(\$)/$1 $2/g;
  # `CORE::my`/`CORE::our`/`CORE::state`/`CORE::local` → drop the `CORE::`.  The
  # prefix forces the core builtin meaning over a same-named user sub, but a
  # declarator can NEVER be shadowed by a user sub (they are grammar keywords,
  # not overridable functions), so `CORE::my` is exactly `my` in every context.
  # It MUST be normalised at the source level rather than on the PPI tree: PPI
  # does not recognise the CORE::-prefixed declarator and mis-structures the
  # enclosing construct (`for CORE::my $v (@l) {…}` yields a `for` compound with
  # NO list/block child — the loop is lost).  The leading $str_re alternative
  # passes quoted strings through untouched, so the identical text inside a
  # string literal is never rewritten.  The negative lookbehind keeps a package
  # variable like `$CORE::my` intact (only a bare declarator word is a target),
  # and the lookahead requires the declarator context (whitespace then a sigil
  # or `(`), so only an actual `CORE::my $x` / `CORE::our (@a)` form is rewritten.
  $src =~ s/($skip_re)|(?<![\w\$\@\%:])CORE::(my|our|state|local)\b(?=\s*[\$\@\%\(])/ defined $1 ? $1 : $2 /ge;
  $src = _strip_format_blocks($src);
  return $src;
}

# Remove `format NAME = … .` report templates.  `format`/`write` are
# deliberately not-supported (docs/not-supported.md), but PPI does not
# recognise the keyword: it swallows the picture lines AND the following
# statement into one bogus PPI::Statement, so the `.` terminator surfaces as an
# unknown Operator and the next real statement is lost — the very corruption
# this exists to prevent.
#
# LINE-ANCHORED, not a regex over the whole file (s415).  The regex version
# tried to protect string literals with a leading `($str_re)|` alternative, and
# that heuristic is not merely imperfect, it is WRONG at scale: a quote
# character inside a regex (`qr/Undefined format "…"/`), inside a format
# PICTURE line, or an apostrophe pair across two comments (`# doesn't` … `#
# that's`) opens a "string" that swallows the next format header.  Measured
# before this change: t/op/write.t stripped 39 of its 104 formats, and the 65
# survivors each ate the statement after them; t/op/closure.t (267 passing
# rows), t/comp/parser.t, t/op/gv.t and t/uni/gv.t each lost one the same way.
#
# perl's own rule is line-based — a header line ending at `=`, a body, a line
# holding just `.` — so this reads it that way and cannot be confused by
# quoting.  What it gives up is the one-line-string guard, which cost nothing
# real: a header must OWN its line, so only a multi-line string containing a
# line `format X =` AND a later line `.` could be touched; the safety valve
# below (no terminator within 500 lines ⇒ leave everything alone) bounds even
# that.  The NAME is anything up to the `=`: perl takes a qualified name in
# either spelling and a non-ASCII one (`format ::two =`, `format 'one =`,
# `format Ẋ =`, `format +x =`, and bare `format =` for STDOUT).
#
# Removed lines become EMPTY lines rather than disappearing, so every later
# line keeps its number and a diagnostic points where perl would point.
sub _strip_format_blocks {
  my ($src) = @_;
  return $src unless $src =~ /^[ \t]*format\b/m;
  my @lines = split /^/, $src;
  my $i = 0;
  while ($i < @lines) {
    unless ($lines[$i] =~ /^[ \t]*format(?:[ \t]+[^\n=]+)?[ \t]*=[ \t]*\r?\n?$/) {
      $i++;
      next;
    }
    my ($end, $limit) = (undef, $i + 500 < $#lines ? $i + 500 : $#lines);
    for my $j ($i + 1 .. $limit) {
      if ($lines[$j] =~ /^\.[ \t]*\r?\n?$/) { $end = $j; last }
    }
    if (!defined $end) { $i++; next }   # no terminator in sight: not a format
    $lines[$_] = "\n" for $i .. $end;
    $i = $end + 1;
  }
  return join '', @lines;
}

# PPI tokenizes `%-` / `%+` (the named-capture magic hashes) greedily, so `7%-3`
# becomes [Number 7][Magic %-][Number 3] — losing the modulo operator and giving
# a PARSE ERROR. But the magic hashes can never legally be *followed* by an
# operand and only ever *follow* a non-term (operator, comma, `(`, list-op word,
# or start-of-expression). So when a `%-`/`%+` Magic token directly follows a
# TERM (number, variable, string, `$#a`, or a closing `(...)`/`[...]`/`{...}`),
# it is really the modulo operator applied to a signed operand. We re-split it
# by inserting a space (`%-` -> `% -`) and re-parse. Working on the PPI tree
# (not the raw source) means strings and regex bodies — already their own token
# types — are never touched.
sub _fix_modulo_magic {
  my ($doc) = @_;
  my $changed = 0;
  for my $tok (@{ $doc->find('PPI::Token::Magic') || [] }) {
    my $c = $tok->content;
    next unless $c eq '%-' || $c eq '%+';
    my $prev = $tok->sprevious_sibling;
    next unless $prev;
    my $is_term =
         $prev->isa('PPI::Token::Number')
      || $prev->isa('PPI::Token::Symbol')
      || $prev->isa('PPI::Token::Magic')
      || $prev->isa('PPI::Token::Quote')
      || $prev->isa('PPI::Token::ArrayIndex')
      || $prev->isa('PPI::Structure');   # closing (...) / [...] / {...}
    next unless $is_term;
    $tok->set_content('% ' . substr($c, 1));
    $changed = 1;
  }
  return $changed;
}

# Perl allows whitespace between a sigil and its identifier (`my $ bits = …`
# is the variable $bits — perl's own caller.t line 279 uses this).  PPI
# tokenizes that as a Cast token followed by a bare Word, and PCL then emits
# WRONG code (`print $ arr[1]` called an undefined function `pl-arr`).  A
# genuine dereference Cast is always followed by a Symbol, Magic token, or
# {...} block — never a bare Word — so Cast+Word is unambiguously the
# spaced-sigil form.  Merge the name into the Cast; the caller re-parses, and
# the serialized `$bits` re-tokenizes as one ordinary Symbol.  Working on the
# PPI tree means strings/regex bodies are never touched.  Deliberately Word
# ONLY: names PPI tokenizes as something else (`$ x` → Operator, `$ s` →
# a swallowed substitution) are unsalvageable-torture territory and stay
# unsupported, as does `foreach my $ i` (PPI fails the whole parse).
sub _fix_spaced_sigils {
  my ($doc) = @_;
  my $changed = 0;
  for my $tok (@{ $doc->find('PPI::Token::Cast') || [] }) {
    my $sig = $tok->content;
    next unless $sig eq '$' || $sig eq '@' || $sig eq '%';
    my $next = $tok->snext_sibling;
    next unless $next && $next->isa('PPI::Token::Word');
    my $name = $next->content;
    next unless $name =~ /^(?:::)?\w+(?:(?:::|')\w+)*$/;
    $tok->set_content($sig . $name);
    $next->delete;
    $changed = 1;
  }
  return $changed;
}

# ---- Core feature pragmas, answered for PPI's own lexer (task #360) -------
#
# PPI decides whether `try` / `signatures` are IN EFFECT at a token, and it
# gets two of perl's three spellings wrong: `use experimental 'try'` comes back
# as `signatures => 0`, and a VERSION BUNDLE (`use v5.40`) answers signatures
# only — so `use v5.40; try {…} catch ($e) {…}` lexed like the no-feature case
# (one swallowing statement) and the whole statement was DROPPED, announced,
# rc 0.  `use feature 'try'` was the only spelling that worked.
#
# A feature-enabling CORE pragma is LANGUAGE, not module behaviour (CLAUDE.md
# 9a): `feature`, `experimental` and the `use vN` bundles are perl's own, the
# way `strict` / `warnings` / `utf8` already are here.  And the mechanism is
# PPI's OWN hook — `custom_feature_include_cb` is consulted BEFORE its built-in
# logic — so this is a table, not a source rewrite.  Returning undef falls
# through to PPI, which still owns every other include.
#
# The bundle thresholds are perl's, read off `%feature::feature_bundle` rather
# than guessed: `try` is in bundles 5.39 and 5.40, `signatures` from 5.35.
# They are a STATIC table on purpose — PCL must compile `use v5.40; try` the
# same way whatever perl it happens to run under — and `Pl/t/feature-pragma-01.t`
# re-derives them from the running perl so a drift fails a row.
our %PCL_FEATURE_BUNDLE = (signatures => 5.035, try => 5.039);

sub _pcl_feature_include_cb {
  my ($inc) = @_;
  my $type = $inc->type // '';
  return undef if $type ne 'use' && $type ne 'no';       # require enables nothing
  # `use v5.40;` / `use 5.040;` — a VERSION BUNDLE.  It REPLACES the scope's
  # feature set (perl: `use feature 'try'; use v5.36;` leaves try OFF), which
  # is why the "off" answers here are explicit rather than omitted.
  my $v = $inc->version;
  if (defined $v && length $v) {
    return undef if $type eq 'no';
    my $n = _perl_version_number($v);
    return undef if !defined $n;
    return { map +($_ => ($n >= $PCL_FEATURE_BUNDLE{$_} ? 'perl' : 0)),
             keys %PCL_FEATURE_BUNDLE };
  }
  my $module = $inc->module // '';
  return undef if $module ne 'feature' && $module ne 'experimental';
  # perl's experimental.pm is `feature->import(@names)` plus a warnings
  # unimport, so for the features PPI's lexer knows the two spellings are the
  # same answer.  Naming only what the statement asks for matters: a returned
  # key STOPS PPI's logic, and `use experimental 'try'` must not also claim
  # something about signatures (which is exactly PPI's bug here).
  my $on = $type eq 'use' ? 'perl' : 0;
  my %mods;
  for my $name (_include_string_args($inc)) {
    $mods{$name} = $on if exists $PCL_FEATURE_BUNDLE{$name};
  }
  # An EMPTY answer, not undef: this table owns both pragmas outright, and
  # "you asked for a feature I do not model" must mean "nothing changes".
  # Falling through would hand `use experimental 'defer'` back to PPI, whose
  # experimental branch answers `signatures => 0` for it — silently turning
  # OFF a feature an earlier `use feature 'signatures'` switched on.
  return \%mods;
}

# A perl version literal as a number comparable with the bundle table above:
# `v5.40` / `v5.40.1` are dotted (minor is thousandths), `5.036` is already
# one.  Deliberately NOT version.pm — `lib/` carries a version.pm SHIM for
# transpiled code, and the transpiler must not depend on which of the two its
# own @INC finds first.
sub _perl_version_number {
  my ($v) = @_;
  return undef if !defined $v;
  $v =~ s/_//g;
  return $1 + $2 / 1000 + ($3 // 0) / 1_000_000
    if $v =~ /^v(\d+)\.(\d+)(?:\.(\d+))?\z/
    || $v =~ /^(\d+)\.(\d+)\.(\d+)\z/;
  return $v + 0 if $v =~ /^\d+(?:\.\d+)?\z/;
  return undef;
}

# The string arguments of an include, in order: `'try'`, `qw(try say)` and the
# parenthesised spellings alike.  (PPI has a private _decompose_arguments; this
# reads the public ->arguments instead, so a PPI internal cannot move under us.)
sub _include_string_args {
  my ($inc) = @_;
  my @out;
  for my $arg ($inc->arguments) {
    my @toks = $arg->isa('PPI::Token') ? ($arg)
             : $arg->isa('PPI::Node')
               ? @{ $arg->find(sub {
                     $_[1]->isa('PPI::Token::Quote')
                  || $_[1]->isa('PPI::Token::QuoteLike::Words') }) || [] }
             : ();
    for my $t (@toks) {
      push @out, $t->isa('PPI::Token::QuoteLike::Words') ? $t->literal
               : $t->isa('PPI::Token::Quote')            ? $t->string
               : ();
    }
  }
  return @out;
}

# THE PPI construction site.  Every document PCL parses is built here — the
# first parse and the post-repair reparse below — so the feature callback is
# attached once and cannot drift between them.
sub _ppi_new {
  my ($src, %opt) = @_;
  return PPI::Document->new(\$src,
                            custom_feature_include_cb => \&_pcl_feature_include_cb,
                            %opt);
}

sub _ppi_parse {
  my ($self, $src, %opt) = @_;
  # %opt is how EVAL-MODE seeds the lexer with the features in effect at the
  # eval SITE (#364): perl's feature pragmas are lexical and a string eval
  # inherits them, but this parse only ever sees the bare eval text.
  my $doc = _ppi_new($src, %opt);
  # PPI GLOBAL-STATE BUG (docs/ppi-upstream-bugs.md §13, task #356): once
  # certain documents have been parsed in a PROCESS, a later document's
  # trailing `__END__`/`__DATA__` section comes back carrying ONE EXTRA
  # newline.  The same bytes round-trip clean in a fresh process and grow by
  # one in a warm one (measured: parse t/op/switch.t, then re-parse its last
  # two lines).  It matters here because the DATA section IS program data —
  # `while (<DATA>)` would read a line the file does not contain — and this is
  # the ONE place both pipelines turn source into a document (Parser2's
  # repair reparses through here too), so the invented tail is trimmed once,
  # against the bytes we were actually given.
  _trim_invented_tail($doc, $src) if $doc;
  # _extract_prototype_attributes must run BEFORE _desugar_anon_signatures:
  # it strips the `:prototype(...)` attribute (and wraps anon subs), and the
  # signature desugar then finds the now-attribute-free `sub` it spliced in
  # (its find() runs over the already-mutated tree).
  # _rewrite_current_sub runs LAST: an ANON sub's signature is a single
  # Token::Prototype until _desugar_anon_signatures turns it into statements,
  # so a `__SUB__` in a parameter DEFAULT (`sub ($k, $r = __SUB__->($k-1))`)
  # is not a Word — and therefore invisible — before that pass.  A NAMED sub's
  # signature is already a Structure, so it never depended on the order.
  if ($doc && (_fix_modulo_magic($doc) | _fix_spaced_sigils($doc)
               | $self->_extract_prototype_attributes($doc)
               | $self->_desugar_anon_signatures($doc)
               | _rewrite_current_sub($doc))) {
    my $fixed = $doc->serialize;
    my $redo  = _ppi_new($fixed, %opt);   # the seed applies to the reparse too
    if ($redo) {
      _trim_invented_tail($redo, $fixed);
      $doc = $redo;
    }
  }
  # AFTER any serialize+reparse: this pass swaps token classes in place with
  # the text unchanged, so a reparse would just re-create the Version tokens.
  _reclassify_bare_vwords($doc) if $doc;
  _merge_unicode_symbols($doc) if $doc;
  _merge_punct_array_symbols($doc) if $doc;
  return $doc;
}

# Drop trailing whitespace the PARSE invented (see the note in _ppi_parse).
# Bounded and byte-exact: it stops the moment the document is no longer longer
# than the source it came from, and only ever deletes WHITESPACE, so a document
# PPI parsed faithfully is untouched.
sub _trim_invented_tail {
  my ($doc, $src) = @_;
  while (length($doc->serialize) > length($src)) {
    my $last = ($doc->tokens)[-1] or last;
    last unless $last->isa('PPI::Token::Whitespace');
    $last->delete or last;
  }
  return;
}

# Perl engages v-stringness for a DOTLESS `vNN` only in expression position
# with no sub of that name declared (op/ver.t): `v65 => …` and `$h{v65}`
# autoquote to the plain string "v65" ("v-stringness is not engaged for vX"),
# and a declared `sub v77` wins over the v-string reading — `$x = v77` calls
# it (the "poetry optimization").  PPI tokenizes ALL of these as
# Token::Number::Version, so reclassify the affected tokens as ordinary Words
# and let the existing bareword machinery (fat-comma autoquote, subscript
# autoquote, no-parens sub call) decide the meaning.  Dotted forms
# (v1.2 / 1.2.3) always stay v-strings — even when a sub of the leading name
# exists — and `use`/`require` version statements are never touched.
sub _reclassify_bare_vwords {
  my ($doc) = @_;
  my $verts = $doc->find('PPI::Token::Number::Version') || [];
  return if !@$verts;
  my %subs;
  for my $s (@{ $doc->find('PPI::Statement::Sub') || [] }) {
    my $n = $s->name;
    $subs{$n} = 1 if defined $n;
  }
  for my $tok (@$verts) {
    my $c = $tok->content;
    next if $c !~ /^v\d+$/;   # dotless vNN only
    my $stmt = $tok->statement;
    next if $stmt && $stmt->isa('PPI::Statement::Include');
    my $as_word = 0;
    my $next = $tok->snext_sibling;
    if ($next && $next->isa('PPI::Token::Operator') && $next->content eq '=>') {
      $as_word = 1;   # fat-comma LHS autoquotes any bareword
    }
    else {
      my $parent = $tok->parent;
      my $gp = $parent && $parent->parent;
      if ($parent && $parent->isa('PPI::Statement::Expression')
          && $gp && $gp->isa('PPI::Structure::Subscript')
          && $gp->start && $gp->start->content eq '{'
          && scalar(grep { $_->significant } $parent->children) == 1) {
        $as_word = 1;   # single-bareword hash subscript autoquotes
      }
      elsif ($subs{$c}) {
        $as_word = 1;   # declared sub wins over the v-string reading
      }
    }
    next if !$as_word;
    my $word = PPI::Token::Word->new($c);
    $tok->insert_before($word);
    $tok->delete;
  }
  return;
}


# __SUB__ (feature "current_sub") is the sub currently executing.  Both
# spellings — the bare word and `CORE::__SUB__` — are rewritten at the shared
# PPI entry, so the answer is static and costs nothing at run time; the walk
# always stops at the INNERMOST enclosing sub.
#
#   NAMED sub  → `(\&name)`.  Its body or its signature default both count as
#     "lexically inside" (op/signatures.t t122: `sub f ($c = 5, $r = $c > 0 ?
#     __SUB__->($c-1) : "")` recurses through the default).  p-backslash-sub's
#     late binding keeps redefinition semantics.
#
#   ANON sub   → a source-level SELF-REFERENCE (task #378).  The closure has
#     no name to take a reference to, so give it one:
#
#       sub { … __SUB__ … }
#         → do { my $__SUB__N; $__SUB__N = sub { … $__SUB__N … }; $__SUB__N }
#
#     `my $w; $w = sub { $w->(…) }` is a shape PCL already compiles, so this
#     rides an existing path with no new mechanism, and `__SUB__ == $f` holds
#     because the variable holds the very coderef being built.  Anon subs that
#     do not mention the token are untouched.  (The rejected alternative was a
#     dynamic *pcl-current-sub* bound per anon call — a special bind on EVERY
#     call to serve a rare token.)  Before #378 these died in the runtime stub,
#     which cost op/sub.t 26 rows by aborting the file at its [perl #122845]
#     closure-recursion test.
sub _rewrite_current_sub {
  my ($doc) = @_;
  my $changed = 0;
  my (@blocks, %words_of);            # anon-sub block => its __SUB__ tokens

  for my $word (@{ $doc->find('PPI::Token::Word') || [] }) {
    next unless $word->parent;
    next unless ($word->content // '') =~ /^(?:CORE::)?__SUB__$/;
    my ($name, $block) = _current_sub_owner($word);
    if (defined $name) {
      _replace_element($word, "(\\&$name)");
      $changed = 1;
      next;
    }
    next unless $block;
    # refaddr, NOT the object: PPI overloads stringification to an element's
    # CONTENT, so a text key would both collide between two identical anon
    # subs and go STALE the moment a nested one is rewritten (measured: the
    # outer sub's own __SUB__ silently kept the runtime stub).
    push @blocks, $block unless $words_of{ refaddr $block };
    push @{ $words_of{ refaddr $block } }, $word;
  }

  # Innermost first: wrapping a block SERIALIZES it, so a nested anon sub must
  # already carry its own rewrite when its parent is re-emitted.  Depth is the
  # ancestor count — the same "the covering declaration with the latest start
  # wins" ordering #337's lexical-sub rename uses.
  my %depth = map { refaddr($_) => _ppi_depth($_) } @blocks;
  my $n = 0;                       # per-DOCUMENT: emission stays deterministic
  for my $block (sort { $depth{refaddr $b} <=> $depth{refaddr $a} } @blocks) {
    $changed = 1 if _wrap_anon_self_ref($block, $words_of{ refaddr $block }, $n++);
  }

  return $changed;
}

# Replace one element with the elements a fragment mini-parse produces.
sub _replace_element {
  my ($el, $text) = @_;
  my $ndoc = PPI::Document->new(\$text) or return 0;
  my @new = map { $_->isa('PPI::Statement') ? $_->children : $_ }
            $ndoc->children;
  $_->remove for @new;
  $el->insert_before($_) for @new;
  $el->delete;
  return 1;
}

sub _ppi_depth {
  my ($el) = @_;
  my $d = 0;
  $d++ while $el = $el->parent;
  return $d;
}

# The sub a `__SUB__` token belongs to: (NAME, undef) for a named sub,
# (undef, BLOCK) for an anonymous one, () when it is in no sub at all.
sub _current_sub_owner {
  my ($word) = @_;
  for (my $el = $word->parent; $el; $el = $el->parent) {
    # A signature/list directly under a sub statement, or the sub's block:
    # both mean "lexically inside that sub".
    my $par = $el->parent or last;
    if ($el->isa('PPI::Structure::Block')) {
      return ($par->name, undef)
        if $par->isa('PPI::Statement::Sub') && defined $par->name;
      return (undef, $el) if _anon_sub_word($el);
    }
    elsif (($el->isa('PPI::Structure::Signature')
            || $el->isa('PPI::Structure::List'))
           && $par->isa('PPI::Statement::Sub') && defined $par->name) {
      return ($par->name, undef);
    }
  }
  return ();
}

# The `sub` keyword heading an ANONYMOUS sub whose body is $block, or undef.
# Walks back over the signature/prototype/attribute tokens that may sit
# between the keyword and the `{`.
sub _anon_sub_word {
  my ($block) = @_;
  my $p = $block->sprevious_sibling;
  while ($p && ($p->isa('PPI::Structure::List')
             || $p->isa('PPI::Structure::Signature')
             || $p->isa('PPI::Token::Prototype')
             || $p->isa('PPI::Token::Attribute')
             || ($p->isa('PPI::Token::Operator') && $p->content eq ':'))) {
    $p = $p->sprevious_sibling;
  }
  return $p if $p && $p->isa('PPI::Token::Word') && $p->content eq 'sub';
  return undef;
}

# `sub SIG { … __SUB__ … }` → `do { my $__SUB__N; $__SUB__N = sub SIG { …
# $__SUB__N … }; $__SUB__N }`.  No parentheses around the `do`: `print sub
# {…}->()` would become `print (…)->()`, which perl reads as a call.
sub _wrap_anon_self_ref {
  my ($block, $words, $n) = @_;
  my $head = _anon_sub_word($block) or return 0;
  my $var  = "\$__SUB__$n";

  _replace_element($_, $var) for @$words;

  my @run;
  for (my $el = $head; $el; $el = $el->next_sibling) {
    push @run, $el;
    last if $el == $block;
  }
  return 0 unless @run && $run[-1] == $block;

  my $text = join '', map { $_->content } @run;
  return 0 unless _replace_element($run[0],
                                   "do { my $var; $var = $text; $var }");
  $_->delete for @run[1 .. $#run];
  return 1;
}

# The `:prototype(...)` attribute (perl 5.20+) declares a sub's prototype
# while the parenthesised list stays a signature.  PCL desugars it at the
# shared PPI entry into plain Perl that registers the prototype at runtime
# (p-__pcl_set_prototype, the registry p-prototype reads):
#
#   sub f :prototype($) ($a) { … }   →  sub f ($a) { … } __pcl_set_prototype(\&f, '$');
#   sub :prototype($) ($a) { … }     →  __pcl_set_prototype(sub ($a) { … }, '$')
#
# The attribute tokens are stripped either way — an attribute between `sub`
# and the signature otherwise derails the anon-sub signature desugar (the
# sig is no longer `sub`'s snext_sibling), which broke `my $c = sub
# :prototype($) ($a) {…}` wholesale (op/signatures.t t118).  Everything is
# spliced as tokens on the SAME line, so line numbers are preserved; the
# serialize+reparse in _ppi_parse rebuilds proper statement structure.
# Other attributes (:method, :lvalue, …) are left untouched.
sub _extract_prototype_attributes {
  my ($self, $doc) = @_;
  my $changed = 0;

  for my $attr (@{ $doc->find('PPI::Token::Attribute') || [] }) {
    next unless $attr->parent;                       # already spliced out
    my $c = $attr->content;
    next unless $c =~ /^prototype\((.*)\)$/s;
    my $proto = $1;
    (my $quoted = $proto) =~ s/([\\'])/\\$1/g;       # single-quote escape

    my $stmt = $attr->statement or next;
    if ($stmt->isa('PPI::Statement::Sub') && defined $stmt->name) {
      # Named sub: needs a body (forward decls keep their attribute).
      next unless grep { $_->isa('PPI::Structure::Block') } $stmt->schildren;
      my $name = $stmt->name;
      # Register at PARSE time too: perl applies :prototype at compile
      # time, so call-site parsing (unary vs list-op, signature-default
      # swallowing) must see it — the runtime registry alone is invisible
      # to the parser.  Marked from_attr so the definition's later default
      # registration knows not to clobber it (both pipelines).
      if ($self->environment) {
        my $si = $self->parse_prototype_or_signature($proto, $stmt);
        $si->{from_attr} = 1;
        $self->environment->add_prototype($name, $si);
      }
      _delete_attribute_and_colon($attr);
      my $text = " __pcl_set_prototype(\\&$name, '$quoted');";
      my $ndoc = PPI::Document->new(\$text) or next;
      my @el = map { $_->isa('PPI::Statement') ? $_->children : $_ }
               $ndoc->children;
      $_->remove for @el;
      my $anchor = $stmt;
      for my $e (@el) { $anchor->insert_after($e); $anchor = $e; }
      $changed = 1;
    }
    else {
      # Anonymous sub: walk back to the `sub` word, forward to its block.
      my $sub;
      for (my $p = $attr->sprevious_sibling; $p; $p = $p->sprevious_sibling) {
        if ($p->isa('PPI::Token::Word') && $p->content eq 'sub') { $sub = $p; last }
        last unless $p->isa('PPI::Token::Attribute')
                 || ($p->isa('PPI::Token::Operator') && $p->content eq ':');
      }
      next unless $sub;
      my ($block, $ok) = (undef, 1);
      for (my $p = $sub->snext_sibling; $p; $p = $p->snext_sibling) {
        if ($p->isa('PPI::Structure::Block')) { $block = $p; last }
        next if $p->isa('PPI::Token::Attribute')
             || ($p->isa('PPI::Token::Operator') && $p->content eq ':')
             || $p->isa('PPI::Structure::List')
             || $p->isa('PPI::Structure::Signature')
             || $p->isa('PPI::Token::Prototype');
        $ok = 0; last;
      }
      next unless $ok && $block;
      _delete_attribute_and_colon($attr);
      my @span;
      for (my $p = $sub; $p; $p = $p->next_sibling) {
        push @span, $p;
        last if $p == $block;
      }
      my $text = "__pcl_set_prototype("
               . join('', map { $_->content } @span) . ", '$quoted')";
      my $ndoc = PPI::Document->new(\$text) or next;
      my @el = map { $_->isa('PPI::Statement') ? $_->children : $_ }
               $ndoc->children;
      $_->remove for @el;
      $span[0]->insert_before($_) for @el;
      $_->delete for @span;
      $changed = 1;
    }
  }

  return $changed;
}

# Remove one attribute token; its introducing `:` operator goes too unless
# another attribute still follows it (`: prototype($) method` keeps the `:`).
sub _delete_attribute_and_colon {
  my ($attr) = @_;
  my $prev = $attr->sprevious_sibling;
  $attr->delete;
  if ($prev && $prev->isa('PPI::Token::Operator') && $prev->content eq ':') {
    my $after = $prev->snext_sibling;
    $prev->delete unless $after && $after->isa('PPI::Token::Attribute');
  }
  return;
}

# An ANONYMOUS sub's signature — `sub ($x, $y = 3, @rest) { BODY }` — is
# desugared here, at the shared document entry, into the plain-Perl code that
# binds it:
#
#   sub { die "Too few arguments…" if @_ < 1; my ($x,$y,@rest) = @_;
#         $y = (3) if @_ <= 1; BODY }
#
# WHY HERE, and why source-level.  A named sub's signature is lowered by
# _process_sub_statement (p-check-arity + a let* of p-copy-scalar-arg), but an
# anonymous sub in expression position reaches PExpr's handle_subcalls, which
# had no case for it at all: with `use feature "signatures"` in scope PPI hands
# the `($x)` back as a Structure::List, so `sub` looked like a CALL and the
# whole enclosing statement died "Fell through. Missing case" (op/signatures.t);
# without the pragma PPI hands back a Token::Prototype, which handle_subcalls
# silently DROPPED — the params never bound, which is worse than a parse error.
# Both parse routes converge on the PPI tree, so the desugar sits before either
# ever runs, and the resulting body binds its params through the ordinary
# `my (...) = @_` machinery instead of a second signature lowering.
#
# The prologue is emitted on ONE line, so it occupies the line the signature
# already occupied and every line of the body keeps its original number (die
# and warn report source lines).  A `#` anywhere in the generated text — only
# possible from inside a default expression — forces newline separation
# instead, since a comment would otherwise swallow the rest of the line.
sub _desugar_anon_signatures {
  my ($self, $doc) = @_;
  my $changed = 0;

  # Fixpoint: a desugared signature's DEFAULT EXPRESSION can itself contain a
  # signatured anon sub (`sub ($a, $t = sub ($p) {…}) {…}`, op/signatures.t
  # t132/t135).  The inner sub is spliced into the tree as new tokens AFTER
  # find() snapshotted the word list, so one pass never sees it — its
  # signature was silently dropped.  Re-scan until nothing changes; each round
  # consumes at least one signature, so this terminates.
  my $round_changed = 1;
  while ($round_changed) {
    $round_changed = $self->_desugar_anon_signatures_once($doc);
    $changed ||= $round_changed;
  }

  return $changed;
}

sub _desugar_anon_signatures_once {
  my ($self, $doc) = @_;
  my $changed = 0;

  for my $word (@{ $doc->find('PPI::Token::Word') || [] }) {
    # An earlier iteration's $sig->delete destroys the signature's children —
    # including a NESTED `sub` word from a default expression (its rewritten
    # copy was spliced in as new tokens) — so a stale find-list entry can be a
    # destroyed token: no parent, undef content.
    next unless $word->parent && ($word->content // '') eq 'sub';
    my $sig = $word->snext_sibling or next;
    # A NAMED sub has its name here instead, and keeps the named-sub path.
    # PPI hands the parameter list back as a Structure when it saw the
    # signatures feature enabled (`use feature "signatures"`, `use v5.36`,
    # `use experimental "signatures"`) and as a Token::Prototype when it did
    # not.  Both shapes arrive here because PPI's view is not the whole story:
    # its pragma tracking only takes effect on the NEXT LINE (a one-liner
    # `use feature "signatures"; my $c = sub ($x) {…}` yields a Prototype),
    # and a string eval inherits the feature from a scope PPI never sees.
    my $is_struct = $sig->isa('PPI::Structure::List')
                 || $sig->isa('PPI::Structure::Signature');
    next unless $is_struct || $sig->isa('PPI::Token::Prototype');
    my $block = $sig->snext_sibling or next;
    next unless $block->isa('PPI::Structure::Block');

    # So the Structure shape is taken as authoritative — it IS a signature,
    # including `()` (arity 0) and `($, $)` (anonymous mandatory slots) — and a
    # Prototype is read with the SAME textual discriminator the named-sub path
    # uses (parse_prototype_or_signature): a NAMED parameter means signature,
    # anything else is a real prototype ((&@), ($$), ()) which binds nothing and
    # is dropped downstream.  Consequence, shared with named subs: signature
    # syntax with the feature genuinely off is read as a signature, where perl
    # reads a prototype — see docs/not-supported.md §Signature syntax.
    my $text = $sig->content;
    next if !$is_struct && $text !~ /[\$\@\%]\w/;

    my $prologue = $self->_anon_signature_prologue($text);
    next unless defined $prologue;

    my $pdoc = PPI::Document->new(\$prologue) or next;
    my @pel  = $pdoc->children;
    next unless @pel;
    $_->remove for @pel;

    # Prepend: detach the body, add the prologue, re-add the body.  add_element
    # only accepts parentless elements, so the detach has to come first.
    my @body = $block->children;
    $_->remove for @body;
    $block->add_element($_) for @pel, @body;
    $sig->delete;
    $changed = 1;
  }

  return $changed;
}


# The Perl source that binds one anonymous sub's signature, or undef if the
# signature declares nothing at all.  Shares _signature_param_specs with the
# named-sub lowering, so placeholder naming and default-operator handling
# cannot drift between the two.
sub _anon_signature_prologue {
  my ($self, $sig_text) = @_;

  my $inner = $sig_text;
  $inner =~ s/^\s*\(\s*//;
  $inner =~ s/\s*\)\s*$//;

  my $specs = $self->_signature_param_specs($inner);
  # Text that declares nothing recognisable is not a signature we can bind —
  # e.g. `($$)`, which is a syntax error under the feature and an ordinary
  # prototype without it.  Leave it to the prototype drop rather than emit an
  # arity-0 check that would reject every call.
  return undef if !@$specs && $inner ne '';
  my @names = map { $_->{name} } @$specs;

  # Arity bounds, mirroring p-check-arity: min counts the leading mandatory
  # scalars, max is unbounded once a slurpy @/% param appears.
  my $min = 0;
  my $seen_optional = 0;
  for my $spec (@$specs) {
    $seen_optional = 1 if defined $spec->{default_expr};
    $min++ if !$seen_optional && $spec->{name} !~ /^[\@\%]/;
  }
  my $slurpy = (@names && $names[-1] =~ /^[\@\%]/) ? 1 : 0;
  my $max    = $slurpy ? undef : scalar(@names);
  # Perl words a sub that can take a range of counts as "at least"/"at most".
  my $flex   = ($slurpy || $seen_optional) ? 1 : 0;

  my @stmts;
  my $got = q{scalar(@_)};
  my $who = q{"'" . __PACKAGE__ . "::__ANON__'"};
  if ($min > 0) {
    my $want = ($flex ? 'at least ' : '') . $min;
    push @stmts, qq{die "Too few arguments for subroutine " . $who .}
               . qq{ " (got " . $got . "; expected $want)" if \@_ < $min;};
  }
  if (defined $max) {
    my $want = ($flex ? 'at most ' : '') . $max;
    push @stmts, qq{die "Too many arguments for subroutine " . $who .}
               . qq{ " (got " . $got . "; expected $want)" if \@_ > $max;};
  }

  # A slurpy %hash must receive an EVEN number of leftover args (perl dies
  # "Odd name/value argument ..."), mirroring p-check-arity's hash-start.
  if ($slurpy && $names[-1] =~ /^\%/) {
    my $hash_start = scalar(@names) - 1;
    push @stmts, qq{die "Odd name/value argument for subroutine " . $who}
               . qq{ if \@_ > $hash_start && (\@_ - $hash_start) % 2;};
  }

  push @stmts, 'my (' . join(', ', @names) . ') = @_;' if @names;

  # Defaults.  `=` applies only when the argument is ABSENT (index test);
  # `//=` and `||=` (perl 5.38+) additionally apply to an undef/false argument,
  # which is exactly what the plain Perl operators already mean.
  my $idx = 0;
  for my $spec (@$specs) {
    my $expr = $spec->{default_expr};
    $idx++, next if !defined $expr;
    my $name = $spec->{name};
    if ($spec->{default_op} eq '=') {
      push @stmts, "$name = ($expr) if \@_ <= $idx;";
    } else {
      push @stmts, "$name $spec->{default_op} ($expr);";
    }
    $idx++;
  }

  return undef if !@stmts;
  my $sep = (grep { /#/ } @stmts) ? "\n" : ' ';
  return join($sep, @stmts) . $sep;
}

# PPI's Symbol regex is ASCII-bounded: a unicode variable name splits into
# Cast + Word ($ᕘ → `$` + `ᕘ`) or Symbol + Word ($main::ᕘ → `$main::` + `ᕘ`).
# The stray Word then parses as a bareword — worst case a CALL of a
# same-named sub ($ᕘ inside `sub ᕘ` = infinite recursion, uni/gv.t).  Merge
# the abutting fragments back into one Symbol.  Runs on the FINAL document
# (a serialize+reparse would just re-split); mutation is token-local.
# Adjacency is required — an intervening whitespace token blocks the merge —
# and in valid perl a bareword never directly abuts a variable name.
sub _merge_unicode_symbols {
  my ($doc) = @_;
  my @repaired;
  for my $cast (@{ $doc->find('PPI::Token::Cast') || [] }) {
    next unless $cast->content =~ /^(?:[\$\@\%\&\*]|\$\#)$/;
    my $next = $cast->next_sibling;
    next unless $next && $next->isa('PPI::Token::Word');
    my $sym = PPI::Token::Symbol->new($cast->content . $next->content);
    $cast->insert_before($sym);
    $next->delete;
    $cast->delete;
    push @repaired, $sym;
  }
  for my $sym (@{ $doc->find('PPI::Token::Symbol') || [] }) {
    my $merged = 0;
    while (1) {
      my $next = $sym->next_sibling;
      last unless $next && $next->isa('PPI::Token::Word');
      last unless $sym->content =~ /::$/
               || $next->content =~ /^::/
               || $next->content =~ /^[^\x00-\x7F]/;
      $sym->add_content($next->content);
      $next->delete;
      $merged = 1;
    }
    push @repaired, $sym if $merged;
  }
  _reclass_subscripts_after($_) for @repaired;
  return 1;
}

# PUNCTUATION-NAMED ARRAYS (`@?`, `@!`, `@.`, …).  perl lets any punctuation
# character name a global, and `@?` is legal perl that real code writes
# (`ok( ! @?, … )`, t/re/subst.t:346 — one of the #415 census drops).  PPI
# has no Magic token for them and its Symbol regex is word-bounded, so `@?`
# comes out as Cast('@') + Operator('?') and the statement was DROPPED
# (docs/ppi-upstream-bugs.md §24).
#
# The merge is unambiguous: in valid perl a `@` CAST must be followed by `$`,
# `{` or an identifier, so a Cast immediately abutting an OPERATOR can only
# ever be a mis-lexed punctuation name.  Adjacency is still required, as in
# _merge_unicode_symbols.
#
# %PUNCT_ARRAY_CHARS is the set this repair covers, and the boundary is the
# CL SYMBOL SPELLING, not perl: these characters are CL constituents, so the
# emitted symbol reads BARE the way the sibling `@#` already does.  The rest
# of perl's punctuation names (`@,` `@;` `@|` `@'` `@"` `@(` — every CL macro
# or escape character) would need a pipe-quoted spelling, which is its own
# emission rule; they keep DROPPING, loudly, and are filed as task #449.
my %PUNCT_ARRAY_CHARS = map { $_ => 1 } split //, q{?!./~^&%=<>};

sub _merge_punct_array_symbols {
  my ($doc) = @_;
  my @repaired;
  for my $cast (@{ $doc->find('PPI::Token::Cast') || [] }) {
    next unless $cast->content eq '@';
    my $next = $cast->next_sibling;
    next unless $next && $next->isa('PPI::Token::Operator');
    next unless length($next->content) == 1 && $PUNCT_ARRAY_CHARS{$next->content};
    my $sym = PPI::Token::Symbol->new('@' . $next->content);
    $cast->insert_before($sym);
    $next->delete;
    $cast->delete;
    push @repaired, $sym;
  }
  _reclass_subscripts_after($_) for @repaired;
  return 1;
}

# The LEXER had already decided what the `{…}` / `[…]` after one of those
# fragments was, and it decided it from the BAREWORD it saw there: `$Ｊ{a}`
# came out as a BLOCK and `$Ｖ[0]` as an anonymous-array CONSTRUCTOR, never as
# the SUBSCRIPT each one is (task #410; uni/gv.t, uni/stash.t, uni/caller.t and
# the two mro utf8 files drop 21 statements on it).  Merging the tokens does not
# move the tree, so re-class the postfix chain that follows a repaired symbol.
# Text and tokens are untouched — only the two container CLASSES change, which
# is exactly what PPI::Lexer would have chosen had the symbol been whole.
# An explicit `->` was lexed correctly (PPI reads a subscript after an arrow
# whatever precedes it), so it is stepped over, not re-classed.
# The walk steps over WHITESPACE (`snext_sibling`), because PPI's own lexer
# does: `$h {a}` and `@h {qw(a b)}` are Subscripts in the ASCII spelling
# (dumped, PPI 1.291), so the repaired spelling has to agree or `$Ｘ {a}`
# reads as a BLOCK — which made `print $Ｘ {a}` a block-form FILEHANDLE spec
# and `print $Ｖ [1]` an anonymous array (task #422 item 2).  The mirror is
# exact: this pass only ever re-decides what PPI decided from a bareword it
# should never have seen.
sub _reclass_subscripts_after {
  my ($sym) = @_;
  my $node = $sym;
  while (1) {
    my $next = $node->snext_sibling or last;
    if ($next->isa('PPI::Token::Operator') && $next->content eq '->') {
      $node = $next;
      next;
    }
    last unless $next->isa('PPI::Structure') && $next->start;
    last unless $next->start->content eq '{' || $next->start->content eq '[';
    if (!$next->isa('PPI::Structure::Subscript')) {
      bless $next, 'PPI::Structure::Subscript';
      for my $kid ($next->children) {
        bless $kid, 'PPI::Statement::Expression' if ref($kid) eq 'PPI::Statement';
      }
    }
    $node = $next;
  }
  return;
}

sub _build_ppi_doc {
  my $self = shift;

  if ($self->has_filename) {
    open(my $fh, '<', $self->filename)
      or die "Failed to open file: " . $self->filename;
    my $src = _preprocess_source(_maybe_decode_utf8(do { local $/; <$fh> }));
    close $fh;
    my $doc = $self->_ppi_parse($src);
    return $doc if $doc;
    die "Failed to parse file: " . $self->filename;
  }
  elsif ($self->has_code) {
    my $code = _preprocess_source(_maybe_decode_utf8($self->code));
    my $doc = $self->_ppi_parse($code);
    return $doc if $doc;
    die "Failed to parse code";
  }
  else {
    die "Must provide either 'filename' or 'code'";
  }
}



# (v1's file-level `parse()` — the whole-file transpile entry until E4.1
# step 3, then the prototype-collection walker only — is DELETED at s412,
# task #391: its last callers, the two prototype extractors below, walk the
# document for FACTS instead.  Nothing constructs a whole-file v1 emission
# any more; the per-statement/expression code stays reachable through
# Parser2's seam only.)

# PROTOTYPE COLLECTION as a FACTS WALK (task #391, s412).  What a use'd
# module contributes to its user is exactly two things — the prototype
# records of the subs it declares (`sub NAME (PROTO)`, signatures, `use
# constant` zero-arg terms, the `:prototype` attribute — and, transitively,
# what ITS use'd modules contribute) and its @EXPORT names — and until s412
# they were gathered by running v1's ENTIRE statement pipeline over the
# module with emission suppressed (`parse()` in collect_prototypes_only
# mode): every statement processed, every expression parsed and generated,
# every block compiled, for a walk whose consumers read `prototypes` and
# `export_names` and nothing else.  Measured s412: 26 % of whole-corpus
# compile time (13.1 s of a 50 s sample; perl-tests' 3 000-line test.pl was
# fully compiled on every transpile).
#
# This walk visits the document in ORDER (the same depth-first source order
# v1's walk registered things in — a nested named sub or a `use` inside a
# body is reached when its enclosing statement is) and reads only the facts:
#   PPI::Statement::Sub      → the head (_sub_head / _sub_sig_info /
#                              _register_sub_prototype — the SAME helpers the
#                              definition uses, one copy), then its body for
#                              nested declarations;
#   PPI::Statement::Include  → v1's own include handler, which is where `use
#                              constant`, `use lib` (the shared @INC list),
#                              `use Module LIST` (extract + merge, recursing
#                              into that module) and `require "file"` live —
#                              its emission is a no-op here;
#   any other node           → recurse (BEGIN blocks, compounds, expression
#                              statements whose anon-sub bodies declare
#                              named subs).
# Nothing else in a module can add a prototype: the four add_prototype sites
# are the sub definition, the :prototype attribute pre-pass (run by _ppi_parse
# when the document is built), `use constant`, and the module merge.
sub collect_prototypes {
  my ($self, $doc) = @_;
  # v1's walk reset these for its own emission; here nothing is emitted, but
  # `use base`/overload/lib handlers still read the package stack.
  $self->environment->package_stack([$self->eval_pkg // 'main']);
  $self->_sections([]);
  $self->_cur_bucket('runtime');
  $self->_open_section('pcl');
  $self->_walk_prototype_facts($doc);
  return 1;
}

sub _walk_prototype_facts {
  my ($self, $node) = @_;
  for my $child ($node->schildren) {
    # (PPI::Statement::Scheduled — BEGIN/END/… — ISA Statement::Sub; it is a
    # block, not a declaration: recurse only.)
    if ($child->isa('PPI::Statement::Sub') && !$child->isa('PPI::Statement::Scheduled')) {
      my ($name, $prototype, $is_sig_syntax, $block) = $self->_sub_head($child);
      my $sig_info = $self->_sub_sig_info($child, $prototype, $is_sig_syntax);
      $self->_register_sub_prototype($child, $name, $sig_info, $prototype, $is_sig_syntax);
      $self->_walk_prototype_facts($block) if $block;
    }
    elsif ($child->isa('PPI::Statement::Include')) {
      $self->_process_include_statement($child);
    }
    elsif ($child->isa('PPI::Node')) {
      $self->_walk_prototype_facts($child);
    }
  }
}

# Transform Perl qualified sub name to CL format
# Perl: A::DESTROY -> CL: A::pl-DESTROY
# Perl: Hash::Util::func -> CL: |Hash::Util|::pl-func
# Perl: Class::DESTROY -> CL: |Class|::pl-DESTROY (avoid CL conflict)
# Perl: simple_sub -> CL: pl-simple_sub
sub _qualified_sub_to_cl {
  my ($self, $name) = @_;
  # Perl allows ' as package separator (old style): BASEOBJ'doit == BASEOBJ::doit
  # Convert to :: before processing
  $name =~ s/'/::/g;
  if ($name =~ /^(.+)::([^:]+)$/) {
    my ($pkg, $bare) = ($1, $2);
    # Only MULTI-segment names need pipe-quoting; single-segment names are
    # upcased by the reader (matching the runtime's perl-pkg-to-cl-pkg-name).
    # MUST agree with _cl_pkg_designator — single source of truth for the rule.
    # (The old class/error/method/function special-case is obsolete: CLOS class
    #  names are plc-prefixed now, so the package name needs no escaping.)
    (my $cl_pkg = $self->_cl_pkg_designator($pkg)) =~ s/^://;
    # Register package so it gets pre-declared
    $self->environment->add_referenced_package($pkg) if $self->environment;
    return "${cl_pkg}::" . cl_sym("pl-$bare");
  }
  return cl_sym("pl-$name");
}

# ============================================================
# Output bucket system helpers
# ============================================================

# Open a new output section for a package (called from _emit_package_preamble).
# Each section holds four named buckets assembled in order:
#   preamble → declarations → definitions → runtime
sub _open_section {
  my ($self, $pkg_name) = @_;
  push @{$self->_sections}, {
    pkg          => $pkg_name,
    preamble     => [],
    declarations => [],
    definitions  => [],
    runtime      => [],
  };
  $self->_cur_section($#{$self->_sections});
}

# Temporarily switch to a named bucket, run $code, then restore.
sub _with_bucket {
  my ($self, $bucket, $code) = @_;
  my $old = $self->_cur_bucket;
  $self->_cur_bucket($bucket);
  $code->();
  $self->_cur_bucket($old);
}

# ---------------------------------------------------------------- the v2 seam
#
# capture_v1 — THE seam function (docs/plan-one-compiler-s411.md Phase B2;
# E5.1 as a function, not an object).  Pl::Parser2 lowers a statement it has
# no native arm for (the ~12 v1 classes + `local`) and an embedded block the
# structural route declines by running v1's machinery ON THIS PARSER — whose
# emission is text into the section buckets.  This function isolates that:
# it saves the parser's emission state (sections, current bucket, indent,
# open `local` depth, block depth), installs a fresh scratch section and the
# caller's choices, runs $code, DRAINS every bucket of every scratch section
# by name, restores the saved state and returns
#
#   { result  => what $code returned,
#     runtime => [lines the statement itself emitted],
#     decls   => [preamble + declarations lines — defvars, package forms],
#     defs    => [definitions lines — hoisted sub/BEGIN definitions],
#     opens   => open `local` let forms the runtime text leaves unclosed }
#
# %opt: bucket => the bucket _emit writes to during $code ('runtime' for a
#       statement; 'definitions' for a block compile whose hoists must land
#       at a section top), block_depth => v1's `_block_depth` for the run
#       (several bucket decisions key on it — a `require` inside a block
#       stays inline), hook => the `_v2_embed` block hook to install.
# Parser2 never reads or writes those five fields itself: it calls this.
sub capture_v1 {
  my ($self, $code, %opt) = @_;
  my @saved = ($self->_sections, $self->_cur_section, $self->_cur_bucket,
               $self->indent_level, $self->{_local_let_depth}, $self->_block_depth);
  my $bucket = $opt{bucket} // 'runtime';
  $self->_block_depth($opt{block_depth}) if defined $opt{block_depth};
  $self->_sections([]);
  $self->_open_section('pcl');
  $self->_cur_bucket($bucket);
  $self->indent_level(0);
  $self->{_local_let_depth} = 0;
  my $result = do {
    local $self->{_v2_embed} = $opt{hook} if $opt{hook};
    $code->();
  };
  my %out = (result => $result, opens => $self->{_local_let_depth},
             runtime => [], decls => [], defs => []);
  for my $sec (@{ $self->_sections }) {
    push @{ $out{decls} },   grep { /\S/ } @{$sec->{preamble}}, @{$sec->{declarations}};
    push @{ $out{defs} },    grep { /\S/ } @{$sec->{definitions}};
    push @{ $out{runtime} }, grep { /\S/ } @{$sec->{runtime}};
  }
  $self->_sections($saved[0]);
  $self->_cur_section($saved[1]);
  $self->_cur_bucket($saved[2]);
  $self->indent_level($saved[3]);
  $self->{_local_let_depth} = $saved[4];
  $self->_block_depth($saved[5]);
  return \%out;
}


# embed_block — how THIS parser compiles an expression-embedded block
# (Phase B3: PExpr's block sites ask exactly this, one route).  $kind is
# 'map' | 'grep' | 'sort' | 'eval' — the answer is an arrayref of BODY forms
# for the inline lambda — or 'do' | 'sub' — the answer is the whole LAMBDA
# form.  A parser serving as Pl::Parser2's seam carries Parser2's hook in
# `_v2_embed` (installed by capture_v1 / _lower_expr around every lowering
# parse), and the hook ALWAYS answers: structurally when it can, else through
# embed_block_v1 below inside its own capture.  Without a hook (a parser with
# no Parser2 above it — the prototype-collection walk of a use'd module,
# v1's constant/default-expression compiles) the answer is v1's own text.
sub embed_block {
  my ($self, $block, $kind) = @_;
  if (my $hook = $self->{_v2_embed}) {
    return $hook->($block, $kind);
  }
  warn "pcl-raw\tdecl:no-hook\n" if $ENV{PCL_E2_RAW_CENSUS};
  return $self->embed_block_v1($block, $kind);
}

# v1's text compile of an embedded block, as the forms embed_block promises:
# the body kinds through parse_block_to_cl_string (a map/grep hash-constructor
# block `map({k => $_}, …)` through its dedicated route), do{} and sub{} through
# parse_block_as_function as a returned lambda — do{} is a plain 0-arg block
# whose body is loop-transparent (an unlabeled last/next inside it reaches
# the enclosing loop, as in perl); a sub receives call arguments via @_.
# The text rides as ONE raw form (E2's residue rule).  parse_block_as_function
# pushes the hoists it finds inside the block (a `use`, a BEGIN, an `our`
# defvar) into the CURRENT section — a seam parser reaches this only inside
# capture_v1, whose drain carries them to Parser2's _captured_decls.
sub embed_block_v1 {
  my ($self, $block, $kind) = @_;
  if ($kind eq 'sub') {
    return Pl::CLForm::raw($self->parse_block_as_function($block, [], 1, 1));
  }
  if ($kind eq 'do') {
    return Pl::CLForm::raw($self->parse_block_as_function($block, [], 0, 1, 1));
  }
  # (map/grep only — after eval the braces are always a BLOCK; see
  # Parser2::_lower_embedded_body.)
  my $text = ($kind eq 'map' || $kind eq 'grep')
             && Pl::PExpr::_block_is_hash_constructor($block)
    ? $self->parse_hash_block_to_cl_string($block)
    : $self->parse_block_to_cl_string($block, $kind);
  return [Pl::CLForm::raw($text)];
}

# The seam's standing scratch section (become_seam) must be EMPTY at the end
# of every Parser2 parse: v1 text is produced on a seam parser only inside
# capture_v1, whose drain hands it back — a line left here is emission nobody
# drained, i.e. silently lost output (rule 12).
sub assert_seam_clean {
  my ($self) = @_;
  for my $sec (@{ $self->_sections }) {
    for my $b (qw(preamble declarations definitions runtime)) {
      my ($line) = grep { /\S/ } @{ $sec->{$b} };
      die "PCL internal: v1 emission on the seam parser outside capture_v1 "
        . "($b): $line\n" if defined $line;
    }
  }
}



# A top-level `local $x = ...;` in Perl puts the whole rest of the enclosing
# block into the dynamic extent of the local, so PCL wraps every subsequent
# statement in one `(let (($x ...)) ...)`.  At file/package scope that can be
# thousands of lines — a single enormous CL function.  R1 declaims the hot
# fast-path operators `inline`, and inlining even a handful of type-dispatch
# diamonds into a function that large makes SBCL's constraint propagation blow
# up superlinearly (measured 1.2 GB compiling local.t's tail form, OOM-killing
# the default 1 GB heap — the s268 R1 crash regression).  Such a form is
# cold top-level code that runs once, so inlining buys nothing there anyway.
# Wrap any oversized top-level runtime form in a `(locally (declare (notinline
# ...)))` so the inline proclamation is suppressed inside it; compilation drops
# back to ~95 MB while hot code elsewhere keeps open-coding the fast paths.
# The hot fast-path operators/accessors that R1 declaims `inline` in the
# runtime (must match the `(declaim (inline ...))` at the end of
# cl/pcl-runtime.lisp).  A `(declare (notinline ...))` naming these overrides
# the global inline proclamation for one lexical scope.
sub _notinline_ops_decl {
  my $ops = join(' ', map { "pcl::$_" } qw(
    p-+ p-- p-* p-/ p-% p-== p-!= p-< p-> p-<= p->= p-<=>
    p-. p-str-eq p-str-ne p-str-lt p-str-gt p-str-le p-str-ge p-str-cmp
    unbox to-number to-string p-true-p p-bool %pcl-nan-p));
  return "(declare (notinline $ops))";
}

my $HUGE_FORM_CHARS = 20000;
sub _cap_inlining_if_huge {
  my ($form, $size) = @_;
  # $size: optional layout-invariant measure of the form (whitespace
  # collapsed).  The v2 assembly passes it because the structural printer's
  # depth indentation inflates length($form) far past the v1-flat text this
  # threshold was calibrated against (E2.final root flip); SBCL's compile
  # cost tracks the form's content, not its indentation.
  return $form unless ($size // length($form)) > $HUGE_FORM_CHARS;
  # Only wrap plain expression forms; never wrap a top-level definition
  # (eval-when / p-sub / defvar / defpackage), since (locally ...) would strip
  # its top-level-ness and break compile-time visibility.
  return $form if $form =~ /\A\s*\((?:eval-when|pcl:p-sub|pcl:p-defpackage|p-sub|defvar|defparameter|defun|in-package)\b/;
  return "(locally " . _notinline_ops_decl() . "\n$form)";
}

# Wrap the minimal set of top-level runtime lines that participate in a
# goto/label pair in individual (tagbody ...) forms.
#
# Real generated labels are marked with a ";; pcl-label" sentinel so we can
# distinguish them from ":word" patterns inside CL string literals.
#
# @rt elements can be multi-line strings.  A goto counts as "top-level" only
# when it is reachable by CL's lexically-scoped (go ...) — meaning it must
# NOT be inside a lambda or named function scope.  We detect this by:
#   - Skipping @rt elements that start with whitespace (indented = nested).
#   - Skipping @rt elements that are p-sub/eval-when definitions.
#   - Skipping a (go :LABEL) match if the text preceding it within the same
#     @rt element contains "lambda" (i.e. the goto is inside a lambda body).
#
# Algorithm:
#   1. Find each ":LABEL  ;; pcl-label" element (real label, first occurrence).
#   2. Find the last qualifying (go :LABEL) for each label.
#   3. Build [min(label_pos, last_goto_pos), max(...)] ranges, merge overlaps.
#   4. Wrap each range in (tagbody ...), hoisting definition elements out.
#   5. Everything outside the ranges is emitted as independent top-level forms.
# Scan a list of generated CL lines and return, for the START of each line:
#   { depth => <paren depth>, in_lambda => <bool: inside a nested lambda/p-sub> }
# plus a trailing entry (index == #lines) holding the final paren depth.
# String-, comment- and #\char-literal-aware (mirrors the paren checker in
# CLAUDE.md).  A line is "in_lambda" when a `(lambda` or `(p-sub` form opened
# earlier and has not yet closed — used to exclude gotos in nested function
# scopes (CL `go` cannot lexically reach a tag across a lambda boundary).
sub _scan_lisp_lines {
  my $lines = shift;
  my @info;
  my $depth = 0;
  my @lambda_at;   # stack of paren depths at which a lambda/p-sub opened
  for my $i (0 .. $#$lines) {
    push @info, { depth => $depth, in_lambda => (@lambda_at ? 1 : 0) };
    my @c = split //, $lines->[$i];
    my ($j, $in_str) = (0, 0);
    while ($j <= $#c) {
      my $ch = $c[$j];
      if ($in_str) {
        if    ($ch eq '\\') { $j += 2; next; }
        elsif ($ch eq '"')  { $in_str = 0; }
        $j++; next;
      }
      if    ($ch eq '"') { $in_str = 1; }
      elsif ($ch eq ';') { last; }                         # comment to EOL
      elsif ($ch eq '#' && $j < $#c && $c[$j+1] eq '\\') { $j += 3; next; } # #\X
      elsif ($ch eq '(') {
        my $rest = join '', @c[$j+1 .. $#c];
        push @lambda_at, $depth if $rest =~ /^\s*(?:lambda|p-sub)\b/;
        $depth++;
      }
      elsif ($ch eq ')') {
        $depth--;
        pop @lambda_at while @lambda_at && $lambda_at[-1] >= $depth;
      }
      $j++;
    }
  }
  push @info, { depth => $depth, in_lambda => (@lambda_at ? 1 : 0) };
  return \@info;
}

# Wrap the minimal run of COMPLETE top-level forms that participate in a
# goto/label pair in a (tagbody ...).  Works at form granularity (not line
# granularity) so a goto nested inside a multi-line form (e.g. inside a
# `(p-if … (progn (go :X)))`) is wrapped together with its enclosing form,
# never splitting parens.  Used for both top-level runtime lines and sub
# bodies (lines may be indented).
# The two spellings a LABEL token can have in emitted text: the bare ASCII
# identifier, or the |…| quoted form a name carrying a non-ASCII character
# takes (#418, Pl::CLForm::cl_sym).  This pass matches the emitted TEXT, so it
# has to know both, or a `use utf8` label's goto/label pair goes unrecognised
# and the tagbody wrap never happens (perl's own t/uni/labels.t).
our $LABEL_TOK = qr/[A-Za-z][A-Za-z0-9_]*|\|[^|]*\|/;

sub _wrap_runtime_labels {
  my $rt_ref = shift;
  my @rt = @$rt_ref;

  # Quick exit: no real label sentinels (allow leading indentation).
  return @rt unless grep { /^\s*:$LABEL_TOK\s*;; pcl-label/ } @rt;

  # Definition lines must stay outside any tagbody (eval-when etc. need
  # top-level context).  Allow leading indentation (sub bodies are indented).
  my $is_definition = sub {
    $_[0] =~ /^\s*\((?:p-sub|eval-when|defvar|defpackage|in-package|p-defpackage|p-BEGIN)\b/;
  };

  # Group lines into complete forms: [start_line, end_line].  A form closes
  # when the running paren depth returns to 0.  Depth-0 blank/comment lines
  # become their own trivial single-line forms.
  my $info = _scan_lisp_lines(\@rt);
  my @forms;
  my $start;
  for my $i (0 .. $#rt) {
    $start = $i unless defined $start;
    if ($info->[$i + 1]{depth} == 0) {     # depth after line $i
      push @forms, [ $start, $i ];
      $start = undef;
    }
  }
  push @forms, [ $start, $#rt ] if defined $start;   # unbalanced tail (defensive)

  # Per-form: label name (if any), and the set of qualifying goto labels.
  my %label_first;   # label name → first form index defining it
  my %last_goto;     # label name → last form index with a reachable goto
  for my $fi (0 .. $#forms) {
    my ($s, $e) = @{$forms[$fi]};
    for my $i ($s .. $e) {
      # A label only belongs to THIS region when it is a direct sibling
      # (paren depth 0).  A label nested inside a child form (depth > 0) is
      # handled by that child's own _process_block wrapping pass.
      if ($info->[$i]{depth} == 0
          && $rt[$i] =~ /^\s*:($LABEL_TOK)\s*;; pcl-label/) {
        $label_first{$1} //= $fi;
      }
      next if $info->[$i]{in_lambda};   # goto inside a nested lambda → unreachable
      while ($rt[$i] =~ /\(go\s+:($LABEL_TOK)\)/g) {
        my $lbl    = $1;
        my $prefix = substr($rt[$i], 0, $-[0]);
        next if $prefix =~ /\b(?:lambda|p-sub)\b/;  # opened+used on same line
        $last_goto{$lbl} = $fi;
      }
    }
  }

  # Keep only labels that have a matching reachable goto.
  delete $label_first{$_} for grep { !exists $last_goto{$_} } keys %label_first;
  return @rt unless %label_first;

  # Minimal [start_form, end_form] ranges covering each label and its goto.
  my @ranges;
  for my $lbl (keys %label_first) {
    my ($lf, $gf) = ($label_first{$lbl}, $last_goto{$lbl});
    push @ranges, [ ($lf < $gf ? $lf : $gf), ($lf > $gf ? $lf : $gf) ];
  }
  @ranges = sort { $a->[0] <=> $b->[0] } @ranges;
  my @merged;
  for my $r (@ranges) {
    if (@merged && $r->[0] <= $merged[-1][1] + 1) {
      $merged[-1][1] = $r->[1] if $r->[1] > $merged[-1][1];
    } else {
      push @merged, [ $r->[0], $r->[1] ];
    }
  }

  # Assemble result, wrapping each form range in (tagbody ...) and hoisting
  # definition forms out of the tagbody.
  my @result;
  my $fpos = 0;
  for my $region (@merged) {
    my ($fs, $fe) = @$region;
    # Forms before this region, emitted as-is.
    push @result, map { @rt[$forms[$_][0] .. $forms[$_][1]] } ($fpos .. $fs - 1)
      if $fs > $fpos;
    my @tb;
    for my $fi ($fs .. $fe) {
      my @flines = @rt[$forms[$fi][0] .. $forms[$fi][1]];
      if ($is_definition->($rt[$forms[$fi][0]])) {
        push @result, "(tagbody", @tb, ")" if @tb;
        @tb = ();
        push @result, @flines;
      } else {
        push @tb, @flines;
      }
    }
    push @result, "(tagbody", @tb, ")" if @tb;
    $fpos = $fe + 1;
  }
  # Forms after the last region.
  push @result, map { @rt[$forms[$_][0] .. $forms[$_][1]] } ($fpos .. $#forms)
    if $fpos <= $#forms;

  return @result;
}

# DELETED: _insert_sub_forward_declarations (replaced by bucket routing)
# DELETED: _reorder_compile_runtime_forms   (replaced by bucket ordering)
# DELETED: _parse_output_chunks             (no longer needed)
# DELETED: _is_compile_time_form            (no longer needed)
# DELETED: _insert_package_predeclarations  (folded into _assemble_output)


# Insert defvar for package variables used without my/our declaration.
# Scans all output buckets; pushes defvars into first section's declarations.
# ── AST-level free-variable detection for eval "STRING" bodies ───────────────
#
# A free variable is one referenced inside the eval but declared NOWHERE in its
# enclosing eval scope chain — it must be captured from the caller (it becomes a
# p-eval-thunk lambda parameter; see docs/eval-lexical-capture.md).  Unlike the
# old post-codegen regex this is SCOPE-AWARE and descends into NAMED subs (the
# Class::Method::Modifiers idiom: eval "sub $name { ... \$wrapped ... }").  Works
# on the PPI parse tree, not generated CL.  See docs/eval-free-vars-plan.md.

# Globals that are NOT capturable lexicals but which PPI classifies as PLAIN
# PPI::Token::Symbol (so the Magic-token filter below does not catch them).
# Punctuation/magic specials ($_, @_, $@, $0, $1.., $., $!, $/, $&, $;, $^W, ...)
# are all PPI::Token::Magic and are excluded by type in _eval_scope_parts.
my %EVAL_RUNTIME_VARS = map { $_ => 1 } qw(
  @ARGV @INC %ENV %INC %SIG $$ $? %_args
);

sub _eval_free_vars_from_ppi {
  my ($self, $doc) = @_;
  return {} unless $doc;
  my %free;
  $self->_eval_scope_free($doc, {}, \%free);
  delete $free{$_} for ('$a', '$b');   # $a/$b handled specially by the caller
  return \%free;
}

# Analyze one lexical scope ($root) given the names bound by enclosing scopes.
# Collects this scope's own declarations, flags free refs, recurses into nested
# subs (each carrying the enclosing+local bound set).
sub _eval_scope_free {
  my ($self, $root, $enclosing, $free) = @_;
  my (@decls, @refs, @subs);
  $self->_eval_scope_parts($root, \@decls, \@refs, \@subs);
  my %bound = (%$enclosing, map { $_ => 1 } @decls);
  for my $v (@refs) {
    next if $bound{$v};
    next if $EVAL_RUNTIME_VARS{$v};
    next if $v =~ /::/;                 # package-qualified: not a lexical
    next if $v =~ /^[\$\@\%][0-9]/;     # $1, $2, ... capture vars
    $free->{$v} = 1;
  }
  $self->_eval_scope_free($_, \%bound, $free) for @subs;
}

# Walk ONE scope's elements: collect declared vars, referenced symbols, and the
# bodies of nested subs (named + anonymous), WITHOUT descending into those sub
# bodies (they are separate scopes, handled by _eval_scope_free recursion).
sub _eval_scope_parts {
  my ($self, $node, $decls, $refs, $subs) = @_;
  for my $child ($node->children) {
    next unless ref $child;
    next if $child->isa('PPI::Token::Whitespace')
         || $child->isa('PPI::Token::Comment')
         || $child->isa('PPI::Token::Pod');
    # Named sub: its block is a nested scope.
    if ($child->isa('PPI::Statement::Sub')) {
      push @$subs, $child->block if $child->can('block') && $child->block;
      next;
    }
    # Anonymous sub block: a Block preceded by 'sub' (optionally a prototype/sig).
    if ($child->isa('PPI::Structure::Block') && _block_is_anon_sub($child)) {
      push @$subs, $child;
      next;
    }
    # Variable declaration: record the declared names.  Do NOT `next` — fall
    # through so the initializer's own references (my $x = $y → $y) are scanned.
    if ($child->isa('PPI::Statement::Variable') && $child->can('type') && $child->type) {
      push @$decls, $child->variables;
    }
    # Reference symbol (normalized: $items[1] → @items, $h{k} → %h).
    # Skip MAGIC vars by type ($_, @_, $@, $0, $1.., and every punctuation
    # special: $. $! $/ $& $` $' $+ $; $, $\ $" $^W ...).  They are globals, never
    # capturable lexicals, and many aren't valid CL parameter names.
    if ($child->isa('PPI::Token::Symbol')) {
      push @$refs, $child->symbol unless $child->isa('PPI::Token::Magic');
      next;
    }
    # Recurse into composite nodes (statements, lists, blocks) — still THIS scope.
    if ($child->isa('PPI::Node')) {
      $self->_eval_scope_parts($child, $decls, $refs, $subs);
    }
  }
}

# True if a Structure::Block is an anonymous sub body (`sub { }` / `sub (proto){}`).
sub _block_is_anon_sub {
  my ($block) = @_;
  my $s = $block->sprevious_sibling;
  # Skip an intervening prototype/signature: sub ($x){...} / sub (){...}.
  while ($s && ($s->isa('PPI::Structure::List')
            || ($s->isa('PPI::Structure') && (($s->braces // '') eq '()')))) {
    $s = $s->sprevious_sibling;
  }
  return $s && $s->isa('PPI::Token::Word') && $s->content eq 'sub';
}

# True if a leading-`{` statement that PPI tokenized as a bare block
# (PPI::Statement::Compound → PPI::Structure::Block) is really an anonymous
# hash constructor.  PPI only classifies `{...}` as a Constructor when the
# first separator is `=>`; Perl's parser also treats `{ LITERAL , ... }` —
# a string or number literal followed by a comma — as a hashref in term
# context (e.g. `eval "{ 'a', 'b' }"`).  Barewords (`{ foo, 1 }`) and
# variables (`{ $x, 1 }`) stay blocks, matching Perl.  This is deliberately
# narrower than `_block_is_hash_constructor` (used for map/grep blocks, where
# `{ 'a', $_ }` IS a code block, not a hash).
sub _bare_block_is_anon_hash {
  my ($block) = @_;
  my @ch = grep { ref($_) !~ /Whitespace|Comment/ } $block->children();
  return 0 unless @ch == 1 && ref($ch[0]) eq 'PPI::Statement';
  my @sig = grep { ref($_) !~ /Whitespace|Comment/ } $ch[0]->children();
  return 0 unless @sig >= 2;
  my $r0 = ref($sig[0]);
  return 0 unless $r0 =~ /^PPI::Token::Quote::(?:Single|Double|Literal|Interpolate)$/
               || $r0 =~ /^PPI::Token::Number/;
  return 0 unless ref($sig[1]) eq 'PPI::Token::Operator'
               && ($sig[1]->content eq ',' || $sig[1]->content eq '=>');
  return 1;
}



# Transform package-qualified variable names for CL
# $Pkg::Var -> Pkg::$Var, $Pkg::Sub::Var -> |Pkg::Sub|::$Var
# Also: $::var -> main::$var (empty package = main)
sub _transform_pkg_var {
  my ($self, $var) = @_;
  # Handle package stash access: %Pkg:: or $Pkg:: (symbol table reference)
  # Must be checked BEFORE the package-qualified variable regex
  if ($var =~ /^([\$\%])(.*)::$/) {
    my ($sigil, $pkg) = ($1, $2);
    $pkg = 'main' if $pkg eq '';
    return "(p-stash \"$pkg\")";
  }
  # Handle package-qualified variables: $Pkg::var -> Pkg::$var
  # Note: Use (.*) not (.+) to allow empty package (main shorthand)
  if ($var =~ /^([\$\@\%])(.*)::([^:]+)$/) {
    my ($sigil, $pkg, $name) = ($1, $2, $3);
    # Empty package means main (e.g., $::foo = $main::foo)
    $pkg = 'main' if $pkg eq '';
    return cl_pkg($pkg) . '::' . cl_sym("${sigil}${name}");
  }
  # Pipe-quote if the name contains characters CL can't read as a bare symbol
  # (e.g. $" → |$"|, $\ → |$\\|, $| → |$\||, $; → |$;|)
  if ($var =~ /["|;,()\[\]{}'`\\]/) {
    (my $inner = $var) =~ s/([|\\])/\\$1/g;
    return "|$inner|";
  }
  return cl_sym($var);
}

# Extract individual key/index token groups from a PPI::Structure::Subscript,
# splitting on top-level comma operators.
# Returns a list of arrayrefs, one per key/index expression.
sub _subscript_key_groups {
  my ($self, $sub) = @_;

  # Flatten subscript children: unwrap Statement::Expression wrappers
  my @tokens;
  for my $child ($sub->children()) {
    next if ref($child) eq 'PPI::Token::Whitespace';
    if (ref($child) =~ /^PPI::Statement/) {
      for my $gc ($child->children()) {
        next if ref($gc) eq 'PPI::Token::Whitespace';
        push @tokens, $gc;
      }
    } else {
      push @tokens, $child;
    }
  }

  # Split on top-level comma (simple split; does not handle nested commas)
  my @groups;
  my @current;
  for my $tok (@tokens) {
    if (ref($tok) eq 'PPI::Token::Operator' && $tok->content eq ',') {
      push @groups, [@current] if @current;
      @current = ();
    } else {
      push @current, $tok;
    }
  }
  push @groups, [@current] if @current;
  return @groups;
}

# Return a list of individual CL key expression strings for a subscript.
# Unlike _subscript_key_groups + _subscript_key_expr, this expands qw// into
# individual strings — needed for delete local @h{qw/a b/} where each key
# must get its own p-local-hash-elem scope.
sub _subscript_key_cl_list {
  my ($self, $sub, $open, $stmt) = @_;

  # Collect non-whitespace children
  my @tokens;
  for my $child ($sub->children()) {
    next if ref($child) eq 'PPI::Token::Whitespace';
    if (ref($child) =~ /^PPI::Statement/) {
      for my $gc ($child->children()) {
        next if ref($gc) eq 'PPI::Token::Whitespace';
        push @tokens, $gc;
      }
    } else {
      push @tokens, $child;
    }
  }

  # Special case: single qw// token — expand into individual quoted strings
  if (@tokens == 1 && ref($tokens[0]) =~ /QuoteLike::Words/) {
    my $raw = $tokens[0]->content;      # e.g. "qw/b d/" or "qw(b d)"
    $raw =~ s/^qw\s*\S//;              # strip "qw" + opening delimiter
    $raw =~ s/\S$//;                   # strip closing delimiter
    my @words = grep { $_ ne '' } split /\s+/, $raw;
    return map { "\"$_\"" } @words;
  }

  # General case: split on commas and evaluate each group
  my @groups = $self->_subscript_key_groups($sub);
  return map { $self->_subscript_key_expr($_, $open, $stmt) } @groups;
}

# Parse one key group to a CL expression string.
# For hash subscripts ({...}), auto-quote single bareword tokens (Perl hash key rule).
sub _subscript_key_expr {
  my ($self, $group, $open, $stmt, $ctx) = @_;
  $ctx //= 0;  # default SCALAR_CTX
  if ($open eq '{' && @$group == 1 && ref($group->[0]) eq 'PPI::Token::Word') {
    my $word = $group->[0]->content;
    # Only auto-quote if it's not a keyword
    unless ($word =~ /^(?:if|unless|while|until|for|foreach|sub|my|our|local|state|undef|defined|not|and|or)$/) {
      return "\"$word\"";
    }
  }
  return $self->_parse_expression($group, $stmt, $ctx) // 'nil';
}

# Process children of a PPI node (Document or Block)
sub _process_children {
  my $self     = shift;
  my $parent   = shift;

  my @children = $parent->children;
  my %skip;

  for my $i (0 .. $#children) {
    next if $skip{$i};
    my $child = $children[$i];

    # Lookahead: bare block compound statement followed by continue { }
    if (ref($child) eq 'PPI::Statement::Compound') {
      my ($continue, $trailing) = $self->_find_continue_sibling(\@children, $i, \%skip);
      if ($continue) {
        $self->_process_compound_statement($child, $continue);
        $self->_process_trailing_tokens($trailing) if $trailing && @$trailing;
        next;
      }
    }

    $self->_process_element($child);

    # Flush any BEGIN blocks hoisted out of an expression-level do{}/eval{} that
    # sat inside this top-level statement (see parse_block_as_function).  Doing
    # it here — once the statement is fully emitted and we are back at file
    # scope — appends the BEGIN to `definitions` AFTER the enclosing form
    # (no mid-form interleaving) yet still before any later runtime code.
    if ($self->environment->in_subroutine == 0
        && $self->{_pending_hoisted_defs}
        && @{$self->{_pending_hoisted_defs}}) {
      my $section = $self->_sections->[$self->_cur_section];
      push @{$section->{'definitions'}}, @{$self->{_pending_hoisted_defs}};
      $self->{_pending_hoisted_defs} = [];
    }
  }
}

# Look ahead for a continue { } statement after a bare block compound statement.
# PPI splits "{ ... } continue { ... }" into two sibling statements for bare blocks.
# PPI may also include trailing statements in the continue PPI::Statement.
# Returns ($continue_block, \@trailing_children) if found, () otherwise.
sub _find_continue_sibling {
  my ($self, $children, $i, $skip) = @_;

  my $child = $children->[$i];

  # Check if this is a bare block (first significant child is a Block, not a keyword)
  my $is_bare_block = 0;
  for my $cc ($child->children) {
    my $ref = ref($cc);
    next if $ref eq 'PPI::Token::Whitespace';
    next if $ref eq 'PPI::Token::Label';
    if ($ref eq 'PPI::Structure::Block') {
      $is_bare_block = 1;
    }
    last;
  }
  return () unless $is_bare_block;

  # Look ahead for continue { } as next non-whitespace sibling
  for my $j ($i+1 .. $#$children) {
    my $sibling = $children->[$j];
    next if ref($sibling) eq 'PPI::Token::Whitespace';
    if (ref($sibling) eq 'PPI::Statement') {
      my @sib_kids = $sibling->children;
      my $k = 0;
      $k++ while $k <= $#sib_kids && ref($sib_kids[$k]) eq 'PPI::Token::Whitespace';
      if ($k <= $#sib_kids && ref($sib_kids[$k]) eq 'PPI::Token::Word'
          && $sib_kids[$k]->content eq 'continue') {
        my $cont_idx = $k;
        $k++;
        $k++ while $k <= $#sib_kids && ref($sib_kids[$k]) eq 'PPI::Token::Whitespace';
        if ($k <= $#sib_kids && ref($sib_kids[$k]) eq 'PPI::Structure::Block') {
          my $continue_block = $sib_kids[$k];
          # Collect trailing children after the continue block (PPI quirk:
          # PPI may include subsequent statements in the same PPI::Statement)
          my @trailing;
          for my $t ($k+1 .. $#sib_kids) {
            push @trailing, $sib_kids[$t];
          }
          $skip->{$j} = 1;
          return ($continue_block, \@trailing);
        }
      }
    }
    last;  # Only check immediate next non-whitespace sibling
  }
  return ();
}

# Process trailing PPI tokens that were orphaned when a continue { } statement
# was consumed by the bare block lookahead. PPI may include subsequent code
# (e.g., "$ok = 1;") in the same PPI::Statement as the continue block.
sub _process_trailing_tokens {
  my ($self, $trailing) = @_;

  # Filter out whitespace-only trailing content
  my @significant = grep { ref($_) ne 'PPI::Token::Whitespace' } @$trailing;
  return unless @significant;

  # Create a synthetic PPI::Statement containing the trailing tokens
  # and process it as an expression statement
  my $synth = PPI::Statement->new();
  for my $token (@$trailing) {
    $synth->add_element($token->clone());
  }
  $self->_process_expression_statement($synth);
}

# Process a single PPI element
sub _process_element {
  my $self    = shift;
  my $element = shift;

  my $ref = ref($element);

  # Skip whitespace and POD
  return if $ref eq 'PPI::Token::Whitespace';
  return if $ref eq 'PPI::Token::Pod';

  # Emit Perl comments as Lisp comments
  if ($ref eq 'PPI::Token::Comment') {
    my $comment = $element->content;
    chomp $comment;
    $self->_emit(";; $comment");
    return;
  }

  # Handle different statement types
  if ($ref eq 'PPI::Statement') {
    # CORE::state $x = ... is a variable declaration that PPI sees as a plain statement
    my ($first) = grep { ref($_) ne 'PPI::Token::Whitespace' } $element->children;
    if (defined $first && ref($first) eq 'PPI::Token::Word'
        && $first->content =~ /^CORE::(my|our|state|local)$/) {
      $first->{content} = $1;  # strip CORE:: prefix so _process_variable_statement recognizes it
      $self->_process_variable_statement($element);
    } else {
      $self->_process_expression_statement($element);
    }
  }
  elsif ($ref eq 'PPI::Statement::Expression') {
    $self->_process_expression_statement($element);
  }
  elsif ($ref eq 'PPI::Statement::Variable') {
    # Variable declaration: my $x = 10;
    $self->_process_variable_statement($element);
  }
  elsif ($ref eq 'PPI::Statement::Compound') {
    # Control flow: if, while, for, etc.
    $self->_process_compound_statement($element);
  }
  elsif ($ref eq 'PPI::Statement::Sub') {
    # Subroutine declaration
    $self->_process_sub_statement($element);
  }
  elsif ($ref eq 'PPI::Statement::Package') {
    # Package declaration
    $self->_process_package_statement($element);
  }
  elsif ($ref eq 'PPI::Statement::Include') {
    # use/require
    $self->_process_include_statement($element);
  }
  elsif ($ref eq 'PPI::Statement::Scheduled') {
    # BEGIN, END, CHECK, INIT blocks
    $self->_process_scheduled_block($element);
  }
  elsif ($ref eq 'PPI::Statement::End' || $ref eq 'PPI::Statement::Data') {
    # __END__ / __DATA__ — register DATA filehandle with embedded text
    my ($data_tok) = grep { ref($_) eq 'PPI::Token::Data'
                            || ref($_) eq 'PPI::Token::End' }
                           $element->children;
    my $data = $data_tok ? $data_tok->content : '';
    $data =~ s/\\/\\\\/g;
    $data =~ s/"/\\"/g;
    $self->_with_bucket('preamble', sub {
      $self->_emit(";; $ref — register DATA filehandle");
      $self->_emit("(setf (gethash 'DATA *p-filehandles*)");
      $self->_emit("  (make-string-input-stream \"$data\"))");
    });
    return;
  }
  elsif ($ref =~ /^PPI::Statement/) {
    # Other statement types - treat as expression for now
    $self->_process_expression_statement($element);
  }
  else {
    # Unknown - emit comment
    $self->_emit(";; UNKNOWN: $ref");
  }
}


# Process a simple expression statement
sub _process_expression_statement {
  my $self = shift;
  my $stmt = shift;

  my $perl_code = $stmt->content;
  $perl_code =~ s/;\s*$//;  # Remove trailing semicolon
  $perl_code =~ s/\n/ /g;   # Collapse newlines

  # Get the expression parts (skip semicolon, whitespace, and comments)
  my @parts = grep {
    my $ref = ref($_);
    $ref ne 'PPI::Token::Whitespace'
      && $ref ne 'PPI::Token::Comment'
      && !($ref eq 'PPI::Token::Structure' && $_->content eq ';')
  } $stmt->children;

  return unless @parts;

  # Handle '...' (yada yada / unimplemented placeholder) as a statement.
  # In Perl, a bare '...' statement dies with "Unimplemented".
  if (@parts == 1 && ref($parts[0]) eq 'PPI::Token::Operator' && $parts[0]->content eq '...') {
    $self->_emit(";; $perl_code (yada yada)");
    # Perl dies "Unimplemented at $0 line N.\n".  The file part is the runtime
    # program name ($0), not the compile-time source file, so build the :loc
    # location at runtime from $0 and the literal source line.
    my $yada_line = (ref($parts[0]) && $parts[0]->can('line_number'))
      ? ($parts[0]->line_number // 0) : 0;
    $self->_emit(qq{(p-die "Unimplemented" :loc (format nil "~A line ~D" (to-string (unbox \$0)) $yada_line))});
    return;
  }

  # Handle '++ state $y' / '-- state $y': PPI treats these as generic expression
  # statements (not PPI::Statement::Variable) because they start with an operator.
  # We detect the 'state' keyword here, register the state declaration (emits
  # defvar + rename), strip 'state' from parts, then parse the remaining expression.
  for my $i (0 .. $#parts) {
    if (ref($parts[$i]) eq 'PPI::Token::Word' && $parts[$i]->content eq 'state') {
      # Build synthetic parts for just the state declaration (state keyword + vars)
      my @decl_parts = @parts[$i .. $#parts];
      if ($self->environment->in_subroutine == 0) {
        $self->_process_toplevel_state_declaration($stmt, \@decl_parts, $perl_code);
      } else {
        $self->_process_state_declaration($stmt, \@decl_parts, $perl_code);
      }
      # Remove the 'state' token from parts so the remaining expression parses cleanly.
      # The state_var_renames lookup in ExprToCL will apply the rename.
      splice(@parts, $i, 1);
      last;
    }
  }

  # Special case: "import PACKAGE" is syntactic sugar for "PACKAGE->import()"
  # PPI parses this as two barewords, so we detect and convert it
  # Use funcall+intern to avoid read-time package dependency
  if (@parts == 2
      && ref($parts[0]) eq 'PPI::Token::Word' && $parts[0]->content eq 'import'
      && ref($parts[1]) eq 'PPI::Token::Word') {
    my $pkg = $parts[1]->content;
    $self->_emit(";; $perl_code");
    $self->_emit("(funcall (intern \"PL-IMPORT\" :$pkg))");
    $self->_emit("");
    return;
  }

  # Handle: delete local SYMBOL SUBSCRIPT (standalone delete+local statement)
  # PPI::Statement: Word("delete"), Word("local"), Symbol, Subscript
  # Opens a p-local-X-elem scope that wraps the rest of the block (closed at block end).
  # Symbol-form `delete local $h{k}` / `$a[N]`, plus the arrow-deref form
  # `delete local $ref->{k}` / `$ref->[N]` (container = unboxed referent).
  my ($sl_sub, $sl_cl_var, $sl_open);
  if (@parts >= 4
      && ref($parts[0]) eq 'PPI::Token::Word' && $parts[0]->content eq 'delete'
      && ref($parts[1]) eq 'PPI::Token::Word' && $parts[1]->content eq 'local'
      && ref($parts[2]) eq 'PPI::Token::Symbol'
      && ref($parts[3]) eq 'PPI::Structure::Subscript') {
    my $sym       = $parts[2]->content;
    $sl_sub       = $parts[3];
    $sl_open      = $sl_sub->start->content;
    my $base      = substr($sym, 1);
    my $new_sigil = ($sl_open eq '{') ? '%' : '@';
    $sl_cl_var    = $self->_transform_pkg_var("${new_sigil}${base}");
    # Stash element — not supported; skip save/restore so subsequent tests can run.
    if ($sl_cl_var =~ /^\(p-stash /) {
      $self->_emit(";; $perl_code (delete local on stash — not supported, skipped)");
      return;
    }
  }
  elsif (@parts >= 5
      && ref($parts[0]) eq 'PPI::Token::Word'     && $parts[0]->content eq 'delete'
      && ref($parts[1]) eq 'PPI::Token::Word'     && $parts[1]->content eq 'local'
      && ref($parts[2]) eq 'PPI::Token::Symbol'
      && ref($parts[3]) eq 'PPI::Token::Operator' && $parts[3]->content eq '->'
      && ref($parts[4]) eq 'PPI::Structure::Subscript') {
    $sl_sub     = $parts[4];
    $sl_open    = $sl_sub->start->content;
    my $base_cl = $self->_parse_expression([$parts[2]], $stmt);
    $sl_cl_var  = "(unbox $base_cl)";
  }
  if ($sl_sub) {
    my $sub       = $sl_sub;
    my $open      = $sl_open;
    my $cl_var    = $sl_cl_var;
    my @key_cls = $self->_subscript_key_cl_list($sub, $open, $stmt);
    if (@key_cls) {
      my $macro   = ($open eq '{') ? 'p-local-hash-elem'  : 'p-local-array-elem';
      my $del_fn  = ($open eq '{') ? 'p-delete'            : 'p-delete-array';
      $self->_emit(";; $perl_code");
      for my $key_cl (@key_cls) {
        $self->_emit("($macro $cl_var $key_cl");
        $self->_emit("  ($del_fn $cl_var $key_cl)");
        $self->indent_level($self->indent_level + 1);
        $self->{_local_let_depth} //= 0;
        $self->{_local_let_depth}++;
      }
      $self->_emit("");
      return;
    }
  }

  # Check for statement modifiers: EXPR if/unless/while/until/for COND
  my $modifier_idx = -1;
  my $modifier;
  for my $i (0 .. $#parts) {
    if (ref($parts[$i]) eq 'PPI::Token::Word') {
      my $word = $parts[$i]->content;
      if (Pl::PExpr::Config::is_statement_modifier($word)) {
        $modifier_idx = $i;
        $modifier = $word;
        last;
      }
    }
  }

  my $cl_code;
  if ($modifier_idx > 0) {
    # Split into expression and condition
    my @expr_parts = @parts[0 .. $modifier_idx - 1];
    my @cond_parts = @parts[$modifier_idx + 1 .. $#parts];

    # Unwrap PPI::Structure::Condition to get the inner expression children.
    # PPI wraps postfix-if conditions in Condition nodes: `if ($x > 1)` → Condition(...)
    if (@cond_parts == 1 && ref($cond_parts[0]) eq 'PPI::Structure::Condition') {
      @cond_parts = grep {
        ref($_) ne 'PPI::Token::Whitespace'
      } $cond_parts[0]->children;
    }

    my $expr_cl = $self->_parse_expression(\@expr_parts, $stmt, VOID_CTX);
    # Drop inline-leading indent: expr/cond are spliced onto the same line as
    # "(p-if "/"(p-foreach " below, so a leading prefix makes a weird gap.
    $expr_cl =~ s/^[ \t]+// if defined $expr_cl;

    # Generate appropriate control structure
    # Note: 'for' and 'foreach' modifiers use p-foreach (iterate over list),
    # not p-for (C-style for loop)
    my $cl_modifier = $modifier;
    if ($modifier eq 'for' || $modifier eq 'foreach') {
      $cl_modifier = 'foreach';
      # The list must be in LIST_CTX (= 1) so split() returns elements not count
      # An all-single-scalar list takes the `(vector …)` shape at every k, not
      # the run-time flattener — Parser.pm's _foreach_scalar_elements has the
      # rule.  Same decision as the block-form site in Parser2, one resolver.
      my @el = _foreach_scalar_elements(\@cond_parts);
      my $cond_cl;
      if (@el > 1) {
        # Each element run is lowered exactly once; the whole list never is.
        # Alias VERDICTS come off the untouched tokens BEFORE any lowering
        # (PExpr's cleanup mutates them), then map onto the lowered elements
        # by position — the sole-element rewrite, applied k times.
        my @hd = map { [ _foreach_alias_rewrite($_) ] } @el;
        my @forms;
        for my $i (0 .. $#el) {
          my $f = $self->_parse_expression($el[$i], $stmt, 1);
          $f =~ s/^[ \t]+// if defined $f;
          $f = _apply_alias_head($f, @{ $hd[$i] })
            // die "foreach alias: element head "
                 . $hd[$i][0] . " not outermost in: $f\n"
            if @{ $hd[$i] };
          push @forms, $f;
        }
        $cond_cl = '(vector ' . join(' ', @forms) . ')';
      }
      else {
        $cond_cl = $self->_parse_expression(\@cond_parts, $stmt, 1);
        $cond_cl =~ s/^[ \t]+// if defined $cond_cl;
        # `$_ = "w" for ($h{k})` aliases the live element exactly as the block
        # form does — same rewrite, same helper (#263).
        $cond_cl = _apply_foreach_alias_rewrite($cond_cl, \@cond_parts);
        $cond_cl = "(vector $cond_cl)" if @el;
      }
      $cl_code = "(p-foreach (\$_ $cond_cl) $expr_cl)";
    }
    else {
      my $cond_cl = $self->_parse_expression(\@cond_parts, $stmt);
      # Strip leading indent BEFORE the ^\(p-... auto-defined regexes below,
      # which are anchored and would miss a space-prefixed cond.
      $cond_cl =~ s/^[ \t]+// if defined $cond_cl;
      # Apply Perl's auto-defined() insertion for while-modifier loops.
      # while ($x = readdir/each/readline/glob) terminates on undef, not on false.
      $cond_cl = $self->_auto_defined_cond($cond_cl) if $cl_modifier eq 'while';
      # `do BLOCK while/until COND` is a POST-test loop in Perl: BLOCK always
      # runs at least once and the condition is tested afterwards.  Detect the
      # `do { ... }` expression (Word 'do' + Structure::Block) and emit the
      # post-test macro instead of the pre-test p-while/p-until.
      if (($cl_modifier eq 'while' || $cl_modifier eq 'until')
          && @expr_parts == 2
          && ref($expr_parts[0]) eq 'PPI::Token::Word'
          && $expr_parts[0]->content eq 'do'
          && ref($expr_parts[1]) eq 'PPI::Structure::Block') {
        $cl_code = "(p-do-$cl_modifier $cond_cl $expr_cl)";
      } else {
        $cl_code = "(p-$cl_modifier $cond_cl $expr_cl)";
      }
    }
  }
  else {
    # No modifier - bare expression statement; result is normally discarded
    # (void context).  Exception: the tail statement of a map { } block, whose
    # value IS consumed in LIST context — parse it that way so `..` is a range,
    # an @array flattens, etc.
    my $stmt_ctx = ($self->environment->tail_position
                    && $self->environment->tail_wants_list)
        ? LIST_CTX : VOID_CTX;
    $cl_code = $self->_parse_expression(\@parts, $stmt, $stmt_ctx);
  }

  # Emit as comment + code.
  # When inside a sub body and NOT at tail position, wrap in void context so that
  # dynamic operators like /g regex don't inherit the caller's list context.
  #
  # Exception: a bare `return EXPR` (no statement modifier) needs no wrap — the
  # p-return macro re-binds *wantarray* to *pcl-caller-wantarray* before
  # evaluating EXPR, so an enclosing (let ((*wantarray* :void)) ...) is dead code
  # (it is shadowed before it is ever read).  Suppressing it keeps the generated
  # CL readable without any behaviour change.
  my $is_bare_return = ($modifier_idx < 0)
      && ref($parts[0]) eq 'PPI::Token::Word'
      && $parts[0]->content eq 'return';
  if (defined $cl_code
      && $self->environment->in_subroutine > 0
      && !$self->environment->tail_position
      && !$is_bare_return) {
    # Non-tail statement runs in void context.  When a sub-body void regime is
    # active (wa_void_active: *wantarray* already bound to :void once around the
    # whole body), trust that inherited dynamic binding and emit no wrap.
    # Otherwise (a do/eval/map/grep/sort boundary, or no regime) wrap this one.
    $cl_code = "(let ((*wantarray* :void)) $cl_code)"
      unless $self->environment->wa_void_active;
  }
  elsif (defined $cl_code
         && $self->environment->wa_void_active
         && $self->environment->tail_position
         && !$is_bare_return) {
    # Tail (value/return position) under an active void regime: the body bound
    # *wantarray* to :void, but the implicit return value must use the CALLER's
    # context.  Restore it for this one statement (explicit `return` is excluded —
    # p-return already restores *pcl-caller-wantarray* itself).
    $cl_code = "(let ((*wantarray* *pcl-caller-wantarray*)) $cl_code)";
  }
  $self->_emit(";; $perl_code");
  $self->_emit($cl_code) if defined $cl_code;
  $self->_emit("");
}


# Process variable declaration: my $x = 10;
sub _process_variable_statement {
  my $self = shift;
  my $stmt = shift;

  my $perl_code = $stmt->content;
  $perl_code =~ s/;\s*$//;
  $perl_code =~ s/\n/ /g;

  # Get expression parts (skip semicolon and whitespace)
  my @parts = grep {
    my $ref = ref($_);
    $ref ne 'PPI::Token::Whitespace'
      && !($ref eq 'PPI::Token::Structure' && $_->content eq ';')
  } $stmt->children;

  return unless @parts;

  # Check declarator type
  my $declarator = '';
  if (ref($parts[0]) eq 'PPI::Token::Word' && $parts[0]->content =~ /^(my|our|state|local)$/) {
    $declarator = $1;
  }

  # Statement modifier on a `my` declaration inside a sub:
  #   my $x = EXPR if COND;   my @a = LIST unless COND;   my $c = shift if @_;
  # The lexical is declared UNCONDITIONALLY — its `let` is opened by the block
  # scanner from the `my` — and only the initializer assignment is conditional.
  # Strip the declarator and route `$x = EXPR if COND` through the expression-
  # statement path, which already lowers if/unless/while/until/for modifiers.
  # Without this the modifier tokens stayed in @parts and the RHS parser choked
  # (`my $c = shift if @_>1` → (p-shift (p-if ...)) crash; `my $c = 5 if @_>1`
  # → dropped initializer).  Scoped to in-sub `my`: top-level my/our/local/state
  # emit their declaration inline (not via the scanner), so they keep their own
  # paths.
  if ($declarator eq 'my' && $self->environment->in_subroutine > 0) {
    my $mod_idx = -1;
    for my $i (1 .. $#parts) {
      next unless ref($parts[$i]) eq 'PPI::Token::Word';
      if (Pl::PExpr::Config::is_statement_modifier($parts[$i]->content)) {
        $mod_idx = $i;
        last;
      }
    }
    if ($mod_idx > 0) {
      $self->_emit(";; $perl_code");
      my $synth = PPI::Statement->new();
      $synth->add_element($_->clone) for @parts[1 .. $#parts];
      $self->_process_expression_statement($synth);
      return;
    }
  }

  # Handle 'our' declarations - package variables
  if ($declarator eq 'our') {
    $self->_process_our_declaration($stmt, \@parts, $perl_code);
    return;
  }

  # Handle 'local' declarations - dynamic scoping
  if ($declarator eq 'local') {
    $self->_process_local_declaration($stmt, \@parts, $perl_code);
    return;
  }

  # Handle top-level 'my' declarations - need eval-when+defvar for BEGIN block visibility
  # Inside subs, my uses regular let bindings (handled elsewhere)
  # Exception: if the var was renamed by _with_declarations (closure capture at pkg level),
  # skip _process_my_toplevel_declaration and fall through to the rename handling below.
  if ($declarator eq 'my' && $self->environment->in_subroutine == 0) {
    my $scope_renames = $self->{_current_scope_new_renames} // {};
    my $var_for_check;
    for my $p (@parts) {
      my $ref = ref($p);
      last if $ref eq 'PPI::Structure::List';
      if ($ref eq 'PPI::Token::Symbol') { $var_for_check = $p->content; last; }
    }
    unless (defined $var_for_check && exists $scope_renames->{$var_for_check}) {
      $self->_process_my_toplevel_declaration($stmt, \@parts, $perl_code);
      return;
    }
    # Fall through: this var was renamed for closure capture — handle via rename path
  }

  # Check if this is a state declaration inside a sub
  my $is_state = ($declarator eq 'state');
  my $state_vars = $self->{_current_state_vars} // {};

  # Package-level state: needs init-once guard (unlike `my` which runs once at load)
  # Each `state $var` declaration at top-level gets a unique name and init flag
  # so multiple `state $var` in different loops don't share the same variable.
  if ($is_state && $self->environment->in_subroutine == 0) {
    $self->_process_toplevel_state_declaration($stmt, \@parts, $perl_code);
    return;
  }

  if ($is_state && %$state_vars) {
    # State declaration inside a sub - generate init guard
    $self->_process_state_declaration($stmt, \@parts, $perl_code);
    return;
  }

  # Check for bare declaration without assignment (my $x; or my @arr;)
  # These have: declarator, variable, no operator
  my $has_operator = grep { ref($_) eq 'PPI::Token::Operator' } @parts;
  if (!$has_operator) {
    # Bare declaration - just emit as comment, runtime will auto-declare
    # For state, we also need init guard for bare declarations
    if ($is_state && %$state_vars) {
      $self->_process_state_declaration($stmt, \@parts, $perl_code);
      return;
    }
    $self->_emit(";; $perl_code (bare declaration)");
    $self->_emit("");
    return;
  }

  # Special case: scalar 'my $var = EXPR' inside a sub where $var was renamed by
  # _with_declarations (captured by a closure). Parse only the RHS with the rename
  # for $var temporarily absent so that '$var' in the RHS refers to the outer scope.
  # This handles 'my $i = $i + 1' shadowing correctly (outer $i → 5, not the new lex box).
  if ($declarator eq 'my') {
    my $scope_renames = $self->{_current_scope_new_renames} // {};

    # Find the declared scalar variable (skip if list declaration)
    my $var_name;
    for my $p (@parts) {
      my $ref = ref($p);
      last if $ref eq 'PPI::Structure::List';  # list decl — handled below
      if ($ref eq 'PPI::Token::Symbol') { $var_name = $p->content; last; }
    }

    if (defined $var_name && exists $scope_renames->{$var_name}) {
      # Find '=' and split into RHS tokens
      my $eq_idx = -1;
      for my $i (0 .. $#parts) {
        if (ref($parts[$i]) eq 'PPI::Token::Operator' && $parts[$i]->content eq '=') {
          $eq_idx = $i; last;
        }
      }
      if ($eq_idx >= 0) {
        my @rhs_parts = @parts[$eq_idx + 1 .. $#parts];
        my $new_name  = $scope_renames->{$var_name};

        # Temporarily remove new rename so RHS sees the outer/old binding for $var_name
        my $old_rn      = $self->{_current_scope_old_renames} // {};
        my $env_renames = $self->environment->state_var_renames // {};
        my %temp = %$env_renames;
        if (defined $old_rn->{$var_name}) {
          $temp{$var_name} = $old_rn->{$var_name};
        } else {
          delete $temp{$var_name};
        }
        $self->environment->state_var_renames(\%temp);
        # Also remove it from _current_scope_new_renames for the RHS parse: an
        # anon sub in the RHS may RE-DECLARE `my $var_name` (Moo install_delayed:
        # my $c = defer_sub ... sub { my $c = gen(); $c }).  That inner decl is a
        # SHADOW with its own plain let binding; if this map still carries the
        # rename, the inner decl's assignment would target OUR new __lex__ var
        # while the inner body reads its plain binding (assignment lost).
        my $saved_scope_rn = $self->{_current_scope_new_renames};
        {
          my %scope = %{ $saved_scope_rn // {} };
          delete $scope{$var_name};
          $self->{_current_scope_new_renames} = \%scope;
        }

        # An array/hash LHS forces LIST context on the RHS — resolved at COMPILE
        # time (no runtime *wantarray* check): `my @a = (1,2,3)` captured by a
        # closure inside a scalar-context block (my $r = do { ... my @a=... })
        # must still build the whole list, not collapse to its last element.
        my $lhs_ctx = ($var_name =~ /^[\@%]/) ? 1 : 0;  # 1 = LIST_CTX, 0 = SCALAR
        my $rhs_cl = $self->_parse_expression(\@rhs_parts, $stmt, $lhs_ctx);

        # Re-apply new rename
        $self->environment->state_var_renames($env_renames);
        $self->{_current_scope_new_renames} = $saved_scope_rn;

        # Choose the assignment op by LHS sigil.  A captured array/hash is a
        # let-bound LEXICAL (already an adjustable array / hash table), so we
        # fill it in place via p-array-fill / p-hash-fill — NOT p-array-=/p-hash-=
        # (their boundp/proclaim-special guard would make the lexical special and
        # break the closure) and NOT p-my-= (box-set is a no-op on a non-box
        # array/hash place, so the aggregate would never be populated).
        my $sigil = substr($var_name, 0, 1);
        my $assign = $sigil eq '@' ? "(p-array-fill $new_name $rhs_cl)"
                   : $sigil eq '%' ? "(p-hash-fill $new_name $rhs_cl)"
                   :                 "(p-my-= $new_name $rhs_cl)";
        $self->_emit(";; $perl_code");
        $self->_emit($assign) if defined $rhs_cl && $rhs_cl ne '';
        $self->_emit("");
        return;
      }
    }
  }

  # Special case: 'my $x = state $y = EXPR' inside a sub.
  # The state $y part needs its init guard; the whole expr must return $y's current value.
  # Detected when declarator is 'my', there's a state var in state_vars, and RHS has 'state'.
  if ($declarator eq 'my' && %$state_vars) {
    my ($eq_idx, $state_idx) = (-1, -1);
    for my $i (0 .. $#parts) {
      my $pref = ref($parts[$i]);
      if ($pref eq 'PPI::Token::Operator' && $parts[$i]->content eq '=' && $eq_idx < 0) {
        $eq_idx = $i;
      }
      if ($eq_idx >= 0 && $pref eq 'PPI::Token::Word' && $parts[$i]->content eq 'state') {
        $state_idx = $i; last;
      }
    }
    if ($state_idx > $eq_idx && $eq_idx >= 0) {
      # Find the state var and its init
      my $state_var_name;
      my $state_eq_idx = -1;
      my $rhs_state_parts = [grep { ref($_) ne 'PPI::Token::Whitespace' }
                              @parts[($state_idx + 1) .. $#parts]];
      for my $i (0 .. $#$rhs_state_parts) {
        my $pref = ref($rhs_state_parts->[$i]);
        if ($pref eq 'PPI::Token::Symbol' && !defined $state_var_name) {
          $state_var_name = $rhs_state_parts->[$i]->content;
        }
        if ($pref eq 'PPI::Token::Operator' && $rhs_state_parts->[$i]->content eq '=' && $state_eq_idx < 0) {
          $state_eq_idx = $i;
        }
      }
      if (defined $state_var_name) {
        my $renames  = $self->environment->state_var_renames // {};
        my $cl_state = $renames->{$state_var_name} // $state_var_name;
        my $flag     = "${cl_state}__init";

        # Find the LHS variable name
        my $lhs_name;
        for my $p (@parts[0 .. ($eq_idx - 1)]) {
          if (ref($p) eq 'PPI::Token::Symbol') { $lhs_name = $p->content; last; }
        }
        $lhs_name //= '$__unused';

        # Parse init expression if present
        my $init_cl = 'nil';
        if ($state_eq_idx >= 0) {
          my @init_parts = grep { ref($_) ne 'PPI::Token::Whitespace' }
                           @$rhs_state_parts[($state_eq_idx + 1) .. $#$rhs_state_parts];
          $init_cl = $self->_parse_expression(\@init_parts, $stmt) // 'nil' if @init_parts;
        }

        $self->_emit(";; $perl_code");
        $self->_emit("(p-my-= $lhs_name");
        $self->indent_level($self->indent_level + 1);
        $self->_emit("(progn");
        $self->indent_level($self->indent_level + 1);
        $self->_emit("(unless $flag");
        $self->indent_level($self->indent_level + 1);
        $self->_emit("(box-set $cl_state $init_cl)");
        $self->_emit("(setf $flag t))");
        $self->indent_level($self->indent_level - 1);
        $self->_emit("$cl_state))");
        $self->indent_level($self->indent_level - 2);
        $self->_emit("");
        return;
      }
    }
  }

  # For 'my @arr = EXPR' inside a block where init was pre-computed into the let binding
  # (self-referential init: my @bee = @bee), skip the body assignment — it's already done.
  if ($declarator eq 'my') {
    my $binding_inits = $self->{_my_binding_init_vars} // {};
    if (%$binding_inits) {
      my $decl_var;
      for my $p (@parts) {
        my $ref = ref($p);
        last if $ref eq 'PPI::Token::Operator' && $p->content eq '=';
        last if $ref eq 'PPI::Structure::List';  # multi-var: don't skip
        if ($ref eq 'PPI::Token::Symbol') { $decl_var = $p->content; last; }
      }
      if (defined $decl_var && $binding_inits->{$decl_var}) {
        $self->_emit(";; $perl_code (init in let binding)");
        $self->_emit("");
        return;
      }
    }
  }

  # Parse with PExpr (handles declarator extraction)
  my $cl_code = $self->_parse_expression(\@parts, $stmt);

  $self->_emit(";; $perl_code");
  $self->_emit($cl_code) if defined $cl_code;
  $self->_emit("");
}

# Qualify an `our` variable name to its fully-qualified CL symbol, byte-identical
# to what gen_node (ExprToCL.pm ~541-546) emits for *references* to the same var.
# Inside a `{ package Foo; ... }` BLOCK the whole block is one top-level CL form, so
# the inline `(in-package :Foo)` does NOT affect how the reader interns the bare
# names within it — bare `$x` would become MAIN::$x while references read Foo::$x.
# Emitting the qualified name here keeps declaration/assignment and reference in
# the same symbol. (At file scope `package Foo;` bare names already intern into
# Foo, so qualifying is a no-op there.)
sub _our_var_cl_name {
  my ($self, $pkg, $var) = @_;
  return $var if !defined($pkg) || $pkg eq 'main';
  return $var unless $var =~ /^([\$\@\%])(\w+)$/;
  my ($sigil, $name) = ($1, $2);
  return cl_pkg($pkg) . '::' . cl_sym("${sigil}${name}");
}

# Process 'our' variable declaration - package-level variable
sub _process_our_declaration {
  my $self = shift;
  my $stmt = shift;
  my $parts = shift;
  my $perl_code = shift;

  my $pkg = $self->environment->current_package;

  # Find variable(s) and optional initializer
  my @vars;
  my $init_idx = -1;

  # A trailing statement modifier belongs to the STATEMENT, not to the
  # declaration, so it is split off BEFORE the names are read — otherwise
  # `our $c++ if $x` would read $x as a second declared name.  Same scan
  # (and same shared predicate) as the in-sub `my` branch in
  # _process_variable_statement, whose comment explains the shape.
  my $mod_idx = -1;
  for my $i (1 .. $#$parts) {
    next unless ref($parts->[$i]) eq 'PPI::Token::Word';
    next unless Pl::PExpr::Config::is_statement_modifier($parts->[$i]->content);
    $mod_idx = $i;
    last;
  }
  my @decl_parts = $mod_idx > 0 ? @$parts[0 .. $mod_idx - 1] : @$parts;

  my $names_end = 0;
  for my $i (0 .. $#decl_parts) {
    my $p = $decl_parts[$i];
    my $ref = ref($p);

    if ($ref eq 'PPI::Token::Symbol' || $ref eq 'PPI::Token::Magic') {
      push @vars, $p->content;
      $names_end = $i;
    }
    elsif ($ref eq 'PPI::Structure::List') {
      # List declaration: our ($x, $y)
      push @vars, $self->_find_symbols_in_list($p);
      $names_end = $i;
    }
    elsif ($ref eq 'PPI::Token::Operator' && $p->content eq '=') {
      $init_idx = $i;
      last;
    }
  }

  return unless @vars;

  # Register in environment
  for my $var (@vars) {
    $self->environment->add_our_variable($pkg, $var);
  }

  # Special handling for @ISA - inheritance declaration.  Deliberately BEFORE
  # the modifier/tail branch below: `our @ISA = (...)` sets inheritance up at
  # COMPILE time, which a runtime modifier cannot express, and no corpus
  # writes a conditional @ISA (perl-tests + perl's own t/ + lib: zero).
  if (@vars == 1 && $vars[0] eq '@ISA' && $init_idx >= 0) {
    $self->_process_isa_declaration($stmt, $parts, $init_idx, $perl_code);
    return;
  }

  # DECLARE, then run `NAMES <tail>` as an ordinary statement.  Two shapes
  # reach here that the three branches below cannot express, and both used to
  # be lost in SILENCE — the declaration was emitted and the tail simply
  # vanished (measured s410 inside an anon sub containing a `local`, which
  # routes its whole body here: `our $c++` left $c at 0, `our $V ||= 7` left
  # $V undef, `our $d += 4` left $d at 3, `our $e = 1 if 1` left $e undef,
  # `our @a = (1,2) if 1` emitted a PARSE ERROR into the assignment):
  #
  #   - a trailing statement MODIFIER — perl declares the package cell
  #     unconditionally (a compile-time act) and makes only the tail
  #     conditional;
  #   - a tail that is not an `=` assignment at all (`our $count++;`,
  #     `our $Verbose ||= 0;` — Exporter's idiom), which the `=` scan above
  #     never sees and the bare-declaration branch below discards.
  #
  # Both are `NAMES <tail>` once the declarator is stripped, i.e. exactly the
  # statement the expression path already lowers (it owns all six modifiers) —
  # the same move the in-sub `my` branch of _process_variable_statement makes,
  # and the same rule Parser2::_lower_our_decl states for v2.
  my $has_expr_tail = $mod_idx > 0
    || ($init_idx < 0
        && grep { $_->significant } @$parts[$names_end + 1 .. $#$parts]);
  if ($has_expr_tail) {
    $self->_emit_our_declarations(\@vars, $pkg, $perl_code);
    my $synth = PPI::Statement->new();
    $synth->add_element($_->clone) for @$parts[1 .. $#$parts];
    $self->_process_expression_statement($synth);
    $self->_emit("");
    return;
  }

  # Compile-time declarations (defvar) go to declarations bucket.
  # Separate declaration from initialization (runtime) to match Perl:
  # 'our $x = 1; BEGIN { $x = 2 }' → at runtime $x becomes 1 (init overwrites BEGIN)
  # When inside a sub, _insert_variable_forward_declarations won't see our variables
  # at file scope, so we must emit defvars explicitly here.
  $self->_with_bucket('declarations', sub {
    $self->_emit(";; $perl_code");
    if ($self->environment->in_subroutine > 0) {
      # Multi-segment package names (Foo::Bar) must be pipe-quoted in the CL
      # symbol prefix, else the reader sees "Foo::Bar::$var" as too many colons.
      # _cl_pkg_designator is the single source of truth (':|Foo::Bar|' / ':main');
      # strip the leading ':' to get the symbol-package prefix (cf. line ~5458).
      (my $cl_pkg_sym = $self->_cl_pkg_designator($pkg)) =~ s/^://;
      for my $var (@vars) {
        my $sigil = substr($var, 0, 1);
        my $init = $sigil eq '$' ? '(make-p-box nil)'
                 : $sigil eq '@' ? '(make-array 0 :adjustable t :fill-pointer 0)'
                 :                 '(make-hash-table :test #\'equal)';
        my $cl_var = "${cl_pkg_sym}::${var}";
        $self->_emit(global_decl_form("$cl_var", "$init"));
      }
    }
  });

  if ($init_idx >= 0) {
    # Has initializer - parse the RHS
    my @rhs_parts = @$parts[($init_idx + 1) .. $#$parts];
    @rhs_parts = grep { ref($_) ne 'PPI::Token::Whitespace' } @rhs_parts;

    if (@vars == 1) {
      # Single variable: our $x = value, our @arr = (), our %hash = ()
      my $var = $vars[0];
      my $sigil = substr($var, 0, 1);

      # Check for empty list initializer ()
      my $is_empty_list = (@rhs_parts == 1 &&
                           ref($rhs_parts[0]) eq 'PPI::Structure::List' &&
                           $self->_is_empty_structure($rhs_parts[0]));

      my $cl_var = $self->_our_var_cl_name($pkg, $var);
      if ($sigil eq '@') {
        # Array: declare at compile time, initialize at runtime
        $self->_with_bucket('declarations', sub {
          $self->_emit("(p-eval-always");
          $self->_emit("  " . global_decl_form("$cl_var", "(make-array 0 :adjustable t :fill-pointer 0)") . ")");
        });
        unless ($is_empty_list) {
          # Parse full statement so PExpr sees '@arr = ...' and propagates
          # LIST context to the RHS (e.g. split() gets LIST_CTX, not SCALAR_CTX)
          my $cl_code = $self->_parse_expression($parts, $stmt);
          $self->_emit($cl_code) if defined $cl_code;
        }
      }
      elsif ($sigil eq '%') {
        # Hash: declare at compile time, initialize at runtime
        $self->_with_bucket('declarations', sub {
          $self->_emit("(p-eval-always");
          $self->_emit("  " . global_decl_form("$cl_var", "(make-hash-table :test 'equal)") . ")");
        });
        unless ($is_empty_list) {
          # Parse full statement so PExpr sees '%hash = ...' and propagates
          # LIST context to the RHS
          my $cl_code = $self->_parse_expression($parts, $stmt);
          $self->_emit($cl_code) if defined $cl_code;
        }
      }
      else {
        # Scalar: declare with nil box at compile time, set value at runtime
        my $init_cl = $self->_parse_expression(\@rhs_parts, $stmt) // 'nil';
        $self->_with_bucket('declarations', sub {
          $self->_emit("(p-eval-always");
          $self->_emit("  " . global_decl_form("$cl_var", "(make-p-box nil)") . ")");
        });
        $self->_emit("(setf (p-box-value $cl_var) $init_cl)");
      }
    }
    else {
      # Multiple variables: our ($x, $y) = (1, 2)
      # First declare all at compile time, then assign at runtime
      $self->_emit_our_declarations(\@vars, $pkg);
      # Now do the assignment at runtime.
      # our (...) = (...) is a list assignment, so the RHS is LIST context
      # (so '1..3' generates a range, not a flip-flop).
      my $init_cl = $self->_parse_expression(\@rhs_parts, $stmt, 1) // 'nil';  # 1 = LIST_CTX
      my $vars_vector = "(vector " .
        join(" ", map { $self->_our_var_cl_name($pkg, $_) } @vars) . ")";
      $self->_emit("(p-list-= $vars_vector $init_cl)");
    }
  }
  else {
    # Bare declaration: our $x; or our @arr; or our %hash;
    $self->_emit_our_declarations(\@vars, $pkg);
  }

  $self->_emit("");
}

# The compile-time half of an `our` declaration: one p-eval-always defvar per
# name, in the declarations bucket, container chosen by sigil.  Three callers
# had the same eight lines (multi-var init, bare declaration, and the
# declare-then-run-the-tail branch); this is the one copy.
sub _emit_our_declarations {
  my ($self, $vars, $pkg, $perl_code) = @_;
  $self->_with_bucket('declarations', sub {
    $self->_emit(";; $perl_code") if defined $perl_code;
    for my $var (@$vars) {
      my $sigil  = substr($var, 0, 1);
      my $cl_var = $self->_our_var_cl_name($pkg, $var);
      my $init = $sigil eq '@' ? "(make-array 0 :adjustable t :fill-pointer 0)"
               : $sigil eq '%' ? "(make-hash-table :test 'equal)"
               :                 "(make-p-box nil)";
      $self->_emit("(p-eval-always");
      $self->_emit("  " . global_decl_form("$cl_var", $init) . ")");
    }
  });
}

# Process top-level 'state' declaration - like my but with init-once guard.
# Each declaration gets a unique renamed CL variable so that multiple `state $x`
# in different loops at the same file scope don't share the same binding.
sub _process_toplevel_state_declaration {
  my ($self, $stmt, $parts, $perl_code) = @_;

  # Parse variable(s) and optional initializer
  my @vars;
  my $init_idx   = -1;
  my $postfix_op = '';  # '++' or '--' after variable (no '=')
  for my $i (0 .. $#$parts) {
    my $p    = $parts->[$i];
    my $pref = ref($p);
    if ($pref eq 'PPI::Token::Symbol' || $pref eq 'PPI::Token::Magic') {
      push @vars, $p->content;
    }
    elsif ($pref eq 'PPI::Structure::List') {
      push @vars, $self->_find_symbols_in_list($p);
    }
    elsif ($pref eq 'PPI::Token::Operator' && $p->content eq '=') {
      $init_idx = $i; last;
    }
    elsif ($pref eq 'PPI::Token::Operator' && $p->content =~ /^(\+\+|--)$/ && @vars) {
      $postfix_op = $p->content; last;
    }
  }
  return unless @vars;

  # Assign each variable a unique renamed CL name and an init flag.
  my %renames_for_this;
  my $env_renames = $self->environment->state_var_renames // {};
  for my $var (@vars) {
    my ($sigil, $bare) = ($var =~ /^([\$\@\%])(.+)$/);
    ($sigil, $bare) = ('$', $var) unless defined $bare;
    (my $slug = $bare) =~ s/[^a-zA-Z0-9]/_/g;
    my $n = ++$state_var_counter;
    my $new_name = sprintf('%sstate__toplevel__%s__%d', $sigil, $slug, $n);
    $renames_for_this{$var} = $new_name;
  }

  # Persist renames so subsequent code in this file uses the new names.
  $self->environment->state_var_renames({ %$env_renames, %renames_for_this });

  # Emit declarations (defvar for each renamed var + init flag)
  $self->_with_bucket('declarations', sub {
    $self->_emit(";; $perl_code");
    for my $var (@vars) {
      my $cl_var = $renames_for_this{$var};
      my $sigil  = substr($cl_var, 0, 1);
      $self->_emit("(p-eval-always");
      if ($sigil eq '@') {
        $self->_emit("  " . global_decl_form("$cl_var", "(make-array 0 :adjustable t :fill-pointer 0)") . ")");
      } elsif ($sigil eq '%') {
        $self->_emit("  " . global_decl_form("$cl_var", "(make-hash-table :test 'equal)") . ")");
      } else {
        $self->_emit("  " . global_decl_form("$cl_var", "(make-p-box nil)") . ")");
      }
      $self->_emit("(p-eval-always " . global_decl_form("${cl_var}__init", "nil") . ")");
    }
  });

  # Emit inline init guard (only runs init expression once)
  if ($init_idx >= 0) {
    my @rhs_parts = grep { ref($_) ne 'PPI::Token::Whitespace' }
                    @$parts[($init_idx + 1) .. $#$parts];
    my $init_cl = 'nil';
    $init_cl = $self->_parse_expression(\@rhs_parts, $stmt) // 'nil' if @rhs_parts;

    $self->_emit(";; $perl_code");
    for my $var (@vars) {
      my $cl_var  = $renames_for_this{$var};
      my $sigil   = substr($cl_var, 0, 1);
      my $flag    = "${cl_var}__init";
      $self->_emit("(unless $flag");
      $self->indent_level($self->indent_level + 1);
      if ($sigil eq '$') {
        $self->_emit("(box-set $cl_var $init_cl)");
      } elsif ($sigil eq '@') {
        $self->_emit("(p-array-= $cl_var (let ((*wantarray* t)) (list $init_cl)))");
      } elsif ($sigil eq '%') {
        $self->_emit("(p-hash-= $cl_var (let ((*wantarray* t)) (list $init_cl)))");
      }
      $self->_emit("(setf $flag t))");
      $self->indent_level($self->indent_level - 1);
    }
    $self->_emit("");
  }

  # Emit post-increment/decrement when no initializer (state $z++)
  if ($postfix_op && $init_idx < 0) {
    my $cl_op = ($postfix_op eq '++') ? 'p-post++' : 'p-post--';
    $self->_emit(";; $perl_code");
    for my $var (@vars) {
      my $cl_var = $renames_for_this{$var};
      $self->_emit("($cl_op $cl_var)");
    }
    $self->_emit("");
  }

  # The value of a `state $x = EXPR` expression is the CURRENT value of $x
  # (not the init-guard result). Emit the variable as the trailing form so the
  # statement yields the right value in tail/expression position — e.g. as a
  # map/grep block return or a sub's implicit return. Only meaningful for a
  # single declared variable; list forms are left as-is.
  if (!$postfix_op && @vars == 1) {
    $self->_emit($renames_for_this{$vars[0]});
  }
}

# Process top-level 'my' declaration - lexical at file scope
# Uses eval-when for BEGIN block visibility: declaration at compile time,
# initialization at runtime. Inside subs, 'my' uses regular let bindings.
sub _process_my_toplevel_declaration {
  my $self = shift;
  my $stmt = shift;
  my $parts = shift;
  my $perl_code = shift;

  # Find variable(s) and optional initializer
  my @vars;
  my $init_idx = -1;
  # Whether the LHS was parenthesized: my ($x) is a LIST assignment (so a single
  # scalar gets the FIRST RHS element), whereas my $x is a SCALAR assignment (the
  # comma operator / array-in-scalar count).  Without this, my ($x) = @a wrongly
  # compiled to (box-set $x @a) → element count, and my ($x) = (a,b) → last elem.
  my $lhs_is_list = 0;
  # An `undef` placeholder in the LHS list — my (undef, $x) = … — occupies a
  # position but is not a declared symbol, so @vars under-counts it.  The
  # single-var `(vector $x)` shortcut below would then misalign ($x would take
  # the FIRST RHS element); detect placeholders so that case routes through the
  # general list-assignment path, which emits (vector (p-undef) $x) in position.
  my $lhs_has_placeholder = 0;

  for my $i (0 .. $#$parts) {
    my $p = $parts->[$i];
    my $ref = ref($p);

    if ($ref eq 'PPI::Token::Symbol' || $ref eq 'PPI::Token::Magic') {
      push @vars, $p->content;
    }
    elsif ($ref eq 'PPI::Structure::List') {
      # List declaration: my ($x, $y) — or a parenthesized single var my ($x)
      $lhs_is_list = 1;
      push @vars, $self->_find_symbols_in_list($p);
      $lhs_has_placeholder = 1 if $self->_list_has_undef_placeholder($p);
    }
    elsif ($ref eq 'PPI::Token::Operator' && $p->content eq '=') {
      $init_idx = $i;
      last;
    }
  }

  return unless @vars;

  # Compile-time declarations (defvar) go to the declarations bucket
  $self->_with_bucket('declarations', sub {
    $self->_emit(";; $perl_code");
    for my $var (@vars) {
      my $sigil = substr($var, 0, 1);
      $self->_emit("(p-eval-always");
      if ($sigil eq '@') {
        $self->_emit("  " . global_decl_form("$var", "(make-array 0 :adjustable t :fill-pointer 0)") . ")");
      } elsif ($sigil eq '%') {
        $self->_emit("  " . global_decl_form("$var", "(make-hash-table :test 'equal)") . ")");
      } else {
        $self->_emit("  " . global_decl_form("$var", "(make-p-box nil)") . ")");
      }
    }
  });

  # Handle initialization at runtime (stays in current bucket)
  if ($init_idx >= 0) {
    my @rhs_parts = @$parts[($init_idx + 1) .. $#$parts];
    @rhs_parts = grep { ref($_) ne 'PPI::Token::Whitespace' } @rhs_parts;

    # Check for empty list initializer ()
    my $is_empty_list = (@rhs_parts == 1 &&
                         ref($rhs_parts[0]) eq 'PPI::Structure::List' &&
                         $self->_is_empty_structure($rhs_parts[0]));

    unless ($is_empty_list) {
      # Check for: my VARS = delete local SYMBOL SUBSCRIPT
      # This must be handled before normal expression parsing because:
      #  - The local save/restore must scope to the enclosing block
      #  - The delete is done inside the local scope, and result assigned to VARS
      my @clean_rhs = grep { ref($_) ne 'PPI::Token::Whitespace' } @rhs_parts;
      # Match `delete local $h{k}` / `$a[N]` (Symbol Subscript) AND the arrow-deref
      # form `delete local $ref->{k}` / `$ref->[N]` (Symbol -> Subscript), whose
      # container is the unboxed referent.  Without the arrow branch the `local`
      # was silently dropped (no save/restore), so the element never restored.
      my ($dl_sub, $dl_cl_var, $dl_open);
      if (@clean_rhs >= 4
          && ref($clean_rhs[0]) eq 'PPI::Token::Word'  && $clean_rhs[0]->content eq 'delete'
          && ref($clean_rhs[1]) eq 'PPI::Token::Word'  && $clean_rhs[1]->content eq 'local'
          && ref($clean_rhs[2]) eq 'PPI::Token::Symbol'
          && ref($clean_rhs[3]) eq 'PPI::Structure::Subscript') {
        my $sym  = $clean_rhs[2]->content;
        $dl_sub  = $clean_rhs[3];
        $dl_open = $dl_sub->start->content;
        my $base = substr($sym, 1);
        my $new_sigil = ($dl_open eq '{') ? '%' : '@';
        $dl_cl_var = $self->_transform_pkg_var("${new_sigil}${base}");
        # Stash element — not supported; emit comment and skip
        if ($dl_cl_var =~ /^\(p-stash /) {
          $self->_emit(";; $perl_code (delete local on stash — not supported, skipped)");
          return;
        }
      }
      elsif (@clean_rhs >= 5
          && ref($clean_rhs[0]) eq 'PPI::Token::Word'     && $clean_rhs[0]->content eq 'delete'
          && ref($clean_rhs[1]) eq 'PPI::Token::Word'     && $clean_rhs[1]->content eq 'local'
          && ref($clean_rhs[2]) eq 'PPI::Token::Symbol'
          && ref($clean_rhs[3]) eq 'PPI::Token::Operator' && $clean_rhs[3]->content eq '->'
          && ref($clean_rhs[4]) eq 'PPI::Structure::Subscript') {
        $dl_sub    = $clean_rhs[4];
        $dl_open   = $dl_sub->start->content;
        # Container is the unboxed referent of the scalar (same as plain delete's
        # `(p-delete (unbox $ref) k)`); the local macros unbox a hash again (no-op
        # on a hash-table) and use a vector directly.
        my $base_cl = $self->_parse_expression([$clean_rhs[2]], $stmt);
        $dl_cl_var  = "(unbox $base_cl)";
      }
      if ($dl_sub) {
        my $sub     = $dl_sub;
        my $open    = $dl_open;
        my $cl_var  = $dl_cl_var;
        my @key_cls = $self->_subscript_key_cl_list($sub, $open, $stmt);
        if (@key_cls) {
          my $macro   = ($open eq '{') ? 'p-local-hash-elem'  : 'p-local-array-elem';
          my $del_fn  = ($open eq '{') ? 'p-delete'            : 'p-delete-array';
          $self->_emit(";; $perl_code");
          # Pre-evaluate original values BEFORE opening local scopes,
          # so the result of "delete local" is the ORIGINAL value (not the fresh nil box).
          my $get_fn = ($open eq '{') ? 'p-gethash' : 'p-aref';
          $self->{_local_counter} //= 0;
          my @del_tmp_vars;
          for my $key_cl (@key_cls) {
            my $tmp = "pcl-del-" . $self->{_local_counter}++;
            push @del_tmp_vars, $tmp;
            $self->_emit("(let (($tmp ($get_fn $cl_var $key_cl)))");
            $self->indent_level($self->indent_level + 1);
            $self->{_local_let_depth} //= 0;
            $self->{_local_let_depth}++;
          }
          # Open local save/restore scope for each key (nested, closed at block end)
          for my $key_cl (@key_cls) {
            $self->_emit("($macro $cl_var $key_cl");
            $self->_emit("  ($del_fn $cl_var $key_cl)");
            $self->indent_level($self->indent_level + 1);
            $self->{_local_let_depth} //= 0;
            $self->{_local_let_depth}++;
          }
          # Emit the assignment inside the local scope using pre-saved values
          if (@vars == 1 && @key_cls == 1) {
            # my $c = delete local $a[N]  or  my $c = delete local $h{k}
            my $var = $vars[0];
            $self->_emit("(box-set $var $del_tmp_vars[0])");
          } else {
            # my ($x,$y) = delete local @a[N,M]  or  my ($x,$y) = delete local @h{k1,k2}
            my $lhs_cl = "(vector " . join(' ', @vars) . ")";
            my $rhs_cl = "(let ((*wantarray* t)) (vector " . join(' ', @del_tmp_vars) . "))";
            $self->_emit("(p-list-= $lhs_cl $rhs_cl)");
          }
          $self->_emit("");
          return;
        }
      }

      if (@vars == 1 && !$lhs_has_placeholder) {
        my $var = $vars[0];
        my $sigil = substr($var, 0, 1);

        if ($sigil eq '$' && $lhs_is_list) {
          # my ($x) = LIST — parenthesized single scalar is a LIST assignment:
          # $x gets the FIRST element (RHS parsed in list context).
          my $rhs_cl = $self->_parse_expression(\@rhs_parts, $stmt, 1) // 'nil';
          $self->_emit("(p-list-= (vector $var) $rhs_cl)");
        } elsif ($sigil eq '$') {
          # my $x = EXPR — scalar assignment; box-set unboxes the source properly
          my $init_cl = $self->_parse_expression(\@rhs_parts, $stmt) // 'nil';
          $self->_emit("(box-set $var $init_cl)");
        } else {
          # Array/hash: check if init was moved to the let binding (self-referential init)
          if (($self->{_my_binding_init_vars} // {})->{$var}) {
            $self->_emit(";; $perl_code (init in let binding)");
          } else {
            # Parse full statement through expression parser for proper list context
            # This generates (p-array-= @arr (vector ...)) or (p-hash-= %h (p-hash ...))
            my $cl_code = $self->_parse_expression($parts, $stmt);
            $self->_emit($cl_code) if defined $cl_code;
          }
        }
      } else {
        # Multiple variables, OR a single var with an `undef` placeholder
        # (my (undef, $x) = …): parse the full statement through the expression
        # parser, which builds the LHS vector with placeholders in position
        # ((vector (p-undef) $x)) so the RHS elements line up.
        my $cl_code = $self->_parse_expression($parts, $stmt);
        $self->_emit($cl_code) if defined $cl_code;
      }
    }
  }

  $self->_emit("");
}

# Helper to check if a PPI structure is empty (for () detection)
sub _is_empty_structure {
  my ($self, $struct) = @_;
  my @children = $struct->children;
  # Filter out whitespace
  @children = grep { ref($_) ne 'PPI::Token::Whitespace' } @children;
  return @children == 0;
}

# Process @ISA declaration - emit CLOS class with parents for MRO
sub _process_isa_declaration {
  my ($self, $stmt, $parts, $init_idx, $perl_code) = @_;

  my $pkg = $self->environment->current_package;

  # Extract parent class names from RHS.  Split into LITERAL parents (known at
  # compile time → baked into the CLOS defclass supers + MRO) and INTERPOLATED
  # parents (a runtime-only class name, e.g. File::Spec's
  # `our @ISA = ("File::Spec::$module")`).  An interpolated parent can't go in
  # the defclass — its name isn't known until run time — so it is only pushed
  # onto @ISA at run time, and method dispatch resolves it via the runtime
  # %pcl-isa-ancestry walk (verified: a string pushed onto @ISA dispatches).
  my @rhs_parts = @$parts[($init_idx + 1) .. $#$parts];
  @rhs_parts = grep { ref($_) ne 'PPI::Token::Whitespace' } @rhs_parts;

  my ($parents, $expr_parents) = $self->_classify_isa_parents(\@rhs_parts);

  $self->_with_bucket('declarations', sub {
    $self->_emit(";; $perl_code");
  });

  if (@$parents) {
    # Emit CLOS class with parent classes for MRO tracking
    my $cl_class = $self->_pkg_to_clos_class($pkg);
    # Package-qualify parent class symbols so they resolve correctly regardless
    # of which CL package the defclass is emitted in.
    # Animal -> Animal::animal (reads as ANIMAL::ANIMAL)
    # Foo::Bar -> |Foo::Bar|::foo--bar (pipe-quoting preserves :: in pkg name)
    my $parents_cl = join(' ', map {
      my $cls = $self->_pkg_to_clos_class($_);
      my $pkg_prefix = cl_pkg($_);
      "$pkg_prefix\:\:$cls"
    } @$parents);

    # Store parent list in environment for later use
    $self->environment->set_isa($pkg, $parents);

    # Redefine the CLOS class with parents for MRO.  Normally this goes in the
    # package's preamble (hoisted before runtime code).  But inside a runtime
    # block ({ package X; our @ISA=... }), the package's *bare* defclass is
    # emitted inline (see _emit_package_preamble), so a preamble "Redefine" would
    # be hoisted BEFORE that bare defclass and then get clobbered by it.  Emit the
    # parented defclass inline instead, so it follows the bare one in the stream.
    if ($self->_block_depth > 0) {
      # Qualify the class name (same read-time-package reason as the bare
      # defclass in _emit_package_preamble's block branch).
      my $q_class = $self->_qualified_clos_class($pkg);
      $self->_emit(";; Redefine CLOS class with parents for MRO");
      $self->_emit("(defclass $q_class ($parents_cl) ())");
    }
    else {
      $self->_with_bucket('preamble', sub {
        $self->_emit(";; Redefine CLOS class with parents for MRO");
        $self->_emit("(defclass $cl_class ($parents_cl) ())");
      });
    }
  }

  # Declare @ISA in declarations bucket, initialize at runtime.
  # When inside a sub OR a runtime block (an inline package whose @ISA push runs
  # under (in-package :Pkg)), qualify @ISA with the package name so the defvar
  # lands in the SAME package the push targets — otherwise the runtime
  # %pcl-isa-ancestry walk reads an unpopulated Pkg::@ISA and inheritance breaks.
  my $qualify = ($self->environment->in_subroutine > 0
                 || $self->_block_depth > 0);
  my $isa_sym = $qualify ? $self->_qualified_isa_symbol($pkg) : "\@ISA";
  $self->_with_bucket('declarations', sub {
    $self->_emit(global_decl_form("$isa_sym", "(make-array 0 :adjustable t :fill-pointer 0)"));
  });
  for my $parent (@$parents) {
    $self->_emit("(p-push $isa_sym \"$parent\")");
  }
  # Interpolated parents: push the runtime-evaluated class-name string.
  for my $expr_cl (@$expr_parents) {
    $self->_emit("(p-push $isa_sym $expr_cl)");
  }

  $self->_emit("");
}

# Classify @ISA RHS elements into literal parent names (compile-time, for the
# CLOS defclass) and interpolated parent expressions (runtime CL strings, for a
# runtime push).  Returns (\@literal_names, \@expr_cl).  An interpolating quote
# (double-quoted / qq) that actually contains a sigil is treated as runtime;
# everything else (qw, single-quoted, sigil-free double-quoted) is literal.
sub _classify_isa_parents {
  my ($self, $parts) = @_;
  my (@literal, @expr);

  for my $part (@$parts) {
    my $ref = ref($part);
    if ($ref eq 'PPI::Token::Quote::Double'
        || $ref eq 'PPI::Token::Quote::Interpolate') {
      if ($part->string =~ /[\$\@]/) {
        # Interpolated, runtime-only class name → (p-string-concat ...).
        my $cl = $self->_parse_expression([$part]);
        $cl =~ s/^[ \t]+//;
        push @expr, $cl if defined $cl && length $cl;
        next;
      }
      push @literal, $part->string;       # double-quoted but no sigil
    }
    elsif ($ref eq 'PPI::Structure::List') {
      for my $child ($part->schildren) {
        if ($child->isa('PPI::Statement::Expression')) {
          my ($l, $e) = $self->_classify_isa_parents([$child->schildren]);
          push @literal, @$l;
          push @expr,    @$e;
        }
        elsif ($child->isa('PPI::Token::Quote')) {
          my ($l, $e) = $self->_classify_isa_parents([$child]);
          push @literal, @$l;
          push @expr,    @$e;
        }
      }
    }
    else {
      # qw(...), single-quoted, etc. — all literal: reuse the existing extractor.
      push @literal, $self->_extract_parent_classes([$part]);
    }
  }

  @literal = grep { defined $_ && $_ ne '' } @literal;
  return (\@literal, \@expr);
}

# Process 'use base' / 'use parent' - equivalent to push @ISA, ...
# Also sets up CLOS inheritance for MRO.
sub _process_use_base {
  my ($self, $stmt, $perl_code, $module) = @_;

  # Extract parent class names from the argument list
  my @parents;
  my $skip_next = 0;
  my $norequire = 0;   # 'use parent -norequire, ...' suppresses the implicit require
  for my $child ($stmt->children) {
    my $ref = ref($child);
    # 'use parent -norequire, qw(...)' — PPI tokenizes -norequire as a single
    # Word "-norequire" (not operator '-' + word).  Handle both spellings.
    if ($ref eq 'PPI::Token::Word' && $child->content eq '-norequire') {
      $norequire = 1; next;
    }
    if ($ref eq 'PPI::Token::Operator' && $child->content eq '-') {
      $skip_next = 1; next;
    }
    if ($skip_next && $ref eq 'PPI::Token::Word') {
      $norequire = 1 if $child->content eq 'norequire';
      $skip_next = 0; next;
    }
    $skip_next = 0;
    if ($ref eq 'PPI::Token::QuoteLike::Words') {
      my $content = $child->content;
      $content =~ s/^qw[^\w\s]//;
      $content =~ s/[^\w\s]$//;
      push @parents, split /\s+/, $content;
    }
    elsif ($ref eq 'PPI::Token::Quote::Single' || $ref eq 'PPI::Token::Quote::Double') {
      push @parents, $child->string;
    }
    elsif ($ref eq 'PPI::Structure::List') {
      for my $item ($child->children) {
        next if ref($item) =~ /Whitespace|Separator/;
        if (ref($item) eq 'PPI::Token::Quote::Single' || ref($item) eq 'PPI::Token::Quote::Double') {
          push @parents, $item->string;
        }
      }
    }
  }
  @parents = grep { /\S/ } @parents;
  return unless @parents;

  my $pkg = $self->environment->current_package;

  # Redefine CLOS class with parents for MRO
  my $cl_class = $self->_pkg_to_clos_class($pkg);
  my $parents_cl = join(' ', map {
    my $cls = $self->_pkg_to_clos_class($_);
    my $pkg_prefix = cl_pkg($_);
    "$pkg_prefix\:\:$cls"
  } @parents);
  $self->environment->set_isa($pkg, \@parents);
  $self->_with_bucket('preamble', sub {
    $self->_emit(";; $perl_code");
    # Perl's `use parent`/`use base` does an implicit `require` of each parent
    # (unless -norequire).  Emit it BEFORE the defclass: loading the parent both
    # brings in its methods AND creates the Parent:: package, without which the
    # `(defclass child (Parent::class) ...)` form below cannot even be READ.
    # p-require-parent is non-fatal (inline same-file parents have no .pm).
    unless ($norequire) {
      for my $parent (@parents) {
        $self->_emit("(p-eval-always (p-require-parent \"$parent\"))");
      }
    }
    # Pre-declare each parent's PACKAGE regardless: the require can fail
    # legitimately (a parent defined inside another module's file, e.g.
    # Tie::StdScalar in Tie/Scalar.pm, whose `use Tie::Scalar` now runs at
    # its source position in the compile stream, possibly AFTER this form) —
    # the defclass below must still READ.  p-defpackage is idempotent, and
    # CL allows a forward-referenced superclass at evaluation time.
    for my $parent (@parents) {
      $self->_emit("(p-eval-always (p-defpackage "
                   . $self->_cl_pkg_designator($parent) . "))");
    }
    $self->_emit("(defclass $cl_class ($parents_cl) ())");
  });

  # Declare @ISA in declarations bucket, push parents at load time
  $self->_with_bucket('declarations', sub {
    $self->_emit(global_decl_form("\@ISA", "(make-array 0 :adjustable t :fill-pointer 0)"));
  });
  for my $parent (@parents) {
    $self->_emit("(p-push \@ISA \"$parent\")");
  }
  $self->_emit("");
}

# Extract parent class names from an @ISA initializer expression
# Handles: qw(Parent1 Parent2), ('Parent1', 'Parent2'), ("Parent")
sub _extract_parent_classes {
  my ($self, $parts) = @_;
  my @parents;

  for my $part (@$parts) {
    my $ref = ref($part);

    if ($ref eq 'PPI::Token::QuoteLike::Words') {
      # qw(Parent1 Parent2) — strip the qw and ANY delimiter, not just brackets.
      # `our @ISA = qw/ Foo /` (slash, or !|#, etc.) must work too; the old
      # bracket-only strip left "qw/" and "/" as bogus parent names → a broken
      # (defclass ... (qw/::plc-qw/ ... /::plc-/)) that fails to READ.
      # Mirrors the general strip in _process_use_base.
      my $content = $part->content;
      $content =~ s/^qw\s*[^\w\s]//;
      $content =~ s/[^\w\s]$//;
      push @parents, split(/\s+/, $content);
    }
    elsif ($ref eq 'PPI::Token::Quote::Single'
	   || $ref eq 'PPI::Token::Quote::Double') {
      # 'Parent' or "Parent"
      push @parents, $part->string;
    }
    elsif ($ref eq 'PPI::Structure::List') {
      # (list) - recurse into children
      for my $child ($part->schildren) {
        if ($child->isa('PPI::Statement::Expression')) {
          push @parents, $self->_extract_parent_classes([$child->schildren]);
        }
        elsif ($child->isa('PPI::Token::Quote')) {
          push @parents, $child->string;
        }
      }
    }
  }

  return grep { defined $_ && $_ ne '' } @parents;
}

# Process 'local' variable declaration - dynamic scoping
# Emits a (let ...) that stays open until block end
# Split a trailing `if`/`unless` statement modifier off the RHS parts of a
# `local LHS = RHS if/unless COND` declaration.  A bare `if`/`unless` Word can
# only be the statement modifier here (ternaries use `?:`, hash keys live inside
# a Subscript), so the first one ends the value expression.  Truncates
# @$rhs_parts to just the value and returns ($modifier, \@cond_parts); ('', [])
# when there is no modifier.
sub _split_local_init_modifier {
  my ($self, $rhs_parts) = @_;
  for my $i (0 .. $#$rhs_parts) {
    my $p = $rhs_parts->[$i];
    if (ref($p) eq 'PPI::Token::Word' && $p->content =~ /^(?:if|unless)$/) {
      my $mod  = $p->content;
      my @cond = @$rhs_parts[$i + 1 .. $#$rhs_parts];
      splice(@$rhs_parts, $i);
      return ($mod, \@cond);
    }
  }
  return ('', []);
}

# Build the init form for a conditional `local LHS = RHS if/unless COND`.
# Perl localizes only when the condition selects RHS; otherwise the slot keeps
# its current value.  We always localize and make the *value* conditional:
# `COND ? RHS : <current value of LHS>` (swapped for `unless`).  Localizing to
# the current value is observationally identical to not localizing (it is
# saved and restored unchanged), and reuses the ordinary local-init machinery —
# no special macro (cf. p-local-glob-if, which a glob needs because it has no
# single rvalue).  $lhs_rval_cl reads the LHS as an rvalue (old value).
sub _conditional_local_init {
  my ($self, $modifier, $cond_cl, $rhs_cl, $lhs_rval_cl) = @_;
  my $test = "(p-true-p $cond_cl)";
  return $modifier eq 'unless'
    ? "(if $test $lhs_rval_cl $rhs_cl)"
    : "(if $test $rhs_cl $lhs_rval_cl)";
}

sub _process_local_declaration {
  my $self = shift;
  my $stmt = shift;
  my $parts = shift;
  my $perl_code = shift;

  # Handle local *foo and local *foo = RHS (typeglob localization)
  # Use p-local-glob which saves/restores all slots via unwind-protect.
  # @parts includes the 'local' keyword as first element — skip it.
  my @non_ws = grep {
    my $r = ref($_);
    $r ne 'PPI::Token::Whitespace'
    && !($r eq 'PPI::Token::Word' && $_->content eq 'local')
  } @$parts;

  # ── Pre-unwrap: local(*foo) — single symbol in parens. Unwrap before typeglob check.
  if (@non_ws >= 1 && ref($non_ws[0]) eq 'PPI::Structure::List') {
    my @flat;
    for my $child ($non_ws[0]->children) {
      my $cr = ref($child);
      next if $cr eq 'PPI::Token::Whitespace';
      next if $cr eq 'PPI::Token::Structure';
      if ($cr =~ /^PPI::Statement/) {
        for my $gc ($child->children) {
          next if ref($gc) eq 'PPI::Token::Whitespace';
          push @flat, $gc;
        }
      } else {
        push @flat, $child;
      }
    }
    my $has_comma = grep {
      ref($_) eq 'PPI::Token::Operator' && $_->content eq ','
    } @flat;
    if (@flat == 1 && ref($flat[0]) eq 'PPI::Token::Symbol') {
      # local(*foo) or local($scalar) with no subscript — unwrap the parens
      splice(@non_ws, 0, 1, @flat);
    }
    elsif (@flat >= 2 && !$has_comma
           && ref($flat[0]) eq 'PPI::Token::Symbol') {
      # local($h{k}) / local($a[i]) / local($ref->{k}) / local($ref->[i]) —
      # a single subscripted lvalue in parens.  Unwrap so the subscript-aware
      # handlers below (Symbol+Subscript / Symbol+'->'+Subscript) fire; without
      # this it falls through to the generic list-local path which clobbers the
      # base scalar (e.g. `local($s->{apad}) = ...` overwrote $s).
      splice(@non_ws, 0, 1, @flat);
    }
  }

  if (@non_ws && ref($non_ws[0]) eq 'PPI::Token::Symbol'
      && $non_ws[0]->content =~ /^\*(.+)$/) {
    my $glob_content = $non_ws[0]->content;  # e.g. "*foo" or "*Pkg::foo"
    my ($pkg, $name);
    if ($glob_content =~ /^\*(.*)::([^:]+)$/) {
      ($pkg, $name) = ($1 || 'main', $2);
    } else {
      $glob_content =~ /^\*(\w+)$/;
      $name = $1;
      $pkg  = $self->environment ? $self->environment->current_package : 'main';
      $pkg //= 'main';
    }
    $self->_emit(";; $perl_code");
    # Find initializer (after '=')
    my $has_init = grep { ref($_) eq 'PPI::Token::Operator' && $_->content eq '=' } @non_ws;
    if ($has_init) {
      my @rhs_parts;
      my @cond_parts;
      my $modifier;        # 'if' / 'unless' statement modifier, if present
      my $past_eq = 0;
      for my $p (@non_ws) {
        if (!$past_eq && ref($p) eq 'PPI::Token::Operator' && $p->content eq '=') {
          $past_eq = 1;
          next;
        }
        next unless $past_eq;
        # A trailing `if`/`unless` bareword is the statement modifier (it cannot
        # appear inside a value expression): `local *_ = RHS if COND`.
        if (!$modifier && ref($p) eq 'PPI::Token::Word'
            && $p->content =~ /^(?:if|unless)$/) {
          $modifier = $p->content;
          next;
        }
        if ($modifier) { push @cond_parts, $p; }
        else           { push @rhs_parts,  $p; }
      }
      my $rhs_cl = $self->_parse_expression(\@rhs_parts, $stmt) // 'nil';
      if ($modifier) {
        # Conditional local (`local *foo = RHS if COND`): only localize+assign
        # when COND is true; otherwise the rest of the scope keeps the outer
        # slots.  p-local-glob-if always saves/restores but evaluates RHS (while
        # the slots are still intact, so it can read @_) and clears+assigns only
        # when COND holds.  Push truthiness into a p-true-p test here.
        my $cond_cl = $self->_parse_expression(\@cond_parts, $stmt) // 'nil';
        my $test = $modifier eq 'unless'
                 ? "(not (p-true-p $cond_cl))" : "(p-true-p $cond_cl)";
        $self->_emit("(p-local-glob-if $test \"$pkg\" \"$name\" $rhs_cl");
        $self->indent_level($self->indent_level + 1);
        $self->{_local_let_depth} //= 0;
        $self->{_local_let_depth}++;
      } else {
        # Perl evaluates the RHS of `local *foo = EXPR` in the ENCLOSING scope,
        # BEFORE *foo is localized.  This matters because localizing *_ clears the
        # @_ slot too, so an RHS that reads @_ (e.g. local *_ = \join('', @_), the
        # Text::ParseWords idiom) must see the old @_.  Bind the RHS in a wrapping
        # let so it is computed before p-local-glob clears slots.
        $self->{_local_glob_counter} //= 0;
        my $rhs_tmp = '--local-glob-rhs--' . $self->{_local_glob_counter}++;
        $self->_emit("(let (($rhs_tmp $rhs_cl))");
        $self->indent_level($self->indent_level + 1);
        $self->_emit("(p-local-glob \"$pkg\" \"$name\"");
        $self->indent_level($self->indent_level + 1);
        $self->_emit("(p-glob-assign \"$pkg\" \"$name\" $rhs_tmp)");
        # Two wrapping forms (let + p-local-glob) → two closing parens at scope end.
        $self->{_local_let_depth} //= 0;
        $self->{_local_let_depth} += 2;
      }
    } else {
      $self->_emit("(p-local-glob \"$pkg\" \"$name\"");
      $self->indent_level($self->indent_level + 1);
      $self->{_local_let_depth} //= 0;
      $self->{_local_let_depth}++;
    }
    $self->_emit("");
    return;
  }

  # ── Handle local $#array = N (RT #7411).
  # Perl's local on the array-length magic ($#a) changes the length but does NOT
  # restore it on scope exit — a long-standing Perl limitation (RT #7411: the
  # "after local($#a) ... should be restored" tests in perl-tests/local.t are
  # marked local $::TODO).  Match Perl exactly: emit a plain length-set with no
  # save/restore wrapper.  PPI tokenizes $#a as PPI::Token::ArrayIndex, which the
  # generic symbol/list extraction below does not recognize (so the statement was
  # silently dropped).
  if (@non_ws && ref($non_ws[0]) eq 'PPI::Token::ArrayIndex') {
    $self->_emit(";; $perl_code");
    my $cl = $self->_parse_expression(\@non_ws, $stmt) // 'nil';
    $self->_emit($cl);
    return;
  }

  # ── Unwrap local(ELEM) parens form: local($a[N]) / local($h{key}) / local(@a[N,M])
  # PPI gives Structure::List when parens are used; unwrap it so the handler below fires.
  if (@non_ws >= 1 && ref($non_ws[0]) eq 'PPI::Structure::List') {
    my @flat;
    for my $child ($non_ws[0]->children) {
      my $cr = ref($child);
      next if $cr eq 'PPI::Token::Whitespace';
      next if $cr eq 'PPI::Token::Structure';   # skip '(' and ')'
      if ($cr =~ /^PPI::Statement/) {
        for my $gc ($child->children) {
          next if ref($gc) eq 'PPI::Token::Whitespace';
          push @flat, $gc;
        }
      } else {
        push @flat, $child;
      }
    }
    if (@flat == 2
        && ref($flat[0]) eq 'PPI::Token::Symbol'
        && ref($flat[1]) eq 'PPI::Structure::Subscript') {
      splice(@non_ws, 0, 1, @flat);
    }
    elsif (@flat == 1
           && ref($flat[0]) eq 'PPI::Token::Symbol') {
      # local(*foo), local($scalar), etc. — single symbol in parens
      splice(@non_ws, 0, 1, @flat);
    }
  }

  # ── Handle local $hash{key}, local @arr[N], local @hash{@keys}, local @arr[N,M]
  # PPI gives: Symbol("$hash") + Structure::Subscript("{key}").  Also the
  # arrow-deref form local $ref->{key} / $ref->[N] (Symbol -> Subscript), whose
  # container is the unboxed referent.  Without the arrow branch the `local` was
  # silently dropped and `$ref` itself was mis-bound to the RHS (a crash).
  my ($ld_sub, $ld_cl_var, $ld_sub_idx);
  if (@non_ws >= 2
      && ref($non_ws[0]) eq 'PPI::Token::Symbol'
      && ref($non_ws[1]) eq 'PPI::Structure::Subscript') {
    my $sym       = $non_ws[0]->content;           # e.g. "$hash", "@arr"
    $ld_sub       = $non_ws[1];
    $ld_sub_idx   = 1;
    my $open      = $ld_sub->start()->content();    # '{' or '['
    my $base      = substr($sym, 1);               # "hash" or "arr"
    my $new_sigil = ($open eq '{') ? '%' : '@';
    $ld_cl_var    = $self->_transform_pkg_var("${new_sigil}${base}");
    # Stash slice/element: $Pkg::{key} — stash manipulation is not supported.
    if ($ld_cl_var =~ /^\(p-stash /) {
      $self->_emit(";; $perl_code (stash element local — not supported, running body only)");
      return;
    }
  }
  elsif (@non_ws >= 3
      && ref($non_ws[0]) eq 'PPI::Token::Symbol'
      && ref($non_ws[1]) eq 'PPI::Token::Operator' && $non_ws[1]->content eq '->'
      && ref($non_ws[2]) eq 'PPI::Structure::Subscript') {
    $ld_sub     = $non_ws[2];
    $ld_sub_idx = 2;
    my $base_cl = $self->_parse_expression([$non_ws[0]], $stmt);
    $ld_cl_var  = "(unbox $base_cl)";
  }
  if ($ld_sub) {
    my $sub   = $ld_sub;
    my $open  = $sub->start()->content();       # '{' or '['
    my $cl_var = $ld_cl_var;

    # Extract individual key/index expressions from the subscript
    my @key_groups = $self->_subscript_key_groups($sub);
    if (@key_groups) {
      # Check for initializer (= expr) after the subscript
      my ($has_init, @rhs_parts);
      for my $i (($ld_sub_idx + 1) .. $#non_ws) {
        if (ref($non_ws[$i]) eq 'PPI::Token::Operator' && $non_ws[$i]->content eq '=') {
          $has_init = 1;
          @rhs_parts = @non_ws[($i + 1) .. $#non_ws];
          last;
        }
      }
      # Strip a trailing `if`/`unless` statement modifier from the RHS so it does
      # not leak into the value parse (which fell through to a "Missing case" die).
      my ($ld_mod, $ld_cond) = $has_init
        ? $self->_split_local_init_modifier(\@rhs_parts) : ('', []);
      # Slice form (multiple keys): the RHS is a list assignment — parse it in
      # LIST_CTX so a literal (a, b) emits (vector a b), not a scalar progn.
      # (The old runtime (if *wantarray* …) shape hid this; gen_progn now
      # honours the static annotation.)  Single element keeps scalar context.
      my $init_cl = $has_init
        ? ($self->_parse_expression(\@rhs_parts, $stmt, (@key_groups > 1 ? 1 : 0)) // 'nil')
        : undef;

      $self->_emit(";; $perl_code");

      # Parse all key CL expressions up front (need them for both macro open and init)
      # Array subscripts use LIST_CTX so that 1..2 generates a range vector instead
      # of a flip-flop (which would trigger $SIG{__WARN__} via uninitialized $..).
      my $sub_ctx = ($open eq '[') ? 1 : 0;
      my @key_cls = map { $self->_subscript_key_expr($_, $open, $stmt, $sub_ctx) } @key_groups;

      # Conditional `local $h{k} = V if COND`: make the value conditional on COND,
      # falling back to the element's current value (read before the fresh box is
      # installed by the *-init macro).
      if ($ld_mod && defined $init_cl) {
        my $cond_cl = $self->_parse_expression($ld_cond, $stmt) // 'nil';
        my $lhs_rval = @key_cls == 1
          ? ($open eq '{' ? "(p-gethash $cl_var $key_cls[0])"
                          : "(p-aref $cl_var $key_cls[0])")
          : ($open eq '{' ? "(p-hslice $cl_var " . join(' ', @key_cls) . ")"
                          : "(p-aslice $cl_var " . join(' ', @key_cls) . ")");
        $init_cl = $self->_conditional_local_init($ld_mod, $cond_cl, $init_cl, $lhs_rval);
      }

      # Choose the macro based on subscript type.
      # p-local-array-slice handles both scalar and vector (range) indices.
      my $macro      = ($open eq '{') ? 'p-local-hash-elem'      : 'p-local-array-slice';
      my $macro_init = ($open eq '{') ? 'p-local-hash-elem-init'  : 'p-local-array-elem-init';

      if (defined $init_cl && @key_cls == 1) {
        # Single element with initializer: use the *-init macro which evaluates
        # init-form BEFORE installing the fresh box, preventing stale-read bugs
        # like local($a[2]) = $a[2] reading the fresh undef box.
        my $key_cl = $key_cls[0];
        $self->_emit("($macro_init $cl_var $key_cl $init_cl");
        $self->indent_level($self->indent_level + 1);
        $self->{_local_let_depth} //= 0;
        $self->{_local_let_depth}++;
      } elsif (defined $init_cl) {
        # Slice with initializer: pre-evaluate RHS before any macros open,
        # then emit nested macro opens, then assign using the saved value.
        $self->{_local_counter} //= 0;
        my $tmp = "pcl-local-init-" . $self->{_local_counter}++;
        my $ctx = "(let ((*wantarray* t)) ";
        my $ctx_close = ")";
        $self->_emit("(let (($tmp ${ctx}$init_cl${ctx_close}))");
        $self->indent_level($self->indent_level + 1);
        $self->{_local_let_depth}++;
        for my $key_cl (@key_cls) {
          $self->_emit("($macro $cl_var $key_cl");
          $self->indent_level($self->indent_level + 1);
          $self->{_local_let_depth}++;
        }
        my $keys_str = join(' ', @key_cls);
        if ($open eq '{') {
          $self->_emit("(let ((*wantarray* t)) (p-setf (p-hslice $cl_var $keys_str) $tmp))");
        } else {
          $self->_emit("(let ((*wantarray* t)) (p-setf (p-aslice $cl_var $keys_str) $tmp))");
        }
      } else {
        # No initializer: emit one macro call per key (nested open forms)
        for my $key_cl (@key_cls) {
          $self->_emit("($macro $cl_var $key_cl");
          $self->indent_level($self->indent_level + 1);
          $self->{_local_let_depth} //= 0;
          $self->{_local_let_depth}++;
        }
      }

      $self->_emit("");
      return;
    }
  }

  # ── local on a deref / symbolic ref: local ${EXPR}, local $$x, @{…}, %$x, …
  # PPI gives Cast($/@/%) followed by either a Block ({EXPR}) or a Symbol ($x).
  # Only the *symbolic* (string) ref form is localizable; localizing through a
  # hard reference dies at runtime ("Can't localize through a reference").
  # Resolution + save/restore lives in the p-local-deref-{scalar,array,hash}
  # runtime macros (the value of EXPR decides symbolic-vs-hard at run time).
  if (@non_ws >= 2
      && ref($non_ws[0]) eq 'PPI::Token::Cast'
      && $non_ws[0]->content =~ /^[\$\@%]$/
      && (ref($non_ws[1]) eq 'PPI::Structure::Block'
          || ref($non_ws[1]) eq 'PPI::Token::Symbol')) {
    my $sigil = $non_ws[0]->content;
    my $ref_cl;
    if (ref($non_ws[1]) eq 'PPI::Token::Symbol') {
      # $$x / @$x / %$x — deref the named scalar
      $ref_cl = $self->_parse_expression([$non_ws[1]], $stmt) // 'nil';
    } else {
      # ${EXPR} — extract the block's inner expression
      my @inner;
      for my $child ($non_ws[1]->children) {
        my $cr = ref($child);
        next if $cr eq 'PPI::Token::Whitespace';
        next if $cr eq 'PPI::Token::Structure';   # the { }
        if ($cr =~ /^PPI::Statement/) {
          for my $gc ($child->children) {
            next if ref($gc) eq 'PPI::Token::Whitespace';
            push @inner, $gc;
          }
        } else {
          push @inner, $child;
        }
      }
      if (@inner == 1 && ref($inner[0]) eq 'PPI::Token::Word') {
        # ${aa} — a bareword names the package variable; route as a symbolic ref.
        my $name = $inner[0]->content;
        $ref_cl = "\"$name\"";
      } else {
        $ref_cl = $self->_parse_expression(\@inner, $stmt) // 'nil';
      }
    }
    my $macro = $sigil eq '@' ? 'p-local-deref-array'
              : $sigil eq '%' ? 'p-local-deref-hash'
              :                 'p-local-deref-scalar';
    $self->_emit(";; $perl_code");
    $self->_emit("($macro $ref_cl");
    $self->indent_level($self->indent_level + 1);
    $self->{_local_let_depth} //= 0;
    $self->{_local_let_depth}++;
    $self->_emit("");
    return;
  }

  # Detect 'local our $var' — the 'our' qualifier means a package variable.
  # Emit a defvar so the variable is declared as special before local binds it.
  my $has_our = (@non_ws && ref($non_ws[0]) eq 'PPI::Token::Word'
                          && $non_ws[0]->content eq 'our');
  if ($has_our) {
    my $pkg = $self->environment ? $self->environment->current_package : 'main';
    $self->_with_bucket('declarations', sub {
      for my $p (@non_ws[1..$#non_ws]) {
        next if ref($p) ne 'PPI::Token::Symbol';
        my $var = $p->content;
        last if $var =~ /^=/; # stop at '=' (won't happen but be safe)
        my $sigil = substr($var, 0, 1);
        my $init  = $sigil eq '$' ? '(make-p-box nil)'
                  : $sigil eq '@' ? '(make-array 0 :adjustable t :fill-pointer 0)'
                  :                 "(make-hash-table :test #'equal)";
        my $cl_var = "${pkg}::${var}";
        $self->_emit(global_decl_form("$cl_var", "$init"));
        $self->environment->add_our_variable($pkg, $var) if $self->environment;
      }
    });
  }

  # Find variable and optional initializer
  my @vars;
  my $init_idx = -1;

  for my $i (0 .. $#$parts) {
    my $p = $parts->[$i];
    my $ref = ref($p);

    if ($ref eq 'PPI::Token::Symbol' || $ref eq 'PPI::Token::Magic') {
      push @vars, $self->_transform_pkg_var($p->content);
    }
    elsif ($ref eq 'PPI::Structure::List') {
      # Use undef-aware extraction so local(undef, $a, undef, $b) keeps skip markers
      push @vars, $self->_find_symbols_and_undefs_in_list($p);
    }
    elsif ($ref eq 'PPI::Token::Operator' && $p->content eq '=') {
      $init_idx = $i;
      last;
    }
  }

  return unless @vars;

  # Whole-stash local — `local %Pkg::` (and `local *Pkg::`) — is stash
  # localization, which PCL does not support (see not-supported.md "Live
  # symbol-table hashes").  _transform_pkg_var renders these as a (p-stash ...)
  # form, which is NOT a valid let-binding place (it crashed the SBCL compile,
  # aborting the whole file).  Drop such vars; if that leaves nothing to
  # localize, run the body unshadowed.
  if (grep { /^\(p-stash / } @vars) {
    @vars = grep { !/^\(p-stash / } @vars;
    $self->_emit(";; $perl_code (whole-stash local — not supported, skipped)");
    return unless @vars;
  }

  $self->_emit(";; $perl_code");

  # local $. — the line-number magic refers to the last-accessed filehandle, so
  # localizing it must save/restore (current handle, its line counter), not
  # rebind a plain box (which would read undef inside the scope).  p-local-dot
  # wraps the rest of the block; closed by _local_let_depth at block end.
  if (@vars == 1 && ($vars[0] eq '$.' || $vars[0] eq '|$.|') && $init_idx < 0) {
    $self->_emit("(p-local-dot");
    $self->indent_level($self->indent_level + 1);
    $self->{_local_let_depth} //= 0;
    $self->{_local_let_depth}++;
    return;
  }

  # local $| — same shape as $.: the autoflush value is magic (writes clamp to
  # 0/1), so localizing must rebind the underlying dynamic value, not shadow
  # the box (which would drop the clamp inside the scope).
  if (@vars == 1 && ($vars[0] eq '$|' || $vars[0] eq '|$\\||') && $init_idx < 0) {
    $self->_emit("(p-local-pipe");
    $self->indent_level($self->indent_level + 1);
    $self->{_local_let_depth} //= 0;
    $self->{_local_let_depth}++;
    return;
  }

  # Build the localization bindings.  Each entry is [PLACE, INIT-FORM]; the
  # emission below splits them by Pl::GlobalPartition (task #289): an ORDINARY
  # package global becomes a `p-local-cell` open (its storage is a global cell,
  # not a dynamic binding, so a `let` would install a LEXICAL shadow that no
  # called sub can see), while temporaries and exception-set names keep today's
  # dynamic let/let*.
  my @bindings;
  my $use_let_star = 0;
  my $local_tail_cl;          # #138: `local $x = A, B` — B, emitted in the let
  if ($init_idx >= 0 && @vars == 1) {
    # local $x = value
    my @rhs_parts = @$parts[($init_idx + 1) .. $#$parts];
    @rhs_parts = grep { ref($_) ne 'PPI::Token::Whitespace' } @rhs_parts;

    # Strip a trailing `if`/`unless` statement modifier (see element branch).
    my ($lmod, $lcond) = $self->_split_local_init_modifier(\@rhs_parts);

    # #138: `local $x = A, B;` is `(local $x = A), B` — assignment binds
    # tighter than `,`/`=>`/`or`/`and`/`xor`, so the tail is NOT part of the
    # initializer (it folded, and $x got B's value).  The `local` let wraps
    # the block remainder, so the tail cannot simply be hoisted out: it is
    # emitted as the first form INSIDE the let, where perl also runs it (the
    # localization is already in effect by then).  When the comma may belong
    # to a parenless list operator instead, the split is declined and the run
    # stays whole — see Pl::PExpr::TokenUtils::lowprec_split_safe.
    my $lp = $lmod ? undef : Pl::PExpr::TokenUtils::lowprec_idx(\@rhs_parts, 0);
    if (defined $lp
        && Pl::PExpr::TokenUtils::lowprec_split_safe(\@rhs_parts, 0, $lp)) {
      # `or`/`and`/`xor` test the localized variable's NEW value, so they
      # cannot become a separate statement — only a comma tail can.
      if ($rhs_parts[$lp]->content =~ /^(?:,|=>)$/) {
        my @tail = @rhs_parts[$lp + 1 .. $#rhs_parts];
        splice(@rhs_parts, $lp);
        $local_tail_cl = $self->_parse_expression(\@tail, $stmt) if @tail;
      }
    }

    my $var = $vars[0];
    # For qualified vars (e.g. A::@ISA), the sigil is embedded after '::'.
    # For simple vars (e.g. @arr), it is the first character.
    my ($sigil) = ($var =~ /::([%\@\$])/) ? ($1) : (substr($var, 0, 1));

    # Use LIST_CTX for array/hash RHS so '..' generates a range, not a flip-flop
    my $rhs_ctx = ($sigil eq '@' || $sigil eq '%') ? 1 : 0;
    my $init_cl = $self->_parse_expression(\@rhs_parts, $stmt, $rhs_ctx) // 'nil';

    # Conditional `local $x = V if COND`: value is COND ? RHS : current value, so
    # a false condition localizes to (and restores) the unchanged current value.
    if ($lmod) {
      my $cond_cl  = $self->_parse_expression($lcond, $stmt) // 'nil';
      my $lhs_rval = $sigil eq '$' ? "(unbox $var)" : $var;
      $init_cl = $self->_conditional_local_init($lmod, $cond_cl, $init_cl, $lhs_rval);
    }

    if ($var eq '$!' || $var eq '|$!|') {
      # local $! = N: bind *p-stored-errno* (auto-restored by let) and set C errno
      push @bindings, ["pcl::*p-stored-errno*", "(pcl::%pcl-local-errno-init $init_cl)"];
    }
    elsif ($sigil eq '@') {
      # local @arr = EXPR: evaluate EXPR with old @arr, make an independent copy.
      # Special case: when EXPR is (p-array-= VAR RHS), p-array-= mutates VAR in-place
      # during let binding evaluation. CL saves the symbol-value POINTER before binding,
      # but p-array-= has already mutated the pointed-to vector. On let exit, CL restores
      # the pointer to the (now-mutated) old vector — giving the wrong restored value.
      #
      # Fix: detect (p-array-= VAR RHS) and bypass the in-place mutation:
      #   Same var (local @bee = local(@bee) = RHS): use inner RHS for p-copy-array.
      #   Different var (local @bim = local(@bee) = RHS): give @bee its own let binding.
      (my $init_cl_trimmed = $init_cl) =~ s/^\s+|\s+$//gs;
      if ($init_cl_trimmed =~ /^\(p-array-= (\S+) (.+)\)$/s) {
        my ($mutated_var, $inner_rhs) = ($1, $2);
        if ($mutated_var eq $var) {
          # Same-var: skip the p-array-= mutation; copy the RHS directly.
          push @bindings, ["$var", "(p-copy-array (let ((*wantarray* t)) $inner_rhs))"];
        } else {
          # Different-var: bind BOTH vars so CL saves/restores each independently.
          $self->{_local_counter} //= 0;
          my $tmp = "pcl-local-inner-" . $self->{_local_counter}++;
          unshift @bindings, ["$tmp", "(let ((*wantarray* t)) $inner_rhs)"];
          push @bindings, ["$mutated_var", "(p-copy-array $tmp)"];
          push @bindings, ["$var", "(p-copy-array $tmp)"];
          $use_let_star = 1;
        }
      } else {
        push @bindings, ["$var", "(p-copy-array (let ((*wantarray* t)) $init_cl))"];
      }
    }
    elsif ($sigil eq '%') {
      # local %h = EXPR: evaluate EXPR with old %h, make an independent copy.
      push @bindings, ["$var", "(p-copy-hash (let ((*wantarray* t)) $init_cl))"];
    }
    else {
      push @bindings, ["$var", "(p-box-for-local $init_cl)"];
    }
  }
  else {
    # Bare local or multiple vars - just shadow with nil/empty.
    # Skip undef markers (they are skip slots, not real variables).
    for my $var (@vars) {
      next if $var eq '(p-undef)';  # undef slot: no binding needed
      my ($sigil) = ($var =~ /::([%\@\$])/) ? ($1) : (substr($var, 0, 1));
      if ($var eq '$!' || $var eq '|$!|') {
        # bare local $!: save/restore *p-stored-errno*, clear to 0 (Perl undef $! = 0)
        push @bindings, ["pcl::*p-stored-errno*", "0"];
      }
      elsif ($sigil eq '@') {
        push @bindings, ["$var", "(make-array 0 :adjustable t :fill-pointer 0)"];
      }
      elsif ($sigil eq '%') {
        push @bindings, ["$var", "(make-hash-table :test 'equal)"];
      }
      else {
        push @bindings, ["$var", "(make-p-box nil)"];
      }
    }
  }

  # For multi-var local with initializer: local($a, $b, @arr) = @_
  # Pre-evaluate the RHS BEFORE the let bindings so that variables in the RHS
  # (e.g. @arr) still refer to their OLD values, not the freshly-bound empty ones.
  # Example: local (undef, @bee) = @bee  — @bee on RHS must see old @bee.
  # Use let* with the RHS as the first binding, then the fresh variable slots.
  my ($rhs_tmp_cl);
  if ($init_idx >= 0 && @vars > 1) {
    my @rhs_parts = @$parts[($init_idx + 1) .. $#$parts];
    @rhs_parts = grep { ref($_) ne 'PPI::Token::Whitespace' } @rhs_parts;
    my $rhs_cl = $self->_parse_expression(\@rhs_parts, $stmt, 1) // 'nil';  # 1 = LIST_CTX
    $rhs_cl = "(let ((*wantarray* t) (*p-in-list-assign-rhs* t)) $rhs_cl)";
    $self->{_local_counter} //= 0;
    $rhs_tmp_cl = "pcl-local-rhs-" . $self->{_local_counter}++;
    unshift @bindings, ["$rhs_tmp_cl", "$rhs_cl"];
  }

  # Split by the partition (task #289).  ORDINARY package globals live in a
  # global cell reached through a symbol macro: a `let` of such a name is legal
  # CL but installs a LEXICAL shadow, which perl's `local` is not — the whole
  # point of `local` is that a called sub sees the new value.  Those become
  # nested `p-local-cell` opens.  Everything else — the generated temporaries
  # (pcl-local-rhs-N / pcl-local-inner-N, which have no sigil), `$a`/`$b` and
  # the runtime's magic vars — is still a real special, and keeps today's
  # dynamic let/let* (also the faster path: ~4.6 ns vs ~41 ns, and magic-var
  # `local` is where real perl code spends that time).
  #
  # ORDER MATTERS: the let/let* comes FIRST, because its temporaries hold RHS
  # values that must be read while the OLD values are still installed; the cell
  # opens follow, each evaluating its own init before overwriting the cell.
  my @let_b  = grep {  is_exception_global($_->[0]) } @bindings;
  my @cell_b = grep { !is_exception_global($_->[0]) } @bindings;

  my $let_form = ($rhs_tmp_cl || $use_let_star) ? "let*" : "let";
  # "Top level" = this `local` wraps the REST OF THE FILE (see the notinline
  # comment below).  Under Parser2's seam every v1-routed statement is
  # lowered at indent 0 (capture_v1), so indent_level alone called a `local`
  # inside a file-level loop/if/eval body top-level too and wrapped that
  # body's remainder in `(locally (declare (notinline …)))` — suppressing the
  # fast-path inlining in exactly the hot loop bodies the discriminator meant
  # to exclude (found s412 when Phase C routed `eval { local … }` bodies
  # here).  _block_depth is the seam's real-nesting fact (0 only for a
  # statement outside every block); v1's own walk tracks it the same way.
  my $at_top_level = ($self->environment->in_subroutine == 0
                      && $self->indent_level == 0
                      && !$self->_block_depth);
  if (@let_b) {
    my $bindings_str = join("\n        ", map { "($_->[0] $_->[1])" } @let_b);
    $self->_emit("($let_form ($bindings_str)");
    $self->indent_level($self->indent_level + 1);
    $self->{_local_let_depth} //= 0;
    $self->{_local_let_depth}++;
  }
  elsif ($at_top_level) {
    # No dynamic binding left to carry the notinline declaration below, but a
    # top-level `local` still wraps the rest of the file — see the comment
    # there.  `locally` is the declaration-only form of `let`.
    $self->_emit("(locally");
    $self->indent_level($self->indent_level + 1);
    $self->{_local_let_depth} //= 0;
    $self->{_local_let_depth}++;
  }

  # A `local` that is a DIRECT top-level statement (indent_level 0, not in a
  # sub) has dynamic scope extending to end of file, so PCL wraps the entire
  # remainder of the program in this one `let` — potentially thousands of
  # lines, a single enormous cold-run-once CL function.  R1 declaims the
  # fast-path operators `inline`; inlining them into a function that large
  # blows up SBCL's constraint propagation (measured 1.2 GB → OOM compiling
  # perl-tests/local.t; the session-268 R1 crash regression).  Emit a
  # `(declare (notinline ...))` at the head of this let body to suppress
  # inlining here only — it runs once, so nothing is lost.
  #
  # `indent_level == 0` is the precise discriminator: a `local` nested inside a
  # top-level loop/if/block is indented and its scope is bounded by that
  # construct (a small body that must keep inlining, since the loop may be
  # hot), so it is correctly excluded.  Subs (indent > 0) also keep inlining.
  if ($at_top_level) {
    $self->_emit(_notinline_ops_decl());
  }

  # The ORDINARY globals: one `p-local-cell` open each, nested inside the let
  # (and inside each other), every one counting toward _local_let_depth exactly
  # as the let does — the block end closes them all.
  for my $b (@cell_b) {
    $self->_emit("(p-local-cell $b->[0] $b->[1]");
    $self->indent_level($self->indent_level + 1);
    $self->{_local_let_depth} //= 0;
    $self->{_local_let_depth}++;
  }

  # #138: the comma tail of `local $x = A, B;` — a plain statement that runs
  # with the localization in effect, before the rest of the block.
  $self->_emit($local_tail_cl) if defined $local_tail_cl;

  if ($rhs_tmp_cl) {
    my $lhs_cl = "(vector " . join(" ", @vars) . ")";
    $self->_emit("(p-list-= $lhs_cl $rhs_tmp_cl)");
  }
  elsif ($init_idx >= 0 && @vars == 1) {
    # Single array/hash local with init: emit the var as the default return value.
    # local @arr = EXPR as last expression in a sub should return the assigned list.
    # Subsequent statements override this as the actual return value.
    my ($sigil) = ($vars[0] =~ /::([%\@\$])/) ? ($1) : (substr($vars[0], 0, 1));
    if ($sigil eq '@' || $sigil eq '%') {
      $self->_emit("$vars[0]");
    }
  }

  $self->_emit("");
}

# Process state variable declaration with init guard
sub _process_state_declaration {
  my $self = shift;
  my $stmt = shift;
  my $parts = shift;
  my $perl_code = shift;

  # Find the variable name(s) and initializer
  my @vars;
  my $found_assign = 0;  # 1 = '=', 2 = '//='
  my $postfix_op   = '';  # '++' or '--' after variable (no '=')
  my @init_parts;

  for my $part (@$parts) {
    my $ref = ref($part);

    if ($ref eq 'PPI::Token::Word' && $part->content eq 'state') {
      next;  # Skip 'state' keyword
    }
    elsif ($ref eq 'PPI::Token::Symbol' && !$found_assign) {
      push @vars, $part->content;
    }
    elsif ($ref eq 'PPI::Structure::List' && !$found_assign) {
      # state ($t, $u) — extract symbols from the list
      push @vars, $self->_find_symbols_in_list($part);
    }
    elsif ($ref eq 'PPI::Token::Operator' && $part->content eq '=' && !$found_assign) {
      $found_assign = 1;
    }
    elsif ($ref eq 'PPI::Token::Operator' && $part->content eq '//=' && !$found_assign) {
      $found_assign = 2;  # defined-or-assign: same semantics as = for state vars
    }
    elsif ($ref eq 'PPI::Token::Operator' && $part->content =~ /^(\+\+|--)$/ && @vars && !$found_assign) {
      $postfix_op = $part->content; last;
    }
    elsif ($found_assign) {
      push @init_parts, $part;
    }
  }

  # Parse the initializer expression
  my $init_cl = 'nil';
  if (@init_parts) {
    $init_cl = $self->_parse_expression(\@init_parts, $stmt) // 'nil';
  }

  $self->_emit(";; $perl_code");

  # Generate init guard for each state variable, using the unique CL name.
  my $renames = $self->environment->state_var_renames // {};
  for my $var (@vars) {
    my $cl_var   = $renames->{$var} // $var;
    my $init_flag = "${cl_var}__init";
    my $sigil = substr($var, 0, 1);
    $self->_emit("(unless $init_flag");
    $self->indent_level($self->indent_level + 1);
    if ($sigil eq '$') {
      # Use box-set so tied init values call FETCH instead of copying the proxy
      $self->_emit("(box-set $cl_var $init_cl)");
    } elsif ($sigil eq '@') {
      # Array: only initialize if there's an explicit init expression
      # Force list context so qw(...) and other list exprs return all elements.
      $self->_emit("(p-array-= $cl_var (let ((*wantarray* t)) (list $init_cl)))") if @init_parts;
    } elsif ($sigil eq '%') {
      # Hash: only initialize if there's an explicit init expression
      $self->_emit("(p-hash-= $cl_var (let ((*wantarray* t)) $init_cl))") if @init_parts;
    }
    $self->_emit("(setf $init_flag t))");
    $self->indent_level($self->indent_level - 1);
  }

  # Emit post-increment/decrement when no initializer (state $z++)
  if ($postfix_op && !$found_assign) {
    my $cl_op = ($postfix_op eq '++') ? 'p-post++' : 'p-post--';
    for my $var (@vars) {
      my $cl_var = $renames->{$var} // $var;
      $self->_emit("($cl_op $cl_var)");
    }
  }

  # The value of a `state $x = EXPR` expression is the CURRENT value of $x
  # (not the init-guard result), so emit the variable as the trailing form for
  # tail/expression position (map/grep block, implicit sub return). Single
  # scalar/array/hash declarations only; list forms left as-is.
  if (!$postfix_op && @vars == 1) {
    my $cl_var = $renames->{$vars[0]} // $vars[0];
    $self->_emit($cl_var);
  }

  $self->_emit("");
}


# Process compound statement (if/while/for/bare block)
sub _process_compound_statement {
  my $self = shift;
  my $stmt = shift;
  my $external_continue = shift;  # Optional: continue block from sibling lookahead

  # Get the first keyword to determine statement type
  # Also detect any label (LABEL:) before the keyword
  my $first_word;
  my $first_block;
  my $label;
  for my $child ($stmt->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Token::Label') {
      # Label like "OUTER:" - extract just the name
      $label = $child->content;
      $label =~ s/:$//;  # Remove trailing colon
    }
    elsif ($ref eq 'PPI::Token::Word') {
      $first_word = $child->content;
      last;
    }
    elsif ($ref eq 'PPI::Structure::Block' && !$first_block) {
      $first_block = $child;
      last;  # Found the block - don't scan further (avoid picking up 'continue' as first_word)
    }
  }

  if (!$first_word && $first_block && !$label
      && _bare_block_is_anon_hash($first_block)) {
    # PPI mis-tokenized an anon-hash constructor `{ LITERAL , ... }` as a bare
    # block.  Emit it as a hash-constructor expression (its value is discarded
    # in void context, or returned as the last statement of a string eval).
    $self->_emit(";; { ... } (anon hash constructor)");
    $self->_emit($self->parse_hash_block_to_cl_string($first_block));
    $self->_emit("");
    return;
  }

  if (!$first_word && $first_block) {
    # Bare block: { ... } possibly with continue { ... }
    # Scan remaining children for continue block (when PPI keeps it as child)
    my $continue_block = $external_continue;  # May have been found by sibling lookahead
    my $found_continue = 0;
    for my $child ($stmt->children) {
      my $ref = ref($child);
      if ($ref eq 'PPI::Token::Word' && $child->content eq 'continue') {
        $found_continue = 1;
      }
      elsif ($ref eq 'PPI::Structure::Block' && $found_continue) {
        $continue_block = $child;
        last;
      }
    }
    $self->_process_bare_block($first_block, $label, $continue_block);
  }
  elsif (!$first_word) {
    if ($label) {
      # Standalone label statement: LABEL: → emit as tagbody tag.
      # The ;; pcl-label sentinel lets _wrap_runtime_labels distinguish
      # real generated labels from ":word" patterns inside string literals.
      $self->_emit(':' . cl_sym($label) . "  ;; pcl-label");
    } else {
      # Neither block nor keyword found - emit as comment
      my $perl_code = $stmt->content;
      $perl_code =~ s/\n/ /g;
      $self->_emit(";; COMPOUND (unknown) not handled: $perl_code");
      $self->_emit("");
    }
  }
  elsif ($first_word eq 'if' || $first_word eq 'unless') {
    $self->_process_if_statement($stmt, $first_word);
  }
  elsif ($first_word eq 'while' || $first_word eq 'until') {
    $self->_process_while_statement($stmt, $first_word, $label);
  }
  elsif ($first_word eq 'for' || $first_word eq 'foreach') {
    $self->_process_for_statement($stmt, $first_word, $label);
  }
  else {
    # Unknown compound - emit as comment
    my $perl_code = $stmt->content;
    $perl_code =~ s/\n/ /g;
    $self->_emit(";; COMPOUND ($first_word) not yet implemented:");
    $self->_emit(";; $perl_code");
    $self->_emit("");
  }
}


# Process a bare block: { ... } possibly with continue { ... }
sub _process_bare_block {
  my $self  = shift;
  my $block = shift;
  my $label = shift;  # Optional label (e.g., TEST1: { ... })
  my $continue_block = shift;  # Optional continue block

  $self->_emit(";; { ... }");

  # Wrap in *package* save/restore so that any (in-package ...) calls inside
  # the block don't leak *package* to subsequent top-level forms after the block.
  # This is a no-op when no package changes happen inside.
  $self->_emit("(let ((*package* *package*))");
  $self->indent_level($self->indent_level + 1);

  # Wrap the entire bare block in a let for any my-declarations inside it.
  # Each bare block is its own lexical scope in Perl, so inner my-vars must
  # NOT be hoisted to the enclosing sub's let (see _find_all_declarations which
  # now stops recursing at Block boundaries).
  $self->_with_declarations($block, sub {

  # Save current section so that package changes inside the block don't
  # permanently redirect subsequent code (including after-block statements)
  # to a different CL package section. The closers and post-block code
  # must go to the same section as the block opening.
  my $saved_section = $self->_cur_section;
  # Save the transpile-time package stack so __PACKAGE__ and variable name
  # generation see the correct package after the block exits.
  my $saved_pkg_stack = [@{$self->environment->package_stack}];

  if ($label) {
    # Labeled bare block: use (block LABEL ...)
    # In Perl, a bare block is a single-iteration loop - last/next/redo all work.
    # With continue: wrap tagbody in catch for labeled next, then run continue after
    $self->_emit('(block ' . cl_sym($label));
    $self->indent_level($self->indent_level + 1);
    # Wrap contents in LAST-LABEL catch so p-last-dynamic can throw to exit the block.
    # Mirrors how p-next/p-redo use throw for dynamic (cross-function) labeled exits.
    # e.g. Test::More's skip() calls (last SKIP) from inside a called function.
    $self->_emit("(catch (pcl::%pcl-loop-tag \"LAST\" '" . cl_sym($label) . ")");
    $self->indent_level($self->indent_level + 1);
    # Always wrap tagbody in NEXT-LABEL catch so that (p-next LABEL) works even
    # without a continue block.  When next LABEL is thrown from an inner function
    # (e.g. eval { next $label }), the throw lands here; when there is a continue
    # block it runs after the catch returns, just as in the continue case.
    $self->_emit("(catch (pcl::%pcl-loop-tag \"NEXT\" '" . cl_sym($label) . ")");
    $self->indent_level($self->indent_level + 1);
    $self->_emit("(tagbody");
    $self->indent_level($self->indent_level + 1);
    $self->_emit(":redo");
    # Use pcl:: prefix to match the package used by p-redo macro's throw
    $self->_emit("(catch (pcl::%pcl-loop-tag \"REDO\" '" . cl_sym($label) . ")");
    $self->indent_level($self->indent_level + 1);
    $self->_emit("(progn");
    $self->indent_level($self->indent_level + 1);
    $self->_block_depth($self->_block_depth + 1);
    $self->_process_block($block);
    $self->_block_depth($self->_block_depth - 1);
    $self->_cur_section($saved_section);
    $self->environment->package_stack($saved_pkg_stack);
    $self->_emit("(go :next)))");  # close progn + catch'REDO + tagbody... no:
    # Actually: close progn ), close catch ), NOT tagbody
    $self->indent_level($self->indent_level - 2);
    # Back to tagbody content level
    $self->_emit("(go :redo)");
    $self->_emit(":next)");  # close tagbody
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");  # close catch for NEXT
    $self->indent_level($self->indent_level - 1);
    if ($continue_block) {
      $self->_emit("(progn");
      $self->indent_level($self->indent_level + 1);
      $self->_process_block($continue_block);
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")");
    }
    $self->_emit(")");  # close LAST-LABEL catch
    $self->indent_level($self->indent_level - 1);
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  } else {
    # Unlabeled bare block: (block nil (tagbody :redo ... :next))
    # Supports redo, next, last without labels
    # Continue block runs after tagbody (after next/normal exit, not after last)
    #
    # Tail position (task #64): tagbody always yields NIL, which dropped the
    # block's value — but Perl returns the last statement's value from a
    # loop-once bare block that is the sub's tail (`sub f { …; { @x } }`).
    # Capture it by bracketing the SAME statement emission in
    # (setf RET (progn …)) inside the tagbody and reading RET after the
    # block: `last` return-from's past the setf (RET stays nil), `redo`
    # re-runs and re-assigns, `next` jumps to :next skipping the assignment
    # — loop-once semantics unchanged.  Scoped to sub-tail without continue
    # (a continue-block tail keeps the old void shape) so void-position
    # emission is byte-identical.
    my $blk_ret;
    if ($self->environment->in_subroutine > 0
        && $self->environment->tail_position
        && !$continue_block) {
      $blk_ret = '--pcl-blk-ret--' . ($self->{_tail_ret_counter}++);
      $self->_emit("(let (($blk_ret nil))");
      $self->indent_level($self->indent_level + 1);
    }
    $self->_emit("(block nil");
    $self->indent_level($self->indent_level + 1);
    $self->_emit("(tagbody :redo");
    $self->indent_level($self->indent_level + 1);
    if ($blk_ret) {
      $self->_emit("(setf $blk_ret (progn");
      $self->indent_level($self->indent_level + 1);
    }
    $self->_block_depth($self->_block_depth + 1);
    $self->_process_block($block);
    $self->_block_depth($self->_block_depth - 1);
    $self->_cur_section($saved_section);
    $self->environment->package_stack($saved_pkg_stack);
    if ($blk_ret) {
      $self->indent_level($self->indent_level - 1);
      $self->_emit("))");
    }
    $self->_emit(":next)");
    $self->indent_level($self->indent_level - 1);
    if ($continue_block) {
      $self->_emit("(progn");
      $self->indent_level($self->indent_level + 1);
      $self->_process_block($continue_block);
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")");
    }
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
    if ($blk_ret) {
      $self->_emit($blk_ret);
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")");
    }
  }

  }); # end _with_declarations

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");  # close (let ((*package* *package*)) ...)
  $self->_emit("");
}


# Process if/elsif/else statement
# Build a source-echo comment for a compound statement, collapsing each
# top-level brace block to "{ ... }".  The block's inner statements get their
# own ";; ..." comments when we recurse into them, so echoing the whole body in
# the header comment just duplicates it (and produces an absurdly long line).
# Conditions/lists (PPI::Structure::Condition / ::List / ::For) are kept
# verbatim, so hash subscripts and string braces inside them survive intact.
sub _compound_comment {
  my ($self, $stmt) = @_;
  my $code = '';
  for my $child ($stmt->children) {
    if (ref($child) eq 'PPI::Structure::Block') {
      $code .= '{ ... }';
    }
    else {
      $code .= $child->content;
    }
  }
  $code =~ s/\n/ /g;
  return $code;
}

sub _process_if_statement {
  my $self     = shift;
  my $stmt     = shift;
  my $keyword  = shift;  # 'if' or 'unless'

  # Emit the original Perl as comment (body collapsed; see _compound_comment)
  my $perl_code = $self->_compound_comment($stmt);
  $self->_emit(";; $perl_code");

  # Collect the if/elsif/else chain and conditions for declaration scanning
  my @clauses;  # Each: { type => 'if'|'elsif'|'else', cond => ..., block => ... }
  my @conditions;  # All condition elements for declaration scanning

  my $current_type;
  my $current_cond;

  for my $child ($stmt->children) {
    my $ref = ref($child);

    if ($ref eq 'PPI::Token::Word') {
      my $word = $child->content;
      if ($word eq 'if' || $word eq 'elsif' || $word eq 'unless') {
        $current_type = $word;
      }
      elsif ($word eq 'else') {
        $current_type = 'else';
      }
    }
    elsif ($ref eq 'PPI::Structure::Condition') {
      $current_cond = $child;
      push @conditions, $child;
    }
    elsif ($ref eq 'PPI::Structure::Block') {
      push @clauses, {
        type  => $current_type,
        cond  => $current_cond,
        block => $child,
      };
      $current_cond = undef;
    }
  }

  # Use common helper to wrap with declarations
  $self->_with_declarations(\@conditions, sub {
    $self->_generate_if_clauses(\@clauses);
  });

  $self->_emit("");
}


# Generate CL for if/elsif/else chain
sub _generate_if_clauses {
  my $self    = shift;
  my $clauses = shift;
  # $void: are this if/else's branches in void context?  Computed once on the
  # initial call from whether the if-statement itself is in value/tail position
  # (this same generator serves both a tail if-with-else, whose branches DO
  # propagate the caller's wantarray, and a non-tail/void if, whose branches must
  # NOT — else a branch's /g regex inherits list context).  Threaded through the
  # recursive elsif chain.
  my $void = shift;
  $void = ($self->environment->in_subroutine > 0
           && !$self->environment->tail_position) ? 1 : 0
    unless defined $void;

  return unless @$clauses;

  my $first = $clauses->[0];
  my $rest  = [@$clauses[1 .. $#$clauses]];

  # Generate condition
  my $cond_cl = $self->_parse_condition($first->{cond});

  # Emit comment for this clause
  my $cond_perl = $first->{cond} ? $first->{cond}->content : "";
  $cond_perl =~ s/^\s*\(\s*//;  # Remove leading paren
  $cond_perl =~ s/\s*\)\s*$//;  # Remove trailing paren
  $cond_perl =~ s/\n/\n;; /g;   # Add ;; to continuation lines
  $self->_emit(";; $first->{type} ($cond_perl)");

  # Handle 'unless' by negating
  if ($first->{type} eq 'unless') {
    $cond_cl = "(p-not $cond_cl)";
  }

  $self->_emit("(p-if $cond_cl");
  $self->indent_level($self->indent_level + 1);

  # Then block
  $self->_emit("(progn");
  $self->indent_level($self->indent_level + 1);
  $self->_with_declarations($first->{block}, sub {
    $self->_process_block($first->{block}, $void);
  });
  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");

  # Else/elsif part
  if (@$rest) {
    my $next = $rest->[0];
    if ($next->{type} eq 'else') {
      # Simple else
      $self->_emit(";; else");
      $self->_emit("(progn");
      $self->indent_level($self->indent_level + 1);
      $self->_with_declarations($next->{block}, sub {
        $self->_process_block($next->{block}, $void);
      });
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")");
    }
    else {
      # elsif - recursive
      $self->_generate_if_clauses($rest, $void);
    }
  }
  else {
    # No else clause - emit nil
    $self->_emit("nil");
  }

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");
}


# ── Bare-if implicit return (B1) ─────────────────────────────────────────────
#
# Perl rule: the last expression *evaluated* is the return value of a sub.
# For `if (COND) { BODY }` with no else and COND false, COND itself is the
# last thing evaluated, so COND is returned (not undef).
#
# Fix: when the last statement of a sub block is an if/unless without else
# (block-form or postfix), save the condition into a fresh CL variable, and
# return that variable after the if form.

# Returns a fresh CL symbol name for the if-return let binding.
sub _fresh_ret_var {
  my $self = shift;
  $self->{_tail_ret_counter} //= 0;
  return '--pcl-if-ret--' . $self->{_tail_ret_counter}++;
}

# True if $stmt is a compound if/unless statement WITHOUT a final else.
sub _is_if_without_else {
  my ($self, $stmt) = @_;
  return 0 unless ref($stmt) eq 'PPI::Statement::Compound';
  my ($first_word, $has_else);
  for my $child ($stmt->children) {
    next if ref($child) eq 'PPI::Token::Whitespace';
    if (ref($child) eq 'PPI::Token::Word') {
      my $w = $child->content;
      $first_word //= $w;
      $has_else = 1 if $w eq 'else';
    }
  }
  return 0 if $has_else;
  return ($first_word // '') eq 'if' || ($first_word // '') eq 'unless';
}

# True if $stmt is a postfix if/unless modifier (PPI::Statement with if/unless word).
sub _is_postfix_if_without_else {
  my ($self, $stmt) = @_;
  return 0 unless ref($stmt) eq 'PPI::Statement';
  for my $child ($stmt->children) {
    next if ref($child) eq 'PPI::Token::Whitespace';
    if (ref($child) eq 'PPI::Token::Word') {
      my $w = $child->content;
      return 1 if $w eq 'if' || $w eq 'unless';
    }
  }
  return 0;
}

# Process a single statement in tail position.
# Wraps its result in (setf ret_var ...) so the outer let captures the value.
sub _process_tail_stmt {
  my ($self, $stmt, $ret_var) = @_;

  # Block-form if/unless without else: recurse with same ret_var (no new let)
  if ($self->_is_if_without_else($stmt)) {
    $self->_process_if_tail($stmt, $ret_var);
    return;
  }

  # Postfix if/unless: emit tail form
  if ($self->_is_postfix_if_without_else($stmt)) {
    my $perl_code = $stmt->content;
    $perl_code =~ s/;\s*$//;
    $perl_code =~ s/\n/ /g;
    my @parts = grep {
      my $r = ref($_);
      $r ne 'PPI::Token::Whitespace'
        && $r ne 'PPI::Token::Comment'
        && !($r eq 'PPI::Token::Structure' && $_->content eq ';')
    } $stmt->children;

    my ($modifier_idx, $modifier);
    for my $i (0 .. $#parts) {
      if (ref($parts[$i]) eq 'PPI::Token::Word') {
        my $w = $parts[$i]->content;
        if ($w eq 'if' || $w eq 'unless') {
          $modifier_idx = $i;
          $modifier = $w;
          last;
        }
      }
    }

    if (defined $modifier_idx && $modifier_idx > 0) {
      my @expr_parts = @parts[0 .. $modifier_idx - 1];
      my @cond_parts = @parts[$modifier_idx + 1 .. $#parts];

      if (@cond_parts == 1 && ref($cond_parts[0]) eq 'PPI::Structure::Condition') {
        @cond_parts = grep { ref($_) ne 'PPI::Token::Whitespace' } $cond_parts[0]->children;
      }

      my $expr_cl = $self->_parse_expression(\@expr_parts, $stmt);
      my $cond_cl = $self->_parse_expression(\@cond_parts, $stmt);
      # Drop inline-leading indent so the (setf ...) operands sit flush.
      $expr_cl =~ s/^[ \t]+// if defined $expr_cl;
      $cond_cl =~ s/^[ \t]+// if defined $cond_cl;

      $self->_emit(";; $perl_code");
      if ($modifier eq 'if') {
        $self->_emit("(p-if (setf $ret_var $cond_cl)");
        $self->indent_level($self->indent_level + 1);
        $self->_emit("(setf $ret_var $expr_cl)");
        $self->_emit("nil)");
        $self->indent_level($self->indent_level - 1);
      } else {  # unless
        $self->_emit("(p-unless (setf $ret_var $cond_cl)");
        $self->indent_level($self->indent_level + 1);
        $self->_emit("(setf $ret_var $expr_cl))");
        $self->indent_level($self->indent_level - 1);
      }
      $self->_emit("");
      return;
    }
    # Fall through to normal emit if we couldn't parse the modifier
  }

  # Simple expression statement: wrap result with setf
  if (ref($stmt) eq 'PPI::Statement' || ref($stmt) eq 'PPI::Statement::Expression') {
    my $perl_code = $stmt->content;
    $perl_code =~ s/;\s*$//;
    $perl_code =~ s/\n/ /g;
    my @parts = grep {
      my $r = ref($_);
      $r ne 'PPI::Token::Whitespace'
        && $r ne 'PPI::Token::Comment'
        && !($r eq 'PPI::Token::Structure' && $_->content eq ';')
    } $stmt->children;
    # A trailing for/foreach/while/until modifier (`EXPR foreach LIST`) makes
    # this a loop, not a value: its result in Perl is the empty list.  Don't try
    # to wrap it in (setf ret_var ...) — that fed the modifier tokens into the
    # value parser and fell through to the "Missing case" die.  Emit the loop via
    # the normal statement path and leave ret_var holding "" (empty).
    if (grep { ref($_) eq 'PPI::Token::Word'
               && $_->content =~ /^(?:for|foreach|while|until)$/ } @parts) {
      $self->_emit("(setf $ret_var \"\")");
      $self->_process_expression_statement($stmt);
      return;
    }
    if (@parts) {
      my $cl = $self->_parse_expression(\@parts, $stmt);
      $cl =~ s/^[ \t]+// if defined $cl;  # drop inline-leading indent (setf gap)
      $self->_emit(";; $perl_code");
      $self->_emit("(setf $ret_var $cl)") if defined $cl;
      $self->_emit("");
    }
    return;
  }

  # Everything else (variable decl, loops, etc.): emit normally.
  # ret_var holds the condition value from the outer if — best-effort.
  $self->_process_element($stmt);
}

# Generate CL for an if/elsif/else chain in tail position.
# Mirrors _generate_if_clauses but wraps the condition (and each branch's
# last expr) so that ret_var always holds the correct return value.
sub _generate_if_tail_clauses {
  my ($self, $clauses, $ret_var) = @_;
  return unless @$clauses;

  my $first = $clauses->[0];
  my $rest  = [@$clauses[1 .. $#$clauses]];

  my $cond_cl = $self->_parse_condition($first->{cond});

  my $cond_perl = $first->{cond} ? $first->{cond}->content : "";
  $cond_perl =~ s/^\s*\(\s*//;
  $cond_perl =~ s/\s*\)\s*$//;
  $cond_perl =~ s/\n/\n;; /g;
  $self->_emit(";; $first->{type} ($cond_perl)");

  # Wrap condition to save its Perl value.
  # For 'unless': save first, then negate for the p-if test.
  my $wrapped_cond;
  if ($first->{type} eq 'unless') {
    $wrapped_cond = "(progn (setf $ret_var $cond_cl) (p-not $ret_var))";
  } else {
    $wrapped_cond = "(setf $ret_var $cond_cl)";
  }

  $self->_emit("(p-if $wrapped_cond");
  $self->indent_level($self->indent_level + 1);

  # Then block
  $self->_emit("(progn");
  $self->indent_level($self->indent_level + 1);
  $self->_with_declarations($first->{block}, sub {
    $self->_process_block_in_tail_context($first->{block}, $ret_var);
  });
  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");

  # Else/elsif
  if (@$rest) {
    my $next = $rest->[0];
    if ($next->{type} eq 'else') {
      $self->_emit(";; else");
      $self->_emit("(progn");
      $self->indent_level($self->indent_level + 1);
      $self->_with_declarations($next->{block}, sub {
        $self->_process_block_in_tail_context($next->{block}, $ret_var);
      });
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")");
    } else {
      # elsif: recurse with same ret_var (no new let)
      $self->_generate_if_tail_clauses($rest, $ret_var);
    }
  } else {
    # No else: nil placeholder.  ret_var already holds the last cond value.
    $self->_emit("nil");
  }

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");
}

# Process a tail-position if/unless compound statement.
# Collects clauses (same as _process_if_statement) and calls
# _generate_if_tail_clauses — no new let is opened here.
sub _process_if_tail {
  my ($self, $stmt, $ret_var) = @_;

  my $perl_code = $stmt->content;
  $perl_code =~ s/\n/ /g;
  $self->_emit(";; $perl_code");

  my @clauses;
  my @conditions;
  my $current_type;
  my $current_cond;

  for my $child ($stmt->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Token::Word') {
      my $w = $child->content;
      if ($w eq 'if' || $w eq 'elsif' || $w eq 'unless') {
        $current_type = $w;
      } elsif ($w eq 'else') {
        $current_type = 'else';
      }
    } elsif ($ref eq 'PPI::Structure::Condition') {
      $current_cond = $child;
      push @conditions, $child;
    } elsif ($ref eq 'PPI::Structure::Block') {
      push @clauses, {
        type  => $current_type,
        cond  => $current_cond,
        block => $child,
      };
      $current_cond = undef;
    }
  }

  $self->_with_declarations(\@conditions, sub {
    $self->_generate_if_tail_clauses(\@clauses, $ret_var);
  });

  $self->_emit("");
}

# Process a block's contents where the last statement contributes to ret_var.
# Mirrors _process_block but dispatches the last significant statement
# through _process_tail_stmt instead of _process_element.
sub _process_block_in_tail_context {
  my ($self, $block, $ret_var) = @_;

  # Isolate _pending_let_closes so that our flush at the end does not
  # accidentally close let forms opened by an enclosing _emit_scoped_block.
  # Mirrors the save/reset/restore done by _process_block.
  my $saved_pending = $self->{_pending_let_closes};
  $self->{_pending_let_closes} = [];

  $self->environment->push_scope();
  my $start_depth = $self->{_local_let_depth} // 0;

  my @sig      = $block->schildren;
  my $last_sig = @sig ? $sig[-1] : undef;

  my @children = $block->children;
  my %skip;
  for my $i (0 .. $#children) {
    next if $skip{$i};
    my $child = $children[$i];
    my $ref   = ref($child);
    next if $ref eq 'PPI::Token::Whitespace';
    next if $ref eq 'PPI::Token::Comment';

    # Fire the scoped-block hook before each significant statement.
    $self->{_stmt_pre_hook}->($self, $child) if $self->{_stmt_pre_hook};

    if ($ref eq 'PPI::Statement::Compound') {
      my ($continue, $trailing) = $self->_find_continue_sibling(\@children, $i, \%skip);
      if ($continue) {
        $self->_process_compound_statement($child, $continue);
        $self->_process_trailing_tokens($trailing) if $trailing && @$trailing;
        next;
      }
    }

    if (defined $last_sig && $child == $last_sig) {
      $self->_process_tail_stmt($child, $ret_var);
    } else {
      $self->_process_element($child);
    }
  }

  # Flush only the let closes opened within this block's scope.
  while (@{$self->{_pending_let_closes} // []}) {
    pop @{$self->{_pending_let_closes}};
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  }

  my $end_depth = $self->{_local_let_depth} // 0;
  while ($end_depth > $start_depth) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")  ;; end local");
    $self->{_local_let_depth}--;
    $end_depth--;
  }

  $self->environment->pop_scope();

  # Restore the outer pending closes (from enclosing _emit_scoped_block).
  $self->{_pending_let_closes} = $saved_pending;
}

# ─────────────────────────────────────────────────────────────────────────────


# Process a block's contents
sub _process_block {
  my $self  = shift;
  my $block = shift;
  # $void_body: the block's value is discarded (a loop body — while/for/foreach
  # and continue blocks).  Such a block is NEVER in value/tail position, so its
  # last statement must NOT inherit the enclosing sub's wantarray (otherwise a
  # statement-level `m//g` in a loop body would run in list context — e.g.
  # Text::Balanced::_match_bracketed's tokenizer loop, which then never advances
  # pos() and hangs).  Defaults to false to preserve the value-position behavior
  # for sub bodies and other callers.
  my $void_body = shift;

  # Isolate _pending_let_closes so that inner _process_block calls (e.g.
  # for if/while/bare block bodies) cannot accidentally flush pending let
  # closes that belong to an enclosing _emit_scoped_block context.
  # Each _process_block call owns its own slice; the outer value is restored
  # after this call finishes (including after flushing any closes we opened).
  my $saved_pending_block     = $self->{_pending_let_closes};
  $self->{_pending_let_closes} = [];

  # Enter new scope for filehandles
  $self->environment->push_scope();

  # Track local let depth at block start
  my $start_depth = $self->{_local_let_depth} // 0;

  # ── Tail-if detection ─────────────────────────────────────────────────────
  # When the last significant statement of a sub block is an if/unless without
  # else (or a postfix if/unless), wrap the block in a let-binding that captures
  # the condition value.  This implements Perl's "last expression evaluated"
  # return semantics for bare-if.
  my ($tail_ret_var, $tail_last_sig, $tail_sig);
  if (!$void_body && $self->environment->in_subroutine > 0) {
    my @sig = $block->schildren;
    # Skip BEGIN/END/INIT/CHECK blocks — they produce no runtime code,
    # so the tail is the last *runtime* significant statement.
    my $last;
    for my $s (reverse @sig) {
      unless (ref($s) eq 'PPI::Statement::Scheduled') {
        $last = $s;
        last;
      }
    }
    if ($last) {
      $tail_sig = $last;  # track for tail_position context propagation
      if ($self->_is_if_without_else($last) || $self->_is_postfix_if_without_else($last)) {
        $tail_ret_var  = $self->_fresh_ret_var();
        $tail_last_sig = $last;
      }
    }
  }
  if ($tail_ret_var) {
    $self->_emit("(let (($tail_ret_var nil))");
    $self->indent_level($self->indent_level + 1);
  }
  # ─────────────────────────────────────────────────────────────────────────

  # Save/restore tail_position around child processing: the per-child loop sets
  # it explicitly (0 for non-tail), so without restoring, a value-position tail
  # child would leave tail_position=1 leaking into whatever the caller processes
  # next (e.g. the next top-level statement, making a void call wrongly propagate
  # the ambient wantarray).
  my $saved_tail_position = $self->environment->tail_position;

  my @children = $block->children;
  my %skip;

  # Hoist named sub definitions that are called BEFORE their definition in the block.
  # Perl compiles all named subs at compile time so they are callable anywhere in their
  # lexical scope.  In a let-bound block p-sub is emitted inline and evaluated
  # sequentially — calls to the sub BEFORE the p-sub form would use the forward stub
  # (returns nil).  Only hoist when actually needed (called before defined), to avoid
  # disturbing package context for subs already defined before their calls.
  # Only fires when _let_bound_vars is non-empty (otherwise subs already go to
  # declarations/definitions and are pre-hoisted).
  if (%{$self->lex_home->{_let_bound_vars} // {}}) {
    # First pass: find which sub names appear as word tokens before their FIRST definition.
    # Only the first definition of a sub is ever hoisted — later redefinitions stay in order
    # so that last-definition-wins semantics are preserved.
    my %words_seen;    # token content => 1 if seen in a non-sub statement
    my %sub_defined;   # sub_name => 1 once we've seen the first definition
    my %needs_hoist;   # child index => 1 for subs that need hoisting
    for my $i (0 .. $#children) {
      my $child = $children[$i];
      my $ref = ref($child);
      next if $ref eq 'PPI::Token::Whitespace' || $ref eq 'PPI::Token::Comment';
      if ($ref eq 'PPI::Statement::Sub') {
        my ($sub_name, $has_block) = ('', 0);
        for my $sc ($child->children) {
          my $scref = ref($sc);
          if ($scref eq 'PPI::Token::Word' && $sc->content ne 'sub'
              && $sc->content ne 'my' && $sc->content ne 'our' && $sc->content ne 'state') {
            $sub_name ||= $sc->content;
          }
          $has_block = 1 if $scref eq 'PPI::Structure::Block';
        }
        if ($sub_name && $has_block) {
          # Hoist only if: (a) called before this definition, and (b) no earlier definition
          if ($words_seen{$sub_name} && !$sub_defined{$sub_name}) {
            $needs_hoist{$i} = 1;
          }
          $sub_defined{$sub_name} = 1;
        }
      } else {
        # Record all word tokens as potential calls.
        #
        # ASK THE CLASS, NOT THE NAME (task #353).  `children` (unlike
        # `schildren`) yields INSIGNIFICANT tokens too, and the skips above
        # name only Whitespace and Comment — so a `PPI::Token::Pod` reached
        # here and `find` is a PPI::Node method, not a Token one.  It died,
        # the caller turned that into "Failed to extract prototypes from
        # <Module>" and cached undef, and EVERY prototype in a module with
        # top-level POD inside a let-bound block was silently unavailable to
        # the compiler (Unicode::UCD, six companion files).  A bare Word IS
        # its own call site; anything else here (Pod, __END__ separators)
        # contributes no words.  Same guard shape as
        # Parser2::_collect_named_subs.
        my $words = $child->isa('PPI::Node')
                    ? ($child->find('PPI::Token::Word') || [])
                    : ($child->isa('PPI::Token::Word') ? [$child] : []);
        for my $w (@$words) {
          $words_seen{$w->content} = 1;
        }
      }
    }
    # Second pass: hoist only subs that need it
    for my $i (sort { $a <=> $b } keys %needs_hoist) {
      $self->_process_element($children[$i]);
      $skip{$i} = 1;
    }
  }

  # Capture the bucket + start index of this block's emitted statements so we
  # can post-process any goto/label pairs that are direct siblings here into a
  # (tagbody …).  CL `go` needs a lexically-enclosing tagbody; labels emitted
  # as statement siblings (e.g. an intra-sub `LABEL:` jumped to by `goto LABEL`)
  # would otherwise have no tagbody.  Capturing here (around the child loop)
  # places the tagbody INSIDE any wrapping (let …) for declarations, so the tag
  # is reachable.  No-op unless a label sentinel is emitted as a direct sibling.
  my $lbl_sec    = $self->_cur_section;
  my $lbl_bucket = $self->_cur_bucket;
  my $lbl_start  = scalar @{$self->_sections->[$lbl_sec]{$lbl_bucket}};

  for my $i (0 .. $#children) {
    next if $skip{$i};
    my $child = $children[$i];
    my $ref = ref($child);
    next if $ref eq 'PPI::Token::Whitespace';
    next if $ref eq 'PPI::Token::Comment';

    # Fire the scoped-block hook (set by _emit_scoped_block) before each
    # significant statement.  The hook opens (let ...) for any 'my'
    # declarations associated with this statement's PPI object.
    $self->{_stmt_pre_hook}->($self, $child) if $self->{_stmt_pre_hook};

    # Lookahead: bare block followed by continue { } as sibling
    if ($ref eq 'PPI::Statement::Compound') {
      my ($continue, $trailing) = $self->_find_continue_sibling(\@children, $i, \%skip);
      if ($continue) {
        $self->_process_compound_statement($child, $continue);
        $self->_process_trailing_tokens($trailing) if $trailing && @$trailing;
        next;
      }
    }

    # Intercept last significant child when tail-if is active
    if ($tail_ret_var && defined $tail_last_sig && $child == $tail_last_sig) {
      $self->_process_tail_stmt($child, $tail_ret_var);
      next;
    }

    # Set tail_position so gen_funcall/gen_methodcall propagate *wantarray*
    # instead of overriding it — allowing context to flow from the call site.
    # Non-tail statements are void, so set tail_position EXPLICITLY to 0 for them
    # (not merely leave it): otherwise a nested dynamic-context op like /g regex in
    # a non-tail statement inherits the sub's list wantarray.  $void_body (loop
    # bodies) forces every statement void.  tail_position now accurately reflects
    # "the current statement is in value/return position", which _generate_if_clauses
    # reads to decide whether its branches propagate context or are void.
    my $is_tail = !$void_body && defined $tail_sig && $child == $tail_sig;
    $self->environment->tail_position($is_tail ? 1 : 0);
    $self->_process_element($child);
  }
  $self->environment->tail_position($saved_tail_position);

  # Wrap any goto/label pairs that are direct siblings in this block into a
  # (tagbody …).  Only when the block stayed in the same section/bucket and a
  # label sentinel was actually emitted (the helper no-ops otherwise).
  if ($self->_cur_section == $lbl_sec && $self->_cur_bucket eq $lbl_bucket) {
    my $arr = $self->_sections->[$lbl_sec]{$lbl_bucket};
    my $end = $#$arr;
    if ($end >= $lbl_start
        && grep { /^\s*:$LABEL_TOK\s*;; pcl-label/ }
               @{$arr}[$lbl_start .. $end]) {
      my @wrapped = _wrap_runtime_labels([ @{$arr}[$lbl_start .. $end] ]);
      splice @$arr, $lbl_start, ($end - $lbl_start + 1), @wrapped;
    }
  }

  # Flush let forms opened by _emit_scoped_block's hook (innermost first).
  # Must happen here, inside _process_block, so the closes land BEFORE any
  # tagbody/:next structure that $emit_body emits after _process_block returns.
  while (@{$self->{_pending_let_closes} // []}) {
    pop @{$self->{_pending_let_closes}};
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  }

  # Close any let forms opened by local declarations in this block
  my $end_depth = $self->{_local_let_depth} // 0;
  while ($end_depth > $start_depth) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")  ;; end local");
    $self->{_local_let_depth}--;
    $end_depth--;
  }

  # Close the tail let (after local lets, so ret_var is the outermost form)
  if ($tail_ret_var) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit("$tail_ret_var)");
  }

  # Leave scope - removes filehandles added in this block
  $self->environment->pop_scope();

  # Restore the outer _pending_let_closes (saved at the top of this call).
  $self->{_pending_let_closes} = $saved_pending_block;
}


# Parse a block as a named function for eval/sub blocks
# Returns the generated function name
# $params is arrayref: [] for eval/sub
# $is_anon_sub: 1 for sub{} anonymous subs (adds &rest %_args + @_ binding)
# $return_lambda: 1 = return a (lambda ...) CL string instead of emitting (defun ...)
# Note: grep/map/sort now use parse_block_to_cl_string with inline lambdas
sub parse_block_as_function {
  my $self          = shift;
  my $block         = shift;  # PPI::Structure::Block
  my $params        = shift // [];  # Parameter names
  my $is_anon_sub   = shift // 0;   # 1 = anonymous sub (receives call args via @_)
  my $return_lambda = shift // 0;   # 1 = return lambda string, don't emit defun
  my $loop_transparent = shift // 0; # 1 = wrap body in (progn ...) not (block nil ...)
                                     #     so unlabeled last/next/redo propagate to the
                                     #     enclosing loop (Perl do{} semantics)

  # Generate unique function name (used only for defun path)
  my $func_name = sprintf("--anon-block-%d--", ++$anon_block_counter);

  # For $return_lambda: redirect all _emit calls to a temp section so we
  # can collect the output as a string to return inline.
  my ($saved_sections, $saved_cur_section, $saved_cur_bucket, $saved_indent);
  if ($return_lambda) {
    $saved_sections    = $self->_sections;
    $saved_cur_section = $self->_cur_section;
    $saved_cur_bucket  = $self->_cur_bucket;
    $saved_indent      = $self->indent_level;
    $self->_sections([{
      pkg => '_lambda_', preamble => [], declarations => [], definitions => [], runtime => [],
    }]);
    $self->_cur_section(0);
    $self->_cur_bucket('runtime');
    $self->indent_level(0);
  }

  # For anonymous subs, detect state variables and wrap in outer let.
  # This mirrors the logic in _process_sub_statement for named subs.
  my %state_renames;
  my %anon_state_vars_set;
  if ($is_anon_sub && $block) {
    my @all_decls = @{$self->_find_all_declarations($block)};
    my %seen;
    my @state_vars = grep { !$seen{$_}++ }
                     map  { $_->{var} }
                     grep { $_->{type} eq 'state' } @all_decls;
    if (@state_vars) {
      %anon_state_vars_set = map { $_ => 1 } @state_vars;
      my @bindings;
      for my $var (@state_vars) {
        my ($sigil, $bare) = ($var =~ /^([\$\@\%])(.+)$/);
        $bare //= $var; $sigil //= '$';
        my $bare_slug = $bare; $bare_slug =~ s/[^a-zA-Z0-9]/_/g;
        my $unique = sprintf("%sstate__anon__%s__%d",
                             $sigil, $bare_slug, ++$state_var_counter);
        $state_renames{$var} = $unique;
        # Initialize to sigil-appropriate empty container
        my $init_val = $sigil eq '$' ? '(make-p-box nil)'
                     : $sigil eq '@' ? '(make-array 0 :adjustable t :fill-pointer 0)'
                     :                 '(make-hash-table :test (quote equal))';
        push @bindings, "($unique $init_val)";
        push @bindings, "(${unique}__init nil)";
      }
      $self->_emit("(let (" . join(" ", @bindings) . ")");
      $self->indent_level($self->indent_level + 1);
    }
  }

  # Anonymous subs accept arguments like named subs: (&rest %_args)
  # with @_ bound inside the body via p-flatten-args.
  my $params_cl = $is_anon_sub ? '&rest %_args' : join(' ', @$params);

  # Emit the function definition
  if ($return_lambda) {
    $self->_emit("(lambda ($params_cl)");
  } else {
    $self->_emit("(defun $func_name ($params_cl)");
  }
  $self->indent_level($self->indent_level + 1);

  if ($is_anon_sub) {
    $self->_emit("(let ((\@_ (p-flatten-args %_args))");
    $self->_emit("      (*pcl-caller-wantarray* *wantarray*))");
    $self->indent_level($self->indent_level + 1);
    $self->_emit("(catch :p-return");
    $self->indent_level($self->indent_level + 1);
  }

  # Perl block-scopes `package NAME;`: the switch reverts when the block
  # ends.  When the body contains a package statement, bind
  # *pcl-current-package* around the body so the runtime switch
  # (p-set-current-package) reverts on exit too — a let passes the block's
  # tail value through, unlike an appended restore form which would become
  # the do-block's value.  (PPI find returns 0, not undef, when empty.)
  my $bind_cur_pkg = $block && @{$block->find('PPI::Statement::Package') || []};
  if ($bind_cur_pkg) {
    $self->_emit("(let ((*pcl-current-package* *pcl-current-package*))");
    $self->indent_level($self->indent_level + 1);
  }

  # A do{} block is loop-transparent: wrap in (progn ...) so an unlabeled
  # last/next/redo (return-from nil / go :next / go :redo) escapes to the
  # ENCLOSING loop instead of being caught here.  A (block nil) would shadow
  # the loop's own block for the return-from-nil that `last` compiles to.
  # `return` still escapes (it throws :p-return, caught by the enclosing sub).
  $self->_emit($loop_transparent ? "(progn" : "(block nil");
  $self->indent_level($self->indent_level + 1);

  # Anon-sub void regime (like named subs): bind *wantarray* :void once around
  # the body; the tail restores *pcl-caller-wantarray* (bound in the let above).
  # do{}/eval{} blocks (is_anon_sub=0) are boundaries — they run in their OWN
  # caller context, so they keep the per-statement wraps (wa_void_active=0).
  if ($is_anon_sub) {
    $self->_emit("(let ((*wantarray* :void))");
    $self->indent_level($self->indent_level + 1);
  }

  # Enter new scope for filehandles; count as a subroutine so 'my'
  # declarations use the let-binding path, not eval-when+defvar.
  $self->environment->push_scope();
  $self->environment->in_subroutine($self->environment->in_subroutine + 1);

  # A bare `package NAME;` inside a do{}/eval{}/anon-sub body is block-scoped
  # in Perl: the switch reverts when the block ends.  Snapshot the package
  # stack and restore after processing so the switch cannot leak into code
  # after the block — and, because expression blocks are parsed repeatedly by
  # pre-passes, even into code BEFORE it (task #49: `do { package X8; 1 }`
  # made an unrelated earlier call emit as X8::pl-f5).
  my $saved_pkg_stack = [@{$self->environment->package_stack}];
  # The package this block's emitted text will be READ in — i.e. the enclosing
  # section's `in-package`.  A `package Foo;` INSIDE the block is only a
  # RUNTIME switch, so anything hoisted out of here (a `use`, whose import
  # target is *package*) must name its package explicitly once the two
  # diverge.  See the `:into` branch in _process_include_statement.
  local $self->{_seam_outer_pkg} = $saved_pkg_stack->[-1];
  # Bump _block_depth so a named sub defined after the inline switch gets a
  # fully-qualified p-sub name (the emitted form is read in the ENCLOSING
  # section's CL package, where a bare name would intern) — matching the
  # XD::pl-mk qualification its call sites get.  Gated on $bind_cur_pkg so
  # blocks without a package statement emit byte-identically to before.
  $self->_block_depth($self->_block_depth + 1) if $bind_cur_pkg;

  # Wrap body in let for any 'my' declarations, then process contents.
  # For anon subs with state vars, set the rename map so _process_state_declaration
  # uses the unique CL names, and set _current_state_vars so it triggers.
  {
    local $self->environment->{wa_void_active} = $is_anon_sub ? 1 : 0;
    local $self->{_current_state_vars} = \%anon_state_vars_set;
    my $saved_renames = $self->environment->state_var_renames;
    if (%state_renames) {
      # Merge with existing renames (parent closure renames must still apply)
      my %merged = (%{$saved_renames // {}}, %state_renames);
      $self->environment->state_var_renames(\%merged);
    }
    $self->_with_declarations($block, sub {
      $self->_process_block($block);
    }, 1);  # is_sub_body=1: enable two-phase scoped block
    $self->environment->state_var_renames($saved_renames);
  }

  $self->environment->in_subroutine($self->environment->in_subroutine - 1);

  # Leave scope - removes filehandles added in this block
  $self->environment->pop_scope();
  # Revert any inline `package NAME;` switch made inside the block (see above)
  $self->_block_depth($self->_block_depth - 1) if $bind_cur_pkg;
  $self->environment->package_stack($saved_pkg_stack);

  if ($is_anon_sub) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");  # close (let ((*wantarray* :void)))
  }

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");  # close block nil

  if ($bind_cur_pkg) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");  # close (let ((*pcl-current-package* ...)))
  }

  if ($is_anon_sub) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");  # close catch :p-return
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");  # close let @_
  }

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");  # close defun/lambda

  # Close outer state let if we opened one
  if (%state_renames) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");  # close state let
  }

  if ($return_lambda) {
    # Collect all emitted lines from temp section and return as lambda string.
    # Definitions (BEGIN blocks) and declarations (our $var defvars) are hoisted
    # to the real buckets rather than inlined — otherwise (eval-when ...) ends up
    # as the first argument to p-funcall-ref, making NIL the function ref.
    my $temp = $self->_sections->[0];
    my @hoisted_defs  = @{$temp->{definitions}};
    my @hoisted_decls = @{$temp->{declarations}};
    my @lines = (
      @{$temp->{preamble}},
      @{$temp->{runtime}},
    );
    $self->_sections($saved_sections);
    $self->_cur_section($saved_cur_section);
    $self->_cur_bucket($saved_cur_bucket);
    $self->indent_level($saved_indent);
    # Re-emit hoisted definitions (BEGIN blocks, etc.) into the real sections.
    # Interleaving guard: since s253b a top-level sub body is emitted
    # incrementally into the `definitions` bucket.  If a do{}/eval{} block sits
    # in that sub (e.g. inside an elsif CONDITION) and contains a BEGIN, naively
    # appending the hoisted BEGIN to `definitions` drops it between the lines of
    # the in-progress form — landing a stray (p-BEGIN ...) between two p-if
    # branches and corrupting the macro call.  When the bucket currently being
    # emitted to IS `definitions`, DEFER the hoist into a pending buffer that
    # _process_children flushes once the current top-level statement is fully
    # emitted (so the BEGIN lands AFTER the enclosing sub, where the constants it
    # references — also in `definitions`, earlier in source — already exist).
    if (@hoisted_defs) {
      my $section = $self->_sections->[$self->_cur_section];
      # …but that DEFERRAL is a v1-only mechanism: it relies on
      # _process_children flushing the buffer once the enclosing top-level
      # statement is emitted, and under v2 this parser is only the expression
      # SEAM — nothing ever flushes it, so the hoist was silently DROPPED.
      # `do { package P; use M; … }` lost its `use` entirely that way: the
      # module never loaded and the only symptom was an undefined function at
      # run time.  The interleaving hazard does not exist under v2 either: the
      # seam runs with a FRESH scratch section that _lower_expr drains whole
      # (and an analysis-only parse throws away), so the plain push is both
      # correct and picked up by the existing drain.
      if ($self->_cur_bucket eq 'definitions' && !$self->{_v2_owner}) {
        push @{$self->{_pending_hoisted_defs} //= []}, @hoisted_defs;
      } else {
        push @{$section->{'definitions'}}, @hoisted_defs;
      }
    }
    # Re-emit hoisted declarations (our $var defvars, etc.) into the real sections
    if (@hoisted_decls) {
      my $section = $self->_sections->[$self->_cur_section];
      push @{$section->{'declarations'}}, @hoisted_decls;
    }
    return @lines ? join("\n", @lines) : "(lambda () nil)";
  }

  $self->_emit("");  # Blank line after function

  return $func_name;
}

# Parse a block and return its body as CL code string (for inline lambdas)
# Returns the CL code string for the block body
sub parse_block_to_cl_string {
  my $self     = shift;
  my $block    = shift;  # PPI::Structure::Block
  my $for_func = shift // '';  # 'map'/'grep'/'sort'/... — selects tail context

  # map { ... } evaluates its block in LIST context (so `..` is range, an @array
  # tail flattens, etc.).  grep/sort blocks stay scalar/boolean (their default).
  my $tail_wants_list = ($for_func eq 'map');

  # Boundary: the map/grep/sort macro rebinds *wantarray* (to list/scalar), so an
  # enclosing sub-body void regime does NOT reach here.  Clear wa_void_active so
  # the block's non-tail statements get their own per-statement :void wraps.
  local $self->environment->{wa_void_active} = 0;

  # Save current bucket state and indent; set up a fresh temp section
  my $saved_sections    = $self->_sections;
  my $saved_cur_section = $self->_cur_section;
  my $saved_cur_bucket  = $self->_cur_bucket;
  my $saved_indent      = $self->indent_level;
  my $saved_local_depth = $self->{_local_let_depth} // 0;

  $self->_sections([{
    pkg => '_temp_', preamble => [], declarations => [], definitions => [], runtime => [],
  }]);
  $self->_cur_section(0);
  $self->_cur_bucket('runtime');
  $self->indent_level(1);  # Start with some indent for readability

  # Enter new scope for filehandles
  $self->environment->push_scope();

  # A bare `package NAME;` inside this block is block-scoped in Perl.  Bump
  # _block_depth so the package statement emits INLINE — the top-level path
  # opens a new SECTION, whose lines this string collector silently drops
  # (`eval { package X; ... }` lost its entire body and became
  # `(p-eval-block nil)`).  Snapshot the package stack for the compile-time
  # revert; the runtime revert is the *package*/*pcl-current-package*
  # binding wrapped around the returned body below.  (PPI find returns 0,
  # not undef, when nothing matches.)
  my $has_pkg_stmt = @{$block->find('PPI::Statement::Package') || []};
  my $saved_pkg_stack = [@{$self->environment->package_stack}];
  # See parse_block_as_function: the package this block's text is READ in, so a
  # hoisted `use` can name its import target when an inline `package` diverges.
  local $self->{_seam_outer_pkg} = $saved_pkg_stack->[-1];
  $self->_block_depth($self->_block_depth + 1) if $has_pkg_stmt;

  # Per-iteration closure capture: if a `my` var declared in this block is
  # captured by a nested anon sub, wrap the body in a `let` of a fresh lexical
  # so each block invocation (the block is a (lambda ($_) ...) called once per
  # element) gets its own binding.  No-op for ordinary blocks.
  my $clo_scope = $self->_begin_block_closure_scope($block);

  # Find last significant child so we can set tail_position correctly.
  # This prevents the VOID_CTX wrap (in _process_expression_statement) from
  # incorrectly wrapping the lambda's return value in map/grep/sort blocks.
  my @sig = grep {
    my $r = ref($_);
    $r ne 'PPI::Token::Whitespace' && $r ne 'PPI::Token::Comment'
  } $block->children;
  my $last_sig = @sig ? $sig[-1] : undef;

  # Process block contents
  my $has_content = 0;
  for my $child ($block->children) {
    my $ref = ref($child);
    next if $ref eq 'PPI::Token::Whitespace';
    next if $ref eq 'PPI::Token::Comment';

    my $is_tail = defined $last_sig && $child == $last_sig;
    $self->environment->tail_position(1) if $is_tail;
    $self->environment->tail_wants_list(1) if $is_tail && $tail_wants_list;
    $self->_process_element($child);
    $self->environment->tail_position(0) if $is_tail;
    $self->environment->tail_wants_list(0) if $is_tail && $tail_wants_list;
    $has_content = 1;
  }

  # Close any local forms opened inside this block (e.g. local $h{key})
  # Same logic as _process_block, but emitting into the temp section.
  my $end_depth = $self->{_local_let_depth} // 0;
  while ($end_depth > $saved_local_depth) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
    $self->{_local_let_depth}--;
    $end_depth--;
  }
  $self->{_local_let_depth} = $saved_local_depth;

  # Close the per-iteration closure-capture let (if one was opened).
  $self->_end_block_closure_scope($clo_scope);

  # Revert any `package NAME;` switch made inside the block (see above)
  $self->_block_depth($self->_block_depth - 1) if $has_pkg_stmt;
  $self->environment->package_stack($saved_pkg_stack);

  # Leave scope
  $self->environment->pop_scope();

  # Collect all lines from the temp section (assembled order)
  my $temp = $self->_sections->[0];
  my @body_lines = (
    @{$temp->{preamble}},
    @{$temp->{declarations}},
    @{$temp->{definitions}},
    @{$temp->{runtime}},
  );

  # Restore original state
  $self->_sections($saved_sections);
  $self->_cur_section($saved_cur_section);
  $self->_cur_bucket($saved_cur_bucket);
  $self->indent_level($saved_indent);

  # Return body as string (or "nil" if empty)
  if (@body_lines) {
    my $body = join("\n", @body_lines);
    # Runtime revert of an inline package switch: the body contains
    # (in-package X) + (p-set-current-package ...); bind both specials so
    # they restore at block exit while the tail value passes through the let.
    $body = "(let ((*package* *package*)\n"
          . "      (*pcl-current-package* *pcl-current-package*))\n"
          . "$body)"
      if $has_pkg_stmt;
    return $body;
  } else {
    return "nil";
  }
}

# Open a per-iteration closure-capture scope for a map/grep/sort block body.
# A `my` var declared in the block AND captured by a nested anonymous sub is
# renamed to a fresh, never-`defvar`'d lexical ($x__lex__N) and bound by a `let`
# wrapping the body.  The block compiles to a (lambda ($_) ...) called once per
# element, so the `let` mints a new box per element — giving Perl's per-iteration
# capture (`map { my $x=$_; sub {$x} } qw(a b c)` → "abc", not "ccc").
#
# This reproduces _with_declarations's rename, but emits the `let` directly into
# the temp-section string this path collects (the bucket-based _emit_scoped_block
# does not compose with that string collection).  Returns a state hashref for
# _end_block_closure_scope, or undef (strict no-op) when the block has no
# closure-captured block-local `my` — the overwhelmingly common case.
sub _begin_block_closure_scope {
  my ($self, $block) = @_;
  return undef unless ref($block) && $block->can('children');

  # Cheap gate: only blocks containing a nested `sub` can capture anything.
  my $captured = $self->_vars_referenced_in_closures($block);
  return undef unless %$captured;

  # Block-local `my` declarations (NOT those inside the nested sub — that path
  # is excluded by _find_all_declarations) that the closure actually captures.
  my %seen;
  my @vars = grep { !$seen{$_}++ }
             grep { $captured->{$_} }
             map  { $_->{var} }
             grep { $_->{type} eq 'my' }
             @{ $self->_find_all_declarations($block) };
  return undef unless @vars;

  my $env_renames = $self->environment->state_var_renames // {};
  my $clo = {
    saved_env       => { %$env_renames },
    saved_scope_new => $self->{_current_scope_new_renames},
    saved_scope_old => $self->{_current_scope_old_renames},
    saved_letbound  => $self->lex_home->{_let_bound_vars},
    saved_indent    => $self->indent_level,
  };

  my (%new_renames, @bindings);
  my %env = %$env_renames;
  for my $var (@vars) {
    my ($sigil, $bare) = ($var =~ /^([\$\@\%])(.+)$/);
    $sigil //= '$'; $bare //= $var;
    (my $slug = $bare) =~ s/[^a-zA-Z0-9]/_/g;
    my $uniq = sprintf('%s%s__lex__%d', $sigil, $slug, ++$lex_var_counter);
    $new_renames{$var} = $uniq;
    $env{$var}         = $uniq;
    push @bindings, "($uniq " . _let_init($sigil) . ")";
  }

  # state_var_renames → references + string interpolation emit the lexical name.
  # _current_scope_new_renames → _process_variable_statement takes the rename path
  #   (and skips _process_my_toplevel_declaration's defvar at top level).
  # _current_scope_old_renames → the RHS of `my $x = $x` sees the outer binding.
  # _let_bound_vars → the var is treated as lexical, not a package global.
  $self->environment->state_var_renames(\%env);
  $self->{_current_scope_new_renames} = { %{$clo->{saved_scope_new} // {}}, %new_renames };
  $self->{_current_scope_old_renames} = { %{$clo->{saved_scope_old} // {}},
                                          map { $_ => $clo->{saved_env}{$_} } @vars };
  $self->lex_home->{_let_bound_vars} = { %{$clo->{saved_letbound} // {}}, map { $_ => 1 } @vars };

  $self->_emit("(let (" . join(" ", @bindings) . ")");
  $self->indent_level($self->indent_level + 1);
  return $clo;
}

# Close the let opened by _begin_block_closure_scope and restore the rename maps.
sub _end_block_closure_scope {
  my ($self, $clo) = @_;
  return unless $clo;
  $self->indent_level($clo->{saved_indent});
  $self->_emit(")");
  $self->environment->state_var_renames($clo->{saved_env});
  $self->{_current_scope_new_renames} = $clo->{saved_scope_new};
  $self->{_current_scope_old_renames} = $clo->{saved_scope_old};
  $self->lex_home->{_let_bound_vars}            = $clo->{saved_letbound};
}


# Parse a block that contains hash key-value pairs: {key => val, ...}
# Used for map({key=>$_}, LIST) where the block is a hash constructor.
# Returns CL string: "(make-p-box (p-hash key val ...))"
sub parse_hash_block_to_cl_string {
  my $self  = shift;
  my $block = shift;  # PPI::Structure::Block

  my @raw = grep { ref($_) !~ /Whitespace|Comment/ } $block->children();
  if (@raw == 1 && $raw[0]->isa('PPI::Statement')) {
    @raw = grep { ref($_) !~ /Whitespace|Comment/ } $raw[0]->children();
  }

  my $result;
  eval {
    my $expr_o = $self->_expr_parser(\@raw);
    my $pair_ids = $expr_o->parse_list(\@raw);
    my ($top_node, $top_id) = $expr_o->make_node_insert('hash_init');
    for my $id (@$pair_ids) {
      $expr_o->add_child_to_node($top_id, $id);
    }
    my $gen = $self->_expr_generator($expr_o);
    $result = $gen->generate($top_id);
  };
  die $@ if $@ && $@ =~ /^PCL:/;
  return $result // '(make-p-box (p-hash))';
}

# task #78: CLForm twin of parse_hash_block_to_cl_string, for the v2
# embedded-block route (`map { {k=>$_} } …` bodies).  Same parse; the
# generation goes through gen_node_form so a converted emitter yields a
# structured form (unconverted children embed as raw atoms — the caller
# runs the embed-safety scan).  undef = decline (caller keeps v1's text).
sub parse_hash_block_to_cl_form {
  my ($self, $block) = @_;
  my @raw = grep { ref($_) !~ /Whitespace|Comment/ } $block->children();
  if (@raw == 1 && $raw[0]->isa('PPI::Statement')) {
    @raw = grep { ref($_) !~ /Whitespace|Comment/ } $raw[0]->children();
  }
  my $result;
  eval {
    my $expr_o = $self->_expr_parser(\@raw);
    my $pair_ids = $expr_o->parse_list(\@raw);
    my ($top_node, $top_id) = $expr_o->make_node_insert('hash_init');
    for my $id (@$pair_ids) {
      $expr_o->add_child_to_node($top_id, $id);
    }
    my $gen = $self->_expr_generator($expr_o);
    $result = $gen->gen_node_form($top_id);
  };
  die $@ if $@ && $@ =~ /^PCL:/;
  return $result;
}


# Find all variable declarations recursively in a PPI element
# Returns arrayref of { type => 'my'|'our'|..., var => '$x' }
sub _find_all_declarations {
  my $self = shift;
  my $elem = shift;
  my @decls;

  # Handle arrays of elements
  my @children;
  if (ref($elem) eq 'ARRAY') {
    @children = @$elem;
  } elsif (ref($elem) && $elem->can('children')) {
    @children = $elem->children;
  } else {
    return [];
  }

  my $pending_decl;
  for my $child (@children) {
    my $ref = ref($child);

    # Found a declarator keyword
    if ($ref eq 'PPI::Token::Word' && $child->content =~ /^(my|our|state|local)$/) {
      $pending_decl = $1;
    }
    # Found a variable after declarator
    elsif ($pending_decl && $ref eq 'PPI::Token::Symbol') {
      push @decls, { type => $pending_decl, var => $child->content };
      $pending_decl = undef;
    }
    # Found a list after declarator: my ($x, $y)
    elsif ($pending_decl && $ref eq 'PPI::Structure::List') {
      # Find all symbols inside the list
      my @list_vars = $self->_find_symbols_in_list($child);
      for my $var (@list_vars) {
        push @decls, { type => $pending_decl, var => $var };
      }
      $pending_decl = undef;
      # Don't recurse into this list - we already processed it
      next;
    }
    # Not a symbol or list after declarator - reset
    elsif ($pending_decl && $ref !~ /Whitespace/) {
      $pending_decl = undef;
    }

    # Recurse into nested elements, but NOT into:
    #   - Named sub definitions (PPI::Statement::Sub)
    #   - BEGIN/END/etc blocks (PPI::Statement::Scheduled)
    #   - Anonymous sub bodies: PPI::Structure::Block whose prev sibling is 'sub'
    #   - eval { } / do { } blocks: PPI::Structure::Block whose prev sibling is
    #     'eval' or 'do'.  Each becomes its own CL lambda scope — `my` vars
    #     inside are scoped to that block, so hoisting them to the enclosing let
    #     would (a) shadow outer vars of the same name and (b) leave the
    #     hoisted let open around the rest of the enclosing body (the do-block's
    #     own (let …) then double-binds and the outer one nests every following
    #     statement, breaking e.g. intra-sub goto/label tagbody wrapping).
    # For bare blocks (no prev non-whitespace sibling): recurse but only keep
    #   'state' declarations — 'my' vars in bare blocks are scoped to the block
    #   by _process_bare_block/_with_declarations and must NOT be hoisted to the
    #   enclosing sub level (would shadow same-name package globals outside).
    if ($ref && $child->can('children')
        && $ref ne 'PPI::Statement::Sub'
        && $ref ne 'PPI::Statement::Scheduled'
        && !($ref eq 'PPI::Structure::Block' && do {
               my $prev = $child->sprevious_sibling;
               $prev && ref($prev) eq 'PPI::Token::Word'
                     && ($prev->content eq 'sub' || $prev->content eq 'eval'
                         || $prev->content eq 'do')
             })) {
      my $is_bare_block = $ref eq 'PPI::Structure::Block' && do {
        my $prev = $child->sprevious_sibling;
        !$prev;
      };
      my $inner = $self->_find_all_declarations($child);
      if ($is_bare_block) {
        push @decls, grep { $_->{type} eq 'state' } @$inner;
      } else {
        push @decls, @$inner;
      }
    }
  }

  return \@decls;
}

# Return true if the block contains any directly-nested named sub statements.
# Used by _process_sub_statement to decide whether to use defvar (global) vs
# let (lexical) for state variables: if inner named subs exist, they must be
# hoisted to the definitions bucket and need to access state vars globally.
sub _block_has_inner_named_subs {
  my ($self, $block) = @_;
  for my $child ($block->children) {
    next unless ref($child) eq 'PPI::Statement::Sub';
    for my $c ($child->children) {
      my $ref = ref($c);
      next if $ref eq 'PPI::Token::Whitespace';
      next if $ref eq 'PPI::Token::Word' && $c->content =~ /^(sub|my|our|state)$/;
      return 1 if $ref eq 'PPI::Token::Word';  # first non-reserved word = name
      last;
    }
  }
  return 0;
}

# Helper: find all symbol names in a list structure like ($x, $y, @z)
sub _find_symbols_in_list {
  my $self = shift;
  my $list = shift;
  my @vars;

  for my $child ($list->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Token::Symbol') {
      push @vars, $child->content;
    }
    elsif ($ref && $child->can('children')) {
      push @vars, $self->_find_symbols_in_list($child);
    }
  }

  return @vars;
}

# True if the declaration list contains an `undef` placeholder — my (undef, $x).
# Such a placeholder occupies a position in the list assignment but is not a
# declared symbol, so it must be preserved positionally (the single-var
# (vector $x) shortcut drops it).  Walks nested structures like _find_symbols.
sub _list_has_undef_placeholder {
  my $self = shift;
  my $list = shift;

  for my $child ($list->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Token::Word' && $child->content eq 'undef') {
      return 1;
    }
    elsif ($ref && $child->can('children')) {
      return 1 if $self->_list_has_undef_placeholder($child);
    }
  }
  return 0;
}

# Like _find_symbols_in_list but also includes undef placeholders.
# Used by local() to preserve undef skip slots in list assignment.
sub _find_symbols_and_undefs_in_list {
  my $self = shift;
  my $list = shift;
  my @vars;

  for my $child ($list->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Token::Symbol' || $ref eq 'PPI::Token::Magic') {
      push @vars, $self->_transform_pkg_var($child->content);
    }
    elsif ($ref eq 'PPI::Token::Word' && $child->content eq 'undef') {
      push @vars, '(p-undef)';  # skip marker for p-list-=
    }
    elsif ($ref && $child->can('children')) {
      push @vars, $self->_find_symbols_and_undefs_in_list($child);
    }
  }

  return @vars;
}

# Return a hashref of all symbol names referenced inside any nested 'sub { }' blocks
# found within $elements. These are the variables "captured" by closures.
# Only direct PPI children's sub-blocks are searched; the caller intersects
# the result with _find_all_declarations to know which to rename.
sub _vars_referenced_in_closures {
  my ($self, $elements) = @_;
  my @elems = ref($elements) eq 'ARRAY' ? @$elements : ($elements);
  my %captured;
  for my $elem (@elems) {
    next unless ref($elem) && $elem->can('find');
    my $sub_kws = $elem->find(
      sub { $_[1]->isa('PPI::Token::Word') && $_[1]->content eq 'sub' }
    ) || [];   # PPI returns 0 (not undef) when nothing found — use || not //
    for my $kw (@$sub_kws) {
      # NAMED subs (sub foo { ... }) count as capturers too: a named sub inside
      # a block that references a block-local 'my' must see that lexical (Perl
      # closes over the first instance).  Without the rename, the block's let
      # of the defvar'd name dynamically shadows the global the defun reads —
      # the sub sees nil (Moo: Sub::Quote's eval'd `{ my $default_for_b = ...;
      # sub new { ... $default_for_b->($new) ... } }`).  Over-inclusion is safe:
      # callers intersect this set with the block-local 'my' declarations, so
      # file-level `my $x; sub f { $x }` (no enclosing block) is unaffected.

      # Walk forward to find the block (skipping name/prototypes/attributes)
      my $sib = $kw->next_sibling;
      $sib = $sib->next_sibling while $sib && !$sib->isa('PPI::Structure::Block');
      next unless $sib;
      my $syms = $sib->find('PPI::Token::Symbol') || [];  # same: || not //
      $captured{$_->content} = 1 for @$syms;

      # Variables used ONLY via string interpolation / regex (e.g. sub { "v=$x" })
      # are not PPI::Token::Symbol nodes — they live inside quote/heredoc/regex
      # tokens. Scan those too, or the closure-capture rename misses them and the
      # var stays a shared global. Over-inclusion is safe: callers intersect this
      # set with the block-local `my` declarations.
      my $interp = $sib->find(sub {
        my $t = $_[1];
        $t->isa('PPI::Token::Quote::Double')
          || $t->isa('PPI::Token::Quote::Interpolate')
          || $t->isa('PPI::Token::QuoteLike::Backtick')
          || $t->isa('PPI::Token::QuoteLike::Command')
          || $t->isa('PPI::Token::HereDoc')
          || $t->isa('PPI::Token::Regexp::Match')
          || $t->isa('PPI::Token::Regexp::Substitute');
      }) || [];
      for my $t (@$interp) {
        $captured{$_} = 1 for _vars_in_interpolated_text($t->content);
      }
    }
  }
  return \%captured;
}

# Extract interpolated variable names ($name, ${name}, @name, @{name}, and the
# base var of $name[..]/$name{..}) from raw interpolating text. A deliberately
# liberal over-approximation used only to decide closure capture; an unescaped
# sigil immediately followed by a word character (or {word}) is taken as a ref.
sub _vars_in_interpolated_text {
  my ($txt) = @_;
  my @vars;
  while ($txt =~ /(?<!\\)([\$\@])\{?(\w+)\}?/g) {
    push @vars, "$1$2";
  }
  return @vars;
}

# Build the $outer scope hashref passed to BlockAnalyzer::analyze.
# Collects let-bound, state-renamed, constant, and our variables from the
# current environment so BlockAnalyzer can distinguish local from outer refs.
sub _current_outer_scope {
  my ($self) = @_;
  my %outer;
  for my $v (keys %{$self->lex_home->{_let_bound_vars} // {}}) {
    $outer{$v} = { type => 'my', cl_name => $v };
  }
  my $renames = $self->environment->state_var_renames // {};
  for my $v (keys %$renames) {
    $outer{$v} = { type => 'state', cl_name => $renames->{$v} };
  }
  return \%outer;
}

# CL initialization expression for a let-binding by sigil.
sub _let_init {
  my ($sigil) = @_;
  return '(make-array 0 :adjustable t :fill-pointer 0)' if $sigil eq '@';
  return "(make-hash-table :test #'equal)"              if $sigil eq '%';
  return '(make-p-box nil)';
}

# Scoped block codegen — opens nested (let ...) forms at the exact statement
# where each 'my' declaration first appears, rather than hoisting them all to
# the top of the block.  Called by _with_declarations when $elements is a
# PPI::Structure::Block.  Sets _stmt_pre_hook so _process_block fires the hook
# before each significant statement.
sub _emit_scoped_block {
  my ($self, $analysis, $emit_body) = @_;

  my $decls      = $analysis->{declarations};
  my $vars       = $analysis->{vars};
  my $state_vars = $self->{_current_state_vars} // {};

  # Collect globally unique 'my' vars (excluding state vars and vars already
  # let-bound by an enclosing _emit_scoped_block, preserving order).
  my $already_bound = $self->lex_home->{_let_bound_vars} // {};
  my (%seen_var);
  my @all_my_vars = grep { !$seen_var{$_}++ && !$state_vars->{$_}
                                             && !$already_bound->{$_} }
                    map  { @{$_->{vars}} }
                    grep { $_->{decl_type} eq 'my' } @$decls;

  # Nothing to scope? Emit body, but still isolate _pending_let_closes so that
  # inner _process_block calls (e.g. then/else blocks of a nested if) do not
  # accidentally flush pending closes that belong to an enclosing scoped block.
  unless (@all_my_vars) {
    my $saved_pending = $self->{_pending_let_closes};
    $self->{_pending_let_closes} = [];
    $emit_body->();
    $self->{_pending_let_closes} = $saved_pending;
    return;
  }

  # Compute renames: closure-captured vars → __lex__N.  (Case-collision
  # renaming retired 2026-06-21 — under (readtable-case :invert) lexicals that
  # differ only in case, e.g. $T and $t, already map to distinct CL symbols.)
  my (%new_renames, %old_renames);
  my $existing = $self->environment->state_var_renames // {};
  for my $var (@all_my_vars) {
    my $vinfo = $vars->{$var} // {};
    if ($vinfo->{captured}) {
      my ($sigil, $bare) = ($var =~ /^([\$\@\%])(.+)$/);
      ($sigil, $bare) = ('$', $var) unless defined $bare;
      (my $slug = $bare) =~ s/[^a-zA-Z0-9]/_/g;
      my $u = sprintf('%s%s__lex__%d', $sigil, $slug, ++$lex_var_counter);
      $new_renames{$var} = $u;
      $old_renames{$var} = $existing->{$var};
    }
  }

  # Apply renames to the environment.
  my ($saved_env_renames, $saved_scope_renames);
  $saved_scope_renames = $self->{_current_scope_new_renames};
  if (%new_renames) {
    $saved_env_renames = $self->environment->state_var_renames // {};
    $self->environment->state_var_renames({ %$saved_env_renames, %new_renames });
    $self->{_current_scope_new_renames} = \%new_renames;
    $self->{_current_scope_old_renames} = \%old_renames;
  }

  # Build ppi_stmt_key → [vars] map for the hook.
  # Vars for the same ppi_stmt are batched into one let form.
  my (%vars_at_ppi, %seen_at_ppi);
  for my $d (@$decls) {
    next if $d->{decl_type} ne 'my';
    my $key = "$d->{ppi_stmt}";
    for my $var (@{$d->{vars}}) {
      next if $state_vars->{$var} || $seen_at_ppi{$key}{$var}++;
      push @{$vars_at_ppi{$key}}, $var;
    }
  }

  # Install the per-statement hook.  _process_block calls it with each child
  # element before dispatching.  The hook opens (let ...) for any 'my' vars
  # declared at that statement's position.
  #
  # Pending closes are stored on $self->{_pending_let_closes} (not a local var)
  # so that _process_block can flush them at the end of its statement loop,
  # BEFORE any tagbody/:next structure emitted by $emit_body closes things up.
  # _emit_scoped_block saves/restores so nested scopes don't interfere.
  my $saved_hook        = $self->{_stmt_pre_hook};
  my $old_let_vars      = $self->lex_home->{_let_bound_vars};
  my $saved_pending     = $self->{_pending_let_closes};
  $self->lex_home->{_let_bound_vars}     = { %{$old_let_vars // {}} };
  $self->{_pending_let_closes} = [];

  $self->{_stmt_pre_hook} = sub {
    my ($parser, $child) = @_;
    my $key = "$child";
    return unless $vars_at_ppi{$key};

    my @bindings;
    for my $var (@{$vars_at_ppi{$key}}) {
      my $lv    = $new_renames{$var} // $var;
      my $sigil = substr($lv, 0, 1);
      push @bindings, "($lv " . _let_init($sigil) . ")";
      $parser->lex_home->{_let_bound_vars}{$lv} = 1;
    }
    return unless @bindings;
    $parser->_emit("(let (" . join(" ", @bindings) . ")");
    $parser->indent_level($parser->indent_level + 1);
    push @{$parser->{_pending_let_closes}}, 1;
  };

  $emit_body->();

  # Restore state.  Pending closes are flushed inside _process_block (at end
  # of the statement loop, before any tagbody/:next structure emitted by
  # $emit_body).  Nothing left to close here.
  $self->lex_home->{_let_bound_vars}     = $old_let_vars;
  $self->{_pending_let_closes} = $saved_pending;
  $self->{_stmt_pre_hook}      = $saved_hook;
  if (%new_renames) {
    $self->environment->state_var_renames($saved_env_renames);
    $self->{_current_scope_new_renames} = $saved_scope_renames;
    delete $self->{_current_scope_old_renames};
  }
}

# Common helper: wrap emitted code with let for any 'my' declarations
# Usage: $self->_with_declarations($ppi_elements, sub { ... emit code ... });
# $ppi_elements can be a single PPI element or arrayref of elements to scan
# True if the block has a standalone statement label (`LABEL:` with no block of
# its own — i.e. a goto target, not a loop/bare-block label).  Such a body uses
# the flat-let declaration path (not the two-phase per-statement scoped lets) so
# the label and its sibling statements share one lexical scope, letting
# _process_block wrap them in a single (tagbody …) for `goto LABEL`.
sub _block_has_standalone_label {
  my $block = shift;
  return 0 unless ref($block) eq 'PPI::Structure::Block';
  for my $c ($block->schildren) {
    next unless ref($c) eq 'PPI::Statement::Compound';
    next unless $c->find_first('PPI::Token::Label');
    return 1 unless $c->find_first('PPI::Structure::Block');
  }
  return 0;
}

sub _with_declarations {
  my $self = shift;
  my $elements = shift;  # PPI element(s) to scan for declarations
  my $emit_body = shift; # Callback to emit the body code
  my $is_sub_body = shift // 0;  # 1 only for direct sub body blocks

  # Phase 2: for DIRECT sub body blocks only, use the two-phase scoping fix
  # (_emit_scoped_block) which opens let-bindings at the exact statement where
  # each 'my' declaration first appears, rather than hoisting everything to
  # the top of the block.
  #
  # IMPORTANT: restrict to $is_sub_body=1, set only from _process_sub_statement.
  # if/else/while/bare blocks INSIDE subs must NOT use _emit_scoped_block:
  # those inner blocks share their parent sub's rename map, and running BlockAnalyzer
  # on them re-fires closure-capture detection and creates a spurious nested let that
  # shadows already-bound outer vars (e.g. breaks closure.t bizz() test).
  #
  # At the top level (in_subroutine=0), 'my' vars are defvar'd as dynamic variables,
  # and inline-let semantics interact badly with defvar + _process_my_toplevel_declaration.
  if (ref($elements) eq 'PPI::Structure::Block'
      && $self->environment->in_subroutine > 0
      && $is_sub_body
      && !_block_has_standalone_label($elements)) {
    require Pl::BlockAnalyzer;
    my $outer    = $self->_current_outer_scope();
    my $analysis = Pl::BlockAnalyzer->analyze($elements, $outer);

    # Supplemental hoisting: _find_all_declarations does a deep recursive search
    # and finds 'my' vars declared inside expressions (e.g. open(my $fh, '>', ...)).
    # BlockAnalyzer only sees statement-level PPI::Statement::Variable nodes.
    # Any var found by the deep search but not by BlockAnalyzer needs a hoisted
    # flat-let at the top of the block so it is visible to all subsequent statements.
    my $state_vars    = $self->{_current_state_vars} // {};
    my $already_bound = $self->lex_home->{_let_bound_vars}     // {};
    my %stmt_level    = map  { $_ => 1 }
                        grep { !$state_vars->{$_} && !$already_bound->{$_} }
                        map  { @{$_->{vars}} }
                        grep { $_->{decl_type} eq 'my' } @{$analysis->{declarations}};
    my $deep_decls    = $self->_find_all_declarations($elements);
    my (@hoisted, %seen_hoist);
    for my $d (@$deep_decls) {
      next unless $d->{type} eq 'my';
      my $v = $d->{var};
      next if $seen_hoist{$v}++ || $stmt_level{$v}
           || $state_vars->{$v}  || $already_bound->{$v};
      push @hoisted, $v;
    }

    if (@hoisted) {
      # Open a hoisted flat-let for expression-level my vars, wrapping the
      # inline-let scoped block. This ensures vars like $fh (from open(my $fh,...))
      # are visible to all subsequent statements in the block.
      my $bindings = join(" ", map {
        my $sigil = substr($_, 0, 1);
        "($_ " . _let_init($sigil) . ")"
      } @hoisted);
      $self->_emit("(let ($bindings)");
      $self->indent_level($self->indent_level + 1);
      my $old_let = $self->lex_home->{_let_bound_vars};
      $self->lex_home->{_let_bound_vars} = { %{$old_let // {}}, map { $_ => 1 } @hoisted };
      $self->_emit_scoped_block($analysis, $emit_body);
      $self->lex_home->{_let_bound_vars} = $old_let;
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")");
    } else {
      $self->_emit_scoped_block($analysis, $emit_body);
    }
    return;
  }

  # Collect declarations from all elements
  my @all_decls;
  my @elems = ref($elements) eq 'ARRAY' ? @$elements : ($elements);
  for my $elem (@elems) {
    next unless defined $elem;
    push @all_decls, @{$self->_find_all_declarations($elem)};
  }

  # Get unique 'my' variables (skip state vars - they're handled at sub level)
  my $state_vars = $self->{_current_state_vars} // {};
  my %seen;
  my @my_vars = grep { !$seen{$_}++ }
                grep { !$state_vars->{$_} }  # skip state vars
                map { $_->{var} }
                grep { $_->{type} eq 'my' } @all_decls;

  # When inside a subroutine, rename 'my' vars that are captured by nested closures.
  # Fresh names (e.g. $i__lex__3) are never defvar'd, so the CL 'let' creates a
  # LEXICAL binding. Lambdas then capture the correct per-call copy, not a dynamic ref.
  my %new_renames;  # original perl name → unique CL name
  my %old_renames;  # original perl name → previous rename entry (undef if absent)
  if (@my_vars) {
    my $captured  = $self->_vars_referenced_in_closures($elements);
    my $existing  = $self->environment->state_var_renames // {};
    for my $var (@my_vars) {
      next unless $captured->{$var};
      my ($sigil, $bare) = ($var =~ /^([\$\@\%])(.+)$/);
      $sigil //= '$'; $bare //= $var;
      (my $slug = $bare) =~ s/[^a-zA-Z0-9]/_/g;
      my $unique = sprintf('%s%s__lex__%d', $sigil, $slug, ++$lex_var_counter);
      $new_renames{$var} = $unique;
      $old_renames{$var} = $existing->{$var};  # undef if no prior rename
    }
    # (Case-collision renaming retired 2026-06-21: under (readtable-case
    # :invert) lexicals that differ only in case — e.g. $T and $t — already
    # map to distinct CL symbols, so no disambiguation pass is needed.)
  }

  # Wrap in let if we have declarations
  if (@my_vars) {
    # Pre-scan: for 'my @arr = EXPR' / 'my (LIST) = EXPR' statements where the RHS
    # self-references a declared array/hash variable, pre-evaluate EXPR in the let
    # binding init position so the outer binding is still visible.
    # Single-var my @arr=EXPR: use full EXPR as init, mark to skip body assignment.
    # Multi-var  my(LIST)=EXPR: pre-init @arr to a copy of its outer value only;
    #   keep body so p-list-= can do the actual list destructuring from the correct source.
    my %arr_rhs_inits;   # let_var => cl_init_string
    my %skip_body_vars;  # perl_var => 1  (skip body emit for single-var case)
    {
      my %my_ah_set = map { $_ => 1 } grep { /^[@%]/ } @my_vars;
      if (%my_ah_set) {
        my @top_stmts;
        if (ref($elements) eq 'PPI::Structure::Block') {
          @top_stmts = grep { ref($_) eq 'PPI::Statement::Variable' } $elements->children;
        } elsif (ref($elements) eq 'ARRAY') {
          @top_stmts = grep { ref($_) eq 'PPI::Statement::Variable' } @$elements;
        }
        for my $chk_stmt (@top_stmts) {
          my @sp = grep { ref($_) ne 'PPI::Token::Whitespace' &&
                          !(ref($_) eq 'PPI::Token::Structure' && $_->content eq ';') }
                   $chk_stmt->children;
          next unless @sp >= 3;
          next unless ref($sp[0]) eq 'PPI::Token::Word' && $sp[0]->content eq 'my';
          my $eq_idx = -1;
          my @decl_ah;
          my $is_single = 0;
          if (ref($sp[1]) eq 'PPI::Token::Symbol') {
            my $v = $sp[1]->content;
            push @decl_ah, $v if $my_ah_set{$v};
            $is_single = 1;
            for my $i (2 .. $#sp) {
              if (ref($sp[$i]) eq 'PPI::Token::Operator' && $sp[$i]->content eq '=') {
                $eq_idx = $i; last;
              }
            }
          } elsif (ref($sp[1]) eq 'PPI::Structure::List') {
            # List children may be wrapped in PPI::Statement::Expression — use find()
            my $found = $sp[1]->find('PPI::Token::Symbol') || [];
            for my $lv (@$found) {
              my $v = $lv->content;
              push @decl_ah, $v if $my_ah_set{$v};
            }
            for my $i (2 .. $#sp) {
              if (ref($sp[$i]) eq 'PPI::Token::Operator' && $sp[$i]->content eq '=') {
                $eq_idx = $i; last;
              }
            }
          }
          next if $eq_idx < 0 || !@decl_ah;
          my @rhs_p = @sp[$eq_idx+1 .. $#sp];
          next unless @rhs_p;
          # Skip double-my (e.g. my @x = my @x = qw(...)) — existing code handles it
          next if grep { ref($_) eq 'PPI::Token::Word' && $_->content eq 'my' } @rhs_p;
          # Collect all Symbol tokens from RHS (including inside nested structures)
          my @rhs_syms;
          for my $rp (@rhs_p) {
            if (ref($rp) eq 'PPI::Token::Symbol') {
              push @rhs_syms, $rp->content;
            } elsif ($rp->can('find')) {
              my $found = $rp->find('PPI::Token::Symbol') || [];
              push @rhs_syms, map { $_->content } @$found;
            }
          }
          my %rhs_sym_set = map { $_ => 1 } @rhs_syms;
          my @self_ref = grep { $rhs_sym_set{$_} } @decl_ah;
          next unless @self_ref;
          if ($is_single) {
            my $var     = $self_ref[0];
            my $rhs_cl  = $self->_parse_expression(\@rhs_p, $chk_stmt) // 'nil';
            my $let_var = $new_renames{$var} // $var;
            my $sigil   = substr($var, 0, 1);
            my $copyfn  = $sigil eq '@' ? 'p-copy-array' : 'p-copy-hash';
            $arr_rhs_inits{$let_var} = "($copyfn (let ((*wantarray* t)) $rhs_cl))";
            $skip_body_vars{$var} = 1;
          } else {
            for my $var (@self_ref) {
              my $sigil   = substr($var, 0, 1);
              my $copyfn  = $sigil eq '@' ? 'p-copy-array' : 'p-copy-hash';
              my $outer   = $old_renames{$var} // $var;
              my $let_var = $new_renames{$var} // $var;
              $arr_rhs_inits{$let_var} = "($copyfn (let ((*wantarray* t)) $outer))";
            }
          }
        }
      }
    }

    # Build let bindings using the (possibly renamed) CL variable names
    my $bindings = join(" ", map {
      my $let_var = $new_renames{$_} // $_;
      my $sigil = substr($let_var, 0, 1);
      my $init = exists $arr_rhs_inits{$let_var} ? $arr_rhs_inits{$let_var}
               : $sigil eq '@' ? '(make-array 0 :adjustable t :fill-pointer 0)'
               : $sigil eq '%' ? "(make-hash-table :test #'equal)"
               :                 '(make-p-box nil)';
      "($let_var $init)"
    } @my_vars);
    $self->_emit("(let ($bindings)");
    $self->indent_level($self->indent_level + 1);

    # Track renamed/original vars as let-bound so _emit replaces p-scalar-= with
    # p-my-= (box-set), preventing the proclaim-special side-effect that would
    # turn future let bindings from lexical to dynamic and break closure capture.
    #
    # HAZARD: _let_bound_vars is critical for correctness of closure capture.
    # p-scalar-= has a side effect: (unless (boundp ',place) (proclaim '(special ,place)))
    # which converts a variable to a CL special (dynamic) on its first write. Once
    # special, ALL let-bindings of that name become dynamic forever in this image —
    # closures capture the symbol (which is nil after the loop) rather than the
    # per-iteration value. p-my-= (box-set) skips this proclaim, preserving lexicality.
    # If you add a new let-binding path, you MUST update _let_bound_vars accordingly.
    my $old_let_vars = $self->lex_home->{_let_bound_vars};
    my @bound_names = map { $new_renames{$_} // $_ } @my_vars;
    $self->lex_home->{_let_bound_vars} = { %{$old_let_vars // {}}, map { $_ => 1 } @bound_names };

    # Apply new renames to environment so ExprToCL emits the unique CL names.
    # Also expose them via _current_scope_new_renames for _process_variable_statement
    # to split RHS parsing (handles 'my $i = $i + 1' shadowing correctly).
    #
    # Shadow removal: if any let-bound 'my' var is also in state_var_renames (e.g.
    # 'my $x' in 'foreach my $x' after an outer 'state $x' registered a rename),
    # remove it from the map so the let binding (not the state defvar) is used inside.
    my $saved_env_renames;
    my $saved_scope_renames = $self->{_current_scope_new_renames};
    my $cur_env_renames = $self->environment->state_var_renames // {};
    my %shadowed_state = map { $_ => 1 }
                         grep { exists $cur_env_renames->{$_} }
                         @my_vars;
    if (%new_renames || %shadowed_state) {
      $saved_env_renames = $cur_env_renames;
      my %merged = %$cur_env_renames;
      delete @merged{keys %shadowed_state};    # remove state renames shadowed by let
      %merged = (%merged, %new_renames);       # apply closure-capture renames
      $self->environment->state_var_renames(\%merged);
      if (%new_renames) {
        $self->{_current_scope_new_renames} = \%new_renames;
        $self->{_current_scope_old_renames} = \%old_renames;
      }
      else {
        # All of this body's 'my' vars shadow renames from an OUTER sub scope
        # (e.g. `my $c = sub { my $c = f(); $c }` — Moo's install_delayed maker).
        # The let above binds the PLAIN name and the env map was stripped, so
        # references resolve plainly; _current_scope_new_renames must drop the
        # shadowed names too, or _process_variable_statement's rename path
        # emits the inner decl's assignment against the OUTER __lex__ variable
        # while the body reads the plain let binding (assignment lost).
        my %scope = %{ $saved_scope_renames // {} };
        delete @scope{keys %shadowed_state};
        $self->{_current_scope_new_renames} = \%scope;
      }
    }

    # Save/restore _my_binding_init_vars so nested _with_declarations calls don't interfere.
    # REPLACE (don't merge) with this block's skip set: inner blocks that create a new
    # let for @bee must NOT inherit the outer block's skip flag for @bee — the inner let
    # has its own init and should not inherit the skip from an outer block's let.
    my $old_skip_body = $self->{_my_binding_init_vars};
    $self->{_my_binding_init_vars} = \%skip_body_vars;

    $emit_body->();

    $self->{_my_binding_init_vars} = $old_skip_body;

    # Restore rename map
    if (%new_renames || %shadowed_state) {
      $self->environment->state_var_renames($saved_env_renames);
      if (%new_renames) {
        $self->{_current_scope_new_renames} = $saved_scope_renames;
        delete $self->{_current_scope_old_renames};
      }
      else {
        $self->{_current_scope_new_renames} = $saved_scope_renames;
      }
    }

    $self->lex_home->{_let_bound_vars} = $old_let_vars;
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  } else {
    $emit_body->();
  }
}

# Parse a condition (from PPI::Structure::Condition)
# Returns: ($cl_code, $declarations_arrayref) in list context
#          $cl_code in scalar context
sub _parse_condition {
  my $self = shift;
  my $cond = shift;

  # Find ALL declarations recursively (including nested ones like my $x = my $y = 3)
  my $all_decls = $self->_find_all_declarations($cond);

  # Get the expression inside the condition
  my @parts;
  for my $child ($cond->children) {
    my $ref = ref($child);
    next if $ref eq 'PPI::Token::Whitespace';

    if ($ref eq 'PPI::Statement::Expression') {
      # Get children of the expression
      push @parts, grep {
        ref($_) ne 'PPI::Token::Whitespace'
      } $child->children;
    }
    else {
      push @parts, $child;
    }
  }

  my ($result, $decls) = $self->_parse_expression(\@parts, $cond);
  $result //= "nil";

  # The expression generator prefixes the result with (indent_str x
  # indent_level).  A condition is inlined right after "(p-if "/"(p-while " on
  # the SAME line, so that leading indentation produces a weird gap
  # ("(p-if                       (cond)").  Strip the leading whitespace; any
  # internal newlines (multi-line conditions) keep their alignment.
  $result =~ s/^[ \t]+//;

  # Merge: use our recursive findings (which catches nested decls)
  return wantarray ? ($result, $all_decls) : $result;
}


# Process while/until loop
sub _process_while_statement {
  my $self    = shift;
  my $stmt    = shift;
  my $keyword = shift;
  my $label   = shift;  # Optional loop label

  my $perl_code = $self->_compound_comment($stmt);
  $self->_emit(";; $perl_code");

  # Find condition, block, and optional continue block
  my ($cond, $block, $continue_block);
  my $found_body = 0;
  my $found_continue = 0;
  for my $child ($stmt->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Structure::Condition') {
      $cond = $child;
    }
    elsif ($ref eq 'PPI::Structure::Block') {
      if (!$found_body) {
        $block = $child;
        $found_body = 1;
      } elsif ($found_continue) {
        $continue_block = $child;
      }
    }
    elsif ($ref eq 'PPI::Token::Word' && $child->content eq 'continue') {
      $found_continue = 1;
    }
  }

  # Get condition CL code
  my $cond_cl = $cond ? $self->_parse_condition($cond) : "t";
  $cond_cl //= "t";
  $cond_cl =~ s/^\s+//;  # generate() prepends indentation; strip it for regex checks below

  # while (<FH>) with no explicit assignment → implicitly assign to $_
  # PPI: condition has a single Expression containing a single QuoteLike::Readline
  if ($cond && $cond_cl =~ /^\(p-readline\b/) {
    my @non_ws = grep { !ref($_) || ref($_) ne 'PPI::Token::Whitespace' } $cond->children;
    if (@non_ws == 1 && ref($non_ws[0]) eq 'PPI::Statement::Expression') {
      my @expr_ch = grep { ref($_) ne 'PPI::Token::Whitespace' } $non_ws[0]->children;
      if (@expr_ch == 1 && ref($expr_ch[0]) eq 'PPI::Token::QuoteLike::Readline') {
        $cond_cl = "(p-setf \$_ $cond_cl)";
      }
    }
  }

  # Perl auto-defined insertion: while ($x = FUNC) terminates when FUNC returns undef,
  # not when it returns a false-but-defined value like "0".
  # Functions: each, readdir, readline (<FH>), glob.
  # Patterns:
  #   (p-scalar-= $var (p-each/readdir/readline/glob ...)) → (progn COND (p-defined $var))
  #   (p-my-= $var (p-each/readdir/readline/glob ...))     → same
  #   (p-setf (p-gethash/aref ...) (p-each/readdir/...))  → (p-defined COND)
  #   Bare (p-readdir/p-glob ...)   → (progn (p-setf $_ COND) (p-defined $_))
  #   Bare (p-each ...)             → same (sets $_ to each's return value)
  $cond_cl = $self->_auto_defined_cond($cond_cl) if $keyword ne 'until';

  # Handle 'until' by negating
  if ($keyword eq 'until') {
    $cond_cl = "(p-not $cond_cl)";
  }

  # Build the loop form with optional label
  my $label_arg = $label ? ' :label ' . cl_sym($label) : "";

  # Use common helper to wrap with declarations
  $self->_with_declarations($cond, sub {
    $self->_emit("(p-while $cond_cl$label_arg");
    $self->indent_level($self->indent_level + 1);
    if ($block) {
      $self->_with_declarations($block, sub {
        $self->_process_block($block, 1);
      });
    }
    if ($continue_block) {
      $self->_emit(":continue (progn");
      $self->indent_level($self->indent_level + 1);
      $self->_process_block($continue_block, 1);
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")");
    }
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  });

  $self->_emit("");
}


# Process for/foreach loop
# Apply Perl's implicit defined() insertion to a loop CONDITION whose value is
# the result of each / readdir / readline(<FH>) / glob — these terminate the
# loop on *undef*, not on a false-but-defined value (each's index 0, a "0" line,
# an empty record).  The call may be wrapped in a CONTEXT BIND — `(p-scalar-ctx
# …)` / `(p-list-ctx …)` since #281 item 1, and still the raw `(let
# ((*wantarray* nil/t)) …)` in older captured text (the wantarray-leak fix, see
# docs/wantarray-leak-review.md) — so the detection sees through either
# spelling.  Shared by the while, while-modifier and C-style-for condition
# handlers.  Returns the (possibly rewritten) condition.
sub _auto_defined_cond {
  my ($self, $cond_cl) = @_;
  return $cond_cl unless defined $cond_cl;
  my $auto_pat = qr/p-each|p-readdir|p-readline|p-glob/;
  my $waw      = qr/(?:\(let \(\(\*wantarray\* (?:nil|t)\)\) |\(p-(?:list|scalar|void|caller)-ctx )?/;
  if ($cond_cl =~ /^\(p-(?:scalar|my)-=\s+(\$\S+)\s+$waw\((?:$auto_pat)\b/) {
    return "(progn $cond_cl (p-defined $1))";
  } elsif ($cond_cl =~ /^\((?:p-)?setf\s+\(p-(?:gethash|aref)\b.*\((?:$auto_pat)\b/) {
    # (`setf` as well as `p-setf`: ExprToCL's elem-setf rule writes a
    # let-bound container's element through CL setf directly, s411)
    return "(p-defined $cond_cl)";
  } elsif ($cond_cl =~ /^\(p-setf\s+\$_\s+$waw\((?:$auto_pat)\b/) {
    return "(progn $cond_cl (p-defined \$_))";
  } elsif ($cond_cl =~ /^\(box-set\s+\(p-if\b.*$waw\((?:$auto_pat)\b/) {
    # `($c ? $a : $b) = readdir(D)` — a scalar assignment through the ternary
    # lvalue (ExprToCL's _sole_ternary_lvalue_id branch); box-set returns the
    # TARGET BOX, whose value is the just-assigned value, so defined() applies
    # to it directly (defins.t t10).
    return "(p-defined $cond_cl)";
  } elsif ($cond_cl =~ /^$waw\((?:$auto_pat)\b/) {
    return "(progn (p-setf \$_ $cond_cl) (p-defined \$_))";
  }
  return $cond_cl;
}

sub _process_for_statement {
  my $self    = shift;
  my $stmt    = shift;
  my $keyword = shift;
  my $label   = shift;  # Optional loop label

  my $perl_code = $self->_compound_comment($stmt);
  $self->_emit(";; $perl_code");

  # Check for C-style for vs foreach style, and detect continue block
  my $c_style_for;
  my $block;
  my $continue_block;
  my $found_continue = 0;

  for my $child ($stmt->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Structure::For') {
      $c_style_for = $child;
    }
    elsif ($ref eq 'PPI::Structure::Block') {
      if ($found_continue) {
        $continue_block = $child;
      } elsif (!$block || $c_style_for) {
        $block = $child;
      }
    }
    elsif ($ref eq 'PPI::Token::Word' && $child->content eq 'continue') {
      $found_continue = 1;
    }
  }

  if ($c_style_for) {
    $self->_process_c_style_for($c_style_for, $block, $stmt, $label);
  }
  else {
    $self->_process_foreach_loop($stmt, $block, $label, $continue_block);
  }
}


# Process C-style for loop: for (init; cond; incr) { }
sub _process_c_style_for {
  my $self   = shift;
  my $for_struct = shift;
  my $block  = shift;
  my $stmt   = shift;
  my $label  = shift;  # Optional loop label

  # Collect the three statements from the for structure
  my @statements;
  for my $child ($for_struct->children) {
    my $ref = ref($child);
    next if $ref eq 'PPI::Token::Whitespace';
    if ($ref =~ /^PPI::Statement/) {
      push @statements, $child;
    }
  }

  my $init_cl = '';
  my $cond_cl = 't';  # Default to true
  my $incr_cl = '';

  # Process init statement (may be variable declaration or expression)
  if (@statements >= 1) {
    my $init_stmt = $statements[0];
    my @parts = grep {
      ref($_) ne 'PPI::Token::Whitespace' &&
      !(ref($_) eq 'PPI::Token::Structure' && $_->content eq ';')
    } $init_stmt->children;

    if (@parts) {
      # Strip 'my'/'our'/'local' keywords for parsing (declarations handled by _with_declarations)
      if (ref($init_stmt) eq 'PPI::Statement::Variable') {
        @parts = grep { !(ref($_) eq 'PPI::Token::Word' && $_->content =~ /^(my|our|local)$/) } @parts;
      }
      $init_cl = $self->_parse_expression(\@parts, $stmt) // '' if @parts;
    }
  }

  # Process condition
  if (@statements >= 2) {
    my @parts = grep {
      ref($_) ne 'PPI::Token::Whitespace' &&
      !(ref($_) eq 'PPI::Token::Structure' && $_->content eq ';')
    } $statements[1]->children;
    if (@parts) {
      $cond_cl = $self->_parse_expression(\@parts, $stmt) // 't';
      # Perl special case: for(; $k = each/readline/glob COLL ;) terminates on
      # undef, not on a false-but-defined value (each's index 0).  Shared with
      # the while handlers; also sees through the wantarray-leak `(let …)` wrap.
      $cond_cl = $self->_auto_defined_cond($cond_cl);
    }
  }

  # Process increment
  if (@statements >= 3) {
    my @parts = grep {
      ref($_) ne 'PPI::Token::Whitespace' &&
      !(ref($_) eq 'PPI::Token::Structure' && $_->content eq ';')
    } $statements[2]->children;
    $incr_cl = $self->_parse_expression(\@parts, $stmt) // '' if @parts;
  }

  # Build label argument if present
  my $label_arg = $label ? ' :label ' . cl_sym($label) : "";

  # Use common helper - scan init and condition for declarations
  my @decl_sources = grep { defined } @statements[0..1];
  $self->_with_declarations(\@decl_sources, sub {
    $self->_emit("(p-for ($init_cl)");
    $self->_emit("        ($cond_cl)");
    $self->_emit("        ($incr_cl)$label_arg");
    $self->indent_level($self->indent_level + 1);
    if ($block) {
      $self->_with_declarations($block, sub {
        $self->_process_block($block, 1);
      });
    }
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  });

  $self->_emit("");
}


# The wrappers a foreach LIST can arrive in, peeled down to the significant
# tokens of its sole element.  ONE resolver, shared by every pass that reasons
# about the list's shape: the alias rewrite below, the single-scalar wrap
# (_foreach_single_scalar_p) and VarAnnotator's raw-slot veto.  The spelling
# decides the wrapper — the block form hands over the list's PPI::Statement
# children, the statement-MODIFIER form hands over the parens themselves — and
# when the passes disagreed about which wrappers to peel, the modifier spelling
# silently lost its write-through (#263: `$_ = "w" for ($h{k})` wrote nothing
# while the block form wrote through).
sub _foreach_list_unwrap {
  my ($list_parts) = @_;
  my @sig = grep { ref($_) ne 'PPI::Token::Whitespace' } @$list_parts;
  while (@sig == 1
         && (ref($sig[0]) eq 'PPI::Statement'
             || ref($sig[0]) eq 'PPI::Statement::Expression'
             || ref($sig[0]) eq 'PPI::Structure::List'
             || ref($sig[0]) eq 'PPI::Structure::Condition')) {
    @sig = grep { ref($_) ne 'PPI::Token::Whitespace' } $sig[0]->children;
  }
  return @sig;
}

# Apply the aliasing rewrite (if any) to an already-lowered foreach LIST
# string.  The one place v1 turns the AST verdict into emission, so both v1
# lowering sites — block form and statement modifier — go through it.
sub _apply_foreach_alias_rewrite {
  my ($list_cl, $list_parts) = @_;
  my @hd = _foreach_alias_rewrite($list_parts);
  return $list_cl unless @hd;
  # A FAILED ANCHOR IS ALWAYS A COMPILER SELF-INCONSISTENCY, never a benign
  # decline (ruled s371, task #274): _foreach_alias_rewrite predicted this head
  # from the AST, so if the lowered text's outermost call is not that head then
  # either (a) the verdict was right and the swap is being skipped — the write
  # lands on a copy, which IS the #262/#263 silent-wrong — or (b) the verdict
  # was wrong about the lowering, in which case boxing would be wrong too and
  # the two functions disagree about the same tokens.  Both halves are bugs;
  # neither may pass silently.  Same wording as the multi-element site.
  return _apply_alias_head($list_cl, @hd)
    // die "foreach alias: element head $hd[0] not outermost in: $list_cl\n";
}

# The ONE text head-swap for an already-lowered foreach element (the v1 seam's
# counterpart to Parser2::_alias_box_form, which works on the CLForm tree).
# ANCHORED at the outermost call, allowing the `(vector ` wrap that one site
# applies before this runs and the other after.  The head the AST predicted
# must BE the outermost one: if it is not, boxing some INNER call would hand
# p-gethash a box where it expects a container — a silent wrong in place of a
# missed alias — so a failed anchor returns undef and BOTH callers die on it
# (s371: the k=1 and k>1 paths are the same rule, see the sole-element site).
sub _apply_alias_head {
  my ($list_cl, $from, $to) = @_;
  return undef unless $list_cl =~ s/\A(\s*(?:\(vector\s+)?)\(\Q$from\E /$1($to /;
  return $list_cl;
}

# If a foreach list is a single aliasable lvalue, return (FROM-HEAD, TO-HEAD) so
# the caller can rewrite the generated call head to its box-returning form, making
# the loop variable alias the live container (write-through).  Otherwise ().
# Three shapes are recognised (AST-level, per the codegen-style preference —
# inspect the PPI nodes, don't pattern-match the generated CL):
#   - a magic-lvalue builtin call: substr(...) / pos(...) / vec(...)
#     -> p-substr -> p-substr-lvalue-cell, etc.  (Word + argument-List)
#   - a single hash/array ELEMENT of a NAMED container: $h{k} / $a[i]
#     -> p-gethash -> p-gethash-box  /  p-aref -> p-aref-box  (Symbol + Subscript)
#   - the same element THROUGH A REFERENCE: $r->{k} / $r->[i] / $$r{k} / $$r[i]
#     -> p-gethash-deref -> p-gethash-deref-box  /  p-aref-deref -> …-deref-box
#     (the two spellings lower to the same head, so one entry covers both)
# The element shapes are matched as a two- or three-token TERM, which is also
# what guards against multi-element lists like `for (substr(...), $y)`.
# Slices (@a[...], @h{...}) and `values %h` are intentionally NOT handled here —
# they flatten through the shared copy machinery; see docs/foreach-aliasing.md.
# A list of SEVERAL elements is a known gap in both spellings (task #267): one
# head swap cannot rewrite N element forms, and picking them apart needs the
# per-element lowering the seam does not do.
sub _foreach_alias_rewrite {
  my ($list_parts) = @_;
  my @sig = _foreach_list_unwrap($list_parts);
  return () unless @sig;

  # Magic-lvalue builtin call: Word + argument-List.
  if (@sig == 2
      && ref($sig[0]) eq 'PPI::Token::Word'
      && ref($sig[1]) eq 'PPI::Structure::List') {
    my %head = (substr => 'p-substr', pos => 'p-pos', vec => 'p-vec');
    my $h = $head{ $sig[0]->content } or return ();
    return ($h, "$h-lvalue-cell");
  }

  # Otherwise: any SINGLE-SCALAR term that ENDS in a subscript is an element
  # access, whatever its spelling — `$h{k}`, `$r->{k}`, `$$r{k}`, `${$r}{k}`,
  # `$h{a}{b}`, `$r->{a}[0]`, `$a[0][1]`.  The term shape is the sibling
  # predicate's decision (one resolver), so anything it rejects — a slice, a
  # bare `$x`, `\@a` — never reaches here.
  return () unless ref($sig[-1]) eq 'PPI::Structure::Subscript'
                && _foreach_single_scalar_p(\@sig);
  # Which HEAD the outermost access emitted follows from what it reads FROM:
  # a scalar REFERENCE (`$r->{k}` / `$$r{k}` / `${$r}{k}`) lowers to the
  # -deref head, a named container or an intermediate value (any chain of two
  # or more subscripts, whose base is the previous access's VALUE) to the
  # plain one.  Only the LAST bracket picks hash vs array.
  my $nsub = grep { ref($_) eq 'PPI::Structure::Subscript' } @sig;
  my $through_ref =
       (ref($sig[0]) eq 'PPI::Token::Cast' && $sig[0]->content eq '$')
    || (@sig > 1 && ref($sig[1]) eq 'PPI::Token::Operator'
        && $sig[1]->content eq '->');
  my $suffix = ($nsub == 1 && $through_ref) ? '-deref' : '';
  my $sub = $sig[-1]->content;
  return ("p-gethash$suffix", "p-gethash$suffix-box") if $sub =~ /^\{/;
  return ("p-aref$suffix",    "p-aref$suffix-box")    if $sub =~ /^\[/;
  return ();
}

# The foreach LIST split into its DEPTH-0 elements, but ONLY when EVERY one of
# them is a single-scalar operand (_foreach_single_scalar_p).  Returns the list
# of significant-token runs (one arrayref per element), or () when the list does
# not qualify — a mixed list (`for ($x, @a)`), a `$x or $y` expression, a
# call-shaped element, an empty slot from a trailing comma.
#
# WHY the whole list and not just the sole element: the N=1 rule below IS the
# N=k rule (ruled s369).  A list whose every element is a single scalar has a
# STATICALLY known length k — perl-side flattening is impossible — so
# `(p-flatten-args (list …))` and `(vector E1 … Ek)` are extensionally identical
# on this population, and only the vector form can carry BOXES (a box handed to
# p-flatten-args is indistinguishable from an @array box and gets spread: the
# #262/#263 silent-wrong, one level up).  Callers therefore switch the wrapper
# for the whole list, never per element.
#
# The split is the SHARED low-precedence one (#140/#138,
# Pl::PExpr::TokenUtils::lowprec_idx) — never a private comma scan.  It also
# splits `=>`, which is right here: `for ($a => $b)` is a two-element list.  An
# `or`/`and`/`xor` at depth 0 means the parens hold ONE expression, not a list,
# and the run declines (as it does today — the operator is not a `->`).
#
# PAIRED WITH Pl::VarAnnotator::_ev_foreach_alias_list, which walks the same
# list for commas a second time (deliberately TWO walks, ruled s371 §3 — read
# that comment too before touching either).  The invariant that makes two
# walks safe:
#   (a) On every list that QUALIFIES here — only `,`/`=>` at depth 0, both
#       walkers fed the same _foreach_list_unwrap output — the two walks
#       partition the tokens IDENTICALLY, so the hazard shape (this function
#       hands out a vector + boxes while the veto missed a raw `$name` slot)
#       cannot occur.
#   (b) On lists that do NOT qualify the veto is deliberately a SUPERSET: it
#       keeps splitting past `or`/`and`/`xor` and vetoes slots this function
#       rejected, because `for ($x, @a)` still aliases `$x` through
#       p-flatten-args.  A superset is the only safe direction for a veto.
# A THIRD comma walk in this family reopens the shared-primitive question.
sub _foreach_scalar_elements {
  my ($list_parts) = @_;
  my @sig = _foreach_list_unwrap($list_parts);
  return () unless @sig;

  my @elems;
  my $from = 0;
  while (defined(my $lp = Pl::PExpr::TokenUtils::lowprec_idx(\@sig, $from))) {
    return () unless $sig[$lp]->content =~ /^(?:,|=>)$/;
    return () unless Pl::PExpr::TokenUtils::lowprec_split_safe(\@sig, $from, $lp);
    push @elems, [ @sig[$from .. $lp - 1] ];
    $from = $lp + 1;
  }
  push @elems, [ @sig[$from .. $#sig] ];

  for my $e (@elems) {
    return () unless @$e && _foreach_single_scalar_p($e);
  }
  return @elems;
}

# Is this foreach LIST exactly ONE SCALAR-valued operand — `$x`, `$h{k}`,
# `$$r`, `${$r}`, `$obj->{k}`?
#
# perl flattens a foreach list, but a SCALAR contributes exactly one element
# even when it holds an ARRAY or HASH ref.  PCL's box model cannot tell those
# apart at RUNTIME: `\@a` and `[1,2]` are both "a p-box wrapping a vector",
# which is also how an @array box arrives, and there is no ref-kind slot
# (rejected by measurement, ruled s335).  So `%p-flatten-for-list` spread the
# REFERENT: `for ($r) {…}` ran once per element of @$r (probed s361 against
# perl: 3 iterations instead of 1, and the loop var was an element, not the
# ref — a silent wrong for a common idiom).
#
# The EMITTER knows the sigil, so the decision belongs here — and the fix is
# to route the single scalar through the SAME `(vector …)` shape a
# multi-element list already uses (`for ($r, $h)` was always correct).  Only
# unambiguously-one-value shapes qualify: a `$`-cast run, a `$`-sigil Symbol
# or `${…}` block primary, then subscripts / `->` subscripts.  A `->` WORD is
# a method call (may return a list) and a bare `@`/`%` symbol is a real list —
# both decline, keeping today's behaviour.
sub _foreach_single_scalar_p {
  my ($list_parts) = @_;
  # A sole element arrives wrapped: PPI::Statement (block form) or the
  # parens themselves (statement-modifier form, `EXPR for ($r)`).
  my @sig = _foreach_list_unwrap($list_parts);
  return 0 unless @sig;

  my $i = 0;
  my $casts = 0;
  # A LEADING `\` makes the whole thing one reference — `\@a`, `\%h`, `\&f`
  # are single scalars.  (`\(@a)` is perl's DISTRIBUTED form, a list of refs;
  # it declines below because its primary is a List, not a Symbol.)
  my $refcast = 0;
  if ($i < @sig && ref($sig[$i]) eq 'PPI::Token::Cast'
      && $sig[$i]->content eq '\\') {
    $refcast = 1;
    $i++;
  }
  while ($i < @sig && ref($sig[$i]) eq 'PPI::Token::Cast') {
    return 0 unless $sig[$i]->content eq '$';   # @$r / %$r are real lists
    $casts++;
    $i++;
  }
  return 0 unless $i < @sig;
  my $prim = $sig[$i];
  if (ref($prim) eq 'PPI::Token::Symbol') {
    # Without a cast the sigil must be '$'; after a '$' cast the symbol is
    # the ref being dereferenced ($$r), and after `\` any sigil is fine
    # (the reference, not the aggregate, is the value).
    return 0 unless $casts || $refcast || $prim->content =~ /^\$/;
  } elsif (ref($prim) eq 'PPI::Structure::Block') {
    return 0 unless $casts || $refcast;         # only as ${ EXPR } / \{…}
  } else {
    return 0;
  }
  $i++;
  # `\@a[0,1]` / `\@h{a,b}` are SLICES — perl distributes the ref over the
  # elements, so they are lists.  A `\`-cast term takes no postfix here.
  return 0 if $refcast && $i < @sig;

  while ($i < @sig) {
    if (ref($sig[$i]) eq 'PPI::Structure::Subscript') { $i++; next }
    if (ref($sig[$i]) eq 'PPI::Token::Operator'
        && $sig[$i]->content eq '->'
        && $i + 1 < @sig
        && ref($sig[$i + 1]) eq 'PPI::Structure::Subscript') {
      $i += 2;
      next;
    }
    return 0;
  }
  return 1;
}

# Process foreach-style loop: for/foreach VAR (LIST) { }
sub _process_foreach_loop {
  my $self  = shift;
  my $stmt  = shift;
  my $block = shift;
  my $label = shift;  # Optional loop label
  my $continue_block = shift;  # Optional continue block

  my $loop_var;
  my $loop_var_is_my = 0;  # true when declared 'for my $var' (Perl lexical)
  my @list_parts;

  for my $child ($stmt->children) {
    my $ref = ref($child);

    if ($ref eq 'PPI::Token::Word' && $child->content eq 'my') {
      $loop_var_is_my = 1;
    }
    elsif ($ref eq 'PPI::Token::Symbol' && !$loop_var) {
      $loop_var = $child->content;
    }
    elsif ($ref eq 'PPI::Structure::List') {
      # The list expression
      for my $lc ($child->children) {
        next if ref($lc) eq 'PPI::Token::Whitespace';
        if (ref($lc) eq 'PPI::Statement::Expression') {
          push @list_parts, grep {
            ref($_) ne 'PPI::Token::Whitespace'
          } $lc->children;
        }
        else {
          push @list_parts, $lc;
        }
      }
    }
  }

  $loop_var //= '$_';
  # 'for my $var' declares a Perl lexical — it must never be defvar'd as a package global.
  # Record it so _insert_variable_forward_declarations can skip the defvar.
  if ($loop_var_is_my && $loop_var ne '$_') {
    $self->{_lexical_foreach_vars}{$loop_var} = 1;
  }

  # If the loop variable has been renamed for closure capture (e.g. $x → $x__lex__N),
  # the p-foreach form must use the renamed symbol so each iteration's lambda closes
  # over the correct per-iteration binding, not the outer let's initial binding.
  # IMPORTANT: only apply __lex__ renames here. State-variable renames (state__toplevel__
  # or state__subname__) must NOT be applied to loop variables — the loop variable is a
  # fresh lexical binding, not the state variable itself.
  my $renames = $self->environment->state_var_renames // {};
  my $candidate = $renames->{$loop_var};
  my $cl_loop_var = (defined $candidate && $candidate =~ /__lex__\d+$/)
                  ? $candidate
                  : Pl::ExprToCL::qualified_var_to_cl($loop_var, $self->environment);
  # Also track renamed name as lexical foreach var to skip defvar generation
  if ($cl_loop_var ne $loop_var) {
    $self->{_lexical_foreach_vars}{$cl_loop_var} = 1;
  }

  my $list_cl = @list_parts
    ? ($self->_parse_expression(\@list_parts, $stmt, 1) // "(list)")  # 1 = LIST_CTX
    : "(list)";

  # Convert (progn ...) to (vector ...) for foreach list context
  # This handles: foreach (1, 2, 3) which parses as a progn
  # Use \s* to handle potential leading whitespace from indentation
  $list_cl =~ s/^\s*\(progn\b/(vector/;

  # Wrap single scalar values in a vector for foreach
  # This handles: foreach (42) or foreach ($x) where $x is a scalar
  # Skip if it's already a vector, array, hash, range, or function call
  if ($list_cl !~ /^\s*\(/ && $list_cl !~ /^[@%]/) {
    $list_cl = "(vector $list_cl)";
  }

  # foreach-aliasing of a single lvalue: `for (substr($x,1,3)) { $_ = ... }` or
  # `for ($h{k}) { $_ = ... }` must bind $_ to the live lvalue (substr window /
  # hash or array slot) so writing $_ writes through — matching how `for (@a) {
  # $_ = ... }` aliases array elements.  A plain rvalue form ((p-substr ...) /
  # (p-gethash ...)) yields a VALUE in a fresh box, so the write is lost.  Detect
  # the shape at the AST level and rewrite the (one) generated call head to its
  # box-returning form; %p-flatten-for-list keeps the single box and p-foreach
  # binds $_ to it.  The outer call appears before its args, so the first
  # occurrence is the right one; the trailing space avoids matching e.g.
  # (p-substr-ref / (p-gethash-box .
  $list_cl = _apply_foreach_alias_rewrite($list_cl, \@list_parts);

  # Build label argument if present
  my $label_arg = $label ? ' :label ' . cl_sym($label) : "";
  # `:my t` for `foreach my $x` — see the Parser2 foreach branch and
  # %p-cell-loop-var-p: the macro cannot see the declaration, so a package
  # variable of the same name would make it localize a cell where perl
  # declares a lexical (#294).
  $label_arg .= " :my t" if $loop_var_is_my && $cl_loop_var ne '$_';

  $self->_emit("(p-foreach ($cl_loop_var $list_cl)$label_arg");
  $self->indent_level($self->indent_level + 1);
  if ($block) {
    # The foreach loop variable creates a fresh lexical binding that shadows any
    # state_var_rename for the same name (e.g. 'foreach my $x' after 'state $x').
    # Temporarily remove the rename so body expressions use the loop's $x, not the defvar.
    my $saved_loop_var_rename;
    my $cur_renames = $self->environment->state_var_renames // {};
    if (exists $cur_renames->{$loop_var}) {
      $saved_loop_var_rename = delete $cur_renames->{$loop_var};
      $self->environment->state_var_renames({ %$cur_renames });
    }
    # The loop variable is a live CL binding inside the p-foreach body, but it
    # lives in _lexical_foreach_vars, not _let_bound_vars.  Add it to
    # _let_bound_vars for the body so a string eval in the body captures it
    # (e.g. `for my $x (...) { eval '$x' }`).  Save/restore around the body.
    my $saved_let_bound = $self->lex_home->{_let_bound_vars};
    if ($cl_loop_var ne '$_') {
      $self->lex_home->{_let_bound_vars} = { %{$saved_let_bound // {}}, $cl_loop_var => 1 };
    }
    $self->_with_declarations($block, sub {
      $self->_process_block($block, 1);
    });
    $self->lex_home->{_let_bound_vars} = $saved_let_bound;
    if (defined $saved_loop_var_rename) {
      $cur_renames = $self->environment->state_var_renames // {};
      $self->environment->state_var_renames({ %$cur_renames, $loop_var => $saved_loop_var_rename });
    }
  }
  if ($continue_block) {
    $self->_emit(":continue (progn");
    $self->indent_level($self->indent_level + 1);
    $self->_process_block($continue_block, 1);
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  }
  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");
  $self->_emit("");
}


# Process subroutine declaration
# The HEAD of a named-sub statement: its name (PPI may split "main::::foo"
# into "main::" + "::foo" — concatenated), the prototype text or signature
# text, whether the latter is 5.20+ signature SYNTAX, and the body block.
# ONE copy for the two readers: _process_sub_statement (the definition) and
# the prototype-collection walk (task #391 — a facts walk over a use'd
# module reads exactly this and nothing of the body).
sub _sub_head {
  my ($self, $stmt) = @_;
  my ($name, $prototype, $is_signature_syntax, $block) = ('', '', 0, undef);
  for my $child ($stmt->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Token::Word' && $child->content ne 'sub'
        && $child->content ne 'my' && $child->content ne 'our'
        && $child->content ne 'state') {
      $name .= $child->content unless $block;
    }
    elsif ($ref eq 'PPI::Token::Prototype') {
      $prototype = $child->content;
    }
    elsif ($ref eq 'PPI::Structure::Signature') {
      # Perl 5.20+ signature (when 'use feature "signatures"' is used)
      $prototype = $child->content;
      $is_signature_syntax = 1;
    }
    elsif ($ref eq 'PPI::Structure::Block') {
      $block = $child;
    }
  }
  return ($name, $prototype, $is_signature_syntax, $block);
}

# The sub's prototype RECORD (the sig_info every caller reads through
# get_prototype), from its head.  Default: -1 = "unknown/list" — the sub
# takes any number of args; only an explicit prototype/signature narrows it.
# Signature syntax is ALWAYS parsed as a signature, never as an old-style
# prototype — even all-anonymous forms like ($) or ($, $) that would
# otherwise look like one.  Empty () => min 0.
sub _sub_sig_info {
  my ($self, $stmt, $prototype, $is_signature_syntax) = @_;
  my $sig_info = { params => [], min_params => -1, is_proto => 0 };
  if ($is_signature_syntax) {
    my $inner = $prototype;
    $inner =~ s/^\s*\(\s*//;
    $inner =~ s/\s*\)\s*$//;
    $sig_info = $self->_parse_signature($inner, $stmt);
  } elsif ($prototype) {
    $sig_info = $self->parse_prototype_or_signature($prototype, $stmt);
  }
  return $sig_info;
}

# Register a named sub's prototype record and its declaration site.  A
# :prototype-attribute proto (from_attr, registered by
# _extract_prototype_attributes) is applied at compile time in perl — keep it
# when this definition carries no inline prototype/signature of its own.
# The SITE goes with the declaration: a bareword call site decides
# call-vs-string by asking whether the declaration is above it (task #266),
# and an entry with no site is read as the old whole-file answer — so this
# seam must supply one too, or a sub lowered through here would silently
# neuter the position test for its own name.
sub _register_sub_prototype {
  my ($self, $stmt, $name, $sig_info, $prototype, $is_signature_syntax) = @_;
  return unless $name;
  my $prev = $self->environment->get_prototype($name);
  my $pkg  = $self->environment->current_package();
  if (!($prev && $prev->{from_attr} && !$prototype && !$is_signature_syntax)) {
    # The declaring package goes with the prototype as it goes with the
    # declaration below (task #421).
    $self->environment->add_prototype($name, $sig_info, $pkg);
  }
  $self->environment->add_declared_sub($name, $pkg,
                                       Pl::PExpr::TokenUtils::decl_site($stmt));
}

sub _process_sub_statement {
  my $self = shift;
  my $stmt = shift;

  my ($name, $prototype, $is_signature_syntax, $block) = $self->_sub_head($stmt);

  # At file scope, route named sub definitions to the DEFINITIONS bucket, in
  # SOURCE ORDER alongside use/BEGIN/require.  This reproduces Perl's compile-time
  # stream: a `use`/`BEGIN` sees exactly the subs written BEFORE it and none
  # written after.  (The old policy routed subs to the `declarations` bucket,
  # which assembles before `definitions`, so every sub ran before every use/BEGIN
  # — breaking any module that introspects the package's subs at use-time, e.g.
  # Moo::Role's make_role.  See docs/declaration-ordering-fix-plan.md.)
  # Forward references (a runtime `foo()` or `\&foo` written before `sub foo`)
  # still resolve: the compile-time stream is assembled before the runtime stream
  # (C1), and the `p-declare-sub` stub (unshifted into declarations below) covers
  # a `\&foo` taken inside an earlier BEGIN.
  # Inside subs (in_subroutine > 0), nested NAMED subs are hoisted to the
  # definitions bucket (at indent 0) so they are available before the outer
  # sub runs.  Their state variables use defvar (global special) instead of
  # let (lexical) so the inner sub can reference them from outside the let.
  #
  # Exception: when inside a let block (_let_bound_vars non-empty), the sub
  # must be emitted in-place so its lambda closes over the let-bound lexical
  # variables (e.g. $x__lex__N renamed for closure capture).  p-declare-sub
  # still goes to declarations for forward-reference support.
  my $is_nested_named = $name && $self->environment->in_subroutine > 0
                        && !%{$self->lex_home->{_let_bound_vars} // {}};
  my $old_bucket = $self->_cur_bucket;
  my $old_indent = $self->indent_level;
  if ($self->environment->in_subroutine == 0 && !%{$self->lex_home->{_let_bound_vars} // {}}) {
    $self->_cur_bucket('definitions');
  } elsif ($is_nested_named) {
    # A nested NAMED sub must be hoisted OUT of the enclosing sub's form so it is
    # installed independently (callable before the outer runs).  The hoist works
    # by emitting it into a DIFFERENT bucket than the one currently open for the
    # enclosing top-level sub — otherwise its lines interleave inside the outer's
    # parens.  Top-level subs now live in `definitions`, so nested subs go to
    # `declarations` (assembled earlier, separate array → clean separation).
    $self->_cur_bucket('declarations');
    $self->indent_level(0);
  }

  # Emit Perl code as comment
  my $perl_code = $stmt->content;
  $perl_code =~ s/\{.*\}$/{ ... }/s;  # Abbreviate body
  $perl_code =~ s/\n/ /g;
  $self->_emit(";; $perl_code");

  # Parse prototype/signature (the shared head reader; see _sub_sig_info).
  my $sig_info = $self->_sub_sig_info($stmt, $prototype, $is_signature_syntax);

  # A real Perl signature (feature "signatures"), not an old-style prototype.
  # When set, args are flattened into @_, arity is checked with Perl's exact
  # error message, and the named params are bound from @_ (see below).
  my $is_sig = $is_signature_syntax && !$sig_info->{is_proto};

  # Partition signature params into required / optional / slurpy and derive the
  # arity bounds.  Used both for the arity check and the @_-based binding.
  my (@sig_req, @sig_opt, $sig_slurpy);
  if ($is_sig) {
    for my $param (@{$sig_info->{params}}) {
      my $pname = $param->{name};
      if ($pname =~ /^[\@\%]/)          { $sig_slurpy = $pname; }
      elsif (defined $param->{default_cl}) { push @sig_opt, $param; }
      else                              { push @sig_req, $param; }
    }
  }
  my $sig_min  = scalar @sig_req;
  my $sig_max  = $sig_slurpy ? 'nil' : ($sig_min + scalar @sig_opt);
  my $sig_flex = (@sig_opt || $sig_slurpy) ? 't' : 'nil';
  # A slurpy %hash must receive an EVEN number of leftover args (perl dies
  # "Odd name/value argument for subroutine ..."): pass the index where the
  # hash's args start so p-check-arity can verify evenness.
  my $sig_hash_start = ($sig_slurpy && $sig_slurpy =~ /^\%/)
                     ? ($sig_min + scalar @sig_opt) : 'nil';

  # Store in environment for later use by PExpr (+ the declaration site).
  $self->_register_sub_prototype($stmt, $name, $sig_info, $prototype, $is_signature_syntax);

  # Build parameter list for defun
  my @param_names;
  my @optional_params;
  my $in_optional = 0;

  for my $param (@{$sig_info->{params}}) {
    my $pname = $param->{name};

    # For old-style prototypes, skip ALL params - body uses @_ directly
    # (We still store proto_type for auto-boxing at call sites)
    next if $sig_info->{is_proto};

    if (defined $param->{default_cl}) {
      # Parameter with default goes to &optional
      push @optional_params, { name => $pname, default => $param->{default_cl} };
      $in_optional = 1;
    }
    elsif ($pname =~ /^[\@\%]/) {
      # Slurpy parameter - use &rest
      push @optional_params, { name => $pname, rest => 1 };
    }
    elsif ($in_optional) {
      # After seeing optional, all are optional
      push @optional_params, { name => $pname, default => 'nil' };
    }
    else {
      # Required parameter
      push @param_names, $pname;
    }
  }

  # Build the parameter string
  # CL order: required &optional &rest &key
  my $params_cl = join(' ', @param_names);

  if (@optional_params) {
    my @opt_strs;
    my $rest_param;

    for my $opt (@optional_params) {
      if ($opt->{rest}) {
        $rest_param = $opt->{name};
      }
      else {
        push @opt_strs, "($opt->{name} $opt->{default})";
      }
    }

    # &optional comes before &rest
    if (@opt_strs) {
      $params_cl .= ' &optional ' . join(' ', @opt_strs);
    }

    # &rest before &key
    if ($rest_param) {
      $params_cl .= ' &rest ' . $rest_param;
    }
  }

  # If no explicit parameters, add &rest %_args to capture arguments
  # Then convert to @_ vector so shift/pop work correctly
  # wantarray is handled via *wantarray* dynamic variable (set by caller)
  my $needs_args_conversion = 0;
  if (!@param_names && !@optional_params) {
    $params_cl = '&rest %_args';
    $needs_args_conversion = 1;
  }

  # Real signatures: discard the CL-lambda param list built above and instead
  # capture every arg via &rest, then flatten + arity-check + bind from @_.
  # This makes foo(@arr) flatten correctly and gives Perl's exact arity error.
  if ($is_sig) {
    $params_cl = '&rest %_args';
    $needs_args_conversion = 0;   # we emit our own @_ binding below
  }

  # Perl package-qualified name for the arity error message ("main::foo").
  my $sig_qname;
  if ($is_sig) {
    my $pkg = $self->environment->current_package();
    my $bn  = $name ne '' ? $name : '__ANON__';
    $sig_qname = ($bn =~ /::/) ? $bn
               : ($pkg eq 'main' ? "main::$bn" : "$pkg\::$bn");
  }

  # Find state declarations in the block (they need special handling)
  my @state_vars;
  if ($block) {
    my @all_decls = @{$self->_find_all_declarations($block)};
    my %seen;
    @state_vars = grep { !$seen{$_}++ }
                  map { $_->{var} }
                  grep { $_->{type} eq 'state' } @all_decls;
  }

  # If we have state vars, wrap defun in a let for persistent storage.
  # Use unique CL names ($state--subname--varname--N) to avoid colliding with
  # any defvar declarations at file scope, which would make the symbol SPECIAL
  # and turn the lexical let into a dynamic binding that evaporates after load.
  #
  # Exception: when the block contains inner NAMED subs (which will be hoisted
  # to the definitions bucket), use defvar (global special) for state vars so
  # the hoisted inner subs can reference them without being inside the let scope.
  my %state_renames;
  my $use_defvar_state = 0;
  if (@state_vars) {
    my $sub_slug = $name ? $name : 'anon';
    $sub_slug =~ s/[^a-zA-Z0-9]/-/g;
    $use_defvar_state = $block && $self->_block_has_inner_named_subs($block);
    my @bindings;
    for my $var (@state_vars) {
      # Strip sigil for the slug part, keep sigil for the CL name
      my ($sigil, $bare) = ($var =~ /^([\$\@\%])(.+)$/);
      $bare //= $var; $sigil //= '$';
      my $bare_slug = $bare; $bare_slug =~ s/[^a-zA-Z0-9]/-/g;
      my $unique = sprintf("%sstate__%s__%s__%d",
                           $sigil, $sub_slug, $bare_slug, ++$state_var_counter);
      $state_renames{$var} = $unique;
      # Initialize state var to an appropriate empty container by sigil.
      # $ → box(nil) so p-pre++/p-post++ work even before init guard fires.
      # @ → empty adjustable vector; % → empty hash table.
      my $init_val = $sigil eq '$' ? '(make-p-box nil)'
                   : $sigil eq '@' ? '(make-array 0 :adjustable t :fill-pointer 0)'
                   :                 '(make-hash-table :test (quote equal))';
      if ($use_defvar_state) {
        # Emit as global defvar so inner named subs can access outside the let.
        # Save/restore bucket+indent to emit at top level in declarations.
        my $saved_b = $self->_cur_bucket; my $saved_i = $self->indent_level;
        $self->_cur_bucket('declarations'); $self->indent_level(0);
        $self->_emit(global_decl_form("$unique", "$init_val"));
        $self->_emit(global_decl_form("${unique}__init", "nil"));
        $self->_cur_bucket($saved_b); $self->indent_level($saved_i);
      } else {
        push @bindings, "($unique $init_val)";
        push @bindings, "(${unique}__init nil)";
      }
    }
    if (!$use_defvar_state) {
      $self->_emit("(let (" . join(" ", @bindings) . ")");
      $self->indent_level($self->indent_level + 1);
    }
  }

  # User-defined subs get p- prefix to avoid conflicts with CL built-ins
  # Use p-sub macro to wrap in eval-when for BEGIN block visibility
  # Wrap body in (block nil ...) so p-return works
  # Handle qualified names: A::foo -> A::p-foo (not p-A::foo)

  # When inside a bare block (block_depth > 0), a simple 'package Foo;'
  # changes the environment's package stack but NOT the CL section.
  # The sub must carry a fully-qualified name so SBCL interns it in the
  # correct CL package at read time, regardless of *package*.
  my $effective_name = $name;
  if ($name && $name !~ /::/ && $self->_block_depth > 0) {
    my $pkg = $self->environment->current_package();
    $effective_name = "$pkg\::$name" if $pkg ne 'main';
  }
  my $cl_sub_name = $self->_qualified_sub_to_cl($effective_name);

  # All named subs get a p-declare-sub stub in the declarations bucket.
  # This ensures forward references (e.g. \&foo in a BEGIN block before
  # 'sub foo {}' in source) always resolve, regardless of source order.
  # The declarations bucket is assembled before definitions in the output.
  # p-declare-sub is idempotent: it only creates the stub if the real
  # definition hasn't loaded yet.
  if ($name) {
    unshift @{$self->_sections->[$self->_cur_section]{declarations}},
            "(p-declare-sub $cl_sub_name)";
  }

  # Forward declaration: sub name; or sub name ($); or sub name : attrs;
  # The p-declare-sub stub in declarations is sufficient; nothing more needed.
  unless ($block) {
    $self->_cur_bucket($old_bucket);
    return;
  }

  $self->_emit("(p-sub $cl_sub_name ($params_cl)");
  $self->indent_level($self->indent_level + 1);

  # Number of wrapper forms ((let ...)/(let* ...)) opened for a signature sub,
  # so the close section emits exactly that many ')'.
  my $sig_wrap_closes = 0;

  if ($is_sig) {
    # (p-args-body ...) — flatten %_args into @_ (foo(@arr) spreads); see the
    # p-args-body macro in cl/pcl-runtime.lisp.
    $self->_emit("(p-args-body");
    $self->indent_level($self->indent_level + 1);
    $sig_wrap_closes++;
    # Arity check BEFORE any binding (a too-few call must not index past @_).
    $self->_emit("(p-check-arity \"$sig_qname\" (length \@_) $sig_min $sig_max $sig_flex $sig_hash_start)");
    # Bind the named params positionally from @_ via a sequential let*
    # (so an optional default can reference an earlier param, e.g. $r = f($c)).
    #
    # Each scalar param is copied into a FRESH box via p-copy-scalar-arg: Perl
    # params are copies of @_ (`my ($x)=@_`), so the param must be its own
    # mutable box.  The param names are also registered in _let_bound_vars (see
    # below) so the body's `$x = ...` lowers to p-my-= (box-set) instead of
    # p-scalar-= — the latter's (proclaim special) would globalise the param and
    # make the write a silent no-op.  See docs/variable-declarations-spec.md §4.1.
    my @binds;
    my @sig_param_names;
    my @local_wraps;          # `local $G = …` defaults: localise $G for the body
    my $idx = 0;
    for my $p (@sig_req) {
      push @binds, "($p->{name} (p-copy-scalar-arg (aref \@_ $idx)))";
      push @sig_param_names, $p->{name};
      $idx++;
    }
    for my $p (@sig_opt) {
      # `=` applies the default only when the arg is absent; `//=` also when the
      # supplied arg is undef; `||=` also when it is false.  The `and` short-
      # circuits so an absent arg never indexes past @_.
      my $op    = $p->{default_op} // '=';
      my $avail = "(> (length \@_) $idx)";
      my $cond  = $op eq '//=' ? "(and $avail (%pcl-definedp (aref \@_ $idx)))"
                : $op eq '||=' ? "(and $avail (p-true-p (aref \@_ $idx)))"
                :                $avail;
      push @binds,
        "($p->{name} (p-copy-scalar-arg (if $cond (aref \@_ $idx) $p->{default_cl})))";
      push @sig_param_names, $p->{name};
      push @local_wraps, { var => $p->{local_var}, name => $p->{name}, idx => $idx }
        if $p->{local_var};
      $idx++;
    }
    if ($sig_slurpy) {
      my $fn = $sig_slurpy =~ /^\@/ ? 'p-sig-rest-array' : 'p-sig-rest-hash';
      push @binds, "($sig_slurpy ($fn \@_ $idx))";
    }
    if (@binds) {
      $self->_emit("(let* (" . join(' ', @binds) . ")");
      $self->indent_level($self->indent_level + 1);
      $sig_wrap_closes++;
    }
    # `local $G = RHS` default: localise $G to the param's value (= RHS) when the
    # default was taken, restored on sub exit via CL dynamic unwinding.  When an
    # arg was supplied the default did not run, so $G is rebound to itself (a
    # no-op rebinding that restores to the same box).  See spec §4.2.
    for my $lw (@local_wraps) {
      $self->_emit("(let (($lw->{var} (if (> (length \@_) $lw->{idx}) $lw->{var}"
                 . " (p-box-for-local (unbox $lw->{name})))))");
      $self->indent_level($self->indent_level + 1);
      $sig_wrap_closes++;
    }
    # Scalar params are lexical 'my'-style boxes: record them so _emit rewrites
    # their (p-scalar-= ...) to (p-my-= ...) for the duration of the body.
    $self->{_sig_param_names} = \@sig_param_names;
  }
  # If using %_args, convert to @_ vector (p-args-body macro binds @_)
  elsif ($needs_args_conversion) {
    $self->_emit("(p-args-body");
    $self->indent_level($self->indent_level + 1);
  }

  $self->_emit("(block nil");
  $self->indent_level($self->indent_level + 1);

  # Sub-body void regime: bind *wantarray* to :void ONCE around the whole body
  # instead of wrapping every non-tail statement.  Nested if/while/for blocks
  # inherit this dynamic binding; the tail (implicit return) restores the
  # caller's context via *pcl-caller-wantarray* (see the wrap site in
  # _process_expression_statement).  wa_void_active tells statement emitters the
  # ambient is already :void so they can trust it.
  $self->_emit("(let ((*wantarray* :void))");
  $self->indent_level($self->indent_level + 1);

  # Track that we're inside a subroutine (for shift/pop @_ vs @ARGV)
  $self->environment->in_subroutine($self->environment->in_subroutine + 1);

  if ($block) {
    local $self->environment->{wa_void_active} = 1;
    # Wrap sub body with let for local variable declarations.
    # Pass state_vars so _with_declarations knows to skip them.
    # Also set rename map in environment so ExprToCL remaps $x -> $state--sub--x--N.
    local $self->{_current_state_vars} = { map { $_ => 1 } @state_vars };
    my $saved_renames = $self->environment->state_var_renames;
    $self->environment->state_var_renames(\%state_renames) if %state_renames;
    # Register scalar signature params so the body's `$param = ...` is rewritten
    # to p-my-= (box-set) by _emit, not p-scalar-=.  Kept in a SEPARATE set from
    # _let_bound_vars: the latter gates nested-named-sub hoisting (a sub inside a
    # `let` body stays inline to capture the lexicals), and params must NOT flip
    # that gate — an independently-called inner named sub must still hoist.
    local $self->{_sig_param_lexicals} = {
      %{$self->{_sig_param_lexicals} // {}},
      map { $_ => 1 } @{$self->{_sig_param_names} // []},
    };
    delete $self->{_sig_param_names};
    # Save package stack: inline 'package NAME;' inside a sub body must not leak
    my $saved_pkg_stack = [@{$self->environment->package_stack}];
    $self->_with_declarations($block, sub {
      $self->_process_block($block);
    }, 1);  # is_sub_body=1: enable two-phase scoped block
    # Restore package stack in case of inline package switches inside the sub
    $self->environment->package_stack($saved_pkg_stack);
    $self->environment->state_var_renames($saved_renames);
  }
  else {
    $self->_emit("nil");
  }

  # Leaving subroutine
  $self->environment->in_subroutine($self->environment->in_subroutine - 1);

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");  # close (let ((*wantarray* :void)))

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");  # close block

  if ($is_sig) {
    for (1 .. $sig_wrap_closes) {
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")");  # close signature let / let*
    }
  }
  elsif ($needs_args_conversion) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");  # close let
  }

  $self->indent_level($self->indent_level - 1);
  $self->_emit(")");  # close defun

  # Close state vars let (only when using lexical let, not defvar)
  if (@state_vars && !$use_defvar_state) {
    $self->indent_level($self->indent_level - 1);
    $self->_emit(")");
  }

  $self->_emit("");

  # Restore previous bucket and indent (indent was saved for hoisted inner subs)
  $self->_cur_bucket($old_bucket);
  $self->indent_level($old_indent) if $is_nested_named;
}


# Process package declaration
# `package NAME VERSION [{...}]` sets $NAME::VERSION to VERSION (perlfunc).
# Emit the qualified $VERSION box (defvar, idempotent — the cross-package dedup
# scan keys on the same `(defvar PKG::$var` shape) plus the assignment.  Numeric
# version literals (`11`, `1.23`) are emitted as CL numbers; anything else
# (v-strings) falls back to a string, which stringifies the same way for the
# common comparisons.
sub _emit_package_version {
  my ($self, $pkg_name, $version) = @_;
  return unless defined $version && $version ne '';
  # PPI's ->version returns the BLOCK content (`{ ... }`) for a block-form
  # `package Foo { ... }` with no version, so only accept genuine version
  # literals: an optional leading `v`, digits, dots and underscores.
  return unless $version =~ /^v?\d+(?:[._]\d+)*$/;
  (my $prefix = $self->_cl_pkg_designator($pkg_name)) =~ s/^://;
  my $sym = "$prefix\::\$VERSION";
  my $ver_cl = ($version =~ /^\d+(?:\.\d+)?$/) ? $version : "\"$version\"";
  # NOTE: Perl sets $NAME::VERSION at COMPILE time (visible even on a source line
  # BEFORE the `package` statement).  PCL emits it in source order, so the
  # value is correct from the `package` statement onward — which covers the
  # normal "read $VERSION after the module is loaded" case but not the rare
  # read-before-declaration-in-the-same-unit pattern (perl-tests package_block.t
  # test 2).  Matching that needs cross-section BEGIN-phase emission.
  $self->_emit("(eval-when (:compile-toplevel :load-toplevel :execute)");
  $self->_emit("  " . global_decl_form("$sym", "(make-p-box nil)") . ")");
  $self->_emit("(p-scalar-= $sym $ver_cl)");
}

sub _process_package_statement {
  my $self = shift;
  my $stmt = shift;

  my $pkg_name = $stmt->namespace // 'main';
  my $pkg_version = eval { $stmt->version };

  # Register package as a known class/package for method call resolution
  $self->environment->add_package($pkg_name);

  # Check for block form: package Foo { ... }
  my $block;
  for my $child ($stmt->schildren) {
    if (ref($child) eq 'PPI::Structure::Block') {
      $block = $child;
      last;
    }
  }

  if ($block) {
    # Block form: push package, process block, pop
    if ($self->environment->in_subroutine > 0) {
      # Inside a function body: emit package setup INLINE (no new section, no
      # in-package).  Using (in-package ...) here would change the CL reader's
      # package context for the rest of the file, corrupting all subsequent code.
      # Instead we:
      #   1. Emit (p-defpackage) and (defclass) inline.
      #   2. Temporarily increment _block_depth so _process_sub_statement emits
      #      fully-qualified names like |Point|::p-new (not just p-new).
      #      p-sub already handles qualified names via symbol-package.
      #   3. At runtime (when the enclosing function is called) the eval-when
      #      :execute semantics kick in and the package + methods are created in
      #      the right order within the same call.
      my $saved_pkg_stack = [@{$self->environment->package_stack}];

      my $cl_pkg = $self->_cl_pkg_designator($pkg_name);
      my $cl_class = $self->_pkg_to_clos_class($pkg_name);

      $self->_emit(";;; inline package $pkg_name");
      $self->_emit("(p-defpackage $cl_pkg)");
      $self->_emit(";; CLOS class for MRO");
      $self->_emit("(defclass $cl_class () ())");
      $self->_emit("(p-set-current-package $cl_pkg \"$pkg_name\")");
      $self->_emit("");

      $self->environment->push_package($pkg_name);
      $self->_emit_package_version($pkg_name, $pkg_version);
      # Increment _block_depth so sub names become fully qualified (e.g. |Point|::p-new)
      $self->_block_depth($self->_block_depth + 1);

      for my $child ($block->schildren) {
        $self->_process_element($child);
      }

      $self->_block_depth($self->_block_depth - 1);
      $self->environment->pop_package();
      $self->environment->package_stack($saved_pkg_stack);
    }
    else {
      # Top-level block form: push package, process block, pop
      $self->_emit_package_preamble($pkg_name);
      $self->environment->push_package($pkg_name);
      $self->_emit_package_version($pkg_name, $pkg_version);

      # Process the block contents
      for my $child ($block->schildren) {
        $self->_process_element($child);
      }

      $self->environment->pop_package();
      # Switch back to previous package: open a new section with in-package in preamble
      my $prev_pkg = $self->environment->current_package();
      my $cl_prev  = $self->_cl_pkg_designator($prev_pkg);
      $self->_open_section($prev_pkg);
      $self->_cur_bucket('runtime');
      $self->_with_bucket('preamble', sub {
        $self->_emit("(in-package $cl_prev)");
        $self->_emit(";;; end package $pkg_name");
        $self->_emit("");
      });
      # Restore the runtime current-package to the enclosing package.
      $self->_emit("(p-set-current-package $cl_prev \"$prev_pkg\")");
    }
  }
  else {
    # Simple form: package Foo;
    # This changes the current package until another package declaration
    if ($self->environment->in_subroutine > 0) {
      # Inside a sub body: just update the environment.
      # Do NOT emit (in-package) or open a new section — that would break the
      # SBCL reader's package context and corrupt the section/bucket structure.
      # The environment's current_package is used by codegen (e.g. 1-arg bless).
      $self->environment->push_package($pkg_name);
      # Reflect the package switch at runtime too (caller()/__PACKAGE__ case).
      # The setf is restored on sub exit via p-sub's dynamic binding.
      $self->_emit("(p-set-current-package " . $self->_cl_pkg_designator($pkg_name) .
                   " \"$pkg_name\")");
      $self->_emit_package_version($pkg_name, $pkg_version);
    } else {
      $self->_emit_package_preamble($pkg_name);
      $self->environment->push_package($pkg_name);
      $self->_emit_package_version($pkg_name, $pkg_version);
      # Note: no pop - package remains active until next package declaration
    }
  }
}


# Emit CL package preamble (defpackage + in-package)
# Uses pipe-quoting for package names with :: or that conflict with CL symbols
# Also emits a CLOS class for MRO tracking (inheritance)
# The CL package designator codegen uses for a Perl package name.  Names with
# :: (or that collide with CL symbols) are pipe-quoted so the reader preserves
# case; plain single-segment names are emitted bare and the reader upcases them.
# Single source of truth — used wherever a (p-defpackage ...) / in-package form
# or a runtime package reference is emitted.
sub _cl_pkg_designator {
  my ($self, $pkg_name) = @_;
  # Only MULTI-segment names need pipe-quoting (to preserve the '::' and case).
  # Single-segment names are upcased by the reader (:Class -> CLASS), which is
  # exactly what the runtime's perl-pkg-to-cl-pkg-name does and what bareword
  # call qualifiers (Class::pl-foo) resolve to — so they MUST stay bare to agree.
  # (The old `class/error/method/function` special-case existed only because
  # `(defclass class ...)` collided with CL:CLASS; CLOS class names are now
  # plc-prefixed, so escaping the *package* name here is redundant and actually
  # caused a :|Class| vs CLASS mismatch.)
  # A name carrying a NON-ASCII character is quoted too (#418): a bare token's
  # characters go through the reader's NFKC normalisation, so `:ＦＯＯ` names
  # the package "foo" that no runtime string ever spells.  Pl::CLForm::cl_pkg
  # is the one place that rule lives.
  return ':' . cl_pkg($pkg_name);
}

# The package-qualified CL symbol for a package's @ISA array, e.g.
# Dog::@ISA for single-segment, |Foo::Bar|::@ISA for multi-segment.  Built from
# the package designator so it reads into the SAME package the runtime resolves
# via perl-pkg-to-cl-pkg-name (and that %pcl-isa-ancestry searches).
sub _qualified_isa_symbol {
  my ($self, $pkg_name) = @_;
  (my $prefix = $self->_cl_pkg_designator($pkg_name)) =~ s/^://;
  return "${prefix}::\@ISA";
}

# The package-qualified CLOS class symbol for a package, e.g. Foo::plc-foo (or
# |Foo::Bar|::plc-foo--bar).  Used INSIDE a runtime block, where the whole
# package is one top-level (let ...) form: the inner (in-package :Foo) does not
# take effect at READ time, so a bare `(defclass plc-foo ...)` would intern
# plc-foo in the read-time package instead of :Foo.  A sibling class that names
# this one as a superclass (`(defclass plc-bar (Foo::plc-foo) ())`) DOES use the
# qualified symbol, so without qualifying the definition the two diverge and the
# referenced class is left FORWARD-REFERENCED (finalize-inheritance crash).
sub _qualified_clos_class {
  my ($self, $pkg_name) = @_;
  (my $prefix = $self->_cl_pkg_designator($pkg_name)) =~ s/^://;
  return "${prefix}::" . $self->_pkg_to_clos_class($pkg_name);
}

sub _emit_package_preamble {
  my $self     = shift;
  my $pkg_name = shift;

  my $cl_pkg = $self->_cl_pkg_designator($pkg_name);

  my $cl_class = $self->_pkg_to_clos_class($pkg_name);

  if ($self->_block_depth > 0) {
    # Inside a runtime block: emit package setup inline to the current bucket.
    # Opening a new section here would place its preamble/declarations outside
    # the block in the linear assembly, causing scope and symbol-table confusion.
    $self->_emit(";;; package $pkg_name");
    $self->_emit("(p-defpackage $cl_pkg)");
    $self->_emit("(in-package $cl_pkg)");
    $self->_emit(";; CLOS class for MRO");
    # Qualify the class name: the inline (in-package) above has not taken effect
    # at READ time (the whole block is one top-level form), so a bare class name
    # would intern in the wrong package — see _qualified_clos_class.
    $self->_emit("(defclass @{[ $self->_qualified_clos_class($pkg_name) ]} () ())");
    # Declare $a/$b as special in this package using fully-qualified names in the
    # top-level declarations bucket.  Using pkg::$a at top level (where the reader's
    # *package* is whatever the enclosing section uses) ensures SBCL sees these as
    # globally special before compiling any lambdas that reference them.
    # The inline `defvar $a` would declare MAIN::$A (wrong package), so we skip it.
    # Use same quoting as $cl_pkg: strip leading ':' to get the CL symbol prefix.
    # E.g. ':|Class|' → '|Class|::$a' so SBCL reads it as Class::$a not CLASS::$A.
    (my $cl_pkg_sym = $cl_pkg) =~ s/^://;
    my $pkg_a = $cl_pkg_sym . '::$a';
    my $pkg_b = $cl_pkg_sym . '::$b';
    $self->_with_bucket('declarations', sub {
      $self->_emit(global_decl_form("$pkg_a", "(make-p-box nil)"));
      $self->_emit(global_decl_form("$pkg_b", "(make-p-box nil)"));
      $self->_emit("");
    });
    # Record original-case name + make current at runtime (caller()/__PACKAGE__).
    $self->_emit("(p-set-current-package $cl_pkg \"$pkg_name\")");
    $self->_emit("");
    return;
  }

  # Open a new section for this package; preamble goes in its preamble bucket
  $self->_open_section($pkg_name);
  $self->_cur_bucket('runtime');  # subsequent code defaults to runtime

  $self->_with_bucket('preamble', sub {
    $self->_emit(";;; package $pkg_name");
    $self->_emit("(p-defpackage $cl_pkg)");
    $self->_emit("(in-package $cl_pkg)");
    $self->_emit(";; CLOS class for MRO");
    $self->_emit("(defclass $cl_class () ())");
    # Register the original-case name eagerly (preamble is hoisted before any
    # runtime code, so before this package's `use` statements run).  Needed so
    # caller()/__PACKAGE__ inside an imported module's import() resolve this
    # use-site package to its original case rather than the upcased CL name.
    $self->_emit("(p-register-pkg-name $cl_pkg \"$pkg_name\")");
    $self->_emit("");
  });
  # Declare $a/$b as special in this package so sort comparator lambdas
  # (lambda ($a $b) ...) create dynamic bindings visible to named comparator subs.
  $self->_with_bucket('declarations', sub {
    $self->_emit(global_decl_form("\$a", "(make-p-box nil)"));
    $self->_emit(global_decl_form("\$b", "(make-p-box nil)"));
    $self->_emit("");
  });
  # Record original-case name + make current at runtime (caller()/__PACKAGE__).
  # Emitted into the section's runtime bucket so it runs in execution order
  # (cur_bucket was set to 'runtime' above), not hoisted with the preamble.
  $self->_emit("(p-set-current-package $cl_pkg \"$pkg_name\")");
}

# Convert Perl package name to CLOS class name
# Foo::Bar -> foo-bar
# Pipe-quote names that might conflict with CL symbols (e.g., class, method)
sub _pkg_to_clos_class {
  my ($self, $pkg) = @_;
  my $class = lc($pkg);
  $class =~ s/::/-/g;
  # Prefix with plc- so the CLOS class symbol can NEVER collide with a
  # COMMON-LISP / SBCL symbol once the reader upcases it.  A Perl
  # `package If` / `Second` / `Symbol` / `List` / `Car` ... would otherwise
  # become CL:IF / CL:SECOND / CL:SYMBOL ... — symbols in the *locked*
  # COMMON-LISP package, so the emitted `(defclass NAME ...)` dies with a
  # package-lock violation.  The plc- prefix (PL Class) extends the existing
  # naming discipline: builtins are `p-`, user subs `pl-`, classes `plc-`.
  # This subsumes the old ad-hoc pipe-escape list (class/method/error/...).
  # MUST stay in lock-step with `perl-pkg-to-clos-class` in cl/pcl-runtime.lisp.
  # cl_sym quotes the token when the package name carried a non-ASCII
  # character (#418) — the runtime interns this name through
  # %pcl-invert-case, which is likewise the identity on such a name, so the
  # two sides land on the same class symbol.
  return cl_sym("plc-$class");
}


# PCL runtime symbols that might conflict with user sub names
my %PCL_SYMBOLS = map { $_ => 1 } qw(
  p-x p-y p-print p-say p-length p-push p-pop p-shift p-unshift
  p-keys p-values p-exists p-delete p-sort p-reverse p-map p-grep
  p-join p-split p-ref p-bless p-die p-warn p-open p-close
  p-read p-write p-int p-abs p-substr p-index p-lc p-uc
);


# Process use/require statements
sub _process_include_statement {
  my $self = shift;
  my $stmt = shift;

  my $perl_code = $stmt->content;
  $perl_code =~ s/;\s*$//;
  $perl_code =~ s/\n/ /g;   # Collapse newlines (multi-line use statements break CL ;; comments)

  my $type = $stmt->type // 'use';    # 'use', 'require', 'no'
  my $module = $stmt->module // '';

  # `require Foo if COND;` / `require Foo unless COND;` — the statement
  # modifier is part of the statement, and require is a RUNTIME op, so the
  # condition must gate it.  PCL used to hoist every require into the
  # definitions bucket and drop the modifier entirely, so a platform-guarded
  # require ran on every platform: File::Temp's `require VMS::Stdio if $^O eq
  # 'VMS'` died with "Can't locate VMS/Stdio.pm" on Linux, taking all of
  # Capture-Tiny with it (task #197).  Same family as #187, where a `use`
  # inside do{} was dropped.
  if ($type eq 'require' && $module ne '') {
    my ($mod_word, $cond_cl) = $self->_include_statement_modifier($stmt);
    if (defined $mod_word) {
      $self->_emit(";; $perl_code");
      $self->_emit($mod_word eq 'if'
                   ? "(p-if $cond_cl (p-require \"$module\"))"
                   : "(p-unless $cond_cl (p-require \"$module\"))");
      $self->_emit("");
      return;
    }
  }

  # Handle 'use constant' specially
  if ($module eq 'constant') {
    $self->_process_use_constant($stmt, $perl_code);
    return;
  }

  # Handle 'use vars' - declare package globals with defvar
  if ($module eq 'vars') {
    $self->_process_use_vars($stmt, $perl_code);
    return;
  }

  # Handle 'no' statements
  if ($type eq 'no') {
    # 'no integer' - turn off integer pragma in current scope
    if ($module eq 'integer') {
      $self->environment->set_pragma('use_integer', 0);
    }
    # 'no strict' / 'no strict "subs"' - disable strict-subs mode
    if ($module eq 'strict') {
      my @args = map { $_->string } grep { $_->isa('PPI::Token::Quote') } $stmt->children;
      if (!@args || grep { /\bsubs\b/ } @args) {
        $self->environment->set_pragma('strict_subs', 0);
      }
    }
    $self->_emit(";; $perl_code (no-op)");
    $self->_emit("");
    return;
  }

  # Handle version declarations (use v5.30, use 5.030, etc.)
  if ($perl_code =~ /^use\s+v?5[\d.]+$/) {
    $self->_emit(";; $perl_code (pragma)");
    $self->_emit("");
    return;
  }

  # Handle require with path expression (e.g., require "./test.pl", require $path,
  # require $path . "/" . $file)
  # PPI returns empty module for these - we need to parse the expression
  if ($module eq '' && $type eq 'require') {
    # Collect all tokens after 'require' (excluding whitespace at start/end and semicolon)
    my @tokens;
    my $found_require = 0;
    for my $child ($stmt->children) {
      if ($child->isa('PPI::Token::Word') && $child->content eq 'require') {
        $found_require = 1;
        next;
      }
      next unless $found_require;
      next if $child->isa('PPI::Token::Structure');  # Skip semicolon
      push @tokens, $child;
    }

    # Skip leading/trailing whitespace
    shift @tokens while @tokens && $tokens[0]->isa('PPI::Token::Whitespace');
    pop @tokens while @tokens && $tokens[-1]->isa('PPI::Token::Whitespace');

    if (@tokens) {
      # Version number (require 5.007, require v5.10, require 10.0.2): a
      # runtime check that dies when the version exceeds the running perl's.
      if (@tokens == 1 && ($tokens[0]->isa('PPI::Token::Number')
                           || ($tokens[0]->isa('PPI::Token::Word')
                               && $tokens[0]->content =~ /^v\d/))) {
        my $lit = $tokens[0]->content;
        $lit =~ s/(["\\])/\\$1/g;
        $self->_emit("(p-require-version \"$lit\")");
        $self->_emit("");
        return;
      }

      # Check if it's a simple string literal (compile-time).  An INTERPOLATING
      # quote with variables — require "File/Spec/$module.pm" (the real
      # File::Spec OS-dispatch) — must NOT be emitted as a raw literal: the
      # $module is only known at runtime.  Treat it as compile-time literal only
      # when the quote does not interpolate (single-quoted / q{}) OR has no
      # sigils; otherwise fall through to the runtime expression path below,
      # which lowers the interpolation to (p-string-concat ...).
      if (@tokens == 1 && $tokens[0]->isa('PPI::Token::Quote')) {
        my $q = $tokens[0];
        my $interpolating = $q->isa('PPI::Token::Quote::Double')
                         || $q->isa('PPI::Token::Quote::Interpolate');
        my $path = $q->string;
        if (!$interpolating || $path !~ /[\$\@]/) {
          # Learn prototypes declared in the required file (e.g. test.pl's
          # `sub is ($$@)`) so child_context can impose SCALAR context on the
          # leading args — same mechanism as `use Module` -> shim prototypes.
          my $file_env = $self->_extract_file_prototypes($path);
          $self->_merge_module_prototypes($file_env, undef) if $file_env;
          $self->_emit(";; $perl_code");
          $self->_emit("(p-eval-always");
          $self->_emit("  (p-require-file \"$path\"))");
          $self->_emit("");
          return;
        }
        # interpolating with variables → fall through to the expression path
      }

      # Otherwise, parse as expression (runtime)
      # Use the parser's _parse_expression method
      my $expr_cl = $self->_parse_expression(\@tokens);
      if ($expr_cl) {
        $self->_emit(";; $perl_code");
        $self->_emit("(p-require-file $expr_cl)");
        $self->_emit("");
        return;
      }
    }

    # Fallback
    $self->_emit(";; $perl_code (require without path)");
    $self->_emit("");
    return;
  }

  # Handle use with empty module (version pragmas handled above)
  if ($module eq '') {
    $self->_emit(";; $perl_code (pragma)");
    $self->_emit("");
    return;
  }

  # Handle 'use overload' - register operator overloading for the current package
  if ($module eq 'overload') {
    $self->_process_use_overload($stmt, $perl_code);
    return;
  }

  # Handle 'use base' / 'use parent' - set up @ISA inheritance
  if ($module eq 'base' || $module eq 'parent') {
    $self->_process_use_base($stmt, $perl_code, $module);
    return;
  }

  # Handle pragmas - emit as comment (no CL equivalent)
  if ($module =~ /^(strict|warnings|warnings::register|feature|utf8|open|bytes|locale|integer|builtin|overloading|XSLoader|DynaLoader|re)$/) {
    # 'use integer' - enable integer pragma in current scope
    if ($module eq 'integer') {
      $self->environment->set_pragma('use_integer', 1);
    }
    # 'use strict' / 'use strict "subs"' - enable strict-subs mode for bareword disambiguation
    if ($module eq 'strict') {
      my @args = map { $_->string } grep { $_->isa('PPI::Token::Quote') } $stmt->children;
      if (!@args || grep { /\bsubs\b/ } @args) {
        $self->environment->set_pragma('strict_subs', 1);
      }
    }
    $self->_emit(";; $perl_code (pragma)");
    $self->_emit("");
    return;
  }

  # Handle 'use lib' - modify @INC
  if ($module eq 'lib') {
    $self->_process_use_lib($stmt, $perl_code);
    return;
  }

  # `require` is a RUNTIME statement AT EVERY DEPTH (task #350, s404).  Only
  # `use` is compile-time; perl runs `require Foo;` where it stands, so a
  # file-top one must not be hoisted above the code before it:
  #
  #     push @INC, $dir;  require MyLocal;    perl: loads
  #                                           PCL before this fix: Can't locate
  #
  # It used to hoist only at depth 0 (into the definitions bucket, as
  # `(p-eval-always (p-require …))`).  A nested one never hoisted, for three
  # reasons that were always the same reason: `eval { require Foo }` must be
  # able to catch the load failure, a `require` in a block runs at runtime, and
  # a `require` inside `SKIP { }` must not run when the block is skipped
  # (scalar.t's `require threads` — hoisting it loads XS unconditionally and
  # dies).  The same argument applies at the top level; the hoist predates the
  # emitted `(p-defpackage …)` line that guarantees read-time package existence
  # today, so nothing depends on it (measured: 52 of 657 files over both
  # populations change emission, every one of them exactly this form moving to
  # its own position).  The quoted-path and expression spellings
  # (`require "f.pl"`, `require $var`) already emitted in place — this makes
  # the family consistent.
  if ($type eq 'require') {
    $self->_emit(";; $perl_code");
    $self->_emit("(p-require \"$module\")");
    $self->_emit("");
    return;
  }

  # General use/require — emit to definitions bucket (before runtime code)
  $self->_with_bucket('definitions', sub {
    if ($type eq 'use') {
      my @imports = $self->_parse_use_import_list($stmt);

      # Extract prototypes from module at transpile time
      # This allows prototypes in other files to work..
      my $module_env = $self->_extract_module_prototypes($module);
      if ($module_env) {
        $self->_merge_module_prototypes($module_env, \@imports);
      }

      # Perl: `use Module LIST` makes LIST a normal list that is evaluated and
      # passed to Module->import(LIST).  Transpile the import-arg tokens through
      # the ordinary list parser (so tests => 5 / qw(a b) / 'no_plan' all work),
      # and hand the resulting vector to p-use.  Bare `use Module;` (no args)
      # passes no :import-args -> import called with no args (default exports).
      $self->_emit(";; $perl_code");
      my @arg_tokens = $self->_use_import_arg_tokens($stmt);
      my $args_cl = @arg_tokens
                  ? $self->_parse_expression(\@arg_tokens, $stmt, 1)  # LIST ctx
                  : undef;
      # A `package Foo;` inside a do{}/eval{}/anon-sub body is only a RUNTIME
      # switch (no CL section, so no `in-package`), and this `use` hoists out
      # of the block to the definitions bucket — where *package* is the
      # ENCLOSING package.  Perl imports into the package in effect at the use,
      # so name it explicitly.  _block_depth is bumped exactly for blocks that
      # carry a package statement, so nothing else changes emission.
      my $cur_pkg = $self->environment->current_package // 'main';
      my $into = ($self->_block_depth > 0
                  && defined $self->{_seam_outer_pkg}
                  && $self->{_seam_outer_pkg} ne $cur_pkg)
               ? qq{ :into "$cur_pkg"}
               : '';
      $self->_emit("(p-eval-always");
      # `use Foo ()` / `use Foo qw()` — an EXPLICIT empty list means "load it,
      # do NOT call import", which is the only reason anyone writes it (both
      # spellings verified against perl).  This must be decided BEFORE the
      # import-args branch, because an empty list still parses to `(vector)`.
      # Calling import anyway is how core's IO/Handle.pm — whose `use IO ()`
      # exists precisely to skip IO.pm's loader — dragged in IO::Socket and
      # friends behind its back (task #197).
      if ($self->_use_has_empty_import_list($stmt)) {
        $self->_emit("  (p-use \"$module\" :do-import nil$into))");
      } elsif (defined $args_cl && $args_cl ne '') {
        $self->_emit("  (p-use \"$module\" :import-args $args_cl$into))");
      } else {
        $self->_emit("  (p-use \"$module\"$into))");
      }
    }
    elsif ($type eq 'require') {
      $self->_emit(";; $perl_code");
      $self->_emit("(p-eval-always");
      $self->_emit("  (p-require \"$module\"))");
    }
    else {
      # Unknown type
      $self->_emit(";; $perl_code");
      $self->_emit(";; (include type '$type' not yet implemented)");
    }
    $self->_emit("");
  });
}


# Process scheduled blocks: BEGIN, END, CHECK, INIT
sub _process_scheduled_block {
  my $self = shift;
  my $stmt = shift;

  my $type = $stmt->type;  # 'BEGIN', 'END', 'CHECK', 'INIT', 'UNITCHECK'
  my $perl_code = $stmt->content;
  $perl_code =~ s/\n.*//s;  # First line only for comment

  # Find the block
  my ($block) = grep { $_->isa('PPI::Structure::Block') } $stmt->schildren;
  unless ($block) {
    $self->_emit(";; $type { } (no block found)");
    return;
  }

  # A bare `package NAME;` statement inside the block is block-scoped in Perl:
  # the package reverts when the block ends.  Snapshot the package stack and
  # bump _block_depth so a package switch inside emits inline (no new section)
  # and gets fully-qualified sub names; after processing, revert any switch so
  # both the CL reader package and the parser environment are restored.
  my $saved_pkg_stack = [@{$self->environment->package_stack}];
  my $prev_pkg        = $self->environment->current_package();
  my $process = sub {
    $self->_block_depth($self->_block_depth + 1);
    # A `local` inside the block opens a (let …) that wraps the rest of the
    # block and is closed by whoever owns the scope.  _process_children does
    # NOT close them — only _process_block and the sub-body path do — so
    # without this the block emits one paren too few and swallows whatever
    # follows it.  For END that was catastrophic and silent at transpile time:
    # File::Temp's `END { local($.,$@,$!,$^E,$?); cleanup(at_exit=>1) }` made
    # the emitted `(push (lambda () …) *end-blocks*)` absorb every later
    # top-level form, so the module died at load with "too many elements in
    # (push …)" — and with it every consumer (Capture::Tiny, task #199).
    my $start_depth = $self->{_local_let_depth} // 0;
    $self->_process_children($block);
    if ($self->environment->current_package() ne $prev_pkg) {
      my $cl_prev = $self->_cl_pkg_designator($prev_pkg);
      $self->_emit("(in-package $cl_prev)");
      $self->_emit("(p-set-current-package $cl_prev \"$prev_pkg\")");
    }
    my $end_depth = $self->{_local_let_depth} // 0;
    while ($end_depth > $start_depth) {
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")  ;; end local");
      $self->{_local_let_depth}--;
      $end_depth--;
    }
    $self->_block_depth($self->_block_depth - 1);
    $self->environment->package_stack($saved_pkg_stack);
  };

  if ($type eq 'BEGIN') {
    # BEGIN blocks execute at compile time — route to definitions bucket.
    # NOT at :load-toplevel - BEGIN should only run once, not again when loading fasl.
    $self->_with_bucket('definitions', sub {
      $self->_emit(";; $perl_code");
      $self->_emit("(p-BEGIN");
      $self->indent_level($self->indent_level + 1);
      # BEGIN blocks run in the definitions bucket, BEFORE this package's runtime
      # `p-set-current-package` (runtime bucket).  Without setting it here,
      # *pcl-current-package* lags during the BEGIN, so caller()/__PACKAGE__ —
      # and any explicit `Module->import` (Moo's `require Moo; Moo->import;`
      # bootstrap) — resolve to the wrong package.  Set it as the first stmt so
      # imports install into the enclosing package.  (in-package is already set
      # in the hoisted preamble, so the CL reader package is correct; this fixes
      # the runtime call-context pointer.)
      my $cl_cur = $self->_cl_pkg_designator($prev_pkg);
      $self->_emit("(p-set-current-package $cl_cur \"$prev_pkg\")");
      $process->();
      $self->indent_level($self->indent_level - 1);
      $self->_emit(")");
      $self->_emit("");
    });
  }
  elsif ($type eq 'END') {
    # END blocks execute at program exit — route to definitions bucket.
    # Push a lambda to *end-blocks* (push gives LIFO = correct reverse order)
    $self->_with_bucket('definitions', sub {
      $self->_emit(";; $perl_code");
      $self->_emit("(push (lambda ()");
      $self->indent_level($self->indent_level + 2);
      $process->();
      $self->indent_level($self->indent_level - 2);
      $self->_emit("  ) *end-blocks*)");
      $self->_emit("");
    });
  }
  elsif ($type eq 'CHECK' || $type eq 'UNITCHECK') {
    # Collected during load (compile phase), run in reverse order at the
    # compile->run boundary by (p-run-compile-phase-blocks) — the same
    # thunk-collector pattern END blocks use (push = LIFO = reverse).
    my $list = $type eq 'CHECK' ? '*check-blocks*' : '*unitcheck-blocks*';
    $self->_with_bucket('definitions', sub {
      $self->_emit(";; $perl_code");
      $self->_emit("(push (lambda ()");
      $self->indent_level($self->indent_level + 2);
      $process->();
      $self->indent_level($self->indent_level - 2);
      $self->_emit("  ) $list)");
      $self->_emit("");
    });
  }
  elsif ($type eq 'INIT') {
    # Collected during load, run in SOURCE order at the compile->run
    # boundary (after CHECKs), before any main runtime code — a mid-file
    # INIT must not run at its source position.
    $self->_with_bucket('definitions', sub {
      $self->_emit(";; $perl_code");
      $self->_emit("(push (lambda ()");
      $self->indent_level($self->indent_level + 2);
      $process->();
      $self->indent_level($self->indent_level - 2);
      $self->_emit("  ) *init-blocks*)");
      $self->_emit("");
    });
  }
  else {
    $self->_emit(";; $type { } (unrecognized scheduled block)");
  }
}


# Process 'use overload' — register operator overloading for the current package.
# Generates: (p-register-overloads (package-name *package*) PAIRS-VECTOR)
# where PAIRS-VECTOR is the transpiled form of the alternating op/handler list.
sub _process_use_overload {
  my ($self, $stmt, $perl_code) = @_;

  # Collect all tokens after the 'overload' keyword and before the semicolon.
  my @arg_tokens;
  my $past_module = 0;
  for my $child ($stmt->children) {
    if (!$past_module) {
      $past_module = 1
        if $child->isa('PPI::Token::Word') && $child->content eq 'overload';
      next;
    }
    next if $child->isa('PPI::Token::Structure');   # semicolon
    # Skip only leading whitespace (preserve whitespace within the list)
    next if !@arg_tokens && $child->isa('PPI::Token::Whitespace');
    push @arg_tokens, $child;
  }

  # Strip trailing whitespace
  pop @arg_tokens while @arg_tokens && $arg_tokens[-1]->isa('PPI::Token::Whitespace');

  if (!@arg_tokens) {
    $self->_emit(";; $perl_code (use overload - no handlers)");
    $self->_emit("");
    return;
  }

  # Parse the arg list in list context.
  # "+" => \&add, "0+" => \&numify, ...  →  (vector "+" #'pl-add "0+" #'pl-numify ...)
  my $args_cl = $self->_parse_expression(\@arg_tokens, $stmt, 1);  # 1 = LIST_CTX

  # Use the Perl package name as a literal string (not (package-name *package*))
  # because CL upcases package names ("MyStr" → "MYSTR") but p-bless stores the
  # original Perl class name as-is.  We need them to match at lookup time.
  my $pkg_name = $self->environment->current_package() // 'main';

  # Sanitize perl_code for comment — multi-line use overload would break CL
  (my $comment = $perl_code) =~ s/\n.*//s;  # keep only first line
  $self->_emit(";; $comment ...");
  $self->_emit("(p-register-overloads \"$pkg_name\" $args_cl)");
  $self->_emit("");
}


# Process 'use lib' statements
sub _process_use_lib {
  my ($self, $stmt, $perl_code) = @_;

  # use lib is compile-time @INC manipulation — route to definitions bucket
  # so it appears before any 'require' or 'use' in the same section
  $self->_with_bucket('definitions', sub {
    $self->_emit(";; $perl_code");
    $self->_emit("(p-eval-always");

    # Extract path arguments from the statement
    for my $child ($stmt->schildren) {
      if ($child->isa('PPI::Token::Quote')) {
        # Through the ordinary expression path, never $child->string wrapped in
        # CL quotes: `use lib "$ENV{HOME}/lib"` is INTERPOLATED by perl at
        # compile time, and the raw text put the literal characters
        # `$ENV{HOME}/lib` on @INC — which surfaces much later as
        # "Can't locate X.pm in @INC" with the un-interpolated path in the
        # message, the way this was found (task #235).  The same seam also
        # escapes a quote or a backslash inside the path, which the raw wrap
        # did not.
        $self->_emit("  (p-unshift \@INC " . $self->_parse_expression([$child], $stmt) . ")");
      }
      elsif ($child->isa('PPI::Token::QuoteLike::Words')) {
        # qw(path1 path2)
        my $content = $child->content;
        $content =~ s/^qw\s*[\(\[\{<]//;
        $content =~ s/[\)\]\}>]$//;
        for my $path (split /\s+/, $content) {
          $self->_emit("  (p-unshift \@INC \"$path\")") if $path;
        }
      }
    }
    $self->_emit(")");  # Close eval-when
    $self->_emit("");
  });

  # Also add to transpiler's inc_paths for module finding
  for my $child ($stmt->schildren) {
    if ($child->isa('PPI::Token::Quote')) {
      unshift @{$self->inc_paths}, $child->string;
    }
    elsif ($child->isa('PPI::Token::QuoteLike::Words')) {
      my $content = $child->content;
      $content =~ s/^qw\s*[\(\[\{<]//;
      $content =~ s/[\)\]\}>]$//;
      for my $path (split /\s+/, $content) {
        unshift @{$self->inc_paths}, $path if $path;
      }
    }
  }
}


# Find a module file in @INC paths
# Returns the path to the .pm file, or undef if not found
sub _find_module_file {
  my ($self, $module) = @_;

  # Convert Module::Name to Module/Name.pm
  my $file = $module;
  $file =~ s/::/\//g;
  $file .= '.pm';

  for my $inc (@{$self->inc_paths}) {
    my $path = "$inc/$file";
    return $path if -f $path;
  }

  return undef;
}


# Extract prototypes from a module by parsing it at transpile time.
# Returns the Environment from parsing, or undef on failure.
#
# NOTE: This is RECURSIVE. When the module being parsed contains 'use'
# statements, those trigger _extract_module_prototypes() calls for their
# modules, and so on. The _parsing_modules hash (shared across all
# recursive calls) prevents infinite loops from circular dependencies.
# Results are memoized in a state hash to avoid re-parsing modules.
sub _extract_module_prototypes {
  my ($self, $module) = @_;

  # Memoization cache (persists across all calls)
  state $cache = {};

  # Return cached result if already parsed
  return $cache->{$module} if exists $cache->{$module};

  # Skip known core modules that don't have prototypes affecting codegen.
  # (List::Util is intentionally NOT skipped: its shim declares block
  # prototypes — first/any/reduce/pair* (&@) — that the block-form parser
  # needs, so its prototypes must be extracted from lib/List/Util.pm.
  # File::Spec[::Functions] is NOT skipped either: sub-EXISTENCE is data too —
  # a bareword before a comma is a call only for a KNOWN sub, so skipping the
  # shim made `catfile(updir, ...)` read updir as the string "updir"
  # (op/signatures.t keywords block, s316l — same lesson as List::Util.)
  if ($module =~ /^(Carp|Scalar::Util|Time::HiRes|Cwd|
                    XSLoader|DynaLoader|Exporter|base|parent|strict|warnings|
                    utf8|bytes|overload|mro|B::|POSIX|File::(?!Spec)|IO::|Data::Dumper)/x) {
    return $cache->{$module} = undef;
  }
  # Skip the heavy Test2 stack and Test:: internals — EXCEPT Test::More, whose
  # tiny lib/Test/More.pm shim declares the assertion prototypes (is($$;$),
  # ok($;$), like($$;$), …) that child_context needs to impose SCALAR context on
  # their arguments.  The shim wins in @INC, so we read the prototype stub, never
  # the real Test2 stack.  (Test::Simple has no scalar-forcing prototypes — its
  # only export, ok, is satisfied by the internal TAP layer — so it is skipped
  # like every other Test:: module.)
  if (($module =~ /^Test2::/ || $module =~ /^Test::/)
      && $module !~ /^Test::More$/) {
    return $cache->{$module} = undef;
  }

  # Cycle detection
  return undef if $self->_parsing_modules->{$module};
  local $self->_parsing_modules->{$module} = 1;

  my $module_path = $self->_find_module_file($module);
  return $cache->{$module} = undef unless $module_path;

  my $module_env = Pl::Environment->new();

  my $module_parser = Pl::Parser->new(
    filename                => $module_path,
    environment             => $module_env,
    inc_paths               => $self->inc_paths,
    _parsing_modules        => $self->_parsing_modules,
    collect_prototypes_only => 1,
  );

  # ONE PPI parse (ppi_doc = _ppi_parse: the shared repairs + the :prototype
  # attribute pre-pass); the walk may recursively call
  # _extract_module_prototypes() for the module's own `use` statements.
  my $doc = eval { $module_parser->ppi_doc };
  return $cache->{$module} = undef unless $doc;
  eval { $module_parser->collect_prototypes($doc) };
  if ($@) {
    warn "Failed to extract prototypes from $module: $@";
    return $cache->{$module} = undef;
  }
  # PCL_PROTO_ORACLE=DIR (task #391 measurement): dump what this walk
  # produced — the prototype records and the export names — one JSON file
  # per module, so a facts-only walk can be diffed against it.
  _dump_proto_oracle($module, $module_path, $module_env) if $ENV{PCL_PROTO_ORACLE};

  # Record @EXPORT/@EXPORT_OK names: _merge_module_prototypes imports exported
  # plain subs for their EXISTENCE (a bareword before a comma is a call only
  # for a known sub — `catfile(updir, ...)`).  qw() lists only: that is the
  # shape every shim (and nearly every real module) uses.
  my %exported;
  for my $st (@{ $doc->find('PPI::Statement::Variable') || [] }) {
    my $content = $st->content;
    next if $content !~ /\@EXPORT(?:_OK)?\b/;
    while ($content =~ /qw\s*[\(\[\{<]\s*([^)\]\}>]*)/g) {
      $exported{$_} = 1 for split ' ', $1;
    }
  }
  $module_env->export_names(\%exported);

  return $cache->{$module} = $module_env;
}

sub _dump_proto_oracle {
  my ($module, $path, $env) = @_;
  require JSON::PP;
  (my $slug = $module) =~ s/[^\w.]+/_/g;
  my $dir = $ENV{PCL_PROTO_ORACLE};
  my %rec;
  for my $name (sort keys %{ $env->prototypes }) {
    my $p = $env->prototypes->{$name};
    # default_cl carries compiled CL text (a signature default) — keep it, it
    # is part of the record a caller receives.
    $rec{$name} = $p;
  }
  my $out = { module => $module, path => $path, prototypes => \%rec,
              export_names => [ sort keys %{ $env->export_names // {} } ] };
  open my $o, '>:raw', "$dir/$slug.json" or die "$dir/$slug.json: $!";
  print $o JSON::PP->new->utf8->canonical->pretty->encode($out);
  close $o;
}


# Extract prototypes from a file required by literal path (require "./test.pl").
# This is the require-equivalent of _extract_module_prototypes: perl-tests load
# their assertion helpers via `require './test.pl'` (not `use Test::More`), and
# that file declares the real prototypes (sub is ($$@), ...) which child_context
# needs to impose SCALAR context on the leading args.  Nested requires in the
# parsed file recurse through this same path, so the test.pl -> t/test.pl
# redirect is followed automatically.
sub _extract_file_prototypes {
  my ($self, $path) = @_;
  state $cache = {};

  # Resolve the path: relative to cwd first, then to the source file's dir,
  # then walking UP the source file's ancestors.  The ancestor walk handles
  # Perl's ubiquitous test idiom `chdir 't' if -d 't'; require './test.pl'`:
  # at RUNTIME the process is in the `t/` dir so `./test.pl` resolves, but at
  # COMPILE time the source is e.g. `.../t/io/scalar.t` while `test.pl` lives in
  # the grandparent `.../t/`.  Searching ancestors for the (leading-`./`-stripped)
  # basename-path finds it, so the required file's prototypes (test.pl's
  # `sub is ($$@)`) are learned and child_context can impose SCALAR context.
  my $rel = $path;
  $rel =~ s{^\./}{};
  my @candidates = ($path);
  if ($self->filename) {
    require File::Basename;
    my $dir = File::Basename::dirname($self->filename);
    if (defined $dir && length $dir) {
      push @candidates, "$dir/$path";
      # Walk up to 8 ancestors looking for the (relative) required file.
      my $up = $dir;
      for (1 .. 8) {
        push @candidates, "$up/$rel";
        my $parent = File::Basename::dirname($up);
        last if !defined $parent || !length $parent || $parent eq $up;
        $up = $parent;
      }
    }
  }
  my $resolved;
  for my $c (@candidates) {
    if (-f $c) { $resolved = $c; last; }
  }
  return undef unless $resolved;

  require Cwd;
  my $abs = Cwd::abs_path($resolved) // $resolved;
  return $cache->{$abs} if exists $cache->{$abs};

  # Cycle detection (shared across the require chain)
  return undef if $self->_parsing_modules->{"file:$abs"};
  local $self->_parsing_modules->{"file:$abs"} = 1;

  my $file_env = Pl::Environment->new();
  my $file_parser = Pl::Parser->new(
    filename                => $abs,
    environment             => $file_env,
    inc_paths               => $self->inc_paths,
    _parsing_modules        => $self->_parsing_modules,
    collect_prototypes_only => 1,
  );
  my $doc = eval { $file_parser->ppi_doc };
  return $cache->{$abs} = undef unless $doc;
  eval { $file_parser->collect_prototypes($doc) };
  return $cache->{$abs} = undef if $@;
  _dump_proto_oracle("file:$abs", $abs, $file_env) if $ENV{PCL_PROTO_ORACLE};
  return $cache->{$abs} = $file_env;
}


# Merge prototypes from another environment (only exported ones)
sub _merge_module_prototypes {
  my ($self, $module_env, $imports) = @_;

  # A merged prototype is tagged from_module, and a merge never overwrites
  # a LOCAL declaration (an untagged entry).  Under v2, sub definitions are
  # HOISTED before the use statement's seam re-merge runs, so without this
  # the re-merge clobbered a local `sub modify_array(\$)` override with the
  # imported (\@) — and every later call auto-boxed the WRONG way (silent
  # wrong; Pl/t/prototype-01.t rows 80-82).  Re-merging the same module is
  # still idempotent (from_module entries overwrite each other freely).
  my $add = sub {
    my ($name, $proto) = @_;
    my $existing = $self->environment->get_prototype($name);
    return if $existing && !$existing->{from_module};
    $self->environment->add_prototype($name, { %$proto, from_module => 1 });
  };

  # If specific imports requested, only import those
  if ($imports && @$imports) {
    for my $name (@$imports) {
      my $proto = $module_env->get_prototype($name);
      if ($proto) {
        $add->($name, $proto);
      }
    }
    return;
  }

  # Otherwise import @EXPORT (we'd need to track this in Environment)
  # For now, import all prototypes that affect code generation:
  # - has_block_arg: requires &{} wrapping
  # - reference params (\@, \%, \$): require auto-boxing
  # - scalar params ($): impose SCALAR context on that argument (child_context),
  #   so e.g. Test::More's is($$;$) evaluates `is(try {...}, ...)` in scalar
  #   context.  Any old-style prototype ($-proto) is a context signal now, so a
  #   plain ($$) sub propagates too — not just block/ref prototypes.
  # This is a simplification - full implementation would track @EXPORT
  for my $name (keys %{$module_env->prototypes}) {
    my $proto = $module_env->get_prototype($name);
    next unless $proto;

    # Check if this prototype affects code generation
    my $needs_import = 0;
    $needs_import = 1 if $proto->{has_block_arg};

    # An old-style prototype with explicit parameter slots affects argument
    # context (scalar $, ref \X, or slurpy @/%) — import it so child_context
    # can apply the right wantarray to each argument.
    if ($proto->{is_proto} && $proto->{params} && @{$proto->{params}}) {
      for my $param (@{$proto->{params}}) {
        my $ptype = $param->{proto_type} // $param->{name} // '';
        if ($ptype =~ /^\\/ || $ptype eq '$' || $ptype eq '@' || $ptype eq '%') {
          $needs_import = 1;
          last;
        }
      }
    }

    # Exported plain sub: its EXISTENCE is the data — without it a bareword
    # use before a comma (`catfile(updir, ...)`) reads as a string.
    $needs_import = 1 if $module_env->export_names->{$name};

    if ($needs_import) {
      $add->($name, $proto);
    }
  }
}


# The import-argument tokens of a `use`/`no` statement: everything after
# `use Module [VERSION]`, minus the trailing ';'.  These ARE a Perl list (the
# LIST in `use Module LIST`), so the caller runs them through the normal list
# parser.  The optional module VERSION (use Foo 1.2 …) is NOT an import arg
# (Perl calls Foo->VERSION(1.2)), so it's skipped.
sub _include_statement_modifier {
  # An include statement carrying a trailing `if`/`unless` modifier:
  # returns (MODIFIER-WORD, CONDITION-AS-CL) or () when there is none.
  # Only if/unless are recognised — a loop modifier on an include is not
  # something real Perl writes, and guessing at one would be worse than
  # leaving it on the existing (unconditional) path.
  my ($self, $stmt) = @_;
  my @kids = $stmt->schildren;
  my ($mod_i, $mod_word);
  for my $i (0 .. $#kids) {
    next unless $kids[$i]->isa('PPI::Token::Word');
    my $c = $kids[$i]->content;
    next unless $c eq 'if' || $c eq 'unless';
    # The modifier is the LAST such word (a module named `if` is a real
    # pragma, so only a word that has an expression after it counts).
    ($mod_i, $mod_word) = ($i, $c);
  }
  return () unless defined $mod_i;
  my @cond = grep { !($_->isa('PPI::Token::Structure') && $_->content eq ';') }
             @kids[$mod_i + 1 .. $#kids];
  return () unless @cond;
  my $cl = $self->_parse_expression(\@cond, $stmt);
  return () unless defined $cl && $cl ne '';
  return ($mod_word, $cl);
}

sub _use_has_empty_import_list {
  # True for `use Foo ()` / `use Foo ( )` — an explicitly EMPTY import list,
  # which Perl reads as "load the module, do not call its import".  A bare
  # `use Foo;` (no list at all) is a different thing: import with no arguments.
  my ($self, $stmt) = @_;
  my @args = $self->_use_import_arg_tokens($stmt);
  return 0 unless @args == 1;
  my $list = $args[0];
  # qw() with no words is the same statement to perl: import is not called.
  return (($list->literal)[0] ? 0 : 1)
    if $list->isa('PPI::Token::QuoteLike::Words');
  return 0 unless $list->isa('PPI::Structure::List');
  # Empty either way PPI spells it: no children at all, or one expression with
  # no significant children.
  my @kids = $list->schildren;
  return 1 if !@kids;
  return 1 if @kids == 1
           && $kids[0]->isa('PPI::Statement::Expression')
           && !$kids[0]->schildren;
  return 0;
}

sub _use_import_arg_tokens {
  my ($self, $stmt) = @_;
  my $ver = $stmt->version;
  $ver = (defined $ver && ref($ver)) ? $ver->content : $ver;
  my @args;
  my $past_module = 0;
  for my $c ($stmt->schildren) {
    if (!$past_module) {
      next if $c->isa('PPI::Token::Word')
           && ($c->content eq 'use' || $c->content eq 'no' || $c->content eq 'require');
      $past_module = 1, next if $c->isa('PPI::Token::Word');   # module name
      next;                                                    # anything else before it
    }
    next if $c->isa('PPI::Token::Structure') && $c->content eq ';';
    # Skip the module VERSION number sitting right after the module name.
    # PPI's $stmt->version is only filled for PERL-version statements
    # (`use 5.030`), never for `use Module 1.0 LIST` — so the $ver comparison
    # below was dead for module versions and every `use Foo 1.0 LIST` sent
    # "1.0 LIST" through the expression parser (a PARSE ERROR progn, s327).
    # Recognize the version positionally instead: a Number as the FIRST arg
    # token is a VERSION unless an operator follows it (`use Foo 1.0, 'x'`
    # makes it a plain list element — perl separates VERSION from LIST by the
    # absence of a comma).
    if (!@args
        && ($c->isa('PPI::Token::Number') || $c->isa('PPI::Token::Number::Version'))) {
      next if defined $ver && $ver ne '' && $c->content eq $ver;
      my $next = $c->snext_sibling;
      next if !$next || !$next->isa('PPI::Token::Operator');
    }
    push @args, $c;
  }
  return @args;
}

# Parse import list from use statement (e.g., qw(foo bar) or ('foo', 'bar'))
sub _parse_use_import_list {
  my ($self, $stmt) = @_;
  my @imports;

  for my $child ($stmt->schildren) {
    if ($child->isa('PPI::Token::QuoteLike::Words')) {
      # qw(foo bar baz) — use PPI's literal() so ALL delimiters work
      # (qw/.../, qw!...!, qw,..., not just brackets). The old manual strip
      # only handled ([{< and silently passed e.g. qw/%Config/ through as the
      # literal token "qw/%Config/", breaking `use Config qw/%Config/`.
      push @imports, $child->literal;
    }
    elsif ($child->isa('PPI::Structure::List')) {
      # ('foo', 'bar') import list
      for my $item ($child->schildren) {
        if ($item->isa('PPI::Statement::Expression')) {
          for my $expr_child ($item->schildren) {
            if ($expr_child->isa('PPI::Token::Quote')) {
              push @imports, $expr_child->string;
            }
          }
        }
        elsif ($item->isa('PPI::Token::Quote')) {
          push @imports, $item->string;
        }
      }
    }
  }

  return grep { defined $_ && $_ ne '' } @imports;
}


# Process 'use vars' - declare package globals with defvar
# use vars '@foo', use vars qw($a @b %c)
sub _process_use_vars {
  my ($self, $stmt, $perl_code) = @_;

  # Collect variable names from the argument list
  # Handles: use vars '@foo'       (single string)
  #          use vars qw(@a $b %c) (qw() list)
  #          use vars ('@a', '$b') (list)
  my @vars;
  for my $child ($stmt->children) {
    my $ref = ref($child);
    if ($ref eq 'PPI::Token::QuoteLike::Words') {
      # qw(@a $b)
      my $content = $child->content;
      $content =~ s/^qw[^\w\s]//;  # strip leading qw(
      $content =~ s/[^\w\s]$//;    # strip trailing )
      push @vars, split /\s+/, $content;
    }
    elsif ($ref eq 'PPI::Structure::List') {
      # ('@a', '$b')
      for my $item ($child->children) {
        next if ref($item) =~ /Whitespace|Separator/;
        if (ref($item) eq 'PPI::Token::Quote::Single' || ref($item) eq 'PPI::Token::Quote::Double') {
          push @vars, $item->string;
        }
      }
    }
    elsif ($ref eq 'PPI::Token::Quote::Single' || $ref eq 'PPI::Token::Quote::Double') {
      # use vars '@foo'  (single arg as string)
      push @vars, $child->string;
    }
  }

  # Filter to actual sigiled variables
  @vars = grep { /^[\$\@\%]/ } @vars;
  return unless @vars;

  my $pkg = $self->environment->current_package;
  for my $var (@vars) {
    $self->environment->add_our_variable($pkg, $var);
  }

  # Route defvars to declarations bucket
  $self->_with_bucket('declarations', sub {
    $self->_emit(";; $perl_code");
    for my $var (@vars) {
      my $sigil = substr($var, 0, 1);
      my $name = substr($var, 1);
      my $cl_var;
      if ($sigil eq '$') {
        $cl_var = "\$$name";
      } elsif ($sigil eq '@') {
        $cl_var = "\@$name";
      } else {
        $cl_var = "\%$name";
      }
      my $init = $sigil eq '$' ? '(make-p-box nil)'
               : $sigil eq '@' ? '(make-array 0 :adjustable t :fill-pointer 0)'
               :                 '(make-hash-table :test #\'equal)';
      $self->_emit("(p-eval-always");
      $self->_emit("  " . global_decl_form(cl_sym($cl_var), "$init") . ")");
    }
    $self->_emit("");
  });
}


# Process 'use constant' declarations
sub _process_use_constant {
  my $self      = shift;
  my $stmt      = shift;
  my $perl_code = shift;

  # Get children after 'constant'
  my @children = $stmt->schildren;
  # Skip: 'use', 'constant'
  my $i = 0;
  while ($i < @children && $children[$i]->content ne 'constant') {
    $i++;
  }
  $i++;  # Skip 'constant' itself

  # What's next determines the form
  return if $i >= @children;

  my $next = $children[$i];

  if ($next->isa('PPI::Structure::Constructor')) {
    # Hash-style: use constant { PI => 3.14, E => 2.71 };
    $self->_process_constant_hash($next, $perl_code);
  }
  elsif ($next->isa('PPI::Token::Word')) {
    # Single: use constant PI => 3.14;
    my $name = $next->content;
    # Get the value (skip => operator)
    my @value_parts;
    for my $j ($i+1 .. $#children) {
      my $child = $children[$j];
      next if $child->isa('PPI::Token::Operator') && $child->content eq '=>';
      next if $child->isa('PPI::Token::Structure');  # Skip ;
      push @value_parts, $child;
    }
    $self->_process_single_constant($name, \@value_parts, $perl_code);
  }
}


# Process hash-style constant declaration
sub _process_constant_hash {
  my $self      = shift;
  my $struct    = shift;
  my $perl_code = shift;

  # Constants are compile-time — route to definitions bucket
  $self->_with_bucket('definitions', sub {
    $self->_emit(";; $perl_code");

    # Get the expression inside the braces
    my @contents = $struct->schildren;

    for my $content (@contents) {
      next unless $content->isa('PPI::Statement::Expression');

      # Parse the expression children for name => value pairs
      my @parts = $content->schildren;
      my $i = 0;
      while ($i < @parts) {
        # Get name
        my $name_tok = $parts[$i];
        last unless $name_tok && $name_tok->isa('PPI::Token::Word');
        my $name = $name_tok->content;
        $i++;

        # Skip =>
        $i++ while $i < @parts && $parts[$i]->isa('PPI::Token::Operator') && $parts[$i]->content eq '=>';

        # Collect value tokens until , or end
        my @value_parts;
        while ($i < @parts) {
          my $part = $parts[$i];
          last if $part->isa('PPI::Token::Operator') && $part->content eq ',';
          push @value_parts, $part;
          $i++;
        }

        # Skip comma
        $i++ if $i < @parts && $parts[$i]->isa('PPI::Token::Operator') && $parts[$i]->content eq ',';

        # Process this constant
        $self->_emit_constant($name, \@value_parts);
      }
    }
    $self->_emit("");
  });
}


# Process single constant declaration
sub _process_single_constant {
  my $self        = shift;
  my $name        = shift;
  my $value_parts = shift;
  my $perl_code   = shift;

  # Constants are compile-time — route to definitions bucket
  $self->_with_bucket('definitions', sub {
    $self->_emit(";; $perl_code");
    $self->_emit_constant($name, $value_parts);
    $self->_emit("");
  });
}


# Emit a single constant definition
# Constants are implemented as zero-arg functions (like Perl does internally)
sub _emit_constant {
  my $self        = shift;
  my $name        = shift;
  my $value_parts = shift;

  # Compile the value expression to CL
  my $cl_value = $self->_compile_constant_value($value_parts);

  # Emit as a function (Perl implements constants as subs)
  # Use p-sub for compile-time visibility (BEGIN blocks can use constants)
  # Every Perl sub accepts @_, so the constant must tolerate args too — a bare
  # `&CONST` (no parens) re-uses the caller's @_ and would otherwise hit an
  # arity error against a strict 0-arg lambda list.  The `(progn %_args VALUE)`
  # references %_args so it ignores (and silences the unused-var warning on)
  # the arguments while still returning the constant value.
  my $cl_sub_name = $self->_qualified_sub_to_cl($name);
  $self->_emit("(p-sub $cl_sub_name (&rest %_args) (progn %_args $cl_value))");

  # Register as a zero-arg prototype so bareword is recognized as function call
  $self->environment->add_prototype($name, {
    params     => [],
    min_params => 0,
    is_proto   => 0,
  });
}


# Compile a constant's value expression to CL
sub _compile_constant_value {
  my $self  = shift;
  my $parts = shift;

  # NOTHING is short-circuited here — every value goes through the same
  # ExprToCL path the rest of the program's expressions use.
  #
  # Numbers never were: emitting $tok->content raw mishandles octal
  # (0777 -> CL 777) and, worse, float literals that overflow double range
  # (e.g. POSIX::LDBL_MAX 1.18e+4932) which SBCL cannot even read, crashing
  # compile-file.  A single QUOTE token used to be, wrapping $tok->string in CL
  # quotes, and that was the same trap one type over — four ways at once
  # (probed s374, task #235's family):
  #   use constant X => "$ENV{HOME}/a";  # interpolation DROPPED
  #   use constant X => "a\nb";          # CL reads \n as n  -> "anb"
  #   use constant X => 'a\b';           # CL eats the \     -> "ab"
  #   use constant X => 'a"b';           # emits "a"b" — UNREADABLE CL, the
  #                                      #   whole file dies at load
  # The ExprToCL path escapes and interpolates correctly for all of them.
  #
  # Complex expression: use PExpr to parse
  my $result;
  eval {
    my $expr_o = $self->_expr_parser($parts);
    my ($node_id, $cdecls) = $expr_o->parse_expr_to_tree($parts);
    # An embedded `our $var` in the constant value (e.g. use constant K => \our
    # $v) is stripped by extract_declarations and otherwise lost: this value
    # compiles to a sub body, so the file-scope forward-declaration scan never
    # sees the reference and the package global is never defvar'd. Register each
    # `our` here (BEFORE generate, so non-main packages qualify consistently) and
    # queue a file-level defvar via expression_our_vars.
    if ($cdecls && $self->environment) {
      my $pkg = $self->environment->current_package;
      for my $d (@$cdecls) {
        next unless ($d->{type} // '') eq 'our';
        my $var = $d->{var} // '';
        next unless $var =~ /^([\$\@\%])\w+$/;
        $self->environment->add_our_variable($pkg, $var);
        $self->environment->expression_our_vars->{ $self->_our_var_cl_name($pkg, $var) } = $1;
      }
    }
    my $gen = $self->_expr_generator($expr_o);
    $result = $gen->generate($node_id);
  };

  die $@ if $@ && $@ =~ /^PCL:/;
  return $result // '0';  # Fallback
}


# Parse an expression using PExpr and generate CL
sub _parse_expression {
  my $self    = shift;
  my $parts   = shift;
  my $stmt    = shift;  # Original statement for full_PPI
  my $context = shift // 0;  # 0 = SCALAR_CTX (default), 1 = LIST_CTX

  # Call the internal version that returns declarations too
  my ($result, $decls) = $self->_parse_expression_internal($parts, $stmt, $context);

  # In scalar context, just return result (backwards compatible)
  return $result unless wantarray;

  # In list context, return result and declarations
  return ($result, $decls);
}

# Internal: parse expression and return both CL code and declarations
sub _parse_expression_internal {
  my $self    = shift;
  my $parts   = shift;
  my $stmt    = shift;
  my $context = shift // 0;  # 0 = SCALAR_CTX (default), 1 = LIST_CTX

  my $result;
  my @decls;

  eval {
    my $expr_o = $self->_expr_parser($parts, $stmt);

    # Capture declarations in list context
    my ($node_id, $decl_list) = $expr_o->parse_expr_to_tree($parts);
    @decls = @{$decl_list // []};

    # Annotate AST with context information (scalar/list)
    $expr_o->annotate_contexts($node_id, $context);

    my $gen = $self->_expr_generator($expr_o);

    $result = $gen->generate($node_id);
  };

  if ($@) {
    my $error = $self->_shape_expr_error($@);
    $self->_announce_dropped_statement($parts, $stmt, $error);
    return ($self->_dropped_statement_cl($parts, $stmt, $error), []);
  }

  return ($result // ";; (no output)", \@decls);
}

# THE TWO CONSTRUCTIONS every expression entry in this file makes (#387
# families 14 + 26 — six copies).  They stay two calls rather than one
# "compile this" helper because what happens BETWEEN them differs at every
# site: parse_hash_block_* assembles a hash_init top node, _compile_constant_value
# registers embedded `our` declarations before generating, the two
# _parse_expression entries annotate contexts.  The generator must see the
# tree its own site built.
#
# full_PPI is passed only where the site passed it: it is never READ (grep
# says so) — it exists to keep the PPI document alive so its tokens are not
# collected mid-parse — so handing it undef would be a silent change of who
# owns that reference.
sub _expr_parser {
  my ($self, $parts, $full_ppi) = @_;
  return Pl::PExpr->new(
    e           => $parts,
    (defined $full_ppi ? (full_PPI => $full_ppi) : ()),
    environment => $self->environment,
    parser      => $self,
  );
}

# indent_level 0 at every site: generate() prepends only the form's own
# indentation and _emit re-applies the caller's base indent, so passing
# $self->indent_level here double-counted it and pushed each emitted form far
# right of its ;; comment.
sub _expr_generator {
  my ($self, $expr_o, %opt) = @_;
  return Pl::ExprToCL->new(
    expr_o       => $expr_o,
    environment  => $self->environment,
    indent_level => 0,
    ($opt{sub_info} ? (sub_info => $opt{sub_info}) : ()),
    ($opt{lexicals} ? (lexicals => $opt{lexicals}) : ()),
  );
}

# Shared by _parse_expression_internal and _parse_expression_form.
# Hard errors (e.g. unsupported features) must propagate — don't swallow.
# EXCEPT: assignment to a non-lvalue (user :lvalue) sub call.  That die is
# only *meant* to be hard in eval-string mode, where it makes a feature
# probe (CMM's `eval 'return 1; &_sub = 1'`) fail and return undef.  In
# whole-file mode it must degrade to a per-statement PARSE ERROR so one
# unsupported lvalue-sub assignment doesn't abort the entire file (e.g.
# perl-tests/substr.t defines `sub bar : lvalue` and does `bar = "XXX"`).
# Returns the shaped one-line message for the soft ones.
sub _shape_expr_error {
  my ($self, $error) = @_;
  die $error if $error =~ /^PCL:/
    && ($self->eval_mode || $error !~ /non-lvalue subroutine/);
  $error =~ s/ at \/.*//s;  # Remove file/line info
  $error =~ s/\n.*//s;      # First line only
  return $error;
}

# ---------------------------------------------------------------------------
# THE RULED REFUSALS AT THE DROP SITE (Option B phase 2 Track A, task #371;
# spec docs/option-b-phase2-plan.md §2).
#
# A statement the compiler could not lower is replaced by `nil` and the
# program runs on -- the #138 family, and silence is its sin (the announcement
# above removes that).  For a handful of shapes, though, CONTINUING is wrong
# too: the drop is not a compiler gap but a FEATURE PCL does not have, so the
# program that results is not the program that was written, and no later fix
# to the term grammar will change that.  perl agrees for two of them --
# `given`/`when` and smart match were REMOVED in perl 5.42, so perl itself
# refuses such a file at compile time rather than running part of it.
#
# The classifier below therefore runs ONLY where a statement was ALREADY lost
# (a statement that compiles never reaches it, so it cannot break working
# code) and DIES perl-shaped, citing docs/not-supported.md, for five families:
# given/when, class/field/method/ADJUST, format, defer, and INFIX `~~`.
# Everything else keeps dropping, announced, until the census is explained and
# the emitters flip wholesale (fable-answers-s400.md §6.4, task #343).
#
# WHAT IS DELIBERATELY NOT HERE:
#   * lvalue-sub assignment -- ruled to stay a loud DROP (§6.3): the drop
#     costs one statement, a die costs every other row of substr.t and
#     op/sub_lval.t.
#   * INDIRECT OBJECT syntax (`doit $object "FOO"`) -- Track A's table lists
#     it, but the measurement says it does not belong with the others: its
#     only two census drops are perl-tests/ref.t:334 and perl-tests/method.t:72
#     -- files that contribute 191 and 97 passing rows today -- and it is a
#     syntax PCL could parse rather than a feature perl has removed.  Task
#     #399 carries the measurement and the decision.
#
# EVERY BRANCH IS CONSERVATIVE, because the two directions are not
# symmetrical: a missed refusal leaves today's behaviour, a false one kills a
# whole file.  `~~` is the sharp edge -- PPI lexes ONE `~~` Operator token for
# both the smart match and the double bitwise complement, and
# `is(~~$y, 3)` (perl-tests/bop.t, 507 rows) is the latter.  So the smart
# match is recognised only when the token has a TERM before it.
sub _drop_lead_parts {
  my ($src) = @_;
  my @el = grep { ref $_ && $_->isa('PPI::Element') } @{ $src // [] };
  return () unless @el;
  # ONE path for the two callers of the announcer: it hands us either the
  # statement itself or the token run that was being lowered.
  return $el[0]->schildren
    if @el == 1 && $el[0]->isa('PPI::Statement');
  return grep { !$_->isa('PPI::Token::Whitespace')
             && !$_->isa('PPI::Token::Comment') } @el;
}

# True when the file has told us the 5.38 object syntax is in play, so that a
# `method`/`field`/`ADJUST` statement is that feature and not a sub of the
# same name (Moose-style `method foo {...}` must NOT be misdiagnosed).
# TWO KEYS, deliberately, and the difference is what the statement costs if the
# answer is wrong (RULED s416, `docs/fable-answers-s415.md` §7.5):
#
#   LOOSE (the default, drop sites) also accepts the `use v5.38+` BUNDLE as
#     evidence.  At a drop site the statement is already lost, so a guess that
#     names the feature is strictly better than "Fell through".
#   STRICT ($strict, for a refusal on code that COMPILES) does NOT.  'class' is
#     experimental and is in NO version bundle — `use v5.38; class Foo;` is a
#     perl SYNTAX ERROR, probed — so a bundle can never be evidence about code
#     that compiles.  Strict also requires a `class NAME {` BLOCK as the
#     in-file evidence, never the statement form: `class NAME ;` cannot be its
#     own reason for being refused.
#
# ONE scanner with a flag rather than two, so the two readings cannot drift.
sub _class_feature_in_scope {
  my ($self, $el, $strict) = @_;
  my $doc = eval { $el->top } or return 0;
  my $key = $strict ? 'strict' : 'loose';
  return $self->{_class_feature_seen}{$key}{ refaddr $doc } //= do {
    my $seen = 0;
    for my $st (@{ $doc->find('PPI::Statement') || [] }) {
      if ($st->isa('PPI::Statement::Include')) {
        my $c = $st->content // '';
        if ($c =~ /\b(?:feature|experimental)\b[^;]*['"]class['"]/
            || (!$strict
                && $c =~ /^\s*(?:use|require)\s+v?5\.0*(\d+)/ && $1 >= 38)) {
          $seen = 1; last;
        }
        next;
      }
      my $w = $st->schild(0) or next;
      next unless $w->isa('PPI::Token::Word') && $w->content eq 'class';
      my $n = $w->snext_sibling;
      next unless $n && $n->isa('PPI::Token::Word');
      if (!$strict) { $seen = 1; last }
      # Strict: the BLOCK form only.  `class NAME { … }` is Word Word Block.
      my $b = $n->snext_sibling;
      if ($b && $b->isa('PPI::Structure::Block')) { $seen = 1; last }
    }
    $seen;
  };
}

# `class NAME ;` — the statement form of perl 5.38's object syntax — parses as
# indirect-object notation, i.e. `NAME->class`, in PPI and in PCL and in PERL
# ITSELF when the feature is off (probed: perl dies "Can't locate object method
# \"class\" via package \"Foo\"").  So the reading is RIGHT by default and must
# not change.  It is wrong only for a file that has actually switched the
# feature on, and there PCL would silently run a method call where the author
# declared a class.  Refuse those, perl-shaped, and leave every other file
# exactly as it was (RULED s416 §7.5; `docs/not-supported.md`).
#
# Returns the refusal text, or undef to lower the statement as usual.
sub class_statement_refusal {
  my ($self, $stmt) = @_;
  return undef unless ref $stmt && eval { $stmt->isa('PPI::Statement') };
  return undef if $stmt->isa('PPI::Statement::Include');
  my $w = $stmt->schild(0)                                  or return undef;
  return undef unless $w->isa('PPI::Token::Word') && $w->content eq 'class';
  my $n = $w->snext_sibling                                 or return undef;
  return undef unless $n->isa('PPI::Token::Word');
  # A BLOCK after the name is the block form, which is a different statement
  # and not what this refusal is keyed on.
  my $after = $n->snext_sibling;
  return undef if $after && $after->isa('PPI::Structure::Block');
  return undef unless $self->_class_feature_in_scope($stmt, 1);
  return "feature 'class' is not supported";
}

# A `~~` that has a TERM to its left is the smart match; one that has an
# operator, a comma or nothing to its left is the prefix double complement.
# The left side is a WHITELIST on purpose (see the header): an unrecognised
# neighbour keeps today's drop.
# A `~~` token that reaches a DROP is the smart match, and this asks nothing
# else — because the infix/prefix question is already answered, once, upstream:
# `Parser2::_repair_term_initial_complement` (task #370) splits every `~~` that
# is NOT after a term into two `~` complements before the parse, using
# `_ends_term`.  So anything still spelled `~~` here survived that repair and
# is after a term.  Asking the question a second time here, with a second
# predicate, is exactly the drift CLAUDE.md rule 11 is about — an earlier cut
# of this file had a hand-rolled whitelist that missed `$o->method ~~ 1`,
# which `_ends_term` gets right.
sub _has_infix_smartmatch {
  my ($el) = @_;
  for my $e (@$el) {
    my @ops = $e->isa('PPI::Token::Operator')
            ? ($e) : @{ $e->isa('PPI::Node') ? ($e->find('PPI::Token::Operator') || []) : [] };
    return 1 if grep { $_->content eq '~~' } @ops;
  }
  return 0;
}

# The refusal text for a dropped statement, or undef to keep dropping.
# @$src is the announcer's own view of what was lost -- one list, one place.
sub _ruled_refusal_for_drop {
  my ($self, $src) = @_;
  my @el = grep { ref $_ && $_->isa('PPI::Element') } @{ $src // [] };
  return unless @el;
  my @p   = _drop_lead_parts($src);
  my $w   = @p && $p[0]->isa('PPI::Token::Word') ? $p[0]->content : '';
  my $nx  = $p[1];

  # given / when / default.  PPI has statement classes for the two block
  # forms; `CORE::given` and a lowered token run arrive as words, and there
  # the following List/Block is what makes the word the keyword.
  return "given/when (feature 'switch') is not supported"
       . " -- removed in perl 5.42"
    if $el[0]->isa('PPI::Statement::Given')
    || $el[0]->isa('PPI::Statement::When')
    || ($w =~ /^(?:CORE::)?(?:given|when|default)$/
        && $nx && ($nx->isa('PPI::Structure::List')
                || $nx->isa('PPI::Structure::Block')));

  # class / field / method / ADJUST (perl 5.38 object syntax).
  # `class NAME` carries its own evidence (a call would be followed by a List,
  # a fat-comma key by an Operator).  The other three are ordinary identifiers
  # in every other Perl, so THE FILE has to say the feature is on — and that
  # is the whole guard: no shape test rides along, because PPI does not know
  # the feature either and lexes `method m { 1 }` as Word + a MATCH operator
  # (`m { 1 }`), which is correct for a file without it.
  return "feature 'class' is not supported"
    if ($w eq 'class' && $nx && $nx->isa('PPI::Token::Word'))
    || ($w =~ /^(?:field|method|ADJUST)$/
        && $self->_class_feature_in_scope($el[0]));

  # format NAME = ... .   (`format` alone takes STDOUT.)
  return "format/write report formatting is not supported"
    if $w eq 'format'
    && $nx && (($nx->isa('PPI::Token::Word') && $p[2]
                && $p[2]->isa('PPI::Token::Operator') && $p[2]->content eq '=')
            || ($nx->isa('PPI::Token::Operator') && $nx->content eq '='));

  # defer { ... } (perl 5.36).
  return "defer blocks are not supported"
    if $w eq 'defer' && $nx && $nx->isa('PPI::Structure::Block');

  return "smart match (~~) is not supported -- removed in perl 5.42"
    if _has_infix_smartmatch(\@el);

  return;
}

# THE DROP ANNOUNCEMENT (task #339, ruled fable-answers-s400.md §6.2).
# THE DROP SITE — file, line, source text — built in ONE place, because two
# things must name the SAME statement: the transpile-time announcement below,
# and the RUN-TIME die the two PARSE ERROR emitters now emit (rule 11).  The
# @src elements come back too: the ruled-refusal classifier reads them.
sub _drop_site {
  my ($self, $parts, $stmt) = @_;
  my $file = $self->has_filename ? $self->filename : '-';
  # The statement is the best source text when the caller has it; otherwise the
  # token list it was called with is exactly what could not be lowered.
  my @src = (ref($stmt) && eval { $stmt->isa('PPI::Element') })
          ? ($stmt)
          : grep { ref($_) && eval { $_->isa('PPI::Element') } }
                 (ref($parts) eq 'ARRAY' ? @$parts : ());
  my $line = 0;
  for my $s (@src) { $line = $s->line_number // 0; last if $line }
  my $text = join '', map { $_->content // '' } @src;
  $text =~ s/\s+/ /g;
  $text =~ s/^ //; $text =~ s/ $//;
  $text = substr($text, 0, 120) . '...' if length($text) > 123;
  $text = '(no source text)' unless length $text;
  return ($file, $line, $text, \@src);
}

# THE DROP FORM — the announce->DIE flip (Option B phase 2's last step; ruled
# docs/fable-answers-s433.md §A.1, executed s435).
#
# A statement the compiler cannot lower is still REPLACED, and the
# `;; PARSE ERROR: <reason>` comment stays BYTE-FOR-BYTE — it is what
# tools/drop-census.pl, tools/corpus-diff.pl's SILENT-DROP counter and both
# runners' `drops` column read, so the census stays the gap-finder and the
# gate.  What takes the statement's place is no longer `nil`:
#
#   (progn ;; PARSE ERROR: <reason>
#     (pcl:p-die "PCL: statement not supported at F line N: <text> -- <reason>\n"))
#
# ONE shape for every drop in every mode — no exempt/registered/deliberate/gap
# classifier here.  That distinction is the CENSUS's (it owns the reasons and
# their owners), and a classifier in the emitter would be asymmetric in the
# dangerous direction: a MISS on a registered row would kill a whole file.
#
# The unit is the STATEMENT, which is what makes the flip affordable: every row
# before it still runs, a program that never reaches the statement is
# unaffected, and one that does gets perl's own `die` — trappable in `$@`,
# assertable in a test row.  Module mode is covered by construction: the die is
# IN the emission, so it survives the module cache and needs no announcement
# (`pl2cl --module` still says nothing, ruled s403 — the statement now says it
# itself, when it is reached).
#
# The message ends in "\n" so p-die does NOT append its " at FILE line N."
# suffix: the location is already in the text, and it is the PERL file's line,
# not the generated CL's.  The text goes through the string-literal escaper
# (it carries quotes, backslashes and, in the uni/ corpus, characters CL cannot
# spell bare).
sub _dropped_statement_cl {
  my ($self, $parts, $stmt, $error) = @_;
  my ($file, $line, $text) = $self->_drop_site($parts, $stmt);
  my $msg = "PCL: statement not supported at $file line $line: $text -- $error\n";
  return "(progn ;; PARSE ERROR: $error\n "
       . "(pcl:p-die " . Pl::ExprToCL::cl_string_literal($msg) . "))";
}

# Both PARSE ERROR emitters above replace a whole statement.  They USED to
# replace it with `nil` and let the program run on — the #138 family, and the
# worst failure mode in this codebase, because it was SILENT: perl-tests/
# bless.t dropped a test row that then appeared in no count, in a file the
# sweep reported as passing.  Since s435 the replacement is a `p-die`
# (`_dropped_statement_cl` above), so the statement is loud twice: when the
# file is COMPILED, by the announcement below, and when it is REACHED, by the
# die.  This is what the announcement says, once, on stderr:
#
#   PCL: statement dropped at FILE line N: <source text> -- <reason>
#
# FIXED PREFIX: runners and tools/gate-set-scan.pl key on it, so do not
# reword it.  (The ruling spells the separator as an em dash; it is ASCII `--`
# here because pl2cl does `binmode(STDERR, ":utf8")`, so a raw UTF-8 em dash in
# the source would be DOUBLE-encoded on the way out, and a `\x{2014}` character
# would warn "Wide character" under any entry point without that layer.  Every
# other diagnostic in this compiler is ASCII for the same reason.)  It is a
# TRANSPILE-time diagnostic and pl2cl's exit status stays 0 — the RUN-time
# half of a drop is the emitted die, which is a separate event with its own
# wording (`statement not supported`, not `statement dropped`), deliberately:
# an uncaught die prints to stderr too, and one shared verb would let a
# run-time death be miscounted as a transpile announcement by the runners that
# key on this prefix.  In eval-string mode the transpile runs in the `pl2cl --server`
# subprocess, whose stderr the runtime discards (`:error nil`), so this line
# is a file-mode diagnostic in practice.
#
# Nothing is said at the DECLINE site (Pl/PExpr.pm's "Handle single node of
# unknown type" die): a decline is not an event — the term walker declines by
# design and callers re-route.
# OFF in `pl2cl --module` (set there): that mode is the RUNTIME transpiling a
# module while a program runs, so the line would land in the PROGRAM's stderr —
# and only on a cold module cache, which makes it nondeterministic output as
# well as noise.  Module drops are still counted, by tools/drop-census.pl and
# tools/corpus-diff.pl, which read the emitted CL.  `PCL_DROP_ANNOUNCE=all`
# forces them on, which is how you see a drop inside a CPAN module
# (Data::Dump.pm has one).
our $ANNOUNCE_DROPS = 1;
my %announced_drop;   # "file:line:text" seen this process — announce ONCE
my %logged_drop;      # same key, for the PCL_DROP_LOG side channel (#472)

# THE SIDE CHANNEL (task #472, ruled fable-answers-s437 §2 ask 5).  A program
# run by fresh_perl_is/runperl is transpiled from a STRING at run time
# (tools/pclperl-for-tests), so its emission is never a .lisp FILE and no
# instrument the project has can see a drop in it — tools/drop-census.pl,
# tools/corpus-diff.pl's SILENT-DROP counter and both runners' `drops` column
# all count by reading emitted CL.  Two such drops are known, and BOTH were
# rows that had been passing for years on nothing (perl-tests/split.t:682,
# perl-tests/bop.t:701, exposed by the s435 flip); the size of the population
# was unknown until this arm.
#
# WHY A FILE AND NOT STDERR: the child's stderr IS the row's observed output
# — fresh_perl_is compares it — so routing the announcement there would change
# verdicts, which is the one thing an instrument may not do.  The caller names
# the file in PCL_DROP_LOG; every drop appends one line
# `FILE<TAB>LINE<TAB>TEXT<TAB>REASON`, append-mode so parallel children on one
# file cannot truncate each other.
#
# WHAT IT COUNTS, exactly: the same thing the census counts — a statement lost
# from an EMITTED program.  So it is deduplicated per statement like the
# announcement (a statement can reach an emitter twice), a ruled refusal is not
# a drop (the file is refused, loudly, in every mode), and an eval-string drop
# is not one either: it DIES at transpile (#363) and emits nothing, so it never
# appears in the census's population and must not appear in this one.
sub _log_dropped_statement {
  my ($file, $line, $text, $reason) = @_;
  my $path = $ENV{PCL_DROP_LOG};
  return if !defined $path || !length $path;
  return if $logged_drop{"$file:$line:$text"}++;
  # Never let the instrument break the transpile it is measuring.
  open(my $lh, '>>', $path) or return;
  print $lh join("\t", $file, $line, $text, $reason), "\n";
  close $lh;
  return;
}

sub _announce_dropped_statement {
  my ($self, $parts, $stmt, $reason) = @_;
  # Prototype-extraction parses throw their output away (_emit is a no-op), so
  # a drop there costs the program nothing and would only double the line.
  return if $self->collect_prototypes_only;

  # ONE builder for the site (below): this line, the emitted die and the
  # PCL_DROP_LOG record must name the same statement, or the halves of a drop
  # read as separate events.
  my ($file, $line, $text, $src) = $self->_drop_site($parts, $stmt);
  my @src = @$src;
  # Computed here rather than at its own site below because the log arm must
  # not record a refusal as a drop; the DIE it produces stays below the stderr
  # gate, so module mode is as silent as it was (ruled s403).
  my $refusal = $self->_ruled_refusal_for_drop(\@src);

  # UNGATED BY `PCL_DROP_ANNOUNCE` (#472): the side channel is a measurement,
  # not a diagnostic, and it must count module-mode drops too.
  _log_dropped_statement($file, $line, $text, $reason)
    if !$refusal && !$self->eval_mode;

  # (The eval-mode DIE below is decided before this gate, deliberately: what
  # `PCL_DROP_ANNOUNCE` controls is a diagnostic, not whether a statement may
  # vanish from a program.)
  return if !$self->eval_mode
         && !($ANNOUNCE_DROPS || ($ENV{PCL_DROP_ANNOUNCE} // '') eq 'all');

  # A RULED REFUSAL COMES FIRST (Track A, #371): for the five families above
  # this statement is not a compiler gap but a feature PCL does not have, and
  # the honest answer is perl's -- refuse the file, in every mode, rather than
  # run a program the author did not write.  The location is spelled like the
  # announcement's so the two read alike; the message keeps its `PCL:` prefix
  # so that in eval-string mode it travels the ruled refusal route into $@.
  if ($refusal) {
    my $where = $self->eval_mode ? '(eval)' : $file;
    die "PCL: $refusal, at $where line $line\n";
  }
  # ONCE per statement: a statement can reach an emitter twice (the v2 seam
  # tries the form entry, a block body is lowered inside an outer lowering),
  # and op/switch.t measured 138 events for 112 emitted drops.  The COUNT of
  # drops is the runners' `drops` column (task #343), which reads the emitted
  # CL; this line is the identity of the statement that was lost.
  # #363: IN EVAL-STRING MODE A DROP DIES.  perl's contract for `eval STRING`
  # is "what does not compile sets $@", and here nothing else can say so: the
  # runtime starts `pl2cl --server` with :error nil, so the line below goes
  # NOWHERE and the statement simply disappears from the program (measured:
  # `eval q{ f ref $u, "m" or g "fb"; 7 }` returned 7 with $@ empty and the
  # call gone).  The die travels the route the ruled `PCL: unsupported in
  # string eval:` refusals already take — the server answers "err", p-eval
  # turns that into $@ — which is why the text keeps the `PCL:` prefix.
  #
  # This is Option B phase 2's announce->DIE step (fable-answers-s400 §6.4)
  # taken EARLY, and only for the path that cannot announce.  FILE mode is
  # untouched: it still announces and exits 0 until phase 2 flips it too.
  #
  # NOT deduplicated (the dedupe below is for a repeated diagnostic): the
  # first drop in an eval ends that eval.
  die "PCL: statement dropped at (eval) line $line: $text -- $reason\n"
    if $self->eval_mode;
  return if $announced_drop{"$file:$line:$text"}++;
  print STDERR "PCL: statement dropped at $file line $line: $text -- $reason\n";
  return;
}


# E2.final root flip (task #78): identical parse + context annotation to
# _parse_expression_internal, but generation goes through gen_node_form, so
# the caller (Parser2's _lower_expr fallback seam) receives a CLForm TREE
# whose raw residue is only the genuinely-declining subtrees — not the whole
# expression as one text atom.  Error semantics mirror the text entry
# exactly; the PARSE ERROR / no-output shapes come back as raw chunks.
sub _parse_expression_form {
  my ($self, $parts, $stmt, $context, %opt) = @_;
  # %opt: sub_info + lexicals — the two FACTS ExprToCL's Kind-A rules read
  # (Phase A: they used to reach only ExprToCL2's native attempt).

  my $form;
  eval {
    my $expr_o = $self->_expr_parser($parts, $stmt);
    my ($node_id) = $expr_o->parse_expr_to_tree($parts);
    $expr_o->annotate_contexts($node_id, $context);
    my $gen = $self->_expr_generator($expr_o, %opt);
    $form = $gen->gen_node_form($node_id);
  };

  if ($@) {
    my $error = $self->_shape_expr_error($@);
    $self->_announce_dropped_statement($parts, $stmt, $error);
    return Pl::CLForm::raw($self->_dropped_statement_cl($parts, $stmt, $error));
  }

  return $form // Pl::CLForm::raw(";; (no output)");
}


# Emit a line to output
sub _emit {
  my $self = shift;
  my $line = shift;

  # Don't emit if we're just extracting prototypes
  return if $self->collect_prototypes_only;

  # For let-bound 'my' variables, replace (p-scalar-= $var ...) with
  # (p-my-= $var ...) to avoid p-scalar-='s (proclaim 'special) side-effect.
  # proclaim at runtime contaminates future compilations: the next time code
  # using the same name is compiled, the let creates a dynamic binding instead
  # of a lexical one, breaking closure capture.
  # p-my-= is a semantic macro (expands to box-set) that expresses intent for
  # other compiler backends reading the generated IR.
  if ($line && ($self->lex_home->{_let_bound_vars} || $self->{_sig_param_lexicals})) {
    my %lex = (%{$self->lex_home->{_let_bound_vars} // {}},
               %{$self->{_sig_param_lexicals} // {}});
    for my $var (keys %lex) {
      my $pat = quotemeta("(p-scalar-= $var");
      $line =~ s/$pat(?=[\s)])/(p-my-= $var/g;
    }
  }

  my $indent = "  " x $self->indent_level;
  my $section = $self->_sections->[$self->_cur_section];
  push @{$section->{$self->_cur_bucket}}, $indent . $line;
}


# (The parse_file/parse_code convenience class methods — v1's external
# two-pass transpile API — were deleted at E4.1 step 3: their last callers,
# 27 Pl/t files, were ported to Pl::Parser2 in s358 (#255), and parse()
# above now guards against full-emission use.)


# ============================================================
# Parse a subroutine prototype or signature string.
#
# Input: prototype string like "($x, $y = 10, @rest)" or "($$;$)"
# Returns: {
#   params     => [ { name => '$x', default_cl => undef },
#                   { name => '$y', default_cl => '10' },
#                   { name => '@rest', default_cl => undef } ],
#   min_params => 1,   # minimum required parameters
#   is_proto   => 0,   # 1 if old-style prototype, 0 if signature
# }
#
# This is a separate sub so it can be moved to its own module later.
# ============================================================
sub parse_prototype_or_signature {
  my $self      = shift;
  my $proto_str = shift;
  my $context   = shift;  # PPI context for parsing defaults (e.g., the sub statement)

  # Remove surrounding parens and whitespace
  $proto_str =~ s/^\s*\(\s*//;
  $proto_str =~ s/\s*\)\s*$//;

  return { params => [], min_params => 0, is_proto => 0 } if $proto_str eq '';

  # Detect if this is an old-style prototype (no variable names, just sigils)
  # Old-style: ($$), (\@$), ($;$@)
  # New-style: ($x, $y), ($x = 10)
  my $is_proto = ($proto_str !~ /[\$\@\%]\w/);

  if ($is_proto) {
    return $self->_parse_old_prototype($proto_str);
  } else {
    return $self->_parse_signature($proto_str, $context);
  }
}


# Parse old-style prototype like "$$", "\@$", "$;$$"
sub _parse_old_prototype {
  my $self      = shift;
  my $proto_str = shift;

  my @params;
  my $min_params = 0;
  my $in_optional = 0;
  my $param_idx = 0;  # Counter for unique parameter names

  # Split into characters, handling backslash escapes
  my $i = 0;
  while ($i < length($proto_str)) {
    my $char = substr($proto_str, $i, 1);

    if ($char eq ';') {
      # Semicolon marks start of optional parameters
      $in_optional = 1;
      $i++;
      next;
    }
    elsif ($char eq '\\') {
      # Reference type: \@, \$, \%, \*
      my $next = substr($proto_str, $i + 1, 1);
      my $name = '$_proto_arg' . $param_idx++;
      push @params, {
        name => $name,
        default_cl => undef,
        proto_type => "\\$next"  # Preserve original for auto-boxing
      };
      $min_params++ unless $in_optional;
      $i += 2;
    }
    elsif ($char =~ /[\$\@\%\&\*_]/) {
      # Generate unique name with appropriate sigil
      my $sigil = ($char eq '@' || $char eq '%') ? $char : '$';
      my $name = $sigil . '_proto_arg' . $param_idx++;
      push @params, {
        name => $name,
        default_cl => undef,
        proto_type => $char  # Preserve original for special handling
      };
      $min_params++ unless $in_optional || $char eq '@' || $char eq '%';
      $i++;
    }
    else {
      # Skip unknown/whitespace
      $i++;
    }
  }

  # Check if prototype has & (block argument)
  my $has_block_arg = ($proto_str =~ /&/);

  return {
    params        => \@params,
    min_params    => $min_params,
    is_proto      => 1,
    has_block_arg => $has_block_arg,
    proto_string  => $proto_str,
  };
}


# Split a signature string into its parameters, PURELY TEXTUALLY: no CL is
# compiled and no environment state is touched.  Returns
#   [ { name => '$y', default_op => '=' | '//=' | '||=', default_expr => '10' }, ... ]
# with default_expr undef for a parameter that has no default.
#
# Two consumers share this: _parse_signature (named subs — compiles each
# default to CL) and _desugar_anon_signatures (anonymous subs — rewrites the
# signature back into Perl source).  Keeping the shape analysis here means the
# two agree on placeholder naming and on which `=`-variant a default uses.
# A signature's text may legally contain comments, newlines, spaced sigils
# (`$ #cmt \n a` is the parameter $a) and repeated commas (op/signatures.t
# t086/t087).  Normalize ONCE here, at the shared spec parser, so both the
# named and anon lowerings see clean text: PPI-tokenize (string literals in
# default expressions survive verbatim as single tokens), drop comments,
# collapse whitespace, and merge a parameter-position sigil onto its name
# (semantics-preserving anywhere: `$ y` IS $y).  Fast path: untouched unless
# a comment or a spaced sigil is present.
sub _normalize_signature_text {
  my ($self, $text) = @_;
  return $text unless $text =~ /#/ || $text =~ /[\$\@\%]\s/;
  my $doc = PPI::Document->new(\$text) or return $text;
  my $out = '';
  for my $tok ($doc->tokens) {
    next if $tok->isa('PPI::Token::Comment');
    $out .= $tok->isa('PPI::Token::Whitespace') ? ' ' : $tok->content;
  }
  $out =~ s/ {2,}/ /g;
  $out =~ s/(^|,)\s*([\$\@\%])\s+(?=\w)/$1$2/g;
  return $out;
}

# True when a signature-default text contains, at paren/bracket depth 0 and
# outside quotes, a PARENLESS call to a known non-prototyped sub.  Such a
# call is a LIST OPERATOR: perl parses everything to its right — commas and
# all — as its argument list, so the "params" after it are really part of
# this default (`sub t017 ($p = t018 222, $a = 333)` has ONE param;
# op/signatures.t t017).  Old-style prototypes limit the call's arity at
# parse time, so those subs do not swallow.
my %SIG_DEFAULT_KEYWORDS = map { $_ => 1 }
  qw(do state my our local sub undef defined not and or xor eq ne lt gt
     le ge cmp x if unless while until for foreach return);
sub _sig_default_swallows {
  my ($self, $text) = @_;
  # Blank quoted spans and nested groups so only depth-0 unquoted barewords
  # are considered.
  my ($depth, $q, $masked) = (0, '', '');
  for my $c (split //, $text) {
    if ($q)               { $q = '' if $c eq $q; $masked .= ' '; next }
    if ($c =~ /["']/)     { $q = $c; $masked .= ' '; next }
    if ($c =~ /[\(\[\{]/) {
      # keep a depth-0 opener visible: `f(...)` must NOT read as parenless
      $masked .= $depth == 0 ? $c : ' ';
      $depth++;
      next;
    }
    if ($c =~ /[\)\]\}]/) { $depth--; $masked .= ' '; next }
    $masked .= $depth == 0 ? $c : ' ';
  }
  while ($masked =~ /(?<![\$\@\%&>:\w])([a-zA-Z_]\w*)\b(?!\s*(?:\(|=>|::))/g) {
    my $w = $1;
    next if $SIG_DEFAULT_KEYWORDS{$w};
    my $proto = $self->environment ? $self->environment->get_prototype($w)
                                   : undef;
    return 1 if $proto && !$proto->{is_proto};
  }
  return 0;
}

sub _signature_param_specs {
  my $self    = shift;
  my $sig_str = shift;

  $sig_str = $self->_normalize_signature_text($sig_str);

  my @specs;
  my $anon_counter = 0;

  my @segs = $self->_split_signature_params($sig_str);
  # Merge trailing segments into a default that ends in a parenless
  # list-op call (see _sig_default_swallows).
  for my $i (0 .. $#segs - 1) {
    if ($segs[$i] =~ m{(?://=|\|\|=|=)\s*(.+)$}s
        && $self->_sig_default_swallows($1)) {
      splice @segs, $i, scalar(@segs) - $i, join(', ', @segs[$i .. $#segs]);
      last;
    }
  }

  for my $param_str (@segs) {
    $param_str =~ s/^\s+//;
    $param_str =~ s/\s+$//;
    next if $param_str eq '';

    my ($name, $default_expr);
    my $default_op = '=';   # '=' | '//=' | '||=' — when the default applies

    if ($param_str =~ m{^([\$\@\%]\w+)\s*(//=|\|\|=|=)\s*(.+)$}) {
      # Parameter with default: $x = 10, $x //= 10 (apply on absent/undef),
      # $x ||= 10 (apply on absent/false).  Perl 5.38+ allows //= and ||=.
      ($name, $default_op, $default_expr) = ($1, $2, $3);
    }
    elsif ($param_str =~ /^([\$\@\%]\w+)$/) {
      # Simple parameter: $x
      $name = $1;
    }
    elsif ($param_str =~ /^([\$\@\%])\s*=\s*(.*)$/) {
      # Anonymous placeholder with default: ($ = undef), ($ =)
      # Still counts toward arity; bound to a throwaway name.
      $name = $1 . '_sig_anon' . (++$anon_counter);
      my $rhs = $2;
      $rhs =~ s/\s+$//;
      $default_expr = ($rhs eq '') ? 'undef' : $rhs;
    }
    elsif ($param_str =~ /^([\$\@\%])$/) {
      # Anonymous mandatory placeholder: ($a, $) — the bare $ is a required slot.
      $name = $1 . '_sig_anon' . (++$anon_counter);
    }
    else {
      # Unknown format, skip
      next;
    }

    push @specs, { name => $name, default_op => $default_op,
                   default_expr => $default_expr };
  }

  return \@specs;
}


# True iff every `(` in $str has a matching `)` (plain count, no string
# awareness — inputs are the short default-expression texts of a signature).
sub _paren_balanced {
  my ($str) = @_;
  my $depth = 0;
  for my $c (split //, $str) {
    $depth++ if $c eq '(';
    if ($c eq ')') { $depth--; return 0 if $depth < 0 }
  }
  return $depth == 0;
}

# Longest prefix of a `state $v = INIT` init text that belongs to the decl:
# stops before an unmatched `)` or `}` (a closer owned by an enclosing paren
# group or do-block, not by the init expression).
sub _init_prefix {
  my ($str) = @_;
  my $depth = 0;
  my $i = 0;
  for my $c (split //, $str) {
    if    ($c eq '(' || $c eq '{') { $depth++ }
    elsif ($c eq ')' || $c eq '}') { last if $depth == 0; $depth-- }
    $i++;
  }
  return substr($str, 0, $i);
}

# Parse new-style signature like "$x, $y = 10, @rest"
sub _parse_signature {
  my $self      = shift;
  my $sig_str   = shift;
  my $context   = shift;

  my @params;
  my $min_params = 0;
  my $seen_optional = 0;

  for my $spec (@{ $self->_signature_param_specs($sig_str) }) {
    my $name         = $spec->{name};
    my $default_op   = $spec->{default_op};
    my $default_expr = $spec->{default_expr};
    $seen_optional = 1 if defined $default_expr;

    # A `local $G = RHS` default localises $G for the sub's dynamic extent (and
    # the param's value is RHS).  PExpr would drop the `local` in expression
    # position (clobbering $G permanently), so peel it off here: compile only the
    # RHS as the default value and record the localised var for a body wrapper.
    # See docs/variable-declarations-spec.md §4.2.
    my $local_var;
    if (defined $default_expr
        && $default_expr =~ /^\s*\(?\s*local\s+(\$\w+)\s*=\s*(.+?)\s*\)?\s*$/s) {
      $local_var    = $1;
      $default_expr = $2;
    }

    # `our $VAR` inside a default declares a package global.  PExpr drops the
    # `our` keyword in expression position (so `(our $k)++` compiles to
    # `(p-post++ $k)`), but without an explicit declaration $VAR is never
    # defvar'd → unbound at runtime.  Register + emit the defvar here, mirroring
    # _process_our_declaration; the default expression keeps referencing $VAR.
    # See docs/variable-declarations-spec.md §4.3.
    if (defined $default_expr && $default_expr =~ /\bour\b/) {
      my $pkg = $self->environment->current_package;
      while ($default_expr =~ /\bour\s+([\$\@\%]\w+)/g) {
        my $ovar  = $1;
        my $sigil = substr($ovar, 0, 1);
        my $init  = $sigil eq '$' ? '(make-p-box nil)'
                  : $sigil eq '@' ? '(make-array 0 :adjustable t :fill-pointer 0)'
                  :                 '(make-hash-table :test #\'equal)';
        $self->environment->add_our_variable($pkg, $ovar);
        $self->_with_bucket('declarations', sub {
          $self->_emit("(p-eval-always " . global_decl_form("$ovar", "$init") . ")");
        });
      }
    }

    # `state $v = INIT` inside a default runs its init ONCE (the first call
    # that hits the default) and evaluates to $v's current value thereafter
    # (op/signatures.t t126/t127).  The statement-level state machinery never
    # sees a default expression, so peel the decls here: each becomes a
    # defvar'd per-sub cell + __init flag box, the decl in the source is
    # replaced by the plain variable (renamed to the cell for the extent of
    # the compile), and a once-guard per init is hoisted in front of the
    # compiled default.  The flag is a p-box tested with Perl truthiness
    # (p-!) — the forward-decl scans declare every name they see in emitted
    # text as `(make-p-box nil)`, and this way that declaration is correct
    # (v2 declares expression-position state cells the same way).  Cell
    # names are DETERMINISTIC (pkg + sub name): a signatured sub is parsed
    # twice — call-site registration and the v1 definition lowering — and
    # both parses must agree on the cell.  Scalar state only; the cell is
    # not visible to the sub body (nothing in the suite references it there).
    my $default_src = $default_expr;
    my (%sig_state_renames, @sig_state_inits);
    if (defined $default_src && $default_src =~ /\bstate\s+\$\w/) {
      my $sub_name = ($context && eval { $context->isa('PPI::Statement::Sub') })
                   ? ($context->name // '') : '';
      my $pkg = $self->environment->current_package // 'main';
      # `(state $v = ...)` whole-default: strip the balanced outer parens so
      # the init capture below never swallows the closing one.
      if ($default_src =~ /^\s*\((.*)\)\s*$/s
          && _paren_balanced($1)) {
        $default_src = $1;
      }
      $default_src =~ s{\bstate\s+(\$\w+)(?:\s*=\s*([^;]*))?}{
        my ($sv, $init) = ($1, $2);
        my $tail = '';
        if (defined $init) {
          # Keep only the part of INIT belonging to the decl: stop at an
          # unmatched paren/brace closer — the decl may sit inside a paren
          # group or a do-block whose closer is not ours to consume.
          my $keep = _init_prefix($init);
          $tail = substr($init, length $keep);
          $init = $keep;
          $init =~ s/\s+$//;
          push @sig_state_inits, [$sv, $init] if $init ne '';
        }
        if (!$sig_state_renames{$sv}) {
          (my $bare = $sv) =~ s/^\$//;
          (my $slug = "${pkg}_${sub_name}") =~ s/[^a-zA-Z0-9]/_/g;
          $slug .= '_' . ++$state_var_counter if $sub_name eq '';
          my $cell = "\$${bare}__state_sig__${slug}";
          $sig_state_renames{$sv} = $cell;
          # v1's forward-decl scan reads perl SOURCE, so it never sees these
          # generated names — declare them here.  Under v2 its text scan also
          # declares them; the duplicate defvars are identical no-ops.
          $self->_with_bucket('declarations', sub {
            $self->_emit(global_decl_form("$cell", "(make-p-box nil)"));
            $self->_emit(global_decl_form("${cell}__init", "(make-p-box nil)"));
          });
        }
        # Substitute the cell name directly: the decl statement's value is
        # the variable itself, and this keeps it correct even on paths that
        # skip the rename lookup (lone-symbol statement in a do-block).
        $sig_state_renames{$sv} . $tail;
      }ge;
    }

    my $default_cl = undef;
    if (defined $default_src) {
      # Compile the default expression to CL
      if (%sig_state_renames) {
        my $saved = $self->environment->state_var_renames // {};
        $self->environment->state_var_renames({ %$saved, %sig_state_renames });
        my @guards;
        for my $st (@sig_state_inits) {
          my ($sv, $init_src) = @$st;
          my $cell    = $sig_state_renames{$sv};
          my $init_cl = $self->_compile_default_expr($init_src, $context);
          next unless defined $init_cl;
          push @guards, "(p-if (p-! ${cell}__init) "
                      . "(progn (box-set $cell $init_cl) "
                      . "(p-scalar-= ${cell}__init 1)))";
        }
        my $rest_cl = $self->_compile_default_expr($default_src, $context);
        $self->environment->state_var_renames($saved);
        if (defined $rest_cl) {
          $default_cl = @guards
            ? '(progn ' . join(' ', @guards) . " $rest_cl)"
            : $rest_cl;
        }
      }
      else {
        $default_cl = $self->_compile_default_expr($default_src, $context);
      }
    }

    push @params, {
      name       => $name,
      default_cl => $default_cl,
      default_op => $default_op,
      local_var  => $local_var,
    };

    # Count mandatory params (before any optional, and not slurpy)
    if (!$seen_optional && !defined $default_expr && $name !~ /^[\@\%]/) {
      $min_params++;
    }
  }

  return {
    params     => \@params,
    min_params => $min_params,
    is_proto   => 0,
  };
}


# Split signature string on commas, respecting nested parens
sub _split_signature_params {
  my $self = shift;
  my $str  = shift;

  my @result;
  my $current = '';
  my $depth = 0;

  for my $char (split //, $str) {
    if ($char eq ',' && $depth == 0) {
      push @result, $current;
      $current = '';
    }
    else {
      $depth++ if $char eq '(' || $char eq '[' || $char eq '{';
      $depth-- if $char eq ')' || $char eq ']' || $char eq '}';
      $current .= $char;
    }
  }
  push @result, $current if $current ne '';

  return @result;
}


# Compile a default expression to CL
sub _compile_default_expr {
  my $self    = shift;
  my $expr    = shift;
  my $context = shift;

  my $result;
  eval {
    my $doc = PPI::Document->new(\$expr);
    my @stmts = $doc->children;
    return undef unless @stmts;

    my @parts = grep {
      ref($_) ne 'PPI::Token::Whitespace'
    } $stmts[0]->children;

    return undef unless @parts;

    my $expr_o = $self->_expr_parser(\@parts, $doc);

    my $node_id = $expr_o->parse_expr_to_tree(\@parts);

    my $gen = $self->_expr_generator($expr_o);

    $result = $gen->generate($node_id);
  };

  if ($@) {
    die $@ if $@ =~ /^PCL:/;
    warn "Failed to compile default expression '$expr': $@";
    return undef;
  }

  return $result;
}


1;

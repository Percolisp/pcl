# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::RegexTier;
# THE REGEX TIER CLASSIFIER (task #1211; docs/js-target-plan.md §II.8 item 3,
# settled s460f; docs/plan-speed-and-ir-s470.md Part B item B5).
#
# ONE PURE FUNCTION over the pattern text and the modifier letters, answering
# which class of regex ENGINE a target needs for this literal:
#
#   native    a rewrite into the host's own regex engine expresses it
#             (JavaScript `RegExp` at ES2018+, which has named captures,
#             lookbehind, `\p{…}` under /u, dotall and sticky)
#   pcre      the construct exists in perl and PCRE2 but not in a plain
#             host engine — a PCRE2 binding (native or WASM) is required
#   refused   perl code inside the pattern: `(?{…})` / `(??{…})`.  No engine
#             runs it; the CL target already declines it
#             (docs/not-supported.md "Regex code blocks") and every other
#             target refuses it the same way (ir-spec §9.3b)
#   dynamic   the pattern is not known at compile time (it interpolates), so
#             the tier is a RUN-TIME question for the target.  A DECLARED
#             absence, never a guess — this is the answer for
#             `p-regex-from-parts`
#
# WHAT IT IS FOR, AND WHAT IT IS NOT.  PCL's own CL target IGNORES the tier:
# cl-ppcre runs everything cl-ppcre runs, and the emitted `:tier` keyword is
# never read at run time.  The tier is a FACT for OTHER targets, carried in the
# IR so a backend author does not re-scan Perl source to learn which of their
# literals need the heavy engine (plan §B.3).  Being advisory is also why an
# UNRECOGNISED modifier letter answers `pcre` instead of dying: rule 12's
# boundary (DECIDED s329) is "die when the missing case produces a value the
# program consumes", and nothing here reaches the program.  `pcre` is the
# conservative answer — "this target needs the bigger engine" — and a wrong
# `native` would be the harmful one.
#
# THE SCAN.  A single left-to-right pass that tracks exactly two states a
# construct test can be fooled by: a BACKSLASH escape (so `\(?{` is not a code
# block and `\\K` is not `\K`) and a BRACKETED CHARACTER CLASS (so `[(?>]` is
# not an atomic group and `[[:alpha:]]` is recognised only there).  It is not a
# regex parser and does not need to be: every construct below is identified by
# a two-to-four character opener whose meaning does not depend on the
# surrounding grammar.
use v5.20;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(tier tier_keyword);

our $VERSION = '1.0';

# The closed set, in order of increasing demand on the target.  A caller that
# spells a tier not in this set is a bug in the caller.
our @TIERS = qw(native pcre refused dynamic);
our %TIERS = map { $_ => 1 } @TIERS;

# The modifier letters perl accepts on m// / qr// / s/// / tr///.  Letters
# that change what the ENGINE must do are marked with their tier; every other
# letter is a match-OPERATION flag (or a charset default a rewrite expresses)
# and leaves the tier alone.
#
#   l   locale-dependent case folding and character classes — perl consults
#       the process locale, which a plain host engine has no notion of
#   p   /p is a no-op since 5.20 (${^PREMATCH} is always available)
#
# `xx` is not a letter of its own: it is `x` twice (task #179).
my %FLAG_TIER = (l => 'pcre');
my %FLAG_KNOWN = map { $_ => 1 } split //, 'msixpogcedualnr';

# ── The construct table ───────────────────────────────────────────────────
# Each entry: a matcher applied at the current scan position OUTSIDE a
# character class, and the tier it demands.  Order matters only in that the
# refused openers are tested before the generic `(?` ones.
#
# The tier a construct demands (and why), for docs/ir-spec.md §10's regex row:
#
#   refused  (?{…}  (??{…}          perl code in the pattern
#   pcre     (?>                    atomic group
#            (?R) (?0) (?N) (?-N) (?+N) (?&name) (?P>name)
#                                   recursion / subroutine call
#            (?(                    conditional
#            (?|                    branch reset
#            (*VERB) (*VERB:…)      backtracking control verbs
#            X*+ X++ X?+ X{n,m}+    possessive quantifiers
#            \K                     keep — reset the reported match start
#            \R \h \H \v \V         perl's line-break and horizontal /
#                                   vertical whitespace classes
#            \N                     any character but newline (NOT `\N{…}`,
#                                   which is a named character)
#            \C \X                  single byte / extended grapheme cluster
#            \g1 \g{…}              relative / braced backreference
#            [[:alpha:]]            POSIX class (inside a bracketed class)
#   native   everything else, including (?:  (?=  (?!  (?<=  (?<!
#            (?<name>…)  \k<name>  \p{…}  \P{…}  (?#…)  \A \z \Z \G \b
#            and the ordinary quantifiers, alternation and backreferences
#
# `\p{…}` is native because ES2018's `u` mode implements the Unicode
# *standard* property names.  A perl-only spelling (`\p{IsAlpha}`,
# `\p{Word}`, a user-defined `\p{In...}` sub) is not, and that residue is
# named in ir-spec §10 rather than guessed at here: the property NAME would
# have to be checked against a table this classifier deliberately does not
# carry.

sub _flags_tier {
  my ($flags) = @_;
  my $t = 'native';
  for my $c (split //, ($flags // '')) {
    next if $c =~ /\s/;
    if (my $ft = $FLAG_TIER{$c}) { $t = $ft }
    elsif (!$FLAG_KNOWN{$c})     { $t = 'pcre' }   # advisory: see the header
  }
  return $t;
}

# TRUE when the `(` at position $i opens one of the PCRE-only group forms.
sub _pcre_group_at {
  my ($p, $i) = @_;
  my $rest = substr($p, $i);
  return 1 if $rest =~ /\A\(\?>/;                       # atomic group
  return 1 if $rest =~ /\A\(\?\(/;                      # conditional
  return 1 if $rest =~ /\A\(\?\|/;                      # branch reset
  return 1 if $rest =~ /\A\(\?(?:R|[0-9]+|[-+][0-9]+)\)/;  # recursion
  return 1 if $rest =~ /\A\(\?(?:&|P>)/;                # named subroutine call
  return 1 if $rest =~ /\A\(\*[A-Z]/;                   # (*SKIP) (*FAIL) …
  return 0;
}

# The single-character escapes whose tier is not native.  `\N` only when it is
# NOT `\N{` (a named character, which PCL shims — ir-spec §3.2).
my %ESC_PCRE = map { $_ => 1 } qw(K R h H v V C X g);

sub _escape_tier {
  my ($p, $i) = @_;    # $p[$i] is the backslash
  my $c = substr($p, $i + 1, 1);
  return 'native' if !length $c;
  return 'pcre'   if $c eq 'N' && substr($p, $i + 2, 1) ne '{';
  return 'pcre'   if $ESC_PCRE{$c};
  return 'native';
}

# A possessive quantifier: a `+` directly after `*`, `+`, `?` or the `}` of a
# COUNTED quantifier.  The `}` half needs the counted test and that is not
# pedantry: `\p{L}+` is a Unicode property followed by an ordinary `+`, and
# reading its `}+` as possessive called every `\p{…}+` pattern pcre (probed —
# it was this classifier's first bug).
sub _possessive_at {
  my ($p, $i) = @_;
  my $q = substr($p, $i, 1);
  return 0 unless $q eq '*' || $q eq '+' || $q eq '?' || $q eq '}';
  return 0 unless substr($p, $i + 1, 1) eq '+';
  return 1 unless $q eq '}';
  my $open = rindex($p, '{', $i);
  return 0 if $open < 0;
  return substr($p, $open, $i - $open + 1) =~ /\A\{\d+(?:,\d*)?\}\z/ ? 1 : 0;
}

# The worst tier of two, by @TIERS order — except that `dynamic` is never a
# result of the scan (only of a missing pattern) and `refused` wins outright.
my %RANK = (native => 0, pcre => 1, refused => 2, dynamic => 3);
sub _worse { return $RANK{$_[0]} >= $RANK{$_[1]} ? $_[0] : $_[1] }

# tier(PATTERN, FLAGS) — PATTERN is the raw perl pattern text WITHOUT
# delimiters, FLAGS the modifier letters as written.  PATTERN undef means "not
# known at compile time" and answers `dynamic`.
sub tier {
  my ($pat, $flags) = @_;
  return 'dynamic' if !defined $pat;
  my $t = _flags_tier($flags);
  my $n = length $pat;
  my ($i, $in_class) = (0, 0);
  while ($i < $n) {
    my $c = substr($pat, $i, 1);
    if ($c eq '\\') {
      $t = _worse($t, _escape_tier($pat, $i)) unless $in_class;
      $i += 2;
      next;
    }
    if ($in_class) {
      # A POSIX class is the only class-interior construct that changes the
      # tier.  `]` closes the class — except as the FIRST character, where it
      # is a literal (perl and PCRE both).
      if ($c eq '[' && substr($pat, $i) =~ /\A\[:\^?[a-z]+:\]/) {
        $t = _worse($t, 'pcre');
        $i += length($&);
        next;
      }
      $in_class = 0 if $c eq ']';
      $i++;
      next;
    }
    if ($c eq '[') {
      $in_class = 1;
      $i++;
      # A leading `^` then a leading `]` are literal members, not the close.
      $i++ if substr($pat, $i, 1) eq '^';
      $i++ if substr($pat, $i, 1) eq ']';
      next;
    }
    if ($c eq '(') {
      my $rest = substr($pat, $i);
      if ($rest =~ /\A\(\?\??\{/) { return 'refused' }   # (?{…} / (??{…}
      $t = _worse($t, 'pcre') if _pcre_group_at($pat, $i);
      $i++;
      next;
    }
    $t = _worse($t, 'pcre') if _possessive_at($pat, $i);
    $i++;
  }
  return $t;
}

# The CL keyword the emitter writes: `:native` / `:pcre` / `:refused` /
# `:dynamic`.  One place, so the spelling in the IR cannot drift from the
# spelling in this table.
sub tier_keyword {
  my ($pat, $flags) = @_;
  my $t = tier($pat, $flags);
  die "Pl::RegexTier: '$t' is not one of the tiers (@TIERS)\n" unless $TIERS{$t};
  return ":$t";
}

1;

package Pl::GlobalPartition;

# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

# THE global partition (direction D, task #289 — plan docs/direction-d-plan.md).
#
# Every package global PCL emits falls in exactly one of two classes, and the
# SAME answer must be given by the declaration emitter and by the `local`
# lowering — two sites, one function (CLAUDE.md rule 11).  A disagreement is
# not cosmetic: declaring a name `defvar` (special) and then also
# `define-symbol-macro`ing it is a load-time error in SBCL, and localizing a
# cell through the wrong mechanism restores the wrong storage.
#
#   ORDINARY  → (define-symbol-macro $x (sb-ext:symbol-global-value '$x))
#               over an initialized cell; `local` lowers to (p-local-cell …).
#               These are user package variables.  They are NEVER dynamically
#               bound, which is what makes the direct global-cell accessor
#               valid (probed s382d) and what lets a `my` shadow be a plain
#               lexical instead of a dynamic rebind.
#
#   EXCEPTION → today's `defvar` + dynamic `let`, byte-identical.  Two causes,
#               both image-global and both name-decidable:
#
#     (a) NOT WORD-SHAPED — punctuation and caret magic ($@, $1, $!, $|, $.,
#         @#, |${^WARNING_BITS}|, …).  These are the runtime's own variables:
#         it defines them, reads them by name, and several are magic cells
#         whose `local` has a bespoke lowering (p-local-dot, p-local-pipe,
#         *p-stored-errno*).  They are also where `local` actually runs HOT in
#         real perl code, which is why keeping their fast dynamic bind is the
#         answer to direction D's one measured regression (plan §2: `local` of
#         an ORDINARY global costs ~41 ns vs ~4.6 ns; magic vars keep 4.6).
#
#     (b) WORD-SHAPED but runtime-owned or thread-of-control ($_, @_, @ARGV,
#         %ENV, …) plus the sort pair $a/$b in EVERY package.  $a/$b are bound
#         by the sort lowering itself — a dynamic bind is the mechanism, not
#         an implementation detail (#287, s380).
#
# ── Relationship to the two OTHER name tables (read before editing) ─────────
# Parser2 has %PKG_SWITCH_IMMUNE_VARS ("can an in-block `package X;` re-home
# this name?") and _forward_global_decls's %runtime_vars ("does this name need
# a defvar at all?").  Those two are deliberately NOT derived from each other,
# and this third one is not derived from either: the question here is "does
# this name keep a DYNAMIC binding?".  The three answer sets happen to
# coincide closely TODAY; Pl/t/global-partition-01.t pins that coincidence so
# a future divergence shows up as a failing row instead of a silent drift.
# Keep them in step by CAUSE, not by copy.

use v5.20;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(is_exception_global partition_name global_decl_form);

# Cause (b): word-shaped names that stay dynamically bound.  The magic half is
# the same membership %PKG_SWITCH_IMMUNE_VARS states for its own question —
# written out here, not imported, because the questions differ (see above).
my %WORD_SHAPED_EXCEPTION = map { $_ => 1 } qw(
  $_ @_ %_args @ARGV $ARGV @ARGVOUT @INC %INC %ENV %SIG
  $a $b
);

# Split a CL variable name into (package, sigil+name), or () when it is not a
# plain variable at all.  Accepted spellings, exactly the ones the two
# emitters produce:
#   $x  @arr  %h              unqualified
#   Foo::$x   Foo::Bar::@a    package-qualified (the sigil sits after the ::)
#   |Foo Bar|::$x             pipe-quoted package (a CL package name that
#                             needs escaping) — the PACKAGE is quoted, the
#                             variable part is not
# A pipe-quoted VARIABLE (|$.|, |${^MPE}|), a bare punctuation name ($@, @#),
# a (p-stash …) form or anything else returns () — "not word-shaped", which
# the caller reads as EXCEPTION.
#
# $ID is the identifier shape a package SEGMENT or a variable NAME has, in
# the spelling the two emitters produce: one word character that is not a
# digit, then word characters.  It must be UNICODE-AWARE, because `use utf8`
# source hands PCL real Unicode package names (perl's own t/mro/*_utf8.t).
# Task #313: the head used to be ASCII-only (`[A-Za-z_]`) while the `\w` after
# it already matched Unicode, so `Baɾ::` split but `બʑ::` did not — the split
# failed, partition_name read that as "not word-shaped" = EXCEPTION, and the
# `defvar` it then emitted for `બʑ::@ISA` collided with the symbol-macro cell
# p-defpackage had already made for that package's @ISA: a LOAD-TIME crash
# ("Cannot proclaim a macro variable special"), which is precisely the hazard
# this file's header warns about.
my $ID = qr/[^\W\d]\w*/;

sub _split_name {
  my ($cl_name) = @_;
  return () if !defined $cl_name;
  my $rest = $cl_name;
  my $pkg;
  if ($rest =~ s/^\|([^|]*)\|:://)      { $pkg = $1 }
  elsif ($rest =~ s/^($ID(?:::$ID)*):://) { $pkg = $1 }
  return () if $rest !~ /^([\$\@\%])($ID)\z/;
  return ($pkg, "$1$2");
}

# The partition, as a word: 'ordinary' or 'exception'.  Total — every input
# gets an answer, and anything unrecognised answers 'exception' (the
# conservative side: it keeps today's emission).
sub partition_name {
  my ($cl_name) = @_;
  my ($pkg, $bare) = _split_name($cl_name);
  return 'exception' if !defined $bare;                    # cause (a)
  return 'exception' if $WORD_SHAPED_EXCEPTION{$bare};     # cause (b)
  return 'ordinary';
}

# Does this name keep today's defvar + dynamic-let lowering?
sub is_exception_global {
  my ($cl_name) = @_;
  return partition_name($cl_name) eq 'exception' ? 1 : 0;
}

# The DECLARATION line for a global, as the partition decides it.  Every
# emitter that declares a package variable goes through here — the spelling
# and the partition must never drift apart, and a name declared `defvar` in
# one place and `p-defcell` in another is a load-time error in SBCL (which is
# the good outcome; the bad one is two emitters silently disagreeing about
# WHICH cell a `local` saves).
#
# INIT is the same fresh-container form both sides already build
# (make-p-box / make-array / make-hash-table), so the two arms differ only in
# the declarer.  p-defcell is define-once via a boundp guard, exactly as
# defvar is — see its docstring in cl/pcl-runtime.lisp.
sub global_decl_form {
  my ($cl_name, $init) = @_;
  my $declarer = is_exception_global($cl_name) ? 'defvar' : 'p-defcell';
  return "($declarer $cl_name $init)";
}

1;

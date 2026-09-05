# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::Passes;
# The optimization registry (docs/v2-target-architecture.md §3;
# docs/plan-one-compiler-s411.md Phase R, task #383).
#
# Two kinds of optimization, one switchboard:
#
#   Kind A — a FACTS-LICENSED EMISSION: the emitter uses a fast shape only
#     when an analysis fact grants it and the general (boxed) form otherwise.
#     Every such site asks `Pl::Passes::enabled('name')` and takes the general
#     form when the answer is no.  The names are the closed set %KIND_A below;
#     a site that consults an unknown name dies (rule 12 — a typo would
#     otherwise silently mean "always on").
#
#   Kind B — a CLForm→CLForm PASS run on every top-level form just before it
#     is printed: `register_pass('name', \&f)` appends to an ordered list;
#     `run($form)` applies the enabled ones in registration order.  A pass is
#     a pure function of the tree (plus whatever facts it closes over); it
#     never sees text.  The first one is `classic-sort' (Pl/ClassicSort.pm,
#     task #996 half A5): it is Kind B and not Kind A because its licence is a
#     fact about the sort's CONSUMER — `sort' returns aliases, so a
#     value-sorting fast path is legal only where the consumer copies — and
#     the consumers are six different emitters with no parent link in the
#     PExpr tree.  The lowered CLForm tree is the one place they all pass
#     through, and it already carries the `p-foreach-raw' head, i.e.
#     VarAnnotator's read-only verdict, so the pass consults that fact rather
#     than walking for it a second time (rule 11).
#
# The switch is the environment: PCL_OPT=none turns everything off;
# PCL_OPT=-raw-numeric,+str-buffer is a comma list of -name (off) / +name or
# name (on), applied left to right after `none` if both are present.  It is
# read once at load and inherited by every compiler process (pl2cl --server,
# the runtime's module transpiles), so a whole run sees one setting.  The
# pre-registry knob PCL_NO_RAW_VERDICT=1 is kept as an alias of -raw-numeric.
#
# What the flag is FOR: an optimization can be developed, measured and
# bisected AFTER the compiler is done — `PCL_OPT=none` is the general-form
# compiler, and corpus-diff between two settings shows exactly what a
# transform buys.  What it is NOT: a correctness switch.  Every general form
# must be correct on its own; the Phase R bar runs the gate and the sweep
# under PCL_OPT=none for that reason.
use v5.20;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(enabled register_pass run);

# Kind-A names → the fact/site that owns each.  Add a name here when a new
# facts-licensed emission lands; the emitter's gate is the second half.
our %KIND_A = (
  'raw-slot'      => 'VarAnnotator unboxable verdict: an unshared `my` scalar becomes a raw let slot instead of a p-box',
  'raw-numeric'   => 'VarAnnotator B-regime freeze (docs/raw-numeric-verdict.md): use-proven eager coercion, %pcl-to-number-strict / %pcl-to-string-strict',
  'str-buffer'    => 'VarAnnotator S1 verdict: an append-only string slot holds a fill-pointer buffer, `.=` becomes in-place %pcl-str-append',
  'foreach-range' => 'Parser2 foreach: `for $v (A..B)` lowers to the counting macro p-foreach-range instead of materializing the range',
  'insensitive-call' => 'ExprToCL funcall: a KNOWN user sub whose body never observes *wantarray* (Parser2::_sub_return_facts) is called without the context bind (the p-…-ctx wrap, ir-spec §4)',
  'elem-setf'      => 'ExprToCL `=`: `$h{k} = V` / `$a[i] = V` on a let-bound container with a pure key writes through CL setf instead of p-setf (no boundp auto-declare)',
  'tail-return'   => "Parser2 _lower_body_regime: a `return EXPR` that IS the sub body's last statement lowers as the tail expression EXPR (task #994) -- its value is already the frame's, so the throw to :p-return and p-return-value's normalisation (which %p-leavesub repeats at the frame exit) are pure overhead",
  # --- verdict-COVERAGE narrowings (s453 review §13, tasks #758-#761).  Each
  # widens where the raw-slot/raw-numeric verdicts above may fire; none is a
  # new fast shape except raw-topic.  Named separately so a coverage widening
  # can be bisected on its own (PCL_OPT=-raw-op-family), which -raw-slot
  # cannot do (it turns the whole verdict off).
  'raw-block-eval' => 'VarAnnotator eval-in-region: a BLOCK `eval {…}` is not a boxing event (the capture alist is a STRING-eval mechanism), so it no longer boxes every name in its region',
  'raw-op-family'  => "VarAnnotator write family: a root write whose RHS is a closed-set arith/string OPERATOR takes the operator's result family, so `\$s = \$s + \$_` proves num like `\$s += \$_` does",
  'raw-closure-capture' => 'VarAnnotator nested-sub-ref: an anon sub CAPTURING a name is not itself a boxing event (a CL closure captures a raw let slot natively) — only a real boxing event inside the closure vetoes',
  'raw-topic'      => 'Parser2 foreach: a topic loop `for (A..B) {…}` whose body has no dynamic `$_` reader binds `$_` as a raw per-iteration lexical (p-foreach-range-raw) instead of localizing the global',
  'raw-return-family' => "Parser2 sub_info `returns` + VarAnnotator write family (task #77): a root write `my \$x = f()` calling a KNOWN user sub whose every return is operator-coerced or literal takes THAT family, so the slot is a PROVEN raw write (no strict-freeze wrapper) instead of an unproven shape",
  'local-push'     => "VarAnnotator array facts (task #1140) + ExprToCL's push emission: `push \@a, SCALAR` on a NON-ESCAPING `my \@a` is (%p-push1 \@a X) -- %p-array-store-scalar plus the new length -- instead of p-push-impl's &rest consing, array-shape type test and four-way per-item cond (task #996 half A3)",
  # THE ONE Kind-A gate that is not purely about speed, and it says so: with
  # it OFF a loop emits today's frame-less shape AND the exit site keeps half
  # (a)'s trappable "unsupported" die, so the two halves agree (no frame =>
  # the site dies).  A program with no dynamic loop exit runs identically
  # either way, which is the identity `Pl/t/passes-01.t` guards; a program
  # that HAS one cannot, because the frame is the feature.
  'dyn-loop-exit'  => "Pl::PExpr::TokenUtils::may_dyn_exit + Parser2 loops (task #1022 half (b), licence narrowed by #1162): a loop that can REACH a marked exit site BY NAME in this compilation unit establishes ONE catch of 'p-loop-dyn per loop ENTRY (`:dyn t`, %p-loop-driver / p-dyn-once), so a bare `last`/`next`/`redo` in a CALLED sub acts on the caller's innermost loop as perl's does; every other loop is emitted byte-identically and pays nothing, which matters because the catch costs ~4.8 MB of SBCL COMPILE IR",
  'foreach-raw'    => 'VarAnnotator foreach_ro + Parser2 foreach: a `for my $v (LIST)` whose only region event is the foreach alias itself AND which has no native-write fact either (a root `$v = …` / `$v *= 2` / `$v++` leaves no event) — i.e. every use is a pure read — lowers to p-foreach-raw, which binds the slot AS IT STANDS instead of promoting each element to a box (boxed-aggregates design SS4.4, the proven arm)',
);

my @PASSES;          # [name, coderef] in registration order (Kind B)
my $FORM_HOOK;      # an OBSERVER of every finished top-level form (set_form_hook)
my $TEXT_HOOK;      # ... and of the two v1-TEXT buckets (set_text_hook)
my %PASS_INDEX;
my ($all_off, %off, %on, $checked);
_parse_env();

sub _parse_env {
  ($all_off, %off, %on, $checked) = (0);
  my $spec = $ENV{PCL_OPT} // '';
  $spec = "-raw-numeric,$spec" if $ENV{PCL_NO_RAW_VERDICT};
  for my $tok (grep { length } map { s/^\s+|\s+$//gr } split /,/, $spec) {
    if    ($tok eq 'none') { $all_off = 1; %off = (); %on = () }
    elsif ($tok eq 'all')  { $all_off = 0; %off = (); %on = () }
    elsif ($tok =~ /^-(.+)$/) { $off{$1} = 1; delete $on{$1} }
    elsif ($tok =~ /^\+?(.+)$/) { $on{$1} = 1; delete $off{$1} }
  }
}

# Names in PCL_OPT are checked against the registry LAZILY — Parser2::parse
# calls check_env() once every module is loaded (a Kind-B pass registered by
# a later module can still be named in the environment); enabled() calls it
# too as the backstop.  An unknown name is an error, not a silent no-op.
sub check_env {
  return if $checked++;
  my @unknown = sort grep { !$KIND_A{$_} && !exists $PASS_INDEX{$_} } keys %off, keys %on;
  die "Pl::Passes: unknown optimization name(s) in PCL_OPT: @unknown\n"
    . "  known: " . join(' ', names()) . "\n" if @unknown;
}

sub enabled {
  my ($name) = @_;
  die "Pl::Passes::enabled: '$name' is not a registered optimization\n"
    unless $KIND_A{$name} || exists $PASS_INDEX{$name};
  check_env();
  return 1 if $on{$name};
  return 0 if $off{$name} || $all_off;
  return 1;
}

sub register_pass {
  my ($name, $code) = @_;
  die "Pl::Passes::register_pass: '$name' is already registered\n" if exists $PASS_INDEX{$name};
  die "Pl::Passes::register_pass: '$name' collides with a Kind-A name\n" if $KIND_A{$name};
  push @PASSES, [$name, $code];
  $PASS_INDEX{$name} = $#PASSES;
  $checked = 0;   # a new name may make a pending PCL_OPT token valid
  return;
}

# Apply every enabled Kind-B pass, in order, to one top-level form.
#
# THE FORM HOOK HANGS OFF THIS FUNCTION (task #1171, Part B item B2): every
# top-level form reaches here on its way to text, so a consumer that wants to
# see the finished tree — `pl2cl --manifest`'s ONE walk — installs itself
# with set_form_hook and needs no second walk and no second tree
# representation.  An OBSERVER, not a pass: its return value is discarded, so
# it cannot change emission, and with no hook installed the cost is one
# scalar test per top-level form.
sub run {
  my ($form) = @_;
  return $form unless @PASSES || $FORM_HOOK;
  for my $p (@PASSES) {
    next unless enabled($p->[0]);
    $form = $p->[1]->($form);
  }
  $FORM_HOOK->($form) if $FORM_HOOK;
  return $form;
}

sub set_form_hook {
  my ($code) = @_;
  die "Pl::Passes::set_form_hook: not a code ref\n"
    if defined $code && ref($code) ne 'CODE';
  $FORM_HOOK = $code;
  return;
}

# The TEXT twin.  Two top-level buckets are v1 text and never become trees
# (Parser2's `captured` and `sched`), so an observer that wants the WHOLE
# program has to be offered them in the only form they have.  Same contract:
# an observer, return value discarded, one scalar test when nothing watches.
sub set_text_hook {
  my ($code) = @_;
  die "Pl::Passes::set_text_hook: not a code ref\n"
    if defined $code && ref($code) ne 'CODE';
  $TEXT_HOOK = $code;
  return;
}

sub note_text {
  $TEXT_HOOK->($_[0]) if $TEXT_HOOK;
  return;
}

sub names { return (sort(keys %KIND_A), map { $_->[0] } @PASSES) }
sub passes { return map { $_->[0] } @PASSES }

1;

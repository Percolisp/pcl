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
#     never sees text.  Empty until the first pass lands (#73/#74/#77 are the
#     queued candidates).
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
  'insensitive-call' => 'ExprToCL funcall: a KNOWN user sub whose body never observes *wantarray* (Parser2::_sub_ctx_insensitive) is called without the context bind (the p-…-ctx wrap, ir-spec §4)',
  'elem-setf'      => 'ExprToCL `=`: `$h{k} = V` / `$a[i] = V` on a let-bound container with a pure key writes through CL setf instead of p-setf (no boundp auto-declare)',
  # --- verdict-COVERAGE narrowings (s453 review §13, tasks #758-#761).  Each
  # widens where the raw-slot/raw-numeric verdicts above may fire; none is a
  # new fast shape except raw-topic.  Named separately so a coverage widening
  # can be bisected on its own (PCL_OPT=-raw-op-family), which -raw-slot
  # cannot do (it turns the whole verdict off).
  'raw-block-eval' => 'VarAnnotator eval-in-region: a BLOCK `eval {…}` is not a boxing event (the capture alist is a STRING-eval mechanism), so it no longer boxes every name in its region',
  'raw-op-family'  => "VarAnnotator write family: a root write whose RHS is a closed-set arith/string OPERATOR takes the operator's result family, so `\$s = \$s + \$_` proves num like `\$s += \$_` does",
  'raw-closure-capture' => 'VarAnnotator nested-sub-ref: an anon sub CAPTURING a name is not itself a boxing event (a CL closure captures a raw let slot natively) — only a real boxing event inside the closure vetoes',
  'raw-topic'      => 'Parser2 foreach: a topic loop `for (A..B) {…}` whose body has no dynamic `$_` reader binds `$_` as a raw per-iteration lexical (p-foreach-range-raw) instead of localizing the global',
);

my @PASSES;          # [name, coderef] in registration order (Kind B)
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
sub run {
  my ($form) = @_;
  return $form unless @PASSES;
  for my $p (@PASSES) {
    next unless enabled($p->[0]);
    $form = $p->[1]->($form);
  }
  return $form;
}

sub names { return (sort(keys %KIND_A), map { $_->[0] } @PASSES) }
sub passes { return map { $_->[0] } @PASSES }

1;

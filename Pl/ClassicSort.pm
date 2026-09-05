# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::ClassicSort;

# THE `classic-sort' OPTIMIZATION (task #996 half A5) — a Kind-B CLForm pass
# in the registry (Pl/Passes.pm), so `PCL_OPT=none' and `PCL_OPT=-classic-sort'
# emit exactly today's general form.
#
# WHAT IT DOES.  `sort LIST' and the four classic comparator blocks —
# { $a <=> $b } { $b <=> $a } { $a cmp $b } { $b cmp $a } — lower to
# (%p-sort-classic MODE ARGS…) instead of
# (p-sort (p-sort-cmp ($a $b) (p-scalar-ctx (p-<=> $a $b))) ARGS…).
# The runtime half (cl/pcl-runtime.lisp, "THE CLASSIC-SORT FAST PATH") says
# what that buys and when it hands the sort back to p-sort.
#
# WHY A Kind-B PASS AND NOT A Kind-A EMISSION SITE.  The licence is a fact
# about the sort's CONSUMER, not about the sort: `sort' returns ALIASES in
# perl and in PCL (`$_++ for sort { $a <=> $b } @a' writes back into @a —
# probed), so a fast path that reads VALUES is legal only where nobody can
# observe which box came back.  The consumers are six different emitters
# (array assignment, list assignment, join/print/say/printf, the anon-array
# constructor, return, foreach) and the ExprToCL tree has no parent link, so a
# Kind-A gate would have to be spelled at all six — the rule-11 hard stop.
# The lowered CLForm tree is the ONE place they all pass through
# (Parser2::_lower_program: "the one place a lowered tree becomes text"), and
# it also carries the foreach-raw verdict already: a `p-foreach-raw' head IS
# VarAnnotator's proof that the loop variable is only ever read, so the pass
# CONSULTS that fact rather than computing a second one.
#
# THE TWO LICENCES (either one suffices; both were probed against perl 5.40.3
# — scratch probes alias.pl / alias2.pl / alias3.pl / alias4.pl):
#   A — a COPYING CONSUMER.  The sort's value is copied before anyone can
#       write through it: an array/hash/list assignment, an anon-array
#       constructor, push/unshift, join/print/say/printf, return.  `reverse'
#       is TRANSPARENT (it hands the aliases through unchanged) and so is the
#       context wrapper, so both propagate the licence.
#       THE `p-foreach-raw' MEMBER INHERITS foreach-raw's OWN CONDITION, no
#       more and no less (s470a5 merge review).  That verdict covers writes
#       THROUGH the loop variable; it does NOT cover a write to the ARRAY by
#       another path during the loop, because VarAnnotator has no array facts
#       at all.  So `for my $x (@fa) { $fa[0] = 99; print $x; last }' already
#       prints perl's 99 only with `PCL_OPT=-foreach-raw', and the sort-fed
#       spelling is exactly as sound as that — no better, no worse.  The
#       array-fact family that makes both exact is task #1140.
#       NOT a member: `do { sort … }'.  A do-block's tail hands the aliases
#       through in perl (`$_++ for do { sort @d }' writes back into @d —
#       probed), so it is not a copying consumer.  `eval { }' and a sub tail
#       DO copy; the eval-block head is left out anyway, conservatively.
#   B — a TEMPORARY-PRODUCING SOURCE.  Every top-level argument is a fresh
#       value: a literal, `keys', `map', `split', `readdir', `glob', a range,
#       or a call to a user sub (the sub-call boundary copies — probed with a
#       sub returning `@arr' AND one returning `values %h'; neither aliases).
#
# `values' IS NOT FRESH — measured, and it is the one correction this pass
# makes to the design it was written from: `for my $v (sort { $a <=> $b }
# values %h) { $v .= "!" }' DOES write back into the hash, in perl and in PCL
# alike.  So do `grep' and a nested `sort'.

use v5.20;
use strict;
use warnings;

use Pl::Passes ();

# The comparator's binary operator → the two modes (ascending, descending).
my %OP_MODE = (
  'p-<=>'     => [':num-asc', ':num-desc'],
  'p-str-cmp' => [':str-asc', ':str-desc'],
);

# Heads that hand their operand's aliases through unchanged, so the licence
# passes through them: the context wrapper and `reverse'.
my %TRANSPARENT = map { $_ => 1 } qw(p-list-ctx p-reverse);

# Heads whose every operand is COPIED before anyone can write through it
# (licence A).  A target operand is always an atom (`@x'), so marking the
# whole form is exact as well as cheap.
my %COPYING = map { $_ => 1 } qw(
  p-array-= p-hash-= p-list-= p-array-fill p-array-init p-hash-init
  p-join p-print p-say p-printf
  p-push p-unshift
  p-return p-tail-value
);

# Scalar/void context: `sort' in scalar context is not licensed at all (its
# value is not a list any more), so a direct operand of one of these refuses
# even when licence B would hold.
my %SCALARCTX = map { $_ => 1 } qw(p-scalar-ctx p-void-ctx p-scalar);

# Licence B: heads that build fresh values.  NOT p-values / p-grep / p-sort —
# all three hand aliases through (probed).
my %FRESH_HEAD = map { $_ => 1 } qw(p-keys p-map p-split p-readdir p-glob p-.. p-...);

use constant { PLAIN => 0, COPYING => 1, SCALAR => 2 };

Pl::Passes::register_pass('classic-sort', \&run);

sub run { my ($form) = @_; return _walk($form, PLAIN) }

# Walk one form, rewriting every licensed p-sort in it.  Children are rewritten
# IN PLACE (a lowered form is printed once), so an unchanged tree costs one
# traversal and no allocation.
sub _walk {
  my ($f, $mode) = @_;
  my $r = ref $f or return $f;
  if ($r eq 'Pl::CLForm::RawWrap') {
    $_ = _walk($_, PLAIN) for @{ $f->{body} };
    return $f;
  }
  return $f unless $r eq 'ARRAY';
  my $head = ref($f->[0]) ? '' : ($f->[0] // '');
  # The children are rewritten BEFORE this node, so a replacement built here
  # carries the rewritten children rather than the arrayrefs they replaced.
  # The foreach pair is (VAR LIST): only the LIST is a consumer position, and
  # only the read-only (`-raw') verdict makes the alias unobservable.
  if ($head eq 'p-foreach-raw' && ref($f->[1]) eq 'ARRAY'
      && ($f->[1][0] // '') eq 'list' && @{$f->[1]} == 3) {
    $f->[1][2] = _walk($f->[1][2], COPYING);
    for my $i (2 .. $#$f) { $f->[$i] = _walk($f->[$i], PLAIN) }
    return $f;
  }
  my $child = $TRANSPARENT{$head} ? ($mode == SCALAR ? PLAIN : $mode)
            : $COPYING{$head}     ? COPYING
            : $SCALARCTX{$head}   ? SCALAR
            :                       PLAIN;
  for my $i (0 .. $#$f) { $f->[$i] = _walk($f->[$i], $child) }
  return $f unless $head eq 'p-sort';
  return _rewrite_sort($f, $mode) // $f;
}

# The p-sort form, licensed → its %p-sort-classic replacement, else undef.
# The children are walked by the caller either way (a nested sort inside a
# comparator body is judged on its own).
sub _rewrite_sort {
  my ($f, $mode) = @_;
  return undef if $mode == SCALAR;
  my ($cl_mode, $first_arg) = _comparator_mode($f);
  return undef unless defined $cl_mode;
  my @args = @$f[$first_arg .. $#$f];
  return undef unless $mode == COPYING || _all_fresh(\@args);
  return ['%p-sort-classic', $cl_mode, @args];
}

# (MODE, index of the first list argument) for a p-sort form whose comparator
# is one of the five licensed shapes; the empty list otherwise.
sub _comparator_mode {
  my ($f) = @_;
  my $cmp = $f->[1];
  return (':default', 1) if !ref $cmp || ref $cmp ne 'ARRAY';
  my $ch = ref($cmp->[0]) ? '' : ($cmp->[0] // '');
  # The three shapes gen_inline_lambda_form gives a COMPARATOR: the block, the
  # `sort NAME LIST' name-carrier, and the `sort $cmp LIST' *package* rebind.
  # Only the first can be a classic shape; the other two keep the general form
  # (their comparator runs user code per comparison).
  return () if $ch eq 'p-sort-named' || $ch eq 'let';
  return (':default', 1) unless $ch eq 'p-sort-cmp';
  my ($params, @rest) = @$cmp[1 .. $#$cmp];
  shift @rest while @rest && ref $rest[0] eq 'ARRAY' && ($rest[0][0] // '') eq 'declare';
  return () unless @rest == 1 && ref $params eq 'ARRAY' && ($params->[0] // '') eq 'list'
                && @$params == 3 && !ref($params->[1]) && !ref($params->[2])
                && $params->[1] ne $params->[2];
  my $body = $rest[0];
  return () unless ref $body eq 'ARRAY' && ($body->[0] // '') eq 'p-scalar-ctx' && @$body == 2;
  my $op = $body->[1];
  return () unless ref $op eq 'ARRAY' && @$op == 3 && !ref($op->[0]);
  my $modes = $OP_MODE{ $op->[0] } or return ();
  my ($a, $b) = ($params->[1], $params->[2]);
  return ($modes->[0], 2) if $op->[1] eq $a && $op->[2] eq $b;
  return ($modes->[1], 2) if $op->[1] eq $b && $op->[2] eq $a;
  return ();
}

sub _all_fresh {
  my ($args) = @_;
  return 0 unless @$args;
  for my $a (@$args) { return 0 unless _fresh_arg($a) }
  return 1;
}

# Licence B, one argument: does evaluating it produce values nothing else
# holds a box for?
sub _fresh_arg {
  my ($f) = @_;
  if (!ref $f) {
    # A literal number or double-quoted string; every other atom (a variable,
    # a keyword marker, `nil') is not a fresh producer.
    return ($f =~ /\A-?[0-9]/ || $f =~ /\A"/) ? 1 : 0;
  }
  return 0 unless ref $f eq 'ARRAY';
  my $head = ref($f->[0]) ? '' : ($f->[0] // '');
  return _fresh_arg($f->[1]) if $head eq 'p-list-ctx' && @$f == 2;
  return 1 if $FRESH_HEAD{$head};
  # A user sub call (`pl-NAME'): the call boundary copies its return list.
  return 1 if $head =~ /\Apl-/;
  if ($head eq 'vector') {
    for my $i (1 .. $#$f) { return 0 unless _fresh_arg($f->[$i]) }
    return 1;
  }
  return 0;
}

1;

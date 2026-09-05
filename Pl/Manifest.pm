# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::Manifest;
# THE PER-PROGRAM MANIFEST (task #1171; docs/plan-speed-and-ir-s470.md Part B
# item B2).  `pl2cl --manifest FILE` prints JSON saying what THIS program
# demands of a target:
#
#   uses   every runtime op with its count (and, in `uses_other`, every other
#          head symbol, so nothing is silently dropped)
#   needs  the OBLIGATION classes with counts — dynamic scope, non-local exit,
#          string eval, regex, phase, tie, overload, formats, xs, io, process
#   facts  which Kind-A/Kind-B licences fired, as `fired / candidates` wherever
#          the tree can say both (Pl/Passes.pm's names)
#
# A backend author reads it and knows what the program needs before writing a
# line: "string_eval 2 → not the browser without a compiler service";
# "dynamic_scope.local 7 → I need the save/restore stack".
#
# WHERE THE COUNTS COME FROM — ONE walk of the LOWERED CLForm tree, at the one
# place a lowered tree becomes text.  That place is `Pl::Passes::run`, which
# every top-level form passes through already (Parser2::_lower_sections calls
# it for decls, defs, run and pkg_enter), so the collector hangs off the
# registry's own hook and there is no second tree walk and no second tree
# representation.  The two v1-TEXT buckets (`captured`, `sched`) never become
# trees, so they are handed to note_text instead — a regex over `(NAME`, which
# is approximate by construction and says so in the output (`text_scanned`).
#
# OFF BY DEFAULT AND OFF IN EVERY OTHER MODE.  Nothing here runs unless
# `enable()` was called, so the default emission cannot move: the collector's
# entry points are two `return unless $ON` guards.  `pl2cl --manifest` prints
# the JSON INSTEAD of the CL, so the flag cannot contaminate an output file
# either.
#
# THE OBLIGATION CLASSES ARE THIS MODULE'S OWN DATA, and deliberately NOT the
# 53-family taxonomy of docs/ir-op-inventory.tsv: that answers "what is this
# op", this answers "what machinery must a target have".  They overlap but are
# not the same question (`p-die` is `exception` in the taxonomy and
# `nonlocal_exit` here).  The drift risk is real and is closed by a GATE ROW,
# not by a comment: Pl/t/manifest-01.t asserts that every op name below exists
# in the generated inventory, so a renamed op fails a row instead of silently
# dropping out of `needs`.
use v5.20;
use strict;
use warnings;

our $VERSION = '1.0';

my $ON = 0;
my (%HEAD, %TEXT_HEAD, %PLET_CLASS, %PARAM_CLASS, %PLET_FACT, %SUB_FACT);
my ($TEXT_CHUNKS, $SUBS, $SETF_ELEM, $PSETF_ELEM, $LOCAL_VALUE_WRAPS)
  = (0, 0, 0, 0, 0);

sub enable {
  $ON = 1;
  require Pl::Passes;
  # The ONE walk: Pl::Passes::run sees every finished top-level form, and its
  # text twin sees the two v1-TEXT buckets.  Installed as HOOKS so neither
  # Pl::Passes nor Pl::Parser2 gains a compile-time dependency on a
  # flag-only module — with no hook installed each site is a scalar test.
  Pl::Passes::set_form_hook(\&note_form);
  Pl::Passes::set_text_hook(\&note_text);
  return;
}
sub enabled { return $ON }

sub reset_all {
  (%HEAD, %TEXT_HEAD, %PLET_CLASS, %PARAM_CLASS, %PLET_FACT, %SUB_FACT) = ();
  ($TEXT_CHUNKS, $SUBS, $SETF_ELEM, $PSETF_ELEM, $LOCAL_VALUE_WRAPS)
    = (0, 0, 0, 0, 0);
  return;
}

# ── The obligation classes ────────────────────────────────────────────────
# op name => the class it obliges.  A name here that the runtime no longer
# exports is caught by Pl/t/manifest-01.t against docs/ir-op-inventory.tsv.
my %OBLIGATION = (
  # dynamic scope: the save/restore stack a target must have for `local`.
  # `p-box-for-local` is in the set and needs its correction below — see
  # _local_sites.
  (map { ($_ => 'dynamic_scope.local') }
    qw(p-local-cell p-local-cell-if p-local-maybe p-local-glob p-local-glob-if
       p-local-glob-dynamic p-local-dot p-local-pipe p-local-hash-elem
       p-local-array-elem p-local-hash-elem-init p-local-array-elem-init
       p-local-array-slice p-local-deref-scalar p-local-deref-array
       p-local-deref-hash p-box-for-local)),
  # non-local exit: an exception, a longjmp, or a labelled break
  'p-return'        => 'nonlocal_exit.return',
  'p-return-empty'  => 'nonlocal_exit.return',
  'p-last'          => 'nonlocal_exit.loop_control',
  'p-last-dynamic'  => 'nonlocal_exit.loop_control',
  'p-next'          => 'nonlocal_exit.loop_control',
  'p-redo'          => 'nonlocal_exit.loop_control',
  'p-goto-sub'      => 'nonlocal_exit.goto',
  'p-goto-computed' => 'nonlocal_exit.goto',
  'p-die'           => 'nonlocal_exit.die',
  'p-eval-block'    => 'nonlocal_exit.eval_block',
  'p-try'           => 'nonlocal_exit.eval_block',
  # string eval: a compiler must be reachable at run time
  'p-eval'          => 'string_eval.eval',
  'p-eval-thunk'    => 'string_eval.thunk',
  'p-evalbytes'     => 'string_eval.eval',
  # regex: which engine tier the program needs (the TIER classifier is not
  # ours — B5 owns it — so the manifest prints tier: unclassified)
  'p-regex'             => 'regex.literal',
  'p-regex-from-parts'  => 'regex.interpolated',
  'p-qr'                => 'regex.qr',
  'p-subst'             => 'regex.subst',
  'p-tr'                => 'regex.tr',
  'p-split'             => 'regex.split',
  # the phase model
  'p-BEGIN'                    => 'phase.begin',
  'p-CHECK'                    => 'phase.check',
  'p-eval-always'              => 'phase.eval_when',
  'p-run-compile-phase-blocks' => 'phase.run_blocks',
  # tie / overload
  'p-tie'   => 'tie', 'p-untie' => 'tie', 'p-tied' => 'tie',
  'p-register-overloads' => 'overload',
  # formats — `write` is the only format op PCL has
  'p-write' => 'formats',
  # XS
  'p-load-extension' => 'xs',
  # I/O: filehandles, directories, glob
  (map { ($_ => 'io') }
    qw(p-print p-say p-printf p-open p-sysopen p-close p-eof p-tell p-seek
       p-sysseek p-pipe p-select p-binmode p-read p-sysread p-syswrite
       p-fileno p-fcntl p-getc p-readline p-lock p-opendir p-readdir
       p-closedir p-rewinddir p-glob)),
  # processes and the shell
  (map { ($_ => 'process') }
    qw(p-exit p-system p-fork p-waitpid p-wait p-getppid p-kill p-exec
       p-getpgrp p-setpgrp p-getpriority p-backtick)),
);

# The assignment heads whose FIRST argument is the write target — used to spot
# a magic-global write, which is the other half of `dynamic_scope`.
my %ASSIGN_HEAD = map { ($_ => 1) }
  qw(p-setf p-scalar-= p-array-= p-hash-= p-list-= p-my-= p-array-fill
     p-hash-fill p-box-init p-incf p-decf p-pre++ p-post++ p-pre-- p-post--
     p-incf-raw p-decf-raw);

# A MAGIC-GLOBAL NAME, by shape (ir-spec §8).  Written as a pattern set
# because the population is open at the punctuation end: perl allows a
# punctuation name for every sigil, and the runtime owns them all (#506).
my @MAGIC_SHAPE = (
  qr/\A\$[0-9]+\z/,            # $1 .. $20
  qr/\A\$_\z/,                 # the topic
  qr/\A\|?[\$\@%]\^/,          # $^W, |$^E|, %^H
  qr/\A\|?[\$\@%][^A-Za-z_|]/, # $/ @- %+ |$"| — a punctuation name
  qr/\A[\@%](?:INC|ENV|SIG|ARGV)\z/,
  qr/\A\$(?:ARGV|0|@|\$|\?|!)\z/,
);
sub _is_magic_name {
  my ($n) = @_;
  return 0 if !defined $n || ref $n;
  for my $r (@MAGIC_SHAPE) { return 1 if $n =~ $r }
  return 0;
}

# The element places `elem-setf` competes for: a CL `setf` of one, versus
# `p-setf` of one.
my %ELEM_PLACE = map { ($_ => 1) } qw(p-aref p-gethash);

# ── Collection ────────────────────────────────────────────────────────────

# Called from Pl::Passes::run — the one place a lowered top-level form is
# finished.  Walks the tree; a Raw leaf's TEXT goes to the scanner.
sub note_form {
  return unless $ON;
  my ($form) = @_;
  _walk($form);
  return;
}

sub note_text {
  return unless $ON;
  my ($text) = @_;
  return unless defined $text && length $text;
  $TEXT_CHUNKS++;
  while ($text =~ /\(([^\s()"]+)/g) { $TEXT_HEAD{ _canon($1) }++ }
  # THE `local` SITE CORRECTION, measured s470bm on five spellings.  The whole
  # `local` family is v1 TEXT (a raw_wrap open), and it has TWO shapes:
  #
  #   local $g = 2;        (p-local-cell $g (p-box-for-local 2)   <- BOTH heads
  #   local @ga = (2);     (p-local-cell @ga (p-copy-array …)     <- one head
  #   local $h{a} = 3;     (p-local-hash-elem-init %h "a" 3       <- one head
  #   local $/ = ":";      (let (($/ (p-box-for-local ":")))      <- NO p-local
  #   local $_ = "x";      (let (($_ (p-box-for-local "x")))      <- NO p-local
  #
  # So `p-box-for-local` is sometimes the site's only marker and sometimes
  # merely the VALUE wrapper of a `p-local-*` call.  Counting both heads gave
  # 6 sites for 5 `local`s; counting only `p-local-*` gave 3.  The site count
  # is therefore (p-local-* heads) + (p-box-for-local) − (the wraps), and this
  # counts the wraps.
  $LOCAL_VALUE_WRAPS++
    while $text =~ /\(p-local-[^\s()]*\s+[^\s()]+\s+\(p-box-for-local\b/g;
  return;
}

# `pcl::p-qr` and `pcl:p-defpackage` are the same ops as `p-qr`/
# `p-defpackage`; the emitter writes a few heads package-qualified.  ONE
# canonicalisation, or the same op is counted under two names.
sub _canon {
  my ($n) = @_;
  $n =~ s/\Apcl::?//;
  return $n;
}

sub _walk {
  my ($f) = @_;
  return unless defined $f;
  if (!ref $f) { return }
  if (Pl::CLForm::is_raw($f))      { note_text($$f); return }
  if (Pl::CLForm::is_raw_wrap($f)) {
    note_text($f->{open});
    _walk($_) for @{ $f->{body} };
    return;
  }
  return unless ref $f eq 'ARRAY';
  my ($head, @args) = @$f;

  if (!ref $head) {
    my $h = _canon($head);
    if ($h ne 'list') {
      $HEAD{$h}++;
      _note_special($h, \@args);
    }
    else {
      # A headless list: a let-binding list, a lambda list, a facts plist.
      # Nothing to count as an op; its members are walked below.
    }
  }
  _walk($_) for @args;
  return;
}

# The head-specific readings: the declaration classes (ir-spec §2b.2a), the
# p-sub facts plist (§5.1), and the two element-write shapes.
sub _note_special {
  my ($h, $args) = @_;
  if ($h eq 'p-let')        { _note_let_entries($args->[0]) }
  elsif ($h eq 'p-raw-params') { _note_param_entries($args->[0]) }
  elsif ($h eq 'p-sub')     { $SUBS++; _note_sub_facts($args->[2]) }
  elsif ($h eq 'setf')      { $SETF_ELEM++  if _is_elem_place($args->[0]) }
  elsif ($h eq 'p-setf')    { $PSETF_ELEM++ if _is_elem_place($args->[0]) }
  if ($ASSIGN_HEAD{$h} && _is_magic_name($args->[0])) {
    $HEAD{'#magic-write'}++;
  }
  return;
}

sub _is_elem_place {
  my ($p) = @_;
  return 0 unless ref $p eq 'ARRAY' && !ref $p->[0];
  return $ELEM_PLACE{ _canon($p->[0]) } ? 1 : 0;
}

# `(p-let ((NAME CLASS INIT . FACTS) …) …)` — the bindings are a headless
# list of headless lists.
sub _note_let_entries {
  my ($bindings) = @_;
  return unless ref $bindings eq 'ARRAY' && !ref $bindings->[0]
             && $bindings->[0] eq 'list';
  for my $e (@$bindings[1 .. $#$bindings]) {
    next unless ref $e eq 'ARRAY' && !ref $e->[0] && $e->[0] eq 'list';
    my (undef, $name, $class, undef, @facts) = @$e;
    next if ref $class || !defined $class || $class !~ /\A:/;
    $PLET_CLASS{$class}++;
    for (my $i = 0; $i < @facts; $i += 2) {
      next if ref $facts[$i] || $facts[$i] !~ /\A:/;
      $PLET_FACT{ $facts[$i] }++;
    }
  }
  return;
}

# `(p-raw-params ((NAME CLASS) …) …)`
sub _note_param_entries {
  my ($params) = @_;
  return unless ref $params eq 'ARRAY' && !ref $params->[0]
             && $params->[0] eq 'list';
  for my $e (@$params[1 .. $#$params]) {
    next unless ref $e eq 'ARRAY' && !ref $e->[0] && $e->[0] eq 'list';
    my $class = $e->[2];
    next if ref $class || !defined $class || $class !~ /\A:/;
    $PARAM_CLASS{$class}++;
  }
  return;
}

# `(p-sub pl-NAME LAMBDA-LIST FACTS body…)` — FACTS is at a FIXED position
# and is a headless plist, possibly empty (ir-spec §5.1).
sub _note_sub_facts {
  my ($facts) = @_;
  return unless ref $facts eq 'ARRAY' && !ref $facts->[0]
             && $facts->[0] eq 'list';
  my @kv = @$facts[1 .. $#$facts];
  for (my $i = 0; $i < @kv; $i += 2) {
    next if ref $kv[$i] || $kv[$i] !~ /\A:/;
    $SUB_FACT{ $kv[$i] }++;
  }
  return;
}

# ── The report ───────────────────────────────────────────────────────────

sub _count { my ($h, @k) = @_; my $n = 0; $n += $h->{$_} // 0 for @k; return $n }

sub report {
  my (%o) = @_;
  # The head census: the runtime vocabulary by SPELLING, and everything else.
  # Both maps are printed, so a head can never be silently dropped.
  my %all;
  $all{$_} += $HEAD{$_}      for keys %HEAD;
  $all{$_} += $TEXT_HEAD{$_} for keys %TEXT_HEAD;
  my $magic_writes = delete($all{'#magic-write'}) // 0;
  my (%uses, %other);
  for my $k (keys %all) {
    if ($k =~ /\A%?p-/ || $k =~ /\A%pcl-/) { $uses{$k} = $all{$k} }
    else { $other{$k} = $all{$k} }
  }

  # The obligation classes, from the head census through %OBLIGATION.
  my %needs;
  for my $k (keys %all) {
    my $cls = $OBLIGATION{$k} or next;
    my ($group, $sub) = split /\./, $cls, 2;
    if (defined $sub) { $needs{$group}{$sub} += $all{$k} }
    else              { $needs{$group} += $all{$k} }
  }
  # The `local` site count, corrected for the value-wrapper double count
  # (see note_text).  Clamped at 0: an arithmetic correction must never make
  # a count negative, and if it ever would the shape assumption has changed —
  # which is what Pl/t/manifest-01.t's exact rows are there to catch.
  if (($needs{dynamic_scope}{local} // 0) && $LOCAL_VALUE_WRAPS) {
    my $n = $needs{dynamic_scope}{local} - $LOCAL_VALUE_WRAPS;
    $needs{dynamic_scope}{local} = $n > 0 ? $n : 0;
  }
  $needs{dynamic_scope}{magic_global_write} = $magic_writes;
  # Every class is PRESENT with a zero, never absent: "the program does not
  # need tie" is an answer, and a missing key would make a consumer guess.
  $needs{$_} //= 0 for qw(tie overload formats xs io process);
  for my $g (qw(dynamic_scope nonlocal_exit string_eval regex phase)) {
    $needs{$g} //= {};
  }
  $needs{dynamic_scope}{local}       //= 0;
  $needs{nonlocal_exit}{$_}          //= 0 for qw(return loop_control goto die eval_block);
  $needs{string_eval}{$_}            //= 0 for qw(eval thunk);
  $needs{regex}{$_}                  //= 0 for qw(literal interpolated qr subst tr split);
  $needs{regex}{tier}                  = 'unclassified';
  $needs{phase}{$_}                  //= 0 for qw(begin check eval_when run_blocks);

  # The Kind-A/Kind-B licences.  `fired`/`candidates` wherever the tree can
  # say both — a bare `fired` where it cannot.
  my $scalarish = _count(\%PLET_CLASS, ':box', ':scalar', ':num', ':str', ':str-buffer');
  my %facts = (
    'raw-slot' => {
      fired      => _count(\%PLET_CLASS, ':scalar', ':num', ':str', ':str-buffer'),
      candidates => $scalarish,
    },
    'raw-numeric' => {
      fired      => _count(\%PLET_CLASS, ':num', ':str'),
      candidates => $scalarish,
    },
    'str-buffer' => {
      fired      => _count(\%PLET_CLASS, ':str-buffer'),
      candidates => $scalarish,
    },
    'foreach-range' => {
      fired      => _count(\%all, 'p-foreach-range', 'p-foreach-range-raw'),
      candidates => _count(\%all, 'p-foreach', 'p-foreach-raw',
                                  'p-foreach-range', 'p-foreach-range-raw'),
    },
    'foreach-raw' => {
      fired      => _count(\%all, 'p-foreach-raw', 'p-foreach-range-raw'),
      candidates => _count(\%all, 'p-foreach', 'p-foreach-raw',
                                  'p-foreach-range', 'p-foreach-range-raw'),
    },
    'local-push' => {
      fired      => _count(\%all, '%p-push1'),
      candidates => _count(\%all, '%p-push1', 'p-push'),
    },
    'classic-sort' => {
      fired      => _count(\%all, '%p-sort-classic'),
      candidates => _count(\%all, '%p-sort-classic', 'p-sort'),
    },
    'tail-return' => {
      fired      => _count(\%all, 'p-tail-value'),
      candidates => _count(\%all, 'p-tail-value', 'p-return'),
    },
    'elem-setf' => {
      fired      => $SETF_ELEM,
      candidates => $SETF_ELEM + $PSETF_ELEM,
    },
    # insensitive-call's licence is a SUB fact, not a call-site shape the tree
    # can count both sides of: what the IR carries is the proof.
    'insensitive-call' => {
      fired      => $SUB_FACT{':wantarray-insensitive'} // 0,
      candidates => $SUBS,
    },
    # The declaration/parameter/sub verdicts themselves (#1035) — a fast
    # backend's raw material (plan-speed-and-ir §B.3).
    'declaration_classes' => { %PLET_CLASS },
    'declaration_facts'   => { %PLET_FACT },
    'parameter_classes'   => { %PARAM_CLASS },
    'sub_facts'           => { subs => $SUBS, %SUB_FACT },
  );

  return {
    file        => $o{file},
    mode        => $o{mode},
    generation  => $o{generation},
    manifest_version => $VERSION,
    # The v1-text chunks the head census could only REGEX, not walk.  Nonzero
    # means `uses` is approximate for those chunks and says so out loud.
    text_scanned => $TEXT_CHUNKS,
    uses        => \%uses,
    uses_other  => \%other,
    needs       => \%needs,
    facts       => \%facts,
  };
}

1;

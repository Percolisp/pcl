#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-host-leak.pl — THE HOST-LEAK CENSUS (task #1172;
# docs/plan-speed-and-ir-s470.md Part B item B4; the measured whitelist is
# normative in docs/ir-spec.md §11b).
#
# RUN IT (from the repo root):
#     tools/ir-host-leak.pl                    # the 111 perl-tests files
#     tools/ir-host-leak.pl --list FILE        # a population, one path per line
#     tools/ir-host-leak.pl --module lib/*.pm  # transpile in module mode
#     tools/ir-host-leak.pl --unclassified     # print the unknown-token census
#                                              # instead of the leak report
#                                              # (this is how §11b was measured)
#     tools/ir-host-leak.pl --jobs N           # parallel transpiles (default 8)
#
# WHEN TO RUN IT: after a change to `cl/**` that adds or renames an export, and
# after a change to `Pl/**` that changes what the emitter can put in the output.
# A leak is not visible to any other instrument — the gate runs the CL on SBCL,
# where a host symbol works perfectly.
#
# THE CLAIM IT CHECKS.  A PCL-emitted file must contain only these
# vocabularies, and nothing else:
#   (i)   the runtime's exported names — read from docs/ir-op-inventory.tsv,
#         so the two Part-B instruments share ONE list (task #1170);
#   (ii)  a WHITELISTED kernel of CL special forms and macros (%KERNEL below,
#         measured from the corpus, not guessed) — this is the set a C or JS
#         backend must implement, and it is written down in ir-spec §11b with
#         each form's JS and C rendering;
#   (iii) literals — numbers, strings, characters, keywords;
#   (iv)  the PROGRAM's own identifiers, by shape: a perl sigil name (`$x`,
#         `@a`, `%h`), a `pl-`/`plc-` sub name, a pipe-quoted token, a
#         package-qualified name, the `%_args`/`%_...` internals.  These are
#         DATA, not host constructs: a backend renames them and moves on.
# Anything else is a LEAK: `sb-*`, a bare CL function reaching the output
# through a seam, an SBCL-internal symbol.  ir-spec §11 lists exactly one
# licensed SBCL name (`p-double-inf`), which is an export and so is allowed by
# (i); every other one is news.
#
# WHY A SCANNER AND NOT A READER.  The question is per-TOKEN and needs
# file:line, and the emitted CL contains raw control characters and NUL bytes
# inside string literals (generated-cl-ir-review §3.2), so a line-oriented
# tool must track string state itself anyway.  The scanner below tracks
# strings, `;` line comments, `#| |#` block comments, `#\c` character literals
# and `|…|` multiple-escape sections — the same five states
# tools/check-parens.lisp gets from SBCL's own reader.
use strict;
use warnings;
use FindBin qw($RealBin);
use Getopt::Long;

my $root = "$RealBin/..";
my ($list_file, $module_mode, $unclassified, $jobs, $quiet)
  = (undef, 0, 0, 8, 0);
my $help = 0;
GetOptions('list=s' => \$list_file, 'module' => \$module_mode,
           'unclassified' => \$unclassified, 'jobs=i' => \$jobs,
           'quiet' => \$quiet, 'help' => \$help)
  or die usage();
if ($help) { print usage(); exit 0 }
sub usage {
  return <<'U';
usage: tools/ir-host-leak.pl [options] [FILE...]
  --list FILE       read input paths from FILE (one per line)
  --module          transpile with --module (a .pm population)
  --unclassified    print the unknown-token census instead of the leak report
  --jobs N          parallel transpiles (default 8)
  --quiet           only the verdict line
  --help            this message
  default input: perl-tests/*.t (corpus-diff's 111 files)
U
}

# ── (ii) THE CL KERNEL a backend must implement ──────────────────────────
# MEASURED: every non-runtime token the corpus's emitted CL actually contains,
# classified by hand once, and kept here as the closed set.  ir-spec §11b is
# this table with each form's JS and C rendering; the two must agree, and
# Pl/t/ir-host-leak-01.t is what makes a disagreement loud.
my %KERNEL = map { ($_ => 1) } qw(
  block return-from tagbody go catch throw unwind-protect progn prog1 prog2
  let let* lambda function funcall apply multiple-value-bind values
  multiple-value-list nth-value if when unless cond case ecase typecase and
  or not null quote setq setf psetf incf decf push pop
  declare declaim locally the eval-when defvar defparameter defconstant
  define-symbol-macro defun defmacro in-package defclass defmethod defgeneric
  make-instance find-class
  list list* cons car cdr first rest append length aref elt svref
  vector make-array vector-push-extend make-hash-table gethash equal eql equalp
  ignore ignorable dynamic-extent notinline inline optimize special ftype type
  speed safety debug space compilation-speed
  &optional &rest &body &key &aux &allow-other-keys
  t nil otherwise
  *package*
);

# ── (iv) a PROGRAM IDENTIFIER, by shape ──────────────────────────────────
# Data, not a host construct.  Ordered most-specific first; each pattern is
# one emission convention (ir-spec §1, §7.1, §2b.3).
my @IDENT_SHAPE = (
  qr/\A\|/,                       # a pipe-quoted token — verbatim by rule
  qr/\A[\$\@&][^\s()]*\z/,        # a perl sigil name, incl. $x__lex__7
  qr/\A%(?!p-|pcl-)[^\s()]*\z/,   # a perl HASH name — but never a `%p-`/
                                  # `%pcl-` runtime internal, which must be an
                                  # export to be legal (see _is_ident)
  qr/\Apl[c]?-/,                  # a sub name (pl-foo) or a constant (plc-)
  qr/\A--pcl-/,                   # a compiler TEMP (--pcl-if-ret--0)
  qr/\A[A-Z][A-Za-z0-9_]*\z/,     # a bareword filehandle / package designator
  qr/\A#[Pp]\z/,                  # #P"path" — the preamble's pathname
);

# A PACKAGE-QUALIFIED name is a program identifier — EXCEPT when the package
# is `pcl` itself.  `pcl::%pcl-to-integer` is not a perl name at all, it is a
# runtime INTERNAL reached from the emitter, and the "qualified therefore
# data" rule was letting the whole class through unseen (measured s470bm: the
# `use integer` emission uses four of them).  So a `pcl::`-qualified token is
# legal only if the name it qualifies is an EXPORT.
sub _is_ident {
  my ($t, $exports) = @_;
  return 0 if $t =~ /\Apcl::?/;      # settled by the export test, not by shape
  for my $r (@IDENT_SHAPE) { return 1 if $t =~ $r }
  return 1 if $t =~ /\A[A-Za-z_][A-Za-z0-9_]*::/;   # a perl package
  return 0;
}

# A LITERAL token: a number in any of the shapes the emitter produces, or a
# keyword.  (Strings and characters never reach the classifier — the scanner
# consumes them.)
sub _is_literal {
  my ($t) = @_;
  return 1 if $t =~ /\A:/;                       # keyword
  return 1 if $t =~ /\A[-+]?[0-9]+\.?\z/;        # integer, 1.
  return 1 if $t =~ /\A[-+]?[0-9]*\.[0-9]+(?:[dDeEsSfF][-+]?[0-9]+)?\z/;
  return 1 if $t =~ /\A[-+]?[0-9]+(?:[dDeEsSfF][-+]?[0-9]+)\z/;
  return 1 if $t =~ m{\A[-+]?[0-9]+/[0-9]+\z};   # a ratio
  # RADIX literals.  The emitter writes character codes and bit masks as
  # `#xFF` (measured: 400+ of them in perl-tests/pack.t alone), which is CL
  # syntax for a NUMBER, not a symbol — ir-spec §11b says a backend reads them
  # as hex/octal/binary integers.
  return 1 if $t =~ /\A#[xX][-+]?[0-9a-fA-F]+\z/;
  return 1 if $t =~ /\A#[oO][-+]?[0-7]+\z/;
  return 1 if $t =~ /\A#[bB][-+]?[01]+\z/;
  return 1 if $t =~ /\A#[0-9]+[rR][-+]?[0-9a-zA-Z]+\z/;
  return 0;
}

# ── The runtime's exported names, from B1's generated TSV ────────────────
sub runtime_exports {
  my $tsv = "$root/docs/ir-op-inventory.tsv";
  open my $fh, '<:raw', $tsv
    or die "ir-host-leak.pl: cannot read $tsv: $!\n"
         . "  it is generated — run tools/ir-inventory.pl\n";
  my %n;
  my $header = 0;
  while (my $l = <$fh>) {
    next if $l =~ /\A#/;
    if (!$header) { $header = 1; next }          # the column header row
    my ($name) = split /\t/, $l;
    next unless defined $name && length $name;
    $n{$name} = 1;
    # The emitter writes a few heads package-QUALIFIED (`pcl::p-qr`); the
    # qualified spelling is the same op.
    $n{"pcl::$name"} = 1;
    $n{"pcl:$name"}  = 1;
  }
  close $fh;
  die "ir-host-leak.pl: the inventory TSV has no rows — regenerate it\n"
    unless keys %n;
  # (The `p-qr` / `%pcl-loop-tag` exceptions that stood here are GONE, task
  # #1177: both — and the three other names the emitter wrote qualified — are
  # exports now, so the inventory rows above cover them and this tool needs no
  # per-name knowledge.  An exception here would hide exactly the class #1177
  # was about.)
  return \%n;
}

# ── The scanner ──────────────────────────────────────────────────────────
# Yields [token, line] for every symbol-ish token outside strings, comments
# and character literals.  A `|…|` section is absorbed into its token.
sub scan_tokens {
  my ($text) = @_;
  my @out;
  my $line = 1;
  my $i = 0;
  my $len = length $text;
  # POSITION, because the same token means different things in different ones
  # and the plan's question is about two of the four:
  #   op      the first element of a list whose parent is NOT a headless list
  #           — an operator, i.e. something the host is asked to CALL
  #   quoted  the token right after `'` or `#'` — a symbol used as DATA (an
  #           operator selector, a loop tag, a hash-table test)
  #   bind    the first element of a list inside a HEADLESS list (`((x 1))`)
  #           — a binding NAME: a `let`/`p-let` variable, never a call
  #   arg     everything else — a variable read, a label, a tagbody tag
  # Without this a perl LABEL (`(block start …)`) and a `let` temp are the
  # same SHAPE as a leaked CL function, and the census cannot tell them apart.
  # A list is HEADLESS when its own first element is a list: that is exactly
  # the shape of every binding list, lambda list and plist the emitter writes.
  my @frame;            # one per open paren; {first => undef|'list'|'atom'}
  my $quoted_next = 0;
  while ($i < $len) {
    my $c = substr($text, $i, 1);
    if ($c eq "\n") { $line++; $i++; next }
    if ($c =~ /\s/) { $i++; next }
    if ($c eq ';') {                                    # line comment
      my $nl = index($text, "\n", $i);
      $i = $nl < 0 ? $len : $nl;
      next;
    }
    if ($c eq '"') {                                    # string
      $i++;
      while ($i < $len) {
        my $d = substr($text, $i, 1);
        if ($d eq "\\") { $line++ if substr($text, $i + 1, 1) eq "\n"; $i += 2; next }
        if ($d eq "\n") { $line++ }
        if ($d eq '"')  { $i++; last }
        $i++;
      }
      $frame[-1]{first} //= 'atom' if @frame;
      $quoted_next = 0;
      next;
    }
    if ($c eq '#') {
      my $d = substr($text, $i + 1, 1);
      if ($d eq '\\') {                                 # character literal
        $i += 2;
        # a named character (#\Newline) runs to the next delimiter
        $i++ while $i < $len && substr($text, $i, 1) !~ /[\s()'";]/;
        $frame[-1]{first} //= 'atom' if @frame;
        $quoted_next = 0;
        next;
      }
      if ($d eq '|') {                                  # block comment
        my $depth = 1;
        $i += 2;
        while ($i < $len && $depth) {
          my $two = substr($text, $i, 2);
          if ($two eq '#|') { $depth++; $i += 2; next }
          if ($two eq '|#') { $depth--; $i += 2; next }
          $line++ if substr($text, $i, 1) eq "\n";
          $i++;
        }
        next;
      }
      if ($d eq "'") { $quoted_next = 1; $i += 2; next }  # #'foo
      # #P #( #x … fall through to the token scanner
    }
    if ($c eq '(') {
      $frame[-1]{first} //= 'list' if @frame;           # the parent is headless
      push @frame, { first => undef };
      $i++;
      next;
    }
    if ($c eq ')') { pop @frame; $quoted_next = 0; $i++; next }
    if ($c eq "'") { $quoted_next = 1; $i++; next }
    if ($c =~ /[`,]/) {              $i++; next }       # backquote, unquote
    # a token: runs to the next delimiter, absorbing |…| sections whole
    my $start = $i;
    my $tok = '';
    while ($i < $len) {
      my $d = substr($text, $i, 1);
      if ($d eq '|') {                                  # multiple escape
        $tok .= $d; $i++;
        while ($i < $len) {
          my $e = substr($text, $i, 1);
          if ($e eq "\\") { $tok .= substr($text, $i, 2); $i += 2; next }
          $tok .= $e; $i++;
          last if $e eq '|';
          $line++ if $e eq "\n";
        }
        next;
      }
      last if $d =~ /[\s()'";]/;
      if ($d eq "\\") { $tok .= substr($text, $i, 2); $i += 2; next }
      $tok .= $d; $i++;
    }
    if (length $tok) {
      my $pos;
      if ($quoted_next)  { $pos = 'quoted' }
      elsif (!@frame)    { $pos = 'arg' }               # top level, bare atom
      elsif (defined $frame[-1]{first}) { $pos = 'arg' }
      else {
        # the first element of this list: an OPERATOR, unless the enclosing
        # list is headless — then it is a binding name
        $pos = (@frame > 1 && ($frame[-2]{first} // '') eq 'list')
             ? 'bind' : 'op';
      }
      $frame[-1]{first} //= 'atom' if @frame;
      push @out, [$tok, $line, $pos];
    }
    $quoted_next = 0;
    $i = $start + 1 if $i == $start;                    # never stall
  }
  return \@out;
}

# ── Per-file classification ──────────────────────────────────────────────
sub classify_file {
  my ($path, $exports) = @_;
  my $extra = $module_mode ? '--module ' : '';
  my $cl = `cd "$root" && ./pl2cl $extra"$path" 2>/dev/null`;
  return { file => $path, transpile_failed => 1 } unless defined $cl && length $cl;
  my $toks = scan_tokens($cl);
  my (%leak, %unknown);
  for my $t (@$toks) {
    my ($tok, $line, $pos) = @$t;
    next if $exports->{$tok};
    # THE EMPTY-ESCAPE READER RULE (measured s470bm, ir-spec §11b): a `|…|`
    # section with nothing in it contributes nothing to the symbol's name, so
    # the emitted token `p-||` names the symbol whose name is "P-" — i.e. the
    # op the inventory prints as `p-`.  A text-parsing backend must fold them
    # or it will not find the operator at all.
    if ($tok =~ /\|\|/) {
      (my $folded = $tok) =~ s/\|\|//g;
      next if $exports->{$folded} || $KERNEL{$folded};
    }
    next if $KERNEL{$tok};
    next if _is_literal($tok);
    next if _is_ident($tok);
    # An unclassified token in ARGUMENT position is the program's own data —
    # a perl LABEL (`(block start …)`), a compiler temp, a `loop` keyword.
    # It cannot be a call, so it cannot be a host FUNCTION leak, and by shape
    # it is indistinguishable from a label; only OPERATOR position (it is
    # being called) and QUOTED position (it is a symbol the runtime will
    # dispatch on) are claims about the host.  That is exactly the pair the
    # plan asks for.  Reported separately with --unclassified.
    if ($pos eq 'arg' || $pos eq 'bind') { $unknown{"$pos:$tok"}++; next }
    push @{ $leak{"$pos:$tok"} }, $line;
    $unknown{"$pos:$tok"}++;
  }
  return { file => $path, leaks => \%leak, unknown => \%unknown,
           tokens => scalar @$toks };
}

# ── Run ──────────────────────────────────────────────────────────────────
my @files = @ARGV;
if ($list_file) {
  open my $fh, '<', $list_file or die "ir-host-leak.pl: $list_file: $!\n";
  while (my $l = <$fh>) { chomp $l; push @files, $l if length $l }
  close $fh;
}
@files = sort glob("$root/perl-tests/*.t") unless @files;
@files = map { my $p = $_; $p =~ s{\A\Q$root\E/}{}; $p } @files;

my $exports = runtime_exports();

# Parallel by fork, results back through temp files (one line per leak).
my @results;
if ($jobs > 1 && @files > 1) {
  require File::Temp;
  my $dir = File::Temp::tempdir(CLEANUP => 1);
  my @queue = @files;
  my %pid;
  my $slot = 0;
  while (@queue || keys %pid) {
    while (@queue && keys(%pid) < $jobs) {
      my $f = shift @queue;
      my $out = "$dir/" . $slot++;
      my $pid = fork();
      die "ir-host-leak.pl: fork: $!\n" unless defined $pid;
      if (!$pid) {
        my $r = classify_file($f, $exports);
        open my $oh, '>:raw', $out or exit 1;
        if ($r->{transpile_failed}) { print {$oh} "FAIL\t$f\n" }
        else {
          print {$oh} "OK\t$f\t$r->{tokens}\n";
          for my $tok (sort keys %{ $r->{leaks} }) {
            print {$oh} "LEAK\t$f\t$tok\t"
              . join(',', @{ $r->{leaks}{$tok} }) . "\n";
          }
        }
        close $oh;
        exit 0;
      }
      $pid{$pid} = $out;
    }
    my $done = wait();
    last if $done < 0;
    my $out = delete $pid{$done};
    next unless defined $out && -e $out;
    open my $ih, '<:raw', $out or next;
    push @results, <$ih>;
    close $ih;
  }
}
else {
  for my $f (@files) {
    my $r = classify_file($f, $exports);
    if ($r->{transpile_failed}) { push @results, "FAIL\t$f\n"; next }
    push @results, "OK\t$f\t$r->{tokens}\n";
    for my $tok (sort keys %{ $r->{leaks} }) {
      push @results, "LEAK\t$f\t$tok\t" . join(',', @{ $r->{leaks}{$tok} }) . "\n";
    }
  }
}

my (%by_tok, %by_file, @failed, $n_ok, $n_tokens);
for my $l (@results) {
  chomp $l;
  my @f = split /\t/, $l;
  if ($f[0] eq 'FAIL') { push @failed, $f[1]; next }
  if ($f[0] eq 'OK')   { $n_ok++; $n_tokens += $f[2]; next }
  if ($f[0] eq 'LEAK') {
    my ($file, $tok, $lines) = @f[1, 2, 3];
    my @l = split /,/, ($lines // '');
    $by_tok{$tok}{count} += scalar @l;
    $by_tok{$tok}{files}{$file} = $l[0];
    push @{ $by_file{$file} }, [$tok, $l[0]];
  }
}

if ($unclassified) {
  printf "unknown-token census over %d files (%d tokens scanned)\n",
    $n_ok, $n_tokens;
  for my $tok (sort { $by_tok{$b}{count} <=> $by_tok{$a}{count} || $a cmp $b }
               keys %by_tok) {
    printf "%8d  %-34s in %d file(s), first %s:%s\n",
      $by_tok{$tok}{count}, $tok, scalar keys %{ $by_tok{$tok}{files} },
      (sort keys %{ $by_tok{$tok}{files} })[0],
      $by_tok{$tok}{files}{ (sort keys %{ $by_tok{$tok}{files} })[0] };
  }
  exit 0;
}

if (!$quiet) {
  printf "%d files transpiled, %d tokens scanned, %d failed to transpile\n",
    $n_ok, $n_tokens, scalar @failed;
  print "  transpile FAILED: $_\n" for @failed;
  if (%by_tok) {
    print "\nHOST LEAKS — a symbol that is neither a runtime export, a\n"
        . "whitelisted CL kernel form (ir-spec §11b), a literal, nor one of\n"
        . "the program's own identifiers:\n\n";
    for my $tok (sort { $by_tok{$b}{count} <=> $by_tok{$a}{count} || $a cmp $b }
                 keys %by_tok) {
      my @fs = sort keys %{ $by_tok{$tok}{files} };
      printf "  %-32s %5d occurrence(s) in %d file(s)\n",
        $tok, $by_tok{$tok}{count}, scalar @fs;
      for my $f (@fs[0 .. ($#fs < 2 ? $#fs : 2)]) {
        printf "      %s:%s\n", $f, $by_tok{$tok}{files}{$f};
      }
      printf "      … and %d more file(s)\n", scalar(@fs) - 3 if @fs > 3;
    }
  }
}
printf "%s: %d distinct leaked symbol(s) over %d file(s)\n",
  (%by_tok ? 'LEAKS' : 'CLEAN'), scalar keys %by_tok, $n_ok;
exit(%by_tok ? 1 : 0);

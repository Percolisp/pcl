#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-data-form-01.t — THE IR AS DATA and the three emission facts that came
# with it (tasks #1210–#1215, Part B item B5 + §B.3; docs/ir-spec.md §12b).
#
# Four things are guarded here, each of which is a promise to a BACKEND AUTHOR
# and none of which any other row in the tree would notice breaking:
#
#   1. `pl2cl --emit-sexp` is READABLE BY A ~50-LINE READER.  The claim in
#      ir-spec §12b is not "it is s-expressions", it is "you need no CL reader
#      rules", so the row implements that reader HERE, in 50 lines of plain
#      Perl, parses the emission with it and RE-PRINTS — and the re-print must
#      be byte-identical to what PCL wrote.  A reader this file cannot write is
#      a promise the spec cannot make.
#   2. The regex literal's TIER (task #1211) — one construct per tier, so the
#      classifier's three answers are all reachable from real Perl source.
#   3. `p-esc` (task #1212) for every control character, and its DECODE:
#      the payload must denote exactly the string the program wrote.
#   4. `--facts` (#1213) names the licences, and `:needs` (#1214) answers per
#      sub — including the EMPTY answer, which is the half a target uses.
#
# Cost: one `pl2cl` per fixture, no SBCL — wall time is the gate's metric
# (CLAUDE.md §6).

use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use File::Temp qw(tempdir);

my $root  = "$RealBin/../..";
my $tmp   = tempdir(CLEANUP => 1);

sub write_pl {
  my ($name, $src) = @_;
  my $p = "$tmp/$name";
  open my $fh, '>', $p or die "ir-data-form-01.t: $p: $!";
  print {$fh} $src;
  close $fh;
  return $p;
}

sub run_pl2cl {
  my (@args) = @_;
  my $out = `cd "$root" && ./pl2cl @args 2>/dev/null`;
  return defined $out ? $out : '';
}

# ── THE ir-spec §12b READER, in Perl ─────────────────────────────────────
# This is the reference implementation the spec quotes, and it is the whole
# claim: symbols are `|…|` with `\` escaping, strings carry six escapes,
# numbers are decimal, and nothing else needs knowing.  A node is
# ['sym', NAME] / ['str', VALUE] / ['num', TEXT] / ['list', @nodes].
sub df_read_line {
  my ($s) = @_;
  my ($node, $rest) = df_node($s);
  $rest =~ s/\A\s+//;
  die "trailing text: $rest\n" if length $rest;
  return $node;
}

sub df_node {
  my ($s) = @_;
  $s =~ s/\A\s+//;
  if ($s =~ s/\A\(//) {
    my @kids;
    while (1) {
      $s =~ s/\A\s+//;
      last if $s =~ s/\A\)//;
      die "unterminated list\n" unless length $s;
      my ($k, $rest) = df_node($s);
      push @kids, $k;
      $s = $rest;
    }
    return (['list', \@kids], $s);
  }
  if ($s =~ s/\A\|//) {
    my $name = '';
    while (length $s) {
      my $c = substr($s, 0, 1);
      $s = substr($s, 1);
      if ($c eq "\\") { $name .= substr($s, 0, 1); $s = substr($s, 1); next }
      last if $c eq '|';
      $name .= $c;
    }
    return (['sym', $name], $s);
  }
  if ($s =~ s/\A"//) {
    my $v = '';
    while (length $s) {
      my $c = substr($s, 0, 1);
      $s = substr($s, 1);
      last if $c eq '"';
      if ($c ne "\\") { $v .= $c; next }
      my $d = substr($s, 0, 1);
      $s = substr($s, 1);
      if    ($d eq 'n') { $v .= "\n" }
      elsif ($d eq 't') { $v .= "\t" }
      elsif ($d eq 'r') { $v .= "\r" }
      elsif ($d eq 'u') {
        my $cp = hex(substr($s, 0, 4)); $s = substr($s, 4);
        if ($cp >= 0xD800 && $cp <= 0xDBFF && $s =~ /\A\\u([0-9A-Fa-f]{4})/) {
          my $lo = hex($1);
          if ($lo >= 0xDC00 && $lo <= 0xDFFF) {
            $cp = 0x10000 + (($cp - 0xD800) << 10) + ($lo - 0xDC00);
            $s = substr($s, 6);
          }
        }
        $v .= chr($cp);
      }
      else { $v .= $d }          # \\ and \"
    }
    return (['str', $v], $s);
  }
  $s =~ s/\A([^\s()]+)// or die "not a token: $s\n";
  return (['num', $1], $s);
}

# The re-printer: the inverse of the reader, so a round trip is a byte compare.
sub df_print {
  my ($n) = @_;
  return '(' . join(' ', map { df_print($_) } @{ $n->[1] }) . ')' if $n->[0] eq 'list';
  if ($n->[0] eq 'sym') { (my $x = $n->[1]) =~ s/([|\\])/\\$1/g; return "|$x|" }
  return $n->[1] if $n->[0] eq 'num';
  my $out = '"';
  for my $ch (split //, $n->[1]) {
    my $cp = ord $ch;
    if    ($ch eq "\\") { $out .= '\\\\' }
    elsif ($ch eq '"')  { $out .= '\\"' }
    elsif ($ch eq "\n") { $out .= '\\n' }
    elsif ($ch eq "\t") { $out .= '\\t' }
    elsif ($ch eq "\r") { $out .= '\\r' }
    elsif ($cp >= 0x20 && $cp < 0x7F) { $out .= $ch }
    elsif ($cp > 0xFFFF) {
      my $v = $cp - 0x10000;
      $out .= sprintf('\\u%04X\\u%04X', 0xD800 + ($v >> 10), 0xDC00 + ($v & 0x3FF));
    }
    else { $out .= sprintf('\\u%04X', $cp) }
  }
  return $out . '"';
}

# ── 1. The round trip ────────────────────────────────────────────────────
# A fixture with something from every shape the printer has to spell: a
# string with escapes, a non-ASCII character, a radix literal, a quoted
# symbol, a keyword plist, a regex literal, a heredoc, a sub with facts.
my $rt_src = <<'PERL';
my $s = "tab\there\nand \x{263a} and \\ and \"q\"";
my @a = (0x2a, 0777, 3.5, 1e3);
my %h = (k => 1);
sub f { my $x = shift; return $x + 1 }
print f($a[0]), $s, "\n";
if ($s =~ /(\w+)\s+/) { print "$1\n" }
$s =~ s/tab/TAB/g;
print <<EOT;
body line
EOT
PERL
my $rt = write_pl('rt.pl', $rt_src);
my $sexp = run_pl2cl('--emit-sexp', $rt);
ok(length $sexp, 'emit-sexp produced output');

my @lines = grep { /\S/ } split /\n/, $sexp;
cmp_ok(scalar @lines, '>=', 4, 'one top-level form per line, several of them');
is(scalar(grep { /^\(/ } @lines), scalar @lines,
   'every line is a form (no comments, no continuation)');
ok(!grep { /[^\x00-\x7f]/ } @lines, 'the data form is 7-bit');

my ($read_ok, $rt_ok, $bad) = (0, 0, '');
for my $l (@lines) {
  my $n = eval { df_read_line($l) };
  if (!$n) { $bad = "read failed: $@ on $l"; last }
  $read_ok++;
  my $back = df_print($n);
  if ($back ne $l) { $bad = "round trip differs:\n  in:  $l\n  out: $back"; last }
  $rt_ok++;
}
is($read_ok, scalar @lines, "the 50-line reader parsed all $read_ok lines")
  or diag($bad);
is($rt_ok, scalar @lines, 're-printing every form is byte-identical')
  or diag($bad);

# The provenance header and the census trailer are part of the grammar.
like($lines[0], qr/^\(\|p-data-form\| 1 /, 'first form is the provenance header');
like($lines[-1], qr/^\(\|p-data-form-end\| \d+ \d+\)$/,
     'last form is the census trailer (forms, islands)');
like($sexp, qr/\Q(|p-bucket|\E/, 'bucket markers name the section and phase');

# The default emission must not move: --emit-sexp is a SECOND printer.
my $cl_plain = run_pl2cl($rt);
like($cl_plain, qr/^;;; pcl: pipeline=v2/, 'the CL path still emits CL');
unlike($cl_plain, qr/p-data-form/, 'and carries none of the data form');

# ── 2. The regex TIER, one construct per tier ────────────────────────────
my %tier_case = (
  native  => 'my $x = "a"; $x =~ /(\w+)\s*/;',
  pcre    => 'my $x = "a"; $x =~ /a*+/;',
  refused => 'my $x = "a"; $x =~ /a(?{ 1 })/;',
  dynamic => 'my $p = "a"; my $x = "a"; $x =~ /$p/;',
);
for my $t (sort keys %tier_case) {
  my $f = write_pl("tier-$t.pl", $tier_case{$t} . "\n");
  my $cl = run_pl2cl($f);
  like($cl, qr/:tier :\Q$t\E\b/, "a $t-tier construct emits :tier :$t");
}
# The tier is a FACT, not a gate: PCL runs every tier.
my $ref_f = write_pl('tier-run.pl', "my \$x='a'; print((\$x =~ /a(?{1})/) ? 1 : 0), \"\\n\";\n");
like(run_pl2cl($ref_f), qr/p-regex/, 'a :refused literal is still emitted, not declined');

# The keyword form itself, for each of the five entry points.
my $rx = write_pl('rx.pl', <<'PERL');
my $s = "ab";
$s =~ /a/;
my $q = qr/a/i;
$s =~ s/a/b/g;
$s =~ tr/a/b/;
my $p = "a"; $s =~ /$p/;
PERL
my $rxcl = run_pl2cl($rx);
like($rxcl, qr/\(p-regex :pat "a" :flags "" :tier :native\)/, 'p-regex keyword form');
like($rxcl, qr/p-qr :pat "a" :flags "i" :tier :native/,        'p-qr keyword form');
like($rxcl, qr/p-subst :pat "a" :rep "b" :flags "g" :tier :native/, 'p-subst keyword form');
like($rxcl, qr/p-tr :from "a" :to "b" :flags ""/,              'p-tr keyword form (no tier)');
like($rxcl, qr/p-regex-from-parts :pat \$p :flags "" :tier :dynamic/,
     'p-regex-from-parts keyword form');

# ── 3. p-esc, per control character ──────────────────────────────────────
# Keyed by the PERL escape written in the fixture; the value is the exact
# text the emission must contain, compared with index() rather than a regex
# (the payload is backslash-dense and a regex of it is unreadable and,
# measured, easy to get wrong by one level of quoting).
my @ctrl = (
  [ '\n',     'A\\\\nB'      ],
  [ '\t',     'A\\\\tB'      ],
  [ '\r',     'A\\\\rB'      ],
  [ '\x00',   'A\\\\u0000B'  ],
  [ '\x07',   'A\\\\u0007B'  ],
  [ '\x1b',   'A\\\\u001BB'  ],
);
my $ctrl_i = 0;
for my $c (@ctrl) {
  my ($esc, $want) = @$c;
  my $f = write_pl('esc-' . $ctrl_i++ . '.pl', "my \$x = \"A${esc}B\";\n");
  my $cl = run_pl2cl($f);
  ok(index($cl, "(p-esc \"$want\")") >= 0,
     "a literal $esc is emitted as (p-esc \"$want\")")
    or diag("emission tail: " . substr($cl, -200));
}
# A literal WITHOUT a control character stays a plain string — the common case
# must not gain a wrapper.
my $plain = write_pl('esc-plain.pl', "my \$x = \"hello \\\\ \\\"q\\\"\";\n");
my $plaincl = run_pl2cl($plain);
unlike($plaincl, qr/p-esc/, 'a control-character-free literal is a plain literal');
# Non-ASCII stays readable in the CL file (only < 0x20 triggers p-esc).
my $uni = write_pl('esc-uni.pl', "my \$x = \"sm\\x{263a}ile\";\n");
unlike(run_pl2cl($uni), qr/p-esc/, 'a non-ASCII literal needs no p-esc');
# And the emitted file is LINE-ORIENTED: no CL string may carry a raw newline.
my $nl = write_pl('esc-nl.pl', "print \"one\\ntwo\\n\";\nprint <<EOT;\nheredoc\nEOT\n");
my $nlcl = run_pl2cl($nl);
my ($body) = ($nlcl =~ /\(p-run-compile-phase-blocks\)(.*)\z/s);
$body //= '';
# A STRING-STATE-AWARE scan, not a regex: a naive `"[^"\n]*\n[^"]*"` matches
# across two different literals on two lines and reports a false positive
# (measured while writing this file).  The claim is per LITERAL.
sub _multiline_literal {
  my ($t) = @_;
  my @c = split //, $t;
  my ($i, $in, $lit) = (0, 0, '');
  while ($i < @c) {
    my $ch = $c[$i];
    if ($in) {
      if ($ch eq "\\") { $i += 2; next }
      if ($ch eq '"')  { return $lit if $lit =~ /\n/; $in = 0; $lit = ''; $i++; next }
      $lit .= $ch;
    }
    elsif ($ch eq '"') { $in = 1; $lit = '' }
    $i++;
  }
  return undef;
}
is(_multiline_literal($body), undef,
   'no emitted string literal spans a line (the p-esc promise)');

# ── 4. --facts and :needs ────────────────────────────────────────────────
my $facts_src = <<'PERL';
my @a;
for my $i (1..5) { push @a, $i * 2 }
my @s = sort { $a <=> $b } (3,1,2);
my %h; $h{k} = 1;
my $n = 0; $n += 2;
sub ins { my $x = shift; return $x + 1 }
my @b = (7,8); my $t = 0;
for my $x (@b) { $t += $x }
print ins(1), "$t @s $h{k}\n";
print ${"main::n"}, "\n";
PERL
my $ff = write_pl('facts.pl', $facts_src);
my $fcl = run_pl2cl('--facts', $ff);
for my $name (qw(classic-sort elem-setf foreach-range foreach-raw
                 insensitive-call local-push numeric-slot symref-const
                 tail-return)) {
  like($fcl, qr/\(p-fact \(\Q$name\E\b/, "--facts names the $name licence");
}
unlike(run_pl2cl($ff), qr/p-fact/, 'without --facts no licence is printed');
# The counterfactual half: with the emission SWITCHED OFF the licences that do
# not depend on a suppressed verdict must still be named.  (`numeric-slot`
# does: its site is only reached because `raw-slot` fired, so it is absent
# under PCL_OPT=none — stated in ir-spec §12c.)
my $none = `cd "$root" && PCL_OPT=none ./pl2cl --facts "$ff" 2>/dev/null`;
for my $name (qw(classic-sort elem-setf foreach-range foreach-raw
                 insensitive-call local-push symref-const tail-return)) {
  like($none, qr/\(p-fact \(\Q$name\E\b/,
       "PCL_OPT=none --facts still names $name (the fact, not the shape)");
}
unlike($none, qr/%p-sort-classic|%p-push1|p-foreach-raw|p-tail-value/,
       'and PCL_OPT=none really did emit the general forms');

# :needs, one sub per class — and the EMPTY answer.
my $needs_src = <<'PERL';
sub pure   { my $x = shift; return $x + 1 }
sub throws { die "x" }
sub prints { print "hi\n" }
sub dyn    { local $/ = ":"; return 1 }
sub rx     { my $s = shift; return $s =~ /a/ ? 1 : 0 }
print pure(1), "\n";
PERL
my $nf  = write_pl('needs.pl', $needs_src);
my $ncl = run_pl2cl($nf);
like($ncl, qr/p-sub pl-pure.*?:needs \(\)/s,   ':needs () for a sub that needs nothing');
like($ncl, qr/:needs \([^)]*:nonlocal_exit\.die/,  ':needs names nonlocal_exit.die');
like($ncl, qr/:needs \([^)]*:io\b/,                ':needs names io');
like($ncl, qr/:needs \([^)]*:dynamic_scope\.local/, ':needs names dynamic_scope.local');
like($ncl, qr/:needs \([^)]*:regex\.(?:literal|native)/, ':needs names the regex classes');

# The manifest's tier histogram stops saying `unclassified` (ir-spec §10b).
my $mf = `cd "$root" && ./pl2cl --manifest "$rx" 2>/dev/null`;
like($mf, qr/"tier"\s*:\s*\{/, 'the manifest tier is a histogram, not a word');
like($mf, qr/"native"\s*:\s*[1-9]/, 'and it counts the native-tier literals');

done_testing();

#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ppi-bugs-registry-01.t -- THE CANARY of the PPI-bug registry Pl::PPIBugs
# (s525p; docs/parser-review-s525.md §7, docs/expect-state-checker-s525.md).
#
# For every row: while `fixed_in` is undef, PPI's dump of `repro` must still BE
# `got` -- the bug still reproduces.  A row that stops reproducing on a newer
# PPI FAILS with
#     PPI <version> fixed §<id>: set fixed_in
# (the STALE DETECTOR: the repair the row names can then go).  A row whose
# `fixed_in` is <= the installed PPI asserts `want` instead.
#
# Second, the expect-state checker (Pl::PPIExpect) must still PREDICT every
# row whose `when` names a check, and stay silent on the rows it cannot see.
# PPI only -- no transpile, no SBCL: a fast file.
use strict;
use warnings;
use utf8;
use Test::More;
use FindBin ();
use lib "$FindBin::RealBin/../..";
use Pl::VendorPPI;   # PPI, from vendor/PPI/lib (s527a)
use FindBin qw($RealBin);
use lib "$RealBin/../..";
use Pl::PPIBugs;
use Pl::PPIExpect qw(check_source);

binmode $_, ':utf8' for Test::More->builder->output, Test::More->builder->failure_output;

# ── THE DUMP FORMS (what a `got` / `want` string means) ─────────────────────
sub short { my $c = shift; $c =~ s/^PPI::(?:Token::)?//; $c }
sub esc   { my $s = shift; $s =~ s/\n/\\n/g; $s }
sub doc   { my ($src) = @_; PPI::Document->new(\$src) }
sub died  { ('LEXER-DIES: ' . (PPI::Document->errstr // '')) }
my %FORM = (
  # significant tokens, "Class=content"
  tokens => sub { my $d = doc($_[0]) or return died();
    map { short(ref $_) . '=' . esc($_->content) } grep { $_->significant } $d->tokens },
  # the same, each bracket with its structure and that structure's parent
  structure => sub { my $d = doc($_[0]) or return died();
    map { short(ref $_) . '=' . esc($_->content)
          . ($_->isa('PPI::Token::Structure') && $_->content =~ /^[\[\]{}()]$/
             ? ' in ' . short(ref $_->parent) . ' < ' . short(ref $_->parent->parent) : '') }
    grep { $_->significant } $d->tokens },
  # one string per top-level statement: its class and its significant tokens
  statements => sub { my $d = doc($_[0]) or return died();
    map { short(ref $_) . ': ' . join(' ', map { esc($_->content) } grep { $_->significant } $_->tokens) }
    $d->schildren },
  # does a document exist at all
  lexer => sub { my $d = doc($_[0]); $d ? ('DOCUMENT') : died() },
  # each here-doc and its body lines (indentation already removed by PPI)
  heredoc => sub { my $d = doc($_[0]) or return died();
    map { (short(ref $_) . '=' . $_->content, map { 'body=' . esc($_) } $_->heredoc) }
    @{ $d->find('PPI::Token::HereDoc') || [] } },
  # each Symbol and what its ->symbol method answers
  symbol => sub { my $d = doc($_[0]) or return died();
    map { short(ref $_) . '=' . $_->content . ' ->symbol=' . $_->symbol }
    @{ $d->find('PPI::Token::Symbol') || [] } },
  # the document's serialization when parsed with $/ undefined
  slurp => sub { my $src = $_[0]; my $d = do { local $/; PPI::Document->new(\$src) } or return died();
    ('serialize=' . esc($d->serialize)) },
);

sub vcmp { my ($a, $b) = @_; $a <=> $b }   # PPI versions are decimal numbers (1.291)

my @bugs = Pl::PPIBugs::bugs();
my $v = $PPI::VERSION;

# ── the schema ───────────────────────────────────────────────────────────────
{
  my @bad;
  my %seen;
  for my $b (@bugs) {
    my $id = $b->{id} // '?';
    push @bad, "§$id duplicate" if $seen{$id}++;
    push @bad, "§$id kind" unless ($b->{kind} // '') =~ /^(?:token|structure|source|lexer-dies)$/;
    push @bad, "§$id dump" unless $FORM{ $b->{dump} // '' };
    push @bad, "§$id fix" unless ($b->{fix}{kind} // '') =~ /^(?:relex|retag|handler)$/;
    for my $f (qw(title repro perl when tokens unblocks tasks ppi)) { push @bad, "§$id $f" unless defined $b->{$f} }
    push @bad, "§$id got == want" if !defined $b->{ppi}{fixed_in}
      && join("\0", @{ $b->{tokens}{got} }) eq join("\0", @{ $b->{tokens}{want} });
  }
  ok(!@bad, 'registry rows carry the review §7 schema (' . scalar(@bugs) . ' rows)') or diag "@bad";
}

# ── the canary: each bug still reproduces, or its fix is recorded ────────────
my $vendored = Pl::VendorPPI::is_vendored();
for my $b (@bugs) {
  my ($id, $fixed) = ($b->{id}, $b->{ppi}{fixed_in});
  my $dump = [ $FORM{ $b->{dump} }->($b->{repro}) ];
  # Fixed on PCL's PPI branch (Pl::PPIBugs header): `want` on the vendored
  # lib; on a system PPI the bug must still reproduce, and a dump that is
  # `want` there is upstream catching up.
  if (defined $fixed && $fixed eq 'pcl') {
    if ($vendored) {
      is_deeply($dump, $b->{tokens}{want}, "§$id stays fixed on PCL's PPI branch ($v, vendored)")
        or diag join("\n", 'dump:', @$dump);
      next;
    }
    my $same = join("\0", @$dump) eq join("\0", @{ $b->{tokens}{got} });
    my $want = join("\0", @$dump) eq join("\0", @{ $b->{tokens}{want} });
    ok($same, $same ? "§$id reproduces on system PPI $v (fixed on PCL's branch)"
                    : $want ? "upstream PPI $v fixed §$id: set fixed_in"
                            : "system PPI $v changed §$id (neither got nor want)")
      or diag join("\n", "the dump of the §${id} repro is now:", @$dump);
    next;
  }
  if (defined $fixed && vcmp($fixed, $v) <= 0) {
    is_deeply($dump, $b->{tokens}{want}, "§$id stays fixed on PPI $v (fixed in $fixed)")
      or diag join("\n", 'dump:', @$dump);
    next;
  }
  my $same = join("\0", @$dump) eq join("\0", @{ $b->{tokens}{got} });
  ok($same, $same ? "§$id reproduces on PPI $v" : "PPI $v fixed §$id: set fixed_in")
    or diag join("\n", "the dump of the §${id} repro is now:", @$dump,
                 (join("\0", @$dump) eq join("\0", @{ $b->{tokens}{want} }) ? '(== want: PPI fixed it)'
                                                                           : '(!= want either: PPI changed it)'));
}

# ── the checker predicts the token-walk-visible rows, and only those ─────────
for my $b (@bugs) {
  my $id = $b->{id};
  next if defined $b->{ppi}{fixed_in};
  my $check = $b->{when}{check};
  my @rows = grep { !$_->{cascade} } check_source($b->{repro}, file => "§$id");
  if (defined $check) {
    my $hit = grep { $_->{kind} eq $check && $_->{sec} =~ /^(?:\d+\/)?\Q$id\E(?:\/\d+)?$/ } @rows;
    ok($hit, "the checker flags §$id ($check)")
      or diag join("\n", map { "$_->{kind} $_->{class} '$_->{text}' \@$_->{state} §$_->{sec}" } @rows);
  }
  else {
    my $hit = grep { $_->{sec} eq $id } @rows;
    ok(!$hit, "§$id is not visible to the token walk (when.check undef) and the checker does not claim it");
  }
}

# A DEREFERENCE as the foreach loop variable is perl's own form (yyl_foreach:
# any scalar lvalue term before the `(`).  Once PCL's PPI lexes it (§6 §29,
# s527c), the checker must walk `for $$f`, `for ${*$f}`, `for ${$r}` and
# `for $$$rr` without a row -- before s527c it read the cast as a for-head
# contradiction and the `(` after `$$f` as a paren after a term.
if ($vendored) {
  my $src = 'our ($z, $v); my $f = "z"; my $r = \$z; my $rr = \$r;'
          . ' for $$f (5, 11) { 1 } for ${*$f} (5) { 1 } for ${$r} (1, 2) { 1 }'
          . ' for $$$rr (1) { 1 } for my $x (1) { $x } for $v (1) { 1 } print "x\n";';
  my @rows = check_source($src, file => 'deref-forvar');
  ok(!@rows, 'the checker walks a dereference foreach loop variable without a row (s527c)')
    or diag join("\n", map { "$_->{kind} '$_->{text}' \@$_->{state} col $_->{col}" } @rows);
}

done_testing();

package Pl::CLForm;

# CLForm — structured CL emission for the v2 pipeline (prototype).
#
# A *form* is one of:
#   - a plain string          → an atom already in CL syntax ("$x", "3", "\"s\"")
#   - a Raw object            → a pre-generated CL string (from the ORIGINAL
#                               Pl::ExprToCL) embedded opaquely; the escape
#                               hatch that lets v2 fall back per-expression
#   - [ $head, @args ]        → the list form ($head @args)
#   - [ 'list', @elems ]      → a headless parenthesized list (@elems) — used
#                               for let-bindings, lambda lists, (var list) pairs
#
# The printer is the ONLY place text is produced: parens always balance and
# indentation encodes depth (2 spaces/level) by construction. No downstream
# pass ever re-parses emitted text (the whole point of the v2 rewrite).

use v5.30;
use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(raw is_raw to_string to_program);

use constant ONE_LINE_MAX => 95;

sub raw {
  my ($s) = @_;
  return bless \$s, 'Pl::CLForm::Raw';
}

sub is_raw { ref($_[0]) eq 'Pl::CLForm::Raw' }

# One-line rendering, or undef if the form contains a multi-line raw chunk.
sub _flat {
  my ($f) = @_;
  return $f unless ref $f;
  if (is_raw($f)) { return $$f =~ /\n/ ? undef : $$f }
  my ($head, @args) = @$f;
  my @parts = $head eq 'list' ? () : ($head);
  for my $a (@args) {
    my $p = _flat($a);
    return undef unless defined $p;
    push @parts, $p;
  }
  return '(' . join(' ', @parts) . ')';
}

sub to_string {
  my ($f, $depth) = @_;
  $depth //= 0;
  return $f unless ref $f;
  # Raw chunks pass through verbatim: re-indenting would corrupt string
  # literals that contain newlines, and alignment is only cosmetic.
  return $$f if is_raw($f);
  my $flat = _flat($f);
  return $flat if defined $flat && length($flat) + 2 * $depth <= ONE_LINE_MAX;

  my ($head, @args) = @$f;
  my $ind1 = '  ' x ($depth + 1);
  if ($head eq 'list') {
    return '(' . join("\n$ind1", map { to_string($_, $depth + 1) } @args) . ')';
  }
  # Keep short scrutinee args (var/list pairs of let/foreach) on the head line
  # when the first arg fits flat; body args go one per line.
  my $first = @args ? _flat($args[0]) : undef;
  if (defined $first && length($head) + length($first) + 2 * $depth < ONE_LINE_MAX) {
    my @rest = @args[1 .. $#args];
    return "($head $first)" unless @rest;
    return "($head $first\n"
      . join("\n", map { $ind1 . to_string($_, $depth + 1) } @rest) . ')';
  }
  return "($head\n"
    . join("\n", map { $ind1 . to_string($_, $depth + 1) } @args) . ')';
}

sub to_program {
  my (@forms) = @_;
  return join("\n", map { ref($_) || $_ ne '' ? to_string($_, 0) : '' } @forms) . "\n";
}

1;

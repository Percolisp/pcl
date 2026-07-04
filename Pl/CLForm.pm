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
our @EXPORT_OK = qw(raw raw_wrap is_raw is_raw_wrap to_string to_program);

use constant ONE_LINE_MAX => 95;

sub raw {
  my ($s) = @_;
  return bless \$s, 'Pl::CLForm::Raw';
}

sub is_raw { ref($_[0]) eq 'Pl::CLForm::Raw' }

# A raw OPEN chunk from the original generator that deliberately leaves
# $closes forms unclosed (v1's `local` machinery emits `(let ((…))` /
# `(p-local-… …` opens and defers the closes to block end, counted in
# _local_let_depth), wrapping @body — the lowered remainder of the enclosing
# v2 block — in their dynamic extent.  The printer appends exactly $closes
# `)`s after the body, so balance still holds by construction: the open
# text's depth surplus comes from the SAME counter v1 itself uses to close
# these scopes.
sub raw_wrap {
  my ($open, $closes, @body) = @_;
  return bless { open => $open, closes => $closes, body => [@body] },
    'Pl::CLForm::RawWrap';
}

sub is_raw_wrap { ref($_[0]) eq 'Pl::CLForm::RawWrap' }

# One-line rendering, or undef if the form contains a multi-line raw chunk
# or a string atom with an embedded newline (CL has no \n escape — a literal
# newline inside a string atom makes the "one line" lie to the indenter).
# A chunk containing a `;` comment (outside a string literal) can never be
# joined onto one line either: the comment would swallow every sibling that
# follows it on that line (v1's fallback emits `;; source-echo` raws).
sub _no_flat {
  my ($s) = @_;
  return 1 if $s =~ /\n/;
  return 0 unless $s =~ /;/;
  my @c = split //, $s;
  my ($in_str, $i) = (0, 0);
  while ($i < @c) {
    my $ch = $c[$i];
    if ($in_str) { if ($ch eq "\\") { $i += 2; next } $in_str = 0 if $ch eq '"' }
    elsif ($ch eq '"') { $in_str = 1 }
    elsif ($ch eq ';') { return 1 }
    $i++;
  }
  return 0;
}

sub _flat {
  my ($f) = @_;
  if (!ref $f) { return _no_flat($f) ? undef : $f }
  if (is_raw($f)) { return _no_flat($$f) ? undef : $$f }
  return undef if is_raw_wrap($f);
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
  if (is_raw_wrap($f)) {
    my $ind1 = '  ' x ($depth + 1);
    return $f->{open}
      . join('', map { "\n$ind1" . to_string($_, $depth + 1) } @{ $f->{body} })
      . (')' x $f->{closes});
  }
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

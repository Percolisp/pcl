# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::CaseMods;

# THE case-modifier state machine of an interpolating construct (task #3510):
# perl's `\U \L \F \Q \E \u \l` are DOUBLE-QUOTED-STRING operators (toke.c),
# applied where the literal is interpolated -- in a dq string AND in a regex
# literal -- never by the regex compiler.  Both consumers drive this one
# machine (ir-spec §3.2g); each supplies only how its PARTS are built:
#
#   group => sub { my ($mode, \@parts) = @_; ... }   # wrap a closed U/L/F/Q group
#   char  => sub { my ($c, $part) = @_; ... }        # wrap one part in ucfirst/lcfirst ('u'/'l')
#
# The consumer calls ->add($part) for every literal or interpolated part in
# source order, ->escape($letter) for every case escape, and ->finish for the
# list of top-level parts (an unclosed group closes at the end, as perl's
# implicit \E does).  The rules, each probed against perl 5.40.3 when the dq
# decoder was written (StringInterpolation.pm, #2441):
#   * \E closes the innermost group and cancels a pending \u / \l.
#   * \u / \l at the very START of an open group modifies the group's OUTPUT
#     ("\L\u$x" is ucfirst(lc $x)); elsewhere it applies to the next part.
#   * \U \L \F first close groups from the top while any non-Q group is open
#     ("\LAB\LCD\EEF" is abcdEF); \Q nests.
#   * a \u / \l pending when \U \L \F \Q opens applies to that group's output.

use strict;
use warnings;

sub new {
  my ($class, %cb) = @_;
  die "Pl::CaseMods: group and char callbacks are required\n"
    if !$cb{group} || !$cb{char};
  my $top = [];
  return bless { cb => \%cb, stack => [], top => $top, cur => $top,
                 pending => undef }, $class;
}

# Is LETTER one of the case escapes this machine consumes?
sub is_escape { return $_[0] =~ /^[ULulQFE]\z/ ? 1 : 0 }

sub add {
  my ($self, $part) = @_;
  if ($self->{pending}) {
    $part = $self->{cb}{char}->($self->{pending}, $part);
    $self->{pending} = undef;
  }
  push @{ $self->{cur} }, $part;
  return;
}

sub _close_group {
  my ($self, $keep_pending) = @_;
  my $group = pop @{ $self->{stack} };
  $self->{cur} = @{ $self->{stack} } ? $self->{stack}[-1]{parts} : $self->{top};
  my $wrapped = $self->{cb}{group}->($group->{mode}, $group->{parts});
  my $oc = $group->{outer_char};
  if (!$oc && !$keep_pending && $self->{pending}) {
    $oc = $self->{pending};
    $self->{pending} = undef;
  }
  $wrapped = $self->{cb}{char}->($oc, $wrapped) if $oc;
  push @{ $self->{cur} }, $wrapped;
  return;
}

sub escape {
  my ($self, $cmd) = @_;
  my $stack = $self->{stack};
  if ($cmd eq 'E') {
    $self->_close_group if @$stack;
    $self->{pending} = undef;
  } elsif ($cmd eq 'u' || $cmd eq 'l') {
    if (@$stack && !@{ $stack->[-1]{parts} } && !$stack->[-1]{outer_char}) {
      $stack->[-1]{outer_char} = $cmd;
    } else {
      $self->{pending} = $cmd;
    }
  } elsif ($cmd =~ /^[ULFQ]\z/) {
    if ($cmd ne 'Q') {
      $self->_close_group(1) while grep { $_->{mode} ne 'Q' } @$stack;
    }
    my $parts = [];
    push @$stack, { mode => $cmd, parts => $parts,
                    outer_char => $self->{pending} };
    $self->{pending} = undef;
    $self->{cur} = $parts;
  } else {
    die "Pl::CaseMods: not a case escape: \\$cmd\n";   # rule 12
  }
  return;
}

sub finish {
  my ($self) = @_;
  $self->_close_group(1) while @{ $self->{stack} };
  return @{ $self->{top} };
}

1;

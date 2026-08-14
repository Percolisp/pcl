package Pl::OpcodeTree;

# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

use v5.20;
use strict;
use warnings;

use Moo;


# This isn't meant to be efficient if adding/deleting a lot of stuff,
# there is no garbage collection.


use PPI;
use PPI::Dumper;

use Data::Dump qw/dump/;


has nodes => (
  is        => 'rw',
  default => sub {return []; },
);


has node_top => (
  is        => 'rw',
  default => sub { 0 },
);


sub node_data {
  my $self      = shift;
  my $node_id   = shift;

  my $node_list = $self->nodes;
  my $node      = $node_list->[$node_id];
# say "Get node: $node_id"; say dump $node->{n}; say "+" x 50;
# use btmp; main::where_5("Huh?");
  return $node->{n};
}


sub children_ids {
  my $self      = shift;
  my $node_id   = shift;

  my $node      = $self->nodes()->[$node_id];
  return $node->{c} // [];
}


# Gets new node value. Returns ID of new node.
sub add_node {
  my $self      = shift;
  my $node      = shift;

  my $node_list = $self->nodes();
  my $no        = scalar @$node_list;
  push @$node_list, {n => $node, c => [], meta => {}};
  return $no;
}

sub add_child_id {
  my $self      = shift;
  my $node_id   = shift;
  my $child_id  = shift;

  return
      if $node_id == -1 || $child_id == -1;

  # use btmp; main::where_3("$node_id, c: $child_id");
  my $kids      = $self->nodes()->[$node_id]->{c};
  push @$kids, $child_id;
  # say "XXXXXXXX  Added child $child_id to node $node_id";
}


sub unshift_child_id {
  my $self      = shift;
  my $node_id   = shift;
  my $child_id  = shift;

  return
      if $node_id == -1 || $child_id == -1;

  # use btmp; main::where_3("$node_id, c: $child_id");
  my $kids      = $self->nodes()->[$node_id]->{c};
  unshift @$kids, $child_id;
  # say "XXXXXXXX  Added child $child_id to node $node_id";
}



# (add_extra/extras and the {xa} slot deleted #303/s392: a WRITE-ONLY field.
# The one writer was PExpr::_add_tag_to_node, called from string
# interpolation to stash the original quote token's text on the concat node;
# nothing ever read it back — not extras(), not a raw {xa} — in the whole
# repo.  Rule: settle the PAIR, never keep a write-only field for a
# hypothetical consumer; git history is the archive.)

# - - - Metadata storage (for context, etc.)

sub set_metadata {
  my $self      = shift;
  my $node_id   = shift;
  my $key       = shift;
  my $value     = shift;

  my $node      = $self->nodes()->[$node_id];
  $node->{meta}{$key} = $value;
}

sub get_metadata {
  my $self      = shift;
  my $node_id   = shift;
  my $key       = shift;

  my $node      = $self->nodes()->[$node_id];
  return $node->{meta}{$key};
}


1;

# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-25-moo-class.pl
use strict; use warnings;
package Counter; use Moo;
has name => (is => "ro", required => 1); has count => (is => "rw", default => sub { 0 }); has log => (is => "lazy"); has limit => (is => "ro", isa => sub { die "limit must be positive\n" if $_[0] <= 0 }, default => 10);
sub _build_log { [] } sub inc { my $s = shift; die "limit\n" if $s->count >= $s->limit; $s->count($s->count + 1); push @{ $s->log }, "inc"; $s }
around inc => sub { my ($orig, $s, @a) = @_; push @{ $s->log }, "before"; $s->$orig(@a) };
package Named; use Moo::Role; requires "name"; sub label { "[" . $_[0]->name . "]" }
package Sub; use Moo; extends "Counter"; with "Named"; has "+limit" => (default => 2); sub BUILD { my $s = shift; push @{ $s->log }, "built" }
package main;
my $c = Sub->new(name => "c1"); $c->inc->inc; my $over = eval { $c->inc; 1 } ? "no-limit" : "limit:$@"; chomp $over;
print $c->label, " ", $c->count, " @{$c->log} $over ", (eval { Counter->new; 1 } ? "lived" : ($@ =~ /Missing required arguments: name/ ? "required-ok" : $@)), " ", (eval { Counter->new(name => "x", limit => -1); 1 } ? "lived" : "isa-ok"), " ", ($c->does("Named") ? "does" : "doesnt"), " ", ($c->isa("Counter") ? "isa" : "nota"), " ", (eval { $c->name("new"); 1 } ? "rw?" : "ro-ok"), "\n";

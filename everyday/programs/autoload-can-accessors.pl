# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-23-autoload-can-accessors.pl
use strict; use warnings;
package Rec; our $AUTOLOAD; my %fields = map { $_ => 1 } qw(name age);
sub new { my ($c, %a) = @_; bless { %a }, $c }
sub AUTOLOAD { my $s = shift; (my $n = $AUTOLOAD) =~ s/.*:://; return if $n eq "DESTROY"; die "no method $n" if !$fields{$n}; no strict "refs"; *{$AUTOLOAD} = sub { my $o = shift; $o->{$n} = shift if @_; $o->{$n} }; unshift @_, $s; goto &$AUTOLOAD }
sub can { my ($s, $m) = @_; $s->SUPER::can($m) || ($fields{$m} ? sub { my $o = shift; $o->$m(@_) } : undef) }
package main;
my $r = Rec->new(name => "ann", age => 30); $r->age(31); my $m = "name";
print $r->name, " ", $r->age, " ", $r->$m, " ", (Rec->can("age") ? "can" : "cannot"), " ", (Rec->can("nosuch") ? "can" : "cannot"), " ", (defined &Rec::age ? "installed" : "not"), " ", (eval { $r->bogus; 1 } ? "lived" : ($@ =~ /^no method bogus/ ? "died-ok" : $@)), " ", ref($r), " ", $r->isa("Rec") ? "isa" : "nota", " ", Rec->can("new") == \&Rec::new ? "same-cv" : "diff-cv", "\n";

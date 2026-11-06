#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# modifier-my-01.t -- a `my` declared in the MODIFIER CONDITION of a
# `return` / `next` / `last` statement is a lexical of the enclosing block
# (task #4304, s532c; the reduction of #4221).
#
#     sub replace {
#       my ($self, $new) = @_;
#       return $self->parse($new) if (my $tree = $self->tree)->[0] eq 'root';
#       return $self->_replace(_parent($tree), $tree, _nodes($self->_parse($new)));
#     }
#
# is Mojo::DOM58's own spelling (`parent`, `replace`, `_replace` ...).  PCL's
# embedded-`my` arm took only a PLAIN statement, so in a Statement::Break the
# `my $tree` got no `let` and every such sub wrote ONE package global $tree:
# the nested `_parse` call overwrote replace's node, `->remove` spliced
# nothing (Mojo-DOM58 t/dom.t 'Tap into method chain', 'Nodes', then "Not an
# ARRAY reference").  Perl ($^X) is the oracle; the row FAILS on ce665621.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt      = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

my $workdir = tempdir(CLEANUP => 1);

sub both {
    my ($tag, $body) = @_;
    my $src = "$workdir/$tag.pl";
    open(my $s, '>', $src) or die; print {$s} $body; close $s;
    my $perl = `cd $workdir && $^X $src 2>&1`;
    my $cl_code = `cd $workdir && $pl2cl $src 2>/dev/null`;
    my $cl_file = "$workdir/$tag.lisp";
    open(my $c, '>', $cl_file) or die; print {$c} $cl_code; close $c;
    my $pcl = `cd $workdir && sbcl @sbcl_rt --load $cl_file 2>&1`;
    $pcl =~ s/^;.*\n//gm;
    $pcl =~ s/^PCL Runtime loaded\n//gm;
    $pcl =~ s/^\s*\n//gm;
    return ($perl, $pcl);
}

my ($perl, $pcl) = both('modmy', <<'PL');
use strict;
package O;
sub new { my ($c, $t) = @_; bless { t => $t }, $c }
sub tree { $_[0]{t} }
sub other {
  my ($self) = @_;
  return "root" if (my $tree = ['fresh'])->[0] eq 'root';
  return $tree->[0];
}
sub replace {
  my ($self, $new) = @_;
  return "parsed" if (my $tree = $self->tree)->[0] eq 'root';
  my $before = $tree->[0];
  my $o = $self->other;
  return "before=$before after=$tree->[0] other=$o";
}
package main;
print "1 ", O->new(['tag'])->replace('x'), "\n";
print "2 ", O->new(['root'])->replace('x'), "\n";
sub inner { for my $i (1 .. 3) { next unless (my $sq = $i * $i) > 3; return "first-sq=$sq" } "none" }
sub outer { for my $i (1 .. 2) { next if (my $sq = $i * 10) > 100; my $in = inner(); return "outer-sq=$sq $in" } }
print "3 ", outer(), "\n";
my @seen;
for my $w (qw(a bb ccc)) { last if (my $len = length $w) > 2; push @seen, "$w:$len" }
print "4 @seen\n";
PL
is($pcl, $perl, 'a my in a return / next / last modifier condition is a lexical of the enclosing block');

done_testing();

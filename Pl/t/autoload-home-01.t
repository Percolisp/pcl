#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# autoload-home-01.t -- the $AUTOLOAD perl sets is the one of the package the
# AUTOLOAD sub was DEFINED under, not the package of the glob it was
# INSTALLED in (task #1486; s532c, from Template-Toolkit t/proc.t).
#
# gv.c sets $AUTOLOAD in `varstash = GvSTASH(CvGV(cv))`: a named sub's own
# package, an anonymous sub's compile package.  Template::Plugin::Procedural
# compiles `sub { $AUTOLOAD =~ s!^.*::!!; ... }` (its `our $AUTOLOAD`) and
# assigns it to *{"${proxy}::AUTOLOAD"}; PCL set $PROXY::AUTOLOAD, the
# closure read Procedural's empty one and every procedural plugin method
# answered "".  Row 2 is #1486's own five-spelling reproducer.  Perl ($^X) is
# the oracle at test time; both rows FAIL on ce665621.

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

my ($perl, $pcl) = both('proxy', <<'PL');
package Maker;
our $AUTOLOAD;
sub install {
  my ($class, $proxy) = @_;
  no strict 'refs';
  *{ $proxy . "::AUTOLOAD" } = sub {
    my $name = $AUTOLOAD;
    $name =~ s!^.*::!!;
    return "maker-saw:" . ($AUTOLOAD // 'undef') . " name=$name";
  };
  *{ $proxy . "::new" } = sub { bless {}, $_[0] };
}
package main;
Maker->install("Prox::Y");
my $o = Prox::Y->new;
print "1 ", $o->foo, "\n";
print "2 Prox::Y::AUTOLOAD=", ($Prox::Y::AUTOLOAD // 'undef'), "\n";
package Own;
our $AUTOLOAD;
sub AUTOLOAD { return "own:$AUTOLOAD" }
package Kid; our @ISA = ('Own');
package main;
print "3 ", Own->bar, " ", Kid->baz, "\n";
sub main::fallback;
sub AUTOLOAD { our $AUTOLOAD; return "main-al:$AUTOLOAD" }
print "4 ", fallback(), "\n";
PL
is($pcl, $perl, 'an installed anonymous AUTOLOAD reads the $AUTOLOAD of its compile package');

($perl, $pcl) = both('i1486', <<'PL');
{ package B1; sub m1 {1}
  *B1::AUTOLOAD = sub { our $AUTOLOAD; return $AUTOLOAD };
  package main; print "in-pkg  =[", (eval { B1->later } // "undef"), "]\n" }
{ package B2; sub m1 {1}
  package main;
  *B2::AUTOLOAD = sub { our $AUTOLOAD; return $AUTOLOAD };
  print "in-main =[", (eval { B2->later } // "undef"), "]\n" }
{ package B3; sub m1 {1}
  package main; *B3::AUTOLOAD = \&B1::AUTOLOAD;
  print "alias   =[", (eval { B3->later } // "undef"), "]\n" }
{ package B4; sub m1 {1} sub AUTOLOAD { our $AUTOLOAD; return $AUTOLOAD }
  package main; print "declared=[", (eval { B4->later } // "undef"), "]\n" }
{ package B5; sub m1 {1}
  package main; *B5::plainmeth = sub { "PM" };
  print "plainglob=[", (eval { B5->plainmeth } // "undef"), "]\n" }
PL
is($pcl, $perl, '#1486: an AUTOLOAD installed from another package, or aliased, sets the defining package\'s $AUTOLOAD');

done_testing();

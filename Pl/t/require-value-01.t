#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# require-value-01.t -- a required file's VALUE (task #1688).
#
# perl's require returns the value of the file's last run-time statement the
# first time (1 afterwards) and dies "NAME did not return a true value" when
# it is false -- an empty file, a file of subs only, a trailing `0;` -- unless
# the `module_true` feature (5.37+, `use v5.38`) is in force at the end of the
# file, which makes it 1.  A top-level `return EXPR` ends the file with EXPR
# as its value (it used to return from the REQUIRE's caller).  Template
# Toolkit loads compiled templates with `$doc = require $file`.  Perl ($^X)
# is the oracle at test time; every row FAILS on ade297e7 (require answered 1
# for every file and never died).

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
mkdir "$workdir/mt" or die;
my %mods = (
  MtA => "package MtA; sub foo {} 7;\n",
  MtB => "package MtB; sub foo {} 0;\n",
  MtC => "package MtC; use feature 'module_true'; sub foo {}\n",
  MtD => "",
  MtE => "package MtE; 1; sub foo {}\n",
  MtF => "package MtF; use v5.38; 0;\n",
  MtG => "package MtG; our \$x = 5;\n",
  MtI => "package MtI; return 'ret'; 0;\n",
  MtL => "package MtL; use feature 'module_true'; 0;\n",
  MtM => "package MtM;\n__PACKAGE__;\n__END__\n0;\n",
  MtN => "package MtN; +{ BLOCK => 'b', name => 'doc' };\n",
);
for my $m (sort keys %mods) {
  open(my $f, '>', "$workdir/mt/$m.pm") or die; print {$f} $mods{$m}; close $f;
}

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

my ($perl, $pcl) = both('values', <<'PL');
use lib 'mt';
for my $m (qw(MtA MtB MtC MtD MtE MtF MtG MtL MtM)) {
  my $rv = eval "require $m";
  my $e = $@ =~ /did not return a true value/ ? "false-value" : $@ ? "other" : "";
  print "$m rv=", (defined $rv ? "'$rv'" : 'undef'), " err=$e inc=",
    (exists $INC{"$m.pm"} ? "set" : "absent"), "\n";
}
my @l = eval { require MtA }; print "again list=(@l)\n";
my $s = require MtG; print "again scalar=$s\n";
my $doc = require MtN; print "hashref name=$doc->{name}\n";
sub f { my $v = require MtI; return "f-saw-$v" }
print f(), " inc=", (exists $INC{"MtI.pm"} ? "set" : "absent"), "\n";
my $fv = do "./mt/MtA.pm"; print "do=$fv\n";
PL
is($pcl, $perl, 'require answers the file value; a false one dies; module_true; a top-level return ends the file');

($perl, $pcl) = both('strreq', <<'PL');
my $rv = require "./mt/MtA.pm"; print "path rv=$rv\n";
eval { require "./mt/MtB.pm" }; print "path false: ", ($@ =~ /MtB\.pm did not return a true value/ ? "dies" : "no: $@"), "\n";
PL
is($pcl, $perl, 'require "path": the same value and the same die');

done_testing;

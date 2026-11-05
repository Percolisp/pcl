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
  # #4162 #4166: a file that DIES while it loads
  DiesMod => "package DiesMod; \$main::dm_runs++; die \"dm\\n\"; 1;\n",
  DiesObj => "package DiesObj; die bless {}, 'XErr'; 1;\n",
  Second  => "package Second; sub v { 7 } 1;\n",
  # #4168: the module_true state AT a top-level return; a tail statement's value
  TlA => "package TlA; 5 unless \"x\";\n",
  TlB => "package TlB; { 7 }\n",
  TlC => "package TlC; my \$v = 7; unless (\$v) { 0 }\n",
  TlD => "package TlD; my \$v = 2; if (\$v == 1) { 0 } elsif (\$v == 2) { 6 } else { 0 }\n",
  TlE => "package TlE; if (0) { 1 }\n",
  TlF => "package TlF; use feature 'module_true'; { no feature 'module_true'; return 0; }\n",
  TlG => "package TlG; use feature 'module_true'; return 0; no feature 'module_true';\n",
  TlH => "package TlH; my \$x = 4; { my \$y = \$x + 1; \$y }\n",
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

# #4162: a file that DIES leaves $INC{NAME} = undef (exists, not defined), the
# die says "Compilation failed in require", and a second require dies
# "Attempt to reload" -- the file never runs twice (it used to: every side
# effect of the module happened again).  The hook path and an explicit path
# the same; a FALSE file leaves the entry absent (#1688, the control).  The
# location text after "require" is not compared (line-track is off).
($perl, $pcl) = both('diesload', <<'PL');
use lib 'mt';
sub tag { my $e = shift; $e =~ /^Attempt to reload \S+ aborted\.\nCompilation failed in require/ ? "reload"
          : $e =~ /^(.*?)Compilation failed in require/s ? "failed($1)" : "other($e)" }
eval { require DiesMod }; print "1 ", tag($@), " e=", (exists $INC{'DiesMod.pm'} ? 1 : 0), (defined $INC{'DiesMod.pm'} ? 1 : 0), "\n";
eval { require DiesMod }; print "2 ", tag($@), "\n";
eval { require "DiesMod.pm" }; print "3 ", tag($@), " runs=$main::dm_runs\n";
eval "use DiesMod; 1" or print "4 ", tag($@), "\n";
eval { require DiesObj }; print "5 ", ($@ =~ /^XErr=HASH\(0x[0-9a-f]+\)Compilation failed in require/ ? "stringified" : "no($@)"), "\n";
unshift @INC, sub { return unless $_[1] eq 'HookDies.pm'; open my $fh, '<', \"package HookDies; \$main::hr++; die qq{hd\\n}; 1;"; $fh };
eval { require HookDies }; print "6 ", tag($@), " e=", (exists $INC{'HookDies.pm'} ? 1 : 0), (defined $INC{'HookDies.pm'} ? 1 : 0), "\n";
eval { require HookDies }; print "7 ", tag($@), " hr=$main::hr\n";
eval { require MtB }; print "8 false e=", (exists $INC{'MtB.pm'} ? 1 : 0), "\n";
$INC{'Second.pm'} = undef; eval { require Second }; print "9 ", tag($@), "\n";
PL
is($pcl, $perl, 'a file that dies: $INC{NAME} = undef, "Compilation failed in require", "Attempt to reload" (#4162)');

# #4166: a require that FINDS its file clears $! (pp_ctl.c S_require_file),
# so a die while the file loads exits 255 -- not the ENOENT `use lib` (or a
# search miss) left behind.
sub pcl_rc {
    my ($tag, $body) = @_;
    my $src = "$workdir/$tag.pl";
    open(my $s, '>', $src) or die; print {$s} $body; close $s;
    system("cd $workdir && $^X $src >/dev/null 2>&1");
    my $perl_rc = $? >> 8;
    my $cl_code = `cd $workdir && $pl2cl $src 2>/dev/null`;
    my $cl_file = "$workdir/$tag.lisp";
    open(my $c, '>', $cl_file) or die; print {$c} $cl_code; close $c;
    system("cd $workdir && sbcl @sbcl_rt --load $cl_file >/dev/null 2>&1");
    return ($perl_rc, $? >> 8);
}
my ($prc, $crc) = pcl_rc('diesrc', "use lib 'mt'; -d '/nonexistent/x'; require DiesMod;\n");
is("rc=$crc", "rc=$prc", 'a die while a required file loads exits 255, not a stale ENOENT (#4166)');
($perl, $pcl) = both('errnoreq', "use lib 'mt'; -d '/nonexistent/x'; require Second; print 'errno=', 0+\$!, qq{\\n};\n");
is($pcl, $perl, 'a require that found its file leaves $! = 0 (#4166)');

# #4168: perl's t/comp/require.t Demo matrix (module_true AT the return) and
# the value of a TAIL statement -- modifier, bare block, if/unless.
my $demo = <<'CODE';
package %s;
use feature 'module_true';

return 1 if $main::test_mode == 1;
return 0 if $main::test_mode == 2;

{
  no feature 'module_true';
  return 0 if $main::test_mode == 3;
}
no feature 'module_true';
CODE
for my $n (1 .. 4) {
  open(my $f, '>', "$workdir/mt/Demo$n.pm") or die; printf {$f} $demo, "Demo$n"; close $f;
}
($perl, $pcl) = both('tailval', <<'PL');
use lib 'mt';
for $main::test_mode (1 .. 4) {
  my $m = "Demo$main::test_mode"; my $r = 0;
  my $ok = eval "\$r = require '$m.pm'; 1";
  print "$m ok=", ($ok ? 1 : 0), " r=$r", ($ok ? "" : $@ =~ /did not return a true value/ ? " false-value" : " other"), "\n";
}
for my $m (qw(TlA TlB TlC TlD TlE TlF TlG TlH)) {
  my $rv = eval "require $m";
  print "$m rv=", (defined $rv ? "'$rv'" : 'undef'), ($@ =~ /did not return a true value/ ? " false-value" : $@ ? " other" : ""), "\n";
}
PL
is($pcl, $perl, 'module_true is read AT a top-level return; a tail modifier / block / if carries its value (#4168)');

done_testing;

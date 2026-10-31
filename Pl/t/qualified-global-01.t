#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# qualified-global-01.t - a package-QUALIFIED variable is always the global
# (task #3080).  In CL package Foo the symbols `$x` and `Foo::$x` are ONE
# symbol, and a lexical `my $x` is a `let` that shadows the global's symbol
# macro -- so `my $x; ... $Foo::x` read the lexical.  A `my`/`state` of a name
# the file also spells qualified is renamed (`$x__excl__N`, the exception-global
# pass, reason :qualified-global).  Every expected text is perl's own output.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 8;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($pl_file) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl $pl_file");
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub perl_oracle {
    my ($name, $code) = @_;
    my $pl_file = write_pl($code);
    my $want = `$^X $pl_file 2>&1`;
    is(run_cl($pl_file), $want, "$name (perl: " . ($want =~ s/\n/\\n/gr) . ")");
}

perl_oracle("#3080 the filed reproducers: a sub-local my shadows nothing qualified", <<'PL');
package Foo;
$Foo::flags = 6;
sub get { my ($flags) = @_; return defined $flags ? $flags : $Foo::flags }
print get(), " ", get(3), "\n";
sub get2 { my $flags; return $Foo::flags }
print get2(), "\n";
our $x = 'pkg'; sub get3 { my $x = 'lex'; return "$x $Foo::x" } print get3(), "\n";
PL

perl_oracle("#3080 every sigil: \@Foo::a, \$#Foo::a, \$Foo::a[1], \$Foo::h{k}, keys %Foo::h, push", <<'PL');
package Foo;
our @a = (1,2); our %h = (k => 'v');
sub s4 { my @a = (9); my %h = (k => 'L'); return "@a @Foo::a $#Foo::a $Foo::a[1] $Foo::h{k} " . join(",", keys %Foo::h) . " " . scalar(@Foo::a) }
print s4(), "\n";
sub s7 { my @a; push @Foo::a, 3; return scalar(@Foo::a) . " " . scalar(@a) } print s7(), "\n";
PL

perl_oracle("#3080 local, a write, a ref and a block under a shadowing my", <<'PL');
package Foo;
our $x = 'pkg';
sub g5 { $Foo::x }
sub s5 { my $x = 'lex'; local $Foo::x = 'loc'; return g5() . " $x" } print s5(), " $Foo::x\n";
sub s6 { my $x = 'lex'; $Foo::x = 'w'; my $r = \$Foo::x; return "$$r $x" } print s6(), " ", g5(), "\n";
{ my $x = 'blk'; print "$x $Foo::x $main::x\n"; }
print ${"Foo::x"}, "\n";
PL

perl_oracle("#3080 main:: and :: from main; \$main::x from Foo stays right", <<'PL');
our $x = 'mainx';
$Foo::x = 'foox';
sub m1 { my $x = 'ml'; return "$x $main::x $Foo::x $::x" } print m1(), "\n";
{ package Foo; sub m2 { my $x = 'L'; "$x $main::x" } } print Foo::m2(), "\n";
sub m3 { my $y = 'only'; return $y } print m3(), "\n";
PL

my $dir = tempdir(CLEANUP => 1);
open my $pm, '>', "$dir/QgMod.pm" or die "QgMod.pm: $!";
print $pm <<'PM';
package QgMod;
our $flags = 6;
sub get { my ($flags) = @_; return defined $flags ? $flags : $QgMod::flags }
1;
PM
close $pm;
perl_oracle("#3080 the same shape in a use'd module", <<"PL");
use lib '$dir';
use QgMod;
print QgMod::get(), " ", QgMod::get(3), "\\n";
PL

perl_oracle("#3080 an `our` of the name in the lexical's scope keeps the #470 demotion (the rename stands aside)", <<'PL');
my $y = 7;
sub nm { $y }
$main::y = 3;
{ our $y; $y .= "!"; }
print "[", (defined $main::y ? $main::y : "undef"), "]\n";
print "[", nm(), "]\n";
print "[", (defined $::y ? $::y : "undef"), "]\n";
PL

# #4080 (s530b): an ELEMENT or DEREFERENCE store through a package-QUALIFIED
# scalar -- `$Q::r->{k} = ...`, `$$Q::r = ...`, and every store through a
# file-level `our $x` in a package (emitted `Q::$x`) -- took the plain-scalar
# assignment `p-scalar-=`, whose `(boundp '<place>)` died "is not a symbol" at
# run time: the three sigil tests of ExprToCL's `=` dispatch matched a `::$`
# ANYWHERE in the left side's text.  Template::Parser.pm:92 (the TAG_STYLE
# chain) is the shape that stopped Template-Toolkit loading.
perl_oracle("#4080 element / deref stores through a qualified or our-aliased scalar", <<'PL');
package Q;
our $t;
our $T = { default => [1, 2] };
$t->{a} = 1;
print "r1 $t->{a}\n";
$T->{template} = $T->{tt2} = $T->{default};
print "r2 $T->{template}[1] $T->{tt2}[0]\n";
$Q::r = \$Q::z;
$$Q::r = 3;
print "r3 $Q::z\n";
${$Q::r} = 4;
print "r4 $Q::z\n";
$Q::a->[0] = 5;
${$Q::a}[1] = 6;
print "r5 @$Q::a\n";
our $u;
$u->[2] = 7;
$u->[0]{k} = 8;
print "r6 $u->[2] $u->[0]{k}\n";
$Q::s = 9;
@Q::arr = (10, 11);
%Q::h = (k => 12);
$Q::h{j} = 13;
$Q::arr[2] = 14;
print "r7 $Q::s @Q::arr $Q::h{k} $Q::h{j}\n";
@{$Q::a}[2, 3] = (15, 16);
@$t{qw(b c)} = (17, 18);
print "r8 @$Q::a[2,3] $t->{b}$t->{c}\n";
PL

{
  my $f  = write_pl("package Q;\nour \$t;\n\$t->{a} = 1;\n\$\$Q::r = 2;\n\$Q::a->[0] = 3;\n\$Q::s = 4;\n");
  my $cl = PCLCore::transpile("$pl2cl $f");
  my @bad = $cl =~ /(\(p-scalar-= \(p-\S+)/g;
  ok(!@bad && $cl =~ /\(p-scalar-= Q::\$s 4\)/,
     "#4080 shape: no p-scalar-= on a non-variable place; a plain qualified scalar keeps it"
     . (@bad ? " (found: @bad)" : ""));
}

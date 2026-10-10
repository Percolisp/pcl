#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Tied FILEHANDLES (task #2590, s514d): `tie *FH` / `tie *$fh` / `tie *STDOUT`
# dispatch every I/O builtin to the tie object (docs/tie-aggregates.md "Tied
# handles", ir-spec).  Every row compares against perl (5.38-safe: core only).
# Each program logs the method calls into @L and prints them to an UNTIED
# handle at the end, so stdout/stderr interleaving never decides a row.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

# Run a Perl file and return output
sub run_perl {
    my ($code) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return `perl $file 2>&1`;
}

# Transpile and run CL, return output
sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    my $cl_code = PCLCore::transpile(qq{$pl2cl $pl_file});

    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^WARNING:.*\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_th {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out   = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL:   $cl_out");
}

# The tie class every row uses: each method logs its call and arguments.
my $CLASS = <<'PERL';
package T;
our @L;
my $data = "abcdef";
sub TIEHANDLE { my ($c, @a) = @_; push @L, "TIEHANDLE(@a)"; bless {}, $c }
sub PRINT    { my $s = shift; push @L, "PRINT(" . join("|", @_) . ")"
                 . " ors=" . (defined $\ ? ($\ eq "\n" ? "NL" : $\) : "U"); 7 }
sub PRINTF   { my $s = shift; push @L, "PRINTF(" . join("|", @_) . ")"; 8 }
sub READLINE { push @L, "READLINE(" . (wantarray ? "list" : "scalar") . ")";
               wantarray ? ("l1\n", "l2\n") : "one\n" }
sub READ     { my $s = shift; push @L, "READ(" . scalar(@_) . ")";
               substr($_[0], $_[2] || 0) = substr($data, 0, $_[1]); $_[1] }
sub WRITE    { my ($s, $b, $l, $o) = @_; push @L, "WRITE($b,$l," . (defined $o ? $o : "U") . ")"; $l }
sub GETC     { push @L, "GETC"; "g" }
sub EOF      { push @L, "EOF($_[1])"; "" }
sub CLOSE    { push @L, "CLOSE"; "closed" }
sub BINMODE  { push @L, "BINMODE(" . (@_ - 1) . ")"; 1 }
sub FILENO   { push @L, "FILENO"; 42 }
sub SEEK     { push @L, "SEEK($_[1],$_[2])"; 1 }
sub TELL     { push @L, "TELL"; 99 }
sub OPEN     { my $s = shift; push @L, "OPEN(" . join("|", @_) . ")"; 1 }
sub UNTIE    { push @L, "UNTIE" }
package main;
PERL

test_th('print / printf / say: PRINT gets the LIST, $, is not applied, say localizes $\\ to a newline', $CLASS . <<'PERL');
use feature 'say';
my $o = tie *FH, 'T', 'a', 'b';
$, = "-";
my $r = print FH "p", "q";
$, = undef;
{ local $\ = "Z"; print FH "x"; say FH "s1", "s2"; push @T::L, "after say ors=" . $\; }
printf FH "%s=%d", "k", 3;
untie *FH;
print "ret $r obj ", ref($o), "\n", join("\n", @T::L), "\n";
PERL

test_th('readline: scalar, list, while(<FH>) sets $_, chomp(<FH>)', $CLASS . <<'PERL');
tie *FH, 'T';
my $l = <FH>; my @l = <FH>;
while (<FH>) { chomp(my $g = $_); push @T::L, "got $g"; last }
chomp(my $c = <FH>);
my $rl = readline(FH);
untie *FH;
print "[$l] [@l] [$c] [$rl]\n", join("\n", @T::L), "\n";
PERL

test_th('read / sysread write INTO the buffer (an alias), with and without OFFSET; syswrite is WRITE', $CLASS . <<'PERL');
tie *FH, 'T';
my $buf = "zz"; my $r1 = read(FH, $buf, 3);
my $b2 = "xyzasd"; my $r2 = sysread(FH, $b2, 3, 3);
my $w1 = syswrite(FH, "qwerty", 4, 1); my $w2 = syswrite(FH, "qwerty");
untie *FH;
print "$r1 [$buf] $r2 [$b2] $w1 $w2\n", join("\n", @T::L), "\n";
PERL

test_th('getc / eof(FH)=1 and bare eof=0 / close / binmode / fileno / seek / tell / open', $CLASS . <<'PERL');
tie *FH, 'T';
my $g = getc(FH);
my $e1 = eof(FH); my $ln = <FH>; my $e0 = eof;
my $c = close(FH); binmode(FH); binmode(FH, ":raw");
my $fn = fileno(FH); my $sk = seek(FH, 5, 1); my $t = tell(FH);
my $op = open(FH, "<", "/no/such/file");
untie *FH;
print "$g [$e1] [$e0] $c $fn $sk $t $op\n", join("\n", @T::L), "\n";
PERL

test_th('tied / untie: tie returns the object, tied(*FH) is it, a re-tie replaces it, untie calls UNTIE', $CLASS . <<'PERL');
my $o = tie *FH, 'T';
my $same = (tied(*FH) == $o) ? "same" : "diff";
my $o2 = tie *FH, 'T';
my $re = (tied(*FH) == $o2) ? "replaced" : "kept";
untie *FH;
my $after = defined(tied(*FH)) ? "tied" : "untied";
print "$same $re $after\n", join("\n", @T::L), "\n";
PERL

test_th('a TIEHANDLE that returns a non-object leaves the handle untied', <<'PERL');
package U; sub TIEHANDLE { undef }
package main;
my $r = tie *FH, 'U';
print defined($r) ? "def" : "undef", " ", (defined(tied(*FH)) ? "tied" : "untied"), "\n";
PERL

test_th('tie *STDOUT: print with no handle goes to PRINT; untie restores the real output', $CLASS . <<'PERL');
tie *STDOUT, 'T';
print "one", "two";
printf "%s!", "f";
untie *STDOUT;
print "back\n", join("\n", @T::L), "\n";
PERL

test_th('the same tie through \\*FH, {*FH}, a sub argument, *ALIAS = *FH and select(FH)', $CLASS . <<'PERL');
tie *FH, 'T';
my $fh = \*FH;
print $fh "viaref";
print {*FH} "viaglob";
sub viasub { my $h = shift; print $h "insub" }
viasub(\*FH);
*ALIAS = *FH;
print ALIAS "alias";
my $old = select(FH); print "selected"; select($old);
untie *FH;
print join("\n", @T::L), "\n";
PERL

test_th('tie *$fh on an OPEN lexical handle: tied while tied, writes to the file after untie', $CLASS . <<'PERL');
my $f = "/tmp/pcl-tiehandle-01.$$";
open(my $fh, ">", $f) or die;
tie *$fh, 'T';
print $fh "to-tie";
my $obj = ref(tied(*$fh));
untie *$fh;
print $fh "to-file\n"; close $fh;
open($fh, "<", $f) or die; my $got = <$fh>; close $fh; unlink $f;
print "$obj $got", join("\n", @T::L), "\n";
PERL

test_th('local *FH: a tie made inside the scope is gone after it (perl change 11639)', $CLASS . <<'PERL');
{ local *BAZ; tie *BAZ, 'T'; push @T::L, "inside " . ref(tied(*BAZ)); }
print defined(tied(*BAZ)) ? "tied" : "untied", "\n", join("\n", @T::L), "\n";
PERL

test_th('warn writes to a tied STDERR through PRINT', $CLASS . <<'PERL');
use Symbol;
my $fh = gensym;
tie *$fh, 'T';
{ local *STDERR = *$fh; warn "some", "text", "\n"; }
untie *$fh;
print join("\n", @T::L), "\n";
PERL

test_th('a missing tie method dies perl-shaped and trappably; eof() on a tied ARGV passes 2', <<'PERL');
package P; sub TIEHANDLE { bless {}, shift } sub EOF { "eof$_[1]" }
package main;
tie *FH, 'P';
my $ok = eval { printf FH "%s", "x"; 1 };
print $ok ? "lived" : ($@ =~ /^Can't locate object method "PRINTF" via package "P"/ ? "died" : "other: $@"), "\n";
{ local *ARGV; tie *ARGV, 'P'; print eof(), " ", eof(ARGV), "\n"; }
PERL

test_th('<> on a tied ARGV is READLINE: scalar in while(<>), list in my @a = <>', <<'PERL');
package P; sub TIEHANDLE { bless {n=>0}, shift }
sub READLINE { my $s = shift; wantarray ? ("x\n", "y\n") : ($s->{n}++ < 2 ? "r$s->{n}\n" : undef) }
package main;
{ local *ARGV; tie *ARGV, 'P'; while (<>) { print "got $_" } my @a = <>; print "list @a"; }
PERL

done_testing;

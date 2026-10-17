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

# #3144: `\do { local *HANDLE }` is a GLOB ref -- the value of a bare
# `local *NAME` is the glob (it was nil, so the ref was a SCALAR ref), and a
# blessed one keeps its class through open() (%p-install-fh attaches the stream
# to the glob).  The self-tied-handle-object idiom (Capture-Tiny's TieLC,
# IO::String).
test_th('#3144 a blessed \\do{local *H} keeps its class (and dispatch) across open/close', <<'PERL');
package K; sub new { my $fh = \do { local *HANDLE }; bless $fh, shift } sub hello { "K::hello" }
package main;
my $fh = K->new; print ref($fh), " ", $fh->hello, "\n";
open($fh, '<', $0) or die "open: $!"; print ref($fh), " ", $fh->hello, "\n";
my $line = <$fh>; print "read ", (defined $line ? "ok" : "undef"), "\n";
print "isa ", (UNIVERSAL::isa($fh, 'K') ? 1 : 0), " ", ($fh->can('hello') ? 1 : 0), "\n";
close $fh; print "closed ", ref($fh), " ", $fh->hello, "\n";
my $g = K->new; print "distinct ", ($g == $fh ? "same" : "diff"), " ", ref(\do { local *X }), "\n";
eval { $fh->nosuch }; print "nosuch ", ($@ =~ /via package "K"/ ? "K" : "other"), "\n";
PERL

test_th('#3144 a TieLC-shaped self-tied handle object writes through its own glob', <<'PERL');
package LC;
sub TIEHANDLE { my $class = shift; my $fh = \do { local *HANDLE }; bless $fh, $class;
                $fh->OPEN(@_) if @_; return $fh }
sub OPEN  { @_ == 2 ? open($_[0], $_[1]) : open($_[0], $_[1], $_[2]) }
sub CLOSE { close($_[0]) }
sub WRITE { my $fh = $_[0]; print $fh substr($_[1], 0, $_[2]) }
sub PRINT { my ($self, @what) = @_; my $buf = lc join('', @what); $self->WRITE($buf, length($buf), 0) }
package main;
my $f = "/tmp/pcl-3144-$$.txt";
tie *OUT, 'LC', '>', $f; print OUT "HeLLo WoRLD\n"; close OUT; untie *OUT;
open(my $in, '<', $f) or die; print "file: ", <$in>; close $in; unlink $f;
PERL

# #3185 (s515a): an fdopen (`>&=STDOUT`) or a dup (`>&STDOUT`) is its OWN IO in
# perl -- a tie on STDOUT is not a tie on it, and closing it leaves STDOUT open.
# PCL shared STDOUT's stream object, which is the tie table's key: PRINT's
# `print {$self}` found STDOUT's tie again and recursed (Capture-Tiny 13/14).
# The depth guard turns the old unbounded recursion into a short failure.
test_th('#3185 a tie object fdopened on the tied handle (>&=STDOUT) prints through', <<'PERL');
package TieLC; my $depth = 0;
sub TIEHANDLE { my $c = shift; my $fh = \do { local *HANDLE }; bless $fh, $c; open($fh, $_[0]) or die "open: $!"; $fh }
sub PRINT  { my $self = shift; die "RECURSION\n" if ++$depth > 20; my $r = print {$self} map { lc } @_; $depth--; $r }
sub PRINTF { my $self = shift; die "RECURSION\n" if ++$depth > 20; my $f = shift; my $r = printf {$self} $f, @_; $depth--; $r }
sub CLOSE  { my $self = shift; close $self }
package main;
tie *STDOUT, 'TieLC', ">&=STDOUT";
print "Hello WORLD\n"; printf "%s-%d\n", "ABC", 7;
close STDOUT; untie *STDOUT;
print "after untie\n";
PERL

test_th('#3185 a tie object duped from the tied handle (>&STDOUT) prints through', <<'PERL');
package TieUC; my $depth = 0;
sub TIEHANDLE { my $c = shift; my $fh = \do { local *HANDLE }; bless $fh, $c; open($fh, $_[0]) or die "open: $!"; $fh }
sub PRINT  { my $self = shift; die "RECURSION\n" if ++$depth > 20; my $r = print {$self} map { uc } @_; $depth--; $r }
sub CLOSE  { my $self = shift; close $self }
package main;
tie *STDOUT, 'TieUC', ">&STDOUT";
print "dup obj\n"; close STDOUT; untie *STDOUT;
print "after untie\n";
PERL

test_th('#3185 closing an fdopen of STDOUT leaves STDOUT open', <<'PERL');
open my $d, ">&=STDOUT" or die; print $d "via d\n"; close $d;
my $r1 = close $d; my $r2 = print $d "lost\n";
print "x ", ($r1 ? 1 : 0), ($r2 ? 1 : 0), "\n";
open my $e, ">&=", \*STDOUT or die; close $e; print "y\n";
PERL

test_th('#3185 two handles tied to the same object; untie of one leaves the other', <<'PERL');
package Obj; my $o = bless { log => [] }, 'Obj';
sub TIEHANDLE { $o } sub PRINT { my $s = shift; push @{$s->{log}}, join "", @_; 1 }
package main;
my $t1 = tie *FA, 'Obj'; my $t2 = tie *FB, 'Obj';
print FA "a1"; print FB "b1"; untie *FA; print FB "b2";
print "same ", ($t1 == $t2 ? 1 : 0), " log ", join(",", @{$t2->{log}}), " fa ", (defined tied(*FA) ? 1 : 0), "\n";
PERL

test_th('#3203 a tie made inside local *STDOUT ends with the scope; print in the scope reaches PRINT', <<'PERL');
package Obj; our @L; sub TIEHANDLE { bless {}, shift } sub PRINT { shift; push @L, join "", @_; 1 }
package main;
my @r;
{ local *STDOUT; tie *STDOUT, 'Obj'; print "hidden"; print STDOUT "h2"; push @r, (tied(*STDOUT) ? 1 : 0); }
push @r, (tied(*STDOUT) ? 1 : 0);
{ local *STDOUT; my $p = print "x"; push @r, ($p ? 1 : 0), (tied(*STDOUT) ? 1 : 0); }
print "end r=@r L=@Obj::L\n";
PERL

test_th('#3203 open of STDOUT inside local *STDOUT (static, conditional, run-time named) is undone', <<'PERL');
my $t = "/tmp/pcl-tiehandle-01-$$.txt"; my $n = "STDOUT";
{ local *STDOUT; open STDOUT, ">", $t or die; print "in1\n"; close STDOUT; }
print "out1\n";
{ local *STDOUT if 1; open STDOUT, ">>", $t or die; print "in2\n"; close STDOUT; }
{ no strict 'refs'; local *{"main::$n"}; open STDOUT, ">>", $t or die; print "in3\n"; }
print "out2\n";
open my $f, "<", $t or die; my @l = <$f>; close $f; unlink $t; print "file: @l";
PERL

test_th('#3283 an outer tie whose OPEN dies is not reached by an open inside local *STDOUT', <<'PERL');
package Evil; sub TIEHANDLE { my $s; bless \$s, shift } sub OPEN { die "unimplemented\n" }
sub PRINT { my $s = shift; $$s .= join "", @_; 1 }
package main;
tie *STDOUT, 'Evil'; my $o = tied *STDOUT; my $t = "/tmp/pcl-tiehandle-01b-$$.txt";
{ local *STDOUT; open STDOUT, ">", $t or die "open: $!"; print "inner\n"; close STDOUT; }
my $same = (tied(*STDOUT) == $o) ? 1 : 0; print "outer\n"; untie *STDOUT;
open my $f, "<", $t or die; my $l = <$f>; close $f; unlink $t;
print "same=$same file=$l", "buf=${$o}";
PERL

done_testing;

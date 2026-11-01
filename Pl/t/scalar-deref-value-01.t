#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later
# scalar-deref-value-01.t — `${ EXPR }` OF A REFERENCE VALUE THAT POINTS AT A
# SCALAR HOLDING A REFERENCE IS THAT REFERENCE (tasks #3551, #3827 shape 1;
# s531d).
#
# PCL keeps a hash / array / code reference as a BOX around the raw table /
# vector / function: the box IS the reference.  p-cast-$ answered the BOX
# when EXPR was a variable (`$$r`) but its bare PAYLOAD when EXPR was a
# reference VALUE -- `${ \$x }`, `${ f() }`, `${ scalar($r) }`, an overload
# handler's `${$_[0]}` on an object returned by a constructor.  The payload
# is not a reference: a list spread it (`print ${\$x}` printed 78), a scalar
# store took a hash's KEY COUNT, and a method called on it got the raw hash,
# whose `my $self = shift` read 0 -- `is(Mojo::DOM58->new, '')` died "Can't
# use string ("0") as a HASH ref".  The deref now answers the box, the
# symbolic-ref arm's #3394 rule.  Every expected line is perl 5.40.3's.

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

plan tests => 3;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    is(run_cl($code), $expected, $name);
}

test_cl('${ REF-VALUE } of a scalar holding an array / hash / code ref is the reference (#3827)',
        <<'PL', <<'OUT');
my $x = [7, 8]; my $r = \$x;
print ${\$x} =~ /^ARRAY/ ? "A" : "?", "\n";
my @l = (${\$x}); print scalar(@l), "\n";
sub n { scalar(@_) } print n(${\$x}), " ", n($$r), "\n";
my $h = {k => 1}; print join(",", map { ref } ${\$h}, ${ \$h }), "\n";
my $c = sub { "C@_" }; print ${\$c}->(1), " ", &{${\$c}}(2), "\n";
my $s = "str"; print ${\$s}, " ", ${ \ "lit" }, " ", ${\ 42}, "\n";
${\$x}->[0] = 9; print "$x->[0]\n";
sub f { \$_[0] } my $v = [1]; print ref(${ f($v) }), "\n";
my %hh = (a => [1,2]); sub g { \$hh{a} } print scalar(@{ ${ g() } }), " ", ref(${ g() }), "\n";
my @copy = map { $_ } ${\$x}; print scalar(@copy), "\n";
my $n = ${ \$x }; print ref($n), "\n";
PL
A
1
1 1
HASH,HASH
C1 C2
str lit 42
9
ARRAY
2 ARRAY
1
ARRAY
OUT

test_cl('a method called on ${ REF-VALUE } gets the object, not its hash (#3551)',
        <<'PL', <<'OUT');
use strict;
package K; sub new { bless {n=>1}, shift } sub me2 { my $s = shift; return ref($s) unless @_; "x" }
sub mk { return bless \ K->new, 'O' }
package main;
print "1 ", ${ \ K->new }->me2, "\n";
my $o = K->new;
print "2 ", ${ \$o }->me2, "\n";
print "3 ", ${ K::mk() }->me2, "\n";
print "4 ", ${ scalar(K::mk()) }->me2, "\n";
sub id2 { return $_[0] } print "5 ", ${ id2(\$o) }->me2, "\n";
my $x = ${ \ K->new }; print "6 ", ref($x), "\n";
sub takes { my $v = shift; ref $v } print "7 ", takes(${ \ K->new }), " ", takes(${ \$o }), "\n";
my @l = (${ \$o }); print "8 ", ref($l[0]), "\n";
PL
1 K
2 K
3 K
4 K
5 K
6 K
7 K K
8 K
OUT

test_cl('an overload handler derefs a returned object (Mojo::DOM58 shape, #3551)',
        <<'PL', <<'OUT');
package Inner; use strict;
sub new { bless {}, shift }
sub t4 { my $self = shift; return ref($self) . ":" . scalar(@_) unless @_; 5 }
sub t9 { my ($self) = @_; return ref($self) }
package Outer; use strict;
use overload '""' => sub { my $i = ${$_[0]}; ref($i) . "/" . ${$_[0]}->t9 . "/" . ${$_[0]}->t4 }, fallback => 1;
sub new { my $class = shift; my $self = bless \Inner->new, $class; return $self; }
sub new4 { return bless \Inner->new, $_[0] }
package main;
print "A ", "" . scalar(Outer->new), "\n";
print "B ", "" . Outer->new, "\n";
my $o = Outer->new; print "C ", "" . $o, "\n";
print "D ", "" . scalar($o), "\n";
print "E ", "" . scalar(Outer->new4), "\n";
PL
A Inner/Inner/Inner:0
B Inner/Inner/Inner:0
C Inner/Inner/Inner:0
D Inner/Inner/Inner:0
E Inner/Inner/Inner:0
OUT

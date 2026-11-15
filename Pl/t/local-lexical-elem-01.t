#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# local-lexical-elem-01.t -- `local` on an ELEMENT of a file-lexical aggregate
# (task #4866, s540c).
#
# A file-level `my %h` / `my @a` / `my $r` that a named sub captures becomes a
# package cell, and in a MODULE (or a program that declares the name twice)
# the cell takes the mangled name `%h__file__N`.  The rename skipped exactly
# one use: the symbol after `local`, because `_symbol_is_declarator` counted
# `local` as a declarator.  So `local $h{k} = V` saved and restored the
# ORIGINAL-name cell while every other use read the renamed one -- a silent
# wrong (every Try::Tiny `finally` block got an empty @_).  `local` declares
# nothing; the binder resolves its operand like any use.
#
# Every expected text below was probed under perl 5.40.3.  The module rows run
# through BOTH load paths: the fasl (PCL_COMPILE_DIRS='*', the fixtures live
# in a tempdir) and the text (PCL_NO_FASL_CACHE=1).

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use File::Spec;
use FindBin qw($RealBin);

my $root = File::Spec->rel2abs("$RealBin/../..");
my $pcl  = "$root/pcl";
my $pl2cl = "$root/pl2cl";

plan skip_all => "pcl not found"  if !-x $pcl;
plan skip_all => "sbcl not found" if !`which sbcl 2>/dev/null`;

plan tests => 7;

my $dir = tempdir(CLEANUP => 1);
local $ENV{PCL_CACHE_DIR} = $ENV{PCL_CACHE_DIR};   # the shared cache, as the gate runs

sub write_file {
    my ($name, $body) = @_;
    my $path = File::Spec->catfile($dir, $name);
    open my $fh, '>', $path or die "write $path: $!";
    print $fh $body;
    close $fh;
    return $path;
}

# THE MODULE: every shape of `local` on an element of a file lexical.
my $mod = write_file('LexLocal.pm', <<'PM');
package LexLocal;
our %o;
my %h = (base => 1);
my @a = (1, 2, 3);
my $r = { k => 'orig' };
my $early = sub { $h{late} // 'undef' };
sub ourl   { local $o{q} = 7; return "our=$o{q}" }
sub init   { local $h{q} = 9; return "init=$h{q}" }
sub assign { local $h{q}; $h{q} = 8; return "assign=$h{q}" }
sub aelem  { local $a[0] = 10; return "aelem=$a[0]" }
sub hslice { local @h{qw(a b)} = (5, 6); return "hslice=$h{a},$h{b}" }
sub viaref { local $r->{k} = 'tmp'; return "viaref=$r->{k}" }
sub reader { return "reader=" . ($h{shared} // 'undef') }
sub writer { local $h{shared} = 'w'; return reader() }
sub clos   { local $h{late} = 'L'; return "clos=" . $early->() }
sub nested { local $h{n} = 1; { local $h{n} = 2; } return "nested=$h{n}" }
sub after  { return join ' ', "a0=$a[0]", "rk=$r->{k}", "keys=" . join(',', sort keys %h) }
1;
PM

my $code = 'use LexLocal; print join("|", LexLocal::ourl(), LexLocal::init(), LexLocal::assign(), '
         . 'LexLocal::init(), LexLocal::aelem(), LexLocal::hslice(), LexLocal::viaref(), '
         . 'LexLocal::writer(), LexLocal::clos(), LexLocal::nested(), LexLocal::after()), "\n";';
my $want = "our=7|init=9|assign=8|init=9|aelem=10|hslice=5,6|viaref=tmp|reader=w|clos=L|nested=1"
         . "|a0=1 rk=orig keys=base\n";

sub run_pcl {
    my ($src, %env) = @_;
    local $ENV{PERL5LIB};
    delete $ENV{PERL5LIB};
    local @ENV{keys %env} = values %env;
    my $out = `$pcl -I '$dir' -E '$src' 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^PCL: module .*\n//gm;
    return $out;
}

# 1-2: the module, through the fasl path and through the text path.
{
    local $ENV{PCL_NO_FASL_CACHE};
    delete $ENV{PCL_NO_FASL_CACHE};
    is(run_pcl($code, PCL_COMPILE_DIRS => '*'), $want,
       'module, fasl path: local on file-lexical elements reaches the renamed cell (#4866)');
}
is(run_pcl($code, PCL_NO_FASL_CACHE => 1), $want,
   'module, text path: the same answers');

# 3: the emission -- the local form names the SAME cell as the plain reads.
{
    my $cl = `perl -I '$root' '$pl2cl' --module '$mod' 2>/dev/null`;
    my ($cell) = $cl =~ /\(p-defcell (%h__file__\d+) /;
    ok(defined $cell && $cl =~ /\(p-local-hash-elem-init \Q$cell\E "q" 9/
                     && $cl !~ /\(p-local-hash-elem(?:-init)? %h /,
       'emission: `local $h{q}` names the promoted %h cell, never the bare %h');
}

# 4: Try::Tiny's own shape -- a finally guard list kept in a localized element
# of a file-lexical hash, read by a sub the local's scope calls.
{
    write_file('Guarded.pm', <<'PM');
package Guarded;
my %guards;
sub run { local $guards{list} = [ @_ ]; return inner() }
sub inner { my $g = $guards{list}; return ref($g) ? scalar(@$g) : 'none' }
sub left { return exists $guards{list} ? 'leaked' : 'restored' }
1;
PM
    is(run_pcl('use Guarded; print Guarded::run(qw(a b c)), " ", Guarded::left(), "\n";',
               PCL_COMPILE_DIRS => '*'),
       "3 restored\n", 'the Try::Tiny finally-guard shape: the callee sees the localized list');
}

# 5-6: a PROGRAM takes the mangle too when the file declares the name twice.
{
    my $prog = write_file('twice.pl', <<'PL');
my %h;
sub f { local $h{q} = 9; return "f=" . ($h{q} // 'undef') }
sub g { local $h{q}; $h{q} = 8; return "g=$h{q}" }
sub gone { return exists $h{q} ? "leaked" : "gone" }
my @a = (1, 2, 3);
sub la { local $a[0] = 10; return "la=$a[0]" }
print join("|", f(), g(), gone(), f(), la(), "a0=$a[0]"), "\n";
{ my %h = (x => 1); my @a = (5); }
PL
    my $out = `$pcl '$prog' 2>&1`;
    $out =~ s/^PCL Runtime loaded\n//gm;
    is($out, "f=9|g=8|gone|f=9|la=10|a0=1\n",
       'program, name declared twice (mangled cell): local on its elements');
    my $cl = `perl -I '$root' '$pl2cl' '$prog' 2>/dev/null`;
    ok($cl =~ /\(p-defcell %h__file__\d+ / && $cl !~ /\(p-local-hash-elem(?:-init)? %h /,
       'program emission: the local form names the mangled cell');
}

# 7: a scalar `local` on a PACKAGE variable beside a same-named file lexical
# in ANOTHER package section is untouched (local still never renames a plain
# `local $x`, which can only name a package variable).
{
    write_file('PkgLocal.pm', <<'PM');
package PkgLocal;
our $x = 'pkg';
sub show { return $x }
sub tmp { local $x = 'tmp'; return show() }
1;
PM
    is(run_pcl('use PkgLocal; print PkgLocal::tmp(), " ", PkgLocal::show(), "\n";',
               PCL_COMPILE_DIRS => '*'),
       "tmp pkg\n", '`local $x` on a package variable: unchanged');
}

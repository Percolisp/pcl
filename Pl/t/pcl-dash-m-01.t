#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Test the `pcl` runner's -M handling, matching perl(1) semantics:
#   -MModule            → use Module;
#   -MModule=a,b        → use Module qw(a b);   (import list)
#   -M-Module           → no Module;
# Regression: `pcl -MData::Dump=dump -E '...'` used to ignore the `=dump`
# import list, so `dump` was never imported and the run died with
# "function pl-dump is undefined".

use v5.30;
use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);

my $root = "$RealBin/../..";
my $pcl  = "$root/pcl";

plan skip_all => "pcl not found"  unless -x $pcl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

# Strip the runtime banner line that pcl prints on stderr/stdout.
sub run_pcl {
    my (@args) = @_;
    my $cmd = join(' ', $pcl, @args) . ' 2>&1';
    my $out = `$cmd`;
    $out =~ s/^PCL Runtime loaded\n//m;
    return $out;
}

# -MModule=imports → the named imports are available unqualified.
{
    my $out = run_pcl(q{-MList::Util=sum,max -E 'say sum(1,2,3); say max(4,5,6)'});
    is($out, "6\n6\n", '-MList::Util=sum,max imports sum and max');
}

# -MModule (no imports) → module loaded, fully-qualified call works.
{
    my $out = run_pcl(q{-MPOSIX -E 'say POSIX::floor(3.9)'});
    is($out, "3\n", '-MPOSIX with no import list still loads the module');
}

# The original report: Data::Dump=dump.  The SUBJECT is the real CPAN module
# (transpiled from @INC) -- a fixture, not a PCL dependency; the row SKIPS
# where it is not installed (a stock perl has only PPI and Moo).
SKIP: {
    skip "Data::Dump not installed (the CPAN module is this row's fixture)", 1
        if !eval { require Data::Dump; 1 };
    my $out = run_pcl(q{-MData::Dump=dump -E 'dump({a=>1,b=>[2,3]})'});
    like($out, qr/\{ a => 1, b => \[2, 3\] \}/,
         '-MData::Dump=dump imports dump()');
}

# #526: `use Test::More` printed "# PCL Test library loaded" on STDOUT ahead
# of the TAP -- but only when the TAP layer was loaded at RUN time (a warm
# script cache, every `pcl -e` after the first); a cold MISS loaded it while
# compiling, with stdout muffled.  So each spelling runs TWICE: the second run
# is the warm one.  --check compares STDOUT byte for byte against perl.
for my $pass (1, 2) {
    my $out = `$pcl --check -e 'use Test::More tests => 1; ok(1, "one")' 2>&1`;
    like($out, qr/^pcl --check: IDENTICAL/m,
         "#526 pcl --check on a Test::More one-liner is IDENTICAL (run $pass)")
        or diag $out;
}

# ── #2461 / #2460: $0, __FILE__ and line numbers under -e and -M ───────────
# `pcl -e` used to report its temp file for $0 and __FILE__, and a file run
# with -M was compiled from a temp copy with the use-lines on lines of their
# own: $0, __FILE__ and every line number named the copy, and FindBin died
# "Cannot find current script".  Each row runs perl and pcl on the same
# command line, from the same directory, and compares stdout+stderr.
use File::Temp qw(tempdir);
use File::Path qw(make_path);
my $fx = tempdir(CLEANUP => 1);
sub fx_write { my ($rel, $text) = @_; my $p = "$fx/$rel"; (my $d = $p) =~ s{/[^/]*$}{};
               make_path($d); open my $h, '>', $p or die "$p: $!"; print $h $text; close $h }
fx_write('sub/zero.pl', <<'PL');
use FindBin;
print "$0 ", __FILE__, " ", __LINE__, "\n";
print "bin:", ($FindBin::Bin =~ m{/sub$} ? 1 : 0), " script:$FindBin::Script\n";
warn "w\n"; warn "x";
print Dumper([1]) if defined &Dumper;
PL
sub same_as_perl {
    my ($name, $args, %o) = @_;
    my $env = $o{env} ? "$o{env} " : '';
    my $want = `cd '$fx' && ${env}perl $args 2>&1`;
    my $got  = `cd '$fx' && ${env}$pcl $args 2>&1`;
    $got =~ s/^PCL Runtime loaded\n//m;
    is($got, $want, $name);
}
same_as_perl('#2461 pcl -e: $0 and __FILE__ are "-e", warn says "at -e line 1"',
    q{-e 'print "$0 ", __FILE__, " ", __LINE__, "\n"; warn "x"'});
same_as_perl('#2461 pcl -E: $0 is "-e"', q{-E 'say "$0 ", __FILE__'});
same_as_perl('#2460 pcl -M Mod sub/script.pl: $0, __FILE__, line numbers, FindBin, warn name the script',
    q{-MData::Dumper sub/zero.pl});
same_as_perl('#2460 pcl -M Mod -e: the -M line takes no line of its own',
    q{-MData::Dumper -e 'print __LINE__, " $0\n"; warn "y"; print Dumper(2)'});

# ── #2462: @INC in perl's ORDER, the PCL tree out of it, shims by name ─────
# perl: -I entries, then PERL5LIB, then the library.  PCL puts its shim lib/
# in front of perl's own directories and nowhere else; the PCL tree ROOT is
# not a library and must not make Pl::* requireable.  A shim that replaces a
# module whose real copy cannot run under PCL (the `# pcl-shim: must-win`
# marker) wins by NAME: a PERL5LIB carrying perl's REAL List/Util.pm and
# Carp.pm -- ordinary under local::lib -- must not break the program.  A
# pure-Perl shim does NOT pre-empt a user's -I copy.
fx_write('ia/.keep', ''); fx_write('ib/.keep', ''); fx_write('px/.keep', '');
fx_write('userlib/File/Spec/Functions.pm',
         "package File::Spec::Functions; sub whose { 'user copy' } 1;\n");
for my $m ('List/Util.pm', 'Carp.pm') {
    (my $mod = $m) =~ s{/}{::}g; $mod =~ s/\.pm$//;
    my $real = `perl -M$mod -e 'print \$INC{"$m"}'`;
    open my $in, '<', $real or die "$real: $!"; local $/; my $t = <$in>;
    fx_write("reallib/$m", $t);
}
fx_write('lu.pl', <<'PL');
use List::Util qw(sum first);
use Carp;
print sum(1,2,3), " ", (first { $_ > 1 } 1,2,3), "\n";
eval { croak "boom\n" }; print "carp:", ($@ =~ /^boom/ ? 1 : 0), "\n";
PL
same_as_perl('#2462 -I entries come first, in command-line order',
    q{-I ia -I ib -e 'print "@INC[0..1]\n"'});
same_as_perl('#2462 PERL5LIB follows the -I entries',
    q{-I ia -e 'print "@INC[0..1]\n"'}, env => 'PERL5LIB=px');
same_as_perl('#2462 a user -I copy shadows a pure-Perl shim (File::Spec::Functions)',
    q{-I userlib -e 'require File::Spec::Functions; print defined(&File::Spec::Functions::whose) ? File::Spec::Functions::whose() : "shim", "\n"'});
same_as_perl('#2462 PERL5LIB holding the REAL List::Util and Carp: the must-win shims still load',
    q{lu.pl}, env => 'PERL5LIB=reallib');
{
    my $out = `cd '$fx' && $pcl -e 'print eval { require Pl::Parser; 1 } ? "loaded\n" : "died\n"; print scalar(grep { m{/tools/lib\$} || -f "\$_/cl/pcl-runtime.lisp" } \@INC), "\n"' 2>&1`;
    is($out, "died\n0\n", '#2462 the PCL tree is not on @INC: require Pl::Parser dies, no root/tools/lib entry');
}

done_testing();

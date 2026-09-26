#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# strict-refs-01.t — task #2103: `use strict 'refs'` is ENFORCED, lexically.
#
# The pragma (Pl::Parser::strict_refs_regions_of) is a set of source-location
# regions — the `use open` shape: a region runs from `use strict` / `no
# strict` (naming refs, or bare) or an implying `use VERSION` >= 5.011 to the
# end of its enclosing block, an explicit statement beats a `use VERSION`
# whichever comes first, and a string eval inherits its site's answer.  A
# FILE-level reading would kill every `{ no strict 'refs'; *{"..."} = ... }`
# block, which is why the block-scoped rows below are the breaking cases.
#
# The emitter marks each dereference site (Pl::ExprToCL::_strict_deref_marker,
# docs/ir-spec.md §3.2c): :strict on an RVALUE deref under strict refs (undef,
# a string and a number die), :strict-lv on a vivifying one (a string or a
# number dies, undef vivifies), :rvalue on an rvalue deref outside strict (the
# empty symbolic @{""}, no vivification), nothing on the rest.  An element
# READ `$r->[0]` / `$r->{k}` carries :strict under strict refs.
#
# The run rows are two programs (one strict file, one not), each line an
# independent eval'd probe; expected = perl 5.40.3's output of the same
# source, recorded at authoring time (died/lived AND the message — the
# message text is not a goal, but PCL's happens to match and a row that
# stops matching is worth reading).  The USER's bar is the PLACE.

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

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub cl_of { PCLCore::transpile("$pl2cl " . write_pl($_[0])) }

sub run_cl {
    my ($code) = @_;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh cl_of($code);
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub compare_run {
    my ($got, $want, $what) = @_;
    my @g = split /\n/, $got;
    my @w = split /\n/, $want;
    for my $i (0 .. $#w) {
        my ($tag) = $w[$i] =~ /^([^:]+)/;
        is($g[$i] // '<missing>', $w[$i], "$what: $tag");
    }
    is(scalar(@g), scalar(@w), "$what: no extra output lines");
}

# ---- the markers (transpile only) ------------------------------------------

like(cl_of('use strict; my $u; my @a = @$u; print "@a\n";'),
     qr/\(p-cast-\@ \$u :strict\)/, 'strict rvalue deref: :strict');
like(cl_of('use strict; my $u; push @$u, 1; print "@$u\n";'),
     qr/\(p-cast-\@ \$u :strict-lv\)/, 'strict vivifying deref: :strict-lv');
like(cl_of('use strict; my $u; my $v = $u->[0]; print defined $v ? 1 : 0;'),
     qr/\(p-aref-deref \$u 0 :strict\)/, 'strict element read: :strict');
like(cl_of('my $u; my @a = @$u; print scalar(@a), "\n";'),
     qr/\(p-cast-\@ \$u :rvalue\)/, 'no-strict rvalue deref: :rvalue');
unlike(cl_of('my $u; push @$u, 1; print "ok\n";'),
       qr/\(p-cast-\@ \$u :/, 'no-strict vivifying deref: unmarked');
like(cl_of(qq{use strict; our \@g = (1); { no strict 'refs'; my \@a = \@{"main::g"}; print "\@a\\n"; }}),
     qr/\(p-cast-\@ "main::g" \(p-symref-site\)\)/,
     'a `no strict refs` block keeps the symbolic idiom and its site cache');
like(cl_of(qq{use strict; sub f { no strict 'refs'; my \$n = shift; scalar \@{"main::\$n"} } my \$u; my \@a = \@\$u;}),
     qr/\(p-cast-\@ \(p-string-concat "main::" \$n\) :rvalue\).*\(p-cast-\@ \$u :strict\)/s,
     'the sub\'s `no strict refs` scopes to the sub; the file stays strict');
like(cl_of(qq{use strict 'subs'; my \$u; my \@a = \@\$u; print scalar(\@a);}),
     qr/\(p-cast-\@ \$u :rvalue\)/, "`use strict 'subs'` alone does not turn refs on");
like(cl_of(qq{use v5.12; my \$u; my \@a = \@\$u; print scalar(\@a);}),
     qr/\(p-cast-\@ \$u :strict\)/, '`use v5.12` implies strict refs');

# ---- the run rows ----------------------------------------------------------

my $prog1 = <<'PERL';
use strict;
no warnings;
sub t { my ($name, $code) = @_; my $r = eval { $code->(); 1 }; my $e = $@; $e =~ s/ at .*//s; print "$name: ", ($r ? "lived" : "died: $e"), "\n" }
our ($x, @x, %x) = (7, (1, 2), ());
# --- rvalue derefs of undef die under strict ---
t("rv my \@a = \@\$u",   sub { my $u; my @a = @$u });
t("rv my (\$a) = \@\$u", sub { my $u; my ($a) = @$u });
t("rv my %h = %\$u",     sub { my $u; my %h = %$u });
t("rv join",             sub { my $u; my $j = join ",", @$u });
t("rv scalar()",         sub { my $u; my $n = scalar(@$u) });
t("rv my \$n = \@\$u",   sub { my $u; my $n = @$u });
t("rv sort",             sub { my $u; my @s = sort @$u });
t("rv reverse",          sub { my $u; my @s = reverse @$u });
t("rv interp",           sub { my $u; my $s = "@$u" });
t("rv list",             sub { my $u; my @s = (@$u, 1) });
t("rv if",               sub { my $u; if (@$u) {} });
t("rv if %",             sub { my $u; if (%$u) {} });
t("rv !",                sub { my $u; if (!@$u) {} });
t("rv ternary cond",     sub { my $u; my $v = @$u ? 1 : 0 });
t("rv &&",               sub { my $u; my $v = @$u && 1 });
t("rv while",            sub { my $u; while (@$u) { last } });
t("rv \$\$u",            sub { my $u; my $v = $$u });
t("rv print",            sub { my $u; open my $fh, ">", \my $buf; print $fh @$u });
t("rv return",           sub { my $u; my @r = (sub { return @$u })->() });
t("rv ==",               sub { my $u; my $v = @$u == 0 });
t("rv [ ]",              sub { my $u; my $v = [@$u] });
t("rv { }",              sub { my $u; my $v = {%$u} });
t("rv elem of raw undef", sub { my $v = (sub { undef })->()->[0] });
# --- vivifying positions live and vivify under strict ---
t("viv push",            sub { my $u; push @$u, 1; die "no\n" if ref $u ne "ARRAY" });
t("viv for",             sub { my $u; for (@$u) {} die "no\n" if ref $u ne "ARRAY" });
t("viv keys",            sub { my $u; my @k = keys %$u; die "no\n" if ref $u ne "HASH" });
t("viv map",             sub { my $u; my @m = map { $_ } @$u; die "no\n" if ref $u ne "ARRAY" });
t("viv grep",            sub { my $u; my @g = grep { $_ } @$u; die "no\n" if ref $u ne "ARRAY" });
t("viv elem read",       sub { my $u; my $v = $u->[0]; die "no\n" if ref $u ne "ARRAY" });
t("viv chain read",      sub { my $u; my $v = $u->{a}{b}; die "no\n" if ref $u->{a} ne "HASH" });
t("viv exists",          sub { my $u; my $v = exists $u->{k}; die "no\n" if ref $u ne "HASH" });
t("viv delete",          sub { my $u; my $v = delete $u->{k}; die "no\n" if ref $u ne "HASH" });
t("viv \$#",             sub { my $u; my $n = $#$u; die "no\n" if ref $u ne "ARRAY" });
t("viv \\\@",            sub { my $u; my $r = \@$u; die "no\n" if ref $u ne "ARRAY" });
t("viv sub arg",         sub { my $u; sub f1 { scalar @_ } f1(@$u); die "no\n" if ref $u ne "ARRAY" });
t("viv slice",           sub { my $u; my @s = @$u[0,1]; die "no\n" if ref $u ne "ARRAY" });
t("viv assign",          sub { my $u; @$u = (1); die "no\n" if ref $u ne "ARRAY" });
t("viv \$\$u = 1",       sub { my $u; $$u = 1; die "no\n" if ref $u ne "SCALAR" });
# --- strings and numbers die under strict ---
t("str \@\$s",           sub { my $s = "x"; my @a = @$s });
t("str push",            sub { my $s = "x"; push @$s, 1 });
t("str %\$s",            sub { my $s = "x"; my %h = %$s });
t("str \$\$s",           sub { my $s = "x"; my $v = $$s });
t("str \$\$s = 1",       sub { my $s = "x"; $$s = 1 });
t("str ->[0]",           sub { my $s = "x"; my $v = $s->[0] });
t("str ->{k}",           sub { my $s = "x"; my $v = $s->{k} });
t("num ->[0]",           sub { my $n = 5; my $v = $n->[0] });
t("num \@\$n",           sub { my $n = 5; my @a = @$n });
# --- no strict 'refs' in a block: the symbolic idiom lives ---
t("block no strict",     sub { no strict 'refs'; my $n = "x"; my @a = @{"main::$n"}; die "no\n" if "@a" ne "1 2" });
t("block no strict \$",  sub { no strict 'refs'; my $v = ${"main::x"}; die "no\n" if $v != 7 });
t("block glob install",  sub { my $n = "gen1"; { no strict 'refs'; *{"main::$n"} = sub { 42 } } die "no\n" if main->$n != 42 });
t("after block strict",  sub { { no strict 'refs'; } my $s = "x"; my @a = @$s });
t("strict subs only",    sub { no strict; use strict 'subs'; my $s = "x"; my @a = @$s });
t("no strict subs",      sub { no strict 'subs'; my $s = "x"; my @a = @$s });
sub nostrict_sub { no strict 'refs'; my $n = shift; return scalar @{"main::$n"} }
t("sub-level no strict", sub { die "no\n" if nostrict_sub("x") != 2 });
{ no strict; sub nostrict2 { my $s = "x"; my @a = @$s; scalar @a } }
t("block-scoped sub",    sub { die "no\n" if nostrict2() != 2 });
PERL

my $want1 = <<'WANT';
rv my @a = @$u: died: Can't use an undefined value as an ARRAY reference
rv my ($a) = @$u: died: Can't use an undefined value as an ARRAY reference
rv my %h = %$u: died: Can't use an undefined value as a HASH reference
rv join: died: Can't use an undefined value as an ARRAY reference
rv scalar(): died: Can't use an undefined value as an ARRAY reference
rv my $n = @$u: died: Can't use an undefined value as an ARRAY reference
rv sort: died: Can't use an undefined value as an ARRAY reference
rv reverse: died: Can't use an undefined value as an ARRAY reference
rv interp: died: Can't use an undefined value as an ARRAY reference
rv list: died: Can't use an undefined value as an ARRAY reference
rv if: died: Can't use an undefined value as an ARRAY reference
rv if %: died: Can't use an undefined value as a HASH reference
rv !: died: Can't use an undefined value as an ARRAY reference
rv ternary cond: died: Can't use an undefined value as an ARRAY reference
rv &&: died: Can't use an undefined value as an ARRAY reference
rv while: died: Can't use an undefined value as an ARRAY reference
rv $$u: died: Can't use an undefined value as a SCALAR reference
rv print: died: Can't use an undefined value as an ARRAY reference
rv return: died: Can't use an undefined value as an ARRAY reference
rv ==: died: Can't use an undefined value as an ARRAY reference
rv [ ]: died: Can't use an undefined value as an ARRAY reference
rv { }: died: Can't use an undefined value as a HASH reference
rv elem of raw undef: died: Can't use an undefined value as an ARRAY reference
viv push: lived
viv for: lived
viv keys: lived
viv map: lived
viv grep: lived
viv elem read: lived
viv chain read: lived
viv exists: lived
viv delete: lived
viv $#: lived
viv \@: lived
viv sub arg: lived
viv slice: lived
viv assign: lived
viv $$u = 1: lived
str @$s: died: Can't use string ("x") as an ARRAY ref while "strict refs" in use
str push: died: Can't use string ("x") as an ARRAY ref while "strict refs" in use
str %$s: died: Can't use string ("x") as a HASH ref while "strict refs" in use
str $$s: died: Can't use string ("x") as a SCALAR ref while "strict refs" in use
str $$s = 1: died: Can't use string ("x") as a SCALAR ref while "strict refs" in use
str ->[0]: died: Can't use string ("x") as an ARRAY ref while "strict refs" in use
str ->{k}: died: Can't use string ("x") as a HASH ref while "strict refs" in use
num ->[0]: died: Can't use string ("5") as an ARRAY ref while "strict refs" in use
num @$n: died: Can't use string ("5") as an ARRAY ref while "strict refs" in use
block no strict: lived
block no strict $: lived
block glob install: lived
after block strict: died: Can't use string ("x") as an ARRAY ref while "strict refs" in use
strict subs only: lived
no strict subs: died: Can't use string ("x") as an ARRAY ref while "strict refs" in use
sub-level no strict: lived
block-scoped sub: lived
WANT

my $prog2 = <<'PERL';
no warnings;
sub t { my ($name, $code) = @_; my $r = eval { $code->(); 1 }; my $e = $@; $e =~ s/ at .*//s; print "$name: ", ($r ? "lived" : "died: $e"), "\n" }
our @x = (1, 2);
sub strict_inside { use strict; my $s = "x"; my @a = @$s; scalar @a }
t("use strict in a sub",   sub { strict_inside() });
t("file is not strict",    sub { my $s = "x"; my @a = @$s; die "no\n" if @a != 2 });
t("rv undef no strict",    sub { my $u; my @a = @$u; die "no\n" if @a || defined $u });
t("interp no strict",      sub { my $u; my $s = "[@$u]"; die "no\n" if $s ne "[]" || defined $u });
{ use v5.12; sub v512 { my $s = "x"; my @a = @$s; 1 } }
t("use v5.12 implies",     sub { v512() });
{ no strict "refs"; use v5.12; sub v512off { my $s = "x"; my @a = @$s; scalar @a } }
t("explicit no strict wins", sub { die "no\n" if v512off() != 2 });
{ use 5.012; sub n5012 { my $u; my @a = @$u; 1 } }
t("use 5.012 implies",     sub { n5012() });
{ use v5.10; sub v510 { my $s = "x"; my @a = @$s; scalar @a } }
t("use v5.10 does not",    sub { die "no\n" if v510() != 2 });
{ use strict; my $s = "x"; t("string eval inherits no pragma", sub { my $r = eval q{ my @a = @$s; scalar @a }; die "no\n" if $r != 2 }); }
PERL

my $want2 = <<'WANT';
use strict in a sub: died: Can't use string ("x") as an ARRAY ref while "strict refs" in use
file is not strict: lived
rv undef no strict: lived
interp no strict: lived
use v5.12 implies: died: Can't use string ("x") as an ARRAY ref while "strict refs" in use
explicit no strict wins: lived
use 5.012 implies: died: Can't use an undefined value as an ARRAY reference
use v5.10 does not: lived
string eval inherits no pragma: died: no
WANT

compare_run(run_cl($prog1), $want1, 'use strict file');
compare_run(run_cl($prog2), $want2, 'no strict file');

done_testing();

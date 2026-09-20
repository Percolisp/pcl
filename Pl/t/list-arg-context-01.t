#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# list-arg-context-01.t — task #2004: a CORE BUILTIN's argument context is a
# fact of its PROTOTYPE, not of the context the call itself sits in.
#
# `my $s = sprintf("%02d:%02d:%02d", @t[2,1,0])` formatted the slice's LAST
# element ("05:00:00" where perl says "03:04:05"), `sprintf("%s-%s", pair())`
# called pair() in SCALAR context, and the canonical
# `sprintf("%04d-%02d-%02d", (localtime)[5,4,3])` was wrong — silently, in
# code people write every day.  It reached core Time::Local, whose cache key
# is `pack('ss', @_[4,5])`: `timegm` answered the same epoch for every month.
#
# THE CAUSE: `Pl::PExpr::child_context` decided a builtin's argument context
# from SIX hand-written name regexes (`map|grep|sort|…`, `join`,
# `push|unshift|splice|reverse`, `chop|chomp`, `print|say`, `scalar`, the
# named unaries, `split`, the filehandle-first list), and a builtin in NONE of
# them inherited its caller's context.  sprintf / pack / die / warn / printf /
# system / exec / chmod / chown / kill / utime / formline / syscall / tie were
# in no list.  The lists also disagreed with perl where they did fire:
# `reverse` is `(@)`, slurpy from argument 0, but the hand rule started at
# argument 1 (`reverse sort @x` ran the sort in scalar context), and `splice`'s
# OFFSET and LENGTH are `$$`, which the same rule made LIST.
#
# THE FIX is ONE reading — `Pl::PExpr::Config::core_arg_context`, which asks
# the running perl for `prototype("CORE::NAME")` (the same authority the
# runtime's generated `%pcl-core-prototypes` table is built from) and answers
# LIST at and after the first slurpy slot, SCALAR for `$`/`_`/`+`/`*`, and
# "inherit" for a reference slot.  The builtins perl gives no prototype
# (print/say/printf/system/exec/chop/chomp/split/eval/defined) are ONE
# explicit table beside it.  The map/grep/sort arm stays: it answers a
# STRUCTURAL question (which CHILD is the list), not a context one.
#
# A LIST SLICE is the same leak on another node: `(LIST)[i]` evaluates both
# its list and its index list in LIST context whatever context the slice sits
# in — `sub r { return (ctx(), "z")[0] }` called in scalar context still runs
# ctx() in list context, and `(localtime)[5,4,3]` must not lower to
# `(progn 5 4)`.  child_context reads the `list_ctx_subscript` marker PExpr's
# two slice sites already set.
#
# Every row here is compared against THIS perl: the bar is agreement, never a
# written-down string.  Inverse-verified on a 5ce155cb extraction (rows 1-5
# fail there).

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

plan tests => 11;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^(?:Missing|Redundant) argument in sprintf.*\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub both_agree {
    my ($code, $desc) = @_;
    my $pl_file = write_pl($code);
    my $perl = `perl $pl_file 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, $desc);
}

my $PRELUDE = <<'P';
sub ctx  { wantarray ? "L" : defined(wantarray) ? "S" : "V" }
sub pair { (1, 2) }
sub two  { ("a", "b") }
my @g = (5, 4, 3);
my %h = (x => 1, y => 2);
P

# ---- 1. the slurpy tail: a slice / a list-returning call in a scalar-context
#         sprintf / pack / die / warn / printf ------------------------------
both_agree($PRELUDE . <<'P', 'slurpy-tail arguments are LIST context (sprintf/pack/die/warn)');
print "a ", sprintf("%02d:%02d:%02d", @g[2, 1, 0]), "\n";
print "b ", sprintf("%d/%d", @h{qw(x y)}), "\n";
print "c ", sprintf("%s-%s", pair()), "\n";
print "d ", sprintf("%s", ctx()), "\n";
print "e ", join(",", unpack("C*", pack("CC", pair()))), "\n";
print "f ", (eval { die ctx(), "\n" } || $@);
my $w; { local $SIG{__WARN__} = sub { $w = $_[0] }; warn(ctx(), "\n"); }
print "g $w";
print "h ", sprintf("%s-%s", reverse two()), "\n";
P

# ---- 2. the leak travelled through every scalar-context PARENT -------------
both_agree($PRELUDE . <<'P', 'the enclosing scalar context no longer reaches the argument list');
print "a ", "x" . sprintf("%s-%s", pair()), "\n";
sub fmt { return sprintf("%s-%s", pair()) } print "b ", fmt(), "\n";
print "c ", (1 ? sprintf("%s-%s", pair()) : ""), "\n";
print "d ", lc(sprintf("%s-%s", pair())), "\n";
print "e ", (sprintf("%s%s", pair()) eq "12" ? "ok" : "bad"), "\n";
my $s = sprintf("%s-%s", pair()); print "f $s\n";
P

# ---- 3. a LIST SLICE's list and index list are always LIST context ---------
both_agree($PRELUDE . <<'P', 'a list slice evaluates its list and its index list in LIST context');
sub r { return (ctx(), "z")[0] } my $x = r(); print "a $x\n";
my $f = sprintf("%s,%s,%s", (localtime(0))[5, 4, 3]);
print "b ", ($f =~ /^\d+,\d+,\d+$/ ? "ok" : $f), "\n";
my $y = (ctx(), "z")[0]; print "c $y\n";
my $z = sprintf "%s-%s", (pair())[1, 0]; print "d $z\n";
print "e ", join("|", (10, 11, 12)[0, 2]), "\n";
P

# ---- 4. the `$` / `_` / `*` slots: SCALAR even in a list-context parent ----
both_agree($PRELUDE . <<'P', 'a scalar prototype slot imposes SCALAR context on its argument');
my @r = (join(ctx(), "p", "q")); print "a $r[0]\n";
print "b ", join(",", "a", scalar(ctx()), "b"), "\n";
print "c ", substr("abcdef", 1, 2), "\n";
print "d ", index("hello", "l"), "\n";
my @sp = (0,1,2,3,4); my @cut = splice(@sp, 1, 2); print "e @cut | @sp\n";
print "f ", ucfirst(reverse("abc")), "\n";
P

# ---- 5. THE ACCEPTANCE CASE: core Time::Local's month cache ----------------
#      Time::Local caches on `pack('ss', @_[4,5])`, so the slice leak made
#      timegm answer the same epoch for every month of a year.
both_agree(<<'P', 'core Time::Local: timegm answers a different epoch per month');
use Time::Local qw(timegm);
print join(" ", map { timegm(0, 0, 0, 1, $_, 2024) } 0 .. 11), "\n";
P

# ---- 6. the cases the rule must NOT break ---------------------------------
both_agree($PRELUDE . <<'P', 'the shapes the prototype rule must not break');
print "a ", (reverse("ab", "cd")), "\n";
print "b ", (eval { die bless({}, "E") } || ref($@)), "\n";
$_ = "x\n"; chomp; print "c [$_]\n";
my @p; push @p, scalar(two()); print "d $p[0]\n";
print "e "; print sort { $a <=> $b } @g; print "\n";
my @c = ("ab", "cd"); chop(@c); print "f @c\n";
sub is3 ($$;$) { my ($u, $v) = @_; "$u/$v" } print "g ", is3(ctx(), ctx()), "\n";
sub takes { scalar(@_) } print "h ", takes(two()), "\n";
print "i ", sprintf("%d%d%d", 1..3), "\n";
printf "j %s-%s\n", two();
P

# ---- transpile shapes (no SBCL): the emission the fix produces ------------
sub transpile {
    my ($code) = @_;
    return PCLCore::transpile("$pl2cl < " . write_pl($code));
}

my $cl = transpile('my @g = (5,4,3); my $a = sprintf("%02d:%02d", @g[1,0]);');
like($cl, qr/\(p-sprintf "%02d:%02d" \(p-aslice \@g 1 0\)\)/,
     'a slice in a scalar-context sprintf reaches p-sprintf whole');
unlike($cl, qr/p-list-scalar \(p-aslice/,
       'the slice is no longer collapsed to its last element');

my $cl2 = transpile('my $f = sprintf("%s,%s", (localtime(0))[5,4]);');
like($cl2, qr/\(vector 5 4\)/,
     "a list slice's INDEX list is a vector, never (progn 5 4)");

my $cl3 = transpile('my @x = (3,1,2); my $s = reverse sort @x;');
like($cl3, qr/p-reverse \(p-list-ctx \(/,
     'reverse is slurpy from argument 0: its first argument runs in list context');

# `o`/`n` observe their context, so the bind is emitted rather than elided by
# Pl::Passes' `insensitive-call` licence.
my $cl4 = transpile('my @sp=(1,2,3); sub o { wantarray ? 9 : 1 } sub n { wantarray ? 9 : 2 }'
                  . ' my @c = splice(@sp, o(), n());');
like($cl4, qr/p-splice \@sp \(p-scalar-ctx \(pl-o\)\) \(p-scalar-ctx \(pl-n\)\)/,
     "splice's OFFSET and LENGTH are scalar prototype slots");

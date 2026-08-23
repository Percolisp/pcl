#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# prefix-incr-deref-01.t — task #463 item 2: prefix `++` / `--` applied to a
# SCALAR DEREFERENCE (`++$$r`, `++${$n}`, `++${"name"}`).
#
# The whole statement was DROPPED.  Cause, measured s441a: `op_info` calls
# every PPI Cast a right-associative one-operand operator of precedence 90,
# and `++`/`--` are the ONLY prefix operators above that (92) — so the
# operator loop selected `++` FIRST and then took its operand as the single
# next token, which is the bare `$` cast.  `parse([Cast])` has no case for a
# lone cast, the term walker declined, and the statement went.  Every other
# prefix operator (`!` `~` `\` unary `-`, the filetests) sits AT 90 or below,
# where the tie resolves rightmost and the cast reduces first — which is why
# `\$$r` and `!$$r` always worked and only the increments did not.
#
# The fix is the mechanism that already exists for exactly this ("adjacent
# prefix operators must reduce INNER-first, regardless of their precedence
# numbers", PExpr.pm): one predicate, `_is_prefix_op_token`, now answers YES
# for a Cast token.  `\` — a Cast token too — had always answered yes through
# the prefix table; its siblings had not.
#
# Two RUNTIME defects came with it, both unreachable until the statement
# parsed at all:
#   * `p-pre--`'s setf-able-place arm was `(decf place)`, i.e. `(- place 1)`,
#     and p-cast-$ hands back the referent BOX for a hard ref — `--$$r` died
#     "#S(p-box …) is not of type number".  It now numifies like p-pre++.
#   * `%p-accessor-place-p` (the compound-assign store-back test) did not list
#     `p-cast-$`, so `${"name"} += 2` read the symbolic ref's VALUE and then
#     BOX-SET a number, which silently no-ops.  `$$r += 2` worked by accident
#     (p-cast-$ returns the referent box for a HARD ref).  The two lists —
#     this one and the ++/-- macros' — answer the same question and had
#     drifted apart.
#
# Rows behind it: t/op/universal.t 61→79 C_ok, t/uni/method.t 2 drops → 0.
#
# Rows 10-11 are task #463 ITEM 1, the other half of the same task and the
# same PPI mis-lex family: `$${EXPR}` is `${ ${EXPR} }`, but PPI hands over the
# PID Magic plus a SUBSCRIPT.  The Magic→two-casts pre-pass (#305) had been
# repairing the token since s390 and leaving the braces a Subscript, which is a
# hash-element access with no base — declined, statement dropped.  The pre-pass
# now re-classes those braces to the deref BLOCK `${EXPR}` arrives as.
# docs/ppi-upstream-bugs.md §1b has the repro; adjacency is perl's own rule
# (`$$ {$r}` with a space is an element of `%$`, probed).

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

plan tests => 12;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    binmode($fh, ':raw');
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub emitted { return PCLCore::transpile("$pl2cl " . write_pl($_[0])) }

sub run_cl {
    my ($code) = @_;
    my $cl_code = emitted($code);
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    binmode($cl_fh, ':raw');
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub run_perl { my $f = write_pl($_[0]); return scalar `perl $f 2>&1` }

# ── 1. The census shape, and every spelling of the increment family ─────────
{
    my $prog = <<'PL';
no strict 'refs';
++${"23::foo"};
print "1 ", ${"23::foo"}, "\n";
my $n = "yy";
${$n} = 10;
++$$n;   print "2 ", ${"yy"}, "\n";
++${$n}; print "3 ", ${"yy"}, "\n";
--$$n;   print "4 ", ${"yy"}, "\n";
++${"main::z"}; print "5 ", $main::z, "\n";
my $v = 3; my $r = \$v;
++$$r;   print "6 $v\n";
--${$r}; print "7 $v\n";
my $rr = \$r; ++$$$rr; print "8 $v\n";
my $l = 7; ++${\ $l}; print "9 $l\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#463(2): prefix ++/-- through every scalar-deref spelling (perl oracle)');
}

# ── 2. The statement must not be a DROP any more ────────────────────────────
{
    my $cl = emitted(qq{no strict 'refs';\n++\${"23::foo"};\n});
    unlike($cl, qr/PARSE ERROR/,
           '#463(2): `++${"23::foo"};` is no longer a dropped statement');
    like($cl, qr/\(p-pre\+\+ \(p-cast-\$ "23::foo"\)\)/,
         '#463(2): it lowers to the ordinary prefix-increment of a scalar deref');
}

# ── 3. undef and the magical string increment reach the deref place too ─────
{
    my $prog = <<'PL';
no strict 'refs';
my $u; my $ru = \$u;  ++$$ru;  print "1 $$ru\n";
my $w; my $rw = \$w;  --$$rw;  print "2 $$rw\n";
++${"nu1"}; print "3 ", ${"nu1"}, "\n";
--${"nu2"}; print "4 ", ${"nu2"}, "\n";
${"sy"} = "Az"; ++${"sy"}; print "5 ", ${"sy"}, "\n";
my $s = "Az"; my $sr = \$s; ++$$sr; print "6 $s\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#463(2): undef and magical-string increment through a deref (perl oracle)');
}

# ── 4. The COMPOUND-assignment half: a symbolic ref is a place ──────────────
# `${"x"} += 2` read the value and BOX-SET a number, i.e. did nothing at all.
{
    my $prog = <<'PL';
no strict 'refs';
${"x"} = 5;
${"x"} += 2;  print "1 ", ${"x"}, "\n";
${"x"} -= 1;  print "2 ", ${"x"}, "\n";
${"x"} *= 3;  print "3 ", ${"x"}, "\n";
${"x"} .= "z"; print "4 ", ${"x"}, "\n";
my $v = 5; my $r = \$v;
$$r += 2;  print "5 $v\n";
$$r .= "z"; print "6 $v\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#463(2): compound assignment through a symbolic and a hard scalar ref');
}

# ── 5. Prefix ++ in a VALUE position, not only as a whole statement ─────────
{
    my $prog = <<'PL';
no strict 'refs';
sub f { return "[@_]" }
${"s1"} = 1;
print "1 ", ++${"s1"}, "\n";
print "2 ", f(++${"s1"}), "\n";
my @l = (++${"s1"}, 9);          print "3 @l\n";
my $c = ++${"s1"} + 1;           print "4 $c\n";
if (++${"s1"} > 4) { print "5 yes\n" } else { print "5 no\n" }
my $p = ${"s1"}++;               print "6 $p ", ${"s1"}, "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#463(2): prefix ++ on a deref in argument, list, operand and condition position');
}

# ── 6. INVERSE: `$$` is still the PID, not two casts ────────────────────────
{
    my $prog = <<'PL';
print "1 ", ($$ > 0 ? "ok" : "bad"), "\n";
print "2 ", (("$$" =~ /^\d+$/) ? "ok" : "bad"), "\n";
my @p = ($$, $$); print "3 ", ($p[0] == $p[1] ? "ok" : "bad"), "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#463(2) inverse: a bare `$$` is still the process id');
}

# ── 7. INVERSE: the element and slice forms of ++/-- are untouched ──────────
{
    my $prog = <<'PL';
my %h = (a => 1); ++$h{a};   print "1 $h{a}\n";
my @a = (5);      ++$a[0];   print "2 $a[0]\n";
my $ar = \@a;     ++$$ar[0]; print "3 $a[0]\n";
my $hr = \%h;     ++$$hr{a}; print "4 $h{a}\n";
                  --${$ar}[0]; print "5 $a[0]\n";
                  --${$hr}{a}; print "6 $h{a}\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#463(2) inverse: ++/-- on array/hash elements, direct and through a ref');
}

# ── 8. INVERSE: the other prefix operators over a cast run ──────────────────
# These are the shapes that ALREADY worked because their precedence ties with
# the cast's; the widened predicate must not move them.
{
    my $prog = <<'PL';
my $v = 3; my $r = \$v; my @a = (1,2); my $ar = \@a;
my $q;
$q = \$$r;        print "1 ", $$q, "\n";
$q = !$$r;        print "2 ", ($q ? "t" : "f"), "\n";
$q = ~$$r;        print "3 ", ($q > 0 ? "big" : "small"), "\n";
$q = -$$r;        print "4 $q\n";
$q = defined $$r; print "5 ", ($q ? "d" : "u"), "\n";
print "6 ", scalar(@$ar), scalar(@{$ar}), "\n";
my $rr = \$r;     print "7 ", $$$rr, "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#463(2) inverse: \\ ! ~ unary-minus and defined over a cast run');
}

# ── 9. INVERSE: a caret-named variable is one Magic token, not Cast+Block ───
# `++${^MPE}` (t/op/magic.t) never reached the broken site and must not start.
{
    my $cl = emitted("no warnings;\nmy \$x = ++\${^MPE};\nprint \"v=\$x\\n\";\n");
    unlike($cl, qr/PARSE ERROR/,
           '#463(2) inverse: `++${^CARET}` still lowers without a drop');
}

# ── 10. #463 item 1: `$${EXPR}` is `${ ${EXPR} }`, not a subscript ──────────
# PPI lexes the `$$` as the PID Magic (bug 1, docs/ppi-upstream-bugs.md §1)
# AND structures the braces as a Subscript — so repairing only the Magic left
# `Cast Cast Subscript`, a hash-element access with no base, which the term
# walker declined: the statement was dropped.  t/op/gv.t:911-912 and
# t/uni/gv.t:805-806 are a tie class whose FETCH/STORE are exactly this.
{
    my $prog = <<'PL';
no strict 'refs'; no warnings;
my $x = 5; my $r = \$x; my $rr = \$r;
sub g { $${$_[0]} }
print "1 ", g($rr), "\n";
print "2 ", $${$rr}, "\n";
print "3 ", $$$rr, "\n";
print "4 ", ${$$rr}, "\n";
my @rs = ($rr); print "5 ", $${$rs[0]}, "\n";
my %hh = (a => $rr); print "6 ", $${$hh{a}}, "\n";
$${$rr} = 11;    print "7 $x\n";
my %h = (k => 7); my $hr = \%h; my $rh = \$hr;
print "8 ", $${$rh}{k}, "\n";
print "9 ", scalar(keys %{$$rh}), "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#463(1): `$${EXPR}` reads, writes and carries a subscript (perl oracle)');
}

# ── 11. INVERSE for item 1: `$$` is the PID and `$$x{k}` is a hash element ──
# The repair fires only on SOURCE ADJACENCY, which is perl's own rule: `$$ {…}`
# with a space is an element of `%$`, not a double deref (probed on 5.40.3).
{
    my $prog = <<'PL';
my %h = (k => 7); my $hr = \%h;
my @a = (3,4);    my $ar = \@a;
print "1 ", $$hr{k}, "\n";
print "2 ", $$ar[1], "\n";
print "3 ", $$hr{k} + $$ar[0], "\n";
my $h2 = { z => 8 }; print "4 ", $$h2{z}, "\n";
print "5 ", (("$$" =~ /^\d+$/) ? "pid" : "notpid"), "\n";
print "6 ", (($$ == $$) ? "eq" : "ne"), "\n";
my @p = ($$, $$); print "7 ", ($p[0] == $p[1] ? "same" : "differ"), "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#463(1) inverse: a bare `$$` is the PID and `$$x{k}` is still an element');
}

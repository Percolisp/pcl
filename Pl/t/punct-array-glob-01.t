#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# punct-array-glob-01.t — task #415, the two odd singles of the s419 flip-gate
# census that were measured before they were fixed.
#
# 1. PUNCTUATION-NAMED ARRAYS.  perl lets any punctuation character name a
#    global and real code writes them (`ok( ! @?, … )`, t/re/subst.t:346).  PPI
#    has Magic tokens only for the arrays perl documents, so `@?` came out as
#    Cast('@') + Operator('?') and the statement was DROPPED
#    (docs/ppi-upstream-bugs.md §24).  Merging the tokens is one half; the
#    other is that `@?` must be FORWARD-DECLARED like its sibling `@#` — and
#    that half was already broken without any PPI bug: `$?[1]` is element 1 of
#    `@?`, PCL lowered it to `(p-aref @? 1)` through the very machinery that
#    serves `$#[0]`, and only `@#` was ever declared, so a file containing
#    `$?[…]` died at LOAD with an unbound variable.
#
# 2. `<~>`.  perlop states the readline rule as a whitelist — "If what's within
#    the angle brackets is neither a filehandle nor a simple scalar variable
#    containing a filehandle name, typeglob, or typeglob reference, it is
#    interpreted as a filename pattern to be globbed" — and PCL had the
#    inverse, a blacklist of glob metacharacters.  So `<~>` was a readline on a
#    filehandle named `~` and emitted the unbound CL symbol `~`, which killed
#    the whole file; so did `</etc/hostname>` and every other metacharacter-free
#    pattern.  The glob side then has to expand the leading tilde, which is
#    bsd_glob's job and perl's answer for `<~>` (t/op/glob.t:110 `ok <~>`).

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

plan tests => 17;

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

# ── 1. `@?` and its ten siblings, against the perl oracle ───────────────────
# The census row's own shape (`! @?` on the empty array) plus a write, a read,
# an element and an interpolation.  Every character here is a CL symbol
# CONSTITUENT — that is the boundary of the repair, not perl's, and the rest
# (`@,` `@;` `@|` `@'` `@"` `@(`) still drop loudly (task #449).
{
    my $prog = <<'PL';
no warnings; no strict;
print "empty:", (! @? ? 1 : 0), ":", scalar(@?), "\n";
@? = (1, 2, 3);
@! = (4); @. = (5); @/ = (6); @~ = (7); @^ = (8);
@& = (9); @% = (10); @= = (11); @< = (12); @> = (13);
print "n:", scalar(@?), " e:", $?[1], "\n";
print "all:", scalar(@!), scalar(@.), scalar(@/), scalar(@~), scalar(@^),
              scalar(@&), scalar(@%), scalar(@=), scalar(@<), scalar(@>), "\n";
my @c = @?;
print "copy:", scalar(@c), "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#415: `@?` and its punctuation siblings — read, write, element, copy (perl oracle)');
}

# The LATENT half, which needs no `@?` in the source at all: `$?[1]` is an
# element of `@?`, and the file used to die at load for want of its defvar.
{
    my $prog = <<'PL';
no warnings; no strict;
print "A\n";
print "e:", $?[1], "\n";
print "B\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#415: `$?[1]` alone — the forward declaration the `@#` sibling always had');
}

# INVERSE GUARD: the `@` CAST must keep being a cast.  A deref, a block deref,
# an anonymous-array deref, a ternary after an array, and the modulus operator
# all sit one character away from the repair.
{
    my $prog = <<'PL';
no warnings; no strict;
my @a = (1, 2, 3);
my $r = \@a;
print scalar(@$r), scalar(@{$r}), join("", @{[ 4, 5 ]}), "\n";
print( (@a ? "y" : "n"), (@$r ? "y" : "n"), 7 % 3, "\n");
PL
    is(run_cl($prog), run_perl($prog),
       '#415 inverse: `@$r`, `@{$r}`, `@{[…]}`, a ternary and `%` are untouched');
}

# ── 2. `<~>` — and the rule change under it ────────────────────────────────
{
    my $prog = <<'PL';
print "A\n";
my @g = <~>;
print "n:", (scalar(@g) > 0 ? 1 : 0), "\n";
print "B\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#415: `<~>` globs the home directory instead of reading a handle named `~`');
}

# bsd_glob's tilde rules, which perl applies to glob() as well: `~`, `~/path`
# and `~user` expand; an unknown user and a non-leading tilde do not.
{
    my $prog = <<'PL';
for my $p ('~', '~/.bashrc', '~root') {
    my @g = glob($p);
    print "$p=", scalar(@g), ":", join(",", @g), "\n";
}
PL
    is(run_cl($prog), run_perl($prog),
       '#415: glob() expands ~, ~/path and ~user (perl oracle)');
}

# INVERSE GUARD: every readline spelling must stay a readline.  A bareword
# handle, a lexical handle, a while loop and `<DATA>` are the four the rule
# change could have turned into globs.
{
    my $prog = <<'PL';
use strict; use warnings;
my $tmp = "/tmp/pcl-punct-array-glob-01-$$.txt";
open(my $out, '>', $tmp) or die; print $out "l1\nl2\nl3\n"; close $out;
open(my $fh, '<', $tmp) or die;
my $first = <$fh>; chomp $first;
my @rest = <$fh>;
close $fh;
open(FH, '<', $tmp) or die; my $bare = <FH>; chomp $bare; close FH;
open(my $g, '<', $tmp) or die;
my $n = 0; while (my $line = <$g>) { $n++ } close $g;
my @d = <DATA>;
unlink $tmp;
print "$first/", scalar(@rest), "/$bare/$n/", scalar(@d), "\n";
__DATA__
d1
d2
PL
    is(run_cl($prog), run_perl($prog),
       '#415 inverse: <$fh>, <FH>, a while loop and <DATA> are still readline');
}

# A metacharacter-free pattern that is not an identifier IS a glob in perl —
# the half of the rule change that has nothing to do with `~`.
{
    my $prog = <<'PL';
my @g = </etc/hostname>;
print scalar(@g), ":@g\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#415: `</etc/hostname>` is a filename pattern, not a handle named /etc/hostname');
}

# The census row's exact spelling, which needs a SECOND thing: after a
# list-operator Word, PPI lexes `<~>` as three operators (§14 of
# docs/ppi-upstream-bugs.md), and `_fix_ppi_glob_after_block`'s rebuild test
# did not count `~` as a glob metacharacter.  It is one — bsd_glob expands a
# leading tilde — and it is less ambiguous than the `*` already in that class,
# because a `~` can only be bitwise-not where a term is expected.
{
    my $prog = <<'PL';
sub ok { print "ok:$_[1]\n" if $_[0] }
ok <~>, '~ works';
PL
    is(run_cl($prog), run_perl($prog),
       '#415: `ok <~>, ...` — the t/op/glob.t:110 spelling (perl oracle)');
}

# INVERSE GUARD for that widening: a `~` after a value is bitwise-not, and the
# rebuild must not fire.  `$a < ~$b` is the shape one character away from it.
{
    my $prog = <<'PL';
use strict; use warnings;
my $a = 5; my $b = 3;
print "1:", ($a < ~$b ? "y" : "n"), "\n";
print "2:", (~0 & 0xff), "\n";
sub f { 7 }
print "3:", (f() < 9 ? "lt" : "ge"), "\n";
print "4:", ($a <=> $b), "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#415 inverse: `$a < ~$b`, `~0`, `f() < 9` and `<=>` are untouched');
}

# `<<>>` — perl 5.22's DOUBLE DIAMOND, which PPI hands over as one Readline
# token whose inner text is `<>`.  It is a readline, and it is the case the
# whitelist would silently get wrong: the old blacklist rule emitted an unbound
# CL symbol `<>` (t/io/argv.t's own failure note, and a CRASH), while a
# whitelist without this case would have globbed the string "<>" and returned
# nothing at all.  Found by the companion A/B, not by the probe table.
like(emitted(q{while (<<>>) { print }}), qr/\(p-readline\)/,
     '#415: `<<>>` is the ARGV readline, exactly like `<>`');

is(emitted(q{while (<<>>) { print }}), emitted(q{while (<>) { print }}),
   '#415: `<<>>` and `<>` emit the same form');

# A handle NAME is a perl IDENTIFIER, and under `use utf8` that means unicode
# word characters.  An ASCII-only whitelist reads `<ＦＨ>` as a filename
# pattern and silently globs it — the case Pl/t/utf8-source-01.t's #418
# bareword-filehandle row caught when the two branches met.  The pair here is
# the discriminator: the same non-ASCII letters ARE a handle when they form an
# identifier and ARE a pattern when they do not.
{
    my $src = "use utf8;\nmy \$l = <\x{ff26}\x{ff28}>;\nmy \@g = <\x{ff26}.txt>;\n";
    utf8::encode($src);
    my $cl = emitted($src);
    # the emitted CL is UTF-8 BYTES, so the patterns are built from encoded
    # text rather than written as \x{} character escapes.
    my ($fh_name, $f_letter) = ("\x{ff26}\x{ff28}", "\x{ff26}");
    utf8::encode($fh_name); utf8::encode($f_letter);
    like($cl, qr/\(p-readline '\|\Q$fh_name\E\|\)/,
         '#415: a NON-ASCII bareword filehandle is still a readline');
    like($cl, qr/\(p-glob "\Q$f_letter\E\.txt"\)/,
         '#415: the same letters with a dot are a filename pattern');
}

# Task #452 (s438g): a PACKAGE-QUALIFIED bareword handle.  `_bareword_fh_p`
# tested a plain identifier, so `main::FH2` failed it and `<main::FH2>` emitted
# `(p-readline main::FH2)` — a BARE CL symbol — which died at LOAD with "The
# variable FH2 is unbound" and took the whole file with it.  The same predicate
# serves the print/say/printf `:fh` marker, so `print main::FH5 "x"` died the
# same way; only `readline(main::FH3)`, which goes through the BUILTIN path and
# quotes the name itself, was right.  One spelling of one thing, answered three
# ways.  perl-tests/method.t:672 (`while (<Colour::H1>)`) is the live case.
{
    # The `open` matters: PCL decides `print NAME LIST` is a filehandle print
    # from what the file has DECLARED as a handle, so without it the same text
    # is a call to a sub named main::FH5 — which is perl's reading too.
    my $cl = emitted('open(main::FH5, ">", "/dev/null");'
                   . ' my $l = <main::FH2>; print main::FH5 "x";');
    like($cl, qr/\(p-readline 'main::FH2\)/,
         '#452: a qualified handle in <> is QUOTED, like the unqualified one');
    like($cl, qr/:fh 'main::FH5/,
         '#452: ... and in the print filehandle slot, from the same predicate');
}

# The inverses: a LEXICAL handle is a form to evaluate, not a name to quote,
# and an unqualified bareword is unchanged.
{
    my $cl = emitted(q{open(my $fh, "<", "/etc/hostname"); my $a = <$fh>; my $b = <FH6>;});
    like($cl, qr/\(p-readline \$fh\)/,
         '#452 inverse: a lexical handle is passed through, never quoted');
    like($cl, qr/\(p-readline 'FH6\)/,
         '#452 inverse: an unqualified bareword handle is unchanged');
}

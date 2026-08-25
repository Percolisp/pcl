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

plan tests => 34;

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

# This row compares two WHOLE emissions, which means it also compares the
# preamble — and since s446i (task #512) the preamble carries `$0`, the script
# pl2cl was given, so two calls to emitted() differ by their two temp-file
# names.  Strip exactly that one line from both sides: the claim is about the
# FORM the two spellings produce, and the program name is the harness's, not
# the spelling's.
sub emitted_no_argv0 {
    my $cl = emitted($_[0]);
    $cl =~ s/^\(pcl::box-set pcl::\$0 "[^"]*"\)\n//m;
    return $cl;
}
is(emitted_no_argv0(q{while (<<>>) { print }}),
   emitted_no_argv0(q{while (<>) { print }}),
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
    #
    # #491 STRENGTHENED THESE TWO ROWS, and they used to read 'main::FH2 /
    # 'main::FH5: #452 made the qualified spelling QUOTED like the unqualified
    # one, and #491 makes it the SAME TOKEN — `main::FH2` IS `FH2` in package
    # main, so the registry keys on one name and every consumer emits one
    # symbol.  Two symbols that only found each other through
    # `%p-resolve-fh`'s by-name fallback are now one, which is what makes the
    # `open(main::FH)` / `print FH` pair below meet at all.
    my $cl = emitted('open(main::FH5, ">", "/dev/null");'
                   . ' my $l = <main::FH2>; print main::FH5 "x";');
    like($cl, qr/\(p-readline 'FH2\)/,
         '#491: `main::X` canonicalises to `X` in <>, one token for one handle');
    like($cl, qr/:fh 'FH5/,
         '#491: ... and in the print filehandle slot, from the same seam');
    unlike($cl, qr/main::FH/,
           '#491: ... and the qualified spelling is gone from the emission');
}

# Task #491 (s443f): the three spellings #452 left.  The handle NAME was never
# canonicalised, so one handle had as many identities as it had spellings:
# `print main::STDOUT "a"` was a CALL to an undefined sub, `open(Foo::H1,…)`
# emitted a BARE `Foo::H1` that killed the file at READ ("Package Foo does not
# exist"), and `open(main::FH)` registered "main::FH" while `print FH` asked
# about "FH".  perl's rule, probed: the standard handles are forced into
# `main::` from every package, every other bareword handle is qualified with
# the CURRENT package, and a qualifier naming main (or the package you are in)
# is the same glob as the bare name.
{
    my $prog = <<'PL';
print main::STDOUT "a\n";
printf main::STDOUT "%s\n", "b";
PL
    is(run_cl($prog), run_perl($prog),
       '#491: `print`/`printf main::STDOUT` write to STDOUT (perl oracle)');
}
{
    # A handle in a package that does not exist as a package: perl's stash
    # autovivifies, so `Foo::H1` is simply a handle.  PCL emits it as a QUOTED
    # symbol and registers the CL package, because an unquoted `Foo::H1` is a
    # READ error that loses the whole file.
    my $prog = <<'PL';
my $f = "/tmp/pcl-agentF-h1-$$.txt";
open(Foo::H1, ">", $f) or die "open: $!";
print Foo::H1 "hello\n";
close(Foo::H1);
open(Foo::H1, "<", $f) or die;
my $l = <Foo::H1>;
close(Foo::H1);
unlink $f;
print "got:$l";
PL
    is(run_cl($prog), run_perl($prog),
       '#491: `open(Foo::H1)` with no `package Foo` round-trips (perl oracle)');
}
{
    # The registry used to key on the SPELLING, so these two never met.
    my $prog = <<'PL';
my $f = "/tmp/pcl-agentF-h3-$$.txt";
open(main::FH, ">", $f) or die;
print FH "x\n";
close(FH);
open(FH, "<", $f) or die;
my $l = <main::FH>;
close(main::FH);
unlink $f;
print "got:$l";
PL
    is(run_cl($prog), run_perl($prog),
       '#491: `open(main::FH)` and `print FH` are one handle (perl oracle)');
}
{
    # The DIRHANDLE family reaches the same seam: `opendir(main::D)` used to
    # emit the STRING "main::D" while `readdir(main::D)` emitted a SYMBOL, so
    # readdir silently returned nothing.
    my $prog = <<'PL';
opendir(main::D, "/etc") or die "opendir: $!";
my @e = grep { $_ eq "hostname" } readdir(main::D);
closedir(D);
print "n:", scalar(@e), "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#491: `opendir(main::D)` / `readdir(main::D)` / `closedir(D)` agree');
}
# THE INVERSE, and it is the half that says the rule is perl's and not a
# convenience: only the EIGHT forced-main handles collapse across packages,
# and an explicit qualifier still names THAT package's glob.  `Foo::STDOUT` is
# an unopened handle — perl writes nothing and returns undef — so PCL must not
# let it reach main's STDOUT through the runtime's by-name fallback.
{
    my $prog = <<'PL';
my $r = print Foo::STDOUT "x\n";
print STDOUT "r=", (defined $r ? $r : 'undef'), "\n";
package Foo;
my $r2 = print Foo::STDOUT "y\n";
print STDOUT "r2=", (defined $r2 ? $r2 : 'undef'), "\n";
print STDOUT "plain=", (print STDOUT ""), "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#491 inverse: `Foo::STDOUT` is NOT STDOUT, in main or in package Foo');
}
{
    # And the lower-case negative is perl's own: a qualified name that is not
    # handle-SHAPED stays a call, so `print main::f "a"` calls f.
    my $prog = <<'PL';
sub main::f { return "f(" . join(",", @_) . ")" }
print main::f "a";
print "\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#491 inverse: `print main::f LIST` with a declared `f` is a CALL');
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

# Task #451 (s438h): the INTERPOLATION twin of s427's punctuation-array work.
# perl's rule — a `[` after a punctuation name subscripts the punctuation
# ARRAY — is the one that makes `$-[0]` and `$+[0]` elements of @-/@+, and it
# is not special to those two.  Pl::InterpScan's `$` arm continued into a
# subscript chain for `+` and `-` only, so `"$?[1]"` left the subscript
# LITERAL and printed `0[1]` — silent, with the scalar `$?` in front of it.
#
# THE SET WAS PROBED ONE CHARACTER AT A TIME against perl 5.40.3, and it is
# NOT Pl::Parser's %PUNCT_ARRAY_CHARS: `^` is in that set (because `@^` is a
# legal array and the character is CL-safe) but `"$^[1]"` does NOT subscript
# in perl — `$^` is the format-top-of-page name.  Two sets, two questions.
{
    my $out = run_cl(join "\n",
        '@? = (11,22,33); @. = (1,2,3); @/ = (4,5,6);',
        'print "A:$?[1] B:$.[2] C:$/[0]\n";',
        '"abc" =~ /b/; print "D:$-[0]:$+[0]\n";',
        '@x = (); print "E:[$?[9]]\n";',
        '');
    is($out, "A:22 B:3 C:4\nD:1:2\nE:[]\n",
       '#451: a punctuation ARRAY element interpolates (and @-/@+ still do)');
}

# The inverses.  A SPACE before the bracket ends the reference, so the scalar
# is interpolated and `[1]` stays literal — perl prints "0 [1]" for `"$? [1]"`
# with $? unset.  And `$^` keeps its old reading.
{
    my $out = run_cl(qq{\@? = (11,22,33);\nprint "F:\$? [1]\\n";\n});
    is($out, "F:0 [1]\n",
       '#451 inverse: a space ends the reference; the subscript stays literal');
}

# Task #498 (s440, found by the SHAPES corpus): perl forces every punctuation
# name into package main, so `@?` written in package A IS `@?` read in package
# B.  PCL forward-declared the #415 arrays per PACKAGE (`(in-package :A)
# (defvar @? …)` / `(in-package :B) (defvar @? …)` = two symbols); they are
# runtime-owned and exported now, like `@-`/`@+`.  Block and statement forms,
# all eleven siblings + the element spelling; the perl oracle is the
# expectation.  INVERSE: a NAMED array in two packages stays two arrays.
{
    my $prog = <<'PL';
no strict; no warnings;
{ package A; @? = (1, 2, 3); @! = (4); @. = (5); @/ = (6); @~ = (7); @^ = (8);
             @& = (9); @% = (10); @= = (11); @< = (12); @> = (13); }
{ package B; print "e:$?[1] n:", scalar(@?), " sibs:", scalar(@!), scalar(@.), scalar(@/),
             scalar(@~), scalar(@^), scalar(@&), scalar(@%), scalar(@=), scalar(@<), scalar(@>), "\n"; }
PL
    is(run_cl($prog), run_perl($prog),
       '#498: a punctuation array written in package A is read in package B (block form)');
}
{
    my $prog = <<'PL';
no strict; no warnings;
package A; @? = (1, 2, 3);
package B; print "e:$?[1] n:", scalar(@?), "\n"; $?[0] = 9;
package main; print "m:@?\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#498: ... and in the statement form, through three packages');
}
{
    my $prog = <<'PL';
no strict; no warnings;
{ package A; @x = (1, 2, 3); }
{ package B; print "B:", scalar(@x), " A:", scalar(@A::x), "\n"; }
PL
    is(run_cl($prog), run_perl($prog),
       '#498 inverse: a NAMED array in two packages stays two arrays');
}

# Task #450 (s438i): perl's glob returns a pattern with no METACHARACTER as
# ITSELF, whether or not anything of that name exists — `glob("/nope-xyz")` is
# "/nope-xyz", `glob("/home/")` is "/home/", `glob("~/")` is the expanded home.
# PCL went to the filesystem unconditionally and answered EMPTY for all three.
# The same fix gives PCL perl's WORD model: a glob pattern is a
# whitespace-separated LIST of patterns, so `glob("a b")` is two results.
{
    my $out = run_cl(<<'PL');
for my $p ('/nope-xyz', '/home/', 'x~y', '~nosuchuser42', 'aa bb') {
    my @g = glob($p);
    print scalar(@g), ":", join("|", @g), "\n";
}
PL
    is($out, "1:/nope-xyz\n1:/home/\n1:x~y\n1:~nosuchuser42\n2:aa|bb\n",
       '#450: a metacharacter-free pattern is itself, and words split');
}

# The inverses: a real wildcard still reaches the filesystem, and an all-blank
# pattern has no words at all (perl gives an empty list for both `""` and
# `"   "`).
{
    my $out = run_cl(<<'PL');
my @c = glob("/etc/host*");
print((grep { $_ eq "/etc/hostname" } @c) ? "found\n" : "missing\n");
print scalar(glob("")), ":", scalar(my @e = glob("   ")), "\n";
PL
    is($out, "found\n0:0\n",
       '#450 inverse: a wildcard still globs; a blank pattern has no words');
}

# ── Task #506 (s446j): the punctuation HASHES, and the REST of the arrays ───
# `$$ {EXPR}` — the PID magic, a SPACE, then braces — is an element of the
# hash `%$` (perl's own adjacency rule: without the space it is the double
# deref `${${EXPR}}`, probed).  PCL emitted a bare `%$` that nothing declared,
# so the file died at LOAD before line 1 ran.  Measured char by char on
# 5.40.3: perl accepts a punctuation name for EVERY character here, and PCL
# was unbound for 25 of the hashes and 15 more of the arrays — the twelve of
# #415 were the set one repair happened to cover, not the set perl allows.
# Only a READ shows it: a write auto-vivifies through p-setf.
{
    my $prog = <<'PL';
no strict; no warnings;
my $x = 5; my $r = \$x; my $rr = \$r;
print "1:[", (defined($$ {$rr}) ? $$ {$rr} : ""), "]\n";
$$ {"k"} = "V";
print "2:[", $$ {"k"}, "] exists:", (exists $$ {"k"} ? 1 : 0), "\n";
print "3:[", (defined($$ [0]) ? $$ [0] : ""), "]\n";
$$ [1] = "A1";
print "4:[", $$ [1], "]\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#506: `$$ {EXPR}` is an element of %$ (and `$$ [0]` of @$), read-first');
}

# perlvar: a punctuation name is forced into package main, so a write in one
# package and a read in another meet — the #498 rule, now for the containers
# that were never declared at all.
{
    my $prog = <<'PL';
no strict; no warnings;
{ package P1; $, {"x"} = "one"; $; [0] = "two"; }
{ package P2; print "cross:", $, {"x"}, ",", $; [0], "\n"; }
PL
    is(run_cl($prog), run_perl($prog),
       '#506: a punctuation container written in one package reads in another');
}

# The READ-FIRST spelling of the characters measured unbound, in one program:
# before the fix any ONE of these lines killed the whole file at load.
{
    my $prog = <<'PL';
no strict; no warnings;
print "start\n";
my $n = 0;
$n++ if defined $$ {"q"}; $n++ if defined $% {"q"};
$n++ if defined $& {"q"}; $n++ if defined $( {"q"};
$n++ if defined $) {"q"}; $n++ if defined $* {"q"}; $n++ if defined $, {"q"};
$n++ if defined $. {"q"}; $n++ if defined $/ {"q"}; $n++ if defined $: {"q"};
$n++ if defined $; {"q"}; $n++ if defined $< {"q"}; $n++ if defined $= {"q"};
$n++ if defined $> {"q"}; $n++ if defined $? {"q"}; $n++ if defined $@ {"q"};
$n++ if defined $[ {"q"}; $n++ if defined $] {"q"}; $n++ if defined $^ {"q"};
$n++ if defined $| {"q"}; $n++ if defined $~ {"q"};
$n++ if defined $$ [0];
$n++ if defined $( [0]; $n++ if defined $) [0]; $n++ if defined $* [0];
$n++ if defined $, [0]; $n++ if defined $: [0]; $n++ if defined $; [0];
$n++ if defined $@ [0]; $n++ if defined $[ [0]; $n++ if defined $] [0];
$n++ if defined $| [0];
print "defined:$n\nend\n";
PL
    is(run_cl($prog), run_perl($prog),
       '#506: reading any punctuation hash or array first no longer kills the file');
}

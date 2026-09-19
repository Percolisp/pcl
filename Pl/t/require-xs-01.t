#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# require-xs-01.t — AN XS MODULE WITH NO PCL BUILD IS *NOT INSTALLED* (#1917).
#
# PCL's @INC includes the host perl's site_perl, so a .pm whose body bootstraps
# a loadable object is FOUND, transpiled and run — and its XSLoader::load then
# died with perl's "Can't locate loadable object for module M in @INC".  That
# is perl's message for a BROKEN INSTALL (a .pm whose .so went missing), and
# every optional-XS wrapper on CPAN reads it as one: the common
# `_maybe_load_module` shape matches only /\ACan't locate $file / for "not
# installed" and WARNS on anything else.  So on every machine where such a
# module is installed for perl, a PCL program printed a line perl never prints.
#
# For PCL the situation is not a broken install: the module is simply not
# available here until somebody runs tools/pcl-xs-install for it.  So the
# bootstrap failure carries a MARKER CLASS, and the require frame that owns the
# file's own top-level load converts it into perl's module-NOT-FOUND die.
#
# THE FIXTURE IS A MECHANISM, NOT A MODULE NAME (CLAUDE.md 9a): every .pm here
# is written into a temp dir by this file.  Nothing under Pl/ or cl/ knows any
# CPAN name.
#
# HALF THE ROWS ARE THE OTHER DIRECTION — what the change must NOT break, with
# PERL AS THE ORACLE for each (perl on a box whose .so really is missing gives
# exactly these answers, and they are asserted against a live perl run):
#   * a DIRECT XSLoader::load outside any require keeps today's text;
#   * a module that catches its OWN bootstrap failure and falls back to pure
#     perl still LOADS, and lands in %INC;
#   * `eval { require XS; 1 } or <pure perl>` still falls back silently;
#   * a module that catches its bootstrap failure and then dies of something
#     else reports ITS OWN death — the marker must not leak past an eval.

# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use File::Path qw(make_path);
use File::Spec;
use FindBin qw($RealBin);

my $root = File::Spec->rel2abs("$RealBin/../..");
my $pcl  = "$root/pcl";

use lib "$RealBin/../../tools/lib";
use PCLSbcl ();

plan skip_all => "pcl not found"  if !-x $pcl;
plan skip_all => "sbcl not found" if !`which sbcl 2>/dev/null`;

my $core = PCLSbcl::cached_core("$root/cl/pcl-runtime.lisp");
plan skip_all => "no cached core" if !($core && -f $core);

plan tests => 16;

my $d = tempdir(CLEANUP => 1);
make_path("$d/Opt");

sub put {
    my ($rel, $body) = @_;
    open my $fh, '>', "$d/$rel" or die "write $d/$rel: $!";
    print $fh $body;
    close $fh;
}

# The XS shape: a .pm that bootstraps a loadable object nobody built.
put("Opt/XS.pm", <<'PM');
package Opt::XS;
require XSLoader;
XSLoader::load('Opt::XS', '0.01');
1;
PM
put("Opt/XS2.pm", <<'PM');
package Opt::XS2;
require XSLoader;
XSLoader::load('Opt::XS2', '0.01');
1;
PM

# The optional-XS wrapper: catches its own failure, falls back to pure perl.
put("SelfFallback.pm", <<'PM');
package SelfFallback;
our $IMPL = 'pp';
eval { require XSLoader; XSLoader::load('SelfFallback', '0.01'); $IMPL = 'xs'; 1 }
  or do { $IMPL = 'pp' };
sub impl { $IMPL }
1;
PM

# A module that requires an XS module at ITS top level.
put("Outer.pm", <<'PM');
package Outer;
require Opt::XS;
sub hi { 1 }
1;
PM

# Catches its bootstrap failure, then dies of something else entirely.
put("CatchThenDie.pm", <<'PM');
package CatchThenDie;
eval { require XSLoader; XSLoader::load('CatchThenDie', '0.01'); 1 };
die "my own death\n";
1;
PM

# ONE program, many rows: a run of the compiler is seconds, so the rows share
# one.  Each prints `KEY value`.
put("prog.pl", <<'PL');
sub first_line { my $s = shift; $s =~ s/\n.*//s; $s }

my $ok = eval { require Opt::XS; 1 };
my $e  = first_line($@);
print "A ", ($ok ? 1 : 0), "\n";
print "B ", ($e =~ m{\ACan't locate Opt/XS\.pm in \@INC } ? "yes" : "no [$e]"), "\n";
print "C ", ($e =~ /no PCL build/ ? "yes" : "no [$e]"), "\n";
print "D ", (exists $INC{'Opt/XS.pm'} ? "present" : "absent"), "\n";

my $ok2 = eval { require Opt::XS; 1 };
my $e2  = first_line($@);
print "E ", ($e2 eq $e ? "same" : "differs [$e2]"), "\n";

require XSLoader;
eval { XSLoader::load('Zzz::Direct', '0.01'); 1 };
my $ec = first_line($@);
print "F ", ($ec =~ m{\ACan't locate loadable object for module Zzz::Direct in \@INC}
             ? "loadable-object" : "other [$ec]"), "\n";

my $okd = eval { require SelfFallback; 1 };
print "G ", ($okd ? 1 : 0), " ",
      (SelfFallback->can('impl') ? SelfFallback::impl() : "none"), " ",
      (exists $INC{'SelfFallback.pm'} ? "present" : "absent"), "\n";

my $oke = eval { require Outer; 1 };
my $ee  = first_line($@);
print "H ", ($oke ? 1 : 0), " ",
      ($ee =~ m{\ACan't locate Opt/XS\.pm } ? "inner" : "other [$ee]"), " ",
      (exists $INC{'Outer.pm'} ? "present" : "absent"), "\n";

print "I ", (eval { require Opt::XS; 1 } ? "xs" : "pp"), "\n";

my $okg = eval q{ use Opt::XS2; 1 };
my $eg  = first_line($@);
print "J ", ($eg =~ m{\ACan't locate Opt/XS2\.pm in \@INC } ? "yes" : "no [$eg]"), "\n";

my $okh = eval { require CatchThenDie; 1 };
my $eh  = first_line($@);
print "K ", ($eh =~ /own death/ ? "own-die" : "other [$eh]"), "\n";
PL

# The uncaught shape, its own program: perl exits 2 (ENOENT) and prints ONE
# line for an uncaught "Can't locate".
put("uncaught.pl", "require Opt::XS;\nprint \"NOT REACHED\\n\";\n");

sub run {
    my ($cmd) = @_;
    my $out = `$cmd 2>$d/stderr`;
    my $rc  = $? >> 8;
    my $err = do { local $/; my $fh; open($fh, '<', "$d/stderr") ? <$fh> : '' };
    return ($out, (defined $err ? $err : ''), $rc);
}

local $ENV{PCL_CORE} = $core;
my ($pout, $perr, $prc) = run("$pcl -I '$d' '$d/prog.pl'");
my %r = map { /^(\w+) (.*)$/ ? ($1, $2) : () } split /\n/, $pout;

# perl's answers for the rows where PCL must AGREE (this perl has no .so for
# any of these either, which is precisely the shape being compared).
my ($lout) = run("perl -I '$d' '$d/prog.pl'");
my %l = map { /^(\w+) (.*)$/ ? ($1, $2) : () } split /\n/, $lout;

diag("pcl stderr: $perr") if length $perr;

# ── the divergence PCL is FOR ────────────────────────────────────────────
is($r{A}, '0', 'require of an XS module with no PCL build fails');
is($r{B}, 'yes',
   "the message is perl's own up to `in \@INC ' — what the ecosystem greps");
is($r{C}, 'yes', '... and says in our own parenthesis why');
is($r{D}, 'absent', 'a module presented as not installed leaves no %INC entry');
is($r{E}, 'same', 'a SECOND require answers identically (the per-process memo)');
is($r{J}, 'yes', '`use` at compile time reports the same not-found text');
is($r{H}, '0 inner absent',
   'a module requiring an XS module fails naming the INNER file (innermost frame wins)');

# ── what must NOT change, perl as the oracle ─────────────────────────────
is($r{F}, 'loadable-object',
   'a DIRECT XSLoader::load outside a require keeps the loadable-object text');
is($r{F}, $l{F}, '... which is what perl says too');
is($r{G}, '1 pp present',
   'a module that catches its OWN bootstrap failure still loads and falls back');
is($r{G}, $l{G}, '... exactly as under perl');
is($r{I}, 'pp', 'the `eval { require XS } or pure perl` idiom still falls back');
is($r{I}, $l{I}, '... exactly as under perl');
is($r{K}, 'own-die',
   "the marker does not leak past the module's own eval: its own die is reported");
is($r{K}, $l{K}, '... exactly as under perl');

# ── uncaught: perl's message once, perl's ENOENT status ──────────────────
# perl exits 2 (ENOENT) for an uncaught "Can't locate", and says it once.  The
# COUNT is what is asserted, not the line count: a module under `-I` is trust
# class :local, so PCL loads its TEXT, and SBCL's load-as-source prints its own
# "While evaluating the form starting at line N" context note for any error
# that passes through — pre-existing, unrelated to this file's subject, and
# filed as #1970.  An :installed module (the shape #1917 is about) takes the
# FASL path and has no such note.
{
    my ($out, $err, $rc) = run("$pcl -I '$d' '$d/uncaught.pl'");
    my $said = () = $err =~ m{Can't locate Opt/XS\.pm in \@INC }g;
    is("rc=$rc said=$said reached=" . ($out =~ /NOT REACHED/ ? 1 : 0),
       'rc=2 said=1 reached=0',
       "an uncaught require of it exits 2 and says perl's message once");
}

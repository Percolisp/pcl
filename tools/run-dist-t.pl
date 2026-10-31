#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# run-dist-t.pl — transpile and run a CPAN dist's own t/*.t file through PCL.
#
# Usage:
#   tools/run-dist-t.pl <dist-dir> <t-file>            # run one t-file
#   tools/run-dist-t.pl --no-dist-lib <dist-dir> <t>   # don't add <dist>/lib to @INC
#   tools/run-dist-t.pl --inc DIR <dist-dir> <t>       # also put DIR on @INC (repeatable)
#   tools/run-dist-t.pl --summary <dist-dir> <t-file>  # print only the TAP summary
#   tools/run-dist-t.pl --rows <dist-dir> <t-file>     # summary + one ROW line
#                                                      # per FAILING assertion
#   tools/run-dist-t.pl --rows --oracle DIST T         # ... and the PERL oracle's lines
#   tools/run-dist-t.pl --oracle-only DIST T           # only the oracle (cached)
#        [--refresh-oracle] [--oracle-perl PATH] [--oracle-timeout SECS]
#   (the oracle: real perl on the same t-file, from the dist root, the same
#    @INC rule and PERL5LIB -- see "THE PERL ORACLE" below; s531b)
#
# --rows is the summary plus, for each failing row,
#   ROW <TAB> num <TAB> description <TAB> got <TAB> expected <TAB> directive
# so tools/cpan-scoreboard.pl can keep a ROW-level board baseline with a cause
# column (task #1502).  Both counts and rows come from ONE parse
# (tools/lib/PCLTap.pm) — two parsers would drift (rule 11).
#
# <t-file> may be relative to <dist-dir> (e.g. t/basic.t) or an absolute path.
#
# `use Test::More` resolves to PCL's TAP layer (cl/pcl-test.lisp), loaded on
# demand by the runtime — so a dist's author tests run as TAP.
#
# @INC for the transpile: the dist's t/lib and (by default) lib are added via
# -I so the module-under-test and its test helpers resolve.
#
# CAVEAT (do NOT remove): for XS-stubbed dists (Scalar-List-Utils etc.) adding
# <dist>/lib pollutes pl2cl's OWN @INC — pl2cl `use`s Moo -> Scalar::Util, and a
# dist copy of Scalar/Util.pm shadows PCL's shim, causing a false TRANSPILE-FAIL.
# PCL uses its lib/ shims for those anyway, so pass --no-dist-lib for them.

use strict;
use warnings;
use File::Basename qw(dirname);
use Cwd qw(abs_path);
use FindBin;
use lib "$FindBin::RealBin/lib";
use PCLTap ();   # the ONE TAP reader shared with tools/cpan-scoreboard.pl
use PCLSbcl ();  # the ONE sbcl command-line builder (task #344; #2959: this runner drifted)
use PCLProc ();  # the ONE child-isolation helper: a run killed by `timeout`
                 # takes its pl2cl / SBCL with it (task #3064)
use PCLDistPrereq ();  # the dist's BUILT prerequisites' blibs (task #3520)
use Digest::MD5 ();    # the oracle cache key (s531b)
use File::Path ();
use Time::HiRes ();

my $root = abs_path(dirname(abs_path($0)) . "/..");

my ($no_dist_lib, $summary_only, $rows_out) = (0, 0, 0);
my ($oracle, $oracle_only, $refresh_oracle, $oracle_perl, $oracle_to) = (0, 0, 0, undef, 120);
my (@args, @extra_inc);
while (@ARGV) {
  $_ = shift @ARGV;
  if    ($_ eq '--no-dist-lib') { $no_dist_lib = 1 }
  elsif ($_ eq '--inc')         { push @extra_inc, shift(@ARGV) // die "--inc needs a directory\n" }
  elsif ($_ eq '--summary')     { $summary_only = 1 }
  elsif ($_ eq '--rows')        { $rows_out = 1; $summary_only = 1 }
  elsif ($_ eq '--oracle')      { $oracle = 1 }
  elsif ($_ eq '--oracle-only') { $oracle = 1; $oracle_only = 1 }
  elsif ($_ eq '--refresh-oracle') { $refresh_oracle = 1 }
  elsif ($_ eq '--oracle-perl') { $oracle_perl = shift(@ARGV) // die "--oracle-perl needs a path\n" }
  elsif ($_ eq '--oracle-timeout') { $oracle_to = shift(@ARGV) // die "--oracle-timeout needs seconds\n" }
  else                          { push @args, $_ }
}

@args == 2 or die "usage: $0 [--no-dist-lib] [--summary] <dist-dir> <t-file>\n";
my ($dist, $tfile) = @args;
$dist = abs_path($dist) or die "dist dir not found: $args[0]\n";
$tfile = "$dist/$tfile" unless $tfile =~ m{^/};
-f $tfile or die "t-file not found: $tfile\n";

# @INC additions for the transpile (-I dirs are baked into the generated @INC).
my @inc = ("$dist/t/lib");
push @inc, "$dist/lib" unless $no_dist_lib;
# --inc: a DEPENDENCY's lib/, fetched under ~/.cpan/build and never installed
# (Type-Tiny needs Exporter-Tiny, s518e).
for my $d (@extra_inc) { -d $d or die "--inc: no directory $d\n"; push @inc, abs_path($d) }
@inc = grep { -d } @inc;
my $iflags = join " ", map { "-I$_" } @inc;

# CWD = THE DIST ROOT for both children (#1575), as `prove` / `make test` and
# the board's perl oracle run a t-file: a test that reads a RELATIVE path
# (`do "./t/util.pl"`, a `corpus/` fixture) otherwise fails for a reason that
# is not PCL's.  Every path this script passes on is absolute.
chdir $dist or die "cannot chdir to $dist: $!\n";

my $lisp = "/tmp/run-dist-t_$$.lisp";
my $err  = "/tmp/run-dist-t_$$.err";

my $out  = "/tmp/run-dist-t_$$.out";

# The environment `make test` has under the CPAN shell (task #3520): a
# requirement the box perl does not satisfy, satisfied by a BUILT copy under
# ~/.cpan/build, goes on PERL5LIB -- for the RUN only (the transpile below
# bakes PERL5LIB into the program's @INC; this is for the `$^X` children a
# test spawns, which run the box perl, and for the perl ORACLE).  The header
# says what was added, and what is built nowhere (never fetched or built here).
my ($prereq, $prereq_notes) = PCLDistPrereq::built_prereq_blibs(dist => $dist);
my @prereq_dirs = PCLDistPrereq::perl5lib_dirs($prereq);
my $run_perl5lib = join ':', @prereq_dirs, grep { defined && length } $ENV{PERL5LIB};
{
  my $hdr = $summary_only || $oracle_only ? \*STDOUT : \*STDERR;
  printf $hdr "PREREQ\tadded\t%s %s (box perl %s) from %s\n", $_->{module}, $_->{built},
    $_->{installed} // 'none', $_->{dir} for @$prereq;
  print $hdr "PREREQ\tmissing\t$_\n" for @$prereq_notes;
}

# ── THE PERL ORACLE (s531b, Fable ruling s531 #4) ──────────────────────────
# The same t-file under real perl, from the dist root, with the same @INC rule
# and the run's PERL5LIB -- what `make test` would say.  Printed as
#
#   PERL    <TAB> plan <TAB> skip_all <TAB> ok <TAB> notok <TAB> rc
#   PERLROW <TAB> num <TAB> description <TAB> directive     (each perl NOT-OK row,
#                                                            its TODO rows included)
#   ORACLE  <TAB> cached|measured|timeout <TAB> seconds
#
# skip_all is `# SKIP reason` when perl printed `1..0` (reason may be empty),
# else empty.  The scoreboard's SKIP verdict needs BOTH sides to skip; its
# MISMATCH line checks a blessed PERL-SKIP / PERL-FAILS row against this.
#
# CACHED under $PCL_ORACLE_CACHE (default ~/.pcl-cache/oracle/), keyed by the
# oracle perl (its version, or a non-default binary's path + size + mtime),
# the t-file's real path + size + mtime, and the @INC / PERL5LIB the run uses
# -- so after the first board run the oracle costs nothing.  The key cannot
# see a module installed on the box (which changes perl's skips): the round
# run passes --refresh-oracle.  A timed-out oracle is never cached.
sub oracle_lines {
  my $perl = $oracle_perl // $^X;
  my $who = defined $oracle_perl
    ? do { my @s = stat($perl) or die "oracle perl not found: $perl\n";
           join(' ', abs_path($perl), $s[7], $s[9]) }
    : sprintf('perl %vd %s', $^V, $^X);
  my @ts = stat($tfile);
  my $key = Digest::MD5::md5_hex(join "\0", $who, abs_path($tfile), $ts[7], $ts[9],
                                 $iflags, $run_perl5lib);
  my $cdir = $ENV{PCL_ORACLE_CACHE} // "$ENV{HOME}/.pcl-cache/oracle";
  my $cfile = "$cdir/$key";
  if (!$refresh_oracle && open(my $c, '<', $cfile)) {
    local $/; my $body = <$c>; close $c;
    return ($body, "ORACLE\tcached\t0\n");
  }
  my $pout = "/tmp/run-dist-t_$$.perl";
  my $t0 = Time::HiRes::time();
  my ($st) = do {
    local $ENV{PERL5LIB} = $run_perl5lib if length $run_perl5lib;
    PCLProc::run_isolated("timeout -k 5 $oracle_to $perl $iflags $tfile >$pout 2>/dev/null </dev/null");
  };
  my $secs = sprintf '%.2f', Time::HiRes::time() - $t0;
  my $text = '';
  if (open(my $f, '<', $pout)) { local $/; $text = <$f> // '' }
  unlink $pout;
  my $prc = $st >> 8;
  return ('', "ORACLE\ttimeout\t$secs\n") if $prc == 124 || ($st & 127);
  my $tap = PCLTap::parse_tap($text);
  my $body = sprintf "PERL\t%s\t%s\t%d\t%d\t%d\n", $tap->{plan} // '',
    defined $tap->{skip_all} ? '# SKIP ' . PCLTap::tsv_clean($tap->{skip_all}) : '',
    $tap->{ok}, $tap->{notok}, $prc;
  $body .= join("\t", 'PERLROW', $_->{num}, $_->{desc}, $_->{directive}) . "\n" for @{ $tap->{rows} };
  if (File::Path::make_path($cdir) || -d $cdir) {
    my $tmpc = "$cfile.tmp.$$";
    if (open(my $w, '>', $tmpc)) { print $w $body; close $w; rename $tmpc, $cfile or unlink $tmpc }
  }
  return ($body, "ORACLE\tmeasured\t$secs\n");
}
if ($oracle) {
  my ($body, $how) = oracle_lines();
  print $body, $how;
  exit 0 if $oracle_only;
}

# Both children run in their OWN session (PCLProc::run_isolated): a hung SBCL
# ignores TERM, and before #3064 a `timeout N run-dist-t.pl` killed only this
# script and left the SBCL spinning, orphaned, for hours.
my ($rc) = PCLProc::run_isolated(
  "perl -I$root $iflags $root/pl2cl --no-cache --lenient-ppi $tfile >$lisp 2>$err");
if ($rc != 0) {
  my $e = do { local $/; open(my $f, '<', $err) or die; <$f> };
  print STDERR "TRANSPILE-FAIL ($tfile):\n$e";
  unlink $lisp, $err;
  exit 2;
}

# The SBCL command line comes from the ONE builder every runner shares (#2959:
# this runner spelled it by hand, in SOURCE mode, so every board file recompiled
# the whole runtime -- ~25 s a file).  PCLSbcl picks the cached core; PCL_NO_CORE=1
# gives the source-mode line back.  `*pcl-skip-cache*` is about the MODULE fasl
# cache (a dist test transpiles its modules fresh), not the runtime: the core
# changes nothing there.
my $sbcl_cmd = PCLSbcl::sbcl_prefix_str(runtime => "$root/cl/pcl-runtime.lisp", quote => 0)
  . " --eval \"(setf pcl::*pcl-skip-cache* t)\" --load $lisp";
print STDERR "SBCL[run-dist-t]: $sbcl_cmd\n" if $ENV{PCL_SHOW_SBCL};
# The run's PERL5LIB (the dist's built prerequisites, computed above).
local $ENV{PERL5LIB} = $run_perl5lib if @prereq_dirs;
my ($sbcl_status) = PCLProc::run_isolated("$sbcl_cmd >$out 2>$err");
# TAP is read from STDOUT ALONE, as `prove` reads it (#3241).  The two streams
# used to share one file, and a stderr line written without its newline (the
# runtime's "While evaluating the form ... of #P\"x.lisp\":" context, ahead of
# a caught error) glued the next TAP line onto its end -- HTTP-Tiny's
# done_testing `1..1` became `...lisp":1..1`, and the file read "no plan".
# Every TAP line, diagnostics included, is on stdout (cl/pcl-test.lisp's
# %tap-out); stderr is shown, never parsed.
my $raw = '';
if (open(my $f, '<', $out)) { local $/; $raw = <$f> // ''; }
my $stderr = '';
if (open(my $f, '<', $err)) { local $/; $stderr = <$f> // ''; }
# The child's WAIT STATUS, which this script used to throw away.  A SIGNAL
# there (the OOM killer on a loaded box) produces an empty TAP stream that is
# indistinguishable from "the file ran and printed nothing" — and the board's
# rule is "zero ok = FAIL", so a killed run is silently published as a verdict.
# Reported in --rows mode so the scoreboard can retry it instead of believing
# it (measured s473w: two Algorithm-Diff files read FAIL 0/0 in a loaded board
# run and 32/3 + 102/20 alone).
unlink $lisp, $err, $out;

# Filter SBCL noise.
$raw =~ s/^;[^\n]*\n//gm;
$raw =~ s/^PCL (?:Runtime|Test)[^\n]*\n//gm;
$raw =~ s/^STYLE-WARNING[^\n]*\n//gm;

if ($summary_only) {
  # ONE parse feeds the counts AND the rows (tools/lib/PCLTap.pm).  The counts
  # are the historical /^ok \d+/ and /^not ok \d+/ ones, so no blessed board
  # snapshot moves because the parser moved.
  my $tap = PCLTap::parse_tap($raw);
  print "pass=$tap->{ok} fail=$tap->{notok}  ($tfile)\n";
  if ($rows_out) {
    # The fourth field (#3651): 1 when the exit status is the one the TAP
    # layer's END chose from the rows (Test::Builder's: the failures, or 255
    # for a missed plan), 0 when it is unexplained -- a crash, or an exit the
    # run was already leaving with (PCLTap::exit_is_verdict).
    printf "SBCL\t%d\t%d\t%d\n", $sbcl_status >> 8, $sbcl_status & 127,
           PCLTap::exit_is_verdict($raw, $sbcl_status >> 8, $tap->{ok} + $tap->{notok},
                                   $tap->{plan}, $tap->{failed}) ? 1 : 0;
    printf "PLAN\t%s\t%s\n", $tap->{plan} // '',
           defined $tap->{skip_all}
             ? '# SKIP ' . PCLTap::tsv_clean($tap->{skip_all}) : '';
    for my $r (@{ $tap->{rows} }) {
      print join("\t", 'ROW', $r->{num}, $r->{desc}, $r->{got},
                       $r->{expected}, $r->{directive}), "\n";
    }
  }
  # Show the detail on a failure AND on a run that stops short of its plan
  # (#3282: JSON-PP's 099_binaryNN.t printed `1..1536`, ran 0 rows and its
  # only words -- the TAP layer's `# Looks like you planned 1536 tests but ran
  # 0.` -- were thrown away with the rest of the stream, so the run read as
  # silent).
  my $short = defined $tap->{plan} && $tap->{ok} + $tap->{notok} < $tap->{plan};
  print STDERR $raw, $stderr if $tap->{notok} || $short;
} else {
  print $raw;
  print STDERR $stderr;
}

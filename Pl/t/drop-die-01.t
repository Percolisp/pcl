#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# drop-die-01.t — THE FLIP (Option B phase 2's last step; ruled
# docs/fable-answers-s433.md §A.1, executed s435).
#
# A statement the compiler cannot lower used to be replaced by `nil`: the
# program ran on without it, and nothing at run time ever said so.  That is
# the #138 family — perl-tests/bless.t's drop was a TEST ROW that never ran,
# in a file the sweep reported as passing.  Now the statement is replaced by
# a `p-die` naming the file, line, source text and reason, so reaching it
# dies, perl-shaped and trappable.
#
# What each row is holding down:
#   1  the die is REACHED and TRAPPABLE — `$@` gets it, `eval` returns undef;
#   2  the statements before it ran, and the program continues after the eval
#      (the unit is the STATEMENT — that is what makes the flip affordable);
#   3  a drop the program never REACHES costs nothing at all;
#   4  the `;; PARSE ERROR: <reason>` comment survives BYTE-FOR-BYTE — the
#      census, tools/corpus-diff.pl's SILENT-DROP counter and both runners'
#      `drops` column all find drops by that exact text;
#   5  the transpile-time announcement and the emitted die name the SAME
#      statement.  They are built by ONE helper (Pl::Parser::_drop_site) for
#      exactly this reason: two spellings of the site would read as two
#      events.  This row is the guard on that seam;
#   6  the INVERSE — a program with no drop gets no die and no PARSE ERROR,
#      i.e. the flip is confined to the drop site;
#   7  a dropped statement containing a TILDE is still trappable.  This flip is
#      the first emitter to feed arbitrary user SOURCE TEXT to p-die, whose
#      no-location branch was `(error msg)` — i.e. the message was a CL FORMAT
#      CONTROL string.  `f() = ($x =~ /b/)` fed `~ ` to the format engine and
#      raised an untrappable sb-format:format-error that killed the whole file
#      instead of setting $@ (found in the s435 Fable review; the branch is
#      `(error "~A" msg)` now).  `=~` is common in the census families, so this
#      row guards the flip's central promise for the shapes most likely to hit
#      it.
#
# The shape under test is the lvalue-sub refusal (`f() = 7`), which stays a
# drop by design — docs/not-supported.md §Lvalue subroutines.  perl refuses
# the whole FILE at compile time; PCL is finer-grained, and that divergence
# is documented there, not asserted here.

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

plan tests => 10;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# transpile_raw, not transpile: these rows MEAN to read the drop announcement,
# which PCLCore::transpile fails a row for (and rightly, everywhere else).
sub transpile_dropping {
    my ($code) = @_;
    my ($cl, $err, $rc) = PCLCore::transpile_raw("$pl2cl " . write_pl($code));
    die "pl2cl exit " . ($rc >> 8) . ": $err" if $rc != 0;
    return ($cl, $err);
}

sub run_cl {
    my ($cl_code) = @_;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# ── The dropping program.  Line 3 is the drop; the prints on either side are
# rows 2's evidence. ─────────────────────────────────────────────────────────
my $DROPPING = <<'PL';
sub f { 42 }
print "before\n";
my $ok = eval { f() = 7; 1 };
print "returned=", (defined $ok ? "value" : "undef"), "\n";
print "err=$@";
print "after\n";
PL

my ($cl, $err) = transpile_dropping($DROPPING);
my $out = run_cl($cl);

# 1 ── the die is reached and trappable.
like($out, qr/^err=PCL: statement not supported at \S+ line 3: f\(\) = 7; -- PCL: Can't modify non-lvalue subroutine call in assignment$/m,
     'the dropped statement DIES when reached, and $@ carries file, line, source text and reason');
like($out, qr/^returned=undef$/m,
     'the eval returns undef — the drop is a real die, not a value');

# 2 ── the unit is the statement: everything around it still runs.
like($out, qr/before.*returned=undef.*after/s,
     'statements before the drop ran, and the program continues after the eval');

# 3 ── a drop the program never reaches costs nothing.
my ($cl_unreached) = transpile_dropping(<<'PL');
sub f { 42 }
if (0) { f() = 7 }
print "unreached-costs-nothing\n";
PL
is(run_cl($cl_unreached), "unreached-costs-nothing\n",
   'a drop in a branch the program never takes neither dies nor warns');

# 4 ── the census key is byte-for-byte what every counter reads.
like($cl, qr/\(progn ;; PARSE ERROR: PCL: Can't modify non-lvalue subroutine call in assignment\n/,
     'the `;; PARSE ERROR: <reason>` comment is unchanged — the census key survives the flip');

# 5 ── ONE builder: the announcement and the die name the SAME statement.
my ($ann_site) = $err =~ /^PCL: statement dropped at (.+?) -- /m;
my ($die_site) = $cl  =~ /"PCL: statement not supported at (.+?) -- /;
ok(defined $ann_site && length $ann_site, 'the transpile-time announcement still fires')
    or diag("stderr was: $err");
is($die_site, $ann_site,
   'the announcement and the emitted die name the SAME statement (one _drop_site builder)');

# 6 ── the inverse: no drop, no die, no PARSE ERROR anywhere in the emission.
my ($cl_clean, $err_clean) = transpile_dropping(<<'PL');
sub f { 42 }
my $x = f();
print "$x\n";
PL
ok($cl_clean !~ /PARSE ERROR/ && $cl_clean !~ /statement not supported/
     && $err_clean !~ /statement dropped/,
   'a program with no drop is untouched by the flip — no die, no PARSE ERROR, nothing on stderr');

# 7 ── a TILDE in the dropped statement's source text must not reach a format
# control string.  Before the p-die fix this died with an unhandled
# sb-format:format-error naming "Unknown format directive (character: Space)",
# outside any eval, taking the file with it.
my ($cl_tilde) = transpile_dropping(<<'PL');
sub f { 42 }
my $x = "abc";
my $ok = eval { f() = ($x =~ /b/); 1 };
print "trapped=", (defined $ok ? "no" : "yes"), "\n";
print "after\n";
PL
my $out_tilde = run_cl($cl_tilde);
like($out_tilde, qr/^trapped=yes$/m,
     'a dropped statement containing `~` still dies TRAPPABLY (p-die takes the message as data, not as a format control string)');
like($out_tilde, qr/^after$/m,
     'and the file survives it — the format-error used to abort the whole load');

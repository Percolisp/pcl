#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# imported-term-01.t — task #365: an IMPORTED `()`-prototype sub is a TERM.
#
# `sub pi () {…}` makes the bareword `pi` a term in perl, so `2 * pi` is
# 2 * pi(), `print pi, "\n"` prints the number, and `pi + 1` is pi() + 1.
# PCL printed the STRING "pi" after an operator or a comma (`2 * pi` was 0)
# and read the head of an expression as a LIST OPERATOR (`pi + 1` parsed as
# pi(+1)) — the #266 classifier answering from an ABSENCE of knowledge.
#
# THE CAUSE WAS THE `use` SEAM, NOT THE TERM READING (measured s438c, and it
# is the opposite of where the task pointed).  `Parser::_merge_module_
# prototypes` imported a module's prototype only when it affected code
# generation — a block arg, or a parameter SLOT (`$`, `\X`, `@`, `%`) — or
# when `export_names` listed it.  A `()` prototype has no slots, and
# `export_names` reads literal `qw()` out of `@EXPORT`/`@EXPORT_OK`, which real
# modules build from variables:
#
#     my @trig = qw( pi tan … );                       # Math::Complex
#     our @EXPORT = (qw( i Re Im … atan2 ), @trig);
#
# so `pi` reached @EXPORT through @trig and the scan never saw it (and
# Math::Trig re-exports it from there, which is the reported case).  Following
# that would mean interpreting the module's own code; the fix keys on the
# PROTOTYPE instead — an empty one is a PARSE fact, so it crosses a `use` on
# the same footing as a block prototype.
#
# ONE PREDICATE, `Pl::Environment::proto_is_zero_arg`: PExpr::_is_zero_arg_func
# (does this bareword parse as a term?) and the merge (must this prototype
# cross a `use`?) had drifted into two copies of the record test, which is what
# let this through.
#
# Emission is IDENTICAL across the four populations with the fix in (951 files
# A/B'd, 0 DIFF, plus corpus-diff over the 111), so no corpus guards it: these
# rows are the guard.  The fixture builds its @EXPORT the way Math::Complex
# does, so the row tests the MECHANISM and not one CPAN module's spelling.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use File::Path qw(make_path);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 18;

# A module whose @EXPORT is built from a VARIABLE, like Math::Complex's.
my $libdir = tempdir(CLEANUP => 1);
make_path("$libdir/T438");
open(my $mfh, '>', "$libdir/T438/Konst.pm") or die "fixture: $!";
print $mfh <<'PM';
package T438::Konst;
use strict; use warnings;
require Exporter;
our @ISA = qw(Exporter);
my @consts = qw( kpi khalf );
our @EXPORT = (qw( kname ), @consts);
sub kpi   () { 3.25 }
sub khalf () { 0.5 }
sub kname { "T438" }
1;
PM
close $mfh;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "use lib '$libdir';\n$code";
    close $fh;
    return $pl_file;
}

# Transpile (PCLCore::transpile FAILS the row on a dropped statement) and run.
sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- the bug: a term after an operator or a separator ---------------------

both_agree('use T438::Konst; print kpi, "\n";',
           'a `()` sub after a COMMA is the term, not the string "kpi"');

both_agree('use T438::Konst; my $w = 2 * kpi; print "$w\n";',
           '... and after a binary operator (was 0: "kpi" numified)');

both_agree('use T438::Konst; my $z = kpi + 1; print "$z\n";',
           '... and at the HEAD of one: kpi() + 1, not kpi(+1)');

both_agree('use T438::Konst; my @l = (kpi, 1); print "@l\n";',
           '... and inside a list');

# ---- the inverses, which were already right and must stay right ----------

both_agree('use T438::Konst; my $y = kpi; print "$y\n";',
           'a lone term is unchanged');

both_agree('use T438::Konst; print kname(), "\n";',
           'a plain (non-prototyped) exported sub still calls');

# ---- the negative: an unknown bareword is still a STRING -----------------

both_agree('use T438::Konst; print "x=", nosuchword, "\n";',
           'an UNKNOWN bareword stays the string (no strict subs)');

# ---- task #484: the TOKEN REPAIRS ask the same question -------------------
#
# perl reads `/` after a bareword as DIVISION only when the word is a TERM, and
# `_repair_word_match` (the #351 `WORD /` repair) already asked exactly that —
# but through `_word_is_term`, which scanned only THIS DOCUMENT's `use
# constant`s and `sub NAME ()`.  An imported term is in neither: its prototype
# crossed the `use` into the shared Environment, and the pre-merge that fills
# that table ran AFTER the repair block.  So `kpi / 2 + kpi / 4` was repaired
# into `kpi m/ 2 + kpi /` and the statement DROPPED (announced, and fatal when
# reached since s435) where perl computes 2.4375.
#
# The fix runs the pre-merge before the repairs as well (it is idempotent —
# extraction memoizes by module name) and lets the term predicate read the
# Environment.  `_repair_word_x_call` asks the same predicate, so the last row
# below is the same fact in the other repair: `kpi x 2` is REPETITION in perl
# because kpi is a term, and the repair must not turn it into a call to `x`.
both_agree('use T438::Konst; my $q = kpi / 2 + kpi / 4; print "$q\n";',
           'an IMPORTED term keeps DIVISION: the statement is not repaired away (#484)');

both_agree('use T438::Konst; my $x = kpi / 2; print "$x\n";',
           '... one slash: no closing delimiter, the repair declined already');

both_agree('use T438::Konst; $_ = "foobar"; print "r=", (kname /foo/), "\n";',
           'the NEGATIVE: a non-term imported sub still gets the match repair');

both_agree('use T438::Konst; sub x { "X" } print kpi x 2, "\n";',
           'the x repair asks the same predicate: `kpi x 2` stays repetition');

# ---- task #711: the file's OWN `use constant`, seen from a SUB BODY -------
#
# Same fact, other seam.  A named sub's BODY is lowered before the in-stream
# `use constant` statement reaches the emitter that registers the name, so
# inside a sub the constant was not known to be zero-arg and swallowed the
# rest of the expression as ARGUMENTS:
#
#     use constant K => 4;
#     sub p { substr($_[0], $_[1] - K + 1, K) }
#
# emitted `(p-substr $s (p-- $start (pl-K 1 (pl-K))))` — K(+1, K) — so substr
# got TWO arguments and answered 5 characters where perl gives 4.  That is
# what made `File::Temp::tempfile(OPEN => 0)` die "The template must end with
# at least 4 'X' characters" on a template ending in ten X.  The registration
# now happens in a PRE-PASS (Parser2::_premerge_use_constant_prototypes),
# beside the one that does it for the `*NAME = sub () {…}` idiom.
#
# Rows 3 and 4 are the inverses: at TOP LEVEL this was always right (the
# statement had already lowered), and a plain sub of the same shape must keep
# swallowing its arguments — the fix keys on the constant, not on the shape.

both_agree('use constant K => 4;' . "\n"
         . 'sub p { my ($s,$i)=@_; return substr($s, $i - K + 1, K) }' . "\n"
         . 'print "[", p("/tmp/XXXXXXXXXX", 14), "]\n";',
           '#711 a `use constant` name inside a SUB BODY is a term, not a list op');

both_agree('use constant { KA => 4, KB => 2 };' . "\n"
         . 'sub q2 { my ($s,$i)=@_; return substr($s, $i - KA + 1, KB) }' . "\n"
         . 'print "[", q2("/tmp/XXXXXXXXXX", 14), "]\n";',
           '#711 ... and the HASH form declares them the same way');

both_agree('use constant K => 4;' . "\n"
         . 'my $s = "/tmp/XXXXXXXXXX";' . "\n"
         . 'print "[", substr($s, 14 - K + 1, K), "]\n";',
           '#711 inverse: at TOP LEVEL it was already right and stays right');

both_agree('sub K { 4 }' . "\n"
         . 'sub p3 { my ($s,$i)=@_; return substr($s, $i - K + 1, K) }' . "\n"
         . 'print "[", p3("/tmp/XXXXXXXXXX", 14), "]\n";',
           '#711 inverse: a PLAIN sub of the same shape still takes arguments');

# ---- task #733: the UNPARENTHESISED single-quote import list ---------------
#
# `use Perl::OSType 'os_type';` — PPI hands the quote over as a DIRECT CHILD
# of the Include statement, so `_parse_use_import_list` returned an EMPTY
# list, the merge took its import-everything branch, and that branch imports a
# plain sub only when the `@EXPORT` scan lists it.  Real modules build their
# export list from a variable (Perl::OSType: `our @EXPORT_OK = @{ $EXPORT_TAGS
# {all} }`), so the scan sees nothing and the bareword `os_type` was emitted as
# the STRING "os_type" (t/op/filetest.t:112).
#
# THE ONE-LINE FIX IS WRONG, MEASURED (s451z): `use Test::More 'no_plan'` is
# the identical shape and 'no_plan' is an ARGUMENT, and a non-empty import
# list makes the merge import ONLY those names — so Test::More's `is($$;$)`
# stopped arriving and every `is(...)` argument lost scalar context (three
# cpan-tests/Test-Simple files moved).
#
# So the reading comes from the MODULE (does it declare a sub of that name?),
# and — this is what makes it safe — it NEVER RESTRICTS: it runs as a SECOND,
# restricted merge on top of the ordinary one, so a name the module does not
# declare adds nothing and takes nothing away.  Rows 2 and 3 are that inverse.

my $vlib = tempdir(CLEANUP => 1);
make_path("$vlib/T733");
open(my $vfh, '>', "$vlib/T733/Var.pm") or die "fixture: $!";
print $vfh <<'PM';
package T733::Var;
use strict; use warnings;
require Exporter;
our @ISA = qw(Exporter);
my @names = qw( vfun );
our @EXPORT_OK = @names;
sub vfun { "VFUN" }
1;
PM
close $vfh;

# A module whose import ALSO accepts option words that are not subs — the
# Test::More 'no_plan' shape — and whose exported sub carries a ($$)
# prototype, which is the thing the s451z damage lost.
open(my $ofh, '>', "$vlib/T733/Opt.pm") or die "fixture: $!";
print $ofh <<'PM';
package T733::Opt;
use strict; use warnings;
require Exporter;
our @EXPORT = qw( oshow );
sub import { my $c = shift; local @_ = ($c); goto &Exporter::import }
our @ISA = qw(Exporter);
sub oshow ($$) { print "$_[0]|$_[1]\n" }
1;
PM
close $ofh;

sub both_agree_v {
    my ($code, $desc) = @_;
    my ($fh, $pl) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "use lib '$vlib';\n$code";
    close $fh;
    my $perl = `perl $pl 2>&1`;
    my $cl_code = PCLCore::transpile("$pl2cl $pl");
    my ($cfh, $cl) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl_code;
    close $cfh;
    my $pcl = `sbcl @sbcl_rt --load $cl 2>&1`;
    $pcl =~ s/^;.*\n//gm;
    $pcl =~ s/^PCL Runtime loaded\n//gm;
    $pcl =~ s/^\s*\n//gm;
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

both_agree_v(q{use T733::Var 'vfun'; print "[", vfun, "]\n";},
             '#733 an unparenthesised import NAME makes the bareword a call');

both_agree_v(q{use T733::Opt 'quiet'; sub cx { wantarray ? "LIST" : "SCALAR" } oshow(cx(), "x");},
             '#733 inverse: an option WORD does not restrict the prototype merge');

both_agree_v(q{use T733::Var qw(vfun); print "[", vfun, "]\n";},
             '#733 inverse: the qw() spelling is unchanged');

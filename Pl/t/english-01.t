#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# english-01.t — task #502: `use English` works.
#
# Core English.pm aliases every English name to its punctuation variable with a
# whole-GLOB assignment whose right-hand side is a punctuation glob:
#
#     *LAST_PAREN_MATCH = *+ ;
#
# PCL does not lower `*+` (the glob-VALUE family, #463 items 3-5), so the whole
# module died at TRANSPILE and every English name was unreachable — the repro
# `use English; print defined($ORS) ? "d" : "u";` never ran.  A module's
# behaviour belongs in lib/ (rule 9a), so `lib/English.pm` supplies the same
# aliases with the two mechanisms PCL has:
#
#   *NAME = \$PUNCT   a SCALAR-slot alias, live in BOTH directions, for every
#                     punctuation variable PCL keeps in an ordinary cell.
#   tie $NAME, ...    for the six that are not cells: $& $` $' $+ $^N are raw
#                     globals the runtime REBINDS on every match (set-match-
#                     vars), so a value alias freezes at load time, and $!
#                     was a call into C errno with no variable behind it.
#                     ($! and $^E gained canonical magic boxes in task #561,
#                     so their tie is now a CHOICE — swapping it for a
#                     scalar-slot alias is a lib/ change with its own sweep.)
#                     $ARG is tied for
#                     the same reason — perl's shared glob tracks the DYNAMIC
#                     $_ that foreach/map/grep bind, and `\$_` does not (that
#                     is true in perl too: `*A = \$_` misses the loop there).
#
# The oracle is real perl running real English.pm; PCL runs the shim.  Rows
# that agree are the point — the shim has to be indistinguishable, not merely
# loadable.
#
# THE ONE KNOWN GAP is @ARG inside a sub, asserted below as a canary and
# written up in docs/not-supported.md ("`use English` — everything works
# except `@ARG` inside a sub").

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 15;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
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

# ---- the repro ------------------------------------------------------------

both_agree(<<'PL', 'the #502 repro: use English loads and $ORS is visible');
use English;
print defined($ORS) ? "d" : "u", "\n";
PL

# ---- reading a punctuation variable THROUGH the English name --------------

both_agree(<<'PL', 'reads through the alias see the punctuation variable');
use English;
$\ = "|";
print "ORS=[$ORS]\n";
$\ = "";
$, = "-";
print "OFS=[$OFS]\n";
$, = "";
$" = ":";
print "LS=[$LIST_SEPARATOR]\n";
$" = " ";
$/ = "X";
print "RS=[$INPUT_RECORD_SEPARATOR]\n";
$/ = "\n";
PL

# ---- writing through the English name reaches the punctuation variable -----

both_agree(<<'PL', 'writes through the alias reach the punctuation variable');
use English;
$OUTPUT_FIELD_SEPARATOR = "+";
print "a", "b", "\n";
$, = "";
my @x = (1,2,3);
$LIST_SEPARATOR = ";";
print "[@x]\n";
$" = " ";
$INPUT_RECORD_SEPARATOR = "Z";
print "RS=[$/]\n";
$/ = "\n";
$OUTPUT_RECORD_SEPARATOR = "!";
print "ORS";
$\ = "";
print "\n";
PL

# ---- the match variables are LIVE, not frozen at load time ----------------

both_agree(<<'PL', 'the match variables track EACH match (not frozen at load)');
use English;
"hello world" =~ /(o) (w)/;
print "1 M=[$MATCH] P=[$PREMATCH] O=[$POSTMATCH]\n";
print "2 LPM=[$LAST_PAREN_MATCH] LSR=[$LAST_SUBMATCH_RESULT]\n";
print "3 MS=[@LAST_MATCH_START] ME=[@LAST_MATCH_END]\n";
"abcdef" =~ /cd/;
print "4 M=[$MATCH] P=[$PREMATCH] O=[$POSTMATCH]\n";
PL

both_agree(<<'PL', 'a write to $MATCH dies, as it does in perl');
use English;
"ab" =~ /a/;
my $e = "no";
eval { $MATCH = "x"; 1 } or $e = "died";
print "match-write=[$e]\n";
PL

# ---- $! reaches errno through a tie (task #561 made an alias possible too) --

both_agree(<<'PL', '$OS_ERROR/$ERRNO follow errno, in string AND numeric context');
use English;
open(my $fh, "<", "/nope-xyz-abc-502") or 1;
print "1 str=[$OS_ERROR]\n";
print "2 num=[", $ERRNO+0, "]\n";
$OS_ERROR = 13;
print "3 bang=[$!] num=[", $!+0, "]\n";
open($fh, "<", "/nope-xyz-abc-502") or 1;
print "4 ext=[$EXTENDED_OS_ERROR]\n";
PL

# ---- $ARG tracks the DYNAMIC $_ ------------------------------------------

both_agree(<<'PL', '$ARG is the $_ in effect — foreach, map, grep, and writes');
use English;
$_ = "top";
print "1 [$ARG]\n";
for (qw(p q)) { print "2 [$ARG]\n" }
my @a = (1,2,3);
for (@a) { $ARG = $ARG * 10 }
print "3 [@a]\n";
print "4 [", join(",", map { $ARG . "!" } qw(x y)), "]\n";
print "5 [", join(",", grep { $ARG =~ /a/ } qw(ab cd ae)), "]\n";
$ARG = "T";
print "6 [$_]\n";
PL

# ---- the rest of the bar list --------------------------------------------

both_agree(<<'PL', '$EVAL_ERROR / $PROCESS_ID / $CHILD_ERROR / ids / $OSNAME');
use English;
eval { die "boom\n" };
print "1 [$EVAL_ERROR]";
print "2 pid=[", ($PROCESS_ID == $$ ? "y" : "n"), "] pid2=[", ($PID == $$ ? "y" : "n"), "]\n";
system("true");
print "3 child=[$CHILD_ERROR]\n";
print "4 uid=[", ($UID == $< ? "y" : "n"), "] euid=[", ($EUID == $> ? "y" : "n"), "]\n";
print "5 os=[$OSNAME] x=[", ($EXECUTABLE_NAME eq $^X ? "y" : "n"), "]\n";
print "6 warn=[$WARNING] db=[$PERLDB] comp=[$COMPILING]\n";
PL

# ---- the import list -----------------------------------------------------

both_agree(<<'PL', 'use English qw(-no_match_vars) omits the three match names');
use English qw( -no_match_vars );
"hello world" =~ /(o) (w)/;
print "1 M=", (defined($MATCH) ? "d" : "u"), "\n";
$\ = "|";
print "2 ORS=[$ORS]\n";
$\ = "";
print "3 LPM=[$LAST_PAREN_MATCH]\n";
PL

both_agree(<<'PL', 'a $NAME import list is grandfathered to the glob name');
use English qw( $ORS $EVAL_ERROR );
$\ = "|";
print "1 ORS=[$ORS]\n";
$\ = "";
eval { die "x\n" };
print "2 EVAL=[$EVAL_ERROR]";
print "3 ARG=", (defined($ARG) ? "d" : "u"), "\n";
PL

both_agree(<<'PL', 'a second package gets its own aliases onto the same globals');
package Other;
use English;
sub ors { return defined($ORS) ? "[$ORS]" : "u" }
package main;
use English;
$\ = "|";
print "1 other=", Other::ors(), "\n";
$\ = "";
print "2 main=", (defined($ORS) ? "d" : "u"), "\n";
PL

# ---- THE DOCUMENTED GAP, as a canary --------------------------------------
#
# ---- `local` THROUGH an English name (s507b, #2084 (2)) ---------------------
# The separators and $| are TIED to their punctuation variable, and `local` on
# a tied scalar goes through the tie (FETCH, STORE, STORE back), so `local
# $LIST_SEPARATOR` reaches `$"` and a `local $"` is seen through the alias.
# INVERSE: main 08de9e4f joined with a space and ignored every `local` here.

both_agree(<<'PL', 'local through an English alias reaches the punctuation variable, both ways');
use English;
my @l = (1, 2, 3);
{ local $LIST_SEPARATOR = "-"; print "01 [@l]\n"; } print "02 [@l]\n";
{ local $" = "+"; print "03 [$LIST_SEPARATOR]\n"; }
{ local $OFS = ","; print "04", "a"; print "\n"; }
{ local $ORS = "!\n"; print "05"; }
{ local $SUBSEP = "#"; my %h; $h{3,4} = 1; print "06 [", join(",", keys %h), "]\n"; }
{ local $RS = undef; print "07 [", (defined $/ ? "def" : "undef"), "]\n"; }
{ local $OUTPUT_AUTOFLUSH = 1; print "08 [$|]\n"; } print "09 [$|]\n";
sub show { print "10 [@l]\n" } { local $LIST_SEPARATOR = "/"; show(); }
$LIST_SEPARATOR = ":"; print "11 [@l]\n";
PL

# ...and the mechanism alone: `local` on a TIED package scalar FETCHes, STOREs
# and STOREs the saved value back (main 08de9e4f replaced the tied box: no
# STORE ever ran).
both_agree(<<'PL', 'local on a tied scalar goes through FETCH/STORE');
package T; sub TIESCALAR { my $v = $_[1]; bless \$v } sub FETCH { print "F\n"; ${$_[0]} } sub STORE { print "S(", (defined $_[1] ? $_[1] : "u"), ")\n"; ${$_[0]} = $_[1] }
package main;
our $x; tie $x, "T", "orig";
{ local $x = "new"; print "in [$x]\n"; }
print "out [$x]\n";
PL

# perl's `*ARG = *_` shares one symbol-table entry, and perl swaps the AV in
# *main::_ on every call, so @ARG inside a sub IS that sub's @_.  PCL binds @_
# per call and no pure-Perl mechanism reaches the caller's copy (a tied array's
# FETCH runs in its own frame), so @ARG holds what perl's @main::_ holds
# OUTSIDE a sub — nothing.  When true glob-to-glob aliasing lands, this row
# fails: that is the signal to delete it and fold the shape into the
# `$ARG`/`@ARG` row above.

{
    my $code = "use English;\nsub f { return scalar(\@ARG) }\nprint \"n=\", f(\"A\",\"B\"), \"\\n\";\n";
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    is($perl, "n=2\n", 'perl: @ARG inside a sub IS that sub\'s @_ (the oracle)');
    is(run_cl($code), "n=0\n",
       'PCL: @ARG is empty inside a sub — the ONE documented gap (not-supported.md)');
}

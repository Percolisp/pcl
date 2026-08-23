#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# print-fh-magic-01.t — task #466: a MAGIC scalar in the FILEHANDLE slot of
# print/printf/say.
#
#     local $_ = \*STDOUT;  print $_ "x\n";      # perl: x     PCL: DROPPED
#     my $fh   = \*STDOUT;  print $fh "x\n";     # perl: x     PCL: x
#
# perl's grammar is `listop: LSTOP indirob listexpr` with
# `indirob: WORD | scalar | block`, and `scalar` is ANY scalar variable — the
# punctuation, digit and caret spellings included.  PPI hands those over as
# PPI::Token::Magic, which IS a subclass of PPI::Token::Symbol, and PCL's three
# filehandle-slot tests were exact-class `ref($t) eq 'PPI::Token::Symbol'`: they
# answered "not a scalar", no filehandle was extracted, and the leftover
# `$_ "x\n"` run had two terms with no operator between them — so the WHOLE
# statement was dropped ("Bug. Fell through. Missing case").  One predicate,
# `Pl::PExpr::_is_scalar_fh_token`, now answers for all three sites (the
# operator-loop path, the `print $fh -e $f` filetest repair, the paren form).
#
# Every expectation below is the live `perl` answer (probed s441b, 5.40.3).
#
# THE NEGATIVES ARE THE POINT: widening the slot must not turn an ordinary
# `print $_ ...` into a filehandle write.  The second half of the decision —
# "does what follows START A TERM?" (`_is_print_term_start`) — is unchanged and
# is what keeps `print $_ . "\n"`, `print $_, "\n"`, `print $_ x 2` etc. reading
# $_ as the argument.  Rows 6..9 assert exactly that, and they are the rows that
# fail if the predicate is ever widened past `$`-sigil scalars.
#
# NOT asserted here, and deliberately: `$0 = "STDOUT"; print $0 "x"` prints x in
# perl (a STRING in the slot is a symbolic handle NAME) but nothing in PCL —
# because PCL's `$0` is not writable at all (`$0 = "X"; print $0` reads "sbcl"),
# which has nothing to do with the filehandle slot.  Same for `open($fh,'>&',…)`.
# Both are pre-existing and filed separately.

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

plan tests => 11;

my $dir = tempdir(CLEANUP => 1);
my $FIX = qq{my \$O = "$dir/out.txt";\n};

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (a DROPPED statement fails the row, via PCLCore) and run.
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

sub emitted { return PCLCore::transpile("$pl2cl " . write_pl($_[0])) }

# ── 1. $_ in the slot really WRITES TO THE HANDLE ────────────────────────────
# The write goes to a file, so a row that merely printed to stdout (the
# pre-fix reading, had it parsed at all) cannot pass by accident.
is(run_cl($FIX . <<'PL'), "file:[to-the-handle\n]\n",
open(my $fh, '>', $O) or die "open: $!";
local $_ = $fh;
print $_ "to-the-handle\n";
close $fh;
open(my $in, '<', $O) or die "reopen: $!";
my $got = do { local $/; <$in> };
close $in;
print "file:[$got]\n";
PL
   '`print $_ LIST` writes to the handle $_ holds');

is(run_cl($FIX . <<'PL'), "file:[pf-42\n]\n",
open(my $fh, '>', $O) or die "open: $!";
local $_ = $fh;
printf $_ "pf-%d\n", 42;
close $fh;
open(my $in, '<', $O) or die "reopen: $!";
my $got = do { local $/; <$in> };
close $in;
print "file:[$got]\n";
PL
   '`printf $_ FMT, LIST` writes to the handle $_ holds');

is(run_cl($FIX . <<'PL'), "file:[said\n]\n",
use feature 'say';
open(my $fh, '>', $O) or die "open: $!";
local $_ = $fh;
say $_ "said";
close $fh;
open(my $in, '<', $O) or die "reopen: $!";
my $got = do { local $/; <$in> };
close $in;
print "file:[$got]\n";
PL
   '`say $_ LIST` writes to the handle $_ holds');

# ── 2. the other parse paths the same input can take ─────────────────────────
# The paren form is a SECOND site (_extract_paren_filehandle) and the block form
# a THIRD; the block form always worked and must keep working.
is(run_cl(<<'PL'), "paren\nblock\n",
local $_ = \*STDOUT;
print($_ "paren\n");
print {$_} "block\n";
PL
   'the paren form and the block form both take $_ as the handle');

# ── 3. other magic spellings are scalars too (perl: `indirob: scalar`) ───────
# `$,` is the output field separator; holding a handle in it is absurd Perl and
# is exactly why it is here — the rule is the SIGIL, not the name.
is(run_cl(<<'PL'), "comma-var\ncaret-var\n",
local $, = \*STDOUT;
print $, "comma-var\n";
local ${^MYHANDLE} = \*STDOUT;
print ${^MYHANDLE} "caret-var\n";
PL
   'a punctuation or caret scalar in the slot is a handle, like any scalar');

# ── 4. the wild shape (Test::Builder::NoOutput lines 118/125) ────────────────
is(run_cl(<<'PL'), "one\none\ntwo\ntwo\n",
sub emit { my $self = shift; print $_ @_ for @$self }
sub emitf { my $self = shift; printf $_ @_ for @$self }
my $two = [\*STDOUT, \*STDOUT];
emit($two, "one\n");
emitf($two, "%s\n", "two");
PL
   '`print $_ @_ for @$self` — the shape this task was minimised from');

# ── 5. THE NEGATIVES: an ordinary `print $_ …` must stay an ARGUMENT ─────────
is(run_cl(<<'PL'), "N\nO,\nP\n4\nUU\nV\nyes\nm\n",
local $_ = "N"; print $_ . "\n";
local $_ = "O"; print $_, ",\n";
local $_ = "P"; print $_; print "\n";
local $_ = 3;   print $_ + 1, "\n";
local $_ = "U"; print $_ x 2, "\n";
local $_ = "V"; print uc $_, "\n";
local $_ = "T"; print $_ ? "yes\n" : "no\n";
local $_ = "W"; print $_ =~ /W/ ? "m\n" : "n\n";
PL
   'an operator, a comma or end-of-statement after $_ keeps it an argument');

is(run_cl(<<'PL'), "QR\nab\n[S]\n",
my @l = ("Q","R"); print $_ for @l; print "\n";
my @l2 = ("a","b"); for (@l2) { print $_ } print "\n";
my @l3 = ("S"); print "[$_]" for @l3; print "\n";
PL
   'the `for` modifier and a foreach body keep `print $_` printing $_');

is(run_cl(<<'PL'), "k\n0\n",
my %h = (x => "k"); local $_ = \%h; print $_->{x}, "\n";
my @a = (0,1); local $_ = \@a; print $_->[0], "\n";
PL
   'a `->` chain after $_ keeps it an argument, not a handle');

# ── 6. transpile shape: the two readings are visibly different ───────────────
like(emitted(q{local $_ = \*STDOUT; print $_ "x\n";}),
     qr/\(p-print :fh \$_/,
     'the handle reading emits (p-print :fh $_ …)');

unlike(emitted(q{local $_ = "x"; print $_ . "\n";}),
       qr/:fh/,
       'the argument reading emits no :fh slot');

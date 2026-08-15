#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# regex-interp-01.t — interpolation INSIDE a regex pattern (task #237).
#
# A pattern interpolates like a dq string but with perl's pattern start rules
# and S_intuit_more deciding whether the `[…]`/`{…}` after a variable is a
# SUBSCRIPT or regex syntax (charclass / {n,m} quantifier).  Both live in
# Pl::InterpScan (unit-tested against the perl sources by interp-scan-01.t);
# ExprToCL::_gen_interp_regex_pattern is its consumer, and THIS file guards
# the consumer end to end.
#
# Before s382f the consumer was a private walk that knew only `$name`,
# `${name}` and explicit `->[i]`/`->{k}` chains, so `/$a[0]/`, `/$h{k}/` and
# `qr/\G$_[1]/` leaked into the pattern as literal text (#237), `/$r->[0]/`
# emitted a non-dereferencing (p-aref …), and `@arr`/`$#a`/`$1`/`$^O` never
# interpolated at all.  The charclass/quantifier readings that DID work
# (`/$x[abc]/`, `/$x[^a]/`, `/$x{2,3}/`) must keep working — those are the
# shapes a naive "always a subscript" fix breaks.
#
# Method: REAL PERL is the oracle, re-derived at test time.  One program runs
# under perl and under PCL and the outputs are compared key by key, so a perl
# upgrade that changed a reading would fail loudly instead of drifting.
use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use lib "$RealBin/../..";
use PCLCore;
use Pl::Parser2;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt      = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

# Every `t(...)` line below is one guard row; keep the list and the count in
# step (5 emission rows follow the oracle comparison).
my @KEYS = (
  'x[abc]', 'x[^a]', 'x{2,3}', 'x{2}', 'x[\w]',
  'a[0]', 'h{k}', 'h{-3}', 'ul_deref', 'qr_G_ul1',
  'arrow[0]', 'arrow{k}', 'chain', 'idxexpr', 'idxvar', 'keyquoted',
  'array', 'derefsnail', 'lastidx', 'braced', 'bracedlit',
  'cap', 'caret', 'qualified', 'exprderef', 'emptyarr',
  'subst', 'dq',
);
plan tests => scalar(@KEYS) + 5;

my $PROG = <<'PERL';
use strict; use warnings;
package Foo; our $bar = 'QQ'; package main;
our @x = ('zero','one','two'); our $x = 'x';
our %h = (k => 'KV', 'a b' => 'SPACED', '-3' => 'NEG');
our @arr = ('p','q');
our $r = \@x;
our $hr = { k => 'HK' };
our $i = 1;
our $s = 'x';
our @m = ([ 'inner' ]);
our @e = ();
sub t { my ($d, $g) = @_; print "$d = ", (defined $g ? "[$g]" : "UNDEF"), "\n" }

# intuit_more says "regex syntax", NOT a subscript — these must keep working
t('x[abc]',  ("xabc" =~ /$x[abc]/) ? 'M' : 'N');
t('x[^a]',   ("xz"   =~ /$x[^a]/)  ? 'M' : 'N');
t('x{2,3}',  ("xxx"  =~ /$s{2,3}/) ? 'M' : 'N');
t('x{2}',    ("xx"   =~ /$s{2}/)   ? 'M' : 'N');
t('x[\w]',   ("xq"   =~ /$s[\w]/)  ? 'M' : 'N');

# intuit_more says "subscript" — the #237 cause
t('a[0]',    ("zero"  =~ /^$x[0]$/) ? 'M' : 'N');
t('h{k}',    ("KV"    =~ /^$h{k}$/) ? 'M' : 'N');
t('h{-3}',   ("NEGq"  =~ /^$h{-3}q$/) ? 'M' : 'N');
{ local $_ = [0, 'ARG']; t('ul_deref', ("ARG" =~ /^$$_[1]$/) ? 'M' : 'N'); }
sub gg { my $re = qr/\G$_[1]/; return ("ARG" =~ $re) ? 'M' : 'N' }
t('qr_G_ul1', gg('x','ARG'));
t('arrow[0]', ("zero"  =~ /^$r->[0]$/) ? 'M' : 'N');
t('arrow{k}', ("HK"    =~ /^$hr->{k}$/) ? 'M' : 'N');
t('chain',    ("inner" =~ /^$m[0][0]$/) ? 'M' : 'N');
t('idxexpr',  ("two"   =~ /^$x[$i+1]$/) ? 'M' : 'N');
t('idxvar',   ("one"   =~ /^$x[$i]$/) ? 'M' : 'N');
t('keyquoted',("SPACED" =~ /^$h{'a b'}$/) ? 'M' : 'N');

# reference kinds the private walk never interpolated at all
t('array',     ("p q" =~ /^@arr$/) ? 'M' : 'N');
t('derefsnail',("zero one two" =~ /^@$r$/) ? 'M' : 'N');
t('lastidx',   ("2" =~ /^$#x$/) ? 'M' : 'N');
t('braced',    ("x" =~ /^${s}$/) ? 'M' : 'N');
t('bracedlit', ("x[0]" =~ /^${s}\[0\]$/) ? 'M' : 'N');   # braces CLOSE the ref
t('caret',     ($^O =~ /^\Q$^O\E$/) ? 'M' : 'N');
t('qualified', ("QQ" =~ /^$Foo::bar$/) ? 'M' : 'N');
t('exprderef', ("x" =~ /^${\ $s}$/) ? 'M' : 'N');
t('emptyarr',  ("" =~ /^@e$/) ? 'M' : 'N');
"abc" =~ /(b)/;
t('cap',       ("b" =~ /^$1$/) ? 'M' : 'N');

# the s/// pattern side shares the consumer
{ my $tt = "zero-tail"; $tt =~ s/^$x[0]/HIT/; t('subst', $tt) }
# ... and dq text must NOT move
t('dq', "a$x[0]-$h{k}-@arr-$#x");
PERL

sub outputs_of {
    my ($cmd_out) = @_;
    my %v;
    for my $line (split /\n/, $cmd_out) {
        $v{$1} = $2 if $line =~ /^(.+?) = (.*)$/;
    }
    return \%v;
}

my ($pfh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
print $pfh $PROG;
close $pfh;

my $perl_out = `$^X $pl_file 2>&1`;
my $cl_code  = `$pl2cl $pl_file 2>/dev/null`;
my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $cfh $cl_code;
close $cfh;
my $pcl_out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
$pcl_out =~ s/^;.*\n//gm;
$pcl_out =~ s/^PCL Runtime loaded\n//gm;

my $perl = outputs_of($perl_out);
my $pcl  = outputs_of($pcl_out);
for my $k (@KEYS) {
    if (!defined $perl->{$k}) {
        fail("$k — the perl oracle produced no row (perl drift?)");
        diag($perl_out);
        next;
    }
    is($pcl->{$k}, $perl->{$k}, "$k: PCL matches perl ($perl->{$k})");
}

# --- emission shapes: the reference lowers as it does in code ---------------
# One expression pipeline answers every shape, so a pattern reference emits
# exactly what the same reference emits as code (rule 11) — no second table.
my $cl = Pl::Parser2->parse_code(<<'EOT');
our @x = (1,2); our %h = (k=>1); our @arr = (1); our $s = "x";
my $a = "s" =~ /$x[0]/;
my $b = "s" =~ /$h{k}/;
my $c = "s" =~ /@arr/;
my $d = "s" =~ /$s[abc]/;
print $a;
EOT
like($cl, qr/\(p-aref \@x 0\)/,          'direct $x[0] in a pattern reads @x');
like($cl, qr/\(p-gethash \%h "k"\)/,     'direct $h{k} in a pattern reads %h');
like($cl, qr/\(p-join \|\$"\| \@arr\)/,  '@arr in a pattern joins with $"');
like($cl, qr/\Q"[abc]"\E/,               '$s[abc] keeps the charclass literal');
unlike($cl, qr/p-aref \@s /,             '$s[abc] does NOT read @s');


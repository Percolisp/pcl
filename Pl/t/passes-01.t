#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# passes-01.t — the optimization registry (Pl::Passes, PCL_OPT; task #383,
# docs/plan-one-compiler-s411.md Phase R).  Three properties, each of which
# would break QUIETLY:
#
#   1. every Kind-A name actually gates its emission — `-name` removes the
#      fast shape and the general form appears (a gate that reads a name
#      nobody wired is "always on", which is what the registry exists to
#      make impossible);
#   2. the flag is not a correctness switch — the SAME program prints the
#      same thing under every setting (one SBCL run per setting; the sweep
#      under PCL_OPT=none is the population-wide version of this row);
#   3. a typo dies naming the known list, and a Kind-B pass registers, runs
#      in order, and obeys the flag (in-process, no SBCL).
use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use lib "$RealBin/../..";
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);
plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;
plan tests => 32;

# One program that exercises the Kind-A transforms: a raw let slot with a
# counting loop (raw-slot + foreach-range), an append-only string
# (str-buffer), a B-regime numeric freeze (raw-numeric), and a read-only
# foreach over an ARRAY (foreach-raw, task #862 ARM A).
my $PROG = <<'PERL';
my $n = 0; for my $i (1..10) { $n += $i } print "$n\n";
my $s = ""; for my $j (1..3) { $s .= "x" } print "$s\n";
my $c = shift // 5; $c = $c * 2; print $c + 1, "\n";
my @a = (7, 8); my $t = 0; for my $x (@a) { $t += $x } print "$t\n";
PERL
my ($pfh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
print $pfh $PROG;
close $pfh;

sub transpile_with {
    my ($opt) = @_;
    local $ENV{PCL_OPT} = $opt if defined $opt;
    delete local $ENV{PCL_OPT} unless defined $opt;
    return PCLCore::transpile(qq{$pl2cl $pl_file});
}
sub run_with {
    my (%env) = @_;
    local @ENV{keys %env} = values %env;
    my $cl = PCLCore::transpile(qq{$pl2cl $pl_file});
    my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl;
    close $cfh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    # the runtime's source-load chatter (same strip as transpile-test-10.t)
    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# --- 1. each name gates its emission -----------------------------------
my $def = transpile_with(undef);
like($def, qr/\(let \(\(\$n 0\)\)/,          'default: raw let slot');
like($def, qr/p-foreach-range-raw/,           'default: counting loop, raw var');
like($def, qr/%pcl-str-append/,               'default: str-buffer append');
like($def, qr/%pcl-to-number-strict/,         'default: raw-numeric freeze');
like($def, qr/\(p-foreach-raw \(\$x \@a\)/,   'default: read-only foreach-LIST takes the raw arm');

my $none = transpile_with('none');
like($none,   qr/\(let \(\(\$n \(make-p-box nil\)\)\)/, 'none: the slot is a box');
unlike($none, qr/p-foreach-range/,            'none: no counting loop');
like($none,   qr/\(p-foreach \(\$i \(p-\.\. 1 10\)\)/, 'none: general p-foreach over the range');
unlike($none, qr/%pcl-str-(?:append|buffer)/, 'none: no str-buffer');
unlike($none, qr/%pcl-to-number-strict/,      'none: no freeze');
unlike($none, qr/p-foreach-raw/,              'none: no raw foreach arm');
like($none,   qr/\(p-foreach \(\$x \@a\)/,    'none: the general aliasing p-foreach over the array');

my $no_slot = transpile_with('-raw-slot');
like($no_slot, qr/\(let \(\(\$n \(make-p-box nil\)\)\)/, '-raw-slot: boxed');
like($no_slot, qr/\(p-foreach-range \(\$i 1 10\)/, '-raw-slot: the counting loop keeps its BOXED variant');

my $no_num = transpile_with('-raw-numeric');
unlike($no_num, qr/%pcl-to-number-strict/,    '-raw-numeric: no freeze');
like($no_num,   qr/p-foreach-range-raw/,      '-raw-numeric: other transforms untouched');

my $alias = do { local $ENV{PCL_NO_RAW_VERDICT} = 1; transpile_with(undef) };
is($alias, $no_num, 'PCL_NO_RAW_VERDICT=1 is exactly -raw-numeric');

my $no_buf = transpile_with('-str-buffer');
unlike($no_buf, qr/%pcl-str-append/,          '-str-buffer: no buffer append');
like($no_buf,   qr/\(p-\.=-raw \$s "x"\)/,    '-str-buffer: the raw slot keeps a plain raw append');

my $no_range = transpile_with('-foreach-range');
like($no_range, qr/\(p-foreach \(\$i \(p-\.\. 1 10\)\) :my t \(p-incf-raw \$n \$i\)\)/,
     '-foreach-range: general loop over a materialized range, slot still raw');

my $no_fe = transpile_with('-foreach-raw');
like($no_fe,   qr/\(p-foreach \(\$x \@a\) :my t \(p-incf-raw \$t \$x\)\)/,
     '-foreach-raw: the aliasing loop is back, the accumulator slot still raw');
like($no_fe,   qr/p-foreach-range-raw/,       '-foreach-raw: other transforms untouched');

# Phase A names (folded from the deleted ExprToCL2): a call to a context-
# INSENSITIVE user sub gets no *wantarray* bind; `$h{k} = V` on a let-bound
# hash writes through CL setf.
{
    my ($afh, $a_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $afh <<'PERL';
sub ins { my ($x) = @_; return $x + 1 }
sub sens { return wantarray ? "L" : "S" }
my %h = (k => 1); my @a = (1, 2);
$h{k} = ins(2); $a[1] = 7; my $s = sens();
print "$h{k} $a[1] $s\n";
PERL
    close $afh;
    my $t = sub { my ($opt) = @_; local $ENV{PCL_OPT} = $opt if defined $opt; delete local $ENV{PCL_OPT} unless defined $opt; PCLCore::transpile(qq{$pl2cl $a_file}) };
    my $d = $t->(undef);
    like($d,   qr/\(setf \(p-gethash %h "k"\) \(pl-ins 2\)\)/, 'default: elem-setf + no bind around the insensitive call');
    like($d,   qr/\(p-scalar-ctx \(pl-sens\)\)/,   'default: a context-SENSITIVE sub keeps its bind');
    my $ni = $t->('-insensitive-call');
    like($ni,  qr/\(setf \(p-gethash %h "k"\) \(p-scalar-ctx \(pl-ins 2\)\)\)/, '-insensitive-call: the bind is back, setf stays');
    my $ne = $t->('-elem-setf');
    like($ne,  qr/\(p-setf \(p-gethash %h "k"\) \(pl-ins 2\)\)/, '-elem-setf: p-setf is back, no bind stays');
    like($ne,  qr/\(p-setf \(p-aref \@a 1\) 7\)/,           '-elem-setf: the array element too');
}

# --- 2. not a correctness switch: same output under every setting --------
my $out_def  = run_with();
my $out_none = run_with(PCL_OPT => 'none');
is($out_def, "55\nxxx\n11\n15\n", 'default setting runs to the expected output');
is($out_none, $out_def, 'PCL_OPT=none runs to the SAME output (general forms are correct)');

# --- 3. the registry itself: typo dies; Kind-B passes -------------------
{
    my (undef, $err, $rc) = PCLCore::transpile_raw(qq{PCL_OPT=bogus $pl2cl $pl_file});
    ok($rc != 0 && $err =~ /unknown optimization name\(s\) in PCL_OPT: bogus/
        && $err =~ /known: .*raw-numeric.*str-buffer/,
       'an unknown name in PCL_OPT dies naming the known list');
}
{
    # In-process: register two passes, check order and the flag, with the
    # environment set BEFORE the module loads (it is read once, at load).
    local $ENV{PCL_OPT} = '-second';
    require Pl::Passes;
    Pl::Passes::register_pass('first',  sub { my ($f) = @_; ref $f && $f->[0] eq 'p-print' ? ['p-say', @{$f}[1 .. $#$f]] : $f });
    Pl::Passes::register_pass('second', sub { my ($f) = @_; ref $f && $f->[0] eq 'p-say' ? ['p-shout', @{$f}[1 .. $#$f]] : $f });
    my $got = Pl::Passes::run(['p-print', '"x"']);
    is_deeply($got, ['p-say', '"x"'], 'Kind-B: the first pass ran; the second is switched off by PCL_OPT=-second');
    ok(Pl::Passes::enabled('first') && !Pl::Passes::enabled('second') && Pl::Passes::enabled('raw-slot'),
       'enabled() answers per name; an unmentioned name is on');
}

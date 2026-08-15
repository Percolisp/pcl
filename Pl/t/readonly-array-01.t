#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# readonly-array-01.t: Internals::SvREADONLY(@a, 1) makes an array FIXED SIZE
# (task #159).  Perl's read-only AV forbids every size change and nothing else:
# its ELEMENTS stay writable.  PCL implements that by swapping the variable's
# storage for a simple vector (no fill pointer, not adjustable), so every
# size-changing operation fails by construction; the runtime checks only turn
# that into perl's "Modification of a read-only value attempted".
#
# Every expectation below was taken from real perl 5.40 running the same
# program (scratchpad ro3.pl/ro4.pl, session 337) — including the two perl
# quirks that look like typos and are not: unshift dies even for an EMPTY list
# while push with an empty list is allowed, and `$#a = 0` TRUNCATES a read-only
# array instead of dying.
#
# The INVERSE guards matter as much as the positives: a plain array must still
# push/pop/splice, and clearing the flag must restore a fully mutable array.

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

plan skip_all => "pl2cl not found" if ! -x $pl2cl;
plan skip_all => "sbcl not found"  if ! `which sbcl 2>/dev/null`;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>&1`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

plan tests => 18;

# One SBCL launch for the whole family: each line prints one answer.
my $prog = <<'EOF';
# try(TAG, CODE): print DIE when CODE dies with perl's read-only message,
# OK when it runs, and the raw $@ for any other death (so a wrong message
# fails the test loudly instead of passing as "some death").
sub try {
    my ($tag, $code) = @_;
    my $ok = eval { $code->(); 1 };
    if ($ok)                                         { print "$tag:OK\n" }
    elsif ($@ =~ /^Modification of a read-only value/) { print "$tag:DIE\n" }
    else { my $e = $@; $e =~ s/\n.*//s; print "$tag:OTHER($e)\n" }
}
sub fresh { my @a = (1,2,3); Internals::SvREADONLY(@a, 1); return \@a }

# --- reads and ELEMENT writes are allowed: read-only means fixed SIZE
try('t1', sub { my $r = fresh(); my $x = $r->[1]; die "bad read" if $x != 2 });
try('t2', sub { my $r = fresh(); $r->[0] = 99; die "not stored" if $r->[0] != 99 });

# --- every size change dies
try('t3', sub { push    @{fresh()}, 9 });
try('t4', sub { unshift @{fresh()}, 9 });
try('t5', sub { pop     @{fresh()} });
try('t6', sub { shift   @{fresh()} });
try('t7', sub { splice  @{fresh()}, 1, 0, () });   # even a no-op splice
try('t8', sub { delete  ${fresh()}[1] });
try('t9', sub { fresh()->[5] = 99 });              # out of bounds = would extend
try('t10', sub { @{fresh()} = (1,2) });            # whole-array assignment
try('t11', sub { undef @{fresh()} });
try('t12', sub { $#{fresh()} = 5 });               # growing via $#a

# --- the two asymmetries perl really has
try('t13', sub { push @{fresh()}, () });           # stores nothing: allowed
try('t14', sub { unshift @{fresh()}, () });        # stores nothing: still dies

# --- the flag reads back, and clearing it restores a mutable array
my @w = (1,2);
print "t15:[", Internals::SvREADONLY(@w), "]\n";
my @z = (1,2);
Internals::SvREADONLY(@z, 1);
print "t16:[", Internals::SvREADONLY(@z), "]\n";
Internals::SvREADONLY(@z, 0);
push @z, 9;
print "t17:", join(",", @z), "\n";

# --- INVERSE: a plain array is untouched by all of the above
my @p = (1,2,3);
push @p, 4; unshift @p, 0; pop @p; splice @p, 1, 1; $p[4] = 9; $#p = 4;
print "t18:", join(",", map { defined $_ ? $_ : 'u' } @p), "\n";
EOF

my $out = run_cl($prog);

like $out, qr/^t1:OK$/m,  'reading a read-only array element is allowed';
like $out, qr/^t2:OK$/m,  'writing an IN-BOUNDS element is allowed (elements are not read-only)';

like $out, qr/^t3:DIE$/m,  'push onto a read-only array dies with perl\'s message';
like $out, qr/^t4:DIE$/m,  'unshift dies';
like $out, qr/^t5:DIE$/m,  'pop dies';
like $out, qr/^t6:DIE$/m,  'shift dies';
like $out, qr/^t7:DIE$/m,  'splice dies even when it would change nothing';
like $out, qr/^t8:DIE$/m,  'delete of an element dies';
like $out, qr/^t9:DIE$/m,  'an OUT-OF-BOUNDS element write dies (it would extend)';
like $out, qr/^t10:DIE$/m, 'whole-array assignment dies (this is sort.t\'s in-place sort row)';
like $out, qr/^t11:DIE$/m, 'undef @a dies';
like $out, qr/^t12:DIE$/m, '$#a = 5 dies (growing)';

like $out, qr/^t13:OK$/m,  'push of an EMPTY list is allowed — nothing would be stored (push.t)';
like $out, qr/^t14:DIE$/m, 'unshift of an empty list still dies — perl is asymmetric here (unshift.t)';

like $out, qr/^t15:\[\]$/m,  'the getter is "" (false) for a writable array';
like $out, qr/^t16:\[1\]$/m, 'and 1 once the flag is set';
like $out, qr/^t17:1,2,9$/m, 'clearing the flag restores a mutable array';

like $out, qr/^t18:0,2,3,u,9$/m,
    'INVERSE: an ordinary array still push/unshift/pop/splice/extends normally';

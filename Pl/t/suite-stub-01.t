#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# suite-stub-01.t — the helpers perl-tests/t/test.pl owes perl's own t/test.pl.
#
# The stub is what `require './test.pl'` resolves to when tools/run-perl-suite.pl
# runs perl's own t/ files through PCL (the shadow t/, task #136).  A helper the
# stub does not define is NOT a missing feature: it is an `undef-fn` abort that
# kills every REMAINING top-level form of the calling file, and the rows behind
# that form simply never appear.  Measured s473t4 (#1501): `unlink_tempfiles`
# cost t/run/runenv_hashseed.t 269 of its 278 rows and `is_linux_container` cost
# t/op/stat.t all 111 of its rows — both files reported DIFF, not CRASH, so the
# loss was invisible to every verdict.
#
# The stub is loaded in a CHILD perl, never here: its `sub ok ($@);` prototype
# declarations would collide with Test::More's imports in this process.

use strict;
use warnings;
use Test::More tests => 4;
use FindBin qw($RealBin);

my $stub = "$RealBin/../../perl-tests/t/test.pl";
ok(-f $stub, 'the PCL test.pl stub is where the shadow t/ symlinks it');

# Each helper is probed for its BEHAVIOUR, not merely its existence.
my $out = `perl -e 'require q{$stub};
    my \$f = main::tempfile();
    open my \$fh, ">", \$f or die; print \$fh "x"; close \$fh;
    print "made=", (-e \$f ? 1 : 0), "\n";
    main::unlink_tempfiles();
    print "gone=", (-e \$f ? 0 : 1), "\n";
    print "container=", (main::is_linux_container() =~ /^[01]\$/ ? "ok" : "BAD"), "\n";
    print "unlink_all=", main::unlink_all(), "\n";
' 2>&1`;

like($out, qr/^made=1$/m, 'tempfile() still hands out a fresh name');
like($out, qr/^gone=1$/m,
     'unlink_tempfiles() removes every name tempfile() handed out (run/runenv_hashseed.t)');
like($out, qr/^container=ok$/m,
     'is_linux_container() answers 0 or 1 without dying (op/stat.t)');

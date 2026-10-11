#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# elem-pos-01.t -- task #3063 (and #1567): a //g match whose TARGET is an
# ELEMENT keeps its pos() on the element, as perl keeps it on the element's SV.
#
# PCL passed an element target to the matcher as its VALUE, so the /g state was
# keyed by the string object, while `pos($h{k})` / `pos($r->{k}) = N` read and
# wrote the element's CELL: the reads answered undef and the writes were lost.
# Text::CSV_PP's `__get` (`$ctx->{used} = pos($ctx->{tmp})`) then never
# advanced and `csv(in => FILE, skip_empty_rows => 1)` spun forever.  The
# target of a //g match is now the element's cell -- the live slot when it
# exists, a lazy alias when it does not (a failed match vivifies nothing).
# Every row compares against perl itself.

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

plan tests => 18;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile and run, bounded: a regression here is a HANG, not a wrong line.
sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `timeout -k 5 60 sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    return $output;
}

sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    is(run_cl($code), $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

both_agree('my %h = (t => "abc"); $h{t} =~ /./g; print pos($h{t}) // "undef", "\n";',
           'pos() of a hash element after a //g match on it');
both_agree('my $r = { t => "abc" }; $r->{t} =~ /./g; print pos($r->{t}) // "undef", "\n";',
           '... of a hash-ref element');
both_agree('my @a = ("abc"); $a[0] =~ /./g; my $r = \@a; $r->[0] =~ /./g; print pos($a[0]), "\n";',
           '... of an array element, the same SV through a ref');
both_agree('my $r = { t => "abcd" }; $r->{t} =~ /./g; pos($r->{t}) = 3; $r->{t} =~ /\G(.)/g; print "$1\n";',
           'pos($r->{k}) = N positions the next \G match');
both_agree('sub f { my $m = ($_[0] =~ /\Gabc/gc) ? 1 : 0; "$m/" . (pos($_[0]) // "undef") } '
         . 'my $s = "abcdef"; print f($s), " outer=", pos($s) // "undef", "\n";',
           '#1567: a //g match on $_[0] records pos on the CALLER\'s variable');
both_agree('my $x = { k => "ab" }; my $c = $x->{k}; $x->{k} =~ /a/g; print pos($c) // "undef", "|", pos($x->{k}), "\n";',
           'a copy of the element has its own (undef) pos');
both_agree('my @a; $a[2] =~ /x/g; my %h; $h{k} =~ /x/g; my $r = {}; $r->{k} =~ /x/g; '
         . 'print scalar(@a), (exists $h{k} ? 1 : 0), (exists $r->{k} ? 1 : 0), "\n";',
           'a //g match on a MISSING element vivifies nothing');
both_agree('my $u; my $m = ($u->{k} =~ /x/g) ? 1 : 0; print ref($u), "\n";',
           '... but an undef container is still dereferenced (vivified), as perl does');
both_agree('my %h = (k => "aaa"); my @all = ($h{k} =~ /a/g); print scalar(@all), " ", pos($h{k}) // "undef", "\n";',
           'a LIST-context //g leaves the element\'s pos undef');

# The Text::CSV_PP shape: a scanner that reads its position back with pos().
my $scan = 'my $t0 = time; my $ctx = { tmp => ("ab," x 4000) . "\n" }; my $n = 0;'
         . ' while ((pos($ctx->{tmp}) // 0) < length $ctx->{tmp}) { $ctx->{tmp} =~ /\G(.)/gcs; $n++ }'
         . ' printf "%d %.3f\n", $n, time - $t0;';
$scan = "use Time::HiRes qw(time); $scan";
my ($perl_n, $perl_t) = split ' ', `perl @{[ write_pl($scan) ]} 2>&1`;
my ($pcl_n, $pcl_t)   = split ' ', run_cl($scan);
is($pcl_n, $perl_n, 'a scanner reading pos($h->{k}) back ends (was an endless loop)');
my $bound = 8 * $perl_t + 0.5;
ok(defined $pcl_t && $pcl_t <= $bound,
   sprintf('#3063 timed: the scan within 8x perl + 0.5 s = %.2f s (took %s s, perl %.3f s)',
           $bound, $pcl_t // 'NEVER', $perl_t));

# A write that CHANGES the string resets pos (s515a review, ~/pcl-agent-scratch/s513/review/p515a/iso-pos2.pl):
# a successful s///, s///g, a changing tr///; a no-match s/// and a tr that
# changes nothing keep it.  Scalar, array element, hash element, foreach alias.
both_agree('my $s = "qq"; $s =~ /q/g; $s =~ s/q/Q/; print pos($s) // "undef", "\n";',
           'a successful s/// resets a scalar\'s pos');
both_agree('my $t = "qq"; $t =~ /q/g; $t =~ s/x/y/; print pos($t) // "undef", "\n";',
           '... a no-match s/// keeps it');
both_agree('my @a = ("qq"); $a[0] =~ /q/g; $a[0] =~ s/q/Q/; print pos($a[0]) // "undef", "\n";',
           'a successful s/// resets an array element\'s pos');
both_agree('my @b = ("qq"); $b[0] =~ /q/g; $b[0] =~ s/q/Q/g; my @c = ("qq"); $c[0] =~ /q/g; $c[0] =~ tr/q/Q/; print pos($b[0]) // "undef", " ", pos($c[0]) // "undef", "\n";',
           '... s///g and a changing tr/// reset it too');
both_agree('my $u = "qq"; $u =~ /q/g; $u =~ tr/q/q/; print pos($u) // "undef", "\n";',
           '... a tr/// that changes nothing keeps it');
both_agree('my %h = (k => "qq"); $h{k} =~ /q/g; $h{k} =~ s/q/Q/; print pos($h{k}) // "undef", "\n";',
           'a successful s/// resets a hash element\'s pos');
both_agree('my @f = ("qq"); $f[0] =~ /q/g; for ($f[0]) { s/q/Q/ } my @g = ("qq"); for ($g[0]) { /q/g } $g[0] =~ s/q/Q/; print pos($f[0]) // "undef", " ", pos($g[0]) // "undef", "\n";',
           '... through a foreach alias, in both directions');

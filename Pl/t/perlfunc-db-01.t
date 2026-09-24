#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# perlfunc-db-01.t - the perlfunc-index builtins that did not exist (task #2093,
# s495f): telldir/seekdir, the host / service / protocol / network databases
# (by-name, by-number, and the get*ent iterators), setpriority, and formline's
# ruled die.
#
# Every row but the last is DIFFERENTIAL against real perl on the same machine,
# so the host's own /etc/services, /etc/hosts and resolver are the oracle on
# both sides -- a machine without /etc/services misses on both.  Output lines
# are never blank (run_cl strips blank lines).

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

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = PCLCore::transpile(qq{$pl2cl $pl_file});
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^WARNING:.*\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_diff {
    my ($name, $code) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $perl_out = `perl $file 2>&1`;
    my $cl_out   = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: [$perl_out]\nCL:   [$cl_out]");
}

test_diff('telldir/seekdir resume on the same handle; closed handle is undef',
    'opendir my $d, "/etc" or die; my $e = readdir $d; my $pos = telldir $d;
     my $n1 = readdir $d; my $r = seekdir $d, $pos; my $n2 = readdir $d;
     print "seekdir [$r] ", ($n1 eq $n2 ? "same" : "differ"), "\n";
     my @rest = readdir $d; seekdir $d, $pos; my $n3 = readdir $d;
     print "after-list ", ($n3 eq $n1 ? "same" : "differ"), "\n";
     opendir D, "/etc"; readdir D; my $t = telldir D; my $b1 = readdir D;
     seekdir D, $t; print "bareword ", ($b1 eq readdir(D) ? "same" : "differ"), "\n";
     closedir $d; my $c = telldir $d; my $s = seekdir $d, 0;
     print "closed ", (defined $c ? "def" : "undef"), " ", (defined $s ? "def" : "undef"), "\n";');

test_diff('gethostbyname/gethostbyaddr in list and scalar context, misses, inet_aton of a name',
    'use Socket;
     my @h = gethostbyname("localhost");
     print scalar(@h), " [$h[0]] [$h[1]] $h[2] $h[3] ", inet_ntoa($h[4]), "\n";
     my $s = gethostbyname("localhost"); print length($s), " ", inet_ntoa($s), "\n";
     my @m = gethostbyname("no-such-host.invalid"); my $m = gethostbyname("no-such-host.invalid");
     print "miss ", scalar(@m), " ", (defined $m ? "def" : "undef"), "\n";
     my $n = gethostbyaddr(inet_aton("127.0.0.1"), AF_INET); print "byaddr [$n]\n";
     my ($first) = gethostbyaddr(inet_aton("127.0.0.1"), AF_INET); print "listassign [$first]\n";
     print "aton ", inet_ntoa(inet_aton("localhost")), "\n";
     print "aton-miss ", (defined inet_aton("no-such-host.invalid") ? "def" : "undef"), "\n";');

test_diff('getservbyname/getservbyport: aliases, protocols, scalar context, misses',
    'my @s = getservbyname("http", "tcp"); print scalar(@s), " [", join("|", @s), "]\n";
     print "scalar-byname [", scalar(getservbyname("http", "tcp")), "]\n";
     print "byport [", join("|", getservbyport(22, "tcp")), "]\n";
     print "scalar-byport [", scalar(getservbyport(22, "tcp")), "]\n";
     print "alias [", join("|", getservbyname("www", "tcp")), "]\n";
     print "empty-proto [", join("|", getservbyname("ssh", "")), "]\n";
     my @x = getservbyname("http", "udp"); my @c = getservbyname("HTTP", "tcp");
     print "misses ", scalar(@x), " ", scalar(@c), " ",
       (defined(scalar getservbyname("nosuch", "tcp")) ? "def" : "undef"), "\n";
     sub f { return getservbyname("ssh", "tcp") } my $r = f(); print "sub-scalar [$r]\n";');

test_diff('the get*ent iterators: protocols, services, hosts, networks; set*/end* rewind',
    'my ($i, $j, $k) = (0, 0, 0);
     while (my @p = getprotoent()) { $i++; print "proto [", join("|", @p), "]\n" if $i < 3 }
     setprotoent(0); print "proto-scalar [", scalar(getprotoent()), "] n=$i\n"; endprotoent();
     while (my @s = getservent()) { $j++; print "serv [", join("|", @s), "]\n" if $j < 3 }
     print "serv-set [", setservent(1), "] [", scalar(getservent()), "] n=$j\n"; endservent();
     while (my @h = gethostent()) { $k++; print "host [$h[0]] $h[2] $h[3]\n" if $k < 3 }
     print "host-end [", endhostent(), "] [", sethostent(0), "]\n";
     my @n = getnetent(); print "net ", scalar(@n), "\n"; setnetent(0); endnetent();
     print "netbyaddr ", scalar(my @na = getnetbyaddr(127, 2)), "\n";');

test_diff('setpriority returns 1 on success and 0 with $! on failure',
    'my $p = getpriority(0, 0);
     print "same [", setpriority(0, 0, $p), "]\n";
     print "up [", setpriority(0, 0, $p + 1), "] now ", getpriority(0, 0) - $p, "\n";
     my $r = setpriority(99, 0, 0); print "bad-which [$r] ", ($! + 0), "\n";');

# A WIDE character (> 0xFF) in a name/address dies "Wide character in NAME" in
# every lookup (t/op/ver.t's `Non-bytes leak to gethostbyaddr' row); a byte
# string still looks up (the inverse guard: the last case).
test_diff('host/service/protocol/network lookups die on a wide character',
    'for my $c (sub { gethostbyaddr(v2004.148.0.1, 2) }, sub { gethostbyname("\x{100}x") },
                sub { getservbyname("\x{100}", "tcp") }, sub { getprotobyname("\x{100}") },
                sub { getnetbyname("\x{100}") }, sub { getservbyport(22, "\x{100}") },
                sub { getservbyname("ssh", "tcp") }) {
       eval { $c->() }; print "[", ($@ =~ /Wide character in (\w+)/ ? $1 : "none"), "]\n" }');

# formline is RULED with format/write (docs/not-supported.md, the format
# section): perl fills $^A, PCL dies -- one trappable line.  NOT differential.
like(run_cl('my $ok = eval { formline("@<<< @>>>\n", "ab", "cd"); 1 };
             print $ok ? "lived\n" : "died: $@";'),
     qr/^died: PCL: formline \(format\/write report formatting\) is not supported$/m,
     'formline dies with the ruled message instead of leaving $^A empty');

done_testing();

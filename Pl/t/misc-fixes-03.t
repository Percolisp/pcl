#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# misc-fixes-03.t -- small fixes with their guard rows (s513f fillers), each
# compared against perl's STDOUT.  No 5.40 syntax: CI's perl is 5.38.

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

plan tests => 7;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "no warnings;\n$code";
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
    my $output = `sbcl @sbcl_rt --load $cl_file 2>/dev/null`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub both_agree {
    my ($code, $desc) = @_;
    my $file = write_pl($code);
    my $perl = scalar `perl $file 2>/dev/null`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- #2982: `^` under /m does not match after a newline that ENDS the string

both_agree(q{for my $s ("", "\n", "x\n", "x\ny", "x\ny\n", "\n\n") {
  my @m = ($s =~ /^/mg); my $u = $s; $u =~ s/^/> /mg; my @f = split /^/m, $s; my @g = split /^/, $s;
  (my $v = $s) =~ s/^(.?)/[$1]/mg; my @p; while ($s =~ /^/mg) { push @p, pos($s) }
  print scalar(@m), " ", ($u =~ s/\n/N/gr), " ", scalar(@f), "/", scalar(@g), " ", ($v =~ s/\n/N/gr), " p=@p\n";
}},
           '#2982 /m `^`: counts, s///mg, split, pos over six strings');

both_agree(q{print "a\n" =~ /a\n^/m ? "m1\n" : "m0\n"; print "x\n" =~ /^\z/m ? "z1\n" : "z0\n";
print "x\ny" =~ /(?m:^y)/ ? "g1\n" : "g0\n"; print "x\n" =~ /x\n(?m)^/ ? "f1\n" : "f0\n"; print "a\nb" =~ /\n^b/ ? "n1\n" : "n0\n";},
           '#2982 /m `^` at the end, in a (?m:) group, after (?m), and without /m');

# ---- #2924: a glob or glob ref as truncate handle --------------------
both_agree(q{my $f = "/tmp/pcl_mf03_$$.txt"; sub mk { open my $w, ">", $f or die; print $w "abcdef"; close $w }
mk(); open(TFH, "+<", $f) or die; my $r = truncate(\*TFH, 0); close TFH; print "r=", ($r ? 1 : 0), " size=", -s $f, "\n";
mk(); open(TFH, "+<", $f) or die; $r = truncate(*TFH, 2); close TFH; print "r=", ($r ? 1 : 0), " size=", -s $f, "\n"; unlink $f;},
           "#2924 truncate(\\*FH, N) and truncate(*FH, N) truncate");

# ---- #2922: $! after a failing opendir / seek ----------------------------
both_agree(q{$! = 0; opendir(my $d, "/nonexistent/q") or print "od: $!\n";
$! = 0; opendir(my $e, $0) or print "od2: $!\n";
open(my $g, "<", $0) or die; $! = 0; seek($g, -5, 0) or print "seek: $!\n"; close $g; $! = 0; seek($g, 0, 0) or print "closed: $!\n";},
           "#2922 \$! after a failing opendir (ENOENT, ENOTDIR) and seek (EINVAL, EBADF)");

# ---- #2921: the read / sysread buffer as an element or a deref place ------
both_agree(q{open my $g, "<", $0; my %h; my @a; read($g, $h{x}, 3); read($g, $a[2], 3); my $hr = {}; read($g, $hr->{y}, 3);
sub s2 { read($_[0], ${$_[1]}, 3) } my $d; s2($g, \$d); sub s3 { sysread($_[0], ${$_[1]}, 2) } my $e; sysseek($g, 0, 0); s3($g, \$e);
$h{z} = "XY"; seek($g, 0, 0); read($g, $h{z}, 2, 1); print "$h{x}|$a[2]|$hr->{y}|$d|$e|$h{z}\n";},
           "#2921 read into \$h{k} / \$a[i] / \$r->{k} / \${\$_[1]}, sysread, an offset read");

# ---- #2943: the buffer PLACE's subscript is evaluated ONCE ----------------
both_agree(q{open my $fh, '<', \"abcdef" or die; my @b; my $i = 0;
read($fh, $b[$i++], 1); print "i=$i b0=", ($b[0]//'U'), " b1=", ($b[1]//'U'), "\n";
my %h; my $k = 0; read($fh, $h{$k++ . "x"}, 1); print "k=$k ", join(",", map {"$_=$h{$_}"} sort keys %h), "\n";
my $n = 0; sub f { $n++; 0 } my $r = []; read($fh, $r->[f()], 1); print "n=$n r0=", ($r->[0]//'U'), " r1=", ($r->[1]//'U'), "\n";
my $hr = {}; my $j = 0; read($fh, $hr->{$j++}, 2, 1); print "j=$j ", join(",", map {"$_=" . length($hr->{$_})} sort keys %$hr), "\n";
my $s = "zz"; my $sr = \$s; my $m = 0; sub g { $m++; $sr } read($fh, ${g()}, 1); print "m=$m s=$s\n";
open my $f2, '<', $0 or die; my @c; my $q = 0; sysread($f2, $c[$q++], 1);
print "q=$q c0=", (defined $c[0] ? 1 : 0), " c1=", (defined $c[1] ? 1 : 0), "\n";
my $h2 = {}; read($fh, $h2->{a}{b}, 1); print "viv=", ref($h2->{a}), "\n";},
           "#2943 read / sysread into \$b[\$i++], \$h{\$k++ . 'x'}, \$r->[f()], \${g()}: the subscript runs once");
both_agree(q{use Socket; socketpair(my $x, my $y, AF_UNIX, SOCK_STREAM, 0) or die; syswrite($x, "xy");
my @r; my $i = 0; recv($y, $r[$i++], 1, 0); print "i=$i r0=$r[0]\n";},
           "#2943 recv into \$r[\$i++]: the subscript runs once");

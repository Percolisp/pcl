#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# opendir-01.t -- task #3243 (s516a): `opendir` on a COPY of a glob ref
# (`my ($dh) = @_` where the caller passed a gensym or `\*DIR`) opens the
# GLOB, as `open` does -- PCL stored the dirhandle in the copy's box and the
# caller's glob read NOTHING, so core IO::Dir->new(DIR)->read was empty.  The
# one key is cl/pcl-runtime.lisp %p-typeglob-handle-key (open's) through
# %p-dh-key (opendir / readdir / rewinddir / closedir).  Every row compares
# stdout with perl itself over a real directory.

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

plan tests => 8;

my $d = tempdir(CLEANUP => 1);
for my $n ('a b.txt', 'c.txt', 'e.txt') {
    open my $fh, '>', "$d/$n" or die "$d/$n: $!";
    close $fh;
}

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "use Symbol;\nmy \$d = '$d';\nsub op { my (\$dh, \$n) = \@_; return opendir(\$dh, \$n) }\n$code";
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($code) = @_;
    my $cl = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>/dev/null`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    return $output;
}

sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>/dev/null`;
    is(run_cl($code), $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

both_agree('my $g = gensym; op($g, $d) or die; my @a = sort readdir($g); print "@a\n";',
           'opendir on a COPY of a gensym (a sub argument): the caller reads the entries');
both_agree('op(\*DH, $d) or die; my @a = sort readdir(DH); print "@a\n"; print closedir(DH) ? "closed\n" : "not\n";',
           'opendir on a copy of \*DH: the bareword DH reads and closes it');
both_agree('my $g = gensym; op($g, $d); my $x = readdir($g); my $y = readdir($g); rewinddir($g);'
           . ' my @r = readdir($g); print "$x ", scalar(@r), "\n";',
           '... scalar readdir and rewinddir on the original glob');
both_agree('my $g = gensym; op($g, $d); print defined(closedir($g)) ? 1 : 0, defined(closedir($g)) ? 1 : 0, "\n";',
           '... closedir closes it once (1, then undef)');
both_agree('sub op2 { opendir($_[0], $_[1]) } my $h; op2($h, $d) or die; my @a = sort readdir($h); print "@a\n";',
           'INVERSE: opendir($_[0], ...) aliases the caller\'s scalar (worked before)');
both_agree('opendir(my $m, $d) or die; my @a = sort readdir $m; print "@a\n"; closedir $m;',
           'opendir(my $dh, ...) autovivifies');
both_agree('use IO::Dir; my $io = IO::Dir->new($d) or die; my @a = sort $io->read; print "@a\n";',
           'core IO::Dir->new(DIR)->read lists the entries');
both_agree('my $g = gensym; my $c = $g; op($c, $d); opendir(my $o, "/") or die; my @a = sort readdir($g); print "@a\n";',
           'a SECOND copy: opening another handle leaves the glob\'s');

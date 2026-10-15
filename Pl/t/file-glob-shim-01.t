#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# file-glob-shim-01.t -- task #3160 (s514e): `use File::Glob` works without
# XS, and CORE::glob is perl's bsd_glob with the csh flags.
#
# lib/File/Glob.pm is plain Perl over ONE runtime glob engine
# (builtin::bsd_glob, the engine CORE::glob runs).  Before it, `use
# File::Glob` died "the module is XS and has no PCL build"; and CORE::glob
# had no brace expansion (#488), no backslash quoting, sorted in ASCII
# instead of case-insensitive order, and never listed `.` / `..` -- so every
# row here fails on the base.
#
# Each program prints one LABELLED line per row, compared with the same
# program under perl (File::Glob is core; nothing needs perl 5.40).  Paths are
# printed relative to the program's own temp directory.

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

plan skip_all => "pl2cl not found" if !-x $pl2cl;
plan skip_all => "sbcl not found"  if !`which sbcl 2>/dev/null`;

my $HELPERS = <<'H';
use strict; no warnings;
use File::Temp qw(tempdir);
my $D = tempdir(CLEANUP => 1);
for my $n (qw(b.txt a.txt .hid c.TXT B.c a.c), "sp ace.txt", "x{1}.txt", "st*r") {
    open my $f, '>', "$D/$n" or die "$n: $!"; close $f;
}
mkdir "$D/sub"; mkdir "$D/sub/deep";
open my $f1, '>', "$D/sub/s.txt" or die; close $f1;
open my $f2, '>', "$D/sub/deep/d.txt" or die; close $f2;
chdir $D or die;
sub rel { join ',', map { s{^\Q$D\E/?}{}r } @_ }
sub row { my ($l, @v) = @_; print "$l: ", join(' | ', @v), "\n" }
H

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $HELPERS, $code;
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($file) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl $file");
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>/dev/null`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    return $output;
}

sub compare_lines {
    my ($code, $what) = @_;
    my $file = write_pl($code);
    my @perl = split /\n/, scalar `perl $file 2>/dev/null`;
    my %pcl;
    for my $l (split /\n/, run_cl($file)) {
        $pcl{$1} = $l if $l =~ /^(.+?): /;
    }
    for my $l (@perl) {
        my ($label) = $l =~ /^(.+?): /;
        is($pcl{$label} // "(no '$label' line)", $l, "$what: $label");
    }
}

# 1. bsd_glob and its flags.
compare_lines(<<'P', 'bsd_glob');
use File::Glob qw(:glob csh_glob);
row('plain',      rel(bsd_glob("$D/*.txt")));
row('relative',   rel(bsd_glob("*.c")));
row('nocase',     rel(bsd_glob("$D/*.txt", GLOB_NOCASE)));
row('nocase lit', rel(bsd_glob("$D/A.TXT", GLOB_NOCASE)));
row('mark',       rel(bsd_glob("$D/s*", GLOB_MARK)));
row('brace',      rel(bsd_glob("$D/{b,a}.txt", GLOB_BRACE)));
row('brace nest', rel(bsd_glob("$D/{a.{txt,c},b.txt}", GLOB_BRACE)));
row('no brace',   scalar(() = bsd_glob("$D/{a,b}.txt", 0)));
row('none',       scalar(() = bsd_glob("$D/nope*")));
row('nocheck',    rel(bsd_glob("$D/nope*", GLOB_NOCHECK)));
row('nomagic',    rel(bsd_glob("$D/nope", GLOB_NOMAGIC)), scalar(() = bsd_glob("$D/nope*", GLOB_NOMAGIC)));
row('no flags',   scalar(() = bsd_glob("$D/nope", 0)));
row('quote',      rel(bsd_glob("$D/st\\*r", GLOB_QUOTE)), rel(bsd_glob("$D/n\\*o", GLOB_QUOTE | GLOB_NOCHECK)));
row('space',      rel(bsd_glob("$D/sp ace.txt")));
row('dots',       join(',', sort map { s{.*/}{}r } bsd_glob("$D/.*")));
row('dir wild',   rel(bsd_glob("$D/s*/*.txt")), rel(bsd_glob("$D/*/*/*.txt")));
row('class',      rel(bsd_glob("$D/[ab].txt")), rel(bsd_glob("$D/[!a].txt")));
row('alphasort',  rel(bsd_glob("$D/*.c", GLOB_ALPHASORT)), rel(bsd_glob("$D/*.c", 0)));
row('csh',        scalar(my @c = csh_glob("$D/*.txt $D/*.c")));
row('error',      GLOB_ERROR(), scalar(() = bsd_glob("/nonexistent-pcl-dir/*", GLOB_ERR)), GLOB_ERROR());
row('consts',     GLOB_BRACE + 0, GLOB_CSH, GLOB_NOSPACE, GLOB_ABEND);
row('tilde',      (bsd_glob("~", GLOB_TILDE))[0] eq $ENV{HOME} ? 'home' : 'other');
P

# 2. CORE::glob is csh_glob: braces, quoting, case-insensitive order, words.
compare_lines(<<'P', 'CORE::glob');
row('order',     rel(glob("*.c")), rel(glob("$D/*.c")));
row('braces',    rel(glob("{b,a}.txt")), rel(glob("x{1}.txt")));
row('unbalanced', join(',', glob("{x")), join(',', glob("{}x")));
row('words',     rel(glob("a.txt b.txt")));
row('quote',     rel(glob("st\\*r")));
row('angle',     rel(<*.c>));
my @it; while (my $f = glob("*.c")) { push @it, $f } row('scalar', rel(@it));
P

# 3. `:bsd_glob` makes the caller's glob one pattern (spaces kept).
compare_lines(<<'P', ':bsd_glob');
use File::Glob qw(:bsd_glob);
row('one pattern', rel(glob("sp ace.txt")), scalar(() = glob("a.txt b.txt")));
row('angle',       rel(<sp ace.txt>));
my $p = "*.c";
row('interp',      rel(<$D/$p>));
my @it; while (my $f = <*.c>) { push @it, $f } row('scalar', rel(@it));
P

# 4. DynaLoader's `bootstrap` with no loadable object DIES as perl does (it
# was a silent no-op: Compress::Raw::Zlib's fallback `bootstrap` then
# "succeeded" and its AUTOLOAD recursed through the missing XS constant()
# until SBCL's binding stack was exhausted -- a crash, not a die).
compare_lines(<<'P', 'bootstrap');
open my $m, '>', "$D/NoXsPcl.pm" or die;
print $m "package NoXsPcl; require DynaLoader; our \@ISA = ('DynaLoader');\n",
         "bootstrap NoXsPcl; 1;\n";
close $m;
unshift @INC, $D;
my $ok = eval { require NoXsPcl; 1 };
row('no artifact', $ok ? 'loaded' : ($@ =~ /\ACan't locate/ ? 'dies-locate' : "other: $@"));
P

# 4. bsd_glob in SCALAR context answers the LAST element, never the count
# (#3246, s515c: Path-Tiny basic.t computes tilde expansion this way).
compare_lines(<<'P', 'bsd_glob scalar');
use File::Glob ();
my $one = File::Glob::bsd_glob("{root}");
row('scalar one', $one);
my $last = File::Glob::bsd_glob("$D/*.txt");
row('scalar many', rel($last));
my $none = File::Glob::bsd_glob("$D/nope*");
row('scalar none', defined $none ? "[$none]" : 'undef');
P

{
    # `use Compress::Zlib` gets past File::GlobMapper (this file's shim) and
    # LOADS, as in perl, since s517b's Compress::Raw::Zlib shim over libz
    # (#3161; it used to die in one line at Compress::Raw::Zlib, which this
    # row asserted).  On a box WITHOUT libz the shim dies at load in ONE
    # perl-shaped line, which is the other answer this row accepts.
    my $file = write_pl(<<'P');
my $ok = eval "use Compress::Zlib; 1";
my @l = split /\n/, $@;
print "zlib: ", ($ok ? 'loaded' : scalar(@l) . ' ' . ($@ =~ /the system zlib \(libz\.so\.1\) is not available/ ? 'nolibz' : 'other')), "\n";
P
    my ($got) = run_cl($file) =~ /^(zlib: .*)$/m;
    like($got // '(no line)', qr/\Azlib: (?:loaded|1 nolibz)\z/,
         'use Compress::Zlib loads (or, without libz, dies in one line at Compress::Raw::Zlib)');
}

done_testing;

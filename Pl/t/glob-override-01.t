#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# glob-override-01.t -- tasks #2879 + #3206 (s516a): `use File::Glob
# ':bsd_glob'` displaces the builtin glob in EVERY spelling perl accepts, and
# the detector table never lists an override's call site as "builtin".
#
#   #2879: the import list of ONE quoted string (`use M ':tag'`) is the same
#          import as its qw() spelling (Pl::Parser::bare_quote_import_names,
#          the one reading the prototype merge and the override recorder
#          share).  Before: the quoted spelling kept the builtin p-glob and
#          csh word-splitting -- `glob("$d/a b.txt")` found 2 files, perl 1.
#   #3206: under the qw() spelling the emission already called the override,
#          yet the table registered each site as ("main" "glob" N "builtin"),
#          so PCL_DETECT=announce reported every glob() as displaced.
# Every run row compares stdout + the detector's stderr lines with perl's
# stdout, so a detector line fails it.

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

plan tests => 10;

# The fixture directory: a space in a file name, and two plain files.
my $d = tempdir(CLEANUP => 1);
for my $n ('a b.txt', 'c.txt', 'e.txt') {
    open my $fh, '>', "$d/$n" or die "$d/$n: $!";
    close $fh;
}

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "my \$d = '$d';\n$code";
    close $fh;
    return $pl_file;
}

sub transpile { PCLCore::transpile("$pl2cl " . write_pl($_[0])) }

# Run with the detector ANNOUNCING; returns stdout . the detector's lines.
sub run_cl {
    my ($code) = @_;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh transpile($code);
    close $cl_fh;
    my (undef, $err_file) = tempfile(SUFFIX => '.err', UNLINK => 1);
    local $ENV{PCL_DETECT} = 'announce';
    my $output = `sbcl @sbcl_rt --load $cl_file 2>$err_file`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    open my $e, '<', $err_file or die;
    return $output . join '', grep { /^PCL: .*was parsed as/ } <$e>;
}

sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    is(run_cl($code), $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

my $shapes = <<'P';
my @l = glob("$d/a b.txt");         print "list space: ", scalar(@l), "\n";
my $s = glob("$d/a b.txt");         print "scalar space: ", (defined $s ? $s =~ s{.*/}{}r : 'undef'), "\n";
my @w = glob("$d/c.txt $d/e.txt");  print "list words: ", scalar(@w), "\n";
my $i = 0; while (my $f = glob("$d/*.txt")) { $i++; last if $i > 5 } print "iter: $i\n";
P

# ---- #2879: the quoted-string spelling IS the import ----------------------
both_agree("use File::Glob ':bsd_glob';\n$shapes",
           "use File::Glob ':bsd_glob' (one quoted string): list, scalar, words, iterator");
both_agree(qq{use File::Glob ":bsd_glob";\n$shapes},
           '... a double-quoted string');
both_agree("use File::Glob (':bsd_glob');\n$shapes",
           '... a parenthesised list of strings');

# ---- #3206: the qw() spelling, values and NO detector line ----------------
both_agree("use File::Glob qw(:bsd_glob);\n$shapes",
           'use File::Glob qw(:bsd_glob): perl values, no detector line');
both_agree("use File::Glob qw(:bsd_glob);\n" . 'my @l = <$d/a b.txt>; my @w = <$d/c.txt $d/e.txt>; '
           . 'print scalar(@l), " ", scalar(@w), "\n";',
           '... the <PATTERN> spelling');

# ---- the transpile shape: an override's site is not a "builtin" entry -----
for my $use ("use File::Glob qw(:bsd_glob);", "use File::Glob ':bsd_glob';") {
    my $cl = transpile("$use\nmy \@l = glob(\"\$d/*\");\n");
    ok($cl =~ /\(pl-glob / && $cl !~ /"glob" \d+ "builtin"/,
       "SHAPE: `$use` -- the site calls the override and its table entry is not \"builtin\"");
}

# ---- negatives: the builtin stays where nothing displaced it --------------
both_agree($shapes, 'NEGATIVE: no use File::Glob -- the builtin, csh words');
both_agree("use File::Glob qw(bsd_glob);\n$shapes"
           . 'my @b = bsd_glob("$d/c.txt $d/e.txt"); print "bsd: ", scalar(@b), "\n";',
           'NEGATIVE: use File::Glob qw(bsd_glob) without the tag -- glob stays the builtin');
my $cl = transpile("use File::Glob qw(bsd_glob);\nmy \@l = glob(\"\$d/*\");\n");
ok($cl =~ /\(p-glob / && $cl =~ /"glob" \d+ "builtin"/,
   'SHAPE NEGATIVE: without the tag the site is the builtin p-glob, registered "builtin"');

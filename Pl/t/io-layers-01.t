#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Task #1115 — a filehandle carries OCTETS unless a layer says otherwise.
#
# perl's default I/O discipline is BYTES: `open $fh,'<',$f` reads one CHARACTER
# PER OCTET, so `length` on the slurp is the file's size.  Before #1115 PCL
# opened every handle with SBCL's default (UTF-8), so every read of a non-ASCII
# file measured characters — the widest silent divergence PCL had left, since
# any byte-oriented use of the result (a size, a seek offset, a checksum, a
# re-write) was then wrong.
#
# Every expectation here is perl 5.40.3's own answer, measured on the fixture
# this file writes (15 octets: "abc" U+00E9 "def" U+2019 "ghi\n").  The INVERSE
# guard: on an `f728637` worktree rows 1/2/6/7/8 fail (len 12 where perl says
# 15, and the round-trip writes double-encoded octets).

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

# The fixture: 15 OCTETS, two of the characters multi-byte in UTF-8.
my $dir  = tempdir(CLEANUP => 1);
my $data = "$dir/data.txt";
{
    open my $fh, '>:raw', $data or die "open $data: $!";
    print $fh "abc\xc3\xa9def\xe2\x80\x99ghi\n";
    close $fh;
}
is(-s $data, 15, 'fixture is 15 octets on disk');

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
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^WARNING:.*\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# Slurp $data through MODE (plus an optional binmode call) and report the
# length and the 4th character's ordinal — the two numbers that separate
# octets from characters.
sub slurp_report {
    my ($mode, $binmode) = @_;
    my $bm = defined $binmode ? "binmode(\$fh, $binmode);" : '';
    return run_cl(<<"PL");
open my \$fh, '$mode', '$data' or die "open: \$!";
$bm
my \$s = do { local \$/; <\$fh> };
close \$fh;
printf "len=%d ord3=%d\\n", length(\$s), ord(substr(\$s,3,1));
PL
}

is(slurp_report('<'),      "len=15 ord3=195\n", 'default open reads OCTETS');
is(slurp_report('<:raw'),  "len=15 ord3=195\n", ':raw reads octets');
is(slurp_report('<:utf8'), "len=12 ord3=233\n", ':utf8 in the mode DECODES');
is(slurp_report('<:encoding(UTF-8)'), "len=12 ord3=233\n",
   ':encoding(UTF-8) in the mode decodes');
is(slurp_report('<', "':utf8'"), "len=12 ord3=233\n",
   "binmode(\$fh,':utf8') decodes a handle opened as bytes");
is(slurp_report('<', "':encoding(UTF-8)'"), "len=12 ord3=233\n",
   "binmode(\$fh,':encoding(UTF-8)') decodes");
is(slurp_report('<:encoding(UTF-8)', undef), "len=12 ord3=233\n",
   'the decoding open on its own still decodes');
is(slurp_report('<:encoding(UTF-8)', "':raw'"), "len=15 ord3=195\n",
   "binmode(\$fh,':raw') pops a decoding handle back to octets");

# `binmode($fh)` with no layer POPS back to bytes (probed 5.40.3).
is(run_cl(<<"PL"), "len=15 ord3=195\n", 'bare binmode() pops back to OCTETS');
open my \$fh, '<:encoding(UTF-8)', '$data' or die;
binmode(\$fh);
my \$s = do { local \$/; <\$fh> };
close \$fh;
printf "len=%d ord3=%d\\n", length(\$s), ord(substr(\$s,3,1));
PL

# A byte-for-byte round trip: read default, write default.  This is the shape
# every CPAN module that rewrites a file has, and it was silently corrupting
# the two multi-byte characters before #1115.
is(run_cl(<<"PL"), "out=15 same=1\n", 'read-then-write round-trips byte for byte');
open my \$r, '<', '$data' or die; my \$s = do { local \$/; <\$r> }; close \$r;
open my \$w, '>', '$dir/rt.bin' or die; print \$w \$s; close \$w;
open my \$c, '<:raw', '$dir/rt.bin' or die; my \$d = do { local \$/; <\$c> }; close \$c;
open my \$o, '<:raw', '$data' or die; my \$e = do { local \$/; <\$o> }; close \$o;
printf "out=%d same=%d\\n", length(\$d), (\$d eq \$e ? 1 : 0);
PL

# tell/-s/read/getc all count the same units as length now.
is(run_cl(<<"PL"), "tell=15 size=15 read=5 ord4=195\n", q{tell, -s, read and getc all count OCTETS on a byte handle});
open my \$fh, '<', '$data' or die;
my \$line = <\$fh>;
my \$t = tell(\$fh);
close \$fh;
open my \$g, '<', '$data' or die; my \$buf = ''; my \$n = read(\$g, \$buf, 5); close \$g;
printf "tell=%d size=%d read=%d ord4=%d\\n", \$t, -s '$data', \$n, ord(substr(\$buf,3,1));
PL

# `use open qw(:std :utf8)` moves the default for BOTH directions and the
# standard handles.  A wide character then reaches STDOUT without a warning.
is(run_cl(<<"PL"), "len=12 ord3=233\n", 'use open qw(:std :utf8) makes the default DECODE');
use open qw( :std :utf8 );
open my \$fh, '<', '$data' or die;
my \$s = do { local \$/; <\$fh> };
close \$fh;
printf "len=%d ord3=%d\\n", length(\$s), ord(substr(\$s,3,1));
PL

is(run_cl(<<"PL"), "len=15 ord3=195\n", 'an explicit :raw still overrides use open');
use open qw( :std :utf8 );
open my \$fh, '<:raw', '$data' or die;
my \$s = do { local \$/; <\$fh> };
close \$fh;
printf "len=%d ord3=%d\\n", length(\$s), ord(substr(\$s,3,1));
PL

# A WIDE character on a byte handle: perl warns once per ARGUMENT and writes
# the string's whole UTF-8 encoding — the \x{e9} becomes TWO octets although
# one would hold it, because perl decides per SV, not per character.
is(run_cl(<<"PL"), "bytes=c3a9e28099 warns=2\n", q{a wide character upgrades the WHOLE string and warns once per argument});
my \@w;
local \$SIG{__WARN__} = sub { push \@w, \$_[0] };
my \$s = "\\x{e9}\\x{2019}";
open my \$fh, '>', '$dir/wide.bin' or die;
print \$fh \$s, \$s;
close \$fh;
open my \$r, '<:raw', '$dir/wide.bin' or die; my \$d = do { local \$/; <\$r> }; close \$r;
printf "bytes=%s warns=%d\\n", join('', map { sprintf '%02x', ord } split //, substr(\$d,0,5)),
       scalar(grep { /Wide character/ } \@w);
PL

# ...and a string that FITS in octets is written as octets, with no warning.
is(run_cl(<<"PL"), "bytes=e9 warns=0\n", q{a string that fits in octets is written as octets and does not warn});
my \@w;
local \$SIG{__WARN__} = sub { push \@w, \$_[0] };
open my \$fh, '>', '$dir/nar.bin' or die;
print \$fh "\\x{e9}";
close \$fh;
open my \$r, '<:raw', '$dir/nar.bin' or die; my \$d = do { local \$/; <\$r> }; close \$r;
printf "bytes=%s warns=%d\\n", join('', map { sprintf '%02x', ord } split //, \$d),
       scalar(grep { /Wide character/ } \@w);
PL

# perl makes syswrite FATAL where print warns (probed 5.40.3).
like(run_cl(<<"PL"), qr/died=Wide character in syswrite/, q{syswrite of a wide character DIES, as perl does});
open my \$fh, '>', '$dir/sw.bin' or die;
my \$n = eval { syswrite(\$fh, "a\\x{2019}b") };
print "died=\$@" if \$@;
close \$fh;
PL

# The emission: `use open` is the one never-loaded pragma with a runtime effect.
{
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "use open qw(:std :utf8);\nprint \"x\\n\";\n";
    close $fh;
    my $cl = PCLCore::transpile(qq{$pl2cl $pl_file});
    like($cl, qr/\(p-use-open \(vector ":std" ":utf8"\)\)/,
         'use open lowers to a p-use-open call with its list');
}

# --- #1222: `use open` is LEXICAL ------------------------------------------
#
# perl's open pragma is compile-time and LEXICAL, so a module's own `use open`
# must not touch its CALLER's I/O and a block-scoped one must not leak past the
# block.  A runtime global got both wrong, and #1115 is what made it a silent
# WRONG rather than a silent no-op: once bytes are the default, any CPAN module
# carrying `use open` flips its caller's reads.  Every expectation is perl
# 5.40.3's own answer on an 11-octet file that is 8 characters decoded.
{
    my $lib = "$dir/lib";
    mkdir $lib or die "mkdir $lib: $!";
    open my $m, '>', "$lib/UOpen1222.pm" or die "write UOpen1222: $!";
    print $m <<'PM';
package UOpen1222;
use open qw(:utf8);
sub slurp {
    my ($p) = @_;
    open my $fh, '<', $p or die "UOpen1222: $!";
    my $s = do { local $/; <$fh> };
    close $fh;
    return length($s);
}
1;
PM
    close $m;
    my $d11 = "$dir/d11.txt";
    open my $w, '>:raw', $d11 or die "write d11: $!";
    print $w "ab\xc3\xa9de\xe2\x80\x99fg";     # 11 octets, 8 characters
    close $w;

    my $mod = run_cl(<<"PL");
use lib "$lib";
use UOpen1222;
sub plain { open my \$fh, '<', \$_[0] or die; my \$s = do { local \$/; <\$fh> }; close \$fh; length(\$s) }
print "main-plain: ", plain("$d11"), "\\n";
print "module-utf8: ", UOpen1222::slurp("$d11"), "\\n";
print "after: ", plain("$d11"), "\\n";
PL
    is($mod, "main-plain: 11\nmodule-utf8: 8\nafter: 11\n",
       q{#1222 a module's own `use open` does NOT reach its caller's open});

    my $blk = run_cl(<<"PL");
{
    use open qw(:utf8);
    open my \$a, '<', "$d11" or die;
    my \$s = do { local \$/; <\$a> };
    close \$a;
    print "in-block: ", length(\$s), "\\n";
}
open my \$b, '<', "$d11" or die;
my \$s2 = do { local \$/; <\$b> };
close \$b;
print "after-block: ", length(\$s2), "\\n";
PL
    is($blk, "in-block: 8\nafter-block: 11\n",
       q{#1222 a block-scoped `use open` does not leak past the block});

    my $nest = run_cl(<<"PL");
{
    use open qw(:utf8);
    open my \$c, '<', "$d11" or die; my \$s = do { local \$/; <\$c> }; close \$c;
    print "outer: ", length(\$s), "\\n";
    {
        use open IN => ':raw';
        open my \$d, '<', "$d11" or die; my \$t = do { local \$/; <\$d> }; close \$d;
        print "nested-raw: ", length(\$t), "\\n";
    }
    open my \$e, '<', "$d11" or die; my \$u = do { local \$/; <\$e> }; close \$e;
    print "back: ", length(\$u), "\\n";
}
PL
    is($nest, "outer: 8\nnested-raw: 11\nback: 8\n",
       q{#1222 a nested `use open IN => ':raw'` overrides only the direction it names});

    # `no open` is a NO-OP in perl: open.pm defines `import` and NO `unimport`
    # (probed 5.40.3 — this prints 8, and `no open ':utf8'` is FATAL there).
    my $noopen = run_cl(<<"PL");
{
    use open qw(:utf8);
    no open;
    open my \$h, '<', "$d11" or die; my \$s = do { local \$/; <\$h> }; close \$h;
    print "no-open: ", length(\$s), "\\n";
}
PL
    is($noopen, "no-open: 8\n",
       q{#1222 `no open` does NOT end the region — perl's open.pm has no unimport});
}

done_testing();

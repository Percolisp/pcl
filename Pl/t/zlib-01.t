#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# zlib-01.t -- task #3161 (s517b): Compress::Raw::Zlib over the system libz
# (lib/Compress/Raw/Zlib.pm + the builtin::zlib_* primitives; TEMPORARY until
# pclxs, USER s516), and what it needs from the compiler:
#   - #2636 the copy-in/copy-out of a `my` scalar handed to a callee the
#     compiler cannot see (a method): `my $out; $d->deflate($in, $out)` is
#     how every caller of the module receives its output;
#   - #3340 `*$obj->{k}` is `(*$obj)->{k}`, the glob's hash
#     (IO::Compress::Base::Common's createSelfTiedObject).
# Before this batch `use Compress::Raw::Zlib` died "the module is XS", so
# every row here fails on the base.
#
# Each program's output is compared LINE BY LINE with the same program under
# perl (all of it is core perl; nothing needs perl 5.40).  When the box has
# no libz, the file skips and says so -- the module then dies at load, by
# design (docs/not-supported.md "Compress::Raw::Zlib").

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

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($file, $err) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl $file");
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>$err`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    return $output;
}

my $probe = run_cl(write_pl('print "zlib ", builtin::zlib_version(), "\n";'), "&1");
plan skip_all => "the system zlib (libz.so.1 / libz.dylib) is not on this box: "
    . "Compress::Raw::Zlib dies at load by design ($probe)"
    if $probe =~ /zlib \(libz\.so\.1\) is not available/;

sub compare_lines {
    my ($code, $what) = @_;
    my $file = write_pl($code);
    my @perl = split /\n/, scalar `perl $file 2>/dev/null`;
    my @pcl  = split /\n/, run_cl($file, "/dev/null");
    for my $k (0 .. $#perl) {
        is($pcl[$k] // "(no line " . ($k + 1) . ")", $perl[$k], "$what: line " . ($k + 1));
    }
}

# 1. The aliasing shapes (#2636): a `my` scalar handed to a method, a
# qualified call, a dynamic method and a coderef call is written back.
compare_lines(<<'P', 'unseen callee writes @_');
package T; sub new { bless {}, shift } sub m2 { my $self = shift; for (@_) { $_ .= "x" } } sub m4 { $_[1] .= "x" }
package main; my $o = T->new; my @r;
sub c1 { my ($self, $n, $rec) = @_; $self->m2($rec); $rec }      push @r, "list-decl+method=" . c1($o, 1, "v");
sub c2 { my ($self, $n, $rec) = @_; $self->m4($rec); $rec }      push @r, "list-decl+method-elem=" . c2($o, 1, "v");
sub c3 { my $self = shift; my $rec = shift; $self->m2($rec); $rec } push @r, "shift+method=" . c3($o, "v");
sub c4 { my ($self, $rec) = @_; T::m2($self, $rec); $rec }       push @r, "list-decl+fn=" . c4($o, "v");
sub c5 { my ($self, $rec) = @_; my $m = "m2"; $self->$m($rec); $rec } push @r, "list-decl+dynmethod=" . c5($o, "v");
sub c6 { my ($self, $rec) = @_; T->can("m2")->($self, $rec); $rec }  push @r, "list-decl+coderef=" . c6($o, "v");
sub c7 { my ($self, $rec) = @_; $self->m2($rec); $self->m2($rec); $rec } push @r, "twice=" . c7($o, "v");
{ my ($p, $q) = ($o, "v"); $p->m2($q); push @r, "file-level list-decl=$q" }
sub c8 { my ($self, @recs) = @_; $self->m2(@recs); "@recs" }     push @r, "array param=" . c8($o, "v", "w");
sub c9 { my ($self, $rec) = @_; $self->m2($rec) if 1; chomp(my $c = $rec); $c } push @r, "then-read=" . c9($o, "v");
print "$_\n" for @r;
P

# 2. A glob's hash through the arrow (#3340).
compare_lines(<<'P', 'glob arrow');
use Symbol ();
my $g = Symbol::gensym(); *$g->{A} = 1; print "plain ", *$g->{A}, "\n";
my $b = bless Symbol::gensym(), 'Foo'; *$b->{C} = 3; print "blessed ", *$b->{C}, " ", ${*$b}{C}, "\n";
*{$b}->{D} = 4; print "braced ", *{$b}->{D}, "\n";
P

# 3. Compress::Raw::Zlib's API, edge by edge.
compare_lines(<<'P', 'Compress::Raw::Zlib');
use strict; use warnings; use Compress::Raw::Zlib;
printf "crc32 abc %d adler32 abc %d crc32 empty %d chained %d == %d\n", crc32("abc"), adler32("abc"), crc32(""), crc32("def", crc32("abc")), crc32("abcdef");
printf "Z_OK=%d Z_STREAM_END=%d Z_BUF_ERROR=%d Z_DATA_ERROR=%d MAX_WBITS=%d Z_SYNC_FLUSH=%d WANT_GZIP=%d\n", Z_OK, Z_STREAM_END, Z_BUF_ERROR, Z_DATA_ERROR, MAX_WBITS, Z_SYNC_FLUSH, WANT_GZIP;
my $in = join("", map { chr(65 + $_ % 26) } 0 .. 9999) x 3;
my ($d, $st) = Compress::Raw::Zlib::Deflate->new(-Level => Z_BEST_COMPRESSION); print "Deflate->new: ", ($st == Z_OK ? "ok" : "st=$st"), " msg=[", "$st", "]\n";
my $out = ""; $st = $d->deflate($in, $out); print "deflate st=", 0+$st, " [", "$st", "] out=", length($out), "\n";
my $tail = ""; $st = $d->flush($tail); print "flush st=", 0+$st, " tail=", length($tail), " total_in=", $d->total_in, " total_out=", $d->total_out, "\n";
my $comp = $out . $tail;
my ($i, $st2) = Compress::Raw::Zlib::Inflate->new(); my $plain = ""; my $c = $comp; $st2 = $i->inflate($c, $plain);
print "inflate st=", 0+$st2, " [", "$st2", "] len=", length($plain), " same=", ($plain eq $in ? 1 : 0), " left=", length($c), " total_out=", $i->total_out, "\n";
my ($i2) = Compress::Raw::Zlib::Inflate->new(); my $t = substr($comp, 0, 20); my $p2 = ""; my $st3 = $i2->inflate($t, $p2); print "truncated st=", 0+$st3, " [", "$st3", "] got=", length($p2), "\n";
my ($i3) = Compress::Raw::Zlib::Inflate->new(); my $bad = "not zlib data at all"; my $p3 = ""; my $st4 = $i3->inflate($bad, $p3); print "bad st=", 0+$st4, " [", "$st4", "] msg=[", $i3->msg // "undef", "] left=", length($bad), "\n";
my ($d2) = Compress::Raw::Zlib::Deflate->new(); my $o1 = ""; $d2->deflate("hello ", $o1); my $o2 = ""; $d2->flush($o2, Z_SYNC_FLUSH); my $o3 = ""; $d2->deflate("world", $o3); my $o4 = ""; $d2->flush($o4);
my ($i4) = Compress::Raw::Zlib::Inflate->new(); my $all = $o1 . $o2 . $o3 . $o4; my $p4 = ""; my $st5 = $i4->inflate($all, $p4); print "sync: [$p4] st=", 0+$st5, " o2=", length($o2), "\n";
my ($i5) = Compress::Raw::Zlib::Inflate->new(-LimitOutput => 1, -Bufsize => 16); my $c5 = $comp; my $p5 = ""; my $n = 0; my $st6;
while (1) { my $chunk = ""; $st6 = $i5->inflate($c5, $chunk); $p5 .= $chunk; $n++; last if $st6 != Z_OK && $st6 != Z_BUF_ERROR; last if $n > 100000; }
print "limit: loops>1? ", ($n > 1 ? 1 : 0), " st=", 0+$st6, " same=", ($p5 eq $in ? 1 : 0), " left=", length($c5), "\n";
my ($i6) = Compress::Raw::Zlib::Inflate->new(-ConsumeInput => 0); my $c6 = $comp; my $p6 = ""; my $st7 = $i6->inflate($c6, $p6); print "noconsume: st=", 0+$st7, " left=", length($c6), " same=", ($p6 eq $in ? 1 : 0), "\n";
my ($i6b) = Compress::Raw::Zlib::Inflate->new(); my $c6b = $comp . "TAIL"; my $p6b = ""; $i6b->inflate($c6b, $p6b); print "consume tail: [$c6b]\n";
my ($d3) = Compress::Raw::Zlib::Deflate->new(); my $z = ""; my $zo = ""; $d3->deflate($z, $zo); my $zt = ""; $d3->flush($zt); my ($i7) = Compress::Raw::Zlib::Inflate->new(); my $zc = $zo . $zt; my $zp = ""; my $st8 = $i7->inflate($zc, $zp);
print "zero: comp=", length($zo . $zt), " st=", 0+$st8, " plain=[", $zp, "]\n";
my $dict = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" x 4;
my ($d4) = Compress::Raw::Zlib::Deflate->new(-Dictionary => $dict); my $o5 = ""; $d4->deflate($in, $o5); my $o6 = ""; $d4->flush($o6); my $cd = $o5 . $o6;
my ($i8) = Compress::Raw::Zlib::Inflate->new(-Dictionary => $dict); my $p8 = ""; my $st9 = $i8->inflate($cd, $p8); print "dict: adler=", $d4->dict_adler, " st=", 0+$st9, " same=", ($p8 eq $in ? 1 : 0), "\n";
my ($i8b) = Compress::Raw::Zlib::Inflate->new(); my $cd2 = $cd; my $p8b = ""; my $st9b = $i8b->inflate($cd2, $p8b); print "dict-missing: st=", 0+$st9b, " [", "$st9b", "]\n";
my ($d5) = Compress::Raw::Zlib::Deflate->new(-WindowBits => -MAX_WBITS()); my $o7 = ""; $d5->deflate("raw raw raw", $o7); my $o8 = ""; $d5->flush($o8);
my ($i9) = Compress::Raw::Zlib::Inflate->new(-WindowBits => -MAX_WBITS()); my $r9 = $o7 . $o8; my $p9 = ""; my $st10 = $i9->inflate($r9, $p9); print "raw: [$p9] st=", 0+$st10, "\n";
my ($d6) = Compress::Raw::Zlib::Deflate->new(-WindowBits => WANT_GZIP); my $o9 = ""; $d6->deflate("gz gz gz", $o9); my $o10 = ""; $d6->flush($o10); my $g = $o9 . $o10;
printf "gzip magic %02x%02x\n", ord(substr($g,0,1)), ord(substr($g,1,1));
my ($i10) = Compress::Raw::Zlib::Inflate->new(-WindowBits => WANT_GZIP_OR_ZLIB); my $p10 = ""; my $st11 = $i10->inflate($g, $p10); print "gunzip-auto: [$p10] st=", 0+$st11, "\n";
my $w = eval { my ($dw) = Compress::Raw::Zlib::Deflate->new(); my $ow = ""; $dw->deflate("\x{100}", $ow); "no-die" }; print "wide: ", (defined $w ? $w : "die: " . ($@ =~ /Wide character/ ? "Wide character" : $@)), "\n";
my ($d7) = Compress::Raw::Zlib::Deflate->new(-AppendOutput => 1); my $acc = "pre"; $d7->deflate("abc", $acc); $d7->flush($acc); print "append: prefix=", substr($acc, 0, 3), " len>3=", (length($acc) > 3 ? 1 : 0), "\n";
$d7->deflateReset(); my $acc2 = ""; $d7->deflate("abc", $acc2); $d7->flush($acc2); print "reset: len=", length($acc2), " deflateParams=", 0 + $d7->deflateParams(-Level => Z_BEST_SPEED), "\n";
my ($dw2, $sw) = Compress::Raw::Zlib::Deflate->new(-WindowBits => 99); print "bad windowbits: ", (defined $dw2 ? "obj" : "undef"), " ", 0+$sw, " [$sw]\n";
eval { Compress::Raw::Zlib::Deflate->new(-Fred => 1) }; print "unknown key: ", $@ =~ /unknown key value\(s\) Fred/ ? "croak" : $@, "\n";
P

# 4. The modules over the shim: Compress::Zlib, IO::Compress, IO::Zlib.
compare_lines(<<'P', 'modules');
use strict; use warnings;
use Compress::Zlib; use IO::Compress::Gzip qw(gzip $GzipError); use IO::Uncompress::Gunzip qw(gunzip $GunzipError);
my $in = join("", map { sprintf "%05d:%s\n", $_, "x" x ($_ % 7) } 1 .. 3000);
my $gz; gzip(\$in => \$gz) or die "gzip: $GzipError"; my $back; gunzip(\$gz => \$back) or die "gunzip: $GunzipError";
print "gzip/gunzip: in=", length($in), " same=", ($back eq $in ? 1 : 0), " magic=", (substr($gz, 0, 2) eq "\x1f\x8b" ? 1 : 0), "\n";
my $m = Compress::Zlib::memGzip($in); my $mb = Compress::Zlib::memGunzip($m); print "memGzip: same=", ($mb eq $in ? 1 : 0), "\n";
my $cross = Compress::Zlib::memGunzip($gz); print "cross: same=", ($cross eq $in ? 1 : 0), "\n";
my $c = compress($in); my $u = uncompress($c); print "compress: ", length($c), " same=", ($u eq $in ? 1 : 0), "\n";
my $bad = Compress::Zlib::memGunzip("not a gzip"); print "bad header: ", (defined $bad ? "defined" : "undef"), "\n";
my ($d) = deflateInit(); my ($o1, $s1) = $d->deflate("hello"); my ($o2, $s2) = $d->flush(); my ($i) = inflateInit(); my ($p, $s3) = $i->inflate($o1 . $o2); print "Compress::Zlib stream: [$p] $s1 $s2 $s3\n";
print "crc32 ", Compress::Zlib::crc32("abc"), " adler32 ", Compress::Zlib::adler32("abc"), "\n";
P

done_testing;

#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# digest-shim-01.t -- s514a (#2947): `lib/Digest/MD5.pm` and `lib/Digest/SHA.pm`,
# plain Perl.
#
# The real modules are XS, so `use Digest::MD5` / `use Digest::SHA` died "Can't
# locate ... (the module is XS and has no PCL build)".  perl's XS modules are
# the ORACLE for every row: ONE program prints `LABEL<TAB>VALUE` lines, it runs
# under perl (the XS modules) and under PCL (the shims), and each label is a
# row.  The program covers the RFC 1321 / FIPS 180 vectors (incl. the one
# million "a"s), the raw / hex / base64 forms, every SHA width, HMAC (incl. a
# key longer than the block), OO incremental == functional, `addfile`, the
# reset-on-`digest` rule, `clone`, `new` on an instance, `reset` keeping the
# algorithm, an unknown algorithm, a wide character dying, an upgraded string
# hashing as its bytes, and the import lists.
#
# The die rows compare the message WITHOUT perl's " at FILE line N." suffix:
# PCL's Carp does not append it (docs/shipped-modules.md, `Carp`).

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
plan skip_all => "perl has no XS Digest::MD5 / Digest::SHA to be the oracle"
    unless eval { require Digest::MD5; require Digest::SHA; 1 };

my ($dfh, $datafile) = tempfile(SUFFIX => '.bin', UNLINK => 1);
binmode $dfh;
print $dfh join('', map { chr($_ % 256) } 0 .. 9999);
close $dfh;

my $PROGRAM = <<'PERL';
use strict; use warnings;
use Digest::MD5 qw(md5 md5_hex md5_base64);
use Digest::SHA qw(sha1_hex sha1_base64 sha224_hex sha256 sha256_hex sha256_base64
                   sha384_hex sha512_hex sha512224_hex sha512256_hex
                   hmac_sha1_hex hmac_sha256_hex hmac_sha256_base64 hmac_sha512_hex);
my $file = "__DATAFILE__";
sub row { print "$_[0]\t$_[1]\n" }
sub dies { my ($code) = @_; my $ok = eval { $code->(); 1 }; return $ok ? "no die" : ($@ =~ s/ at \S+ line \d+\.?\n?\z//r) }
my @v = ("", "a", "abc", "message digest", "abcdefghijklmnopqrstuvwxyz",
         "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq");
row("md5 vectors",    join(' ', map { md5_hex($_) } @v));
row("md5 raw+b64",    join(' ', map { unpack('H*', md5($_)) . '/' . md5_base64($_) } @v[0..2]));
row("md5 block edges", join(' ', map { md5_hex("x" x $_) } 55, 56, 63, 64, 65, 119, 120, 128));
row("md5 all bytes",  md5_hex(join '', map { chr } 0 .. 255));
row("md5 list args",  md5_hex("ab", "c", "") . ' ' . md5_hex());
my $million = "a" x 1_000_000;
row("md5 million a",  md5_hex($million));
row("sha256 million a", sha256_hex($million));
row("sha1 vectors",   join(' ', map { sha1_hex($_) } @v));
row("sha224 vectors", join(' ', map { sha224_hex($_) } @v));
row("sha256 vectors", join(' ', map { sha256_hex($_) } @v));
row("sha384 vectors", join(' ', map { sha384_hex($_) } @v));
row("sha512 vectors", join(' ', map { sha512_hex($_) } @v));
row("sha512/224 + /256", join(' ', map { sha512224_hex($_) . '/' . sha512256_hex($_) } @v[0..2]));
row("sha block edges", join(' ', map { sha256_hex("y" x $_) . sha512_hex("y" x $_) } 55, 56, 64, 111, 112, 128));
row("sha base64 + raw", sha1_base64("abc") . ' ' . sha256_base64("abc") . ' ' . unpack('H*', sha256("abc")));
row("hmac rfc4231 1", hmac_sha256_hex("Hi There", "\x0b" x 20));
row("hmac rfc4231 2", hmac_sha256_hex("what do ya want for nothing?", "Jefe"));
row("hmac long key",  hmac_sha256_hex("data", "k" x 100) . ' ' . hmac_sha512_hex("data", "k" x 200));
row("hmac sha1 + b64 + list", hmac_sha1_hex("data", "key") . ' ' . hmac_sha256_base64("data", "key")
                              . ' ' . hmac_sha256_hex("da", "ta", "key"));
my $m = Digest::MD5->new;
$m->add("message ")->add("dig", "est");
my $mc = $m->clone;
row("md5 oo incremental", $m->hexdigest . ' ' . $m->hexdigest . ' ' . $mc->b64digest);
my $m2 = Digest::MD5->new; $m2->add("junk"); $m2->new; $m2->add("abc");
row("md5 new on instance resets", $m2->hexdigest);
open(my $fh, '<', $file) or die "open $file: $!"; binmode $fh;
row("md5 addfile", Digest::MD5->new->addfile($fh)->hexdigest);
close $fh;
row("sha addfile name", Digest::SHA->new(256)->addfile($file)->hexdigest . ' '
                        . Digest::SHA->new(1)->addfile($file, "b")->hexdigest);
for my $alg (1, 224, 256, 384, 512, 512224, 512256) {
    my $o = Digest::SHA->new($alg);
    $o->add("ab")->add("c", "d");
    my $c = $o->clone;
    row("sha oo $alg", join(' ', $o->algorithm, $o->hashsize, $o->hexdigest, $o->hexdigest, $c->b64digest));
}
row("sha new spellings", join(' ', map { Digest::SHA->new($_)->algorithm } 'sha256', 'SHA-512/224', 384));
row("sha unknown alg", defined(Digest::SHA->new(999)) ? "defined" : "undef");
my $d = Digest::SHA->new(256); $d->add("zz"); $d->reset; $d->add("abc");
row("sha reset keeps alg", $d->algorithm . ' ' . $d->hexdigest);
$d->add_bits("011000010110001001100011");
row("sha add_bits", $d->hexdigest);
row("md5 wide char dies", dies(sub { md5_hex("\x{100}") }));
row("sha wide char dies", dies(sub { Digest::SHA->new->add("\x{263a}") }));
my $u = "caf\x{e9}"; utf8::upgrade($u);
row("upgraded string = its bytes", md5_hex($u) . ' ' . sha1_hex($u));
row("isa Digest::base", join(' ', map { $_->isa('Digest::base') ? 1 : 0 } Digest::MD5->new, Digest::SHA->new));
row("import lists", join(' ', map { defined(&{"main::$_"}) ? 1 : 0 } qw(md5_hex sha256_hex sha1 sha384 hmac_sha1)));
PERL

my ($pfh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
print $pfh $PROGRAM =~ s{__DATAFILE__}{$datafile}r;
close $pfh;

sub rows_of {
    my ($text) = @_;
    my (%r, @order);
    for my $line (split /\n/, $text) {
        my ($k, $v) = split /\t/, $line, 2;
        next if !defined $v;
        push @order, $k;
        $r{$k} = $v;
    }
    return (\%r, \@order);
}

my $perl_out = `$^X $pl_file 2>&1`;
my $cl_code  = PCLCore::transpile("$pl2cl $pl_file");
my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
print $cl_fh $cl_code;
close $cl_fh;
my $pcl_out = `sbcl @sbcl_rt --load $cl_file 2>&1`;

my ($perl, $order) = rows_of($perl_out);
my ($pcl)          = rows_of($pcl_out);

plan tests => 1 + 39;
is(scalar(@$order), 39, 'the oracle program printed its 39 rows under perl')
    or diag($perl_out);
for my $k (@$order) {
    is($pcl->{$k}, $perl->{$k}, "$k (perl: " . substr($perl->{$k}, 0, 60) . ")");
}

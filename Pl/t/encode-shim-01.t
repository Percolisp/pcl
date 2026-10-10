#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# encode-shim-01.t -- task #2946 (s513i): `use Encode` works without XS.
#
# lib/Encode.pm is plain Perl over the runtime's codec primitives
# (builtin::encode_octets / decode_octets / encoding_known, the ONE codec table
# `:encoding(NAME)` uses).  Before it, `use Encode` died "the module is XS and
# has no PCL build" -- so every row here fails on the base.
#
# Each program prints one LABELLED line per row and each line is compared with
# the same program under perl (Encode is core).  Nothing here needs Encode
# 3.21 (CI runs perl 5.38 / Encode 3.19) or perl 5.40 syntax.  The answers
# PCL does not give, by design, are not asked: the per-scalar UTF-8 flag after
# ENCODE (#1389: is_utf8 is 1 for every string), a CHECK on a read-only literal
# (perl: "Modification of a read-only value"), a croak's location (#233), the
# in-place remainder through a raw-slot argument (#3060), and code points above
# U+10FFFF (the host's character limit).  docs/shipped-modules.md lists them.

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
sub cp { my $s = shift; return 'undef' if !defined $s; return '""' if !length $s;
         join ' ', map { sprintf '%X', ord } split //, $s }
sub row { my ($l, @v) = @_; print "$l: ", join(' | ', @v), "\n" }
sub trap { my $c = shift; my $r = eval { $c->() };
           return defined $r ? "ok:$r" : 'died:' . ($@ =~ s/ at \S+ line \d+\.?\n?//sr =~ s/\n\z//r) }
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

# Every perl output line is one row: its label must appear in PCL's output
# with the same value.
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

# 1. Round trips, the form a decode returns, encode_utf8 / decode_utf8.
compare_lines(<<'P', 'round trip');
use Encode qw(encode decode encode_utf8 decode_utf8 is_utf8);
my $s = "caf\x{E9} \x{263A}"; my $l = "caf\x{E9}";
for my $e (qw(UTF-8 utf8 UTF-16LE UTF-16BE UTF-16 UTF-32LE)) {
  my $o = encode($e, $s); my $b = decode($e, $o);
  row("rt $e", cp($o), cp($b), (is_utf8($b) ? 1 : 0), ($b eq $s ? 'same' : 'DIFF'));
}
for my $e (qw(latin1 ascii cp1252)) {
  my $o = encode($e, $l); my $b = decode($e, $o);
  row("rt $e", cp($o), cp($b), (is_utf8($b) ? 1 : 0));
}
row('cp1252 euro', cp(encode('cp1252', "\x{20AC}")), cp(decode('cp1252', "\x80")));
row('decode ascii text is upgraded', is_utf8(decode('UTF-8', 'abc')) ? 1 : 0);
row('decode_utf8', cp(decode_utf8("caf\xC3\xA9")));
row('decode_utf8 of a decoded string', cp(decode_utf8(decode_utf8("caf\xC3\xA9"))));
row('encode_utf8', cp(encode_utf8($s)), length encode_utf8($s));
row('undef in, undef out', cp(encode('UTF-8', undef)), cp(decode('UTF-8', undef)), cp(encode_utf8(undef)));
row('decode a wide string', trap(sub { decode('UTF-8', "\x{263A}") }));
P

# 2. Malformed input under every CHECK mode (sources are VARIABLES).
compare_lines(<<'P', 'decode CHECK');
use Encode qw(:all);
sub d { my ($e, $in, $chk) = @_; my $src = $in; my $r = decode($e, $src, $chk); return cp($r) }
row('FB_DEFAULT', d('UTF-8', "a\xE9b", FB_DEFAULT));
row('FB_CROAK', trap(sub { my $v = "a\xE9b"; decode('UTF-8', $v, FB_CROAK) }));
row('FB_CROAK utf8', trap(sub { my $v = "a\xE9b"; decode('utf8', $v, FB_CROAK) }));
row('FB_CROAK cp1252', trap(sub { my $v = "a\x81b"; decode('cp1252', $v, FB_CROAK) }));
row('FB_QUIET', d('UTF-8', "a\xE9b", FB_QUIET));
{ my $w = ''; local $SIG{__WARN__} = sub { $w = $_[0] =~ s/ at \S+ line \d+\.?\n?//r };
  row('FB_WARN', d('UTF-8', "a\xE9b", FB_WARN), $w); }
row('FB_PERLQQ', decode('UTF-8', my $v1 = "a\xE9b", FB_PERLQQ));
row('FB_HTMLCREF', decode('UTF-8', my $v2 = "a\xE9b", FB_HTMLCREF));
row('FB_XMLCREF', decode('UTF-8', my $v3 = "a\xE9b", FB_XMLCREF));
row('CODE ref', decode('UTF-8', "a\xE9b", sub { sprintf '<%X>', shift }));
row('surrogate strict', d('UTF-8', "\xED\xA0\x80z", FB_DEFAULT));
row('surrogate lax', d('utf8', "\xED\xA0\x80z", FB_DEFAULT));
row('overlong lax', d('utf8', "\xC0\xAFz", FB_DEFAULT));
row('noncharacter strict', d('UTF-8', "\xEF\xBF\xBFz", FB_DEFAULT));
row('FF FE bytes', d('UTF-8', "\xFF\xFEz", FB_DEFAULT));
row('truncated at end', d('UTF-8', "ab\xE2\x98", FB_DEFAULT));
row('truncated, croak', trap(sub { my $v = "a\xE2\x98"; decode('UTF-8', $v, FB_CROAK) }));
row('ascii high byte', d('ascii', "a\xC8b", FB_DEFAULT));
row('cp1252 hole PERLQQ', decode('cp1252', my $v4 = "a\x81b", FB_PERLQQ));
my %q = (a => "a\xE9b", b => "ab\xE2\x98", c => "a\xE9b", d => "ab\xE2\x98");
decode('UTF-8', $q{a}, FB_QUIET); row('FB_QUIET remainder', cp($q{a}));
row('STOP_AT_PARTIAL', cp(decode('UTF-8', $q{b}, STOP_AT_PARTIAL)), cp($q{b}));
decode('UTF-8', $q{c}, FB_QUIET | LEAVE_SRC); row('LEAVE_SRC', cp($q{c}));
row('decode_utf8 FB_QUIET', cp(decode_utf8($q{d}, FB_QUIET)), cp($q{d}));
P

# 3. Encode CHECK modes, strict/lax UTF-8 on encode, names, objects, layers.
compare_lines(<<'P', 'encode, names');
use Encode qw(:all);
my $s = "a\x{263A}b\x{E9}";
for my $e (qw(iso-8859-1 ascii cp1252)) {
  row("enc $e default", cp(encode($e, $s)));
  row("enc $e PERLQQ", encode($e, my $t1 = $s, FB_PERLQQ));
  row("enc $e HTML", encode($e, my $t2 = $s, FB_HTMLCREF));
  row("enc $e XML", encode($e, my $t3 = $s, FB_XMLCREF));
}
row('enc croak', trap(sub { my $v = $s; encode('latin1', $v, FB_CROAK) }));
row('enc CODE ref', encode('latin1', "a\x{263a}b", sub { sprintf '<%X>', shift }));
row('enc U+D800 strict', cp(encode('UTF-8', "\x{D800}")), trap(sub { my $v = "\x{D800}"; encode('UTF-8', $v, FB_CROAK) }));
row('enc U+D800 lax', cp(encode('utf8', "\x{D800}")));
row('enc U+FFFF strict', cp(encode('UTF-8', "\x{FFFF}")));
for my $n (qw(latin1 ISO_8859-1 l1 UTF-8 utf8 sjis gbk windows-1252 WinLatin1 nope)) {
  row("alias $n", resolve_alias($n) // 'undef');
}
for my $n (qw(UTF-8 utf8 latin1 ascii cp1252 UTF-16LE)) {
  my $o = find_encoding($n);
  row("obj $n", $o->name, $o->mime_name, ($o->isa('Encode::Encoding') ? 1 : 0),
      ($o->perlio_ok ? 1 : 0), cp($o->encode("\x{E9}")), cp($o->decode("A")));
}
row('unknown encoding', trap(sub { encode('xyz', 'a') }), trap(sub { decode('xyz', 'a') }),
    (defined find_encoding('xyz') ? 'def' : 'undef'));
my %enc = map { $_ => 1 } Encode->encodings(':all');
row('encodings', join ',', map { $enc{$_} ? 1 : 0 } qw(ascii iso-8859-1 utf8 utf-8-strict UTF-16LE UTF-16BE UTF-32LE cp1252));
my %ft = (x => "a\xE9"); my $n = from_to($ft{x}, "latin1", "utf8"); row("from_to", $n, cp($ft{x}));
row('utf16 decode BOM', cp(decode('UTF-16', "\xFF\xFEa\x00")), cp(decode('UTF-16', "\xFE\xFF\x00a")));
row('utf16le keeps a BOM', cp(decode('UTF-16LE', "\xFF\xFEa\x00")));
row('constants', FB_DEFAULT, FB_CROAK, FB_QUIET, FB_WARN, FB_PERLQQ, FB_HTMLCREF, FB_XMLCREF, LEAVE_SRC, STOP_AT_PARTIAL);
row('fallbacks tag', join ',', @{ $Encode::EXPORT_TAGS{fallbacks} });
my $u = decode('UTF-8', "\xC3\xA9"); Encode::_utf8_off($u); row('_utf8_off', cp($u));
require PerlIO::encoding; row('PerlIO::encoding', $PerlIO::encoding::fallback, $PerlIO::encoding::VERSION ? 'hasver' : 'nover');
my ($fh, $fn) = (undef, "/tmp/encode-shim-01-$$.txt");
open $fh, '>:raw', $fn or die; print $fh "caf\xC3\xA9\n"; close $fh;
open my $i, '<:encoding(UTF-8)', $fn or die; my $line = <$i>; close $i; chomp $line;
row('read :encoding(UTF-8)', cp($line), (is_utf8($line) ? 1 : 0));
open my $r, '<:raw', $fn or die; my $rl = <$r>; close $r; chomp $rl; row('read :raw', cp($rl));
unlink $fn;
# perl's own Encode::Alias is consulted when a program loaded it: a string
# alias and a CODE alias (ExtUtils::MakeMaker::Locale's "locale" shape).
require Encode::Alias;
our $ENCODING_MINE = 'UTF-8';
Encode::Alias::define_alias(sub { no strict 'refs'; ${"ENCODING_" . uc(shift)} }, 'mine');
Encode::Alias::define_alias(myl1 => 'iso-8859-1');
row('Encode::Alias', trap(sub { find_encoding('mine')->name }), trap(sub { find_encoding('myl1')->name }),
    trap(sub { resolve_alias('myl1') }), trap(sub { cp(decode(mine => "\xC3\xA9")) }));
P

done_testing();

#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# storable-shim-01.t -- s514a (#2948): `lib/Storable.pm`, plain Perl, in PERL'S
# OWN binary format.
#
# The real module is XS, so `use Storable` died "Can't locate ... (the module is
# XS and has no PCL build)".  The shim writes and reads perl's Storable stream,
# so frozen strings and files interchange with real perl.  perl's XS Storable
# is the ORACLE: ONE program prints `LABEL<TAB>VALUE` lines, it runs under perl
# and under PCL, and each label is a row.  The program's own `dd` dumps a
# structure canonically AND marks a reference it has seen before (`SEEN#n`), so
# a row compares sharing and cycles, not only values.
#
# Rows that cross the boundary: bytes perl's `freeze` / `nfreeze` wrote (hex,
# computed here at run time) are thawed by the program; a file perl's `store`
# wrote is retrieved by the program; and the file the PCL run stores is
# retrieved by perl after the run.
#
# Where PCL cannot be byte-identical (no per-scalar UTF-8 flag, no boolean
# SV) the structures here avoid those shapes; docs/not-supported.md
# ("Storable") has them.  Die rows compare the message without perl's
# location suffix (PCL's Carp does not append it).

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
plan skip_all => "perl has no XS Storable to be the oracle"
    unless eval { require Storable; 1 };

my $dir = tempdir(CLEANUP => !$ENV{PCL_KEEP_TMP});
diag("kept: $dir") if $ENV{PCL_KEEP_TMP};

# The structure every cross-boundary row uses: numbers, byte and wide strings,
# nesting, a blessed hash, sharing.
my $STRUCT = q{do { my $sh = [7, 8];
    { int => [0, 1, -1, 127, 128, -129, 70000, -70000, 2**40, -2**40],
      dbl => [1.5, -0.25, 1e100], str => ["", "abc", "x" x 300, "caf\x{e9}", "\x{263a}\x{263b}"],
      undef => undef, nest => {a => [1, [2, [3]]], b => {}}, obj => bless({k => "v"}, "Foo::Bar"),
      sh1 => $sh, sh2 => $sh, ref => \"scal", rr => \\5, "\x{263a}key" => 1 } }};

# Run CODE under real perl (a script file: no shell quoting) and return its STDOUT.
my $nscript = 0;
sub perl_run {
    my ($code) = @_;
    my $file = "$dir/oracle-" . $nscript++ . ".pl";
    open(my $fh, '>', $file) or die "oracle script: $!";
    print $fh "use strict; use warnings; use Storable; use Scalar::Util;\n$code\n";
    close $fh;
    open(my $out, '-|', $^X, $file) or die "run oracle: $!";
    local $/;
    my $text = <$out>;
    close $out;
    die "perl oracle failed: $code" if $?;
    return $text;
}
sub perl_hex {
    my ($fn) = @_;
    my $hex = perl_run("\$Storable::canonical = 1; print unpack 'H*', Storable::$fn($STRUCT);");
    die "perl $fn failed" if $hex !~ /\A[0-9a-f]+\z/;
    return $hex;
}
my $freeze_hex  = perl_hex('freeze');
my $nfreeze_hex = perl_hex('nfreeze');
my $perl_file = "$dir/by-perl.st";
perl_run("\$Storable::canonical = 1; Storable::nstore($STRUCT, '$perl_file');");

my $PROGRAM = <<'PERL';
use strict; use warnings; use Scalar::Util ();
use Storable qw(freeze nfreeze thaw dclone store nstore retrieve store_fd fd_retrieve
                lock_store lock_retrieve);
$Storable::canonical = 1;
my $dir = "__DIR__";
my $tag = "__TAG__";
sub row { print "$_[0]\t$_[1]\n" }
sub dies { my ($code) = @_; my @r = eval { $code->() }; return "no die: " . scalar(@r) if !$@;
           my $e = $@; $e =~ s/ at \S+ line \d+\.?//g; $e =~ s/\s+\z//; $e =~ s/,\z//; return $e }
my %seen; my $n;
sub dd { my ($v) = @_; return 'undef' if !defined $v; return "'$v'" if !ref $v;
  my $id = Scalar::Util::refaddr($v);
  return "SEEN#$seen{$id}" if exists $seen{$id};
  $seen{$id} = $n++;
  my $c = Scalar::Util::blessed($v); my $p = defined $c ? "$c=" : '';
  my $t = Scalar::Util::reftype($v);
  return $p . '[' . join(',', map { exists $v->[$_] ? dd($v->[$_]) : 'HOLE' } 0 .. $#$v) . ']' if $t eq 'ARRAY';
  return $p . '{' . join(',', map { "$_=>" . dd($v->{$_}) } sort keys %$v) . '}' if $t eq 'HASH';
  my $w = ($t eq 'SCALAR' || $t eq 'REF') ? $$v : undef;   # a temporary: #3081
  return $p . '\\' . dd($w) if $t eq 'SCALAR' || $t eq 'REF';
  return "$p<$t>" }
sub D { %seen = (); $n = 0; my $s = dd($_[0]); $s =~ s/([^\x20-\x7e])/sprintf('\\x{%x}', ord $1)/ge; $s }
my $struct = __STRUCT__;
row("nfreeze bytes == perl's", unpack('H*', nfreeze($struct)));
row("freeze bytes == perl's",  unpack('H*', freeze($struct)));
row("thaw(freeze) round trip", D(thaw(freeze($struct))));
row("thaw(nfreeze) round trip", D(thaw(nfreeze($struct))));
row("thaw perl-written freeze bytes", D(thaw(pack('H*', "__FREEZE_HEX__"))));
row("thaw perl-written nfreeze bytes", D(thaw(pack('H*', "__NFREEZE_HEX__"))));
row("retrieve a file perl stored", D(retrieve("__PERL_FILE__")));
nstore($struct, "$dir/by-$tag.st");
row("nstore then retrieve", D(retrieve("$dir/by-$tag.st")));
row("store returns", store([1, 2], "$dir/s2-$tag.st") . ' ' . D(retrieve("$dir/s2-$tag.st")));
lock_store({a => 1}, "$dir/l-$tag.st");
row("lock_store / lock_retrieve", D(lock_retrieve("$dir/l-$tag.st")));
open(my $wf, '>', "$dir/fd-$tag.st") or die; binmode $wf;
store_fd([1], $wf); store_fd({two => 2}, $wf); close $wf;
open(my $rf, '<', "$dir/fd-$tag.st") or die; binmode $rf;
my $first = fd_retrieve($rf); my $second = fd_retrieve($rf); close $rf;
row("store_fd x2 / fd_retrieve x2", D($first) . ' ' . D($second));
my $x = [1];
my $t = dclone([$x, $x, {r => $x}]);
row("sharing survives dclone", ($t->[0] == $t->[1] && $t->[2]{r} == $t->[0] ? 'shared' : 'copied') . ' ' . D($t));
my $c = {name => 'loop'}; $c->{self} = $c; my $l = [1]; push @$l, $l;
my $tc = thaw(freeze($c)); my $tl = thaw(nfreeze($l));
row("cycles survive", ($tc->{self} == $tc ? 'hash-cycle' : 'broken') . ' ' . ($tl->[1] == $tl ? 'array-cycle' : 'broken'));
my $orig = {list => [1, 2], h => {k => 'v'}};
my $copy = dclone($orig); push @{ $copy->{list} }, 3; $copy->{h}{k} = 'changed';
row("dclone is deep", D($orig) . ' ' . D($copy));
my @sparse; $sparse[3] = 'x'; $sparse[1] = undef;
row("sparse array keeps holes", D(thaw(freeze(\@sparse))) . ' ' . unpack('H*', freeze(\@sparse)));
row("blessed objects + class index", D(thaw(freeze([bless([1], 'A'), bless({}, 'B'), bless(\(my $s = 3), 'A')]))));
row("scalar and ref-to-ref roots", D(thaw(freeze(\"str"))) . ' ' . D(thaw(freeze(\\[1]))));
row("wide strings and keys", D(thaw(nfreeze({"\x{263a}" => "\x{100}\x{2603}", "k\x{e9}" => "\x{e9}"}))));
row("long string + long wide string", length(${ thaw(freeze(\("y" x 70000))) }) . ' '
                                      . length(${ thaw(freeze(\("\x{263a}" x 300))) }));
row("integers native vs network", unpack('H*', freeze([2**31, -2**31 - 1, 2**62])) . ' '
                                  . unpack('H*', nfreeze([2**31 - 1, -2**31, 2**31])));
row("numbers from strings stay strings", unpack('H*', freeze(["12", "1.5", "0"])));
row("doubles", unpack('H*', freeze([0.1, -1e-300, 3.0])) . ' ' . unpack('H*', nfreeze([0.1, 3.0])));
row("large array", scalar(@{ thaw(freeze([1 .. 5000])) }) . ' ' . thaw(freeze([1 .. 5000]))->[4999]);
package Ov { use overload '""' => sub { "Ov(" . $_[0]{v} . ")" }, fallback => 1; }
my $ov = thaw(freeze([bless({v => 9}, 'Ov')]));
row("overloaded object keeps its overloading", "$ov->[0] " . unpack('H*', freeze([bless({v => 9}, 'Ov')])));
row("booleans perl wrote thaw as true/false", join(',', map { $_ ? 'T' : 'F' } @{ thaw(pack('H*', '050b02000000022223')) }));
my $qr = qr/a.b/i;
my $tq = thaw(freeze([$qr, $qr]));
row("regexp: bytes, match, sharing", unpack('H*', freeze([$qr])) . ' ' . ("A-B" =~ $tq->[0] ? 1 : 0)
                                     . ' ' . ($tq->[0] == $tq->[1] ? 'shared' : 'copied'));
package TH { sub TIEHASH { my ($c, %a) = @_; bless {%a}, $c } sub FETCH { $_[0]{$_[1]} }
  sub FIRSTKEY { my @k = sort keys %{$_[0]}; $k[0] }
  sub NEXTKEY { my @k = sort keys %{$_[0]}; for my $i (0 .. $#k) { return $k[$i + 1] if $k[$i] eq $_[1] } undef } }
tie my %tied, 'TH', a => 1, b => 2;
my $tt = thaw(freeze({h => \%tied}));
row("tied hash: bytes, retrieved tied", unpack('H*', freeze(\%tied)) . ' ' . ref(tied(%{ $tt->{h} })) . ' '
                                        . join(',', map { "$_=$tt->{h}{$_}" } sort keys %{ $tt->{h} }));
row("flags: no BLESS_OK = unblessed, no TIE_OK dies",
    ref(thaw(freeze([bless {}, 'X']), 0)->[0]) . ' ' . dies(sub { thaw(freeze(\%tied), Storable::BLESS_OK()) }));
row("CODE dies", dies(sub { freeze([sub { 1 }]) }));
row("GLOB dies", dies(sub { freeze([\*STDOUT]) }));
row("not a reference", dies(sub { freeze(1) }) . ' / ' . dies(sub { dclone("x") }));
row("dclone CODE dies", dies(sub { dclone({c => sub { 1 }}) }));
{ local $Storable::forgive_me = 1; local $SIG{__WARN__} = sub {};
  my $f = thaw(freeze([sub { 1 }]));
  row("forgive_me stores a placeholder", ref($f->[0]) . ' ' . (${ $f->[0] } =~ /^You lost CODE\(0x[0-9a-f]+\)\z/ ? 'placeholder' : ${ $f->[0] })); }
my $fr = freeze([1, 2, 'abc']);
row("truncated stream thaws to undef", defined(thaw(substr($fr, 0, -2))) ? 'defined' : 'undef');
row("newer major dies", dies(sub { my $g = $fr; substr($g, 0, 2) = "\x06\x00"; thaw($g) }));
row("newer minor is accepted", D(do { my $g = $fr; substr($g, 1, 1) = "\x63"; thaw($g) }));
row("unknown tag dies", dies(sub { my $g = nfreeze([1]); substr($g, -2, 1) = "\x60"; thaw($g) }));
row("thaw undef / empty", (defined(thaw(undef)) ? 'def' : 'undef') . ' ' . dies(sub { thaw('') }));
row("retrieve a missing file dies", dies(sub { retrieve("$dir/no/such.st") }));
row("store into a missing dir dies", dies(sub { store([1], "$dir/no/such.st") }));
row("last_op_in_netorder", join(',', do { nfreeze([1]); Storable::last_op_in_netorder() ? 1 : 0 },
                                     do { freeze([1]);  Storable::last_op_in_netorder() ? 1 : 0 }));
PERL

sub program {
    my ($tag) = @_;
    my $p = $PROGRAM;
    $p =~ s/__STRUCT__/$STRUCT/;
    $p =~ s/__DIR__/$dir/;
    $p =~ s/__TAG__/$tag/;
    $p =~ s/__FREEZE_HEX__/$freeze_hex/;
    $p =~ s/__NFREEZE_HEX__/$nfreeze_hex/;
    $p =~ s/__PERL_FILE__/$perl_file/;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', DIR => $dir);
    print $fh $p;
    close $fh;
    return $file;
}

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

my $perl_out = `$^X @{[ program('perl') ]} 2>&1`;
my $cl_code  = PCLCore::transpile("$pl2cl " . program('pcl'));
my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', DIR => $dir);
print $cl_fh $cl_code;
close $cl_fh;
my $pcl_out = `sbcl @sbcl_rt --load $cl_file 2>&1`;

my ($perl, $order) = rows_of($perl_out);
my ($pcl)          = rows_of($pcl_out);

my $ROWS = 41;
plan tests => 2 + $ROWS;
is(scalar(@$order), $ROWS, "the oracle program printed its $ROWS rows under perl")
    or diag($perl_out);
for my $k (@$order) {
    is($pcl->{$k}, $perl->{$k}, "$k (perl: " . substr($perl->{$k}, 0, 60) . ")")
        or diag(substr($pcl_out, 0, 2000));
}

# The file the PCL run stored, retrieved by real perl.
my $dump = q{sub dd { my ($v) = @_; return 'undef' if !defined $v; return "'$v'" if !ref $v;
  my $c = Scalar::Util::blessed($v); my $p = defined $c ? "$c=" : ''; my $t = Scalar::Util::reftype($v);
  return $p . '[' . join(',', map { dd($_) } @$v) . ']' if $t eq 'ARRAY';
  return $p . '{' . join(',', map { "$_=>" . dd($v->{$_}) } sort keys %$v) . '}' if $t eq 'HASH';
  return $p . chr(92) . dd($$v) }};
my $by_pcl  = eval { perl_run("$dump print dd(Storable::retrieve('$dir/by-pcl.st'));") } // "perl could not retrieve it: $@";
my $by_perl = perl_run("$dump print dd(Storable::retrieve('$dir/by-perl.st'));");
is($by_pcl, $by_perl, 'perl retrieves the file the PCL run nstored, equal to its own');

#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# fh-collect-01.t -- the COLLECTOR closes a filehandle the program can no longer
# reach (task #2006 PART 2, s540c; docs/ir-spec.md §6.5b).
#
# A handle that ESCAPED the block that opened it (returned by a sub, passed to
# a helper) used to stay open until exit, keeping its descriptor and any
# `flock' it held: Path::Tiny's `slurp' (a returned LOCK_SH handle) followed by
# a locked `append' waited on the program's own lock forever, and a loop over
# more files than the descriptor limit died.  Now a finalizer flushes and
# closes a stream nothing reaches, and the two places the program would block
# or fail without it -- a `flock' that would block on a file this process has
# locked, an opener out of descriptors -- run the collector and try once more.
#
# Each program runs under perl (the oracle) and under PCL, under `timeout' so
# that the base -- which HANGS on the blocking row -- fails instead of
# stopping the gate.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use File::Spec;
use FindBin qw($RealBin);

my $root = File::Spec->rel2abs("$RealBin/../..");
my $pcl  = "$root/pcl";

plan skip_all => "pcl not found"  if !-x $pcl;
plan skip_all => "sbcl not found" if !`which sbcl 2>/dev/null`;

plan tests => 12;

my $dir = tempdir(CLEANUP => 1);

sub write_prog {
    my ($name, $body) = @_;
    my $path = File::Spec->catfile($dir, $name);
    open my $fh, '>', $path or die "write $path: $!";
    print $fh $body;
    close $fh;
    return $path;
}

sub run_both {
    my ($prog, $pre) = @_;
    $pre //= '';
    my $perl = `$pre timeout 60 $^X '$prog' 2>&1`;
    my $pcl_out = `$pre timeout 100 '$pcl' '$prog' 2>&1`;
    $pcl_out =~ s/^PCL Runtime loaded\n//gm;
    return ($perl, $pcl_out);
}

# 1: the lock shapes.  A handle dropped (returned and let go, held by a dup that
# is then dropped, captured by a closure that is then dropped) releases its
# lock; one still held (in a hash, by a live dup, by a live closure) keeps it.
# LOCK_NB answers the question without blocking.
my $locks = write_prog('locks.pl', <<'PL');
use strict; use warnings;
use Fcntl qw(:flock);
my $f = "$ARGV[0]/locks.$$";
{ open my $w, ">", $f or die; print $w "x\n"; close $w }
sub lockfh { my ($mode, $how) = @_; open my $fh, $how // "<", $f or die; flock($fh, $mode) or die "flock: $!"; return $fh }
sub probe {
    my ($tag) = @_;
    open my $o, ">>", $f or die "open: $!";
    my $ok = flock($o, LOCK_EX | LOCK_NB);
    print "$tag: ", ($ok ? "got it" : "WOULD BLOCK"), "\n";
    close $o;
}
sub slurp { my $fh = lockfh(LOCK_SH); local $/; my $b = <$fh>; return $b }
slurp(); probe("dropped");
my %keep;
sub keepit { $keep{h} = lockfh(LOCK_SH); probe("kept-in-hash"); delete $keep{h}; 1 }
keepit(); probe("deleted-from-hash");
my $dup;
sub dupit { my $fh = lockfh(LOCK_SH); open $dup, "<&", $fh or die "dup: $!"; 1 }
dupit(); probe("dup-alive");
sub dropdup { undef $dup; 1 } dropdup(); probe("dup-dropped");
my $cl;
sub mkclos { my $fh = lockfh(LOCK_SH); $cl = sub { $fh }; 1 }
mkclos(); probe("live-closure");
sub dropcl { undef $cl; 1 } dropcl(); probe("closure-dropped");
sub mkcl { my $fh = lockfh(LOCK_SH); my $c = sub { scalar <$fh> }; $c->(); return 1 }
mkcl(); probe("dead-closure");
unlink $f;
PL
{
    my ($perl, $pcl_out) = run_both("$locks' '$dir");
    is($pcl_out, $perl, 'lock shapes: a dropped handle releases its flock, a held one keeps it');
}

# 2: a BLOCKING flock after the program dropped a handle that holds LOCK_EX on
# the same file -- perl gets the lock; PCL used to wait for itself forever.
# The dropped write handle's bytes are in the file afterwards.
my $block = write_prog('block.pl', <<'PL');
use strict; use warnings;
use Fcntl qw(:flock);
my $f = "$ARGV[0]/block.$$";
{ open my $w, ">", $f or die; print $w "x\n"; close $w }
sub exfh { open my $fh, ">>", $f or die; flock($fh, LOCK_EX) or die; return $fh }
sub usex { my $fh = exfh(); print $fh "y\n"; return 1 }
usex();
open my $r, "<", $f or die;
print "blocking flock: ", (flock($r, LOCK_SH) ? "got it" : "failed $!"), "\n";
close $r;
print "size: ", -s $f, "\n";
unlink $f;
PL
{
    my ($perl, $pcl_out) = run_both("$block' '$dir");
    is($pcl_out, $perl, 'a blocking flock after a dropped LOCK_EX handle: no self-deadlock, bytes flushed');
}

# 3-4: more opens than the descriptor limit, each handle ESCAPING its block
# (passed to a helper; returned by one).  Under `ulimit -n 256' PCL died at
# ~250 opens; the out-of-descriptors retry collects and goes on.  Every line a
# dropped write handle buffered is in the file once the program has exited.
my $fds = write_prog('fds.pl', <<'PL');
use strict; use warnings;
sub first_line { my ($h) = @_; my $l = <$h>; return length($l // '') }
my $n = 1000;
my $s = 0;
for my $i (1 .. $n) { open(my $fh, "<", $0) or die "open failed at i=$i: $!\n"; $s += first_line($fh) }
my $f = "$ARGV[0]/fds.out";
unlink $f;
sub app { open my $fh, ">>", $f or die "append failed: $!\n"; return $fh }
for my $i (1 .. $n) { my $fh = app(); print $fh "$i\n" }
print "done $n sum=$s\n";
PL
{
    my $pre = "ulimit -n 256 &&";
    my $out_f = "$dir/fds.out";
    my $perl = `bash -c "$pre timeout 60 $^X '$fds' '$dir'" 2>&1`;
    my $plines = do { open my $in, '<', $out_f or die; my @l = <$in>; scalar @l };
    unlink $out_f;
    my $pcl_out = `bash -c "$pre timeout 100 '$pcl' '$fds' '$dir'" 2>&1`;
    $pcl_out =~ s/^PCL Runtime loaded\n//gm;
    my $lines = -e $out_f ? do { open my $in, '<', $out_f or die; my @l = <$in>; scalar @l } : 0;
    is($pcl_out, $perl, 'ulimit -n 256: 1000 escaping read handles + 1000 escaping write handles');
    is($lines, $plines, 'every line a dropped write handle buffered reached the file (1000)');
}

# 5: a fork after a dropped write handle: its bytes are written ONCE.
my $fork = write_prog('fork.pl', <<'PL');
use strict; use warnings;
my $g = "$ARGV[0]/fork.out";
unlink $g;
sub gw { open my $fh, ">>", $g or die; print $fh "once\n"; return $fh }
sub gdrop { my $fh = gw(); return 1 }
gdrop();
my $pid = fork();
if ($pid == 0) { exit 0 }
waitpid($pid, 0);
open my $in, "<", $g or die; my @l = <$in>; close $in;
print "lines after fork: ", scalar(@l), "\n";
PL
{
    my ($perl, $pcl_out) = run_both("$fork' '$dir");
    is($pcl_out, $perl, 'fork after a dropped write handle: its bytes are written once');
}

# 6: a lock held through a lexical that is IN SCOPE but never used again (the
# lock-guard idiom) stays held while its scope runs, through collections.  The
# collector cannot tell such a guard from a dropped handle (SBCL keeps no dead
# variable alive), so a finalizer that CLOSED at the collection released it at
# a random moment -- Path-Tiny locking.t, s541; SBCL's own `open' finalizer did
# the same on main for the open spelling.  A child asks with LOCK_NB.
my $guard = write_prog('guard.pl', <<'PL');
use strict; use warnings; use Fcntl qw(:flock O_WRONLY O_CREAT);
my $b = "$ARGV[0]/guard.lock";
sub child_got { system($^X, "-e", 'use Fcntl ":flock"; open my $h, "<", $ARGV[0] or die; exit(flock($h, LOCK_SH|LOCK_NB) ? 1 : 0)', $b); $? >> 8 }
sub churn { my @junk; push @junk, [ (1) x 50 ] for 1 .. 300_000; select(undef, undef, undef, 0.2) }
sub work { sysopen(my $g, $b, O_WRONLY|O_CREAT, 0666) or die; flock($g, LOCK_EX) or die; churn(); return child_got() }
sub guard { open(my $g, ">", $b) or die; flock($g, LOCK_EX) or die; return $g }
sub work2 { my $guard = guard(); churn(); return child_got() }
print "sysopen in a sub: ", work(), "\n";
print "guard from a helper: ", work2(), "\n";
PL
{
    my ($perl, $pcl_out) = run_both("$guard' '$dir");
    is($pcl_out, $perl, 'a lock held by an in-scope lexical guard survives collections (no child gets it)');
}

# 7: #4794 (s543b) -- `undef $fh;' / `undef($fh);' / `$fh = undef;' on a
# handle that ESCAPED its opening block (a helper returned it) FLUSHES it right
# there: the read-back idiom sees the bytes (perl closes at the last reference;
# PCL, without reference counts, writes the buffer and never closes).  At file
# level too; a tied scalar is not FETCHed.  The handle-in-a-hash `delete' and
# the object holding a handle are NOT covered (no statement names the handle).
# INVERSE: main c5f618f2 printed 0 for rows a b c d.
my $undef = write_prog('undef.pl', <<'PL');
use strict; use warnings;
my $f = "$ARGV[0]/undef.out";
sub mk { open my $fh, ">", $f or die; $fh }
sub size { -s $f // 0 }
{ my $fh = mk(); print $fh "hello";  undef $fh;   print "a ", size(), "\n"; }
{ my $fh = mk(); print $fh "hello!"; $fh = undef; print "b ", size(), "\n"; }
{ my $fh = mk(); print $fh "paren7"; undef($fh);  print "c ", size(), "\n"; }
{ open my $fh, ">", $f or die; print $fh "local77"; undef $fh; print "e ", size(), "\n"; }
{ package TS; our @log; sub TIESCALAR { bless {}, shift } sub FETCH { push @log, "F"; 1 } sub STORE { push @log, "S" } }
{ tie my $t, "TS"; undef $t; $t = undef; print "f ", join(",", @TS::log), "\n"; }
my $top = mk(); print $top "file13"; undef $top; print "d ", size(), "\n";
PL
{
    my ($perl, $pcl_out) = run_both("$undef' '$dir");
    is($pcl_out, $perl, '#4794: undef / = undef of an escaped handle flushes it (read-back sees the bytes)');
}
# 8: #5128 (s543g) -- the same statements INSIDE A SUB (or a closure, a
# nested sub, on a `state` scalar) on a lexical the sub CAPTURES: the binder
# answers "a my / state scalar" wherever it was declared, and the statement
# flushes it.  An inner `my` of the same name is the one dropped.  Line h: a
# lexical the block-package requalifier spells `$main::p` (a `package Q { }`
# in its block) is flushed through its lowered form `main::$p` -- the first
# cut emitted the token's spelling and the reader died (perl-tests/scalar.t,
# caught by the sweep).  INVERSE: main 8959afae printed 0 on every row.
my $capt = write_prog('capt.pl', <<'PL');
use strict; use warnings; use feature "state";
my $f = "$ARGV[0]/capt.out"; my $f2 = "$ARGV[0]/capt2.out";
sub mk { open my $fh, ">", $_[0] // $f or die; $fh }
sub size { -s ($_[0] // $f) // 0 }
my $g = mk(); sub drop_g { undef $g; print "a ", size(), "\n" } print $g "first"; drop_g();
my $h = mk(); sub drop_h { $h = undef; print "b ", size(), "\n" } print $h "second!"; drop_h();
my $k = mk(); sub drop_k { undef($k) } print $k "third...."; drop_k(); print "c ", size(), "\n";
my $m = mk(); sub outer { my $x = sub { inner() }; $x->() } sub inner { undef $m; print "d ", size(), "\n" }
print $m "nest"; outer();
my $n = mk(); print $n "closure"; my $c = sub { $n = undef; print "e ", size(), "\n" }; $c->();
sub st { state $s = mk(); print $s "state!"; my $d = sub { undef $s; print "f ", size(), "\n" }; $d->() } st();
my $o = mk(); print $o "outer";
sub sh { my $o = mk($f2); print $o "inner!"; my $e = sub { undef $o; print "g ", size(), " ", size($f2), "\n" }; $e->() } sh();
{ my $p = mk(); package QQ { use overload fallback => 1, q("") => sub { "q" } } print $p "pkgblk"; undef $p; print "h ", size(), "\n"; }
PL
{
    my ($perl, $pcl_out) = run_both("$capt' '$dir");
    is($pcl_out, $perl, '#5128: undef / = undef of a CAPTURED lexical inside a sub flushes the escaped handle');
}
# 9: #5160 (s543g) -- a GLOB handle (`IO::File->new`, `Symbol::gensym` + open,
# an fdopen alias `IO::Handle->new_from_fd`) dropped by `undef $x;` / `$x =
# undef;` is flushed through the glob's IO slot (the handle table, no method
# called; a tied handle runs no FETCH / CLOSE).  INVERSE: main 8959afae printed
# 0 for a b c d e.
my $glob = write_prog('glob.pl', <<'PL');
use strict; use warnings; use IO::File; use IO::Handle; use Symbol;
my $f = "$ARGV[0]/glob.out";
sub size { -s $f // 0 }
my $io = IO::File->new($f, "w"); print $io "io\n"; undef $io; print "a ", size(), "\n";
$io = IO::File->new($f, "w"); $io->print("io2\n"); $io = undef; print "b ", size(), "\n";
my $g = gensym; open $g, ">", $f or die; print $g "gensym"; undef $g; print "c ", size(), "\n";
open my $raw, ">", $f or die; my $h = IO::Handle->new_from_fd(fileno($raw), "w"); print $h "fromfd!";
undef $h; print "d ", size(), "\n"; close $raw;
sub w { my $s = IO::File->new($f, "w"); print $s "scoped12"; return $s } my $k = w(); undef $k; print "e ", size(), "\n";
{ package TH; our @log; sub TIEHANDLE { bless {}, shift } sub PRINT { push @log, "P"; 1 } sub FETCH { push @log, "F" } sub CLOSE { push @log, "C" } }
my $t = gensym; tie *$t, "TH"; print $t "x"; undef $t; print "f @TH::log\n";
PL
{
    my ($perl, $pcl_out) = run_both("$glob' '$dir");
    is($pcl_out, $perl, '#5160: undef / = undef of a GLOB handle (IO::File, gensym, an fdopen alias) flushes it');
}

# 10: #4794 shapes 4 and 5 (s543g) -- `delete $h{K};` / `delete $r->{K};` as a
# void statement flushes the dropped element (the delete's own value, read
# once: a tied hash's DELETE runs once, no FETCH), and `undef $obj;` /
# `$obj = undef;` flushes a handle held ONE level inside the dropped hash or
# array (an object holding its handle: Pod::Simple::HTMLBatch's `undef($page)`);
# a handle two levels down is not reached.  INVERSE: main 8959afae printed 0
# for a b c d e f.
my $obj = write_prog('obj.pl', <<'PL');
use strict; use warnings; use IO::File;
my $f = "$ARGV[0]/obj.out";
sub mk { open my $fh, ">", $f or die; $fh }
sub size { -s $f // 0 }
{ my %h; $h{fh} = mk(); print { $h{fh} } "inhash88"; delete $h{fh}; print "a ", size(), "\n"; }
{ my $r = { fh => mk() }; print { $r->{fh} } "hashref9"; delete $r->{fh}; print "b ", size(), "\n"; }
{ my $o = bless { fh => mk() }, "Obj"; print { $o->{fh} } "object1010"; undef $o; print "c ", size(), "\n"; }
{ my $o = bless [1, mk()], "Arr"; print { $o->[1] } "blessarr"; $o = undef; print "d ", size(), "\n"; }
{ my $o = { io => IO::File->new($f, "w") }; $o->{io}->print("ioobj"); undef $o; print "e ", size(), "\n"; }
{ my %h = (io => IO::File->new($f, "w")); $h{io}->print("iohash!"); delete $h{io}; print "f ", size(), "\n"; }
my %d = (a => 1, b => 2); delete $d{a}; print "g ", join(",", sort keys %d), "\n";
PL
{
    my ($perl, $pcl_out) = run_both("$obj' '$dir");
    is($pcl_out, $perl, '#4794: delete $h{fh} / delete $r->{fh} and undef $obj (one level) flush the dropped handle');
}

# 11: #5254 (s543j) -- the same statements on a PACKAGE variable: an `our`
# alias (in a sub), a qualified `$Foo::fh`, a `local`ized global, a `use vars`
# one, an object in a package variable, another package's `our`; the core's
# special variables (`$@` `$_` `$/` `$!`) keep their plain emission.
# INVERSE: main b58114ca printed 0 for a b c d e f g.
my $pkg = write_prog('pkg.pl', <<'PL');
use strict; use warnings; use vars qw($uv);
my $f = "$ARGV[0]/pkg.out";
sub mk { open my $fh, ">", $f or die; $fh }
sub size { -s $f // 0 }
our $g = mk(); print $g "our1"; sub dg { undef $g; print "a ", size(), "\n" } dg();
$Foo::fh = mk(); print {$Foo::fh} "pkg22"; undef $Foo::fh; print "b ", size(), "\n";
our $l = 5; { local $l = mk(); print $l "loc333"; undef $l; print "c ", size(), "\n"; }
$uv = mk(); print $uv "vars4444"; $uv = undef; print "d ", size(), "\n";
our $o = { fh => mk() }; print { $o->{fh} } "object555"; undef $o; print "e ", size(), "\n";
{ package Bar; our $q = main::mk(); print $q "barq66"; undef $q; print "f ", main::size(), "\n"; }
our $t = mk(); print $t "assign777"; $t = undef; print "g ", size(), "\n";
$@ = "x"; undef $@; $_ = "y"; $_ = undef; { local $/; undef $/; } print "h ", (defined $@ ? "D" : "U"), (defined $_ ? "D" : "U"), "\n";
PL
{
    my ($perl, $pcl_out) = run_both("$pkg' '$dir");
    is($pcl_out, $perl, '#5254: undef / = undef of a PACKAGE variable (our, $Foo::fh, local, use vars) flushes the escaped handle');
}

# 12: #5255 (s543k) -- the same statements INSIDE A STRING EVAL on the eval
# SITE's lexical (the binder's kind `outer`: the thunk parameter bound to the
# caller's container): a file lexical, a sub's lexical, `= undef`, a nested
# eval, a closure's capture, `evalbytes`, a handle one level inside an object;
# the text's own `my` and a non-handle are the controls.  INVERSE: main
# 4a0fd0ec printed 0 for a b c d e f.
my $evs = write_prog('evs.pl', <<'PL');
use strict; use warnings; use feature 'evalbytes';
my $f = "$ARGV[0]/evs.out";
sub mk { open my $fh, ">", $f or die; $fh }
sub size { -s $f // 0 }
my $g = mk(); print $g "evalx"; sub da { eval q{ undef $g; }; print "a ", size(), "\n" } da();
sub db { my $h = mk(); print $h "subxy"; eval q{ $h = undef; }; print "b ", size(), "\n" } db();
sub dc { my $h = mk(); print $h "nested"; eval q{ eval q{ undef $h; }; }; print "c ", size(), "\n" } dc();
my $cl = do { my $h = mk(); print $h "closure!"; sub { print $h ""; eval q{ undef $h; }; size() } }; print "d ", $cl->(), "\n";
sub de { my $h = mk(); print $h "bytes"; evalbytes q{ undef $h; }; print "e ", size(), "\n" } de();
sub df { my $o = { fh => mk() }; print { $o->{fh} } "inobj"; eval q{ undef $o; }; print "f ", size(), "\n" } df();
sub dg { my $h = mk(); print $h "g1"; eval q{ my $k = mk(); print $k "own-my"; undef $k; print "g ", size(), "\n"; } } dg();
sub dh { my $x = 5; eval q{ undef $x; }; print "h ", defined($x) ? "def" : "undef", "\n" } dh();
PL
{
    my ($perl, $pcl_out) = run_both("$evs' '$dir");
    is($pcl_out, $perl, '#5255: undef / = undef inside a string eval of the SITE\'s lexical flushes the escaped handle');
}

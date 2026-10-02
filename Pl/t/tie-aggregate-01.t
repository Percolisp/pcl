#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# tie-aggregate-01.t — task #155: `tie` on an ARRAY and on a HASH.
#
# The representation (docs/tie-aggregates.md): a tied container stays the SAME
# Lisp object, EMPTIED, and its tie lives in a weak side table guarded by a
# global COUNT.  What matters to a program is not the representation but the
# METHOD-CALL LOG: which of FETCH / STORE / FETCHSIZE / FIRSTKEY / NEXTKEY /
# EXISTS / DELETE / CLEAR / PUSH / ... perl calls for each operation, in which
# order and how many times.  So each probe below runs a logging Tie::StdHash /
# Tie::StdArray subclass and prints, per operation, the log and the result;
# every printed line is one row, and the expectation is the live `perl`
# answer to the same program.
#
# NAMED DIFFERENCES (docs/tie-aggregates.md, "the method-call table"): a few
# operations call one method more or fewer than perl -- the answer is the same,
# the log is not.  Those rows compare the RESULT only (after `=>`), and the
# table below says why; a change that closes one makes its row compare the
# whole line again (delete it from %RESULT_ONLY).
#
# THE FILE IS TWO SBCL SPAWNS (one per program) — its cost is its wall time.

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

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($pl_file) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl $pl_file");
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# label => why only the result is compared
my %RESULT_ONLY = (
  'h:preinc'        => 'perl re-FETCHes to return ++$h{a}; PCL returns the stored value',
  'h:values-sort'   => 'sort reads the lazy value proxies in COMPARISON order, perl FETCHes in key order',
  'h:delete-local'  => 'PCL localizes (STORE undef) before the delete; perl deletes directly',
  'a:for-alias'     => 'perl re-asks FETCHSIZE every iteration; PCL takes the size once',
  'a:for-read'      => 'perl re-asks FETCHSIZE every iteration; PCL takes the size once',
  'a:assign-count'  => 'scalar(@t = LIST): PCL counts through FETCHSIZE, perl counts the RHS',
  'a:push-void'     => 'PCL cannot see VOID context at a call argument, so it always asks FETCHSIZE',
  'a:untie-restores' => 'the push inside it asks FETCHSIZE (a:push-void\'s reason)',
  'a:list-assign-list' => 'the list-context value of the assignment reads the tied target through its view, which asks FETCHSIZE',
  'a:copy-sub'      => q{perl's `my ($x, $y) = @_` copies ALL of @_ (FETCH 1..N-1, then 0); PCL's copy FETCHes only the parameters it binds},
);

sub compare_program {
    my ($name, $code) = @_;
    my $pl = write_pl($code);
    my @perl = split /\n/, `perl $pl 2>&1`;
    my @pcl  = split /\n/, run_cl($pl);
    is(scalar(@pcl), scalar(@perl), "$name: PCL prints as many lines as perl");
    for my $i (0 .. $#perl) {
        my $want = $perl[$i];
        my $got  = $pcl[$i] // '<missing>';
        my ($label) = $want =~ /^\S+\s+(\S+)/;
        $label //= "line $i";
        if (my $why = $RESULT_ONLY{$label}) {
            my ($w) = $want =~ /=> (.*)$/;
            my ($g) = $got  =~ /=> (.*)$/;
            is($g, $w, "$name $label: result (log differs by design: $why)");
        }
        else {
            is($got, $want, "$name $label: $want");
        }
    }
}

my $common = <<'PERL';
use strict; use warnings;
my $n = 0;
sub op { my ($label, $code) = @_; @main::LOG = (); my @r = eval { $code->() };
  my $e = $@ ? " DIED:" . ($@ =~ s/ at .*//sr) : "";
  printf "%02d %s [%s] => %s%s\n", ++$n, $label, join(" ", @main::LOG),
    join(",", map { defined $_ ? $_ : "undef" } @r), $e }
sub _a { join(",", map { defined $_ ? (ref $_ ? ref $_ : s{\n}{\\n}gr) : "undef" } @_) }
PERL

my $hash_prog = $common . <<'PERL';
package LH;
require Tie::Hash;
our @ISA = ('Tie::StdHash');
our %IT;
sub TIEHASH  { my $c = shift; push @main::LOG, "TIEHASH(" . main::_a(@_) . ")"; bless {}, $c }
sub FETCH    { push @main::LOG, "FETCH(" . main::_a($_[1]) . ")"; $_[0]->SUPER::FETCH($_[1]) }
sub STORE    { push @main::LOG, "STORE(" . main::_a($_[1]) . "," . main::_a($_[2]) . ")"; $_[0]->SUPER::STORE($_[1], $_[2]) }
sub EXISTS   { push @main::LOG, "EXISTS($_[1])"; $_[0]->SUPER::EXISTS($_[1]) }
sub DELETE   { push @main::LOG, "DELETE($_[1])"; $_[0]->SUPER::DELETE($_[1]) }
sub CLEAR    { push @main::LOG, "CLEAR"; $_[0]->SUPER::CLEAR() }
sub FIRSTKEY { push @main::LOG, "FIRSTKEY"; $IT{$_[0]} = [sort keys %{$_[0]}]; shift @{$IT{$_[0]}} }
sub NEXTKEY  { push @main::LOG, "NEXTKEY($_[1])"; shift @{$IT{$_[0]}} }
sub SCALAR   { push @main::LOG, "SCALAR"; scalar %{$_[0]} }
sub UNTIE    { push @main::LOG, "UNTIE" }
package SelfT;
sub TIEHASH { bless $_[1], $_[0] }
package main;
my %pre = (old => 1);
op("h:tie", sub { my $o = tie %pre, 'LH', 'x'; ref $o });
op("h:hidden", sub { exists $pre{old} ? "vis" : "hid" });
op("h:write", sub { $pre{a} = 1; () });
op("h:read", sub { $pre{a} });
op("h:read-missing", sub { $pre{zz} });
op("h:postinc", sub { $pre{a}++ });
op("h:preinc", sub { ++$pre{a} });
op("h:concat-assign", sub { $pre{s} .= "x"; () });
op("h:or-assign", sub { $pre{o} ||= 5; () });
op("h:or-assign-set", sub { $pre{o} ||= 6; () });
op("h:add-assign", sub { $pre{a} += 10; () });
op("h:interp-twice", sub { "$pre{a}$pre{a}" });
op("h:exists", sub { exists $pre{a} ? 1 : 0 });
op("h:exists-no", sub { exists $pre{q} ? 1 : 0 });
op("h:delete", sub { delete $pre{o} });
op("h:keys", sub { join "", sort keys %pre });
op("h:keys-scalar", sub { scalar keys %pre });
op("h:values-sort", sub { join "", sort values %pre });
op("h:each", sub { my @x; while (my ($k, $v) = each %pre) { push @x, "$k=$v" } join ";", @x });
op("h:scalar", sub { scalar(%pre) ? "t" : "f" });
op("h:bool", sub { %pre ? "t" : "f" });
op("h:slice-read", sub { join "|", map { $_ // "u" } @pre{qw(a s)} });
op("h:slice-write", sub { @pre{qw(p q)} = (7, 8); () });
op("h:flatten", sub { my %c = %pre; join ",", map { "$_=$c{$_}" } sort keys %c });
op("h:list-assign", sub { %pre = (m => 1, n => 2); () });
op("h:list-assign-count", sub { scalar(%pre = (m => 1, n => 2, m => 3)) });
op("h:for-values-alias", sub { $_ .= "v" for values %pre; () });
op("h:sub-arg-alias", sub { my $f = sub { $_[0] = "w" }; $f->($pre{m}); () });
op("h:ref-elem", sub { my $r = \$pre{n}; $$r = "r"; () });
op("h:ref-elem-read", sub { my $r = \$pre{n}; $$r });
op("h:local-elem", sub { { local $pre{m} = "L"; push @main::LOG, "--"; } () });
op("h:delete-local", sub { { delete local $pre{m}; push @main::LOG, "--"; } () });
op("h:local-missing", sub { { local $pre{nokey}; push @main::LOG, "--"; } () });
op("h:self-tie", sub { my %c; tie %c, 'LH'; untie %c; my $o = eval { no warnings; tie %c, 'SelfT', \%c; 1 }; $@ =~ /^(Self-ties of arrays and hashes are not supported)/ ? $1 : "no die" });
op("h:tied", sub { ref tied(%pre) });
op("h:map-read", sub { join ",", map { $pre{$_} } qw(m n) });
op("h:delete-slice", sub { join ",", map { $_ // "u" } delete @pre{qw(m zz)} });
op("h:clear", sub { %pre = (); () });
op("h:untie", sub { untie %pre; () });
op("h:after-untie", sub { join ",", map { "$_=$pre{$_}" } sort keys %pre });
op("h:tied-after", sub { defined tied(%pre) ? "t" : "u" });
op("h:untie-restores", sub { my %x = (k => 1); tie %x, 'LH'; $x{n} = 2; untie %x; join ",", %x });
op("h:retie", sub { tie %pre, 'LH'; tie %pre, 'LH'; $pre{z} = 1; () });
op("h:tie-ref", sub { my $r = {}; tie %$r, 'LH'; $r->{k} = 2; $r->{k} });
op("h:tie-symbolic", sub { no strict 'refs'; tie %{"main::G"}, 'LH'; $main::G{g} = 3; $main::G{g} });
op("h:tie-alias-before", sub { my %x; my $r = \%x; tie %x, 'LH'; $r->{k} = 4; $x{k} });
op("h:tie-in-eval", sub { my $r = eval q{ my %e; tie %e, 'LH'; $e{e} = 5; $e{e} }; $r });
op("h:nested-autoviv", sub { my %x; tie %x, 'LH'; $x{a}{b} = 1; ref $x{a} });
op("h:return", sub { my %x; tie %x, 'LH'; %x = (a => 1); my $f = sub { %x }; my %c = $f->(); join ",", %c });
op("h:push-autoviv", sub { my %x; tie %x, 'LH'; push @{$x{l}}, 1, 2; scalar @{$x{l}} });
op("h:store-count", sub { my %x; tie %x, 'LH'; my @q = (1, 2, 3); $x{c} = @q; $x{c} });
op("h:ref-key-store", sub { my %x; tie %x, 'LH'; my $k = \"s"; $x{$k} = 1; () });
op("h:undef-key-fetch", sub { my %x; tie %x, 'LH'; no warnings; () = $x{+undef}; () });
op("h:refhash", sub { require Tie::RefHash; tie my %x, 'Tie::RefHash'; my $k = [1]; $x{$k} = 5; ref((keys %x)[0]) . ":" . $x{$k} });
op("h:anon-copy", sub { my %x; tie %x, 'LH'; $x{a} = 1; my $c = {%x}; join ",", %$c });
# s504 seam (s501q args-copy lever): a NAMED sub that only copies @_ binds its
# parameters from the flattened argument VALUES -- a tied hash must spread there too
sub h_copy_sub { my (%o) = @_; join ",", map { "$_=$o{$_}" } sort keys %o }
op("h:copy-sub", sub { my %x; tie %x, 'LH'; $x{a} = 1; $x{b} = 2; h_copy_sub(%x) });
PERL

my $array_prog = $common . <<'PERL';
package LA;
require Tie::Array;
our @ISA = ('Tie::StdArray');
sub TIEARRAY  { my $c = shift; push @main::LOG, "TIEARRAY(" . main::_a(@_) . ")"; bless [], $c }
sub FETCH     { push @main::LOG, "FETCH($_[1])"; $_[0]->SUPER::FETCH($_[1]) }
sub STORE     { push @main::LOG, "STORE($_[1]," . main::_a($_[2]) . ")"; $_[0]->SUPER::STORE($_[1], $_[2]) }
sub FETCHSIZE { push @main::LOG, "FETCHSIZE"; $_[0]->SUPER::FETCHSIZE() }
sub STORESIZE { push @main::LOG, "STORESIZE($_[1])"; $_[0]->SUPER::STORESIZE($_[1]) }
sub EXTEND    { push @main::LOG, "EXTEND($_[1])"; $_[0]->SUPER::EXTEND($_[1]) }
sub EXISTS    { push @main::LOG, "EXISTS($_[1])"; $_[0]->SUPER::EXISTS($_[1]) }
sub DELETE    { push @main::LOG, "DELETE($_[1])"; $_[0]->SUPER::DELETE($_[1]) }
sub CLEAR     { push @main::LOG, "CLEAR"; $_[0]->SUPER::CLEAR() }
sub PUSH      { my $s = shift; push @main::LOG, "PUSH(" . main::_a(@_) . ")"; $s->SUPER::PUSH(@_) }
sub POP       { push @main::LOG, "POP"; $_[0]->SUPER::POP() }
sub SHIFT     { push @main::LOG, "SHIFT"; $_[0]->SUPER::SHIFT() }
sub UNSHIFT   { my $s = shift; push @main::LOG, "UNSHIFT(" . main::_a(@_) . ")"; $s->SUPER::UNSHIFT(@_) }
sub SPLICE    { my $s = shift; push @main::LOG, "SPLICE(" . main::_a(@_) . ")"; $s->SUPER::SPLICE(@_) }
sub UNTIE     { push @main::LOG, "UNTIE" }
package NI;
our @ISA = ('Tie::StdArray');
our $NEGATIVE_INDICES = 1;
sub FETCH { push @main::LOG, "NI-FETCH($_[1])"; "neg" }
package main;
my @a = (7, 8);
op("a:tie", sub { my $o = tie @a, 'LA', 'x'; ref $o });
op("a:hidden", sub { scalar(@a) });
op("a:write0", sub { $a[0] = 1; () });
op("a:write2", sub { $a[2] = 3; () });
op("a:read", sub { $a[0] });
op("a:read-oob", sub { $a[9] });
op("a:read-neg", sub { $a[-1] });
op("a:write-neg", sub { $a[-1] = 4; () });
op("a:postinc", sub { $a[0]++ });
op("a:concat", sub { $a[1] .= "c"; () });
op("a:or-assign", sub { $a[1] ||= 9; () });
op("a:length", sub { scalar(@a) });
op("a:lastidx", sub { $#a });
op("a:set-lastidx", sub { $#a = 4; () });
op("a:shrink-lastidx", sub { $#a = 2; () });
op("a:push", sub { push @a, 5, 6 });
op("a:pop", sub { pop @a });
op("a:shift", sub { shift @a });
op("a:unshift", sub { unshift @a, 0 });
op("a:splice", sub { join "|", map { $_ // "u" } splice(@a, 1, 1, "s1", "s2") });
op("a:exists", sub { exists $a[1] ? 1 : 0 });
op("a:delete", sub { delete $a[1] });
op("a:join", sub { join ",", map { $_ // "u" } @a });
op("a:copy", sub { my @c = @a; scalar @c });
op("a:for-alias", sub { for (@a) { $_ = "f" } () });
op("a:for-read", sub { my $s = ""; for my $e (@a) { $s .= $e } $s });
op("a:map", sub { join ",", map { $_ . "m" } @a });
op("a:grep", sub { scalar grep { $_ eq "f" } @a });
op("a:sort", sub { join ",", sort @a });
op("a:reverse", sub { join ",", reverse @a });
op("a:list-assign", sub { @a = (1, 2, 3); () });
op("a:assign-count", sub { scalar(@a = (4, 5)) });
op("a:slice-read", sub { join ",", @a[0, 1] });
op("a:slice-write", sub { @a[0, 1] = (8, 9); () });
op("a:sub-arg-alias", sub { my $f = sub { $_[0] = "w" }; $f->($a[0]); () });
op("a:sub-args-flat", sub { my $f = sub { scalar @_ }; $f->(@a) });
op("a:ref-elem", sub { my $r = \$a[1]; $$r = "r"; () });
op("a:local-elem", sub { { local $a[0] = "L"; push @main::LOG, "--"; } () });
op("a:interp", sub { "@a" });
op("a:interp-elem", sub { "$a[0]$a[0]" });
op("a:tied", sub { ref tied(@a) });
op("a:chomp", sub { @a = ("x\n", "y"); chomp(@a); "@a" });
op("a:clear", sub { @a = (); () });
op("a:untie", sub { untie @a; () });
op("a:after-untie", sub { "[@a]" });
op("a:untie-restores", sub { my @x = (1, 2); tie @x, 'LA'; push @x, 9; untie @x; "@x" });
op("a:push-void", sub { my $r = []; tie @$r, 'LA'; push @$r, 1; $r->[0] });
op("a:neg-oob", sub { my @x; tie @x, 'LA'; my $v = $x[-1]; defined $v ? $v : "u" });
op("a:neg-oob-write", sub { my @x; tie @x, 'LA'; $x[-1] = 1; "lived" });
op("a:each", sub { my @x; tie @x, 'LA'; @x = (1, 2); my @p; while (my ($i, $v) = each @x) { push @p, "$i$v" } "@p" });
op("a:negative-indices", sub { my @x; tie @x, 'NI'; $x[-2] });
op("a:return", sub { my @x; tie @x, 'LA'; @x = (1, 2); my $f = sub { return @x }; my @c = $f->(); my $s = $f->(); "@c|$s" });
op("a:anon-copy", sub { my @x; tie @x, 'LA'; @x = (3); my $c = [@x]; "@$c" });
op("a:defelem-then-tie", sub { our @g; my $r = sub { tie @g, 'LA'; $#g = 20; $g[10] = "crumpets"; "$_[0]" }->($g[10]); $r });
op("a:range-fill", sub { my @x; tie @x, 'LA'; @x = 1 .. 3; "@x" });
op("a:reverse-inplace", sub { my @x; tie @x, 'LA'; @x = (1, 2, 3, 4); delete $x[1]; @x = reverse @x; join ",", map { exists $x[$_] ? $x[$_] : "-" } 0 .. 3 });
op("a:list-assign-list", sub { my @x; tie @x, 'LA'; my @r = ((my $f), @x) = (1, 2, 3); scalar(@r) . ":@x" });
op("a:deref-assign", sub { my @x; tie @x, 'LA'; my $r = \@x; @$r = (5, 6); "@x" });
# s504 seam (s501q args-copy / sig-classic levers): a tied array passed to a
# NAMED copying sub and to a signature sub spreads through its methods
sub a_copy_sub { my ($x, $y) = @_; "$x,$y" }
sub a_shift_sub { my $x = shift; my $y = shift; "$x,$y" }
use feature "signatures"; no warnings "experimental::signatures";
sub a_sig_sub ($x, $y = "d", @r) { "$x,$y,@r" }
sub a_sig_exact ($x, $y, $z) { "$z,$y,$x" }
op("a:copy-sub", sub { my @x; tie @x, 'LA'; @x = (1, 2, 3); a_copy_sub(@x) });
op("a:shift-sub", sub { my @x; tie @x, 'LA'; @x = (1, 2, 3); a_shift_sub(@x) });
op("a:sig-sub", sub { my @x; tie @x, 'LA'; @x = (1, 2, 3); a_sig_sub(@x) });
op("a:sig-exact", sub { my @x; tie @x, 'LA'; @x = (1, 2, 3); a_sig_exact(@x) });
# s504 (op/gmagic.t:87): `tie ${EXPR}` names the scalar's BOX (p-cast-$-box) --
# an undef operand is an LVALUE and vivifies a SCALAR ref, and a hard ref names
# the referent variable itself (so `tied $x` sees `tie $$rx`)
op("s:tie-deref-viv", sub { require Tie::Scalar; my $s; my $o = tie $$s, 'Tie::StdScalar'; $$s = 4; ref($o) . "|" . ref(tied $$s) . "|" . ref($s) . "|$$s" });
op("s:tie-deref-ref", sub { require Tie::Scalar; my $x; my $rx = \$x; tie $$rx, 'Tie::StdScalar'; $x = 6; ref(tied $x) . "|$$rx" });
PERL

compare_program('hash', $hash_prog);
compare_program('array', $array_prog);

done_testing();

#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# b-shim-01.t -- task #3281 (s518e): the core B module without XS (lib/B.pm).
# Before it, `require B` died "Can't locate B.pm (the module is XS ...)" --
# Type::Tiny's load blocker -- so every row here fails on the base.  The string
# quoters must be BYTE-identical to perl (generated code is eval'ed), over all
# 256 byte values and wide strings; svref_2object answers the sub-name and
# scalar-representation facts from runtime primitives.  Each program prints
# LABELLED lines, compared with perl (nothing needs 5.40).  The rows at the end
# are PCL-only: what B cannot do here DIES naming itself (docs/not-supported.md
# "B"), where perl answers.

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
sub row { my ($l, @v) = @_; print "$l: ", join(' | ', @v), "\n" }
sub trap { my $c = shift; my $r = eval { $c->(); 1 };
           return $r ? 'lived' : 'died:' . ($@ =~ s/ at \S+ line \d+\.?\n?//sr =~ s/\n\z//r) }
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

# 1. perlstring / cstring over every byte value, then wide strings (perl's
#    UTF-8 display form for perlstring, the UTF-8 BYTES for cstring), the
#    trigraph guard, undef and numbers.
compare_lines(<<'P', 'quote');
use B ();
row("byte $_", B::perlstring(chr $_), B::cstring(chr $_)) for 0 .. 255;
my @w = ("\x{263a}", "a\x{100}b",
         "\x{263a}\x{e9}\n\t\"\$\@\\x\x{1F600}\x01\x7f\e\a\b\f\r\x0b\0",
         "a??b??=", "", "\\n\\x", "mixed \$x and \@y");
row("wide $_", B::perlstring($w[$_]), B::cstring($w[$_])) for 0 .. $#w;
row('undef', B::perlstring(undef), B::cstring(undef));
row('numbers', B::perlstring(12), B::perlstring(1.5), B::cstring(-3));
row('roundtrip', (eval(B::perlstring("q\$\@\"\\\n\x{263a}")) eq "q\$\@\"\\\n\x{263a}") ? 1 : 0);
row('defined', defined &B::perlstring ? 1 : 0, defined &B::cstring ? 1 : 0);
P

# 2. svref_2object on subs: the name a sub was DEFINED under survives an
#    alias; an anonymous sub is __ANON__; a stub's START is B::NULL; a shim
#    sub (perl: XS) reports its name and package like perl's.
compare_lines(<<'P', 'cv');
use B ();
use Scalar::Util ();
package Foo; sub bar { 1 } sub stub;
package main;
sub named { 2 }
my $anon = sub { 3 };
BEGIN { *main::alias = \&Foo::bar; }
for my $p (['named', \&named], ['qualified', \&Foo::bar], ['alias', \&main::alias]) {
    my ($l, $c) = @$p;
    my $cv = B::svref_2object($c);
    row($l, ref($cv), $cv->GV->NAME, $cv->GV->SAFENAME, $cv->GV->STASH->NAME, $cv->STASH->NAME,
        $cv->XSUB ? 1 : 0, $cv->START->isa('B::NULL') ? 1 : 0, $cv->ROOT->isa('B::NULL') ? 1 : 0,
        $cv->isa('B::CV') ? 1 : 0, $cv->isa('B::SV') ? 1 : 0, $cv->GV->isa('B::SPECIAL') ? 1 : 0,
        B::class($cv), $cv->object_2svref == $c ? 1 : 0);
}
my $a = B::svref_2object($anon);
row('anon', $a->GV->NAME, $a->XSUB ? 1 : 0, $a->START->isa('B::NULL') ? 1 : 0);
row('stub', B::svref_2object(\&Foo::stub)->START->isa('B::NULL') ? 1 : 0);
my $bl = B::svref_2object(\&Scalar::Util::blessed);
row('shim sub', $bl->GV->NAME, $bl->GV->STASH->NAME);
row('not a ref', trap(sub { B::svref_2object(1) }));
# #3362: the CV flag bits PCL knows, MASKED as perl's own op/stash.t reads them
# (the unknown bits read 0 -- docs/not-supported.md "B").
sub lv :lvalue { my $x } sub meth :method { 1 }
for my $p (['flags named', \&named], ['flags anon', $anon], ['flags lvalue', \&lv], ['flags method', \&meth]) {
    my $f = B::svref_2object($p->[1])->CvFLAGS;
    row($p->[0], $f & B::CVf_ANON, $f & B::CVf_LVALUE, $f & B::CVf_METHOD);
}
# #3492: an anonymous sub's GV is PKG::__ANON__, PKG the package the `sub {}`
# was COMPILED in (a closure made in Foo and called from main is Foo's); a
# `\&{"name"}` stub lives in its name's package.
package Foo { our $fa = sub { 4 }; sub mk { my $x = shift; sub { $x } } }
package Foo::Bar; our $fb = sub { 5 };
package main;
my $stub = do { package Six; \&{"six"} };
for my $p (['anon main', $anon], ['anon Foo', $Foo::fa], ['anon Foo::Bar', $Foo::Bar::fb],
           ['closure Foo', Foo::mk(6)], ['eval Bar', do { package Bar; eval 'sub { 7 }' }]) {
    my $cv = B::svref_2object($p->[1]);
    row($p->[0], $cv->GV->STASH->NAME, $cv->STASH->NAME);
}
row('stub ref', B::svref_2object($stub)->GV->STASH->NAME);
P

# 3. svref_2object on scalars: the class and the OK flags of perl's SV.
compare_lines(<<'P', 'sv');
use B qw(svref_2object SVf_IOK SVf_NOK SVf_POK SVf_ROK);
my ($i, $n, $s, $u, $r, $neg) = (42, 1.5, "str", undef, [], -7);
my %c = (int => \$i, float => \$n, string => \$s, undef => \$u, ref => \$r, negative => \$neg);
for my $k (sort keys %c) {
    my $o = svref_2object($c{$k});
    row($k, ref($o), $o->FLAGS & (SVf_IOK | SVf_NOK | SVf_POK | SVf_ROK));
}
row('values', svref_2object(\$i)->IV, svref_2object(\$n)->NV, svref_2object(\$s)->PV,
    ref(svref_2object(\$r)->RV));
row('number vs string', (svref_2object(\"7")->FLAGS & SVf_POK) ? 1 : 0,
    (svref_2object(\7)->FLAGS & SVf_POK) ? 1 : 0);
row('safename', B::safename("\x14MP"), B::safename("x"));
P

# 3b. #3514: an imported constant is a TERM under every spelling -- the bare
#     one read the bareword string when lib/B.pm's export list was computed.
compare_lines(<<'P', 'bare constant');
no strict;   # the issue spelling: without strict a bareword is a string
use B qw(svref_2object CVf_ANON SVf_POK);
row('three spellings', B::CVf_ANON(), CVf_ANON(), CVf_ANON);
row('masked', (svref_2object(sub { 1 })->CvFLAGS & CVf_ANON) ? 'anon' : 'named');
row('operand', SVf_POK + 0, (svref_2object(\"x")->FLAGS & SVf_POK) ? 1 : 0);
P

# ... and the shim's three spellings of its constant list agree: the %CONST
# values, the literal @EXPORT_OK, the () declarations (lib/B.pm, #3514).
{
    open my $bh, '<', "$project_root/lib/B.pm" or die "lib/B.pm: $!";
    my $src = do { local $/; <$bh> };
    my ($const) = $src =~ /^my %CONST = \((.*?)^\);/ms;
    my ($export) = $src =~ /^our \@EXPORT_OK = qw\((.*?)\);/ms;
    my @c = sort $const =~ /(\w+)\s*=>/g;
    my @e = sort grep { /^(?:CVf|SV[fpst])_/ } split ' ', $export // '';
    my @d = sort $src =~ /sub ((?:CVf|SV[fpst])_\w+) \(\);/g;
    ok(@c > 30, 'B constants: the %CONST table was found');
    is("@e", "@c", 'B constants: the literal @EXPORT_OK names every %CONST key');
    is("@d", "@c", 'B constants: every %CONST key is declared with its () prototype');
}

# 4. PCL-only: what PCL cannot answer dies, perl-shaped and trappable.
{
    my $file = write_pl(<<'P');
use B ();
row('deparse', trap(sub { require B::Deparse }));
row('unknown function', trap(sub { B::main_cv() }));
my $cv = B::svref_2object(sub { 1 });
row('unrecorded stash', trap(sub { B::svref_2object(\&builtin::true)->STASH->NAME }));
row('unknown method', trap(sub { $cv->PADLIST }));
row('op accessor', trap(sub { B::svref_2object(\&B::perlstring)->START->line }));
row('after', 'still running');
P
    my $out = run_cl($file);
    like($out, qr/^deparse: died:B::Deparse is not supported by PCL/m, 'require B::Deparse dies, trappable');
    like($out, qr/^unknown function: died:B::main_cv is not supported by PCL/m, 'an unimplemented B function dies naming itself');
    like($out, qr/^unrecorded stash: died:B::CV::STASH of a sub with no recorded package is not supported by PCL/m, 'the stash of a sub no PCL `sub` created dies (an anonymous sub answers, #3492)');
    like($out, qr/^unknown method: died:B::CV::PADLIST is not supported by PCL/m, 'an unimplemented method dies naming itself');
    like($out, qr/^op accessor: died:B::COP::line is not supported by PCL/m, 'an op accessor dies');
    like($out, qr/^after: still running/m, 'every die was trappable');
}

done_testing;

#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# constant-scope-01.t -- task #3870: a `use constant` belongs to the PACKAGE
# its `use` compiles in.
#
# THE BUG (a silent wrong in every Log::Log4perl program): inside
# Log::Log4perl::Appender's `sub new`, `print "..." if _INTERNAL_DEBUG;` read
# the constant as the STRING "_INTERNAL_DEBUG" -- true -- so every program
# printed the appender's debug lines on STDOUT.  Reduced to the fixture below:
# a module A with `use constant _D => 0`, a sub reading `_D` ABOVE a
# `require A::B` (a module declaring the SAME constant name), and a later
# `no strict` (which only decides what an UNKNOWN bareword becomes: the string,
# not a call).
#
# THE MECHANISM: Parser2's `use constant` pre-merge registered the file's own
# constants under `main` (the package stack's value during the pre-pass), while
# the include pre-merge positions a `require`d module's records in the package
# the `require` compiles in (#3143) -- so A's table held the IMPORTED record
# from A::B, sited at the require, and no record of its own: a call above the
# require saw "unknown".  The pre-merge now registers in `_package_of_element`
# of the `use`, as the include pre-merge does.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use File::Path qw(make_path);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $pcl_bin      = "$project_root/pcl";

plan skip_all => "pl2cl not found" unless -x $pl2cl && -x $pcl_bin;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 20;

my $libdir = tempdir(CLEANUP => 1);
make_path("$libdir/C3870");

sub write_file {
    my ($path, $text) = @_;
    open(my $fh, '>', "$libdir/$path") or die "fixture $path: $!";
    print $fh $text;
    close $fh;
}

# A: its own constant, read in a sub ABOVE the require of a module that
# declares the same name, then a later `no strict` sub.
write_file('C3870/A.pm', <<'PM');
package C3870::A;
use strict;
use constant _D => 0;
sub new {
    print "debug-new\n" if _D;
    if (!$INC{'C3870/B.pm'}) {
        require C3870::B;
    }
    return bless {}, shift;
}
sub later { print "debug-later\n" if _D; 1 }
sub filter {
    no strict qw(vars);
}
1;
PM

# B: the SAME constant name with a DIFFERENT value -- each package reads its own.
write_file('C3870/B.pm', <<'PM');
package C3870::B;
use strict;
use constant _D => 1;
sub f { return _D ? "B-sees-1" : "B-sees-0" }
1;
PM

# The same shape with `package NAME { ... }` blocks and a value that is TRUE in
# the requiring module and FALSE in the required one.
write_file('C3870/Blk.pm', <<'PM');
package C3870::Blk {
    use constant FLAG => 7;
    sub get { return FLAG + 0 }
    sub load { require C3870::Blk2; return C3870::Blk2::get() }
    sub tail { no strict; 1 }
}
1;
PM
write_file('C3870/Blk2.pm', <<'PM');
package C3870::Blk2 {
    use constant FLAG => 0;
    sub get { return FLAG + 0 }
}
1;
PM

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "use lib '$libdir';\n$code";
    close $fh;
    return $pl_file;
}

# Run through `pcl`, the way a user runs a program: the module is transpiled
# at its `use` with the program's @INC, so the `require` inside it finds
# C3870::B and B's prototypes are merged.  A bare pl2cl + sbcl load does not
# reach that merge (measured: the bug did not reproduce there).
# The gate's transpile server serves that module transpile too: the runtime
# hands it the program's @INC as `perl -I DIR pl2cl --module`, and since task
# #4041 a SERVED request carries perl's -I list -- before it, the require
# inside the module did not find C3870::B under the server, so this row ran
# with `PCL_XSERVER=` (it passed on the unfixed tree otherwise).
sub both_agree {
    my ($code, $desc) = @_;
    my $pl   = write_pl($code);
    my $perl = `perl $pl 2>&1 </dev/null`;
    my $pcl  = `$pcl_bin $pl 2>&1 </dev/null`;
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- the module's own transpile: the constant is a CALL, never a string ----
{
    my $cl = PCLCore::transpile("PERL5LIB=$libdir $pl2cl --module < $libdir/C3870/A.pm");
    unlike($cl, qr/\(p-if "_D"/, '#3870: `_D` above the require is not the STRING "_D"');
    # The general form (`const-fold` off, task #4982): both reads CALL the
    # package constant.  By default both are its folded literal 0 -- which is
    # the same fact, read off the same record.
    my $gen = PCLCore::transpile("PCL_OPT=-const-fold PERL5LIB=$libdir $pl2cl --module < $libdir/C3870/A.pm");
    my $calls = () = $gen =~ /\(C3870::A::pl-_D\)|\(\|C3870::A\|::pl-_D\)/g;
    is($calls, 2, '... both reads of `_D` are calls of the package constant');
    my $folds = () = $cl =~ /\(p-if 0 \(p-print \(p-esc "debug-/g;
    is($folds, 2, '... and both are its literal 0 when folded (#4982)');
    unlike($cl, qr/\("main" "_D"/, '... and no `main` record of a constant `package C3870::A` declares');
}
{
    # The package a pre-merged registration lands in is _package_of_element's
    # answer, and a `package NAME { ... }` BLOCK above the `use` does not govern
    # it (perl-tests/index.t: `package o { use overload ... }` then
    # `use constant quire => ...` in main).
    my ($fh, $f) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh "package Blk3870 { sub x { 1 } }\nuse constant K3870 => 5;\nsub f { K3870 + 1 }\nprint f(), \"\\n\";\n";
    close $fh;
    my $cl = PCLCore::transpile("$pl2cl $f");
    unlike($cl, qr/\("Blk3870" "K3870"/, 'a `package NAME { }` block does not own a later `use constant`');
    like($cl, qr/\("main" "K3870" "" 2\)/, '... main does');
}

# ---- behaviour: what perl prints, nothing more ----------------------------
both_agree('use C3870::A; my $o = C3870::A->new; $o->later; print "made\n";',
           'a constant read above a `require` of a same-named constant is the constant');
both_agree('use C3870::A; C3870::A->new; print C3870::B::f(), "\n";',
           '... and the required module reads ITS OWN value (the inverse)');
both_agree('require C3870::A; C3870::A->new; print C3870::A::_D() + 0, " ", C3870::B::_D() + 0, "\n";',
           '... the two constants are two subs, each with its own value');
both_agree('use C3870::Blk; print C3870::Blk::get(), " ", C3870::Blk::load(), "\n";',
           '`package NAME { }` blocks: each block reads its own constant');

# ---- the single-file shapes, which were right and must stay right ---------
both_agree(<<'PL', 'two packages in one file, the same constant name, two values');
package P1; use constant K => 0; sub f { return K ? "P1-true" : "P1-false" }
package P2; use constant K => 1; sub g { return K ? "P2-true" : "P2-false" }
package main; print P1::f(), " ", P2::g(), "\n";
PL
both_agree(<<'PL', '... and the reverse values');
package P1; use constant K => 1; sub f { return K ? "P1-true" : "P1-false" }
package P2; use constant K => 0; sub g { return K ? "P2-true" : "P2-false" }
package main; print P1::f(), " ", P2::g(), "\n";
PL
both_agree(<<'PL', 'a main-file constant used inside a sub above a later package');
use constant TOP => 3;
sub t { return TOP * 2 }
package Q; use constant TOP => 5; sub u { return TOP * 2 }
package main; print t(), " ", Q::u(), "\n";
PL

# ---- #4795 (s543b): a constant used bare in ITS OWN package is resolved
# against that package's record -- a LATER package's `sub X` neither shadows it
# nor says "declared below".  The #266 classifier's declared-subs scan was
# package-blind, so Bar's `sub X` below made Foo's bare `X` a not-yet
# bareword: the STRING "X" (literal constants hid it behind the const-fold;
# PCL_OPT=-const-fold showed it for both).  INVERSE: main c5f618f2 printed
# `X lit` / `X Y` / `X 42 barsub` / `5 5 5 5 X` / `X`.
my $c4795 = <<'PL';
package Foo; use constant X => 2 + 3; use constant Y => "lit"; sub get { X } sub gety { Y }
package Bar; sub X { "barsub" } sub Y { "barsuby" }
package main;
print Foo::get(), " ", Foo::gety(), "\n";
PL
both_agree($c4795, '#4795: a later package\'s `sub X` does not turn the bare constant X into a string');
{
    local $ENV{PCL_OPT} = '-const-fold';
    both_agree($c4795, '#4795: ... nor under PCL_OPT=-const-fold');
}
both_agree(<<'PL', '#4795: three packages, two constants and a sub of one name');
package Foo; use constant X => 2 + 3; sub get { X }
package Bar; sub X { "barsub" }
package Baz; use constant X => 7 * 6; sub get { X }
package main;
print Foo::get(), " ", Baz::get(), " ", Bar::X(), "\n";
PL
both_agree(<<'PL', '#4795: every spelling of the constant in its own package');
package Foo; use constant X => 2 + 3; sub a { Foo::X } sub b { Foo->X } sub c { &X } sub d { X() } sub e { X }
package Bar; sub X { "barsub" }
package main;
print join(" ", Foo::a(), Foo::b(), Foo::c(), Foo::d(), Foo::e()), "\n";
PL
both_agree(<<'PL', '#4795: block-form packages');
package Foo { use constant X => 2 + 3; sub get { X } }
package Bar { sub X { "barsub" } }
print Foo::get(), "\n";
PL

# ---- #4562 (s536b, Role-Tiny concrete-methods.t): a `use constant` in a
# BLOCK after `package R;` defines R's sub (it took the bare name, read in the
# section's package: main::X), and a name declared in a BEGIN -- a constant,
# a prototype -- is known to the BEGIN's own later statements (the parse
# clock kept the BEGIN's site for every statement inside it).
both_agree(<<'PL', '#4562: `use constant` after `package R;` in a BEGIN / a sub / a hash form defines R\'s constant');
BEGIN { package R; use constant X2 => 2; print "b: ", X2, "\n"; }
print "c: ", R::X2(), " ", (defined &main::X2 ? "main" : "-"), "\n";
sub f { package R4; use constant X4 => 4; return X4; }
print "d: ", f(), " ", R4::X4(), "\n";
BEGIN { package R5; use constant { X5 => 5, Y5 => [5, 6] }; print "e: ", X5, " ", Y5->[1], "\n"; }
print "f: ", R5::X5(), " ", R5::Y5()->[0], "\n";
BEGIN { package R6; use constant L6 => (1, 2, 3); }
my @l = R6::L6(); print "g: @l\n";
PL
both_agree(<<'PL', '#4562: a constant / a prototype declared in a BEGIN applies to the BEGIN\'s later statements');
BEGIN {
  use constant K => 4;
  my $s = "abcdefgh";
  print "a: ", substr($s, 1, K), "|", K + 1, "\n";
  sub one ($) { "<$_[0]>" }
  my @r = (one 1, 2); print "b: @r\n";
}
PL

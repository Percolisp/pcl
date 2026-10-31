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

plan tests => 12;

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
# PCL_XSERVER= (empty) keeps the gate's transpile server out of it: the
# runtime hands the module transpile its @INC as `perl -I DIR pl2cl --module`,
# and a SERVED request does not carry perl's -I switches (task #4041) -- under
# the server the require inside the module did not find C3870::B, so the row
# passed on the unfixed tree.
sub both_agree {
    my ($code, $desc) = @_;
    my $pl   = write_pl($code);
    my $perl = `perl $pl 2>&1 </dev/null`;
    my $pcl  = `PCL_XSERVER= $pcl_bin $pl 2>&1 </dev/null`;
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# ---- the module's own transpile: the constant is a CALL, never a string ----
{
    my $cl = PCLCore::transpile("PERL5LIB=$libdir $pl2cl --module < $libdir/C3870/A.pm");
    unlike($cl, qr/\(p-if "_D"/, '#3870: `_D` above the require is not the STRING "_D"');
    my $calls = () = $cl =~ /\(C3870::A::pl-_D\)|\(\|C3870::A\|::pl-_D\)/g;
    is($calls, 2, '... both reads of `_D` are calls of the package constant');
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

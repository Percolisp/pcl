#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# runtime-name-hygiene-01.t — task #2100: a user sub named like one of the
# RUNTIME's own internal operators must not be hijacked at its call site.
#
# `Pl::ExprToCL`'s `%RUNTIME_NAMES` is NOT perl's builtin list: beside `print`
# and `reverse` it holds the runtime's own vocabulary — `flatten`, `hash`,
# `aref`, `aslice`, `box`, `setf`, `let`, `regex`, `subst`, `typeglob`, `cwd`,
# `reftype`, `weaken`, … — and a PLAIN call lowered to `p-NAME` on that table
# alone, so the user's sub was never reached:
#
#     sub hash { "user:@_" }  print hash(1,2);   # printed "12"  (SILENT)
#     sub flatten { ... }     flatten(1,2);      # CL program error
#     sub let { ... }         let(1);            # failed inside a MACROEXPANSION,
#                                                # taking the whole file with it
#
# 27 of the 36 such names failed; the method call and the `&NAME(...)` form
# were right all along, which is what located the bug in `cl_name`'s plain-call
# arm.  `sub flatten`, `sub hash`, `sub box`, `sub regex`, `sub exception`,
# `sub cwd` are everyday names.
#
# THE RULE: a name the program DECLARES is the user's, UNLESS it is a perl
# KEYWORD — perl calls the builtin for `sub reverse {…}; reverse(…)` unless it
# was imported or predeclared with `use subs` (#1870/#1992 own that half).
# "Is it a keyword" is MEASURED, not listed: `prototype("CORE::NAME")` DIES for
# a non-keyword, so the die is the discriminator
# (`Pl::PExpr::Config::is_core_keyword`).  The `CORE::NAME` spelling names the
# builtin unconditionally and is carried through as a separate flag — without
# it, lib/Cwd.pm's `sub cwd { CORE::cwd() }` called ITSELF and exhausted the
# binding stack.
#
# THE NAMES ARE DERIVED AT TEST TIME from the runtime's own export list, so a
# runtime operator added tomorrow is covered the day it is added.  All of them
# go into ONE program (one SBCL launch) and the whole output is compared with
# perl's.

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

plan tests => 17;

# ---- the names, derived from the TABLE THAT DECIDES ------------------------
# `%RUNTIME_NAMES` in Pl/ExprToCL.pm is what routes a plain call to `p-NAME`,
# so the set is read from there at test time: every entry that is a legal Perl
# identifier and that perl does NOT know as a keyword.  A runtime operator
# added to that table tomorrow joins this test the day it is added.
my @names;
{
    open my $fh, '<', "$project_root/Pl/ExprToCL.pm" or die "open ExprToCL: $!";
    my $in_table = 0;
    my $body     = '';
    while (my $l = <$fh>) {
        if (!$in_table) { $in_table = 1 if $l =~ /^my \%RUNTIME_NAMES\s*=\s*map .* qw\(/; next }
        last if $l =~ /^\);/;
        $body .= $l;
    }
    close $fh;
    $body =~ s/#.*//g;
    my %seen;
    for my $n (split ' ', $body) {
        next if $n !~ /\A[A-Za-z_][A-Za-z_0-9]*\z/;             # not an identifier
        next if $seen{$n}++;
        next if eval { my $p = prototype("CORE::$n"); 1 };      # a perl keyword
        push @names, $n;
    }
}
@names = sort @names;

cmp_ok(scalar(@names), '>=', 20,
       '%RUNTIME_NAMES holds at least 20 non-keyword identifiers to test (got '
     . scalar(@names) . ')');

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, $desc);
}

# ---- ONE program, every name, three call spellings each -------------------
my $prog = join('', map { "sub $_ { return \"user:\@_\" }\n" } @names)
         . join('', map { "print \"$_=\", $_(1,2), \"|\", main->$_(3), \"|\", &$_(4), \"\\n\";\n" }
                    @names);
both_agree($prog, 'a user sub of a runtime-internal name wins at every call spelling');

# ---- the cases the rule must NOT break ------------------------------------
both_agree(<<'P', 'a perl KEYWORD still wins over a user sub of the same name');
sub reverse { return "MINE" }
print "1 ", CORE::reverse("ab"), " ", &reverse("x"), "\n";
my $r = [1,2];
print "2 ", ref($r), "\n";
package Obj; sub new { bless {}, shift } sub hi { "hi" }
package main;
my $o = Obj->new;
print "3 ", ($o->can("hi") ? "can" : "cannot"), " ", ($o->isa("Obj") ? "isa" : "not"), "\n";
print "4 ", $o->can("hi")->($o), "\n";
P

# lib/Cwd.pm is `sub cwd { CORE::cwd() }`, and Scalar::Util's exports collide
# with the runtime's names too — an imported sub must still reach its shim.
both_agree(<<'P', 'an IMPORTED sub of a runtime name still reaches its shim');
use Cwd qw(cwd getcwd abs_path);
use Scalar::Util qw(reftype blessed looks_like_number);
print "1 ", (cwd() =~ m{/} ? "ok" : "BAD"), " ", (getcwd() =~ m{/} ? "ok" : "BAD"), "\n";
print "2 ", (abs_path(".") =~ m{/} ? "ok" : "BAD"), "\n";
my $r = [1,2];
print "3 ", reftype($r), " ", (blessed($r) // "undef"), "\n";
print "4 ", looks_like_number("3.5") ? 1 : 0, "\n";
P

# #3825: the call's TAIL (the *wantarray* bind, the #2636 copy-back) asked the
# name table while its HEAD asked cl_name, so a declared or imported sub named
# like a runtime operator was called as `pl-NAME` with NO context bind: as a
# call argument it ran in the statement's VOID context.  Test::Fatal's
# `exception { ... }` returned undef there (Try::Tiny returns nothing in void),
# so every `like exception { ... }, qr/.../` in a CPAN test failed.
both_agree(<<'P', 'a declared sub named like a runtime operator gets its caller\'s context (#3825)');
sub exception (&) { my $c = shift; $c->(); wantarray ? "list" : defined(wantarray) ? "scalar" : "void" }
sub hash { wantarray ? "L" : defined(wantarray) ? "S" : "V" }
sub show { print join(",", @_), "\n" }
show(exception { 1 });
show(exception { 1 }, "x");
show(hash(1), hash());
my $s = hash(); my @l = (hash()); print "$s @l\n";
P

# ---- #1611: the sub nobody DECLARED -----------------------------------------
# #2100 made a DECLARED (or imported) sub of a runtime-internal name win; a sub
# installed by a COMPUTED glob write is declared nowhere the compiler can see,
# and its call still lowered to the runtime operator.  Test::Deep builds
# `hash` / `array` / ... that way, so `Test::Deep::wrap` called the runtime's
# hash constructor and every second cmp_deeply of a hash died.  perl's rule:
# a word that is not a keyword, followed by `(` or used as a list operator,
# is a SUB CALL whether or not the sub was seen (Pl::ExprToCL::
# source_call_is_builtin; docs/ir-spec.md §10-name).
my $glob_prog = "BEGIN { no strict 'refs'; for my \$n (qw(@names)) "
              . "{ *{\"main::\$n\"} = sub { \"g-\$n(\@_)\" } } }\n"
              . join('', map { "print \"$_=\", $_(1,2), \"\\n\";\n" } @names);
both_agree($glob_prog, 'a sub a computed glob write installs is called by its plain name (#1611)');

both_agree(<<'P', 'the other call spellings of an undeclared runtime-name sub (#1611)');
BEGIN { no strict 'refs'; for my $n (qw(hash flatten exception box)) { *{"main::$n"} = sub { "g-$n(@_)" } } }
print "list-op ", hash 3, 4;
print "\nlist-op ", flatten 5, 6;
print "\nlist-op ", exception 7;
print "\n";
package Foo;
BEGIN { no strict 'refs'; *{"Foo::box"} = sub { "Foo-box(@_)" } }
print "pkg ", box(10), "|", Foo::box(11), "|", main::box(12), "\n";
package main;
print "qual ", Foo::box(13), "|", main::hash(14), "\n";
my @s = sort hash 3, 1, 2;
print "sort @s\n";
hash;
print "neg ", -hash(4), "\n";
print "meth ", main->hash(5), "\n";
print "amp ", &hash(6), "\n";
P

# The CONTROLS: a perl keyword keeps the builtin even with a user sub of its
# name declared before or after the call, and PCL's own shims reach the
# runtime's operators through the `CORE::` spelling (lib/Scalar/Util.pm,
# lib/Sub/Util.pm and the `:prototype` desugar -> CORE::__pcl_set_prototype;
# lib/File/Spec.pm -> CORE::cwd) -- a plain spelling there is a sub call now.
both_agree(<<'P', 'keywords stay builtins; the shims reach the runtime by CORE:: (#1611 controls)');
print "1 ", length("abc"), " ", uc("ab"), " ", join("-", 1, 2), " ", scalar(reverse("ab")), "\n";
my %h = (k => 1);
print "2 ", join(",", keys %h), " ", ref([]), " ", (defined(undef) ? "d" : "u"), "\n";
sub length { "MYLEN" }
sub uc { "MYUC" }
sub join { "MYJOIN" }
sub keys { "MYKEYS" }
sub ref { "MYREF" }
print "3 ", length("abcd"), " ", uc("cd"), " ", join("+", 3, 4), " ", ref({}), "\n";
require Scalar::Util;
sub f1 { 1 }
Scalar::Util::set_prototype(\&f1, q($$));
print "4 ", prototype(\&f1), "\n";
require Sub::Util;
Sub::Util::set_prototype('$;@', \&f1);
print "5 ", prototype(\&f1), "\n";
my $c1 = sub ($$) { 1 };
my $c2 = sub :prototype(\@) { 1 };
print "6 ", prototype($c1), " ", prototype($c2), "\n";
require File::Spec;
require Cwd;
my $d = Cwd::cwd();
print "7 ", (File::Spec->rel2abs("x") eq "$d/x" ? "ok" : "BAD"), " ",
      File::Spec->abs2rel("$d/a/b"), "\n";
P

# The real modules the bug was found in (#1611: Test::Deep's `hash`; Test::Fatal's
# `exception`, the s481b filing).  Neither is on CI's stock machine, so the row
# skips there.  STDOUT only: Test::Deep::Cache::Simple's `weaken` sites print a
# known-harmless detector line on stderr (docs/detector-measurement-s513d.md).
SKIP: {
    skip 'Test::Fatal / Test::Deep not installed', 1
        unless eval { require Test::Fatal; 1 } && eval { require Test::Deep; 1 };
    my $src = write_pl(<<'P');
use strict; use warnings;
use Test::Fatal;
use Test::Deep qw(cmp_deeply);
use Test::More tests => 5;
like(exception { die "boom\n" }, qr/^boom/, 'exception { die } returns the error');
is(exception { 1 }, undef, 'exception { 1 } is undef');
is(ref(Test::Deep::wrap({ pi => 1 })), 'Test::Deep::Hash', 'wrap of a hash is a Test::Deep::Hash');
cmp_deeply({ pi => 1 }, { pi => 1 }, 'cmp_deeply of a hash');
cmp_deeply({ pi => 1, x => [1, 2] }, { pi => 1, x => [1, 2] }, 'a second cmp_deeply of a hash');
P
    my $perl = `perl $src 2>/dev/null`;
    my $cl_code = PCLCore::transpile("$pl2cl $src");
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $pcl = `sbcl @sbcl_rt --load $cl_file 2>/dev/null`;
    $pcl =~ s/^;.*\n//gm;
    $pcl =~ s/^PCL Runtime loaded\n//gm;
    is($pcl, $perl, 'Test::Fatal exception {} and Test::Deep cmp_deeply of hashes, as perl (#1611)');
}

# SHAPES: what reaches `p-` and what does not.
{
    my $cl = PCLCore::transpile("$pl2cl " . write_pl(<<'P'));
my $c = sub ($$) { 1 };
my $d = sub :prototype($) { 1 };
my $e = sub :lvalue { 1 };
sub length { 1 }
print length("x");
BEGIN { *{"main::hash"} = sub { 1 } }
print hash(1);
P
    like($cl, qr/\(pl-hash 1\)/,    'an undeclared non-keyword call is the sub `(pl-hash 1)`');
    unlike($cl, qr/\(p-hash 1\)/,   '... and never the runtime operator');
    like($cl, qr/\(p-length "x"\)/, 'a perl keyword with a user sub of its name stays the builtin');
    my $protos = () = $cl =~ /\(p-__pcl_set_prototype\s/g;
    is($protos, 2, 'both anonymous-sub prototype registrations reach the runtime (compiler-built head + CORE:: text)');
    like($cl, qr/\(p-__pcl_set_attributes\s/, 'the anonymous-sub attribute registration reaches the runtime');
    unlike($cl, qr/pl-__pcl_set_/,  'no compiler-built registration became a sub call');
}

# #4601 (s536g): `cwd` / `getcwd` are Cwd's functions, which perl declares
# WITHOUT a prototype -- never core arity.  A list-operator call of a
# glob-installed or a user sub of that name takes its arguments (the core
# table's `cwd => 0` split `cwd 1, 2` and DROPPED the statement), and the
# imported one is still a term before an operator, as in perl.
both_agree(<<'P', '#4601 a user / glob-installed cwd or getcwd is a list operator');
BEGIN { no strict 'refs'; *{"main::cwd"} = sub { "g-cwd(@_)" } }
print "1 ", cwd 1, 2;
print "\n";
sub getcwd { "my-getcwd(@_)" }
print "2 ", getcwd(3), " ", getcwd 4, 5;
print "\n";
P

both_agree(<<'P', '#4601 the imported cwd / getcwd: terms before an operator, a list operator before a list');
use Cwd;
my $d = cwd();
my @p = (cwd, 1);
print "3 ", (cwd . "/x" eq "$d/x" ? "ok" : "no"), " ", (cwd eq $d ? "ok" : "no"), " ", scalar(@p),
  " ", ((cwd 1, 2) eq $d ? "ok" : "no"), " ", (getcwd eq $d ? "ok" : "no"), " ", (Cwd::cwd() eq $d ? "ok" : "no"), "\n";
P

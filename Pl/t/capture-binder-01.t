#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# capture-binder-01.t -- the CAPTURE GATE and the PROMOTER ask the BINDER
# (s533d, parser program step 1c port 2; docs/binder-dual-run-s525.md §6
# item 2).
#
# A named sub is hoisted out of the `let`s that bind the file's lexicals, so
# the compiler must know which file lexicals a sub READS: those it promotes to
# a package cell (`_promote_captured`), and a capture it cannot promote
# refuses the file (`_check_sub_captures`, `_hoist_nested_sub`).  The question
# "does this sub read that lexical?" was a private scope walk with per-canon
# TEXT patterns (`_block_captures_name_0`); it is now
# `Pl::Binder::captures_name`, perl's pad model -- positional, so a `my`
# nested in a call's argument list shadows (#3802) and a `my` declared AFTER
# the sub is not visible in it (#3806, and the plain-sub twin of it) -- plus
# the STRING-EVAL fact: a literal text naming a lexical that some string eval
# can see counts as a read (`eval '$x'`, `my $c = '$q'; eval $c`, the text in
# one sub and the eval in another).  Rows 1 and 3-5 FAIL on the base 0f2db817
# (inverse-verified: the unneeded cell, or the refusal), and so does row 5b
# (#4462, the empty interpolation); row 2 passes there too (#3802 never
# changed a value, only the cell); rows 6-10 are the
# string-eval shapes the old text scan got right and the binder must keep;
# row 11 is the promoter's eval-pair test, which must keep the base's answer
# too; row 12 is rule 12.  Every expected text is perl 5.40.3's own output.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use lib "$RealBin/../..";
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;
plan tests => 13;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (a DROPPED statement fails the row, via PCLCore) and run.
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

# ── 1-2. #3802: a `my $c` nested in a call's ARGUMENT LIST (`print(my $c =
#    ...)`, `ok(my $csv = ...)`, `open(my $l, ...)`) is the sub's own lexical
#    from the next statement on (op.c newSTATEOP -> intro_my), so the sub does
#    NOT capture the file's `$c`: no cell is minted for it and the sub carries
#    no :captures.  The base promoted it ($c__file__0, `:captures`).
my $c1 = <<'PL';
my $c = "outer";
sub p { my $f = shift; print(my $c = "inner$f"); print "\n"; for (1) { $c .= "+" } $c }
print p(1), "\n", $c, "\n";
my $r = p(2); print "$r $c\n";
PL
my $c1_cl = PCLCore::transpile("$pl2cl " . write_pl($c1));
ok($c1_cl !~ /\$c__file__/ && $c1_cl !~ /:captures/,
   '#3802: a `my` in a call\'s argument list shadows -- no cell, no :captures');
is(run_cl($c1), "inner1\ninner1+\nouter\ninner2\ninner2+ outer\n",
   '#3802: ... and the program answers as perl does');

# ── 3. #3806: a signature sub whose body names `@x` while the file's `my @x`
#    is declared AFTER the sub -- perl reads the GLOBAL @main::x.  The base
#    refused the file ("file lexical 'x' captured by sub g").
is(run_cl(<<'PL'), "g(1)[]\n", '#3806: a different-sigil `my` declared AFTER the sub is the global in it');
use feature "signatures";
sub g ($x) { "g($x)[@x]" }
my @x = (7, 8);
print g(1), "\n";
PL

# ── 4-5. #3806's plain-sub twin: no signature, a scalar, the global written
#    before the `my` -- the sub reads the global, never the later lexical, and
#    the lexical's `let` does not shadow the global for the sub.
is(run_cl(<<'PL'), "g:u\nx=5\n", 'a sub BEFORE the file `my $x` reads the global $x (undef)');
sub g { print "g:", (defined $x ? $x : 'u'), "\n" }
my $x = 5; g();
print "x=$x\n";
PL
is(run_cl(<<'PL'), "pre=3\ng:3\nx=5\n", '... and the global it reads keeps its value under the later lexical');
$x = 3;
sub g { print "g:", (defined $x ? $x : 'u'), "\n" }
print "pre=$x\n";
my $x = 5; g();
print "x=$x\n";
PL

# ── 5b. #4462: a file lexical interpolated in a sub's REGEX as `${x}[...]` /
#    `$x[...]` where perl reads the SCALAR and a character class (toke.c
#    intuit_more, Pl::InterpScan) IS a capture.  The old scan's own text
#    pattern took it for an element of @x, the cell was not promoted and the
#    interpolation was EMPTY (core Pod::Simple::RTF, File::Spec::Win32).
is(run_cl(<<'PL'), "see <foo\@bar> and x\n20\n", '#4462: a file lexical as `${x}[...]` / `$x[...]` in a sub\'s regex is captured');
my $id_re = qr/[a-z]+/;
sub m1 { my $s = shift; $s =~ s/(${id_re}[\$\@:]\S+)/<$1>/g; $s }
print m1("see foo\@bar and x"), "\n";
my $d = '[a-zA-Z]:';
sub abs1 { my $f = shift; $f =~ m{^$d[\\/]} ? 2 : 0 }
print abs1('C:\\x'), abs1('zz'), "\n";
PL

# ── 6-10. THE STRING-EVAL FACT: a lexical that a string eval can see is read
#    by any literal text naming it, wherever the text sits.
is(run_cl(<<'PL'), "n=5\ng=6\n", "eval '\$n = \$o' / return eval '...' in a named sub");
my $o = 5; my $n;
sub f { eval '$n = $o' } f(); print "n=$n\n";
my $y = 3;
sub g { return eval '$y * 2' } print "g=", g(), "\n";
PL
is(run_cl(<<'PL'), "k=12\n", 'the text in a variable, the eval of the variable');
my $q = 4;
sub k { my $code = '$q * 3'; eval $code } print "k=", k(), "\n";
PL
is(run_cl(<<'PL'), "run=3\n", 'the text in one sub, the string eval in another');
my $w = 2;
sub src { '$w + 1' } sub run { eval $_[0] } print "run=", run(src()), "\n";
PL
is(run_cl(<<'PL'), "h=8\n", 'a string eval in an anon sub inside a named sub');
my $z = 7;
sub h { my $c = sub { eval '$z+1' }; $c->() } print "h=", h(), "\n";
PL
is(run_cl(<<'PL'), "2\n", 'closure.t:548: an anon sub returned by a named sub evals an outer lexical');
{
    $x = 1;
    my $x = 2;
    sub tmp { sub { eval '$x' } }
    my $a = tmp();
    undef &tmp;
    print $a->(), "\n";
}
PL

# ── 11. the promoter's eval-pair test (_enclosing_lex_decl): a same-name FILE
#    `my` declared AFTER the block still encloses it -- the block cell takes
#    the per-site pair, so the global name after the block is untouched (the
#    alias path would leak the cell into it, #4461; eval.t:151's shape).
is(run_cl(<<'PL'), "in=ok 3 ev=ok 3\nbetween=u\nafter=file ev=file show=file\n", 'a block cell under a LATER same-name file `my` keeps out of the global name');
{
    my $got;
    sub rec { my $l = shift; if ($l < 3) { eval 'rec($l + 1)'; } else { $got = "ok $l" } }
    rec(0);
    print "in=$got ev=", eval('$got'), "\n";
}
print "between=", (defined $got ? $got : 'u'), "\n";
my $got = "file";
sub show { eval '$got' }
print "after=$got ev=", eval('$got'), " show=", show(), "\n";
PL

# ── 12. RULE 12: a block holding a token the binder has no record of (here:
#    one inserted after the build) DIES naming the question -- never a silent
#    "not captured".  Built in-process from a real binder.
{
    require Pl::VendorPPI;
    require Pl::Parser2;
    require Pl::Binder;
    my $src = "my \$q = 1; sub r { \$z = 1; }\n";
    my $doc = PPI::Document->new(\$src);
    my $b = Pl::Binder->new($doc)->bind;
    my $parser = bless { _binder => $b }, 'Pl::Parser2';
    my ($blk) = @{ $doc->find('PPI::Structure::Block') };
    my ($st) = $blk->schildren;
    $st->first_element->insert_before(PPI::Token::Symbol->new('$q'));
    my $err = eval { $parser->_block_captures_name($blk, 'q', undef); 1 } ? '' : $@;
    like($err, qr/^PCL internal: the binder holds no record of the block at line 1 \(no-record\) -- the capture question about 'q'/,
         'rule 12: the capture question dies on a block the binder does not fully hold');
}

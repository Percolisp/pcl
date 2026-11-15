#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# rename-binder-01.t -- the DECLARATION RENAMES ask the BINDER (s539b, parser
# program step 1c port 4; docs/binder-dual-run-s525.md §6 item 4), and the
# interpolation-rewrite bugs found beside them (#4822 #4921).
#
# A `state` cell, an exception-partition name (`my $a` -> `$a__excl__N`), an
# embedded `my` its own statement re-reads (`__shadow__`, #4642) and a seam
# shadow take a fresh name, and so must EXACTLY the uses of that declaration.
# The rename was a forward walk over a region with a skip region, a stop at
# the next same-scope declaration and a per-token shadow test; it is now
# Pl::Parser2::_rename_binding -- the declarator plus Pl::Binder::uses_of.
# Rows marked BASE-FAIL fail on the base (inverse-verified on its extraction):
# a silently wrong value or a loud refusal of valid perl.  The other rows are
# shapes the old walk got right and the port must keep.  The last rows are
# rule 12, in-process.  Every expected text is perl 5.40.3's own output.

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
plan tests => 20;

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

# ── 1-4. #4822: a WHOLE hash named inside an interpolated code block
#    (`"@{[ %h ]}"`, `"@{[ scalar(keys %h) ]}"`) was no use of the hash: the
#    binder dropped a code span with no `$`/`@`, and the rename's text rewrite
#    had no `%name` arm.  A renamed `state %k` read the package global (empty),
#    a span-promoted `%h` crashed unbound or read empty.  BASE-FAIL, all four.
is(run_cl(<<'PL'), "k v|1\n", '#4822: a classic `state %k` read whole in an interpolated code block');
use feature 'state';
sub f { state %k = (k => 'v'); return "@{[ %k ]}|@{[ scalar(keys %k) ]}" }
print f(), "\n";
PL
is(run_cl(<<'PL'), "file: a 1|1\n", '#4822: ... a file-level `state %fk` (the source rewrite)');
use feature 'state';
state %fk = (a => 1);
print "file: @{[ %fk ]}|@{[ scalar(keys %fk) ]}\n";
PL
is(run_cl(<<'PL'), "a 1|1|1\n", '#4822: ... a file `my %h` read whole in a later section (was: %h unbound)');
my %h = (a => 1);
package Q;
print "@{[ %h ]}|@{[ scalar(keys %h) ]}|@{[ $h{a} ]}\n";
PL
is(run_cl(<<'PL'), "a 1|1\n", '#4822: ... beside an inner `my %h` (the span was not seen at all)');
my %h = (a => 1); { my %h; }
package Q;
print "@{[ %h ]}|@{[ scalar(keys %h) ]}\n";
PL

# ── 5. #4822's other side: a `%name` in plain interpolated TEXT is literal.
is(run_cl(<<'PL'), "inner w 3 pct 100%b\nouter z 26 50%b 1\n", '#4822: "100%b" is text, `@{[ %b ]}` is the hash (control)');
my ($a, %b) = (1, z => 26);
{ my %b = (w => 3); print "inner @{[ %b ]} pct 100%b\n" }
print "outer @{[ %b ]} 50%b $a\n";
PL

# ── 6-7. #4921: a scalar DEREF with a subscript in interpolated text
#    ("$$a[1]", "@$a[0,1]", "$$h{k}") is a use of the SCALAR -- the rewrite
#    read `$a[` as an element of @a.  After the exception rename the text read
#    the special `$a` (empty); the span pass refused the file.  BASE-FAIL.
is(run_cl(<<'PL'), "2 1 2 1 v\n7|8 7\n", '#4921: "$$a[1]" / "@$a[0,1]" after the exception rename of `my $a`');
my $a = [1, 2]; my $h = { k => 'v' };
print "$$a[1] @$a[0,1] ${$a}[0] $$h{k}\n";
sub f { my $s = shift; return "$$s[0]|@$s[1,0]" }
print f([7, 8]), "\n";
PL
is(run_cl(<<'PL'), "6 5 6\n", '#4921: ... a spanning `my $c` deref-read in a later section (was: refused)');
my $c = [5, 6];
package Q;
print "$$c[1] @$c[0,1]\n";
PL

# ── 8-9. #4921's second face: a braced NAME closes the reference ("${a}[0]"
#    is $a then "[0]", "@{b}{" is @b then "{", perl #20716 -- t/comp/parser.t)
#    -- the rewrite read `${a}[` as an element of @a and `${a}{` / `@{b}{` as
#    %a / %b.  After the exception rename of `my $a` the text read the
#    special; a promoted file `my $x` read braced in a named sub read the
#    package variable.  BASE-FAIL, both.
is(run_cl(<<'PL'), "A{|A[|B{|A[0]|B[0]\nA{\nA{ A[ B{\n", '#4921: "${a}{" "${a}[" "@{b}{" in a string, an s/// replacement and a heredoc');
{
    my $a = "A";
    my @b = ("B");
    print "${a}{|${a}[|@{b}{|${a}[0]|@{b}[0]\n";
    my $c = "A{";
    $c =~ s/${a}\{/foo/;
    $c =~ s/foo/${a}{/;
    print "$c\n";
    print <<"${a}{";
${a}{ ${a}[ @{b}{
${a}{
}
PL
is(run_cl(<<'PL'), "S[0] 7 7[0]\n", '#4921: "${x}[0]" of a promoted file `my $x` read in a named sub');
my @x = (1, 2); my $x = 'S';
sub f { my @x = (7); "${x}[0] $x[0] @{x}[0]" }
print f(), "\n";
PL

# ── 10. a default inside a ONE-TOKEN signature (Token::Prototype, #455)
#    reading an exception-renamed file `my $a`: the region walk never
#    reached the token's text, so the default read the special `$a`; the
#    port renames it (a binder use) -- and the binder re-reads the rewritten
#    token, or the OTHER default's `@b` lost its capture.  BASE-FAIL.
is(run_cl(<<'PL'), "f=5/8 f=1/2\ng=6\n", 'a one-token signature default reads the renamed `my $a` (and keeps @b)');
my $a = 5; my @b = (7, 8);
use feature 'signatures'; no warnings; sub f ($x = $a, $y = $b[1]) { return "f=$x/$y" }
print f(), " ", f(1, 2), "\n";
{ my $a = 6; use feature 'signatures'; my $g = sub ($z = $a) { "g=$z" }; print $g->(), "\n"; }
PL

# ── KEEP: the shapes the region walk got right -- state cells in each home,
#    condition / loop heads, nested and later same-name declarations, the
#    self-reading initializer at three depths, the exception partition, an
#    embedded `my`, `catch ($e)`, closures and string evals over each renamed
#    kind, interpolation beside a renamed name, `local $_`.
is(run_cl(<<'PL'), "1:1:1\n2:1 2:1,2\n3:1 2 3:1,2,3\n11/1 11 12/1 11 12\nloop 1 [1]\nloop 3 [1 2]\nloop 6 [1 2 3]\nfile 6 1 2 3 v 3\n1010102\n", 'keep: state scalar / array / hash in a named sub, an anon sub, a loop body, at file level');
use strict; use warnings; use feature 'state';
sub ns { state $n = 0; state @a; state %h; $n++; push @a, $n; $h{$n} = 1; return "$n:@a:" . join(',', sort keys %h) }
print ns(), "\n" for 1 .. 3;
my $an = sub { state $c = 10; state @l = (1); push @l, ++$c; return "$c/@l" };
print $an->(), " ", $an->(), "\n";
for my $i (1 .. 3) { state $t = 0; state @seen; $t += $i; push @seen, $i; print "loop $t [@seen]\n" }
state $fl = 5; state @fa = (1, 2); state %fh = (k => 'v');
$fl++; push @fa, 3; print "file $fl @fa $fh{k} ${\ scalar(@fa)}\n";
sub inner { if ($_[0]) { state $z = 100; return ++$z } return 0 }
print inner(1), inner(0), inner(1), "\n";
PL
is(run_cl(<<'PL'), "if 5\nelsif 0 3\nafter outer\nw 1 2\nw 2 4\nw 3 6\nn nested\nnn nested2\nn2 nested\na outer\ndeep 4\nd2 3\nd1 2\nd0 1\ni 2\ni 4\ni 6\n17\n", 'keep: condition / while heads, nested and later same-name `my`, `my $x = $x + 1` at three depths');
use strict; use warnings;
my $x = 'outer';
sub f { return $_[0] }
if ((my $x = f(5)) > 1) { print "if $x\n" } else { print "else $x\n" }
if ((my $x = f(0)) > 1) { print "if $x\n" } elsif ((my $y = f(3)) > 2) { print "elsif $x $y\n" } else { print "else $x\n" }
print "after $x\n";
my @q = (1, 2, 3);
while (my $l = shift @q) { my $m = $l * 2; print "w $l $m\n" }
{ my $x = 'nested'; print "n $x\n"; { my $x = $x . '2'; print "nn $x\n" } print "n2 $x\n" }
print "a $x\n";
my $v = 1; { my $v = $v + 1; { my $v = $v + 1; { my $v = $v + 1; print "deep $v\n" } print "d2 $v\n" } print "d1 $v\n" } print "d0 $v\n";
for my $i (1 .. 3) { my $i = $i * 2; print "i $i\n" }
our $gv = 7;
sub g { my $gv = $gv + 10; return $gv } print g(), "\n";
PL
is(run_cl(<<'PL'), "ok\nc=made\n1 2 3\nab 10 20\nblk f:blk\n2\nab2 10 20 10x 20\n", 'keep: an embedded `ok(my $c = f())`, the exception partition beside a sort block');
use strict; use warnings;
sub ok { print(($_[0] ? "ok" : "not ok"), "\n"); $_[0] }
sub mk { return "made" }
ok(my $c = mk()); print "c=$c\n";
my @data = sort { $a <=> $b } (3, 1, 2); print "@data\n";
my ($a, $b) = (10, 20); print "ab $a $b\n";
{ my $a = 'blk'; my $f = sub { "f:$a" }; print "$a ", $f->(), "\n" }
sub cmpf { my ($a, $b) = @_; return $a - $b } print cmpf(5, 3), "\n";
print "ab2 $a $b ${a}x @{[ $b ]}\n";
PL
is(run_cl(<<'PL'), "caught boom\nafter outer-e\na0 innery inner hv inner\ncl:inner\ninner\na0 Xy X X\nL1\nL2\n", 'keep: `catch ($e)` beside an outer $e; "$x[0]" "${x}y" "@{[ $x ]}" beside a shadowing $x; local $_');
use strict; use warnings; use feature 'try'; no warnings 'experimental::try';
my $e = 'outer-e';
try { die "boom\n" } catch ($e) { print "caught $e" }
print "after $e\n";
my $x = 'X'; my @x = ('a0', 'a1'); my %x = (k => 'hv');
{ my $x = 'inner'; print "$x[0] ${x}y @{[ $x ]} $x{k} $x\n"; my $cl = sub { "cl:$x" }; print $cl->(), "\n"; print eval('$x'), "\n" }
print "$x[0] ${x}y @{[ $x ]} $x\n";
for (1 .. 2) { local $_ = "L$_"; my $y = $_; print "$y\n" }
PL
is(run_cl(<<'PL'), "AB\nAB\ns=11s=22\n10,20\nblk\n1\n1|1 B=2\n", 'keep: closures and string evals over a renamed exception pair, a state cell, a shadow; the re-read embedded `my`');
use strict; use warnings; use feature 'state';
my ($a, $b) = ('A', 'B');
my $ca = sub { "$a$b" }; print $ca->(), "\n";
print eval '$a . $b', "\n";
sub st { state $s = 0; $s++; my $f = sub { "s=$s" }; return $f->() . eval('$s') }
print st(), st(), "\n";
my $outer = 1;
my @subs = map { my $outer = $_ * 10; sub { $outer } } 1 .. 2; print join(',', map { $_->() } @subs), "\n";
{ my $outer = 'blk'; print eval('$outer'), "\n" }
print "$outer\n";
my @env = ('A=1', 'B=2'); { chomp(my @env2 = grep { s/^A=// } @env); print "@env2|@env\n" }
PL

is(run_cl(<<'PL'), "<2> <4> <6> 1\n7 7y 1\na=1;a=2;\nargv p q 1\n1:1:0 2:1 2:1\n5 1 3 4\n3 9\n", 'keep: seam shadows in map / do, a foreach `my $a`, `my @ARGV`, `state ($p, @l)`, a file `my $b` beside sort');
use feature 'state';
my $x = 1; my @r = map { my $x = $_ * 2; "<$x>" } 1 .. 3; print "@r $x\n";
my @q = do { my $x = 7; ($x, "${x}y") }; print "@q $x\n";
for my $a (1, 2) { print "a=$a;" } print "\n";
{ my @ARGV = ('p', 'q'); print "argv @ARGV $#ARGV\n"; }
sub st { state ($p, @l); $p++; push @l, $p; return "$p:@l:$#l" } print st(), " ", st(), "\n";
my $b = [3, 4]; my @s = sort { $a <=> $b } (5, 1); print "@s @$b\n";
my $n = 3; my $m = $n, my $k = 9; print "$m $k\n";
PL

# ── RULE 12 (in-process, a real binder): a use the rewrite cannot reach, a
#    token inserted into the scope since the build, a token that declares
#    nothing -- each DIES naming it, and the document is left as it was.
{
    require Pl::VendorPPI;
    require Pl::Parser2;
    require Pl::Binder;
    my $setup = sub {
        my ($src) = @_;
        my $doc = PPI::Document->new(\$src);
        my $parser = bless {}, 'Pl::Parser2';
        $parser->{_binder} = Pl::Binder->new($doc, parser => $parser)->bind;
        return ($doc, $parser, @{ $doc->find('PPI::Token::Symbol') });
    };
    my ($doc, $p, $decl, $use) = $setup->("my \$x = 1;\nprint \$x;\n");
    $use->set_content('$y');
    my $err = eval { $p->_rename_binding($decl, '$x__state__0'); 1 } ? '' : $@;
    like($err, qr/^PCL internal: the rename of `\$x` to \$x__state__0 cannot reach its use at line 2 \(symbol\)/,
         'rule 12: a use whose text no longer spells the name dies naming it');
    is($decl->content, '$x', 'rule 12: ... and the declarator is left as it was');
    ($doc, $p, $decl, $use) = $setup->("my \$x = 1;\nprint \$x;\n");
    $use->insert_after(PPI::Token::Symbol->new('$x'));
    $err = eval { $p->_rename_binding($decl, '$x__state__0'); 1 } ? '' : $@;
    like($err, qr/^PCL internal: the binder does not hold every token of the scope of `\$x` \(line 1\)/,
         'rule 12: a token inserted into the scope since the build dies');
    ($doc, $p, $decl, $use) = $setup->("my \$x = 1;\nprint \$x;\n");
    $err = eval { $p->_rename_binding($use, '$x__state__0'); 1 } ? '' : $@;
    like($err, qr/^PCL internal: the binder holds no declaration for `\$x` at line 2/,
         'rule 12: a use is no declaration');
}

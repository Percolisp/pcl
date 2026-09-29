#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# fh-scope-close-01.t — task #2006 (b): a lexical filehandle that does not
# ESCAPE its block is closed (and flushed) when the block exits, as perl
# closes it (Kind-A `fh-scope-close', docs/ir-spec.md §6.5).
#
#   1. SEMANTIC rows: one program, run by perl and by PCL, compared line by
#      line — the acceptance table of the design (normal exit, `return' with
#      the return expression evaluated first, `next'/`last'/`redo', a die
#      unwinding through the block, the implicit pipe close leaving `$?',
#      the condition-`my' handle closing at the ENCLOSING block's exit,
#      `undef $fh' / `$fh = undef', and the shapes the change could break:
#      a block's value, `return <$fh>' in list context, re-open, explicit
#      close, `local $\', printf / `print {$fh}' / read / seek / tell / eof).
#   2. SHAPE rows: the licensed spellings emit `(p-scope-close (…) …)'; the
#      ESCAPES (returned, stored, assigned, captured, passed to a sub, a
#      method call on it, select()ed, referenced, interpolated, a string
#      eval in scope, a dup source) emit NONE — a close there would close a
#      handle under a live holder, a silent wrong.
#   3. The flag: PCL_OPT=none keeps it (it is perl's semantics, not an
#      optimisation), PCL_OPT=-fh-scope-close removes it.
#   4. The runtime macro called directly.
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

plan skip_all => "pl2cl not found" if !(-x $pl2cl);
plan skip_all => "sbcl not found"  if !(`which sbcl 2>/dev/null`);

my $dir = tempdir(CLEANUP => 1);

sub strip_sbcl {
    my ($output) = @_;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^WARNING:.*\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub write_tmp {
    my ($text, $suffix) = @_;
    my ($fh, $file) = tempfile(SUFFIX => $suffix, UNLINK => 1);
    print $fh $text;
    close $fh;
    return $file;
}

sub run_pcl {
    my ($code) = @_;
    my $pl = write_tmp($code, '.pl');
    my $cl = PCLCore::transpile(qq{$pl2cl $pl});
    my $lisp = write_tmp($cl, '.lisp');
    return strip_sbcl(scalar `sbcl @sbcl_rt --load $lisp 2>&1`);
}

sub shape {
    my ($code, %env) = @_;
    local @ENV{keys %env} = values %env;
    my $pl = write_tmp($code, '.pl');
    return PCLCore::transpile(qq{$pl2cl $pl});
}

# --- 1. semantic rows -----------------------------------------------------
my $PROG = <<'PERL';
my $f = "@DIR@/f.txt";
sub sz { (-s $f) // 0 }
# the design's implicit.pl rows 1 3 4 5
$? = 0; { open(my $p, "-|", "sh", "-c", "echo hi; exit 3") or die; my $l = <$p>; } print "r1 implicit pipe close keeps \$?: ", $? >> 8, "\n";
$? = 0; { open(my $p, "-|", "sh", "-c", "echo hi; exit 3") or die; my $l = <$p>; close($p); } print "r1b explicit pipe close: ", $? >> 8, "\n";
sub w { open(my $o, ">", $f) or die; print $o "abcdef"; return -s $f } print "r3 return expr first: ", w(), " after: ", sz(), "\n";
unlink $f; for my $i (1 .. 3) { open(my $a, ">>", $f) or die; print $a "line $i\n"; next if $i == 2; } print "r4 next: ", sz(), "\n";
unlink $f; eval { open(my $o, ">", $f) or die; print $o "before die"; die "x\n" }; print "r5 die: ", sz(), "\n";
# cond.pl: a condition-my closes at the ENCLOSING block's exit
unlink $f; { if (open(my $o, ">", $f)) { print $o "in if"; } print "c9a inside: ", sz(), "\n"; } print "c9b after: ", sz(), "\n";
unlink $f; for my $i (1 .. 2) { if (open(my $o, ">>", $f)) { print $o "i=$i\n" } print "c9c iter $i: ", sz(), "\n" } print "c9d loop: ", sz(), "\n";
unlink $f; { open(my $o, ">", $f) or die; print $o "x" x 5; { print "c11 inner: ", sz(), "\n" } } print "c11b after: ", sz(), "\n";
# fhscope1.pl rows 1-5
unlink $f; { open(my $fh, ">", $f) or die; print $fh "scoped\n"; } print "s1 block: ", sz(), "\n";
sub writer { open(my $o, ">", $f) or die; print $o "from sub\n"; return 1 } writer(); print "s2 sub: ", sz(), "\n";
unlink $f; for my $i (1..3) { open(my $a, ">>", $f) or die; print $a "line $i\n"; }
open(my $r, "<", $f) or die; my @l = <$r>; close $r; print "s3 lines: ", scalar(@l), "\n";
{ open(my $w, ">", $f) or die; print $w "x" x 10; undef $w; } print "s4 undef: ", sz(), "\n";
{ open(my $w, ">", $f) or die; print $w "y" x 5; $w = undef; } print "s5 = undef: ", sz(), "\n";
# what the change could break
{ open(my $o, ">", $f) or die; print $o "one\ntwo\n"; close $o; }
my $n = do { open(my $fh, "<", $f) or die; scalar(<$fh>) }; print "b1 do value: $n";
sub cnt { open(my $fh, "<", $f) or die; my $c = 0; while (<$fh>) { $c++ } $c } print "b2 count: ", cnt(), "\n";
sub lines { open(my $fh, "<", $f) or die; return <$fh> } my @x = lines(); print "b3 list return: ", scalar(@x), "\n";
unlink $f; for my $i (1 .. 4) { open(my $a, ">>", $f) or die; print $a "i$i\n"; last if $i == 3; } print "b4 last: ", sz(), "\n";
my $rd = 0; unlink $f; for my $i (1 .. 2) { open(my $a, ">>", $f) or die; print $a "r$i\n"; if ($rd++ == 0) { redo } } print "b4b redo: ", sz(), "\n";
sub thrower { open(my $o, ">", $f) or die; print $o "abcd"; die "t\n" } unlink $f; eval { thrower() }; print "b5 die through sub: ", sz(), " $@";
unlink $f; { open(my $o, ">", $f) or die; print $o "first"; open($o, ">>", $f) or die; print $o "+second"; } print "b6 reopen: ", sz(), "\n";
unlink $f; { open(my $o, ">", $f) or die; print $o "xy"; close($o) or die; $! = 0; } print "b7 explicit close: ", $! + 0, " ", sz(), "\n";
unlink $f; { local $\ = "X"; open(my $o, ">", $f) or die; print $o "abc"; } print "b9 local ORS: ", sz(), "\n";
unlink $f; { open(my $o, ">", $f) or die; if ($o && defined $o) { printf $o "%s", "pf"; print {$o} "blk" } } print "b11 printf+block: ", sz(), "\n";
{ open(my $o, "+<", $f) or die; binmode $o; seek($o, 0, 0); my $b; read($o, $b, 2); print "b12 read: $b tell=", tell($o), " eof=", (eof($o) ? 1 : 0), "\n"; }
sub findit { for my $i (1 .. 3) { open(my $fh, "<", $f) or die; my $l = <$fh>; return "got $i" if $i == 2 } "none" } print "b14 return in loop: ", findit(), "\n";
sub ctx { open(my $fh, "<", $f) or die; wantarray ? "list" : "scalar" } my @c = ctx(); my $cs = ctx(); print "b15 ctx: $c[0] $cs\n";
unlink $f; { open(my $o, ">", $f) or die; print $o "a"; undef $o; open($o, ">>", $f) or die; print $o "bc"; } print "b16 drop+reopen: ", sz(), "\n";
# the licence is per DECLARATION: an escaping declaration of the same name in
# the same sub, or an escaping OUTER handle around a condition-my, stays open
sub two { { open(my $fh, ">", "$f.1") or die; print $fh "one"; } { open(my $fh, ">", "$f.2") or die; print $fh "two"; return $fh } }
my $h2 = two(); print "p1 first closed: ", ((-s "$f.1") // 0), "\n"; print $h2 "+more"; close $h2; print "p1 second open after return: ", ((-s "$f.2") // 0), "\n";
sub g { open(my $o, ">", "$f.3") or die; if (open(my $o, ">", "$f.4")) { print $o "inner" } print $o "outer"; return $o }
my $g2 = g(); print "p2 inner closed: ", ((-s "$f.4") // 0), "\n"; print $g2 "+x"; close $g2; print "p2 outer open: ", ((-s "$f.3") // 0), "\n";
my $code = '1'; { open(my $fh, ">", "$f.6") or die; print $fh "evalafter"; } print "p4 eval after the scope: ", ((-s "$f.6") // 0), "\n"; eval $code;
unlink $f, "$f.1", "$f.2", "$f.3", "$f.4", "$f.6";
PERL
$PROG =~ s/\@DIR\@/$dir/g;
my $pl_file = write_tmp($PROG, '.pl');
my @want = split /\n/, scalar `$^X $pl_file 2>&1`;
my @got  = split /\n/, run_pcl($PROG);
for my $i (0 .. $#want) {
    my ($tag) = $want[$i] =~ /^(\S+)/;
    is($got[$i], $want[$i], "semantic $tag = perl");
}
is(scalar(@got), scalar(@want), 'semantic: same number of lines as perl');

# --- 2. shape rows ---------------------------------------------------------
my $LICENSED = <<'PERL';
sub a { open(my $fh, "<", $_[0]) or die; my $l = <$fh>; return $l }
sub b { for my $f (@_) { open(my $in, "<", $f) or next; while (<$in>) { print } } }
sub c { { if (open(my $o, ">", $_[0])) { print $o "x" } } }
PERL
my $cl = shape($LICENSED);
like($cl, qr/\(p-scope-close \(\$fh\)/, 'shape: sub-body handle is scope-closed');
like($cl, qr/\(p-scope-close \(\$in\)/, 'shape: loop-body handle is scope-closed');
like($cl, qr/\(p-scope-close \(--pcl-fh-late--\d+\)/, 'shape: condition-my handle closes through the enclosing block');

my %ESCAPE = (
    'returned'        => 'sub e { open(my $h, "<", $_[0]) or die; return $h }',
    'stored'          => 'our @k; sub e { open(my $h, "<", $_[0]) or die; push @k, $h; 1 }',
    'assigned from'   => 'our $g; sub e { open(my $h, "<", $_[0]) or die; $g = $h; 1 }',
    'closure capture' => 'sub e { open(my $h, "<", $_[0]) or die; return sub { <$h> } }',
    'passed to sub'   => 'sub put { 1 } sub e { open(my $h, "<", $_[0]) or die; put($h); 1 }',
    'method call'     => 'sub e { open(my $h, ">", $_[0]) or die; $h->autoflush(1); 1 }',
    'select'          => 'sub e { open(my $h, ">", $_[0]) or die; my $o = select($h); select($o); 1 }',
    'reference'       => 'our $r; sub e { open(my $h, "<", $_[0]) or die; $r = \$h; 1 }',
    'glob deref'      => 'our $g; sub e { open(my $h, "<", $_[0]) or die; $g = *$h; 1 }',
    'interpolated'    => 'sub e { open(my $h, "<", $_[0]) or die; my $s = "$h"; 1 }',
    'string eval'     => 'our @k; sub e { open(my $h, "<", $_[0]) or die; eval q{push @k, $h}; 1 }',
    'dup source'      => 'our $d; sub e { open(my $h, "<", $_[0]) or die; open($d, "<&", $h) or die; 1 }',
    'undef in modifier' => 'sub e { open(my $h, "<", $_[0]) or die; undef $h if $_[1]; 1 }',
    'opendir'         => 'sub e { opendir(my $h, $_[0]) or die; my @e = readdir($h); 1 }',
    'file level'      => 'open(my $h, "<", $0) or die; my $l = <$h>;',
);
for my $why (sort keys %ESCAPE) {
    unlike(shape($ESCAPE{$why}), qr/p-scope-close/, "shape: escape ($why) is NOT scope-closed");
}

# --- 2b. #2534 (s501b): a named sub capturing an EMBEDDED `open(my $h …)` ----
# The captured declaration is renamed `$h__file__N` like a statement-level
# captured `my`; every same-name `open(my $h …)` in another block is its own
# variable with its own `p-let` (and so its own scope close).  Before, one
# forward-defvar'd `$h` served every block: the second block re-opened the
# sub's handle and the sub's output was lost (perl `1 5 5`, PCL `1 0 5`).
# The family is "an embedded `my` in a plain statement": opendir, pipe (two
# decls), read's buffer, chomp(my $l = …), a user sub's argument, sysopen.
my $PROG2534 = <<'PERL';
use strict; use warnings;
my $f = "@DIR@/c.txt";
{ open(my $h, ">", "$f.n") or die; sub usesh { print $h "named" } sub closeh { close $h } }
{ open(my $h, ">", $f) or die; print $h "other"; close $h; }
usesh(); closeh();
print "c1 sibling-block: ", -s "$f.n", " ", -s $f, "\n";
{ open(my $h, ">", "$f.v") or die; sub usesv { print $h "named" } usesv(); }
for my $i (1..2) { open(my $h, ">", $f) or die; print $h "loop$i"; }
print "c2 loop sibling: ", -s $f, "\n";
if (1) { open(my $h, ">", $f) or die; print $h "if"; }
print "c3 if sibling: ", -s $f, "\n";
{ opendir(my $d, "@DIR@") or die; sub dget { scalar grep { /^c\.txt\.n$/ } readdir($d) } }
{ opendir(my $d, "/") or die; closedir $d; }
print "c4 opendir: ", dget(), "\n";
{ pipe(my $r, my $w) or die; sub pw { print $w "p\n"; close $w; scalar <$r> } }
{ pipe(my $r, my $w) or die; close $r; close $w; }
print "c5 pipe: ", pw();
{ chomp(my $l = "line\n"); sub gl { $l } }
{ chomp(my $l = "other\n"); print "c6 chomp sibling: $l\n"; }
print "c7 chomp captured: ", gl(), "\n";
{ foo(my $v); sub gv { $v } }
{ foo(my $v); $v .= "!"; print "c8 user-sub sibling: $v\n"; }
print "c9 user-sub captured: ", gv(), "\n";
sub foo { $_[0] = "set" }
unlink $f, "$f.n", "$f.v";
PERL
$PROG2534 =~ s/\@DIR\@/$dir/g;
{
    my @w = split /\n/, scalar `$^X @{[write_tmp($PROG2534, '.pl')]} 2>&1`;
    my @g = split /\n/, run_pcl($PROG2534);
    for my $i (0 .. $#w) {
        my ($tag) = $w[$i] =~ /^(\S+)/;
        is($g[$i], $w[$i], "#2534 semantic $tag = perl");
    }
    is(scalar(@g), scalar(@w), '#2534 semantic: same number of lines as perl');
}
# The handle licence and the emission agree: every `F-DEBUG … CLOSED` verdict
# is an emitted `(p-scope-close`, and the captured one is NOT closed.
{
    local $ENV{PCL_B_DEBUG} = 1;
    my $pl = write_tmp($PROG2534, '.pl');
    my ($efh, $err) = tempfile(SUFFIX => '.err', UNLINK => 1);
    close $efh;
    my $cl = `$pl2cl $pl 2>$err`;
    open my $eh, '<', $err or die;
    my $closed = grep { /F-DEBUG.*CLOSED/ } <$eh>;
    my $emitted = () = $cl =~ /\(p-scope-close\b/g;
    is($emitted, $closed, "#2534: licence verdicts ($closed CLOSED) = emitted p-scope-close forms");
    like($cl, qr/\(p-defcell \$h__file__0\b/, '#2534: the captured embedded decl is a renamed cell');
}
# The breaking case: a FILE-level `open(my $h …)` captured by a named sub with
# NO same-name sibling keeps its emission (one cell under the original name).
unlike(shape('open(my $h, ">", "/dev/null") or die; sub w { print $h "x" } w();'),
       qr/__file__/, '#2534 inverse: a lone file-level captured embedded decl is not renamed');

# --- 3. the flag -----------------------------------------------------------
like(shape($LICENSED, PCL_OPT => 'none'), qr/\(p-scope-close \(\$fh\)/,
     'PCL_OPT=none keeps fh-scope-close (it is semantics, not an optimisation)');
unlike(shape($LICENSED, PCL_OPT => '-fh-scope-close'), qr/p-scope-close/,
       'PCL_OPT=-fh-scope-close removes it');

# --- 4. the runtime macro called directly ------------------------------------
my $cf = "$dir/direct.txt";
my $lisp = write_tmp(<<"LISP", '.lisp');
(in-package :pcl)
(setf *p-stored-errno* 7)
(let ((\$sfh (make-p-box nil)))
  (p-scope-close (\$sfh)
    (p-open \$sfh ">" "$cf")
    (p-print :fh \$sfh "direct")))
(format t "size ~A errno ~A~%" (with-open-file (s "$cf") (file-length s)) *p-stored-errno*)
(format t "never-opened ~A~%" (progn (p-scope-close ((make-p-box nil)) 1) "ok"))
(format t "value ~A~%" (let ((b (make-p-box nil))) (p-scope-close (b) (+ 1 2))))
LISP
my $direct = strip_sbcl(scalar `sbcl @sbcl_rt --load $lisp 2>&1`);
like($direct, qr/^size 6 errno 7$/m, 'p-scope-close closes (flushes) the stream and leaves $! alone');
like($direct, qr/^never-opened ok$/m, 'p-scope-close on a never-opened box is a no-op');
like($direct, qr/^value 3$/m, 'p-scope-close returns its body\'s value');

done_testing();

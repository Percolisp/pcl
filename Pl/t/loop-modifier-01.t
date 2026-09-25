#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# loop-modifier-01.t — s494p, task #2098 member 3: the str-buffer licence
# stops depending on how the LOOP is spelled.
#
# `$s .= "ab" for 1 .. $n` was QUADRATIC (200k appends 30 s; perl 0.02 s)
# while `for my $i (1 .. $n) { $s .= "ab" }` was linear, and `$s = $s . "ab"`
# was quadratic in any loop.  Neither was a SAFETY decision of the S1 verdict
# (docs/raw-numeric-verdict.md §S1) — measured with PCL_B_DEBUG:
#   * a for/foreach/while/until statement MODIFIER lowered whole-statement
#     through the v1 seam, so the annotator walked its body with no native
#     root and every variable it wrote was boxed (reason `write-compound`).
#     Pl::Parser::_desugar_loop_modifiers now rewrites `EXPR for LIST;` into
#     `for (LIST) { EXPR; }` (and the while/until forms) before anything reads
#     the document, so one compiler — the native loop arms — owns both
#     spellings;
#   * `$s = $s . REST` never counted as a `.=` write.  ONE predicate,
#     Pl::VarAnnotator::append_rest, now makes the annotator count it and
#     Parser2 emit %pcl-str-append for it on a licensed slot only.
# And one fix the desugar exposed: Parser2's topic foreach registered the
# GLOBAL `$_` as a let-bound lexical, so a string eval in the body captured it
# and the eval's own inner `for (7) { $_ }` read the outer box (perl: 7).
#
# SHAPE rows fail on a pre-s494p tree; ANSWER rows are perl 5.40.3's output,
# probed perl -> the 3e8bff3e base -> this tree (scratch/s494p/mod1.pl,
# app2.pl, evtopic.pl in the s494p worktree).
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
my $pl2cl   = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);
plan skip_all => "pl2cl not found" if !-x $pl2cl;
plan skip_all => "sbcl not found"  if !`which sbcl 2>/dev/null`;

sub write_pl {
    my ($src) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $src;
    close $fh;
    return $file;
}
sub emitted { return PCLCore::transpile("$pl2cl " . write_pl($_[0])) }
sub run_pl {
    my $cl = emitted($_[0]);
    my ($cfh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cfh $cl;
    close $cfh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^(?:caught |compilation unit|-->|==>|PCL Runtime loaded).*\n//gm;
    return $out;
}

# ─────────────────────────────────────────────────────────────────────────────
# THE SHAPES
# ─────────────────────────────────────────────────────────────────────────────
my $e1 = emitted(q{sub f { my $s = ""; $s .= "ab" for 1 .. $_[0]; length $s } print f(3), "\n";});
like($e1, qr/\(p-foreach-range-raw \(\$_ 1 \(p-aref \@_ 0\)\) \(%pcl-str-append \$s "ab"\)\)/,
     'a `.=` under a for MODIFIER is the in-place str-buffer append, in the counted range loop');
like($e1, qr/\(\$s :str-buffer/, '... and its accumulator is a str-buffer slot');

my $e2 = emitted(q{sub f { my $s = ""; for my $i (1 .. $_[0]) { $s = $s . "ab" } length $s } print f(3), "\n";});
like($e2, qr/\(%pcl-str-append \$s "ab"\)/, '`$s = $s . "ab"` on a licensed slot IS the append');

my $e3 = emitted(q{sub f { my $s = ""; my $n = 0; while ($n++ < $_[0]) { $s = $s . "x" x 2 } length $s } print f(3), "\n";});
like($e3, qr/\(%pcl-str-append \$s \(p-str-x "x" 2\)\)/, 'REST binding tighter than `.` (`x`) is still the append');

my $e4 = emitted(q{sub f { my $s = "1"; for my $i (1 .. 3) { $s = $s . $i + 1 } $s } print f(), "\n";});
unlike($e4, qr/%pcl-str-append/, '`$s = $s . $i + 1` is ($s . $i) + 1 — NOT an append');

my $e5 = emitted(q{sub f { my $s = ""; for my $i (1 .. 2) { $s = $s . "a" . "b" } $s } print f(), "\n";});
unlike($e5, qr/%pcl-str-append/, 'a second `.` in REST declines (overload call order)');

my $e6 = emitted(q{sub f { my $t = 0; $t += $_ for @_; $t } print f(1, 2), "\n";});
like($e6, qr/\(p-incf-raw \$t \$_\)/, 'a numeric accumulator under a for MODIFIER takes the raw slot too');

{
    # perl restores a modifier's `local` once per iteration — the block's own
    # scope — so it desugars too; the old route DROPPED the statement.
    my ($cl, $err) = PCLCore::transpile_raw("$pl2cl " . write_pl(q{our $p = 1; local $p = $_ for (1, 2); print "$p\n";}));
    unlike($err, qr/^PCL: statement dropped/m, 'a `local` under a loop modifier is no longer dropped');
    like($cl, qr/\(p-foreach \(\$_ \(vector 1 2\)\)/, '... it lowers as the block loop');
}

my $e8 = emitted(q{for (1, 2) { eval 'print $_' }});
unlike($e8, qr/\(cons "\$_" \$_\)/, 'the topic loop does not put the GLOBAL $_ in a string eval capture alist');

# ─────────────────────────────────────────────────────────────────────────────
# THE ANSWERS — perl 5.40.3's own output.
# ─────────────────────────────────────────────────────────────────────────────
my @cases = (
  ['modifier and block topic loops return the same statement value',
   'sub f1 { $_ for 1..3 } sub g1 { for (1..3) { $_ } } my @a = f1(); my @b = g1(); my $x = f1(); print scalar(@a), "/", scalar(@b), " [", join(",", map { defined ? $_ : "u" } @a), "] ", defined $x ? "d:$x" : "u", "\n";',
   "1/1 [] d:\n"],
  ['modifier aliases $_ into an array, a scalar list, a hash key',
   'my @l = (1, 2, 3); $_ *= 10 for @l; my ($p, $q) = (1, 2); $_ .= "x" for $p, $q; my %h; $h{$_}++ for qw(a b a); print "@l $p $q ", join(",", map { "$_=$h{$_}" } sort keys %h), "\n";',
   "10 20 30 1x 2x a=2,b=1\n"],
  ['modifier with a scalar ref element does not spread the referent',
   'my $r = [1, 2]; $_ = 5 for $r; print ref($r) || $r, "\n";',
   "5\n"],
  ['modifier: push / .= / += / s/// / next / last',
   'my @o; push @o, $_ * 2 for 1 .. 3; my $s = ""; $s .= $_ for 1 .. 5; my $t = 0; $t += $_ for 1 .. 100; my @w = (" a ", "b "); s/^\s+|\s+$//g for @w; my $n = 0; $n++, next for 1 .. 3; sub lastf { my $c = 0; ($c++ >= 2 and last) for 1 .. 10; $c } print "@o $s $t ", join("|", @w), " $n ", lastf(), "\n";',
   "2 4 6 12345 5050 a|b 3 3\n"],
  ['modifier: a callee sees the loop $_; return from inside',
   'sub tpi { $_ } sub tp { my @t; push @t, tpi() for 1 .. 2; "@t" } sub ret { return $_ * 3 for 2 .. 4; 0 } print tp(), " ", ret(), "\n";',
   "1 2 6\n"],
  ['while / until / foreach-sort / grep list modifiers',
   'my $cnt = 0; $cnt++ while $cnt < 5; my $k = 10; $k-- until $k <= 7; my @st = (3, 1, 2); my $str = ""; $str .= "[$_]" foreach sort @st; my $i = 0; my @g; push @g, $_ for grep { $i++ < 2 } 1 .. 5; print "$cnt $k $str @g\n";',
   "5 7 [1][2][3] 1 2\n"],
  ['nested modifier inside a block loop, die inside, empty list',
   'my @nest; for my $x (1 .. 2) { push @nest, "$x$_" for qw(a b) } my @ev; eval { die "boom\n" for 1 .. 2; 1 } or push @ev, $@; my @emp = (); my $z = 0; $z++ for @emp; print "@nest $z ", @ev;',
   "1a 1b 2a 2b 0 boom\n"],
  ['a `local` under a loop modifier still restores per iteration',
   'our $lp = "o"; local $lp = $_ for (1, 2); print "$lp\n";',
   "o\n"],
  ['the append spelling: plain, x, self, call, *, ternary, modifier',
   'sub a1 { my $s = ""; for my $i (1 .. 3) { $s = $s . "ab" } length($s) . " $s" } sub a2 { my $s = ""; for my $i (1 .. 3) { $s = $s . "xy" x 2 } length($s) . " $s" } sub a3 { my $s = "q"; for my $i (1 .. 3) { $s = $s . $s } length($s) . " $s" } sub fx { "F" } sub a4 { my $s = ""; for my $i (1 .. 2) { $s = $s . fx() } length($s) . " $s" } sub a5 { my $s = ""; for my $i (1 .. 3) { $s = $s . $i * 2 } length($s) . " $s" } sub a6 { my $s = ""; for my $i (1 .. 3) { $s = $s . ($i > 1 ? "b" : "a") } length($s) . " $s" } sub a7 { my $s = "z"; $s = $s . "!" for 1 .. 3; length($s) . " $s" } print join("|", a1(), a2(), a3(), a4(), a5(), a6(), a7()), "\n";',
   "6 ababab|12 xyxyxyxyxyxy|8 qqqqqqqq|2 FF|3 246|3 abb|4 z!!!\n"],
  ['the declined append spellings keep their answers',
   'sub b1 { my $s = "1"; for my $i (1 .. 3) { $s = $s . $i + 1 } length($s) . " $s" } sub b2 { my @a = (1, 2); my $s = ""; for my $i (1 .. 2) { $s = $s . join ",", @a } length($s) . " $s" } sub b3 { my $s = ""; my $t = ""; for my $i (1 .. 2) { $s = $s . "s"; $t = $s } "$s $t" } sub b4 { my $s = 5; for my $i (1 .. 2) { $s = $s . 0 } $s + 1 } print join("|", b1(), b2(), b3(), b4()), "\n";',
   "4 1234|6 1,21,2|ss ss|501\n"],
  ['string eval inside a topic loop runs its OWN inner topic loop',
   'for (1, 2) { eval q{print "[$_]"; for (7) { print "<$_>" } print "{$_}"} } eval q{print "[$_]"; for (8) { print "<$_>" } print "{$_}"} for 3, 4; print "\n";',
   "[1]<7>{1}[2]<7>{2}[3]<8>{3}[4]<8>{4}\n"],
);

my $prog = "no warnings;\n";
$prog .= "{ print \"#$_\\n\"; $cases[$_][1] }\n" for 0 .. $#cases;
my %got;
my $k;
for my $line (split /^/, run_pl($prog)) {
    if ($line =~ /^#(\d+)$/) { $k = $1; $got{$k} = ""; next }
    $got{$k} .= $line if defined $k;
}
for my $i (0 .. $#cases) {
    my ($desc, undef, $want) = @{ $cases[$i] };
    is($got{$i}, $want, $desc);
}

done_testing();

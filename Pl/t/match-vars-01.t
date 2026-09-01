#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# match-vars-01.t: $& (MATCH), $` (PREMATCH), $' (POSTMATCH) runtime behaviour.
#
# These punctuation match variables were previously broken at the codegen level:
# $' was emitted as a bare `$'` (CL quote reader macro) and $` as `$`` (CL
# quasiquote), so any code using them produced unreadable Lisp.  They are now
# mapped to pipe-quoted symbols (|$&| |$`| |$'|), defvar'd/exported, and *set*
# on every successful match/substitution via set-match-vars.

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

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = PCLCore::transpile(qq{$pl2cl $pl_file});
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

plan tests => 39;

# 1. $& whole match
is run_cl(<<'END'), "world\n", '$& is the whole matched string';
"hello world" =~ /w\w+/;
print "$&\n";
END

# 2. $` prematch and $' postmatch
is run_cl(<<'END'), "abc|def\n", '$` prematch and $\' postmatch';
"abcXYZdef" =~ /XYZ/;
print "$`|$'\n";
END

# 3. All three together
is run_cl(<<'END'), "[ab][cd][ef]\n", '$`/$&/$\' partition the string';
"abcdef" =~ /cd/;
print "[$`][$&][$']\n";
END

# 4. Match vars only update on success (failed match leaves them)
is run_cl(<<'END'), "cd\n", 'failed match does not clobber $&';
"abcdef" =~ /cd/;
"abcdef" =~ /zzz/;   # fails
print "$&\n";
END

# 5. $& after substitution
is run_cl(<<'END'), "foo\n", '$& set by s///';
my $s = "foofoofoo";
$s =~ s/foo/bar/;
print "$&\n";
END

# 6. $& in s///e replacement
is run_cl(<<'END'), "[A][B]\n", '$& available inside s///e';
my $s = "ab";
$s =~ s/(\w)/"[".uc($&)."]"/ge;
print "$s\n";
END

# 7. $` / $' standalone (not interpolated) assign to a variable
is run_cl(<<'END'), "pre=abc post=def\n", '$` and $\' as standalone scalars';
"abcMMMdef" =~ /MMM/;
my $pre  = $`;
my $post = $';
print "pre=$pre post=$post\n";
END

# ── qr// stringification preserves modifier flags (Perl 5.14+ "(?^FLAGS:pat)").
#    The flag check used non-existent keys (:case-insensitive ...) so flags were
#    always dropped; Perl emits them in the fixed order m,s,i,x.

# 8. single flag
is run_cl(qq{print qr/abc/i, "\\n";\n}), "(?^i:abc)\n",
    'qr/abc/i stringifies as (?^i:abc)';

# 9. all four flags, in Perl's canonical m,s,i,x order regardless of source order
is run_cl(qq{print qr/abc/imsx, "\\n";\n}), "(?^msix:abc)\n",
    'qr/abc/imsx stringifies as (?^msix:abc)';

# 10. a subset preserves order
is run_cl(qq{print qr/abc/xs, "\\n";\n}), "(?^sx:abc)\n",
    'qr/abc/xs stringifies as (?^sx:abc)';

# 11. no flags
is run_cl(qq{print qr/abc/, "\\n";\n}), "(?^:abc)\n",
    'qr/abc/ stringifies as (?^:abc)';

# 12. an interpolated case-insensitive qr keeps its flag when matched
is run_cl(qq{my \$q = qr/HELLO/i; print(("hello world" =~ \$q) ? "match\\n" : "no\\n");\n}),
    "match\n",
    'qr/HELLO/i still matches case-insensitively when used as a variable';

# ── Unknown ${^NAME} caret variables degrade to ordinary main-forced global
#    scalars (perlvar) instead of aborting the transpile.  Previously any
#    unmodelled ${^...} threw "unsupported special variable" and killed the
#    whole file (e.g. magic.t was 0/208 because of ${^TAINT} on line 44).

# 13. an unallocated caret name reads as undef
is run_cl(<<'END'), "undef\n", '${^MPE} reads as undef before assignment';
print defined(${^MPE}) ? "def\n" : "undef\n";
END

# 14. it autovivifies and increments like a normal global (magic.t's assertion)
is run_cl(<<'END'), "1\n", '++${^MPE} is 1 (autovivifies from undef)';
print ++${^MPE}, "\n";
END

# 15. it holds an assigned value
is run_cl(<<'END'), "42\n", '${^MY_VAR} round-trips an assigned value';
${^MY_VAR} = 42;
print ${^MY_VAR}, "\n";
END

# ── @- (@LAST_MATCH_START) and @+ (@LAST_MATCH_END): match/group offset arrays
#    set on every successful match.  Element 0 is the whole match; element N is
#    capture group N.  (magic.t lines 627-628.)

# 16. @- / @+ as plain arrays
is run_cl(<<'END'), "0 0 2 7 | 10 1 6 10\n", '@- and @+ hold match/group offsets';
"I like pie" =~ /(I) (like) (pie)/;
my @s = @-;
my @e = @+;
print "@s | @e\n";
END

# 17. @- / @+ interpolated directly (the original 5.6.1 interpolation bug)
is run_cl(<<'END'), "0 0 2 7\n10 1 6 10\n", '@- and @+ interpolate in strings';
"I like pie" =~ /(I) (like) (pie)/;
print "@-\n@+\n";
END

# ── $$ is assignable (Perl 5.16+); previously it was a bare integer, not a box.

# 18. $$ reads as the real pid (> 0)
is run_cl(<<'END'), "ok\n", '$$ reads as a positive pid';
print $$ > 0 ? "ok\n" : "no\n";
END

# 19. $$ can be assigned (magic.t: 'is $$, 42')
is run_cl(<<'END'), "42\n", '$$ can be modified';
$$ = 42;
print "$$\n";
END

# ── The BRACE spelling of a punctuation variable: `@{+}`, `@{-}`, `%{+}` ─────
# Perl's `${ NAME }` takes a punctuation name as readily as an identifier, so
# these are the variables @+ / @- / %+ themselves.  PPI produces a single Magic
# token for the identifier and caret spellings but Cast + Block{Operator} for
# these, so they were a SILENT EMPTY list in code and a die inside a regex
# ("cannot compile interpolated regex reference '@{+}'") — which was all of
# t/re/pat_rt_report.t, 2513 rows (#314).  Expectations probed against perl.

# 20. @{+} / @{-} in ordinary code
is run_cl(<<'END'), "3/1\n", '@{+} and @{-} are the magic arrays';
"abcd" =~ /bc/;
my @p = @{+};
my @m = @{-};
print "@p/@m\n";
END

# 21. …and interpolated in a string
is run_cl(<<'END'), "3/1\n", '@{+} interpolates like @+';
"abcd" =~ /bc/;
print "@{+}/@{-}\n";
END

# 22. …and inside a regex, which is where perl's own suite spells it
is run_cl(<<'END'), "ok\n", '@{+} interpolates into a pattern (Bug 27940)';
"abcd" =~ /bc/;
print "ok\n" if "A@+B" =~ /A@{+}B/;
END

# 23. %{+} is the named-capture hash
is run_cl(<<'END'), "n\n", '%{+} is %+';
"x" =~ /(?<n>x)/;
print join(",", sort keys %{+}), "\n";
END

# 24. $#- / $#+ — PPI lexes these as ONE Magic token, not an ArrayIndex, so
# they used to be emitted as the literal (unbound) symbols |$#-| / |$#+|.
is run_cl(<<'END'), "3 3\n", '$#- and $#+ are the last indices of @- and @+';
"I like pie" =~ /(I) (like) (pie)/;
print "$#- $#+\n";
END

# 25. INVERSE: the ordinary `@{…}` derefs must not have moved.
is run_cl(<<'END'), "7 8|3|A3B\n", 'ordinary @{$ref} and @{[expr]} are untouched';
my $r = [7,8];
my @c = @{[1+2]};
print "@{$r}|@c|", ("A3B" =~ /A@{[1+2]}B/ ? "A3B" : "no"), "\n";
END

# 26. @{^CAPTURE} (5.26+) and its two hash synonyms.  The array had no runtime
# variable at all (the emission was a BARE symbol, which reads down-cased under
# :invert and aborted the load unbound), and `$#{^CAPTURE}` — the one spelling
# PPI hands over as Cast + Block — was dropped whole (task #412; 5 rows of
# t/re/pat.t).  One program, every shape, all values perl-probed.
is run_cl(<<'END'), "2 1 a|b\n0 -1 \n1 0 a\nx a x\n", '@{^CAPTURE} / $#{^CAPTURE} / %{^CAPTURE}';
sub show { print scalar(@{^CAPTURE}), " ", $#{^CAPTURE}, " ",
                 join("|", map { defined $_ ? $_ : "U" } @{^CAPTURE}), "\n" }
"abc" =~ /(a)(b)/;   show();
"abc" =~ /a/;        show();
"abc" =~ /(a)(z)?/;  show();
"abc" =~ /(?<x>a)/;
print join(",", sort keys %{^CAPTURE}), " ", ${^CAPTURE}[0], " ",
      join(",", sort keys %{^CAPTURE_ALL}), "\n";
END

# 27. @- and @+ are sized DIFFERENTLY, and perl means it (task #417): @- stops
# after the last PARTICIPATING group, @+ runs to the pattern's group count.
is run_cl(<<'END'), "3 1\n2 2\n0 0\n", '$#+ counts the groups, $#- the matched ones';
"ab" =~ /(a)(x)?(y)?/; print "$#+ $#-\n";
"ab" =~ /(a)(b)/;      print "$#+ $#-\n";
"ab" =~ /a/;           print "$#+ $#-\n";
END

# 28. s/// with no match returns perl's FALSE — PL_sv_no, the ("",0) dualvar —
# not the number 0, so it prints as nothing (task #416).  tr/// really does
# return a count of 0 there (probed), and m// already answered "".
is run_cl(<<'END'), "1:<> 0 T F\n2:<1>\n3:<0>\n4:<>\n", 's/// no-match returns "" (PL_sv_no)';
my $q = "abc";
my $n = ($q =~ s/zzz/x/);
print "1:<$n> ", $n+0, " ", (defined $n ? "T" : "F"), " ", ($n ? "T" : "F"), "\n";
my $m = ($q =~ s/b/B/);   print "2:<$m>\n";
my $t = "abc"; my $c = ($t =~ tr/z/y/);  print "3:<$c>\n";
my $u = ("abc" =~ /zzz/); print "4:<$u>\n";
END

# 29-31: $&/$`/$' are CUT ON DEMAND from the last match's offsets (task #477 —
# building them eagerly made every scalar-context m//g loop quadratic: 100k
# chars 3.4 s, 200k 12.8 s, where perl does 1M in 0.09 s).  These three rows
# are the invariants a deferred cut can break; every expectation is perl
# 5.40.3's own output.

# 29. THE ONE A LAZY CUT COULD GET WRONG: the subject is MUTATED between the
# match and the read.  perl answers from the string as it was AT MATCH TIME,
# and so must this — which holds because every string writer in the runtime
# builds a new string rather than mutating one (lvalue substr, 4-arg substr,
# tr///, chop probed here; see the set-match-vars comment).
is run_cl(<<'END'), "1:[ab][cd][ef] Zbcdef\n2:[ab][cd][ef] zbcdef\n3:[ab][cd][ef] aQcde\n",
   '$`/$&/$\' answer from the subject AS IT WAS AT MATCH TIME';
my $m = "abcdef"; $m =~ /cd/; substr($m, 0, 1) = "Z";
print "1:[$`][$&][$'] $m\n";
my $t = "abcdef"; $t =~ /cd/; $t =~ tr/a/z/;
print "2:[$`][$&][$'] $t\n";
my $c = "abcdef"; $c =~ /cd/; chop $c; substr($c, 1, 1, "Q");
print "3:[$`][$&][$'] $c\n";
END

# 30. The symbolic spelling ${"&"} (task #505) still answers.  A computed magic
# scalar holds NO value in its symbol, so the `boundp`/`symbol-value` route
# that serves ${NAME} has to know about it.
is run_cl(<<'END'), "[f][oob][ar]\n", '${"&"} / ${"`"} / ${"\'"} read the computed match vars';
"foobar" =~ /oob/;
print "[", ${"`"}, "][", ${"&"}, "][", ${"'"}, "]\n";
END

# 31. A read MEMOISES the cut; the next match must not answer from the memo.
is run_cl(<<'END'), "[y][y][q][p][r]\n", 'a new match invalidates the memoised $&';
"xyz" =~ /y/;
my $a1 = $&; my $a2 = $&;
"pqr" =~ /q/;
print "[$a1][$a2][$&][$`][$']\n";
END

# 32-34: the #680 per-match cost work (memoized p-regex / struct-cached
# scanner / high-water capture clear / in-place @-/@+ element boxes).  Every
# expectation is perl 5.40.3's own output.

# 32. @-/@+ ELEMENTS ARE MAGIC in perl: a saved \$-[0] reads the CURRENT
# match.  The in-place box reuse (%p-at-elem-set) is what makes this true —
# rebuilding with fresh boxes left the saved ref on the stale box (a live
# divergence before #680).  \@- identity must survive matches too.
is run_cl(<<'END'), "elem-ref=2 arr-ref=2\n", 'a saved \\$-[0] reads the CURRENT match (magic elements)';
"ab"=~/(a)/;
my $r=\$-[0]; my $ra=\@-;
"zzb"=~/b/;
print "elem-ref=$$r arr-ref=$$ra[0]\n";
END

# 33. THE HIGH-WATER CLEAR must be invisible: a 0-group SUCCESSFUL match
# clears $1..$N from the previous match (perl does), a FAILED match keeps
# them, and a match with FEWER groups clears the excess.
is run_cl(<<'END'), "a:1undef 4undef\nb:1z\nc:1c 2undef\n", 'capture vars clear per perl across group counts';
"abcd" =~ /(a)(b)(c)(d)/;
"q" =~ /q/;
print "a:", (defined $1 ? "1def" : "1undef"), " ", (defined $4 ? "4def" : "4undef"), "\n";
"z" =~ /(z)/;
"q" =~ /x/;
print "b:1$1\n";
"ab" =~ /(a)(b)/;
"cd" =~ /(c)/;
print "c:1$1 2", (defined $2 ? $2 : "undef"), "\n";
END

# 34. THE MEMO CANNOT CONFLATE: the same pattern TEXT under different
# modifiers is a different op (p-regex keys on the whole source text), and an
# interpolated /$pat/g (p-regex-from-parts, list-keyed in the same table)
# still iterates and terminates.
is run_cl(<<'END'), "mods=01\ninterp-g=2\n", 'regex-op memo keys keep modifiers and interpolation apart';
my $m1 = "ABC" =~ /abc/ ? 1 : 0;
my $m2 = "ABC" =~ /abc/i ? 1 : 0;
print "mods=$m1$m2\n";
my $pat = "b"; my $c = 0;
$c++ while "abcabc" =~ /$pat/g;
print "interp-g=$c\n";
END

# ── #962 / #459: A FAILED MATCH IN LIST CONTEXT IS THE EMPTY LIST ────────────
# perlop: "in list context a failed match returns the empty list".  PCL's
# do-regex-match answered raw NIL there, and raw nil is the runtime's empty
# list to ONE consumer only — %p-flatten-list drops it as an empty-list/hole
# marker.  Every other list consumer reads it as one slot: p-array-fill keeps
# it as an array HOLE and p-flatten-args spreads it as one argument.  So a
# failed match ALONE on the RHS was right (`my @l = (/zzz/)` is 0) and a
# failed match anywhere ELSE in a list contributed a phantom element that
# shifted every value after it.  It now returns %p-empty-list — a zero-length
# vector, the shape the SUCCESS arm and the /g list arm already yield, which
# every consumer splices to nothing with no arm of its own.

# 35. THE ARGUMENT LIST is where it bites hardest: perl hands the callee one
# argument, PCL handed it two.  The capture-BEARING failure is the same bug
# (a captured miss and a capture-less miss are both the empty list), and the
# two successful twins are the negatives that must not move.
is run_cl(<<'END'), "n=1 [d]\nn=2 [1][d]\nn=3 [a][a][d]\nn=1 [d]\n", 'a failed match contributes NO argument (#459)';
sub sh { print "n=", scalar(@_), " [", join("][", map { defined $_ ? $_ : "U" } @_), "]\n" }
$_ = 'aaaccc';
sh(/a*b/, "d");
sh(/a/, "d");
sh(/(a)(a)/, "d");
sh(/(z)(z)/, "d");
END

# 36. THE SHAPE THAT MAKES IT A SILENT WRONG rather than a count: `ok /pat/,
# "desc"` is the Test::More idiom, so on a MISS perl's $_[0] is the
# DESCRIPTION (a true string) and $_[1] is undef — the row reads "ok" with no
# name.  PCL's phantom "" took $_[0] and the description slid into $_[1], so
# the row reported the opposite verdict.  This is #940's residue, isolated.
is run_cl(<<'END'), "ok - U\nok - hit\n", 'a miss does not shift `ok /pat/, "desc"` (#962)';
sub myok { print(($_[0] ? "ok" : "not ok"), " - ", (defined $_[1] ? $_[1] : "U"), "\n") }
$_ = "zzz";
myok /q+/, "miss";
myok /z+/, "hit";
END

# 37. EVERY LIST CONSUMER, because the fix is a VALUE change and each consumer
# has its own walk: array assignment, map/grep/sort, join, push, foreach, hash
# assignment (an odd phantom would have shifted the key/value pairing), a miss
# in the MIDDLE of a literal list, and the array-HOLE question — p-array-fill's
# nil arm is exactly what made the old value a defined-less phantom slot.
is run_cl(<<'END'), "arr=1\nmap=1\ngrep=1\nsort=1\njoin=b\npush=1\neach=1\nhash=k,v\nmid=2:1 2\nhole=D:b\n", 'a failed match splices to nothing in every list consumer';
$_ = "aaa";
my @a = (/zzz/, "b");                 print "arr=", scalar(@a), "\n";
my @m = map { "x" } (/zzz/, "b");     print "map=", scalar(@m), "\n";
my @g = grep { 1 } (/zzz/, "b");      print "grep=", scalar(@g), "\n";
my @s = sort (/zzz/, "b");            print "sort=", scalar(@s), "\n";
print "join=", join("|", /zzz/, "b"), "\n";
my @p; push @p, /zzz/, "b";           print "push=", scalar(@p), "\n";
my $c = 0; foreach my $e (/zzz/, "b") { $c++ }  print "each=$c\n";
my %h = (/zzz/, "k", "v");            print "hash=", join(",", %h), "\n";
my @n = (1, /zzz/, 2);                print "mid=", scalar(@n), ":@n\n";
my @h2 = (/zzz/, "b");                print "hole=", (defined $h2[0] ? "D" : "U"), ":$h2[0]\n";
END

# 38. THE NEIGHBOUR THAT MUST NOT MOVE (#416): the SCALAR answer of a failed
# match is perl's DEFINED "" — not undef, not 0 — and s/// answers the same
# while tr/// answers the count 0.  Only the LIST-context value changed, so
# every row here reads the same before and after (it passes on the base tree,
# which is the point of having it).
is run_cl(<<'END'), "m=<>D\ns=<>D\nt=<0>D\ng=<>D\nif=F not=T\nnum=1\n", 'the scalar answer of a failed match is unchanged (#416)';
my $q = "abc";
my $m = ($q =~ /zzz/);   print "m=<$m>", (defined $m ? "D" : "U"), "\n";
my $s = ($q =~ s/zzz/x/);print "s=<$s>", (defined $s ? "D" : "U"), "\n";
my $t = ($q =~ tr/z/y/); print "t=<$t>", (defined $t ? "D" : "U"), "\n";
my $g = ($q =~ /zzz/g);  print "g=<$g>", (defined $g ? "D" : "U"), "\n";
print "if=", ($q =~ /zzz/ ? "T" : "F"), " not=", ($q !~ /zzz/ ? "T" : "F"), "\n";
print "num=", (($q =~ /zzz/) + 1), "\n";
END

# 39. THE OTHER LIST-CONTEXT ARMS and the two consumers that read the value as
# a COUNT: /g in list context (which already yielded an empty vector, so it is
# the shape the no-/g arm was taught), the `() = ` count, a list assignment in
# BOOLEAN position (the count must stay 0 = false), the `my ($x) = $s =~ /…/`
# loop idiom, and a failed match RETURNED from a sub in list context.
is run_cl(<<'END'), "gmiss=1\nghit=3:1 2 b\ncount0=0\nbool=FT\nloop=2/1\nret=1\n", 'count, boolean and return consumers of an empty match list';
$_ = "a1b2";
my @c = (/zzz/g, "b");                 print "gmiss=", scalar(@c), "\n";
my @d = (/(\d)/g, "b");                print "ghit=", scalar(@d), ":@d\n";
my $n = (() = /zzz/);                  print "count0=$n\n";
print "bool=", ((my ($k) = /zzz/) ? "T" : "F"), ((my ($j) = /a/) ? "T" : "F"), "\n";
my @src = ("a1", "bb", "c2");
my $i = 0; my $u = 0;
for my $t (@src) { my ($z) = $t =~ /(\d)/; defined $z ? $i++ : $u++ }
print "loop=$i/$u\n";
sub sub_ret { my $x = shift; $x =~ /zzz/ }
my @r = (sub_ret("q"), "b");           print "ret=", scalar(@r), "\n";
END

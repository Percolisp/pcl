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

plan tests => 34;

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

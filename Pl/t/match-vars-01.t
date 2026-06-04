#!/usr/bin/env perl
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

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>&1`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

plan tests => 19;

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

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

plan tests => 7;

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

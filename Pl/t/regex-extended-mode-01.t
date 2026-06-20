#!/usr/bin/env perl
# regex-extended-mode-01.t — the /x (extended mode) cl-ppcre workaround.
#
# cl-ppcre does NOT restore extended-mode after an inline (?-x:..)/(?x:..) mode
# group, so a /x regex that mixes whitespace/comments with a scoped (?-x:..) span
# silently fails to match (it blocked Text::ParseWords::parse_line). PCL strips
# the /x layer itself for patterns that contain an x mode-modifier, leaving plain
# /x patterns on cl-ppcre's native (correct) extended mode.
# See docs/clppcre-extended-mode-modifier-bug.md.

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

plan tests => 6;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>/dev/null`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub test_cl {
    my ($name, $code, $expected) = @_;
    is(run_cl($code), $expected, $name);
}

# The Text::ParseWords::parse_line shape: /x with comments + a (?-x:$delim) span.
# (Uses real newlines so the # comments terminate correctly.)
test_cl('/x with comments and a scoped (?-x:) delimiter matches',
    qq{my \$delim = q{\\s+};\n}
  . qq{my \$ok = "foo bar" =~ /^ (\\w+?)      # a word\n}
  . qq{                         (?-x:\$delim) # the delimiter, x off\n}
  . qq{                         (\\w+)        # another word\n}
  . qq{                        /x;\n}
  . qq{print "[", (\$ok?1:0), "][\$1][\$2]\\n";\n},
    "[1][foo][bar]\n");

# Scope: whitespace is LITERAL inside (?-x:), STRIPPED outside; plain /x and a
# non-x modifier (?i:) are left on cl-ppcre untouched.
test_cl('/x scope: literal ws inside (?-x:), stripped outside; others untouched',
    'print "[", ("a b" =~ /a (?-x:\ ) b/x ? 1:0), "]",'
  . '       "[", ("ab"  =~ /a (?-x:\ ) b/x ? 1:0), "]",'
  . '       "[", ("abc" =~ /a b c/x ? 1:0), "]",'
  . '       "[", ("aBc" =~ /a (?i:b) c/x ? 1:0), "]\n";',
    "[1][0][1][1]\n");

# (?x:) turning extended ON inside a non-/x base regex.
test_cl('(?x:) enables extended mode inside a non-/x pattern',
    'print "[", ("abc" =~ /a(?x: b )c/ ? 1:0), "]\n";',
    "[1]\n");

# A quantifier immediately after an x-group must still bind to the group
# (the mode-restore must not steal it): (?-x:a){3} means a{3}.
test_cl('quantifier after an x-group binds to the group, not the restore',
    'print "[", ("aaa" =~ /^(?-x:a){3}$/x ? 1:0), "]",'
  . '       "[", ("aa"  =~ /^(?-x:a){3}$/x ? 1:0), "]\n";',
    "[1][0]\n");

# (?#...) comment groups: their # and spaces are literal, not /x comments.
test_cl('(?#...) comment group is not treated as an /x comment',
    'print "[", ("a b" =~ /a (?#note: spaces here) (?-x:\ ) b/x ? 1:0), "]\n";',
    "[1]\n");

# Nested scopes: (?-x: .. (?x: .. ) .. ) — each level restores correctly.
test_cl('nested (?-x: (?x: ) ) scopes each restore the enclosing mode',
    'print "[", ("a b c d e" =~ /a (?-x: b (?x:c) d ) e/x ? 1:0), "]\n";',
    "[1]\n");

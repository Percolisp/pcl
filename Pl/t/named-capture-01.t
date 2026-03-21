#!/usr/bin/env perl
# named-capture-01.t: %+ named regex capture hash tests

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
    $out =~ s/^caught .*\n//gm;
    $out =~ s/^compilation unit.*\n//gm;
    $out =~ s/^\s*Undefined.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

plan tests => 10;

# 1. Basic named capture
is run_cl(<<'END'), "foo\n", 'basic named capture $+{name}';
"foo123" =~ /(?<word>[a-z]+)/;
print $+{word}, "\n";
END

# 2. Multiple named captures
is run_cl(<<'END'), "2024 03\n", 'multiple named captures';
"2024-03-21" =~ /(?<year>\d{4})-(?<month>\d{2})/;
print "$+{year} $+{month}\n";
END

# 3. Named captures also set $1, $2
is run_cl(<<'END'), "foo bar\n", 'named captures also set $1/$2';
"foo bar" =~ /(?<first>\w+) (?<second>\w+)/;
print "$1 $2\n";
END

# 4. Failed match clears %+
is run_cl(<<'END'), "hello\nafter\n", 'failed match clears %+';
"hello" =~ /(?<w>\w+)/;
print "$+{w}\n";
"hello" =~ /(?<x>\d+)/;
print defined($+{w}) ? $+{w} : "after", "\n";
END

# 5. $+{name} in string interpolation
is run_cl(<<'END'), "year=2024 month=03\n", '$+{name} in string interpolation';
"2024-03" =~ /(?<year>\d{4})-(?<month>\d{2})/;
print "year=$+{year} month=$+{month}\n";
END

# 6. Mix of named and unnamed capture groups
is run_cl(<<'END'), "foo bar\n", 'named and unnamed groups coexist';
"foo bar" =~ /(?<first>\w+) (\w+)/;
print "$+{first} $2\n";
END

# 7. s///e using $+{name} in replacement
is run_cl(<<'END'), "HELLO\n", 's///e with named capture in replacement';
my $s = "hello";
$s =~ s/(?<w>\w+)/uc($+{w})/e;
print "$s\n";
END

# 8. Optional group that doesn't match leaves key absent/undef
is run_cl(<<'END'), "word=hello num=undef\n", 'unmatched optional group is undef in %+';
"hello" =~ /(?<num>\d+)?(?<word>[a-z]+)/;
my $num = defined($+{num}) ? $+{num} : "undef";
print "word=$+{word} num=$num\n";
END

# 9. keys %+ returns the named groups that matched
is run_cl(<<'END'), "month year\n", 'keys %+ lists named captures';
"2024-03" =~ /(?<year>\d{4})-(?<month>\d{2})/;
print join(" ", sort keys %+), "\n";
END

# 10. /g in scalar context sets %+ on each iteration
is run_cl(<<'END'), "cat dog\n", '/g updates %+ each iteration';
my $str = "cat and dog";
my @words;
while ($str =~ /(?<w>\w+)/g) {
    push @words, $+{w} if $+{w} ne "and";
}
print join(" ", @words), "\n";
END

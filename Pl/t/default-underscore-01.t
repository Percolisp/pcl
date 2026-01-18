#!/usr/bin/env perl

use v5.30;
use strict;
use warnings;

use Test::More;
use File::Temp qw(tempfile);

use lib ".";
use Pl::Parser;

# Helper to parse and return CL output
sub parse_pl {
    my $code = shift;
    my $parser = Pl::Parser->new(code => $code);
    return $parser->parse();
}

# Helper to run transpiled code
sub run_pl {
    my $code = shift;
    my $parser = Pl::Parser->new(code => $code);
    my $cl_code = $parser->parse();

    my ($fh, $filename) = tempfile(SUFFIX => '.lisp');
    print $fh $cl_code;
    close $fh;

    my $output = `sbcl --noinform --non-interactive --load cl/pcl-runtime.lisp --load "$filename" 2>&1`;
    unlink $filename;

    # Filter SBCL noise and style warnings
    $output =~ s/^;.*\n//gm;
    $output =~ s/^\s*\n//gm;
    $output =~ s/PCL Runtime loaded\n?//g;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s+//;

    return $output;
}

plan tests => 15;

say "# -------- \$_ with Built-in Functions:";

# Test: length with implicit $_
{
    my $output = run_pl(q{
$_ = "hello";
say length;
});
    like($output, qr/^5/, 'length() with implicit $_ works');
}

# Test: lc with implicit $_
{
    my $output = run_pl(q{
$_ = "HELLO";
say lc;
});
    like($output, qr/^hello/, 'lc() with implicit $_ works');
}

# Test: uc with implicit $_
{
    my $output = run_pl(q{
$_ = "hello";
say uc;
});
    like($output, qr/^HELLO/, 'uc() with implicit $_ works');
}

say "# -------- \$_ in foreach Loops:";

# Test: foreach with implicit $_
{
    my $output = run_pl(q{
my @items = (1, 2, 3);
foreach (@items) {
    say $_;
}
});
    like($output, qr/^1\n2\n3/, 'foreach with implicit $_ works');
}

# Test: foreach with implicit $_ and function call
{
    my $output = run_pl(q{
my @items = ("a", "b", "c");
foreach (@items) {
    say uc;
}
});
    like($output, qr/^A\nB\nC/, 'foreach $_ works with uc()');
}

say "# -------- \$_ with Regex:";

# Test: standalone regex match
{
    my $output = run_pl(q{
$_ = "hello world";
if (/world/) {
    say "matched";
} else {
    say "no match";
}
});
    like($output, qr/^matched/, 'standalone /regex/ uses implicit $_');
}

# Test: substitution on $_
{
    my $output = run_pl(q{
$_ = "hello world";
s/world/there/;
say $_;
});
    like($output, qr/^hello there/, 's/// on implicit $_ works');
}

say "# -------- \$_ in map/grep:";

# Test: grep with $_
{
    my $output = run_pl(q{
my @nums = (1, 2, 3, 4, 5);
my @even = grep { $_ % 2 == 0 } @nums;
say scalar @even;
});
    like($output, qr/^2/, 'grep with $_ works');
}

# Test: map with $_
{
    my $output = run_pl(q{
my @nums = (1, 2, 3);
my @doubled = map { $_ * 2 } @nums;
say join(",", @doubled);
});
    like($output, qr/^2,4,6/, 'map with $_ works');
}

say "# -------- \$_ Direct Access:";

# Test: assign from $_
{
    my $output = run_pl(q{
$_ = "test value";
my $x = $_;
say $x;
});
    like($output, qr/^test value/, 'assign from $_ works');
}

# Test: modify $_
{
    my $output = run_pl(q{
$_ = 10;
$_ = $_ + 5;
say $_;
});
    like($output, qr/^15/, 'modify $_ works');
}

say "# -------- \$_ in Nested Contexts:";

# Test: $_ in nested loops
{
    my $output = run_pl(q{
my @outer = (1, 2);
foreach my $o (@outer) {
    foreach (10, 20) {
        say "$o:$_";
    }
}
});
    like($output, qr/1:10.*1:20.*2:10.*2:20/s, 'nested foreach with $_ works');
}

# Test: $_ preserved in sub
{
    my $output = run_pl(q{
sub double_it {
    return $_ * 2;
}

$_ = 5;
say double_it();
say $_;
});
    like($output, qr/^10\n5/, '$_ accessible in sub and preserved');
}

say "# -------- \$_ Transpilation:";

# Test: $_ generates correct CL code
{
    my $cl = parse_pl('$_ = "hello"; say length;');
    like($cl, qr/pl-length \$_/, 'length() transpiles with $_ parameter');
}

# Test: foreach loop generates $_ binding
{
    my $cl = parse_pl('foreach (1,2,3) { say $_; }');
    like($cl, qr/pl-foreach \(\$_/, 'foreach without var uses $_ as loop var');
}

done_testing();

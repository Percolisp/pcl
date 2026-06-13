#!/usr/bin/env perl
# difftest-eval.pl — differential fuzzer for string-eval lexical capture.
#
# Generates random Perl programs that exercise `eval "CODE"` capturing the
# enclosing scope's lexicals in several ways (axes), runs them through real
# `perl` (the oracle) and through PCL, and reports per-axis mismatches.
#
#   perl tools/difftest-eval.pl [--count N] [--seed S] [--axis NAME] [--show]
#
# For speed, ALL generated cases are emitted into ONE program (each case is a
# uniquely-named sub printing "T<idx>=<value>"), transpiled+run ONCE under PCL
# and once under perl; results are matched per tag.  (A PCL crash truncates the
# tag stream, so the missing tags cluster right after the offending case.)
#
# Axes:
#   read     eval reads an int/string expr over captured vars
#   write    eval writes back to a captured var; printed after
#   closure  eval builds `sub {...captured...}`; called
#   foreach  eval inside `for my $x (...)`, capturing the loop var
#   nested   eval inside a nested block, capturing an outer lexical
#   array    eval reads a captured array element / whole array
#   context  eval in list context (joined)
#
# Integer/string ops only — a mismatch means a CAPTURE bug, not a float/
# stringification divergence (those live in difftest-ops.pl).

use v5.30;
use strict;
use warnings;
use File::Temp qw(tempfile);
use Getopt::Long;

my $count = 400;
my $seed  = $$ ^ time;
my $only_axis;
my $show;
GetOptions(
    'count=i' => \$count, 'seed=i' => \$seed,
    'axis=s'  => \$only_axis, 'show' => \$show,
) or die "bad args\n";
srand($seed);

my @AXES = qw(read write closure foreach nested array context);
@AXES = ($only_axis) if $only_axis;

my $vc = 0;
sub vname { '$v' . (++$vc) }
sub ival  { int(rand(20)) - 5 }
sub pick  { $_[int(rand(@_))] }

sub iexpr {                      # int expression over @_ var names + literals
    my @vars = @_;
    my @terms;
    for (1 .. 1 + int(rand(3))) {
        push @terms, (@vars && rand() < 0.7) ? pick(@vars) : ival();
    }
    join(' ' . pick('+', '-', '*') . ' ', @terms);
}

# Each generator returns the program chunk for case $i: a `sub fN {...}` plus a
# `print "TN=", fN(), "\n";`.  Var names are globally unique (vname); the sub
# name carries the case index.
sub gen_read {
    my $i = shift;
    my ($a, $b) = (vname(), vname());
    my $e = iexpr($a, $b);
    qq{sub f$i { my $a = @{[ival()]}; my $b = @{[ival()]}; return eval '$e'; } print "T$i=", f$i(), "\\n";\n};
}
sub gen_write {
    my $i = shift;
    my $a = vname();
    my $op = pick('=', '+=', '-=', '*=');
    qq{sub f$i { my $a = @{[ival()]}; eval '$a $op @{[1+int(rand(9))]}'; return $a; } print "T$i=", f$i(), "\\n";\n};
}
sub gen_closure {
    my $i = shift;
    my $a = vname();
    my $e = iexpr($a);
    qq{sub f$i { my $a = @{[ival()]}; my \$g = eval 'sub { $e }'; return \$g->(); } print "T$i=", f$i(), "\\n";\n};
}
sub gen_foreach {
    my $i = shift;
    my $x = vname();
    my $e = iexpr($x);
    qq{sub f$i { my \@r; for my $x (1..@{[1+int(rand(3))]}) { push \@r, eval '$e' } return "\@r"; } print "T$i=", f$i(), "\\n";\n};
}
sub gen_nested {
    my $i = shift;
    my ($a, $b) = (vname(), vname());
    my $e = iexpr($a, $b);
    qq{sub f$i { my $a = @{[ival()]}; { my $b = @{[ival()]}; return eval '$e'; } } print "T$i=", f$i(), "\\n";\n};
}
sub gen_array {
    my $i = shift;
    my $an = 'a' . $i;                       # unique array name -> @aN / $aN[..]
    my @vals = map { ival() } 1 .. (2 + int(rand(3)));
    my $body = rand() < 0.5
        ? "\$$an\[" . int(rand(@vals)) . "]"
        : "join(',', \@$an)";
    qq{sub f$i { my \@$an = (@{[join ',', @vals]}); return eval '$body'; } print "T$i=", f$i(), "\\n";\n};
}
sub gen_context {
    my $i = shift;
    my $a = vname();
    qq{sub f$i { my $a = @{[ival()]}; return join(',', eval '($a, $a+1, $a+2)'); } print "T$i=", f$i(), "\\n";\n};
}
my %BUILD = (
    read => \&gen_read, write => \&gen_write, closure => \&gen_closure,
    foreach => \&gen_foreach, nested => \&gen_nested, array => \&gen_array,
    context => \&gen_context,
);

# Build the combined program + a tag->axis map.
my (@chunks, %axis_of);
for my $i (1 .. $count) {
    my $axis = pick(@AXES);
    $axis_of{"T$i"} = $axis;
    push @chunks, $BUILD{$axis}->($i);
}
my $prog = join('', @chunks);

# Run once under perl, once under PCL.
sub run {
    my ($cmd, $prog) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $prog; close $fh;
    my $out = `$cmd $file 2>&1`;
    $out =~ s/^;.*\n//gm; $out =~ s/PCL Runtime loaded\n?//g; $out =~ s/[\0]//g;
    my %tag;
    for my $line (split /\n/, $out) { $tag{$1} = $2 if $line =~ /^(T\d+)=(.*)$/; }
    return \%tag;
}
my $perl = run('perl', $prog);
my $pcl  = run('./runpl', $prog);

# Compare per tag.
my (%pass, %fail, @miss);
for my $i (1 .. $count) {
    my $tag  = "T$i";
    my $axis = $axis_of{$tag};
    my $w = $perl->{$tag};
    next unless defined $w;                  # perl itself didn't emit -> skip
    my $g = $pcl->{$tag} // '<missing>';
    if ($g eq $w) { $pass{$axis}++ }
    else {
        $fail{$axis}++;
        push @miss, { tag => $tag, axis => $axis, want => $w, got => $g,
                      chunk => $chunks[$i - 1] };
    }
}

print "seed=$seed  count=$count\n";
my ($tp, $tf) = (0, 0);
for my $axis (@AXES) {
    my ($p, $f) = ($pass{$axis} // 0, $fail{$axis} // 0);
    $tp += $p; $tf += $f;
    printf "  %-9s %4d pass / %3d fail\n", $axis, $p, $f;
}
printf "TOTAL      %4d pass / %3d fail\n", $tp, $tf;

if (@miss) {
    print "\n=== MISMATCHES ", ($show ? "(all)" : "(first 15; --show for all)"), " ===\n";
    my @s = $show ? @miss : @miss[0 .. ($#miss < 14 ? $#miss : 14)];
    for my $m (@s) {
        (my $c = $m->{chunk}) =~ s/\n/ /g;
        print "--- [$m->{axis}] perl=[$m->{want}] pcl=[$m->{got}]\n    $c\n";
    }
}
exit(@miss ? 1 : 0);

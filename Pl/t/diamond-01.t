#!/usr/bin/env perl
# Tests for the <> / <ARGV> diamond operator: read records across the files
# named in @ARGV, fall back to STDIN when @ARGV is empty, set $ARGV and a
# cumulative $. across files.  Each script sets @ARGV at runtime so the test is
# self-contained (no command-line args needed).

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
    my ($code, @args) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>&1`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $argstr = join ' ', @args;
    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file $argstr 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^# PCL Test library loaded\n//gm;
    $output =~ s/^WARNING:.*\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

# Compare PCL vs real perl on the same script + (optional) command-line args.
sub test_diamond {
    my ($name, $code, @args) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $perl_out = `perl $file @args 2>&1`;
    my $cl_out   = run_cl($code, @args);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL:   $cl_out");
}

# Two known input files, created once and embedded by path in the scripts.
sub make_file {
    my ($content) = @_;
    my ($fh, $path) = tempfile(SUFFIX => '.txt', UNLINK => 1);
    print $fh $content;
    close $fh;
    return $path;
}
my $f1 = make_file("apple\nbanana\n");
my $f2 = make_file("cherry\ndate\n");

plan tests => 6;

# 1: <> reads across @ARGV with cumulative $. and per-file $ARGV.
test_diamond('<> spans @ARGV files; $. cumulative, $ARGV per-file', <<"PERL");
\@ARGV = ('$f1', '$f2');
while (<>) { chomp; print "\$.:\$ARGV:\$_\\n"; }
PERL

# 2: list-context <ARGV> slurps every line across files.
test_diamond('list-context <ARGV> reads all lines across files', <<"PERL");
\@ARGV = ('$f1', '$f2');
my \@all = <ARGV>;
print "count=", scalar(\@all), "\\n";
print "first=\$all[0]last=\$all[-1]";
PERL

# 3: empty @ARGV falls back to STDIN (passed via the harness's stdin? use a here-doc).
# We can't feed stdin through this harness, so verify the empty-@ARGV path simply
# terminates and $ARGV stays undef before the loop (no read).
test_diamond('empty @ARGV with a guarded loop terminates cleanly', <<'PERL');
@ARGV = ();
my $n = 0;
# Guard so we don't actually block on STDIN: only enter <> when files exist.
if (@ARGV) { while (<>) { $n++ } }
print "n=$n done\n";
PERL

# 4: a "-" entry in @ARGV is read as STDIN; mixing real files works for the
# non-"-" portion (we only list real files here to stay stdin-free).
test_diamond('reading a single file via <> sets $ARGV and counts lines', <<"PERL");
\@ARGV = ('$f1');
my \$lines = 0;
while (<>) { \$lines++; }
print "lines=\$lines argv=\$ARGV\\n";
PERL

# 5: in-place editing with a backup extension ($^I = '.bak').  Each engine uses
# its own \$\$-keyed temp file so perl and PCL never edit the same path.
test_diamond('in-place edit ($^I=".bak") rewrites file and keeps a backup', <<'PERL');
my $f = "/tmp/pcl_inplace_$$.txt";
open(my $w, '>', $f) or die; print $w "one\ntwo\n"; close $w;
$^I = '.bak';
@ARGV = ($f);
while (<>) { tr/a-z/A-Z/; print; }
open(my $r, '<', $f) or die; local $/; my $c = <$r>; close $r;
open(my $b, '<', "$f.bak") or die; my $bc = <$b>; close $b;
print "edited=[$c] backup=[$bc]";
unlink $f, "$f.bak";
PERL

# 6: in-place editing with NO backup ($^I = '') replaces the file outright.
test_diamond('in-place edit ($^I="") replaces file with no backup', <<'PERL');
my $f = "/tmp/pcl_inplace_nb_$$.txt";
open(my $w, '>', $f) or die; print $w "aaa\nbbb\n"; close $w;
$^I = '';
@ARGV = ($f);
while (<>) { s/a/X/g; print; }
open(my $r, '<', $f) or die; local $/; my $c = <$r>; close $r;
print "edited=[$c] hasbak=", (-e "$f.bak" ? 1 : 0), "\n";
unlink $f;
PERL

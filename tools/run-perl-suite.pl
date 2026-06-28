#!/usr/bin/env perl

# run-perl-suite.pl — run Perl's own core test files (t/base, t/cmd, t/comp, …)
# through PCL and compare TAP results to real perl, with a crash signature.
#
# This is a bug finder: Perl's distribution t/ tree exercises areas that PCL's
# perl-tests/ corpus (mostly t/op/) never touches.  See
# docs/perl-test-suite-survey.md for the running results and categorisation —
# UPDATE that doc when a row changes so we don't re-investigate the same files.
#
# Usage:
#   tools/run-perl-suite.pl base/rs.t comp/our.t      # specific files (rel to t/)
#   tools/run-perl-suite.pl --dir comp                # all SELF-CONTAINED files in t/<dir>
#   tools/run-perl-suite.pl --tdir /path/to/perl/t --dir base
#
# "Self-contained" = does not `require './test.pl'`, `chdir`, or fiddle @INC in
# BEGIN — those need the perl build tree's harness and won't transpile cleanly.
#
# Output columns: P:perl_ok/notok  C:pcl_ok/notok  STATUS  [crash-signature]

use strict;
use warnings;
use File::Basename qw(dirname);
use Cwd qw(abs_path);

my $root = abs_path(dirname(abs_path($0)) . "/..");
my $pl2cl   = "$root/pl2cl";
my $runtime = "$root/cl/pcl-runtime.lisp";

my $tdir = "/home/bernt/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/t";
my $dir;
my @files;
while (@ARGV) {
  my $a = shift @ARGV;
  if    ($a eq '--tdir') { $tdir = shift @ARGV }
  elsif ($a eq '--dir')  { $dir  = shift @ARGV }
  else                   { push @files, $a }
}
-d $tdir or die "perl t/ tree not found: $tdir (pass --tdir)\n";

# --dir: enumerate self-contained files in t/<dir>
if (defined $dir) {
  for my $f (sort glob "$tdir/$dir/*.t") {
    open my $fh, '<', $f or next;
    local $/; my $src = <$fh>; close $fh;
    # Skip files needing the perl build-tree harness.
    next if $src =~ m{require\s+['"]\./test\.pl};
    next if $src =~ m{\bchdir\b};
    next if $src =~ m{BEGIN[^\n]*\@INC};
    push @files, "$dir/" . (split m{/}, $f)[-1];
  }
}
@files or die "no files (give t-relative paths or --dir <subdir>)\n";

my ($n_ok, $n_diff) = (0, 0);
for my $rel (@files) {
  my $f = "$tdir/$rel";
  unless (-f $f) { printf "%-24s (missing)\n", $rel; next; }

  my $perl = `cd "$tdir" && timeout 30 perl "$f" 2>/dev/null`;
  my $p_ok    = () = $perl =~ /^ok /mg;
  my $p_notok = () = $perl =~ /^not ok /mg;

  my $lisp = "/tmp/run-perl-suite_$$.lisp";
  my $terr = system("perl -I$root $pl2cl '$f' > $lisp 2>/tmp/run-perl-suite_$$.err");
  my $pcl = `cd "$tdir" && timeout 90 sbcl --noinform --non-interactive --load $runtime --load $lisp 2>&1`;
  my $c_ok    = () = $pcl =~ /^ok /mg;
  my $c_notok = () = $pcl =~ /^not ok /mg;

  my $sig = "";
  $sig = "TRANSPILE-FAIL"                         if $terr != 0;
  $sig ||= "unbound:$1"   if $pcl =~ /The variable (\S+) is unbound/;
  $sig ||= "undef-fn:$1"  if $pcl =~ /The function (\S+) is undefined/;
  $sig ||= "parse-error"  if $pcl =~ /PARSE ERROR/;
  $sig ||= "crash:$1"     if $pcl =~ /Unhandled (\S+)/;
  $sig ||= "crash:$1"     if $pcl =~ /debugger invoked on a (\S+)/;

  my $ok = ($p_ok == $c_ok && $p_notok == $c_notok && !$sig);
  $ok ? $n_ok++ : $n_diff++;
  printf "%-24s P:%4d/%-3d C:%4d/%-4d %-5s %s\n",
    $rel, $p_ok, $p_notok, $c_ok, $c_notok, ($ok ? "OK" : "DIFF"), $sig;
}
printf "----\n%d OK, %d DIFF (%d files)\n", $n_ok, $n_diff, $n_ok + $n_diff;
exit($n_diff ? 1 : 0);

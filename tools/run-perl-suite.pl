#!/usr/bin/env perl

# run-perl-suite.pl — run Perl's own core test files (t/base, t/cmd, t/comp,
# t/mro, t/class, …) through PCL and compare TAP results to real perl, with a
# crash signature.  These are the distribution tests that were NOT copied into
# PCL's perl-tests/ sweep corpus (which is almost entirely t/op/), so this is
# the companion sweep for everything the sweep doesn't cover (task #25 / R1
# gate; results catalogue: docs/perl-test-suite-survey.md — UPDATE that doc
# when a row changes so we don't re-investigate the same files).
#
# Usage:
#   tools/run-perl-suite.pl base/rs.t comp/our.t   # specific files (rel to t/)
#   tools/run-perl-suite.pl --dir comp             # all SELF-CONTAINED files in t/<dir>
#   tools/run-perl-suite.pl --all                  # every default dir, NOT-copied files only
#   tools/run-perl-suite.pl                        # == --all
#
# Options:
#   --tdir PATH        perl build t/ tree (default: the 5.40.3 build below)
#   --dir D            add one subdir (repeatable); implies the copied-file filter
#   --all              scan the default dir set (see @DEFAULT_DIRS)
#   --include-copied   with --all/--dir: also run files whose basename exists
#                      in perl-tests/ (default: skip them — the sweep owns those)
#   --jobs N           parallel workers (default 8)
#   --timeout N        per-file SBCL timeout seconds (default 90)
#   --no-core          skip the saved-core fast path (source-load the runtime)
#   --tsv FILE         also write one TSV row per file (rel, P ok/notok,
#                      C ok/notok, status, signature) for diffing runs
#
# Speed: like tools/prove-core, a FRESH SBCL core with the runtime compiled in
# is built once per invocation (never stale, removed on exit); each test then
# starts from the core (~0.003s) instead of recompiling the runtime (~1.2s).
#
# "Self-contained" = does not `require './test.pl'`, `chdir`, or fiddle @INC in
# BEGIN — those need the perl build tree's harness and won't transpile cleanly.
# Dir scans report how many files each filter dropped, so coverage is visible.
#
# Output columns: P:perl_ok/notok  C:pcl_ok/notok  STATUS  [crash-signature]
# STATUS: OK (counts match) | DIFF | TRANSPILE | TIMEOUT | NOTAP (perl itself
# produced no TAP — not comparable, doesn't fail the run; PCL result shown).
# Exit: nonzero iff any DIFF/TRANSPILE/TIMEOUT/MISSING/NO-RESULT.

use strict;
use warnings;
use File::Basename qw(basename dirname);
use File::Temp qw(tempfile tempdir);
use Cwd qw(abs_path);
use POSIX qw(:sys_wait_h _exit);

my $root    = abs_path(dirname(abs_path($0)) . "/..");
my $pl2cl   = "$root/pl2cl";
my $runtime = "$root/cl/pcl-runtime.lisp";

# Dirs worth sweeping.  Excluded on purpose: porting (perl-repo hygiene),
# win32, bigmem (huge memory), perf/benchmark (timing), test_pl (tests the
# harness itself), japh (obfuscated), lib (needs the build-tree module layout).
my @DEFAULT_DIRS = qw(base cmd comp opbasic op mro class run uni re io);

my $tdir = "/home/bernt/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/t";
my ($all, $include_copied, $no_core, $tsv_file);
my $jobs = 8;
my $timeout = 90;
my (@dirs, @files);
while (@ARGV) {
  my $a = shift @ARGV;
  if    ($a eq '--tdir')           { $tdir = shift @ARGV }
  elsif ($a eq '--dir')            { push @dirs, shift @ARGV }
  elsif ($a eq '--all')            { $all = 1 }
  elsif ($a eq '--include-copied') { $include_copied = 1 }
  elsif ($a eq '--jobs')           { $jobs = shift @ARGV }
  elsif ($a eq '--timeout')        { $timeout = shift @ARGV }
  elsif ($a eq '--no-core')        { $no_core = 1 }
  elsif ($a eq '--tsv')            { $tsv_file = shift @ARGV }
  else                             { push @files, $a }
}
-d $tdir or die "perl t/ tree not found: $tdir (pass --tdir)\n";
$all = 1 if !@files && !@dirs;
push @dirs, @DEFAULT_DIRS if $all;

# Files already in the sweep corpus — dir scans skip them by default.  Keyed
# by basename PLUS matching head content (first 300 bytes): several t/ files
# share a basename with a corpus file copied from a DIFFERENT dir (cmd/for.t
# vs op/for.t, class/method.t vs op/method.t, most of uni/) and must still run
# here.  A true copy whose head was locally edited would merely run in both
# sweeps — harmless.
my %corpus_head;
for my $f (glob "$root/perl-tests/*.t") {
  open my $fh, '<', $f or next;
  read $fh, my $head, 300;
  $corpus_head{ basename($f) } = $head // '';
  close $fh;
}
sub in_corpus {
  my ($f) = @_;
  my $head = $corpus_head{ basename($f) };
  return 0 unless defined $head;
  open my $fh, '<', $f or return 0;
  read $fh, my $h, 300;
  close $fh;
  return ($h // '') eq $head;
}

# Enumerate self-contained files in each requested dir.
for my $d (@dirs) {
  my ($n_all, $n_harness, $n_copied) = (0, 0, 0);
  for my $f (sort glob "$tdir/$d/*.t") {
    $n_all++;
    my $base = basename($f);
    if (!$include_copied && in_corpus($f)) { $n_copied++; next }
    open my $fh, '<', $f or next;
    local $/; my $src = <$fh>; close $fh;
    # Skip files needing the perl build-tree harness.
    if ($src =~ m{require\s+['"]\./test\.pl}
        || $src =~ m{\bchdir\b}
        || $src =~ m{BEGIN[^\n]*\@INC}) { $n_harness++; next }
    push @files, "$d/$base";
  }
  printf STDERR "scan t/%-8s %3d files: %3d runnable, %3d need-harness%s\n",
    $d, $n_all, $n_all - $n_harness - $n_copied, $n_harness,
    $n_copied ? ", $n_copied in sweep corpus" : "";
}
@files or die "no files (give t-relative paths, --dir <subdir>, or --all)\n";

# Fresh saved core (tools/prove-core pattern): runtime compiled in ONCE,
# rebuilt every invocation so it can never be stale; removed on exit.
my $core = "";
unless ($no_core) {
  (undef, $core) = tempfile("pcl-suite-core.XXXXXX", TMPDIR => 1, OPEN => 0);
  print STDERR "run-perl-suite: building fresh core from cl/pcl-runtime.lisp ...\n";
  if (system("sbcl --noinform --non-interactive --load \Q$runtime\E "
           . "--eval '(sb-ext:save-lisp-and-die \"$core\" :executable nil)' "
           . ">/dev/null 2>&1") != 0) {
    print STDERR "run-perl-suite: core build FAILED — falling back to source-load\n";
    unlink $core; $core = "";
  }
}
END { unlink $core if $core }
# --core must precede all other toplevel sbcl options.
my $sbcl = $core ? "sbcl --core \Q$core\E --noinform --non-interactive"
                 : "sbcl --noinform --non-interactive --load \Q$runtime\E";

my $tmpdir = tempdir(CLEANUP => 0);
END { system("rm -rf \Q$tmpdir\E") if $tmpdir && -d $tmpdir }

# ---------------------------------------------------------------- worker
sub run_one {
  my ($rel, $result_file) = @_;
  my ($p_ok, $p_notok, $c_ok, $c_notok, $status, $sig) = (0, 0, 0, 0, 'OK', '');
  my $f = "$tdir/$rel";

  unless (-f $f) {
    ($status, $sig) = ('MISSING', '');
    goto WRITE;
  }

  my $perl = `cd \Q$tdir\E && timeout 30 perl \Q$f\E 2>/dev/null`;
  $p_ok    = () = $perl =~ /^ok /mg;
  $p_notok = () = $perl =~ /^not ok /mg;

  (my $safe = $rel) =~ s{/}{_}g;
  my $lisp = "$tmpdir/$safe.lisp";
  my $terr = system("perl -I\Q$root\E \Q$pl2cl\E --no-cache --lenient-ppi \Q$f\E "
                  . "> \Q$lisp\E 2>\Q$lisp\E.err");
  my $pcl = "";
  my $sbcl_exit = 0;
  if ($terr == 0) {
    # CWD = perl's t/ dir (fixture files are opened relative to it); timeout(1)
    # actually kills a hung SBCL (alarm in the parent would leave an orphan).
    my $out = "$tmpdir/$safe.out";
    system("cd \Q$tdir\E && timeout $timeout $sbcl --load \Q$lisp\E > \Q$out\E 2>&1");
    $sbcl_exit = $? >> 8;
    $pcl = do { local $/; my $fh; open($fh, '<', $out) ? (<$fh> // '') : '' };
    $c_ok    = () = $pcl =~ /^ok /mg;
    $c_notok = () = $pcl =~ /^not ok /mg;
  }

  $sig = "TRANSPILE-FAIL"     if $terr != 0;
  $sig ||= "timeout"          if $sbcl_exit == 124;
  $sig ||= "unbound:$1"       if $pcl =~ /The variable (\S+) is unbound/;
  $sig ||= "undef-fn:$1"      if $pcl =~ /The function (\S+) is undefined/;
  $sig ||= "parse-error"      if $pcl =~ /PARSE ERROR/;
  $sig ||= "crash:$1"         if $pcl =~ /Unhandled ([^\s:]+(?::[^\s]+)?)/;
  $sig ||= "crash:$1"         if $pcl =~ /debugger invoked on a (\S+)/;

  $status = $terr != 0                                   ? 'TRANSPILE'
          : $sbcl_exit == 124                            ? 'TIMEOUT'
          : ($p_ok + $p_notok) == 0                      ? 'NOTAP'
          : ($p_ok == $c_ok && $p_notok == $c_notok && !$sig) ? 'OK'
          :                                                'DIFF';

WRITE:
  open my $rf, '>', $result_file or _exit(1);
  print $rf join("\t", $rel, $p_ok, $p_notok, $c_ok, $c_notok, $status, $sig), "\n";
  close $rf;
}

# ---------------------------------------------------- parallel dispatch
my @queue = @files;
my (%children, %results);
while (@queue || %children) {
  while (@queue && keys(%children) < $jobs) {
    my $rel = shift @queue;
    my (undef, $result_file) = tempfile(DIR => $tmpdir, SUFFIX => '.res', OPEN => 0);
    my $pid = fork();
    die "fork: $!" unless defined $pid;
    if ($pid == 0) { run_one($rel, $result_file); _exit(0) }
    $children{$pid} = { rel => $rel, result_file => $result_file, start => time() };
  }
  for my $pid (keys %children) {
    next unless waitpid($pid, WNOHANG) == $pid;
    my $info = delete $children{$pid};
    my $line = '';
    if (open my $rf, '<', $info->{result_file}) { chomp($line = <$rf> // ''); close $rf }
    my @r = split /\t/, $line, 7;
    @r = ($info->{rel}, 0, 0, 0, 0, 'NO-RESULT', '') if @r < 6;
    $results{$info->{rel}} = \@r;
    printf "%-24s P:%4d/%-3d C:%4d/%-4d %-7s %s\n", @r[0 .. 5], $r[6] // '';
    STDOUT->flush();
  }
  # Hard-kill stragglers the in-child timeout somehow missed.
  for my $pid (keys %children) {
    my $info = $children{$pid};
    next unless time() - $info->{start} > $timeout + 40;
    kill 'KILL', $pid; waitpid($pid, 0);
    $results{$info->{rel}} = [$info->{rel}, 0, 0, 0, 0, 'TIMEOUT', '(killed)'];
    printf "%-24s %s\n", $info->{rel}, 'TIMEOUT (killed)';
    delete $children{$pid};
  }
  select(undef, undef, undef, 0.1) if @queue || %children;
}

# ----------------------------------------------------------- summary
my %by_status;
push @{ $by_status{ $results{$_}[5] } }, $_ for keys %results;
print "----\n";
for my $st (sort keys %by_status) {
  my @f = sort @{ $by_status{$st} };
  printf "%-8s %3d%s\n", $st, scalar @f,
    ($st eq 'OK' ? '' : ':  ' . join(', ', @f));
}
my $n_bad = grep { $results{$_}[5] !~ /^(?:OK|NOTAP)$/ } keys %results;
printf "%d files: %d OK, %d NOTAP, %d divergent\n",
  scalar(keys %results), scalar(@{ $by_status{OK} // [] }),
  scalar(@{ $by_status{NOTAP} // [] }), $n_bad;

if ($tsv_file) {
  open my $tf, '>', $tsv_file or die "write $tsv_file: $!\n";
  print $tf join("\t", @{ $results{$_} }), "\n" for sort keys %results;
  close $tf;
  print "wrote $tsv_file\n";
}
exit($n_bad ? 1 : 0);

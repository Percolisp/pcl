#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# board-timing.pl -- the CPAN board timed under perl and under PCL, file by
# file, with PCL's time split into TRANSPILE / LOAD / RUN (task #4787, s540a).
#
# Usage:
#   tools/board-timing.pl [SELECTION] [--modes cold,warm] [--repeat N]
#        [--cache-root DIR] [--cold-continue] [--out FILE.tsv] [--timeout S]
#        [--no-hooks] [--no-perl] [--prof cpu|alloc --prof-dir DIR [--prof-from start]]
#        [--preload FASL] [--env K=V] [--variant NAME]   (a LEVER's A/B: a
#        file loaded in front of the program, an environment setting for the
#        transpile and the run, and the name its rows carry)
#   tools/board-timing.pl --report [--top N] [--norm] FILE.tsv...
#
# SELECTION (any combination; default = the whole board):
#   --list FILE      the dist list (default baselines/cpan-board-dists.tsv)
#   --dist NAME      a dist directory's basename (repeatable; prefix match)
#   --only REGEX     DIST/TNAME must match (TNAME = the path under t/)
#   --files FILE     lines `DIST<TAB>TNAME`: exactly these files
#
# A FILE IS RUN AS THE BOARD RUNS IT (tools/run-dist-t.pl): cwd = the dist
# root; -I <dist>/t/lib, <dist>/lib (not with the list's --no-dist-lib), the
# list's --inc dirs; PERL5LIB = the dist's built prerequisites, for the perl
# run and the SBCL run (not the transpile); the same timeout table
# (baselines/cpan-board-timeouts.tsv, max with --timeout).  ONE difference,
# on purpose: the board transpiles with `pl2cl --no-cache`, which emits
# `(setf *pcl-skip-cache* t)` and so transpiles and source-loads every module
# on every file; here the MODULE CACHE IS ON, in a per-dist cache directory
# (PCL_CACHE_DIR = <cache-root>/<dist>; the saved runtime core is passed
# explicitly, so the fresh directory does not rebuild it):
#
#   cold  the dist's cache directory is emptied first, then its files run in
#         order -- the first file pays every module's transpile + fasl build,
#         later files hit what earlier ones built: "a first run of the suite".
#   warm  the same directory, after the cold pass (or after one throw-away run
#         per file when there was none): "a second run, nothing changed".
#
# The test file itself is transpiled and loaded as TEXT on every run (as the
# board does, and as `pcl` does on the first run of a changed file; `pcl`'s
# script cache would skip both on a repeat run).
#
# HOW EACH NUMBER IS TAKEN
#   wall      Time::HiRes around the child, from fork to reap.
#   cpu       user+sys of the whole process TREE (perl's `times` children
#             delta, so every descendant its parent waited for; 10 ms ticks).
#             A child nobody waits for -- the string-eval transpile server the
#             runtime kills at exit -- is not in it: its time is in `evalx`.
#   phases    a hooks fasl is loaded in front of the program (never part of
#             cl/: it ENCAPSULATES runtime entry points) and keeps, per
#             bucket, wall and CPU EXCLUSIVE of nested buckets; it writes them
#             at exit (an exit hook before the END phase, and again after it).
#             Buckets: modload (p-load-module-cached, p-load-extension: the
#             module's fasl or text load and its top-level code), tap (the
#             Test::More layer cl/pcl-test.lisp, loaded on demand), *compile
#             (SBCL's compile-in-lexenv, by whom: s = the script's own forms,
#             m = a module text, t = the TAP layer, e = a string eval), cfile
#             (a module fasl build), xpile (a module transpile: a pl2cl
#             child, so its CPU is the TREE's, not SBCL's), evalx + xserver
#             (string-eval transpiles through the pl2cl server), evalrun (the
#             eval'd code's own run).
#     A transpile = the test file's pl2cl + xpile + evalx + xserver
#     B load      = SBCL start (fork to the hooks' first form) + modload + tap
#                   + every *compile + cfile
#     C run       = the SBCL wall/CPU minus start and every bucket: the
#                   program, the eval'd code, the END phase.
#   A/B/C CPU: SBCL's own clock for B and C; SBCL's CHILDREN (its tree CPU
#   minus its own at exit) are split -- up to the transpile buckets' wall to
#   A (SBCL waits on a module's pl2cl, so its CPU is about its wall), the rest
#   to C (subprocesses the program itself starts: system, `$^X ...`, pipes).
#   A = the test file's pl2cl tree CPU + that transpile share.
#
# THE COMPARISON (column cmp): C = PCL ran to its plan with no failing row and
# the same ok count as perl, which also ran to its plan; PE = the same rows
# reached as perl and the same ok count, but not a PCL PASS (failing rows perl
# fails too, a shortfall at the same place) -- listed, not compared; S = both
# skip the file; X = anything else (a file that stops early does less work).
#
# --prof cpu|alloc: sb-sprof from the main program's compile->run boundary
# (p-run-compile-phase-blocks) to exit, one report per run in --prof-dir.
#
# --report: the summary tables (markdown) over one or more TSVs; a later
# file's row for the same (dist, file, mode) replaces an earlier one, and with
# --repeat the best rep (least CPU) is the row, its spread beside it.  A
# --variant whose name starts with `precise` is a RE-TIMING of the base (a
# quiet bench): its rows replace the base rows; any other variant (a lever's
# A/B) is its own population -- report it from its own TSV.  --norm: CPU
# normalised for load (see normalise()); --table OUT.tsv: the per-file table.

use strict;
use warnings;
use FindBin;
use lib "$FindBin::RealBin/lib";
use PCLTap ();
use PCLSbcl ();
use PCLProc ();
use PCLDistPrereq ();
use PCLTimeouts ();
use Cwd qw(abs_path);
use File::Basename qw(basename);
use File::Path ();
use File::Spec ();
use File::Temp ();
use Time::HiRes ();

my $root = abs_path("$FindBin::RealBin/..");

my (@want_dist, $only_rx, $files_list, $out, $prof, $prof_dir, $core, $sha);
# THE RUN PHASE (s541a, task #4791): --run, --fasl, --xp-dir, --tree, --perl-c, --perl-late
my ($run_mode, $fasl_mode, $xp_dir, $perl_c, $perl_late) = (0, 0, undef, 0, 0);
my ($list, $modes, $repeat, $cache_root, $timeout, $timeouts_tsv, $hooks, $report, $top)
  = ("$root/baselines/cpan-board-dists.tsv", 'cold,warm', 1, undef, 120,
     "$root/baselines/cpan-board-timeouts.tsv", 1, 0, 60);
my ($perl_side, $cold_continue, $variant, @preload, %xenv) = (1, 0, q{});
my $norm = 0;
my $prof_from = q{boundary};
my $table;    # --report --table OUT.tsv: the per-file table
my @ARGV_COPY = @ARGV;
my @positional;
while (@ARGV) {
  my $a = shift @ARGV;
  if    ($a eq '--list')       { $list = shift @ARGV }
  elsif ($a eq '--dist')       { push @want_dist, shift @ARGV }
  elsif ($a eq '--only')       { my $r = shift @ARGV; $only_rx = qr/$r/ }
  elsif ($a eq '--files')      { $files_list = shift @ARGV }
  elsif ($a eq '--modes')      { $modes = shift @ARGV }
  elsif ($a eq '--repeat')     { $repeat = shift @ARGV }
  elsif ($a eq '--cache-root') { $cache_root = shift @ARGV }
  elsif ($a eq '--out')        { $out = shift @ARGV }
  elsif ($a eq '--timeout')    { $timeout = shift @ARGV }
  elsif ($a eq '--timeouts')   { $timeouts_tsv = shift @ARGV }
  elsif ($a eq '--no-hooks')   { $hooks = 0 }
  elsif ($a eq '--no-perl')    { $perl_side = 0 }
  elsif ($a eq '--cold-continue') { $cold_continue = 1 }
  elsif ($a eq q{--preload}) { push @preload, abs_path(shift @ARGV) }
  elsif ($a eq q{--env})     { my ($k, $v) = split /=/, shift(@ARGV), 2; $xenv{$k} = $v }
  elsif ($a eq q{--variant}) { $variant = shift @ARGV }
  elsif ($a eq '--prof')       { $prof = shift @ARGV }
  elsif ($a eq '--prof-dir')   { $prof_dir = shift @ARGV }
  elsif ($a eq q{--prof-from})  { $prof_from = shift @ARGV }
  elsif ($a eq '--core')       { $core = shift @ARGV }
  elsif ($a eq '--sha')        { $sha = shift @ARGV }
  elsif ($a eq '--report')     { $report = 1 }
  elsif ($a eq '--top')        { $top = shift @ARGV }
  elsif ($a eq q{--norm})       { $norm = 1 }
  elsif ($a eq q{--table})      { $table = shift @ARGV }
  elsif ($a eq q{--run})        { $run_mode = 1 }
  elsif ($a eq q{--fasl})       { $fasl_mode = 1; $run_mode = 1 }
  elsif ($a eq q{--xp-dir})     { $xp_dir = File::Spec->rel2abs(shift @ARGV) }
  elsif ($a eq q{--tree})       { $root = abs_path(shift @ARGV) // die "board-timing.pl: --tree: no such directory\n" }
  elsif ($a eq q{--perl-c})     { $perl_c = 1 }
  elsif ($a eq q{--perl-late})  { $perl_late = 1 }
  elsif ($a =~ /^-/)           { die "board-timing.pl: unknown option $a\n" }
  else                         { push @positional, $a }
}

if ($report) {
  @positional or die "board-timing.pl --report FILE.tsv...\n";
  print $run_mode ? report_run(\@positional, $top) : report(\@positional, $top);
  exit 0;
}
!@positional or die "board-timing.pl: unexpected argument(s) @positional (use --dist NAME)\n";

my %mode = map { $_ => 1 } split /,/, $modes;
!grep { !/^(cold|warm)$/ } keys %mode or die "board-timing.pl: --modes takes cold and/or warm\n";
!defined $prof || $prof =~ /^(cpu|alloc)$/ or die "board-timing.pl: --prof cpu|alloc\n";
$prof_dir //= '.' if $prof;
$sha //= do { my $s = `git -C '$root' log -1 --format=%h -- Pl cl lib vendor 2>/dev/null`; chomp $s; $s || 'unknown' };

my $tmp = File::Temp::tempdir('board-timing-XXXXXX', TMPDIR => 1, CLEANUP => 1);
my $own_cache = !defined $cache_root;
$cache_root //= "$tmp/cache";
$cache_root = File::Spec->rel2abs($cache_root);
private_dir($cache_root);

# The SBCL line: the board's (tools/run-dist-t.pl), with the core resolved
# HERE, under the caller's cache root, so a fresh PCL_CACHE_DIR never builds
# one.
my $runtime = "$root/cl/pcl-runtime.lisp";
$core //= PCLSbcl::cached_core($runtime) or die "board-timing.pl: no cached core for $runtime\n";
my $sbcl = PCLSbcl::sbcl_prefix_str(runtime => $runtime, core => $core, quote => 0);
my $hooks_fasl = $hooks ? build_hooks() : undef;

my @work = select_work();
@work or die "board-timing.pl: the selection is empty\n";
my %file_to = %{ PCLTimeouts::read_timeouts($timeouts_tsv) };

my @COLS = qw(sha mode rep dist file load1 timeout
  p_wall p_cpu p_rc p_ok p_notok p_plan p_skip
  c_class cmp c_rc c_ok c_notok c_plan c_wall c_cpu
  x_wall x_cpu lisp_kb s_wall s_cpu s_self start
  A_wall B_wall C_wall A_cpu B_cpu C_cpu
  modload tap tcompile mcompile scompile ecompile cfile xpile evalx xserver evalrun
  modload_c tap_c tcompile_c mcompile_c scompile_c ecompile_c cfile_c evalrun_c
  n_mod n_fasl n_text n_eval n_xpile boundary mb_consed gc end_wall rec cpu_v variant
  run_how q_init q_wall q_cpu q_sub q_late_wall q_late_cpu q_late_n q_c_wall
  r_wall r_cpu r_lw_wall r_lw_cpu r_late_wall r_late_cpu r_late_n r_sub r_gc r_mb
  pr_wall pr_cpu sread r_rcomp r_rcomp_n r_eval_cpu r_xsrv r_build);
my $ofh = \*STDOUT;
if (defined $out) {
  my $new = !-s $out;
  open $ofh, '>>', $out or die "board-timing.pl: cannot append $out: $!\n";
  $ofh->autoflush(1);
  if ($new) {
    printf $ofh "# taken-at: code %s, %s, load %s; tools/board-timing.pl %s\n", $sha,
      scalar(localtime), load1(), join(' ', map { /\s/ ? "'$_'" : $_ } @ARGV_COPY);
    print $ofh join("\t", @COLS), "\n";
  }
}

my %by_dist;
push @{ $by_dist{$_->{dist}} }, $_ for @work;
for my $dist (sort keys %by_dist) {
  my $files = $by_dist{$dist};
  my $cache = "$cache_root/" . basename($dist);
  my %perl;    # tname -> [rows by rep]
  if ($mode{cold}) {
    # --cold-continue: a dist's cold pass split over several runs (--only):
    # the later parts start from what the earlier parts built, as one run would.
    File::Path::remove_tree($cache) if !$cold_continue;
    private_dir($cache);
    for my $w (@$files) {
      $perl{$w->{tname}}[1] = perl_run($w) if $perl_side;
      emit(pcl_run($w, 'cold', $cache, 1), $perl{$w->{tname}}[1]);
    }
  }
  next unless $mode{warm};
  if (!$mode{cold} && !-d "$cache/modules") {
    private_dir($cache);
    pcl_run($_, 'warmup', $cache, 0) for @$files;    # not recorded
  }
  for my $w (@$files) {
    for my $rep (1 .. $repeat) {
      $perl{$w->{tname}}[$rep] //= perl_run($w, $rep) if $perl_side;
      emit(pcl_run($w, 'warm', $cache, $rep), $perl{$w->{tname}}[$rep]);
    }
  }
}
File::Path::remove_tree($cache_root) if $own_cache;
exit 0;

# ---------------------------------------------------------------- selection --
sub select_work {
  my %want_file;
  if (defined $files_list) {
    open my $fh, '<', $files_list or die "board-timing.pl: cannot read $files_list: $!\n";
    while (<$fh>) {
      chomp;
      next if /^\s*(#|$)/;
      my ($d, $t) = split /\t/;
      $want_file{"$d/$t"} = 1;
    }
  }
  open my $fh, '<', $list or die "board-timing.pl: cannot read the dist list $list: $!\n";
  my @w;
  while (<$fh>) {
    chomp;
    next if /^\s*(#|$)/;
    my ($dname, $flags) = split /\t/;
    # cpan-board.sh's rule (a path with a `/` is under the tree, a bare name
    # under ~/.cpan/build), plus an absolute path as itself (tools/t's fixture).
    my $dir = $dname =~ m{^/} ? $dname
            : $dname =~ m{/}  ? "$root/$dname" : "$ENV{HOME}/.cpan/build/$dname";
    -d "$dir/t" or die "board-timing.pl: no dist directory $dir\n";
    my $base = basename($dir);
    next if @want_dist && !grep { $base =~ /^\Q$_\E(?:-v?\d|$)/ } @want_dist;   # NAME or NAME-VERSION, never Data-Dump for Data-Dumper
    my ($ndl, @inc) = (0);
    for my $fl (split ' ', $flags // '') {
      if    ($fl eq '--no-dist-lib') { $ndl = 1 }
      elsif ($fl =~ /^--inc=(.+)$/)  { push @inc, map { m{^/} ? $_ : "$ENV{HOME}/.cpan/build/$_" } split /,/, $1 }
    }
    $dir = abs_path($dir);
    for my $t (t_files("$dir/t")) {
      (my $tname = $t) =~ s{^\Q$dir\E/t/}{};
      my $rel = "$base/$tname";
      next if $only_rx && $rel !~ $only_rx;
      next if %want_file && !$want_file{$rel};
      push @w, { dist => $dir, base => $base, t => $t, tname => $tname, rel => $rel,
                 ndl => $ndl, inc => [@inc] };
    }
  }
  return @w;
}

# tools/cpan-scoreboard.pl's t_files: recursive, sorted.
sub t_files {
  my ($tdir) = @_;
  my @out;
  my @todo = ($tdir);
  while (defined(my $d = shift @todo)) {
    opendir(my $dh, $d) or next;
    for my $e (sort grep { !/^\./ } readdir $dh) {
      my $p = "$d/$e";
      if (-d $p) { push @todo, $p }
      elsif ($e =~ /\.t\z/ && -f $p) { push @out, $p }
    }
  }
  return sort @out;
}

# ------------------------------------------------------------------ running --
sub load1 {
  open my $f, '<', '/proc/loadavg' or return '';
  return (split ' ', scalar <$f>)[0];
}

# Run CMD (a shell line) in its own session from DIR, with ENV overrides
# (undef deletes); returns (wait status, wall seconds, tree CPU seconds).
sub timed {
  my ($cmd, $dir, $env) = @_;
  local %ENV = %ENV;
  delete @ENV{qw(PCL_XSERVER PCL_XSERVER_OWNER PCL_XSERVER_TRIED PCL_SHOW_SBCL)};
  for my $k (keys %$env) {
    if (defined $env->{$k}) { $ENV{$k} = $env->{$k} } else { delete $ENV{$k} }
  }
  my $here = Cwd::getcwd();
  chdir $dir or die "board-timing.pl: cannot chdir to $dir: $!\n";
  my @t0 = times;
  my $w0 = Time::HiRes::time();
  my ($st) = PCLProc::run_isolated($cmd, runner => 'board-timing.pl');
  my $w1 = Time::HiRes::time();
  my @t1 = times;
  chdir $here;
  return ($st, $w1 - $w0, ($t1[2] - $t0[2]) + ($t1[3] - $t0[3]), $w0);
}

sub inc_flags {
  my ($w) = @_;
  my @inc = ("$w->{dist}/t/lib");
  push @inc, "$w->{dist}/lib" if !$w->{ndl};
  push @inc, map { abs_path($_) // $_ } @{ $w->{inc} };
  return join ' ', map { "-I$_" } grep { -d } @inc;
}

sub perl5lib {
  my ($w) = @_;
  $w->{p5lib} //= do {
    my ($pre) = PCLDistPrereq::built_prereq_blibs(dist => $w->{dist});
    join ':', PCLDistPrereq::perl5lib_dirs($pre), grep { defined && length } $ENV{PERL5LIB};
  };
  return length $w->{p5lib} ? $w->{p5lib} : $ENV{PERL5LIB};
}

# A cache directory must be private (the runtime refuses a group-writable one).
sub private_dir {
  my ($d) = @_;
  File::Path::make_path($d);
  chmod 0700, $d or die "board-timing.pl: chmod $d: $!\n";
}

sub slurp { my ($f) = @_; open my $h, '<', $f or return ''; local $/; my $s = <$h>; return $s // '' }

sub to_for { my ($w) = @_; return PCLTimeouts::timeout_for(\%file_to, $w->{rel}, $timeout) }

sub perl_run {
  my ($w, $rep) = @_;
  my $to = to_for($w);
  my $o = "$tmp/perl.out";
  my $load = load1();
  my $prec = "$tmp/perl.rec";
  unlink $prec;
  my $helper = $run_mode ? '-I' . perl_helper() . ' -MPCLBTRun ' : '';
  my ($st, $wall, $cpu) = timed("timeout -k 5 $to $^X $helper" . inc_flags($w) . " $w->{t} >$o 2>/dev/null </dev/null",
                                $w->{dist}, { PERL5LIB => perl5lib($w), PCL_BT_PERL_OUT => $prec, PCL_BT_PERL_LATE => undef });
  my $tap = PCLTap::parse_tap(slurp($o));
  my %p = (wall => $wall, cpu => $cpu, rc => $st >> 8, sig => $st & 127, tap => $tap, load => $load);
  return \%p if !$run_mode;
  my %q = slurp($prec) =~ /^(\w+)=(\S*)$/mg;
  if (defined $q{init_w}) {
    @p{qw(q_init q_wall q_cpu q_sub)} = (1, $q{end_w} - $q{init_w}, $q{end_c} - $q{init_c}, $q{end_k} - $q{init_k});
  } else {
    $p{q_init} = 0;    # perl left before its run phase (a skip in BEGIN)
  }
  if ($perl_c) {    # perl -c: the compile phase alone (the cross-check of the INIT/END clock)
    my ($cst, $cw) = timed("timeout -k 5 $to $^X -c " . inc_flags($w) . " $w->{t} >/dev/null 2>&1 </dev/null",
                           $w->{dist}, { PERL5LIB => perl5lib($w) });
    $p{q_c_wall} = $cw;
  }
  if ($perl_late && ($rep // 1) == 1) {   # once (rep 1) more with the require hook: perl's run-time `require`s, timed
    unlink $prec;
    timed("timeout -k 5 $to $^X $helper" . inc_flags($w) . " $w->{t} >/dev/null 2>&1 </dev/null",
          $w->{dist}, { PERL5LIB => perl5lib($w), PCL_BT_PERL_OUT => $prec, PCL_BT_PERL_LATE => 1 });
    my %l = slurp($prec) =~ /^(\w+)=(\S*)$/mg;
    @p{qw(q_late_wall q_late_cpu q_late_n)} = map { $l{$_} // '' } qw(late_w late_c late_n);
  }
  return \%p;
}

# perl's RUN PHASE clock: a module loaded with -M before the program compiles,
# so its INIT block is the FIRST to run (INIT is FIFO) and its END block the
# LAST (END is LIFO).  It removes its own directory from @INC; a forked child
# does not write.  With PCL_BT_PERL_LATE it also overrides CORE::GLOBAL::require
# from BEGIN on and times the run-time requires (outermost only) -- a separate
# run, because the override adds a frame and a call to every require.
my $perl_helper_dir;
sub perl_helper {
  return $perl_helper_dir if defined $perl_helper_dir;
  my $dir = $perl_helper_dir = "$tmp/perl-helper";
  File::Path::make_path($dir);
  open my $h, '>', "$dir/PCLBTRun.pm" or die "board-timing.pl: $dir/PCLBTRun.pm: $!\n";
  print $h <<'PERL';
package PCLBTRun;
use Time::HiRes ();
our ($pid, $live, $depth, $late_w, $late_c, $late_n, @init) = ($$, 0, 0, 0, 0, 0);
sub cpu { Time::HiRes::clock_gettime(Time::HiRes::CLOCK_PROCESS_CPUTIME_ID()) }
sub kids { my @t = times; $t[2] + $t[3] }
BEGIN {
  my ($me) = grep { !ref && -f "$_/PCLBTRun.pm" } @INC;
  @INC = grep { ref || $_ ne $me } @INC if defined $me;
  if ($ENV{PCL_BT_PERL_LATE}) {
    *CORE::GLOBAL::require = sub {
      my ($f) = @_;
      return CORE::require($f) if !$live || $depth;
      local $depth = 1;
      my ($w0, $c0) = (Time::HiRes::time(), cpu());
      my $r = eval { CORE::require($f) };
      my $e = $@;
      $late_w += Time::HiRes::time() - $w0;
      $late_c += cpu() - $c0;
      $late_n++;
      die $e if $e;
      return $r;
    };
  }
}
INIT { $live = 1; @init = (Time::HiRes::time(), cpu(), kids()) }
END {
  my $out = $ENV{PCL_BT_PERL_OUT};
  if ($$ == $pid && defined $out && @init) {
    local ($!, $@, $?);
    my @e = (Time::HiRes::time(), cpu(), kids());
    if (open my $fh, '>', $out) {
      printf $fh "init_w=%.6f\ninit_c=%.6f\ninit_k=%.2f\nend_w=%.6f\nend_c=%.6f\nend_k=%.2f\nlate_w=%.6f\nlate_c=%.6f\nlate_n=%d\n",
        @init, @e, $late_w, $late_c, $late_n;
      close $fh;
    }
  }
}
1;
PERL
  close $h;
  return $dir;
}

# One PCL run: (a) pl2cl of the test file, then (b + c) SBCL on its text.
sub pcl_run {
  my ($w, $mode, $cache, $rep) = @_;
  my $to = to_for($w);
  my ($lisp, $xerr, $sout, $serr, $rec) = map { "$tmp/pcl.$_" } qw(lisp xerr out err rec);
  unlink $rec;
  my %r = (mode => $mode, rep => $rep, w => $w, load => load1(), to => $to);
  # --xp-dir: the test file's transpile is kept (and reused) per file, so a
  # timing leg runs no pl2cl -- the run phase does not contain it anyway.
  my ($how, $reused) = (q{text}, 0);
  my ($xst, $xw, $xc) = (0, 0, 0);
  if (defined $xp_dir) {
    $lisp = "$xp_dir/$w->{base}/$w->{tname}.lisp";
    File::Path::make_path(File::Basename::dirname($lisp));
    $reused = 1 if -s $lisp;
  }
  if (!$reused) {
    ($xst, $xw, $xc) = timed("timeout -k 5 $to perl -I$root " . inc_flags($w)
                             . " $root/pl2cl --lenient-ppi $w->{t} >$lisp.tmp 2>$xerr </dev/null",
                             $w->{dist}, { PCL_CACHE_DIR => $cache, %xenv });
    if ($xst == 0) { rename "$lisp.tmp", $lisp or die "board-timing.pl: rename $lisp: $!\n" } else { unlink "$lisp.tmp" }
  }
  @r{qw(x_wall x_cpu lisp_kb)} = ($xw, $xc, int((-s $lisp // 0) / 1024));
  if ($xst != 0) {
    @r{qw(class rc ok notok plan)} = ($xst >> 8 == 124 ? 'TIMEOUT' : 'XFAIL', $xst >> 8, 0, 0, '');
    return \%r;
  }
  my $left = $to - $xw;
  $left = 10 if $left < 10;
  my $loads = ($hooks_fasl ? "--load $hooks_fasl " : '') . join('', map { "--load $_ " } @preload);
  my %env = (PCL_CACHE_DIR => $cache, PERL5LIB => perl5lib($w), PCL_BT_OUT => $rec,
             PCL_BT_PROF => undef, PCL_BT_PROF_OUT => undef, PCL_BT_PROF_FROM => undef);
  @env{keys %xenv} = values %xenv;
  # --fasl: the program's text compiled to a fasl FIRST, and the run loads the
  # fasl: nothing of the program is read or compiled inside the run.  The
  # build is `pcl`'s own first run (task #2702, docs/first-run-from-text-design
  # -s510.md): a separate SBCL runs the program from its text, output thrown
  # away, and builds the fasl in that image after the END phase
  # (pcl::*p-exit-build*, the script cache's deferred build) -- so every module
  # body and BEGIN block runs once there, as in any run.  The timed run is then
  # `pcl`'s repeated run of an unchanged script.  A build that fails leaves
  # the run on the text (run_how = text).
  if ($fasl_mode) {
    (my $fasl = $lisp) =~ s/\.lisp\z/.fasl/;
    $fasl = "$tmp/pcl.fasl" if $fasl eq $lisp;
    unlink $fasl if !defined $xp_dir;
    if (!-s $fasl) {
      timed("timeout -k 5 $left $sbcl --eval '(setf pcl::*p-exit-build* (list (sb-posix:getpid) \"$lisp\" \"$fasl\"))' --load $lisp >/dev/null 2>&1 </dev/null",
            $w->{dist}, { %env, PCL_BT_OUT => undef });
    }
    if (-s $fasl) { $lisp = $fasl; $how = 'fasl' }
  }
  $r{run_how} = $how . ($reused ? q{+xp} : q{});
  if ($prof && $mode ne 'warmup') {
    File::Path::make_path($prof_dir);
    (my $stem = $w->{rel}) =~ s{/}{__}g;
    @env{qw(PCL_BT_PROF PCL_BT_PROF_OUT)} = ($prof, abs_path($prof_dir) . "/$stem.$prof" . ($prof_from eq q{start} ? q{.start} : q{}) . ".prof");
    $env{PCL_BT_PROF_FROM} = $prof_from;
  }
  my ($st, $sw, $sc, $spawn) = timed("timeout -k 5 $left $sbcl $loads--load $lisp >$sout 2>$serr </dev/null",
                                     $w->{dist}, \%env);
  my $end = $spawn + $sw;
  my $raw = slurp($sout);
  $raw =~ s/^;[^\n]*\n//gm;
  $raw =~ s/^PCL (?:Runtime|Test)[^\n]*\n//gm;
  $raw =~ s/^STYLE-WARNING[^\n]*\n//gm;
  my $tap = PCLTap::parse_tap($raw);
  my $rc = $st >> 8;
  @r{qw(s_wall s_cpu rc sig taps raw)} = ($sw, $sc, $rc, $st & 127, $tap, $raw);
  @r{qw(ok notok plan skip)} = ($tap->{ok}, $tap->{notok}, $tap->{plan} // '', $tap->{skip_all});
  my %h = slurp($rec) =~ /^(\w+)=(\S*)$/mg;
  $r{h} = \%h;
  $r{rec} = $h{phase} // 'none';
  if (%h) {
    my $us = sub { ($h{$_[0]} // 0) / 1e6 };
    $r{start} = $h{t0_us} / 1e6 - $spawn;
    $r{s_self} = $us->('exit_cpu_us');
    $r{end_wall} = $end - $h{exit_us} / 1e6;
    $r{boundary} = $h{boundary_us} ? ($h{boundary_us} - $h{t0_us}) / 1e6 : '';
    $r{mb_consed} = sprintf '%.0f', ($h{bytes} // 0) / 1e6;
    $r{gc} = $us->('gc_us');
    $r{$_} = $us->("${_}_wall_us") for qw(modload tap tcompile mcompile scompile ecompile cfile xpile evalx xserver evalrun sread);
    $r{"${_}_c"} = $us->("${_}_cpu_us") for qw(modload tap tcompile mcompile scompile ecompile cfile evalrun sread);
    @r{qw(n_mod n_fasl n_text n_eval n_xpile)} = map { $h{$_} // 0 } qw(modload_n faslload_n textload_n evalrun_n xpile_n);
    run_phase(\%r, \%h) if $run_mode;
    my $abuck = $r{xpile} + $r{evalx} + $r{xserver};
    my @bw = qw(modload tap tcompile mcompile scompile ecompile cfile sread);
    my $bbuck = 0; $bbuck += $r{$_} for @bw;
    my $bcpu = $us->('t0_cpu_us'); $bcpu += $r{"${_}_c"} for @bw;
    # SBCL's own run-time compiles (rcompile: CLOS constructors / dispatch)
    # before the boundary are load (B); after it, run (C)
    my $rpre = $h{boundary_us} ? 'pre_rcompile' : 'rcompile';
    $bbuck += $us->("${rpre}_wall_us");
    $bcpu += $us->("${rpre}_cpu_us");
    $r{A_wall} = $xw + $abuck;
    $r{B_wall} = $r{start} + $bbuck;
    $r{C_wall} = $sw - $r{start} - $abuck - $bbuck;
    # SBCL's CHILDREN: the module transpiles (pl2cl, SBCL blocked meanwhile,
    # so their CPU is about their wall) belong to A; the rest -- the
    # subprocesses the PROGRAM starts (system, `$^X ...`, pipes) -- to C.
    my $child = $sc - $r{s_self};
    $child = 0 if $child < 0;
    my $xchild = $abuck < $child ? $abuck : $child;
    $r{A_cpu} = $xc + $xchild;
    $r{B_cpu} = $bcpu;
    $r{C_cpu} = $r{s_self} - $bcpu - $us->('xpile_cpu_us') - $us->('evalx_cpu_us') - $us->('xserver_cpu_us')
               + ($child - $xchild);
    $r{cpu_v} = 2;
  }
  $r{class} = classify($tap, $rc, $r{sig});
  return \%r;
}

# THE RUN PHASE of one PCL run (--run), from the hooks' record: the window from
# the boundary (perl's INIT: after the main program's CHECK blocks) to the last
# exit hook (after the END phase), and what inside it is NOT run:
#   r_lw   the LOAD work the window still holds on the text path -- reading and
#          compiling the program's own forms after the boundary (sread,
#          scompile; zero with --fasl).  Subtracted: perl did it before INIT.
#   r_late a LATE load / compile: a module first required at run time, a string
#          eval's transpile and compile (modload tap tcompile mcompile ecompile
#          cfile xpile evalx xserver).  It IS run time -- perl pays its own --
#          so it stays IN the run number and is reported beside it.  Its CPU
#          counts the pl2cl children reaped inside xpile and the WALL of evalx /
#          xserver (the transpile server's work: a child nobody reaps until exit).
#   r_sub  the children the PROGRAM starts (reaped in the window, minus the
#          late-transpile children): listed, never inside the run number.
#   pr_cpu = r_cpu (SBCL's own CPU in the window) - r_lw_cpu + the late
#            transpile's outside CPU;  pr_wall = r_wall - r_lw_wall (includes
#            the waits on children).
sub run_phase {
  my ($r, $h) = @_;
  return if !$h->{boundary_us};
  my $us = sub { ($h->{$_[0]} // 0) / 1e6 };
  my $post = sub { my ($b, $k) = @_; my $x = $k eq q{bytes} ? $k : "${k}_us"; $us->("${b}_$x") - $us->("pre_${b}_$x") };
  my $sum = sub { my ($k, @b) = @_; my $s = 0; $s += $post->($_, $k) for @b; $s };
  my @lw = qw(sread scompile);
  my @late = qw(modload tap tcompile mcompile ecompile cfile xpile evalx xserver);
  $r->{r_wall} = ($h->{exit_us} - $h->{boundary_us}) / 1e6;
  $r->{r_cpu} = ($h->{exit_cpu_us} - $h->{boundary_cpu_us}) / 1e6;
  $r->{r_lw_wall} = $sum->('wall', @lw);
  $r->{r_lw_cpu} = $sum->('cpu', @lw);
  my $tchild = $sum->('child', qw(xpile evalx xserver));
  my $outside = $tchild + $post->('evalx', 'wall') + $post->('xserver', 'wall');
  $r->{r_late_wall} = $sum->('wall', @late);
  $r->{r_late_cpu} = $sum->('cpu', grep { !/^(evalx|xserver)$/ } @late) + $outside;
  # of it: the STRING EVALS (their transpile, the server start, their compile) -- the rest is late requires
  $r->{r_eval_cpu} = $post->(q{ecompile}, q{cpu}) + $post->(q{evalx}, q{wall}) + $post->(q{xserver}, q{wall}) + $post->(q{evalx}, q{child}) + $post->(q{xserver}, q{child});
  $r->{r_xsrv} = $post->(q{xserver}, q{wall});
  # a module TRANSPILED or fasl-BUILT inside the run: the cache had not converged (a warm row should read 0)
  $r->{r_build} = $post->(q{xpile}, q{wall}) + $post->(q{cfile}, q{wall});
  $r->{r_late_n} = sprintf '%d/%d', map { ($h->{"${_}_n"} // 0) - ($h->{"pre_${_}_n"} // 0) } qw(modload evalrun);
  my $sub = ($h->{exit_child_us} - $h->{boundary_child_us}) / 1e6 - $tchild;
  $r->{r_sub} = $sub < 0 ? 0 : $sub;
  $r->{r_rcomp} = $post->('rcompile', 'cpu');
  $r->{r_rcomp_n} = ($h->{rcompile_n} // 0) - ($h->{pre_rcompile_n} // 0);
  # GC and allocation of the RUN: the window's, minus what the load work inside it did
  $r->{r_gc} = $us->('gc_us') - $us->('boundary_gc_us') - $sum->('gc', @lw);
  $r->{r_mb} = sprintf '%.1f', (($h->{bytes} // 0) - ($h->{boundary_bytes} // 0) - 1e6 * $sum->('bytes', @lw)) / 1e6;
  $r->{pr_cpu} = $r->{r_cpu} - $r->{r_lw_cpu} + $outside;
  $r->{pr_wall} = $r->{r_wall} - $r->{r_lw_wall};
}

sub classify {
  my ($tap, $rc, $sig) = @_;
  return 'TIMEOUT' if $rc == 124 || $sig;
  my $ran = $tap->{ok} + $tap->{notok};
  return 'SKIP' if $ran == 0 && defined $tap->{skip_all};
  return 'FAIL' if $tap->{ok} == 0;
  my $short = !defined $tap->{plan} || $ran != $tap->{plan};
  my $verdict = PCLTap::exit_is_verdict('', $rc, $ran, $tap->{plan}, $tap->{failed});
  return ($tap->{notok} || $short || ($rc && !$verdict)) ? 'PARTIAL' : 'PASS';
}

sub compare_class {
  my ($c, $p) = @_;
  return 'X' if !$p;
  my ($pt, $ct) = ($p->{tap}, $c->{taps} // { ok => 0, notok => 0 });
  my $pran = $pt->{ok} + $pt->{notok};
  my $pplan = defined $pt->{plan} && $pran == $pt->{plan} && !$p->{sig} && $p->{rc} != 124;
  return 'S' if ($c->{class} // '') eq 'SKIP' && $pran == 0 && defined $pt->{skip_all};
  return 'C' if ($c->{class} // '') eq 'PASS' && $pplan && !$pt->{notok} && $ct->{ok} == $pt->{ok};
  return 'PE' if $pplan && $ct->{ok} == $pt->{ok} && $ct->{ok} + $ct->{notok} == $pran
                 && ($c->{class} // '') ne 'TIMEOUT';
  return 'X';
}

sub emit {
  my ($r, $p) = @_;
  my $w = $r->{w};
  my $f = sub { my $v = shift; return '' if !defined $v; return $v =~ /^-?\d+\.\d+(?:e-?\d+)?$/i ? sprintf('%.3f', $v) : $v };
  my %row = (sha => $sha, mode => $r->{mode}, rep => $r->{rep}, variant => $variant, dist => $w->{base}, file => $w->{tname},
             load1 => $r->{load}, timeout => $r->{to},
             c_class => $r->{class}, cmp => compare_class($r, $p), c_rc => $r->{rc},
             c_ok => $r->{ok}, c_notok => $r->{notok}, c_plan => $r->{plan},
             c_wall => ($r->{x_wall} // 0) + ($r->{s_wall} // 0), c_cpu => ($r->{x_cpu} // 0) + ($r->{s_cpu} // 0),
             map { $_ => $r->{$_} } grep { exists $r->{$_} } @COLS);
  if ($p) {
    @row{qw(p_wall p_cpu p_rc p_ok p_notok p_plan p_skip)} =
      ($p->{wall}, $p->{cpu}, $p->{rc}, $p->{tap}{ok}, $p->{tap}{notok}, $p->{tap}{plan} // '',
       defined $p->{tap}{skip_all} ? 1 : 0);
    $row{$_} = $p->{$_} for grep { /^q_/ && defined $p->{$_} } keys %$p;
  }
  # run-phase seconds carry 0.1 ms (a perl run phase can be a few ms)
  my $g = sub { my $v = shift; return '' if !defined $v; return $v =~ /^-?\d+(?:\.\d+)?(?:e-?\d+)?$/i ? sprintf('%.5f', $v) : $v };
  print $ofh join("\t", map { /^(?:q_(?!init|late_n)|r_(?!late_n|mb|rcomp_n)|pr_|sread)/ ? $g->($row{$_}) : $f->($row{$_}) } @COLS), "\n";
}

# ------------------------------------------------------------------ report --
# Rows by (dist, file, mode): the LAST input file that has the key supplies
# its reps; the rep with the least PCL CPU is the row, with the reps' spread.
sub read_rows {
  my ($files) = @_;
  my ($reps, $taken) = read_reps($files);
  my %reps = %$reps;
  my %row;
  for my $k (keys %reps) {
    my @r = sort { num($a->{c_cpu}) <=> num($b->{c_cpu}) } @{ $reps{$k} };
    my %best = %{ $r[0] };
    my ($lo, $hi) = (num($r[0]{c_cpu}), num($r[-1]{c_cpu}));
    $best{spread} = @r > 1 && $lo > 0 ? ($hi - $lo) / $lo : '';
    $best{nrep} = scalar @r;
    my ($plo) = sort { $a <=> $b } map { num($_->{p_cpu}) } @r;
    $best{p_cpu} = $plo if @r > 1;
    my ($pw) = sort { $a <=> $b } map { num($_->{p_wall}) } @r;
    my ($cw) = sort { $a <=> $b } map { num($_->{c_wall}) } @r;
    @best{qw(p_wall c_wall)} = ($pw, $cw) if @r > 1;
    $row{$k} = \%best;
  }
  return (\%row, $taken);
}

# Every rep by (dist, file, mode, variant); the LAST input file that has the key supplies them.
sub read_reps {
  my ($files) = @_;
  my (%reps, %src, @taken);
  for my $f (@$files) {
    open my $h, '<', $f or die "board-timing.pl: cannot read $f: $!\n";
    my @cols;
    while (my $line = <$h>) {
      chomp $line;
      if ($line =~ /^# taken-at: (.*)/) { push @taken, basename($f) . ": $1"; next }
      next if $line =~ /^#/ || !length $line;
      my @v = split /\t/, $line, -1;
      if ($v[0] eq 'sha') { @cols = @v; next }
      my %r;
      @r{@cols} = @v;
      next if $r{mode} eq 'warmup';
      if (!$r{cpu_v} && length($r{s_self} // '')) {
        # rows of the first version charged ALL of SBCL's child CPU to A
        my $child = num($r{s_cpu}) - num($r{s_self});
        $child = 0 if $child < 0;
        my $ab = num($r{xpile}) + num($r{evalx}) + num($r{xserver});
        my $xchild = $ab < $child ? $ab : $child;
        $r{A_cpu} = num($r{x_cpu}) + $xchild;
        $r{C_cpu} = num($r{C_cpu}) + $child - $xchild;
      }
      # a variant named `precise...` RE-TIMES the base (a quiet bench): its rows
      # replace the base rows; any other variant is its own population.
      my $v = $r{variant} // q{};
      $v = q{} if $v =~ /^precise/;
      my $k = "$r{dist}\t$r{file}\t$r{mode}\t$v";
      if (($src{$k} // '') ne $f) { $reps{$k} = []; $src{$k} = $f }
      $r{src} = basename($f);
      push @{ $reps{$k} }, \%r;
    }
  }
  return (\%reps, \@taken);
}

sub num { my ($v) = @_; return (defined $v && length $v) ? $v : 0 }

# The run phase's subprocess CPU: SBCL's children beyond the transpile share
# (already scaled by the row's load factor f when --norm ran).
sub subproc {
  my ($r) = @_;
  my $child = num($r->{s_cpu}) - num($r->{s_self});
  $child = 0 if $child < 0;
  my $ab = num($r->{xpile}) + num($r->{evalx}) + num($r->{xserver});
  my $x = $child - ($ab < $child ? $ab : $child);
  return $x;
}
sub s2 { my ($v) = @_; return sprintf '%.1f', num($v) }
sub f2 { my ($v) = @_; return sprintf q{%.2f}, num($v) }
sub ratio { my ($a, $b) = @_; return num($b) > 0 ? sprintf('%.1fx', num($a) / num($b)) : '-' }

# LOAD NORMALISATION (--norm): CPU seconds inflate with the box's load (turbo
# and shared cores: the same compile is 0.385 s at load 1 and ~1 s at load 13,
# measured s540a).  Every Test::More file compiles the SAME TAP layer, so its
# `tcompile` CPU is a fixed workload in the row itself: f = tcompile / the
# quiet median (rows at load < 2.5), clamped to [1, 4]; a row without it takes
# the median f of its load bucket.  Every CPU field of the row is divided by f.
sub normalise {
  my ($rows) = @_;
  my @q = sort { $a <=> $b } map { num($_->{tcompile_c}) } grep { num($_->{tcompile_c}) > 0 && num($_->{load1}) < 2.5 } @$rows;
  my $tq = @q >= 5 ? $q[$#q / 2] : 0.385;
  my %bucket;
  for my $r (@$rows) {
    next unless num($r->{tcompile_c}) > 0;
    my $f = num($r->{tcompile_c}) / $tq;
    $r->{f} = $f < 1 ? 1 : $f > 4 ? 4 : $f;
    push @{ $bucket{int(num($r->{load1}) / 2)} }, $r->{f};
  }
  my %bmed = map { my @s = sort { $a <=> $b } @{ $bucket{$_} }; ($_ => $s[$#s / 2]) } keys %bucket;
  for my $r (@$rows) {
    $r->{f} //= $bmed{int(num($r->{load1}) / 2)} // 1;
    for my $k (qw(p_cpu c_cpu x_cpu s_cpu s_self A_cpu B_cpu C_cpu gc), map { "${_}_c" } qw(modload tap tcompile mcompile scompile ecompile cfile evalrun)) {
      $r->{$k} = num($r->{$k}) / $r->{f} if defined $r->{$k} && length $r->{$k};
    }
  }
  return $tq;
}

sub report {
  my ($files, $topn) = @_;
  my ($rows, $taken) = read_rows($files);
  if ($norm) {
    my $tq = normalise([values %$rows]);
    push @$taken, sprintf('CPU LOAD-NORMALISED (--norm): quiet TAP-layer compile %.3f s', $tq);
  }
  my @warm = grep { $_->{mode} eq 'warm' } values %$rows;
  my %cold = map { ("$_->{dist}\t$_->{file}" => $_) } grep { $_->{mode} eq 'cold' } values %$rows;
  my @cmp = grep { $_->{cmp} eq 'C' } @warm;
  my @B = qw(modload tap tcompile mcompile scompile ecompile cfile xpile evalx xserver evalrun);
  my $o = "# board-timing report\n\n" . join('', map { "- $_\n" } @$taken) . "\n";
  my %n;
  $n{$_->{cmp}}++ for @warm;
  $o .= sprintf "Warm rows: %d files; compared (C) %d, partial-equal (PE) %d, both skip (S) %d, excluded (X) %d.\n\n",
    scalar @warm, map { $n{$_} // 0 } qw(C PE S X);
  # totals
  my %t;
  for my $r (@cmp) {
    $t{$_} += num($r->{$_}) for qw(p_wall p_cpu c_wall c_cpu A_wall B_wall C_wall A_cpu B_cpu C_cpu gc mb_consed), @B, map { "${_}_c" } @B;
    my $c = $cold{"$r->{dist}\t$r->{file}"};
    if ($c) { $t{cold_cpu} += num($c->{c_cpu}); $t{cold_wall} += num($c->{c_wall}); $t{cold_n}++ }
  }
  $o .= "## Totals over the compared files (seconds)\n\n";
  $o .= "| | wall | CPU |\n|---|---:|---:|\n";
  $o .= sprintf "| perl | %s | %s |\n| PCL warm | %s | %s |\n| PCL / perl | %s | %s |\n", s2($t{p_wall}), s2($t{p_cpu}),
    s2($t{c_wall}), s2($t{c_cpu}), ratio($t{c_wall}, $t{p_wall}), ratio($t{c_cpu}, $t{p_cpu});
  $o .= sprintf "| A transpile | %s | %s |\n| B load | %s | %s |\n| C run | %s | %s |\n",
    map { s2($t{$_}) } qw(A_wall A_cpu B_wall B_cpu C_wall C_cpu);
  $o .= sprintf "| PCL cold (%d files) | %s | %s |\n", $t{cold_n} // 0, s2($t{cold_wall}), s2($t{cold_cpu});
  $o .= sprintf "| GC (in C and B) | | %s |\n\nMB consed: %.0f\n\n", s2($t{gc}), num($t{mb_consed});
  $o .= "Buckets (wall / SBCL CPU, exclusive): " . join(', ', map { sprintf '%s %s/%s', $_, s2($t{$_}), s2($t{"${_}_c"}) } @B) . "\n\n";
  # WHERE PCL'S CPU GOES over the compared files: one line per component,
  # with the files and dists it is more than 0.05 s in.
  my @comp = (
    ['test file pl2cl (A)',         sub { num($_[0]{x_cpu}) }],
    ['module transpiles (A)',       sub { num($_[0]{A_cpu}) - num($_[0]{x_cpu}) }],
    ['SBCL start + hooks (B)', sub { my $r = shift; my $b = num($r->{B_cpu}); $b -= num($r->{"${_}_c"}) for qw(modload tap tcompile mcompile scompile ecompile cfile); $b }],
    ['module loads, fasl/text run (B: modload)', sub { num($_[0]{modload_c}) + num($_[0]{tap_c}) }],
    ['TAP layer compile (B: tcompile)',  sub { num($_[0]{tcompile_c}) }],
    ['module text compile (B: mcompile)', sub { num($_[0]{mcompile_c}) }],
    ['test file forms compile (B: scompile)', sub { num($_[0]{scompile_c}) }],
    ['string-eval compile (B: ecompile)', sub { num($_[0]{ecompile_c}) }],
    ['module fasl build (B: cfile)', sub { num($_[0]{cfile_c}) }],
    ['run: SBCL itself (C)',        sub { num($_[0]{C_cpu}) - subproc($_[0]) }],
    ['run: subprocesses (C)',       sub { subproc($_[0]) }],
    ['perl, the whole file',        sub { num($_[0]{p_cpu}) }],
  );
  $o .= "## Where PCL's CPU goes (compared files)\n\n| component | CPU s | files > 0.05 s | dists |\n|---|---:|---:|---:|\n";
  for my $c (@comp) {
    my ($s, $nf, %nd) = (0, 0);
    for my $r (@cmp) {
      my $v = $c->[1]->($r);
      $s += $v;
      if ($v > 0.05) { $nf++; $nd{$r->{dist}}++ }
    }
    $o .= sprintf "| %s | %.1f | %d | %d |\n", $c->[0], $s, $nf, scalar keys %nd;
  }
  $o .= "\n";
  # per dist
  my %d;
  for my $r (@warm) {
    my $x = $d{$r->{dist}} //= {};
    $x->{$r->{cmp}}++;
    next if $r->{cmp} ne 'C';
    $x->{$_} += num($r->{$_}) for qw(p_cpu c_cpu A_cpu B_cpu C_cpu p_wall c_wall tcompile_c);
    my $c = $cold{"$r->{dist}\t$r->{file}"};
    $x->{cold} += num($c->{c_cpu}) if $c;
  }
  $o .= "## Per distribution (compared files; CPU seconds)\n\n";
  $o .= "| dist | C | PE | S | X | perl | PCL | excess | ratio | A | B | (TAP) | C run | cold PCL |\n";
  $o .= "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|\n";
  for my $dn (sort { num($d{$b}{c_cpu}) - num($d{$b}{p_cpu}) <=> num($d{$a}{c_cpu}) - num($d{$a}{p_cpu}) } keys %d) {
    my $x = $d{$dn};
    $o .= sprintf "| %s | %d | %d | %d | %d | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n", $dn,
      (map { $x->{$_} // 0 } qw(C PE S X)), s2($x->{p_cpu}), s2($x->{c_cpu}),
      s2(num($x->{c_cpu}) - num($x->{p_cpu})), ratio($x->{c_cpu}, $x->{p_cpu}),
      map { s2($x->{$_}) } qw(A_cpu B_cpu tcompile_c C_cpu cold);
  }
  my $line = sub {
    my ($r) = @_;
    my ($tb) = sort { num($r->{"${b}_c"} // $r->{$b}) <=> num($r->{"${a}_c"} // $r->{$a}) } qw(tcompile scompile mcompile ecompile modload evalx);
    return sprintf "| %s/%s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n", $r->{dist}, $r->{file},
      f2($r->{p_cpu}), f2($r->{c_cpu}), f2(num($r->{c_cpu}) - num($r->{p_cpu})), ratio($r->{c_cpu}, $r->{p_cpu}),
      f2($r->{A_cpu}), f2($r->{B_cpu}), f2($r->{C_cpu}), $tb,
      ($r->{nrep} // 1) > 1 ? sprintf('%.0f%%', 100 * num($r->{spread})) : '-', $r->{src};
  };
  my $hdr = "| file | perl | PCL | excess | ratio | A | B | C | top B/A bucket | spread | from |\n|---|---:|---:|---:|---:|---:|---:|---:|---|---:|---|\n";
  my @by_ex = sort { num($b->{c_cpu}) - num($b->{p_cpu}) <=> num($a->{c_cpu}) - num($a->{p_cpu}) } @cmp;
  $o .= "\n## Top $topn by excess CPU (PCL - perl)\n\n$hdr" . join('', map { $line->($_) } grep { defined } @by_ex[0 .. $topn - 1]);
  my @by_run = sort { num($b->{C_cpu}) - num($b->{p_cpu}) <=> num($a->{C_cpu}) - num($a->{p_cpu}) } @cmp;
  my $tr = $topn < 25 ? $topn : 25;
  $o .= "\n## Top $tr by RUN-phase excess (C - perl)\n\n$hdr" . join('', map { $line->($_) } grep { defined } @by_run[0 .. $tr - 1]);
  # perl's own time includes its start and its Test::More load (~0.05 s), so
  # only a file perl spends real time in can be judged: >= 0.25 s.
  my @fast = grep { num($_->{p_cpu}) >= 0.25 && num($_->{C_cpu}) * 2 < num($_->{p_cpu}) - 0.05 } @cmp;
  $o .= "\n## Run phase faster than perl by more than 2x (possible silent skips; perl >= 0.25 s)\n\n$hdr" . join('', map { $line->($_) } sort { num($b->{p_cpu}) <=> num($a->{p_cpu}) } @fast);
  my @pe = grep { $_->{cmp} eq 'PE' } @warm;
  $o .= "\n## Partial-equal (listed, not compared)\n\n$hdr" . join('', map { $line->($_) } sort { $a->{dist} cmp $b->{dist} || $a->{file} cmp $b->{file} } @pe);
  my %s;
  for my $r (grep { $_->{cmp} eq 'S' } @warm) { $s{n}++; $s{$_} += num($r->{$_}) for qw(p_cpu c_cpu A_cpu B_cpu C_cpu) }
  $o .= sprintf "\nBoth-skip files: %d, perl %s s, PCL %s s CPU (A %s, B %s, C %s) -- pure overhead.\n",
    $s{n} // 0, map { s2($s{$_}) } qw(p_cpu c_cpu A_cpu B_cpu C_cpu);
  my %x;
  $x{$_->{c_class}}++ for grep { $_->{cmp} eq 'X' } @warm;
  $o .= "Excluded by PCL class: " . join(', ', map { "$_ $x{$_}" } sort keys %x) . "\n";
  write_table($table, \@warm, \%cold, $taken) if defined $table;
  return $o;
}

# ------------------------------------------------------------ report --run --
# THE RUN PHASE, per file over its warm reps (s541a): PCL's run = the least
# pr_cpu of its COMPARED reps, perl's = the least q_cpu of all reps, each with
# its spread ((max - min) / min); load = the highest 1-minute load any rep
# started at.  A file is compared when its best PCL rep is class C.
sub run_rows {
  my ($files) = @_;
  my ($reps, $taken) = read_reps($files);
  my @rows;
  for my $k (sort keys %$reps) {
    my @all = grep { $_->{mode} eq 'warm' } @{ $reps->{$k} };
    next if !@all;
    my @c = grep { $_->{cmp} eq 'C' && length($_->{pr_cpu} // '') } @all;
    my %r = (dist => $all[0]{dist}, file => $all[0]{file}, nrep => scalar @all, ncmp => scalar @c,
             src => $all[0]{src}, cmp => @c ? 'C' : $all[0]{cmp}, c_class => $all[0]{c_class});
    my @q = sort { $a <=> $b } map { num($_->{q_cpu}) } grep { $_->{q_init} } @all;
    my ($load) = sort { $b <=> $a } map { num($_->{load1}) } @all;
    $r{load} = $load;
    $r{q_init} = @q ? 1 : 0;
    if (@q) {
      $r{q_cpu} = $q[0];
      $r{q_spread} = $q[0] > 0 ? ($q[-1] - $q[0]) / $q[0] : 0;
      my ($qb) = sort { num($a->{q_cpu}) <=> num($b->{q_cpu}) } grep { $_->{q_init} } @all;
      $r{$_} = $qb->{$_} for qw(q_wall q_sub p_ok);
      my ($ql) = grep { length($_->{q_late_cpu} // '') } @all;
      @r{qw(q_late_cpu q_late_n)} = $ql ? ($ql->{q_late_cpu}, $ql->{q_late_n}) : ('', '');
      my ($qc) = sort { $a <=> $b } map { num($_->{q_c_wall}) } grep { length($_->{q_c_wall} // '') } @all;
      my ($pw) = sort { $a <=> $b } map { num($_->{p_wall}) } @all;
      @r{qw(q_c_wall p_wall)} = ($qc // '', $pw);
    }
    if (@c) {
      my @s = sort { num($a->{pr_cpu}) <=> num($b->{pr_cpu}) } @c;
      my $b = $s[0];
      $r{pr_cpu} = num($b->{pr_cpu});
      $r{pr_spread} = $r{pr_cpu} > 0 ? (num($s[-1]{pr_cpu}) - $r{pr_cpu}) / $r{pr_cpu} : 0;
      $r{$_} = $b->{$_} for qw(pr_wall r_cpu r_lw_cpu r_late_cpu r_late_n r_rcomp r_rcomp_n r_eval_cpu r_xsrv r_build r_sub r_gc r_mb run_how c_ok);
    }
    push @rows, \%r;
  }
  return (\@rows, $taken);
}

sub ms { my ($v) = @_; return length($v // '') ? sprintf('%.1f', 1000 * $v) : '' }

sub report_run {
  my ($files, $topn) = @_;
  my ($rows, $taken) = run_rows($files);
  my @cmp = grep { $_->{cmp} eq 'C' && $_->{q_init} } @$rows;
  my $o = "# board-timing run-phase report\n\n" . join('', map { "- $_\n" } @$taken) . "\n";
  my %n;
  $n{$_->{cmp}}++ for @$rows;
  my ($maxload) = sort { $b <=> $a } map { $_->{load} } @cmp;
  $o .= sprintf "Files: %d; compared (C, perl reached INIT) %d; PE %d, S %d, X %d.  Highest start load of a compared rep: %s.\n\n",
    scalar @$rows, scalar @cmp, map({ $n{$_} // 0 } qw(PE S X)), $maxload // '-';
  my %t;
  for my $r (@cmp) {
    $t{$_} += num($r->{$_}) for qw(q_cpu pr_cpu q_wall pr_wall q_sub r_sub q_late_cpu r_late_cpu r_rcomp r_eval_cpu r_xsrv r_build r_gc r_mb p_wall);
  }
  $o .= "## Totals over the compared files (seconds)\n\n| | perl | PCL | PCL - perl | PCL / perl |\n|---|---:|---:|---:|---:|\n";
  $o .= sprintf "| run phase, CPU | %s | %s | %s | %s |\n", f2($t{q_cpu}), f2($t{pr_cpu}), f2($t{pr_cpu} - $t{q_cpu}), ratio($t{pr_cpu}, $t{q_cpu});
  $o .= sprintf "| run phase, wall (waits on children inside) | %s | %s | %s | %s |\n", f2($t{q_wall}), f2($t{pr_wall}), f2($t{pr_wall} - $t{q_wall}), ratio($t{pr_wall}, $t{q_wall});
  $o .= sprintf "| of the run: late load / compile | %s | %s | | |\n", f2($t{q_late_cpu}), f2($t{r_late_cpu});
  $o .= sprintf "| of the run: SBCL run-time compiles (CLOS) | - | %s | | |\n", f2($t{r_rcomp});
  $o .= sprintf "| of the late: string evals (transpile + server start + compile) | - | %s | | |\n", f2($t{r_eval_cpu});
  $o .= sprintf "| of those: the transpile server's start | - | %s | | |\n", f2($t{r_xsrv});
  $o .= sprintf "| of the late: a module transpiled or fasl-built INSIDE the run (cache not converged: %d files) | - | %s | | |\n", scalar(grep { num($_->{r_build}) > 0.01 } @cmp), f2($t{r_build});
  $o .= sprintf "| of the run: GC | - | %s | | |\n", f2($t{r_gc});
  $o .= sprintf "| subprocesses (NOT in the run) | %s | %s | | |\n", f2($t{q_sub}), f2($t{r_sub});
  $o .= sprintf "| perl's whole file, wall | %s | | | |\n\nMB consed in PCL's run phase: %.0f\n\n", f2($t{p_wall}), $t{r_mb};
  # the distribution of ratios
  my @b = ([q{under 1x}, 0, 1], [q{1-2x}, 1, 2], [q{2-5x}, 2, 5], [q{5-10x}, 5, 10], [q{over 10x}, 10, 1e99]);
  $o .= "## Ratio distribution (PCL run / perl run)\n\n| ratio | files | perl s | PCL s | PCL - perl |\n|---|---:|---:|---:|---:|\n";
  for my $bk (@b) {
    my @in = grep { my $x = $_->{q_cpu} > 0 ? $_->{pr_cpu} / $_->{q_cpu} : 1e99; $x >= $bk->[1] && $x < $bk->[2] } @cmp;
    my ($p, $c) = (0, 0); for (@in) { $p += $_->{q_cpu}; $c += $_->{pr_cpu} }
    $o .= sprintf "| %s | %d | %s | %s | %s |\n", $bk->[0], scalar @in, f2($p), f2($c), f2($c - $p);
  }
  # per dist
  my %d;
  for my $r (@$rows) {
    my $x = $d{$r->{dist}} //= {};
    $x->{n}{$r->{cmp}}++;
    next if !($r->{cmp} eq 'C' && $r->{q_init});
    $x->{$_} += num($r->{$_}) for qw(q_cpu pr_cpu q_late_cpu r_late_cpu r_rcomp q_sub r_sub r_gc);
    my $ex = $r->{pr_cpu} - $r->{q_cpu};
    if (!defined $x->{top} || $ex > $x->{topx}) { $x->{top} = $r->{file}; $x->{topx} = $ex }
  }
  $o .= "\n## Per distribution (compared files; CPU seconds of the run phase)\n\n";
  $o .= "| dist | C | other | perl | PCL | PCL - perl | ratio | late perl / PCL | CLOS compiles | GC | subproc perl / PCL | largest excess |\n";
  $o .= "|---|---:|---:|---:|---:|---:|---:|---|---:|---:|---|---|\n";
  for my $dn (sort { num($d{$b}{pr_cpu}) - num($d{$b}{q_cpu}) <=> num($d{$a}{pr_cpu}) - num($d{$a}{q_cpu}) } keys %d) {
    my $x = $d{$dn};
    my $other = 0; $other += $x->{n}{$_} for grep { $_ ne 'C' } keys %{ $x->{n} };
    $o .= sprintf "| %s | %d | %d | %s | %s | %s | %s | %s / %s | %s | %s | %s / %s | %s |\n", $dn, $x->{n}{C} // 0, $other,
      f2($x->{q_cpu}), f2($x->{pr_cpu}), f2(num($x->{pr_cpu}) - num($x->{q_cpu})), ratio($x->{pr_cpu}, $x->{q_cpu}),
      f2($x->{q_late_cpu}), f2($x->{r_late_cpu}), f2($x->{r_rcomp}), f2($x->{r_gc}), f2($x->{q_sub}), f2($x->{r_sub}),
      defined $x->{top} ? sprintf('%s (%s)', $x->{top}, f2($x->{topx})) : '-';
  }
  my $hdr = "| # | file | perl ms | PCL ms | excess ms | ratio | late perl / PCL ms | CLOS ms | GC ms | MB | subproc perl / PCL ms | spread perl / PCL | load |\n"
          . "|---:|---|---:|---:|---:|---:|---|---:|---:|---:|---|---|---:|\n";
  my $i = 0;
  my $line = sub {
    my ($r) = @_;
    $i++;
    return sprintf "| %d | %s/%s | %s | %s | %s | %s | %s / %s | %s | %s | %s | %s / %s | %.0f%% / %.0f%% | %.2f |\n", $i, $r->{dist}, $r->{file},
      ms($r->{q_cpu}), ms($r->{pr_cpu}), ms($r->{pr_cpu} - $r->{q_cpu}), ratio($r->{pr_cpu}, $r->{q_cpu}),
      ms($r->{q_late_cpu}), ms($r->{r_late_cpu}), ms($r->{r_rcomp}), ms($r->{r_gc}), $r->{r_mb} // '',
      ms($r->{q_sub}), ms($r->{r_sub}), 100 * num($r->{q_spread}), 100 * num($r->{pr_spread}), $r->{load};
  };
  my @by_ex = sort { ($b->{pr_cpu} - $b->{q_cpu}) <=> ($a->{pr_cpu} - $a->{q_cpu}) } @cmp;
  $o .= "\n## Top $topn by run-phase excess (PCL - perl)\n\n$hdr" . join('', map { $line->($_) } grep { defined } @by_ex[0 .. $topn - 1]);
  # PCL under HALF of perl: a possible silent skip (or the native TAP layer)
  my @fast = sort { $b->{q_cpu} <=> $a->{q_cpu} } grep { $_->{pr_cpu} * 2 < $_->{q_cpu} } @cmp;
  $i = 0;
  $o .= "\n## PCL's run phase under HALF of perl's (" . scalar(@fast) . " files: possible silent skips, read before believing)\n\n$hdr"
      . join('', map { $line->($_) } @fast);
  # perl's run phase two ways: the INIT/END clock against (whole file - perl -c), walls, best of the reps
  my @pc = grep { length($_->{q_c_wall} // '') } @cmp;
  if (@pc) {
    my @top = (sort { $b->{q_wall} <=> $a->{q_wall} } @pc)[0 .. 9];
    $o .= "\n## perl's run phase two ways (wall, best of the reps): the INIT/END clock vs the whole file minus `perl -c`\n\n"
        . "| file | whole | perl -c | whole - (-c) | INIT..END | difference | INIT..END spread |\n|---|---:|---:|---:|---:|---:|---:|\n";
    for my $r (grep { defined } @top) {
      my $d = $r->{p_wall} - $r->{q_c_wall};
      $o .= sprintf "| %s/%s | %s | %s | %s | %s | %s | %.0f%% |\n", $r->{dist}, $r->{file}, ms($r->{p_wall}), ms($r->{q_c_wall}),
        ms($d), ms($r->{q_wall}), ms($d - $r->{q_wall}), 100 * num($r->{q_spread});
    }
    my @diff = sort { $a <=> $b } map { $_->{p_wall} - $_->{q_c_wall} - $_->{q_wall} } @pc;
    my ($sd, $sq) = (0, 0); for (@pc) { $sd += $_->{p_wall} - $_->{q_c_wall}; $sq += $_->{q_wall} }
    $o .= sprintf "\nOver all %d files (ms): (whole - perl -c) - INIT..END = median %s, p10 %s, p90 %s; sums %s s vs %s s.\n",
      scalar @diff, ms($diff[$#diff / 2]), ms($diff[int($#diff * 0.1)]), ms($diff[int($#diff * 0.9)]), f2($sd), f2($sq);
  }
  my @nq = grep { $_->{cmp} eq 'C' && !$_->{q_init} } @$rows;
  $o .= sprintf "\nCompared files where perl never reached INIT (left before its run phase): %d%s\n", scalar @nq,
    @nq ? ' -- ' . join(', ', map { "$_->{dist}/$_->{file}" } @nq) : '';
  write_run_table($table, $rows, $taken) if defined $table;
  return $o;
}

sub write_run_table {
  my ($path, $rows, $taken) = @_;
  my @c = qw(dist file cmp c_class nrep ncmp load q_init q_cpu pr_cpu excess ratio q_spread pr_spread q_wall pr_wall
             q_late_cpu q_late_n r_late_cpu r_late_n r_rcomp r_rcomp_n r_eval_cpu r_xsrv r_build r_gc r_mb q_sub r_sub r_cpu r_lw_cpu run_how p_ok c_ok p_wall q_c_wall src);
  open my $t, '>', $path or die "board-timing.pl: cannot write $path: $!\n";
  print $t "# taken-at: $_\n" for @$taken;
  print $t join("\t", @c), "\n";
  for my $r (sort { $a->{dist} cmp $b->{dist} || $a->{file} cmp $b->{file} } @$rows) {
    my %v = %$r;
    if (length($r->{pr_cpu} // '') && length($r->{q_cpu} // '')) {
      $v{excess} = $r->{pr_cpu} - $r->{q_cpu};
      $v{ratio} = $r->{q_cpu} > 0 ? sprintf('%.2f', $r->{pr_cpu} / $r->{q_cpu}) : '';
    }
    print $t join("\t", map { my $x = $v{$_}; !defined $x ? ''
                              : $x =~ /^-?\d*\.\d+(?:e-?\d+)?$/i ? sprintf('%.5f', $x) : $x } @c), "\n";
  }
  close $t;
}

# The per-file table (--report --table OUT.tsv): one line per board file, its
# warm row (the precise one when a later input supplied it) and its cold CPU;
# CPU columns are load-normalised when --norm ran (column `f` = the factor).
sub write_table {
  my ($path, $warm, $cold, $taken) = @_;
  my @c = qw(dist file cmp c_class src nrep spread load1 f p_wall p_cpu c_wall c_cpu
             A_cpu B_cpu C_cpu x_cpu tcompile_c mcompile_c scompile_c ecompile_c cfile_c
             modload_c subproc_cpu cold_cpu mb_consed gc p_ok c_ok);
  open my $t, '>', $path or die "board-timing.pl: cannot write $path: $!\n";
  print $t "# taken-at: $_\n" for @$taken;
  print $t join("\t", @c), "\n";
  for my $r (sort { $a->{dist} cmp $b->{dist} || $a->{file} cmp $b->{file} } @$warm) {
    my %v = (%$r, subproc_cpu => subproc($r),
             cold_cpu => ($cold->{"$r->{dist}\t$r->{file}"} // {})->{c_cpu});
    print $t join("\t", map { my $x = $v{$_}; !defined $x ? ''
                              : $x =~ /^-?\d*\.\d+(?:e-?\d+)?$/i ? sprintf('%.3f', $x) : $x } @c), "\n";
  }
  close $t;
}

# ------------------------------------------------------------------- hooks --
# The hooks: written here, compiled ONCE per invocation with the same core
# (a fasl is core-specific), loaded in front of every program.
sub build_hooks {
  my $src = "$tmp/bt-hooks.lisp";
  open my $h, '>', $src or die "board-timing.pl: $src: $!\n";
  print $h hooks_source();
  close $h;
  my $fasl = "$tmp/bt-hooks.fasl";
  my $log = `$sbcl --eval '(compile-file "$src" :output-file "$fasl")' 2>&1 </dev/null`;
  -s $fasl or die "board-timing.pl: the hooks did not compile:\n$log\n";
  return $fasl;
}

sub hooks_source { return <<'LISP' }
;;; board-timing hooks (tools/board-timing.pl): wrap runtime entry points and
;;; report, at exit, wall + CPU per bucket, EXCLUSIVE of nested buckets.
(defpackage :board-timing (:use :cl))
(in-package :board-timing)
(defun now-us ()
  (multiple-value-bind (s us) (sb-ext:get-time-of-day) (+ (* s 1000000) us)))
(defun cpu-us ()
  (round (* (get-internal-run-time) 1000000) internal-time-units-per-second))
;;; CHILD CPU (s541a): the reaped children's user+sys (getrusage CHILDREN), so a
;;; module transpile (a pl2cl child, reaped inside its bucket) is told apart from
;;; the subprocesses the PROGRAM starts.
(defun child-us ()
  (multiple-value-bind (ok u s) (sb-unix:unix-getrusage sb-unix:rusage_children)
    (if ok (+ u s) 0)))
(defvar *pid0* (sb-posix:getpid))
(defvar *t0-us* (now-us))
(defvar *t0-cpu* (cpu-us))
(defvar *t0-bytes* (sb-ext:get-bytes-consed))
(defvar *t0-gc* sb-ext:*gc-run-time*)
(defvar *boundary* nil)
(defvar *pre* nil)
(defvar *in-boundary* nil)
;;; MEASURES per bucket, each EXCLUSIVE of nested buckets: wall, cpu, child (reaped
;;; children's CPU), gc (SBCL's GC run time), bytes (consed).  All microseconds but bytes.
(defun gc-now () (round (* sb-ext:*gc-run-time* 1000000) internal-time-units-per-second))
(defun measures () (vector (now-us) (cpu-us) (child-us) (gc-now) (sb-ext:get-bytes-consed)))
(defvar *measure-names* '("wall" "cpu" "child" "gc" "bytes"))
(defvar *tabs* (coerce (loop repeat 5 collect (make-hash-table :test 'eq)) 'vector))
(defvar *wall* (svref *tabs* 0))
(defvar *cpu* (svref *tabs* 1))
(defvar *child* (svref *tabs* 2))
(defvar *count* (make-hash-table :test 'eq))
(defvar *frame* nil)
(defvar *ctx* :script)
(defvar *prof* (sb-ext:posix-getenv "PCL_BT_PROF"))
(when *prof* (require :sb-sprof))
(defvar *prof-from-start* (equal (sb-ext:posix-getenv "PCL_BT_PROF_FROM") "start"))
(defun call-in (bucket ctx fn args)
  (let ((frame (make-array 5 :initial-element 0)) (m0 (measures)))
    (incf (gethash bucket *count* 0))
    (unwind-protect
         (let ((*frame* frame) (*ctx* (or ctx *ctx*)))
           (apply fn args))
      (let ((m1 (measures)))
        (dotimes (i 5)
          (let ((d (- (svref m1 i) (svref m0 i))))
            (incf (gethash bucket (svref *tabs* i) 0) (- d (svref frame i)))
            (when *frame* (incf (svref *frame* i) d))))))))
(defun copy-table (h)
  (let ((c (make-hash-table :test 'eq)))
    (maphash (lambda (k v) (setf (gethash k c) v)) h)
    c))
;;; THE PROGRAM'S RUN PHASE starts where perl's INIT phase starts: after the
;;; main program's CHECK blocks (%p-drain-compile-blocks inside the main
;;; p-run-compile-phase-blocks), before its INIT blocks.  The bucket tables are
;;; snapshotted there, so a bucket's after-boundary share is total - pre.
(defun mark-boundary ()
  (unless *boundary*
    (setf *boundary* (list (now-us) (cpu-us) (child-us) (sb-ext:get-bytes-consed)
                           sb-ext:*gc-run-time*)
          *pre* (cons (copy-table *count*) (map 'vector #'copy-table *tabs*)))
    (when (and *prof* (not *prof-from-start*)) (prof-start))))
;;; WHO ASKED FOR A NATIVE COMPILE (s541a): the text loader (SBCL's top-level
;;; eval, sb-impl::%simple-eval) compiling a form of the program / a module /
;;; the TAP layer, or a string eval -- or SBCL itself at RUN time: CLOS (SB-PCL)
;;; compiles a constructor or a dispatch function the first time a class is
;;; instantiated or a generic function called (cl-ppcre's regex node classes),
;;; bucket RCOMPILE: run time, which perl does not have.
(defun compile-caller ()
  (let ((n 0))
    (sb-debug:map-backtrace
     (lambda (frame)
       (incf n)
       (let* ((name (sb-di:debug-fun-name (sb-di:frame-debug-fun frame)))
              (sym (if (consp name) (second name) name)))
         (when (and (symbolp sym) sym (> n 1)
                    (not (member (symbol-name sym) '("COMPILE-IN-LEXENV" "COMPILE-CALLER" "COMPILE-BUCKET" "CALL-IN")
                                 :test #'string=)))
           (let ((pkg (symbol-package sym)))
             (return-from compile-caller
               (cond ((string= (symbol-name sym) "%SIMPLE-EVAL") :eval)
                     ((and pkg (string= (package-name pkg) "SB-PCL")) :clos)
                     (t :other)))))))
     :count 12)
    :other))
(defun compile-bucket ()
  (let ((who (compile-caller)))
    (cond ((eq who :clos) :rcompile)
          ((and (eq *ctx* :script) (not (eq who :eval))) :rcompile)
          (t (ecase *ctx* (:script :scompile) (:mod :mcompile) (:tap :tcompile) (:eval :ecompile))))))
(defmacro wrap (fname bucket ctx)
  `(when (fboundp ',fname)
     (sb-int:encapsulate ',fname 'board-timing
                         (lambda (f &rest args) (call-in ,bucket ,ctx f args)))))
(defmacro counter (fname key)
  `(when (fboundp ',fname)
     (sb-int:encapsulate ',fname 'board-timing-count
                         (lambda (f &rest args)
                           (incf (gethash ,key *count* 0))
                           (apply f args)))))
(defun prof-start ()
  (funcall (intern "START-PROFILING" "SB-SPROF")
           :mode (if (string= *prof* "alloc") :alloc :cpu)
           :max-samples 400000 :sample-interval 0.001 :threads :all))
(defun prof-report ()
  (let ((path (sb-ext:posix-getenv "PCL_BT_PROF_OUT")))
    (when (and *prof* path (or *boundary* *prof-from-start*))
      (funcall (intern "STOP-PROFILING" "SB-SPROF"))
      (with-open-file (s path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
        (let ((*standard-output* s))
          (funcall (intern "REPORT" "SB-SPROF") :type :flat :max 400)
          (funcall (intern "REPORT" "SB-SPROF") :type :graph :max 40)))
      (setf *prof* nil))))
(sb-ext:without-package-locks
  (wrap pcl::p-load-module-cached :modload :mod)
  (wrap pcl::%p-load-module-uncached :modload :mod)
  (wrap pcl::p-load-extension :modload :mod)
  (wrap pcl::p-ensure-test-lib :tap :tap)
  (wrap pcl::p-transpile-file :xpile nil)
  (wrap pcl::p-transpile-string :evalx nil)
  (wrap pcl::p-ensure-transpiler :xserver nil)
  (wrap pcl::%p-build-module-fasl :cfile :mod)
  (wrap pcl::%p-eval-1 :evalrun :eval)
  (counter pcl::%p-load-module-fasl :faslload)
  (counter pcl::%p-load-text-stream :textload)
  (when (fboundp 'pcl::p-run-compile-phase-blocks)
    (sb-int:encapsulate 'pcl::p-run-compile-phase-blocks 'board-timing
                        (lambda (f &rest args)
                          (multiple-value-prog1
                              (let ((*in-boundary* (not pcl::*p-unit-load*)))
                                (apply f args))
                            ;; no CHECK drain seen (a runtime without it): the end
                            (when (not pcl::*p-unit-load*) (mark-boundary))))))
  (when (fboundp 'pcl::%p-drain-compile-blocks)
    (sb-int:encapsulate 'pcl::%p-drain-compile-blocks 'board-timing
                        (lambda (f &rest args)
                          (multiple-value-prog1 (apply f args)
                            (when *in-boundary* (mark-boundary))))))
  ;; READING the program's text (s541a): SBCL's LOAD of a source file reads
  ;; each top-level form inside sb-c::%do-forms-from-info and hands it to a
  ;; callback that evaluates it.  For the MAIN program (no bucket open, the
  ;; script's context) the whole call is bucket SREAD and the callback bucket
  ;; SEVAL, so SREAD's exclusive time is the reading alone; SEVAL is run.
  (when (fboundp 'sb-c::%do-forms-from-info)
    (sb-int:encapsulate 'sb-c::%do-forms-from-info 'board-timing
                        (lambda (f fn &rest rest)
                          (if (and (null *frame*) (eq *ctx* :script))
                              (call-in :sread :script f
                                       (list* (lambda (&rest a) (call-in :seval :script fn a))
                                              rest))
                              (apply f fn rest)))))
  (sb-int:encapsulate 'sb-c:compile-in-lexenv 'board-timing
                      (lambda (f &rest args) (call-in (compile-bucket) nil f args))))
(defun gc-us (v)
  (round (* v 1000000) internal-time-units-per-second))
(defun write-record (phase)
  (let ((path (sb-ext:posix-getenv "PCL_BT_OUT")))
    ;; a forked child runs the exit hooks too: only the program's own process writes
    (when (and path (= (sb-posix:getpid) *pid0*))
      (with-open-file (s path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
        (format s "phase=~A~%t0_us=~D~%t0_cpu_us=~D~%exit_us=~D~%exit_cpu_us=~D~%exit_child_us=~D~%"
                phase *t0-us* *t0-cpu* (now-us) (cpu-us) (child-us))
        (when *boundary*
          (destructuring-bind (w c k b g) *boundary*
            (format s "boundary_us=~D~%boundary_cpu_us=~D~%boundary_child_us=~D~%boundary_bytes=~D~%boundary_gc_us=~D~%"
                    w c k (- b *t0-bytes*) (gc-us (- g *t0-gc*)))))
        (format s "bytes=~D~%gc_us=~D~%"
                (- (sb-ext:get-bytes-consed) *t0-bytes*)
                (gc-us (- sb-ext:*gc-run-time* *t0-gc*)))
        (write-tables s "" *tabs* *count*)
        (when *pre* (write-tables s "pre_" (cdr *pre*) (car *pre*)))))))
(defun write-tables (s prefix tabs count)
  (loop for name in *measure-names* for i from 0
        do (maphash (lambda (k v) (format s "~A~(~A~)_~A~:[_us~;~]=~D~%" prefix k name (string= name "bytes") v))
                    (svref tabs i)))
  (maphash (lambda (k v) (format s "~A~(~A~)_n=~D~%" prefix k v)) count))
(when (and *prof* *prof-from-start*) (prof-start))
(push (lambda () (ignore-errors (write-record "front")))
      sb-ext:*exit-hooks*)
(setf sb-ext:*exit-hooks*
      (append sb-ext:*exit-hooks*
              (list (lambda ()
                      (when (= (sb-posix:getpid) *pid0*) (ignore-errors (prof-report)))
                      (ignore-errors (write-record "last"))))))
LISP

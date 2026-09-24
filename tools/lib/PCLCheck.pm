# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package PCLCheck;
# `pcl --check` (task #2194, s494k): run ONE program under perl AND under PCL
# and say whether they agree -- so a user can find out, for the program they
# care about, that PCL gave a wrong answer.  PCL's own method is "perl is the
# oracle"; this hands that method to the user.  docs/pcl-check.md is the page.
#
# What is compared, and why (the USER's ruling on errors, DECIDED `## s494`:
# "fail in the SAME PLACES, not horribly messy" -- message TEXT is free):
#   * STDOUT, byte for byte;
#   * the exit status, by CLASS: success / non-zero exit / death by signal.
#     Two different non-zero exit codes are the SAME failure (perl's `die`
#     is 255, an `exit 2` is 2 -- the texts behind them may differ freely);
#     a death by signal on one side only is not;
#   * STDERR is never compared -- only its LINE COUNT is reported per side,
#     because "perl warned or died and PCL said nothing" is exactly the
#     failing-in-different-places case.
#
# The oracle is $^X -- the perl running `pcl` itself, never a `perl` found on
# PATH.  The PCL side re-invokes the SAME driver (`$root/pcl`) without
# --check, so the script cache, the core and every option behave as in a
# plain run.  Each child sees PCL_CHECK_SIDE=perl or PCL_CHECK_SIDE=pcl.
#
# Only core modules (CI is a stock machine: Pl/t/core-deps-01.t).
#
# Exit status of `pcl --check`: 0 = the two agree (IDENTICAL, SAME FAILURE),
# 1 = they differ, 2 = the check could not be made.
use strict;
use warnings;
use Config ();
use File::Path qw(make_path remove_tree);
use File::Spec ();
use File::Temp qw(tempdir);
use POSIX ();

my $MAX_SHOW = 200;     # longest (raw) slice of a line shown in a verdict
my $DASH = "\xE2\x80\x94";   # an em dash, as UTF-8 bytes (STDOUT has no layer)

# Entry point.  %o comes from the driver's own option parse:
#   root inline inline_flag(e|E) source args inc mods warnings no_cache
#   verbose check_only stdin keep
# Returns the exit status for `pcl --check`.
sub run_check {
  my (%o) = @_;
  my $why = _cannot_check_reason(\%o);
  return _cannot($why) if defined $why;

  my ($dir, $kept) = _capture_dir($o{keep});
  return _cannot("cannot create the capture directory $o{keep}: $dir") if !defined $kept;
  my $stdin = defined $o{stdin} ? $o{stdin} : File::Spec->devnull;

  my $perl = _run_side('perl', [_perl_cmd(\%o)], $stdin, $dir);
  return _cannot("could not run perl ($^X): $perl->{exec_error}") if $perl->{exec_error};
  my $pcl = _run_side('pcl', [_pcl_cmd(\%o)], $stdin, $dir);
  return _cannot("could not run pcl ($o{root}/pcl): $pcl->{exec_error}") if $pcl->{exec_error};

  my ($rc, @lines) = verdict($perl, $pcl);
  print "$_\n" for @lines;
  _closing($rc, $dir, $kept, $o{root});
  remove_tree($dir) if !$kept && $rc == 0;
  return $rc;
}

sub _cannot_check_reason {
  my ($o) = @_;
  return "-c only transpiles; --check needs the program to RUN" if $o->{check_only};
  return "no script and no -e (usage: pcl --check [options] script.pl [args...])"
    if !defined $o->{inline} && !defined $o->{source};
  return "can't open script \"$o->{source}\": No such file or directory"
    if defined $o->{source} && !-f $o->{source};
  return "can't read --check-stdin file \"$o->{stdin}\""
    if defined $o->{stdin} && !-r $o->{stdin};
  return undef;
}

sub _cannot {
  my ($why) = @_;
  print "pcl --check: cannot check $DASH $why\n";
  return 2;
}

# ($dir, $kept): --check-keep DIR is created and kept always; otherwise a
# fresh temp dir, kept only when the verdict is a difference.
sub _capture_dir {
  my ($keep) = @_;
  if (defined $keep) {
    eval { make_path($keep) if !-d $keep; 1 } or return ($@ || 'mkdir failed', undef);
    return (-d $keep ? ($keep, 1) : ("not a directory", undef));
  }
  return (tempdir('pcl-check-XXXXXX', TMPDIR => 1), 0);
}

# ---- the two command lines -------------------------------------------------

# perl(1) takes -I, -M and -w with the same meaning pcl gives them.  -E keeps
# its perl meaning here (pcl treats it as -e; perl enables the features).
sub _perl_cmd {
  my ($o) = @_;
  my @cmd = ($^X, map({ "-I$_" } @{ $o->{inc} || [] }),
             map({ "-M$_" } @{ $o->{mods} || [] }), ($o->{warnings} ? '-w' : ()));
  my @args = @{ $o->{args} || [] };
  return (@cmd, '--', $o->{source}, @args) if !defined $o->{inline};
  # After -e, perl reads a leading `-x` as ITS switch; a `--` ends that.  When
  # the user typed the `--` themselves it is theirs, and perl strips it.
  push @cmd, "-$o->{inline_flag}", $o->{inline};
  return (@cmd, (@args && $args[0] eq '--' ? () : ('--')), @args);
}

# The same driver, the same options, minus --check.  NO `--`: pcl keeps a
# `--` in @ARGV (pass_through), and the script and its arguments reach pcl's
# option parse exactly as they did in the --check invocation itself.
sub _pcl_cmd {
  my ($o) = @_;
  my @cmd = ($^X, "$o->{root}/pcl", map({ ('-I', $_) } @{ $o->{inc} || [] }),
             map({ ('-M', $_) } @{ $o->{mods} || [] }),
             ($o->{warnings} ? '-w' : ()), ($o->{no_cache} ? '--no-cache' : ()),
             ($o->{verbose} ? '-v' : ()));
  push @cmd, defined $o->{inline} ? ('-e', $o->{inline}) : ($o->{source});
  return (@cmd, @{ $o->{args} || [] });
}

# ---- running one side ------------------------------------------------------

# Runs @$cmd with stdin from $stdin and stdout/stderr into $dir/$side.out and
# $dir/$side.err.  An exec failure comes back over a close-on-exec pipe, so it
# cannot be mistaken for the program exiting 127.
sub _run_side {
  my ($side, $cmd, $stdin, $dir) = @_;
  my ($out, $err) = ("$dir/$side.out", "$dir/$side.err");
  pipe(my $r, my $w) or return { exec_error => "pipe: $!" };
  my $pid = fork;
  return { exec_error => "fork: $!" } if !defined $pid;
  if ($pid == 0) {
    close $r;
    local $ENV{PCL_CHECK_SIDE} = $side;
    my $ok = open(STDIN, '<', $stdin) && open(STDOUT, '>', $out) && open(STDERR, '>', $err);
    exec { $cmd->[0] } @$cmd if $ok;
    print {$w} "$!";
    close $w;
    POSIX::_exit(127);
  }
  close $w;
  my $exec_error = do { local $/; <$r> };
  close $r;
  waitpid($pid, 0);
  my $status = $?;
  return { exec_error => $exec_error } if defined $exec_error && length $exec_error;
  return { side => $side, status => $status, out => _slurp($out), err => _slurp($err) };
}

sub _slurp {
  my ($path) = @_;
  open my $fh, '<:raw', $path or return '';
  local $/;
  my $c = <$fh>;
  return defined $c ? $c : '';
}

# ---- the verdict -----------------------------------------------------------

# (rc, lines): the verdict line first, the details after.  Pure: it reads only
# the two result records, so it can be tested without running anything.
sub verdict {
  my ($p, $c) = @_;
  my ($pf, $cf) = (_failed($p->{status}), _failed($c->{status}));
  my $n = length $p->{out};
  if ($p->{out} ne $c->{out}) {
    return (1, _different_output($p, $c));
  }
  if (!$pf && !$cf) {
    return (0, "pcl --check: IDENTICAL $DASH $n bytes of output, exit 0");
  }
  my @status = ("  exit: perl " . status_text($p->{status}) . ", pcl " . status_text($c->{status}),
                _stderr_counts($p, $c));
  if ($pf && $cf) {
    if (_signalled($p->{status}) == _signalled($c->{status})) {
      return (0, "pcl --check: SAME FAILURE $DASH output identical ($n bytes), both failed "
                 . "(perl " . status_text($p->{status}) . ", pcl " . status_text($c->{status}) . ")",
              _stderr_counts($p, $c));
    }
    return (1, "pcl --check: DIFFERENT FAILURE $DASH output identical ($n bytes), but only one "
               . "side was killed by a signal", @status, _first_err('perl', $p), _first_err('pcl', $c));
  }
  my ($who, $failed) = $pf ? ('PERL', $p) : ('PCL', $c);
  return (1, "pcl --check: ONLY $who FAILED $DASH output identical ($n bytes)", @status,
          _first_err(lc $who, $failed));
}

sub _failed    { my ($s) = @_; return $s != 0 }
sub _signalled { my ($s) = @_; return ($s & 127) ? 1 : 0 }

# "exit N" or "killed by signal N (NAME)"
sub status_text {
  my ($s) = @_;
  my $sig = $s & 127;
  return "exit " . ($s >> 8) if !$sig;
  my @names = split ' ', ($Config::Config{sig_name} || '');
  my $name = defined $names[$sig] ? " (SIG$names[$sig])" : '';
  return "killed by signal $sig$name";
}

sub _stderr_counts {
  my ($p, $c) = @_;
  return "  stderr: perl " . _count_lines($p->{err}) . ", pcl " . _count_lines($c->{err})
       . " (stderr is not compared -- only counted)";
}

sub _count_lines {
  my ($s) = @_;
  my $n = () = $s =~ /\n/g;
  $n++ if length $s && substr($s, -1) ne "\n";
  return $n == 1 ? "1 line" : "$n lines";
}

sub _first_err {
  my ($name, $r) = @_;
  return () if !_failed($r->{status});
  my ($first) = $r->{err} =~ /\A([^\n]*)/;
  return sprintf("  %-5s first stderr line: %s", "$name", length($first) ? show($first) : '(stderr is empty)');
}

sub _different_output {
  my ($p, $c) = @_;
  my @pl = split /(?<=\n)/, $p->{out};
  my @cl = split /(?<=\n)/, $c->{out};
  my $i = 0;
  $i++ while $i < @pl && $i < @cl && $pl[$i] eq $cl[$i];
  my ($pa, $ca) = ($pl[$i], $cl[$i]);
  my $col = _first_diff_col(defined $pa ? $pa : "", defined $ca ? $ca : "");
  my $start = $col - 1 > $MAX_SHOW / 2 ? $col - 1 - int($MAX_SHOW / 4) : 0;
  my @out = ("pcl --check: DIFFERENT OUTPUT $DASH first difference at line " . ($i + 1),
             "  perl: " . _show_line($pa, $start), "  pcl:  " . _show_line($ca, $start),
             "  (byte column $col of that line is the first difference)");
  push @out, "  exit: perl " . status_text($p->{status}) . ", pcl " . status_text($c->{status}),
             _stderr_counts($p, $c), _first_err('perl', $p), _first_err('pcl', $c);
  return @out;
}

# 1-based byte column of the first difference between two lines.
sub _first_diff_col {
  my ($s, $t) = @_;
  my $x = $s ^ $t;
  return 1 + ($x =~ /[^\0]/ ? $-[0] : (length($s) < length($t) ? length($s) : length($t)));
}

# One output line for display: the terminating newline stripped (and its
# absence said), a window of at most $MAX_SHOW raw bytes from $start,
# non-printable bytes as \xNN.
sub _show_line {
  my ($line, $start) = @_;
  return "(no line here -- the output ended)" if !defined $line;
  my $nl = $line =~ s/\n\z// ? '' : ' (no newline at end of output)';
  my $len = length $line;
  $start = 0 if $start >= $len;
  my $cut = substr($line, $start, $MAX_SHOW);
  my $pre = $start > 0 ? '...' : '';
  my $post = $start + $MAX_SHOW < $len ? "... ($len bytes)" : '';
  return $pre . show($cut) . $post . $nl;
}

# Escape every byte outside printable ASCII as \xNN (and a backslash as \\,
# so an escape in the output cannot be confused with one we made).
sub show {
  my ($s) = @_;
  $s =~ s/\\/\\\\/g;
  $s =~ s/([^\x20-\x7e])/sprintf('\\x%02X', ord $1)/ge;
  return $s;
}

# ---- the closing lines -----------------------------------------------------

sub _closing {
  my ($rc, $dir, $kept, $root) = @_;
  return if $rc == 0 && !$kept;
  print "  captured: perl.out perl.err pcl.out pcl.err in $dir\n";
  return if $rc == 0;
  my $url = issues_url($root);
  print "If perl is right and PCL is wrong, this is a bug worth reporting: "
      . (defined $url ? $url : "see the PCL README for where") . "\n";
}

# THE issues URL is read from README.md -- the one place it is written
# ("Issues and pull requests are welcome at <URL>"); the installer copies
# README.md into an installed tree for this reason.
sub issues_url {
  my ($root) = @_;
  open my $fh, '<', "$root/README.md" or return undef;
  local $/;
  my $text = <$fh>;
  return undef if !defined $text;
  return undef if $text !~ /Issues and pull requests are welcome at\s*<(https?:\/\/[^>\s]+)>/;
  (my $u = $1) =~ s{/+\z}{};
  return "$u/issues";
}

1;

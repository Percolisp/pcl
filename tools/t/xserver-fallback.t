#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pl2cl's transpile-server CLIENT falls back to the ordinary path SILENTLY when
# the server goes away (task #3513, ruled s521): a reply cut off by the
# server's exit, the server's `PCLXS2 DOWN` reply (its capture vanished with
# its owner's cleanup), and a PCL_XSERVER directory that no longer exists.
# A forked child of a program outliving the harness that owns the server
# (t/op/fork.t case 9) hit all three, and each printed a harness line on the
# program's stderr -- or a "Failed to transpile" -- where perl's child is fine.
#
# A FAKE server stands in for the real one: it accepts one request and then
# closes the connection (or answers DOWN).  The socket path is pl2cl's own
# derivation (_xc_key, reproduced below), over an environment this test pins.
#
# NOT in the Pl/t gate (it measures the harness).  Run it directly:
#   prove tools/t/xserver-fallback.t
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use Cwd qw(abs_path);
use FindBin;
use Socket;
use POSIX ();

my $root  = abs_path("$FindBin::RealBin/../..");
my $pl2cl = "$root/pl2cl";
my $tmp   = tempdir(CLEANUP => 1);
my $src   = "$tmp/p.pl";
{ open my $f, '>', $src or die; print $f "my \$x = 1 + 2;\n"; close $f }

# pl2cl's _xc_key: FNV-1a over "$^X\0$script" and the sorted PCL*/PERL5LIB env.
sub xc_key {
  my ($env) = @_;
  my $s = join "\0", "$^X\0$pl2cl",
                     map { "$_=" . (defined $env->{$_} ? $env->{$_} : '') }
                     sort grep { /^PCL/ || $_ eq 'PERL5LIB' } keys %$env;
  my $h = 2166136261;
  for my $c (unpack 'C*', $s) { $h = (($h ^ $c) * 16777619) & 0xFFFFFFFF }
  return sprintf '%08x', $h;
}

# Run pl2cl on $src with PCL_XSERVER=$base and a fake server doing $mode.
sub run_with {
  my ($base, $mode) = @_;
  my %env = map { $_ => $ENV{$_} } grep { !/^PCL/ } keys %ENV;
  $env{PCL_XSERVER} = $base;
  $env{PCL_XSERVER_OWNER} = $$;
  my $sock = "$base-" . xc_key(\%env) . ".sock";
  my $srv_pid;
  if ($mode ne 'nodir') {
    socket(my $srv, PF_UNIX, SOCK_STREAM, 0) or die "socket: $!";
    unlink $sock;
    bind($srv, pack_sockaddr_un($sock)) or die "bind $sock: $!";
    listen($srv, 4) or die "listen: $!";
    $srv_pid = fork() // die "fork: $!";
    if (!$srv_pid) {
      accept(my $c, $srv) or POSIX::_exit(1);
      my $hdr = <$c>;                          # "PCLXS2 <len>\n"
      my ($len) = ($hdr // '') =~ /^PCLXS2 (\d+)/;
      read($c, my $blob, $len // 0);
      print {$c} "PCLXS2 DOWN\n" if $mode eq 'down';
      close $c;                                # 'cut': EOF, no reply
      POSIX::_exit(0);
    }
    close $srv;
  }
  my ($out, $err) = ("$tmp/out", "$tmp/err");
  my $rc = do {
    local %ENV = %env;
    system("$^X $pl2cl $src > $out 2> $err");
  };
  waitpid($srv_pid, 0) if $srv_pid;
  my $o = do { local $/; open my $f, '<', $out or die; <$f> };
  my $e = do { local $/; open my $f, '<', $err or die; <$f> };
  return ($rc, $o, $e);
}

for my $mode (qw(cut down)) {
  my ($rc, $o, $e) = run_with("$tmp/s$mode", $mode);
  is($rc, 0, "$mode: pl2cl exits 0 (fell back to the ordinary path)");
  like($o, qr/\(p-my-= \$x|p-setf \$x|\$x/, "$mode: and transpiled the program");
  is($e, '', "$mode: and said NOTHING on stderr");
}
{
  my ($rc, $o, $e) = run_with("$tmp/gone/s", 'nodir');
  is($rc, 0, 'a vanished PCL_XSERVER directory: exit 0');
  is($e, '', '... and no "names no existing directory" line on stderr');
}

done_testing;

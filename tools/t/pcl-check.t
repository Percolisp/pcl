#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Test for `pcl --check` (task #2194, s494k): run a program under perl AND
# under PCL and say whether they agree.  docs/pcl-check.md is the page.
#
# NOT part of the Pl/t gate (it tests a driver option, and every row spawns
# perl AND an SBCL).  Run it directly:  prove tools/t/pcl-check.t
#
# DETERMINISTIC ON ANY TREE: a "differ" fixture never relies on a PCL bug --
# it prints $ENV{PCL_CHECK_SIDE}, which `pcl --check` sets to `perl` or `pcl`
# in each child.  Each row asserts the verdict's FIRST LINE and the exit
# status of `pcl --check`; the prose after the first line is free to change,
# except where a row names the detail it is about (escaping, the column).
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempdir);
use FindBin qw($RealBin);

my $root = "$RealBin/../..";
plan skip_all => "sbcl not found" if !`which sbcl 2>/dev/null`;

my $dir = tempdir('pcl-check-t-XXXXXX', TMPDIR => 1, CLEANUP => 1);

sub put {
  my ($name, $text) = @_;
  open my $fh, '>:raw', "$dir/$name" or die "$dir/$name: $!";
  print $fh $text;
  close $fh;
  return "$dir/$name";
}

# (exit status, stdout lines) of `pcl --check @args`
sub check {
  my (@args) = @_;
  open my $fh, '-|', $^X, "$root/pcl", '--check', @args or die "cannot run pcl: $!";
  my @lines = <$fh>;
  close $fh;
  chomp @lines;
  return ($? >> 8, @lines);
}

my $DASH = "\xE2\x80\x94";

# ---- the verdicts -----------------------------------------------------------

my $same = put('same.pl', qq{print "hello\\n";\n});
my ($rc, @v) = check($same);
is($v[0], "pcl --check: IDENTICAL $DASH 6 bytes of output, exit 0", 'identical program: IDENTICAL');
is($rc, 0, 'IDENTICAL exits 0');

my $side = put('side.pl', qq{print "\$ENV{PCL_CHECK_SIDE}\\n";\n});
($rc, @v) = check($side);
is($v[0], "pcl --check: DIFFERENT OUTPUT $DASH first difference at line 1", 'side-dependent output: DIFFERENT OUTPUT');
is($rc, 1, 'DIFFERENT OUTPUT exits 1');
ok((grep { m{^If perl is right and PCL is wrong, this is a bug worth reporting: https?://\S+/issues$} } @v),
   'a difference ends with the invitation to report it, and the URL comes from README');
ok((grep { /^  captured: perl\.out perl\.err pcl\.out pcl\.err in (\S+)$/ && -f "$1/pcl.out" } @v),
   'a difference keeps the four captured files and names where');

my $die = put('die.pl', qq{print "before\\n";\ndie "boom\\n";\n});
($rc, @v) = check($die);
is($v[0], "pcl --check: SAME FAILURE $DASH output identical (7 bytes), both failed (perl exit 255, pcl exit 255)",
   'a die on both sides: SAME FAILURE');
is($rc, 0, 'SAME FAILURE exits 0');

my $only_pcl = put('only-pcl.pl', qq{print "a\\n";\nexit 3 if \$ENV{PCL_CHECK_SIDE} eq "pcl";\n});
($rc, @v) = check($only_pcl);
is($v[0], "pcl --check: ONLY PCL FAILED $DASH output identical (2 bytes)", 'exit 3 on the pcl side only: ONLY PCL FAILED');
is($rc, 1, 'ONLY PCL FAILED exits 1');
ok((grep { $_ eq '  exit: perl exit 0, pcl exit 3' } @v), 'and it shows both statuses');

my $only_perl = put('only-perl.pl', qq{print "a\\n";\nexit 3 if \$ENV{PCL_CHECK_SIDE} eq "perl";\n});
($rc, @v) = check($only_perl);
is($v[0], "pcl --check: ONLY PERL FAILED $DASH output identical (2 bytes)", 'the mirror: ONLY PERL FAILED');
is($rc, 1, 'ONLY PERL FAILED exits 1');

($rc, @v) = check("$dir/no-such-script.pl");
like($v[0], qr/^pcl --check: cannot check $DASH can't open script /, 'a missing script: cannot check');
is($rc, 2, 'cannot check exits 2');

($rc, @v) = check('-c', $same);
like($v[0], qr/^pcl --check: cannot check $DASH -c /, '-c with --check: cannot check (it does not run)');
is($rc, 2, 'and exits 2');

# ---- the invocation forms --------------------------------------------------

($rc, @v) = check('-e', 'print 1+1, "\n"');
is($v[0], "pcl --check: IDENTICAL $DASH 2 bytes of output, exit 0", 'the -e form');
is($rc, 0, '-e form exits 0');

($rc, @v) = check('-E', 'say "hi"');
is($v[0], "pcl --check: IDENTICAL $DASH 3 bytes of output, exit 0", 'the -E form: perl gets -E (say is enabled)');

my $args = put('args.pl', qq{print join("|", \@ARGV), "\\n";\n});
($rc, @v) = check($args, 'a', '-x', 'b c');
is($v[0], "pcl --check: IDENTICAL $DASH 9 bytes of output, exit 0", 'script arguments reach both sides unchanged');

mkdir "$dir/inc" or die;
put('inc/CheckMod.pm', qq{package CheckMod;\nsub v { "from-mod" }\n1;\n});
my $use = put('use.pl', qq{use CheckMod;\nprint CheckMod::v(), "\\n";\n});
($rc, @v) = check('-I', "$dir/inc", $use);
is($v[0], "pcl --check: IDENTICAL $DASH 9 bytes of output, exit 0", '-I reaches BOTH sides (else one fails to load)');

($rc, @v) = check('-MCheckMod', '-I', "$dir/inc", '-e', 'print CheckMod::v(), "\n"');
is($v[0], "pcl --check: IDENTICAL $DASH 9 bytes of output, exit 0", '-M reaches both sides');

# ---- stdin -------------------------------------------------------------------

my $reader = put('reader.pl', qq{my \$l = <STDIN>;\nprint defined \$l ? "got \$l" : "none\\n";\n});
($rc, @v) = check($reader);
is($v[0], "pcl --check: IDENTICAL $DASH 5 bytes of output, exit 0", 'without --check-stdin, STDIN is /dev/null');
my $in = put('in.txt', "one line\n");
($rc, @v) = check('--check-stdin', $in, $reader);
is($v[0], "pcl --check: IDENTICAL $DASH 13 bytes of output, exit 0", '--check-stdin FILE feeds both sides');
($rc, @v) = check('--check-stdin', "$dir/no-such-input", $reader);
is($rc, 2, 'an unreadable --check-stdin file: cannot check');

# ---- escaping, long lines, --check-keep ------------------------------------

my $bin = put('bin.pl', qq{binmode STDOUT;\nprint "ok\\n", "x\\0\\xff", (\$ENV{PCL_CHECK_SIDE} eq "perl" ? "A" : "B"), "\\n";\n});
($rc, @v) = check($bin);
is($v[0], "pcl --check: DIFFERENT OUTPUT $DASH first difference at line 2", 'binary output: the differing line is found');
is($v[1], '  perl: x\x00\xFFA', 'a NUL and a high byte are shown as \xNN (perl side)');
is($v[2], '  pcl:  x\x00\xFFB', 'and on the pcl side');
ok((grep { /byte column 4 of that line/ } @v), 'the first differing byte column is named');

my $long = put('long.pl', qq{print "a" x 400, \$ENV{PCL_CHECK_SIDE}, "b" x 100, "\\n";\n});
($rc, @v) = check($long);
is($v[0], "pcl --check: DIFFERENT OUTPUT $DASH first difference at line 1", 'a long line differing far in');
ok(length($v[1]) < 260 && $v[1] =~ /^  perl: \.\.\.a+perl/, 'is cut to a window around the difference');
ok((grep { /byte column 402 of that line/ } @v), 'and names column 402 (the p of perl/pcl is shared)');

my $keep = "$dir/kept";
($rc, @v) = check('--check-keep', $keep, $same);
is($v[0], "pcl --check: IDENTICAL $DASH 6 bytes of output, exit 0", '--check-keep: the verdict is unchanged');
ok(-f "$keep/perl.out" && -f "$keep/perl.err" && -f "$keep/pcl.out" && -f "$keep/pcl.err",
   '--check-keep keeps all four files even when identical');
open my $po, '<', "$keep/pcl.out" or die;
is(scoalesce(<$po>), "hello\n", 'and pcl.out holds what PCL printed');

sub scoalesce { my ($s) = @_; return defined $s ? $s : '' }

# ---- the status classes, without running anything --------------------------
require lib; lib->import("$root/tools/lib");
require PCLCheck;
my %r = (out => '', err => "x\n");
my ($vr, @vl) = PCLCheck::verdict({ %r, status => 9 }, { %r, status => 15 });
like($vl[0], qr/^pcl --check: SAME FAILURE /, 'killed by different signals on both sides: SAME FAILURE');
($vr, @vl) = PCLCheck::verdict({ %r, status => 255 << 8 }, { %r, status => 9 });
like($vl[0], qr/^pcl --check: DIFFERENT FAILURE /, 'a signal on ONE side only is not the same failure');
is($vr, 1, 'DIFFERENT FAILURE exits 1');
($vr, @vl) = PCLCheck::verdict({ %r, status => 2 << 8 }, { %r, status => 255 << 8 });
like($vl[0], qr/^pcl --check: SAME FAILURE .*\(perl exit 2, pcl exit 255\)$/,
     'two different non-zero exits are the SAME failure, both shown');
is(PCLCheck::status_text(9), 'killed by signal 9 (SIGKILL)', 'a signal is named');

done_testing();

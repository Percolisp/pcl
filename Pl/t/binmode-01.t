#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# binmode-01.t: binmode() return value + EBADF on an unopened handle, and the
# PerlIO::Layer->find introspection shim.
#
# Regression for the t/io/binmode.t crash (2026-06-25): `find PerlIO::Layer
# 'perlio'` (an indirect method call on a core package PCL did not ship) died
# with an uncaught "Can't locate object method", aborting the whole file.  PCL
# now auto-requires a minimal lib/PerlIO/Layer.pm shim on first dispatch.  Also:
# binmode on a filehandle that is not open must fail with errno EBADF, not
# silently succeed.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = PCLCore::transpile(qq{$pl2cl $pl_file});
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

plan tests => 13;

# binmode on an open handle returns true.
is run_cl('open(my $fh, ">", "/tmp/pcl_binmode_t.txt") or die;
           print binmode($fh) ? "ok\n" : "no\n"; close $fh;'),
   "ok\n", 'binmode(open handle) is true';

# binmode with a layer string on a standard handle returns true.
is run_cl('print binmode(STDOUT, ":raw") ? "ok\n" : "no\n";'),
   "ok\n", 'binmode(STDOUT, ":raw") is true';

# binmode on an UNOPENED bareword handle fails and sets $! to EBADF (9).
is run_cl('$! = 0; my $r = binmode(NOPE);
           printf "r=%s errno=%d\n", ($r ? "T" : "F"), ($!+0);'),
   "r=F errno=9\n", 'binmode(unopened) returns false and sets $! = EBADF';

# PerlIO::Layer->find must not crash, and reports known core layers.
is run_cl('print( (find PerlIO::Layer "perlio") ? "yes\n" : "no\n");'),
   "yes\n", 'find PerlIO::Layer "perlio" is true (no crash)';

is run_cl('print( (find PerlIO::Layer "nosuchlayer") ? "yes\n" : "no\n");'),
   "no\n", 'find PerlIO::Layer on an unknown layer is false';

# The whole program continues after the introspection call (no abort).
is run_cl('my $ok = find PerlIO::Layer "perlio";
           print "after=", ($ok ? 1 : 0), "\n";
           print "still running\n";'),
   "after=1\nstill running\n", 'program continues past PerlIO::Layer->find';

# Task #2777: a binmode that changes the discipline used to dup the descriptor,
# build a new stream, CLOSE the original and re-point only the ONE variable it
# was given -- every other name for the handle (a sub's copy of its argument,
# `my $g = $fh`, a hash element, an object's slot) was left on a closed stream
# and its output was lost.  The stream is now re-formatted IN PLACE.  The
# programs are the s510 review's probes; expected text probed on perl 5.40.3.

is run_cl(<<'PL'),
my $T = "/tmp/pcl-s510b-b.$$"; unlink $T; sub w { my ($h) = @_; binmode($h, ":utf8"); print $h "caf\x{e9}\n"; } open(my $fh, ">", $T) or die; w($fh); close($fh); print "v1 after close: ", (-s $T), "\n";
PL
   "v1 after close: 6\n",
   'binmode through a sub\'s COPY of the handle: the caller\'s close flushes it (#2777)';

is run_cl(<<'PL'),
my $T = "/tmp/pcl-s510b-b.$$"; unlink $T; sub w { my ($h) = @_; binmode($h, ":utf8"); print $h "caf\x{e9}\n"; } open(my $fh, ">", $T) or die; w($fh); print "v2 no close, in-process: ", (-s $T) // "undef", "\n";
PL
   "v2 no close, in-process: 0\n",
   'binmode through a sub\'s copy: nothing reaches the file before close (#2777)';

is run_cl(<<'PL'),
my $T = "/tmp/pcl-s510b-b.$$"; unlink $T; sub w { my ($h) = @_; binmode($h); print $h "abc\n"; } open(my $fh, ">", $T) or die; w($fh); close($fh); print "v3 plain binmode: ", (-s $T), "\n";
PL
   "v3 plain binmode: 4\n",
   'plain binmode through a sub\'s copy keeps the caller\'s handle (#2777)';

is run_cl(<<'PL'),
my $T = "/tmp/pcl-s510b-b.$$"; unlink $T; sub w { binmode($_[0], ":utf8"); print {$_[0]} "caf\x{e9}\n"; } open(my $fh, ">", $T) or die; w($fh); close($fh); print "v4 via \$_[0]: ", (-s $T), "\n";
PL
   "v4 via \$_[0]: 6\n",
   'binmode through the alias $_[0] (#2777)';

is run_cl(<<'PL'),
my $T = "/tmp/pcl-s510b-b.$$"; unlink $T; open(my $fh, ">", $T) or die; my $copy = $fh; binmode($copy, ":utf8"); print $copy "caf\x{e9}\n"; print $fh "tail\n"; close($fh); print "v5 copy in same scope: ", (-s $T), "\n";
PL
   "v5 copy in same scope: 11\n",
   'binmode through a same-scope copy: the original\'s later print is kept (#2777)';

is run_cl(<<'PL'),
my $tmp = "/tmp/pcl-s510b-u.$$";
sub slurp_raw { open(my $in, "<", $tmp) or die; local $/; my $t = <$in>; close $in; defined $t ? length($t) . ":" . join(" ", map { sprintf "%02x", ord } split //, $t) : "undef" }
open(my $fh, ">", $tmp) or die; binmode($fh, ":utf8"); print $fh "\x{263A}", "e\x{301}", "\x{1F600}"; close $fh;
print "A size=", (-s $tmp), " raw=", slurp_raw(), "\n";
open($fh, ">", $tmp) or die; binmode($fh, ":utf8"); print $fh "\x{263A}"; close $fh;
print "B size=", (-s $tmp), " raw=", slurp_raw(), "\n";
open($fh, ">:utf8", $tmp) or die; print $fh "\x{263A}", "x"; close $fh;
print "C size=", (-s $tmp), " raw=", slurp_raw(), "\n";
open($fh, ">", $tmp) or die; binmode($fh, ":utf8"); { local ($,, $\) = (undef, undef); print $fh "\x{263A}", "x"; } close $fh;
print "D size=", (-s $tmp), " raw=", slurp_raw(), "\n";
sub cap { my ($code) = @_; open(my $h, ">", $tmp) or die; $code->($h); close $h; -s $tmp }
print "E size=", cap(sub { my $h = shift; binmode($h, ":utf8"); print $h "\x{263A}", "x" }), "\n";
print "F size=", cap(sub { my $h = shift; binmode($h, ":utf8"); print $h "\x{1F600}" }), "\n";
print "G size=", cap(sub { my $h = shift; binmode($h, ":utf8"); print $h "e\x{301}" }), "\n";
unlink $tmp;
PL
   "A size=10 raw=10:e2 98 ba 65 cc 81 f0 9f 98 80\n"
 . "B size=3 raw=3:e2 98 ba\n"
 . "C size=4 raw=4:e2 98 ba 78\n"
 . "D size=4 raw=4:e2 98 ba 78\n"
 . "E size=4\n"
 . "F size=4\n"
 . "G size=3\n",
   'binmode(:utf8) through a sub\'s copy: each size as perl (#2777)';

is run_cl(<<'PL'),
my $T = "/tmp/pcl-s510b-n.$$";
sub hexof { join " ", map { sprintf "%02x", ord } split //, $_[0] }
sub slurp { open(my $i, "<:raw", $T) or die; local $/; my $t = <$i>; close $i; $t }
my %h; my @a; my $c; my $o;
for my $n (qw(hash array closure object globref ioref)) {
  open(my $fh, ">", $T) or die; open(FH, ">", $T) or die if $n =~ /ref/;
  my $g = $n eq "hash" ? do { $h{k} = $fh; $h{k} } : $n eq "array" ? do { @a = ($fh); $a[0] }
        : $n eq "closure" ? do { $c = sub { $fh }; $c->() } : $n eq "object" ? do { $o = bless { fh => $fh }, "H"; $o->{fh} }
        : $n eq "globref" ? \*FH : *FH{IO};
  my $orig = $n =~ /ref/ ? \*FH : $fh;
  binmode($g, ":encoding(UTF-8)"); print $g "\x{e9}"; print $orig "t"; close $orig; close $fh;
  print "$n=", hexof(slurp()), "\n";
}
open(my $w, ">:raw", $T) or die; print $w "a\n\xc3\xa9\n\xc3\xa9\n"; close $w;
open(my $in, "<", $T) or die; my $gi = $in; my $l1 = <$in>; binmode($gi, ":utf8"); my $l2 = <$gi>; binmode($gi); my $l3 = <$in>;
printf "in: l2=[%s] l3=[%s] tell=%d .=%d\n", hexof($l2), hexof($l3), tell($in), $.;
close $in;
{ open(my $fh, ">", $T) or die; my $g = $fh; select((select($fh), $| = 1)[0]); binmode($g, ":utf8"); print $fh "x"; print "autoflush kept: ", (-s $T), "\n"; binmode($g, ":utf8"); print $g "y"; close $fh; print "twice: ", hexof(slurp()), "\n"; }
{ open(my $p, "|-", "cat > $T; exit 4") or die; my $g = $p; binmode($g, ":utf8"); print $g "\x{e9}"; print $p "z"; close $p; printf "pipe close status=%d bytes=%s\n", $? >> 8, hexof(slurp()); }
{ open(my $fh, ">", $T) or die; open(my $dup, ">&", $fh) or die; binmode($dup, ":utf8"); print $fh "t"; close $fh; print $dup "\x{e9}"; close $dup; print "dup stays separate: ", hexof(slurp()), "\n"; }
unlink $T;
PL
   "hash=c3 a9 74\n"
 . "array=c3 a9 74\n"
 . "closure=c3 a9 74\n"
 . "object=c3 a9 74\n"
 . "globref=c3 a9 74\n"
 . "ioref=c3 a9 74\n"
 . "in: l2=[e9 0a] l3=[c3 a9 0a] tell=8 .=3\n"
 . "autoflush kept: 1\n"
 . "twice: 78 79\n"
 . "pipe close status=4 bytes=c3 a9 7a\n"
 . "dup stays separate: 74 c3 a9\n",
   'every second name for a handle sees one binmode; $|, $., tell, pipe status, dup stays separate (#2777)';

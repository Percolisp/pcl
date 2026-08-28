#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# print-fh-magic-01.t — task #466: a MAGIC scalar in the FILEHANDLE slot of
# print/printf/say.
#
#     local $_ = \*STDOUT;  print $_ "x\n";      # perl: x     PCL: DROPPED
#     my $fh   = \*STDOUT;  print $fh "x\n";     # perl: x     PCL: x
#
# perl's grammar is `listop: LSTOP indirob listexpr` with
# `indirob: WORD | scalar | block`, and `scalar` is ANY scalar variable — the
# punctuation, digit and caret spellings included.  PPI hands those over as
# PPI::Token::Magic, which IS a subclass of PPI::Token::Symbol, and PCL's three
# filehandle-slot tests were exact-class `ref($t) eq 'PPI::Token::Symbol'`: they
# answered "not a scalar", no filehandle was extracted, and the leftover
# `$_ "x\n"` run had two terms with no operator between them — so the WHOLE
# statement was dropped ("Bug. Fell through. Missing case").  One predicate,
# `Pl::PExpr::_is_scalar_fh_token`, now answers for all three sites (the
# operator-loop path, the `print $fh -e $f` filetest repair, the paren form).
#
# Every expectation below is the live `perl` answer (probed s441b, 5.40.3).
#
# THE NEGATIVES ARE THE POINT: widening the slot must not turn an ordinary
# `print $_ ...` into a filehandle write.  The second half of the decision —
# "does what follows START A TERM?" (`_is_print_term_start`) — is unchanged and
# is what keeps `print $_ . "\n"`, `print $_, "\n"`, `print $_ x 2` etc. reading
# $_ as the argument.  Rows 6..9 assert exactly that, and they are the rows that
# fail if the predicate is ever widened past `$`-sigil scalars.
#
# The two residues this file used to record as unfixed are FIXED — s446i, tasks
# #512 and #513, sections 7 and 8 below:
#   * `$0` is a WRITABLE box now (it was a bare CL string, and a bare string is
#     not a place, so `$0 = "X"` was a silent no-op and every reader saw the
#     SBCL binary's name), and pl2cl's preamble initialises it to the script it
#     was given, which is what perl puts there.  That makes the symbolic-handle
#     spelling `$0 = "H"; print $0 "x"` reach the handle named H, which is what
#     this file's predicate was widened for.
#   * `open($d, ">&", \*STDOUT)` — perldoc's own spelling — failed EBADF in
#     every glob-ref/lexical-handle form, because %p-open-impl stringified the
#     third argument before the dup path saw it and a glob ref stringifies to
#     "GLOB(0x…)".  The raw value now goes along, and %p-resolve-fh (THE
#     filehandle resolver, which already knew every one of those shapes)
#     answers.
#
# STILL divergent, pre-existing, filed: the INTERLEAVING of writes through a
# dup and through the source when both go to a pipe — perl block-buffers a
# non-tty STDOUT and PCL line-buffers it, so `print $dup "a"; print "b"` comes
# out in a different order.  Nothing here asserts stdout ordering across two
# handles; the dup rows write to a FILE and read it back.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile tempdir);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 29;

my $dir = tempdir(CLEANUP => 1);
my $FIX = qq{my \$O = "$dir/out.txt";\n};
my $FIXD = qq{my \$D = "$dir";\n};

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

# Transpile (a DROPPED statement fails the row, via PCLCore) and run.
sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;
    return $output;
}

sub emitted { return PCLCore::transpile("$pl2cl " . write_pl($_[0])) }

# ── 1. $_ in the slot really WRITES TO THE HANDLE ────────────────────────────
# The write goes to a file, so a row that merely printed to stdout (the
# pre-fix reading, had it parsed at all) cannot pass by accident.
is(run_cl($FIX . <<'PL'), "file:[to-the-handle\n]\n",
open(my $fh, '>', $O) or die "open: $!";
local $_ = $fh;
print $_ "to-the-handle\n";
close $fh;
open(my $in, '<', $O) or die "reopen: $!";
my $got = do { local $/; <$in> };
close $in;
print "file:[$got]\n";
PL
   '`print $_ LIST` writes to the handle $_ holds');

is(run_cl($FIX . <<'PL'), "file:[pf-42\n]\n",
open(my $fh, '>', $O) or die "open: $!";
local $_ = $fh;
printf $_ "pf-%d\n", 42;
close $fh;
open(my $in, '<', $O) or die "reopen: $!";
my $got = do { local $/; <$in> };
close $in;
print "file:[$got]\n";
PL
   '`printf $_ FMT, LIST` writes to the handle $_ holds');

is(run_cl($FIX . <<'PL'), "file:[said\n]\n",
use feature 'say';
open(my $fh, '>', $O) or die "open: $!";
local $_ = $fh;
say $_ "said";
close $fh;
open(my $in, '<', $O) or die "reopen: $!";
my $got = do { local $/; <$in> };
close $in;
print "file:[$got]\n";
PL
   '`say $_ LIST` writes to the handle $_ holds');

# ── 2. the other parse paths the same input can take ─────────────────────────
# The paren form is a SECOND site (_extract_paren_filehandle) and the block form
# a THIRD; the block form always worked and must keep working.
is(run_cl(<<'PL'), "paren\nblock\n",
local $_ = \*STDOUT;
print($_ "paren\n");
print {$_} "block\n";
PL
   'the paren form and the block form both take $_ as the handle');

# ── 3. other magic spellings are scalars too (perl: `indirob: scalar`) ───────
# `$,` is the output field separator; holding a handle in it is absurd Perl and
# is exactly why it is here — the rule is the SIGIL, not the name.
is(run_cl(<<'PL'), "comma-var\ncaret-var\n",
local $, = \*STDOUT;
print $, "comma-var\n";
local ${^MYHANDLE} = \*STDOUT;
print ${^MYHANDLE} "caret-var\n";
PL
   'a punctuation or caret scalar in the slot is a handle, like any scalar');

# ── 4. the wild shape (Test::Builder::NoOutput lines 118/125) ────────────────
is(run_cl(<<'PL'), "one\none\ntwo\ntwo\n",
sub emit { my $self = shift; print $_ @_ for @$self }
sub emitf { my $self = shift; printf $_ @_ for @$self }
my $two = [\*STDOUT, \*STDOUT];
emit($two, "one\n");
emitf($two, "%s\n", "two");
PL
   '`print $_ @_ for @$self` — the shape this task was minimised from');

# ── 5. THE NEGATIVES: an ordinary `print $_ …` must stay an ARGUMENT ─────────
is(run_cl(<<'PL'), "N\nO,\nP\n4\nUU\nV\nyes\nm\n",
local $_ = "N"; print $_ . "\n";
local $_ = "O"; print $_, ",\n";
local $_ = "P"; print $_; print "\n";
local $_ = 3;   print $_ + 1, "\n";
local $_ = "U"; print $_ x 2, "\n";
local $_ = "V"; print uc $_, "\n";
local $_ = "T"; print $_ ? "yes\n" : "no\n";
local $_ = "W"; print $_ =~ /W/ ? "m\n" : "n\n";
PL
   'an operator, a comma or end-of-statement after $_ keeps it an argument');

is(run_cl(<<'PL'), "QR\nab\n[S]\n",
my @l = ("Q","R"); print $_ for @l; print "\n";
my @l2 = ("a","b"); for (@l2) { print $_ } print "\n";
my @l3 = ("S"); print "[$_]" for @l3; print "\n";
PL
   'the `for` modifier and a foreach body keep `print $_` printing $_');

is(run_cl(<<'PL'), "k\n0\n",
my %h = (x => "k"); local $_ = \%h; print $_->{x}, "\n";
my @a = (0,1); local $_ = \@a; print $_->[0], "\n";
PL
   'a `->` chain after $_ keeps it an argument, not a handle');

# ── 6. transpile shape: the two readings are visibly different ───────────────
like(emitted(q{local $_ = \*STDOUT; print $_ "x\n";}),
     qr/\(p-print :fh \$_/,
     'the handle reading emits (p-print :fh $_ …)');

unlike(emitted(q{local $_ = "x"; print $_ . "\n";}),
       qr/:fh/,
       'the argument reading emits no :fh slot');

# ── 7. task #512 — $0 is an ordinary WRITABLE scalar ────────────────────────
# Every expectation below is the live `perl` answer (probed s446i, 5.40.3).
# Pre-fix each of these read the SBCL binary's name instead.
is(run_cl(<<'PL'), "w:renamed\ns:in-sub\nc:in-sub-x\nl:L\nr:in-sub-x\n",
$0 = "renamed";
print "w:$0\n";
sub inner { $0 = "in-sub"; return $0 }
print "s:", inner(), "\n";
$0 .= "-x";
print "c:$0\n";
{ local $0 = "L"; print "l:$0\n"; }
print "r:$0\n";
PL
   '$0 is writable: assignment, assignment inside a sub, .=, and local $0');

# …and its VALUE is the script pl2cl was given, exactly as perl reports it —
# so a program can open itself, and __FILE__ and $0 agree.  (write_pl's temp
# file ends in .pl and its first line is the one below.)
is(run_cl(<<'PL'), "script:ok\nselfread:ok\nfile-eq-0:ok\n",
print(($0 =~ /\.pl$/) ? "script:ok\n" : "script:no[$0]\n");
open(my $f, '<', $0) or die "self-open: $!\n";
my $first = <$f>; close $f;
print(($first =~ /^print\(/) ? "selfread:ok\n" : "selfread:no\n");
print((__FILE__ eq $0) ? "file-eq-0:ok\n" : "file-eq-0:no\n");
PL
   '$0 is the script pl2cl was given, so open($0) reads the program itself');

# THE CONSEQUENCE this file exists for: a writable $0 in the filehandle slot
# names a handle by STRING, and the write must reach that handle — proven by
# reading the FILE back, not by what appeared on stdout.
is(run_cl($FIX . <<'PL'), "file:[through-name\n]still:H\n",
open(H, '>', $O) or die "open: $!\n";
$0 = "H";
print $0 "through-name\n";
close H;
open(my $in, '<', $O) or die "reopen: $!\n";
my $got = do { local $/; <$in> }; close $in;
print "file:[$got]";
print "still:$0\n";
PL
   '`$0 = "H"; print $0 LIST` writes through the handle NAMED by $0');

# ── 8. task #513 — open FH, ">&", SRC (filehandle dup) ──────────────────────
# Both handles write to the same file and both writes must land; sorting the
# lines keeps the row independent of flush order (the one thing that still
# differs from perl — see the header).
is(run_cl($FIX . <<'PL'), "n:2\nl:through-dup\nl:through-orig\n",
open(my $out, '>', $O) or die "open: $!\n";
open(my $dup, ">&", $out) or die "dup: $!\n";
print $dup "through-dup\n";
print $out "through-orig\n";
close $dup; close $out;
open(my $in, '<', $O) or die "reopen: $!\n";
my @l = sort <$in>; close $in;
print "n:", scalar(@l), "\n";
print "l:$_" for @l;
PL
   'a dup of a lexical handle writes to the same file as its source');

# Every source spelling perl accepts.  The glob-ref form is perldoc's own and
# was the one that failed in PCL; the bareword TARGET and `<&` are here because
# they travel the same resolver.
is(run_cl(<<'PL'), "globref\ntwoarg\nname\nfd\nbareword\nin-ok\n",
open(my $d1, ">&", \*STDOUT) or die "glob-ref: $!\n";
print $d1 "globref\n"; close $d1;
open(my $d2, ">&STDOUT")    or die "two-arg: $!\n";
print $d2 "twoarg\n"; close $d2;
open(my $d3, ">&", "STDOUT") or die "name: $!\n";
print $d3 "name\n"; close $d3;
open(my $d4, ">&", 1)        or die "fd: $!\n";
print $d4 "fd\n"; close $d4;
open(D5, ">&", \*STDOUT)     or die "bareword: $!\n";
print D5 "bareword\n"; close D5;
open(my $d6, "<&", \*STDIN)  or die "in: $!\n";
close $d6;
print "in-ok\n";
PL
   'the dup source may be a glob ref, a two-arg name, a string name, an fd or STDIN');

is(run_cl(<<'PL'), "source-still-open\n",
open(my $dup, ">&", \*STDOUT) or die "dup: $!\n";
close $dup;
print "source-still-open\n";
PL
   'closing a dup leaves the source handle open');

# The failure shapes are perl's, and they are NOT the same shape: in the
# THREE-argument form a NAME that names no open handle is FATAL, a CLOSED
# lexical handle is a plain false.  (The two-argument form never dies at all —
# task #621, the row below this one.)
is(run_cl(<<'PL'), "ret:undef\nerr:ok\nclosed:0\nclosed-no-die\n",
my $r = eval { open(my $d, ">&", "NOSUCHHANDLE"); 1 };
print "ret:", (defined $r ? $r : "undef"), "\n";
print(($@ =~ /^Bad filehandle: NOSUCHHANDLE/) ? "err:ok\n" : "err:[$@]\n");
open(my $f, '<', "/etc/hostname") or die "o: $!\n"; close $f;
my $ok = eval { my $x = open(my $d2, ">&", $f); print "closed:", (defined $x && $x ? 1 : 0), "\n"; 1 };
print(($ok && $@ eq '') ? "closed-no-die\n" : "closed-died:[$@]\n");
PL
   'a 3-arg unknown source NAME dies "Bad filehandle: N"; a closed handle is a plain false');

# task #621 — the same designator in the TWO-argument spelling is NOT fatal.
# perl fails the open with $! = EINVAL and runs on; PCL p-died, which killed the
# whole program at an `open(my $x, "<&NOSUCH")` that perl merely returns undef
# from.  The discriminator is the argument FORM, not the designator kind
# (probed s449q, 5.40.3, over nineteen shapes): every unfindable two-argument
# source is EINVAL — an unknown name, an empty one, a package-qualified one, a
# lexical handle that stringified to "GLOB(0x…)" — while a bad fd NUMBER stays
# EBADF in BOTH forms.  The last two lines are the point of the row: the
# statements after the failed open still run.
is(run_cl(<<'PL'), "unknown:undef/22\nempty:undef/22\nqual:undef/22\nglob:undef/22\nfd99:undef/9\nalive\n",
sub shape { my ($tag, $r) = @_; printf "%s:%s/%d\n", $tag, (defined $r ? $r : "undef"), $!+0 }
open(my $good, '<', "/etc/hostname") or die "o: $!\n";
shape("unknown", open(my $a, "<&NOSUCHHANDLE"));
shape("empty",   open(my $b, "<&"));
shape("qual",    open(my $c, "<&main::NOSUCHHANDLE"));
shape("glob",    open(my $d, "<&$good"));
shape("fd99",    open(my $e, "<&99"));
close $good;
print "alive\n";
PL
   'a 2-arg dup from an unfindable source is undef+EINVAL, not a die (#621)');

# ── 9. task #543 — the READ-WRITE dup modes `+<&` / `+>&` and their `=` forms ─
# They were absent from the runtime's dup-mode list, so the three-argument
# spelling reached the `Unknown open mode` arm (warn + undef, perl-tests/
# scalar.t:84's shape) and the TWO-argument spelling was worse: "+>&SRC" was
# read as mode "+>" on a FILE literally named "&SRC", which it then CREATED.
# Every expectation below is the live `perl` answer (probed s448n, 5.40.3).
is(run_cl($FIX . <<'PL'), "+<&:hello\n+>&:hello\n+<&=:hello\n+>&=:hello\n",
open(my $c, '>', $O) or die "mk: $!\n"; print $c "hello\n"; close $c;
for my $m ('+<&', '+>&', '+<&=', '+>&=') {
    open(my $fh, '+<', $O) or die "src: $!\n";
    my $r = open(my $dup, $m, $fh);
    my $l = $r ? <$dup> : undef;
    print "$m:", (defined $l ? $l : "UNDEF\n");
    close $dup if $r;
    close $fh;
}
PL
   'all four read-write dup modes open, and the dup READS the source file');

# The `+` is not decoration: the dup must be WRITABLE too (the old two-way
# direction test read every `+` mode as `<`).  And a `+>&` dup does NOT
# truncate — the `>` spells a direction, not an open-for-write of a file.
is(run_cl($FIX . <<'PL'), "after-write:[XYllo\n]after-plusgt:[XYllo\n]",
open(my $c, '>', $O) or die "mk: $!\n"; print $c "hello\n"; close $c;
open(my $fh, '+<', $O) or die "src: $!\n";
open(my $dup, '+<&', $fh) or die "dup: $!\n";
print $dup "XY";
close $dup; close $fh;
open(my $in, '<', $O) or die "re: $!\n"; my $got = do { local $/; <$in> }; close $in;
print "after-write:[$got]";
open(my $f2, '+<', $O) or die "src2: $!\n";
open(my $d2, '+>&', $f2) or die "dup2: $!\n";
close $d2; close $f2;
open(my $i2, '<', $O) or die "re2: $!\n"; my $g2 = do { local $/; <$i2> }; close $i2;
print "after-plusgt:[$g2]";
PL
   'a `+<&` dup is WRITABLE, and `+>&` does not truncate the file');

# The two-argument spellings, and the stray-file check that is the silent-wrong
# this closes: `stray:00` says no file named "&SRC" / "&SRC2" was created.
is(run_cl($FIXD . <<'PL'), "d1:hello\nd2:hello\nstray:00\n",
chdir $D or die "chdir: $!\n";
open(my $c, '>', "src.txt") or die "mk: $!\n"; print $c "hello\n"; close $c;
open(SRC, '+<', "src.txt") or die "src: $!\n";
open(my $d1, '+<&SRC') or die "2arg-lt: $!\n";
my $l1 = <$d1>; close $d1;
open(SRC2, '+<', "src.txt") or die "src2: $!\n";
open(my $d2, '+>&SRC2') or die "2arg-gt: $!\n";
my $l2 = <$d2>; close $d2;
close SRC; close SRC2;
print "d1:", (defined $l1 ? $l1 : "UNDEF\n");
print "d2:", (defined $l2 ? $l2 : "UNDEF\n");
print "stray:", (-e "&SRC" ? 1 : 0), (-e "&SRC2" ? 1 : 0), "\n";
PL
   'two-arg "+<&SRC" / "+>&SRC" are DUPS, not opens of a file named "&SRC"');

# ── 10. task #591 — the dup starts where the PROGRAM is, not where the buffer
# left the descriptor.  A read handle that consumed one line has already pulled
# the whole file into its buffer, so the descriptor sat at EOF and the dup read
# undef; a write handle holds text the descriptor has never seen, so the dup's
# writes reached the file FIRST.  %p-sync-fd-position is the one flush, and its
# CONSEQUENCE is asserted too: perl's source handle reads undef afterwards,
# because its buffer went with the flush.  Probed s449q, 5.40.3.
is(run_cl($FIX . <<'PL'), "first:[aaa\n]dup:[bbb\n]src:[UNDEF]\n",
open(my $c, '>', $O) or die "mk: $!\n"; print $c "aaa\nbbb\nccc\n"; close $c;
open(my $s, '<', $O) or die "src: $!\n";
my $first = <$s>;
open(my $d, '<&', $s) or die "dup: $!\n";
my $next = <$d>;
my $after = <$s>;
close $d; close $s;
print "first:[$first]dup:[$next]src:[", (defined $after ? $after : "UNDEF"), "]\n";
PL
   'a `<&` dup continues at the source handle\'s logical position');

is(run_cl($FIX . <<'PL'), "order:[one\ntwo\n]\n",
open(my $o, '>', $O) or die "mk: $!\n";
print $o "one\n";
open(my $od, '>&', $o) or die "dup: $!\n";
print $od "two\n";
close $od; close $o;
open(my $in, '<', $O) or die "re: $!\n"; my $got = do { local $/; <$in> }; close $in;
print "order:[$got]\n";
PL
   'a buffered write source is FLUSHED before the dup, so program order holds');

# The negative: an UNSEEKABLE source (a pipe) has no position to sync, and perl
# answers undef there because its buffer swallowed the rest.  The flush must
# leave it alone rather than dying — file-position answers nil both ways.
is(run_cl(<<'PL'), "first:[hi\n]open:1 next:[UNDEF]\n",
open(my $p, '-|', "echo hi; echo there") or die "pipe: $!\n";
my $l = <$p>;
my $ok = open(my $pd, '<&', $p);
my $n = $ok ? <$pd> : undef;
close $pd if $ok; close $p;
print "first:[$l]open:", ($ok ? 1 : 0), " next:[", (defined $n ? $n : "UNDEF"), "]\n";
PL
   'a dup of an UNSEEKABLE source (a pipe) is left alone, as in perl');

# ── 11. task #590 — a write the OS refuses is a FALSE print, not a condition ──
# `print $dup "ZZ"` on a dup of a read-only descriptor reported SUCCESS (SBCL
# buffered it) and the EBADF surfaced at the CLOSE as an unhandled stream error
# that killed the whole program.  Two halves, and both are needed: the direction
# of a dup stream is perl's model (read iff the DESCRIPTOR is readable — the
# mode letter does not gate it; write iff the MODE asks AND the descriptor
# allows), and any write error that does escape becomes perl's false + $!.
# Every expectation is the live `perl` answer, probed s449q 5.40.3.
#
# THE DIRECTION TABLE is the row that pins the model, negatives included: the
# `>&` dup of a read-only fd still READS (perl does), and the `<&` dup of a
# write-only fd does neither.
is(run_cl($FIX . <<'PL'), "1:r=hello\n1:w=undef\n2:r=UNDEF\n2:w=undef\n3:r=hello\n3:w=undef\n4:r=hello\n4:w=1\n5:w=undef\n5:r=hello\n",
open(my $c, '>', $O) or die "mk: $!\n"; print $c "hello\nworld\n"; close $c;
sub sh { my ($t,$v) = @_; print "$t=", (defined $v ? ($v ? $v : "F") : "undef"), (($t =~ /r$/) ? "" : "\n") }
sub shr { my ($t,$v) = @_; print "$t=", (defined $v ? $v : "UNDEF\n") }
open(my $ro, '<', $O) or die; open(my $d1, '>&', $ro) or die "d1: $!\n";
shr("1:r", scalar <$d1>); sh("1:w", print($d1 "z")); close $d1; close $ro;
open(my $wo, '>>', $O) or die; open(my $d2, '<&', $wo) or die "d2: $!\n";
shr("2:r", scalar <$d2>); sh("2:w", print($d2 "z")); close $d2; close $wo;
open(my $r3, '<', $O) or die; open(my $d3, '+<&', $r3) or die "d3: $!\n";
shr("3:r", scalar <$d3>); sh("3:w", print($d3 "z")); close $d3; close $r3;
open(my $rw, '+<', $O) or die; open(my $d4, '+<&', $rw) or die "d4: $!\n";
shr("4:r", scalar <$d4>); sh("4:w", print($d4 "Z")); close $d4; close $rw;
open(my $r5, '<', $O) or die;
sh("5:w", print($r5 "z")); shr("5:r", scalar <$r5>); close $r5;
PL
   'a dup reads iff its DESCRIPTOR can, and writes iff the mode AND the descriptor can');

# The task's own reproducer: the print is false, $! is set, and — the point —
# the program SURVIVES the close, which used to abort it.
is(run_cl($FIX . <<'PL'), "open:1 print:F errno:9\nclose:1\nsource-reads:[hello\n]\nalive\n",
open(my $c, '>', $O) or die "mk: $!\n"; print $c "hello\n"; close $c;
open(my $ro, '<', $O) or die "ro: $!\n";
my $r = open(my $d, '>&', $ro);
$! = 0;
my $ok = print $d "ZZ";
print "open:", ($r?1:0), " print:", (defined $ok ? ($ok?1:"F") : "F"), " errno:", $!+0, "\n";
my $cl = close $d;
print "close:", ($cl?1:0), "\n";
print "source-reads:[", scalar(<$ro>), "]\n";
close $ro;
print "alive\n";
PL
   'a write to a dup of a READ-ONLY fd is false, and the close no longer aborts');

# An in-memory READ handle is the same question with no descriptor in it: PCL
# opened every `<` scalar handle read+write, so the print silently OVERWROTE the
# character the next read would have returned.
is(run_cl(<<'PL'), "print:F\nread:[abc\n]\nsrc:[abc\n]\n",
my $src = "abc\n";
open(my $mi, '<', \$src) or die "mem: $!\n";
my $ok = print $mi "x";
print "print:", (defined $ok ? ($ok?1:"F") : "F"), "\n";
print "read:[", scalar(<$mi>), "]\n";
close $mi;
print "src:[$src]\n";
PL
   'an in-memory READ handle refuses a print instead of overwriting its own buffer');

# The OTHER half of #590 — the write that is legitimately attempted and fails at
# the OS.  /dev/full is the only portable way to make one: a small write is
# BUFFERED, so the ENOSPC lands at the close, which is exactly the deferred
# failure that used to abort the program.  perl: the small print is TRUE (it
# only buffered), the big one is undef, and both closes are false.
# The expectation adapts to a machine without /dev/full, and the CL side prints
# the same token, so the row still runs there.
{
    my $have = (-w "/dev/full") ? 1 : 0;
    my $want = $have ? "big:F close:F\nsmall:1 close:F\nalive\n"
                     : "skip\nskip\nalive\n";
    is(run_cl(<<'PL'), $want,
if (-w "/dev/full" and open(my $fu, '>', "/dev/full")) {
    my $p = print $fu ("x" x 200000);
    my $c = close $fu;
    print "big:", (defined $p ? ($p?1:"F") : "F"), " close:", ($c?1:"F"), "\n";
    open(my $f2, '>', "/dev/full") or die "full2: $!\n";
    my $p2 = print $f2 "tiny";
    my $c2 = close $f2;
    print "small:", (defined $p2 ? ($p2?1:"F") : "F"), " close:", ($c2?1:"F"), "\n";
} else {
    print "skip\nskip\n";
}
print "alive\n";
PL
       'a write that fails at the OS is a false print and a false close, never a crash');
}

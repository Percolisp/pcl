#!/usr/bin/env perl
# Tests for lexical filehandle I/O (open/read/write/close/eof with $fh).

use v5.30;
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
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    my $cl_code = `$pl2cl $pl_file 2>&1`;

    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    my $output = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
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

# Create a temp file with known content, return its path.
# The path is embedded as a literal string in the generated Perl code.
sub make_temp_data_file {
    my (@lines) = @_;
    my ($fh, $path) = tempfile(SUFFIX => '.txt', UNLINK => 1);
    print $fh join('', @lines);
    close $fh;
    return $path;
}

sub test_io {
    my ($name, $code) = @_;
    my ($fh, $file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $perl_out = `perl $file 2>&1`;
    my $cl_out   = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL:   $cl_out");
}

plan tests => 29;

# --- Test 1: Bareword write + read (baseline) ---
{
    my $tmpfile = make_temp_data_file();
    test_io('bareword fh: write and read back', <<"PERL");
open(F, '>', '$tmpfile') or die "open: \$!";
print F "line1\n";
print F "line2\n";
close F;
open(F, '<', '$tmpfile') or die "open: \$!";
while (my \$line = <F>) {
    print \$line;
}
close F;
PERL
}

# --- Test 2: Lexical \$fh write + read ---
{
    my $tmpfile = make_temp_data_file();
    test_io('lexical fh: write and read back', <<"PERL");
open(my \$fh, '>', '$tmpfile') or die "open: \$!";
print \$fh "hello\n";
print \$fh "world\n";
close \$fh;
open(my \$fh2, '<', '$tmpfile') or die "open: \$!";
while (my \$line = <\$fh2>) {
    print \$line;
}
close \$fh2;
PERL
}

# --- Test 3: eof on lexical handle ---
{
    my $tmpfile = make_temp_data_file("alpha\n", "beta\n", "gamma\n");
    test_io('lexical fh: eof detection in while loop', <<"PERL");
open(my \$fh, '<', '$tmpfile') or die;
my \@lines;
while (!eof(\$fh)) {
    my \$line = <\$fh>;
    chomp \$line;
    push \@lines, \$line;
}
close \$fh;
print join(',', \@lines), "\n";
PERL
}

# --- Test 4: <\$var> where \$var is undef (not a filehandle) ---
test_io('readline on undef handle returns undef gracefully', <<'PERL');
no warnings;
my $fh;   # not opened
my $result = readline($fh);
if (!defined($result)) {
    print "undef\n";
} else {
    print "got: $result\n";
}
PERL

# --- Test 5: Read all lines into array from lexical handle ---
{
    my $tmpfile = make_temp_data_file("one\n", "two\n", "three\n");
    test_io('lexical fh: read all lines into array via while loop', <<"PERL");
open(my \$fh, '<', '$tmpfile') or die;
my \@lines;
while (my \$line = <\$fh>) {
    chomp \$line;
    push \@lines, \$line;
}
close \$fh;
print scalar(\@lines), "\\n";
print join(':', \@lines), "\\n";
PERL
}

# --- Test 6: tell/seek on lexical handle ---
{
    my $tmpfile = make_temp_data_file("ABCDE\nFGHIJ\n");
    test_io('lexical fh: tell and seek', <<"PERL");
open(my \$fh, '<', '$tmpfile') or die;
my \$line1 = <\$fh>;
my \$pos = tell(\$fh);
my \$line2 = <\$fh>;
seek(\$fh, \$pos, 0);
my \$line2again = <\$fh>;
close \$fh;
chomp \$line1; chomp \$line2; chomp \$line2again;
print "\$line1\n";
print "\$line2\n";
print "\$line2again\n";
print \$line2 eq \$line2again ? "same\n" : "different\n";
PERL
}

# --- Test 7: binmode on lexical handle (must not crash) ---
{
    my $tmpfile = make_temp_data_file("data\n");
    test_io('lexical fh: binmode does not crash', <<"PERL");
open(my \$fh, '<', '$tmpfile') or die;
binmode(\$fh);
my \$line = <\$fh>;
close \$fh;
chomp \$line;
print "\$line\n";
PERL
}

# --- Test 8: lexical fh with 2-arg open ---
{
    my $tmpfile = make_temp_data_file("twoline\n");
    test_io('lexical fh: 2-arg open for reading', <<"PERL");
open(my \$fh, '<$tmpfile') or die;
my \$line = <\$fh>;
close \$fh;
chomp \$line;
print "\$line\n";
PERL
}

# --- Test 9: Reuse lexical \$fh variable for multiple opens ---
{
    my $tmpfile = make_temp_data_file("first\n");
    my $tmpfile2 = make_temp_data_file("second\n");
    test_io('lexical fh: reuse variable for second open', <<"PERL");
open(my \$fh, '<', '$tmpfile') or die;
my \$a = <\$fh>;
close \$fh;
open(\$fh, '<', '$tmpfile2') or die;
my \$b = <\$fh>;
close \$fh;
chomp \$a; chomp \$b;
print "\$a \$b\n";
PERL
}

# --- Test 10: pipe + syswrite + readline roundtrip ---
{
    test_io('pipe: syswrite then readline', <<'PERL');
my ($in, $out);
pipe $in, $out;
syswrite $out, "once\n";
my $line = readline $in;
print $line;
PERL
}

# --- Test 11: pipe + .= append of readline result ---
{
    test_io('pipe: $line .= readline', <<'PERL');
my $line = 'ascii';
my ($in, $out);
pipe $in, $out;
syswrite $out, "...\n";
$line .= readline $in;
print $line;
PERL
}

# --- Test 12: alarm fires $SIG{ALRM} and interrupts a blocking readline ---
{
    test_io('alarm interrupts blocking readline via $SIG{ALRM}', <<'PERL');
my ($in, $out);
pipe $in, $out;
my $timed_out = 0;
my $line = 'untouched';
eval {
    local $SIG{ALRM} = sub { $timed_out = 1; die "abort\n" };
    alarm 1;
    $line = readline $in;   # blocks forever; SIGALRM aborts it
    alarm 0;
};
print "timed_out=$timed_out line=$line\n";
PERL
}

# --- Test 13: system() sets $? to the child's wait status ---
# Differential vs. real perl.  $? packs the exit code in the high byte
# ($? >> 8) and the signal in the low byte.  (Not exercised elsewhere as a
# runtime test — magic-vars-01.t only checks $? string interpolation; magic.t's
# own $? rows shell out to a './perl' that doesn't exist outside Perl's tree.)
{
    test_io('system() exit code is reported via $? >> 8', <<'PERL');
system("/bin/sh", "-c", "exit 0");
print "zero: ", ($? >> 8), " raw ", $?, "\n";
system("/bin/sh", "-c", "exit 3");
print "three: ", ($? >> 8), " raw ", $?, "\n";
system("/bin/sh", "-c", "exit 42");
print "fortytwo: ", ($? >> 8), "\n";
PERL
}

# --- Test 14: paren-form print/printf/say with a leading filehandle ---
# print($fh LIST) puts the filehandle *inside* the parens (no comma before
# the first argument).  Both the no-paren and paren forms must agree.
{
    test_io('paren-form print/printf/say(FH ...) routes to the filehandle', <<'PERL');
use feature "say";
my $buf = '';
open(my $fh, '>', \$buf) or die "open: $!";
print($fh "alpha\n");
printf($fh "n=%d\n", 7);
say($fh "omega");
close $fh;
print $buf;
PERL
}

# --- Test 15: in-memory scalar handle seek/tell, zero-fill, +< read+write ---
# Exercises the PerlIO ":scalar" position semantics: tell() after open, a
# forward seek that zero-fills with NUL, overwrite-in-place, SEEK_END append,
# and a "+<" handle that both reads and writes the same scalar.
{
    test_io('in-memory scalar: seek/tell, zero-fill, overwrite, +< read+write', <<'PERL');
use Fcntl qw(SEEK_SET SEEK_END);
my $foo;
open(my $w, '>', \$foo) or die "open: $!";
print "tell0=", tell($w), "\n";
seek($w, 5, SEEK_SET);
print $w "x";
printf "len=%d zerofill=%d\n", length($foo), ($foo =~ /^\0{5}x$/ ? 1 : 0);
seek($w, 0, SEEK_SET);
print $w "AB";                       # overwrite first two bytes in place
(my $shown = $foo) =~ s/\0/./g;
print "overwrite=[$shown]\n";
close $w;

my $rw = "hello world";
open(my $rwh, '+<', \$rw) or die "open: $!";
my $first = <$rwh>;                  # read the whole record
seek($rwh, 0, SEEK_END);
print $rwh "!";                      # append at end
print "readback=[$first] rw=[$rw]\n";
close $rwh;
PERL
}

# --- Test 16: read(FH, BUF, LEN [, OFFSET]) writes BUF and returns the count ---
# Regression: read() used to ignore its buffer and return the read STRING (so
# $n got "hello" and $buf stayed empty).  It must fill BUF in place, return the
# byte count, NUL-pad/append at a positive OFFSET, and report EBADF on an
# unopened handle.
{
    test_io('read fills buffer, returns count, honours OFFSET', <<'PERL');
my $path = "/tmp/pcl_read_test_$$.dat";
open(my $w, '>', $path) or die "open: $!";
print $w "abcdef";
close $w;

open(my $in, '<', $path) or die "open: $!";
my $buf = "";
my $n = read($in, $buf, 3);
print "n=$n buf=[$buf]\n";
# read 2 more at offset 5 — chars 0..2 kept, gap NUL-filled to offset 5
my $n2 = read($in, $buf, 2, 5);
(my $shown = $buf) =~ s/\0/./g;
print "n2=$n2 buf2=[$shown]\n";
close $in;
unlink $path;
PERL
}

# --- Test 17: print/say with no list argument default to $_ ---
# Regression: bare `print;` / `say;` / `print STDOUT;` used to emit nothing.
{
    test_io('print/say with no args default to $_', <<'PERL');
use feature "say";
$_ = "deefolt";
say;
say STDOUT;
print;
print "\n";
print STDOUT;
print "\n";
PERL
}

# --- Test 18: print/say to an unopened handle fails with EBADF, prints nothing ---
# Regression: an unresolved filehandle silently fell through to STDOUT (CL's
# (princ x nil) writes to *standard-output*).  Now it must set $!=EBADF and emit
# nothing.  Also: a string/scalar holding a handle NAME ('STDOUT') resolves.
{
    test_io('print to unopened handle = EBADF + no output; named handle resolves', <<'PERL');
use feature "say";
no warnings 'unopened';
$! = 0;
print NOPE "should-not-appear\n";
print "ebadf=", ($! ? "yes" : "no"), "\n";
my $h = "STDOUT";
print $h "via-string-handle\n";   # scalar holding a handle name
say {"STDOUT"} "via-block-string"; # block string handle
PERL
}

# --- Test 19: open(FH, ">-") / open(FH, "<-") use STDOUT/STDIN ---
# The magic filename "-" is a standard stream (Perl dups it), not a file named
# "-".  Previously PCL created a literal file called "-".
{
    test_io('open(FH, ">-") writes to STDOUT', <<'PERL');
open(FOO, ">-") or die "open: $!";
print FOO "to-stdout-via-dash\n";
open(my $bar, ">-") or die "open: $!";
print $bar "to-stdout-via-dash-lexical\n";
PERL
}

# --- Test 20: $/ = \N — fixed-length record mode ---
# Regression: get-input-record-separator returned nil (slurp) for a ref, so
# $/ = \N read the whole file instead of N-char records.
{
    test_io('$/ = \N reads fixed-length records', <<'PERL');
open(my $w, '>', "/tmp/pcl_rec_$$.dat") or die; print $w "abcdefgh"; close $w;
open(my $in, '<', "/tmp/pcl_rec_$$.dat") or die;
local $/ = \3;
my @recs;
while (my $r = <$in>) { push @recs, $r; }
print "n=", scalar(@recs), " recs=", join('|', @recs), "\n";
close $in;
unlink "/tmp/pcl_rec_$$.dat";
PERL
}

# --- Test 21: paragraph mode keeps the file's final newline state ---
# Regression: paragraph mode ($/ = "") always appended a trailing \n at EOF,
# inventing one for a file whose last line had none.
{
    test_io('paragraph mode does not invent a trailing newline at EOF', <<'PERL');
open(my $w, '>', "/tmp/pcl_para_$$.dat") or die;
print $w "a1\na2\n\nb1\nb2";   # last record "b1\nb2" has NO trailing newline
close $w;
open(my $in, '<', "/tmp/pcl_para_$$.dat") or die;
local $/ = "";
while (my $r = <$in>) {
    (my $s = $r) =~ s/\n/N/g;
    print "rec=[$s] len=", length($r), "\n";
}
close $in;
unlink "/tmp/pcl_para_$$.dat";
PERL
}

# --- Test 22: glob filehandle aliasing (*FH = $glob) ---
# Regression: p-glob-copy copied CODE/SCALAR/ARRAY/HASH slots but not the IO
# (filehandle) slot, so `*FH = shift` (a passed *HANDLE glob) then <FH> read
# nothing.  Used by base/rs.t's test_string/test_record helpers.
{
    test_io('glob assignment aliases the filehandle (*FH = $glob)', <<'PERL');
open(SRC, '>', "/tmp/pcl_glob_$$.dat") or die;
print SRC "line1\nline2\nline3\n";
close SRC;
open(SRC, '<', "/tmp/pcl_glob_$$.dat") or die;
sub read_two { *FH = shift; my $a = <FH>; my $b = <FH>; return "$a$b"; }
print "got: ", read_two(*SRC);
close SRC;
unlink "/tmp/pcl_glob_$$.dat";
PERL
}

# --- Test 23: symbolic filehandle — open($scalar) where $scalar holds a NAME ---
# `$TST = "TST"; open($TST, ...)` opens the *named* glob (*TST) and leaves $TST
# holding the string — it does NOT autovivify a lexical handle.  Both the
# bareword form (<TST>, eof(TST)) and the scalar form (<$TST>) reach the handle.
# (tell.t / old-style perl idiom.)
{
    test_io('open($s) with $s holding a NAME opens the symbolic glob', <<'PERL');
my $path = "/tmp/pcl_sym_$$.dat";
open(my $w, '>', $path) or die; print $w "aaa\nbbb\n"; close $w;
$TST = "TST";
open($TST, '<', $path) or die "open: $!";
print "still-string=", ($TST eq "TST" ? 1 : 0), "\n";
print "eof-fresh=", (eof(TST) ? 1 : 0), "\n";        # 0 — file is non-empty
my $l1 = <TST>;                                       # via bareword
my $l2 = <$TST>;                                      # via the scalar name
print "l1=$l1l2=$l2";
print "eof-end=", (eof(TST) ? 1 : 0), "\n";          # 1 — both lines consumed
close TST;
unlink $path;
PERL
}

# --- Test 24: argument-less eof tests the LAST filehandle read ---
# Bare `eof` (and `eof` inside `while (<FH>)`) refers to the last file read, not
# STDIN.  Regression: %p-eof-impl used *standard-input* for the no-arg form.
{
    test_io('argument-less eof tests the last handle read', <<'PERL');
my $path = "/tmp/pcl_eof_$$.dat";
open(my $w, '>', $path) or die; print $w "x\ny\nz\n"; close $w;
open(IN, '<', $path) or die;
my $n = 0;
while (<IN>) { $n++ if eof; }    # eof (no arg) = eof(IN)
print "eofs-seen=$n\n";          # exactly 1, on the last line
close IN;
unlink $path;
PERL
}

# --- Test 25: bareword FH whose open went through the scalar form still reads ---
# Mixed/lower-case names round-trip: open($h="Log123", ...) then <Log123> must
# resolve the SAME handle (the funcall-wrapped bareword case-recovery in
# %p-fh-arg).
{
    test_io('symbolic-open then bareword read agree on the handle name', <<'PERL');
my $path = "/tmp/pcl_name_$$.dat";
open(my $w, '>', $path) or die; print $w "one\ntwo\n"; close $w;
my $h = "Log123";
open($h, '<', $path) or die "open: $!";
print "a=", scalar(<Log123>);
print "b=", scalar(<Log123>);
close Log123;
unlink $path;
PERL
}

# --- Test 26: umask round-trips and returns the previous mask ---
{
    test_io('umask sets and returns the previous mask', <<'PERL');
my $old = umask(0022);
printf "set=%04o prev_is_num=%d cur=%04o\n", umask(), ($old =~ /^\d+$/ ? 1 : 0), umask();
umask($old);
PERL
}

# --- Test 27: link / symlink / readlink ---
{
    test_io('link, symlink and readlink', <<'PERL');
my $f = "/tmp/pcl_lnk_$$"; my $hard = "$f.hard"; my $sym = "$f.sym";
open(my $w, '>', $f) or die; print $w "data\n"; close $w;
print "link=",    (link($f, $hard)    ? 1 : 0), "\n";
print "symlink=", (symlink($f, $sym)  ? 1 : 0), "\n";
# Compare to $f (not the raw path) — the path embeds $$, which differs between
# the perl and pcl processes the harness runs.
print "readlink-ok=", (readlink($sym) eq $f ? 1 : 0), "\n";
print "hard-content=", do { open(my $r, '<', $hard); my $l = <$r>; close $r; $l };
unlink $f, $hard, $sym;
PERL
}

# --- Test 28: stat reports real fields (size, Unix-epoch atime/mtime) ---
# Regression: stat used CL's 1900-epoch file-write-date for both atime and
# mtime (off by 2208988800 and identical), and stubbed inode/size/mode.
{
    test_io('stat returns real size + Unix-epoch atime/mtime after utime', <<'PERL');
my $f = "/tmp/pcl_stat_$$";
open(my $w, '>', $f) or die; print $w "hello"; close $w;
utime(500000001, 500000002, $f);
my @s = stat($f);
printf "size=%d atime=%d mtime=%d ino_nonzero=%d nlink=%d\n",
       $s[7], $s[8], $s[9], ($s[1] ? 1 : 0), $s[3];
# a filehandle stats the same open file
open(my $r, '<', $f) or die; my @h = stat($r); close $r;
print "fh_size=$h[7]\n";
unlink $f;
PERL
}

# --- Test 29: truncate by name and by filehandle; bareword no path fallback ---
# A bareword filehandle that is not open must FAIL rather than truncate a file
# named after the bareword.
{
    test_io('truncate(name) and truncate($fh) shrink the file', <<'PERL');
my $f = "/tmp/pcl_trunc_$$";
open(my $w, '>', $f) or die; print $w "x" x 200; close $w;
truncate($f, 5);
print "by_name=", -s $f, "\n";
open(my $rw, '+<', $f) or die; truncate($rw, 2); close $rw;
print "by_fh=", -s $f, "\n";
unlink $f;
PERL
}

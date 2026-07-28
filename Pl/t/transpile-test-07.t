#!/usr/bin/env perl
# Transpile tests part 7.  NEW TESTS GO HERE (or a future -08) — the
# BIGGEST test file bounds the parallel suite's wall time (one SBCL spawn
# per test), so start a new file instead of growing the current largest.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use PCLCore;

# Path to pl2cl and runtime
my $project_root = "$RealBin/../..";
my $pl2cl = "$project_root/pl2cl";
my $runtime = "$project_root/cl/pcl-runtime.lisp";
# Optional saved-core fast path (PCL_TEST_CORE=1); source-load otherwise.
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

# Check dependencies
plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

# Run a Perl snippet and return output
sub run_perl {
    my ($code) = @_;
    my $full_code = 'use feature "state"; use Cwd; ' . $code;
    # Shell-escape embedded single quotes ('…' -> '\''), or any tick in the
    # snippet truncates the -e arg.
    (my $sh_code = $full_code) =~ s/'/'\\''/g;
    my $output = `perl -e '$sh_code' 2>&1`;
    return $output;
}

# Transpile and run CL, return output
sub run_cl {
    my ($code) = @_;

    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;

    my $cl_code = `$pl2cl $pl_file 2>&1`;

    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;

    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;

    $output =~ s/^;.*\n//gm;
    $output =~ s/^caught .*\n//gm;
    $output =~ s/^compilation unit.*\n//gm;
    $output =~ s/^\s*Undefined.*\n//gm;
    $output =~ s/^-->.*\n//gm;
    $output =~ s/^==>.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    $output =~ s/^\s*\n//gm;

    return $output;
}

# Test helper: compare Perl and CL output
sub test_transpile {
    my ($name, $code) = @_;
    my $perl_out = run_perl($code);
    my $cl_out = run_cl($code);
    is($cl_out, $perl_out, $name) or diag("Perl: $perl_out\nCL: $cl_out");
}

# #25 suite family: high capture groups ($10+) and $^N.  $^N is the
# participating group with the rightmost CLOSING paren — the nested case
# is where it differs from $+ (highest-numbered opener).
test_transpile("capture groups past \$9", '
"abcdefghij" =~ /(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)/;
print "ten:$10:$+\n";
"xy" =~ /(x)(y)/;
print "stale:", (defined $10 ? "still" : "cleared"), "\n";
');

test_transpile("\$^N rightmost-closer rule incl. nested groups", '
"ab" =~ /(a(b))/;      print "nested:", $^N, "\n";
"ab" =~ /(a)(b)/;      print "flat:", $^N, "\n";
"b"  =~ /(?:(a)|(b))/; print "alt:$^N\n";
');

# Task #114 (range.t 162, RT #130841): a range whose byte size wraps
# size_t must croak in perl's "panic: memory wrap|Out of memory" family,
# not a PCL-specific overflow message.
test_transpile("huge range croak speaks perl's memory-wrap family", '
my $max_iv = (~0 >> 1);
eval { my @range = 1..($max_iv - 1); };
print "err:", (($@ =~ /panic: memory wrap|Out of memory/) ? "wrap-family" : "other:$@"), "\n";
');

# #25 suite family: the full filetest set (-l/-p/-u/-g/-k/-o/-O/-R/-W/-X/
# -T/-B/-M/-A/-C/-t...), plus the op_info case-sensitivity fix: the
# precedence lookup lowercased operators, so -M/-A (no lowercase twin in
# the table) fell through as parse errors while -C/-T/-S only worked by
# landing on the OTHER filetest's identical entry.
test_transpile("filetest family incl. -M/-A case-sensitive lookup", '
my $d = "/tmp/pcl-ft-$$";
mkdir $d;
open my $fh, ">", "$d/f.txt"; print $fh "hello\n"; close $fh;
symlink("f.txt", "$d/ln");
print "l:", (-l "$d/ln" ? 1 : 0), (-l "$d/f.txt" ? 1 : 0), "\n";
print "TB:", (-T "$d/f.txt" ? 1 : 0), (-B "$d/f.txt" ? 1 : 0), "\n";
print "own:", (-o "$d/f.txt" ? 1 : 0), (-O "$d/f.txt" ? 1 : 0), "\n";
print "age:", int(-M "$d/f.txt"), int(-A "$d/f.txt"), int(-C "$d/f.txt"), "\n";
print "miss:", (-l "$d/nosuch" ? 1 : 0), (-u "$d/nosuch" ? 1 : 0), "\n";
print "W:$^W\n";
unlink "$d/ln", "$d/f.txt"; rmdir $d;
');

# #25 suite family: perl's `Bareword::` package-name-string form —
# `Foo::` eq "Foo" in term position, as a method-call invocant, as a
# funcall argument (a separate parse route), as a hash key, and after a
# fat comma.  Normalized in cleanup_for_parsing (all parse routes) +
# _make_string_of_token_word (the autoquote paths).
test_transpile("Bareword:: package-name string form", '
package Foo { sub new { bless {}, shift } sub who { ref($_[0]) || $_[0] } }
print Foo::, "|", main::, "\n";
print Foo::->new->who, "|", Foo::->who, "\n";
sub takes { return $_[0] }
print takes(Foo::), "\n";
my %h = (Foo => 1, "Foo::" => 2);
print $h{Foo::}, "|";
my %g = (Foo:: => 5);
print join(",", keys %g), "\n";
');

# lock: a no-op on unthreaded perl (op/lock.t crashed on undefined pl-lock)
test_transpile("lock is a no-op that returns its argument", '
my @a = (1,2,3);
lock @a;
my $x = 5;
print lock($x) + 1, ":", scalar(@a), "\n";
');

# #25: real fileno (fd-stream backed), glob/glob-ref filehandle
# designators, and pure-perl POSIX::dup on top of them.
test_transpile("fileno returns real fds; glob designators; POSIX::dup", '
require POSIX;
print "std:", fileno(\*STDIN), fileno(*STDOUT), fileno(STDERR), "\n";
open my $fh, "<", "/etc/hostname" or die;
print "real:", (fileno($fh) > 2 ? "y" : "n"), "\n";
my $fd1 = POSIX::dup(fileno(\*STDOUT));
my $fd2 = POSIX::dup(fileno(\*STDOUT));
print "dup:", ($fd1 > 2 ? "y" : "n"), ($fd2 > $fd1 ? "y" : "n"), "\n";
');

# #25: passwd-database family (new) + the grent family's latent context
# bugs (wantarray was a &key nothing passed — list context never spread;
# scalar getgrnam returned the name where perl returns the GID).
test_transpile("getpw*/getgr* families are context-sensitive", '
my @e = getpwuid($<);
print "n:", scalar(@e), "|", ($e[2] == $< ? "uid-ok" : "uid-bad"), "\n";
my $name = getpwuid($<);
my $uid  = getpwnam($e[0]);
print "scalar:", ($name eq $e[0] ? "y" : "n"), ($uid == $< ? "y" : "n"), "\n";
setpwent();
my @first = getpwent();
my @second = getpwent();
print "ent:", scalar(@first), ":", ($first[0] ne $second[0] ? "advances" : "stuck"), "\n";
endpwent();
my @g = getgrnam("root");
my $gid = getgrnam("root");
print "gr:", scalar(@g), ":$gid\n";
');

done_testing();

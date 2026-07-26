#!/usr/bin/env perl
# bless-referent-01.t: scalar-ref blessing lives on the REFERENT (s314).
#
# Perl blesses the referent, not the reference.  PCL used to record the
# class only on the wrapper box, so a second \$x wrapper never saw the
# bless, a re-bless through one alias was invisible through another, and
# XS asking SvSTASH(SvRV(rv)) found nothing (the one pcl-conform failure).
# The referent box is now the source of truth (%p-referent-class); the
# wrapper/variable slots remain caches.  The copy-out rule is the flip
# side: copying a plain VALUE out of a blessed referent must NOT carry
# the class (perl's stash is attached to the SV, not the value).

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

plan skip_all => "pl2cl not found" if ! -x $pl2cl;
plan skip_all => "sbcl not found"  if ! `which sbcl 2>/dev/null`;

sub run_cl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    my $cl_code = `$pl2cl $pl_file 2>&1`;
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

plan tests => 11;

# One SBCL launch for the whole family: each line prints one answer.
my $prog = <<'EOF';
my $x = 42;
my $r = \$x;
bless $r, "A";
my $r2 = \$x;
print "t1:", ref($r2), "\n";
sub A::hi { return "hi-from-A" }
print "t2:", (eval { $r2->hi } // "DIED"), "\n";
bless $r2, "B";
print "t3:", ref($r), "\n";
$$r = "changed";
print "t4:", ref($r), "\n";
my $y = $$r;
print "t5:", (ref(\$y) eq 'SCALAR' ? 'plain' : ref(\$y)), "\n";
print "t6:", ref(\$x), "\n";
my $a1 = bless {}, "AA";
my $f = bless \$a1, "FF";
print "t7:", ref($a1), "\n";
print "t8:", ref(\$a1), "\n";
print "t9:", ("$a1" =~ /^AA=HASH/ ? "ok" : "$a1"), "\n";
bless $a1, "A2";
my $alias = $a1;
print "t10:", ref($alias), "\n";
my $plainstash = "v";
bless \$plainstash, "PS";
print "t11:", (ref($plainstash) eq "" ? "notref" : ref($plainstash)),
      ":", "$plainstash", "\n";
EOF

my $out = run_cl($prog);

like $out, qr/^t1:A$/m,
    'second \\$x wrapper sees the bless (referent carries the class)';
like $out, qr/^t2:hi-from-A$/m,
    'method call through an aliased ref dispatches';
like $out, qr/^t3:B$/m,
    're-bless through one alias is visible through the other';
like $out, qr/^t4:B$/m,
    'class survives a write through the ref (stash stays with the SV)';
like $out, qr/^t5:plain$/m,
    'copying the value out does not copy the blessing';
like $out, qr/^t6:B$/m,
    'a fresh \\$x after the fact reports the class';

# The other direction (s315, bless.t 29-32 / join.t 36-40): the class on a
# variable box is the SCALAR's own SvSTASH -- ref($x)/"$x"/dispatch read the
# class of what $x POINTS TO, never $x's own slot.
like $out, qr/^t7:AA$/m,
    'blessing \\$a1 does not change the class of the object $a1 holds';
like $out, qr/^t8:FF$/m,
    '...while ref(\\$a1) sees the scalar\'s own stash';
like $out, qr/^t9:ok$/m,
    '"$a1" stringifies with the held object\'s class, not the scalar\'s stash';
like $out, qr/^t10:A2$/m,
    're-bless through the variable restamps the target (visible via an alias)';
like $out, qr/^t11:notref:v$/m,
    'a blessed plain scalar is not a ref and stringifies as its plain value';

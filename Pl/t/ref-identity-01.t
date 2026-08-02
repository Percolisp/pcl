#!/usr/bin/env perl
# ref-identity-01.t: a reference's printed TYPE and ADDRESS are properties of
# the referent, not of the storage path it took (task #163).
#
# PCL represents `\$x` as a fresh wrapper box around $x's box.  Both the
# stringifier and `==` used to read the WRAPPER — so two `\$x` printed two
# different addresses and compared unequal — and box-sv guessed which level it
# was at by counting boxes, so the SAME reference printed SCALAR through a
# variable (`my $r = \$x; print $r`) and REF straight into print, into an
# array element, or into a raw sub parameter.  One rule now answers both:
# %p-ref-referent, keyed on the wrapper's is-ref flag, which is what p-ref
# already used for its LVALUE/REF arms.
#
# The INVERSE guards matter as much as the positives: widening `\$aref` to REF
# must not turn `\@a` or an element holding an array wrapper into REF too.

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

plan tests => 20;

# One SBCL launch for the whole family: each line prints one answer.
# Every expectation below was taken from real perl running the same program.
my $prog = <<'EOF';
sub ty { my $s = "$_[0]"; $s =~ s/\(0x[0-9a-f]+\)//; return $s }
my %h = (k => 1); my @a = (10, 20); my $x = 5;

# --- identity: two `\` of the same thing are the same reference
print "t1:", (\$x == \$x ? "EQ" : "NE"), "\n";
print "t2:", (\$h{k} == \$h{k} ? "EQ" : "NE"), "\n";
print "t3:", (\$a[0] == \$a[0] ? "EQ" : "NE"), "\n";
print "t4:", ("".\$x eq "".\$x ? "SAME" : "DIFF"), "\n";
print "t5:", (\$x == \$a[0] ? "COLLIDE" : "DISTINCT"), "\n";

# --- printed type does not depend on how the ref reached the printer
my $r = \$x;
print "t6:",  ty($r), "\n";          # through a variable
print "t7:",  ty(\$x), "\n";         # straight into a sub argument (raw slot)
print "t8:",  ty(\$h{k}), "\n";      # element referent
print "t9:",  ty(\5), "\n";          # \literal: a fresh box, still SCALAR
my @refs = (\$x); my %hr = (s => \$x);
print "t10:", ty($refs[0]), "\n";    # array element
print "t11:", ty($hr{s}), "\n";      # hash value

# --- SCALAR vs REF is decided by what the referent CURRENTLY holds
my $lit = \1; my $rr = \$lit;
print "t12:", ty($rr), "\n";         # referent holds a ref -> REF
$lit = 5;
print "t13:", ty($rr), "\n";         # ...now holds a plain scalar -> SCALAR
my $aref = \@a; my $href = \%h;
print "t14:", ref(\$aref), ":", ref(\$href), "\n";

# --- INVERSE: aggregate refs are untouched by the widening above
print "t15:", ty(\@a), ":", ty(\%h), ":", ty(\&ty), "\n";
print "t16:", ref($aref), ":", ref($href), "\n";
my @agg = (\@a, \%h);
print "t17:", ref($agg[0]), ":", ref($agg[1]), "\n";
print "t18:", (\@a == \@a ? "EQ" : "NE"), "\n";

# --- a ref to a plain SCALAR is not a container: perl's fatal, not a silent
#     empty list and not an SBCL type error (#154's two shapes)
my $z = 7; my $sref = \$z;
my @errs;
for my $t (sub { my @c = @$sref; 1 },
           sub { my %c = %$sref; 1 },
           sub { $sref->{k} },
           sub { $sref->[0] }) {
    push @errs, (eval { $t->(); 'NO-DIE' } // ($@ =~ /^(Not an? \w+ reference)/ ? $1 : "OTHER: $@"));
}
print "t19:", join("|", @errs), "\n";
print "t20:", $z, "\n";
EOF

my $out = run_cl($prog);

like $out, qr/^t1:EQ$/m,
    '\\$x == \\$x — identity is the referent, not the fresh wrapper';
like $out, qr/^t2:EQ$/m,
    '\\$h{k} taken twice is one reference (the element box is stable)';
like $out, qr/^t3:EQ$/m,
    '\\$a[0] taken twice is one reference';
like $out, qr/^t4:SAME$/m,
    'and the printed address matches, not just the numeric compare';
like $out, qr/^t5:DISTINCT$/m,
    'INVERSE: refs to different scalars stay distinct';

like $out, qr/^t6:SCALAR$/m, 'SCALAR through a variable';
like $out, qr/^t7:SCALAR$/m, 'SCALAR straight into a raw sub parameter';
like $out, qr/^t8:SCALAR$/m, 'SCALAR for a hash-element referent';
like $out, qr/^t9:SCALAR$/m, '\\5 is SCALAR (its own fresh box), not REF';
like $out, qr/^t10:SCALAR$/m, 'SCALAR out of an array element';
like $out, qr/^t11:SCALAR$/m, 'SCALAR out of a hash value';

like $out, qr/^t12:REF$/m,
    'ref-to-ref prints REF while the referent holds a reference';
like $out, qr/^t13:SCALAR$/m,
    '...and becomes SCALAR once the referent holds a plain value (perl decides dynamically)';
like $out, qr/^t14:REF:REF$/m,
    'ref(\\$aref) is REF — the referent is the SCALAR, not the array it points to';

like $out, qr/^t15:ARRAY:HASH:CODE$/m,
    'INVERSE: \\@a / \\%h / \\&f still print their own kinds';
like $out, qr/^t16:ARRAY:HASH$/m,
    'INVERSE: the aggregate refs themselves are unchanged';
like $out, qr/^t17:ARRAY:HASH$/m,
    'INVERSE: an element holding an aggregate wrapper is ARRAY/HASH, not REF';
like $out, qr/^t18:EQ$/m,
    'INVERSE: \\@a == \\@a — aggregate identity is the container';

like $out, qr/^t19:Not an ARRAY reference\|Not a HASH reference\|Not a HASH reference\|Not an ARRAY reference$/m,
    'a scalar ref used as a container dies with perl\'s message on all four paths';
like $out, qr/^t20:7$/m,
    'INVERSE: the scalar behind that ref is untouched by the failed derefs';

#!/usr/bin/env perl
# format-skip-01.t: `format`/`write` report templates are not-supported, but a
# format block must NOT corrupt parsing of the surrounding code, and write()/
# close()/select() must not crash the program.
#
# Regression for the t/io/defout.t work (2026-06-25):
#   - `format NAME = ... .` is stripped at the source level (PPI otherwise
#     swallows the next statement into the format and parse-errors on the `.`).
#   - write() is a no-op stub returning 1 (not an undefined-function crash).
#   - close() with no args is a success no-op (not an arg-count crash).
#   - select() returns a true handle name, not raw nil (raw nil vanishes during
#     Perl list flattening — see also pos-01.t).

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

sub transpile {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return scalar `$pl2cl $pl_file 2>&1`;
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = transpile($code);
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl --noinform --non-interactive --load $runtime --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

plan tests => 6;

# A format block must not eat the statement that follows it.
my $with_format = <<'END';
my $a = 'fooo';
format STDOUT =
@ @<<
"#", $a
.
print "after format\n";
END
unlike transpile($with_format), qr/PARSE ERROR/,
    'format block does not produce a parse error';
is run_cl($with_format), "after format\n",
    'statement after a format block still runs';

# write() must not crash (no-op stub).
is run_cl('my $r = write(); print "w=$r\n";'), "w=1\n",
    'write() is a no-op returning 1, not an undefined-function crash';

# close() with no args must not crash (arg-count error at macroexpand).
is run_cl('my $r = close(); print "c=$r\n";'), "c=1\n",
    'close() with no args is a success no-op';

# select() returns a true value, not raw nil that drops from a flattened list.
is run_cl('sub f { my ($x,$y)=@_; print "y=", (defined $y?$y:"U"), "\n"; }
           f(select(), "tail");'), "y=tail\n",
    'select() result does not drop the following list element';

# A "format" inside a string literal must be left untouched by the stripper.
is run_cl('my $s = "format X =\nbody\n.\n"; print length($s) > 0 ? "kept\n" : "lost\n";'),
    "kept\n",
    'format-like text inside a string literal is not stripped';

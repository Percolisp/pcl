#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# heredoc-indent-01.t — an INDENTED here-doc (`<<~DELIM`) whose DELIMITER
# itself begins with whitespace, or is empty (task #1501 round 3, s473t5b).
#
# perl strips from every body line the whitespace that stands BEFORE THE
# DELIMITER TEXT on the terminator line.  PPI 1.291 takes `^(\s*)` of the
# whole terminator line, so `<<~' EOF'` counts the delimiter's own leading
# space as indentation and `<<~''` counts the NEWLINE
# (docs/ppi-upstream-bugs.md §30): PPI then declares the here-doc damaged and
# strips NOTHING — `print <<~' EOF'` printed its body with the indentation
# intact, a SILENT WRONG — or, when the body happens to be indented as far as
# the over-count, strips ONE CHARACTER TOO MANY.
#
# `Pl::Parser::_repair_indented_heredocs` recomputed the indentation as the
# terminator line minus the delimiter text and repaired the token.  Since s527b
# PCL's own PPI computes it that way (vendor/PPI, group M), the repair is
# deleted, and all eleven readers of `->heredoc` get perl's answer from the
# tokenizer itself -- including the shape the repair could not separate (a
# body indented as far as the over-count, terminator at EOF).
#
# Every program here is built from EXPLICIT "\n" pieces, never from a here-doc
# in this file: the test IS the whitespace, and a delimiter like `' EOF '` has
# a trailing space on its terminator line that any editor or hook would eat.
# The oracle is real perl, run on the same bytes.

use v5.30;
use strict;
use warnings;
use Test::More;
use File::Temp qw(tempfile);
use FindBin qw($RealBin);
use lib $RealBin;
use lib "$RealBin/../..";
use PCLCore;

my $project_root = "$RealBin/../..";
my $pl2cl        = "$project_root/pl2cl";
my $runtime      = "$project_root/cl/pcl-runtime.lisp";
my @sbcl_rt = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found"  unless `which sbcl 2>/dev/null`;

plan tests => 15;

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    binmode($fh, ':raw');
    print $fh $code;
    close $fh;
    return $pl_file;
}

sub run_cl {
    my ($code) = @_;
    my $cl_code = PCLCore::transpile("$pl2cl " . write_pl($code));
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    binmode($cl_fh, ':raw');
    print $cl_fh $cl_code;
    close $cl_fh;
    my $output = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $output =~ s/^;.*\n//gm;
    $output =~ s/^PCL Runtime loaded\n//gm;
    return $output;
}

sub run_perl { my $f = write_pl($_[0]); return scalar `perl $f 2>&1` }

# label => [ the `<<~DELIM` opener, the body lines, the terminator line ]
my @shapes = (
    # a delimiter with a LEADING space: perl strips the 2 spaces before it
    [ lead1 => "<<~' EOF'",  ["  some data"],            "   EOF"  ],
    # …and only those 2, even when the body has 3 (PPI stripped all 3)
    [ lead2 => "<<~' EOF'",  ["   some data"],           "   EOF"  ],
    # leading AND trailing space in the delimiter
    [ lead3 => "<<~' EOF '", ["  some data"],            "   EOF " ],
    # two leading spaces, body indented 4
    [ lead4 => "<<~'  EOF'", ["    some data"],          "      EOF" ],
    # the interpolating spelling takes the same path
    [ dq1   => '<<~" EOF"',  ["  data"],                 "   EOF"  ],
    # tabs, with a leading-space delimiter
    [ tab1  => "<<~' EOF'",  ["\t\tsome data"],          "\t\t EOF" ],
    # the EMPTY delimiter: the terminator line is whitespace only, and PPI's
    # `^(\s*)` swallowed its NEWLINE
    [ empty1 => "<<~''",     ["  some data"],            "  "      ],
    # the control PPI gets right
    [ plain => '<<~EOF',     ["  some data"],            "  EOF"   ],
    # several body lines, one of them blank (perl leaves a blank line alone)
    [ multi => "<<~' EOF'",  ["  line one", "    line two", "", "  line three"],
                                                         "   EOF"  ],
);

my $prog = '';
for my $s (@shapes) {
    my ($label, $open, $body, $term) = @$s;
    $prog .= qq{print "$label:", $open;\n} . join('', map { "$_\n" } @$body)
           . "$term\n";
}

my $got  = run_cl($prog);
my $want = run_perl($prog);

sub by_label {
    my %h;
    my $cur;
    for my $l (split /\n/, $_[0], -1) {
        if ($l =~ /^([a-z0-9]+):(.*)$/ && !defined $h{$1}) { $cur = $1; $h{$cur} = "$2\n" }
        elsif (defined $cur && length $l) { $h{$cur} .= "$l\n" }
    }
    return \%h;
}
my ($g, $w) = (by_label($got), by_label($want));

# perl's own answers, spelled out, so a wrong ORACLE cannot make a row pass.
is($w->{lead1}, "some data\n",  "oracle: perl strips 2 for <<~' EOF'");
is($w->{lead2}, " some data\n", "oracle: perl strips 2 (NOT 3) for <<~' EOF' over a 3-space body");

for my $s (@shapes) {
    my $label = $s->[0];
    is($g->{$label}, $w->{$label}, "<<~ indentation, $label: PCL matches perl");
}

# THE TERMINATOR AS THE FILE'S LAST LINE, with no newline after it: PPI takes
# a different branch there (it runs off the end and pops the terminator back
# out of the body), and t/op/heredoc.t has EIGHT rows in exactly this shape —
# every `$script_end = ""` twin of the rows above.  It needs its own program,
# because nothing can follow it in the file.
{
    my $prog = "print <<~' EOF'\n  some data\n   EOF";
    my ($g2, $w2) = (run_cl($prog), run_perl($prog));
    is($w2, "some data\n", "oracle: perl strips 2 with the terminator at EOF");
    is($g2, $w2, "<<~ indentation, terminator at EOF with no newline: PCL matches perl");
}

# The INVERSE side of the fix, at the PPI level and with no SBCL: after it
# the document round-trips byte-exact through serialize, which plain PPI does
# not manage (§30's second face) — the check that says the fixed
# indentation and the fixed body agree.
{
    require Pl::Parser;
    my $src = "print <<~' EOF'\n  some data\n   EOF\n";
    my $doc = Pl::Parser::fragment_doc($src);
    is($doc && $doc->serialize, $src,
       "#1501/§30: a <<~' EOF' document round-trips through serialize");
}

# FIXED ON PCL'S PPI BRANCH (s527b, group M; CLAUDE.md rule 13): the
# vendored PPI strips the body itself.  This row FAILS on PPI 1.291 (and on
# PCL_SYSTEM_PPI=1 until upstream takes the fix), where it is the guard that
# the deleted _repair_indented_heredocs is not needed.
{
    require Pl::VendorPPI;
    my $src = "print <<~' EOF'\n  some data\n   EOF\n";
    my $doc = PPI::Document->new(\$src);
    my ($hd) = @{ $doc->find('PPI::Token::HereDoc') || [] };
    is(join('', $hd->heredoc), "some data\n",
       "FIXED ON PCL'S PPI BRANCH: PPI strips the body of <<~' EOF' (§30)");
}

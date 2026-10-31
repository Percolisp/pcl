#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ppi-token-rows-01.t -- s527d: the tokenizer rows fixed on PCL's PPI branch
# (docs/ppi-upstream-bugs.md §39-§47, docs/ppi-fix-at-source-s527.md §8), each
# run end to end against perl.  The PPI half of each row is guarded by the
# registry canary (Pl/t/ppi-bugs-registry-01.t); these rows guard what PCL
# DOES with the now-correct token -- the lowering half, which the corpora do
# not reach (none of the shapes occurs in the 111 perl-tests files).

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

sub write_pl {
    my ($code) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    return $pl_file;
}

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

sub both_agree {
    my ($code, $desc) = @_;
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    my $pcl  = run_cl($code);
    is($pcl, $perl, "$desc (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# §39 -- a spaced sigil is ONE Symbol whose text keeps the space; the
# compiler sees the canonical name (Pl::Parser::_canonical_spaced_symbols),
# in a whole document and in a string-eval fragment alike.
both_agree('my $ bits = 5; $ x = 2; print $bits + $x, "\n";',
           '§39: my $ bits / $ x are $bits / $x');
both_agree('my $ s = 3; my $ y = 4; print $s * $y, "\n";',
           '§39: $ s and $ y are variables, not a substitution / transliteration');
both_agree('for (1, 2) { print $ _ } print "\n";',
           '§39: $ _ is $_');
both_agree('my $r = eval q{my $ z = 6; $z + 1}; print "$r\n";',
           '§39: a spaced sigil inside a string eval');

# §41 -- an empty package segment is part of the name.
both_agree('sub foo::::bar { 7 } print foo::::bar(), "\n";',
           '§41: sub foo::::bar and its call');

# §44 -- CORE::__END__ / CORE::__DATA__ end the code (and the data section
# is DATA's).
both_agree(qq{print "x\\n";\nCORE::__END__\nnot code\n},
           '§44: CORE::__END__ ends the code');
both_agree(qq{print scalar(<DATA>);\nCORE::__DATA__\nline one\nline two\n},
           '§44: CORE::__DATA__ starts the DATA section');

# §45 -- & before whitespace and a block / a scalar is the code sigil.
both_agree('& {sub { print "ok\n" }};',
           '§45: & {BLOCK} calls the code');
both_agree('sub c { print "c@_\n" } my $code = \&c; & $code(1);',
           '§45: & $code(1) calls the code');
both_agree('my ($a1, $b1) = (6, 3); print $a1 & $b1, "\n";',
           '§45: after a term & is still the bitwise and (control)');

# §46 -- the old package separator opening a sub name.
both_agree(q{no warnings; sub 'Hello'_he_said { "hi" } print Hello::_he_said(), "\n";},
           q{§46: sub 'Hello'_he_said defines Hello::_he_said});

# §47 -- WORD : in a ternary's middle operand is not a label.
both_agree('sub f { my $c = 0 ? ref shift : shift; return $c } print f(5), "\n";',
           '§47: 0 ? ref shift : shift');

# §9 -- a punctuation variable in braces is ONE Magic token keeping the
# braces; the compiler reads it as the plain spelling.
both_agree('"ab" =~ /(a)(b)/; my @p = @{+}; my @m = @{ - }; print "@p|@m\n";',
           '§9: @{+} and @{ - } are @+ and @-');
both_agree('$! = 2; my $e = ${ ! }; print $e + 0, "\n";',
           '§9: ${ ! } is $!');
both_agree('"xab" =~ /(a)(b)/; print "@{+}|@{-}\n"; print "ok\n" if "A2B" =~ /A@{+}[2]B/ || 1;',
           '§9: @{+} / @{-} interpolated in a string');
both_agree('"ab" =~ /(a)/; my %c = (x => 1); my @k = keys %{+}; print scalar(@k), "\n";',
           '§9: %{+} is %+');

# §20 -- use experimental 'try' enables try for PPI too (the construct
# is one compound statement; the next statement survives).
both_agree(qq{use experimental "try"; no warnings;\ntry { die "e\\n" } catch (\$e) { print "caught \$e" }\nprint "after\\n";},
           q{§20: use experimental 'try'});

# §14, the after-a-block half: <$fh> after a map block is a readline.
both_agree('open my $fh, "<", \"a\nb\n"; my %h; my @l = map { $h{$_}++ } <$fh>; print scalar(@l), "\n";',
           '§14: map { ... } <$fh>');
# s527g (#4100): after a BUILTIN list operator PPI now lexes the glob too
# (group AH, 5c74a167) -- also when the path holds a -DIGIT, which used to
# cascade into a transliteration that swallowed the next statement.
both_agree('my @b = sort <./*>; print @b > 0 ? "y\n" : "n\n";',
           '§14: sort <./*> (after a list operator)');
both_agree('my @b = sort </nonexistent-s527g/x-1/y/*.pl>; print scalar(@b), "\n"; print "next\n";',
           '§14 #4100: sort </abs/x-1/y/*.pl> -- a -DIGIT in the glob');
both_agree('open my $fh, "<", \"l1\nl2\n"; sub g { return <$fh> } my @g = g(); print scalar(@g), "\n"; print reverse </nonexistent-s527g/*>; print "e\n";',
           '§14 #4100: return <$fh>, reverse <glob>');

# §43 (s527g, group AL) -- a lexical sub named like a quote-like operator
# or x is called by its bare name in its scope; after its block the keyword
# is back.
both_agree(q{use feature "lexical_subs"; no warnings; my sub s { "S@_" } print s(1), "\n"; my sub y { "Y@_" } print y(2), "\n"; { my sub q { "Q@_" } print q(3), "\n"; } my $v = q(lit); print "$v\n"; my $n = 3 x 2; print "$n\n";},
           '§43: my sub s / y / q called by the bare name');
both_agree(q{use feature "lexical_subs"; no warnings; sub f { my sub tr { "T@_" } return tr(4) } print f(), "\n"; $_ = "abc"; { my sub m { "M@_" } print m(5), "\n"; } print "m\n" if m/b/;},
           '§43: my sub tr / m inside a block, m/b/ after it');

done_testing();

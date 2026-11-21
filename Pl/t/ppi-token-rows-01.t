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
# ...and after a USER sub, which only a declaration makes a list operator:
# PPI keeps the less-than and PCL's source-level glob repair
# (_repair_glob_pattern_cascade) also reads the y/ derail.
both_agree('sub foo { scalar @_ } my $n = foo <./*>; print $n > 0 ? "y\n" : "n\n"; my @b = foo </nonexistent-s527g/x-1/y/*.pl>; print "next @b\n";',
           '§14 #4100: foo </abs/x-1/y/*.pl> after a user sub');

# §43 (s527g, group AL) -- a lexical sub named like a quote-like operator
# or x is called by its bare name in its scope; after its block the keyword
# is back.
both_agree(q{use feature "lexical_subs"; no warnings; my sub s { "S@_" } print s(1), "\n"; my sub y { "Y@_" } print y(2), "\n"; { my sub q { "Q@_" } print q(3), "\n"; } my $v = q(lit); print "$v\n"; my $n = 3 x 2; print "$n\n";},
           '§43: my sub s / y / q called by the bare name');
both_agree(q{use feature "lexical_subs"; no warnings; sub f { my sub tr { "T@_" } return tr(4) } print f(), "\n"; $_ = "abc"; { my sub m { "M@_" } print m(5), "\n"; } print "m\n" if m/b/;},
           '§43: my sub tr / m inside a block, m/b/ after it');

# ---- s536j: PPI batch 8 (§62 §63 §64) -- the lowering half of each fixed row --------

# §62 #4701: a negated QUALIFIED word is one Word now; PExpr::_split_negated_call_words
# splits it like -NAME when the name is callable, and a qualified callable name is not
# autoquoted by a fat comma or a hash subscript (perl quotes an identifier only).
both_agree(q{sub Foo::baz { 3 } print -Foo::bar, " ", -Foo::baz, " ", -Foo::baz(), " ", -foo::bar::baz, " ", -Foo::, " ", - Foo::baz, " ", (-e::foo ? "t" : "f"), "\n";},
           '§62: -Foo::bar (string), -Foo::baz (call), -Foo:: ("-Foo"), -e::foo (a filetest)');
both_agree(q{my $z = 1; my $v = -CORE::log($z + CORE::sqrt(4)); printf "%.6f %d %d\n", $v, -CORE::exp(0), -CORE::abs(-2);},
           '§62: -CORE::log(...) and -CORE::exp(0) negate the builtin (core Math::Complex 750 1081)');
both_agree(q{sub Foo::bar { 3 } my %g = (Foo::bar => 1, Foo::qux => 2, -Foo::bar => 3, -Foo::qux => 4); my %h = ("-3" => "a", "-Foo::qux" => "b", 3 => "c", "Foo::qux" => "d"); print join(",", sort keys %g), " ", $h{-Foo::bar}, $h{-Foo::qux}, $h{Foo::bar}, $h{Foo::qux}, "\n";},
           '§62: a fat comma and a hash subscript call a declared qualified name, quote an undeclared one');
# Math::Complex's load sets `local $SIG{FPE}`, which PCL announces on stderr and
# does not install (docs/not-supported.md, the SBCL-owned signals); that one
# line is the only difference perl's run does not have, so it is taken out.
{
    my $code = q{use Math::Complex; printf "%.6f %.6f %.6f\n", cbrt(-8), Re(acos(2)), Im(acos(2));};
    my $perl = `perl @{[ write_pl($code) ]} 2>&1`;
    (my $pcl = run_cl($code)) =~ s/^PCL: %SIG: \$SIG\{FPE\} is not implemented[^\n]*\n//m;
    is($pcl, $perl, "§62: core Math::Complex cbrt / acos run (perl: " . ($perl =~ s/\n/\\n/gr) . ")");
}

# §63 #4542: x<digits> (and x) after a block that ends a statement is a word -- a call.
both_agree(q{sub x3 { print "called(@_)\n"; 7 } x3(); sub x64 { print "x64(@_)\n" } x64 1; my @m = map { 1 } x3(); BEGIN { 1 } x3(); if (1) { 2 } x3(); my $r = "ab" x3; my $s = do { "c" } x3; print "$r $s @m\n";},
           '§63: x3() after a sub body / map / BEGIN / if block is a call; after a term or a do block it repeats');

# §64 #4482: __DATA__ / __END__ before a fat comma is the string; the data section after it still starts.
both_agree(qq{our %K = (\n  __FILE__ => 1,\n  __DATA__ => 2,\n  __END__  => 3,\n);\nprint join(",", sort keys %K), "\\n";\nmy \@l = (__END__ => 3); print "\@l\\n";\nprint <DATA>;\n__DATA__\nline one\n},
           '§64: __DATA__ => / __END__ => in a list; the real __DATA__ after it');

# §66 #4784: x<digits> where perl expects a term -- after a builtin that takes an operand, in the
# filehandle slot of print, and before a ( -- is a word (a call); after a term or a constant it repeats.
both_agree(q{sub x3 { "<@_>" } sub x64 { "x64" } sub f { return x3(9) } sub foo { "foo(@_)" } print x64(), "|"; print x3() x2, "|"; print STDOUT x3(), "|"; print x3 1; print "|", f(), "|", foo x3(), "|"; my @s = sort { $a <=> $b } 3, 1; print "@s|" if x3(); print defined x3() ? "d" : "u", "\n";},
           '§66: x64() / x3() after print / STDOUT / return / if / defined / a user word is a call');
both_agree(q{use constant C => "c"; sub x3 { $b <=> $a } open my $fh, ">&", \*STDOUT or die; print $fh x3(1), "|"; close $fh; my $s = "z"; print "a" x3, "|"; print STDOUT "b" x3; print "|", C x3, "|"; my @l = sort x3 3, 1, 2; my @r = (1) x3; print "@l @r|"; print $s x 2, "\n";},
           '§66: x3 after print $fh / sort is a word; after a string, a list, a constant, $s it repeats');

# §67 #3555: the first { inside an anonymous array is an anonymous hash, whatever follows it.
both_agree(q{my $x = "k"; my %h = (a => 2); my @t = ( [ sub { "abc" } => "s" ], [ { "abc", 1 } => "a" ], [ { \1, "abc" } => "b" ], [ { $x, 1 } => "c" ], [ { %h } => "d" ] ); print join(",", map { ref($_->[0]) . ":" . $_->[1] } @t), " $t[3][0]{k} $t[4][0]{a}\n";},
           '§67: [ { \1, "abc" } ], [ { $x, 1 } ], [ { %h } ] hold hashes; [ sub { } ] a code ref');

# §65 #4761: a CR in the source -- a FILE reads CR LF as LF before the data section and keeps a lone
# CR (here-doc body included) and the data section raw; an eval STRING keeps every CR of a literal and
# reads a here-doc body's CR LF / lone CR as LF.  The programs hold real CR bytes.
both_agree(qq{my \$a = q{a\r\nb}; my \$b = "c\rd"; print length(\$a), length(\$b), " ", __LINE__, "|";\r\nmy \$h = <<"T";\r\nx\ry\r\nT\r\nprint unpack("H*", \$h), " ", __LINE__, "|";\nprint unpack("H*", do { local \$/; <DATA> }), "\\n";\n__DATA__\r\nj\r\nk\rl\n},
           '§65: file mode -- q{} CR LF -> LF, a lone CR kept, the here-doc CR LF -> LF, __DATA__ raw, lines at LF');
both_agree(q{my $c = "q{a\r\nb}"; my $h = "my \$v = <<T;\r\nx\r\ny\rz\r\nT\r\n\$v"; my $s = "\"a\r\n\r\nb\" =~ s/(\r\n){2,}/X/r"; print length(eval $c), " ", unpack("H*", eval $h), " ", eval($s), "\n";},
           '§65: eval mode -- a literal keeps its CR LF, a here-doc body reads CR LF and a lone CR as LF');

# §34 #2095 / §48 #2541 (verified s543d): after a float of any spelling and after a list slice's ] the
# minus is the operator -- perlfaq4's first example and the slice shape.
both_agree(q{my $x = 2; print 0.5-1, " ", $x*0.5-1, " ", int(0.6/0.2-2), " ", .5-1, " ", 1e3-1, " ", +(1)[0]-1, " ", +(1)[0]-0.5, " ", -3**2, "\n";},
           '§34 / #2541: 0.5-1, $x*0.5-1, int(0.6/0.2-2), +(1)[0]-1 subtract; -3**2 is -(3**2)');

# §68 (s543d): __FILE__ / __LINE__ / __PACKAGE__ end a term -- a - or a . before a digit after them is the operator.
both_agree(q{my $l = __LINE__-1; my $m = __LINE__ -1; my $f = (__FILE__.1 eq __FILE__ . "1") ? "cat" : "no"; print "$l $m $f ", __LINE__.5, " ", __PACKAGE__.1, " ", __LINE__ % 2, "\n";},
           '§68: __LINE__-1 subtracts, __FILE__.1 / __LINE__.5 / __PACKAGE__.1 concatenate');

# §69 #483 (s543d): << and a word after a term is the left shift by a call; a here-doc where a term is expected.
both_agree(qq{sub f { 2 } my \$D = "psl"; my \$num = 0; \$num |= (1<<index(\$D, \$_)) for split //, "sl"; my \$x = 1; my \@a = (1); my %h = (a => 1); print 1<<index("ab","b"), " ", \$x<<f(), " ", \$a[0]<<f(), " ", \$h{a}<<f(), " \$num ", <<EOT;\nbody\nEOT\nprint STDOUT <<EOT;\ntwo\nEOT\n},
           '§69: 1<<index(...), $x<<f(), $a[0]<<f(), $h{a}<<f() shift (Devel::Peek debug_flags); print <<EOT is a here-doc');

# §70 #937 (s543d): a bare block opened by {; ends its statement (PCL's _normalize_null_statements
# reads the same shapes; the PPI half is the registry canary's).
both_agree(q{{;} print "a"; { ; } print "b"; {; my $x = 1; print "c$x" } {; local $_ = "d"; print } my $h = {}; print ref($h), "\n";},
           '§70: {;} / { ; } / {; ... } are bare blocks; the statement after each is its own');
# ... and a `{; PAIR }` bare block that is a sub's value stays a block when a later pass reparses
# the document (here the word-match repair's reparse; no perl-5.40-only syntax: CI runs 5.38):
# _normalize_null_statements keeps its `;` as `();` for a Compound statement too (with only the PPI
# fix it became an anonymous hash: `1 HASH`).
both_agree(q{sub f { {; a => 1 } } my @r = f(); print scalar(@r), " ", ref($r[0]), "|"; sub ok { print $_[0] ? "m" : "n", "\n" } $_ = "x"; ok /x/, 1;},
           '§70: sub f { {; a => 1 } } returns the pair, after a reparse too');

# §71 #2774 (s543f, group BE): a / after a builtin that takes an operand starts a match -- the ;
# and the # inside `sin / 25 ; # /` are pattern text (the die runs; it was commented out, a silent
# wrong), and PExpr's term walker takes the match as the named unary's operand (`sin /;/, 9`,
# `lc /A;/i ? ...` were drops).  `no warnings`: perl's default "ambiguous" warning on stderr.
both_agree(q{no warnings; $_ = "x 25 ; # y"; eval { my $r = sin / 25 ; # / ; die "regex\n" }; my $d = $@; $_ = "a;b"; my @r = (sin /;/, 9); print $d eq "regex\n" ? "died " : "ran ", "@r ", (lc /A;/i ? "y" : "n"), "\n";},
           '§71: sin / 25 ; # / ; die runs the die; sin /;/, 9 and lc /A;/i ? are the matches');
# ... in the filehandle slot of print $fh /x/ (no space after the /) the list starts; print/;/
# without a space, /=/ after print, 1 while /x/g; the controls: shift // 7 is the defined-or, a
# method called split divides, time / time divides.
both_agree(q{$_ = "x=y;x"; my $fh = \*STDOUT; print $fh /x/, "|"; print /=/ ? "eq" : "no", "|"; print/;/; my $n = 0; $n++ while /x/g; print "|$n|", join("-", split //, "ab"), "|"; sub f { my $x = shift // 7; $x } print f(), f(3), "|"; package P; sub new { bless {}, shift } sub split { 6 } package main; print P->new->split / 3, " ", time / time, "\n";},
           '§71: print $fh /x/, print /=/, print/;/, 1 while /x/g match; shift // 7, ->split / 3, time / time');

# §73 #3922 (s543f, group BG): a lone _ is the stat filehandle only as the operand of a filetest,
# stat or lstat; elsewhere it is a word -- a call of the sub named _ (the gettext idiom, a DROP
# before), a fat-comma key and $h{_} (the key read %pcl-stat-cache, a silent wrong), a bareword
# value, the comparison sub of sort _, a method set through the glob.  The filehandle beside it.
both_agree(q{sub _ { "<@_>" } print _(), _("a"), _ ("b"), "|"; my %h = (_ => 1); print join(",", keys %h), $h{_}, "|"; stat "/"; print -e _ ? "e" : "n", -d _ ? "d" : "n", (stat _)[2] ? "s" : "z", "\n";},
           '§73: _(), _("a"), _ ("b") call sub _; (_ => 1) and $h{_} are the key "_"; -e _, -d _, stat _ the filehandle');
both_agree(q{no strict; my %h = (_ => 5, a => 6); print join(",", @h{_, "a"}), "|"; package Q; *Q::_ = sub { "m" }; package main; print Q->_(), "\n";},
           '§73: @h{_, "a"} reads the key "_"; Q->_() calls the method named _');
both_agree(q{sub _ { $a <=> $b } my @l = sort _ 3, 1, 2; print "@l\n";},
           '§73: sort _ LIST sorts with the sub named _');

# The closes of s543f (member 5) -- reproducers that equal perl on main since an earlier PPI group:
# #2670 (§35, group N): a filetest right after a BEGIN block on the same line is the filetest.
both_agree(q{BEGIN { 1 } -e _; print "x\n";},
           '#2670: BEGIN { 1 } -e _; is a filetest of the stat cache, not minus a call of e');
# #3805: `$#+` / `$#-` are PPI's Magic tokens by design; PCL compiles them, and the braced
# spellings, as the last index of @+ / @-.
both_agree(q{"abc" =~ /(a)(b)(c)/; print "$#+ $#- $#{+} $#{-}\n";},
           '#3805: $#+ $#- $#{+} $#{-} are the last indexes of @+ and @-');

done_testing();

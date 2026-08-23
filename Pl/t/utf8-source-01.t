#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# Regression: `use utf8` must decode the *source* as UTF-8, so multi-byte
# sequences in string literals and identifiers are single CHARACTERS (length /
# substr / index / regex see chars, not bytes).  Without the pragma, high bytes
# stay Latin-1 (byte semantics), matching Perl.
#
# This file is written with raw UTF-8 bytes on purpose (é = 0xC3 0xA9, etc.).

use v5.30;
use strict;
use warnings;
use utf8;                       # so the literals below are UTF-8 in THIS file too

use Test::More;
use File::Temp qw(tempfile);
use Encode qw(encode_utf8 decode_utf8);

use lib ".";
use Pl::Parser2;

sub run_bytes {
    # CODE is a *byte* string (as PCL reads a source file); transpile + run.
    my $code = shift;
        my $cl_code = Pl::Parser2->parse_code($code);
    my ($fh, $filename) = tempfile(SUFFIX => '.lisp');
    binmode($fh, ':encoding(utf-8)');   # pl2cl binmodes its output to utf8 too
    print $fh $cl_code;
    close $fh;
    my $output = `sbcl --noinform --non-interactive --load cl/pcl-runtime.lisp --load "$filename" 2>&1`;
    unlink $filename;
    $output =~ s/^;.*\n//gm;
    $output =~ s/PCL Runtime loaded\n?//g;
    $output =~ s/^\s*\n//gm;
    $output =~ s/^\s+//;
    # sbcl output is UTF-8 bytes; decode so it compares against this file's
    # `use utf8` character literals.
    return decode_utf8($output);
}

sub transpile_bytes { return Pl::Parser2->parse_code($_[0]) }

sub run_file_bytes {
    # Same as run_bytes but through the REAL driver.  Pl::Parser2->parse_code
    # omits the `(p-defpackage :main)` pl2cl emits, so a program that opens a
    # non-main `package` and later switches back dies "package MAIN does not
    # exist" under the parse_code harness (docs/DECIDED.md) — a multi-package
    # row has to go through pl2cl/runpcl.
    my ($fh, $plfile) = tempfile(SUFFIX => '.pl');
    binmode($fh);
    print $fh $_[0];
    close $fh;
    my $out = `./runpcl "$plfile" 2>&1`;
    unlink $plfile;
    return decode_utf8($out);
}

plan tests => 30;

# café = 4 chars under use utf8 (é is one char), 5 bytes without it.
is(run_bytes(encode_utf8('use utf8; my $s = "café"; print length($s), "\n";')),
   "4\n", 'use utf8: length("café") == 4 (chars)');

is(run_bytes(encode_utf8('my $s = "café"; print length($s), "\n";')),
   "5\n", 'no use utf8: length("café") == 5 (bytes)');

# substr on a decoded string indexes by character.
is(run_bytes(encode_utf8('use utf8; my $s = "héllo"; print substr($s,1,1), "\n";')),
   "é\n", 'use utf8: substr indexes by character');

# UTF-8 identifiers parse and round-trip under use utf8.
is(run_bytes(encode_utf8("use utf8;\nmy \$café = 42;\nprint \$café, \"\\n\";")),
   "42\n", 'use utf8: UTF-8 identifier');

# index() is character-based under use utf8.
is(run_bytes(encode_utf8('use utf8; my $s = "axé"; print index($s,"é"), "\n";')),
   "2\n", 'use utf8: index() is character-based');

# Task #313 (LOAD-TIME CRASH, found by the s392 companion-suite audit): a
# package whose name STARTS with a non-ASCII letter must land in the ORDINARY
# global partition like any other user package.  Pl::GlobalPartition matched a
# segment's first character ASCII-only, so `બʑ::@ISA` was misread as "not
# word-shaped" = EXCEPTION and declared `defvar` — while p-defpackage had
# already made that package's @ISA a symbol-macro cell.  SBCL then refused the
# file outright ("Cannot proclaim a macro variable special").  The nested-block
# shape is required: it is what makes the declaration come out QUALIFIED.
is(run_bytes(encode_utf8(
     "use utf8;\nuse strict; use warnings;\n"
   . "{\n"
   . "    { package Ascii; our \@ISA = ('X'); }\n"
   . "    { package બʑ;    our \@ISA = ('X'); }\n"
   . "}\n"
   . "print \"ok\\n\";\n")),
   "ok\n", 'utf8 package name in a nested block: @ISA is an ordinary cell (#313)');

# Task #410 (21 census drops across uni/gv.t, uni/stash.t, uni/caller.t,
# uni/method.t, uni/readline.t and the two mro utf8 files).  PPI's `$` branch
# tests /[a-z_]/i where its `%`/`@`/`*`/`&` siblings test /[\w:]/, so `$Ｘ`
# alone splits into Cast + Word (docs/ppi-upstream-bugs.md §23).  Merging the
# two tokens back was not enough: the LEXER had already decided what the
# following `{…}`/`[…]` was from the bareword it saw, making `$Ｘ{a}` a BLOCK
# and `$Ｖ[0]` an anonymous-array CONSTRUCTOR — so the statement was dropped.
# The postfix chain after a repaired symbol is re-classed now.
is(run_bytes(encode_utf8(
     "use utf8;\n"
   . "our %\x{ff38}; our \@\x{ff36};\n"
   . "\$\x{ff38}{a} = 1;\n"
   . "\$\x{ff36}[0] = 7;\n"
   . "my %\x{ff44} = (k => { x => [9] });\n"
   . "my \$ref = \\%\x{ff38};\n"
   . "print \$\x{ff38}{a}, \$\x{ff36}[0], \$\x{ff44}{k}{x}[0], \$ref->{a}, \"\\n\";\n")),
   "1791\n", 'non-ASCII scalar name: hash, array and chained subscripts (#410)');

# ---------------------------------------------------------------------------
# Task #418 (widened s421): SBCL's reader NFKC-normalizes a BARE token and then
# applies :invert to its case, so the fullwidth Ｘ (U+FF38) folds to an ASCII X
# and `%Ｘ` / `%X` became ONE symbol — a SILENT WRONG (perl 1256, PCL 2266).
# The rule is now: any emitted symbol whose name carries a non-ASCII character
# is pipe-quoted (Pl::CLForm::cl_sym), and the runtime's %pcl-invert-case is
# the identity on such a name so the two sides of the seam agree.

is(run_bytes(encode_utf8(
     "use utf8;\n"
   . "our %\x{ff38} = (a=>1); our %X = (a=>2);\n"
   . "our \$\x{ff38} = 5;     our \$X = 6;\n"
   . "our \@\x{ff38} = (7);   our \@X = (8);\n"
   . "print \$\x{ff38}{a}, \$X{a}, \$\x{ff38}, \$X, \$\x{ff38}[0], \$X[0], \"\\n\";\n")),
   "125678\n", 'GLOBAL: a fullwidth name and its ASCII twin are distinct (#418)');

is(run_bytes(encode_utf8(
     "use utf8;\n"
   . "my %\x{ff38} = (a=>1); my %X = (a=>2);\n"
   . "my \$\x{ff38} = 5;     my \$X = 6;\n"
   . "my \@\x{ff38} = (7);   my \@X = (8);\n"
   . "print \$\x{ff38}{a}, \$X{a}, \$\x{ff38}, \$X, \$\x{ff38}[0], \$X[0], \"\\n\";\n")),
   "125678\n", 'LEXICAL: the same twins in one scope are distinct lets (#418)');

is(run_bytes(encode_utf8(
     "use utf8;\nsub \x{ff26} {1} sub F {2}\nprint \x{ff26}(), F(), \"\\n\";\n")),
   "12\n", 'SUB NAME: a fullwidth sub name and its ASCII twin are distinct (#418)');

# The package designator and the runtime STRING must name the same package:
# `(p-defpackage :ＦＯＯ)` read as "foo" while `(p-stash "ＦＯＯ")` looked for
# the perl characters, so `exists $ＦＯＯ::{bar}` answered 0 where perl says 1.
is(run_file_bytes(encode_utf8(
     "use utf8;\n"
   . "{ package \x{ff26}\x{ff2f}\x{ff2f}; our \$z = 5; sub bar { 1 } }\n"
   . "print exists \$\x{ff26}\x{ff2f}\x{ff2f}::{bar} ? 1 : 0,\n"
   . "      \$\x{ff26}\x{ff2f}\x{ff2f}::z, \x{ff26}\x{ff2f}\x{ff2f}::bar(), \"\\n\";\n")),
   "151\n", 'PACKAGE: p-defpackage and p-stash agree on a fullwidth name (#418)');

# A LOOP LABEL is a symbol too, and `p-last-dynamic` builds its tag from a
# runtime string — so the fullwidth label must not fold onto its ASCII twin.
is(run_bytes(encode_utf8(
     "use utf8;\nmy \$out = '';\n"
   . "\x{ff2c}\x{ff2f}\x{ff2f}\x{ff30}: for my \$i (1..3) {\n"
   . "  LOOP: for my \$j (1..3) {\n"
   . "    next \x{ff2c}\x{ff2f}\x{ff2f}\x{ff30} if \$j == 2;\n"
   . "    \$out .= \"\$i\$j,\";\n"
   . "  }\n}\nprint \"\$out\\n\";\n")),
   "11,21,31,\n", 'LABEL: a fullwidth label and its ASCII twin are distinct (#418)');

# THE INVERSE GUARD, in emission: an ASCII name must stay BARE.  Under :invert
# a bare `$x` reads as the symbol `$X` while `|$x|` reads as `$x`, so quoting an
# ASCII name would silently rename it — which is why cl_sym is the identity on
# ASCII and why the population-wide acceptance bar for #418 is byte-identical
# emission over every ASCII file.
{
  my $ascii = transpile_bytes("our \$x = 1; our \@y = (1); sub f {1}\n");
  my $wide  = transpile_bytes(encode_utf8(
      "use utf8;\nour \$\x{ff38} = 1; our \@\x{ff39} = (1); sub \x{ff26} {1}\n"));
  like($ascii, qr/\(p-defcell \$x /,  'ASCII scalar stays a BARE token (#418 inverse)');
  unlike($ascii, qr/\|\$x\|/,         'ASCII scalar is never pipe-quoted (#418 inverse)');
  like($wide, qr/\(p-defcell \|\$\x{ff38}\| /, 'non-ASCII scalar is pipe-quoted (#418)');
  like($wide, qr/\(p-sub \|pl-\x{ff26}\|/,     'non-ASCII sub name is pipe-quoted (#418)');
}

# The three sites the #418 rule reached that its first pass did not (s423).
# Each is a place where ONE side of a seam spelled the name and the other did
# not, so the two named different symbols and the program ran on the wrong one.

# (1) THE EVAL PREAMBLE.  pl2cl's build_eval_preamble had its own copy of the
# package-designator rule, so a string eval compiled inside `package <wide>`
# opened the reader's NFKC-folded package while the FILE used the quoted one:
# the eval's free `$z` resolved in a different package and read back undef.
is(run_file_bytes(encode_utf8(
     "use utf8;\n"
   . "package \x{ff26}\x{ff2f}\x{ff2f};\n"
   . "our \$z = 5;\n"
   . "sub f { my \$v = 7; return eval 'q(x) . \$v . \$z' }\n"
   . "package main;\n"
   . "print \x{ff26}\x{ff2f}\x{ff2f}::f(), \"\\n\";\n")),
   "x75\n", 'EVAL PREAMBLE: a string eval resolves globals in the SAME package (#418)');

# (2) A BAREWORD FILEHANDLE.  `print FH`/`<FH>` quoted the name, `open`/`close`
# did not — so the program wrote to one handle and read from another and
# printed NOTHING.  The `is_filehandle` registry is what tells the leaf
# emitter that this bareword is a symbol.
{
  my ($tfh, $tmp) = tempfile(SUFFIX => '.txt');  close $tfh;  unlink $tmp;
  is(run_bytes(encode_utf8(
       "use utf8;\n"
     . "open(\x{ff26}\x{ff28}, '>', '$tmp') or die;\n"
     . "print \x{ff26}\x{ff28} \"ok\\n\";\n"
     . "close(\x{ff26}\x{ff28});\n"
     . "open(\x{ff26}\x{ff28}, '<', '$tmp') or die;\n"
     . "my \$l = <\x{ff26}\x{ff28}>;\n"
     . "close \x{ff26}\x{ff28};\n"
     . "print \$l;\n")),
     "ok\n", 'BAREWORD FH: open/print/readline/close agree on the name (#418)');
  unlink $tmp;
}

# (3) THE ELEMENT SIGIL SWAP.  `$Pkg::A[0]` names the ARRAY @A, and the swap
# that turns `$` into `@` reads the EMITTED token — which for a non-ASCII name
# is `Pkg::|$A|`.  The old two-alternative pattern knew the bare and the
# whole-token quoted spellings but not the QUALIFIED quoted one, so p-aref got
# the SCALAR and died in gethash.
is(run_file_bytes(encode_utf8(
     "use utf8;\n"
   . "our \@\x{ff2c} = (1,2); our %\x{ff2d} = (a=>3);\n"
   . "print \"\$main::\x{ff2c}\[0]|\$main::\x{ff2d}{a}|\$\x{ff2c}\[1]\\n\";\n")),
   "1|3|2\n", 'QUALIFIED ELEMENT: the sigil swap sees through the pipes (#418)');

# The inverse for (2): an ASCII bareword filehandle stays a BARE token on both
# sides.  cl_sym is the identity on ASCII, and the registry lookup that gates
# the leaf emitter must not change that.
{
  my $ascii = transpile_bytes("open(FH, '<', '/dev/null'); my \$l = <FH>; close FH;\n");
  like($ascii,   qr/\(p-open FH /,  'ASCII bareword FH stays bare at open (#418 inverse)');
  unlike($ascii, qr/\|FH\|/,        'ASCII bareword FH is never pipe-quoted (#418 inverse)');
}

# (4) A NON-ASCII bareword HASH KEY inside a dq string autoquotes like its ASCII
# twin (s425 review probe): the interpolation autoquote predicate had an
# ASCII-only head class, so `"$ｈ{ｋ}"` went to the expression path and called
# sub ｋ (undefined-function crash) while `$ｈ{ｋ}` in code was right.
is(run_bytes(encode_utf8('use utf8; my %ｈ = (ｋ => "v", k => "w"); print "$ｈ{ｋ}$ｈ{k}|", $ｈ{ｋ}, "\n";')),
   "vw|v\n", 'INTERPOLATED non-ASCII hash key autoquotes like its ASCII twin (#418 residue)');

# (5) $#NAME — the ArrayIndex emitter built the array token BARE, bypassing the
# #418 spelling (s425 review probe): `$#Ｘ` read back as the NFKC-folded `@X`
# ("unbound") in code and in strings alike; and the same site emitted a
# MULTI-segment package as `Foo::Bar::@x`, which SBCL cannot read (ASCII,
# pre-existing: the whole file died at load).  Both halves now go through
# cl_pkg/cl_sym like every other emitter.
is(run_bytes(encode_utf8('use utf8; our @Ｘ = (1,2,3); print $#Ｘ, " ", $#{Ｘ}, " ", "$#Ｘ $#{Ｘ}\n";')),
   "2 2 2 2\n", '$#NAME with a non-ASCII name, code and string (#418 residue)');
is(run_bytes('package Foo::Bar; our @x=(1,2); package Foo; our @x=(1,2,3); our @z=(1,2,3,4); print $#Foo::Bar::x, " ", $#Foo::x, " ", $#z, " ", $#Foo::z, " ", "$#Foo::Bar::x $#Foo::x $#z $#Foo::z\n";'),
   "1 2 3 3 1 2 3 3\n", '$#Pkg::Seg::name (multi-segment package) is readable; single-segment and bare unchanged (ASCII inverse)');

# Task #422 item 2 (s427): WHITESPACE between a repaired non-ASCII symbol and
# its subscript.  `_reclass_subscripts_after` walked with `next_sibling`, so it
# stopped at the Whitespace token and left `{…}`/`[…]` as the LEXER had built
# them — `print $Ｘ {a}` became a block-form FILEHANDLE spec and `print $Ｖ [1]`
# an anonymous array.  PPI itself steps over whitespace here (the ASCII
# `$h {a}` and `@h {qw(a b)}` dump as Subscripts, PPI 1.291), so the repair
# mirrors it with `snext_sibling`.  The last two statements are the control: a
# `{` that really is a BLOCK must stay one.
is(run_bytes(encode_utf8(
     "use utf8;\n"
   . "my %\x{ff38} = (a => 1, b => 2); my \@\x{ff36} = (5, 6, 7);\n"
   . "print \$\x{ff38} {a}, \$\x{ff36} [1], \"\\n\";\n"
   . "my \@sl = \@\x{ff38} {qw(a b)};\n"
   . "print \"\@sl\\n\";\n"
   . "if (\$\x{ff36} [0]) { print \"blk\\n\" }\n"
   . "my \@ms = map { \$_ } \@\x{ff36};\n"
   . "print scalar(\@ms), \"\\n\";\n")),
   "16\n1 2\nblk\n3\n",
   'a space before the subscript of a repaired non-ASCII symbol (#422.2)');

# Task #435 (s438f): EVERY FRAGMENT RE-PARSE now runs the in-place token
# repairs, via Pl::Parser::fragment_doc.  The document-level repair
# (_merge_unicode_symbols, #410) shipped in s420 and put `$ｉ` — which PPI
# splits into Cast + Word, ppi-upstream-bugs.md §23 — back together for a whole
# FILE; every interpolated subscript, `@{[ … ]}` block and spliced prologue
# built its own PPI::Document and got none of it.  So a non-ASCII name INSIDE
# a re-parsed fragment read as the symbolic reference ${"ｉ"} (undef → index 0,
# a SILENT WRONG) or died calling an undefined sub of that name.
#
# The last two rows are the ASCII inverse: the repairs preserve text exactly
# and are no-ops on ASCII, so the same shapes must be untouched.
is(run_bytes(encode_utf8(
     "use utf8;\n"
   . "our \@\x{ff38} = (1,2,3); our %\x{ff28} = (k => 8); our \$\x{ff36} = 1;\n"
   . "my \$\x{ff49} = 1; my \$\x{ff4b} = 'k';\n"
   . "print \"\$\x{ff38}\[\$\x{ff49}]\", \" \", \"\$\x{ff28}\{\$\x{ff4b}}\", \"\\n\";\n")),
   "2 8\n",
   'a non-ASCII name as the SUBSCRIPT of an interpolated element (#435)');

is(run_bytes(encode_utf8(
     "use utf8;\n"
   . "our \@\x{ff38} = (1,2,3); our \$\x{ff36} = 1;\n"
   . "my \$\x{ff49} = 1;\n"
   . "print \"\$\x{ff38}\[\$\x{ff49}+1]\", \" \", \"\@\{[ \$\x{ff38}\[0] + \$\x{ff36} ]}\", \"\\n\";\n")),
   "3 2\n",
   '... in an expression subscript and inside an embedded block (#435: both used to die)');

is(run_bytes('our @X = (1,2,3); our %H = (k => 8); my $i = 1; my $k = "k";'
           . ' print "$X[$i] $H{$k} $X[$i+1] @{[ $X[0] + 1 ]}\n";'),
   "2 8 3 2\n",
   'the ASCII inverse: identical shapes, untouched by the fragment repairs');

# Task #492 (s443g): the s/// REPLACEMENT side.  Deciding whether a
# replacement interpolates was a PRIVATE `(?<!\\)[\$\@][a-zA-Z_{]` class in
# ExprToCL, ASCII where the rest of the pipeline is Unicode-aware — so with
# `use utf8` in force `s/Ｘ/$ｉ/` answered NO and the replacement went out as
# the LITERAL text `$ｉ`.  Silent wrong, and inconsistent three ways: the
# braced `${ｉ}` spelling was right (the `{` was in the class), the identical
# dq string `"$ｉ"` was right (the rows above), and the PATTERN side was right
# (it already asked Pl::InterpScan).  The replacement text reaches that site
# DECODED — measured, utf8 flag on and ord 65353 — so the class was the whole
# bug; the gate now asks the same one scanner.
is(run_bytes(encode_utf8(
     "use utf8;\n"
   . "my \@\x{ff38} = (10,20,30); my \$\x{ff49} = 1;\n"
   . "my \$u = \"a\x{ff38}b\"; \$u =~ s/\x{ff38}/\$\x{ff49}/;\n"
   . "my \$t = \"a\x{ff38}b\"; \$t =~ s/\x{ff38}/\$\x{ff38}[1]/;\n"
   . "my \$v = \"a\x{ff38}b\"; \$v =~ s/\x{ff38}/\$\{\x{ff49}}/;\n"
   . "my \$w = \"a\x{ff38}b\"; \$w =~ s/\x{ff38}/\@\x{ff38}/;\n"
   . "print \"\$u \$t \$v \$w\\n\";\n")),
   "a1b a20b a1b a10 20 30b\n",
   'a non-ASCII name in an s/// REPLACEMENT interpolates (#492)');

is(run_bytes('my @X = (10,20,30); my $i = 1;'
           . ' my $s = "aXb"; $s =~ s/X/$X[$i]/;'
           . ' my $r = "aXb"; $r =~ s/X/${i}/;'
           . ' my $q = "aXb"; $q =~ s/X/@X/;'
           . ' print "$s $r $q\n";'),
   "a20b a1b a10 20 30b\n",
   'the ASCII inverse: the same three s/// replacement spellings, unchanged');

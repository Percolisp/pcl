#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# THE COMPILER'S OWN FACTS ON THE DECLARATION FORMS (task #1035 steps 2-4).
#
# `p-let` carried a CLASS from s466; this file guards what steps 2 and 3 added
# beside it, and the two properties that make the whole thing free:
#
#   step 2  the RENAME MANIFEST -- `:perl "$x" :why :FAMILY` on the entry of a
#           name the compiler renamed, threaded from the mint site (never
#           re-derived from the suffix text), plus `:captured t` where a nested
#           closure names the binding;
#   step 3  a per-name CLASS on every `p-raw-params` entry and a per-sub FACTS
#           PLIST at a fixed position after `p-sub`'s lambda list;
#   both    the expansion is byte-identical to the form that stood before, the
#           key/class sets are CLOSED (an unknown member is an error at
#           macroexpansion, rule 12), and PCL_IR_PLAIN=1 prints the old
#           spelling -- the switch the normaliser bar runs on.
#
# Rows are cheap: most are pure transpiles, and the three that spawn SBCL do so
# for a whole batch of assertions at once.

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
my @sbcl_rt      = PCLCore::sbcl_prefix($runtime);

plan skip_all => "pl2cl not found" unless -x $pl2cl;
plan skip_all => "sbcl not found" unless `which sbcl 2>/dev/null`;

# Transpile CODE and return the emitted CL.  %env entries are exported around
# the run (the PCL_IR_PLAIN rows).
sub cl_of {
    my ($code, %env) = @_;
    my ($fh, $pl_file) = tempfile(SUFFIX => '.pl', UNLINK => 1);
    print $fh $code;
    close $fh;
    local @ENV{ keys %env } = values %env;
    return PCLCore::transpile(qq{$pl2cl $pl_file});
}

# Transpile CODE, run it, return the output.
sub run_cl {
    my ($code) = @_;
    my $cl_code = cl_of($code);
    my ($cl_fh, $cl_file) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $cl_fh $cl_code;
    close $cl_fh;
    my $out = `sbcl @sbcl_rt --load $cl_file 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    return $out;
}

# Load LISP at the runtime and return its printed output.
sub lisp_out {
    my ($lisp_src) = @_;
    my ($lfh, $lisp) = tempfile(SUFFIX => '.lisp', UNLINK => 1);
    print $lfh $lisp_src;
    close $lfh;
    my $out = `sbcl @sbcl_rt --load $lisp 2>&1`;
    $out =~ s/^;.*\n//gm;
    $out =~ s/^PCL Runtime loaded\n//gm;
    $out =~ s/^\s*\n//gm;
    return $out;
}

# ── step 2: the rename manifest, one row per family and per CARRIER ───────────
#
# There are five rename families and THREE carriers, because a rename does not
# always bind a `let`: `:exception-global`, `:seam-shadow` and `:state-cell`
# reach a `p-let` entry (below) or a `p-raw-params` entry (further down), while
# `:spanning` and `:captured` promote to a package CELL and are carried there.
# All three read the SAME `_decl_facts`, so the spellings cannot disagree about
# what a rename was.

{
    # `my $a` of an EXCEPTION-SET name: CL cannot lexically bind a proclaimed
    # special, so the declaration takes a fresh symbol.  The commonest family
    # by far -- see the session record for the corpus-wide count, which is
    # audited by matching every registration against a printed pair.
    my $cl = cl_of('my @o; { my $a = 7; my $b = 8; push @o, $a + $b; } print "@o\n";');
    like($cl, qr/\(\$a__excl__0 :scalar 7 :perl "\$a" :why :exception-global\)/,
         '#1035: an exception-global rename carries :perl and :why on its p-let entry');
    like($cl, qr/\(\$b__excl__1 :scalar 8 :perl "\$b" :why :exception-global\)/,
         '#1035: ... for every name of the family, with its own source spelling');
    is(run_cl('my @o; { my $a = 7; my $b = 8; push @o, $a + $b; } print "@o\n";'),
       "15\n", '#1035: and the program still runs (the facts are inert)');
}

{
    # A `my` inside a block that lowers through the v1 expression seam while an
    # outer lexical of the same name is live.
    my $src = 'my $x = 1; my @m = map { my $x = $_ * 2; $x + 1 } (1, 2, 3);'
            . ' print "@m|$x\n";';
    my $cl = cl_of($src);
    like($cl, qr/\(\$x__shadow__0 :scalar .*:perl "\$x" :why :seam-shadow\)/,
         '#1035: a seam-shadow rename carries :perl and :why');
    is(run_cl($src), "3 5 7|1\n", '#1035: ... and the shadowed outer $x is untouched');
}

{
    # `state` in a named sub: promoted to a per-sub package cell.  The cell is a
    # p-defcell, but an anon sub's state decl reaches a p-let, which is the
    # spelling this row pins.
    my $cl = cl_of('use feature "state";'
                 . ' my $c = sub { state $s = 0; return ++$s };'
                 . ' print $c->(), $c->(), "\n";');
    like($cl, qr/:why :state-cell/,
         '#1035: a state-cell rename carries its :why');
}

{
    # `:captured` is VarAnnotator's verdict that a nested anonymous sub names
    # the binding -- the heap-vs-stack fact a backend without native closures
    # needs.  It is stated on the binding, and its ABSENCE is equally load-
    # bearing: an untouched sibling must not acquire it.
    my $cl = cl_of('my $cap = 1; my $plain = 2;'
                 . ' my $f = sub { $cap + 1 }; print $f->() + $plain, "\n";');
    like($cl, qr/\(\$cap :\w[\w-]* 1 :captured t\)/,
         '#1035: a binding a nested closure names carries :captured t');
    unlike($cl, qr/\(\$plain :\w[\w-]* 2 :captured/,
           '#1035: ... and one it does not name carries no :captured');
}

# ── step 2: the manifest on a CELL, for the families that never bind a let ───
#
# A PROMOTED lexical publishes a package cell instead of binding a `let`, so
# `:captured` and `:spanning` -- the two families the task named FIRST -- reach
# emission only here.  Without the facts on the cell a consumer is back to
# parsing `__file__0` off the symbol, which is the whole thing this step
# removes (found by the s469bg merge review; the first version annotated
# p-let entries only).

{
    my $src = 'my $x = 1; sub f { $x + 1 } { my $x = 20; print "inner=$x\n"; }'
            . ' print "f=", f(), "\n";';
    my $cl = cl_of($src);
    like($cl, qr/\(p-defcell \$x__file__\d+ \(make-p-box nil\) :perl "\$x" :why :captured\)/,
         '#1035: a file lexical a NAMED SUB captures carries its manifest on the CELL');
    is(run_cl($src), "inner=20\nf=2\n",
       '#1035: ... and the promotion still works (the facts are inert)');
}

{
    # SPANNING: declared in one package segment, used from a later one.  The
    # name must not be file-unique, or the compiler keeps it (the #470 identity
    # promotion) and there is no rename to record -- which is itself right, and
    # the second row pins it.
    my $span = cl_of('my $shared = 7; package Later; print "later=$shared\n";'
                   . ' package main; { my $shared = 99; print "inner=$shared\n"; }');
    like($span, qr/\(p-defcell \$shared__file__\d+ \(make-p-box nil\) :perl "\$shared" :why :spanning\)/,
         '#1035: a lexical spanning a package boundary carries :why :spanning');
    my $uniq = cl_of('my $shared = 7; package Later; sub peek { $shared }'
                   . ' package main; print Later::peek(), "\n";');
    like($uniq, qr/\(p-defcell \$shared \(make-p-box nil\)\)/,
         '#1035: an IDENTITY promotion is not a rename and carries no manifest');
}

{
    # A NAME CAN BE RENAMED TWICE, and `:perl` must still be the SOURCE
    # spelling.  `$a` is exception-set (renamed once) AND captured by a named
    # sub (renamed again); recording only the second link made `:perl` name
    # `"$a__excl__0"`, a spelling nowhere in the perl program.
    # perl-tests/aassign.t's own shape, minimised: a block `my ($a, @b)` that a
    # NAMED sub inside the block writes.
    my $src = <<'PL';
{
    local @_ = (1, 2, 3);
    my ($a, @b) = @_;
    print "($a)(@b)\n";
    sub f17 { ($a, @b) = @_; return "$a" }
}
print f17(7, 8), "\n";
PL
    my $cl = cl_of($src);
    like($cl, qr/\(p-defcell \$a__excl__\d+__file__\d+ [^\n]*:perl "\$a" :why \(:exception-global :captured\)\)/,
         '#1035: a CHAINED rename resolves :perl to the SOURCE and lists both reasons');
    is(run_cl($src), "(1)(2 3)\n7\n",
       '#1035: ... and the doubly-renamed variable still works');
}

# ── step 3a: p-raw-params entries carry the class AND the manifest ───────────

{
    my $cl = cl_of('sub add { my ($p, $q) = @_; return $p + $q } print add(1,2), "\n";');
    like($cl, qr/\(p-raw-params \(\(\$p :scalar\) \(\$q :scalar\)\)/,
         '#1035: every p-raw-params entry is (NAME CLASS), from the same _slot_class');
    # A RENAMED parameter carries the same manifest a `p-let` entry would --
    # the entry is `(NAME CLASS . FACTS)`, one shape for both (s469bg review:
    # the first version classed parameters but did not annotate them).
    like(cl_of('sub add { my ($a, $b) = @_; $a + $b } print add(2,3), "\n";'),
         qr/\(\$a__excl__\d+ :\w[\w-]* :perl "\$a" :why :exception-global\)/,
         '#1035: a RENAMED parameter carries :perl/:why on its p-raw-params entry');
    is(run_cl('sub add { my ($a, $b) = @_; $a + $b } print add(2,3), "\n";'),
       "5\n", '#1035: ... and the exception-set parameters still bind');
    is(run_cl('sub add { my ($p, $q) = @_; return $p + $q } print add(1,2), "\n";'),
       "3\n", '#1035: ... and the fast path still binds the arguments');
}

# ── step 3b: the p-sub facts plist ───────────────────────────────────────────

{
    my $src = <<'PERL';
sub add    { my ($p, $q) = @_; return $p + $q }
sub ctx    { return wantarray ? "list" : "scalar" }
sub writer { $_[0] = 9 }
sub protod ($$) { my ($m, $n) = @_; $m . $n }
sub evaler { my $t = shift; return eval $t }
my $z = 3;
print add(1,2), ctx(), protod(4,5), evaler("40+2"), "\n";
writer($z);
print "$z\n";
PERL
    my $cl = cl_of($src);
    like($cl, qr/\(p-sub pl-add\s+\(&rest %_args\)\s+\(:returns :num :wantarray-insensitive t :writes-args nil\)/,
         '#1035: a scalar-shaped, wantarray-free sub says so on its definition');
    unlike($cl, qr/\(p-sub pl-ctx\s+\(&rest %_args\)\s+\([^)]*:wantarray-insensitive/,
           '#1035: ... and one that READS wantarray does not (true-only key)');
    like($cl, qr/\(p-sub pl-writer\s+\(&rest %_args\)\s+\(:writes-args t\)/,
         '#1035: :writes-args is printed in BOTH directions -- 0 is a proof too');
    like($cl, qr/\(p-sub pl-protod\s+\(&rest %_args\)\s+\(:prototype "\$\$"\)/,
         '#1035: an old-style prototype prints its text');
    like($cl, qr/\(p-sub pl-evaler\s+\(&rest %_args\)\s+\([^)]*:string-eval t\)/,
         '#1035: a body containing a string eval says so');
    # (`ctx()` sits in a print LIST, so wantarray is true there -- probed
    # against perl 5.40.3, which prints the same line.)
    is(run_cl($src), "3list4542\n9\n",
       '#1035: ... and every one of those subs still runs unchanged');
}

{
    # `:captures` names the promoted package cells a hoisted named sub closes
    # over -- recorded by the promotion that PROVED the capture.
    my $cl = cl_of('my $n = 5; sub bump { $n++ } bump(); print "$n\n";');
    like($cl, qr/\(p-sub pl-bump\s+\(&rest %_args\)\s+\([^)]*:captures \(\$\w+\)\)/,
         '#1035: a sub that captures a file lexical lists the promoted cell');
    is(run_cl('my $n = 5; sub bump { $n++ } bump(); print "$n\n";'), "6\n",
       '#1035: ... and the capture still works');
}

{
    # ALWAYS printed, possibly empty: a consumer reads the slot by POSITION,
    # never by shape.  `use constant` is lowered by v1's constant emitter,
    # which proves none of the facts -- 155 of the corpus's 661 p-sub forms
    # print the empty plist for that reason.
    my $cl = cl_of('use constant NADA => 5; print NADA, "\n";');
    like($cl, qr/\(p-sub pl-NADA \(&rest %_args\) \(\) /,
         '#1035: a sub the compiler proved nothing about still prints ()');
    is(run_cl('use constant NADA => 5; print NADA, "\n";'), "5\n",
       '#1035: ... and it still answers');
}

# ── the two properties that make it free ─────────────────────────────────────

{
    # PCL_IR_PLAIN=1 prints the spelling that stood before #1035: a plain `let`
    # with no class and no facts, bare p-raw-params names, no p-sub plist.  This
    # is the switch `PCL_IR_PLAIN=1 tools/corpus-diff.pl <base>` runs on, and a
    # step that leaves it behind stops being provably syntax-only.
    my $src = 'my $a = 7; sub add { my ($p, $q) = @_; return $p + $q }'
            . ' print add($a, 1), "\n";';
    my $plain = cl_of($src, PCL_IR_PLAIN => 1);
    unlike($plain, qr/\(p-let /,       '#1035: PCL_IR_PLAIN prints `let`, not `p-let`');
    unlike($plain, qr/:perl "|:why :|:captured t/,
           '#1035: ... no fact keys');
    like($plain, qr/\(p-raw-params \(\$p \$q\)/,
         '#1035: ... bare p-raw-params names');
    like($plain, qr/\(p-sub pl-add \(&rest %_args\) \(p-raw-params/,
         '#1035: ... and no p-sub facts plist');
    my $cell = cl_of('my $x = 1; sub f { $x + 1 } { my $x = 20; } print f();',
                     PCL_IR_PLAIN => 1);
    like($cell, qr/\(p-defcell \$x__file__\d+ \(make-p-box nil\)\)/,
         '#1035: ... and a promoted CELL prints without its facts tail');
}

{
    # The expansion is EXACTLY what it was: p-let is a `let`, a facts tail
    # changes nothing, a classed p-raw-params entry binds the same name, and a
    # p-sub facts plist leaves the definition alone.  Proven by macroexpansion
    # at the runtime, not by reading the emitter.
    my $out = lisp_out(<<'LISP');
(in-package :pcl)
(princ (if (equal (macroexpand-1 '(p-let (($x :box (make-p-box nil))) $x))
                  '(let (($x (make-p-box nil))) $x)) "T" "F"))
(princ (if (equal (macroexpand-1
                    '(p-let (($x :num 0 :perl "$y" :why :exception-global
                                 :captured t))
                       $x))
                  '(let (($x 0)) $x)) "T" "F"))
(princ (if (equal (macroexpand-1 '(p-raw-params (($a :scalar) ($b :num)) body))
                  (macroexpand-1 '(p-raw-params (($a :box) ($b :str)) body)))
           "T" "F"))
(princ (if (equal (macroexpand-1
                    '(p-raw-params (($a :scalar :perl "$a" :why :exception-global)) body))
                  (macroexpand-1 '(p-raw-params (($a :scalar)) body)))
           "T" "F"))
(princ (if (equal (macroexpand-1 '(p-defcell $x (make-p-box nil)))
                  (macroexpand-1
                    '(p-defcell $x (make-p-box nil) :perl "$y" :why :captured)))
           "T" "F"))
(princ (if (equal (macroexpand-1 '(p-sub pl-f (&rest %_args) () body))
                  (macroexpand-1
                    '(p-sub pl-f (&rest %_args)
                            (:returns :num :writes-args nil) body)))
           "T" "F"))
(terpri)
LISP
    is($out, "TTTTTT\n",
       '#1035: class and facts are inert -- every form expands to what it did before');
}

{
    # The sets are CLOSED at the RUNTIME end (rule 12): a class or key the spec
    # does not name is an error at macroexpansion, never a silently untyped
    # binding or an unreadable fact.  Each row prints T when the expansion
    # signalled.
    my $out = lisp_out(<<'LISP');
(in-package :pcl)
(defmacro try-expand (form)
  `(princ (if (nth-value 1 (ignore-errors (macroexpand-1 ',form))) "T" "F")))
(try-expand (p-let (($x :fixnum 0)) $x))
(try-expand (p-let (($x :box (make-p-box nil) :colour "red")) $x))
(try-expand (p-let (($x :box (make-p-box nil) :perl)) $x))
(try-expand (p-raw-params (($a :fixnum)) body))
(try-expand (p-raw-params ($a) body))
(try-expand (p-raw-params (($a :box :colour "red")) body))
(try-expand (p-defcell $x (make-p-box nil) :colour "red"))
(try-expand (p-defcell $x (make-p-box nil) :perl))
(try-expand (p-sub pl-f (&rest %_args) (:inlinable t) body))
(try-expand (p-sub pl-f (&rest %_args) (:returns) body))
(terpri)
LISP
    is($out, "TTTTTTTTTT\n",
       '#1035: an unknown class, an unknown fact key or a stray atom DIES at macroexpansion');
}

# ── #1118: the FACTS PLIST is not a declaration entry ────────────────────────
{
    # A p-let declaration entry `(NAME CLASS INIT …FACTS)` and this plist are
    # both HEADLESS lists, and CLForm::to_string keyed the entry layout on
    # "atom, then keyword atom" -- which `(:returns :str …)` matches exactly.
    # Claimed as an entry, the plist's tail went through `join`, so the nested
    # `:captures (CELLS…)` -- an ARRAY REF -- was stringified into the emitted
    # CL as `ARRAY(0x…)`.  Three consequences, all silent until the file was
    # RUN: a fresh address per transpile (non-deterministic output, so every
    # A/B read DIFF), an ODD-length plist, and death at LOAD in %p-check-facts
    # ("facts of pl-f are not keyword pairs").  It needed a proven `:returns`
    # family AND a capture manifest AND a plist too long to print flat --
    # t/opbasic/concat.t and t/op/try.t both died on it.
    my $src = <<'PERL';
my $first_captured_cell_name = "abc";
my $second_captured_cell_name = "def";
sub f { my $t = $first_captured_cell_name . $second_captured_cell_name; return "$t!" }
print f(), "\n";
PERL
    my $cl = cl_of($src);
    unlike($cl, qr/ARRAY\(0x/,
           '#1118: a perl arrayref never reaches the emitted CL');
    like($cl, qr/\(p-sub pl-f\s+\(&rest %_args\)\s+\(:returns :str\s+:writes-args nil\s+:captures \(\$first_captured_cell_name \$second_captured_cell_name\)\)/,
         '#1118: a long facts plist breaks between PAIRS, and :captures names the cells');
    # Two transpiles of the same source must be BYTE-identical.  The
    # tempfile path is in the preamble and differs per call, so it is
    # normalised away; the address was not, and that is the whole point.
    my $strip = sub { my $t = shift; $t =~ s{/tmp/\S+\.pl}{SRC}g; return $t };
    is($strip->($cl), $strip->(cl_of($src)),
       '#1118: ... so the emission is deterministic (it was an address before)');
    is(run_cl($src), "abcdef!\n",
       '#1118: ... and the file LOADS -- the odd-length plist killed it');
}

done_testing();

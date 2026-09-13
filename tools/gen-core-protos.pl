#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# gen-core-protos.pl — generate the runtime's `CORE::` prototype table from
# THIS PERL's own answers (task #1586).
#
#   tools/gen-core-protos.pl                  # print the Lisp form
#   tools/gen-core-protos.pl --check          # compare it against the runtime's
#                                             # committed table; exit 1 on drift
#   tools/gen-core-protos.pl --arity          # the Config.pm disagreement report
#
# WHY A GENERATOR AND NOT A HAND TABLE.  `prototype("CORE::abs")` is LANGUAGE
# data — the same kind of fact as an operator's precedence — so it belongs in
# the runtime (CLAUDE.md 9a's core-builtin exception, and it is not a module's
# behaviour).  But it is ~190 strings that perl itself is the only authority
# for, and a hand-typed copy of them is a copy that drifts silently.  So the
# committed table is generated, and `--check` is a measurement any session can
# repeat: it re-asks the live perl and diffs.
#
# THE KEYWORD LIST vs THE VALUES.  Two different problems:
#
#   * THE VALUES are always a live measurement — `eval { prototype "CORE::$kw" }`
#     on the perl running this script.  Nothing is transcribed.
#
#   * THE LIST of names to ask about cannot be enumerated from a running perl:
#     `%CORE::` is empty (keyword subs are autovivified on demand) and there is
#     no `keys %keywords`.  So the candidates are the UNION of
#       (a) the first column of the `__DATA__` section of perl's own
#           `t/op/cproto.t` — perl's keyword list, from the perl distribution
#           this checkout's tests already come from (found through
#           PCLPaths::perl_suite_t, never a written-down path), and
#       (b) every name in `Pl::PExpr::Config`'s `known_no_of_params`, so a
#           builtin PCL knows about is probed even if (a) is missing or old.
#     Only the names are read from (a); its expectations are the ORACLE and are
#     never copied — that is what keeps the guard row non-circular.
#
# THREE OUTCOMES PER NAME, and the table's shape follows from them:
#   a prototype STRING  -> an entry mapping the name to that string
#   perl answers undef  -> an entry mapping the name to NIL (a keyword with no
#                          prototype: `if`, `my`, `print`, …)
#   perl DIES           -> NO entry.  Absence is what makes `prototype
#                          "CORE::nosuchthing"` die, as perl does.

use strict;
use warnings;
use FindBin qw($RealBin);
use lib "$RealBin/lib";
use PCLPaths;

my %opt;
for my $a (@ARGV) {
    if    ($a eq '--check') { $opt{check} = 1 }
    elsif ($a eq '--arity') { $opt{arity} = 1 }
    else { die "gen-core-protos.pl: unknown argument '$a'\n" }
}

my $root = PCLPaths::root($RealBin);

# ---------------------------------------------------------------- candidates
sub candidate_names {
    my %seen;
    # (a) perl's own keyword list, names only.
    my $t = eval { PCLPaths::perl_suite_t() };
    if (defined $t && -f "$t/op/cproto.t") {
        open my $fh, '<', "$t/op/cproto.t" or die "open $t/op/cproto.t: $!";
        my $in_data = 0;
        while (my $line = <$fh>) {
            if (!$in_data) { $in_data = 1 if $line =~ /^__DATA__\s*$/; next }
            chomp $line;
            next if $line !~ /\S/;
            my ($kw) = split ' ', $line, 2;
            $seen{$kw} = 1 if defined $kw && length $kw;
        }
        close $fh;
    }
    # (b) every builtin PCL's parser knows an arity for.
    {
        local @INC = (@INC, $root);
        if (eval { require Pl::PExpr::Config; 1 }) {
            my $tbl = Pl::PExpr::Config->new->known_no_of_params;
            $seen{$_} = 1 for keys %$tbl;
        }
    }
    return sort keys %seen;
}

# ------------------------------------------------------------- the live probe
# Answers ('proto', STRING) / ('undef') / ('unknown') for one name.
sub ask_perl {
    my ($kw) = @_;
    my $p = eval { prototype("CORE::$kw") };
    return ('unknown') if $@;
    return ('undef')   if !defined $p;
    return ('proto', $p);
}

# ------------------------------------------------------------ Lisp rendering
# A prototype string is arbitrary punctuation (`\[$@%&*]`, `$$;$`, `_`), so
# every one is escaped for a CL string literal; a name is a plain identifier
# plus the four punctuation keywords (`-X` filetests arrive as single letters).
sub lisp_string {
    my ($s) = @_;
    $s =~ s/([\\"])/\\$1/g;
    return "\"$s\"";
}

our $BEGIN_MARK = ';;; BEGIN GENERATED core-prototypes (tools/gen-core-protos.pl)';
our $END_MARK   = ';;; END GENERATED core-prototypes';

sub table_form {
    my (@rows) = @_;                 # [name, 'undef'|'proto', string?]
    # The data is ONE quoted alist consumed by ONE loop — not 253 `setf` forms.
    # A `(NAME . PROTO)` pair carries a prototype; a `(NAME)` pair is a keyword
    # perl answers undef for (its cdr is NIL, which is the stored value).
    my @pairs = map {
        my ($kw, $kind, $proto) = @$_;
        $kind eq 'undef' ? '(' . lisp_string($kw) . ')'
                         : '(' . lisp_string($kw) . ' . ' . lisp_string($proto) . ')';
    } @rows;
    # Pack the pairs into lines of at most ~74 columns at depth 4 (8 spaces),
    # so the block stays readable and every line runs at one paren depth.
    my (@data_lines, $cur);
    for my $p (@pairs) {
        if (!defined $cur)                       { $cur = $p }
        elsif (length($cur) + 1 + length($p) <= 66) { $cur .= " $p" }
        else { push @data_lines, $cur; $cur = $p }
    }
    push @data_lines, $cur if defined $cur;
    # INDENTATION IS THE FORMATTER'S, not a choice: a LOOP clause's argument
    # aligns under the clause word (col 10 here, `for … in`), and the list's
    # elements one deeper (col 12).  The repo's .lisp formatter hook re-indents
    # to exactly that, so generating anything else makes `--check` report a
    # phantom DRIFT the first time the file is touched (it did, once).
    $data_lines[-1] = $data_lines[-1] . ")";
    @data_lines = map { ($_ ? ' ' x 12 : (' ' x 10) . "'(") . $data_lines[$_] }
                  0 .. $#data_lines;
    my @lines = (
      $BEGIN_MARK,
      ';;;',
      ';;; `prototype("CORE::NAME")` — perl\'s own prototype strings, which are',
      ';;; LANGUAGE data (CLAUDE.md 9a\'s core-builtin exception) and so live here.',
      ';;; Regenerate with `tools/gen-core-protos.pl > …`; verify with',
      ';;; `tools/gen-core-protos.pl --check`, which re-asks the live perl and',
      ';;; diffs.  DO NOT HAND-EDIT: perl is the only authority for these strings,',
      ';;; and a hand copy is a copy that drifts silently (task #1586).',
      '(defparameter %pcl-core-prototypes',
      '  (let ((h (make-hash-table :test \'equal)))',
      '    (loop for (kw . proto) in',
      @data_lines,
      '          do (setf (gethash kw h) proto))',
      '    h)',
      '  "Every perl KEYWORD, mapped to the prototype string `prototype(\\"CORE::NAME\\")`',
      '   answers for it, or NIL where perl answers undef (`if`, `my`, `print`, and',
      '   the other ~70 control-flow words).  A name that is NOT A KEY is not a',
      '   keyword at all: %p-core-prototype then dies `Can\'t find an opnumber for',
      '   \\"NAME\\"`, perl\'s own message.  Generated — see the header.")',
      $END_MARK,
    );
    return join("\n", @lines) . "\n";
}

# ------------------------------------------------------------------- the work
my @rows;
for my $kw (candidate_names()) {
    my ($kind, $proto) = ask_perl($kw);
    next if $kind eq 'unknown';
    push @rows, [$kw, $kind, $proto];
}
die "gen-core-protos.pl: no keywords resolved — is this a real perl?\n"
  unless @rows > 50;

if ($opt{arity}) {
    # THE DISAGREEMENT MEASUREMENT the filler owes (#1586): PCL's parser has a
    # SECOND table of the same builtins — `known_no_of_params`, which records
    # ARITIES for parsing, not perl's prototype TEXT.  The two are not unified
    # here; this report says how far apart they are, so the decision to unify
    # is made from a list and not from a guess.  Arity implied by a prototype:
    # count the top-level items of the prototype, stopping at `;` for the
    # minimum and at `@`/`%` for "list".
    local @INC = (@INC, $root);
    require Pl::PExpr::Config;
    my $tbl = Pl::PExpr::Config->new->known_no_of_params;
    my (@dis, %n);
    for my $r (@rows) {
        my ($kw, $kind, $proto) = @$r;
        next unless exists $tbl->{$kw};
        $n{both}++;
        my $spec = $tbl->{$kw};
        my $spec_txt = ref($spec) eq 'ARRAY' ? "[@{$spec}]" : $spec;
        if ($kind eq 'undef')                               { $n{no_proto}++; next }
        if (grep { $_ < 0 } (ref($spec) eq 'ARRAY' ? @$spec : ($spec))) {
            $n{list_spec}++; next;
        }
        $n{compared}++;
        next if arity_agrees($spec, $proto);
        push @dis, sprintf("%-16s proto=%-12s implied=%-8s Config=%s",
                           $kw, "($proto)", proto_arity($proto), $spec_txt);
    }
    print "# CORE prototype vs Pl::PExpr::Config known_no_of_params (task #1586)\n";
    printf "# keywords with both facts: %d\n", $n{both} // 0;
    printf "#   perl answers undef (the prototype says nothing): %d\n", $n{no_proto} // 0;
    printf "#   Config spec is a LIST spec (admits every arity):  %d\n", $n{list_spec} // 0;
    printf "#   actually COMPARED:                                %d\n", $n{compared} // 0;
    printf "#   DISAGREEING:                                      %d\n", scalar(@dis);
    print "$_\n" for @dis;
    exit(@dis ? 1 : 0);
}

my $form = table_form(@rows);

if ($opt{check}) {
    my $rt = "$root/cl/pcl-runtime.lisp";
    open my $fh, '<', $rt or die "open $rt: $!";
    my $src = do { local $/; <$fh> };
    close $fh;
    my $b = quotemeta $BEGIN_MARK;
    my $e = quotemeta $END_MARK;
    my ($committed) = $src =~ /^($b\n.*?^$e\n)/ms;
    die "gen-core-protos.pl --check: no generated block in $rt\n"
      . "(expected the sentinel line: $BEGIN_MARK)\n"
      unless defined $committed;
    if ($committed eq $form) {
        print "gen-core-protos: table matches this perl (", scalar(@rows), " keywords)\n";
        exit 0;
    }
    print "gen-core-protos: DRIFT — the committed table is not what this perl answers\n";
    my @c = split /\n/, $committed;
    my @w = split /\n/, $form;
    for my $i (0 .. ($#c > $#w ? $#c : $#w)) {
        next if defined $c[$i] && defined $w[$i] && $c[$i] eq $w[$i];
        printf "  line %d:\n    committed: %s\n    this perl: %s\n",
               $i + 1, $c[$i] // '(missing)', $w[$i] // '(missing)';
    }
    exit 1;
}

print $form;

# The minimum/maximum argument count a prototype implies, as a short label.
# A `\[$@%*]` GROUP is ONE argument (a reference to any of those), so the
# bracketed set must be consumed as a token BEFORE looking for a slurpy
# `@`/`%` — reading it as "list" made all twelve of `each`/`keys`/`values`/
# `shift`/`tied`/… look like disagreements when they are arity 1.
sub proto_arity {
    my ($p) = @_;
    my ($req, $opt) = (0, 0);
    my $optional = 0;
    while ($p =~ /\G(\\\[[^\]]*\]|\\.|;|.)/gs) {
        my $tok = $1;
        if ($tok eq ';')                  { $optional = 1; next }
        return 'list' if $tok =~ /^[\@\%]$/;   # a bare slurpy ends the count
        $optional ? $opt++ : $req++;
    }
    return $opt ? "$req-" . ($req + $opt) : "$req";
}

# Does Config's spec agree with the prototype's implied arity?  Deliberately
# lenient: only a FLAT contradiction counts (a fixed count against a different
# fixed count, or fixed against list), because the two tables answer different
# questions and a soft mismatch is not a bug.
# Does Config's spec ADMIT the arity the prototype implies?  The two tables
# answer different questions -- Config says what the PARSER may accept, the
# prototype says what perl's own op takes -- so the test is containment, not
# equality: a disagreement is a spec that CANNOT accept the prototype's arity.
sub arity_agrees {
    my ($spec, $proto) = @_;
    return 1 unless defined $proto;                 # undef proto says nothing
    my $implied = proto_arity($proto);
    my @spec    = ref($spec) eq 'ARRAY' ? @$spec : ($spec);
    # Any negative entry (-1 list, -2 $_-default, -3 @_-default, -12 one+list)
    # is a LIST spec and admits every arity.
    return 1 if grep { $_ < 0 } @spec;
    return 0 if $implied eq 'list';                 # slurpy vs fixed counts
    my ($lo, $hi) = $implied =~ /^(\d+)-(\d+)$/ ? ($1, $2) : ($implied, $implied);
    return (grep { $_ >= $lo && $_ <= $hi } @spec) ? 1 : 0;
}

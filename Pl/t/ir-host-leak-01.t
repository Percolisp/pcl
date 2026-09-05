#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-host-leak-01.t — the emitted CL must contain only the three declared
# vocabularies (task #1172, Part B item B4; docs/ir-spec.md §11b).
#
# A PCL-emitted file may contain: the runtime's exported `p-*` names (§10a),
# the WHITELISTED CL kernel (§11b), literals, and the program's own
# identifiers.  Anything else is a HOST LEAK — a bare CL function that reached
# the output through a v1 seam, an `sb-*` symbol, a host type designator.
#
# A LEAK IS INVISIBLE TO EVERY OTHER INSTRUMENT.  The gate runs the emitted CL
# on SBCL, where `(concatenate 'string …)` works perfectly; corpus-diff only
# compares two emissions of the same compiler.  Only this question catches it.
#
# WHAT THIS FILE DOES, and why it is not the full census: the population run
# (`tools/ir-host-leak.pl`, 111 files, ~40 s) is a WHAT-TO-RUN-WHEN item for a
# `cl/**` or `Pl/**` change, not a gate row — measured 8.4 s for six real
# corpus files, which is more than this file may spend.  So the rows below are
# SHAPE fixtures: one tiny program per known leak family, asserting the exact
# symbols that family leaks TODAY.  The set is the baseline, in both
# directions:
#
#   * a NEW symbol in a fixture's output is a new leak — fix it or bless it;
#   * a MISSING one means the leak was CLOSED — delete the row, close the task,
#     and move the family out of §11b's leak table into the kernel table.
#
# The families and their tasks are §11b's last table (#1175-#1177).  The
# fixtures are the minimal perl that reaches each one, taken from the corpus
# sites the census named.

use strict;
use warnings;
use Test::More;
use FindBin qw($RealBin);
use File::Temp qw(tempdir);

my $root = "$RealBin/../..";
my $tool = "$root/tools/ir-host-leak.pl";
ok(-x $tool, 'tools/ir-host-leak.pl is present and executable')
  or BAIL_OUT('the census tool is missing — this test cannot check anything');

my $tmp = tempdir(CLEANUP => 1);

# perl source => [ the exact leaked symbols, sorted ], with the family's task.
# `op:` is a symbol in OPERATOR position (the host is asked to CALL it);
# `quoted:` is a quoted symbol (the runtime dispatches on it).
my @CASES = (
  # CLOSED s470bo (#1175 family 2): the availability test is `p-arg-supplied-p`
  # now, so `op:>` left this set.  The row STAYS with an EMPTY expectation —
  # that is what makes a re-opened family fail here instead of going unnoticed.
  { name => 'sigdefault',
    why  => '#1175 family 2, CLOSED s470bo: the signature-default arity test '
          . 'is `p-arg-supplied-p`, not CL `>` on `(length @_)`',
    perl => "use feature 'signatures';\nno warnings;\nsub f (\$a = 222) { return \$a }\nprint f(), \"\\n\";\n",
    leaks => [] },
  # MEASURED: the trigger is the `use integer` PRAGMA, not `int()` — one
  # pragma emits nine bare CL operators plus a qualified runtime internal.
  { name => 'useinteger',
    why  => '#1175: `use integer` emits CL `rem`/`truncate`/`*` and the bit '
          . 'ops inline.  `pcl::%pcl-to-integer` LEFT this set in s470bo: '
          . '#1177 exported the five names the emitter writes qualified, so '
          . 'the inventory covers it and it is no longer a leak',
    perl => "use integer;\nmy \$x = length(\"abc\") % -10;\n"
          . "my \$y = (3 / -10) * -10;\nmy \$b = 5 & 3;\nmy \$c = 5 | 3;\n"
          . "my \$d = 5 ^ 3;\nmy \$e = ~5;\nprint \"\$x \$y \$b \$c \$d \$e\\n\";\n",
    leaks => ['op:*', 'op:logand', 'op:logior', 'op:lognot', 'op:logxor',
              'op:rem', 'op:truncate'] },
  # CLOSED s470bo (#1175 family 3): one `p-literal-string` with the bad code
  # points as INTEGER arguments; the `'string` type designator went with it.
  { name => 'wideescape',
    why  => '#1175 family 3, CLOSED s470bo: a \\x{...} escape is one '
          . '`p-literal-string`, not `concatenate`/`string`/`code-char` and a '
          . 'quoted host type designator',
    perl => "my \$s = \"\\x{d800}\\x{ffff}\";\nprint length(\$s), \"\\n\";\n",
    leaks => [] },
  # MEASURED, not guessed: `open \$scalar` does NOT reach this — the emitter
  # wrote the stream form only for the `__DATA__` handle's registration.
  # CLOSED s470bo (#1175 family 4): one `p-install-data-handle`, which also
  # takes the quoted handle NAME out of the emission (#1176).
  { name => 'datahandle',
    why  => '#1175 family 4, CLOSED s470bo: the __DATA__ handle is '
          . '`p-install-data-handle`, not CL `make-string-input-stream`',
    perl => "print \"x\\n\";\n__DATA__\nhello\n",
    leaks => [] },
  # MEASURED: `\\(1 .. 2)` alone does not reach it — the spread shape needs a
  # refgen over a LIST with more than one element (ref.t:367's own spelling).
  # CLOSED s470bo (#1175 family 5): `p-vector-append`.
  { name => 'refgenlist',
    why  => "#1175 family 5, CLOSED s470bo: \\(LIST)'s spread is "
          . '`p-vector-append`, not CL\'s `loop` macro written out',
    perl => "my(\@fuu) = \\(1..2,3);\nprint scalar(\@fuu), \"\\n\";\n",
    leaks => [] },
  { name => 'chaincmp',
    why  => '#1176: a chained comparison passes its operator as a bare quoted CL symbol',
    perl => "no warnings;\nmy \@a = (1, 2, 3);\nprint((\$a[0] < \$a[1] < \$a[2]) ? \"y\" : \"n\", \"\\n\");\n",
    leaks => ['quoted:<'] },
);

for my $c (@CASES) {
  my $p = "$tmp/$c->{name}.pl";
  open my $fh, '>', $p or die "ir-host-leak-01.t: $p: $!";
  print {$fh} $c->{perl};
  close $fh;

  my $out = `cd "$root" && ./tools/ir-host-leak.pl --unclassified --jobs 1 "$p" 2>&1`;
  my @got;
  for my $l (split /\n/, $out) {
    push @got, $1 if $l =~ /^\s*\d+\s+((?:op|quoted):\S+)/;
  }
  @got = sort @got;
  is_deeply(\@got, [ sort @{ $c->{leaks} } ],
            "$c->{name}: leaks exactly the blessed set")
    or diag("$c->{why}\n"
          . "  got:      @got\n"
          . "  expected: @{ $c->{leaks} }\n"
          . "  A NEW symbol is a new leak; a MISSING one means the family was\n"
          . "  closed — update this row, the task, and ir-spec §11b.");
}

# The kernel whitelist in the tool and the one written down in ir-spec §11b
# must be the same set.  They are two copies by necessity (one is machine
# data, one is a table with per-target renderings), so the drift has to be
# loud: §11b states the SIZE, and the tool is the size.
{
  my $src = do {
    open my $th, '<', $tool or die "ir-host-leak-01.t: $!";
    local $/;
    <$th>;
  };
  my ($kern) = $src =~ /my %KERNEL = map \{ \(\$_ => 1\) \} qw\((.*?)\n\);/s;
  ok(defined $kern, 'found %KERNEL in tools/ir-host-leak.pl');
  my @names = split ' ', ($kern // '');
  my %seen;
  my @dups = grep { $seen{$_}++ } @names;
  is_deeply(\@dups, [], 'the kernel whitelist has no duplicate names');

  my $spec = do {
    open my $sh, '<', "$root/docs/ir-spec.md" or die "ir-host-leak-01.t: $!";
    local $/;
    <$sh>;
  };
  my ($claimed) = $spec =~ /\*\*The whole kernel is (\d+) names\.\*\*/;
  ok(defined $claimed, 'ir-spec §11b states the kernel size');
  is($claimed, scalar @names,
     "ir-spec §11b's kernel size matches tools/ir-host-leak.pl (\%KERNEL)")
    or diag("§11b says $claimed, the tool has " . scalar(@names) . ".\n"
          . "  Adding a kernel form means updating BOTH: the tool's \%KERNEL\n"
          . "  and §11b's table (with the form's JS and C rendering).");
}

done_testing();

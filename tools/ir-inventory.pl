#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# ir-inventory.pl — THE IR OP INVENTORY AS GENERATED DATA (task #1170;
# docs/plan-speed-and-ir-s470.md Part B item B1).
#
# REGENERATE (from the repo root):
#     tools/ir-inventory.pl
#   writes docs/ir-op-inventory.tsv  (machine form, one row per exported name)
#     and docs/ir-op-inventory.md    (the readable table, grouped by family)
#
# WHY IT EXISTS.  A backend author (JavaScript, C, another Lisp) needs a PORT
# LIST: every name a PCL-emitted file can contain, what kind of thing it is,
# what its obligations are.  Today that list is a 682-symbol `(:export …)` form
# inside a 22k-line runtime, and the semantics are one docstring per op.  This
# tool turns the two into data.
#
# WHERE THE DATA COMES FROM — the LOADED runtime, not the source text.  The
# authority is `do-external-symbols (:pcl)` in an SBCL that has the runtime
# loaded, because that IS the evaluated `(:export …)` list AND it sees the ops
# a definer macro generates: `p-+`, `p-*`, the six numeric compares, the six
# string compares and the whole compound-assignment family have NO textual
# `(defun p-NAME` anywhere — they are expansions of `%def-overloaded-arith`,
# `%def-overloaded-cmp`, `%def-overloaded-str-cmp`.  A text scan silently
# misses 20+ of the most-used ops in the IR, which is exactly the kind of hole
# an inventory exists to close.  Cost: one SBCL start on the cached core
# (~0.4 s, tools/lib/PCLSbcl.pm picks it).
#
# THE `Contract:` DOCSTRING TAIL is the extension point.  The runtime "is the
# spec" by project rule, so the machine-readable contract lives where the prose
# already does — as a final paragraph of the op's own docstring:
#
#     Contract: ctx=insensitive coerce=num magic=none dies=no dynamic=no
#               phase=no host=none
#
# Keys and values are CLOSED SETS (%CONTRACT_KEYS below); an unknown key or
# value DIES naming it (CLAUDE.md rule 12) rather than being recorded as data
# nobody can read.  An op with no tail prints UNCLASSIFIED in every contract
# column — that is the campaign's remaining work, counted per run.  The grammar
# is stated normatively in docs/ir-spec.md §10.
#
# RULE 12 HERE: an exported name whose definition the runtime cannot show
# (neither function, macro, variable, constant, symbol-macro nor class) DIES
# naming it — unless it is in one of the two declared no-definition classes:
# the TAP names, which are exported eagerly and defined by cl/pcl-test.lisp on
# demand (that set is MEASURED from that file, never listed here), and the two
# hand-classified names in %NO_DEFINITION.
#
# THE GATE ROW is Pl/t/ir-inventory-01.t: it regenerates the TSV into a temp
# file and compares the body, the #1072 pattern.  A new export, or a changed
# `Contract:` tail, without a regenerated inventory is a red row.
use utf8;
use strict;
use warnings;
use FindBin qw($RealBin);
use File::Temp qw(tempfile);
use Getopt::Long;

binmode(STDOUT, ':encoding(UTF-8)');
binmode(STDERR, ':encoding(UTF-8)');

my $root = "$RealBin/..";
my ($out_tsv, $out_md, $quiet) = ("$root/docs/ir-op-inventory.tsv",
                                  "$root/docs/ir-op-inventory.md", 0);
my $help = 0;
GetOptions('out-tsv=s' => \$out_tsv, 'out-md=s' => \$out_md, 'quiet' => \$quiet,
           'help' => \$help)
  or die usage();
if ($help) { print usage(); exit 0 }
sub usage {
  return "usage: tools/ir-inventory.pl [--out-tsv PATH] [--out-md PATH] [--quiet] [--help]\n"
       . "  regenerates docs/ir-op-inventory.tsv and .md from the loaded runtime\n";
}

# ── The contract grammar ──────────────────────────────────────────────────
# key => [closed value set] or ['*LIST*', closed member set] for comma lists,
# or ['*NAMES*'] for a comma list of magic-global names (sigil-led, or `none`).
my %CONTRACT_KEYS = (
  # does the op read the calling context (*wantarray*)?
  ctx     => [qw(sensitive insensitive)],
  # which Perl coercion it applies to its operands
  coerce  => ['*LIST*', [qw(none num str bool)]],
  # magic globals it reads or writes (§8), or `none`
  magic   => ['*NAMES*'],
  # can the op itself raise a Perl exception?  A die from a user `use overload`
  # handler or a tie method reached THROUGH it does not count — that is the
  # handler's contract, not this op's.
  dies    => [qw(yes no)],
  # does it need a dynamic (save/restore) binding to be implemented?
  dynamic => [qw(yes no)],
  # does it participate in the compile/load phase model (§9)?
  phase   => [qw(yes no)],
  # what the implementation needs from its host beyond plain computation
  host    => ['*LIST*', [qw(none sbcl posix ppcre ffi subprocess)]],
);
my @CONTRACT_ORDER = qw(ctx coerce magic dies dynamic phase host);

# ── Families ──────────────────────────────────────────────────────────────
# `spec` is the ir-spec §10 row this family IS (its rule is quoted in the .md
# from that table); `added` families are ones §10 has no row for, and the count
# of names in them is the measurement that says which rows §10 still owes.
my %FAMILY_META = (
  # --- ir-spec §10's own rows -------------------------------------------
  'numeric'             => { spec => 'numeric ops' },
  'bitwise'             => { spec => 'bitwise (mode-dispatched)' },
  'numeric-compare'     => { spec => 'numeric compare' },
  'string'              => { spec => 'string ops' },
  'string-compare'      => { spec => 'string compare' },
  'logical'             => { spec => 'logical',
    note => "The row named `p-` IS perl's `||`: the runtime writes it `p-||`, "
          . "and the CL reader takes the `||` as an EMPTY multiple-escape "
          . "section, so the symbol's name is `P-` (probed s470bm).  A "
          . "text-parsing backend must fold `p-||` and `p-` — ir-spec §11b." },
  'assignment'          => { spec => 'assignment' },
  'compound-assignment' => { spec => 'compound assignment' },
  'increment'           => { spec => 'increment' },
  'elements'            => { spec => 'elements' },
  'slice-delete'        => { spec => 'slice delete' },
  'aggregate-builtin'   => { spec => 'array/hash builtins' },
  'regex'               => { spec => 'regex' },
  'compiled-regex'      => { spec => 'compiled regex (qr)' },
  'io'                  => { spec => 'I/O' },
  'command-capture'     => { spec => 'command capture' },
  'introspection'       => { spec => 'introspection' },
  'context-frame'       => { spec => 'context & frames' },
  'declaration'         => { spec => 'declarations' },
  # --- families §10 has no row for -------------------------------------
  'box'              => { note => 'the box/undef/coercion primitives — ir-spec §2.1, §2.2, §3' },
  'math'             => { note => 'the transcendental builtins — no §10 row; each dispatches `use overload` since #1005' },
  'range'            => { note => 'the range and flip-flop operators — ir-spec §3.4' },
  'control-flow'     => { note => 'conditionals, loops, loop control, sub return — ir-spec §6' },
  'exception'        => { note => 'die/warn, eval BLOCK, string eval, try — ir-spec §6.3, §9.1' },
  'filetest'         => { note => 'the -X operators and the `_` stat cache — ir-spec §10 I/O adjacent' },
  'file-ops'         => { note => 'filesystem builtins that are not filehandle operations' },
  'directory-io'     => { note => 'opendir/readdir/closedir/rewinddir' },
  'socket'           => { note => 'the socket builtins' },
  'time'             => { note => 'time/sleep/alarm/localtime/gmtime' },
  'process'          => { note => 'fork/exec/wait/kill/exit and the process ids' },
  'user-db'          => { note => 'the passwd/group database builtins' },
  'env'              => { note => '%ENV and its accessors' },
  'module-system'    => { note => 'use/require and the @INC bookkeeping — ir-spec §9' },
  'magic-global'     => { note => 'the magic globals themselves — ir-spec §8' },
  'reference'        => { note => 'reference construction, deref casts, lvalue cells — ir-spec §2.5' },
  'refaliasing'      => { note => "use feature 'refaliasing' assignment targets" },
  'typeglob'         => { note => 'typeglobs and glob assignment — ir-spec §7.1' },
  'dynamic-scope'    => { note => '`local` in each of its place shapes — ir-spec §7.2' },
  'oo'               => { note => 'method dispatch and C3 — ir-spec §7.3' },
  'overload'         => { note => '`use overload` registry and dispatch' },
  'tie'              => { note => '`tie`/`untie`/`tied` and the proxy — ir-spec §2.2b' },
  'phase'            => { note => 'BEGIN/CHECK/INIT/END and the eval-when wrappers — ir-spec §9' },
  'package-tracking'  => { note => 'the caller()/__PACKAGE__ bookkeeping — docs/caller-implementation.md' },
  'sub-definition'   => { note => 'the sub-definition and frame forms — ir-spec §5.1' },
  'signature'        => { note => 'signature arity and slurpy binding — ir-spec §5.2' },
  'call'             => { note => 'the code-ref call form' },
  'bit-string'       => { note => 'vec() — a string used as a bit vector' },
  'pack'             => { note => 'pack/unpack (a transpiled artifact, cl/pcl-pack.lisp)' },
  'extension'        => { note => 'the lazy-extension loader — docs/extensions.md' },
  'capture-io'       => { note => 'the harness output-capture hooks' },
  'runtime-config'   => { note => 'runtime switches a test or REPL may flip' },
  'misc-builtin'     => { note => 'builtins with no family of their own' },
  'tap'              => { note => 'the Test::More layer (cl/pcl-test.lisp) — not part of the language' },
);

# name → family.  Written down once, reviewable, and complete: a name that
# reaches neither this map nor @FAMILY_PATTERN prints UNCLASSIFIED and is
# counted in the report.
my %FAMILY;
{
  my %by_family = (
  'box' => [qw(p-box make-p-box p-box-p p-box-value unbox ensure-boxed
               p-copy-scalar-arg box-set box-nv box-sv to-string to-number
               %pcl-to-number-strict %pcl-to-string-strict %pcl-dualvar-p
               %pcl-str-buffer %pcl-str-append p-undef p-defined p-defined-fh
               %pcl-definedp p-true-p p-$ p-dualvar)],
  'numeric' => [qw(p-+ p-- p-* p-/ p-% p-** p-int p-abs p-double-inf)],
  'math' => [qw(p-sin p-cos p-atan2 p-exp p-log p-sqrt p-rand p-srand)],
  'string' => [qw(p-. p-str-x p-length p-substr p-lc p-uc p-fc p-quotemeta
                  p-chomp p-chop p-index p-rindex p-string-concat p-chr p-ord
                  p-hex p-oct p-lcfirst p-ucfirst p-sprintf p-crypt
                  p-version-string p-unrepresentable-char p-unparsable-quote
                  p-join p-reverse)],
  'string-compare' => [qw(p-str-eq p-str-ne p-str-lt p-str-gt p-str-le p-str-ge
                          p-str-cmp)],
  'numeric-compare' => [qw(p-== p-!= p-< p-> p-<= p->= p-<=> p-chain-cmp)],
  'range' => [qw(p-.. p-... p-flipflop p-flipflop-3 p-flipflop-num
                 p-flipflop-num-3 p-flipflop-dyn p-flipflop-dyn-3)],
  # `p-` IS Perl's `||`.  The runtime writes the export and the defmacro as
  # `p-||`, and the CL reader takes the `||` as an EMPTY multiple-escape
  # segment: the symbol's name is "P-".  So the emitted token `p-||`, the
  # token `p-`, and this row are all one op (probed s470bm).  A backend that
  # parses the IR text must fold them; ir-spec §11b states the rule.
  'logical' => [qw(p-&& p- p-! p-not p-and p-or p-xor p-//)],
  'bitwise' => [qw(p-bit-and p-bit-or p-bit-xor p-bit-not p-<< p->>
                   p-str-bit-and p-str-bit-or p-str-bit-xor p-str-bit-not
                   p-to-s64 p-<<-int p->>-int)],
  'assignment' => [qw(p-setf p-scalar-= p-array-= p-hash-= p-list-= p-my-=
                      p-array-fill p-hash-fill p-box-init p-hash-deref-=
                      p-array-deref-=)],
  'compound-assignment' => [qw(p-incf p-decf p-*= p-/= p-%= p-**= p-.= p-str-x=
                               p-bit-and= p-bit-or= p-bit-xor= p-<<= p->>=
                               p-str-bit-and= p-str-bit-or= p-str-bit-xor=
                               p-and-assign p-or-assign p-//=)],
  'increment' => [qw(p-pre++ p-post++ p-pre-- p-post--)],
  'elements' => [qw(p-aref p-aref-box p-aref-argbox p-gethash-argbox
                    p-aref-deref p-aref-deref-box p-gethash p-gethash-box
                    p-gethash-deref p-gethash-deref-box p-aslice p-hslice
                    p-kv-hslice p-kv-aslice p-exists p-exists-array p-delete
                    p-delete-array p-ensure-hashref p-ensure-arrayref
                    p-list-scalar p-slice-result)],
  'slice-delete' => [qw(p-delete-hash-slice p-delete-kv-hash-slice
                        p-delete-array-slice p-delete-kv-array-slice)],
  'aggregate-builtin' => [qw(p-hash p-array-init p-array-last-index
                             p-set-array-length p-push p-pop p-shift p-unshift
                             p-splice p-flatten p-flatten-args %p-push1 p-keys
                             p-values p-each p-grep p-map p-sort p-sort-get-fn
                             p-sort-named %p-sort-classic p-split p-copy-array
                             p-copy-hash p-scalar p-wantarray p-list-x)],
  'signature' => [qw(p-check-arity p-sig-rest-array p-sig-rest-hash)],
  'control-flow' => [qw(p-if p-unless p-while p-until p-do-while p-do-until
                        p-for p-foreach p-foreach-raw p-foreach-range
                        p-foreach-range-raw p-return p-return-empty p-tail-value
                        p-goto-sub p-goto-computed p-last p-last-dynamic p-next
                        p-redo p-continue p-break p-do)],
  'exception' => [qw(p-eval p-eval-block p-eval-thunk p-eval-lex-lookup p-try
                     p-alias-eval-cell *p-eval-lex-alist* p-exception
                     p-exception-object p-die p-warn p-evalbytes)],
  'io' => [qw(p-print p-say p-printf p-open p-sysopen p-close p-eof p-tell
              p-seek p-sysseek p-pipe p-select p-write p-binmode p-read
              p-sysread p-syswrite p-fileno p-fcntl p-getc p-readline
              *p-filehandles* p-lock)],
  'directory-io' => [qw(p-opendir p-readdir p-closedir p-rewinddir)],
  'socket' => [qw(p-socket p-socketpair p-bind p-connect p-listen p-accept
                  p-send p-recv p-shutdown p-getsockname p-getpeername
                  p-getprotobyname p-getprotobynumber p-setsockopt
                  p-getsockopt)],
  'filetest' => [qw(_ *pcl-stat-cache-path*)],
  'file-ops' => [qw(p-unlink p-glob p-chdir p-set_up_inc p-mkdir p-rmdir
                    p-getcwd p-cwd p-rename p-chmod p-umask p-link p-symlink
                    p-readlink p-chown p-utime p-truncate p-stat p-lstat)],
  'time' => [qw(p-time p-times p-sleep p-alarm p-localtime p-gmtime)],
  'misc-builtin' => [qw(p-study p-reset p-pos)],
  'bit-string' => [qw(p-vec p-vec-set)],
  'process' => [qw(p-exit p-system p-fork p-waitpid p-wait p-getppid p-kill
                   p-exec p-getpgrp p-setpgrp p-getpriority)],
  'command-capture' => [qw(p-backtick)],
  'user-db' => [qw(p-getgrent p-setgrent p-endgrent p-getgrgid p-getgrnam
                   p-getpwent p-setpwent p-endpwent p-getpwuid p-getpwnam
                   p-getlogin)],
  'env' => [qw(%ENV p-env-get p-env-set)],
  'module-system' => [qw(p-use p-require p-require-parent p-require-file
                         p-require-version p-note-inc)],
  'refaliasing' => [qw(p-alias-scalar-target p-alias-array-target
                       p-alias-hash-target p-alias-code-target p-alias-hash-slot
                       p-alias-array-slot p-alias-array-elements)],
  'reference' => [qw(p-backslash p-backslash-sub p-backslash-sub-ref
                     p-backslash-list p-arylen-ref p-substr-ref p-pos-ref
                     p-vec-ref p-substr-lvalue-cell p-pos-lvalue-cell
                     p-vec-lvalue-cell p-arylen-lvalue-cell p-refgen-list
                     p-box-for-local p-get-coderef p-cast-@ p-cast-% p-cast-$
                     p-weaken p-isweak)],
  'introspection' => [qw(p-ref p-reftype p-bless p-caller p-can p-isa
                         p-prototype p-__pcl_set_prototype p-stash p-sub-exists
                         p-sub-defined p-undef-sub p-coderef-exists-p
                         p-coderef-defined-p pl-__SUB__)],
  'typeglob' => [qw(p-typeglob p-typeglob-p make-p-typeglob p-typeglob-package
                    p-typeglob-name p-make-typeglob p-glob-assign
                    p-glob-assign-dynamic p-dynamic-typeglob p-glob-copy
                    p-glob-slot p-glob-undef)],
  'dynamic-scope' => [qw(p-local-glob p-local-glob-if p-local-glob-dynamic
                         p-local-dot p-defcell p-local-cell p-local-cell-if
                         p-local-maybe p-local-pipe p-local-hash-elem
                         p-local-array-elem p-local-hash-elem-init
                         p-local-array-elem-init p-local-array-slice
                         p-local-deref-scalar p-local-deref-array
                         p-local-deref-hash)],
  'pack' => [qw(p-pack p-unpack)],
  'extension' => [qw(p-load-extension)],
  'capture-io' => [qw(p-high-capture p-capture-write p-capture-fh)],
  'call' => [qw(p-funcall-ref)],
  'oo' => [qw(p-get-class p-method-call p-resolve-invocant p-super-call
              perl-pkg-to-clos-class)],
  'overload' => [qw(*p-overload-table* p-register-overloads p-find-overload
                    p-call-overload p-overload-strval p-overloaded)],
  'regex' => [qw(p-=~ p-!~ p-subst p-tr p-regex)],
  'compiled-regex' => [qw(p-regex-from-parts)],
  'magic-global' => [qw(@INC %INC %SIG @ARGV $ARGV @_ %_args p-errno-string
                        *p-errno-table*)],
  'context-frame' => [qw(*wantarray* p-list-ctx p-scalar-ctx p-void-ctx
                         p-caller-ctx p-sort-cmp *pcl-caller-wantarray*
                         *p-in-list-assign-rhs* *pcl-sub-call-depth*)],
  'runtime-config' => [qw(*p-raw-elems*)],
  'package-tracking' => [qw(*pcl-current-package* *pcl-caller-pkg-stack*
                            *pcl-caller-subname-stack* p-set-current-package
                            p-register-pkg-name)],
  'phase' => [qw(*end-blocks* *unitcheck-blocks* *check-blocks* *init-blocks*
                 p-run-compile-phase-blocks p-eval-always p-BEGIN p-CHECK)],
  'tie' => [qw(p-tie-proxy make-p-tie-proxy p-tie-proxy-p p-tie-proxy-tie-obj
               p-tie-proxy-saved-value p-tie p-untie p-tied)],
  'sub-definition' => [qw(p-defpackage p-sub-frame p-cloned-sub p-args-body
                          p-declare-sub)],
  'declaration' => [qw(p-let p-raw-params p-sub)],
  );
  for my $fam (sort keys %by_family) {
    exists $FAMILY_META{$fam} or die "ir-inventory.pl: family '$fam' has no "
      . "\%FAMILY_META entry (rule 12: every family states its rule or its "
      . "absence from ir-spec \x{a7}10)\n";
    for my $n (@{ $by_family{$fam} }) {
      die "ir-inventory.pl: '$n' is in two families ($FAMILY{$n}, $fam)\n"
        if exists $FAMILY{$n};
      $FAMILY{$n} = $fam;
    }
  }
}

# Ordered pattern rules, consulted only when %FAMILY has no exact entry.  Each
# covers a UNIFORM closed sub-family whose members would otherwise be a hundred
# hand-written rows: the 27 filetests, the raw compound-assignment twins, the
# capture variables, and every punctuation global.
my @FAMILY_PATTERN = (
  [ qr/^p--[A-Za-z]$/,                      'filetest' ],
  [ qr/^p-.*=-raw$/,                        'compound-assignment' ],
  [ qr/^p-(?:incf|decf)-raw$/,              'compound-assignment' ],
  [ qr/^\$(?:[0-9]+|_)$/,                   'magic-global' ],
  [ qr/^\|?[\$\@%]/,                        'magic-global' ],
  [ qr/^%\+$/,                              'magic-global' ],
  [ qr/^pl-/,                               'tap' ],
);

sub family_of {
  my ($name) = @_;
  return $FAMILY{$name} if exists $FAMILY{$name};
  for my $r (@FAMILY_PATTERN) { return $r->[1] if $name =~ $r->[0] }
  return 'UNCLASSIFIED';
}

# The two exported names with no definition of their own and a reason each.
# Everything else undefined DIES (rule 12); the TAP set is measured, below.
my %NO_DEFINITION = (
  '%_args' => 'a LAMBDA-LIST name: the &rest parameter the general sub shape '
            . 'binds for @_ (ir-spec §5.2).  Exported so generated code in a '
            . 'user package names the same symbol; never a callable.',
  'p-backslash-list' => 'a PLACE MARKER: `(p-setf (p-backslash-list @a) …)` is '
            . "matched by p-setf's expansion on (car place) and never "
            . 'evaluated as a form (ir-spec §2.5 refaliasing).',
);

# ── ir-spec §10's cited member names, for the cross-check ────────────────
# Every name the §10 table prints as a family member.  A cited name that the
# runtime does not export is a STALE CITATION — reported, because a backend
# author works from that table.
my @SPEC10_CITED = qw(
  p-+ p-- p-* p-/ p-% p-** p-<< p->>
  p-& p-| p-^ p-~ p-str-bit-and p-str-bit-or p-str-bit-xor p-str-bit-not
  p-== p-!= p-< p-> p-<= p->= p-<=>
  p-. p-x p-lc p-uc p-lcfirst p-ucfirst p-length p-substr p-index p-reverse
  p-sprintf p-join
  p-eq p-ne p-lt p-gt p-le p-ge p-cmp
  p-&& p-|| p-// p-! p-not
  p-my-= p-scalar-= p-array-= p-hash-= p-list-=
  p-incf p-decf p-*= p-/= p-%= p-**= p-.= p-str-x= p-bit-and= p-bit-or=
  p-bit-xor= p-<<= p->>= p-str-bit-and= p-str-bit-or= p-str-bit-xor=
  p-and-assign p-or-assign p-//=
  p-++ p---- p-++-post p----post p-incf-raw p-decf-raw
  p-aref p-gethash p-setf p-exists p-delete p-aslice p-hslice
  p-delete-hash-slice p-delete-array-slice p-delete-kv-hash-slice
  p-delete-kv-array-slice
  p-push p-pop p-shift p-unshift p-splice p-keys p-values p-each p-sort p-map
  p-grep p-wantarray p-scalar p-defined %p-sort-classic %p-push1
  p-=~ p-!~ p-regex p-subst p-tr p-split %p-empty-list
  p-qr p-regex-from-parts
  p-print p-say p-printf p-open p-close p-readline p-eof p-binmode
  p-backtick
  p-ref p-bless p-caller p-can p-isa
  p-list-ctx p-scalar-ctx p-void-ctx p-caller-ctx p-sort-cmp
  p-let p-raw-params p-sub
);

# A §10 citation whose SPELLING differs from the symbol the reader produces.
# `p-||`'s pipes are an empty multiple-escape segment, so its symbol name is
# "P-" and prin1 prints it `p-`; the citation is not stale, the spelling is a
# reader fact (ir-spec §11b).
my %CITED_ALIAS = ('p-||' => 'p-');

# ── The runtime dump ─────────────────────────────────────────────────────
my $DUMP_LISP = <<'LISP';
;;; ir-inventory.pl's dump program — see that tool's header.
(require :sb-introspect)
(in-package :pcl)
(defun %ir-inv-esc (s)
  (with-output-to-string (o)
    (loop for c across s do
      (case c (#\\ (write-string "\\\\" o))
              (#\Tab (write-string "\\t" o))
              (#\Newline (write-string "\\n" o))
              (t (write-char c o))))))
;;; ONE RECORD PER LINE is the contract, so the pretty printer is OFF: with it
;;; on, `prin1-to-string' of make-p-tie-proxy's &key lambda list wrapped onto a
;;; second line and the reader of this dump saw a sixth field it could not
;;; classify (measured, s470bm).
(let ((names '())
      (*print-pretty* nil) (*print-right-margin* nil) (*print-lines* nil)
      (*print-length* nil) (*print-level* nil) (*print-circle* nil))
  (do-external-symbols (s :pcl) (push s names))
  (dolist (s (sort names #'string< :key #'symbol-name))
    (let* ((mac (and (macro-function s) t))
           (fn  (and (fboundp s) (not mac)))
           (vk  (sb-int:info :variable :kind s))
           (cls (and (find-class s nil) t))
           (kind (cond (mac "macro")
                       ((and fn (typep (fdefinition s) 'generic-function))
                        "generic-function")
                       (fn "function")
                       ((eq vk :special) "variable")
                       ((eq vk :constant) "constant")
                       ((eq vk :macro) "symbol-macro")
                       ((eq vk :global) "global")
                       (cls "class")
                       (t "UNDEFINED")))
           (lam (if (or mac fn)
                    (handler-case (prin1-to-string
                                    (sb-introspect:function-lambda-list s))
                      (error () "?"))
                    ""))
           (doc (or (documentation s (if (or mac fn) 'function 'variable))
                    (documentation s 'variable)
                    (documentation s 'function)
                    "")))
      (format t "~a~c~a~c~a~c~a~c~a~%"
              (prin1-to-string s) #\Tab kind #\Tab
              (%ir-inv-esc lam) #\Tab (%ir-inv-esc doc) #\Tab
              (if (fboundp (list 'setf s)) "setf" ""))))
  ;; The INTERNAL `p-`/`%p-` symbols, so the ir-spec §10 citation check can
  ;; tell "the spelling in the doc is wrong" from "this one exists but is not
  ;; part of the IR vocabulary" (an internal helper, or an op the emitter
  ;; writes package-QUALIFIED and therefore need not export).
  (let ((seen (make-hash-table :test 'eq)))
    (do-external-symbols (s :pcl) (setf (gethash s seen) t))
    (let ((internal '()))
      (do-symbols (s :pcl)
        (when (and (eq (symbol-package s) (find-package :pcl))
                   (not (gethash s seen))
                   (let ((n (symbol-name s)))
                     (or (eql 0 (search "P-" n)) (eql 0 (search "%P-" n)))))
          (push s internal)))
      (dolist (s (sort internal #'string< :key #'symbol-name))
        (format t "#INTERNAL~c~a~%" #\Tab (prin1-to-string s))))))
LISP

sub dump_runtime {
  my ($tmp, $path) = tempfile('ir-inv-dumpXXXXXX', SUFFIX => '.lisp', TMPDIR => 1);
  binmode($tmp, ':encoding(UTF-8)');   # the program text carries §
  print {$tmp} $DUMP_LISP;
  close $tmp;
  # PCLSbcl is the ONE place an SBCL command line is built (task #344).
  my @prefix = do {
    local @INC = (@INC, "$root/tools/lib");
    require PCLSbcl;
    PCLSbcl::sbcl_prefix(runtime => "$root/cl/pcl-runtime.lisp");
  };
  my @cmd = ('sbcl', @prefix, '--load', $path);
  open my $ph, '-|', @cmd or die "ir-inventory.pl: cannot run sbcl: $!\n";
  binmode($ph, ':encoding(UTF-8)');
  my @lines = <$ph>;
  close $ph;
  my $rc = $?;
  unlink $path;
  die "ir-inventory.pl: the runtime dump failed (sbcl status $rc)\n" if $rc;
  die "ir-inventory.pl: the runtime dump produced nothing — is "
    . "cl/pcl-runtime.lisp loadable?\n" unless @lines;
  return \@lines;
}

sub unesc {
  my ($s) = @_;
  $s =~ s/\\(.)/$1 eq 'n' ? "\n" : $1 eq 't' ? "\t" : $1/ge;
  return $s;
}

# The TAP names cl/pcl-test.lisp defines — MEASURED, so a name added there is
# covered the day it lands and one removed stops being excused.
sub lazy_tap_names {
  my $f = "$root/cl/pcl-test.lisp";
  open my $fh, '<', $f or die "ir-inventory.pl: cannot read $f: $!\n";
  my %n;
  while (my $l = <$fh>) { $n{$1} = 1 if $l =~ /^\((?:defun|defmacro)\s+(pl-\S+)/ }
  close $fh;
  die "ir-inventory.pl: found no `pl-` definitions in $f — the lazy-TAP "
    . "measurement broke, and every TAP export would now die (rule 12)\n"
    unless keys %n;
  return \%n;
}

# ── Parse the `Contract:` tail out of a docstring ────────────────────────
# The tail is the last paragraph, starting with `Contract:` at the head of a
# line (leading whitespace allowed) and running to the end of the docstring.
sub parse_contract {
  my ($name, $doc) = @_;
  return undef if !defined $doc || $doc !~ /(?:^|\n)[ \t]*Contract:[ \t]*(.*)\z/s;
  my $tail = $1;
  $tail =~ s/\s+/ /g;
  $tail =~ s/^\s+|\s+$//g;
  my %got;
  for my $tok (split / /, $tail) {
    my ($k, $v) = $tok =~ /^([a-z]+)=(\S+)$/
      or die "ir-inventory.pl: $name: malformed Contract token '$tok' "
           . "(want key=value; the grammar is in this tool's header and "
           . "docs/ir-spec.md \x{a7}10)\n";
    my $spec = $CONTRACT_KEYS{$k}
      or die "ir-inventory.pl: $name: unknown Contract key '$k' "
           . "(known: " . join(' ', @CONTRACT_ORDER) . ")\n";
    die "ir-inventory.pl: $name: Contract key '$k' given twice\n"
      if exists $got{$k};
    if ($spec->[0] eq '*NAMES*') {
      for my $piece (split /,/, $v) {
        next if $piece eq 'none';
        die "ir-inventory.pl: $name: Contract $k value '$piece' is not a magic "
          . "global name (want a sigil-led name like \$_ or \@- , or 'none')\n"
          unless $piece =~ /^[\$\@%][^\s,]*$/;
      }
    }
    elsif ($spec->[0] eq '*LIST*') {
      my %ok = map { ($_ => 1) } @{ $spec->[1] };
      for my $piece (split /,/, $v) {
        die "ir-inventory.pl: $name: Contract $k value '$piece' is not in the "
          . "closed set (" . join(' ', @{ $spec->[1] }) . ")\n" unless $ok{$piece};
      }
      die "ir-inventory.pl: $name: Contract $k mixes 'none' with other values\n"
        if $v =~ /(?:^|,)none(?:,|$)/ && $v ne 'none';
    }
    else {
      my %ok = map { ($_ => 1) } @$spec;
      die "ir-inventory.pl: $name: Contract $k value '$v' is not in the closed "
        . "set (" . join(' ', @$spec) . ")\n" unless $ok{$v};
    }
    $got{$k} = $v;
  }
  my @missing = grep { !exists $got{$_} } @CONTRACT_ORDER;
  die "ir-inventory.pl: $name: Contract tail omits " . join(', ', @missing)
    . " — a partial contract would read as a default, and an absent key means "
    . "'not stated' everywhere else in this IR (ir-spec \x{a7}2b.2a)\n" if @missing;
  return \%got;
}

# ── Build the rows ───────────────────────────────────────────────────────
my $lines = dump_runtime();
my $lazy  = lazy_tap_names();
my (@rows, %exported, %internal);
for my $l (@$lines) {
  chomp $l;
  next unless length $l;
  if ($l =~ /^#INTERNAL\t(.*)$/) { $internal{$1} = 1; next }
  # EXACTLY five fields, and the guard is rule 12: tabs and newlines are
  # escaped on the Lisp side, so anything else means the dump lost its
  # one-record-per-line shape and a row would silently be garbage.
  my @f = split /\t/, $l, -1;
  die "ir-inventory.pl: malformed dump line (" . scalar(@f)
    . " fields, want 5): $l\n" unless @f == 5;
  my ($name, $kind, $lam, $doc, $flags) = @f;
  $lam = unesc($lam);
  $doc = unesc($doc);
  $exported{$name} = 1;
  if ($kind eq 'UNDEFINED') {
    if ($lazy->{$name})          { $kind = 'function(lazy)' }
    elsif ($NO_DEFINITION{$name}) { $kind = 'name-only' }
    else {
      die "ir-inventory.pl: exported name '$name' has NO definition in the "
        . "loaded runtime and no declared reason.  Either define it, or add it "
        . "to \%NO_DEFINITION with the reason (rule 12: a silent row here is a "
        . "name a backend author cannot implement).\n";
    }
  }
  my $fam = family_of($name);
  my $c   = parse_contract($name, $doc);
  # The one-line summary: the docstring's first sentence-ish line, with the
  # Contract tail removed (it is in its own columns).
  my $sum = $doc;
  $sum =~ s/(?:^|\n)[ \t]*Contract:.*\z//s;
  $sum =~ s/\s+/ /g;
  $sum =~ s/^\s+|\s+$//g;
  $sum = substr($sum, 0, 160) if length($sum) > 160;
  push @rows, {
    name => $name, kind => $kind, lambda => $lam, family => $fam,
    flags => $flags, summary => $sum,
    contract => $c,
  };
}

# ── Write the TSV ────────────────────────────────────────────────────────
my @TSV_COLS = (qw(name kind family lambda_list), @CONTRACT_ORDER, qw(flags summary));
{
  open my $fh, '>:encoding(UTF-8)', $out_tsv or die "ir-inventory.pl: $out_tsv: $!\n";
  print {$fh} "# docs/ir-op-inventory.tsv — GENERATED, do not edit.\n";
  print {$fh} "# regenerate:  tools/ir-inventory.pl\n";
  print {$fh} "# one row per name exported by the :pcl runtime package (the IR's\n";
  print {$fh} "# whole vocabulary).  Contract columns are UNCLASSIFIED until the op's\n";
  print {$fh} "# docstring carries a `Contract:` tail (grammar: docs/ir-spec.md \x{a7}10).\n";
  print {$fh} join("\t", @TSV_COLS), "\n";
  for my $r (@rows) {
    my @f = ($r->{name}, $r->{kind}, $r->{family}, $r->{lambda});
    push @f, map { $r->{contract} ? $r->{contract}{$_} : 'UNCLASSIFIED' } @CONTRACT_ORDER;
    push @f, $r->{flags}, $r->{summary};
    s/\t/ /g for @f;
    print {$fh} join("\t", @f), "\n";
  }
  close $fh;
}

# ── Write the Markdown ───────────────────────────────────────────────────
# The §10 rule text, quoted per family so the readable table carries the rule
# a backend author must implement beside the names it governs.  Kept here and
# NOT re-derived from ir-spec.md: a markdown table cell is not parseable text,
# and a silently-empty quote would be worse than a stated one.
my %SPEC10_RULE = (
  'numeric' => 'numify operands (§3.1), return raw number; overload hook first; `/` yields a double when inexact; `%` follows Perl sign rules; the shifts truncate to integer (Inf→0) and clamp a shift count ≥ the word size to 0',
  'bitwise' => 'overload hook first; then ONE mode decision (`%p-bitwise-operand-kind`): the op is NUMERIC iff an operand carries a number, else it STRINGIFIES both operands and operates byte by byte',
  'numeric-compare' => 'numify; return `1`/`""` (`<=>` −1/0/1; NaN comparisons → `""`/undef)',
  'string' => 'stringify operands (§3.2), return raw string; Perl\'s `$_`-default forms arrive with `$_` already explicit in the tree (§8)',
  'string-compare' => 'stringify; return `1`/`""`',
  'logical' => 'short-circuit macros returning operand values (§3.4)',
  'assignment' => 'store per §2.2; a list assignment used as a VALUE is two-faced (task #721): scalar/void yields the number of elements the RHS produced, LIST context yields the LHS *lvalues* after the assignment',
  'compound-assignment' => 'read-modify-write; boxed macros store back via box-set/setf per place shape, `-raw` twins are `(setf slot NEW)` with the identical NEW form; `&&=`/`||=`/`//=` short-circuit and store the RHS unchanged',
  'increment' => 'numeric ±1 on the box/slot; `p-++` on a pure-alpha string does Perl string increment (`"az"→"ba"`)',
  'elements' => 'reads unbox scalars, keep reference boxes (§2.3–2.4); writes through `p-setf` autovivify intermediate refs; `p-delete` returns the removed value',
  'slice-delete' => 'every one flattens its key/index arguments alike (`%p-flatten-slice-args`), and every one answers nil for an EMPTY slice — undef in scalar context, the empty list in list context, per [perl #29127]',
  'aggregate-builtin' => 'Perl signatures; `p-sort` default is string order, comparator lambda gets `$a`/`$b`; `p-defined` returns `1`/`""`.  `%p-sort-classic` and `%p-push1` are sugar — rewrite them back and nothing is lost',
  'regex' => 'match/substitute/transliterate against a box (writes back for s///, tr///); sets §8 match state; list context returns captures.  A FAILED `m//` answers by context and by NOTHING else: scalar/void gives `""`, LIST context gives the EMPTY LIST, spelled `(%p-empty-list)`',
  'compiled-regex' => 'a Regexp OBJECT, not a string: it carries its own flags and identity, and stringifies as perl\'s `(?^flags:SOURCE)` wrapper.  A pattern that is exactly ONE interpolated qr *is* that qr (the outer modifiers are ignored); a qr used as PART of a larger pattern embeds its wrapper verbatim',
  'io' => 'Perl builtins; bareword handles are symbols; `p-open` boxes its handle argument.  2-arg `p-open` parses pipe/dup modes; `p-close` on a pipe handle reaps the child and sets `$?`',
  'command-capture' => 'wantarray-sensitive, exactly like `p-readline`: scalar/void yields the whole captured stdout as one string, LIST context yields it SPLIT INTO `$/` RECORDS',
  'introspection' => '§7; `p-caller` returns package but file/line are stubs (divergence)',
  'context-frame' => 'names, not operations: each expands to exactly the `let`/`lambda` shape it replaced, so a translator implements the expansion and nothing else',
  'declaration' => 'names carrying the compiler\'s own VERDICTS — a binding\'s class, a parameter\'s class, a sub\'s proven facts.  Every one expands to exactly the form it replaced, and every set is CLOSED.  A translator may drop all three vocabularies and still produce a correct program',
);

{
  my %by_fam;
  push @{ $by_fam{ $_->{family} } }, $_ for @rows;
  my (@spec_fams, @added_fams);
  for my $f (sort keys %by_fam) {
    next if $f eq 'UNCLASSIFIED';
    push @{ $FAMILY_META{$f}{spec} ? \@spec_fams : \@added_fams }, $f;
  }

  my $n_contract = grep { $_->{contract} } @rows;
  my @stale = grep { !$exported{ $CITED_ALIAS{$_} // $_ } } @SPEC10_CITED;

  open my $fh, '>:encoding(UTF-8)', $out_md or die "ir-inventory.pl: $out_md: $!\n";
  print {$fh} <<"HDR";
# The PCL IR op inventory

**GENERATED — do not edit.**  Regenerate with `tools/ir-inventory.pl` (writes
this file and the machine form `docs/ir-op-inventory.tsv`).  The gate row
`Pl/t/ir-inventory-01.t` fails when the checked-in files are not what the tool
produces today.

This is the PORT LIST: every name a PCL-emitted file can contain, taken from
the LOADED runtime's `:pcl` external symbols — which is the evaluated
`(:export …)` list, and therefore also sees the ops a definer macro generates
(`p-+`, `p-*`, the numeric and string compares and the compound-assignment
family have no textual `defun` anywhere).  The semantics of each op are its
docstring in `cl/pcl-runtime.lisp`; the family RULES are `docs/ir-spec.md` §10
and are quoted below per family.

* names exported: **@{[ scalar @rows ]}**
* families: **@{[ scalar @spec_fams ]}** with an ir-spec §10 rule, **@{[ scalar @added_fams ]}** without one
* with a machine-readable `Contract:` tail: **$n_contract** of @{[ scalar @rows ]}
* UNCLASSIFIED (no family rule matches): **@{[ scalar @{ $by_fam{UNCLASSIFIED} // [] } ]}**

The contract columns come from a final `Contract:` paragraph of the op's own
docstring — the runtime is the spec, so the machine-readable form lives where
the prose does.  The grammar (keys, closed value sets, and what each means) is
normative in `docs/ir-spec.md` §10.  `UNCLASSIFIED` in a contract column means
the op has no tail yet, never a default.
HDR

  if (@stale) {
    my @wrong    = grep { !$internal{$_} } @stale;
    my @internals = grep {  $internal{$_} } @stale;
    print {$fh} "\n## Citations in ir-spec §10 that are not IR names\n\n"
      . "Names the §10 table prints as family members that the runtime does "
      . "NOT export.  A backend author works from that table, so each one is "
      . "a name they would look for and not find.\n";
    if (@wrong) {
      print {$fh} "\n**No such symbol at all** — the citation is a Perl-facing "
        . "spelling, not the runtime name (" . scalar(@wrong) . "):\n\n";
      print {$fh} "* `$_`\n" for @wrong;
    }
    if (@internals) {
      print {$fh} "\n**Exists, but INTERNAL to `:pcl`** — never emitted bare, "
        . "so it is not part of the IR's vocabulary: either a runtime-only "
        . "helper, or an op the emitter writes package-QUALIFIED ("
        . scalar(@internals) . "):\n\n";
      print {$fh} "* `$_`\n" for @internals;
    }
  }

  for my $group ([ 'Families with an ir-spec §10 rule', \@spec_fams ],
                 [ 'Families ir-spec §10 has no row for', \@added_fams ],
                 [ 'UNCLASSIFIED', [ grep { $by_fam{$_} } 'UNCLASSIFIED' ] ]) {
    my ($title, $fams) = @$group;
    next unless @$fams;
    print {$fh} "\n# $title\n";
    for my $f (@$fams) {
      my $meta = $FAMILY_META{$f} // {};
      print {$fh} "\n## $f (" . scalar(@{ $by_fam{$f} }) . ")\n\n";
      if ($meta->{spec}) {
        print {$fh} "ir-spec §10 row **$meta->{spec}** — "
          . ($SPEC10_RULE{$f} // '(rule not quoted here)') . "\n\n";
        # A §10 family may ALSO carry a note; the one that does is `logical`,
        # whose `p-` row is unreadable without it.
        print {$fh} "$meta->{note}\n\n" if $meta->{note};
      }
      elsif ($meta->{note}) {
        print {$fh} "*No ir-spec §10 row.*  $meta->{note}\n\n";
      }
      else {
        print {$fh} "*No ir-spec §10 row and no note.*\n\n";
      }
      print {$fh} "| name | kind | lambda list | ctx | coerce | magic | dies | dynamic | phase | host |\n";
      print {$fh} "|---|---|---|---|---|---|---|---|---|---|\n";
      for my $r (sort { $a->{name} cmp $b->{name} } @{ $by_fam{$f} }) {
        my @c = map { $r->{contract} ? $r->{contract}{$_} : '—' } @CONTRACT_ORDER;
        my $lam = $r->{lambda};
        $lam =~ s/\|/\\|/g;
        $lam = '' if $lam eq '';
        printf {$fh} "| `%s` | %s | `%s` | %s |\n",
          $r->{name}, $r->{kind}, $lam, join(' | ', @c);
      }
    }
  }
  close $fh;
}

# ── Report ───────────────────────────────────────────────────────────────
if (!$quiet) {
  my %fam_count;
  $fam_count{ $_->{family} }++ for @rows;
  my $n_contract = grep { $_->{contract} } @rows;
  my @stale = grep { !$exported{ $CITED_ALIAS{$_} // $_ } } @SPEC10_CITED;
  printf "ir-inventory: %d exported names, %d families, %d with a Contract: tail, %d UNCLASSIFIED\n",
    scalar @rows, scalar keys %fam_count, $n_contract, ($fam_count{UNCLASSIFIED} // 0);
  printf "  ir-spec \x{a7}10 cites %d names; %d are NOT exported: %s\n",
    scalar @SPEC10_CITED, scalar @stale, join(' ', @stale) if @stale;
  print "  wrote $out_tsv\n  wrote $out_md\n";
}
exit 0;

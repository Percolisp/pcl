# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package Pl::PExpr::Config;

use v5.20;
use strict;
use warnings;

use Moo;

# Postfix operators
has postfix => (
  is        => 'ro',
  default   => sub {
    return {
      '++'  => 1,
      '--'  => 1,
      # Anyone has value 2, only postfix.
    };
  },
);

# Prefix operators
# XXXX Are there others than ++/-- (and '!'?). The rest can be seen as
#      one parameter operators?
has prefix => (
  is        => 'ro',
  default   => sub {
    return {
      '++'  => 92,
      '--'  => 92,
      '~'   => 91,
      '\\'  => 91,
      '-f'  => 1,
      '-r'  => 1,
      '-w'  => 1,
      '-e'  => 1,   # File exists
      '-z'  => 1,   # File has zero size
      '-s'  => 1,   # File has non-zero size (returns size)
      '-d'  => 1,   # File is a directory
      '-l'  => 1,   # File is a symbolic link
      '-p'  => 1,   # File is a named pipe
      '-S'  => 1,   # File is a socket
      '-b'  => 1,   # File is a block special file
      '-c'  => 1,   # File is a character special file
      '-t'  => 1,   # Filehandle is opened to a tty
      '-u'  => 1,   # File has setuid bit set
      '-g'  => 1,   # File has setgid bit set
      '-k'  => 1,   # File has sticky bit set
      '-T'  => 1,   # File is a text file
      '-B'  => 1,   # File is a binary file
      '-M'  => 1,   # Age of file (modification time)
      '-A'  => 1,   # Age of file (access time)
      '-C'  => 1,   # Age of file (inode change time)
      '-x'  => 1,   # File is executable
      '-o'  => 1,   # File is owned by effective uid
      '-O'  => 1,   # File is owned by real uid
      '-R'  => 1,   # File is readable by real uid
      '-W'  => 1,   # File is writable by real uid
      '-X'  => 1,   # File is executable by real uid
      '!'   => 90,
      # etc.
    };
  },
);

# Named unary operators - these take ONE term only, with high precedence.
# e.g., "defined $x && $y" parses as "(defined $x) && $y", not "defined($x && $y)"
# From perldoc perlop: these bind tighter than binary operators
has named_unary => (
  is        => 'ro',
  default   => sub {
    return {
      # Core named unary
      'defined' => 1,
      'ref'     => 1,
      'scalar'  => 1,
      'exists'  => 1,
      'delete'  => 1,
      # String functions
      'chr'     => 1,
      'ord'     => 1,
      'length'  => 1,
      'lc'      => 1,
      'uc'      => 1,
      'fc'      => 1,
      'lcfirst' => 1,
      'ucfirst' => 1,
      'quotemeta' => 1,
      'hex'     => 1,
      'oct'     => 1,
      # Math functions
      'abs'     => 1,
      'int'     => 1,
      'sqrt'    => 1,
      'sin'     => 1,
      'cos'     => 1,
      'exp'     => 1,
      'log'     => 1,
      'rand'    => 1,
      'srand'   => 1,
      # File tests (single arg)
      'readlink' => 1,
      'stat'    => 1,
      'lstat'   => 1,
      # Misc
      'caller'  => 1,
      'do'      => 1,
      'eval'    => 1,
      'evalbytes' => 1,
      'wantarray' => 1,
      'prototype' => 1,
      # use overload introspection
      'overloaded'      => 1,
      'overload-strval' => 1,
    };
  },
);

# Operator precedence and associativity table
# Note: ?: (ternary) is handled specially in parse(), not in this table
# XXXX m// without '=~'?? s///??
# Should document for '=' etc, when a L-value should be generated?
has precedences => (
  is        => 'ro',
  default   => sub {
      #  See perldoc perlop. Look at 'chained' (e.g. 5 < $x < 10).
      return {
        # from perldoc perlop: print ++($foo = "99");      # prints "100"
        # Need to specify that something should return "lvalue"??
        '--'  => { assoc => 'x', no => 1, prec => 92 }, # (nonassociative)
        '++'  => { assoc => 'x', no => 1, prec => 92 }, # (nonassociative)
        '**'  => { assoc => 'r', no => 2, prec => 91 },
        '!'   => { assoc => 'r', no => 1, prec => 90 },
        '~'   => { assoc => 'r', no => 1, prec => 90 }, # Bitwise or.
        '~.'  => { assoc => 'r', no => 1, prec => 90 },
        '\\'  => { assoc => 'r', no => 1, prec => 90 }, # Ref or array etc.
        # Note: Cast operators ($$ref, @$ref, %$ref, &$ref, *$ref) are handled
        # specially in PExpr::op_info() to avoid hash key conflicts with *, etc.
        '=~'  => { assoc => 'l', no => 2, prec => 85 },
        '!~'  => { assoc => 'l', no => 2, prec => 85 },
        '*'   => { assoc => 'l', no => 2, prec => 80 },
        '/'   => { assoc => 'l', no => 2, prec => 80 },
        '%'   => { assoc => 'l', no => 2, prec => 80 },
        'x'   => { assoc => 'l', no => 2, prec => 80 },
        '+'   => { assoc => 'l', no => 2, prec => 60 },
        '-'   => { assoc => 'l', no => 2, prec => 60 },
        '.'   => { assoc => 'l', no => 2, prec => 60 },
        '<<'  => { assoc => 'l', no => 2, prec => 55 },
        '>>'  => { assoc => 'l', no => 2, prec => 55 },

        # Range operator (non-associative)
        '..'  => { assoc => 'x', no => 2, prec => 17 },
        '...' => { assoc => 'x', no => 2, prec => 17 },  # Three-dot range

        # Ternary operator (right-associative: a ? b : c ? d : e = a ? b : (c ? d : e))
        # Both ? and : have same prec so : acts as boundary when scanning for condition extent
        '?'   => { assoc => 'r', no => 3, prec => 15 },
        ':'   => { assoc => 'x', no => 0, prec => 15 },  # Marker only, not processed as operator

        # XXXX Here are "named unary operators" like chdir, -f, rand, etc.
        #      Do I need to list them all??

        # XXXX File operator don't treat parentheses like fun calls,
        #      more like ! etc.
        '-f'  => { assoc => 'l', no => 1, prec => 52,},
        '-r'  => { assoc => 'l', no => 1, prec => 52,},
        '-w'  => { assoc => 'l', no => 1, prec => 52,},
	'-e'  => { assoc => 'l', no => 1, prec => 52,},
        '-z'  => { assoc => 'l', no => 1, prec => 52,},
        '-s'  => { assoc => 'l', no => 1, prec => 52,},
        '-d'  => { assoc => 'l', no => 1, prec => 52,},
        '-l'  => { assoc => 'l', no => 1, prec => 52,},
        '-p'  => { assoc => 'l', no => 1, prec => 52,},
        '-S'  => { assoc => 'l', no => 1, prec => 52,},
        '-b'  => { assoc => 'l', no => 1, prec => 52,},
        '-c'  => { assoc => 'l', no => 1, prec => 52,},
        '-t'  => { assoc => 'l', no => 1, prec => 52,},
        '-u'  => { assoc => 'l', no => 1, prec => 52,},
        '-g'  => { assoc => 'l', no => 1, prec => 52,},
        '-k'  => { assoc => 'l', no => 1, prec => 52,},
        '-T'  => { assoc => 'l', no => 1, prec => 52,},
        '-B'  => { assoc => 'l', no => 1, prec => 52,},
        '-M'  => { assoc => 'l', no => 1, prec => 52,},
        '-A'  => { assoc => 'l', no => 1, prec => 52,},
        '-C'  => { assoc => 'l', no => 1, prec => 52,},
        '-x'  => { assoc => 'l', no => 1, prec => 52,},
        '-o'  => { assoc => 'l', no => 1, prec => 52,},
        '-O'  => { assoc => 'l', no => 1, prec => 52,},
        '-R'  => { assoc => 'l', no => 1, prec => 52,},
        '-W'  => { assoc => 'l', no => 1, prec => 52,},
        '-X'  => { assoc => 'l', no => 1, prec => 52,},

        'isa' => { assoc => 'x', no => 2, prec => 50 },

        # XXXX Chained < > <= >= lt gt le ge
        '<'   => { assoc => 'r', no => 2, prec => 40, chained => 1 },
        '>'   => { assoc => 'r', no => 2, prec => 40, chained => 1 },
        '<='  => { assoc => 'r', no => 2, prec => 40, chained => 1 },
        '>='  => { assoc => 'r', no => 2, prec => 40, chained => 1 },
        'lt'  => { assoc => 'r', no => 2, prec => 40, chained => 1 },
        'gt'  => { assoc => 'r', no => 2, prec => 40, chained => 1 },
        'le'  => { assoc => 'r', no => 2, prec => 40, chained => 1 },
        'ge'  => { assoc => 'r', no => 2, prec => 40, chained => 1 },

        '=='  => { assoc => 'r', no => 2, prec => 30, chained => 1 },
        '!='  => { assoc => 'r', no => 2, prec => 30, chained => 1 },
        'eq'  => { assoc => 'r', no => 2, prec => 30, chained => 1 },
        'ne'  => { assoc => 'r', no => 2, prec => 30, chained => 1 },
        '<=>' => { assoc => 'l', no => 2, prec => 30 },
        'cmp' => { assoc => 'l', no => 2, prec => 30 },

        # Bitwise operators
        '&'   => { assoc => 'l', no => 2, prec => 25 }, # Bitwise AND
        '|'   => { assoc => 'l', no => 2, prec => 24 }, # Bitwise OR
        '^'   => { assoc => 'l', no => 2, prec => 24 }, # Bitwise XOR
        # Dotted string bitwise operators (always string, never numeric)
        '&.'  => { assoc => 'l', no => 2, prec => 25 }, # String bitwise AND
        '|.'  => { assoc => 'l', no => 2, prec => 24 }, # String bitwise OR
        '^.'  => { assoc => 'l', no => 2, prec => 24 }, # String bitwise XOR

        # Logical operators (higher precedence than //)
        '&&'  => { assoc => 'l', no => 2, prec => 20 },
        '||'  => { assoc => 'l', no => 2, prec => 19 },
        '^^'  => { assoc => 'l', no => 2, prec => 19 }, # Perl 5.40 logical XOR (same prec as ||)

        # Defined-or operator (same precedence as ||)
        '//'  => { assoc => 'l', no => 2, prec => 19 },

        # XXXX Returns an lvalue. Need to represent that??
        '='   => { assoc => 'r', no => 2, prec =>  8 },
        '+='  => { assoc => 'r', no => 2, prec =>  8 },
        '-='  => { assoc => 'r', no => 2, prec =>  8 },
        '*='  => { assoc => 'r', no => 2, prec =>  8 },
        '/='  => { assoc => 'r', no => 2, prec =>  8 },
        '%='  => { assoc => 'r', no => 2, prec =>  8 },
        '**=' => { assoc => 'r', no => 2, prec =>  8 },
        '//=' => { assoc => 'r', no => 2, prec =>  8 },
        '&&=' => { assoc => 'r', no => 2, prec =>  8 },
        '||=' => { assoc => 'r', no => 2, prec =>  8 },
        '.='  => { assoc => 'r', no => 2, prec =>  8 },
        'x='  => { assoc => 'r', no => 2, prec =>  8 },
        '&='  => { assoc => 'r', no => 2, prec =>  8 },
        '|='  => { assoc => 'r', no => 2, prec =>  8 },
        '^='  => { assoc => 'r', no => 2, prec =>  8 },
        '&.=' => { assoc => 'r', no => 2, prec =>  8 }, # String bitwise AND assign
        '|.=' => { assoc => 'r', no => 2, prec =>  8 }, # String bitwise OR assign
        '^.=' => { assoc => 'r', no => 2, prec =>  8 }, # String bitwise XOR assign
        '<<=' => { assoc => 'r', no => 2, prec =>  8 },
        '>>=' => { assoc => 'r', no => 2, prec =>  8 },

        # Lower precedence than '='.
        # Really. Try: p -E '$q=1; $w=14; $x=($z = $q+5 and $w); say "$z, $x";'
        # Gives: 6, 14
        not   => { assoc => 'l', no => 1, prec =>  3 },
        and   => { assoc => 'l', no => 2, prec =>  2 }, # higher than or/xor
        or    => { assoc => 'l', no => 2, prec =>  1 },
        xor   => { assoc => 'l', no => 2, prec =>  1 },
      };
    },
);

# Functions with known number of parameters (and types).
# Should also have declarations.

# XXXX Also need to rething subs with declared prototypes etc. (Does
#      constants for e.g. tcp/ip integratiion need special handling??)

# XXXX Need to be extensible, for Prototypes.

# XXXXX Need to flag for if it returns different in scalar/list contexts.

# XXXXX Would it be shorter to list all funs not defaulting to $_?

# -1 means list. -1x means x parameters before a list.
# -2 means use $_ as default.
# -3 means use @_ as default (in sub) or @ARGV (in main).

# Perl's control-flow operators.  They are deliberately NOT in
# known_no_of_params: their operand is a LABEL, not a value, and the statement
# layer parses them.  But they ARE core callable words, and anything asking
# "is this bareword a call or a string?" has to count them — `push @a, last;`
# is a `last`, not the string "last" (probed; task #266 measured it live in
# loopctl.t and my.t).
has control_flow_ops => (
  is      => 'ro',
  default => sub { return { map { $_ => 1 } qw(last next redo goto return) } },
);

# statement_keywords: the words that open (or modify) a STATEMENT.  They are
# grammar, never functions — perl will not let a sub of these names be called
# as one, and neither can PCL: the statement layer has already consumed them by
# the time an expression sees one, so a bareword survivor is a shape the
# compiler could not lower.
#
# THE ONE COPY of both, and they are two questions, not one:
#   statement_modifiers — the six that can TRAIL an expression (`EXPR if COND`).
#                         Asked by the modifier splitters in Parser.pm and
#                         Parser2.pm, which used to inline this regex four times.
#   statement_keywords  — those six PLUS `else`/`elsif`: the words that can only
#                         ever be grammar.  Asked by ExprToCL when a bareword
#                         reaches the funcall generator.
# The two WIDER sets elsewhere answer different questions and stay where they
# are: `%SIG_DEFAULT_KEYWORDS` (Parser.pm) is "does this bareword make a
# signature default swallow", and `%KEYWORDS` (InterpScan) is perl's whole
# keyword list — builtins included — for the interpolation weigher.
#
# Why the keyword set exists (task #374): `if` is in ExprToCL's %RUNTIME_NAMES,
# so a bareword one lowered to the p-if MACRO with whatever arity it had —
# `my $x = if if if` (legal perl when a lexical sub is named `if`, and
# t/op/lexsub.t asserts it) emitted a zero-argument `(p-if)` whose
# MACROEXPANSION error killed the whole file at load.
our %STATEMENT_MODIFIERS = map { $_ => 1 } qw(if unless while until for foreach);
our %STATEMENT_KEYWORDS  = (%STATEMENT_MODIFIERS, map { $_ => 1 } qw(else elsif));

sub is_statement_modifier { return $STATEMENT_MODIFIERS{ $_[0] // '' } ? 1 : 0 }

has statement_keywords => (
  is      => 'ro',
  default => sub { return { %STATEMENT_KEYWORDS } },
);

has statement_modifiers => (
  is      => 'ro',
  default => sub { return { %STATEMENT_MODIFIERS } },
);

# ----------------------------------------------------------------------
# THE ONE READING of "what context does a core builtin evaluate its Nth
# argument in" (task #2004).  Perl answers it from the builtin's PROTOTYPE:
# every argument at or after the first slurpy (`@`/`%`) slot is LIST context,
# a `$` / `_` / `+` slot is SCALAR, a `*` (glob) slot is SCALAR, and a
# reference slot (`\@`, `\[%@]`, …) is an lvalue container PCL lowers on its
# own — it keeps inheriting.  The VALUES are always measured
# (`prototype("CORE::NAME")`, the same authority the runtime's generated
# `%pcl-core-prototypes` table is built from) — never hand-copied, because a
# hand copy drifts silently.
#
# `child_context` used to answer this from SIX hand-written name regexes, and
# a builtin in none of them INHERITED its caller's context: that is why
# `my $s = sprintf("%02d:%02d", @t[2,1])` formatted only the slice's LAST
# element and `sprintf("%s", f())` called f in SCALAR context (#2004 — a
# silent wrong reaching core Time::Local).  The regexes are gone; what stays
# beside this is (a) the map/grep/sort arm, which is STRUCTURAL (which CHILD
# is the list, not what context it gets) and (b) this table, for the builtins
# perl gives no prototype at all.
#
# `chomp`/`chop` are here rather than in `Pl::Environment::_builtin_prototypes`
# for the reason that table's own comment gives: it is read by codegen paths
# too, and a `(@)` entry there changes how `chomp @a` COMPILES.  This table is
# read for context only.
our %NO_PROTO_ARG_CONTEXT = (
  # perl: `print LIST` / `printf FORMAT, LIST` — the whole argument run is one
  # list, so `printf @a` takes $a[0] as the format (NOT scalar(@a)).
  print   => 'LIST',
  say     => 'LIST',
  printf  => 'LIST',
  system  => 'LIST',
  exec    => 'LIST',
  # chop/chomp take a LIST of lvalues (`chop @a`, `chop @h{@k}`, `chop($a,$b)`).
  chop    => 'LIST',
  chomp   => 'LIST',
  # split's pattern/string/limit are scalars although split returns a list.
  split   => 'SCALAR',
  # `eval EXPR` imposes scalar context on its operand (task #1249(4)).
  eval    => 'SCALAR',
  defined => 'SCALAR',
);

my %CORE_SLOTS;    # name -> [\@slot_kinds, $slurpy_index_or_undef], memoized
my %CORE_KEYWORD;  # name -> 0/1, memoized

# IS THIS NAME A PERL KEYWORD AT ALL?  Measured, never listed: perl's
# `prototype("CORE::NAME")` DIES ("Can't find an opnumber for") when NAME is
# not a keyword and answers a string or undef when it is — so the die IS the
# discriminator, and the answer comes from the perl that is running us rather
# than from a hand-maintained list that drifts.
#
# It exists because the compiler's `%RUNTIME_NAMES` is NOT a list of perl
# builtins: beside `print` and `reverse` it holds the runtime's own internal
# operators (`flatten`, `hash`, `aref`, `setf`, `regex`, `box`, …), and a plain
# call lowered to `p-NAME` on that table alone.  A user `sub flatten {…}` was
# therefore hijacked at its call site — `hash(1,2)` printed "12", `let`/`setf`
# failed inside a MACROEXPANSION and took the whole file with them (task
# #2100).  A perl keyword keeps winning over a declared sub of the same name,
# because that is what perl does (`sub reverse {…}; reverse(...)` calls the
# BUILTIN unless imported or `use subs`).
sub is_core_keyword {
  my $name = shift;
  return 0 if !defined $name || !length $name;
  return $CORE_KEYWORD{$name} if exists $CORE_KEYWORD{$name};
  my $ok = eval { my $p = prototype("CORE::$name"); 1 };
  return $CORE_KEYWORD{$name} = ($ok ? 1 : 0);
}

# Ask the running perl for a builtin's prototype.  Not a keyword at all =>
# perl dies ("Can't find an opnumber for"); a keyword with no prototype (`if`,
# `print`, `sort`, …) => undef.  Both mean "this table has nothing to say".
sub _core_slots {
  my $name = shift;
  return $CORE_SLOTS{$name} if exists $CORE_SLOTS{$name};
  my $p = eval { prototype("CORE::$name") };
  return $CORE_SLOTS{$name} = undef if !defined $p || $p eq '';
  my (@slots, $slurpy);
  while ($p =~ /\G(.)/gcs) {
    my $c = $1;
    next if $c eq ';';                       # start of the optional tail
    if ($c eq '\\') {                        # \@  \%  \[$@%&*]  — one ref slot
      $p =~ /\G\[[^\]]*\]/gc or $p =~ /\G./gcs;
      push @slots, 'ref';
      next;
    }
    if ($c eq '@' || $c eq '%') { $slurpy = scalar(@slots); last }
    push @slots, $c eq '$' || $c eq '_' || $c eq '+' ? 'scalar'
               : $c eq '*'                           ? 'glob'
               : $c eq '&'                           ? 'code'
               :                                       'other';
  }
  return $CORE_SLOTS{$name} = [\@slots, $slurpy];
}

# 'LIST' | 'SCALAR' | undef (= inherit the caller's context), for ARGUMENT
# index $idx (0-based) of core builtin $name.
sub core_arg_context {
  my ($name, $idx) = @_;
  return $NO_PROTO_ARG_CONTEXT{$name} if exists $NO_PROTO_ARG_CONTEXT{$name};
  my $rec = _core_slots($name);
  return undef if !$rec;
  my ($slots, $slurpy) = @$rec;
  return 'LIST' if defined $slurpy && $idx >= $slurpy;
  my $kind = $idx <= $#$slots ? $slots->[$idx] : undef;
  return undef if !defined $kind;
  return 'SCALAR' if $kind eq 'scalar' || $kind eq 'glob';
  return undef;                              # ref / code slots inherit
}

# perldoc perlfun:
has known_no_of_params => (
  is        => 'ro',
  default   => sub {
    return {
      # Special compile-time tokens (zero-arg constants)
      '__FILE__' => 0,
      '__LINE__' => 0,
      '__PACKAGE__' => 0,

      open       => [1, 2, 3, -1],  # 1-arg reopen, 2-arg mode+file, 3-arg, 4+ for pipe
      sysopen    => [3, 4],         # sysopen FH, PATH, O_FLAGS [, PERMS] (#730)
      close      => [0, 1],         # close or close FH
      pos        => [0, 1, -2],  # pos or pos SCALAR (defaults to $_)
      grep       => -12,        # grep BLOCK|EXPR, LIST (1 before list)
      time       => 0,
      times      => 0,
      fork       => 0,          # fork() — real fork(2) via sb-posix (see p-fork)
      wait       => 0,          # wait() — reap any child, sets $?
      waitpid    => [1, 2],     # waitpid(PID, FLAGS)
      getppid    => 0,          # getppid()
      getlogin   => 0,          # getlogin()
      getpgrp    => [0, 1],     # getpgrp or getpgrp PID
      setpgrp    => [0, 1, 2],  # setpgrp / setpgrp PID / setpgrp PID, PGRP
      getpriority => 2,         # getpriority WHICH, WHO
      setpriority => 3,         # setpriority WHICH, WHO, PRIORITY
      kill       => -12,        # kill SIGNAL, LIST (1 before list)
      exec       => -1,         # exec LIST
      localtime  => [0,   1],
      gmtime     => [0,   1],

      # Functions for SCALARs or strings
      # "chomp", "chop", "chr", "crypt", "fc", "hex", "index", "lc",
      # "lcfirst", "length", "oct", "ord", "pack", "q//", "qq//", "reverse",
      # "rindex", "sprintf", "substr", "tr///", "uc", "ucfirst", "y///"
      chomp      => [-1, -2],
      chop       => [-1, -2],
      chr        => [1,  -2],
      crypt      => 2,
      fc         => [1,  -2],
      lc         => [1,  -2],
      lcfirst    => [1,  -2],
      uc         => [1,  -2],
      ucfirst    => [1,  -2],
      hex        => [1,  -2],
      glob       => [1,  -2],   # glob EXPR or bare glob (defaults to $_)
      index      => [2,   3],
      lcfirst    => [1,  -2],
      length     => [1,  -2],
      oct        => [1,  -2],
      ord        => [1,  -2],
      pack       => -11,            # pack TEMPLATE, LIST (1 before list)
      quotemeta  => [1,  -2],
      reverse    => [-1, -2],
      rindex     => [2,   3],
      sprintf    => -11,        # One parameter as default, before a list.
      substr     => [2, 3, 4],
      # XXXXX Handle s///, tr///??

      #  Numeric functions
      # "abs", "atan2", "cos", "exp", "hex", "int", "log", "oct", "rand",
      # "sin", "sqrt", "srand"
      abs        => [1,  -2],
      atan2      => 2,
      cos        => [1,  -2],
      exp        => [1,  -2],
      int        => [1,  -2],
      log        => [1,  -2],
      rand       => [0,   1],
      sin        => [1,  -2],
      sqrt       => [1,  -2],
      srand      => [0,   1],

      # Functions for real @ARRAYs
      #  "each", "keys", "pop", "push", "shift", "splice", "unshift",
      #  "values"
      shift      => [1,  -3],   # shift ARRAY or shift (defaults to @_/@ARGV)
      pop        => [1,  -3],   # pop ARRAY or pop (defaults to @_/@ARGV)
      push       => -12,        # push ARRAY, LIST (2+ args, first is array)
      unshift    => -12,        # unshift ARRAY, LIST
      splice     => [1, 2, 3, 4, -1],  # splice ARRAY [, OFFSET [, LENGTH [, LIST]]]

      # Functions for list data
      # "grep", "join", "map", "qw//", "reverse", "sort", "unpack"

      # Functions for real %HASHes
      # "delete", "each", "exists", "keys", "values"

      # Input and output functions
      # "binmode", "close", "closedir", "dbmclose", "dbmopen", "die", "eof",
      # "fileno", "flock", "format", "getc", "print", "printf", "read",
      # "readdir", "readline", "rewinddir", "say", "seek", "seekdir",
      # "select", "syscall", "sysread", "sysseek", "syswrite", "tell",
      # "telldir", "truncate", "warn", "write"
      binmode    => [1, 2],       # binmode FH or binmode FH, LAYER
      eof        => [0, 1],       # eof or eof FH
      fcntl      => 3,            # fcntl FH, FUNCTION, SCALAR
      fileno     => 1,            # fileno FH
      flock      => 2,            # flock FH, OPERATION
      getc       => [0, 1],       # getc or getc FH
      read       => [3, 4],       # read FH, SCALAR, LENGTH [, OFFSET]
      readline   => [0, 1],       # readline FH or readline
      seek       => 3,            # seek FH, POS, WHENCE
      tell       => [0, 1],       # tell or tell FH
      truncate   => 2,            # truncate FILE/FH, LENGTH
      sysread    => [3, 4],       # sysread FH, SCALAR, LENGTH [, OFFSET]
      syswrite   => [2, 3, 4],    # syswrite FH, SCALAR [, LENGTH [, OFFSET]]
      sysseek    => 3,            # sysseek FH, POS, WHENCE

      # Socket operations (AF_INET/AF_UNIX, SOCK_STREAM/DGRAM via sb-bsd-sockets)
      socket     => 4,            # socket SOCK, DOMAIN, TYPE, PROTOCOL
      socketpair => 5,            # socketpair S1, S2, DOMAIN, TYPE, PROTOCOL
      bind       => 2,            # bind SOCK, NAME
      connect    => 2,            # connect SOCK, NAME
      listen     => 2,            # listen SOCK, QUEUESIZE
      accept     => 2,            # accept NEWSOCK, GENERICSOCK
      send       => [3, 4],       # send SOCK, MSG, FLAGS [, TO]
      recv       => 4,            # recv SOCK, SCALAR, LEN, FLAGS
      shutdown   => 2,            # shutdown SOCK, HOW
      getsockname => 1,           # getsockname SOCK
      getpeername => 1,           # getpeername SOCK
      getprotobyname => 1,        # getprotobyname NAME
      getprotobynumber => 1,      # getprotobynumber NUMBER
      getprotoent => 0,           # getprotoent
      setprotoent => 1,           # setprotoent STAYOPEN
      endprotoent => 0,           # endprotoent
      getservbyname => 2,         # getservbyname NAME, PROTO
      getservbyport => 2,         # getservbyport PORT, PROTO
      getservent => 0,            # getservent
      setservent => 1,            # setservent STAYOPEN
      endservent => 0,            # endservent
      gethostbyname => 1,         # gethostbyname NAME
      gethostbyaddr => 2,         # gethostbyaddr ADDR, ADDRTYPE
      gethostent => 0,            # gethostent
      sethostent => 1,            # sethostent STAYOPEN
      endhostent => 0,            # endhostent
      getnetbyname => 1,          # getnetbyname NAME
      getnetbyaddr => 2,          # getnetbyaddr NET, ADDRTYPE
      getnetent => 0,             # getnetent
      setnetent => 1,             # setnetent STAYOPEN
      endnetent => 0,             # endnetent
      formline => -1,             # formline PICTURE, LIST (dies: ruled with format)
      setsockopt => 4,            # setsockopt SOCK, LEVEL, OPTNAME, OPTVAL
      getsockopt => 3,            # getsockopt SOCK, LEVEL, OPTNAME

      # Directory operations
      opendir    => 2,            # opendir DH, DIRNAME
      readdir    => 1,            # readdir DH
      closedir   => 1,            # closedir DH
      rewinddir  => 1,            # rewinddir DH
      telldir    => 1,            # telldir DH
      seekdir    => 2,            # seekdir DH, POS

      # File stat functions
      stat       => [0, 1, -2],   # stat FILE or stat FH or stat (uses $_)
      lstat      => [0, 1, -2],   # lstat FILE or lstat (uses $_)

      # File/directory operations
      chdir      => [0, 1],       # chdir or chdir DIR
      mkdir      => [0, 1, 2, -2], # mkdir [DIR [, MODE]] defaults to $_
      rmdir      => [0, 1, -2],   # rmdir [DIR] defaults to $_
      rename     => 2,            # rename OLD, NEW
      chmod      => -12,          # chmod MODE, LIST (1 before list)
      umask      => [0, 1],       # umask or umask EXPR
      link       => 2,            # link OLD, NEW
      symlink    => 2,            # symlink OLD, NEW
      readlink   => [1, -2],      # readlink EXPR or readlink (uses $_)
      chown      => -1,           # chown UID, GID, LIST
      utime      => -1,           # utime ATIME, MTIME, LIST
      getcwd     => 0,            # getcwd() - Cwd module but common
      cwd        => 0,            # cwd() - alias for getcwd

      # "say" is available only if the "say" feature is enabled or if it is
      # prefixed with "CORE::". The "say" feature is enabled automatically
      # with a "use v5.10" (or higher) declaration in the current scope.

      # Functions for fixed-length data or records
      # "pack", "read", "syscall", "sysread", "sysseek", "syswrite",
      # "unpack", "vec"
      unpack     => [1, 2, -2], # unpack TEMPLATE [, EXPR] defaults to $_

      # ...Etc...


      # Group database functions
      getgrent   => 0,
      setgrent   => 0,
      endgrent   => 0,
      getgrgid   => 1,
      getgrnam   => 1,
      # Passwd database functions (sibling family)
      getpwent   => 0,
      setpwent   => 0,
      endpwent   => 0,
      getpwuid   => 1,
      getpwnam   => 1,

      # Linux varants:
      # -*X*, binmode, chmod, chown, chroot, crypt, dbmclose,
      # dbmopen, dump, endhostent, endnetent, endprotoent,
      # endpwent, endservent, exec, fcntl, flock, fork,
      # gethostbyname, gethostent, getlogin, getnetbyaddr,
      # getnetbyname, getnetent, getppid,
      # getprotoent, getpwent, getpwnam, getpwuid,
      # getservbyport, getservent, getsockopt, glob, ioctl, kill,
      # link, lstat, msgctl, msgget, msgrcv, msgsnd, open, pipe,
      # readlink, rename, select, semctl, semget, semop,
      # sethostent, setnetent, setpriority, setprotoent,
      # setpwent, setservent, setsockopt, shmctl, shmget, shmread,
      # shmwrite, socket, socketpair, stat, symlink, syscall,
      # sysopen, system, times, truncate, umask, unlink, utime,
      # wait, waitpid

      # Object-oriented functions
      bless      => [1,   2],   # bless(REF) or bless(REF, CLASS)
      ref        => [1,  -2],   # ref(EXPR) or ref
      tied       => 1,
      tie        => [2,  -1],
      untie      => 1,

      # Context and type functions
      scalar     => 1,          # scalar(EXPR) - force scalar context
      wantarray  => 0,          # wantarray() - check calling context
      caller     => [0, 1],     # caller() or caller(LEVEL)
      defined    => [1, -2],    # defined(EXPR) or defined
      prototype  => [0, 1],     # prototype or prototype FUNCTION
      undef      => [0, 1],     # undef or undef EXPR (undefines variable)

      # List/hash functions (in runtime but need specs)
      split      => [0, 1, 2, 3, -2],  # split [/PATTERN/ [, EXPR [, LIMIT]]] defaults to $_
      join       => -12,        # join EXPR, LIST (1 before list)
      keys       => 1,          # keys HASH
      values     => 1,          # values HASH
      each       => 1,          # each HASH
      exists     => 1,          # exists EXPR (hash/array element)
      delete     => 1,          # delete EXPR (hash/array element)
      sort       => -1,         # sort [SUBNAME|BLOCK] LIST
      map        => -1,         # map BLOCK|EXPR, LIST

      # Error handling
      die        => -1,         # die LIST
      warn       => -1,         # warn LIST
      exit       => [0, 1],     # exit or exit EXPR
      system     => -1,         # system CMD or system PROG, ARGS
      # readpipe EXPR / readpipe (defaults to $_) — the NAMED spelling of
      # `CMD` / qx{CMD} / <<`TAG`, and a first-class builtin in its own right
      # (t/op/exec.t rows 21-26).  It emits the SAME p-backtick the term
      # spellings do (gen_funcall_form in Pl::ExprToCL), so its list-context
      # record split and its `use subs` override come for free — task #734.
      readpipe   => [1, -2],    # readpipe EXPR or readpipe (defaults to $_)
      do         => 1,          # do BLOCK or do FILE (always 1 arg)
      eval       => [0, 1, -2], # eval EXPR or eval BLOCK or eval (defaults to $_)
      require    => [0, 1, -2], # require [VERSION|MODULE|FILE]; bare = $_ as file

      vec        => 3,          # vec EXPR, OFFSET, BITS

      # Misc
      sleep      => [0, 1],     # sleep or sleep EXPR
      alarm      => [0, 1],     # alarm or alarm EXPR
      evalbytes  => [0, 1, -2], # evalbytes [EXPR] defaults to $_
      print      => -1,         # print [FH] LIST (special handling)
      say        => -1,         # say [FH] LIST (special handling)
      printf     => -12,        # printf [FH] FORMAT, LIST
    };
  },
);

1;

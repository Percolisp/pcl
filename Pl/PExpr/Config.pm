package Pl::PExpr::Config;

# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

use v5.30;
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
      'wantarray' => 1,
      'prototype' => 1,
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

        '=='  => { assoc => 'l', no => 2, prec => 30 }, # These are non assoc,
        '!='  => { assoc => 'l', no => 2, prec => 30 }, # leave as 'l'. Should
        'eq'  => { assoc => 'l', no => 2, prec => 30 }, # work.
        'ne'  => { assoc => 'l', no => 2, prec => 30 },
        '<=>' => { assoc => 'l', no => 2, prec => 30 },
        'cmp' => { assoc => 'l', no => 2, prec => 30 },

        # Bitwise operators
        '&'   => { assoc => 'l', no => 2, prec => 25 }, # Bitwise AND
        '|'   => { assoc => 'l', no => 2, prec => 24 }, # Bitwise OR
        '^'   => { assoc => 'l', no => 2, prec => 24 }, # Bitwise XOR

        # Logical operators (higher precedence than //)
        '&&'  => { assoc => 'l', no => 2, prec => 20 },
        '||'  => { assoc => 'l', no => 2, prec => 19 },

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
        '<<=' => { assoc => 'r', no => 2, prec =>  8 },
        '>>=' => { assoc => 'r', no => 2, prec =>  8 },

        # Lower precedence than '='.
        # Really. Try: p -E '$q=1; $w=14; $x=($z = $q+5 and $w); say "$z, $x";'
        # Gives: 6, 14
        not   => { assoc => 'l', no => 1, prec =>  3 },
        and   => { assoc => 'l', no => 2, prec =>  1 },
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

# perldoc perlfun:
has known_no_of_params => (
  is        => 'ro',
  default   => sub {
    return {
      # Special compile-time tokens (zero-arg constants)
      '__FILE__' => 0,
      '__LINE__' => 0,
      '__PACKAGE__' => 0,

      open       => [2,   3],
      close      => 1,
      pos        => [0,   1],
      grep       => 2,
      time       => 0,
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
      index      => [2,   3],
      lcfirst    => [1,  -2],
      length     => [1,  -2],
      oct        => [1,  -2],
      ord        => [1,  -2],
      pack       => 2,
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
      splice     => [2, 3, 4, -1],  # splice ARRAY, OFFSET, LENGTH, LIST

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
      fileno     => 1,            # fileno FH
      getc       => [0, 1],       # getc or getc FH
      read       => [3, 4],       # read FH, SCALAR, LENGTH [, OFFSET]
      readline   => [0, 1],       # readline FH or readline
      seek       => 3,            # seek FH, POS, WHENCE
      tell       => [0, 1],       # tell or tell FH
      truncate   => 2,            # truncate FILE/FH, LENGTH
      sysread    => [3, 4],       # sysread FH, SCALAR, LENGTH [, OFFSET]
      syswrite   => [2, 3, 4],    # syswrite FH, SCALAR [, LENGTH [, OFFSET]]
      sysseek    => 3,            # sysseek FH, POS, WHENCE

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
      mkdir      => [1, 2],       # mkdir DIR or mkdir DIR, MODE
      rmdir      => 1,            # rmdir DIR
      rename     => 2,            # rename OLD, NEW
      chmod      => -12,          # chmod MODE, LIST (1 before list)
      getcwd     => 0,            # getcwd() - Cwd module but common
      cwd        => 0,            # cwd() - alias for getcwd

      # "say" is available only if the "say" feature is enabled or if it is
      # prefixed with "CORE::". The "say" feature is enabled automatically
      # with a "use v5.10" (or higher) declaration in the current scope.

      # Functions for fixed-length data or records
      # "pack", "read", "syscall", "sysread", "sysseek", "syswrite",
      # "unpack", "vec"

      # ...Etc...


      # Linux varants:
      # -*X*, binmode, chmod, chown, chroot, crypt, dbmclose,
      # dbmopen, dump, endgrent, endhostent, endnetent, endprotoent,
      # endpwent, endservent, exec, fcntl, flock, fork, getgrent,
      # getgrgid, gethostbyname, gethostent, getlogin, getnetbyaddr,
      # getnetbyname, getnetent, getppid, getpgrp, getpriority,
      # getprotobynumber, getprotoent, getpwent, getpwnam, getpwuid,
      # getservbyport, getservent, getsockopt, glob, ioctl, kill,
      # link, lstat, msgctl, msgget, msgrcv, msgsnd, open, pipe,
      # readlink, rename, select, semctl, semget, semop, setgrent,
      # sethostent, setnetent, setpgrp, setpriority, setprotoent,
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
      undef      => 0,          # undef - returns undefined value

      # List/hash functions (in runtime but need specs)
      split      => [1, 2, 3],  # split /PATTERN/, EXPR, LIMIT
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

      # Misc
      sleep      => [0, 1],     # sleep or sleep EXPR
      print      => -1,         # print [FH] LIST (special handling)
      say        => -1,         # say [FH] LIST (special handling)
      printf     => -12,        # printf [FH] FORMAT, LIST
    };
  },
);

1;

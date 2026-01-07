# PCL - Remaining Work and Limitations

**Current Status:** 29 test files, 1652 tests passing

---

## What Works

### Expressions & Operators
- All arithmetic, string, comparison, logical operators
- Correct operator precedence (92 levels)
- Ternary `? :`
- Range operator `..`
- String repetition `x`

### Control Flow
- `if`/`elsif`/`else`/`unless`
- `while`/`until`
- `for` (C-style)
- `foreach` (list iteration)
- `last`, `next`, `redo`
- Loop labels: `OUTER: for ...`, `next OUTER`, `last OUTER`

### Data Structures
- Scalars, arrays, hashes
- References and dereferencing (`\$x`, `$$ref`, `$arr->[0]`, `$hash->{key}`)
- Array/hash slices
- Anonymous arrays `[]` and hashes `{}`

### Functions
- User-defined subs with signatures: `sub foo($x, $y = 10) { }`
- Default parameter values
- Old-style prototypes (parsed, but see [Prototype Limitations](#prototype-limitations))
- Built-ins: `print`, `say`, `length`, `substr`, `index`, `push`, `pop`, `shift`, `unshift`, `keys`, `values`, `exists`, `delete`, `grep`, `map`, `sort`, `join`, `split`, `reverse`, `sprintf`, `chr`, `ord`, `lc`, `uc`, `abs`, `int`, `sin`, `cos`, `sqrt`, `exp`, `log`, `rand`

### File I/O
- `open`, `close`, `print`, `say` to filehandles
- `<FH>` readline / diamond operator
- `eof`, `tell`, `seek`
- `unlink`, `rename`, `chmod`
- `chdir`, `mkdir`, `rmdir`, `getcwd`

### Time & System
- `time`, `localtime`, `gmtime`
- `sleep`, `exit`
- `system()`, backticks `` `cmd` ``
- `%ENV` (read and write environment variables)

### Regex (CL-PPCRE)
- Match: `$s =~ /pattern/`
- Substitution: `$s =~ s/pat/repl/gimsx`
- Transliteration: `$s =~ tr/from/to/cds`
- Capture groups: `$1`, `$2`, ... `$9` (including in substitution replacement)
- All common metacharacters: `.`, `*`, `+`, `?`, `^`, `$`, `\d`, `\w`, `\s`, `\b`, `[...]`, `|`, `{n,m}`

### Special Variables
- `$$` - process ID

### OO
- `package` declarations
- `bless`, `ref`
- Method calls: `$obj->method()`, `Class->new()`
- Basic method dispatch

### Other
- `use constant`
- `state` variables (inside subs)
- String interpolation
- Comments preserved in output

---

## Known Limitations

### NOT Implemented

#### Special Variables
```perl
$_      # NOT systematically handled
$@      # eval error NOT implemented
$/      # input record separator ignored
$!      # system error NOT implemented
@_      # works in subs, but no systematic handling
```

#### Module System
```perl
use Module;        # NOT implemented
require Module;    # NOT implemented
use Module qw(...);# NOT implemented
```
Only `use constant NAME => VALUE` works.

#### Variable Scoping
```perl
local $var;   # Parsed, but dynamic scoping NOT implemented
our $var;     # Parsed, but package linking NOT implemented
```

#### Exception Handling
```perl
eval { ... };   # Deferred to P5 (self-hosting)
eval "string";  # Deferred to P5 (self-hosting)
$@ capture      # Deferred to P5 (after eval)
die/warn        # Basic, no stack trace
```

#### Advanced OO
```perl
@ISA             # Inheritance NOT implemented
AUTOLOAD         # NOT implemented
DESTROY          # NOT implemented
SUPER::method()  # NOT implemented
```

#### Process Control
```perl
kill $sig, $pid  # NOT implemented - use sb-posix:kill
fork()           # NOT implemented - use sb-posix:fork
wait()           # NOT implemented - use sb-posix:wait
```

#### Misc
```perl
goto LABEL       # NOT implemented
BEGIN/END blocks # NOT implemented - use eval-when / sb-ext:*exit-hooks*
do FILENAME      # NOT implemented
wantarray        # Partial (in subs only)
defined()        # Works, but not for all edge cases
```

---

## Won't Implement

These Perl features are deprecated, experimental, or too complex for the initial implementation:

- `given`/`when` - deprecated, removed in Perl 5.42
- Smart match `~~` - deprecated
- `tie`/`untie`/`tied` - complex, rarely needed
- Formats (`format`, `write`) - legacy feature
- Lvalue subs (`:lvalue`) - rare
- Indirect object syntax (`new Class`) - deprecated
- Devel::* introspection
- XS/C extensions (future: XS bridge)

---

## Priority Roadmap

### P1 - Core Completion
1. ~~Loop labels (OUTER:, next OUTER)~~ ✓
2. `$_` default variable

### P2 - Practical Scripts
1. ~~Easy built-ins: `chdir`, `mkdir`, `rmdir`, `getcwd`, `rename`, `chmod`~~ ✓
2. ~~Time: `time`, `sleep`, `localtime`, `gmtime`~~ ✓
3. ~~Process: `exit`, `system()`, backticks~~ ✓
4. ~~`%ENV` access~~ ✓
5. `use`/`require` for pure-Perl modules

### P3 - Advanced
1. Inheritance (`@ISA`, `SUPER::`)
2. `AUTOLOAD`, `DESTROY`
3. `local` dynamic scoping

### P4 - Future
1. XS bridge for CPAN modules
2. Debugger integration

### P5 - Self-Hosting (after transpiler runs in Lisp)
1. `eval { }` blocks - use transpiler at runtime
2. `eval "string"` - dynamic code execution
3. `s/pat/code/e` - eval replacement modifier

---

## Parser Gaps

The expression parser (PExpr.pm) handles most Perl syntax. Known gaps:

1. **Regex + ternary**: `$s =~ /pat/ ? $a : $b` may fail (use `if` instead)
2. **Glob `<*.txt>`**: File glob NOT implemented (only `<FH>` readline)

### Prototype Limitations

**What works:**
```perl
# New-style signatures (recommended)
sub foo($x, $y = 10, @rest) { }    # ✓ Full support with defaults

# Old-style prototypes - parsed but limited
sub bar($$) { }                     # ✓ Parsed, param count tracked
sub baz($;$) { }                    # ✓ Optional params after ;
sub qux(\@$) { }                    # ✓ Reference sigils parsed
```

**What does NOT work:**
```perl
# Reference context forcing
sub myfunc(\@) { }
myfunc(@array);        # Does NOT auto-pass \@array, must write myfunc(\@array)

# Block prototypes
sub mygrep(&@) { }
mygrep { $_ > 5 } @list;   # Does NOT parse bare block - use mygrep(sub { }, @list)

# Typeglob/filehandle prototypes
sub myprint(*@) { }
myprint STDERR "msg";      # Does NOT parse bareword filehandle specially

# Default $_ prototype
sub mytrim(_) { }
mytrim;                    # Does NOT use $_ when called without args

# Prototype enforcement at call sites
sub takes_two($$) { }
takes_two(1, 2, 3);        # Compiles - extra args NOT rejected
takes_two(1);              # Compiles - missing args NOT detected
```

---

## Runtime Gaps

The CL runtime (pcl-runtime.lisp) is missing:

1. **`e` modifier**: `s/pat/code/e` - deferred to P5 (self-hosting)
2. **Localization**: UTF-8 case folding, locale-aware sorting
3. **Tied variables**: Not planned

---

## Contributing

To add a new built-in function:
1. Add to `Pl/PExpr/Config.pm` `known_no_of_params`
2. Implement `pl-funcname` in `cl/pcl-runtime.lisp`
3. Add tests to `Pl/t/`

To add a new statement type:
1. Add case in `Pl/Parser.pm` `_process_element()`
2. Create `_process_X_statement()` method
3. Add code generation in `Pl/ExprToCL.pm` if needed

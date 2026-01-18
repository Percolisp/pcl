# PCL - Remaining Work and Limitations

**Current Status:** 43 test files, 2085 tests passing

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
- Old-style prototypes including `&` (block argument): `sub foo (&;@) { }`
- Compile-time prototype extraction from `use`d modules
- Built-ins: `print`, `say`, `length`, `substr`, `index`, `push`, `pop`, `shift`, `unshift`, `keys`, `values`, `exists`, `delete`, `grep`, `map`, `sort`, `join`, `split`, `reverse`, `sprintf`, `chr`, `ord`, `lc`, `uc`, `fc`, `lcfirst`, `ucfirst`, `chop`, `chomp`, `quotemeta`, `abs`, `int`, `sin`, `cos`, `sqrt`, `exp`, `log`, `rand`, `pos`, `caller`

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
- `$_` - default variable for built-ins, `foreach`, standalone regex, `s///`, `tr///`
- `$$` - process ID
- `$!` - system error message (errno) - works in string interpolation
- `$0` - program name
- `$^O` - OS name (linux, darwin, MSWin32)
- `$^V` - Perl version
- `__FILE__` - compile-time filename
- `__LINE__` - compile-time line number
- `__PACKAGE__` - compile-time package name
- `@ARGV` - command line arguments
- `@_` - subroutine arguments; `shift`/`pop` default to `@_` in subs, `@ARGV` at top level
- Other magic vars (`$?`, `$.`, `$/`, etc.) - declared, parse/interpolate, but not fully functional

### OO
- `package` declarations
- `bless`, `ref`
- Method calls: `$obj->method()`, `Class->new()`
- Inheritance via `our @ISA = qw(Parent1 Parent2)`
- `SUPER::method()` calls to parent class
- C3 Method Resolution Order (via CLOS)
- Multiple inheritance / diamond inheritance

### Module System
- `use Module;` and `require Module;` for pure-Perl modules
- `use lib "path";` modifies `@INC`
- `use Module qw(imports);` with import tracking
- Module caching in `~/.pcl-cache/`
- Nested packages with `::` (e.g., `Foo::Bar::Baz`)
- Forward declarations: subs can be called before definition (top-level code works)

### Package Variables
- `our $var`, `our @arr`, `our %hash` - package-level variables
- `local $var` - dynamic scoping (value restored on scope exit)
- Package variables persist across function calls
- Cross-package references: `$Pkg::var`, `Pkg::func()`

### Other
- `use constant`
- `state` variables (inside subs)
- String interpolation (including `$arr[0]`, `$hash{key}` in strings)
- Heredocs (`<<EOF`, `<<'EOF'`, `<<"EOF"`)
- Comments preserved in output

---

## Known Limitations

### NOT Implemented

#### Special Variables
```perl
$_      # ✓ Implemented - default for built-ins, foreach, regex, s///, tr///
$!      # ✓ Implemented - system error (errno string), works in interpolation
$$      # ✓ Implemented - process ID
$0      # ✓ Implemented - program name
$^O     # ✓ Implemented - OS name
$^V     # ✓ Implemented - Perl version (returns "v5.30.0")
$@      # ✓ Implemented - set by eval { } on error, cleared on success
$?      # Declared - not set by system()/backticks yet
$.      # Declared - not set by readline yet (needs per-FH tracking)
$/      # Declared - readline doesn't use it yet
$\      # Declared - print doesn't append it yet
$"      # ✓ Implemented - array interpolation uses it for join
@_      # works in subs; shift/pop default to @_ in subs, @ARGV at top level
@ARGV   # ✓ Implemented - command line arguments
```

#### Module System
```perl
use Module;         # ✓ Implemented (transpiles and loads .pm files)
require Module;     # ✓ Implemented (dynamic loading with %INC tracking)
use Module qw(...); # ✓ Implemented (imports tracked, not enforced)
use lib "path";     # ✓ Implemented (modifies @INC)
```
Module caching supported via `~/.pcl-cache/`. Prototype extraction is memoized and skips known core modules (Test2::*, Carp, Scalar::Util, etc.) for fast transpilation.

#### Variable Scoping
```perl
our $var;     # ✓ Implemented - generates defvar in package scope
our @arr;     # ✓ Implemented - arrays and hashes supported
our %hash;    # ✓ Implemented
local $var;   # ✓ Implemented - true dynamic scoping via CL special variables
local @arr;   # ✓ Implemented
```

#### Exception Handling
```perl
eval { ... };   # ✓ Implemented - catches errors, sets $@
eval "string";  # Deferred to P5 (self-hosting)
$@ capture      # ✓ Implemented - set on error, cleared on success
die/warn        # ✓ Implemented - die supports exception objects
```

#### Advanced OO
```perl
@ISA             # ✓ Implemented - C3 MRO via CLOS
SUPER::method()  # ✓ Implemented - walks parent MRO
can($method)     # ✓ Implemented - returns code ref or undef
isa($class)      # ✓ Implemented - checks inheritance via MRO
AUTOLOAD         # NOT implemented
DESTROY          # NOT implemented
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
wantarray        # Works, but no void context (returns nil for both scalar and void)
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

## CPAN Compatibility

Running unmodified CPAN modules has several challenges:

### Deal Breakers

**1. XS/C Extensions** - The biggest obstacle. Many popular modules have XS components:
```
DBI, DBD::*           # Database access
JSON::XS              # Fast JSON (use JSON::PP instead)
Moose                 # OO framework internals
DateTime              # Date/time internals
List::Util            # Some functions are XS
```
Pure-Perl alternatives exist for some (JSON::PP, Moo instead of Moose), but dependencies often pull in XS transitively.

**2. `eval "string"`** - Runtime code generation used by metaprogramming modules:
```perl
eval "sub $name { ... }";           # Dynamic sub creation
eval "package $pkg; ..."            # Dynamic package manipulation
```
Fixable with self-hosting (transpiler available at runtime), but significant work.

**3. Symbol Table Manipulation** - Used heavily by OO frameworks:
```perl
*foo = \&bar;                       # Sub aliasing
*{$pkg.'::'.$name} = $coderef;      # Runtime sub installation
no strict 'refs'; *{"$pkg::$_"} = ... # Metaprogramming
```
Moo, Moose, Class::Accessor, and many others depend on this.

**Note:** See `MOO_MOOSE_DESIGN.md` for a plan to support Moo/Moose by recognizing their usage patterns and emitting CLOS equivalents instead of transpiling the modules directly.

**4. `BEGIN` Blocks** - Compile-time execution:
```perl
BEGIN { $VERSION = '1.0' }          # Compile-time setup
BEGIN { extends 'Parent' }          # OO configuration
```
Many modules configure themselves at compile time.

### What Should Work

Target **pure-Perl, minimal-magic modules**:
- Text processing utilities
- Simple parsers (recursive descent, etc.)
- Config file handlers (INI, simple formats)
- Protocol implementations (pure-Perl HTTP, SMTP, FTP)
- Data structure utilities
- Template systems (simple ones)
- Test modules (Test::More basics work)

### Implementation Notes

**BEGIN Blocks**

Simple case - straightforward:
```perl
BEGIN { $VERSION = '1.0' }
```
Emit as top-level CL form. Top-level forms execute in order, so this "just works".

Hard case - affects parsing:
```perl
BEGIN {
    require Some::Module;
    Some::Module->import('func');  # 'func' now available for parsing
}
```
This requires executing code during transpilation. Options:
1. Shell out to Perl to run the BEGIN block
2. Self-hosting: transpiler runs in Lisp, can execute transpiled BEGIN blocks
3. Two-pass: first pass collects BEGIN blocks, executes them, second pass uses results

**Symbol Table Manipulation**

Static aliasing - easy:
```perl
*foo = \&bar;
```
```lisp
(setf (symbol-function 'pl-foo) (symbol-function 'pl-bar))
```

Dynamic installation - medium:
```perl
*{$pkg.'::'.$name} = $coderef;
```
```lisp
(setf (symbol-function (intern (format nil "pl-~A" name) pkg)) coderef)
```

Full typeglob emulation - hard:
Perl's typeglob holds scalar, array, hash, sub, and filehandle under one name. Would need either:
1. A `typeglob` struct with slots for each type
2. Naming convention: `$foo` → `$foo`, `@foo` → `@foo`, `&foo` → `pl-foo` (current approach)
3. Handle each assignment type separately based on reftype of RHS

Current approach (2) works if we detect what's being assigned and route to the right slot.

### Path Forward

1. **Short term**: Focus on pure-Perl modules without metaprogramming
2. **Medium term**: Implement `BEGIN` blocks (simple cases) and basic symbol table ops
3. **Long term**: XS bridge to call Perl's C API from Lisp

The XS bridge would allow using XS modules by embedding a Perl interpreter, but that's a significant project and partially defeats the purpose of transpiling.

---

## Priority Roadmap

### P1 - Core Completion
1. ~~Loop labels (OUTER:, next OUTER)~~ ✓
2. ~~`$_` default variable~~ ✓

### P2 - Practical Scripts
1. ~~Easy built-ins: `chdir`, `mkdir`, `rmdir`, `getcwd`, `rename`, `chmod`~~ ✓
2. ~~Time: `time`, `sleep`, `localtime`, `gmtime`~~ ✓
3. ~~Process: `exit`, `system()`, backticks~~ ✓
4. ~~`%ENV` access~~ ✓
5. ~~`use`/`require` for pure-Perl modules~~ ✓
6. ~~`our` package variables~~ ✓

### P3 - Advanced
1. ~~`local` dynamic scoping~~ ✓
2. ~~Inheritance (`@ISA`, `SUPER::`)~~ ✓
3. `AUTOLOAD`, `DESTROY`

### P4 - Future
1. XS bridge for CPAN modules
2. Debugger integration

### P5 - Self-Hosting (after transpiler runs in Lisp)
1. ~~`eval { }` blocks~~ ✓ Done (native handler-case)
2. `eval "string"` - dynamic code execution
3. `s/pat/code/e` - eval replacement modifier

---

## Parser Gaps

The expression parser (PExpr.pm) handles most Perl syntax. Known gaps:

1. **Glob `<*.txt>`**: File glob NOT implemented (only `<FH>` readline)
2. **Indirect filehandle**: `print {$fh} "text"` not fully supported (use `print $fh "text"`)
3. **Symbolic sub refs**: `&{"$pkg::$name"}()` not implemented

### Prototype Limitations

**What works:**
```perl
# New-style signatures (recommended)
sub foo($x, $y = 10, @rest) { }    # ✓ Full support with defaults

# Old-style prototypes - parsed and functional
sub bar($$) { }                     # ✓ Parsed, param count tracked
sub baz($;$) { }                    # ✓ Optional params after ;
sub qux(\@$) { }                    # ✓ Reference sigils parsed

# Block prototypes (&) - ✓ WORKS
sub wrapper (&) { }
wrapper { code };                   # ✓ Block converted to sub ref

# Reference auto-boxing (\@, \%, \$) - ✓ WORKS
sub modify(\@) { }
modify(@array);                     # ✓ Auto-boxes to \@array

# Prototype import from modules at compile time
use MyModule;                       # ✓ Prototypes (& and \) extracted and imported
my_func { block } @args;            # ✓ Works if my_func has & prototype
my_func(@array);                    # ✓ Works if my_func has \@ prototype
```

**What does NOT work:**
```perl
# Typeglob/filehandle prototypes
sub myprint(*@) { }
myprint STDERR "msg";      # Does NOT parse bareword filehandle specially

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

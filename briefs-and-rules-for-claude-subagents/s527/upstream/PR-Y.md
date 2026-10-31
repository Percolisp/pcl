# PR Y -- Lexer: a block filehandle right after print( is a block

**The perl rule.** `print`, `printf`, `say`, `exec` and `system` read a `{` right after their `(` the way they read it without the parentheses: toke.c is in its filehandle expectation (XREF) there, and the `{` is a BLOCK -- the filehandle, or the program of `exec` / `system` -- unless its first term decides an anonymous hash (a word or a string followed by `,` or `=>`, or an immediate `}`; the same look-ahead as everywhere else).

    perl -e 'my $fh = \*STDOUT; print({$fh} "a", "b"); printf({$fh} "%s\n", "c")'   # abc
    perl -MO=Deparse -e 'print({ STDOUT } "Hello")'                                 # print {'STDOUT';} 'Hello';
    perl -e 'print({ a => 1 })'                                                     # HASH(0x...)  (control)

**PPI before / after:**

```
BEFORE (master 455bfa73)
print({$fh} "a", "b")     List( Statement::Compound( Block{$fh} )  Statement::Expression( "a", "b" ) )
printf({$fh} "%s", "c")   List( Statement( Constructor{$fh} "%s" , "c" ) )
system({$p} "a", "b")     List( Statement( Constructor{$p} "a" , "b" ) )

AFTER (branch pcl)
print({$fh} "a", "b")     List( Statement( Block{$fh} "a" , "b" ) )
printf({$fh} "%s", "c")   List( Statement( Block{$fh} "%s" , "c" ) )
system({$p} "a", "b")     List( Statement( Block{$p} "a" , "b" ) )
print({ a => 1 })         List( Statement( Constructor{ a => 1 } ) )        (unchanged)
```

**PPI's own fixture changes, and why.** `t/data/05_lexer/13_braces_in_parens.dump` stored its second line, `print({ STDOUT } 'Hello');`, as the bug: a `Statement::Compound` holding the block, then a `Statement::Expression` holding `'Hello'`.  perl prints `Hello` -- the block is the filehandle of one print statement (Deparse above).  The fix commit updates exactly that statement of the dump (Compound -> plain Statement, the quote inside it); the file's other two lines, `print({ a => 1 });` and `$hashref = ({ %base_args, arg => $mod_value });`, keep their anonymous-hash Constructors and their dump is unchanged.

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §37 (six rows: print, say, CORE::say, print with a map in the list, system, exec) plus the control `print({a => 1})`.

**Commits** (tests first, marked $TODO, then the fix; PPI's own suite passes after each):

- `d15cb43f` TODO tests: a block filehandle right after print( is a block
- `56330a40` Lexer: a block filehandle right after print( is a block

(The two are not adjacent on the `pcl` branch: the fix was held for a ruling on the fixture change and landed at the end of the series.  For the upstream PR, cherry-pick the pair onto master in this order.)

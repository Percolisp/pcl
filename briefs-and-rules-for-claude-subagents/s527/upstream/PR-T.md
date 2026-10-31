# PR T -- Lexer: finally belongs to the try statement

**The perl rule.** perly.y reads `try BLOCK catch (VAR) BLOCK` with an optional `finally BLOCK` (5.36) as ONE statement that needs no `;` (Syntax::Keyword::Try also allows `try BLOCK finally BLOCK`).

    perl -E 'use feature "try"; no warnings; try { die "e\n" } catch ($e) { print "c $e" } finally { print "fin\n" } print "after\n"'
    # c e / fin / after

**PPI before / after:**

```
BEFORE (master 455bfa73)
Statement::Compound: try { die "e\n" } catch ( $e ) { print "c $e" }
Statement:           finally { print "fin\n" } print "after\n" ;      <- swallows the next statement

AFTER (branch pcl)
Statement::Compound: try { die "e\n" } catch ( $e ) { print "c $e" } finally { print "fin\n" }
Statement:           print "after\n" ;
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §18 (three rows).  Regression pair `t/data/08_regression/60_try_finally`.

**Commits** (tests first, marked $TODO, then the fix; PPI's own suite passes after each):

- `44b918d4` TODO tests: finally belongs to the try statement
- `4e021b91` Lexer: finally belongs to the try statement

# Lexer: the { after a glob, and after a glob's dereference block, subscripts it  (PCL §49 §54 §57; group AI)

**Perl's rule.** A glob `*NAME` is a complete term; after a term perl expects an operator (toke.c XOPERATOR) and a `{` there subscripts the term -- the glob-slot syntax `*NAME{THING}`:

    perl -e 'our %x = (a => 1); my $h = *x{HASH}; print $h->{a}, "\n"'    # 1

After a cast, as in `require *{"name"}`, the `{` opens the cast's dereference block.

**Before.** `_curly` built `*x{HASH}`'s `{HASH}` as a `Structure::Block` (and its contents as a statement); `require *{"..."}` fired the Include `use constant {` rule and became a `Structure::Constructor`.
**After.** `Symbol[*x] Structure::Subscript[{HASH}]`; `Cast[*] Structure::Block[{"..."}]`.

**Rows fixed** in t/ppi_bugs_pcl.t: PCL §49, §54 (§57 is §49's twin).  Two commits: the TODO tests (shared "TODO tests: the Lexer's structure cases from PCL" commit), then the fix.  PPI's suite PASS plain + AUTHOR_TESTING after each.

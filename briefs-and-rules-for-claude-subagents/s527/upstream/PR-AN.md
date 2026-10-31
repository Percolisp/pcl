# Lexer: the class feature builds its statements  (PCL §53; group AN)

**Perl's rule.** Under the class feature (perl 5.38) `class NAME [VERSION] [ATTRS] BLOCK` and `class NAME;` declare a package, `method NAME BLOCK` a sub, `field VAR [ATTRS] [= EXPR];` a variable and `ADJUST BLOCK` a phaser block (perly.y `KW_CLASS`, `KW_METHOD_named`, `KW_FIELD`, `PHASER`).

    perl -E 'use v5.38; use experimental "class"; class P { field $v = 1; method get { $v } } say P->new->get'   # 1

**Before.** PPI tracked the feature (group AG) but the Lexer built one plain statement from `class P { ... }` and the statement after it; `field` / `method` / `ADJUST` were plain statements.
**After.** Under the feature `class` / `field` / `ADJUST` pick `PPI::Statement::Package` / `::Variable` / `::Scheduled`, and `method NAME` takes the sub detection (`PPI::Statement::Sub`; an anonymous `method { }` stays an expression).  The braces after the name are a Block through the existing Package / Sub rules.  Without the feature the four words stay ordinary words (control row).

**Credit.** The statement classes are those of upstream PR #346 (by toddr-bot, fixing issue #299: "Support for Corinna keywords"); this group takes its Lexer half.  #346 also changes the tokenizer (attributes after `method` / `field`, labels), which the rows here do not need; it can land on top.

**Rows fixed:** PCL §53 (two rows; one control).  TODO tests commit `634d8e6d`, then the fix `9286f7c0` (26 lines).  Suite PASS plain + AUTHOR_TESTING.

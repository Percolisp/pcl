# PR U -- Lexer: the loop variable of foreach is any of perl's forms

**The perl rule.** perly.y's foreach reads, between `for` and `( LIST )`: an optional `my`/`our`/`state`, an optional `\` (refaliasing), then a scalar, a dereference (`$$f`, `${$r}`, `${*$f}`) or, after `my`, a list of scalars (5.36).

    perl -e 'no strict "refs"; our $z = 7; my $f = "z"; for $$f (5, 11) { print "$z " } print "$z\n"'   # 5 11 7
    perl -E 'for my ($q, $r) (1..4) { print "$q$r " }'                                                # 12 34

**PPI before / after:**

```
BEFORE (master 455bfa73)
for $$f (5, 11) { 1 } f();          Lexer failed: Illegal state in 'for' compound statement   (no document at all)
for my ($q, $r) (@l) { 1 } f();     Compound: for my | Statement: (...) (...) {...} f ( ) ;
for \my %h (@l) { 1 } f();          Compound: for    | Statement: \ my %h (...) {...} f ( ) ;

AFTER (branch pcl)
for $$f (5, 11) { 1 } f();          Compound(foreach): for $ $f (...) {...}     | Statement: f ( ) ;
for my ($q, $r) (@l) { 1 } f();     Compound(foreach): for my (...) (...) {...} | Statement: f ( ) ;
for \my %h (@l) { 1 } f();          Compound(foreach): for \ my %h (...) {...}  | Statement: f ( ) ;
```

One helper, `_foreach_head`, walks the head; `PPI::Statement::Compound::type` says `foreach` when a cast follows `for`.

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §6, §10, §29 (the `for $$$r` row stays TODO: the tokenizer reads `$$$r` as `$$` + `$r`, PCL §1).  Regression pair `61_foreach_variable`.

**Commits:**

- `949debd9` TODO tests: every loop variable foreach accepts
- `a320f21a` Lexer: the loop variable of foreach is any of perl's forms

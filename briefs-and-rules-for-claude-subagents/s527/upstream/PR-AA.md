# PR AA -- §44

**The perl rule.** CORE::__END__ and CORE::__DATA__ end the code like __END__ and __DATA__ (toke.c yyl_data_handle).

    printf 'print "x\n";\nCORE::__END__\nnot code\n' | perl   # x

**PPI before / after:**

```
BEFORE (master 455bfa73)
print 1;\nCORE::__END__\nnot code\n
    Word(print) Number(1) Structure(;) Word(CORE::__END__) Operator(not) Word(code)
print 1;\nCORE::__DATA__\nfoo\n
    Word(print) Number(1) Structure(;) Word(CORE::__DATA__) Word(foo)

AFTER (branch pcl)
print 1;\nCORE::__END__\nnot code\n
    Word(print) Number(1) Structure(;) Separator(CORE::__END__)
print 1;\nCORE::__DATA__\nfoo\n
    Word(print) Number(1) Structure(;) Separator(CORE::__DATA__) Data(foo
)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §44.

**Second fix commit (s527f, the Lexer half).**  The tokenizer change makes the Separator; the Lexer still picked the statement class by the bare spelling, so `CORE::__END__` sat in a plain `PPI::Statement`.  `%STATEMENT_CLASSES` now maps `CORE::__END__` / `CORE::__DATA__` to `PPI::Statement::End` / `::Data` (TODO tests `ac62cb85`, fix `49a24b96`; two more §44 rows).  Suite PASS plain + AUTHOR_TESTING.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1): see `git log` of the branch; the group's commits are listed in `vendor/PPI/UPSTREAM` of PCL under letter AA.

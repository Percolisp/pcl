# PR M -- HereDoc: the indentation is what stands before the terminator

**The perl rule.** perl strips, from every line of an indented here-doc, the whitespace before the terminator on its line -- not ^(\s*) of the line, which also counts a terminator that begins with whitespace (<<~' EOF') or is empty.

    printf 'print <<~%s;\n  some data\n   EOF\n' "' EOF'" | perl   # some data

**PPI before / after:**

```
BEFORE (master 455bfa73)
print <<~' EOF';\n  some data\n   EOF\n
    Word(print) HereDoc(<<~' EOF') Structure(;)

AFTER (branch pcl)
print <<~' EOF';\n  some data\n   EOF\n
    Word(print) HereDoc(<<~' EOF') Structure(;)
```

**Rows fixed in t/ppi_bugs_pcl.t:** PCL §30.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked $TODO, then the fix that removes the markers -- PPI's own suite passes after each, plain and AUTHOR_TESTING=1):

- `e5b46488` TODO tests: an indented here-doc's indentation stops at its terminator
- `3ed5f883` HereDoc: the indentation is what stands before the terminator

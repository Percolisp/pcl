---
name: feedback_split_lisp_on_defun
description: "When debugging paren problems in .lisp files, split on top-level (defun into /tmp/ chunks — never count parens across the whole file"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: aafba459-cc38-48d6-8388-b89615aaa56f
---

When a `.lisp` file has a paren or formatting problem, split it by top-level `(defun` lines (lines starting at column 0) into individual `/tmp/defun-FUNCNAME.lisp` files. Format and check each chunk independently.

**Why:** pcl-pack.lisp has 700+ lines with functions up to 370 lines deep. Counting parens across the whole file was consuming enormous token budgets over two sessions. Splitting into per-function files makes the problem scope manageable — each chunk is one function.

**How to apply:** Use `.claude/hooks/split-lisp.pl FILENAME.lisp`. Split on `^(def\w+` (column 0), NOT by counting paren depth. Do not use paren counting to find function boundaries — that defeats the purpose.

See also: [[feedback_cl_indentation_depth]]

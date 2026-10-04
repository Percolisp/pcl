---
name: feedback_fix_at_right_layer
description: "CPAN-module fixes go in lib/*.pm shims, NOT the parser (Pl/) or runtime (cl/) — fix at the right layer"
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 23a45cf5-6f24-4989-a4b9-9b1d7fcf198c
---

When a CPAN module misbehaves, do NOT patch the parser (`Pl/*.pm`,
e.g. `Environment.pm`/`Parser.pm`) or runtime (`cl/pcl-runtime.lisp`) with
module-specific knowledge. That is the closest layer and therefore the wrong
one by default. The user flagged this as a **repeated problem** (session 244).

**Three layers, each owns a different kind of fact:**
- `lib/<Module>.pm` shim → one module's behavior (its subs, prototypes,
  exports, constants). PCL transpiles it like user code.
- `Pl/*.pm` parser/codegen → *generic* language mechanisms, keyed on the
  **mechanism, never a name** (e.g. "a `(&@)` prototype → block-form parse").
- `cl/pcl-runtime.lisp` → genuine Perl *core* semantics (builtins, box model).

**Decision rule:** could a user write this in plain Perl? If yes → `.pm` shim.

**Smell test (hard stop):** if the diff adds a literal CPAN module name or a
non-core function name to a file under `Pl/` or `cl/`, it's the wrong layer.
Exception: core builtins (grep/map/sort/print) ARE language.

**Why:** otherwise the language core slowly fills with a registry of per-module
trivia; each new module repeats the mistake.

**How to apply:** worked example — List::Util `first {…}` block form failed.
WRONG = `first => {has_block_arg=>1}` in `Environment.pm`. RIGHT = `sub first (&@)`
in `lib/List/Util.pm` + stop `_extract_module_prototypes` skipping List::Util so
the generic block-form parser reads the prototype. Codified in CLAUDE.md
Design Principle 9a. See [[project_cpan_module_survey]], `docs/shipped-modules.md`.

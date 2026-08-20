This is the critical realization that makes or breaks a project like this.

The short answer is **yes**. If you want full CPAN compatibility where `BEGIN` blocks can call functions defined earlier in the same file, you **must** wrap your function definitions in `eval-when`.

Here is the deep dive into why, and the specific architecture you will likely need for a Perl-to-Lisp compiler.

### The "Compile-Time Environment" Problem

In Common Lisp, compiling a file does not automatically load the functions defined in that file into the compiler's memory.

1. **Standard Lisp Behavior:**
```lisp
;; The compiler compiles this function to machine code/bytecode
;; but does NOT define it in the running Lisp image yet.
(defun my-helper () (+ 1 1))

;; This block executes NOW (during compilation).
(eval-when (:compile-toplevel :execute)
  ;; ERROR! The function 'my-helper' is not defined yet!
  (print (my-helper)))

```


2. **Perl Behavior:**
```perl
sub my_helper { return 1 + 1; }

# Perl defines subs immediately as it parses them.
BEGIN {
    # This works perfectly.
    print my_helper();
}

```



To make the Lisp code behave like Perl, you must force the `defun` to happen in the compile-time environment so subsequent `BEGIN` blocks (mapped to `eval-when`) can see it.

### The Strategy: "The CPAN Protocol"

Since you are targeting CPAN, you are dealing with legacy code that abuses compile-time side effects. To handle this, your compiler should likely default to wrapping **almost everything** in `eval-when`.

#### 1. Function Definitions

You need to generate code that looks like this:

```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun my-perl-sub (args)
    ... implementation ...))

```

**Why all three flags?**

* `:compile-toplevel`: Defines the function *inside the compiler* so subsequent `BEGIN` blocks in the same file can call it.
* `:load-toplevel`: Defines the function when the compiled `.fasl` file is loaded (standard runtime behavior).
* `:execute`: Defines the function if you are just interpreting the code (REPL or script mode).

#### 2. Variable Definitions (`my` vs `our`)

This is where it gets tricky.

* **Globals (`our`, `$Package::Var`):**
These must be `defparameter` or `defvar` wrapped in `eval-when` so `BEGIN` blocks can read/write them.
```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *package-var* nil))

```


* **Lexicals (`my`):**
Lexical variables in Perl are tricky because they don't persist across the "compile-time / run-time" boundary easily if they are at the top level.
If you have:
```perl
my $config = 1;
BEGIN { $config = 2; }

```


You generally cannot wrap `let` in `eval-when`. For top-level `my` variables, you might have to promote them to **global variables** (renamed to avoid collision, e.g., `*lexical-config-G123*`) to allow `BEGIN` blocks to modify them during compilation.

### A Warning on Recursive Dependency

If you wrap everything in `eval-when`, you might hit a snag with macros.

If `sub A` calls `sub B`, and you wrap both in `eval-when`, everything is fine. But if you try to "compile" Perl semantics into Lisp **Macros**, be careful.

If you translate Perl subs into Lisp macros to try and get "compile time" behavior, you will enter "Macrology Hell." Stick to **Functions wrapped in `eval-when**`. It is verbose, but it is the only way to emulate Perl's "executable parser" state.

### Next Step for your Compiler

Since you are targeting CPAN, you will encounter `require` and `use` inside conditional blocks.

Would you like me to explain how to handle **conditional compilation** (e.g., `if ($^O eq 'linux') { use Linux::Stuff; }`), which is common in CPAN but very difficult to map to Lisp's static compilation model?

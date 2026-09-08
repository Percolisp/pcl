;;;; Copyright (c) 2025-2026 the PCL authors
;;;; This is free software; you can redistribute it and/or modify it under the
;;;; same terms as the Perl 5 programming language system itself.
;;;; SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

;;;; PCL: build a standalone executable from an emitted program (task #1060).
;;;;
;;;; This file is loaded ONLY by the build SBCL that `pl2cl --executable`
;;;; spawns.  It is not part of the runtime and is not in the saved core --
;;;; but the toplevel function it defines IS saved into the produced binary,
;;;; because the image is dumped from this same process.
;;;;
;;;; THE PROBLEM IT SOLVES.  A transpiled program is a sequence of top-level
;;;; CL forms, and `load`ing it RUNS the program.  The old --executable
;;;; `load`ed the file into the build image and then saved an image whose
;;;; :toplevel merely exited: the program ran at BUILD time and the binary did
;;;; nothing (rc 0, no output).  With an `exit` anywhere in the program the
;;;; build did not even produce a binary -- the program's exit killed the
;;;; builder.
;;;;
;;;; THE SEAM.  The emission already separates the two phases: Pl/Parser2.pm
;;;; assembles every section's COMPILE phase first, then emits exactly one
;;;; `(p-run-compile-phase-blocks)`, then every section's RUN phase (the
;;;; two-pass loop at Parser2.pm:2044-2135; the same boundary
;;;; Pl/t/ir-data-form-01.t splits on).  So:
;;;;
;;;;   forms up to AND INCLUDING (p-run-compile-phase-blocks)
;;;;       -> EVALuated at build time.  That IS perl's compile phase: sub
;;;;          definitions, package/global cells, `use` (so the use-closure is
;;;;          preloaded into the image), BEGIN, UNITCHECK/CHECK/INIT.
;;;;   every later form
;;;;       -> COMPILED into a thunk and kept in *p-exe-main-thunks*, to be
;;;;          called by %p-exe-toplevel when the BINARY starts.
;;;;
;;;; WHAT IS STILL NOT STANDALONE (docs/single-binary-plan.md steps 3-5, and
;;;; task #217): @INC and *pcl-pl2cl-path* are baked at build time, a run-time
;;;; `require` of a module not already loaded still shells out to perl+pl2cl,
;;;; and the pack/mro/warnings extensions still load lazily from the build
;;;; tree.  Those failures are LOUD, not silent, and are documented in
;;;; `pcl --help` and the plan.

(in-package :pcl)

(defvar *p-exe-main-thunks* nil
  "The program's RUN-phase top-level forms, compiled, in source order.
   Filled at build time by %p-exe-load-program; called by %p-exe-toplevel
   when the produced binary starts.")

(defvar *p-exe-source* nil
  "The perl source file the binary was built from -- for diagnostics only.")

(defun %p-exe-note (fmt &rest args)
  "Build-time progress, on stderr, so it never mixes with a program's stdout."
  (apply #'format *error-output* fmt args)
  (finish-output *error-output*))

(defun %p-exe-load-program (path)
  "Read PATH form by form, EVALuating the compile phase as it goes and
   collecting the run phase.  Returns the run-phase forms as (form . package)
   conses in source order.

   Read and eval INTERLEAVE, exactly as `load` does them, because they depend
   on each other: `(p-defpackage :main)` must have run before `(in-package
   :main)` can be READ, and `in-package` is read-time, so the reader's package
   must move with it.  Evaluating the `in-package` form does both -- it is a
   setq of *package*, and the binding it hits is this LET's.

   DIES when the file carries no (p-run-compile-phase-blocks) boundary: a
   missing case must never fall through to 'it was all compile phase', which
   is exactly the silent build-time run this file exists to remove."
  (let ((run-forms '()) (boundary nil)
        (*package* (find-package :pcl)))
    (with-open-file (in path :direction :input :external-format :utf-8)
      (loop
       (let ((form (read in nil '%p-exe-eof)))
         (when (eq form '%p-exe-eof) (return))
         (cond
           ((not boundary)
            (eval form)
            (when (and (consp form)
                       (eq (first form) 'p-run-compile-phase-blocks))
              (setf boundary t)))
           ;; Past the boundary an in-package is READER state only: evaluate
           ;; it (that is all it does) and never make it a thunk, because a
           ;; nested one cannot re-home the symbols around it.
           ((and (consp form) (eq (first form) 'cl:in-package))
            (eval form))
           (t (push (cons form *package*) run-forms))))))
    (unless boundary
      (error "PCL: --executable: ~A has no (p-run-compile-phase-blocks) form.~@
              That form is the compile/run phase boundary every program~@
              transpile emits exactly once; without it the program's run-time~@
              statements cannot be separated from its definitions."
             path))
    (nreverse run-forms)))

(defun %p-exe-compile-run-phase (forms)
  "Compile each RUN-phase form into a thunk, in order.  One thunk per
   top-level form -- the same granularity `load` uses, so a form that worked
   under `load` works here."
  (let ((thunks '()))
    (with-compilation-unit (:override nil)
      (dolist (entry forms)
        (let ((*package* (cdr entry)))
          (push (compile nil `(lambda () ,(car entry))) thunks))))
    (nreverse thunks)))

(defun %p-exe-message (e)
  "An uncaught condition as perl would print it: the die message, with a
   trailing newline if it has none.  p-die's own messages already end in one
   (either the user's, or ' at FILE line N.' + newline); an internal CL error
   does not."
  (let ((s (princ-to-string e)))
    (if (and (plusp (length s)) (char= (char s (1- (length s))) #\Newline))
        s
        (concatenate 'string s (string #\Newline)))))

(defun %p-exe-die (e)
  "Perl's uncaught-die behaviour for a standalone binary: the message on
   stderr and nothing else, exit 255.  sb-ext:exit runs the exit hooks, so
   END blocks still run and every handle is still flushed -- which is perl.
   PCL_BACKTRACE=1 adds the CL backtrace, the only way to debug a binary."
  (format *error-output* "~A" (%p-exe-message e))
  (let ((bt (sb-posix:getenv "PCL_BACKTRACE")))
    (when (and bt (string/= bt "") (string/= bt "0"))
      (sb-debug:print-backtrace :stream *error-output* :count 40)))
  (finish-output *error-output*)
  (sb-ext:exit :code 255))

(defun %p-exe-toplevel ()
  "The saved image's entry point.  Takes @ARGV and $0 from the REAL process
   (the runtime's own defvars hold the BUILDER's argv), runs the program's
   run-phase thunks, and exits perl-shaped."
  (let* ((argv sb-ext:*posix-argv*)
         (args (cdr argv))
         (n (length args)))
    (box-set $0 (or (car argv) "perl"))
    (setf @ARGV (if args
                    (make-array n :adjustable t :fill-pointer n
                                :initial-contents args)
                    (make-array 0 :adjustable t :fill-pointer 0))))
  (handler-case
      (dolist (th *p-exe-main-thunks*) (funcall th))
    (sb-sys:interactive-interrupt () (sb-ext:exit :code 130))
    (error (e) (%p-exe-die e)))
  (sb-ext:exit :code 0))

(defun %p-exe-build (lisp-file output &key source)
  "Build OUTPUT, a standalone executable, from the emitted LISP-FILE.
   Never returns: save-lisp-and-die ends the process."
  (setf *p-exe-source* source)
  (handler-bind ((warning #'muffle-warning))
    (let ((run-forms (%p-exe-load-program lisp-file)))
      (%p-exe-note "  run phase: ~D top-level form~:P~%" (length run-forms))
      (setf *p-exe-main-thunks* (%p-exe-compile-run-phase run-forms))))
  (%p-exe-note "  saving image...~%")
  ;; :save-runtime-options t so the BINARY does not eat its own arguments as
  ;; SBCL runtime options (--help, --dynamic-space-size, ...): they all belong
  ;; to the perl program.  It also freezes the builder's stack/heap sizes,
  ;; which is what PCLSbcl chose for this build.
  (sb-ext:save-lisp-and-die output
                            :toplevel #'%p-exe-toplevel
                            :executable t
                            :save-runtime-options t))

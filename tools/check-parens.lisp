;;; tools/check-parens.lisp — exact structural balance check for .lisp files.
;;;
;;;   sbcl --script tools/check-parens.lisp FILE [FILE ...]
;;;
;;; Uses the real Lisp reader with *read-suppress* bound (nothing is
;;; evaluated or interned), so it inherits the reader's actual rules and is
;;; immune to everything that breaks a textual scanner: pipe-quoted symbols
;;; containing quotes/parens/semicolons (the runtime's |$"| for Perl's $"
;;; false-positived the old perl one-liner), #| block comments |#, #\( #\"
;;; character literals, #.(...), and escapes inside strings.
;;;
;;; Prints one line per file; exit status 0 iff every file is balanced.
;;; An extra ")" is reported at its own line; a missing ")" is reported as
;;; the line of the form that never closes.

(defun byte-line (file offset)
  "1-based line number of byte OFFSET in FILE (counting newline bytes, so it
is exact for ASCII and off only by convention, never by parse state, for
multi-byte UTF-8)."
  (with-open-file (s file :element-type '(unsigned-byte 8))
    (let ((line 1))
      (dotimes (i offset line)
        (let ((b (read-byte s nil nil)))
          (when (null b) (return line))
          (when (= b 10) (incf line)))))))

(defun one-line (c)
  "The condition's report text with whitespace runs collapsed to one space."
  (let ((text (substitute #\Space #\Newline (format nil "~A" c))))
    (with-output-to-string (out)
      (let ((prev-space nil))
        (loop for ch across text
              for spacep = (char= ch #\Space)
              do (unless (and spacep prev-space) (write-char ch out))
              (setf prev-space spacep))))))

(let ((fail 0))
  (dolist (file (rest sb-ext:*posix-argv*))
    (with-open-file (s file)
      (let ((*read-suppress* t)
            (start 0))
        (handler-case
            (progn
              (loop (setf start (file-position s))
               (when (eq (read s nil s) s) (return)))
              (format t "balanced: ~A~%" file))
          (end-of-file ()
            (incf fail)
            (format t "UNBALANCED (missing close paren): ~A — form starting at line ~D never closes~%"
                    file (byte-line file start)))
          (error (c)
            (incf fail)
            (format t "UNBALANCED: ~A near line ~D — ~A~%"
                    file (byte-line file (or (file-position s) 0)) (one-line c)))))))
  (sb-ext:exit :code (if (zerop fail) 0 1)))

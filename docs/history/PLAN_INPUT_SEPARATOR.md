# Plan: Implement $/ (Input Record Separator)

## Problem

The special variable `$/` is currently defined as a plain string in `pcl-runtime.lisp`:
```lisp
(defvar |$/| (string #\Newline) "Input record separator")
```

But generated code uses `(pl-setf |$/| "oo")` which calls `box-set`, expecting a box. This causes:
```
The value "\n" is not of type PCL:PL-BOX
```

Additionally, `pl-readline` ignores `$/` entirely - it always reads lines ending with `\n`.

## Solution

### 1. Box the Special Variables

In `cl/pcl-runtime.lisp`, change these plain variables to boxes:

```lisp
;; Before:
(defvar |$/| (string #\Newline) "Input record separator")
(defvar |$\\| "" "Output record separator")
(defvar |$"| " " "List separator for array interpolation")

;; After:
(defvar |$/| (make-pl-box (string #\Newline)) "Input record separator")
(defvar |$\\| (make-pl-box "") "Output record separator")
(defvar |$"| (make-pl-box " ") "List separator for array interpolation")
```

### 2. Create Helper to Read $/

Since `$/` is now boxed, code that reads it needs to unbox:

```lisp
(defun get-input-record-separator ()
  "Get the current value of $/ (unboxed)"
  (let ((val (if (pl-box-p |$/|) (pl-box-value |$/|) |$/|)))
    ;; Handle :undef -> nil for slurp mode
    (if (eq val :undef) nil val)))
```

### 3. Update pl-readline to Respect $/

Perl's `$/` has several modes:
- **Default `"\n"`**: Read until newline (line mode)
- **`undef`**: Read entire file (slurp mode)
- **`""`**: Paragraph mode (read until blank line)
- **Other string**: Read until that string appears
- **Reference to integer**: Read N bytes (rare, lower priority)

```lisp
(defun pl-readline (&optional fh)
  "Perl readline / diamond operator <FH> - read a line from filehandle.
   Respects $/ (input record separator)."
  (let ((stream (if fh (pl-get-stream fh) *standard-input*))
        (sep (get-input-record-separator)))
    (when stream
      (cond
        ;; Slurp mode: $/ = undef
        ((null sep)
         (let ((content (make-string-output-stream)))
           (loop for char = (read-char stream nil nil)
                 while char
                 do (write-char char content))
           (let ((result (get-output-stream-string content)))
             (if (string= result "") nil result))))

        ;; Paragraph mode: $/ = ""
        ((string= sep "")
         (let ((lines nil)
               (seen-content nil))
           (loop
             (multiple-value-bind (line missing-nl) (read-line stream nil nil)
               (cond
                 ((null line)
                  (return (if lines
                              (format nil "~{~A~^~%~}~%" (nreverse lines))
                              nil)))
                 ((string= line "")
                  (if seen-content
                      (return (format nil "~{~A~^~%~}~%~%" (nreverse lines)))
                      nil))  ; Skip leading blank lines
                 (t
                  (setf seen-content t)
                  (push line lines)))))))

        ;; Single character separator (common case, optimized)
        ((= (length sep) 1)
         (let ((sep-char (char sep 0))
               (result (make-string-output-stream)))
           (loop for char = (read-char stream nil nil)
                 while char
                 do (write-char char result)
                 when (char= char sep-char)
                   return (get-output-stream-string result))
           ;; EOF reached
           (let ((str (get-output-stream-string result)))
             (if (string= str "") nil str))))

        ;; Multi-character separator
        (t
         (let ((result (make-string-output-stream))
               (sep-len (length sep)))
           (loop for char = (read-char stream nil nil)
                 while char
                 do (write-char char result)
                 when (and (>= (file-position result) sep-len)
                           (string= (subseq (get-output-stream-string result)
                                            (- (file-position result) sep-len))
                                    sep))
                   return (get-output-stream-string result))
           (let ((str (get-output-stream-string result)))
             (if (string= str "") nil str))))))))
```

### 4. Update pl-print to Respect $\

The output record separator `$\` is appended after each `print` (but not `say`):

```lisp
;; In pl-print, after printing all arguments:
(let ((ors (if (pl-box-p |$\\|) (pl-box-value |$\\|) |$\\|)))
  (when (and ors (not (string= ors "")))
    (princ ors stream)))
```

### 5. Update Array Interpolation to Respect $"

When arrays are interpolated in strings, `$"` is the separator:

```lisp
;; Wherever array interpolation happens:
(let ((list-sep (if (pl-box-p |$"|) (pl-box-value |$"|) |$"|)))
  (format nil "~{~A~}" list-sep array-elements))
```

## Files to Modify

1. **`cl/pcl-runtime.lisp`**:
   - Change `|$/|`, `|$\\|`, `|$"|` to boxed variables
   - Add `get-input-record-separator` helper
   - Rewrite `pl-readline` to respect `$/`
   - Update `pl-print` to respect `$\`
   - Update string interpolation for `$"`

## Testing

After implementation:
```bash
./run-perl-test.pl perl-tests/chop.t
```

The `$/ = "oo"` assignment should work, and readline should respect the new separator.

## Notes

- The reference-to-integer mode (`$/ = \4096` for fixed-record) is rare and can be deferred
- `chomp` uses `$/` to determine what to remove - verify this still works after boxing
- Local `$/` in blocks should work automatically with dynamic binding since we use `defvar`

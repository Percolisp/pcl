;; cl/pcl-pack.lisp — full pack/unpack implementation for PCL
;; Loaded automatically from pcl-runtime.lisp when present.
;; Redefines the stub p-pack and p-unpack from pcl-runtime.lisp.
;;
;; Template parsing follows Perl's S_next_symbol() in pp_pack.c:
;;   code → ! modifier → >/< endian modifier → count → /
;; Groups (...)N are processed RECURSIVELY with inherited endianness,
;; not by pre-expansion — pre-expansion cannot propagate group modifiers.
(in-package :pcl)

;;; ============================================================
;;; Size/sign table — x86-64 Linux
;;; ============================================================
;;; Returns (values nbytes signedp big-endian-default)
;;; l/L are always 32-bit in Perl; l!/L! are 64-bit (native long).
;;; n/N are unsigned big-endian; v/V are unsigned little-endian.
;;; n!/N! or v!/V! make them signed.

(defun %pack-type-info (ch bang)
  (case ch
    (#\c (values 1 t   nil))
    (#\C (values 1 nil nil))
    (#\s (values 2 t   nil))
    (#\S (values 2 nil nil))
    ((#\n #\v) (values 2 (if bang t nil) (char= ch #\n)))
    ((#\i #\I) (values 4 (char= ch #\i) nil))
    (#\l (values (if bang 8 4) t   nil))
    (#\L (values (if bang 8 4) nil nil))
    ((#\N #\V) (values 4 (if bang t nil) (char= ch #\N)))
    ((#\q #\Q) (values 8 (char= ch #\q) nil))
    ((#\j #\J) (values 8 (char= ch #\j) nil))
    (t         nil)))

;;; ============================================================
;;; Helpers
;;; ============================================================

(defun %pack-skip-ws (s ti tlen)
  "Skip whitespace and # comments. Returns new ti."
  (loop
    (when (>= ti tlen) (return ti))
    (let ((ch (char s ti)))
      (cond
        ((or (char= ch #\Space) (char= ch #\Tab) (char= ch #\Newline)
             (char= ch #\Return) (char= ch #\Page))
         (incf ti))
        ((char= ch #\#)
         (incf ti)
         (loop while (and (< ti tlen) (char/= (char s ti) #\Newline)) do (incf ti))
         (when (< ti tlen) (incf ti)))   ; skip the newline
        (t (return ti))))))

(defun %pack-find-group-end (s ti tlen)
  "Find the index of the matching ')' in s starting at ti (after opening '(').
   Skips nested () and # comments. Returns index of the ')'."
  (let ((depth 1))
    (loop while (and (< ti tlen) (> depth 0)) do
      (let ((ch (char s ti)))
        (cond
          ((char= ch #\#)
           (incf ti)
           (loop while (and (< ti tlen) (char/= (char s ti) #\Newline)) do (incf ti)))
          ((char= ch #\() (incf depth) (incf ti))
          ((char= ch #\)) (decf depth) (when (> depth 0) (incf ti)))
          (t (incf ti)))))
    ti))  ; points at ')'

(defun %pack-emit-int (val nbytes signedp big-endian result)
  (let* ((max  (expt 2 (* nbytes 8)))
         (v    (if (and signedp (< val 0)) (+ val max) val))
         (v    (logand v (1- max))))
    (if big-endian
        (loop for byte-idx from (1- nbytes) downto 0
              do (vector-push-extend
                  (code-char (logand #xff (ash v (* -8 byte-idx)))) result))
        (dotimes (byte-idx nbytes)
          (vector-push-extend
           (code-char (logand #xff (ash v (* -8 byte-idx)))) result)))))

(defun %pack-to-int (arg)
  (let ((v (to-number (unbox arg))))
    (if (or (%pcl-nan-p v)
            (and (floatp v) (= v (/ v 2))))   ; Inf
        0
        (truncate v))))

;;; ============================================================
;;; p-pack
;;; ============================================================
(defun p-pack (template &rest args)
  "Perl pack() — full implementation with recursive group processing."
  (let* ((raw-tmpl (to-string (unbox template)))
         (result   (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (arg-idx  0)
         (nargs    (length args)))
    (flet ((next-arg ()
             (if (< arg-idx nargs)
                 (prog1 (nth arg-idx args) (incf arg-idx))
                 *p-undef*))
           (push-byte (b)
             (vector-push-extend (code-char (logand b #xff)) result)))
      (labels
          ((pack-scalar (ch bang count nrep star-p force-be force-le)
             "Dispatch scalar format character ch — called from pack-tmpl normal mode."
             (case ch
               (#\f
                (dotimes (_ nrep)
                  (let* ((v    (coerce (to-number (unbox (next-arg))) 'single-float))
                         (bits (sb-kernel:single-float-bits v))
                         (be   (cond (force-le nil) (force-be t) (t nil))))
                    (if be
                        (loop for k from 3 downto 0 do (push-byte (logand #xff (ash bits (* -8 k)))))
                        (dotimes (k 4) (push-byte (logand #xff (ash bits (* -8 k)))))))))
               ((#\d #\F)
                (dotimes (_ nrep)
                  (let* ((v    (coerce (to-number (unbox (next-arg))) 'double-float))
                         (bits (sb-kernel:double-float-bits v))
                         (be   (cond (force-le nil) (force-be t) (t nil))))
                    (if be
                        (loop for k from 7 downto 0 do (push-byte (logand #xff (ash bits (* -8 k)))))
                        (dotimes (k 8) (push-byte (logand #xff (ash bits (* -8 k)))))))))
               (#\a
                (let* ((str (to-string (unbox (next-arg))))
                       (len (if star-p (length str) nrep)))
                  (loop for k from 0 below len
                        do (push-byte (if (< k (length str)) (char-code (char str k)) 0)))))
               (#\A
                (let* ((str (to-string (unbox (next-arg))))
                       (len (if star-p (length str) nrep)))
                  (loop for k from 0 below len
                        do (push-byte (if (< k (length str)) (char-code (char str k)) 32)))))
               (#\Z
                (let* ((str (to-string (unbox (next-arg))))
                       (len (if star-p (1+ (length str)) nrep)))
                  (loop for k from 0 below (max 0 (1- len))
                        do (push-byte (if (< k (length str)) (char-code (char str k)) 0)))
                  (push-byte 0)))
               (#\b
                (let* ((str   (to-string (unbox (next-arg))))
                       (nbits (if star-p (length str) nrep)))
                  (loop for byte-start from 0 below nbits by 8
                        do (let ((byte 0))
                             (loop for bit from 0 below 8
                                   for idx = (+ byte-start bit)
                                   when (and (< idx nbits) (< idx (length str))
                                             (char= (char str idx) #\1))
                                   do (setf byte (logior byte (ash 1 bit))))
                             (push-byte byte)))))
               (#\B
                (let* ((str   (to-string (unbox (next-arg))))
                       (nbits (if star-p (length str) nrep)))
                  (loop for byte-start from 0 below nbits by 8
                        do (let ((byte 0))
                             (loop for bit from 0 below 8
                                   for idx = (+ byte-start bit)
                                   when (and (< idx nbits) (< idx (length str))
                                             (char= (char str idx) #\1))
                                   do (setf byte (logior byte (ash 1 (- 7 bit)))))
                             (push-byte byte)))))
               (#\H
                (let* ((str     (to-string (unbox (next-arg))))
                       (nybbles (if star-p (length str) (* 2 nrep))))
                  (loop for k from 0 below nybbles by 2
                        do (push-byte
                            (logior
                             (ash (or (and (< k (length str)) (digit-char-p (char str k) 16)) 0) 4)
                             (or (and (< (1+ k) (length str)) (digit-char-p (char str (1+ k)) 16)) 0))))))
               (#\h
                (let* ((str     (to-string (unbox (next-arg))))
                       (nybbles (if star-p (length str) (* 2 nrep))))
                  (loop for k from 0 below nybbles by 2
                        do (push-byte
                            (logior
                             (ash (or (and (< (1+ k) (length str)) (digit-char-p (char str (1+ k)) 16)) 0) 4)
                             (or (and (< k (length str)) (digit-char-p (char str k) 16)) 0))))))
               (#\x (dotimes (_ nrep) (push-byte 0)))
               (#\X (setf (fill-pointer result) (max 0 (- (fill-pointer result) nrep))))
               (#\@
                (let ((target (or count 0)))
                  (cond ((< (fill-pointer result) target)
                         (loop repeat (- target (fill-pointer result)) do (push-byte 0)))
                        (t (setf (fill-pointer result) target)))))
               (#\u
                (let* ((str  (to-string (unbox (next-arg))))
                       (slen (length str)))
                  (loop for chunk-start from 0 below slen by 45
                        do (let* ((ce    (min (+ chunk-start 45) slen))
                                  (chunk (subseq str chunk-start ce))
                                  (clen  (length chunk)))
                             (push-byte (+ 32 clen))
                             (loop for k from 0 below clen by 3
                                   do (let* ((b0 (char-code (char chunk k)))
                                             (b1 (if (< (1+ k) clen) (char-code (char chunk (1+ k))) 0))
                                             (b2 (if (< (+ k 2) clen) (char-code (char chunk (+ k 2))) 0))
                                             (cm (logior (ash b0 16) (ash b1 8) b2)))
                                        (flet ((uu-c (v)
                                                 (let ((c (+ 32 (logand v 63))))
                                                   (if (= c 32) 96 c))))
                                          (push-byte (uu-c (ash cm -18)))
                                          (push-byte (uu-c (ash cm -12)))
                                          (push-byte (uu-c (ash cm -6)))
                                          (push-byte (uu-c cm)))))
                             (push-byte (char-code #\Newline))))))
               (#\U
                (dotimes (_ nrep)
                  (let ((code (%pack-to-int (next-arg))))
                    (cond
                      ((< code #x80) (push-byte code))
                      ((< code #x800)
                       (push-byte (logior #xC0 (ash code -6)))
                       (push-byte (logior #x80 (logand code #x3F))))
                      ((< code #x10000)
                       (push-byte (logior #xE0 (ash code -12)))
                       (push-byte (logior #x80 (logand (ash code -6) #x3F)))
                       (push-byte (logior #x80 (logand code #x3F))))
                      (t
                       (push-byte (logior #xF0 (ash code -18)))
                       (push-byte (logior #x80 (logand (ash code -12) #x3F)))
                       (push-byte (logior #x80 (logand (ash code -6) #x3F)))
                       (push-byte (logior #x80 (logand code #x3F))))))))
               (#\W
                (dotimes (_ nrep)
                  (push-byte (logand (%pack-to-int (next-arg)) #xff))))
               (#\w
                (dotimes (_ nrep)
                  (let ((v (abs (%pack-to-int (next-arg)))))
                    (if (= v 0)
                        (push-byte 0)
                        (let ((bytes '()))
                          (loop while (> v 0)
                                do (push (logand v #x7f) bytes) (setf v (ash v -7)))
                          (loop for (b . rest) on bytes
                                do (push-byte (if rest (logior b #x80) b))))))))
               ((#\p #\P #\D) nil)
               (otherwise (error "Invalid type '~A' in pack" ch))))

           (pack-tmpl (s inh-be inh-le)
             "Pack items from template string s.
              inh-be / inh-le are endianness inherited from an enclosing group."
             (let ((ti 0) (tlen (length s)))
               (loop
                (setf ti (%pack-skip-ws s ti tlen))
                (when (>= ti tlen) (return))
                (let ((ch (char s ti)))
                  (incf ti)
                  (let ((grpbeg nil) (grpend nil))
                    (when (char= ch #\()
                      (let ((ge (%pack-find-group-end s ti tlen)))
                        (setf grpbeg ti grpend ge ti (1+ ge))))
                    (let ((bang nil) (force-be inh-be) (force-le inh-le))
                      (loop while (< ti tlen) do
                            (case (char s ti)
                              (#\! (setf bang t) (incf ti))
                              (#\> (setf force-be t force-le nil) (incf ti))
                              (#\< (setf force-le t force-be nil) (incf ti))
                              (t (return))))
                      (setf ti (%pack-skip-ws s ti tlen))
                      (let* ((star-p (and (< ti tlen) (char= (char s ti) #\*)))
                             (count  (cond
                                       (star-p (incf ti) nil)
                                       ((and (< ti tlen) (char= (char s ti) #\[))
                                        (incf ti)
                                        (let ((n 0))
                                          (loop while (and (< ti tlen) (digit-char-p (char s ti)))
                                                do (setf n (+ (* n 10) (digit-char-p (char s ti))))
                                                (incf ti))
                                          (when (and (< ti tlen) (char= (char s ti) #\])) (incf ti))
                                          n))
                                       ((and (< ti tlen) (digit-char-p (char s ti)))
                                        (let ((n 0))
                                          (loop while (and (< ti tlen) (digit-char-p (char s ti)))
                                                do (setf n (+ (* n 10) (digit-char-p (char s ti))))
                                                (incf ti))
                                          n))
                                       (t 1)))
                             (nrep   (or count 1)))
                        (setf ti (%pack-skip-ws s ti tlen))
                        (if (and (< ti tlen) (char= (char s ti) #\/))
                            ;; --- Slash mode ---
                            (progn
                              (incf ti)
                              (let* ((slash-arg (next-arg))
                                     (slash-str  (to-string (unbox slash-arg)))
                                     (slash-n    (length slash-str)))
                                (multiple-value-bind (nbytes signedp default-be)
                                    (%pack-type-info ch bang)
                                  (when nbytes
                                    (let ((be (cond (force-be t) (force-le nil) (t default-be))))
                                      (%pack-emit-int slash-n nbytes signedp be result))))
                                (setf ti (%pack-skip-ws s ti tlen))
                                (when (< ti tlen)
                                  (let* ((dch   (char s ti))
                                         (dbang nil)
                                         (dbe   force-be)
                                         (dle   force-le))
                                    (incf ti)
                                    (loop while (< ti tlen) do
                                          (case (char s ti)
                                            (#\! (setf dbang t) (incf ti))
                                            (#\> (setf dbe t dle nil) (incf ti))
                                            (#\< (setf dle t dbe nil) (incf ti))
                                            (t (return))))
                                    (setf ti (%pack-skip-ws s ti tlen))
                                    (let* ((dall-p (and (< ti tlen) (char= (char s ti) #\*)))
                                           (dcnt   (cond
                                                     (dall-p (incf ti) nil)
                                                     ((and (< ti tlen) (digit-char-p (char s ti)))
                                                      (let ((n 0))
                                                        (loop while (and (< ti tlen) (digit-char-p (char s ti)))
                                                              do (setf n (+ (* n 10) (digit-char-p (char s ti))))
                                                              (incf ti))
                                                        n))
                                                     (t nil)))
                                           (dnrep  (or dcnt slash-n)))
                                      (case dch
                                        ((#\a #\A)
                                         (loop for k from 0 below dnrep
                                               do (push-byte (if (< k (length slash-str))
                                                                 (char-code (char slash-str k))
                                                                 (if (char= dch #\A) 32 0)))))
                                        (#\Z
                                         (loop for k from 0 below (max 0 (1- dnrep))
                                               do (push-byte (if (< k (length slash-str))
                                                                 (char-code (char slash-str k)) 0)))
                                         (push-byte 0))
                                        (otherwise
                                         (multiple-value-bind (dnbytes dsignedp ddefault-be)
                                             (%pack-type-info dch (if dbang t nil))
                                           (when dnbytes
                                             (let ((dbe2 (cond (dbe t) (dle nil) (t ddefault-be))))
                                               (dotimes (_ dnrep)
                                                 (%pack-emit-int (%pack-to-int (next-arg))
                                                                 dnbytes dsignedp dbe2 result))))))))))))
                            ;; --- Normal mode ---
                            (if grpbeg
                                (let ((inner (subseq s grpbeg grpend)))
                                  (if star-p
                                      (loop while (< arg-idx nargs) do
                                            (pack-tmpl inner force-be force-le))
                                      (dotimes (_ nrep)
                                        (pack-tmpl inner force-be force-le))))
                                (multiple-value-bind (nbytes signedp default-be)
                                    (%pack-type-info ch bang)
                                  (if nbytes
                                      (let ((be (cond (force-be t) (force-le nil) (t default-be))))
                                        (dotimes (_ nrep)
                                          (%pack-emit-int (%pack-to-int (next-arg))
                                                          nbytes signedp be result)))
                                      (pack-scalar ch bang count nrep star-p
                                                   force-be force-le))))))))))
               )))
        (pack-tmpl raw-tmpl nil nil)
        result))))

;;; ============================================================
;;; Read N bytes from string as integer
;;; ============================================================

        (pack-tmpl raw-tmpl nil nil)
        result))))

;;; ============================================================
;;; Read N bytes from string as integer
;;; ============================================================

(defun %unpack-read-int (s si nbytes big-endian signedp)
  (let ((v 0) (slen (length s)))
    (if big-endian
        (loop for k from 0 below nbytes
              do (setf v (logior (ash v 8)
                                 (if (< (+ si k) slen) (char-code (char s (+ si k))) 0))))
        (loop for k from (1- nbytes) downto 0
              do (setf v (logior (ash v 8)
(defun p-unpack (template &optional (str $_))
  "Perl unpack() — full implementation with recursive group processing."
  (let* ((raw-tmpl (to-string (unbox template)))
         (raw-s    (to-string (unbox str)))
         ;; %N checksum detection — on stripped+ws-free template
         (tmpl0    (let ((r (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
                         (i 0) (len (length raw-tmpl)))
                     (loop while (< i len) do
                           (let ((ch (char raw-tmpl i)))
                             (cond
                               ((char= ch #\#)
                                (incf i)
                                (loop while (and (< i len) (char/= (char raw-tmpl i) #\Newline)) do (incf i)))
                               ((member ch '(#\Space #\Tab #\Newline #\Return #\Page)) (incf i))
                               (t (vector-push-extend ch r) (incf i)))))
                     r))
         (checksum-p (and (> (length tmpl0) 0) (char= (char tmpl0 0) #\%)))
         (checksum-width 16)
         (tmpl-start (if checksum-p
                         (let* ((di (or (position-if-not #'digit-char-p tmpl0 :start 1)
                                        (length tmpl0)))
                                (ws (subseq tmpl0 1 di)))
                           (setf checksum-width (if (zerop (length ws)) 16 (parse-integer ws)))
                           di)
                         0))
         ;; U0 mode detection — from the effective template start
         (utf8-mode (and (>= (- (length tmpl0) tmpl-start) 2)
                         (char= (char tmpl0 tmpl-start) #\U)
                         (char= (char tmpl0 (1+ tmpl-start)) #\0)))
         ;; The source string: re-encode as UTF-8 bytes if U0 mode
         (s (if utf8-mode
                (flet ((encode-char (code)
                         (cond
                           ((< code #x80) (list code))
                           ((< code #x800)
                            (list (logior #xC0 (ash code -6))
                                  (logior #x80 (logand code #x3F))))
                           ((< code #x10000)
                            (list (logior #xE0 (ash code -12))
                                  (logior #x80 (logand (ash code -6) #x3F))
                                  (logior #x80 (logand code #x3F))))
                           (t
                            (list (logior #xF0 (ash code -18))
                                  (logior #x80 (logand (ash code -12) #x3F))
                                  (logior #x80 (logand (ash code -6) #x3F))
                                  (logior #x80 (logand code #x3F)))))))
                  (map 'string #'code-char
                       (loop for c across raw-s nconc (encode-char (char-code c)))))
                raw-s))
         (slen     (length s))
         (result   (make-array 0 :adjustable t :fill-pointer 0))
         (checksum 0)
         (si       0))   ; mutable source position — shared across recursive calls
    (flet ((byte-at (i) (if (< i slen) (char-code (char s i)) 0))
           (push-val (v)
             (if checksum-p
                 (setf checksum (+ checksum (to-number (unbox v))))
                 (vector-push-extend (make-p-box v) result))))
      (labels
          ((unpack-tmpl (tmpl inh-be inh-le)
             "Unpack from global s/si using template tmpl.
              inh-be / inh-le = endianness inherited from enclosing group."
             (let ((ti 0) (tlen (length tmpl)))
               (loop
                (setf ti (%pack-skip-ws tmpl ti tlen))
                (when (>= ti tlen) (return))
                (let ((ch (char tmpl ti)))
                  (incf ti)
                  (let ((grpbeg nil) (grpend nil))
                    (when (char= ch #\()
                      (let ((ge (%pack-find-group-end tmpl ti tlen)))
                        (setf grpbeg ti grpend ge ti (1+ ge))))
                    (let ((bang nil) (force-be inh-be) (force-le inh-le))
                      (loop while (< ti tlen) do
                            (case (char tmpl ti)
                              (#\! (setf bang t) (incf ti))
                              (#\> (setf force-be t force-le nil) (incf ti))
                              (#\< (setf force-le t force-be nil) (incf ti))
                              (t (return))))
                      ;; Count
                      (setf ti (%pack-skip-ws tmpl ti tlen))
                      (let* ((all-p  (and (< ti tlen) (char= (char tmpl ti) #\*)))
                             (count  (cond
                                       (all-p (incf ti) nil)
                                       ((and (< ti tlen) (char= (char tmpl ti) #\[))
                                        (incf ti)
                                        (let ((n 0))
                                          (loop while (and (< ti tlen) (digit-char-p (char tmpl ti)))
                                                do (setf n (+ (* n 10) (digit-char-p (char tmpl ti))))
                                                (incf ti))
                                          (when (and (< ti tlen) (char= (char tmpl ti) #\])) (incf ti))
                                          n))
                                       ((and (< ti tlen) (digit-char-p (char tmpl ti)))
                                        (let ((n 0))
                                          (loop while (and (< ti tlen) (digit-char-p (char tmpl ti)))
                                                do (setf n (+ (* n 10) (digit-char-p (char tmpl ti))))
                                                (incf ti))
                                          n))
                                       (t 1)))
                             (nrep   (or count 1)))
                        ;; / slash mode
                        (setf ti (%pack-skip-ws tmpl ti tlen))
                        (if (and (< ti tlen) (char= (char tmpl ti) #\/))
                            ;; --- Slash mode ---
                            (progn
                              (incf ti)
                              (let* (;; Unpack count using ch
                                     (slash-n
                                      (multiple-value-bind (nbytes signedp default-be)
                                          (%pack-type-info ch bang)
                                        (if nbytes
                                            (let* ((be (cond (force-be t) (force-le nil) (t default-be)))
                                                   (v  (%unpack-read-int s si nbytes be signedp)))
                                              (incf si nbytes)
                                              v)
                                            (if (char= ch #\w)
                                                (let ((v 0))
                                                  (loop do (let ((b (byte-at si)))
                                                             (incf si)
                                                             (setf v (logior (ash v 7) (logand b #x7f)))
                                                             (when (zerop (logand b #x80)) (return))))
                                                  v)
                                                ;; string type: read then parse as integer
                                                (let* ((n   (if all-p (max 0 (- slen si)) nrep))
                                                       (raw (subseq s (min si slen) (min (+ si n) slen)))
                                                       (str (cond
                                                              ((char= ch #\A) (string-right-trim '(#\Space #\Nul) raw))
                                                              ((char= ch #\Z)
                                                               (let ((np (position #\Nul raw)))
                                                                 (if np (subseq raw 0 np) raw)))
                                                              (t raw))))
                                                  (incf si n)
                                                  (if (zerop (length str)) 0
                                                      (or (parse-integer str :junk-allowed t) 0)))))))
                                     ;; Read data format
                                     (dch   (progn (setf ti (%pack-skip-ws tmpl ti tlen))
                                                   (when (< ti tlen)
                                                     (prog1 (char tmpl ti) (incf ti)))))
                                     (dbang nil) (dbe force-be) (dle force-le))
                                ;; data modifiers
                                (when dch
                                  (loop while (< ti tlen) do
                                        (case (char tmpl ti)
                                          (#\! (setf dbang t) (incf ti))
                                          (#\> (setf dbe t dle nil) (incf ti))
                                          (#\< (setf dle t dbe nil) (incf ti))
                                          (t (return))))
                                  ;; data count
                                  (setf ti (%pack-skip-ws tmpl ti tlen))
                                  (let* ((dall-p (and (< ti tlen) (char= (char tmpl ti) #\*)))
                                         (dcnt   (cond
                                                   (dall-p (incf ti) nil)
                                                   ((and (< ti tlen) (digit-char-p (char tmpl ti)))
                                                    (let ((n 0))
                                                      (loop while (and (< ti tlen) (digit-char-p (char tmpl ti)))
                                                            do (setf n (+ (* n 10) (digit-char-p (char tmpl ti))))
                                                            (incf ti))
                                                      n))
                                                   (t nil)))
                                         (dnrep  (or dcnt slash-n)))
                                    (multiple-value-bind (dnbytes dsignedp ddefault-be)
                                        (%pack-type-info dch (if dbang t nil))
                                      (if dnbytes
                                          (let* ((dbe2 (cond (dbe t) (dle nil) (t ddefault-be)))
                                                 (n    dnrep))
                                            (dotimes (i n)
                                              (push-val (%unpack-read-int s (+ si (* i dnbytes))
                                                                          dnbytes dbe2 dsignedp)))
                                            (incf si (* n dnbytes)))
                                          (case dch
                                            ((#\A #\a #\Z)
                                             (let* ((n   dnrep)
                                                    (raw (subseq s (min si slen) (min (+ si n) slen))))
                                               (push-val (cond
                                                           ((char= dch #\A) (string-right-trim '(#\Space #\Nul) raw))
                                                           ((char= dch #\Z)
                                                            (let ((np (position #\Nul raw)))
                                                              (if np (subseq raw 0 np) raw)))
                                                           (t raw)))
                                               (incf si n)))
                                            (#\(
                                             (let* ((ge    (%pack-find-group-end tmpl ti tlen))
                                                    (inner (subseq tmpl ti ge)))
                                               (setf ti (1+ ge))
                                               (if dall-p
                                                   (loop while (< si slen)
                                                         do (unpack-tmpl inner force-be force-le))
                                                   (dotimes (_ dnrep)
                                                     (unpack-tmpl inner force-be force-le)))))
                                            (otherwise nil)))))))))
                        ;; --- Normal mode ---
                        (flet ((rcount (item-bytes)
                                 (if all-p (max 0 (floor (- slen si) (max 1 item-bytes))) nrep)))
                          (if grpbeg
                              ;; Group: recurse
                              (let ((inner (subseq tmpl grpbeg grpend)))
                                (if all-p
                                    (loop while (< si slen) do
                                          (unpack-tmpl inner force-be force-le))
                                    (dotimes (_ nrep)
                                      (unpack-tmpl inner force-be force-le))))
                              ;; Scalar dispatch
                              (multiple-value-bind (nbytes signedp default-be)
                                  (%pack-type-info ch bang)
                                (if nbytes
                                    (let* ((be (cond (force-be t) (force-le nil) (t default-be)))
                                           (n  (rcount nbytes)))
                                      (dotimes (i n)
                                        (push-val (%unpack-read-int s (+ si (* i nbytes))
                                                                    nbytes be signedp)))
                                      (incf si (* n nbytes)))
                                    (case ch
                                      ;; float 32-bit
                                      (#\f
                                       (let* ((be (cond (force-le nil) (force-be t) (t nil)))
                                              (n  (rcount 4)))
                                         (dotimes (i n)
                                           (let* ((pos  (+ si (* i 4)))
                                                  (bits (if be
                                                            (logior (ash (byte-at pos) 24)
                                                                    (ash (byte-at (+ pos 1)) 16)
                                                                    (ash (byte-at (+ pos 2)) 8)
                                                                    (byte-at (+ pos 3)))
                                                            (logior (byte-at pos)
                                                                    (ash (byte-at (+ pos 1)) 8)
                                                                    (ash (byte-at (+ pos 2)) 16)
                                                                    (ash (byte-at (+ pos 3)) 24)))))
                                             (push-val (sb-kernel:make-single-float bits))))
                                         (incf si (* n 4))))
                                      ;; float 64-bit
                                      ((#\d #\F)
                                       (let* ((be (cond (force-le nil) (force-be t) (t nil)))
                                              (n  (rcount 8)))
                                         (dotimes (i n)
                                           (let* ((pos (+ si (* i 8)))
                                                  (raw (if be
                                                           (loop for k from 0 below 8
                                                                 sum (ash (byte-at (+ pos k)) (* 8 (- 7 k))))
                                                           (loop for k from 0 below 8
                                                                 sum (ash (byte-at (+ pos k)) (* 8 k))))))
                                             (push-val (sb-kernel:make-double-float
                                                        (ldb (byte 32 32) raw)
                                                        (ldb (byte 32 0)  raw)))))
                                         (incf si (* n 8))))
                                      ;; strings
                                      ((#\A #\a #\Z)
                                       (let* ((n   (if all-p (max 0 (- slen si)) nrep))
                                              (raw (subseq s (min si slen) (min (+ si n) slen))))
                                         (push-val (cond
                                                     ((char= ch #\A) (string-right-trim '(#\Space #\Nul) raw))
                                                     ((char= ch #\Z)
                                                      (let ((np (position #\Nul raw)))
                                                        (if np (subseq raw 0 np) raw)))
                                                     (t raw)))
                                         (incf si n)))
                                      ;; hex H (high nybble first)
                                      (#\H
                                       (let* ((n   (if all-p (* 2 (- slen si)) (* 2 nrep)))
                                              (hex (make-string n :initial-element #\0)))
                                         (dotimes (i (floor n 2))
                                           (let ((b (byte-at (+ si i))))
                                             (setf (char hex (* i 2))      (digit-char (ash b -4) 16)
                                                   (char hex (1+ (* i 2))) (digit-char (logand b 15) 16))))
                                         (push-val (string-downcase hex))
                                         (incf si (ceiling n 2))))
                                      ;; hex h (low nybble first)
                                      (#\h
                                       (let* ((n   (if all-p (* 2 (- slen si)) (* 2 nrep)))
                                              (hex (make-string n :initial-element #\0)))
                                         (dotimes (i (floor n 2))
                                           (let ((b (byte-at (+ si i))))
                                             (setf (char hex (* i 2))      (digit-char (logand b 15) 16)
                                                   (char hex (1+ (* i 2))) (digit-char (ash b -4) 16))))
                                         (push-val (string-downcase hex))
                                         (incf si (ceiling n 2))))
                                      ;; bit string B (MSB first)
                                      (#\B
                                       (let* ((nbits (if all-p (* 8 (- slen si)) nrep))
                                              (bits  (make-string nbits :initial-element #\0)))
                                         (dotimes (i nbits)
                                           (setf (char bits i)
                                                 (if (logbitp (- 7 (mod i 8)) (byte-at (+ si (floor i 8))))
                                                     #\1 #\0)))
                                         (push-val bits)
                                         (incf si (ceiling nbits 8))))
                                      ;; bit string b (LSB first)
                                      (#\b
                                       (let* ((nbits (if all-p (* 8 (- slen si)) nrep))
                                              (bits  (make-string nbits :initial-element #\0)))
                                         (dotimes (i nbits)
                                           (setf (char bits i)
                                                 (if (logbitp (mod i 8) (byte-at (+ si (floor i 8))))
                                                     #\1 #\0)))
                                         (push-val bits)
                                         (incf si (ceiling nbits 8))))
                                      ;; skip / back / seek
                                      (#\x (incf si (if all-p (- slen si) nrep)))
                                      (#\X (decf si nrep) (when (< si 0) (setf si 0)))
                                      (#\@ (setf si (or count 0)))
                                      ;; uudecode
                                      (#\u
                                       (let ((decoded (make-array 0 :element-type 'character
                                                                  :adjustable t :fill-pointer 0)))
                                         (flet ((uu-val (c) (logand (- (char-code c) 32) 63)))
                                           (loop while (< si slen)
                                                 do (let* ((len-char (char s si))
                                                           (nbytes   (uu-val len-char)))
                                                      (incf si)
                                                      (when (zerop nbytes) (return))
                                                      (let* ((ngroups (ceiling nbytes 3))
                                                             (chars   (* ngroups 4)))
                                                        (loop for k from 0 below ngroups
                                                              do (let* ((c0 (uu-val (if (< (+ si (* k 4))     slen) (char s (+ si (* k 4)))     #\Space)))
                                                                        (c1 (uu-val (if (< (+ si (* k 4) 1) slen) (char s (+ si (* k 4) 1)) #\Space)))
                                                                        (c2 (uu-val (if (< (+ si (* k 4) 2) slen) (char s (+ si (* k 4) 2)) #\Space)))
                                                                        (c3 (uu-val (if (< (+ si (* k 4) 3) slen) (char s (+ si (* k 4) 3)) #\Space)))
                                                                        (cm (logior (ash c0 18) (ash c1 12) (ash c2 6) c3)))
                                                                   (when (<= (* k 3) (1- nbytes))
                                                                     (vector-push-extend (code-char (logand (ash cm -16) #xff)) decoded))
                                                                   (when (<= (+ (* k 3) 1) (1- nbytes))
                                                                     (vector-push-extend (code-char (logand (ash cm -8) #xff)) decoded))
                                                                   (when (<= (+ (* k 3) 2) (1- nbytes))
                                                                     (vector-push-extend (code-char (logand cm #xff)) decoded))))
                                                        (incf si chars))
                                                      (when (and (< si slen) (char= (char s si) #\Newline))
                                                        (incf si)))))
                                         (push-val decoded)))
                                      ;; UTF-8 codepoints
                                      (#\U
                                       (let ((n (if all-p most-positive-fixnum nrep)))
                                         (loop repeat n while (< si slen)
                                               do (let* ((b0     (byte-at si))
                                                         (nbytes (cond ((< b0 #x80) 1)
                                                                       ((< b0 #xE0) 2)
                                                                       ((< b0 #xF0) 3)
                                                                       (t 4)))
                                                         (code   (cond
                                                                   ((= nbytes 1) b0)
                                                                   ((= nbytes 2)
                                                                    (logior (ash (logand b0 #x1F) 6)
                                                                            (logand (byte-at (+ si 1)) #x3F)))
                                                                   ((= nbytes 3)
                                                                    (logior (ash (logand b0 #x0F) 12)
                                                                            (ash (logand (byte-at (+ si 1)) #x3F) 6)
                                                                            (logand (byte-at (+ si 2)) #x3F)))
                                                                   (t
                                                                    (logior (ash (logand b0 #x07) 18)
                                                                            (ash (logand (byte-at (+ si 1)) #x3F) 12)
                                                                            (ash (logand (byte-at (+ si 2)) #x3F) 6)
                                                                            (logand (byte-at (+ si 3)) #x3F))))))
                                                    (push-val code)
                                                    (incf si nbytes)))))
                                      ;; wide byte
                                      (#\W
                                       (let ((n (rcount 1)))
                                         (dotimes (i n) (push-val (byte-at (+ si i))))
                                         (incf si n)))
                                      ;; BER integer
                                      (#\w
                                       (loop while (< si slen)
                                             do (let ((v 0))
                                                  (loop do (let ((b (byte-at si)))
                                                             (incf si)
                                                             (setf v (logior (ash v 7) (logand b #x7F)))
                                                             (when (zerop (logand b #x80)) (return))))
                                                  (push-val v))
                                             while all-p))
                                      ;; checksum prefix (only valid at top level, handled above)
                                      (#\% nil)
                                      ;; not implementable
                                      ((#\p #\P #\D) nil)
                                      ;; unknown
                                      (otherwise
                                       (unless (char= ch #\!)
                                         (error "Invalid type '~A' in unpack" ch))))))))))))))
             ))
        ;; Start unpack from tmpl0, skipping %N prefix and optional U0
        (let ((eff-tmpl (subseq tmpl0 (if utf8-mode (+ tmpl-start 2) tmpl-start))))
          (unpack-tmpl eff-tmpl nil nil))))
    ;; Return result
    (if checksum-p
        (let ((val (make-p-box (mod checksum (expt 2 checksum-width)))))
          (if (eq *wantarray* t)
              (let ((v (make-array 1 :adjustable t :fill-pointer 1)))
                (setf (aref v 0) val) v)
              val))
        (if (eq *wantarray* t)
            result
            (if (> (length result) 0) (aref result 0) *p-undef*)))))

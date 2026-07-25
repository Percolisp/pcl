;;;; pcl-xs.lisp — the PCL host adapter for pclxs (CPAN XS extensions).
;;;;
;;;; Loaded like any other PCL extension (docs/extensions.md): a file in the
;;;; runtime directory, pulled in by p-load-extension the first time XS is
;;;; actually needed.  Nothing here runs unless a module asks for it.
;;;;
;;;; WHAT THIS FILE IS
;;;;
;;;; pclxs (../pclxs, pinned by pcl/xs-pin) compiles a CPAN distribution's
;;;; C against replacement perl headers and links it to libpclxs.  libpclxs
;;;; then needs a HOST: something that owns the Perl-visible data and can
;;;; answer ~50 questions about it.  This file is that host, for PCL.
;;;;
;;;; The contract is `include/pclxs/pclxs_host.h` in that repo, and its
;;;; ownership rules O1-O5 are the part to read before changing anything
;;;; here.  Two of them shape this file directly:
;;;;
;;;;   O1/O2  A handle is an INDEX into *xs-objects*, never an address --
;;;;          SBCL's GC moves objects, so a pointer would be a bug that
;;;;          appears under load.  A live index is also the C side's strong
;;;;          reference: the table is NOT weak, so while C holds a handle
;;;;          the object cannot be collected.
;;;;
;;;;   O4     NOTHING may unwind out of a callback into C.  A CL condition
;;;;          crossing an alien-callable frame skips C cleanups and is
;;;;          platform-dependent.  Every callback body is therefore wrapped
;;;;          (see WITH-XS-GUARD); the two entries that can legitimately
;;;;          observe a Perl die report it as PS_DIED plus $@ instead.
;;;;
;;;; No new semantics live here.  Every callback is a thin adapter onto a
;;;; runtime function that already exists -- get_bool is p-true-p, get_iv is
;;;; to-number, bless is p-bless -- because the alternative is a second
;;;; implementation of Perl's rules that drifts from the first.

(in-package :pcl)

;;; ============================================================
;;; The shared library

(defvar *pclxs-lib-loaded* nil)
(defvar *pclxs-ctx* nil
  "The pclxs_ctx* we were given by pclxs_init, or NIL before initialisation.")

(defun %pclxs-dir ()
  "Where the pclxs checkout lives.  ONE rule, shared with tools/build-pclxs:
   $PCLXS_DIR, else a sibling of this checkout.  Anything that re-derives
   this rule elsewhere is a second source of truth."
  ;; *pcl-runtime-directory* is <pcl>/cl/, so the sibling of the CHECKOUT is
  ;; two levels up, not one.  (Getting this wrong looks like "libpclxs not
  ;; built" and sends you to build a library that is already built.)
  (or (sb-posix:getenv "PCLXS_DIR")
      (namestring (merge-pathnames "../../pclxs/"
                                   (or *pcl-runtime-directory*
                                       *default-pathname-defaults*)))))

(defun %pclxs-load-library ()
  (unless *pclxs-lib-loaded*
    (let ((path (merge-pathnames "build/libpclxs.so"
                                 (pathname (concatenate 'string
                                                        (%pclxs-dir) "/")))))
      (unless (probe-file path)
        (p-die (format nil "pclxs: ~A not built (run: cd ~A && perl build.pl lib)"
                       path (%pclxs-dir))))
      (sb-alien:load-shared-object (namestring path))
      (setf *pclxs-lib-loaded* t)))
  t)

;;; ============================================================
;;; The handle table (ownership rules O1/O2)
;;;
;;; id -> object, with a freelist.  `dup` allocates a second id for the same
;;; object so that releases stay 1:1 with handles and no per-object refcount
;;; is needed on this side.  Ids are 1-based: 0 means "none" in the contract.

(defvar *xs-objects* (make-array 256 :adjustable t :initial-element nil))
(defvar *xs-free-ids* '())
(defvar *xs-next-id* 1)

(declaim (inline %xs-deref))
(defun %xs-deref (h)
  (let ((i (1- h)))
    (if (and (>= i 0) (< i (length *xs-objects*)))
        (aref *xs-objects* i)
        nil)))

(defun %xs-intern (obj)
  "Give OBJ a handle.  NIL is the contract's 0 (\"none\")."
  (if (null obj)
      0
      (let ((id (if *xs-free-ids*
                    (pop *xs-free-ids*)
                    (prog1 *xs-next-id* (incf *xs-next-id*)))))
        (when (> id (length *xs-objects*))
          (adjust-array *xs-objects* (max (* 2 (length *xs-objects*)) id)
                        :initial-element nil))
        (setf (aref *xs-objects* (1- id)) obj)
        id)))

(defun %xs-release (h)
  (let ((i (1- h)))
    (when (and (>= i 0) (< i (length *xs-objects*)))
      (setf (aref *xs-objects* i) nil)
      (push h *xs-free-ids*))))

;;; A handle always names a BOX for scalars, never the unboxed value: that
;;; is what makes an lvalue fetch write through (rule O3), and what lets
;;; set_iv on an argument reach the caller's variable.
(defun %xs-box (h)
  (let ((o (%xs-deref h)))
    (if (p-box-p o) o nil)))

;;; ============================================================
;;; Guarding the boundary (ownership rule O4)

(defmacro with-xs-guard ((&key (on-error 0)) &body body)
  "Run BODY, and let NOTHING escape into C.
   A CL condition unwinding through an alien-callable frame would skip the
   C side's cleanups and is platform-dependent -- the pclxs design calls
   any apparent success at doing so a bug, not a shortcut.  So an
   unexpected condition here becomes ON-ERROR, and (because silence would
   be worse) a warning on *error-output*."
  `(handler-case (progn ,@body)
     (error (e)
       (format *error-output* "~&pclxs: host callback error: ~A~%" e)
       ,on-error)))

;;; ============================================================
;;; The callbacks
;;;
;;; One define-alien-callable per vtable entry.  They are deliberately
;;; boring: each maps onto a runtime function that already implements the
;;; Perl rule in question.

(sb-alien:define-alien-callable xs-release sb-alien:void
  ((h sb-alien:long))
  (with-xs-guard () (%xs-release h) (values)))

(sb-alien:define-alien-callable xs-dup sb-alien:long
  ((h sb-alien:long))
  (with-xs-guard () (%xs-intern (%xs-deref h))))

;;; ---- construction --------------------------------------------------------

(sb-alien:define-alien-callable xs-new-undef sb-alien:long ()
                                (with-xs-guard () (%xs-intern (make-p-box *p-undef*))))

(sb-alien:define-alien-callable xs-new-iv sb-alien:long
  ((v sb-alien:long))
  (with-xs-guard () (%xs-intern (make-p-box v))))

(sb-alien:define-alien-callable xs-new-nv sb-alien:long
  ((v sb-alien:double))
  (with-xs-guard () (%xs-intern (make-p-box v))))

(sb-alien:define-alien-callable xs-new-pvn sb-alien:long
  ((bytes (sb-alien:* sb-alien:char)) (len sb-alien:unsigned-long)
   (utf8 sb-alien:int))
  (with-xs-guard ()
    (%xs-intern (make-p-box (%xs-string-in bytes len utf8)))))

;;; ---- strings across the boundary (design §4, the byte rule) -------------
;;;
;;; C sees bytes plus a utf8 flag; PCL strings are sequences of characters.
;;; utf8=1 means the bytes are UTF-8 and decode to characters; utf8=0 means
;;; each byte IS a character code (Latin-1), which is exactly perl's
;;; downgraded/upgraded distinction.

(defun %xs-string-in (bytes len utf8)
  (let ((octets (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len)
      (setf (aref octets i)
            (ldb (byte 8 0) (sb-alien:deref bytes i))))
    (if (zerop utf8)
        (map 'string #'code-char octets)
        (sb-ext:octets-to-string octets :external-format :utf-8))))

(defun %xs-string-out (string)
  "Encode STRING the way the contract wants it: raw bytes when every
   character fits in one, UTF-8 otherwise.  Returns (values octets utf8)."
  (if (every (lambda (c) (< (char-code c) 256)) string)
      (values (map '(vector (unsigned-byte 8)) #'char-code string) 0)
      (values (sb-ext:string-to-octets string :external-format :utf-8) 1)))

;;; ---- scalar reads ---------------------------------------------------------
;;;
;;; The coercions are the runtime's, not new ones: to-number already knows
;;; that "3 apples" is 3, p-true-p already knows that "0.0" is true.

(sb-alien:define-alien-callable xs-get-iv sb-alien:long
  ((h sb-alien:long))
  (with-xs-guard ()
    (let ((n (to-number (unbox (%xs-deref h)))))
      (cond ((not (realp n)) 0)
            ;; Perl's IV is 64-bit and saturates; PCL integers are bignums.
            ((> n 9223372036854775807) 9223372036854775807)
            ((< n -9223372036854775808) -9223372036854775808)
            (t (truncate n))))))

(sb-alien:define-alien-callable xs-get-nv sb-alien:double
  ((h sb-alien:long))
  (with-xs-guard (:on-error 0d0)
    (let ((n (to-number (unbox (%xs-deref h)))))
      (if (realp n) (float n 1d0) 0d0))))

(sb-alien:define-alien-callable xs-get-bool sb-alien:int
  ((h sb-alien:long))
  (with-xs-guard () (if (p-true-p (unbox (%xs-deref h))) 1 0)))

(sb-alien:define-alien-callable xs-is-defined sb-alien:int
  ((h sb-alien:long))
  (with-xs-guard ()
    (let ((v (unbox (%xs-deref h))))
      (if (or (null v) (eq v *p-undef*)) 0 1))))

;; Perl's looks_like_number: a number is one, a string is one if the WHOLE
;; string parses as one, undef and everything else is not.
;;
;; This called `p-looks-like-number` until pclxs's conformance corpus was
;; first run against PCL, and there is no such function -- the runtime's is
;; `looks-like-number`, and it takes a string.  WITH-XS-GUARD dutifully
;; turned the undefined-function error into the on-error value, 0, so the
;; shim was told "not a number" about every value in the program and the
;; only symptom was four flag divergences.  A guard that must never let a
;; condition reach C will also, by construction, turn a typo into a
;; plausible answer; the corpus is what catches that.
(sb-alien:define-alien-callable xs-looks-like-number sb-alien:int
  ((h sb-alien:long))
  (with-xs-guard ()
    (let ((v (unbox (%xs-deref h))))
      (cond ((or (null v) (eq v *p-undef*)) 0)
            ((numberp v)                    1)
            ((stringp v) (if (looks-like-number v) 1 0))
            (t                              0)))))

;;; scalar_flags (pclxs ABI 3) -- "what IS this value", the question XS asks
;;; with SvIOK/SvNOK/SvPOK.  The shim calls it once for every argument
;;; entering an XSUB, because modules read the flags BEFORE they read the
;;; value (Params::Util's whole API is a raw flag test; JSON::XS picks
;;; number-vs-string encoding from them).
;;;
;;; This is NOT a coercion, and implementing it as one would be a quiet
;;; disaster: the string "12" must answer POK alone.  Answer POK|IOK and
;;; every string in the program starts being encoded as a number.  What
;;; perl then does to those flags when someone calls SvIV is the SHIM's
;;; business, and it already does it.
(sb-alien:define-alien-callable xs-scalar-flags sb-alien:unsigned-int
  ((h sb-alien:long))
  (with-xs-guard ()
    (let* ((cell (%xs-deref h))
           (v    (unbox cell)))
      (cond ((or (null v) (eq v *p-undef*)) 0)
            ((integerp v) #x0001)                        ; PS_SVF_IOK
            ((realp v)    #x0002)                        ; PS_SVF_NOK
            ((stringp v)
             ;; PS_SVF_POK, plus PS_SVF_UTF8 on the same rule
             ;; %xs-string-out uses when it hands the bytes over.
             (logior #x0004
                     (if (every (lambda (c) (< (char-code c) 256)) v)
                         0 #x0010)))
            ;; Anything else is a container, code, glob or scalar cell --
            ;; a reference in Perl's terms.  Ask the same function
            ;; xs-ref-type asks, so the two callbacks cannot disagree
            ;; about whether the shim should treat this as an RV.
            (t (let ((rt (p-reftype cell)))
                 (if (or (null rt) (eq rt *p-undef*) (equal rt ""))
                     0
                     #x0008)))))))                       ; PS_SVF_ROK

;;; get_pvn hands the string out through a sink the SHIM provides, so the
;;; buffer's lifetime is the call (rule O5) and neither side guesses.
(sb-alien:define-alien-callable xs-get-pvn sb-alien:void
  ((h sb-alien:long) (sink (sb-alien:* (sb-alien:function sb-alien:void
                                                          (sb-alien:* t)
                                                          (sb-alien:* sb-alien:char)
                                                          sb-alien:unsigned-long
                                                          sb-alien:int)))
   (ud (sb-alien:* t)))
  (with-xs-guard ()
    (let ((s (to-string (unbox (%xs-deref h)))))
      (multiple-value-bind (octets utf8) (%xs-string-out s)
        (sb-sys:with-pinned-objects (octets)
          (sb-alien:with-alien ((buf (sb-alien:* sb-alien:char)))
            (declare (ignorable buf))
            (let ((sap (sb-sys:vector-sap octets)))
              (sb-alien:alien-funcall
               sink ud (sb-alien:sap-alien sap (sb-alien:* sb-alien:char))
               (length octets) utf8))))))
    (values)))

;;; ---- scalar writes --------------------------------------------------------
;;;
;;; box-set, not a fresh box: the handle names the caller's box, so a write
;;; here is a write to the Perl variable the XSUB was given.

(sb-alien:define-alien-callable xs-set-iv sb-alien:void
  ((h sb-alien:long) (v sb-alien:long))
  (with-xs-guard ()
    (let ((b (%xs-box h))) (when b (box-set b v)))
    (values)))

(sb-alien:define-alien-callable xs-set-nv sb-alien:void
  ((h sb-alien:long) (v sb-alien:double))
  (with-xs-guard ()
    (let ((b (%xs-box h))) (when b (box-set b v)))
    (values)))

(sb-alien:define-alien-callable xs-set-pvn sb-alien:void
  ((h sb-alien:long) (bytes (sb-alien:* sb-alien:char))
   (len sb-alien:unsigned-long) (utf8 sb-alien:int))
  (with-xs-guard ()
    (let ((b (%xs-box h)))
      (when b (box-set b (%xs-string-in bytes len utf8))))
    (values)))

(sb-alien:define-alien-callable xs-set-undef sb-alien:void
  ((h sb-alien:long))
  (with-xs-guard ()
    (let ((b (%xs-box h))) (when b (box-set b *p-undef*)))
    (values)))

(sb-alien:define-alien-callable xs-set-sv sb-alien:void
  ((dst sb-alien:long) (src sb-alien:long))
  (with-xs-guard ()
    (let ((d (%xs-box dst)))
      (when d (box-set d (unbox (%xs-deref src)))))
    (values)))

;;; ============================================================
;;; Initialisation
;;;
;;; The table is built BY NAME (pclxs_vtable_set), not by mirroring the
;;; struct layout in sb-alien.  Mirroring would mean re-checking 50 field
;;; offsets at every ABI bump, and getting one wrong calls the wrong
;;; callback with no diagnostic.  Names fail loudly instead.

(sb-alien:define-alien-routine ("pclxs_vtable_new" %pclxs-vtable-new)
    (sb-alien:* t))
(sb-alien:define-alien-routine ("pclxs_vtable_set" %pclxs-vtable-set)
    sb-alien:int
  (vt (sb-alien:* t)) (name sb-alien:c-string) (fn (sb-alien:* t)))
(sb-alien:define-alien-routine ("pclxs_vtable_check" %pclxs-vtable-check)
    sb-alien:c-string
  (vt (sb-alien:* t)))
(sb-alien:define-alien-routine ("pclxs_init" %pclxs-init)
    (sb-alien:* t)
  (vt (sb-alien:* t)))
(sb-alien:define-alien-routine ("pclxs_abi_version" %pclxs-abi-version)
    sb-alien:unsigned-int)
(sb-alien:define-alien-routine ("pclxs_last_error" %pclxs-last-error)
    sb-alien:c-string
  (ctx (sb-alien:* t)))
(sb-alien:define-alien-routine ("pclxs_boot" %pclxs-boot)
    sb-alien:int
  (ctx (sb-alien:* t)) (path sb-alien:c-string) (sym sb-alien:c-string))

(defparameter *xs-callbacks*
  '(("release"           . xs-release)
    ("dup"               . xs-dup)
    ("new_undef"         . xs-new-undef)
    ("new_iv"            . xs-new-iv)
    ("new_nv"            . xs-new-nv)
    ("new_pvn"           . xs-new-pvn)
    ("get_iv"            . xs-get-iv)
    ("get_nv"            . xs-get-nv)
    ("get_pvn"           . xs-get-pvn)
    ("get_bool"          . xs-get-bool)
    ("is_defined"        . xs-is-defined)
    ("looks_like_number" . xs-looks-like-number)
    ("scalar_flags"      . xs-scalar-flags)
    ("set_iv"            . xs-set-iv)
    ("set_nv"            . xs-set-nv)
    ("set_pvn"           . xs-set-pvn)
    ("set_undef"         . xs-set-undef)
    ("set_sv"            . xs-set-sv)
    ("new_ref"           . xs-new-ref)
    ("ref_type"          . xs-ref-type)
    ("ref_target"        . xs-ref-target)
    ("bless"             . xs-bless)
    ("blessed_class"     . xs-blessed-class)
    ("isa"               . xs-isa)
    ("new_av"            . xs-new-av)
    ("av_count"          . xs-av-count)
    ("av_fetch"          . xs-av-fetch)
    ("av_store"          . xs-av-store)
    ("av_push"           . xs-av-push)
    ("av_pop"            . xs-av-pop)
    ("av_shift"          . xs-av-shift)
    ("av_unshift_n"      . xs-av-unshift-n)
    ("av_clear"          . xs-av-clear)
    ("av_reserve"        . xs-av-reserve)
    ("new_hv"            . xs-new-hv)
    ("hv_fetch"          . xs-hv-fetch)
    ("hv_store"          . xs-hv-store)
    ("hv_exists"         . xs-hv-exists)
    ("hv_delete"         . xs-hv-delete)
    ("hv_clear"          . xs-hv-clear)
    ("hv_count"          . xs-hv-count)
    ("hv_iter_new"       . xs-hv-iter-new)
    ("hv_iter_next"      . xs-hv-iter-next)
    ("get_global"        . xs-get-global)
    ("define_xsub"       . xs-define-xsub)
    ("call"              . xs-call)
    ("eval_string"       . xs-eval-string)
    ("set_errsv"         . xs-set-errsv)
    ("set_errsv_h"       . xs-set-errsv-h)
    ("get_errsv"         . xs-get-errsv)
    ("warn"              . xs-warn))
  "Every entry in the vtable.  pclxs_init refuses an incomplete table and
   names the first missing callback, so a typo here is a message rather
   than a memory fault at address zero later.")

(defun p-xs-init ()
  "Bring up the XS bridge.  Returns the context, or dies with a message
   naming what is missing."
  (or *pclxs-ctx*
      (progn
        (%pclxs-load-library)
        (let ((abi (%pclxs-abi-version))
              (vt  (%pclxs-vtable-new)))
          (declare (ignorable abi))
          (dolist (entry *xs-callbacks*)
            (let ((ok (%pclxs-vtable-set
                       vt (car entry)
                       (sb-alien:alien-sap
                        (sb-alien:alien-callable-function (cdr entry))))))
              (when (zerop ok)
                (p-die (format nil "pclxs: no vtable entry named '~A' (ABI ~D)"
                               (car entry) abi)))))
          ;; Report what is still missing rather than crashing on the first
          ;; null callback, which is the whole point of the name-keyed API.
          (let ((missing (%pclxs-vtable-check vt)))
            (when missing
              (format *error-output*
                      "~&pclxs: vtable incomplete (first missing: ~A) — ~
                       scalar-only bridge~%" missing)))
          (let ((ctx (%pclxs-init vt)))
            (when (sb-alien:null-alien ctx)
              (p-die (format nil "pclxs: pclxs_init failed (ABI ~D)" abi)))
            (setf *pclxs-ctx* ctx))))))

(defun p-xs-boot (so-path boot-symbol)
  "dlopen a shim-built module and run its boot function, which registers
   its XSUBs through define_xsub."
  (let* ((ctx (p-xs-init))
         (rc  (%pclxs-boot ctx so-path boot-symbol)))
    (unless (zerop rc)
      (p-die (format nil "pclxs: boot ~A failed: ~A"
                     boot-symbol (%pclxs-last-error ctx))))
    t))

;;; ============================================================
;;; References, blessing, aggregates
;;;
;;; PCL's model (docs/ir-spec.md §2): a reference is a box whose is-ref flag
;;; is set; an array is an adjustable vector of boxes; a hash is an EQUAL
;;; table keyed by strings.  Blessing lives on the box's class slot, except
;;; for hashes, where p-bless stores it under :__class__ so it survives
;;; unboxing -- which is why every class question goes through p-ref rather
;;; than reading a slot here.

(defun %xs-own-copy (h)
  "Rule O2 in one place: OUR OWN reference to the value behind H, because
   the caller keeps its handle and may release it the moment we return.

   Copies the value AND THE CLASS.  The class matters because PCL records
   a scalar ref's blessing on the wrapper box, so a plain
   `(make-p-box (unbox cell))` -- which is what av_store, av_push and
   hv_store all did -- turns a blessed reference into a fresh box holding
   its referent: the ref-ness and the class both gone.  That is the
   T_PTROBJ object model, and it broke the moment an object crossed to the
   host and came back, which is exactly what `Digest::MD5->new` followed by
   `->add` does.  Every existing case passed because every existing case
   stored a plain scalar.  Pinned now by pclxs's ptrobj_via_host."
  (let ((cell (%xs-deref h)))
    (if (p-box-p cell)
        (make-p-box (unbox cell) (p-box-class cell))
        (make-p-box (if (null cell) *p-undef* cell)))))

(sb-alien:define-alien-callable xs-new-ref sb-alien:long
  ((target sb-alien:long))
  (with-xs-guard () (%xs-intern (p-backslash (%xs-deref target)))))

(sb-alien:define-alien-callable xs-ref-type sb-alien:int
  ((h sb-alien:long))
  (with-xs-guard ()
    ;; The contract's enum: 0 none, 1 scalar, 2 array, 3 hash, 4 code, 5 glob.
    (let ((rt (p-reftype (%xs-deref h))))
      (cond ((or (null rt) (eq rt *p-undef*) (equal rt "")) 0)
            ((equal rt "ARRAY")  2)
            ((equal rt "HASH")   3)
            ((equal rt "CODE")   4)
            ((equal rt "GLOB")   5)
            (t 1)))))

(sb-alien:define-alien-callable xs-ref-target sb-alien:long
  ((h sb-alien:long))
  (with-xs-guard ()
    (let ((v (unbox (%xs-deref h))))
      (%xs-intern v))))

(sb-alien:define-alien-callable xs-bless sb-alien:void
  ((h sb-alien:long) (cls (sb-alien:* sb-alien:char))
   (len sb-alien:unsigned-long))
  (with-xs-guard ()
    (let ((ref (%xs-deref h)))
      (when ref (p-bless ref (%xs-string-in cls len 0))))
    (values)))

(sb-alien:define-alien-callable xs-blessed-class sb-alien:int
  ((h sb-alien:long) (sink (sb-alien:* (sb-alien:function sb-alien:void
                                                          (sb-alien:* t)
                                                          (sb-alien:* sb-alien:char)
                                                          sb-alien:unsigned-long
                                                          sb-alien:int)))
   (ud (sb-alien:* t)))
  (with-xs-guard ()
    ;; p-ref returns the CLASS for a blessed ref and the reftype otherwise,
    ;; so "blessed" is "p-ref disagrees with p-reftype".
    (let* ((obj (%xs-deref h))
           (r   (p-ref obj))
           (rt  (p-reftype obj)))
      (if (and (stringp r) (string/= r "") (not (equal r rt)))
          (progn (%xs-send-string sink ud r) 1)
          0))))

(sb-alien:define-alien-callable xs-isa sb-alien:int
  ((h sb-alien:long) (cls (sb-alien:* sb-alien:char))
   (len sb-alien:unsigned-long))
  (with-xs-guard ()
    ;; sv_derived_from: the @ISA walk PCL already implements for method
    ;; dispatch.  (sv_isa, the exact-class check, is the macro layer's job.)
    (if (p-isa (%xs-deref h) (%xs-string-in cls len 0)) 1 0)))

;;; ---- arrays --------------------------------------------------------------

(sb-alien:define-alien-callable xs-new-av sb-alien:long ()
                                (with-xs-guard ()
                                  (%xs-intern (make-array 0 :adjustable t :fill-pointer 0))))

(defun %xs-av (h)
  (let ((v (unbox (%xs-deref h))))
    (if (and (vectorp v) (not (stringp v))) v nil)))

(sb-alien:define-alien-callable xs-av-count sb-alien:long
  ((h sb-alien:long))
  (with-xs-guard () (let ((a (%xs-av h))) (if a (length a) 0))))

(sb-alien:define-alien-callable xs-av-fetch sb-alien:long
  ((h sb-alien:long) (i sb-alien:long) (lval sb-alien:int))
  (with-xs-guard ()
    (let ((a (%xs-av h)))
      (cond ((null a) 0)
            ((and (>= i 0) (< i (length a)))
             ;; The ELEMENT BOX, not its value: that is what makes an lvalue
             ;; fetch write through (rule O3).
             (let ((elem (aref a i)))
               (unless (p-box-p elem)
                 (setf elem (make-p-box (if (null elem) *p-undef* elem))
                       (aref a i) elem))
               (%xs-intern elem)))
            ((zerop lval) 0)
            (t (loop while (<= (length a) i)
                     do (vector-push-extend (make-p-box *p-undef*) a))
               (%xs-intern (aref a i)))))))

(sb-alien:define-alien-callable xs-av-store sb-alien:void
  ((h sb-alien:long) (i sb-alien:long) (v sb-alien:long))
  (with-xs-guard ()
    (let ((a (%xs-av h)))
      (when (and a (>= i 0))
        (loop while (<= (length a) i)
              do (vector-push-extend (make-p-box *p-undef*) a))
        ;; Rule O2: take our own reference to the VALUE; the caller keeps
        ;; its handle and may release it the moment we return.
        (setf (aref a i) (%xs-own-copy v))))
    (values)))

(sb-alien:define-alien-callable xs-av-push sb-alien:void
  ((h sb-alien:long) (v sb-alien:long))
  (with-xs-guard ()
    (let ((a (%xs-av h)))
      (when a (vector-push-extend (%xs-own-copy v) a)))
    (values)))

(sb-alien:define-alien-callable xs-av-pop sb-alien:long
  ((h sb-alien:long))
  (with-xs-guard ()
    (let ((a (%xs-av h)))
      (if (and a (plusp (length a))) (%xs-intern (vector-pop a)) 0))))

(sb-alien:define-alien-callable xs-av-shift sb-alien:long
  ((h sb-alien:long))
  (with-xs-guard ()
    (let ((a (%xs-av h)))
      (if (and a (plusp (length a)))
          (let ((first (aref a 0)))
            (loop for i from 1 below (length a)
                  do (setf (aref a (1- i)) (aref a i)))
            (decf (fill-pointer a))
            (%xs-intern first))
          0))))

(sb-alien:define-alien-callable xs-av-unshift-n sb-alien:void
  ((h sb-alien:long) (n sb-alien:long))
  (with-xs-guard ()
    (let ((a (%xs-av h)))
      (when (and a (plusp n))
        (let ((old (length a)))
          (dotimes (i n) (vector-push-extend (make-p-box *p-undef*) a))
          (loop for i from (1- old) downto 0
                do (setf (aref a (+ i n)) (aref a i)))
          (dotimes (i n) (setf (aref a i) (make-p-box *p-undef*))))))
    (values)))

(sb-alien:define-alien-callable xs-av-clear sb-alien:void
  ((h sb-alien:long))
  (with-xs-guard ()
    (let ((a (%xs-av h))) (when a (setf (fill-pointer a) 0)))
    (values)))

(sb-alien:define-alien-callable xs-av-reserve sb-alien:void
  ((h sb-alien:long) (n sb-alien:long))
  ;; av_extend is a capacity hint; an adjustable vector sizes itself.
  (declare (ignore h n))
  (values))

;;; ---- hashes --------------------------------------------------------------

(sb-alien:define-alien-callable xs-new-hv sb-alien:long ()
                                (with-xs-guard () (%xs-intern (make-hash-table :test #'equal))))

(defun %xs-hv (h)
  (let ((v (unbox (%xs-deref h))))
    (if (hash-table-p v) v nil)))

(sb-alien:define-alien-callable xs-hv-fetch sb-alien:long
  ((h sb-alien:long) (k (sb-alien:* sb-alien:char))
   (kl sb-alien:unsigned-long) (utf8 sb-alien:int) (lval sb-alien:int))
  (with-xs-guard ()
    (let ((tbl (%xs-hv h)))
      (if (null tbl)
          0
          (let* ((key (%xs-string-in k kl utf8))
                 (cur (gethash key tbl)))
            (cond ((p-box-p cur) (%xs-intern cur))
                  (cur (let ((b (make-p-box cur)))
                         (setf (gethash key tbl) b)
                         (%xs-intern b)))
                  ((zerop lval) 0)
                  (t (let ((b (make-p-box *p-undef*)))
                       (setf (gethash key tbl) b)
                       (%xs-intern b)))))))))

(sb-alien:define-alien-callable xs-hv-store sb-alien:void
  ((h sb-alien:long) (k (sb-alien:* sb-alien:char))
   (kl sb-alien:unsigned-long) (utf8 sb-alien:int) (v sb-alien:long))
  (with-xs-guard ()
    (let ((tbl (%xs-hv h)))
      (when tbl
        (setf (gethash (%xs-string-in k kl utf8) tbl)
              (%xs-own-copy v))))
    (values)))

(sb-alien:define-alien-callable xs-hv-exists sb-alien:int
  ((h sb-alien:long) (k (sb-alien:* sb-alien:char))
   (kl sb-alien:unsigned-long) (utf8 sb-alien:int))
  (with-xs-guard ()
    (let ((tbl (%xs-hv h)))
      (if (and tbl (nth-value 1 (gethash (%xs-string-in k kl utf8) tbl))) 1 0))))

(sb-alien:define-alien-callable xs-hv-delete sb-alien:long
  ((h sb-alien:long) (k (sb-alien:* sb-alien:char))
   (kl sb-alien:unsigned-long) (utf8 sb-alien:int))
  (with-xs-guard ()
    (let ((tbl (%xs-hv h)))
      (if (null tbl)
          0
          (let* ((key (%xs-string-in k kl utf8))
                 (val (gethash key tbl)))
            (remhash key tbl)
            (if val (%xs-intern val) 0))))))

(sb-alien:define-alien-callable xs-hv-clear sb-alien:void
  ((h sb-alien:long))
  (with-xs-guard ()
    (let ((tbl (%xs-hv h))) (when tbl (clrhash tbl)))
    (values)))

(sb-alien:define-alien-callable xs-hv-count sb-alien:long
  ((h sb-alien:long))
  (with-xs-guard ()
    (let ((tbl (%xs-hv h))) (if tbl (hash-table-count tbl) 0))))

;;; A SNAPSHOT iterator, which is why the vtable has one at all: the shim
;;; must not pin a live PCL hash while XS code walks it, and the host must
;;; stay free to rehash.
(sb-alien:define-alien-callable xs-hv-iter-new sb-alien:long
  ((h sb-alien:long))
  (with-xs-guard ()
    (let ((tbl (%xs-hv h))
          (pairs '()))
      (when tbl
        (maphash (lambda (k v) (push (cons k v) pairs)) tbl))
      (%xs-intern (list :xs-iter (nreverse pairs))))))

(sb-alien:define-alien-callable xs-hv-iter-next sb-alien:int
  ((it sb-alien:long) (keysink (sb-alien:* (sb-alien:function sb-alien:void
                                                              (sb-alien:* t)
                                                              (sb-alien:* sb-alien:char)
                                                              sb-alien:unsigned-long
                                                              sb-alien:int)))
   (kud (sb-alien:* t)) (val-out (sb-alien:* sb-alien:long)))
  (with-xs-guard ()
    (let ((state (%xs-deref it)))
      (if (or (not (consp state)) (null (second state)))
          0
          (let* ((pair (pop (second state)))
                 (key  (car pair))
                 (val  (cdr pair)))
            (%xs-send-string keysink kud (to-string key))
            (setf (sb-alien:deref val-out 0)
                  (%xs-intern (if (p-box-p val) val (make-p-box val))))
            1)))))

;;; ---- the symbol table ----------------------------------------------------

(sb-alien:define-alien-callable xs-get-global sb-alien:long
  ((sigil sb-alien:char) (name (sb-alien:* sb-alien:char))
   (len sb-alien:unsigned-long) (create sb-alien:int))
  (with-xs-guard ()
    (declare (ignorable create))
    (let* ((n (%xs-string-in name len 0))
           (c (code-char (ldb (byte 8 0) sigil))))
      ;; Package variables live in CL symbols named for the sigil+name; the
      ;; runtime's own accessor knows the mangling, so ask it rather than
      ;; re-deriving it here.
      (let ((sym (p-xs-global-symbol c n create)))
        (if sym (%xs-intern sym) 0)))))

;;; ============================================================
;;; Helpers the callbacks above share

(defun %xs-send-string (sink ud string)
  "Hand STRING to the shim through its sink: bytes + length + utf8 flag,
   valid only for the duration of the call (ownership rule O5)."
  (multiple-value-bind (octets utf8) (%xs-string-out string)
    (sb-sys:with-pinned-objects (octets)
      (sb-alien:alien-funcall
       sink ud
       (sb-alien:sap-alien (sb-sys:vector-sap octets)
                           (sb-alien:* sb-alien:char))
       (length octets) utf8))))

(defun %xs-errsv ()
  "PCL's $@ is one runtime variable, not a per-package global: the eval
   machinery reads THAT box, so writing anywhere else means eval {} never
   sees what an XSUB croaked with.  (It did not, until this existed.)"
  pcl::$@)

(defun p-xs-global-symbol (sigil name create)
  "The box/vector/table behind a package variable, e.g. ('$' \"Foo::bar\").
   PCL names package variables with the sigil in the symbol -- $foo is the
   symbol |$foo| in the package -- so this is a find-symbol, not a new
   naming scheme."
  (let* ((pos  (search "::" name :from-end t))
         (pkg  (if pos (subseq name 0 pos) "main"))
         (base (if pos (subseq name (+ pos 2)) name))
         (package (%pcl-find-package pkg))
         (sym-name (concatenate 'string (string sigil) base)))
    (when package
      (let ((sym (find-symbol sym-name package)))
        (cond ((and sym (boundp sym)) (symbol-value sym))
              ((zerop create) nil)
              (t (let ((s (intern sym-name package)))
                   (setf (symbol-value s)
                         (case sigil
                           (#\@ (make-array 0 :adjustable t :fill-pointer 0))
                           (#\% (make-hash-table :test #'equal))
                           (t   (make-p-box *p-undef*))))
                   (symbol-value s))))))))

;;; ============================================================
;;; Errors and warnings
;;;
;;; $@ is an ordinary PCL global, so "set $@" is a write to it -- no new
;;; error channel, and `eval {}` around an XS call sees exactly what it
;;; would see around a Perl call.

(sb-alien:define-alien-callable xs-set-errsv sb-alien:void
  ((bytes (sb-alien:* sb-alien:char)) (len sb-alien:unsigned-long)
   (utf8 sb-alien:int))
  (with-xs-guard ()
    (let ((b (%xs-errsv)))
      (when b (box-set b (%xs-string-in bytes len utf8))))
    (values)))

(sb-alien:define-alien-callable xs-set-errsv-h sb-alien:void
  ((h sb-alien:long))
  (with-xs-guard ()
    ;; An exception OBJECT, not a message: store it unchanged so that
    ;; `die $obj` from XS behaves like `die $obj` from Perl.
    (let ((b (%xs-errsv)))
      (when b (box-set b (unbox (%xs-deref h)))))
    (values)))

(sb-alien:define-alien-callable xs-get-errsv sb-alien:long ()
                                (with-xs-guard ()
                                  (let ((b (%xs-errsv)))
                                    (%xs-intern b))))

(sb-alien:define-alien-callable xs-warn sb-alien:void
  ((bytes (sb-alien:* sb-alien:char)) (len sb-alien:unsigned-long)
   (utf8 sb-alien:int))
  (with-xs-guard ()
    ;; p-warn already honours $SIG{__WARN__}, which is the whole reason to
    ;; route through it rather than writing to *error-output*.
    (p-warn (%xs-string-in bytes len utf8))
    (values)))

;;; ============================================================
;;; Sub registration and the trampoline
;;;
;;; define_xsub says "the Perl name FOO::bar is this C function".  The host
;;; side of that is: install a CL function under the name PCL would have
;;; given a `sub bar` in package FOO, whose body re-enters the shim.

(sb-alien:define-alien-routine ("pclxs_invoke_xsub" %pclxs-invoke-xsub)
    sb-alien:int
  (ctx (sb-alien:* t)) (fnptr (sb-alien:* t))
  (args (sb-alien:* sb-alien:long)) (nargs sb-alien:unsigned-long)
  (gimme sb-alien:int)
  (push-result (sb-alien:* t)) (ud (sb-alien:* t)))

(defvar *xs-results* nil
  "Where the current invoke's results accumulate, newest first.  Bound per
   call, so nesting (an XSUB calling a Perl sub that calls another XSUB)
   works without a stack of its own.")

(sb-alien:define-alien-callable xs-collect-result sb-alien:void
  ((ud (sb-alien:* t)) (h sb-alien:long))
  (declare (ignore ud))
  (with-xs-guard ()
    ;; Rule O1: this handle is ours now, so read the value and drop it.
    ;;
    ;; UNBOX ONLY A PLAIN SCALAR.  A result that is a REFERENCE has to come
    ;; back as the reference itself: unboxing one yields its referent, which
    ;; throws away both the ref-ness and -- since PCL records a scalar ref's
    ;; blessing on the wrapper box -- the class.  That is how
    ;; `Digest::MD5->new` returned undef while `md5_hex` worked: the
    ;; functional interface returns a string, the OO one returns a blessed
    ;; scalar ref built by sv_setref_pv, and only the second went through
    ;; this line.  Nothing caught it for a phase because no test called an
    ;; XSUB that returns a reference.
    (let* ((cell (%xs-deref h))
           (rt   (p-reftype cell)))
      (push (if (or (null rt) (eq rt *p-undef*) (equal rt ""))
                (unbox cell)
                cell)
            *xs-results*))
    (%xs-release h)
    (values)))

(defun p-xs-invoke (fnptr args)
  "Call the XSUB at FNPTR with ARGS (already flattened Perl values).
   Returns a list of results.  Dies -- as an ordinary PCL die, catchable by
   eval {} -- if the XSUB croaked."
  (let* ((ctx (p-xs-init))
         (n   (length args))
         (gimme (cond ((eq *wantarray* :void) 0)
                      ((null *wantarray*) 1)
                      (t 2)))
         (*xs-results* '())
         (handles '()))
    (unwind-protect
         (sb-alien:with-alien ((argv (sb-alien:array sb-alien:long 64)))
           (when (> n 64)
             (p-die "pclxs: more than 64 arguments to an XSUB"))
           (loop for a in args
                 for i from 0
                 do (let ((h (%xs-intern (if (p-box-p a) a (make-p-box a)))))
                      (push h handles)
                      (setf (sb-alien:deref argv i) h)))
           (let ((rc (%pclxs-invoke-xsub
                      ctx fnptr
                      (sb-alien:cast argv (sb-alien:* sb-alien:long))
                      n gimme
                      (sb-alien:alien-sap
                       (sb-alien:alien-callable-function 'xs-collect-result))
                      (sb-sys:int-sap 0))))
             (unless (zerop rc)
               ;; PS_DIED: $@ already holds what the XSUB croaked with, and
               ;; re-dying with it is what makes eval {} behave (rule O4 --
               ;; the C side never unwound through us to get here).
               (p-die (to-string (unbox (%xs-errsv)))))
             (nreverse *xs-results*)))
      (dolist (h handles) (%xs-release h)))))

(sb-alien:define-alien-callable xs-define-xsub sb-alien:void
  ((name (sb-alien:* sb-alien:char)) (len sb-alien:unsigned-long)
   (fnptr (sb-alien:* t)) (filename sb-alien:c-string))
  (declare (ignore filename))
  (with-xs-guard ()
    (let* ((full (%xs-string-in name len 0))
           (pos  (search "::" full :from-end t))
           (pkg  (if pos (subseq full 0 pos) "main"))
           (base (if pos (subseq full (+ pos 2)) full))
           (package (%xs-ensure-package pkg))
           ;; %pcl-cl-sub-name, NOT a hand-rolled "PL-" + upcase: PCL reads
           ;; with :invert, so the mangling has to be the runtime's own or
           ;; the symbol we define is not the one a caller looks up.
           (sym (intern (%pcl-cl-sub-name base) package))
           (ptr (sb-alien:sap-alien (sb-alien:alien-sap fnptr)
                                    (sb-alien:* t))))
      ;; An XSUB has to look like any other PCL sub to its callers: same
      ;; name, same @_ flattening, same wantarray.  So the installed
      ;; function is an ordinary funcallable that re-enters the shim.
      (setf (fdefinition sym)
            (lambda (&rest args)
              (let ((results (p-xs-invoke ptr args)))
                (if (eq *wantarray* t)
                    (let ((v (make-array (length results)
                                         :adjustable t :fill-pointer t)))
                      (loop for r in results
                            for i from 0
                            do (setf (aref v i) (make-p-box r)))
                      v)
                    (car (last results))))))
      (setf (gethash sym *p-declared-subs*) :defined))
    (values)))

(defun %xs-ensure-package (pkg-str)
  "The CL package for a Perl package name, creating it if the module is the
   first thing to mention it.  Naming goes through the runtime's own
   perl-pkg-to-cl-pkg-name, because PCL reads with :invert and a
   hand-rolled upcase would create a package nobody else can find."
  (or (%pcl-find-package pkg-str)
      (make-package (perl-pkg-to-cl-pkg-name pkg-str) :use '(:cl :pcl))))

;;; ============================================================
;;; Calling back into Perl (the other direction)

(sb-alien:define-alien-callable xs-call sb-alien:int
  ((code sb-alien:long) (name (sb-alien:* sb-alien:char))
   (namelen sb-alien:unsigned-long) (is-method sb-alien:int)
   (args (sb-alien:* sb-alien:long)) (nargs sb-alien:unsigned-long)
   (gimme sb-alien:int) (trap sb-alien:int)
   (push-result (sb-alien:* t)) (ud (sb-alien:* t)))
  ;; Rule O4 in its most important instance: a PCL die must NOT unwind
  ;; through this frame into C.  Catch it, publish $@, return PS_DIED, and
  ;; let the shim decide whether to re-croak (no G_EVAL) or hand ERRSV to
  ;; the XS code (G_EVAL).  The two mechanisms never overlap.
  (with-xs-guard (:on-error 1)
    (let* ((fn (cond ((not (zerop code))
                      (let ((v (unbox (%xs-deref code))))
                        (if (functionp v) v nil)))
                     (t (let* ((full (%xs-string-in name namelen 0))
                               (pos (search "::" full :from-end t))
                               (pkg (if pos (subseq full 0 pos) "main"))
                               (base (if pos (subseq full (+ pos 2)) full))
                               (p (%pcl-find-package pkg))
                               (sym (and p (find-symbol (%pcl-cl-sub-name base) p))))
                          (and sym (fboundp sym) (symbol-function sym))))))
           (argv (loop for i from 0 below nargs
                       collect (unbox (%xs-deref (sb-alien:deref args i))))))
      (declare (ignorable is-method))
      (if (null fn)
          (progn
            (let ((b (%xs-errsv)))
              (when b (box-set b (format nil "Undefined subroutine &~A called"
                                         (%xs-string-in name namelen 0)))))
            1)                                  ; PS_DIED
          (handler-case
              (let* ((*wantarray* (case gimme (0 :void) (1 nil) (t t)))
                     (result (apply fn argv))
                     (values-list
                      (if (and (eq *wantarray* t) (vectorp result)
                               (not (stringp result)))
                          (coerce result 'list)
                          (list result))))
                (dolist (v values-list)
                  (sb-alien:alien-funcall
                   (sb-alien:sap-alien (sb-alien:alien-sap push-result)
                                       (sb-alien:function sb-alien:void
                                                          (sb-alien:* t)
                                                          sb-alien:long))
                   ud (%xs-intern (if (p-box-p v) v (make-p-box v)))))
                0)                              ; PS_OK
            (error (e)
              (let ((b (%xs-errsv)))
                (when b (box-set b (princ-to-string e))))
              1))))))                           ; PS_DIED

(sb-alien:define-alien-callable xs-eval-string sb-alien:int
  ((code (sb-alien:* sb-alien:char)) (len sb-alien:unsigned-long)
   (gimme sb-alien:int) (push-result (sb-alien:* t)) (ud (sb-alien:* t)))
  (declare (ignore gimme push-result ud))
  (with-xs-guard (:on-error 1)
    ;; String eval IS supported by PCL, but wiring it here needs the
    ;; eval-mode entry rather than a plain funcall; until then this refuses
    ;; the way any other unimplemented host entry must -- as a Perl-level
    ;; error, never a crash.
    (let ((b (%xs-errsv)))
      (when b (box-set b (format nil "pclxs: eval_string from XS is not wired up yet (~A)"
                                 (%xs-string-in code (min len 60) 0)))))
    1))

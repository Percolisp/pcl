;;; pcl: pipeline=v2 gen=v2-147
(in-package :pcl)
(setf pcl::*pcl-pl2cl-path* #P"/home/bernt/pcl/pl2cl")
;; Initialize @INC from Perl
(setf pcl::@INC (make-array 0 :adjustable t :fill-pointer 0))
(vector-push-extend "/home/bernt/pcl/lib" pcl::@INC)
(vector-push-extend "." pcl::@INC)
(vector-push-extend "/home/bernt/pcl/lib" pcl::@INC)
(vector-push-extend "/home/bernt/pcl" pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/x86_64-linux" pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3" pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/5.40.3/x86_64-linux" pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/5.40.3" pcl::@INC)
(setf pcl::*p-core-inc-dirs* (list "/home/bernt/pcl/lib" "/home/bernt/pcl" "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/x86_64-linux" "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3" "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/5.40.3/x86_64-linux" "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/5.40.3"))
;; Switch to main package (Perl's default for code without 'package' statement)
(p-defpackage :main)
(in-package :main)

(pcl:p-defpackage :warnings)

(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))



;;; package warnings
(p-defpackage :warnings)
(in-package :warnings)
(defclass plc-warnings () ())
(p-register-pkg-name :warnings "warnings")

(p-declare-sub pl-import)
(p-declare-sub pl-unimport)
(p-declare-sub pl-enabled)
(p-declare-sub pl-fatal_enabled)
(p-declare-sub pl-enabled_at_level)
(p-declare-sub pl-fatal_enabled_at_level)
(p-declare-sub pl-register_categories)
(p-declare-sub pl-warn)
(p-declare-sub pl-warnif)
(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))

;; Forward declarations for undeclared package globals
(p-defcell $message (make-p-box nil))
(p-defcell warnings::$VERSION (make-p-box nil))

(p-defcell $VERSION (make-p-box nil))

(p-sub pl-import (&rest %_args) (p-args-body (block nil)))

(p-sub pl-unimport (&rest %_args) (p-args-body (block nil)))

(p-sub pl-enabled (&rest %_args) (p-args-body (block nil (p-return 1))))

(p-sub pl-fatal_enabled (&rest %_args) (p-args-body (block nil (p-return 0))))

(p-sub pl-enabled_at_level (&rest %_args) (p-args-body (block nil (p-return 1))))

(p-sub pl-fatal_enabled_at_level (&rest %_args) (p-args-body (block nil (p-return 0))))

(p-sub pl-register_categories (&rest %_args) (p-args-body (block nil)))

(p-sub pl-warn
  (&rest %_args)
  (p-args-body
    (block nil
      (let ((*wantarray* :void))
        (let (($message (make-p-box nil)))
          (p-my-= $message (p-pop @_))
          (let ((*wantarray* *pcl-caller-wantarray*))
            (p-warn :loc "lib/warnings.pm line 33" $message)))))))

(p-sub pl-warnif
  (&rest %_args)
  (p-args-body
    (block nil
      (let ((*wantarray* :void))
        (let (($message (make-p-box nil)))
          (p-my-= $message (p-pop @_))
          (let ((*wantarray* *pcl-caller-wantarray*))
            (p-warn :loc "lib/warnings.pm line 39" $message)))))))

(p-set-current-package :warnings "warnings")

(p-run-compile-phase-blocks)

(p-scalar-= warnings::$VERSION "1.70")

1


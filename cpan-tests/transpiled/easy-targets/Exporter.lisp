(in-package :pcl)
(setf pcl::*pcl-pl2cl-path* #P"/home/bernt/pcl/pl2cl")
;; Initialize @INC from Perl
(setf pcl::@INC (make-array 0 :adjustable t :fill-pointer 0))
(vector-push-extend "/home/bernt/pcl/cpan-tests/modules/easy-targets" pcl::@INC)
(vector-push-extend "." pcl::@INC)
(vector-push-extend "." pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/x86_64-linux" pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3" pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/5.40.3/x86_64-linux" pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/5.40.3" pcl::@INC)

;;; package Exporter
(defpackage :Exporter
  (:use :cl :pcl))
(in-package :Exporter)
;; CLOS class for MRO
(defclass exporter () ())

;; use strict (pragma)

;; no strict 'refs' (no-op)

;; our $Debug = 0
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $Debug (make-pl-box nil)))
(setf (pl-box-value $Debug) 0)

;; our $ExportLevel = 0
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $ExportLevel (make-pl-box nil)))
(setf (pl-box-value $ExportLevel) 0)

;; our $Verbose ||= 0
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $Verbose (make-pl-box nil)))

;; our $VERSION = '5.78'
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $VERSION (make-pl-box nil)))
(setf (pl-box-value $VERSION) "5.78")

;; our %Cache
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar %Cache (make-hash-table :test 'equal)))

;; sub as_heavy { ... }
(pl-sub pl-as_heavy (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($c (make-pl-box nil)))
        ;; require Exporter::Heavy
        (eval-when (:compile-toplevel :load-toplevel :execute)
          (pl-require "Exporter::Heavy"))
        
        ;; my $c = (caller(1))[3]
                (pcl:pl-setf $c (pl-aref-deref (pl-caller 1) 3))
        
        ;; $c =~ s/.*:://
                (pcl:pl-=~ $c (pl-subst ".*::" ""))
        
        ;; \&{"Exporter::Heavy::heavy_$c"}
                (pl-backslash (pl-bit-and (pl-string-concat "Exporter::Heavy::heavy_" $c)))
        
      )
    )
  )
)

;; sub export { ... }
(pl-sub pl-export (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; goto &{as_heavy()}
            (pl-goto (pl-bit-and (pl-as_heavy)))
      
    )
  )
)

;; sub import { ... }
(pl-sub pl-import (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($pkg (make-pl-box nil)) ($callpkg (make-pl-box nil)) ($exports (make-pl-box nil)) ($fail (make-pl-box nil)) ($export_cache (make-pl-box nil)) ($args (make-pl-box nil)) ($heavy (make-pl-box nil)))
        ;; my $pkg = shift
                (pcl:pl-setf $pkg (pl-shift @_))
        
        ;; my $callpkg = caller($ExportLevel)
                (pcl:pl-setf $callpkg (pl-caller $ExportLevel))
        
        ;; if ($pkg eq "Exporter" and @_ and $_[0] eq "import") {     *{$callpkg."::import"} = \&import;     return;   }
        ;; if ($pkg eq "Exporter" and @_ and $_[0] eq "import")
        (pl-if         (pcl:pl-and (pcl:pl-and (pcl:pl-str-eq $pkg "Exporter") @_) (pcl:pl-str-eq (pl-aref @_ 0) "import"))
          (progn
            ;; *{$callpkg."::import"} = \&import
                        (pcl:pl-setf (pl-* (pcl:pl-. $callpkg "::import")) #'pl-import)
            
            ;; return
                        (pl-return)
            
          )
          nil
        )
        
        ;; my $exports = \@{"$pkg\::EXPORT"}
                (pcl:pl-setf $exports (pl-backslash (pl-cast-@ (pl-string-concat $pkg "\\::EXPORT"))))
        
        ;; my $fail = ${$pkg . '::'}{EXPORT_FAIL} && \@{"$pkg\::EXPORT_FAIL"}
                (pcl:pl-setf $fail (pcl:pl-&& (pl-cast-$ (pl-gethash (pcl:pl-. $pkg "::") "EXPORT_FAIL")) (pl-backslash (pl-cast-@ (pl-string-concat $pkg "\\::EXPORT_FAIL")))))
        
        ;; return export $pkg, $callpkg, @_     if $Verbose or $Debug or $fail && @$fail > 1
        (pl-if         (pcl:pl-or (pcl:pl-or $Verbose $Debug) (pcl:pl-&& $fail (pcl:pl-> (pl-cast-@ $fail) 1)))         (pl-return (pl-export $pkg $callpkg @_)))
        
        ;; my $export_cache = ($Cache{$pkg} ||= {})
                (pcl:pl-setf $export_cache (pcl:pl-or-assign (pl-gethash %Cache $pkg) (pl-hash )))
        
        ;; my $args = @_ or @_ = @$exports
                (pcl:pl-or (pcl:pl-setf $args @_) (pcl:pl-setf @_ (pl-cast-@ $exports)))
        
        ;; if ($args and not %$export_cache) {     s/^&//, $export_cache->{$_} = 1       foreach (@$exports, @{"$pkg\::EXPORT_OK"});   }
        ;; if ($args and not %$export_cache)
        (pl-if         (pcl:pl-and $args (pl-not (pl-cast-% $export_cache)))
          (progn
            ;; s/^&//, $export_cache->{$_} = 1       foreach (@$exports, @{"$pkg\::EXPORT_OK"})
            (pl-foreach             (progn (pl-cast-@ $exports) (pl-cast-@ (pl-string-concat $pkg "\\::EXPORT_OK")))             (progn (pcl:pl-=~ $_ (pl-subst "^&" "")) (pcl:pl-setf (pl-gethash-deref $export_cache $_) 1)))
            
          )
          nil
        )
        
        ;; my $heavy (bare declaration)
        
        ;; if ($args or $fail) {     ($heavy = (/\W/ or $args and not exists $export_cache->{$_}                or $fail and @$fail and $_ eq $fail->[0])) and last                  foreach (@_);   } else {     ($heavy = /\W/) and last       foreach (@_);   }
        ;; if ($args or $fail)
        (pl-if         (pcl:pl-or $args $fail)
          (progn
            ;;      ($heavy = (/\W/ or $args and not exists $export_cache->{$_}                or $fail and @$fail and $_ eq $fail->[0])) and last                  foreach (@_)
            (pl-foreach             @_             (pcl:pl-and (pcl:pl-setf $heavy (pcl:pl-and (pcl:pl-and (pcl:pl-or (pcl:pl-and (pcl:pl-or (pcl:pl-=~ $_ (pl-regex "/\\W/")) $args) (pl-not (pl-gethash-deref (pl-exists $export_cache) $_))) $fail) (pl-cast-@ $fail)) (pcl:pl-str-eq $_ (pl-aref-deref $fail 0)))) (pl-last)))
            
          )
          ;; else
          (progn
            ;;      ($heavy = /\W/) and last       foreach (@_)
            (pl-foreach             @_             (pcl:pl-and (pcl:pl-setf $heavy (pcl:pl-=~ $_ (pl-regex "/\\W/"))) (pl-last)))
            
          )
        )
        
        ;; return export $pkg, $callpkg, ($args ? @_ : ()) if $heavy
        (pl-if         $heavy         (pl-return (pl-export $pkg $callpkg (pl-if $args @_ (progn )))))
        
        ;; local $SIG{__WARN__} =  	sub {require Carp; &Carp::carp} if not $SIG{__WARN__}
        (defun --anon-block-2-- ()
          ;; require Carp
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (pl-require "Carp"))
          
          ;; &Carp::carp
                    &Carp::carp
          
        )
        
        (let (($SIG (make-pl-box ;; PARSE ERROR: Fell through. Missing case: [)))
          
          ;; *{"$callpkg\::$_"} = \&{"$pkg\::$_"} foreach @_
          (pl-foreach           @_           (pcl:pl-setf (pl-* (pl-string-concat $callpkg "\\::" $_)) (pl-backslash (pl-bit-and (pl-string-concat $pkg "\\::" $_)))))
          
        )  ;; end local
      )
    )
  )
)

;; # Default methods
;; sub export_fail { ... }
(pl-sub pl-export_fail (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; @_
                @_
        
      )
    )
  )
)

;; # Unfortunately, caller(1)[3] "does not work" if the caller is aliased as
;; # *name = \&foo.  Thus the need to create a lot of identical subroutines
;; # Otherwise we could have aliased them to export().
;; sub export_to_level { ... }
(pl-sub pl-export_to_level (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; goto &{as_heavy()}
            (pl-goto (pl-bit-and (pl-as_heavy)))
      
    )
  )
)

;; sub export_tags { ... }
(pl-sub pl-export_tags (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; goto &{as_heavy()}
            (pl-goto (pl-bit-and (pl-as_heavy)))
      
    )
  )
)

;; sub export_ok_tags { ... }
(pl-sub pl-export_ok_tags (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; goto &{as_heavy()}
            (pl-goto (pl-bit-and (pl-as_heavy)))
      
    )
  )
)

;; sub require_version { ... }
(pl-sub pl-require_version (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; goto &{as_heavy()}
            (pl-goto (pl-bit-and (pl-as_heavy)))
      
    )
  )
)

;; 1
1

;; __END__

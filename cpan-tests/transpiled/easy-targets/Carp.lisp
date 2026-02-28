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

;; Pre-declare package for dynamic loading
(defpackage :CORE (:use :cl :pcl))

;; Pre-declare package for dynamic loading
(defpackage :warnings (:use :cl :pcl))

;;; package Carp
(defpackage :Carp
  (:use :cl :pcl))
(in-package :Carp)
;; CLOS class for MRO
(defclass carp () ())

;; { ... }
(progn
  ;; use 5.006 (pragma)
  
)

;; use strict (pragma)

;; use warnings (pragma)

;; BEGIN {
(eval-when (:compile-toplevel :execute)
  ;;     # Very old versions of warnings.pm load Carp.  This can go wrong due
  ;;     # to the circular dependency.  If warnings is invoked before Carp,
  ;;     # then warnings starts by loading Carp, then Carp (above) tries to
  ;;     # invoke warnings, and gets nothing because warnings is in the process
  ;;     # of loading and hasn't defined its import method yet.  If we were
  ;;     # only turning on warnings ("use warnings" above) this wouldn't be too
  ;;     # bad, because Carp would just gets the state of the -w switch and so
  ;;     # might not get some warnings that it wanted.  The real problem is
  ;;     # that we then want to turn off Unicode warnings, but "no warnings
  ;;     # 'utf8'" won't be effective if we're in this circular-dependency
  ;;     # situation.  So, if warnings.pm is an affected version, we turn
  ;;     # off all warnings ourselves by directly setting ${^WARNING_BITS}.
  ;;     # On unaffected versions, we turn off just Unicode warnings, via
  ;;     # the proper API.
  ;; if(!defined($warnings::VERSION) || eval($warnings::VERSION) < 1.06) { 	${^WARNING_BITS} = "";     } else { 	"warnings"->unimport("utf8");     }
  ;; if (!defined($warnings::VERSION) || eval($warnings::VERSION) < 1.06)
  (pl-if   (pcl:pl-|| (pl-! (pl-defined warnings::$VERSION)) (pcl:pl-< (pl-eval warnings::$VERSION) 1.06))
    (progn
      ;; ${^WARNING_BITS} = ""
            (pcl:pl-setf ${^WARNING_BITS} "")
      
    )
    ;; else
    (progn
      ;; "warnings"->unimport("utf8")
            (pl-method-call "warnings" 'unimport "utf8")
      
    )
  )
  
)

;; sub _fetch_sub { ... }
(pl-sub pl-_fetch_sub (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($pack (make-pl-box nil)) ($sub (make-pl-box nil)))
        ;; my($pack, $sub) = @_
                (pcl:pl-setf (vector $pack $sub) @_)
        
        ;; $pack .= '::'
                (pcl:pl-.= $pack "::")
        
        ;; return unless exists($::{$pack})
        (pl-unless         (pl-exists (pl-gethash %:: $pack))         (pl-return))
        
        ;; for ($::{$pack}) { 	return unless ref \$_ eq 'GLOB' && *$_{HASH} && exists $$_{$sub}; 	for ($$_{$sub}) { 	    return ref \$_ eq 'GLOB' ? *$_{CODE} : undef 	}     }
        (pl-foreach ($_         (pl-gethash %:: $pack))
          ;; return unless ref \$_ eq 'GLOB' && *$_{HASH} && exists $$_{$sub}
          (pl-unless           (pcl:pl-&& (pcl:pl-&& (pcl:pl-str-eq (pl-ref (pl-backslash $_)) "GLOB") (pl-* (pl-gethash %_ "HASH"))) (pl-gethash (pl-exists (pl-cast-$ $_)) $sub))           (pl-return))
          
          ;; for ($$_{$sub}) { 	    return ref \$_ eq 'GLOB' ? *$_{CODE} : undef 	}
          (pl-foreach ($_           (pl-cast-$ (pl-gethash %_ $sub)))
            ;; return ref \$_ eq 'GLOB' ? *$_{CODE} : undef
                        (pl-return (pl-if (pcl:pl-str-eq (pl-ref (pl-backslash $_)) "GLOB") (pl-* (pl-gethash %_ "CODE")) (pl-undef)))
            
          )
          
        )
        
      )
    )
  )
)

;; # UTF8_REGEXP_PROBLEM is a compile-time constant indicating whether Carp
;; # must avoid applying a regular expression to an upgraded (is_utf8)
;; # string.  There are multiple problems, on different Perl versions,
;; # that require this to be avoided.  All versions prior to 5.13.8 will
;; # load utf8_heavy.pl for the swash system, even if the regexp doesn't
;; # use character classes.  Perl 5.6 and Perls [5.11.2, 5.13.11) exhibit
;; # specific problems when Carp is being invoked in the aftermath of a
;; # syntax error.
;; BEGIN {
(eval-when (:compile-toplevel :execute)
  ;; if("$]" < 5.013011) { 	*UTF8_REGEXP_PROBLEM = sub () { 1 };     } else { 	*UTF8_REGEXP_PROBLEM = sub () { 0 };     }
  ;; if ("$]" < 5.013011)
  (pl-if   (pcl:pl-< $] 5.013011)
    (progn
      ;; *UTF8_REGEXP_PROBLEM = sub () { 1 }
      ;; PARSE ERROR: Fell through. Missing case: [
      
    )
    ;; else
    (progn
      ;; *UTF8_REGEXP_PROBLEM = sub () { 0 }
      ;; PARSE ERROR: Fell through. Missing case: [
      
    )
  )
  
)

;; # is_utf8() is essentially the utf8::is_utf8() function, which indicates
;; # whether a string is represented in the upgraded form (using UTF-8
;; # internally).  As utf8::is_utf8() is only available from Perl 5.8
;; # onwards, extra effort is required here to make it work on Perl 5.6.
;; BEGIN {
(eval-when (:compile-toplevel :execute)
  ;; if(defined(my $sub = _fetch_sub utf8 => 'is_utf8')) { 	*is_utf8 = $sub;     } else { 	# black magic for perl 5.6 	*is_utf8 = sub { unpack("C", "\xaa".$_[0]) != 170 };     }
  (let (($sub (make-pl-box nil)))
    ;; if (defined(my $sub = _fetch_sub utf8 , 'is_utf8'))
    (pl-if     (pl-defined (pcl:pl-setf $sub (pl-_fetch_sub "utf8" "is_utf8")))
      (progn
        ;; *is_utf8 = $sub
                (pcl:pl-setf *is_utf8 $sub)
        
      )
      ;; else
      (progn
        (defun --anon-block-16-- ()
          ;; unpack("C", "\xaa".$_[0]) != 170
                    (pcl:pl-!= (pl-unpack "C" (pcl:pl-. "\\xaa" (pl-aref @_ 0))) 170)
          
        )
        
        ;; *is_utf8 = sub { unpack("C", "\xaa".$_[0]) != 170 }
                (setf (symbol-function 'pl-is_utf8) #'--anon-block-16--)
        
      )
    )
  )
  
)

;; # The downgrade() function defined here is to be used for attempts to
;; # downgrade where it is acceptable to fail.  It must be called with a
;; # second argument that is a true value.
;; BEGIN {
(eval-when (:compile-toplevel :execute)
  ;; if(defined(my $sub = _fetch_sub utf8 => 'downgrade')) { 	*downgrade = \&{"utf8::downgrade"};     } else { 	*downgrade = sub { 	    my $r = ""; 	    my $l = length($_[0]); 	    for(my $i = 0; $i != $l; $i++) { 		my $o = ord(substr($_[0], $i, 1)); 		return if $o > 255; 		$r .= chr($o); 	    } 	    $_[0] = $r; 	};     }
  (let (($sub (make-pl-box nil)))
    ;; if (defined(my $sub = _fetch_sub utf8 , 'downgrade'))
    (pl-if     (pl-defined (pcl:pl-setf $sub (pl-_fetch_sub "utf8" "downgrade")))
      (progn
        ;; *downgrade = \&{"utf8::downgrade"}
                (pcl:pl-setf *downgrade (pl-backslash (pl-bit-and "utf8::downgrade")))
        
      )
      ;; else
      (progn
        (defun --anon-block-17-- ()
          ;; my $r = ""
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (defvar $r (make-pl-box nil)))
          (box-set $r           "")
          
          ;; my $l = length($_[0])
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (defvar $l (make-pl-box nil)))
          (box-set $l           (pl-length (pl-aref @_ 0)))
          
          ;; for(my $i = 0; $i != $l; $i++) { 		my $o = ord(substr($_[0], $i, 1)); 		return if $o > 255; 		$r .= chr($o); 	    }
          (let (($i (make-pl-box nil)))
            (pl-for (          (pcl:pl-setf $i 0))
                    (          (pcl:pl-!= $i $l))
                    (          (pl-post++ $i))
              ;; my $o = ord(substr($_[0], $i, 1))
              (eval-when (:compile-toplevel :load-toplevel :execute)
                (defvar $o (make-pl-box nil)))
              (box-set $o               (pl-ord (pl-substr (pl-aref @_ 0) $i 1)))
              
              ;; return if $o > 255
              (pl-if               (pcl:pl-> $o 255)               (pl-return))
              
              ;; $r .= chr($o)
                            (pcl:pl-.= $r (pl-chr $o))
              
            )
          )
          
          ;; $_[0] = $r
                    (pcl:pl-setf (pl-aref @_ 0) $r)
          
        )
        
        ;; *downgrade = sub { 	    my $r = ""; 	    my $l = length($_[0]); 	    for(my $i = 0; $i != $l; $i++) { 		my $o = ord(substr($_[0], $i, 1)); 		return if $o > 255; 		$r .= chr($o); 	    } 	    $_[0] = $r; 	}
                (setf (symbol-function 'pl-downgrade) #'--anon-block-17--)
        
      )
    )
  )
  
)

;; # is_safe_printable_codepoint() indicates whether a character, specified
;; # by integer codepoint, is OK to output literally in a trace.  Generally
;; # this is if it is a printable character in the ancestral character set
;; # (ASCII or EBCDIC).  This is used on some Perls in situations where a
;; # regexp can't be used.
;; BEGIN {
(eval-when (:compile-toplevel :execute)
  ;; *is_safe_printable_codepoint = 	"$]" >= 5.007_003 ? 	    eval(q(sub ($) { 		my $u = utf8::native_to_unicode($_[0]); 		$u >= 0x20 && $u <= 0x7e; 	    })) 	: ord("A") == 65 ? 	    sub ($) { $_[0] >= 0x20 && $_[0] <= 0x7e } 	: 	    sub ($) { 		# Early EBCDIC 		# 3 EBCDIC code pages supported then;  all controls but one 		# are the code points below SPACE.  The other one is 0x5F on 		# POSIX-BC; FF on the other two. 		# FIXME: there are plenty of unprintable codepoints other 		# than those that this code and the comment above identifies 		# as "controls". 		$_[0] >= ord(" ") && $_[0] <= 0xff && 		    $_[0] != (ord ("^") == 106 ? 0x5f : 0xff); 	    } 	
  ;; PARSE ERROR: Handle single node of unknown type. Dump:
  
)

;; sub _univ_mod_loaded { ... }
(pl-sub pl-_univ_mod_loaded (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; return 0 unless exists($::{"UNIVERSAL::"})
      (pl-unless       (pl-exists (pl-gethash %:: "UNIVERSAL::"))       (pl-return 0))
      
      ;; for ($::{"UNIVERSAL::"}) { 	return 0 unless ref \$_ eq "GLOB" && *$_{HASH} && exists $$_{"$_[0]::"}; 	for ($$_{"$_[0]::"}) { 	    return 0 unless ref \$_ eq "GLOB" && *$_{HASH} && exists $$_{"VERSION"}; 	    for ($$_{"VERSION"}) { 		return 0 unless ref \$_ eq "GLOB"; 		return ${*$_{SCALAR}}; 	    } 	}     }
      (pl-foreach ($_       (pl-gethash %:: "UNIVERSAL::"))
        ;; return 0 unless ref \$_ eq "GLOB" && *$_{HASH} && exists $$_{"$_[0]::"}
        (pl-unless         (pcl:pl-&& (pcl:pl-&& (pcl:pl-str-eq (pl-ref (pl-backslash $_)) "GLOB") (pl-* (pl-gethash %_ "HASH"))) (pl-gethash (pl-exists (pl-cast-$ $_)) (pl-string-concat (pl-aref @_ 0) "::")))         (pl-return 0))
        
        ;; for ($$_{"$_[0]::"}) { 	    return 0 unless ref \$_ eq "GLOB" && *$_{HASH} && exists $$_{"VERSION"}; 	    for ($$_{"VERSION"}) { 		return 0 unless ref \$_ eq "GLOB"; 		return ${*$_{SCALAR}}; 	    } 	}
        (pl-foreach ($_         (pl-cast-$ (pl-gethash %_ (pl-string-concat (pl-aref @_ 0) "::"))))
          ;; return 0 unless ref \$_ eq "GLOB" && *$_{HASH} && exists $$_{"VERSION"}
          (pl-unless           (pcl:pl-&& (pcl:pl-&& (pcl:pl-str-eq (pl-ref (pl-backslash $_)) "GLOB") (pl-* (pl-gethash %_ "HASH"))) (pl-gethash (pl-exists (pl-cast-$ $_)) "VERSION"))           (pl-return 0))
          
          ;; for ($$_{"VERSION"}) { 		return 0 unless ref \$_ eq "GLOB"; 		return ${*$_{SCALAR}}; 	    }
          (pl-foreach ($_           (pl-cast-$ (pl-gethash %_ "VERSION")))
            ;; return 0 unless ref \$_ eq "GLOB"
            (pl-unless             (pcl:pl-str-eq (pl-ref (pl-backslash $_)) "GLOB")             (pl-return 0))
            
            ;; return ${*$_{SCALAR}}
                        (pl-return (pl-cast-$ (pl-* (pl-gethash %_ "SCALAR"))))
            
          )
          
        )
        
      )
      
    )
  )
)

;; # _maybe_isa() is usually the UNIVERSAL::isa function.  We have to avoid
;; # the latter if the UNIVERSAL::isa module has been loaded, to avoid infi-
;; # nite recursion; in that case _maybe_isa simply returns true.
;; my $isa
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $isa (make-pl-box nil)))

;; BEGIN {
(eval-when (:compile-toplevel :execute)
  ;; if (_univ_mod_loaded('isa')) {         *_maybe_isa = sub { 1 }     }     else {         # Since we have already done the check, record $isa for use below         # when defining _StrVal.         *_maybe_isa = $isa = _fetch_sub(UNIVERSAL => "isa");     }
  ;; if (_univ_mod_loaded('isa'))
  (pl-if   (pl-_univ_mod_loaded "isa")
    (progn
      (defun --anon-block-18-- ()
        ;; 1
                1
        
      )
      
      ;; *_maybe_isa = sub { 1 }
            (setf (symbol-function 'pl-_maybe_isa) #'--anon-block-18--)
      
    )
    ;; else
    (progn
      ;; *_maybe_isa = $isa = _fetch_sub(UNIVERSAL => "isa")
            (pcl:pl-setf *_maybe_isa (pcl:pl-setf $isa (pl-_fetch_sub "UNIVERSAL" "isa")))
      
    )
  )
  
)

;; # We need an overload::StrVal or equivalent function, but we must avoid
;; # loading any modules on demand, as Carp is used from __DIE__ handlers and
;; # may be invoked after a syntax error.
;; # We can copy recent implementations of overload::StrVal and use
;; # overloading.pm, which is the fastest implementation, so long as
;; # overloading is available.  If it is not available, we use our own pure-
;; # Perl StrVal.  We never actually use overload::StrVal, for various rea-
;; # sons described below.
;; # overload versions are as follows:
;; #     undef-1.00 (up to perl 5.8.0)   uses bless (avoid!)
;; #     1.01-1.17  (perl 5.8.1 to 5.14) uses Scalar::Util
;; #     1.18+      (perl 5.16+)         uses overloading
;; # The ancient 'bless' implementation (that inspires our pure-Perl version)
;; # blesses unblessed references and must be avoided.  Those using
;; # Scalar::Util use refaddr, possibly the pure-Perl implementation, which
;; # has the same blessing bug, and must be avoided.  Also, Scalar::Util is
;; # loaded on demand.  Since we avoid the Scalar::Util implementations, we
;; # end up having to implement our own overloading.pm-based version for perl
;; # 5.10.1 to 5.14.  Since it also works just as well in more recent ver-
;; # sions, we use it there, too.
;; BEGIN {
(eval-when (:compile-toplevel :execute)
  ;; if (eval { require "overloading.pm" }) {         *_StrVal = eval 'sub { no overloading; "$_[0]" }'     }     else {         # Work around the UNIVERSAL::can/isa modules to avoid recursion.          # _mycan is either UNIVERSAL::can, or, in the presence of an         # override, overload::mycan.         *_mycan = _univ_mod_loaded('can')             ? do { require "overload.pm"; _fetch_sub overload => 'mycan' }             : \&UNIVERSAL::can;          # _blessed is either UNIVERSAL::isa(...), or, in the presence of an         # override, a hideous, but fairly reliable, workaround.         *_blessed = $isa             ? sub { &$isa($_[0], "UNIVERSAL") }             : sub {                 my $probe = "UNIVERSAL::Carp_probe_" . rand;                 no strict 'refs';                 local *$probe = sub { "unlikely string" };                 local $@;                 local $SIG{__DIE__} = sub{};                 (eval { $_[0]->$probe } || '') eq 'unlikely string'               };          *_StrVal = sub {             my $pack = ref $_[0];             # Perl's overload mechanism uses the presence of a special             # "method" named "((" or "()" to signal it is in effect.             # This test seeks to see if it has been set up.  "((" post-             # dates overloading.pm, so we can skip it.             return "$_[0]" unless _mycan($pack, "()");             # Even at this point, the invocant may not be blessed, so             # check for that.             return "$_[0]" if not _blessed($_[0]);             bless $_[0], "Carp";             my $str = "$_[0]";             bless $_[0], $pack;             $pack . substr $str, index $str, "=";         }     }
  (defun --anon-block-19-- ()
    ;; require "overloading.pm" (pragma)
    
  )
  
  ;; if (eval { require "overloading.pm" })
  (pl-if   (pl-eval-block (funcall #'--anon-block-19--))
    (progn
      ;; *_StrVal = eval 'sub { no overloading; "$_[0]" }'
            (pcl:pl-setf *_StrVal (pl-eval "sub { no overloading; \"$_[0]\" }"))
      
    )
    ;; else
    (progn
      ;; *_mycan = _univ_mod_loaded('can')             ? do { require "overload.pm"; _fetch_sub overload => 'mycan' }             : \&UNIVERSAL::can
      ;; PARSE ERROR: Fell through. Missing case: [
      
      (defun --anon-block-20-- ()
        ;; &$isa($_[0], "UNIVERSAL")
        ;; PARSE ERROR: Fell through. Missing case: [
        
      )
      
      (defun --anon-block-21-- ()
        ;; my $probe = "UNIVERSAL::Carp_probe_" . rand
        (eval-when (:compile-toplevel :load-toplevel :execute)
          (defvar $probe (make-pl-box nil)))
        (box-set $probe         (pcl:pl-. "UNIVERSAL::Carp_probe_" (pl-rand)))
        
        ;; no strict 'refs' (no-op)
        
        ;; local *$probe = sub { "unlikely string" }
        (defun --anon-block-22-- ()
          ;; "unlikely string"
                    "unlikely string"
          
        )
        
        (let (($probe (make-pl-box         #'--anon-block-22--)))
          
          ;; local $SIG{__DIE__} = sub{}
          (defun --anon-block-23-- ()
            nil
          )
          
          (let (($SIG (make-pl-box           #'--anon-block-23--)))
            
            (defun --anon-block-24-- ()
              ;; $_[0]->$probe
                            (pl-method-call (pl-aref @_ 0) $probe)
              
            )
            
            ;;                  (eval { $_[0]->$probe } || '') eq 'unlikely string'
            ;; PARSE ERROR: Fell through. Missing case: [] at Pl/PExpr.pm line 1188.
            
          )
          
          ;; *_blessed = $isa             ? sub { &$isa($_[0], "UNIVERSAL") }             : sub {                 my $probe = "UNIVERSAL::Carp_probe_" . rand;                 no strict 'refs';                 local *$probe = sub { "unlikely string" };                 local $@;                 local $SIG{__DIE__} = sub{};                 (eval { $_[0]->$probe } || '') eq 'unlikely string'               }
          ;; PARSE ERROR: Ternary operator: Found '?' but no matching ':'
          
          (defun --anon-block-25-- ()
            ;; my $pack = ref $_[0]
            (eval-when (:compile-toplevel :load-toplevel :execute)
              (defvar $pack (make-pl-box nil)))
            (box-set $pack             (pl-aref (pl-ref $_) 0))
            
            ;; return "$_[0]" unless _mycan($pack, "()")
            (pl-unless             (pl-_mycan $pack "()")             (pl-return (pl-aref @_ 0)))
            
            ;; return "$_[0]" if not _blessed($_[0])
            (pl-if             (pl-not (pl-_blessed (pl-aref @_ 0)))             (pl-return (pl-aref @_ 0)))
            
            ;; bless $_[0], "Carp"
                        (pl-bless (pl-aref @_ 0) "Carp")
            
            ;; my $str = "$_[0]"
            (eval-when (:compile-toplevel :load-toplevel :execute)
              (defvar $str (make-pl-box nil)))
            (box-set $str             (pl-aref @_ 0))
            
            ;; bless $_[0], $pack
                        (pl-bless (pl-aref @_ 0) $pack)
            
            ;; $pack . substr $str, index $str, "="
                        (pcl:pl-. $pack (pl-substr $str (pl-index $str "=")))
            
          )
          
          ;; *_StrVal = sub {             my $pack = ref $_[0];             # Perl's overload mechanism uses the presence of a special             # "method" named "((" or "()" to signal it is in effect.             # This test seeks to see if it has been set up.  "((" post-             # dates overloading.pm, so we can skip it.             return "$_[0]" unless _mycan($pack, "()");             # Even at this point, the invocant may not be blessed, so             # check for that.             return "$_[0]" if not _blessed($_[0]);             bless $_[0], "Carp";             my $str = "$_[0]";             bless $_[0], $pack;             $pack . substr $str, index $str, "=";         }
                    (setf (symbol-function 'pl-_StrVal) #'--anon-block-25--)
          
        )  ;; end local
      )  ;; end local
    )
  )
  
)

;; our $VERSION = '1.54'
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $VERSION (make-pl-box nil)))
(setf (pl-box-value $VERSION) "1.54")

;; $VERSION =~ tr/_//d
(pcl:pl-=~ $VERSION (pl-tr "_" "" :d))

;; our $MaxEvalLen = 0
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $MaxEvalLen (make-pl-box nil)))
(setf (pl-box-value $MaxEvalLen) 0)

;; our $Verbose    = 0
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $Verbose (make-pl-box nil)))
(setf (pl-box-value $Verbose) 0)

;; our $CarpLevel  = 0
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $CarpLevel (make-pl-box nil)))
(setf (pl-box-value $CarpLevel) 0)

;; our $MaxArgLen  = 64
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $MaxArgLen (make-pl-box nil)))
(setf (pl-box-value $MaxArgLen) 64)

;; # How much of each argument to print. 0 = all.
;; our $MaxArgNums = 8
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $MaxArgNums (make-pl-box nil)))
(setf (pl-box-value $MaxArgNums) 8)

;; # How many arguments to print. 0 = all.
;; our $RefArgFormatter = undef
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $RefArgFormatter (make-pl-box nil)))
(setf (pl-box-value $RefArgFormatter) (pl-undef))

;; # allow caller to format reference arguments
;; require Exporter (pragma)

;; our @ISA       = ('Exporter')
;; Redefine CLOS class with parents for MRO
(defclass carp (exporter) ())
(defvar @ISA (make-array 0 :adjustable t :fill-pointer 0))
(pl-push @ISA "Exporter")

;; our @EXPORT    = qw(confess croak carp)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar @EXPORT (make-array 0 :adjustable t :fill-pointer 0)))
(pl-setf @EXPORT (progn "confess" "croak" "carp"))

;; our @EXPORT_OK = qw(cluck verbose longmess shortmess)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar @EXPORT_OK (make-array 0 :adjustable t :fill-pointer 0)))
(pl-setf @EXPORT_OK (progn "cluck" "verbose" "longmess" "shortmess"))

;; our @EXPORT_FAIL = qw(verbose)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar @EXPORT_FAIL (make-array 0 :adjustable t :fill-pointer 0)))
(pl-setf @EXPORT_FAIL (progn "verbose"))

;; # hook to enable verbose mode
;; # The members of %Internal are packages that are internal to perl.
;; # Carp will not report errors from within these packages if it
;; # can.  The members of %CarpInternal are internal to Perl's warning
;; # system.  Carp will not report errors from within these packages
;; # either, and will not report calls *to* these packages for carp and
;; # croak.  They replace $CarpLevel, which is deprecated.    The
;; # $Max(EvalLen|(Arg(Len|Nums)) variables are used to specify how the eval
;; # text and function arguments should be formatted when printed.
;; our %CarpInternal
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar %CarpInternal (make-hash-table :test 'equal)))

;; our %Internal
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar %Internal (make-hash-table :test 'equal)))

;; # disable these by default, so they can live w/o require Carp
;; $CarpInternal{Carp}++
(pl-post++ (pl-gethash %CarpInternal "Carp"))

;; $CarpInternal{warnings}++
(pl-post++ (pl-gethash %CarpInternal "warnings"))

;; $Internal{Exporter}++
(pl-post++ (pl-gethash %Internal "Exporter"))

;; $Internal{'Exporter::Heavy'}++
(pl-post++ (pl-gethash %Internal "Exporter::Heavy"))

;; # if the caller specifies verbose usage ("perl -MCarp=verbose script.pl")
;; # then the following method will be called by the Exporter which knows
;; # to do this thanks to @EXPORT_FAIL, above.  $_[1] will contain the word
;; # 'verbose'.
;; sub export_fail { ... }
(pl-sub pl-export_fail (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; shift
            (pl-shift @_)
      
      ;; $Verbose = shift if $_[0] eq 'verbose'
      (pl-if       (pcl:pl-str-eq (pl-aref @_ 0) "verbose")       (pcl:pl-setf $Verbose (pl-shift @_)))
      
      ;; @_
            @_
      
    )
  )
)

;; sub _cgc { ... }
(pl-sub pl-_cgc (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; no strict 'refs' (no-op)
      
      ;; return \&{"CORE::GLOBAL::caller"} if defined &{"CORE::GLOBAL::caller"}
      (pl-if       (pl-defined (pl-bit-and "CORE::GLOBAL::caller"))       (pl-return (pl-backslash (pl-bit-and "CORE::GLOBAL::caller"))))
      
      ;; return
            (pl-return)
      
    )
  )
)

;; sub longmess { ... }
(pl-sub pl-longmess (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($cgc (make-pl-box nil)) ($call_pack (make-pl-box nil)))
        ;; my $cgc = _cgc()
                (pcl:pl-setf $cgc (pl-_cgc))
        
        ;; my $call_pack = $cgc ? $cgc->() : caller()
                (pcl:pl-setf $call_pack (pl-if $cgc (pl-funcall-ref $cgc) (pl-caller)))
        
        ;; if ( $Internal{$call_pack} or $CarpInternal{$call_pack} ) {         return longmess_heavy(@_);     }     else {         local $CarpLevel = $CarpLevel + 1;         return longmess_heavy(@_);     }
        ;; if ($Internal{$call_pack} or $CarpInternal{$call_pack})
        (pl-if         (pcl:pl-or (pl-gethash %Internal $call_pack) (pl-gethash %CarpInternal $call_pack))
          (progn
            ;; return longmess_heavy(@_)
                        (pl-return (pl-longmess_heavy @_))
            
          )
          ;; else
          (progn
            ;; local $CarpLevel = $CarpLevel + 1
            (let (($CarpLevel (make-pl-box             (pcl:pl-+ $CarpLevel 1))))
              
              ;; return longmess_heavy(@_)
                            (pl-return (pl-longmess_heavy @_))
              
            )  ;; end local
          )
        )
        
      )
    )
  )
)

;; our @CARP_NOT
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar @CARP_NOT (make-array 0 :adjustable t :fill-pointer 0)))

;; sub shortmess { ... }
(pl-sub pl-shortmess (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($cgc (make-pl-box nil)))
        ;; my $cgc = _cgc()
                (pcl:pl-setf $cgc (pl-_cgc))
        
        ;; local @CARP_NOT = scalar( $cgc ? $cgc->() : caller() )
        (let ((@CARP_NOT (make-array 0 :adjustable t :fill-pointer 0)))
          
          ;; shortmess_heavy(@_)
                    (pl-shortmess_heavy @_)
          
        )  ;; end local
      )
    )
  )
)

;; sub croak   { ... }
(pl-sub pl-croak (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; die shortmess @_
            (pl-die (pl-shortmess @_))
      
    )
  )
)

;; sub confess { ... }
(pl-sub pl-confess (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; die longmess @_
            (pl-die (pl-longmess @_))
      
    )
  )
)

;; sub carp    { ... }
(pl-sub pl-carp (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; warn shortmess @_
            (pl-warn (pl-shortmess @_))
      
    )
  )
)

;; sub cluck   { ... }
(pl-sub pl-cluck (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; warn longmess @_
            (pl-warn (pl-longmess @_))
      
    )
  )
)

;; BEGIN {
(eval-when (:compile-toplevel :execute)
  ;; if("$]" >= 5.015002 || ("$]" >= 5.014002 && "$]" < 5.015) || 	    ("$]" >= 5.012005 && "$]" < 5.013)) { 	*CALLER_OVERRIDE_CHECK_OK = sub () { 1 };     } else { 	*CALLER_OVERRIDE_CHECK_OK = sub () { 0 };     }
  ;; if ("$]" >= 5.015002 || ("$]" >= 5.014002 && "$]" < 5.015) ||
;; 	    ("$]" >= 5.012005 && "$]" < 5.013))
  (pl-if   (pcl:pl-|| (pcl:pl-|| (pcl:pl->= $] 5.015002) (pcl:pl-&& (pcl:pl->= $] 5.014002) (pcl:pl-< $] 5.015))) (pcl:pl-&& (pcl:pl->= $] 5.012005) (pcl:pl-< $] 5.013)))
    (progn
      ;; *CALLER_OVERRIDE_CHECK_OK = sub () { 1 }
      ;; PARSE ERROR: Fell through. Missing case: [
      
    )
    ;; else
    (progn
      ;; *CALLER_OVERRIDE_CHECK_OK = sub () { 0 }
      ;; PARSE ERROR: Fell through. Missing case: [
      
    )
  )
  
)

;; sub caller_info { ... }
(pl-sub pl-caller_info (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($i (make-pl-box nil)) (%call_info (make-pl-box nil)) ($cgc (make-pl-box nil)) ($sub_name (make-pl-box nil)) (@args (make-pl-box nil)) ($arg (make-pl-box nil)) ($where (make-pl-box nil)) ($func (make-pl-box nil)) ($gv (make-pl-box nil)) ($package (make-pl-box nil)) ($subname (make-pl-box nil)) ($overflow (make-pl-box nil)))
        ;; my $i = shift(@_) + 1
                (pcl:pl-setf $i (pcl:pl-+ (pl-shift @_) 1))
        
        ;; my %call_info (bare declaration)
        
        ;; my $cgc = _cgc()
                (pcl:pl-setf $cgc (pl-_cgc))
        
        ;; { ... }
        (progn
          ;; @DB::args = \$i if CALLER_OVERRIDE_CHECK_OK
          (pl-if           (pl-CALLER_OVERRIDE_CHECK_OK)           (pcl:pl-setf DB::@args (pl-backslash $i)))
          
          ;;; package DB
          (defpackage :DB
            (:use :cl :pcl))
          (in-package :DB)
          ;; CLOS class for MRO
          (defclass db () ())
          
          ;; @call_info{             qw(pack file line sub has_args wantarray evaltext is_require) }             = $cgc ? $cgc->($i) : caller($i)
                    (pcl:pl-setf (pl-hslice @call_info "pack" "file" "line" "sub" "has_args" "wantarray" "evaltext" "is_require") (pl-if $cgc (let ((*wantarray* t)) (pl-funcall-ref $cgc $i)) (let ((*wantarray* t)) (pl-caller $i))))
          
        )
        
        ;; unless ( defined $call_info{file} ) {         return ();     }
        ;; unless (defined $call_info{"file"})
        (pl-if (pl-not         (pl-defined (pl-gethash %call_info "file")))
          (progn
            ;; return ()
                        (pl-return)
            
          )
          nil
        )
        
        ;; my $sub_name = Carp::get_subname( \%call_info )
                (pcl:pl-setf $sub_name (Carp::pl-get_subname (pl-backslash %call_info)))
        
        ;; if ( $call_info{has_args} ) {         # Guard our serialization of the stack from stack refcounting bugs         # NOTE this is NOT a complete solution, we cannot 100% guard against         # these bugs.  However in many cases Perl *is* capable of detecting         # them and throws an error when it does.  Unfortunately serializing         # the arguments on the stack is a perfect way of finding these bugs,         # even when they would not affect normal program flow that did not         # poke around inside the stack.  Inside of Carp.pm it makes little         # sense reporting these bugs, as Carp's job is to report the callers         # errors, not the ones it might happen to tickle while doing so.         # See: https://rt.perl.org/Public/Bug/Display.html?id=131046         # and: https://rt.perl.org/Public/Bug/Display.html?id=52610         # for more details and discussion. - Yves         my @args = map {                 my $arg;                 local $@= $@;                 eval {                     $arg = $_;                     1;                 } or do {                     $arg = '** argument not available anymore **';                 };                 $arg;             } @DB::args;         if (CALLER_OVERRIDE_CHECK_OK && @args == 1             && ref $args[0] eq ref \$i             && $args[0] == \$i ) {             @args = ();    # Don't let anyone see the address of $i             local $@;             my $where = eval {                 my $func    = $cgc or return '';                 my $gv      =                     (_fetch_sub B => 'svref_2object' or return '')                         ->($func)->GV;                 my $package = $gv->STASH->NAME;                 my $subname = $gv->NAME;                 return unless defined $package && defined $subname;                  # returning CORE::GLOBAL::caller isn't useful for tracing the cause:                 return if $package eq 'CORE::GLOBAL' && $subname eq 'caller';                 " in &${package}::$subname";             } || '';             @args                 = "** Incomplete caller override detected$where; \@DB::args were not set **";         }         else {             my $overflow;             if ( $MaxArgNums and @args > $MaxArgNums )             {    # More than we want to show?                 $#args = $MaxArgNums - 1;                 $overflow = 1;             }              @args = map { Carp::format_arg($_) } @args;              if ($overflow) {                 push @args, '...';             }         }          # Push the args onto the subroutine         $sub_name .= '(' . join( ', ', @args ) . ')';     }
        ;; if ($call_info{"has_args"})
        (pl-if         (pl-gethash %call_info "has_args")
          (progn
            (defun --anon-block-26-- ($_)
              ;; my $arg (bare declaration)
              
              (defun --anon-block-27-- ()
                ;; $arg = $_
                                (pcl:pl-setf $arg $_)
                
                ;; 1
                                1
                
              )
              
              ;; eval {                     $arg = $_;                     1;                 } or do {                     $arg = '** argument not available anymore **';                 }
              ;; PARSE ERROR: Fell through. Missing case: [
              
              ;; $arg
                            $arg
              
            )
            
            ;; my @args = map {                 my $arg;                 local $@= $@;                 eval {                     $arg = $_;                     1;                 } or do {                     $arg = '** argument not available anymore **';                 };                 $arg;             } @DB::args
                        (pcl:pl-setf @args (let ((*wantarray* t)) (pl-map #'--anon-block-26-- DB::@args)))
            
            ;; if (CALLER_OVERRIDE_CHECK_OK && @args == 1             && ref $args[0] eq ref \$i             && $args[0] == \$i ) {             @args = ();    # Don't let anyone see the address of $i             local $@;             my $where = eval {                 my $func    = $cgc or return '';                 my $gv      =                     (_fetch_sub B => 'svref_2object' or return '')                         ->($func)->GV;                 my $package = $gv->STASH->NAME;                 my $subname = $gv->NAME;                 return unless defined $package && defined $subname;                  # returning CORE::GLOBAL::caller isn't useful for tracing the cause:                 return if $package eq 'CORE::GLOBAL' && $subname eq 'caller';                 " in &${package}::$subname";             } || '';             @args                 = "** Incomplete caller override detected$where; \@DB::args were not set **";         }         else {             my $overflow;             if ( $MaxArgNums and @args > $MaxArgNums )             {    # More than we want to show?                 $#args = $MaxArgNums - 1;                 $overflow = 1;             }              @args = map { Carp::format_arg($_) } @args;              if ($overflow) {                 push @args, '...';             }         }
            ;; if (CALLER_OVERRIDE_CHECK_OK && @args == 1
;;             && ref $args[0] eq ref \$i
;;             && $args[0] == \$i)
            (pl-if             (pcl:pl-&& (pcl:pl-&& (pcl:pl-&& (pl-CALLER_OVERRIDE_CHECK_OK) (pcl:pl-== @args 1)) (pcl:pl-str-eq (pl-ref (pl-aref @args 0)) (pl-ref (pl-backslash $i)))) (pcl:pl-== (pl-aref @args 0) (pl-backslash $i)))
              (progn
                ;; @args = ()
                                (pcl:pl-setf @args (vector ))
                
                (defun --anon-block-28-- ()
                  ;; my $func    = $cgc or return ''
                                    (pcl:pl-or (pcl:pl-setf $func $cgc) (pl-return ""))
                  
                  ;; my $gv      =                     (_fetch_sub B => 'svref_2object' or return '')                         ->($func)->GV
                                    (pcl:pl-setf $gv (pl-method-call (pl-funcall-ref (pcl:pl-or (pl-_fetch_sub "B" "svref_2object") (pl-return "")) $func) 'GV))
                  
                  ;; my $package = $gv->STASH->NAME
                                    (pcl:pl-setf $package (pl-method-call (pl-method-call $gv 'STASH) 'NAME))
                  
                  ;; my $subname = $gv->NAME
                                    (pcl:pl-setf $subname (pl-method-call $gv 'NAME))
                  
                  ;; return unless defined $package && defined $subname
                  (pl-unless                   (pcl:pl-&& (pl-defined $package) (pl-defined $subname))                   (pl-return))
                  
                  ;; return if $package eq 'CORE::GLOBAL' && $subname eq 'caller'
                  (pl-if                   (pcl:pl-&& (pcl:pl-str-eq $package "CORE::GLOBAL") (pcl:pl-str-eq $subname "caller"))                   (pl-return))
                  
                  ;; " in &${package}::$subname"
                                    (pl-string-concat " in &" (pl-UNDEFINED) "::" $subname)
                  
                )
                
                ;; my $where = eval {                 my $func    = $cgc or return '';                 my $gv      =                     (_fetch_sub B => 'svref_2object' or return '')                         ->($func)->GV;                 my $package = $gv->STASH->NAME;                 my $subname = $gv->NAME;                 return unless defined $package && defined $subname;                  # returning CORE::GLOBAL::caller isn't useful for tracing the cause:                 return if $package eq 'CORE::GLOBAL' && $subname eq 'caller';                 " in &${package}::$subname";             } || ''
                ;; PARSE ERROR: Fell through. Missing case: [] at Pl/PExpr.pm line 1188.
                
                ;; @args                 = "** Incomplete caller override detected$where; \@DB::args were not set **"
                                (pcl:pl-setf @args (pl-string-concat "** Incomplete caller override detected" $where "; \\" (pl-join |$"| @DB) "::args were not set **"))
                
              )
              ;; else
              (progn
                ;; my $overflow (bare declaration)
                
                ;; if ( $MaxArgNums and @args > $MaxArgNums )             {    # More than we want to show?                 $#args = $MaxArgNums - 1;                 $overflow = 1;             }
                ;; if ($MaxArgNums and @args > $MaxArgNums)
                (pl-if                 (pcl:pl-and $MaxArgNums (pcl:pl-> @args $MaxArgNums))
                  (progn
                    ;; $#args = $MaxArgNums - 1
                                        (pcl:pl-setf (pl-array-last-index @args) (pcl:pl-- $MaxArgNums 1))
                    
                    ;; $overflow = 1
                                        (pcl:pl-setf $overflow 1)
                    
                  )
                  nil
                )
                
                (defun --anon-block-29-- ($_)
                  ;; Carp::format_arg($_)
                                    (Carp::pl-format_arg $_)
                  
                )
                
                ;; @args = map { Carp::format_arg($_) } @args
                                (pcl:pl-setf @args (let ((*wantarray* t)) (pl-map #'--anon-block-29-- @args)))
                
                ;; if ($overflow) {                 push @args, '...';             }
                ;; if ($overflow)
                (pl-if                 $overflow
                  (progn
                    ;; push @args, '...'
                                        (pl-push @args "...")
                    
                  )
                  nil
                )
                
              )
            )
            
            ;; $sub_name .= '(' . join( ', ', @args ) . ')'
                        (pcl:pl-.= $sub_name (pcl:pl-. (pcl:pl-. "(" (pl-join ", " @args)) ")"))
            
          )
          nil
        )
        
        ;; $call_info{sub_name} = $sub_name
                (pcl:pl-setf (pl-gethash %call_info "sub_name") $sub_name)
        
        ;; return wantarray() ? %call_info : \%call_info
                (pl-return (pl-if (pl-wantarray) %call_info (pl-backslash %call_info)))
        
      )
    )
  )
)

;; # Transform an argument to a function into a string.
;; our $in_recurse
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $in_recurse (make-pl-box nil)))

;; sub format_arg { ... }
(pl-sub pl-format_arg (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($arg (make-pl-box nil)) ($pack (make-pl-box nil)) ($suffix (make-pl-box nil)) ($i (make-pl-box nil)) ($c (make-pl-box nil)) ($x (make-pl-box nil)) ($o (make-pl-box nil)))
        ;; my $arg = shift
                (pcl:pl-setf $arg (pl-shift @_))
        
        ;; if ( my $pack= ref($arg) ) {           # legitimate, let's not leak it.         if (!$in_recurse && _maybe_isa( $arg, 'UNIVERSAL' ) && 	    do {                 local $@; 	        local $in_recurse = 1; 		local $SIG{__DIE__} = sub{};                 eval {$arg->can('CARP_TRACE') }             })         {             return $arg->CARP_TRACE();         }         elsif (!$in_recurse && 	       defined($RefArgFormatter) && 	       do {                 local $@; 	        local $in_recurse = 1; 		local $SIG{__DIE__} = sub{};                 eval {$arg = $RefArgFormatter->($arg); 1}                 })         {             return $arg;         }         else         {             # Argument may be blessed into a class with overloading, and so             # might have an overloaded stringification.  We don't want to             # risk getting the overloaded stringification, so we need to             # use _StrVal, our overload::StrVal()-equivalent.             return _StrVal $arg;         }     }
        (let (($pack (make-pl-box nil)))
          ;; if (my $pack= ref($arg))
          (pl-if           (pcl:pl-setf $pack (pl-ref $arg))
            (progn
              ;; if (!$in_recurse && _maybe_isa( $arg, 'UNIVERSAL' ) && 	    do {                 local $@; 	        local $in_recurse = 1; 		local $SIG{__DIE__} = sub{};                 eval {$arg->can('CARP_TRACE') }             })         {             return $arg->CARP_TRACE();         }         elsif (!$in_recurse && 	       defined($RefArgFormatter) && 	       do {                 local $@; 	        local $in_recurse = 1; 		local $SIG{__DIE__} = sub{};                 eval {$arg = $RefArgFormatter->($arg); 1}                 })         {             return $arg;         }         else         {             # Argument may be blessed into a class with overloading, and so             # might have an overloaded stringification.  We don't want to             # risk getting the overloaded stringification, so we need to             # use _StrVal, our overload::StrVal()-equivalent.             return _StrVal $arg;         }
              ;; if (!$in_recurse && _maybe_isa( $arg, 'UNIVERSAL' ) &&
;; 	    do {
;;                 local $@;
;; 	        local $in_recurse = 1;
;; 		local $SIG{__DIE__} = sub{};
;;                 eval {$arg->can('CARP_TRACE') }
;;             })
              (pl-if ;; PARSE ERROR: Fell through. Missing case: [
                (progn
                  ;; return $arg->CARP_TRACE()
                                    (pl-return (pl-method-call $arg 'CARP_TRACE))
                  
                )
                ;; elsif (!$in_recurse &&
;; 	       defined($RefArgFormatter) &&
;; 	       do {
;;                 local $@;
;; 	        local $in_recurse = 1;
;; 		local $SIG{__DIE__} = sub{};
;;                 eval {$arg = $RefArgFormatter->($arg); 1}
;;                 })
                (pl-if ;; PARSE ERROR: Fell through. Missing case: [
                  (progn
                    ;; return $arg
                                        (pl-return $arg)
                    
                  )
                  ;; else
                  (progn
                    ;; return _StrVal $arg
                                        (pl-return (pl-_StrVal $arg))
                    
                  )
                )
              )
              
            )
            nil
          )
        )
        
        ;; return "undef" if !defined($arg)
        (pl-if         (pl-! (pl-defined $arg))         (pl-return "undef"))
        
        ;; downgrade($arg, 1)
                (pl-downgrade $arg 1)
        
        ;; return $arg if !(UTF8_REGEXP_PROBLEM && is_utf8($arg)) && 	    $arg =~ /\A-?[0-9]+(?:\.[0-9]*)?(?:[eE][-+]?[0-9]+)?\z/
        (pl-if         (pcl:pl-&& (pl-! (pcl:pl-&& (pl-UTF8_REGEXP_PROBLEM) (pl-is_utf8 $arg))) (pcl:pl-=~ $arg (pl-regex "/\\A-?[0-9]+(?:\\.[0-9]*)?(?:[eE][-+]?[0-9]+)?\\z/")))         (pl-return $arg))
        
        ;; my $suffix = ""
                (pcl:pl-setf $suffix "")
        
        ;; if ( 2 < $MaxArgLen and $MaxArgLen < length($arg) ) {         substr ( $arg, $MaxArgLen - 3 ) = ""; 	$suffix = "...";     }
        ;; if (2 < $MaxArgLen and $MaxArgLen < length($arg))
        (pl-if         (pcl:pl-and (pcl:pl-< 2 $MaxArgLen) (pcl:pl-< $MaxArgLen (pl-length $arg)))
          (progn
            ;; substr ( $arg, $MaxArgLen - 3 ) = ""
                        (pcl:pl-setf (pl-substr $arg (pcl:pl-- $MaxArgLen 3)) "")
            
            ;; $suffix = "..."
                        (pcl:pl-setf $suffix "...")
            
          )
          nil
        )
        
        ;; if(UTF8_REGEXP_PROBLEM && is_utf8($arg)) { 	for(my $i = length($arg); $i--; ) { 	    my $c = substr($arg, $i, 1); 	    my $x = substr($arg, 0, 0);   # work around bug on Perl 5.8.{1,2} 	    if($c eq "\"" || $c eq "\\" || $c eq "\$" || $c eq "\@") { 		substr $arg, $i, 0, "\\"; 		next; 	    } 	    my $o = ord($c); 	    substr $arg, $i, 1, sprintf("\\x{%x}", $o) 		unless is_safe_printable_codepoint($o); 	}     } else { 	$arg =~ s/([\"\\\$\@])/\\$1/g;         # This is all the ASCII printables spelled-out.  It is portable to all         # Perl versions and platforms (such as EBCDIC).  There are other more         # compact ways to do this, but may not work everywhere every version.         $arg =~ s/([^ !"#\$\%\&'()*+,\-.\/0123456789:;<=>?\@ABCDEFGHIJKLMNOPQRSTUVWXYZ\[\\\]^_`abcdefghijklmnopqrstuvwxyz\{|}~])/sprintf("\\x{%x}",ord($1))/eg;     }
        ;; if (UTF8_REGEXP_PROBLEM && is_utf8($arg))
        (pl-if         (pcl:pl-&& (pl-UTF8_REGEXP_PROBLEM) (pl-is_utf8 $arg))
          (progn
            ;; for(my $i = length($arg); $i--; ) { 	    my $c = substr($arg, $i, 1); 	    my $x = substr($arg, 0, 0);   # work around bug on Perl 5.8.{1,2} 	    if($c eq "\"" || $c eq "\\" || $c eq "\$" || $c eq "\@") { 		substr $arg, $i, 0, "\\"; 		next; 	    } 	    my $o = ord($c); 	    substr $arg, $i, 1, sprintf("\\x{%x}", $o) 		unless is_safe_printable_codepoint($o); 	}
            (let (($i (make-pl-box nil)))
              (pl-for (            (pcl:pl-setf $i (pl-length $arg)))
                      (            (pl-post-- $i))
                      ()
                ;; my $c = substr($arg, $i, 1)
                                (pcl:pl-setf $c (pl-substr $arg $i 1))
                
                ;; my $x = substr($arg, 0, 0)
                                (pcl:pl-setf $x (pl-substr $arg 0 0))
                
                ;; if($c eq "\"" || $c eq "\\" || $c eq "\$" || $c eq "\@") { 		substr $arg, $i, 0, "\\"; 		next; 	    }
                ;; if ($c eq "\"" || $c eq "\\" || $c eq "\$" || $c eq "\@")
                (pl-if                 (pcl:pl-|| (pcl:pl-|| (pcl:pl-|| (pcl:pl-str-eq $c "\"") (pcl:pl-str-eq $c "\\")) (pcl:pl-str-eq $c "$")) (pcl:pl-str-eq $c "@"))
                  (progn
                    ;; substr $arg, $i, 0, "\\"
                                        (pl-substr $arg $i 0 "\\")
                    
                    ;; next
                                        (pl-next)
                    
                  )
                  nil
                )
                
                ;; my $o = ord($c)
                                (pcl:pl-setf $o (pl-ord $c))
                
                ;; substr $arg, $i, 1, sprintf("\\x{%x}", $o) 		unless is_safe_printable_codepoint($o)
                (pl-unless                 (pl-is_safe_printable_codepoint $o)                 (pl-substr $arg $i 1 (pl-sprintf "\\x{%x}" $o)))
                
              )
            )
            
          )
          ;; else
          (progn
            ;; $arg =~ s/([\"\\\$\@])/\\$1/g
                        (pcl:pl-=~ $arg (pl-subst "([\\\"\\\\\\$\\@])" "\\\\$1" :g))
            
            ;; $arg =~ s/([^ !"#\$\%\&'()*+,\-.\/0123456789:;<=>?\@ABCDEFGHIJKLMNOPQRSTUVWXYZ\[\\\]^_`abcdefghijklmnopqrstuvwxyz\{|}~])/sprintf("\\x{%x}",ord($1))/eg
                        (pcl:pl-=~ $arg (pl-subst "([^ !\"#\\$\\%\\&'()*+,\\-.\\/0123456789:;<=>?\\@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\[\\\\\\]^_`abcdefghijklmnopqrstuvwxyz\\{|}~])" "sprintf(\"\\\\x{%x}\",ord($1))" :e :g))
            
          )
        )
        
        ;; downgrade($arg, 1)
                (pl-downgrade $arg 1)
        
        ;; return "\"".$arg."\"".$suffix
                (pl-return (pcl:pl-. (pcl:pl-. (pcl:pl-. "\"" $arg) "\"") $suffix))
        
      )
    )
  )
)

;; sub Regexp::CARP_TRACE { ... }
(pl-sub pl-Regexp::CARP_TRACE (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($arg (make-pl-box nil)) ($i (make-pl-box nil)) ($o (make-pl-box nil)) ($x (make-pl-box nil)) ($suffix (make-pl-box nil)))
        ;; my $arg = "$_[0]"
                (pcl:pl-setf $arg (pl-aref @_ 0))
        
        ;; downgrade($arg, 1)
                (pl-downgrade $arg 1)
        
        ;; if(UTF8_REGEXP_PROBLEM && is_utf8($arg)) { 	for(my $i = length($arg); $i--; ) { 	    my $o = ord(substr($arg, $i, 1)); 	    my $x = substr($arg, 0, 0);   # work around bug on Perl 5.8.{1,2} 	    substr $arg, $i, 1, sprintf("\\x{%x}", $o) 		unless is_safe_printable_codepoint($o); 	}     } else {         # See comment in format_arg() about this same regex.         $arg =~ s/([^ !"#\$\%\&'()*+,\-.\/0123456789:;<=>?\@ABCDEFGHIJKLMNOPQRSTUVWXYZ\[\\\]^_`abcdefghijklmnopqrstuvwxyz\{|}~])/sprintf("\\x{%x}",ord($1))/eg;     }
        ;; if (UTF8_REGEXP_PROBLEM && is_utf8($arg))
        (pl-if         (pcl:pl-&& (pl-UTF8_REGEXP_PROBLEM) (pl-is_utf8 $arg))
          (progn
            ;; for(my $i = length($arg); $i--; ) { 	    my $o = ord(substr($arg, $i, 1)); 	    my $x = substr($arg, 0, 0);   # work around bug on Perl 5.8.{1,2} 	    substr $arg, $i, 1, sprintf("\\x{%x}", $o) 		unless is_safe_printable_codepoint($o); 	}
            (let (($i (make-pl-box nil)))
              (pl-for (            (pcl:pl-setf $i (pl-length $arg)))
                      (            (pl-post-- $i))
                      ()
                ;; my $o = ord(substr($arg, $i, 1))
                                (pcl:pl-setf $o (pl-ord (pl-substr $arg $i 1)))
                
                ;; my $x = substr($arg, 0, 0)
                                (pcl:pl-setf $x (pl-substr $arg 0 0))
                
                ;; substr $arg, $i, 1, sprintf("\\x{%x}", $o) 		unless is_safe_printable_codepoint($o)
                (pl-unless                 (pl-is_safe_printable_codepoint $o)                 (pl-substr $arg $i 1 (pl-sprintf "\\x{%x}" $o)))
                
              )
            )
            
          )
          ;; else
          (progn
            ;; $arg =~ s/([^ !"#\$\%\&'()*+,\-.\/0123456789:;<=>?\@ABCDEFGHIJKLMNOPQRSTUVWXYZ\[\\\]^_`abcdefghijklmnopqrstuvwxyz\{|}~])/sprintf("\\x{%x}",ord($1))/eg
                        (pcl:pl-=~ $arg (pl-subst "([^ !\"#\\$\\%\\&'()*+,\\-.\\/0123456789:;<=>?\\@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\[\\\\\\]^_`abcdefghijklmnopqrstuvwxyz\\{|}~])" "sprintf(\"\\\\x{%x}\",ord($1))" :e :g))
            
          )
        )
        
        ;; downgrade($arg, 1)
                (pl-downgrade $arg 1)
        
        ;; my $suffix = ""
                (pcl:pl-setf $suffix "")
        
        ;; if($arg =~ /\A\(\?\^?([a-z]*)(?:-[a-z]*)?:(.*)\)\z/s) { 	($suffix, $arg) = ($1, $2);     }
        ;; if ($arg =~ /\A\(\?\^?([a-z]*)(?:-[a-z]*)?:(.*)\)\z/s)
        (pl-if         (pcl:pl-=~ $arg (pl-regex "/\\A\\(\\?\\^?([a-z]*)(?:-[a-z]*)?:(.*)\\)\\z/s"))
          (progn
            ;;  	($suffix, $arg) = ($1, $2)
                        (pcl:pl-setf (vector $suffix $arg) (vector $1 $2))
            
          )
          nil
        )
        
        ;; if ( 2 < $MaxArgLen and $MaxArgLen < length($arg) ) {         substr ( $arg, $MaxArgLen - 3 ) = ""; 	$suffix = "...".$suffix;     }
        ;; if (2 < $MaxArgLen and $MaxArgLen < length($arg))
        (pl-if         (pcl:pl-and (pcl:pl-< 2 $MaxArgLen) (pcl:pl-< $MaxArgLen (pl-length $arg)))
          (progn
            ;; substr ( $arg, $MaxArgLen - 3 ) = ""
                        (pcl:pl-setf (pl-substr $arg (pcl:pl-- $MaxArgLen 3)) "")
            
            ;; $suffix = "...".$suffix
                        (pcl:pl-setf $suffix (pcl:pl-. "..." $suffix))
            
          )
          nil
        )
        
        ;; return "qr($arg)$suffix"
                (pl-return (pl-string-concat "qr(" $arg ")" $suffix))
        
      )
    )
  )
)

;; # Takes an inheritance cache and a package and returns
;; # an anon hash of known inheritances and anon array of
;; # inheritances which consequences have not been figured
;; # for.
;; sub get_status { ... }
(pl-sub pl-get_status (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($cache (make-pl-box nil)) ($pkg (make-pl-box nil)))
        ;; my $cache = shift
                (pcl:pl-setf $cache (pl-shift @_))
        
        ;; my $pkg   = shift
                (pcl:pl-setf $pkg (pl-shift @_))
        
        ;; $cache->{$pkg} ||= [ { $pkg => $pkg }, [ trusts_directly($pkg) ] ]
                (pcl:pl-or-assign (pl-gethash-deref $cache $pkg) (make-array 2 :adjustable t :fill-pointer t :initial-contents (list (pl-hash $pkg $pkg) (make-array 1 :adjustable t :fill-pointer t :initial-contents (list (let ((*wantarray* t)) (pl-trusts_directly $pkg)))))))
        
        ;; return @{ $cache->{$pkg} }
                (pl-return (pl-cast-@ (pl-gethash-deref $cache $pkg)))
        
      )
    )
  )
)

;; # Takes the info from caller() and figures out the name of
;; # the sub/require/eval
;; sub get_subname { ... }
(pl-sub pl-get_subname (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($info (make-pl-box nil)) ($eval (make-pl-box nil)))
        ;; my $info = shift
                (pcl:pl-setf $info (pl-shift @_))
        
        ;; if ( defined( $info->{evaltext} ) ) {         my $eval = $info->{evaltext};         if ( $info->{is_require} ) {             return "require $eval";         }         else {             $eval =~ s/([\\\'])/\\$1/g;             return "eval '" . str_len_trim( $eval, $MaxEvalLen ) . "'";         }     }
        ;; if (defined( $info->{"evaltext"} ))
        (pl-if         (pl-defined (pl-gethash-deref $info "evaltext"))
          (progn
            ;; my $eval = $info->{evaltext}
                        (pcl:pl-setf $eval (pl-gethash-deref $info "evaltext"))
            
            ;; if ( $info->{is_require} ) {             return "require $eval";         }         else {             $eval =~ s/([\\\'])/\\$1/g;             return "eval '" . str_len_trim( $eval, $MaxEvalLen ) . "'";         }
            ;; if ($info->{"is_require"})
            (pl-if             (pl-gethash-deref $info "is_require")
              (progn
                ;; return "require $eval"
                                (pl-return (pl-string-concat "require " $eval))
                
              )
              ;; else
              (progn
                ;; $eval =~ s/([\\\'])/\\$1/g
                                (pcl:pl-=~ $eval (pl-subst "([\\\\\\'])" "\\\\$1" :g))
                
                ;; return "eval '" . str_len_trim( $eval, $MaxEvalLen ) . "'"
                                (pl-return (pcl:pl-. (pcl:pl-. "eval '" (pl-str_len_trim $eval $MaxEvalLen)) "'"))
                
              )
            )
            
          )
          nil
        )
        
        ;; if ( !defined( $info->{sub} ) ) {         return '__ANON__::__ANON__';     }
        ;; if (!defined( $info->{"sub"} ))
        (pl-if         (pl-! (pl-defined (pl-gethash-deref $info "sub")))
          (progn
            ;; return '__ANON__::__ANON__'
                        (pl-return "__ANON__::__ANON__")
            
          )
          nil
        )
        
        ;; return ( $info->{sub} eq '(eval)' ) ? 'eval {...}' : $info->{sub}
                (pl-if (pl-return (pcl:pl-str-eq (pl-gethash-deref $info "sub") "(eval)")) "eval {...}" (pl-gethash-deref $info "sub"))
        
      )
    )
  )
)

;; # Figures out what call (from the point of view of the caller)
;; # the long error backtrace should start at.
;; sub long_error_loc { ... }
(pl-sub pl-long_error_loc (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($i (make-pl-box nil)) ($lvl (make-pl-box nil)) ($cgc (make-pl-box nil)) (@caller (make-pl-box nil)) ($pkg (make-pl-box nil)))
        ;; my $i (bare declaration)
        
        ;; my $lvl = $CarpLevel
                (pcl:pl-setf $lvl $CarpLevel)
        
        ;; { ... }
        (progn
          ;; ++$i
                    (pl-pre++ $i)
          
          ;; my $cgc = _cgc()
                    (pcl:pl-setf $cgc (pl-_cgc))
          
          ;; my @caller = $cgc ? $cgc->($i) : caller($i)
                    (pcl:pl-setf @caller (pl-if $cgc (let ((*wantarray* t)) (pl-funcall-ref $cgc $i)) (let ((*wantarray* t)) (pl-caller $i))))
          
          ;; my $pkg = $caller[0]
                    (pcl:pl-setf $pkg (pl-aref @caller 0))
          
          ;; unless ( defined($pkg) ) {              # This *shouldn't* happen.             if (%Internal) {                 local %Internal;                 $i = long_error_loc();                 last;             }             elsif (defined $caller[2]) {                 # this can happen when the stash has been deleted                 # in that case, just assume that it's a reasonable place to                 # stop (the file and line data will still be intact in any                 # case) - the only issue is that we can't detect if the                 # deleted package was internal (so don't do that then)                 # -doy                 redo unless 0 > --$lvl;                 last;             }             else {                 return 2;             }         }
          ;; unless (defined($pkg))
          (pl-if (pl-not           (pl-defined $pkg))
            (progn
              ;; if (%Internal) {                 local %Internal;                 $i = long_error_loc();                 last;             }             elsif (defined $caller[2]) {                 # this can happen when the stash has been deleted                 # in that case, just assume that it's a reasonable place to                 # stop (the file and line data will still be intact in any                 # case) - the only issue is that we can't detect if the                 # deleted package was internal (so don't do that then)                 # -doy                 redo unless 0 > --$lvl;                 last;             }             else {                 return 2;             }
              ;; if (%Internal)
              (pl-if               %Internal
                (progn
                  ;; local %Internal
                  (let ((%Internal (make-hash-table :test 'equal)))
                    
                    ;; $i = long_error_loc()
                                        (pcl:pl-setf $i (pl-long_error_loc))
                    
                    ;; last
                                        (pl-last)
                    
                  )  ;; end local
                )
                ;; elsif (defined $caller[2])
                (pl-if                 (pl-defined (pl-aref @caller 2))
                  (progn
                    ;; redo unless 0 > --$lvl
                    (pl-unless                     (pcl:pl-> 0 (pl-pre-- $lvl))                     (pl-redo))
                    
                    ;; last
                                        (pl-last)
                    
                  )
                  ;; else
                  (progn
                    ;; return 2
                                        (pl-return 2)
                    
                  )
                )
              )
              
            )
            nil
          )
          
          ;; redo if $CarpInternal{$pkg}
          (pl-if           (pl-gethash %CarpInternal $pkg)           (pl-redo))
          
          ;; redo unless 0 > --$lvl
          (pl-unless           (pcl:pl-> 0 (pl-pre-- $lvl))           (pl-redo))
          
          ;; redo if $Internal{$pkg}
          (pl-if           (pl-gethash %Internal $pkg)           (pl-redo))
          
        )
        
        ;; return $i - 1
                (pl-return (pcl:pl-- $i 1))
        
      )
    )
  )
)

;; sub longmess_heavy { ... }
(pl-sub pl-longmess_heavy (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($i (make-pl-box nil)))
        ;; if ( ref( $_[0] ) ) {   # don't break references as exceptions         return wantarray ? @_ : $_[0];     }
        ;; if (ref( $_[0] ))
        (pl-if         (pl-ref (pl-aref @_ 0))
          (progn
            ;; return wantarray ? @_ : $_[0]
                        (pl-return (pl-if (pl-wantarray) @_ (pl-aref @_ 0)))
            
          )
          nil
        )
        
        ;; my $i = long_error_loc()
                (pcl:pl-setf $i (pl-long_error_loc))
        
        ;; return ret_backtrace( $i, @_ )
                (pl-return (pl-ret_backtrace $i @_))
        
      )
    )
  )
)

;; BEGIN {
(eval-when (:compile-toplevel :execute)
  ;; if("$]" >= 5.017004) {         # The LAST_FH constant is a reference to the variable.         $Carp::{LAST_FH} = \eval '\${^LAST_FH}';     } else {         eval '*LAST_FH = sub () { 0 }';     }
  ;; if ("$]" >= 5.017004)
  (pl-if   (pcl:pl->= $] 5.017004)
    (progn
      ;; $Carp::{LAST_FH} = \eval '\${^LAST_FH}'
            (pcl:pl-setf (pl-gethash (pl-stash "Carp") "LAST_FH") (pl-backslash (pl-eval "\\${^LAST_FH}")))
      
    )
    ;; else
    (progn
      ;; eval '*LAST_FH = sub () { 0 }'
            (pl-eval "*LAST_FH = sub () { 0 }")
      
    )
  )
  
)

;; # Returns a full stack backtrace starting from where it is
;; # told.
;; sub ret_backtrace { ... }
(pl-sub pl-ret_backtrace (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($i (make-pl-box nil)) (@error (make-pl-box nil)) ($mess (make-pl-box nil)) ($err (make-pl-box nil)) ($tid_msg (make-pl-box nil)) ($tid (make-pl-box nil)) (%i (make-pl-box nil)))
        ;; my ( $i, @error ) = @_
                (pcl:pl-setf (vector $i @error) @_)
        
        ;; my $mess (bare declaration)
        
        ;; my $err = join '', @error
                (pcl:pl-setf $err (pl-join "" @error))
        
        ;; $i++
                (pl-post++ $i)
        
        ;; my $tid_msg = ''
                (pcl:pl-setf $tid_msg "")
        
        ;; if ( defined &threads::tid ) {         my $tid = threads->tid;         $tid_msg = " thread $tid" if $tid;     }
        ;; if (defined &threads::tid)
        (pl-if         (pl-defined &threads::tid)
          (progn
            ;; my $tid = threads->tid
                        (pcl:pl-setf $tid (pl-method-call (pl-resolve-invocant "threads") 'tid))
            
            ;; $tid_msg = " thread $tid" if $tid
            (pl-if             $tid             (pcl:pl-setf $tid_msg (pl-string-concat " thread " $tid)))
            
          )
          nil
        )
        
        ;; my %i = caller_info($i)
                (pcl:pl-setf %i (let ((*wantarray* t)) (pl-caller_info $i)))
        
        ;; $mess = "$err at $i{file} line $i{line}$tid_msg"
                (pcl:pl-setf $mess (pl-string-concat $err " at " (pl-gethash %i "file") " line " (pl-gethash %i "line") $tid_msg))
        
        ;; if( $. ) {       # Use ${^LAST_FH} if available.       if (LAST_FH) {         if (${+LAST_FH}) {             $mess .= sprintf ", <%s> %s %d",                               *${+LAST_FH}{NAME},                               ($/ eq "\n" ? "line" : "chunk"), $.         }       }       else {         local $@ = '';         local $SIG{__DIE__};         eval {             CORE::die;         };         if($@ =~ /^Died at .*(, <.*?> (?:line|chunk) \d+).$/ ) {             $mess .= $1;         }       }     }
        ;; if ($.)
        (pl-if         |$.|
          (progn
            ;; if (LAST_FH) {         if (${+LAST_FH}) {             $mess .= sprintf ", <%s> %s %d",                               *${+LAST_FH}{NAME},                               ($/ eq "\n" ? "line" : "chunk"), $.         }       }       else {         local $@ = '';         local $SIG{__DIE__};         eval {             CORE::die;         };         if($@ =~ /^Died at .*(, <.*?> (?:line|chunk) \d+).$/ ) {             $mess .= $1;         }       }
            ;; if (LAST_FH)
            (pl-if             (pl-LAST_FH)
              (progn
                ;; if (${+LAST_FH}) {             $mess .= sprintf ", <%s> %s %d",                               *${+LAST_FH}{NAME},                               ($/ eq "\n" ? "line" : "chunk"), $.         }
                ;; if (${+LAST_FH})
                (pl-if                 (pl-cast-$ (pl-+ (pl-LAST_FH)))
                  (progn
                    ;; $mess .= sprintf ", <%s> %s %d",                               *${+LAST_FH}{NAME},                               ($/ eq "\n" ? "line" : "chunk"), $.
                                        (pcl:pl-.= $mess (pl-sprintf ", <%s> %s %d" (pl-* (pl-cast-$ (pl-gethash (pl-+ (pl-LAST_FH)) "NAME"))) (pl-if (pcl:pl-str-eq |$/| "
") "line" "chunk") |$.|))
                    
                  )
                  nil
                )
                
              )
              ;; else
              (progn
                ;; local $SIG{__DIE__}
                (let (($SIG (make-pl-box nil)))
                  
                  (defun --anon-block-30-- ()
                    ;; CORE::die
                                        (CORE::pl-die)
                    
                  )
                  
                  ;; eval {             CORE::die;         }
                                    (pl-eval-block (funcall #'--anon-block-30--))
                  
                  ;; if($@ =~ /^Died at .*(, <.*?> (?:line|chunk) \d+).$/ ) {             $mess .= $1;         }
                  ;; if ($@ =~ /^Died at .*(, <.*?> (?:line|chunk) \d+).$/)
                  (pl-if                   (pcl:pl-=~ $@ (pl-regex "/^Died at .*(, <.*?> (?:line|chunk) \\d+).$/"))
                    (progn
                      ;; $mess .= $1
                                            (pcl:pl-.= $mess $1)
                      
                    )
                    nil
                  )
                  
                )  ;; end local
              )
            )
            
          )
          nil
        )
        
        ;; $mess .= "\.\n"
                (pcl:pl-.= $mess "\\.
")
        
        ;; while ( my %i = caller_info( ++$i ) ) {         $mess .= "\t$i{sub_name} called at $i{file} line $i{line}$tid_msg\n";     }
        (let ((%i (make-pl-box nil)))
          (pl-while         (pcl:pl-setf %i (let ((*wantarray* t)) (pl-caller_info (pl-pre++ $i))))
            ;; $mess .= "\t$i{sub_name} called at $i{file} line $i{line}$tid_msg\n"
                        (pcl:pl-.= $mess (pl-string-concat "	" (pl-gethash %i "sub_name") " called at " (pl-gethash %i "file") " line " (pl-gethash %i "line") $tid_msg "
"))
            
          )
        )
        
        ;; return $mess
                (pl-return $mess)
        
      )
    )
  )
)

;; sub ret_summary { ... }
(pl-sub pl-ret_summary (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($i (make-pl-box nil)) (@error (make-pl-box nil)) ($err (make-pl-box nil)) ($tid_msg (make-pl-box nil)) ($tid (make-pl-box nil)) (%i (make-pl-box nil)))
        ;; my ( $i, @error ) = @_
                (pcl:pl-setf (vector $i @error) @_)
        
        ;; my $err = join '', @error
                (pcl:pl-setf $err (pl-join "" @error))
        
        ;; $i++
                (pl-post++ $i)
        
        ;; my $tid_msg = ''
                (pcl:pl-setf $tid_msg "")
        
        ;; if ( defined &threads::tid ) {         my $tid = threads->tid;         $tid_msg = " thread $tid" if $tid;     }
        ;; if (defined &threads::tid)
        (pl-if         (pl-defined &threads::tid)
          (progn
            ;; my $tid = threads->tid
                        (pcl:pl-setf $tid (pl-method-call (pl-resolve-invocant "threads") 'tid))
            
            ;; $tid_msg = " thread $tid" if $tid
            (pl-if             $tid             (pcl:pl-setf $tid_msg (pl-string-concat " thread " $tid)))
            
          )
          nil
        )
        
        ;; my %i = caller_info($i)
                (pcl:pl-setf %i (let ((*wantarray* t)) (pl-caller_info $i)))
        
        ;; return "$err at $i{file} line $i{line}$tid_msg\.\n"
                (pl-return (pl-string-concat $err " at " (pl-gethash %i "file") " line " (pl-gethash %i "line") $tid_msg "\\.
"))
        
      )
    )
  )
)

;; sub short_error_loc { ... }
(pl-sub pl-short_error_loc (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($cache (make-pl-box nil)) ($i (make-pl-box nil)) ($lvl (make-pl-box nil)) ($cgc (make-pl-box nil)) ($called (make-pl-box nil)) ($caller (make-pl-box nil)) (@caller (make-pl-box nil)))
        ;; my $cache = {}
                (pcl:pl-setf $cache (pl-hash ))
        
        ;; my $i     = 1
                (pcl:pl-setf $i 1)
        
        ;; my $lvl   = $CarpLevel
                (pcl:pl-setf $lvl $CarpLevel)
        
        ;; { ... }
        (progn
          ;; my $cgc = _cgc()
                    (pcl:pl-setf $cgc (pl-_cgc))
          
          ;; my $called = $cgc ? $cgc->($i) : caller($i)
                    (pcl:pl-setf $called (pl-if $cgc (pl-funcall-ref $cgc $i) (pl-caller $i)))
          
          ;; $i++
                    (pl-post++ $i)
          
          ;; my $caller = $cgc ? $cgc->($i) : caller($i)
                    (pcl:pl-setf $caller (pl-if $cgc (pl-funcall-ref $cgc $i) (pl-caller $i)))
          
          ;; if (!defined($caller)) {             my @caller = $cgc ? $cgc->($i) : caller($i);             if (@caller) {                 # if there's no package but there is other caller info, then                 # the package has been deleted - treat this as a valid package                 # in this case                 redo if defined($called) && $CarpInternal{$called};                 redo unless 0 > --$lvl;                 last;             }             else {                 return 0;             }         }
          ;; if (!defined($caller))
          (pl-if           (pl-! (pl-defined $caller))
            (progn
              ;; my @caller = $cgc ? $cgc->($i) : caller($i)
                            (pcl:pl-setf @caller (pl-if $cgc (let ((*wantarray* t)) (pl-funcall-ref $cgc $i)) (let ((*wantarray* t)) (pl-caller $i))))
              
              ;; if (@caller) {                 # if there's no package but there is other caller info, then                 # the package has been deleted - treat this as a valid package                 # in this case                 redo if defined($called) && $CarpInternal{$called};                 redo unless 0 > --$lvl;                 last;             }             else {                 return 0;             }
              ;; if (@caller)
              (pl-if               @caller
                (progn
                  ;; redo if defined($called) && $CarpInternal{$called}
                  (pl-if                   (pcl:pl-&& (pl-defined $called) (pl-gethash %CarpInternal $called))                   (pl-redo))
                  
                  ;; redo unless 0 > --$lvl
                  (pl-unless                   (pcl:pl-> 0 (pl-pre-- $lvl))                   (pl-redo))
                  
                  ;; last
                                    (pl-last)
                  
                )
                ;; else
                (progn
                  ;; return 0
                                    (pl-return 0)
                  
                )
              )
              
            )
            nil
          )
          
          ;; redo if $Internal{$caller}
          (pl-if           (pl-gethash %Internal $caller)           (pl-redo))
          
          ;; redo if $CarpInternal{$caller}
          (pl-if           (pl-gethash %CarpInternal $caller)           (pl-redo))
          
          ;; redo if $CarpInternal{$called}
          (pl-if           (pl-gethash %CarpInternal $called)           (pl-redo))
          
          ;; redo if trusts( $called, $caller, $cache )
          (pl-if           (pl-trusts $called $caller $cache)           (pl-redo))
          
          ;; redo if trusts( $caller, $called, $cache )
          (pl-if           (pl-trusts $caller $called $cache)           (pl-redo))
          
          ;; redo unless 0 > --$lvl
          (pl-unless           (pcl:pl-> 0 (pl-pre-- $lvl))           (pl-redo))
          
        )
        
        ;; return $i - 1
                (pl-return (pcl:pl-- $i 1))
        
      )
    )
  )
)

;; sub shortmess_heavy { ... }
(pl-sub pl-shortmess_heavy (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($i (make-pl-box nil)))
        ;; return longmess_heavy(@_) if $Verbose
        (pl-if         $Verbose         (pl-return (pl-longmess_heavy @_)))
        
        ;; return @_ if ref( $_[0] )
        (pl-if         (pl-ref (pl-aref @_ 0))         (pl-return @_))
        
        ;; my $i = short_error_loc()
                (pcl:pl-setf $i (pl-short_error_loc))
        
        ;; if ($i) {         ret_summary( $i, @_ );     }     else {         longmess_heavy(@_);     }
        ;; if ($i)
        (pl-if         $i
          (progn
            ;; ret_summary( $i, @_ )
                        (pl-ret_summary $i @_)
            
          )
          ;; else
          (progn
            ;; longmess_heavy(@_)
                        (pl-longmess_heavy @_)
            
          )
        )
        
      )
    )
  )
)

;; # If a string is too long, trims it with ...
;; sub str_len_trim { ... }
(pl-sub pl-str_len_trim (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($str (make-pl-box nil)) ($max (make-pl-box nil)))
        ;; my $str = shift
                (pcl:pl-setf $str (pl-shift @_))
        
        ;; my $max = shift || 0
                (pcl:pl-setf $max (pcl:pl-|| (pl-shift @_) 0))
        
        ;; if ( 2 < $max and $max < length($str) ) {         substr( $str, $max - 3 ) = '...';     }
        ;; if (2 < $max and $max < length($str))
        (pl-if         (pcl:pl-and (pcl:pl-< 2 $max) (pcl:pl-< $max (pl-length $str)))
          (progn
            ;; substr( $str, $max - 3 ) = '...'
                        (pcl:pl-setf (pl-substr $str (pcl:pl-- $max 3)) "...")
            
          )
          nil
        )
        
        ;; return $str
                (pl-return $str)
        
      )
    )
  )
)

;; # Takes two packages and an optional cache.  Says whether the
;; # first inherits from the second.
;; #
;; # Recursive versions of this have to work to avoid certain
;; # possible endless loops, and when following long chains of
;; # inheritance are less efficient.
;; sub trusts { ... }
(pl-sub pl-trusts (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($child (make-pl-box nil)) ($parent (make-pl-box nil)) ($cache (make-pl-box nil)) ($known (make-pl-box nil)) ($partial (make-pl-box nil)) ($anc (make-pl-box nil)) ($anc_knows (make-pl-box nil)) ($anc_partial (make-pl-box nil)) (@found (make-pl-box nil)))
        ;; my $child  = shift
                (pcl:pl-setf $child (pl-shift @_))
        
        ;; my $parent = shift
                (pcl:pl-setf $parent (pl-shift @_))
        
        ;; my $cache  = shift
                (pcl:pl-setf $cache (pl-shift @_))
        
        ;; my ( $known, $partial ) = get_status( $cache, $child )
                (pcl:pl-setf (vector $known $partial) (let ((*wantarray* t)) (pl-get_status $cache $child)))
        
        ;; while ( @$partial and not exists $known->{$parent} ) {         my $anc = shift @$partial;         next if exists $known->{$anc};         $known->{$anc}++;         my ( $anc_knows, $anc_partial ) = get_status( $cache, $anc );         my @found = keys %$anc_knows;         @$known{@found} = ();         push @$partial, @$anc_partial;     }
        (pl-while         (pcl:pl-and (pl-cast-@ $partial) (pl-not (pl-gethash-deref (pl-exists $known) $parent)))
          ;; my $anc = shift @$partial
                    (pcl:pl-setf $anc (pl-shift (pl-cast-@ $partial)))
          
          ;; next if exists $known->{$anc}
          (pl-if           (pl-gethash-deref (pl-exists $known) $anc)           (pl-next))
          
          ;; $known->{$anc}++
                    (pl-post++ (pl-gethash-deref $known $anc))
          
          ;; my ( $anc_knows, $anc_partial ) = get_status( $cache, $anc )
                    (pcl:pl-setf (vector $anc_knows $anc_partial) (let ((*wantarray* t)) (pl-get_status $cache $anc)))
          
          ;; my @found = keys %$anc_knows
                    (pcl:pl-setf @found (let ((*wantarray* t)) (pl-keys (pl-cast-% $anc_knows))))
          
          ;; @$known{@found} = ()
                    (pcl:pl-setf (pl-cast-@ (pl-gethash %known @found)) (progn ))
          
          ;; push @$partial, @$anc_partial
                    (pl-push (pl-cast-@ $partial) (pl-cast-@ $anc_partial))
          
        )
        
        ;; return exists $known->{$parent}
                (pl-return (pl-gethash-deref (pl-exists $known) $parent))
        
      )
    )
  )
)

;; # Takes a package and gives a list of those trusted directly
;; sub trusts_directly { ... }
(pl-sub pl-trusts_directly (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($class (make-pl-box nil)) ($stash (make-pl-box nil)) ($var (make-pl-box nil)))
        ;; my $class = shift
                (pcl:pl-setf $class (pl-shift @_))
        
        ;; no strict 'refs' (no-op)
        
        ;; my $stash = \%{"$class\::"}
                (pcl:pl-setf $stash (pl-backslash (pl-cast-% (pl-string-concat $class "\\::"))))
        
        ;; for my $var (qw/ CARP_NOT ISA /) {         # Don't try using the variable until we know it exists,         # to avoid polluting the caller's namespace.         if ( $stash->{$var} && ref \$stash->{$var} eq 'GLOB'           && *{$stash->{$var}}{ARRAY} && @{$stash->{$var}} ) {            return @{$stash->{$var}}         }     }
        (pl-foreach ($var (vector "CARP_NOT" "ISA"))
          ;; if ( $stash->{$var} && ref \$stash->{$var} eq 'GLOB'           && *{$stash->{$var}}{ARRAY} && @{$stash->{$var}} ) {            return @{$stash->{$var}}         }
          ;; if ($stash->{$var} && ref \$stash->{$var} eq 'GLOB'
;;           && *{$stash->{$var}}{ARRAY} && @{$stash->{$var}})
          (pl-if ;; PARSE ERROR: Fell through. Missing case: [
            (progn
              ;; return @{$stash->{$var}}
                            (pl-return (pl-cast-@ (pl-gethash-deref $stash $var)))
              
            )
            nil
          )
          
        )
        
        ;; return
                (pl-return)
        
      )
    )
  )
)

;; if(!defined($warnings::VERSION) || 	do { no warnings "numeric"; $warnings::VERSION < 1.03 }) {     # Very old versions of warnings.pm import from Carp.  This can go     # wrong due to the circular dependency.  If Carp is invoked before     # warnings, then Carp starts by loading warnings, then warnings     # tries to import from Carp, and gets nothing because Carp is in     # the process of loading and hasn't defined its import method yet.     # So we work around that by manually exporting to warnings here.     no strict "refs";     *{"warnings::$_"} = \&$_ foreach @EXPORT; }
;; if (!defined($warnings::VERSION) ||
;; 	do { no warnings "numeric"; $warnings::VERSION < 1.03 })
(pl-if ;; PARSE ERROR: Fell through. Missing case: [
  (progn
    ;; no strict "refs" (no-op)
    
    ;; *{"warnings::$_"} = \&$_ foreach @EXPORT
    (pl-foreach     @EXPORT     (pcl:pl-setf (pl-* (pl-string-concat "warnings::" $_)) (pl-backslash (pl-bit-and $_))))
    
  )
  nil
)

;; 1
1

;; __END__

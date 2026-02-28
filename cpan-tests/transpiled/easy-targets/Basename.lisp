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
(defpackage :Carp (:use :cl :pcl))

;;; package File::Basename
(defpackage :|File::Basename|
  (:use :cl :pcl))
(in-package :|File::Basename|)
;; Forward declarations: Perl subs can be called before definition,
;; but top-level Lisp code executes immediately. Declare stubs now.
(unless (fboundp 'pl-_strip_trailing_sep) (defun pl-_strip_trailing_sep (&rest args) (declare (ignore args)) nil))
(unless (fboundp 'pl-basename) (defun pl-basename (&rest args) (declare (ignore args)) nil))
(unless (fboundp 'pl-dirname) (defun pl-dirname (&rest args) (declare (ignore args)) nil))
(unless (fboundp 'pl-fileparse) (defun pl-fileparse (&rest args) (declare (ignore args)) nil))
(unless (fboundp 'pl-fileparse_set_fstype) (defun pl-fileparse_set_fstype (&rest args) (declare (ignore args)) nil))

;; CLOS class for MRO
(defclass file-basename () ())

;; # File::Basename is used during the Perl build, when the re extension may
;; # not be available, but we only actually need it if running under tainting.
;; BEGIN {
(eval-when (:compile-toplevel :execute)
  ;; if (${^TAINT}) {     require re;     re->import('taint');   }
  ;; if (${^TAINT})
  (pl-if   ${^TAINT}
    (progn
      ;; require re
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (pl-require "re"))
      
      ;; re->import('taint')
            (pl-method-call (pl-resolve-invocant "re") 'import "taint")
      
    )
    nil
  )
  
)

;; use strict (pragma)

;; use 5.006 (pragma)

;; use warnings (pragma)

;; our(@ISA, @EXPORT, $VERSION, $Fileparse_fstype, $Fileparse_igncase)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar @ISA (make-array 0 :adjustable t :fill-pointer 0)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar @EXPORT (make-array 0 :adjustable t :fill-pointer 0)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $VERSION (make-pl-box nil)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $Fileparse_fstype (make-pl-box nil)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $Fileparse_igncase (make-pl-box nil)))

;; require Exporter (pragma)

;; @ISA = qw(Exporter)
(pl-setf @ISA (vector "Exporter"))

;; @EXPORT = qw(fileparse fileparse_set_fstype basename dirname)
(pl-setf @EXPORT (vector "fileparse" "fileparse_set_fstype" "basename" "dirname"))

;; $VERSION = "2.86"
(pl-setf $VERSION "2.86")

;; fileparse_set_fstype($^O)
(pl-fileparse_set_fstype |$^O|)

;; sub fileparse { ... }
(pl-sub pl-fileparse (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($fullname (make-pl-box nil)) (@suffices (make-pl-box nil)) ($orig_type (make-pl-box nil)) ($type (make-pl-box nil)) ($igncase (make-pl-box nil)) ($taint (make-pl-box nil)) ($dirpath (make-pl-box nil)) ($basename (make-pl-box nil)) ($devspec (make-pl-box nil)) ($remainder (make-pl-box nil)) ($tail (make-pl-box nil)) ($suffix (make-pl-box nil)) ($pat (make-pl-box nil)))
        ;; my($fullname,@suffices) = @_
                (pl-setf (vector $fullname @suffices) @_)
        
        ;; unless (defined $fullname) {       require Carp;       Carp::croak("fileparse(): need a valid pathname");   }
        ;; unless (defined $fullname)
        (pl-if (pl-not         (pl-defined $fullname))
          (progn
            ;; require Carp
            (eval-when (:compile-toplevel :load-toplevel :execute)
              (pl-require "Carp"))
            
            ;; Carp::croak("fileparse(): need a valid pathname")
                        (Carp::pl-croak "fileparse(): need a valid pathname")
            
          )
          nil
        )
        
        ;; my $orig_type = ''
                (pl-setf $orig_type "")
        
        ;; my($type,$igncase) = ($Fileparse_fstype, $Fileparse_igncase)
                (pl-setf (vector $type $igncase) (vector $Fileparse_fstype $Fileparse_igncase))
        
        ;; my($taint) = substr($fullname,0,0)
                (pl-setf (vector $taint) (let ((*wantarray* t)) (pl-substr $fullname 0 0)))
        
        ;; if ($type eq "VMS" and $fullname =~ m{/} ) {     # We're doing Unix emulation     $orig_type = $type;     $type = 'Unix';   }
        ;; if ($type eq "VMS" and $fullname =~ m{/})
        (pl-if         (pl-and (pl-str-eq $type "VMS") (pl-=~ $fullname (pl-regex "m{/}")))
          (progn
            ;; $orig_type = $type
                        (pl-setf $orig_type $type)
            
            ;; $type = 'Unix'
                        (pl-setf $type "Unix")
            
          )
          nil
        )
        
        ;; my($dirpath, $basename) (bare declaration)
        
        ;; if (grep { $type eq $_ } qw(MSDOS DOS MSWin32 Epoc)) {     ($dirpath,$basename) = ($fullname =~ /^((?:.*[:\\\/])?)(.*)/s);     $dirpath .= '.\\' unless $dirpath =~ /[\\\/]\z/;   }   elsif ($type eq "OS2") {     ($dirpath,$basename) = ($fullname =~ m#^((?:.*[:\\/])?)(.*)#s);     $dirpath = './' unless $dirpath;	# Can't be 0     $dirpath .= '/' unless $dirpath =~ m#[\\/]\z#;   }   elsif ($type eq "MacOS") {     ($dirpath,$basename) = ($fullname =~ /^(.*:)?(.*)/s);     $dirpath = ':' unless $dirpath;   }   elsif ($type eq "AmigaOS") {     ($dirpath,$basename) = ($fullname =~ /(.*[:\/])?(.*)/s);     $dirpath = './' unless $dirpath;   }   elsif ($type eq 'VMS' ) {     ($dirpath,$basename) = ($fullname =~ /^(.*[:>\]])?(.*)/s);     $dirpath ||= '';  # should always be defined   }   else { # Default to Unix semantics.     ($dirpath,$basename) = ($fullname =~ m{^(.*/)?(.*)}s);     if ($orig_type eq 'VMS' and $fullname =~ m{^(/[^/]+/000000(/|$))(.*)}) {       # dev:[000000] is top of VMS tree, similar to Unix '/'       # so strip it off and treat the rest as "normal"       my $devspec  = $1;       my $remainder = $3;       ($dirpath,$basename) = ($remainder =~ m{^(.*/)?(.*)}s);       $dirpath ||= '';  # should always be defined       $dirpath = $devspec.$dirpath;     }     $dirpath = './' unless $dirpath;   }
        ;; if (grep { $type eq $_ } qw(MSDOS DOS MSWin32 Epoc))
        (pl-if         (pl-grep (lambda ($_)
  ;; $type eq $_
    (pl-str-eq $type $_)
  ) (vector "MSDOS" "DOS" "MSWin32" "Epoc"))
          (progn
            ;;      ($dirpath,$basename) = ($fullname =~ /^((?:.*[:\\\/])?)(.*)/s)
                        (pl-setf (vector $dirpath $basename) (let ((*wantarray* t)) (pl-=~ $fullname (pl-regex "/^((?:.*[:\\\\\\/])?)(.*)/s"))))
            
            ;; $dirpath .= '.\\' unless $dirpath =~ /[\\\/]\z/
            (pl-unless             (pl-=~ $dirpath (pl-regex "/[\\\\\\/]\\z/"))             (pl-.= $dirpath ".\\"))
            
          )
          ;; elsif ($type eq "OS2")
          (pl-if           (pl-str-eq $type "OS2")
            (progn
              ;;      ($dirpath,$basename) = ($fullname =~ m#^((?:.*[:\\/])?)(.*)#s)
                            (pl-setf (vector $dirpath $basename) (let ((*wantarray* t)) (pl-=~ $fullname (pl-regex "m#^((?:.*[:\\\\/])?)(.*)#s"))))
              
              ;; $dirpath = './' unless $dirpath
              (pl-unless               $dirpath               (pl-setf $dirpath "./"))
              
              ;; $dirpath .= '/' unless $dirpath =~ m#[\\/]\z#
              (pl-unless               (pl-=~ $dirpath (pl-regex "m#[\\\\/]\\z#"))               (pl-.= $dirpath "/"))
              
            )
            ;; elsif ($type eq "MacOS")
            (pl-if             (pl-str-eq $type "MacOS")
              (progn
                ;;      ($dirpath,$basename) = ($fullname =~ /^(.*:)?(.*)/s)
                                (pl-setf (vector $dirpath $basename) (let ((*wantarray* t)) (pl-=~ $fullname (pl-regex "/^(.*:)?(.*)/s"))))
                
                ;; $dirpath = ':' unless $dirpath
                (pl-unless                 $dirpath                 (pl-setf $dirpath ":"))
                
              )
              ;; elsif ($type eq "AmigaOS")
              (pl-if               (pl-str-eq $type "AmigaOS")
                (progn
                  ;;      ($dirpath,$basename) = ($fullname =~ /(.*[:\/])?(.*)/s)
                                    (pl-setf (vector $dirpath $basename) (let ((*wantarray* t)) (pl-=~ $fullname (pl-regex "/(.*[:\\/])?(.*)/s"))))
                  
                  ;; $dirpath = './' unless $dirpath
                  (pl-unless                   $dirpath                   (pl-setf $dirpath "./"))
                  
                )
                ;; elsif ($type eq 'VMS')
                (pl-if                 (pl-str-eq $type "VMS")
                  (progn
                    ;;      ($dirpath,$basename) = ($fullname =~ /^(.*[:>\]])?(.*)/s)
                                        (pl-setf (vector $dirpath $basename) (let ((*wantarray* t)) (pl-=~ $fullname (pl-regex "/^(.*[:>\\]])?(.*)/s"))))
                    
                    ;; $dirpath ||= ''
                                        (pl-or-assign $dirpath "")
                    
                  )
                  ;; else
                  (progn
                    ;;  # Default to Unix semantics.     ($dirpath,$basename) = ($fullname =~ m{^(.*/)?(.*)}s)
                                        (pl-setf (vector $dirpath $basename) (let ((*wantarray* t)) (pl-=~ $fullname (pl-regex "m{^(.*/)?(.*)}s"))))
                    
                    ;; if ($orig_type eq 'VMS' and $fullname =~ m{^(/[^/]+/000000(/|$))(.*)}) {       # dev:[000000] is top of VMS tree, similar to Unix '/'       # so strip it off and treat the rest as "normal"       my $devspec  = $1;       my $remainder = $3;       ($dirpath,$basename) = ($remainder =~ m{^(.*/)?(.*)}s);       $dirpath ||= '';  # should always be defined       $dirpath = $devspec.$dirpath;     }
                    ;; if ($orig_type eq 'VMS' and $fullname =~ m{^(/[^/]+/000000(/|$))(.*)})
                    (pl-if                     (pl-and (pl-str-eq $orig_type "VMS") (pl-=~ $fullname (pl-regex "m{^(/[^/]+/000000(/|$))(.*)}")))
                      (progn
                        ;; my $devspec  = $1
                                                (pl-setf $devspec $1)
                        
                        ;; my $remainder = $3
                                                (pl-setf $remainder $3)
                        
                        ;;        ($dirpath,$basename) = ($remainder =~ m{^(.*/)?(.*)}s)
                                                (pl-setf (vector $dirpath $basename) (let ((*wantarray* t)) (pl-=~ $remainder (pl-regex "m{^(.*/)?(.*)}s"))))
                        
                        ;; $dirpath ||= ''
                                                (pl-or-assign $dirpath "")
                        
                        ;; $dirpath = $devspec.$dirpath
                                                (pl-setf $dirpath (pl-. $devspec $dirpath))
                        
                      )
                      nil
                    )
                    
                    ;; $dirpath = './' unless $dirpath
                    (pl-unless                     $dirpath                     (pl-setf $dirpath "./"))
                    
                  )
                )
              )
            )
          )
        )
        
        ;; my $tail   = ''
                (pl-setf $tail "")
        
        ;; my $suffix = ''
                (pl-setf $suffix "")
        
        ;; if (@suffices) {     foreach $suffix (@suffices) {       my $pat = ($igncase ? '(?i)' : '') . "($suffix)\$";       if ($basename =~ s/$pat//s) {         $taint .= substr($suffix,0,0);         $tail = $1 . $tail;       }     }   }
        ;; if (@suffices)
        (pl-if         @suffices
          (progn
            ;; foreach $suffix (@suffices) {       my $pat = ($igncase ? '(?i)' : '') . "($suffix)\$";       if ($basename =~ s/$pat//s) {         $taint .= substr($suffix,0,0);         $tail = $1 . $tail;       }     }
            (pl-foreach ($suffix (vector             @suffices))
              ;; my $pat = ($igncase ? '(?i)' : '') . "($suffix)\$"
                            (pl-setf $pat (pl-. (pl-if $igncase "(?i)" "") (pl-string-concat "(" $suffix ")\\" "$")))
              
              ;; if ($basename =~ s/$pat//s) {         $taint .= substr($suffix,0,0);         $tail = $1 . $tail;       }
              ;; if ($basename =~ s/$pat//s)
              (pl-if               (pl-=~ $basename (pl-subst "$pat" "" :s))
                (progn
                  ;; $taint .= substr($suffix,0,0)
                                    (pl-.= $taint (pl-substr $suffix 0 0))
                  
                  ;; $tail = $1 . $tail
                                    (pl-setf $tail (pl-. $1 $tail))
                  
                )
                nil
              )
              
            )
            
          )
          nil
        )
        
        ;; $tail .= $taint
                (pl-.= $tail $taint)
        
        ;; wantarray ? ($basename .= $taint, $dirpath .= $taint, $tail)             : ($basename .= $taint)
                (pl-if (pl-wantarray) (vector (pl-.= $basename $taint) (pl-.= $dirpath $taint) $tail) (pl-.= $basename $taint))
        
      )
    )
  )
)

;; sub basename { ... }
(pl-sub pl-basename (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($path (make-pl-box nil)) ($basename (make-pl-box nil)) ($dirname (make-pl-box nil)) ($suffix (make-pl-box nil)))
        ;; my($path) = shift
                (pl-setf (vector $path) (let ((*wantarray* t)) (pl-shift @_)))
        
        ;; _strip_trailing_sep($path)
                (pl-_strip_trailing_sep $path)
        
        ;; my($basename, $dirname, $suffix) = fileparse( $path, map("\Q$_\E",@_) )
                (pl-setf (vector $basename $dirname $suffix) (let ((*wantarray* t)) (pl-fileparse $path (pl-map (lambda ($_) (pl-string-concat "\\Q" $_ "\\E")) @_))))
        
        ;; if( length $suffix and !length $basename ) {       $basename = $suffix;   }
        ;; if (length $suffix and !length $basename)
        (pl-if         (pl-and (pl-length $suffix) (pl-! (pl-length $basename)))
          (progn
            ;; $basename = $suffix
                        (pl-setf $basename $suffix)
            
          )
          nil
        )
        
        ;; if( !length $basename ) {       $basename = $dirname;   }
        ;; if (!length $basename)
        (pl-if         (pl-! (pl-length $basename))
          (progn
            ;; $basename = $dirname
                        (pl-setf $basename $dirname)
            
          )
          nil
        )
        
        ;; return $basename
                (pl-return $basename)
        
      )
    )
  )
)

;; sub dirname { ... }
(pl-sub pl-dirname (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($path (make-pl-box nil)) ($type (make-pl-box nil)) ($basename (make-pl-box nil)) ($dirname (make-pl-box nil)))
        ;; my $path = shift
                (pl-setf $path (pl-shift @_))
        
        ;; my($type) = $Fileparse_fstype
                (pl-setf (vector $type) $Fileparse_fstype)
        
        ;; if( $type eq 'VMS' and $path =~ m{/} ) {         # Parse as Unix         local($File::Basename::Fileparse_fstype) = '';         return dirname($path);     }
        ;; if ($type eq 'VMS' and $path =~ m{/})
        (pl-if         (pl-and (pl-str-eq $type "VMS") (pl-=~ $path (pl-regex "m{/}")))
          (progn
            ;; local($File::Basename::Fileparse_fstype) = ''
            (let ((|File::Basename|::$Fileparse_fstype (make-pl-box             "")))
              
              ;; return dirname($path)
                            (pl-return (pl-dirname $path))
              
            )  ;; end local
          )
          nil
        )
        
        ;; my($basename, $dirname) = fileparse($path)
                (pl-setf (vector $basename $dirname) (let ((*wantarray* t)) (pl-fileparse $path)))
        
        ;; if ($type eq 'VMS') {          $dirname ||= $ENV{DEFAULT};     }     elsif ($type eq 'MacOS') { 	if( !length($basename) && $dirname !~ /^[^:]+:\z/) {             _strip_trailing_sep($dirname); 	    ($basename,$dirname) = fileparse $dirname; 	} 	$dirname .= ":" unless $dirname =~ /:\z/;     }     elsif (grep { $type eq $_ } qw(MSDOS DOS MSWin32 OS2)) {          _strip_trailing_sep($dirname);         unless( length($basename) ) { 	    ($basename,$dirname) = fileparse $dirname; 	    _strip_trailing_sep($dirname); 	}     }     elsif ($type eq 'AmigaOS') {         if ( $dirname =~ /:\z/) { return $dirname }         chop $dirname;         $dirname =~ s{[^:/]+\z}{} unless length($basename);     }     else {         _strip_trailing_sep($dirname);         unless( length($basename) ) { 	    ($basename,$dirname) = fileparse $dirname; 	    _strip_trailing_sep($dirname); 	}     }
        ;; if ($type eq 'VMS')
        (pl-if         (pl-str-eq $type "VMS")
          (progn
            ;; $dirname ||= $ENV{DEFAULT}
                        (pl-or-assign $dirname (pl-gethash %ENV "DEFAULT"))
            
          )
          ;; elsif ($type eq 'MacOS')
          (pl-if           (pl-str-eq $type "MacOS")
            (progn
              ;; if( !length($basename) && $dirname !~ /^[^:]+:\z/) {             _strip_trailing_sep($dirname); 	    ($basename,$dirname) = fileparse $dirname; 	}
              ;; if (!length($basename) && $dirname !~ /^[^:]+:\z/)
              (pl-if               (pl-&& (pl-! (pl-length $basename)) (pl-!~ $dirname (pl-regex "/^[^:]+:\\z/")))
                (progn
                  ;; _strip_trailing_sep($dirname)
                                    (pl-_strip_trailing_sep $dirname)
                  
                  ;;  	    ($basename,$dirname) = fileparse $dirname
                                    (pl-setf (vector $basename $dirname) (let ((*wantarray* t)) (pl-fileparse $dirname)))
                  
                )
                nil
              )
              
              ;; $dirname .= ":" unless $dirname =~ /:\z/
              (pl-unless               (pl-=~ $dirname (pl-regex "/:\\z/"))               (pl-.= $dirname ":"))
              
            )
            ;; elsif (grep { $type eq $_ } qw(MSDOS DOS MSWin32 OS2))
            (pl-if             (pl-grep (lambda ($_)
  ;; $type eq $_
    (pl-str-eq $type $_)
  ) (vector "MSDOS" "DOS" "MSWin32" "OS2"))
              (progn
                ;; _strip_trailing_sep($dirname)
                                (pl-_strip_trailing_sep $dirname)
                
                ;; unless( length($basename) ) { 	    ($basename,$dirname) = fileparse $dirname; 	    _strip_trailing_sep($dirname); 	}
                ;; unless (length($basename))
                (pl-if (pl-not                 (pl-length $basename))
                  (progn
                    ;;  	    ($basename,$dirname) = fileparse $dirname
                                        (pl-setf (vector $basename $dirname) (let ((*wantarray* t)) (pl-fileparse $dirname)))
                    
                    ;; _strip_trailing_sep($dirname)
                                        (pl-_strip_trailing_sep $dirname)
                    
                  )
                  nil
                )
                
              )
              ;; elsif ($type eq 'AmigaOS')
              (pl-if               (pl-str-eq $type "AmigaOS")
                (progn
                  ;; if ( $dirname =~ /:\z/) { return $dirname }
                  ;; if ($dirname =~ /:\z/)
                  (pl-if                   (pl-=~ $dirname (pl-regex "/:\\z/"))
                    (progn
                      ;; return $dirname
                                            (pl-return $dirname)
                      
                    )
                    nil
                  )
                  
                  ;; chop $dirname
                                    (pl-chop $dirname)
                  
                  ;; $dirname =~ s{[^:/]+\z}{} unless length($basename)
                  (pl-unless                   (pl-length $basename)                   (pl-=~ $dirname (pl-subst "[^:/]+\\z" "")))
                  
                )
                ;; else
                (progn
                  ;; _strip_trailing_sep($dirname)
                                    (pl-_strip_trailing_sep $dirname)
                  
                  ;; unless( length($basename) ) { 	    ($basename,$dirname) = fileparse $dirname; 	    _strip_trailing_sep($dirname); 	}
                  ;; unless (length($basename))
                  (pl-if (pl-not                   (pl-length $basename))
                    (progn
                      ;;  	    ($basename,$dirname) = fileparse $dirname
                                            (pl-setf (vector $basename $dirname) (let ((*wantarray* t)) (pl-fileparse $dirname)))
                      
                      ;; _strip_trailing_sep($dirname)
                                            (pl-_strip_trailing_sep $dirname)
                      
                    )
                    nil
                  )
                  
                )
              )
            )
          )
        )
        
        ;; $dirname
                $dirname
        
      )
    )
  )
)

;; # Strip the trailing path separator.
;; sub _strip_trailing_sep  { ... }
(pl-sub pl-_strip_trailing_sep (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($type (make-pl-box nil)))
        ;; my $type = $Fileparse_fstype
                (pl-setf $type $Fileparse_fstype)
        
        ;; if ($type eq 'MacOS') {         $_[0] =~ s/([^:]):\z/$1/s;     }     elsif (grep { $type eq $_ } qw(MSDOS DOS MSWin32 OS2)) {          $_[0] =~ s/([^:])[\\\/]*\z/$1/;     }     else {         $_[0] =~ s{(.)/*\z}{$1}s;     }
        ;; if ($type eq 'MacOS')
        (pl-if         (pl-str-eq $type "MacOS")
          (progn
            ;; $_[0] =~ s/([^:]):\z/$1/s
                        (pl-=~ (pl-aref @_ 0) (pl-subst "([^:]):\\z" "$1" :s))
            
          )
          ;; elsif (grep { $type eq $_ } qw(MSDOS DOS MSWin32 OS2))
          (pl-if           (pl-grep (lambda ($_)
  ;; $type eq $_
    (pl-str-eq $type $_)
  ) (vector "MSDOS" "DOS" "MSWin32" "OS2"))
            (progn
              ;; $_[0] =~ s/([^:])[\\\/]*\z/$1/
                            (pl-=~ (pl-aref @_ 0) (pl-subst "([^:])[\\\\\\/]*\\z" "$1"))
              
            )
            ;; else
            (progn
              ;; $_[0] =~ s{(.)/*\z}{$1}s
                            (pl-=~ (pl-aref @_ 0) (pl-subst "(.)/*\\z" "$1" :s))
              
            )
          )
        )
        
      )
    )
  )
)

;; BEGIN {
(eval-when (:compile-toplevel :execute)
  ;; my @Ignore_Case = qw(MacOS VMS AmigaOS OS2 RISCOS MSWin32 MSDOS DOS Epoc)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defvar @Ignore_Case (make-array 0 :adjustable t :fill-pointer 0)))
    (pl-setf @Ignore_Case (vector "MacOS" "VMS" "AmigaOS" "OS2" "RISCOS" "MSWin32" "MSDOS" "DOS" "Epoc"))
  
  ;; my @Types = (@Ignore_Case, qw(Unix))
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defvar @Types (make-array 0 :adjustable t :fill-pointer 0)))
    (pl-setf @Types (vector @Ignore_Case (vector "Unix")))
  
  ;; sub fileparse_set_fstype { ... }
  (pl-sub pl-fileparse_set_fstype (&rest %_args)
    (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
      (block nil
        (let (($old (make-pl-box nil)) ($new_type (make-pl-box nil)) ($type (make-pl-box nil)))
          ;; my $old = $Fileparse_fstype
                    (pl-setf $old $Fileparse_fstype)
          
          ;; if (@_) {         my $new_type = shift;          $Fileparse_fstype = 'Unix';  # default         foreach my $type (@Types) {             $Fileparse_fstype = $type if $new_type =~ /^$type/i;         }          $Fileparse_igncase =            (grep $Fileparse_fstype eq $_, @Ignore_Case) ? 1 : 0;     }
          ;; if (@_)
          (pl-if           @_
            (progn
              ;; my $new_type = shift
                            (pl-setf $new_type (pl-shift @_))
              
              ;; $Fileparse_fstype = 'Unix'
                            (pl-setf $Fileparse_fstype "Unix")
              
              ;; foreach my $type (@Types) {             $Fileparse_fstype = $type if $new_type =~ /^$type/i;         }
              (pl-foreach ($type (vector               @Types))
                ;; $Fileparse_fstype = $type if $new_type =~ /^$type/i
                (pl-if                 (pl-=~ $new_type (pl-regex "/^$type/i"))                 (pl-setf $Fileparse_fstype $type))
                
              )
              
              ;; $Fileparse_igncase =            (grep $Fileparse_fstype eq $_, @Ignore_Case) ? 1 : 0
                            (pl-setf $Fileparse_igncase (pl-if (pl-grep (lambda ($_) (pl-str-eq $Fileparse_fstype $_)) @Ignore_Case) 1 0))
              
            )
            nil
          )
          
          ;; return $old
                    (pl-return $old)
          
        )
      )
    )
  )
  
)

;; 1
1


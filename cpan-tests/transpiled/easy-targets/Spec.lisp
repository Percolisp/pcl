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

;;; package File::Spec
(defpackage :|File::Spec|
  (:use :cl :pcl))
(in-package :|File::Spec|)
;; CLOS class for MRO
(defclass file-spec () ())

;; use strict (pragma)

;; # Keep $VERSION consistent in all *.pm files in this distribution, including
;; # Cwd.pm.
;; our $VERSION = '3.91'
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $VERSION (make-pl-box nil)))
(setf (pl-box-value $VERSION) "3.91")

;; $VERSION =~ tr/_//d
(pcl:pl-=~ $VERSION (pl-tr "_" "" :d))

;; my %module = ( 	      MSWin32 => 'Win32', 	      os2     => 'OS2', 	      VMS     => 'VMS', 	      NetWare => 'Win32', # Yes, File::Spec::Win32 works on NetWare. 	      symbian => 'Win32', # Yes, File::Spec::Win32 works on symbian. 	      dos     => 'OS2',   # Yes, File::Spec::OS2 works on DJGPP. 	      cygwin  => 'Cygwin', 	      amigaos => 'AmigaOS')
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar %module (make-hash-table :test 'equal)))
(pcl:pl-setf %module (pl-hash "MSWin32" "Win32" "os2" "OS2" "VMS" "VMS" "NetWare" "Win32" "symbian" "Win32" "dos" "OS2" "cygwin" "Cygwin" "amigaos" "AmigaOS"))

;; my $module = $module{$^O} || 'Unix'
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $module (make-pl-box nil)))
(box-set $module (pcl:pl-|| (pl-gethash %module |$^O|) "Unix"))

;; require "File/Spec/$module.pm" (pragma)

;; our @ISA = ("File::Spec::$module")
;; Redefine CLOS class with parents for MRO
(defclass file-spec (file-spec-$module) ())
(defvar @ISA (make-array 0 :adjustable t :fill-pointer 0))
(pl-push @ISA "File::Spec::$module")

;; 1
1

;; __END__

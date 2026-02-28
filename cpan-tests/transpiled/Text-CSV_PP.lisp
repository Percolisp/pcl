(in-package :pcl)
(setf pcl::*pcl-pl2cl-path* #P"/home/bernt/pcl/pl2cl")
;; Initialize @INC from Perl
(setf pcl::@INC (make-array 0 :adjustable t :fill-pointer 0))
(vector-push-extend "/home/bernt/pcl/cpan-tests/modules/Text-CSV-2.04/lib/Text" pcl::@INC)
(vector-push-extend "." pcl::@INC)
(vector-push-extend "." pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.2/lib/site_perl/5.40.2/x86_64-linux" pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.2/lib/site_perl/5.40.2" pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.2/lib/5.40.2/x86_64-linux" pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.2/lib/5.40.2" pcl::@INC)

;; Pre-declare package for dynamic loading
(defpackage :Carp (:use :cl :pcl))

;; Pre-declare package for dynamic loading
(defpackage :Encode (:use :cl :pcl))

;; Pre-declare package for dynamic loading
(defpackage :PerlIO (:use :cl :pcl))

;; Pre-declare package for dynamic loading
(defpackage :|Scalar::Util| (:use :cl :pcl))

;; Pre-declare package for dynamic loading
(defpackage :UNIVERSAL (:use :cl :pcl))

;; Pre-declare package for dynamic loading
(defpackage :bytes (:use :cl :pcl))

;; Pre-declare package for dynamic loading
(defpackage :utf8 (:use :cl :pcl))

;;; package Text::CSV_PP
(defpackage :|Text::CSV_PP|
  (:use :cl :pcl))
(in-package :|Text::CSV_PP|)
;; CLOS class for MRO
(defclass text-csv_pp () ())

;; ################################################################################
;; #
;; # Text::CSV_PP - Text::CSV_XS compatible pure-Perl module
;; #
;; ################################################################################
;; require 5.006001 (pragma)

;; use strict (pragma)

;; use Exporter () (pragma)

;; use vars qw($VERSION @ISA @EXPORT_OK %EXPORT_TAGS)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pl-use "vars" :imports '("$VERSION" "@ISA" "@EXPORT_OK" "%EXPORT_TAGS")))

;; use Carp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pl-use "Carp"))

;; $VERSION = '2.04'
(pcl:pl-setf $VERSION "2.04")

;; @ISA = qw(Exporter)
(pcl:pl-setf @ISA (vector "Exporter"))

;; sub PV  { ... }
(pl-sub pl-PV (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; 0
            0
      
    )
  )
)

;; sub IV  { ... }
(pl-sub pl-IV (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; 1
            1
      
    )
  )
)

;; sub NV  { ... }
(pl-sub pl-NV (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; 2
            2
      
    )
  )
)

;; sub CSV_TYPE_PV { ... }
(pl-sub pl-CSV_TYPE_PV (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; PV
            (pl-PV)
      
    )
  )
)

;; sub CSV_TYPE_IV { ... }
(pl-sub pl-CSV_TYPE_IV (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; IV
            (pl-IV)
      
    )
  )
)

;; sub CSV_TYPE_NV { ... }
(pl-sub pl-CSV_TYPE_NV (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; NV
            (pl-NV)
      
    )
  )
)

;; sub IS_QUOTED () { ... }
(pl-sub pl-IS_QUOTED (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; 0x0001
            0x0001
      
    )
  )
)

;; sub IS_BINARY () { ... }
(pl-sub pl-IS_BINARY (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; 0x0002
            0x0002
      
    )
  )
)

;; sub IS_ERROR ()  { ... }
(pl-sub pl-IS_ERROR (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; 0x0004
            0x0004
      
    )
  )
)

;; sub IS_MISSING () { ... }
(pl-sub pl-IS_MISSING (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; 0x0010
            0x0010
      
    )
  )
)

;; sub CSV_FLAGS_IS_QUOTED      { ... }
(pl-sub pl-CSV_FLAGS_IS_QUOTED (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; IS_QUOTED
            (pl-IS_QUOTED)
      
    )
  )
)

;; sub CSV_FLAGS_IS_BINARY      { ... }
(pl-sub pl-CSV_FLAGS_IS_BINARY (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; IS_BINARY
            (pl-IS_BINARY)
      
    )
  )
)

;; sub CSV_FLAGS_ERROR_IN_FIELD { ... }
(pl-sub pl-CSV_FLAGS_ERROR_IN_FIELD (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; IS_ERROR
            (pl-IS_ERROR)
      
    )
  )
)

;; sub CSV_FLAGS_IS_MISSING     { ... }
(pl-sub pl-CSV_FLAGS_IS_MISSING (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; IS_MISSING
            (pl-IS_MISSING)
      
    )
  )
)

;; sub HOOK_ERROR () { ... }
(pl-sub pl-HOOK_ERROR (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; 0x0001
            0x0001
      
    )
  )
)

;; sub HOOK_AFTER_PARSE () { ... }
(pl-sub pl-HOOK_AFTER_PARSE (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; 0x0002
            0x0002
      
    )
  )
)

;; sub HOOK_BEFORE_PRINT () { ... }
(pl-sub pl-HOOK_BEFORE_PRINT (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; 0x0004
            0x0004
      
    )
  )
)

;; sub useIO_EOF () { ... }
(pl-sub pl-useIO_EOF (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; 0x0010
            0x0010
      
    )
  )
)

;; %EXPORT_TAGS = (     CONSTANTS => [qw(         CSV_FLAGS_IS_QUOTED         CSV_FLAGS_IS_BINARY         CSV_FLAGS_ERROR_IN_FIELD         CSV_FLAGS_IS_MISSING          CSV_TYPE_PV         CSV_TYPE_IV         CSV_TYPE_NV     )], )
(pcl:pl-setf %EXPORT_TAGS (pl-hash "CONSTANTS" (make-array 1 :adjustable t :fill-pointer t :initial-contents (list (vector "CSV_FLAGS_IS_QUOTED" "CSV_FLAGS_IS_BINARY" "CSV_FLAGS_ERROR_IN_FIELD" "CSV_FLAGS_IS_MISSING" "CSV_TYPE_PV" "CSV_TYPE_IV" "CSV_TYPE_NV")))))

;; @EXPORT_OK = (qw(csv PV IV NV), @{$EXPORT_TAGS{'CONSTANTS'}})
(pcl:pl-setf @EXPORT_OK (vector (vector "csv" "PV" "IV" "NV") (pl-cast-@ (pl-gethash %EXPORT_TAGS "CONSTANTS"))))

;; my $ERRORS = {         # Generic errors         1000 => "INI - constructor failed",         1001 => "INI - sep_char is equal to quote_char or escape_char",         1002 => "INI - allow_whitespace with escape_char or quote_char SP or TAB",         1003 => "INI - \\r or \\n in main attr not allowed",         1004 => "INI - callbacks should be undef or a hashref",         1005 => "INI - EOL too long",         1006 => "INI - SEP too long",         1007 => "INI - QUOTE too long",         1008 => "INI - SEP undefined",          1010 => "INI - the header is empty",         1011 => "INI - the header contains more than one valid separator",         1012 => "INI - the header contains an empty field",         1013 => "INI - the header contains nun-unique fields",         1014 => "INI - header called on undefined stream",          # Syntax errors         1500 => "PRM - Invalid/unsupported arguments(s)",         1501 => "PRM - The key attribute is passed as an unsupported type",         1502 => "PRM - The value attribute is passed without the key attribute",         1503 => "PRM - The value attribute is passed as an unsupported type",          # Parse errors         2010 => "ECR - QUO char inside quotes followed by CR not part of EOL",         2011 => "ECR - Characters after end of quoted field",         2012 => "EOF - End of data in parsing input stream",         2013 => "ESP - Specification error for fragments RFC7111",         2014 => "ENF - Inconsistent number of fields",         2015 => "ERW - Empty row",          # EIQ - Error Inside Quotes         2021 => "EIQ - NL char inside quotes, binary off",         2022 => "EIQ - CR char inside quotes, binary off",         2023 => "EIQ - QUO character not allowed",         2024 => "EIQ - EOF cannot be escaped, not even inside quotes",         2025 => "EIQ - Loose unescaped escape",         2026 => "EIQ - Binary character inside quoted field, binary off",         2027 => "EIQ - Quoted field not terminated",          # EIF - Error Inside Field         2030 => "EIF - NL char inside unquoted verbatim, binary off",         2031 => "EIF - CR char is first char of field, not part of EOL",         2032 => "EIF - CR char inside unquoted, not part of EOL",         2034 => "EIF - Loose unescaped quote",         2035 => "EIF - Escaped EOF in unquoted field",         2036 => "EIF - ESC error",         2037 => "EIF - Binary character in unquoted field, binary off",          # Combine errors         2110 => "ECB - Binary character in Combine, binary off",          # IO errors         2200 => "EIO - print to IO failed. See errno",          # Hash-Ref errors         3001 => "EHR - Unsupported syntax for column_names ()",         3002 => "EHR - getline_hr () called before column_names ()",         3003 => "EHR - bind_columns () and column_names () fields count mismatch",         3004 => "EHR - bind_columns () only accepts refs to scalars",         3006 => "EHR - bind_columns () did not pass enough refs for parsed fields",         3007 => "EHR - bind_columns needs refs to writable scalars",         3008 => "EHR - unexpected error in bound fields",         3009 => "EHR - print_hr () called before column_names ()",         3010 => "EHR - print_hr () called with invalid arguments",          4001 => "PRM - The key does not exist as field in the data",          5001 => "PRM - The result does not match the output to append to",         5002 => "PRM - Unsupported output",          0    => "", }
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $ERRORS (make-pl-box nil)))
(box-set $ERRORS (pl-hash 1000 "INI - constructor failed" 1001 "INI - sep_char is equal to quote_char or escape_char" 1002 "INI - allow_whitespace with escape_char or quote_char SP or TAB" 1003 "INI - \\ or \\
 in main attr not allowed" 1004 "INI - callbacks should be undef or a hashref" 1005 "INI - EOL too long" 1006 "INI - SEP too long" 1007 "INI - QUOTE too long" 1008 "INI - SEP undefined" 1010 "INI - the header is empty" 1011 "INI - the header contains more than one valid separator" 1012 "INI - the header contains an empty field" 1013 "INI - the header contains nun-unique fields" 1014 "INI - header called on undefined stream" 1500 "PRM - Invalid/unsupported arguments(s)" 1501 "PRM - The key attribute is passed as an unsupported type" 1502 "PRM - The value attribute is passed without the key attribute" 1503 "PRM - The value attribute is passed as an unsupported type" 2010 "ECR - QUO char inside quotes followed by CR not part of EOL" 2011 "ECR - Characters after end of quoted field" 2012 "EOF - End of data in parsing input stream" 2013 "ESP - Specification error for fragments RFC7111" 2014 "ENF - Inconsistent number of fields" 2015 "ERW - Empty row" 2021 "EIQ - NL char inside quotes, binary off" 2022 "EIQ - CR char inside quotes, binary off" 2023 "EIQ - QUO character not allowed" 2024 "EIQ - EOF cannot be escaped, not even inside quotes" 2025 "EIQ - Loose unescaped escape" 2026 "EIQ - Binary character inside quoted field, binary off" 2027 "EIQ - Quoted field not terminated" 2030 "EIF - NL char inside unquoted verbatim, binary off" 2031 "EIF - CR char is first char of field, not part of EOL" 2032 "EIF - CR char inside unquoted, not part of EOL" 2034 "EIF - Loose unescaped quote" 2035 "EIF - Escaped EOF in unquoted field" 2036 "EIF - ESC error" 2037 "EIF - Binary character in unquoted field, binary off" 2110 "ECB - Binary character in Combine, binary off" 2200 "EIO - print to IO failed. See errno" 3001 "EHR - Unsupported syntax for column_names ()" 3002 "EHR - getline_hr () called before column_names ()" 3003 "EHR - bind_columns () and column_names () fields count mismatch" 3004 "EHR - bind_columns () only accepts refs to scalars" 3006 "EHR - bind_columns () did not pass enough refs for parsed fields" 3007 "EHR - bind_columns needs refs to writable scalars" 3008 "EHR - unexpected error in bound fields" 3009 "EHR - print_hr () called before column_names ()" 3010 "EHR - print_hr () called with invalid arguments" 4001 "PRM - The key does not exist as field in the data" 5001 "PRM - The result does not match the output to append to" 5002 "PRM - Unsupported output" 0 ""))

;; BEGIN {
(eval-when (:compile-toplevel :execute)
  ;; if ( $] < 5.006 ) {         $INC{'bytes.pm'} = 1 unless $INC{'bytes.pm'}; # dummy         no strict 'refs';         *{"utf8::is_utf8"} = sub { 0; };         *{"utf8::decode"}  = sub { };     }     elsif ( $] < 5.008 ) {         no strict 'refs';         *{"utf8::is_utf8"} = sub { 0; };         *{"utf8::decode"}  = sub { };         *{"utf8::encode"}  = sub { };     }     elsif ( !defined &utf8::is_utf8 ) {        require Encode;        *utf8::is_utf8 = *Encode::is_utf8;     }
  ;; if ($] < 5.006)
  (pl-if   (pcl:pl-< $] 5.006)
    (progn
      ;; $INC{'bytes.pm'} = 1 unless $INC{'bytes.pm'}
      (pl-unless       (pl-gethash %INC "bytes.pm")       (pcl:pl-setf (pl-gethash %INC "bytes.pm") 1))
      
      ;; no strict 'refs' (no-op)
      
      (defun --anon-block-65-- ()
        ;; 0
                0
        
      )
      
      ;; *{"utf8::is_utf8"} = sub { 0; }
            (pcl:pl-setf (pl-* "utf8::is_utf8") #'--anon-block-65--)
      
      (defun --anon-block-66-- ()
        nil
      )
      
      ;; *{"utf8::decode"}  = sub { }
            (pcl:pl-setf (pl-* "utf8::decode") #'--anon-block-66--)
      
    )
    ;; elsif ($] < 5.008)
    (pl-if     (pcl:pl-< $] 5.008)
      (progn
        ;; no strict 'refs' (no-op)
        
        (defun --anon-block-67-- ()
          ;; 0
                    0
          
        )
        
        ;; *{"utf8::is_utf8"} = sub { 0; }
                (pcl:pl-setf (pl-* "utf8::is_utf8") #'--anon-block-67--)
        
        (defun --anon-block-68-- ()
          nil
        )
        
        ;; *{"utf8::decode"}  = sub { }
                (pcl:pl-setf (pl-* "utf8::decode") #'--anon-block-68--)
        
        (defun --anon-block-69-- ()
          nil
        )
        
        ;; *{"utf8::encode"}  = sub { }
                (pcl:pl-setf (pl-* "utf8::encode") #'--anon-block-69--)
        
      )
      ;; elsif (!defined &utf8::is_utf8)
      (pl-if       (pl-! (pl-defined &utf8::is_utf8))
        (progn
          ;; require Encode
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (pl-require "Encode"))
          
          ;; *utf8::is_utf8 = *Encode::is_utf8
                    (pcl:pl-setf *utf8::is_utf8 *Encode::is_utf8)
          
        )
        nil
      )
    )
  )
  
  ;; eval q| require Scalar::Util |
  ;; PARSE ERROR: Handle single node of unknown type. Dump:
  
  ;; if ( $@ ) {         eval q| require B |;         if ( $@ ) {             Carp::croak $@;         }         else {             my %tmap = qw(                 B::NULL   SCALAR                 B::HV     HASH                 B::AV     ARRAY                 B::CV     CODE                 B::IO     IO                 B::GV     GLOB                 B::REGEXP REGEXP             );             *Scalar::Util::reftype = sub (\$) {                 my $r = shift;                 return undef unless length(ref($r));                 my $t = ref(B::svref_2object($r));                 return                     exists $tmap{$t} ? $tmap{$t}                   : length(ref($$r)) ? 'REF'                   :                    'SCALAR';             };             *Scalar::Util::readonly = sub (\$) {                 my $b = B::svref_2object( $_[0] );                 $b->FLAGS & 0x00800000; # SVf_READONLY?             };         }     }
  ;; if ($@)
  (pl-if   $@
    (progn
      ;; eval q| require B |
      ;; PARSE ERROR: Handle single node of unknown type. Dump:
      
      ;; if ( $@ ) {             Carp::croak $@;         }         else {             my %tmap = qw(                 B::NULL   SCALAR                 B::HV     HASH                 B::AV     ARRAY                 B::CV     CODE                 B::IO     IO                 B::GV     GLOB                 B::REGEXP REGEXP             );             *Scalar::Util::reftype = sub (\$) {                 my $r = shift;                 return undef unless length(ref($r));                 my $t = ref(B::svref_2object($r));                 return                     exists $tmap{$t} ? $tmap{$t}                   : length(ref($$r)) ? 'REF'                   :                    'SCALAR';             };             *Scalar::Util::readonly = sub (\$) {                 my $b = B::svref_2object( $_[0] );                 $b->FLAGS & 0x00800000; # SVf_READONLY?             };         }
      ;; if ($@)
      (pl-if       $@
        (progn
          ;; Carp::croak $@
                    (Carp::pl-croak $@)
          
        )
        ;; else
        (progn
          ;; my %tmap = qw(                 B::NULL   SCALAR                 B::HV     HASH                 B::AV     ARRAY                 B::CV     CODE                 B::IO     IO                 B::GV     GLOB                 B::REGEXP REGEXP             )
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (defvar %tmap (make-hash-table :test 'equal)))
                    (pcl:pl-setf %tmap (pl-hash "B::NULL" "SCALAR" "B::HV" "HASH" "B::AV" "ARRAY" "B::CV" "CODE" "B::IO" "IO" "B::GV" "GLOB" "B::REGEXP" "REGEXP"))
          
          ;; *Scalar::Util::reftype = sub (\$) {                 my $r = shift;                 return undef unless length(ref($r));                 my $t = ref(B::svref_2object($r));                 return                     exists $tmap{$t} ? $tmap{$t}                   : length(ref($$r)) ? 'REF'                   :                    'SCALAR';             }
          ;; PARSE ERROR: Fell through. Missing case: [
          
          ;; *Scalar::Util::readonly = sub (\$) {                 my $b = B::svref_2object( $_[0] );                 $b->FLAGS & 0x00800000; # SVf_READONLY?             }
          ;; PARSE ERROR: Fell through. Missing case: [
          
        )
      )
      
    )
    nil
  )
  
)

;; ################################################################################
;; #
;; # Common pure perl methods, taken almost directly from Text::CSV_XS.
;; # (These should be moved into a common class eventually, so that
;; # both XS and PP don't need to apply the same changes.)
;; #
;; ################################################################################
;; ################################################################################
;; # version
;; ################################################################################
;; sub version { ... }
(pl-sub pl-version (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; return $VERSION
            (pl-return $VERSION)
      
    )
  )
)

;; ################################################################################
;; # new
;; ################################################################################
;; my %def_attr = (     eol				=> '',     sep_char			=> ',',     quote_char			=> '"',     escape_char			=> '"',     binary			=> 0,     decode_utf8			=> 1,     auto_diag			=> 0,     diag_verbose		=> 0,     strict              => 0,     blank_is_undef		=> 0,     empty_is_undef		=> 0,     allow_whitespace		=> 0,     allow_loose_quotes		=> 0,     allow_loose_escapes		=> 0,     allow_unquoted_escape	=> 0,     always_quote		=> 0,     quote_empty			=> 0,     quote_space			=> 1,     quote_binary		=> 1,     escape_null			=> 1,     keep_meta_info		=> 0,     verbatim			=> 0,     formula			=> 0,     skip_empty_rows => 0,     undef_str			=> undef,     comment_str     => undef,     types			=> undef,     callbacks			=> undef,      _EOF			=> 0,     _RECNO			=> 0,     _STATUS			=> undef,     _FIELDS			=> undef,     _FFLAGS			=> undef,     _STRING			=> undef,     _ERROR_INPUT		=> undef,     _COLUMN_NAMES		=> undef,     _BOUND_COLUMNS		=> undef,     _AHEAD			=> undef,     _FORMULA_CB     => undef,     _EMPTROW_CB     => undef,      ENCODING			=> undef, )
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar %def_attr (make-hash-table :test 'equal)))
(pcl:pl-setf %def_attr (pl-hash "eol" "" "sep_char" "," "quote_char" "\"" "escape_char" "\"" "binary" 0 "decode_utf8" 1 "auto_diag" 0 "diag_verbose" 0 "strict" 0 "blank_is_undef" 0 "empty_is_undef" 0 "allow_whitespace" 0 "allow_loose_quotes" 0 "allow_loose_escapes" 0 "allow_unquoted_escape" 0 "always_quote" 0 "quote_empty" 0 "quote_space" 1 "quote_binary" 1 "escape_null" 1 "keep_meta_info" 0 "verbatim" 0 "formula" 0 "skip_empty_rows" 0 "undef_str" (let ((*wantarray* t)) (pl-undef)) "comment_str" (let ((*wantarray* t)) (pl-undef)) "types" (let ((*wantarray* t)) (pl-undef)) "callbacks" (let ((*wantarray* t)) (pl-undef)) "_EOF" 0 "_RECNO" 0 "_STATUS" (let ((*wantarray* t)) (pl-undef)) "_FIELDS" (let ((*wantarray* t)) (pl-undef)) "_FFLAGS" (let ((*wantarray* t)) (pl-undef)) "_STRING" (let ((*wantarray* t)) (pl-undef)) "_ERROR_INPUT" (let ((*wantarray* t)) (pl-undef)) "_COLUMN_NAMES" (let ((*wantarray* t)) (pl-undef)) "_BOUND_COLUMNS" (let ((*wantarray* t)) (pl-undef)) "_AHEAD" (let ((*wantarray* t)) (pl-undef)) "_FORMULA_CB" (let ((*wantarray* t)) (pl-undef)) "_EMPTROW_CB" (let ((*wantarray* t)) (pl-undef)) "ENCODING" (let ((*wantarray* t)) (pl-undef))))

;; my %attr_alias = (     quote_always		=> "always_quote",     verbose_diag		=> "diag_verbose",     quote_null			=> "escape_null",     escape			=> "escape_char",     comment         => "comment_str",     )
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar %attr_alias (make-hash-table :test 'equal)))
(pcl:pl-setf %attr_alias (pl-hash "quote_always" "always_quote" "verbose_diag" "diag_verbose" "quote_null" "escape_null" "escape" "escape_char" "comment" "comment_str"))

;; my $last_new_error = Text::CSV_PP->SetDiag(0)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $last_new_error (make-pl-box nil)))
(box-set $last_new_error (pl-method-call "Text::CSV_PP" 'SetDiag 0))

;; my $ebcdic         = ord("A") == 0xC1
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $ebcdic (make-pl-box nil)))
(box-set $ebcdic (pcl:pl-== (pl-ord "A") 0xC1))

;; # Faster than $Config{'ebcdic'}
;; my @internal_kh
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar @internal_kh (make-array 0 :adjustable t :fill-pointer 0)))

;; my $last_error
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $last_error (make-pl-box nil)))

;; # NOT a method: is also used before bless
;; sub _unhealthy_whitespace { ... }
(pl-sub pl-_unhealthy_whitespace (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($aw (make-pl-box nil)) ($quo (make-pl-box nil)) ($esc (make-pl-box nil)))
        ;; my ($self, $aw) = @_
                (pcl:pl-setf (vector $self $aw) @_)
        
        ;; $aw or return 0
                (pcl:pl-or $aw (pl-return 0))
        
        ;; my $quo = $self->{quote}
                (pcl:pl-setf $quo (pl-gethash-deref $self "quote"))
        
        ;; defined $quo && length ($quo) or $quo = $self->{quote_char}
                (pcl:pl-or (pcl:pl-&& (pl-defined $quo) (pl-length $quo)) (pcl:pl-setf $quo (pl-gethash-deref $self "quote_char")))
        
        ;; my $esc = $self->{escape_char}
                (pcl:pl-setf $esc (pl-gethash-deref $self "escape_char"))
        
        ;; defined $quo && $quo =~ m/^[ \t]/ and return 1002
                (pcl:pl-and (pcl:pl-&& (pl-defined $quo) (pcl:pl-=~ $quo (pl-regex "m/^[ \\t]/"))) (pl-return 1002))
        
        ;; defined $esc && $esc =~ m/^[ \t]/ and return 1002
                (pcl:pl-and (pcl:pl-&& (pl-defined $esc) (pcl:pl-=~ $esc (pl-regex "m/^[ \\t]/"))) (pl-return 1002))
        
        ;; return 0
                (pl-return 0)
        
      )
    )
  )
)

;; sub _check_sanity { ... }
(pl-sub pl-_check_sanity (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($eol (make-pl-box nil)) ($sep (make-pl-box nil)) ($quo (make-pl-box nil)) ($esc (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; my $eol = $self->{eol}
                (pcl:pl-setf $eol (pl-gethash-deref $self "eol"))
        
        ;; my $sep = $self->{sep}
                (pcl:pl-setf $sep (pl-gethash-deref $self "sep"))
        
        ;; defined $sep && length ($sep) or $sep = $self->{sep_char}
                (pcl:pl-or (pcl:pl-&& (pl-defined $sep) (pl-length $sep)) (pcl:pl-setf $sep (pl-gethash-deref $self "sep_char")))
        
        ;; my $quo = $self->{quote}
                (pcl:pl-setf $quo (pl-gethash-deref $self "quote"))
        
        ;; defined $quo && length ($quo) or $quo = $self->{quote_char}
                (pcl:pl-or (pcl:pl-&& (pl-defined $quo) (pl-length $quo)) (pcl:pl-setf $quo (pl-gethash-deref $self "quote_char")))
        
        ;; my $esc = $self->{escape_char}
                (pcl:pl-setf $esc (pl-gethash-deref $self "escape_char"))
        
        ;; $sep ne ""         or  return 1008
                (pcl:pl-or (pcl:pl-str-ne $sep "") (pl-return 1008))
        
        ;; length ($sep) > 16     and return 1006
                (pcl:pl-and (pcl:pl-> (pl-length $sep) 16) (pl-return 1006))
        
        ;; $sep =~ m/[\r\n]/      and return 1003
                (pcl:pl-and (pcl:pl-=~ $sep (pl-regex "m/[\\r\\n]/")) (pl-return 1003))
        
        ;; if (defined $quo) {         $quo eq $sep        and return 1001;         length ($quo) > 16  and return 1007;         $quo =~ m/[\r\n]/   and return 1003;         }
        ;; if (defined $quo)
        (pl-if         (pl-defined $quo)
          (progn
            ;; $quo eq $sep        and return 1001
                        (pcl:pl-and (pcl:pl-str-eq $quo $sep) (pl-return 1001))
            
            ;; length ($quo) > 16  and return 1007
                        (pcl:pl-and (pcl:pl-> (pl-length $quo) 16) (pl-return 1007))
            
            ;; $quo =~ m/[\r\n]/   and return 1003
                        (pcl:pl-and (pcl:pl-=~ $quo (pl-regex "m/[\\r\\n]/")) (pl-return 1003))
            
          )
          nil
        )
        
        ;; if (defined $esc) {         $esc eq $sep        and return 1001;         $esc =~ m/[\r\n]/   and return 1003;         }
        ;; if (defined $esc)
        (pl-if         (pl-defined $esc)
          (progn
            ;; $esc eq $sep        and return 1001
                        (pcl:pl-and (pcl:pl-str-eq $esc $sep) (pl-return 1001))
            
            ;; $esc =~ m/[\r\n]/   and return 1003
                        (pcl:pl-and (pcl:pl-=~ $esc (pl-regex "m/[\\r\\n]/")) (pl-return 1003))
            
          )
          nil
        )
        
        ;; if (defined $eol) {         length ($eol) > 16  and return 1005;         }
        ;; if (defined $eol)
        (pl-if         (pl-defined $eol)
          (progn
            ;; length ($eol) > 16  and return 1005
                        (pcl:pl-and (pcl:pl-> (pl-length $eol) 16) (pl-return 1005))
            
          )
          nil
        )
        
        ;; return _unhealthy_whitespace ($self, $self->{allow_whitespace})
                (pl-return (pl-_unhealthy_whitespace $self (pl-gethash-deref $self "allow_whitespace")))
        
      )
    )
  )
)

;; sub known_attributes { ... }
(pl-sub pl-known_attributes (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; sort grep !m/^_/ => "sep", "quote", keys %def_attr
            (pl-sort (pl-grep (pl-! (pcl:pl-=~ $_ (pl-regex "m/^_/"))) "sep" "quote" (pl-keys %def_attr)))
      
    )
  )
)

;; sub new { ... }
(pl-sub pl-new (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($proto (make-pl-box nil)) ($class (make-pl-box nil)) ($attr (make-pl-box nil)) (%attr (make-pl-box nil)) ($k (make-pl-box nil)) ($sep_aliased (make-pl-box nil)) ($quote_aliased (make-pl-box nil)) ($attr_formula (make-pl-box nil)) (@b (make-pl-box nil)) ($self (make-pl-box nil)) ($ec (make-pl-box nil)))
        ;; $last_new_error   = Text::CSV_PP->SetDiag(1000,         'usage: my $csv = Text::CSV_PP->new ([{ option => value, ... }]);')
                (pcl:pl-setf $last_new_error (pl-method-call "Text::CSV_PP" 'SetDiag 1000 "usage: my $csv = Text::CSV_PP->new ([{ option => value, ... }]);"))
        
        ;; my $proto = shift
                (pcl:pl-setf $proto (pl-shift @_))
        
        ;; my $class = ref $proto || $proto	or  return
                (pcl:pl-or (pcl:pl-setf $class (pcl:pl-|| (pl-ref $proto) $proto)) (pl-return))
        
        ;; @_ > 0 &&   ref $_[0] ne "HASH"	and return
                (pcl:pl-and (pcl:pl-&& (pcl:pl-> @_ 0) (pcl:pl-str-ne (pl-aref (pl-ref $_) 0) "HASH")) (pl-return))
        
        ;; my $attr  = shift || {}
                (pcl:pl-setf $attr (pcl:pl-|| (pl-shift @_) (pl-hash )))
        
        (defun --anon-block-70-- ($_)
          ;; my $k = m/^[a-zA-Z]\w+$/ ? lc $_ : $_
          ;; PARSE ERROR: Fell through. Missing case: [
          
          ;; exists $attr_alias{$k} and $k = $attr_alias{$k}
                    (pcl:pl-and (pl-exists (pl-gethash %attr_alias $k)) (pcl:pl-setf $k (pl-gethash %attr_alias $k)))
          
          ;;          ($k => $attr->{$_})
                    (progn $k (pl-gethash-deref $attr $_))
          
        )
        
        ;; my %attr  = map {         my $k = m/^[a-zA-Z]\w+$/ ? lc $_ : $_;         exists $attr_alias{$k} and $k = $attr_alias{$k};         ($k => $attr->{$_});         } keys %$attr
                (pcl:pl-setf %attr (let ((*wantarray* t)) (pl-map #'--anon-block-70-- (let ((*wantarray* t)) (pl-keys (pl-cast-% $attr))))))
        
        ;; my $sep_aliased = 0
                (pcl:pl-setf $sep_aliased 0)
        
        ;; if (exists $attr{sep}) {         $attr{sep_char} = delete $attr{sep};         $sep_aliased = 1;         }
        ;; if (exists $attr{"sep"})
        (pl-if         (pl-exists (pl-gethash %attr "sep"))
          (progn
            ;; $attr{sep_char} = delete $attr{sep}
                        (pcl:pl-setf (pl-gethash %attr "sep_char") (pl-delete (pl-gethash %attr "sep")))
            
            ;; $sep_aliased = 1
                        (pcl:pl-setf $sep_aliased 1)
            
          )
          nil
        )
        
        ;; my $quote_aliased = 0
                (pcl:pl-setf $quote_aliased 0)
        
        ;; if (exists $attr{quote}) {         $attr{quote_char} = delete $attr{quote};         $quote_aliased = 1;         }
        ;; if (exists $attr{"quote"})
        (pl-if         (pl-exists (pl-gethash %attr "quote"))
          (progn
            ;; $attr{quote_char} = delete $attr{quote}
                        (pcl:pl-setf (pl-gethash %attr "quote_char") (pl-delete (pl-gethash %attr "quote")))
            
            ;; $quote_aliased = 1
                        (pcl:pl-setf $quote_aliased 1)
            
          )
          nil
        )
        
        ;; exists $attr{formula_handling} and         $attr{formula} = delete $attr{formula_handling}
                (pcl:pl-and (pl-exists (pl-gethash %attr "formula_handling")) (pcl:pl-setf (pl-gethash %attr "formula") (pl-delete (pl-gethash %attr "formula_handling"))))
        
        ;; my $attr_formula = delete $attr{formula}
                (pcl:pl-setf $attr_formula (pl-delete (pl-gethash %attr "formula")))
        
        ;; for (keys %attr) {         if (m/^[a-z]/ && exists $def_attr{$_}) {             # uncoverable condition false             defined $attr{$_} && m/_char$/ and utf8::decode ($attr{$_});             next;             } #        croak?         $last_new_error = Text::CSV_PP->SetDiag(1000, "INI - Unknown attribute '$_'");         $attr{auto_diag} and error_diag ();         return;         }
        (pl-foreach ($_         (pl-keys %attr))
          ;; if (m/^[a-z]/ && exists $def_attr{$_}) {             # uncoverable condition false             defined $attr{$_} && m/_char$/ and utf8::decode ($attr{$_});             next;             }
          ;; if (m/^[a-z]/ && exists $def_attr{$_})
          (pl-if           (pcl:pl-&& (pcl:pl-=~ $_ (pl-regex "m/^[a-z]/")) (pl-exists (pl-gethash %def_attr $_)))
            (progn
              ;; defined $attr{$_} && m/_char$/ and utf8::decode ($attr{$_})
                            (pcl:pl-and (pcl:pl-&& (pl-defined (pl-gethash %attr $_)) (pcl:pl-=~ $_ (pl-regex "m/_char$/"))) (utf8::pl-decode (pl-gethash %attr $_)))
              
              ;; next
                            (pl-next)
              
            )
            nil
          )
          
          ;; $last_new_error = Text::CSV_PP->SetDiag(1000, "INI - Unknown attribute '$_'")
                    (pcl:pl-setf $last_new_error (pl-method-call "Text::CSV_PP" 'SetDiag 1000 (pl-string-concat "INI - Unknown attribute '" $_ "'")))
          
          ;; $attr{auto_diag} and error_diag ()
                    (pcl:pl-and (pl-gethash %attr "auto_diag") (pl-error_diag))
          
          ;; return
                    (pl-return)
          
        )
        
        ;; if ($sep_aliased) {         my @b = unpack "U0C*", $attr{sep_char};         if (@b > 1) {             $attr{sep} = $attr{sep_char};             $attr{sep_char} = "\0";             }         else {             $attr{sep} = undef;             }         }
        ;; if ($sep_aliased)
        (pl-if         $sep_aliased
          (progn
            ;; my @b = unpack "U0C*", $attr{sep_char}
                        (pcl:pl-setf @b (let ((*wantarray* t)) (pl-unpack "U0C*" (pl-gethash %attr "sep_char"))))
            
            ;; if (@b > 1) {             $attr{sep} = $attr{sep_char};             $attr{sep_char} = "\0";             }         else {             $attr{sep} = undef;             }
            ;; if (@b > 1)
            (pl-if             (pcl:pl-> @b 1)
              (progn
                ;; $attr{sep} = $attr{sep_char}
                                (pcl:pl-setf (pl-gethash %attr "sep") (pl-gethash %attr "sep_char"))
                
                ;; $attr{sep_char} = "\0"
                                (pcl:pl-setf (pl-gethash %attr "sep_char") " ")
                
              )
              ;; else
              (progn
                ;; $attr{sep} = undef
                                (pcl:pl-setf (pl-gethash %attr "sep") (pl-undef))
                
              )
            )
            
          )
          nil
        )
        
        ;; if ($quote_aliased and defined $attr{quote_char}) {         my @b = unpack "U0C*", $attr{quote_char};         if (@b > 1) {             $attr{quote} = $attr{quote_char};             $attr{quote_char} = "\0";             }         else {             $attr{quote} = undef;             }         }
        ;; if ($quote_aliased and defined $attr{"quote_char"})
        (pl-if         (pcl:pl-and $quote_aliased (pl-defined (pl-gethash %attr "quote_char")))
          (progn
            ;; my @b = unpack "U0C*", $attr{quote_char}
                        (pcl:pl-setf @b (let ((*wantarray* t)) (pl-unpack "U0C*" (pl-gethash %attr "quote_char"))))
            
            ;; if (@b > 1) {             $attr{quote} = $attr{quote_char};             $attr{quote_char} = "\0";             }         else {             $attr{quote} = undef;             }
            ;; if (@b > 1)
            (pl-if             (pcl:pl-> @b 1)
              (progn
                ;; $attr{quote} = $attr{quote_char}
                                (pcl:pl-setf (pl-gethash %attr "quote") (pl-gethash %attr "quote_char"))
                
                ;; $attr{quote_char} = "\0"
                                (pcl:pl-setf (pl-gethash %attr "quote_char") " ")
                
              )
              ;; else
              (progn
                ;; $attr{quote} = undef
                                (pcl:pl-setf (pl-gethash %attr "quote") (pl-undef))
                
              )
            )
            
          )
          nil
        )
        
        ;; my $self = { %def_attr, %attr }
                (pcl:pl-setf $self (pl-hash %def_attr %attr))
        
        ;; if (my $ec = _check_sanity ($self)) {         $last_new_error   = Text::CSV_PP->SetDiag($ec);         $attr{auto_diag} and error_diag ();         return;         }
        (let (($ec (make-pl-box nil)))
          ;; if (my $ec = _check_sanity ($self))
          (pl-if           (pcl:pl-setf $ec (pl-_check_sanity $self))
            (progn
              ;; $last_new_error   = Text::CSV_PP->SetDiag($ec)
                            (pcl:pl-setf $last_new_error (pl-method-call "Text::CSV_PP" 'SetDiag $ec))
              
              ;; $attr{auto_diag} and error_diag ()
                            (pcl:pl-and (pl-gethash %attr "auto_diag") (pl-error_diag))
              
              ;; return
                            (pl-return)
              
            )
            nil
          )
        )
        
        ;; if (defined $self->{callbacks} && ref $self->{callbacks} ne "HASH") {         Carp::carp "The 'callbacks' attribute is set but is not a hash: ignored\n";         $self->{callbacks} = undef;         }
        ;; if (defined $self->{"callbacks"} && ref $self->{"callbacks"} ne "HASH")
        (pl-if         (pcl:pl-&& (pl-gethash-deref (pl-defined $self) "callbacks") (pcl:pl-str-ne (pl-gethash-deref (pl-ref $self) "callbacks") "HASH"))
          (progn
            ;; Carp::carp "The 'callbacks' attribute is set but is not a hash: ignored\n"
                        (Carp::pl-carp "The 'callbacks' attribute is set but is not a hash: ignored
")
            
            ;; $self->{callbacks} = undef
                        (pcl:pl-setf (pl-gethash-deref $self "callbacks") (pl-undef))
            
          )
          nil
        )
        
        ;; $last_new_error = Text::CSV_PP->SetDiag(0)
                (pcl:pl-setf $last_new_error (pl-method-call "Text::CSV_PP" 'SetDiag 0))
        
        ;; defined $\ && !exists $attr{eol} and $self->{eol} = $\
                (pcl:pl-and (pcl:pl-&& (pl-defined |$\|) (pl-! (pl-exists (pl-gethash %attr "eol")))) (pcl:pl-setf (pl-gethash-deref $self "eol") |$\|))
        
        ;; bless $self, $class
                (pl-bless $self $class)
        
        ;; defined $self->{'types'}           and $self->types($self->{'types'})
                (pcl:pl-and (pl-gethash-deref (pl-defined $self) "types") (pl-method-call $self 'types (pl-gethash-deref $self "types")))
        
        ;; defined $self->{'skip_empty_rows'} and $self->{'skip_empty_rows'} = _supported_skip_empty_rows($self, $self->{'skip_empty_rows'})
                (pcl:pl-and (pl-gethash-deref (pl-defined $self) "skip_empty_rows") (pcl:pl-setf (pl-gethash-deref $self "skip_empty_rows") (pl-_supported_skip_empty_rows $self (pl-gethash-deref $self "skip_empty_rows"))))
        
        ;; defined $attr_formula              and $self->{'formula'}         = _supported_formula($self, $attr_formula)
                (pcl:pl-and (pl-defined $attr_formula) (pcl:pl-setf (pl-gethash-deref $self "formula") (pl-_supported_formula $self $attr_formula)))
        
        ;; $self
                $self
        
      )
    )
  )
)

;; # Keep in sync with XS!
;; my %_cache_id = ( # Only expose what is accessed from within PM     quote_char			=>  0,     escape_char			=>  1,     sep_char			=>  2,     sep				=> 39,	# 39 .. 55     binary			=>  3,     keep_meta_info		=>  4,     always_quote		=>  5,     allow_loose_quotes		=>  6,     allow_loose_escapes		=>  7,     allow_unquoted_escape	=>  8,     allow_whitespace		=>  9,     blank_is_undef		=> 10,     eol				=> 11,     quote			=> 15,     verbatim			=> 22,     empty_is_undef		=> 23,     auto_diag			=> 24,     diag_verbose		=> 33,     quote_space			=> 25,     quote_empty			=> 37,     quote_binary		=> 32,     escape_null			=> 31,     decode_utf8			=> 35,     _has_ahead			=> 30,     _has_hooks			=> 36,     _is_bound			=> 26,	# 26 .. 29     formula			=> 38,     strict   			=> 42,     skip_empty_rows     => 43,     undef_str  		=> 46,     comment_str     => 54,     types           => 62,     )
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar %_cache_id (make-hash-table :test 'equal)))
(pcl:pl-setf %_cache_id (pl-hash "quote_char" 0 "escape_char" 1 "sep_char" 2 "sep" 39 "binary" 3 "keep_meta_info" 4 "always_quote" 5 "allow_loose_quotes" 6 "allow_loose_escapes" 7 "allow_unquoted_escape" 8 "allow_whitespace" 9 "blank_is_undef" 10 "eol" 11 "quote" 15 "verbatim" 22 "empty_is_undef" 23 "auto_diag" 24 "diag_verbose" 33 "quote_space" 25 "quote_empty" 37 "quote_binary" 32 "escape_null" 31 "decode_utf8" 35 "_has_ahead" 30 "_has_hooks" 36 "_is_bound" 26 "formula" 38 "strict" 42 "skip_empty_rows" 43 "undef_str" 46 "comment_str" 54 "types" 62))

;; my %_hidden_cache_id = qw(     sep_len		38     eol_len		12     eol_is_cr		13     quo_len		16     has_error_input		34 )
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar %_hidden_cache_id (make-hash-table :test 'equal)))
(pcl:pl-setf %_hidden_cache_id (pl-hash "sep_len" "38" "eol_len" "12" "eol_is_cr" "13" "quo_len" "16" "has_error_input" "34"))

;; my %_reverse_cache_id = (     map({$_cache_id{$_} => $_} keys %_cache_id),     map({$_hidden_cache_id{$_} => $_} keys %_hidden_cache_id), )
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar %_reverse_cache_id (make-hash-table :test 'equal)))
;; PARSE ERROR: Fell through. Missing case: [

;; # A `character'
;; sub _set_attr_C { ... }
(pl-sub pl-_set_attr_C (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($name (make-pl-box nil)) ($val (make-pl-box nil)) ($ec (make-pl-box nil)))
        ;; my ($self, $name, $val, $ec) = @_
                (pcl:pl-setf (vector $self $name $val $ec) @_)
        
        ;; defined $val and utf8::decode($val)
                (pcl:pl-and (pl-defined $val) (utf8::pl-decode $val))
        
        ;; $self->{$name} = $val
                (pcl:pl-setf (pl-gethash-deref $self $name) $val)
        
        ;; $ec = _check_sanity ($self) and croak ($self->SetDiag ($ec))
                (pcl:pl-and (pcl:pl-setf $ec (pl-_check_sanity $self)) (pl-croak (pl-method-call $self 'SetDiag $ec)))
        
        ;; $self->_cache_set ($_cache_id{$name}, $val)
                (pl-method-call $self '_cache_set (pl-gethash %_cache_id $name) $val)
        
      )
    )
  )
)

;; # A flag
;; sub _set_attr_X { ... }
(pl-sub pl-_set_attr_X (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($name (make-pl-box nil)) ($val (make-pl-box nil)))
        ;; my ($self, $name, $val) = @_
                (pcl:pl-setf (vector $self $name $val) @_)
        
        ;; defined $val or $val = 0
                (pcl:pl-or (pl-defined $val) (pcl:pl-setf $val 0))
        
        ;; $self->{$name} = $val
                (pcl:pl-setf (pl-gethash-deref $self $name) $val)
        
        ;; $self->_cache_set ($_cache_id{$name}, 0 + $val)
                (pl-method-call $self '_cache_set (pl-gethash %_cache_id $name) (pcl:pl-+ 0 $val))
        
      )
    )
  )
)

;; # A number
;; sub _set_attr_N { ... }
(pl-sub pl-_set_attr_N (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($name (make-pl-box nil)) ($val (make-pl-box nil)))
        ;; my ($self, $name, $val) = @_
                (pcl:pl-setf (vector $self $name $val) @_)
        
        ;; $self->{$name} = $val
                (pcl:pl-setf (pl-gethash-deref $self $name) $val)
        
        ;; $self->_cache_set ($_cache_id{$name}, 0 + $val)
                (pl-method-call $self '_cache_set (pl-gethash %_cache_id $name) (pcl:pl-+ 0 $val))
        
      )
    )
  )
)

;; # Accessor methods.
;; #   It is unwise to change them halfway through a single file!
;; sub quote_char { ... }
(pl-sub pl-quote_char (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; if (@_) {         $self->_set_attr_C ("quote_char", shift);         $self->_cache_set ($_cache_id{quote}, "");         }
        ;; if (@_)
        (pl-if         @_
          (progn
            ;; $self->_set_attr_C ("quote_char", shift)
                        (pl-method-call $self '_set_attr_C "quote_char" (pl-shift @_))
            
            ;; $self->_cache_set ($_cache_id{quote}, "")
                        (pl-method-call $self '_cache_set (pl-gethash %_cache_id "quote") "")
            
          )
          nil
        )
        
        ;; $self->{quote_char}
                (pl-gethash-deref $self "quote_char")
        
      )
    )
  )
)

;; sub quote { ... }
(pl-sub pl-quote (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($quote (make-pl-box nil)) (@b (make-pl-box nil)) ($ec (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; if (@_) {         my $quote = shift;         defined $quote or $quote = "";         utf8::decode ($quote);         my @b = unpack "U0C*", $quote;         if (@b > 1) {             @b > 16 and croak ($self->SetDiag (1007));             $self->quote_char ("\0");             }         else {             $self->quote_char ($quote);             $quote = "";             }         $self->{quote} = $quote;          my $ec = _check_sanity ($self);         $ec and croak ($self->SetDiag ($ec));          $self->_cache_set ($_cache_id{quote}, $quote);         }
        ;; if (@_)
        (pl-if         @_
          (progn
            ;; my $quote = shift
                        (pcl:pl-setf $quote (pl-shift @_))
            
            ;; defined $quote or $quote = ""
                        (pcl:pl-or (pl-defined $quote) (pcl:pl-setf $quote ""))
            
            ;; utf8::decode ($quote)
                        (utf8::pl-decode $quote)
            
            ;; my @b = unpack "U0C*", $quote
                        (pcl:pl-setf @b (let ((*wantarray* t)) (pl-unpack "U0C*" $quote)))
            
            ;; if (@b > 1) {             @b > 16 and croak ($self->SetDiag (1007));             $self->quote_char ("\0");             }         else {             $self->quote_char ($quote);             $quote = "";             }
            ;; if (@b > 1)
            (pl-if             (pcl:pl-> @b 1)
              (progn
                ;; @b > 16 and croak ($self->SetDiag (1007))
                                (pcl:pl-and (pcl:pl-> @b 16) (pl-croak (pl-method-call $self 'SetDiag 1007)))
                
                ;; $self->quote_char ("\0")
                                (pl-method-call $self 'quote_char " ")
                
              )
              ;; else
              (progn
                ;; $self->quote_char ($quote)
                                (pl-method-call $self 'quote_char $quote)
                
                ;; $quote = ""
                                (pcl:pl-setf $quote "")
                
              )
            )
            
            ;; $self->{quote} = $quote
                        (pcl:pl-setf (pl-gethash-deref $self "quote") $quote)
            
            ;; my $ec = _check_sanity ($self)
                        (pcl:pl-setf $ec (pl-_check_sanity $self))
            
            ;; $ec and croak ($self->SetDiag ($ec))
                        (pcl:pl-and $ec (pl-croak (pl-method-call $self 'SetDiag $ec)))
            
            ;; $self->_cache_set ($_cache_id{quote}, $quote)
                        (pl-method-call $self '_cache_set (pl-gethash %_cache_id "quote") $quote)
            
          )
          nil
        )
        
        ;; my $quote = $self->{quote}
                (pcl:pl-setf $quote (pl-gethash-deref $self "quote"))
        
        ;; defined $quote && length ($quote) ? $quote : $self->{quote_char}
                (pl-if (pcl:pl-&& (pl-defined $quote) (pl-length $quote)) $quote (pl-gethash-deref $self "quote_char"))
        
      )
    )
  )
)

;; sub escape_char { ... }
(pl-sub pl-escape_char (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($ec (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; if (@_) {         my $ec = shift;         $self->_set_attr_C ("escape_char", $ec);         $ec or $self->_set_attr_X ("escape_null", 0);         }
        ;; if (@_)
        (pl-if         @_
          (progn
            ;; my $ec = shift
                        (pcl:pl-setf $ec (pl-shift @_))
            
            ;; $self->_set_attr_C ("escape_char", $ec)
                        (pl-method-call $self '_set_attr_C "escape_char" $ec)
            
            ;; $ec or $self->_set_attr_X ("escape_null", 0)
                        (pcl:pl-or $ec (pl-method-call $self '_set_attr_X "escape_null" 0))
            
          )
          nil
        )
        
        ;; $self->{escape_char}
                (pl-gethash-deref $self "escape_char")
        
      )
    )
  )
)

;; sub sep_char { ... }
(pl-sub pl-sep_char (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; if (@_) {         $self->_set_attr_C ("sep_char", shift);         $self->_cache_set ($_cache_id{sep}, "");         }
        ;; if (@_)
        (pl-if         @_
          (progn
            ;; $self->_set_attr_C ("sep_char", shift)
                        (pl-method-call $self '_set_attr_C "sep_char" (pl-shift @_))
            
            ;; $self->_cache_set ($_cache_id{sep}, "")
                        (pl-method-call $self '_cache_set (pl-gethash %_cache_id "sep") "")
            
          )
          nil
        )
        
        ;; $self->{sep_char}
                (pl-gethash-deref $self "sep_char")
        
      )
    )
  )
)

;; sub sep { ... }
(pl-sub pl-sep (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($sep (make-pl-box nil)) (@b (make-pl-box nil)) ($ec (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; if (@_) {         my $sep = shift;         defined $sep or $sep = "";         utf8::decode ($sep);         my @b = unpack "U0C*", $sep;         if (@b > 1) {             @b > 16 and croak ($self->SetDiag (1006));             $self->sep_char ("\0");             }         else {             $self->sep_char ($sep);             $sep = "";             }         $self->{sep} = $sep;          my $ec = _check_sanity ($self);         $ec and croak ($self->SetDiag ($ec));          $self->_cache_set ($_cache_id{sep}, $sep);         }
        ;; if (@_)
        (pl-if         @_
          (progn
            ;; my $sep = shift
                        (pcl:pl-setf $sep (pl-shift @_))
            
            ;; defined $sep or $sep = ""
                        (pcl:pl-or (pl-defined $sep) (pcl:pl-setf $sep ""))
            
            ;; utf8::decode ($sep)
                        (utf8::pl-decode $sep)
            
            ;; my @b = unpack "U0C*", $sep
                        (pcl:pl-setf @b (let ((*wantarray* t)) (pl-unpack "U0C*" $sep)))
            
            ;; if (@b > 1) {             @b > 16 and croak ($self->SetDiag (1006));             $self->sep_char ("\0");             }         else {             $self->sep_char ($sep);             $sep = "";             }
            ;; if (@b > 1)
            (pl-if             (pcl:pl-> @b 1)
              (progn
                ;; @b > 16 and croak ($self->SetDiag (1006))
                                (pcl:pl-and (pcl:pl-> @b 16) (pl-croak (pl-method-call $self 'SetDiag 1006)))
                
                ;; $self->sep_char ("\0")
                                (pl-method-call $self 'sep_char " ")
                
              )
              ;; else
              (progn
                ;; $self->sep_char ($sep)
                                (pl-method-call $self 'sep_char $sep)
                
                ;; $sep = ""
                                (pcl:pl-setf $sep "")
                
              )
            )
            
            ;; $self->{sep} = $sep
                        (pcl:pl-setf (pl-gethash-deref $self "sep") $sep)
            
            ;; my $ec = _check_sanity ($self)
                        (pcl:pl-setf $ec (pl-_check_sanity $self))
            
            ;; $ec and croak ($self->SetDiag ($ec))
                        (pcl:pl-and $ec (pl-croak (pl-method-call $self 'SetDiag $ec)))
            
            ;; $self->_cache_set ($_cache_id{sep}, $sep)
                        (pl-method-call $self '_cache_set (pl-gethash %_cache_id "sep") $sep)
            
          )
          nil
        )
        
        ;; my $sep = $self->{sep}
                (pcl:pl-setf $sep (pl-gethash-deref $self "sep"))
        
        ;; defined $sep && length ($sep) ? $sep : $self->{sep_char}
                (pl-if (pcl:pl-&& (pl-defined $sep) (pl-length $sep)) $sep (pl-gethash-deref $self "sep_char"))
        
      )
    )
  )
)

;; sub eol { ... }
(pl-sub pl-eol (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($eol (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; if (@_) {         my $eol = shift;         defined $eol or $eol = "";         length ($eol) > 16 and croak ($self->SetDiag (1005));         $self->{eol} = $eol;         $self->_cache_set ($_cache_id{eol}, $eol);         }
        ;; if (@_)
        (pl-if         @_
          (progn
            ;; my $eol = shift
                        (pcl:pl-setf $eol (pl-shift @_))
            
            ;; defined $eol or $eol = ""
                        (pcl:pl-or (pl-defined $eol) (pcl:pl-setf $eol ""))
            
            ;; length ($eol) > 16 and croak ($self->SetDiag (1005))
                        (pcl:pl-and (pcl:pl-> (pl-length $eol) 16) (pl-croak (pl-method-call $self 'SetDiag 1005)))
            
            ;; $self->{eol} = $eol
                        (pcl:pl-setf (pl-gethash-deref $self "eol") $eol)
            
            ;; $self->_cache_set ($_cache_id{eol}, $eol)
                        (pl-method-call $self '_cache_set (pl-gethash %_cache_id "eol") $eol)
            
          )
          nil
        )
        
        ;; $self->{eol}
                (pl-gethash-deref $self "eol")
        
      )
    )
  )
)

;; sub always_quote { ... }
(pl-sub pl-always_quote (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; @_ and $self->_set_attr_X ("always_quote", shift)
                (pcl:pl-and @_ (pl-method-call $self '_set_attr_X "always_quote" (pl-shift @_)))
        
        ;; $self->{always_quote}
                (pl-gethash-deref $self "always_quote")
        
      )
    )
  )
)

;; sub quote_space { ... }
(pl-sub pl-quote_space (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; @_ and $self->_set_attr_X ("quote_space", shift)
                (pcl:pl-and @_ (pl-method-call $self '_set_attr_X "quote_space" (pl-shift @_)))
        
        ;; $self->{quote_space}
                (pl-gethash-deref $self "quote_space")
        
      )
    )
  )
)

;; sub quote_empty { ... }
(pl-sub pl-quote_empty (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; @_ and $self->_set_attr_X ("quote_empty", shift)
                (pcl:pl-and @_ (pl-method-call $self '_set_attr_X "quote_empty" (pl-shift @_)))
        
        ;; $self->{quote_empty}
                (pl-gethash-deref $self "quote_empty")
        
      )
    )
  )
)

;; sub escape_null { ... }
(pl-sub pl-escape_null (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; @_ and $self->_set_attr_X ("escape_null", shift)
                (pcl:pl-and @_ (pl-method-call $self '_set_attr_X "escape_null" (pl-shift @_)))
        
        ;; $self->{escape_null}
                (pl-gethash-deref $self "escape_null")
        
      )
    )
  )
)

;; sub quote_null { ... }
(pl-sub pl-quote_null (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; goto &escape_null
            (pl-goto &escape_null)
      
    )
  )
)

;; sub quote_binary { ... }
(pl-sub pl-quote_binary (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; @_ and $self->_set_attr_X ("quote_binary", shift)
                (pcl:pl-and @_ (pl-method-call $self '_set_attr_X "quote_binary" (pl-shift @_)))
        
        ;; $self->{quote_binary}
                (pl-gethash-deref $self "quote_binary")
        
      )
    )
  )
)

;; sub binary { ... }
(pl-sub pl-binary (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; @_ and $self->_set_attr_X ("binary", shift)
                (pcl:pl-and @_ (pl-method-call $self '_set_attr_X "binary" (pl-shift @_)))
        
        ;; $self->{binary}
                (pl-gethash-deref $self "binary")
        
      )
    )
  )
)

;; sub strict { ... }
(pl-sub pl-strict (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; @_ and $self->_set_attr_X ("strict", shift)
                (pcl:pl-and @_ (pl-method-call $self '_set_attr_X "strict" (pl-shift @_)))
        
        ;; $self->{strict}
                (pl-gethash-deref $self "strict")
        
      )
    )
  )
)

;; sub _supported_skip_empty_rows { ... }
(pl-sub pl-_supported_skip_empty_rows (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($f (make-pl-box nil)))
        ;; my ($self, $f) = @_
                (pcl:pl-setf (vector $self $f) @_)
        
        ;; defined $f or return 0
                (pcl:pl-or (pl-defined $f) (pl-return 0))
        
        ;; if ($self && $f && ref $f && ref $f eq "CODE") {        $self->{'_EMPTROW_CB'} = $f;        return 6;        }
        ;; if ($self && $f && ref $f && ref $f eq "CODE")
        (pl-if         (pcl:pl-&& (pcl:pl-&& (pcl:pl-&& $self $f) (pl-ref $f)) (pcl:pl-str-eq (pl-ref $f) "CODE"))
          (progn
            ;; $self->{'_EMPTROW_CB'} = $f
                        (pcl:pl-setf (pl-gethash-deref $self "_EMPTROW_CB") $f)
            
            ;; return 6
                        (pl-return 6)
            
          )
          nil
        )
        
        ;; $f =~ m/^(?: 0 | undef         )$/xi ? 0 :     $f =~ m/^(?: 1 | skip          )$/xi ? 1 :     $f =~ m/^(?: 2 | eof   | stop  )$/xi ? 2 :     $f =~ m/^(?: 3 | die           )$/xi ? 3 :     $f =~ m/^(?: 4 | croak         )$/xi ? 4 :     $f =~ m/^(?: 5 | error         )$/xi ? 5 :     $f =~ m/^(?: 6 | cb            )$/xi ? 6 : do {        $self ||= "Text::CSV_PP";        croak ($self->_SetDiagInfo (1500, "skip_empty_rows '$f' is not supported"));        }
        ;; PARSE ERROR: Fell through. Missing case: [
        
      )
    )
  )
)

;; sub skip_empty_rows { ... }
(pl-sub pl-skip_empty_rows (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($ser (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; @_ and $self->_set_attr_N ("skip_empty_rows", _supported_skip_empty_rows ($self, shift))
                (pcl:pl-and @_ (pl-method-call $self '_set_attr_N "skip_empty_rows" (pl-_supported_skip_empty_rows $self (pl-shift @_))))
        
        ;; my $ser = $self->{'skip_empty_rows'}
                (pcl:pl-setf $ser (pl-gethash-deref $self "skip_empty_rows"))
        
        ;; $ser == 6 or $self->{'_EMPTROW_CB'} = undef
                (pcl:pl-or (pcl:pl-== $ser 6) (pcl:pl-setf (pl-gethash-deref $self "_EMPTROW_CB") (pl-undef)))
        
        ;; $ser <= 1 ? $ser : $ser == 2 ? "eof"   : $ser == 3 ? "die"   :                       $ser == 4 ? "croak" : $ser == 5 ? "error" :                       $self->{'_EMPTROW_CB'}
                (pl-if (pcl:pl-<= $ser 1) $ser (pl-if (pcl:pl-== $ser 2) "eof" (pl-if (pcl:pl-== $ser 3) "die" (pl-if (pcl:pl-== $ser 4) "croak" (pl-if (pcl:pl-== $ser 5) "error" (pl-gethash-deref $self "_EMPTROW_CB"))))))
        
      )
    )
  )
)

;; sub _SetDiagInfo { ... }
(pl-sub pl-_SetDiagInfo (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($err (make-pl-box nil)) ($msg (make-pl-box nil)) ($em (make-pl-box nil)) ($sep (make-pl-box nil)))
        ;; my ($self, $err, $msg) = @_
                (pcl:pl-setf (vector $self $err $msg) @_)
        
        ;; $self->SetDiag ($err)
                (pl-method-call $self 'SetDiag $err)
        
        ;; my $em  = $self->error_diag
                (pcl:pl-setf $em (pl-method-call $self 'error_diag))
        
        ;; $em =~ s/^\d+$// and $msg =~ s/^/# /
                (pcl:pl-and (pcl:pl-=~ $em (pl-subst "^\\d+$" "")) (pcl:pl-=~ $msg (pl-subst "^" "# ")))
        
        ;; my $sep = $em =~ m/[;\n]$/ ? "\n\t" : ": "
                (pcl:pl-setf $sep (pl-if (pcl:pl-=~ $em (pl-regex "m/[;\\n]$/")) "
	" ": "))
        
        ;; join $sep => grep m/\S\S\S/ => $em, $msg
                (pl-join $sep (pl-grep (pcl:pl-=~ $_ (pl-regex "m/\\S\\S\\S/")) $em $msg))
        
      )
    )
  )
)

;; sub _supported_formula { ... }
(pl-sub pl-_supported_formula (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($f (make-pl-box nil)))
        ;; my ($self, $f) = @_
                (pcl:pl-setf (vector $self $f) @_)
        
        ;; defined $f or return 5
                (pcl:pl-or (pl-defined $f) (pl-return 5))
        
        ;; if ($self && $f && ref $f && ref $f eq "CODE") {     $self->{_FORMULA_CB} = $f;     return 6;     }
        ;; if ($self && $f && ref $f && ref $f eq "CODE")
        (pl-if         (pcl:pl-&& (pcl:pl-&& (pcl:pl-&& $self $f) (pl-ref $f)) (pcl:pl-str-eq (pl-ref $f) "CODE"))
          (progn
            ;; $self->{_FORMULA_CB} = $f
                        (pcl:pl-setf (pl-gethash-deref $self "_FORMULA_CB") $f)
            
            ;; return 6
                        (pl-return 6)
            
          )
          nil
        )
        
        ;; $f =~ m/^(?: 0 | none    )$/xi ? 0 :     $f =~ m/^(?: 1 | die     )$/xi ? 1 :     $f =~ m/^(?: 2 | croak   )$/xi ? 2 :     $f =~ m/^(?: 3 | diag    )$/xi ? 3 :     $f =~ m/^(?: 4 | empty | )$/xi ? 4 :     $f =~ m/^(?: 5 | undef   )$/xi ? 5 :     $f =~ m/^(?: 6 | cb      )$/xi ? 6 : do {         $self ||= "Text::CSV_PP";         croak ($self->_SetDiagInfo (1500, "formula-handling '$f' is not supported"));         }
        ;; PARSE ERROR: Fell through. Missing case: [
        
      )
    )
  )
)

;; sub formula { ... }
(pl-sub pl-formula (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; @_ and $self->_set_attr_N ("formula", _supported_formula ($self, shift))
                (pcl:pl-and @_ (pl-method-call $self '_set_attr_N "formula" (pl-_supported_formula $self (pl-shift @_))))
        
        ;; $self->{formula} == 6 or $self->{_FORMULA_CB} = undef
                (pcl:pl-or (pcl:pl-== (pl-gethash-deref $self "formula") 6) (pcl:pl-setf (pl-gethash-deref $self "_FORMULA_CB") (pl-undef)))
        
        ;;      [qw( none die croak diag empty undef cb )]->[_supported_formula ($self, $self->{formula})]
                (pl-aref-deref (make-array 1 :adjustable t :fill-pointer t :initial-contents (list (vector "none" "die" "croak" "diag" "empty" "undef" "cb"))) (pl-_supported_formula $self (pl-gethash-deref $self "formula")))
        
      )
    )
  )
)

;; sub formula_handling { ... }
(pl-sub pl-formula_handling (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; $self->formula (@_)
                (pl-method-call $self 'formula @_)
        
      )
    )
  )
)

;; sub decode_utf8 { ... }
(pl-sub pl-decode_utf8 (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; @_ and $self->_set_attr_X ("decode_utf8", shift)
                (pcl:pl-and @_ (pl-method-call $self '_set_attr_X "decode_utf8" (pl-shift @_)))
        
        ;; $self->{decode_utf8}
                (pl-gethash-deref $self "decode_utf8")
        
      )
    )
  )
)

;; sub keep_meta_info { ... }
(pl-sub pl-keep_meta_info (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($v (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; if (@_) {         my $v = shift;         !defined $v || $v eq "" and $v = 0;         $v =~ m/^[0-9]/ or $v = lc $v eq "false" ? 0 : 1; # true/truth = 1         $self->_set_attr_X ("keep_meta_info", $v);         }
        ;; if (@_)
        (pl-if         @_
          (progn
            ;; my $v = shift
                        (pcl:pl-setf $v (pl-shift @_))
            
            ;; !defined $v || $v eq "" and $v = 0
                        (pcl:pl-and (pcl:pl-|| (pl-! (pl-defined $v)) (pcl:pl-str-eq $v "")) (pcl:pl-setf $v 0))
            
            ;; $v =~ m/^[0-9]/ or $v = lc $v eq "false" ? 0 : 1
                        (pcl:pl-or (pcl:pl-=~ $v (pl-regex "m/^[0-9]/")) (pcl:pl-setf $v (pl-lc (pl-if (pcl:pl-str-eq $v "false") 0 1))))
            
            ;; $self->_set_attr_X ("keep_meta_info", $v)
                        (pl-method-call $self '_set_attr_X "keep_meta_info" $v)
            
          )
          nil
        )
        
        ;; $self->{keep_meta_info}
                (pl-gethash-deref $self "keep_meta_info")
        
      )
    )
  )
)

;; sub allow_loose_quotes { ... }
(pl-sub pl-allow_loose_quotes (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; @_ and $self->_set_attr_X ("allow_loose_quotes", shift)
                (pcl:pl-and @_ (pl-method-call $self '_set_attr_X "allow_loose_quotes" (pl-shift @_)))
        
        ;; $self->{allow_loose_quotes}
                (pl-gethash-deref $self "allow_loose_quotes")
        
      )
    )
  )
)

;; sub allow_loose_escapes { ... }
(pl-sub pl-allow_loose_escapes (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; @_ and $self->_set_attr_X ("allow_loose_escapes", shift)
                (pcl:pl-and @_ (pl-method-call $self '_set_attr_X "allow_loose_escapes" (pl-shift @_)))
        
        ;; $self->{allow_loose_escapes}
                (pl-gethash-deref $self "allow_loose_escapes")
        
      )
    )
  )
)

;; sub allow_whitespace { ... }
(pl-sub pl-allow_whitespace (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($aw (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; if (@_) {         my $aw = shift;         _unhealthy_whitespace ($self, $aw) and             croak ($self->SetDiag (1002));         $self->_set_attr_X ("allow_whitespace", $aw);         }
        ;; if (@_)
        (pl-if         @_
          (progn
            ;; my $aw = shift
                        (pcl:pl-setf $aw (pl-shift @_))
            
            ;; _unhealthy_whitespace ($self, $aw) and             croak ($self->SetDiag (1002))
                        (pcl:pl-and (pl-_unhealthy_whitespace $self $aw) (pl-croak (pl-method-call $self 'SetDiag 1002)))
            
            ;; $self->_set_attr_X ("allow_whitespace", $aw)
                        (pl-method-call $self '_set_attr_X "allow_whitespace" $aw)
            
          )
          nil
        )
        
        ;; $self->{allow_whitespace}
                (pl-gethash-deref $self "allow_whitespace")
        
      )
    )
  )
)

;; sub allow_unquoted_escape { ... }
(pl-sub pl-allow_unquoted_escape (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; @_ and $self->_set_attr_X ("allow_unquoted_escape", shift)
                (pcl:pl-and @_ (pl-method-call $self '_set_attr_X "allow_unquoted_escape" (pl-shift @_)))
        
        ;; $self->{allow_unquoted_escape}
                (pl-gethash-deref $self "allow_unquoted_escape")
        
      )
    )
  )
)

;; sub blank_is_undef { ... }
(pl-sub pl-blank_is_undef (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; @_ and $self->_set_attr_X ("blank_is_undef", shift)
                (pcl:pl-and @_ (pl-method-call $self '_set_attr_X "blank_is_undef" (pl-shift @_)))
        
        ;; $self->{blank_is_undef}
                (pl-gethash-deref $self "blank_is_undef")
        
      )
    )
  )
)

;; sub empty_is_undef { ... }
(pl-sub pl-empty_is_undef (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; @_ and $self->_set_attr_X ("empty_is_undef", shift)
                (pcl:pl-and @_ (pl-method-call $self '_set_attr_X "empty_is_undef" (pl-shift @_)))
        
        ;; $self->{empty_is_undef}
                (pl-gethash-deref $self "empty_is_undef")
        
      )
    )
  )
)

;; sub verbatim { ... }
(pl-sub pl-verbatim (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; @_ and $self->_set_attr_X ("verbatim", shift)
                (pcl:pl-and @_ (pl-method-call $self '_set_attr_X "verbatim" (pl-shift @_)))
        
        ;; $self->{verbatim}
                (pl-gethash-deref $self "verbatim")
        
      )
    )
  )
)

;; sub undef_str { ... }
(pl-sub pl-undef_str (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($v (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; if (@_) {         my $v = shift;         $self->{undef_str} = defined $v ? "$v" : undef;         $self->_cache_set ($_cache_id{undef_str}, $self->{undef_str});         }
        ;; if (@_)
        (pl-if         @_
          (progn
            ;; my $v = shift
                        (pcl:pl-setf $v (pl-shift @_))
            
            ;; $self->{undef_str} = defined $v ? "$v" : undef
                        (pcl:pl-setf (pl-gethash-deref $self "undef_str") (pl-if (pl-defined $v) $v (pl-undef)))
            
            ;; $self->_cache_set ($_cache_id{undef_str}, $self->{undef_str})
                        (pl-method-call $self '_cache_set (pl-gethash %_cache_id "undef_str") (pl-gethash-deref $self "undef_str"))
            
          )
          nil
        )
        
        ;; $self->{undef_str}
                (pl-gethash-deref $self "undef_str")
        
      )
    )
  )
)

;; sub comment_str { ... }
(pl-sub pl-comment_str (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($v (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; if (@_) {         my $v = shift;         $self->{comment_str} = defined $v ? "$v" : undef;         $self->_cache_set ($_cache_id{comment_str}, $self->{comment_str});         }
        ;; if (@_)
        (pl-if         @_
          (progn
            ;; my $v = shift
                        (pcl:pl-setf $v (pl-shift @_))
            
            ;; $self->{comment_str} = defined $v ? "$v" : undef
                        (pcl:pl-setf (pl-gethash-deref $self "comment_str") (pl-if (pl-defined $v) $v (pl-undef)))
            
            ;; $self->_cache_set ($_cache_id{comment_str}, $self->{comment_str})
                        (pl-method-call $self '_cache_set (pl-gethash %_cache_id "comment_str") (pl-gethash-deref $self "comment_str"))
            
          )
          nil
        )
        
        ;; $self->{comment_str}
                (pl-gethash-deref $self "comment_str")
        
      )
    )
  )
)

;; sub auto_diag { ... }
(pl-sub pl-auto_diag (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($v (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; if (@_) {         my $v = shift;         !defined $v || $v eq "" and $v = 0;         $v =~ m/^[0-9]/ or $v = lc $v eq "false" ? 0 : 1; # true/truth = 1         $self->_set_attr_X ("auto_diag", $v);         }
        ;; if (@_)
        (pl-if         @_
          (progn
            ;; my $v = shift
                        (pcl:pl-setf $v (pl-shift @_))
            
            ;; !defined $v || $v eq "" and $v = 0
                        (pcl:pl-and (pcl:pl-|| (pl-! (pl-defined $v)) (pcl:pl-str-eq $v "")) (pcl:pl-setf $v 0))
            
            ;; $v =~ m/^[0-9]/ or $v = lc $v eq "false" ? 0 : 1
                        (pcl:pl-or (pcl:pl-=~ $v (pl-regex "m/^[0-9]/")) (pcl:pl-setf $v (pl-lc (pl-if (pcl:pl-str-eq $v "false") 0 1))))
            
            ;; $self->_set_attr_X ("auto_diag", $v)
                        (pl-method-call $self '_set_attr_X "auto_diag" $v)
            
          )
          nil
        )
        
        ;; $self->{auto_diag}
                (pl-gethash-deref $self "auto_diag")
        
      )
    )
  )
)

;; sub diag_verbose { ... }
(pl-sub pl-diag_verbose (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($v (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; if (@_) {         my $v = shift;         !defined $v || $v eq "" and $v = 0;         $v =~ m/^[0-9]/ or $v = lc $v eq "false" ? 0 : 1; # true/truth = 1         $self->_set_attr_X ("diag_verbose", $v);         }
        ;; if (@_)
        (pl-if         @_
          (progn
            ;; my $v = shift
                        (pcl:pl-setf $v (pl-shift @_))
            
            ;; !defined $v || $v eq "" and $v = 0
                        (pcl:pl-and (pcl:pl-|| (pl-! (pl-defined $v)) (pcl:pl-str-eq $v "")) (pcl:pl-setf $v 0))
            
            ;; $v =~ m/^[0-9]/ or $v = lc $v eq "false" ? 0 : 1
                        (pcl:pl-or (pcl:pl-=~ $v (pl-regex "m/^[0-9]/")) (pcl:pl-setf $v (pl-lc (pl-if (pcl:pl-str-eq $v "false") 0 1))))
            
            ;; $self->_set_attr_X ("diag_verbose", $v)
                        (pl-method-call $self '_set_attr_X "diag_verbose" $v)
            
          )
          nil
        )
        
        ;; $self->{diag_verbose}
                (pl-gethash-deref $self "diag_verbose")
        
      )
    )
  )
)

;; ################################################################################
;; # status
;; ################################################################################
;; sub status { ... }
(pl-sub pl-status (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; $_[0]->{_STATUS}
            (pl-gethash (pl-aref @_ 0) "_STATUS")
      
    )
  )
)

;; sub eof { ... }
(pl-sub pl-eof (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; $_[0]->{_EOF}
            (pl-gethash (pl-aref @_ 0) "_EOF")
      
    )
  )
)

;; sub types { ... }
(pl-sub pl-types (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($types (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; if (@_) {         if (my $types = shift) {             $self->{'_types'} = join("", map{ chr($_) } @$types);             $self->{'types'} = $types;             $self->_cache_set ($_cache_id{'types'}, $self->{'_types'});         }         else {             delete $self->{'types'};             delete $self->{'_types'};             $self->_cache_set ($_cache_id{'types'}, undef);             undef;         }     }     else {         $self->{'types'};     }
        ;; if (@_)
        (pl-if         @_
          (progn
            ;; if (my $types = shift) {             $self->{'_types'} = join("", map{ chr($_) } @$types);             $self->{'types'} = $types;             $self->_cache_set ($_cache_id{'types'}, $self->{'_types'});         }         else {             delete $self->{'types'};             delete $self->{'_types'};             $self->_cache_set ($_cache_id{'types'}, undef);             undef;         }
            (let (($types (make-pl-box nil)))
              ;; if (my $types = shift)
              (pl-if               (pcl:pl-setf $types (pl-shift @_))
                (progn
                  (defun --anon-block-71-- ($_)
                    ;; chr($_)
                                        (pl-chr $_)
                    
                  )
                  
                  ;; $self->{'_types'} = join("", map{ chr($_) } @$types)
                                    (pcl:pl-setf (pl-gethash-deref $self "_types") (pl-join "" (pl-map #'--anon-block-71-- (pl-cast-@ $types))))
                  
                  ;; $self->{'types'} = $types
                                    (pcl:pl-setf (pl-gethash-deref $self "types") $types)
                  
                  ;; $self->_cache_set ($_cache_id{'types'}, $self->{'_types'})
                                    (pl-method-call $self '_cache_set (pl-gethash %_cache_id "types") (pl-gethash-deref $self "_types"))
                  
                )
                ;; else
                (progn
                  ;; delete $self->{'types'}
                                    (pl-gethash-deref (pl-delete $self) "types")
                  
                  ;; delete $self->{'_types'}
                                    (pl-gethash-deref (pl-delete $self) "_types")
                  
                  ;; $self->_cache_set ($_cache_id{'types'}, undef)
                                    (pl-method-call $self '_cache_set (pl-gethash %_cache_id "types") (pl-undef))
                  
                  ;; undef
                                    (pl-undef)
                  
                )
              )
            )
            
          )
          ;; else
          (progn
            ;; $self->{'types'}
                        (pl-gethash-deref $self "types")
            
          )
        )
        
      )
    )
  )
)

;; sub callbacks { ... }
(pl-sub pl-callbacks (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($cb (make-pl-box nil)) ($hf (make-pl-box nil)) ($cbk (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; if (@_) {         my $cb;         my $hf = 0x00;         if (defined $_[0]) {             grep { !defined } @_ and croak ($self->SetDiag (1004));             $cb = @_ == 1 && ref $_[0] eq "HASH" ? shift                 : @_ % 2 == 0                    ? { @_ }                 : croak ($self->SetDiag (1004));             foreach my $cbk (keys %$cb) {                 # A key cannot be a ref. That would be stored as the *string                 # 'SCALAR(0x1f3e710)' or 'ARRAY(0x1a5ae18)'                 $cbk =~ m/^[\w.]+$/ && ref $cb->{$cbk} eq "CODE" or                     croak ($self->SetDiag (1004));                 }             exists $cb->{error}        and $hf |= 0x01;             exists $cb->{after_parse}  and $hf |= 0x02;             exists $cb->{before_print} and $hf |= 0x04;             }         elsif (@_ > 1) {             # (undef, whatever)             croak ($self->SetDiag (1004));             }         $self->_set_attr_X ("_has_hooks", $hf);         $self->{callbacks} = $cb;         }
        ;; if (@_)
        (pl-if         @_
          (progn
            ;; my $cb (bare declaration)
            
            ;; my $hf = 0x00
                        (pcl:pl-setf $hf 0x00)
            
            ;; if (defined $_[0]) {             grep { !defined } @_ and croak ($self->SetDiag (1004));             $cb = @_ == 1 && ref $_[0] eq "HASH" ? shift                 : @_ % 2 == 0                    ? { @_ }                 : croak ($self->SetDiag (1004));             foreach my $cbk (keys %$cb) {                 # A key cannot be a ref. That would be stored as the *string                 # 'SCALAR(0x1f3e710)' or 'ARRAY(0x1a5ae18)'                 $cbk =~ m/^[\w.]+$/ && ref $cb->{$cbk} eq "CODE" or                     croak ($self->SetDiag (1004));                 }             exists $cb->{error}        and $hf |= 0x01;             exists $cb->{after_parse}  and $hf |= 0x02;             exists $cb->{before_print} and $hf |= 0x04;             }         elsif (@_ > 1) {             # (undef, whatever)             croak ($self->SetDiag (1004));             }
            ;; if (defined $_[0])
            (pl-if             (pl-aref (pl-defined $_) 0)
              (progn
                (defun --anon-block-72-- ($_)
                  ;; !defined
                                    (pl-! (pl-defined $_))
                  
                )
                
                ;; grep { !defined } @_ and croak ($self->SetDiag (1004))
                                (pl-grep #'--anon-block-72-- (pcl:pl-and @_ (let ((*wantarray* t)) (pl-croak (let ((*wantarray* t)) (pl-method-call $self 'SetDiag 1004))))))
                
                ;; $cb = @_ == 1 && ref $_[0] eq "HASH" ? shift                 : @_ % 2 == 0                    ? { @_ }                 : croak ($self->SetDiag (1004))
                                (pcl:pl-setf $cb (pl-if (pcl:pl-&& (pcl:pl-== @_ 1) (pcl:pl-str-eq (pl-aref (pl-ref $_) 0) "HASH")) (pl-shift @_) (pl-if (pcl:pl-== (pcl:pl-% @_ 2) 0) (pl-hash @_) (pl-croak (pl-method-call $self 'SetDiag 1004)))))
                
                ;; foreach my $cbk (keys %$cb) {                 # A key cannot be a ref. That would be stored as the *string                 # 'SCALAR(0x1f3e710)' or 'ARRAY(0x1a5ae18)'                 $cbk =~ m/^[\w.]+$/ && ref $cb->{$cbk} eq "CODE" or                     croak ($self->SetDiag (1004));                 }
                (pl-foreach ($cbk                 (pl-keys (pl-cast-% $cb)))
                  ;; $cbk =~ m/^[\w.]+$/ && ref $cb->{$cbk} eq "CODE" or                     croak ($self->SetDiag (1004))
                                    (pcl:pl-or (pcl:pl-&& (pcl:pl-=~ $cbk (pl-regex "m/^[\\w.]+$/")) (pcl:pl-str-eq (pl-gethash-deref (pl-ref $cb) $cbk) "CODE")) (pl-croak (pl-method-call $self 'SetDiag 1004)))
                  
                )
                
                ;; exists $cb->{error}        and $hf |= 0x01
                                (pcl:pl-and (pl-gethash-deref (pl-exists $cb) "error") (pcl:pl-bit-or= $hf 0x01))
                
                ;; exists $cb->{after_parse}  and $hf |= 0x02
                                (pcl:pl-and (pl-gethash-deref (pl-exists $cb) "after_parse") (pcl:pl-bit-or= $hf 0x02))
                
                ;; exists $cb->{before_print} and $hf |= 0x04
                                (pcl:pl-and (pl-gethash-deref (pl-exists $cb) "before_print") (pcl:pl-bit-or= $hf 0x04))
                
              )
              ;; elsif (@_ > 1)
              (pl-if               (pcl:pl-> @_ 1)
                (progn
                  ;; croak ($self->SetDiag (1004))
                                    (pl-croak (pl-method-call $self 'SetDiag 1004))
                  
                )
                nil
              )
            )
            
            ;; $self->_set_attr_X ("_has_hooks", $hf)
                        (pl-method-call $self '_set_attr_X "_has_hooks" $hf)
            
            ;; $self->{callbacks} = $cb
                        (pcl:pl-setf (pl-gethash-deref $self "callbacks") $cb)
            
          )
          nil
        )
        
        ;; $self->{callbacks}
                (pl-gethash-deref $self "callbacks")
        
      )
    )
  )
)

;; ################################################################################
;; # error_diag
;; ################################################################################
;; sub error_diag { ... }
(pl-sub pl-error_diag (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) (@diag (make-pl-box nil)) ($context (make-pl-box nil)) ($msg (make-pl-box nil)) ($lvl (make-pl-box nil)) (@c (make-pl-box nil)) ($hints (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; my @diag = (0 + $last_new_error, $last_new_error, 0, 0, 0)
                (pcl:pl-setf @diag (vector (pcl:pl-+ 0 $last_new_error) $last_new_error 0 0 0))
        
        ;; if ($self && ref $self && # Not a class method or direct call         UNIVERSAL::isa ($self, __PACKAGE__) && exists $self->{_ERROR_DIAG}) {         $diag[0] = 0 + $self->{_ERROR_DIAG};         $diag[1] =     $self->{_ERROR_DIAG};         $diag[2] = 1 + $self->{_ERROR_POS} if exists $self->{_ERROR_POS};         $diag[3] =     $self->{_RECNO};         $diag[4] =     $self->{_ERROR_FLD} if exists $self->{_ERROR_FLD};          $diag[0] && $self->{callbacks} && $self->{callbacks}{error} and             return $self->{callbacks}{error}->(@diag);         }
        ;; if ($self && ref $self && # Not a class method or direct call
;;         UNIVERSAL::isa ($self, __PACKAGE__) && exists $self->{"_ERROR_DIAG"})
        (pl-if         (pcl:pl-&& (pcl:pl-&& (pcl:pl-&& $self (pl-ref $self)) (UNIVERSAL::pl-isa $self "Text::CSV_PP")) (pl-gethash-deref (pl-exists $self) "_ERROR_DIAG"))
          (progn
            ;; $diag[0] = 0 + $self->{_ERROR_DIAG}
                        (pcl:pl-setf (pl-aref @diag 0) (pcl:pl-+ 0 (pl-gethash-deref $self "_ERROR_DIAG")))
            
            ;; $diag[1] =     $self->{_ERROR_DIAG}
                        (pcl:pl-setf (pl-aref @diag 1) (pl-gethash-deref $self "_ERROR_DIAG"))
            
            ;; $diag[2] = 1 + $self->{_ERROR_POS} if exists $self->{_ERROR_POS}
            (pl-if             (pl-gethash-deref (pl-exists $self) "_ERROR_POS")             (pcl:pl-setf (pl-aref @diag 2) (pcl:pl-+ 1 (pl-gethash-deref $self "_ERROR_POS"))))
            
            ;; $diag[3] =     $self->{_RECNO}
                        (pcl:pl-setf (pl-aref @diag 3) (pl-gethash-deref $self "_RECNO"))
            
            ;; $diag[4] =     $self->{_ERROR_FLD} if exists $self->{_ERROR_FLD}
            (pl-if             (pl-gethash-deref (pl-exists $self) "_ERROR_FLD")             (pcl:pl-setf (pl-aref @diag 4) (pl-gethash-deref $self "_ERROR_FLD")))
            
            ;; $diag[0] && $self->{callbacks} && $self->{callbacks}{error} and             return $self->{callbacks}{error}->(@diag)
                        (pcl:pl-and (pcl:pl-&& (pcl:pl-&& (pl-aref @diag 0) (pl-gethash-deref $self "callbacks")) (pl-gethash (pl-gethash-deref $self "callbacks") "error")) (pl-return (pl-funcall-ref (pl-gethash (pl-gethash-deref $self "callbacks") "error") @diag)))
            
          )
          nil
        )
        
        ;; my $context = wantarray
                (pcl:pl-setf $context (pl-wantarray))
        
        ;; unless (defined $context) {	# Void context, auto-diag         if ($diag[0] && $diag[0] != 2012) {             my $msg = "# CSV_PP ERROR: $diag[0] - $diag[1] \@ rec $diag[3] pos $diag[2]\n";             $diag[4] and $msg =~ s/$/ field $diag[4]/;              unless ($self && ref $self) {        # auto_diag                     # called without args in void context                 warn $msg;                 return;                 }              $self->{diag_verbose} and $self->{_ERROR_INPUT} and             $msg .= "$self->{_ERROR_INPUT}'\n".                 (" " x ($diag[2] - 1))."^\n";              my $lvl = $self->{auto_diag};             if ($lvl < 2) {                 my @c = caller (2);                 if (@c >= 11 && $c[10] && ref $c[10] eq "HASH") {                     my $hints = $c[10];                     (exists $hints->{autodie} && $hints->{autodie} or                      exists $hints->{"guard Fatal"} &&                     !exists $hints->{"no Fatal"}) and                         $lvl++;                     # Future releases of autodie will probably set $^H{autodie}                     #  to "autodie @args", like "autodie :all" or "autodie open"                     #  so we can/should check for "open" or "new"                     }                 }             $lvl > 1 ? die $msg : warn $msg;             }         return;         }
        ;; unless (defined $context)
        (pl-if (pl-not         (pl-defined $context))
          (progn
            ;; if ($diag[0] && $diag[0] != 2012) {             my $msg = "# CSV_PP ERROR: $diag[0] - $diag[1] \@ rec $diag[3] pos $diag[2]\n";             $diag[4] and $msg =~ s/$/ field $diag[4]/;              unless ($self && ref $self) {        # auto_diag                     # called without args in void context                 warn $msg;                 return;                 }              $self->{diag_verbose} and $self->{_ERROR_INPUT} and             $msg .= "$self->{_ERROR_INPUT}'\n".                 (" " x ($diag[2] - 1))."^\n";              my $lvl = $self->{auto_diag};             if ($lvl < 2) {                 my @c = caller (2);                 if (@c >= 11 && $c[10] && ref $c[10] eq "HASH") {                     my $hints = $c[10];                     (exists $hints->{autodie} && $hints->{autodie} or                      exists $hints->{"guard Fatal"} &&                     !exists $hints->{"no Fatal"}) and                         $lvl++;                     # Future releases of autodie will probably set $^H{autodie}                     #  to "autodie @args", like "autodie :all" or "autodie open"                     #  so we can/should check for "open" or "new"                     }                 }             $lvl > 1 ? die $msg : warn $msg;             }
            ;; if ($diag[0] && $diag[0] != 2012)
            (pl-if             (pcl:pl-&& (pl-aref @diag 0) (pcl:pl-!= (pl-aref @diag 0) 2012))
              (progn
                ;; my $msg = "# CSV_PP ERROR: $diag[0] - $diag[1] \@ rec $diag[3] pos $diag[2]\n"
                                (pcl:pl-setf $msg (pl-string-concat "# CSV_PP ERROR: " (pl-aref @diag ) " - " (pl-aref @diag ) " \\" "@" " rec " (pl-aref @diag ) " pos " (pl-aref @diag ) "
"))
                
                ;; $diag[4] and $msg =~ s/$/ field $diag[4]/
                                (pcl:pl-and (pl-aref @diag 4) (pcl:pl-=~ $msg (pl-subst "$" " field $diag[4]")))
                
                ;; unless ($self && ref $self) {        # auto_diag                     # called without args in void context                 warn $msg;                 return;                 }
                ;; unless ($self && ref $self)
                (pl-if (pl-not                 (pcl:pl-&& $self (pl-ref $self)))
                  (progn
                    ;; warn $msg
                                        (pl-warn $msg)
                    
                    ;; return
                                        (pl-return)
                    
                  )
                  nil
                )
                
                ;; $self->{diag_verbose} and $self->{_ERROR_INPUT} and             $msg .= "$self->{_ERROR_INPUT}'\n".                 (" " x ($diag[2] - 1))."^\n"
                                (pcl:pl-and (pcl:pl-and (pl-gethash-deref $self "diag_verbose") (pl-gethash-deref $self "_ERROR_INPUT")) (pcl:pl-.= $msg (pcl:pl-. (pcl:pl-. (pl-string-concat $self "->{_ERROR_INPUT}'
") (pcl:pl-str-x " " (pcl:pl-- (pl-aref @diag 2) 1))) "^
")))
                
                ;; my $lvl = $self->{auto_diag}
                                (pcl:pl-setf $lvl (pl-gethash-deref $self "auto_diag"))
                
                ;; if ($lvl < 2) {                 my @c = caller (2);                 if (@c >= 11 && $c[10] && ref $c[10] eq "HASH") {                     my $hints = $c[10];                     (exists $hints->{autodie} && $hints->{autodie} or                      exists $hints->{"guard Fatal"} &&                     !exists $hints->{"no Fatal"}) and                         $lvl++;                     # Future releases of autodie will probably set $^H{autodie}                     #  to "autodie @args", like "autodie :all" or "autodie open"                     #  so we can/should check for "open" or "new"                     }                 }
                ;; if ($lvl < 2)
                (pl-if                 (pcl:pl-< $lvl 2)
                  (progn
                    ;; my @c = caller (2)
                                        (pcl:pl-setf @c (let ((*wantarray* t)) (pl-caller 2)))
                    
                    ;; if (@c >= 11 && $c[10] && ref $c[10] eq "HASH") {                     my $hints = $c[10];                     (exists $hints->{autodie} && $hints->{autodie} or                      exists $hints->{"guard Fatal"} &&                     !exists $hints->{"no Fatal"}) and                         $lvl++;                     # Future releases of autodie will probably set $^H{autodie}                     #  to "autodie @args", like "autodie :all" or "autodie open"                     #  so we can/should check for "open" or "new"                     }
                    ;; if (@c >= 11 && $c[10] && ref $c[10] eq "HASH")
                    (pl-if                     (pcl:pl-&& (pcl:pl-&& (pcl:pl->= @c 11) (pl-aref @c 10)) (pcl:pl-str-eq (pl-ref (pl-aref @c 10)) "HASH"))
                      (progn
                        ;; my $hints = $c[10]
                                                (pcl:pl-setf $hints (pl-aref @c 10))
                        
                        ;;                      (exists $hints->{autodie} && $hints->{autodie} or                      exists $hints->{"guard Fatal"} &&                     !exists $hints->{"no Fatal"}) and                         $lvl++
                                                (pcl:pl-and (pcl:pl-or (pcl:pl-&& (pl-gethash-deref (pl-exists $hints) "autodie") (pl-gethash-deref $hints "autodie")) (pcl:pl-&& (pl-gethash-deref (pl-exists $hints) "guard Fatal") (pl-! (pl-gethash-deref (pl-exists $hints) "no Fatal")))) (pl-post++ $lvl))
                        
                      )
                      nil
                    )
                    
                  )
                  nil
                )
                
                ;; $lvl > 1 ? die $msg : warn $msg
                ;; PARSE ERROR: Fell through. Missing case: [
                
              )
              nil
            )
            
            ;; return
                        (pl-return)
            
          )
          nil
        )
        
        ;; return $context ? @diag : $diag[1]
                (pl-return (pl-if $context @diag (pl-aref @diag 1)))
        
      )
    )
  )
)

;; sub record_number { ... }
(pl-sub pl-record_number (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; return shift->{_RECNO}
            (pl-return (pl-gethash-deref (pl-shift @_) "_RECNO"))
      
    )
  )
)

;; ################################################################################
;; # string
;; ################################################################################
;; *string = \&_string
(pcl:pl-setf *string (pl-backslash &_string))

;; sub _string { ... }
(pl-sub pl-_string (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; defined $_[0]->{_STRING} ? ${ $_[0]->{_STRING} } : undef
            (pl-if (pl-gethash (pl-aref (pl-defined $_) 0) "_STRING") (pl-cast-$ (pl-gethash (pl-aref @_ 0) "_STRING")) (pl-undef))
      
    )
  )
)

;; ################################################################################
;; # fields
;; ################################################################################
;; *fields = \&_fields
(pcl:pl-setf *fields (pl-backslash &_fields))

;; sub _fields { ... }
(pl-sub pl-_fields (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; ref($_[0]->{_FIELDS}) ?  @{$_[0]->{_FIELDS}} : undef
            (pl-if (pl-ref (pl-gethash (pl-aref @_ 0) "_FIELDS")) (pl-cast-@ (pl-gethash (pl-aref @_ 0) "_FIELDS")) (pl-undef))
      
    )
  )
)

;; ################################################################################
;; # meta_info
;; ################################################################################
;; sub meta_info { ... }
(pl-sub pl-meta_info (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; $_[0]->{_FFLAGS} ? @{ $_[0]->{_FFLAGS} } : undef
            (pl-if (pl-gethash (pl-aref @_ 0) "_FFLAGS") (pl-cast-@ (pl-gethash (pl-aref @_ 0) "_FFLAGS")) (pl-undef))
      
    )
  )
)

;; sub is_quoted { ... }
(pl-sub pl-is_quoted (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; return unless (defined $_[0]->{_FFLAGS})
      (pl-unless ;; PARSE ERROR: Handle single node of unknown type. Dump:       (pl-return))
      
      ;; return if( $_[1] =~ /\D/ or $_[1] < 0 or  $_[1] > $#{ $_[0]->{_FFLAGS} } )
      (pl-if ;; PARSE ERROR: Handle single node of unknown type. Dump:       (pl-return))
      
      ;; $_[0]->{_FFLAGS}->[$_[1]] & IS_QUOTED ? 1 : 0
            (pl-if (pcl:pl-bit-and (pl-aref (pl-gethash (pl-aref @_ 0) "_FFLAGS") (pl-aref @_ 1)) (pl-IS_QUOTED)) 1 0)
      
    )
  )
)

;; sub is_binary { ... }
(pl-sub pl-is_binary (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; return unless (defined $_[0]->{_FFLAGS})
      (pl-unless ;; PARSE ERROR: Handle single node of unknown type. Dump:       (pl-return))
      
      ;; return if( $_[1] =~ /\D/ or $_[1] < 0 or  $_[1] > $#{ $_[0]->{_FFLAGS} } )
      (pl-if ;; PARSE ERROR: Handle single node of unknown type. Dump:       (pl-return))
      
      ;; $_[0]->{_FFLAGS}->[$_[1]] & IS_BINARY ? 1 : 0
            (pl-if (pcl:pl-bit-and (pl-aref (pl-gethash (pl-aref @_ 0) "_FFLAGS") (pl-aref @_ 1)) (pl-IS_BINARY)) 1 0)
      
    )
  )
)

;; sub is_missing { ... }
(pl-sub pl-is_missing (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($idx (make-pl-box nil)) ($val (make-pl-box nil)))
        ;; my ($self, $idx, $val) = @_
                (pcl:pl-setf (vector $self $idx $val) @_)
        
        ;; return unless $self->{keep_meta_info}
        (pl-unless         (pl-gethash-deref $self "keep_meta_info")         (pl-return))
        
        ;; $idx < 0 || !ref $self->{_FFLAGS} and return
                (pcl:pl-and (pcl:pl-|| (pcl:pl-< $idx 0) (pl-! (pl-gethash-deref (pl-ref $self) "_FFLAGS"))) (pl-return))
        
        ;; $idx >= @{$self->{_FFLAGS}} and return 1
                (pcl:pl-and (pcl:pl->= $idx (pl-cast-@ (pl-gethash-deref $self "_FFLAGS"))) (pl-return 1))
        
        ;; $self->{_FFLAGS}[$idx] & IS_MISSING ? 1 : 0
                (pl-if (pcl:pl-bit-and (pl-aref (pl-gethash-deref $self "_FFLAGS") $idx) (pl-IS_MISSING)) 1 0)
        
      )
    )
  )
)

;; ################################################################################
;; # combine
;; ################################################################################
;; *combine = \&_combine
(pcl:pl-setf *combine (pl-backslash &_combine))

;; sub _combine { ... }
(pl-sub pl-_combine (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) (@fields (make-pl-box nil)) ($str (make-pl-box nil)))
        ;; my ($self, @fields) = @_
                (pcl:pl-setf (vector $self @fields) @_)
        
        ;; my $str  = ""
                (pcl:pl-setf $str "")
        
        ;; $self->{_FIELDS} = \@fields
                (pcl:pl-setf (pl-gethash-deref $self "_FIELDS") (pl-backslash @fields))
        
        ;; $self->{_STATUS} = (@fields > 0) && $self->__combine(\$str, \@fields, 0)
                (pcl:pl-setf (pl-gethash-deref $self "_STATUS") (pcl:pl-&& (pcl:pl-> @fields 0) (pl-method-call $self '__combine (pl-backslash $str) (pl-backslash @fields) 0)))
        
        ;; $self->{_STRING} = \$str
                (pcl:pl-setf (pl-gethash-deref $self "_STRING") (pl-backslash $str))
        
        ;; $self->{_STATUS}
                (pl-gethash-deref $self "_STATUS")
        
      )
    )
  )
)

;; ################################################################################
;; # parse
;; ################################################################################
;; *parse = \&_parse
(pcl:pl-setf *parse (pl-backslash &_parse))

;; sub _parse { ... }
(pl-sub pl-_parse (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($str (make-pl-box nil)) ($fields (make-pl-box nil)) ($fflags (make-pl-box nil)))
        ;; my ($self, $str) = @_
                (pcl:pl-setf (vector $self $str) @_)
        
        ;; ref $str and croak ($self->SetDiag (1500))
                (pcl:pl-and (pl-ref $str) (pl-croak (pl-method-call $self 'SetDiag 1500)))
        
        ;; my $fields = []
                (pcl:pl-setf $fields (make-array 0 :adjustable t :fill-pointer 0))
        
        ;; my $fflags = []
                (pcl:pl-setf $fflags (make-array 0 :adjustable t :fill-pointer 0))
        
        ;; $self->{_STRING} = \$str
                (pcl:pl-setf (pl-gethash-deref $self "_STRING") (pl-backslash $str))
        
        ;; if (defined $str && $self->__parse ($fields, $fflags, $str, 0)) {         $self->{_FIELDS} = $fields;         $self->{_FFLAGS} = $fflags;         $self->{_STATUS} = 1;         }     else {         $self->{_FIELDS} = undef;         $self->{_FFLAGS} = undef;         $self->{_STATUS} = 0;         }
        ;; if (defined $str && $self->__parse ($fields, $fflags, $str, 0))
        (pl-if         (pcl:pl-&& (pl-defined $str) (pl-method-call $self '__parse $fields $fflags $str 0))
          (progn
            ;; $self->{_FIELDS} = $fields
                        (pcl:pl-setf (pl-gethash-deref $self "_FIELDS") $fields)
            
            ;; $self->{_FFLAGS} = $fflags
                        (pcl:pl-setf (pl-gethash-deref $self "_FFLAGS") $fflags)
            
            ;; $self->{_STATUS} = 1
                        (pcl:pl-setf (pl-gethash-deref $self "_STATUS") 1)
            
          )
          ;; else
          (progn
            ;; $self->{_FIELDS} = undef
                        (pcl:pl-setf (pl-gethash-deref $self "_FIELDS") (pl-undef))
            
            ;; $self->{_FFLAGS} = undef
                        (pcl:pl-setf (pl-gethash-deref $self "_FFLAGS") (pl-undef))
            
            ;; $self->{_STATUS} = 0
                        (pcl:pl-setf (pl-gethash-deref $self "_STATUS") 0)
            
          )
        )
        
        ;; $self->{_STATUS}
                (pl-gethash-deref $self "_STATUS")
        
      )
    )
  )
)

;; sub column_names { ... }
(pl-sub pl-column_names (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) (@columns (make-pl-box nil)))
        ;; my ( $self, @columns ) = @_
                (pcl:pl-setf (vector $self @columns) @_)
        
        ;; @columns or return defined $self->{_COLUMN_NAMES} ? @{$self->{_COLUMN_NAMES}} : ()
                (pcl:pl-or @columns (pl-return (pl-if (pl-gethash-deref (pl-defined $self) "_COLUMN_NAMES") (pl-cast-@ (pl-gethash-deref $self "_COLUMN_NAMES")) (progn ))))
        
        ;; @columns == 1 && ! defined $columns[0] and return $self->{_COLUMN_NAMES} = undef
                (pcl:pl-and (pcl:pl-&& (pcl:pl-== @columns 1) (pl-! (pl-defined (pl-aref @columns 0)))) (pl-return (pcl:pl-setf (pl-gethash-deref $self "_COLUMN_NAMES") (pl-undef))))
        
        ;; if ( @columns == 1 && ref $columns[0] eq "ARRAY" ) {         @columns = @{ $columns[0] };     }     elsif ( join "", map { defined $_ ? ref $_ : "" } @columns ) {         croak $self->SetDiag( 3001 );     }
        ;; if (@columns == 1 && ref $columns[0] eq "ARRAY")
        (pl-if         (pcl:pl-&& (pcl:pl-== @columns 1) (pcl:pl-str-eq (pl-ref (pl-aref @columns 0)) "ARRAY"))
          (progn
            ;; @columns = @{ $columns[0] }
                        (pcl:pl-setf @columns (pl-cast-@ (pl-aref @columns 0)))
            
          )
          (defun --anon-block-73-- ($_)
            ;; defined $_ ? ref $_ : ""
                        (pl-if (pl-defined $_) (pl-ref $_) "")
            
          )
          
          ;; elsif (join "", map { defined $_ ? ref $_ : "" } @columns)
          (pl-if           (pl-join "" (pl-map #'--anon-block-73-- @columns))
            (progn
              ;; croak $self->SetDiag( 3001 )
                            (pl-croak (pl-method-call $self 'SetDiag 3001))
              
            )
            nil
          )
        )
        
        ;; if ( $self->{_BOUND_COLUMNS} && @columns != @{$self->{_BOUND_COLUMNS}} ) {         croak $self->SetDiag( 3003 );     }
        ;; if ($self->{"_BOUND_COLUMNS"} && @columns != @{$self->{"_BOUND_COLUMNS"}})
        (pl-if         (pcl:pl-&& (pl-gethash-deref $self "_BOUND_COLUMNS") (pcl:pl-!= @columns (pl-cast-@ (pl-gethash-deref $self "_BOUND_COLUMNS"))))
          (progn
            ;; croak $self->SetDiag( 3003 )
                        (pl-croak (pl-method-call $self 'SetDiag 3003))
            
          )
          nil
        )
        
        (defun --anon-block-74-- ($_)
          ;; defined $_ ? $_ : "\cAUNDEF\cA"
                    (pl-if (pl-defined $_) $_ "\\cAUNDEF\\cA")
          
        )
        
        ;; $self->{_COLUMN_NAMES} = [ map { defined $_ ? $_ : "\cAUNDEF\cA" } @columns ]
                (pcl:pl-setf (pl-gethash-deref $self "_COLUMN_NAMES") (make-array 1 :adjustable t :fill-pointer t :initial-contents (list (let ((*wantarray* t)) (pl-map #'--anon-block-74-- @columns)))))
        
        ;; @{ $self->{_COLUMN_NAMES} }
                (pl-cast-@ (pl-gethash-deref $self "_COLUMN_NAMES"))
        
      )
    )
  )
)

;; sub header { ... }
(pl-sub pl-header (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($fh (make-pl-box nil)) (@args (make-pl-box nil)) (@seps (make-pl-box nil)) (%args (make-pl-box nil)) ($hdr (make-pl-box nil)) (%sep (make-pl-box nil)) ($sep (make-pl-box nil)) ($enc (make-pl-box nil)) ($l (make-pl-box nil)) ($x (make-pl-box nil)) ($ahead (make-pl-box nil)) ($eol (make-pl-box nil)) ($hr (make-pl-box nil)) ($h (make-pl-box nil)) ($row (make-pl-box nil)) (@hdr (make-pl-box nil)) (%hdr (make-pl-box nil)))
        ;; my ($self, $fh, @args) = @_
                (pcl:pl-setf (vector $self $fh @args) @_)
        
        ;; $fh or croak ($self->SetDiag (1014))
                (pcl:pl-or $fh (pl-croak (pl-method-call $self 'SetDiag 1014)))
        
        ;; my (@seps, %args) (bare declaration)
        
        ;; for (@args) {         if (ref $_ eq "ARRAY") {             push @seps, @$_;             next;             }         if (ref $_ eq "HASH") {             %args = %$_;             next;             }         croak (q{usage: $csv->header ($fh, [ seps ], { options })});         }
        (pl-foreach ($_ (vector         @args))
          ;; if (ref $_ eq "ARRAY") {             push @seps, @$_;             next;             }
          ;; if (ref $_ eq "ARRAY")
          (pl-if           (pcl:pl-str-eq (pl-ref $_) "ARRAY")
            (progn
              ;; push @seps, @$_
                            (pl-push @seps (pl-cast-@ $_))
              
              ;; next
                            (pl-next)
              
            )
            nil
          )
          
          ;; if (ref $_ eq "HASH") {             %args = %$_;             next;             }
          ;; if (ref $_ eq "HASH")
          (pl-if           (pcl:pl-str-eq (pl-ref $_) "HASH")
            (progn
              ;; %args = %$_
                            (pcl:pl-setf %args (pl-cast-% $_))
              
              ;; next
                            (pl-next)
              
            )
            nil
          )
          
          ;; croak (q{usage: $csv->header ($fh, [ seps ], { options })})
          ;; PARSE ERROR: Handle single node of unknown type. Dump:
          
        )
        
        ;; defined $args{munge} && !defined $args{munge_column_names} and         $args{munge_column_names} = $args{munge}
                (pcl:pl-and (pcl:pl-&& (pl-defined (pl-gethash %args "munge")) (pl-! (pl-defined (pl-gethash %args "munge_column_names")))) (pcl:pl-setf (pl-gethash %args "munge_column_names") (pl-gethash %args "munge")))
        
        ;; defined $args{detect_bom}         or $args{detect_bom}         = 1
                (pcl:pl-or (pl-defined (pl-gethash %args "detect_bom")) (pcl:pl-setf (pl-gethash %args "detect_bom") 1))
        
        ;; defined $args{set_column_names}   or $args{set_column_names}   = 1
                (pcl:pl-or (pl-defined (pl-gethash %args "set_column_names")) (pcl:pl-setf (pl-gethash %args "set_column_names") 1))
        
        ;; defined $args{munge_column_names} or $args{munge_column_names} = "lc"
                (pcl:pl-or (pl-defined (pl-gethash %args "munge_column_names")) (pcl:pl-setf (pl-gethash %args "munge_column_names") "lc"))
        
        ;; $self->{_RECNO}        = 0
                (pcl:pl-setf (pl-gethash-deref $self "_RECNO") 0)
        
        ;; $self->{_AHEAD}        = undef
                (pcl:pl-setf (pl-gethash-deref $self "_AHEAD") (pl-undef))
        
        ;; $self->{_COLUMN_NAMES} = undef if $args{set_column_names}
        (pl-if         (pl-gethash %args "set_column_names")         (pcl:pl-setf (pl-gethash-deref $self "_COLUMN_NAMES") (pl-undef)))
        
        ;; $self->{_BOUND_COLUMNS}    = undef if $args{set_column_names}
        (pl-if         (pl-gethash %args "set_column_names")         (pcl:pl-setf (pl-gethash-deref $self "_BOUND_COLUMNS") (pl-undef)))
        
        ;; $self->_cache_set($_cache_id{'_has_ahead'}, 0)
                (pl-method-call $self '_cache_set (pl-gethash %_cache_id "_has_ahead") 0)
        
        ;; if (defined $args{sep_set}) {         ref $args{sep_set} eq "ARRAY" or             croak ($self->_SetDiagInfo (1500, "sep_set should be an array ref"));         @seps =  @{$args{sep_set}};     }
        ;; if (defined $args{"sep_set"})
        (pl-if         (pl-defined (pl-gethash %args "sep_set"))
          (progn
            ;; ref $args{sep_set} eq "ARRAY" or             croak ($self->_SetDiagInfo (1500, "sep_set should be an array ref"))
                        (pcl:pl-or (pcl:pl-str-eq (pl-ref (pl-gethash %args "sep_set")) "ARRAY") (pl-croak (pl-method-call $self '_SetDiagInfo 1500 "sep_set should be an array ref")))
            
            ;; @seps =  @{$args{sep_set}}
                        (pcl:pl-setf @seps (pl-cast-@ (pl-gethash %args "sep_set")))
            
          )
          nil
        )
        
        ;; $^O eq "MSWin32" and binmode $fh
                (pcl:pl-and (pcl:pl-str-eq |$^O| "MSWin32") (pl-binmode $fh))
        
        ;; my $hdr = <$fh>
                (pcl:pl-setf $hdr (pl-readline $fh))
        
        ;; defined $hdr && $hdr ne "" or croak ($self->SetDiag (1010))
                (pcl:pl-or (pcl:pl-&& (pl-defined $hdr) (pcl:pl-str-ne $hdr "")) (pl-croak (pl-method-call $self 'SetDiag 1010)))
        
        ;; my %sep (bare declaration)
        
        ;; @seps or @seps = (",", ";")
                (pcl:pl-or @seps (pcl:pl-setf @seps (vector "," ";")))
        
        ;; foreach my $sep (@seps) {         index ($hdr, $sep) >= 0 and $sep{$sep}++;         }
        (pl-foreach ($sep (vector         @seps))
          ;; index ($hdr, $sep) >= 0 and $sep{$sep}++
                    (pcl:pl-and (pcl:pl->= (pl-index $hdr $sep) 0) (pl-post++ (pl-gethash %sep $sep)))
          
        )
        
        ;; keys %sep >= 2 and croak ($self->SetDiag (1011))
                (pcl:pl-and (pl-keys (pcl:pl->= %sep 2)) (pl-croak (pl-method-call $self 'SetDiag 1011)))
        
        ;; $self->sep (keys %sep)
                (pl-method-call $self 'sep (pl-keys %sep))
        
        ;; my $enc = ""
                (pcl:pl-setf $enc "")
        
        ;; if ($args{detect_bom}) { # UTF-7 is not supported            if ($hdr =~ s/^\x00\x00\xfe\xff//) { $enc = "utf-32be"   }         elsif ($hdr =~ s/^\xff\xfe\x00\x00//) { $enc = "utf-32le"   }         elsif ($hdr =~ s/^\xfe\xff//)         { $enc = "utf-16be"   }         elsif ($hdr =~ s/^\xff\xfe//)         { $enc = "utf-16le"   }         elsif ($hdr =~ s/^\xef\xbb\xbf//)     { $enc = "utf-8"      }         elsif ($hdr =~ s/^\xf7\x64\x4c//)     { $enc = "utf-1"      }         elsif ($hdr =~ s/^\xdd\x73\x66\x73//) { $enc = "utf-ebcdic" }         elsif ($hdr =~ s/^\x0e\xfe\xff//)     { $enc = "scsu"       }         elsif ($hdr =~ s/^\xfb\xee\x28//)     { $enc = "bocu-1"     }         elsif ($hdr =~ s/^\x84\x31\x95\x33//) { $enc = "gb-18030"   }         elsif ($hdr =~ s/^\x{feff}//)         { $enc = ""           }          $self->{ENCODING} = $enc ? uc $enc : undef;          $hdr eq "" and croak ($self->SetDiag (1010));          if ($enc) {             $ebcdic && $enc eq "utf-ebcdic" and $enc = "";             if ($enc =~ m/([13]).le$/) {                 my $l = 0 + $1;                 my $x;                 $hdr .= "\0" x $l;                 read $fh, $x, $l;                 }             if ($enc) {             if ($enc ne "utf-8") {                require Encode;                $hdr = Encode::decode ($enc, $hdr);                }             binmode $fh, ":encoding($enc)";             }             }         }
        ;; if ($args{"detect_bom"})
        (pl-if         (pl-gethash %args "detect_bom")
          (progn
            ;; if ($hdr =~ s/^\x00\x00\xfe\xff//) { $enc = "utf-32be"   }         elsif ($hdr =~ s/^\xff\xfe\x00\x00//) { $enc = "utf-32le"   }         elsif ($hdr =~ s/^\xfe\xff//)         { $enc = "utf-16be"   }         elsif ($hdr =~ s/^\xff\xfe//)         { $enc = "utf-16le"   }         elsif ($hdr =~ s/^\xef\xbb\xbf//)     { $enc = "utf-8"      }         elsif ($hdr =~ s/^\xf7\x64\x4c//)     { $enc = "utf-1"      }         elsif ($hdr =~ s/^\xdd\x73\x66\x73//) { $enc = "utf-ebcdic" }         elsif ($hdr =~ s/^\x0e\xfe\xff//)     { $enc = "scsu"       }         elsif ($hdr =~ s/^\xfb\xee\x28//)     { $enc = "bocu-1"     }         elsif ($hdr =~ s/^\x84\x31\x95\x33//) { $enc = "gb-18030"   }         elsif ($hdr =~ s/^\x{feff}//)         { $enc = ""           }
            ;; if ($hdr =~ s/^\x00\x00\xfe\xff//)
            (pl-if             (pcl:pl-=~ $hdr (pl-subst "^\\x00\\x00\\xfe\\xff" ""))
              (progn
                ;; $enc = "utf-32be"
                                (pcl:pl-setf $enc "utf-32be")
                
              )
              ;; elsif ($hdr =~ s/^\xff\xfe\x00\x00//)
              (pl-if               (pcl:pl-=~ $hdr (pl-subst "^\\xff\\xfe\\x00\\x00" ""))
                (progn
                  ;; $enc = "utf-32le"
                                    (pcl:pl-setf $enc "utf-32le")
                  
                )
                ;; elsif ($hdr =~ s/^\xfe\xff//)
                (pl-if                 (pcl:pl-=~ $hdr (pl-subst "^\\xfe\\xff" ""))
                  (progn
                    ;; $enc = "utf-16be"
                                        (pcl:pl-setf $enc "utf-16be")
                    
                  )
                  ;; elsif ($hdr =~ s/^\xff\xfe//)
                  (pl-if                   (pcl:pl-=~ $hdr (pl-subst "^\\xff\\xfe" ""))
                    (progn
                      ;; $enc = "utf-16le"
                                            (pcl:pl-setf $enc "utf-16le")
                      
                    )
                    ;; elsif ($hdr =~ s/^\xef\xbb\xbf//)
                    (pl-if                     (pcl:pl-=~ $hdr (pl-subst "^\\xef\\xbb\\xbf" ""))
                      (progn
                        ;; $enc = "utf-8"
                                                (pcl:pl-setf $enc "utf-8")
                        
                      )
                      ;; elsif ($hdr =~ s/^\xf7\x64\x4c//)
                      (pl-if                       (pcl:pl-=~ $hdr (pl-subst "^\\xf7\\x64\\x4c" ""))
                        (progn
                          ;; $enc = "utf-1"
                                                    (pcl:pl-setf $enc "utf-1")
                          
                        )
                        ;; elsif ($hdr =~ s/^\xdd\x73\x66\x73//)
                        (pl-if                         (pcl:pl-=~ $hdr (pl-subst "^\\xdd\\x73\\x66\\x73" ""))
                          (progn
                            ;; $enc = "utf-ebcdic"
                                                        (pcl:pl-setf $enc "utf-ebcdic")
                            
                          )
                          ;; elsif ($hdr =~ s/^\x0e\xfe\xff//)
                          (pl-if                           (pcl:pl-=~ $hdr (pl-subst "^\\x0e\\xfe\\xff" ""))
                            (progn
                              ;; $enc = "scsu"
                                                            (pcl:pl-setf $enc "scsu")
                              
                            )
                            ;; elsif ($hdr =~ s/^\xfb\xee\x28//)
                            (pl-if                             (pcl:pl-=~ $hdr (pl-subst "^\\xfb\\xee\\x28" ""))
                              (progn
                                ;; $enc = "bocu-1"
                                                                (pcl:pl-setf $enc "bocu-1")
                                
                              )
                              ;; elsif ($hdr =~ s/^\x84\x31\x95\x33//)
                              (pl-if                               (pcl:pl-=~ $hdr (pl-subst "^\\x84\\x31\\x95\\x33" ""))
                                (progn
                                  ;; $enc = "gb-18030"
                                                                    (pcl:pl-setf $enc "gb-18030")
                                  
                                )
                                ;; elsif ($hdr =~ s/^\x{feff}//)
                                (pl-if                                 (pcl:pl-=~ $hdr (pl-subst "^\\x{feff}" ""))
                                  (progn
                                    ;; $enc = ""
                                                                        (pcl:pl-setf $enc "")
                                    
                                  )
                                  nil
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
            
            ;; $self->{ENCODING} = $enc ? uc $enc : undef
            ;; PARSE ERROR: Fell through. Missing case: [
            
            ;; $hdr eq "" and croak ($self->SetDiag (1010))
                        (pcl:pl-and (pcl:pl-str-eq $hdr "") (pl-croak (pl-method-call $self 'SetDiag 1010)))
            
            ;; if ($enc) {             $ebcdic && $enc eq "utf-ebcdic" and $enc = "";             if ($enc =~ m/([13]).le$/) {                 my $l = 0 + $1;                 my $x;                 $hdr .= "\0" x $l;                 read $fh, $x, $l;                 }             if ($enc) {             if ($enc ne "utf-8") {                require Encode;                $hdr = Encode::decode ($enc, $hdr);                }             binmode $fh, ":encoding($enc)";             }             }
            ;; if ($enc)
            (pl-if             $enc
              (progn
                ;; $ebcdic && $enc eq "utf-ebcdic" and $enc = ""
                                (pcl:pl-and (pcl:pl-&& $ebcdic (pcl:pl-str-eq $enc "utf-ebcdic")) (pcl:pl-setf $enc ""))
                
                ;; if ($enc =~ m/([13]).le$/) {                 my $l = 0 + $1;                 my $x;                 $hdr .= "\0" x $l;                 read $fh, $x, $l;                 }
                ;; if ($enc =~ m/([13]).le$/)
                (pl-if                 (pcl:pl-=~ $enc (pl-regex "m/([13]).le$/"))
                  (progn
                    ;; my $l = 0 + $1
                                        (pcl:pl-setf $l (pcl:pl-+ 0 $1))
                    
                    ;; my $x (bare declaration)
                    
                    ;; $hdr .= "\0" x $l
                                        (pcl:pl-.= $hdr (pcl:pl-str-x " " $l))
                    
                    ;; read $fh, $x, $l
                                        (pl-read $fh $x $l)
                    
                  )
                  nil
                )
                
                ;; if ($enc) {             if ($enc ne "utf-8") {                require Encode;                $hdr = Encode::decode ($enc, $hdr);                }             binmode $fh, ":encoding($enc)";             }
                ;; if ($enc)
                (pl-if                 $enc
                  (progn
                    ;; if ($enc ne "utf-8") {                require Encode;                $hdr = Encode::decode ($enc, $hdr);                }
                    ;; if ($enc ne "utf-8")
                    (pl-if                     (pcl:pl-str-ne $enc "utf-8")
                      (progn
                        ;; require Encode
                        (eval-when (:compile-toplevel :load-toplevel :execute)
                          (pl-require "Encode"))
                        
                        ;; $hdr = Encode::decode ($enc, $hdr)
                                                (pcl:pl-setf $hdr (Encode::pl-decode $enc $hdr))
                        
                      )
                      nil
                    )
                    
                    ;; binmode $fh, ":encoding($enc)"
                                        (pl-binmode $fh (pl-string-concat ":encoding(" $enc ")"))
                    
                  )
                  nil
                )
                
              )
              nil
            )
            
          )
          nil
        )
        
        ;; my ($ahead, $eol) (bare declaration)
        
        ;; if ($hdr and $hdr =~ s/\Asep=(\S)([\r\n]+)//i) { # Also look in xs:Parse         $self->sep ($1);         length $hdr or $hdr = <$fh>;     }
        ;; if ($hdr and $hdr =~ s/\Asep=(\S)([\r\n]+)//i)
        (pl-if         (pcl:pl-and $hdr (pcl:pl-=~ $hdr (pl-subst "\\Asep=(\\S)([\\r\\n]+)" "" :i)))
          (progn
            ;; $self->sep ($1)
                        (pl-method-call $self 'sep $1)
            
            ;; length $hdr or $hdr = <$fh>
                        (pcl:pl-or (pl-length $hdr) (pcl:pl-setf $hdr (pl-readline $fh)))
            
          )
          nil
        )
        
        ;; if ($hdr =~ s/^([^\r\n]+)([\r\n]+)([^\r\n].+)\z/$1/s) {         $eol   = $2;         $ahead = $3;     }
        ;; if ($hdr =~ s/^([^\r\n]+)([\r\n]+)([^\r\n].+)\z/$1/s)
        (pl-if         (pcl:pl-=~ $hdr (pl-subst "^([^\\r\\n]+)([\\r\\n]+)([^\\r\\n].+)\\z" "$1" :s))
          (progn
            ;; $eol   = $2
                        (pcl:pl-setf $eol $2)
            
            ;; $ahead = $3
                        (pcl:pl-setf $ahead $3)
            
          )
          nil
        )
        
        ;; my $hr = \$hdr
                (pcl:pl-setf $hr (pl-backslash $hdr))
        
        ;; open my $h, "<", $hr or croak ($self->SetDiag (1010))
                (pcl:pl-or (pl-open $h "<" $hr) (pl-croak (pl-method-call $self 'SetDiag 1010)))
        
        ;; my $row = $self->getline ($h) or croak
                (pcl:pl-or (pcl:pl-setf $row (pl-method-call $self 'getline $h)) (pl-croak))
        
        ;; close $h
                (pl-close $h)
        
        ;; if (   $args{'munge_column_names'} eq "lc") {         $_ = lc for @{$row};     }     elsif ($args{'munge_column_names'} eq "uc") {         $_ = uc for @{$row};     }     elsif ($args{'munge_column_names'} eq "db") {         for (@{$row}) {             s/\W+/_/g;             s/^_+//;             $_ = lc;         }     }
        ;; if ($args{'munge_column_names'} eq "lc")
        (pl-if         (pcl:pl-str-eq (pl-gethash %args "munge_column_names") "lc")
          (progn
            ;; $_ = lc for @{$row}
            (pl-for             (pl-cast-@ $row)             (pcl:pl-setf $_ (pl-lc $_)))
            
          )
          ;; elsif ($args{'munge_column_names'} eq "uc")
          (pl-if           (pcl:pl-str-eq (pl-gethash %args "munge_column_names") "uc")
            (progn
              ;; $_ = uc for @{$row}
              (pl-for               (pl-cast-@ $row)               (pcl:pl-setf $_ (pl-uc $_)))
              
            )
            ;; elsif ($args{'munge_column_names'} eq "db")
            (pl-if             (pcl:pl-str-eq (pl-gethash %args "munge_column_names") "db")
              (progn
                ;; for (@{$row}) {             s/\W+/_/g;             s/^_+//;             $_ = lc;         }
                (pl-foreach ($_                 (pl-cast-@ $row))
                  ;; s/\W+/_/g
                                    (pcl:pl-=~ $_ (pl-subst "\\W+" "_" :g))
                  
                  ;; s/^_+//
                                    (pcl:pl-=~ $_ (pl-subst "^_+" ""))
                  
                  ;; $_ = lc
                                    (pcl:pl-setf $_ (pl-lc $_))
                  
                )
                
              )
              nil
            )
          )
        )
        
        ;; if ($ahead) { # Must be after getline, which creates the cache         $self->_cache_set ($_cache_id{_has_ahead}, 1);         $self->{_AHEAD} = $ahead;         $eol =~ m/^\r([^\n]|\z)/ and $self->eol ($eol);         }
        ;; if ($ahead)
        (pl-if         $ahead
          (progn
            ;; $self->_cache_set ($_cache_id{_has_ahead}, 1)
                        (pl-method-call $self '_cache_set (pl-gethash %_cache_id "_has_ahead") 1)
            
            ;; $self->{_AHEAD} = $ahead
                        (pcl:pl-setf (pl-gethash-deref $self "_AHEAD") $ahead)
            
            ;; $eol =~ m/^\r([^\n]|\z)/ and $self->eol ($eol)
                        (pcl:pl-and (pcl:pl-=~ $eol (pl-regex "m/^\\r([^\\n]|\\z)/")) (pl-method-call $self 'eol $eol))
            
          )
          nil
        )
        
        ;; my @hdr = @$row
                (pcl:pl-setf @hdr (pl-cast-@ $row))
        
        (defun --anon-block-75-- ($_)
          ;; $args{munge_column_names}->($_)
                    (pl-funcall-ref (pl-gethash %args "munge_column_names") $_)
          
        )
        
        ;; ref $args{munge_column_names} eq "CODE" and         @hdr = map { $args{munge_column_names}->($_)       } @hdr
                (pcl:pl-and (pcl:pl-str-eq (pl-ref (pl-gethash %args "munge_column_names")) "CODE") (pcl:pl-setf @hdr (let ((*wantarray* t)) (pl-map #'--anon-block-75-- @hdr))))
        
        (defun --anon-block-76-- ($_)
          ;; $args{munge_column_names}->{$_} || $_
                    (pcl:pl-|| (pl-gethash (pl-gethash %args "munge_column_names") $_) $_)
          
        )
        
        ;; ref $args{munge_column_names} eq "HASH" and         @hdr = map { $args{munge_column_names}->{$_} || $_ } @hdr
                (pcl:pl-and (pcl:pl-str-eq (pl-ref (pl-gethash %args "munge_column_names")) "HASH") (pcl:pl-setf @hdr (let ((*wantarray* t)) (pl-map #'--anon-block-76-- @hdr))))
        
        ;; my %hdr (bare declaration)
        
        ;; $hdr{$_}++ for @hdr
        (pl-for         @hdr         (pl-post++ (pl-gethash %hdr $_)))
        
        ;; exists $hdr{''} and croak ($self->SetDiag (1012))
                (pcl:pl-and (pl-exists (pl-gethash %hdr "")) (pl-croak (pl-method-call $self 'SetDiag 1012)))
        
        ;; unless (keys %hdr == @hdr) {         croak ($self->_SetDiagInfo (1013, join ", " =>             map { "$_ ($hdr{$_})" } grep { $hdr{$_} > 1 } keys %hdr));         }
        ;; unless (keys %hdr == @hdr)
        (pl-if (pl-not         (pl-keys (pcl:pl-== %hdr @hdr)))
          (progn
            (defun --anon-block-77-- ($_)
              ;; "$_ ($hdr{$_})"
                            (pl-string-concat $_ " (" (pl-gethash %hdr ) ")")
              
            )
            
            (defun --anon-block-78-- ($_)
              ;; $hdr{$_} > 1
                            (pcl:pl-> (pl-gethash %hdr $_) 1)
              
            )
            
            ;; croak ($self->_SetDiagInfo (1013, join ", " =>             map { "$_ ($hdr{$_})" } grep { $hdr{$_} > 1 } keys %hdr))
                        (pl-croak (pl-method-call $self '_SetDiagInfo 1013 (pl-join ", " (pl-map #'--anon-block-77-- (let ((*wantarray* t)) (pl-grep #'--anon-block-78-- (let ((*wantarray* t)) (pl-keys %hdr))))))))
            
          )
          nil
        )
        
        ;; $args{set_column_names} and $self->column_names (@hdr)
                (pcl:pl-and (pl-gethash %args "set_column_names") (pl-method-call $self 'column_names @hdr))
        
        ;; wantarray ? @hdr : $self
                (pl-if (pl-wantarray) @hdr $self)
        
      )
    )
  )
)

;; sub bind_columns { ... }
(pl-sub pl-bind_columns (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) (@refs (make-pl-box nil)))
        ;; my ( $self, @refs ) = @_
                (pcl:pl-setf (vector $self @refs) @_)
        
        ;; @refs or return defined $self->{_BOUND_COLUMNS} ? @{$self->{_BOUND_COLUMNS}} : undef
                (pcl:pl-or @refs (pl-return (pl-if (pl-gethash-deref (pl-defined $self) "_BOUND_COLUMNS") (pl-cast-@ (pl-gethash-deref $self "_BOUND_COLUMNS")) (pl-undef))))
        
        ;; @refs == 1 && ! defined $refs[0] and return $self->{_BOUND_COLUMNS} = undef
                (pcl:pl-and (pcl:pl-&& (pcl:pl-== @refs 1) (pl-! (pl-defined (pl-aref @refs 0)))) (pl-return (pcl:pl-setf (pl-gethash-deref $self "_BOUND_COLUMNS") (pl-undef))))
        
        ;; if ( $self->{_COLUMN_NAMES} && @refs != @{$self->{_COLUMN_NAMES}} ) {         croak $self->SetDiag( 3003 );     }
        ;; if ($self->{"_COLUMN_NAMES"} && @refs != @{$self->{"_COLUMN_NAMES"}})
        (pl-if         (pcl:pl-&& (pl-gethash-deref $self "_COLUMN_NAMES") (pcl:pl-!= @refs (pl-cast-@ (pl-gethash-deref $self "_COLUMN_NAMES"))))
          (progn
            ;; croak $self->SetDiag( 3003 )
                        (pl-croak (pl-method-call $self 'SetDiag 3003))
            
          )
          nil
        )
        
        ;; if ( grep { ref $_ ne "SCALAR" } @refs ) { # why don't use grep?         croak $self->SetDiag( 3004 );     }
        (defun --anon-block-79-- ($_)
          ;; ref $_ ne "SCALAR"
                    (pcl:pl-str-ne (pl-ref $_) "SCALAR")
          
        )
        
        ;; if (grep { ref $_ ne "SCALAR" } @refs)
        (pl-if         (pl-grep #'--anon-block-79-- @refs)
          (progn
            ;; croak $self->SetDiag( 3004 )
                        (pl-croak (pl-method-call $self 'SetDiag 3004))
            
          )
          nil
        )
        
        ;; $self->_set_attr_N("_is_bound", scalar @refs)
                (pl-method-call $self '_set_attr_N "_is_bound" (pl-scalar @refs))
        
        ;; $self->{_BOUND_COLUMNS} = [ @refs ]
                (pcl:pl-setf (pl-gethash-deref $self "_BOUND_COLUMNS") (make-array 1 :adjustable t :fill-pointer t :initial-contents (list @refs)))
        
        ;; @refs
                @refs
        
      )
    )
  )
)

;; sub getline_hr { ... }
(pl-sub pl-getline_hr (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) (@args (make-pl-box nil)) (%hr (make-pl-box nil)) ($fr (make-pl-box nil)))
        ;; my ($self, @args, %hr) = @_
                (pcl:pl-setf (vector $self @args %hr) @_)
        
        ;; $self->{_COLUMN_NAMES} or croak ($self->SetDiag (3002))
                (pcl:pl-or (pl-gethash-deref $self "_COLUMN_NAMES") (pl-croak (pl-method-call $self 'SetDiag 3002)))
        
        ;; my $fr = $self->getline (@args) or return
                (pcl:pl-or (pcl:pl-setf $fr (pl-method-call $self 'getline @args)) (pl-return))
        
        ;; if (ref $self->{_FFLAGS}) { # missing         $self->{_FFLAGS}[$_] = IS_MISSING             for (@$fr ? $#{$fr} + 1 : 0) .. $#{$self->{_COLUMN_NAMES}};         @$fr == 1 && (!defined $fr->[0] || $fr->[0] eq "") and             $self->{_FFLAGS}[0] ||= IS_MISSING;         }
        ;; if (ref $self->{"_FFLAGS"})
        (pl-if         (pl-gethash-deref (pl-ref $self) "_FFLAGS")
          (progn
            ;; $self->{_FFLAGS}[$_] = IS_MISSING             for (@$fr ? $#{$fr} + 1 : 0) .. $#{$self->{_COLUMN_NAMES}}
            (pl-for             (pcl:pl-.. (pl-if (pl-cast-@ $fr) (pcl:pl-+ (pl-$# $fr) 1) 0) (pl-$# (pl-gethash-deref $self "_COLUMN_NAMES")))             (pcl:pl-setf (pl-aref (pl-gethash-deref $self "_FFLAGS") $_) (pl-IS_MISSING)))
            
            ;; @$fr == 1 && (!defined $fr->[0] || $fr->[0] eq "") and             $self->{_FFLAGS}[0] ||= IS_MISSING
                        (pcl:pl-and (pcl:pl-&& (pcl:pl-== (pl-cast-@ $fr) 1) (pcl:pl-|| (pl-! (pl-aref-deref (pl-defined $fr) 0)) (pcl:pl-str-eq (pl-aref-deref $fr 0) ""))) (pcl:pl-or-assign (pl-aref (pl-gethash-deref $self "_FFLAGS") 0) (pl-IS_MISSING)))
            
          )
          nil
        )
        
        ;; @hr{@{$self->{_COLUMN_NAMES}}} = @$fr
                (pcl:pl-setf (pl-hslice @hr (pl-cast-@ (pl-gethash-deref $self "_COLUMN_NAMES"))) (pl-cast-@ $fr))
        
        ;; \%hr
                (pl-backslash %hr)
        
      )
    )
  )
)

;; sub getline_hr_all { ... }
(pl-sub pl-getline_hr_all (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($io (make-pl-box nil)) (@args (make-pl-box nil)) (@cn (make-pl-box nil)) (%h (make-pl-box nil)))
        ;; my ( $self, $io, @args ) = @_
                (pcl:pl-setf (vector $self $io @args) @_)
        
        ;; unless ( $self->{_COLUMN_NAMES} ) {         croak $self->SetDiag( 3002 );     }
        ;; unless ($self->{"_COLUMN_NAMES"})
        (pl-if (pl-not         (pl-gethash-deref $self "_COLUMN_NAMES"))
          (progn
            ;; croak $self->SetDiag( 3002 )
                        (pl-croak (pl-method-call $self 'SetDiag 3002))
            
          )
          nil
        )
        
        ;; my @cn = @{$self->{_COLUMN_NAMES}}
                (pcl:pl-setf @cn (pl-cast-@ (pl-gethash-deref $self "_COLUMN_NAMES")))
        
        (defun --anon-block-80-- ($_)
          ;; my %h (bare declaration)
          
          ;; @h{ @cn } = @$_
                    (pcl:pl-setf (pl-hslice @h @cn) (pl-cast-@ $_))
          
          ;; \%h
                    (pl-backslash %h)
          
        )
        
        ;; return [ map { my %h; @h{ @cn } = @$_; \%h } @{ $self->getline_all( $io, @args ) } ]
                (pl-return (make-array 1 :adjustable t :fill-pointer t :initial-contents (list (let ((*wantarray* t)) (pl-map #'--anon-block-80-- (pl-cast-@ (let ((*wantarray* t)) (pl-method-call $self 'getline_all $io @args))))))))
        
      )
    )
  )
)

;; sub say { ... }
(pl-sub pl-say (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($io (make-pl-box nil)) (@f (make-pl-box nil)) ($eol (make-pl-box nil)) ($state (make-pl-box nil)))
        ;; my ($self, $io, @f) = @_
                (pcl:pl-setf (vector $self $io @f) @_)
        
        ;; my $eol = $self->eol
                (pcl:pl-setf $eol (pl-method-call $self 'eol))
        
        ;; $eol eq "" and $self->eol ($\ || $/)
                (pcl:pl-and (pcl:pl-str-eq $eol "") (pl-method-call $self 'eol (pcl:pl-|| |$\| |$/|)))
        
        ;; my $state = $self->print ($io, @f == 1 && !defined $f[0] ? undef : @f)
                (pcl:pl-setf $state (pl-method-call $self 'print $io (pl-if (pcl:pl-&& (pcl:pl-== @f 1) (pl-! (pl-defined (pl-aref @f 0)))) (pl-undef) @f)))
        
        ;; $self->eol ($eol)
                (pl-method-call $self 'eol $eol)
        
        ;; return $state
                (pl-return $state)
        
      )
    )
  )
)

;; sub print_hr { ... }
(pl-sub pl-print_hr (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($io (make-pl-box nil)) ($hr (make-pl-box nil)))
        ;; my ($self, $io, $hr) = @_
                (pcl:pl-setf (vector $self $io $hr) @_)
        
        ;; $self->{_COLUMN_NAMES} or croak($self->SetDiag(3009))
                (pcl:pl-or (pl-gethash-deref $self "_COLUMN_NAMES") (pl-croak (pl-method-call $self 'SetDiag 3009)))
        
        ;; ref $hr eq "HASH"      or croak($self->SetDiag(3010))
                (pcl:pl-or (pcl:pl-str-eq (pl-ref $hr) "HASH") (pl-croak (pl-method-call $self 'SetDiag 3010)))
        
        (defun --anon-block-81-- ($_)
          ;; $hr->{$_}
                    (pl-gethash-deref $hr $_)
          
        )
        
        ;; $self->print ($io, [ map { $hr->{$_} } $self->column_names ])
                (pl-method-call $self 'print $io (make-array 1 :adjustable t :fill-pointer t :initial-contents (list (let ((*wantarray* t)) (pl-map #'--anon-block-81-- (let ((*wantarray* t)) (pl-method-call $self 'column_names)))))))
        
      )
    )
  )
)

;; sub fragment { ... }
(pl-sub pl-fragment (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($io (make-pl-box nil)) ($spec (make-pl-box nil)) ($qd (make-pl-box nil)) ($qs (make-pl-box nil)) ($qr (make-pl-box nil)) ($qc (make-pl-box nil)) ($type (make-pl-box nil)) ($range (make-pl-box nil)) (@h (make-pl-box nil)) (@c (make-pl-box nil)) (@spec (make-pl-box nil)) ($min_row (make-pl-box nil)) ($max_row (make-pl-box nil)) ($tlr (make-pl-box nil)) ($tlc (make-pl-box nil)) ($brr (make-pl-box nil)) ($brc (make-pl-box nil)) ($r (make-pl-box nil)) ($row (make-pl-box nil)) (%row (make-pl-box nil)) ($lc (make-pl-box nil)) ($s (make-pl-box nil)) ($rr (make-pl-box nil)) (%h (make-pl-box nil)) (@r (make-pl-box nil)) ($eod (make-pl-box nil)) ($from (make-pl-box nil)) ($to (make-pl-box nil)))
        ;; my ($self, $io, $spec) = @_
                (pcl:pl-setf (vector $self $io $spec) @_)
        
        ;; my $qd = qr{\s* [0-9]+ \s* }x
        ;; PARSE ERROR: Handle single node of unknown type. Dump:
        
        ;; my $qs = qr{\s* (?: [0-9]+ | \* ) \s*}x
        ;; PARSE ERROR: Handle single node of unknown type. Dump:
        
        ;; my $qr = qr{$qd (?: - $qs )?}x
        ;; PARSE ERROR: Handle single node of unknown type. Dump:
        
        ;; my $qc = qr{$qr (?: ; $qr )*}x
        ;; PARSE ERROR: Handle single node of unknown type. Dump:
        
        ;; defined $spec && $spec =~ m{^ \s*         \x23 ? \s*                                # optional leading #         ( row | col | cell ) \s* =         ( $qc                                        # for row and col         | $qd , $qd (?: - $qs , $qs)?                # for cell (ranges)           (?: ; $qd , $qd (?: - $qs , $qs)? )*        # and cell (range) lists         ) \s* $}xi or croak ($self->SetDiag (2013))
                (pcl:pl-or (pcl:pl-&& (pl-defined $spec) (pcl:pl-=~ $spec (pl-regex "m{^ \\s*
        \\x23 ? \\s*                                # optional leading #
        ( row | col | cell ) \\s* =
        ( $qc                                        # for row and col
        | $qd , $qd (?: - $qs , $qs)?                # for cell (ranges)
          (?: ; $qd , $qd (?: - $qs , $qs)? )*        # and cell (range) lists
        ) \\s* $}xi"))) (pl-croak (pl-method-call $self 'SetDiag 2013)))
        
        ;; my ($type, $range) = (lc $1, $2)
                (pcl:pl-setf (vector $type $range) (vector (let ((*wantarray* t)) (pl-lc $1 $2))))
        
        ;; my @h = $self->column_names ()
                (pcl:pl-setf @h (let ((*wantarray* t)) (pl-method-call $self 'column_names)))
        
        ;; my @c (bare declaration)
        
        ;; if ($type eq "cell") {         my @spec;         my $min_row;         my $max_row = 0;         for (split m/\s*;\s*/ => $range) {             my ($tlr, $tlc, $brr, $brc) = (m{                     ^ \s* ([0-9]+     ) \s* , \s* ([0-9]+     ) \s*                 (?: - \s* ([0-9]+ | \*) \s* , \s* ([0-9]+ | \*) \s* )?                     $}x) or croak ($self->SetDiag (2013));             defined $brr or ($brr, $brc) = ($tlr, $tlc);             $tlr == 0 || $tlc == 0 ||                 ($brr ne "*" && ($brr == 0 || $brr < $tlr)) ||                 ($brc ne "*" && ($brc == 0 || $brc < $tlc))                     and croak ($self->SetDiag (2013));             $tlc--;             $brc-- unless $brc eq "*";             defined $min_row or $min_row = $tlr;             $tlr < $min_row and $min_row = $tlr;             $brr eq "*" || $brr > $max_row and                 $max_row = $brr;             push @spec, [ $tlr, $tlc, $brr, $brc ];             }         my $r = 0;         while (my $row = $self->getline ($io)) {             ++$r < $min_row and next;             my %row;             my $lc;             foreach my $s (@spec) {                 my ($tlr, $tlc, $brr, $brc) = @$s;                 $r <  $tlr || ($brr ne "*" && $r > $brr) and next;                 !defined $lc || $tlc < $lc and $lc = $tlc;                 my $rr = $brc eq "*" ? $#$row : $brc;                 $row{$_} = $row->[$_] for $tlc .. $rr;                 }             push @c, [ @row{sort { $a <=> $b } keys %row } ];             if (@h) {                 my %h; @h{@h} = @{$c[-1]};                 $c[-1] = \%h;                 }             $max_row ne "*" && $r == $max_row and last;             }         return \@c;         }
        ;; if ($type eq "cell")
        (pl-if         (pcl:pl-str-eq $type "cell")
          (progn
            ;; my @spec (bare declaration)
            
            ;; my $min_row (bare declaration)
            
            ;; my $max_row = 0
                        (pcl:pl-setf $max_row 0)
            
            ;; for (split m/\s*;\s*/ => $range) {             my ($tlr, $tlc, $brr, $brc) = (m{                     ^ \s* ([0-9]+     ) \s* , \s* ([0-9]+     ) \s*                 (?: - \s* ([0-9]+ | \*) \s* , \s* ([0-9]+ | \*) \s* )?                     $}x) or croak ($self->SetDiag (2013));             defined $brr or ($brr, $brc) = ($tlr, $tlc);             $tlr == 0 || $tlc == 0 ||                 ($brr ne "*" && ($brr == 0 || $brr < $tlr)) ||                 ($brc ne "*" && ($brc == 0 || $brc < $tlc))                     and croak ($self->SetDiag (2013));             $tlc--;             $brc-- unless $brc eq "*";             defined $min_row or $min_row = $tlr;             $tlr < $min_row and $min_row = $tlr;             $brr eq "*" || $brr > $max_row and                 $max_row = $brr;             push @spec, [ $tlr, $tlc, $brr, $brc ];             }
            (pl-foreach ($_             (pl-split (pl-regex "m/\\s*;\\s*/") $range))
              ;; my ($tlr, $tlc, $brr, $brc) = (m{                     ^ \s* ([0-9]+     ) \s* , \s* ([0-9]+     ) \s*                 (?: - \s* ([0-9]+ | \*) \s* , \s* ([0-9]+ | \*) \s* )?                     $}x) or croak ($self->SetDiag (2013))
                            (pcl:pl-or (pcl:pl-setf (vector $tlr $tlc $brr $brc) (vector (pcl:pl-=~ $_ (pl-regex "m{
                    ^ \\s* ([0-9]+     ) \\s* , \\s* ([0-9]+     ) \\s*
                (?: - \\s* ([0-9]+ | \\*) \\s* , \\s* ([0-9]+ | \\*) \\s* )?
                    $}x")))) (pl-croak (pl-method-call $self 'SetDiag 2013)))
              
              ;; defined $brr or ($brr, $brc) = ($tlr, $tlc)
                            (pcl:pl-or (pl-defined $brr) (pcl:pl-setf (vector $brr $brc) (vector $tlr $tlc)))
              
              ;; $tlr == 0 || $tlc == 0 ||                 ($brr ne "*" && ($brr == 0 || $brr < $tlr)) ||                 ($brc ne "*" && ($brc == 0 || $brc < $tlc))                     and croak ($self->SetDiag (2013))
                            (pcl:pl-and (pcl:pl-|| (pcl:pl-|| (pcl:pl-|| (pcl:pl-== $tlr 0) (pcl:pl-== $tlc 0)) (pcl:pl-&& (pcl:pl-str-ne $brr "*") (pcl:pl-|| (pcl:pl-== $brr 0) (pcl:pl-< $brr $tlr)))) (pcl:pl-&& (pcl:pl-str-ne $brc "*") (pcl:pl-|| (pcl:pl-== $brc 0) (pcl:pl-< $brc $tlc)))) (pl-croak (pl-method-call $self 'SetDiag 2013)))
              
              ;; $tlc--
                            (pl-post-- $tlc)
              
              ;; $brc-- unless $brc eq "*"
              (pl-unless               (pcl:pl-str-eq $brc "*")               (pl-post-- $brc))
              
              ;; defined $min_row or $min_row = $tlr
                            (pcl:pl-or (pl-defined $min_row) (pcl:pl-setf $min_row $tlr))
              
              ;; $tlr < $min_row and $min_row = $tlr
                            (pcl:pl-and (pcl:pl-< $tlr $min_row) (pcl:pl-setf $min_row $tlr))
              
              ;; $brr eq "*" || $brr > $max_row and                 $max_row = $brr
                            (pcl:pl-and (pcl:pl-|| (pcl:pl-str-eq $brr "*") (pcl:pl-> $brr $max_row)) (pcl:pl-setf $max_row $brr))
              
              ;; push @spec, [ $tlr, $tlc, $brr, $brc ]
                            (pl-push @spec (make-array 4 :adjustable t :fill-pointer t :initial-contents (list $tlr $tlc $brr $brc)))
              
            )
            
            ;; my $r = 0
                        (pcl:pl-setf $r 0)
            
            ;; while (my $row = $self->getline ($io)) {             ++$r < $min_row and next;             my %row;             my $lc;             foreach my $s (@spec) {                 my ($tlr, $tlc, $brr, $brc) = @$s;                 $r <  $tlr || ($brr ne "*" && $r > $brr) and next;                 !defined $lc || $tlc < $lc and $lc = $tlc;                 my $rr = $brc eq "*" ? $#$row : $brc;                 $row{$_} = $row->[$_] for $tlc .. $rr;                 }             push @c, [ @row{sort { $a <=> $b } keys %row } ];             if (@h) {                 my %h; @h{@h} = @{$c[-1]};                 $c[-1] = \%h;                 }             $max_row ne "*" && $r == $max_row and last;             }
            (let (($row (make-pl-box nil)))
              (pl-while             (pcl:pl-setf $row (pl-method-call $self 'getline $io))
                ;; ++$r < $min_row and next
                                (pcl:pl-and (pcl:pl-< (pl-pre++ $r) $min_row) (pl-next))
                
                ;; my %row (bare declaration)
                
                ;; my $lc (bare declaration)
                
                ;; foreach my $s (@spec) {                 my ($tlr, $tlc, $brr, $brc) = @$s;                 $r <  $tlr || ($brr ne "*" && $r > $brr) and next;                 !defined $lc || $tlc < $lc and $lc = $tlc;                 my $rr = $brc eq "*" ? $#$row : $brc;                 $row{$_} = $row->[$_] for $tlc .. $rr;                 }
                (pl-foreach ($s (vector                 @spec))
                  ;; my ($tlr, $tlc, $brr, $brc) = @$s
                                    (pcl:pl-setf (vector $tlr $tlc $brr $brc) (pl-cast-@ $s))
                  
                  ;; $r <  $tlr || ($brr ne "*" && $r > $brr) and next
                                    (pcl:pl-and (pcl:pl-|| (pcl:pl-< $r $tlr) (pcl:pl-&& (pcl:pl-str-ne $brr "*") (pcl:pl-> $r $brr))) (pl-next))
                  
                  ;; !defined $lc || $tlc < $lc and $lc = $tlc
                                    (pcl:pl-and (pcl:pl-|| (pl-! (pl-defined $lc)) (pcl:pl-< $tlc $lc)) (pcl:pl-setf $lc $tlc))
                  
                  ;; my $rr = $brc eq "*" ? $#$row : $brc
                                    (pcl:pl-setf $rr (pl-if (pcl:pl-str-eq $brc "*") (pl-$# $row) $brc))
                  
                  ;; $row{$_} = $row->[$_] for $tlc .. $rr
                  (pl-for                   (pcl:pl-.. $tlc $rr)                   (pcl:pl-setf (pl-gethash %row $_) (pl-aref-deref $row $_)))
                  
                )
                
                (defun --anon-block-82-- ($a $b)
                  ;; $a <=> $b
                                    (pcl:pl-<=> $a $b)
                  
                )
                
                ;; push @c, [ @row{sort { $a <=> $b } keys %row } ]
                                (pl-push @c (make-array 1 :adjustable t :fill-pointer t :initial-contents (list (pl-hslice @row (let ((*wantarray* t)) (pl-sort #'--anon-block-82-- (let ((*wantarray* t)) (pl-keys %row))))))))
                
                ;; if (@h) {                 my %h; @h{@h} = @{$c[-1]};                 $c[-1] = \%h;                 }
                ;; if (@h)
                (pl-if                 @h
                  (progn
                    ;; my %h (bare declaration)
                    
                    ;; @h{@h} = @{$c[-1]}
                                        (pcl:pl-setf (pl-hslice @h @h) (pl-cast-@ (pl-aref @c -1)))
                    
                    ;; $c[-1] = \%h
                                        (pcl:pl-setf (pl-aref @c -1) (pl-backslash %h))
                    
                  )
                  nil
                )
                
                ;; $max_row ne "*" && $r == $max_row and last
                                (pcl:pl-and (pcl:pl-&& (pcl:pl-str-ne $max_row "*") (pcl:pl-== $r $max_row)) (pl-last))
                
              )
            )
            
            ;; return \@c
                        (pl-return (pl-backslash @c))
            
          )
          nil
        )
        
        ;; my @r (bare declaration)
        
        ;; my $eod = 0
                (pcl:pl-setf $eod 0)
        
        ;; for (split m/\s*;\s*/ => $range) {         my ($from, $to) = m/^\s* ([0-9]+) (?: \s* - \s* ([0-9]+ | \* ))? \s* $/x             or croak ($self->SetDiag (2013));         $to ||= $from;         $to eq "*" and ($to, $eod) = ($from, 1);         # $to cannot be <= 0 due to regex and ||=         $from <= 0 || $to < $from and croak ($self->SetDiag (2013));         $r[$_] = 1 for $from .. $to;         }
        (pl-foreach ($_         (pl-split (pl-regex "m/\\s*;\\s*/") $range))
          ;; my ($from, $to) = m/^\s* ([0-9]+) (?: \s* - \s* ([0-9]+ | \* ))? \s* $/x             or croak ($self->SetDiag (2013))
                    (pcl:pl-or (pcl:pl-setf (vector $from $to) (pcl:pl-=~ $_ (pl-regex "m/^\\s* ([0-9]+) (?: \\s* - \\s* ([0-9]+ | \\* ))? \\s* $/x"))) (pl-croak (pl-method-call $self 'SetDiag 2013)))
          
          ;; $to ||= $from
                    (pcl:pl-or-assign $to $from)
          
          ;; $to eq "*" and ($to, $eod) = ($from, 1)
                    (pcl:pl-and (pcl:pl-str-eq $to "*") (pcl:pl-setf (vector $to $eod) (vector $from 1)))
          
          ;; $from <= 0 || $to < $from and croak ($self->SetDiag (2013))
                    (pcl:pl-and (pcl:pl-|| (pcl:pl-<= $from 0) (pcl:pl-< $to $from)) (pl-croak (pl-method-call $self 'SetDiag 2013)))
          
          ;; $r[$_] = 1 for $from .. $to
          (pl-for           (pcl:pl-.. $from $to)           (pcl:pl-setf (pl-aref @r $_) 1))
          
        )
        
        ;; my $r = 0
                (pcl:pl-setf $r 0)
        
        ;; $type eq "col" and shift @r
                (pcl:pl-and (pcl:pl-str-eq $type "col") (pl-shift @r))
        
        ;; $_ ||= 0 for @r
        (pl-for         @r         (pcl:pl-or-assign $_ 0))
        
        ;; while (my $row = $self->getline ($io)) {         $r++;         if ($type eq "row") {             if (($r > $#r && $eod) || $r[$r]) {                 push @c, $row;                 if (@h) {                     my %h; @h{@h} = @{$c[-1]};                     $c[-1] = \%h;                     }                 }             next;             }         push @c, [ map { ($_ > $#r && $eod) || $r[$_] ? $row->[$_] : () } 0..$#$row ];         if (@h) {             my %h; @h{@h} = @{$c[-1]};             $c[-1] = \%h;             }         }
        (let (($row (make-pl-box nil)))
          (pl-while         (pcl:pl-setf $row (pl-method-call $self 'getline $io))
            ;; $r++
                        (pl-post++ $r)
            
            ;; if ($type eq "row") {             if (($r > $#r && $eod) || $r[$r]) {                 push @c, $row;                 if (@h) {                     my %h; @h{@h} = @{$c[-1]};                     $c[-1] = \%h;                     }                 }             next;             }
            ;; if ($type eq "row")
            (pl-if             (pcl:pl-str-eq $type "row")
              (progn
                ;; if (($r > $#r && $eod) || $r[$r]) {                 push @c, $row;                 if (@h) {                     my %h; @h{@h} = @{$c[-1]};                     $c[-1] = \%h;                     }                 }
                ;; if (($r > $#r && $eod) || $r[$r])
                (pl-if                 (pcl:pl-|| (pcl:pl-&& (pcl:pl-> $r (pl-array-last-index @r)) $eod) (pl-aref @r $r))
                  (progn
                    ;; push @c, $row
                                        (pl-push @c $row)
                    
                    ;; if (@h) {                     my %h; @h{@h} = @{$c[-1]};                     $c[-1] = \%h;                     }
                    ;; if (@h)
                    (pl-if                     @h
                      (progn
                        ;; my %h (bare declaration)
                        
                        ;; @h{@h} = @{$c[-1]}
                                                (pcl:pl-setf (pl-hslice @h @h) (pl-cast-@ (pl-aref @c -1)))
                        
                        ;; $c[-1] = \%h
                                                (pcl:pl-setf (pl-aref @c -1) (pl-backslash %h))
                        
                      )
                      nil
                    )
                    
                  )
                  nil
                )
                
                ;; next
                                (pl-next)
                
              )
              nil
            )
            
            (defun --anon-block-83-- ($_)
              ;;  ($_ > $#r && $eod) || $r[$_] ? $row->[$_] : ()
                            (pl-if (pcl:pl-|| (pcl:pl-&& (pcl:pl-> $_ (pl-array-last-index @r)) $eod) (pl-aref @r $_)) (pl-aref-deref $row $_) (progn ))
              
            )
            
            ;; push @c, [ map { ($_ > $#r && $eod) || $r[$_] ? $row->[$_] : () } 0..$#$row ]
                        (pl-push @c (make-array 1 :adjustable t :fill-pointer t :initial-contents (list (let ((*wantarray* t)) (pl-map #'--anon-block-83-- (pcl:pl-.. 0 (pl-$# $row)))))))
            
            ;; if (@h) {             my %h; @h{@h} = @{$c[-1]};             $c[-1] = \%h;             }
            ;; if (@h)
            (pl-if             @h
              (progn
                ;; my %h (bare declaration)
                
                ;; @h{@h} = @{$c[-1]}
                                (pcl:pl-setf (pl-hslice @h @h) (pl-cast-@ (pl-aref @c -1)))
                
                ;; $c[-1] = \%h
                                (pcl:pl-setf (pl-aref @c -1) (pl-backslash %h))
                
              )
              nil
            )
            
          )
        )
        
        ;; return \@c
                (pl-return (pl-backslash @c))
        
      )
    )
  )
)

;; my $csv_usage = q{usage: my $aoa = csv (in => $file);}
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar $csv_usage (make-pl-box nil)))
(box-set $csv_usage ;; PARSE ERROR: Handle single node of unknown type. Dump:)

;; sub _csv_attr { ... }
(pl-sub pl-_csv_attr (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let ((%attr (make-pl-box nil)) ($enc (make-pl-box nil)) ($stack (make-pl-box nil)) ($fh (make-pl-box nil)) ($sink (make-pl-box nil)) ($cls (make-pl-box nil)) ($in (make-pl-box nil)) ($out (make-pl-box nil)) ($fn (make-pl-box nil)) (@layers (make-pl-box nil)) ($hdrs (make-pl-box nil)) ($frag (make-pl-box nil)) ($key (make-pl-box nil)) ($val (make-pl-box nil)) ($kh (make-pl-box nil)) ($cbai (make-pl-box nil)) ($cbbo (make-pl-box nil)) ($cboi (make-pl-box nil)) ($hd_s (make-pl-box nil)) ($hd_b (make-pl-box nil)) ($hd_m (make-pl-box nil)) ($hd_c (make-pl-box nil)) ($f (make-pl-box nil)) ($t (make-pl-box nil)) ($fltr (make-pl-box nil)) (%fltr (make-pl-box nil)) ($form (make-pl-box nil)) ($csv (make-pl-box nil)))
        ;; my %attr = (@_ == 1 && ref $_[0] eq "HASH" ? %{$_[0]} : @_) or croak
                (pcl:pl-or (pcl:pl-setf %attr (pl-hash (pl-if (pcl:pl-&& (pcl:pl-== @_ 1) (pcl:pl-str-eq (pl-aref (pl-ref $_) 0) "HASH")) (pl-cast-% (pl-aref @_ 0)) @_))) (pl-croak))
        
        ;; $attr{binary} = 1
                (pcl:pl-setf (pl-gethash %attr "binary") 1)
        
        ;; my $enc = delete $attr{enc} || delete $attr{encoding} || ""
                (pcl:pl-setf $enc (pcl:pl-|| (pcl:pl-|| (pl-delete (pl-gethash %attr "enc")) (pl-delete (pl-gethash %attr "encoding"))) ""))
        
        ;; $enc eq "auto" and ($attr{detect_bom}, $enc) = (1, "")
                (pcl:pl-and (pcl:pl-str-eq $enc "auto") (pcl:pl-setf (vector (pl-gethash %attr "detect_bom") $enc) (vector 1 "")))
        
        ;; my $stack = $enc =~ s/(:\w.*)// ? $1 : ""
                (pcl:pl-setf $stack (pl-if (pcl:pl-=~ $enc (pl-subst "(:\\w.*)" "")) $1 ""))
        
        ;; $enc =~ m/^[-\w.]+$/ and $enc = ":encoding($enc)"
                (pcl:pl-and (pcl:pl-=~ $enc (pl-regex "m/^[-\\w.]+$/")) (pcl:pl-setf $enc (pl-string-concat ":encoding(" $enc ")")))
        
        ;; $enc .= $stack
                (pcl:pl-.= $enc $stack)
        
        ;; my $fh (bare declaration)
        
        ;; my $sink = 0
                (pcl:pl-setf $sink 0)
        
        ;; my $cls  = 0
                (pcl:pl-setf $cls 0)
        
        ;; my $in   = delete $attr{in}  || delete $attr{file} or croak $csv_usage
                (pcl:pl-or (pcl:pl-setf $in (pcl:pl-|| (pl-delete (pl-gethash %attr "in")) (pl-delete (pl-gethash %attr "file")))) (pl-croak $csv_usage))
        
        ;; my $out  = exists $attr{out} && !$attr{out} ? \"skip"         : delete $attr{out} || delete $attr{file}
                (pcl:pl-setf $out (pl-if (pcl:pl-&& (pl-exists (pl-gethash %attr "out")) (pl-! (pl-gethash %attr "out"))) (pl-backslash "skip") (pcl:pl-|| (pl-delete (pl-gethash %attr "out")) (pl-delete (pl-gethash %attr "file")))))
        
        ;; ref $in eq "CODE" || ref $in eq "ARRAY" and $out ||= \*STDOUT
                (pcl:pl-and (pcl:pl-|| (pcl:pl-str-eq (pl-ref $in) "CODE") (pcl:pl-str-eq (pl-ref $in) "ARRAY")) (pcl:pl-or-assign $out (pl-backslash *STDOUT)))
        
        ;; $in && $out && !ref $in && !ref $out and croak join "\n" =>        qq{Cannot use a string for both in and out. Instead use:},        qq{ csv (in => csv (in => "$in"), out => "$out");\n}
                (pcl:pl-and (pcl:pl-&& (pcl:pl-&& (pcl:pl-&& $in $out) (pl-! (pl-ref $in))) (pl-! (pl-ref $out))) (pl-croak (pl-join "
" "Cannot use a string for both in and out. Instead use:" (pl-string-concat " csv (in => csv (in => \"" $in "\"), out => \"" $out "\");
"))))
        
        ;; if ($out) {         if (ref $out and ("ARRAY" eq ref $out or "HASH" eq ref $out)) {             delete $attr{out};             $sink = 1;             }         elsif ((ref $out and "SCALAR" ne ref $out) or "GLOB" eq ref \$out) {             $fh = $out;             }         elsif (ref $out and "SCALAR" eq ref $out and defined $$out and $$out eq "skip") {             delete $attr{out};             $sink = 1;             }         else {             open $fh, ">", $out or croak "$out: $!";             $cls = 1;             }         if ($fh) {             if ($enc) {                 binmode $fh, $enc;                 my $fn = fileno $fh; # This is a workaround for a bug in PerlIO::via::gzip             }             unless (defined $attr{eol}) {                 my @layers = eval { PerlIO::get_layers ($fh) };                 $attr{eol} = (grep m/crlf/ => @layers) ? "\n" : "\r\n";                 }             }         }
        ;; if ($out)
        (pl-if         $out
          (progn
            ;; if (ref $out and ("ARRAY" eq ref $out or "HASH" eq ref $out)) {             delete $attr{out};             $sink = 1;             }         elsif ((ref $out and "SCALAR" ne ref $out) or "GLOB" eq ref \$out) {             $fh = $out;             }         elsif (ref $out and "SCALAR" eq ref $out and defined $$out and $$out eq "skip") {             delete $attr{out};             $sink = 1;             }         else {             open $fh, ">", $out or croak "$out: $!";             $cls = 1;             }
            ;; if (ref $out and ("ARRAY" eq ref $out or "HASH" eq ref $out))
            (pl-if             (pcl:pl-and (pl-ref $out) (pcl:pl-or (pcl:pl-str-eq "ARRAY" (pl-ref $out)) (pcl:pl-str-eq "HASH" (pl-ref $out))))
              (progn
                ;; delete $attr{out}
                                (pl-delete (pl-gethash %attr "out"))
                
                ;; $sink = 1
                                (pcl:pl-setf $sink 1)
                
              )
              ;; elsif ((ref $out and "SCALAR" ne ref $out) or "GLOB" eq ref \$out)
              (pl-if ;; PARSE ERROR: Fell through. Missing case: [
                (progn
                  ;; $fh = $out
                                    (pcl:pl-setf $fh $out)
                  
                )
                ;; elsif (ref $out and "SCALAR" eq ref $out and defined $$out and $$out eq "skip")
                (pl-if ;; PARSE ERROR: Fell through. Missing case: [
                  (progn
                    ;; delete $attr{out}
                                        (pl-delete (pl-gethash %attr "out"))
                    
                    ;; $sink = 1
                                        (pcl:pl-setf $sink 1)
                    
                  )
                  ;; else
                  (progn
                    ;; open $fh, ">", $out or croak "$out: $!"
                                        (pcl:pl-or (pl-open $fh ">" $out) (pl-croak (pl-string-concat $out ": " (pl-errno-string))))
                    
                    ;; $cls = 1
                                        (pcl:pl-setf $cls 1)
                    
                  )
                )
              )
            )
            
            ;; if ($fh) {             if ($enc) {                 binmode $fh, $enc;                 my $fn = fileno $fh; # This is a workaround for a bug in PerlIO::via::gzip             }             unless (defined $attr{eol}) {                 my @layers = eval { PerlIO::get_layers ($fh) };                 $attr{eol} = (grep m/crlf/ => @layers) ? "\n" : "\r\n";                 }             }
            ;; if ($fh)
            (pl-if             $fh
              (progn
                ;; if ($enc) {                 binmode $fh, $enc;                 my $fn = fileno $fh; # This is a workaround for a bug in PerlIO::via::gzip             }
                ;; if ($enc)
                (pl-if                 $enc
                  (progn
                    ;; binmode $fh, $enc
                                        (pl-binmode $fh $enc)
                    
                    ;; my $fn = fileno $fh
                                        (pcl:pl-setf $fn (pl-fileno $fh))
                    
                  )
                  nil
                )
                
                ;; unless (defined $attr{eol}) {                 my @layers = eval { PerlIO::get_layers ($fh) };                 $attr{eol} = (grep m/crlf/ => @layers) ? "\n" : "\r\n";                 }
                ;; unless (defined $attr{"eol"})
                (pl-if (pl-not                 (pl-defined (pl-gethash %attr "eol")))
                  (progn
                    (defun --anon-block-84-- ()
                      ;; PerlIO::get_layers ($fh)
                                            (PerlIO::pl-get_layers $fh)
                      
                    )
                    
                    ;; my @layers = eval { PerlIO::get_layers ($fh) }
                                        (pcl:pl-setf @layers (pl-eval-block (funcall #'--anon-block-84--)))
                    
                    ;; $attr{eol} = (grep m/crlf/ => @layers) ? "\n" : "\r\n"
                                        (pcl:pl-setf (pl-gethash %attr "eol") (pl-if (pl-grep (pcl:pl-=~ $_ (pl-regex "m/crlf/")) @layers) "
" "
"))
                    
                  )
                  nil
                )
                
              )
              nil
            )
            
          )
          nil
        )
        
        ;; if (   ref $in eq "CODE" or ref $in eq "ARRAY") {         # All done         }     elsif (ref $in eq "SCALAR") {         # Strings with code points over 0xFF may not be mapped into in-memory file handles         # "<$enc" does not change that :(         open $fh, "<", $in or croak "Cannot open from SCALAR using PerlIO";         $cls = 1;         }     elsif (ref $in or "GLOB" eq ref \$in) {         if (!ref $in && $] < 5.008005) {             $fh = \*$in; # uncoverable statement ancient perl version required             }         else {             $fh = $in;             }         }     else {         open $fh, "<$enc", $in or croak "$in: $!";         $cls = 1;         }
        ;; if (ref $in eq "CODE" or ref $in eq "ARRAY")
        (pl-if         (pcl:pl-or (pcl:pl-str-eq (pl-ref $in) "CODE") (pcl:pl-str-eq (pl-ref $in) "ARRAY"))
          (progn
          )
          ;; elsif (ref $in eq "SCALAR")
          (pl-if           (pcl:pl-str-eq (pl-ref $in) "SCALAR")
            (progn
              ;; open $fh, "<", $in or croak "Cannot open from SCALAR using PerlIO"
                            (pcl:pl-or (pl-open $fh "<" $in) (pl-croak "Cannot open from SCALAR using PerlIO"))
              
              ;; $cls = 1
                            (pcl:pl-setf $cls 1)
              
            )
            ;; elsif (ref $in or "GLOB" eq ref \$in)
            (pl-if ;; PARSE ERROR: Fell through. Missing case: [
              (progn
                ;; if (!ref $in && $] < 5.008005) {             $fh = \*$in; # uncoverable statement ancient perl version required             }         else {             $fh = $in;             }
                ;; if (!ref $in && $] < 5.008005)
                (pl-if                 (pcl:pl-&& (pl-! (pl-ref $in)) (pcl:pl-< $] 5.008005))
                  (progn
                    ;; $fh = \*$in
                                        (pcl:pl-setf $fh (pl-backslash (pl-* $in)))
                    
                  )
                  ;; else
                  (progn
                    ;; $fh = $in
                                        (pcl:pl-setf $fh $in)
                    
                  )
                )
                
              )
              ;; else
              (progn
                ;; open $fh, "<$enc", $in or croak "$in: $!"
                                (pcl:pl-or (pl-open $fh (pl-string-concat "<" $enc) $in) (pl-croak (pl-string-concat $in ": " (pl-errno-string))))
                
                ;; $cls = 1
                                (pcl:pl-setf $cls 1)
                
              )
            )
          )
        )
        
        ;; $fh || $sink or croak qq{No valid source passed. "in" is required}
                (pcl:pl-or (pcl:pl-|| $fh $sink) (pl-croak "No valid source passed. \"in\" is required"))
        
        ;; my $hdrs = delete $attr{headers}
                (pcl:pl-setf $hdrs (pl-delete (pl-gethash %attr "headers")))
        
        ;; my $frag = delete $attr{fragment}
                (pcl:pl-setf $frag (pl-delete (pl-gethash %attr "fragment")))
        
        ;; my $key  = delete $attr{key}
                (pcl:pl-setf $key (pl-delete (pl-gethash %attr "key")))
        
        ;; my $val  = delete $attr{value}
                (pcl:pl-setf $val (pl-delete (pl-gethash %attr "value")))
        
        ;; my $kh   = delete $attr{keep_headers}      ||           delete $attr{keep_column_names}      ||           delete $attr{kh}
                (pcl:pl-setf $kh (pcl:pl-|| (pcl:pl-|| (pl-delete (pl-gethash %attr "keep_headers")) (pl-delete (pl-gethash %attr "keep_column_names"))) (pl-delete (pl-gethash %attr "kh"))))
        
        ;; my $cbai = delete $attr{callbacks}{after_in}    ||                delete $attr{after_in}               ||                delete $attr{callbacks}{after_parse} ||                delete $attr{after_parse}
                (pcl:pl-setf $cbai (pcl:pl-|| (pcl:pl-|| (pcl:pl-|| (pl-gethash (pl-delete (pl-gethash %attr "callbacks")) "after_in") (pl-delete (pl-gethash %attr "after_in"))) (pl-gethash (pl-delete (pl-gethash %attr "callbacks")) "after_parse")) (pl-delete (pl-gethash %attr "after_parse"))))
        
        ;; my $cbbo = delete $attr{callbacks}{before_out}  ||                delete $attr{before_out}
                (pcl:pl-setf $cbbo (pcl:pl-|| (pl-gethash (pl-delete (pl-gethash %attr "callbacks")) "before_out") (pl-delete (pl-gethash %attr "before_out"))))
        
        ;; my $cboi = delete $attr{callbacks}{on_in}       ||                delete $attr{on_in}
                (pcl:pl-setf $cboi (pcl:pl-|| (pl-gethash (pl-delete (pl-gethash %attr "callbacks")) "on_in") (pl-delete (pl-gethash %attr "on_in"))))
        
        ;; my $hd_s = delete $attr{sep_set}                ||                delete $attr{seps}
                (pcl:pl-setf $hd_s (pcl:pl-|| (pl-delete (pl-gethash %attr "sep_set")) (pl-delete (pl-gethash %attr "seps"))))
        
        ;; my $hd_b = delete $attr{detect_bom}             ||                delete $attr{bom}
                (pcl:pl-setf $hd_b (pcl:pl-|| (pl-delete (pl-gethash %attr "detect_bom")) (pl-delete (pl-gethash %attr "bom"))))
        
        ;; my $hd_m = delete $attr{munge}                  ||                delete $attr{munge_column_names}
                (pcl:pl-setf $hd_m (pcl:pl-|| (pl-delete (pl-gethash %attr "munge")) (pl-delete (pl-gethash %attr "munge_column_names"))))
        
        ;; my $hd_c = delete $attr{set_column_names}
                (pcl:pl-setf $hd_c (pl-delete (pl-gethash %attr "set_column_names")))
        
        ;; for ([ quo    => "quote"                ],          [ esc    => "escape"                ],          [ escape => "escape_char"        ],          ) {         my ($f, $t) = @$_;         exists $attr{$f} and !exists $attr{$t} and $attr{$t} = delete $attr{$f};         }
        (pl-foreach ($_ (vector (make-array 2 :adjustable t :fill-pointer t :initial-contents (list "quo" "quote")) (make-array 2 :adjustable t :fill-pointer t :initial-contents (list "esc" "escape")) (make-array 2 :adjustable t :fill-pointer t :initial-contents (list "escape" "escape_char"))))
          ;; my ($f, $t) = @$_
                    (pcl:pl-setf (vector $f $t) (pl-cast-@ $_))
          
          ;; exists $attr{$f} and !exists $attr{$t} and $attr{$t} = delete $attr{$f}
                    (pcl:pl-and (pcl:pl-and (pl-exists (pl-gethash %attr $f)) (pl-! (pl-exists (pl-gethash %attr $t)))) (pcl:pl-setf (pl-gethash %attr $t) (pl-delete (pl-gethash %attr $f))))
          
        )
        
        ;; my $fltr = delete $attr{filter}
                (pcl:pl-setf $fltr (pl-delete (pl-gethash %attr "filter")))
        
        (defun --anon-block-85-- ()
          ;; @{$_[1]} > 1 or defined $_[1][0] && $_[1][0] ne ""
                    (pcl:pl-or (pcl:pl-> (pl-cast-@ (pl-aref @_ 1)) 1) (pcl:pl-&& (pl-aref (pl-aref (pl-defined $_) 1) 0) (pcl:pl-str-ne (pl-aref (pl-aref @_ 1) 0) "")))
          
        )
        
        (defun --anon-block-86-- ()
          (defun --anon-block-87-- ($_)
            ;; defined && $_ ne ""
                        (pcl:pl-&& (pl-defined $_) (pcl:pl-str-ne $_ ""))
            
          )
          
          ;; grep { defined && $_ ne "" } @{$_[1]}
                    (pl-grep #'--anon-block-87-- (pl-cast-@ (pl-aref @_ 1)))
          
        )
        
        (defun --anon-block-88-- ()
          (defun --anon-block-89-- ($_)
            ;; defined && m/\S/
                        (pcl:pl-&& (pl-defined $_) (pcl:pl-=~ $_ (pl-regex "m/\\S/")))
            
          )
          
          ;; grep { defined && m/\S/    } @{$_[1]}
                    (pl-grep #'--anon-block-89-- (pl-cast-@ (pl-aref @_ 1)))
          
        )
        
        ;; my %fltr = (         not_blank => sub { @{$_[1]} > 1 or defined $_[1][0] && $_[1][0] ne "" },         not_empty => sub { grep { defined && $_ ne "" } @{$_[1]} },         filled    => sub { grep { defined && m/\S/    } @{$_[1]} },         )
        ;; PARSE ERROR: Fell through. Missing case: [
        
        ;; defined $fltr && !ref $fltr && exists $fltr{$fltr} and         $fltr = { 0 => $fltr{$fltr} }
                (pcl:pl-and (pcl:pl-&& (pcl:pl-&& (pl-defined $fltr) (pl-! (pl-ref $fltr))) (pl-exists (pl-gethash %fltr $fltr))) (pcl:pl-setf $fltr (pl-hash 0 (pl-gethash %fltr $fltr))))
        
        ;; ref $fltr eq "CODE" and $fltr = { 0 => $fltr }
                (pcl:pl-and (pcl:pl-str-eq (pl-ref $fltr) "CODE") (pcl:pl-setf $fltr (pl-hash 0 $fltr)))
        
        ;; ref $fltr eq "HASH" or $fltr = undef
                (pcl:pl-or (pcl:pl-str-eq (pl-ref $fltr) "HASH") (pcl:pl-setf $fltr (pl-undef)))
        
        ;; my $form = delete $attr{formula}
                (pcl:pl-setf $form (pl-delete (pl-gethash %attr "formula")))
        
        ;; defined $attr{auto_diag}   or $attr{auto_diag}   = 1
                (pcl:pl-or (pl-defined (pl-gethash %attr "auto_diag")) (pcl:pl-setf (pl-gethash %attr "auto_diag") 1))
        
        ;; defined $attr{escape_null} or $attr{escape_null} = 0
                (pcl:pl-or (pl-defined (pl-gethash %attr "escape_null")) (pcl:pl-setf (pl-gethash %attr "escape_null") 0))
        
        ;; my $csv = delete $attr{csv} || Text::CSV_PP->new (\%attr)         or croak $last_new_error
                (pcl:pl-or (pcl:pl-setf $csv (pcl:pl-|| (pl-delete (pl-gethash %attr "csv")) (pl-method-call "Text::CSV_PP" 'new (pl-backslash %attr)))) (pl-croak $last_new_error))
        
        ;; defined $form and $csv->formula ($form)
                (pcl:pl-and (pl-defined $form) (pl-method-call $csv 'formula $form))
        
        ;; $kh && !ref $kh && $kh =~ m/^(?:1|yes|true|internal|auto)$/i and         $kh = \@internal_kh
                (pcl:pl-and (pcl:pl-&& (pcl:pl-&& $kh (pl-! (pl-ref $kh))) (pcl:pl-=~ $kh (pl-regex "m/^(?:1|yes|true|internal|auto)$/i"))) (pcl:pl-setf $kh (pl-backslash @internal_kh)))
        
        ;; return {         csv  => $csv,         attr => { %attr },         fh   => $fh,         cls  => $cls,         in   => $in,         sink => $sink,         out  => $out,         enc  => $enc,         hdrs => $hdrs,         key  => $key,         val  => $val,         kh   => $kh,         frag => $frag,         fltr => $fltr,         cbai => $cbai,         cbbo => $cbbo,         cboi => $cboi,         hd_s => $hd_s,         hd_b => $hd_b,         hd_m => $hd_m,         hd_c => $hd_c,         }
                (pl-return (pl-hash "csv" $csv "attr" (pl-hash %attr) "fh" $fh "cls" $cls "in" $in "sink" $sink "out" $out "enc" $enc "hdrs" $hdrs "key" $key "val" $val "kh" $kh "frag" $frag "fltr" $fltr "cbai" $cbai "cbbo" $cbbo "cboi" $cboi "hd_s" $hd_s "hd_b" $hd_b "hd_m" $hd_m "hd_c" $hd_c))
        
      )
    )
  )
)

;; sub csv { ... }
(pl-sub pl-csv (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($c (make-pl-box nil)) ($csv (make-pl-box nil)) ($in (make-pl-box nil)) ($fh (make-pl-box nil)) ($hdrs (make-pl-box nil)) (%hdr (make-pl-box nil)) ($hdr (make-pl-box nil)) ($row (make-pl-box nil)) (@hdrs (make-pl-box nil)) (@row1 (make-pl-box nil)) (%harg (make-pl-box nil)) (@hdr (make-pl-box nil)) ($key (make-pl-box nil)) ($val (make-pl-box nil)) ($h (make-pl-box nil)) ($has_h (make-pl-box nil)) ($cr (make-pl-box nil)) (%f (make-pl-box nil)) ($CSV (make-pl-box nil)) ($ROW (make-pl-box nil)) ($FLD (make-pl-box nil)) ($frag (make-pl-box nil)) ($ref (make-pl-box nil)) (@h (make-pl-box nil)) (%h (make-pl-box nil)) ($k (make-pl-box nil)) ($j (make-pl-box nil)) (@f (make-pl-box nil)) (@mk (make-pl-box nil)) ($r (make-pl-box nil)) ($K (make-pl-box nil)) ($ro (make-pl-box nil)))
        ;; @_ && (ref $_[0] eq __PACKAGE__ or ref $_[0] eq 'Text::CSV') and splice @_, 0, 0, "csv"
                (pcl:pl-and (pcl:pl-&& @_ (pcl:pl-or (pcl:pl-str-eq (pl-aref (pl-ref $_) 0) "Text::CSV_PP") (pcl:pl-str-eq (pl-aref (pl-ref $_) 0) "Text::CSV"))) (pl-splice @_ 0 0 "csv"))
        
        ;; @_ or croak $csv_usage
                (pcl:pl-or @_ (pl-croak $csv_usage))
        
        ;; my $c = _csv_attr (@_)
                (pcl:pl-setf $c (pl-_csv_attr @_))
        
        ;; my ($csv, $in, $fh, $hdrs) = @{$c}{qw( csv in fh hdrs )}
                (pcl:pl-setf (vector $csv $in $fh $hdrs) (pl-cast-@ (pl-gethash %c (vector "csv" "in" "fh" "hdrs"))))
        
        ;; my %hdr (bare declaration)
        
        ;; if (ref $hdrs eq "HASH") {         %hdr  = %$hdrs;         $hdrs = "auto";         }
        ;; if (ref $hdrs eq "HASH")
        (pl-if         (pcl:pl-str-eq (pl-ref $hdrs) "HASH")
          (progn
            ;; %hdr  = %$hdrs
                        (pcl:pl-setf %hdr (pl-cast-% $hdrs))
            
            ;; $hdrs = "auto"
                        (pcl:pl-setf $hdrs "auto")
            
          )
          nil
        )
        
        ;; if ($c->{out} && !$c->{sink}) {        !$hdrs && ref $c->{'kh'} && $c->{'kh'} == \@internal_kh and             $hdrs = $c->{'kh'};          if (ref $in eq "CODE") {             my $hdr = 1;             while (my $row = $in->($csv)) {                 if (ref $row eq "ARRAY") {                     $csv->print ($fh, $row);                     next;                     }                 if (ref $row eq "HASH") {                     if ($hdr) {                         $hdrs ||= [ map { $hdr{$_} || $_ } keys %$row ];                         $csv->print ($fh, $hdrs);                         $hdr = 0;                         }                     $csv->print ($fh, [ @{$row}{@$hdrs} ]);                     }                 }             }         elsif (@{$in} == 0 or ref $in->[0] eq "ARRAY") { # aoa             ref $hdrs and $csv->print ($fh, $hdrs);             for (@{$in}) {                 $c->{cboi} and $c->{cboi}->($csv, $_);                 $c->{cbbo} and $c->{cbbo}->($csv, $_);                 $csv->print ($fh, $_);                 }             }         else { # aoh             my @hdrs = ref $hdrs ? @{$hdrs} : keys %{$in->[0]};             defined $hdrs or $hdrs = "auto";             ref $hdrs || $hdrs eq "auto" and @hdrs and                 $csv->print ($fh, [ map { $hdr{$_} || $_ } @hdrs ]);             for (@{$in}) {                 local %_;                 *_ = $_;                 $c->{cboi} and $c->{cboi}->($csv, $_);                 $c->{cbbo} and $c->{cbbo}->($csv, $_);                 $csv->print ($fh, [ @{$_}{@hdrs} ]);                 }             }          $c->{cls} and close $fh;         return 1;         }
        ;; if ($c->{"out"} && !$c->{"sink"})
        (pl-if         (pcl:pl-&& (pl-gethash-deref $c "out") (pl-! (pl-gethash-deref $c "sink")))
          (progn
            ;; !$hdrs && ref $c->{'kh'} && $c->{'kh'} == \@internal_kh and             $hdrs = $c->{'kh'}
                        (pcl:pl-and (pcl:pl-&& (pcl:pl-&& (pl-! $hdrs) (pl-gethash-deref (pl-ref $c) "kh")) (pcl:pl-== (pl-gethash-deref $c "kh") (pl-backslash @internal_kh))) (pcl:pl-setf $hdrs (pl-gethash-deref $c "kh")))
            
            ;; if (ref $in eq "CODE") {             my $hdr = 1;             while (my $row = $in->($csv)) {                 if (ref $row eq "ARRAY") {                     $csv->print ($fh, $row);                     next;                     }                 if (ref $row eq "HASH") {                     if ($hdr) {                         $hdrs ||= [ map { $hdr{$_} || $_ } keys %$row ];                         $csv->print ($fh, $hdrs);                         $hdr = 0;                         }                     $csv->print ($fh, [ @{$row}{@$hdrs} ]);                     }                 }             }         elsif (@{$in} == 0 or ref $in->[0] eq "ARRAY") { # aoa             ref $hdrs and $csv->print ($fh, $hdrs);             for (@{$in}) {                 $c->{cboi} and $c->{cboi}->($csv, $_);                 $c->{cbbo} and $c->{cbbo}->($csv, $_);                 $csv->print ($fh, $_);                 }             }         else { # aoh             my @hdrs = ref $hdrs ? @{$hdrs} : keys %{$in->[0]};             defined $hdrs or $hdrs = "auto";             ref $hdrs || $hdrs eq "auto" and @hdrs and                 $csv->print ($fh, [ map { $hdr{$_} || $_ } @hdrs ]);             for (@{$in}) {                 local %_;                 *_ = $_;                 $c->{cboi} and $c->{cboi}->($csv, $_);                 $c->{cbbo} and $c->{cbbo}->($csv, $_);                 $csv->print ($fh, [ @{$_}{@hdrs} ]);                 }             }
            ;; if (ref $in eq "CODE")
            (pl-if             (pcl:pl-str-eq (pl-ref $in) "CODE")
              (progn
                ;; my $hdr = 1
                                (pcl:pl-setf $hdr 1)
                
                ;; while (my $row = $in->($csv)) {                 if (ref $row eq "ARRAY") {                     $csv->print ($fh, $row);                     next;                     }                 if (ref $row eq "HASH") {                     if ($hdr) {                         $hdrs ||= [ map { $hdr{$_} || $_ } keys %$row ];                         $csv->print ($fh, $hdrs);                         $hdr = 0;                         }                     $csv->print ($fh, [ @{$row}{@$hdrs} ]);                     }                 }
                (let (($row (make-pl-box nil)))
                  (pl-while                 (pcl:pl-setf $row (pl-funcall-ref $in $csv))
                    ;; if (ref $row eq "ARRAY") {                     $csv->print ($fh, $row);                     next;                     }
                    ;; if (ref $row eq "ARRAY")
                    (pl-if                     (pcl:pl-str-eq (pl-ref $row) "ARRAY")
                      (progn
                        ;; $csv->print ($fh, $row)
                                                (pl-method-call $csv 'print $fh $row)
                        
                        ;; next
                                                (pl-next)
                        
                      )
                      nil
                    )
                    
                    ;; if (ref $row eq "HASH") {                     if ($hdr) {                         $hdrs ||= [ map { $hdr{$_} || $_ } keys %$row ];                         $csv->print ($fh, $hdrs);                         $hdr = 0;                         }                     $csv->print ($fh, [ @{$row}{@$hdrs} ]);                     }
                    ;; if (ref $row eq "HASH")
                    (pl-if                     (pcl:pl-str-eq (pl-ref $row) "HASH")
                      (progn
                        ;; if ($hdr) {                         $hdrs ||= [ map { $hdr{$_} || $_ } keys %$row ];                         $csv->print ($fh, $hdrs);                         $hdr = 0;                         }
                        ;; if ($hdr)
                        (pl-if                         $hdr
                          (progn
                            (defun --anon-block-90-- ($_)
                              ;; $hdr{$_} || $_
                                                            (pcl:pl-|| (pl-gethash %hdr $_) $_)
                              
                            )
                            
                            ;; $hdrs ||= [ map { $hdr{$_} || $_ } keys %$row ]
                                                        (pcl:pl-or-assign $hdrs (make-array 1 :adjustable t :fill-pointer t :initial-contents (list (let ((*wantarray* t)) (pl-map #'--anon-block-90-- (let ((*wantarray* t)) (pl-keys (pl-cast-% $row))))))))
                            
                            ;; $csv->print ($fh, $hdrs)
                                                        (pl-method-call $csv 'print $fh $hdrs)
                            
                            ;; $hdr = 0
                                                        (pcl:pl-setf $hdr 0)
                            
                          )
                          nil
                        )
                        
                        ;; $csv->print ($fh, [ @{$row}{@$hdrs} ])
                                                (pl-method-call $csv 'print $fh (make-array 1 :adjustable t :fill-pointer t :initial-contents (list (pl-cast-@ (pl-gethash %row (pl-cast-@ $hdrs))))))
                        
                      )
                      nil
                    )
                    
                  )
                )
                
              )
              ;; elsif (@{$in} == 0 or ref $in->[0] eq "ARRAY")
              (pl-if               (pcl:pl-or (pcl:pl-== (pl-cast-@ $in) 0) (pcl:pl-str-eq (pl-aref-deref (pl-ref $in) 0) "ARRAY"))
                (progn
                  ;; ref $hdrs and $csv->print ($fh, $hdrs)
                                    (pcl:pl-and (pl-ref $hdrs) (pl-method-call $csv 'print $fh $hdrs))
                  
                  ;; for (@{$in}) {                 $c->{cboi} and $c->{cboi}->($csv, $_);                 $c->{cbbo} and $c->{cbbo}->($csv, $_);                 $csv->print ($fh, $_);                 }
                  (pl-foreach ($_                   (pl-cast-@ $in))
                    ;; $c->{cboi} and $c->{cboi}->($csv, $_)
                                        (pcl:pl-and (pl-gethash-deref $c "cboi") (pl-funcall-ref (pl-gethash-deref $c "cboi") $csv $_))
                    
                    ;; $c->{cbbo} and $c->{cbbo}->($csv, $_)
                                        (pcl:pl-and (pl-gethash-deref $c "cbbo") (pl-funcall-ref (pl-gethash-deref $c "cbbo") $csv $_))
                    
                    ;; $csv->print ($fh, $_)
                                        (pl-method-call $csv 'print $fh $_)
                    
                  )
                  
                )
                ;; else
                (progn
                  ;; my @hdrs = ref $hdrs ? @{$hdrs} : keys %{$in->[0]}
                                    (pcl:pl-setf @hdrs (pl-if (pl-ref $hdrs) (pl-cast-@ $hdrs) (let ((*wantarray* t)) (pl-keys (pl-cast-% (pl-aref-deref $in 0))))))
                  
                  ;; defined $hdrs or $hdrs = "auto"
                                    (pcl:pl-or (pl-defined $hdrs) (pcl:pl-setf $hdrs "auto"))
                  
                  (defun --anon-block-91-- ($_)
                    ;; $hdr{$_} || $_
                                        (pcl:pl-|| (pl-gethash %hdr $_) $_)
                    
                  )
                  
                  ;; ref $hdrs || $hdrs eq "auto" and @hdrs and                 $csv->print ($fh, [ map { $hdr{$_} || $_ } @hdrs ])
                                    (pcl:pl-and (pcl:pl-and (pcl:pl-|| (pl-ref $hdrs) (pcl:pl-str-eq $hdrs "auto")) @hdrs) (pl-method-call $csv 'print $fh (make-array 1 :adjustable t :fill-pointer t :initial-contents (list (let ((*wantarray* t)) (pl-map #'--anon-block-91-- @hdrs))))))
                  
                  ;; for (@{$in}) {                 local %_;                 *_ = $_;                 $c->{cboi} and $c->{cboi}->($csv, $_);                 $c->{cbbo} and $c->{cbbo}->($csv, $_);                 $csv->print ($fh, [ @{$_}{@hdrs} ]);                 }
                  (pl-foreach ($_                   (pl-cast-@ $in))
                    ;; local %_
                    (let ((%_ (make-hash-table :test 'equal)))
                      
                      ;; *_ = $_
                                            (pcl:pl-setf *_ $_)
                      
                      ;; $c->{cboi} and $c->{cboi}->($csv, $_)
                                            (pcl:pl-and (pl-gethash-deref $c "cboi") (pl-funcall-ref (pl-gethash-deref $c "cboi") $csv $_))
                      
                      ;; $c->{cbbo} and $c->{cbbo}->($csv, $_)
                                            (pcl:pl-and (pl-gethash-deref $c "cbbo") (pl-funcall-ref (pl-gethash-deref $c "cbbo") $csv $_))
                      
                      ;; $csv->print ($fh, [ @{$_}{@hdrs} ])
                                            (pl-method-call $csv 'print $fh (make-array 1 :adjustable t :fill-pointer t :initial-contents (list (pl-cast-@ (pl-gethash %_ @hdrs)))))
                      
                    )  ;; end local
                  )
                  
                )
              )
            )
            
            ;; $c->{cls} and close $fh
                        (pcl:pl-and (pl-gethash-deref $c "cls") (pl-close $fh))
            
            ;; return 1
                        (pl-return 1)
            
          )
          nil
        )
        
        ;; my @row1 (bare declaration)
        
        ;; if (defined $c->{hd_s} || defined $c->{hd_b} || defined $c->{hd_m} || defined $c->{hd_c}) {         my %harg;         !defined $c->{'hd_s'} &&  $c->{'attr'}{'sep_char'} and                  $c->{'hd_s'} = [ $c->{'attr'}{'sep_char'} ];         !defined $c->{'hd_s'} &&  $c->{'attr'}{'sep'} and                  $c->{'hd_s'} = [ $c->{'attr'}{'sep'} ];         defined  $c->{'hd_s'} and $harg{'sep_set'}            = $c->{'hd_s'};         defined  $c->{'hd_b'} and $harg{'detect_bom'}         = $c->{'hd_b'};         defined  $c->{'hd_m'} and $harg{'munge_column_names'} = $hdrs ? "none" : $c->{'hd_m'};         defined  $c->{'hd_c'} and $harg{'set_column_names'}   = $hdrs ? 0      : $c->{'hd_c'};         @row1 = $csv->header ($fh, \%harg);         my @hdr = $csv->column_names;         @hdr and $hdrs ||= \@hdr;         }
        ;; if (defined $c->{"hd_s"} || defined $c->{"hd_b"} || defined $c->{"hd_m"} || defined $c->{"hd_c"})
        (pl-if         (pcl:pl-|| (pcl:pl-|| (pcl:pl-|| (pl-gethash-deref (pl-defined $c) "hd_s") (pl-gethash-deref (pl-defined $c) "hd_b")) (pl-gethash-deref (pl-defined $c) "hd_m")) (pl-gethash-deref (pl-defined $c) "hd_c"))
          (progn
            ;; my %harg (bare declaration)
            
            ;; !defined $c->{'hd_s'} &&  $c->{'attr'}{'sep_char'} and                  $c->{'hd_s'} = [ $c->{'attr'}{'sep_char'} ]
                        (pcl:pl-and (pcl:pl-&& (pl-! (pl-gethash-deref (pl-defined $c) "hd_s")) (pl-gethash (pl-gethash-deref $c "attr") "sep_char")) (pcl:pl-setf (pl-gethash-deref $c "hd_s") (make-array 1 :adjustable t :fill-pointer t :initial-contents (list (pl-gethash (pl-gethash-deref $c "attr") "sep_char")))))
            
            ;; !defined $c->{'hd_s'} &&  $c->{'attr'}{'sep'} and                  $c->{'hd_s'} = [ $c->{'attr'}{'sep'} ]
                        (pcl:pl-and (pcl:pl-&& (pl-! (pl-gethash-deref (pl-defined $c) "hd_s")) (pl-gethash (pl-gethash-deref $c "attr") "sep")) (pcl:pl-setf (pl-gethash-deref $c "hd_s") (make-array 1 :adjustable t :fill-pointer t :initial-contents (list (pl-gethash (pl-gethash-deref $c "attr") "sep")))))
            
            ;; defined  $c->{'hd_s'} and $harg{'sep_set'}            = $c->{'hd_s'}
                        (pcl:pl-and (pl-gethash-deref (pl-defined $c) "hd_s") (pcl:pl-setf (pl-gethash %harg "sep_set") (pl-gethash-deref $c "hd_s")))
            
            ;; defined  $c->{'hd_b'} and $harg{'detect_bom'}         = $c->{'hd_b'}
                        (pcl:pl-and (pl-gethash-deref (pl-defined $c) "hd_b") (pcl:pl-setf (pl-gethash %harg "detect_bom") (pl-gethash-deref $c "hd_b")))
            
            ;; defined  $c->{'hd_m'} and $harg{'munge_column_names'} = $hdrs ? "none" : $c->{'hd_m'}
                        (pcl:pl-and (pl-gethash-deref (pl-defined $c) "hd_m") (pcl:pl-setf (pl-gethash %harg "munge_column_names") (pl-if $hdrs "none" (pl-gethash-deref $c "hd_m"))))
            
            ;; defined  $c->{'hd_c'} and $harg{'set_column_names'}   = $hdrs ? 0      : $c->{'hd_c'}
                        (pcl:pl-and (pl-gethash-deref (pl-defined $c) "hd_c") (pcl:pl-setf (pl-gethash %harg "set_column_names") (pl-if $hdrs 0 (pl-gethash-deref $c "hd_c"))))
            
            ;; @row1 = $csv->header ($fh, \%harg)
                        (pcl:pl-setf @row1 (let ((*wantarray* t)) (pl-method-call $csv 'header $fh (pl-backslash %harg))))
            
            ;; my @hdr = $csv->column_names
                        (pcl:pl-setf @hdr (let ((*wantarray* t)) (pl-method-call $csv 'column_names)))
            
            ;; @hdr and $hdrs ||= \@hdr
                        (pcl:pl-and @hdr (pcl:pl-or-assign $hdrs (pl-backslash @hdr)))
            
          )
          nil
        )
        
        ;; if ($c->{kh}) {         @internal_kh = ();         ref $c->{kh} eq "ARRAY" or croak ($csv->SetDiag (1501));         $hdrs ||= "auto";         }
        ;; if ($c->{"kh"})
        (pl-if         (pl-gethash-deref $c "kh")
          (progn
            ;; @internal_kh = ()
                        (pcl:pl-setf @internal_kh (vector ))
            
            ;; ref $c->{kh} eq "ARRAY" or croak ($csv->SetDiag (1501))
                        (pcl:pl-or (pcl:pl-str-eq (pl-gethash-deref (pl-ref $c) "kh") "ARRAY") (pl-croak (pl-method-call $csv 'SetDiag 1501)))
            
            ;; $hdrs ||= "auto"
                        (pcl:pl-or-assign $hdrs "auto")
            
          )
          nil
        )
        
        ;; my $key = $c->{key}
                (pcl:pl-setf $key (pl-gethash-deref $c "key"))
        
        ;; if ($key) {        !ref $key or ref $key eq "ARRAY" && @$key > 1 or croak ($csv->SetDiag (1501));         $hdrs ||= "auto";         }
        ;; if ($key)
        (pl-if         $key
          (progn
            ;; !ref $key or ref $key eq "ARRAY" && @$key > 1 or croak ($csv->SetDiag (1501))
                        (pcl:pl-or (pcl:pl-or (pl-! (pl-ref $key)) (pcl:pl-&& (pcl:pl-str-eq (pl-ref $key) "ARRAY") (pcl:pl-> (pl-cast-@ $key) 1))) (pl-croak (pl-method-call $csv 'SetDiag 1501)))
            
            ;; $hdrs ||= "auto"
                        (pcl:pl-or-assign $hdrs "auto")
            
          )
          nil
        )
        
        ;; my $val = $c->{val}
                (pcl:pl-setf $val (pl-gethash-deref $c "val"))
        
        ;; if ($val) {        $key                                          or croak ($csv->SetDiag (1502));        !ref $val or ref $val eq "ARRAY" && @$val > 0 or croak ($csv->SetDiag (1503));        }
        ;; if ($val)
        (pl-if         $val
          (progn
            ;; $key                                          or croak ($csv->SetDiag (1502))
                        (pcl:pl-or $key (pl-croak (pl-method-call $csv 'SetDiag 1502)))
            
            ;; !ref $val or ref $val eq "ARRAY" && @$val > 0 or croak ($csv->SetDiag (1503))
                        (pcl:pl-or (pcl:pl-or (pl-! (pl-ref $val)) (pcl:pl-&& (pcl:pl-str-eq (pl-ref $val) "ARRAY") (pcl:pl-> (pl-cast-@ $val) 0))) (pl-croak (pl-method-call $csv 'SetDiag 1503)))
            
          )
          nil
        )
        
        ;; $c->{fltr} && grep m/\D/ => keys %{$c->{fltr}} and $hdrs ||= "auto"
                (pcl:pl-or-assign (pcl:pl-&& (pl-gethash-deref $c "fltr") (pl-grep (pcl:pl-=~ $_ (pl-regex "m/\\D/")) (pcl:pl-and (let ((*wantarray* t)) (pl-keys (pl-cast-% (pl-gethash-deref $c "fltr")))) $hdrs))) "auto")
        
        ;; if (defined $hdrs) {         if (!ref $hdrs or ref $hdrs eq "CODE") {             my $h = $c->{'hd_b'}                 ? [ $csv->column_names () ]                 :   $csv->getline ($fh);             my $has_h = $h && @$h;              if (ref $hdrs) {                 $has_h or return;                 my $cr = $hdrs;                 $hdrs  = [ map {  $cr->($hdr{$_} || $_) } @{$h} ];                 }             elsif ($hdrs eq "skip") {                 # discard;                 }             elsif ($hdrs eq "auto") {                 $has_h or return;                 $hdrs = [ map {      $hdr{$_} || $_ } @$h ];                 }             elsif ($hdrs eq "lc") {                 $has_h or return;                 $hdrs = [ map { lc ($hdr{$_} || $_) } @$h ];                 }             elsif ($hdrs eq "uc") {                 $has_h or return;                 $hdrs = [ map { uc ($hdr{$_} || $_) } @$h ];                 }             }         $c->{kh} and $hdrs and @{$c->{kh}} = @$hdrs;         }
        ;; if (defined $hdrs)
        (pl-if         (pl-defined $hdrs)
          (progn
            ;; if (!ref $hdrs or ref $hdrs eq "CODE") {             my $h = $c->{'hd_b'}                 ? [ $csv->column_names () ]                 :   $csv->getline ($fh);             my $has_h = $h && @$h;              if (ref $hdrs) {                 $has_h or return;                 my $cr = $hdrs;                 $hdrs  = [ map {  $cr->($hdr{$_} || $_) } @{$h} ];                 }             elsif ($hdrs eq "skip") {                 # discard;                 }             elsif ($hdrs eq "auto") {                 $has_h or return;                 $hdrs = [ map {      $hdr{$_} || $_ } @$h ];                 }             elsif ($hdrs eq "lc") {                 $has_h or return;                 $hdrs = [ map { lc ($hdr{$_} || $_) } @$h ];                 }             elsif ($hdrs eq "uc") {                 $has_h or return;                 $hdrs = [ map { uc ($hdr{$_} || $_) } @$h ];                 }             }
            ;; if (!ref $hdrs or ref $hdrs eq "CODE")
            (pl-if             (pcl:pl-or (pl-! (pl-ref $hdrs)) (pcl:pl-str-eq (pl-ref $hdrs) "CODE"))
              (progn
                ;; my $h = $c->{'hd_b'}                 ? [ $csv->column_names () ]                 :   $csv->getline ($fh)
                                (pcl:pl-setf $h (pl-if (pl-gethash-deref $c "hd_b") (make-array 1 :adjustable t :fill-pointer t :initial-contents (list (let ((*wantarray* t)) (pl-method-call $csv 'column_names)))) (pl-method-call $csv 'getline $fh)))
                
                ;; my $has_h = $h && @$h
                                (pcl:pl-setf $has_h (pcl:pl-&& $h (pl-cast-@ $h)))
                
                ;; if (ref $hdrs) {                 $has_h or return;                 my $cr = $hdrs;                 $hdrs  = [ map {  $cr->($hdr{$_} || $_) } @{$h} ];                 }             elsif ($hdrs eq "skip") {                 # discard;                 }             elsif ($hdrs eq "auto") {                 $has_h or return;                 $hdrs = [ map {      $hdr{$_} || $_ } @$h ];                 }             elsif ($hdrs eq "lc") {                 $has_h or return;                 $hdrs = [ map { lc ($hdr{$_} || $_) } @$h ];                 }             elsif ($hdrs eq "uc") {                 $has_h or return;                 $hdrs = [ map { uc ($hdr{$_} || $_) } @$h ];                 }
                ;; if (ref $hdrs)
                (pl-if                 (pl-ref $hdrs)
                  (progn
                    ;; $has_h or return
                                        (pcl:pl-or $has_h (pl-return))
                    
                    ;; my $cr = $hdrs
                                        (pcl:pl-setf $cr $hdrs)
                    
                    (defun --anon-block-92-- ($_)
                      ;; $cr->($hdr{$_} || $_)
                                            (pl-funcall-ref $cr (pcl:pl-|| (pl-gethash %hdr $_) $_))
                      
                    )
                    
                    ;; $hdrs  = [ map {  $cr->($hdr{$_} || $_) } @{$h} ]
                                        (pcl:pl-setf $hdrs (make-array 1 :adjustable t :fill-pointer t :initial-contents (list (let ((*wantarray* t)) (pl-map #'--anon-block-92-- (pl-cast-@ $h))))))
                    
                  )
                  ;; elsif ($hdrs eq "skip")
                  (pl-if                   (pcl:pl-str-eq $hdrs "skip")
                    (progn
                    )
                    ;; elsif ($hdrs eq "auto")
                    (pl-if                     (pcl:pl-str-eq $hdrs "auto")
                      (progn
                        ;; $has_h or return
                                                (pcl:pl-or $has_h (pl-return))
                        
                        (defun --anon-block-93-- ($_)
                          ;; $hdr{$_} || $_
                                                    (pcl:pl-|| (pl-gethash %hdr $_) $_)
                          
                        )
                        
                        ;; $hdrs = [ map {      $hdr{$_} || $_ } @$h ]
                                                (pcl:pl-setf $hdrs (make-array 1 :adjustable t :fill-pointer t :initial-contents (list (let ((*wantarray* t)) (pl-map #'--anon-block-93-- (pl-cast-@ $h))))))
                        
                      )
                      ;; elsif ($hdrs eq "lc")
                      (pl-if                       (pcl:pl-str-eq $hdrs "lc")
                        (progn
                          ;; $has_h or return
                                                    (pcl:pl-or $has_h (pl-return))
                          
                          (defun --anon-block-94-- ($_)
                            ;; lc ($hdr{$_} || $_)
                                                        (pl-lc (pcl:pl-|| (pl-gethash %hdr $_) $_))
                            
                          )
                          
                          ;; $hdrs = [ map { lc ($hdr{$_} || $_) } @$h ]
                                                    (pcl:pl-setf $hdrs (make-array 1 :adjustable t :fill-pointer t :initial-contents (list (let ((*wantarray* t)) (pl-map #'--anon-block-94-- (pl-cast-@ $h))))))
                          
                        )
                        ;; elsif ($hdrs eq "uc")
                        (pl-if                         (pcl:pl-str-eq $hdrs "uc")
                          (progn
                            ;; $has_h or return
                                                        (pcl:pl-or $has_h (pl-return))
                            
                            (defun --anon-block-95-- ($_)
                              ;; uc ($hdr{$_} || $_)
                                                            (pl-uc (pcl:pl-|| (pl-gethash %hdr $_) $_))
                              
                            )
                            
                            ;; $hdrs = [ map { uc ($hdr{$_} || $_) } @$h ]
                                                        (pcl:pl-setf $hdrs (make-array 1 :adjustable t :fill-pointer t :initial-contents (list (let ((*wantarray* t)) (pl-map #'--anon-block-95-- (pl-cast-@ $h))))))
                            
                          )
                          nil
                        )
                      )
                    )
                  )
                )
                
              )
              nil
            )
            
            ;; $c->{kh} and $hdrs and @{$c->{kh}} = @$hdrs
                        (pcl:pl-and (pcl:pl-and (pl-gethash-deref $c "kh") $hdrs) (pcl:pl-setf (pl-cast-@ (pl-gethash-deref $c "kh")) (pl-cast-@ $hdrs)))
            
          )
          nil
        )
        
        ;; if ($c->{fltr}) {         my %f = %{$c->{fltr}};         # convert headers to index         my @hdr;         if (ref $hdrs) {             @hdr = @{$hdrs};             for (0 .. $#hdr) {                 exists $f{$hdr[$_]} and $f{$_ + 1} = delete $f{$hdr[$_]};                 }             }         $csv->callbacks (after_parse => sub {             my ($CSV, $ROW) = @_; # lexical sub-variables in caps             foreach my $FLD (sort keys %f) {                 local $_ = $ROW->[$FLD - 1];                 local %_;                 @hdr and @_{@hdr} = @$ROW;                 $f{$FLD}->($CSV, $ROW) or return \"skip";                 $ROW->[$FLD - 1] = $_;                 }             });         }
        ;; if ($c->{"fltr"})
        (pl-if         (pl-gethash-deref $c "fltr")
          (progn
            ;; my %f = %{$c->{fltr}}
                        (pcl:pl-setf %f (pl-cast-% (pl-gethash-deref $c "fltr")))
            
            ;; my @hdr (bare declaration)
            
            ;; if (ref $hdrs) {             @hdr = @{$hdrs};             for (0 .. $#hdr) {                 exists $f{$hdr[$_]} and $f{$_ + 1} = delete $f{$hdr[$_]};                 }             }
            ;; if (ref $hdrs)
            (pl-if             (pl-ref $hdrs)
              (progn
                ;; @hdr = @{$hdrs}
                                (pcl:pl-setf @hdr (pl-cast-@ $hdrs))
                
                ;; for (0 .. $#hdr) {                 exists $f{$hdr[$_]} and $f{$_ + 1} = delete $f{$hdr[$_]};                 }
                (pl-foreach ($_                 (pcl:pl-.. 0 (pl-array-last-index @hdr)))
                  ;; exists $f{$hdr[$_]} and $f{$_ + 1} = delete $f{$hdr[$_]}
                                    (pcl:pl-and (pl-exists (pl-gethash %f (pl-aref @hdr $_))) (pcl:pl-setf (pl-gethash %f (pcl:pl-+ $_ 1)) (pl-delete (pl-gethash %f (pl-aref @hdr $_)))))
                  
                )
                
              )
              nil
            )
            
            (defun --anon-block-96-- ()
              ;; my ($CSV, $ROW) = @_
                            (pcl:pl-setf (vector $CSV $ROW) @_)
              
              ;; foreach my $FLD (sort keys %f) {                 local $_ = $ROW->[$FLD - 1];                 local %_;                 @hdr and @_{@hdr} = @$ROW;                 $f{$FLD}->($CSV, $ROW) or return \"skip";                 $ROW->[$FLD - 1] = $_;                 }
              (pl-foreach ($FLD               (pl-sort (pl-keys %f)))
                ;; local %_
                (let ((%_ (make-hash-table :test 'equal)))
                  
                  ;; @hdr and @_{@hdr} = @$ROW
                                    (pcl:pl-and @hdr (pcl:pl-setf (pl-hslice @_ @hdr) (pl-cast-@ $ROW)))
                  
                  ;; $f{$FLD}->($CSV, $ROW) or return \"skip"
                                    (pcl:pl-or (pl-funcall-ref (pl-gethash %f $FLD) $CSV $ROW) (pl-return (pl-backslash "skip")))
                  
                  ;; $ROW->[$FLD - 1] = $_
                                    (pcl:pl-setf (pl-aref-deref $ROW (pcl:pl-- $FLD 1)) $_)
                  
                )  ;; end local
              )
              
            )
            
            ;; $csv->callbacks (after_parse => sub {             my ($CSV, $ROW) = @_; # lexical sub-variables in caps             foreach my $FLD (sort keys %f) {                 local $_ = $ROW->[$FLD - 1];                 local %_;                 @hdr and @_{@hdr} = @$ROW;                 $f{$FLD}->($CSV, $ROW) or return \"skip";                 $ROW->[$FLD - 1] = $_;                 }             })
                        (pl-method-call $csv 'callbacks "after_parse" #'--anon-block-96--)
            
          )
          nil
        )
        
        ;; my $frag = $c->{frag}
                (pcl:pl-setf $frag (pl-gethash-deref $c "frag"))
        
        ;; my $ref = ref $hdrs         ? # aoh           do {             my @h = $csv->column_names ($hdrs);             my %h; $h{$_}++ for @h;             exists $h{''} and croak ($csv->SetDiag (1012));             unless (keys %h == @h) {                 croak ($csv->_SetDiagInfo (1013, join ", " =>                     map { "$_ ($h{$_})" } grep { $h{$_} > 1 } keys %h));                 }             $frag ? $csv->fragment ($fh, $frag) :             $key  ? do {                         my ($k, $j, @f) = ref $key ? (undef, @$key) : ($key);                         if (my @mk = grep { !exists $h{$_} } grep { defined } $k, @f) {                             croak ($csv->_SetDiagInfo (4001, join ", " => @mk));                             }                         +{ map {                             my $r = $_;                             my $K = defined $k ? $r->{$k} : join $j => @{$r}{@f};                             ( $K => (                             $val                                 ? ref $val                                     ? { map { $_ => $r->{$_} } @$val }                                     : $r->{$val}                                 : $r ));                             } @{$csv->getline_hr_all ($fh)} }                         }                   : $csv->getline_hr_all ($fh);             }         : # aoa             $frag ? $csv->fragment ($fh, $frag)                   : $csv->getline_all ($fh)
        ;; PARSE ERROR: Fell through. Missing case: [
        
        ;; if ($ref) {         @row1 && !$c->{hd_c} && !ref $hdrs and unshift @$ref, \@row1;         }     else {         Text::CSV_PP->auto_diag;         }
        ;; if ($ref)
        (pl-if         $ref
          (progn
            ;; @row1 && !$c->{hd_c} && !ref $hdrs and unshift @$ref, \@row1
                        (pcl:pl-and (pcl:pl-&& (pcl:pl-&& @row1 (pl-! (pl-gethash-deref $c "hd_c"))) (pl-! (pl-ref $hdrs))) (pl-unshift (pl-cast-@ $ref) (pl-backslash @row1)))
            
          )
          ;; else
          (progn
            ;; Text::CSV_PP->auto_diag
                        (pl-method-call "Text::CSV_PP" 'auto_diag)
            
          )
        )
        
        ;; $c->{cls} and close $fh
                (pcl:pl-and (pl-gethash-deref $c "cls") (pl-close $fh))
        
        ;; if ($ref and $c->{cbai} || $c->{cboi}) {         # Default is ARRAYref, but with key =>, you'll get a hashref         foreach my $r (ref $ref eq "ARRAY" ? @{$ref} : values %{$ref}) {             local %_;             ref $r eq "HASH" and *_ = $r;             $c->{cbai} and $c->{cbai}->($csv, $r);             $c->{cboi} and $c->{cboi}->($csv, $r);             }         }
        ;; if ($ref and $c->{"cbai"} || $c->{"cboi"})
        (pl-if         (pcl:pl-and $ref (pcl:pl-|| (pl-gethash-deref $c "cbai") (pl-gethash-deref $c "cboi")))
          (progn
            ;; foreach my $r (ref $ref eq "ARRAY" ? @{$ref} : values %{$ref}) {             local %_;             ref $r eq "HASH" and *_ = $r;             $c->{cbai} and $c->{cbai}->($csv, $r);             $c->{cboi} and $c->{cboi}->($csv, $r);             }
            (pl-foreach ($r             (pl-if (pcl:pl-str-eq (pl-ref $ref) "ARRAY") (pl-cast-@ $ref) (pl-values (pl-cast-% $ref))))
              ;; local %_
              (let ((%_ (make-hash-table :test 'equal)))
                
                ;; ref $r eq "HASH" and *_ = $r
                                (pcl:pl-and (pcl:pl-str-eq (pl-ref $r) "HASH") (pcl:pl-setf *_ $r))
                
                ;; $c->{cbai} and $c->{cbai}->($csv, $r)
                                (pcl:pl-and (pl-gethash-deref $c "cbai") (pl-funcall-ref (pl-gethash-deref $c "cbai") $csv $r))
                
                ;; $c->{cboi} and $c->{cboi}->($csv, $r)
                                (pcl:pl-and (pl-gethash-deref $c "cboi") (pl-funcall-ref (pl-gethash-deref $c "cboi") $csv $r))
                
              )  ;; end local
            )
            
          )
          nil
        )
        
        ;; if ($c->{sink}) {         my $ro = ref $c->{out} or return;          $ro eq "SCALAR" && ${$c->{out}} eq "skip" and             return;          $ro eq ref $ref or             croak ($csv->_SetDiagInfo (5001, "Output type mismatch"));          if ($ro eq "ARRAY") {             if (@{$c->{out}} and @$ref and ref $c->{out}[0] eq ref $ref->[0]) {                 push @{$c->{out}} => @$ref;                 return $c->{out};             }             croak ($csv->_SetDiagInfo (5001, "Output type mismatch"));         }          if ($ro eq "HASH") {             @{$c->{out}}{keys %{$ref}} = values %{$ref};             return $c->{out};         }          croak ($csv->_SetDiagInfo (5002, "Unsupported output type"));     }
        ;; if ($c->{"sink"})
        (pl-if         (pl-gethash-deref $c "sink")
          (progn
            ;; my $ro = ref $c->{out} or return
                        (pcl:pl-or (pcl:pl-setf $ro (pl-gethash-deref (pl-ref $c) "out")) (pl-return))
            
            ;; $ro eq "SCALAR" && ${$c->{out}} eq "skip" and             return
                        (pcl:pl-and (pcl:pl-&& (pcl:pl-str-eq $ro "SCALAR") (pcl:pl-str-eq (pl-cast-$ (pl-gethash-deref $c "out")) "skip")) (pl-return))
            
            ;; $ro eq ref $ref or             croak ($csv->_SetDiagInfo (5001, "Output type mismatch"))
                        (pcl:pl-or (pcl:pl-str-eq $ro (pl-ref $ref)) (pl-croak (pl-method-call $csv '_SetDiagInfo 5001 "Output type mismatch")))
            
            ;; if ($ro eq "ARRAY") {             if (@{$c->{out}} and @$ref and ref $c->{out}[0] eq ref $ref->[0]) {                 push @{$c->{out}} => @$ref;                 return $c->{out};             }             croak ($csv->_SetDiagInfo (5001, "Output type mismatch"));         }
            ;; if ($ro eq "ARRAY")
            (pl-if             (pcl:pl-str-eq $ro "ARRAY")
              (progn
                ;; if (@{$c->{out}} and @$ref and ref $c->{out}[0] eq ref $ref->[0]) {                 push @{$c->{out}} => @$ref;                 return $c->{out};             }
                ;; if (@{$c->{"out"}} and @$ref and ref $c->{"out"}[0] eq ref $ref->[0])
                (pl-if                 (pcl:pl-and (pcl:pl-and (pl-cast-@ (pl-gethash-deref $c "out")) (pl-cast-@ $ref)) (pcl:pl-str-eq (pl-aref (pl-gethash-deref (pl-ref $c) "out") 0) (pl-aref-deref (pl-ref $ref) 0)))
                  (progn
                    ;; push @{$c->{out}} => @$ref
                                        (pl-push (pl-cast-@ (pl-gethash-deref $c "out")) (pl-cast-@ $ref))
                    
                    ;; return $c->{out}
                                        (pl-return (pl-gethash-deref $c "out"))
                    
                  )
                  nil
                )
                
                ;; croak ($csv->_SetDiagInfo (5001, "Output type mismatch"))
                                (pl-croak (pl-method-call $csv '_SetDiagInfo 5001 "Output type mismatch"))
                
              )
              nil
            )
            
            ;; if ($ro eq "HASH") {             @{$c->{out}}{keys %{$ref}} = values %{$ref};             return $c->{out};         }
            ;; if ($ro eq "HASH")
            (pl-if             (pcl:pl-str-eq $ro "HASH")
              (progn
                ;; @{$c->{out}}{keys %{$ref}} = values %{$ref}
                                (pcl:pl-setf (pl-cast-@ (pl-gethash (pl-gethash-deref $c "out") (pl-keys (pl-cast-% $ref)))) (pl-values (pl-cast-% $ref)))
                
                ;; return $c->{out}
                                (pl-return (pl-gethash-deref $c "out"))
                
              )
              nil
            )
            
            ;; croak ($csv->_SetDiagInfo (5002, "Unsupported output type"))
                        (pl-croak (pl-method-call $csv '_SetDiagInfo 5002 "Unsupported output type"))
            
          )
          nil
        )
        
        ;; defined wantarray or         return csv (             in => $ref,             headers => $hdrs,             %{$c->{attr}},         )
                (pcl:pl-or (pl-defined (pl-wantarray)) (pl-return (pl-csv "in" $ref "headers" $hdrs (pl-cast-% (pl-gethash-deref $c "attr")))))
        
        ;; return $ref
                (pl-return $ref)
        
      )
    )
  )
)

;; # The end of the common pure perl part.
;; ################################################################################
;; #
;; # The following are methods implemented in XS in Text::CSV_XS or
;; # helper methods for Text::CSV_PP only
;; #
;; ################################################################################
;; sub _setup_ctx { ... }
(pl-sub pl-_setup_ctx (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($ctx (make-pl-box nil)) ($sep_len (make-pl-box nil)) ($quote_char (make-pl-box nil)) ($quote_len (make-pl-box nil)) ($escape_char (make-pl-box nil)) ($eol (make-pl-box nil)) ($eol_len (make-pl-box nil)) ($cb (make-pl-box nil)) ($bound (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; $last_error = undef
                (pcl:pl-setf $last_error (pl-undef))
        
        ;; my $ctx (bare declaration)
        
        ;; if ($self->{_CACHE}) {         %$ctx = %{$self->{_CACHE}};     } else {         $ctx->{sep} = ',';         if (defined $self->{sep_char}) {             $ctx->{sep} = $self->{sep_char};         }         if (defined $self->{sep} and $self->{sep} ne '') {             use bytes;             $ctx->{sep} = $self->{sep};             my $sep_len = length($ctx->{sep});             $ctx->{sep_len} = $sep_len if $sep_len > 1;         }          $ctx->{quo} = '"';         if (exists $self->{quote_char}) {             my $quote_char = $self->{quote_char};             if (defined $quote_char and length $quote_char) {                 $ctx->{quo} = $quote_char;             } else {                 $ctx->{quo} = "\0";             }         }         if (defined $self->{quote} and $self->{quote} ne '') {             use bytes;             $ctx->{quo} = $self->{quote};             my $quote_len = length($ctx->{quo});             $ctx->{quo_len} = $quote_len if $quote_len > 1;         }          $ctx->{escape_char} = '"';         if (exists $self->{escape_char}) {             my $escape_char = $self->{escape_char};             if (defined $escape_char and length $escape_char) {                 $ctx->{escape_char} = $escape_char;             } else {                 $ctx->{escape_char} = "\0";             }         }          if (defined $self->{eol}) {             my $eol = $self->{eol};             my $eol_len = length($eol);             $ctx->{eol} = $eol;             $ctx->{eol_len} = $eol_len;             if ($eol_len == 1 and $eol eq "\015") {                 $ctx->{eol_is_cr} = 1;             }         }          $ctx->{undef_flg} = 0;         if (defined $self->{undef_str}) {             $ctx->{undef_str} = $self->{undef_str};             $ctx->{undef_flg} = 3 if utf8::is_utf8($self->{undef_str});         } else {             $ctx->{undef_str} = undef;         }         if (defined $self->{comment_str}) {             $ctx->{comment_str} = $self->{comment_str};         }          if (defined $self->{_types}) {             $ctx->{types} = $self->{_types};             $ctx->{types_len} = length($ctx->{types});         }          if (defined $self->{_is_bound}) {             $ctx->{is_bound} = $self->{_is_bound};         }          if (defined $self->{callbacks}) {             my $cb = $self->{callbacks};             $ctx->{has_hooks} = 0;             if (defined $cb->{after_parse} and ref $cb->{after_parse} eq 'CODE') {                 $ctx->{has_hooks} |= HOOK_AFTER_PARSE;             }             if (defined $cb->{before_print} and ref $cb->{before_print} eq 'CODE') {                 $ctx->{has_hooks} |= HOOK_BEFORE_PRINT;             }         }          for (qw/             binary decode_utf8 always_quote strict quote_empty             allow_loose_quotes allow_loose_escapes             allow_unquoted_escape allow_whitespace blank_is_undef             empty_is_undef verbatim auto_diag diag_verbose             keep_meta_info formula skip_empty_rows         /) {             $ctx->{$_} = defined $self->{$_} ? $self->{$_} : 0;         }         for (qw/quote_space escape_null quote_binary/) {             $ctx->{$_} = defined $self->{$_} ? $self->{$_} : 1;         }         if ($ctx->{escape_char} eq "\0") {             $ctx->{escape_null} = 0;         }          # FIXME: readonly         %{$self->{_CACHE}} = %$ctx;     }
        ;; if ($self->{"_CACHE"})
        (pl-if         (pl-gethash-deref $self "_CACHE")
          (progn
            ;; %$ctx = %{$self->{_CACHE}}
                        (pcl:pl-setf (pl-cast-% $ctx) (pl-cast-% (pl-gethash-deref $self "_CACHE")))
            
          )
          ;; else
          (progn
            ;; $ctx->{sep} = ','
                        (pcl:pl-setf (pl-gethash-deref $ctx "sep") ",")
            
            ;; if (defined $self->{sep_char}) {             $ctx->{sep} = $self->{sep_char};         }
            ;; if (defined $self->{"sep_char"})
            (pl-if             (pl-gethash-deref (pl-defined $self) "sep_char")
              (progn
                ;; $ctx->{sep} = $self->{sep_char}
                                (pcl:pl-setf (pl-gethash-deref $ctx "sep") (pl-gethash-deref $self "sep_char"))
                
              )
              nil
            )
            
            ;; if (defined $self->{sep} and $self->{sep} ne '') {             use bytes;             $ctx->{sep} = $self->{sep};             my $sep_len = length($ctx->{sep});             $ctx->{sep_len} = $sep_len if $sep_len > 1;         }
            ;; if (defined $self->{"sep"} and $self->{"sep"} ne '')
            (pl-if             (pcl:pl-and (pl-gethash-deref (pl-defined $self) "sep") (pcl:pl-str-ne (pl-gethash-deref $self "sep") ""))
              (progn
                ;; use bytes
                (eval-when (:compile-toplevel :load-toplevel :execute)
                  (pl-use "bytes"))
                
                ;; $ctx->{sep} = $self->{sep}
                                (pcl:pl-setf (pl-gethash-deref $ctx "sep") (pl-gethash-deref $self "sep"))
                
                ;; my $sep_len = length($ctx->{sep})
                                (pcl:pl-setf $sep_len (pl-length (pl-gethash-deref $ctx "sep")))
                
                ;; $ctx->{sep_len} = $sep_len if $sep_len > 1
                (pl-if                 (pcl:pl-> $sep_len 1)                 (pcl:pl-setf (pl-gethash-deref $ctx "sep_len") $sep_len))
                
              )
              nil
            )
            
            ;; $ctx->{quo} = '"'
                        (pcl:pl-setf (pl-gethash-deref $ctx "quo") "\"")
            
            ;; if (exists $self->{quote_char}) {             my $quote_char = $self->{quote_char};             if (defined $quote_char and length $quote_char) {                 $ctx->{quo} = $quote_char;             } else {                 $ctx->{quo} = "\0";             }         }
            ;; if (exists $self->{"quote_char"})
            (pl-if             (pl-gethash-deref (pl-exists $self) "quote_char")
              (progn
                ;; my $quote_char = $self->{quote_char}
                                (pcl:pl-setf $quote_char (pl-gethash-deref $self "quote_char"))
                
                ;; if (defined $quote_char and length $quote_char) {                 $ctx->{quo} = $quote_char;             } else {                 $ctx->{quo} = "\0";             }
                ;; if (defined $quote_char and length $quote_char)
                (pl-if                 (pcl:pl-and (pl-defined $quote_char) (pl-length $quote_char))
                  (progn
                    ;; $ctx->{quo} = $quote_char
                                        (pcl:pl-setf (pl-gethash-deref $ctx "quo") $quote_char)
                    
                  )
                  ;; else
                  (progn
                    ;; $ctx->{quo} = "\0"
                                        (pcl:pl-setf (pl-gethash-deref $ctx "quo") " ")
                    
                  )
                )
                
              )
              nil
            )
            
            ;; if (defined $self->{quote} and $self->{quote} ne '') {             use bytes;             $ctx->{quo} = $self->{quote};             my $quote_len = length($ctx->{quo});             $ctx->{quo_len} = $quote_len if $quote_len > 1;         }
            ;; if (defined $self->{"quote"} and $self->{"quote"} ne '')
            (pl-if             (pcl:pl-and (pl-gethash-deref (pl-defined $self) "quote") (pcl:pl-str-ne (pl-gethash-deref $self "quote") ""))
              (progn
                ;; use bytes
                (eval-when (:compile-toplevel :load-toplevel :execute)
                  (pl-use "bytes"))
                
                ;; $ctx->{quo} = $self->{quote}
                                (pcl:pl-setf (pl-gethash-deref $ctx "quo") (pl-gethash-deref $self "quote"))
                
                ;; my $quote_len = length($ctx->{quo})
                                (pcl:pl-setf $quote_len (pl-length (pl-gethash-deref $ctx "quo")))
                
                ;; $ctx->{quo_len} = $quote_len if $quote_len > 1
                (pl-if                 (pcl:pl-> $quote_len 1)                 (pcl:pl-setf (pl-gethash-deref $ctx "quo_len") $quote_len))
                
              )
              nil
            )
            
            ;; $ctx->{escape_char} = '"'
                        (pcl:pl-setf (pl-gethash-deref $ctx "escape_char") "\"")
            
            ;; if (exists $self->{escape_char}) {             my $escape_char = $self->{escape_char};             if (defined $escape_char and length $escape_char) {                 $ctx->{escape_char} = $escape_char;             } else {                 $ctx->{escape_char} = "\0";             }         }
            ;; if (exists $self->{"escape_char"})
            (pl-if             (pl-gethash-deref (pl-exists $self) "escape_char")
              (progn
                ;; my $escape_char = $self->{escape_char}
                                (pcl:pl-setf $escape_char (pl-gethash-deref $self "escape_char"))
                
                ;; if (defined $escape_char and length $escape_char) {                 $ctx->{escape_char} = $escape_char;             } else {                 $ctx->{escape_char} = "\0";             }
                ;; if (defined $escape_char and length $escape_char)
                (pl-if                 (pcl:pl-and (pl-defined $escape_char) (pl-length $escape_char))
                  (progn
                    ;; $ctx->{escape_char} = $escape_char
                                        (pcl:pl-setf (pl-gethash-deref $ctx "escape_char") $escape_char)
                    
                  )
                  ;; else
                  (progn
                    ;; $ctx->{escape_char} = "\0"
                                        (pcl:pl-setf (pl-gethash-deref $ctx "escape_char") " ")
                    
                  )
                )
                
              )
              nil
            )
            
            ;; if (defined $self->{eol}) {             my $eol = $self->{eol};             my $eol_len = length($eol);             $ctx->{eol} = $eol;             $ctx->{eol_len} = $eol_len;             if ($eol_len == 1 and $eol eq "\015") {                 $ctx->{eol_is_cr} = 1;             }         }
            ;; if (defined $self->{"eol"})
            (pl-if             (pl-gethash-deref (pl-defined $self) "eol")
              (progn
                ;; my $eol = $self->{eol}
                                (pcl:pl-setf $eol (pl-gethash-deref $self "eol"))
                
                ;; my $eol_len = length($eol)
                                (pcl:pl-setf $eol_len (pl-length $eol))
                
                ;; $ctx->{eol} = $eol
                                (pcl:pl-setf (pl-gethash-deref $ctx "eol") $eol)
                
                ;; $ctx->{eol_len} = $eol_len
                                (pcl:pl-setf (pl-gethash-deref $ctx "eol_len") $eol_len)
                
                ;; if ($eol_len == 1 and $eol eq "\015") {                 $ctx->{eol_is_cr} = 1;             }
                ;; if ($eol_len == 1 and $eol eq "\015")
                (pl-if                 (pcl:pl-and (pcl:pl-== $eol_len 1) (pcl:pl-str-eq $eol " 15"))
                  (progn
                    ;; $ctx->{eol_is_cr} = 1
                                        (pcl:pl-setf (pl-gethash-deref $ctx "eol_is_cr") 1)
                    
                  )
                  nil
                )
                
              )
              nil
            )
            
            ;; $ctx->{undef_flg} = 0
                        (pcl:pl-setf (pl-gethash-deref $ctx "undef_flg") 0)
            
            ;; if (defined $self->{undef_str}) {             $ctx->{undef_str} = $self->{undef_str};             $ctx->{undef_flg} = 3 if utf8::is_utf8($self->{undef_str});         } else {             $ctx->{undef_str} = undef;         }
            ;; if (defined $self->{"undef_str"})
            (pl-if             (pl-gethash-deref (pl-defined $self) "undef_str")
              (progn
                ;; $ctx->{undef_str} = $self->{undef_str}
                                (pcl:pl-setf (pl-gethash-deref $ctx "undef_str") (pl-gethash-deref $self "undef_str"))
                
                ;; $ctx->{undef_flg} = 3 if utf8::is_utf8($self->{undef_str})
                (pl-if                 (utf8::pl-is_utf8 (pl-gethash-deref $self "undef_str"))                 (pcl:pl-setf (pl-gethash-deref $ctx "undef_flg") 3))
                
              )
              ;; else
              (progn
                ;; $ctx->{undef_str} = undef
                                (pcl:pl-setf (pl-gethash-deref $ctx "undef_str") (pl-undef))
                
              )
            )
            
            ;; if (defined $self->{comment_str}) {             $ctx->{comment_str} = $self->{comment_str};         }
            ;; if (defined $self->{"comment_str"})
            (pl-if             (pl-gethash-deref (pl-defined $self) "comment_str")
              (progn
                ;; $ctx->{comment_str} = $self->{comment_str}
                                (pcl:pl-setf (pl-gethash-deref $ctx "comment_str") (pl-gethash-deref $self "comment_str"))
                
              )
              nil
            )
            
            ;; if (defined $self->{_types}) {             $ctx->{types} = $self->{_types};             $ctx->{types_len} = length($ctx->{types});         }
            ;; if (defined $self->{"_types"})
            (pl-if             (pl-gethash-deref (pl-defined $self) "_types")
              (progn
                ;; $ctx->{types} = $self->{_types}
                                (pcl:pl-setf (pl-gethash-deref $ctx "types") (pl-gethash-deref $self "_types"))
                
                ;; $ctx->{types_len} = length($ctx->{types})
                                (pcl:pl-setf (pl-gethash-deref $ctx "types_len") (pl-length (pl-gethash-deref $ctx "types")))
                
              )
              nil
            )
            
            ;; if (defined $self->{_is_bound}) {             $ctx->{is_bound} = $self->{_is_bound};         }
            ;; if (defined $self->{"_is_bound"})
            (pl-if             (pl-gethash-deref (pl-defined $self) "_is_bound")
              (progn
                ;; $ctx->{is_bound} = $self->{_is_bound}
                                (pcl:pl-setf (pl-gethash-deref $ctx "is_bound") (pl-gethash-deref $self "_is_bound"))
                
              )
              nil
            )
            
            ;; if (defined $self->{callbacks}) {             my $cb = $self->{callbacks};             $ctx->{has_hooks} = 0;             if (defined $cb->{after_parse} and ref $cb->{after_parse} eq 'CODE') {                 $ctx->{has_hooks} |= HOOK_AFTER_PARSE;             }             if (defined $cb->{before_print} and ref $cb->{before_print} eq 'CODE') {                 $ctx->{has_hooks} |= HOOK_BEFORE_PRINT;             }         }
            ;; if (defined $self->{"callbacks"})
            (pl-if             (pl-gethash-deref (pl-defined $self) "callbacks")
              (progn
                ;; my $cb = $self->{callbacks}
                                (pcl:pl-setf $cb (pl-gethash-deref $self "callbacks"))
                
                ;; $ctx->{has_hooks} = 0
                                (pcl:pl-setf (pl-gethash-deref $ctx "has_hooks") 0)
                
                ;; if (defined $cb->{after_parse} and ref $cb->{after_parse} eq 'CODE') {                 $ctx->{has_hooks} |= HOOK_AFTER_PARSE;             }
                ;; if (defined $cb->{"after_parse"} and ref $cb->{"after_parse"} eq 'CODE')
                (pl-if                 (pcl:pl-and (pl-gethash-deref (pl-defined $cb) "after_parse") (pcl:pl-str-eq (pl-gethash-deref (pl-ref $cb) "after_parse") "CODE"))
                  (progn
                    ;; $ctx->{has_hooks} |= HOOK_AFTER_PARSE
                                        (pcl:pl-bit-or= (pl-gethash-deref $ctx "has_hooks") (pl-HOOK_AFTER_PARSE))
                    
                  )
                  nil
                )
                
                ;; if (defined $cb->{before_print} and ref $cb->{before_print} eq 'CODE') {                 $ctx->{has_hooks} |= HOOK_BEFORE_PRINT;             }
                ;; if (defined $cb->{"before_print"} and ref $cb->{"before_print"} eq 'CODE')
                (pl-if                 (pcl:pl-and (pl-gethash-deref (pl-defined $cb) "before_print") (pcl:pl-str-eq (pl-gethash-deref (pl-ref $cb) "before_print") "CODE"))
                  (progn
                    ;; $ctx->{has_hooks} |= HOOK_BEFORE_PRINT
                                        (pcl:pl-bit-or= (pl-gethash-deref $ctx "has_hooks") (pl-HOOK_BEFORE_PRINT))
                    
                  )
                  nil
                )
                
              )
              nil
            )
            
            ;; for (qw/             binary decode_utf8 always_quote strict quote_empty             allow_loose_quotes allow_loose_escapes             allow_unquoted_escape allow_whitespace blank_is_undef             empty_is_undef verbatim auto_diag diag_verbose             keep_meta_info formula skip_empty_rows         /) {             $ctx->{$_} = defined $self->{$_} ? $self->{$_} : 0;         }
            (pl-foreach ($_ (vector "binary" "decode_utf8" "always_quote" "strict" "quote_empty" "allow_loose_quotes" "allow_loose_escapes" "allow_unquoted_escape" "allow_whitespace" "blank_is_undef" "empty_is_undef" "verbatim" "auto_diag" "diag_verbose" "keep_meta_info" "formula" "skip_empty_rows"))
              ;; $ctx->{$_} = defined $self->{$_} ? $self->{$_} : 0
                            (pcl:pl-setf (pl-gethash-deref $ctx $_) (pl-if (pl-gethash-deref (pl-defined $self) $_) (pl-gethash-deref $self $_) 0))
              
            )
            
            ;; for (qw/quote_space escape_null quote_binary/) {             $ctx->{$_} = defined $self->{$_} ? $self->{$_} : 1;         }
            (pl-foreach ($_ (vector "quote_space" "escape_null" "quote_binary"))
              ;; $ctx->{$_} = defined $self->{$_} ? $self->{$_} : 1
                            (pcl:pl-setf (pl-gethash-deref $ctx $_) (pl-if (pl-gethash-deref (pl-defined $self) $_) (pl-gethash-deref $self $_) 1))
              
            )
            
            ;; if ($ctx->{escape_char} eq "\0") {             $ctx->{escape_null} = 0;         }
            ;; if ($ctx->{"escape_char"} eq "\0")
            (pl-if             (pcl:pl-str-eq (pl-gethash-deref $ctx "escape_char") " ")
              (progn
                ;; $ctx->{escape_null} = 0
                                (pcl:pl-setf (pl-gethash-deref $ctx "escape_null") 0)
                
              )
              nil
            )
            
            ;; %{$self->{_CACHE}} = %$ctx
                        (pcl:pl-setf (pl-cast-% (pl-gethash-deref $self "_CACHE")) (pl-cast-% $ctx))
            
          )
        )
        
        ;; $ctx->{utf8} = 0
                (pcl:pl-setf (pl-gethash-deref $ctx "utf8") 0)
        
        ;; $ctx->{size} = 0
                (pcl:pl-setf (pl-gethash-deref $ctx "size") 0)
        
        ;; $ctx->{used} = 0
                (pcl:pl-setf (pl-gethash-deref $ctx "used") 0)
        
        ;; if ($ctx->{is_bound}) {         my $bound = $self->{_BOUND_COLUMNS};         if ($bound and ref $bound eq 'ARRAY') {             $ctx->{bound} = $bound;         } else {             $ctx->{is_bound} = 0;         }     }
        ;; if ($ctx->{"is_bound"})
        (pl-if         (pl-gethash-deref $ctx "is_bound")
          (progn
            ;; my $bound = $self->{_BOUND_COLUMNS}
                        (pcl:pl-setf $bound (pl-gethash-deref $self "_BOUND_COLUMNS"))
            
            ;; if ($bound and ref $bound eq 'ARRAY') {             $ctx->{bound} = $bound;         } else {             $ctx->{is_bound} = 0;         }
            ;; if ($bound and ref $bound eq 'ARRAY')
            (pl-if             (pcl:pl-and $bound (pcl:pl-str-eq (pl-ref $bound) "ARRAY"))
              (progn
                ;; $ctx->{bound} = $bound
                                (pcl:pl-setf (pl-gethash-deref $ctx "bound") $bound)
                
              )
              ;; else
              (progn
                ;; $ctx->{is_bound} = 0
                                (pcl:pl-setf (pl-gethash-deref $ctx "is_bound") 0)
                
              )
            )
            
          )
          nil
        )
        
        ;; $ctx->{eol_pos} = -1
                (pcl:pl-setf (pl-gethash-deref $ctx "eol_pos") -1)
        
        ;; $ctx->{eolx} = $ctx->{eol_len}         ? $ctx->{verbatim} || $ctx->{eol_len} >= 2             ? 1             : $ctx->{eol} =~ /\A[\015\012]/ ? 0 : 1         : 0
        ;; PARSE ERROR: Fell through. Missing case: [
        
        ;; if ($ctx->{sep_len} and $ctx->{sep_len} > 1 and _is_valid_utf8($ctx->{sep})) {         $ctx->{utf8} = 1;     }
        ;; if ($ctx->{"sep_len"} and $ctx->{"sep_len"} > 1 and _is_valid_utf8($ctx->{"sep"}))
        (pl-if         (pcl:pl-and (pcl:pl-and (pl-gethash-deref $ctx "sep_len") (pcl:pl-> (pl-gethash-deref $ctx "sep_len") 1)) (pl-_is_valid_utf8 (pl-gethash-deref $ctx "sep")))
          (progn
            ;; $ctx->{utf8} = 1
                        (pcl:pl-setf (pl-gethash-deref $ctx "utf8") 1)
            
          )
          nil
        )
        
        ;; if ($ctx->{quo_len} and $ctx->{quo_len} > 1 and _is_valid_utf8($ctx->{quo})) {         $ctx->{utf8} = 1;     }
        ;; if ($ctx->{"quo_len"} and $ctx->{"quo_len"} > 1 and _is_valid_utf8($ctx->{"quo"}))
        (pl-if         (pcl:pl-and (pcl:pl-and (pl-gethash-deref $ctx "quo_len") (pcl:pl-> (pl-gethash-deref $ctx "quo_len") 1)) (pl-_is_valid_utf8 (pl-gethash-deref $ctx "quo")))
          (progn
            ;; $ctx->{utf8} = 1
                        (pcl:pl-setf (pl-gethash-deref $ctx "utf8") 1)
            
          )
          nil
        )
        
        ;; $ctx
                $ctx
        
      )
    )
  )
)

;; sub _cache_set { ... }
(pl-sub pl-_cache_set (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($idx (make-pl-box nil)) ($value (make-pl-box nil)) ($cache (make-pl-box nil)) ($key (make-pl-box nil)) ($len (make-pl-box nil)))
        ;; my ($self, $idx, $value) = @_
                (pcl:pl-setf (vector $self $idx $value) @_)
        
        ;; return unless exists $self->{_CACHE}
        (pl-unless         (pl-gethash-deref (pl-exists $self) "_CACHE")         (pl-return))
        
        ;; my $cache = $self->{_CACHE}
                (pcl:pl-setf $cache (pl-gethash-deref $self "_CACHE"))
        
        ;; my $key = $_reverse_cache_id{$idx}
                (pcl:pl-setf $key (pl-gethash %_reverse_cache_id $idx))
        
        ;; if (!defined $key) {         warn (sprintf "Unknown cache index %d ignored\n", $idx);     } elsif ($key eq 'sep_char') {         $cache->{sep} = $value;         $cache->{sep_len} = 0;     }     elsif ($key eq 'quote_char') {         $cache->{quo} = $value;         $cache->{quo_len} = 0;     }     elsif ($key eq '_has_ahead') {         $cache->{has_ahead} = $value;     }     elsif ($key eq '_has_hooks') {         $cache->{has_hooks} = $value;     }     elsif ($key eq '_is_bound') {         $cache->{is_bound} = $value;     }     elsif ($key eq 'sep') {         use bytes;         my $len = bytes::length($value);         $cache->{sep} = $value if $len;         $cache->{sep_len} = $len == 1 ? 0 : $len;     }     elsif ($key eq 'quote') {         use bytes;         my $len = bytes::length($value);         $cache->{quo} = $value if $len;         $cache->{quo_len} = $len == 1 ? 0 : $len;     }     elsif ($key eq 'eol') {         if (defined($value)) {             $cache->{eol} = $value;             $cache->{eol_len} = length($value);         }         $cache->{eol_is_cr} = $value eq "\015" ? 1 : 0;     }     elsif ($key eq 'undef_str') {         if (defined $value) {             $cache->{undef_str} = $value;             $cache->{undef_flg} = 3 if utf8::is_utf8($value);         } else {             $cache->{undef_str} = undef;             $cache->{undef_flg} = 0;         }     }     else {         $cache->{$key} = $value;     }
        ;; if (!defined $key)
        (pl-if         (pl-! (pl-defined $key))
          (progn
            ;; warn (sprintf "Unknown cache index %d ignored\n", $idx)
                        (pl-warn (pl-sprintf "Unknown cache index %d ignored
" $idx))
            
          )
          ;; elsif ($key eq 'sep_char')
          (pl-if           (pcl:pl-str-eq $key "sep_char")
            (progn
              ;; $cache->{sep} = $value
                            (pcl:pl-setf (pl-gethash-deref $cache "sep") $value)
              
              ;; $cache->{sep_len} = 0
                            (pcl:pl-setf (pl-gethash-deref $cache "sep_len") 0)
              
            )
            ;; elsif ($key eq 'quote_char')
            (pl-if             (pcl:pl-str-eq $key "quote_char")
              (progn
                ;; $cache->{quo} = $value
                                (pcl:pl-setf (pl-gethash-deref $cache "quo") $value)
                
                ;; $cache->{quo_len} = 0
                                (pcl:pl-setf (pl-gethash-deref $cache "quo_len") 0)
                
              )
              ;; elsif ($key eq '_has_ahead')
              (pl-if               (pcl:pl-str-eq $key "_has_ahead")
                (progn
                  ;; $cache->{has_ahead} = $value
                                    (pcl:pl-setf (pl-gethash-deref $cache "has_ahead") $value)
                  
                )
                ;; elsif ($key eq '_has_hooks')
                (pl-if                 (pcl:pl-str-eq $key "_has_hooks")
                  (progn
                    ;; $cache->{has_hooks} = $value
                                        (pcl:pl-setf (pl-gethash-deref $cache "has_hooks") $value)
                    
                  )
                  ;; elsif ($key eq '_is_bound')
                  (pl-if                   (pcl:pl-str-eq $key "_is_bound")
                    (progn
                      ;; $cache->{is_bound} = $value
                                            (pcl:pl-setf (pl-gethash-deref $cache "is_bound") $value)
                      
                    )
                    ;; elsif ($key eq 'sep')
                    (pl-if                     (pcl:pl-str-eq $key "sep")
                      (progn
                        ;; use bytes
                        (eval-when (:compile-toplevel :load-toplevel :execute)
                          (pl-use "bytes"))
                        
                        ;; my $len = bytes::length($value)
                                                (pcl:pl-setf $len (bytes::pl-length $value))
                        
                        ;; $cache->{sep} = $value if $len
                        (pl-if                         $len                         (pcl:pl-setf (pl-gethash-deref $cache "sep") $value))
                        
                        ;; $cache->{sep_len} = $len == 1 ? 0 : $len
                                                (pcl:pl-setf (pl-gethash-deref $cache "sep_len") (pl-if (pcl:pl-== $len 1) 0 $len))
                        
                      )
                      ;; elsif ($key eq 'quote')
                      (pl-if                       (pcl:pl-str-eq $key "quote")
                        (progn
                          ;; use bytes
                          (eval-when (:compile-toplevel :load-toplevel :execute)
                            (pl-use "bytes"))
                          
                          ;; my $len = bytes::length($value)
                                                    (pcl:pl-setf $len (bytes::pl-length $value))
                          
                          ;; $cache->{quo} = $value if $len
                          (pl-if                           $len                           (pcl:pl-setf (pl-gethash-deref $cache "quo") $value))
                          
                          ;; $cache->{quo_len} = $len == 1 ? 0 : $len
                                                    (pcl:pl-setf (pl-gethash-deref $cache "quo_len") (pl-if (pcl:pl-== $len 1) 0 $len))
                          
                        )
                        ;; elsif ($key eq 'eol')
                        (pl-if                         (pcl:pl-str-eq $key "eol")
                          (progn
                            ;; if (defined($value)) {             $cache->{eol} = $value;             $cache->{eol_len} = length($value);         }
                            ;; if (defined($value))
                            (pl-if                             (pl-defined $value)
                              (progn
                                ;; $cache->{eol} = $value
                                                                (pcl:pl-setf (pl-gethash-deref $cache "eol") $value)
                                
                                ;; $cache->{eol_len} = length($value)
                                                                (pcl:pl-setf (pl-gethash-deref $cache "eol_len") (pl-length $value))
                                
                              )
                              nil
                            )
                            
                            ;; $cache->{eol_is_cr} = $value eq "\015" ? 1 : 0
                                                        (pcl:pl-setf (pl-gethash-deref $cache "eol_is_cr") (pl-if (pcl:pl-str-eq $value " 15") 1 0))
                            
                          )
                          ;; elsif ($key eq 'undef_str')
                          (pl-if                           (pcl:pl-str-eq $key "undef_str")
                            (progn
                              ;; if (defined $value) {             $cache->{undef_str} = $value;             $cache->{undef_flg} = 3 if utf8::is_utf8($value);         } else {             $cache->{undef_str} = undef;             $cache->{undef_flg} = 0;         }
                              ;; if (defined $value)
                              (pl-if                               (pl-defined $value)
                                (progn
                                  ;; $cache->{undef_str} = $value
                                                                    (pcl:pl-setf (pl-gethash-deref $cache "undef_str") $value)
                                  
                                  ;; $cache->{undef_flg} = 3 if utf8::is_utf8($value)
                                  (pl-if                                   (utf8::pl-is_utf8 $value)                                   (pcl:pl-setf (pl-gethash-deref $cache "undef_flg") 3))
                                  
                                )
                                ;; else
                                (progn
                                  ;; $cache->{undef_str} = undef
                                                                    (pcl:pl-setf (pl-gethash-deref $cache "undef_str") (pl-undef))
                                  
                                  ;; $cache->{undef_flg} = 0
                                                                    (pcl:pl-setf (pl-gethash-deref $cache "undef_flg") 0)
                                  
                                )
                              )
                              
                            )
                            ;; else
                            (progn
                              ;; $cache->{$key} = $value
                                                            (pcl:pl-setf (pl-gethash-deref $cache $key) $value)
                              
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
        
        ;; return 1
                (pl-return 1)
        
      )
    )
  )
)

;; sub _cache_diag { ... }
(pl-sub pl-_cache_diag (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($cache (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; unless (exists $self->{_CACHE}) {         warn ("CACHE: invalid\n");         return;     }
        ;; unless (exists $self->{"_CACHE"})
        (pl-if (pl-not         (pl-gethash-deref (pl-exists $self) "_CACHE"))
          (progn
            ;; warn ("CACHE: invalid\n")
                        (pl-warn "CACHE: invalid
")
            
            ;; return
                        (pl-return)
            
          )
          nil
        )
        
        ;; my $cache = $self->{_CACHE}
                (pcl:pl-setf $cache (pl-gethash-deref $self "_CACHE"))
        
        ;; warn ("CACHE:\n")
                (pl-warn "CACHE:
")
        
        ;; $self->__cache_show_char(quote_char => $cache->{quo})
                (pl-method-call $self '__cache_show_char "quote_char" (pl-gethash-deref $cache "quo"))
        
        ;; $self->__cache_show_char(escape_char => $cache->{escape_char})
                (pl-method-call $self '__cache_show_char "escape_char" (pl-gethash-deref $cache "escape_char"))
        
        ;; $self->__cache_show_char(sep_char => $cache->{sep})
                (pl-method-call $self '__cache_show_char "sep_char" (pl-gethash-deref $cache "sep"))
        
        ;; for (qw/         binary decode_utf8 allow_loose_escapes allow_loose_quotes allow_unquoted_escape         allow_whitespace always_quote quote_empty quote_space         escape_null quote_binary auto_diag diag_verbose formula strict skip_empty_rows         has_error_input blank_is_undef empty_is_undef has_ahead         keep_meta_info verbatim has_hooks eol_is_cr eol_len     /) {         $self->__cache_show_byte($_ => $cache->{$_});     }
        (pl-foreach ($_ (vector "binary" "decode_utf8" "allow_loose_escapes" "allow_loose_quotes" "allow_unquoted_escape" "allow_whitespace" "always_quote" "quote_empty" "quote_space" "escape_null" "quote_binary" "auto_diag" "diag_verbose" "formula" "strict" "skip_empty_rows" "has_error_input" "blank_is_undef" "empty_is_undef" "has_ahead" "keep_meta_info" "verbatim" "has_hooks" "eol_is_cr" "eol_len"))
          ;; $self->__cache_show_byte($_ => $cache->{$_})
                    (pl-method-call $self '__cache_show_byte $_ (pl-gethash-deref $cache $_))
          
        )
        
        ;; $self->__cache_show_str(eol => $cache->{eol_len}, $cache->{eol})
                (pl-method-call $self '__cache_show_str "eol" (pl-gethash-deref $cache "eol_len") (pl-gethash-deref $cache "eol"))
        
        ;; $self->__cache_show_byte(sep_len => $cache->{sep_len})
                (pl-method-call $self '__cache_show_byte "sep_len" (pl-gethash-deref $cache "sep_len"))
        
        ;; if ($cache->{sep_len} and $cache->{sep_len} > 1) {         $self->__cache_show_str(sep => $cache->{sep_len}, $cache->{sep});     }
        ;; if ($cache->{"sep_len"} and $cache->{"sep_len"} > 1)
        (pl-if         (pcl:pl-and (pl-gethash-deref $cache "sep_len") (pcl:pl-> (pl-gethash-deref $cache "sep_len") 1))
          (progn
            ;; $self->__cache_show_str(sep => $cache->{sep_len}, $cache->{sep})
                        (pl-method-call $self '__cache_show_str "sep" (pl-gethash-deref $cache "sep_len") (pl-gethash-deref $cache "sep"))
            
          )
          nil
        )
        
        ;; $self->__cache_show_byte(quo_len => $cache->{quo_len})
                (pl-method-call $self '__cache_show_byte "quo_len" (pl-gethash-deref $cache "quo_len"))
        
        ;; if ($cache->{quo_len} and $cache->{quo_len} > 1) {         $self->__cache_show_str(quote => $cache->{quo_len}, $cache->{quo});     }
        ;; if ($cache->{"quo_len"} and $cache->{"quo_len"} > 1)
        (pl-if         (pcl:pl-and (pl-gethash-deref $cache "quo_len") (pcl:pl-> (pl-gethash-deref $cache "quo_len") 1))
          (progn
            ;; $self->__cache_show_str(quote => $cache->{quo_len}, $cache->{quo})
                        (pl-method-call $self '__cache_show_str "quote" (pl-gethash-deref $cache "quo_len") (pl-gethash-deref $cache "quo"))
            
          )
          nil
        )
        
        ;; if ($cache->{types_len}) {         $self->__cache_show_str(types => $cache->{types_len}, $cache->{types});     } else {         $self->__cache_show_str(types => 0, "");     }
        ;; if ($cache->{"types_len"})
        (pl-if         (pl-gethash-deref $cache "types_len")
          (progn
            ;; $self->__cache_show_str(types => $cache->{types_len}, $cache->{types})
                        (pl-method-call $self '__cache_show_str "types" (pl-gethash-deref $cache "types_len") (pl-gethash-deref $cache "types"))
            
          )
          ;; else
          (progn
            ;; $self->__cache_show_str(types => 0, "")
                        (pl-method-call $self '__cache_show_str "types" 0 "")
            
          )
        )
        
        ;; if ($cache->{bptr}) {         $self->__cache_show_str(bptr => length($cache->{bptr}), $cache->{bptr});     }
        ;; if ($cache->{"bptr"})
        (pl-if         (pl-gethash-deref $cache "bptr")
          (progn
            ;; $self->__cache_show_str(bptr => length($cache->{bptr}), $cache->{bptr})
                        (pl-method-call $self '__cache_show_str "bptr" (pl-length (pl-gethash-deref $cache "bptr")) (pl-gethash-deref $cache "bptr"))
            
          )
          nil
        )
        
        ;; if ($cache->{tmp}) {         $self->__cache_show_str(tmp => length($cache->{tmp}), $cache->{tmp});     }
        ;; if ($cache->{"tmp"})
        (pl-if         (pl-gethash-deref $cache "tmp")
          (progn
            ;; $self->__cache_show_str(tmp => length($cache->{tmp}), $cache->{tmp})
                        (pl-method-call $self '__cache_show_str "tmp" (pl-length (pl-gethash-deref $cache "tmp")) (pl-gethash-deref $cache "tmp"))
            
          )
          nil
        )
        
      )
    )
  )
)

;; sub __cache_show_byte { ... }
(pl-sub pl-__cache_show_byte (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($key (make-pl-box nil)) ($value (make-pl-box nil)))
        ;; my ($self, $key, $value) = @_
                (pcl:pl-setf (vector $self $key $value) @_)
        
        ;; warn (sprintf "  %-21s %02x:%3d\n", $key, defined $value ? ord($value) : 0, defined $value ? $value : 0)
                (pl-warn (pl-sprintf "  %-21s %02x:%3d
" $key (pl-if (pl-defined $value) (pl-ord $value) 0) (pl-if (pl-defined $value) $value 0)))
        
      )
    )
  )
)

;; sub __cache_show_char { ... }
(pl-sub pl-__cache_show_char (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($key (make-pl-box nil)) ($value (make-pl-box nil)) ($v (make-pl-box nil)) (@b (make-pl-box nil)))
        ;; my ($self, $key, $value) = @_
                (pcl:pl-setf (vector $self $key $value) @_)
        
        ;; my $v = $value
                (pcl:pl-setf $v $value)
        
        ;; if (defined $value) {         my @b = unpack "U0C*", $value;         $v = pack "U*", $b[0];     }
        ;; if (defined $value)
        (pl-if         (pl-defined $value)
          (progn
            ;; my @b = unpack "U0C*", $value
                        (pcl:pl-setf @b (let ((*wantarray* t)) (pl-unpack "U0C*" $value)))
            
            ;; $v = pack "U*", $b[0]
                        (pcl:pl-setf $v (pl-pack "U*" (pl-aref @b 0)))
            
          )
          nil
        )
        
        ;; warn (sprintf "  %-21s %02x:%s\n", $key, defined $v ? ord($v) : 0, $self->__pretty_str($v, 1))
                (pl-warn (pl-sprintf "  %-21s %02x:%s
" $key (pl-if (pl-defined $v) (pl-ord $v) 0) (pl-method-call $self '__pretty_str $v 1)))
        
      )
    )
  )
)

;; sub __cache_show_str { ... }
(pl-sub pl-__cache_show_str (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($key (make-pl-box nil)) ($len (make-pl-box nil)) ($value (make-pl-box nil)))
        ;; my ($self, $key, $len, $value) = @_
                (pcl:pl-setf (vector $self $key $len $value) @_)
        
        ;; warn (sprintf "  %-21s %02d:%s\n", $key, $len, $self->__pretty_str($value, $len))
                (pl-warn (pl-sprintf "  %-21s %02d:%s
" $key $len (pl-method-call $self '__pretty_str $value $len)))
        
      )
    )
  )
)

;; sub __pretty_str { ... }
(pl-sub pl-__pretty_str (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($str (make-pl-box nil)) ($len (make-pl-box nil)))
        ;; my ($self, $str, $len) = @_
                (pcl:pl-setf (vector $self $str $len) @_)
        
        ;; return '' unless defined $str
        (pl-unless         (pl-defined $str)         (pl-return ""))
        
        ;; $str = substr($str, 0, $len)
                (pcl:pl-setf $str (pl-substr $str 0 $len))
        
        ;; $str =~ s/"/\\"/g
                (pcl:pl-=~ $str (pl-subst "\"" "\\\\\"" :g))
        
        ;; $str =~ s/([^\x09\x20-\x7e])/sprintf '\\x{%x}', ord($1)/eg
                (pcl:pl-=~ $str (pl-subst "([^\\x09\\x20-\\x7e])" "sprintf '\\\\x{%x}', ord($1)" :e :g))
        
        ;; qq{"$str"}
                (pl-string-concat "\"" $str "\"")
        
      )
    )
  )
)

;; sub _hook { ... }
(pl-sub pl-_hook (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($name (make-pl-box nil)) ($fields (make-pl-box nil)) ($cb (make-pl-box nil)) (@res (make-pl-box nil)))
        ;; my ($self, $name, $fields) = @_
                (pcl:pl-setf (vector $self $name $fields) @_)
        
        ;; return 0 unless $self->{callbacks}
        (pl-unless         (pl-gethash-deref $self "callbacks")         (pl-return 0))
        
        ;; my $cb = $self->{callbacks}{$name}
                (pcl:pl-setf $cb (pl-gethash (pl-gethash-deref $self "callbacks") $name))
        
        ;; return 0 unless $cb && ref $cb eq 'CODE'
        (pl-unless         (pcl:pl-&& $cb (pcl:pl-str-eq (pl-ref $cb) "CODE"))         (pl-return 0))
        
        ;; my (@res) = $cb->($self, $fields)
                (pcl:pl-setf (vector @res) (let ((*wantarray* t)) (pl-funcall-ref $cb $self $fields)))
        
        ;; if (@res) {         return 0 if ref $res[0] eq 'SCALAR' and ${$res[0]} eq "skip";     }
        ;; if (@res)
        (pl-if         @res
          (progn
            ;; return 0 if ref $res[0] eq 'SCALAR' and ${$res[0]} eq "skip"
            (pl-if             (pcl:pl-and (pcl:pl-str-eq (pl-ref (pl-aref @res 0)) "SCALAR") (pcl:pl-str-eq (pl-cast-$ (pl-aref @res 0)) "skip"))             (pl-return 0))
            
          )
          nil
        )
        
        ;; scalar @res
                (pl-scalar @res)
        
      )
    )
  )
)

;; ################################################################################
;; # methods for combine
;; ################################################################################
;; sub __combine { ... }
(pl-sub pl-__combine (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($dst (make-pl-box nil)) ($fields (make-pl-box nil)) ($useIO (make-pl-box nil)) ($ctx (make-pl-box nil)) ($binary (make-pl-box nil)) ($quot (make-pl-box nil)) ($sep (make-pl-box nil)) ($esc (make-pl-box nil)) ($quote_space (make-pl-box nil)) ($re_esc (make-pl-box nil)) ($bound (make-pl-box nil)) ($n (make-pl-box nil)) ($check_meta (make-pl-box nil)) ($must_be_quoted (make-pl-box nil)) (@results (make-pl-box nil)) ($i (make-pl-box nil)) ($v_ref (make-pl-box nil)) ($value (make-pl-box nil)))
        ;; my ($self, $dst, $fields, $useIO) = @_
                (pcl:pl-setf (vector $self $dst $fields $useIO) @_)
        
        ;; my $ctx = $self->_setup_ctx
                (pcl:pl-setf $ctx (pl-method-call $self '_setup_ctx))
        
        ;; my ($binary, $quot, $sep, $esc, $quote_space) = @{$ctx}{qw/binary quo sep escape_char quote_space/}
                (pcl:pl-setf (vector $binary $quot $sep $esc $quote_space) (pl-cast-@ (pl-gethash %ctx (vector "binary" "quo" "sep" "escape_char" "quote_space"))))
        
        ;; if(!defined $quot or $quot eq "\0"){ $quot = ''; }
        ;; if (!defined $quot or $quot eq "\0")
        (pl-if         (pcl:pl-or (pl-! (pl-defined $quot)) (pcl:pl-str-eq $quot " "))
          (progn
            ;; $quot = ''
                        (pcl:pl-setf $quot "")
            
          )
          nil
        )
        
        ;; my $re_esc (bare declaration)
        
        ;; if ($esc ne '' and $esc ne "\0") {       if ($quot ne '') {         $re_esc = $self->{_re_comb_escape}->{$quot}->{$esc} ||= qr/(\Q$quot\E|\Q$esc\E)/;       } else {         $re_esc = $self->{_re_comb_escape}->{$quot}->{$esc} ||= qr/(\Q$esc\E)/;       }     }
        ;; if ($esc ne '' and $esc ne "\0")
        (pl-if         (pcl:pl-and (pcl:pl-str-ne $esc "") (pcl:pl-str-ne $esc " "))
          (progn
            ;; if ($quot ne '') {         $re_esc = $self->{_re_comb_escape}->{$quot}->{$esc} ||= qr/(\Q$quot\E|\Q$esc\E)/;       } else {         $re_esc = $self->{_re_comb_escape}->{$quot}->{$esc} ||= qr/(\Q$esc\E)/;       }
            ;; if ($quot ne '')
            (pl-if             (pcl:pl-str-ne $quot "")
              (progn
                ;; $re_esc = $self->{_re_comb_escape}->{$quot}->{$esc} ||= qr/(\Q$quot\E|\Q$esc\E)/
                ;; PARSE ERROR: Handle single node of unknown type. Dump:
                
              )
              ;; else
              (progn
                ;; $re_esc = $self->{_re_comb_escape}->{$quot}->{$esc} ||= qr/(\Q$esc\E)/
                ;; PARSE ERROR: Handle single node of unknown type. Dump:
                
              )
            )
            
          )
          nil
        )
        
        ;; my $bound = 0
                (pcl:pl-setf $bound 0)
        
        ;; my $n = @$fields - 1
                (pcl:pl-setf $n (pcl:pl-- (pl-cast-@ $fields) 1))
        
        ;; if ($n < 0 and $ctx->{is_bound}) {         $n = $ctx->{is_bound} - 1;         $bound = 1;     }
        ;; if ($n < 0 and $ctx->{"is_bound"})
        (pl-if         (pcl:pl-and (pcl:pl-< $n 0) (pl-gethash-deref $ctx "is_bound"))
          (progn
            ;; $n = $ctx->{is_bound} - 1
                        (pcl:pl-setf $n (pcl:pl-- (pl-gethash-deref $ctx "is_bound") 1))
            
            ;; $bound = 1
                        (pcl:pl-setf $bound 1)
            
          )
          nil
        )
        
        ;; my $check_meta = ($ctx->{keep_meta_info} >= 10 and @{$self->{_FFLAGS} || []} >= $n) ? 1 : 0
                (pcl:pl-setf $check_meta (pl-if (pcl:pl-and (pcl:pl->= (pl-gethash-deref $ctx "keep_meta_info") 10) (pcl:pl->= (pl-cast-@ (pcl:pl-|| (pl-gethash-deref $self "_FFLAGS") (make-array 0 :adjustable t :fill-pointer 0))) $n)) 1 0))
        
        ;; my $must_be_quoted (bare declaration)
        
        ;; my @results (bare declaration)
        
        ;; for(my $i = 0; $i <= $n; $i++) {         my $v_ref;         if ($bound) {             $v_ref = $self->__bound_field($ctx, $i, 1);         } else {             if (@$fields > $i) {                 $v_ref = \($fields->[$i]);             }         }         next unless $v_ref;          my $value = $$v_ref;          if (!defined $value) {             if ($ctx->{undef_str}) {                 if ($ctx->{undef_flg}) {                     $ctx->{utf8} = 1;                     $ctx->{binary} = 1;                 }                 push @results, $ctx->{undef_str};             } else {                 push @results, '';             }             next;         }          if ( substr($value, 0, 1) eq '=' && $ctx->{formula} ) {             $value = $self->_formula($ctx, $value, $i);             if (!defined $value) {                 push @results, '';                 next;             }         }          $must_be_quoted = $ctx->{always_quote} ? 1 : 0;         if ($value eq '') {             $must_be_quoted++ if $ctx->{quote_empty} or ($check_meta && $self->is_quoted($i));         }         else {              if (utf8::is_utf8 $value) {                 $ctx->{utf8} = 1;                 $ctx->{binary} = 1;             }              $must_be_quoted++ if $check_meta && $self->is_quoted($i);              if (!$must_be_quoted and $quot ne '') {                 use bytes;                 $must_be_quoted++ if                     ($value =~ /\Q$quot\E/) ||                     ($sep ne '' and $sep ne "\0" and $value =~ /\Q$sep\E/) ||                     ($esc ne '' and $esc ne "\0" and $value =~ /\Q$esc\E/) ||                     ($ctx->{quote_binary} && $value =~ /[\x00-\x1f\x7f-\xa0]/) ||                     ($ctx->{quote_space} && $value =~ /[\x09\x20]/);             }              if (!$ctx->{binary} and $value =~ /[^\x09\x20-\x7E]/) {                 # an argument contained an invalid character...                 $self->{_ERROR_INPUT} = $value;                 $self->SetDiag(2110);                 return 0;             }              if ($re_esc) {                 $value =~ s/($re_esc)/$esc$1/g;             }             if ($ctx->{escape_null}) {                 $value =~ s/\0/${esc}0/g;             }         }          if ($must_be_quoted) {             $value = $quot . $value . $quot;         }         push @results, $value;     }
        (let (($i (make-pl-box nil)))
          (pl-for (        (pcl:pl-setf $i 0))
                  (        (pcl:pl-<= $i $n))
                  (        (pl-post++ $i))
            ;; my $v_ref (bare declaration)
            
            ;; if ($bound) {             $v_ref = $self->__bound_field($ctx, $i, 1);         } else {             if (@$fields > $i) {                 $v_ref = \($fields->[$i]);             }         }
            ;; if ($bound)
            (pl-if             $bound
              (progn
                ;; $v_ref = $self->__bound_field($ctx, $i, 1)
                                (pcl:pl-setf $v_ref (pl-method-call $self '__bound_field $ctx $i 1))
                
              )
              ;; else
              (progn
                ;; if (@$fields > $i) {                 $v_ref = \($fields->[$i]);             }
                ;; if (@$fields > $i)
                (pl-if                 (pcl:pl-> (pl-cast-@ $fields) $i)
                  (progn
                    ;; $v_ref = \($fields->[$i])
                                        (pcl:pl-setf $v_ref (pl-backslash (pl-aref-deref $fields $i)))
                    
                  )
                  nil
                )
                
              )
            )
            
            ;; next unless $v_ref
            (pl-unless             $v_ref             (pl-next))
            
            ;; my $value = $$v_ref
                        (pcl:pl-setf $value (pl-cast-$ $v_ref))
            
            ;; if (!defined $value) {             if ($ctx->{undef_str}) {                 if ($ctx->{undef_flg}) {                     $ctx->{utf8} = 1;                     $ctx->{binary} = 1;                 }                 push @results, $ctx->{undef_str};             } else {                 push @results, '';             }             next;         }
            ;; if (!defined $value)
            (pl-if             (pl-! (pl-defined $value))
              (progn
                ;; if ($ctx->{undef_str}) {                 if ($ctx->{undef_flg}) {                     $ctx->{utf8} = 1;                     $ctx->{binary} = 1;                 }                 push @results, $ctx->{undef_str};             } else {                 push @results, '';             }
                ;; if ($ctx->{"undef_str"})
                (pl-if                 (pl-gethash-deref $ctx "undef_str")
                  (progn
                    ;; if ($ctx->{undef_flg}) {                     $ctx->{utf8} = 1;                     $ctx->{binary} = 1;                 }
                    ;; if ($ctx->{"undef_flg"})
                    (pl-if                     (pl-gethash-deref $ctx "undef_flg")
                      (progn
                        ;; $ctx->{utf8} = 1
                                                (pcl:pl-setf (pl-gethash-deref $ctx "utf8") 1)
                        
                        ;; $ctx->{binary} = 1
                                                (pcl:pl-setf (pl-gethash-deref $ctx "binary") 1)
                        
                      )
                      nil
                    )
                    
                    ;; push @results, $ctx->{undef_str}
                                        (pl-push @results (pl-gethash-deref $ctx "undef_str"))
                    
                  )
                  ;; else
                  (progn
                    ;; push @results, ''
                                        (pl-push @results "")
                    
                  )
                )
                
                ;; next
                                (pl-next)
                
              )
              nil
            )
            
            ;; if ( substr($value, 0, 1) eq '=' && $ctx->{formula} ) {             $value = $self->_formula($ctx, $value, $i);             if (!defined $value) {                 push @results, '';                 next;             }         }
            ;; if (substr($value, 0, 1) eq '=' && $ctx->{"formula"})
            (pl-if             (pcl:pl-&& (pcl:pl-str-eq (pl-substr $value 0 1) "=") (pl-gethash-deref $ctx "formula"))
              (progn
                ;; $value = $self->_formula($ctx, $value, $i)
                                (pcl:pl-setf $value (pl-method-call $self '_formula $ctx $value $i))
                
                ;; if (!defined $value) {                 push @results, '';                 next;             }
                ;; if (!defined $value)
                (pl-if                 (pl-! (pl-defined $value))
                  (progn
                    ;; push @results, ''
                                        (pl-push @results "")
                    
                    ;; next
                                        (pl-next)
                    
                  )
                  nil
                )
                
              )
              nil
            )
            
            ;; $must_be_quoted = $ctx->{always_quote} ? 1 : 0
                        (pcl:pl-setf $must_be_quoted (pl-if (pl-gethash-deref $ctx "always_quote") 1 0))
            
            ;; if ($value eq '') {             $must_be_quoted++ if $ctx->{quote_empty} or ($check_meta && $self->is_quoted($i));         }         else {              if (utf8::is_utf8 $value) {                 $ctx->{utf8} = 1;                 $ctx->{binary} = 1;             }              $must_be_quoted++ if $check_meta && $self->is_quoted($i);              if (!$must_be_quoted and $quot ne '') {                 use bytes;                 $must_be_quoted++ if                     ($value =~ /\Q$quot\E/) ||                     ($sep ne '' and $sep ne "\0" and $value =~ /\Q$sep\E/) ||                     ($esc ne '' and $esc ne "\0" and $value =~ /\Q$esc\E/) ||                     ($ctx->{quote_binary} && $value =~ /[\x00-\x1f\x7f-\xa0]/) ||                     ($ctx->{quote_space} && $value =~ /[\x09\x20]/);             }              if (!$ctx->{binary} and $value =~ /[^\x09\x20-\x7E]/) {                 # an argument contained an invalid character...                 $self->{_ERROR_INPUT} = $value;                 $self->SetDiag(2110);                 return 0;             }              if ($re_esc) {                 $value =~ s/($re_esc)/$esc$1/g;             }             if ($ctx->{escape_null}) {                 $value =~ s/\0/${esc}0/g;             }         }
            ;; if ($value eq '')
            (pl-if             (pcl:pl-str-eq $value "")
              (progn
                ;; $must_be_quoted++ if $ctx->{quote_empty} or ($check_meta && $self->is_quoted($i))
                (pl-if                 (pcl:pl-or (pl-gethash-deref $ctx "quote_empty") (pcl:pl-&& $check_meta (pl-method-call $self 'is_quoted $i)))                 (pl-post++ $must_be_quoted))
                
              )
              ;; else
              (progn
                ;; if (utf8::is_utf8 $value) {                 $ctx->{utf8} = 1;                 $ctx->{binary} = 1;             }
                ;; if (utf8::is_utf8 $value)
                (pl-if                 (utf8::pl-is_utf8 $value)
                  (progn
                    ;; $ctx->{utf8} = 1
                                        (pcl:pl-setf (pl-gethash-deref $ctx "utf8") 1)
                    
                    ;; $ctx->{binary} = 1
                                        (pcl:pl-setf (pl-gethash-deref $ctx "binary") 1)
                    
                  )
                  nil
                )
                
                ;; $must_be_quoted++ if $check_meta && $self->is_quoted($i)
                (pl-if                 (pcl:pl-&& $check_meta (pl-method-call $self 'is_quoted $i))                 (pl-post++ $must_be_quoted))
                
                ;; if (!$must_be_quoted and $quot ne '') {                 use bytes;                 $must_be_quoted++ if                     ($value =~ /\Q$quot\E/) ||                     ($sep ne '' and $sep ne "\0" and $value =~ /\Q$sep\E/) ||                     ($esc ne '' and $esc ne "\0" and $value =~ /\Q$esc\E/) ||                     ($ctx->{quote_binary} && $value =~ /[\x00-\x1f\x7f-\xa0]/) ||                     ($ctx->{quote_space} && $value =~ /[\x09\x20]/);             }
                ;; if (!$must_be_quoted and $quot ne '')
                (pl-if                 (pcl:pl-and (pl-! $must_be_quoted) (pcl:pl-str-ne $quot ""))
                  (progn
                    ;; use bytes
                    (eval-when (:compile-toplevel :load-toplevel :execute)
                      (pl-use "bytes"))
                    
                    ;; $must_be_quoted++ if                     ($value =~ /\Q$quot\E/) ||                     ($sep ne '' and $sep ne "\0" and $value =~ /\Q$sep\E/) ||                     ($esc ne '' and $esc ne "\0" and $value =~ /\Q$esc\E/) ||                     ($ctx->{quote_binary} && $value =~ /[\x00-\x1f\x7f-\xa0]/) ||                     ($ctx->{quote_space} && $value =~ /[\x09\x20]/)
                    (pl-if ;; PARSE ERROR: Handle single node of unknown type. Dump:                     (pl-post++ $must_be_quoted))
                    
                  )
                  nil
                )
                
                ;; if (!$ctx->{binary} and $value =~ /[^\x09\x20-\x7E]/) {                 # an argument contained an invalid character...                 $self->{_ERROR_INPUT} = $value;                 $self->SetDiag(2110);                 return 0;             }
                ;; if (!$ctx->{"binary"} and $value =~ /[^\x09\x20-\x7E]/)
                (pl-if                 (pcl:pl-and (pl-! (pl-gethash-deref $ctx "binary")) (pcl:pl-=~ $value (pl-regex "/[^\\x09\\x20-\\x7E]/")))
                  (progn
                    ;; $self->{_ERROR_INPUT} = $value
                                        (pcl:pl-setf (pl-gethash-deref $self "_ERROR_INPUT") $value)
                    
                    ;; $self->SetDiag(2110)
                                        (pl-method-call $self 'SetDiag 2110)
                    
                    ;; return 0
                                        (pl-return 0)
                    
                  )
                  nil
                )
                
                ;; if ($re_esc) {                 $value =~ s/($re_esc)/$esc$1/g;             }
                ;; if ($re_esc)
                (pl-if                 $re_esc
                  (progn
                    ;; $value =~ s/($re_esc)/$esc$1/g
                                        (pcl:pl-=~ $value (pl-subst "($re_esc)" "$esc$1" :g))
                    
                  )
                  nil
                )
                
                ;; if ($ctx->{escape_null}) {                 $value =~ s/\0/${esc}0/g;             }
                ;; if ($ctx->{"escape_null"})
                (pl-if                 (pl-gethash-deref $ctx "escape_null")
                  (progn
                    ;; $value =~ s/\0/${esc}0/g
                                        (pcl:pl-=~ $value (pl-subst "\\0" "${esc}0" :g))
                    
                  )
                  nil
                )
                
              )
            )
            
            ;; if ($must_be_quoted) {             $value = $quot . $value . $quot;         }
            ;; if ($must_be_quoted)
            (pl-if             $must_be_quoted
              (progn
                ;; $value = $quot . $value . $quot
                                (pcl:pl-setf $value (pcl:pl-. (pcl:pl-. $quot $value) $quot))
                
              )
              nil
            )
            
            ;; push @results, $value
                        (pl-push @results $value)
            
          )
        )
        
        ;; $$dst = join($sep, @results) . ( defined $ctx->{eol} ? $ctx->{eol} : '' )
                (pcl:pl-setf (pl-cast-$ $dst) (pcl:pl-. (pl-join $sep @results) (pl-if (pl-gethash-deref (pl-defined $ctx) "eol") (pl-gethash-deref $ctx "eol") "")))
        
        ;; return 1
                (pl-return 1)
        
      )
    )
  )
)

;; sub _formula { ... }
(pl-sub pl-_formula (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($ctx (make-pl-box nil)) ($value (make-pl-box nil)) ($i (make-pl-box nil)) ($fa (make-pl-box nil)) ($rec (make-pl-box nil)) ($field (make-pl-box nil)) ($column_names (make-pl-box nil)) ($column_name (make-pl-box nil)))
        ;; my ($self, $ctx, $value, $i) = @_
                (pcl:pl-setf (vector $self $ctx $value $i) @_)
        
        ;; my $fa = $ctx->{formula} or return
                (pcl:pl-or (pcl:pl-setf $fa (pl-gethash-deref $ctx "formula")) (pl-return))
        
        ;; if ($fa == 1) { die "Formulas are forbidden\n" }
        ;; if ($fa == 1)
        (pl-if         (pcl:pl-== $fa 1)
          (progn
            ;; die "Formulas are forbidden\n"
                        (pl-die "Formulas are forbidden
")
            
          )
          nil
        )
        
        ;; if ($fa == 2) { die "Formulas are forbidden\n" }
        ;; if ($fa == 2)
        (pl-if         (pcl:pl-== $fa 2)
          (progn
            ;; die "Formulas are forbidden\n"
                        (pl-die "Formulas are forbidden
")
            
          )
          nil
        )
        
        ;; if ($fa == 3) {         my $rec = '';         if ($ctx->{recno}) {             $rec = sprintf " in record %lu", $ctx->{recno} + 1;         }         my $field = '';         my $column_names = $self->{_COLUMN_NAMES};         if (ref $column_names eq 'ARRAY' and @$column_names >= $i - 1) {             my $column_name = $column_names->[$i - 1];             $field = sprintf " (column: '%.100s')", $column_name if defined $column_name;         }         warn sprintf("Field %d%s%s contains formula '%s'\n", $i, $field, $rec, $value);         return $value;     }
        ;; if ($fa == 3)
        (pl-if         (pcl:pl-== $fa 3)
          (progn
            ;; my $rec = ''
                        (pcl:pl-setf $rec "")
            
            ;; if ($ctx->{recno}) {             $rec = sprintf " in record %lu", $ctx->{recno} + 1;         }
            ;; if ($ctx->{"recno"})
            (pl-if             (pl-gethash-deref $ctx "recno")
              (progn
                ;; $rec = sprintf " in record %lu", $ctx->{recno} + 1
                                (pcl:pl-setf $rec (pl-sprintf " in record %lu" (pcl:pl-+ (pl-gethash-deref $ctx "recno") 1)))
                
              )
              nil
            )
            
            ;; my $field = ''
                        (pcl:pl-setf $field "")
            
            ;; my $column_names = $self->{_COLUMN_NAMES}
                        (pcl:pl-setf $column_names (pl-gethash-deref $self "_COLUMN_NAMES"))
            
            ;; if (ref $column_names eq 'ARRAY' and @$column_names >= $i - 1) {             my $column_name = $column_names->[$i - 1];             $field = sprintf " (column: '%.100s')", $column_name if defined $column_name;         }
            ;; if (ref $column_names eq 'ARRAY' and @$column_names >= $i - 1)
            (pl-if             (pcl:pl-and (pcl:pl-str-eq (pl-ref $column_names) "ARRAY") (pcl:pl->= (pl-cast-@ $column_names) (pcl:pl-- $i 1)))
              (progn
                ;; my $column_name = $column_names->[$i - 1]
                                (pcl:pl-setf $column_name (pl-aref-deref $column_names (pcl:pl-- $i 1)))
                
                ;; $field = sprintf " (column: '%.100s')", $column_name if defined $column_name
                (pl-if                 (pl-defined $column_name)                 (pcl:pl-setf $field (pl-sprintf " (column: '%.100s')" $column_name)))
                
              )
              nil
            )
            
            ;; warn sprintf("Field %d%s%s contains formula '%s'\n", $i, $field, $rec, $value)
                        (pl-warn (pl-sprintf "Field %d%s%s contains formula '%s'
" $i $field $rec $value))
            
            ;; return $value
                        (pl-return $value)
            
          )
          nil
        )
        
        ;; if ($fa == 4) {         return '';     }
        ;; if ($fa == 4)
        (pl-if         (pcl:pl-== $fa 4)
          (progn
            ;; return ''
                        (pl-return "")
            
          )
          nil
        )
        
        ;; if ($fa == 5) {         return undef;     }
        ;; if ($fa == 5)
        (pl-if         (pcl:pl-== $fa 5)
          (progn
            ;; return undef
                        (pl-return (pl-undef))
            
          )
          nil
        )
        
        ;; if ($fa == 6) {         if (ref $self->{_FORMULA_CB} eq 'CODE') {             local $_ = $value;             return $self->{_FORMULA_CB}->();         }     }
        ;; if ($fa == 6)
        (pl-if         (pcl:pl-== $fa 6)
          (progn
            ;; if (ref $self->{_FORMULA_CB} eq 'CODE') {             local $_ = $value;             return $self->{_FORMULA_CB}->();         }
            ;; if (ref $self->{"_FORMULA_CB"} eq 'CODE')
            (pl-if             (pcl:pl-str-eq (pl-gethash-deref (pl-ref $self) "_FORMULA_CB") "CODE")
              (progn
                ;; return $self->{_FORMULA_CB}->()
                                (pl-return (pl-funcall-ref (pl-gethash-deref $self "_FORMULA_CB")))
                
              )
              nil
            )
            
          )
          nil
        )
        
        ;; return
                (pl-return)
        
      )
    )
  )
)

;; sub print { ... }
(pl-sub pl-print (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($io (make-pl-box nil)) ($fields (make-pl-box nil)) ($str (make-pl-box nil)))
        ;; my ($self, $io, $fields) = @_
                (pcl:pl-setf (vector $self $io $fields) @_)
        
        ;; require IO::Handle
        (eval-when (:compile-toplevel :load-toplevel :execute)
          (pl-require "IO::Handle"))
        
        ;; if (!defined $fields) {         $fields = [];     } elsif(ref($fields) ne 'ARRAY'){         Carp::croak("Expected fields to be an array ref");     }
        ;; if (!defined $fields)
        (pl-if         (pl-! (pl-defined $fields))
          (progn
            ;; $fields = []
                        (pcl:pl-setf $fields (make-array 0 :adjustable t :fill-pointer 0))
            
          )
          ;; elsif (ref($fields) ne 'ARRAY')
          (pl-if           (pcl:pl-str-ne (pl-ref $fields) "ARRAY")
            (progn
              ;; Carp::croak("Expected fields to be an array ref")
                            (Carp::pl-croak "Expected fields to be an array ref")
              
            )
            nil
          )
        )
        
        ;; $self->_hook(before_print => $fields)
                (pl-method-call $self '_hook "before_print" $fields)
        
        ;; my $str = ""
                (pcl:pl-setf $str "")
        
        ;; $self->__combine(\$str, $fields, 1) or return ''
                (pcl:pl-or (pl-method-call $self '__combine (pl-backslash $str) $fields 1) (pl-return ""))
        
        ;; $io->print( $str ) or $self->_set_error_diag(2200)
                (pcl:pl-or (pl-method-call $io 'print $str) (pl-method-call $self '_set_error_diag 2200))
        
      )
    )
  )
)

;; ################################################################################
;; # methods for parse
;; ################################################################################
;; sub __parse { ... }
(pl-sub pl-__parse (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($fields (make-pl-box nil)) ($fflags (make-pl-box nil)) ($src (make-pl-box nil)) ($useIO (make-pl-box nil)) ($ctx (make-pl-box nil)) ($state (make-pl-box nil)))
        ;; my ($self, $fields, $fflags, $src, $useIO) = @_
                (pcl:pl-setf (vector $self $fields $fflags $src $useIO) @_)
        
        ;; my $ctx = $self->_setup_ctx
                (pcl:pl-setf $ctx (pl-method-call $self '_setup_ctx))
        
        ;; my $state = $self->___parse($ctx, $fields, $fflags, $src, $useIO)
                (pcl:pl-setf $state (pl-method-call $self '___parse $ctx $fields $fflags $src $useIO))
        
        ;; if ($state and ($ctx->{has_hooks} || 0) & HOOK_AFTER_PARSE) {         $self->_hook(after_parse => $fields);     }
        ;; if ($state and ($ctx->{"has_hooks"} || 0) & HOOK_AFTER_PARSE)
        (pl-if         (pcl:pl-and $state (pcl:pl-bit-and (pcl:pl-|| (pl-gethash-deref $ctx "has_hooks") 0) (pl-HOOK_AFTER_PARSE)))
          (progn
            ;; $self->_hook(after_parse => $fields)
                        (pl-method-call $self '_hook "after_parse" $fields)
            
          )
          nil
        )
        
        ;; return $state || !$last_error
                (pl-return (pcl:pl-|| $state (pl-! $last_error)))
        
      )
    )
  )
)

;; sub ___parse { ... }
(pl-sub pl-___parse (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($ctx (make-pl-box nil)) ($fields (make-pl-box nil)) ($fflags (make-pl-box nil)) ($src (make-pl-box nil)) ($useIO (make-pl-box nil)) ($result (make-pl-box nil)) ($len (make-pl-box nil)) ($i (make-pl-box nil)) ($value (make-pl-box nil)) ($type (make-pl-box nil)))
        ;; my ($self, $ctx, $fields, $fflags, $src, $useIO) = @_
                (pcl:pl-setf (vector $self $ctx $fields $fflags $src $useIO) @_)
        
        ;; if ($ctx->{useIO} = $useIO) {         require IO::Handle;          $ctx->{tmp} = undef;         if ($ctx->{has_ahead} and defined $self->{_AHEAD}) {             $ctx->{tmp} = $self->{_AHEAD};             $ctx->{size} = length $ctx->{tmp};             $ctx->{used} = 0;         }     } else {         $ctx->{tmp} = $src;         $ctx->{size} = length $src;         $ctx->{used} = 0;         $ctx->{utf8} = utf8::is_utf8($src);     }
        ;; if ($ctx->{"useIO"} = $useIO)
        (pl-if         (pcl:pl-setf (pl-gethash-deref $ctx "useIO") $useIO)
          (progn
            ;; require IO::Handle
            (eval-when (:compile-toplevel :load-toplevel :execute)
              (pl-require "IO::Handle"))
            
            ;; $ctx->{tmp} = undef
                        (pcl:pl-setf (pl-gethash-deref $ctx "tmp") (pl-undef))
            
            ;; if ($ctx->{has_ahead} and defined $self->{_AHEAD}) {             $ctx->{tmp} = $self->{_AHEAD};             $ctx->{size} = length $ctx->{tmp};             $ctx->{used} = 0;         }
            ;; if ($ctx->{"has_ahead"} and defined $self->{"_AHEAD"})
            (pl-if             (pcl:pl-and (pl-gethash-deref $ctx "has_ahead") (pl-gethash-deref (pl-defined $self) "_AHEAD"))
              (progn
                ;; $ctx->{tmp} = $self->{_AHEAD}
                                (pcl:pl-setf (pl-gethash-deref $ctx "tmp") (pl-gethash-deref $self "_AHEAD"))
                
                ;; $ctx->{size} = length $ctx->{tmp}
                                (pcl:pl-setf (pl-gethash-deref $ctx "size") (pl-length (pl-gethash-deref $ctx "tmp")))
                
                ;; $ctx->{used} = 0
                                (pcl:pl-setf (pl-gethash-deref $ctx "used") 0)
                
              )
              nil
            )
            
          )
          ;; else
          (progn
            ;; $ctx->{tmp} = $src
                        (pcl:pl-setf (pl-gethash-deref $ctx "tmp") $src)
            
            ;; $ctx->{size} = length $src
                        (pcl:pl-setf (pl-gethash-deref $ctx "size") (pl-length $src))
            
            ;; $ctx->{used} = 0
                        (pcl:pl-setf (pl-gethash-deref $ctx "used") 0)
            
            ;; $ctx->{utf8} = utf8::is_utf8($src)
                        (pcl:pl-setf (pl-gethash-deref $ctx "utf8") (utf8::pl-is_utf8 $src))
            
          )
        )
        
        ;; if ($ctx->{has_error_input}) {         $self->{_ERROR_INPUT} = undef;         $ctx->{has_error_input} = 0;     }
        ;; if ($ctx->{"has_error_input"})
        (pl-if         (pl-gethash-deref $ctx "has_error_input")
          (progn
            ;; $self->{_ERROR_INPUT} = undef
                        (pcl:pl-setf (pl-gethash-deref $self "_ERROR_INPUT") (pl-undef))
            
            ;; $ctx->{has_error_input} = 0
                        (pcl:pl-setf (pl-gethash-deref $ctx "has_error_input") 0)
            
          )
          nil
        )
        
        ;; my $result = $self->____parse($ctx, $src, $fields, $fflags)
                (pcl:pl-setf $result (pl-method-call $self '____parse $ctx $src $fields $fflags))
        
        ;; $self->{_RECNO} = ++($ctx->{recno})
                (pcl:pl-setf (pl-gethash-deref $self "_RECNO") (pl-pre++ (pl-gethash-deref $ctx "recno")))
        
        ;; $self->{_EOF} = ''
                (pcl:pl-setf (pl-gethash-deref $self "_EOF") "")
        
        ;; if ($ctx->{strict}) {         $ctx->{strict_n} ||= $ctx->{fld_idx};         if ($ctx->{strict_n} != $ctx->{fld_idx}) {             unless ($ctx->{useIO} & useIO_EOF) {                 $self->__parse_error($ctx, 2014, $ctx->{used});             }             if ($last_error) {                 $result = undef;             }         }     }
        ;; if ($ctx->{"strict"})
        (pl-if         (pl-gethash-deref $ctx "strict")
          (progn
            ;; $ctx->{strict_n} ||= $ctx->{fld_idx}
                        (pcl:pl-or-assign (pl-gethash-deref $ctx "strict_n") (pl-gethash-deref $ctx "fld_idx"))
            
            ;; if ($ctx->{strict_n} != $ctx->{fld_idx}) {             unless ($ctx->{useIO} & useIO_EOF) {                 $self->__parse_error($ctx, 2014, $ctx->{used});             }             if ($last_error) {                 $result = undef;             }         }
            ;; if ($ctx->{"strict_n"} != $ctx->{"fld_idx"})
            (pl-if             (pcl:pl-!= (pl-gethash-deref $ctx "strict_n") (pl-gethash-deref $ctx "fld_idx"))
              (progn
                ;; unless ($ctx->{useIO} & useIO_EOF) {                 $self->__parse_error($ctx, 2014, $ctx->{used});             }
                ;; unless ($ctx->{"useIO"} & useIO_EOF)
                (pl-if (pl-not                 (pcl:pl-bit-and (pl-gethash-deref $ctx "useIO") (pl-useIO_EOF)))
                  (progn
                    ;; $self->__parse_error($ctx, 2014, $ctx->{used})
                                        (pl-method-call $self '__parse_error $ctx 2014 (pl-gethash-deref $ctx "used"))
                    
                  )
                  nil
                )
                
                ;; if ($last_error) {                 $result = undef;             }
                ;; if ($last_error)
                (pl-if                 $last_error
                  (progn
                    ;; $result = undef
                                        (pcl:pl-setf $result (pl-undef))
                    
                  )
                  nil
                )
                
              )
              nil
            )
            
          )
          nil
        )
        
        ;; if ($ctx->{useIO}) {         if (defined $ctx->{tmp} and $ctx->{used} < $ctx->{size} and $ctx->{has_ahead}) {             $self->{_AHEAD} = substr($ctx->{tmp}, $ctx->{used}, $ctx->{size} - $ctx->{used});         } else {             $ctx->{has_ahead} = 0;             if ($ctx->{useIO} & useIO_EOF) {                 $self->{_EOF} = 1;             }         }         %{$self->{_CACHE}} = %$ctx;          if ($fflags) {             if ($ctx->{keep_meta_info}) {                 $self->{_FFLAGS} = $fflags;             } else {                 undef $fflags;             }         }     } else {         %{$self->{_CACHE}} = %$ctx;     }
        ;; if ($ctx->{"useIO"})
        (pl-if         (pl-gethash-deref $ctx "useIO")
          (progn
            ;; if (defined $ctx->{tmp} and $ctx->{used} < $ctx->{size} and $ctx->{has_ahead}) {             $self->{_AHEAD} = substr($ctx->{tmp}, $ctx->{used}, $ctx->{size} - $ctx->{used});         } else {             $ctx->{has_ahead} = 0;             if ($ctx->{useIO} & useIO_EOF) {                 $self->{_EOF} = 1;             }         }
            ;; if (defined $ctx->{"tmp"} and $ctx->{"used"} < $ctx->{"size"} and $ctx->{"has_ahead"})
            (pl-if             (pcl:pl-and (pcl:pl-and (pl-gethash-deref (pl-defined $ctx) "tmp") (pcl:pl-< (pl-gethash-deref $ctx "used") (pl-gethash-deref $ctx "size"))) (pl-gethash-deref $ctx "has_ahead"))
              (progn
                ;; $self->{_AHEAD} = substr($ctx->{tmp}, $ctx->{used}, $ctx->{size} - $ctx->{used})
                                (pcl:pl-setf (pl-gethash-deref $self "_AHEAD") (pl-substr (pl-gethash-deref $ctx "tmp") (pl-gethash-deref $ctx "used") (pcl:pl-- (pl-gethash-deref $ctx "size") (pl-gethash-deref $ctx "used"))))
                
              )
              ;; else
              (progn
                ;; $ctx->{has_ahead} = 0
                                (pcl:pl-setf (pl-gethash-deref $ctx "has_ahead") 0)
                
                ;; if ($ctx->{useIO} & useIO_EOF) {                 $self->{_EOF} = 1;             }
                ;; if ($ctx->{"useIO"} & useIO_EOF)
                (pl-if                 (pcl:pl-bit-and (pl-gethash-deref $ctx "useIO") (pl-useIO_EOF))
                  (progn
                    ;; $self->{_EOF} = 1
                                        (pcl:pl-setf (pl-gethash-deref $self "_EOF") 1)
                    
                  )
                  nil
                )
                
              )
            )
            
            ;; %{$self->{_CACHE}} = %$ctx
                        (pcl:pl-setf (pl-cast-% (pl-gethash-deref $self "_CACHE")) (pl-cast-% $ctx))
            
            ;; if ($fflags) {             if ($ctx->{keep_meta_info}) {                 $self->{_FFLAGS} = $fflags;             } else {                 undef $fflags;             }         }
            ;; if ($fflags)
            (pl-if             $fflags
              (progn
                ;; if ($ctx->{keep_meta_info}) {                 $self->{_FFLAGS} = $fflags;             } else {                 undef $fflags;             }
                ;; if ($ctx->{"keep_meta_info"})
                (pl-if                 (pl-gethash-deref $ctx "keep_meta_info")
                  (progn
                    ;; $self->{_FFLAGS} = $fflags
                                        (pcl:pl-setf (pl-gethash-deref $self "_FFLAGS") $fflags)
                    
                  )
                  ;; else
                  (progn
                    ;; undef $fflags
                    ;; PARSE ERROR: Fell through. Missing case: [
                    
                  )
                )
                
              )
              nil
            )
            
          )
          ;; else
          (progn
            ;; %{$self->{_CACHE}} = %$ctx
                        (pcl:pl-setf (pl-cast-% (pl-gethash-deref $self "_CACHE")) (pl-cast-% $ctx))
            
          )
        )
        
        ;; if ($result and $ctx->{types}) {         my $len = @$fields;         for(my $i = 0; $i <= $len && $i <= $ctx->{types_len}; $i++) {             my $value = $fields->[$i];             next unless defined $value;             my $type = ord(substr($ctx->{types}, $i, 1));             if ($type == IV) {                 $fields->[$i] = int($value);             } elsif ($type == NV) {                 $fields->[$i] = $value + 0.0;             }         }     }
        ;; if ($result and $ctx->{"types"})
        (pl-if         (pcl:pl-and $result (pl-gethash-deref $ctx "types"))
          (progn
            ;; my $len = @$fields
                        (pcl:pl-setf $len (pl-cast-@ $fields))
            
            ;; for(my $i = 0; $i <= $len && $i <= $ctx->{types_len}; $i++) {             my $value = $fields->[$i];             next unless defined $value;             my $type = ord(substr($ctx->{types}, $i, 1));             if ($type == IV) {                 $fields->[$i] = int($value);             } elsif ($type == NV) {                 $fields->[$i] = $value + 0.0;             }         }
            (let (($i (make-pl-box nil)))
              (pl-for (            (pcl:pl-setf $i 0))
                      (            (pcl:pl-&& (pcl:pl-<= $i $len) (pcl:pl-<= $i (pl-gethash-deref $ctx "types_len"))))
                      (            (pl-post++ $i))
                ;; my $value = $fields->[$i]
                                (pcl:pl-setf $value (pl-aref-deref $fields $i))
                
                ;; next unless defined $value
                (pl-unless                 (pl-defined $value)                 (pl-next))
                
                ;; my $type = ord(substr($ctx->{types}, $i, 1))
                                (pcl:pl-setf $type (pl-ord (pl-substr (pl-gethash-deref $ctx "types") $i 1)))
                
                ;; if ($type == IV) {                 $fields->[$i] = int($value);             } elsif ($type == NV) {                 $fields->[$i] = $value + 0.0;             }
                ;; if ($type == IV)
                (pl-if                 (pcl:pl-== $type (pl-IV))
                  (progn
                    ;; $fields->[$i] = int($value)
                                        (pcl:pl-setf (pl-aref-deref $fields $i) (pl-int $value))
                    
                  )
                  ;; elsif ($type == NV)
                  (pl-if                   (pcl:pl-== $type (pl-NV))
                    (progn
                      ;; $fields->[$i] = $value + 0.0
                                            (pcl:pl-setf (pl-aref-deref $fields $i) (pcl:pl-+ $value 0.0))
                      
                    )
                    nil
                  )
                )
                
              )
            )
            
          )
          nil
        )
        
        ;; $result
                $result
        
      )
    )
  )
)

;; sub ____parse { ... }
(pl-sub pl-____parse (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($ctx (make-pl-box nil)) ($src (make-pl-box nil)) ($fields (make-pl-box nil)) ($fflags (make-pl-box nil)) ($quot (make-pl-box nil)) ($sep (make-pl-box nil)) ($esc (make-pl-box nil)) ($eol (make-pl-box nil)) ($seenSomething (make-pl-box nil)) ($spl (make-pl-box nil)) ($waitingForField (make-pl-box nil)) ($value (make-pl-box nil)) ($v_ref (make-pl-box nil)) ($fnum (make-pl-box nil)) ($re_str (make-pl-box nil)) ($re (make-pl-box nil)) ($hit (make-pl-box nil)) ($c (make-pl-box nil)) ($quoesc (make-pl-box nil)) ($c2 (make-pl-box nil)) ($c3 (make-pl-box nil)) ($ser (make-pl-box nil)) ($cb (make-pl-box nil)) ($rv (make-pl-box nil)) ($n (make-pl-box nil)) ($i (make-pl-box nil)) ($rvi (make-pl-box nil)) ($len (make-pl-box nil)))
        ;; my ($self, $ctx, $src, $fields, $fflags) = @_
                (pcl:pl-setf (vector $self $ctx $src $fields $fflags) @_)
        
        ;; my ($quot, $sep, $esc, $eol) = @{$ctx}{qw/quo sep escape_char eol/}
                (pcl:pl-setf (vector $quot $sep $esc $eol) (pl-cast-@ (pl-gethash %ctx (vector "quo" "sep" "escape_char" "eol"))))
        
        ;; utf8::encode($sep)  if !$ctx->{utf8} and $ctx->{sep_len}
        (pl-if         (pcl:pl-and (pl-! (pl-gethash-deref $ctx "utf8")) (pl-gethash-deref $ctx "sep_len"))         (utf8::pl-encode $sep))
        
        ;; utf8::encode($quot) if !$ctx->{utf8} and $ctx->{quo_len}
        (pl-if         (pcl:pl-and (pl-! (pl-gethash-deref $ctx "utf8")) (pl-gethash-deref $ctx "quo_len"))         (utf8::pl-encode $quot))
        
        ;; utf8::encode($eol)  if !$ctx->{utf8} and $ctx->{eol_len}
        (pl-if         (pcl:pl-and (pl-! (pl-gethash-deref $ctx "utf8")) (pl-gethash-deref $ctx "eol_len"))         (utf8::pl-encode $eol))
        
        ;; my $seenSomething =  0
                (pcl:pl-setf $seenSomething 0)
        
        ;; my $spl = -1
                (pcl:pl-setf $spl -1)
        
        ;; my $waitingForField = 1
                (pcl:pl-setf $waitingForField 1)
        
        ;; my ($value, $v_ref) (bare declaration)
        
        ;; $ctx->{fld_idx} = my $fnum = 0
                (pcl:pl-setf (pl-gethash-deref $ctx "fld_idx") (pcl:pl-setf $fnum 0))
        
        ;; $ctx->{flag} = 0
                (pcl:pl-setf (pl-gethash-deref $ctx "flag") 0)
        
        (defun --anon-block-97-- ($a $b)
          ;; length $b <=> length $a
                    (pl-length (pcl:pl-<=> $b (pl-length $a)))
          
        )
        
        (defun --anon-block-98-- ($_)
          ;; defined $_ and $_ ne ''
                    (pcl:pl-and (pl-defined $_) (pcl:pl-str-ne $_ ""))
          
        )
        
        ;; my $re_str = join '|', map({$_ eq "\0" ? '[\\0]' : quotemeta($_)} sort {length $b <=> length $a} grep {defined $_ and $_ ne ''} $sep, $quot, $esc, $eol), "\015", "\012", "\x09", " "
        ;; PARSE ERROR: Fell through. Missing case: [
        
        ;; $ctx->{_re} = qr/$re_str/
        ;; PARSE ERROR: Handle single node of unknown type. Dump:
        
        ;; my $re = qr/$re_str|[^\x09\x20-\x7E]|$/
        ;; PARSE ERROR: Handle single node of unknown type. Dump:
        
        ;; LOOP:     while($self->__get_from_src($ctx, $src)) {         while($ctx->{tmp} =~ /\G(.*?)($re)/gs) {             my ($hit, $c) = ($1, $2);             $ctx->{used} = pos($ctx->{tmp});             if (!$waitingForField and $c eq '' and $hit ne '' and $ctx->{useIO} and !($ctx->{useIO} & useIO_EOF)) {                 $self->{_AHEAD} = $hit;                 $ctx->{has_ahead} = 1;                 $ctx->{has_leftover} = 1;                 last;             }             last if $seenSomething and $hit eq '' and $c eq ''; # EOF              # new field             if (!$v_ref) {                 if ($ctx->{is_bound}) {                     $v_ref = $self->__bound_field($ctx, $fnum, 0);                 } else {                     $value = '';                     $v_ref = \$value;                 }                 $fnum++;                 return unless $v_ref;                 $ctx->{flag} = 0;                     $ctx->{fld_idx}++;             }              $seenSomething = 1;             $spl++;              if (defined $hit and $hit ne '') {                 if ($waitingForField) {                     if (!$spl && $ctx->{comment_str} && $ctx->{tmp} =~ /\A\Q$ctx->{comment_str}/) {                         $ctx->{used} = $ctx->{size};                         $ctx->{fld_idx} = 0;                         $seenSomething = 0;                         next LOOP;                     }                     $waitingForField = 0;                 }                 if ($hit =~ /[^\x09\x20-\x7E]/) {                     $ctx->{flag} |= IS_BINARY;                 }                 $$v_ref .= $hit;             }  RESTART:             if (defined $c and defined $sep and $c eq $sep) {                 if ($waitingForField) {                     # ,1,"foo, 3",,bar,                     # ^           ^                     if ($ctx->{blank_is_undef} or $ctx->{empty_is_undef}) {                         $$v_ref = undef;                     } else {                         $$v_ref = "";                     }                     unless ($ctx->{is_bound}) {                         push @$fields, $$v_ref;                     }                     $v_ref = undef;                     if ($ctx->{keep_meta_info} and $fflags) {                         push @$fflags, $ctx->{flag};                     }                 } elsif ($ctx->{flag} & IS_QUOTED) {                     # ,1,"foo, 3",,bar,                     #        ^                     $$v_ref .= $c;                 } else {                     # ,1,"foo, 3",,bar,                     #   ^        ^    ^                     $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                     $v_ref = undef;                     $waitingForField = 1;                 }             }             elsif (defined $c and defined $quot and $quot ne "\0" and $c eq $quot) {                 if ($waitingForField) {                     # ,1,"foo, 3",,bar,\r\n                     #    ^                     $ctx->{flag} |= IS_QUOTED;                     $waitingForField = 0;                     next;                 }                 if ($ctx->{flag} & IS_QUOTED) {                     # ,1,"foo, 3",,bar,\r\n                     #           ^                     my $quoesc = 0;                     my $c2 = $self->__get($ctx, $src);                      if ($ctx->{allow_whitespace}) {                         # , 1 , "foo, 3" , , bar , \r\n                         #               ^                         while($self->__is_whitespace($ctx, $c2)) {                             if ($ctx->{allow_loose_quotes} and !(defined $esc and $c2 eq $esc)) {                                 $$v_ref .= $c;                                 $c = $c2;                             }                             $c2 = $self->__get($ctx, $src);                         }                     }                      if (!defined $c2) { # EOF                         # ,1,"foo, 3"                         #            ^                         $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                         return 1;                     }                      if (defined $c2 and defined $sep and $c2 eq $sep) {                         # ,1,"foo, 3",,bar,\r\n                         #            ^                         $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                         $v_ref = undef;                         $waitingForField = 1;                         next;                     }                     if (defined $c2 and ($c2 eq "\012" or (defined $eol and $c2 eq $eol))) { # FIXME: EOLX                         # ,1,"foo, 3",,"bar"\n                         #                   ^                         $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                         return 1;                     }                      if (defined $esc and $c eq $esc) {                         $quoesc = 1;                         if (defined $c2 and $c2 eq '0') {                             # ,1,"foo, 3"056",,bar,\r\n                             #            ^                             $$v_ref .= "\0";                             next;                         }                         if (defined $c2 and defined $quot and $c2 eq $quot) {                             # ,1,"foo, 3""56",,bar,\r\n                             #            ^                             if ($ctx->{utf8}) {                                 $ctx->{flag} |= IS_BINARY;                             }                             $$v_ref .= $c2;                             next;                         }                         if ($ctx->{allow_loose_escapes} and defined $c2 and $c2 ne "\015") {                             # ,1,"foo, 3"56",,bar,\r\n                             #            ^                             $$v_ref .= $c;                             $c = $c2;                             goto RESTART;                         }                     }                     if (defined $c2 and $c2 eq "\015") {                         if ($ctx->{eol_is_cr}) {                             # ,1,"foo, 3"\r                             #            ^                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                          my $c3 = $self->__get($ctx, $src);                         if (defined $c3 and $c3 eq "\012") {                             # ,1,"foo, 3"\r\n                             #              ^                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                          if ($ctx->{useIO} and !$ctx->{eol_len}) {                             if ($c3 eq "\015") { # \r followed by an empty line                                 # ,1,"foo, 3"\r\r                                 #              ^                                 $self->__set_eol_is_cr($ctx);                                 goto EOLX;                             }                             if ($c3 !~ /[^\x09\x20-\x7E]/) {                                 # ,1,"foo\n 3",,"bar"\r                                 # baz,4                                 # ^                                 $self->__set_eol_is_cr($ctx);                                 $ctx->{used}--;                                 $ctx->{has_ahead} = 1;                                 $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                                 return 1;                             }                         }                          $self->__parse_error($ctx, $quoesc ? 2023 : 2010, $ctx->{used} - 2);                         return;                     }                      if ($ctx->{allow_loose_quotes} and !$quoesc) {                         # ,1,"foo, 3"456",,bar,\r\n                         #            ^                         $$v_ref .= $c;                         $c = $c2;                         goto RESTART;                     }                     # 1,"foo" ",3                     #        ^                     if ($quoesc) {                         $ctx->{used}--;                         $self->__error_inside_quotes($ctx, 2023);                         return;                     }                     $self->__error_inside_quotes($ctx, 2011);                     return;                 }                 # !waitingForField, !InsideQuotes                 if ($ctx->{allow_loose_quotes}) { # 1,foo "boo" d'uh,1                     $ctx->{flag} |= IS_ERROR;                     $$v_ref .= $c;                 } else {                     $self->__error_inside_field($ctx, 2034);                     return;                 }             }             elsif (defined $c and defined $esc and $esc ne "\0" and $c eq $esc) {                 # This means quote_char != escape_char                 if ($waitingForField) {                     $waitingForField = 0;                     if ($ctx->{allow_unquoted_escape}) {                         # The escape character is the first character of an                         # unquoted field                         # ... get and store next character                         my $c2 = $self->__get($ctx, $src);                         $$v_ref = "";                          if (!defined $c2) { # EOF                             $ctx->{used}--;                             $self->__error_inside_field($ctx, 2035);                             return;                         }                         if ($c2 eq '0') {                             $$v_ref .= "\0";                         }                         elsif (                             (defined $quot and $c2 eq $quot) or                             (defined $sep and $c2 eq $sep) or                             (defined $esc and $c2 eq $esc) or                             $ctx->{allow_loose_escapes}                         ) {                             if ($ctx->{utf8}) {                                 $ctx->{flag} |= IS_BINARY;                             }                             $$v_ref .= $c2;                         } else {                             $self->__parse_inside_quotes($ctx, 2025);                             return;                         }                     }                 }                 elsif ($ctx->{flag} & IS_QUOTED) {                     my $c2 = $self->__get($ctx, $src);                     if (!defined $c2) { # EOF                         $ctx->{used}--;                         $self->__error_inside_quotes($ctx, 2024);                         return;                     }                     if ($c2 eq '0') {                         $$v_ref .= "\0";                     }                     elsif (                         (defined $quot and $c2 eq $quot) or                         (defined $sep and $c2 eq $sep) or                         (defined $esc and $c2 eq $esc) or                         $ctx->{allow_loose_escapes}                     ) {                         if ($ctx->{utf8}) {                             $ctx->{flag} |= IS_BINARY;                         }                         $$v_ref .= $c2;                     } else {                         $ctx->{used}--;                         $self->__error_inside_quotes($ctx, 2025);                         return;                     }                 }                 elsif ($v_ref) {                     my $c2 = $self->__get($ctx, $src);                     if (!defined $c2) { # EOF                         $ctx->{used}--;                         $self->__error_inside_field($ctx, 2035);                         return;                     }                     $$v_ref .= $c2;                 }                 else {                     $self->__error_inside_field($ctx, 2036);                     return;                 }             }             elsif (defined $c and ($c eq "\012" or $c eq '' or (defined $eol and $c eq $eol and $eol ne "\015"))) { # EOL     EOLX:                 if ($fnum == 1 && $ctx->{flag} == 0 && (!$v_ref || $$v_ref eq '') && $ctx->{skip_empty_rows}) {                     ### SkipEmptyRow                     my $ser = $ctx->{skip_empty_rows};                     if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }                     if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }                     if ($ser == 5) { $self->SetDiag(2015); return undef; }                      if ($ser <= 2) { # skip & eof                         $ctx->{fld_idx} = 0;                         $c = $self->__get($ctx, $src);                         if (!defined $c or $ser == 2) {  # EOF                             $v_ref = undef;                             $seenSomething = 0;                             if ($ser == 2) { return undef; }                             last LOOP;                         }                     }                      if ($ser == 6) {                         my $cb = $self->{_EMPTROW_CB};                         unless ($cb && ref $cb eq 'CODE') {                             return undef;  # A callback is wanted, but none found                         }                         local $_ = $v_ref;                         my $rv = $cb->();                         # Result should be a ref to a list.                         unless (ref $rv eq 'ARRAY') {                             return undef;                         }                         my $n = @$rv;                         if ($n <= 0) {                             return 1;                         }                         if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                             $n = $ctx->{is_bound} - 1;                         }                         for (my $i = 0; $i < $n; $i++) {                             my $rvi = $rv->[$i];                             $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                         }                         return 1;                     }                     goto RESTART;                 }                  if ($waitingForField) {                     # ,1,"foo, 3",,bar,                     #                  ^                     if ($ctx->{blank_is_undef} or $ctx->{empty_is_undef}) {                         $$v_ref = undef;                     } else {                         $$v_ref = "";                     }                     unless ($ctx->{is_bound}) {                         push @$fields, $$v_ref;                     }                     if ($ctx->{keep_meta_info} and $fflags) {                         push @$fflags, $ctx->{flag};                     }                     return 1;                 }                 if ($ctx->{flag} & IS_QUOTED) {                     # ,1,"foo\n 3",,bar,                     #        ^                     $ctx->{flag} |= IS_BINARY;                     unless ($ctx->{binary}) {                         $self->__error_inside_quotes($ctx, 2021);                         return;                     }                     $$v_ref .= $c;                 }                 elsif ($ctx->{verbatim}) {                     # ,1,foo\n 3,,bar,                     # This feature should be deprecated                     $ctx->{flag} |= IS_BINARY;                     unless ($ctx->{binary}) {                         $self->__error_inside_field($ctx, 2030);                         return;                     }                     $$v_ref .= $c unless $ctx->{eol} eq $c and $ctx->{useIO};                 }                 else {                     # sep=,                     #      ^                     if (!$ctx->{recno} and $ctx->{fld_idx} == 1 and $ctx->{useIO} and $hit =~ /^sep=(.{1,16})$/i) {                         $ctx->{sep} = $1;                         use bytes;                         my $len = length $ctx->{sep};                         if ($len <= 16) {                             $ctx->{sep_len} = $len == 1 ? 0 : $len;                             return $self->____parse($ctx, $src, $fields, $fflags);                         }                     }                      # ,1,"foo\n 3",,bar                     #                  ^                     $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                     return 1;                 }             }             elsif (defined $c and $c eq "\015" and !$ctx->{verbatim}) {                 if ($waitingForField) {                     if ($ctx->{eol_is_cr}) {                         # ,1,"foo\n 3",,bar,\r                         #                   ^                         $c = "\012";                         goto EOLX;                     }                      my $c2 = $self->__get($ctx, $src);                     if (!defined $c2) { # EOF                         # ,1,"foo\n 3",,bar,\r                         #                     ^                         $c = undef;                         last unless $seenSomething;                         goto RESTART;                     }                     if ($c2 eq "\012") { # \r is not optional before EOLX!                         # ,1,"foo\n 3",,bar,\r\n                         #                     ^                         $c = $c2;                         goto EOLX;                     }                      if ($ctx->{useIO} and !$ctx->{eol_len}) {                         if ($c2 eq "\012") { # \r followed by an empty line                             # ,1,"foo\n 3",,bar,\r\r                             #                     ^                             $self->__set_eol_is_cr($ctx);                             goto EOLX;                         }                         $waitingForField = 0;                         if ($c2 !~ /[^\x09\x20-\x7E]/) {                             # ,1,"foo\n 3",,bar,\r                             # baz,4                             # ^                             $self->__set_eol_is_cr($ctx);                             $ctx->{used}--;                             $ctx->{has_ahead} = 1;                             if ($fnum == 1 && $ctx->{flag} == 0 && (!$v_ref or $$v_ref eq '') && $ctx->{skip_empty_rows}) {                                 ### SkipEmptyRow                                 my $ser = $ctx->{skip_empty_rows};                                 if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 5) { $self->SetDiag(2015); return undef; }                                  if ($ser <= 2) { # skip & eof                                     $ctx->{fld_idx} = 0;                                     $c = $self->__get($ctx, $src);                                     if (!defined $c) { # EOF                                         $v_ref = undef;                                         $waitingForField = 1;                                         $seenSomething = 0;                                         last LOOP;                                     }                                 }                                  if ($ser == 6) {                                     my $cb = $self->{_EMPTROW_CB};                                     unless ($cb && ref $cb eq 'CODE') {                                         return undef;  # A callback is wanted, but none found                                     }                                     local $_ = $v_ref;                                     my $rv = $cb->();                                     # Result should be a ref to a list.                                     unless (ref $rv eq 'ARRAY') {                                         return undef;                                     }                                     my $n = @$rv;                                     if ($n <= 0) {                                         return 1;                                     }                                     if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                                         $n = $ctx->{is_bound} - 1;                                     }                                     for (my $i = 0; $i < $n; $i++) {                                         my $rvi = $rv->[$i];                                         $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                                     }                                     return 1;                                 }                                  $$v_ref = $c2;                                 goto RESTART;                             }                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                     }                      # ,1,"foo\n 3",,bar,\r\t                     #                     ^                     $ctx->{used}--;                     $self->__error_inside_field($ctx, 2031);                     return;                 }                 if ($ctx->{flag} & IS_QUOTED) {                     # ,1,"foo\r 3",,bar,\r\t                     #        ^                     $ctx->{flag} |= IS_BINARY;                     unless ($ctx->{binary}) {                         $self->__error_inside_quotes($ctx, 2022);                         return;                     }                     $$v_ref .= $c;                 }                 else {                     if ($ctx->{eol_is_cr}) {                         # ,1,"foo\n 3",,bar\r                         #                  ^                         goto EOLX;                     }                      my $c2 = $self->__get($ctx, $src);                     if (defined $c2 and $c2 eq "\012") { # \r is not optional before EOLX!                         # ,1,"foo\n 3",,bar\r\n                         #                    ^                         goto EOLX;                     }                      if ($ctx->{useIO} and !$ctx->{eol_len}) {                         if ($c2 !~ /[^\x09\x20-\x7E]/                             # ,1,"foo\n 3",,bar\r                             # baz,4                             # ^                             or $c2 eq "\015"                             # ,1,"foo\n 3",,bar,\r\r                             #                     ^                         ) {                             $self->__set_eol_is_cr($ctx);                             $ctx->{used}--;                             $ctx->{has_ahead} = 1;                             if ($fnum == 1 && $ctx->{flag} == 0 && (!$v_ref or $$v_ref eq '') && $ctx->{skip_empty_rows}) {                                 ### SKipEmptyRow                                 my $ser = $ctx->{skip_empty_rows};                                 if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 5) { $self->SetDiag(2015); return undef; }                                  if ($ser <= 2) { # skip & eof                                     $ctx->{fld_idx} = 0;                                     $c = $self->__get($ctx, $src);                                     if (!defined $c) { # EOL                                         $v_ref = undef;                                         $seenSomething = 0;                                         last LOOP;                                     }                                 }                                  if ($ser == 6) {                                     my $cb = $self->{_EMPTROW_CB};                                     unless ($cb && ref $cb eq 'CODE') {                                         return undef;  # A callback is wanted, but none found                                     }                                     local $_ = $v_ref;                                     my $rv = $cb->();                                     # Result should be a ref to a list.                                     unless (ref $rv eq 'ARRAY') {                                         return undef;                                     }                                     my $n = @$rv;                                     if ($n <= 0) {                                         return 1;                                     }                                     if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                                         $n = $ctx->{is_bound} - 1;                                     }                                     for (my $i = 0; $i < $n; $i++) {                                         my $rvi = $rv->[$i];                                         $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                                     }                                     return 1;                                 }                                 goto RESTART;                             }                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                     }                      # ,1,"foo\n 3",,bar\r\t                     #                    ^                     $self->__error_inside_field($ctx, 2032);                     return;                 }             }             else {                 if ($ctx->{eolx} and $c eq $eol) {                     $c = '';                     goto EOLX;                 }                  if ($waitingForField) {                     if (!$spl && $ctx->{comment_str} && $ctx->{tmp} =~ /\A$ctx->{comment_str}/) {                         $ctx->{used} = $ctx->{size};                         $ctx->{fld_idx} = 0;                         $seenSomething = 0;                         next LOOP;                     }                     if ($ctx->{allow_whitespace} and $self->__is_whitespace($ctx, $c)) {                         do {                             $c = $self->__get($ctx, $src);                             last if !defined $c;                         } while $self->__is_whitespace($ctx, $c);                         goto RESTART;                     }                     $waitingForField = 0;                     goto RESTART;                 }                 if ($ctx->{flag} & IS_QUOTED) {                     if (!defined $c or $c =~ /[^\x09\x20-\x7E]/) {                         $ctx->{flag} |= IS_BINARY;                         unless ($ctx->{binary} or $ctx->{utf8}) {                             $self->__error_inside_quotes($ctx, 2026);                             return;                         }                     }                     $$v_ref .= $c;                 } else {                     if (!defined $c or $c =~ /[^\x09\x20-\x7E]/) {                         last if $ctx->{useIO} && !defined $c;                         $ctx->{flag} |= IS_BINARY;                         unless ($ctx->{binary} or $ctx->{utf8}) {                             $self->__error_inside_field($ctx, 2037);                             return;                         }                     }                     $$v_ref .= $c;                 }             }             last LOOP if $ctx->{useIO} and $ctx->{verbatim} and $ctx->{used} == $ctx->{size};         }     }
        (pl-while         (pl-method-call $self '__get_from_src $ctx $src) :label LOOP
          ;; while($ctx->{tmp} =~ /\G(.*?)($re)/gs) {             my ($hit, $c) = ($1, $2);             $ctx->{used} = pos($ctx->{tmp});             if (!$waitingForField and $c eq '' and $hit ne '' and $ctx->{useIO} and !($ctx->{useIO} & useIO_EOF)) {                 $self->{_AHEAD} = $hit;                 $ctx->{has_ahead} = 1;                 $ctx->{has_leftover} = 1;                 last;             }             last if $seenSomething and $hit eq '' and $c eq ''; # EOF              # new field             if (!$v_ref) {                 if ($ctx->{is_bound}) {                     $v_ref = $self->__bound_field($ctx, $fnum, 0);                 } else {                     $value = '';                     $v_ref = \$value;                 }                 $fnum++;                 return unless $v_ref;                 $ctx->{flag} = 0;                     $ctx->{fld_idx}++;             }              $seenSomething = 1;             $spl++;              if (defined $hit and $hit ne '') {                 if ($waitingForField) {                     if (!$spl && $ctx->{comment_str} && $ctx->{tmp} =~ /\A\Q$ctx->{comment_str}/) {                         $ctx->{used} = $ctx->{size};                         $ctx->{fld_idx} = 0;                         $seenSomething = 0;                         next LOOP;                     }                     $waitingForField = 0;                 }                 if ($hit =~ /[^\x09\x20-\x7E]/) {                     $ctx->{flag} |= IS_BINARY;                 }                 $$v_ref .= $hit;             }  RESTART:             if (defined $c and defined $sep and $c eq $sep) {                 if ($waitingForField) {                     # ,1,"foo, 3",,bar,                     # ^           ^                     if ($ctx->{blank_is_undef} or $ctx->{empty_is_undef}) {                         $$v_ref = undef;                     } else {                         $$v_ref = "";                     }                     unless ($ctx->{is_bound}) {                         push @$fields, $$v_ref;                     }                     $v_ref = undef;                     if ($ctx->{keep_meta_info} and $fflags) {                         push @$fflags, $ctx->{flag};                     }                 } elsif ($ctx->{flag} & IS_QUOTED) {                     # ,1,"foo, 3",,bar,                     #        ^                     $$v_ref .= $c;                 } else {                     # ,1,"foo, 3",,bar,                     #   ^        ^    ^                     $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                     $v_ref = undef;                     $waitingForField = 1;                 }             }             elsif (defined $c and defined $quot and $quot ne "\0" and $c eq $quot) {                 if ($waitingForField) {                     # ,1,"foo, 3",,bar,\r\n                     #    ^                     $ctx->{flag} |= IS_QUOTED;                     $waitingForField = 0;                     next;                 }                 if ($ctx->{flag} & IS_QUOTED) {                     # ,1,"foo, 3",,bar,\r\n                     #           ^                     my $quoesc = 0;                     my $c2 = $self->__get($ctx, $src);                      if ($ctx->{allow_whitespace}) {                         # , 1 , "foo, 3" , , bar , \r\n                         #               ^                         while($self->__is_whitespace($ctx, $c2)) {                             if ($ctx->{allow_loose_quotes} and !(defined $esc and $c2 eq $esc)) {                                 $$v_ref .= $c;                                 $c = $c2;                             }                             $c2 = $self->__get($ctx, $src);                         }                     }                      if (!defined $c2) { # EOF                         # ,1,"foo, 3"                         #            ^                         $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                         return 1;                     }                      if (defined $c2 and defined $sep and $c2 eq $sep) {                         # ,1,"foo, 3",,bar,\r\n                         #            ^                         $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                         $v_ref = undef;                         $waitingForField = 1;                         next;                     }                     if (defined $c2 and ($c2 eq "\012" or (defined $eol and $c2 eq $eol))) { # FIXME: EOLX                         # ,1,"foo, 3",,"bar"\n                         #                   ^                         $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                         return 1;                     }                      if (defined $esc and $c eq $esc) {                         $quoesc = 1;                         if (defined $c2 and $c2 eq '0') {                             # ,1,"foo, 3"056",,bar,\r\n                             #            ^                             $$v_ref .= "\0";                             next;                         }                         if (defined $c2 and defined $quot and $c2 eq $quot) {                             # ,1,"foo, 3""56",,bar,\r\n                             #            ^                             if ($ctx->{utf8}) {                                 $ctx->{flag} |= IS_BINARY;                             }                             $$v_ref .= $c2;                             next;                         }                         if ($ctx->{allow_loose_escapes} and defined $c2 and $c2 ne "\015") {                             # ,1,"foo, 3"56",,bar,\r\n                             #            ^                             $$v_ref .= $c;                             $c = $c2;                             goto RESTART;                         }                     }                     if (defined $c2 and $c2 eq "\015") {                         if ($ctx->{eol_is_cr}) {                             # ,1,"foo, 3"\r                             #            ^                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                          my $c3 = $self->__get($ctx, $src);                         if (defined $c3 and $c3 eq "\012") {                             # ,1,"foo, 3"\r\n                             #              ^                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                          if ($ctx->{useIO} and !$ctx->{eol_len}) {                             if ($c3 eq "\015") { # \r followed by an empty line                                 # ,1,"foo, 3"\r\r                                 #              ^                                 $self->__set_eol_is_cr($ctx);                                 goto EOLX;                             }                             if ($c3 !~ /[^\x09\x20-\x7E]/) {                                 # ,1,"foo\n 3",,"bar"\r                                 # baz,4                                 # ^                                 $self->__set_eol_is_cr($ctx);                                 $ctx->{used}--;                                 $ctx->{has_ahead} = 1;                                 $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                                 return 1;                             }                         }                          $self->__parse_error($ctx, $quoesc ? 2023 : 2010, $ctx->{used} - 2);                         return;                     }                      if ($ctx->{allow_loose_quotes} and !$quoesc) {                         # ,1,"foo, 3"456",,bar,\r\n                         #            ^                         $$v_ref .= $c;                         $c = $c2;                         goto RESTART;                     }                     # 1,"foo" ",3                     #        ^                     if ($quoesc) {                         $ctx->{used}--;                         $self->__error_inside_quotes($ctx, 2023);                         return;                     }                     $self->__error_inside_quotes($ctx, 2011);                     return;                 }                 # !waitingForField, !InsideQuotes                 if ($ctx->{allow_loose_quotes}) { # 1,foo "boo" d'uh,1                     $ctx->{flag} |= IS_ERROR;                     $$v_ref .= $c;                 } else {                     $self->__error_inside_field($ctx, 2034);                     return;                 }             }             elsif (defined $c and defined $esc and $esc ne "\0" and $c eq $esc) {                 # This means quote_char != escape_char                 if ($waitingForField) {                     $waitingForField = 0;                     if ($ctx->{allow_unquoted_escape}) {                         # The escape character is the first character of an                         # unquoted field                         # ... get and store next character                         my $c2 = $self->__get($ctx, $src);                         $$v_ref = "";                          if (!defined $c2) { # EOF                             $ctx->{used}--;                             $self->__error_inside_field($ctx, 2035);                             return;                         }                         if ($c2 eq '0') {                             $$v_ref .= "\0";                         }                         elsif (                             (defined $quot and $c2 eq $quot) or                             (defined $sep and $c2 eq $sep) or                             (defined $esc and $c2 eq $esc) or                             $ctx->{allow_loose_escapes}                         ) {                             if ($ctx->{utf8}) {                                 $ctx->{flag} |= IS_BINARY;                             }                             $$v_ref .= $c2;                         } else {                             $self->__parse_inside_quotes($ctx, 2025);                             return;                         }                     }                 }                 elsif ($ctx->{flag} & IS_QUOTED) {                     my $c2 = $self->__get($ctx, $src);                     if (!defined $c2) { # EOF                         $ctx->{used}--;                         $self->__error_inside_quotes($ctx, 2024);                         return;                     }                     if ($c2 eq '0') {                         $$v_ref .= "\0";                     }                     elsif (                         (defined $quot and $c2 eq $quot) or                         (defined $sep and $c2 eq $sep) or                         (defined $esc and $c2 eq $esc) or                         $ctx->{allow_loose_escapes}                     ) {                         if ($ctx->{utf8}) {                             $ctx->{flag} |= IS_BINARY;                         }                         $$v_ref .= $c2;                     } else {                         $ctx->{used}--;                         $self->__error_inside_quotes($ctx, 2025);                         return;                     }                 }                 elsif ($v_ref) {                     my $c2 = $self->__get($ctx, $src);                     if (!defined $c2) { # EOF                         $ctx->{used}--;                         $self->__error_inside_field($ctx, 2035);                         return;                     }                     $$v_ref .= $c2;                 }                 else {                     $self->__error_inside_field($ctx, 2036);                     return;                 }             }             elsif (defined $c and ($c eq "\012" or $c eq '' or (defined $eol and $c eq $eol and $eol ne "\015"))) { # EOL     EOLX:                 if ($fnum == 1 && $ctx->{flag} == 0 && (!$v_ref || $$v_ref eq '') && $ctx->{skip_empty_rows}) {                     ### SkipEmptyRow                     my $ser = $ctx->{skip_empty_rows};                     if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }                     if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }                     if ($ser == 5) { $self->SetDiag(2015); return undef; }                      if ($ser <= 2) { # skip & eof                         $ctx->{fld_idx} = 0;                         $c = $self->__get($ctx, $src);                         if (!defined $c or $ser == 2) {  # EOF                             $v_ref = undef;                             $seenSomething = 0;                             if ($ser == 2) { return undef; }                             last LOOP;                         }                     }                      if ($ser == 6) {                         my $cb = $self->{_EMPTROW_CB};                         unless ($cb && ref $cb eq 'CODE') {                             return undef;  # A callback is wanted, but none found                         }                         local $_ = $v_ref;                         my $rv = $cb->();                         # Result should be a ref to a list.                         unless (ref $rv eq 'ARRAY') {                             return undef;                         }                         my $n = @$rv;                         if ($n <= 0) {                             return 1;                         }                         if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                             $n = $ctx->{is_bound} - 1;                         }                         for (my $i = 0; $i < $n; $i++) {                             my $rvi = $rv->[$i];                             $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                         }                         return 1;                     }                     goto RESTART;                 }                  if ($waitingForField) {                     # ,1,"foo, 3",,bar,                     #                  ^                     if ($ctx->{blank_is_undef} or $ctx->{empty_is_undef}) {                         $$v_ref = undef;                     } else {                         $$v_ref = "";                     }                     unless ($ctx->{is_bound}) {                         push @$fields, $$v_ref;                     }                     if ($ctx->{keep_meta_info} and $fflags) {                         push @$fflags, $ctx->{flag};                     }                     return 1;                 }                 if ($ctx->{flag} & IS_QUOTED) {                     # ,1,"foo\n 3",,bar,                     #        ^                     $ctx->{flag} |= IS_BINARY;                     unless ($ctx->{binary}) {                         $self->__error_inside_quotes($ctx, 2021);                         return;                     }                     $$v_ref .= $c;                 }                 elsif ($ctx->{verbatim}) {                     # ,1,foo\n 3,,bar,                     # This feature should be deprecated                     $ctx->{flag} |= IS_BINARY;                     unless ($ctx->{binary}) {                         $self->__error_inside_field($ctx, 2030);                         return;                     }                     $$v_ref .= $c unless $ctx->{eol} eq $c and $ctx->{useIO};                 }                 else {                     # sep=,                     #      ^                     if (!$ctx->{recno} and $ctx->{fld_idx} == 1 and $ctx->{useIO} and $hit =~ /^sep=(.{1,16})$/i) {                         $ctx->{sep} = $1;                         use bytes;                         my $len = length $ctx->{sep};                         if ($len <= 16) {                             $ctx->{sep_len} = $len == 1 ? 0 : $len;                             return $self->____parse($ctx, $src, $fields, $fflags);                         }                     }                      # ,1,"foo\n 3",,bar                     #                  ^                     $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                     return 1;                 }             }             elsif (defined $c and $c eq "\015" and !$ctx->{verbatim}) {                 if ($waitingForField) {                     if ($ctx->{eol_is_cr}) {                         # ,1,"foo\n 3",,bar,\r                         #                   ^                         $c = "\012";                         goto EOLX;                     }                      my $c2 = $self->__get($ctx, $src);                     if (!defined $c2) { # EOF                         # ,1,"foo\n 3",,bar,\r                         #                     ^                         $c = undef;                         last unless $seenSomething;                         goto RESTART;                     }                     if ($c2 eq "\012") { # \r is not optional before EOLX!                         # ,1,"foo\n 3",,bar,\r\n                         #                     ^                         $c = $c2;                         goto EOLX;                     }                      if ($ctx->{useIO} and !$ctx->{eol_len}) {                         if ($c2 eq "\012") { # \r followed by an empty line                             # ,1,"foo\n 3",,bar,\r\r                             #                     ^                             $self->__set_eol_is_cr($ctx);                             goto EOLX;                         }                         $waitingForField = 0;                         if ($c2 !~ /[^\x09\x20-\x7E]/) {                             # ,1,"foo\n 3",,bar,\r                             # baz,4                             # ^                             $self->__set_eol_is_cr($ctx);                             $ctx->{used}--;                             $ctx->{has_ahead} = 1;                             if ($fnum == 1 && $ctx->{flag} == 0 && (!$v_ref or $$v_ref eq '') && $ctx->{skip_empty_rows}) {                                 ### SkipEmptyRow                                 my $ser = $ctx->{skip_empty_rows};                                 if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 5) { $self->SetDiag(2015); return undef; }                                  if ($ser <= 2) { # skip & eof                                     $ctx->{fld_idx} = 0;                                     $c = $self->__get($ctx, $src);                                     if (!defined $c) { # EOF                                         $v_ref = undef;                                         $waitingForField = 1;                                         $seenSomething = 0;                                         last LOOP;                                     }                                 }                                  if ($ser == 6) {                                     my $cb = $self->{_EMPTROW_CB};                                     unless ($cb && ref $cb eq 'CODE') {                                         return undef;  # A callback is wanted, but none found                                     }                                     local $_ = $v_ref;                                     my $rv = $cb->();                                     # Result should be a ref to a list.                                     unless (ref $rv eq 'ARRAY') {                                         return undef;                                     }                                     my $n = @$rv;                                     if ($n <= 0) {                                         return 1;                                     }                                     if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                                         $n = $ctx->{is_bound} - 1;                                     }                                     for (my $i = 0; $i < $n; $i++) {                                         my $rvi = $rv->[$i];                                         $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                                     }                                     return 1;                                 }                                  $$v_ref = $c2;                                 goto RESTART;                             }                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                     }                      # ,1,"foo\n 3",,bar,\r\t                     #                     ^                     $ctx->{used}--;                     $self->__error_inside_field($ctx, 2031);                     return;                 }                 if ($ctx->{flag} & IS_QUOTED) {                     # ,1,"foo\r 3",,bar,\r\t                     #        ^                     $ctx->{flag} |= IS_BINARY;                     unless ($ctx->{binary}) {                         $self->__error_inside_quotes($ctx, 2022);                         return;                     }                     $$v_ref .= $c;                 }                 else {                     if ($ctx->{eol_is_cr}) {                         # ,1,"foo\n 3",,bar\r                         #                  ^                         goto EOLX;                     }                      my $c2 = $self->__get($ctx, $src);                     if (defined $c2 and $c2 eq "\012") { # \r is not optional before EOLX!                         # ,1,"foo\n 3",,bar\r\n                         #                    ^                         goto EOLX;                     }                      if ($ctx->{useIO} and !$ctx->{eol_len}) {                         if ($c2 !~ /[^\x09\x20-\x7E]/                             # ,1,"foo\n 3",,bar\r                             # baz,4                             # ^                             or $c2 eq "\015"                             # ,1,"foo\n 3",,bar,\r\r                             #                     ^                         ) {                             $self->__set_eol_is_cr($ctx);                             $ctx->{used}--;                             $ctx->{has_ahead} = 1;                             if ($fnum == 1 && $ctx->{flag} == 0 && (!$v_ref or $$v_ref eq '') && $ctx->{skip_empty_rows}) {                                 ### SKipEmptyRow                                 my $ser = $ctx->{skip_empty_rows};                                 if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 5) { $self->SetDiag(2015); return undef; }                                  if ($ser <= 2) { # skip & eof                                     $ctx->{fld_idx} = 0;                                     $c = $self->__get($ctx, $src);                                     if (!defined $c) { # EOL                                         $v_ref = undef;                                         $seenSomething = 0;                                         last LOOP;                                     }                                 }                                  if ($ser == 6) {                                     my $cb = $self->{_EMPTROW_CB};                                     unless ($cb && ref $cb eq 'CODE') {                                         return undef;  # A callback is wanted, but none found                                     }                                     local $_ = $v_ref;                                     my $rv = $cb->();                                     # Result should be a ref to a list.                                     unless (ref $rv eq 'ARRAY') {                                         return undef;                                     }                                     my $n = @$rv;                                     if ($n <= 0) {                                         return 1;                                     }                                     if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                                         $n = $ctx->{is_bound} - 1;                                     }                                     for (my $i = 0; $i < $n; $i++) {                                         my $rvi = $rv->[$i];                                         $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                                     }                                     return 1;                                 }                                 goto RESTART;                             }                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                     }                      # ,1,"foo\n 3",,bar\r\t                     #                    ^                     $self->__error_inside_field($ctx, 2032);                     return;                 }             }             else {                 if ($ctx->{eolx} and $c eq $eol) {                     $c = '';                     goto EOLX;                 }                  if ($waitingForField) {                     if (!$spl && $ctx->{comment_str} && $ctx->{tmp} =~ /\A$ctx->{comment_str}/) {                         $ctx->{used} = $ctx->{size};                         $ctx->{fld_idx} = 0;                         $seenSomething = 0;                         next LOOP;                     }                     if ($ctx->{allow_whitespace} and $self->__is_whitespace($ctx, $c)) {                         do {                             $c = $self->__get($ctx, $src);                             last if !defined $c;                         } while $self->__is_whitespace($ctx, $c);                         goto RESTART;                     }                     $waitingForField = 0;                     goto RESTART;                 }                 if ($ctx->{flag} & IS_QUOTED) {                     if (!defined $c or $c =~ /[^\x09\x20-\x7E]/) {                         $ctx->{flag} |= IS_BINARY;                         unless ($ctx->{binary} or $ctx->{utf8}) {                             $self->__error_inside_quotes($ctx, 2026);                             return;                         }                     }                     $$v_ref .= $c;                 } else {                     if (!defined $c or $c =~ /[^\x09\x20-\x7E]/) {                         last if $ctx->{useIO} && !defined $c;                         $ctx->{flag} |= IS_BINARY;                         unless ($ctx->{binary} or $ctx->{utf8}) {                             $self->__error_inside_field($ctx, 2037);                             return;                         }                     }                     $$v_ref .= $c;                 }             }             last LOOP if $ctx->{useIO} and $ctx->{verbatim} and $ctx->{used} == $ctx->{size};         }
          (pl-while           (pcl:pl-=~ (pl-gethash-deref $ctx "tmp") (pl-regex "/\\G(.*?)($re)/gs"))
            ;; my ($hit, $c) = ($1, $2)
                        (pcl:pl-setf (vector $hit $c) (vector $1 $2))
            
            ;; $ctx->{used} = pos($ctx->{tmp})
                        (pcl:pl-setf (pl-gethash-deref $ctx "used") (pl-pos (pl-gethash-deref $ctx "tmp")))
            
            ;; if (!$waitingForField and $c eq '' and $hit ne '' and $ctx->{useIO} and !($ctx->{useIO} & useIO_EOF)) {                 $self->{_AHEAD} = $hit;                 $ctx->{has_ahead} = 1;                 $ctx->{has_leftover} = 1;                 last;             }
            ;; if (!$waitingForField and $c eq '' and $hit ne '' and $ctx->{"useIO"} and !($ctx->{"useIO"} & useIO_EOF))
            (pl-if             (pcl:pl-and (pcl:pl-and (pcl:pl-and (pcl:pl-and (pl-! $waitingForField) (pcl:pl-str-eq $c "")) (pcl:pl-str-ne $hit "")) (pl-gethash-deref $ctx "useIO")) (pl-! (pcl:pl-bit-and (pl-gethash-deref $ctx "useIO") (pl-useIO_EOF))))
              (progn
                ;; $self->{_AHEAD} = $hit
                                (pcl:pl-setf (pl-gethash-deref $self "_AHEAD") $hit)
                
                ;; $ctx->{has_ahead} = 1
                                (pcl:pl-setf (pl-gethash-deref $ctx "has_ahead") 1)
                
                ;; $ctx->{has_leftover} = 1
                                (pcl:pl-setf (pl-gethash-deref $ctx "has_leftover") 1)
                
                ;; last
                                (pl-last)
                
              )
              nil
            )
            
            ;; last if $seenSomething and $hit eq '' and $c eq ''
            (pl-if             (pcl:pl-and (pcl:pl-and $seenSomething (pcl:pl-str-eq $hit "")) (pcl:pl-str-eq $c ""))             (pl-last))
            
            ;; if (!$v_ref) {                 if ($ctx->{is_bound}) {                     $v_ref = $self->__bound_field($ctx, $fnum, 0);                 } else {                     $value = '';                     $v_ref = \$value;                 }                 $fnum++;                 return unless $v_ref;                 $ctx->{flag} = 0;                     $ctx->{fld_idx}++;             }
            ;; if (!$v_ref)
            (pl-if             (pl-! $v_ref)
              (progn
                ;; if ($ctx->{is_bound}) {                     $v_ref = $self->__bound_field($ctx, $fnum, 0);                 } else {                     $value = '';                     $v_ref = \$value;                 }
                ;; if ($ctx->{"is_bound"})
                (pl-if                 (pl-gethash-deref $ctx "is_bound")
                  (progn
                    ;; $v_ref = $self->__bound_field($ctx, $fnum, 0)
                                        (pcl:pl-setf $v_ref (pl-method-call $self '__bound_field $ctx $fnum 0))
                    
                  )
                  ;; else
                  (progn
                    ;; $value = ''
                                        (pcl:pl-setf $value "")
                    
                    ;; $v_ref = \$value
                                        (pcl:pl-setf $v_ref (pl-backslash $value))
                    
                  )
                )
                
                ;; $fnum++
                                (pl-post++ $fnum)
                
                ;; return unless $v_ref
                (pl-unless                 $v_ref                 (pl-return))
                
                ;; $ctx->{flag} = 0
                                (pcl:pl-setf (pl-gethash-deref $ctx "flag") 0)
                
                ;; $ctx->{fld_idx}++
                                (pl-post++ (pl-gethash-deref $ctx "fld_idx"))
                
              )
              nil
            )
            
            ;; $seenSomething = 1
                        (pcl:pl-setf $seenSomething 1)
            
            ;; $spl++
                        (pl-post++ $spl)
            
            ;; if (defined $hit and $hit ne '') {                 if ($waitingForField) {                     if (!$spl && $ctx->{comment_str} && $ctx->{tmp} =~ /\A\Q$ctx->{comment_str}/) {                         $ctx->{used} = $ctx->{size};                         $ctx->{fld_idx} = 0;                         $seenSomething = 0;                         next LOOP;                     }                     $waitingForField = 0;                 }                 if ($hit =~ /[^\x09\x20-\x7E]/) {                     $ctx->{flag} |= IS_BINARY;                 }                 $$v_ref .= $hit;             }
            ;; if (defined $hit and $hit ne '')
            (pl-if             (pcl:pl-and (pl-defined $hit) (pcl:pl-str-ne $hit ""))
              (progn
                ;; if ($waitingForField) {                     if (!$spl && $ctx->{comment_str} && $ctx->{tmp} =~ /\A\Q$ctx->{comment_str}/) {                         $ctx->{used} = $ctx->{size};                         $ctx->{fld_idx} = 0;                         $seenSomething = 0;                         next LOOP;                     }                     $waitingForField = 0;                 }
                ;; if ($waitingForField)
                (pl-if                 $waitingForField
                  (progn
                    ;; if (!$spl && $ctx->{comment_str} && $ctx->{tmp} =~ /\A\Q$ctx->{comment_str}/) {                         $ctx->{used} = $ctx->{size};                         $ctx->{fld_idx} = 0;                         $seenSomething = 0;                         next LOOP;                     }
                    ;; if (!$spl && $ctx->{"comment_str"} && $ctx->{"tmp"} =~ /\A\Q$ctx->{comment_str}/)
                    (pl-if                     (pcl:pl-&& (pcl:pl-&& (pl-! $spl) (pl-gethash-deref $ctx "comment_str")) (pcl:pl-=~ (pl-gethash-deref $ctx "tmp") (pl-regex "/\\A\\Q$ctx->{comment_str}/")))
                      (progn
                        ;; $ctx->{used} = $ctx->{size}
                                                (pcl:pl-setf (pl-gethash-deref $ctx "used") (pl-gethash-deref $ctx "size"))
                        
                        ;; $ctx->{fld_idx} = 0
                                                (pcl:pl-setf (pl-gethash-deref $ctx "fld_idx") 0)
                        
                        ;; $seenSomething = 0
                                                (pcl:pl-setf $seenSomething 0)
                        
                        ;; next LOOP
                                                (pl-next LOOP)
                        
                      )
                      nil
                    )
                    
                    ;; $waitingForField = 0
                                        (pcl:pl-setf $waitingForField 0)
                    
                  )
                  nil
                )
                
                ;; if ($hit =~ /[^\x09\x20-\x7E]/) {                     $ctx->{flag} |= IS_BINARY;                 }
                ;; if ($hit =~ /[^\x09\x20-\x7E]/)
                (pl-if                 (pcl:pl-=~ $hit (pl-regex "/[^\\x09\\x20-\\x7E]/"))
                  (progn
                    ;; $ctx->{flag} |= IS_BINARY
                                        (pcl:pl-bit-or= (pl-gethash-deref $ctx "flag") (pl-IS_BINARY))
                    
                  )
                  nil
                )
                
                ;; $$v_ref .= $hit
                                (pcl:pl-.= (pl-cast-$ $v_ref) $hit)
                
              )
              nil
            )
            
            ;; COMPOUND () not yet implemented:
            ;; RESTART:
            
            ;; if (defined $c and defined $sep and $c eq $sep) {                 if ($waitingForField) {                     # ,1,"foo, 3",,bar,                     # ^           ^                     if ($ctx->{blank_is_undef} or $ctx->{empty_is_undef}) {                         $$v_ref = undef;                     } else {                         $$v_ref = "";                     }                     unless ($ctx->{is_bound}) {                         push @$fields, $$v_ref;                     }                     $v_ref = undef;                     if ($ctx->{keep_meta_info} and $fflags) {                         push @$fflags, $ctx->{flag};                     }                 } elsif ($ctx->{flag} & IS_QUOTED) {                     # ,1,"foo, 3",,bar,                     #        ^                     $$v_ref .= $c;                 } else {                     # ,1,"foo, 3",,bar,                     #   ^        ^    ^                     $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                     $v_ref = undef;                     $waitingForField = 1;                 }             }             elsif (defined $c and defined $quot and $quot ne "\0" and $c eq $quot) {                 if ($waitingForField) {                     # ,1,"foo, 3",,bar,\r\n                     #    ^                     $ctx->{flag} |= IS_QUOTED;                     $waitingForField = 0;                     next;                 }                 if ($ctx->{flag} & IS_QUOTED) {                     # ,1,"foo, 3",,bar,\r\n                     #           ^                     my $quoesc = 0;                     my $c2 = $self->__get($ctx, $src);                      if ($ctx->{allow_whitespace}) {                         # , 1 , "foo, 3" , , bar , \r\n                         #               ^                         while($self->__is_whitespace($ctx, $c2)) {                             if ($ctx->{allow_loose_quotes} and !(defined $esc and $c2 eq $esc)) {                                 $$v_ref .= $c;                                 $c = $c2;                             }                             $c2 = $self->__get($ctx, $src);                         }                     }                      if (!defined $c2) { # EOF                         # ,1,"foo, 3"                         #            ^                         $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                         return 1;                     }                      if (defined $c2 and defined $sep and $c2 eq $sep) {                         # ,1,"foo, 3",,bar,\r\n                         #            ^                         $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                         $v_ref = undef;                         $waitingForField = 1;                         next;                     }                     if (defined $c2 and ($c2 eq "\012" or (defined $eol and $c2 eq $eol))) { # FIXME: EOLX                         # ,1,"foo, 3",,"bar"\n                         #                   ^                         $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                         return 1;                     }                      if (defined $esc and $c eq $esc) {                         $quoesc = 1;                         if (defined $c2 and $c2 eq '0') {                             # ,1,"foo, 3"056",,bar,\r\n                             #            ^                             $$v_ref .= "\0";                             next;                         }                         if (defined $c2 and defined $quot and $c2 eq $quot) {                             # ,1,"foo, 3""56",,bar,\r\n                             #            ^                             if ($ctx->{utf8}) {                                 $ctx->{flag} |= IS_BINARY;                             }                             $$v_ref .= $c2;                             next;                         }                         if ($ctx->{allow_loose_escapes} and defined $c2 and $c2 ne "\015") {                             # ,1,"foo, 3"56",,bar,\r\n                             #            ^                             $$v_ref .= $c;                             $c = $c2;                             goto RESTART;                         }                     }                     if (defined $c2 and $c2 eq "\015") {                         if ($ctx->{eol_is_cr}) {                             # ,1,"foo, 3"\r                             #            ^                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                          my $c3 = $self->__get($ctx, $src);                         if (defined $c3 and $c3 eq "\012") {                             # ,1,"foo, 3"\r\n                             #              ^                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                          if ($ctx->{useIO} and !$ctx->{eol_len}) {                             if ($c3 eq "\015") { # \r followed by an empty line                                 # ,1,"foo, 3"\r\r                                 #              ^                                 $self->__set_eol_is_cr($ctx);                                 goto EOLX;                             }                             if ($c3 !~ /[^\x09\x20-\x7E]/) {                                 # ,1,"foo\n 3",,"bar"\r                                 # baz,4                                 # ^                                 $self->__set_eol_is_cr($ctx);                                 $ctx->{used}--;                                 $ctx->{has_ahead} = 1;                                 $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                                 return 1;                             }                         }                          $self->__parse_error($ctx, $quoesc ? 2023 : 2010, $ctx->{used} - 2);                         return;                     }                      if ($ctx->{allow_loose_quotes} and !$quoesc) {                         # ,1,"foo, 3"456",,bar,\r\n                         #            ^                         $$v_ref .= $c;                         $c = $c2;                         goto RESTART;                     }                     # 1,"foo" ",3                     #        ^                     if ($quoesc) {                         $ctx->{used}--;                         $self->__error_inside_quotes($ctx, 2023);                         return;                     }                     $self->__error_inside_quotes($ctx, 2011);                     return;                 }                 # !waitingForField, !InsideQuotes                 if ($ctx->{allow_loose_quotes}) { # 1,foo "boo" d'uh,1                     $ctx->{flag} |= IS_ERROR;                     $$v_ref .= $c;                 } else {                     $self->__error_inside_field($ctx, 2034);                     return;                 }             }             elsif (defined $c and defined $esc and $esc ne "\0" and $c eq $esc) {                 # This means quote_char != escape_char                 if ($waitingForField) {                     $waitingForField = 0;                     if ($ctx->{allow_unquoted_escape}) {                         # The escape character is the first character of an                         # unquoted field                         # ... get and store next character                         my $c2 = $self->__get($ctx, $src);                         $$v_ref = "";                          if (!defined $c2) { # EOF                             $ctx->{used}--;                             $self->__error_inside_field($ctx, 2035);                             return;                         }                         if ($c2 eq '0') {                             $$v_ref .= "\0";                         }                         elsif (                             (defined $quot and $c2 eq $quot) or                             (defined $sep and $c2 eq $sep) or                             (defined $esc and $c2 eq $esc) or                             $ctx->{allow_loose_escapes}                         ) {                             if ($ctx->{utf8}) {                                 $ctx->{flag} |= IS_BINARY;                             }                             $$v_ref .= $c2;                         } else {                             $self->__parse_inside_quotes($ctx, 2025);                             return;                         }                     }                 }                 elsif ($ctx->{flag} & IS_QUOTED) {                     my $c2 = $self->__get($ctx, $src);                     if (!defined $c2) { # EOF                         $ctx->{used}--;                         $self->__error_inside_quotes($ctx, 2024);                         return;                     }                     if ($c2 eq '0') {                         $$v_ref .= "\0";                     }                     elsif (                         (defined $quot and $c2 eq $quot) or                         (defined $sep and $c2 eq $sep) or                         (defined $esc and $c2 eq $esc) or                         $ctx->{allow_loose_escapes}                     ) {                         if ($ctx->{utf8}) {                             $ctx->{flag} |= IS_BINARY;                         }                         $$v_ref .= $c2;                     } else {                         $ctx->{used}--;                         $self->__error_inside_quotes($ctx, 2025);                         return;                     }                 }                 elsif ($v_ref) {                     my $c2 = $self->__get($ctx, $src);                     if (!defined $c2) { # EOF                         $ctx->{used}--;                         $self->__error_inside_field($ctx, 2035);                         return;                     }                     $$v_ref .= $c2;                 }                 else {                     $self->__error_inside_field($ctx, 2036);                     return;                 }             }             elsif (defined $c and ($c eq "\012" or $c eq '' or (defined $eol and $c eq $eol and $eol ne "\015"))) { # EOL     EOLX:                 if ($fnum == 1 && $ctx->{flag} == 0 && (!$v_ref || $$v_ref eq '') && $ctx->{skip_empty_rows}) {                     ### SkipEmptyRow                     my $ser = $ctx->{skip_empty_rows};                     if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }                     if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }                     if ($ser == 5) { $self->SetDiag(2015); return undef; }                      if ($ser <= 2) { # skip & eof                         $ctx->{fld_idx} = 0;                         $c = $self->__get($ctx, $src);                         if (!defined $c or $ser == 2) {  # EOF                             $v_ref = undef;                             $seenSomething = 0;                             if ($ser == 2) { return undef; }                             last LOOP;                         }                     }                      if ($ser == 6) {                         my $cb = $self->{_EMPTROW_CB};                         unless ($cb && ref $cb eq 'CODE') {                             return undef;  # A callback is wanted, but none found                         }                         local $_ = $v_ref;                         my $rv = $cb->();                         # Result should be a ref to a list.                         unless (ref $rv eq 'ARRAY') {                             return undef;                         }                         my $n = @$rv;                         if ($n <= 0) {                             return 1;                         }                         if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                             $n = $ctx->{is_bound} - 1;                         }                         for (my $i = 0; $i < $n; $i++) {                             my $rvi = $rv->[$i];                             $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                         }                         return 1;                     }                     goto RESTART;                 }                  if ($waitingForField) {                     # ,1,"foo, 3",,bar,                     #                  ^                     if ($ctx->{blank_is_undef} or $ctx->{empty_is_undef}) {                         $$v_ref = undef;                     } else {                         $$v_ref = "";                     }                     unless ($ctx->{is_bound}) {                         push @$fields, $$v_ref;                     }                     if ($ctx->{keep_meta_info} and $fflags) {                         push @$fflags, $ctx->{flag};                     }                     return 1;                 }                 if ($ctx->{flag} & IS_QUOTED) {                     # ,1,"foo\n 3",,bar,                     #        ^                     $ctx->{flag} |= IS_BINARY;                     unless ($ctx->{binary}) {                         $self->__error_inside_quotes($ctx, 2021);                         return;                     }                     $$v_ref .= $c;                 }                 elsif ($ctx->{verbatim}) {                     # ,1,foo\n 3,,bar,                     # This feature should be deprecated                     $ctx->{flag} |= IS_BINARY;                     unless ($ctx->{binary}) {                         $self->__error_inside_field($ctx, 2030);                         return;                     }                     $$v_ref .= $c unless $ctx->{eol} eq $c and $ctx->{useIO};                 }                 else {                     # sep=,                     #      ^                     if (!$ctx->{recno} and $ctx->{fld_idx} == 1 and $ctx->{useIO} and $hit =~ /^sep=(.{1,16})$/i) {                         $ctx->{sep} = $1;                         use bytes;                         my $len = length $ctx->{sep};                         if ($len <= 16) {                             $ctx->{sep_len} = $len == 1 ? 0 : $len;                             return $self->____parse($ctx, $src, $fields, $fflags);                         }                     }                      # ,1,"foo\n 3",,bar                     #                  ^                     $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                     return 1;                 }             }             elsif (defined $c and $c eq "\015" and !$ctx->{verbatim}) {                 if ($waitingForField) {                     if ($ctx->{eol_is_cr}) {                         # ,1,"foo\n 3",,bar,\r                         #                   ^                         $c = "\012";                         goto EOLX;                     }                      my $c2 = $self->__get($ctx, $src);                     if (!defined $c2) { # EOF                         # ,1,"foo\n 3",,bar,\r                         #                     ^                         $c = undef;                         last unless $seenSomething;                         goto RESTART;                     }                     if ($c2 eq "\012") { # \r is not optional before EOLX!                         # ,1,"foo\n 3",,bar,\r\n                         #                     ^                         $c = $c2;                         goto EOLX;                     }                      if ($ctx->{useIO} and !$ctx->{eol_len}) {                         if ($c2 eq "\012") { # \r followed by an empty line                             # ,1,"foo\n 3",,bar,\r\r                             #                     ^                             $self->__set_eol_is_cr($ctx);                             goto EOLX;                         }                         $waitingForField = 0;                         if ($c2 !~ /[^\x09\x20-\x7E]/) {                             # ,1,"foo\n 3",,bar,\r                             # baz,4                             # ^                             $self->__set_eol_is_cr($ctx);                             $ctx->{used}--;                             $ctx->{has_ahead} = 1;                             if ($fnum == 1 && $ctx->{flag} == 0 && (!$v_ref or $$v_ref eq '') && $ctx->{skip_empty_rows}) {                                 ### SkipEmptyRow                                 my $ser = $ctx->{skip_empty_rows};                                 if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 5) { $self->SetDiag(2015); return undef; }                                  if ($ser <= 2) { # skip & eof                                     $ctx->{fld_idx} = 0;                                     $c = $self->__get($ctx, $src);                                     if (!defined $c) { # EOF                                         $v_ref = undef;                                         $waitingForField = 1;                                         $seenSomething = 0;                                         last LOOP;                                     }                                 }                                  if ($ser == 6) {                                     my $cb = $self->{_EMPTROW_CB};                                     unless ($cb && ref $cb eq 'CODE') {                                         return undef;  # A callback is wanted, but none found                                     }                                     local $_ = $v_ref;                                     my $rv = $cb->();                                     # Result should be a ref to a list.                                     unless (ref $rv eq 'ARRAY') {                                         return undef;                                     }                                     my $n = @$rv;                                     if ($n <= 0) {                                         return 1;                                     }                                     if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                                         $n = $ctx->{is_bound} - 1;                                     }                                     for (my $i = 0; $i < $n; $i++) {                                         my $rvi = $rv->[$i];                                         $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                                     }                                     return 1;                                 }                                  $$v_ref = $c2;                                 goto RESTART;                             }                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                     }                      # ,1,"foo\n 3",,bar,\r\t                     #                     ^                     $ctx->{used}--;                     $self->__error_inside_field($ctx, 2031);                     return;                 }                 if ($ctx->{flag} & IS_QUOTED) {                     # ,1,"foo\r 3",,bar,\r\t                     #        ^                     $ctx->{flag} |= IS_BINARY;                     unless ($ctx->{binary}) {                         $self->__error_inside_quotes($ctx, 2022);                         return;                     }                     $$v_ref .= $c;                 }                 else {                     if ($ctx->{eol_is_cr}) {                         # ,1,"foo\n 3",,bar\r                         #                  ^                         goto EOLX;                     }                      my $c2 = $self->__get($ctx, $src);                     if (defined $c2 and $c2 eq "\012") { # \r is not optional before EOLX!                         # ,1,"foo\n 3",,bar\r\n                         #                    ^                         goto EOLX;                     }                      if ($ctx->{useIO} and !$ctx->{eol_len}) {                         if ($c2 !~ /[^\x09\x20-\x7E]/                             # ,1,"foo\n 3",,bar\r                             # baz,4                             # ^                             or $c2 eq "\015"                             # ,1,"foo\n 3",,bar,\r\r                             #                     ^                         ) {                             $self->__set_eol_is_cr($ctx);                             $ctx->{used}--;                             $ctx->{has_ahead} = 1;                             if ($fnum == 1 && $ctx->{flag} == 0 && (!$v_ref or $$v_ref eq '') && $ctx->{skip_empty_rows}) {                                 ### SKipEmptyRow                                 my $ser = $ctx->{skip_empty_rows};                                 if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 5) { $self->SetDiag(2015); return undef; }                                  if ($ser <= 2) { # skip & eof                                     $ctx->{fld_idx} = 0;                                     $c = $self->__get($ctx, $src);                                     if (!defined $c) { # EOL                                         $v_ref = undef;                                         $seenSomething = 0;                                         last LOOP;                                     }                                 }                                  if ($ser == 6) {                                     my $cb = $self->{_EMPTROW_CB};                                     unless ($cb && ref $cb eq 'CODE') {                                         return undef;  # A callback is wanted, but none found                                     }                                     local $_ = $v_ref;                                     my $rv = $cb->();                                     # Result should be a ref to a list.                                     unless (ref $rv eq 'ARRAY') {                                         return undef;                                     }                                     my $n = @$rv;                                     if ($n <= 0) {                                         return 1;                                     }                                     if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                                         $n = $ctx->{is_bound} - 1;                                     }                                     for (my $i = 0; $i < $n; $i++) {                                         my $rvi = $rv->[$i];                                         $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                                     }                                     return 1;                                 }                                 goto RESTART;                             }                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                     }                      # ,1,"foo\n 3",,bar\r\t                     #                    ^                     $self->__error_inside_field($ctx, 2032);                     return;                 }             }             else {                 if ($ctx->{eolx} and $c eq $eol) {                     $c = '';                     goto EOLX;                 }                  if ($waitingForField) {                     if (!$spl && $ctx->{comment_str} && $ctx->{tmp} =~ /\A$ctx->{comment_str}/) {                         $ctx->{used} = $ctx->{size};                         $ctx->{fld_idx} = 0;                         $seenSomething = 0;                         next LOOP;                     }                     if ($ctx->{allow_whitespace} and $self->__is_whitespace($ctx, $c)) {                         do {                             $c = $self->__get($ctx, $src);                             last if !defined $c;                         } while $self->__is_whitespace($ctx, $c);                         goto RESTART;                     }                     $waitingForField = 0;                     goto RESTART;                 }                 if ($ctx->{flag} & IS_QUOTED) {                     if (!defined $c or $c =~ /[^\x09\x20-\x7E]/) {                         $ctx->{flag} |= IS_BINARY;                         unless ($ctx->{binary} or $ctx->{utf8}) {                             $self->__error_inside_quotes($ctx, 2026);                             return;                         }                     }                     $$v_ref .= $c;                 } else {                     if (!defined $c or $c =~ /[^\x09\x20-\x7E]/) {                         last if $ctx->{useIO} && !defined $c;                         $ctx->{flag} |= IS_BINARY;                         unless ($ctx->{binary} or $ctx->{utf8}) {                             $self->__error_inside_field($ctx, 2037);                             return;                         }                     }                     $$v_ref .= $c;                 }             }
            ;; if (defined $c and defined $sep and $c eq $sep)
            (pl-if             (pcl:pl-and (pcl:pl-and (pl-defined $c) (pl-defined $sep)) (pcl:pl-str-eq $c $sep))
              (progn
                ;; if ($waitingForField) {                     # ,1,"foo, 3",,bar,                     # ^           ^                     if ($ctx->{blank_is_undef} or $ctx->{empty_is_undef}) {                         $$v_ref = undef;                     } else {                         $$v_ref = "";                     }                     unless ($ctx->{is_bound}) {                         push @$fields, $$v_ref;                     }                     $v_ref = undef;                     if ($ctx->{keep_meta_info} and $fflags) {                         push @$fflags, $ctx->{flag};                     }                 } elsif ($ctx->{flag} & IS_QUOTED) {                     # ,1,"foo, 3",,bar,                     #        ^                     $$v_ref .= $c;                 } else {                     # ,1,"foo, 3",,bar,                     #   ^        ^    ^                     $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                     $v_ref = undef;                     $waitingForField = 1;                 }
                ;; if ($waitingForField)
                (pl-if                 $waitingForField
                  (progn
                    ;; if ($ctx->{blank_is_undef} or $ctx->{empty_is_undef}) {                         $$v_ref = undef;                     } else {                         $$v_ref = "";                     }
                    ;; if ($ctx->{"blank_is_undef"} or $ctx->{"empty_is_undef"})
                    (pl-if                     (pcl:pl-or (pl-gethash-deref $ctx "blank_is_undef") (pl-gethash-deref $ctx "empty_is_undef"))
                      (progn
                        ;; $$v_ref = undef
                                                (pcl:pl-setf (pl-cast-$ $v_ref) (pl-undef))
                        
                      )
                      ;; else
                      (progn
                        ;; $$v_ref = ""
                                                (pcl:pl-setf (pl-cast-$ $v_ref) "")
                        
                      )
                    )
                    
                    ;; unless ($ctx->{is_bound}) {                         push @$fields, $$v_ref;                     }
                    ;; unless ($ctx->{"is_bound"})
                    (pl-if (pl-not                     (pl-gethash-deref $ctx "is_bound"))
                      (progn
                        ;; push @$fields, $$v_ref
                                                (pl-push (pl-cast-@ $fields) (pl-cast-$ $v_ref))
                        
                      )
                      nil
                    )
                    
                    ;; $v_ref = undef
                                        (pcl:pl-setf $v_ref (pl-undef))
                    
                    ;; if ($ctx->{keep_meta_info} and $fflags) {                         push @$fflags, $ctx->{flag};                     }
                    ;; if ($ctx->{"keep_meta_info"} and $fflags)
                    (pl-if                     (pcl:pl-and (pl-gethash-deref $ctx "keep_meta_info") $fflags)
                      (progn
                        ;; push @$fflags, $ctx->{flag}
                                                (pl-push (pl-cast-@ $fflags) (pl-gethash-deref $ctx "flag"))
                        
                      )
                      nil
                    )
                    
                  )
                  ;; elsif ($ctx->{"flag"} & IS_QUOTED)
                  (pl-if                   (pcl:pl-bit-and (pl-gethash-deref $ctx "flag") (pl-IS_QUOTED))
                    (progn
                      ;; $$v_ref .= $c
                                            (pcl:pl-.= (pl-cast-$ $v_ref) $c)
                      
                    )
                    ;; else
                    (progn
                      ;; $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum)
                                            (pl-method-call $self '__push_value $ctx $v_ref $fields $fflags (pl-gethash-deref $ctx "flag") $fnum)
                      
                      ;; $v_ref = undef
                                            (pcl:pl-setf $v_ref (pl-undef))
                      
                      ;; $waitingForField = 1
                                            (pcl:pl-setf $waitingForField 1)
                      
                    )
                  )
                )
                
              )
              ;; elsif (defined $c and defined $quot and $quot ne "\0" and $c eq $quot)
              (pl-if               (pcl:pl-and (pcl:pl-and (pcl:pl-and (pl-defined $c) (pl-defined $quot)) (pcl:pl-str-ne $quot " ")) (pcl:pl-str-eq $c $quot))
                (progn
                  ;; if ($waitingForField) {                     # ,1,"foo, 3",,bar,\r\n                     #    ^                     $ctx->{flag} |= IS_QUOTED;                     $waitingForField = 0;                     next;                 }
                  ;; if ($waitingForField)
                  (pl-if                   $waitingForField
                    (progn
                      ;; $ctx->{flag} |= IS_QUOTED
                                            (pcl:pl-bit-or= (pl-gethash-deref $ctx "flag") (pl-IS_QUOTED))
                      
                      ;; $waitingForField = 0
                                            (pcl:pl-setf $waitingForField 0)
                      
                      ;; next
                                            (pl-next)
                      
                    )
                    nil
                  )
                  
                  ;; if ($ctx->{flag} & IS_QUOTED) {                     # ,1,"foo, 3",,bar,\r\n                     #           ^                     my $quoesc = 0;                     my $c2 = $self->__get($ctx, $src);                      if ($ctx->{allow_whitespace}) {                         # , 1 , "foo, 3" , , bar , \r\n                         #               ^                         while($self->__is_whitespace($ctx, $c2)) {                             if ($ctx->{allow_loose_quotes} and !(defined $esc and $c2 eq $esc)) {                                 $$v_ref .= $c;                                 $c = $c2;                             }                             $c2 = $self->__get($ctx, $src);                         }                     }                      if (!defined $c2) { # EOF                         # ,1,"foo, 3"                         #            ^                         $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                         return 1;                     }                      if (defined $c2 and defined $sep and $c2 eq $sep) {                         # ,1,"foo, 3",,bar,\r\n                         #            ^                         $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                         $v_ref = undef;                         $waitingForField = 1;                         next;                     }                     if (defined $c2 and ($c2 eq "\012" or (defined $eol and $c2 eq $eol))) { # FIXME: EOLX                         # ,1,"foo, 3",,"bar"\n                         #                   ^                         $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                         return 1;                     }                      if (defined $esc and $c eq $esc) {                         $quoesc = 1;                         if (defined $c2 and $c2 eq '0') {                             # ,1,"foo, 3"056",,bar,\r\n                             #            ^                             $$v_ref .= "\0";                             next;                         }                         if (defined $c2 and defined $quot and $c2 eq $quot) {                             # ,1,"foo, 3""56",,bar,\r\n                             #            ^                             if ($ctx->{utf8}) {                                 $ctx->{flag} |= IS_BINARY;                             }                             $$v_ref .= $c2;                             next;                         }                         if ($ctx->{allow_loose_escapes} and defined $c2 and $c2 ne "\015") {                             # ,1,"foo, 3"56",,bar,\r\n                             #            ^                             $$v_ref .= $c;                             $c = $c2;                             goto RESTART;                         }                     }                     if (defined $c2 and $c2 eq "\015") {                         if ($ctx->{eol_is_cr}) {                             # ,1,"foo, 3"\r                             #            ^                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                          my $c3 = $self->__get($ctx, $src);                         if (defined $c3 and $c3 eq "\012") {                             # ,1,"foo, 3"\r\n                             #              ^                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                          if ($ctx->{useIO} and !$ctx->{eol_len}) {                             if ($c3 eq "\015") { # \r followed by an empty line                                 # ,1,"foo, 3"\r\r                                 #              ^                                 $self->__set_eol_is_cr($ctx);                                 goto EOLX;                             }                             if ($c3 !~ /[^\x09\x20-\x7E]/) {                                 # ,1,"foo\n 3",,"bar"\r                                 # baz,4                                 # ^                                 $self->__set_eol_is_cr($ctx);                                 $ctx->{used}--;                                 $ctx->{has_ahead} = 1;                                 $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                                 return 1;                             }                         }                          $self->__parse_error($ctx, $quoesc ? 2023 : 2010, $ctx->{used} - 2);                         return;                     }                      if ($ctx->{allow_loose_quotes} and !$quoesc) {                         # ,1,"foo, 3"456",,bar,\r\n                         #            ^                         $$v_ref .= $c;                         $c = $c2;                         goto RESTART;                     }                     # 1,"foo" ",3                     #        ^                     if ($quoesc) {                         $ctx->{used}--;                         $self->__error_inside_quotes($ctx, 2023);                         return;                     }                     $self->__error_inside_quotes($ctx, 2011);                     return;                 }
                  ;; if ($ctx->{"flag"} & IS_QUOTED)
                  (pl-if                   (pcl:pl-bit-and (pl-gethash-deref $ctx "flag") (pl-IS_QUOTED))
                    (progn
                      ;; my $quoesc = 0
                                            (pcl:pl-setf $quoesc 0)
                      
                      ;; my $c2 = $self->__get($ctx, $src)
                                            (pcl:pl-setf $c2 (pl-method-call $self '__get $ctx $src))
                      
                      ;; if ($ctx->{allow_whitespace}) {                         # , 1 , "foo, 3" , , bar , \r\n                         #               ^                         while($self->__is_whitespace($ctx, $c2)) {                             if ($ctx->{allow_loose_quotes} and !(defined $esc and $c2 eq $esc)) {                                 $$v_ref .= $c;                                 $c = $c2;                             }                             $c2 = $self->__get($ctx, $src);                         }                     }
                      ;; if ($ctx->{"allow_whitespace"})
                      (pl-if                       (pl-gethash-deref $ctx "allow_whitespace")
                        (progn
                          ;; while($self->__is_whitespace($ctx, $c2)) {                             if ($ctx->{allow_loose_quotes} and !(defined $esc and $c2 eq $esc)) {                                 $$v_ref .= $c;                                 $c = $c2;                             }                             $c2 = $self->__get($ctx, $src);                         }
                          (pl-while                           (pl-method-call $self '__is_whitespace $ctx $c2)
                            ;; if ($ctx->{allow_loose_quotes} and !(defined $esc and $c2 eq $esc)) {                                 $$v_ref .= $c;                                 $c = $c2;                             }
                            ;; if ($ctx->{"allow_loose_quotes"} and !(defined $esc and $c2 eq $esc))
                            (pl-if                             (pcl:pl-and (pl-gethash-deref $ctx "allow_loose_quotes") (pl-! (pcl:pl-and (pl-defined $esc) (pcl:pl-str-eq $c2 $esc))))
                              (progn
                                ;; $$v_ref .= $c
                                                                (pcl:pl-.= (pl-cast-$ $v_ref) $c)
                                
                                ;; $c = $c2
                                                                (pcl:pl-setf $c $c2)
                                
                              )
                              nil
                            )
                            
                            ;; $c2 = $self->__get($ctx, $src)
                                                        (pcl:pl-setf $c2 (pl-method-call $self '__get $ctx $src))
                            
                          )
                          
                        )
                        nil
                      )
                      
                      ;; if (!defined $c2) { # EOF                         # ,1,"foo, 3"                         #            ^                         $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                         return 1;                     }
                      ;; if (!defined $c2)
                      (pl-if                       (pl-! (pl-defined $c2))
                        (progn
                          ;; $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum)
                                                    (pl-method-call $self '__push_value $ctx $v_ref $fields $fflags (pl-gethash-deref $ctx "flag") $fnum)
                          
                          ;; return 1
                                                    (pl-return 1)
                          
                        )
                        nil
                      )
                      
                      ;; if (defined $c2 and defined $sep and $c2 eq $sep) {                         # ,1,"foo, 3",,bar,\r\n                         #            ^                         $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                         $v_ref = undef;                         $waitingForField = 1;                         next;                     }
                      ;; if (defined $c2 and defined $sep and $c2 eq $sep)
                      (pl-if                       (pcl:pl-and (pcl:pl-and (pl-defined $c2) (pl-defined $sep)) (pcl:pl-str-eq $c2 $sep))
                        (progn
                          ;; $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum)
                                                    (pl-method-call $self '__push_value $ctx $v_ref $fields $fflags (pl-gethash-deref $ctx "flag") $fnum)
                          
                          ;; $v_ref = undef
                                                    (pcl:pl-setf $v_ref (pl-undef))
                          
                          ;; $waitingForField = 1
                                                    (pcl:pl-setf $waitingForField 1)
                          
                          ;; next
                                                    (pl-next)
                          
                        )
                        nil
                      )
                      
                      ;; if (defined $c2 and ($c2 eq "\012" or (defined $eol and $c2 eq $eol))) { # FIXME: EOLX                         # ,1,"foo, 3",,"bar"\n                         #                   ^                         $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                         return 1;                     }
                      ;; if (defined $c2 and ($c2 eq "\012" or (defined $eol and $c2 eq $eol)))
                      (pl-if                       (pcl:pl-and (pl-defined $c2) (pcl:pl-or (pcl:pl-str-eq $c2 " 12") (pcl:pl-and (pl-defined $eol) (pcl:pl-str-eq $c2 $eol))))
                        (progn
                          ;; $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum)
                                                    (pl-method-call $self '__push_value $ctx $v_ref $fields $fflags (pl-gethash-deref $ctx "flag") $fnum)
                          
                          ;; return 1
                                                    (pl-return 1)
                          
                        )
                        nil
                      )
                      
                      ;; if (defined $esc and $c eq $esc) {                         $quoesc = 1;                         if (defined $c2 and $c2 eq '0') {                             # ,1,"foo, 3"056",,bar,\r\n                             #            ^                             $$v_ref .= "\0";                             next;                         }                         if (defined $c2 and defined $quot and $c2 eq $quot) {                             # ,1,"foo, 3""56",,bar,\r\n                             #            ^                             if ($ctx->{utf8}) {                                 $ctx->{flag} |= IS_BINARY;                             }                             $$v_ref .= $c2;                             next;                         }                         if ($ctx->{allow_loose_escapes} and defined $c2 and $c2 ne "\015") {                             # ,1,"foo, 3"56",,bar,\r\n                             #            ^                             $$v_ref .= $c;                             $c = $c2;                             goto RESTART;                         }                     }
                      ;; if (defined $esc and $c eq $esc)
                      (pl-if                       (pcl:pl-and (pl-defined $esc) (pcl:pl-str-eq $c $esc))
                        (progn
                          ;; $quoesc = 1
                                                    (pcl:pl-setf $quoesc 1)
                          
                          ;; if (defined $c2 and $c2 eq '0') {                             # ,1,"foo, 3"056",,bar,\r\n                             #            ^                             $$v_ref .= "\0";                             next;                         }
                          ;; if (defined $c2 and $c2 eq '0')
                          (pl-if                           (pcl:pl-and (pl-defined $c2) (pcl:pl-str-eq $c2 "0"))
                            (progn
                              ;; $$v_ref .= "\0"
                                                            (pcl:pl-.= (pl-cast-$ $v_ref) " ")
                              
                              ;; next
                                                            (pl-next)
                              
                            )
                            nil
                          )
                          
                          ;; if (defined $c2 and defined $quot and $c2 eq $quot) {                             # ,1,"foo, 3""56",,bar,\r\n                             #            ^                             if ($ctx->{utf8}) {                                 $ctx->{flag} |= IS_BINARY;                             }                             $$v_ref .= $c2;                             next;                         }
                          ;; if (defined $c2 and defined $quot and $c2 eq $quot)
                          (pl-if                           (pcl:pl-and (pcl:pl-and (pl-defined $c2) (pl-defined $quot)) (pcl:pl-str-eq $c2 $quot))
                            (progn
                              ;; if ($ctx->{utf8}) {                                 $ctx->{flag} |= IS_BINARY;                             }
                              ;; if ($ctx->{"utf8"})
                              (pl-if                               (pl-gethash-deref $ctx "utf8")
                                (progn
                                  ;; $ctx->{flag} |= IS_BINARY
                                                                    (pcl:pl-bit-or= (pl-gethash-deref $ctx "flag") (pl-IS_BINARY))
                                  
                                )
                                nil
                              )
                              
                              ;; $$v_ref .= $c2
                                                            (pcl:pl-.= (pl-cast-$ $v_ref) $c2)
                              
                              ;; next
                                                            (pl-next)
                              
                            )
                            nil
                          )
                          
                          ;; if ($ctx->{allow_loose_escapes} and defined $c2 and $c2 ne "\015") {                             # ,1,"foo, 3"56",,bar,\r\n                             #            ^                             $$v_ref .= $c;                             $c = $c2;                             goto RESTART;                         }
                          ;; if ($ctx->{"allow_loose_escapes"} and defined $c2 and $c2 ne "\015")
                          (pl-if                           (pcl:pl-and (pcl:pl-and (pl-gethash-deref $ctx "allow_loose_escapes") (pl-defined $c2)) (pcl:pl-str-ne $c2 " 15"))
                            (progn
                              ;; $$v_ref .= $c
                                                            (pcl:pl-.= (pl-cast-$ $v_ref) $c)
                              
                              ;; $c = $c2
                                                            (pcl:pl-setf $c $c2)
                              
                              ;; goto RESTART
                                                            (pl-goto (pl-RESTART))
                              
                            )
                            nil
                          )
                          
                        )
                        nil
                      )
                      
                      ;; if (defined $c2 and $c2 eq "\015") {                         if ($ctx->{eol_is_cr}) {                             # ,1,"foo, 3"\r                             #            ^                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                          my $c3 = $self->__get($ctx, $src);                         if (defined $c3 and $c3 eq "\012") {                             # ,1,"foo, 3"\r\n                             #              ^                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                          if ($ctx->{useIO} and !$ctx->{eol_len}) {                             if ($c3 eq "\015") { # \r followed by an empty line                                 # ,1,"foo, 3"\r\r                                 #              ^                                 $self->__set_eol_is_cr($ctx);                                 goto EOLX;                             }                             if ($c3 !~ /[^\x09\x20-\x7E]/) {                                 # ,1,"foo\n 3",,"bar"\r                                 # baz,4                                 # ^                                 $self->__set_eol_is_cr($ctx);                                 $ctx->{used}--;                                 $ctx->{has_ahead} = 1;                                 $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                                 return 1;                             }                         }                          $self->__parse_error($ctx, $quoesc ? 2023 : 2010, $ctx->{used} - 2);                         return;                     }
                      ;; if (defined $c2 and $c2 eq "\015")
                      (pl-if                       (pcl:pl-and (pl-defined $c2) (pcl:pl-str-eq $c2 " 15"))
                        (progn
                          ;; if ($ctx->{eol_is_cr}) {                             # ,1,"foo, 3"\r                             #            ^                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }
                          ;; if ($ctx->{"eol_is_cr"})
                          (pl-if                           (pl-gethash-deref $ctx "eol_is_cr")
                            (progn
                              ;; $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum)
                                                            (pl-method-call $self '__push_value $ctx $v_ref $fields $fflags (pl-gethash-deref $ctx "flag") $fnum)
                              
                              ;; return 1
                                                            (pl-return 1)
                              
                            )
                            nil
                          )
                          
                          ;; my $c3 = $self->__get($ctx, $src)
                                                    (pcl:pl-setf $c3 (pl-method-call $self '__get $ctx $src))
                          
                          ;; if (defined $c3 and $c3 eq "\012") {                             # ,1,"foo, 3"\r\n                             #              ^                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }
                          ;; if (defined $c3 and $c3 eq "\012")
                          (pl-if                           (pcl:pl-and (pl-defined $c3) (pcl:pl-str-eq $c3 " 12"))
                            (progn
                              ;; $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum)
                                                            (pl-method-call $self '__push_value $ctx $v_ref $fields $fflags (pl-gethash-deref $ctx "flag") $fnum)
                              
                              ;; return 1
                                                            (pl-return 1)
                              
                            )
                            nil
                          )
                          
                          ;; if ($ctx->{useIO} and !$ctx->{eol_len}) {                             if ($c3 eq "\015") { # \r followed by an empty line                                 # ,1,"foo, 3"\r\r                                 #              ^                                 $self->__set_eol_is_cr($ctx);                                 goto EOLX;                             }                             if ($c3 !~ /[^\x09\x20-\x7E]/) {                                 # ,1,"foo\n 3",,"bar"\r                                 # baz,4                                 # ^                                 $self->__set_eol_is_cr($ctx);                                 $ctx->{used}--;                                 $ctx->{has_ahead} = 1;                                 $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                                 return 1;                             }                         }
                          ;; if ($ctx->{"useIO"} and !$ctx->{"eol_len"})
                          (pl-if                           (pcl:pl-and (pl-gethash-deref $ctx "useIO") (pl-! (pl-gethash-deref $ctx "eol_len")))
                            (progn
                              ;; if ($c3 eq "\015") { # \r followed by an empty line                                 # ,1,"foo, 3"\r\r                                 #              ^                                 $self->__set_eol_is_cr($ctx);                                 goto EOLX;                             }
                              ;; if ($c3 eq "\015")
                              (pl-if                               (pcl:pl-str-eq $c3 " 15")
                                (progn
                                  ;; $self->__set_eol_is_cr($ctx)
                                                                    (pl-method-call $self '__set_eol_is_cr $ctx)
                                  
                                  ;; goto EOLX
                                                                    (pl-goto (pl-EOLX))
                                  
                                )
                                nil
                              )
                              
                              ;; if ($c3 !~ /[^\x09\x20-\x7E]/) {                                 # ,1,"foo\n 3",,"bar"\r                                 # baz,4                                 # ^                                 $self->__set_eol_is_cr($ctx);                                 $ctx->{used}--;                                 $ctx->{has_ahead} = 1;                                 $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                                 return 1;                             }
                              ;; if ($c3 !~ /[^\x09\x20-\x7E]/)
                              (pl-if                               (pcl:pl-!~ $c3 (pl-regex "/[^\\x09\\x20-\\x7E]/"))
                                (progn
                                  ;; $self->__set_eol_is_cr($ctx)
                                                                    (pl-method-call $self '__set_eol_is_cr $ctx)
                                  
                                  ;; $ctx->{used}--
                                                                    (pl-post-- (pl-gethash-deref $ctx "used"))
                                  
                                  ;; $ctx->{has_ahead} = 1
                                                                    (pcl:pl-setf (pl-gethash-deref $ctx "has_ahead") 1)
                                  
                                  ;; $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum)
                                                                    (pl-method-call $self '__push_value $ctx $v_ref $fields $fflags (pl-gethash-deref $ctx "flag") $fnum)
                                  
                                  ;; return 1
                                                                    (pl-return 1)
                                  
                                )
                                nil
                              )
                              
                            )
                            nil
                          )
                          
                          ;; $self->__parse_error($ctx, $quoesc ? 2023 : 2010, $ctx->{used} - 2)
                                                    (pl-method-call $self '__parse_error $ctx (pl-if $quoesc 2023 2010) (pcl:pl-- (pl-gethash-deref $ctx "used") 2))
                          
                          ;; return
                                                    (pl-return)
                          
                        )
                        nil
                      )
                      
                      ;; if ($ctx->{allow_loose_quotes} and !$quoesc) {                         # ,1,"foo, 3"456",,bar,\r\n                         #            ^                         $$v_ref .= $c;                         $c = $c2;                         goto RESTART;                     }
                      ;; if ($ctx->{"allow_loose_quotes"} and !$quoesc)
                      (pl-if                       (pcl:pl-and (pl-gethash-deref $ctx "allow_loose_quotes") (pl-! $quoesc))
                        (progn
                          ;; $$v_ref .= $c
                                                    (pcl:pl-.= (pl-cast-$ $v_ref) $c)
                          
                          ;; $c = $c2
                                                    (pcl:pl-setf $c $c2)
                          
                          ;; goto RESTART
                                                    (pl-goto (pl-RESTART))
                          
                        )
                        nil
                      )
                      
                      ;; if ($quoesc) {                         $ctx->{used}--;                         $self->__error_inside_quotes($ctx, 2023);                         return;                     }
                      ;; if ($quoesc)
                      (pl-if                       $quoesc
                        (progn
                          ;; $ctx->{used}--
                                                    (pl-post-- (pl-gethash-deref $ctx "used"))
                          
                          ;; $self->__error_inside_quotes($ctx, 2023)
                                                    (pl-method-call $self '__error_inside_quotes $ctx 2023)
                          
                          ;; return
                                                    (pl-return)
                          
                        )
                        nil
                      )
                      
                      ;; $self->__error_inside_quotes($ctx, 2011)
                                            (pl-method-call $self '__error_inside_quotes $ctx 2011)
                      
                      ;; return
                                            (pl-return)
                      
                    )
                    nil
                  )
                  
                  ;; if ($ctx->{allow_loose_quotes}) { # 1,foo "boo" d'uh,1                     $ctx->{flag} |= IS_ERROR;                     $$v_ref .= $c;                 } else {                     $self->__error_inside_field($ctx, 2034);                     return;                 }
                  ;; if ($ctx->{"allow_loose_quotes"})
                  (pl-if                   (pl-gethash-deref $ctx "allow_loose_quotes")
                    (progn
                      ;; $ctx->{flag} |= IS_ERROR
                                            (pcl:pl-bit-or= (pl-gethash-deref $ctx "flag") (pl-IS_ERROR))
                      
                      ;; $$v_ref .= $c
                                            (pcl:pl-.= (pl-cast-$ $v_ref) $c)
                      
                    )
                    ;; else
                    (progn
                      ;; $self->__error_inside_field($ctx, 2034)
                                            (pl-method-call $self '__error_inside_field $ctx 2034)
                      
                      ;; return
                                            (pl-return)
                      
                    )
                  )
                  
                )
                ;; elsif (defined $c and defined $esc and $esc ne "\0" and $c eq $esc)
                (pl-if                 (pcl:pl-and (pcl:pl-and (pcl:pl-and (pl-defined $c) (pl-defined $esc)) (pcl:pl-str-ne $esc " ")) (pcl:pl-str-eq $c $esc))
                  (progn
                    ;; if ($waitingForField) {                     $waitingForField = 0;                     if ($ctx->{allow_unquoted_escape}) {                         # The escape character is the first character of an                         # unquoted field                         # ... get and store next character                         my $c2 = $self->__get($ctx, $src);                         $$v_ref = "";                          if (!defined $c2) { # EOF                             $ctx->{used}--;                             $self->__error_inside_field($ctx, 2035);                             return;                         }                         if ($c2 eq '0') {                             $$v_ref .= "\0";                         }                         elsif (                             (defined $quot and $c2 eq $quot) or                             (defined $sep and $c2 eq $sep) or                             (defined $esc and $c2 eq $esc) or                             $ctx->{allow_loose_escapes}                         ) {                             if ($ctx->{utf8}) {                                 $ctx->{flag} |= IS_BINARY;                             }                             $$v_ref .= $c2;                         } else {                             $self->__parse_inside_quotes($ctx, 2025);                             return;                         }                     }                 }                 elsif ($ctx->{flag} & IS_QUOTED) {                     my $c2 = $self->__get($ctx, $src);                     if (!defined $c2) { # EOF                         $ctx->{used}--;                         $self->__error_inside_quotes($ctx, 2024);                         return;                     }                     if ($c2 eq '0') {                         $$v_ref .= "\0";                     }                     elsif (                         (defined $quot and $c2 eq $quot) or                         (defined $sep and $c2 eq $sep) or                         (defined $esc and $c2 eq $esc) or                         $ctx->{allow_loose_escapes}                     ) {                         if ($ctx->{utf8}) {                             $ctx->{flag} |= IS_BINARY;                         }                         $$v_ref .= $c2;                     } else {                         $ctx->{used}--;                         $self->__error_inside_quotes($ctx, 2025);                         return;                     }                 }                 elsif ($v_ref) {                     my $c2 = $self->__get($ctx, $src);                     if (!defined $c2) { # EOF                         $ctx->{used}--;                         $self->__error_inside_field($ctx, 2035);                         return;                     }                     $$v_ref .= $c2;                 }                 else {                     $self->__error_inside_field($ctx, 2036);                     return;                 }
                    ;; if ($waitingForField)
                    (pl-if                     $waitingForField
                      (progn
                        ;; $waitingForField = 0
                                                (pcl:pl-setf $waitingForField 0)
                        
                        ;; if ($ctx->{allow_unquoted_escape}) {                         # The escape character is the first character of an                         # unquoted field                         # ... get and store next character                         my $c2 = $self->__get($ctx, $src);                         $$v_ref = "";                          if (!defined $c2) { # EOF                             $ctx->{used}--;                             $self->__error_inside_field($ctx, 2035);                             return;                         }                         if ($c2 eq '0') {                             $$v_ref .= "\0";                         }                         elsif (                             (defined $quot and $c2 eq $quot) or                             (defined $sep and $c2 eq $sep) or                             (defined $esc and $c2 eq $esc) or                             $ctx->{allow_loose_escapes}                         ) {                             if ($ctx->{utf8}) {                                 $ctx->{flag} |= IS_BINARY;                             }                             $$v_ref .= $c2;                         } else {                             $self->__parse_inside_quotes($ctx, 2025);                             return;                         }                     }
                        ;; if ($ctx->{"allow_unquoted_escape"})
                        (pl-if                         (pl-gethash-deref $ctx "allow_unquoted_escape")
                          (progn
                            ;; my $c2 = $self->__get($ctx, $src)
                                                        (pcl:pl-setf $c2 (pl-method-call $self '__get $ctx $src))
                            
                            ;; $$v_ref = ""
                                                        (pcl:pl-setf (pl-cast-$ $v_ref) "")
                            
                            ;; if (!defined $c2) { # EOF                             $ctx->{used}--;                             $self->__error_inside_field($ctx, 2035);                             return;                         }
                            ;; if (!defined $c2)
                            (pl-if                             (pl-! (pl-defined $c2))
                              (progn
                                ;; $ctx->{used}--
                                                                (pl-post-- (pl-gethash-deref $ctx "used"))
                                
                                ;; $self->__error_inside_field($ctx, 2035)
                                                                (pl-method-call $self '__error_inside_field $ctx 2035)
                                
                                ;; return
                                                                (pl-return)
                                
                              )
                              nil
                            )
                            
                            ;; if ($c2 eq '0') {                             $$v_ref .= "\0";                         }                         elsif (                             (defined $quot and $c2 eq $quot) or                             (defined $sep and $c2 eq $sep) or                             (defined $esc and $c2 eq $esc) or                             $ctx->{allow_loose_escapes}                         ) {                             if ($ctx->{utf8}) {                                 $ctx->{flag} |= IS_BINARY;                             }                             $$v_ref .= $c2;                         } else {                             $self->__parse_inside_quotes($ctx, 2025);                             return;                         }
                            ;; if ($c2 eq '0')
                            (pl-if                             (pcl:pl-str-eq $c2 "0")
                              (progn
                                ;; $$v_ref .= "\0"
                                                                (pcl:pl-.= (pl-cast-$ $v_ref) " ")
                                
                              )
                              ;; elsif ((defined $quot and $c2 eq $quot) or
;;                             (defined $sep and $c2 eq $sep) or
;;                             (defined $esc and $c2 eq $esc) or
;;                             $ctx->{"allow_loose_escapes"})
                              (pl-if                               (pcl:pl-or (pcl:pl-or (pcl:pl-or (pcl:pl-and (pl-defined $quot) (pcl:pl-str-eq $c2 $quot)) (pcl:pl-and (pl-defined $sep) (pcl:pl-str-eq $c2 $sep))) (pcl:pl-and (pl-defined $esc) (pcl:pl-str-eq $c2 $esc))) (pl-gethash-deref $ctx "allow_loose_escapes"))
                                (progn
                                  ;; if ($ctx->{utf8}) {                                 $ctx->{flag} |= IS_BINARY;                             }
                                  ;; if ($ctx->{"utf8"})
                                  (pl-if                                   (pl-gethash-deref $ctx "utf8")
                                    (progn
                                      ;; $ctx->{flag} |= IS_BINARY
                                                                            (pcl:pl-bit-or= (pl-gethash-deref $ctx "flag") (pl-IS_BINARY))
                                      
                                    )
                                    nil
                                  )
                                  
                                  ;; $$v_ref .= $c2
                                                                    (pcl:pl-.= (pl-cast-$ $v_ref) $c2)
                                  
                                )
                                ;; else
                                (progn
                                  ;; $self->__parse_inside_quotes($ctx, 2025)
                                                                    (pl-method-call $self '__parse_inside_quotes $ctx 2025)
                                  
                                  ;; return
                                                                    (pl-return)
                                  
                                )
                              )
                            )
                            
                          )
                          nil
                        )
                        
                      )
                      ;; elsif ($ctx->{"flag"} & IS_QUOTED)
                      (pl-if                       (pcl:pl-bit-and (pl-gethash-deref $ctx "flag") (pl-IS_QUOTED))
                        (progn
                          ;; my $c2 = $self->__get($ctx, $src)
                                                    (pcl:pl-setf $c2 (pl-method-call $self '__get $ctx $src))
                          
                          ;; if (!defined $c2) { # EOF                         $ctx->{used}--;                         $self->__error_inside_quotes($ctx, 2024);                         return;                     }
                          ;; if (!defined $c2)
                          (pl-if                           (pl-! (pl-defined $c2))
                            (progn
                              ;; $ctx->{used}--
                                                            (pl-post-- (pl-gethash-deref $ctx "used"))
                              
                              ;; $self->__error_inside_quotes($ctx, 2024)
                                                            (pl-method-call $self '__error_inside_quotes $ctx 2024)
                              
                              ;; return
                                                            (pl-return)
                              
                            )
                            nil
                          )
                          
                          ;; if ($c2 eq '0') {                         $$v_ref .= "\0";                     }                     elsif (                         (defined $quot and $c2 eq $quot) or                         (defined $sep and $c2 eq $sep) or                         (defined $esc and $c2 eq $esc) or                         $ctx->{allow_loose_escapes}                     ) {                         if ($ctx->{utf8}) {                             $ctx->{flag} |= IS_BINARY;                         }                         $$v_ref .= $c2;                     } else {                         $ctx->{used}--;                         $self->__error_inside_quotes($ctx, 2025);                         return;                     }
                          ;; if ($c2 eq '0')
                          (pl-if                           (pcl:pl-str-eq $c2 "0")
                            (progn
                              ;; $$v_ref .= "\0"
                                                            (pcl:pl-.= (pl-cast-$ $v_ref) " ")
                              
                            )
                            ;; elsif ((defined $quot and $c2 eq $quot) or
;;                         (defined $sep and $c2 eq $sep) or
;;                         (defined $esc and $c2 eq $esc) or
;;                         $ctx->{"allow_loose_escapes"})
                            (pl-if                             (pcl:pl-or (pcl:pl-or (pcl:pl-or (pcl:pl-and (pl-defined $quot) (pcl:pl-str-eq $c2 $quot)) (pcl:pl-and (pl-defined $sep) (pcl:pl-str-eq $c2 $sep))) (pcl:pl-and (pl-defined $esc) (pcl:pl-str-eq $c2 $esc))) (pl-gethash-deref $ctx "allow_loose_escapes"))
                              (progn
                                ;; if ($ctx->{utf8}) {                             $ctx->{flag} |= IS_BINARY;                         }
                                ;; if ($ctx->{"utf8"})
                                (pl-if                                 (pl-gethash-deref $ctx "utf8")
                                  (progn
                                    ;; $ctx->{flag} |= IS_BINARY
                                                                        (pcl:pl-bit-or= (pl-gethash-deref $ctx "flag") (pl-IS_BINARY))
                                    
                                  )
                                  nil
                                )
                                
                                ;; $$v_ref .= $c2
                                                                (pcl:pl-.= (pl-cast-$ $v_ref) $c2)
                                
                              )
                              ;; else
                              (progn
                                ;; $ctx->{used}--
                                                                (pl-post-- (pl-gethash-deref $ctx "used"))
                                
                                ;; $self->__error_inside_quotes($ctx, 2025)
                                                                (pl-method-call $self '__error_inside_quotes $ctx 2025)
                                
                                ;; return
                                                                (pl-return)
                                
                              )
                            )
                          )
                          
                        )
                        ;; elsif ($v_ref)
                        (pl-if                         $v_ref
                          (progn
                            ;; my $c2 = $self->__get($ctx, $src)
                                                        (pcl:pl-setf $c2 (pl-method-call $self '__get $ctx $src))
                            
                            ;; if (!defined $c2) { # EOF                         $ctx->{used}--;                         $self->__error_inside_field($ctx, 2035);                         return;                     }
                            ;; if (!defined $c2)
                            (pl-if                             (pl-! (pl-defined $c2))
                              (progn
                                ;; $ctx->{used}--
                                                                (pl-post-- (pl-gethash-deref $ctx "used"))
                                
                                ;; $self->__error_inside_field($ctx, 2035)
                                                                (pl-method-call $self '__error_inside_field $ctx 2035)
                                
                                ;; return
                                                                (pl-return)
                                
                              )
                              nil
                            )
                            
                            ;; $$v_ref .= $c2
                                                        (pcl:pl-.= (pl-cast-$ $v_ref) $c2)
                            
                          )
                          ;; else
                          (progn
                            ;; $self->__error_inside_field($ctx, 2036)
                                                        (pl-method-call $self '__error_inside_field $ctx 2036)
                            
                            ;; return
                                                        (pl-return)
                            
                          )
                        )
                      )
                    )
                    
                  )
                  ;; elsif (defined $c and ($c eq "\012" or $c eq '' or (defined $eol and $c eq $eol and $eol ne "\015")))
                  (pl-if                   (pcl:pl-and (pl-defined $c) (pcl:pl-or (pcl:pl-or (pcl:pl-str-eq $c " 12") (pcl:pl-str-eq $c "")) (pcl:pl-and (pcl:pl-and (pl-defined $eol) (pcl:pl-str-eq $c $eol)) (pcl:pl-str-ne $eol " 15"))))
                    (progn
                      ;; COMPOUND () not yet implemented:
                      ;; EOLX:
                      
                      ;; if ($fnum == 1 && $ctx->{flag} == 0 && (!$v_ref || $$v_ref eq '') && $ctx->{skip_empty_rows}) {                     ### SkipEmptyRow                     my $ser = $ctx->{skip_empty_rows};                     if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }                     if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }                     if ($ser == 5) { $self->SetDiag(2015); return undef; }                      if ($ser <= 2) { # skip & eof                         $ctx->{fld_idx} = 0;                         $c = $self->__get($ctx, $src);                         if (!defined $c or $ser == 2) {  # EOF                             $v_ref = undef;                             $seenSomething = 0;                             if ($ser == 2) { return undef; }                             last LOOP;                         }                     }                      if ($ser == 6) {                         my $cb = $self->{_EMPTROW_CB};                         unless ($cb && ref $cb eq 'CODE') {                             return undef;  # A callback is wanted, but none found                         }                         local $_ = $v_ref;                         my $rv = $cb->();                         # Result should be a ref to a list.                         unless (ref $rv eq 'ARRAY') {                             return undef;                         }                         my $n = @$rv;                         if ($n <= 0) {                             return 1;                         }                         if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                             $n = $ctx->{is_bound} - 1;                         }                         for (my $i = 0; $i < $n; $i++) {                             my $rvi = $rv->[$i];                             $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                         }                         return 1;                     }                     goto RESTART;                 }
                      ;; if ($fnum == 1 && $ctx->{"flag"} == 0 && (!$v_ref || $$v_ref eq '') && $ctx->{"skip_empty_rows"})
                      (pl-if                       (pcl:pl-&& (pcl:pl-&& (pcl:pl-&& (pcl:pl-== $fnum 1) (pcl:pl-== (pl-gethash-deref $ctx "flag") 0)) (pcl:pl-|| (pl-! $v_ref) (pcl:pl-str-eq (pl-cast-$ $v_ref) ""))) (pl-gethash-deref $ctx "skip_empty_rows"))
                        (progn
                          ;; my $ser = $ctx->{skip_empty_rows}
                                                    (pcl:pl-setf $ser (pl-gethash-deref $ctx "skip_empty_rows"))
                          
                          ;; if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }
                          ;; if ($ser == 3)
                          (pl-if                           (pcl:pl-== $ser 3)
                            (progn
                              ;; $self->SetDiag(2015)
                                                            (pl-method-call $self 'SetDiag 2015)
                              
                              ;; die "Empty row\n"
                                                            (pl-die "Empty row
")
                              
                            )
                            nil
                          )
                          
                          ;; if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }
                          ;; if ($ser == 4)
                          (pl-if                           (pcl:pl-== $ser 4)
                            (progn
                              ;; $self->SetDiag(2015)
                                                            (pl-method-call $self 'SetDiag 2015)
                              
                              ;; die "Empty row\n"
                                                            (pl-die "Empty row
")
                              
                            )
                            nil
                          )
                          
                          ;; if ($ser == 5) { $self->SetDiag(2015); return undef; }
                          ;; if ($ser == 5)
                          (pl-if                           (pcl:pl-== $ser 5)
                            (progn
                              ;; $self->SetDiag(2015)
                                                            (pl-method-call $self 'SetDiag 2015)
                              
                              ;; return undef
                                                            (pl-return (pl-undef))
                              
                            )
                            nil
                          )
                          
                          ;; if ($ser <= 2) { # skip & eof                         $ctx->{fld_idx} = 0;                         $c = $self->__get($ctx, $src);                         if (!defined $c or $ser == 2) {  # EOF                             $v_ref = undef;                             $seenSomething = 0;                             if ($ser == 2) { return undef; }                             last LOOP;                         }                     }
                          ;; if ($ser <= 2)
                          (pl-if                           (pcl:pl-<= $ser 2)
                            (progn
                              ;; $ctx->{fld_idx} = 0
                                                            (pcl:pl-setf (pl-gethash-deref $ctx "fld_idx") 0)
                              
                              ;; $c = $self->__get($ctx, $src)
                                                            (pcl:pl-setf $c (pl-method-call $self '__get $ctx $src))
                              
                              ;; if (!defined $c or $ser == 2) {  # EOF                             $v_ref = undef;                             $seenSomething = 0;                             if ($ser == 2) { return undef; }                             last LOOP;                         }
                              ;; if (!defined $c or $ser == 2)
                              (pl-if                               (pcl:pl-or (pl-! (pl-defined $c)) (pcl:pl-== $ser 2))
                                (progn
                                  ;; $v_ref = undef
                                                                    (pcl:pl-setf $v_ref (pl-undef))
                                  
                                  ;; $seenSomething = 0
                                                                    (pcl:pl-setf $seenSomething 0)
                                  
                                  ;; if ($ser == 2) { return undef; }
                                  ;; if ($ser == 2)
                                  (pl-if                                   (pcl:pl-== $ser 2)
                                    (progn
                                      ;; return undef
                                                                            (pl-return (pl-undef))
                                      
                                    )
                                    nil
                                  )
                                  
                                  ;; last LOOP
                                                                    (pl-last LOOP)
                                  
                                )
                                nil
                              )
                              
                            )
                            nil
                          )
                          
                          ;; if ($ser == 6) {                         my $cb = $self->{_EMPTROW_CB};                         unless ($cb && ref $cb eq 'CODE') {                             return undef;  # A callback is wanted, but none found                         }                         local $_ = $v_ref;                         my $rv = $cb->();                         # Result should be a ref to a list.                         unless (ref $rv eq 'ARRAY') {                             return undef;                         }                         my $n = @$rv;                         if ($n <= 0) {                             return 1;                         }                         if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                             $n = $ctx->{is_bound} - 1;                         }                         for (my $i = 0; $i < $n; $i++) {                             my $rvi = $rv->[$i];                             $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                         }                         return 1;                     }
                          ;; if ($ser == 6)
                          (pl-if                           (pcl:pl-== $ser 6)
                            (progn
                              ;; my $cb = $self->{_EMPTROW_CB}
                                                            (pcl:pl-setf $cb (pl-gethash-deref $self "_EMPTROW_CB"))
                              
                              ;; unless ($cb && ref $cb eq 'CODE') {                             return undef;  # A callback is wanted, but none found                         }
                              ;; unless ($cb && ref $cb eq 'CODE')
                              (pl-if (pl-not                               (pcl:pl-&& $cb (pcl:pl-str-eq (pl-ref $cb) "CODE")))
                                (progn
                                  ;; return undef
                                                                    (pl-return (pl-undef))
                                  
                                )
                                nil
                              )
                              
                              ;; my $rv = $cb->()
                                                            (pcl:pl-setf $rv (pl-funcall-ref $cb))
                              
                              ;; unless (ref $rv eq 'ARRAY') {                             return undef;                         }
                              ;; unless (ref $rv eq 'ARRAY')
                              (pl-if (pl-not                               (pcl:pl-str-eq (pl-ref $rv) "ARRAY"))
                                (progn
                                  ;; return undef
                                                                    (pl-return (pl-undef))
                                  
                                )
                                nil
                              )
                              
                              ;; my $n = @$rv
                                                            (pcl:pl-setf $n (pl-cast-@ $rv))
                              
                              ;; if ($n <= 0) {                             return 1;                         }
                              ;; if ($n <= 0)
                              (pl-if                               (pcl:pl-<= $n 0)
                                (progn
                                  ;; return 1
                                                                    (pl-return 1)
                                  
                                )
                                nil
                              )
                              
                              ;; if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                             $n = $ctx->{is_bound} - 1;                         }
                              ;; if ($ctx->{"is_bound"} && $ctx->{"is_bound"} < $n)
                              (pl-if                               (pcl:pl-&& (pl-gethash-deref $ctx "is_bound") (pcl:pl-< (pl-gethash-deref $ctx "is_bound") $n))
                                (progn
                                  ;; $n = $ctx->{is_bound} - 1
                                                                    (pcl:pl-setf $n (pcl:pl-- (pl-gethash-deref $ctx "is_bound") 1))
                                  
                                )
                                nil
                              )
                              
                              ;; for (my $i = 0; $i < $n; $i++) {                             my $rvi = $rv->[$i];                             $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                         }
                              (let (($i (make-pl-box nil)))
                                (pl-for (                              (pcl:pl-setf $i 0))
                                        (                              (pcl:pl-< $i $n))
                                        (                              (pl-post++ $i))
                                  ;; my $rvi = $rv->[$i]
                                                                    (pcl:pl-setf $rvi (pl-aref-deref $rv $i))
                                  
                                  ;; $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum)
                                                                    (pl-method-call $self '__push_value $ctx (pl-backslash $rvi) $fields $fflags (pl-gethash-deref $ctx "flag") $fnum)
                                  
                                )
                              )
                              
                              ;; return 1
                                                            (pl-return 1)
                              
                            )
                            nil
                          )
                          
                          ;; goto RESTART
                                                    (pl-goto (pl-RESTART))
                          
                        )
                        nil
                      )
                      
                      ;; if ($waitingForField) {                     # ,1,"foo, 3",,bar,                     #                  ^                     if ($ctx->{blank_is_undef} or $ctx->{empty_is_undef}) {                         $$v_ref = undef;                     } else {                         $$v_ref = "";                     }                     unless ($ctx->{is_bound}) {                         push @$fields, $$v_ref;                     }                     if ($ctx->{keep_meta_info} and $fflags) {                         push @$fflags, $ctx->{flag};                     }                     return 1;                 }
                      ;; if ($waitingForField)
                      (pl-if                       $waitingForField
                        (progn
                          ;; if ($ctx->{blank_is_undef} or $ctx->{empty_is_undef}) {                         $$v_ref = undef;                     } else {                         $$v_ref = "";                     }
                          ;; if ($ctx->{"blank_is_undef"} or $ctx->{"empty_is_undef"})
                          (pl-if                           (pcl:pl-or (pl-gethash-deref $ctx "blank_is_undef") (pl-gethash-deref $ctx "empty_is_undef"))
                            (progn
                              ;; $$v_ref = undef
                                                            (pcl:pl-setf (pl-cast-$ $v_ref) (pl-undef))
                              
                            )
                            ;; else
                            (progn
                              ;; $$v_ref = ""
                                                            (pcl:pl-setf (pl-cast-$ $v_ref) "")
                              
                            )
                          )
                          
                          ;; unless ($ctx->{is_bound}) {                         push @$fields, $$v_ref;                     }
                          ;; unless ($ctx->{"is_bound"})
                          (pl-if (pl-not                           (pl-gethash-deref $ctx "is_bound"))
                            (progn
                              ;; push @$fields, $$v_ref
                                                            (pl-push (pl-cast-@ $fields) (pl-cast-$ $v_ref))
                              
                            )
                            nil
                          )
                          
                          ;; if ($ctx->{keep_meta_info} and $fflags) {                         push @$fflags, $ctx->{flag};                     }
                          ;; if ($ctx->{"keep_meta_info"} and $fflags)
                          (pl-if                           (pcl:pl-and (pl-gethash-deref $ctx "keep_meta_info") $fflags)
                            (progn
                              ;; push @$fflags, $ctx->{flag}
                                                            (pl-push (pl-cast-@ $fflags) (pl-gethash-deref $ctx "flag"))
                              
                            )
                            nil
                          )
                          
                          ;; return 1
                                                    (pl-return 1)
                          
                        )
                        nil
                      )
                      
                      ;; if ($ctx->{flag} & IS_QUOTED) {                     # ,1,"foo\n 3",,bar,                     #        ^                     $ctx->{flag} |= IS_BINARY;                     unless ($ctx->{binary}) {                         $self->__error_inside_quotes($ctx, 2021);                         return;                     }                     $$v_ref .= $c;                 }                 elsif ($ctx->{verbatim}) {                     # ,1,foo\n 3,,bar,                     # This feature should be deprecated                     $ctx->{flag} |= IS_BINARY;                     unless ($ctx->{binary}) {                         $self->__error_inside_field($ctx, 2030);                         return;                     }                     $$v_ref .= $c unless $ctx->{eol} eq $c and $ctx->{useIO};                 }                 else {                     # sep=,                     #      ^                     if (!$ctx->{recno} and $ctx->{fld_idx} == 1 and $ctx->{useIO} and $hit =~ /^sep=(.{1,16})$/i) {                         $ctx->{sep} = $1;                         use bytes;                         my $len = length $ctx->{sep};                         if ($len <= 16) {                             $ctx->{sep_len} = $len == 1 ? 0 : $len;                             return $self->____parse($ctx, $src, $fields, $fflags);                         }                     }                      # ,1,"foo\n 3",,bar                     #                  ^                     $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                     return 1;                 }
                      ;; if ($ctx->{"flag"} & IS_QUOTED)
                      (pl-if                       (pcl:pl-bit-and (pl-gethash-deref $ctx "flag") (pl-IS_QUOTED))
                        (progn
                          ;; $ctx->{flag} |= IS_BINARY
                                                    (pcl:pl-bit-or= (pl-gethash-deref $ctx "flag") (pl-IS_BINARY))
                          
                          ;; unless ($ctx->{binary}) {                         $self->__error_inside_quotes($ctx, 2021);                         return;                     }
                          ;; unless ($ctx->{"binary"})
                          (pl-if (pl-not                           (pl-gethash-deref $ctx "binary"))
                            (progn
                              ;; $self->__error_inside_quotes($ctx, 2021)
                                                            (pl-method-call $self '__error_inside_quotes $ctx 2021)
                              
                              ;; return
                                                            (pl-return)
                              
                            )
                            nil
                          )
                          
                          ;; $$v_ref .= $c
                                                    (pcl:pl-.= (pl-cast-$ $v_ref) $c)
                          
                        )
                        ;; elsif ($ctx->{"verbatim"})
                        (pl-if                         (pl-gethash-deref $ctx "verbatim")
                          (progn
                            ;; $ctx->{flag} |= IS_BINARY
                                                        (pcl:pl-bit-or= (pl-gethash-deref $ctx "flag") (pl-IS_BINARY))
                            
                            ;; unless ($ctx->{binary}) {                         $self->__error_inside_field($ctx, 2030);                         return;                     }
                            ;; unless ($ctx->{"binary"})
                            (pl-if (pl-not                             (pl-gethash-deref $ctx "binary"))
                              (progn
                                ;; $self->__error_inside_field($ctx, 2030)
                                                                (pl-method-call $self '__error_inside_field $ctx 2030)
                                
                                ;; return
                                                                (pl-return)
                                
                              )
                              nil
                            )
                            
                            ;; $$v_ref .= $c unless $ctx->{eol} eq $c and $ctx->{useIO}
                            (pl-unless                             (pcl:pl-and (pcl:pl-str-eq (pl-gethash-deref $ctx "eol") $c) (pl-gethash-deref $ctx "useIO"))                             (pcl:pl-.= (pl-cast-$ $v_ref) $c))
                            
                          )
                          ;; else
                          (progn
                            ;; if (!$ctx->{recno} and $ctx->{fld_idx} == 1 and $ctx->{useIO} and $hit =~ /^sep=(.{1,16})$/i) {                         $ctx->{sep} = $1;                         use bytes;                         my $len = length $ctx->{sep};                         if ($len <= 16) {                             $ctx->{sep_len} = $len == 1 ? 0 : $len;                             return $self->____parse($ctx, $src, $fields, $fflags);                         }                     }
                            ;; if (!$ctx->{"recno"} and $ctx->{"fld_idx"} == 1 and $ctx->{"useIO"} and $hit =~ /^sep=(.{1,16})$/i)
                            (pl-if                             (pcl:pl-and (pcl:pl-and (pcl:pl-and (pl-! (pl-gethash-deref $ctx "recno")) (pcl:pl-== (pl-gethash-deref $ctx "fld_idx") 1)) (pl-gethash-deref $ctx "useIO")) (pcl:pl-=~ $hit (pl-regex "/^sep=(.{1,16})$/i")))
                              (progn
                                ;; $ctx->{sep} = $1
                                                                (pcl:pl-setf (pl-gethash-deref $ctx "sep") $1)
                                
                                ;; use bytes
                                (eval-when (:compile-toplevel :load-toplevel :execute)
                                  (pl-use "bytes"))
                                
                                ;; my $len = length $ctx->{sep}
                                                                (pcl:pl-setf $len (pl-length (pl-gethash-deref $ctx "sep")))
                                
                                ;; if ($len <= 16) {                             $ctx->{sep_len} = $len == 1 ? 0 : $len;                             return $self->____parse($ctx, $src, $fields, $fflags);                         }
                                ;; if ($len <= 16)
                                (pl-if                                 (pcl:pl-<= $len 16)
                                  (progn
                                    ;; $ctx->{sep_len} = $len == 1 ? 0 : $len
                                                                        (pcl:pl-setf (pl-gethash-deref $ctx "sep_len") (pl-if (pcl:pl-== $len 1) 0 $len))
                                    
                                    ;; return $self->____parse($ctx, $src, $fields, $fflags)
                                                                        (pl-return (pl-method-call $self '____parse $ctx $src $fields $fflags))
                                    
                                  )
                                  nil
                                )
                                
                              )
                              nil
                            )
                            
                            ;; $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum)
                                                        (pl-method-call $self '__push_value $ctx $v_ref $fields $fflags (pl-gethash-deref $ctx "flag") $fnum)
                            
                            ;; return 1
                                                        (pl-return 1)
                            
                          )
                        )
                      )
                      
                    )
                    ;; elsif (defined $c and $c eq "\015" and !$ctx->{"verbatim"})
                    (pl-if                     (pcl:pl-and (pcl:pl-and (pl-defined $c) (pcl:pl-str-eq $c " 15")) (pl-! (pl-gethash-deref $ctx "verbatim")))
                      (progn
                        ;; if ($waitingForField) {                     if ($ctx->{eol_is_cr}) {                         # ,1,"foo\n 3",,bar,\r                         #                   ^                         $c = "\012";                         goto EOLX;                     }                      my $c2 = $self->__get($ctx, $src);                     if (!defined $c2) { # EOF                         # ,1,"foo\n 3",,bar,\r                         #                     ^                         $c = undef;                         last unless $seenSomething;                         goto RESTART;                     }                     if ($c2 eq "\012") { # \r is not optional before EOLX!                         # ,1,"foo\n 3",,bar,\r\n                         #                     ^                         $c = $c2;                         goto EOLX;                     }                      if ($ctx->{useIO} and !$ctx->{eol_len}) {                         if ($c2 eq "\012") { # \r followed by an empty line                             # ,1,"foo\n 3",,bar,\r\r                             #                     ^                             $self->__set_eol_is_cr($ctx);                             goto EOLX;                         }                         $waitingForField = 0;                         if ($c2 !~ /[^\x09\x20-\x7E]/) {                             # ,1,"foo\n 3",,bar,\r                             # baz,4                             # ^                             $self->__set_eol_is_cr($ctx);                             $ctx->{used}--;                             $ctx->{has_ahead} = 1;                             if ($fnum == 1 && $ctx->{flag} == 0 && (!$v_ref or $$v_ref eq '') && $ctx->{skip_empty_rows}) {                                 ### SkipEmptyRow                                 my $ser = $ctx->{skip_empty_rows};                                 if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 5) { $self->SetDiag(2015); return undef; }                                  if ($ser <= 2) { # skip & eof                                     $ctx->{fld_idx} = 0;                                     $c = $self->__get($ctx, $src);                                     if (!defined $c) { # EOF                                         $v_ref = undef;                                         $waitingForField = 1;                                         $seenSomething = 0;                                         last LOOP;                                     }                                 }                                  if ($ser == 6) {                                     my $cb = $self->{_EMPTROW_CB};                                     unless ($cb && ref $cb eq 'CODE') {                                         return undef;  # A callback is wanted, but none found                                     }                                     local $_ = $v_ref;                                     my $rv = $cb->();                                     # Result should be a ref to a list.                                     unless (ref $rv eq 'ARRAY') {                                         return undef;                                     }                                     my $n = @$rv;                                     if ($n <= 0) {                                         return 1;                                     }                                     if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                                         $n = $ctx->{is_bound} - 1;                                     }                                     for (my $i = 0; $i < $n; $i++) {                                         my $rvi = $rv->[$i];                                         $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                                     }                                     return 1;                                 }                                  $$v_ref = $c2;                                 goto RESTART;                             }                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                     }                      # ,1,"foo\n 3",,bar,\r\t                     #                     ^                     $ctx->{used}--;                     $self->__error_inside_field($ctx, 2031);                     return;                 }
                        ;; if ($waitingForField)
                        (pl-if                         $waitingForField
                          (progn
                            ;; if ($ctx->{eol_is_cr}) {                         # ,1,"foo\n 3",,bar,\r                         #                   ^                         $c = "\012";                         goto EOLX;                     }
                            ;; if ($ctx->{"eol_is_cr"})
                            (pl-if                             (pl-gethash-deref $ctx "eol_is_cr")
                              (progn
                                ;; $c = "\012"
                                                                (pcl:pl-setf $c " 12")
                                
                                ;; goto EOLX
                                                                (pl-goto (pl-EOLX))
                                
                              )
                              nil
                            )
                            
                            ;; my $c2 = $self->__get($ctx, $src)
                                                        (pcl:pl-setf $c2 (pl-method-call $self '__get $ctx $src))
                            
                            ;; if (!defined $c2) { # EOF                         # ,1,"foo\n 3",,bar,\r                         #                     ^                         $c = undef;                         last unless $seenSomething;                         goto RESTART;                     }
                            ;; if (!defined $c2)
                            (pl-if                             (pl-! (pl-defined $c2))
                              (progn
                                ;; $c = undef
                                                                (pcl:pl-setf $c (pl-undef))
                                
                                ;; last unless $seenSomething
                                (pl-unless                                 $seenSomething                                 (pl-last))
                                
                                ;; goto RESTART
                                                                (pl-goto (pl-RESTART))
                                
                              )
                              nil
                            )
                            
                            ;; if ($c2 eq "\012") { # \r is not optional before EOLX!                         # ,1,"foo\n 3",,bar,\r\n                         #                     ^                         $c = $c2;                         goto EOLX;                     }
                            ;; if ($c2 eq "\012")
                            (pl-if                             (pcl:pl-str-eq $c2 " 12")
                              (progn
                                ;; $c = $c2
                                                                (pcl:pl-setf $c $c2)
                                
                                ;; goto EOLX
                                                                (pl-goto (pl-EOLX))
                                
                              )
                              nil
                            )
                            
                            ;; if ($ctx->{useIO} and !$ctx->{eol_len}) {                         if ($c2 eq "\012") { # \r followed by an empty line                             # ,1,"foo\n 3",,bar,\r\r                             #                     ^                             $self->__set_eol_is_cr($ctx);                             goto EOLX;                         }                         $waitingForField = 0;                         if ($c2 !~ /[^\x09\x20-\x7E]/) {                             # ,1,"foo\n 3",,bar,\r                             # baz,4                             # ^                             $self->__set_eol_is_cr($ctx);                             $ctx->{used}--;                             $ctx->{has_ahead} = 1;                             if ($fnum == 1 && $ctx->{flag} == 0 && (!$v_ref or $$v_ref eq '') && $ctx->{skip_empty_rows}) {                                 ### SkipEmptyRow                                 my $ser = $ctx->{skip_empty_rows};                                 if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 5) { $self->SetDiag(2015); return undef; }                                  if ($ser <= 2) { # skip & eof                                     $ctx->{fld_idx} = 0;                                     $c = $self->__get($ctx, $src);                                     if (!defined $c) { # EOF                                         $v_ref = undef;                                         $waitingForField = 1;                                         $seenSomething = 0;                                         last LOOP;                                     }                                 }                                  if ($ser == 6) {                                     my $cb = $self->{_EMPTROW_CB};                                     unless ($cb && ref $cb eq 'CODE') {                                         return undef;  # A callback is wanted, but none found                                     }                                     local $_ = $v_ref;                                     my $rv = $cb->();                                     # Result should be a ref to a list.                                     unless (ref $rv eq 'ARRAY') {                                         return undef;                                     }                                     my $n = @$rv;                                     if ($n <= 0) {                                         return 1;                                     }                                     if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                                         $n = $ctx->{is_bound} - 1;                                     }                                     for (my $i = 0; $i < $n; $i++) {                                         my $rvi = $rv->[$i];                                         $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                                     }                                     return 1;                                 }                                  $$v_ref = $c2;                                 goto RESTART;                             }                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                     }
                            ;; if ($ctx->{"useIO"} and !$ctx->{"eol_len"})
                            (pl-if                             (pcl:pl-and (pl-gethash-deref $ctx "useIO") (pl-! (pl-gethash-deref $ctx "eol_len")))
                              (progn
                                ;; if ($c2 eq "\012") { # \r followed by an empty line                             # ,1,"foo\n 3",,bar,\r\r                             #                     ^                             $self->__set_eol_is_cr($ctx);                             goto EOLX;                         }
                                ;; if ($c2 eq "\012")
                                (pl-if                                 (pcl:pl-str-eq $c2 " 12")
                                  (progn
                                    ;; $self->__set_eol_is_cr($ctx)
                                                                        (pl-method-call $self '__set_eol_is_cr $ctx)
                                    
                                    ;; goto EOLX
                                                                        (pl-goto (pl-EOLX))
                                    
                                  )
                                  nil
                                )
                                
                                ;; $waitingForField = 0
                                                                (pcl:pl-setf $waitingForField 0)
                                
                                ;; if ($c2 !~ /[^\x09\x20-\x7E]/) {                             # ,1,"foo\n 3",,bar,\r                             # baz,4                             # ^                             $self->__set_eol_is_cr($ctx);                             $ctx->{used}--;                             $ctx->{has_ahead} = 1;                             if ($fnum == 1 && $ctx->{flag} == 0 && (!$v_ref or $$v_ref eq '') && $ctx->{skip_empty_rows}) {                                 ### SkipEmptyRow                                 my $ser = $ctx->{skip_empty_rows};                                 if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 5) { $self->SetDiag(2015); return undef; }                                  if ($ser <= 2) { # skip & eof                                     $ctx->{fld_idx} = 0;                                     $c = $self->__get($ctx, $src);                                     if (!defined $c) { # EOF                                         $v_ref = undef;                                         $waitingForField = 1;                                         $seenSomething = 0;                                         last LOOP;                                     }                                 }                                  if ($ser == 6) {                                     my $cb = $self->{_EMPTROW_CB};                                     unless ($cb && ref $cb eq 'CODE') {                                         return undef;  # A callback is wanted, but none found                                     }                                     local $_ = $v_ref;                                     my $rv = $cb->();                                     # Result should be a ref to a list.                                     unless (ref $rv eq 'ARRAY') {                                         return undef;                                     }                                     my $n = @$rv;                                     if ($n <= 0) {                                         return 1;                                     }                                     if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                                         $n = $ctx->{is_bound} - 1;                                     }                                     for (my $i = 0; $i < $n; $i++) {                                         my $rvi = $rv->[$i];                                         $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                                     }                                     return 1;                                 }                                  $$v_ref = $c2;                                 goto RESTART;                             }                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }
                                ;; if ($c2 !~ /[^\x09\x20-\x7E]/)
                                (pl-if                                 (pcl:pl-!~ $c2 (pl-regex "/[^\\x09\\x20-\\x7E]/"))
                                  (progn
                                    ;; $self->__set_eol_is_cr($ctx)
                                                                        (pl-method-call $self '__set_eol_is_cr $ctx)
                                    
                                    ;; $ctx->{used}--
                                                                        (pl-post-- (pl-gethash-deref $ctx "used"))
                                    
                                    ;; $ctx->{has_ahead} = 1
                                                                        (pcl:pl-setf (pl-gethash-deref $ctx "has_ahead") 1)
                                    
                                    ;; if ($fnum == 1 && $ctx->{flag} == 0 && (!$v_ref or $$v_ref eq '') && $ctx->{skip_empty_rows}) {                                 ### SkipEmptyRow                                 my $ser = $ctx->{skip_empty_rows};                                 if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 5) { $self->SetDiag(2015); return undef; }                                  if ($ser <= 2) { # skip & eof                                     $ctx->{fld_idx} = 0;                                     $c = $self->__get($ctx, $src);                                     if (!defined $c) { # EOF                                         $v_ref = undef;                                         $waitingForField = 1;                                         $seenSomething = 0;                                         last LOOP;                                     }                                 }                                  if ($ser == 6) {                                     my $cb = $self->{_EMPTROW_CB};                                     unless ($cb && ref $cb eq 'CODE') {                                         return undef;  # A callback is wanted, but none found                                     }                                     local $_ = $v_ref;                                     my $rv = $cb->();                                     # Result should be a ref to a list.                                     unless (ref $rv eq 'ARRAY') {                                         return undef;                                     }                                     my $n = @$rv;                                     if ($n <= 0) {                                         return 1;                                     }                                     if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                                         $n = $ctx->{is_bound} - 1;                                     }                                     for (my $i = 0; $i < $n; $i++) {                                         my $rvi = $rv->[$i];                                         $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                                     }                                     return 1;                                 }                                  $$v_ref = $c2;                                 goto RESTART;                             }
                                    ;; if ($fnum == 1 && $ctx->{"flag"} == 0 && (!$v_ref or $$v_ref eq '') && $ctx->{"skip_empty_rows"})
                                    (pl-if                                     (pcl:pl-&& (pcl:pl-&& (pcl:pl-&& (pcl:pl-== $fnum 1) (pcl:pl-== (pl-gethash-deref $ctx "flag") 0)) (pcl:pl-or (pl-! $v_ref) (pcl:pl-str-eq (pl-cast-$ $v_ref) ""))) (pl-gethash-deref $ctx "skip_empty_rows"))
                                      (progn
                                        ;; my $ser = $ctx->{skip_empty_rows}
                                                                                (pcl:pl-setf $ser (pl-gethash-deref $ctx "skip_empty_rows"))
                                        
                                        ;; if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }
                                        ;; if ($ser == 3)
                                        (pl-if                                         (pcl:pl-== $ser 3)
                                          (progn
                                            ;; $self->SetDiag(2015)
                                                                                        (pl-method-call $self 'SetDiag 2015)
                                            
                                            ;; die "Empty row\n"
                                                                                        (pl-die "Empty row
")
                                            
                                          )
                                          nil
                                        )
                                        
                                        ;; if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }
                                        ;; if ($ser == 4)
                                        (pl-if                                         (pcl:pl-== $ser 4)
                                          (progn
                                            ;; $self->SetDiag(2015)
                                                                                        (pl-method-call $self 'SetDiag 2015)
                                            
                                            ;; die "Empty row\n"
                                                                                        (pl-die "Empty row
")
                                            
                                          )
                                          nil
                                        )
                                        
                                        ;; if ($ser == 5) { $self->SetDiag(2015); return undef; }
                                        ;; if ($ser == 5)
                                        (pl-if                                         (pcl:pl-== $ser 5)
                                          (progn
                                            ;; $self->SetDiag(2015)
                                                                                        (pl-method-call $self 'SetDiag 2015)
                                            
                                            ;; return undef
                                                                                        (pl-return (pl-undef))
                                            
                                          )
                                          nil
                                        )
                                        
                                        ;; if ($ser <= 2) { # skip & eof                                     $ctx->{fld_idx} = 0;                                     $c = $self->__get($ctx, $src);                                     if (!defined $c) { # EOF                                         $v_ref = undef;                                         $waitingForField = 1;                                         $seenSomething = 0;                                         last LOOP;                                     }                                 }
                                        ;; if ($ser <= 2)
                                        (pl-if                                         (pcl:pl-<= $ser 2)
                                          (progn
                                            ;; $ctx->{fld_idx} = 0
                                                                                        (pcl:pl-setf (pl-gethash-deref $ctx "fld_idx") 0)
                                            
                                            ;; $c = $self->__get($ctx, $src)
                                                                                        (pcl:pl-setf $c (pl-method-call $self '__get $ctx $src))
                                            
                                            ;; if (!defined $c) { # EOF                                         $v_ref = undef;                                         $waitingForField = 1;                                         $seenSomething = 0;                                         last LOOP;                                     }
                                            ;; if (!defined $c)
                                            (pl-if                                             (pl-! (pl-defined $c))
                                              (progn
                                                ;; $v_ref = undef
                                                                                                (pcl:pl-setf $v_ref (pl-undef))
                                                
                                                ;; $waitingForField = 1
                                                                                                (pcl:pl-setf $waitingForField 1)
                                                
                                                ;; $seenSomething = 0
                                                                                                (pcl:pl-setf $seenSomething 0)
                                                
                                                ;; last LOOP
                                                                                                (pl-last LOOP)
                                                
                                              )
                                              nil
                                            )
                                            
                                          )
                                          nil
                                        )
                                        
                                        ;; if ($ser == 6) {                                     my $cb = $self->{_EMPTROW_CB};                                     unless ($cb && ref $cb eq 'CODE') {                                         return undef;  # A callback is wanted, but none found                                     }                                     local $_ = $v_ref;                                     my $rv = $cb->();                                     # Result should be a ref to a list.                                     unless (ref $rv eq 'ARRAY') {                                         return undef;                                     }                                     my $n = @$rv;                                     if ($n <= 0) {                                         return 1;                                     }                                     if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                                         $n = $ctx->{is_bound} - 1;                                     }                                     for (my $i = 0; $i < $n; $i++) {                                         my $rvi = $rv->[$i];                                         $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                                     }                                     return 1;                                 }
                                        ;; if ($ser == 6)
                                        (pl-if                                         (pcl:pl-== $ser 6)
                                          (progn
                                            ;; my $cb = $self->{_EMPTROW_CB}
                                                                                        (pcl:pl-setf $cb (pl-gethash-deref $self "_EMPTROW_CB"))
                                            
                                            ;; unless ($cb && ref $cb eq 'CODE') {                                         return undef;  # A callback is wanted, but none found                                     }
                                            ;; unless ($cb && ref $cb eq 'CODE')
                                            (pl-if (pl-not                                             (pcl:pl-&& $cb (pcl:pl-str-eq (pl-ref $cb) "CODE")))
                                              (progn
                                                ;; return undef
                                                                                                (pl-return (pl-undef))
                                                
                                              )
                                              nil
                                            )
                                            
                                            ;; my $rv = $cb->()
                                                                                        (pcl:pl-setf $rv (pl-funcall-ref $cb))
                                            
                                            ;; unless (ref $rv eq 'ARRAY') {                                         return undef;                                     }
                                            ;; unless (ref $rv eq 'ARRAY')
                                            (pl-if (pl-not                                             (pcl:pl-str-eq (pl-ref $rv) "ARRAY"))
                                              (progn
                                                ;; return undef
                                                                                                (pl-return (pl-undef))
                                                
                                              )
                                              nil
                                            )
                                            
                                            ;; my $n = @$rv
                                                                                        (pcl:pl-setf $n (pl-cast-@ $rv))
                                            
                                            ;; if ($n <= 0) {                                         return 1;                                     }
                                            ;; if ($n <= 0)
                                            (pl-if                                             (pcl:pl-<= $n 0)
                                              (progn
                                                ;; return 1
                                                                                                (pl-return 1)
                                                
                                              )
                                              nil
                                            )
                                            
                                            ;; if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                                         $n = $ctx->{is_bound} - 1;                                     }
                                            ;; if ($ctx->{"is_bound"} && $ctx->{"is_bound"} < $n)
                                            (pl-if                                             (pcl:pl-&& (pl-gethash-deref $ctx "is_bound") (pcl:pl-< (pl-gethash-deref $ctx "is_bound") $n))
                                              (progn
                                                ;; $n = $ctx->{is_bound} - 1
                                                                                                (pcl:pl-setf $n (pcl:pl-- (pl-gethash-deref $ctx "is_bound") 1))
                                                
                                              )
                                              nil
                                            )
                                            
                                            ;; for (my $i = 0; $i < $n; $i++) {                                         my $rvi = $rv->[$i];                                         $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                                     }
                                            (let (($i (make-pl-box nil)))
                                              (pl-for (                                            (pcl:pl-setf $i 0))
                                                      (                                            (pcl:pl-< $i $n))
                                                      (                                            (pl-post++ $i))
                                                ;; my $rvi = $rv->[$i]
                                                                                                (pcl:pl-setf $rvi (pl-aref-deref $rv $i))
                                                
                                                ;; $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum)
                                                                                                (pl-method-call $self '__push_value $ctx (pl-backslash $rvi) $fields $fflags (pl-gethash-deref $ctx "flag") $fnum)
                                                
                                              )
                                            )
                                            
                                            ;; return 1
                                                                                        (pl-return 1)
                                            
                                          )
                                          nil
                                        )
                                        
                                        ;; $$v_ref = $c2
                                                                                (pcl:pl-setf (pl-cast-$ $v_ref) $c2)
                                        
                                        ;; goto RESTART
                                                                                (pl-goto (pl-RESTART))
                                        
                                      )
                                      nil
                                    )
                                    
                                    ;; $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum)
                                                                        (pl-method-call $self '__push_value $ctx $v_ref $fields $fflags (pl-gethash-deref $ctx "flag") $fnum)
                                    
                                    ;; return 1
                                                                        (pl-return 1)
                                    
                                  )
                                  nil
                                )
                                
                              )
                              nil
                            )
                            
                            ;; $ctx->{used}--
                                                        (pl-post-- (pl-gethash-deref $ctx "used"))
                            
                            ;; $self->__error_inside_field($ctx, 2031)
                                                        (pl-method-call $self '__error_inside_field $ctx 2031)
                            
                            ;; return
                                                        (pl-return)
                            
                          )
                          nil
                        )
                        
                        ;; if ($ctx->{flag} & IS_QUOTED) {                     # ,1,"foo\r 3",,bar,\r\t                     #        ^                     $ctx->{flag} |= IS_BINARY;                     unless ($ctx->{binary}) {                         $self->__error_inside_quotes($ctx, 2022);                         return;                     }                     $$v_ref .= $c;                 }                 else {                     if ($ctx->{eol_is_cr}) {                         # ,1,"foo\n 3",,bar\r                         #                  ^                         goto EOLX;                     }                      my $c2 = $self->__get($ctx, $src);                     if (defined $c2 and $c2 eq "\012") { # \r is not optional before EOLX!                         # ,1,"foo\n 3",,bar\r\n                         #                    ^                         goto EOLX;                     }                      if ($ctx->{useIO} and !$ctx->{eol_len}) {                         if ($c2 !~ /[^\x09\x20-\x7E]/                             # ,1,"foo\n 3",,bar\r                             # baz,4                             # ^                             or $c2 eq "\015"                             # ,1,"foo\n 3",,bar,\r\r                             #                     ^                         ) {                             $self->__set_eol_is_cr($ctx);                             $ctx->{used}--;                             $ctx->{has_ahead} = 1;                             if ($fnum == 1 && $ctx->{flag} == 0 && (!$v_ref or $$v_ref eq '') && $ctx->{skip_empty_rows}) {                                 ### SKipEmptyRow                                 my $ser = $ctx->{skip_empty_rows};                                 if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 5) { $self->SetDiag(2015); return undef; }                                  if ($ser <= 2) { # skip & eof                                     $ctx->{fld_idx} = 0;                                     $c = $self->__get($ctx, $src);                                     if (!defined $c) { # EOL                                         $v_ref = undef;                                         $seenSomething = 0;                                         last LOOP;                                     }                                 }                                  if ($ser == 6) {                                     my $cb = $self->{_EMPTROW_CB};                                     unless ($cb && ref $cb eq 'CODE') {                                         return undef;  # A callback is wanted, but none found                                     }                                     local $_ = $v_ref;                                     my $rv = $cb->();                                     # Result should be a ref to a list.                                     unless (ref $rv eq 'ARRAY') {                                         return undef;                                     }                                     my $n = @$rv;                                     if ($n <= 0) {                                         return 1;                                     }                                     if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                                         $n = $ctx->{is_bound} - 1;                                     }                                     for (my $i = 0; $i < $n; $i++) {                                         my $rvi = $rv->[$i];                                         $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                                     }                                     return 1;                                 }                                 goto RESTART;                             }                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                     }                      # ,1,"foo\n 3",,bar\r\t                     #                    ^                     $self->__error_inside_field($ctx, 2032);                     return;                 }
                        ;; if ($ctx->{"flag"} & IS_QUOTED)
                        (pl-if                         (pcl:pl-bit-and (pl-gethash-deref $ctx "flag") (pl-IS_QUOTED))
                          (progn
                            ;; $ctx->{flag} |= IS_BINARY
                                                        (pcl:pl-bit-or= (pl-gethash-deref $ctx "flag") (pl-IS_BINARY))
                            
                            ;; unless ($ctx->{binary}) {                         $self->__error_inside_quotes($ctx, 2022);                         return;                     }
                            ;; unless ($ctx->{"binary"})
                            (pl-if (pl-not                             (pl-gethash-deref $ctx "binary"))
                              (progn
                                ;; $self->__error_inside_quotes($ctx, 2022)
                                                                (pl-method-call $self '__error_inside_quotes $ctx 2022)
                                
                                ;; return
                                                                (pl-return)
                                
                              )
                              nil
                            )
                            
                            ;; $$v_ref .= $c
                                                        (pcl:pl-.= (pl-cast-$ $v_ref) $c)
                            
                          )
                          ;; else
                          (progn
                            ;; if ($ctx->{eol_is_cr}) {                         # ,1,"foo\n 3",,bar\r                         #                  ^                         goto EOLX;                     }
                            ;; if ($ctx->{"eol_is_cr"})
                            (pl-if                             (pl-gethash-deref $ctx "eol_is_cr")
                              (progn
                                ;; goto EOLX
                                                                (pl-goto (pl-EOLX))
                                
                              )
                              nil
                            )
                            
                            ;; my $c2 = $self->__get($ctx, $src)
                                                        (pcl:pl-setf $c2 (pl-method-call $self '__get $ctx $src))
                            
                            ;; if (defined $c2 and $c2 eq "\012") { # \r is not optional before EOLX!                         # ,1,"foo\n 3",,bar\r\n                         #                    ^                         goto EOLX;                     }
                            ;; if (defined $c2 and $c2 eq "\012")
                            (pl-if                             (pcl:pl-and (pl-defined $c2) (pcl:pl-str-eq $c2 " 12"))
                              (progn
                                ;; goto EOLX
                                                                (pl-goto (pl-EOLX))
                                
                              )
                              nil
                            )
                            
                            ;; if ($ctx->{useIO} and !$ctx->{eol_len}) {                         if ($c2 !~ /[^\x09\x20-\x7E]/                             # ,1,"foo\n 3",,bar\r                             # baz,4                             # ^                             or $c2 eq "\015"                             # ,1,"foo\n 3",,bar,\r\r                             #                     ^                         ) {                             $self->__set_eol_is_cr($ctx);                             $ctx->{used}--;                             $ctx->{has_ahead} = 1;                             if ($fnum == 1 && $ctx->{flag} == 0 && (!$v_ref or $$v_ref eq '') && $ctx->{skip_empty_rows}) {                                 ### SKipEmptyRow                                 my $ser = $ctx->{skip_empty_rows};                                 if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 5) { $self->SetDiag(2015); return undef; }                                  if ($ser <= 2) { # skip & eof                                     $ctx->{fld_idx} = 0;                                     $c = $self->__get($ctx, $src);                                     if (!defined $c) { # EOL                                         $v_ref = undef;                                         $seenSomething = 0;                                         last LOOP;                                     }                                 }                                  if ($ser == 6) {                                     my $cb = $self->{_EMPTROW_CB};                                     unless ($cb && ref $cb eq 'CODE') {                                         return undef;  # A callback is wanted, but none found                                     }                                     local $_ = $v_ref;                                     my $rv = $cb->();                                     # Result should be a ref to a list.                                     unless (ref $rv eq 'ARRAY') {                                         return undef;                                     }                                     my $n = @$rv;                                     if ($n <= 0) {                                         return 1;                                     }                                     if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                                         $n = $ctx->{is_bound} - 1;                                     }                                     for (my $i = 0; $i < $n; $i++) {                                         my $rvi = $rv->[$i];                                         $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                                     }                                     return 1;                                 }                                 goto RESTART;                             }                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }                     }
                            ;; if ($ctx->{"useIO"} and !$ctx->{"eol_len"})
                            (pl-if                             (pcl:pl-and (pl-gethash-deref $ctx "useIO") (pl-! (pl-gethash-deref $ctx "eol_len")))
                              (progn
                                ;; if ($c2 !~ /[^\x09\x20-\x7E]/                             # ,1,"foo\n 3",,bar\r                             # baz,4                             # ^                             or $c2 eq "\015"                             # ,1,"foo\n 3",,bar,\r\r                             #                     ^                         ) {                             $self->__set_eol_is_cr($ctx);                             $ctx->{used}--;                             $ctx->{has_ahead} = 1;                             if ($fnum == 1 && $ctx->{flag} == 0 && (!$v_ref or $$v_ref eq '') && $ctx->{skip_empty_rows}) {                                 ### SKipEmptyRow                                 my $ser = $ctx->{skip_empty_rows};                                 if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 5) { $self->SetDiag(2015); return undef; }                                  if ($ser <= 2) { # skip & eof                                     $ctx->{fld_idx} = 0;                                     $c = $self->__get($ctx, $src);                                     if (!defined $c) { # EOL                                         $v_ref = undef;                                         $seenSomething = 0;                                         last LOOP;                                     }                                 }                                  if ($ser == 6) {                                     my $cb = $self->{_EMPTROW_CB};                                     unless ($cb && ref $cb eq 'CODE') {                                         return undef;  # A callback is wanted, but none found                                     }                                     local $_ = $v_ref;                                     my $rv = $cb->();                                     # Result should be a ref to a list.                                     unless (ref $rv eq 'ARRAY') {                                         return undef;                                     }                                     my $n = @$rv;                                     if ($n <= 0) {                                         return 1;                                     }                                     if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                                         $n = $ctx->{is_bound} - 1;                                     }                                     for (my $i = 0; $i < $n; $i++) {                                         my $rvi = $rv->[$i];                                         $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                                     }                                     return 1;                                 }                                 goto RESTART;                             }                             $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);                             return 1;                         }
                                ;; if ($c2 !~ /[^\x09\x20-\x7E]/
;;                             # ,1,"foo\n 3",,bar\r
;;                             # baz,4
;;                             # ^
;;                             or $c2 eq "\015"
;;                             # ,1,"foo\n 3",,bar,\r\r
;;                             #                     ^)
                                (pl-if                                 (pcl:pl-or (pcl:pl-!~ $c2 (pl-regex "/[^\\x09\\x20-\\x7E]/")) (pcl:pl-str-eq $c2 " 15"))
                                  (progn
                                    ;; $self->__set_eol_is_cr($ctx)
                                                                        (pl-method-call $self '__set_eol_is_cr $ctx)
                                    
                                    ;; $ctx->{used}--
                                                                        (pl-post-- (pl-gethash-deref $ctx "used"))
                                    
                                    ;; $ctx->{has_ahead} = 1
                                                                        (pcl:pl-setf (pl-gethash-deref $ctx "has_ahead") 1)
                                    
                                    ;; if ($fnum == 1 && $ctx->{flag} == 0 && (!$v_ref or $$v_ref eq '') && $ctx->{skip_empty_rows}) {                                 ### SKipEmptyRow                                 my $ser = $ctx->{skip_empty_rows};                                 if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }                                 if ($ser == 5) { $self->SetDiag(2015); return undef; }                                  if ($ser <= 2) { # skip & eof                                     $ctx->{fld_idx} = 0;                                     $c = $self->__get($ctx, $src);                                     if (!defined $c) { # EOL                                         $v_ref = undef;                                         $seenSomething = 0;                                         last LOOP;                                     }                                 }                                  if ($ser == 6) {                                     my $cb = $self->{_EMPTROW_CB};                                     unless ($cb && ref $cb eq 'CODE') {                                         return undef;  # A callback is wanted, but none found                                     }                                     local $_ = $v_ref;                                     my $rv = $cb->();                                     # Result should be a ref to a list.                                     unless (ref $rv eq 'ARRAY') {                                         return undef;                                     }                                     my $n = @$rv;                                     if ($n <= 0) {                                         return 1;                                     }                                     if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                                         $n = $ctx->{is_bound} - 1;                                     }                                     for (my $i = 0; $i < $n; $i++) {                                         my $rvi = $rv->[$i];                                         $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                                     }                                     return 1;                                 }                                 goto RESTART;                             }
                                    ;; if ($fnum == 1 && $ctx->{"flag"} == 0 && (!$v_ref or $$v_ref eq '') && $ctx->{"skip_empty_rows"})
                                    (pl-if                                     (pcl:pl-&& (pcl:pl-&& (pcl:pl-&& (pcl:pl-== $fnum 1) (pcl:pl-== (pl-gethash-deref $ctx "flag") 0)) (pcl:pl-or (pl-! $v_ref) (pcl:pl-str-eq (pl-cast-$ $v_ref) ""))) (pl-gethash-deref $ctx "skip_empty_rows"))
                                      (progn
                                        ;; my $ser = $ctx->{skip_empty_rows}
                                                                                (pcl:pl-setf $ser (pl-gethash-deref $ctx "skip_empty_rows"))
                                        
                                        ;; if ($ser == 3) { $self->SetDiag(2015); die "Empty row\n"; }
                                        ;; if ($ser == 3)
                                        (pl-if                                         (pcl:pl-== $ser 3)
                                          (progn
                                            ;; $self->SetDiag(2015)
                                                                                        (pl-method-call $self 'SetDiag 2015)
                                            
                                            ;; die "Empty row\n"
                                                                                        (pl-die "Empty row
")
                                            
                                          )
                                          nil
                                        )
                                        
                                        ;; if ($ser == 4) { $self->SetDiag(2015); die "Empty row\n"; }
                                        ;; if ($ser == 4)
                                        (pl-if                                         (pcl:pl-== $ser 4)
                                          (progn
                                            ;; $self->SetDiag(2015)
                                                                                        (pl-method-call $self 'SetDiag 2015)
                                            
                                            ;; die "Empty row\n"
                                                                                        (pl-die "Empty row
")
                                            
                                          )
                                          nil
                                        )
                                        
                                        ;; if ($ser == 5) { $self->SetDiag(2015); return undef; }
                                        ;; if ($ser == 5)
                                        (pl-if                                         (pcl:pl-== $ser 5)
                                          (progn
                                            ;; $self->SetDiag(2015)
                                                                                        (pl-method-call $self 'SetDiag 2015)
                                            
                                            ;; return undef
                                                                                        (pl-return (pl-undef))
                                            
                                          )
                                          nil
                                        )
                                        
                                        ;; if ($ser <= 2) { # skip & eof                                     $ctx->{fld_idx} = 0;                                     $c = $self->__get($ctx, $src);                                     if (!defined $c) { # EOL                                         $v_ref = undef;                                         $seenSomething = 0;                                         last LOOP;                                     }                                 }
                                        ;; if ($ser <= 2)
                                        (pl-if                                         (pcl:pl-<= $ser 2)
                                          (progn
                                            ;; $ctx->{fld_idx} = 0
                                                                                        (pcl:pl-setf (pl-gethash-deref $ctx "fld_idx") 0)
                                            
                                            ;; $c = $self->__get($ctx, $src)
                                                                                        (pcl:pl-setf $c (pl-method-call $self '__get $ctx $src))
                                            
                                            ;; if (!defined $c) { # EOL                                         $v_ref = undef;                                         $seenSomething = 0;                                         last LOOP;                                     }
                                            ;; if (!defined $c)
                                            (pl-if                                             (pl-! (pl-defined $c))
                                              (progn
                                                ;; $v_ref = undef
                                                                                                (pcl:pl-setf $v_ref (pl-undef))
                                                
                                                ;; $seenSomething = 0
                                                                                                (pcl:pl-setf $seenSomething 0)
                                                
                                                ;; last LOOP
                                                                                                (pl-last LOOP)
                                                
                                              )
                                              nil
                                            )
                                            
                                          )
                                          nil
                                        )
                                        
                                        ;; if ($ser == 6) {                                     my $cb = $self->{_EMPTROW_CB};                                     unless ($cb && ref $cb eq 'CODE') {                                         return undef;  # A callback is wanted, but none found                                     }                                     local $_ = $v_ref;                                     my $rv = $cb->();                                     # Result should be a ref to a list.                                     unless (ref $rv eq 'ARRAY') {                                         return undef;                                     }                                     my $n = @$rv;                                     if ($n <= 0) {                                         return 1;                                     }                                     if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                                         $n = $ctx->{is_bound} - 1;                                     }                                     for (my $i = 0; $i < $n; $i++) {                                         my $rvi = $rv->[$i];                                         $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                                     }                                     return 1;                                 }
                                        ;; if ($ser == 6)
                                        (pl-if                                         (pcl:pl-== $ser 6)
                                          (progn
                                            ;; my $cb = $self->{_EMPTROW_CB}
                                                                                        (pcl:pl-setf $cb (pl-gethash-deref $self "_EMPTROW_CB"))
                                            
                                            ;; unless ($cb && ref $cb eq 'CODE') {                                         return undef;  # A callback is wanted, but none found                                     }
                                            ;; unless ($cb && ref $cb eq 'CODE')
                                            (pl-if (pl-not                                             (pcl:pl-&& $cb (pcl:pl-str-eq (pl-ref $cb) "CODE")))
                                              (progn
                                                ;; return undef
                                                                                                (pl-return (pl-undef))
                                                
                                              )
                                              nil
                                            )
                                            
                                            ;; my $rv = $cb->()
                                                                                        (pcl:pl-setf $rv (pl-funcall-ref $cb))
                                            
                                            ;; unless (ref $rv eq 'ARRAY') {                                         return undef;                                     }
                                            ;; unless (ref $rv eq 'ARRAY')
                                            (pl-if (pl-not                                             (pcl:pl-str-eq (pl-ref $rv) "ARRAY"))
                                              (progn
                                                ;; return undef
                                                                                                (pl-return (pl-undef))
                                                
                                              )
                                              nil
                                            )
                                            
                                            ;; my $n = @$rv
                                                                                        (pcl:pl-setf $n (pl-cast-@ $rv))
                                            
                                            ;; if ($n <= 0) {                                         return 1;                                     }
                                            ;; if ($n <= 0)
                                            (pl-if                                             (pcl:pl-<= $n 0)
                                              (progn
                                                ;; return 1
                                                                                                (pl-return 1)
                                                
                                              )
                                              nil
                                            )
                                            
                                            ;; if ($ctx->{is_bound} && $ctx->{is_bound} < $n) {                                         $n = $ctx->{is_bound} - 1;                                     }
                                            ;; if ($ctx->{"is_bound"} && $ctx->{"is_bound"} < $n)
                                            (pl-if                                             (pcl:pl-&& (pl-gethash-deref $ctx "is_bound") (pcl:pl-< (pl-gethash-deref $ctx "is_bound") $n))
                                              (progn
                                                ;; $n = $ctx->{is_bound} - 1
                                                                                                (pcl:pl-setf $n (pcl:pl-- (pl-gethash-deref $ctx "is_bound") 1))
                                                
                                              )
                                              nil
                                            )
                                            
                                            ;; for (my $i = 0; $i < $n; $i++) {                                         my $rvi = $rv->[$i];                                         $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum);                                     }
                                            (let (($i (make-pl-box nil)))
                                              (pl-for (                                            (pcl:pl-setf $i 0))
                                                      (                                            (pcl:pl-< $i $n))
                                                      (                                            (pl-post++ $i))
                                                ;; my $rvi = $rv->[$i]
                                                                                                (pcl:pl-setf $rvi (pl-aref-deref $rv $i))
                                                
                                                ;; $self->__push_value($ctx, \$rvi, $fields, $fflags, $ctx->{flag}, $fnum)
                                                                                                (pl-method-call $self '__push_value $ctx (pl-backslash $rvi) $fields $fflags (pl-gethash-deref $ctx "flag") $fnum)
                                                
                                              )
                                            )
                                            
                                            ;; return 1
                                                                                        (pl-return 1)
                                            
                                          )
                                          nil
                                        )
                                        
                                        ;; goto RESTART
                                                                                (pl-goto (pl-RESTART))
                                        
                                      )
                                      nil
                                    )
                                    
                                    ;; $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum)
                                                                        (pl-method-call $self '__push_value $ctx $v_ref $fields $fflags (pl-gethash-deref $ctx "flag") $fnum)
                                    
                                    ;; return 1
                                                                        (pl-return 1)
                                    
                                  )
                                  nil
                                )
                                
                              )
                              nil
                            )
                            
                            ;; $self->__error_inside_field($ctx, 2032)
                                                        (pl-method-call $self '__error_inside_field $ctx 2032)
                            
                            ;; return
                                                        (pl-return)
                            
                          )
                        )
                        
                      )
                      ;; else
                      (progn
                        ;; if ($ctx->{eolx} and $c eq $eol) {                     $c = '';                     goto EOLX;                 }
                        ;; if ($ctx->{"eolx"} and $c eq $eol)
                        (pl-if                         (pcl:pl-and (pl-gethash-deref $ctx "eolx") (pcl:pl-str-eq $c $eol))
                          (progn
                            ;; $c = ''
                                                        (pcl:pl-setf $c "")
                            
                            ;; goto EOLX
                                                        (pl-goto (pl-EOLX))
                            
                          )
                          nil
                        )
                        
                        ;; if ($waitingForField) {                     if (!$spl && $ctx->{comment_str} && $ctx->{tmp} =~ /\A$ctx->{comment_str}/) {                         $ctx->{used} = $ctx->{size};                         $ctx->{fld_idx} = 0;                         $seenSomething = 0;                         next LOOP;                     }                     if ($ctx->{allow_whitespace} and $self->__is_whitespace($ctx, $c)) {                         do {                             $c = $self->__get($ctx, $src);                             last if !defined $c;                         } while $self->__is_whitespace($ctx, $c);                         goto RESTART;                     }                     $waitingForField = 0;                     goto RESTART;                 }
                        ;; if ($waitingForField)
                        (pl-if                         $waitingForField
                          (progn
                            ;; if (!$spl && $ctx->{comment_str} && $ctx->{tmp} =~ /\A$ctx->{comment_str}/) {                         $ctx->{used} = $ctx->{size};                         $ctx->{fld_idx} = 0;                         $seenSomething = 0;                         next LOOP;                     }
                            ;; if (!$spl && $ctx->{"comment_str"} && $ctx->{"tmp"} =~ /\A$ctx->{comment_str}/)
                            (pl-if                             (pcl:pl-&& (pcl:pl-&& (pl-! $spl) (pl-gethash-deref $ctx "comment_str")) (pcl:pl-=~ (pl-gethash-deref $ctx "tmp") (pl-regex "/\\A$ctx->{comment_str}/")))
                              (progn
                                ;; $ctx->{used} = $ctx->{size}
                                                                (pcl:pl-setf (pl-gethash-deref $ctx "used") (pl-gethash-deref $ctx "size"))
                                
                                ;; $ctx->{fld_idx} = 0
                                                                (pcl:pl-setf (pl-gethash-deref $ctx "fld_idx") 0)
                                
                                ;; $seenSomething = 0
                                                                (pcl:pl-setf $seenSomething 0)
                                
                                ;; next LOOP
                                                                (pl-next LOOP)
                                
                              )
                              nil
                            )
                            
                            ;; if ($ctx->{allow_whitespace} and $self->__is_whitespace($ctx, $c)) {                         do {                             $c = $self->__get($ctx, $src);                             last if !defined $c;                         } while $self->__is_whitespace($ctx, $c);                         goto RESTART;                     }
                            ;; if ($ctx->{"allow_whitespace"} and $self->__is_whitespace($ctx, $c))
                            (pl-if                             (pcl:pl-and (pl-gethash-deref $ctx "allow_whitespace") (pl-method-call $self '__is_whitespace $ctx $c))
                              (progn
                                ;; do {                             $c = $self->__get($ctx, $src);                             last if !defined $c;                         } while $self->__is_whitespace($ctx, $c)
                                (pl-while                                 (pl-method-call $self '__is_whitespace $ctx $c) ;; PARSE ERROR: Fell through. Missing case: [)
                                
                                ;; goto RESTART
                                                                (pl-goto (pl-RESTART))
                                
                              )
                              nil
                            )
                            
                            ;; $waitingForField = 0
                                                        (pcl:pl-setf $waitingForField 0)
                            
                            ;; goto RESTART
                                                        (pl-goto (pl-RESTART))
                            
                          )
                          nil
                        )
                        
                        ;; if ($ctx->{flag} & IS_QUOTED) {                     if (!defined $c or $c =~ /[^\x09\x20-\x7E]/) {                         $ctx->{flag} |= IS_BINARY;                         unless ($ctx->{binary} or $ctx->{utf8}) {                             $self->__error_inside_quotes($ctx, 2026);                             return;                         }                     }                     $$v_ref .= $c;                 } else {                     if (!defined $c or $c =~ /[^\x09\x20-\x7E]/) {                         last if $ctx->{useIO} && !defined $c;                         $ctx->{flag} |= IS_BINARY;                         unless ($ctx->{binary} or $ctx->{utf8}) {                             $self->__error_inside_field($ctx, 2037);                             return;                         }                     }                     $$v_ref .= $c;                 }
                        ;; if ($ctx->{"flag"} & IS_QUOTED)
                        (pl-if                         (pcl:pl-bit-and (pl-gethash-deref $ctx "flag") (pl-IS_QUOTED))
                          (progn
                            ;; if (!defined $c or $c =~ /[^\x09\x20-\x7E]/) {                         $ctx->{flag} |= IS_BINARY;                         unless ($ctx->{binary} or $ctx->{utf8}) {                             $self->__error_inside_quotes($ctx, 2026);                             return;                         }                     }
                            ;; if (!defined $c or $c =~ /[^\x09\x20-\x7E]/)
                            (pl-if                             (pcl:pl-or (pl-! (pl-defined $c)) (pcl:pl-=~ $c (pl-regex "/[^\\x09\\x20-\\x7E]/")))
                              (progn
                                ;; $ctx->{flag} |= IS_BINARY
                                                                (pcl:pl-bit-or= (pl-gethash-deref $ctx "flag") (pl-IS_BINARY))
                                
                                ;; unless ($ctx->{binary} or $ctx->{utf8}) {                             $self->__error_inside_quotes($ctx, 2026);                             return;                         }
                                ;; unless ($ctx->{"binary"} or $ctx->{"utf8"})
                                (pl-if (pl-not                                 (pcl:pl-or (pl-gethash-deref $ctx "binary") (pl-gethash-deref $ctx "utf8")))
                                  (progn
                                    ;; $self->__error_inside_quotes($ctx, 2026)
                                                                        (pl-method-call $self '__error_inside_quotes $ctx 2026)
                                    
                                    ;; return
                                                                        (pl-return)
                                    
                                  )
                                  nil
                                )
                                
                              )
                              nil
                            )
                            
                            ;; $$v_ref .= $c
                                                        (pcl:pl-.= (pl-cast-$ $v_ref) $c)
                            
                          )
                          ;; else
                          (progn
                            ;; if (!defined $c or $c =~ /[^\x09\x20-\x7E]/) {                         last if $ctx->{useIO} && !defined $c;                         $ctx->{flag} |= IS_BINARY;                         unless ($ctx->{binary} or $ctx->{utf8}) {                             $self->__error_inside_field($ctx, 2037);                             return;                         }                     }
                            ;; if (!defined $c or $c =~ /[^\x09\x20-\x7E]/)
                            (pl-if                             (pcl:pl-or (pl-! (pl-defined $c)) (pcl:pl-=~ $c (pl-regex "/[^\\x09\\x20-\\x7E]/")))
                              (progn
                                ;; last if $ctx->{useIO} && !defined $c
                                (pl-if                                 (pcl:pl-&& (pl-gethash-deref $ctx "useIO") (pl-! (pl-defined $c)))                                 (pl-last))
                                
                                ;; $ctx->{flag} |= IS_BINARY
                                                                (pcl:pl-bit-or= (pl-gethash-deref $ctx "flag") (pl-IS_BINARY))
                                
                                ;; unless ($ctx->{binary} or $ctx->{utf8}) {                             $self->__error_inside_field($ctx, 2037);                             return;                         }
                                ;; unless ($ctx->{"binary"} or $ctx->{"utf8"})
                                (pl-if (pl-not                                 (pcl:pl-or (pl-gethash-deref $ctx "binary") (pl-gethash-deref $ctx "utf8")))
                                  (progn
                                    ;; $self->__error_inside_field($ctx, 2037)
                                                                        (pl-method-call $self '__error_inside_field $ctx 2037)
                                    
                                    ;; return
                                                                        (pl-return)
                                    
                                  )
                                  nil
                                )
                                
                              )
                              nil
                            )
                            
                            ;; $$v_ref .= $c
                                                        (pcl:pl-.= (pl-cast-$ $v_ref) $c)
                            
                          )
                        )
                        
                      )
                    )
                  )
                )
              )
            )
            
            ;; last LOOP if $ctx->{useIO} and $ctx->{verbatim} and $ctx->{used} == $ctx->{size}
            (pl-if             (pcl:pl-and (pcl:pl-and (pl-gethash-deref $ctx "useIO") (pl-gethash-deref $ctx "verbatim")) (pcl:pl-== (pl-gethash-deref $ctx "used") (pl-gethash-deref $ctx "size")))             (pl-last LOOP))
            
          )
          
        )
        
        ;; if ($waitingForField) {         if ($seenSomething or !$ctx->{useIO}) {             # new field             if (!$v_ref) {                 if ($ctx->{is_bound}) {                     $v_ref = $self->__bound_field($ctx, $fnum, 0);                 } else {                     $value = '';                     $v_ref = \$value;                 }                 $fnum++;                 return unless $v_ref;                 $ctx->{flag} = 0;                 $ctx->{fld_idx}++;             }             if ($ctx->{blank_is_undef} or $ctx->{empty_is_undef}) {                 $$v_ref = undef;             } else {                 $$v_ref = "";             }             unless ($ctx->{is_bound}) {                 push @$fields, $$v_ref;             }             if ($ctx->{keep_meta_info} and $fflags) {                 push @$fflags, $ctx->{flag};             }             return 1;         }         $self->SetDiag(2012);         return;     }
        ;; if ($waitingForField)
        (pl-if         $waitingForField
          (progn
            ;; if ($seenSomething or !$ctx->{useIO}) {             # new field             if (!$v_ref) {                 if ($ctx->{is_bound}) {                     $v_ref = $self->__bound_field($ctx, $fnum, 0);                 } else {                     $value = '';                     $v_ref = \$value;                 }                 $fnum++;                 return unless $v_ref;                 $ctx->{flag} = 0;                 $ctx->{fld_idx}++;             }             if ($ctx->{blank_is_undef} or $ctx->{empty_is_undef}) {                 $$v_ref = undef;             } else {                 $$v_ref = "";             }             unless ($ctx->{is_bound}) {                 push @$fields, $$v_ref;             }             if ($ctx->{keep_meta_info} and $fflags) {                 push @$fflags, $ctx->{flag};             }             return 1;         }
            ;; if ($seenSomething or !$ctx->{"useIO"})
            (pl-if             (pcl:pl-or $seenSomething (pl-! (pl-gethash-deref $ctx "useIO")))
              (progn
                ;; if (!$v_ref) {                 if ($ctx->{is_bound}) {                     $v_ref = $self->__bound_field($ctx, $fnum, 0);                 } else {                     $value = '';                     $v_ref = \$value;                 }                 $fnum++;                 return unless $v_ref;                 $ctx->{flag} = 0;                 $ctx->{fld_idx}++;             }
                ;; if (!$v_ref)
                (pl-if                 (pl-! $v_ref)
                  (progn
                    ;; if ($ctx->{is_bound}) {                     $v_ref = $self->__bound_field($ctx, $fnum, 0);                 } else {                     $value = '';                     $v_ref = \$value;                 }
                    ;; if ($ctx->{"is_bound"})
                    (pl-if                     (pl-gethash-deref $ctx "is_bound")
                      (progn
                        ;; $v_ref = $self->__bound_field($ctx, $fnum, 0)
                                                (pcl:pl-setf $v_ref (pl-method-call $self '__bound_field $ctx $fnum 0))
                        
                      )
                      ;; else
                      (progn
                        ;; $value = ''
                                                (pcl:pl-setf $value "")
                        
                        ;; $v_ref = \$value
                                                (pcl:pl-setf $v_ref (pl-backslash $value))
                        
                      )
                    )
                    
                    ;; $fnum++
                                        (pl-post++ $fnum)
                    
                    ;; return unless $v_ref
                    (pl-unless                     $v_ref                     (pl-return))
                    
                    ;; $ctx->{flag} = 0
                                        (pcl:pl-setf (pl-gethash-deref $ctx "flag") 0)
                    
                    ;; $ctx->{fld_idx}++
                                        (pl-post++ (pl-gethash-deref $ctx "fld_idx"))
                    
                  )
                  nil
                )
                
                ;; if ($ctx->{blank_is_undef} or $ctx->{empty_is_undef}) {                 $$v_ref = undef;             } else {                 $$v_ref = "";             }
                ;; if ($ctx->{"blank_is_undef"} or $ctx->{"empty_is_undef"})
                (pl-if                 (pcl:pl-or (pl-gethash-deref $ctx "blank_is_undef") (pl-gethash-deref $ctx "empty_is_undef"))
                  (progn
                    ;; $$v_ref = undef
                                        (pcl:pl-setf (pl-cast-$ $v_ref) (pl-undef))
                    
                  )
                  ;; else
                  (progn
                    ;; $$v_ref = ""
                                        (pcl:pl-setf (pl-cast-$ $v_ref) "")
                    
                  )
                )
                
                ;; unless ($ctx->{is_bound}) {                 push @$fields, $$v_ref;             }
                ;; unless ($ctx->{"is_bound"})
                (pl-if (pl-not                 (pl-gethash-deref $ctx "is_bound"))
                  (progn
                    ;; push @$fields, $$v_ref
                                        (pl-push (pl-cast-@ $fields) (pl-cast-$ $v_ref))
                    
                  )
                  nil
                )
                
                ;; if ($ctx->{keep_meta_info} and $fflags) {                 push @$fflags, $ctx->{flag};             }
                ;; if ($ctx->{"keep_meta_info"} and $fflags)
                (pl-if                 (pcl:pl-and (pl-gethash-deref $ctx "keep_meta_info") $fflags)
                  (progn
                    ;; push @$fflags, $ctx->{flag}
                                        (pl-push (pl-cast-@ $fflags) (pl-gethash-deref $ctx "flag"))
                    
                  )
                  nil
                )
                
                ;; return 1
                                (pl-return 1)
                
              )
              nil
            )
            
            ;; $self->SetDiag(2012)
                        (pl-method-call $self 'SetDiag 2012)
            
            ;; return
                        (pl-return)
            
          )
          nil
        )
        
        ;; if ($ctx->{flag} & IS_QUOTED) {         $self->__error_inside_quotes($ctx, 2027);         return;     }
        ;; if ($ctx->{"flag"} & IS_QUOTED)
        (pl-if         (pcl:pl-bit-and (pl-gethash-deref $ctx "flag") (pl-IS_QUOTED))
          (progn
            ;; $self->__error_inside_quotes($ctx, 2027)
                        (pl-method-call $self '__error_inside_quotes $ctx 2027)
            
            ;; return
                        (pl-return)
            
          )
          nil
        )
        
        ;; if ($v_ref) {         $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum);     } elsif ($ctx->{flag} == 0 && $fnum == 1 && $ctx->{skip_empty_rows} == 1) {         return undef;     }
        ;; if ($v_ref)
        (pl-if         $v_ref
          (progn
            ;; $self->__push_value($ctx, $v_ref, $fields, $fflags, $ctx->{flag}, $fnum)
                        (pl-method-call $self '__push_value $ctx $v_ref $fields $fflags (pl-gethash-deref $ctx "flag") $fnum)
            
          )
          ;; elsif ($ctx->{"flag"} == 0 && $fnum == 1 && $ctx->{"skip_empty_rows"} == 1)
          (pl-if           (pcl:pl-&& (pcl:pl-&& (pcl:pl-== (pl-gethash-deref $ctx "flag") 0) (pcl:pl-== $fnum 1)) (pcl:pl-== (pl-gethash-deref $ctx "skip_empty_rows") 1))
            (progn
              ;; return undef
                            (pl-return (pl-undef))
              
            )
            nil
          )
        )
        
        ;; return 1
                (pl-return 1)
        
      )
    )
  )
)

;; sub __get_from_src { ... }
(pl-sub pl-__get_from_src (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($ctx (make-pl-box nil)) ($src (make-pl-box nil)) ($res (make-pl-box nil)))
        ;; my ($self, $ctx, $src) = @_
                (pcl:pl-setf (vector $self $ctx $src) @_)
        
        ;; return 1 if defined $ctx->{tmp} and $ctx->{used} <= 0
        (pl-if         (pcl:pl-and (pl-gethash-deref (pl-defined $ctx) "tmp") (pcl:pl-<= (pl-gethash-deref $ctx "used") 0))         (pl-return 1))
        
        ;; return 1 if $ctx->{used} < $ctx->{size}
        (pl-if         (pcl:pl-< (pl-gethash-deref $ctx "used") (pl-gethash-deref $ctx "size"))         (pl-return 1))
        
        ;; return unless $ctx->{useIO}
        (pl-unless         (pl-gethash-deref $ctx "useIO")         (pl-return))
        
        ;; my $res = $src->getline
                (pcl:pl-setf $res (pl-method-call $src 'getline))
        
        ;; if (defined $res) {         if ($ctx->{has_ahead}) {             $ctx->{tmp} = $self->{_AHEAD};             $ctx->{tmp} .= $ctx->{eol} if $ctx->{eol_len};             $ctx->{tmp} .= $res;             $ctx->{has_ahead} = 0;         } else {             $ctx->{tmp} = $res;         }         if ($ctx->{size} = length $ctx->{tmp}) {             $ctx->{used} = -1;             $ctx->{utf8} = 1 if utf8::is_utf8($ctx->{tmp});             pos($ctx->{tmp}) = 0;             return 1;         }     } elsif (delete $ctx->{has_leftover}) {         $ctx->{tmp} = $self->{_AHEAD};         $ctx->{has_ahead} = 0;         $ctx->{useIO} |= useIO_EOF;         if ($ctx->{size} = length $ctx->{tmp}) {             $ctx->{used} = -1;             $ctx->{utf8} = 1 if utf8::is_utf8($ctx->{tmp});             pos($ctx->{tmp}) = 0;             return 1;         }     }
        ;; if (defined $res)
        (pl-if         (pl-defined $res)
          (progn
            ;; if ($ctx->{has_ahead}) {             $ctx->{tmp} = $self->{_AHEAD};             $ctx->{tmp} .= $ctx->{eol} if $ctx->{eol_len};             $ctx->{tmp} .= $res;             $ctx->{has_ahead} = 0;         } else {             $ctx->{tmp} = $res;         }
            ;; if ($ctx->{"has_ahead"})
            (pl-if             (pl-gethash-deref $ctx "has_ahead")
              (progn
                ;; $ctx->{tmp} = $self->{_AHEAD}
                                (pcl:pl-setf (pl-gethash-deref $ctx "tmp") (pl-gethash-deref $self "_AHEAD"))
                
                ;; $ctx->{tmp} .= $ctx->{eol} if $ctx->{eol_len}
                (pl-if                 (pl-gethash-deref $ctx "eol_len")                 (pcl:pl-.= (pl-gethash-deref $ctx "tmp") (pl-gethash-deref $ctx "eol")))
                
                ;; $ctx->{tmp} .= $res
                                (pcl:pl-.= (pl-gethash-deref $ctx "tmp") $res)
                
                ;; $ctx->{has_ahead} = 0
                                (pcl:pl-setf (pl-gethash-deref $ctx "has_ahead") 0)
                
              )
              ;; else
              (progn
                ;; $ctx->{tmp} = $res
                                (pcl:pl-setf (pl-gethash-deref $ctx "tmp") $res)
                
              )
            )
            
            ;; if ($ctx->{size} = length $ctx->{tmp}) {             $ctx->{used} = -1;             $ctx->{utf8} = 1 if utf8::is_utf8($ctx->{tmp});             pos($ctx->{tmp}) = 0;             return 1;         }
            ;; if ($ctx->{"size"} = length $ctx->{"tmp"})
            (pl-if             (pcl:pl-setf (pl-gethash-deref $ctx "size") (pl-length (pl-gethash-deref $ctx "tmp")))
              (progn
                ;; $ctx->{used} = -1
                                (pcl:pl-setf (pl-gethash-deref $ctx "used") -1)
                
                ;; $ctx->{utf8} = 1 if utf8::is_utf8($ctx->{tmp})
                (pl-if                 (utf8::pl-is_utf8 (pl-gethash-deref $ctx "tmp"))                 (pcl:pl-setf (pl-gethash-deref $ctx "utf8") 1))
                
                ;; pos($ctx->{tmp}) = 0
                                (pcl:pl-setf (pl-pos (pl-gethash-deref $ctx "tmp")) 0)
                
                ;; return 1
                                (pl-return 1)
                
              )
              nil
            )
            
          )
          ;; elsif (delete $ctx->{"has_leftover"})
          (pl-if           (pl-gethash-deref (pl-delete $ctx) "has_leftover")
            (progn
              ;; $ctx->{tmp} = $self->{_AHEAD}
                            (pcl:pl-setf (pl-gethash-deref $ctx "tmp") (pl-gethash-deref $self "_AHEAD"))
              
              ;; $ctx->{has_ahead} = 0
                            (pcl:pl-setf (pl-gethash-deref $ctx "has_ahead") 0)
              
              ;; $ctx->{useIO} |= useIO_EOF
                            (pcl:pl-bit-or= (pl-gethash-deref $ctx "useIO") (pl-useIO_EOF))
              
              ;; if ($ctx->{size} = length $ctx->{tmp}) {             $ctx->{used} = -1;             $ctx->{utf8} = 1 if utf8::is_utf8($ctx->{tmp});             pos($ctx->{tmp}) = 0;             return 1;         }
              ;; if ($ctx->{"size"} = length $ctx->{"tmp"})
              (pl-if               (pcl:pl-setf (pl-gethash-deref $ctx "size") (pl-length (pl-gethash-deref $ctx "tmp")))
                (progn
                  ;; $ctx->{used} = -1
                                    (pcl:pl-setf (pl-gethash-deref $ctx "used") -1)
                  
                  ;; $ctx->{utf8} = 1 if utf8::is_utf8($ctx->{tmp})
                  (pl-if                   (utf8::pl-is_utf8 (pl-gethash-deref $ctx "tmp"))                   (pcl:pl-setf (pl-gethash-deref $ctx "utf8") 1))
                  
                  ;; pos($ctx->{tmp}) = 0
                                    (pcl:pl-setf (pl-pos (pl-gethash-deref $ctx "tmp")) 0)
                  
                  ;; return 1
                                    (pl-return 1)
                  
                )
                nil
              )
              
            )
            nil
          )
        )
        
        ;; $ctx->{tmp} = '' unless defined $ctx->{tmp}
        (pl-unless         (pl-gethash-deref (pl-defined $ctx) "tmp")         (pcl:pl-setf (pl-gethash-deref $ctx "tmp") ""))
        
        ;; $ctx->{useIO} |= useIO_EOF
                (pcl:pl-bit-or= (pl-gethash-deref $ctx "useIO") (pl-useIO_EOF))
        
        ;; return
                (pl-return)
        
      )
    )
  )
)

;; sub __set_eol_is_cr { ... }
(pl-sub pl-__set_eol_is_cr (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($ctx (make-pl-box nil)))
        ;; my ($self, $ctx) = @_
                (pcl:pl-setf (vector $self $ctx) @_)
        
        ;; $ctx->{eol} = "\015"
                (pcl:pl-setf (pl-gethash-deref $ctx "eol") " 15")
        
        ;; $ctx->{eol_is_cr} = 1
                (pcl:pl-setf (pl-gethash-deref $ctx "eol_is_cr") 1)
        
        ;; $ctx->{eol_len} = 1
                (pcl:pl-setf (pl-gethash-deref $ctx "eol_len") 1)
        
        ;; %{$self->{_CACHE}} = %$ctx
                (pcl:pl-setf (pl-cast-% (pl-gethash-deref $self "_CACHE")) (pl-cast-% $ctx))
        
        ;; $self->{eol} = $ctx->{eol}
                (pcl:pl-setf (pl-gethash-deref $self "eol") (pl-gethash-deref $ctx "eol"))
        
      )
    )
  )
)

;; sub __bound_field { ... }
(pl-sub pl-__bound_field (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($ctx (make-pl-box nil)) ($i (make-pl-box nil)) ($keep (make-pl-box nil)) ($ref (make-pl-box nil)))
        ;; my ($self, $ctx, $i, $keep) = @_
                (pcl:pl-setf (vector $self $ctx $i $keep) @_)
        
        ;; if ($i >= $ctx->{is_bound}) {         $self->SetDiag(3006);         return;     }
        ;; if ($i >= $ctx->{"is_bound"})
        (pl-if         (pcl:pl->= $i (pl-gethash-deref $ctx "is_bound"))
          (progn
            ;; $self->SetDiag(3006)
                        (pl-method-call $self 'SetDiag 3006)
            
            ;; return
                        (pl-return)
            
          )
          nil
        )
        
        ;; if (ref $ctx->{bound} eq 'ARRAY') {         my $ref = $ctx->{bound}[$i];         if (ref $ref) {             if ($keep) {                 return $ref;             }             unless (Scalar::Util::readonly($$ref)) {                 $$ref = "";                 return $ref;             }         }     }
        ;; if (ref $ctx->{"bound"} eq 'ARRAY')
        (pl-if         (pcl:pl-str-eq (pl-gethash-deref (pl-ref $ctx) "bound") "ARRAY")
          (progn
            ;; my $ref = $ctx->{bound}[$i]
                        (pcl:pl-setf $ref (pl-aref (pl-gethash-deref $ctx "bound") $i))
            
            ;; if (ref $ref) {             if ($keep) {                 return $ref;             }             unless (Scalar::Util::readonly($$ref)) {                 $$ref = "";                 return $ref;             }         }
            ;; if (ref $ref)
            (pl-if             (pl-ref $ref)
              (progn
                ;; if ($keep) {                 return $ref;             }
                ;; if ($keep)
                (pl-if                 $keep
                  (progn
                    ;; return $ref
                                        (pl-return $ref)
                    
                  )
                  nil
                )
                
                ;; unless (Scalar::Util::readonly($$ref)) {                 $$ref = "";                 return $ref;             }
                ;; unless (Scalar::Util::readonly($$ref))
                (pl-if (pl-not                 (|Scalar::Util|::pl-readonly (pl-cast-$ $ref)))
                  (progn
                    ;; $$ref = ""
                                        (pcl:pl-setf (pl-cast-$ $ref) "")
                    
                    ;; return $ref
                                        (pl-return $ref)
                    
                  )
                  nil
                )
                
              )
              nil
            )
            
          )
          nil
        )
        
        ;; $self->SetDiag(3008)
                (pl-method-call $self 'SetDiag 3008)
        
        ;; return
                (pl-return)
        
      )
    )
  )
)

;; sub __get { ... }
(pl-sub pl-__get (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($ctx (make-pl-box nil)) ($src (make-pl-box nil)) ($pos (make-pl-box nil)) ($c (make-pl-box nil)))
        ;; my ($self, $ctx, $src) = @_
                (pcl:pl-setf (vector $self $ctx $src) @_)
        
        ;; return unless defined $ctx->{used}
        (pl-unless         (pl-gethash-deref (pl-defined $ctx) "used")         (pl-return))
        
        ;; if ($ctx->{used} >= $ctx->{size}) {         if ($self->__get_from_src($ctx, $src)) {             return $self->__get($ctx, $src);         }         return;     }
        ;; if ($ctx->{"used"} >= $ctx->{"size"})
        (pl-if         (pcl:pl->= (pl-gethash-deref $ctx "used") (pl-gethash-deref $ctx "size"))
          (progn
            ;; if ($self->__get_from_src($ctx, $src)) {             return $self->__get($ctx, $src);         }
            ;; if ($self->__get_from_src($ctx, $src))
            (pl-if             (pl-method-call $self '__get_from_src $ctx $src)
              (progn
                ;; return $self->__get($ctx, $src)
                                (pl-return (pl-method-call $self '__get $ctx $src))
                
              )
              nil
            )
            
            ;; return
                        (pl-return)
            
          )
          nil
        )
        
        ;; my $pos = pos($ctx->{tmp})
                (pcl:pl-setf $pos (pl-pos (pl-gethash-deref $ctx "tmp")))
        
        ;; if ($ctx->{tmp} =~ /\G($ctx->{_re}|.)/gs) {         my $c = $1;         if ($c =~ /[^\x09\012\015\x20-\x7e]/) {             $ctx->{flag} |= IS_BINARY;         }         $ctx->{used} = pos($ctx->{tmp});         return $c;     } else {         if ($self->__get_from_src($ctx, $src)) {             return $self->__get($ctx, $src);         }         pos($ctx->{tmp}) = $pos;         return;     }
        ;; if ($ctx->{"tmp"} =~ /\G($ctx->{_re}|.)/gs)
        (pl-if         (pcl:pl-=~ (pl-gethash-deref $ctx "tmp") (pl-regex "/\\G($ctx->{_re}|.)/gs"))
          (progn
            ;; my $c = $1
                        (pcl:pl-setf $c $1)
            
            ;; if ($c =~ /[^\x09\012\015\x20-\x7e]/) {             $ctx->{flag} |= IS_BINARY;         }
            ;; if ($c =~ /[^\x09\012\015\x20-\x7e]/)
            (pl-if             (pcl:pl-=~ $c (pl-regex "/[^\\x09\\012\\015\\x20-\\x7e]/"))
              (progn
                ;; $ctx->{flag} |= IS_BINARY
                                (pcl:pl-bit-or= (pl-gethash-deref $ctx "flag") (pl-IS_BINARY))
                
              )
              nil
            )
            
            ;; $ctx->{used} = pos($ctx->{tmp})
                        (pcl:pl-setf (pl-gethash-deref $ctx "used") (pl-pos (pl-gethash-deref $ctx "tmp")))
            
            ;; return $c
                        (pl-return $c)
            
          )
          ;; else
          (progn
            ;; if ($self->__get_from_src($ctx, $src)) {             return $self->__get($ctx, $src);         }
            ;; if ($self->__get_from_src($ctx, $src))
            (pl-if             (pl-method-call $self '__get_from_src $ctx $src)
              (progn
                ;; return $self->__get($ctx, $src)
                                (pl-return (pl-method-call $self '__get $ctx $src))
                
              )
              nil
            )
            
            ;; pos($ctx->{tmp}) = $pos
                        (pcl:pl-setf (pl-pos (pl-gethash-deref $ctx "tmp")) $pos)
            
            ;; return
                        (pl-return)
            
          )
        )
        
      )
    )
  )
)

;; sub __error_inside_quotes { ... }
(pl-sub pl-__error_inside_quotes (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($ctx (make-pl-box nil)) ($error (make-pl-box nil)))
        ;; my ($self, $ctx, $error) = @_
                (pcl:pl-setf (vector $self $ctx $error) @_)
        
        ;; $self->__parse_error($ctx, $error, $ctx->{used} - 1)
                (pl-method-call $self '__parse_error $ctx $error (pcl:pl-- (pl-gethash-deref $ctx "used") 1))
        
      )
    )
  )
)

;; sub __error_inside_field { ... }
(pl-sub pl-__error_inside_field (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($ctx (make-pl-box nil)) ($error (make-pl-box nil)))
        ;; my ($self, $ctx, $error) = @_
                (pcl:pl-setf (vector $self $ctx $error) @_)
        
        ;; $self->__parse_error($ctx, $error, $ctx->{used} - 1)
                (pl-method-call $self '__parse_error $ctx $error (pcl:pl-- (pl-gethash-deref $ctx "used") 1))
        
      )
    )
  )
)

;; sub __parse_error { ... }
(pl-sub pl-__parse_error (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($ctx (make-pl-box nil)) ($error (make-pl-box nil)) ($pos (make-pl-box nil)))
        ;; my ($self, $ctx, $error, $pos) = @_
                (pcl:pl-setf (vector $self $ctx $error $pos) @_)
        
        ;; $self->{_ERROR_POS} = $pos
                (pcl:pl-setf (pl-gethash-deref $self "_ERROR_POS") $pos)
        
        ;; $self->{_ERROR_FLD} = $ctx->{fld_idx}
                (pcl:pl-setf (pl-gethash-deref $self "_ERROR_FLD") (pl-gethash-deref $ctx "fld_idx"))
        
        ;; $self->{_ERROR_INPUT} = $ctx->{tmp} if $ctx->{tmp}
        (pl-if         (pl-gethash-deref $ctx "tmp")         (pcl:pl-setf (pl-gethash-deref $self "_ERROR_INPUT") (pl-gethash-deref $ctx "tmp")))
        
        ;; $self->SetDiag($error)
                (pl-method-call $self 'SetDiag $error)
        
        ;; return
                (pl-return)
        
      )
    )
  )
)

;; sub __is_whitespace { ... }
(pl-sub pl-__is_whitespace (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($ctx (make-pl-box nil)) ($c (make-pl-box nil)))
        ;; my ($self, $ctx, $c) = @_
                (pcl:pl-setf (vector $self $ctx $c) @_)
        
        ;; return unless defined $c
        (pl-unless         (pl-defined $c)         (pl-return))
        
        ;; return (         (!defined $ctx->{sep} or $c ne $ctx->{sep}) &&         (!defined $ctx->{quo} or $c ne $ctx->{quo}) &&         (!defined $ctx->{escape_char} or $c ne $ctx->{escape_char}) &&         ($c eq " " or $c eq "\t")     )
                (pl-return (pcl:pl-&& (pcl:pl-&& (pcl:pl-&& (pcl:pl-or (pl-! (pl-gethash-deref (pl-defined $ctx) "sep")) (pcl:pl-str-ne $c (pl-gethash-deref $ctx "sep"))) (pcl:pl-or (pl-! (pl-gethash-deref (pl-defined $ctx) "quo")) (pcl:pl-str-ne $c (pl-gethash-deref $ctx "quo")))) (pcl:pl-or (pl-! (pl-gethash-deref (pl-defined $ctx) "escape_char")) (pcl:pl-str-ne $c (pl-gethash-deref $ctx "escape_char")))) (pcl:pl-or (pcl:pl-str-eq $c " ") (pcl:pl-str-eq $c "	"))))
        
      )
    )
  )
)

;; sub __push_value { ... }
(pl-sub pl-__push_value (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($ctx (make-pl-box nil)) ($v_ref (make-pl-box nil)) ($fields (make-pl-box nil)) ($fflags (make-pl-box nil)) ($flag (make-pl-box nil)) ($fnum (make-pl-box nil)) ($value (make-pl-box nil)))
        ;; my ($self, $ctx, $v_ref, $fields, $fflags, $flag, $fnum) = @_
                (pcl:pl-setf (vector $self $ctx $v_ref $fields $fflags $flag $fnum) @_)
        
        ;; utf8::encode($$v_ref) if $ctx->{utf8}
        (pl-if         (pl-gethash-deref $ctx "utf8")         (utf8::pl-encode (pl-cast-$ $v_ref)))
        
        ;; if ($ctx->{formula} && defined $$v_ref && substr($$v_ref, 0, 1) eq '=') {         my $value = $self->_formula($ctx, $$v_ref, $fnum);         push @$fields, defined $value ? $value : undef;         return;     }
        ;; if ($ctx->{"formula"} && defined $$v_ref && substr($$v_ref, 0, 1) eq '=')
        (pl-if ;; PARSE ERROR: Fell through. Missing case: [
          (progn
            ;; my $value = $self->_formula($ctx, $$v_ref, $fnum)
                        (pcl:pl-setf $value (pl-method-call $self '_formula $ctx (pl-cast-$ $v_ref) $fnum))
            
            ;; push @$fields, defined $value ? $value : undef
                        (pl-push (pl-cast-@ $fields) (pl-if (pl-defined $value) $value (pl-undef)))
            
            ;; return
                        (pl-return)
            
          )
          nil
        )
        
        ;; if (         (!defined $$v_ref or $$v_ref eq '') and         ($ctx->{empty_is_undef} or (!($flag & IS_QUOTED) and $ctx->{blank_is_undef}))     ) {         $$v_ref = undef;     } else {         if ($ctx->{allow_whitespace} && !($flag & IS_QUOTED)) {             $$v_ref =~ s/[ \t]+$//;         }         if ($flag & IS_BINARY and $ctx->{decode_utf8} and ($ctx->{utf8} || _is_valid_utf8($$v_ref))) {             utf8::decode($$v_ref);         }     }
        ;; if ((!defined $$v_ref or $$v_ref eq '') and
;;         ($ctx->{empty_is_undef} or (!($flag & IS_QUOTED) and $ctx->{blank_is_undef})))
        (pl-if ;; PARSE ERROR: Fell through. Missing case: [
          (progn
            ;; $$v_ref = undef
                        (pcl:pl-setf (pl-cast-$ $v_ref) (pl-undef))
            
          )
          ;; else
          (progn
            ;; if ($ctx->{allow_whitespace} && !($flag & IS_QUOTED)) {             $$v_ref =~ s/[ \t]+$//;         }
            ;; if ($ctx->{"allow_whitespace"} && !($flag & IS_QUOTED))
            (pl-if             (pcl:pl-&& (pl-gethash-deref $ctx "allow_whitespace") (pl-! (pcl:pl-bit-and $flag (pl-IS_QUOTED))))
              (progn
                ;; $$v_ref =~ s/[ \t]+$//
                                (pcl:pl-=~ (pl-cast-$ $v_ref) (pl-subst "[ \\t]+$" ""))
                
              )
              nil
            )
            
            ;; if ($flag & IS_BINARY and $ctx->{decode_utf8} and ($ctx->{utf8} || _is_valid_utf8($$v_ref))) {             utf8::decode($$v_ref);         }
            ;; if ($flag & IS_BINARY and $ctx->{"decode_utf8"} and ($ctx->{"utf8"} || _is_valid_utf8($$v_ref)))
            (pl-if             (pcl:pl-and (pcl:pl-and (pcl:pl-bit-and $flag (pl-IS_BINARY)) (pl-gethash-deref $ctx "decode_utf8")) (pcl:pl-|| (pl-gethash-deref $ctx "utf8") (pl-_is_valid_utf8 (pl-cast-$ $v_ref))))
              (progn
                ;; utf8::decode($$v_ref)
                                (utf8::pl-decode (pl-cast-$ $v_ref))
                
              )
              nil
            )
            
          )
        )
        
        ;; unless ($ctx->{is_bound}) {         push @$fields, $$v_ref;     }
        ;; unless ($ctx->{"is_bound"})
        (pl-if (pl-not         (pl-gethash-deref $ctx "is_bound"))
          (progn
            ;; push @$fields, $$v_ref
                        (pl-push (pl-cast-@ $fields) (pl-cast-$ $v_ref))
            
          )
          nil
        )
        
        ;; if ($ctx->{keep_meta_info} and $fflags) {         push @$fflags, $flag;     }
        ;; if ($ctx->{"keep_meta_info"} and $fflags)
        (pl-if         (pcl:pl-and (pl-gethash-deref $ctx "keep_meta_info") $fflags)
          (progn
            ;; push @$fflags, $flag
                        (pl-push (pl-cast-@ $fflags) $flag)
            
          )
          nil
        )
        
      )
    )
  )
)

;; sub getline { ... }
(pl-sub pl-getline (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($io (make-pl-box nil)) (@fields (make-pl-box nil)) (@fflags (make-pl-box nil)) ($res (make-pl-box nil)))
        ;; my ($self, $io) = @_
                (pcl:pl-setf (vector $self $io) @_)
        
        ;; my (@fields, @fflags) (bare declaration)
        
        ;; my $res = $self->__parse(\@fields, \@fflags, $io, 1)
                (pcl:pl-setf $res (pl-method-call $self '__parse (pl-backslash @fields) (pl-backslash @fflags) $io 1))
        
        ;; $res ? \@fields : undef
                (pl-if $res (pl-backslash @fields) (pl-undef))
        
      )
    )
  )
)

;; sub getline_all { ... }
(pl-sub pl-getline_all (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($io (make-pl-box nil)) ($offset (make-pl-box nil)) ($len (make-pl-box nil)) ($ctx (make-pl-box nil)) ($tail (make-pl-box nil)) ($n (make-pl-box nil)) (@row (make-pl-box nil)) (@list (make-pl-box nil)))
        ;; my ( $self, $io, $offset, $len ) = @_
                (pcl:pl-setf (vector $self $io $offset $len) @_)
        
        ;; my $ctx = $self->_setup_ctx
                (pcl:pl-setf $ctx (pl-method-call $self '_setup_ctx))
        
        ;; my $tail = 0
                (pcl:pl-setf $tail 0)
        
        ;; my $n = 0
                (pcl:pl-setf $n 0)
        
        ;; $offset ||= 0
                (pcl:pl-or-assign $offset 0)
        
        ;; if ( $offset < 0 ) {         $tail = -$offset;         $offset = -1;     }
        ;; if ($offset < 0)
        (pl-if         (pcl:pl-< $offset 0)
          (progn
            ;; $tail = -$offset
                        (pcl:pl-setf $tail (pl-- $offset))
            
            ;; $offset = -1
                        (pcl:pl-setf $offset -1)
            
          )
          nil
        )
        
        ;; my (@row, @list) (bare declaration)
        
        ;; while ($self->___parse($ctx, \@row, undef, $io, 1)) {         $ctx = $self->_setup_ctx;          if ($offset > 0) {             $offset--;             @row = ();             next;         }         if ($n++ >= $tail and $tail) {             shift @list;             $n--;         }         if (($ctx->{has_hooks} || 0) & HOOK_AFTER_PARSE) {             unless ($self->_hook(after_parse => \@row)) {                 @row = ();                 next;             }         }         push @list, [@row];         @row = ();          last if defined $len && $n >= $len and $offset >= 0;   # exceeds limit size     }
        (pl-while         (pl-method-call $self '___parse $ctx (pl-backslash @row) (pl-undef) $io 1)
          ;; $ctx = $self->_setup_ctx
                    (pcl:pl-setf $ctx (pl-method-call $self '_setup_ctx))
          
          ;; if ($offset > 0) {             $offset--;             @row = ();             next;         }
          ;; if ($offset > 0)
          (pl-if           (pcl:pl-> $offset 0)
            (progn
              ;; $offset--
                            (pl-post-- $offset)
              
              ;; @row = ()
                            (pcl:pl-setf @row (vector ))
              
              ;; next
                            (pl-next)
              
            )
            nil
          )
          
          ;; if ($n++ >= $tail and $tail) {             shift @list;             $n--;         }
          ;; if ($n++ >= $tail and $tail)
          (pl-if           (pcl:pl-and (pcl:pl->= (pl-post++ $n) $tail) $tail)
            (progn
              ;; shift @list
                            (pl-shift @list)
              
              ;; $n--
                            (pl-post-- $n)
              
            )
            nil
          )
          
          ;; if (($ctx->{has_hooks} || 0) & HOOK_AFTER_PARSE) {             unless ($self->_hook(after_parse => \@row)) {                 @row = ();                 next;             }         }
          ;; if (($ctx->{"has_hooks"} || 0) & HOOK_AFTER_PARSE)
          (pl-if           (pcl:pl-bit-and (pcl:pl-|| (pl-gethash-deref $ctx "has_hooks") 0) (pl-HOOK_AFTER_PARSE))
            (progn
              ;; unless ($self->_hook(after_parse => \@row)) {                 @row = ();                 next;             }
              ;; unless ($self->_hook(after_parse , \@row))
              (pl-if (pl-not               (pl-method-call $self '_hook "after_parse" (pl-backslash @row)))
                (progn
                  ;; @row = ()
                                    (pcl:pl-setf @row (vector ))
                  
                  ;; next
                                    (pl-next)
                  
                )
                nil
              )
              
            )
            nil
          )
          
          ;; push @list, [@row]
                    (pl-push @list (make-array 1 :adjustable t :fill-pointer t :initial-contents (list @row)))
          
          ;; @row = ()
                    (pcl:pl-setf @row (vector ))
          
          ;; last if defined $len && $n >= $len and $offset >= 0
          (pl-if           (pcl:pl-and (pcl:pl-&& (pl-defined $len) (pcl:pl->= $n $len)) (pcl:pl->= $offset 0))           (pl-last))
          
        )
        
        ;; if ( defined $len && $n > $len ) {         @list = splice( @list, 0, $len);     }
        ;; if (defined $len && $n > $len)
        (pl-if         (pcl:pl-&& (pl-defined $len) (pcl:pl-> $n $len))
          (progn
            ;; @list = splice( @list, 0, $len)
                        (pcl:pl-setf @list (let ((*wantarray* t)) (pl-splice @list 0 $len)))
            
          )
          nil
        )
        
        ;; return \@list
                (pl-return (pl-backslash @list))
        
      )
    )
  )
)

;; sub _is_valid_utf8 { ... }
(pl-sub pl-_is_valid_utf8 (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; return ( $_[0] =~ /^(?:          [\x00-\x7F]         |[\xC2-\xDF][\x80-\xBF]         |[\xE0][\xA0-\xBF][\x80-\xBF]         |[\xE1-\xEC][\x80-\xBF][\x80-\xBF]         |[\xED][\x80-\x9F][\x80-\xBF]         |[\xEE-\xEF][\x80-\xBF][\x80-\xBF]         |[\xF0][\x90-\xBF][\x80-\xBF][\x80-\xBF]         |[\xF1-\xF3][\x80-\xBF][\x80-\xBF][\x80-\xBF]         |[\xF4][\x80-\x8F][\x80-\xBF][\x80-\xBF]     )+$/x )  ? 1 : 0
            (pl-if (pl-return (pcl:pl-=~ (pl-aref @_ 0) (pl-regex "/^(?:
         [\\x00-\\x7F]
        |[\\xC2-\\xDF][\\x80-\\xBF]
        |[\\xE0][\\xA0-\\xBF][\\x80-\\xBF]
        |[\\xE1-\\xEC][\\x80-\\xBF][\\x80-\\xBF]
        |[\\xED][\\x80-\\x9F][\\x80-\\xBF]
        |[\\xEE-\\xEF][\\x80-\\xBF][\\x80-\\xBF]
        |[\\xF0][\\x90-\\xBF][\\x80-\\xBF][\\x80-\\xBF]
        |[\\xF1-\\xF3][\\x80-\\xBF][\\x80-\\xBF][\\x80-\\xBF]
        |[\\xF4][\\x80-\\x8F][\\x80-\\xBF][\\x80-\\xBF]
    )+$/x"))) 1 0)
      
    )
  )
)

;; ################################################################################
;; # methods for errors
;; ################################################################################
;; sub _set_error_diag { ... }
(pl-sub pl-_set_error_diag (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($error (make-pl-box nil)) ($pos (make-pl-box nil)))
        ;; my ( $self, $error, $pos ) = @_
                (pcl:pl-setf (vector $self $error $pos) @_)
        
        ;; $self->SetDiag($error)
                (pl-method-call $self 'SetDiag $error)
        
        ;; if (defined $pos) {         $_[0]->{_ERROR_POS} = $pos;     }
        ;; if (defined $pos)
        (pl-if         (pl-defined $pos)
          (progn
            ;; $_[0]->{_ERROR_POS} = $pos
                        (pcl:pl-setf (pl-gethash (pl-aref @_ 0) "_ERROR_POS") $pos)
            
          )
          nil
        )
        
        ;; return
                (pl-return)
        
      )
    )
  )
)

;; sub error_input { ... }
(pl-sub pl-error_input (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)))
        ;; my $self = shift
                (pcl:pl-setf $self (pl-shift @_))
        
        ;; if ($self and ((Scalar::Util::reftype($self) || '') eq 'HASH' or (ref $self) =~ /^Text::CSV/)) {         return $self->{_ERROR_INPUT};     }
        ;; if ($self and ((Scalar::Util::reftype($self) || '') eq 'HASH' or (ref $self) =~ /^Text::CSV/))
        (pl-if         (pcl:pl-and $self (pcl:pl-or (pcl:pl-str-eq (pcl:pl-|| (|Scalar::Util|::pl-reftype $self) "") "HASH") (pcl:pl-=~ (pl-ref $self) (pl-regex "/^Text::CSV/"))))
          (progn
            ;; return $self->{_ERROR_INPUT}
                        (pl-return (pl-gethash-deref $self "_ERROR_INPUT"))
            
          )
          nil
        )
        
        ;; return
                (pl-return)
        
      )
    )
  )
)

;; sub _sv_diag { ... }
(pl-sub pl-_sv_diag (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($error (make-pl-box nil)))
        ;; my ($self, $error) = @_
                (pcl:pl-setf (vector $self $error) @_)
        
        ;; bless [$error, $ERRORS->{$error}], 'Text::CSV::ErrorDiag'
                (pl-bless (make-array 2 :adjustable t :fill-pointer t :initial-contents (list $error (pl-gethash-deref $ERRORS $error))) "Text::CSV::ErrorDiag")
        
      )
    )
  )
)

;; sub _set_diag { ... }
(pl-sub pl-_set_diag (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($ctx (make-pl-box nil)) ($error (make-pl-box nil)))
        ;; my ($self, $ctx, $error) = @_
                (pcl:pl-setf (vector $self $ctx $error) @_)
        
        ;; $last_error = $self->_sv_diag($error)
                (pcl:pl-setf $last_error (pl-method-call $self '_sv_diag $error))
        
        ;; $self->{_ERROR_DIAG} = $last_error
                (pcl:pl-setf (pl-gethash-deref $self "_ERROR_DIAG") $last_error)
        
        ;; if ($error == 0) {         $self->{_ERROR_POS} = 0;         $self->{_ERROR_FLD} = 0;         $self->{_ERROR_INPUT} = undef;         $ctx->{has_error_input} = 0;     }
        ;; if ($error == 0)
        (pl-if         (pcl:pl-== $error 0)
          (progn
            ;; $self->{_ERROR_POS} = 0
                        (pcl:pl-setf (pl-gethash-deref $self "_ERROR_POS") 0)
            
            ;; $self->{_ERROR_FLD} = 0
                        (pcl:pl-setf (pl-gethash-deref $self "_ERROR_FLD") 0)
            
            ;; $self->{_ERROR_INPUT} = undef
                        (pcl:pl-setf (pl-gethash-deref $self "_ERROR_INPUT") (pl-undef))
            
            ;; $ctx->{has_error_input} = 0
                        (pcl:pl-setf (pl-gethash-deref $ctx "has_error_input") 0)
            
          )
          nil
        )
        
        ;; if ($error == 2012) { # EOF         $self->{_EOF} = 1;     }
        ;; if ($error == 2012)
        (pl-if         (pcl:pl-== $error 2012)
          (progn
            ;; $self->{_EOF} = 1
                        (pcl:pl-setf (pl-gethash-deref $self "_EOF") 1)
            
          )
          nil
        )
        
        ;; if ($ctx->{auto_diag}) {         $self->error_diag;     }
        ;; if ($ctx->{"auto_diag"})
        (pl-if         (pl-gethash-deref $ctx "auto_diag")
          (progn
            ;; $self->error_diag
                        (pl-method-call $self 'error_diag)
            
          )
          nil
        )
        
        ;; return $last_error
                (pl-return $last_error)
        
      )
    )
  )
)

;; sub SetDiag { ... }
(pl-sub pl-SetDiag (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($self (make-pl-box nil)) ($error (make-pl-box nil)) ($errstr (make-pl-box nil)) ($res (make-pl-box nil)) ($ctx (make-pl-box nil)))
        ;; my ($self, $error, $errstr) = @_
                (pcl:pl-setf (vector $self $error $errstr) @_)
        
        ;; my $res (bare declaration)
        
        ;; if (ref $self) {         my $ctx = $self->_setup_ctx;         $res = $self->_set_diag($ctx, $error);      } else {         $last_error = $error;         $res = $self->_sv_diag($error);     }
        ;; if (ref $self)
        (pl-if         (pl-ref $self)
          (progn
            ;; my $ctx = $self->_setup_ctx
                        (pcl:pl-setf $ctx (pl-method-call $self '_setup_ctx))
            
            ;; $res = $self->_set_diag($ctx, $error)
                        (pcl:pl-setf $res (pl-method-call $self '_set_diag $ctx $error))
            
          )
          ;; else
          (progn
            ;; $last_error = $error
                        (pcl:pl-setf $last_error $error)
            
            ;; $res = $self->_sv_diag($error)
                        (pcl:pl-setf $res (pl-method-call $self '_sv_diag $error))
            
          )
        )
        
        ;; if (defined $errstr) {         $res->[1] = $errstr;     }
        ;; if (defined $errstr)
        (pl-if         (pl-defined $errstr)
          (progn
            ;; $res->[1] = $errstr
                        (pcl:pl-setf (pl-aref-deref $res 1) $errstr)
            
          )
          nil
        )
        
        ;; $res
                $res
        
      )
    )
  )
)

;; ################################################################################
;;; package Text::CSV::ErrorDiag
(defpackage :|Text::CSV::ErrorDiag|
  (:use :cl :pcl))
(in-package :|Text::CSV::ErrorDiag|)
;; CLOS class for MRO
(defclass text-csv-errordiag () ())

;; use strict (pragma)

;; use overload (
    '""' => \&stringify,
    '+'  => \&numeric,
    '-'  => \&numeric,
    '*'  => \&numeric,
    '/'  => \&numeric,
    fallback => 1,
)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pl-use "overload" :imports '("""" "+" "-" "*" "/")))

;; sub numeric { ... }
(pl-sub pl-numeric (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      (let (($left (make-pl-box nil)) ($right (make-pl-box nil)))
        ;; my ($left, $right) = @_
                (pcl:pl-setf (vector $left $right) @_)
        
        ;; return ref $left ? $left->[0] : $right->[0]
                (pl-return (pl-if (pl-ref $left) (pl-aref-deref $left 0) (pl-aref-deref $right 0)))
        
      )
    )
  )
)

;; sub stringify { ... }
(pl-sub pl-stringify (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; $_[0]->[1]
            (pl-aref (pl-aref @_ 0) 1)
      
    )
  )
)

;; ################################################################################
;; 1
1

;; __END__  =head1 NAME  Text::CSV_PP - Text::CSV_XS compatible pure-Perl module   =head1 SYNOPSIS  This section is taken from Text::CSV_XS.   # Functional interface  use Text::CSV_PP qw( csv );   # Read whole file in memory  my $aoa = csv (in => "data.csv");    # as array of array  my $aoh = csv (in => "data.csv",                 headers => "auto");   # as array of hash   # Write array of arrays as csv file  csv (in => $aoa, out => "file.csv", sep_char => ";");   # Only show lines where "code" is odd  csv (in => "data.csv", filter => { code => sub { $_ % 2 }});   # Object interface  use Text::CSV_PP;   my @rows;  # Read/parse CSV  my $csv = Text::CSV_PP->new ({ binary => 1, auto_diag => 1 });  open my $fh, "<:encoding(utf8)", "test.csv" or die "test.csv: $!";  while (my $row = $csv->getline ($fh)) {      $row->[2] =~ m/pattern/ or next; # 3rd field should match      push @rows, $row;      }  close $fh;   # and write as CSV  open $fh, ">:encoding(utf8)", "new.csv" or die "new.csv: $!";  $csv->say ($fh, $_) for @rows;  close $fh or die "new.csv: $!";  =head1 DESCRIPTION  Text::CSV_PP is a pure-perl module that provides facilities for the composition and decomposition of comma-separated values. This is (almost) compatible with much faster L<Text::CSV_XS>, and mainly used as its fallback module when you use L<Text::CSV> module without having installed Text::CSV_XS. If you don't have any reason to use this module directly, use Text::CSV for speed boost and portability (or maybe Text::CSV_XS when you write an one-off script and don't need to care about portability).  The following caveats are taken from the doc of Text::CSV_XS.  =head2 Embedded newlines  B<Important Note>:  The default behavior is to accept only ASCII characters in the range from C<0x20> (space) to C<0x7E> (tilde).   This means that the fields can not contain newlines. If your data contains newlines embedded in fields, or characters above C<0x7E> (tilde), or binary data, you B<I<must>> set C<< binary => 1 >> in the call to L</new>. To cover the widest range of parsing options, you will always want to set binary.  But you still have the problem  that you have to pass a correct line to the L</parse> method, which is more complicated from the usual point of usage:   my $csv = Text::CSV_PP->new ({ binary => 1, eol => $/ });  while (<>) {		#  WRONG!      $csv->parse ($_);      my @fields = $csv->fields ();      }  this will break, as the C<while> might read broken lines:  it does not care about the quoting. If you need to support embedded newlines,  the way to go is to  B<not>  pass L<C<eol>|/eol> in the parser  (it accepts C<\n>, C<\r>, B<and> C<\r\n> by default) and then   my $csv = Text::CSV_PP->new ({ binary => 1 });  open my $fh, "<", $file or die "$file: $!";  while (my $row = $csv->getline ($fh)) {      my @fields = @$row;      }  The old(er) way of using global file handles is still supported   while (my $row = $csv->getline (*ARGV)) { ... }  =head2 Unicode  Unicode is only tested to work with perl-5.8.2 and up.  See also L</BOM>.  The simplest way to ensure the correct encoding is used for  in- and output is by either setting layers on the filehandles, or setting the L</encoding> argument for L</csv>.   open my $fh, "<:encoding(UTF-8)", "in.csv"  or die "in.csv: $!"; or  my $aoa = csv (in => "in.csv",     encoding => "UTF-8");   open my $fh, ">:encoding(UTF-8)", "out.csv" or die "out.csv: $!"; or  csv (in => $aoa, out => "out.csv", encoding => "UTF-8");  On parsing (both for  L</getline> and  L</parse>),  if the source is marked being UTF8, then all fields that are marked binary will also be marked UTF8.  On combining (L</print>  and  L</combine>):  if any of the combining fields was marked UTF8, the resulting string will be marked as UTF8.  Note however that all fields  I<before>  the first field marked UTF8 and contained 8-bit characters that were not upgraded to UTF8,  these will be  C<bytes>  in the resulting string too, possibly causing unexpected errors.  If you pass data of different encoding,  or you don't know if there is  different  encoding, force it to be upgraded before you pass them on:   $csv->print ($fh, [ map { utf8::upgrade (my $x = $_); $x } @data ]);  For complete control over encoding, please use L<Text::CSV::Encoded>:   use Text::CSV::Encoded;  my $csv = Text::CSV::Encoded->new ({      encoding_in  => "iso-8859-1", # the encoding comes into   Perl      encoding_out => "cp1252",     # the encoding comes out of Perl      });   $csv = Text::CSV::Encoded->new ({ encoding  => "utf8" });  # combine () and print () accept *literally* utf8 encoded data  # parse () and getline () return *literally* utf8 encoded data   $csv = Text::CSV::Encoded->new ({ encoding  => undef }); # default  # combine () and print () accept UTF8 marked data  # parse () and getline () return UTF8 marked data  =head2 BOM  BOM  (or Byte Order Mark)  handling is available only inside the L</header> method.   This method supports the following encodings: C<utf-8>, C<utf-1>, C<utf-32be>, C<utf-32le>, C<utf-16be>, C<utf-16le>, C<utf-ebcdic>, C<scsu>, C<bocu-1>, and C<gb-18030>. See L<Wikipedia|https://en.wikipedia.org/wiki/Byte_order_mark>.  If a file has a BOM, the easiest way to deal with that is   my $aoh = csv (in => $file, detect_bom => 1);  All records will be encoded based on the detected BOM.  This implies a call to the  L</header>  method,  which defaults to also set the L</column_names>. So this is B<not> the same as   my $aoh = csv (in => $file, headers => "auto");  which only reads the first record to set  L</column_names>  but ignores any meaning of possible present BOM.  =head1 METHODS  This section is also taken from Text::CSV_XS.  =head2 version  (Class method) Returns the current module version.  =head2 new  (Class method) Returns a new instance of class Text::CSV_PP. The attributes are described by the (optional) hash ref C<\%attr>.   my $csv = Text::CSV_PP->new ({ attributes ... });  The following attributes are available:  =head3 eol   my $csv = Text::CSV_PP->new ({ eol => $/ });            $csv->eol (undef);  my $eol = $csv->eol;  The end-of-line string to add to rows for L</print> or the record separator for L</getline>.  When not passed in a B<parser> instance,  the default behavior is to accept C<\n>, C<\r>, and C<\r\n>, so it is probably safer to not specify C<eol> at all. Passing C<undef> or the empty string behave the same.  When not passed in a B<generating> instance,  records are not terminated at all, so it is probably wise to pass something you expect. A safe choice for C<eol> on output is either C<$/> or C<\r\n>.  Common values for C<eol> are C<"\012"> (C<\n> or Line Feed),  C<"\015\012"> (C<\r\n> or Carriage Return, Line Feed),  and C<"\015">  (C<\r> or Carriage Return). The L<C<eol>|/eol> attribute cannot exceed 7 (ASCII) characters.  If both C<$/> and L<C<eol>|/eol> equal C<"\015">, parsing lines that end on only a Carriage Return without Line Feed, will be L</parse>d correct.  =head3 sep_char   my $csv = Text::CSV_PP->new ({ sep_char => ";" });          $csv->sep_char (";");  my $c = $csv->sep_char;  The char used to separate fields, by default a comma. (C<,>).  Limited to a single-byte character, usually in the range from C<0x20> (space) to C<0x7E> (tilde). When longer sequences are required, use L<C<sep>|/sep>.  The separation character can not be equal to the quote character  or to the escape character.  =head3 sep   my $csv = Text::CSV_PP->new ({ sep => "\N{FULLWIDTH COMMA}" });            $csv->sep (";");  my $sep = $csv->sep;  The chars used to separate fields, by default undefined. Limited to 8 bytes.  When set, overrules L<C<sep_char>|/sep_char>.  If its length is one byte it acts as an alias to L<C<sep_char>|/sep_char>.  =head3 quote_char   my $csv = Text::CSV_PP->new ({ quote_char => "'" });          $csv->quote_char (undef);  my $c = $csv->quote_char;  The character to quote fields containing blanks or binary data,  by default the double quote character (C<">).  A value of undef suppresses quote chars (for simple cases only). Limited to a single-byte character, usually in the range from  C<0x20> (space) to  C<0x7E> (tilde).  When longer sequences are required, use L<C<quote>|/quote>.  C<quote_char> can not be equal to L<C<sep_char>|/sep_char>.  =head3 quote   my $csv = Text::CSV_PP->new ({ quote => "\N{FULLWIDTH QUOTATION MARK}" });              $csv->quote ("'");  my $quote = $csv->quote;  The chars used to quote fields, by default undefined. Limited to 8 bytes.  When set, overrules L<C<quote_char>|/quote_char>. If its length is one byte it acts as an alias to L<C<quote_char>|/quote_char>.  This method does not support C<undef>.  Use L<C<quote_char>|/quote_char> to disable quotation.  =head3 escape_char   my $csv = Text::CSV_PP->new ({ escape_char => "\\" });          $csv->escape_char (":");  my $c = $csv->escape_char;  The character to  escape  certain characters inside quoted fields.  This is limited to a  single-byte  character,  usually  in the  range from  C<0x20> (space) to C<0x7E> (tilde).  The C<escape_char> defaults to being the double-quote mark (C<">). In other words the same as the default L<C<quote_char>|/quote_char>. This means that doubling the quote mark in a field escapes it:   "foo","bar","Escape ""quote mark"" with two ""quote marks""","baz"  If  you  change  the   L<C<quote_char>|/quote_char>  without  changing  the C<escape_char>,  the  C<escape_char> will still be the double-quote (C<">). If instead you want to escape the  L<C<quote_char>|/quote_char> by doubling it you will need to also change the  C<escape_char>  to be the same as what you have changed the L<C<quote_char>|/quote_char> to.  Setting C<escape_char> to C<undef> or C<""> will completely disable escapes and is greatly discouraged. This will also disable C<escape_null>.  The escape character can not be equal to the separation character.  =head3 binary   my $csv = Text::CSV_PP->new ({ binary => 1 });          $csv->binary (0);  my $f = $csv->binary;  If this attribute is C<1>,  you may use binary characters in quoted fields, including line feeds, carriage returns and C<NULL> bytes. (The latter could be escaped as C<"0>.) By default this feature is off.  If a string is marked UTF8,  C<binary> will be turned on automatically when binary characters other than C<CR> and C<NL> are encountered.   Note that a simple string like C<"\x{00a0}"> might still be binary, but not marked UTF8, so setting C<< { binary => 1 } >> is still a wise option.  =head3 strict   my $csv = Text::CSV_PP->new ({ strict => 1 });          $csv->strict (0);  my $f = $csv->strict;  If this attribute is set to C<1>, any row that parses to a different number of fields than the previous row will cause the parser to throw error 2014.  =head3 skip_empty_rows   my $csv = Text::CSV_PP->new ({ skip_empty_rows => 1 });          $csv->skip_empty_rows ("eof");  my $f = $csv->skip_empty_rows;  This attribute defines the behavior for empty rows:  an L</eol> immediately following the start of line. Default behavior is to return one single empty field.  This attribute is only used in parsing.  This attribute is ineffective when using L</parse> and L</fields>.  Possible values for this attribute are  =over 2  =item 0 | undef   my $csv = Text::CSV_PP->new ({ skip_empty_rows => 0 });  $csv->skip_empty_rows (undef);  No special action is taken. The result will be one single empty field.  =item 1 | "skip"   my $csv = Text::CSV_PP->new ({ skip_empty_rows => 1 });  $csv->skip_empty_rows ("skip");  The row will be skipped.  =item 2 | "eof" | "stop"   my $csv = Text::CSV_PP->new ({ skip_empty_rows => 2 });  $csv->skip_empty_rows ("eof");  The parsing will stop as if an L</eof> was detected.  =item 3 | "die"   my $csv = Text::CSV_PP->new ({ skip_empty_rows => 3 });  $csv->skip_empty_rows ("die");  The parsing will stop.  The internal error code will be set to 2015 and the parser will C<die>.  =item 4 | "croak"   my $csv = Text::CSV_PP->new ({ skip_empty_rows => 4 });  $csv->skip_empty_rows ("croak");  The parsing will stop.  The internal error code will be set to 2015 and the parser will C<croak>.  =item 5 | "error"   my $csv = Text::CSV_PP->new ({ skip_empty_rows => 5 });  $csv->skip_empty_rows ("error");  The parsing will fail.  The internal error code will be set to 2015.  =item callback   my $csv = Text::CSV_PP->new ({ skip_empty_rows => sub { [] } });  $csv->skip_empty_rows (sub { [ 42, $., undef, "empty" ] });  The callback is invoked and its result used instead.  If you want the parse to stop after the callback, make sure to return a false value.  The returned value from the callback should be an array-ref. Any other type will cause the parse to stop, so these are equivalent in behavior:   csv (in => $fh, skip_empty_rows => "stop");  csv (in => $fh. skip_empty_rows => sub { 0; });  =back  Without arguments, the current value is returned: C<0>, C<1>, C<eof>, C<die>, C<croak> or the callback.  =head3 formula_handling  Alias for L</formula>  =head3 formula   my $csv = Text::CSV_PP->new ({ formula => "none" });          $csv->formula ("none");  my $f = $csv->formula;  This defines the behavior of fields containing I<formulas>. As formulas are considered dangerous in spreadsheets, this attribute can define an optional action to be taken if a field starts with an equal sign (C<=>).  For purpose of code-readability, this can also be written as   my $csv = Text::CSV_PP->new ({ formula_handling => "none" });          $csv->formula_handling ("none");  my $f = $csv->formula_handling;  Possible values for this attribute are  =over 2  =item none  Take no specific action. This is the default.   $csv->formula ("none");  =item die  Cause the process to C<die> whenever a leading C<=> is encountered.   $csv->formula ("die");  =item croak  Cause the process to C<croak> whenever a leading C<=> is encountered.  (See L<Carp>)   $csv->formula ("croak");  =item diag  Report position and content of the field whenever a leading  C<=> is found. The value of the field is unchanged.   $csv->formula ("diag");  =item empty  Replace the content of fields that start with a C<=> with the empty string.   $csv->formula ("empty");  $csv->formula ("");  =item undef  Replace the content of fields that start with a C<=> with C<undef>.   $csv->formula ("undef");  $csv->formula (undef);  =item a callback  Modify the content of fields that start with a  C<=>  with the return-value of the callback.  The original content of the field is available inside the callback as C<$_>;   # Replace all formula's with 42  $csv->formula (sub { 42; });   # same as $csv->formula ("empty") but slower  $csv->formula (sub { "" });   # Allow =4+12  $csv->formula (sub { s/^=(\d+\+\d+)$/$1/eer });   # Allow more complex calculations  $csv->formula (sub { eval { s{^=([-+*/0-9()]+)$}{$1}ee }; $_ });  =back  All other values will give a warning and then fallback to C<diag>.  =head3 decode_utf8   my $csv = Text::CSV_PP->new ({ decode_utf8 => 1 });          $csv->decode_utf8 (0);  my $f = $csv->decode_utf8;  This attributes defaults to TRUE.  While I<parsing>,  fields that are valid UTF-8, are automatically set to be UTF-8, so that    $csv->parse ("\xC4\xA8\n");  results in    PV("\304\250"\0) [UTF8 "\x{128}"]  Sometimes it might not be a desired action.  To prevent those upgrades, set this attribute to false, and the result will be    PV("\304\250"\0)  =head3 auto_diag   my $csv = Text::CSV_PP->new ({ auto_diag => 1 });          $csv->auto_diag (2);  my $l = $csv->auto_diag;  Set this attribute to a number between C<1> and C<9> causes  L</error_diag> to be automatically called in void context upon errors.  In case of error C<2012 - EOF>, this call will be void.  If C<auto_diag> is set to a numeric value greater than C<1>, it will C<die> on errors instead of C<warn>.  If set to anything unrecognized,  it will be silently ignored.  Future extensions to this feature will include more reliable auto-detection of  C<autodie>  being active in the scope of which the error occurred which will increment the value of C<auto_diag> with  C<1> the moment the error is detected.  =head3 diag_verbose   my $csv = Text::CSV_PP->new ({ diag_verbose => 1 });          $csv->diag_verbose (2);  my $l = $csv->diag_verbose;  Set the verbosity of the output triggered by C<auto_diag>.   Currently only adds the current  input-record-number  (if known)  to the diagnostic output with an indication of the position of the error.  =head3 blank_is_undef   my $csv = Text::CSV_PP->new ({ blank_is_undef => 1 });          $csv->blank_is_undef (0);  my $f = $csv->blank_is_undef;  Under normal circumstances, C<CSV> data makes no distinction between quoted- and unquoted empty fields.  These both end up in an empty string field once read, thus   1,"",," ",2  is read as   ("1", "", "", " ", "2")  When I<writing>  C<CSV> files with either  L<C<always_quote>|/always_quote> or  L<C<quote_empty>|/quote_empty> set, the unquoted  I<empty> field is the result of an undefined value.   To enable this distinction when  I<reading> C<CSV>  data,  the  C<blank_is_undef>  attribute will cause  unquoted empty fields to be set to C<undef>, causing the above to be parsed as   ("1", "", undef, " ", "2")  Note that this is specifically important when loading  C<CSV> fields into a database that allows C<NULL> values,  as the perl equivalent for C<NULL> is C<undef> in L<DBI> land.  =head3 empty_is_undef   my $csv = Text::CSV_PP->new ({ empty_is_undef => 1 });          $csv->empty_is_undef (0);  my $f = $csv->empty_is_undef;  Going one  step  further  than  L<C<blank_is_undef>|/blank_is_undef>,  this attribute converts all empty fields to C<undef>, so   1,"",," ",2  is read as   (1, undef, undef, " ", 2)  Note that this affects only fields that are  originally  empty,  not fields that are empty after stripping allowed whitespace. YMMV.  =head3 allow_whitespace   my $csv = Text::CSV_PP->new ({ allow_whitespace => 1 });          $csv->allow_whitespace (0);  my $f = $csv->allow_whitespace;  When this option is set to true,  the whitespace  (C<TAB>'s and C<SPACE>'s) surrounding  the  separation character  is removed when parsing.  If either C<TAB> or C<SPACE> is one of the three characters L<C<sep_char>|/sep_char>, L<C<quote_char>|/quote_char>, or L<C<escape_char>|/escape_char> it will not be considered whitespace.  Now lines like:   1 , "foo" , bar , 3 , zapp  are parsed as valid C<CSV>, even though it violates the C<CSV> specs.  Note that  B<all>  whitespace is stripped from both  start and  end of each field.  That would make it  I<more> than a I<feature> to enable parsing bad C<CSV> lines, as   1,   2.0,  3,   ape  , monkey  will now be parsed as   ("1", "2.0", "3", "ape", "monkey")  even if the original line was perfectly acceptable C<CSV>.  =head3 allow_loose_quotes   my $csv = Text::CSV_PP->new ({ allow_loose_quotes => 1 });          $csv->allow_loose_quotes (0);  my $f = $csv->allow_loose_quotes;  By default, parsing unquoted fields containing L<C<quote_char>|/quote_char> characters like   1,foo "bar" baz,42  would result in parse error 2034.  Though it is still bad practice to allow this format,  we  cannot  help  the  fact  that  some  vendors  make  their applications spit out lines styled this way.  If there is B<really> bad C<CSV> data, like   1,"foo "bar" baz",42  or   1,""foo bar baz"",42  there is a way to get this data-line parsed and leave the quotes inside the quoted field as-is.  This can be achieved by setting  C<allow_loose_quotes> B<AND> making sure that the L<C<escape_char>|/escape_char> is  I<not> equal to L<C<quote_char>|/quote_char>.  =head3 allow_loose_escapes   my $csv = Text::CSV_PP->new ({ allow_loose_escapes => 1 });          $csv->allow_loose_escapes (0);  my $f = $csv->allow_loose_escapes;  Parsing fields  that  have  L<C<escape_char>|/escape_char>  characters that escape characters that do not need to be escaped, like:   my $csv = Text::CSV_PP->new ({ escape_char => "\\" });  $csv->parse (qq{1,"my bar\'s",baz,42});  would result in parse error 2025.   Though it is bad practice to allow this format,  this attribute enables you to treat all escape character sequences equal.  =head3 allow_unquoted_escape   my $csv = Text::CSV_PP->new ({ allow_unquoted_escape => 1 });          $csv->allow_unquoted_escape (0);  my $f = $csv->allow_unquoted_escape;  A backward compatibility issue where L<C<escape_char>|/escape_char> differs from L<C<quote_char>|/quote_char>  prevents  L<C<escape_char>|/escape_char> to be in the first position of a field.  If L<C<quote_char>|/quote_char> is equal to the default C<"> and L<C<escape_char>|/escape_char> is set to C<\>, this would be illegal:   1,\0,2  Setting this attribute to C<1>  might help to overcome issues with backward compatibility and allow this style.  =head3 always_quote   my $csv = Text::CSV_PP->new ({ always_quote => 1 });          $csv->always_quote (0);  my $f = $csv->always_quote;  By default the generated fields are quoted only if they I<need> to be.  For example, if they contain the separator character. If you set this attribute to C<1> then I<all> defined fields will be quoted. (C<undef> fields are not quoted, see L</blank_is_undef>). This makes it quite often easier to handle exported data in external applications.  =head3 quote_space   my $csv = Text::CSV_PP->new ({ quote_space => 1 });          $csv->quote_space (0);  my $f = $csv->quote_space;  By default,  a space in a field would trigger quotation.  As no rule exists this to be forced in C<CSV>,  nor any for the opposite, the default is true for safety.   You can exclude the space  from this trigger  by setting this attribute to 0.  =head3 quote_empty   my $csv = Text::CSV_PP->new ({ quote_empty => 1 });          $csv->quote_empty (0);  my $f = $csv->quote_empty;  By default the generated fields are quoted only if they I<need> to be.   An empty (defined) field does not need quotation. If you set this attribute to C<1> then I<empty> defined fields will be quoted.  (C<undef> fields are not quoted, see L</blank_is_undef>). See also L<C<always_quote>|/always_quote>.  =head3 quote_binary   my $csv = Text::CSV_PP->new ({ quote_binary => 1 });          $csv->quote_binary (0);  my $f = $csv->quote_binary;  By default,  all "unsafe" bytes inside a string cause the combined field to be quoted.  By setting this attribute to C<0>, you can disable that trigger for bytes C<< >= 0x7F >>.  =head3 escape_null   my $csv = Text::CSV_PP->new ({ escape_null => 1 });          $csv->escape_null (0);  my $f = $csv->escape_null;  By default, a C<NULL> byte in a field would be escaped. This option enables you to treat the  C<NULL>  byte as a simple binary character in binary mode (the C<< { binary => 1 } >> is set).  The default is true.  You can prevent C<NULL> escapes by setting this attribute to C<0>.  When the C<escape_char> attribute is set to undefined,  this attribute will be set to false.  The default setting will encode "=\x00=" as   "="0="  With C<escape_null> set, this will result in   "=\x00="  The default when using the C<csv> function is C<false>.  For backward compatibility reasons,  the deprecated old name  C<quote_null> is still recognized.  =head3 keep_meta_info   my $csv = Text::CSV_PP->new ({ keep_meta_info => 1 });          $csv->keep_meta_info (0);  my $f = $csv->keep_meta_info;  By default, the parsing of input records is as simple and fast as possible. However,  some parsing information - like quotation of the original field - is lost in that process.  Setting this flag to true enables retrieving that information after parsing with  the methods  L</meta_info>,  L</is_quoted>, and L</is_binary> described below.  Default is false for performance.  If you set this attribute to a value greater than 9,   then you can control output quotation style like it was used in the input of the the last parsed record (unless quotation was added because of other reasons).   my $csv = Text::CSV_PP->new ({     binary         => 1,     keep_meta_info => 1,     quote_space    => 0,     });   my $row = $csv->parse (q{1,,"", ," ",f,"g","h""h",help,"help"});   $csv->print (*STDOUT, \@row);  # 1,,, , ,f,g,"h""h",help,help  $csv->keep_meta_info (11);  $csv->print (*STDOUT, \@row);  # 1,,"", ," ",f,"g","h""h",help,"help"  =head3 undef_str   my $csv = Text::CSV_PP->new ({ undef_str => "\\N" });          $csv->undef_str (undef);  my $s = $csv->undef_str;  This attribute optionally defines the output of undefined fields. The value passed is not changed at all, so if it needs quotation, the quotation needs to be included in the value of the attribute.  Use with caution, as passing a value like  C<",",,,,""">  will for sure mess up your output. The default for this attribute is C<undef>, meaning no special treatment.  This attribute is useful when exporting  CSV data  to be imported in custom loaders, like for MySQL, that recognize special sequences for C<NULL> data.  This attribute has no meaning when parsing CSV data.  =head3 comment_str   my $csv = Text::CSV_PP->new ({ comment_str => "#" });          $csv->comment_str (undef);  my $s = $csv->comment_str;  This attribute optionally defines a string to be recognized as comment.  If this attribute is defined,   all lines starting with this sequence will not be parsed as CSV but skipped as comment.  This attribute has no meaning when generating CSV.  Comment strings that start with any of the special characters/sequences are not supported (so it cannot start with any of L</sep_char>, L</quote_char>, L</escape_char>, L</sep>, L</quote>, or L</eol>).  For convenience, C<comment> is an alias for C<comment_str>.  =head3 verbatim   my $csv = Text::CSV_PP->new ({ verbatim => 1 });          $csv->verbatim (0);  my $f = $csv->verbatim;  This is a quite controversial attribute to set,  but makes some hard things possible.  The rationale behind this attribute is to tell the parser that the normally special characters newline (C<NL>) and Carriage Return (C<CR>)  will not be special when this flag is set,  and be dealt with  as being ordinary binary characters. This will ease working with data with embedded newlines.  When  C<verbatim>  is used with  L</getline>,  L</getline>  auto-C<chomp>'s every line.  Imagine a file format like   M^^Hans^Janssen^Klas 2\n2A^Ja^11-06-2007#\r\n  where, the line ending is a very specific C<"#\r\n">, and the sep_char is a C<^> (caret).   None of the fields is quoted,   but embedded binary data is likely to be present. With the specific line ending, this should not be too hard to detect.  By default,  Text::CSV_PP'  parse function is instructed to only know about C<"\n"> and C<"\r">  to be legal line endings,  and so has to deal with the embedded newline as a real C<end-of-line>,  so it can scan the next line if binary is true, and the newline is inside a quoted field. With this option, we tell L</parse> to parse the line as if C<"\n"> is just nothing more than a binary character.  For L</parse> this means that the parser has no more idea about line ending and L</getline> C<chomp>s line endings on reading.  =head3 types  A set of column types; the attribute is immediately passed to the L</types> method.  =head3 callbacks  See the L</Callbacks> section below.  =head3 accessors  To sum it up,   $csv = Text::CSV_PP->new ();  is equivalent to   $csv = Text::CSV_PP->new ({      eol                   => undef, # \r, \n, or \r\n      sep_char              => ',',      sep                   => undef,      quote_char            => '"',      quote                 => undef,      escape_char           => '"',      binary                => 0,      decode_utf8           => 1,      auto_diag             => 0,      diag_verbose          => 0,      blank_is_undef        => 0,      empty_is_undef        => 0,      allow_whitespace      => 0,      allow_loose_quotes    => 0,      allow_loose_escapes   => 0,      allow_unquoted_escape => 0,      always_quote          => 0,      quote_empty           => 0,      quote_space           => 1,      escape_null           => 1,      quote_binary          => 1,      keep_meta_info        => 0,      strict                => 0,      skip_empty_rows       => 0,      formula               => 0,      verbatim              => 0,      undef_str             => undef,      comment_str           => undef,      types                 => undef,      callbacks             => undef,      });  For all of the above mentioned flags, an accessor method is available where you can inquire the current value, or change the value   my $quote = $csv->quote_char;  $csv->binary (1);  It is not wise to change these settings halfway through writing C<CSV> data to a stream. If however you want to create a new stream using the available C<CSV> object, there is no harm in changing them.  If the L</new> constructor call fails,  it returns C<undef>,  and makes the fail reason available through the L</error_diag> method.   $csv = Text::CSV_PP->new ({ ecs_char => 1 }) or      die "".Text::CSV_PP->error_diag ();  L</error_diag> will return a string like   "INI - Unknown attribute 'ecs_char'"  =head2 known_attributes   @attr = Text::CSV_PP->known_attributes;  @attr = Text::CSV_PP::known_attributes;  @attr = $csv->known_attributes;  This method will return an ordered list of all the supported  attributes as described above.   This can be useful for knowing what attributes are valid in classes that use or extend Text::CSV_PP.  =head2 print   $status = $csv->print ($fh, $colref);  Similar to  L</combine> + L</string> + L</print>,  but much more efficient. It expects an array ref as input  (not an array!)  and the resulting string is not really  created,  but  immediately  written  to the  C<$fh>  object, typically an IO handle or any other object that offers a L</print> method.  For performance reasons  C<print>  does not create a result string,  so all L</string>, L</status>, L</fields>, and L</error_input> methods will return undefined information after executing this method.  If C<$colref> is C<undef>  (explicit,  not through a variable argument) and L</bind_columns>  was used to specify fields to be printed,  it is possible to make performance improvements, as otherwise data would have to be copied as arguments to the method call:   $csv->bind_columns (\($foo, $bar));  $status = $csv->print ($fh, undef);  A short benchmark   my @data = ("aa" .. "zz");  $csv->bind_columns (\(@data));   $csv->print ($fh, [ @data ]);   # 11800 recs/sec  $csv->print ($fh,  \@data  );   # 57600 recs/sec  $csv->print ($fh,   undef  );   # 48500 recs/sec  =head2 say   $status = $csv->say ($fh, $colref);  Like L<C<print>|/print>, but L<C<eol>|/eol> defaults to C<$\>.  =head2 print_hr   $csv->print_hr ($fh, $ref);  Provides an easy way  to print a  C<$ref>  (as fetched with L</getline_hr>) provided the column names are set with L</column_names>.  It is just a wrapper method with basic parameter checks over   $csv->print ($fh, [ map { $ref->{$_} } $csv->column_names ]);  =head2 combine   $status = $csv->combine (@fields);  This method constructs a C<CSV> record from  C<@fields>,  returning success or failure.   Failure can result from lack of arguments or an argument that contains an invalid character.   Upon success,  L</string> can be called to retrieve the resultant C<CSV> string.  Upon failure,  the value returned by L</string> is undefined and L</error_input> could be called to retrieve the invalid argument.  =head2 string   $line = $csv->string ();  This method returns the input to  L</parse>  or the resultant C<CSV> string of L</combine>, whichever was called more recently.  =head2 getline   $colref = $csv->getline ($fh);  This is the counterpart to  L</print>,  as L</parse>  is the counterpart to L</combine>:  it parses a row from the C<$fh>  handle using the L</getline> method associated with C<$fh>  and parses this row into an array ref.  This array ref is returned by the function or C<undef> for failure.  When C<$fh> does not support C<getline>, you are likely to hit errors.  When fields are bound with L</bind_columns> the return value is a reference to an empty list.  The L</string>, L</fields>, and L</status> methods are meaningless again.  =head2 getline_all   $arrayref = $csv->getline_all ($fh);  $arrayref = $csv->getline_all ($fh, $offset);  $arrayref = $csv->getline_all ($fh, $offset, $length);  This will return a reference to a list of L<getline ($fh)|/getline> results. In this call, C<keep_meta_info> is disabled.  If C<$offset> is negative, as with C<splice>, only the last  C<abs ($offset)> records of C<$fh> are taken into consideration. Parameters C<$offset> and C<$length> are expected to be integers. Non-integer values are interpreted as integer without check.  Given a CSV file with 10 lines:   lines call  ----- ---------------------------------------------------------  0..9  $csv->getline_all ($fh)         # all  0..9  $csv->getline_all ($fh,  0)     # all  8..9  $csv->getline_all ($fh,  8)     # start at 8  -     $csv->getline_all ($fh,  0,  0) # start at 0 first 0 rows  0..4  $csv->getline_all ($fh,  0,  5) # start at 0 first 5 rows  4..5  $csv->getline_all ($fh,  4,  2) # start at 4 first 2 rows  8..9  $csv->getline_all ($fh, -2)     # last 2 rows  6..7  $csv->getline_all ($fh, -4,  2) # first 2 of last  4 rows  =head2 getline_hr  The L</getline_hr> and L</column_names> methods work together  to allow you to have rows returned as hashrefs.  You must call L</column_names> first to declare your column names.   $csv->column_names (qw( code name price description ));  $hr = $csv->getline_hr ($fh);  print "Price for $hr->{name} is $hr->{price} EUR\n";  L</getline_hr> will croak if called before L</column_names>.  Note that  L</getline_hr>  creates a hashref for every row and will be much slower than the combined use of L</bind_columns>  and L</getline> but still offering the same easy to use hashref inside the loop:   my @cols = @{$csv->getline ($fh)};  $csv->column_names (@cols);  while (my $row = $csv->getline_hr ($fh)) {      print $row->{price};      }  Could easily be rewritten to the much faster:   my @cols = @{$csv->getline ($fh)};  my $row = {};  $csv->bind_columns (\@{$row}{@cols});  while ($csv->getline ($fh)) {      print $row->{price};      }  Your mileage may vary for the size of the data and the number of rows. With perl-5.14.2 the comparison for a 100_000 line file with 14 columns:              Rate hashrefs getlines  hashrefs 1.00/s       --     -76%  getlines 4.15/s     313%       --  =head2 getline_hr_all   $arrayref = $csv->getline_hr_all ($fh);  $arrayref = $csv->getline_hr_all ($fh, $offset);  $arrayref = $csv->getline_hr_all ($fh, $offset, $length);  This will return a reference to a list of   L<getline_hr ($fh)|/getline_hr> results.  In this call, L<C<keep_meta_info>|/keep_meta_info> is disabled.  =head2 parse   $status = $csv->parse ($line);  This method decomposes a  C<CSV>  string into fields,  returning success or failure.   Failure can result from a lack of argument  or the given  C<CSV> string is improperly formatted.   Upon success, L</fields> can be called to retrieve the decomposed fields. Upon failure calling L</fields> will return undefined data and  L</error_input>  can be called to retrieve  the invalid argument.  You may use the L</types>  method for setting column types.  See L</types>' description below.  The C<$line> argument is supposed to be a simple scalar. Everything else is supposed to croak and set error 1500.  =head2 fragment  This function tries to implement RFC7111  (URI Fragment Identifiers for the text/csv Media Type) - https://datatracker.ietf.org/doc/html/rfc7111   my $AoA = $csv->fragment ($fh, $spec);  In specifications,  C<*> is used to specify the I<last> item, a dash (C<->) to indicate a range.   All indices are C<1>-based:  the first row or column has index C<1>. Selections can be combined with the semi-colon (C<;>).  When using this method in combination with  L</column_names>,  the returned reference  will point to a  list of hashes  instead of a  list of lists.  A disjointed  cell-based combined selection  might return rows with different number of columns making the use of hashes unpredictable.   $csv->column_names ("Name", "Age");  my $AoH = $csv->fragment ($fh, "col=3;8");  If the L</after_parse> callback is active,  it is also called on every line parsed and skipped before the fragment.  =over 2  =item row   row=4  row=5-7  row=6-*  row=1-2;4;6-*  =item col   col=2  col=1-3  col=4-*  col=1-2;4;7-*  =item cell  In cell-based selection, the comma (C<,>) is used to pair row and column   cell=4,1  The range operator (C<->) using C<cell>s can be used to define top-left and bottom-right C<cell> location   cell=3,1-4,6  The C<*> is only allowed in the second part of a pair   cell=3,2-*,2    # row 3 till end, only column 2  cell=3,2-3,*    # column 2 till end, only row 3  cell=3,2-*,*    # strip row 1 and 2, and column 1  Cells and cell ranges may be combined with C<;>, possibly resulting in rows with different numbers of columns   cell=1,1-2,2;3,3-4,4;1,4;4,1  Disjointed selections will only return selected cells.   The cells that are not  specified  will  not  be  included  in the  returned set,  not even as C<undef>.  As an example given a C<CSV> like   11,12,13,...19  21,22,...28,29  :            :  91,...97,98,99  with C<cell=1,1-2,2;3,3-4,4;1,4;4,1> will return:   11,12,14  21,22  33,34  41,43,44  Overlapping cell-specs will return those cells only once, So C<cell=1,1-3,3;2,2-4,4;2,3;4,2> will return:   11,12,13  21,22,23,24  31,32,33,34  42,43,44  =back  L<RFC7111|https://datatracker.ietf.org/doc/html/rfc7111> does  B<not>  allow different types of specs to be combined   (either C<row> I<or> C<col> I<or> C<cell>). Passing an invalid fragment specification will croak and set error 2013.  =head2 column_names  Set the "keys" that will be used in the  L</getline_hr>  calls.  If no keys (column names) are passed, it will return the current setting as a list.  L</column_names> accepts a list of scalars  (the column names)  or a single array_ref, so you can pass the return value from L</getline> too:   $csv->column_names ($csv->getline ($fh));  L</column_names> does B<no> checking on duplicates at all, which might lead to unexpected results.   Undefined entries will be replaced with the string C<"\cAUNDEF\cA">, so   $csv->column_names (undef, "", "name", "name");  $hr = $csv->getline_hr ($fh);  will set C<< $hr->{"\cAUNDEF\cA"} >> to the 1st field,  C<< $hr->{""} >> to the 2nd field, and C<< $hr->{name} >> to the 4th field,  discarding the 3rd field.  L</column_names> croaks on invalid arguments.  =head2 header  This method does NOT work in perl-5.6.x  Parse the CSV header and set L<C<sep>|/sep>, column_names and encoding.   my @hdr = $csv->header ($fh);  $csv->header ($fh, { sep_set => [ ";", ",", "|", "\t" ] });  $csv->header ($fh, { detect_bom => 1, munge_column_names => "lc" });  The first argument should be a file handle.  This method resets some object properties,  as it is supposed to be invoked only once per file or stream.  It will leave attributes C<column_names> and C<bound_columns> alone if setting column names is disabled. Reading headers on previously process objects might fail on perl-5.8.0 and older.  Assuming that the file opened for parsing has a header, and the header does not contain problematic characters like embedded newlines,   read the first line from the open handle then auto-detect whether the header separates the column names with a character from the allowed separator list.  If any of the allowed separators matches,  and none of the I<other> allowed separators match,  set  L<C<sep>|/sep>  to that  separator  for the current CSV_PP instance and use it to parse the first line, map those to lowercase, and use that to set the instance L</column_names>:   my $csv = Text::CSV_PP->new ({ binary => 1, auto_diag => 1 });  open my $fh, "<", "file.csv";  binmode $fh; # for Windows  $csv->header ($fh);  while (my $row = $csv->getline_hr ($fh)) {      ...      }  If the header is empty,  contains more than one unique separator out of the allowed set,  contains empty fields,   or contains identical fields  (after folding), it will croak with error 1010, 1011, 1012, or 1013 respectively.  If the header contains embedded newlines or is not valid  CSV  in any other way, this method will croak and leave the parse error untouched.  A successful call to C<header>  will always set the  L<C<sep>|/sep>  of the C<$csv> object. This behavior can not be disabled.  =head3 return value  On error this method will croak.  In list context,  the headers will be returned whether they are used to set L</column_names> or not.  In scalar context, the instance itself is returned.  B<Note>: the values as found in the header will effectively be  B<lost> if  C<set_column_names> is false.  =head3 Options  =over 2  =item sep_set   $csv->header ($fh, { sep_set => [ ";", ",", "|", "\t" ] });  The list of legal separators defaults to C<[ ";", "," ]> and can be changed by this option.  As this is probably the most often used option,  it can be passed on its own as an unnamed argument:   $csv->header ($fh, [ ";", ",", "|", "\t", "::", "\x{2063}" ]);  Multi-byte  sequences are allowed,  both multi-character and  Unicode.  See L<C<sep>|/sep>.  =item detect_bom   $csv->header ($fh, { detect_bom => 1 });  The default behavior is to detect if the header line starts with a BOM.  If the header has a BOM, use that to set the encoding of C<$fh>.  This default behavior can be disabled by passing a false value to C<detect_bom>.  Supported encodings from BOM are: UTF-8, UTF-16BE, UTF-16LE, UTF-32BE,  and UTF-32LE. BOM also supports UTF-1, UTF-EBCDIC, SCSU, BOCU-1,  and GB-18030 but L<Encode> does not (yet). UTF-7 is not supported.  If a supported BOM was detected as start of the stream, it is stored in the object attribute C<ENCODING>.   my $enc = $csv->{ENCODING};  The encoding is used with C<binmode> on C<$fh>.  If the handle was opened in a (correct) encoding,  this method will  B<not> alter the encoding, as it checks the leading B<bytes> of the first line. In case the stream starts with a decoded BOM (C<U+FEFF>), C<{ENCODING}> will be C<""> (empty) instead of the default C<undef>.  =item munge_column_names  This option offers the means to modify the column names into something that is most useful to the application.   The default is to map all column names to lower case.   $csv->header ($fh, { munge_column_names => "lc" });  The following values are available:    lc     - lower case   uc     - upper case   db     - valid DB field names   none   - do not change   \%hash - supply a mapping   \&cb   - supply a callback  =over 2  =item Lower case   $csv->header ($fh, { munge_column_names => "lc" });  The header is changed to all lower-case   $_ = lc;  =item Upper case   $csv->header ($fh, { munge_column_names => "uc" });  The header is changed to all upper-case   $_ = uc;  =item Literal   $csv->header ($fh, { munge_column_names => "none" });  =item Hash   $csv->header ($fh, { munge_column_names => { foo => "sombrero" });  if a value does not exist, the original value is used unchanged  =item Database   $csv->header ($fh, { munge_column_names => "db" });  =over 2  =item -  lower-case  =item -  all sequences of non-word characters are replaced with an underscore  =item -  all leading underscores are removed  =back   $_ = lc (s/\W+/_/gr =~ s/^_+//r);  =item Callback   $csv->header ($fh, { munge_column_names => sub { fc } });  $csv->header ($fh, { munge_column_names => sub { "column_".$col++ } });  $csv->header ($fh, { munge_column_names => sub { lc (s/\W+/_/gr) } });  As this callback is called in a C<map>, you can use C<$_> directly.  =back  =item set_column_names   $csv->header ($fh, { set_column_names => 1 });  The default is to set the instances column names using  L</column_names> if the method is successful,  so subsequent calls to L</getline_hr> can return a hash. Disable setting the header can be forced by using a false value for this option.  As described in L</return value> above, content is lost in scalar context.  =back  =head3 Validation  When receiving CSV files from external sources,  this method can be used to protect against changes in the layout by restricting to known headers  (and typos in the header fields).   my %known = (      "record key" => "c_rec",      "rec id"     => "c_rec",      "id_rec"     => "c_rec",      "kode"       => "code",      "code"       => "code",      "vaule"      => "value",      "value"      => "value",      );  my $csv = Text::CSV_PP->new ({ binary => 1, auto_diag => 1 });  open my $fh, "<", $source or die "$source: $!";  $csv->header ($fh, { munge_column_names => sub {      s/\s+$//;      s/^\s+//;      $known{lc $_} or die "Unknown column '$_' in $source";      }});  while (my $row = $csv->getline_hr ($fh)) {      say join "\t", $row->{c_rec}, $row->{code}, $row->{value};      }  =head2 bind_columns  Takes a list of scalar references to be used for output with  L</print>  or to store in the fields fetched by L</getline>.  When you do not pass enough references to store the fetched fields in, L</getline> will fail with error C<3006>.  If you pass more than there are fields to return,  the content of the remaining references is left untouched.   $csv->bind_columns (\$code, \$name, \$price, \$description);  while ($csv->getline ($fh)) {      print "The price of a $name is \x{20ac} $price\n";      }  To reset or clear all column binding, call L</bind_columns> with the single argument C<undef>. This will also clear column names.   $csv->bind_columns (undef);  If no arguments are passed at all, L</bind_columns> will return the list of current bindings or C<undef> if no binds are active.  Note that in parsing with  C<bind_columns>,  the fields are set on the fly. That implies that if the third field of a row causes an error  (or this row has just two fields where the previous row had more),  the first two fields already have been assigned the values of the current row, while the rest of the fields will still hold the values of the previous row.  If you want the parser to fail in these cases, use the L<C<strict>|/strict> attribute.  =head2 eof   $eof = $csv->eof ();  If L</parse> or  L</getline>  was used with an IO stream,  this method will return true (1) if the last call hit end of file,  otherwise it will return false ('').  This is useful to see the difference between a failure and end of file.  Note that if the parsing of the last line caused an error,  C<eof> is still true.  That means that if you are I<not> using L</auto_diag>, an idiom like   while (my $row = $csv->getline ($fh)) {      # ...      }  $csv->eof or $csv->error_diag;  will I<not> report the error. You would have to change that to   while (my $row = $csv->getline ($fh)) {      # ...      }  +$csv->error_diag and $csv->error_diag;  =head2 types   $csv->types (\@tref);  This method is used to force that  (all)  columns are of a given type.  For example, if you have an integer column,  two  columns  with  doubles  and a string column, then you might do a   $csv->types ([Text::CSV_PP::IV (),                Text::CSV_PP::NV (),                Text::CSV_PP::NV (),                Text::CSV_PP::PV ()]);  Column types are used only for I<decoding> columns while parsing,  in other words by the L</parse> and L</getline> methods.  You can unset column types by doing a   $csv->types (undef);  or fetch the current type settings with   $types = $csv->types ();  =over 4  =item IV  =item CSV_TYPE_IV  Set field type to integer.  =item NV  =item CSV_TYPE_NV  Set field type to numeric/float.  =item PV  =item CSV_TYPE_PV  Set field type to string.  =back  =head2 fields   @columns = $csv->fields ();  This method returns the input to   L</combine>  or the resultant decomposed fields of a successful L</parse>, whichever was called more recently.  Note that the return value is undefined after using L</getline>, which does not fill the data structures returned by L</parse>.  =head2 meta_info   @flags = $csv->meta_info ();  This method returns the "flags" of the input to L</combine> or the flags of the resultant  decomposed fields of  L</parse>,   whichever was called more recently.  For each field,  a meta_info field will hold  flags that  inform  something about  the  field  returned  by  the  L</fields>  method or  passed to  the L</combine> method. The flags are bit-wise-C<or>'d like:  =over 2  =item C<0x0001>  =item C<CSV_FLAGS_IS_QUOTED>  The field was quoted.  =item C<0x0002>  =item C<CSV_FLAGS_IS_BINARY>  The field was binary.  =item C<0x0004>  =item C<CSV_FLAGS_ERROR_IN_FIELD>  The field was invalid.  Currently only used when C<allow_loose_quotes> is active.  =item C<0x0010>  =item C<CSV_FLAGS_IS_MISSING>  The field was missing.  =back  See the C<is_***> methods below.  =head2 is_quoted   my $quoted = $csv->is_quoted ($column_idx);  where  C<$column_idx> is the  (zero-based)  index of the column in the last result of L</parse>.  This returns a true value  if the data in the indicated column was enclosed in L<C<quote_char>|/quote_char> quotes.  This might be important for fields where content C<,20070108,> is to be treated as a numeric value,  and where C<,"20070108",> is explicitly marked as character string data.  This method is only valid when L</keep_meta_info> is set to a true value.  =head2 is_binary   my $binary = $csv->is_binary ($column_idx);  where  C<$column_idx> is the  (zero-based)  index of the column in the last result of L</parse>.  This returns a true value if the data in the indicated column contained any byte in the range C<[\x00-\x08,\x10-\x1F,\x7F-\xFF]>.  This method is only valid when L</keep_meta_info> is set to a true value.  =head2 is_missing   my $missing = $csv->is_missing ($column_idx);  where  C<$column_idx> is the  (zero-based)  index of the column in the last result of L</getline_hr>.   $csv->keep_meta_info (1);  while (my $hr = $csv->getline_hr ($fh)) {      $csv->is_missing (0) and next; # This was an empty line      }  When using  L</getline_hr>,  it is impossible to tell if the  parsed fields are C<undef> because they where not filled in the C<CSV> stream  or because they were not read at all, as B<all> the fields defined by L</column_names> are set in the hash-ref.    If you still need to know if all fields in each row are provided, you should enable L<C<keep_meta_info>|/keep_meta_info> so you can check the flags.  If  L<C<keep_meta_info>|/keep_meta_info>  is C<false>,  C<is_missing>  will always return C<undef>, regardless of C<$column_idx> being valid or not. If this attribute is C<true> it will return either C<0> (the field is present) or C<1> (the field is missing).  A special case is the empty line.  If the line is completely empty -  after dealing with the flags - this is still a valid CSV line:  it is a record of just one single empty field. However, if C<keep_meta_info> is set, invoking C<is_missing> with index C<0> will now return true.  =head2 status   $status = $csv->status ();  This method returns the status of the last invoked L</combine> or L</parse> call. Status is success (true: C<1>) or failure (false: C<undef> or C<0>).  Note that as this only keeps track of the status of above mentioned methods, you are probably looking for L<C<error_diag>|/error_diag> instead.  =head2 error_input   $bad_argument = $csv->error_input ();  This method returns the erroneous argument (if it exists) of L</combine> or L</parse>,  whichever was called more recently.  If the last invocation was successful, C<error_input> will return C<undef>.  Depending on the type of error, it I<might> also hold the data for the last error-input of L</getline>.  =head2 error_diag   Text::CSV_PP->error_diag ();  $csv->error_diag ();  $error_code               = 0  + $csv->error_diag ();  $error_str                = "" . $csv->error_diag ();  ($cde, $str, $pos, $rec, $fld) = $csv->error_diag ();  If (and only if) an error occurred,  this function returns  the diagnostics of that error.  If called in void context,  this will print the internal error code and the associated error message to STDERR.  If called in list context,  this will return  the error code  and the error message in that order.  If the last error was from parsing, the rest of the values returned are a best guess at the location  within the line  that was being parsed. Their values are 1-based.  The position currently is index of the byte at which the parsing failed in the current record. It might change to be the index of the current character in a later release. The records is the index of the record parsed by the csv instance. The field number is the index of the field the parser thinks it is currently  trying to  parse. See F<examples/csv-check> for how this can be used.  If called in  scalar context,  it will return  the diagnostics  in a single scalar, a-la C<$!>.  It will contain the error code in numeric context, and the diagnostics message in string context.  When called as a class method or a  direct function call,  the  diagnostics are that of the last L</new> call.  =head2 record_number   $recno = $csv->record_number ();  Returns the records parsed by this csv instance.  This value should be more accurate than C<$.> when embedded newlines come in play. Records written by this instance are not counted.  =head2 SetDiag   $csv->SetDiag (0);  Use to reset the diagnostics if you are dealing with errors.  =head1 FUNCTIONS  This section is also taken from Text::CSV_XS.  =head2 csv  This function is not exported by default and should be explicitly requested:   use Text::CSV_PP qw( csv );  This is a high-level function that aims at simple (user) interfaces.  This can be used to read/parse a C<CSV> file or stream (the default behavior) or to produce a file or write to a stream (define the  C<out>  attribute).  It returns an array- or hash-reference on parsing (or C<undef> on fail) or the numeric value of  L</error_diag>  on writing.  When this function fails you can get to the error using the class call to L</error_diag>   my $aoa = csv (in => "test.csv") or      die Text::CSV_PP->error_diag;  This function takes the arguments as key-value pairs. This can be passed as a list or as an anonymous hash:   my $aoa = csv (  in => "test.csv", sep_char => ";");  my $aoh = csv ({ in => $fh, headers => "auto" });  The arguments passed consist of two parts:  the arguments to L</csv> itself and the optional attributes to the  C<CSV>  object used inside the function as enumerated and explained in L</new>.  If not overridden, the default option used for CSV is   auto_diag   => 1  escape_null => 0  The option that is always set and cannot be altered is   binary      => 1  As this function will likely be used in one-liners,  it allows  C<quote> to be abbreviated as C<quo>,  and  C<escape_char> to be abbreviated as  C<esc> or C<escape>.  Alternative invocations:   my $aoa = Text::CSV_PP::csv (in => "file.csv");   my $csv = Text::CSV_PP->new ();  my $aoa = $csv->csv (in => "file.csv");  In the latter case, the object attributes are used from the existing object and the attribute arguments in the function call are ignored:   my $csv = Text::CSV_PP->new ({ sep_char => ";" });  my $aoh = $csv->csv (in => "file.csv", bom => 1);  will parse using C<;> as C<sep_char>, not C<,>.  =head3 in  Used to specify the source.  C<in> can be a file name (e.g. C<"file.csv">), which will be  opened for reading  and closed when finished,  a file handle (e.g.  C<$fh> or C<FH>),  a reference to a glob (e.g. C<\*ARGV>),  the glob itself (e.g. C<*STDIN>), or a reference to a scalar (e.g. C<\q{1,2,"csv"}>).  When used with L</out>, C<in> should be a reference to a CSV structure (AoA or AoH)  or a CODE-ref that returns an array-reference or a hash-reference. The code-ref will be invoked with no arguments.   my $aoa = csv (in => "file.csv");   open my $fh, "<", "file.csv";  my $aoa = csv (in => $fh);   my $csv = [ [qw( Foo Bar )], [ 1, 2 ], [ 2, 3 ]];  my $err = csv (in => $csv, out => "file.csv");  If called in void context without the L</out> attribute, the resulting ref will be used as input to a subsequent call to csv:   csv (in => "file.csv", filter => { 2 => sub { length > 2 }})  will be a shortcut to   csv (in => csv (in => "file.csv", filter => { 2 => sub { length > 2 }}))  where, in the absence of the C<out> attribute, this is a shortcut to   csv (in  => csv (in => "file.csv", filter => { 2 => sub { length > 2 }}),       out => *STDOUT)  =head3 out   csv (in => $aoa, out => "file.csv");  csv (in => $aoa, out => $fh);  csv (in => $aoa, out =>   STDOUT);  csv (in => $aoa, out =>  *STDOUT);  csv (in => $aoa, out => \*STDOUT);  csv (in => $aoa, out => \my $data);  csv (in => $aoa, out =>  undef);  csv (in => $aoa, out => \"skip");   csv (in => $fh,  out => \@aoa);  csv (in => $fh,  out => \@aoh, bom => 1);  csv (in => $fh,  out => \%hsh, key => "key");  In output mode, the default CSV options when producing CSV are   eol       => "\r\n"  The L</fragment> attribute is ignored in output mode.  C<out> can be a file name  (e.g.  C<"file.csv">),  which will be opened for writing and closed when finished,  a file handle (e.g. C<$fh> or C<FH>),  a reference to a glob (e.g. C<\*STDOUT>),  the glob itself (e.g. C<*STDOUT>), or a reference to a scalar (e.g. C<\my $data>).   csv (in => sub { $sth->fetch },            out => "dump.csv");  csv (in => sub { $sth->fetchrow_hashref }, out => "dump.csv",       headers => $sth->{NAME_lc});  When a code-ref is used for C<in>, the output is generated  per invocation, so no buffering is involved. This implies that there is no size restriction on the number of records. The C<csv> function ends when the coderef returns a false value.  If C<out> is set to a reference of the literal string C<"skip">, the output will be suppressed completely,  which might be useful in combination with a filter for side effects only.   my %cache;  csv (in    => "dump.csv",       out   => \"skip",       on_in => sub { $cache{$_[1][1]}++ });  Currently,  setting C<out> to any false value  (C<undef>, C<"">, 0) will be equivalent to C<\"skip">.  If the C<in> argument point to something to parse, and the C<out> is set to a reference to an C<ARRAY> or a C<HASH>, the output is appended to the data in the existing reference. The result of the parse should match what exists in the reference passed. This might come handy when you have to parse a set of files with similar content (like data stored per period) and you want to collect that into a single data structure:   my %hash;  csv (in => $_, out => \%hash, key => "id") for sort glob "foo-[0-9]*.csv";   my @list; # List of arrays  csv (in => $_, out => \@list)              for sort glob "foo-[0-9]*.csv";   my @list; # List of hashes  csv (in => $_, out => \@list, bom => 1)    for sort glob "foo-[0-9]*.csv";  =head3 encoding  If passed,  it should be an encoding accepted by the  C<:encoding()> option to C<open>. There is no default value. This attribute does not work in perl 5.6.x.  C<encoding> can be abbreviated to C<enc> for ease of use in command line invocations.  If C<encoding> is set to the literal value C<"auto">, the method L</header> will be invoked on the opened stream to check if there is a BOM and set the encoding accordingly.   This is equal to passing a true value in the option L<C<detect_bom>|/detect_bom>.  Encodings can be stacked, as supported by C<binmode>:   # Using PerlIO::via::gzip  csv (in       => \@csv,       out      => "test.csv:via.gz",       encoding => ":via(gzip):encoding(utf-8)",       );  $aoa = csv (in => "test.csv:via.gz",  encoding => ":via(gzip)");   # Using PerlIO::gzip  csv (in       => \@csv,       out      => "test.csv:via.gz",       encoding => ":gzip:encoding(utf-8)",       );  $aoa = csv (in => "test.csv:gzip.gz", encoding => ":gzip");  =head3 detect_bom  If  C<detect_bom>  is given, the method  L</header>  will be invoked on the opened stream to check if there is a BOM and set the encoding accordingly.  C<detect_bom> can be abbreviated to C<bom>.  This is the same as setting L<C<encoding>|/encoding> to C<"auto">.  Note that as the method  L</header> is invoked,  its default is to also set the headers.  =head3 headers  If this attribute is not given, the default behavior is to produce an array of arrays.  If C<headers> is supplied,  it should be an anonymous list of column names, an anonymous hashref, a coderef, or a literal flag:  C<auto>, C<lc>, C<uc>, or C<skip>.  =over 2  =item skip  When C<skip> is used, the header will not be included in the output.   my $aoa = csv (in => $fh, headers => "skip");  C<skip> is invalid/ignored in combinations with L<C<detect_bom>|/detect_bom>.  =item auto  If C<auto> is used, the first line of the C<CSV> source will be read as the list of field headers and used to produce an array of hashes.   my $aoh = csv (in => $fh, headers => "auto");  =item lc  If C<lc> is used,  the first line of the  C<CSV> source will be read as the list of field headers mapped to  lower case and used to produce an array of hashes. This is a variation of C<auto>.   my $aoh = csv (in => $fh, headers => "lc");  =item uc  If C<uc> is used,  the first line of the  C<CSV> source will be read as the list of field headers mapped to  upper case and used to produce an array of hashes. This is a variation of C<auto>.   my $aoh = csv (in => $fh, headers => "uc");  =item CODE  If a coderef is used,  the first line of the  C<CSV> source will be read as the list of mangled field headers in which each field is passed as the only argument to the coderef. This list is used to produce an array of hashes.   my $aoh = csv (in      => $fh,                 headers => sub { lc ($_[0]) =~ s/kode/code/gr });  this example is a variation of using C<lc> where all occurrences of C<kode> are replaced with C<code>.  =item ARRAY  If  C<headers>  is an anonymous list,  the entries in the list will be used as field names. The first line is considered data instead of headers.   my $aoh = csv (in => $fh, headers => [qw( Foo Bar )]);  csv (in => $aoa, out => $fh, headers => [qw( code description price )]);  =item HASH  If C<headers> is a hash reference, this implies C<auto>, but header fields that exist as key in the hashref will be replaced by the value for that key. Given a CSV file like   post-kode,city,name,id number,fubble  1234AA,Duckstad,Donald,13,"X313DF"  using   csv (headers => { "post-kode" => "pc", "id number" => "ID" }, ...  will return an entry like   { pc     => "1234AA",    city   => "Duckstad",    name   => "Donald",    ID     => "13",    fubble => "X313DF",    }  =back  See also L<C<munge_column_names>|/munge_column_names> and L<C<set_column_names>|/set_column_names>.  =head3 munge_column_names  If C<munge_column_names> is set,  the method  L</header>  is invoked on the opened stream with all matching arguments to detect and set the headers.  C<munge_column_names> can be abbreviated to C<munge>.  =head3 key  If passed,  will default  L<C<headers>|/headers>  to C<"auto"> and return a hashref instead of an array of hashes. Allowed values are simple scalars or array-references where the first element is the joiner and the rest are the fields to join to combine the key.   my $ref = csv (in => "test.csv", key => "code");  my $ref = csv (in => "test.csv", key => [ ":" => "code", "color" ]);  with test.csv like   code,product,price,color  1,pc,850,gray  2,keyboard,12,white  3,mouse,5,black  the first example will return    { 1   => {         code    => 1,         color   => 'gray',         price   => 850,         product => 'pc'         },     2   => {         code    => 2,         color   => 'white',         price   => 12,         product => 'keyboard'         },     3   => {         code    => 3,         color   => 'black',         price   => 5,         product => 'mouse'         }     }  the second example will return    { "1:gray"    => {         code    => 1,         color   => 'gray',         price   => 850,         product => 'pc'         },     "2:white"   => {         code    => 2,         color   => 'white',         price   => 12,         product => 'keyboard'         },     "3:black"   => {         code    => 3,         color   => 'black',         price   => 5,         product => 'mouse'         }     }  The C<key> attribute can be combined with L<C<headers>|/headers> for C<CSV> date that has no header line, like   my $ref = csv (      in      => "foo.csv",      headers => [qw( c_foo foo bar description stock )],      key     =>     "c_foo",      );  =head3 value  Used to create key-value hashes.  Only allowed when C<key> is valid. A C<value> can be either a single column label or an anonymous list of column labels.  In the first case,  the value will be a simple scalar value, in the latter case, it will be a hashref.   my $ref = csv (in => "test.csv", key   => "code",                                   value => "price");  my $ref = csv (in => "test.csv", key   => "code",                                   value => [ "product", "price" ]);  my $ref = csv (in => "test.csv", key   => [ ":" => "code", "color" ],                                   value => "price");  my $ref = csv (in => "test.csv", key   => [ ":" => "code", "color" ],                                   value => [ "product", "price" ]);  with test.csv like   code,product,price,color  1,pc,850,gray  2,keyboard,12,white  3,mouse,5,black  the first example will return    { 1 => 850,     2 =>  12,     3 =>   5,     }  the second example will return    { 1   => {         price   => 850,         product => 'pc'         },     2   => {         price   => 12,         product => 'keyboard'         },     3   => {         price   => 5,         product => 'mouse'         }     }  the third example will return    { "1:gray"    => 850,     "2:white"   =>  12,     "3:black"   =>   5,     }  the fourth example will return    { "1:gray"    => {         price   => 850,         product => 'pc'         },     "2:white"   => {         price   => 12,         product => 'keyboard'         },     "3:black"   => {         price   => 5,         product => 'mouse'         }     }  =head3 keep_headers  When using hashes,  keep the column names into the arrayref passed,  so all headers are available after the call in the original order.   my $aoh = csv (in => "file.csv", keep_headers => \my @hdr);  This attribute can be abbreviated to C<kh> or passed as C<keep_column_names>.  This attribute implies a default of C<auto> for the C<headers> attribute.  The headers can also be kept internally to keep stable header order:   csv (in      => csv (in => "file.csv", kh => "internal"),       out     => "new.csv",       kh      => "internal");  where C<internal> can also be C<1>, C<yes>, or C<true>. This is similar to   my @h;  csv (in      => csv (in => "file.csv", kh => \@h),       out     => "new.csv",       headers => \@h);  =head3 fragment  Only output the fragment as defined in the L</fragment> method. This option is ignored when I<generating> C<CSV>. See L</out>.  Combining all of them could give something like   use Text::CSV_PP qw( csv );  my $aoh = csv (      in       => "test.txt",      encoding => "utf-8",      headers  => "auto",      sep_char => "|",      fragment => "row=3;6-9;15-*",      );  say $aoh->[15]{Foo};  =head3 sep_set  If C<sep_set> is set, the method L</header> is invoked on the opened stream to detect and set L<C<sep_char>|/sep_char> with the given set.  C<sep_set> can be abbreviated to C<seps>. If neither C<sep_set> not C<seps> is given, but C<sep> is defined, C<sep_set> defaults to C<[ sep ]>. This is only supported for perl version 5.10 and up.  Note that as the  L</header> method is invoked,  its default is to also set the headers.  =head3 set_column_names  If  C<set_column_names> is passed,  the method L</header> is invoked on the opened stream with all arguments meant for L</header>.  If C<set_column_names> is passed as a false value, the content of the first row is only preserved if the output is AoA:  With an input-file like   bAr,foo  1,2  3,4,5  This call   my $aoa = csv (in => $file, set_column_names => 0);  will result in   [[ "bar", "foo"     ],   [ "1",   "2"       ],   [ "3",   "4",  "5" ]]  and   my $aoa = csv (in => $file, set_column_names => 0, munge => "none");  will result in   [[ "bAr", "foo"     ],   [ "1",   "2"       ],   [ "3",   "4",  "5" ]]  =head2 Callbacks  Callbacks enable actions triggered from the I<inside> of Text::CSV_PP.  While most of what this enables  can easily be done in an  unrolled loop as described in the L</SYNOPSIS> callbacks can be used to meet special demands or enhance the L</csv> function.  =over 2  =item error   $csv->callbacks (error => sub { $csv->SetDiag (0) });  the C<error>  callback is invoked when an error occurs,  but  I<only>  when L</auto_diag> is set to a true value. A callback is invoked with the values returned by L</error_diag>:   my ($c, $s);   sub ignore3006 {      my ($err, $msg, $pos, $recno, $fldno) = @_;      if ($err == 3006) {          # ignore this error          ($c, $s) = (undef, undef);          Text::CSV_PP->SetDiag (0);          }      # Any other error      return;      } # ignore3006   $csv->callbacks (error => \&ignore3006);  $csv->bind_columns (\$c, \$s);  while ($csv->getline ($fh)) {      # Error 3006 will not stop the loop      }  =item after_parse   $csv->callbacks (after_parse => sub { push @{$_[1]}, "NEW" });  while (my $row = $csv->getline ($fh)) {      $row->[-1] eq "NEW";      }  This callback is invoked after parsing with  L</getline>  only if no  error occurred.  The callback is invoked with two arguments:   the current C<CSV> parser object and an array reference to the fields parsed.  The return code of the callback is ignored  unless it is a reference to the string "skip", in which case the record will be skipped in L</getline_all>.   sub add_from_db {      my ($csv, $row) = @_;      $sth->execute ($row->[4]);      push @$row, $sth->fetchrow_array;      } # add_from_db   my $aoa = csv (in => "file.csv", callbacks => {      after_parse => \&add_from_db });  This hook can be used for validation:  =over 2  =item FAIL  Die if any of the records does not validate a rule:   after_parse => sub {      $_[1][4] =~ m/^[0-9]{4}\s?[A-Z]{2}$/ or          die "5th field does not have a valid Dutch zipcode";      }  =item DEFAULT  Replace invalid fields with a default value:   after_parse => sub { $_[1][2] =~ m/^\d+$/ or $_[1][2] = 0 }  =item SKIP  Skip records that have invalid fields (only applies to L</getline_all>):   after_parse => sub { $_[1][0] =~ m/^\d+$/ or return \"skip"; }  =back  =item before_print   my $idx = 1;  $csv->callbacks (before_print => sub { $_[1][0] = $idx++ });  $csv->print (*STDOUT, [ 0, $_ ]) for @members;  This callback is invoked  before printing with  L</print>  only if no error occurred.  The callback is invoked with two arguments:  the current  C<CSV> parser object and an array reference to the fields passed.  The return code of the callback is ignored.   sub max_4_fields {      my ($csv, $row) = @_;      @$row > 4 and splice @$row, 4;      } # max_4_fields   csv (in => csv (in => "file.csv"), out => *STDOUT,      callbacks => { before_print => \&max_4_fields });  This callback is not active for L</combine>.  =back  =head3 Callbacks for csv ()  The L</csv> allows for some callbacks that do not integrate in XS internals but only feature the L</csv> function.    csv (in        => "file.csv",        callbacks => {            filter       => { 6 => sub { $_ > 15 } },    # first            after_parse  => sub { say "AFTER PARSE";  }, # first            after_in     => sub { say "AFTER IN";     }, # second            on_in        => sub { say "ON IN";        }, # third            },        );    csv (in        => $aoh,        out       => "file.csv",        callbacks => {            on_in        => sub { say "ON IN";        }, # first            before_out   => sub { say "BEFORE OUT";   }, # second            before_print => sub { say "BEFORE PRINT"; }, # third            },        );  =over 2  =item filter  This callback can be used to filter records.  It is called just after a new record has been scanned.  The callback accepts a:  =over 2  =item hashref  The keys are the index to the row (the field name or field number, 1-based) and the values are subs to return a true or false value.   csv (in => "file.csv", filter => {             3 => sub { m/a/ },       # third field should contain an "a"             5 => sub { length > 4 }, # length of the 5th field minimal 5             });   csv (in => "file.csv", filter => { foo => sub { $_ > 4 }});  If the keys to the filter hash contain any character that is not a digit it will also implicitly set L</headers> to C<"auto">  unless  L</headers>  was already passed as argument.  When headers are active, returning an array of hashes, the filter is not applicable to the header itself.  All sub results should match, as in AND.  The context of the callback sets  C<$_> localized to the field indicated by the filter. The two arguments are as with all other callbacks, so the other fields in the current row can be seen:   filter => { 3 => sub { $_ > 100 ? $_[1][1] =~ m/A/ : $_[1][6] =~ m/B/ }}  If the context is set to return a list of hashes  (L</headers> is defined), the current record will also be available in the localized C<%_>:   filter => { 3 => sub { $_ > 100 && $_{foo} =~ m/A/ && $_{bar} < 1000  }}  If the filter is used to I<alter> the content by changing C<$_>,  make sure that the sub returns true in order not to have that record skipped:   filter => { 2 => sub { $_ = uc }}  will upper-case the second field, and then skip it if the resulting content evaluates to false. To always accept, end with truth:   filter => { 2 => sub { $_ = uc; 1 }}  =item coderef   csv (in => "file.csv", filter => sub { $n++; 0; });  If the argument to C<filter> is a coderef,  it is an alias or shortcut to a filter on column 0:   csv (filter => sub { $n++; 0 });  is equal to   csv (filter => { 0 => sub { $n++; 0 });  =item filter-name   csv (in => "file.csv", filter => "not_blank");  csv (in => "file.csv", filter => "not_empty");  csv (in => "file.csv", filter => "filled");  These are predefined filters  Given a file like (line numbers prefixed for doc purpose only):   1:1,2,3  2:  3:,  4:""  5:,,  6:, ,  7:"",  8:" "  9:4,5,6  =over 2  =item not_blank  Filter out the blank lines  This filter is a shortcut for   filter => { 0 => sub { @{$_[1]} > 1 or              defined $_[1][0] && $_[1][0] ne "" } }  Due to the implementation,  it is currently impossible to also filter lines that consists only of a quoted empty field. These lines are also considered blank lines.  With the given example, lines 2 and 4 will be skipped.  =item not_empty  Filter out lines where all the fields are empty.  This filter is a shortcut for   filter => { 0 => sub { grep { defined && $_ ne "" } @{$_[1]} } }  A space is not regarded being empty, so given the example data, lines 2, 3, 4, 5, and 7 are skipped.  =item filled  Filter out lines that have no visible data  This filter is a shortcut for   filter => { 0 => sub { grep { defined && m/\S/ } @{$_[1]} } }  This filter rejects all lines that I<not> have at least one field that does not evaluate to the empty string.  With the given example data, this filter would skip lines 2 through 8.  =back  =back  One could also use modules like L<Types::Standard>:   use Types::Standard -types;   my $type   = Tuple[Str, Str, Int, Bool, Optional[Num]];  my $check  = $type->compiled_check;   # filter with compiled check and warnings  my $aoa = csv (     in     => \$data,     filter => {         0 => sub {             my $ok = $check->($_[1]) or                 warn $type->get_message ($_[1]), "\n";             return $ok;             },         },     );  =item after_in  This callback is invoked for each record after all records have been parsed but before returning the reference to the caller.  The hook is invoked with two arguments:  the current  C<CSV>  parser object  and a  reference to the record.   The reference can be a reference to a  HASH  or a reference to an ARRAY as determined by the arguments.  This callback can also be passed as  an attribute without the  C<callbacks> wrapper.  =item before_out  This callback is invoked for each record before the record is printed.  The hook is invoked with two arguments:  the current C<CSV> parser object and a reference to the record.   The reference can be a reference to a  HASH or a reference to an ARRAY as determined by the arguments.  This callback can also be passed as an attribute  without the  C<callbacks> wrapper.  This callback makes the row available in C<%_> if the row is a hashref.  In this case C<%_> is writable and will change the original row.  =item on_in  This callback acts exactly as the L</after_in> or the L</before_out> hooks.  This callback can also be passed as an attribute  without the  C<callbacks> wrapper.  This callback makes the row available in C<%_> if the row is a hashref.  In this case C<%_> is writable and will change the original row. So e.g. with    my $aoh = csv (       in      => \"foo\n1\n2\n",       headers => "auto",       on_in   => sub { $_{bar} = 2; },       );  C<$aoh> will be:    [ { foo => 1,       bar => 2,       }     { foo => 2,       bar => 2,       }     ]  =item csv  The I<function>  L</csv> can also be called as a method or with an existing Text::CSV_PP object. This could help if the function is to be invoked a lot of times and the overhead of creating the object internally over  and  over again would be prevented by passing an existing instance.   my $csv = Text::CSV_PP->new ({ binary => 1, auto_diag => 1 });   my $aoa = $csv->csv (in => $fh);  my $aoa = csv (in => $fh, csv => $csv);  both act the same. Running this 20000 times on a 20 lines CSV file,  showed a 53% speedup.  =back  =head1 DIAGNOSTICS  This section is also taken from Text::CSV_XS.  Still under construction ...  If an error occurs,  C<< $csv->error_diag >> can be used to get information on the cause of the failure. Note that for speed reasons the internal value is never cleared on success,  so using the value returned by L</error_diag> in normal cases - when no error occurred - may cause unexpected results.  If the constructor failed, the cause can be found using L</error_diag> as a class method, like C<< Text::CSV_PP->error_diag >>.  The C<< $csv->error_diag >> method is automatically invoked upon error when the contractor was called with  L<C<auto_diag>|/auto_diag>  set to  C<1> or C<2>, or when L<autodie> is in effect.  When set to C<1>, this will cause a C<warn> with the error message,  when set to C<2>, it will C<die>. C<2012 - EOF> is excluded from L<C<auto_diag>|/auto_diag> reports.  Errors can be (individually) caught using the L</error> callback.  The errors as described below are available. I have tried to make the error itself explanatory enough, but more descriptions will be added. For most of these errors, the first three capitals describe the error category:  =over 2  =item * INI  Initialization error or option conflict.  =item * ECR  Carriage-Return related parse error.  =item * EOF  End-Of-File related parse error.  =item * EIQ  Parse error inside quotation.  =item * EIF  Parse error inside field.  =item * ECB  Combine error.  =item * EHR  HashRef parse related error.  =back  And below should be the complete list of error codes that can be returned:  =over 2  =item * 1001 "INI - sep_char is equal to quote_char or escape_char"  The  L<separation character|/sep_char>  cannot be equal to  L<the quotation character|/quote_char> or to L<the escape character|/escape_char>,  as this would invalidate all parsing rules.  =item * 1002 "INI - allow_whitespace with escape_char or quote_char SP or TAB"  Using the  L<C<allow_whitespace>|/allow_whitespace>  attribute  when either L<C<quote_char>|/quote_char> or L<C<escape_char>|/escape_char>  is equal to C<SPACE> or C<TAB> is too ambiguous to allow.  =item * 1003 "INI - \r or \n in main attr not allowed"  Using default L<C<eol>|/eol> characters in either L<C<sep_char>|/sep_char>, L<C<quote_char>|/quote_char>,   or  L<C<escape_char>|/escape_char>  is  not allowed.  =item * 1004 "INI - callbacks should be undef or a hashref"  The L<C<callbacks>|/Callbacks>  attribute only allows one to be C<undef> or a hash reference.  =item * 1005 "INI - EOL too long"  The value passed for EOL is exceeding its maximum length (16).  =item * 1006 "INI - SEP too long"  The value passed for SEP is exceeding its maximum length (16).  =item * 1007 "INI - QUOTE too long"  The value passed for QUOTE is exceeding its maximum length (16).  =item * 1008 "INI - SEP undefined"  The value passed for SEP should be defined and not empty.  =item * 1010 "INI - the header is empty"  The header line parsed in the L</header> is empty.  =item * 1011 "INI - the header contains more than one valid separator"  The header line parsed in the  L</header>  contains more than one  (unique) separator character out of the allowed set of separators.  =item * 1012 "INI - the header contains an empty field"  The header line parsed in the L</header> contains an empty field.  =item * 1013 "INI - the header contains nun-unique fields"  The header line parsed in the  L</header>  contains at least  two identical fields.  =item * 1014 "INI - header called on undefined stream"  The header line cannot be parsed from an undefined source.  =item * 1500 "PRM - Invalid/unsupported argument(s)"  Function or method called with invalid argument(s) or parameter(s).  =item * 1501 "PRM - The key attribute is passed as an unsupported type"  The C<key> attribute is of an unsupported type.  =item * 1502 "PRM - The value attribute is passed without the key attribute"  The C<value> attribute is only allowed when a valid key is given.  =item * 1503 "PRM - The value attribute is passed as an unsupported type"  The C<value> attribute is of an unsupported type.  =item * 2010 "ECR - QUO char inside quotes followed by CR not part of EOL"  When  L<C<eol>|/eol>  has  been  set  to  anything  but the  default,  like C<"\r\t\n">,  and  the  C<"\r">  is  following  the   B<second>   (closing) L<C<quote_char>|/quote_char>, where the characters following the C<"\r"> do not make up the L<C<eol>|/eol> sequence, this is an error.  =item * 2011 "ECR - Characters after end of quoted field"  Sequences like C<1,foo,"bar"baz,22,1> are not allowed. C<"bar"> is a quoted field and after the closing double-quote, there should be either a new-line sequence or a separation character.  =item * 2012 "EOF - End of data in parsing input stream"  Self-explaining. End-of-file while inside parsing a stream. Can happen only when reading from streams with L</getline>,  as using  L</parse> is done on strings that are not required to have a trailing L<C<eol>|/eol>.  =item * 2013 "INI - Specification error for fragments RFC7111"  Invalid specification for URI L</fragment> specification.  =item * 2014 "ENF - Inconsistent number of fields"  Inconsistent number of fields under strict parsing.  =item * 2015 "ERW - Empty row"  An empty row was not allowed.  =item * 2021 "EIQ - NL char inside quotes, binary off"  Sequences like C<1,"foo\nbar",22,1> are allowed only when the binary option has been selected with the constructor.  =item * 2022 "EIQ - CR char inside quotes, binary off"  Sequences like C<1,"foo\rbar",22,1> are allowed only when the binary option has been selected with the constructor.  =item * 2023 "EIQ - QUO character not allowed"  Sequences like C<"foo "bar" baz",qu> and C<2023,",2008-04-05,"Foo, Bar",\n> will cause this error.  =item * 2024 "EIQ - EOF cannot be escaped, not even inside quotes"  The escape character is not allowed as last character in an input stream.  =item * 2025 "EIQ - Loose unescaped escape"  An escape character should escape only characters that need escaping.  Allowing  the escape  for other characters  is possible  with the attribute L</allow_loose_escapes>.  =item * 2026 "EIQ - Binary character inside quoted field, binary off"  Binary characters are not allowed by default.    Exceptions are fields that contain valid UTF-8,  that will automatically be upgraded if the content is valid UTF-8. Set L<C<binary>|/binary> to C<1> to accept binary data.  =item * 2027 "EIQ - Quoted field not terminated"  When parsing a field that started with a quotation character,  the field is expected to be closed with a quotation character.   When the parsed line is exhausted before the quote is found, that field is not terminated.  =item * 2030 "EIF - NL char inside unquoted verbatim, binary off"  =item * 2031 "EIF - CR char is first char of field, not part of EOL"  =item * 2032 "EIF - CR char inside unquoted, not part of EOL"  =item * 2034 "EIF - Loose unescaped quote"  =item * 2035 "EIF - Escaped EOF in unquoted field"  =item * 2036 "EIF - ESC error"  =item * 2037 "EIF - Binary character in unquoted field, binary off"  =item * 2110 "ECB - Binary character in Combine, binary off"  =item * 2200 "EIO - print to IO failed. See errno"  =item * 3001 "EHR - Unsupported syntax for column_names ()"  =item * 3002 "EHR - getline_hr () called before column_names ()"  =item * 3003 "EHR - bind_columns () and column_names () fields count mismatch"  =item * 3004 "EHR - bind_columns () only accepts refs to scalars"  =item * 3006 "EHR - bind_columns () did not pass enough refs for parsed fields"  =item * 3007 "EHR - bind_columns needs refs to writable scalars"  =item * 3008 "EHR - unexpected error in bound fields"  =item * 3009 "EHR - print_hr () called before column_names ()"  =item * 3010 "EHR - print_hr () called with invalid arguments"  =back  =head1 SEE ALSO  L<Text::CSV_XS>, L<Text::CSV>  Older versions took many regexp from L<http://www.din.or.jp/~ohzaki/perl.htm>  =head1 AUTHOR  Kenichi Ishigaki, E<lt>ishigaki[at]cpan.orgE<gt> Makamaka Hannyaharamitu, E<lt>makamaka[at]cpan.orgE<gt>  Text::CSV_XS was written by E<lt>joe[at]ispsoft.deE<gt> and maintained by E<lt>h.m.brand[at]xs4all.nlE<gt>.  Text::CSV was written by E<lt>alan[at]mfgrtl.comE<gt>.  =head1 COPYRIGHT AND LICENSE  Copyright 2017- by Kenichi Ishigaki, E<lt>ishigaki[at]cpan.orgE<gt> Copyright 2005-2015 by Makamaka Hannyaharamitu, E<lt>makamaka[at]cpan.orgE<gt>  Most of the code and doc is directly taken from the pure perl part of Text::CSV_XS.  Copyright (C) 2007-2016 H.Merijn Brand.  All rights reserved. Copyright (C) 1998-2001 Jochen Wiedmann. All rights reserved. Copyright (C) 1997      Alan Citterman.  All rights reserved.  This library is free software; you can redistribute it and/or modify it under the same terms as Perl itself.  =cut 
;; PARSE ERROR: Fell through. Missing case: [


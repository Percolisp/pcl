(in-package :pcl)
(setf pcl::*pcl-pl2cl-path* #P"/home/bernt/pcl/pl2cl")
;; Initialize @INC from Perl
(setf pcl::@INC (make-array 0 :adjustable t :fill-pointer 0))
(vector-push-extend "." pcl::@INC)
(vector-push-extend "/home/bernt/pcl/lib" pcl::@INC)
(vector-push-extend "/home/bernt/pcl" pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3/x86_64-linux" pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/site_perl/5.40.3" pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/5.40.3/x86_64-linux" pcl::@INC)
(vector-push-extend "/home/bernt/perl5/perlbrew/perls/perl-5.40.3/lib/5.40.3" pcl::@INC)
;; Switch to main package (Perl's default for code without 'package' statement)
(p-defpackage :main)
(in-package :main)

(p-declare-sub pl-p_unpack)
(p-declare-sub pl-_next_format_item)
(p-declare-sub pl-_unpack_tmpl)
(p-declare-sub pl-_unpack_str)
(p-declare-sub pl-_unpack_utf8_char)
(p-declare-sub pl-p_pack)
(p-declare-sub pl-_pack_check_brackets)
(p-declare-sub pl-_pack_tmpl)
(p-declare-sub pl-_pack_utf8_char)
(p-declare-sub pl-_pack_str_one)
(p-declare-sub pl-_unpack_float64)
(p-declare-sub pl-_unpack_float32)
(p-declare-sub pl-_pack_float64)
(p-declare-sub pl-_pack_float32)
(p-declare-sub pl-_unpack_read_int)
(p-declare-sub pl-_pack_emit_int)
(p-declare-sub pl-_pack_parse_count)
(p-declare-sub pl-_pack_template_size)
(p-declare-sub pl-_pack_parse_mods)
(p-declare-sub pl-_pack_find_group_end)
(p-declare-sub pl-_pack_skip_ws)
(p-declare-sub pl-_pack_type_info)
;; my $CAN_ENDIAN = 'sSiIlLqQjJfFdDpP'
(p-eval-always
  (defvar $CAN_ENDIAN (make-p-box nil)))
;; my $CAN_SHRIEK = 'sSiIlLnNvVxX.@'
(p-eval-always
  (defvar $CAN_SHRIEK (make-p-box nil)))
;; my $MAX_GROUP_DEPTH = 100
(p-eval-always
  (defvar $MAX_GROUP_DEPTH (make-p-box nil)))
;; sub _pack_type_info { ... }
(p-sub pl-_pack_type_info (&rest %_args)
  (p-args-body
    (block nil
      (let (($ch (make-p-box nil)) ($bang (make-p-box nil)))
        ;; my ($ch, $bang) = @_
                (let ((*wantarray* nil)) (p-list-= (vector $ch $bang) @_))
        
        ;; if    ($ch eq 'c') { return (1, 1, 0) }     elsif ($ch eq 'C') { return (1, 0, 0) }     elsif ($ch eq 's') { return (2, 1, 0) }     elsif ($ch eq 'S') { return (2, 0, 0) }     elsif ($ch eq 'n') { return (2, ($bang ? 1 : 0), 1) }     elsif ($ch eq 'v') { return (2, ($bang ? 1 : 0), 0) }     elsif ($ch eq 'i') { return (4, 1, 0) }     elsif ($ch eq 'I') { return (4, 0, 0) }     elsif ($ch eq 'l') { return (($bang ? 8 : 4), 1, 0) }     elsif ($ch eq 'L') { return (($bang ? 8 : 4), 0, 0) }     elsif ($ch eq 'N') { return (4, ($bang ? 1 : 0), 1) }     elsif ($ch eq 'V') { return (4, ($bang ? 1 : 0), 0) }     elsif ($ch eq 'q') { return (8, 1, 0) }     elsif ($ch eq 'Q') { return (8, 0, 0) }     elsif ($ch eq 'j') { return (8, 1, 0) }     elsif ($ch eq 'J') { return (8, 0, 0) }
        ;; if ($ch eq 'c')
        (p-if (p-str-eq $ch "c")
          (progn
            ;; return (1, 1, 0)
                        (p-return 1 1 0)
            
          )
          ;; elsif ($ch eq 'C')
          (p-if (p-str-eq $ch "C")
            (progn
              ;; return (1, 0, 0)
                            (p-return 1 0 0)
              
            )
            ;; elsif ($ch eq 's')
            (p-if (p-str-eq $ch "s")
              (progn
                ;; return (2, 1, 0)
                                (p-return 2 1 0)
                
              )
              ;; elsif ($ch eq 'S')
              (p-if (p-str-eq $ch "S")
                (progn
                  ;; return (2, 0, 0)
                                    (p-return 2 0 0)
                  
                )
                ;; elsif ($ch eq 'n')
                (p-if (p-str-eq $ch "n")
                  (progn
                    ;; return (2, ($bang ? 1 : 0), 1)
                                        (p-return 2 (p-if $bang 1 0) 1)
                    
                  )
                  ;; elsif ($ch eq 'v')
                  (p-if (p-str-eq $ch "v")
                    (progn
                      ;; return (2, ($bang ? 1 : 0), 0)
                                            (p-return 2 (p-if $bang 1 0) 0)
                      
                    )
                    ;; elsif ($ch eq 'i')
                    (p-if (p-str-eq $ch "i")
                      (progn
                        ;; return (4, 1, 0)
                                                (p-return 4 1 0)
                        
                      )
                      ;; elsif ($ch eq 'I')
                      (p-if (p-str-eq $ch "I")
                        (progn
                          ;; return (4, 0, 0)
                                                    (p-return 4 0 0)
                          
                        )
                        ;; elsif ($ch eq 'l')
                        (p-if (p-str-eq $ch "l")
                          (progn
                            ;; return (($bang ? 8 : 4), 1, 0)
                                                        (p-return (p-if $bang 8 4) 1 0)
                            
                          )
                          ;; elsif ($ch eq 'L')
                          (p-if (p-str-eq $ch "L")
                            (progn
                              ;; return (($bang ? 8 : 4), 0, 0)
                                                            (p-return (p-if $bang 8 4) 0 0)
                              
                            )
                            ;; elsif ($ch eq 'N')
                            (p-if (p-str-eq $ch "N")
                              (progn
                                ;; return (4, ($bang ? 1 : 0), 1)
                                                                (p-return 4 (p-if $bang 1 0) 1)
                                
                              )
                              ;; elsif ($ch eq 'V')
                              (p-if (p-str-eq $ch "V")
                                (progn
                                  ;; return (4, ($bang ? 1 : 0), 0)
                                                                    (p-return 4 (p-if $bang 1 0) 0)
                                  
                                )
                                ;; elsif ($ch eq 'q')
                                (p-if (p-str-eq $ch "q")
                                  (progn
                                    ;; return (8, 1, 0)
                                                                        (p-return 8 1 0)
                                    
                                  )
                                  ;; elsif ($ch eq 'Q')
                                  (p-if (p-str-eq $ch "Q")
                                    (progn
                                      ;; return (8, 0, 0)
                                                                            (p-return 8 0 0)
                                      
                                    )
                                    ;; elsif ($ch eq 'j')
                                    (p-if (p-str-eq $ch "j")
                                      (progn
                                        ;; return (8, 1, 0)
                                                                                (p-return 8 1 0)
                                        
                                      )
                                      ;; elsif ($ch eq 'J')
                                      (p-if (p-str-eq $ch "J")
                                        (progn
                                          ;; return (8, 0, 0)
                                                                                    (p-return 8 0 0)
                                          
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
                )
              )
            )
          )
        )
        
        ;; return ()
                (p-return)
        
      )
    )
  )
)

;; our $pcl_pack_comma_warned = 0
(p-eval-always
  (defvar $pcl_pack_comma_warned (make-p-box nil)))
;; sub _pack_skip_ws { ... }
(p-sub pl-_pack_skip_ws (&rest %_args)
  (p-args-body
    (block nil
      (let (($ch (make-p-box nil)))
        (let (($s (make-p-box nil)) ($ti (make-p-box nil)))
          ;; my ($s, $ti) = @_
                    (let ((*wantarray* nil)) (p-list-= (vector $s $ti) @_))
          
          (let (($tlen (make-p-box nil)))
            ;; my $tlen = length($s)
                        (p-my-= $tlen (p-length $s))
            
            ;; while ($ti < $tlen) {         my $ch = substr($s, $ti, 1);         if ($ch eq ' ' || $ch eq "\t" || $ch eq "\n" || $ch eq "\r" || $ch eq "\f") {             $ti++;         } elsif ($ch eq ',') {             # Perl warns on commas (once per pack call) but treats as separator             unless ($pcl_pack_comma_warned) {                 warn "Invalid type ',' in pack\n";                 $pcl_pack_comma_warned = 1;             }             $ti++;         } elsif ($ch eq '#') {             $ti++;             while ($ti < $tlen && substr($s, $ti, 1) ne "\n") { $ti++ }             $ti++ if $ti < $tlen;         } else {             last;         }     }
            (p-while (p-< $ti $tlen)
              (let (($ch (make-p-box nil)))
                ;; my $ch = substr($s, $ti, 1)
                                (p-my-= $ch (p-substr $s $ti 1))
                
                ;; if ($ch eq ' ' || $ch eq "\t" || $ch eq "\n" || $ch eq "\r" || $ch eq "\f") {             $ti++;         } elsif ($ch eq ',') {             # Perl warns on commas (once per pack call) but treats as separator             unless ($pcl_pack_comma_warned) {                 warn "Invalid type ',' in pack\n";                 $pcl_pack_comma_warned = 1;             }             $ti++;         } elsif ($ch eq '#') {             $ti++;             while ($ti < $tlen && substr($s, $ti, 1) ne "\n") { $ti++ }             $ti++ if $ti < $tlen;         } else {             last;         }
                ;; if ($ch eq ' ' || $ch eq "\t" || $ch eq "\n" || $ch eq "\r" || $ch eq "\f")
                (p-if (p-|| (p-|| (p-|| (p-|| (p-str-eq $ch " ") (p-str-eq $ch "	")) (p-str-eq $ch "
")) (p-str-eq $ch "")) (p-str-eq $ch ""))
                  (progn
                    ;; $ti++
                                        (p-post++ $ti)
                    
                  )
                  ;; elsif ($ch eq ',')
                  (p-if (p-str-eq $ch ",")
                    (progn
                      ;; unless ($pcl_pack_comma_warned) {                 warn "Invalid type ',' in pack\n";                 $pcl_pack_comma_warned = 1;             }
                      ;; unless ($pcl_pack_comma_warned)
                      (p-if (p-not $pcl_pack_comma_warned)
                        (progn
                          ;; warn "Invalid type ',' in pack\n"
                          (let ((*wantarray* :void))                           (p-warn :loc "- line 56" "Invalid type ',' in pack
"))
                          
                          ;; $pcl_pack_comma_warned = 1
                                                    (p-scalar-= $pcl_pack_comma_warned 1)
                          
                        )
                        nil
                      )
                      
                      ;; $ti++
                                            (p-post++ $ti)
                      
                    )
                    ;; elsif ($ch eq '#')
                    (p-if (p-str-eq $ch "#")
                      (progn
                        (let ((--pcl-if-ret--0 nil))
                          ;; $ti++
                          (let ((*wantarray* :void))                           (p-post++ $ti))
                          
                          ;; while ($ti < $tlen && substr($s, $ti, 1) ne "\n") { $ti++ }
                          (p-while (p-&& (p-< $ti $tlen) (p-str-ne (p-substr $s $ti 1) "
"))
                            ;; $ti++
                                                        (p-post++ $ti)
                            
                          )
                          
                          ;; $ti++ if $ti < $tlen
                          (p-if (setf --pcl-if-ret--0 (p-< $ti $tlen))
                            (setf --pcl-if-ret--0 (p-post++ $ti))
                            nil)
                          
                        --pcl-if-ret--0)
                      )
                      ;; else
                      (progn
                        ;; last
                                                (p-last)
                        
                      )
                    )
                  )
                )
                
              )
            )
            
            ;; return $ti
                        (p-return $ti)
            
          )
        )
      )
    )
  )
)

;; sub _pack_find_group_end { ... }
(p-sub pl-_pack_find_group_end (&rest %_args)
  (p-args-body
    (block nil
      (let (($ch (make-p-box nil)))
        (let (($s (make-p-box nil)) ($ti (make-p-box nil)))
          ;; my ($s, $ti) = @_
                    (let ((*wantarray* nil)) (p-list-= (vector $s $ti) @_))
          
          (let (($tlen (make-p-box nil)))
            ;; my $tlen = length($s)
                        (p-my-= $tlen (p-length $s))
            
            (let (($depth (make-p-box nil)))
              ;; my $depth = 1
                            (p-my-= $depth 1)
              
              ;; while ($ti < $tlen && $depth > 0) {         my $ch = substr($s, $ti, 1);         if ($ch eq '#') {             $ti++;             while ($ti < $tlen && substr($s, $ti, 1) ne "\n") { $ti++ }         } elsif ($ch eq '(') {             $depth++; $ti++;         } elsif ($ch eq ')') {             $depth--;             $ti++ if $depth > 0;         } else {             $ti++;         }     }
              (p-while (p-&& (p-< $ti $tlen) (p-> $depth 0))
                (let (($ch (make-p-box nil)))
                  ;; my $ch = substr($s, $ti, 1)
                                    (p-my-= $ch (p-substr $s $ti 1))
                  
                  ;; if ($ch eq '#') {             $ti++;             while ($ti < $tlen && substr($s, $ti, 1) ne "\n") { $ti++ }         } elsif ($ch eq '(') {             $depth++; $ti++;         } elsif ($ch eq ')') {             $depth--;             $ti++ if $depth > 0;         } else {             $ti++;         }
                  ;; if ($ch eq '#')
                  (p-if (p-str-eq $ch "#")
                    (progn
                      ;; $ti++
                                            (p-post++ $ti)
                      
                      ;; while ($ti < $tlen && substr($s, $ti, 1) ne "\n") { $ti++ }
                      (p-while (p-&& (p-< $ti $tlen) (p-str-ne (p-substr $s $ti 1) "
"))
                        ;; $ti++
                                                (p-post++ $ti)
                        
                      )
                      
                    )
                    ;; elsif ($ch eq '(')
                    (p-if (p-str-eq $ch "(")
                      (progn
                        ;; $depth++
                        (let ((*wantarray* :void))                         (p-post++ $depth))
                        
                        ;; $ti++
                                                (p-post++ $ti)
                        
                      )
                      ;; elsif ($ch eq ')')
                      (p-if (p-str-eq $ch ")")
                        (progn
                          (let ((--pcl-if-ret--1 nil))
                            ;; $depth--
                            (let ((*wantarray* :void))                             (p-post-- $depth))
                            
                            ;; $ti++ if $depth > 0
                            (p-if (setf --pcl-if-ret--1 (p-> $depth 0))
                              (setf --pcl-if-ret--1 (p-post++ $ti))
                              nil)
                            
                          --pcl-if-ret--1)
                        )
                        ;; else
                        (progn
                          ;; $ti++
                                                    (p-post++ $ti)
                          
                        )
                      )
                    )
                  )
                  
                )
              )
              
              ;; return $ti
                            (p-return $ti)
              
            )
          )
        )
      )
    )
  )
)

;; sub _pack_parse_mods { ... }
(p-sub pl-_pack_parse_mods (&rest %_args)
  (p-args-body
    (block nil
      (let (($m (make-p-box nil)))
        (let (($tmpl (make-p-box nil)) ($ti_ref (make-p-box nil)) ($inh_be (make-p-box nil)) ($inh_le (make-p-box nil)) ($ch (make-p-box nil)) ($ctx (make-p-box nil)))
          ;; my ($tmpl, $ti_ref, $inh_be, $inh_le, $ch, $ctx) = @_
                    (let ((*wantarray* nil)) (p-list-= (vector $tmpl $ti_ref $inh_be $inh_le $ch $ctx) @_))
          
          (let (($tlen (make-p-box nil)))
            ;; my $tlen = length($tmpl)
                        (p-my-= $tlen (p-length $tmpl))
            
            (let (($bang (make-p-box nil)) ($be (make-p-box nil)) ($le (make-p-box nil)))
              ;; my ($bang, $be, $le) = (0, $inh_be, $inh_le)
                            (let ((*wantarray* nil)) (p-list-= (vector $bang $be $le) (vector 0 $inh_be $inh_le)))
              
              (let (($got_be (make-p-box nil)) ($got_le (make-p-box nil)) ($got_bang (make-p-box nil)))
                ;; my ($got_be, $got_le, $got_bang) = (0, 0, 0)
                                (let ((*wantarray* nil)) (p-list-= (vector $got_be $got_le $got_bang) (vector 0 0 0)))
                
                ;; while ($$ti_ref < $tlen) {         my $m = substr($tmpl, $$ti_ref, 1);         if ($m eq '!') {             die "'!' allowed only after types $CAN_SHRIEK in $ctx\n"                 unless index($CAN_SHRIEK, $ch) >= 0;             warn "Duplicate modifier '!' after '$ch' in $ctx\n" if $got_bang;             $bang = 1; $got_bang = 1; $$ti_ref++;         } elsif ($m eq '>') {             die "'>' allowed only after types $CAN_ENDIAN in $ctx\n"                 unless index($CAN_ENDIAN, $ch) >= 0 || $ch eq '(';             die "Can't use both '<' and '>' after type '$ch' in $ctx\n" if $got_le;             die "Can't use '>' in a group with different byte-order in $ctx\n" if $inh_le;             warn "Duplicate modifier '>' after '$ch' in $ctx\n" if $got_be;             $be = 1; $le = 0; $got_be = 1; $$ti_ref++;         } elsif ($m eq '<') {             die "'<' allowed only after types $CAN_ENDIAN in $ctx\n"                 unless index($CAN_ENDIAN, $ch) >= 0 || $ch eq '(';             die "Can't use both '<' and '>' after type '$ch' in $ctx\n" if $got_be;             die "Can't use '<' in a group with different byte-order in $ctx\n" if $inh_be;             warn "Duplicate modifier '<' after '$ch' in $ctx\n" if $got_le;             $le = 1; $be = 0; $got_le = 1; $$ti_ref++;         } else {             last;         }     }
                (p-while (p-< (p-cast-$ $ti_ref) $tlen)
                  (let (($m (make-p-box nil)))
                    ;; my $m = substr($tmpl, $$ti_ref, 1)
                                        (p-my-= $m (p-substr $tmpl (p-cast-$ $ti_ref) 1))
                    
                    ;; if ($m eq '!') {             die "'!' allowed only after types $CAN_SHRIEK in $ctx\n"                 unless index($CAN_SHRIEK, $ch) >= 0;             warn "Duplicate modifier '!' after '$ch' in $ctx\n" if $got_bang;             $bang = 1; $got_bang = 1; $$ti_ref++;         } elsif ($m eq '>') {             die "'>' allowed only after types $CAN_ENDIAN in $ctx\n"                 unless index($CAN_ENDIAN, $ch) >= 0 || $ch eq '(';             die "Can't use both '<' and '>' after type '$ch' in $ctx\n" if $got_le;             die "Can't use '>' in a group with different byte-order in $ctx\n" if $inh_le;             warn "Duplicate modifier '>' after '$ch' in $ctx\n" if $got_be;             $be = 1; $le = 0; $got_be = 1; $$ti_ref++;         } elsif ($m eq '<') {             die "'<' allowed only after types $CAN_ENDIAN in $ctx\n"                 unless index($CAN_ENDIAN, $ch) >= 0 || $ch eq '(';             die "Can't use both '<' and '>' after type '$ch' in $ctx\n" if $got_be;             die "Can't use '<' in a group with different byte-order in $ctx\n" if $inh_be;             warn "Duplicate modifier '<' after '$ch' in $ctx\n" if $got_le;             $le = 1; $be = 0; $got_le = 1; $$ti_ref++;         } else {             last;         }
                    ;; if ($m eq '!')
                    (p-if (p-str-eq $m "!")
                      (progn
                        ;; die "'!' allowed only after types $CAN_SHRIEK in $ctx\n"                 unless index($CAN_SHRIEK, $ch) >= 0
                        (p-unless (p->= (p-index $CAN_SHRIEK $ch) 0) (p-die :loc "- line 102" (p-string-concat "'!' allowed only after types " $CAN_SHRIEK " in " $ctx "
")))
                        
                        ;; warn "Duplicate modifier '!' after '$ch' in $ctx\n" if $got_bang
                        (p-if $got_bang (p-warn :loc "- line 104" (p-string-concat "Duplicate modifier '!' after '" $ch "' in " $ctx "
")))
                        
                        ;; $bang = 1
                                                (p-my-= $bang 1)
                        
                        ;; $got_bang = 1
                                                (p-my-= $got_bang 1)
                        
                        ;; $$ti_ref++
                                                (p-post++ (p-cast-$ $ti_ref))
                        
                      )
                      ;; elsif ($m eq '>')
                      (p-if (p-str-eq $m ">")
                        (progn
                          ;; die "'>' allowed only after types $CAN_ENDIAN in $ctx\n"                 unless index($CAN_ENDIAN, $ch) >= 0 || $ch eq '('
                          (let ((*wantarray* :void)) (p-unless (p-|| (p->= (p-index $CAN_ENDIAN $ch) 0) (p-str-eq $ch "(")) (p-die :loc "- line 107" (p-string-concat "'>' allowed only after types " $CAN_ENDIAN " in " $ctx "
"))))
                          
                          ;; die "Can't use both '<' and '>' after type '$ch' in $ctx\n" if $got_le
                          (let ((*wantarray* :void)) (p-if $got_le (p-die :loc "- line 109" (p-string-concat "Can't use both '<' and '>' after type '" $ch "' in " $ctx "
"))))
                          
                          ;; die "Can't use '>' in a group with different byte-order in $ctx\n" if $inh_le
                          (let ((*wantarray* :void)) (p-if $inh_le (p-die :loc "- line 110" (p-string-concat "Can't use '>' in a group with different byte-order in " $ctx "
"))))
                          
                          ;; warn "Duplicate modifier '>' after '$ch' in $ctx\n" if $got_be
                          (let ((*wantarray* :void)) (p-if $got_be (p-warn :loc "- line 111" (p-string-concat "Duplicate modifier '>' after '" $ch "' in " $ctx "
"))))
                          
                          ;; $be = 1
                          (let ((*wantarray* :void))                           (p-my-= $be 1))
                          
                          ;; $le = 0
                          (let ((*wantarray* :void))                           (p-my-= $le 0))
                          
                          ;; $got_be = 1
                          (let ((*wantarray* :void))                           (p-my-= $got_be 1))
                          
                          ;; $$ti_ref++
                                                    (p-post++ (p-cast-$ $ti_ref))
                          
                        )
                        ;; elsif ($m eq '<')
                        (p-if (p-str-eq $m "<")
                          (progn
                            ;; die "'<' allowed only after types $CAN_ENDIAN in $ctx\n"                 unless index($CAN_ENDIAN, $ch) >= 0 || $ch eq '('
                            (let ((*wantarray* :void)) (p-unless (p-|| (p->= (p-index $CAN_ENDIAN $ch) 0) (p-str-eq $ch "(")) (p-die :loc "- line 114" (p-string-concat "'<' allowed only after types " $CAN_ENDIAN " in " $ctx "
"))))
                            
                            ;; die "Can't use both '<' and '>' after type '$ch' in $ctx\n" if $got_be
                            (let ((*wantarray* :void)) (p-if $got_be (p-die :loc "- line 116" (p-string-concat "Can't use both '<' and '>' after type '" $ch "' in " $ctx "
"))))
                            
                            ;; die "Can't use '<' in a group with different byte-order in $ctx\n" if $inh_be
                            (let ((*wantarray* :void)) (p-if $inh_be (p-die :loc "- line 117" (p-string-concat "Can't use '<' in a group with different byte-order in " $ctx "
"))))
                            
                            ;; warn "Duplicate modifier '<' after '$ch' in $ctx\n" if $got_le
                            (let ((*wantarray* :void)) (p-if $got_le (p-warn :loc "- line 118" (p-string-concat "Duplicate modifier '<' after '" $ch "' in " $ctx "
"))))
                            
                            ;; $le = 1
                            (let ((*wantarray* :void))                             (p-my-= $le 1))
                            
                            ;; $be = 0
                            (let ((*wantarray* :void))                             (p-my-= $be 0))
                            
                            ;; $got_le = 1
                            (let ((*wantarray* :void))                             (p-my-= $got_le 1))
                            
                            ;; $$ti_ref++
                                                        (p-post++ (p-cast-$ $ti_ref))
                            
                          )
                          ;; else
                          (progn
                            ;; last
                                                        (p-last)
                            
                          )
                        )
                      )
                    )
                    
                  )
                )
                
                ;; return ($bang, $be, $le)
                                (p-return $bang $be $le)
                
              )
            )
          )
        )
      )
    )
  )
)

;; sub _pack_template_size { ... }
(p-sub pl-_pack_template_size (&rest %_args)
  (p-args-body
    (block nil
      (let (($ch (make-p-box nil)) ($grpbeg (make-p-box nil)) ($grpend (make-p-box nil)) ($bang (make-p-box nil)) ($all (make-p-box nil)) ($count (make-p-box nil)) ($nrep (make-p-box nil)) ($inner (make-p-box nil)) ($n (make-p-box nil)) ($nb (make-p-box nil)))
        (let (($tmpl (make-p-box nil)))
          ;; my ($tmpl) = @_
                    (let ((*wantarray* nil)) (p-list-= (vector $tmpl) @_))
          
          (let (($pos (make-p-box nil)))
            ;; my $pos = 0
                        (p-my-= $pos 0)
            
            (let (($ti (make-p-box nil)))
              ;; my $ti = 0
                            (p-my-= $ti 0)
              
              (let (($tlen (make-p-box nil)))
                ;; my $tlen = length($tmpl)
                                (p-my-= $tlen (p-length $tmpl))
                
                ;; while (1) {         $ti = _pack_skip_ws($tmpl, $ti);         last if $ti >= $tlen;         my $ch = substr($tmpl, $ti, 1); $ti++;         my ($grpbeg, $grpend) = (undef, undef);         if ($ch eq '(') {             $grpend = _pack_find_group_end($tmpl, $ti);             $grpbeg = $ti; $ti = $grpend + 1;         }         my $bang = 0;         while ($ti < $tlen && substr($tmpl, $ti, 1) =~ /[!<>]/) {             $bang = 1 if substr($tmpl, $ti, 1) eq '!';             $ti++;         }         # No ws skip here: space between type+mods and count is invalid in Perl.         my ($all, $count, $nrep) = _pack_parse_count($tmpl, \$ti);         $nrep = 1 unless defined $nrep && $nrep >= 1;         if (defined $grpbeg) {             my $inner = substr($tmpl, $grpbeg, $grpend - $grpbeg);             $pos += _pack_template_size($inner) * $nrep;             next;         }         if ($ch eq '@') { $pos = $bang ? (defined($count) ? $count : 0) : (0 + (defined($count) ? $count : 0)); next }         # Note: @!N is absolute, @N is relative to group (here group offset is 0 so same)         if ($ch eq 'x') {             if ($bang) {                 my $n = $nrep > 0 ? $nrep : 1;                 $pos += ($n - ($pos % $n)) % $n;             } else { $pos += $nrep }             next;         }         if ($ch eq 'X') {             if ($bang) {                 my $n = $nrep > 0 ? $nrep : 1;                 $pos = int($pos / $n) * $n;             } else { $pos -= $nrep; $pos = 0 if $pos < 0 }             next;         }         my ($nb) = _pack_type_info($ch, $bang);         if ($nb) { $pos += $nb * $nrep; next }         if ($ch eq 'A' || $ch eq 'a' || $ch eq 'Z') { $pos += $nrep; next }         if ($ch eq 'B' || $ch eq 'b') { $pos += int(($nrep+7)/8); next }         if ($ch eq 'H' || $ch eq 'h') { $pos += int(($nrep+1)/2); next }         if ($ch eq 'f' || $ch eq 'F') { $pos += 4*$nrep; next }         if ($ch eq 'd' || $ch eq 'D') { $pos += 8*$nrep; next }         if ($ch eq 'p' || $ch eq 'P') { $pos += 8*$nrep; next }         if ($ch eq 'W' || $ch eq 'U' || $ch eq 'w') { $pos += $nrep; next }         if ($ch eq '.') { next }  # position marker, no bytes         # u: variable, skip     }
                (p-while 1
                  (let (($ch (make-p-box nil)) ($grpbeg (make-p-box nil)) ($grpend (make-p-box nil)) ($bang (make-p-box nil)) ($all (make-p-box nil)) ($count (make-p-box nil)) ($nrep (make-p-box nil)) ($inner (make-p-box nil)) ($n (make-p-box nil)) ($nb (make-p-box nil)))
                    (let ((--pcl-if-ret--2 nil))
                      ;; $ti = _pack_skip_ws($tmpl, $ti)
                      (let ((*wantarray* :void))                       (p-my-= $ti (let ((*wantarray* nil)) (pl-_pack_skip_ws $tmpl $ti))))
                      
                      ;; last if $ti >= $tlen
                      (let ((*wantarray* :void)) (p-if (p->= $ti $tlen) (p-last)))
                      
                      ;; my $ch = substr($tmpl, $ti, 1)
                                            (p-my-= $ch (p-substr $tmpl $ti 1))
                      
                      ;; $ti++
                      (let ((*wantarray* :void))                       (p-post++ $ti))
                      
                      ;; my ($grpbeg, $grpend) = (undef, undef)
                                            (let ((*wantarray* nil)) (p-list-= (vector $grpbeg $grpend) (vector (let ((*wantarray* t)) (p-undef)) (let ((*wantarray* t)) (p-undef)))))
                      
                      ;; if ($ch eq '(') {             $grpend = _pack_find_group_end($tmpl, $ti);             $grpbeg = $ti; $ti = $grpend + 1;         }
                      ;; if ($ch eq '(')
                      (p-if (p-str-eq $ch "(")
                        (progn
                          ;; $grpend = _pack_find_group_end($tmpl, $ti)
                          (let ((*wantarray* :void))                           (p-my-= $grpend (let ((*wantarray* nil)) (pl-_pack_find_group_end $tmpl $ti))))
                          
                          ;; $grpbeg = $ti
                          (let ((*wantarray* :void))                           (p-my-= $grpbeg $ti))
                          
                          ;; $ti = $grpend + 1
                                                    (p-my-= $ti (p-+ $grpend 1))
                          
                        )
                        nil
                      )
                      
                      ;; my $bang = 0
                                            (p-my-= $bang 0)
                      
                      ;; while ($ti < $tlen && substr($tmpl, $ti, 1) =~ /[!<>]/) {             $bang = 1 if substr($tmpl, $ti, 1) eq '!';             $ti++;         }
                      (p-while (p-&& (p-< $ti $tlen) (let ((*wantarray* nil)) (p-=~ (p-substr $tmpl $ti 1) (p-regex "/[!<>]/"))))
                        ;; $bang = 1 if substr($tmpl, $ti, 1) eq '!'
                        (let ((*wantarray* :void)) (p-if (p-str-eq (p-substr $tmpl $ti 1) "!") (p-my-= $bang 1)))
                        
                        ;; $ti++
                                                (p-post++ $ti)
                        
                      )
                      
                      ;; my ($all, $count, $nrep) = _pack_parse_count($tmpl, \$ti)
                                            (let ((*wantarray* nil)) (p-list-= (vector $all $count $nrep) (let ((*wantarray* t)) (pl-_pack_parse_count $tmpl (p-backslash $ti)))))
                      
                      ;; $nrep = 1 unless defined $nrep && $nrep >= 1
                      (let ((*wantarray* :void)) (p-unless (p-&& (p-defined $nrep) (p->= $nrep 1)) (p-my-= $nrep 1)))
                      
                      ;; if (defined $grpbeg) {             my $inner = substr($tmpl, $grpbeg, $grpend - $grpbeg);             $pos += _pack_template_size($inner) * $nrep;             next;         }
                      ;; if (defined $grpbeg)
                      (p-if (p-defined $grpbeg)
                        (progn
                          (let (($inner (make-p-box nil)))
                            ;; my $inner = substr($tmpl, $grpbeg, $grpend - $grpbeg)
                                                        (p-my-= $inner (p-substr $tmpl $grpbeg (p-- $grpend $grpbeg)))
                            
                            ;; $pos += _pack_template_size($inner) * $nrep
                            (let ((*wantarray* :void))                             (p-incf $pos (p-* (let ((*wantarray* nil)) (pl-_pack_template_size $inner)) $nrep)))
                            
                            ;; next
                                                        (p-next)
                            
                          )
                        )
                        nil
                      )
                      
                      ;; if ($ch eq '@') { $pos = $bang ? (defined($count) ? $count : 0) : (0 + (defined($count) ? $count : 0)); next }
                      ;; if ($ch eq '@')
                      (p-if (p-str-eq $ch "@")
                        (progn
                          ;; $pos = $bang ? (defined($count) ? $count : 0) : (0 + (defined($count) ? $count : 0))
                          (let ((*wantarray* :void))                           (p-my-= $pos (p-if $bang (p-if (p-defined $count) $count 0) (p-+ 0 (p-if (p-defined $count) $count 0)))))
                          
                          ;; next
                                                    (p-next)
                          
                        )
                        nil
                      )
                      
                      ;; if ($ch eq 'x') {             if ($bang) {                 my $n = $nrep > 0 ? $nrep : 1;                 $pos += ($n - ($pos % $n)) % $n;             } else { $pos += $nrep }             next;         }
                      ;; if ($ch eq 'x')
                      (p-if (p-str-eq $ch "x")
                        (progn
                          (let (($n (make-p-box nil)))
                            ;; if ($bang) {                 my $n = $nrep > 0 ? $nrep : 1;                 $pos += ($n - ($pos % $n)) % $n;             } else { $pos += $nrep }
                            ;; if ($bang)
                            (p-if $bang
                              (progn
                                (let (($n (make-p-box nil)))
                                  ;; my $n = $nrep > 0 ? $nrep : 1
                                                                    (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                  
                                  ;; $pos += ($n - ($pos % $n)) % $n
                                                                    (p-incf $pos (p-% (p-- $n (p-% $pos $n)) $n))
                                  
                                )
                              )
                              ;; else
                              (progn
                                ;; $pos += $nrep
                                                                (p-incf $pos $nrep)
                                
                              )
                            )
                            
                            ;; next
                                                        (p-next)
                            
                          )
                        )
                        nil
                      )
                      
                      ;; if ($ch eq 'X') {             if ($bang) {                 my $n = $nrep > 0 ? $nrep : 1;                 $pos = int($pos / $n) * $n;             } else { $pos -= $nrep; $pos = 0 if $pos < 0 }             next;         }
                      ;; if ($ch eq 'X')
                      (p-if (p-str-eq $ch "X")
                        (progn
                          (let (($n (make-p-box nil)))
                            ;; if ($bang) {                 my $n = $nrep > 0 ? $nrep : 1;                 $pos = int($pos / $n) * $n;             } else { $pos -= $nrep; $pos = 0 if $pos < 0 }
                            ;; if ($bang)
                            (p-if $bang
                              (progn
                                (let (($n (make-p-box nil)))
                                  ;; my $n = $nrep > 0 ? $nrep : 1
                                                                    (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                  
                                  ;; $pos = int($pos / $n) * $n
                                                                    (p-my-= $pos (p-* (p-int (p-/ $pos $n)) $n))
                                  
                                )
                              )
                              ;; else
                              (progn
                                (let ((--pcl-if-ret--3 nil))
                                  ;; $pos -= $nrep
                                  (let ((*wantarray* :void))                                   (p-decf $pos $nrep))
                                  
                                  ;; $pos = 0 if $pos < 0
                                  (p-if (setf --pcl-if-ret--3 (p-< $pos 0))
                                    (setf --pcl-if-ret--3 (p-my-= $pos 0))
                                    nil)
                                  
                                --pcl-if-ret--3)
                              )
                            )
                            
                            ;; next
                                                        (p-next)
                            
                          )
                        )
                        nil
                      )
                      
                      ;; my ($nb) = _pack_type_info($ch, $bang)
                                            (let ((*wantarray* nil)) (p-list-= (vector $nb) (let ((*wantarray* t)) (pl-_pack_type_info $ch $bang))))
                      
                      ;; if ($nb) { $pos += $nb * $nrep; next }
                      ;; if ($nb)
                      (p-if $nb
                        (progn
                          ;; $pos += $nb * $nrep
                          (let ((*wantarray* :void))                           (p-incf $pos (p-* $nb $nrep)))
                          
                          ;; next
                                                    (p-next)
                          
                        )
                        nil
                      )
                      
                      ;; if ($ch eq 'A' || $ch eq 'a' || $ch eq 'Z') { $pos += $nrep; next }
                      ;; if ($ch eq 'A' || $ch eq 'a' || $ch eq 'Z')
                      (p-if (p-|| (p-|| (p-str-eq $ch "A") (p-str-eq $ch "a")) (p-str-eq $ch "Z"))
                        (progn
                          ;; $pos += $nrep
                          (let ((*wantarray* :void))                           (p-incf $pos $nrep))
                          
                          ;; next
                                                    (p-next)
                          
                        )
                        nil
                      )
                      
                      ;; if ($ch eq 'B' || $ch eq 'b') { $pos += int(($nrep+7)/8); next }
                      ;; if ($ch eq 'B' || $ch eq 'b')
                      (p-if (p-|| (p-str-eq $ch "B") (p-str-eq $ch "b"))
                        (progn
                          ;; $pos += int(($nrep+7)/8)
                          (let ((*wantarray* :void))                           (p-incf $pos (p-int (p-/ (p-+ $nrep 7) 8))))
                          
                          ;; next
                                                    (p-next)
                          
                        )
                        nil
                      )
                      
                      ;; if ($ch eq 'H' || $ch eq 'h') { $pos += int(($nrep+1)/2); next }
                      ;; if ($ch eq 'H' || $ch eq 'h')
                      (p-if (p-|| (p-str-eq $ch "H") (p-str-eq $ch "h"))
                        (progn
                          ;; $pos += int(($nrep+1)/2)
                          (let ((*wantarray* :void))                           (p-incf $pos (p-int (p-/ (p-+ $nrep 1) 2))))
                          
                          ;; next
                                                    (p-next)
                          
                        )
                        nil
                      )
                      
                      ;; if ($ch eq 'f' || $ch eq 'F') { $pos += 4*$nrep; next }
                      ;; if ($ch eq 'f' || $ch eq 'F')
                      (p-if (p-|| (p-str-eq $ch "f") (p-str-eq $ch "F"))
                        (progn
                          ;; $pos += 4*$nrep
                          (let ((*wantarray* :void))                           (p-incf $pos (p-* 4 $nrep)))
                          
                          ;; next
                                                    (p-next)
                          
                        )
                        nil
                      )
                      
                      ;; if ($ch eq 'd' || $ch eq 'D') { $pos += 8*$nrep; next }
                      ;; if ($ch eq 'd' || $ch eq 'D')
                      (p-if (p-|| (p-str-eq $ch "d") (p-str-eq $ch "D"))
                        (progn
                          ;; $pos += 8*$nrep
                          (let ((*wantarray* :void))                           (p-incf $pos (p-* 8 $nrep)))
                          
                          ;; next
                                                    (p-next)
                          
                        )
                        nil
                      )
                      
                      ;; if ($ch eq 'p' || $ch eq 'P') { $pos += 8*$nrep; next }
                      ;; if ($ch eq 'p' || $ch eq 'P')
                      (p-if (p-|| (p-str-eq $ch "p") (p-str-eq $ch "P"))
                        (progn
                          ;; $pos += 8*$nrep
                          (let ((*wantarray* :void))                           (p-incf $pos (p-* 8 $nrep)))
                          
                          ;; next
                                                    (p-next)
                          
                        )
                        nil
                      )
                      
                      ;; if ($ch eq 'W' || $ch eq 'U' || $ch eq 'w') { $pos += $nrep; next }
                      ;; if ($ch eq 'W' || $ch eq 'U' || $ch eq 'w')
                      (p-if (p-|| (p-|| (p-str-eq $ch "W") (p-str-eq $ch "U")) (p-str-eq $ch "w"))
                        (progn
                          ;; $pos += $nrep
                          (let ((*wantarray* :void))                           (p-incf $pos $nrep))
                          
                          ;; next
                                                    (p-next)
                          
                        )
                        nil
                      )
                      
                      ;; if ($ch eq '.') { next }
                      ;; if ($ch eq '.')
                      (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "."))
                        (progn
                          ;; next
                          (let ((*wantarray* :void))                           (p-next))
                          
                        )
                        nil
                      )
                      
                    --pcl-if-ret--2)
                  )
                )
                
                ;; return $pos
                                (p-return $pos)
                
              )
            )
          )
        )
      )
    )
  )
)

;; sub _pack_parse_count { ... }
(p-sub pl-_pack_parse_count (&rest %_args)
  (p-args-body
    (block nil
      (let (($start (make-p-box nil)) ($depth (make-p-box nil)) ($c (make-p-box nil)) ($inner (make-p-box nil)) ($n (make-p-box nil)))
        (let (($tmpl (make-p-box nil)) ($ti_ref (make-p-box nil)))
          ;; my ($tmpl, $ti_ref) = @_
                    (let ((*wantarray* nil)) (p-list-= (vector $tmpl $ti_ref) @_))
          
          (let (($tlen (make-p-box nil)))
            ;; my $tlen = length($tmpl)
                        (p-my-= $tlen (p-length $tmpl))
            
            ;; if ($$ti_ref < $tlen && substr($tmpl, $$ti_ref, 1) eq '*') {         $$ti_ref++; return (1, undef, 1);     }
            ;; if ($$ti_ref < $tlen && substr($tmpl, $$ti_ref, 1) eq '*')
            (p-if (p-&& (p-< (p-cast-$ $ti_ref) $tlen) (p-str-eq (p-substr $tmpl (p-cast-$ $ti_ref) 1) "*"))
              (progn
                ;; $$ti_ref++
                (let ((*wantarray* :void))                 (p-post++ (p-cast-$ $ti_ref)))
                
                ;; return (1, undef, 1)
                                (p-return 1 (p-undef) 1)
                
              )
              nil
            )
            
            ;; if ($$ti_ref < $tlen && substr($tmpl, $$ti_ref, 1) eq '[') {         $$ti_ref++;  # skip '['         my $start = $$ti_ref;         my $depth = 1;         while ($$ti_ref < $tlen && $depth > 0) {             my $c = substr($tmpl, $$ti_ref, 1); $$ti_ref++;             if ($c eq '[') { $depth++ }             elsif ($c eq ']') { $depth-- }         }         die "No group ending character ']' found in template\n" if $depth > 0;         # $$ti_ref is now just past the closing ']'         my $inner = substr($tmpl, $start, $$ti_ref - $start - 1);         if ($inner =~ /^\d+$/) {             my $n = $inner + 0;             return (0, $n, $n);         }         die "Within \[\]-length '\@' not allowed\n" if index($inner, '@') >= 0;         die "Malformed integer in \[\]\n" if $inner =~ /^\d/ && $inner !~ /^\d+$/;         my $n = _pack_template_size($inner);         return (0, $n, $n);     }
            ;; if ($$ti_ref < $tlen && substr($tmpl, $$ti_ref, 1) eq '[')
            (p-if (p-&& (p-< (p-cast-$ $ti_ref) $tlen) (p-str-eq (p-substr $tmpl (p-cast-$ $ti_ref) 1) "["))
              (progn
                (let (($start (make-p-box nil)) ($depth (make-p-box nil)) ($c (make-p-box nil)) ($inner (make-p-box nil)) ($n (make-p-box nil)))
                  ;; $$ti_ref++
                  (let ((*wantarray* :void))                   (p-post++ (p-cast-$ $ti_ref)))
                  
                  ;; my $start = $$ti_ref
                                    (p-my-= $start (p-cast-$ $ti_ref))
                  
                  ;; my $depth = 1
                                    (p-my-= $depth 1)
                  
                  ;; while ($$ti_ref < $tlen && $depth > 0) {             my $c = substr($tmpl, $$ti_ref, 1); $$ti_ref++;             if ($c eq '[') { $depth++ }             elsif ($c eq ']') { $depth-- }         }
                  (p-while (p-&& (p-< (p-cast-$ $ti_ref) $tlen) (p-> $depth 0))
                    (let (($c (make-p-box nil)))
                      (let ((--pcl-if-ret--4 nil))
                        ;; my $c = substr($tmpl, $$ti_ref, 1)
                                                (p-my-= $c (p-substr $tmpl (p-cast-$ $ti_ref) 1))
                        
                        ;; $$ti_ref++
                        (let ((*wantarray* :void))                         (p-post++ (p-cast-$ $ti_ref)))
                        
                        ;; if ($c eq '[') { $depth++ }             elsif ($c eq ']') { $depth-- }
                        ;; if ($c eq '[')
                        (p-if (setf --pcl-if-ret--4 (p-str-eq $c "["))
                          (progn
                            ;; $depth++
                            (setf --pcl-if-ret--4 (p-post++ $depth))
                            
                          )
                          ;; elsif ($c eq ']')
                          (p-if (setf --pcl-if-ret--4 (p-str-eq $c "]"))
                            (progn
                              ;; $depth--
                              (setf --pcl-if-ret--4 (p-post-- $depth))
                              
                            )
                            nil
                          )
                        )
                        
                      --pcl-if-ret--4)
                    )
                  )
                  
                  ;; die "No group ending character ']' found in template\n" if $depth > 0
                  (let ((*wantarray* :void)) (p-if (p-> $depth 0) (p-die :loc "- line 205" "No group ending character ']' found in template
")))
                  
                  ;; my $inner = substr($tmpl, $start, $$ti_ref - $start - 1)
                                    (p-my-= $inner (p-substr $tmpl $start (p-- (p-- (p-cast-$ $ti_ref) $start) 1)))
                  
                  ;; if ($inner =~ /^\d+$/) {             my $n = $inner + 0;             return (0, $n, $n);         }
                  ;; if ($inner =~ /^\d+$/)
                  (p-if (let ((*wantarray* nil)) (p-=~ $inner (p-regex "/^\\d+$/")))
                    (progn
                      (let (($n (make-p-box nil)))
                        ;; my $n = $inner + 0
                                                (p-my-= $n (p-+ $inner 0))
                        
                        ;; return (0, $n, $n)
                                                (p-return 0 $n $n)
                        
                      )
                    )
                    nil
                  )
                  
                  ;; die "Within \[\]-length '\@' not allowed\n" if index($inner, '@') >= 0
                  (let ((*wantarray* :void)) (p-if (p->= (p-index $inner "@") 0) (p-die :loc "- line 212" "Within []-length '@' not allowed
")))
                  
                  ;; die "Malformed integer in \[\]\n" if $inner =~ /^\d/ && $inner !~ /^\d+$/
                  (let ((*wantarray* :void)) (p-if (p-&& (let ((*wantarray* nil)) (p-=~ $inner (p-regex "/^\\d/"))) (let ((*wantarray* nil)) (p-!~ $inner (p-regex "/^\\d+$/")))) (p-die :loc "- line 213" "Malformed integer in []
")))
                  
                  ;; my $n = _pack_template_size($inner)
                                    (p-my-= $n (let ((*wantarray* nil)) (pl-_pack_template_size $inner)))
                  
                  ;; return (0, $n, $n)
                                    (p-return 0 $n $n)
                  
                )
              )
              nil
            )
            
            ;; if ($$ti_ref < $tlen && substr($tmpl, $$ti_ref, 1) =~ /\d/) {         my $n = 0;         while ($$ti_ref < $tlen && substr($tmpl, $$ti_ref, 1) =~ /\d/) {             $n = $n * 10 + substr($tmpl, $$ti_ref, 1); $$ti_ref++;         }         return (0, $n, $n);     }
            ;; if ($$ti_ref < $tlen && substr($tmpl, $$ti_ref, 1) =~ /\d/)
            (p-if (p-&& (p-< (p-cast-$ $ti_ref) $tlen) (let ((*wantarray* nil)) (p-=~ (p-substr $tmpl (p-cast-$ $ti_ref) 1) (p-regex "/\\d/"))))
              (progn
                (let (($n (make-p-box nil)))
                  ;; my $n = 0
                                    (p-my-= $n 0)
                  
                  ;; while ($$ti_ref < $tlen && substr($tmpl, $$ti_ref, 1) =~ /\d/) {             $n = $n * 10 + substr($tmpl, $$ti_ref, 1); $$ti_ref++;         }
                  (p-while (p-&& (p-< (p-cast-$ $ti_ref) $tlen) (let ((*wantarray* nil)) (p-=~ (p-substr $tmpl (p-cast-$ $ti_ref) 1) (p-regex "/\\d/"))))
                    ;; $n = $n * 10 + substr($tmpl, $$ti_ref, 1)
                    (let ((*wantarray* :void))                     (p-my-= $n (p-+ (p-* $n 10) (p-substr $tmpl (p-cast-$ $ti_ref) 1))))
                    
                    ;; $$ti_ref++
                                        (p-post++ (p-cast-$ $ti_ref))
                    
                  )
                  
                  ;; return (0, $n, $n)
                                    (p-return 0 $n $n)
                  
                )
              )
              nil
            )
            
            ;; return (0, 1, 1)
                        (p-return 0 1 1)
            
          )
        )
      )
    )
  )
)

;; sub _pack_emit_int { ... }
(p-sub pl-_pack_emit_int (&rest %_args)
  (p-args-body
    (block nil
      (let (($k (make-p-box nil)))
        (let (($val (make-p-box nil)) ($nbytes (make-p-box nil)) ($signed (make-p-box nil)) ($be (make-p-box nil)))
          ;; my ($val, $nbytes, $signed, $be) = @_
                    (let ((*wantarray* nil)) (p-list-= (vector $val $nbytes $signed $be) @_))
          
          ;; $val = int($val + 0)
          (let ((*wantarray* :void))           (p-my-= $val (p-int (p-+ $val 0))))
          
          (let (($result (make-p-box nil)))
            ;; my $result = ''
                        (p-my-= $result "")
            
            ;; if ($be) {         for (my $k = $nbytes - 1; $k >= 0; $k--) {             $result .= chr(($val >> (8 * $k)) & 0xFF);         }     } else {         for (my $k = 0; $k < $nbytes; $k++) {             $result .= chr(($val >> (8 * $k)) & 0xFF);         }     }
            ;; if ($be)
            (p-if $be
              (progn
                (let (($k (make-p-box nil)))
                  ;; for (my $k = $nbytes - 1; $k >= 0; $k--) {             $result .= chr(($val >> (8 * $k)) & 0xFF);         }
                  (let (($k (make-p-box nil)))
                    (p-for (                  (p-my-= $k (p-- $nbytes 1)))
                            (                  (p->= $k 0))
                            (                  (p-post-- $k))
                      ;; $result .= chr(($val >> (8 * $k)) & 0xFF)
                                            (p-.= $result (p-chr (p-bit-and (p->> $val (p-* 8 $k)) #xFF)))
                      
                    )
                  )
                  
                )
              )
              ;; else
              (progn
                (let (($k (make-p-box nil)))
                  ;; for (my $k = 0; $k < $nbytes; $k++) {             $result .= chr(($val >> (8 * $k)) & 0xFF);         }
                  (let (($k (make-p-box nil)))
                    (p-for (                  (p-my-= $k 0))
                            (                  (p-< $k $nbytes))
                            (                  (p-post++ $k))
                      ;; $result .= chr(($val >> (8 * $k)) & 0xFF)
                                            (p-.= $result (p-chr (p-bit-and (p->> $val (p-* 8 $k)) #xFF)))
                      
                    )
                  )
                  
                )
              )
            )
            
            ;; return $result
                        (p-return $result)
            
          )
        )
      )
    )
  )
)

;; sub _unpack_read_int { ... }
(p-sub pl-_unpack_read_int (&rest %_args)
  (p-args-body
    (block nil
      (let (($k (make-p-box nil)) ($max (make-p-box nil)))
        (let (($s (make-p-box nil)) ($si (make-p-box nil)) ($nbytes (make-p-box nil)) ($be (make-p-box nil)) ($signed (make-p-box nil)))
          ;; my ($s, $si, $nbytes, $be, $signed) = @_
                    (let ((*wantarray* nil)) (p-list-= (vector $s $si $nbytes $be $signed) @_))
          
          (let (($slen (make-p-box nil)))
            ;; my $slen = length($s)
                        (p-my-= $slen (p-length $s))
            
            (let (($v (make-p-box nil)))
              ;; my $v = 0
                            (p-my-= $v 0)
              
              ;; if ($be) {         for (my $k = 0; $k < $nbytes; $k++) {             $v = ($v << 8) | (($si+$k < $slen) ? ord(substr($s,$si+$k,1)) : 0);         }     } else {         for (my $k = $nbytes - 1; $k >= 0; $k--) {             $v = ($v << 8) | (($si+$k < $slen) ? ord(substr($s,$si+$k,1)) : 0);         }     }
              ;; if ($be)
              (p-if $be
                (progn
                  (let (($k (make-p-box nil)))
                    ;; for (my $k = 0; $k < $nbytes; $k++) {             $v = ($v << 8) | (($si+$k < $slen) ? ord(substr($s,$si+$k,1)) : 0);         }
                    (let (($k (make-p-box nil)))
                      (p-for (                    (p-my-= $k 0))
                              (                    (p-< $k $nbytes))
                              (                    (p-post++ $k))
                        ;; $v = ($v << 8) | (($si+$k < $slen) ? ord(substr($s,$si+$k,1)) : 0)
                                                (p-my-= $v (p-bit-or (p-<< $v 8) (p-if (p-< (p-+ $si $k) $slen) (p-ord (p-substr $s (p-+ $si $k) 1)) 0)))
                        
                      )
                    )
                    
                  )
                )
                ;; else
                (progn
                  (let (($k (make-p-box nil)))
                    ;; for (my $k = $nbytes - 1; $k >= 0; $k--) {             $v = ($v << 8) | (($si+$k < $slen) ? ord(substr($s,$si+$k,1)) : 0);         }
                    (let (($k (make-p-box nil)))
                      (p-for (                    (p-my-= $k (p-- $nbytes 1)))
                              (                    (p->= $k 0))
                              (                    (p-post-- $k))
                        ;; $v = ($v << 8) | (($si+$k < $slen) ? ord(substr($s,$si+$k,1)) : 0)
                                                (p-my-= $v (p-bit-or (p-<< $v 8) (p-if (p-< (p-+ $si $k) $slen) (p-ord (p-substr $s (p-+ $si $k) 1)) 0)))
                        
                      )
                    )
                    
                  )
                )
              )
              
              ;; if ($signed) {         # In Perl, 2**64 is a float and loses precision, but pack-impl.pl is transpiled to CL         # where (expt 2 64) is exact. The general formula works for all sizes in CL.         my $max = 2 ** ($nbytes * 8);         $v -= $max if $v >= $max / 2;     }
              ;; if ($signed)
              (p-if $signed
                (progn
                  (let (($max (make-p-box nil)))
                    (let ((--pcl-if-ret--5 nil))
                      ;; my $max = 2 ** ($nbytes * 8)
                                            (p-my-= $max (p-** 2 (p-* $nbytes 8)))
                      
                      ;; $v -= $max if $v >= $max / 2
                      (p-if (setf --pcl-if-ret--5 (p->= $v (p-/ $max 2)))
                        (setf --pcl-if-ret--5 (p-decf $v $max))
                        nil)
                      
                    --pcl-if-ret--5)
                  )
                )
                nil
              )
              
              ;; return $v
                            (p-return $v)
              
            )
          )
        )
      )
    )
  )
)

;; sub _pack_float32   { ... }
(p-sub pl-_pack_float32 (&rest %_args)
  (p-args-body
    (block nil
      (let (($val (make-p-box nil)) ($be (make-p-box nil)))
        ;; my ($val, $be) = @_
                (let ((*wantarray* nil)) (p-list-= (vector $val $be) @_))
        
        ;; return ""
                (p-return "")
        
      )
    )
  )
)

;; sub _pack_float64   { ... }
(p-sub pl-_pack_float64 (&rest %_args)
  (p-args-body
    (block nil
      (let (($val (make-p-box nil)) ($be (make-p-box nil)))
        ;; my ($val, $be) = @_
                (let ((*wantarray* nil)) (p-list-= (vector $val $be) @_))
        
        ;; return ""
                (p-return "")
        
      )
    )
  )
)

;; sub _unpack_float32 { ... }
(p-sub pl-_unpack_float32 (&rest %_args)
  (p-args-body
    (block nil
      (let (($s (make-p-box nil)) ($si (make-p-box nil)) ($be (make-p-box nil)))
        ;; my ($s, $si, $be) = @_
                (let ((*wantarray* nil)) (p-list-= (vector $s $si $be) @_))
        
        ;; return 0.0
                (p-return 0.0)
        
      )
    )
  )
)

;; sub _unpack_float64 { ... }
(p-sub pl-_unpack_float64 (&rest %_args)
  (p-args-body
    (block nil
      (let (($s (make-p-box nil)) ($si (make-p-box nil)) ($be (make-p-box nil)))
        ;; my ($s, $si, $be) = @_
                (let ((*wantarray* nil)) (p-list-= (vector $s $si $be) @_))
        
        ;; return 0.0
                (p-return 0.0)
        
      )
    )
  )
)

;; sub _pack_str_one { ... }
(p-sub pl-_pack_str_one (&rest %_args)
  (p-args-body
    (block nil
      (let (($len (make-p-box nil)) ($k (make-p-box nil)) ($body (make-p-box nil)) ($nbits (make-p-box nil)) ($bs (make-p-box nil)) ($byte (make-p-box nil)) ($bit (make-p-box nil)) ($idx (make-p-box nil)) ($nyb (make-p-box nil)) ($hi (make-p-box nil)) ($lo (make-p-box nil)) ($line_len (make-p-box nil)) ($cs (make-p-box nil)) ($ce (make-p-box nil)) ($chunk (make-p-box nil)) ($clen (make-p-box nil)) ($b0 (make-p-box nil)) ($b1 (make-p-box nil)) ($b2 (make-p-box nil)) ($cm (make-p-box nil)) ($uu (make-p-box nil)))
        (let ((--pcl-if-ret--6 nil))
          (let (($ch (make-p-box nil)) ($arg (make-p-box nil)) ($nrep (make-p-box nil)) ($star (make-p-box nil)) ($result_ref (make-p-box nil)))
            ;; my ($ch, $arg, $nrep, $star, $result_ref) = @_
                        (let ((*wantarray* nil)) (p-list-= (vector $ch $arg $nrep $star $result_ref) @_))
            
            ;; $arg = '' unless defined $arg
            (let ((*wantarray* :void)) (p-unless (p-defined $arg) (p-my-= $arg "")))
            
            (let (($slen (make-p-box nil)))
              ;; my $slen = length($arg)
                            (p-my-= $slen (p-length $arg))
              
              ;; if ($ch eq 'a') {         my $len = $star ? $slen : $nrep;         for (my $k = 0; $k < $len; $k++) {             $$result_ref .= $k < $slen ? substr($arg,$k,1) : chr(0);         }     } elsif ($ch eq 'A') {         my $len = $star ? $slen : $nrep;         for (my $k = 0; $k < $len; $k++) {             $$result_ref .= $k < $slen ? substr($arg,$k,1) : ' ';         }     } elsif ($ch eq 'Z') {         my $len = $star ? $slen + 1 : $nrep;         if ($len > 0) {             my $body = $len - 1;             for (my $k = 0; $k < $body; $k++) {                 $$result_ref .= $k < $slen ? substr($arg,$k,1) : chr(0);             }             $$result_ref .= chr(0);         }     } elsif ($ch eq 'b') {         my $nbits = $star ? $slen : $nrep;         for (my $bs = 0; $bs < $nbits; $bs += 8) {             my $byte = 0;             for (my $bit = 0; $bit < 8 && $bs+$bit < $nbits; $bit++) {                 my $idx = $bs + $bit;                 $byte |= (1 << $bit) if $idx < $slen && substr($arg,$idx,1) eq '1';             }             $$result_ref .= chr($byte);         }     } elsif ($ch eq 'B') {         my $nbits = $star ? $slen : $nrep;         for (my $bs = 0; $bs < $nbits; $bs += 8) {             my $byte = 0;             for (my $bit = 0; $bit < 8 && $bs+$bit < $nbits; $bit++) {                 my $idx = $bs + $bit;                 $byte |= (1 << (7 - $bit)) if $idx < $slen && substr($arg,$idx,1) eq '1';             }             $$result_ref .= chr($byte);         }     } elsif ($ch eq 'H') {         my $nyb = $star ? $slen : $nrep;  # count is number of nybbles         for (my $k = 0; $k < $nyb; $k += 2) {             my $hi = $k   < $slen ? hex(substr($arg,$k,  1)) : 0;             my $lo = $k+1 < $slen ? hex(substr($arg,$k+1,1)) : 0;             $$result_ref .= chr(($hi << 4) | $lo);         }     } elsif ($ch eq 'h') {         my $nyb = $star ? $slen : $nrep;  # count is number of nybbles         for (my $k = 0; $k < $nyb; $k += 2) {             my $lo = $k   < $slen ? hex(substr($arg,$k,  1)) : 0;             my $hi = $k+1 < $slen ? hex(substr($arg,$k+1,1)) : 0;             $$result_ref .= chr(($hi << 4) | $lo);         }     } elsif ($ch eq 'u') {         my $line_len = 45;         if (!$star && $nrep > 45) {             if ($nrep > 63) {                 warn "Field too wide in 'u' format in pack";                 $line_len = 63;             } else {                 $line_len = $nrep;             }         }         for (my $cs = 0; $cs < $slen; $cs += $line_len) {             my $ce = $cs + $line_len < $slen ? $cs + $line_len : $slen;             my $chunk = substr($arg, $cs, $ce - $cs);             my $clen = length($chunk);             $$result_ref .= chr(32 + $clen);             for (my $k = 0; $k < $clen; $k += 3) {                 my $b0 = ord(substr($chunk,$k,1));                 my $b1 = $k+1 < $clen ? ord(substr($chunk,$k+1,1)) : 0;                 my $b2 = $k+2 < $clen ? ord(substr($chunk,$k+2,1)) : 0;                 my $cm = ($b0 << 16) | ($b1 << 8) | $b2;                 my $uu = sub { my $c = 32 + ($_[0] & 63); $c == 32 ? 96 : $c };                 $$result_ref .= chr($uu->(($cm>>18)&63)) . chr($uu->(($cm>>12)&63))                               . chr($uu->(($cm>> 6)&63)) . chr($uu->( $cm    &63));             }             $$result_ref .= "\n";         }     }
              ;; if ($ch eq 'a')
              (p-if (setf --pcl-if-ret--6 (p-str-eq $ch "a"))
                (progn
                  (let (($len (make-p-box nil)) ($k (make-p-box nil)))
                    ;; my $len = $star ? $slen : $nrep
                                        (p-my-= $len (p-if $star $slen $nrep))
                    
                    ;; for (my $k = 0; $k < $len; $k++) {             $$result_ref .= $k < $slen ? substr($arg,$k,1) : chr(0);         }
                    (let (($k (make-p-box nil)))
                      (p-for (                    (p-my-= $k 0))
                              (                    (p-< $k $len))
                              (                    (p-post++ $k))
                        ;; $$result_ref .= $k < $slen ? substr($arg,$k,1) : chr(0)
                                                (p-.= (p-cast-$ $result_ref) (p-if (p-< $k $slen) (p-substr $arg $k 1) (p-chr 0)))
                        
                      )
                    )
                    
                  )
                )
                ;; elsif ($ch eq 'A')
                (p-if (setf --pcl-if-ret--6 (p-str-eq $ch "A"))
                  (progn
                    (let (($len (make-p-box nil)) ($k (make-p-box nil)))
                      ;; my $len = $star ? $slen : $nrep
                                            (p-my-= $len (p-if $star $slen $nrep))
                      
                      ;; for (my $k = 0; $k < $len; $k++) {             $$result_ref .= $k < $slen ? substr($arg,$k,1) : ' ';         }
                      (let (($k (make-p-box nil)))
                        (p-for (                      (p-my-= $k 0))
                                (                      (p-< $k $len))
                                (                      (p-post++ $k))
                          ;; $$result_ref .= $k < $slen ? substr($arg,$k,1) : ' '
                                                    (p-.= (p-cast-$ $result_ref) (p-if (p-< $k $slen) (p-substr $arg $k 1) " "))
                          
                        )
                      )
                      
                    )
                  )
                  ;; elsif ($ch eq 'Z')
                  (p-if (setf --pcl-if-ret--6 (p-str-eq $ch "Z"))
                    (progn
                      (let (($len (make-p-box nil)) ($body (make-p-box nil)) ($k (make-p-box nil)))
                        ;; my $len = $star ? $slen + 1 : $nrep
                                                (p-my-= $len (p-if $star (p-+ $slen 1) $nrep))
                        
                        ;; if ($len > 0) {             my $body = $len - 1;             for (my $k = 0; $k < $body; $k++) {                 $$result_ref .= $k < $slen ? substr($arg,$k,1) : chr(0);             }             $$result_ref .= chr(0);         }
                        ;; if ($len > 0)
                        (p-if (setf --pcl-if-ret--6 (p-> $len 0))
                          (progn
                            (let (($body (make-p-box nil)) ($k (make-p-box nil)))
                              ;; my $body = $len - 1
                                                            (p-my-= $body (p-- $len 1))
                              
                              ;; for (my $k = 0; $k < $body; $k++) {                 $$result_ref .= $k < $slen ? substr($arg,$k,1) : chr(0);             }
                              (let (($k (make-p-box nil)))
                                (p-for (                              (p-my-= $k 0))
                                        (                              (p-< $k $body))
                                        (                              (p-post++ $k))
                                  ;; $$result_ref .= $k < $slen ? substr($arg,$k,1) : chr(0)
                                                                    (p-.= (p-cast-$ $result_ref) (p-if (p-< $k $slen) (p-substr $arg $k 1) (p-chr 0)))
                                  
                                )
                              )
                              
                              ;; $$result_ref .= chr(0)
                              (setf --pcl-if-ret--6 (p-.= (p-cast-$ $result_ref) (p-chr 0)))
                              
                            )
                          )
                          nil
                        )
                        
                      )
                    )
                    ;; elsif ($ch eq 'b')
                    (p-if (setf --pcl-if-ret--6 (p-str-eq $ch "b"))
                      (progn
                        (let (($nbits (make-p-box nil)) ($bs (make-p-box nil)) ($byte (make-p-box nil)) ($bit (make-p-box nil)) ($idx (make-p-box nil)))
                          ;; my $nbits = $star ? $slen : $nrep
                                                    (p-my-= $nbits (p-if $star $slen $nrep))
                          
                          ;; for (my $bs = 0; $bs < $nbits; $bs += 8) {             my $byte = 0;             for (my $bit = 0; $bit < 8 && $bs+$bit < $nbits; $bit++) {                 my $idx = $bs + $bit;                 $byte |= (1 << $bit) if $idx < $slen && substr($arg,$idx,1) eq '1';             }             $$result_ref .= chr($byte);         }
                          (let (($bs (make-p-box nil)))
                            (p-for (                          (p-my-= $bs 0))
                                    (                          (p-< $bs $nbits))
                                    (                          (p-incf $bs 8))
                              (let (($byte (make-p-box nil)) ($bit (make-p-box nil)) ($idx (make-p-box nil)))
                                ;; my $byte = 0
                                                                (p-my-= $byte 0)
                                
                                ;; for (my $bit = 0; $bit < 8 && $bs+$bit < $nbits; $bit++) {                 my $idx = $bs + $bit;                 $byte |= (1 << $bit) if $idx < $slen && substr($arg,$idx,1) eq '1';             }
                                (let (($bit (make-p-box nil)))
                                  (p-for (                                (p-my-= $bit 0))
                                          (                                (p-&& (p-< $bit 8) (p-< (p-+ $bs $bit) $nbits)))
                                          (                                (p-post++ $bit))
                                    (let (($idx (make-p-box nil)))
                                      (let ((--pcl-if-ret--7 nil))
                                        ;; my $idx = $bs + $bit
                                                                                (p-my-= $idx (p-+ $bs $bit))
                                        
                                        ;; $byte |= (1 << $bit) if $idx < $slen && substr($arg,$idx,1) eq '1'
                                        (p-if (setf --pcl-if-ret--7 (p-&& (p-< $idx $slen) (p-str-eq (p-substr $arg $idx 1) "1")))
                                          (setf --pcl-if-ret--7 (p-bit-or= $byte (p-<< 1 $bit)))
                                          nil)
                                        
                                      --pcl-if-ret--7)
                                    )
                                  )
                                )
                                
                                ;; $$result_ref .= chr($byte)
                                                                (p-.= (p-cast-$ $result_ref) (p-chr $byte))
                                
                              )
                            )
                          )
                          
                        )
                      )
                      ;; elsif ($ch eq 'B')
                      (p-if (setf --pcl-if-ret--6 (p-str-eq $ch "B"))
                        (progn
                          (let (($nbits (make-p-box nil)) ($bs (make-p-box nil)) ($byte (make-p-box nil)) ($bit (make-p-box nil)) ($idx (make-p-box nil)))
                            ;; my $nbits = $star ? $slen : $nrep
                                                        (p-my-= $nbits (p-if $star $slen $nrep))
                            
                            ;; for (my $bs = 0; $bs < $nbits; $bs += 8) {             my $byte = 0;             for (my $bit = 0; $bit < 8 && $bs+$bit < $nbits; $bit++) {                 my $idx = $bs + $bit;                 $byte |= (1 << (7 - $bit)) if $idx < $slen && substr($arg,$idx,1) eq '1';             }             $$result_ref .= chr($byte);         }
                            (let (($bs (make-p-box nil)))
                              (p-for (                            (p-my-= $bs 0))
                                      (                            (p-< $bs $nbits))
                                      (                            (p-incf $bs 8))
                                (let (($byte (make-p-box nil)) ($bit (make-p-box nil)) ($idx (make-p-box nil)))
                                  ;; my $byte = 0
                                                                    (p-my-= $byte 0)
                                  
                                  ;; for (my $bit = 0; $bit < 8 && $bs+$bit < $nbits; $bit++) {                 my $idx = $bs + $bit;                 $byte |= (1 << (7 - $bit)) if $idx < $slen && substr($arg,$idx,1) eq '1';             }
                                  (let (($bit (make-p-box nil)))
                                    (p-for (                                  (p-my-= $bit 0))
                                            (                                  (p-&& (p-< $bit 8) (p-< (p-+ $bs $bit) $nbits)))
                                            (                                  (p-post++ $bit))
                                      (let (($idx (make-p-box nil)))
                                        (let ((--pcl-if-ret--8 nil))
                                          ;; my $idx = $bs + $bit
                                                                                    (p-my-= $idx (p-+ $bs $bit))
                                          
                                          ;; $byte |= (1 << (7 - $bit)) if $idx < $slen && substr($arg,$idx,1) eq '1'
                                          (p-if (setf --pcl-if-ret--8 (p-&& (p-< $idx $slen) (p-str-eq (p-substr $arg $idx 1) "1")))
                                            (setf --pcl-if-ret--8 (p-bit-or= $byte (p-<< 1 (p-- 7 $bit))))
                                            nil)
                                          
                                        --pcl-if-ret--8)
                                      )
                                    )
                                  )
                                  
                                  ;; $$result_ref .= chr($byte)
                                                                    (p-.= (p-cast-$ $result_ref) (p-chr $byte))
                                  
                                )
                              )
                            )
                            
                          )
                        )
                        ;; elsif ($ch eq 'H')
                        (p-if (setf --pcl-if-ret--6 (p-str-eq $ch "H"))
                          (progn
                            (let (($nyb (make-p-box nil)) ($k (make-p-box nil)) ($hi (make-p-box nil)) ($lo (make-p-box nil)))
                              ;; my $nyb = $star ? $slen : $nrep
                                                            (p-my-= $nyb (p-if $star $slen $nrep))
                              
                              ;; for (my $k = 0; $k < $nyb; $k += 2) {             my $hi = $k   < $slen ? hex(substr($arg,$k,  1)) : 0;             my $lo = $k+1 < $slen ? hex(substr($arg,$k+1,1)) : 0;             $$result_ref .= chr(($hi << 4) | $lo);         }
                              (let (($k (make-p-box nil)))
                                (p-for (                              (p-my-= $k 0))
                                        (                              (p-< $k $nyb))
                                        (                              (p-incf $k 2))
                                  (let (($hi (make-p-box nil)) ($lo (make-p-box nil)))
                                    ;; my $hi = $k   < $slen ? hex(substr($arg,$k,  1)) : 0
                                                                        (p-my-= $hi (p-if (p-< $k $slen) (p-hex (p-substr $arg $k 1)) 0))
                                    
                                    ;; my $lo = $k+1 < $slen ? hex(substr($arg,$k+1,1)) : 0
                                                                        (p-my-= $lo (p-if (p-< (p-+ $k 1) $slen) (p-hex (p-substr $arg (p-+ $k 1) 1)) 0))
                                    
                                    ;; $$result_ref .= chr(($hi << 4) | $lo)
                                                                        (p-.= (p-cast-$ $result_ref) (p-chr (p-bit-or (p-<< $hi 4) $lo)))
                                    
                                  )
                                )
                              )
                              
                            )
                          )
                          ;; elsif ($ch eq 'h')
                          (p-if (setf --pcl-if-ret--6 (p-str-eq $ch "h"))
                            (progn
                              (let (($nyb (make-p-box nil)) ($k (make-p-box nil)) ($lo (make-p-box nil)) ($hi (make-p-box nil)))
                                ;; my $nyb = $star ? $slen : $nrep
                                                                (p-my-= $nyb (p-if $star $slen $nrep))
                                
                                ;; for (my $k = 0; $k < $nyb; $k += 2) {             my $lo = $k   < $slen ? hex(substr($arg,$k,  1)) : 0;             my $hi = $k+1 < $slen ? hex(substr($arg,$k+1,1)) : 0;             $$result_ref .= chr(($hi << 4) | $lo);         }
                                (let (($k (make-p-box nil)))
                                  (p-for (                                (p-my-= $k 0))
                                          (                                (p-< $k $nyb))
                                          (                                (p-incf $k 2))
                                    (let (($lo (make-p-box nil)) ($hi (make-p-box nil)))
                                      ;; my $lo = $k   < $slen ? hex(substr($arg,$k,  1)) : 0
                                                                            (p-my-= $lo (p-if (p-< $k $slen) (p-hex (p-substr $arg $k 1)) 0))
                                      
                                      ;; my $hi = $k+1 < $slen ? hex(substr($arg,$k+1,1)) : 0
                                                                            (p-my-= $hi (p-if (p-< (p-+ $k 1) $slen) (p-hex (p-substr $arg (p-+ $k 1) 1)) 0))
                                      
                                      ;; $$result_ref .= chr(($hi << 4) | $lo)
                                                                            (p-.= (p-cast-$ $result_ref) (p-chr (p-bit-or (p-<< $hi 4) $lo)))
                                      
                                    )
                                  )
                                )
                                
                              )
                            )
                            ;; elsif ($ch eq 'u')
                            (p-if (setf --pcl-if-ret--6 (p-str-eq $ch "u"))
                              (progn
                                (let (($line_len (make-p-box nil)) ($cs (make-p-box nil)) ($ce (make-p-box nil)) ($chunk (make-p-box nil)) ($clen (make-p-box nil)) ($k (make-p-box nil)) ($b0 (make-p-box nil)) ($b1 (make-p-box nil)) ($b2 (make-p-box nil)) ($cm (make-p-box nil)) ($uu (make-p-box nil)))
                                  ;; my $line_len = 45
                                                                    (p-my-= $line_len 45)
                                  
                                  ;; if (!$star && $nrep > 45) {             if ($nrep > 63) {                 warn "Field too wide in 'u' format in pack";                 $line_len = 63;             } else {                 $line_len = $nrep;             }         }
                                  ;; if (!$star && $nrep > 45)
                                  (p-if (p-&& (p-! $star) (p-> $nrep 45))
                                    (progn
                                      ;; if ($nrep > 63) {                 warn "Field too wide in 'u' format in pack";                 $line_len = 63;             } else {                 $line_len = $nrep;             }
                                      ;; if ($nrep > 63)
                                      (p-if (p-> $nrep 63)
                                        (progn
                                          ;; warn "Field too wide in 'u' format in pack"
                                                                                    (p-warn :loc "- line 335" "Field too wide in 'u' format in pack")
                                          
                                          ;; $line_len = 63
                                                                                    (p-my-= $line_len 63)
                                          
                                        )
                                        ;; else
                                        (progn
                                          ;; $line_len = $nrep
                                                                                    (p-my-= $line_len $nrep)
                                          
                                        )
                                      )
                                      
                                    )
                                    nil
                                  )
                                  
                                  ;; for (my $cs = 0; $cs < $slen; $cs += $line_len) {             my $ce = $cs + $line_len < $slen ? $cs + $line_len : $slen;             my $chunk = substr($arg, $cs, $ce - $cs);             my $clen = length($chunk);             $$result_ref .= chr(32 + $clen);             for (my $k = 0; $k < $clen; $k += 3) {                 my $b0 = ord(substr($chunk,$k,1));                 my $b1 = $k+1 < $clen ? ord(substr($chunk,$k+1,1)) : 0;                 my $b2 = $k+2 < $clen ? ord(substr($chunk,$k+2,1)) : 0;                 my $cm = ($b0 << 16) | ($b1 << 8) | $b2;                 my $uu = sub { my $c = 32 + ($_[0] & 63); $c == 32 ? 96 : $c };                 $$result_ref .= chr($uu->(($cm>>18)&63)) . chr($uu->(($cm>>12)&63))                               . chr($uu->(($cm>> 6)&63)) . chr($uu->( $cm    &63));             }             $$result_ref .= "\n";         }
                                  (let (($cs (make-p-box nil)))
                                    (p-for (                                  (p-my-= $cs 0))
                                            (                                  (p-< $cs $slen))
                                            (                                  (p-incf $cs $line_len))
                                      (let (($ce (make-p-box nil)) ($chunk (make-p-box nil)) ($clen (make-p-box nil)) ($k (make-p-box nil)) ($b0 (make-p-box nil)) ($b1 (make-p-box nil)) ($b2 (make-p-box nil)) ($cm (make-p-box nil)) ($uu (make-p-box nil)))
                                        ;; my $ce = $cs + $line_len < $slen ? $cs + $line_len : $slen
                                                                                (p-my-= $ce (p-if (p-< (p-+ $cs $line_len) $slen) (p-+ $cs $line_len) $slen))
                                        
                                        ;; my $chunk = substr($arg, $cs, $ce - $cs)
                                                                                (p-my-= $chunk (p-substr $arg $cs (p-- $ce $cs)))
                                        
                                        ;; my $clen = length($chunk)
                                                                                (p-my-= $clen (p-length $chunk))
                                        
                                        ;; $$result_ref .= chr(32 + $clen)
                                        (let ((*wantarray* :void))                                         (p-.= (p-cast-$ $result_ref) (p-chr (p-+ 32 $clen))))
                                        
                                        ;; for (my $k = 0; $k < $clen; $k += 3) {                 my $b0 = ord(substr($chunk,$k,1));                 my $b1 = $k+1 < $clen ? ord(substr($chunk,$k+1,1)) : 0;                 my $b2 = $k+2 < $clen ? ord(substr($chunk,$k+2,1)) : 0;                 my $cm = ($b0 << 16) | ($b1 << 8) | $b2;                 my $uu = sub { my $c = 32 + ($_[0] & 63); $c == 32 ? 96 : $c };                 $$result_ref .= chr($uu->(($cm>>18)&63)) . chr($uu->(($cm>>12)&63))                               . chr($uu->(($cm>> 6)&63)) . chr($uu->( $cm    &63));             }
                                        (let (($k (make-p-box nil)))
                                          (p-for (                                        (p-my-= $k 0))
                                                  (                                        (p-< $k $clen))
                                                  (                                        (p-incf $k 3))
                                            (let (($b0 (make-p-box nil)) ($b1 (make-p-box nil)) ($b2 (make-p-box nil)) ($cm (make-p-box nil)) ($uu (make-p-box nil)))
                                              ;; my $b0 = ord(substr($chunk,$k,1))
                                                                                            (p-my-= $b0 (p-ord (p-substr $chunk $k 1)))
                                              
                                              ;; my $b1 = $k+1 < $clen ? ord(substr($chunk,$k+1,1)) : 0
                                                                                            (p-my-= $b1 (p-if (p-< (p-+ $k 1) $clen) (p-ord (p-substr $chunk (p-+ $k 1) 1)) 0))
                                              
                                              ;; my $b2 = $k+2 < $clen ? ord(substr($chunk,$k+2,1)) : 0
                                                                                            (p-my-= $b2 (p-if (p-< (p-+ $k 2) $clen) (p-ord (p-substr $chunk (p-+ $k 2) 1)) 0))
                                              
                                              ;; my $cm = ($b0 << 16) | ($b1 << 8) | $b2
                                                                                            (p-my-= $cm (p-bit-or (p-bit-or (p-<< $b0 16) (p-<< $b1 8)) $b2))
                                              
                                              ;; my $uu = sub { my $c = 32 + ($_[0] & 63); $c == 32 ? 96 : $c }
                                                                                            (p-my-= $uu (lambda (&rest %_args)
  (let ((@_ (p-flatten-args %_args))
        (*pcl-caller-wantarray* *wantarray*))
    (catch :p-return
      (block nil
        (let (($c (make-p-box nil)))
          ;; my $c = 32 + ($_[0] & 63)
                    (p-my-= $c (p-+ 32 (p-bit-and (p-aref @_ 0) 63)))
          
          ;; $c == 32 ? 96 : $c
                    (p-if (p-== $c 32) 96 $c)
          
        )
      )
    )
  )
))
                                              
                                              ;; $$result_ref .= chr($uu->(($cm>>18)&63)) . chr($uu->(($cm>>12)&63))                               . chr($uu->(($cm>> 6)&63)) . chr($uu->( $cm    &63))
                                                                                            (p-.= (p-cast-$ $result_ref) (p-. (p-. (p-. (p-chr (let ((*wantarray* nil)) (p-funcall-ref $uu (p-bit-and (p->> $cm 18) 63)))) (p-chr (let ((*wantarray* nil)) (p-funcall-ref $uu (p-bit-and (p->> $cm 12) 63))))) (p-chr (let ((*wantarray* nil)) (p-funcall-ref $uu (p-bit-and (p->> $cm 6) 63))))) (p-chr (let ((*wantarray* nil)) (p-funcall-ref $uu (p-bit-and $cm 63))))))
                                              
                                            )
                                          )
                                        )
                                        
                                        ;; $$result_ref .= "\n"
                                                                                (p-.= (p-cast-$ $result_ref) "
")
                                        
                                      )
                                    )
                                  )
                                  
                                )
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
        --pcl-if-ret--6)
      )
    )
  )
)

;; sub _pack_utf8_char { ... }
(p-sub pl-_pack_utf8_char (&rest %_args)
  (p-args-body
    (block nil
      (let (($code (make-p-box nil)) ($r (make-p-box nil)))
        ;; my ($code, $r) = @_
                (let ((*wantarray* nil)) (p-list-= (vector $code $r) @_))
        
        ;; if    ($code < 0x80)    { $$r .= chr($code) }     elsif ($code < 0x800)   { $$r .= chr(0xC0|($code>>6)) . chr(0x80|($code&0x3F)) }     elsif ($code < 0x10000) { $$r .= chr(0xE0|($code>>12)) . chr(0x80|(($code>>6)&0x3F)) . chr(0x80|($code&0x3F)) }     else  { $$r .= chr(0xF0|($code>>18)) . chr(0x80|(($code>>12)&0x3F)) . chr(0x80|(($code>>6)&0x3F)) . chr(0x80|($code&0x3F)) }
        ;; if ($code < 0x80)
        (p-if (p-< $code #x80)
          (progn
            ;; $$r .= chr($code)
                        (p-.= (p-cast-$ $r) (p-chr $code))
            
          )
          ;; elsif ($code < 0x800)
          (p-if (p-< $code #x800)
            (progn
              ;; $$r .= chr(0xC0|($code>>6)) . chr(0x80|($code&0x3F))
                            (p-.= (p-cast-$ $r) (p-. (p-chr (p-bit-or #xC0 (p->> $code 6))) (p-chr (p-bit-or #x80 (p-bit-and $code #x3F)))))
              
            )
            ;; elsif ($code < 0x10000)
            (p-if (p-< $code #x10000)
              (progn
                ;; $$r .= chr(0xE0|($code>>12)) . chr(0x80|(($code>>6)&0x3F)) . chr(0x80|($code&0x3F))
                                (p-.= (p-cast-$ $r) (p-. (p-. (p-chr (p-bit-or #xE0 (p->> $code 12))) (p-chr (p-bit-or #x80 (p-bit-and (p->> $code 6) #x3F)))) (p-chr (p-bit-or #x80 (p-bit-and $code #x3F)))))
                
              )
              ;; else
              (progn
                ;; $$r .= chr(0xF0|($code>>18)) . chr(0x80|(($code>>12)&0x3F)) . chr(0x80|(($code>>6)&0x3F)) . chr(0x80|($code&0x3F))
                                (p-.= (p-cast-$ $r) (p-. (p-. (p-. (p-chr (p-bit-or #xF0 (p->> $code 18))) (p-chr (p-bit-or #x80 (p-bit-and (p->> $code 12) #x3F)))) (p-chr (p-bit-or #x80 (p-bit-and (p->> $code 6) #x3F)))) (p-chr (p-bit-or #x80 (p-bit-and $code #x3F)))))
                
              )
            )
          )
        )
        
      )
    )
  )
)

;; sub _pack_tmpl { ... }
(p-sub pl-_pack_tmpl (&rest %_args)
  (p-args-body
    (block nil
      (let (($ch (make-p-box nil)) ($grpbeg (make-p-box nil)) ($grpend (make-p-box nil)) ($bang (make-p-box nil)) ($be (make-p-box nil)) ($le (make-p-box nil)) ($ti_before_count (make-p-box nil)) ($star (make-p-box nil)) ($count (make-p-box nil)) ($nrep (make-p-box nil)) ($had_count (make-p-box nil)) ($dfmt (make-p-box nil)) ($dbang (make-p-box nil)) ($dbe2 (make-p-box nil)) ($dle2 (make-p-box nil)) ($ti_before_dcount (make-p-box nil)) ($dstar2 (make-p-box nil)) ($dcnt2 (make-p-box nil)) ($dnrep2 (make-p-box nil)) ($had_dcount (make-p-box nil)) ($dnb (make-p-box nil)) ($dsig (make-p-box nil)) ($ddbe (make-p-box nil)) ($actual_count (make-p-box nil)) ($darg (make-p-box nil)) ($dlen (make-p-box nil)) ($nb (make-p-box nil)) ($sig (make-p-box nil)) ($dbe (make-p-box nil)) ($v (make-p-box nil)) (@bytes (make-array 0 :adjustable t :fill-pointer 0)) ($k (make-p-box nil)) ($remaining (make-p-box nil)) ($dbe_eff (make-p-box nil)) ($i (make-p-box nil)) ($val (make-p-box nil)) ($inner (make-p-box nil)) ($gti (make-p-box nil)) ($fc (make-p-box nil)) ($ai_before (make-p-box nil)) ($iter_base (make-p-box nil)) ($r (make-p-box nil)) ($n (make-p-box nil)) ($cur (make-p-box nil)) ($pad (make-p-box nil)) ($fp (make-p-box nil)) ($t (make-p-box nil)) ($tgt (make-p-box nil)) ($abs_tgt (make-p-box nil)) ($be2 (make-p-box nil)) ($nv (make-p-box nil)) ($arg (make-p-box nil)) ($raw (make-p-box nil)) ($orig_s (make-p-box nil)))
        (let (($tmpl (make-p-box nil)) ($ai_ref (make-p-box nil)) ($args_ref (make-p-box nil)) ($result_ref (make-p-box nil)) ($inh_be (make-p-box nil)) ($inh_le (make-p-box nil)) ($out_base (make-p-box nil)) ($depth (make-p-box nil)))
          ;; my ($tmpl, $ai_ref, $args_ref, $result_ref, $inh_be, $inh_le, $out_base, $depth) = @_
                    (let ((*wantarray* nil)) (p-list-= (vector $tmpl $ai_ref $args_ref $result_ref $inh_be $inh_le $out_base $depth) @_))
          
          ;; $out_base = 0 unless defined $out_base
          (let ((*wantarray* :void)) (p-unless (p-defined $out_base) (p-my-= $out_base 0)))
          
          ;; $depth = 0 unless defined $depth
          (let ((*wantarray* :void)) (p-unless (p-defined $depth) (p-my-= $depth 0)))
          
          ;; die "Too deeply nested \(\)-groups in pack\n" if $depth > $MAX_GROUP_DEPTH
          (let ((*wantarray* :void)) (p-if (p-> $depth $MAX_GROUP_DEPTH) (p-die :loc "- line 376" "Too deeply nested ()-groups in pack
")))
          
          (let (($nargs (make-p-box nil)))
            ;; my $nargs = scalar(@$args_ref)
                        (p-my-= $nargs (p-scalar (p-cast-@ $args_ref)))
            
            (let (($ti (make-p-box nil)))
              ;; my $ti = 0
                            (p-my-= $ti 0)
              
              (let (($tlen (make-p-box nil)))
                ;; my $tlen = length($tmpl)
                                (p-my-= $tlen (p-length $tmpl))
                
                ;; while (1) {         $ti = _pack_skip_ws($tmpl, $ti);         last if $ti >= $tlen;         my $ch = substr($tmpl, $ti, 1); $ti++;         my ($grpbeg, $grpend) = (undef, undef);         if ($ch eq '(') {             $grpend = _pack_find_group_end($tmpl, $ti);             $grpbeg = $ti; $ti = $grpend + 1; $ch = '(';         }         my ($bang, $be, $le) = _pack_parse_mods($tmpl, \$ti, $inh_be, $inh_le, $ch, 'pack');         # No ws skip here: space between type+mods and count is invalid in Perl.         my $ti_before_count = $ti;         my ($star, $count, $nrep) = _pack_parse_count($tmpl, \$ti);         my $had_count = ($star || $ti > $ti_before_count);         $ti = _pack_skip_ws($tmpl, $ti);          # Slash â count prefix: ch encodes data length/count, next char is data format.         # Parse the DATA format first to compute the actual count, then encode prefix.         if ($ti < $tlen && substr($tmpl, $ti, 1) eq '/') {             $ti++;             $ti = _pack_skip_ws($tmpl, $ti); last if $ti >= $tlen;             # '/' must not have a count applied directly to it             { my $c = substr($tmpl, $ti, 1);               die "'/' does not take a repeat count in pack\n"                 if $c eq '*' || $c eq '[' || $c =~ /\d/; }             # Parse data format type, mods, count             my $dfmt = substr($tmpl, $ti, 1); $ti++;             my ($dbang, $dbe2, $dle2) = _pack_parse_mods($tmpl, \$ti, $be, $le, $dfmt, 'pack');             $ti = _pack_skip_ws($tmpl, $ti);             my $ti_before_dcount = $ti;             my ($dstar2, $dcnt2, $dnrep2) = _pack_parse_count($tmpl, \$ti);             my $had_dcount = ($dstar2 || $ti > $ti_before_dcount);             my ($dnb, $dsig, $ddbe) = _pack_type_info($dfmt, $dbang);             my $actual_count;             if ($dfmt eq 'a' || $dfmt eq 'A' || $dfmt eq 'Z') {                 # String data: one arg, output length determined by data format count                 my $darg = ($$ai_ref < $nargs) ? $args_ref->[$$ai_ref++] : '';                 $darg = '' unless defined $darg;                 my $dlen = length($darg);                 if (!$had_dcount || $dstar2) {                     # No explicit count or * â pack all chars (+ NUL for Z)                     $actual_count = ($dfmt eq 'Z') ? $dlen + 1 : $dlen;                 } else {                     $actual_count = $dnrep2;  # explicit count (may be 0)                 }                 # Encode the count in the prefix field                 my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang);                 if ($nb) {                     $$result_ref .= _pack_emit_int($actual_count, $nb, $sig, $be ? 1 : ($le ? 0 : $dbe));                 } elsif ($ch eq 'A' || $ch eq 'a') {                     _pack_str_one($ch, "$actual_count", 1, 0, $result_ref);                 } elsif ($ch eq 'Z') {                     _pack_str_one('Z', "$actual_count", length("$actual_count") + 1, 0, $result_ref);                 } elsif ($ch eq 'w') {                     my $v = $actual_count;                     if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }                 }                 # Pack the data using actual_count as the length                 _pack_str_one($dfmt, $darg, $actual_count, 0, $result_ref);             } elsif ($dnb) {                 # Numeric data: count = number of elements to pack                 my $remaining = $nargs - $$ai_ref;                 if (!$had_dcount || $dstar2) {                     # No explicit count or * â pack all remaining args                     $actual_count = $remaining;                 } else {                     # Explicit count â pack min(count, remaining) args                     $actual_count = ($dnrep2 < $remaining) ? $dnrep2 : $remaining;                 }                 # Encode count in prefix                 my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang);                 if ($nb) {                     $$result_ref .= _pack_emit_int($actual_count, $nb, $sig, $be ? 1 : ($le ? 0 : $dbe));                 } elsif ($ch eq 'A' || $ch eq 'a') {                     _pack_str_one($ch, "$actual_count", 1, 0, $result_ref);                 } elsif ($ch eq 'Z') {                     _pack_str_one('Z', "$actual_count", length("$actual_count") + 1, 0, $result_ref);                 } elsif ($ch eq 'w') {                     my $v = $actual_count;                     if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }                 }                 # Pack the elements                 my $dbe_eff = $dbe2 ? 1 : ($dle2 ? 0 : $ddbe);                 for (my $i = 0; $i < $actual_count && $$ai_ref < $nargs; $i++) {                     my $val = $args_ref->[$$ai_ref++];                     $$result_ref .= _pack_emit_int($val, $dnb, $dsig, $dbe_eff);                 }             } elsif ($dfmt eq 'w') {                 # BER data: count = number of BER elements                 my $remaining = $nargs - $$ai_ref;                 $actual_count = (!$had_dcount || $dstar2) ? $remaining :                                 ($dnrep2 < $remaining ? $dnrep2 : $remaining);                 # Encode count in prefix                 my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang);                 if ($nb) {                     $$result_ref .= _pack_emit_int($actual_count, $nb, $sig, $be ? 1 : ($le ? 0 : $dbe));                 } elsif ($ch eq 'w') {                     my $v = $actual_count;                     if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }                 }                 # Pack BER elements                 for (my $i = 0; $i < $actual_count && $$ai_ref < $nargs; $i++) {                     my $v = $args_ref->[$$ai_ref++] + 0;                     if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }                 }             }             next;         }          # Group         if (defined $grpbeg) {             my $inner = substr($tmpl, $grpbeg, $grpend - $grpbeg);             # Check: group must not start with a count             my $gti = _pack_skip_ws($inner, 0);             if ($gti < length($inner)) {                 my $fc = substr($inner, $gti, 1);                 die "\(\)-group starts with a count in pack\n" if $fc =~ /^[\d\*\[]/;             }             if ($star) {                 while ($$ai_ref < $nargs) {                     my $ai_before = $$ai_ref;                     my $iter_base = length($$result_ref);                     _pack_tmpl($inner, $ai_ref, $args_ref, $result_ref, $be, $le, $iter_base, $depth + 1);                     last if $$ai_ref == $ai_before;  # no progress: avoid infinite loop                 }             } else {                 for (my $r = 0; $r < $nrep; $r++) {                     my $iter_base = length($$result_ref);                     _pack_tmpl($inner, $ai_ref, $args_ref, $result_ref, $be, $le, $iter_base, $depth + 1);                 }             }             next;         }          # Position ops (no arg)         if ($ch eq 'x') {             if ($bang) {                 # x!N: pad to N-byte alignment                 my $n = $nrep > 0 ? $nrep : 1;                 my $cur = length($$result_ref);                 my $pad = ($n - ($cur % $n)) % $n;                 $$result_ref .= chr(0) x $pad;             } else {                 $$result_ref .= chr(0) x $nrep;             }             next;         }         if ($ch eq 'X') {             if ($bang) {                 # X!N: truncate back to N-byte alignment                 my $n = $nrep > 0 ? $nrep : 1;                 my $cur = length($$result_ref);                 $$result_ref = substr($$result_ref, 0, int($cur/$n)*$n);             } else {                 my $fp = length($$result_ref) - $nrep;                 $$result_ref = substr($$result_ref, 0, $fp < 0 ? 0 : $fp);             }             next;         }         if ($ch eq '@') {             # @N: relative to group start (out_base). @!N: absolute byte position.             my $n = defined($count) ? $count : 0;             my $t = $bang ? $n : $out_base + $n;             if (length($$result_ref) < $t) { $$result_ref .= chr(0) x ($t - length($$result_ref)) }             elsif (length($$result_ref) > $t) { $$result_ref = substr($$result_ref, 0, $t) }             next;         }         if ($ch eq '.') {             # Position format: reads target from arg list.             # .* = absolute; . or .N = relative to current group's start (out_base).             my $tgt = ($$ai_ref < $nargs) ? int(($args_ref->[$$ai_ref++] // 0) + 0) : 0;             my $abs_tgt = $star ? $tgt : $out_base + $tgt;             my $cur = length($$result_ref);             if ($cur < $abs_tgt) { $$result_ref .= chr(0) x ($abs_tgt - $cur) }             elsif ($cur > $abs_tgt) { $$result_ref = substr($$result_ref, 0, $abs_tgt) }             next;         }         if ($ch eq 'p' || $ch eq 'P' || $ch eq 'D') {             die "Invalid type '$ch' in pack\n";         }          # For multi-arg formats, * means use all remaining args         $nrep = $nargs - $$ai_ref if $star;          # Integer types         my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang);         if ($nb) {             my $be2 = $be ? 1 : ($le ? 0 : $dbe);             for (my $r = 0; $r < $nrep; $r++) {                 my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;                 my $nv = $v + 0;                 die "Cannot pack NaN in pack\n" if $nv != $nv;                 die "Cannot pack " . ($nv < 0 ? "-Inf" : "Inf") . " in pack\n" if $nv != 0 && $nv == $nv * 2;                 $$result_ref .= _pack_emit_int(int($nv), $nb, $sig, $be2);             }             next;         }          # Float types         if ($ch eq 'f') {             my $be2 = $be ? 1 : ($le ? 0 : 0);             for (my $r = 0; $r < $nrep; $r++) {                 my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;                 $$result_ref .= _pack_float32($v, $be2);             }             next;         }         if ($ch eq 'd' || $ch eq 'F') {             my $be2 = $be ? 1 : ($le ? 0 : 0);             for (my $r = 0; $r < $nrep; $r++) {                 my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;                 $$result_ref .= _pack_float64($v, $be2);             }             next;         }          # Single-arg string types         if ($ch eq 'a'||$ch eq 'A'||$ch eq 'Z'||$ch eq 'b'||$ch eq 'B'||             $ch eq 'H'||$ch eq 'h'||$ch eq 'u') {             my $arg = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // '') : '';             _pack_str_one($ch, $arg, $nrep, $star, $result_ref);             next;         }          # Per-rep types: U W w         if ($ch eq 'U') {             for (my $r = 0; $r < $nrep; $r++) {                 my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;                 my $nv = $v + 0;                 die "Cannot pack NaN in pack\n" if $nv != $nv;                 die "Cannot pack " . ($nv < 0 ? "-Inf" : "Inf") . " in pack\n" if $nv != 0 && $nv == $nv * 2;                 _pack_utf8_char(int($nv), $result_ref);             }             next;         }         if ($ch eq 'W') {             for (my $r = 0; $r < $nrep; $r++) {                 my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;                 my $nv = $v + 0;                 die "Cannot pack NaN in pack\n" if $nv != $nv;                 die "Cannot pack " . ($nv < 0 ? "-Inf" : "Inf") . " in pack\n" if $nv != 0 && $nv == $nv * 2;                 $$result_ref .= chr(int($nv));             }             next;         }         if ($ch eq 'w') {             for (my $r = 0; $r < $nrep; $r++) {                 my $raw = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;                 my $orig_s = "$raw";  # stringify BEFORE numeric coercion                 my $v = $raw + 0;                 die "Cannot compress NaN in pack\n" if $v != $v;                 die "Cannot compress -Inf in pack\n" if $v < 0 && $v == $v * 2;                 die "Cannot compress negative numbers in pack\n" if $v < 0;                 die "Cannot compress Inf in pack\n" if $v != 0 && $v == $v * 2;                 die "Can only compress unsigned integers in pack\n" if $v != int($v);                 # Perl also rejects float-notation strings (e.g. "1e21") when value > UV_MAX.                 # Pure-digit strings (even > UV_MAX) succeed; so does any non-string source.                 # In CL: pure-digit strings â exact bignum; e/E strings â double-float.                 # The $v != int($v) check cannot distinguish a large integer-valued float from                 # an exact bignum, so check the original string form for scientific notation.                 die "Can only compress unsigned integers in pack\n"                     if $orig_s =~ /[eE]/ && $v >= 2**64;                 $v = int($v);                 if ($v == 0) { $$result_ref .= chr(0); next }                 my @bytes;                 while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                 for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                 $$result_ref .= chr($bytes[-1]);             }             next;         }          die "Invalid type '/' in pack\n" if $ch eq '/';         die "Invalid type '$ch' in pack\n";     }
                (p-while 1
                  (let (($ch (make-p-box nil)) ($grpbeg (make-p-box nil)) ($grpend (make-p-box nil)) ($bang (make-p-box nil)) ($be (make-p-box nil)) ($le (make-p-box nil)) ($ti_before_count (make-p-box nil)) ($star (make-p-box nil)) ($count (make-p-box nil)) ($nrep (make-p-box nil)) ($had_count (make-p-box nil)) ($dfmt (make-p-box nil)) ($dbang (make-p-box nil)) ($dbe2 (make-p-box nil)) ($dle2 (make-p-box nil)) ($ti_before_dcount (make-p-box nil)) ($dstar2 (make-p-box nil)) ($dcnt2 (make-p-box nil)) ($dnrep2 (make-p-box nil)) ($had_dcount (make-p-box nil)) ($dnb (make-p-box nil)) ($dsig (make-p-box nil)) ($ddbe (make-p-box nil)) ($actual_count (make-p-box nil)) ($darg (make-p-box nil)) ($dlen (make-p-box nil)) ($nb (make-p-box nil)) ($sig (make-p-box nil)) ($dbe (make-p-box nil)) ($v (make-p-box nil)) (@bytes (make-array 0 :adjustable t :fill-pointer 0)) ($k (make-p-box nil)) ($remaining (make-p-box nil)) ($dbe_eff (make-p-box nil)) ($i (make-p-box nil)) ($val (make-p-box nil)) ($inner (make-p-box nil)) ($gti (make-p-box nil)) ($fc (make-p-box nil)) ($ai_before (make-p-box nil)) ($iter_base (make-p-box nil)) ($r (make-p-box nil)) ($n (make-p-box nil)) ($cur (make-p-box nil)) ($pad (make-p-box nil)) ($fp (make-p-box nil)) ($t (make-p-box nil)) ($tgt (make-p-box nil)) ($abs_tgt (make-p-box nil)) ($be2 (make-p-box nil)) ($nv (make-p-box nil)) ($arg (make-p-box nil)) ($raw (make-p-box nil)) ($orig_s (make-p-box nil)))
                    ;; $ti = _pack_skip_ws($tmpl, $ti)
                                        (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                    
                    ;; last if $ti >= $tlen
                    (p-if (p->= $ti $tlen) (p-last))
                    
                    ;; my $ch = substr($tmpl, $ti, 1)
                                        (p-my-= $ch (p-substr $tmpl $ti 1))
                    
                    ;; $ti++
                                        (p-post++ $ti)
                    
                    ;; my ($grpbeg, $grpend) = (undef, undef)
                                        (let ((*wantarray* nil)) (p-list-= (vector $grpbeg $grpend) (vector (p-undef) (p-undef))))
                    
                    ;; if ($ch eq '(') {             $grpend = _pack_find_group_end($tmpl, $ti);             $grpbeg = $ti; $ti = $grpend + 1; $ch = '(';         }
                    ;; if ($ch eq '(')
                    (p-if (p-str-eq $ch "(")
                      (progn
                        ;; $grpend = _pack_find_group_end($tmpl, $ti)
                                                (p-my-= $grpend (pl-_pack_find_group_end $tmpl $ti))
                        
                        ;; $grpbeg = $ti
                                                (p-my-= $grpbeg $ti)
                        
                        ;; $ti = $grpend + 1
                                                (p-my-= $ti (p-+ $grpend 1))
                        
                        ;; $ch = '('
                                                (p-my-= $ch "(")
                        
                      )
                      nil
                    )
                    
                    ;; my ($bang, $be, $le) = _pack_parse_mods($tmpl, \$ti, $inh_be, $inh_le, $ch, 'pack')
                                        (let ((*wantarray* nil)) (p-list-= (vector $bang $be $le) (let ((*wantarray* t)) (pl-_pack_parse_mods $tmpl (p-backslash $ti) $inh_be $inh_le $ch "pack"))))
                    
                    ;; my $ti_before_count = $ti
                                        (p-my-= $ti_before_count $ti)
                    
                    ;; my ($star, $count, $nrep) = _pack_parse_count($tmpl, \$ti)
                                        (let ((*wantarray* nil)) (p-list-= (vector $star $count $nrep) (let ((*wantarray* t)) (pl-_pack_parse_count $tmpl (p-backslash $ti)))))
                    
                    ;; my $had_count = ($star || $ti > $ti_before_count)
                                        (p-my-= $had_count (p-|| $star (p-> $ti $ti_before_count)))
                    
                    ;; $ti = _pack_skip_ws($tmpl, $ti)
                    (let ((*wantarray* :void))                     (p-my-= $ti (let ((*wantarray* nil)) (pl-_pack_skip_ws $tmpl $ti))))
                    
                    ;; if ($ti < $tlen && substr($tmpl, $ti, 1) eq '/') {             $ti++;             $ti = _pack_skip_ws($tmpl, $ti); last if $ti >= $tlen;             # '/' must not have a count applied directly to it             { my $c = substr($tmpl, $ti, 1);               die "'/' does not take a repeat count in pack\n"                 if $c eq '*' || $c eq '[' || $c =~ /\d/; }             # Parse data format type, mods, count             my $dfmt = substr($tmpl, $ti, 1); $ti++;             my ($dbang, $dbe2, $dle2) = _pack_parse_mods($tmpl, \$ti, $be, $le, $dfmt, 'pack');             $ti = _pack_skip_ws($tmpl, $ti);             my $ti_before_dcount = $ti;             my ($dstar2, $dcnt2, $dnrep2) = _pack_parse_count($tmpl, \$ti);             my $had_dcount = ($dstar2 || $ti > $ti_before_dcount);             my ($dnb, $dsig, $ddbe) = _pack_type_info($dfmt, $dbang);             my $actual_count;             if ($dfmt eq 'a' || $dfmt eq 'A' || $dfmt eq 'Z') {                 # String data: one arg, output length determined by data format count                 my $darg = ($$ai_ref < $nargs) ? $args_ref->[$$ai_ref++] : '';                 $darg = '' unless defined $darg;                 my $dlen = length($darg);                 if (!$had_dcount || $dstar2) {                     # No explicit count or * â pack all chars (+ NUL for Z)                     $actual_count = ($dfmt eq 'Z') ? $dlen + 1 : $dlen;                 } else {                     $actual_count = $dnrep2;  # explicit count (may be 0)                 }                 # Encode the count in the prefix field                 my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang);                 if ($nb) {                     $$result_ref .= _pack_emit_int($actual_count, $nb, $sig, $be ? 1 : ($le ? 0 : $dbe));                 } elsif ($ch eq 'A' || $ch eq 'a') {                     _pack_str_one($ch, "$actual_count", 1, 0, $result_ref);                 } elsif ($ch eq 'Z') {                     _pack_str_one('Z', "$actual_count", length("$actual_count") + 1, 0, $result_ref);                 } elsif ($ch eq 'w') {                     my $v = $actual_count;                     if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }                 }                 # Pack the data using actual_count as the length                 _pack_str_one($dfmt, $darg, $actual_count, 0, $result_ref);             } elsif ($dnb) {                 # Numeric data: count = number of elements to pack                 my $remaining = $nargs - $$ai_ref;                 if (!$had_dcount || $dstar2) {                     # No explicit count or * â pack all remaining args                     $actual_count = $remaining;                 } else {                     # Explicit count â pack min(count, remaining) args                     $actual_count = ($dnrep2 < $remaining) ? $dnrep2 : $remaining;                 }                 # Encode count in prefix                 my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang);                 if ($nb) {                     $$result_ref .= _pack_emit_int($actual_count, $nb, $sig, $be ? 1 : ($le ? 0 : $dbe));                 } elsif ($ch eq 'A' || $ch eq 'a') {                     _pack_str_one($ch, "$actual_count", 1, 0, $result_ref);                 } elsif ($ch eq 'Z') {                     _pack_str_one('Z', "$actual_count", length("$actual_count") + 1, 0, $result_ref);                 } elsif ($ch eq 'w') {                     my $v = $actual_count;                     if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }                 }                 # Pack the elements                 my $dbe_eff = $dbe2 ? 1 : ($dle2 ? 0 : $ddbe);                 for (my $i = 0; $i < $actual_count && $$ai_ref < $nargs; $i++) {                     my $val = $args_ref->[$$ai_ref++];                     $$result_ref .= _pack_emit_int($val, $dnb, $dsig, $dbe_eff);                 }             } elsif ($dfmt eq 'w') {                 # BER data: count = number of BER elements                 my $remaining = $nargs - $$ai_ref;                 $actual_count = (!$had_dcount || $dstar2) ? $remaining :                                 ($dnrep2 < $remaining ? $dnrep2 : $remaining);                 # Encode count in prefix                 my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang);                 if ($nb) {                     $$result_ref .= _pack_emit_int($actual_count, $nb, $sig, $be ? 1 : ($le ? 0 : $dbe));                 } elsif ($ch eq 'w') {                     my $v = $actual_count;                     if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }                 }                 # Pack BER elements                 for (my $i = 0; $i < $actual_count && $$ai_ref < $nargs; $i++) {                     my $v = $args_ref->[$$ai_ref++] + 0;                     if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }                 }             }             next;         }
                    ;; if ($ti < $tlen && substr($tmpl, $ti, 1) eq '/')
                    (p-if (p-&& (p-< $ti $tlen) (p-str-eq (p-substr $tmpl $ti 1) "/"))
                      (progn
                        (let (($dfmt (make-p-box nil)) ($dbang (make-p-box nil)) ($dbe2 (make-p-box nil)) ($dle2 (make-p-box nil)) ($ti_before_dcount (make-p-box nil)) ($dstar2 (make-p-box nil)) ($dcnt2 (make-p-box nil)) ($dnrep2 (make-p-box nil)) ($had_dcount (make-p-box nil)) ($dnb (make-p-box nil)) ($dsig (make-p-box nil)) ($ddbe (make-p-box nil)) ($actual_count (make-p-box nil)) ($darg (make-p-box nil)) ($dlen (make-p-box nil)) ($nb (make-p-box nil)) ($sig (make-p-box nil)) ($dbe (make-p-box nil)) ($v (make-p-box nil)) (@bytes (make-array 0 :adjustable t :fill-pointer 0)) ($k (make-p-box nil)) ($remaining (make-p-box nil)) ($dbe_eff (make-p-box nil)) ($i (make-p-box nil)) ($val (make-p-box nil)))
                          ;; $ti++
                          (let ((*wantarray* :void))                           (p-post++ $ti))
                          
                          ;; $ti = _pack_skip_ws($tmpl, $ti)
                          (let ((*wantarray* :void))                           (p-my-= $ti (let ((*wantarray* nil)) (pl-_pack_skip_ws $tmpl $ti))))
                          
                          ;; last if $ti >= $tlen
                          (let ((*wantarray* :void)) (p-if (p->= $ti $tlen) (p-last)))
                          
                          ;; { ... }
                          (let ((*package* *package*))
                            (let (($c (make-p-box nil)))
                              (block nil
                                (tagbody :redo
                                  (let ((--pcl-if-ret--9 nil))
                                    ;; my $c = substr($tmpl, $ti, 1)
                                                                        (p-my-= $c (p-substr $tmpl $ti 1))
                                    
                                    ;; die "'/' does not take a repeat count in pack\n"                 if $c eq '*' || $c eq '[' || $c =~ /\d/
                                    (p-if (setf --pcl-if-ret--9 (p-|| (p-|| (p-str-eq $c "*") (p-str-eq $c "[")) (let ((*wantarray* nil)) (p-=~ $c (p-regex "/\\d/")))))
                                      (setf --pcl-if-ret--9 (p-die :loc "- line 403" "'/' does not take a repeat count in pack
"))
                                      nil)
                                    
                                  --pcl-if-ret--9)
                                  :next)
                              )
                            )
                          )
                          
                          ;; my $dfmt = substr($tmpl, $ti, 1)
                                                    (p-my-= $dfmt (p-substr $tmpl $ti 1))
                          
                          ;; $ti++
                          (let ((*wantarray* :void))                           (p-post++ $ti))
                          
                          ;; my ($dbang, $dbe2, $dle2) = _pack_parse_mods($tmpl, \$ti, $be, $le, $dfmt, 'pack')
                                                    (let ((*wantarray* nil)) (p-list-= (vector $dbang $dbe2 $dle2) (let ((*wantarray* t)) (pl-_pack_parse_mods $tmpl (p-backslash $ti) $be $le $dfmt "pack"))))
                          
                          ;; $ti = _pack_skip_ws($tmpl, $ti)
                          (let ((*wantarray* :void))                           (p-my-= $ti (let ((*wantarray* nil)) (pl-_pack_skip_ws $tmpl $ti))))
                          
                          ;; my $ti_before_dcount = $ti
                                                    (p-my-= $ti_before_dcount $ti)
                          
                          ;; my ($dstar2, $dcnt2, $dnrep2) = _pack_parse_count($tmpl, \$ti)
                                                    (let ((*wantarray* nil)) (p-list-= (vector $dstar2 $dcnt2 $dnrep2) (let ((*wantarray* t)) (pl-_pack_parse_count $tmpl (p-backslash $ti)))))
                          
                          ;; my $had_dcount = ($dstar2 || $ti > $ti_before_dcount)
                                                    (p-my-= $had_dcount (p-|| $dstar2 (p-> $ti $ti_before_dcount)))
                          
                          ;; my ($dnb, $dsig, $ddbe) = _pack_type_info($dfmt, $dbang)
                                                    (let ((*wantarray* nil)) (p-list-= (vector $dnb $dsig $ddbe) (let ((*wantarray* t)) (pl-_pack_type_info $dfmt $dbang))))
                          
                          ;; my $actual_count (bare declaration)
                          
                          ;; if ($dfmt eq 'a' || $dfmt eq 'A' || $dfmt eq 'Z') {                 # String data: one arg, output length determined by data format count                 my $darg = ($$ai_ref < $nargs) ? $args_ref->[$$ai_ref++] : '';                 $darg = '' unless defined $darg;                 my $dlen = length($darg);                 if (!$had_dcount || $dstar2) {                     # No explicit count or * â pack all chars (+ NUL for Z)                     $actual_count = ($dfmt eq 'Z') ? $dlen + 1 : $dlen;                 } else {                     $actual_count = $dnrep2;  # explicit count (may be 0)                 }                 # Encode the count in the prefix field                 my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang);                 if ($nb) {                     $$result_ref .= _pack_emit_int($actual_count, $nb, $sig, $be ? 1 : ($le ? 0 : $dbe));                 } elsif ($ch eq 'A' || $ch eq 'a') {                     _pack_str_one($ch, "$actual_count", 1, 0, $result_ref);                 } elsif ($ch eq 'Z') {                     _pack_str_one('Z', "$actual_count", length("$actual_count") + 1, 0, $result_ref);                 } elsif ($ch eq 'w') {                     my $v = $actual_count;                     if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }                 }                 # Pack the data using actual_count as the length                 _pack_str_one($dfmt, $darg, $actual_count, 0, $result_ref);             } elsif ($dnb) {                 # Numeric data: count = number of elements to pack                 my $remaining = $nargs - $$ai_ref;                 if (!$had_dcount || $dstar2) {                     # No explicit count or * â pack all remaining args                     $actual_count = $remaining;                 } else {                     # Explicit count â pack min(count, remaining) args                     $actual_count = ($dnrep2 < $remaining) ? $dnrep2 : $remaining;                 }                 # Encode count in prefix                 my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang);                 if ($nb) {                     $$result_ref .= _pack_emit_int($actual_count, $nb, $sig, $be ? 1 : ($le ? 0 : $dbe));                 } elsif ($ch eq 'A' || $ch eq 'a') {                     _pack_str_one($ch, "$actual_count", 1, 0, $result_ref);                 } elsif ($ch eq 'Z') {                     _pack_str_one('Z', "$actual_count", length("$actual_count") + 1, 0, $result_ref);                 } elsif ($ch eq 'w') {                     my $v = $actual_count;                     if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }                 }                 # Pack the elements                 my $dbe_eff = $dbe2 ? 1 : ($dle2 ? 0 : $ddbe);                 for (my $i = 0; $i < $actual_count && $$ai_ref < $nargs; $i++) {                     my $val = $args_ref->[$$ai_ref++];                     $$result_ref .= _pack_emit_int($val, $dnb, $dsig, $dbe_eff);                 }             } elsif ($dfmt eq 'w') {                 # BER data: count = number of BER elements                 my $remaining = $nargs - $$ai_ref;                 $actual_count = (!$had_dcount || $dstar2) ? $remaining :                                 ($dnrep2 < $remaining ? $dnrep2 : $remaining);                 # Encode count in prefix                 my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang);                 if ($nb) {                     $$result_ref .= _pack_emit_int($actual_count, $nb, $sig, $be ? 1 : ($le ? 0 : $dbe));                 } elsif ($ch eq 'w') {                     my $v = $actual_count;                     if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }                 }                 # Pack BER elements                 for (my $i = 0; $i < $actual_count && $$ai_ref < $nargs; $i++) {                     my $v = $args_ref->[$$ai_ref++] + 0;                     if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }                 }             }
                          ;; if ($dfmt eq 'a' || $dfmt eq 'A' || $dfmt eq 'Z')
                          (p-if (p-|| (p-|| (p-str-eq $dfmt "a") (p-str-eq $dfmt "A")) (p-str-eq $dfmt "Z"))
                            (progn
                              (let (($darg (make-p-box nil)) ($dlen (make-p-box nil)) ($nb (make-p-box nil)) ($sig (make-p-box nil)) ($dbe (make-p-box nil)) ($v (make-p-box nil)) (@bytes (make-array 0 :adjustable t :fill-pointer 0)) ($k (make-p-box nil)))
                                ;; my $darg = ($$ai_ref < $nargs) ? $args_ref->[$$ai_ref++] : ''
                                                                (p-my-= $darg (p-if (p-< (p-cast-$ $ai_ref) $nargs) (p-aref-deref $args_ref (p-post++ (p-cast-$ $ai_ref))) ""))
                                
                                ;; $darg = '' unless defined $darg
                                (let ((*wantarray* :void)) (p-unless (p-defined $darg) (p-my-= $darg "")))
                                
                                ;; my $dlen = length($darg)
                                                                (p-my-= $dlen (p-length $darg))
                                
                                ;; if (!$had_dcount || $dstar2) {                     # No explicit count or * â pack all chars (+ NUL for Z)                     $actual_count = ($dfmt eq 'Z') ? $dlen + 1 : $dlen;                 } else {                     $actual_count = $dnrep2;  # explicit count (may be 0)                 }
                                ;; if (!$had_dcount || $dstar2)
                                (p-if (p-|| (p-! $had_dcount) $dstar2)
                                  (progn
                                    ;; $actual_count = ($dfmt eq 'Z') ? $dlen + 1 : $dlen
                                                                        (p-my-= $actual_count (p-if (p-str-eq $dfmt "Z") (p-+ $dlen 1) $dlen))
                                    
                                  )
                                  ;; else
                                  (progn
                                    ;; $actual_count = $dnrep2
                                                                        (p-my-= $actual_count $dnrep2)
                                    
                                  )
                                )
                                
                                ;; my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang)
                                                                (let ((*wantarray* nil)) (p-list-= (vector $nb $sig $dbe) (let ((*wantarray* t)) (pl-_pack_type_info $ch $bang))))
                                
                                ;; if ($nb) {                     $$result_ref .= _pack_emit_int($actual_count, $nb, $sig, $be ? 1 : ($le ? 0 : $dbe));                 } elsif ($ch eq 'A' || $ch eq 'a') {                     _pack_str_one($ch, "$actual_count", 1, 0, $result_ref);                 } elsif ($ch eq 'Z') {                     _pack_str_one('Z', "$actual_count", length("$actual_count") + 1, 0, $result_ref);                 } elsif ($ch eq 'w') {                     my $v = $actual_count;                     if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }                 }
                                ;; if ($nb)
                                (p-if $nb
                                  (progn
                                    ;; $$result_ref .= _pack_emit_int($actual_count, $nb, $sig, $be ? 1 : ($le ? 0 : $dbe))
                                                                        (p-.= (p-cast-$ $result_ref) (pl-_pack_emit_int $actual_count $nb $sig (p-if $be 1 (p-if $le 0 $dbe))))
                                    
                                  )
                                  ;; elsif ($ch eq 'A' || $ch eq 'a')
                                  (p-if (p-|| (p-str-eq $ch "A") (p-str-eq $ch "a"))
                                    (progn
                                      ;; _pack_str_one($ch, "$actual_count", 1, 0, $result_ref)
                                                                            (pl-_pack_str_one $ch $actual_count 1 0 $result_ref)
                                      
                                    )
                                    ;; elsif ($ch eq 'Z')
                                    (p-if (p-str-eq $ch "Z")
                                      (progn
                                        ;; _pack_str_one('Z', "$actual_count", length("$actual_count") + 1, 0, $result_ref)
                                                                                (pl-_pack_str_one "Z" $actual_count (p-+ (p-length $actual_count) 1) 0 $result_ref)
                                        
                                      )
                                      ;; elsif ($ch eq 'w')
                                      (p-if (p-str-eq $ch "w")
                                        (progn
                                          (let (($v (make-p-box nil)) (@bytes (make-array 0 :adjustable t :fill-pointer 0)) ($k (make-p-box nil)))
                                            ;; my $v = $actual_count
                                                                                        (p-my-= $v $actual_count)
                                            
                                            ;; if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }
                                            ;; if ($v == 0)
                                            (p-if (p-== $v 0)
                                              (progn
                                                ;; $$result_ref .= chr(0)
                                                                                                (p-.= (p-cast-$ $result_ref) (p-chr 0))
                                                
                                              )
                                              ;; else
                                              (progn
                                                (let ((@bytes (make-array 0 :adjustable t :fill-pointer 0)) ($k (make-p-box nil)))
                                                  ;; my @bytes (bare declaration)
                                                  
                                                  ;; while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }
                                                  (p-while (p-> $v 0)
                                                    ;; unshift @bytes, ($v & 0x7F)
                                                    (let ((*wantarray* :void))                                                     (p-unshift @bytes (p-bit-and $v #x7F)))
                                                    
                                                    ;; $v >>= 7
                                                                                                        (p->>= $v 7)
                                                    
                                                  )
                                                  
                                                  ;; for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }
                                                  (let (($k (make-p-box nil)))
                                                    (p-for (                                                  (p-my-= $k 0))
                                                            (                                                  (p-< $k (p-array-last-index @bytes)))
                                                            (                                                  (p-post++ $k))
                                                      ;; $$result_ref .= chr($bytes[$k] | 0x80)
                                                                                                            (p-.= (p-cast-$ $result_ref) (p-chr (p-bit-or (p-aref @bytes $k) #x80)))
                                                      
                                                    )
                                                  )
                                                  
                                                  ;; $$result_ref .= chr($bytes[-1])
                                                                                                    (p-.= (p-cast-$ $result_ref) (p-chr (p-aref @bytes -1)))
                                                  
                                                )
                                              )
                                            )
                                            
                                          )
                                        )
                                        nil
                                      )
                                    )
                                  )
                                )
                                
                                ;; _pack_str_one($dfmt, $darg, $actual_count, 0, $result_ref)
                                                                (pl-_pack_str_one $dfmt $darg $actual_count 0 $result_ref)
                                
                              )
                            )
                            ;; elsif ($dnb)
                            (p-if $dnb
                              (progn
                                (let (($remaining (make-p-box nil)) ($nb (make-p-box nil)) ($sig (make-p-box nil)) ($dbe (make-p-box nil)) ($v (make-p-box nil)) (@bytes (make-array 0 :adjustable t :fill-pointer 0)) ($k (make-p-box nil)) ($dbe_eff (make-p-box nil)) ($i (make-p-box nil)) ($val (make-p-box nil)))
                                  ;; my $remaining = $nargs - $$ai_ref
                                                                    (p-my-= $remaining (p-- $nargs (p-cast-$ $ai_ref)))
                                  
                                  ;; if (!$had_dcount || $dstar2) {                     # No explicit count or * â pack all remaining args                     $actual_count = $remaining;                 } else {                     # Explicit count â pack min(count, remaining) args                     $actual_count = ($dnrep2 < $remaining) ? $dnrep2 : $remaining;                 }
                                  ;; if (!$had_dcount || $dstar2)
                                  (p-if (p-|| (p-! $had_dcount) $dstar2)
                                    (progn
                                      ;; $actual_count = $remaining
                                                                            (p-my-= $actual_count $remaining)
                                      
                                    )
                                    ;; else
                                    (progn
                                      ;; $actual_count = ($dnrep2 < $remaining) ? $dnrep2 : $remaining
                                                                            (p-my-= $actual_count (p-if (p-< $dnrep2 $remaining) $dnrep2 $remaining))
                                      
                                    )
                                  )
                                  
                                  ;; my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang)
                                                                    (let ((*wantarray* nil)) (p-list-= (vector $nb $sig $dbe) (let ((*wantarray* t)) (pl-_pack_type_info $ch $bang))))
                                  
                                  ;; if ($nb) {                     $$result_ref .= _pack_emit_int($actual_count, $nb, $sig, $be ? 1 : ($le ? 0 : $dbe));                 } elsif ($ch eq 'A' || $ch eq 'a') {                     _pack_str_one($ch, "$actual_count", 1, 0, $result_ref);                 } elsif ($ch eq 'Z') {                     _pack_str_one('Z', "$actual_count", length("$actual_count") + 1, 0, $result_ref);                 } elsif ($ch eq 'w') {                     my $v = $actual_count;                     if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }                 }
                                  ;; if ($nb)
                                  (p-if $nb
                                    (progn
                                      ;; $$result_ref .= _pack_emit_int($actual_count, $nb, $sig, $be ? 1 : ($le ? 0 : $dbe))
                                                                            (p-.= (p-cast-$ $result_ref) (pl-_pack_emit_int $actual_count $nb $sig (p-if $be 1 (p-if $le 0 $dbe))))
                                      
                                    )
                                    ;; elsif ($ch eq 'A' || $ch eq 'a')
                                    (p-if (p-|| (p-str-eq $ch "A") (p-str-eq $ch "a"))
                                      (progn
                                        ;; _pack_str_one($ch, "$actual_count", 1, 0, $result_ref)
                                                                                (pl-_pack_str_one $ch $actual_count 1 0 $result_ref)
                                        
                                      )
                                      ;; elsif ($ch eq 'Z')
                                      (p-if (p-str-eq $ch "Z")
                                        (progn
                                          ;; _pack_str_one('Z', "$actual_count", length("$actual_count") + 1, 0, $result_ref)
                                                                                    (pl-_pack_str_one "Z" $actual_count (p-+ (p-length $actual_count) 1) 0 $result_ref)
                                          
                                        )
                                        ;; elsif ($ch eq 'w')
                                        (p-if (p-str-eq $ch "w")
                                          (progn
                                            (let (($v (make-p-box nil)) (@bytes (make-array 0 :adjustable t :fill-pointer 0)) ($k (make-p-box nil)))
                                              ;; my $v = $actual_count
                                                                                            (p-my-= $v $actual_count)
                                              
                                              ;; if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }
                                              ;; if ($v == 0)
                                              (p-if (p-== $v 0)
                                                (progn
                                                  ;; $$result_ref .= chr(0)
                                                                                                    (p-.= (p-cast-$ $result_ref) (p-chr 0))
                                                  
                                                )
                                                ;; else
                                                (progn
                                                  (let ((@bytes (make-array 0 :adjustable t :fill-pointer 0)) ($k (make-p-box nil)))
                                                    ;; my @bytes (bare declaration)
                                                    
                                                    ;; while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }
                                                    (p-while (p-> $v 0)
                                                      ;; unshift @bytes, ($v & 0x7F)
                                                      (let ((*wantarray* :void))                                                       (p-unshift @bytes (p-bit-and $v #x7F)))
                                                      
                                                      ;; $v >>= 7
                                                                                                            (p->>= $v 7)
                                                      
                                                    )
                                                    
                                                    ;; for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }
                                                    (let (($k (make-p-box nil)))
                                                      (p-for (                                                    (p-my-= $k 0))
                                                              (                                                    (p-< $k (p-array-last-index @bytes)))
                                                              (                                                    (p-post++ $k))
                                                        ;; $$result_ref .= chr($bytes[$k] | 0x80)
                                                                                                                (p-.= (p-cast-$ $result_ref) (p-chr (p-bit-or (p-aref @bytes $k) #x80)))
                                                        
                                                      )
                                                    )
                                                    
                                                    ;; $$result_ref .= chr($bytes[-1])
                                                                                                        (p-.= (p-cast-$ $result_ref) (p-chr (p-aref @bytes -1)))
                                                    
                                                  )
                                                )
                                              )
                                              
                                            )
                                          )
                                          nil
                                        )
                                      )
                                    )
                                  )
                                  
                                  ;; my $dbe_eff = $dbe2 ? 1 : ($dle2 ? 0 : $ddbe)
                                                                    (p-my-= $dbe_eff (p-if $dbe2 1 (p-if $dle2 0 $ddbe)))
                                  
                                  ;; for (my $i = 0; $i < $actual_count && $$ai_ref < $nargs; $i++) {                     my $val = $args_ref->[$$ai_ref++];                     $$result_ref .= _pack_emit_int($val, $dnb, $dsig, $dbe_eff);                 }
                                  (let (($i (make-p-box nil)))
                                    (p-for (                                  (p-my-= $i 0))
                                            (                                  (p-&& (p-< $i $actual_count) (p-< (p-cast-$ $ai_ref) $nargs)))
                                            (                                  (p-post++ $i))
                                      (let (($val (make-p-box nil)))
                                        ;; my $val = $args_ref->[$$ai_ref++]
                                                                                (p-my-= $val (p-aref-deref $args_ref (p-post++ (p-cast-$ $ai_ref))))
                                        
                                        ;; $$result_ref .= _pack_emit_int($val, $dnb, $dsig, $dbe_eff)
                                                                                (p-.= (p-cast-$ $result_ref) (pl-_pack_emit_int $val $dnb $dsig $dbe_eff))
                                        
                                      )
                                    )
                                  )
                                  
                                )
                              )
                              ;; elsif ($dfmt eq 'w')
                              (p-if (p-str-eq $dfmt "w")
                                (progn
                                  (let (($remaining (make-p-box nil)) ($nb (make-p-box nil)) ($sig (make-p-box nil)) ($dbe (make-p-box nil)) ($v (make-p-box nil)) (@bytes (make-array 0 :adjustable t :fill-pointer 0)) ($k (make-p-box nil)) ($i (make-p-box nil)))
                                    ;; my $remaining = $nargs - $$ai_ref
                                                                        (p-my-= $remaining (p-- $nargs (p-cast-$ $ai_ref)))
                                    
                                    ;; $actual_count = (!$had_dcount || $dstar2) ? $remaining :                                 ($dnrep2 < $remaining ? $dnrep2 : $remaining)
                                    (let ((*wantarray* :void))                                     (p-my-= $actual_count (p-if (p-|| (p-! $had_dcount) $dstar2) $remaining (p-if (p-< $dnrep2 $remaining) $dnrep2 $remaining))))
                                    
                                    ;; my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang)
                                                                        (let ((*wantarray* nil)) (p-list-= (vector $nb $sig $dbe) (let ((*wantarray* t)) (pl-_pack_type_info $ch $bang))))
                                    
                                    ;; if ($nb) {                     $$result_ref .= _pack_emit_int($actual_count, $nb, $sig, $be ? 1 : ($le ? 0 : $dbe));                 } elsif ($ch eq 'w') {                     my $v = $actual_count;                     if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }                 }
                                    ;; if ($nb)
                                    (p-if $nb
                                      (progn
                                        ;; $$result_ref .= _pack_emit_int($actual_count, $nb, $sig, $be ? 1 : ($le ? 0 : $dbe))
                                                                                (p-.= (p-cast-$ $result_ref) (pl-_pack_emit_int $actual_count $nb $sig (p-if $be 1 (p-if $le 0 $dbe))))
                                        
                                      )
                                      ;; elsif ($ch eq 'w')
                                      (p-if (p-str-eq $ch "w")
                                        (progn
                                          (let (($v (make-p-box nil)) (@bytes (make-array 0 :adjustable t :fill-pointer 0)) ($k (make-p-box nil)))
                                            ;; my $v = $actual_count
                                                                                        (p-my-= $v $actual_count)
                                            
                                            ;; if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }
                                            ;; if ($v == 0)
                                            (p-if (p-== $v 0)
                                              (progn
                                                ;; $$result_ref .= chr(0)
                                                                                                (p-.= (p-cast-$ $result_ref) (p-chr 0))
                                                
                                              )
                                              ;; else
                                              (progn
                                                (let ((@bytes (make-array 0 :adjustable t :fill-pointer 0)) ($k (make-p-box nil)))
                                                  ;; my @bytes (bare declaration)
                                                  
                                                  ;; while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }
                                                  (p-while (p-> $v 0)
                                                    ;; unshift @bytes, ($v & 0x7F)
                                                    (let ((*wantarray* :void))                                                     (p-unshift @bytes (p-bit-and $v #x7F)))
                                                    
                                                    ;; $v >>= 7
                                                                                                        (p->>= $v 7)
                                                    
                                                  )
                                                  
                                                  ;; for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }
                                                  (let (($k (make-p-box nil)))
                                                    (p-for (                                                  (p-my-= $k 0))
                                                            (                                                  (p-< $k (p-array-last-index @bytes)))
                                                            (                                                  (p-post++ $k))
                                                      ;; $$result_ref .= chr($bytes[$k] | 0x80)
                                                                                                            (p-.= (p-cast-$ $result_ref) (p-chr (p-bit-or (p-aref @bytes $k) #x80)))
                                                      
                                                    )
                                                  )
                                                  
                                                  ;; $$result_ref .= chr($bytes[-1])
                                                                                                    (p-.= (p-cast-$ $result_ref) (p-chr (p-aref @bytes -1)))
                                                  
                                                )
                                              )
                                            )
                                            
                                          )
                                        )
                                        nil
                                      )
                                    )
                                    
                                    ;; for (my $i = 0; $i < $actual_count && $$ai_ref < $nargs; $i++) {                     my $v = $args_ref->[$$ai_ref++] + 0;                     if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }                 }
                                    (let (($i (make-p-box nil)))
                                      (p-for (                                    (p-my-= $i 0))
                                              (                                    (p-&& (p-< $i $actual_count) (p-< (p-cast-$ $ai_ref) $nargs)))
                                              (                                    (p-post++ $i))
                                        (let (($v (make-p-box nil)) (@bytes (make-array 0 :adjustable t :fill-pointer 0)) ($k (make-p-box nil)))
                                          ;; my $v = $args_ref->[$$ai_ref++] + 0
                                                                                    (p-my-= $v (p-+ (p-aref-deref $args_ref (p-post++ (p-cast-$ $ai_ref))) 0))
                                          
                                          ;; if ($v == 0) { $$result_ref .= chr(0); }                     else {                         my @bytes;                         while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                         for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                         $$result_ref .= chr($bytes[-1]);                     }
                                          ;; if ($v == 0)
                                          (p-if (p-== $v 0)
                                            (progn
                                              ;; $$result_ref .= chr(0)
                                                                                            (p-.= (p-cast-$ $result_ref) (p-chr 0))
                                              
                                            )
                                            ;; else
                                            (progn
                                              (let ((@bytes (make-array 0 :adjustable t :fill-pointer 0)) ($k (make-p-box nil)))
                                                ;; my @bytes (bare declaration)
                                                
                                                ;; while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }
                                                (p-while (p-> $v 0)
                                                  ;; unshift @bytes, ($v & 0x7F)
                                                  (let ((*wantarray* :void))                                                   (p-unshift @bytes (p-bit-and $v #x7F)))
                                                  
                                                  ;; $v >>= 7
                                                                                                    (p->>= $v 7)
                                                  
                                                )
                                                
                                                ;; for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }
                                                (let (($k (make-p-box nil)))
                                                  (p-for (                                                (p-my-= $k 0))
                                                          (                                                (p-< $k (p-array-last-index @bytes)))
                                                          (                                                (p-post++ $k))
                                                    ;; $$result_ref .= chr($bytes[$k] | 0x80)
                                                                                                        (p-.= (p-cast-$ $result_ref) (p-chr (p-bit-or (p-aref @bytes $k) #x80)))
                                                    
                                                  )
                                                )
                                                
                                                ;; $$result_ref .= chr($bytes[-1])
                                                                                                (p-.= (p-cast-$ $result_ref) (p-chr (p-aref @bytes -1)))
                                                
                                              )
                                            )
                                          )
                                          
                                        )
                                      )
                                    )
                                    
                                  )
                                )
                                nil
                              )
                            )
                          )
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if (defined $grpbeg) {             my $inner = substr($tmpl, $grpbeg, $grpend - $grpbeg);             # Check: group must not start with a count             my $gti = _pack_skip_ws($inner, 0);             if ($gti < length($inner)) {                 my $fc = substr($inner, $gti, 1);                 die "\(\)-group starts with a count in pack\n" if $fc =~ /^[\d\*\[]/;             }             if ($star) {                 while ($$ai_ref < $nargs) {                     my $ai_before = $$ai_ref;                     my $iter_base = length($$result_ref);                     _pack_tmpl($inner, $ai_ref, $args_ref, $result_ref, $be, $le, $iter_base, $depth + 1);                     last if $$ai_ref == $ai_before;  # no progress: avoid infinite loop                 }             } else {                 for (my $r = 0; $r < $nrep; $r++) {                     my $iter_base = length($$result_ref);                     _pack_tmpl($inner, $ai_ref, $args_ref, $result_ref, $be, $le, $iter_base, $depth + 1);                 }             }             next;         }
                    ;; if (defined $grpbeg)
                    (p-if (p-defined $grpbeg)
                      (progn
                        (let (($inner (make-p-box nil)) ($gti (make-p-box nil)) ($fc (make-p-box nil)) ($ai_before (make-p-box nil)) ($iter_base (make-p-box nil)) ($r (make-p-box nil)))
                          ;; my $inner = substr($tmpl, $grpbeg, $grpend - $grpbeg)
                                                    (p-my-= $inner (p-substr $tmpl $grpbeg (p-- $grpend $grpbeg)))
                          
                          ;; my $gti = _pack_skip_ws($inner, 0)
                                                    (p-my-= $gti (let ((*wantarray* nil)) (pl-_pack_skip_ws $inner 0)))
                          
                          ;; if ($gti < length($inner)) {                 my $fc = substr($inner, $gti, 1);                 die "\(\)-group starts with a count in pack\n" if $fc =~ /^[\d\*\[]/;             }
                          ;; if ($gti < length($inner))
                          (p-if (p-< $gti (p-length $inner))
                            (progn
                              (let (($fc (make-p-box nil)))
                                (let ((--pcl-if-ret--10 nil))
                                  ;; my $fc = substr($inner, $gti, 1)
                                                                    (p-my-= $fc (p-substr $inner $gti 1))
                                  
                                  ;; die "\(\)-group starts with a count in pack\n" if $fc =~ /^[\d\*\[]/
                                  (p-if (setf --pcl-if-ret--10 (let ((*wantarray* nil)) (p-=~ $fc (p-regex "/^[\\d\\*\\[]/"))))
                                    (setf --pcl-if-ret--10 (p-die :loc "- line 520" "()-group starts with a count in pack
"))
                                    nil)
                                  
                                --pcl-if-ret--10)
                              )
                            )
                            nil
                          )
                          
                          ;; if ($star) {                 while ($$ai_ref < $nargs) {                     my $ai_before = $$ai_ref;                     my $iter_base = length($$result_ref);                     _pack_tmpl($inner, $ai_ref, $args_ref, $result_ref, $be, $le, $iter_base, $depth + 1);                     last if $$ai_ref == $ai_before;  # no progress: avoid infinite loop                 }             } else {                 for (my $r = 0; $r < $nrep; $r++) {                     my $iter_base = length($$result_ref);                     _pack_tmpl($inner, $ai_ref, $args_ref, $result_ref, $be, $le, $iter_base, $depth + 1);                 }             }
                          ;; if ($star)
                          (p-if $star
                            (progn
                              (let (($ai_before (make-p-box nil)) ($iter_base (make-p-box nil)))
                                ;; while ($$ai_ref < $nargs) {                     my $ai_before = $$ai_ref;                     my $iter_base = length($$result_ref);                     _pack_tmpl($inner, $ai_ref, $args_ref, $result_ref, $be, $le, $iter_base, $depth + 1);                     last if $$ai_ref == $ai_before;  # no progress: avoid infinite loop                 }
                                (p-while (p-< (p-cast-$ $ai_ref) $nargs)
                                  (let (($ai_before (make-p-box nil)) ($iter_base (make-p-box nil)))
                                    ;; my $ai_before = $$ai_ref
                                                                        (p-my-= $ai_before (p-cast-$ $ai_ref))
                                    
                                    ;; my $iter_base = length($$result_ref)
                                                                        (p-my-= $iter_base (p-length (p-cast-$ $result_ref)))
                                    
                                    ;; _pack_tmpl($inner, $ai_ref, $args_ref, $result_ref, $be, $le, $iter_base, $depth + 1)
                                                                        (pl-_pack_tmpl $inner $ai_ref $args_ref $result_ref $be $le $iter_base (p-+ $depth 1))
                                    
                                    ;; last if $$ai_ref == $ai_before
                                    (p-if (p-== (p-cast-$ $ai_ref) $ai_before) (p-last))
                                    
                                  )
                                )
                                
                              )
                            )
                            ;; else
                            (progn
                              (let (($r (make-p-box nil)) ($iter_base (make-p-box nil)))
                                ;; for (my $r = 0; $r < $nrep; $r++) {                     my $iter_base = length($$result_ref);                     _pack_tmpl($inner, $ai_ref, $args_ref, $result_ref, $be, $le, $iter_base, $depth + 1);                 }
                                (let (($r (make-p-box nil)))
                                  (p-for (                                (p-my-= $r 0))
                                          (                                (p-< $r $nrep))
                                          (                                (p-post++ $r))
                                    (let (($iter_base (make-p-box nil)))
                                      ;; my $iter_base = length($$result_ref)
                                                                            (p-my-= $iter_base (p-length (p-cast-$ $result_ref)))
                                      
                                      ;; _pack_tmpl($inner, $ai_ref, $args_ref, $result_ref, $be, $le, $iter_base, $depth + 1)
                                                                            (pl-_pack_tmpl $inner $ai_ref $args_ref $result_ref $be $le $iter_base (p-+ $depth 1))
                                      
                                    )
                                  )
                                )
                                
                              )
                            )
                          )
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if ($ch eq 'x') {             if ($bang) {                 # x!N: pad to N-byte alignment                 my $n = $nrep > 0 ? $nrep : 1;                 my $cur = length($$result_ref);                 my $pad = ($n - ($cur % $n)) % $n;                 $$result_ref .= chr(0) x $pad;             } else {                 $$result_ref .= chr(0) x $nrep;             }             next;         }
                    ;; if ($ch eq 'x')
                    (p-if (p-str-eq $ch "x")
                      (progn
                        (let (($n (make-p-box nil)) ($cur (make-p-box nil)) ($pad (make-p-box nil)))
                          ;; if ($bang) {                 # x!N: pad to N-byte alignment                 my $n = $nrep > 0 ? $nrep : 1;                 my $cur = length($$result_ref);                 my $pad = ($n - ($cur % $n)) % $n;                 $$result_ref .= chr(0) x $pad;             } else {                 $$result_ref .= chr(0) x $nrep;             }
                          ;; if ($bang)
                          (p-if $bang
                            (progn
                              (let (($n (make-p-box nil)) ($cur (make-p-box nil)) ($pad (make-p-box nil)))
                                ;; my $n = $nrep > 0 ? $nrep : 1
                                                                (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                
                                ;; my $cur = length($$result_ref)
                                                                (p-my-= $cur (p-length (p-cast-$ $result_ref)))
                                
                                ;; my $pad = ($n - ($cur % $n)) % $n
                                                                (p-my-= $pad (p-% (p-- $n (p-% $cur $n)) $n))
                                
                                ;; $$result_ref .= chr(0) x $pad
                                                                (p-.= (p-cast-$ $result_ref) (p-str-x (p-chr 0) $pad))
                                
                              )
                            )
                            ;; else
                            (progn
                              ;; $$result_ref .= chr(0) x $nrep
                                                            (p-.= (p-cast-$ $result_ref) (p-str-x (p-chr 0) $nrep))
                              
                            )
                          )
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if ($ch eq 'X') {             if ($bang) {                 # X!N: truncate back to N-byte alignment                 my $n = $nrep > 0 ? $nrep : 1;                 my $cur = length($$result_ref);                 $$result_ref = substr($$result_ref, 0, int($cur/$n)*$n);             } else {                 my $fp = length($$result_ref) - $nrep;                 $$result_ref = substr($$result_ref, 0, $fp < 0 ? 0 : $fp);             }             next;         }
                    ;; if ($ch eq 'X')
                    (p-if (p-str-eq $ch "X")
                      (progn
                        (let (($n (make-p-box nil)) ($cur (make-p-box nil)) ($fp (make-p-box nil)))
                          ;; if ($bang) {                 # X!N: truncate back to N-byte alignment                 my $n = $nrep > 0 ? $nrep : 1;                 my $cur = length($$result_ref);                 $$result_ref = substr($$result_ref, 0, int($cur/$n)*$n);             } else {                 my $fp = length($$result_ref) - $nrep;                 $$result_ref = substr($$result_ref, 0, $fp < 0 ? 0 : $fp);             }
                          ;; if ($bang)
                          (p-if $bang
                            (progn
                              (let (($n (make-p-box nil)) ($cur (make-p-box nil)))
                                ;; my $n = $nrep > 0 ? $nrep : 1
                                                                (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                
                                ;; my $cur = length($$result_ref)
                                                                (p-my-= $cur (p-length (p-cast-$ $result_ref)))
                                
                                ;; $$result_ref = substr($$result_ref, 0, int($cur/$n)*$n)
                                                                (p-setf (p-cast-$ $result_ref) (p-substr (p-cast-$ $result_ref) 0 (p-* (p-int (p-/ $cur $n)) $n)))
                                
                              )
                            )
                            ;; else
                            (progn
                              (let (($fp (make-p-box nil)))
                                ;; my $fp = length($$result_ref) - $nrep
                                                                (p-my-= $fp (p-- (p-length (p-cast-$ $result_ref)) $nrep))
                                
                                ;; $$result_ref = substr($$result_ref, 0, $fp < 0 ? 0 : $fp)
                                                                (p-setf (p-cast-$ $result_ref) (p-substr (p-cast-$ $result_ref) 0 (p-if (p-< $fp 0) 0 $fp)))
                                
                              )
                            )
                          )
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if ($ch eq '@') {             # @N: relative to group start (out_base). @!N: absolute byte position.             my $n = defined($count) ? $count : 0;             my $t = $bang ? $n : $out_base + $n;             if (length($$result_ref) < $t) { $$result_ref .= chr(0) x ($t - length($$result_ref)) }             elsif (length($$result_ref) > $t) { $$result_ref = substr($$result_ref, 0, $t) }             next;         }
                    ;; if ($ch eq '@')
                    (p-if (p-str-eq $ch "@")
                      (progn
                        (let (($n (make-p-box nil)) ($t (make-p-box nil)))
                          ;; my $n = defined($count) ? $count : 0
                                                    (p-my-= $n (p-if (p-defined $count) $count 0))
                          
                          ;; my $t = $bang ? $n : $out_base + $n
                                                    (p-my-= $t (p-if $bang $n (p-+ $out_base $n)))
                          
                          ;; if (length($$result_ref) < $t) { $$result_ref .= chr(0) x ($t - length($$result_ref)) }             elsif (length($$result_ref) > $t) { $$result_ref = substr($$result_ref, 0, $t) }
                          ;; if (length($$result_ref) < $t)
                          (p-if (p-< (p-length (p-cast-$ $result_ref)) $t)
                            (progn
                              ;; $$result_ref .= chr(0) x ($t - length($$result_ref))
                                                            (p-.= (p-cast-$ $result_ref) (p-str-x (p-chr 0) (p-- $t (p-length (p-cast-$ $result_ref)))))
                              
                            )
                            ;; elsif (length($$result_ref) > $t)
                            (p-if (p-> (p-length (p-cast-$ $result_ref)) $t)
                              (progn
                                ;; $$result_ref = substr($$result_ref, 0, $t)
                                                                (p-setf (p-cast-$ $result_ref) (p-substr (p-cast-$ $result_ref) 0 $t))
                                
                              )
                              nil
                            )
                          )
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if ($ch eq '.') {             # Position format: reads target from arg list.             # .* = absolute; . or .N = relative to current group's start (out_base).             my $tgt = ($$ai_ref < $nargs) ? int(($args_ref->[$$ai_ref++] // 0) + 0) : 0;             my $abs_tgt = $star ? $tgt : $out_base + $tgt;             my $cur = length($$result_ref);             if ($cur < $abs_tgt) { $$result_ref .= chr(0) x ($abs_tgt - $cur) }             elsif ($cur > $abs_tgt) { $$result_ref = substr($$result_ref, 0, $abs_tgt) }             next;         }
                    ;; if ($ch eq '.')
                    (p-if (p-str-eq $ch ".")
                      (progn
                        (let (($tgt (make-p-box nil)) ($abs_tgt (make-p-box nil)) ($cur (make-p-box nil)))
                          ;; my $tgt = ($$ai_ref < $nargs) ? int(($args_ref->[$$ai_ref++] // 0) + 0) : 0
                                                    (p-my-= $tgt (p-if (p-< (p-cast-$ $ai_ref) $nargs) (p-int (p-+ (p-// (p-aref-deref $args_ref (p-post++ (p-cast-$ $ai_ref))) 0) 0)) 0))
                          
                          ;; my $abs_tgt = $star ? $tgt : $out_base + $tgt
                                                    (p-my-= $abs_tgt (p-if $star $tgt (p-+ $out_base $tgt)))
                          
                          ;; my $cur = length($$result_ref)
                                                    (p-my-= $cur (p-length (p-cast-$ $result_ref)))
                          
                          ;; if ($cur < $abs_tgt) { $$result_ref .= chr(0) x ($abs_tgt - $cur) }             elsif ($cur > $abs_tgt) { $$result_ref = substr($$result_ref, 0, $abs_tgt) }
                          ;; if ($cur < $abs_tgt)
                          (p-if (p-< $cur $abs_tgt)
                            (progn
                              ;; $$result_ref .= chr(0) x ($abs_tgt - $cur)
                                                            (p-.= (p-cast-$ $result_ref) (p-str-x (p-chr 0) (p-- $abs_tgt $cur)))
                              
                            )
                            ;; elsif ($cur > $abs_tgt)
                            (p-if (p-> $cur $abs_tgt)
                              (progn
                                ;; $$result_ref = substr($$result_ref, 0, $abs_tgt)
                                                                (p-setf (p-cast-$ $result_ref) (p-substr (p-cast-$ $result_ref) 0 $abs_tgt))
                                
                              )
                              nil
                            )
                          )
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if ($ch eq 'p' || $ch eq 'P' || $ch eq 'D') {             die "Invalid type '$ch' in pack\n";         }
                    ;; if ($ch eq 'p' || $ch eq 'P' || $ch eq 'D')
                    (p-if (p-|| (p-|| (p-str-eq $ch "p") (p-str-eq $ch "P")) (p-str-eq $ch "D"))
                      (progn
                        ;; die "Invalid type '$ch' in pack\n"
                                                (p-die :loc "- line 582" (p-string-concat "Invalid type '" $ch "' in pack
"))
                        
                      )
                      nil
                    )
                    
                    ;; $nrep = $nargs - $$ai_ref if $star
                    (let ((*wantarray* :void)) (p-if $star (p-my-= $nrep (p-- $nargs (p-cast-$ $ai_ref)))))
                    
                    ;; my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang)
                                        (let ((*wantarray* nil)) (p-list-= (vector $nb $sig $dbe) (let ((*wantarray* t)) (pl-_pack_type_info $ch $bang))))
                    
                    ;; if ($nb) {             my $be2 = $be ? 1 : ($le ? 0 : $dbe);             for (my $r = 0; $r < $nrep; $r++) {                 my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;                 my $nv = $v + 0;                 die "Cannot pack NaN in pack\n" if $nv != $nv;                 die "Cannot pack " . ($nv < 0 ? "-Inf" : "Inf") . " in pack\n" if $nv != 0 && $nv == $nv * 2;                 $$result_ref .= _pack_emit_int(int($nv), $nb, $sig, $be2);             }             next;         }
                    ;; if ($nb)
                    (p-if $nb
                      (progn
                        (let (($be2 (make-p-box nil)) ($r (make-p-box nil)) ($v (make-p-box nil)) ($nv (make-p-box nil)))
                          ;; my $be2 = $be ? 1 : ($le ? 0 : $dbe)
                                                    (p-my-= $be2 (p-if $be 1 (p-if $le 0 $dbe)))
                          
                          ;; for (my $r = 0; $r < $nrep; $r++) {                 my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;                 my $nv = $v + 0;                 die "Cannot pack NaN in pack\n" if $nv != $nv;                 die "Cannot pack " . ($nv < 0 ? "-Inf" : "Inf") . " in pack\n" if $nv != 0 && $nv == $nv * 2;                 $$result_ref .= _pack_emit_int(int($nv), $nb, $sig, $be2);             }
                          (let (($r (make-p-box nil)))
                            (p-for (                          (p-my-= $r 0))
                                    (                          (p-< $r $nrep))
                                    (                          (p-post++ $r))
                              (let (($v (make-p-box nil)) ($nv (make-p-box nil)))
                                ;; my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0
                                                                (p-my-= $v (p-if (p-< (p-cast-$ $ai_ref) $nargs) (p-// (p-aref-deref $args_ref (p-post++ (p-cast-$ $ai_ref))) 0) 0))
                                
                                ;; my $nv = $v + 0
                                                                (p-my-= $nv (p-+ $v 0))
                                
                                ;; die "Cannot pack NaN in pack\n" if $nv != $nv
                                (let ((*wantarray* :void)) (p-if (p-!= $nv $nv) (p-die :loc "- line 595" "Cannot pack NaN in pack
")))
                                
                                ;; die "Cannot pack " . ($nv < 0 ? "-Inf" : "Inf") . " in pack\n" if $nv != 0 && $nv == $nv * 2
                                (let ((*wantarray* :void)) (p-if (p-&& (p-!= $nv 0) (p-== $nv (p-* $nv 2))) (p-die :loc "- line 596" (p-. (p-. "Cannot pack " (p-if (p-< $nv 0) "-Inf" "Inf")) " in pack
"))))
                                
                                ;; $$result_ref .= _pack_emit_int(int($nv), $nb, $sig, $be2)
                                                                (p-.= (p-cast-$ $result_ref) (pl-_pack_emit_int (p-int $nv) $nb $sig $be2))
                                
                              )
                            )
                          )
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if ($ch eq 'f') {             my $be2 = $be ? 1 : ($le ? 0 : 0);             for (my $r = 0; $r < $nrep; $r++) {                 my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;                 $$result_ref .= _pack_float32($v, $be2);             }             next;         }
                    ;; if ($ch eq 'f')
                    (p-if (p-str-eq $ch "f")
                      (progn
                        (let (($be2 (make-p-box nil)) ($r (make-p-box nil)) ($v (make-p-box nil)))
                          ;; my $be2 = $be ? 1 : ($le ? 0 : 0)
                                                    (p-my-= $be2 (p-if $be 1 (p-if $le 0 0)))
                          
                          ;; for (my $r = 0; $r < $nrep; $r++) {                 my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;                 $$result_ref .= _pack_float32($v, $be2);             }
                          (let (($r (make-p-box nil)))
                            (p-for (                          (p-my-= $r 0))
                                    (                          (p-< $r $nrep))
                                    (                          (p-post++ $r))
                              (let (($v (make-p-box nil)))
                                ;; my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0
                                                                (p-my-= $v (p-if (p-< (p-cast-$ $ai_ref) $nargs) (p-// (p-aref-deref $args_ref (p-post++ (p-cast-$ $ai_ref))) 0) 0))
                                
                                ;; $$result_ref .= _pack_float32($v, $be2)
                                                                (p-.= (p-cast-$ $result_ref) (pl-_pack_float32 $v $be2))
                                
                              )
                            )
                          )
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if ($ch eq 'd' || $ch eq 'F') {             my $be2 = $be ? 1 : ($le ? 0 : 0);             for (my $r = 0; $r < $nrep; $r++) {                 my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;                 $$result_ref .= _pack_float64($v, $be2);             }             next;         }
                    ;; if ($ch eq 'd' || $ch eq 'F')
                    (p-if (p-|| (p-str-eq $ch "d") (p-str-eq $ch "F"))
                      (progn
                        (let (($be2 (make-p-box nil)) ($r (make-p-box nil)) ($v (make-p-box nil)))
                          ;; my $be2 = $be ? 1 : ($le ? 0 : 0)
                                                    (p-my-= $be2 (p-if $be 1 (p-if $le 0 0)))
                          
                          ;; for (my $r = 0; $r < $nrep; $r++) {                 my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;                 $$result_ref .= _pack_float64($v, $be2);             }
                          (let (($r (make-p-box nil)))
                            (p-for (                          (p-my-= $r 0))
                                    (                          (p-< $r $nrep))
                                    (                          (p-post++ $r))
                              (let (($v (make-p-box nil)))
                                ;; my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0
                                                                (p-my-= $v (p-if (p-< (p-cast-$ $ai_ref) $nargs) (p-// (p-aref-deref $args_ref (p-post++ (p-cast-$ $ai_ref))) 0) 0))
                                
                                ;; $$result_ref .= _pack_float64($v, $be2)
                                                                (p-.= (p-cast-$ $result_ref) (pl-_pack_float64 $v $be2))
                                
                              )
                            )
                          )
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if ($ch eq 'a'||$ch eq 'A'||$ch eq 'Z'||$ch eq 'b'||$ch eq 'B'||             $ch eq 'H'||$ch eq 'h'||$ch eq 'u') {             my $arg = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // '') : '';             _pack_str_one($ch, $arg, $nrep, $star, $result_ref);             next;         }
                    ;; if ($ch eq 'a'||$ch eq 'A'||$ch eq 'Z'||$ch eq 'b'||$ch eq 'B'||
;;             $ch eq 'H'||$ch eq 'h'||$ch eq 'u')
                    (p-if (p-|| (p-|| (p-|| (p-|| (p-|| (p-|| (p-|| (p-str-eq $ch "a") (p-str-eq $ch "A")) (p-str-eq $ch "Z")) (p-str-eq $ch "b")) (p-str-eq $ch "B")) (p-str-eq $ch "H")) (p-str-eq $ch "h")) (p-str-eq $ch "u"))
                      (progn
                        (let (($arg (make-p-box nil)))
                          ;; my $arg = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // '') : ''
                                                    (p-my-= $arg (p-if (p-< (p-cast-$ $ai_ref) $nargs) (p-// (p-aref-deref $args_ref (p-post++ (p-cast-$ $ai_ref))) "") ""))
                          
                          ;; _pack_str_one($ch, $arg, $nrep, $star, $result_ref)
                          (let ((*wantarray* :void))                           (let ((*wantarray* :void)) (pl-_pack_str_one $ch $arg $nrep $star $result_ref)))
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if ($ch eq 'U') {             for (my $r = 0; $r < $nrep; $r++) {                 my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;                 my $nv = $v + 0;                 die "Cannot pack NaN in pack\n" if $nv != $nv;                 die "Cannot pack " . ($nv < 0 ? "-Inf" : "Inf") . " in pack\n" if $nv != 0 && $nv == $nv * 2;                 _pack_utf8_char(int($nv), $result_ref);             }             next;         }
                    ;; if ($ch eq 'U')
                    (p-if (p-str-eq $ch "U")
                      (progn
                        (let (($r (make-p-box nil)) ($v (make-p-box nil)) ($nv (make-p-box nil)))
                          ;; for (my $r = 0; $r < $nrep; $r++) {                 my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;                 my $nv = $v + 0;                 die "Cannot pack NaN in pack\n" if $nv != $nv;                 die "Cannot pack " . ($nv < 0 ? "-Inf" : "Inf") . " in pack\n" if $nv != 0 && $nv == $nv * 2;                 _pack_utf8_char(int($nv), $result_ref);             }
                          (let (($r (make-p-box nil)))
                            (p-for (                          (p-my-= $r 0))
                                    (                          (p-< $r $nrep))
                                    (                          (p-post++ $r))
                              (let (($v (make-p-box nil)) ($nv (make-p-box nil)))
                                ;; my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0
                                                                (p-my-= $v (p-if (p-< (p-cast-$ $ai_ref) $nargs) (p-// (p-aref-deref $args_ref (p-post++ (p-cast-$ $ai_ref))) 0) 0))
                                
                                ;; my $nv = $v + 0
                                                                (p-my-= $nv (p-+ $v 0))
                                
                                ;; die "Cannot pack NaN in pack\n" if $nv != $nv
                                (let ((*wantarray* :void)) (p-if (p-!= $nv $nv) (p-die :loc "- line 633" "Cannot pack NaN in pack
")))
                                
                                ;; die "Cannot pack " . ($nv < 0 ? "-Inf" : "Inf") . " in pack\n" if $nv != 0 && $nv == $nv * 2
                                (let ((*wantarray* :void)) (p-if (p-&& (p-!= $nv 0) (p-== $nv (p-* $nv 2))) (p-die :loc "- line 634" (p-. (p-. "Cannot pack " (p-if (p-< $nv 0) "-Inf" "Inf")) " in pack
"))))
                                
                                ;; _pack_utf8_char(int($nv), $result_ref)
                                                                (pl-_pack_utf8_char (p-int $nv) $result_ref)
                                
                              )
                            )
                          )
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if ($ch eq 'W') {             for (my $r = 0; $r < $nrep; $r++) {                 my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;                 my $nv = $v + 0;                 die "Cannot pack NaN in pack\n" if $nv != $nv;                 die "Cannot pack " . ($nv < 0 ? "-Inf" : "Inf") . " in pack\n" if $nv != 0 && $nv == $nv * 2;                 $$result_ref .= chr(int($nv));             }             next;         }
                    ;; if ($ch eq 'W')
                    (p-if (p-str-eq $ch "W")
                      (progn
                        (let (($r (make-p-box nil)) ($v (make-p-box nil)) ($nv (make-p-box nil)))
                          ;; for (my $r = 0; $r < $nrep; $r++) {                 my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;                 my $nv = $v + 0;                 die "Cannot pack NaN in pack\n" if $nv != $nv;                 die "Cannot pack " . ($nv < 0 ? "-Inf" : "Inf") . " in pack\n" if $nv != 0 && $nv == $nv * 2;                 $$result_ref .= chr(int($nv));             }
                          (let (($r (make-p-box nil)))
                            (p-for (                          (p-my-= $r 0))
                                    (                          (p-< $r $nrep))
                                    (                          (p-post++ $r))
                              (let (($v (make-p-box nil)) ($nv (make-p-box nil)))
                                ;; my $v = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0
                                                                (p-my-= $v (p-if (p-< (p-cast-$ $ai_ref) $nargs) (p-// (p-aref-deref $args_ref (p-post++ (p-cast-$ $ai_ref))) 0) 0))
                                
                                ;; my $nv = $v + 0
                                                                (p-my-= $nv (p-+ $v 0))
                                
                                ;; die "Cannot pack NaN in pack\n" if $nv != $nv
                                (let ((*wantarray* :void)) (p-if (p-!= $nv $nv) (p-die :loc "- line 643" "Cannot pack NaN in pack
")))
                                
                                ;; die "Cannot pack " . ($nv < 0 ? "-Inf" : "Inf") . " in pack\n" if $nv != 0 && $nv == $nv * 2
                                (let ((*wantarray* :void)) (p-if (p-&& (p-!= $nv 0) (p-== $nv (p-* $nv 2))) (p-die :loc "- line 644" (p-. (p-. "Cannot pack " (p-if (p-< $nv 0) "-Inf" "Inf")) " in pack
"))))
                                
                                ;; $$result_ref .= chr(int($nv))
                                                                (p-.= (p-cast-$ $result_ref) (p-chr (p-int $nv)))
                                
                              )
                            )
                          )
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if ($ch eq 'w') {             for (my $r = 0; $r < $nrep; $r++) {                 my $raw = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;                 my $orig_s = "$raw";  # stringify BEFORE numeric coercion                 my $v = $raw + 0;                 die "Cannot compress NaN in pack\n" if $v != $v;                 die "Cannot compress -Inf in pack\n" if $v < 0 && $v == $v * 2;                 die "Cannot compress negative numbers in pack\n" if $v < 0;                 die "Cannot compress Inf in pack\n" if $v != 0 && $v == $v * 2;                 die "Can only compress unsigned integers in pack\n" if $v != int($v);                 # Perl also rejects float-notation strings (e.g. "1e21") when value > UV_MAX.                 # Pure-digit strings (even > UV_MAX) succeed; so does any non-string source.                 # In CL: pure-digit strings â exact bignum; e/E strings â double-float.                 # The $v != int($v) check cannot distinguish a large integer-valued float from                 # an exact bignum, so check the original string form for scientific notation.                 die "Can only compress unsigned integers in pack\n"                     if $orig_s =~ /[eE]/ && $v >= 2**64;                 $v = int($v);                 if ($v == 0) { $$result_ref .= chr(0); next }                 my @bytes;                 while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                 for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                 $$result_ref .= chr($bytes[-1]);             }             next;         }
                    ;; if ($ch eq 'w')
                    (p-if (p-str-eq $ch "w")
                      (progn
                        (let (($r (make-p-box nil)) ($raw (make-p-box nil)) ($orig_s (make-p-box nil)) ($v (make-p-box nil)) (@bytes (make-array 0 :adjustable t :fill-pointer 0)) ($k (make-p-box nil)))
                          ;; for (my $r = 0; $r < $nrep; $r++) {                 my $raw = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0;                 my $orig_s = "$raw";  # stringify BEFORE numeric coercion                 my $v = $raw + 0;                 die "Cannot compress NaN in pack\n" if $v != $v;                 die "Cannot compress -Inf in pack\n" if $v < 0 && $v == $v * 2;                 die "Cannot compress negative numbers in pack\n" if $v < 0;                 die "Cannot compress Inf in pack\n" if $v != 0 && $v == $v * 2;                 die "Can only compress unsigned integers in pack\n" if $v != int($v);                 # Perl also rejects float-notation strings (e.g. "1e21") when value > UV_MAX.                 # Pure-digit strings (even > UV_MAX) succeed; so does any non-string source.                 # In CL: pure-digit strings â exact bignum; e/E strings â double-float.                 # The $v != int($v) check cannot distinguish a large integer-valued float from                 # an exact bignum, so check the original string form for scientific notation.                 die "Can only compress unsigned integers in pack\n"                     if $orig_s =~ /[eE]/ && $v >= 2**64;                 $v = int($v);                 if ($v == 0) { $$result_ref .= chr(0); next }                 my @bytes;                 while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }                 for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }                 $$result_ref .= chr($bytes[-1]);             }
                          (let (($r (make-p-box nil)))
                            (p-for (                          (p-my-= $r 0))
                                    (                          (p-< $r $nrep))
                                    (                          (p-post++ $r))
                              (let (($raw (make-p-box nil)) ($orig_s (make-p-box nil)) ($v (make-p-box nil)) (@bytes (make-array 0 :adjustable t :fill-pointer 0)) ($k (make-p-box nil)))
                                ;; my $raw = ($$ai_ref < $nargs) ? ($args_ref->[$$ai_ref++] // 0) : 0
                                                                (p-my-= $raw (p-if (p-< (p-cast-$ $ai_ref) $nargs) (p-// (p-aref-deref $args_ref (p-post++ (p-cast-$ $ai_ref))) 0) 0))
                                
                                ;; my $orig_s = "$raw"
                                                                (p-my-= $orig_s $raw)
                                
                                ;; my $v = $raw + 0
                                                                (p-my-= $v (p-+ $raw 0))
                                
                                ;; die "Cannot compress NaN in pack\n" if $v != $v
                                (let ((*wantarray* :void)) (p-if (p-!= $v $v) (p-die :loc "- line 654" "Cannot compress NaN in pack
")))
                                
                                ;; die "Cannot compress -Inf in pack\n" if $v < 0 && $v == $v * 2
                                (let ((*wantarray* :void)) (p-if (p-&& (p-< $v 0) (p-== $v (p-* $v 2))) (p-die :loc "- line 655" "Cannot compress -Inf in pack
")))
                                
                                ;; die "Cannot compress negative numbers in pack\n" if $v < 0
                                (let ((*wantarray* :void)) (p-if (p-< $v 0) (p-die :loc "- line 656" "Cannot compress negative numbers in pack
")))
                                
                                ;; die "Cannot compress Inf in pack\n" if $v != 0 && $v == $v * 2
                                (let ((*wantarray* :void)) (p-if (p-&& (p-!= $v 0) (p-== $v (p-* $v 2))) (p-die :loc "- line 657" "Cannot compress Inf in pack
")))
                                
                                ;; die "Can only compress unsigned integers in pack\n" if $v != int($v)
                                (let ((*wantarray* :void)) (p-if (p-!= $v (p-int $v)) (p-die :loc "- line 658" "Can only compress unsigned integers in pack
")))
                                
                                ;; die "Can only compress unsigned integers in pack\n"                     if $orig_s =~ /[eE]/ && $v >= 2**64
                                (let ((*wantarray* :void)) (p-if (p-&& (let ((*wantarray* nil)) (p-=~ $orig_s (p-regex "/[eE]/"))) (p->= $v (p-** 2 64))) (p-die :loc "- line 664" "Can only compress unsigned integers in pack
")))
                                
                                ;; $v = int($v)
                                (let ((*wantarray* :void))                                 (p-my-= $v (p-int $v)))
                                
                                ;; if ($v == 0) { $$result_ref .= chr(0); next }
                                ;; if ($v == 0)
                                (p-if (p-== $v 0)
                                  (progn
                                    ;; $$result_ref .= chr(0)
                                    (let ((*wantarray* :void))                                     (p-.= (p-cast-$ $result_ref) (p-chr 0)))
                                    
                                    ;; next
                                                                        (p-next)
                                    
                                  )
                                  nil
                                )
                                
                                ;; my @bytes (bare declaration)
                                
                                ;; while ($v > 0) { unshift @bytes, ($v & 0x7F); $v >>= 7 }
                                (p-while (p-> $v 0)
                                  ;; unshift @bytes, ($v & 0x7F)
                                  (let ((*wantarray* :void))                                   (p-unshift @bytes (p-bit-and $v #x7F)))
                                  
                                  ;; $v >>= 7
                                                                    (p->>= $v 7)
                                  
                                )
                                
                                ;; for (my $k = 0; $k < $#bytes; $k++) { $$result_ref .= chr($bytes[$k] | 0x80) }
                                (let (($k (make-p-box nil)))
                                  (p-for (                                (p-my-= $k 0))
                                          (                                (p-< $k (p-array-last-index @bytes)))
                                          (                                (p-post++ $k))
                                    ;; $$result_ref .= chr($bytes[$k] | 0x80)
                                                                        (p-.= (p-cast-$ $result_ref) (p-chr (p-bit-or (p-aref @bytes $k) #x80)))
                                    
                                  )
                                )
                                
                                ;; $$result_ref .= chr($bytes[-1])
                                                                (p-.= (p-cast-$ $result_ref) (p-chr (p-aref @bytes -1)))
                                
                              )
                            )
                          )
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; die "Invalid type '/' in pack\n" if $ch eq '/'
                    (let ((*wantarray* :void)) (p-if (p-str-eq $ch "/") (p-die :loc "- line 676" "Invalid type '/' in pack
")))
                    
                    ;; die "Invalid type '$ch' in pack\n"
                                        (p-die :loc "- line 677" (p-string-concat "Invalid type '" $ch "' in pack
"))
                    
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

;; sub _pack_check_brackets { ... }
(p-sub pl-_pack_check_brackets (&rest %_args)
  (p-args-body
    (block nil
      (let (($i (make-p-box nil)) ($c (make-p-box nil)))
        (let (($tmpl (make-p-box nil)))
          ;; my ($tmpl) = @_
                    (let ((*wantarray* nil)) (p-list-= (vector $tmpl) @_))
          
          (let (($n_open (make-p-box nil)) ($n_close (make-p-box nil)))
            ;; my ($n_open, $n_close) = (0, 0)
                        (let ((*wantarray* nil)) (p-list-= (vector $n_open $n_close) (vector 0 0)))
            
            (let (($tlen (make-p-box nil)))
              ;; my $tlen = length($tmpl)
                            (p-my-= $tlen (p-length $tmpl))
              
              ;; for (my $i = 0; $i < $tlen; $i++) {         my $c = substr($tmpl, $i, 1);         if    ($c eq '[') { $n_open++ }         elsif ($c eq ']') { $n_close++ }     }
              (let (($i (make-p-box nil)))
                (p-for (              (p-my-= $i 0))
                        (              (p-< $i $tlen))
                        (              (p-post++ $i))
                  (let (($c (make-p-box nil)))
                    (let ((--pcl-if-ret--11 nil))
                      ;; my $c = substr($tmpl, $i, 1)
                                            (p-my-= $c (p-substr $tmpl $i 1))
                      
                      ;; if    ($c eq '[') { $n_open++ }         elsif ($c eq ']') { $n_close++ }
                      ;; if ($c eq '[')
                      (p-if (setf --pcl-if-ret--11 (p-str-eq $c "["))
                        (progn
                          ;; $n_open++
                          (setf --pcl-if-ret--11 (p-post++ $n_open))
                          
                        )
                        ;; elsif ($c eq ']')
                        (p-if (setf --pcl-if-ret--11 (p-str-eq $c "]"))
                          (progn
                            ;; $n_close++
                            (setf --pcl-if-ret--11 (p-post++ $n_close))
                            
                          )
                          nil
                        )
                      )
                      
                    --pcl-if-ret--11)
                  )
                )
              )
              
              ;; die "No group ending character ']' found in template\n" if $n_open > $n_close
              (let ((*wantarray* :void)) (p-if (p-> $n_open $n_close) (p-die :loc "- line 690" "No group ending character ']' found in template
")))
              
              ;; return unless $n_open > 0
              (let ((*wantarray* :void)) (p-unless (p-> $n_open 0) (p-return)))
              
              (let ((@stk (make-array 0 :adjustable t :fill-pointer 0)))
                ;; my @stk = ()
                                (p-array-= @stk (vector ))
                
                ;; for (my $i = 0; $i < $tlen; $i++) {         my $c = substr($tmpl, $i, 1);         if    ($c eq '[') { push @stk, '[' }         elsif ($c eq '(') { push @stk, '(' }         elsif ($c eq ']') {             die "Mismatched brackets in template\n"                 if !@stk || $stk[-1] ne '[';             pop @stk;         }         elsif ($c eq ')') {             pop @stk if @stk && $stk[-1] eq '(';         }     }
                (let (($i (make-p-box nil)))
                  (p-for (                (p-my-= $i 0))
                          (                (p-< $i $tlen))
                          (                (p-post++ $i))
                    (let (($c (make-p-box nil)))
                      (let ((--pcl-if-ret--12 nil))
                        ;; my $c = substr($tmpl, $i, 1)
                                                (p-my-= $c (p-substr $tmpl $i 1))
                        
                        ;; if    ($c eq '[') { push @stk, '[' }         elsif ($c eq '(') { push @stk, '(' }         elsif ($c eq ']') {             die "Mismatched brackets in template\n"                 if !@stk || $stk[-1] ne '[';             pop @stk;         }         elsif ($c eq ')') {             pop @stk if @stk && $stk[-1] eq '(';         }
                        ;; if ($c eq '[')
                        (p-if (setf --pcl-if-ret--12 (p-str-eq $c "["))
                          (progn
                            ;; push @stk, '['
                            (setf --pcl-if-ret--12 (p-push @stk "["))
                            
                          )
                          ;; elsif ($c eq '(')
                          (p-if (setf --pcl-if-ret--12 (p-str-eq $c "("))
                            (progn
                              ;; push @stk, '('
                              (setf --pcl-if-ret--12 (p-push @stk "("))
                              
                            )
                            ;; elsif ($c eq ']')
                            (p-if (setf --pcl-if-ret--12 (p-str-eq $c "]"))
                              (progn
                                ;; die "Mismatched brackets in template\n"                 if !@stk || $stk[-1] ne '['
                                (p-if (p-|| (p-! @stk) (p-str-ne (p-aref @stk -1) "[")) (p-die :loc "- line 699" "Mismatched brackets in template
"))
                                
                                ;; pop @stk
                                (setf --pcl-if-ret--12 (p-pop @stk))
                                
                              )
                              ;; elsif ($c eq ')')
                              (p-if (setf --pcl-if-ret--12 (p-str-eq $c ")"))
                                (progn
                                  ;; pop @stk if @stk && $stk[-1] eq '('
                                  (p-if (setf --pcl-if-ret--12 (p-&& @stk (p-str-eq (p-aref @stk -1) "(")))
                                    (setf --pcl-if-ret--12 (p-pop @stk))
                                    nil)
                                  
                                )
                                nil
                              )
                            )
                          )
                        )
                        
                      --pcl-if-ret--12)
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

;; sub p_pack { ... }
(p-sub pl-p_pack (&rest %_args)
  (p-args-body
    (block nil
      (let (($tmpl (make-p-box nil)) (@args (make-array 0 :adjustable t :fill-pointer 0)))
        ;; my ($tmpl, @args) = @_
                (let ((*wantarray* nil)) (p-list-= (vector $tmpl @args) @_))
        
        ;; local $pcl_pack_comma_warned = 0
        (let (($pcl_pack_comma_warned (p-box-for-local         0)))
          
          ;; _pack_check_brackets($tmpl)
          (let ((*wantarray* :void))           (let ((*wantarray* :void)) (pl-_pack_check_brackets $tmpl)))
          
          (let (($result (make-p-box nil)))
            ;; my $result = ''
                        (p-my-= $result "")
            
            (let (($ai (make-p-box nil)))
              ;; my $ai = 0
                            (p-my-= $ai 0)
              
              ;; _pack_tmpl($tmpl, \$ai, \@args, \$result, 0, 0)
              (let ((*wantarray* :void))               (let ((*wantarray* :void)) (pl-_pack_tmpl $tmpl (p-backslash $ai) (p-backslash @args) (p-backslash $result) 0 0)))
              
              ;; return $result
                            (p-return $result)
              
            )
          )
        )
      )  ;; end local
    )
  )
)

;; sub _unpack_utf8_char { ... }
(p-sub pl-_unpack_utf8_char (&rest %_args)
  (p-args-body
    (block nil
      (let (($k (make-p-box nil)))
        (let (($s (make-p-box nil)) ($si_ref (make-p-box nil)))
          ;; my ($s, $si_ref) = @_
                    (let ((*wantarray* nil)) (p-list-= (vector $s $si_ref) @_))
          
          (let (($slen (make-p-box nil)))
            ;; my $slen = length($s)
                        (p-my-= $slen (p-length $s))
            
            ;; return 0 if $$si_ref >= $slen
            (let ((*wantarray* :void)) (p-if (p->= (p-cast-$ $si_ref) $slen) (p-return 0)))
            
            (let (($b0 (make-p-box nil)))
              ;; my $b0 = ord(substr($s, $$si_ref, 1))
                            (p-my-= $b0 (p-ord (p-substr $s (p-cast-$ $si_ref) 1)))
              
              (let (($nb (make-p-box nil)) ($code (make-p-box nil)))
                ;; my ($nb, $code) (bare declaration)
                
                ;; if    ($b0 < 0x80) { $nb=1; $code=$b0 }     elsif ($b0 < 0xE0) { $nb=2; $code=$b0&0x1F }     elsif ($b0 < 0xF0) { $nb=3; $code=$b0&0x0F }     else               { $nb=4; $code=$b0&0x07 }
                ;; if ($b0 < 0x80)
                (p-if (p-< $b0 #x80)
                  (progn
                    ;; $nb=1
                    (let ((*wantarray* :void))                     (p-my-= $nb 1))
                    
                    ;; $code=$b0
                                        (p-my-= $code $b0)
                    
                  )
                  ;; elsif ($b0 < 0xE0)
                  (p-if (p-< $b0 #xE0)
                    (progn
                      ;; $nb=2
                      (let ((*wantarray* :void))                       (p-my-= $nb 2))
                      
                      ;; $code=$b0&0x1F
                                            (p-my-= $code (p-bit-and $b0 #x1F))
                      
                    )
                    ;; elsif ($b0 < 0xF0)
                    (p-if (p-< $b0 #xF0)
                      (progn
                        ;; $nb=3
                        (let ((*wantarray* :void))                         (p-my-= $nb 3))
                        
                        ;; $code=$b0&0x0F
                                                (p-my-= $code (p-bit-and $b0 #x0F))
                        
                      )
                      ;; else
                      (progn
                        ;; $nb=4
                        (let ((*wantarray* :void))                         (p-my-= $nb 4))
                        
                        ;; $code=$b0&0x07
                                                (p-my-= $code (p-bit-and $b0 #x07))
                        
                      )
                    )
                  )
                )
                
                ;; for (my $k=1; $k<$nb; $k++) {         $code = ($code<<6)|(ord(substr($s, $$si_ref+$k, 1))&0x3F) if $$si_ref+$k<$slen;     }
                (let (($k (make-p-box nil)))
                  (p-for (                (p-my-= $k 1))
                          (                (p-< $k $nb))
                          (                (p-post++ $k))
                    (let ((--pcl-if-ret--13 nil))
                      ;; $code = ($code<<6)|(ord(substr($s, $$si_ref+$k, 1))&0x3F) if $$si_ref+$k<$slen
                      (p-if (setf --pcl-if-ret--13 (p-< (p-+ (p-cast-$ $si_ref) $k) $slen))
                        (setf --pcl-if-ret--13 (p-my-= $code (p-bit-or (p-<< $code 6) (p-bit-and (p-ord (p-substr $s (p-+ (p-cast-$ $si_ref) $k) 1)) #x3F))))
                        nil)
                      
                    --pcl-if-ret--13)
                  )
                )
                
                ;; $$si_ref += $nb
                (let ((*wantarray* :void))                 (p-incf (p-cast-$ $si_ref) $nb))
                
                ;; return $code
                                (p-return $code)
                
              )
            )
          )
        )
      )
    )
  )
)

;; sub _unpack_str { ... }
(p-sub pl-_unpack_str (&rest %_args)
  (p-args-body
    (block nil
      (let (($n (make-p-box nil)) ($raw (make-p-box nil)) ($hex (make-p-box nil)) ($i (make-p-box nil)) ($b (make-p-box nil)) ($nbits (make-p-box nil)) ($bits (make-p-box nil)) ($decoded (make-p-box nil)) ($lc (make-p-box nil)) ($nb (make-p-box nil)) ($ng (make-p-box nil)) ($k (make-p-box nil)) ($get (make-p-box nil)) ($cm (make-p-box nil)) ($done (make-p-box nil)) ($v (make-p-box nil)) ($more (make-p-box nil)))
        (let ((--pcl-if-ret--14 nil))
          (let (($ch (make-p-box nil)) ($nrep (make-p-box nil)) ($all (make-p-box nil)) ($s__lex__1 (make-p-box nil)) ($si_ref__lex__2 (make-p-box nil)) ($push_val (make-p-box nil)) ($checksum_p (make-p-box nil)))
            ;; my ($ch, $nrep, $all, $s, $si_ref, $push_val, $checksum_p) = @_
                        (let ((*wantarray* nil)) (p-list-= (vector $ch $nrep $all $s__lex__1 $si_ref__lex__2 $push_val $checksum_p) @_))
            
            (let (($slen__lex__3 (make-p-box nil)))
              ;; my $slen = length($s)
              (p-my-= $slen__lex__3               (p-length $s__lex__1))
              
              ;; if ($ch eq 'A' || $ch eq 'a' || $ch eq 'Z') {         my $n = $all ? ($slen - $$si_ref) : $nrep;         $n = 0 if $n < 0;         my $raw = $$si_ref < $slen ? substr($s, $$si_ref, $n) : '';         $$si_ref += $n;         $raw =~ s/[ \x00]+$// if $ch eq 'A';         $raw =~ s/\x00.*//s   if $ch eq 'Z';         $push_val->($raw);     } elsif ($ch eq 'H') {         my $n = $all ? (2 * ($slen - $$si_ref)) : $nrep;  # n = number of nybbles         my $hex = '';         for (my $i = 0; $i < int($n/2); $i++) {             my $b = $$si_ref+$i < $slen ? ord(substr($s,$$si_ref+$i,1)) : 0;             $hex .= sprintf('%02x', $b);         }         $hex = substr($hex, 0, $n);         $$si_ref += int(($n+1)/2);         $push_val->($hex);     } elsif ($ch eq 'h') {         my $n = $all ? (2 * ($slen - $$si_ref)) : $nrep;  # n = number of nybbles         my $hex = '';         for (my $i = 0; $i < int($n/2); $i++) {             my $b = $$si_ref+$i < $slen ? ord(substr($s,$$si_ref+$i,1)) : 0;             $hex .= sprintf('%x%x', $b&0xF, ($b>>4)&0xF);         }         $hex = substr($hex, 0, $n);         $$si_ref += int(($n+1)/2);         $push_val->($hex);     } elsif ($ch eq 'B') {         my $nbits = $all ? (8*($slen-$$si_ref)) : $nrep;         if ($checksum_p) {             for (my $i=0; $i<$nbits; $i++) {                 my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0;                 $push_val->( ($b>>(7-($i%8))) & 1 );             }         } else {             my $bits = '';             for (my $i=0; $i<$nbits; $i++) {                 my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0;                 $bits .= (($b>>(7-($i%8)))&1) ? '1' : '0';             }             $push_val->($bits);         }         $$si_ref += int(($nbits+7)/8);     } elsif ($ch eq 'b') {         my $nbits = $all ? (8*($slen-$$si_ref)) : $nrep;         if ($checksum_p) {             for (my $i=0; $i<$nbits; $i++) {                 my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0;                 $push_val->( ($b>>($i%8)) & 1 );             }         } else {             my $bits = '';             for (my $i=0; $i<$nbits; $i++) {                 my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0;                 $bits .= (($b>>($i%8))&1) ? '1' : '0';             }             $push_val->($bits);         }         $$si_ref += int(($nbits+7)/8);     } elsif ($ch eq 'u') {         my $decoded = '';         while ($$si_ref < $slen) {             my $lc = ord(substr($s, $$si_ref, 1));             my $nb = ($lc - 32) & 63; $$si_ref++;             last if $nb == 0;             my $ng = int(($nb+2)/3);             for (my $k=0; $k<$ng; $k++) {                 my $get = sub { my $i=$$si_ref+$_[0]; $i<$slen?(ord(substr($s,$i,1))-32)&63:0 };                 my $cm = ($get->(4*$k)<<18)|($get->(4*$k+1)<<12)|($get->(4*$k+2)<<6)|$get->(4*$k+3);                 $decoded .= chr(($cm>>16)&0xFF) if $k*3   < $nb;                 $decoded .= chr(($cm>> 8)&0xFF) if $k*3+1 < $nb;                 $decoded .= chr( $cm     &0xFF) if $k*3+2 < $nb;             }             $$si_ref += $ng*4;             $$si_ref++ if $$si_ref < $slen && substr($s,$$si_ref,1) eq "\n";         }         $push_val->($decoded);     } elsif ($ch eq 'U') {         my $n = $all ? 9**9 : $nrep;         my $done = 0;         while ($done < $n && $$si_ref < $slen) {             $push_val->(_unpack_utf8_char($s, $si_ref)); $done++;         }     } elsif ($ch eq 'W') {         my $n = $all ? ($slen-$$si_ref) : $nrep;         for (my $i=0; $i<$n && $$si_ref<$slen; $i++) {             $push_val->(ord(substr($s, $$si_ref++, 1)));         }     } elsif ($ch eq 'w') {         my $done = 0;         while (($all || $done < $nrep) && $$si_ref < $slen) {             my ($v, $more) = (0, 1);             while ($more) {                 die "Unterminated compressed integer in unpack\n" if $$si_ref >= $slen;                 my $b = ord(substr($s, $$si_ref++, 1));                 $more = $b & 0x80; $v = ($v<<7)|($b&0x7F);             }             $push_val->($v); $done++;         }     }
              ;; if ($ch eq 'A' || $ch eq 'a' || $ch eq 'Z')
              (p-if (setf --pcl-if-ret--14 (p-|| (p-|| (p-str-eq $ch "A") (p-str-eq $ch "a")) (p-str-eq $ch "Z")))
                (progn
                  (let (($n (make-p-box nil)) ($raw (make-p-box nil)))
                    ;; my $n = $all ? ($slen - $$si_ref) : $nrep
                                        (p-my-= $n (p-if $all (p-- $slen__lex__3 (p-cast-$ $si_ref__lex__2)) $nrep))
                    
                    ;; $n = 0 if $n < 0
                    (let ((*wantarray* :void)) (p-if (p-< $n 0) (p-my-= $n 0)))
                    
                    ;; my $raw = $$si_ref < $slen ? substr($s, $$si_ref, $n) : ''
                                        (p-my-= $raw (p-if (p-< (p-cast-$ $si_ref__lex__2) $slen__lex__3) (p-substr $s__lex__1 (p-cast-$ $si_ref__lex__2) $n) ""))
                    
                    ;; $$si_ref += $n
                    (let ((*wantarray* :void))                     (p-incf (p-cast-$ $si_ref__lex__2) $n))
                    
                    ;; $raw =~ s/[ \x00]+$// if $ch eq 'A'
                    (let ((*wantarray* :void)) (p-if (p-str-eq $ch "A") (p-=~ $raw (p-subst "[ \\x00]+$" ""))))
                    
                    ;; $raw =~ s/\x00.*//s   if $ch eq 'Z'
                    (let ((*wantarray* :void)) (p-if (p-str-eq $ch "Z") (p-=~ $raw (p-subst "\\x00.*" "" :s))))
                    
                    ;; $push_val->($raw)
                    (setf --pcl-if-ret--14 (let ((*wantarray* nil)) (p-funcall-ref $push_val $raw)))
                    
                  )
                )
                ;; elsif ($ch eq 'H')
                (p-if (setf --pcl-if-ret--14 (p-str-eq $ch "H"))
                  (progn
                    (let (($n (make-p-box nil)) ($hex (make-p-box nil)) ($i (make-p-box nil)) ($b (make-p-box nil)))
                      ;; my $n = $all ? (2 * ($slen - $$si_ref)) : $nrep
                                            (p-my-= $n (p-if $all (p-* 2 (p-- $slen__lex__3 (p-cast-$ $si_ref__lex__2))) $nrep))
                      
                      ;; my $hex = ''
                                            (p-my-= $hex "")
                      
                      ;; for (my $i = 0; $i < int($n/2); $i++) {             my $b = $$si_ref+$i < $slen ? ord(substr($s,$$si_ref+$i,1)) : 0;             $hex .= sprintf('%02x', $b);         }
                      (let (($i (make-p-box nil)))
                        (p-for (                      (p-my-= $i 0))
                                (                      (p-< $i (p-int (p-/ $n 2))))
                                (                      (p-post++ $i))
                          (let (($b (make-p-box nil)))
                            ;; my $b = $$si_ref+$i < $slen ? ord(substr($s,$$si_ref+$i,1)) : 0
                                                        (p-my-= $b (p-if (p-< (p-+ (p-cast-$ $si_ref__lex__2) $i) $slen__lex__3) (p-ord (p-substr $s__lex__1 (p-+ (p-cast-$ $si_ref__lex__2) $i) 1)) 0))
                            
                            ;; $hex .= sprintf('%02x', $b)
                                                        (p-.= $hex (p-sprintf "%02x" $b))
                            
                          )
                        )
                      )
                      
                      ;; $hex = substr($hex, 0, $n)
                      (let ((*wantarray* :void))                       (p-my-= $hex (p-substr $hex 0 $n)))
                      
                      ;; $$si_ref += int(($n+1)/2)
                      (let ((*wantarray* :void))                       (p-incf (p-cast-$ $si_ref__lex__2) (p-int (p-/ (p-+ $n 1) 2))))
                      
                      ;; $push_val->($hex)
                      (setf --pcl-if-ret--14 (let ((*wantarray* nil)) (p-funcall-ref $push_val $hex)))
                      
                    )
                  )
                  ;; elsif ($ch eq 'h')
                  (p-if (setf --pcl-if-ret--14 (p-str-eq $ch "h"))
                    (progn
                      (let (($n (make-p-box nil)) ($hex (make-p-box nil)) ($i (make-p-box nil)) ($b (make-p-box nil)))
                        ;; my $n = $all ? (2 * ($slen - $$si_ref)) : $nrep
                                                (p-my-= $n (p-if $all (p-* 2 (p-- $slen__lex__3 (p-cast-$ $si_ref__lex__2))) $nrep))
                        
                        ;; my $hex = ''
                                                (p-my-= $hex "")
                        
                        ;; for (my $i = 0; $i < int($n/2); $i++) {             my $b = $$si_ref+$i < $slen ? ord(substr($s,$$si_ref+$i,1)) : 0;             $hex .= sprintf('%x%x', $b&0xF, ($b>>4)&0xF);         }
                        (let (($i (make-p-box nil)))
                          (p-for (                        (p-my-= $i 0))
                                  (                        (p-< $i (p-int (p-/ $n 2))))
                                  (                        (p-post++ $i))
                            (let (($b (make-p-box nil)))
                              ;; my $b = $$si_ref+$i < $slen ? ord(substr($s,$$si_ref+$i,1)) : 0
                                                            (p-my-= $b (p-if (p-< (p-+ (p-cast-$ $si_ref__lex__2) $i) $slen__lex__3) (p-ord (p-substr $s__lex__1 (p-+ (p-cast-$ $si_ref__lex__2) $i) 1)) 0))
                              
                              ;; $hex .= sprintf('%x%x', $b&0xF, ($b>>4)&0xF)
                                                            (p-.= $hex (p-sprintf "%x%x" (p-bit-and $b #xF) (p-bit-and (p->> $b 4) #xF)))
                              
                            )
                          )
                        )
                        
                        ;; $hex = substr($hex, 0, $n)
                        (let ((*wantarray* :void))                         (p-my-= $hex (p-substr $hex 0 $n)))
                        
                        ;; $$si_ref += int(($n+1)/2)
                        (let ((*wantarray* :void))                         (p-incf (p-cast-$ $si_ref__lex__2) (p-int (p-/ (p-+ $n 1) 2))))
                        
                        ;; $push_val->($hex)
                        (setf --pcl-if-ret--14 (let ((*wantarray* nil)) (p-funcall-ref $push_val $hex)))
                        
                      )
                    )
                    ;; elsif ($ch eq 'B')
                    (p-if (setf --pcl-if-ret--14 (p-str-eq $ch "B"))
                      (progn
                        (let (($nbits (make-p-box nil)) ($i (make-p-box nil)) ($b (make-p-box nil)) ($bits (make-p-box nil)))
                          ;; my $nbits = $all ? (8*($slen-$$si_ref)) : $nrep
                                                    (p-my-= $nbits (p-if $all (p-* 8 (p-- $slen__lex__3 (p-cast-$ $si_ref__lex__2))) $nrep))
                          
                          ;; if ($checksum_p) {             for (my $i=0; $i<$nbits; $i++) {                 my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0;                 $push_val->( ($b>>(7-($i%8))) & 1 );             }         } else {             my $bits = '';             for (my $i=0; $i<$nbits; $i++) {                 my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0;                 $bits .= (($b>>(7-($i%8)))&1) ? '1' : '0';             }             $push_val->($bits);         }
                          ;; if ($checksum_p)
                          (p-if $checksum_p
                            (progn
                              (let (($i (make-p-box nil)) ($b (make-p-box nil)))
                                ;; for (my $i=0; $i<$nbits; $i++) {                 my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0;                 $push_val->( ($b>>(7-($i%8))) & 1 );             }
                                (let (($i (make-p-box nil)))
                                  (p-for (                                (p-my-= $i 0))
                                          (                                (p-< $i $nbits))
                                          (                                (p-post++ $i))
                                    (let (($b (make-p-box nil)))
                                      ;; my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0
                                                                            (p-my-= $b (p-if (p-< (p-+ (p-cast-$ $si_ref__lex__2) (p-int (p-/ $i 8))) $slen__lex__3) (p-ord (p-substr $s__lex__1 (p-+ (p-cast-$ $si_ref__lex__2) (p-int (p-/ $i 8))) 1)) 0))
                                      
                                      ;; $push_val->( ($b>>(7-($i%8))) & 1 )
                                                                            (p-funcall-ref $push_val (p-bit-and (p->> $b (p-- 7 (p-% $i 8))) 1))
                                      
                                    )
                                  )
                                )
                                
                              )
                            )
                            ;; else
                            (progn
                              (let (($bits (make-p-box nil)) ($i (make-p-box nil)) ($b (make-p-box nil)))
                                ;; my $bits = ''
                                                                (p-my-= $bits "")
                                
                                ;; for (my $i=0; $i<$nbits; $i++) {                 my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0;                 $bits .= (($b>>(7-($i%8)))&1) ? '1' : '0';             }
                                (let (($i (make-p-box nil)))
                                  (p-for (                                (p-my-= $i 0))
                                          (                                (p-< $i $nbits))
                                          (                                (p-post++ $i))
                                    (let (($b (make-p-box nil)))
                                      ;; my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0
                                                                            (p-my-= $b (p-if (p-< (p-+ (p-cast-$ $si_ref__lex__2) (p-int (p-/ $i 8))) $slen__lex__3) (p-ord (p-substr $s__lex__1 (p-+ (p-cast-$ $si_ref__lex__2) (p-int (p-/ $i 8))) 1)) 0))
                                      
                                      ;; $bits .= (($b>>(7-($i%8)))&1) ? '1' : '0'
                                                                            (p-.= $bits (p-if (p-bit-and (p->> $b (p-- 7 (p-% $i 8))) 1) "1" "0"))
                                      
                                    )
                                  )
                                )
                                
                                ;; $push_val->($bits)
                                                                (p-funcall-ref $push_val $bits)
                                
                              )
                            )
                          )
                          
                          ;; $$si_ref += int(($nbits+7)/8)
                          (setf --pcl-if-ret--14 (p-incf (p-cast-$ $si_ref__lex__2) (p-int (p-/ (p-+ $nbits 7) 8))))
                          
                        )
                      )
                      ;; elsif ($ch eq 'b')
                      (p-if (setf --pcl-if-ret--14 (p-str-eq $ch "b"))
                        (progn
                          (let (($nbits (make-p-box nil)) ($i (make-p-box nil)) ($b (make-p-box nil)) ($bits (make-p-box nil)))
                            ;; my $nbits = $all ? (8*($slen-$$si_ref)) : $nrep
                                                        (p-my-= $nbits (p-if $all (p-* 8 (p-- $slen__lex__3 (p-cast-$ $si_ref__lex__2))) $nrep))
                            
                            ;; if ($checksum_p) {             for (my $i=0; $i<$nbits; $i++) {                 my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0;                 $push_val->( ($b>>($i%8)) & 1 );             }         } else {             my $bits = '';             for (my $i=0; $i<$nbits; $i++) {                 my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0;                 $bits .= (($b>>($i%8))&1) ? '1' : '0';             }             $push_val->($bits);         }
                            ;; if ($checksum_p)
                            (p-if $checksum_p
                              (progn
                                (let (($i (make-p-box nil)) ($b (make-p-box nil)))
                                  ;; for (my $i=0; $i<$nbits; $i++) {                 my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0;                 $push_val->( ($b>>($i%8)) & 1 );             }
                                  (let (($i (make-p-box nil)))
                                    (p-for (                                  (p-my-= $i 0))
                                            (                                  (p-< $i $nbits))
                                            (                                  (p-post++ $i))
                                      (let (($b (make-p-box nil)))
                                        ;; my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0
                                                                                (p-my-= $b (p-if (p-< (p-+ (p-cast-$ $si_ref__lex__2) (p-int (p-/ $i 8))) $slen__lex__3) (p-ord (p-substr $s__lex__1 (p-+ (p-cast-$ $si_ref__lex__2) (p-int (p-/ $i 8))) 1)) 0))
                                        
                                        ;; $push_val->( ($b>>($i%8)) & 1 )
                                                                                (p-funcall-ref $push_val (p-bit-and (p->> $b (p-% $i 8)) 1))
                                        
                                      )
                                    )
                                  )
                                  
                                )
                              )
                              ;; else
                              (progn
                                (let (($bits (make-p-box nil)) ($i (make-p-box nil)) ($b (make-p-box nil)))
                                  ;; my $bits = ''
                                                                    (p-my-= $bits "")
                                  
                                  ;; for (my $i=0; $i<$nbits; $i++) {                 my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0;                 $bits .= (($b>>($i%8))&1) ? '1' : '0';             }
                                  (let (($i (make-p-box nil)))
                                    (p-for (                                  (p-my-= $i 0))
                                            (                                  (p-< $i $nbits))
                                            (                                  (p-post++ $i))
                                      (let (($b (make-p-box nil)))
                                        ;; my $b = $$si_ref+int($i/8)<$slen ? ord(substr($s,$$si_ref+int($i/8),1)) : 0
                                                                                (p-my-= $b (p-if (p-< (p-+ (p-cast-$ $si_ref__lex__2) (p-int (p-/ $i 8))) $slen__lex__3) (p-ord (p-substr $s__lex__1 (p-+ (p-cast-$ $si_ref__lex__2) (p-int (p-/ $i 8))) 1)) 0))
                                        
                                        ;; $bits .= (($b>>($i%8))&1) ? '1' : '0'
                                                                                (p-.= $bits (p-if (p-bit-and (p->> $b (p-% $i 8)) 1) "1" "0"))
                                        
                                      )
                                    )
                                  )
                                  
                                  ;; $push_val->($bits)
                                                                    (p-funcall-ref $push_val $bits)
                                  
                                )
                              )
                            )
                            
                            ;; $$si_ref += int(($nbits+7)/8)
                            (setf --pcl-if-ret--14 (p-incf (p-cast-$ $si_ref__lex__2) (p-int (p-/ (p-+ $nbits 7) 8))))
                            
                          )
                        )
                        ;; elsif ($ch eq 'u')
                        (p-if (setf --pcl-if-ret--14 (p-str-eq $ch "u"))
                          (progn
                            (let (($decoded (make-p-box nil)) ($lc (make-p-box nil)) ($nb (make-p-box nil)) ($ng (make-p-box nil)) ($k (make-p-box nil)) ($get (make-p-box nil)) ($cm (make-p-box nil)))
                              ;; my $decoded = ''
                                                            (p-my-= $decoded "")
                              
                              ;; while ($$si_ref < $slen) {             my $lc = ord(substr($s, $$si_ref, 1));             my $nb = ($lc - 32) & 63; $$si_ref++;             last if $nb == 0;             my $ng = int(($nb+2)/3);             for (my $k=0; $k<$ng; $k++) {                 my $get = sub { my $i=$$si_ref+$_[0]; $i<$slen?(ord(substr($s,$i,1))-32)&63:0 };                 my $cm = ($get->(4*$k)<<18)|($get->(4*$k+1)<<12)|($get->(4*$k+2)<<6)|$get->(4*$k+3);                 $decoded .= chr(($cm>>16)&0xFF) if $k*3   < $nb;                 $decoded .= chr(($cm>> 8)&0xFF) if $k*3+1 < $nb;                 $decoded .= chr( $cm     &0xFF) if $k*3+2 < $nb;             }             $$si_ref += $ng*4;             $$si_ref++ if $$si_ref < $slen && substr($s,$$si_ref,1) eq "\n";         }
                              (p-while (p-< (p-cast-$ $si_ref__lex__2) $slen__lex__3)
                                (let (($lc (make-p-box nil)) ($nb (make-p-box nil)) ($ng (make-p-box nil)) ($k (make-p-box nil)) ($get (make-p-box nil)) ($cm (make-p-box nil)))
                                  (let ((--pcl-if-ret--15 nil))
                                    ;; my $lc = ord(substr($s, $$si_ref, 1))
                                                                        (p-my-= $lc (p-ord (p-substr $s__lex__1 (p-cast-$ $si_ref__lex__2) 1)))
                                    
                                    ;; my $nb = ($lc - 32) & 63
                                                                        (p-my-= $nb (p-bit-and (p-- $lc 32) 63))
                                    
                                    ;; $$si_ref++
                                    (let ((*wantarray* :void))                                     (p-post++ (p-cast-$ $si_ref__lex__2)))
                                    
                                    ;; last if $nb == 0
                                    (let ((*wantarray* :void)) (p-if (p-== $nb 0) (p-last)))
                                    
                                    ;; my $ng = int(($nb+2)/3)
                                                                        (p-my-= $ng (p-int (p-/ (p-+ $nb 2) 3)))
                                    
                                    ;; for (my $k=0; $k<$ng; $k++) {                 my $get = sub { my $i=$$si_ref+$_[0]; $i<$slen?(ord(substr($s,$i,1))-32)&63:0 };                 my $cm = ($get->(4*$k)<<18)|($get->(4*$k+1)<<12)|($get->(4*$k+2)<<6)|$get->(4*$k+3);                 $decoded .= chr(($cm>>16)&0xFF) if $k*3   < $nb;                 $decoded .= chr(($cm>> 8)&0xFF) if $k*3+1 < $nb;                 $decoded .= chr( $cm     &0xFF) if $k*3+2 < $nb;             }
                                    (let (($k (make-p-box nil)))
                                      (p-for (                                    (p-my-= $k 0))
                                              (                                    (p-< $k $ng))
                                              (                                    (p-post++ $k))
                                        (let (($get (make-p-box nil)) ($cm (make-p-box nil)))
                                          (let ((--pcl-if-ret--16 nil))
                                            ;; my $get = sub { my $i=$$si_ref+$_[0]; $i<$slen?(ord(substr($s,$i,1))-32)&63:0 }
                                                                                        (p-my-= $get (lambda (&rest %_args)
  (let ((@_ (p-flatten-args %_args))
        (*pcl-caller-wantarray* *wantarray*))
    (catch :p-return
      (block nil
        ;; my $i=$$si_ref+$_[0]
                (p-my-= $i (p-+ (p-cast-$ $si_ref__lex__2) (p-aref @_ 0)))
        
        ;; $i<$slen?(ord(substr($s,$i,1))-32)&63:0
                (p-if (p-< $i $slen__lex__3) (p-bit-and (p-- (p-ord (p-substr $s__lex__1 $i 1)) 32) 63) 0)
        
      )
    )
  )
))
                                            
                                            ;; my $cm = ($get->(4*$k)<<18)|($get->(4*$k+1)<<12)|($get->(4*$k+2)<<6)|$get->(4*$k+3)
                                                                                        (p-my-= $cm (p-bit-or (p-bit-or (p-bit-or (p-<< (let ((*wantarray* nil)) (p-funcall-ref $get (p-* 4 $k))) 18) (p-<< (let ((*wantarray* nil)) (p-funcall-ref $get (p-+ (p-* 4 $k) 1))) 12)) (p-<< (let ((*wantarray* nil)) (p-funcall-ref $get (p-+ (p-* 4 $k) 2))) 6)) (let ((*wantarray* nil)) (p-funcall-ref $get (p-+ (p-* 4 $k) 3)))))
                                            
                                            ;; $decoded .= chr(($cm>>16)&0xFF) if $k*3   < $nb
                                            (let ((*wantarray* :void)) (p-if (p-< (p-* $k 3) $nb) (p-.= $decoded (p-chr (p-bit-and (p->> $cm 16) #xFF)))))
                                            
                                            ;; $decoded .= chr(($cm>> 8)&0xFF) if $k*3+1 < $nb
                                            (let ((*wantarray* :void)) (p-if (p-< (p-+ (p-* $k 3) 1) $nb) (p-.= $decoded (p-chr (p-bit-and (p->> $cm 8) #xFF)))))
                                            
                                            ;; $decoded .= chr( $cm     &0xFF) if $k*3+2 < $nb
                                            (p-if (setf --pcl-if-ret--16 (p-< (p-+ (p-* $k 3) 2) $nb))
                                              (setf --pcl-if-ret--16 (p-.= $decoded (p-chr (p-bit-and $cm #xFF))))
                                              nil)
                                            
                                          --pcl-if-ret--16)
                                        )
                                      )
                                    )
                                    
                                    ;; $$si_ref += $ng*4
                                    (let ((*wantarray* :void))                                     (p-incf (p-cast-$ $si_ref__lex__2) (p-* $ng 4)))
                                    
                                    ;; $$si_ref++ if $$si_ref < $slen && substr($s,$$si_ref,1) eq "\n"
                                    (p-if (setf --pcl-if-ret--15 (p-&& (p-< (p-cast-$ $si_ref__lex__2) $slen__lex__3) (p-str-eq (p-substr $s__lex__1 (p-cast-$ $si_ref__lex__2) 1) "
")))
                                      (setf --pcl-if-ret--15 (p-post++ (p-cast-$ $si_ref__lex__2)))
                                      nil)
                                    
                                  --pcl-if-ret--15)
                                )
                              )
                              
                              ;; $push_val->($decoded)
                              (setf --pcl-if-ret--14 (let ((*wantarray* nil)) (p-funcall-ref $push_val $decoded)))
                              
                            )
                          )
                          ;; elsif ($ch eq 'U')
                          (p-if (setf --pcl-if-ret--14 (p-str-eq $ch "U"))
                            (progn
                              (let (($n (make-p-box nil)) ($done (make-p-box nil)))
                                ;; my $n = $all ? 9**9 : $nrep
                                                                (p-my-= $n (p-if $all (p-** 9 9) $nrep))
                                
                                ;; my $done = 0
                                                                (p-my-= $done 0)
                                
                                ;; while ($done < $n && $$si_ref < $slen) {             $push_val->(_unpack_utf8_char($s, $si_ref)); $done++;         }
                                (p-while (p-&& (p-< $done $n) (p-< (p-cast-$ $si_ref__lex__2) $slen__lex__3))
                                  ;; $push_val->(_unpack_utf8_char($s, $si_ref))
                                  (let ((*wantarray* :void))                                   (let ((*wantarray* :void)) (p-funcall-ref $push_val (let ((*wantarray* :void)) (pl-_unpack_utf8_char $s__lex__1 $si_ref__lex__2)))))
                                  
                                  ;; $done++
                                                                    (p-post++ $done)
                                  
                                )
                                
                              )
                            )
                            ;; elsif ($ch eq 'W')
                            (p-if (setf --pcl-if-ret--14 (p-str-eq $ch "W"))
                              (progn
                                (let (($n (make-p-box nil)) ($i (make-p-box nil)))
                                  ;; my $n = $all ? ($slen-$$si_ref) : $nrep
                                                                    (p-my-= $n (p-if $all (p-- $slen__lex__3 (p-cast-$ $si_ref__lex__2)) $nrep))
                                  
                                  ;; for (my $i=0; $i<$n && $$si_ref<$slen; $i++) {             $push_val->(ord(substr($s, $$si_ref++, 1)));         }
                                  (let (($i (make-p-box nil)))
                                    (p-for (                                  (p-my-= $i 0))
                                            (                                  (p-&& (p-< $i $n) (p-< (p-cast-$ $si_ref__lex__2) $slen__lex__3)))
                                            (                                  (p-post++ $i))
                                      ;; $push_val->(ord(substr($s, $$si_ref++, 1)))
                                                                            (p-funcall-ref $push_val (p-ord (p-substr $s__lex__1 (p-post++ (p-cast-$ $si_ref__lex__2)) 1)))
                                      
                                    )
                                  )
                                  
                                )
                              )
                              ;; elsif ($ch eq 'w')
                              (p-if (setf --pcl-if-ret--14 (p-str-eq $ch "w"))
                                (progn
                                  (let (($done (make-p-box nil)) ($v (make-p-box nil)) ($more (make-p-box nil)) ($b (make-p-box nil)))
                                    ;; my $done = 0
                                                                        (p-my-= $done 0)
                                    
                                    ;; while (($all || $done < $nrep) && $$si_ref < $slen) {             my ($v, $more) = (0, 1);             while ($more) {                 die "Unterminated compressed integer in unpack\n" if $$si_ref >= $slen;                 my $b = ord(substr($s, $$si_ref++, 1));                 $more = $b & 0x80; $v = ($v<<7)|($b&0x7F);             }             $push_val->($v); $done++;         }
                                    (p-while (p-&& (p-|| $all (p-< $done $nrep)) (p-< (p-cast-$ $si_ref__lex__2) $slen__lex__3))
                                      (let (($v (make-p-box nil)) ($more (make-p-box nil)) ($b (make-p-box nil)))
                                        ;; my ($v, $more) = (0, 1)
                                                                                (let ((*wantarray* nil)) (p-list-= (vector $v $more) (vector 0 1)))
                                        
                                        ;; while ($more) {                 die "Unterminated compressed integer in unpack\n" if $$si_ref >= $slen;                 my $b = ord(substr($s, $$si_ref++, 1));                 $more = $b & 0x80; $v = ($v<<7)|($b&0x7F);             }
                                        (p-while $more
                                          (let (($b (make-p-box nil)))
                                            ;; die "Unterminated compressed integer in unpack\n" if $$si_ref >= $slen
                                            (let ((*wantarray* :void)) (p-if (p->= (p-cast-$ $si_ref__lex__2) $slen__lex__3) (p-die :loc "- line 841" "Unterminated compressed integer in unpack
")))
                                            
                                            ;; my $b = ord(substr($s, $$si_ref++, 1))
                                                                                        (p-my-= $b (p-ord (p-substr $s__lex__1 (p-post++ (p-cast-$ $si_ref__lex__2)) 1)))
                                            
                                            ;; $more = $b & 0x80
                                            (let ((*wantarray* :void))                                             (p-my-= $more (p-bit-and $b #x80)))
                                            
                                            ;; $v = ($v<<7)|($b&0x7F)
                                                                                        (p-my-= $v (p-bit-or (p-<< $v 7) (p-bit-and $b #x7F)))
                                            
                                          )
                                        )
                                        
                                        ;; $push_val->($v)
                                        (let ((*wantarray* :void))                                         (let ((*wantarray* :void)) (p-funcall-ref $push_val $v)))
                                        
                                        ;; $done++
                                                                                (p-post++ $done)
                                        
                                      )
                                    )
                                    
                                  )
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
        --pcl-if-ret--14)
      )
    )
  )
)

;; sub _unpack_tmpl { ... }
(p-sub pl-_unpack_tmpl (&rest %_args)
  (p-args-body
    (block nil
      (let (($ch (make-p-box nil)) ($grpbeg (make-p-box nil)) ($grpend (make-p-box nil)) ($bang (make-p-box nil)) ($be (make-p-box nil)) ($le (make-p-box nil)) ($ti_before_count (make-p-box nil)) ($all (make-p-box nil)) ($count (make-p-box nil)) ($nrep (make-p-box nil)) ($had_count (make-p-box nil)) ($nb (make-p-box nil)) ($sig (make-p-box nil)) ($dbe (make-p-box nil)) ($slash_n (make-p-box nil)) ($be2 (make-p-box nil)) ($more (make-p-box nil)) ($b (make-p-box nil)) ($end (make-p-box nil)) ($raw (make-p-box nil)) ($n (make-p-box nil)) ($dch (make-p-box nil)) ($dbang (make-p-box nil)) ($dbe2 (make-p-box nil)) ($dle2 (make-p-box nil)) ($dall (make-p-box nil)) ($dcnt (make-p-box nil)) ($dnrep (make-p-box nil)) ($chain (make-p-box nil)) ($dnb (make-p-box nil)) ($dsig (make-p-box nil)) ($ddbe (make-p-box nil)) ($dbe3 (make-p-box nil)) ($raw2 (make-p-box nil)) ($i (make-p-box nil)) ($ge (make-p-box nil)) ($inner (make-p-box nil)) ($r (make-p-box nil)) ($iter_base (make-p-box nil)) ($gti (make-p-box nil)) ($fc (make-p-box nil)) ($si_before (make-p-box nil)))
        (let (($tmpl (make-p-box nil)) ($s (make-p-box nil)) ($si_ref (make-p-box nil)) ($push_val (make-p-box nil)) ($inh_be (make-p-box nil)) ($inh_le (make-p-box nil)) ($checksum_p (make-p-box nil)) ($group_base (make-p-box nil)) ($depth (make-p-box nil)))
          ;; my ($tmpl, $s, $si_ref, $push_val, $inh_be, $inh_le, $checksum_p, $group_base, $depth) = @_
                    (let ((*wantarray* nil)) (p-list-= (vector $tmpl $s $si_ref $push_val $inh_be $inh_le $checksum_p $group_base $depth) @_))
          
          ;; $group_base = 0 unless defined $group_base
          (let ((*wantarray* :void)) (p-unless (p-defined $group_base) (p-my-= $group_base 0)))
          
          ;; $depth = 0 unless defined $depth
          (let ((*wantarray* :void)) (p-unless (p-defined $depth) (p-my-= $depth 0)))
          
          ;; die "Too deeply nested \(\)-groups in unpack\n" if $depth > $MAX_GROUP_DEPTH
          (let ((*wantarray* :void)) (p-if (p-> $depth $MAX_GROUP_DEPTH) (p-die :loc "- line 857" "Too deeply nested ()-groups in unpack
")))
          
          (let (($slen (make-p-box nil)))
            ;; my $slen = length($s)
                        (p-my-= $slen (p-length $s))
            
            (let (($ti (make-p-box nil)))
              ;; my $ti = 0
                            (p-my-= $ti 0)
              
              (let (($tlen (make-p-box nil)))
                ;; my $tlen = length($tmpl)
                                (p-my-= $tlen (p-length $tmpl))
                
                ;; while (1) {         $ti = _pack_skip_ws($tmpl, $ti);         last if $ti >= $tlen;         my $ch = substr($tmpl, $ti, 1); $ti++;         my ($grpbeg, $grpend) = (undef, undef);         if ($ch eq '(') {             $grpend = _pack_find_group_end($tmpl, $ti);             $grpbeg = $ti; $ti = $grpend + 1; $ch = '(';         }         my ($bang, $be, $le) = _pack_parse_mods($tmpl, \$ti, $inh_be, $inh_le, $ch, 'unpack');         # No ws skip here: space between type+mods and count is invalid in Perl.         my $ti_before_count = $ti;         my ($all, $count, $nrep) = _pack_parse_count($tmpl, \$ti);         my $had_count = ($all || $ti > $ti_before_count);         $ti = _pack_skip_ws($tmpl, $ti);          # Slash mode: count/data pairs, with support for chained slashes (A /A /A ...)         if ($ti < $tlen && substr($tmpl, $ti, 1) eq '/') {             $ti++;             $ti = _pack_skip_ws($tmpl, $ti);             die "Code missing after '/' in unpack\n" if $ti >= $tlen;             # '/' must not have a count applied directly to it (e.g. c/*a or c/1a are invalid;             # Z*/A* is valid because * is the count for Z, not for /).             { my $c = substr($tmpl, $ti, 1);               die "'/' does not take a repeat count in unpack\n"                 if $c eq '*' || $c eq '[' || $c =~ /\d/; }             my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang);             my $slash_n = 0;             if ($nb) {                 my $be2 = $be ? 1 : ($le ? 0 : $dbe);                 if ($$si_ref + $nb > $slen) {                     last unless $depth > 0;                     die "length/code after end of string in unpack\n";                 }                 $slash_n = _unpack_read_int($s, $$si_ref, $nb, $be2, $sig);                 $$si_ref += $nb;             } elsif ($ch eq 'w') {                 my $more = 1;                 while ($more) {                     die "Unterminated compressed integer in unpack\n" if $$si_ref >= $slen;                     my $b = ord(substr($s, $$si_ref++, 1));                     $more = $b & 0x80; $slash_n = ($slash_n<<7)|($b&0x7F);                 }             } elsif ($ch eq 'Z') {                 # Z*/...: read null-terminated decimal count string                 my $end = index($s, "\0", $$si_ref);                 if ($end < 0) { $end = $slen; }  # no null â read to end                 my $raw = substr($s, $$si_ref, $end - $$si_ref);                 $$si_ref = $end + 1;  # skip past the null byte                 $$si_ref = $slen if $$si_ref > $slen;                 $slash_n = $raw + 0;             } else {                 my $n = $all ? ($slen-$$si_ref) : $nrep;                 my $raw = $$si_ref < $slen ? substr($s, $$si_ref, $n) : '';                 $$si_ref += $n;                 $raw =~ s/[ \x00]+$// if $ch eq 'A';                 $slash_n = $raw + 0;  # numeric value of the count string             }             # Process the data field(s). Loop to support chained slashes: A /A /A ...             while (1) {                 $ti = _pack_skip_ws($tmpl, $ti); last if $ti >= $tlen;                 my $dch = substr($tmpl, $ti, 1); $ti++;                 my ($dbang, $dbe2, $dle2) = _pack_parse_mods($tmpl, \$ti, $be, $le, $dch, 'unpack');                 $ti = _pack_skip_ws($tmpl, $ti);                 my ($dall, $dcnt, $dnrep) = _pack_parse_count($tmpl, \$ti);                 $ti = _pack_skip_ws($tmpl, $ti);                  # Check for another chained slash: this data field is itself a count                 my $chain = ($ti < $tlen && substr($tmpl, $ti, 1) eq '/');                 if ($chain) { $ti++ }                  my ($dnb, $dsig, $ddbe) = _pack_type_info($dch, $dbang);                 if ($chain) {                     # This data field is a count for the next slash â read it, don't push                     if ($dnb) {                         my $dbe3 = $dbe2 ? 1 : ($dle2 ? 0 : $ddbe);                         die "length/code after end of string in unpack\n"                             if $$si_ref + $dnb > $slen;                         $slash_n = _unpack_read_int($s, $$si_ref, $dnb, $dbe3, $dsig);                         $$si_ref += $dnb;                     } elsif ($dch eq 'w') {                         $slash_n = 0;                         my $more = 1;                         while ($more) {                             last if $$si_ref >= $slen;                             my $b = ord(substr($s, $$si_ref++, 1));                             $more = $b & 0x80; $slash_n = ($slash_n<<7)|($b&0x7F);                         }                     } else {                         # String type as intermediate count                         my $raw2 = $$si_ref < $slen ? substr($s, $$si_ref, $slash_n) : '';                         $$si_ref += $slash_n;                         $raw2 =~ s/[ \x00]+$// if $dch eq 'A';                         $raw2 =~ s/\x00.*//s   if $dch eq 'Z';                         $slash_n = $raw2 + 0;                     }                     # Loop: continue to process the next data field                 } else {                     # Final data field â read and push                     if ($dnb) {                         my $dbe3 = $dbe2 ? 1 : ($dle2 ? 0 : $ddbe);                         for (my $i=0; $i<$slash_n && $$si_ref+$dnb<=$slen; $i++) {                             $push_val->(_unpack_read_int($s, $$si_ref, $dnb, $dbe3, $dsig));                             $$si_ref += $dnb;                         }                     } elsif ($dch eq 'A'||$dch eq 'a'||$dch eq 'Z'                              ||$dch eq 'B'||$dch eq 'b'||$dch eq 'H'||$dch eq 'h'                              ||$dch eq 'u'||$dch eq 'U') {                         _unpack_str($dch, $slash_n, 0, $s, $si_ref, $push_val, $checksum_p);                     } elsif ($dch eq '(') {                         my $ge = _pack_find_group_end($tmpl, $ti);                         my $inner = substr($tmpl, $ti, $ge - $ti); $ti = $ge + 1;                         for (my $r=0; $r<$slash_n; $r++) {                             my $iter_base = $$si_ref;                             _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1);                         }                     }                     last;  # exit the chain loop                 }             }             next;         }          # Group         if (defined $grpbeg) {             my $inner = substr($tmpl, $grpbeg, $grpend - $grpbeg);             my $gti = _pack_skip_ws($inner, 0);             if ($gti < length($inner)) {                 my $fc = substr($inner, $gti, 1);                 die "\(\)-group starts with a count in unpack\n" if $fc =~ /^[\d\*\[]/;             }             if ($all) {                 while ($$si_ref < $slen) {                     my $si_before = $$si_ref;                     my $iter_base = $$si_ref;                     _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1);                     last if $$si_ref == $si_before;  # no progress: avoid infinite loop                 }             } else {                 for (my $r=0; $r<$nrep; $r++) {                     my $iter_base = $$si_ref;                     _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1);                 }             }             next;         }          # Position/skip (no push)         if ($ch eq 'x') {             if ($bang) {                 # x!N: advance to N-byte alignment                 my $n = $nrep > 0 ? $nrep : 1;                 $$si_ref += ($n - ($$si_ref % $n)) % $n;             } elsif ($all) { $$si_ref = $slen }             else { $$si_ref += $nrep }             next;         }         if ($ch eq 'X') {             if ($bang) {                 # X!N: back up to N-byte alignment                 my $n = $nrep > 0 ? $nrep : 1;                 $$si_ref = int($$si_ref / $n) * $n;             } else { $$si_ref -= $nrep; $$si_ref = 0 if $$si_ref < 0 }             next;         }         # @N: relative to group's base position. @!N: absolute byte position.         if ($ch eq '@') {             my $n = defined($count) ? $count : 0;             $$si_ref = $bang ? $n : $group_base + $n;             next;         }         if ($ch eq '%' || $ch eq '!' ) { next }         if ($ch eq 'p'||$ch eq 'P'||$ch eq 'D') { die "Invalid type '$ch' in unpack\n" }         if ($ch eq '.') {             # Position format: pushes current offset, does not advance.             # .* = absolute pos from string start.             # . / .1 = relative to innermost group (group_base).             # .0 = self offset = 0.             # .N (N>=2) = would need full group stack; approximate with absolute.             if ($all) {                 $push_val->($$si_ref);             } elsif (defined($count) && $count == 0) {                 $push_val->(0);             } elsif (defined($count) && $count >= 2) {                 $push_val->($$si_ref);  # approximate: absolute             } else {                 $push_val->($$si_ref - $group_base);  # relative to innermost group             }             next;         }          # Integer types         my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang);         if ($nb) {             my $be2 = $be ? 1 : ($le ? 0 : $dbe);             my $n = $all ? int(($slen-$$si_ref)/$nb) : $nrep;             for (my $i=0; $i<$n; $i++) {                 last if $$si_ref + $nb > $slen;                 $push_val->(_unpack_read_int($s, $$si_ref, $nb, $be2, $sig));                 $$si_ref += $nb;             }             next;         }          # Float types         if ($ch eq 'f') {             my $be2 = $be ? 1 : ($le ? 0 : 0);             my $n = $all ? int(($slen-$$si_ref)/4) : $nrep;             for (my $i=0; $i<$n; $i++) {                 last if $$si_ref + 4 > $slen;                 $push_val->(_unpack_float32($s, $$si_ref, $be2)); $$si_ref += 4;             }             next;         }         if ($ch eq 'd'||$ch eq 'F') {             my $be2 = $be ? 1 : ($le ? 0 : 0);             my $n = $all ? int(($slen-$$si_ref)/8) : $nrep;             for (my $i=0; $i<$n; $i++) {                 last if $$si_ref + 8 > $slen;                 $push_val->(_unpack_float64($s, $$si_ref, $be2)); $$si_ref += 8;             }             next;         }          # String/bit/etc types         if ($ch eq 'A'||$ch eq 'a'||$ch eq 'Z'||$ch eq 'H'||$ch eq 'h'||             $ch eq 'B'||$ch eq 'b'||$ch eq 'u'||$ch eq 'U'||$ch eq 'W'||$ch eq 'w') {             _unpack_str($ch, $nrep, $all, $s, $si_ref, $push_val, $checksum_p);             next;         }          die "'/' must follow a numeric type in unpack\n" if $ch eq '/';         die "Invalid type '$ch' in unpack\n";     }
                (p-while 1
                  (let (($ch (make-p-box nil)) ($grpbeg (make-p-box nil)) ($grpend (make-p-box nil)) ($bang (make-p-box nil)) ($be (make-p-box nil)) ($le (make-p-box nil)) ($ti_before_count (make-p-box nil)) ($all (make-p-box nil)) ($count (make-p-box nil)) ($nrep (make-p-box nil)) ($had_count (make-p-box nil)) ($nb (make-p-box nil)) ($sig (make-p-box nil)) ($dbe (make-p-box nil)) ($slash_n (make-p-box nil)) ($be2 (make-p-box nil)) ($more (make-p-box nil)) ($b (make-p-box nil)) ($end (make-p-box nil)) ($raw (make-p-box nil)) ($n (make-p-box nil)) ($dch (make-p-box nil)) ($dbang (make-p-box nil)) ($dbe2 (make-p-box nil)) ($dle2 (make-p-box nil)) ($dall (make-p-box nil)) ($dcnt (make-p-box nil)) ($dnrep (make-p-box nil)) ($chain (make-p-box nil)) ($dnb (make-p-box nil)) ($dsig (make-p-box nil)) ($ddbe (make-p-box nil)) ($dbe3 (make-p-box nil)) ($raw2 (make-p-box nil)) ($i (make-p-box nil)) ($ge (make-p-box nil)) ($inner (make-p-box nil)) ($r (make-p-box nil)) ($iter_base (make-p-box nil)) ($gti (make-p-box nil)) ($fc (make-p-box nil)) ($si_before (make-p-box nil)))
                    ;; $ti = _pack_skip_ws($tmpl, $ti)
                                        (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                    
                    ;; last if $ti >= $tlen
                    (p-if (p->= $ti $tlen) (p-last))
                    
                    ;; my $ch = substr($tmpl, $ti, 1)
                                        (p-my-= $ch (p-substr $tmpl $ti 1))
                    
                    ;; $ti++
                                        (p-post++ $ti)
                    
                    ;; my ($grpbeg, $grpend) = (undef, undef)
                                        (let ((*wantarray* nil)) (p-list-= (vector $grpbeg $grpend) (vector (p-undef) (p-undef))))
                    
                    ;; if ($ch eq '(') {             $grpend = _pack_find_group_end($tmpl, $ti);             $grpbeg = $ti; $ti = $grpend + 1; $ch = '(';         }
                    ;; if ($ch eq '(')
                    (p-if (p-str-eq $ch "(")
                      (progn
                        ;; $grpend = _pack_find_group_end($tmpl, $ti)
                                                (p-my-= $grpend (pl-_pack_find_group_end $tmpl $ti))
                        
                        ;; $grpbeg = $ti
                                                (p-my-= $grpbeg $ti)
                        
                        ;; $ti = $grpend + 1
                                                (p-my-= $ti (p-+ $grpend 1))
                        
                        ;; $ch = '('
                                                (p-my-= $ch "(")
                        
                      )
                      nil
                    )
                    
                    ;; my ($bang, $be, $le) = _pack_parse_mods($tmpl, \$ti, $inh_be, $inh_le, $ch, 'unpack')
                                        (let ((*wantarray* nil)) (p-list-= (vector $bang $be $le) (let ((*wantarray* t)) (pl-_pack_parse_mods $tmpl (p-backslash $ti) $inh_be $inh_le $ch "unpack"))))
                    
                    ;; my $ti_before_count = $ti
                                        (p-my-= $ti_before_count $ti)
                    
                    ;; my ($all, $count, $nrep) = _pack_parse_count($tmpl, \$ti)
                                        (let ((*wantarray* nil)) (p-list-= (vector $all $count $nrep) (let ((*wantarray* t)) (pl-_pack_parse_count $tmpl (p-backslash $ti)))))
                    
                    ;; my $had_count = ($all || $ti > $ti_before_count)
                                        (p-my-= $had_count (p-|| $all (p-> $ti $ti_before_count)))
                    
                    ;; $ti = _pack_skip_ws($tmpl, $ti)
                    (let ((*wantarray* :void))                     (p-my-= $ti (let ((*wantarray* nil)) (pl-_pack_skip_ws $tmpl $ti))))
                    
                    ;; if ($ti < $tlen && substr($tmpl, $ti, 1) eq '/') {             $ti++;             $ti = _pack_skip_ws($tmpl, $ti);             die "Code missing after '/' in unpack\n" if $ti >= $tlen;             # '/' must not have a count applied directly to it (e.g. c/*a or c/1a are invalid;             # Z*/A* is valid because * is the count for Z, not for /).             { my $c = substr($tmpl, $ti, 1);               die "'/' does not take a repeat count in unpack\n"                 if $c eq '*' || $c eq '[' || $c =~ /\d/; }             my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang);             my $slash_n = 0;             if ($nb) {                 my $be2 = $be ? 1 : ($le ? 0 : $dbe);                 if ($$si_ref + $nb > $slen) {                     last unless $depth > 0;                     die "length/code after end of string in unpack\n";                 }                 $slash_n = _unpack_read_int($s, $$si_ref, $nb, $be2, $sig);                 $$si_ref += $nb;             } elsif ($ch eq 'w') {                 my $more = 1;                 while ($more) {                     die "Unterminated compressed integer in unpack\n" if $$si_ref >= $slen;                     my $b = ord(substr($s, $$si_ref++, 1));                     $more = $b & 0x80; $slash_n = ($slash_n<<7)|($b&0x7F);                 }             } elsif ($ch eq 'Z') {                 # Z*/...: read null-terminated decimal count string                 my $end = index($s, "\0", $$si_ref);                 if ($end < 0) { $end = $slen; }  # no null â read to end                 my $raw = substr($s, $$si_ref, $end - $$si_ref);                 $$si_ref = $end + 1;  # skip past the null byte                 $$si_ref = $slen if $$si_ref > $slen;                 $slash_n = $raw + 0;             } else {                 my $n = $all ? ($slen-$$si_ref) : $nrep;                 my $raw = $$si_ref < $slen ? substr($s, $$si_ref, $n) : '';                 $$si_ref += $n;                 $raw =~ s/[ \x00]+$// if $ch eq 'A';                 $slash_n = $raw + 0;  # numeric value of the count string             }             # Process the data field(s). Loop to support chained slashes: A /A /A ...             while (1) {                 $ti = _pack_skip_ws($tmpl, $ti); last if $ti >= $tlen;                 my $dch = substr($tmpl, $ti, 1); $ti++;                 my ($dbang, $dbe2, $dle2) = _pack_parse_mods($tmpl, \$ti, $be, $le, $dch, 'unpack');                 $ti = _pack_skip_ws($tmpl, $ti);                 my ($dall, $dcnt, $dnrep) = _pack_parse_count($tmpl, \$ti);                 $ti = _pack_skip_ws($tmpl, $ti);                  # Check for another chained slash: this data field is itself a count                 my $chain = ($ti < $tlen && substr($tmpl, $ti, 1) eq '/');                 if ($chain) { $ti++ }                  my ($dnb, $dsig, $ddbe) = _pack_type_info($dch, $dbang);                 if ($chain) {                     # This data field is a count for the next slash â read it, don't push                     if ($dnb) {                         my $dbe3 = $dbe2 ? 1 : ($dle2 ? 0 : $ddbe);                         die "length/code after end of string in unpack\n"                             if $$si_ref + $dnb > $slen;                         $slash_n = _unpack_read_int($s, $$si_ref, $dnb, $dbe3, $dsig);                         $$si_ref += $dnb;                     } elsif ($dch eq 'w') {                         $slash_n = 0;                         my $more = 1;                         while ($more) {                             last if $$si_ref >= $slen;                             my $b = ord(substr($s, $$si_ref++, 1));                             $more = $b & 0x80; $slash_n = ($slash_n<<7)|($b&0x7F);                         }                     } else {                         # String type as intermediate count                         my $raw2 = $$si_ref < $slen ? substr($s, $$si_ref, $slash_n) : '';                         $$si_ref += $slash_n;                         $raw2 =~ s/[ \x00]+$// if $dch eq 'A';                         $raw2 =~ s/\x00.*//s   if $dch eq 'Z';                         $slash_n = $raw2 + 0;                     }                     # Loop: continue to process the next data field                 } else {                     # Final data field â read and push                     if ($dnb) {                         my $dbe3 = $dbe2 ? 1 : ($dle2 ? 0 : $ddbe);                         for (my $i=0; $i<$slash_n && $$si_ref+$dnb<=$slen; $i++) {                             $push_val->(_unpack_read_int($s, $$si_ref, $dnb, $dbe3, $dsig));                             $$si_ref += $dnb;                         }                     } elsif ($dch eq 'A'||$dch eq 'a'||$dch eq 'Z'                              ||$dch eq 'B'||$dch eq 'b'||$dch eq 'H'||$dch eq 'h'                              ||$dch eq 'u'||$dch eq 'U') {                         _unpack_str($dch, $slash_n, 0, $s, $si_ref, $push_val, $checksum_p);                     } elsif ($dch eq '(') {                         my $ge = _pack_find_group_end($tmpl, $ti);                         my $inner = substr($tmpl, $ti, $ge - $ti); $ti = $ge + 1;                         for (my $r=0; $r<$slash_n; $r++) {                             my $iter_base = $$si_ref;                             _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1);                         }                     }                     last;  # exit the chain loop                 }             }             next;         }
                    ;; if ($ti < $tlen && substr($tmpl, $ti, 1) eq '/')
                    (p-if (p-&& (p-< $ti $tlen) (p-str-eq (p-substr $tmpl $ti 1) "/"))
                      (progn
                        (let (($nb (make-p-box nil)) ($sig (make-p-box nil)) ($dbe (make-p-box nil)) ($slash_n (make-p-box nil)) ($be2 (make-p-box nil)) ($more (make-p-box nil)) ($b (make-p-box nil)) ($end (make-p-box nil)) ($raw (make-p-box nil)) ($n (make-p-box nil)) ($dch (make-p-box nil)) ($dbang (make-p-box nil)) ($dbe2 (make-p-box nil)) ($dle2 (make-p-box nil)) ($dall (make-p-box nil)) ($dcnt (make-p-box nil)) ($dnrep (make-p-box nil)) ($chain (make-p-box nil)) ($dnb (make-p-box nil)) ($dsig (make-p-box nil)) ($ddbe (make-p-box nil)) ($dbe3 (make-p-box nil)) ($raw2 (make-p-box nil)) ($i (make-p-box nil)) ($ge (make-p-box nil)) ($inner (make-p-box nil)) ($r (make-p-box nil)) ($iter_base (make-p-box nil)))
                          ;; $ti++
                          (let ((*wantarray* :void))                           (p-post++ $ti))
                          
                          ;; $ti = _pack_skip_ws($tmpl, $ti)
                          (let ((*wantarray* :void))                           (p-my-= $ti (let ((*wantarray* nil)) (pl-_pack_skip_ws $tmpl $ti))))
                          
                          ;; die "Code missing after '/' in unpack\n" if $ti >= $tlen
                          (let ((*wantarray* :void)) (p-if (p->= $ti $tlen) (p-die :loc "- line 881" "Code missing after '/' in unpack
")))
                          
                          ;; { ... }
                          (let ((*package* *package*))
                            (let (($c (make-p-box nil)))
                              (block nil
                                (tagbody :redo
                                  (let ((--pcl-if-ret--17 nil))
                                    ;; my $c = substr($tmpl, $ti, 1)
                                                                        (p-my-= $c (p-substr $tmpl $ti 1))
                                    
                                    ;; die "'/' does not take a repeat count in unpack\n"                 if $c eq '*' || $c eq '[' || $c =~ /\d/
                                    (p-if (setf --pcl-if-ret--17 (p-|| (p-|| (p-str-eq $c "*") (p-str-eq $c "[")) (let ((*wantarray* nil)) (p-=~ $c (p-regex "/\\d/")))))
                                      (setf --pcl-if-ret--17 (p-die :loc "- line 885" "'/' does not take a repeat count in unpack
"))
                                      nil)
                                    
                                  --pcl-if-ret--17)
                                  :next)
                              )
                            )
                          )
                          
                          ;; my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang)
                                                    (let ((*wantarray* nil)) (p-list-= (vector $nb $sig $dbe) (let ((*wantarray* t)) (pl-_pack_type_info $ch $bang))))
                          
                          ;; my $slash_n = 0
                                                    (p-my-= $slash_n 0)
                          
                          ;; if ($nb) {                 my $be2 = $be ? 1 : ($le ? 0 : $dbe);                 if ($$si_ref + $nb > $slen) {                     last unless $depth > 0;                     die "length/code after end of string in unpack\n";                 }                 $slash_n = _unpack_read_int($s, $$si_ref, $nb, $be2, $sig);                 $$si_ref += $nb;             } elsif ($ch eq 'w') {                 my $more = 1;                 while ($more) {                     die "Unterminated compressed integer in unpack\n" if $$si_ref >= $slen;                     my $b = ord(substr($s, $$si_ref++, 1));                     $more = $b & 0x80; $slash_n = ($slash_n<<7)|($b&0x7F);                 }             } elsif ($ch eq 'Z') {                 # Z*/...: read null-terminated decimal count string                 my $end = index($s, "\0", $$si_ref);                 if ($end < 0) { $end = $slen; }  # no null â read to end                 my $raw = substr($s, $$si_ref, $end - $$si_ref);                 $$si_ref = $end + 1;  # skip past the null byte                 $$si_ref = $slen if $$si_ref > $slen;                 $slash_n = $raw + 0;             } else {                 my $n = $all ? ($slen-$$si_ref) : $nrep;                 my $raw = $$si_ref < $slen ? substr($s, $$si_ref, $n) : '';                 $$si_ref += $n;                 $raw =~ s/[ \x00]+$// if $ch eq 'A';                 $slash_n = $raw + 0;  # numeric value of the count string             }
                          ;; if ($nb)
                          (p-if $nb
                            (progn
                              (let (($be2 (make-p-box nil)))
                                ;; my $be2 = $be ? 1 : ($le ? 0 : $dbe)
                                                                (p-my-= $be2 (p-if $be 1 (p-if $le 0 $dbe)))
                                
                                ;; if ($$si_ref + $nb > $slen) {                     last unless $depth > 0;                     die "length/code after end of string in unpack\n";                 }
                                ;; if ($$si_ref + $nb > $slen)
                                (p-if (p-> (p-+ (p-cast-$ $si_ref) $nb) $slen)
                                  (progn
                                    ;; last unless $depth > 0
                                    (let ((*wantarray* :void)) (p-unless (p-> $depth 0) (p-last)))
                                    
                                    ;; die "length/code after end of string in unpack\n"
                                                                        (p-die :loc "- line 893" "length/code after end of string in unpack
")
                                    
                                  )
                                  nil
                                )
                                
                                ;; $slash_n = _unpack_read_int($s, $$si_ref, $nb, $be2, $sig)
                                (let ((*wantarray* :void))                                 (p-my-= $slash_n (let ((*wantarray* nil)) (pl-_unpack_read_int $s (p-cast-$ $si_ref) $nb $be2 $sig))))
                                
                                ;; $$si_ref += $nb
                                                                (p-incf (p-cast-$ $si_ref) $nb)
                                
                              )
                            )
                            ;; elsif ($ch eq 'w')
                            (p-if (p-str-eq $ch "w")
                              (progn
                                (let (($more (make-p-box nil)) ($b (make-p-box nil)))
                                  ;; my $more = 1
                                                                    (p-my-= $more 1)
                                  
                                  ;; while ($more) {                     die "Unterminated compressed integer in unpack\n" if $$si_ref >= $slen;                     my $b = ord(substr($s, $$si_ref++, 1));                     $more = $b & 0x80; $slash_n = ($slash_n<<7)|($b&0x7F);                 }
                                  (p-while $more
                                    (let (($b (make-p-box nil)))
                                      ;; die "Unterminated compressed integer in unpack\n" if $$si_ref >= $slen
                                      (p-if (p->= (p-cast-$ $si_ref) $slen) (p-die :loc "- line 900" "Unterminated compressed integer in unpack
"))
                                      
                                      ;; my $b = ord(substr($s, $$si_ref++, 1))
                                                                            (p-my-= $b (p-ord (p-substr $s (p-post++ (p-cast-$ $si_ref)) 1)))
                                      
                                      ;; $more = $b & 0x80
                                                                            (p-my-= $more (p-bit-and $b #x80))
                                      
                                      ;; $slash_n = ($slash_n<<7)|($b&0x7F)
                                                                            (p-my-= $slash_n (p-bit-or (p-<< $slash_n 7) (p-bit-and $b #x7F)))
                                      
                                    )
                                  )
                                  
                                )
                              )
                              ;; elsif ($ch eq 'Z')
                              (p-if (p-str-eq $ch "Z")
                                (progn
                                  (let (($end (make-p-box nil)) ($raw (make-p-box nil)))
                                    ;; my $end = index($s, "\0", $$si_ref)
                                                                        (p-my-= $end (p-index $s " " (p-cast-$ $si_ref)))
                                    
                                    ;; if ($end < 0) { $end = $slen; }
                                    ;; if ($end < 0)
                                    (p-if (p-< $end 0)
                                      (progn
                                        ;; $end = $slen
                                                                                (p-my-= $end $slen)
                                        
                                      )
                                      nil
                                    )
                                    
                                    ;; my $raw = substr($s, $$si_ref, $end - $$si_ref)
                                                                        (p-my-= $raw (p-substr $s (p-cast-$ $si_ref) (p-- $end (p-cast-$ $si_ref))))
                                    
                                    ;; $$si_ref = $end + 1
                                    (let ((*wantarray* :void))                                     (p-setf (p-cast-$ $si_ref) (p-+ $end 1)))
                                    
                                    ;; $$si_ref = $slen if $$si_ref > $slen
                                    (let ((*wantarray* :void)) (p-if (p-> (p-cast-$ $si_ref) $slen) (p-setf (p-cast-$ $si_ref) $slen)))
                                    
                                    ;; $slash_n = $raw + 0
                                                                        (p-my-= $slash_n (p-+ $raw 0))
                                    
                                  )
                                )
                                ;; else
                                (progn
                                  (let (($n (make-p-box nil)) ($raw (make-p-box nil)))
                                    ;; my $n = $all ? ($slen-$$si_ref) : $nrep
                                                                        (p-my-= $n (p-if $all (p-- $slen (p-cast-$ $si_ref)) $nrep))
                                    
                                    ;; my $raw = $$si_ref < $slen ? substr($s, $$si_ref, $n) : ''
                                                                        (p-my-= $raw (p-if (p-< (p-cast-$ $si_ref) $slen) (p-substr $s (p-cast-$ $si_ref) $n) ""))
                                    
                                    ;; $$si_ref += $n
                                    (let ((*wantarray* :void))                                     (p-incf (p-cast-$ $si_ref) $n))
                                    
                                    ;; $raw =~ s/[ \x00]+$// if $ch eq 'A'
                                    (let ((*wantarray* :void)) (p-if (p-str-eq $ch "A") (p-=~ $raw (p-subst "[ \\x00]+$" ""))))
                                    
                                    ;; $slash_n = $raw + 0
                                                                        (p-my-= $slash_n (p-+ $raw 0))
                                    
                                  )
                                )
                              )
                            )
                          )
                          
                          ;; while (1) {                 $ti = _pack_skip_ws($tmpl, $ti); last if $ti >= $tlen;                 my $dch = substr($tmpl, $ti, 1); $ti++;                 my ($dbang, $dbe2, $dle2) = _pack_parse_mods($tmpl, \$ti, $be, $le, $dch, 'unpack');                 $ti = _pack_skip_ws($tmpl, $ti);                 my ($dall, $dcnt, $dnrep) = _pack_parse_count($tmpl, \$ti);                 $ti = _pack_skip_ws($tmpl, $ti);                  # Check for another chained slash: this data field is itself a count                 my $chain = ($ti < $tlen && substr($tmpl, $ti, 1) eq '/');                 if ($chain) { $ti++ }                  my ($dnb, $dsig, $ddbe) = _pack_type_info($dch, $dbang);                 if ($chain) {                     # This data field is a count for the next slash â read it, don't push                     if ($dnb) {                         my $dbe3 = $dbe2 ? 1 : ($dle2 ? 0 : $ddbe);                         die "length/code after end of string in unpack\n"                             if $$si_ref + $dnb > $slen;                         $slash_n = _unpack_read_int($s, $$si_ref, $dnb, $dbe3, $dsig);                         $$si_ref += $dnb;                     } elsif ($dch eq 'w') {                         $slash_n = 0;                         my $more = 1;                         while ($more) {                             last if $$si_ref >= $slen;                             my $b = ord(substr($s, $$si_ref++, 1));                             $more = $b & 0x80; $slash_n = ($slash_n<<7)|($b&0x7F);                         }                     } else {                         # String type as intermediate count                         my $raw2 = $$si_ref < $slen ? substr($s, $$si_ref, $slash_n) : '';                         $$si_ref += $slash_n;                         $raw2 =~ s/[ \x00]+$// if $dch eq 'A';                         $raw2 =~ s/\x00.*//s   if $dch eq 'Z';                         $slash_n = $raw2 + 0;                     }                     # Loop: continue to process the next data field                 } else {                     # Final data field â read and push                     if ($dnb) {                         my $dbe3 = $dbe2 ? 1 : ($dle2 ? 0 : $ddbe);                         for (my $i=0; $i<$slash_n && $$si_ref+$dnb<=$slen; $i++) {                             $push_val->(_unpack_read_int($s, $$si_ref, $dnb, $dbe3, $dsig));                             $$si_ref += $dnb;                         }                     } elsif ($dch eq 'A'||$dch eq 'a'||$dch eq 'Z'                              ||$dch eq 'B'||$dch eq 'b'||$dch eq 'H'||$dch eq 'h'                              ||$dch eq 'u'||$dch eq 'U') {                         _unpack_str($dch, $slash_n, 0, $s, $si_ref, $push_val, $checksum_p);                     } elsif ($dch eq '(') {                         my $ge = _pack_find_group_end($tmpl, $ti);                         my $inner = substr($tmpl, $ti, $ge - $ti); $ti = $ge + 1;                         for (my $r=0; $r<$slash_n; $r++) {                             my $iter_base = $$si_ref;                             _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1);                         }                     }                     last;  # exit the chain loop                 }             }
                          (p-while 1
                            (let (($dch (make-p-box nil)) ($dbang (make-p-box nil)) ($dbe2 (make-p-box nil)) ($dle2 (make-p-box nil)) ($dall (make-p-box nil)) ($dcnt (make-p-box nil)) ($dnrep (make-p-box nil)) ($chain (make-p-box nil)) ($dnb (make-p-box nil)) ($dsig (make-p-box nil)) ($ddbe (make-p-box nil)) ($dbe3 (make-p-box nil)) ($more (make-p-box nil)) ($b (make-p-box nil)) ($raw2 (make-p-box nil)) ($i (make-p-box nil)) ($ge (make-p-box nil)) ($inner (make-p-box nil)) ($r (make-p-box nil)) ($iter_base (make-p-box nil)))
                              ;; $ti = _pack_skip_ws($tmpl, $ti)
                              (let ((*wantarray* :void))                               (p-my-= $ti (let ((*wantarray* nil)) (pl-_pack_skip_ws $tmpl $ti))))
                              
                              ;; last if $ti >= $tlen
                              (let ((*wantarray* :void)) (p-if (p->= $ti $tlen) (p-last)))
                              
                              ;; my $dch = substr($tmpl, $ti, 1)
                                                            (p-my-= $dch (p-substr $tmpl $ti 1))
                              
                              ;; $ti++
                              (let ((*wantarray* :void))                               (p-post++ $ti))
                              
                              ;; my ($dbang, $dbe2, $dle2) = _pack_parse_mods($tmpl, \$ti, $be, $le, $dch, 'unpack')
                                                            (let ((*wantarray* nil)) (p-list-= (vector $dbang $dbe2 $dle2) (let ((*wantarray* t)) (pl-_pack_parse_mods $tmpl (p-backslash $ti) $be $le $dch "unpack"))))
                              
                              ;; $ti = _pack_skip_ws($tmpl, $ti)
                              (let ((*wantarray* :void))                               (p-my-= $ti (let ((*wantarray* nil)) (pl-_pack_skip_ws $tmpl $ti))))
                              
                              ;; my ($dall, $dcnt, $dnrep) = _pack_parse_count($tmpl, \$ti)
                                                            (let ((*wantarray* nil)) (p-list-= (vector $dall $dcnt $dnrep) (let ((*wantarray* t)) (pl-_pack_parse_count $tmpl (p-backslash $ti)))))
                              
                              ;; $ti = _pack_skip_ws($tmpl, $ti)
                              (let ((*wantarray* :void))                               (p-my-= $ti (let ((*wantarray* nil)) (pl-_pack_skip_ws $tmpl $ti))))
                              
                              ;; my $chain = ($ti < $tlen && substr($tmpl, $ti, 1) eq '/')
                                                            (p-my-= $chain (p-&& (p-< $ti $tlen) (p-str-eq (p-substr $tmpl $ti 1) "/")))
                              
                              ;; if ($chain) { $ti++ }
                              ;; if ($chain)
                              (p-if $chain
                                (progn
                                  ;; $ti++
                                                                    (p-post++ $ti)
                                  
                                )
                                nil
                              )
                              
                              ;; my ($dnb, $dsig, $ddbe) = _pack_type_info($dch, $dbang)
                                                            (let ((*wantarray* nil)) (p-list-= (vector $dnb $dsig $ddbe) (let ((*wantarray* t)) (pl-_pack_type_info $dch $dbang))))
                              
                              ;; if ($chain) {                     # This data field is a count for the next slash â read it, don't push                     if ($dnb) {                         my $dbe3 = $dbe2 ? 1 : ($dle2 ? 0 : $ddbe);                         die "length/code after end of string in unpack\n"                             if $$si_ref + $dnb > $slen;                         $slash_n = _unpack_read_int($s, $$si_ref, $dnb, $dbe3, $dsig);                         $$si_ref += $dnb;                     } elsif ($dch eq 'w') {                         $slash_n = 0;                         my $more = 1;                         while ($more) {                             last if $$si_ref >= $slen;                             my $b = ord(substr($s, $$si_ref++, 1));                             $more = $b & 0x80; $slash_n = ($slash_n<<7)|($b&0x7F);                         }                     } else {                         # String type as intermediate count                         my $raw2 = $$si_ref < $slen ? substr($s, $$si_ref, $slash_n) : '';                         $$si_ref += $slash_n;                         $raw2 =~ s/[ \x00]+$// if $dch eq 'A';                         $raw2 =~ s/\x00.*//s   if $dch eq 'Z';                         $slash_n = $raw2 + 0;                     }                     # Loop: continue to process the next data field                 } else {                     # Final data field â read and push                     if ($dnb) {                         my $dbe3 = $dbe2 ? 1 : ($dle2 ? 0 : $ddbe);                         for (my $i=0; $i<$slash_n && $$si_ref+$dnb<=$slen; $i++) {                             $push_val->(_unpack_read_int($s, $$si_ref, $dnb, $dbe3, $dsig));                             $$si_ref += $dnb;                         }                     } elsif ($dch eq 'A'||$dch eq 'a'||$dch eq 'Z'                              ||$dch eq 'B'||$dch eq 'b'||$dch eq 'H'||$dch eq 'h'                              ||$dch eq 'u'||$dch eq 'U') {                         _unpack_str($dch, $slash_n, 0, $s, $si_ref, $push_val, $checksum_p);                     } elsif ($dch eq '(') {                         my $ge = _pack_find_group_end($tmpl, $ti);                         my $inner = substr($tmpl, $ti, $ge - $ti); $ti = $ge + 1;                         for (my $r=0; $r<$slash_n; $r++) {                             my $iter_base = $$si_ref;                             _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1);                         }                     }                     last;  # exit the chain loop                 }
                              ;; if ($chain)
                              (p-if $chain
                                (progn
                                  (let (($dbe3 (make-p-box nil)) ($more (make-p-box nil)) ($b (make-p-box nil)) ($raw2 (make-p-box nil)))
                                    ;; if ($dnb) {                         my $dbe3 = $dbe2 ? 1 : ($dle2 ? 0 : $ddbe);                         die "length/code after end of string in unpack\n"                             if $$si_ref + $dnb > $slen;                         $slash_n = _unpack_read_int($s, $$si_ref, $dnb, $dbe3, $dsig);                         $$si_ref += $dnb;                     } elsif ($dch eq 'w') {                         $slash_n = 0;                         my $more = 1;                         while ($more) {                             last if $$si_ref >= $slen;                             my $b = ord(substr($s, $$si_ref++, 1));                             $more = $b & 0x80; $slash_n = ($slash_n<<7)|($b&0x7F);                         }                     } else {                         # String type as intermediate count                         my $raw2 = $$si_ref < $slen ? substr($s, $$si_ref, $slash_n) : '';                         $$si_ref += $slash_n;                         $raw2 =~ s/[ \x00]+$// if $dch eq 'A';                         $raw2 =~ s/\x00.*//s   if $dch eq 'Z';                         $slash_n = $raw2 + 0;                     }
                                    ;; if ($dnb)
                                    (p-if $dnb
                                      (progn
                                        (let (($dbe3 (make-p-box nil)))
                                          ;; my $dbe3 = $dbe2 ? 1 : ($dle2 ? 0 : $ddbe)
                                                                                    (p-my-= $dbe3 (p-if $dbe2 1 (p-if $dle2 0 $ddbe)))
                                          
                                          ;; die "length/code after end of string in unpack\n"                             if $$si_ref + $dnb > $slen
                                          (p-if (p-> (p-+ (p-cast-$ $si_ref) $dnb) $slen) (p-die :loc "- line 937" "length/code after end of string in unpack
"))
                                          
                                          ;; $slash_n = _unpack_read_int($s, $$si_ref, $dnb, $dbe3, $dsig)
                                                                                    (p-my-= $slash_n (pl-_unpack_read_int $s (p-cast-$ $si_ref) $dnb $dbe3 $dsig))
                                          
                                          ;; $$si_ref += $dnb
                                                                                    (p-incf (p-cast-$ $si_ref) $dnb)
                                          
                                        )
                                      )
                                      ;; elsif ($dch eq 'w')
                                      (p-if (p-str-eq $dch "w")
                                        (progn
                                          (let (($more (make-p-box nil)) ($b (make-p-box nil)))
                                            ;; $slash_n = 0
                                            (let ((*wantarray* :void))                                             (p-my-= $slash_n 0))
                                            
                                            ;; my $more = 1
                                                                                        (p-my-= $more 1)
                                            
                                            ;; while ($more) {                             last if $$si_ref >= $slen;                             my $b = ord(substr($s, $$si_ref++, 1));                             $more = $b & 0x80; $slash_n = ($slash_n<<7)|($b&0x7F);                         }
                                            (p-while $more
                                              (let (($b (make-p-box nil)))
                                                ;; last if $$si_ref >= $slen
                                                (p-if (p->= (p-cast-$ $si_ref) $slen) (p-last))
                                                
                                                ;; my $b = ord(substr($s, $$si_ref++, 1))
                                                                                                (p-my-= $b (p-ord (p-substr $s (p-post++ (p-cast-$ $si_ref)) 1)))
                                                
                                                ;; $more = $b & 0x80
                                                                                                (p-my-= $more (p-bit-and $b #x80))
                                                
                                                ;; $slash_n = ($slash_n<<7)|($b&0x7F)
                                                                                                (p-my-= $slash_n (p-bit-or (p-<< $slash_n 7) (p-bit-and $b #x7F)))
                                                
                                              )
                                            )
                                            
                                          )
                                        )
                                        ;; else
                                        (progn
                                          (let (($raw2 (make-p-box nil)))
                                            ;; my $raw2 = $$si_ref < $slen ? substr($s, $$si_ref, $slash_n) : ''
                                                                                        (p-my-= $raw2 (p-if (p-< (p-cast-$ $si_ref) $slen) (p-substr $s (p-cast-$ $si_ref) $slash_n) ""))
                                            
                                            ;; $$si_ref += $slash_n
                                            (let ((*wantarray* :void))                                             (p-incf (p-cast-$ $si_ref) $slash_n))
                                            
                                            ;; $raw2 =~ s/[ \x00]+$// if $dch eq 'A'
                                            (let ((*wantarray* :void)) (p-if (p-str-eq $dch "A") (p-=~ $raw2 (p-subst "[ \\x00]+$" ""))))
                                            
                                            ;; $raw2 =~ s/\x00.*//s   if $dch eq 'Z'
                                            (let ((*wantarray* :void)) (p-if (p-str-eq $dch "Z") (p-=~ $raw2 (p-subst "\\x00.*" "" :s))))
                                            
                                            ;; $slash_n = $raw2 + 0
                                                                                        (p-my-= $slash_n (p-+ $raw2 0))
                                            
                                          )
                                        )
                                      )
                                    )
                                    
                                  )
                                )
                                ;; else
                                (progn
                                  (let (($dbe3 (make-p-box nil)) ($i (make-p-box nil)) ($ge (make-p-box nil)) ($inner (make-p-box nil)) ($r (make-p-box nil)) ($iter_base (make-p-box nil)))
                                    ;; if ($dnb) {                         my $dbe3 = $dbe2 ? 1 : ($dle2 ? 0 : $ddbe);                         for (my $i=0; $i<$slash_n && $$si_ref+$dnb<=$slen; $i++) {                             $push_val->(_unpack_read_int($s, $$si_ref, $dnb, $dbe3, $dsig));                             $$si_ref += $dnb;                         }                     } elsif ($dch eq 'A'||$dch eq 'a'||$dch eq 'Z'                              ||$dch eq 'B'||$dch eq 'b'||$dch eq 'H'||$dch eq 'h'                              ||$dch eq 'u'||$dch eq 'U') {                         _unpack_str($dch, $slash_n, 0, $s, $si_ref, $push_val, $checksum_p);                     } elsif ($dch eq '(') {                         my $ge = _pack_find_group_end($tmpl, $ti);                         my $inner = substr($tmpl, $ti, $ge - $ti); $ti = $ge + 1;                         for (my $r=0; $r<$slash_n; $r++) {                             my $iter_base = $$si_ref;                             _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1);                         }                     }
                                    ;; if ($dnb)
                                    (p-if $dnb
                                      (progn
                                        (let (($dbe3 (make-p-box nil)) ($i (make-p-box nil)))
                                          ;; my $dbe3 = $dbe2 ? 1 : ($dle2 ? 0 : $ddbe)
                                                                                    (p-my-= $dbe3 (p-if $dbe2 1 (p-if $dle2 0 $ddbe)))
                                          
                                          ;; for (my $i=0; $i<$slash_n && $$si_ref+$dnb<=$slen; $i++) {                             $push_val->(_unpack_read_int($s, $$si_ref, $dnb, $dbe3, $dsig));                             $$si_ref += $dnb;                         }
                                          (let (($i (make-p-box nil)))
                                            (p-for (                                          (p-my-= $i 0))
                                                    (                                          (p-&& (p-< $i $slash_n) (p-<= (p-+ (p-cast-$ $si_ref) $dnb) $slen)))
                                                    (                                          (p-post++ $i))
                                              ;; $push_val->(_unpack_read_int($s, $$si_ref, $dnb, $dbe3, $dsig))
                                                                                            (p-funcall-ref $push_val (pl-_unpack_read_int $s (p-cast-$ $si_ref) $dnb $dbe3 $dsig))
                                              
                                              ;; $$si_ref += $dnb
                                                                                            (p-incf (p-cast-$ $si_ref) $dnb)
                                              
                                            )
                                          )
                                          
                                        )
                                      )
                                      ;; elsif ($dch eq 'A'||$dch eq 'a'||$dch eq 'Z'
;;                              ||$dch eq 'B'||$dch eq 'b'||$dch eq 'H'||$dch eq 'h'
;;                              ||$dch eq 'u'||$dch eq 'U')
                                      (p-if (p-|| (p-|| (p-|| (p-|| (p-|| (p-|| (p-|| (p-|| (p-str-eq $dch "A") (p-str-eq $dch "a")) (p-str-eq $dch "Z")) (p-str-eq $dch "B")) (p-str-eq $dch "b")) (p-str-eq $dch "H")) (p-str-eq $dch "h")) (p-str-eq $dch "u")) (p-str-eq $dch "U"))
                                        (progn
                                          ;; _unpack_str($dch, $slash_n, 0, $s, $si_ref, $push_val, $checksum_p)
                                                                                    (pl-_unpack_str $dch $slash_n 0 $s $si_ref $push_val $checksum_p)
                                          
                                        )
                                        ;; elsif ($dch eq '(')
                                        (p-if (p-str-eq $dch "(")
                                          (progn
                                            (let (($ge (make-p-box nil)) ($inner (make-p-box nil)) ($r (make-p-box nil)) ($iter_base (make-p-box nil)))
                                              ;; my $ge = _pack_find_group_end($tmpl, $ti)
                                                                                            (p-my-= $ge (let ((*wantarray* nil)) (pl-_pack_find_group_end $tmpl $ti)))
                                              
                                              ;; my $inner = substr($tmpl, $ti, $ge - $ti)
                                                                                            (p-my-= $inner (p-substr $tmpl $ti (p-- $ge $ti)))
                                              
                                              ;; $ti = $ge + 1
                                              (let ((*wantarray* :void))                                               (p-my-= $ti (p-+ $ge 1)))
                                              
                                              ;; for (my $r=0; $r<$slash_n; $r++) {                             my $iter_base = $$si_ref;                             _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1);                         }
                                              (let (($r (make-p-box nil)))
                                                (p-for (                                              (p-my-= $r 0))
                                                        (                                              (p-< $r $slash_n))
                                                        (                                              (p-post++ $r))
                                                  (let (($iter_base (make-p-box nil)))
                                                    ;; my $iter_base = $$si_ref
                                                                                                        (p-my-= $iter_base (p-cast-$ $si_ref))
                                                    
                                                    ;; _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1)
                                                                                                        (pl-_unpack_tmpl $inner $s $si_ref $push_val $be $le $checksum_p $iter_base (p-+ $depth 1))
                                                    
                                                  )
                                                )
                                              )
                                              
                                            )
                                          )
                                          nil
                                        )
                                      )
                                    )
                                    
                                    ;; last
                                                                        (p-last)
                                    
                                  )
                                )
                              )
                              
                            )
                          )
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if (defined $grpbeg) {             my $inner = substr($tmpl, $grpbeg, $grpend - $grpbeg);             my $gti = _pack_skip_ws($inner, 0);             if ($gti < length($inner)) {                 my $fc = substr($inner, $gti, 1);                 die "\(\)-group starts with a count in unpack\n" if $fc =~ /^[\d\*\[]/;             }             if ($all) {                 while ($$si_ref < $slen) {                     my $si_before = $$si_ref;                     my $iter_base = $$si_ref;                     _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1);                     last if $$si_ref == $si_before;  # no progress: avoid infinite loop                 }             } else {                 for (my $r=0; $r<$nrep; $r++) {                     my $iter_base = $$si_ref;                     _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1);                 }             }             next;         }
                    ;; if (defined $grpbeg)
                    (p-if (p-defined $grpbeg)
                      (progn
                        (let (($inner (make-p-box nil)) ($gti (make-p-box nil)) ($fc (make-p-box nil)) ($si_before (make-p-box nil)) ($iter_base (make-p-box nil)) ($r (make-p-box nil)))
                          ;; my $inner = substr($tmpl, $grpbeg, $grpend - $grpbeg)
                                                    (p-my-= $inner (p-substr $tmpl $grpbeg (p-- $grpend $grpbeg)))
                          
                          ;; my $gti = _pack_skip_ws($inner, 0)
                                                    (p-my-= $gti (let ((*wantarray* nil)) (pl-_pack_skip_ws $inner 0)))
                          
                          ;; if ($gti < length($inner)) {                 my $fc = substr($inner, $gti, 1);                 die "\(\)-group starts with a count in unpack\n" if $fc =~ /^[\d\*\[]/;             }
                          ;; if ($gti < length($inner))
                          (p-if (p-< $gti (p-length $inner))
                            (progn
                              (let (($fc (make-p-box nil)))
                                (let ((--pcl-if-ret--18 nil))
                                  ;; my $fc = substr($inner, $gti, 1)
                                                                    (p-my-= $fc (p-substr $inner $gti 1))
                                  
                                  ;; die "\(\)-group starts with a count in unpack\n" if $fc =~ /^[\d\*\[]/
                                  (p-if (setf --pcl-if-ret--18 (let ((*wantarray* nil)) (p-=~ $fc (p-regex "/^[\\d\\*\\[]/"))))
                                    (setf --pcl-if-ret--18 (p-die :loc "- line 990" "()-group starts with a count in unpack
"))
                                    nil)
                                  
                                --pcl-if-ret--18)
                              )
                            )
                            nil
                          )
                          
                          ;; if ($all) {                 while ($$si_ref < $slen) {                     my $si_before = $$si_ref;                     my $iter_base = $$si_ref;                     _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1);                     last if $$si_ref == $si_before;  # no progress: avoid infinite loop                 }             } else {                 for (my $r=0; $r<$nrep; $r++) {                     my $iter_base = $$si_ref;                     _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1);                 }             }
                          ;; if ($all)
                          (p-if $all
                            (progn
                              (let (($si_before (make-p-box nil)) ($iter_base (make-p-box nil)))
                                ;; while ($$si_ref < $slen) {                     my $si_before = $$si_ref;                     my $iter_base = $$si_ref;                     _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1);                     last if $$si_ref == $si_before;  # no progress: avoid infinite loop                 }
                                (p-while (p-< (p-cast-$ $si_ref) $slen)
                                  (let (($si_before (make-p-box nil)) ($iter_base (make-p-box nil)))
                                    ;; my $si_before = $$si_ref
                                                                        (p-my-= $si_before (p-cast-$ $si_ref))
                                    
                                    ;; my $iter_base = $$si_ref
                                                                        (p-my-= $iter_base (p-cast-$ $si_ref))
                                    
                                    ;; _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1)
                                                                        (pl-_unpack_tmpl $inner $s $si_ref $push_val $be $le $checksum_p $iter_base (p-+ $depth 1))
                                    
                                    ;; last if $$si_ref == $si_before
                                    (p-if (p-== (p-cast-$ $si_ref) $si_before) (p-last))
                                    
                                  )
                                )
                                
                              )
                            )
                            ;; else
                            (progn
                              (let (($r (make-p-box nil)) ($iter_base (make-p-box nil)))
                                ;; for (my $r=0; $r<$nrep; $r++) {                     my $iter_base = $$si_ref;                     _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1);                 }
                                (let (($r (make-p-box nil)))
                                  (p-for (                                (p-my-= $r 0))
                                          (                                (p-< $r $nrep))
                                          (                                (p-post++ $r))
                                    (let (($iter_base (make-p-box nil)))
                                      ;; my $iter_base = $$si_ref
                                                                            (p-my-= $iter_base (p-cast-$ $si_ref))
                                      
                                      ;; _unpack_tmpl($inner, $s, $si_ref, $push_val, $be, $le, $checksum_p, $iter_base, $depth + 1)
                                                                            (pl-_unpack_tmpl $inner $s $si_ref $push_val $be $le $checksum_p $iter_base (p-+ $depth 1))
                                      
                                    )
                                  )
                                )
                                
                              )
                            )
                          )
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if ($ch eq 'x') {             if ($bang) {                 # x!N: advance to N-byte alignment                 my $n = $nrep > 0 ? $nrep : 1;                 $$si_ref += ($n - ($$si_ref % $n)) % $n;             } elsif ($all) { $$si_ref = $slen }             else { $$si_ref += $nrep }             next;         }
                    ;; if ($ch eq 'x')
                    (p-if (p-str-eq $ch "x")
                      (progn
                        (let (($n (make-p-box nil)))
                          ;; if ($bang) {                 # x!N: advance to N-byte alignment                 my $n = $nrep > 0 ? $nrep : 1;                 $$si_ref += ($n - ($$si_ref % $n)) % $n;             } elsif ($all) { $$si_ref = $slen }             else { $$si_ref += $nrep }
                          ;; if ($bang)
                          (p-if $bang
                            (progn
                              (let (($n (make-p-box nil)))
                                ;; my $n = $nrep > 0 ? $nrep : 1
                                                                (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                
                                ;; $$si_ref += ($n - ($$si_ref % $n)) % $n
                                                                (p-incf (p-cast-$ $si_ref) (p-% (p-- $n (p-% (p-cast-$ $si_ref) $n)) $n))
                                
                              )
                            )
                            ;; elsif ($all)
                            (p-if $all
                              (progn
                                ;; $$si_ref = $slen
                                                                (p-setf (p-cast-$ $si_ref) $slen)
                                
                              )
                              ;; else
                              (progn
                                ;; $$si_ref += $nrep
                                                                (p-incf (p-cast-$ $si_ref) $nrep)
                                
                              )
                            )
                          )
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if ($ch eq 'X') {             if ($bang) {                 # X!N: back up to N-byte alignment                 my $n = $nrep > 0 ? $nrep : 1;                 $$si_ref = int($$si_ref / $n) * $n;             } else { $$si_ref -= $nrep; $$si_ref = 0 if $$si_ref < 0 }             next;         }
                    ;; if ($ch eq 'X')
                    (p-if (p-str-eq $ch "X")
                      (progn
                        (let (($n (make-p-box nil)))
                          ;; if ($bang) {                 # X!N: back up to N-byte alignment                 my $n = $nrep > 0 ? $nrep : 1;                 $$si_ref = int($$si_ref / $n) * $n;             } else { $$si_ref -= $nrep; $$si_ref = 0 if $$si_ref < 0 }
                          ;; if ($bang)
                          (p-if $bang
                            (progn
                              (let (($n (make-p-box nil)))
                                ;; my $n = $nrep > 0 ? $nrep : 1
                                                                (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                
                                ;; $$si_ref = int($$si_ref / $n) * $n
                                                                (p-setf (p-cast-$ $si_ref) (p-* (p-int (p-/ (p-cast-$ $si_ref) $n)) $n))
                                
                              )
                            )
                            ;; else
                            (progn
                              (let ((--pcl-if-ret--19 nil))
                                ;; $$si_ref -= $nrep
                                (let ((*wantarray* :void))                                 (p-decf (p-cast-$ $si_ref) $nrep))
                                
                                ;; $$si_ref = 0 if $$si_ref < 0
                                (p-if (setf --pcl-if-ret--19 (p-< (p-cast-$ $si_ref) 0))
                                  (setf --pcl-if-ret--19 (p-setf (p-cast-$ $si_ref) 0))
                                  nil)
                                
                              --pcl-if-ret--19)
                            )
                          )
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if ($ch eq '@') {             my $n = defined($count) ? $count : 0;             $$si_ref = $bang ? $n : $group_base + $n;             next;         }
                    ;; if ($ch eq '@')
                    (p-if (p-str-eq $ch "@")
                      (progn
                        (let (($n (make-p-box nil)))
                          ;; my $n = defined($count) ? $count : 0
                                                    (p-my-= $n (p-if (p-defined $count) $count 0))
                          
                          ;; $$si_ref = $bang ? $n : $group_base + $n
                          (let ((*wantarray* :void))                           (p-setf (p-cast-$ $si_ref) (p-if $bang $n (p-+ $group_base $n))))
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if ($ch eq '%' || $ch eq '!' ) { next }
                    ;; if ($ch eq '%' || $ch eq '!')
                    (p-if (p-|| (p-str-eq $ch "%") (p-str-eq $ch "!"))
                      (progn
                        ;; next
                                                (p-next)
                        
                      )
                      nil
                    )
                    
                    ;; if ($ch eq 'p'||$ch eq 'P'||$ch eq 'D') { die "Invalid type '$ch' in unpack\n" }
                    ;; if ($ch eq 'p'||$ch eq 'P'||$ch eq 'D')
                    (p-if (p-|| (p-|| (p-str-eq $ch "p") (p-str-eq $ch "P")) (p-str-eq $ch "D"))
                      (progn
                        ;; die "Invalid type '$ch' in unpack\n"
                                                (p-die :loc "- line 1033" (p-string-concat "Invalid type '" $ch "' in unpack
"))
                        
                      )
                      nil
                    )
                    
                    ;; if ($ch eq '.') {             # Position format: pushes current offset, does not advance.             # .* = absolute pos from string start.             # . / .1 = relative to innermost group (group_base).             # .0 = self offset = 0.             # .N (N>=2) = would need full group stack; approximate with absolute.             if ($all) {                 $push_val->($$si_ref);             } elsif (defined($count) && $count == 0) {                 $push_val->(0);             } elsif (defined($count) && $count >= 2) {                 $push_val->($$si_ref);  # approximate: absolute             } else {                 $push_val->($$si_ref - $group_base);  # relative to innermost group             }             next;         }
                    ;; if ($ch eq '.')
                    (p-if (p-str-eq $ch ".")
                      (progn
                        ;; if ($all) {                 $push_val->($$si_ref);             } elsif (defined($count) && $count == 0) {                 $push_val->(0);             } elsif (defined($count) && $count >= 2) {                 $push_val->($$si_ref);  # approximate: absolute             } else {                 $push_val->($$si_ref - $group_base);  # relative to innermost group             }
                        ;; if ($all)
                        (p-if $all
                          (progn
                            ;; $push_val->($$si_ref)
                                                        (p-funcall-ref $push_val (p-cast-$ $si_ref))
                            
                          )
                          ;; elsif (defined($count) && $count == 0)
                          (p-if (p-&& (p-defined $count) (p-== $count 0))
                            (progn
                              ;; $push_val->(0)
                                                            (p-funcall-ref $push_val 0)
                              
                            )
                            ;; elsif (defined($count) && $count >= 2)
                            (p-if (p-&& (p-defined $count) (p->= $count 2))
                              (progn
                                ;; $push_val->($$si_ref)
                                                                (p-funcall-ref $push_val (p-cast-$ $si_ref))
                                
                              )
                              ;; else
                              (progn
                                ;; $push_val->($$si_ref - $group_base)
                                                                (p-funcall-ref $push_val (p-- (p-cast-$ $si_ref) $group_base))
                                
                              )
                            )
                          )
                        )
                        
                        ;; next
                                                (p-next)
                        
                      )
                      nil
                    )
                    
                    ;; my ($nb, $sig, $dbe) = _pack_type_info($ch, $bang)
                                        (let ((*wantarray* nil)) (p-list-= (vector $nb $sig $dbe) (let ((*wantarray* t)) (pl-_pack_type_info $ch $bang))))
                    
                    ;; if ($nb) {             my $be2 = $be ? 1 : ($le ? 0 : $dbe);             my $n = $all ? int(($slen-$$si_ref)/$nb) : $nrep;             for (my $i=0; $i<$n; $i++) {                 last if $$si_ref + $nb > $slen;                 $push_val->(_unpack_read_int($s, $$si_ref, $nb, $be2, $sig));                 $$si_ref += $nb;             }             next;         }
                    ;; if ($nb)
                    (p-if $nb
                      (progn
                        (let (($be2 (make-p-box nil)) ($n (make-p-box nil)) ($i (make-p-box nil)))
                          ;; my $be2 = $be ? 1 : ($le ? 0 : $dbe)
                                                    (p-my-= $be2 (p-if $be 1 (p-if $le 0 $dbe)))
                          
                          ;; my $n = $all ? int(($slen-$$si_ref)/$nb) : $nrep
                                                    (p-my-= $n (p-if $all (p-int (p-/ (p-- $slen (p-cast-$ $si_ref)) $nb)) $nrep))
                          
                          ;; for (my $i=0; $i<$n; $i++) {                 last if $$si_ref + $nb > $slen;                 $push_val->(_unpack_read_int($s, $$si_ref, $nb, $be2, $sig));                 $$si_ref += $nb;             }
                          (let (($i (make-p-box nil)))
                            (p-for (                          (p-my-= $i 0))
                                    (                          (p-< $i $n))
                                    (                          (p-post++ $i))
                              ;; last if $$si_ref + $nb > $slen
                              (let ((*wantarray* :void)) (p-if (p-> (p-+ (p-cast-$ $si_ref) $nb) $slen) (p-last)))
                              
                              ;; $push_val->(_unpack_read_int($s, $$si_ref, $nb, $be2, $sig))
                              (let ((*wantarray* :void))                               (let ((*wantarray* :void)) (p-funcall-ref $push_val (let ((*wantarray* :void)) (pl-_unpack_read_int $s (p-cast-$ $si_ref) $nb $be2 $sig)))))
                              
                              ;; $$si_ref += $nb
                                                            (p-incf (p-cast-$ $si_ref) $nb)
                              
                            )
                          )
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if ($ch eq 'f') {             my $be2 = $be ? 1 : ($le ? 0 : 0);             my $n = $all ? int(($slen-$$si_ref)/4) : $nrep;             for (my $i=0; $i<$n; $i++) {                 last if $$si_ref + 4 > $slen;                 $push_val->(_unpack_float32($s, $$si_ref, $be2)); $$si_ref += 4;             }             next;         }
                    ;; if ($ch eq 'f')
                    (p-if (p-str-eq $ch "f")
                      (progn
                        (let (($be2 (make-p-box nil)) ($n (make-p-box nil)) ($i (make-p-box nil)))
                          ;; my $be2 = $be ? 1 : ($le ? 0 : 0)
                                                    (p-my-= $be2 (p-if $be 1 (p-if $le 0 0)))
                          
                          ;; my $n = $all ? int(($slen-$$si_ref)/4) : $nrep
                                                    (p-my-= $n (p-if $all (p-int (p-/ (p-- $slen (p-cast-$ $si_ref)) 4)) $nrep))
                          
                          ;; for (my $i=0; $i<$n; $i++) {                 last if $$si_ref + 4 > $slen;                 $push_val->(_unpack_float32($s, $$si_ref, $be2)); $$si_ref += 4;             }
                          (let (($i (make-p-box nil)))
                            (p-for (                          (p-my-= $i 0))
                                    (                          (p-< $i $n))
                                    (                          (p-post++ $i))
                              ;; last if $$si_ref + 4 > $slen
                              (let ((*wantarray* :void)) (p-if (p-> (p-+ (p-cast-$ $si_ref) 4) $slen) (p-last)))
                              
                              ;; $push_val->(_unpack_float32($s, $$si_ref, $be2))
                              (let ((*wantarray* :void))                               (let ((*wantarray* :void)) (p-funcall-ref $push_val (let ((*wantarray* :void)) (pl-_unpack_float32 $s (p-cast-$ $si_ref) $be2)))))
                              
                              ;; $$si_ref += 4
                                                            (p-incf (p-cast-$ $si_ref) 4)
                              
                            )
                          )
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if ($ch eq 'd'||$ch eq 'F') {             my $be2 = $be ? 1 : ($le ? 0 : 0);             my $n = $all ? int(($slen-$$si_ref)/8) : $nrep;             for (my $i=0; $i<$n; $i++) {                 last if $$si_ref + 8 > $slen;                 $push_val->(_unpack_float64($s, $$si_ref, $be2)); $$si_ref += 8;             }             next;         }
                    ;; if ($ch eq 'd'||$ch eq 'F')
                    (p-if (p-|| (p-str-eq $ch "d") (p-str-eq $ch "F"))
                      (progn
                        (let (($be2 (make-p-box nil)) ($n (make-p-box nil)) ($i (make-p-box nil)))
                          ;; my $be2 = $be ? 1 : ($le ? 0 : 0)
                                                    (p-my-= $be2 (p-if $be 1 (p-if $le 0 0)))
                          
                          ;; my $n = $all ? int(($slen-$$si_ref)/8) : $nrep
                                                    (p-my-= $n (p-if $all (p-int (p-/ (p-- $slen (p-cast-$ $si_ref)) 8)) $nrep))
                          
                          ;; for (my $i=0; $i<$n; $i++) {                 last if $$si_ref + 8 > $slen;                 $push_val->(_unpack_float64($s, $$si_ref, $be2)); $$si_ref += 8;             }
                          (let (($i (make-p-box nil)))
                            (p-for (                          (p-my-= $i 0))
                                    (                          (p-< $i $n))
                                    (                          (p-post++ $i))
                              ;; last if $$si_ref + 8 > $slen
                              (let ((*wantarray* :void)) (p-if (p-> (p-+ (p-cast-$ $si_ref) 8) $slen) (p-last)))
                              
                              ;; $push_val->(_unpack_float64($s, $$si_ref, $be2))
                              (let ((*wantarray* :void))                               (let ((*wantarray* :void)) (p-funcall-ref $push_val (let ((*wantarray* :void)) (pl-_unpack_float64 $s (p-cast-$ $si_ref) $be2)))))
                              
                              ;; $$si_ref += 8
                                                            (p-incf (p-cast-$ $si_ref) 8)
                              
                            )
                          )
                          
                          ;; next
                                                    (p-next)
                          
                        )
                      )
                      nil
                    )
                    
                    ;; if ($ch eq 'A'||$ch eq 'a'||$ch eq 'Z'||$ch eq 'H'||$ch eq 'h'||             $ch eq 'B'||$ch eq 'b'||$ch eq 'u'||$ch eq 'U'||$ch eq 'W'||$ch eq 'w') {             _unpack_str($ch, $nrep, $all, $s, $si_ref, $push_val, $checksum_p);             next;         }
                    ;; if ($ch eq 'A'||$ch eq 'a'||$ch eq 'Z'||$ch eq 'H'||$ch eq 'h'||
;;             $ch eq 'B'||$ch eq 'b'||$ch eq 'u'||$ch eq 'U'||$ch eq 'W'||$ch eq 'w')
                    (p-if (p-|| (p-|| (p-|| (p-|| (p-|| (p-|| (p-|| (p-|| (p-|| (p-|| (p-str-eq $ch "A") (p-str-eq $ch "a")) (p-str-eq $ch "Z")) (p-str-eq $ch "H")) (p-str-eq $ch "h")) (p-str-eq $ch "B")) (p-str-eq $ch "b")) (p-str-eq $ch "u")) (p-str-eq $ch "U")) (p-str-eq $ch "W")) (p-str-eq $ch "w"))
                      (progn
                        ;; _unpack_str($ch, $nrep, $all, $s, $si_ref, $push_val, $checksum_p)
                        (let ((*wantarray* :void))                         (let ((*wantarray* :void)) (pl-_unpack_str $ch $nrep $all $s $si_ref $push_val $checksum_p)))
                        
                        ;; next
                                                (p-next)
                        
                      )
                      nil
                    )
                    
                    ;; die "'/' must follow a numeric type in unpack\n" if $ch eq '/'
                    (let ((*wantarray* :void)) (p-if (p-str-eq $ch "/") (p-die :loc "- line 1092" "'/' must follow a numeric type in unpack
")))
                    
                    ;; die "Invalid type '$ch' in unpack\n"
                                        (p-die :loc "- line 1093" (p-string-concat "Invalid type '" $ch "' in unpack
"))
                    
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

;; sub _next_format_item { ... }
(p-sub pl-_next_format_item (&rest %_args)
  (p-args-body
    (block nil
      (let (($grpend (make-p-box nil)))
        (let (($tmpl (make-p-box nil)))
          ;; my ($tmpl) = @_
                    (let ((*wantarray* nil)) (p-list-= (vector $tmpl) @_))
          
          (let (($tlen (make-p-box nil)))
            ;; my $tlen = length($tmpl)
                        (p-my-= $tlen (p-length $tmpl))
            
            (let (($ti (make-p-box nil)))
              ;; my $ti = 0
                            (p-my-= $ti 0)
              
              ;; $ti = _pack_skip_ws($tmpl, $ti)
              (let ((*wantarray* :void))               (p-my-= $ti (let ((*wantarray* nil)) (pl-_pack_skip_ws $tmpl $ti))))
              
              ;; return ('', '') if $ti >= $tlen
              (let ((*wantarray* :void)) (p-if (p->= $ti $tlen) (p-return "" "")))
              
              (let (($ch (make-p-box nil)))
                ;; my $ch = substr($tmpl, $ti, 1)
                                (p-my-= $ch (p-substr $tmpl $ti 1))
                
                ;; $ti++
                (let ((*wantarray* :void))                 (p-post++ $ti))
                
                ;; if ($ch eq '(') {         my $grpend = _pack_find_group_end($tmpl, $ti);         $ti = $grpend + 1;     }
                ;; if ($ch eq '(')
                (p-if (p-str-eq $ch "(")
                  (progn
                    (let (($grpend (make-p-box nil)))
                      ;; my $grpend = _pack_find_group_end($tmpl, $ti)
                                            (p-my-= $grpend (let ((*wantarray* nil)) (pl-_pack_find_group_end $tmpl $ti)))
                      
                      ;; $ti = $grpend + 1
                                            (p-my-= $ti (p-+ $grpend 1))
                      
                    )
                  )
                  nil
                )
                
                ;; while ($ti < $tlen && substr($tmpl, $ti, 1) =~ /[!<>]/) { $ti++ }
                (p-while (p-&& (p-< $ti $tlen) (let ((*wantarray* nil)) (p-=~ (p-substr $tmpl $ti 1) (p-regex "/[!<>]/"))))
                  ;; $ti++
                                    (p-post++ $ti)
                  
                )
                
                ;; _pack_parse_count($tmpl, \$ti)
                (let ((*wantarray* :void))                 (let ((*wantarray* :void)) (pl-_pack_parse_count $tmpl (p-backslash $ti))))
                
                ;; return (substr($tmpl, 0, $ti), substr($tmpl, $ti))
                                (p-return (p-substr $tmpl 0 $ti) (p-substr $tmpl $ti))
                
              )
            )
          )
        )
      )
    )
  )
)

;; sub p_unpack { ... }
(p-sub pl-p_unpack (&rest %_args)
  (p-args-body
    (block nil
      (let (($bytes (make-p-box nil)) ($c (make-p-box nil)) ($code (make-p-box nil)) ($cs_tmpl (make-p-box nil)) ($rest_tmpl (make-p-box nil)) ($checksum (make-p-box nil)) ($mod (make-p-box nil)) ($q (make-p-box nil)))
        (let (($tmpl (make-p-box nil)) ($s (make-p-box nil)))
          ;; my ($tmpl, $s) = @_
                    (let ((*wantarray* nil)) (p-list-= (vector $tmpl $s) @_))
          
          ;; $s = '' unless defined $s
          (let ((*wantarray* :void)) (p-unless (p-defined $s) (p-my-= $s "")))
          
          ;; $tmpl =~ s/\A(?:[ \t\n\r\f,]|#[^\n]*\n?)*//
          (let ((*wantarray* :void))           (p-=~ $tmpl (p-subst "\\A(?:[ \\t\\n\\r\\f,]|#[^\\n]*\\n?)*" "")))
          
          (let (($checksum_width (make-p-box nil)))
            ;; my $checksum_width = 0
                        (p-my-= $checksum_width 0)
            
            ;; if ($tmpl =~ s/^%(\d*)//) {         $checksum_width = length($1) ? int($1) : 16;     }
            ;; if ($tmpl =~ s/^%(\d*)//)
            (p-if (p-=~ $tmpl (p-subst "^%(\\d*)" ""))
              (progn
                ;; $checksum_width = length($1) ? int($1) : 16
                                (p-my-= $checksum_width (p-if (p-length $1) (p-int $1) 16))
                
              )
              nil
            )
            
            (let (($utf8_mode (make-p-box nil)))
              ;; my $utf8_mode = ($tmpl =~ s/^U0//)
                            (p-my-= $utf8_mode (p-=~ $tmpl (p-subst "^U0" "")))
              
              ;; $tmpl =~ s/\A(?:[ \t\n\r\f,]|#[^\n]*\n?)*// if $checksum_width
              (let ((*wantarray* :void)) (p-if $checksum_width (p-=~ $tmpl (p-subst "\\A(?:[ \\t\\n\\r\\f,]|#[^\\n]*\\n?)*" ""))))
              
              ;; _pack_check_brackets($tmpl)
              (let ((*wantarray* :void))               (let ((*wantarray* :void)) (pl-_pack_check_brackets $tmpl)))
              
              ;; if ($utf8_mode) {         my $bytes = '';         for my $c (split //, $s) {             my $code = ord($c);             if    ($code < 0x80)    { $bytes .= chr($code) }             elsif ($code < 0x800)   { $bytes .= chr(0xC0|($code>>6)) . chr(0x80|($code&0x3F)) }             elsif ($code < 0x10000) { $bytes .= chr(0xE0|($code>>12)).chr(0x80|(($code>>6)&0x3F)).chr(0x80|($code&0x3F)) }             else { $bytes .= chr(0xF0|($code>>18)).chr(0x80|(($code>>12)&0x3F)).chr(0x80|(($code>>6)&0x3F)).chr(0x80|($code&0x3F)) }         }         $s = $bytes;     }
              ;; if ($utf8_mode)
              (p-if $utf8_mode
                (progn
                  (let (($bytes (make-p-box nil)) ($c (make-p-box nil)) ($code (make-p-box nil)))
                    ;; my $bytes = ''
                                        (p-my-= $bytes "")
                    
                    ;; for my $c (split //, $s) {             my $code = ord($c);             if    ($code < 0x80)    { $bytes .= chr($code) }             elsif ($code < 0x800)   { $bytes .= chr(0xC0|($code>>6)) . chr(0x80|($code&0x3F)) }             elsif ($code < 0x10000) { $bytes .= chr(0xE0|($code>>12)).chr(0x80|(($code>>6)&0x3F)).chr(0x80|($code&0x3F)) }             else { $bytes .= chr(0xF0|($code>>18)).chr(0x80|(($code>>12)&0x3F)).chr(0x80|(($code>>6)&0x3F)).chr(0x80|($code&0x3F)) }         }
                    (p-foreach ($c                     (p-split (p-regex "//") $s))
                      (let (($code (make-p-box nil)))
                        ;; my $code = ord($c)
                                                (p-my-= $code (p-ord $c))
                        
                        ;; if    ($code < 0x80)    { $bytes .= chr($code) }             elsif ($code < 0x800)   { $bytes .= chr(0xC0|($code>>6)) . chr(0x80|($code&0x3F)) }             elsif ($code < 0x10000) { $bytes .= chr(0xE0|($code>>12)).chr(0x80|(($code>>6)&0x3F)).chr(0x80|($code&0x3F)) }             else { $bytes .= chr(0xF0|($code>>18)).chr(0x80|(($code>>12)&0x3F)).chr(0x80|(($code>>6)&0x3F)).chr(0x80|($code&0x3F)) }
                        ;; if ($code < 0x80)
                        (p-if (p-< $code #x80)
                          (progn
                            ;; $bytes .= chr($code)
                                                        (p-.= $bytes (p-chr $code))
                            
                          )
                          ;; elsif ($code < 0x800)
                          (p-if (p-< $code #x800)
                            (progn
                              ;; $bytes .= chr(0xC0|($code>>6)) . chr(0x80|($code&0x3F))
                                                            (p-.= $bytes (p-. (p-chr (p-bit-or #xC0 (p->> $code 6))) (p-chr (p-bit-or #x80 (p-bit-and $code #x3F)))))
                              
                            )
                            ;; elsif ($code < 0x10000)
                            (p-if (p-< $code #x10000)
                              (progn
                                ;; $bytes .= chr(0xE0|($code>>12)).chr(0x80|(($code>>6)&0x3F)).chr(0x80|($code&0x3F))
                                                                (p-.= $bytes (p-. (p-. (p-chr (p-bit-or #xE0 (p->> $code 12))) (p-chr (p-bit-or #x80 (p-bit-and (p->> $code 6) #x3F)))) (p-chr (p-bit-or #x80 (p-bit-and $code #x3F)))))
                                
                              )
                              ;; else
                              (progn
                                ;; $bytes .= chr(0xF0|($code>>18)).chr(0x80|(($code>>12)&0x3F)).chr(0x80|(($code>>6)&0x3F)).chr(0x80|($code&0x3F))
                                                                (p-.= $bytes (p-. (p-. (p-. (p-chr (p-bit-or #xF0 (p->> $code 18))) (p-chr (p-bit-or #x80 (p-bit-and (p->> $code 12) #x3F)))) (p-chr (p-bit-or #x80 (p-bit-and (p->> $code 6) #x3F)))) (p-chr (p-bit-or #x80 (p-bit-and $code #x3F)))))
                                
                              )
                            )
                          )
                        )
                        
                      )
                    )
                    
                    ;; $s = $bytes
                                        (p-my-= $s $bytes)
                    
                  )
                )
                nil
              )
              
              (let ((@result__lex__4 (make-array 0 :adjustable t :fill-pointer 0)))
                ;; my @result (bare declaration)
                
                (let (($si (make-p-box nil)))
                  ;; my $si = 0
                                    (p-my-= $si 0)
                  
                  ;; if ($checksum_width) {         # %N applies to the NEXT format item only, then normal unpacking continues.         my ($cs_tmpl, $rest_tmpl) = _next_format_item($tmpl);         my $checksum = 0;         if (length($cs_tmpl)) {             _unpack_tmpl($cs_tmpl, $s, \$si, sub { $checksum += $_[0] }, 0, 0, 1);         }         # Use floor-division modulo: works for negative integers and float checksums.         my $mod = 2 ** $checksum_width;         my $q = int($checksum / $mod);         $q-- if $q * $mod > $checksum;         push @result, $checksum - $q * $mod;         if (length($rest_tmpl)) {             _unpack_tmpl($rest_tmpl, $s, \$si, sub { push @result, $_[0] }, 0, 0, 0);         }     } else {         _unpack_tmpl($tmpl, $s, \$si, sub { push @result, $_[0] }, 0, 0, 0);     }
                  ;; if ($checksum_width)
                  (p-if $checksum_width
                    (progn
                      (let (($cs_tmpl (make-p-box nil)) ($rest_tmpl (make-p-box nil)) ($checksum__lex__5 (make-p-box nil)) ($mod (make-p-box nil)) ($q (make-p-box nil)))
                        (let ((--pcl-if-ret--20 nil))
                          ;; my ($cs_tmpl, $rest_tmpl) = _next_format_item($tmpl)
                                                    (let ((*wantarray* nil)) (p-list-= (vector $cs_tmpl $rest_tmpl) (let ((*wantarray* t)) (pl-_next_format_item $tmpl))))
                          
                          ;; my $checksum = 0
                          (p-my-= $checksum__lex__5                           0)
                          
                          ;; if (length($cs_tmpl)) {             _unpack_tmpl($cs_tmpl, $s, \$si, sub { $checksum += $_[0] }, 0, 0, 1);         }
                          ;; if (length($cs_tmpl))
                          (p-if (p-length $cs_tmpl)
                            (progn
                              ;; _unpack_tmpl($cs_tmpl, $s, \$si, sub { $checksum += $_[0] }, 0, 0, 1)
                              (let ((*wantarray* :void))                               (let ((*wantarray* :void)) (pl-_unpack_tmpl $cs_tmpl $s (p-backslash $si) (lambda (&rest %_args)
  (let ((@_ (p-flatten-args %_args))
        (*pcl-caller-wantarray* *wantarray*))
    (catch :p-return
      (block nil
        ;; $checksum += $_[0]
                (p-incf $checksum__lex__5 (p-aref @_ 0))
        
      )
    )
  )
) 0 0 1)))
                              
                            )
                            nil
                          )
                          
                          ;; my $mod = 2 ** $checksum_width
                                                    (p-my-= $mod (p-** 2 $checksum_width))
                          
                          ;; my $q = int($checksum / $mod)
                                                    (p-my-= $q (p-int (p-/ $checksum__lex__5 $mod)))
                          
                          ;; $q-- if $q * $mod > $checksum
                          (let ((*wantarray* :void)) (p-if (p-> (p-* $q $mod) $checksum__lex__5) (p-post-- $q)))
                          
                          ;; push @result, $checksum - $q * $mod
                          (let ((*wantarray* :void))                           (p-push @result__lex__4 (p-- $checksum__lex__5 (p-* $q $mod))))
                          
                          ;; if (length($rest_tmpl)) {             _unpack_tmpl($rest_tmpl, $s, \$si, sub { push @result, $_[0] }, 0, 0, 0);         }
                          ;; if (length($rest_tmpl))
                          (p-if (setf --pcl-if-ret--20 (p-length $rest_tmpl))
                            (progn
                              ;; _unpack_tmpl($rest_tmpl, $s, \$si, sub { push @result, $_[0] }, 0, 0, 0)
                              (setf --pcl-if-ret--20 (let ((*wantarray* nil)) (pl-_unpack_tmpl $rest_tmpl $s (p-backslash $si) (lambda (&rest %_args)
  (let ((@_ (p-flatten-args %_args))
        (*pcl-caller-wantarray* *wantarray*))
    (catch :p-return
      (block nil
        ;; push @result, $_[0]
                (p-push @result__lex__4 (p-aref @_ 0))
        
      )
    )
  )
) 0 0 0)))
                              
                            )
                            nil
                          )
                          
                        --pcl-if-ret--20)
                      )
                    )
                    ;; else
                    (progn
                      ;; _unpack_tmpl($tmpl, $s, \$si, sub { push @result, $_[0] }, 0, 0, 0)
                      (let ((*wantarray* :void))                       (let ((*wantarray* :void)) (pl-_unpack_tmpl $tmpl $s (p-backslash $si) (lambda (&rest %_args)
  (let ((@_ (p-flatten-args %_args))
        (*pcl-caller-wantarray* *wantarray*))
    (catch :p-return
      (block nil
        ;; push @result, $_[0]
                (p-push @result__lex__4 (p-aref @_ 0))
        
      )
    )
  )
) 0 0 0)))
                      
                    )
                  )
                  
                  ;; return wantarray ? @result : $result[0]
                                    (p-return (p-if (p-wantarray) @result__lex__4 (p-aref @result__lex__4 0)))
                  
                )
              )
            )
          )
        )
      )
    )
  )
)

(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))

;; ## Copyright (c) 2025-2026

;; ## This is free software; you can redistribute it and/or modify it

;; ## under the same terms as the Perl 5 programming language system itself.

;; use strict (pragma)

;; use warnings (pragma)

;; # REBUILD PROCEDURE: after running ./pl2cl < cl/pack-impl.pl to regenerate cl/pcl-pack.lisp:

;; #   1. Keep the manually-maintained header in cl/pcl-pack.lisp (lines up to and including

;; #      the @INC setup). The header begins with (in-package :pcl) and must stay in :pcl.

;; #   2. Remove the generated (p-defpackage :main) and (in-package :main) lines â pack-impl.pl

;; #      has no `package` declaration, so PCL defaults to :main. Those two lines must be deleted

;; #      so all functions stay in the :pcl package (matching pcl-pack.lisp.bak behavior).

;; #      Switching to :main causes "MAIN also shadows" SBCL warnings in subsequent test code.

;; #   3. Ensure the p-pack / p-unpack wrapper defuns are appended at the end (they call

;; #      pl-p_pack / pl-p_unpack, the transpiled names of p_pack / p_unpack below).

(box-set $CAN_ENDIAN "sSiIlLqQjJfFdDpP")

(box-set $CAN_SHRIEK "sSiIlLnNvVxX.@")

(box-set $MAX_GROUP_DEPTH 100)

;; # Returns (nbytes, signed_flag, big_endian_default) for integer types; () otherwise.

(setf (p-box-value $pcl_pack_comma_warned) 0)

;; # reset at start of each p_pack call

;; # Returns index of matching ')'. Starts just after the opening '('.

;; # Parse !, >, < modifiers. Updates $$ti_ref. Dies on invalid modifier.

;; # Compute the byte size that template $tmpl would produce/consume.

;; # Used for [TEMPLATE] count notation (e.g. x[A3 N] means skip sizeof(A3 N) bytes).

;; # Parse count: *, [N], [TEMPLATE], digits. Returns ($all, $count, $nrep).

;; # Pack integer value as nbytes in specified byte order.

;; # Read nbytes from $s at $si as integer.

;; # Float stubs â bodies replaced with SBCL sb-kernel calls after ./pl2cl.

;; # Pack string/bit/uuencode types. Each consumes ONE arg from $args_ref.

;; # Encode one UTF-8 codepoint, append to $$r.

;; # Main pack loop. Reads from $args_ref via $$ai_ref, appends to $$result_ref.

;; # $out_base: position in $$result_ref where the current group started (for @ relative offsets).

;; # $depth: nesting depth for ()-groups (dies if > $MAX_GROUP_DEPTH).

;; # This pack()/unpack() implementation is modelled directly on Perl's own C

;; # version in the interpreter source (pp_pack.c â pp_pack/pp_unpack and their

;; # helpers), reimplemented here in Perl: the template grammar, type table, count/

;; # star handling, endianness ('<'/'>'), the '!' shriek modifier, group '()' and

;; # '/' length-prefix rules, and the checksum logic all mirror pp_pack.c so the

;; # behaviour matches stock Perl rather than a from-scratch interpretation.

;; # Decode one UTF-8 sequence from $s at $$si_ref. Returns codepoint, advances si.

;; # Unpack string/bit/uu types. $checksum_p: for B/b push individual bits.

;; # Core unpack loop. Reads from $s using $$si_ref, pushes items via $push_val.

;; # $group_base: the si value at the start of the current group iteration (for @ relative offsets).

;; # $depth: nesting depth for ()-groups.

;; # Returns (first_item_tmpl, rest_tmpl): the first complete format item and everything after it.

;; # Used by p_unpack to apply %N to only the next item, not the whole template.

;; 1
1


(defun pl-_pack_float32 (&rest %_args)
  (let* ((@_ (pcl::p-flatten-args %_args))
         (val (pcl::to-number (aref @_ 0)))
         (be  (aref @_ 1))
         (sf  (handler-case
                  (coerce (if (integerp val) (float val 1.0s0) val) 'single-float)
                (floating-point-overflow ()
                  (if (and (realp val) (minusp val))
                      sb-ext:single-float-negative-infinity
                      sb-ext:single-float-positive-infinity))))
         (bits (logand (sb-kernel:single-float-bits sf) #xFFFFFFFF))
         (result (make-string 4 :initial-element #\Nul)))
    (if (pcl::p-true-p be)
        (loop for k from 0 below 4 do
              (setf (char result k)
                    (code-char (logand (ash bits (- (* (- 3 k) 8))) #xFF))))
        (loop for k from 0 below 4 do
              (setf (char result k)
                    (code-char (logand (ash bits (- (* k 8))) #xFF)))))
    result))

(defun pl-_pack_float64 (&rest %_args)
  (let* ((@_ (pcl::p-flatten-args %_args))
         (val (pcl::to-number (aref @_ 0)))
         (be  (aref @_ 1))
         (df  (handler-case
                  (coerce (if (integerp val) (float val 1.0d0) val) 'double-float)
                (floating-point-overflow ()
                  (if (and (realp val) (minusp val))
                      sb-ext:double-float-negative-infinity
                      sb-ext:double-float-positive-infinity))))
         (hi  (logand (sb-kernel:double-float-high-bits df) #xFFFFFFFF))
         (lo  (logand (sb-kernel:double-float-low-bits df)  #xFFFFFFFF))
         (bits (logior (ash hi 32) lo))
         (result (make-string 8 :initial-element #\Nul)))
    (if (pcl::p-true-p be)
        (loop for k from 0 below 8 do
              (setf (char result k)
                    (code-char (logand (ash bits (- (* (- 7 k) 8))) #xFF))))
        (loop for k from 0 below 8 do
              (setf (char result k)
                    (code-char (logand (ash bits (- (* k 8))) #xFF)))))
    result))

(defun pl-_unpack_float32 (&rest %_args)
  (let* ((@_ (pcl::p-flatten-args %_args))
         (s    (pcl::to-string (aref @_ 0)))
         (si   (pcl::to-number (aref @_ 1)))
         (be   (aref @_ 2))
         (slen (length s))
         (bits 0))
    (if (pcl::p-true-p be)
        (loop for k from 0 below 4 do
              (setf bits (logior (ash bits 8)
                                 (if (< (+ si k) slen)
                                     (char-code (char s (+ si k))) 0))))
        (loop for k from 3 downto 0 do
              (setf bits (logior (ash bits 8)
                                 (if (< (+ si k) slen)
                                     (char-code (char s (+ si k))) 0)))))
    (coerce (sb-kernel:make-single-float
             (if (logbitp 31 bits) (- bits #x100000000) bits))
            'double-float)))

(defun pl-_unpack_float64 (&rest %_args)
  (let* ((@_ (pcl::p-flatten-args %_args))
         (s    (pcl::to-string (aref @_ 0)))
         (si   (pcl::to-number (aref @_ 1)))
         (be   (aref @_ 2))
         (slen (length s))
         (bits 0))
    (if (pcl::p-true-p be)
        (loop for k from 0 below 8 do
              (setf bits (logior (ash bits 8)
                                 (if (< (+ si k) slen)
                                     (char-code (char s (+ si k))) 0))))
        (loop for k from 7 downto 0 do
              (setf bits (logior (ash bits 8)
                                 (if (< (+ si k) slen)
                                     (char-code (char s (+ si k))) 0)))))
    (let* ((hi (logand (ash bits -32) #xFFFFFFFF))
           (lo (logand bits #xFFFFFFFF))
           (hi-signed (if (logbitp 31 hi) (- hi #x100000000) hi)))
      (sb-kernel:make-double-float hi-signed lo))))

(defun p-pack (template &rest args)
  (apply #'pl-p_pack template args))

(defun p-unpack (template &optional (str $_))
  (pl-p_unpack template str))

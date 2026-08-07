;;; pcl: pipeline=v2 gen=v2-111
(in-package :pcl)
(setf pcl::*pcl-pl2cl-path* #P"/home/bernt/pcl/pl2cl")
;; Initialize @INC from Perl
(setf pcl::@INC (make-array 0 :adjustable t :fill-pointer 0))
(vector-push-extend "/home/bernt/pcl/cl" pcl::@INC)
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

(p-declare-sub pl-_pack_type_info)
(p-declare-sub pl-_pack_skip_ws)
(p-declare-sub pl-_pack_find_group_end)
(p-declare-sub pl-_pack_parse_mods)
(p-declare-sub pl-_pack_template_size)
(p-declare-sub pl-_pack_parse_count)
(p-declare-sub pl-_pack_emit_int)
(p-declare-sub pl-_unpack_read_int)
(p-declare-sub pl-_pack_float32)
(p-declare-sub pl-_pack_float64)
(p-declare-sub pl-_unpack_float32)
(p-declare-sub pl-_unpack_float64)
(p-declare-sub pl-_pack_str_one)
(p-declare-sub pl-_pack_utf8_char)
(p-declare-sub pl-_pack_tmpl)
(p-declare-sub pl-_pack_check_brackets)
(p-declare-sub pl-p_pack)
(p-declare-sub pl-_unpack_utf8_char)
(p-declare-sub pl-_unpack_str)
(p-declare-sub pl-_unpack_tmpl)
(p-declare-sub pl-_next_format_item)
(p-declare-sub pl-p_unpack)
(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))

;; Forward declarations for undeclared package globals
(defvar $pcl_pack_comma_warned (make-p-box nil))

(defvar $CAN_ENDIAN (make-p-box nil))
(defvar $CAN_SHRIEK (make-p-box nil))
(defvar $MAX_GROUP_DEPTH (make-p-box nil))
(defvar $pcl_pack_comma_warned (make-p-box nil))

(p-sub pl-_pack_type_info
  (&rest %_args)
  (p-raw-params ($ch $bang)
    (block nil
      (let ((*wantarray* :void))
        (p-if (p-str-eq $ch "c")
          (progn
            ;; return (1, 1, 0)
(p-return 1 1 0))
          (p-if (p-str-eq $ch "C")
            (progn
              ;; return (1, 0, 0)
(p-return 1 0 0))
            (p-if (p-str-eq $ch "s")
              (progn
                ;; return (2, 1, 0)
(p-return 2 1 0))
              (p-if (p-str-eq $ch "S")
                (progn
                  ;; return (2, 0, 0)
(p-return 2 0 0))
                (p-if (p-str-eq $ch "n")
                  (progn
                    ;; return (2, ($bang ? 1 : 0), 1)
(p-return 2 (p-if $bang 1 0) 1))
                  (p-if (p-str-eq $ch "v")
                    (progn
                      ;; return (2, ($bang ? 1 : 0), 0)
(p-return 2 (p-if $bang 1 0) 0))
                    (p-if (p-str-eq $ch "i")
                      (progn
                        ;; return (4, 1, 0)
(p-return 4 1 0))
                      (p-if (p-str-eq $ch "I")
                        (progn
                          ;; return (4, 0, 0)
(p-return 4 0 0))
                        (p-if (p-str-eq $ch "l")
                          (progn
                            ;; return (($bang ? 8 : 4), 1, 0)
(p-return (p-if $bang 8 4) 1 0))
                          (p-if (p-str-eq $ch "L")
                            (progn
                              ;; return (($bang ? 8 : 4), 0, 0)
(p-return (p-if $bang 8 4) 0 0))
                            (p-if (p-str-eq $ch "N")
                              (progn
                                ;; return (4, ($bang ? 1 : 0), 1)
(p-return 4 (p-if $bang 1 0) 1))
                              (p-if (p-str-eq $ch "V")
                                (progn
                                  ;; return (4, ($bang ? 1 : 0), 0)
(p-return 4 (p-if $bang 1 0) 0))
                                (p-if (p-str-eq $ch "q")
                                  (progn
                                    ;; return (8, 1, 0)
(p-return 8 1 0))
                                  (p-if (p-str-eq $ch "Q")
                                    (progn
                                      ;; return (8, 0, 0)
(p-return 8 0 0))
                                    (p-if (p-str-eq $ch "j")
                                      (progn
                                        ;; return (8, 1, 0)
(p-return 8 1 0))
                                      (p-if (p-str-eq $ch "J")
                                        (progn
                                          ;; return (8, 0, 0)
(p-return 8 0 0))))))))))))))))))
        (p-return (progn))))))

(p-sub pl-_pack_skip_ws
  (&rest %_args)
  (p-args-body
    (block nil
      (let (($s (make-p-box nil)) ($ti (make-p-box nil)))
        (let ((*wantarray* nil)) (p-list-= (vector $s $ti) @_))
        (let ((*wantarray* :void))
          (let (($tlen (%pcl-to-number-strict (p-length $s) "$tlen")))
            (p-while (p-< $ti $tlen)
              (let (($ch (%pcl-to-string-strict (p-substr $s $ti 1) "$ch")))
                (p-if
                  (p-||
                    (p-||
                      (p-|| (p-|| (p-str-eq $ch " ") (p-str-eq $ch "	"))
                        (p-str-eq $ch
                          "
"))
                      (p-str-eq $ch ""))
                    (p-str-eq $ch ""))
                  (progn (p-post++ $ti))
                  (p-if (p-str-eq $ch ",")
                    (progn
                      (p-if (p-! $pcl_pack_comma_warned)
                        (progn
                          (p-warn :loc
                            "cl/pack-impl.pl line 92"
                            "Invalid type ',' in pack
")
                          (p-scalar-= $pcl_pack_comma_warned 1)))
                      (p-post++ $ti))
                    (p-if (p-str-eq $ch "#")
                      (progn (p-post++ $ti)
                        (p-while
                          (p-&& (p-< $ti $tlen)
                            (p-str-ne (p-substr $s $ti 1)
                              "
"))
                          (p-post++ $ti))
                        (p-if (p-< $ti $tlen) (p-post++ $ti)))
                      (progn (p-last)))))))
            (p-return $ti)))))))

(p-sub pl-_pack_find_group_end
  (&rest %_args)
  (p-args-body
    (block nil
      (let (($s (make-p-box nil)) ($ti (make-p-box nil)))
        (let ((*wantarray* nil)) (p-list-= (vector $s $ti) @_))
        (let ((*wantarray* :void))
          (let (($tlen (%pcl-to-number-strict (p-length $s) "$tlen")))
            (let (($depth 1))
              (p-while (p-&& (p-< $ti $tlen) (p-> $depth 0))
                (let (($ch (%pcl-to-string-strict (p-substr $s $ti 1) "$ch")))
                  (p-if (p-str-eq $ch "#")
                    (progn (p-post++ $ti)
                      (p-while
                        (p-&& (p-< $ti $tlen)
                          (p-str-ne (p-substr $s $ti 1)
                            "
"))
                        (p-post++ $ti)))
                    (p-if (p-str-eq $ch "(")
                      (progn (p-incf-raw $depth) (p-post++ $ti))
                      (p-if (p-str-eq $ch ")")
                        (progn (p-decf-raw $depth) (p-if (p-> $depth 0) (p-post++ $ti)))
                        (progn (p-post++ $ti)))))))
              (p-return $ti))))))))

(p-sub pl-_pack_parse_mods
  (&rest %_args)
  (p-args-body
    (block nil
      (let
        (($tmpl (make-p-box nil))
          ($ti_ref (make-p-box nil))
          ($inh_be (make-p-box nil))
          ($inh_le (make-p-box nil))
          ($ch (make-p-box nil))
          ($ctx (make-p-box nil)))
        (let ((*wantarray* nil)) (p-list-= (vector $tmpl $ti_ref $inh_be $inh_le $ch $ctx) @_))
        (let ((*wantarray* :void))
          (let (($tlen (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
            (let (($bang (make-p-box nil)) ($be (make-p-box nil)) ($le (make-p-box nil)))
              (let ((*wantarray* nil))
                (p-list-= (vector $bang $be $le) (vector 0 $inh_be $inh_le)))
              (let
                (($got_be (make-p-box nil))
                  ($got_le (make-p-box nil))
                  ($got_bang (make-p-box nil)))
                (let ((*wantarray* nil))
                  (p-list-= (vector $got_be $got_le $got_bang) (vector 0 0 0)))
                (p-while (p-< (p-cast-$ $ti_ref) $tlen)
                  (let (($m (%pcl-to-string-strict (p-substr $tmpl (p-cast-$ $ti_ref) 1) "$m")))
                    (p-if (p-str-eq $m "!")
                      (progn
                        (p-if (p-! (p->= (p-index $CAN_SHRIEK $ch) 0))
                          (p-die :loc
                            "cl/pack-impl.pl line 138"
                            (p-string-concat "'!' allowed only after types "
                              $CAN_SHRIEK
                              " in "
                              $ctx
                              "
")))
                        (p-if $got_bang
                          (p-warn :loc
                            "cl/pack-impl.pl line 140"
                            (p-string-concat "Duplicate modifier '!' after '"
                              $ch
                              "' in "
                              $ctx
                              "
")))
                        (p-my-= $bang 1)
                        (p-my-= $got_bang 1)
                        (p-post++ (p-cast-$ $ti_ref)))
                      (p-if (p-str-eq $m ">")
                        (progn
                          (p-if
                            (p-! (p-|| (p->= (p-index $CAN_ENDIAN $ch) 0) (p-str-eq $ch "(")))
                            (p-die :loc
                              "cl/pack-impl.pl line 143"
                              (p-string-concat "'>' allowed only after types "
                                $CAN_ENDIAN
                                " in "
                                $ctx
                                "
")))
                          (p-if $got_le
                            (p-die :loc
                              "cl/pack-impl.pl line 145"
                              (p-string-concat "Can't use both '<' and '>' after type '"
                                $ch
                                "' in "
                                $ctx
                                "
")))
                          (p-if $inh_le
                            (p-die :loc
                              "cl/pack-impl.pl line 146"
                              (p-string-concat
                                "Can't use '>' in a group with different byte-order in "
                                $ctx
                                "
")))
                          (p-if $got_be
                            (p-warn :loc
                              "cl/pack-impl.pl line 147"
                              (p-string-concat "Duplicate modifier '>' after '"
                                $ch
                                "' in "
                                $ctx
                                "
")))
                          (p-my-= $be 1)
                          (p-my-= $le 0)
                          (p-my-= $got_be 1)
                          (p-post++ (p-cast-$ $ti_ref)))
                        (p-if (p-str-eq $m "<")
                          (progn
                            (p-if
                              (p-! (p-|| (p->= (p-index $CAN_ENDIAN $ch) 0) (p-str-eq $ch "(")))
                              (p-die :loc
                                "cl/pack-impl.pl line 150"
                                (p-string-concat "'<' allowed only after types "
                                  $CAN_ENDIAN
                                  " in "
                                  $ctx
                                  "
")))
                            (p-if $got_be
                              (p-die :loc
                                "cl/pack-impl.pl line 152"
                                (p-string-concat "Can't use both '<' and '>' after type '"
                                  $ch
                                  "' in "
                                  $ctx
                                  "
")))
                            (p-if $inh_be
                              (p-die :loc
                                "cl/pack-impl.pl line 153"
                                (p-string-concat
                                  "Can't use '<' in a group with different byte-order in "
                                  $ctx
                                  "
")))
                            (p-if $got_le
                              (p-warn :loc
                                "cl/pack-impl.pl line 154"
                                (p-string-concat "Duplicate modifier '<' after '"
                                  $ch
                                  "' in "
                                  $ctx
                                  "
")))
                            (p-my-= $le 1)
                            (p-my-= $be 0)
                            (p-my-= $got_le 1)
                            (p-post++ (p-cast-$ $ti_ref)))
                          (progn (p-last)))))))
                ;; return ($bang, $be, $le)
(p-return $bang $be $le)))))))))

(p-sub pl-_pack_template_size
  (&rest %_args)
  (p-args-body
    (block nil
      (let (($tmpl (make-p-box nil)))
        (let ((*wantarray* nil)) (p-list-= (vector $tmpl) @_))
        (let ((*wantarray* :void))
          (let (($pos (make-p-box nil)))
            (p-my-= $pos 0)
            (let (($ti (make-p-box nil)))
              (p-my-= $ti 0)
              (let (($tlen (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
                (p-while 1
                  (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                  (p-if (p->= $ti $tlen) (p-last))
                  (let (($ch (make-p-box nil)))
                    (p-my-= $ch (p-substr $tmpl $ti 1))
                    (p-post++ $ti)
                    (let (($grpbeg (make-p-box nil)) ($grpend (make-p-box nil)))
                      (let ((*wantarray* nil))
                        (p-list-= (vector $grpbeg $grpend)
                          (vector (let ((*wantarray* t)) (p-undef))
                            (let ((*wantarray* t)) (p-undef)))))
                      (p-if (p-str-eq $ch "(")
                        (progn (p-my-= $grpend (pl-_pack_find_group_end $tmpl $ti))
                          (p-my-= $grpbeg $ti)
                          (p-my-= $ti (p-+ $grpend 1))))
                      (let (($bang (make-p-box nil)))
                        (p-my-= $bang 0)
                        (p-while
                          (p-&& (p-< $ti $tlen)
                            (let ((*wantarray* nil))
                              (p-=~ (p-substr $tmpl $ti 1) (p-regex "/[!<>]/"))))
                          (p-if (p-str-eq (p-substr $tmpl $ti 1) "!") (p-my-= $bang 1))
                          (p-post++ $ti))
                        (let
                          (($all (make-p-box nil))
                            ($count (make-p-box nil))
                            ($nrep (make-p-box nil)))
                          (let ((*wantarray* nil))
                            (p-list-= (vector $all $count $nrep)
                              (let ((*wantarray* t))
                                (pl-_pack_parse_count $tmpl (p-backslash $ti)))))
                          (p-if (p-! (p-&& (p-defined $nrep) (p->= $nrep 1))) (p-my-= $nrep 1))
                          (p-if (p-defined $grpbeg)
                            (progn
                              (let (($inner (make-p-box nil)))
                                (p-my-= $inner (p-substr $tmpl $grpbeg (p-- $grpend $grpbeg)))
                                (p-incf $pos
                                  (p-* (let ((*wantarray* nil)) (pl-_pack_template_size $inner))
                                    $nrep))
                                (p-next))))
                          (p-if (p-str-eq $ch "@")
                            (progn
                              (p-my-= $pos
                                (p-if $bang
                                  (p-if (p-defined $count) $count 0)
                                  (p-+ 0 (p-if (p-defined $count) $count 0))))
                              (p-next)))
                          (p-if (p-str-eq $ch "x")
                            (progn
                              (p-if $bang
                                (progn
                                  (let (($n (make-p-box nil)))
                                    (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                    (p-incf $pos (p-% (p-- $n (p-% $pos $n)) $n))))
                                (progn (p-incf $pos $nrep)))
                              (p-next)))
                          (p-if (p-str-eq $ch "X")
                            (progn
                              (p-if $bang
                                (progn
                                  (let (($n (make-p-box nil)))
                                    (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                    (p-my-= $pos (p-* (p-int (p-/ $pos $n)) $n))))
                                (progn (p-decf $pos $nrep) (p-if (p-< $pos 0) (p-my-= $pos 0))))
                              (p-next)))
                          (let (($nb (make-p-box nil)))
                            (let ((*wantarray* nil))
                              (p-list-= (vector $nb)
                                (let ((*wantarray* t)) (pl-_pack_type_info $ch $bang))))
                            (p-if $nb (progn (p-incf $pos (p-* $nb $nrep)) (p-next)))
                            (p-if
                              (p-|| (p-|| (p-str-eq $ch "A") (p-str-eq $ch "a"))
                                (p-str-eq $ch "Z"))
                              (progn (p-incf $pos $nrep) (p-next)))
                            (p-if (p-|| (p-str-eq $ch "B") (p-str-eq $ch "b"))
                              (progn (p-incf $pos (p-int (p-/ (p-+ $nrep 7) 8))) (p-next)))
                            (p-if (p-|| (p-str-eq $ch "H") (p-str-eq $ch "h"))
                              (progn (p-incf $pos (p-int (p-/ (p-+ $nrep 1) 2))) (p-next)))
                            (p-if (p-|| (p-str-eq $ch "f") (p-str-eq $ch "F"))
                              (progn (p-incf $pos (p-* 4 $nrep)) (p-next)))
                            (p-if (p-|| (p-str-eq $ch "d") (p-str-eq $ch "D"))
                              (progn (p-incf $pos (p-* 8 $nrep)) (p-next)))
                            (p-if (p-|| (p-str-eq $ch "p") (p-str-eq $ch "P"))
                              (progn (p-incf $pos (p-* 8 $nrep)) (p-next)))
                            (p-if
                              (p-|| (p-|| (p-str-eq $ch "W") (p-str-eq $ch "U"))
                                (p-str-eq $ch "w"))
                              (progn (p-incf $pos $nrep) (p-next)))
                            (p-if (p-str-eq $ch ".") (progn (p-next)))))))))
                (p-return $pos)))))))))

(p-sub pl-_pack_parse_count
  (&rest %_args)
  (p-args-body
    (block nil
      (let (($tmpl (make-p-box nil)) ($ti_ref (make-p-box nil)))
        (let ((*wantarray* nil)) (p-list-= (vector $tmpl $ti_ref) @_))
        (let ((*wantarray* :void))
          (let (($tlen (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
            (p-if
              (p-&& (p-< (p-cast-$ $ti_ref) $tlen)
                (p-str-eq (p-substr $tmpl (p-cast-$ $ti_ref) 1) "*"))
              (progn (p-post++ (p-cast-$ $ti_ref))
                ;; return (1, undef, 1)
(p-return 1 (p-undef) 1)))
            (p-if
              (p-&& (p-< (p-cast-$ $ti_ref) $tlen)
                (p-str-eq (p-substr $tmpl (p-cast-$ $ti_ref) 1) "["))
              (progn (p-post++ (p-cast-$ $ti_ref))
                (let (($start (%pcl-to-number-strict (p-cast-$ $ti_ref) "$start")))
                  (let (($depth 1))
                    (p-while (p-&& (p-< (p-cast-$ $ti_ref) $tlen) (p-> $depth 0))
                      (let
                        (($c
                            (%pcl-to-string-strict (p-substr $tmpl (p-cast-$ $ti_ref) 1) "$c")))
                        (p-post++ (p-cast-$ $ti_ref))
                        (p-if (p-str-eq $c "[")
                          (progn (p-incf-raw $depth))
                          (p-if (p-str-eq $c "]") (progn (p-decf-raw $depth))))))
                    (p-if (p-> $depth 0)
                      (p-die :loc
                        "cl/pack-impl.pl line 241"
                        "No group ending character ']' found in template
"))
                    (let (($inner (make-p-box nil)))
                      (p-my-= $inner
                        (p-substr $tmpl $start (p-- (p-- (p-cast-$ $ti_ref) $start) 1)))
                      (p-if (let ((*wantarray* nil)) (p-=~ $inner (p-regex "/^\\d+$/")))
                        (progn
                          (let (($n (make-p-box nil)))
                            (p-my-= $n (p-+ $inner 0))
                            ;; return (0, $n, $n)
(p-return 0 $n $n))))
                      (p-if (p->= (p-index $inner "@") 0)
                        (p-die :loc
                          "cl/pack-impl.pl line 248"
                          "Within []-length '@' not allowed
"))
                      (p-if
                        (p-&& (let ((*wantarray* nil)) (p-=~ $inner (p-regex "/^\\d/")))
                          (let ((*wantarray* nil)) (p-!~ $inner (p-regex "/^\\d+$/"))))
                        (p-die :loc
                          "cl/pack-impl.pl line 249"
                          "Malformed integer in []
"))
                      (let (($n (make-p-box nil)))
                        (p-my-= $n (pl-_pack_template_size $inner))
                        ;; return (0, $n, $n)
(p-return 0 $n $n)))))))
            (p-if
              (p-&& (p-< (p-cast-$ $ti_ref) $tlen)
                (let ((*wantarray* nil))
                  (p-=~ (p-substr $tmpl (p-cast-$ $ti_ref) 1) (p-regex "/\\d/"))))
              (progn
                (let (($n (make-p-box nil)))
                  (p-my-= $n 0)
                  (p-while
                    (p-&& (p-< (p-cast-$ $ti_ref) $tlen)
                      (let ((*wantarray* nil))
                        (p-=~ (p-substr $tmpl (p-cast-$ $ti_ref) 1) (p-regex "/\\d/"))))
                    (p-my-= $n (p-+ (p-* $n 10) (p-substr $tmpl (p-cast-$ $ti_ref) 1)))
                    (p-post++ (p-cast-$ $ti_ref)))
                  ;; return (0, $n, $n)
(p-return 0 $n $n))))
            ;; return (0, 1, 1)
(p-return 0 1 1)))))))

(p-sub pl-_pack_emit_int
  (&rest %_args)
  (p-args-body
    (block nil
      (let
        (($val (make-p-box nil))
          ($nbytes (make-p-box nil))
          ($signed (make-p-box nil))
          ($be (make-p-box nil)))
        (let ((*wantarray* nil)) (p-list-= (vector $val $nbytes $signed $be) @_))
        (let ((*wantarray* :void))
          (p-my-= $val (p-int (p-+ $val 0)))
          (let (($result ""))
            (p-if $be
              (progn
                (let (($k (p-- $nbytes 1)))
                  (p-for ()
                    ((p->= $k 0))
                    ((p-decf-raw $k))
                    (p-.=-raw $result (p-chr (p-bit-and (p->> $val (p-* 8 $k)) #xFF))))))
              (progn
                (let (($k 0))
                  (p-for ()
                    ((p-< $k $nbytes))
                    ((p-incf-raw $k))
                    (p-.=-raw $result (p-chr (p-bit-and (p->> $val (p-* 8 $k)) #xFF)))))))
            (p-return $result)))))))

(p-sub pl-_unpack_read_int
  (&rest %_args)
  (p-raw-params ($s $si $nbytes $be $signed)
    (block nil
      (let ((*wantarray* :void))
        (let (($slen (%pcl-to-number-strict (p-length $s) "$slen")))
          (let (($v (make-p-box nil)))
            (p-my-= $v 0)
            (p-if $be
              (progn
                (let (($k 0))
                  (p-for ()
                    ((p-< $k $nbytes))
                    ((p-incf-raw $k))
                    (p-my-= $v
                      (p-bit-or (p-<< $v 8)
                        (p-if (p-< (p-+ $si $k) $slen) (p-ord (p-substr $s (p-+ $si $k) 1)) 0))))))
              (progn
                (let (($k (p-- $nbytes 1)))
                  (p-for ()
                    ((p->= $k 0))
                    ((p-decf-raw $k))
                    (p-my-= $v
                      (p-bit-or (p-<< $v 8)
                        (p-if (p-< (p-+ $si $k) $slen) (p-ord (p-substr $s (p-+ $si $k) 1)) 0)))))))
            (p-if $signed
              (progn
                (let (($max (p-** 2 (p-* $nbytes 8))))
                  (p-if (p->= $v (p-/ $max 2)) (p-decf $v $max)))))
            (p-return $v)))))))

(p-sub pl-_pack_float32 (&rest %_args) (p-raw-params ($val $be) (block nil (p-return ""))))

(p-sub pl-_pack_float64 (&rest %_args) (p-raw-params ($val $be) (block nil (p-return ""))))

(p-sub pl-_unpack_float32
  (&rest %_args)
  (p-raw-params ($s $si $be) (block nil (p-return 0.0))))

(p-sub pl-_unpack_float64
  (&rest %_args)
  (p-raw-params ($s $si $be) (block nil (p-return 0.0))))

(p-sub pl-_pack_str_one
  (&rest %_args)
  (p-args-body
    (block nil
      (let
        (($ch (make-p-box nil))
          ($arg (make-p-box nil))
          ($nrep (make-p-box nil))
          ($star (make-p-box nil))
          ($result_ref (make-p-box nil)))
        (let ((*wantarray* nil)) (p-list-= (vector $ch $arg $nrep $star $result_ref) @_))
        (let ((*wantarray* :void))
          (p-if (p-! (p-defined $arg)) (p-my-= $arg ""))
          (let (($slen (make-p-box nil)))
            (p-my-= $slen (p-length $arg))
            (let ((--pcl-if-ret--0 nil))
              (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "a"))
                (setf --pcl-if-ret--0
                  (progn
                    (let (($len (make-p-box nil)))
                      (p-my-= $len (p-if $star $slen $nrep))
                      (let (($k 0))
                        (p-for ()
                          ((p-< $k $len))
                          ((p-incf-raw $k))
                          (p-.= (p-cast-$ $result_ref)
                            (p-if (p-< $k $slen) (p-substr $arg $k 1) (p-chr 0))))))))
                (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "A"))
                  (setf --pcl-if-ret--0
                    (progn
                      (let (($len (make-p-box nil)))
                        (p-my-= $len (p-if $star $slen $nrep))
                        (let (($k 0))
                          (p-for ()
                            ((p-< $k $len))
                            ((p-incf-raw $k))
                            (p-.= (p-cast-$ $result_ref)
                              (p-if (p-< $k $slen) (p-substr $arg $k 1) " ")))))))
                  (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "Z"))
                    (setf --pcl-if-ret--0
                      (progn
                        (let (($len (make-p-box nil)))
                          (p-my-= $len (p-if $star (p-+ $slen 1) $nrep))
                          (let ((--pcl-if-ret--1 nil))
                            (p-if (setf --pcl-if-ret--1 (p-> $len 0))
                              (setf --pcl-if-ret--1
                                (progn
                                  (let (($body (p-- $len 1)))
                                    (let (($k 0))
                                      (p-for ()
                                        ((p-< $k $body))
                                        ((p-incf-raw $k))
                                        (p-.= (p-cast-$ $result_ref)
                                          (p-if (p-< $k $slen) (p-substr $arg $k 1) (p-chr 0)))))
                                    (let ((*wantarray* *pcl-caller-wantarray*))
                                      (p-.= (p-cast-$ $result_ref) (p-chr 0))))))
                              nil)
                            --pcl-if-ret--1))))
                    (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "b"))
                      (setf --pcl-if-ret--0
                        (progn
                          (let (($nbits (make-p-box nil)))
                            (p-my-= $nbits (p-if $star $slen $nrep))
                            (let (($bs 0))
                              (p-for ()
                                ((p-< $bs $nbits))
                                ((p-incf-raw $bs 8))
                                (let (($byte (make-p-box nil)))
                                  (p-my-= $byte 0)
                                  (let (($bit 0))
                                    (p-for ()
                                      ((p-&& (p-< $bit 8) (p-< (p-+ $bs $bit) $nbits)))
                                      ((p-incf-raw $bit))
                                      (let (($idx (make-p-box nil)))
                                        (p-my-= $idx (p-+ $bs $bit))
                                        (p-if
                                          (p-&& (p-< $idx $slen)
                                            (p-str-eq (p-substr $arg $idx 1) "1"))
                                          (p-bit-or= $byte (p-<< 1 $bit))))))
                                  (p-.= (p-cast-$ $result_ref) (p-chr $byte))))))))
                      (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "B"))
                        (setf --pcl-if-ret--0
                          (progn
                            (let (($nbits (make-p-box nil)))
                              (p-my-= $nbits (p-if $star $slen $nrep))
                              (let (($bs 0))
                                (p-for ()
                                  ((p-< $bs $nbits))
                                  ((p-incf-raw $bs 8))
                                  (let (($byte (make-p-box nil)))
                                    (p-my-= $byte 0)
                                    (let (($bit 0))
                                      (p-for ()
                                        ((p-&& (p-< $bit 8) (p-< (p-+ $bs $bit) $nbits)))
                                        ((p-incf-raw $bit))
                                        (let (($idx (make-p-box nil)))
                                          (p-my-= $idx (p-+ $bs $bit))
                                          (p-if
                                            (p-&& (p-< $idx $slen)
                                              (p-str-eq (p-substr $arg $idx 1) "1"))
                                            (p-bit-or= $byte (p-<< 1 (p-- 7 $bit)))))))
                                    (p-.= (p-cast-$ $result_ref) (p-chr $byte))))))))
                        (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "H"))
                          (setf --pcl-if-ret--0
                            (progn
                              (let (($nyb (make-p-box nil)))
                                (p-my-= $nyb (p-if $star $slen $nrep))
                                (let (($k 0))
                                  (p-for ()
                                    ((p-< $k $nyb))
                                    ((p-incf-raw $k 2))
                                    (let (($hi (make-p-box nil)))
                                      (p-my-= $hi
                                        (p-if (p-< $k $slen) (p-hex (p-substr $arg $k 1)) 0))
                                      (let (($lo (make-p-box nil)))
                                        (p-my-= $lo
                                          (p-if (p-< (p-+ $k 1) $slen)
                                            (p-hex (p-substr $arg (p-+ $k 1) 1))
                                            0))
                                        (p-.= (p-cast-$ $result_ref)
                                          (p-chr (p-bit-or (p-<< $hi 4) $lo))))))))))
                          (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "h"))
                            (setf --pcl-if-ret--0
                              (progn
                                (let (($nyb (make-p-box nil)))
                                  (p-my-= $nyb (p-if $star $slen $nrep))
                                  (let (($k 0))
                                    (p-for ()
                                      ((p-< $k $nyb))
                                      ((p-incf-raw $k 2))
                                      (let (($lo (make-p-box nil)))
                                        (p-my-= $lo
                                          (p-if (p-< $k $slen) (p-hex (p-substr $arg $k 1)) 0))
                                        (let (($hi (make-p-box nil)))
                                          (p-my-= $hi
                                            (p-if (p-< (p-+ $k 1) $slen)
                                              (p-hex (p-substr $arg (p-+ $k 1) 1))
                                              0))
                                          (p-.= (p-cast-$ $result_ref)
                                            (p-chr (p-bit-or (p-<< $hi 4) $lo))))))))))
                            (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "u"))
                              (setf --pcl-if-ret--0
                                (progn
                                  (let (($line_len (%pcl-to-number-strict 45 "$line_len")))
                                    (p-if (p-&& (p-! $star) (p-> $nrep 45))
                                      (progn
                                        (p-if (p-> $nrep 63)
                                          (progn
                                            (p-warn :loc
                                              "cl/pack-impl.pl line 371"
                                              "Field too wide in 'u' format in pack")
                                            (setf $line_len
                                              (%pcl-to-number-strict 63 "$line_len")))
                                          (progn
                                            (setf $line_len
                                              (%pcl-to-number-strict $nrep "$line_len"))))))
                                    (let (($cs 0))
                                      (p-for ()
                                        ((p-< $cs $slen))
                                        ((p-incf-raw $cs $line_len))
                                        (let
                                          (($ce
                                              (%pcl-to-number-strict
                                                (p-if (p-< (p-+ $cs $line_len) $slen)
                                                  (p-+ $cs $line_len)
                                                  $slen)
                                                "$ce")))
                                          (let
                                            (($chunk
                                                (%pcl-to-string-strict
                                                  (p-substr $arg $cs (p-- $ce $cs))
                                                  "$chunk")))
                                            (let
                                              (($clen
                                                  (%pcl-to-number-strict (p-length $chunk)
                                                    "$clen")))
                                              (p-.= (p-cast-$ $result_ref)
                                                (p-chr (p-+ 32 $clen)))
                                              (let (($k 0))
                                                (p-for ()
                                                  ((p-< $k $clen))
                                                  ((p-incf-raw $k 3))
                                                  (let
                                                    (($b0
                                                        (%pcl-to-number-strict
                                                          (p-ord (p-substr $chunk $k 1))
                                                          "$b0")))
                                                    (let
                                                      (($b1
                                                          (%pcl-to-number-strict
                                                            (p-if (p-< (p-+ $k 1) $clen)
                                                              (p-ord
                                                                (p-substr $chunk (p-+ $k 1) 1))
                                                              0)
                                                            "$b1")))
                                                      (let (($b2 (make-p-box nil)))
                                                        (p-my-= $b2
                                                          (p-if (p-< (p-+ $k 2) $clen)
                                                            (p-ord
                                                              (p-substr $chunk (p-+ $k 2) 1))
                                                            0))
                                                        (let (($cm (make-p-box nil)))
                                                          (p-my-= $cm
                                                            (p-bit-or
                                                              (p-bit-or (p-<< $b0 16)
                                                                (p-<< $b1 8))
                                                              $b2))
                                                          (let (($uu (make-p-box nil)))
                                                            (p-my-= $uu
                                                              (lambda (&rest %_args)
                                                                (let
                                                                  ((@_
                                                                      (p-flatten-args %_args))
                                                                    (*pcl-caller-wantarray*
                                                                      *wantarray*))
                                                                  (catch :p-return
                                                                    (block nil
                                                                      (let ((*wantarray* :void))
                                                                        (let
                                                                          (($c
                                                                              (make-p-box nil)))
                                                                          (p-my-= $c
                                                                            (p-+ 32
                                                                              (p-bit-and
                                                                                (p-aref @_ 0)
                                                                                63)))
                                                                          (let
                                                                            ((*wantarray*
                                                                                *pcl-caller-wantarray*))
                                                                            (p-if (p-== $c 32)
                                                                              96
                                                                              $c)))))))))
                                                            (p-.= (p-cast-$ $result_ref)
                                                              (p-.
                                                                (p-.
                                                                  (p-.
                                                                    (p-chr
                                                                      (let ((*wantarray* nil))
                                                                        (p-funcall-ref $uu
                                                                          (p-bit-and
                                                                            (p->> $cm 18)
                                                                            63))))
                                                                    (p-chr
                                                                      (let ((*wantarray* nil))
                                                                        (p-funcall-ref $uu
                                                                          (p-bit-and
                                                                            (p->> $cm 12)
                                                                            63)))))
                                                                  (p-chr
                                                                    (let ((*wantarray* nil))
                                                                      (p-funcall-ref $uu
                                                                        (p-bit-and (p->> $cm 6)
                                                                          63)))))
                                                                (p-chr
                                                                  (let ((*wantarray* nil))
                                                                    (p-funcall-ref $uu
                                                                      (p-bit-and $cm 63)))))))))))))
                                              (p-.= (p-cast-$ $result_ref)
                                                "
")))))))))
                              nil))))))))
              --pcl-if-ret--0)))))))

(p-sub pl-_pack_utf8_char
  (&rest %_args)
  (p-args-body
    (block nil
      (let (($code (make-p-box nil)) ($r (make-p-box nil)))
        (let ((*wantarray* nil)) (p-list-= (vector $code $r) @_))
        (let ((*wantarray* :void))
          (p-if (p-< $code #x80)
            (progn
              (let ((*wantarray* *pcl-caller-wantarray*)) (p-.= (p-cast-$ $r) (p-chr $code))))
            (p-if (p-< $code #x800)
              (progn
                (let ((*wantarray* *pcl-caller-wantarray*))
                  (p-.= (p-cast-$ $r)
                    (p-. (p-chr (p-bit-or #xC0 (p->> $code 6)))
                      (p-chr (p-bit-or #x80 (p-bit-and $code #x3F)))))))
              (p-if (p-< $code #x10000)
                (progn
                  (let ((*wantarray* *pcl-caller-wantarray*))
                    (p-.= (p-cast-$ $r)
                      (p-.
                        (p-. (p-chr (p-bit-or #xE0 (p->> $code 12)))
                          (p-chr (p-bit-or #x80 (p-bit-and (p->> $code 6) #x3F))))
                        (p-chr (p-bit-or #x80 (p-bit-and $code #x3F)))))))
                (progn
                  (let ((*wantarray* *pcl-caller-wantarray*))
                    (p-.= (p-cast-$ $r)
                      (p-.
                        (p-.
                          (p-. (p-chr (p-bit-or #xF0 (p->> $code 18)))
                            (p-chr (p-bit-or #x80 (p-bit-and (p->> $code 12) #x3F))))
                          (p-chr (p-bit-or #x80 (p-bit-and (p->> $code 6) #x3F))))
                        (p-chr (p-bit-or #x80 (p-bit-and $code #x3F)))))))))))))))

(p-sub pl-_pack_tmpl
  (&rest %_args)
  (p-args-body
    (block nil
      (let
        (($tmpl (make-p-box nil))
          ($ai_ref (make-p-box nil))
          ($args_ref (make-p-box nil))
          ($result_ref (make-p-box nil))
          ($inh_be (make-p-box nil))
          ($inh_le (make-p-box nil))
          ($out_base (make-p-box nil))
          ($depth (make-p-box nil)))
        (let ((*wantarray* nil))
          (p-list-=
            (vector $tmpl $ai_ref $args_ref $result_ref $inh_be $inh_le $out_base $depth)
            @_))
        (let ((*wantarray* :void))
          (p-if (p-! (p-defined $out_base)) (p-my-= $out_base 0))
          (p-if (p-! (p-defined $depth)) (p-my-= $depth 0))
          (p-if (p-> $depth $MAX_GROUP_DEPTH)
            (p-die :loc
              "cl/pack-impl.pl line 412"
              "Too deeply nested ()-groups in pack
"))
          (let (($nargs (%pcl-to-number-strict (p-scalar (p-cast-@ $args_ref)) "$nargs")))
            (let (($ti (make-p-box nil)))
              (p-my-= $ti 0)
              (let (($tlen (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
                (p-while 1
                  (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                  (p-if (p->= $ti $tlen) (p-last))
                  (let (($ch (make-p-box nil)))
                    (p-my-= $ch (p-substr $tmpl $ti 1))
                    (p-post++ $ti)
                    (let (($grpbeg (make-p-box nil)) ($grpend (make-p-box nil)))
                      (let ((*wantarray* nil))
                        (p-list-= (vector $grpbeg $grpend)
                          (vector (let ((*wantarray* t)) (p-undef))
                            (let ((*wantarray* t)) (p-undef)))))
                      (p-if (p-str-eq $ch "(")
                        (progn (p-my-= $grpend (pl-_pack_find_group_end $tmpl $ti))
                          (p-my-= $grpbeg $ti)
                          (p-my-= $ti (p-+ $grpend 1))
                          (p-my-= $ch "(")))
                      (let
                        (($bang (make-p-box nil))
                          ($be (make-p-box nil))
                          ($le (make-p-box nil)))
                        (let ((*wantarray* nil))
                          (p-list-= (vector $bang $be $le)
                            (let ((*wantarray* t))
                              (pl-_pack_parse_mods $tmpl
                                (p-backslash $ti)
                                $inh_be
                                $inh_le
                                $ch
                                "pack"))))
                        (let (($ti_before_count (%pcl-to-number-strict $ti "$ti_before_count")))
                          (let
                            (($star (make-p-box nil))
                              ($count (make-p-box nil))
                              ($nrep (make-p-box nil)))
                            (let ((*wantarray* nil))
                              (p-list-= (vector $star $count $nrep)
                                (let ((*wantarray* t))
                                  (pl-_pack_parse_count $tmpl (p-backslash $ti)))))
                            (let (($had_count (make-p-box nil)))
                              (p-my-= $had_count (p-|| $star (p-> $ti $ti_before_count)))
                              (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                              (p-if (p-&& (p-< $ti $tlen) (p-str-eq (p-substr $tmpl $ti 1) "/"))
                                (progn (p-post++ $ti)
                                  (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                  (p-if (p->= $ti $tlen) (p-last))
                                  (let ((*package* *package*))
                                    (block nil
                                      (tagbody :redo
                                        (let (($c (make-p-box nil)))
                                          (p-my-= $c (p-substr $tmpl $ti 1))
                                          (p-if
                                            (p-|| (p-|| (p-str-eq $c "*") (p-str-eq $c "["))
                                              (let ((*wantarray* nil))
                                                (p-=~ $c (p-regex "/\\d/"))))
                                            (p-die :loc
                                              "cl/pack-impl.pl line 439"
                                              "'/' does not take a repeat count in pack
")))
                                        :next)))
                                  (let (($dfmt (make-p-box nil)))
                                    (p-my-= $dfmt (p-substr $tmpl $ti 1))
                                    (p-post++ $ti)
                                    (let
                                      (($dbang (make-p-box nil))
                                        ($dbe2 (make-p-box nil))
                                        ($dle2 (make-p-box nil)))
                                      (let ((*wantarray* nil))
                                        (p-list-= (vector $dbang $dbe2 $dle2)
                                          (let ((*wantarray* t))
                                            (pl-_pack_parse_mods $tmpl
                                              (p-backslash $ti)
                                              $be
                                              $le
                                              $dfmt
                                              "pack"))))
                                      (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                      (let
                                        (($ti_before_dcount
                                            (%pcl-to-number-strict $ti "$ti_before_dcount")))
                                        (let
                                          (($dstar2 (make-p-box nil))
                                            ($dcnt2 (make-p-box nil))
                                            ($dnrep2 (make-p-box nil)))
                                          (let ((*wantarray* nil))
                                            (p-list-= (vector $dstar2 $dcnt2 $dnrep2)
                                              (let ((*wantarray* t))
                                                (pl-_pack_parse_count $tmpl (p-backslash $ti)))))
                                          (let
                                            (($had_dcount
                                                (%pcl-to-string-strict
                                                  (p-|| $dstar2 (p-> $ti $ti_before_dcount))
                                                  "$had_dcount")))
                                            (let
                                              (($dnb (make-p-box nil))
                                                ($dsig (make-p-box nil))
                                                ($ddbe (make-p-box nil)))
                                              (let ((*wantarray* nil))
                                                (p-list-= (vector $dnb $dsig $ddbe)
                                                  (let ((*wantarray* t))
                                                    (pl-_pack_type_info $dfmt $dbang))))
                                              (let (($actual_count (make-p-box nil)))
                                                (p-if
                                                  (p-||
                                                    (p-|| (p-str-eq $dfmt "a")
                                                      (p-str-eq $dfmt "A"))
                                                    (p-str-eq $dfmt "Z"))
                                                  (progn
                                                    (let (($darg (make-p-box nil)))
                                                      (p-my-= $darg
                                                        (p-if (p-< (p-cast-$ $ai_ref) $nargs)
                                                          (p-aref-deref $args_ref
                                                            (p-post++ (p-cast-$ $ai_ref)))
                                                          ""))
                                                      (p-if (p-! (p-defined $darg))
                                                        (p-my-= $darg ""))
                                                      (let (($dlen (make-p-box nil)))
                                                        (p-my-= $dlen (p-length $darg))
                                                        (p-if (p-|| (p-! $had_dcount) $dstar2)
                                                          (progn
                                                            (p-my-= $actual_count
                                                              (p-if (p-str-eq $dfmt "Z")
                                                                (p-+ $dlen 1)
                                                                $dlen)))
                                                          (progn (p-my-= $actual_count $dnrep2)))
                                                        (let
                                                          (($nb (make-p-box nil))
                                                            ($sig (make-p-box nil))
                                                            ($dbe (make-p-box nil)))
                                                          (let ((*wantarray* nil))
                                                            (p-list-= (vector $nb $sig $dbe)
                                                              (let ((*wantarray* t))
                                                                (pl-_pack_type_info $ch $bang))))
                                                          (p-if $nb
                                                            (progn
                                                              (p-.= (p-cast-$ $result_ref)
                                                                (let ((*wantarray* nil))
                                                                  (pl-_pack_emit_int
                                                                    $actual_count
                                                                    $nb
                                                                    $sig
                                                                    (p-if $be
                                                                      1
                                                                      (vector (p-if $le 0 $dbe)))))))
                                                            (p-if
                                                              (p-|| (p-str-eq $ch "A")
                                                                (p-str-eq $ch "a"))
                                                              (progn
                                                                (pl-_pack_str_one $ch
                                                                  (p-string-concat $actual_count)
                                                                  1
                                                                  0
                                                                  $result_ref))
                                                              (p-if (p-str-eq $ch "Z")
                                                                (progn
                                                                  (pl-_pack_str_one "Z"
                                                                    (p-string-concat
                                                                      $actual_count)
                                                                    (p-+
                                                                      (p-length
                                                                        (p-string-concat
                                                                          $actual_count))
                                                                      1)
                                                                    0
                                                                    $result_ref))
                                                                (p-if (p-str-eq $ch "w")
                                                                  (progn
                                                                    (let (($v (make-p-box nil)))
                                                                      (p-my-= $v $actual_count)
                                                                      (p-if (p-== $v 0)
                                                                        (progn
                                                                          (p-.=
                                                                            (p-cast-$
                                                                              $result_ref)
                                                                            (p-chr 0)))
                                                                        (progn
                                                                          (let
                                                                            ((@bytes
                                                                                (make-array 0 :adjustable t :fill-pointer 0)))
                                                                            (p-while (p-> $v 0)
                                                                              (p-unshift @bytes
                                                                                (p-bit-and $v
                                                                                  #x7F))
                                                                              (p->>= $v 7))
                                                                            (let (($k 0))
                                                                              (p-for ()
                                                                                ((p-< $k
                                                                                    (p-array-last-index
                                                                                      @bytes)))
                                                                                ((p-incf-raw $k))
                                                                                (p-.=
                                                                                  (p-cast-$
                                                                                    $result_ref)
                                                                                  (p-chr
                                                                                    (p-bit-or
                                                                                      (p-aref
                                                                                        @bytes
                                                                                        $k)
                                                                                      #x80)))))
                                                                            (p-.=
                                                                              (p-cast-$
                                                                                $result_ref)
                                                                              (p-chr
                                                                                (p-aref @bytes
                                                                                  -1))))))))))))
                                                          (pl-_pack_str_one $dfmt
                                                            $darg
                                                            $actual_count
                                                            0
                                                            $result_ref)))))
                                                  (p-if $dnb
                                                    (progn
                                                      (let (($remaining (make-p-box nil)))
                                                        (p-my-= $remaining
                                                          (p-- $nargs (p-cast-$ $ai_ref)))
                                                        (p-if (p-|| (p-! $had_dcount) $dstar2)
                                                          (progn
                                                            (p-my-= $actual_count $remaining))
                                                          (progn
                                                            (p-my-= $actual_count
                                                              (p-if (p-< $dnrep2 $remaining)
                                                                $dnrep2
                                                                $remaining))))
                                                        (let
                                                          (($nb (make-p-box nil))
                                                            ($sig (make-p-box nil))
                                                            ($dbe (make-p-box nil)))
                                                          (let ((*wantarray* nil))
                                                            (p-list-= (vector $nb $sig $dbe)
                                                              (let ((*wantarray* t))
                                                                (pl-_pack_type_info $ch $bang))))
                                                          (p-if $nb
                                                            (progn
                                                              (p-.= (p-cast-$ $result_ref)
                                                                (let ((*wantarray* nil))
                                                                  (pl-_pack_emit_int
                                                                    $actual_count
                                                                    $nb
                                                                    $sig
                                                                    (p-if $be
                                                                      1
                                                                      (vector (p-if $le 0 $dbe)))))))
                                                            (p-if
                                                              (p-|| (p-str-eq $ch "A")
                                                                (p-str-eq $ch "a"))
                                                              (progn
                                                                (pl-_pack_str_one $ch
                                                                  (p-string-concat $actual_count)
                                                                  1
                                                                  0
                                                                  $result_ref))
                                                              (p-if (p-str-eq $ch "Z")
                                                                (progn
                                                                  (pl-_pack_str_one "Z"
                                                                    (p-string-concat
                                                                      $actual_count)
                                                                    (p-+
                                                                      (p-length
                                                                        (p-string-concat
                                                                          $actual_count))
                                                                      1)
                                                                    0
                                                                    $result_ref))
                                                                (p-if (p-str-eq $ch "w")
                                                                  (progn
                                                                    (let (($v (make-p-box nil)))
                                                                      (p-my-= $v $actual_count)
                                                                      (p-if (p-== $v 0)
                                                                        (progn
                                                                          (p-.=
                                                                            (p-cast-$
                                                                              $result_ref)
                                                                            (p-chr 0)))
                                                                        (progn
                                                                          (let
                                                                            ((@bytes
                                                                                (make-array 0 :adjustable t :fill-pointer 0)))
                                                                            (p-while (p-> $v 0)
                                                                              (p-unshift @bytes
                                                                                (p-bit-and $v
                                                                                  #x7F))
                                                                              (p->>= $v 7))
                                                                            (let (($k 0))
                                                                              (p-for ()
                                                                                ((p-< $k
                                                                                    (p-array-last-index
                                                                                      @bytes)))
                                                                                ((p-incf-raw $k))
                                                                                (p-.=
                                                                                  (p-cast-$
                                                                                    $result_ref)
                                                                                  (p-chr
                                                                                    (p-bit-or
                                                                                      (p-aref
                                                                                        @bytes
                                                                                        $k)
                                                                                      #x80)))))
                                                                            (p-.=
                                                                              (p-cast-$
                                                                                $result_ref)
                                                                              (p-chr
                                                                                (p-aref @bytes
                                                                                  -1))))))))))))
                                                          (let (($dbe_eff (make-p-box nil)))
                                                            (p-my-= $dbe_eff
                                                              (p-if $dbe2
                                                                1
                                                                (p-if $dle2 0 $ddbe)))
                                                            (let (($i__cond__0 0))
                                                              (p-for ()
                                                                ((p-&&
                                                                    (p-< $i__cond__0
                                                                      $actual_count)
                                                                    (p-< (p-cast-$ $ai_ref)
                                                                      $nargs)))
                                                                ((p-incf-raw $i__cond__0))
                                                                (let (($val (make-p-box nil)))
                                                                  (p-my-= $val
                                                                    (p-aref-deref $args_ref
                                                                      (p-post++
                                                                        (p-cast-$ $ai_ref))))
                                                                  (p-.= (p-cast-$ $result_ref)
                                                                    (let ((*wantarray* nil))
                                                                      (pl-_pack_emit_int $val
                                                                        $dnb
                                                                        $dsig
                                                                        $dbe_eff))))))))))
                                                    (p-if (p-str-eq $dfmt "w")
                                                      (progn
                                                        (let (($remaining (make-p-box nil)))
                                                          (p-my-= $remaining
                                                            (p-- $nargs (p-cast-$ $ai_ref)))
                                                          (p-my-= $actual_count
                                                            (p-if
                                                              (p-|| (p-! $had_dcount) $dstar2)
                                                              $remaining
                                                              (p-if (p-< $dnrep2 $remaining)
                                                                $dnrep2
                                                                $remaining)))
                                                          (let
                                                            (($nb (make-p-box nil))
                                                              ($sig (make-p-box nil))
                                                              ($dbe (make-p-box nil)))
                                                            (let ((*wantarray* nil))
                                                              (p-list-= (vector $nb $sig $dbe)
                                                                (let ((*wantarray* t))
                                                                  (pl-_pack_type_info $ch
                                                                    $bang))))
                                                            (p-if $nb
                                                              (progn
                                                                (p-.= (p-cast-$ $result_ref)
                                                                  (let ((*wantarray* nil))
                                                                    (pl-_pack_emit_int
                                                                      $actual_count
                                                                      $nb
                                                                      $sig
                                                                      (p-if $be
                                                                        1
                                                                        (vector
                                                                          (p-if $le 0 $dbe)))))))
                                                              (p-if (p-str-eq $ch "w")
                                                                (progn
                                                                  (let (($v (make-p-box nil)))
                                                                    (p-my-= $v $actual_count)
                                                                    (p-if (p-== $v 0)
                                                                      (progn
                                                                        (p-.=
                                                                          (p-cast-$ $result_ref)
                                                                          (p-chr 0)))
                                                                      (progn
                                                                        (let
                                                                          ((@bytes
                                                                              (make-array 0 :adjustable t :fill-pointer 0)))
                                                                          (p-while (p-> $v 0)
                                                                            (p-unshift @bytes
                                                                              (p-bit-and $v
                                                                                #x7F))
                                                                            (p->>= $v 7))
                                                                          (let (($k 0))
                                                                            (p-for ()
                                                                              ((p-< $k
                                                                                  (p-array-last-index
                                                                                    @bytes)))
                                                                              ((p-incf-raw $k))
                                                                              (p-.=
                                                                                (p-cast-$
                                                                                  $result_ref)
                                                                                (p-chr
                                                                                  (p-bit-or
                                                                                    (p-aref
                                                                                      @bytes
                                                                                      $k)
                                                                                    #x80)))))
                                                                          (p-.=
                                                                            (p-cast-$
                                                                              $result_ref)
                                                                            (p-chr
                                                                              (p-aref @bytes
                                                                                -1))))))))))
                                                            (let (($i__cond__1 0))
                                                              (p-for ()
                                                                ((p-&&
                                                                    (p-< $i__cond__1
                                                                      $actual_count)
                                                                    (p-< (p-cast-$ $ai_ref)
                                                                      $nargs)))
                                                                ((p-incf-raw $i__cond__1))
                                                                (let (($v (make-p-box nil)))
                                                                  (p-my-= $v
                                                                    (p-+
                                                                      (p-aref-deref $args_ref
                                                                        (p-post++
                                                                          (p-cast-$ $ai_ref)))
                                                                      0))
                                                                  (p-if (p-== $v 0)
                                                                    (progn
                                                                      (p-.=
                                                                        (p-cast-$ $result_ref)
                                                                        (p-chr 0)))
                                                                    (progn
                                                                      (let
                                                                        ((@bytes
                                                                            (make-array 0 :adjustable t :fill-pointer 0)))
                                                                        (p-while (p-> $v 0)
                                                                          (p-unshift @bytes
                                                                            (p-bit-and $v #x7F))
                                                                          (p->>= $v 7))
                                                                        (let (($k 0))
                                                                          (p-for ()
                                                                            ((p-< $k
                                                                                (p-array-last-index
                                                                                  @bytes)))
                                                                            ((p-incf-raw $k))
                                                                            (p-.=
                                                                              (p-cast-$
                                                                                $result_ref)
                                                                              (p-chr
                                                                                (p-bit-or
                                                                                  (p-aref @bytes
                                                                                    $k)
                                                                                  #x80)))))
                                                                        (p-.=
                                                                          (p-cast-$ $result_ref)
                                                                          (p-chr
                                                                            (p-aref @bytes -1)))))))))))))))
                                                (p-next))))))))))
                              (p-if (p-defined $grpbeg)
                                (progn
                                  (let (($inner (make-p-box nil)))
                                    (p-my-= $inner
                                      (p-substr $tmpl $grpbeg (p-- $grpend $grpbeg)))
                                    (let
                                      (($gti
                                          (%pcl-to-number-strict (pl-_pack_skip_ws $inner 0)
                                            "$gti")))
                                      (p-if (p-< $gti (p-length $inner))
                                        (progn
                                          (let (($fc (make-p-box nil)))
                                            (p-my-= $fc (p-substr $inner $gti 1))
                                            (p-if
                                              (let ((*wantarray* nil))
                                                (p-=~ $fc (p-regex "/^[\\d\\*\\[]/")))
                                              (p-die :loc
                                                "cl/pack-impl.pl line 556"
                                                "()-group starts with a count in pack
")))))
                                      (p-if $star
                                        (progn
                                          (p-while (p-< (p-cast-$ $ai_ref) $nargs)
                                            (let
                                              (($ai_before
                                                  (%pcl-to-number-strict (p-cast-$ $ai_ref)
                                                    "$ai_before")))
                                              (let (($iter_base (make-p-box nil)))
                                                (p-my-= $iter_base
                                                  (p-length (p-cast-$ $result_ref)))
                                                (pl-_pack_tmpl $inner
                                                  $ai_ref
                                                  $args_ref
                                                  $result_ref
                                                  $be
                                                  $le
                                                  $iter_base
                                                  (p-+ $depth 1))
                                                (p-if (p-== (p-cast-$ $ai_ref) $ai_before)
                                                  (p-last))))))
                                        (progn
                                          (let (($r__cond__15 0))
                                            (p-for ()
                                              ((p-< $r__cond__15 $nrep))
                                              ((p-incf-raw $r__cond__15))
                                              (let (($iter_base (make-p-box nil)))
                                                (p-my-= $iter_base
                                                  (p-length (p-cast-$ $result_ref)))
                                                (pl-_pack_tmpl $inner
                                                  $ai_ref
                                                  $args_ref
                                                  $result_ref
                                                  $be
                                                  $le
                                                  $iter_base
                                                  (p-+ $depth 1)))))))
                                      (p-next)))))
                              (p-if (p-str-eq $ch "x")
                                (progn
                                  (p-if $bang
                                    (progn
                                      (let (($n (make-p-box nil)))
                                        (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                        (let (($cur (make-p-box nil)))
                                          (p-my-= $cur (p-length (p-cast-$ $result_ref)))
                                          (let (($pad (p-% (p-- $n (p-% $cur $n)) $n)))
                                            (p-.= (p-cast-$ $result_ref)
                                              (p-str-x (p-chr 0) $pad))))))
                                    (progn
                                      (p-.= (p-cast-$ $result_ref) (p-str-x (p-chr 0) $nrep))))
                                  (p-next)))
                              (p-if (p-str-eq $ch "X")
                                (progn
                                  (p-if $bang
                                    (progn
                                      (let (($n (make-p-box nil)))
                                        (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                        (let (($cur (make-p-box nil)))
                                          (p-my-= $cur (p-length (p-cast-$ $result_ref)))
                                          (p-setf (p-cast-$ $result_ref)
                                            (p-substr (p-cast-$ $result_ref)
                                              0
                                              (p-* (p-int (p-/ $cur $n)) $n))))))
                                    (progn
                                      (let
                                        (($fp
                                            (%pcl-to-number-strict
                                              (p-- (p-length (p-cast-$ $result_ref)) $nrep)
                                              "$fp")))
                                        (p-setf (p-cast-$ $result_ref)
                                          (p-substr (p-cast-$ $result_ref)
                                            0
                                            (p-if (p-< $fp 0) 0 $fp))))))
                                  (p-next)))
                              (p-if (p-str-eq $ch "@")
                                (progn
                                  (let (($n (make-p-box nil)))
                                    (p-my-= $n (p-if (p-defined $count) $count 0))
                                    (let
                                      (($t
                                          (%pcl-to-number-strict
                                            (p-if $bang $n (p-+ $out_base $n))
                                            "$t")))
                                      (p-if (p-< (p-length (p-cast-$ $result_ref)) $t)
                                        (progn
                                          (p-.= (p-cast-$ $result_ref)
                                            (p-str-x (p-chr 0)
                                              (p-- $t (p-length (p-cast-$ $result_ref))))))
                                        (p-if (p-> (p-length (p-cast-$ $result_ref)) $t)
                                          (progn
                                            (p-setf (p-cast-$ $result_ref)
                                              (p-substr (p-cast-$ $result_ref) 0 $t)))))
                                      (p-next)))))
                              (p-if (p-str-eq $ch ".")
                                (progn
                                  (let (($tgt (make-p-box nil)))
                                    (p-my-= $tgt
                                      (p-if (p-< (p-cast-$ $ai_ref) $nargs)
                                        (p-int
                                          (p-+
                                            (p-//
                                              (p-aref-deref $args_ref
                                                (p-post++ (p-cast-$ $ai_ref)))
                                              0)
                                            0))
                                        0))
                                    (let
                                      (($abs_tgt
                                          (%pcl-to-number-strict
                                            (p-if $star $tgt (p-+ $out_base $tgt))
                                            "$abs_tgt")))
                                      (let (($cur (make-p-box nil)))
                                        (p-my-= $cur (p-length (p-cast-$ $result_ref)))
                                        (p-if (p-< $cur $abs_tgt)
                                          (progn
                                            (p-.= (p-cast-$ $result_ref)
                                              (p-str-x (p-chr 0) (p-- $abs_tgt $cur))))
                                          (p-if (p-> $cur $abs_tgt)
                                            (progn
                                              (p-setf (p-cast-$ $result_ref)
                                                (p-substr (p-cast-$ $result_ref) 0 $abs_tgt)))))
                                        (p-next))))))
                              (p-if
                                (p-|| (p-|| (p-str-eq $ch "p") (p-str-eq $ch "P"))
                                  (p-str-eq $ch "D"))
                                (progn
                                  (p-die :loc
                                    "cl/pack-impl.pl line 618"
                                    (p-string-concat "Invalid type '"
                                      $ch
                                      "' in pack
"))))
                              (p-if $star (p-my-= $nrep (p-- $nargs (p-cast-$ $ai_ref))))
                              (let
                                (($nb (make-p-box nil))
                                  ($sig (make-p-box nil))
                                  ($dbe (make-p-box nil)))
                                (let ((*wantarray* nil))
                                  (p-list-= (vector $nb $sig $dbe)
                                    (let ((*wantarray* t)) (pl-_pack_type_info $ch $bang))))
                                (p-if $nb
                                  (progn
                                    (let (($be2 (make-p-box nil)))
                                      (p-my-= $be2 (p-if $be 1 (p-if $le 0 $dbe)))
                                      (let (($r__cond__16 0))
                                        (p-for ()
                                          ((p-< $r__cond__16 $nrep))
                                          ((p-incf-raw $r__cond__16))
                                          (let (($v (make-p-box nil)))
                                            (p-my-= $v
                                              (p-if (p-< (p-cast-$ $ai_ref) $nargs)
                                                (p-//
                                                  (p-aref-deref $args_ref
                                                    (p-post++ (p-cast-$ $ai_ref)))
                                                  0)
                                                0))
                                            (let (($nv (make-p-box nil)))
                                              (p-my-= $nv (p-+ $v 0))
                                              (p-if (p-!= $nv $nv)
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 631"
                                                  "Cannot pack NaN in pack
"))
                                              (p-if (p-&& (p-!= $nv 0) (p-== $nv (p-* $nv 2)))
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 632"
                                                  (p-.
                                                    (p-. "Cannot pack "
                                                      (p-if (p-< $nv 0) "-Inf" "Inf"))
                                                    " in pack
")))
                                              (p-.= (p-cast-$ $result_ref)
                                                (let ((*wantarray* nil))
                                                  (pl-_pack_emit_int
                                                    (let ((*wantarray* t)) (p-int $nv))
                                                    $nb
                                                    $sig
                                                    $be2)))))))
                                      (p-next))))
                                (p-if (p-str-eq $ch "f")
                                  (progn
                                    (let (($be2 (make-p-box nil)))
                                      (p-my-= $be2 (p-if $be 1 (p-if $le 0 0)))
                                      (let (($r__cond__17 0))
                                        (p-for ()
                                          ((p-< $r__cond__17 $nrep))
                                          ((p-incf-raw $r__cond__17))
                                          (let (($v (make-p-box nil)))
                                            (p-my-= $v
                                              (p-if (p-< (p-cast-$ $ai_ref) $nargs)
                                                (p-//
                                                  (p-aref-deref $args_ref
                                                    (p-post++ (p-cast-$ $ai_ref)))
                                                  0)
                                                0))
                                            (p-.= (p-cast-$ $result_ref)
                                              (let ((*wantarray* nil))
                                                (pl-_pack_float32 $v $be2))))))
                                      (p-next))))
                                (p-if (p-|| (p-str-eq $ch "d") (p-str-eq $ch "F"))
                                  (progn
                                    (let (($be2 (make-p-box nil)))
                                      (p-my-= $be2 (p-if $be 1 (p-if $le 0 0)))
                                      (let (($r__cond__18 0))
                                        (p-for ()
                                          ((p-< $r__cond__18 $nrep))
                                          ((p-incf-raw $r__cond__18))
                                          (let (($v (make-p-box nil)))
                                            (p-my-= $v
                                              (p-if (p-< (p-cast-$ $ai_ref) $nargs)
                                                (p-//
                                                  (p-aref-deref $args_ref
                                                    (p-post++ (p-cast-$ $ai_ref)))
                                                  0)
                                                0))
                                            (p-.= (p-cast-$ $result_ref)
                                              (let ((*wantarray* nil))
                                                (pl-_pack_float64 $v $be2))))))
                                      (p-next))))
                                (p-if
                                  (p-||
                                    (p-||
                                      (p-||
                                        (p-||
                                          (p-||
                                            (p-|| (p-|| (p-str-eq $ch "a") (p-str-eq $ch "A"))
                                              (p-str-eq $ch "Z"))
                                            (p-str-eq $ch "b"))
                                          (p-str-eq $ch "B"))
                                        (p-str-eq $ch "H"))
                                      (p-str-eq $ch "h"))
                                    (p-str-eq $ch "u"))
                                  (progn
                                    (let (($arg (make-p-box nil)))
                                      (p-my-= $arg
                                        (p-if (p-< (p-cast-$ $ai_ref) $nargs)
                                          (p-//
                                            (p-aref-deref $args_ref
                                              (p-post++ (p-cast-$ $ai_ref)))
                                            "")
                                          ""))
                                      (pl-_pack_str_one $ch $arg $nrep $star $result_ref)
                                      (p-next))))
                                (p-if (p-str-eq $ch "U")
                                  (progn
                                    (let (($r__cond__19 0))
                                      (p-for ()
                                        ((p-< $r__cond__19 $nrep))
                                        ((p-incf-raw $r__cond__19))
                                        (let (($v (make-p-box nil)))
                                          (p-my-= $v
                                            (p-if (p-< (p-cast-$ $ai_ref) $nargs)
                                              (p-//
                                                (p-aref-deref $args_ref
                                                  (p-post++ (p-cast-$ $ai_ref)))
                                                0)
                                              0))
                                          (let (($nv (make-p-box nil)))
                                            (p-my-= $nv (p-+ $v 0))
                                            (p-if (p-!= $nv $nv)
                                              (p-die :loc
                                                "cl/pack-impl.pl line 669"
                                                "Cannot pack NaN in pack
"))
                                            (p-if (p-&& (p-!= $nv 0) (p-== $nv (p-* $nv 2)))
                                              (p-die :loc
                                                "cl/pack-impl.pl line 670"
                                                (p-.
                                                  (p-. "Cannot pack "
                                                    (p-if (p-< $nv 0) "-Inf" "Inf"))
                                                  " in pack
")))
                                            (pl-_pack_utf8_char
                                              (let ((*wantarray* t)) (p-int $nv))
                                              $result_ref)))))
                                    (p-next)))
                                (p-if (p-str-eq $ch "W")
                                  (progn
                                    (let (($r__cond__20 0))
                                      (p-for ()
                                        ((p-< $r__cond__20 $nrep))
                                        ((p-incf-raw $r__cond__20))
                                        (let (($v (make-p-box nil)))
                                          (p-my-= $v
                                            (p-if (p-< (p-cast-$ $ai_ref) $nargs)
                                              (p-//
                                                (p-aref-deref $args_ref
                                                  (p-post++ (p-cast-$ $ai_ref)))
                                                0)
                                              0))
                                          (let (($nv (make-p-box nil)))
                                            (p-my-= $nv (p-+ $v 0))
                                            (p-if (p-!= $nv $nv)
                                              (p-die :loc
                                                "cl/pack-impl.pl line 679"
                                                "Cannot pack NaN in pack
"))
                                            (p-if (p-&& (p-!= $nv 0) (p-== $nv (p-* $nv 2)))
                                              (p-die :loc
                                                "cl/pack-impl.pl line 680"
                                                (p-.
                                                  (p-. "Cannot pack "
                                                    (p-if (p-< $nv 0) "-Inf" "Inf"))
                                                  " in pack
")))
                                            (p-.= (p-cast-$ $result_ref) (p-chr (p-int $nv)))))))
                                    (p-next)))
                                (p-if (p-str-eq $ch "w")
                                  (progn
                                    (let (($r__cond__21 0))
                                      (p-for ()
                                        ((p-< $r__cond__21 $nrep))
                                        ((p-incf-raw $r__cond__21))
                                        (let (($raw (make-p-box nil)))
                                          (p-my-= $raw
                                            (p-if (p-< (p-cast-$ $ai_ref) $nargs)
                                              (p-//
                                                (p-aref-deref $args_ref
                                                  (p-post++ (p-cast-$ $ai_ref)))
                                                0)
                                              0))
                                          (let (($orig_s (make-p-box nil)))
                                            (p-my-= $orig_s (p-string-concat $raw))
                                            (let (($v (make-p-box nil)))
                                              (p-my-= $v (p-+ $raw 0))
                                              (p-if (p-!= $v $v)
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 690"
                                                  "Cannot compress NaN in pack
"))
                                              (p-if (p-&& (p-< $v 0) (p-== $v (p-* $v 2)))
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 691"
                                                  "Cannot compress -Inf in pack
"))
                                              (p-if (p-< $v 0)
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 692"
                                                  "Cannot compress negative numbers in pack
"))
                                              (p-if (p-&& (p-!= $v 0) (p-== $v (p-* $v 2)))
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 693"
                                                  "Cannot compress Inf in pack
"))
                                              (p-if (p-!= $v (p-int $v))
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 694"
                                                  "Can only compress unsigned integers in pack
"))
                                              (p-if
                                                (p-&&
                                                  (let ((*wantarray* nil))
                                                    (p-=~ $orig_s (p-regex "/[eE]/")))
                                                  (p->= $v (p-** 2 64)))
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 700"
                                                  "Can only compress unsigned integers in pack
"))
                                              (p-my-= $v (p-int $v))
                                              (p-if (p-== $v 0)
                                                (progn (p-.= (p-cast-$ $result_ref) (p-chr 0))
                                                  (p-next)))
                                              (let
                                                ((@bytes
                                                    (make-array 0 :adjustable t :fill-pointer 0)))
                                                (p-while (p-> $v 0)
                                                  (p-unshift @bytes (p-bit-and $v #x7F))
                                                  (p->>= $v 7))
                                                (let (($k 0))
                                                  (p-for ()
                                                    ((p-< $k (p-array-last-index @bytes)))
                                                    ((p-incf-raw $k))
                                                    (p-.= (p-cast-$ $result_ref)
                                                      (p-chr (p-bit-or (p-aref @bytes $k) #x80)))))
                                                (p-.= (p-cast-$ $result_ref)
                                                  (p-chr (p-aref @bytes -1)))))))))
                                    (p-next)))
                                (p-if (p-str-eq $ch "/")
                                  (p-die :loc
                                    "cl/pack-impl.pl line 712"
                                    "Invalid type '/' in pack
"))
                                (p-die :loc
                                  "cl/pack-impl.pl line 713"
                                  (p-string-concat "Invalid type '"
                                    $ch
                                    "' in pack
"))))))))))))))))))

(p-sub pl-_pack_check_brackets
  (&rest %_args)
  (p-raw-params ($tmpl)
    (block nil
      (let ((*wantarray* :void))
        (let (($n_open (make-p-box nil)) ($n_close (make-p-box nil)))
          (let ((*wantarray* nil)) (p-list-= (vector $n_open $n_close) (vector 0 0)))
          (let (($tlen (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
            (let (($i__cond__2 0))
              (p-for ()
                ((p-< $i__cond__2 $tlen))
                ((p-incf-raw $i__cond__2))
                (let (($c (make-p-box nil)))
                  (p-my-= $c (p-substr $tmpl $i__cond__2 1))
                  (p-if (p-str-eq $c "[")
                    (progn (p-post++ $n_open))
                    (p-if (p-str-eq $c "]") (progn (p-post++ $n_close)))))))
            (p-if (p-> $n_open $n_close)
              (p-die :loc
                "cl/pack-impl.pl line 726"
                "No group ending character ']' found in template
"))
            (p-if (p-! (p-> $n_open 0)) (p-return))
            (let ((@stk (make-array 0 :adjustable t :fill-pointer 0)))
              (p-array-= @stk (vector))
              (let (($i__cond__3 0))
                (p-for ()
                  ((p-< $i__cond__3 $tlen))
                  ((p-incf-raw $i__cond__3))
                  (let (($c (make-p-box nil)))
                    (p-my-= $c (p-substr $tmpl $i__cond__3 1))
                    (p-if (p-str-eq $c "[")
                      (progn (p-push @stk "["))
                      (p-if (p-str-eq $c "(")
                        (progn (p-push @stk "("))
                        (p-if (p-str-eq $c "]")
                          (progn
                            (p-if (p-|| (p-! @stk) (p-str-ne (p-aref @stk -1) "["))
                              (p-die :loc
                                "cl/pack-impl.pl line 735"
                                "Mismatched brackets in template
"))
                            (p-pop @stk))
                          (p-if (p-str-eq $c ")")
                            (progn
                              (p-if (p-&& @stk (p-str-eq (p-aref @stk -1) "(")) (p-pop @stk)))))))))))))))))

(p-sub pl-p_pack
  (&rest %_args)
  (p-args-body
    (block nil
      (let ((*wantarray* :void))
        (let (($tmpl (make-p-box nil)) (@args (make-array 0 :adjustable t :fill-pointer 0)))
          (let ((*wantarray* nil)) (p-list-= (vector $tmpl @args) @_))
          ;; local $pcl_pack_comma_warned = 0
(let (($pcl_pack_comma_warned (p-box-for-local 0)))
            (pl-_pack_check_brackets $tmpl)
            (let (($result (make-p-box nil)))
              (p-my-= $result "")
              (let (($ai (make-p-box nil)))
                (p-my-= $ai 0)
                (pl-_pack_tmpl $tmpl
                  (p-backslash $ai)
                  (p-backslash @args)
                  (p-backslash $result)
                  0
                  0)
                (p-return $result)))))))))

(p-sub pl-_unpack_utf8_char
  (&rest %_args)
  (p-args-body
    (block nil
      (let (($s (make-p-box nil)) ($si_ref (make-p-box nil)))
        (let ((*wantarray* nil)) (p-list-= (vector $s $si_ref) @_))
        (let ((*wantarray* :void))
          (let (($slen (%pcl-to-number-strict (p-length $s) "$slen")))
            (p-if (p->= (p-cast-$ $si_ref) $slen) (p-return 0))
            (let (($b0 (make-p-box nil)))
              (p-my-= $b0 (p-ord (p-substr $s (p-cast-$ $si_ref) 1)))
              (let (($nb (make-p-box nil)) ($code (make-p-box nil)))
                (p-if (p-< $b0 #x80)
                  (progn (setf $nb 1) (p-my-= $code $b0))
                  (p-if (p-< $b0 #xE0)
                    (progn (setf $nb 2) (p-my-= $code (p-bit-and $b0 #x1F)))
                    (p-if (p-< $b0 #xF0)
                      (progn (setf $nb 3) (p-my-= $code (p-bit-and $b0 #x0F)))
                      (progn (setf $nb 4) (p-my-= $code (p-bit-and $b0 #x07))))))
                (let (($k 1))
                  (p-for ()
                    ((p-< $k $nb))
                    ((p-incf-raw $k))
                    (p-if (p-< (p-+ (p-cast-$ $si_ref) $k) $slen)
                      (p-my-= $code
                        (p-bit-or (p-<< $code 6)
                          (p-bit-and (p-ord (p-substr $s (p-+ (p-cast-$ $si_ref) $k) 1)) #x3F))))))
                (p-incf (p-cast-$ $si_ref) $nb)
                (p-return $code)))))))))

(p-sub pl-_unpack_str
  (&rest %_args)
  (p-args-body
    (block nil
      (let
        (($ch (make-p-box nil))
          ($nrep (make-p-box nil))
          ($all (make-p-box nil))
          ($s (make-p-box nil))
          ($si_ref (make-p-box nil))
          ($push_val (make-p-box nil))
          ($checksum_p (make-p-box nil)))
        (let ((*wantarray* nil))
          (p-list-= (vector $ch $nrep $all $s $si_ref $push_val $checksum_p) @_))
        (let ((*wantarray* :void))
          (let (($slen (make-p-box nil)))
            (p-my-= $slen (p-length $s))
            (let ((--pcl-if-ret--2 nil))
              (p-if
                (setf --pcl-if-ret--2
                  (p-|| (p-|| (p-str-eq $ch "A") (p-str-eq $ch "a")) (p-str-eq $ch "Z")))
                (setf --pcl-if-ret--2
                  (progn
                    (let (($n (make-p-box nil)))
                      (p-my-= $n (p-if $all (p-- $slen (p-cast-$ $si_ref)) $nrep))
                      (p-if (p-< $n 0) (p-my-= $n 0))
                      (let (($raw (make-p-box nil)))
                        (p-my-= $raw
                          (p-if (p-< (p-cast-$ $si_ref) $slen)
                            (p-substr $s (p-cast-$ $si_ref) $n)
                            ""))
                        (p-incf (p-cast-$ $si_ref) $n)
                        (p-if (p-str-eq $ch "A") (p-=~ $raw (p-subst "[ \\x00]+$" "")))
                        (p-if (p-str-eq $ch "Z") (p-=~ $raw (p-subst "\\x00.*" "" :s)))
                        (let ((*wantarray* *pcl-caller-wantarray*))
                          (p-funcall-ref $push_val $raw))))))
                (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "H"))
                  (setf --pcl-if-ret--2
                    (progn
                      (let (($n (make-p-box nil)))
                        (p-my-= $n (p-if $all (p-* 2 (p-- $slen (p-cast-$ $si_ref))) $nrep))
                        (let (($hex (make-p-box nil)))
                          (p-my-= $hex "")
                          (let (($i__cond__4 0))
                            (p-for ()
                              ((p-< $i__cond__4 (p-int (p-/ $n 2))))
                              ((p-incf-raw $i__cond__4))
                              (let (($b (make-p-box nil)))
                                (p-my-= $b
                                  (p-if (p-< (p-+ (p-cast-$ $si_ref) $i__cond__4) $slen)
                                    (p-ord (p-substr $s (p-+ (p-cast-$ $si_ref) $i__cond__4) 1))
                                    0))
                                (p-.= $hex (p-sprintf "%02x" $b)))))
                          (p-my-= $hex (p-substr $hex 0 $n))
                          (p-incf (p-cast-$ $si_ref) (p-int (p-/ (p-+ $n 1) 2)))
                          (let ((*wantarray* *pcl-caller-wantarray*))
                            (p-funcall-ref $push_val $hex))))))
                  (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "h"))
                    (setf --pcl-if-ret--2
                      (progn
                        (let (($n (make-p-box nil)))
                          (p-my-= $n (p-if $all (p-* 2 (p-- $slen (p-cast-$ $si_ref))) $nrep))
                          (let (($hex (make-p-box nil)))
                            (p-my-= $hex "")
                            (let (($i__cond__5 0))
                              (p-for ()
                                ((p-< $i__cond__5 (p-int (p-/ $n 2))))
                                ((p-incf-raw $i__cond__5))
                                (let (($b (make-p-box nil)))
                                  (p-my-= $b
                                    (p-if (p-< (p-+ (p-cast-$ $si_ref) $i__cond__5) $slen)
                                      (p-ord
                                        (p-substr $s (p-+ (p-cast-$ $si_ref) $i__cond__5) 1))
                                      0))
                                  (p-.= $hex
                                    (p-sprintf "%x%x"
                                      (p-bit-and $b #xF)
                                      (p-bit-and (p->> $b 4) #xF))))))
                            (p-my-= $hex (p-substr $hex 0 $n))
                            (p-incf (p-cast-$ $si_ref) (p-int (p-/ (p-+ $n 1) 2)))
                            (let ((*wantarray* *pcl-caller-wantarray*))
                              (p-funcall-ref $push_val $hex))))))
                    (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "B"))
                      (setf --pcl-if-ret--2
                        (progn
                          (let (($nbits (make-p-box nil)))
                            (p-my-= $nbits
                              (p-if $all (p-* 8 (p-- $slen (p-cast-$ $si_ref))) $nrep))
                            (p-if $checksum_p
                              (progn
                                (let (($i__cond__6 0))
                                  (p-for ()
                                    ((p-< $i__cond__6 $nbits))
                                    ((p-incf-raw $i__cond__6))
                                    (let (($b (make-p-box nil)))
                                      (p-my-= $b
                                        (p-if
                                          (p-<
                                            (p-+ (p-cast-$ $si_ref)
                                              (p-int (p-/ $i__cond__6 8)))
                                            $slen)
                                          (p-ord
                                            (p-substr $s
                                              (p-+ (p-cast-$ $si_ref)
                                                (p-int (p-/ $i__cond__6 8)))
                                              1))
                                          0))
                                      (p-funcall-ref $push_val
                                        (p-bit-and (p->> $b (p-- 7 (p-% $i__cond__6 8))) 1))))))
                              (progn
                                (let (($bits (make-p-box nil)))
                                  (p-my-= $bits "")
                                  (let (($i__cond__7 0))
                                    (p-for ()
                                      ((p-< $i__cond__7 $nbits))
                                      ((p-incf-raw $i__cond__7))
                                      (let (($b (make-p-box nil)))
                                        (p-my-= $b
                                          (p-if
                                            (p-<
                                              (p-+ (p-cast-$ $si_ref)
                                                (p-int (p-/ $i__cond__7 8)))
                                              $slen)
                                            (p-ord
                                              (p-substr $s
                                                (p-+ (p-cast-$ $si_ref)
                                                  (p-int (p-/ $i__cond__7 8)))
                                                1))
                                            0))
                                        (p-.= $bits
                                          (p-if
                                            (p-bit-and (p->> $b (p-- 7 (p-% $i__cond__7 8))) 1)
                                            "1"
                                            "0")))))
                                  (p-funcall-ref $push_val $bits))))
                            (let ((*wantarray* *pcl-caller-wantarray*))
                              (p-incf (p-cast-$ $si_ref) (p-int (p-/ (p-+ $nbits 7) 8)))))))
                      (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "b"))
                        (setf --pcl-if-ret--2
                          (progn
                            (let (($nbits (make-p-box nil)))
                              (p-my-= $nbits
                                (p-if $all (p-* 8 (p-- $slen (p-cast-$ $si_ref))) $nrep))
                              (p-if $checksum_p
                                (progn
                                  (let (($i__cond__8 0))
                                    (p-for ()
                                      ((p-< $i__cond__8 $nbits))
                                      ((p-incf-raw $i__cond__8))
                                      (let (($b (make-p-box nil)))
                                        (p-my-= $b
                                          (p-if
                                            (p-<
                                              (p-+ (p-cast-$ $si_ref)
                                                (p-int (p-/ $i__cond__8 8)))
                                              $slen)
                                            (p-ord
                                              (p-substr $s
                                                (p-+ (p-cast-$ $si_ref)
                                                  (p-int (p-/ $i__cond__8 8)))
                                                1))
                                            0))
                                        (p-funcall-ref $push_val
                                          (p-bit-and (p->> $b (p-% $i__cond__8 8)) 1))))))
                                (progn
                                  (let (($bits (make-p-box nil)))
                                    (p-my-= $bits "")
                                    (let (($i__cond__9 0))
                                      (p-for ()
                                        ((p-< $i__cond__9 $nbits))
                                        ((p-incf-raw $i__cond__9))
                                        (let (($b (make-p-box nil)))
                                          (p-my-= $b
                                            (p-if
                                              (p-<
                                                (p-+ (p-cast-$ $si_ref)
                                                  (p-int (p-/ $i__cond__9 8)))
                                                $slen)
                                              (p-ord
                                                (p-substr $s
                                                  (p-+ (p-cast-$ $si_ref)
                                                    (p-int (p-/ $i__cond__9 8)))
                                                  1))
                                              0))
                                          (p-.= $bits
                                            (p-if (p-bit-and (p->> $b (p-% $i__cond__9 8)) 1)
                                              "1"
                                              "0")))))
                                    (p-funcall-ref $push_val $bits))))
                              (let ((*wantarray* *pcl-caller-wantarray*))
                                (p-incf (p-cast-$ $si_ref) (p-int (p-/ (p-+ $nbits 7) 8)))))))
                        (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "u"))
                          (setf --pcl-if-ret--2
                            (progn
                              (let (($decoded (make-p-box nil)))
                                (p-my-= $decoded "")
                                (p-while (p-< (p-cast-$ $si_ref) $slen)
                                  (let
                                    (($lc
                                        (%pcl-to-number-strict
                                          (p-ord (p-substr $s (p-cast-$ $si_ref) 1))
                                          "$lc")))
                                    (let
                                      (($nb
                                          (%pcl-to-number-strict (p-bit-and (p-- $lc 32) 63)
                                            "$nb")))
                                      (p-post++ (p-cast-$ $si_ref))
                                      (p-if (p-== $nb 0) (p-last))
                                      (let
                                        (($ng
                                            (%pcl-to-number-strict (p-int (p-/ (p-+ $nb 2) 3))
                                              "$ng")))
                                        (let (($k 0))
                                          (p-for ()
                                            ((p-< $k $ng))
                                            ((p-incf-raw $k))
                                            (let (($get (make-p-box nil)))
                                              (p-my-= $get
                                                (lambda (&rest %_args)
                                                  (let
                                                    ((@_ (p-flatten-args %_args))
                                                      (*pcl-caller-wantarray* *wantarray*))
                                                    (catch :p-return
                                                      (block nil
                                                        (let ((*wantarray* :void))
                                                          (let
                                                            (($i
                                                                (%pcl-to-number-strict
                                                                  (p-+ (p-cast-$ $si_ref)
                                                                    (p-aref @_ 0))
                                                                  "$i")))
                                                            (let
                                                              ((*wantarray*
                                                                  *pcl-caller-wantarray*))
                                                              (p-if (p-< $i $slen)
                                                                (p-bit-and
                                                                  (p--
                                                                    (p-ord (p-substr $s $i 1))
                                                                    32)
                                                                  63)
                                                                0)))))))))
                                              (let (($cm (make-p-box nil)))
                                                (p-my-= $cm
                                                  (p-bit-or
                                                    (p-bit-or
                                                      (p-bit-or
                                                        (p-<<
                                                          (let ((*wantarray* nil))
                                                            (p-funcall-ref $get (p-* 4 $k)))
                                                          18)
                                                        (p-<<
                                                          (let ((*wantarray* nil))
                                                            (p-funcall-ref $get
                                                              (p-+ (p-* 4 $k) 1)))
                                                          12))
                                                      (p-<<
                                                        (let ((*wantarray* nil))
                                                          (p-funcall-ref $get
                                                            (p-+ (p-* 4 $k) 2)))
                                                        6))
                                                    (let ((*wantarray* nil))
                                                      (p-funcall-ref $get (p-+ (p-* 4 $k) 3)))))
                                                (p-if (p-< (p-* $k 3) $nb)
                                                  (p-.= $decoded
                                                    (p-chr (p-bit-and (p->> $cm 16) #xFF))))
                                                (p-if (p-< (p-+ (p-* $k 3) 1) $nb)
                                                  (p-.= $decoded
                                                    (p-chr (p-bit-and (p->> $cm 8) #xFF))))
                                                (p-if (p-< (p-+ (p-* $k 3) 2) $nb)
                                                  (p-.= $decoded (p-chr (p-bit-and $cm #xFF))))))))
                                        (p-incf (p-cast-$ $si_ref) (p-* $ng 4))
                                        (p-if
                                          (p-&& (p-< (p-cast-$ $si_ref) $slen)
                                            (p-str-eq (p-substr $s (p-cast-$ $si_ref) 1)
                                              "
"))
                                          (p-post++ (p-cast-$ $si_ref)))))))
                                (let ((*wantarray* *pcl-caller-wantarray*))
                                  (p-funcall-ref $push_val $decoded)))))
                          (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "U"))
                            (setf --pcl-if-ret--2
                              (progn
                                (let (($n (make-p-box nil)))
                                  (p-my-= $n (p-if $all (p-** 9 9) $nrep))
                                  (let (($done (make-p-box nil)))
                                    (p-my-= $done 0)
                                    (p-while
                                      (p-&& (p-< $done $n) (p-< (p-cast-$ $si_ref) $slen))
                                      (p-funcall-ref $push_val
                                        (let ((*wantarray* t))
                                          (pl-_unpack_utf8_char $s $si_ref)))
                                      (p-post++ $done))))))
                            (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "W"))
                              (setf --pcl-if-ret--2
                                (progn
                                  (let (($n (make-p-box nil)))
                                    (p-my-= $n
                                      (p-if $all (p-- $slen (p-cast-$ $si_ref)) $nrep))
                                    (let (($i__cond__10 0))
                                      (p-for ()
                                        ((p-&& (p-< $i__cond__10 $n)
                                            (p-< (p-cast-$ $si_ref) $slen)))
                                        ((p-incf-raw $i__cond__10))
                                        (p-funcall-ref $push_val
                                          (let ((*wantarray* t))
                                            (p-ord (p-substr $s (p-post++ (p-cast-$ $si_ref)) 1)))))))))
                              (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "w"))
                                (setf --pcl-if-ret--2
                                  (progn
                                    (let (($done (make-p-box nil)))
                                      (p-my-= $done 0)
                                      (p-while
                                        (p-&& (p-|| $all (p-< $done $nrep))
                                          (p-< (p-cast-$ $si_ref) $slen))
                                        (let (($v (make-p-box nil)) ($more (make-p-box nil)))
                                          (let ((*wantarray* nil))
                                            (p-list-= (vector $v $more) (vector 0 1)))
                                          (p-while $more
                                            (p-if (p->= (p-cast-$ $si_ref) $slen)
                                              (p-die :loc
                                                "cl/pack-impl.pl line 877"
                                                "Unterminated compressed integer in unpack
"))
                                            (let (($b (make-p-box nil)))
                                              (p-my-= $b
                                                (p-ord
                                                  (p-substr $s (p-post++ (p-cast-$ $si_ref)) 1)))
                                              (p-my-= $more (p-bit-and $b #x80))
                                              (p-my-= $v
                                                (p-bit-or (p-<< $v 7) (p-bit-and $b #x7F)))))
                                          (p-funcall-ref $push_val $v)
                                          (p-post++ $done))))))
                                nil)))))))))
              --pcl-if-ret--2)))))))

(p-sub pl-_unpack_tmpl
  (&rest %_args)
  (p-args-body
    (block nil
      (let
        (($tmpl (make-p-box nil))
          ($s (make-p-box nil))
          ($si_ref (make-p-box nil))
          ($push_val (make-p-box nil))
          ($inh_be (make-p-box nil))
          ($inh_le (make-p-box nil))
          ($checksum_p (make-p-box nil))
          ($group_base (make-p-box nil))
          ($depth (make-p-box nil)))
        (let ((*wantarray* nil))
          (p-list-=
            (vector $tmpl $s $si_ref $push_val $inh_be $inh_le $checksum_p $group_base $depth)
            @_))
        (let ((*wantarray* :void))
          (p-if (p-! (p-defined $group_base)) (p-my-= $group_base 0))
          (p-if (p-! (p-defined $depth)) (p-my-= $depth 0))
          (p-if (p-> $depth $MAX_GROUP_DEPTH)
            (p-die :loc
              "cl/pack-impl.pl line 893"
              "Too deeply nested ()-groups in unpack
"))
          (let (($slen (make-p-box nil)))
            (p-my-= $slen (p-length $s))
            (let (($ti (make-p-box nil)))
              (p-my-= $ti 0)
              (let (($tlen (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
                (p-while 1
                  (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                  (p-if (p->= $ti $tlen) (p-last))
                  (let (($ch (make-p-box nil)))
                    (p-my-= $ch (p-substr $tmpl $ti 1))
                    (p-post++ $ti)
                    (let (($grpbeg (make-p-box nil)) ($grpend (make-p-box nil)))
                      (let ((*wantarray* nil))
                        (p-list-= (vector $grpbeg $grpend)
                          (vector (let ((*wantarray* t)) (p-undef))
                            (let ((*wantarray* t)) (p-undef)))))
                      (p-if (p-str-eq $ch "(")
                        (progn (p-my-= $grpend (pl-_pack_find_group_end $tmpl $ti))
                          (p-my-= $grpbeg $ti)
                          (p-my-= $ti (p-+ $grpend 1))
                          (p-my-= $ch "(")))
                      (let
                        (($bang (make-p-box nil))
                          ($be (make-p-box nil))
                          ($le (make-p-box nil)))
                        (let ((*wantarray* nil))
                          (p-list-= (vector $bang $be $le)
                            (let ((*wantarray* t))
                              (pl-_pack_parse_mods $tmpl
                                (p-backslash $ti)
                                $inh_be
                                $inh_le
                                $ch
                                "unpack"))))
                        (let (($ti_before_count (%pcl-to-number-strict $ti "$ti_before_count")))
                          (let
                            (($all (make-p-box nil))
                              ($count (make-p-box nil))
                              ($nrep (make-p-box nil)))
                            (let ((*wantarray* nil))
                              (p-list-= (vector $all $count $nrep)
                                (let ((*wantarray* t))
                                  (pl-_pack_parse_count $tmpl (p-backslash $ti)))))
                            (let (($had_count (make-p-box nil)))
                              (p-my-= $had_count (p-|| $all (p-> $ti $ti_before_count)))
                              (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                              (p-if (p-&& (p-< $ti $tlen) (p-str-eq (p-substr $tmpl $ti 1) "/"))
                                (progn (p-post++ $ti)
                                  (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                  (p-if (p->= $ti $tlen)
                                    (p-die :loc
                                      "cl/pack-impl.pl line 917"
                                      "Code missing after '/' in unpack
"))
                                  (let ((*package* *package*))
                                    (block nil
                                      (tagbody :redo
                                        (let (($c (make-p-box nil)))
                                          (p-my-= $c (p-substr $tmpl $ti 1))
                                          (p-if
                                            (p-|| (p-|| (p-str-eq $c "*") (p-str-eq $c "["))
                                              (let ((*wantarray* nil))
                                                (p-=~ $c (p-regex "/\\d/"))))
                                            (p-die :loc
                                              "cl/pack-impl.pl line 921"
                                              "'/' does not take a repeat count in unpack
")))
                                        :next)))
                                  (let
                                    (($nb (make-p-box nil))
                                      ($sig (make-p-box nil))
                                      ($dbe (make-p-box nil)))
                                    (let ((*wantarray* nil))
                                      (p-list-= (vector $nb $sig $dbe)
                                        (let ((*wantarray* t)) (pl-_pack_type_info $ch $bang))))
                                    (let (($slash_n (make-p-box nil)))
                                      (p-my-= $slash_n 0)
                                      (p-if $nb
                                        (progn
                                          (let (($be2 (make-p-box nil)))
                                            (p-my-= $be2 (p-if $be 1 (p-if $le 0 $dbe)))
                                            (p-if (p-> (p-+ (p-cast-$ $si_ref) $nb) $slen)
                                              (progn (p-if (p-! (p-> $depth 0)) (p-last))
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 929"
                                                  "length/code after end of string in unpack
")))
                                            (p-my-= $slash_n
                                              (let ((*wantarray* nil))
                                                (pl-_unpack_read_int $s
                                                  (p-cast-$ $si_ref)
                                                  $nb
                                                  $be2
                                                  $sig)))
                                            (p-incf (p-cast-$ $si_ref) $nb)))
                                        (p-if (p-str-eq $ch "w")
                                          (progn
                                            (let (($more (make-p-box nil)))
                                              (p-my-= $more 1)
                                              (p-while $more
                                                (p-if (p->= (p-cast-$ $si_ref) $slen)
                                                  (p-die :loc
                                                    "cl/pack-impl.pl line 936"
                                                    "Unterminated compressed integer in unpack
"))
                                                (let (($b (make-p-box nil)))
                                                  (p-my-= $b
                                                    (p-ord
                                                      (p-substr $s
                                                        (p-post++ (p-cast-$ $si_ref))
                                                        1)))
                                                  (p-my-= $more (p-bit-and $b #x80))
                                                  (p-my-= $slash_n
                                                    (p-bit-or (p-<< $slash_n 7)
                                                      (p-bit-and $b #x7F)))))))
                                          (p-if (p-str-eq $ch "Z")
                                            (progn
                                              (let
                                                (($end
                                                    (%pcl-to-number-strict
                                                      (p-index $s " " (p-cast-$ $si_ref))
                                                      "$end")))
                                                (p-if (p-< $end 0)
                                                  (progn
                                                    (setf $end
                                                      (%pcl-to-number-strict $slen "$end"))))
                                                (let (($raw (make-p-box nil)))
                                                  (p-my-= $raw
                                                    (p-substr $s
                                                      (p-cast-$ $si_ref)
                                                      (p-- $end (p-cast-$ $si_ref))))
                                                  (p-setf (p-cast-$ $si_ref) (p-+ $end 1))
                                                  (p-if (p-> (p-cast-$ $si_ref) $slen)
                                                    (p-setf (p-cast-$ $si_ref) $slen))
                                                  (p-my-= $slash_n (p-+ $raw 0)))))
                                            (progn
                                              (let (($n (make-p-box nil)))
                                                (p-my-= $n
                                                  (p-if $all
                                                    (p-- $slen (p-cast-$ $si_ref))
                                                    $nrep))
                                                (let (($raw (make-p-box nil)))
                                                  (p-my-= $raw
                                                    (p-if (p-< (p-cast-$ $si_ref) $slen)
                                                      (p-substr $s (p-cast-$ $si_ref) $n)
                                                      ""))
                                                  (p-incf (p-cast-$ $si_ref) $n)
                                                  (p-if (p-str-eq $ch "A")
                                                    (p-=~ $raw (p-subst "[ \\x00]+$" "")))
                                                  (p-my-= $slash_n (p-+ $raw 0))))))))
                                      (p-while 1
                                        (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                        (p-if (p->= $ti $tlen) (p-last))
                                        (let (($dch (make-p-box nil)))
                                          (p-my-= $dch (p-substr $tmpl $ti 1))
                                          (p-post++ $ti)
                                          (let
                                            (($dbang (make-p-box nil))
                                              ($dbe2 (make-p-box nil))
                                              ($dle2 (make-p-box nil)))
                                            (let ((*wantarray* nil))
                                              (p-list-= (vector $dbang $dbe2 $dle2)
                                                (let ((*wantarray* t))
                                                  (pl-_pack_parse_mods $tmpl
                                                    (p-backslash $ti)
                                                    $be
                                                    $le
                                                    $dch
                                                    "unpack"))))
                                            (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                            (let
                                              (($dall (make-p-box nil))
                                                ($dcnt (make-p-box nil))
                                                ($dnrep (make-p-box nil)))
                                              (let ((*wantarray* nil))
                                                (p-list-= (vector $dall $dcnt $dnrep)
                                                  (let ((*wantarray* t))
                                                    (pl-_pack_parse_count $tmpl
                                                      (p-backslash $ti)))))
                                              (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                              (let
                                                (($chain
                                                    (%pcl-to-string-strict
                                                      (p-&& (p-< $ti $tlen)
                                                        (p-str-eq (p-substr $tmpl $ti 1) "/"))
                                                      "$chain")))
                                                (p-if $chain (progn (p-post++ $ti)))
                                                (let
                                                  (($dnb (make-p-box nil))
                                                    ($dsig (make-p-box nil))
                                                    ($ddbe (make-p-box nil)))
                                                  (let ((*wantarray* nil))
                                                    (p-list-= (vector $dnb $dsig $ddbe)
                                                      (let ((*wantarray* t))
                                                        (pl-_pack_type_info $dch $dbang))))
                                                  (p-if $chain
                                                    (progn
                                                      (p-if $dnb
                                                        (progn
                                                          (let (($dbe3 (make-p-box nil)))
                                                            (p-my-= $dbe3
                                                              (p-if $dbe2
                                                                1
                                                                (p-if $dle2 0 $ddbe)))
                                                            (p-if
                                                              (p-> (p-+ (p-cast-$ $si_ref) $dnb)
                                                                $slen)
                                                              (p-die :loc
                                                                "cl/pack-impl.pl line 973"
                                                                "length/code after end of string in unpack
"))
                                                            (p-my-= $slash_n
                                                              (let ((*wantarray* nil))
                                                                (pl-_unpack_read_int $s
                                                                  (p-cast-$ $si_ref)
                                                                  $dnb
                                                                  $dbe3
                                                                  $dsig)))
                                                            (p-incf (p-cast-$ $si_ref) $dnb)))
                                                        (p-if (p-str-eq $dch "w")
                                                          (progn (p-my-= $slash_n 0)
                                                            (let (($more (make-p-box nil)))
                                                              (p-my-= $more 1)
                                                              (p-while $more
                                                                (p-if
                                                                  (p->= (p-cast-$ $si_ref)
                                                                    $slen)
                                                                  (p-last))
                                                                (let (($b (make-p-box nil)))
                                                                  (p-my-= $b
                                                                    (p-ord
                                                                      (p-substr $s
                                                                        (p-post++
                                                                          (p-cast-$ $si_ref))
                                                                        1)))
                                                                  (p-my-= $more
                                                                    (p-bit-and $b #x80))
                                                                  (p-my-= $slash_n
                                                                    (p-bit-or (p-<< $slash_n 7)
                                                                      (p-bit-and $b #x7F)))))))
                                                          (progn
                                                            (let (($raw2 (make-p-box nil)))
                                                              (p-my-= $raw2
                                                                (p-if
                                                                  (p-< (p-cast-$ $si_ref)
                                                                    $slen)
                                                                  (p-substr $s
                                                                    (p-cast-$ $si_ref)
                                                                    $slash_n)
                                                                  ""))
                                                              (p-incf (p-cast-$ $si_ref)
                                                                $slash_n)
                                                              (p-if (p-str-eq $dch "A")
                                                                (p-=~ $raw2
                                                                  (p-subst "[ \\x00]+$" "")))
                                                              (p-if (p-str-eq $dch "Z")
                                                                (p-=~ $raw2
                                                                  (p-subst "\\x00.*" "" :s)))
                                                              (p-my-= $slash_n (p-+ $raw2 0)))))))
                                                    (progn
                                                      (p-if $dnb
                                                        (progn
                                                          (let (($dbe3 (make-p-box nil)))
                                                            (p-my-= $dbe3
                                                              (p-if $dbe2
                                                                1
                                                                (p-if $dle2 0 $ddbe)))
                                                            (let (($i__cond__11 0))
                                                              (p-for ()
                                                                ((p-&&
                                                                    (p-< $i__cond__11 $slash_n)
                                                                    (p-<=
                                                                      (p-+ (p-cast-$ $si_ref)
                                                                        $dnb)
                                                                      $slen)))
                                                                ((p-incf-raw $i__cond__11))
                                                                (p-funcall-ref $push_val
                                                                  (let ((*wantarray* t))
                                                                    (pl-_unpack_read_int $s
                                                                      (p-cast-$ $si_ref)
                                                                      $dnb
                                                                      $dbe3
                                                                      $dsig)))
                                                                (p-incf (p-cast-$ $si_ref)
                                                                  $dnb)))))
                                                        (p-if
                                                          (p-||
                                                            (p-||
                                                              (p-||
                                                                (p-||
                                                                  (p-||
                                                                    (p-||
                                                                      (p-||
                                                                        (p-||
                                                                          (p-str-eq $dch "A")
                                                                          (p-str-eq $dch "a"))
                                                                        (p-str-eq $dch "Z"))
                                                                      (p-str-eq $dch "B"))
                                                                    (p-str-eq $dch "b"))
                                                                  (p-str-eq $dch "H"))
                                                                (p-str-eq $dch "h"))
                                                              (p-str-eq $dch "u"))
                                                            (p-str-eq $dch "U"))
                                                          (progn
                                                            (pl-_unpack_str $dch
                                                              $slash_n
                                                              0
                                                              $s
                                                              $si_ref
                                                              $push_val
                                                              $checksum_p))
                                                          (p-if (p-str-eq $dch "(")
                                                            (progn
                                                              (let
                                                                (($ge
                                                                    (%pcl-to-number-strict
                                                                      (pl-_pack_find_group_end
                                                                        $tmpl
                                                                        $ti)
                                                                      "$ge")))
                                                                (let (($inner (make-p-box nil)))
                                                                  (p-my-= $inner
                                                                    (p-substr $tmpl
                                                                      $ti
                                                                      (p-- $ge $ti)))
                                                                  (p-my-= $ti (p-+ $ge 1))
                                                                  (let (($r__cond__22 0))
                                                                    (p-for ()
                                                                      ((p-< $r__cond__22
                                                                          $slash_n))
                                                                      ((p-incf-raw $r__cond__22))
                                                                      (let
                                                                        (($iter_base
                                                                            (make-p-box nil)))
                                                                        (p-my-= $iter_base
                                                                          (p-cast-$ $si_ref))
                                                                        (pl-_unpack_tmpl $inner
                                                                          $s
                                                                          $si_ref
                                                                          $push_val
                                                                          $be
                                                                          $le
                                                                          $checksum_p
                                                                          $iter_base
                                                                          (p-+ $depth 1)))))))))))
                                                      (p-last)))))))))
                                      (p-next)))))
                              (p-if (p-defined $grpbeg)
                                (progn
                                  (let (($inner (make-p-box nil)))
                                    (p-my-= $inner
                                      (p-substr $tmpl $grpbeg (p-- $grpend $grpbeg)))
                                    (let
                                      (($gti
                                          (%pcl-to-number-strict (pl-_pack_skip_ws $inner 0)
                                            "$gti")))
                                      (p-if (p-< $gti (p-length $inner))
                                        (progn
                                          (let (($fc (make-p-box nil)))
                                            (p-my-= $fc (p-substr $inner $gti 1))
                                            (p-if
                                              (let ((*wantarray* nil))
                                                (p-=~ $fc (p-regex "/^[\\d\\*\\[]/")))
                                              (p-die :loc
                                                "cl/pack-impl.pl line 1026"
                                                "()-group starts with a count in unpack
")))))
                                      (p-if $all
                                        (progn
                                          (p-while (p-< (p-cast-$ $si_ref) $slen)
                                            (let
                                              (($si_before
                                                  (%pcl-to-number-strict (p-cast-$ $si_ref)
                                                    "$si_before")))
                                              (let (($iter_base (make-p-box nil)))
                                                (p-my-= $iter_base (p-cast-$ $si_ref))
                                                (pl-_unpack_tmpl $inner
                                                  $s
                                                  $si_ref
                                                  $push_val
                                                  $be
                                                  $le
                                                  $checksum_p
                                                  $iter_base
                                                  (p-+ $depth 1))
                                                (p-if (p-== (p-cast-$ $si_ref) $si_before)
                                                  (p-last))))))
                                        (progn
                                          (let (($r__cond__23 0))
                                            (p-for ()
                                              ((p-< $r__cond__23 $nrep))
                                              ((p-incf-raw $r__cond__23))
                                              (let (($iter_base (make-p-box nil)))
                                                (p-my-= $iter_base (p-cast-$ $si_ref))
                                                (pl-_unpack_tmpl $inner
                                                  $s
                                                  $si_ref
                                                  $push_val
                                                  $be
                                                  $le
                                                  $checksum_p
                                                  $iter_base
                                                  (p-+ $depth 1)))))))
                                      (p-next)))))
                              (p-if (p-str-eq $ch "x")
                                (progn
                                  (p-if $bang
                                    (progn
                                      (let (($n (make-p-box nil)))
                                        (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                        (p-incf (p-cast-$ $si_ref)
                                          (p-% (p-- $n (p-% (p-cast-$ $si_ref) $n)) $n))))
                                    (p-if $all
                                      (progn (p-setf (p-cast-$ $si_ref) $slen))
                                      (progn (p-incf (p-cast-$ $si_ref) $nrep))))
                                  (p-next)))
                              (p-if (p-str-eq $ch "X")
                                (progn
                                  (p-if $bang
                                    (progn
                                      (let (($n (make-p-box nil)))
                                        (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                        (p-setf (p-cast-$ $si_ref)
                                          (p-* (p-int (p-/ (p-cast-$ $si_ref) $n)) $n))))
                                    (progn (p-decf (p-cast-$ $si_ref) $nrep)
                                      (p-if (p-< (p-cast-$ $si_ref) 0)
                                        (p-setf (p-cast-$ $si_ref) 0))))
                                  (p-next)))
                              (p-if (p-str-eq $ch "@")
                                (progn
                                  (let (($n (make-p-box nil)))
                                    (p-my-= $n (p-if (p-defined $count) $count 0))
                                    (p-setf (p-cast-$ $si_ref)
                                      (p-if $bang $n (p-+ $group_base $n)))
                                    (p-next))))
                              (p-if (p-|| (p-str-eq $ch "%") (p-str-eq $ch "!"))
                                (progn (p-next)))
                              (p-if
                                (p-|| (p-|| (p-str-eq $ch "p") (p-str-eq $ch "P"))
                                  (p-str-eq $ch "D"))
                                (progn
                                  (p-die :loc
                                    "cl/pack-impl.pl line 1069"
                                    (p-string-concat "Invalid type '"
                                      $ch
                                      "' in unpack
"))))
                              (p-if (p-str-eq $ch ".")
                                (progn
                                  (p-if $all
                                    (progn (p-funcall-ref $push_val (p-cast-$ $si_ref)))
                                    (p-if (p-&& (p-defined $count) (p-== $count 0))
                                      (progn (p-funcall-ref $push_val 0))
                                      (p-if (p-&& (p-defined $count) (p->= $count 2))
                                        (progn (p-funcall-ref $push_val (p-cast-$ $si_ref)))
                                        (progn
                                          (p-funcall-ref $push_val
                                            (p-- (p-cast-$ $si_ref) $group_base))))))
                                  (p-next)))
                              (let
                                (($nb (make-p-box nil))
                                  ($sig (make-p-box nil))
                                  ($dbe (make-p-box nil)))
                                (let ((*wantarray* nil))
                                  (p-list-= (vector $nb $sig $dbe)
                                    (let ((*wantarray* t)) (pl-_pack_type_info $ch $bang))))
                                (p-if $nb
                                  (progn
                                    (let (($be2 (make-p-box nil)))
                                      (p-my-= $be2 (p-if $be 1 (p-if $le 0 $dbe)))
                                      (let (($n (make-p-box nil)))
                                        (p-my-= $n
                                          (p-if $all
                                            (p-int (p-/ (p-- $slen (p-cast-$ $si_ref)) $nb))
                                            $nrep))
                                        (let (($i__cond__12 0))
                                          (p-for ()
                                            ((p-< $i__cond__12 $n))
                                            ((p-incf-raw $i__cond__12))
                                            (p-if (p-> (p-+ (p-cast-$ $si_ref) $nb) $slen)
                                              (p-last))
                                            (p-funcall-ref $push_val
                                              (let ((*wantarray* t))
                                                (pl-_unpack_read_int $s
                                                  (p-cast-$ $si_ref)
                                                  $nb
                                                  $be2
                                                  $sig)))
                                            (p-incf (p-cast-$ $si_ref) $nb)))
                                        (p-next)))))
                                (p-if (p-str-eq $ch "f")
                                  (progn
                                    (let (($be2 (make-p-box nil)))
                                      (p-my-= $be2 (p-if $be 1 (p-if $le 0 0)))
                                      (let (($n (make-p-box nil)))
                                        (p-my-= $n
                                          (p-if $all
                                            (p-int (p-/ (p-- $slen (p-cast-$ $si_ref)) 4))
                                            $nrep))
                                        (let (($i__cond__13 0))
                                          (p-for ()
                                            ((p-< $i__cond__13 $n))
                                            ((p-incf-raw $i__cond__13))
                                            (p-if (p-> (p-+ (p-cast-$ $si_ref) 4) $slen)
                                              (p-last))
                                            (p-funcall-ref $push_val
                                              (let ((*wantarray* t))
                                                (pl-_unpack_float32 $s (p-cast-$ $si_ref) $be2)))
                                            (p-incf (p-cast-$ $si_ref) 4)))
                                        (p-next)))))
                                (p-if (p-|| (p-str-eq $ch "d") (p-str-eq $ch "F"))
                                  (progn
                                    (let (($be2 (make-p-box nil)))
                                      (p-my-= $be2 (p-if $be 1 (p-if $le 0 0)))
                                      (let (($n (make-p-box nil)))
                                        (p-my-= $n
                                          (p-if $all
                                            (p-int (p-/ (p-- $slen (p-cast-$ $si_ref)) 8))
                                            $nrep))
                                        (let (($i__cond__14 0))
                                          (p-for ()
                                            ((p-< $i__cond__14 $n))
                                            ((p-incf-raw $i__cond__14))
                                            (p-if (p-> (p-+ (p-cast-$ $si_ref) 8) $slen)
                                              (p-last))
                                            (p-funcall-ref $push_val
                                              (let ((*wantarray* t))
                                                (pl-_unpack_float64 $s (p-cast-$ $si_ref) $be2)))
                                            (p-incf (p-cast-$ $si_ref) 8)))
                                        (p-next)))))
                                (p-if
                                  (p-||
                                    (p-||
                                      (p-||
                                        (p-||
                                          (p-||
                                            (p-||
                                              (p-||
                                                (p-||
                                                  (p-||
                                                    (p-|| (p-str-eq $ch "A")
                                                      (p-str-eq $ch "a"))
                                                    (p-str-eq $ch "Z"))
                                                  (p-str-eq $ch "H"))
                                                (p-str-eq $ch "h"))
                                              (p-str-eq $ch "B"))
                                            (p-str-eq $ch "b"))
                                          (p-str-eq $ch "u"))
                                        (p-str-eq $ch "U"))
                                      (p-str-eq $ch "W"))
                                    (p-str-eq $ch "w"))
                                  (progn
                                    (pl-_unpack_str $ch
                                      $nrep
                                      $all
                                      $s
                                      $si_ref
                                      $push_val
                                      $checksum_p)
                                    (p-next)))
                                (p-if (p-str-eq $ch "/")
                                  (p-die :loc
                                    "cl/pack-impl.pl line 1128"
                                    "'/' must follow a numeric type in unpack
"))
                                (p-die :loc
                                  "cl/pack-impl.pl line 1129"
                                  (p-string-concat "Invalid type '"
                                    $ch
                                    "' in unpack
"))))))))))))))))))

(p-sub pl-_next_format_item
  (&rest %_args)
  (p-args-body
    (block nil
      (let (($tmpl (make-p-box nil)))
        (let ((*wantarray* nil)) (p-list-= (vector $tmpl) @_))
        (let ((*wantarray* :void))
          (let (($tlen (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
            (let (($ti (make-p-box nil)))
              (p-my-= $ti 0)
              (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
              ;; return ('', '') if $ti >= $tlen
(p-if (p->= $ti $tlen) (p-return "" ""))
              (let (($ch (%pcl-to-string-strict (p-substr $tmpl $ti 1) "$ch")))
                (p-post++ $ti)
                (p-if (p-str-eq $ch "(")
                  (progn
                    (let
                      (($grpend
                          (%pcl-to-number-strict (pl-_pack_find_group_end $tmpl $ti) "$grpend")))
                      (p-my-= $ti (p-+ $grpend 1)))))
                (p-while
                  (p-&& (p-< $ti $tlen)
                    (let ((*wantarray* nil)) (p-=~ (p-substr $tmpl $ti 1) (p-regex "/[!<>]/"))))
                  (p-post++ $ti))
                (pl-_pack_parse_count $tmpl (p-backslash $ti))
                ;; return (substr($tmpl, 0, $ti), substr($tmpl, $ti))
(p-return (p-substr $tmpl 0 $ti) (p-substr $tmpl $ti))))))))))

(p-sub pl-p_unpack
  (&rest %_args)
  (p-args-body
    (block nil
      (let (($tmpl (make-p-box nil)) ($s (make-p-box nil)))
        (let ((*wantarray* nil)) (p-list-= (vector $tmpl $s) @_))
        (let ((*wantarray* :void))
          (p-if (p-! (p-defined $s)) (p-my-= $s ""))
          (p-=~ $tmpl (p-subst "\\A(?:[ \\t\\n\\r\\f,]|#[^\\n]*\\n?)*" ""))
          (let (($checksum_width (make-p-box nil)))
            (p-my-= $checksum_width 0)
            (p-if (p-=~ $tmpl (p-subst "^%(\\d*)" ""))
              (progn (p-my-= $checksum_width (p-if (p-length $1) (p-int $1) 16))))
            (let
              (($utf8_mode
                  (%pcl-to-string-strict (p-=~ $tmpl (p-subst "^U0" "")) "$utf8_mode")))
              (p-if $checksum_width
                (p-=~ $tmpl (p-subst "\\A(?:[ \\t\\n\\r\\f,]|#[^\\n]*\\n?)*" "")))
              (pl-_pack_check_brackets $tmpl)
              (p-if $utf8_mode
                (progn
                  (let (($bytes ""))
                    (p-foreach ($c (p-split (p-regex "//") $s))
                      (let (($code (make-p-box nil)))
                        (p-my-= $code (p-ord $c))
                        (p-if (p-< $code #x80)
                          (progn (p-.=-raw $bytes (p-chr $code)))
                          (p-if (p-< $code #x800)
                            (progn
                              (p-.=-raw $bytes
                                (p-. (p-chr (p-bit-or #xC0 (p->> $code 6)))
                                  (p-chr (p-bit-or #x80 (p-bit-and $code #x3F))))))
                            (p-if (p-< $code #x10000)
                              (progn
                                (p-.=-raw $bytes
                                  (p-.
                                    (p-. (p-chr (p-bit-or #xE0 (p->> $code 12)))
                                      (p-chr (p-bit-or #x80 (p-bit-and (p->> $code 6) #x3F))))
                                    (p-chr (p-bit-or #x80 (p-bit-and $code #x3F))))))
                              (progn
                                (p-.=-raw $bytes
                                  (p-.
                                    (p-.
                                      (p-. (p-chr (p-bit-or #xF0 (p->> $code 18)))
                                        (p-chr (p-bit-or #x80 (p-bit-and (p->> $code 12) #x3F))))
                                      (p-chr (p-bit-or #x80 (p-bit-and (p->> $code 6) #x3F))))
                                    (p-chr (p-bit-or #x80 (p-bit-and $code #x3F)))))))))))
                    (p-my-= $s $bytes))))
              (let ((@result (make-array 0 :adjustable t :fill-pointer 0)))
                (let (($si (make-p-box nil)))
                  (p-my-= $si 0)
                  (p-if $checksum_width
                    (progn
                      (let (($cs_tmpl (make-p-box nil)) ($rest_tmpl (make-p-box nil)))
                        (let ((*wantarray* nil))
                          (p-list-= (vector $cs_tmpl $rest_tmpl)
                            (let ((*wantarray* t)) (pl-_next_format_item $tmpl))))
                        (let (($checksum (make-p-box nil)))
                          (p-my-= $checksum 0)
                          (p-if (p-length $cs_tmpl)
                            (progn
                              (pl-_unpack_tmpl $cs_tmpl
                                $s
                                (p-backslash $si)
                                (lambda (&rest %_args)
                                  (let
                                    ((@_ (p-flatten-args %_args))
                                      (*pcl-caller-wantarray* *wantarray*))
                                    (catch :p-return
                                      (block nil (p-incf $checksum (p-aref @_ 0))))))
                                0
                                0
                                1)))
                          (let (($mod (p-** 2 $checksum_width)))
                            (let (($q (make-p-box nil)))
                              (p-my-= $q (p-int (p-/ $checksum $mod)))
                              (p-if (p-> (p-* $q $mod) $checksum) (p-post-- $q))
                              (p-push @result (p-- $checksum (p-* $q $mod)))
                              (p-if (p-length $rest_tmpl)
                                (progn
                                  (pl-_unpack_tmpl $rest_tmpl
                                    $s
                                    (p-backslash $si)
                                    (lambda (&rest %_args)
                                      (let
                                        ((@_ (p-flatten-args %_args))
                                          (*pcl-caller-wantarray* *wantarray*))
                                        (catch :p-return
                                          (block nil (p-push @result (p-aref @_ 0))))))
                                    0
                                    0
                                    0))))))))
                    (progn
                      (pl-_unpack_tmpl $tmpl
                        $s
                        (p-backslash $si)
                        (lambda (&rest %_args)
                          (let
                            ((@_ (p-flatten-args %_args)) (*pcl-caller-wantarray* *wantarray*))
                            (catch :p-return (block nil (p-push @result (p-aref @_ 0))))))
                        0
                        0
                        0)))
                  (p-return (p-if (p-wantarray) @result (p-aref @result 0))))))))))))

(p-run-compile-phase-blocks)

(p-scalar-= $CAN_ENDIAN "sSiIlLqQjJfFdDpP")

(p-scalar-= $CAN_SHRIEK "sSiIlLnNvVxX.@")

(p-scalar-= $MAX_GROUP_DEPTH 100)

(p-scalar-= $pcl_pack_comma_warned 0)

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

;;; pcl: pipeline=v2 gen=v2-14400
;;;; Copyright (c) 2025-2026 the PCL authors
;;;; This is free software; you can redistribute it and/or modify it under the
;;;; same terms as the Perl 5 programming language system itself.
;;;; SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

(in-package :pcl)
(p-defpackage :main)
(in-package :main)
(pcl::p-eval-always (pcl::p-detect-table "cl/pack-impl.pl"
 '(("main" "_next_format_item" 1172 "list-call")
   ("main" "_next_format_item" 1208 "list-call")
   ("main" "_pack_ber" 438 "list-call")
   ("main" "_pack_ber" 510 "list-call")
   ("main" "_pack_ber" 534 "list-call")
   ("main" "_pack_ber" 553 "list-call")
   ("main" "_pack_ber" 556 "list-call")
   ("main" "_pack_ber" 558 "list-call")
   ("main" "_pack_ber" 701 "list-call")
   ("main" "_pack_ber" 718 "list-call")
   ("main" "_pack_check_brackets" 1172 "list-call")
   ("main" "_pack_check_brackets" 1192 "list-call")
   ("main" "_pack_check_brackets" 743 "list-call")
   ("main" "_pack_check_brackets" 765 "list-call")
   ("main" "_pack_emit_int" 438 "list-call")
   ("main" "_pack_emit_int" 503 "list-call")
   ("main" "_pack_emit_int" 527 "list-call")
   ("main" "_pack_emit_int" 538 "list-call")
   ("main" "_pack_emit_int" 540 "list-call")
   ("main" "_pack_emit_int" 550 "list-call")
   ("main" "_pack_emit_int" 643 "list-call")
   ("main" "_pack_emit_int" 648 "list-call")
   ("main" "_pack_find_group_end" 1024 "list-call")
   ("main" "_pack_find_group_end" 1153 "list-call")
   ("main" "_pack_find_group_end" 1167 "list-call")
   ("main" "_pack_find_group_end" 161 "list-call")
   ("main" "_pack_find_group_end" 177 "list-call")
   ("main" "_pack_find_group_end" 438 "list-call")
   ("main" "_pack_find_group_end" 461 "list-call")
   ("main" "_pack_find_group_end" 807 "list-call")
   ("main" "_pack_find_group_end" 920 "list-call")
   ("main" "_pack_float32" 438 "list-call")
   ("main" "_pack_float32" 656 "list-call")
   ("main" "_pack_float32" 658 "list-call")
   ("main" "_pack_float64" 438 "list-call")
   ("main" "_pack_float64" 664 "list-call")
   ("main" "_pack_float64" 666 "list-call")
   ("main" "_pack_parse_count" 1153 "list-call")
   ("main" "_pack_parse_count" 1171 "list-call")
   ("main" "_pack_parse_count" 161 "list-call")
   ("main" "_pack_parse_count" 186 "list-call")
   ("main" "_pack_parse_count" 438 "list-call")
   ("main" "_pack_parse_count" 467 "list-call")
   ("main" "_pack_parse_count" 485 "list-call")
   ("main" "_pack_parse_count" 807 "list-call")
   ("main" "_pack_parse_count" 926 "list-call")
   ("main" "_pack_parse_count" 978 "list-call")
   ("main" "_pack_parse_mods" 438 "list-call")
   ("main" "_pack_parse_mods" 464 "list-call")
   ("main" "_pack_parse_mods" 482 "list-call")
   ("main" "_pack_parse_mods" 807 "list-call")
   ("main" "_pack_parse_mods" 923 "list-call")
   ("main" "_pack_parse_mods" 976 "list-call")
   ("main" "_pack_skip_ws" 1040 "list-call")
   ("main" "_pack_skip_ws" 1153 "list-call")
   ("main" "_pack_skip_ws" 1163 "list-call")
   ("main" "_pack_skip_ws" 161 "list-call")
   ("main" "_pack_skip_ws" 172 "list-call")
   ("main" "_pack_skip_ws" 438 "list-call")
   ("main" "_pack_skip_ws" 456 "list-call")
   ("main" "_pack_skip_ws" 469 "list-call")
   ("main" "_pack_skip_ws" 475 "list-call")
   ("main" "_pack_skip_ws" 483 "list-call")
   ("main" "_pack_skip_ws" 568 "list-call")
   ("main" "_pack_skip_ws" 807 "list-call")
   ("main" "_pack_skip_ws" 915 "list-call")
   ("main" "_pack_skip_ws" 928 "list-call")
   ("main" "_pack_skip_ws" 933 "list-call")
   ("main" "_pack_skip_ws" 974 "list-call")
   ("main" "_pack_skip_ws" 977 "list-call")
   ("main" "_pack_skip_ws" 979 "list-call")
   ("main" "_pack_str_one" 438 "list-call")
   ("main" "_pack_str_one" 505 "list-call")
   ("main" "_pack_str_one" 507 "list-call")
   ("main" "_pack_str_one" 513 "list-call")
   ("main" "_pack_str_one" 529 "list-call")
   ("main" "_pack_str_one" 531 "list-call")
   ("main" "_pack_str_one" 675 "list-call")
   ("main" "_pack_template_size" 161 "list-call")
   ("main" "_pack_template_size" 190 "list-call")
   ("main" "_pack_template_size" 221 "list-call")
   ("main" "_pack_template_size" 251 "list-call")
   ("main" "_pack_tmpl" 438 "list-call")
   ("main" "_pack_tmpl" 577 "list-call")
   ("main" "_pack_tmpl" 581 "list-call")
   ("main" "_pack_tmpl" 583 "list-call")
   ("main" "_pack_tmpl" 743 "list-call")
   ("main" "_pack_tmpl" 768 "list-call")
   ("main" "_pack_type_info" 1113 "list-call")
   ("main" "_pack_type_info" 161 "list-call")
   ("main" "_pack_type_info" 209 "list-call")
   ("main" "_pack_type_info" 438 "list-call")
   ("main" "_pack_type_info" 487 "list-call")
   ("main" "_pack_type_info" 501 "list-call")
   ("main" "_pack_type_info" 525 "list-call")
   ("main" "_pack_type_info" 548 "list-call")
   ("main" "_pack_type_info" 640 "list-call")
   ("main" "_pack_type_info" 807 "list-call")
   ("main" "_pack_type_info" 940 "list-call")
   ("main" "_pack_type_info" 985 "list-call")
   ("main" "_pack_utf8_char" 438 "list-call")
   ("main" "_pack_utf8_char" 681 "list-call")
   ("main" "_pack_utf8_char" 686 "list-call")
   ("main" "_unpack_float32" 1129 "list-call")
   ("main" "_unpack_float32" 1131 "list-call")
   ("main" "_unpack_float32" 807 "list-call")
   ("main" "_unpack_float64" 1138 "list-call")
   ("main" "_unpack_float64" 1140 "list-call")
   ("main" "_unpack_float64" 807 "list-call")
   ("main" "_unpack_read_int" 1015 "list-call")
   ("main" "_unpack_read_int" 1016 "list-call")
   ("main" "_unpack_read_int" 1117 "list-call")
   ("main" "_unpack_read_int" 1119 "list-call")
   ("main" "_unpack_read_int" 807 "list-call")
   ("main" "_unpack_read_int" 948 "list-call")
   ("main" "_unpack_read_int" 992 "list-call")
   ("main" "_unpack_str" 1022 "list-call")
   ("main" "_unpack_str" 1148 "list-call")
   ("main" "_unpack_str" 807 "list-call")
   ("main" "_unpack_tmpl" 1026 "list-call")
   ("main" "_unpack_tmpl" 1028 "list-call")
   ("main" "_unpack_tmpl" 1049 "list-call")
   ("main" "_unpack_tmpl" 1053 "list-call")
   ("main" "_unpack_tmpl" 1055 "list-call")
   ("main" "_unpack_tmpl" 1172 "list-call")
   ("main" "_unpack_tmpl" 1211 "list-call")
   ("main" "_unpack_tmpl" 1223 "list-call")
   ("main" "_unpack_tmpl" 1226 "list-call")
   ("main" "_unpack_tmpl" 807 "list-call")
   ("main" "_unpack_utf8_char" 793 "list-call")
   ("main" "_unpack_utf8_char" 882 "list-call")
   ("main" "chr" 1172 "builtin")
   ("main" "chr" 1197 "builtin")
   ("main" "chr" 1198 "builtin")
   ("main" "chr" 1199 "builtin")
   ("main" "chr" 1200 "builtin")
   ("main" "chr" 261 "builtin")
   ("main" "chr" 270 "builtin")
   ("main" "chr" 271 "builtin")
   ("main" "chr" 274 "builtin")
   ("main" "chr" 275 "builtin")
   ("main" "chr" 278 "builtin")
   ("main" "chr" 291 "builtin")
   ("main" "chr" 310 "builtin")
   ("main" "chr" 346 "builtin")
   ("main" "chr" 355 "builtin")
   ("main" "chr" 356 "builtin")
   ("main" "chr" 367 "builtin")
   ("main" "chr" 368 "builtin")
   ("main" "chr" 370 "builtin")
   ("main" "chr" 374 "builtin")
   ("main" "chr" 380 "builtin")
   ("main" "chr" 384 "builtin")
   ("main" "chr" 390 "builtin")
   ("main" "chr" 394 "builtin")
   ("main" "chr" 397 "builtin")
   ("main" "chr" 401 "builtin")
   ("main" "chr" 404 "builtin")
   ("main" "chr" 420 "builtin")
   ("main" "chr" 421 "builtin")
   ("main" "chr" 427 "builtin")
   ("main" "chr" 438 "builtin")
   ("main" "chr" 439 "builtin")
   ("main" "chr" 440 "builtin")
   ("main" "chr" 441 "builtin")
   ("main" "chr" 596 "builtin")
   ("main" "chr" 598 "builtin")
   ("main" "chr" 618 "builtin")
   ("main" "chr" 628 "builtin")
   ("main" "chr" 691 "builtin")
   ("main" "chr" 696 "builtin")
   ("main" "chr" 793 "builtin")
   ("main" "chr" 870 "builtin")
   ("main" "chr" 871 "builtin")
   ("main" "chr" 872 "builtin")
   ("main" "defined" 1038 "builtin")
   ("main" "defined" 1088 "builtin")
   ("main" "defined" 1105 "builtin")
   ("main" "defined" 1107 "builtin")
   ("main" "defined" 1172 "builtin")
   ("main" "defined" 1177 "builtin")
   ("main" "defined" 161 "builtin")
   ("main" "defined" 187 "builtin")
   ("main" "defined" 188 "builtin")
   ("main" "defined" 193 "builtin")
   ("main" "defined" 278 "builtin")
   ("main" "defined" 294 "builtin")
   ("main" "defined" 346 "builtin")
   ("main" "defined" 351 "builtin")
   ("main" "defined" 438 "builtin")
   ("main" "defined" 449 "builtin")
   ("main" "defined" 450 "builtin")
   ("main" "defined" 492 "builtin")
   ("main" "defined" 565 "builtin")
   ("main" "defined" 616 "builtin")
   ("main" "defined" 807 "builtin")
   ("main" "defined" 908 "builtin")
   ("main" "defined" 909 "builtin")
   ("main" "die" 1043 "builtin")
   ("main" "die" 1070 "builtin")
   ("main" "die" 1081 "builtin")
   ("main" "die" 1093 "builtin")
   ("main" "die" 1152 "builtin")
   ("main" "die" 1153 "builtin")
   ("main" "die" 127 "builtin")
   ("main" "die" 139 "builtin")
   ("main" "die" 144 "builtin")
   ("main" "die" 146 "builtin")
   ("main" "die" 147 "builtin")
   ("main" "die" 151 "builtin")
   ("main" "die" 153 "builtin")
   ("main" "die" 154 "builtin")
   ("main" "die" 221 "builtin")
   ("main" "die" 242 "builtin")
   ("main" "die" 249 "builtin")
   ("main" "die" 250 "builtin")
   ("main" "die" 438 "builtin")
   ("main" "die" 451 "builtin")
   ("main" "die" 478 "builtin")
   ("main" "die" 571 "builtin")
   ("main" "die" 633 "builtin")
   ("main" "die" 643 "builtin")
   ("main" "die" 646 "builtin")
   ("main" "die" 647 "builtin")
   ("main" "die" 681 "builtin")
   ("main" "die" 684 "builtin")
   ("main" "die" 685 "builtin")
   ("main" "die" 691 "builtin")
   ("main" "die" 694 "builtin")
   ("main" "die" 695 "builtin")
   ("main" "die" 701 "builtin")
   ("main" "die" 705 "builtin")
   ("main" "die" 706 "builtin")
   ("main" "die" 707 "builtin")
   ("main" "die" 708 "builtin")
   ("main" "die" 709 "builtin")
   ("main" "die" 715 "builtin")
   ("main" "die" 723 "builtin")
   ("main" "die" 724 "builtin")
   ("main" "die" 737 "builtin")
   ("main" "die" 741 "builtin")
   ("main" "die" 746 "builtin")
   ("main" "die" 793 "builtin")
   ("main" "die" 807 "builtin")
   ("main" "die" 894 "builtin")
   ("main" "die" 910 "builtin")
   ("main" "die" 934 "builtin")
   ("main" "die" 938 "builtin")
   ("main" "die" 946 "builtin")
   ("main" "die" 953 "builtin")
   ("main" "die" 990 "builtin")
   ("main" "hex" 346 "builtin")
   ("main" "hex" 394 "builtin")
   ("main" "hex" 395 "builtin")
   ("main" "hex" 396 "builtin")
   ("main" "hex" 401 "builtin")
   ("main" "hex" 402 "builtin")
   ("main" "hex" 403 "builtin")
   ("main" "if" 1117 "builtin")
   ("main" "if" 1129 "builtin")
   ("main" "if" 1138 "builtin")
   ("main" "if" 161 "builtin")
   ("main" "if" 278 "builtin")
   ("main" "if" 438 "builtin")
   ("main" "if" 793 "builtin")
   ("main" "if" 807 "builtin")
   ("main" "index" 127 "builtin")
   ("main" "index" 139 "builtin")
   ("main" "index" 144 "builtin")
   ("main" "index" 151 "builtin")
   ("main" "index" 221 "builtin")
   ("main" "index" 249 "builtin")
   ("main" "index" 807 "builtin")
   ("main" "index" 959 "builtin")
   ("main" "int" 1079 "builtin")
   ("main" "int" 1116 "builtin")
   ("main" "int" 1128 "builtin")
   ("main" "int" 1137 "builtin")
   ("main" "int" 1172 "builtin")
   ("main" "int" 1186 "builtin")
   ("main" "int" 1219 "builtin")
   ("main" "int" 161 "builtin")
   ("main" "int" 205 "builtin")
   ("main" "int" 212 "builtin")
   ("main" "int" 213 "builtin")
   ("main" "int" 261 "builtin")
   ("main" "int" 267 "builtin")
   ("main" "int" 278 "builtin")
   ("main" "int" 299 "builtin")
   ("main" "int" 438 "builtin")
   ("main" "int" 607 "builtin")
   ("main" "int" 625 "builtin")
   ("main" "int" 643 "builtin")
   ("main" "int" 648 "builtin")
   ("main" "int" 681 "builtin")
   ("main" "int" 686 "builtin")
   ("main" "int" 691 "builtin")
   ("main" "int" 696 "builtin")
   ("main" "int" 701 "builtin")
   ("main" "int" 709 "builtin")
   ("main" "int" 717 "builtin")
   ("main" "int" 793 "builtin")
   ("main" "int" 807 "builtin")
   ("main" "int" 811 "builtin")
   ("main" "int" 816 "builtin")
   ("main" "int" 821 "builtin")
   ("main" "int" 826 "builtin")
   ("main" "int" 831 "builtin")
   ("main" "int" 832 "builtin")
   ("main" "int" 837 "builtin")
   ("main" "int" 838 "builtin")
   ("main" "int" 843 "builtin")
   ("main" "int" 847 "builtin")
   ("main" "int" 848 "builtin")
   ("main" "int" 853 "builtin")
   ("main" "int" 854 "builtin")
   ("main" "int" 859 "builtin")
   ("main" "int" 866 "builtin")
   ("main" "join" 278 "builtin")
   ("main" "join" 310 "builtin")
   ("main" "last" 102 "builtin")
   ("main" "last" 1031 "builtin")
   ("main" "last" 1050 "builtin")
   ("main" "last" 1118 "builtin")
   ("main" "last" 1130 "builtin")
   ("main" "last" 1139 "builtin")
   ("main" "last" 158 "builtin")
   ("main" "last" 173 "builtin")
   ("main" "last" 304 "builtin")
   ("main" "last" 457 "builtin")
   ("main" "last" 475 "builtin")
   ("main" "last" 578 "builtin")
   ("main" "last" 865 "builtin")
   ("main" "last" 916 "builtin")
   ("main" "last" 945 "builtin")
   ("main" "last" 974 "builtin")
   ("main" "last" 998 "builtin")
   ("main" "length" 1041 "builtin")
   ("main" "length" 105 "builtin")
   ("main" "length" 111 "builtin")
   ("main" "length" 1153 "builtin")
   ("main" "length" 1161 "builtin")
   ("main" "length" 1172 "builtin")
   ("main" "length" 1186 "builtin")
   ("main" "length" 1210 "builtin")
   ("main" "length" 1222 "builtin")
   ("main" "length" 127 "builtin")
   ("main" "length" 133 "builtin")
   ("main" "length" 161 "builtin")
   ("main" "length" 170 "builtin")
   ("main" "length" 221 "builtin")
   ("main" "length" 227 "builtin")
   ("main" "length" 278 "builtin")
   ("main" "length" 300 "builtin")
   ("main" "length" 310 "builtin")
   ("main" "length" 316 "builtin")
   ("main" "length" 346 "builtin")
   ("main" "length" 352 "builtin")
   ("main" "length" 419 "builtin")
   ("main" "length" 438 "builtin")
   ("main" "length" 454 "builtin")
   ("main" "length" 493 "builtin")
   ("main" "length" 507 "builtin")
   ("main" "length" 531 "builtin")
   ("main" "length" 569 "builtin")
   ("main" "length" 576 "builtin")
   ("main" "length" 581 "builtin")
   ("main" "length" 582 "builtin")
   ("main" "length" 594 "builtin")
   ("main" "length" 606 "builtin")
   ("main" "length" 609 "builtin")
   ("main" "length" 618 "builtin")
   ("main" "length" 619 "builtin")
   ("main" "length" 627 "builtin")
   ("main" "length" 724 "builtin")
   ("main" "length" 731 "builtin")
   ("main" "length" 775 "builtin")
   ("main" "length" 78 "builtin")
   ("main" "length" 781 "builtin")
   ("main" "length" 793 "builtin")
   ("main" "length" 799 "builtin")
   ("main" "length" 807 "builtin")
   ("main" "length" 85 "builtin")
   ("main" "length" 911 "builtin")
   ("main" "length" 913 "builtin")
   ("main" "map" 278 "builtin")
   ("main" "map" 310 "builtin")
   ("main" "next" 1034 "builtin")
   ("main" "next" 1058 "builtin")
   ("main" "next" 1073 "builtin")
   ("main" "next" 1084 "builtin")
   ("main" "next" 1090 "builtin")
   ("main" "next" 1092 "builtin")
   ("main" "next" 1109 "builtin")
   ("main" "next" 1122 "builtin")
   ("main" "next" 1133 "builtin")
   ("main" "next" 1142 "builtin")
   ("main" "next" 1149 "builtin")
   ("main" "next" 191 "builtin")
   ("main" "next" 193 "builtin")
   ("main" "next" 200 "builtin")
   ("main" "next" 207 "builtin")
   ("main" "next" 210 "builtin")
   ("main" "next" 211 "builtin")
   ("main" "next" 212 "builtin")
   ("main" "next" 213 "builtin")
   ("main" "next" 214 "builtin")
   ("main" "next" 215 "builtin")
   ("main" "next" 216 "builtin")
   ("main" "next" 217 "builtin")
   ("main" "next" 218 "builtin")
   ("main" "next" 561 "builtin")
   ("main" "next" 586 "builtin")
   ("main" "next" 600 "builtin")
   ("main" "next" 612 "builtin")
   ("main" "next" 620 "builtin")
   ("main" "next" 630 "builtin")
   ("main" "next" 650 "builtin")
   ("main" "next" 660 "builtin")
   ("main" "next" 668 "builtin")
   ("main" "next" 676 "builtin")
   ("main" "next" 688 "builtin")
   ("main" "next" 698 "builtin")
   ("main" "next" 720 "builtin")
   ("main" "ord" 1172 "builtin")
   ("main" "ord" 1196 "builtin")
   ("main" "ord" 310 "builtin")
   ("main" "ord" 319 "builtin")
   ("main" "ord" 320 "builtin")
   ("main" "ord" 323 "builtin")
   ("main" "ord" 324 "builtin")
   ("main" "ord" 346 "builtin")
   ("main" "ord" 421 "builtin")
   ("main" "ord" 422 "builtin")
   ("main" "ord" 423 "builtin")
   ("main" "ord" 424 "builtin")
   ("main" "ord" 775 "builtin")
   ("main" "ord" 783 "builtin")
   ("main" "ord" 790 "builtin")
   ("main" "ord" 793 "builtin")
   ("main" "ord" 807 "builtin")
   ("main" "ord" 811 "builtin")
   ("main" "ord" 812 "builtin")
   ("main" "ord" 821 "builtin")
   ("main" "ord" 822 "builtin")
   ("main" "ord" 831 "builtin")
   ("main" "ord" 832 "builtin")
   ("main" "ord" 837 "builtin")
   ("main" "ord" 838 "builtin")
   ("main" "ord" 847 "builtin")
   ("main" "ord" 848 "builtin")
   ("main" "ord" 853 "builtin")
   ("main" "ord" 854 "builtin")
   ("main" "ord" 863 "builtin")
   ("main" "ord" 868 "builtin")
   ("main" "ord" 886 "builtin")
   ("main" "ord" 887 "builtin")
   ("main" "ord" 895 "builtin")
   ("main" "ord" 954 "builtin")
   ("main" "ord" 999 "builtin")
   ("main" "pop" 724 "builtin")
   ("main" "pop" 741 "builtin")
   ("main" "pop" 748 "builtin")
   ("main" "pop" 751 "builtin")
   ("main" "push" 1172 "builtin")
   ("main" "push" 1221 "builtin")
   ("main" "push" 1223 "builtin")
   ("main" "push" 1226 "builtin")
   ("main" "push" 724 "builtin")
   ("main" "push" 741 "builtin")
   ("main" "push" 743 "builtin")
   ("main" "push" 744 "builtin")
   ("main" "return" 1164 "builtin")
   ("main" "return" 1172 "builtin")
   ("main" "return" 161 "builtin")
   ("main" "return" 231 "builtin")
   ("main" "return" 247 "builtin")
   ("main" "return" 252 "builtin")
   ("main" "return" 259 "builtin")
   ("main" "return" 261 "builtin")
   ("main" "return" 62 "builtin")
   ("main" "return" 63 "builtin")
   ("main" "return" 64 "builtin")
   ("main" "return" 65 "builtin")
   ("main" "return" 66 "builtin")
   ("main" "return" 67 "builtin")
   ("main" "return" 68 "builtin")
   ("main" "return" 69 "builtin")
   ("main" "return" 70 "builtin")
   ("main" "return" 71 "builtin")
   ("main" "return" 72 "builtin")
   ("main" "return" 73 "builtin")
   ("main" "return" 74 "builtin")
   ("main" "return" 75 "builtin")
   ("main" "return" 76 "builtin")
   ("main" "return" 77 "builtin")
   ("main" "scalar" 438 "builtin")
   ("main" "scalar" 452 "builtin")
   ("main" "split" 1172 "builtin")
   ("main" "split" 1195 "builtin")
   ("main" "split" 278 "builtin")
   ("main" "split" 297 "builtin")
   ("main" "sprintf" 278 "builtin")
   ("main" "sprintf" 294 "builtin")
   ("main" "sprintf" 793 "builtin")
   ("main" "sprintf" 811 "builtin")
   ("main" "sprintf" 813 "builtin")
   ("main" "sprintf" 821 "builtin")
   ("main" "sprintf" 823 "builtin")
   ("main" "sub" 1172 "builtin")
   ("main" "sub" 1211 "builtin")
   ("main" "sub" 1223 "builtin")
   ("main" "sub" 1226 "builtin")
   ("main" "sub" 346 "builtin")
   ("main" "sub" 421 "builtin")
   ("main" "sub" 426 "builtin")
   ("main" "sub" 793 "builtin")
   ("main" "sub" 868 "builtin")
   ("main" "substr" 1004 "builtin")
   ("main" "substr" 1025 "builtin")
   ("main" "substr" 1039 "builtin")
   ("main" "substr" 1042 "builtin")
   ("main" "substr" 105 "builtin")
   ("main" "substr" 114 "builtin")
   ("main" "substr" 1153 "builtin")
   ("main" "substr" 1165 "builtin")
   ("main" "substr" 117 "builtin")
   ("main" "substr" 1170 "builtin")
   ("main" "substr" 1172 "builtin")
   ("main" "substr" 1180 "builtin")
   ("main" "substr" 127 "builtin")
   ("main" "substr" 137 "builtin")
   ("main" "substr" 161 "builtin")
   ("main" "substr" 174 "builtin")
   ("main" "substr" 181 "builtin")
   ("main" "substr" 182 "builtin")
   ("main" "substr" 189 "builtin")
   ("main" "substr" 221 "builtin")
   ("main" "substr" 230 "builtin")
   ("main" "substr" 233 "builtin")
   ("main" "substr" 238 "builtin")
   ("main" "substr" 244 "builtin")
   ("main" "substr" 254 "builtin")
   ("main" "substr" 256 "builtin")
   ("main" "substr" 257 "builtin")
   ("main" "substr" 310 "builtin")
   ("main" "substr" 319 "builtin")
   ("main" "substr" 320 "builtin")
   ("main" "substr" 323 "builtin")
   ("main" "substr" 324 "builtin")
   ("main" "substr" 346 "builtin")
   ("main" "substr" 355 "builtin")
   ("main" "substr" 356 "builtin")
   ("main" "substr" 360 "builtin")
   ("main" "substr" 361 "builtin")
   ("main" "substr" 367 "builtin")
   ("main" "substr" 368 "builtin")
   ("main" "substr" 374 "builtin")
   ("main" "substr" 376 "builtin")
   ("main" "substr" 378 "builtin")
   ("main" "substr" 384 "builtin")
   ("main" "substr" 386 "builtin")
   ("main" "substr" 388 "builtin")
   ("main" "substr" 394 "builtin")
   ("main" "substr" 395 "builtin")
   ("main" "substr" 396 "builtin")
   ("main" "substr" 401 "builtin")
   ("main" "substr" 402 "builtin")
   ("main" "substr" 403 "builtin")
   ("main" "substr" 418 "builtin")
   ("main" "substr" 421 "builtin")
   ("main" "substr" 422 "builtin")
   ("main" "substr" 423 "builtin")
   ("main" "substr" 424 "builtin")
   ("main" "substr" 438 "builtin")
   ("main" "substr" 458 "builtin")
   ("main" "substr" 473 "builtin")
   ("main" "substr" 477 "builtin")
   ("main" "substr" 481 "builtin")
   ("main" "substr" 566 "builtin")
   ("main" "substr" 570 "builtin")
   ("main" "substr" 607 "builtin")
   ("main" "substr" 610 "builtin")
   ("main" "substr" 619 "builtin")
   ("main" "substr" 629 "builtin")
   ("main" "substr" 724 "builtin")
   ("main" "substr" 732 "builtin")
   ("main" "substr" 733 "builtin")
   ("main" "substr" 741 "builtin")
   ("main" "substr" 742 "builtin")
   ("main" "substr" 743 "builtin")
   ("main" "substr" 772 "builtin")
   ("main" "substr" 775 "builtin")
   ("main" "substr" 78 "builtin")
   ("main" "substr" 783 "builtin")
   ("main" "substr" 790 "builtin")
   ("main" "substr" 793 "builtin")
   ("main" "substr" 803 "builtin")
   ("main" "substr" 807 "builtin")
   ("main" "substr" 811 "builtin")
   ("main" "substr" 812 "builtin")
   ("main" "substr" 815 "builtin")
   ("main" "substr" 821 "builtin")
   ("main" "substr" 822 "builtin")
   ("main" "substr" 825 "builtin")
   ("main" "substr" 831 "builtin")
   ("main" "substr" 832 "builtin")
   ("main" "substr" 837 "builtin")
   ("main" "substr" 838 "builtin")
   ("main" "substr" 847 "builtin")
   ("main" "substr" 848 "builtin")
   ("main" "substr" 853 "builtin")
   ("main" "substr" 854 "builtin")
   ("main" "substr" 863 "builtin")
   ("main" "substr" 868 "builtin")
   ("main" "substr" 87 "builtin")
   ("main" "substr" 875 "builtin")
   ("main" "substr" 886 "builtin")
   ("main" "substr" 887 "builtin")
   ("main" "substr" 895 "builtin")
   ("main" "substr" 917 "builtin")
   ("main" "substr" 931 "builtin")
   ("main" "substr" 937 "builtin")
   ("main" "substr" 954 "builtin")
   ("main" "substr" 961 "builtin")
   ("main" "substr" 967 "builtin")
   ("main" "substr" 975 "builtin")
   ("main" "substr" 982 "builtin")
   ("main" "substr" 99 "builtin")
   ("main" "substr" 999 "builtin")
   ("main" "undef" 161 "builtin")
   ("main" "undef" 175 "builtin")
   ("main" "undef" 221 "builtin")
   ("main" "undef" 231 "builtin")
   ("main" "undef" 438 "builtin")
   ("main" "undef" 459 "builtin")
   ("main" "undef" 701 "builtin")
   ("main" "undef" 718 "builtin")
   ("main" "undef" 807 "builtin")
   ("main" "undef" 918 "builtin")
   ("main" "unless" 724 "builtin")
   ("main" "unless" 807 "builtin")
   ("main" "unshift" 278 "builtin")
   ("main" "unshift" 303 "builtin")
   ("main" "unshift" 308 "builtin")
   ("main" "wantarray" 1172 "builtin")
   ("main" "wantarray" 1228 "builtin")
   ("main" "warn" 127 "builtin")
   ("main" "warn" 141 "builtin")
   ("main" "warn" 148 "builtin")
   ("main" "warn" 155 "builtin")
   ("main" "warn" 346 "builtin")
   ("main" "warn" 410 "builtin")
   ("main" "warn" 78 "builtin")
   ("main" "warn" 93 "builtin")
   ("utf8" "decode" 743 "unknown")
   ("utf8" "decode" 773 "unknown"))
 '(("main" "_pack_type_info" nil 60)
   ("main" "_pack_skip_ws" nil 83)
   ("main" "_pack_find_group_end" nil 109)
   ("main" "_pack_parse_mods" nil 131)
   ("main" "_pack_template_size" nil 166)
   ("main" "_pack_parse_count" nil 225)
   ("main" "_pack_emit_int" nil 265)
   ("main" "_pack_ber" nil 289)
   ("main" "_unpack_read_int" nil 314)
   ("main" "_pack_float32" nil 343)
   ("main" "_pack_float64" nil 344)
   ("main" "_unpack_float32" nil 345)
   ("main" "_unpack_float64" nil 346)
   ("main" "_pack_str_one" nil 349)
   ("main" "_pack_utf8_char" nil 436)
   ("main" "_pack_tmpl" nil 447)
   ("main" "_pack_check_brackets" nil 728)
   ("main" "p_pack" nil 762)
   ("main" "_unpack_utf8_char" nil 779)
   ("main" "_unpack_str" nil 797)
   ("main" "_unpack_tmpl" nil 906)
   ("main" "_next_format_item" nil 1159)
   ("main" "p_unpack" nil 1175))))

(pcl:p-defpackage :utf8)

(p-declare-sub pl-_pack_type_info :body)
(p-declare-sub pl-_pack_skip_ws :body)
(p-declare-sub pl-_pack_find_group_end :body)
(p-declare-sub pl-_pack_parse_mods :body)
(p-declare-sub pl-_pack_template_size :body)
(p-declare-sub pl-_pack_parse_count :body)
(p-declare-sub pl-_pack_emit_int :body)
(p-declare-sub pl-_pack_ber :body)
(p-declare-sub pl-_unpack_read_int :body)
(p-declare-sub pl-_pack_float32 :body)
(p-declare-sub pl-_pack_float64 :body)
(p-declare-sub pl-_unpack_float32 :body)
(p-declare-sub pl-_unpack_float64 :body)
(p-declare-sub pl-_pack_str_one :body)
(p-declare-sub pl-_pack_utf8_char :body)
(p-declare-sub pl-_pack_tmpl :body)
(p-declare-sub pl-_pack_check_brackets :body)
(p-declare-sub pl-p_pack :body)
(p-declare-sub pl-_unpack_utf8_char :body)
(p-declare-sub pl-_unpack_str :body)
(p-declare-sub pl-_unpack_tmpl :body)
(p-declare-sub pl-_next_format_item :body)
(p-declare-sub pl-p_unpack :body)
(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))

;; Forward declarations for undeclared package globals
(p-defcell $abs_tgt (make-p-box nil))
(p-defcell $actual_count (make-p-box nil))
(p-defcell $ai (make-p-box nil))
(p-defcell $ai_before (make-p-box nil))
(p-defcell $ai_ref (make-p-box nil))
(p-defcell $all (make-p-box nil))
(p-defcell $arg (make-p-box nil))
(p-defcell $args_ref (make-p-box nil))
(p-defcell $b0 (make-p-box nil))
(p-defcell $b1 (make-p-box nil))
(p-defcell $b2 (make-p-box nil))
(p-defcell $b__excl__0 (make-p-box nil) :perl "$b" :why :exception-global)
(p-defcell $b__excl__1 (make-p-box nil) :perl "$b" :why :exception-global)
(p-defcell $b__excl__2 (make-p-box nil) :perl "$b" :why :exception-global)
(p-defcell $b__excl__3 (make-p-box nil) :perl "$b" :why :exception-global)
(p-defcell $b__excl__4 (make-p-box nil) :perl "$b" :why :exception-global)
(p-defcell $b__excl__5 (make-p-box nil) :perl "$b" :why :exception-global)
(p-defcell $b__excl__6 (make-p-box nil) :perl "$b" :why :exception-global)
(p-defcell $b__excl__7 (make-p-box nil) :perl "$b" :why :exception-global)
(p-defcell $b__excl__8 (make-p-box nil) :perl "$b" :why :exception-global)
(p-defcell $bang (make-p-box nil))
(p-defcell $be (make-p-box nil))
(p-defcell $be2 (make-p-box nil))
(p-defcell $bit (make-p-box nil))
(p-defcell $bits (make-p-box nil))
(p-defcell $body (make-p-box nil))
(p-defcell $bs (make-p-box nil))
(p-defcell $byte (make-p-box nil))
(p-defcell $bytes (make-p-box nil))
(p-defcell $c (make-p-box nil))
(p-defcell $c0 (make-p-box nil))
(p-defcell $ce (make-p-box nil))
(p-defcell $ch (make-p-box nil))
(p-defcell $chain (make-p-box nil))
(p-defcell $checksum (make-p-box nil))
(p-defcell $checksum_p (make-p-box nil))
(p-defcell $checksum_width (make-p-box nil))
(p-defcell $chunk (make-p-box nil))
(p-defcell $clen (make-p-box nil))
(p-defcell $cm (make-p-box nil))
(p-defcell $code (make-p-box nil))
(p-defcell $count (make-p-box nil))
(p-defcell $cs (make-p-box nil))
(p-defcell $cs_tmpl (make-p-box nil))
(p-defcell $ctx (make-p-box nil))
(p-defcell $cur (make-p-box nil))
(p-defcell $d (make-p-box nil))
(p-defcell $dall (make-p-box nil))
(p-defcell $darg (make-p-box nil))
(p-defcell $dbang (make-p-box nil))
(p-defcell $dbe (make-p-box nil))
(p-defcell $dbe2 (make-p-box nil))
(p-defcell $dbe3 (make-p-box nil))
(p-defcell $dbe_eff (make-p-box nil))
(p-defcell $dch (make-p-box nil))
(p-defcell $dcnt (make-p-box nil))
(p-defcell $dcnt2 (make-p-box nil))
(p-defcell $ddbe (make-p-box nil))
(p-defcell $decoded (make-p-box nil))
(p-defcell $depth (make-p-box nil))
(p-defcell $dfmt (make-p-box nil))
(p-defcell $digits (make-p-box nil))
(p-defcell $dle2 (make-p-box nil))
(p-defcell $dlen (make-p-box nil))
(p-defcell $dnb (make-p-box nil))
(p-defcell $dnrep (make-p-box nil))
(p-defcell $dnrep2 (make-p-box nil))
(p-defcell $done (make-p-box nil))
(p-defcell $dsig (make-p-box nil))
(p-defcell $dstar2 (make-p-box nil))
(p-defcell $end (make-p-box nil))
(p-defcell $fc (make-p-box nil))
(p-defcell $fp (make-p-box nil))
(p-defcell $ge (make-p-box nil))
(p-defcell $get (make-p-box nil))
(p-defcell $got_bang (make-p-box nil))
(p-defcell $got_be (make-p-box nil))
(p-defcell $got_le (make-p-box nil))
(p-defcell $group_base (make-p-box nil))
(p-defcell $grpbeg (make-p-box nil))
(p-defcell $grpend (make-p-box nil))
(p-defcell $gti (make-p-box nil))
(p-defcell $had_count (make-p-box nil))
(p-defcell $had_dcount (make-p-box nil))
(p-defcell $hex (make-p-box nil))
(p-defcell $hi (make-p-box nil))
(p-defcell $i (make-p-box nil))
(p-defcell $idx (make-p-box nil))
(p-defcell $inh_be (make-p-box nil))
(p-defcell $inh_le (make-p-box nil))
(p-defcell $inner (make-p-box nil))
(p-defcell $iter_base (make-p-box nil))
(p-defcell $k (make-p-box nil))
(p-defcell $lc (make-p-box nil))
(p-defcell $le (make-p-box nil))
(p-defcell $lead_u (make-p-box nil))
(p-defcell $len (make-p-box nil))
(p-defcell $line_len (make-p-box nil))
(p-defcell $lo (make-p-box nil))
(p-defcell $m (make-p-box nil))
(p-defcell $max (make-p-box nil))
(p-defcell $mod (make-p-box nil))
(p-defcell $more (make-p-box nil))
(p-defcell $n (make-p-box nil))
(p-defcell $n_close (make-p-box nil))
(p-defcell $n_open (make-p-box nil))
(p-defcell $nargs (make-p-box nil))
(p-defcell $nb (make-p-box nil))
(p-defcell $nbits (make-p-box nil))
(p-defcell $nbytes (make-p-box nil))
(p-defcell $ng (make-p-box nil))
(p-defcell $nrep (make-p-box nil))
(p-defcell $nv (make-p-box nil))
(p-defcell $nyb (make-p-box nil))
(p-defcell $orig_s (make-p-box nil))
(p-defcell $out_base (make-p-box nil))
(p-defcell $pad (make-p-box nil))
(p-defcell $pcl_pack_comma_warned (make-p-box nil))
(p-defcell $pos (make-p-box nil))
(p-defcell $push_val (make-p-box nil))
(p-defcell $q (make-p-box nil))
(p-defcell $qd (make-p-box nil))
(p-defcell $r (make-p-box nil))
(p-defcell $raw (make-p-box nil))
(p-defcell $raw2 (make-p-box nil))
(p-defcell $remaining (make-p-box nil))
(p-defcell $rest_tmpl (make-p-box nil))
(p-defcell $result (make-p-box nil))
(p-defcell $result_ref (make-p-box nil))
(p-defcell $s (make-p-box nil))
(p-defcell $si (make-p-box nil))
(p-defcell $si_before (make-p-box nil))
(p-defcell $si_ref (make-p-box nil))
(p-defcell $sig (make-p-box nil))
(p-defcell $signed (make-p-box nil))
(p-defcell $slash_n (make-p-box nil))
(p-defcell $slen (make-p-box nil))
(p-defcell $star (make-p-box nil))
(p-defcell $start (make-p-box nil))
(p-defcell $t (make-p-box nil))
(p-defcell $tgt (make-p-box nil))
(p-defcell $ti (make-p-box nil))
(p-defcell $ti_before_count (make-p-box nil))
(p-defcell $ti_before_dcount (make-p-box nil))
(p-defcell $ti_ref (make-p-box nil))
(p-defcell $tlen (make-p-box nil))
(p-defcell $tmpl (make-p-box nil))
(p-defcell $utf8_mode (make-p-box nil))
(p-defcell $uu (make-p-box nil))
(p-defcell $v (make-p-box nil))
(p-defcell $val (make-p-box nil))
(p-defcell @args (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @bytes (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @result (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @stk (make-array 0 :adjustable t :fill-pointer 0))

(p-defcell $CAN_ENDIAN__file__3727705105595030000 (make-p-box nil) :perl "$CAN_ENDIAN" :why :captured)
(p-defcell $CAN_SHRIEK__file__3727705105595030001 (make-p-box nil) :perl "$CAN_SHRIEK" :why :captured)
(p-defcell $MAX_GROUP_DEPTH__file__3727705105595030002 (make-p-box nil) :perl "$MAX_GROUP_DEPTH" :why :captured)
(p-defcell $pcl_pack_comma_warned (make-p-box nil))

(p-eval-always (p-note-inc "strict"))

(p-eval-always (p-note-inc "warnings"))

(p-sub pl-_pack_type_info
  (&rest %_args)
  (:writes-args nil :needs (:nonlocal_exit.return))
  (p-raw-params (($ch :scalar) ($bang :scalar))
    (block nil
      (p-void-ctx
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
        (p-caller-ctx (p-tail-value (progn)))))))

(p-sub pl-_pack_skip_ws
  (&rest %_args)
  (:wantarray-insensitive t :writes-args nil :needs (:nonlocal_exit.loop_control))
  (p-args-body :copy (block nil
      (p-let (($s :box (make-p-box nil)) ($ti :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $s $ti) @_))
        (p-void-ctx
          (p-let (($tlen :num (%pcl-to-number-strict (p-length $s) "$tlen")))
            (p-while (p-< $ti $tlen)
              (p-let (($ch :str (%pcl-to-string-strict (p-substr $s $ti 1) "$ch")))
                (p-if
                  (p-||
                    (p-||
                      (p-|| (p-|| (p-str-eq $ch " ") (p-str-eq $ch (p-esc "\\t")))
                        (p-str-eq $ch (p-esc "\\n")))
                      (p-str-eq $ch (p-esc "\\r")))
                    (p-str-eq $ch (p-esc "\\u000C")))
                  (progn (p-post++ $ti))
                  (p-if (p-str-eq $ch ",")
                    (progn
                      (p-if (p-! $pcl_pack_comma_warned)
                        (progn
                          (p-warn :loc
                            "cl/pack-impl.pl line 93"
                            (p-esc "Invalid type ',' in pack\\n"))
                          (p-scalar-= $pcl_pack_comma_warned 1)))
                      (p-post++ $ti))
                    (p-if (p-str-eq $ch "#")
                      (progn (p-post++ $ti)
                        (p-while
                          (p-&& (p-< $ti $tlen) (p-str-ne (p-substr $s $ti 1) (p-esc "\\n")))
                          (p-post++ $ti))
                        (p-if (p-< $ti $tlen) (p-post++ $ti)))
                      (progn (p-last)))))))
            (p-caller-ctx (p-tail-value $ti))))))))

(p-sub pl-_pack_find_group_end
  (&rest %_args)
  (:wantarray-insensitive t :writes-args nil :needs ())
  (p-args-body :copy (block nil
      (p-let (($s :box (make-p-box nil)) ($ti :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $s $ti) @_))
        (p-void-ctx
          (p-let (($tlen :num (%pcl-to-number-strict (p-length $s) "$tlen")))
            (p-let (($depth :scalar 1))
              (p-while (p-&& (p-< $ti $tlen) (p-> $depth 0))
                (p-let (($ch :str (%pcl-to-string-strict (p-substr $s $ti 1) "$ch")))
                  (p-if (p-str-eq $ch "#")
                    (progn (p-post++ $ti)
                      (p-while
                        (p-&& (p-< $ti $tlen) (p-str-ne (p-substr $s $ti 1) (p-esc "\\n")))
                        (p-post++ $ti)))
                    (p-if (p-str-eq $ch "(")
                      (progn (p-incf-raw $depth :numeric) (p-post++ $ti))
                      (p-if (p-str-eq $ch ")")
                        (progn (p-decf-raw $depth :numeric)
                          (p-if (p-> $depth 0) (p-post++ $ti)))
                        (progn (p-post++ $ti)))))))
              (p-caller-ctx (p-tail-value $ti)))))))))

(p-sub pl-_pack_parse_mods
  (&rest %_args)
  (:writes-args nil
    :captures ($CAN_ENDIAN__file__3727705105595030000 $CAN_SHRIEK__file__3727705105595030001)
    :needs (:nonlocal_exit.die :nonlocal_exit.loop_control :nonlocal_exit.return))
  (p-args-body :copy (block nil
      (p-let
        (($tmpl :box (make-p-box nil))
          ($ti_ref :box (make-p-box nil))
          ($inh_be :box (make-p-box nil))
          ($inh_le :box (make-p-box nil))
          ($ch :box (make-p-box nil))
          ($ctx :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $tmpl $ti_ref $inh_be $inh_le $ch $ctx) @_))
        (p-void-ctx
          (p-let (($tlen :num (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
            (p-let
              (($bang :box (make-p-box nil))
                ($be :box (make-p-box nil))
                ($le :box (make-p-box nil)))
              (p-scalar-ctx (p-list-= (vector $bang $be $le) (vector 0 $inh_be $inh_le)))
              (p-let (($got_be :scalar 0))
                (p-let (($got_le :scalar 0))
                  (p-let (($got_bang :scalar 0))
                    (p-while (p-< (p-cast-$ $ti_ref :strict) $tlen)
                      (p-let
                        (($m :str
                            (%pcl-to-string-strict
                              (p-substr $tmpl (p-cast-$ $ti_ref :strict-lv) 1)
                              "$m")))
                        (p-if (p-str-eq $m "!")
                          (progn
                            (p-if
                              (p-! (p->= (p-index $CAN_SHRIEK__file__3727705105595030001 $ch) 0))
                              (p-die :loc
                                "cl/pack-impl.pl line 139"
                                (p-string-concat "'!' allowed only after types "
                                  $CAN_SHRIEK__file__3727705105595030001
                                  " in "
                                  $ctx
                                  (p-esc "\\n"))))
                            (p-if $got_bang
                              (p-warn :loc
                                "cl/pack-impl.pl line 141"
                                (p-string-concat "Duplicate modifier '!' after '"
                                  $ch
                                  "' in "
                                  $ctx
                                  (p-esc "\\n"))))
                            (p-my-= $bang 1)
                            (setf $got_bang 1)
                            (p-post++ (p-cast-$ $ti_ref)))
                          (p-if (p-str-eq $m ">")
                            (progn
                              (p-if
                                (p-!
                                  (p-||
                                    (p->= (p-index $CAN_ENDIAN__file__3727705105595030000 $ch)
                                      0)
                                    (p-str-eq $ch "(")))
                                (p-die :loc
                                  "cl/pack-impl.pl line 144"
                                  (p-string-concat "'>' allowed only after types "
                                    $CAN_ENDIAN__file__3727705105595030000
                                    " in "
                                    $ctx
                                    (p-esc "\\n"))))
                              (p-if $got_le
                                (p-die :loc
                                  "cl/pack-impl.pl line 146"
                                  (p-string-concat "Can't use both '<' and '>' after type '"
                                    $ch
                                    "' in "
                                    $ctx
                                    (p-esc "\\n"))))
                              (p-if $inh_le
                                (p-die :loc
                                  "cl/pack-impl.pl line 147"
                                  (p-string-concat
                                    "Can't use '>' in a group with different byte-order in "
                                    $ctx
                                    (p-esc "\\n"))))
                              (p-if $got_be
                                (p-warn :loc
                                  "cl/pack-impl.pl line 148"
                                  (p-string-concat "Duplicate modifier '>' after '"
                                    $ch
                                    "' in "
                                    $ctx
                                    (p-esc "\\n"))))
                              (p-my-= $be 1)
                              (p-my-= $le 0)
                              (setf $got_be 1)
                              (p-post++ (p-cast-$ $ti_ref)))
                            (p-if (p-str-eq $m "<")
                              (progn
                                (p-if
                                  (p-!
                                    (p-||
                                      (p->= (p-index $CAN_ENDIAN__file__3727705105595030000 $ch)
                                        0)
                                      (p-str-eq $ch "(")))
                                  (p-die :loc
                                    "cl/pack-impl.pl line 151"
                                    (p-string-concat "'<' allowed only after types "
                                      $CAN_ENDIAN__file__3727705105595030000
                                      " in "
                                      $ctx
                                      (p-esc "\\n"))))
                                (p-if $got_be
                                  (p-die :loc
                                    "cl/pack-impl.pl line 153"
                                    (p-string-concat "Can't use both '<' and '>' after type '"
                                      $ch
                                      "' in "
                                      $ctx
                                      (p-esc "\\n"))))
                                (p-if $inh_be
                                  (p-die :loc
                                    "cl/pack-impl.pl line 154"
                                    (p-string-concat
                                      "Can't use '<' in a group with different byte-order in "
                                      $ctx
                                      (p-esc "\\n"))))
                                (p-if $got_le
                                  (p-warn :loc
                                    "cl/pack-impl.pl line 155"
                                    (p-string-concat "Duplicate modifier '<' after '"
                                      $ch
                                      "' in "
                                      $ctx
                                      (p-esc "\\n"))))
                                (p-my-= $le 1)
                                (p-my-= $be 0)
                                (setf $got_le 1)
                                (p-post++ (p-cast-$ $ti_ref)))
                              (progn (p-last)))))))
                    ;; return ($bang, $be, $le)
(p-return $bang $be $le)))))))))))

(p-sub pl-_pack_template_size
  (&rest %_args)
  (:wantarray-insensitive t
    :writes-args nil
    :needs (:nonlocal_exit.loop_control :regex.literal :regex.native))
  (p-args-body :copy (block nil
      (p-let (($tmpl :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $tmpl) @_))
        (p-void-ctx
          (p-let (($pos :box (make-p-box nil)))
            (p-my-= $pos 0)
            (p-let (($ti :box (make-p-box nil)))
              (p-my-= $ti 0)
              (p-let (($tlen :num (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
                (p-while 1
                  (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                  (p-if (p->= $ti $tlen) (p-last))
                  (p-let (($ch :box (make-p-box nil)))
                    (p-my-= $ch (p-substr $tmpl $ti 1))
                    (p-post++ $ti)
                    (p-let (($grpbeg :box (make-p-box nil)))
                      (p-my-= $grpbeg (p-undef))
                      (p-let (($grpend :num (%pcl-to-number-strict (p-undef) "$grpend")))
                        (p-if (p-str-eq $ch "(")
                          (progn
                            (setf $grpend
                              (%pcl-to-number-strict (pl-_pack_find_group_end $tmpl $ti)
                                "$grpend"))
                            (p-my-= $grpbeg $ti)
                            (p-my-= $ti (p-+ $grpend 1))))
                        (p-let (($bang :box (make-p-box nil)))
                          (p-my-= $bang 0)
                          (p-while
                            (p-&& (p-< $ti $tlen)
                              (p-scalar-ctx
                                (p-=~ (p-substr $tmpl $ti 1)
                                  (p-regex :pat "[!<>]" :flags "" :tier :native))))
                            (p-if (p-str-eq (p-substr $tmpl $ti 1) "!") (p-my-= $bang 1))
                            (p-post++ $ti))
                          (p-let
                            (($all :box (make-p-box nil))
                              ($count :box (make-p-box nil))
                              ($nrep :box (make-p-box nil)))
                            (p-scalar-ctx
                              (p-list-= (vector $all $count $nrep)
                                (p-list-ctx (pl-_pack_parse_count $tmpl (p-backslash $ti)))))
                            (p-if (p-! (p-&& (p-defined $nrep) (p->= $nrep 1)))
                              (p-my-= $nrep 1))
                            (p-if (p-defined $grpbeg)
                              (progn
                                (p-let (($inner :box (make-p-box nil)))
                                  (p-my-= $inner
                                    (p-substr $tmpl $grpbeg (p-- $grpend $grpbeg)))
                                  (p-incf $pos (p-* (pl-_pack_template_size $inner) $nrep))
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
                                    (p-let (($n :box (make-p-box nil)))
                                      (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                      (p-incf $pos (p-% (p-- $n (p-% $pos $n)) $n))))
                                  (progn (p-incf $pos $nrep)))
                                (p-next)))
                            (p-if (p-str-eq $ch "X")
                              (progn
                                (p-if $bang
                                  (progn
                                    (p-let (($n :box (make-p-box nil)))
                                      (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                      (p-my-= $pos (p-* (p-int (p-/ $pos $n)) $n))))
                                  (progn (p-decf $pos $nrep)
                                    (p-if (p-< $pos 0) (p-my-= $pos 0))))
                                (p-next)))
                            (p-let (($nb :box (make-p-box nil)))
                              (p-scalar-ctx
                                (p-list-= (vector $nb)
                                  (p-list-ctx (pl-_pack_type_info $ch $bang))))
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
                              (p-if (p-str-eq $ch ".") (progn (p-next))))))))))
                (p-caller-ctx (p-tail-value $pos))))))))))

(p-sub pl-_pack_parse_count
  (&rest %_args)
  (:writes-args nil
    :needs (:nonlocal_exit.die :nonlocal_exit.return :regex.literal :regex.native))
  (p-args-body :copy (block nil
      (p-let (($tmpl :box (make-p-box nil)) ($ti_ref :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $tmpl $ti_ref) @_))
        (p-void-ctx
          (p-let (($tlen :num (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
            (p-if
              (p-&& (p-< (p-cast-$ $ti_ref :strict) $tlen)
                (p-str-eq (p-substr $tmpl (p-cast-$ $ti_ref :strict-lv) 1) "*"))
              (progn (p-post++ (p-cast-$ $ti_ref))
                ;; return (1, undef, 1)
(p-return 1 (p-undef) 1)))
            (p-if
              (p-&& (p-< (p-cast-$ $ti_ref :strict) $tlen)
                (p-str-eq (p-substr $tmpl (p-cast-$ $ti_ref :strict-lv) 1) "["))
              (progn (p-post++ (p-cast-$ $ti_ref))
                (p-let
                  (($start :num (%pcl-to-number-strict (p-cast-$ $ti_ref :strict) "$start")))
                  (p-let (($depth :scalar 1))
                    (p-while (p-&& (p-< (p-cast-$ $ti_ref :strict) $tlen) (p-> $depth 0))
                      (p-let
                        (($c :str
                            (%pcl-to-string-strict
                              (p-substr $tmpl (p-cast-$ $ti_ref :strict-lv) 1)
                              "$c")))
                        (p-post++ (p-cast-$ $ti_ref))
                        (p-if (p-str-eq $c "[")
                          (progn (p-incf-raw $depth :numeric))
                          (p-if (p-str-eq $c "]") (progn (p-decf-raw $depth :numeric))))))
                    (p-if (p-> $depth 0)
                      (p-die :loc
                        "cl/pack-impl.pl line 242"
                        (p-esc "No group ending character ']' found in template\\n")))
                    (p-let (($inner :box (make-p-box nil)))
                      (p-my-= $inner
                        (p-substr $tmpl $start (p-- (p-- (p-cast-$ $ti_ref :strict) $start) 1)))
                      (p-if
                        (p-scalar-ctx
                          (p-=~ $inner (p-regex :pat "^\\d+$" :flags "" :tier :native)))
                        (progn
                          (p-let (($n :box (make-p-box nil)))
                            (p-my-= $n (p-+ $inner 0))
                            ;; return (0, $n, $n)
(p-return 0 $n $n))))
                      (p-if (p->= (p-index $inner "@") 0)
                        (p-die :loc
                          "cl/pack-impl.pl line 249"
                          (p-esc "Within []-length '@' not allowed\\n")))
                      (p-if
                        (p-&&
                          (p-scalar-ctx
                            (p-=~ $inner (p-regex :pat "^\\d" :flags "" :tier :native)))
                          (p-scalar-ctx
                            (p-!~ $inner (p-regex :pat "^\\d+$" :flags "" :tier :native))))
                        (p-die :loc
                          "cl/pack-impl.pl line 250"
                          (p-esc "Malformed integer in []\\n")))
                      (p-let (($n :box (make-p-box nil)))
                        (p-my-= $n (pl-_pack_template_size $inner))
                        ;; return (0, $n, $n)
(p-return 0 $n $n)))))))
            (p-if
              (p-&& (p-< (p-cast-$ $ti_ref :strict) $tlen)
                (p-scalar-ctx
                  (p-=~ (p-substr $tmpl (p-cast-$ $ti_ref :strict-lv) 1)
                    (p-regex :pat "\\d" :flags "" :tier :native))))
              (progn
                (p-let (($n :box (make-p-box nil)))
                  (p-my-= $n 0)
                  (p-while
                    (p-&& (p-< (p-cast-$ $ti_ref :strict) $tlen)
                      (p-scalar-ctx
                        (p-=~ (p-substr $tmpl (p-cast-$ $ti_ref :strict-lv) 1)
                          (p-regex :pat "\\d" :flags "" :tier :native))))
                    (p-my-= $n
                      (p-+ (p-* $n 10) (p-substr $tmpl (p-cast-$ $ti_ref :strict-lv) 1)))
                    (p-post++ (p-cast-$ $ti_ref)))
                  ;; return (0, $n, $n)
(p-return 0 $n $n))))
            ;; return (0, 1, 1)
(p-return 0 1 1)))))))

(p-sub pl-_pack_emit_int
  (&rest %_args)
  (:wantarray-insensitive t :writes-args nil :needs ())
  (p-args-body :copy (block nil
      (p-let
        (($val :box (make-p-box nil))
          ($nbytes :box (make-p-box nil))
          ($signed :box (make-p-box nil))
          ($be :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $val $nbytes $signed $be) @_))
        (p-void-ctx (p-my-= $val (p-int (p-+ $val 0)))
          (p-let (($result :scalar ""))
            (p-if $be
              (progn
                (p-let (($k :scalar (p-- $nbytes 1)))
                  (p-for ()
                    ((p->= $k 0))
                    ((p-decf-raw $k))
                    (p-.=-raw $result (p-chr (p-bit-and (p->> $val (p-* 8 $k)) #xFF))))))
              (progn
                (p-let (($k :scalar 0))
                  (p-for ()
                    ((p-< $k $nbytes))
                    ((p-incf-raw $k :numeric))
                    (p-.=-raw $result (p-chr (p-bit-and (p->> $val (p-* 8 $k)) #xFF)))))))
            (p-caller-ctx (p-tail-value $result))))))))

(p-sub pl-_pack_ber
  (&rest %_args)
  (:writes-args nil
    :needs (:nonlocal_exit.loop_control
      :nonlocal_exit.return
      :regex.literal
      :regex.split
      :regex.native))
  (p-args-body :copy (block nil
      (p-let (($v :box (make-p-box nil)) ($digits :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $v $digits) @_))
        (p-void-ctx (p-if (p-== $v 0) (p-return (p-chr 0)))
          (p-let ((@bytes :array (make-array 0 :adjustable t :fill-pointer 0)))
            (p-if (p-> $v 18446744073709551615)
              (progn (p-if (p-! (p-defined $digits)) (p-my-= $digits (p-sprintf "%.0f" $v)))
                (p-while 1
                  (p-let (($q :box (make-p-box nil)))
                    (p-my-= $q "")
                    (p-let (($r :scalar 0))
                      (p-foreach-raw
                        ($d (p-split (p-regex :pat "" :flags "" :tier :native) $digits))
                        :my
                        t
                        (setf $r (p-+ (p-* $r 10) $d))
                        (p-let (($qd :box (make-p-box nil)))
                          (p-my-= $qd (p-int (p-/ $r 128)))
                          (p-if (p-|| (p-length $q) $qd) (p-.= $q $qd))
                          (p-decf-raw $r (p-* $qd 128))))
                      (p-unshift @bytes $r)
                      (p-if (p-str-eq $q "") (p-last))
                      (p-my-= $digits $q)))))
              (progn (p-while (p-> $v 0) (p-unshift @bytes (p-bit-and $v #x7F)) (p->>= $v 7))))
            (p-caller-ctx
              (p-tail-value
                (p-.
                  (p-join ""
                    (p-map (lambda ($_) (p-chr (p-bit-or $_ #x80)))
                      (p-aslice @bytes (p-.. 0 (p-- (p-array-last-index @bytes) 1)))))
                  (p-chr (p-aref @bytes -1)))))))))))

(p-sub pl-_unpack_read_int
  (&rest %_args)
  (:wantarray-insensitive t :writes-args nil :needs ())
  (p-raw-params (($s :scalar) ($si :scalar) ($nbytes :scalar) ($be :scalar) ($signed :scalar))
    (block nil
      (p-void-ctx
        (p-let (($slen :num (%pcl-to-number-strict (p-length $s) "$slen")))
          (p-let (($v :box (make-p-box nil)))
            (p-my-= $v 0)
            (p-if $be
              (progn
                (p-let (($k :scalar 0))
                  (p-for ()
                    ((p-< $k $nbytes))
                    ((p-incf-raw $k :numeric))
                    (p-my-= $v
                      (p-bit-or (p-<< $v 8)
                        (p-if (p-< (p-+ $si $k) $slen) (p-ord (p-substr $s (p-+ $si $k) 1)) 0))))))
              (progn
                (p-let (($k :scalar (p-- $nbytes 1)))
                  (p-for ()
                    ((p->= $k 0))
                    ((p-decf-raw $k))
                    (p-my-= $v
                      (p-bit-or (p-<< $v 8)
                        (p-if (p-< (p-+ $si $k) $slen) (p-ord (p-substr $s (p-+ $si $k) 1)) 0)))))))
            (p-if $signed
              (progn
                (p-let (($max :scalar 1))
                  (p-foreach-range-raw ($_ 1 $nbytes) (p-*=-raw $max 256 :numeric))
                  (p-if (p->= $v (p-/ $max 2)) (p-decf $v $max)))))
            (p-caller-ctx (p-tail-value $v))))))))

(p-sub pl-_pack_float32
  (&rest %_args)
  (:returns :str :wantarray-insensitive t :writes-args nil :needs ())
  (p-raw-params (($val :scalar) ($be :scalar)) (block nil (p-tail-value ""))))

(p-sub pl-_pack_float64
  (&rest %_args)
  (:returns :str :wantarray-insensitive t :writes-args nil :needs ())
  (p-raw-params (($val :scalar) ($be :scalar)) (block nil (p-tail-value ""))))

(p-sub pl-_unpack_float32
  (&rest %_args)
  (:returns :num :wantarray-insensitive t :writes-args nil :needs ())
  (p-raw-params (($s :scalar) ($si :scalar) ($be :scalar)) (block nil (p-tail-value 0.0))))

(p-sub pl-_unpack_float64
  (&rest %_args)
  (:returns :num :wantarray-insensitive t :writes-args nil :needs ())
  (p-raw-params (($s :scalar) ($si :scalar) ($be :scalar)) (block nil (p-tail-value 0.0))))

(p-sub pl-_pack_str_one
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body
    (block nil
      (p-let
        (($ch :box (make-p-box nil))
          ($arg :box (make-p-box nil))
          ($nrep :box (make-p-box nil))
          ($star :box (make-p-box nil))
          ($result_ref :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $ch $arg $nrep $star $result_ref) @_))
        (p-void-ctx (p-if (p-! (p-defined $arg)) (p-my-= $arg ""))
          (p-let (($slen :box (make-p-box nil)))
            (p-my-= $slen (p-length $arg))
            (let ((--pcl-if-ret--0 nil))
              (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "a"))
                (setf --pcl-if-ret--0
                  (progn
                    (p-let (($len :box (make-p-box nil)))
                      (p-my-= $len (p-if $star $slen $nrep))
                      (p-let (($k :scalar 0))
                        (p-for ()
                          ((p-< $k $len))
                          ((p-incf-raw $k :numeric))
                          (p-.= (p-cast-$ $result_ref :strict-lv)
                            (p-if (p-< $k $slen) (p-substr $arg $k 1) (p-chr 0))))))))
                (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "A"))
                  (setf --pcl-if-ret--0
                    (progn
                      (p-let (($len :box (make-p-box nil)))
                        (p-my-= $len (p-if $star $slen $nrep))
                        (p-let (($k :scalar 0))
                          (p-for ()
                            ((p-< $k $len))
                            ((p-incf-raw $k :numeric))
                            (p-.= (p-cast-$ $result_ref :strict-lv)
                              (p-if (p-< $k $slen) (p-substr $arg $k 1) " ")))))))
                  (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "Z"))
                    (setf --pcl-if-ret--0
                      (progn
                        (p-let (($len :box (make-p-box nil)))
                          (p-my-= $len (p-if $star (p-+ $slen 1) $nrep))
                          (let ((--pcl-if-ret--1 nil))
                            (p-if (setf --pcl-if-ret--1 (p-> $len 0))
                              (setf --pcl-if-ret--1
                                (progn
                                  (p-let (($body :scalar (p-- $len 1)))
                                    (p-let (($k :scalar 0))
                                      (p-for ()
                                        ((p-< $k $body))
                                        ((p-incf-raw $k :numeric))
                                        (p-.= (p-cast-$ $result_ref :strict-lv)
                                          (p-if (p-< $k $slen) (p-substr $arg $k 1) (p-chr 0)))))
                                    (p-caller-ctx
                                      (p-.= (p-cast-$ $result_ref :strict-lv) (p-chr 0))))))
                              nil)
                            --pcl-if-ret--1))))
                    (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "b"))
                      (setf --pcl-if-ret--0
                        (progn
                          (p-let (($nbits :box (make-p-box nil)))
                            (p-my-= $nbits (p-if $star $slen $nrep))
                            (p-let (($bs :scalar 0))
                              (p-for ()
                                ((p-< $bs $nbits))
                                ((p-incf-raw $bs 8 :numeric))
                                (p-let (($byte :box (make-p-box nil)))
                                  (p-my-= $byte 0)
                                  (p-let (($bit :scalar 0))
                                    (p-for ()
                                      ((p-&& (p-< $bit 8) (p-< (p-+ $bs $bit) $nbits)))
                                      ((p-incf-raw $bit :numeric))
                                      (p-let (($idx :box (make-p-box nil)))
                                        (p-my-= $idx (p-+ $bs $bit))
                                        (p-if
                                          (p-&& (p-< $idx $slen)
                                            (p-str-eq (p-substr $arg $idx 1) "1"))
                                          (p-bit-or= $byte (p-<< 1 $bit))))))
                                  (p-.= (p-cast-$ $result_ref :strict-lv) (p-chr $byte))))))))
                      (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "B"))
                        (setf --pcl-if-ret--0
                          (progn
                            (p-let (($nbits :box (make-p-box nil)))
                              (p-my-= $nbits (p-if $star $slen $nrep))
                              (p-let (($bs :scalar 0))
                                (p-for ()
                                  ((p-< $bs $nbits))
                                  ((p-incf-raw $bs 8 :numeric))
                                  (p-let (($byte :box (make-p-box nil)))
                                    (p-my-= $byte 0)
                                    (p-let (($bit :scalar 0))
                                      (p-for ()
                                        ((p-&& (p-< $bit 8) (p-< (p-+ $bs $bit) $nbits)))
                                        ((p-incf-raw $bit :numeric))
                                        (p-let (($idx :box (make-p-box nil)))
                                          (p-my-= $idx (p-+ $bs $bit))
                                          (p-if
                                            (p-&& (p-< $idx $slen)
                                              (p-str-eq (p-substr $arg $idx 1) "1"))
                                            (p-bit-or= $byte (p-<< 1 (p-- 7 $bit)))))))
                                    (p-.= (p-cast-$ $result_ref :strict-lv) (p-chr $byte))))))))
                        (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "H"))
                          (setf --pcl-if-ret--0
                            (progn
                              (p-let (($nyb :box (make-p-box nil)))
                                (p-my-= $nyb (p-if $star $slen $nrep))
                                (p-let (($k :scalar 0))
                                  (p-for ()
                                    ((p-< $k $nyb))
                                    ((p-incf-raw $k 2 :numeric))
                                    (p-let (($hi :box (make-p-box nil)))
                                      (p-my-= $hi
                                        (p-if (p-< $k $slen) (p-hex (p-substr $arg $k 1)) 0))
                                      (p-let (($lo :box (make-p-box nil)))
                                        (p-my-= $lo
                                          (p-if (p-< (p-+ $k 1) $slen)
                                            (p-hex (p-substr $arg (p-+ $k 1) 1))
                                            0))
                                        (p-.= (p-cast-$ $result_ref :strict-lv)
                                          (p-chr (p-bit-or (p-<< $hi 4) $lo))))))))))
                          (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "h"))
                            (setf --pcl-if-ret--0
                              (progn
                                (p-let (($nyb :box (make-p-box nil)))
                                  (p-my-= $nyb (p-if $star $slen $nrep))
                                  (p-let (($k :scalar 0))
                                    (p-for ()
                                      ((p-< $k $nyb))
                                      ((p-incf-raw $k 2 :numeric))
                                      (p-let (($lo :box (make-p-box nil)))
                                        (p-my-= $lo
                                          (p-if (p-< $k $slen) (p-hex (p-substr $arg $k 1)) 0))
                                        (p-let (($hi :box (make-p-box nil)))
                                          (p-my-= $hi
                                            (p-if (p-< (p-+ $k 1) $slen)
                                              (p-hex (p-substr $arg (p-+ $k 1) 1))
                                              0))
                                          (p-.= (p-cast-$ $result_ref :strict-lv)
                                            (p-chr (p-bit-or (p-<< $hi 4) $lo))))))))))
                            (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "u"))
                              (setf --pcl-if-ret--0
                                (progn
                                  (p-let
                                    (($line_len :num (%pcl-to-number-strict 45 "$line_len")))
                                    (p-if (p-&& (p-! $star) (p-> $nrep 45))
                                      (progn
                                        (p-if (p-> $nrep 63)
                                          (progn
                                            (p-warn :loc
                                              "cl/pack-impl.pl line 410"
                                              "Field too wide in 'u' format in pack")
                                            (setf $line_len
                                              (%pcl-to-number-strict 63 "$line_len")))
                                          (progn
                                            (setf $line_len
                                              (%pcl-to-number-strict $nrep "$line_len"))))))
                                    (p-let (($cs :scalar 0))
                                      (p-for ()
                                        ((p-< $cs $slen))
                                        ((p-incf-raw $cs $line_len))
                                        :dyn
                                        t
                                        (p-let
                                          (($ce :num
                                              (%pcl-to-number-strict
                                                (p-if (p-< (p-+ $cs $line_len) $slen)
                                                  (p-+ $cs $line_len)
                                                  $slen)
                                                "$ce")))
                                          (p-let (($chunk :box (make-p-box nil)))
                                            (p-my-= $chunk (p-substr $arg $cs (p-- $ce $cs)))
                                            (p-let
                                              (($clen :num
                                                  (%pcl-to-number-strict (p-length $chunk)
                                                    "$clen")))
                                              (p-.= (p-cast-$ $result_ref :strict-lv)
                                                (p-chr (p-+ 32 $clen)))
                                              (p-let (($k :scalar 0))
                                                (p-for ()
                                                  ((p-< $k $clen))
                                                  ((p-incf-raw $k 3 :numeric))
                                                  :dyn
                                                  t
                                                  (p-let
                                                    (($b0 :num
                                                        (%pcl-to-number-strict
                                                          (p-ord (p-substr $chunk $k 1))
                                                          "$b0")))
                                                    (p-let
                                                      (($b1 :num
                                                          (%pcl-to-number-strict
                                                            (p-if (p-< (p-+ $k 1) $clen)
                                                              (p-ord
                                                                (p-substr $chunk (p-+ $k 1) 1))
                                                              0)
                                                            "$b1")))
                                                      (p-let (($b2 :box (make-p-box nil)))
                                                        (p-my-= $b2
                                                          (p-if (p-< (p-+ $k 2) $clen)
                                                            (p-ord
                                                              (p-substr $chunk (p-+ $k 2) 1))
                                                            0))
                                                        (p-let (($cm :box (make-p-box nil)))
                                                          (p-my-= $cm
                                                            (p-bit-or
                                                              (p-bit-or (p-<< $b0 16)
                                                                (p-<< $b1 8))
                                                              $b2))
                                                          (p-let (($uu :box (make-p-box nil)))
                                                            (p-my-= $uu
                                                              (p-anon-sub "main"
                                                                (lambda (&rest %_args)
                                                                  (let
                                                                    ((@_
                                                                        (p-flatten-args %_args))
                                                                      (*pcl-current-package*
                                                                        "main")
                                                                      (*pcl-caller-wantarray*
                                                                        *wantarray*))
                                                                    (p-sub-frame
                                                                      (block nil
                                                                        (p-void-ctx
                                                                          (p-let
                                                                            (($c :scalar
                                                                                (p-+ 32
                                                                                  (p-bit-and
                                                                                    (p-aref @_
                                                                                      0)
                                                                                    63))))
                                                                            (p-caller-ctx
                                                                              (p-if (p-== $c 32)
                                                                                96
                                                                                $c))))))))))
                                                            (p-.=
                                                              (p-cast-$ $result_ref :strict-lv)
                                                              (p-.
                                                                (p-.
                                                                  (p-.
                                                                    (p-chr
                                                                      (p-scalar-ctx
                                                                        (p-funcall-ref $uu
                                                                          (p-bit-and
                                                                            (p->> $cm 18)
                                                                            63))))
                                                                    (p-chr
                                                                      (p-scalar-ctx
                                                                        (p-funcall-ref $uu
                                                                          (p-bit-and
                                                                            (p->> $cm 12)
                                                                            63)))))
                                                                  (p-chr
                                                                    (p-scalar-ctx
                                                                      (p-funcall-ref $uu
                                                                        (p-bit-and (p->> $cm 6)
                                                                          63)))))
                                                                (p-chr
                                                                  (p-scalar-ctx
                                                                    (p-funcall-ref $uu
                                                                      (p-bit-and $cm 63)))))))))))))
                                              (p-.= (p-cast-$ $result_ref :strict-lv)
                                                (p-esc "\\n"))))))))))
                              nil))))))))
              --pcl-if-ret--0)))))))

(p-sub pl-_pack_utf8_char
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body :copy (block nil
      (p-let (($code :box (make-p-box nil)) ($r :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $code $r) @_))
        (p-void-ctx
          (p-if (p-< $code #x80)
            (progn (p-caller-ctx (p-.= (p-cast-$ $r :strict-lv) (p-chr $code))))
            (p-if (p-< $code #x800)
              (progn
                (p-caller-ctx
                  (p-.= (p-cast-$ $r :strict-lv)
                    (p-. (p-chr (p-bit-or #xC0 (p->> $code 6)))
                      (p-chr (p-bit-or #x80 (p-bit-and $code #x3F)))))))
              (p-if (p-< $code #x10000)
                (progn
                  (p-caller-ctx
                    (p-.= (p-cast-$ $r :strict-lv)
                      (p-.
                        (p-. (p-chr (p-bit-or #xE0 (p->> $code 12)))
                          (p-chr (p-bit-or #x80 (p-bit-and (p->> $code 6) #x3F))))
                        (p-chr (p-bit-or #x80 (p-bit-and $code #x3F)))))))
                (progn
                  (p-caller-ctx
                    (p-.= (p-cast-$ $r :strict-lv)
                      (p-.
                        (p-.
                          (p-. (p-chr (p-bit-or #xF0 (p->> $code 18)))
                            (p-chr (p-bit-or #x80 (p-bit-and (p->> $code 12) #x3F))))
                          (p-chr (p-bit-or #x80 (p-bit-and (p->> $code 6) #x3F))))
                        (p-chr (p-bit-or #x80 (p-bit-and $code #x3F)))))))))))))))

(p-sub pl-_pack_tmpl
  (&rest %_args)
  (:writes-args nil
    :captures ($MAX_GROUP_DEPTH__file__3727705105595030002)
    :needs (:nonlocal_exit.die :nonlocal_exit.loop_control :regex.literal :regex.native))
  (p-args-body :copy (block nil
      (p-let
        (($tmpl :box (make-p-box nil))
          ($ai_ref :box (make-p-box nil))
          ($args_ref :box (make-p-box nil))
          ($result_ref :box (make-p-box nil))
          ($inh_be :box (make-p-box nil))
          ($inh_le :box (make-p-box nil))
          ($out_base :box (make-p-box nil))
          ($depth :box (make-p-box nil)))
        (p-scalar-ctx
          (p-list-=
            (vector $tmpl $ai_ref $args_ref $result_ref $inh_be $inh_le $out_base $depth)
            @_))
        (p-void-ctx (p-if (p-! (p-defined $out_base)) (p-my-= $out_base 0))
          (p-if (p-! (p-defined $depth)) (p-my-= $depth 0))
          (p-if (p-> $depth $MAX_GROUP_DEPTH__file__3727705105595030002)
            (p-die :loc
              "cl/pack-impl.pl line 451"
              (p-esc "Too deeply nested ()-groups in pack\\n")))
          (p-let
            (($nargs :num
                (%pcl-to-number-strict (p-scalar (p-cast-@ $args_ref :strict)) "$nargs")))
            (p-let (($ti :box (make-p-box nil)))
              (p-my-= $ti 0)
              (p-let (($tlen :num (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
                (p-while 1
                  (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                  (p-if (p->= $ti $tlen) (p-last))
                  (p-let (($ch :box (make-p-box nil)))
                    (p-my-= $ch (p-substr $tmpl $ti 1))
                    (p-post++ $ti)
                    (p-let (($grpbeg :box (make-p-box nil)))
                      (p-my-= $grpbeg (p-undef))
                      (p-let (($grpend :num (%pcl-to-number-strict (p-undef) "$grpend")))
                        (p-if (p-str-eq $ch "(")
                          (progn
                            (setf $grpend
                              (%pcl-to-number-strict (pl-_pack_find_group_end $tmpl $ti)
                                "$grpend"))
                            (p-my-= $grpbeg $ti)
                            (p-my-= $ti (p-+ $grpend 1))
                            (p-my-= $ch "(")))
                        (p-let
                          (($bang :box (make-p-box nil))
                            ($be :box (make-p-box nil))
                            ($le :box (make-p-box nil)))
                          (p-scalar-ctx
                            (p-list-= (vector $bang $be $le)
                              (p-list-ctx
                                (pl-_pack_parse_mods $tmpl
                                  (p-backslash $ti)
                                  $inh_be
                                  $inh_le
                                  $ch
                                  "pack"))))
                          (p-let
                            (($ti_before_count :num
                                (%pcl-to-number-strict $ti "$ti_before_count")))
                            (p-let
                              (($star :box (make-p-box nil))
                                ($count :box (make-p-box nil))
                                ($nrep :box (make-p-box nil)))
                              (p-scalar-ctx
                                (p-list-= (vector $star $count $nrep)
                                  (p-list-ctx (pl-_pack_parse_count $tmpl (p-backslash $ti)))))
                              (p-let (($had_count :box (make-p-box nil)))
                                (p-my-= $had_count (p-|| $star (p-> $ti $ti_before_count)))
                                (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                (p-if
                                  (p-&& (p-< $ti $tlen) (p-str-eq (p-substr $tmpl $ti 1) "/"))
                                  (progn (p-post++ $ti)
                                    (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                    (p-if (p->= $ti $tlen) (p-last))
                                    (let ((*package* *package*))
                                      (block nil
                                        (tagbody :redo
                                          (p-let (($c :box (make-p-box nil)))
                                            (p-my-= $c (p-substr $tmpl $ti 1))
                                            (p-if
                                              (p-|| (p-|| (p-str-eq $c "*") (p-str-eq $c "["))
                                                (p-scalar-ctx
                                                  (p-=~ $c
                                                    (p-regex :pat "\\d"
                                                      :flags ""
                                                      :tier :native))))
                                              (p-die :loc
                                                "cl/pack-impl.pl line 478"
                                                (p-esc
                                                  "'/' does not take a repeat count in pack\\n"))))
                                          :next)))
                                    (p-let (($dfmt :box (make-p-box nil)))
                                      (p-my-= $dfmt (p-substr $tmpl $ti 1))
                                      (p-post++ $ti)
                                      (p-let
                                        (($dbang :box (make-p-box nil))
                                          ($dbe2 :box (make-p-box nil))
                                          ($dle2 :box (make-p-box nil)))
                                        (p-scalar-ctx
                                          (p-list-= (vector $dbang $dbe2 $dle2)
                                            (p-list-ctx
                                              (pl-_pack_parse_mods $tmpl
                                                (p-backslash $ti)
                                                $be
                                                $le
                                                $dfmt
                                                "pack"))))
                                        (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                        (p-let
                                          (($ti_before_dcount :num
                                              (%pcl-to-number-strict $ti "$ti_before_dcount")))
                                          (p-let
                                            (($dstar2 :box (make-p-box nil))
                                              ($dcnt2 :box (make-p-box nil))
                                              ($dnrep2 :box (make-p-box nil)))
                                            (p-scalar-ctx
                                              (p-list-= (vector $dstar2 $dcnt2 $dnrep2)
                                                (p-list-ctx
                                                  (pl-_pack_parse_count $tmpl
                                                    (p-backslash $ti)))))
                                            (p-let
                                              (($had_dcount :str
                                                  (%pcl-to-string-strict
                                                    (p-|| $dstar2 (p-> $ti $ti_before_dcount))
                                                    "$had_dcount")))
                                              (p-let
                                                (($dnb :box (make-p-box nil))
                                                  ($dsig :box (make-p-box nil))
                                                  ($ddbe :box (make-p-box nil)))
                                                (p-scalar-ctx
                                                  (p-list-= (vector $dnb $dsig $ddbe)
                                                    (p-list-ctx
                                                      (pl-_pack_type_info $dfmt $dbang))))
                                                (p-let (($actual_count :box (make-p-box nil)))
                                                  (p-if
                                                    (p-||
                                                      (p-|| (p-str-eq $dfmt "a")
                                                        (p-str-eq $dfmt "A"))
                                                      (p-str-eq $dfmt "Z"))
                                                    (progn
                                                      (p-let (($darg :box (make-p-box nil)))
                                                        (p-my-= $darg
                                                          (p-if
                                                            (p-< (p-cast-$ $ai_ref :strict)
                                                              $nargs)
                                                            (p-aref-deref $args_ref
                                                              (p-post++ (p-cast-$ $ai_ref))
                                                              :strict-lv)
                                                            ""))
                                                        (p-if (p-! (p-defined $darg))
                                                          (p-my-= $darg ""))
                                                        (p-let (($dlen :box (make-p-box nil)))
                                                          (p-my-= $dlen (p-length $darg))
                                                          (p-if (p-|| (p-! $had_dcount) $dstar2)
                                                            (progn
                                                              (p-my-= $actual_count
                                                                (p-if (p-str-eq $dfmt "Z")
                                                                  (p-+ $dlen 1)
                                                                  $dlen)))
                                                            (progn
                                                              (p-my-= $actual_count $dnrep2)))
                                                          (p-let
                                                            (($nb :box (make-p-box nil))
                                                              ($sig :box (make-p-box nil))
                                                              ($dbe :box (make-p-box nil)))
                                                            (p-scalar-ctx
                                                              (p-list-= (vector $nb $sig $dbe)
                                                                (p-list-ctx
                                                                  (pl-_pack_type_info $ch
                                                                    $bang))))
                                                            (p-if $nb
                                                              (progn
                                                                (p-.=
                                                                  (p-cast-$ $result_ref
                                                                    :strict-lv)
                                                                  (pl-_pack_emit_int
                                                                    $actual_count
                                                                    $nb
                                                                    $sig
                                                                    (p-if $be
                                                                      1
                                                                      (vector (p-if $le 0 $dbe))))))
                                                              (p-if
                                                                (p-|| (p-str-eq $ch "A")
                                                                  (p-str-eq $ch "a"))
                                                                (progn
                                                                  (pl-_pack_str_one $ch
                                                                    (p-string-concat
                                                                      $actual_count)
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
                                                                      (p-let
                                                                        (($v :box
                                                                            (make-p-box nil)))
                                                                        (p-my-= $v
                                                                          $actual_count)
                                                                        (p-.=
                                                                          (p-cast-$ $result_ref
                                                                            :strict-lv)
                                                                          (p-scalar-ctx
                                                                            (pl-_pack_ber $v)))))))))
                                                            (pl-_pack_str_one $dfmt
                                                              $darg
                                                              $actual_count
                                                              0
                                                              $result_ref)))))
                                                    (p-if $dnb
                                                      (progn
                                                        (p-let
                                                          (($remaining :box (make-p-box nil)))
                                                          (p-my-= $remaining
                                                            (p-- $nargs
                                                              (p-cast-$ $ai_ref :strict)))
                                                          (p-if (p-|| (p-! $had_dcount) $dstar2)
                                                            (progn
                                                              (p-my-= $actual_count $remaining))
                                                            (progn
                                                              (p-my-= $actual_count
                                                                (p-if (p-< $dnrep2 $remaining)
                                                                  $dnrep2
                                                                  $remaining))))
                                                          (p-let
                                                            (($nb :box (make-p-box nil))
                                                              ($sig :box (make-p-box nil))
                                                              ($dbe :box (make-p-box nil)))
                                                            (p-scalar-ctx
                                                              (p-list-= (vector $nb $sig $dbe)
                                                                (p-list-ctx
                                                                  (pl-_pack_type_info $ch
                                                                    $bang))))
                                                            (p-if $nb
                                                              (progn
                                                                (p-.=
                                                                  (p-cast-$ $result_ref
                                                                    :strict-lv)
                                                                  (pl-_pack_emit_int
                                                                    $actual_count
                                                                    $nb
                                                                    $sig
                                                                    (p-if $be
                                                                      1
                                                                      (vector (p-if $le 0 $dbe))))))
                                                              (p-if
                                                                (p-|| (p-str-eq $ch "A")
                                                                  (p-str-eq $ch "a"))
                                                                (progn
                                                                  (pl-_pack_str_one $ch
                                                                    (p-string-concat
                                                                      $actual_count)
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
                                                                      (p-let
                                                                        (($v :box
                                                                            (make-p-box nil)))
                                                                        (p-my-= $v
                                                                          $actual_count)
                                                                        (p-.=
                                                                          (p-cast-$ $result_ref
                                                                            :strict-lv)
                                                                          (p-scalar-ctx
                                                                            (pl-_pack_ber $v)))))))))
                                                            (p-let
                                                              (($dbe_eff :box
                                                                  (make-p-box nil)))
                                                              (p-my-= $dbe_eff
                                                                (p-if $dbe2
                                                                  1
                                                                  (p-if $dle2 0 $ddbe)))
                                                              (p-let (($i :scalar 0))
                                                                (p-for ()
                                                                  ((p-&& (p-< $i $actual_count)
                                                                      (p-<
                                                                        (p-cast-$ $ai_ref
                                                                          :strict)
                                                                        $nargs)))
                                                                  ((p-incf-raw $i :numeric))
                                                                  (p-let
                                                                    (($val :box
                                                                        (make-p-box nil)))
                                                                    (p-my-= $val
                                                                      (p-aref-deref $args_ref
                                                                        (p-post++
                                                                          (p-cast-$ $ai_ref))
                                                                        :strict-lv))
                                                                    (p-.=
                                                                      (p-cast-$ $result_ref
                                                                        :strict-lv)
                                                                      (pl-_pack_emit_int $val
                                                                        $dnb
                                                                        $dsig
                                                                        $dbe_eff)))))))))
                                                      (p-if (p-str-eq $dfmt "w")
                                                        (progn
                                                          (p-let
                                                            (($remaining :box
                                                                (make-p-box nil)))
                                                            (p-my-= $remaining
                                                              (p-- $nargs
                                                                (p-cast-$ $ai_ref :strict)))
                                                            (p-my-= $actual_count
                                                              (p-if
                                                                (p-|| (p-! $had_dcount)
                                                                  $dstar2)
                                                                $remaining
                                                                (p-if (p-< $dnrep2 $remaining)
                                                                  $dnrep2
                                                                  $remaining)))
                                                            (p-let
                                                              (($nb :box (make-p-box nil))
                                                                ($sig :box (make-p-box nil))
                                                                ($dbe :box (make-p-box nil)))
                                                              (p-scalar-ctx
                                                                (p-list-= (vector $nb $sig $dbe)
                                                                  (p-list-ctx
                                                                    (pl-_pack_type_info $ch
                                                                      $bang))))
                                                              (p-if $nb
                                                                (progn
                                                                  (p-.=
                                                                    (p-cast-$ $result_ref
                                                                      :strict-lv)
                                                                    (pl-_pack_emit_int
                                                                      $actual_count
                                                                      $nb
                                                                      $sig
                                                                      (p-if $be
                                                                        1
                                                                        (vector
                                                                          (p-if $le 0 $dbe))))))
                                                                (p-if (p-str-eq $ch "w")
                                                                  (progn
                                                                    (p-let
                                                                      (($v :box
                                                                          (make-p-box nil)))
                                                                      (p-my-= $v $actual_count)
                                                                      (p-.=
                                                                        (p-cast-$ $result_ref
                                                                          :strict-lv)
                                                                        (p-scalar-ctx
                                                                          (pl-_pack_ber $v)))))))
                                                              (p-let (($i :scalar 0))
                                                                (p-for ()
                                                                  ((p-&& (p-< $i $actual_count)
                                                                      (p-<
                                                                        (p-cast-$ $ai_ref
                                                                          :strict)
                                                                        $nargs)))
                                                                  ((p-incf-raw $i :numeric))
                                                                  (p-let
                                                                    (($v :box
                                                                        (make-p-box nil)))
                                                                    (p-my-= $v
                                                                      (p-+
                                                                        (p-aref-deref $args_ref
                                                                          (p-post++
                                                                            (p-cast-$ $ai_ref))
                                                                          :strict-lv)
                                                                        0))
                                                                    (p-.=
                                                                      (p-cast-$ $result_ref
                                                                        :strict-lv)
                                                                      (p-scalar-ctx
                                                                        (pl-_pack_ber $v))))))))))))
                                                  (p-next))))))))))
                                (p-if (p-defined $grpbeg)
                                  (progn
                                    (p-let (($inner :box (make-p-box nil)))
                                      (p-my-= $inner
                                        (p-substr $tmpl $grpbeg (p-- $grpend $grpbeg)))
                                      (p-let
                                        (($gti :num
                                            (%pcl-to-number-strict (pl-_pack_skip_ws $inner 0)
                                              "$gti")))
                                        (p-if (p-< $gti (p-length $inner))
                                          (progn
                                            (p-let (($fc :box (make-p-box nil)))
                                              (p-my-= $fc (p-substr $inner $gti 1))
                                              (p-if
                                                (p-scalar-ctx
                                                  (p-=~ $fc
                                                    (p-regex :pat "^[\\d\\*\\[]"
                                                      :flags ""
                                                      :tier :native)))
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 571"
                                                  (p-esc
                                                    "()-group starts with a count in pack\\n"))))))
                                        (p-if $star
                                          (progn
                                            (p-while (p-< (p-cast-$ $ai_ref :strict) $nargs)
                                              (p-let
                                                (($ai_before :num
                                                    (%pcl-to-number-strict
                                                      (p-cast-$ $ai_ref :strict)
                                                      "$ai_before")))
                                                (p-let (($iter_base :box (make-p-box nil)))
                                                  (p-my-= $iter_base
                                                    (p-length (p-cast-$ $result_ref :strict-lv)))
                                                  (pl-_pack_tmpl $inner
                                                    $ai_ref
                                                    $args_ref
                                                    $result_ref
                                                    $be
                                                    $le
                                                    $iter_base
                                                    (p-+ $depth 1))
                                                  (p-if
                                                    (p-== (p-cast-$ $ai_ref :strict)
                                                      $ai_before)
                                                    (p-last))))))
                                          (progn
                                            (p-let (($r :scalar 0))
                                              (p-for ()
                                                ((p-< $r $nrep))
                                                ((p-incf-raw $r :numeric))
                                                (p-let (($iter_base :box (make-p-box nil)))
                                                  (p-my-= $iter_base
                                                    (p-length (p-cast-$ $result_ref :strict-lv)))
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
                                        (p-let (($n :box (make-p-box nil)))
                                          (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                          (p-let (($cur :box (make-p-box nil)))
                                            (p-my-= $cur
                                              (p-length (p-cast-$ $result_ref :strict-lv)))
                                            (p-let
                                              (($pad :scalar (p-% (p-- $n (p-% $cur $n)) $n)))
                                              (p-.= (p-cast-$ $result_ref :strict-lv)
                                                (p-str-x (p-chr 0) $pad))))))
                                      (progn
                                        (p-.= (p-cast-$ $result_ref :strict-lv)
                                          (p-str-x (p-chr 0) $nrep))))
                                    (p-next)))
                                (p-if (p-str-eq $ch "X")
                                  (progn
                                    (p-if $bang
                                      (progn
                                        (p-let (($n :box (make-p-box nil)))
                                          (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                          (p-let (($cur :box (make-p-box nil)))
                                            (p-my-= $cur
                                              (p-length (p-cast-$ $result_ref :strict-lv)))
                                            (p-setf (p-cast-$ $result_ref :strict-lv)
                                              (p-substr (p-cast-$ $result_ref :strict-lv)
                                                0
                                                (p-* (p-int (p-/ $cur $n)) $n))))))
                                      (progn
                                        (p-let
                                          (($fp :scalar
                                              (p-- (p-length (p-cast-$ $result_ref :strict-lv))
                                                $nrep)))
                                          (p-setf (p-cast-$ $result_ref :strict-lv)
                                            (p-substr (p-cast-$ $result_ref :strict-lv)
                                              0
                                              (p-if (p-< $fp 0) 0 $fp))))))
                                    (p-next)))
                                (p-if (p-str-eq $ch "@")
                                  (progn
                                    (p-let (($n :box (make-p-box nil)))
                                      (p-my-= $n (p-if (p-defined $count) $count 0))
                                      (p-let
                                        (($t :num
                                            (%pcl-to-number-strict
                                              (p-if $bang $n (p-+ $out_base $n))
                                              "$t")))
                                        (p-if
                                          (p-< (p-length (p-cast-$ $result_ref :strict-lv)) $t)
                                          (progn
                                            (p-.= (p-cast-$ $result_ref :strict-lv)
                                              (p-str-x (p-chr 0)
                                                (p-- $t
                                                  (p-length (p-cast-$ $result_ref :strict-lv))))))
                                          (p-if
                                            (p-> (p-length (p-cast-$ $result_ref :strict-lv))
                                              $t)
                                            (progn
                                              (p-setf (p-cast-$ $result_ref :strict-lv)
                                                (p-substr (p-cast-$ $result_ref :strict-lv)
                                                  0
                                                  $t)))))
                                        (p-next)))))
                                (p-if (p-str-eq $ch ".")
                                  (progn
                                    (p-let (($tgt :box (make-p-box nil)))
                                      (p-my-= $tgt
                                        (p-if (p-< (p-cast-$ $ai_ref :strict) $nargs)
                                          (p-int
                                            (p-+
                                              (p-//
                                                (p-aref-deref $args_ref
                                                  (p-post++ (p-cast-$ $ai_ref))
                                                  :strict-lv)
                                                0)
                                              0))
                                          0))
                                      (p-let
                                        (($abs_tgt :num
                                            (%pcl-to-number-strict
                                              (p-if $star $tgt (p-+ $out_base $tgt))
                                              "$abs_tgt")))
                                        (p-let (($cur :box (make-p-box nil)))
                                          (p-my-= $cur
                                            (p-length (p-cast-$ $result_ref :strict-lv)))
                                          (p-if (p-< $cur $abs_tgt)
                                            (progn
                                              (p-.= (p-cast-$ $result_ref :strict-lv)
                                                (p-str-x (p-chr 0) (p-- $abs_tgt $cur))))
                                            (p-if (p-> $cur $abs_tgt)
                                              (progn
                                                (p-setf (p-cast-$ $result_ref :strict-lv)
                                                  (p-substr (p-cast-$ $result_ref :strict-lv)
                                                    0
                                                    $abs_tgt)))))
                                          (p-next))))))
                                (p-if
                                  (p-|| (p-|| (p-str-eq $ch "p") (p-str-eq $ch "P"))
                                    (p-str-eq $ch "D"))
                                  (progn
                                    (p-die :loc
                                      "cl/pack-impl.pl line 633"
                                      (p-string-concat "Invalid type '"
                                        $ch
                                        (p-esc "' in pack\\n")))))
                                (p-if $star
                                  (p-my-= $nrep (p-- $nargs (p-cast-$ $ai_ref :strict))))
                                (p-let
                                  (($nb :box (make-p-box nil))
                                    ($sig :box (make-p-box nil))
                                    ($dbe :box (make-p-box nil)))
                                  (p-scalar-ctx
                                    (p-list-= (vector $nb $sig $dbe)
                                      (p-list-ctx (pl-_pack_type_info $ch $bang))))
                                  (p-if $nb
                                    (progn
                                      (p-let (($be2 :box (make-p-box nil)))
                                        (p-my-= $be2 (p-if $be 1 (p-if $le 0 $dbe)))
                                        (p-let (($r :scalar 0))
                                          (p-for ()
                                            ((p-< $r $nrep))
                                            ((p-incf-raw $r :numeric))
                                            (p-let (($v :box (make-p-box nil)))
                                              (p-my-= $v
                                                (p-if (p-< (p-cast-$ $ai_ref :strict) $nargs)
                                                  (p-//
                                                    (p-aref-deref $args_ref
                                                      (p-post++ (p-cast-$ $ai_ref))
                                                      :strict-lv)
                                                    0)
                                                  0))
                                              (p-let (($nv :box (make-p-box nil)))
                                                (p-my-= $nv (p-+ $v 0))
                                                (p-if (p-!= $nv $nv)
                                                  (p-die :loc
                                                    "cl/pack-impl.pl line 646"
                                                    (p-esc "Cannot pack NaN in pack\\n")))
                                                (p-if (p-&& (p-!= $nv 0) (p-== $nv (p-* $nv 2)))
                                                  (p-die :loc
                                                    "cl/pack-impl.pl line 647"
                                                    (p-.
                                                      (p-. "Cannot pack "
                                                        (p-if (p-< $nv 0) "-Inf" "Inf"))
                                                      (p-esc " in pack\\n"))))
                                                (p-.= (p-cast-$ $result_ref :strict-lv)
                                                  (pl-_pack_emit_int (p-int $nv) $nb $sig $be2))))))
                                        (p-next))))
                                  (p-if (p-str-eq $ch "f")
                                    (progn
                                      (p-let (($be2 :box (make-p-box nil)))
                                        (p-my-= $be2 (p-if $be 1 (p-if $le 0 0)))
                                        (p-let (($r :scalar 0))
                                          (p-for ()
                                            ((p-< $r $nrep))
                                            ((p-incf-raw $r :numeric))
                                            (p-let (($v :box (make-p-box nil)))
                                              (p-my-= $v
                                                (p-if (p-< (p-cast-$ $ai_ref :strict) $nargs)
                                                  (p-//
                                                    (p-aref-deref $args_ref
                                                      (p-post++ (p-cast-$ $ai_ref))
                                                      :strict-lv)
                                                    0)
                                                  0))
                                              (p-.= (p-cast-$ $result_ref :strict-lv)
                                                (pl-_pack_float32 $v $be2)))))
                                        (p-next))))
                                  (p-if (p-|| (p-str-eq $ch "d") (p-str-eq $ch "F"))
                                    (progn
                                      (p-let (($be2 :box (make-p-box nil)))
                                        (p-my-= $be2 (p-if $be 1 (p-if $le 0 0)))
                                        (p-let (($r :scalar 0))
                                          (p-for ()
                                            ((p-< $r $nrep))
                                            ((p-incf-raw $r :numeric))
                                            (p-let (($v :box (make-p-box nil)))
                                              (p-my-= $v
                                                (p-if (p-< (p-cast-$ $ai_ref :strict) $nargs)
                                                  (p-//
                                                    (p-aref-deref $args_ref
                                                      (p-post++ (p-cast-$ $ai_ref))
                                                      :strict-lv)
                                                    0)
                                                  0))
                                              (p-.= (p-cast-$ $result_ref :strict-lv)
                                                (pl-_pack_float64 $v $be2)))))
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
                                      (p-let (($arg :box (make-p-box nil)))
                                        (p-my-= $arg
                                          (p-if (p-< (p-cast-$ $ai_ref :strict) $nargs)
                                            (p-//
                                              (p-aref-deref $args_ref
                                                (p-post++ (p-cast-$ $ai_ref))
                                                :strict-lv)
                                              "")
                                            ""))
                                        (pl-_pack_str_one $ch $arg $nrep $star $result_ref)
                                        (p-next))))
                                  (p-if (p-str-eq $ch "U")
                                    (progn
                                      (p-let (($r :scalar 0))
                                        (p-for ()
                                          ((p-< $r $nrep))
                                          ((p-incf-raw $r :numeric))
                                          (p-let (($v :box (make-p-box nil)))
                                            (p-my-= $v
                                              (p-if (p-< (p-cast-$ $ai_ref :strict) $nargs)
                                                (p-//
                                                  (p-aref-deref $args_ref
                                                    (p-post++ (p-cast-$ $ai_ref))
                                                    :strict-lv)
                                                  0)
                                                0))
                                            (p-let (($nv :box (make-p-box nil)))
                                              (p-my-= $nv (p-+ $v 0))
                                              (p-if (p-!= $nv $nv)
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 684"
                                                  (p-esc "Cannot pack NaN in pack\\n")))
                                              (p-if (p-&& (p-!= $nv 0) (p-== $nv (p-* $nv 2)))
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 685"
                                                  (p-.
                                                    (p-. "Cannot pack "
                                                      (p-if (p-< $nv 0) "-Inf" "Inf"))
                                                    (p-esc " in pack\\n"))))
                                              (pl-_pack_utf8_char (p-int $nv) $result_ref)))))
                                      (p-next)))
                                  (p-if (p-str-eq $ch "W")
                                    (progn
                                      (p-let (($r :scalar 0))
                                        (p-for ()
                                          ((p-< $r $nrep))
                                          ((p-incf-raw $r :numeric))
                                          (p-let (($v :box (make-p-box nil)))
                                            (p-my-= $v
                                              (p-if (p-< (p-cast-$ $ai_ref :strict) $nargs)
                                                (p-//
                                                  (p-aref-deref $args_ref
                                                    (p-post++ (p-cast-$ $ai_ref))
                                                    :strict-lv)
                                                  0)
                                                0))
                                            (p-let (($nv :box (make-p-box nil)))
                                              (p-my-= $nv (p-+ $v 0))
                                              (p-if (p-!= $nv $nv)
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 694"
                                                  (p-esc "Cannot pack NaN in pack\\n")))
                                              (p-if (p-&& (p-!= $nv 0) (p-== $nv (p-* $nv 2)))
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 695"
                                                  (p-.
                                                    (p-. "Cannot pack "
                                                      (p-if (p-< $nv 0) "-Inf" "Inf"))
                                                    (p-esc " in pack\\n"))))
                                              (p-.= (p-cast-$ $result_ref :strict-lv)
                                                (p-chr (p-int $nv)))))))
                                      (p-next)))
                                  (p-if (p-str-eq $ch "w")
                                    (progn
                                      (p-let (($r :scalar 0))
                                        (p-for ()
                                          ((p-< $r $nrep))
                                          ((p-incf-raw $r :numeric))
                                          (p-let (($raw :box (make-p-box nil)))
                                            (p-my-= $raw
                                              (p-if (p-< (p-cast-$ $ai_ref :strict) $nargs)
                                                (p-//
                                                  (p-aref-deref $args_ref
                                                    (p-post++ (p-cast-$ $ai_ref))
                                                    :strict-lv)
                                                  0)
                                                0))
                                            (p-let (($orig_s :box (make-p-box nil)))
                                              (p-my-= $orig_s (p-string-concat $raw))
                                              (p-let (($v :box (make-p-box nil)))
                                                (p-my-= $v (p-+ $raw 0))
                                                (p-if (p-!= $v $v)
                                                  (p-die :loc
                                                    "cl/pack-impl.pl line 705"
                                                    (p-esc "Cannot compress NaN in pack\\n")))
                                                (p-if (p-&& (p-< $v 0) (p-== $v (p-* $v 2)))
                                                  (p-die :loc
                                                    "cl/pack-impl.pl line 706"
                                                    (p-esc "Cannot compress -Inf in pack\\n")))
                                                (p-if (p-< $v 0)
                                                  (p-die :loc
                                                    "cl/pack-impl.pl line 707"
                                                    (p-esc
                                                      "Cannot compress negative numbers in pack\\n")))
                                                (p-if (p-&& (p-!= $v 0) (p-== $v (p-* $v 2)))
                                                  (p-die :loc
                                                    "cl/pack-impl.pl line 708"
                                                    (p-esc "Cannot compress Inf in pack\\n")))
                                                (p-if (p-!= $v (p-int $v))
                                                  (p-die :loc
                                                    "cl/pack-impl.pl line 709"
                                                    (p-esc
                                                      "Can only compress unsigned integers in pack\\n")))
                                                (p-if
                                                  (p-&&
                                                    (p-scalar-ctx
                                                      (p-=~ $orig_s
                                                        (p-regex :pat "[eE]"
                                                          :flags ""
                                                          :tier :native)))
                                                    (p->= $v (p-** 2 64)))
                                                  (p-die :loc
                                                    "cl/pack-impl.pl line 715"
                                                    (p-esc
                                                      "Can only compress unsigned integers in pack\\n")))
                                                (p-my-= $v (p-int $v))
                                                (p-.= (p-cast-$ $result_ref :strict-lv)
                                                  (p-scalar-ctx
                                                    (pl-_pack_ber $v
                                                      (p-if
                                                        (p-scalar-ctx
                                                          (p-=~ $orig_s
                                                            (p-regex :pat "^\\s*\\+?\\s*(\\d+)(?:\\.\\d*)?\\s*$"
                                                              :flags ""
                                                              :tier :native)))
                                                        $1
                                                        (p-undef))))))))))
                                      (p-next)))
                                  (p-if (p-str-eq $ch "/")
                                    (p-die :loc
                                      "cl/pack-impl.pl line 723"
                                      (p-esc "Invalid type '/' in pack\\n")))
                                  (p-die :loc
                                    "cl/pack-impl.pl line 724"
                                    (p-string-concat "Invalid type '"
                                      $ch
                                      (p-esc "' in pack\\n"))))))))))))))))))))

(p-sub pl-_pack_check_brackets
  (&rest %_args)
  (:writes-args nil :needs (:nonlocal_exit.die :nonlocal_exit.return))
  (p-raw-params (($tmpl :scalar))
    (block nil
      (p-void-ctx
        (p-let (($n_open :scalar 0))
          (p-let (($n_close :scalar 0))
            (p-let (($tlen :num (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
              (p-let (($i :scalar 0))
                (p-for ()
                  ((p-< $i $tlen))
                  ((p-incf-raw $i :numeric))
                  (p-let (($c :box (make-p-box nil)))
                    (p-my-= $c (p-substr $tmpl $i 1))
                    (p-if (p-str-eq $c "[")
                      (progn (p-incf-raw $n_open :numeric))
                      (p-if (p-str-eq $c "]") (progn (p-incf-raw $n_close :numeric)))))))
              (p-if (p-> $n_open $n_close)
                (p-die :loc
                  "cl/pack-impl.pl line 737"
                  (p-esc "No group ending character ']' found in template\\n")))
              (p-if (p-! (p-> $n_open 0)) (p-return))
              (p-let ((@stk :array (make-array 0 :adjustable t :fill-pointer 0)))
                (p-array-= @stk (vector))
                (p-let (($i :scalar 0))
                  (p-for ()
                    ((p-< $i $tlen))
                    ((p-incf-raw $i :numeric))
                    (p-let (($c :box (make-p-box nil)))
                      (p-my-= $c (p-substr $tmpl $i 1))
                      (p-if (p-str-eq $c "[")
                        (progn (%p-push1 @stk "["))
                        (p-if (p-str-eq $c "(")
                          (progn (%p-push1 @stk "("))
                          (p-if (p-str-eq $c "]")
                            (progn
                              (p-if (p-|| (p-! @stk) (p-str-ne (p-aref @stk -1) "["))
                                (p-die :loc
                                  "cl/pack-impl.pl line 746"
                                  (p-esc "Mismatched brackets in template\\n")))
                              (p-pop @stk))
                            (p-if (p-str-eq $c ")")
                              (progn
                                (p-if (p-&& @stk (p-str-eq (p-aref @stk -1) "(")) (p-pop @stk))))))))))))))))))

(p-sub pl-p_pack
  (&rest %_args)
  (:wantarray-insensitive t
    :writes-args nil
    :needs (:dynamic_scope.local :regex.literal :regex.native))
  (p-args-body :copy (block nil
      (p-void-ctx
        (p-let
          (($tmpl :box (make-p-box nil))
            (@args :array (make-array 0 :adjustable t :fill-pointer 0)))
          (p-scalar-ctx (p-list-= (vector $tmpl @args) @_))
          (p-local-cell $pcl_pack_comma_warned (p-box-for-local 0)
            (pl-_pack_check_brackets $tmpl)
            (p-let (($result :box (make-p-box nil)))
              (p-my-= $result "")
              (p-let (($ai :box (make-p-box nil)))
                (p-my-= $ai 0)
                (pl-_pack_tmpl $tmpl
                  (p-backslash $ai)
                  (p-backslash @args)
                  (p-backslash $result)
                  0
                  0)
                (p-let (($c0 :str (%pcl-to-string-strict (p-substr $tmpl 0 1) "$c0")))
                  (p-if
                    (p-|| (p-str-eq $c0 "U")
                      (p-&& (p-str-le $c0 " ")
                        (p-scalar-ctx
                          (p-=~ $tmpl
                            (p-regex :pat "\\A[ \\t\\n\\r\\f]+U" :flags "" :tier :native)))))
                    (utf8::pl-decode $result))
                  (p-caller-ctx (p-tail-value $result)))))))))))

(p-sub pl-_unpack_utf8_char
  (&rest %_args)
  (:wantarray-insensitive t :writes-args nil :needs (:nonlocal_exit.return))
  (p-args-body
    (block nil
      (p-let (($s :box (make-p-box nil)) ($si_ref :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $s $si_ref) @_))
        (p-void-ctx
          (p-let (($slen :num (%pcl-to-number-strict (p-length $s) "$slen")))
            (p-if (p->= (p-cast-$ $si_ref :strict) $slen) (p-return 0))
            (p-let (($b0 :box (make-p-box nil)))
              (p-my-= $b0 (p-ord (p-substr $s (p-cast-$ $si_ref :strict-lv) 1)))
              (p-let (($nb :box (make-p-box nil)) ($code :box (make-p-box nil)))
                (p-if (p-< $b0 #x80)
                  (progn (setf $nb 1) (p-my-= $code $b0))
                  (p-if (p-< $b0 #xE0)
                    (progn (setf $nb 2) (p-my-= $code (p-bit-and $b0 #x1F)))
                    (p-if (p-< $b0 #xF0)
                      (progn (setf $nb 3) (p-my-= $code (p-bit-and $b0 #x0F)))
                      (progn (setf $nb 4) (p-my-= $code (p-bit-and $b0 #x07))))))
                (p-let (($k :scalar 1))
                  (p-for ()
                    ((p-< $k $nb))
                    ((p-incf-raw $k :numeric))
                    (p-if (p-< (p-+ (p-cast-$ $si_ref :strict) $k) $slen)
                      (p-my-= $code
                        (p-bit-or (p-<< $code 6)
                          (p-bit-and (p-ord (p-substr $s (p-+ (p-cast-$ $si_ref :strict) $k) 1))
                            #x3F))))))
                (p-incf (p-cast-$ $si_ref :strict-lv) $nb)
                (p-caller-ctx (p-tail-value $code))))))))))

(p-sub pl-_unpack_str
  (&rest %_args)
  (:writes-args nil
    :needs (:nonlocal_exit.die :nonlocal_exit.loop_control :regex.subst :regex.native))
  (p-args-body
    (block nil
      (p-let
        (($ch :box (make-p-box nil))
          ($nrep :box (make-p-box nil))
          ($all :box (make-p-box nil))
          ($s :box (make-p-box nil))
          ($si_ref :box (make-p-box nil))
          ($push_val :box (make-p-box nil))
          ($checksum_p :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $ch $nrep $all $s $si_ref $push_val $checksum_p) @_))
        (p-void-ctx
          (p-let (($slen :box (make-p-box nil) :captured t))
            (p-my-= $slen (p-length $s))
            (let ((--pcl-if-ret--2 nil))
              (p-if
                (setf --pcl-if-ret--2
                  (p-|| (p-|| (p-str-eq $ch "A") (p-str-eq $ch "a")) (p-str-eq $ch "Z")))
                (setf --pcl-if-ret--2
                  (progn
                    (p-let (($n :box (make-p-box nil)))
                      (p-my-= $n (p-if $all (p-- $slen (p-cast-$ $si_ref :strict)) $nrep))
                      (p-if (p-< $n 0) (p-my-= $n 0))
                      (p-let (($raw :box (make-p-box nil)))
                        (p-my-= $raw
                          (p-if (p-< (p-cast-$ $si_ref :strict) $slen)
                            (p-substr $s (p-cast-$ $si_ref :strict-lv) $n)
                            ""))
                        (p-incf (p-cast-$ $si_ref :strict-lv) $n)
                        (p-if (p-str-eq $ch "A")
                          (p-=~ $raw
                            (p-subst :pat "[ \\x00]+$" :rep "" :flags "" :tier :native)))
                        (p-if (p-str-eq $ch "Z")
                          (p-=~ $raw (p-subst :pat "\\x00.*" :rep "" :flags "s" :tier :native)))
                        (p-caller-ctx (p-funcall-ref $push_val $raw))))))
                (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "H"))
                  (setf --pcl-if-ret--2
                    (progn
                      (p-let (($n :box (make-p-box nil)))
                        (p-my-= $n
                          (p-if $all (p-* 2 (p-- $slen (p-cast-$ $si_ref :strict))) $nrep))
                        (p-let (($hex :box (make-p-box nil)))
                          (p-my-= $hex "")
                          (p-let (($i :scalar 0))
                            (p-for ()
                              ((p-< $i (p-int (p-/ $n 2))))
                              ((p-incf-raw $i :numeric))
                              (p-let
                                (($b__excl__0 :box
                                    (make-p-box nil)
                                    :perl "$b" :why :exception-global))
                                (p-my-= $b__excl__0
                                  (p-if (p-< (p-+ (p-cast-$ $si_ref :strict) $i) $slen)
                                    (p-ord (p-substr $s (p-+ (p-cast-$ $si_ref :strict) $i) 1))
                                    0))
                                (p-.= $hex (p-sprintf "%02x" $b__excl__0)))))
                          (p-my-= $hex (p-substr $hex 0 $n))
                          (p-incf (p-cast-$ $si_ref :strict-lv) (p-int (p-/ (p-+ $n 1) 2)))
                          (p-caller-ctx (p-funcall-ref $push_val $hex))))))
                  (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "h"))
                    (setf --pcl-if-ret--2
                      (progn
                        (p-let (($n :box (make-p-box nil)))
                          (p-my-= $n
                            (p-if $all (p-* 2 (p-- $slen (p-cast-$ $si_ref :strict))) $nrep))
                          (p-let (($hex :box (make-p-box nil)))
                            (p-my-= $hex "")
                            (p-let (($i :scalar 0))
                              (p-for ()
                                ((p-< $i (p-int (p-/ $n 2))))
                                ((p-incf-raw $i :numeric))
                                (p-let
                                  (($b__excl__1 :box
                                      (make-p-box nil)
                                      :perl "$b" :why :exception-global))
                                  (p-my-= $b__excl__1
                                    (p-if (p-< (p-+ (p-cast-$ $si_ref :strict) $i) $slen)
                                      (p-ord (p-substr $s (p-+ (p-cast-$ $si_ref :strict) $i) 1))
                                      0))
                                  (p-.= $hex
                                    (p-sprintf "%x%x"
                                      (p-bit-and $b__excl__1 #xF)
                                      (p-bit-and (p->> $b__excl__1 4) #xF))))))
                            (p-my-= $hex (p-substr $hex 0 $n))
                            (p-incf (p-cast-$ $si_ref :strict-lv) (p-int (p-/ (p-+ $n 1) 2)))
                            (p-caller-ctx (p-funcall-ref $push_val $hex))))))
                    (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "B"))
                      (setf --pcl-if-ret--2
                        (progn
                          (p-let (($nbits :box (make-p-box nil)))
                            (p-my-= $nbits
                              (p-if $all (p-* 8 (p-- $slen (p-cast-$ $si_ref :strict))) $nrep))
                            (p-if $checksum_p
                              (progn
                                (p-let (($i :scalar 0))
                                  (p-for ()
                                    ((p-< $i $nbits))
                                    ((p-incf-raw $i :numeric))
                                    :dyn
                                    t
                                    (p-let
                                      (($b__excl__2 :num
                                          (%pcl-to-number-strict
                                            (p-if
                                              (p-<
                                                (p-+ (p-cast-$ $si_ref :strict)
                                                  (p-int (p-/ $i 8)))
                                                $slen)
                                              (p-ord
                                                (p-substr $s
                                                  (p-+ (p-cast-$ $si_ref :strict)
                                                    (p-int (p-/ $i 8)))
                                                  1))
                                              0)
                                            "$b__excl__2")
                                          :perl "$b" :why :exception-global))
                                      (p-funcall-ref $push_val
                                        (p-bit-and (p->> $b__excl__2 (p-- 7 (p-% $i 8))) 1))))))
                              (progn
                                (p-let (($bits :box (make-p-box nil)))
                                  (p-my-= $bits "")
                                  (p-let (($i :scalar 0))
                                    (p-for ()
                                      ((p-< $i $nbits))
                                      ((p-incf-raw $i :numeric))
                                      (p-let
                                        (($b__excl__3 :num
                                            (%pcl-to-number-strict
                                              (p-if
                                                (p-<
                                                  (p-+ (p-cast-$ $si_ref :strict)
                                                    (p-int (p-/ $i 8)))
                                                  $slen)
                                                (p-ord
                                                  (p-substr $s
                                                    (p-+ (p-cast-$ $si_ref :strict)
                                                      (p-int (p-/ $i 8)))
                                                    1))
                                                0)
                                              "$b__excl__3")
                                            :perl "$b" :why :exception-global))
                                        (p-.= $bits
                                          (p-if
                                            (p-bit-and (p->> $b__excl__3 (p-- 7 (p-% $i 8))) 1)
                                            "1"
                                            "0")))))
                                  (p-funcall-ref $push_val $bits))))
                            (p-caller-ctx
                              (p-incf (p-cast-$ $si_ref :strict-lv)
                                (p-int (p-/ (p-+ $nbits 7) 8)))))))
                      (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "b"))
                        (setf --pcl-if-ret--2
                          (progn
                            (p-let (($nbits :box (make-p-box nil)))
                              (p-my-= $nbits
                                (p-if $all
                                  (p-* 8 (p-- $slen (p-cast-$ $si_ref :strict)))
                                  $nrep))
                              (p-if $checksum_p
                                (progn
                                  (p-let (($i :scalar 0))
                                    (p-for ()
                                      ((p-< $i $nbits))
                                      ((p-incf-raw $i :numeric))
                                      :dyn
                                      t
                                      (p-let
                                        (($b__excl__4 :num
                                            (%pcl-to-number-strict
                                              (p-if
                                                (p-<
                                                  (p-+ (p-cast-$ $si_ref :strict)
                                                    (p-int (p-/ $i 8)))
                                                  $slen)
                                                (p-ord
                                                  (p-substr $s
                                                    (p-+ (p-cast-$ $si_ref :strict)
                                                      (p-int (p-/ $i 8)))
                                                    1))
                                                0)
                                              "$b__excl__4")
                                            :perl "$b" :why :exception-global))
                                        (p-funcall-ref $push_val
                                          (p-bit-and (p->> $b__excl__4 (p-% $i 8)) 1))))))
                                (progn
                                  (p-let (($bits :box (make-p-box nil)))
                                    (p-my-= $bits "")
                                    (p-let (($i :scalar 0))
                                      (p-for ()
                                        ((p-< $i $nbits))
                                        ((p-incf-raw $i :numeric))
                                        (p-let
                                          (($b__excl__5 :num
                                              (%pcl-to-number-strict
                                                (p-if
                                                  (p-<
                                                    (p-+ (p-cast-$ $si_ref :strict)
                                                      (p-int (p-/ $i 8)))
                                                    $slen)
                                                  (p-ord
                                                    (p-substr $s
                                                      (p-+ (p-cast-$ $si_ref :strict)
                                                        (p-int (p-/ $i 8)))
                                                      1))
                                                  0)
                                                "$b__excl__5")
                                              :perl "$b" :why :exception-global))
                                          (p-.= $bits
                                            (p-if (p-bit-and (p->> $b__excl__5 (p-% $i 8)) 1)
                                              "1"
                                              "0")))))
                                    (p-funcall-ref $push_val $bits))))
                              (p-caller-ctx
                                (p-incf (p-cast-$ $si_ref :strict-lv)
                                  (p-int (p-/ (p-+ $nbits 7) 8)))))))
                        (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "u"))
                          (setf --pcl-if-ret--2
                            (progn
                              (p-let (($decoded :box (make-p-box nil)))
                                (p-my-= $decoded "")
                                (p-while (p-< (p-cast-$ $si_ref :strict) $slen)
                                  :dyn
                                  t
                                  (p-let
                                    (($lc :num
                                        (%pcl-to-number-strict
                                          (p-ord (p-substr $s (p-cast-$ $si_ref :strict-lv) 1))
                                          "$lc")))
                                    (p-let
                                      (($nb :num
                                          (%pcl-to-number-strict (p-bit-and (p-- $lc 32) 63)
                                            "$nb")))
                                      (p-post++ (p-cast-$ $si_ref))
                                      (p-if (p-== $nb 0) (p-last))
                                      (p-let
                                        (($ng :num
                                            (%pcl-to-number-strict (p-int (p-/ (p-+ $nb 2) 3))
                                              "$ng")))
                                        (p-let (($k :scalar 0))
                                          (p-for ()
                                            ((p-< $k $ng))
                                            ((p-incf-raw $k :numeric))
                                            :dyn
                                            t
                                            (p-let (($get :box (make-p-box nil)))
                                              (p-my-= $get
                                                (p-anon-sub "main"
                                                  (lambda (&rest %_args)
                                                    (let
                                                      ((@_ (p-flatten-args %_args))
                                                        (*pcl-current-package* "main")
                                                        (*pcl-caller-wantarray* *wantarray*))
                                                      (p-sub-frame
                                                        (block nil
                                                          (p-void-ctx
                                                            (p-let
                                                              (($i :scalar
                                                                  (p-+
                                                                    (p-cast-$ $si_ref :strict)
                                                                    (p-aref @_ 0))))
                                                              (p-caller-ctx
                                                                (p-if (p-< $i $slen)
                                                                  (p-bit-and
                                                                    (p--
                                                                      (p-ord (p-substr $s $i 1))
                                                                      32)
                                                                    63)
                                                                  0))))))))))
                                              (p-let (($cm :box (make-p-box nil)))
                                                (p-my-= $cm
                                                  (p-bit-or
                                                    (p-bit-or
                                                      (p-bit-or
                                                        (p-<<
                                                          (p-scalar-ctx
                                                            (p-funcall-ref $get (p-* 4 $k)))
                                                          18)
                                                        (p-<<
                                                          (p-scalar-ctx
                                                            (p-funcall-ref $get
                                                              (p-+ (p-* 4 $k) 1)))
                                                          12))
                                                      (p-<<
                                                        (p-scalar-ctx
                                                          (p-funcall-ref $get
                                                            (p-+ (p-* 4 $k) 2)))
                                                        6))
                                                    (p-scalar-ctx
                                                      (p-funcall-ref $get (p-+ (p-* 4 $k) 3)))))
                                                (p-if (p-< (p-* $k 3) $nb)
                                                  (p-.= $decoded
                                                    (p-chr (p-bit-and (p->> $cm 16) #xFF))))
                                                (p-if (p-< (p-+ (p-* $k 3) 1) $nb)
                                                  (p-.= $decoded
                                                    (p-chr (p-bit-and (p->> $cm 8) #xFF))))
                                                (p-if (p-< (p-+ (p-* $k 3) 2) $nb)
                                                  (p-.= $decoded (p-chr (p-bit-and $cm #xFF))))))))
                                        (p-incf (p-cast-$ $si_ref :strict-lv) (p-* $ng 4))
                                        (p-if
                                          (p-&& (p-< (p-cast-$ $si_ref :strict) $slen)
                                            (p-str-eq
                                              (p-substr $s (p-cast-$ $si_ref :strict-lv) 1)
                                              (p-esc "\\n")))
                                          (p-post++ (p-cast-$ $si_ref)))))))
                                (p-caller-ctx (p-funcall-ref $push_val $decoded)))))
                          (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "U"))
                            (setf --pcl-if-ret--2
                              (progn
                                (p-let (($n :box (make-p-box nil)))
                                  (p-my-= $n (p-if $all (p-** 9 9) $nrep))
                                  (p-let (($done :box (make-p-box nil)))
                                    (p-my-= $done 0)
                                    (p-while
                                      (p-&& (p-< $done $n)
                                        (p-< (p-cast-$ $si_ref :strict) $slen))
                                      :dyn
                                      t
                                      (p-funcall-ref $push_val
                                        (pl-_unpack_utf8_char $s $si_ref))
                                      (p-post++ $done))))))
                            (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "W"))
                              (setf --pcl-if-ret--2
                                (progn
                                  (p-let (($n :box (make-p-box nil)))
                                    (p-my-= $n
                                      (p-if $all (p-- $slen (p-cast-$ $si_ref :strict)) $nrep))
                                    (p-let (($i :scalar 0))
                                      (p-for ()
                                        ((p-&& (p-< $i $n)
                                            (p-< (p-cast-$ $si_ref :strict) $slen)))
                                        ((p-incf-raw $i :numeric))
                                        :dyn
                                        t
                                        (p-funcall-ref $push_val
                                          (p-ord (p-substr $s (p-post++ (p-cast-$ $si_ref)) 1))))))))
                              (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "w"))
                                (setf --pcl-if-ret--2
                                  (progn
                                    (p-let (($done :box (make-p-box nil)))
                                      (p-my-= $done 0)
                                      (p-while
                                        (p-&& (p-|| $all (p-< $done $nrep))
                                          (p-< (p-cast-$ $si_ref :strict) $slen))
                                        :dyn
                                        t
                                        (p-let (($v :box (make-p-box nil)))
                                          (p-my-= $v 0)
                                          (p-let
                                            (($more :str (%pcl-to-string-strict 1 "$more")))
                                            (p-while $more
                                              (p-if (p->= (p-cast-$ $si_ref :strict) $slen)
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 894"
                                                  (p-esc
                                                    "Unterminated compressed integer in unpack\\n")))
                                              (p-let
                                                (($b__excl__6 :box
                                                    (make-p-box nil)
                                                    :perl "$b" :why :exception-global))
                                                (p-my-= $b__excl__6
                                                  (p-ord
                                                    (p-substr $s
                                                      (p-post++ (p-cast-$ $si_ref))
                                                      1)))
                                                (setf $more
                                                  (%pcl-to-string-strict
                                                    (p-bit-and $b__excl__6 #x80)
                                                    "$more"))
                                                (p-my-= $v
                                                  (p-bit-or (p-<< $v 7)
                                                    (p-bit-and $b__excl__6 #x7F)))))
                                            (p-funcall-ref $push_val $v)
                                            (p-post++ $done)))))))
                                nil)))))))))
              --pcl-if-ret--2)))))))

(p-sub pl-_unpack_tmpl
  (&rest %_args)
  (:writes-args nil
    :captures ($MAX_GROUP_DEPTH__file__3727705105595030002)
    :needs (:nonlocal_exit.die :nonlocal_exit.loop_control :regex.literal :regex.subst :regex.native))
  (p-args-body
    (block nil
      (p-let
        (($tmpl :box (make-p-box nil))
          ($s :box (make-p-box nil))
          ($si_ref :box (make-p-box nil))
          ($push_val :box (make-p-box nil))
          ($inh_be :box (make-p-box nil))
          ($inh_le :box (make-p-box nil))
          ($checksum_p :box (make-p-box nil))
          ($group_base :box (make-p-box nil))
          ($depth :box (make-p-box nil)))
        (p-scalar-ctx
          (p-list-=
            (vector $tmpl $s $si_ref $push_val $inh_be $inh_le $checksum_p $group_base $depth)
            @_))
        (p-void-ctx (p-if (p-! (p-defined $group_base)) (p-my-= $group_base 0))
          (p-if (p-! (p-defined $depth)) (p-my-= $depth 0))
          (p-if (p-> $depth $MAX_GROUP_DEPTH__file__3727705105595030002)
            (p-die :loc
              "cl/pack-impl.pl line 910"
              (p-esc "Too deeply nested ()-groups in unpack\\n")))
          (p-let (($slen :box (make-p-box nil)))
            (p-my-= $slen (p-length $s))
            (p-let (($ti :box (make-p-box nil)))
              (p-my-= $ti 0)
              (p-let (($tlen :num (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
                (p-while 1
                  :dyn
                  t
                  (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                  (p-if (p->= $ti $tlen) (p-last))
                  (p-let (($ch :box (make-p-box nil)))
                    (p-my-= $ch (p-substr $tmpl $ti 1))
                    (p-post++ $ti)
                    (p-let (($grpbeg :box (make-p-box nil)))
                      (p-my-= $grpbeg (p-undef))
                      (p-let (($grpend :num (%pcl-to-number-strict (p-undef) "$grpend")))
                        (p-if (p-str-eq $ch "(")
                          (progn
                            (setf $grpend
                              (%pcl-to-number-strict (pl-_pack_find_group_end $tmpl $ti)
                                "$grpend"))
                            (p-my-= $grpbeg $ti)
                            (p-my-= $ti (p-+ $grpend 1))
                            (p-my-= $ch "(")))
                        (p-let
                          (($bang :box (make-p-box nil))
                            ($be :box (make-p-box nil))
                            ($le :box (make-p-box nil)))
                          (p-scalar-ctx
                            (p-list-= (vector $bang $be $le)
                              (p-list-ctx
                                (pl-_pack_parse_mods $tmpl
                                  (p-backslash $ti)
                                  $inh_be
                                  $inh_le
                                  $ch
                                  "unpack"))))
                          (p-let
                            (($ti_before_count :num
                                (%pcl-to-number-strict $ti "$ti_before_count")))
                            (p-let
                              (($all :box (make-p-box nil))
                                ($count :box (make-p-box nil))
                                ($nrep :box (make-p-box nil)))
                              (p-scalar-ctx
                                (p-list-= (vector $all $count $nrep)
                                  (p-list-ctx (pl-_pack_parse_count $tmpl (p-backslash $ti)))))
                              (p-let (($had_count :box (make-p-box nil)))
                                (p-my-= $had_count (p-|| $all (p-> $ti $ti_before_count)))
                                (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                (p-if
                                  (p-&& (p-< $ti $tlen) (p-str-eq (p-substr $tmpl $ti 1) "/"))
                                  (progn (p-post++ $ti)
                                    (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                    (p-if (p->= $ti $tlen)
                                      (p-die :loc
                                        "cl/pack-impl.pl line 934"
                                        (p-esc "Code missing after '/' in unpack\\n")))
                                    (let ((*package* *package*))
                                      (block nil
                                        (tagbody :redo
                                          (p-let (($c :box (make-p-box nil)))
                                            (p-my-= $c (p-substr $tmpl $ti 1))
                                            (p-if
                                              (p-|| (p-|| (p-str-eq $c "*") (p-str-eq $c "["))
                                                (p-scalar-ctx
                                                  (p-=~ $c
                                                    (p-regex :pat "\\d"
                                                      :flags ""
                                                      :tier :native))))
                                              (p-die :loc
                                                "cl/pack-impl.pl line 938"
                                                (p-esc
                                                  "'/' does not take a repeat count in unpack\\n"))))
                                          :next)))
                                    (p-let
                                      (($nb :box (make-p-box nil))
                                        ($sig :box (make-p-box nil))
                                        ($dbe :box (make-p-box nil)))
                                      (p-scalar-ctx
                                        (p-list-= (vector $nb $sig $dbe)
                                          (p-list-ctx (pl-_pack_type_info $ch $bang))))
                                      (p-let (($slash_n :box (make-p-box nil)))
                                        (p-my-= $slash_n 0)
                                        (p-if $nb
                                          (progn
                                            (p-let (($be2 :box (make-p-box nil)))
                                              (p-my-= $be2 (p-if $be 1 (p-if $le 0 $dbe)))
                                              (p-if
                                                (p-> (p-+ (p-cast-$ $si_ref :strict) $nb)
                                                  $slen)
                                                (progn (p-if (p-! (p-> $depth 0)) (p-last))
                                                  (p-die :loc
                                                    "cl/pack-impl.pl line 946"
                                                    (p-esc
                                                      "length/code after end of string in unpack\\n"))))
                                              (p-my-= $slash_n
                                                (pl-_unpack_read_int $s
                                                  (p-cast-$ $si_ref :strict-lv)
                                                  $nb
                                                  $be2
                                                  $sig))
                                              (p-incf (p-cast-$ $si_ref :strict-lv) $nb)))
                                          (p-if (p-str-eq $ch "w")
                                            (progn
                                              (p-let (($more :box (make-p-box nil)))
                                                (p-my-= $more 1)
                                                (p-while $more
                                                  (p-if (p->= (p-cast-$ $si_ref :strict) $slen)
                                                    (p-die :loc
                                                      "cl/pack-impl.pl line 953"
                                                      (p-esc
                                                        "Unterminated compressed integer in unpack\\n")))
                                                  (p-let
                                                    (($b__excl__7 :box
                                                        (make-p-box nil)
                                                        :perl "$b" :why :exception-global))
                                                    (p-my-= $b__excl__7
                                                      (p-ord
                                                        (p-substr $s
                                                          (p-post++ (p-cast-$ $si_ref))
                                                          1)))
                                                    (p-my-= $more (p-bit-and $b__excl__7 #x80))
                                                    (p-my-= $slash_n
                                                      (p-bit-or (p-<< $slash_n 7)
                                                        (p-bit-and $b__excl__7 #x7F)))))))
                                            (p-if (p-str-eq $ch "Z")
                                              (progn
                                                (p-let
                                                  (($end :num
                                                      (%pcl-to-number-strict
                                                        (p-index $s
                                                          (p-esc "\\u0000")
                                                          (p-cast-$ $si_ref :strict-lv))
                                                        "$end")))
                                                  (p-if (p-< $end 0)
                                                    (progn
                                                      (setf $end
                                                        (%pcl-to-number-strict $slen "$end"))))
                                                  (p-let (($raw :box (make-p-box nil)))
                                                    (p-my-= $raw
                                                      (p-substr $s
                                                        (p-cast-$ $si_ref :strict-lv)
                                                        (p-- $end (p-cast-$ $si_ref :strict))))
                                                    (p-setf (p-cast-$ $si_ref :strict-lv)
                                                      (p-+ $end 1))
                                                    (p-if (p-> (p-cast-$ $si_ref :strict) $slen)
                                                      (p-setf (p-cast-$ $si_ref :strict-lv)
                                                        $slen))
                                                    (p-my-= $slash_n (p-+ $raw 0)))))
                                              (progn
                                                (p-let (($n :box (make-p-box nil)))
                                                  (p-my-= $n
                                                    (p-if $all
                                                      (p-- $slen (p-cast-$ $si_ref :strict))
                                                      $nrep))
                                                  (p-let (($raw :box (make-p-box nil)))
                                                    (p-my-= $raw
                                                      (p-if
                                                        (p-< (p-cast-$ $si_ref :strict) $slen)
                                                        (p-substr $s
                                                          (p-cast-$ $si_ref :strict-lv)
                                                          $n)
                                                        ""))
                                                    (p-incf (p-cast-$ $si_ref :strict-lv) $n)
                                                    (p-if (p-str-eq $ch "A")
                                                      (p-=~ $raw
                                                        (p-subst :pat "[ \\x00]+$"
                                                          :rep ""
                                                          :flags ""
                                                          :tier :native)))
                                                    (p-my-= $slash_n (p-+ $raw 0))))))))
                                        (p-while 1
                                          :dyn
                                          t
                                          (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                          (p-if (p->= $ti $tlen) (p-last))
                                          (p-let (($dch :box (make-p-box nil)))
                                            (p-my-= $dch (p-substr $tmpl $ti 1))
                                            (p-post++ $ti)
                                            (p-let
                                              (($dbang :box (make-p-box nil))
                                                ($dbe2 :box (make-p-box nil))
                                                ($dle2 :box (make-p-box nil)))
                                              (p-scalar-ctx
                                                (p-list-= (vector $dbang $dbe2 $dle2)
                                                  (p-list-ctx
                                                    (pl-_pack_parse_mods $tmpl
                                                      (p-backslash $ti)
                                                      $be
                                                      $le
                                                      $dch
                                                      "unpack"))))
                                              (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                              (p-let
                                                (($dall :box (make-p-box nil))
                                                  ($dcnt :box (make-p-box nil))
                                                  ($dnrep :box (make-p-box nil)))
                                                (p-scalar-ctx
                                                  (p-list-= (vector $dall $dcnt $dnrep)
                                                    (p-list-ctx
                                                      (pl-_pack_parse_count $tmpl
                                                        (p-backslash $ti)))))
                                                (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                                (p-let
                                                  (($chain :str
                                                      (%pcl-to-string-strict
                                                        (p-&& (p-< $ti $tlen)
                                                          (p-str-eq (p-substr $tmpl $ti 1) "/"))
                                                        "$chain")))
                                                  (p-if $chain (progn (p-post++ $ti)))
                                                  (p-let
                                                    (($dnb :box (make-p-box nil))
                                                      ($dsig :box (make-p-box nil))
                                                      ($ddbe :box (make-p-box nil)))
                                                    (p-scalar-ctx
                                                      (p-list-= (vector $dnb $dsig $ddbe)
                                                        (p-list-ctx
                                                          (pl-_pack_type_info $dch $dbang))))
                                                    (p-if $chain
                                                      (progn
                                                        (p-if $dnb
                                                          (progn
                                                            (p-let
                                                              (($dbe3 :box (make-p-box nil)))
                                                              (p-my-= $dbe3
                                                                (p-if $dbe2
                                                                  1
                                                                  (p-if $dle2 0 $ddbe)))
                                                              (p-if
                                                                (p->
                                                                  (p-+
                                                                    (p-cast-$ $si_ref :strict)
                                                                    $dnb)
                                                                  $slen)
                                                                (p-die :loc
                                                                  "cl/pack-impl.pl line 990"
                                                                  (p-esc
                                                                    "length/code after end of string in unpack\\n")))
                                                              (p-my-= $slash_n
                                                                (pl-_unpack_read_int $s
                                                                  (p-cast-$ $si_ref :strict-lv)
                                                                  $dnb
                                                                  $dbe3
                                                                  $dsig))
                                                              (p-incf
                                                                (p-cast-$ $si_ref :strict-lv)
                                                                $dnb)))
                                                          (p-if (p-str-eq $dch "w")
                                                            (progn (p-my-= $slash_n 0)
                                                              (p-let
                                                                (($more :box (make-p-box nil)))
                                                                (p-my-= $more 1)
                                                                (p-while $more
                                                                  (p-if
                                                                    (p->=
                                                                      (p-cast-$ $si_ref
                                                                        :strict)
                                                                      $slen)
                                                                    (p-last))
                                                                  (p-let
                                                                    (($b__excl__8 :box
                                                                        (make-p-box nil)
                                                                        :perl "$b" :why :exception-global))
                                                                    (p-my-= $b__excl__8
                                                                      (p-ord
                                                                        (p-substr $s
                                                                          (p-post++
                                                                            (p-cast-$ $si_ref))
                                                                          1)))
                                                                    (p-my-= $more
                                                                      (p-bit-and $b__excl__8
                                                                        #x80))
                                                                    (p-my-= $slash_n
                                                                      (p-bit-or
                                                                        (p-<< $slash_n 7)
                                                                        (p-bit-and $b__excl__8
                                                                          #x7F)))))))
                                                            (progn
                                                              (p-let
                                                                (($raw2 :box (make-p-box nil)))
                                                                (p-my-= $raw2
                                                                  (p-if
                                                                    (p-<
                                                                      (p-cast-$ $si_ref
                                                                        :strict)
                                                                      $slen)
                                                                    (p-substr $s
                                                                      (p-cast-$ $si_ref
                                                                        :strict-lv)
                                                                      $slash_n)
                                                                    ""))
                                                                (p-incf
                                                                  (p-cast-$ $si_ref :strict-lv)
                                                                  $slash_n)
                                                                (p-if (p-str-eq $dch "A")
                                                                  (p-=~ $raw2
                                                                    (p-subst :pat "[ \\x00]+$"
                                                                      :rep ""
                                                                      :flags ""
                                                                      :tier :native)))
                                                                (p-if (p-str-eq $dch "Z")
                                                                  (p-=~ $raw2
                                                                    (p-subst :pat "\\x00.*"
                                                                      :rep ""
                                                                      :flags "s"
                                                                      :tier :native)))
                                                                (p-my-= $slash_n (p-+ $raw2 0)))))))
                                                      (progn
                                                        (p-if $dnb
                                                          (progn
                                                            (p-let
                                                              (($dbe3 :box (make-p-box nil)))
                                                              (p-my-= $dbe3
                                                                (p-if $dbe2
                                                                  1
                                                                  (p-if $dle2 0 $ddbe)))
                                                              (p-let (($i :scalar 0))
                                                                (p-for ()
                                                                  ((p-&& (p-< $i $slash_n)
                                                                      (p-<=
                                                                        (p-+
                                                                          (p-cast-$ $si_ref
                                                                            :strict)
                                                                          $dnb)
                                                                        $slen)))
                                                                  ((p-incf-raw $i :numeric))
                                                                  :dyn
                                                                  t
                                                                  (p-funcall-ref $push_val
                                                                    (pl-_unpack_read_int $s
                                                                      (p-cast-$ $si_ref
                                                                        :strict-lv)
                                                                      $dnb
                                                                      $dbe3
                                                                      $dsig))
                                                                  (p-incf
                                                                    (p-cast-$ $si_ref
                                                                      :strict-lv)
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
                                                                (p-let
                                                                  (($ge :num
                                                                      (%pcl-to-number-strict
                                                                        (pl-_pack_find_group_end
                                                                          $tmpl
                                                                          $ti)
                                                                        "$ge")))
                                                                  (p-let
                                                                    (($inner :box
                                                                        (make-p-box nil)))
                                                                    (p-my-= $inner
                                                                      (p-substr $tmpl
                                                                        $ti
                                                                        (p-- $ge $ti)))
                                                                    (p-my-= $ti (p-+ $ge 1))
                                                                    (p-let (($r :scalar 0))
                                                                      (p-for ()
                                                                        ((p-< $r $slash_n))
                                                                        ((p-incf-raw $r
                                                                            :numeric))
                                                                        (p-let
                                                                          (($iter_base :box
                                                                              (make-p-box nil)))
                                                                          (p-my-= $iter_base
                                                                            (p-cast-$ $si_ref
                                                                              :strict))
                                                                          (pl-_unpack_tmpl
                                                                            $inner
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
                                    (p-let (($inner :box (make-p-box nil)))
                                      (p-my-= $inner
                                        (p-substr $tmpl $grpbeg (p-- $grpend $grpbeg)))
                                      (p-let
                                        (($gti :num
                                            (%pcl-to-number-strict (pl-_pack_skip_ws $inner 0)
                                              "$gti")))
                                        (p-if (p-< $gti (p-length $inner))
                                          (progn
                                            (p-let (($fc :box (make-p-box nil)))
                                              (p-my-= $fc (p-substr $inner $gti 1))
                                              (p-if
                                                (p-scalar-ctx
                                                  (p-=~ $fc
                                                    (p-regex :pat "^[\\d\\*\\[]"
                                                      :flags ""
                                                      :tier :native)))
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 1043"
                                                  (p-esc
                                                    "()-group starts with a count in unpack\\n"))))))
                                        (p-if $all
                                          (progn
                                            (p-while (p-< (p-cast-$ $si_ref :strict) $slen)
                                              (p-let
                                                (($si_before :num
                                                    (%pcl-to-number-strict
                                                      (p-cast-$ $si_ref :strict)
                                                      "$si_before")))
                                                (p-let (($iter_base :box (make-p-box nil)))
                                                  (p-my-= $iter_base
                                                    (p-cast-$ $si_ref :strict))
                                                  (pl-_unpack_tmpl $inner
                                                    $s
                                                    $si_ref
                                                    $push_val
                                                    $be
                                                    $le
                                                    $checksum_p
                                                    $iter_base
                                                    (p-+ $depth 1))
                                                  (p-if
                                                    (p-== (p-cast-$ $si_ref :strict)
                                                      $si_before)
                                                    (p-last))))))
                                          (progn
                                            (p-let (($r :scalar 0))
                                              (p-for ()
                                                ((p-< $r $nrep))
                                                ((p-incf-raw $r :numeric))
                                                (p-let (($iter_base :box (make-p-box nil)))
                                                  (p-my-= $iter_base
                                                    (p-cast-$ $si_ref :strict))
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
                                        (p-let (($n :box (make-p-box nil)))
                                          (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                          (p-incf (p-cast-$ $si_ref :strict-lv)
                                            (p-% (p-- $n (p-% (p-cast-$ $si_ref :strict) $n))
                                              $n))))
                                      (p-if $all
                                        (progn (p-setf (p-cast-$ $si_ref :strict-lv) $slen))
                                        (progn
                                          (p-if
                                            (p-> (p-+ (p-cast-$ $si_ref :strict) $nrep) $slen)
                                            (p-die :loc
                                              "cl/pack-impl.pl line 1070"
                                              (p-esc "'x' outside of string in unpack\\n")))
                                          (p-incf (p-cast-$ $si_ref :strict-lv) $nrep))))
                                    (p-next)))
                                (p-if (p-str-eq $ch "X")
                                  (progn
                                    (p-if $bang
                                      (progn
                                        (p-let (($n :box (make-p-box nil)))
                                          (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                          (p-setf (p-cast-$ $si_ref :strict-lv)
                                            (p-* (p-int (p-/ (p-cast-$ $si_ref :strict) $n))
                                              $n))))
                                      (progn
                                        (p-if (p-> $nrep (p-cast-$ $si_ref :strict))
                                          (p-die :loc
                                            "cl/pack-impl.pl line 1081"
                                            (p-esc "'X' outside of string in unpack\\n")))
                                        (p-decf (p-cast-$ $si_ref :strict-lv) $nrep)))
                                    (p-next)))
                                (p-if (p-str-eq $ch "@")
                                  (progn
                                    (p-let (($n :box (make-p-box nil)))
                                      (p-my-= $n (p-if (p-defined $count) $count 0))
                                      (p-setf (p-cast-$ $si_ref :strict-lv)
                                        (p-if $bang $n (p-+ $group_base $n)))
                                      (p-next))))
                                (p-if (p-|| (p-str-eq $ch "%") (p-str-eq $ch "!"))
                                  (progn (p-next)))
                                (p-if
                                  (p-|| (p-|| (p-str-eq $ch "p") (p-str-eq $ch "P"))
                                    (p-str-eq $ch "D"))
                                  (progn
                                    (p-die :loc
                                      "cl/pack-impl.pl line 1093"
                                      (p-string-concat "Invalid type '"
                                        $ch
                                        (p-esc "' in unpack\\n")))))
                                (p-if (p-str-eq $ch ".")
                                  (progn
                                    (p-if $all
                                      (progn
                                        (p-funcall-ref $push_val (p-cast-$ $si_ref :strict-lv)))
                                      (p-if (p-&& (p-defined $count) (p-== $count 0))
                                        (progn (p-funcall-ref $push_val 0))
                                        (p-if (p-&& (p-defined $count) (p->= $count 2))
                                          (progn
                                            (p-funcall-ref $push_val
                                              (p-cast-$ $si_ref :strict-lv)))
                                          (progn
                                            (p-funcall-ref $push_val
                                              (p-- (p-cast-$ $si_ref :strict) $group_base))))))
                                    (p-next)))
                                (p-let
                                  (($nb :box (make-p-box nil))
                                    ($sig :box (make-p-box nil))
                                    ($dbe :box (make-p-box nil)))
                                  (p-scalar-ctx
                                    (p-list-= (vector $nb $sig $dbe)
                                      (p-list-ctx (pl-_pack_type_info $ch $bang))))
                                  (p-if $nb
                                    (progn
                                      (p-let (($be2 :box (make-p-box nil)))
                                        (p-my-= $be2 (p-if $be 1 (p-if $le 0 $dbe)))
                                        (p-let (($n :box (make-p-box nil)))
                                          (p-my-= $n
                                            (p-if $all
                                              (p-int
                                                (p-/ (p-- $slen (p-cast-$ $si_ref :strict))
                                                  $nb))
                                              $nrep))
                                          (p-let (($i :scalar 0))
                                            (p-for ()
                                              ((p-< $i $n))
                                              ((p-incf-raw $i :numeric))
                                              :dyn
                                              t
                                              (p-if
                                                (p-> (p-+ (p-cast-$ $si_ref :strict) $nb)
                                                  $slen)
                                                (p-last))
                                              (p-funcall-ref $push_val
                                                (pl-_unpack_read_int $s
                                                  (p-cast-$ $si_ref :strict-lv)
                                                  $nb
                                                  $be2
                                                  $sig))
                                              (p-incf (p-cast-$ $si_ref :strict-lv) $nb)))
                                          (p-next)))))
                                  (p-if (p-str-eq $ch "f")
                                    (progn
                                      (p-let (($be2 :box (make-p-box nil)))
                                        (p-my-= $be2 (p-if $be 1 (p-if $le 0 0)))
                                        (p-let (($n :box (make-p-box nil)))
                                          (p-my-= $n
                                            (p-if $all
                                              (p-int
                                                (p-/ (p-- $slen (p-cast-$ $si_ref :strict)) 4))
                                              $nrep))
                                          (p-let (($i :scalar 0))
                                            (p-for ()
                                              ((p-< $i $n))
                                              ((p-incf-raw $i :numeric))
                                              :dyn
                                              t
                                              (p-if
                                                (p-> (p-+ (p-cast-$ $si_ref :strict) 4) $slen)
                                                (p-last))
                                              (p-funcall-ref $push_val
                                                (pl-_unpack_float32 $s
                                                  (p-cast-$ $si_ref :strict-lv)
                                                  $be2))
                                              (p-incf (p-cast-$ $si_ref :strict-lv) 4)))
                                          (p-next)))))
                                  (p-if (p-|| (p-str-eq $ch "d") (p-str-eq $ch "F"))
                                    (progn
                                      (p-let (($be2 :box (make-p-box nil)))
                                        (p-my-= $be2 (p-if $be 1 (p-if $le 0 0)))
                                        (p-let (($n :box (make-p-box nil)))
                                          (p-my-= $n
                                            (p-if $all
                                              (p-int
                                                (p-/ (p-- $slen (p-cast-$ $si_ref :strict)) 8))
                                              $nrep))
                                          (p-let (($i :scalar 0))
                                            (p-for ()
                                              ((p-< $i $n))
                                              ((p-incf-raw $i :numeric))
                                              :dyn
                                              t
                                              (p-if
                                                (p-> (p-+ (p-cast-$ $si_ref :strict) 8) $slen)
                                                (p-last))
                                              (p-funcall-ref $push_val
                                                (pl-_unpack_float64 $s
                                                  (p-cast-$ $si_ref :strict-lv)
                                                  $be2))
                                              (p-incf (p-cast-$ $si_ref :strict-lv) 8)))
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
                                      "cl/pack-impl.pl line 1152"
                                      (p-esc "'/' must follow a numeric type in unpack\\n")))
                                  (p-die :loc
                                    "cl/pack-impl.pl line 1153"
                                    (p-string-concat "Invalid type '"
                                      $ch
                                      (p-esc "' in unpack\\n"))))))))))))))))))))

(p-sub pl-_next_format_item
  (&rest %_args)
  (:writes-args nil :needs (:nonlocal_exit.return :regex.literal :regex.native))
  (p-args-body :copy (block nil
      (p-let (($tmpl :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $tmpl) @_))
        (p-void-ctx
          (p-let (($tlen :num (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
            (p-let (($ti :box (make-p-box nil)))
              (p-my-= $ti 0)
              (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
              ;; return ('', '') if $ti >= $tlen
(p-if (p->= $ti $tlen) (p-return "" ""))
              (p-let (($ch :str (%pcl-to-string-strict (p-substr $tmpl $ti 1) "$ch")))
                (p-post++ $ti)
                (p-if (p-str-eq $ch "(")
                  (progn
                    (p-let
                      (($grpend :num
                          (%pcl-to-number-strict (pl-_pack_find_group_end $tmpl $ti) "$grpend")))
                      (p-my-= $ti (p-+ $grpend 1)))))
                (p-while
                  (p-&& (p-< $ti $tlen)
                    (p-scalar-ctx
                      (p-=~ (p-substr $tmpl $ti 1)
                        (p-regex :pat "[!<>]" :flags "" :tier :native))))
                  (p-post++ $ti))
                (pl-_pack_parse_count $tmpl (p-backslash $ti))
                ;; return (substr($tmpl, 0, $ti), substr($tmpl, $ti))
(p-return (p-substr $tmpl 0 $ti) (p-substr $tmpl $ti))))))))))

(p-sub pl-p_unpack
  (&rest %_args)
  (:writes-args nil :needs (:regex.literal :regex.split :regex.subst :regex.native))
  (p-args-body
    (block nil
      (p-let (($tmpl :box (make-p-box nil)) ($s :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $tmpl $s) @_))
        (p-void-ctx (p-if (p-! (p-defined $s)) (p-my-= $s ""))
          (p-let (($lead_u :scalar (p-str-eq (p-substr $tmpl 0 1) "U")))
            (p-=~ $tmpl
              (p-subst :pat "\\A(?:[ \\t\\n\\r\\f,]|#[^\\n]*\\n?)*"
                :rep ""
                :flags ""
                :tier :native))
            (p-let (($checksum_width :box (make-p-box nil)))
              (p-my-= $checksum_width 0)
              (p-if (p-=~ $tmpl (p-subst :pat "^%(\\d*)" :rep "" :flags "" :tier :native))
                (progn (p-my-= $checksum_width (p-if (p-length $1) (p-int $1) 16))))
              (p-let
                (($utf8_mode :str
                    (%pcl-to-string-strict
                      (p-|| (p-=~ $tmpl (p-subst :pat "^U0" :rep "" :flags "" :tier :native))
                        $lead_u)
                      "$utf8_mode")))
                (p-if $checksum_width
                  (p-=~ $tmpl
                    (p-subst :pat "\\A(?:[ \\t\\n\\r\\f,]|#[^\\n]*\\n?)*"
                      :rep ""
                      :flags ""
                      :tier :native)))
                (pl-_pack_check_brackets $tmpl)
                (p-if $utf8_mode
                  (progn
                    (p-let (($bytes :scalar ""))
                      (p-foreach-raw ($c (p-split (p-regex :pat "" :flags "" :tier :native) $s))
                        :my
                        t
                        (p-let (($code :box (make-p-box nil)))
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
                                          (p-chr
                                            (p-bit-or #x80 (p-bit-and (p->> $code 12) #x3F))))
                                        (p-chr (p-bit-or #x80 (p-bit-and (p->> $code 6) #x3F))))
                                      (p-chr (p-bit-or #x80 (p-bit-and $code #x3F)))))))))))
                      (p-my-= $s $bytes))))
                (p-let ((@result :array (make-array 0 :adjustable t :fill-pointer 0)))
                  (p-let (($si :box (make-p-box nil)))
                    (p-my-= $si 0)
                    (p-if $checksum_width
                      (progn
                        (p-let
                          (($cs_tmpl :box (make-p-box nil)) ($rest_tmpl :box (make-p-box nil)))
                          (p-scalar-ctx
                            (p-list-= (vector $cs_tmpl $rest_tmpl)
                              (p-list-ctx (pl-_next_format_item $tmpl))))
                          (p-let (($checksum :box (make-p-box nil) :captured t))
                            (p-my-= $checksum 0)
                            (p-if (p-length $cs_tmpl)
                              (progn
                                (pl-_unpack_tmpl $cs_tmpl
                                  $s
                                  (p-backslash $si)
                                  (p-anon-sub "main"
                                    (lambda (&rest %_args)
                                      (let
                                        ((@_ (p-flatten-args %_args))
                                          (*pcl-current-package* "main")
                                          (*pcl-caller-wantarray* *wantarray*))
                                        (p-sub-frame
                                          (block nil (p-incf $checksum (p-aref @_ 0)))))))
                                  0
                                  0
                                  1)))
                            (p-let (($mod :scalar 1))
                              (p-foreach-range-raw ($_ 1 $checksum_width)
                                (p-*=-raw $mod 2 :numeric))
                              (p-let (($q :box (make-p-box nil)))
                                (p-my-= $q (p-int (p-/ $checksum $mod)))
                                (p-if (p-> (p-* $q $mod) $checksum) (p-post-- $q))
                                (p-push @result (p-- $checksum (p-* $q $mod)))
                                (p-if (p-length $rest_tmpl)
                                  (progn
                                    (pl-_unpack_tmpl $rest_tmpl
                                      $s
                                      (p-backslash $si)
                                      (p-anon-sub "main"
                                        (lambda (&rest %_args)
                                          (let
                                            ((@_ (p-flatten-args %_args))
                                              (*pcl-current-package* "main")
                                              (*pcl-caller-wantarray* *wantarray*))
                                            (p-sub-frame
                                              (block nil (p-push @result (p-aref @_ 0)))))))
                                      0
                                      0
                                      0))))))))
                      (progn
                        (pl-_unpack_tmpl $tmpl
                          $s
                          (p-backslash $si)
                          (p-anon-sub "main"
                            (lambda (&rest %_args)
                              (let
                                ((@_ (p-flatten-args %_args))
                                  (*pcl-current-package* "main")
                                  (*pcl-caller-wantarray* *wantarray*))
                                (p-sub-frame (block nil (p-push @result (p-aref @_ 0)))))))
                          0
                          0
                          0)))
                    (p-caller-ctx (p-tail-value (p-if (p-wantarray) @result (p-aref @result 0))))))))))))))

(pcl::p-eval-always (pcl::p-detect-end "cl/pack-impl.pl"))
(p-run-compile-phase-blocks)

(p-scalar-= $CAN_ENDIAN__file__3727705105595030000 "sSiIlLqQjJfFdDpP")

(p-scalar-= $CAN_SHRIEK__file__3727705105595030001 "sSiIlLnNvVxX.@")

(p-scalar-= $MAX_GROUP_DEPTH__file__3727705105595030002 100)

(p-scalar-= $pcl_pack_comma_warned 0)

(p-unit-value
1
)



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

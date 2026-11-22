;;; pcl: pipeline=v2 gen=v2-15700
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
      (p-file "cl/pack-impl.pl")
      (p-void-ctx (p-line 62)
        (p-if (p-str-eq $ch "c")
          (progn (p-line 62)
            ;; return (1, 1, 0)
(p-return 1 1 0))
          (p-if (p-str-eq $ch "C")
            (progn (p-line 63)
              ;; return (1, 0, 0)
(p-return 1 0 0))
            (p-if (p-str-eq $ch "s")
              (progn (p-line 64)
                ;; return (2, 1, 0)
(p-return 2 1 0))
              (p-if (p-str-eq $ch "S")
                (progn (p-line 65)
                  ;; return (2, 0, 0)
(p-return 2 0 0))
                (p-if (p-str-eq $ch "n")
                  (progn (p-line 66)
                    ;; return (2, ($bang ? 1 : 0), 1)
(p-return 2 (p-if $bang 1 0) 1))
                  (p-if (p-str-eq $ch "v")
                    (progn (p-line 67)
                      ;; return (2, ($bang ? 1 : 0), 0)
(p-return 2 (p-if $bang 1 0) 0))
                    (p-if (p-str-eq $ch "i")
                      (progn (p-line 68)
                        ;; return (4, 1, 0)
(p-return 4 1 0))
                      (p-if (p-str-eq $ch "I")
                        (progn (p-line 69)
                          ;; return (4, 0, 0)
(p-return 4 0 0))
                        (p-if (p-str-eq $ch "l")
                          (progn (p-line 70)
                            ;; return (($bang ? 8 : 4), 1, 0)
(p-return (p-if $bang 8 4) 1 0))
                          (p-if (p-str-eq $ch "L")
                            (progn (p-line 71)
                              ;; return (($bang ? 8 : 4), 0, 0)
(p-return (p-if $bang 8 4) 0 0))
                            (p-if (p-str-eq $ch "N")
                              (progn (p-line 72)
                                ;; return (4, ($bang ? 1 : 0), 1)
(p-return 4 (p-if $bang 1 0) 1))
                              (p-if (p-str-eq $ch "V")
                                (progn (p-line 73)
                                  ;; return (4, ($bang ? 1 : 0), 0)
(p-return 4 (p-if $bang 1 0) 0))
                                (p-if (p-str-eq $ch "q")
                                  (progn (p-line 74)
                                    ;; return (8, 1, 0)
(p-return 8 1 0))
                                  (p-if (p-str-eq $ch "Q")
                                    (progn (p-line 75)
                                      ;; return (8, 0, 0)
(p-return 8 0 0))
                                    (p-if (p-str-eq $ch "j")
                                      (progn (p-line 76)
                                        ;; return (8, 1, 0)
(p-return 8 1 0))
                                      (p-if (p-str-eq $ch "J")
                                        (progn (p-line 77)
                                          ;; return (8, 0, 0)
(p-return 8 0 0))))))))))))))))))
        (p-line 78)
        (p-caller-ctx (p-tail-value (progn)))))))

(p-sub pl-_pack_skip_ws
  (&rest %_args)
  (:wantarray-insensitive t :writes-args nil :needs (:nonlocal_exit.loop_control))
  (p-args-body :copy (block nil
      (p-let (($s :box (make-p-box nil)) ($ti :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $s $ti) @_))
        (p-file "cl/pack-impl.pl")
        (p-void-ctx (p-line 85)
          (p-let (($tlen :num (%pcl-to-number-strict (p-length $s) "$tlen")))
            (p-line 86)
            (p-while (p-< $ti $tlen)
              (p-line 87)
              (p-let (($ch :str (%pcl-to-string-strict (p-substr $s $ti 1) "$ch")))
                (p-line 88)
                (p-if
                  (p-||
                    (p-||
                      (p-|| (p-|| (p-str-eq $ch " ") (p-str-eq $ch (p-esc "\\t")))
                        (p-str-eq $ch (p-esc "\\n")))
                      (p-str-eq $ch (p-esc "\\r")))
                    (p-str-eq $ch (p-esc "\\u000C")))
                  (progn (p-line 89) (p-post++ $ti))
                  (p-if (p-str-eq $ch ",")
                    (progn (p-line 92)
                      (p-if (p-! $pcl_pack_comma_warned)
                        (progn (p-line 93)
                          (p-warn :loc
                            "cl/pack-impl.pl line 93"
                            (p-esc "Invalid type ',' in pack\\n"))
                          (p-line 94)
                          (p-scalar-= $pcl_pack_comma_warned 1)))
                      (p-line 96)
                      (p-post++ $ti))
                    (p-if (p-str-eq $ch "#")
                      (progn (p-line 98)
                        (p-post++ $ti)
                        (p-line 99)
                        (p-while
                          (p-&& (p-< $ti $tlen) (p-str-ne (p-substr $s $ti 1) (p-esc "\\n")))
                          (p-line 99)
                          (p-post++ $ti))
                        (p-line 100)
                        (p-if (p-< $ti $tlen) (p-post++ $ti)))
                      (progn (p-line 102) (p-last)))))))
            (p-line 105)
            (p-caller-ctx (p-tail-value $ti))))))))

(p-sub pl-_pack_find_group_end
  (&rest %_args)
  (:wantarray-insensitive t :writes-args nil :needs ())
  (p-args-body :copy (block nil
      (p-let (($s :box (make-p-box nil)) ($ti :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $s $ti) @_))
        (p-file "cl/pack-impl.pl")
        (p-void-ctx (p-line 111)
          (p-let (($tlen :num (%pcl-to-number-strict (p-length $s) "$tlen")))
            (p-line 112)
            (p-let (($depth :scalar 1))
              (p-line 113)
              (p-while (p-&& (p-< $ti $tlen) (p-> $depth 0))
                (p-line 114)
                (p-let (($ch :str (%pcl-to-string-strict (p-substr $s $ti 1) "$ch")))
                  (p-line 115)
                  (p-if (p-str-eq $ch "#")
                    (progn (p-line 116)
                      (p-post++ $ti)
                      (p-line 117)
                      (p-while
                        (p-&& (p-< $ti $tlen) (p-str-ne (p-substr $s $ti 1) (p-esc "\\n")))
                        (p-line 117)
                        (p-post++ $ti)))
                    (p-if (p-str-eq $ch "(")
                      (progn (p-line 119)
                        (p-incf-raw $depth :numeric)
                        (p-line 119)
                        (p-post++ $ti))
                      (p-if (p-str-eq $ch ")")
                        (progn (p-line 121)
                          (p-decf-raw $depth :numeric)
                          (p-line 122)
                          (p-if (p-> $depth 0) (p-post++ $ti)))
                        (progn (p-line 124) (p-post++ $ti)))))))
              (p-line 127)
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
        (p-file "cl/pack-impl.pl")
        (p-void-ctx (p-line 133)
          (p-let (($tlen :num (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
            (p-line 134)
            (p-let
              (($bang :box (make-p-box nil))
                ($be :box (make-p-box nil))
                ($le :box (make-p-box nil)))
              (p-scalar-ctx (p-list-= (vector $bang $be $le) (vector 0 $inh_be $inh_le)))
              (p-line 135)
              (p-let (($got_be :scalar 0))
                (p-line 135)
                (p-let (($got_le :scalar 0))
                  (p-line 135)
                  (p-let (($got_bang :scalar 0))
                    (p-line 136)
                    (p-while (p-< (p-cast-$ $ti_ref :strict) $tlen)
                      (p-line 137)
                      (p-let
                        (($m :str
                            (%pcl-to-string-strict
                              (p-substr $tmpl (p-cast-$ $ti_ref :strict-lv) 1)
                              "$m")))
                        (p-line 138)
                        (p-if (p-str-eq $m "!")
                          (progn (p-line 139)
                            (p-if
                              (p-! (p->= (p-index $CAN_SHRIEK__file__3727705105595030001 $ch) 0))
                              (p-die :loc
                                "cl/pack-impl.pl line 139"
                                (p-string-concat "'!' allowed only after types "
                                  $CAN_SHRIEK__file__3727705105595030001
                                  " in "
                                  $ctx
                                  (p-esc "\\n"))))
                            (p-line 141)
                            (p-if $got_bang
                              (p-warn :loc
                                "cl/pack-impl.pl line 141"
                                (p-string-concat "Duplicate modifier '!' after '"
                                  $ch
                                  "' in "
                                  $ctx
                                  (p-esc "\\n"))))
                            (p-line 142)
                            (p-my-= $bang 1)
                            (p-line 142)
                            (setf $got_bang 1)
                            (p-line 142)
                            (p-post++ (p-cast-$ $ti_ref)))
                          (p-if (p-str-eq $m ">")
                            (progn (p-line 144)
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
                              (p-line 146)
                              (p-if $got_le
                                (p-die :loc
                                  "cl/pack-impl.pl line 146"
                                  (p-string-concat "Can't use both '<' and '>' after type '"
                                    $ch
                                    "' in "
                                    $ctx
                                    (p-esc "\\n"))))
                              (p-line 147)
                              (p-if $inh_le
                                (p-die :loc
                                  "cl/pack-impl.pl line 147"
                                  (p-string-concat
                                    "Can't use '>' in a group with different byte-order in "
                                    $ctx
                                    (p-esc "\\n"))))
                              (p-line 148)
                              (p-if $got_be
                                (p-warn :loc
                                  "cl/pack-impl.pl line 148"
                                  (p-string-concat "Duplicate modifier '>' after '"
                                    $ch
                                    "' in "
                                    $ctx
                                    (p-esc "\\n"))))
                              (p-line 149)
                              (p-my-= $be 1)
                              (p-line 149)
                              (p-my-= $le 0)
                              (p-line 149)
                              (setf $got_be 1)
                              (p-line 149)
                              (p-post++ (p-cast-$ $ti_ref)))
                            (p-if (p-str-eq $m "<")
                              (progn (p-line 151)
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
                                (p-line 153)
                                (p-if $got_be
                                  (p-die :loc
                                    "cl/pack-impl.pl line 153"
                                    (p-string-concat "Can't use both '<' and '>' after type '"
                                      $ch
                                      "' in "
                                      $ctx
                                      (p-esc "\\n"))))
                                (p-line 154)
                                (p-if $inh_be
                                  (p-die :loc
                                    "cl/pack-impl.pl line 154"
                                    (p-string-concat
                                      "Can't use '<' in a group with different byte-order in "
                                      $ctx
                                      (p-esc "\\n"))))
                                (p-line 155)
                                (p-if $got_le
                                  (p-warn :loc
                                    "cl/pack-impl.pl line 155"
                                    (p-string-concat "Duplicate modifier '<' after '"
                                      $ch
                                      "' in "
                                      $ctx
                                      (p-esc "\\n"))))
                                (p-line 156)
                                (p-my-= $le 1)
                                (p-line 156)
                                (p-my-= $be 0)
                                (p-line 156)
                                (setf $got_le 1)
                                (p-line 156)
                                (p-post++ (p-cast-$ $ti_ref)))
                              (progn (p-line 158) (p-last)))))))
                    (p-line 161)
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
        (p-file "cl/pack-impl.pl")
        (p-void-ctx (p-line 168)
          (p-let (($pos :box (make-p-box nil)))
            (p-my-= $pos 0)
            (p-line 169)
            (p-let (($ti :box (make-p-box nil)))
              (p-my-= $ti 0)
              (p-line 170)
              (p-let (($tlen :num (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
                (p-line 171)
                (p-while 1
                  (p-line 172)
                  (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                  (p-line 173)
                  (p-if (p->= $ti $tlen) (p-last))
                  (p-line 174)
                  (p-let (($ch :box (make-p-box nil)))
                    (p-my-= $ch (p-substr $tmpl $ti 1))
                    (p-line 174)
                    (p-post++ $ti)
                    (p-line 175)
                    (p-let (($grpbeg :box (make-p-box nil)))
                      (p-my-= $grpbeg (p-undef))
                      (p-line 175)
                      (p-let (($grpend :num (%pcl-to-number-strict (p-undef) "$grpend")))
                        (p-line 176)
                        (p-if (p-str-eq $ch "(")
                          (progn (p-line 177)
                            (setf $grpend
                              (%pcl-to-number-strict (pl-_pack_find_group_end $tmpl $ti)
                                "$grpend"))
                            (p-line 178)
                            (p-my-= $grpbeg $ti)
                            (p-line 178)
                            (p-my-= $ti (p-+ $grpend 1))))
                        (p-line 180)
                        (p-let (($bang :box (make-p-box nil)))
                          (p-my-= $bang 0)
                          (p-line 181)
                          (p-while
                            (p-&& (p-< $ti $tlen)
                              (p-scalar-ctx
                                (p-=~ (p-substr $tmpl $ti 1)
                                  (p-regex :pat "[!<>]" :flags "" :tier :native))))
                            (p-line 182)
                            (p-if (p-str-eq (p-substr $tmpl $ti 1) "!") (p-my-= $bang 1))
                            (p-line 183)
                            (p-post++ $ti))
                          (p-line 186)
                          (p-let
                            (($all :box (make-p-box nil))
                              ($count :box (make-p-box nil))
                              ($nrep :box (make-p-box nil)))
                            (p-scalar-ctx
                              (p-list-= (vector $all $count $nrep)
                                (p-list-ctx (pl-_pack_parse_count $tmpl (p-backslash $ti)))))
                            (p-line 187)
                            (p-if (p-! (p-&& (p-defined $nrep) (p->= $nrep 1)))
                              (p-my-= $nrep 1))
                            (p-line 188)
                            (p-if (p-defined $grpbeg)
                              (progn (p-line 189)
                                (p-let (($inner :box (make-p-box nil)))
                                  (p-my-= $inner
                                    (p-substr $tmpl $grpbeg (p-- $grpend $grpbeg)))
                                  (p-line 190)
                                  (p-incf $pos (p-* (pl-_pack_template_size $inner) $nrep))
                                  (p-line 191)
                                  (p-next))))
                            (p-line 193)
                            (p-if (p-str-eq $ch "@")
                              (progn (p-line 193)
                                (p-my-= $pos
                                  (p-if $bang
                                    (p-if (p-defined $count) $count 0)
                                    (p-+ 0 (p-if (p-defined $count) $count 0))))
                                (p-line 193)
                                (p-next)))
                            (p-line 195)
                            (p-if (p-str-eq $ch "x")
                              (progn (p-line 196)
                                (p-if $bang
                                  (progn (p-line 197)
                                    (p-let (($n :box (make-p-box nil)))
                                      (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                      (p-line 198)
                                      (p-incf $pos (p-% (p-- $n (p-% $pos $n)) $n))))
                                  (progn (p-line 199) (p-incf $pos $nrep)))
                                (p-line 200)
                                (p-next)))
                            (p-line 202)
                            (p-if (p-str-eq $ch "X")
                              (progn (p-line 203)
                                (p-if $bang
                                  (progn (p-line 204)
                                    (p-let (($n :box (make-p-box nil)))
                                      (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                      (p-line 205)
                                      (p-my-= $pos (p-* (p-int (p-/ $pos $n)) $n))))
                                  (progn (p-line 206)
                                    (p-decf $pos $nrep)
                                    (p-line 206)
                                    (p-if (p-< $pos 0) (p-my-= $pos 0))))
                                (p-line 207)
                                (p-next)))
                            (p-line 209)
                            (p-let (($nb :box (make-p-box nil)))
                              (p-scalar-ctx
                                (p-list-= (vector $nb)
                                  (p-list-ctx (pl-_pack_type_info $ch $bang))))
                              (p-line 210)
                              (p-if $nb
                                (progn (p-line 210)
                                  (p-incf $pos (p-* $nb $nrep))
                                  (p-line 210)
                                  (p-next)))
                              (p-line 211)
                              (p-if
                                (p-|| (p-|| (p-str-eq $ch "A") (p-str-eq $ch "a"))
                                  (p-str-eq $ch "Z"))
                                (progn (p-line 211) (p-incf $pos $nrep) (p-line 211) (p-next)))
                              (p-line 212)
                              (p-if (p-|| (p-str-eq $ch "B") (p-str-eq $ch "b"))
                                (progn (p-line 212)
                                  (p-incf $pos (p-int (p-/ (p-+ $nrep 7) 8)))
                                  (p-line 212)
                                  (p-next)))
                              (p-line 213)
                              (p-if (p-|| (p-str-eq $ch "H") (p-str-eq $ch "h"))
                                (progn (p-line 213)
                                  (p-incf $pos (p-int (p-/ (p-+ $nrep 1) 2)))
                                  (p-line 213)
                                  (p-next)))
                              (p-line 214)
                              (p-if (p-|| (p-str-eq $ch "f") (p-str-eq $ch "F"))
                                (progn (p-line 214)
                                  (p-incf $pos (p-* 4 $nrep))
                                  (p-line 214)
                                  (p-next)))
                              (p-line 215)
                              (p-if (p-|| (p-str-eq $ch "d") (p-str-eq $ch "D"))
                                (progn (p-line 215)
                                  (p-incf $pos (p-* 8 $nrep))
                                  (p-line 215)
                                  (p-next)))
                              (p-line 216)
                              (p-if (p-|| (p-str-eq $ch "p") (p-str-eq $ch "P"))
                                (progn (p-line 216)
                                  (p-incf $pos (p-* 8 $nrep))
                                  (p-line 216)
                                  (p-next)))
                              (p-line 217)
                              (p-if
                                (p-|| (p-|| (p-str-eq $ch "W") (p-str-eq $ch "U"))
                                  (p-str-eq $ch "w"))
                                (progn (p-line 217) (p-incf $pos $nrep) (p-line 217) (p-next)))
                              (p-line 218)
                              (p-if (p-str-eq $ch ".") (progn (p-line 218) (p-next))))))))))
                (p-line 221)
                (p-caller-ctx (p-tail-value $pos))))))))))

(p-sub pl-_pack_parse_count
  (&rest %_args)
  (:writes-args nil
    :needs (:nonlocal_exit.die :nonlocal_exit.return :regex.literal :regex.native))
  (p-args-body :copy (block nil
      (p-let (($tmpl :box (make-p-box nil)) ($ti_ref :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $tmpl $ti_ref) @_))
        (p-file "cl/pack-impl.pl")
        (p-void-ctx (p-line 227)
          (p-let (($tlen :num (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
            (p-line 230)
            (p-if
              (p-&& (p-< (p-cast-$ $ti_ref :strict) $tlen)
                (p-str-eq (p-substr $tmpl (p-cast-$ $ti_ref :strict-lv) 1) "*"))
              (progn (p-line 231)
                (p-post++ (p-cast-$ $ti_ref))
                (p-line 231)
                ;; return (1, undef, 1)
(p-return 1 (p-undef) 1)))
            (p-line 233)
            (p-if
              (p-&& (p-< (p-cast-$ $ti_ref :strict) $tlen)
                (p-str-eq (p-substr $tmpl (p-cast-$ $ti_ref :strict-lv) 1) "["))
              (progn (p-line 234)
                (p-post++ (p-cast-$ $ti_ref))
                (p-line 235)
                (p-let
                  (($start :num (%pcl-to-number-strict (p-cast-$ $ti_ref :strict) "$start")))
                  (p-line 236)
                  (p-let (($depth :scalar 1))
                    (p-line 237)
                    (p-while (p-&& (p-< (p-cast-$ $ti_ref :strict) $tlen) (p-> $depth 0))
                      (p-line 238)
                      (p-let
                        (($c :str
                            (%pcl-to-string-strict
                              (p-substr $tmpl (p-cast-$ $ti_ref :strict-lv) 1)
                              "$c")))
                        (p-line 238)
                        (p-post++ (p-cast-$ $ti_ref))
                        (p-line 239)
                        (p-if (p-str-eq $c "[")
                          (progn (p-line 239) (p-incf-raw $depth :numeric))
                          (p-if (p-str-eq $c "]")
                            (progn (p-line 240) (p-decf-raw $depth :numeric))))))
                    (p-line 242)
                    (p-if (p-> $depth 0)
                      (p-die :loc
                        "cl/pack-impl.pl line 242"
                        (p-esc "No group ending character ']' found in template\\n")))
                    (p-line 244)
                    (p-let (($inner :box (make-p-box nil)))
                      (p-my-= $inner
                        (p-substr $tmpl $start (p-- (p-- (p-cast-$ $ti_ref :strict) $start) 1)))
                      (p-line 245)
                      (p-if
                        (p-scalar-ctx
                          (p-=~ $inner (p-regex :pat "^\\d+$" :flags "" :tier :native)))
                        (progn (p-line 246)
                          (p-let (($n :box (make-p-box nil)))
                            (p-my-= $n (p-+ $inner 0))
                            (p-line 247)
                            ;; return (0, $n, $n)
(p-return 0 $n $n))))
                      (p-line 249)
                      (p-if (p->= (p-index $inner "@") 0)
                        (p-die :loc
                          "cl/pack-impl.pl line 249"
                          (p-esc "Within []-length '@' not allowed\\n")))
                      (p-line 250)
                      (p-if
                        (p-&&
                          (p-scalar-ctx
                            (p-=~ $inner (p-regex :pat "^\\d" :flags "" :tier :native)))
                          (p-scalar-ctx
                            (p-!~ $inner (p-regex :pat "^\\d+$" :flags "" :tier :native))))
                        (p-die :loc
                          "cl/pack-impl.pl line 250"
                          (p-esc "Malformed integer in []\\n")))
                      (p-line 251)
                      (p-let (($n :box (make-p-box nil)))
                        (p-my-= $n (pl-_pack_template_size $inner))
                        (p-line 252)
                        ;; return (0, $n, $n)
(p-return 0 $n $n)))))))
            (p-line 254)
            (p-if
              (p-&& (p-< (p-cast-$ $ti_ref :strict) $tlen)
                (p-scalar-ctx
                  (p-=~ (p-substr $tmpl (p-cast-$ $ti_ref :strict-lv) 1)
                    (p-regex :pat "\\d" :flags "" :tier :native))))
              (progn (p-line 255)
                (p-let (($n :box (make-p-box nil)))
                  (p-my-= $n 0)
                  (p-line 256)
                  (p-while
                    (p-&& (p-< (p-cast-$ $ti_ref :strict) $tlen)
                      (p-scalar-ctx
                        (p-=~ (p-substr $tmpl (p-cast-$ $ti_ref :strict-lv) 1)
                          (p-regex :pat "\\d" :flags "" :tier :native))))
                    (p-line 257)
                    (p-my-= $n
                      (p-+ (p-* $n 10) (p-substr $tmpl (p-cast-$ $ti_ref :strict-lv) 1)))
                    (p-line 257)
                    (p-post++ (p-cast-$ $ti_ref)))
                  (p-line 259)
                  ;; return (0, $n, $n)
(p-return 0 $n $n))))
            (p-line 261)
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
        (p-file "cl/pack-impl.pl")
        (p-void-ctx (p-line 267)
          (p-my-= $val (p-int (p-+ $val 0)))
          (p-line 268)
          (p-let (($result :scalar ""))
            (p-line 269)
            (p-if $be
              (progn (p-line 270)
                (p-let (($k :scalar (p-- $nbytes 1)))
                  (p-for ()
                    ((p->= $k 0))
                    ((p-decf-raw $k))
                    (p-line 271)
                    (p-.=-raw $result (p-chr (p-bit-and (p->> $val (p-* 8 $k)) #xFF))))))
              (progn (p-line 274)
                (p-let (($k :scalar 0))
                  (p-for ()
                    ((p-< $k $nbytes))
                    ((p-incf-raw $k :numeric))
                    (p-line 275)
                    (p-.=-raw $result (p-chr (p-bit-and (p->> $val (p-* 8 $k)) #xFF)))))))
            (p-line 278)
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
        (p-file "cl/pack-impl.pl")
        (p-void-ctx (p-line 291)
          (p-if (p-== $v 0) (p-return (p-chr 0)))
          (p-line 292)
          (p-let ((@bytes :array (make-array 0 :adjustable t :fill-pointer 0)))
            (p-line 293)
            (p-if (p-> $v 18446744073709551615)
              (progn (p-line 294)
                (p-if (p-! (p-defined $digits)) (p-my-= $digits (p-sprintf "%.0f" $v)))
                (p-line 295)
                (p-while 1
                  (p-line 296)
                  (p-let (($q :box (make-p-box nil)))
                    (p-my-= $q "")
                    (p-line 296)
                    (p-let (($r :scalar 0))
                      (p-line 297)
                      (p-foreach-raw
                        ($d (p-split (p-regex :pat "" :flags "" :tier :native) $digits))
                        :my
                        t
                        (p-line 298)
                        (setf $r (p-+ (p-* $r 10) $d))
                        (p-line 299)
                        (p-let (($qd :box (make-p-box nil)))
                          (p-my-= $qd (p-int (p-/ $r 128)))
                          (p-line 300)
                          (p-if (p-|| (p-length $q) $qd) (p-.= $q $qd))
                          (p-line 301)
                          (p-decf-raw $r (p-* $qd 128))))
                      (p-line 303)
                      (p-unshift @bytes $r)
                      (p-line 304)
                      (p-if (p-str-eq $q "") (p-last))
                      (p-line 305)
                      (p-my-= $digits $q)))))
              (progn (p-line 308)
                (p-while (p-> $v 0)
                  (p-line 308)
                  (p-unshift @bytes (p-bit-and $v #x7F))
                  (p-line 308)
                  (p->>= $v 7))))
            (p-line 310)
            (p-caller-ctx
              (p-tail-value
                (p-.
                  (p-join ""
                    (p-map (lambda ($_) (p-line 310) (p-chr (p-bit-or $_ #x80)))
                      (p-aslice @bytes (p-.. 0 (p-- (p-array-last-index @bytes) 1)))))
                  (p-chr (p-aref @bytes -1)))))))))))

(p-sub pl-_unpack_read_int
  (&rest %_args)
  (:wantarray-insensitive t :writes-args nil :needs ())
  (p-raw-params (($s :scalar) ($si :scalar) ($nbytes :scalar) ($be :scalar) ($signed :scalar))
    (block nil
      (p-file "cl/pack-impl.pl")
      (p-void-ctx (p-line 316)
        (p-let (($slen :num (%pcl-to-number-strict (p-length $s) "$slen")))
          (p-line 317)
          (p-let (($v :box (make-p-box nil)))
            (p-my-= $v 0)
            (p-line 318)
            (p-if $be
              (progn (p-line 319)
                (p-let (($k :scalar 0))
                  (p-for ()
                    ((p-< $k $nbytes))
                    ((p-incf-raw $k :numeric))
                    (p-line 320)
                    (p-my-= $v
                      (p-bit-or (p-<< $v 8)
                        (p-if (p-< (p-+ $si $k) $slen) (p-ord (p-substr $s (p-+ $si $k) 1)) 0))))))
              (progn (p-line 323)
                (p-let (($k :scalar (p-- $nbytes 1)))
                  (p-for ()
                    ((p->= $k 0))
                    ((p-decf-raw $k))
                    (p-line 324)
                    (p-my-= $v
                      (p-bit-or (p-<< $v 8)
                        (p-if (p-< (p-+ $si $k) $slen) (p-ord (p-substr $s (p-+ $si $k) 1)) 0)))))))
            (p-line 327)
            (p-if $signed
              (progn (p-line 335)
                (p-let (($max :scalar 1))
                  (p-line 336)
                  (p-foreach-range-raw ($_ 1 $nbytes)
                    (p-line 336)
                    (p-*=-raw $max 256 :numeric))
                  (p-line 337)
                  (p-if (p->= $v (p-/ $max 2)) (p-decf $v $max)))))
            (p-line 339)
            (p-caller-ctx (p-tail-value $v))))))))

(p-sub pl-_pack_float32
  (&rest %_args)
  (:returns :str :wantarray-insensitive t :writes-args nil :needs ())
  (p-raw-params (($val :scalar) ($be :scalar))
    (block nil (p-file "cl/pack-impl.pl") (p-line 343) (p-tail-value ""))))

(p-sub pl-_pack_float64
  (&rest %_args)
  (:returns :str :wantarray-insensitive t :writes-args nil :needs ())
  (p-raw-params (($val :scalar) ($be :scalar))
    (block nil (p-file "cl/pack-impl.pl") (p-line 344) (p-tail-value ""))))

(p-sub pl-_unpack_float32
  (&rest %_args)
  (:returns :num :wantarray-insensitive t :writes-args nil :needs ())
  (p-raw-params (($s :scalar) ($si :scalar) ($be :scalar))
    (block nil (p-file "cl/pack-impl.pl") (p-line 345) (p-tail-value 0.0))))

(p-sub pl-_unpack_float64
  (&rest %_args)
  (:returns :num :wantarray-insensitive t :writes-args nil :needs ())
  (p-raw-params (($s :scalar) ($si :scalar) ($be :scalar))
    (block nil (p-file "cl/pack-impl.pl") (p-line 346) (p-tail-value 0.0))))

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
        (p-file "cl/pack-impl.pl")
        (p-void-ctx (p-line 351)
          (p-if (p-! (p-defined $arg)) (p-my-= $arg ""))
          (p-line 352)
          (p-let (($slen :box (make-p-box nil)))
            (p-my-= $slen (p-length $arg))
            (p-line 353)
            (let ((--pcl-if-ret--0 nil))
              (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "a"))
                (setf --pcl-if-ret--0
                  (progn (p-line 354)
                    (p-let (($len :box (make-p-box nil)))
                      (p-my-= $len (p-if $star $slen $nrep))
                      (p-line 355)
                      (p-let (($k :scalar 0))
                        (p-for ()
                          ((p-< $k $len))
                          ((p-incf-raw $k :numeric))
                          (p-line 356)
                          (p-.= (p-cast-$ $result_ref :strict-lv)
                            (p-if (p-< $k $slen) (p-substr $arg $k 1) (p-chr 0))))))))
                (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "A"))
                  (setf --pcl-if-ret--0
                    (progn (p-line 359)
                      (p-let (($len :box (make-p-box nil)))
                        (p-my-= $len (p-if $star $slen $nrep))
                        (p-line 360)
                        (p-let (($k :scalar 0))
                          (p-for ()
                            ((p-< $k $len))
                            ((p-incf-raw $k :numeric))
                            (p-line 361)
                            (p-.= (p-cast-$ $result_ref :strict-lv)
                              (p-if (p-< $k $slen) (p-substr $arg $k 1) " ")))))))
                  (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "Z"))
                    (setf --pcl-if-ret--0
                      (progn (p-line 364)
                        (p-let (($len :box (make-p-box nil)))
                          (p-my-= $len (p-if $star (p-+ $slen 1) $nrep))
                          (p-line 365)
                          (let ((--pcl-if-ret--1 nil))
                            (p-if (setf --pcl-if-ret--1 (p-> $len 0))
                              (setf --pcl-if-ret--1
                                (progn (p-line 366)
                                  (p-let (($body :scalar (p-- $len 1)))
                                    (p-line 367)
                                    (p-let (($k :scalar 0))
                                      (p-for ()
                                        ((p-< $k $body))
                                        ((p-incf-raw $k :numeric))
                                        (p-line 368)
                                        (p-.= (p-cast-$ $result_ref :strict-lv)
                                          (p-if (p-< $k $slen) (p-substr $arg $k 1) (p-chr 0)))))
                                    (p-line 370)
                                    (p-caller-ctx
                                      (p-.= (p-cast-$ $result_ref :strict-lv) (p-chr 0))))))
                              nil)
                            --pcl-if-ret--1))))
                    (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "b"))
                      (setf --pcl-if-ret--0
                        (progn (p-line 373)
                          (p-let (($nbits :box (make-p-box nil)))
                            (p-my-= $nbits (p-if $star $slen $nrep))
                            (p-line 374)
                            (p-let (($bs :scalar 0))
                              (p-for ()
                                ((p-< $bs $nbits))
                                ((p-incf-raw $bs 8 :numeric))
                                (p-line 375)
                                (p-let (($byte :box (make-p-box nil)))
                                  (p-my-= $byte 0)
                                  (p-line 376)
                                  (p-let (($bit :scalar 0))
                                    (p-for ()
                                      ((p-&& (p-< $bit 8) (p-< (p-+ $bs $bit) $nbits)))
                                      ((p-incf-raw $bit :numeric))
                                      (p-line 377)
                                      (p-let (($idx :box (make-p-box nil)))
                                        (p-my-= $idx (p-+ $bs $bit))
                                        (p-line 378)
                                        (p-if
                                          (p-&& (p-< $idx $slen)
                                            (p-str-eq (p-substr $arg $idx 1) "1"))
                                          (p-bit-or= $byte (p-<< 1 $bit))))))
                                  (p-line 380)
                                  (p-.= (p-cast-$ $result_ref :strict-lv) (p-chr $byte))))))))
                      (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "B"))
                        (setf --pcl-if-ret--0
                          (progn (p-line 383)
                            (p-let (($nbits :box (make-p-box nil)))
                              (p-my-= $nbits (p-if $star $slen $nrep))
                              (p-line 384)
                              (p-let (($bs :scalar 0))
                                (p-for ()
                                  ((p-< $bs $nbits))
                                  ((p-incf-raw $bs 8 :numeric))
                                  (p-line 385)
                                  (p-let (($byte :box (make-p-box nil)))
                                    (p-my-= $byte 0)
                                    (p-line 386)
                                    (p-let (($bit :scalar 0))
                                      (p-for ()
                                        ((p-&& (p-< $bit 8) (p-< (p-+ $bs $bit) $nbits)))
                                        ((p-incf-raw $bit :numeric))
                                        (p-line 387)
                                        (p-let (($idx :box (make-p-box nil)))
                                          (p-my-= $idx (p-+ $bs $bit))
                                          (p-line 388)
                                          (p-if
                                            (p-&& (p-< $idx $slen)
                                              (p-str-eq (p-substr $arg $idx 1) "1"))
                                            (p-bit-or= $byte (p-<< 1 (p-- 7 $bit)))))))
                                    (p-line 390)
                                    (p-.= (p-cast-$ $result_ref :strict-lv) (p-chr $byte))))))))
                        (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "H"))
                          (setf --pcl-if-ret--0
                            (progn (p-line 393)
                              (p-let (($nyb :box (make-p-box nil)))
                                (p-my-= $nyb (p-if $star $slen $nrep))
                                (p-line 394)
                                (p-let (($k :scalar 0))
                                  (p-for ()
                                    ((p-< $k $nyb))
                                    ((p-incf-raw $k 2 :numeric))
                                    (p-line 395)
                                    (p-let (($hi :box (make-p-box nil)))
                                      (p-my-= $hi
                                        (p-if (p-< $k $slen) (p-hex (p-substr $arg $k 1)) 0))
                                      (p-line 396)
                                      (p-let (($lo :box (make-p-box nil)))
                                        (p-my-= $lo
                                          (p-if (p-< (p-+ $k 1) $slen)
                                            (p-hex (p-substr $arg (p-+ $k 1) 1))
                                            0))
                                        (p-line 397)
                                        (p-.= (p-cast-$ $result_ref :strict-lv)
                                          (p-chr (p-bit-or (p-<< $hi 4) $lo))))))))))
                          (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "h"))
                            (setf --pcl-if-ret--0
                              (progn (p-line 400)
                                (p-let (($nyb :box (make-p-box nil)))
                                  (p-my-= $nyb (p-if $star $slen $nrep))
                                  (p-line 401)
                                  (p-let (($k :scalar 0))
                                    (p-for ()
                                      ((p-< $k $nyb))
                                      ((p-incf-raw $k 2 :numeric))
                                      (p-line 402)
                                      (p-let (($lo :box (make-p-box nil)))
                                        (p-my-= $lo
                                          (p-if (p-< $k $slen) (p-hex (p-substr $arg $k 1)) 0))
                                        (p-line 403)
                                        (p-let (($hi :box (make-p-box nil)))
                                          (p-my-= $hi
                                            (p-if (p-< (p-+ $k 1) $slen)
                                              (p-hex (p-substr $arg (p-+ $k 1) 1))
                                              0))
                                          (p-line 404)
                                          (p-.= (p-cast-$ $result_ref :strict-lv)
                                            (p-chr (p-bit-or (p-<< $hi 4) $lo))))))))))
                            (p-if (setf --pcl-if-ret--0 (p-str-eq $ch "u"))
                              (setf --pcl-if-ret--0
                                (progn (p-line 407)
                                  (p-let
                                    (($line_len :num (%pcl-to-number-strict 45 "$line_len")))
                                    (p-line 408)
                                    (p-if (p-&& (p-! $star) (p-> $nrep 45))
                                      (progn (p-line 409)
                                        (p-if (p-> $nrep 63)
                                          (progn (p-line 410)
                                            (p-warn :loc
                                              "cl/pack-impl.pl line 410"
                                              "Field too wide in 'u' format in pack")
                                            (p-line 411)
                                            (setf $line_len
                                              (%pcl-to-number-strict 63 "$line_len")))
                                          (progn (p-line 413)
                                            (setf $line_len
                                              (%pcl-to-number-strict $nrep "$line_len"))))))
                                    (p-line 416)
                                    (p-let (($cs :scalar 0))
                                      (p-for ()
                                        ((p-< $cs $slen))
                                        ((p-incf-raw $cs $line_len))
                                        :dyn
                                        t
                                        (p-line 417)
                                        (p-let
                                          (($ce :num
                                              (%pcl-to-number-strict
                                                (p-if (p-< (p-+ $cs $line_len) $slen)
                                                  (p-+ $cs $line_len)
                                                  $slen)
                                                "$ce")))
                                          (p-line 418)
                                          (p-let (($chunk :box (make-p-box nil)))
                                            (p-my-= $chunk (p-substr $arg $cs (p-- $ce $cs)))
                                            (p-line 419)
                                            (p-let
                                              (($clen :num
                                                  (%pcl-to-number-strict (p-length $chunk)
                                                    "$clen")))
                                              (p-line 420)
                                              (p-.= (p-cast-$ $result_ref :strict-lv)
                                                (p-chr (p-+ 32 $clen)))
                                              (p-line 421)
                                              (p-let (($k :scalar 0))
                                                (p-for ()
                                                  ((p-< $k $clen))
                                                  ((p-incf-raw $k 3 :numeric))
                                                  :dyn
                                                  t
                                                  (p-line 422)
                                                  (p-let
                                                    (($b0 :num
                                                        (%pcl-to-number-strict
                                                          (p-ord (p-substr $chunk $k 1))
                                                          "$b0")))
                                                    (p-line 423)
                                                    (p-let
                                                      (($b1 :num
                                                          (%pcl-to-number-strict
                                                            (p-if (p-< (p-+ $k 1) $clen)
                                                              (p-ord
                                                                (p-substr $chunk (p-+ $k 1) 1))
                                                              0)
                                                            "$b1")))
                                                      (p-line 424)
                                                      (p-let (($b2 :box (make-p-box nil)))
                                                        (p-my-= $b2
                                                          (p-if (p-< (p-+ $k 2) $clen)
                                                            (p-ord
                                                              (p-substr $chunk (p-+ $k 2) 1))
                                                            0))
                                                        (p-line 425)
                                                        (p-let (($cm :box (make-p-box nil)))
                                                          (p-my-= $cm
                                                            (p-bit-or
                                                              (p-bit-or (p-<< $b0 16)
                                                                (p-<< $b1 8))
                                                              $b2))
                                                          (p-line 426)
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
                                                                        (p-file
                                                                          "cl/pack-impl.pl")
                                                                        (p-void-ctx (p-line 426)
                                                                          (p-let
                                                                            (($c :scalar
                                                                                (p-+ 32
                                                                                  (p-bit-and
                                                                                    (p-aref @_
                                                                                      0)
                                                                                    63))))
                                                                            (p-line 426)
                                                                            (p-caller-ctx
                                                                              (p-if (p-== $c 32)
                                                                                96
                                                                                $c))))))))))
                                                            (p-line 427)
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
                                              (p-line 430)
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
        (p-file "cl/pack-impl.pl")
        (p-void-ctx (p-line 438)
          (p-if (p-< $code #x80)
            (progn (p-line 438) (p-caller-ctx (p-.= (p-cast-$ $r :strict-lv) (p-chr $code))))
            (p-if (p-< $code #x800)
              (progn (p-line 439)
                (p-caller-ctx
                  (p-.= (p-cast-$ $r :strict-lv)
                    (p-. (p-chr (p-bit-or #xC0 (p->> $code 6)))
                      (p-chr (p-bit-or #x80 (p-bit-and $code #x3F)))))))
              (p-if (p-< $code #x10000)
                (progn (p-line 440)
                  (p-caller-ctx
                    (p-.= (p-cast-$ $r :strict-lv)
                      (p-.
                        (p-. (p-chr (p-bit-or #xE0 (p->> $code 12)))
                          (p-chr (p-bit-or #x80 (p-bit-and (p->> $code 6) #x3F))))
                        (p-chr (p-bit-or #x80 (p-bit-and $code #x3F)))))))
                (progn (p-line 441)
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
        (p-file "cl/pack-impl.pl")
        (p-void-ctx (p-line 449)
          (p-if (p-! (p-defined $out_base)) (p-my-= $out_base 0))
          (p-line 450)
          (p-if (p-! (p-defined $depth)) (p-my-= $depth 0))
          (p-line 451)
          (p-if (p-> $depth $MAX_GROUP_DEPTH__file__3727705105595030002)
            (p-die :loc
              "cl/pack-impl.pl line 451"
              (p-esc "Too deeply nested ()-groups in pack\\n")))
          (p-line 452)
          (p-let
            (($nargs :num
                (%pcl-to-number-strict (p-scalar (p-cast-@ $args_ref :strict)) "$nargs")))
            (p-line 453)
            (p-let (($ti :box (make-p-box nil)))
              (p-my-= $ti 0)
              (p-line 454)
              (p-let (($tlen :num (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
                (p-line 455)
                (p-while 1
                  (p-line 456)
                  (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                  (p-line 457)
                  (p-if (p->= $ti $tlen) (p-last))
                  (p-line 458)
                  (p-let (($ch :box (make-p-box nil)))
                    (p-my-= $ch (p-substr $tmpl $ti 1))
                    (p-line 458)
                    (p-post++ $ti)
                    (p-line 459)
                    (p-let (($grpbeg :box (make-p-box nil)))
                      (p-my-= $grpbeg (p-undef))
                      (p-line 459)
                      (p-let (($grpend :num (%pcl-to-number-strict (p-undef) "$grpend")))
                        (p-line 460)
                        (p-if (p-str-eq $ch "(")
                          (progn (p-line 461)
                            (setf $grpend
                              (%pcl-to-number-strict (pl-_pack_find_group_end $tmpl $ti)
                                "$grpend"))
                            (p-line 462)
                            (p-my-= $grpbeg $ti)
                            (p-line 462)
                            (p-my-= $ti (p-+ $grpend 1))
                            (p-line 462)
                            (p-my-= $ch "(")))
                        (p-line 464)
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
                          (p-line 466)
                          (p-let
                            (($ti_before_count :num
                                (%pcl-to-number-strict $ti "$ti_before_count")))
                            (p-line 467)
                            (p-let
                              (($star :box (make-p-box nil))
                                ($count :box (make-p-box nil))
                                ($nrep :box (make-p-box nil)))
                              (p-scalar-ctx
                                (p-list-= (vector $star $count $nrep)
                                  (p-list-ctx (pl-_pack_parse_count $tmpl (p-backslash $ti)))))
                              (p-line 468)
                              (p-let (($had_count :box (make-p-box nil)))
                                (p-my-= $had_count (p-|| $star (p-> $ti $ti_before_count)))
                                (p-line 469)
                                (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                (p-line 473)
                                (p-if
                                  (p-&& (p-< $ti $tlen) (p-str-eq (p-substr $tmpl $ti 1) "/"))
                                  (progn (p-line 474)
                                    (p-post++ $ti)
                                    (p-line 475)
                                    (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                    (p-line 475)
                                    (p-if (p->= $ti $tlen) (p-last))
                                    (p-line 475)
                                    (let ((*package* *package*))
                                      (block nil
                                        (tagbody :redo
                                          (p-line 477)
                                          (p-let (($c :box (make-p-box nil)))
                                            (p-my-= $c (p-substr $tmpl $ti 1))
                                            (p-line 478)
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
                                    (p-line 481)
                                    (p-let (($dfmt :box (make-p-box nil)))
                                      (p-my-= $dfmt (p-substr $tmpl $ti 1))
                                      (p-line 481)
                                      (p-post++ $ti)
                                      (p-line 482)
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
                                        (p-line 483)
                                        (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                        (p-line 484)
                                        (p-let
                                          (($ti_before_dcount :num
                                              (%pcl-to-number-strict $ti "$ti_before_dcount")))
                                          (p-line 485)
                                          (p-let
                                            (($dstar2 :box (make-p-box nil))
                                              ($dcnt2 :box (make-p-box nil))
                                              ($dnrep2 :box (make-p-box nil)))
                                            (p-scalar-ctx
                                              (p-list-= (vector $dstar2 $dcnt2 $dnrep2)
                                                (p-list-ctx
                                                  (pl-_pack_parse_count $tmpl
                                                    (p-backslash $ti)))))
                                            (p-line 486)
                                            (p-let
                                              (($had_dcount :str
                                                  (%pcl-to-string-strict
                                                    (p-|| $dstar2 (p-> $ti $ti_before_dcount))
                                                    "$had_dcount")))
                                              (p-line 487)
                                              (p-let
                                                (($dnb :box (make-p-box nil))
                                                  ($dsig :box (make-p-box nil))
                                                  ($ddbe :box (make-p-box nil)))
                                                (p-scalar-ctx
                                                  (p-list-= (vector $dnb $dsig $ddbe)
                                                    (p-list-ctx
                                                      (pl-_pack_type_info $dfmt $dbang))))
                                                (p-line 488)
                                                (p-let (($actual_count :box (make-p-box nil)))
                                                  (p-line 489)
                                                  (p-if
                                                    (p-||
                                                      (p-|| (p-str-eq $dfmt "a")
                                                        (p-str-eq $dfmt "A"))
                                                      (p-str-eq $dfmt "Z"))
                                                    (progn (p-line 491)
                                                      (p-let (($darg :box (make-p-box nil)))
                                                        (p-my-= $darg
                                                          (p-if
                                                            (p-< (p-cast-$ $ai_ref :strict)
                                                              $nargs)
                                                            (p-aref-deref $args_ref
                                                              (p-post++ (p-cast-$ $ai_ref))
                                                              :strict-lv)
                                                            ""))
                                                        (p-line 492)
                                                        (p-if (p-! (p-defined $darg))
                                                          (p-my-= $darg ""))
                                                        (p-line 493)
                                                        (p-let (($dlen :box (make-p-box nil)))
                                                          (p-my-= $dlen (p-length $darg))
                                                          (p-line 494)
                                                          (p-if (p-|| (p-! $had_dcount) $dstar2)
                                                            (progn (p-line 496)
                                                              (p-my-= $actual_count
                                                                (p-if (p-str-eq $dfmt "Z")
                                                                  (p-+ $dlen 1)
                                                                  $dlen)))
                                                            (progn (p-line 498)
                                                              (p-my-= $actual_count $dnrep2)))
                                                          (p-line 501)
                                                          (p-let
                                                            (($nb :box (make-p-box nil))
                                                              ($sig :box (make-p-box nil))
                                                              ($dbe :box (make-p-box nil)))
                                                            (p-scalar-ctx
                                                              (p-list-= (vector $nb $sig $dbe)
                                                                (p-list-ctx
                                                                  (pl-_pack_type_info $ch
                                                                    $bang))))
                                                            (p-line 502)
                                                            (p-if $nb
                                                              (progn (p-line 503)
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
                                                                (progn (p-line 505)
                                                                  (pl-_pack_str_one $ch
                                                                    (p-string-concat
                                                                      $actual_count)
                                                                    1
                                                                    0
                                                                    $result_ref))
                                                                (p-if (p-str-eq $ch "Z")
                                                                  (progn (p-line 507)
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
                                                                    (progn (p-line 509)
                                                                      (p-let
                                                                        (($v :box
                                                                            (make-p-box nil)))
                                                                        (p-my-= $v
                                                                          $actual_count)
                                                                        (p-line 510)
                                                                        (p-.=
                                                                          (p-cast-$ $result_ref
                                                                            :strict-lv)
                                                                          (p-scalar-ctx
                                                                            (pl-_pack_ber $v)))))))))
                                                            (p-line 513)
                                                            (pl-_pack_str_one $dfmt
                                                              $darg
                                                              $actual_count
                                                              0
                                                              $result_ref)))))
                                                    (p-if $dnb
                                                      (progn (p-line 516)
                                                        (p-let
                                                          (($remaining :box (make-p-box nil)))
                                                          (p-my-= $remaining
                                                            (p-- $nargs
                                                              (p-cast-$ $ai_ref :strict)))
                                                          (p-line 517)
                                                          (p-if (p-|| (p-! $had_dcount) $dstar2)
                                                            (progn (p-line 519)
                                                              (p-my-= $actual_count $remaining))
                                                            (progn (p-line 522)
                                                              (p-my-= $actual_count
                                                                (p-if (p-< $dnrep2 $remaining)
                                                                  $dnrep2
                                                                  $remaining))))
                                                          (p-line 525)
                                                          (p-let
                                                            (($nb :box (make-p-box nil))
                                                              ($sig :box (make-p-box nil))
                                                              ($dbe :box (make-p-box nil)))
                                                            (p-scalar-ctx
                                                              (p-list-= (vector $nb $sig $dbe)
                                                                (p-list-ctx
                                                                  (pl-_pack_type_info $ch
                                                                    $bang))))
                                                            (p-line 526)
                                                            (p-if $nb
                                                              (progn (p-line 527)
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
                                                                (progn (p-line 529)
                                                                  (pl-_pack_str_one $ch
                                                                    (p-string-concat
                                                                      $actual_count)
                                                                    1
                                                                    0
                                                                    $result_ref))
                                                                (p-if (p-str-eq $ch "Z")
                                                                  (progn (p-line 531)
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
                                                                    (progn (p-line 533)
                                                                      (p-let
                                                                        (($v :box
                                                                            (make-p-box nil)))
                                                                        (p-my-= $v
                                                                          $actual_count)
                                                                        (p-line 534)
                                                                        (p-.=
                                                                          (p-cast-$ $result_ref
                                                                            :strict-lv)
                                                                          (p-scalar-ctx
                                                                            (pl-_pack_ber $v)))))))))
                                                            (p-line 537)
                                                            (p-let
                                                              (($dbe_eff :box
                                                                  (make-p-box nil)))
                                                              (p-my-= $dbe_eff
                                                                (p-if $dbe2
                                                                  1
                                                                  (p-if $dle2 0 $ddbe)))
                                                              (p-line 538)
                                                              (p-let (($i :scalar 0))
                                                                (p-for ()
                                                                  ((p-&& (p-< $i $actual_count)
                                                                      (p-<
                                                                        (p-cast-$ $ai_ref
                                                                          :strict)
                                                                        $nargs)))
                                                                  ((p-incf-raw $i :numeric))
                                                                  (p-line 539)
                                                                  (p-let
                                                                    (($val :box
                                                                        (make-p-box nil)))
                                                                    (p-my-= $val
                                                                      (p-aref-deref $args_ref
                                                                        (p-post++
                                                                          (p-cast-$ $ai_ref))
                                                                        :strict-lv))
                                                                    (p-line 540)
                                                                    (p-.=
                                                                      (p-cast-$ $result_ref
                                                                        :strict-lv)
                                                                      (pl-_pack_emit_int $val
                                                                        $dnb
                                                                        $dsig
                                                                        $dbe_eff)))))))))
                                                      (p-if (p-str-eq $dfmt "w")
                                                        (progn (p-line 544)
                                                          (p-let
                                                            (($remaining :box
                                                                (make-p-box nil)))
                                                            (p-my-= $remaining
                                                              (p-- $nargs
                                                                (p-cast-$ $ai_ref :strict)))
                                                            (p-line 545)
                                                            (p-my-= $actual_count
                                                              (p-if
                                                                (p-|| (p-! $had_dcount)
                                                                  $dstar2)
                                                                $remaining
                                                                (p-if (p-< $dnrep2 $remaining)
                                                                  $dnrep2
                                                                  $remaining)))
                                                            (p-line 548)
                                                            (p-let
                                                              (($nb :box (make-p-box nil))
                                                                ($sig :box (make-p-box nil))
                                                                ($dbe :box (make-p-box nil)))
                                                              (p-scalar-ctx
                                                                (p-list-= (vector $nb $sig $dbe)
                                                                  (p-list-ctx
                                                                    (pl-_pack_type_info $ch
                                                                      $bang))))
                                                              (p-line 549)
                                                              (p-if $nb
                                                                (progn (p-line 550)
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
                                                                  (progn (p-line 552)
                                                                    (p-let
                                                                      (($v :box
                                                                          (make-p-box nil)))
                                                                      (p-my-= $v $actual_count)
                                                                      (p-line 553)
                                                                      (p-.=
                                                                        (p-cast-$ $result_ref
                                                                          :strict-lv)
                                                                        (p-scalar-ctx
                                                                          (pl-_pack_ber $v)))))))
                                                              (p-line 556)
                                                              (p-let (($i :scalar 0))
                                                                (p-for ()
                                                                  ((p-&& (p-< $i $actual_count)
                                                                      (p-<
                                                                        (p-cast-$ $ai_ref
                                                                          :strict)
                                                                        $nargs)))
                                                                  ((p-incf-raw $i :numeric))
                                                                  (p-line 557)
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
                                                                    (p-line 558)
                                                                    (p-.=
                                                                      (p-cast-$ $result_ref
                                                                        :strict-lv)
                                                                      (p-scalar-ctx
                                                                        (pl-_pack_ber $v))))))))))))
                                                  (p-line 561)
                                                  (p-next))))))))))
                                (p-line 565)
                                (p-if (p-defined $grpbeg)
                                  (progn (p-line 566)
                                    (p-let (($inner :box (make-p-box nil)))
                                      (p-my-= $inner
                                        (p-substr $tmpl $grpbeg (p-- $grpend $grpbeg)))
                                      (p-line 568)
                                      (p-let
                                        (($gti :num
                                            (%pcl-to-number-strict (pl-_pack_skip_ws $inner 0)
                                              "$gti")))
                                        (p-line 569)
                                        (p-if (p-< $gti (p-length $inner))
                                          (progn (p-line 570)
                                            (p-let (($fc :box (make-p-box nil)))
                                              (p-my-= $fc (p-substr $inner $gti 1))
                                              (p-line 571)
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
                                        (p-line 573)
                                        (p-if $star
                                          (progn (p-line 574)
                                            (p-while (p-< (p-cast-$ $ai_ref :strict) $nargs)
                                              (p-line 575)
                                              (p-let
                                                (($ai_before :num
                                                    (%pcl-to-number-strict
                                                      (p-cast-$ $ai_ref :strict)
                                                      "$ai_before")))
                                                (p-line 576)
                                                (p-let (($iter_base :box (make-p-box nil)))
                                                  (p-my-= $iter_base
                                                    (p-length (p-cast-$ $result_ref :strict-lv)))
                                                  (p-line 577)
                                                  (pl-_pack_tmpl $inner
                                                    $ai_ref
                                                    $args_ref
                                                    $result_ref
                                                    $be
                                                    $le
                                                    $iter_base
                                                    (p-+ $depth 1))
                                                  (p-line 578)
                                                  (p-if
                                                    (p-== (p-cast-$ $ai_ref :strict)
                                                      $ai_before)
                                                    (p-last))))))
                                          (progn (p-line 581)
                                            (p-let (($r :scalar 0))
                                              (p-for ()
                                                ((p-< $r $nrep))
                                                ((p-incf-raw $r :numeric))
                                                (p-line 582)
                                                (p-let (($iter_base :box (make-p-box nil)))
                                                  (p-my-= $iter_base
                                                    (p-length (p-cast-$ $result_ref :strict-lv)))
                                                  (p-line 583)
                                                  (pl-_pack_tmpl $inner
                                                    $ai_ref
                                                    $args_ref
                                                    $result_ref
                                                    $be
                                                    $le
                                                    $iter_base
                                                    (p-+ $depth 1)))))))
                                        (p-line 586)
                                        (p-next)))))
                                (p-line 590)
                                (p-if (p-str-eq $ch "x")
                                  (progn (p-line 591)
                                    (p-if $bang
                                      (progn (p-line 593)
                                        (p-let (($n :box (make-p-box nil)))
                                          (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                          (p-line 594)
                                          (p-let (($cur :box (make-p-box nil)))
                                            (p-my-= $cur
                                              (p-length (p-cast-$ $result_ref :strict-lv)))
                                            (p-line 595)
                                            (p-let
                                              (($pad :scalar (p-% (p-- $n (p-% $cur $n)) $n)))
                                              (p-line 596)
                                              (p-.= (p-cast-$ $result_ref :strict-lv)
                                                (p-str-x (p-chr 0) $pad))))))
                                      (progn (p-line 598)
                                        (p-.= (p-cast-$ $result_ref :strict-lv)
                                          (p-str-x (p-chr 0) $nrep))))
                                    (p-line 600)
                                    (p-next)))
                                (p-line 602)
                                (p-if (p-str-eq $ch "X")
                                  (progn (p-line 603)
                                    (p-if $bang
                                      (progn (p-line 605)
                                        (p-let (($n :box (make-p-box nil)))
                                          (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                          (p-line 606)
                                          (p-let (($cur :box (make-p-box nil)))
                                            (p-my-= $cur
                                              (p-length (p-cast-$ $result_ref :strict-lv)))
                                            (p-line 607)
                                            (p-setf (p-cast-$ $result_ref :strict-lv)
                                              (p-substr (p-cast-$ $result_ref :strict-lv)
                                                0
                                                (p-* (p-int (p-/ $cur $n)) $n))))))
                                      (progn (p-line 609)
                                        (p-let
                                          (($fp :scalar
                                              (p-- (p-length (p-cast-$ $result_ref :strict-lv))
                                                $nrep)))
                                          (p-line 610)
                                          (p-setf (p-cast-$ $result_ref :strict-lv)
                                            (p-substr (p-cast-$ $result_ref :strict-lv)
                                              0
                                              (p-if (p-< $fp 0) 0 $fp))))))
                                    (p-line 612)
                                    (p-next)))
                                (p-line 614)
                                (p-if (p-str-eq $ch "@")
                                  (progn (p-line 616)
                                    (p-let (($n :box (make-p-box nil)))
                                      (p-my-= $n (p-if (p-defined $count) $count 0))
                                      (p-line 617)
                                      (p-let
                                        (($t :num
                                            (%pcl-to-number-strict
                                              (p-if $bang $n (p-+ $out_base $n))
                                              "$t")))
                                        (p-line 618)
                                        (p-if
                                          (p-< (p-length (p-cast-$ $result_ref :strict-lv)) $t)
                                          (progn (p-line 618)
                                            (p-.= (p-cast-$ $result_ref :strict-lv)
                                              (p-str-x (p-chr 0)
                                                (p-- $t
                                                  (p-length (p-cast-$ $result_ref :strict-lv))))))
                                          (p-if
                                            (p-> (p-length (p-cast-$ $result_ref :strict-lv))
                                              $t)
                                            (progn (p-line 619)
                                              (p-setf (p-cast-$ $result_ref :strict-lv)
                                                (p-substr (p-cast-$ $result_ref :strict-lv)
                                                  0
                                                  $t)))))
                                        (p-line 620)
                                        (p-next)))))
                                (p-line 622)
                                (p-if (p-str-eq $ch ".")
                                  (progn (p-line 625)
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
                                      (p-line 626)
                                      (p-let
                                        (($abs_tgt :num
                                            (%pcl-to-number-strict
                                              (p-if $star $tgt (p-+ $out_base $tgt))
                                              "$abs_tgt")))
                                        (p-line 627)
                                        (p-let (($cur :box (make-p-box nil)))
                                          (p-my-= $cur
                                            (p-length (p-cast-$ $result_ref :strict-lv)))
                                          (p-line 628)
                                          (p-if (p-< $cur $abs_tgt)
                                            (progn (p-line 628)
                                              (p-.= (p-cast-$ $result_ref :strict-lv)
                                                (p-str-x (p-chr 0) (p-- $abs_tgt $cur))))
                                            (p-if (p-> $cur $abs_tgt)
                                              (progn (p-line 629)
                                                (p-setf (p-cast-$ $result_ref :strict-lv)
                                                  (p-substr (p-cast-$ $result_ref :strict-lv)
                                                    0
                                                    $abs_tgt)))))
                                          (p-line 630)
                                          (p-next))))))
                                (p-line 632)
                                (p-if
                                  (p-|| (p-|| (p-str-eq $ch "p") (p-str-eq $ch "P"))
                                    (p-str-eq $ch "D"))
                                  (progn (p-line 633)
                                    (p-die :loc
                                      "cl/pack-impl.pl line 633"
                                      (p-string-concat "Invalid type '"
                                        $ch
                                        (p-esc "' in pack\\n")))))
                                (p-line 637)
                                (p-if $star
                                  (p-my-= $nrep (p-- $nargs (p-cast-$ $ai_ref :strict))))
                                (p-line 640)
                                (p-let
                                  (($nb :box (make-p-box nil))
                                    ($sig :box (make-p-box nil))
                                    ($dbe :box (make-p-box nil)))
                                  (p-scalar-ctx
                                    (p-list-= (vector $nb $sig $dbe)
                                      (p-list-ctx (pl-_pack_type_info $ch $bang))))
                                  (p-line 641)
                                  (p-if $nb
                                    (progn (p-line 642)
                                      (p-let (($be2 :box (make-p-box nil)))
                                        (p-my-= $be2 (p-if $be 1 (p-if $le 0 $dbe)))
                                        (p-line 643)
                                        (p-let (($r :scalar 0))
                                          (p-for ()
                                            ((p-< $r $nrep))
                                            ((p-incf-raw $r :numeric))
                                            (p-line 644)
                                            (p-let (($v :box (make-p-box nil)))
                                              (p-my-= $v
                                                (p-if (p-< (p-cast-$ $ai_ref :strict) $nargs)
                                                  (p-//
                                                    (p-aref-deref $args_ref
                                                      (p-post++ (p-cast-$ $ai_ref))
                                                      :strict-lv)
                                                    0)
                                                  0))
                                              (p-line 645)
                                              (p-let (($nv :box (make-p-box nil)))
                                                (p-my-= $nv (p-+ $v 0))
                                                (p-line 646)
                                                (p-if (p-!= $nv $nv)
                                                  (p-die :loc
                                                    "cl/pack-impl.pl line 646"
                                                    (p-esc "Cannot pack NaN in pack\\n")))
                                                (p-line 647)
                                                (p-if (p-&& (p-!= $nv 0) (p-== $nv (p-* $nv 2)))
                                                  (p-die :loc
                                                    "cl/pack-impl.pl line 647"
                                                    (p-.
                                                      (p-. "Cannot pack "
                                                        (p-if (p-< $nv 0) "-Inf" "Inf"))
                                                      (p-esc " in pack\\n"))))
                                                (p-line 648)
                                                (p-.= (p-cast-$ $result_ref :strict-lv)
                                                  (pl-_pack_emit_int (p-int $nv) $nb $sig $be2))))))
                                        (p-line 650)
                                        (p-next))))
                                  (p-line 654)
                                  (p-if (p-str-eq $ch "f")
                                    (progn (p-line 655)
                                      (p-let (($be2 :box (make-p-box nil)))
                                        (p-my-= $be2 (p-if $be 1 (p-if $le 0 0)))
                                        (p-line 656)
                                        (p-let (($r :scalar 0))
                                          (p-for ()
                                            ((p-< $r $nrep))
                                            ((p-incf-raw $r :numeric))
                                            (p-line 657)
                                            (p-let (($v :box (make-p-box nil)))
                                              (p-my-= $v
                                                (p-if (p-< (p-cast-$ $ai_ref :strict) $nargs)
                                                  (p-//
                                                    (p-aref-deref $args_ref
                                                      (p-post++ (p-cast-$ $ai_ref))
                                                      :strict-lv)
                                                    0)
                                                  0))
                                              (p-line 658)
                                              (p-.= (p-cast-$ $result_ref :strict-lv)
                                                (pl-_pack_float32 $v $be2)))))
                                        (p-line 660)
                                        (p-next))))
                                  (p-line 662)
                                  (p-if (p-|| (p-str-eq $ch "d") (p-str-eq $ch "F"))
                                    (progn (p-line 663)
                                      (p-let (($be2 :box (make-p-box nil)))
                                        (p-my-= $be2 (p-if $be 1 (p-if $le 0 0)))
                                        (p-line 664)
                                        (p-let (($r :scalar 0))
                                          (p-for ()
                                            ((p-< $r $nrep))
                                            ((p-incf-raw $r :numeric))
                                            (p-line 665)
                                            (p-let (($v :box (make-p-box nil)))
                                              (p-my-= $v
                                                (p-if (p-< (p-cast-$ $ai_ref :strict) $nargs)
                                                  (p-//
                                                    (p-aref-deref $args_ref
                                                      (p-post++ (p-cast-$ $ai_ref))
                                                      :strict-lv)
                                                    0)
                                                  0))
                                              (p-line 666)
                                              (p-.= (p-cast-$ $result_ref :strict-lv)
                                                (pl-_pack_float64 $v $be2)))))
                                        (p-line 668)
                                        (p-next))))
                                  (p-line 672)
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
                                    (progn (p-line 674)
                                      (p-let (($arg :box (make-p-box nil)))
                                        (p-my-= $arg
                                          (p-if (p-< (p-cast-$ $ai_ref :strict) $nargs)
                                            (p-//
                                              (p-aref-deref $args_ref
                                                (p-post++ (p-cast-$ $ai_ref))
                                                :strict-lv)
                                              "")
                                            ""))
                                        (p-line 675)
                                        (pl-_pack_str_one $ch $arg $nrep $star $result_ref)
                                        (p-line 676)
                                        (p-next))))
                                  (p-line 680)
                                  (p-if (p-str-eq $ch "U")
                                    (progn (p-line 681)
                                      (p-let (($r :scalar 0))
                                        (p-for ()
                                          ((p-< $r $nrep))
                                          ((p-incf-raw $r :numeric))
                                          (p-line 682)
                                          (p-let (($v :box (make-p-box nil)))
                                            (p-my-= $v
                                              (p-if (p-< (p-cast-$ $ai_ref :strict) $nargs)
                                                (p-//
                                                  (p-aref-deref $args_ref
                                                    (p-post++ (p-cast-$ $ai_ref))
                                                    :strict-lv)
                                                  0)
                                                0))
                                            (p-line 683)
                                            (p-let (($nv :box (make-p-box nil)))
                                              (p-my-= $nv (p-+ $v 0))
                                              (p-line 684)
                                              (p-if (p-!= $nv $nv)
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 684"
                                                  (p-esc "Cannot pack NaN in pack\\n")))
                                              (p-line 685)
                                              (p-if (p-&& (p-!= $nv 0) (p-== $nv (p-* $nv 2)))
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 685"
                                                  (p-.
                                                    (p-. "Cannot pack "
                                                      (p-if (p-< $nv 0) "-Inf" "Inf"))
                                                    (p-esc " in pack\\n"))))
                                              (p-line 686)
                                              (pl-_pack_utf8_char (p-int $nv) $result_ref)))))
                                      (p-line 688)
                                      (p-next)))
                                  (p-line 690)
                                  (p-if (p-str-eq $ch "W")
                                    (progn (p-line 691)
                                      (p-let (($r :scalar 0))
                                        (p-for ()
                                          ((p-< $r $nrep))
                                          ((p-incf-raw $r :numeric))
                                          (p-line 692)
                                          (p-let (($v :box (make-p-box nil)))
                                            (p-my-= $v
                                              (p-if (p-< (p-cast-$ $ai_ref :strict) $nargs)
                                                (p-//
                                                  (p-aref-deref $args_ref
                                                    (p-post++ (p-cast-$ $ai_ref))
                                                    :strict-lv)
                                                  0)
                                                0))
                                            (p-line 693)
                                            (p-let (($nv :box (make-p-box nil)))
                                              (p-my-= $nv (p-+ $v 0))
                                              (p-line 694)
                                              (p-if (p-!= $nv $nv)
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 694"
                                                  (p-esc "Cannot pack NaN in pack\\n")))
                                              (p-line 695)
                                              (p-if (p-&& (p-!= $nv 0) (p-== $nv (p-* $nv 2)))
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 695"
                                                  (p-.
                                                    (p-. "Cannot pack "
                                                      (p-if (p-< $nv 0) "-Inf" "Inf"))
                                                    (p-esc " in pack\\n"))))
                                              (p-line 696)
                                              (p-.= (p-cast-$ $result_ref :strict-lv)
                                                (p-chr (p-int $nv)))))))
                                      (p-line 698)
                                      (p-next)))
                                  (p-line 700)
                                  (p-if (p-str-eq $ch "w")
                                    (progn (p-line 701)
                                      (p-let (($r :scalar 0))
                                        (p-for ()
                                          ((p-< $r $nrep))
                                          ((p-incf-raw $r :numeric))
                                          (p-line 702)
                                          (p-let (($raw :box (make-p-box nil)))
                                            (p-my-= $raw
                                              (p-if (p-< (p-cast-$ $ai_ref :strict) $nargs)
                                                (p-//
                                                  (p-aref-deref $args_ref
                                                    (p-post++ (p-cast-$ $ai_ref))
                                                    :strict-lv)
                                                  0)
                                                0))
                                            (p-line 703)
                                            (p-let (($orig_s :box (make-p-box nil)))
                                              (p-my-= $orig_s (p-string-concat $raw))
                                              (p-line 704)
                                              (p-let (($v :box (make-p-box nil)))
                                                (p-my-= $v (p-+ $raw 0))
                                                (p-line 705)
                                                (p-if (p-!= $v $v)
                                                  (p-die :loc
                                                    "cl/pack-impl.pl line 705"
                                                    (p-esc "Cannot compress NaN in pack\\n")))
                                                (p-line 706)
                                                (p-if (p-&& (p-< $v 0) (p-== $v (p-* $v 2)))
                                                  (p-die :loc
                                                    "cl/pack-impl.pl line 706"
                                                    (p-esc "Cannot compress -Inf in pack\\n")))
                                                (p-line 707)
                                                (p-if (p-< $v 0)
                                                  (p-die :loc
                                                    "cl/pack-impl.pl line 707"
                                                    (p-esc
                                                      "Cannot compress negative numbers in pack\\n")))
                                                (p-line 708)
                                                (p-if (p-&& (p-!= $v 0) (p-== $v (p-* $v 2)))
                                                  (p-die :loc
                                                    "cl/pack-impl.pl line 708"
                                                    (p-esc "Cannot compress Inf in pack\\n")))
                                                (p-line 709)
                                                (p-if (p-!= $v (p-int $v))
                                                  (p-die :loc
                                                    "cl/pack-impl.pl line 709"
                                                    (p-esc
                                                      "Can only compress unsigned integers in pack\\n")))
                                                (p-line 715)
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
                                                (p-line 717)
                                                (p-my-= $v (p-int $v))
                                                (p-line 718)
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
                                      (p-line 720)
                                      (p-next)))
                                  (p-line 723)
                                  (p-if (p-str-eq $ch "/")
                                    (p-die :loc
                                      "cl/pack-impl.pl line 723"
                                      (p-esc "Invalid type '/' in pack\\n")))
                                  (p-line 724)
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
      (p-file "cl/pack-impl.pl")
      (p-void-ctx (p-line 730)
        (p-let (($n_open :scalar 0))
          (p-line 730)
          (p-let (($n_close :scalar 0))
            (p-line 731)
            (p-let (($tlen :num (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
              (p-line 732)
              (p-let (($i :scalar 0))
                (p-for ()
                  ((p-< $i $tlen))
                  ((p-incf-raw $i :numeric))
                  (p-line 733)
                  (p-let (($c :box (make-p-box nil)))
                    (p-my-= $c (p-substr $tmpl $i 1))
                    (p-line 734)
                    (p-if (p-str-eq $c "[")
                      (progn (p-line 734) (p-incf-raw $n_open :numeric))
                      (p-if (p-str-eq $c "]")
                        (progn (p-line 735) (p-incf-raw $n_close :numeric)))))))
              (p-line 737)
              (p-if (p-> $n_open $n_close)
                (p-die :loc
                  "cl/pack-impl.pl line 737"
                  (p-esc "No group ending character ']' found in template\\n")))
              (p-line 738)
              (p-if (p-! (p-> $n_open 0)) (p-return))
              (p-line 740)
              (p-let ((@stk :array (make-array 0 :adjustable t :fill-pointer 0)))
                (p-array-= @stk (vector))
                (p-line 741)
                (p-let (($i :scalar 0))
                  (p-for ()
                    ((p-< $i $tlen))
                    ((p-incf-raw $i :numeric))
                    (p-line 742)
                    (p-let (($c :box (make-p-box nil)))
                      (p-my-= $c (p-substr $tmpl $i 1))
                      (p-line 743)
                      (p-if (p-str-eq $c "[")
                        (progn (p-line 743) (%p-push1 @stk "["))
                        (p-if (p-str-eq $c "(")
                          (progn (p-line 744) (%p-push1 @stk "("))
                          (p-if (p-str-eq $c "]")
                            (progn (p-line 746)
                              (p-if (p-|| (p-! @stk) (p-str-ne (p-aref @stk -1) "["))
                                (p-die :loc
                                  "cl/pack-impl.pl line 746"
                                  (p-esc "Mismatched brackets in template\\n")))
                              (p-line 748)
                              (p-pop @stk))
                            (p-if (p-str-eq $c ")")
                              (progn (p-line 751)
                                (p-if (p-&& @stk (p-str-eq (p-aref @stk -1) "(")) (p-pop @stk))))))))))))))))))

(p-sub pl-p_pack
  (&rest %_args)
  (:wantarray-insensitive t
    :writes-args nil
    :needs (:dynamic_scope.local :regex.literal :regex.native))
  (p-args-body :copy (block nil
      (p-file "cl/pack-impl.pl")
      (p-void-ctx (p-line 763)
        (p-let
          (($tmpl :box (make-p-box nil))
            (@args :array (make-array 0 :adjustable t :fill-pointer 0)))
          (p-scalar-ctx (p-list-= (vector $tmpl @args) @_))
          (p-line 764)
          (p-local-cell $pcl_pack_comma_warned (p-box-for-local 0)
            (p-line 765)
            (pl-_pack_check_brackets $tmpl)
            (p-line 766)
            (p-let (($result :box (make-p-box nil)))
              (p-my-= $result "")
              (p-line 767)
              (p-let (($ai :box (make-p-box nil)))
                (p-my-= $ai 0)
                (p-line 768)
                (pl-_pack_tmpl $tmpl
                  (p-backslash $ai)
                  (p-backslash @args)
                  (p-backslash $result)
                  0
                  0)
                (p-line 772)
                (p-let (($c0 :str (%pcl-to-string-strict (p-substr $tmpl 0 1) "$c0")))
                  (p-line 773)
                  (p-if
                    (p-|| (p-str-eq $c0 "U")
                      (p-&& (p-str-le $c0 " ")
                        (p-scalar-ctx
                          (p-=~ $tmpl
                            (p-regex :pat "\\A[ \\t\\n\\r\\f]+U" :flags "" :tier :native)))))
                    (utf8::pl-decode $result))
                  (p-line 775)
                  (p-caller-ctx (p-tail-value $result)))))))))))

(p-sub pl-_unpack_utf8_char
  (&rest %_args)
  (:wantarray-insensitive t :writes-args nil :needs (:nonlocal_exit.return))
  (p-args-body
    (block nil
      (p-let (($s :box (make-p-box nil)) ($si_ref :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $s $si_ref) @_))
        (p-file "cl/pack-impl.pl")
        (p-void-ctx (p-line 781)
          (p-let (($slen :num (%pcl-to-number-strict (p-length $s) "$slen")))
            (p-line 782)
            (p-if (p->= (p-cast-$ $si_ref :strict) $slen) (p-return 0))
            (p-line 783)
            (p-let (($b0 :box (make-p-box nil)))
              (p-my-= $b0 (p-ord (p-substr $s (p-cast-$ $si_ref :strict-lv) 1)))
              (p-line 784)
              (p-let (($nb :box (make-p-box nil)) ($code :box (make-p-box nil)))
                (p-line 785)
                (p-if (p-< $b0 #x80)
                  (progn (p-line 785) (setf $nb 1) (p-line 785) (p-my-= $code $b0))
                  (p-if (p-< $b0 #xE0)
                    (progn (p-line 786)
                      (setf $nb 2)
                      (p-line 786)
                      (p-my-= $code (p-bit-and $b0 #x1F)))
                    (p-if (p-< $b0 #xF0)
                      (progn (p-line 787)
                        (setf $nb 3)
                        (p-line 787)
                        (p-my-= $code (p-bit-and $b0 #x0F)))
                      (progn (p-line 788)
                        (setf $nb 4)
                        (p-line 788)
                        (p-my-= $code (p-bit-and $b0 #x07))))))
                (p-line 789)
                (p-let (($k :scalar 1))
                  (p-for ()
                    ((p-< $k $nb))
                    ((p-incf-raw $k :numeric))
                    (p-line 790)
                    (p-if (p-< (p-+ (p-cast-$ $si_ref :strict) $k) $slen)
                      (p-my-= $code
                        (p-bit-or (p-<< $code 6)
                          (p-bit-and (p-ord (p-substr $s (p-+ (p-cast-$ $si_ref :strict) $k) 1))
                            #x3F))))))
                (p-line 792)
                (p-incf (p-cast-$ $si_ref :strict-lv) $nb)
                (p-line 793)
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
        (p-file "cl/pack-impl.pl")
        (p-void-ctx (p-line 799)
          (p-let (($slen :box (make-p-box nil) :captured t))
            (p-my-= $slen (p-length $s))
            (p-line 800)
            (let ((--pcl-if-ret--2 nil))
              (p-if
                (setf --pcl-if-ret--2
                  (p-|| (p-|| (p-str-eq $ch "A") (p-str-eq $ch "a")) (p-str-eq $ch "Z")))
                (setf --pcl-if-ret--2
                  (progn (p-line 801)
                    (p-let (($n :box (make-p-box nil)))
                      (p-my-= $n (p-if $all (p-- $slen (p-cast-$ $si_ref :strict)) $nrep))
                      (p-line 802)
                      (p-if (p-< $n 0) (p-my-= $n 0))
                      (p-line 803)
                      (p-let (($raw :box (make-p-box nil)))
                        (p-my-= $raw
                          (p-if (p-< (p-cast-$ $si_ref :strict) $slen)
                            (p-substr $s (p-cast-$ $si_ref :strict-lv) $n)
                            ""))
                        (p-line 804)
                        (p-incf (p-cast-$ $si_ref :strict-lv) $n)
                        (p-line 805)
                        (p-if (p-str-eq $ch "A")
                          (p-=~ $raw
                            (p-subst :pat "[ \\x00]+$" :rep "" :flags "" :tier :native)))
                        (p-line 806)
                        (p-if (p-str-eq $ch "Z")
                          (p-=~ $raw (p-subst :pat "\\x00.*" :rep "" :flags "s" :tier :native)))
                        (p-line 807)
                        (p-caller-ctx (p-funcall-ref $push_val $raw))))))
                (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "H"))
                  (setf --pcl-if-ret--2
                    (progn (p-line 809)
                      (p-let (($n :box (make-p-box nil)))
                        (p-my-= $n
                          (p-if $all (p-* 2 (p-- $slen (p-cast-$ $si_ref :strict))) $nrep))
                        (p-line 810)
                        (p-let (($hex :box (make-p-box nil)))
                          (p-my-= $hex "")
                          (p-line 811)
                          (p-let (($i :scalar 0))
                            (p-for ()
                              ((p-< $i (p-int (p-/ $n 2))))
                              ((p-incf-raw $i :numeric))
                              (p-line 812)
                              (p-let
                                (($b__excl__0 :box
                                    (make-p-box nil)
                                    :perl "$b" :why :exception-global))
                                (p-my-= $b__excl__0
                                  (p-if (p-< (p-+ (p-cast-$ $si_ref :strict) $i) $slen)
                                    (p-ord (p-substr $s (p-+ (p-cast-$ $si_ref :strict) $i) 1))
                                    0))
                                (p-line 813)
                                (p-.= $hex (p-sprintf "%02x" $b__excl__0)))))
                          (p-line 815)
                          (p-my-= $hex (p-substr $hex 0 $n))
                          (p-line 816)
                          (p-incf (p-cast-$ $si_ref :strict-lv) (p-int (p-/ (p-+ $n 1) 2)))
                          (p-line 817)
                          (p-caller-ctx (p-funcall-ref $push_val $hex))))))
                  (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "h"))
                    (setf --pcl-if-ret--2
                      (progn (p-line 819)
                        (p-let (($n :box (make-p-box nil)))
                          (p-my-= $n
                            (p-if $all (p-* 2 (p-- $slen (p-cast-$ $si_ref :strict))) $nrep))
                          (p-line 820)
                          (p-let (($hex :box (make-p-box nil)))
                            (p-my-= $hex "")
                            (p-line 821)
                            (p-let (($i :scalar 0))
                              (p-for ()
                                ((p-< $i (p-int (p-/ $n 2))))
                                ((p-incf-raw $i :numeric))
                                (p-line 822)
                                (p-let
                                  (($b__excl__1 :box
                                      (make-p-box nil)
                                      :perl "$b" :why :exception-global))
                                  (p-my-= $b__excl__1
                                    (p-if (p-< (p-+ (p-cast-$ $si_ref :strict) $i) $slen)
                                      (p-ord (p-substr $s (p-+ (p-cast-$ $si_ref :strict) $i) 1))
                                      0))
                                  (p-line 823)
                                  (p-.= $hex
                                    (p-sprintf "%x%x"
                                      (p-bit-and $b__excl__1 #xF)
                                      (p-bit-and (p->> $b__excl__1 4) #xF))))))
                            (p-line 825)
                            (p-my-= $hex (p-substr $hex 0 $n))
                            (p-line 826)
                            (p-incf (p-cast-$ $si_ref :strict-lv) (p-int (p-/ (p-+ $n 1) 2)))
                            (p-line 827)
                            (p-caller-ctx (p-funcall-ref $push_val $hex))))))
                    (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "B"))
                      (setf --pcl-if-ret--2
                        (progn (p-line 829)
                          (p-let (($nbits :box (make-p-box nil)))
                            (p-my-= $nbits
                              (p-if $all (p-* 8 (p-- $slen (p-cast-$ $si_ref :strict))) $nrep))
                            (p-line 830)
                            (p-if $checksum_p
                              (progn (p-line 831)
                                (p-let (($i :scalar 0))
                                  (p-for ()
                                    ((p-< $i $nbits))
                                    ((p-incf-raw $i :numeric))
                                    :dyn
                                    t
                                    (p-line 832)
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
                                      (p-line 833)
                                      (p-funcall-ref $push_val
                                        (p-bit-and (p->> $b__excl__2 (p-- 7 (p-% $i 8))) 1))))))
                              (progn (p-line 836)
                                (p-let (($bits :box (make-p-box nil)))
                                  (p-my-= $bits "")
                                  (p-line 837)
                                  (p-let (($i :scalar 0))
                                    (p-for ()
                                      ((p-< $i $nbits))
                                      ((p-incf-raw $i :numeric))
                                      (p-line 838)
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
                                        (p-line 839)
                                        (p-.= $bits
                                          (p-if
                                            (p-bit-and (p->> $b__excl__3 (p-- 7 (p-% $i 8))) 1)
                                            "1"
                                            "0")))))
                                  (p-line 841)
                                  (p-funcall-ref $push_val $bits))))
                            (p-line 843)
                            (p-caller-ctx
                              (p-incf (p-cast-$ $si_ref :strict-lv)
                                (p-int (p-/ (p-+ $nbits 7) 8)))))))
                      (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "b"))
                        (setf --pcl-if-ret--2
                          (progn (p-line 845)
                            (p-let (($nbits :box (make-p-box nil)))
                              (p-my-= $nbits
                                (p-if $all
                                  (p-* 8 (p-- $slen (p-cast-$ $si_ref :strict)))
                                  $nrep))
                              (p-line 846)
                              (p-if $checksum_p
                                (progn (p-line 847)
                                  (p-let (($i :scalar 0))
                                    (p-for ()
                                      ((p-< $i $nbits))
                                      ((p-incf-raw $i :numeric))
                                      :dyn
                                      t
                                      (p-line 848)
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
                                        (p-line 849)
                                        (p-funcall-ref $push_val
                                          (p-bit-and (p->> $b__excl__4 (p-% $i 8)) 1))))))
                                (progn (p-line 852)
                                  (p-let (($bits :box (make-p-box nil)))
                                    (p-my-= $bits "")
                                    (p-line 853)
                                    (p-let (($i :scalar 0))
                                      (p-for ()
                                        ((p-< $i $nbits))
                                        ((p-incf-raw $i :numeric))
                                        (p-line 854)
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
                                          (p-line 855)
                                          (p-.= $bits
                                            (p-if (p-bit-and (p->> $b__excl__5 (p-% $i 8)) 1)
                                              "1"
                                              "0")))))
                                    (p-line 857)
                                    (p-funcall-ref $push_val $bits))))
                              (p-line 859)
                              (p-caller-ctx
                                (p-incf (p-cast-$ $si_ref :strict-lv)
                                  (p-int (p-/ (p-+ $nbits 7) 8)))))))
                        (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "u"))
                          (setf --pcl-if-ret--2
                            (progn (p-line 861)
                              (p-let (($decoded :box (make-p-box nil)))
                                (p-my-= $decoded "")
                                (p-line 862)
                                (p-while (p-< (p-cast-$ $si_ref :strict) $slen)
                                  :dyn
                                  t
                                  (p-line 863)
                                  (p-let
                                    (($lc :num
                                        (%pcl-to-number-strict
                                          (p-ord (p-substr $s (p-cast-$ $si_ref :strict-lv) 1))
                                          "$lc")))
                                    (p-line 864)
                                    (p-let
                                      (($nb :num
                                          (%pcl-to-number-strict (p-bit-and (p-- $lc 32) 63)
                                            "$nb")))
                                      (p-line 864)
                                      (p-post++ (p-cast-$ $si_ref))
                                      (p-line 865)
                                      (p-if (p-== $nb 0) (p-last))
                                      (p-line 866)
                                      (p-let
                                        (($ng :num
                                            (%pcl-to-number-strict (p-int (p-/ (p-+ $nb 2) 3))
                                              "$ng")))
                                        (p-line 867)
                                        (p-let (($k :scalar 0))
                                          (p-for ()
                                            ((p-< $k $ng))
                                            ((p-incf-raw $k :numeric))
                                            :dyn
                                            t
                                            (p-line 868)
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
                                                          (p-file "cl/pack-impl.pl")
                                                          (p-void-ctx (p-line 868)
                                                            (p-let
                                                              (($i :scalar
                                                                  (p-+
                                                                    (p-cast-$ $si_ref :strict)
                                                                    (p-aref @_ 0))))
                                                              (p-line 868)
                                                              (p-caller-ctx
                                                                (p-if (p-< $i $slen)
                                                                  (p-bit-and
                                                                    (p--
                                                                      (p-ord (p-substr $s $i 1))
                                                                      32)
                                                                    63)
                                                                  0))))))))))
                                              (p-line 869)
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
                                                (p-line 870)
                                                (p-if (p-< (p-* $k 3) $nb)
                                                  (p-.= $decoded
                                                    (p-chr (p-bit-and (p->> $cm 16) #xFF))))
                                                (p-line 871)
                                                (p-if (p-< (p-+ (p-* $k 3) 1) $nb)
                                                  (p-.= $decoded
                                                    (p-chr (p-bit-and (p->> $cm 8) #xFF))))
                                                (p-line 872)
                                                (p-if (p-< (p-+ (p-* $k 3) 2) $nb)
                                                  (p-.= $decoded (p-chr (p-bit-and $cm #xFF))))))))
                                        (p-line 874)
                                        (p-incf (p-cast-$ $si_ref :strict-lv) (p-* $ng 4))
                                        (p-line 875)
                                        (p-if
                                          (p-&& (p-< (p-cast-$ $si_ref :strict) $slen)
                                            (p-str-eq
                                              (p-substr $s (p-cast-$ $si_ref :strict-lv) 1)
                                              (p-esc "\\n")))
                                          (p-post++ (p-cast-$ $si_ref)))))))
                                (p-line 877)
                                (p-caller-ctx (p-funcall-ref $push_val $decoded)))))
                          (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "U"))
                            (setf --pcl-if-ret--2
                              (progn (p-line 879)
                                (p-let (($n :box (make-p-box nil)))
                                  (p-my-= $n (p-if $all (p-** 9 9) $nrep))
                                  (p-line 880)
                                  (p-let (($done :box (make-p-box nil)))
                                    (p-my-= $done 0)
                                    (p-line 881)
                                    (p-while
                                      (p-&& (p-< $done $n)
                                        (p-< (p-cast-$ $si_ref :strict) $slen))
                                      :dyn
                                      t
                                      (p-line 882)
                                      (p-funcall-ref $push_val
                                        (pl-_unpack_utf8_char $s $si_ref))
                                      (p-line 882)
                                      (p-post++ $done))))))
                            (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "W"))
                              (setf --pcl-if-ret--2
                                (progn (p-line 885)
                                  (p-let (($n :box (make-p-box nil)))
                                    (p-my-= $n
                                      (p-if $all (p-- $slen (p-cast-$ $si_ref :strict)) $nrep))
                                    (p-line 886)
                                    (p-let (($i :scalar 0))
                                      (p-for ()
                                        ((p-&& (p-< $i $n)
                                            (p-< (p-cast-$ $si_ref :strict) $slen)))
                                        ((p-incf-raw $i :numeric))
                                        :dyn
                                        t
                                        (p-line 887)
                                        (p-funcall-ref $push_val
                                          (p-ord (p-substr $s (p-post++ (p-cast-$ $si_ref)) 1))))))))
                              (p-if (setf --pcl-if-ret--2 (p-str-eq $ch "w"))
                                (setf --pcl-if-ret--2
                                  (progn (p-line 890)
                                    (p-let (($done :box (make-p-box nil)))
                                      (p-my-= $done 0)
                                      (p-line 891)
                                      (p-while
                                        (p-&& (p-|| $all (p-< $done $nrep))
                                          (p-< (p-cast-$ $si_ref :strict) $slen))
                                        :dyn
                                        t
                                        (p-line 892)
                                        (p-let (($v :box (make-p-box nil)))
                                          (p-my-= $v 0)
                                          (p-line 892)
                                          (p-let
                                            (($more :str (%pcl-to-string-strict 1 "$more")))
                                            (p-line 893)
                                            (p-while $more
                                              (p-line 894)
                                              (p-if (p->= (p-cast-$ $si_ref :strict) $slen)
                                                (p-die :loc
                                                  "cl/pack-impl.pl line 894"
                                                  (p-esc
                                                    "Unterminated compressed integer in unpack\\n")))
                                              (p-line 895)
                                              (p-let
                                                (($b__excl__6 :box
                                                    (make-p-box nil)
                                                    :perl "$b" :why :exception-global))
                                                (p-my-= $b__excl__6
                                                  (p-ord
                                                    (p-substr $s
                                                      (p-post++ (p-cast-$ $si_ref))
                                                      1)))
                                                (p-line 896)
                                                (setf $more
                                                  (%pcl-to-string-strict
                                                    (p-bit-and $b__excl__6 #x80)
                                                    "$more"))
                                                (p-line 896)
                                                (p-my-= $v
                                                  (p-bit-or (p-<< $v 7)
                                                    (p-bit-and $b__excl__6 #x7F)))))
                                            (p-line 898)
                                            (p-funcall-ref $push_val $v)
                                            (p-line 898)
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
        (p-file "cl/pack-impl.pl")
        (p-void-ctx (p-line 908)
          (p-if (p-! (p-defined $group_base)) (p-my-= $group_base 0))
          (p-line 909)
          (p-if (p-! (p-defined $depth)) (p-my-= $depth 0))
          (p-line 910)
          (p-if (p-> $depth $MAX_GROUP_DEPTH__file__3727705105595030002)
            (p-die :loc
              "cl/pack-impl.pl line 910"
              (p-esc "Too deeply nested ()-groups in unpack\\n")))
          (p-line 911)
          (p-let (($slen :box (make-p-box nil)))
            (p-my-= $slen (p-length $s))
            (p-line 912)
            (p-let (($ti :box (make-p-box nil)))
              (p-my-= $ti 0)
              (p-line 913)
              (p-let (($tlen :num (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
                (p-line 914)
                (p-while 1
                  :dyn
                  t
                  (p-line 915)
                  (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                  (p-line 916)
                  (p-if (p->= $ti $tlen) (p-last))
                  (p-line 917)
                  (p-let (($ch :box (make-p-box nil)))
                    (p-my-= $ch (p-substr $tmpl $ti 1))
                    (p-line 917)
                    (p-post++ $ti)
                    (p-line 918)
                    (p-let (($grpbeg :box (make-p-box nil)))
                      (p-my-= $grpbeg (p-undef))
                      (p-line 918)
                      (p-let (($grpend :num (%pcl-to-number-strict (p-undef) "$grpend")))
                        (p-line 919)
                        (p-if (p-str-eq $ch "(")
                          (progn (p-line 920)
                            (setf $grpend
                              (%pcl-to-number-strict (pl-_pack_find_group_end $tmpl $ti)
                                "$grpend"))
                            (p-line 921)
                            (p-my-= $grpbeg $ti)
                            (p-line 921)
                            (p-my-= $ti (p-+ $grpend 1))
                            (p-line 921)
                            (p-my-= $ch "(")))
                        (p-line 923)
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
                          (p-line 925)
                          (p-let
                            (($ti_before_count :num
                                (%pcl-to-number-strict $ti "$ti_before_count")))
                            (p-line 926)
                            (p-let
                              (($all :box (make-p-box nil))
                                ($count :box (make-p-box nil))
                                ($nrep :box (make-p-box nil)))
                              (p-scalar-ctx
                                (p-list-= (vector $all $count $nrep)
                                  (p-list-ctx (pl-_pack_parse_count $tmpl (p-backslash $ti)))))
                              (p-line 927)
                              (p-let (($had_count :box (make-p-box nil)))
                                (p-my-= $had_count (p-|| $all (p-> $ti $ti_before_count)))
                                (p-line 928)
                                (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                (p-line 931)
                                (p-if
                                  (p-&& (p-< $ti $tlen) (p-str-eq (p-substr $tmpl $ti 1) "/"))
                                  (progn (p-line 932)
                                    (p-post++ $ti)
                                    (p-line 933)
                                    (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                    (p-line 934)
                                    (p-if (p->= $ti $tlen)
                                      (p-die :loc
                                        "cl/pack-impl.pl line 934"
                                        (p-esc "Code missing after '/' in unpack\\n")))
                                    (p-line 934)
                                    (let ((*package* *package*))
                                      (block nil
                                        (tagbody :redo
                                          (p-line 937)
                                          (p-let (($c :box (make-p-box nil)))
                                            (p-my-= $c (p-substr $tmpl $ti 1))
                                            (p-line 938)
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
                                    (p-line 940)
                                    (p-let
                                      (($nb :box (make-p-box nil))
                                        ($sig :box (make-p-box nil))
                                        ($dbe :box (make-p-box nil)))
                                      (p-scalar-ctx
                                        (p-list-= (vector $nb $sig $dbe)
                                          (p-list-ctx (pl-_pack_type_info $ch $bang))))
                                      (p-line 941)
                                      (p-let (($slash_n :box (make-p-box nil)))
                                        (p-my-= $slash_n 0)
                                        (p-line 942)
                                        (p-if $nb
                                          (progn (p-line 943)
                                            (p-let (($be2 :box (make-p-box nil)))
                                              (p-my-= $be2 (p-if $be 1 (p-if $le 0 $dbe)))
                                              (p-line 944)
                                              (p-if
                                                (p-> (p-+ (p-cast-$ $si_ref :strict) $nb)
                                                  $slen)
                                                (progn (p-line 945)
                                                  (p-if (p-! (p-> $depth 0)) (p-last))
                                                  (p-line 946)
                                                  (p-die :loc
                                                    "cl/pack-impl.pl line 946"
                                                    (p-esc
                                                      "length/code after end of string in unpack\\n"))))
                                              (p-line 948)
                                              (p-my-= $slash_n
                                                (pl-_unpack_read_int $s
                                                  (p-cast-$ $si_ref :strict-lv)
                                                  $nb
                                                  $be2
                                                  $sig))
                                              (p-line 949)
                                              (p-incf (p-cast-$ $si_ref :strict-lv) $nb)))
                                          (p-if (p-str-eq $ch "w")
                                            (progn (p-line 951)
                                              (p-let (($more :box (make-p-box nil)))
                                                (p-my-= $more 1)
                                                (p-line 952)
                                                (p-while $more
                                                  (p-line 953)
                                                  (p-if (p->= (p-cast-$ $si_ref :strict) $slen)
                                                    (p-die :loc
                                                      "cl/pack-impl.pl line 953"
                                                      (p-esc
                                                        "Unterminated compressed integer in unpack\\n")))
                                                  (p-line 954)
                                                  (p-let
                                                    (($b__excl__7 :box
                                                        (make-p-box nil)
                                                        :perl "$b" :why :exception-global))
                                                    (p-my-= $b__excl__7
                                                      (p-ord
                                                        (p-substr $s
                                                          (p-post++ (p-cast-$ $si_ref))
                                                          1)))
                                                    (p-line 955)
                                                    (p-my-= $more (p-bit-and $b__excl__7 #x80))
                                                    (p-line 955)
                                                    (p-my-= $slash_n
                                                      (p-bit-or (p-<< $slash_n 7)
                                                        (p-bit-and $b__excl__7 #x7F)))))))
                                            (p-if (p-str-eq $ch "Z")
                                              (progn (p-line 959)
                                                (p-let
                                                  (($end :num
                                                      (%pcl-to-number-strict
                                                        (p-index $s
                                                          (p-esc "\\u0000")
                                                          (p-cast-$ $si_ref :strict-lv))
                                                        "$end")))
                                                  (p-line 960)
                                                  (p-if (p-< $end 0)
                                                    (progn (p-line 960)
                                                      (setf $end
                                                        (%pcl-to-number-strict $slen "$end"))))
                                                  (p-line 961)
                                                  (p-let (($raw :box (make-p-box nil)))
                                                    (p-my-= $raw
                                                      (p-substr $s
                                                        (p-cast-$ $si_ref :strict-lv)
                                                        (p-- $end (p-cast-$ $si_ref :strict))))
                                                    (p-line 962)
                                                    (p-setf (p-cast-$ $si_ref :strict-lv)
                                                      (p-+ $end 1))
                                                    (p-line 963)
                                                    (p-if (p-> (p-cast-$ $si_ref :strict) $slen)
                                                      (p-setf (p-cast-$ $si_ref :strict-lv)
                                                        $slen))
                                                    (p-line 964)
                                                    (p-my-= $slash_n (p-+ $raw 0)))))
                                              (progn (p-line 966)
                                                (p-let (($n :box (make-p-box nil)))
                                                  (p-my-= $n
                                                    (p-if $all
                                                      (p-- $slen (p-cast-$ $si_ref :strict))
                                                      $nrep))
                                                  (p-line 967)
                                                  (p-let (($raw :box (make-p-box nil)))
                                                    (p-my-= $raw
                                                      (p-if
                                                        (p-< (p-cast-$ $si_ref :strict) $slen)
                                                        (p-substr $s
                                                          (p-cast-$ $si_ref :strict-lv)
                                                          $n)
                                                        ""))
                                                    (p-line 968)
                                                    (p-incf (p-cast-$ $si_ref :strict-lv) $n)
                                                    (p-line 969)
                                                    (p-if (p-str-eq $ch "A")
                                                      (p-=~ $raw
                                                        (p-subst :pat "[ \\x00]+$"
                                                          :rep ""
                                                          :flags ""
                                                          :tier :native)))
                                                    (p-line 970)
                                                    (p-my-= $slash_n (p-+ $raw 0))))))))
                                        (p-line 973)
                                        (p-while 1
                                          :dyn
                                          t
                                          (p-line 974)
                                          (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                          (p-line 974)
                                          (p-if (p->= $ti $tlen) (p-last))
                                          (p-line 975)
                                          (p-let (($dch :box (make-p-box nil)))
                                            (p-my-= $dch (p-substr $tmpl $ti 1))
                                            (p-line 975)
                                            (p-post++ $ti)
                                            (p-line 976)
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
                                              (p-line 977)
                                              (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                              (p-line 978)
                                              (p-let
                                                (($dall :box (make-p-box nil))
                                                  ($dcnt :box (make-p-box nil))
                                                  ($dnrep :box (make-p-box nil)))
                                                (p-scalar-ctx
                                                  (p-list-= (vector $dall $dcnt $dnrep)
                                                    (p-list-ctx
                                                      (pl-_pack_parse_count $tmpl
                                                        (p-backslash $ti)))))
                                                (p-line 979)
                                                (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
                                                (p-line 982)
                                                (p-let
                                                  (($chain :str
                                                      (%pcl-to-string-strict
                                                        (p-&& (p-< $ti $tlen)
                                                          (p-str-eq (p-substr $tmpl $ti 1) "/"))
                                                        "$chain")))
                                                  (p-line 983)
                                                  (p-if $chain
                                                    (progn (p-line 983) (p-post++ $ti)))
                                                  (p-line 985)
                                                  (p-let
                                                    (($dnb :box (make-p-box nil))
                                                      ($dsig :box (make-p-box nil))
                                                      ($ddbe :box (make-p-box nil)))
                                                    (p-scalar-ctx
                                                      (p-list-= (vector $dnb $dsig $ddbe)
                                                        (p-list-ctx
                                                          (pl-_pack_type_info $dch $dbang))))
                                                    (p-line 986)
                                                    (p-if $chain
                                                      (progn (p-line 988)
                                                        (p-if $dnb
                                                          (progn (p-line 989)
                                                            (p-let
                                                              (($dbe3 :box (make-p-box nil)))
                                                              (p-my-= $dbe3
                                                                (p-if $dbe2
                                                                  1
                                                                  (p-if $dle2 0 $ddbe)))
                                                              (p-line 990)
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
                                                              (p-line 992)
                                                              (p-my-= $slash_n
                                                                (pl-_unpack_read_int $s
                                                                  (p-cast-$ $si_ref :strict-lv)
                                                                  $dnb
                                                                  $dbe3
                                                                  $dsig))
                                                              (p-line 993)
                                                              (p-incf
                                                                (p-cast-$ $si_ref :strict-lv)
                                                                $dnb)))
                                                          (p-if (p-str-eq $dch "w")
                                                            (progn (p-line 995)
                                                              (p-my-= $slash_n 0)
                                                              (p-line 996)
                                                              (p-let
                                                                (($more :box (make-p-box nil)))
                                                                (p-my-= $more 1)
                                                                (p-line 997)
                                                                (p-while $more
                                                                  (p-line 998)
                                                                  (p-if
                                                                    (p->=
                                                                      (p-cast-$ $si_ref
                                                                        :strict)
                                                                      $slen)
                                                                    (p-last))
                                                                  (p-line 999)
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
                                                                    (p-line 1000)
                                                                    (p-my-= $more
                                                                      (p-bit-and $b__excl__8
                                                                        #x80))
                                                                    (p-line 1000)
                                                                    (p-my-= $slash_n
                                                                      (p-bit-or
                                                                        (p-<< $slash_n 7)
                                                                        (p-bit-and $b__excl__8
                                                                          #x7F)))))))
                                                            (progn (p-line 1004)
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
                                                                (p-line 1005)
                                                                (p-incf
                                                                  (p-cast-$ $si_ref :strict-lv)
                                                                  $slash_n)
                                                                (p-line 1006)
                                                                (p-if (p-str-eq $dch "A")
                                                                  (p-=~ $raw2
                                                                    (p-subst :pat "[ \\x00]+$"
                                                                      :rep ""
                                                                      :flags ""
                                                                      :tier :native)))
                                                                (p-line 1007)
                                                                (p-if (p-str-eq $dch "Z")
                                                                  (p-=~ $raw2
                                                                    (p-subst :pat "\\x00.*"
                                                                      :rep ""
                                                                      :flags "s"
                                                                      :tier :native)))
                                                                (p-line 1008)
                                                                (p-my-= $slash_n (p-+ $raw2 0)))))))
                                                      (progn (p-line 1013)
                                                        (p-if $dnb
                                                          (progn (p-line 1014)
                                                            (p-let
                                                              (($dbe3 :box (make-p-box nil)))
                                                              (p-my-= $dbe3
                                                                (p-if $dbe2
                                                                  1
                                                                  (p-if $dle2 0 $ddbe)))
                                                              (p-line 1015)
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
                                                                  (p-line 1016)
                                                                  (p-funcall-ref $push_val
                                                                    (pl-_unpack_read_int $s
                                                                      (p-cast-$ $si_ref
                                                                        :strict-lv)
                                                                      $dnb
                                                                      $dbe3
                                                                      $dsig))
                                                                  (p-line 1017)
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
                                                            (progn (p-line 1022)
                                                              (pl-_unpack_str $dch
                                                                $slash_n
                                                                0
                                                                $s
                                                                $si_ref
                                                                $push_val
                                                                $checksum_p))
                                                            (p-if (p-str-eq $dch "(")
                                                              (progn (p-line 1024)
                                                                (p-let
                                                                  (($ge :num
                                                                      (%pcl-to-number-strict
                                                                        (pl-_pack_find_group_end
                                                                          $tmpl
                                                                          $ti)
                                                                        "$ge")))
                                                                  (p-line 1025)
                                                                  (p-let
                                                                    (($inner :box
                                                                        (make-p-box nil)))
                                                                    (p-my-= $inner
                                                                      (p-substr $tmpl
                                                                        $ti
                                                                        (p-- $ge $ti)))
                                                                    (p-line 1025)
                                                                    (p-my-= $ti (p-+ $ge 1))
                                                                    (p-line 1026)
                                                                    (p-let (($r :scalar 0))
                                                                      (p-for ()
                                                                        ((p-< $r $slash_n))
                                                                        ((p-incf-raw $r
                                                                            :numeric))
                                                                        (p-line 1027)
                                                                        (p-let
                                                                          (($iter_base :box
                                                                              (make-p-box nil)))
                                                                          (p-my-= $iter_base
                                                                            (p-cast-$ $si_ref
                                                                              :strict))
                                                                          (p-line 1028)
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
                                                        (p-line 1031)
                                                        (p-last)))))))))
                                        (p-line 1034)
                                        (p-next)))))
                                (p-line 1038)
                                (p-if (p-defined $grpbeg)
                                  (progn (p-line 1039)
                                    (p-let (($inner :box (make-p-box nil)))
                                      (p-my-= $inner
                                        (p-substr $tmpl $grpbeg (p-- $grpend $grpbeg)))
                                      (p-line 1040)
                                      (p-let
                                        (($gti :num
                                            (%pcl-to-number-strict (pl-_pack_skip_ws $inner 0)
                                              "$gti")))
                                        (p-line 1041)
                                        (p-if (p-< $gti (p-length $inner))
                                          (progn (p-line 1042)
                                            (p-let (($fc :box (make-p-box nil)))
                                              (p-my-= $fc (p-substr $inner $gti 1))
                                              (p-line 1043)
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
                                        (p-line 1045)
                                        (p-if $all
                                          (progn (p-line 1046)
                                            (p-while (p-< (p-cast-$ $si_ref :strict) $slen)
                                              (p-line 1047)
                                              (p-let
                                                (($si_before :num
                                                    (%pcl-to-number-strict
                                                      (p-cast-$ $si_ref :strict)
                                                      "$si_before")))
                                                (p-line 1048)
                                                (p-let (($iter_base :box (make-p-box nil)))
                                                  (p-my-= $iter_base
                                                    (p-cast-$ $si_ref :strict))
                                                  (p-line 1049)
                                                  (pl-_unpack_tmpl $inner
                                                    $s
                                                    $si_ref
                                                    $push_val
                                                    $be
                                                    $le
                                                    $checksum_p
                                                    $iter_base
                                                    (p-+ $depth 1))
                                                  (p-line 1050)
                                                  (p-if
                                                    (p-== (p-cast-$ $si_ref :strict)
                                                      $si_before)
                                                    (p-last))))))
                                          (progn (p-line 1053)
                                            (p-let (($r :scalar 0))
                                              (p-for ()
                                                ((p-< $r $nrep))
                                                ((p-incf-raw $r :numeric))
                                                (p-line 1054)
                                                (p-let (($iter_base :box (make-p-box nil)))
                                                  (p-my-= $iter_base
                                                    (p-cast-$ $si_ref :strict))
                                                  (p-line 1055)
                                                  (pl-_unpack_tmpl $inner
                                                    $s
                                                    $si_ref
                                                    $push_val
                                                    $be
                                                    $le
                                                    $checksum_p
                                                    $iter_base
                                                    (p-+ $depth 1)))))))
                                        (p-line 1058)
                                        (p-next)))))
                                (p-line 1062)
                                (p-if (p-str-eq $ch "x")
                                  (progn (p-line 1063)
                                    (p-if $bang
                                      (progn (p-line 1065)
                                        (p-let (($n :box (make-p-box nil)))
                                          (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                          (p-line 1066)
                                          (p-incf (p-cast-$ $si_ref :strict-lv)
                                            (p-% (p-- $n (p-% (p-cast-$ $si_ref :strict) $n))
                                              $n))))
                                      (p-if $all
                                        (progn (p-line 1067)
                                          (p-setf (p-cast-$ $si_ref :strict-lv) $slen))
                                        (progn (p-line 1070)
                                          (p-if
                                            (p-> (p-+ (p-cast-$ $si_ref :strict) $nrep) $slen)
                                            (p-die :loc
                                              "cl/pack-impl.pl line 1070"
                                              (p-esc "'x' outside of string in unpack\\n")))
                                          (p-line 1071)
                                          (p-incf (p-cast-$ $si_ref :strict-lv) $nrep))))
                                    (p-line 1073)
                                    (p-next)))
                                (p-line 1075)
                                (p-if (p-str-eq $ch "X")
                                  (progn (p-line 1076)
                                    (p-if $bang
                                      (progn (p-line 1078)
                                        (p-let (($n :box (make-p-box nil)))
                                          (p-my-= $n (p-if (p-> $nrep 0) $nrep 1))
                                          (p-line 1079)
                                          (p-setf (p-cast-$ $si_ref :strict-lv)
                                            (p-* (p-int (p-/ (p-cast-$ $si_ref :strict) $n))
                                              $n))))
                                      (progn (p-line 1081)
                                        (p-if (p-> $nrep (p-cast-$ $si_ref :strict))
                                          (p-die :loc
                                            "cl/pack-impl.pl line 1081"
                                            (p-esc "'X' outside of string in unpack\\n")))
                                        (p-line 1082)
                                        (p-decf (p-cast-$ $si_ref :strict-lv) $nrep)))
                                    (p-line 1084)
                                    (p-next)))
                                (p-line 1087)
                                (p-if (p-str-eq $ch "@")
                                  (progn (p-line 1088)
                                    (p-let (($n :box (make-p-box nil)))
                                      (p-my-= $n (p-if (p-defined $count) $count 0))
                                      (p-line 1089)
                                      (p-setf (p-cast-$ $si_ref :strict-lv)
                                        (p-if $bang $n (p-+ $group_base $n)))
                                      (p-line 1090)
                                      (p-next))))
                                (p-line 1092)
                                (p-if (p-|| (p-str-eq $ch "%") (p-str-eq $ch "!"))
                                  (progn (p-line 1092) (p-next)))
                                (p-line 1093)
                                (p-if
                                  (p-|| (p-|| (p-str-eq $ch "p") (p-str-eq $ch "P"))
                                    (p-str-eq $ch "D"))
                                  (progn (p-line 1093)
                                    (p-die :loc
                                      "cl/pack-impl.pl line 1093"
                                      (p-string-concat "Invalid type '"
                                        $ch
                                        (p-esc "' in unpack\\n")))))
                                (p-line 1094)
                                (p-if (p-str-eq $ch ".")
                                  (progn (p-line 1100)
                                    (p-if $all
                                      (progn (p-line 1101)
                                        (p-funcall-ref $push_val (p-cast-$ $si_ref :strict-lv)))
                                      (p-if (p-&& (p-defined $count) (p-== $count 0))
                                        (progn (p-line 1103) (p-funcall-ref $push_val 0))
                                        (p-if (p-&& (p-defined $count) (p->= $count 2))
                                          (progn (p-line 1105)
                                            (p-funcall-ref $push_val
                                              (p-cast-$ $si_ref :strict-lv)))
                                          (progn (p-line 1107)
                                            (p-funcall-ref $push_val
                                              (p-- (p-cast-$ $si_ref :strict) $group_base))))))
                                    (p-line 1109)
                                    (p-next)))
                                (p-line 1113)
                                (p-let
                                  (($nb :box (make-p-box nil))
                                    ($sig :box (make-p-box nil))
                                    ($dbe :box (make-p-box nil)))
                                  (p-scalar-ctx
                                    (p-list-= (vector $nb $sig $dbe)
                                      (p-list-ctx (pl-_pack_type_info $ch $bang))))
                                  (p-line 1114)
                                  (p-if $nb
                                    (progn (p-line 1115)
                                      (p-let (($be2 :box (make-p-box nil)))
                                        (p-my-= $be2 (p-if $be 1 (p-if $le 0 $dbe)))
                                        (p-line 1116)
                                        (p-let (($n :box (make-p-box nil)))
                                          (p-my-= $n
                                            (p-if $all
                                              (p-int
                                                (p-/ (p-- $slen (p-cast-$ $si_ref :strict))
                                                  $nb))
                                              $nrep))
                                          (p-line 1117)
                                          (p-let (($i :scalar 0))
                                            (p-for ()
                                              ((p-< $i $n))
                                              ((p-incf-raw $i :numeric))
                                              :dyn
                                              t
                                              (p-line 1118)
                                              (p-if
                                                (p-> (p-+ (p-cast-$ $si_ref :strict) $nb)
                                                  $slen)
                                                (p-last))
                                              (p-line 1119)
                                              (p-funcall-ref $push_val
                                                (pl-_unpack_read_int $s
                                                  (p-cast-$ $si_ref :strict-lv)
                                                  $nb
                                                  $be2
                                                  $sig))
                                              (p-line 1120)
                                              (p-incf (p-cast-$ $si_ref :strict-lv) $nb)))
                                          (p-line 1122)
                                          (p-next)))))
                                  (p-line 1126)
                                  (p-if (p-str-eq $ch "f")
                                    (progn (p-line 1127)
                                      (p-let (($be2 :box (make-p-box nil)))
                                        (p-my-= $be2 (p-if $be 1 (p-if $le 0 0)))
                                        (p-line 1128)
                                        (p-let (($n :box (make-p-box nil)))
                                          (p-my-= $n
                                            (p-if $all
                                              (p-int
                                                (p-/ (p-- $slen (p-cast-$ $si_ref :strict)) 4))
                                              $nrep))
                                          (p-line 1129)
                                          (p-let (($i :scalar 0))
                                            (p-for ()
                                              ((p-< $i $n))
                                              ((p-incf-raw $i :numeric))
                                              :dyn
                                              t
                                              (p-line 1130)
                                              (p-if
                                                (p-> (p-+ (p-cast-$ $si_ref :strict) 4) $slen)
                                                (p-last))
                                              (p-line 1131)
                                              (p-funcall-ref $push_val
                                                (pl-_unpack_float32 $s
                                                  (p-cast-$ $si_ref :strict-lv)
                                                  $be2))
                                              (p-line 1131)
                                              (p-incf (p-cast-$ $si_ref :strict-lv) 4)))
                                          (p-line 1133)
                                          (p-next)))))
                                  (p-line 1135)
                                  (p-if (p-|| (p-str-eq $ch "d") (p-str-eq $ch "F"))
                                    (progn (p-line 1136)
                                      (p-let (($be2 :box (make-p-box nil)))
                                        (p-my-= $be2 (p-if $be 1 (p-if $le 0 0)))
                                        (p-line 1137)
                                        (p-let (($n :box (make-p-box nil)))
                                          (p-my-= $n
                                            (p-if $all
                                              (p-int
                                                (p-/ (p-- $slen (p-cast-$ $si_ref :strict)) 8))
                                              $nrep))
                                          (p-line 1138)
                                          (p-let (($i :scalar 0))
                                            (p-for ()
                                              ((p-< $i $n))
                                              ((p-incf-raw $i :numeric))
                                              :dyn
                                              t
                                              (p-line 1139)
                                              (p-if
                                                (p-> (p-+ (p-cast-$ $si_ref :strict) 8) $slen)
                                                (p-last))
                                              (p-line 1140)
                                              (p-funcall-ref $push_val
                                                (pl-_unpack_float64 $s
                                                  (p-cast-$ $si_ref :strict-lv)
                                                  $be2))
                                              (p-line 1140)
                                              (p-incf (p-cast-$ $si_ref :strict-lv) 8)))
                                          (p-line 1142)
                                          (p-next)))))
                                  (p-line 1146)
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
                                    (progn (p-line 1148)
                                      (pl-_unpack_str $ch
                                        $nrep
                                        $all
                                        $s
                                        $si_ref
                                        $push_val
                                        $checksum_p)
                                      (p-line 1149)
                                      (p-next)))
                                  (p-line 1152)
                                  (p-if (p-str-eq $ch "/")
                                    (p-die :loc
                                      "cl/pack-impl.pl line 1152"
                                      (p-esc "'/' must follow a numeric type in unpack\\n")))
                                  (p-line 1153)
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
        (p-file "cl/pack-impl.pl")
        (p-void-ctx (p-line 1161)
          (p-let (($tlen :num (%pcl-to-number-strict (p-length $tmpl) "$tlen")))
            (p-line 1162)
            (p-let (($ti :box (make-p-box nil)))
              (p-my-= $ti 0)
              (p-line 1163)
              (p-my-= $ti (pl-_pack_skip_ws $tmpl $ti))
              (p-line 1164)
              ;; return ('', '') if $ti >= $tlen
(p-if (p->= $ti $tlen) (p-return "" ""))
              (p-line 1165)
              (p-let (($ch :str (%pcl-to-string-strict (p-substr $tmpl $ti 1) "$ch")))
                (p-line 1165)
                (p-post++ $ti)
                (p-line 1166)
                (p-if (p-str-eq $ch "(")
                  (progn (p-line 1167)
                    (p-let
                      (($grpend :num
                          (%pcl-to-number-strict (pl-_pack_find_group_end $tmpl $ti) "$grpend")))
                      (p-line 1168)
                      (p-my-= $ti (p-+ $grpend 1)))))
                (p-line 1170)
                (p-while
                  (p-&& (p-< $ti $tlen)
                    (p-scalar-ctx
                      (p-=~ (p-substr $tmpl $ti 1)
                        (p-regex :pat "[!<>]" :flags "" :tier :native))))
                  (p-line 1170)
                  (p-post++ $ti))
                (p-line 1171)
                (pl-_pack_parse_count $tmpl (p-backslash $ti))
                (p-line 1172)
                ;; return (substr($tmpl, 0, $ti), substr($tmpl, $ti))
(p-return (p-substr $tmpl 0 $ti) (p-substr $tmpl $ti))))))))))

(p-sub pl-p_unpack
  (&rest %_args)
  (:writes-args nil :needs (:regex.literal :regex.split :regex.subst :regex.native))
  (p-args-body
    (block nil
      (p-let (($tmpl :box (make-p-box nil)) ($s :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $tmpl $s) @_))
        (p-file "cl/pack-impl.pl")
        (p-void-ctx (p-line 1177)
          (p-if (p-! (p-defined $s)) (p-my-= $s ""))
          (p-line 1180)
          (p-let (($lead_u :scalar (p-str-eq (p-substr $tmpl 0 1) "U")))
            (p-line 1182)
            (p-=~ $tmpl
              (p-subst :pat "\\A(?:[ \\t\\n\\r\\f,]|#[^\\n]*\\n?)*"
                :rep ""
                :flags ""
                :tier :native))
            (p-line 1184)
            (p-let (($checksum_width :box (make-p-box nil)))
              (p-my-= $checksum_width 0)
              (p-line 1185)
              (p-if (p-=~ $tmpl (p-subst :pat "^%(\\d*)" :rep "" :flags "" :tier :native))
                (progn (p-line 1186)
                  (p-my-= $checksum_width (p-if (p-length $1) (p-int $1) 16))))
              (p-line 1189)
              (p-let
                (($utf8_mode :str
                    (%pcl-to-string-strict
                      (p-|| (p-=~ $tmpl (p-subst :pat "^U0" :rep "" :flags "" :tier :native))
                        $lead_u)
                      "$utf8_mode")))
                (p-line 1191)
                (p-if $checksum_width
                  (p-=~ $tmpl
                    (p-subst :pat "\\A(?:[ \\t\\n\\r\\f,]|#[^\\n]*\\n?)*"
                      :rep ""
                      :flags ""
                      :tier :native)))
                (p-line 1192)
                (pl-_pack_check_brackets $tmpl)
                (p-line 1193)
                (p-if $utf8_mode
                  (progn (p-line 1194)
                    (p-let (($bytes :scalar ""))
                      (p-line 1195)
                      (p-foreach-raw ($c (p-split (p-regex :pat "" :flags "" :tier :native) $s))
                        :my
                        t
                        (p-line 1196)
                        (p-let (($code :box (make-p-box nil)))
                          (p-my-= $code (p-ord $c))
                          (p-line 1197)
                          (p-if (p-< $code #x80)
                            (progn (p-line 1197) (p-.=-raw $bytes (p-chr $code)))
                            (p-if (p-< $code #x800)
                              (progn (p-line 1198)
                                (p-.=-raw $bytes
                                  (p-. (p-chr (p-bit-or #xC0 (p->> $code 6)))
                                    (p-chr (p-bit-or #x80 (p-bit-and $code #x3F))))))
                              (p-if (p-< $code #x10000)
                                (progn (p-line 1199)
                                  (p-.=-raw $bytes
                                    (p-.
                                      (p-. (p-chr (p-bit-or #xE0 (p->> $code 12)))
                                        (p-chr (p-bit-or #x80 (p-bit-and (p->> $code 6) #x3F))))
                                      (p-chr (p-bit-or #x80 (p-bit-and $code #x3F))))))
                                (progn (p-line 1200)
                                  (p-.=-raw $bytes
                                    (p-.
                                      (p-.
                                        (p-. (p-chr (p-bit-or #xF0 (p->> $code 18)))
                                          (p-chr
                                            (p-bit-or #x80 (p-bit-and (p->> $code 12) #x3F))))
                                        (p-chr (p-bit-or #x80 (p-bit-and (p->> $code 6) #x3F))))
                                      (p-chr (p-bit-or #x80 (p-bit-and $code #x3F)))))))))))
                      (p-line 1202)
                      (p-my-= $s $bytes))))
                (p-line 1204)
                (p-let ((@result :array (make-array 0 :adjustable t :fill-pointer 0)))
                  (p-line 1205)
                  (p-let (($si :box (make-p-box nil)))
                    (p-my-= $si 0)
                    (p-line 1206)
                    (p-if $checksum_width
                      (progn (p-line 1208)
                        (p-let
                          (($cs_tmpl :box (make-p-box nil)) ($rest_tmpl :box (make-p-box nil)))
                          (p-scalar-ctx
                            (p-list-= (vector $cs_tmpl $rest_tmpl)
                              (p-list-ctx (pl-_next_format_item $tmpl))))
                          (p-line 1209)
                          (p-let (($checksum :box (make-p-box nil) :captured t))
                            (p-my-= $checksum 0)
                            (p-line 1210)
                            (p-if (p-length $cs_tmpl)
                              (progn (p-line 1211)
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
                                          (block nil
                                            (p-file "cl/pack-impl.pl")
                                            (p-line 1211)
                                            (p-incf $checksum (p-aref @_ 0)))))))
                                  0
                                  0
                                  1)))
                            (p-line 1217)
                            (p-let (($mod :scalar 1))
                              (p-line 1218)
                              (p-foreach-range-raw ($_ 1 $checksum_width)
                                (p-line 1218)
                                (p-*=-raw $mod 2 :numeric))
                              (p-line 1219)
                              (p-let (($q :box (make-p-box nil)))
                                (p-my-= $q (p-int (p-/ $checksum $mod)))
                                (p-line 1220)
                                (p-if (p-> (p-* $q $mod) $checksum) (p-post-- $q))
                                (p-line 1221)
                                (p-push @result (p-- $checksum (p-* $q $mod)))
                                (p-line 1222)
                                (p-if (p-length $rest_tmpl)
                                  (progn (p-line 1223)
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
                                              (block nil
                                                (p-file "cl/pack-impl.pl")
                                                (p-line 1223)
                                                (p-push @result (p-aref @_ 0)))))))
                                      0
                                      0
                                      0))))))))
                      (progn (p-line 1226)
                        (pl-_unpack_tmpl $tmpl
                          $s
                          (p-backslash $si)
                          (p-anon-sub "main"
                            (lambda (&rest %_args)
                              (let
                                ((@_ (p-flatten-args %_args))
                                  (*pcl-current-package* "main")
                                  (*pcl-caller-wantarray* *wantarray*))
                                (p-sub-frame
                                  (block nil
                                    (p-file "cl/pack-impl.pl")
                                    (p-line 1226)
                                    (p-push @result (p-aref @_ 0)))))))
                          0
                          0
                          0)))
                    (p-line 1228)
                    (p-caller-ctx (p-tail-value (p-if (p-wantarray) @result (p-aref @result 0))))))))))))))

(pcl::p-eval-always (pcl::p-detect-end "cl/pack-impl.pl"))
(p-run-compile-phase-blocks)

(p-file "cl/pack-impl.pl")

(p-line 15)

(p-line 16)

(p-line 55)

(p-scalar-= $CAN_ENDIAN__file__3727705105595030000 "sSiIlLqQjJfFdDpP")

(p-line 56)

(p-scalar-= $CAN_SHRIEK__file__3727705105595030001 "sSiIlLnNvVxX.@")

(p-line 57)

(p-scalar-= $MAX_GROUP_DEPTH__file__3727705105595030002 100)

(p-line 81)

(p-scalar-= $pcl_pack_comma_warned 0)

(p-line 1231)

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

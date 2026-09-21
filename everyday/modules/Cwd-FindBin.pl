# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 modwork / mw-10-Cwd-FindBin.pl
use strict; use warnings; use Cwd qw(getcwd cwd abs_path realpath); use FindBin qw($Bin $Script $RealBin);
chdir "/tmp" or die; print join(" ", getcwd(), (cwd() eq getcwd() ? "cwd-same" : "cwd-diff"), abs_path("/tmp/../tmp/."), realpath("/usr/bin/..") , ($Bin =~ m{^/} ? "bin-abs" : "bin-rel:$Bin"), ($Script =~ /^mw-\d+-Cwd-FindBin\.pl$/ ? "script" : "script:$Script"), ($RealBin eq $Bin ? "real-same" : "real-diff")), "\n";

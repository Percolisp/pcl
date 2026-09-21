# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-11-env-exit-end.pl
# expect-rc: 3
# s495e repair: the program printed `$0` with the path stripped by
# `s/\S*b2-/b2-/` -- keyed on the OLD file name, so after the rename the whole
# absolute path leaked into stdout and the answer became a fact of WHERE the
# file is.  `s{\S*/}{}` strips the directory, which is what the original meant.
use strict; use warnings;
BEGIN { $ENV{PCL_B2_VAR} = "from-begin" }
END { print "end block ran, status=$?\n" }
$ENV{PCL_B2_CHILD} = "child-sees"; my $out = `sh -c 'echo \$PCL_B2_CHILD'`; chomp $out;
print "$ENV{PCL_B2_VAR} $out ", (exists $ENV{PATH} ? "path" : "nopath"), " ", (defined $ENV{NOPE_B2} ? "def" : "undef"), "\n";
delete $ENV{PCL_B2_CHILD}; my $out2 = `sh -c 'echo x\$PCL_B2_CHILD'`; chomp $out2; print "$out2 @ARGV|$0\n" =~ s{\S*/}{}r;
exit 3;

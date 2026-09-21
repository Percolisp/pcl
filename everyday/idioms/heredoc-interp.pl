# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s491 idioms / idiom-16-heredoc-interp.pl
no warnings;
my ($n, @l, %h) = ("w", 1, 2); %h = (k => "v"); my $t = <<"E" . <<'R';
n=$n l=@l h=$h{k} e=@{[ 1+2 ]} m=${\ uc $n}
E
raw $n
R
print $t;

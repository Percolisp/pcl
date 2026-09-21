# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# origin: s492 battery2 / b2-14-nested-eval-errors.pl
use strict; use warnings;
my @log;
my $r = eval { eval { die "inner\n" }; push @log, "caught:$@"; die "outer\n"; 1 }; push @log, "outer:$@" if !$r;
eval { eval { die { code => 42 } }; die $@ if ref $@; }; push @log, "ref:$@->{code}";
eval { my $x = 1; my $y = 0; my $z = $x / $y; }; push @log, ($@ =~ /division by zero/ ? "div0" : "nodiv:$@");
eval { my $u; my @a = @$u; push @log, "autoviv-rvalue-ok" }; push @log, "strict-refs-died" if $@;
eval { local $@ = "kept"; eval { 1 }; }; { local $@; eval { die "scoped\n" }; } push @log, "after:" . ($@ eq "" ? "empty" : $@);
eval { die "with line" }; push @log, ($@ =~ /^with line at \S+ line \d+\.$/ ? "line-appended" : "noline:$@");
$SIG{__DIE__} = sub { push @log, "sigdie" }; eval { die "x\n" }; $SIG{__DIE__} = "DEFAULT";
print join(" ", @log), "\n"; warn "to stderr\n";

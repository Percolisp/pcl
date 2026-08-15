# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# warnings - PCL shim (categories treated as globally enabled).
#
# The real warnings.pm keys everything on the caller's lexical warning bits
# (${^WARNING_BITS}), which PCL does not implement.  This shim serves the
# QUERY/EMIT API that library code calls at runtime (charnames/_charnames.pm:
# `warnings::enabled('utf8')`, Carp, and friends): every category reports
# ENABLED and never FATAL — matching the common case of code running under
# `use warnings`.  `use warnings` itself never loads this file: PCL treats it
# as a pragma (Parser skip list); the runtime exposes these subs via
# always-available self-loading stubs (cl/pcl-warnings.lisp), same pattern
# as mro.  Regenerate after editing:  ./pl2cl lib/warnings.pm > cl/pcl-warnings.lisp

package warnings;
use strict;

our $VERSION = '1.70';

# Unused in practice (pragma is skipped before import is ever called).
sub import { }
sub unimport { }

# No lexical warning bits: every category is enabled, none fatal.
sub enabled { return 1 }
sub fatal_enabled { return 0 }
sub enabled_at_level { return 1 }
sub fatal_enabled_at_level { return 0 }

sub register_categories { }

# warnings::warn([CATEGORY,] MESSAGE) - message is always the last argument.
sub warn {
    my $message = pop @_;
    CORE::warn($message);
}

# warnif: emit only when the category is enabled - which is always, here.
sub warnif {
    my $message = pop @_;
    CORE::warn($message);
}

1;

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
# as mro.  Regenerate after editing:  ./pl2cl --extension lib/warnings.pm > cl/pcl-warnings.lisp; tools/tag-license cl/pcl-warnings.lisp

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

# warnings::register_categories(NAME, ...) — perl's RUNTIME category
# allocator (task #875).  The static table lives in the runtime
# (cl/pcl-runtime.lisp, task #840) because `keys %warnings::Offsets` is READ
# before any warnings:: sub is CALLED; this is the other half — a module that
# declares its own category at load time (`version.pm` does exactly this, and
# it is the whole difference between perl's 81 keys and PCL's 80 after
# `require experimental`).
#
# perl keeps a separate `$LAST_BIT` counter starting at max+2 and advancing by
# 2 per new category.  Deriving the next offset from the TABLE instead gives
# the identical sequence — after k registrations perl's LAST_BIT is 158+2k and
# the table's max is 156+2k... i.e. max+2 IS LAST_BIT at every step — and it
# cannot drift out of step with the mirrored table the way a second constant
# could.  Probed against perl 5.40.3: first new category 160, next 162, an
# already-known name (including "all", offset 0) untouched.
#
# NOT implemented, deliberately: `use warnings::register`, which registers the
# CALLING package as a category.  It is a lexical pragma, and PCL skips those
# at parse time (*p-pragma-modules*), so there is no caller package to read at
# the point it would fire.  Perl's own users of it call register_categories
# explicitly as well, which is the path that works here.
sub register_categories {
    for my $name (@_) {
        next if defined $warnings::Offsets{$name};
        my $bit = 0;
        for my $off (values %warnings::Offsets) {
            $bit = $off if $off > $bit;
        }
        $warnings::Offsets{$name} = $bit + 2;
    }
    return;
}

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

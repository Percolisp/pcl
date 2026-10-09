# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# PCL FACTS OVERLAY for Moo::_Utils (task #2878; docs/shipped-modules.md
# "Facts overlays").  Declarations only: PCL reads this beside the real
# module's source; the real module still runs and installs the real subs.
package Moo::_Utils;

# Moo::_Utils builds this constant in a BEGIN block with a STRING eval
# (`eval "sub _in_global_destruction () { $gd_code }; 1"`), whose text a
# static parse does not compile.  Moo::sification imports it
# (`use Moo::_Utils qw(_in_global_destruction)`) and calls it bare.
sub _in_global_destruction ();

# The same block glob-assigns a `sub () { $gd_code }` to this one.
sub _in_global_destruction_code ();

1;

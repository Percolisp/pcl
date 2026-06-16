# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

# Test::More — PROTOTYPE-ONLY shim for PCL.
#
# PCL provides the actual Test::More TAP implementation INTERNALLY: `use
# Test::More` is intercepted by the runtime (cl/pcl-runtime.lisp) to load the
# TAP layer in cl/pcl-test.lisp.  This file is therefore NOT loaded at run time
# — it exists so the transpiler's prototype extractor
# (Pl::Parser::_extract_module_prototypes) can learn the assertion prototypes.
#
# Why it matters: the comparison assertions take their value arguments in
# SCALAR context (Perl prototypes ($$;$) etc.).  Without these prototypes the
# parser evaluates e.g. `is(try { 42 }, 42)` in the caller's (void) context, so
# wantarray() reports undef inside the callee and a context-sensitive argument
# (Try::Tiny's try, a bare `eval {}`, sort/reverse, …) returns the wrong thing.
# child_context() reads the `$` slots from here and imposes scalar context.
#
# Only the prototypes are declared (forward declarations, no bodies).  List/ref
# assertions (is_deeply, diag, note, plan, …) have no scalar-forcing prototype
# and are intentionally omitted — their arguments correctly inherit list/normal
# context.

package Test::More;

our $VERSION = '0.01';

sub ok      ($;$);
sub is      ($$;$);
sub isnt    ($$;$);
sub like    ($$;$);
sub unlike  ($$;$);
sub cmp_ok  ($$$;$);
sub isa_ok  ($$;$);
sub BAIL_OUT ($);

1;

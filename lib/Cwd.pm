# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

package Cwd;
use strict;

our @EXPORT    = qw(cwd getcwd);
our @EXPORT_OK = qw(cwd getcwd abs_path fast_abs_path realpath fast_cwd);

# No custom import: PCL imports @EXPORT / a requested subset automatically.
# (A hand-rolled Exporter here would just duplicate that.)

# getcwd/cwd: PCL transpiler maps these to p-getcwd/p-cwd (Config.pm line ~384)
sub getcwd { return getcwd() }
sub cwd    { return cwd() }

sub abs_path {
    my ($path) = @_;
    $path = cwd() unless defined $path;
    return $path if $path =~ m{^/};
    return cwd() . '/' . $path;
}

*fast_abs_path = \&abs_path;
*realpath      = \&abs_path;
*fast_cwd      = \&cwd;

1;

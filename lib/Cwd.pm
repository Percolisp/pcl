package Cwd;
use strict;

our @EXPORT    = qw(cwd getcwd);
our @EXPORT_OK = qw(cwd getcwd abs_path fast_abs_path realpath fast_cwd);

sub import {
    my $class = shift;
    my $pkg = caller;
    no strict 'refs';
    my @to_export = @_ ? @_ : @EXPORT;
    for my $fn (@to_export) {
        *{"${pkg}::${fn}"} = \&{"Cwd::${fn}"};
    }
}

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

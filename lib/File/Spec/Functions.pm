# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

package File::Spec::Functions;
use strict;

our @EXPORT = qw(
    catfile catdir splitdir splitpath rel2abs curdir updir rootdir
    file_name_is_absolute no_upwards path devnull tmpdir
);
our @EXPORT_OK = @EXPORT;
our %EXPORT_TAGS = ( DEFAULT => \@EXPORT, ALL => \@EXPORT );

# No custom import: PCL imports @EXPORT / a requested subset (and :DEFAULT/:ALL
# tags) automatically.  A hand-rolled Exporter here would just duplicate that.

sub catfile {
    my @parts = @_;
    return join('/', @parts);
}

sub catdir {
    my @parts = grep { defined $_ && $_ ne '' } @_;
    return '/' if @parts == 0;
    my $path = join('/', @parts);
    $path =~ s{//+}{/}g;
    return $path;
}

sub splitdir {
    my ($path) = @_;
    return split(/\//, $path, -1);
}

sub splitpath {
    my ($path, $no_file) = @_;
    $no_file //= 0;
    if ($path =~ m{^(.*/)?([^/]*)$}) {
        return ('', $1 // '', $no_file ? '' : $2);
    }
    return ('', '', $path);
}

sub rel2abs {
    my ($path, $base) = @_;
    return $path if defined $path && $path =~ m{^/};
    $base //= cwd();
    return $base if !defined $path || $path eq '' || $path eq '.';
    return $base . '/' . $path;
}

sub curdir  { return '.'; }
sub updir   { return '..'; }
sub rootdir { return '/'; }
sub devnull { return '/dev/null'; }

sub tmpdir {
    my $t = $ENV{TMPDIR};
    return $t if defined $t && length $t && $t =~ m{^/};
    return '/tmp';
}

sub file_name_is_absolute {
    my ($path) = @_;
    return (defined $path && $path =~ m{^/}) ? 1 : 0;
}

sub no_upwards {
    return grep { $_ ne '.' && $_ ne '..' } @_;
}

sub path {
    return split(/:/, $ENV{PATH} // '');
}

1;

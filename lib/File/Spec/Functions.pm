package File::Spec::Functions;
use strict;

our @EXPORT = qw(
    catfile catdir splitdir splitpath rel2abs curdir updir rootdir
    file_name_is_absolute no_upwards path
);
our @EXPORT_OK = @EXPORT;
our %EXPORT_TAGS = ( DEFAULT => \@EXPORT, ALL => \@EXPORT );

sub import {
    my $class = shift;
    my $pkg = caller;
    no strict 'refs';
    my @to_export;
    if (@_) {
        for my $item (@_) {
            if ($item eq ':DEFAULT' || $item eq ':ALL') {
                push @to_export, @EXPORT;
            } else {
                push @to_export, $item;
            }
        }
    } else {
        @to_export = @EXPORT;
    }
    for my $fn (do { my %seen; grep { !$seen{$_}++ } @to_export }) {
        *{"${pkg}::${fn}"} = \&{"File::Spec::Functions::${fn}"};
    }
}

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

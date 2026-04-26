package File::Spec;
use strict;

# Minimal Unix-only stub for PCL

sub catfile {
    my $class = shift if @_ > 0 && !ref($_[0]) && $_[0] eq 'File::Spec';
    my @parts = @_;
    return join('/', @parts);
}

sub catdir {
    my $class = shift if @_ > 0 && !ref($_[0]) && $_[0] eq 'File::Spec';
    my @parts = grep { $_ ne '' } @_;
    return '/' if @parts == 0;
    my $path = join('/', @parts);
    $path =~ s{//+}{/}g;
    return $path;
}

sub splitdir {
    my ($class, $path) = @_;
    $path = $class unless defined $path;  # handle non-OO call
    return split(/\//, $path, -1);
}

sub splitpath {
    my ($class, $path, $no_file) = @_;
    if (!defined $no_file) { $no_file = 0; }
    if ($path =~ m{^(.*/)([^/]*)$}) {
        return ('', $1, $no_file ? '' : $2);
    }
    return ('', '', $path);
}

sub rel2abs {
    my ($class, $path, $base) = @_;
    return $path if $path =~ m{^/};
    $base //= Cwd::cwd();
    return $base . '/' . $path;
}

sub curdir  { return '.'; }
sub updir   { return '..'; }
sub rootdir { return '/'; }

sub file_name_is_absolute {
    my ($class, $path) = @_;
    return $path =~ m{^/} ? 1 : 0;
}

sub no_upwards {
    my $class = shift;
    return grep { $_ ne '.' && $_ ne '..' } @_;
}

sub path {
    return split(/:/, $ENV{PATH} // '');
}

1;

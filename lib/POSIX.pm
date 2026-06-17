# Copyright (c) 2025-2026
# This is free software; you can redistribute it and/or modify it
# under the same terms as the Perl 5 programming language system itself.

# PCL stub for POSIX module
# Provides commonly-used POSIX constants and functions

package POSIX;

# Floating-point limits
use constant DBL_MAX => 1.7976931348623157e+308;
use constant DBL_MIN => 2.2250738585072014e-308;
use constant FLT_MAX => 3.4028234663852886e+38;
use constant FLT_MIN => 1.1754943508222875e-38;
use constant LDBL_MAX => 1.1897314953572317e+4932;  # approx for 80-bit extended

# Math constants  
use constant M_PI   => 3.14159265358979323846;
use constant M_E    => 2.71828182845904523536;
use constant M_SQRT2 => 1.41421356237309504880;
use constant M_LN2  => 0.69314718055994530942;
use constant M_LN10 => 2.30258509299404568402;

# Error codes (duplicates Errno)
use constant ENOENT => 2;
use constant EACCES => 13;
use constant EINVAL => 22;
use constant ERANGE => 34;
use constant ENOSYS => 38;
use constant EISDIR => 21;
use constant EBADF  => 9;

# Other constants
use constant INT_MAX => 2147483647;
use constant INT_MIN => -2147483648;
use constant LONG_MAX => 9223372036854775807;
use constant LONG_MIN => -9223372036854775808;
use constant UINT_MAX => 4294967295;

# Math functions (wrap to standard Perl/SBCL equivalents)
sub floor { use POSIX (); int($_[0]) == $_[0] ? $_[0] : (CORE::int($_[0]) - ($_[0] < 0 ? 1 : 0)) }
sub ceil  { use POSIX (); int($_[0]) == $_[0] ? $_[0] : (CORE::int($_[0]) + ($_[0] > 0 ? 1 : 0)) }
sub fmod  { my ($x,$y) = @_; $x - int($x/$y)*$y }
sub pow   { $_[0] ** $_[1] }
sub log2  { log($_[0]) / log(2) }
sub log10 { log($_[0]) / log(10) }

# String conversion
sub strtod { 0+$_[0] }
sub strtol { (int($_[0]), 0) }
sub strtoul { (int($_[0]), 0) }

# Process/IO stubs
sub SEEK_SET { 0 }
sub SEEK_CUR { 1 }
sub SEEK_END { 2 }

sub O_RDONLY { 0 }
sub O_WRONLY { 1 }
sub O_RDWR  { 2 }
sub O_CREAT { 64 }
sub O_TRUNC { 512 }
sub O_APPEND { 1024 }
sub O_EXCL  { 128 }

1;

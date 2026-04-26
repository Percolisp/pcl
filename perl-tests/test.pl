# Redirect: tests that don't chdir('t') first require "./test.pl" from perl-tests/.
# Load the actual stub from t/test.pl.
require './t/test.pl';
1;

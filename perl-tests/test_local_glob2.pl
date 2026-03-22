BEGIN {
    chdir 't' if -d 't';
    require './test.pl';
    set_up_inc( qw(. ../lib) );
}
plan(3);
$foo = "global";
{
    local(*foo);
    is($foo, undef, "inside local: should be undef");
    $foo = "local";
    is($foo, "local", "inside local: set to local");
}
is($foo, "global", "after local: should be global");

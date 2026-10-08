# pcl-shim: autodie without Fatal.pm's code generator (task #2873).
#
# perl's autodie (Fatal.pm) builds its replacement subs from STRING EVALS of
# generated code that reads prototype("CORE::NAME") and calls CORE::NAME with
# the argument count the prototype allows; under PCL those wrappers do not
# compile (CORE::open($_[0]) reaches the p-open macro with too few arguments),
# and with no import LIST nothing displaced the built-in at the call sites
# anyway.  This shim holds autodie's FACTS: which built-ins each tag names and
# how a failure is reported.  The GENERIC mechanism is PCL's builtin-override
# registry (Pl::Environment::builtin_override_target): a weak keyword imported
# into a package is called as that package's sub from the `use` on, and a
# `no` of a module with an unimport ends it there.
#
# What is NOT perl's (docs/not-supported.md "autodie"): the scope is the FILE
# from the `use` (or to a later `no autodie`), not the enclosing block -- perl
# scopes it lexically through %^H; $@ is an autodie::exception carrying the
# message, function, args, file, line and errno, without the rest of that
# class's interface (matches/eval_error/caller/...); and the built-ins of the
# :default list this file does not wrap are ANNOUNCED once on stderr when
# they are requested, never silently left dying-free.

package autodie;
use strict;
use warnings;

our $VERSION = '2.37';

# The names a bare `use autodie;` imports -- the :default set this shim wraps.
# A literal qw() list: PCL's import scan reads @EXPORT at transpile time.
our @EXPORT = qw(
    open close opendir closedir unlink rename mkdir rmdir chdir binmode
    read seek truncate chmod chown utime link symlink readlink umask
    flock sysopen sysread syswrite sysseek pipe kill fork
);

my %WRAPPED = map { $_ => 1 } @EXPORT;

# What each tag IMPORTS here: perl's tag (autodie 2.37 / Fatal.pm %TAGS,
# flattened) restricted to the built-ins this file wraps.  LITERAL on purpose:
# PCL's import scan reads `tag => [qw(...)]` pairs at transpile time
# (Parser::module_import_sets), and a name a tag lists is CALLED as this
# module's sub from the `use` on -- so a name it does not wrap must not be here.
our %EXPORT_TAGS = (
    file     => [qw(open close flock sysopen binmode truncate chmod chown)],
    filesys  => [qw(opendir closedir chdir link unlink rename mkdir symlink rmdir
              readlink umask utime)],
    ipc      => [qw(pipe kill)],
    socket   => [qw()],
    threads  => [qw(fork)],
    system   => [qw()],
    io       => [qw(read seek sysread sysseek syswrite open close flock sysopen
              binmode truncate chmod chown opendir closedir chdir link unlink rename
              mkdir symlink rmdir readlink umask utime pipe kill)],
    default  => [qw(read seek sysread sysseek syswrite open close flock sysopen
              binmode truncate chmod chown opendir closedir chdir link unlink rename
              mkdir symlink rmdir readlink umask utime pipe kill fork)],
    all      => [qw(read seek sysread sysseek syswrite open close flock sysopen
              binmode truncate chmod chown opendir closedir chdir link unlink rename
              mkdir symlink rmdir readlink umask utime pipe kill fork)],
);

# perl's own tags, for what a request names that is NOT wrapped (announced).
my %PERL_TAGS = (
    file     => [qw(open close flock sysopen fcntl binmode ioctl truncate chmod chown)],
    filesys  => [qw(opendir closedir chdir link unlink rename mkdir symlink rmdir
              readlink umask utime)],
    ipc      => [qw(msgctl msgget msgrcv msgsnd semctl semget semop shmctl shmget
              shmread pipe kill)],
    socket   => [qw(accept bind connect getsockopt listen recv send setsockopt
              shutdown socketpair socket)],
    threads  => [qw(fork)],
    system   => [qw(system exec)],
    io       => [qw(read seek sysread sysseek syswrite open close flock sysopen fcntl
              binmode ioctl truncate chmod chown opendir closedir chdir link unlink
              rename mkdir symlink rmdir readlink umask utime msgctl msgget msgrcv
              msgsnd semctl semget semop shmctl shmget shmread pipe kill accept bind
              connect getsockopt listen recv send setsockopt shutdown socketpair
              socket)],
    default  => [qw(read seek sysread sysseek syswrite open close flock sysopen fcntl
              binmode ioctl truncate chmod chown opendir closedir chdir link unlink
              rename mkdir symlink rmdir readlink umask utime msgctl msgget msgrcv
              msgsnd semctl semget semop shmctl shmget shmread pipe kill accept bind
              connect getsockopt listen recv send setsockopt shutdown socketpair
              socket fork)],
    all      => [qw(read seek sysread sysseek syswrite open close flock sysopen fcntl
              binmode ioctl truncate chmod chown opendir closedir chdir link unlink
              rename mkdir symlink rmdir readlink umask utime msgctl msgget msgrcv
              msgsnd semctl semget semop shmctl shmget shmread pipe kill accept bind
              connect getsockopt listen recv send setsockopt shutdown socketpair
              socket fork system exec)],
);

sub _expand {
    my @out;
    for my $a (@_) {
        if ($a =~ /\A:(.*)\z/s) {
            my $t = $PERL_TAGS{$1} or die "autodie: unknown tag '$a'\n";
            push @out, @$t;
        }
        else {
            push @out, $a;
        }
    }
    my %seen;
    return grep { !$seen{$_}++ } @out;
}

my %ANNOUNCED;

sub import {
    my ($class, @args) = @_;
    my $caller = caller;
    @args = (':default') if !@args;
    my (@missing);
    for my $name (_expand(@args)) {
        if ($WRAPPED{$name}) {
            no strict 'refs';
            *{"${caller}::$name"} = \&{"autodie::$name"};
        }
        else {
            push @missing, $name;
        }
    }
    my @new = grep { !$ANNOUNCED{$_}++ } @missing;
    print STDERR "PCL: autodie is not in effect for: @new\n" if @new;
    return;
}

# The registry ends the override at the `no autodie` statement; the subs stay
# installed for the call sites compiled before it.
sub unimport { return }

# ---- the report --------------------------------------------------------

package autodie::exception;
use overload '""' => sub { $_[0]{message} }, 'eq' => sub { "$_[0]" eq "$_[1]" },
             'bool' => sub { 1 }, fallback => 1;
sub function { $_[0]{function} }
sub args     { $_[0]{args} }
sub file     { $_[0]{file} }
sub line     { $_[0]{line} }
sub package  { $_[0]{package} }
sub errno    { $_[0]{errno} }
sub message  { $_[0]{message} }
sub caller   { $_[0]{caller} }
sub context  { 'scalar' }
sub return   { $_[0]{return} }

# matches('open') / matches(':io'): the function, or a tag naming it.
sub matches {
    my ($self, $what) = @_;
    (my $fn = $self->{function}) =~ s/\ACORE:://;
    return $fn eq $what ? 1 : 0 if $what !~ /\A:(.*)\z/s;
    return (grep { $_ eq $fn } @{ autodie::_perl_tag($1) }) ? 1 : 0;
}

package autodie;

sub _fmt_arg {
    my ($v) = @_;
    return 'undef' if !defined $v;
    return "$v" if ref $v;
    return "'$v'";
}

sub _perl_tag { $PERL_TAGS{ $_[0] } || [] }

sub _throw {
    my ($fn, $msg, $args, $ret) = @_;
    my $err = "$!";
    my ($pkg, $file, $line, $sub) = caller(1);
    # caller() names the compiled file under PCL, not the perl source
    # (task #233, caller fidelity DEFERRED): then no location is claimed.
    my $where = (defined $file && $file !~ /\.lisp\z/) ? " at $file line $line" : '';
    die bless {
        function => "CORE::$fn",
        args     => $args,
        errno    => $err,
        file     => $file,
        line     => $line,
        package  => $pkg,
        caller   => $sub,
        return   => $ret,
        message  => "$msg$where\n",
    }, 'autodie::exception';
}

sub _general {
    my ($fn, $args, $ret) = @_;
    _throw($fn, "Can't $fn(" . join(', ', map { _fmt_arg($_) } @$args) . "): $!",
           $args, $ret);
}

sub _mode_arg { sprintf '%#04o', $_[0] }

my %OPEN_MODE = ('<' => 'reading', '>' => 'writing', '>>' => 'appending',
                 '+<' => 'reading and writing', '+>' => 'reading and writing',
                 '+>>' => 'reading and appending');

sub open (*;$@) {
    # One-argument open (the file named by the package scalar of the handle's
    # name) is not modelled here: it fails, loudly, as perl's does when that
    # scalar is unset.
    if (@_ < 2) {
        $! = 2;    # ENOENT
        _throw('open', "Can't open(" . _fmt_arg($_[0]) . "): $!", [@_]);
    }
    my $ok;
    if    (@_ == 2) { $ok = CORE::open($_[0], $_[1]) }
    elsif (@_ == 3) { $ok = CORE::open($_[0], $_[1], $_[2]) }
    else            { $ok = CORE::open($_[0], $_[1], $_[2], @_[3 .. $#_]) }
    return $ok if $ok;
    my ($mode, $file);
    if (@_ >= 3) {
        ($mode, $file) = ($_[1], $_[2]);
    }
    elsif (@_ == 2 && defined $_[1]
           && $_[1] =~ /\A\s*(\+?(?:<|>>|>))?\s*(.*?)\s*\z/s && $2 ne '') {
        ($mode, $file) = ($1 // '<', $2);
    }
    if (defined $mode && @_ <= 3) {
        (my $bare = $mode) =~ s/\s+//g;
        if ($bare =~ /:/) {
            _throw('open', "Can't open '$file' with mode '$mode': '$!'", [@_]);
        }
        if (my $how = $OPEN_MODE{$bare}) {
            _throw('open', "Can't open '$file' for $how: '$!'", [@_]);
        }
    }
    _throw('open', "Can't open(" . join(', ', '$fh', map { _fmt_arg($_) } @_[1 .. $#_]) . "): $!", [@_]);
}

sub close (;*) {
    my $ok = @_ ? CORE::close($_[0]) : CORE::close();
    return $ok if $ok;
    _throw('close', "Can't close(" . (@_ ? "$_[0]" : '') . ") filehandle: '$!'", [@_]);
}

sub opendir (*$) {
    return 1 if CORE::opendir($_[0], $_[1]);
    _throw('opendir', "Can't opendir(\$fh, " . _fmt_arg($_[1]) . "): $!", [@_]);
}

sub closedir (*) {
    return 1 if CORE::closedir($_[0]);
    _throw('closedir', "Can't closedir(\$fh): $!", [@_]);
}

sub unlink (@) {
    my $n = CORE::unlink(@_);
    return $n if $n == @_;
    _general('unlink', [@_], $n);
}

sub rename ($$) {
    return 1 if CORE::rename($_[0], $_[1]);
    _general('rename', [@_]);
}

sub mkdir (_;$) {
    my $ok = @_ > 1 ? CORE::mkdir($_[0], $_[1]) : CORE::mkdir($_[0]);
    return $ok if $ok;
    return _throw('mkdir', "Can't mkdir(" . _fmt_arg($_[0]) . ", " . _mode_arg($_[1])
                           . "): '$!'", [@_]) if @_ > 1;
    _general('mkdir', [@_]);
}

sub rmdir (_) {
    return 1 if CORE::rmdir($_[0]);
    _general('rmdir', [@_]);
}

sub chdir (;$) {
    my $ok = @_ ? CORE::chdir($_[0]) : CORE::chdir();
    return $ok if $ok;
    _general('chdir', [@_]);
}

sub binmode (*;$) {
    my $ok = @_ > 1 ? CORE::binmode($_[0], $_[1]) : CORE::binmode($_[0]);
    return $ok if $ok;
    _general('binmode', [@_]);
}

sub read (*\$$;$) {
    # The `\$` slot arrives as a reference where the call site applied the
    # prototype, and as the (aliased) scalar itself where it did not.
    # (Through a copy of the reference: `${$_[1]}` as read's buffer does not
    # write back under PCL -- task #2921.)
    my $n;
    if (ref($_[1]) eq 'SCALAR') {
        my $buf = $_[1];
        $n = @_ > 3 ? CORE::read($_[0], $$buf, $_[2], $_[3])
                    : CORE::read($_[0], $$buf, $_[2]);
    }
    else {
        $n = @_ > 3 ? CORE::read($_[0], $_[1], $_[2], $_[3])
                    : CORE::read($_[0], $_[1], $_[2]);
    }
    return $n if defined $n;
    _throw('read', "Can't read($_[0], <BUFFER>, " . join(', ', @_[2 .. $#_]) . "): $!", [@_]);
}

sub seek (*$$) {
    return 1 if CORE::seek($_[0], $_[1], $_[2]);
    _throw('seek', "Can't seek(\$fh, " . _fmt_arg($_[1]) . ", " . _fmt_arg($_[2]) . "): $!", [@_]);
}

sub truncate ($$) {
    return 1 if CORE::truncate($_[0], $_[1]);
    _general('truncate', [@_]);
}

sub chmod (@) {
    my ($mode, @files) = @_;
    my $n = CORE::chmod($mode, @files);
    return $n if $n == @files;
    _throw('chmod', "Can't chmod(" . join(', ', _mode_arg($mode), map { _fmt_arg($_) } @files)
                    . "): $!", [@_], $n);
}

sub link ($$) {
    return 1 if CORE::link($_[0], $_[1]);
    _general('link', [@_]);
}

sub symlink ($$) {
    return 1 if CORE::symlink($_[0], $_[1]);
    _general('symlink', [@_]);
}

sub readlink (_) {
    my $r = CORE::readlink($_[0]);
    return $r if defined $r;
    _general('readlink', [@_]);
}

sub chown (@) {
    my ($uid, $gid, @files) = @_;
    my $n = CORE::chown($uid, $gid, @files);
    return $n if $n == @files;
    _general('chown', [@_], $n);
}

sub utime (@) {
    my ($at, $mt, @files) = @_;
    my $n = CORE::utime($at, $mt, @files);
    return $n if $n == @files;
    _general('utime', [@_], $n);
}

sub umask (;$) {
    my $r = @_ ? CORE::umask($_[0]) : CORE::umask();
    return $r if defined $r;
    _general('umask', [@_]);
}

sub flock (*$) {
    return 1 if CORE::flock($_[0], $_[1]);
    # A NON-BLOCKING lock that would block is an answer, not a failure: perl's
    # autodie returns false for it (LOCK_NB = 4).
    return '' if ($_[1] & 4) && ($!{EWOULDBLOCK} || $!{EAGAIN});
    _throw('flock', "Can't flock(\$fh, " . _fmt_arg($_[1]) . "): $!", [@_]);
}

sub sysopen (*$$;$) {
    my $ok = @_ > 3 ? CORE::sysopen($_[0], $_[1], $_[2], $_[3])
                    : CORE::sysopen($_[0], $_[1], $_[2]);
    return $ok if $ok;
    _throw('sysopen', "Can't sysopen(\$fh, " . join(', ', map { _fmt_arg($_) } @_[1 .. $#_])
                      . "): $!", [@_]);
}

sub sysread (*\$$;$) {
    my $n;
    if (ref($_[1]) eq 'SCALAR') {
        my $buf = $_[1];
        $n = @_ > 3 ? CORE::sysread($_[0], $$buf, $_[2], $_[3])
                    : CORE::sysread($_[0], $$buf, $_[2]);
    }
    else {
        $n = @_ > 3 ? CORE::sysread($_[0], $_[1], $_[2], $_[3])
                    : CORE::sysread($_[0], $_[1], $_[2]);
    }
    return $n if defined $n;
    _throw('sysread', "Can't sysread($_[0], <BUFFER>, $_[2]): $!", [@_]);
}

sub syswrite (*$;$$) {
    my $n = @_ > 3 ? CORE::syswrite($_[0], $_[1], $_[2], $_[3])
          : @_ > 2 ? CORE::syswrite($_[0], $_[1], $_[2])
          :          CORE::syswrite($_[0], $_[1]);
    return $n if defined $n;
    _throw('syswrite', "Can't syswrite($_[0], <BUFFER>): $!", [@_]);
}

sub sysseek (*$$) {
    my $r = CORE::sysseek($_[0], $_[1], $_[2]);
    return $r if $r;
    _throw('sysseek', "Can't sysseek(\$fh, " . _fmt_arg($_[1]) . ", " . _fmt_arg($_[2]) . "): $!", [@_]);
}

sub pipe (**) {
    return 1 if CORE::pipe($_[0], $_[1]);
    _throw('pipe', "Can't pipe(\$fh, \$fh): $!", [@_]);
}

sub kill (@) {
    my ($sig, @pids) = @_;
    my $n = CORE::kill($sig, @pids);
    return $n if $n == @pids;
    # Signal 0 only ASKS whether the processes exist: never a failure.
    return $n if $sig =~ /\A-?0\z/;
    _general('kill', [@_], $n);
}

sub fork () {
    my $pid = CORE::fork();
    return $pid if defined $pid;
    _throw('fork', "Can't fork(): $!", []);
}

1;

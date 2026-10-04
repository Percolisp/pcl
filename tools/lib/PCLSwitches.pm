# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

package PCLSwitches;
# PERL'S COMMAND-LINE SWITCHES, for `pcl` (task #2097) and a program's own
# `#!perl -SWITCHES` line (task #1702) -- the ONE parser and the ONE source
# expansion (s506f, design D1/D2).
#
# Two halves, and every caller uses the same two:
#
#   parse_argv(\@argv)      perl's argv GRAMMAR (not Getopt::Long's): clusters
#                           (-lane, -pi.bak, -0777, -l012, -F:), the switches
#                           whose argument is the rest of the cluster OR the
#                           next word (-e -E -I -M -m), those whose argument
#                           is attached only (-i -x -0 -l -C -d -D -V -F),
#                           `--`, `-`, the first non-switch word.  It returns
#                           an ORDERED event list, because order is semantics
#                           (`perl -l -0040` leaves $\ = "\n", `perl -0040 -l`
#                           makes it " ").  pcl's own --word options are
#                           recognised HERE too, so `pcl` has no second parser.
#                           Used by `pcl` and by tools/pclperl-for-tests.
#
#   expand_program($text, \@events, %o)
#                           perlrun's documented source equivalents, applied
#                           to the PROGRAM's text before it is compiled: the
#                           command-line events merged with the program's own
#                           #! line, -x's leading-garbage rule, `LINE: while
#                           (<>) { ... }` for -n/-p, -a/-F's `our @F = split`,
#                           -l's chomp, and every compile-time variable a
#                           switch sets ($/ $\ $^I, the -s variables) in a
#                           BEGIN block so a BEGIN in the program sees them.
#                           The opening text goes on the program's FIRST CODE
#                           LINE, so no line number moves (task #2460's rule);
#                           the closing text goes before __END__/__DATA__.
#                           Used by pl2cl only, for a PROGRAM (never a module,
#                           an extension or an eval string -- perl examines the
#                           #! line of the main program only, probed s506f).
#
# Core perl only: tools/lib is installed with the tree, and the compiler's
# dependencies are exactly PPI and Moo (Pl/t/core-deps-01.t).  The one thing
# this module cannot find without a parser -- where a program's code ENDS
# (a `__END__` inside a heredoc or POD is not the end) -- the caller supplies
# as a callback (pl2cl's is PPI's).
use strict;
use warnings;

# pcl's own long options: name => 1 (a flag) or 2 (takes a value, as
# `--name=VALUE` or `--name VALUE`).  They are recognised only BEFORE the
# program argument, exactly as `pcl` always parsed them.
our %PCL_LONG = (
  'check' => 1, 'check-stdin' => 2, 'check-keep' => 2, 'version' => 1,
  'verbose' => 1, 'no-cache' => 1, 'clear-cache' => 1, 'cache-info' => 1,
  'make-core' => 1, 'help' => 1,
);

# perl's exit status for a switch error is whatever errno the croak finds --
# 25 (ENOTTY) on every probe of perl 5.40.3 here, from a terminal, a pipe and
# /dev/null alike; 29 was seen once with stdout on a pipe.  One number, on
# purpose: a script testing `$? >> 8` after a typo'd switch sees perl's usual
# answer.
our $SWITCH_ERROR_STATUS = 25;

# The switch letters perl knows.  Kind:
#   flag   no argument
#   word   the rest of the cluster, else the NEXT word (-e -E -I -M -m)
#   rest   the rest of the cluster, possibly empty (-i -x -C -d -D -V -F)
#   num    leading digits of the rest (-0 -l), parsed per letter
my %KIND = (
  (map { $_ => 'flag' } qw(n p a c s w W X T t u U v h ? S f g)),
  (map { $_ => 'word' } qw(e E I M m)),
  (map { $_ => 'rest' } qw(i x C d D V F)),
  '0' => 'num', 'l' => 'num',
);

sub _unrecognized {
  my ($rest) = @_;
  return "Unrecognized switch: -$rest  (-h will show valid options).\n";
}

# Parse one CLUSTER ($word without its leading '-') into @$events.  $next is a
# coderef returning the next argv word (undef when there is none) for the
# `word` kind.  Returns undef, or an error event.
sub _parse_cluster {
  my ($c, $events, $next) = @_;
  while (length $c) {
    my $ch = substr($c, 0, 1);
    my $kind = $KIND{$ch};
    return ['!', _unrecognized($c), $SWITCH_ERROR_STATUS] if !defined $kind;
    substr($c, 0, 1, '');
    if ($kind eq 'flag') { push @$events, [$ch, undef]; next }
    if ($kind eq 'word') {
      my $arg = length $c ? $c : $next->();
      $c = '';
      my $err = _check_word_arg($ch, $arg);
      return $err if $err;
      push @$events, [$ch, defined $arg ? $arg : ''];
      next;
    }
    if ($kind eq 'rest') { push @$events, [$ch, $c]; $c = ''; next }
    # num
    if ($ch eq '0') {
      # The switch character IS the number's first digit (perl's grok_oct
      # starts AT it): -0 = "0", -00 = "00" (paragraph), -0777 (slurp);
      # -0xHHH is hexadecimal, but only with a digit after the x -- a bare
      # `-0x` is -0 followed by the -x switch (probed).
      if ($c =~ s/^([xX][0-9a-fA-F_]+)//) { push @$events, ['0', $1]; next }
      $c =~ s/^([0-7]*)//;
      push @$events, ['0', "0$1"];
      next;
    }
    # -l[octal]: up to three octal digits (four when the first is 0).
    my $digits = '';
    if ($c =~ /^[0-9]/) {
      my $max = substr($c, 0, 1) eq '0' ? 4 : 3;
      ($digits) = $c =~ /^([0-7]{0,$max})/;
      substr($c, 0, length $digits, '');
    }
    push @$events, ['l', $digits];
  }
  return undef;
}

# The -e / -M / -m argument errors perl reports (probed, perl 5.40.3).
sub _check_word_arg {
  my ($ch, $arg) = @_;
  if ($ch eq 'e' || $ch eq 'E') {
    return ['!', "No code specified for -$ch.\n", $SWITCH_ERROR_STATUS] if !defined $arg;
    return undef;
  }
  return undef if $ch eq 'I';
  # -M / -m
  my $spec = defined $arg ? $arg : '';
  (my $name = $spec) =~ s/^-//;
  $name =~ s/[= ].*//s;
  return ['!', "Module name required with -$ch option.\n", $SWITCH_ERROR_STATUS]
    if $name eq '';
  return ['!', "Invalid module name $name with -$ch option: contains single ':'.\n",
          $SWITCH_ERROR_STATUS]
    if $name =~ /(?<!:):(?!:)/;
  return undef;
}

# parse_argv(\@argv) -> {
#   events  => [ [LETTER, ARG], ... ]  perl switches in order.  ARG is undef
#              for a flag.  A switch ERROR ends the list as ['!', MSG, STATUS]
#              at the point perl would have stopped.
#   pcl     => { name => value }      pcl's --word options
#   words   => [ ... ]                the argv words that were perl switches,
#              as typed (pcl --check hands perl exactly these)
#   program => FILE | '-' | undef     undef = none named (no -e either:
#              perl then reads the program from STDIN)
#   args    => [ ... ]                the program's @ARGV
#   code    => [ ... ]                the -e/-E texts, in order
# }
sub parse_argv {
  my ($argv) = @_;
  my @w = @$argv;
  my (%r, @events, @words);
  $r{pcl} = {};
  my $has_code = 0;
  while (@w) {
    my $a = $w[0];
    last if $a !~ /^-/;
    if ($a eq '-') { shift @w; $r{program} = '-'; last }
    if ($a eq '--') { shift @w; last }
    if ($a =~ /^--([^=]+)(?:=(.*))?\z/s && $PCL_LONG{$1}) {
      my ($name, $val) = ($1, $2);
      shift @w;
      if ($PCL_LONG{$name} == 2) {
        $val = shift @w if !defined $val;
        $r{pcl}{$name} = $val;
      } else {
        $r{pcl}{$name} = 1;
      }
      next;
    }
    shift @w;
    my $start = @words;
    push @words, $a;
    my $err = _parse_cluster(substr($a, 1), \@events, sub {
      return undef if !@w;
      push @words, $w[0];
      return shift @w;
    });
    if ($err) { push @events, $err; last }
    $has_code ||= grep { $_->[0] eq 'e' || $_->[0] eq 'E' } @events;
  }
  $has_code = grep { $_->[0] eq 'e' || $_->[0] eq 'E' } @events;
  $r{program} = shift @w
    if !$has_code && !defined $r{program} && @w
       && !(@events && $events[-1][0] eq '!');
  $r{events} = \@events;
  $r{words} = \@words;
  $r{args} = \@w;
  $r{code} = [ map { $_->[1] } grep { $_->[0] eq 'e' || $_->[0] eq 'E' } @events ];
  return \%r;
}

# ---- -M / -m -------------------------------------------------------------

# The `use`/`no` text for one -M/-m argument, as perl builds it (perlrun):
#   -MMod            use Mod;
#   -MMod=a,b        use Mod split(/,/, q{a,b});   (the list a, b)
#   -M-Mod[=a,b]     no Mod ...;
#   -M'Mod qw(a b)'  use Mod qw(a b);              (a space: the rest verbatim)
#   -mMod            use Mod ();
#   -mMod=a,b        use Mod split(...)             (-m with = is -M)
sub use_line_for_M {
  my ($spec, $letter) = @_;
  $letter = 'M' if !defined $letter;
  my $verb = 'use';
  if ($spec =~ s/^-//) { $verb = 'no'; }
  if ($spec =~ /^([\w:]+)=(.*)$/s) {
    my ($mod, $imports) = ($1, $2);
    my @args = split /,/, $imports, -1;
    my $list = join(', ', map { my $a = $_; $a =~ s/(['\\])/\\$1/g; "'$a'" } @args);
    return "$verb $mod ($list); ";
  }
  return "$verb $spec (); " if $letter eq 'm' && $spec =~ /^[\w:]+\z/;
  return "$verb $spec; ";
}

# ---- the #! line -----------------------------------------------------------

# Switches perl refuses on a #! line, with its message (probed 5.40.3).
my %SHEBANG_REFUSED = map { $_ => 1 } qw(x E S V e f);

# shebang_events($first_line, $file) -> (\@events) or (undef, ERROR-EVENT)
# perl's rule (toke.c): line 1 starts with `#!` and contains "perl"; the
# switches start after "perl -" if present, else after the word containing
# "perl"; whitespace-separated words are switches while they start with '-'.
# A #! line without "perl" is not examined (perl would exec that interpreter
# instead -- PCL does not; the line is a comment).  Returns an empty list when
# there is nothing to examine.
sub shebang_events {
  my ($line, $file) = @_;
  return [] if !defined $line || $line !~ /\A#!/;
  my $rest;
  if ($line =~ /perl -(.*)\z/s) { $rest = "-$1" }
  elsif ($line =~ /perl(?!6)\S*(.*)\z/s) { $rest = $1 }
  else { return [] }
  $rest =~ s/[\r\n].*//s;
  my @events;
  for my $word (split ' ', $rest) {
    last if $word !~ /^-./;
    last if $word eq '--';
    my $c = substr($word, 1);
    my $first = substr($c, 0, 1);
    return (undef, ['!', qq{Too late for "-$c" option at $file line 1.\n}, 255])
      if $first eq 'M' || $first eq 'm';
    return (undef, ['!', "Can't emulate -$first on #! line at $file line 1.\n", 255])
      if $SHEBANG_REFUSED{$first};
    my $err = _parse_cluster($c, \@events, sub { undef });
    if ($err) {
      (my $m = $err->[1]) =~ s/\.\n\z/ at $file line 1.\n/;
      return (undef, ['!', $m, 255]);
    }
    for my $e (@events) {
      return (undef, ['!', "Can't emulate -$e->[0] on #! line at $file line 1.\n", 255])
        if $SHEBANG_REFUSED{$e->[0]};
    }
  }
  return \@events;
}

# ---- the state the switches build ------------------------------------------

# A fresh state: perl's defaults.  rs = $/ ("\n"); ors = $\ (undef);
# rs_set/ors_set say whether a switch changed them.
sub new_state {
  return { rs => "\n", ors => undef, rs_set => 0, ors_set => 0, inplace => undef,
           n => 0, p => 0, a => 0, l => 0, F => undef, s => 0, E => 0,
           mods => [], incs => [], flags => {} };
}

# Apply @$events to $st in order.
sub apply_events {
  my ($st, $events) = @_;
  for my $e (@$events) {
    my ($k, $arg) = @$e;
    if    ($k eq 'n') { $st->{n} = 1 }
    elsif ($k eq 'p') { $st->{p} = 1 }
    elsif ($k eq 'a') { $st->{a} = 1; $st->{n} = 1 }
    elsif ($k eq 'F') { $st->{F} = $arg; $st->{a} = 1; $st->{n} = 1 }
    elsif ($k eq '0') { _set_rs($st, $arg) }
    elsif ($k eq 'g') { $st->{rs} = undef; $st->{rs_set} = 1 }
    elsif ($k eq 'l') {
      $st->{l} = 1;
      $st->{ors_set} = 1;
      if (length $arg) { $st->{ors} = chr(oct($arg) & 0xFF) }
      elsif (defined $st->{rs} && $st->{rs} eq '') { $st->{ors} = "\n\n" }
      else { $st->{ors} = $st->{rs} }
    }
    elsif ($k eq 'i') { $st->{inplace} = $arg }
    elsif ($k eq 's') { $st->{s} = 1 }
    elsif ($k eq 'E') { $st->{E} = 1 }
    elsif ($k eq 'M' || $k eq 'm') { push @{ $st->{mods} }, [$k, $arg] }
    elsif ($k eq 'I') { push @{ $st->{incs} }, $arg }
    else { $st->{flags}{$k} = defined $arg ? $arg : 1 }
  }
  return $st;
}

sub _set_rs {
  my ($st, $num) = @_;
  $st->{rs_set} = 1;
  if ($num =~ /^[xX](.*)/) {
    (my $h = $1) =~ s/_//g;
    $st->{rs} = chr(hex $h);
    return;
  }
  my $v = oct($num);
  if ($v > 0377)                         { $st->{rs} = undef }
  elsif ($v == 0 && length($num) >= 2)  { $st->{rs} = '' }
  else                                  { $st->{rs} = chr($v) }
}

# ---- the expansion ---------------------------------------------------------

# A perl string literal for $s (undef -> `undef`), all-escapes so it is safe
# for any bytes and any codepoint.
sub _lit {
  my ($s) = @_;
  return 'undef' if !defined $s;
  return '"' . join('', map { sprintf '\\x{%x}', ord } split //, $s) . '"';
}

# The text that goes IN FRONT OF the program's first code line.
#   $loop  = the state the LOOP is built from (perl builds `LINE: while` once,
#            from the switches seen before the program's own #! line -- unless
#            that line turns -n or -p ON, which rebuilds it from the merged
#            state; probed: cmdline -n + `#!perl -l` does not chomp, cmdline
#            -l + `#!perl -n` does)
sub _prefix {
  my ($st, $loop, $o) = @_;
  my @begin;
  push @begin, '$/ = ' . _lit($st->{rs}) . ';' if $st->{rs_set};
  push @begin, '$\\ = ' . _lit($st->{ors}) . ';' if $st->{ors_set};
  push @begin, '$^I = ' . _lit($st->{inplace}) . ';' if defined $st->{inplace};
  push @begin, 'unshift @INC, ' . join(', ', map { _lit($_) } @{ $o->{shebang_incs} || [] }) . ';'
    if @{ $o->{shebang_incs} || [] };
  push @begin, _s_switch_code() if $st->{s};
  my $pre = '';
  $pre .= 'BEGIN { ' . join(' ', @begin) . ' } ' if @begin;
  # perl's -i, given no file to edit, says so as the run starts (after the
  # compile phase: a BEGIN that empties @ARGV silences it -- probed).
  $pre .= 'INIT { warn "-i used with no filenames on the command line, reading from STDIN.\n"'
        . ' if !@ARGV } ' if defined $st->{inplace};
  $pre .= "use feature ':5.40'; use builtin ':5.40'; " if $st->{E};
  $pre .= use_line_for_M($_->[1], $_->[0]) for @{ $st->{mods} };
  if ($loop->{n} || $loop->{p}) {
    $pre .= 'LINE: while (<>) { ';
    $pre .= 'chomp; ' if $loop->{l};
    $pre .= _split_code($loop->{F}) if $loop->{a};
  }
  return $pre;
}

# -s: perl's rudimentary switch parsing of the PROGRAM's arguments, done
# before the program compiles (perl.c S_init_postdump_symbols): every leading
# `-name` sets $main::name = 1, `-name=value` sets it to "value"; a lone `-`
# stops (and stays in @ARGV), `--` stops and is removed.
sub _s_switch_code {
  return 'while (@ARGV && $ARGV[0] =~ /^-/) { last if $ARGV[0] eq "-";'
       . ' my $pcl_s = shift @ARGV; last if $pcl_s eq "--";'
       . ' no strict "refs"; if ($pcl_s =~ /^-([^=]*)=(.*)\z/s) { ${"main::$1"} = $2 }'
       . ' else { ${"main::" . substr($pcl_s, 1)} = 1 } }';
}

# -a / -F: perl's own text (toke.c).  -F's pattern is used verbatim when it
# is wrapped in //, "" or '' (the closing delimiter present); otherwise it is
# a q-quoted STRING, which split treats as a pattern.
sub _split_code {
  my ($F) = @_;
  return "our \@F = split(' ', \$_, 0); " if !defined $F;
  if ($F =~ m{\A([/'"])} && index($F, $1, 1) > 0) {
    return "our \@F = split($F); ";
  }
  for my $d ("\x{1}", "\x{2}", "\x{3}", "\x{4}") {
    next if index($F, $d) >= 0;
    return "our \@F = split(q$d$F$d); ";
  }
  (my $q = $F) =~ s/([\\'])/\\$1/g;
  return "our \@F = split('$q'); ";
}

sub _suffix {
  my ($loop) = @_;
  return '' if !($loop->{n} || $loop->{p});
  return ";}continue{print or die qq(-p destination: \$!\\n);}\n" if $loop->{p};
  return ";}\n";
}

# Put $prefix on the first CODE line of $text, so every line keeps its number
# (task #2460): perl compiles its switch preamble into no line of its own.  A
# program that starts with POD gets it on the first line after its first =cut.
sub prefix_first_code_line {
  my ($prefix, $text) = @_;
  return $text if $prefix eq '';
  if ($text =~ /\A=[a-zA-Z]/) {
    return $text if $text =~ s/(^=cut\b[^\n]*\n)/$1$prefix/m;
    return "$text\n$prefix\n";
  }
  return $prefix . $text;
}

# expand_program($text, \@cmd_events, %o) -> ($new_text, undef)
#                                          | (undef, ERROR-EVENT)
#   %o: file      => the program's name as perl reports it (for messages)
#       end_of_code => sub ($text) -> byte offset where the program's code ends
#                    (the start of __END__/__DATA__, else of a trailing POD,
#                    else length) -- required only when -n/-p is in force
# The cmd events are the SOURCE-AFFECTING command-line switches (pcl hands
# them over; pl2cl gets none when run directly).  The program's own #! line is
# read here.  Text with neither comes back unchanged (the common case, and
# the cheap one: one regex on line 1).
sub expand_program {
  my ($text, $cmd, %o) = @_;
  $cmd ||= [];
  my $file = defined $o{file} ? $o{file} : '-';
  # -x: discard everything before the first `#!...perl` line; line numbers
  # then count from that line, as perl's do (probed).
  my ($x) = grep { $_->[0] eq 'x' } @$cmd;
  if ($x) {
    return (undef, ['!', "No Perl script found in input\n", 255])
      if $text !~ s/\A.*?^(?=#![^\n]*perl)//ms;
  }
  my ($line1) = $text =~ /\A(#![^\n]*)/;
  my ($sh, $err) = defined $line1 ? shebang_events($line1, $file) : ([]);
  return (undef, $err) if $err;
  return ($text, undef) if !@$cmd && !@$sh && !$x;
  my $st = apply_events(new_state(), $cmd);
  my %loop = map { $_ => $st->{$_} } qw(n p a l F);
  my @sh_incs = map { $_->[1] } grep { $_->[0] eq 'I' } @$sh;
  apply_events($st, [ grep { $_->[0] ne 'I' } @$sh ]);
  if (($st->{n} && !$loop{n}) || ($st->{p} && !$loop{p})) {
    %loop = map { $_ => $st->{$_} } qw(n p a l F);
  }
  if ($x && length $x->[1]) {
    $st->{chdir} = $x->[1];
  }
  my $prefix = _prefix($st, \%loop, { shebang_incs => \@sh_incs });
  $prefix = 'BEGIN { chdir ' . _lit($st->{chdir}) . ' or die "Can\'t chdir to '
          . _escape_dq($st->{chdir}) . ': $!\n" } ' . $prefix
    if defined $st->{chdir};
  my $suffix = _suffix(\%loop);
  if ($suffix ne '') {
    die "PCLSwitches: end_of_code callback required for -n/-p\n" if !$o{end_of_code};
    my $at = $o{end_of_code}->($text);
    my $head = substr($text, 0, $at);
    $head .= "\n" if length $head && $head !~ /\n\z/;
    $text = $head . $suffix . substr($text, $at);
  }
  return (prefix_first_code_line($prefix, $text), undef);
}

sub _escape_dq {
  my ($s) = @_;
  $s =~ s/([\\"\$\@])/\\$1/g;
  return $s;
}

# ---- carrying events from a driver to pl2cl --------------------------------

# One argv word, safe for any bytes in -F / -i / -M arguments: each event is
# LETTER . (0|1) . ARG, events joined by NUL (no argv word can hold a NUL),
# the whole hex-encoded.
sub encode_events {
  my ($events) = @_;
  my $s = join "\0", map { $_->[0] . (defined $_->[1] ? '1' . $_->[1] : '0') } @$events;
  utf8::encode($s) if utf8::is_utf8($s);
  return unpack 'H*', $s;
}

sub decode_events {
  my ($hex) = @_;
  return [] if !defined $hex || $hex eq '';
  my $s = pack 'H*', $hex;
  return [ map { [ substr($_, 0, 1), substr($_, 1, 1) eq '1' ? substr($_, 2) : undef ] }
           split /\0/, $s, -1 ];
}

# The events that change the PROGRAM'S SOURCE (what a driver hands pl2cl).
# -I is not one (pcl passes it as -I to pl2cl and the runtime); -e is the
# text itself.
my %SOURCE_AFFECTING = map { $_ => 1 } qw(n p a F l 0 g i s x E M m);
sub source_events {
  my ($events) = @_;
  return [ grep { $SOURCE_AFFECTING{ $_->[0] } } @$events ];
}

1;

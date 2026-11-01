#!/usr/bin/env perl
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# migrate-causes.pl -- rewrite the CPAN board's CAUSE column into the grammar
# (s531b, Fable ruling s531 #7; the grammar: docs/failure-cause-classes.md).
#
#   tools/migrate-causes.pl            # rewrite baselines/cpan-board14-fails.tsv in place
#   tools/migrate-causes.pl --check    # change nothing; list every row it cannot map; exit 1 if any
#   tools/migrate-causes.pl --dry-run  # print the changed rows, write nothing
#   tools/migrate-causes.pl --file F   # another copy of the board baseline
#
# KEPT IN THE TREE and IDEMPOTENT: a second run changes nothing.  It is re-run
# at the batch's final rebase, so rows another batch added meanwhile in
# today's spelling are mapped too, and `--check` lists any it cannot map --
# those are mapped BY HAND (a row in %HAND below), never left legacy.
#
# WHAT IT DOES, in order, per row:
#   1. a row in %HAND (keyed dist, file, description -- and the test number
#      where one description stands for rows with different causes) gets the
#      cause written there, or LEAVES the file (`LEAVE`: it passes now);
#   2. a cell already in the grammar is kept -- except a `#N` whose task holds
#      an UNREDUCED hunt for that file (%UNREDUCED below) and whose note says
#      so ("not isolated" / "not reduced"), which becomes `UNREDUCED #N ...`;
#   3. `NS:<section title> ...` becomes `NS:<id> ...` by %SPELLING (the title
#      is replaced; a spelling that is not the section's own wording is kept
#      after ` -- `);
#   4. a task-first cell naming a ruling (`#930 (NS:Lvalue subroutines ...)`)
#      becomes `NS:<id> -- #930 (NS:Lvalue ...)` (s486a: the ruling is the
#      class, the task is in the note);
#   5. anything else is LISTED by --check, never guessed.
# `--check` also lists every grammar row whose task KEY is closed, missing or
# id-less (CLOSE-WITH-ROWS, ruling 3): those are re-caused from a one-file run
# and entered in %HAND.

use strict;
use warnings;
use FindBin;
use lib "$FindBin::RealBin/lib";
use PCLPaths ();
use PCLCauses qw(parse_cause task_state);
use PCLNsIds qw(read_ids);
use Getopt::Long;

my ($CHECK, $DRY, $FILE);
GetOptions('check' => \$CHECK, 'dry-run' => \$DRY, 'file=s' => \$FILE)
    or die "usage: $0 [--check | --dry-run] [--file baselines/cpan-board14-fails.tsv]\n";
my $root  = PCLPaths::root($FindBin::RealBin);
$FILE   //= "$root/baselines/cpan-board14-fails.tsv";
my $store = $ENV{PCL_TASK_STORE} // "$root/dot-claude-in-home-dir/tasks/pcl";
my $ids   = read_ids("$root/docs/not-supported.md");
die "docs/not-supported.md ids: $_\n" for @{ $ids->{problems} };

# ── the board's not-supported SPELLINGS -> ids (s531b member 2) ────────────
# [the spelling after `NS:`, the id, keep the spelling as a note]
my @SPELLING = (
  ['caller() fidelity',                                   'ns-caller-fidelity', 0],
  ['caller() filename and line number',                   'ns-caller-fidelity', 1],
  ['Error messages: no "at FILE line N" location info',   'ns-error-location',  0],
  ['Error message text and format',                       'ns-error-text',      0],
  ['Error compatibility for invalid Perl input',          'ns-invalid-perl',    0],
  ['use bytes',                                           'ns-use-bytes',       0],
  ['DynaLoader / XS binary extensions',                   'ns-xs',              0],
  ['Warnings-gated diagnostics are absent',               'ns-warnings',        0],
  ['`DESTROY` called by garbage collector',               'ns-destroy-gc',      0],
  ['DESTROY at scope exit',                               'ns-destroy-gc',      1],
  ['Compress::Raw::Zlib (zlib) -- libz via sb-alien, TEMPORARY (task #3161)', 'ns-zlib', 0],
  ['prototype()',                                         'ns-prototype',       0],
  ["Code points above U+10FFFF (perl's extended UTF-8)",  'ns-above-10ffff',    0],
  ['Internals::* C-level introspection',                  'ns-internals',       0],
  ['Source filters',                                      'ns-source-filters',  0],
  ['Lvalue subroutines',                                  'ns-lvalue-subs',     0],
  ['B::Deparse',                                          'ns-b',               1],
  ['`given`/`when` / smart match',                        'ns-given-when',      0],
  ['given/when',                                          'ns-given-when',      1],
);
@SPELLING = sort { length($b->[0]) <=> length($a->[0]) } @SPELLING;   # longest first

# ── UNREDUCED: tasks that hold the hunt AND name the t-file (ruling 1) ─────
my %UNREDUCED = map { ($_ => 1) } qw(4087 3862 3618 3835 3852);

# ── THE HAND TABLE: decided by a person, reproduced by every re-run ────────
# Key: dist <TAB> t-file <TAB> description [<TAB> #num].  Value: the new
# cause, or LEAVE (the row passes now and leaves the baseline BY EDIT).
my $P099 = '#3282 (from the dist root the chunk prints 1..1536 and runs 0 rows -- was #1575, completed s516b '
         . '(the board chdirs to the dist root); re-caused s531b by a one-file run of 099_binary03.t: 1..1536, '
         . '"# No tests run!")';
my %HAND = (
  # the two "cause not isolated" prose rows: a task now holds the hunt
  "Text-CSV-2.04\t80_diag.t\tOK in list context" =>
    'UNREDUCED #4181 -- s522a: $csv->error_diag in list context prints 0||0|0|0 under perl AND PCL in isolation, '
    . 'scratch/s522a/pr/diag.pl; the row\'s is_deeply differs only inside the file -- not #1389 / #3611 by emulation',
  "Text-CSV-2.04\t80_diag.t\tCache debugging output" =>
    'UNREDUCED #4181 -- s523d: `$csv->_cache_diag` answers false after `open STDERR, \'>\', FILE`; was masked by #3611',
  # attributions by family, never reduced; the named tasks do not name the files
  "JSON-PP-4.18-0\t112_upgrade.t\t" =>
    'UNREDUCED #4182 -- #1749 probable (the same unpack/pack \'U\' family; not isolated)',
  "JSON-PP-4.18-0\t119_incr_parse_utf8.t\t" =>
    'UNREDUCED #4182 -- #1389 (utf8::downgrade never fails, so no "Wide character in subroutine entry") + #1749 '
    . '(the incremental parser\'s \u / utf8 rows; not isolated row by row)',
  "Text-CSV-2.04\t76_magic.t\tcolumns fetched on magic" =>
    'UNREDUCED #4183 -- was "#966 probable (a tied scalar bound by bind_columns; not isolated beyond the parse '
    . 'family)"; #966 is completed; one-file run s531b 43/1, got ARRAY(0x1) / expected ARRAY(0x2)',
  "Text-CSV-2.04\t70_rt.t\tLength" =>
    'UNREDUCED #4183 -- #1389 probable (newly reached after #3613; Text::CSV_PP 2206 `$ctx->{utf8} = utf8::is_utf8($src)` '
    . '-- the byte string reads as upgraded, so the field is not decoded; not emulated)',
  # closed keys, re-caused from one-file runs (CLOSE-WITH-ROWS)
  "Text-CSV-2.04\t10_base.t\t" =>
    '#4184 (`ok (new Text::CSV, "Indirect object notation")`: PCL passes the description to new, so ok() gets one '
    . 'argument and the row is unnamed -- was #966 + #1747, both completed; one-file run s531b 61/3)',
  "Mojo-DOM58-3.002-0\tcollection.t\t*PLAN*" =>
    '#4185 (_Collection.pm:101 `local (*{"${caller}::a"}, *{"${caller}::b"}) = ...` is refused, "Parser2 TODO: '
    . 'unsupported `local` target", and the file stops -- was #564, done (its silent half); one-file run s531b 14/1 '
    . 'rc 255; perl 20 rows)',
  (map { ("JSON-PP-4.18-0\t099_binary$_.t\t*FILE*" => $P099) } map { sprintf '%02d', $_ } 3 .. 15),
  # #3530 fixed by s521c: the four redirect-STDOUT rows pass (one-file run s531b 259/6, the six left are the NS rows)
  (map { ("IO-Compress-2.214-0\tcz-14gzopen.t\t\t#$_" => 'LEAVE') } 146, 150, 157, 161),
  # a task-first cell citing the not-supported.md section by name only
  "Path-Tiny-0.150-0\tfilesystem.t\tRead contents of /tmp/XXXX correctly" =>
    'NS:ns-escaping-filehandle -- #2006 PART 2 (the handle ->openw returned escapes its block, so it is not '
    . 'closed/flushed at the block\'s exit -- docs/not-supported.md; s519a, newly reached since #3374)',
  # perl itself TODO-fails these rows (the dist's own TODO; the oracle's PERLROW lines confirm)
  "Class-Method-Modifiers-2.15-0\t041-modify-parent.t\t # TODO pending discussion with stevan" =>
    'PERL-FAILS (the dist marks it pending; perl: not ok # TODO) -- reached since s519b #3373 (a parent named by '
    . '@ISA before it had a class died with a raw FINALIZE-INHERITANCE)',
  "URI-5.34-0\tescape-char.t\tcorrectly created a URI from a utf8-upgraded string # TODO URI::Escape::escape_char misunderstands utf8" =>
    'PERL-FAILS (the dist marks it "URI::Escape::escape_char misunderstands utf8"; perl: not ok # TODO)',
  "URI-5.34-0\tmailto.t\taddress with quoted local part containing spaces is parsed correctly # TODO We can't handle quoted local parts without properly parsing the email addresses" =>
    'PERL-FAILS (the dist marks it "We can\'t handle quoted local parts"; perl: not ok # TODO)',
  "YAML-Tiny-1.74\t86_fail.t\tComments in literal scalars # TODO Failing tests for things YAML::Tiny does wrong" =>
    'PERL-FAILS (the dist marks it "Failing tests for things YAML::Tiny does wrong"; perl: not ok # TODO)',
  # HTTP-Tiny's two no-TAP rows, re-measured: PCL prints perl's own skip line now
  "HTTP-Tiny-0.090-0\t190_find_CA.t\t*FILE*" =>
    'PERL-SKIP (perl: "Only run if HTTP::Tiny->can_ssl()" -- no IO::Socket::SSL / Net::SSLeay here; re-measured '
    . 's531b: PCL prints the same 1..0 # SKIP line, both sides skip)',
  "HTTP-Tiny-0.090-0\t200_live_local_ip.t\t*FILE*" =>
    'PERL-SKIP (network test: "Only run for $ENV{AUTOMATED_TESTING}", the dist skips it itself; re-measured '
    . 's531b: PCL prints the same 1..0 # SKIP line, both sides skip)',
);
# The seven #930 rows (Class-Method-Modifiers 140-lvalue.t): perl TODO-fails
# rows 6 and 7 (the oracle's PERLROW `# TODO` lines, s531b board run); row 6
# PASSES under PCL and is no blessed row, so ONE is PERL-FAILS and the other
# six map mechanically to `NS:ns-lvalue-subs -- #930 ...` (step 4).
$HAND{"Class-Method-Modifiers-2.15-0\t140-lvalue.t\tafter array lvalue attribute sets values # TODO can't apply after to array lvalue method"} =
  'PERL-FAILS (perl: not ok # TODO "can\'t apply after to array lvalue method", the oracle s531b) -- '
  . '#930 (NS:Lvalue subroutines — USER-DEFERRED)';
# The board run's PERL-FAILS CANDIDATE lines (s531b): perl TODO-fails this row too.
$HAND{"Class-Method-Modifiers-2.15-0\t060-caller.t\tparent with modifiers sees 'main' as caller # TODO caller magic not implemented yet"} =
  'PERL-FAILS (perl: not ok # TODO "caller magic not implemented yet", the oracle s531b) -- NS:caller() fidelity '
  . '[DEFERRED, USER s488] + #233 (a modifier must not appear as the caller)';
# The board run's MISMATCH lines (s531b): blessed PERL-SKIP, but perl RUNS the
# file on this box (Test::Pod 1.52 is installed) and PCL skips it -> FAIL.
$HAND{"IO-Compress-2.214-0\t999pod.t\t*FILE*"} =
  '#4186 (PCL skips a file perl RUNS: `use Test::Pod 1.00` dies, "Can\'t locate object method exported_to via '
  . 'package Test::Builder"; perl runs 31 rows -- was PERL-SKIP, a MISMATCH of the s531b oracle)';
$HAND{"Text-CSV-2.04\t00_pod.t\t*FILE*"} =
  '#4186 (PCL skips a file perl RUNS: `use Test::Pod 1.00` dies, "Can\'t locate object method exported_to via '
  . 'package Test::Builder"; perl runs 2 rows -- was PERL-SKIP, a MISMATCH of the s531b oracle)';
# NOT converted, recorded so nobody re-derives it: the oracle also prints
# PERL-FAILS CANDIDATE lines for Template-Toolkit compile2.t / compile3.t /
# compile5.t -- those files read the .ttc files compile1.t writes (#1688), and
# the oracle runs each file alone and unordered, so perl fails them too there.
# They are order effects, not perl's own failures; they stay keyed on their task.

# ── the rewrite ─────────────────────────────────────────────────────────────
sub ns_from_spelling {
    my ($text) = @_;            # the text right after an `NS:`
    for my $s (@SPELLING) {
        my ($sp, $id, $keep) = @$s;
        next unless index($text, $sp) == 0;
        my $rest = substr($text, length $sp);
        next if $rest =~ /^[\w]/;   # a longer word, not this spelling
        return ($id, $keep ? " -- $sp$rest" : $rest);
    }
    return;
}

sub map_cause {
    my ($dist, $file, $num, $desc, $cause) = @_;
    for my $k ("$dist\t$file\t$desc\t#$num", "$dist\t$file\t$desc") {
        return ($HAND{$k}, 'hand') if exists $HAND{$k};
    }
    my ($class, $key, $note, $legacy) = parse_cause($cause);
    if (!$legacy) {
        return ("UNREDUCED #$key$note", 'unreduced')
            if $class eq 'bug' && $UNREDUCED{$key} && $note =~ /not (?:isolated|reduced)/i;
        return ($cause, 'kept');
    }
    if ($cause =~ /^NS:\s*(.*)\z/s) {
        my ($id, $rest) = ns_from_spelling($1);
        return ("NS:$id$rest", 'spelling') if $id;
    }
    if ($cause =~ /^#\d+/ && $cause =~ /NS:\s*(.*)\z/s) {
        my ($id) = ns_from_spelling($1);
        return ("NS:$id -- $cause", 'task-first') if $id;
    }
    return (undef, 'unmapped');
}

open my $in, '<:raw', $FILE or die "$FILE: $!\n";
my @lines = <$in>;
close $in;
my (@out, @problems, @keyprob, %n);
for my $line (@lines) {
    if ($line =~ /^#/ || $line !~ /\S/) { push @out, $line; next }
    chomp(my $l = $line);
    my @f = split /\t/, $l, 7;
    if (@f < 7) { push @out, $line; push @problems, "short row (no cause column): $l"; next }
    my ($dist, $file, $num, $desc, $got, $exp, $cause) = @f;
    my ($new, $how) = map_cause($dist, $file, $num, $desc, $cause);
    $n{$how}++;
    if (!defined $new) {
        push @problems, "UNMAPPED $dist $file #$num [$desc]: " . substr($cause, 0, 120);
        push @out, $line;
        next;
    }
    if ($new eq 'LEAVE') { $n{left}++; print "LEAVE  $dist $file #$num [$desc]\n" if $DRY; next }
    # every mapped cell must land in the grammar with a live key
    my ($class, $key, undef, $legacy) = parse_cause($new);
    push @problems, "NOT IN THE GRAMMAR after mapping ($how) $dist $file [$desc]: " . substr($new, 0, 120) if $legacy;
    if (!$legacy && ($class eq 'not-supported') && (!$ids->{ids}{$key} || $ids->{ids}{$key}{retired})) {
        push @problems, "NS id $key is unknown or RETIRED: $dist $file [$desc]";
    }
    if (!$legacy && $class =~ /^(?:bug|unreduced|harness)$/) {
        my ($state, $st) = task_state($store, $key);
        push @keyprob, "KEY #$key is $state" . ($st ? " ($st)" : '') . ": $dist $file #$num [$desc] -- re-cause from a one-file run, then add a %HAND row"
            unless $state eq 'open';
    }
    $n{changed}++ if $new ne $cause;
    print "CHANGE $dist $file #$num [$desc]\n   was: $cause\n   now: $new\n" if $DRY && $new ne $cause;
    push @out, join("\t", $dist, $file, $num, $desc, $got, $exp, $new) . "\n";
}

printf STDERR "migrate-causes: %d row(s) -- %s\n", scalar(grep { !/^#/ && /\S/ } @lines),
    join(', ', map { "$_ " . ($n{$_} // 0) } qw(kept spelling task-first unreduced hand left changed unmapped));
print STDERR "  $_\n" for @problems, @keyprob;
# A stale task KEY is LISTED (and fails --check) but does not stop the write:
# the store an agent worktree reads is as old as its branch, and a row keyed on
# a task another batch closes is that batch's CLOSE-WITH-ROWS duty.
exit(@problems || @keyprob ? 1 : 0) if $CHECK || $DRY;
die "migrate-causes: NOT WRITTEN -- " . scalar(@problems) . " problem(s) above; map them in %HAND\n" if @problems;
if ($n{changed} || $n{left}) {
    my $note = "# MIGRATED to the cause GRAMMAR by tools/migrate-causes.pl (s531b; docs/failure-cause-classes.md \"The grammar\"):\n"
             . "#   every cause is a leading marker + key (#N / NS:<id> / PERL-SKIP / PERL-FAILS / UNREDUCED #N / HARNESS #N);\n"
             . "#   Pl/t/verdict-register-01.t checks each key (a live not-supported id, an OPEN task).  Re-run it after a rebase.\n";
    my $done = grep { /^# MIGRATED to the cause GRAMMAR/ } @out;
    if (!$done) {
        my $i = 0; $i++ while $i < @out && $out[$i] =~ /^#/;
        splice @out, $i, 0, $note;
    }
    open my $o, '>:raw', "$FILE.tmp" or die "$FILE.tmp: $!\n";
    print $o @out;
    close $o;
    rename "$FILE.tmp", $FILE or die "rename: $!\n";
    printf STDERR "migrate-causes: wrote %s (%d changed, %d left)\n", $FILE, $n{changed} // 0, $n{left} // 0;
}
exit 0;

package PCLNsIds;
# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# PCLNsIds -- the ONE reader of docs/not-supported.md's STABLE IDS (s531b,
# Fable ruling s531 #5).  tools/ns-ids.pl is its command line; the gate row
# Pl/t/verdict-register-01.t and tools/migrate-causes.pl call it.
#
# THE SHAPE: every `##` and `###` heading has, on the line directly above it,
#
#   <a id="ns-some-name"></a>                       a live section
#   <a id="ns-some-name"></a> <!-- RETIRED -->      no longer a limitation
#   <a id="ns-some-name"></a> <!-- RETIRED -> ns-other -->   ... and its successor
#
# The id is the KEY a cause cites (`NS:<id>`, docs/failure-cause-classes.md);
# the GitHub auto-anchors the Index links to follow the heading TEXT and move
# when a status tag changes, so they cannot be keys.

use strict;
use warnings;
use Exporter 'import';
our @EXPORT_OK = qw(read_ids check_ids);

use PCLCauses ();   # $NS_ID_RX: the id's spelling is the grammar's

# read_ids($path) -> { ids => { id => { line, level, heading, retired, successor } },
#                      order => [ id, ... ], problems => [ text, ... ] }
# A problem is anything --check must fail on: a heading with no anchor above
# it, an anchor not followed by a heading, a malformed or duplicate id, a
# successor that does not exist.  A missing file is ONE problem, never an
# empty set of ids that would make every citation look unknown for no reason.
sub read_ids {
    my ($path) = @_;
    my (%ids, @order, @problems);
    open my $fh, '<:raw', $path or return { ids => {}, order => [], problems => ["cannot read $path: $!"] };
    my @lines = <$fh>;
    close $fh;
    my $rx = $PCLCauses::NS_ID_RX;
    my $in_fence = 0;
    for my $i (0 .. $#lines) {
        my $l = $lines[$i];
        $in_fence = !$in_fence if $l =~ /^```/;
        next if $in_fence;
        my $ln = $i + 1;
        if ($l =~ /^<a id="([^"]*)"><\/a>(.*)$/) {
            my ($id, $tail) = ($1, $2);
            my $next = $lines[$i + 1] // '';
            push @problems, "line $ln: id \"$id\" is not of the form ns-name (lower case, digits, hyphens)"
                unless $id =~ /\A$rx\z/;
            push @problems, "line $ln: anchor $id is not directly above a ## / ### heading"
                unless $next =~ /^(#{2,3}) /;
            push @problems, "line $ln: duplicate id $id (first at line $ids{$id}{line})" if $ids{$id};
            my ($retired, $succ) = (0, undef);
            if ($tail =~ /<!--\s*RETIRED(?:\s*->\s*(\S+))?\s*-->/) { ($retired, $succ) = (1, $1) }
            elsif ($tail =~ /\S/) { push @problems, "line $ln: unexpected text after the anchor: $tail" }
            next if $ids{$id};
            my ($hashes, $heading) = $next =~ /^(#{2,3}) (.*?)\s*$/ ? ($1, $2) : ('', '');
            $ids{$id} = { line => $ln, level => length($hashes), heading => $heading,
                          retired => $retired, successor => $succ };
            push @order, $id;
            next;
        }
        if ($l =~ /^#{2,3} / && ($i == 0 || $lines[$i - 1] !~ /^<a id="/)) {
            chomp(my $h = $l);
            push @problems, "line $ln: heading has no stable id above it: $h";
        }
    }
    for my $id (@order) {
        my $s = $ids{$id}{successor};
        push @problems, "$id: RETIRED -> $s, and $s is not an id" if defined $s && !$ids{$s};
    }
    return { ids => \%ids, order => \@order, problems => \@problems };
}

# check_ids($path) -> list of problems (empty = clean).
sub check_ids { return @{ read_ids($_[0])->{problems} } }

1;

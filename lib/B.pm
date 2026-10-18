# Copyright (c) 2025-2026 the PCL authors
# This is free software; you can redistribute it and/or modify it under the
# same terms as the Perl 5 programming language system itself.
# SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

# pcl-shim: must-win -- the real module is XS.  This shim is found BEFORE @INC is
# searched, so a PERL5LIB or -I copy of the real module cannot shadow it
# (task #2462, docs/ir-spec.md 9, docs/shipped-modules.md).
#
# B without XS (task #3281, s518e).  The real B is perl's window onto its own
# interpreter: SVs, ops, pads, stashes.  PCL has none of those, so most of B
# cannot exist here.  What CPAN code actually CALLS was measured (Type::Tiny,
# Sub::Quote, Class::Method::Modifiers, CPAN::Meta::YAML, Sub::Identify, the
# dzil report-prereqs test) and is two kinds of thing:
#
#   * STRING QUOTERS -- perlstring / cstring, used to generate Perl source
#     that is then eval'ed.  They are pure functions of the string and are
#     written here from B.xs's own rules, byte for byte (Pl/t/b-shim-01.t
#     compares all 256 byte values and wide strings with perl's output).
#   * svref_2object -- a few FACTS about a sub or a scalar: the name a sub was
#     defined under (->GV->NAME, ->GV->STASH->NAME, ->STASH->NAME), whether it
#     is XS (->XSUB), whether it is defined (->START / ->ROOT isa B::NULL), and
#     a scalar's representation (->FLAGS, the number-vs-string idiom).  Each
#     is answered from a runtime fact (builtin::sub_name, builtin::sub_stash,
#     builtin::created_as_*), never guessed.
#
# Everything else DIES naming itself (rule 12): an unknown B:: function or
# constant, and every method of a B object that is not listed below, through
# the AUTOLOADs at the end.  B::Deparse is lib/B/Deparse.pm, which dies at
# require.  See docs/not-supported.md "B".
#
# NOT perl's (documented, not guessed): XSUB is 0 for every sub -- PCL runs
# no XS (the XS bridge is parked); a string holding only characters below 256
# is quoted as a BYTE string (octal escapes), because PCL strings carry no
# UTF-8 flag (#1389) -- perl quotes an upgraded one as \x{..}; a stash that
# was deleted or undefined is not tracked, so a sub perl has re-homed to
# __ANON__ still answers the package it was compiled in (#3492).

package B;

use strict;
no warnings;

our $VERSION = '1.89';

my %CONST = (
    SVf_IOK => 256, SVf_NOK => 512, SVf_POK => 1024, SVf_ROK => 2048,
    SVp_IOK => 4096, SVp_NOK => 8192, SVp_POK => 16384,
    SVf_PROTECT => 65536, SVs_PADTMP => 131072, SVs_TEMP => 524288,
    SVs_OBJECT => 1048576, SVs_GMG => 2097152, SVs_SMG => 4194304,
    SVs_RMG => 8388608, SVf_FAKE => 16777216, SVf_OOK => 33554432,
    SVf_READONLY => 134217728, SVf_UTF8 => 536870912, SVf_IVisUV => 2147483648,
    CVf_METHOD => 1, CVf_LVALUE => 2, CVf_CONST => 4, CVf_ISXSUB => 8,
    CVf_WEAKOUTSIDE => 16, CVf_CLONE => 32, CVf_CLONED => 64, CVf_ANON => 128,
    CVf_UNIQUE => 256, CVf_NODEBUG => 512, CVf_DYNFILE => 4096,
    CVf_AUTOLOAD => 8192, CVf_NAMED => 32768, CVf_LEXICAL => 65536,
    CVf_IsMETHOD => 1048576,
    SVt_PVGV => 9, SVt_PVHV => 12,
);
# The constants' () prototype is DECLARED (#3514): a constant imported by
# name is then a TERM at the importing site, as perl's XS constants are
# (`$f & CVf_ANON`, `SVf_POK + 0`).  The bodies are installed below.
sub CVf_ANON (); sub CVf_AUTOLOAD (); sub CVf_CLONE (); sub CVf_CLONED ();
sub CVf_CONST (); sub CVf_DYNFILE (); sub CVf_ISXSUB (); sub CVf_IsMETHOD ();
sub CVf_LEXICAL (); sub CVf_LVALUE (); sub CVf_METHOD (); sub CVf_NAMED ();
sub CVf_NODEBUG (); sub CVf_UNIQUE (); sub CVf_WEAKOUTSIDE ();
sub SVf_FAKE (); sub SVf_IOK (); sub SVf_IVisUV (); sub SVf_NOK ();
sub SVf_OOK (); sub SVf_POK (); sub SVf_PROTECT (); sub SVf_READONLY ();
sub SVf_ROK (); sub SVf_UTF8 (); sub SVp_IOK (); sub SVp_NOK ();
sub SVp_POK (); sub SVs_GMG (); sub SVs_OBJECT (); sub SVs_PADTMP ();
sub SVs_RMG (); sub SVs_SMG (); sub SVs_TEMP (); sub SVt_PVGV ();
sub SVt_PVHV ();
{
    no strict 'refs';
    for my $name (keys %CONST) {
        my $v = $CONST{$name};
        *{"B::$name"} = sub () { $v };
    }
}

our @specialsv_name = ('Nullsv', '&PL_sv_undef', '&PL_sv_yes', '&PL_sv_no',
                       '(SV*)pWARN_ALL', '(SV*)pWARN_NONE', '(SV*)pWARN_STD',
                       '&PL_sv_zero');

require Exporter;
our @ISA = ('Exporter');
# The export list is spelled LITERALLY (#3514): PCL's static export scan reads
# a literal list, and a constant it cannot see is not a TERM at the importing
# site (`use B qw(CVf_ANON); $f & CVf_ANON` read the bareword string).  Kept
# equal to the %CONST keys by Pl/t/b-shim-01.t.
our @EXPORT_OK = qw(
    CVf_ANON CVf_AUTOLOAD CVf_CLONE CVf_CLONED CVf_CONST CVf_DYNFILE
    CVf_ISXSUB CVf_IsMETHOD CVf_LEXICAL CVf_LVALUE CVf_METHOD CVf_NAMED
    CVf_NODEBUG CVf_UNIQUE CVf_WEAKOUTSIDE SVf_FAKE SVf_IOK SVf_IVisUV
    SVf_NOK SVf_OOK SVf_POK SVf_PROTECT SVf_READONLY SVf_ROK SVf_UTF8
    SVp_IOK SVp_NOK SVp_POK SVs_GMG SVs_OBJECT SVs_PADTMP SVs_RMG SVs_SMG
    SVs_TEMP SVt_PVGV SVt_PVHV
    svref_2object perlstring cstring class safename @specialsv_name);

# ---- the string quoters (B.xs cstring(), perlstyle on / off) -------------

my %MNEMONIC = ("\a" => '\a', "\b" => '\b', "\t" => '\t', "\n" => '\n',
                "\f" => '\f', "\r" => '\r', "\e" => '\e');

# A string with a character above 0xFF is perl's UTF-8 case: each character
# as sv_uni_display shows it (printable ASCII as itself, the mnemonic
# controls, else \x{HEX}), with " $ @ \ escaped.
sub _perlstring_wide {
    my ($s) = @_;
    my $out = '"';
    for my $c (split //, $s) {
        my $o = ord $c;
        if    ($c eq '"')  { $out .= '\\"' }
        elsif ($c eq '$')  { $out .= '\\$' }
        elsif ($c eq '@')  { $out .= '\\@' }
        elsif ($c eq '\\') { $out .= '\\\\' }
        elsif (exists $MNEMONIC{$c}) { $out .= $MNEMONIC{$c} }
        elsif ($o >= 0x20 && $o <= 0x7e) { $out .= $c }
        else  { $out .= sprintf('\\x{%x}', $o) }
    }
    return $out . '"';
}

# The byte case.  PERLSTYLE: perlstring (escape $ and @); otherwise cstring
# (\v, and the C trigraph guard: a ? followed by ? with 3+ bytes left).
sub _quote_bytes {
    my ($s, $perlstyle) = @_;
    my $out = '"';
    my $len = length $s;
    for (my $i = 0; $i < $len; $i++) {
        my $c = substr($s, $i, 1);
        my $o = ord $c;
        if    ($c eq '"')  { $out .= '\\"' }
        elsif ($c eq '\\') { $out .= '\\\\' }
        elsif (!$perlstyle && $c eq '?' && $len - $i >= 3
               && substr($s, $i + 1, 1) eq '?') { $out .= '\\077' }
        elsif ($perlstyle && $c eq '$') { $out .= '\\$' }
        elsif ($perlstyle && $c eq '@') { $out .= '\\@' }
        elsif ($o >= 0x20 && $o <= 0x7e) { $out .= $c }
        elsif ($c eq "\n") { $out .= '\\n' }
        elsif ($c eq "\r") { $out .= '\\r' }
        elsif ($c eq "\t") { $out .= '\\t' }
        elsif ($c eq "\a") { $out .= '\\a' }
        elsif ($c eq "\b") { $out .= '\\b' }
        elsif ($c eq "\f") { $out .= '\\f' }
        elsif (!$perlstyle && $o == 11) { $out .= '\\v' }
        else  { $out .= sprintf('\\%03o', $o) }
    }
    return $out . '"';
}

sub perlstring {
    my ($s) = @_;
    return '0' if !defined $s;
    $s = "$s";
    return _perlstring_wide($s) if $s =~ /[^\x00-\xff]/;
    return _quote_bytes($s, 1);
}

sub cstring {
    my ($s) = @_;
    return '0' if !defined $s;
    $s = "$s";
    if ($s =~ /[^\x00-\xff]/) {
        utf8::encode($s);       # cstring quotes the UTF-8 BYTES
    }
    return _quote_bytes($s, 0);
}

# ---- B.pm's own pure-Perl helpers ----------------------------------------

sub class {
    my $obj = shift;
    my $name = ref $obj;
    $name =~ s/^.*:://;
    return $name;
}

sub safename {
    my $name = shift;
    $name =~ s/^\c?/^?/
        or $name =~ s/^([\cA-\cZ\c\\c[\c]\c_\c^])/"^" . chr(64 ^ ord($1))/e;
    return $name;
}

# ---- svref_2object ---------------------------------------------------------

sub _die_unsupported {
    my ($what, $why) = @_;
    require Carp;
    Carp::croak("$what is not supported by PCL ("
                . ($why || 'PCL has no perl interpreter internals: no SVs, ops or pads')
                . '; see docs/not-supported.md "B")');
}

sub svref_2object {
    my ($ref) = @_;
    if (!ref $ref) {
        require Carp;
        Carp::croak("argument is not a reference");
    }
    my $type = builtin::reftype($ref);
    return bless { ref => $ref }, 'B::CV' if $type eq 'CODE';
    if ($type eq 'SCALAR' || $type eq 'VSTRING' || $type eq 'REF') {
        return bless { ref => $ref }, B::SV::_class_of($$ref);
    }
    return bless { ref => $ref }, 'B::AV' if $type eq 'ARRAY';
    return bless { ref => $ref }, 'B::HV' if $type eq 'HASH';
    if ($type eq 'GLOB') {
        no strict 'refs';
        return bless { ref => $ref, name => *{$ref}{NAME},
                       stash => *{$ref}{PACKAGE} }, 'B::GV';
    }
    _die_unsupported("B::svref_2object on a $type reference");
}

# The class hierarchy is perl's, so isa('B::SV') / isa('B::PVMG') answer as
# they do there.
@B::OBJECT::ISA = ();
@B::SV::ISA     = ('B::OBJECT');
@B::NULL::ISA   = ('B::SV');
@B::IV::ISA     = ('B::SV');
@B::NV::ISA     = ('B::SV');
@B::PV::ISA     = ('B::SV');
@B::PVIV::ISA   = ('B::PV', 'B::IV');
@B::PVNV::ISA   = ('B::PVIV', 'B::NV');
@B::PVMG::ISA   = ('B::PVNV');
@B::REGEXP::ISA = ('B::PVMG');
@B::PVLV::ISA   = ('B::GV');
@B::AV::ISA     = ('B::PVMG');
@B::HV::ISA     = ('B::PVMG');
@B::CV::ISA     = ('B::PVMG');
@B::GV::ISA     = ('B::PVMG');
@B::SPECIAL::ISA = ('B::OBJECT');
@B::OP::ISA     = ('B::OBJECT');
@B::COP::ISA    = ('B::OP');
@B::UNOP::ISA   = ('B::OP');

package B::SV;

# The representation of a scalar VALUE, as perl's SV type would show it.
sub _class_of {
    my $v = $_[0];
    return 'B::IV'   if ref $v;                    # an RV is an IV with ROK
    return 'B::NULL' if !defined $v;
    return 'B::PVMG' if builtin::is_vstring($v);
    return 'B::PVNV' if builtin::is_bool($v) || builtin::is_dual($v);
    return builtin::created_as_integer($v) ? 'B::IV' : 'B::NV'
        if builtin::created_as_number($v);
    return 'B::PV';
}

sub FLAGS {
    my $v = $_[0]->_value;
    return B::SVf_ROK() if ref $v;
    return 0 if !defined $v;
    my $pok = B::SVf_POK() | B::SVp_POK();
    my $iok = B::SVf_IOK() | B::SVp_IOK();
    my $nok = B::SVf_NOK() | B::SVp_NOK();
    return $pok | $iok | $nok if builtin::is_bool($v);
    if (builtin::created_as_number($v)) {
        return builtin::created_as_integer($v) ? $iok : $nok;
    }
    return $pok if !builtin::is_dual($v);
    B::_die_unsupported('B::SV::FLAGS of a dualvar',
                        'which numeric slot a dualvar caches is not recorded');
}

# The referenced VALUE.
sub _value { my $v = ${ $_[0]{ref} }; return $v }
sub object_2svref { $_[0]{ref} }
sub REFCNT { B::_die_unsupported('B::SV::REFCNT', 'PCL has no reference counts') }

package B::IV;
sub IV  { my $v = $_[0]->_value; ref $v ? B::_die_unsupported('B::IV::IV of a reference') : int($v) }
sub IVX { $_[0]->IV }
sub UVX { $_[0]->IV }
sub int_value { $_[0]->IV }
sub RV {
    my $v = $_[0]->_value;
    ref $v or B::_die_unsupported('B::IV::RV of a non-reference');
    return B::svref_2object($v);
}

package B::NV;
sub NV  { 0 + $_[0]->_value }
sub NVX { $_[0]->NV }

package B::PV;
sub PV  { my $v = $_[0]->_value; defined $v ? "$v" : undef }
sub PVX { $_[0]->PV }

package B::CV;

# The name the sub was defined under, as (STASH, NAME).  An anonymous sub's
# GV is PKG::__ANON__, PKG the package it was compiled in (builtin::sub_stash,
# #3492); a `\&{"name"}` stub's GV is in its name's package (its NAME stays
# __ANON__ here, the CVf_ANON reading); STASH undef = not recorded.
sub _name_parts {
    my $full = builtin::sub_name($_[0]{ref});
    return (builtin::sub_stash($_[0]{ref}) // builtin::sub_stub_package($_[0]{ref}),
            '__ANON__')
        if !defined $full;
    my ($stash, $name) = $full =~ /^(.*)::([^:]*)\z/s;
    return ($stash, $name);
}

sub GV {
    my ($stash, $name) = $_[0]->_name_parts;
    return bless { name => $name, stash => $stash, cv => $_[0] }, 'B::GV';
}
# CvSTASH: a named sub's defining package; an anonymous sub's compiling
# package (#3492).  A stub's is the package current when perl made it -- not
# recorded, so it dies (rule 12).
sub STASH {
    my $stash = defined builtin::sub_name($_[0]{ref})
        ? ($_[0]->_name_parts)[0] : builtin::sub_stash($_[0]{ref});
    defined $stash
        or B::_die_unsupported('B::CV::STASH of a sub with no recorded package',
                               'this sub was not created by a `sub` PCL compiled');
    return bless { name => $stash }, 'B::HV';
}
sub XSUB    { 0 }
sub XSUBANY { 0 }
sub START   { defined(&{ $_[0]{ref} }) ? bless({}, 'B::COP')  : bless({}, 'B::NULL') }
sub ROOT    { defined(&{ $_[0]{ref} }) ? bless({}, 'B::UNOP') : bless({}, 'B::NULL') }
sub object_2svref { $_[0]{ref} }
sub FILE     { B::_die_unsupported('B::CV::FILE', 'the file a sub was defined in is not recorded') }
# The CV flag bits PCL KNOWS (#3362, ruled s520 (1)): CVf_ANON (the sub has
# no recorded name), CVf_LVALUE / CVf_METHOD (its :lvalue / :method
# attributes).  Every OTHER bit reads 0 -- not perl's raw value (a constant
# sub's CVf_CONST|CVf_ISXSUB, CVf_NAMED, ... are not recorded); perl's own
# t/op/stash.t masks with CVf_ANON.  docs/not-supported.md "B".
sub CvFLAGS {
    my $ref = $_[0]{ref};
    my $f = 0;
    $f |= B::CVf_ANON() if !defined builtin::sub_name($ref);
    require attributes;
    for my $attr (attributes::get($ref)) {
        $f |= B::CVf_LVALUE() if $attr eq 'lvalue';
        $f |= B::CVf_METHOD() if $attr eq 'method';
    }
    return $f;
}
sub const_sv { B::_die_unsupported('B::CV::const_sv') }
sub PADLIST  { B::_die_unsupported('B::CV::PADLIST') }
sub DEPTH    { B::_die_unsupported('B::CV::DEPTH') }
sub NAME_HEK { B::_die_unsupported('B::CV::NAME_HEK') }

package B::GV;
sub NAME     { $_[0]{name} }
sub SAFENAME { B::safename($_[0]{name}) }
sub STASH {
    defined $_[0]{stash}
        or B::_die_unsupported('B::GV::STASH of a sub with no recorded package',
                               'this sub was not created by a `sub` PCL compiled');
    return bless { name => $_[0]{stash} }, 'B::HV';
}
sub LINE { B::_die_unsupported('B::GV::LINE', 'the line a glob was created on is not recorded') }
sub FILE { B::_die_unsupported('B::GV::FILE', 'the file a glob was created in is not recorded') }
sub SV   { B::_die_unsupported('B::GV::SV') }

package B::HV;
sub NAME {
    defined $_[0]{name}
        or B::_die_unsupported('B::HV::NAME of a hash that is not a stash');
    return $_[0]{name};
}

# The ops a defined sub's START / ROOT stand for: PCL has no op tree, so every
# accessor dies -- defined (not AUTOLOADed) for the ones callers probe with
# ->can, so a `can('line')` walk dies loudly instead of ending quietly.
package B::OP;
sub name { B::_die_unsupported('B::OP::name', 'PCL has no op tree') }
sub next { B::_die_unsupported('B::OP::next', 'PCL has no op tree') }
sub line { B::_die_unsupported('B::COP::line', 'PCL has no op tree') }
sub file { B::_die_unsupported('B::COP::file', 'PCL has no op tree') }

package B::OBJECT;
our $AUTOLOAD;
sub DESTROY { }
sub AUTOLOAD {
    my $m = $AUTOLOAD;
    B::_die_unsupported($m);
}

package B;
our $AUTOLOAD;
sub AUTOLOAD {
    my $m = $AUTOLOAD;
    _die_unsupported($m);
}

1;

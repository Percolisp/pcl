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
#     is answered from a runtime fact (builtin::sub_name,
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
# UTF-8 flag (#1389) -- perl quotes an upgraded one as \x{..}; the package an
# ANONYMOUS sub was compiled in is not recorded, so its ->STASH dies.

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
our @EXPORT_OK = (sort(keys %CONST),
                  qw(svref_2object perlstring cstring class safename
                     @specialsv_name));

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

# The name the sub was defined under, as (STASH, NAME); STASH undef = not
# recorded (an anonymous sub).
sub _name_parts {
    my $full = builtin::sub_name($_[0]{ref});
    return (undef, '__ANON__') if !defined $full;
    my ($stash, $name) = $full =~ /^(.*)::([^:]*)\z/s;
    return ($stash, $name);
}

sub GV {
    my ($stash, $name) = $_[0]->_name_parts;
    return bless { name => $name, stash => $stash, cv => $_[0] }, 'B::GV';
}
sub STASH {
    my ($stash) = $_[0]->_name_parts;
    defined $stash
        or B::_die_unsupported('B::CV::STASH of an anonymous sub',
                               'the package an anonymous sub was compiled in is not recorded');
    return bless { name => $stash }, 'B::HV';
}
sub XSUB    { 0 }
sub XSUBANY { 0 }
sub START   { defined(&{ $_[0]{ref} }) ? bless({}, 'B::COP')  : bless({}, 'B::NULL') }
sub ROOT    { defined(&{ $_[0]{ref} }) ? bless({}, 'B::UNOP') : bless({}, 'B::NULL') }
sub object_2svref { $_[0]{ref} }
sub FILE     { B::_die_unsupported('B::CV::FILE', 'the file a sub was defined in is not recorded') }
sub CvFLAGS  { B::_die_unsupported('B::CV::CvFLAGS', 'a sub carries no CV flags') }
sub const_sv { B::_die_unsupported('B::CV::const_sv') }
sub PADLIST  { B::_die_unsupported('B::CV::PADLIST') }
sub DEPTH    { B::_die_unsupported('B::CV::DEPTH') }
sub NAME_HEK { B::_die_unsupported('B::CV::NAME_HEK') }

package B::GV;
sub NAME     { $_[0]{name} }
sub SAFENAME { B::safename($_[0]{name}) }
sub STASH {
    defined $_[0]{stash}
        or B::_die_unsupported('B::GV::STASH of an anonymous sub',
                               'the package an anonymous sub was compiled in is not recorded');
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

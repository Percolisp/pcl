package Pl::PPIExpect;

# Pl::PPIExpect -- THE EXPECT-STATE CHECKER (s525p; parser-structure program
# step 0, member c1; design `docs/parser-review-s525.md` §4 Option 1a + §5 box;
# measurement `docs/expect-state-checker-s525.md`).
#
# INERT BY CONSTRUCTION.  It walks a PPI::Document's significant tokens in
# document order carrying perl's `PL_expect` exactly as toke.c does
# (perl.h:5912's eleven states), and for every token whose PPI CLASS
# contradicts that state it returns ONE ROW.  It never modifies the document,
# no compiler file loads it, and nothing it says changes an emission.  It is
# the measurement 1a is built on: the two tables below ARE the design of the
# one expect-state repair pass that replaces the ~35 repair passes.
#
# THE REFERENCE is perl 5.40.3's own lexer:
#   ~/perl5/perlbrew/build/perl-5.40.3/perl-5.40.3/toke.c
# Every table row cites the handler and line it is read from.  `toke NNNN`
# below means that line of that file.
#
# THE ONE POLICY DEVIATION from toke.c, stated once: perl decides a bareword
# by asking the symbol table ("is a sub of this name declared NOW?"), which a
# static walk cannot ask.  The checker answers from the document itself
# (`sub NAME` with its prototype, `use constant`) and, for a word declared
# nowhere in the file, takes a lower-case word as a CALL (list operator,
# XTERM next) and an ALL-CAPS word as a bareword (XOPERATOR next).  That is
# the valid-Perl assumption (CLAUDE.md principle 9) and exactly the test
# `Pl::Parser2::_repair_word_match` already applies (ppi-upstream-bugs §11).
# The static-parse families of `docs/static-parse-limits-review-s512.md`
# (imported prototypes, BEGIN-time subs) stay outside its reach, as they stay
# outside PCL's.
#
# RULE 12: a significant PPI token class no table row knows DIES naming it.
#
# Debugging aid: PPI_EXPECT_TRACE=1 prints every token with its state.

use strict;
use warnings;
use PPI;
use Scalar::Util qw(refaddr);
use Exporter 'import';

our @EXPORT_OK = qw(check_document check_source);

# The dispatch tables, filled further down (declared here for `use strict`).
our (%PRED, %HANDLER, %PSEUDO_STEP, %KEYWORD_STEP, %RECOVER, %FEATURE_KW);

my $TRACE = $ENV{PPI_EXPECT_TRACE} ? 1 : 0;

# ════════════════════════════════════════════════════════════════════════════
# THE STATES
# ════════════════════════════════════════════════════════════════════════════
#
# perl's eleven (perl.h:5912): XOPERATOR XTERM XREF XSTATE XBLOCK XATTRBLOCK
# XATTRTERM XTERMBLOCK XBLOCKTERM XPOSTDEREF XTERMORDORDOR.  A term-position
# state also carries a REQUIRED bit: after a binary or prefix operator a term
# MUST follow (perly: `term OP term`); after a list operator, a named unary,
# `(`, `,` or `;` it may be omitted (`shift;`, `print;`, `f(1,)`).  The log
# writes a required state with a trailing `!`.
#
# The checker's PSEUDO-STATES are refinements of a perl state at the points
# where toke.c reads ahead with a dedicated handler instead of PL_expect; each
# names the perl state and handler it stands for.
our %PSEUDO = (
  XCAST    => 'XREF via PREREF after a bare sigil (toke 236; yyl_dollar 5379, yyl_snail 6701, yyl_percent 6084, yyl_ampersand 6611, yyl_star 6036)',
  XMETHOD  => 'after ARROW (yyl_hyphen 5960-5978: force_word METHCALL0, `$` OPERATOR, else TERM)',
  XSUBNAME => 'after `sub` (yyl_sub 5535: the name, then XATTRBLOCK; no name = anon, XATTRTERM)',
  XDECL    => 'after my/our/state (yyl_my 7234: optional type word, then the variable or a list)',
  XATTRDECL=> 'the attribute list of a declaration (yyl_colon 6124 with PL_in_my)',
  XMODULE  => 'after use/no (tokenize_use, toke 8846/8455: module or version, then the import list)',
  XLABEL   => 'after next/last/redo/goto/dump (LOOPX toke 240: force_word, keywords excepted)',
  XFORVAR  => 'after a statement-start for/foreach (yyl_foreach 7128: my/our/state, the variable, the list)',
  XFORLIST => 'after the foreach loop variable (yyl_foreach 7128: the `(` must follow)',
  XPKG     => 'after package/class (toke 8525/8038: force_word, force_strict_version, then `;` or a block)',
);

my %TERMPOS = map { $_ => 1 } qw(XTERM XREF XSTATE XBLOCK XATTRBLOCK XATTRTERM
                                 XTERMBLOCK XBLOCKTERM XTERMORDORDOR);

# ════════════════════════════════════════════════════════════════════════════
# THE KEYWORD CLASS TABLE  (toke.c yyl_word_or_keyword 7944-8910)
# ════════════════════════════════════════════════════════════════════════════
#
# word => [ class, toke.c line of its `case KEY_word:` ].  The classes are
# toke.c's macros (toke 231-286, 2102):
#   LOP      LOP(OP_x, XTERM)  -- list operator, a term may follow
#   LOPREF   LOP(OP_x, XREF)   -- list operator taking a filehandle/block/sub
#   UNI      UNI(f) / UNIBRACK -- named unary, XTERM
#   UNIDOR   UNIDOR(f)         -- named unary that may be followed by //
#   FUN0     FUN0 / FUN0OP     -- takes nothing, XOPERATOR
#   FUN1     FUN1 (`not`)      -- XTERM
#   PREBLOCK PREBLOCK(..)      -- a block follows (XBLOCK)
#   OPERATOR OPERATOR(KW_x)    -- XTERM (local, elsif, given, field)
#   MODIFIER if/unless/while/until/when: OPERATOR(KW_x), statement or modifier
#   INFIX    an infix operator spelled as a word (eq, x, and, isa, ...)
#   LOOPX    LOOPX(f) toke 240 -- an optional label word follows
#   OLDLOP   `return`          -- list operator, no indirect-object slot
#   MY SUB DO EVAL REQUIRE USE FOR PACKAGE FORMAT PHASER QUOTE DATA -- the
#            keywords toke.c gives a dedicated handler (yyl_my, yyl_sub, ...)
# Extracted from toke.c mechanically (scratch s525p/probes/keywords.tsv),
# the dedicated-handler rows set by hand from the handler named.
our %KEYWORD = (
__FILE__          => ['FUN0',     7950], __LINE__          => ['FUN0',     7954], __PACKAGE__       => ['FUN0',     7958],
__DATA__          => ['DATA',     7965], __END__           => ['DATA',     7966], __SUB__           => ['FUN0',     7971],
__CLASS__         => ['FUN0',     7979], AUTOLOAD          => ['PHASER',   7982], DESTROY           => ['PHASER',   7983],
BEGIN             => ['PHASER',   7984], UNITCHECK         => ['PHASER',   7985], CHECK             => ['PHASER',   7986],
INIT              => ['PHASER',   7987], END               => ['PHASER',   7988], ADJUST            => ['PHASER',   7993],
abs               => ['UNI',      8003], alarm             => ['UNI',      8006], accept            => ['LOP',      8009],
and               => ['INFIX',    8012], atan2             => ['LOP',      8017], bind              => ['LOP',      8020],
binmode           => ['LOP',      8023], bless             => ['LOP',      8026], break             => ['FUN0',     8029],
catch             => ['PREBLOCK', 8032], chop              => ['UNI',      8035], class             => ['PACKAGE',  8038],
continue          => ['PREBLOCK', 8048], chdir             => ['UNI',      8060], close             => ['UNI',      8065],
closedir          => ['UNI',      8068], cmp               => ['INFIX',    8071], caller            => ['UNI',      8076],
crypt             => ['LOP',      8079], chmod             => ['LOP',      8083], chown             => ['LOP',      8086],
connect           => ['LOP',      8089], chr               => ['UNI',      8092], cos               => ['UNI',      8095],
chroot            => ['UNI',      8098], default           => ['PREBLOCK', 8101], defer             => ['PREBLOCK', 8104],
do                => ['DO',       8109], die               => ['LOP',      8112], defined           => ['UNI',      8116],
delete            => ['UNI',      8119], dbmopen           => ['LOP',      8122], dbmclose          => ['UNI',      8132],
dump              => ['LOOPX',    8135], else              => ['PREBLOCK', 8138], elsif             => ['OPERATOR', 8141],
eq                => ['INFIX',    8145], exists            => ['UNI',      8150], exit              => ['UNI',      8153],
eval              => ['EVAL',     8156], evalbytes         => ['UNI',      8167], eof               => ['UNI',      8171],
exp               => ['UNI',      8174], each              => ['UNI',      8177], exec              => ['LOPREF',   8180],
endhostent        => ['FUN0',     8183], endnetent         => ['FUN0',     8186], endservent        => ['FUN0',     8189],
endprotoent       => ['FUN0',     8192], endpwent          => ['FUN0',     8195], endgrent          => ['FUN0',     8198],
field             => ['OPERATOR', 8201], finally           => ['PREBLOCK', 8213], for               => ['FOR',      8218],
foreach           => ['FOR',      8219], formline          => ['LOP',      8222], fork              => ['FUN0',     8225],
fc                => ['UNI',      8228], fcntl             => ['LOP',      8231], fileno            => ['UNI',      8234],
flock             => ['LOP',      8237], gt                => ['INFIX',    8240], ge                => ['INFIX',    8245],
grep              => ['LOPREF',   8250], goto              => ['LOOPX',    8253], gmtime            => ['UNI',      8256],
getc              => ['UNIDOR',   8259], getppid           => ['FUN0',     8262], getpgrp           => ['UNI',      8265],
getpriority       => ['LOP',      8268], getprotobyname    => ['UNI',      8271], getprotobynumber  => ['LOP',      8274],
getprotoent       => ['FUN0',     8277], getpwent          => ['FUN0',     8280], getpwnam          => ['UNI',      8283],
getpwuid          => ['UNI',      8286], getpeername       => ['UNI',      8289], gethostbyname     => ['UNI',      8292],
gethostbyaddr     => ['LOP',      8295], gethostent        => ['FUN0',     8298], getnetbyname      => ['UNI',      8301],
getnetbyaddr      => ['LOP',      8304], getnetent         => ['FUN0',     8307], getservbyname     => ['LOP',      8310],
getservbyport     => ['LOP',      8313], getservent        => ['FUN0',     8316], getsockname       => ['UNI',      8319],
getsockopt        => ['LOP',      8322], getgrent          => ['FUN0',     8325], getgrnam          => ['UNI',      8328],
getgrgid          => ['UNI',      8331], getlogin          => ['FUN0',     8334], given             => ['OPERATOR', 8337],
glob              => ['LOP',      8343], hex               => ['UNI',      8346], if                => ['MODIFIER', 8349],
index             => ['LOP',      8355], int               => ['UNI',      8358], ioctl             => ['LOP',      8361],
isa               => ['INFIX',    8364], join              => ['LOP',      8367], keys              => ['UNI',      8370],
kill              => ['LOP',      8373], last              => ['LOOPX',    8376], lc                => ['UNI',      8379],
lcfirst           => ['UNI',      8382], local             => ['OPERATOR', 8385], length            => ['UNI',      8388],
lt                => ['INFIX',    8391], le                => ['INFIX',    8396], localtime         => ['UNI',      8401],
log               => ['UNI',      8404], link              => ['LOP',      8407], listen            => ['LOP',      8410],
lock              => ['UNI',      8413], lstat             => ['UNI',      8416], m                 => ['QUOTE',    8419],
map               => ['LOPREF',   8423], mkdir             => ['LOP',      8426], msgctl            => ['LOP',      8429],
msgget            => ['LOP',      8432], msgrcv            => ['LOP',      8435], msgsnd            => ['LOP',      8438],
our               => ['MY',       8441], my                => ['MY',       8442], state             => ['MY',       8443],
next              => ['LOOPX',    8446], ne                => ['INFIX',    8449], no                => ['USE',      8454],
not               => ['FUN1',     8458], open              => ['LOP',      8467], or                => ['INFIX',    8487],
ord               => ['UNI',      8493], oct               => ['UNI',      8496], opendir           => ['LOP',      8499],
print             => ['LOPREF',   8502], printf            => ['LOPREF',   8506], prototype         => ['UNI',      8510],
push              => ['LOP',      8513], pop               => ['UNIDOR',   8516], pos               => ['UNIDOR',   8519],
pack              => ['LOP',      8522], package           => ['PACKAGE',  8525], pipe              => ['LOP',      8531],
q                 => ['QUOTE',    8534], quotemeta         => ['UNI',      8542], qw                => ['QUOTE',    8545],
qq                => ['QUOTE',    8548], qr                => ['QUOTE',    8557], qx                => ['QUOTE',    8561],
return            => ['OLDLOP',   8568], require           => ['REQUIRE',  8571], reset             => ['UNI',      8574],
redo              => ['LOOPX',    8577], rename            => ['LOP',      8580], rand              => ['UNI',      8583],
rmdir             => ['UNI',      8586], rindex            => ['LOP',      8589], read              => ['LOP',      8592],
readdir           => ['UNI',      8595], readline          => ['UNIDOR',   8598], readpipe          => ['UNIDOR',   8601],
rewinddir         => ['UNI',      8604], recv              => ['LOP',      8607], reverse           => ['LOP',      8610],
readlink          => ['UNIDOR',   8613], ref               => ['UNI',      8616], s                 => ['QUOTE',    8619],
say               => ['LOPREF',   8626], chomp             => ['UNI',      8630], scalar            => ['UNI',      8633],
select            => ['LOP',      8636], seek              => ['LOP',      8639], semctl            => ['LOP',      8642],
semget            => ['LOP',      8645], semop             => ['LOP',      8648], send              => ['LOP',      8651],
setpgrp           => ['LOP',      8654], setpriority       => ['LOP',      8657], sethostent        => ['UNI',      8660],
setnetent         => ['UNI',      8663], setservent        => ['UNI',      8666], setprotoent       => ['UNI',      8669],
setpwent          => ['FUN0',     8672], setgrent          => ['FUN0',     8675], seekdir           => ['LOP',      8678],
setsockopt        => ['LOP',      8681], shift             => ['UNIDOR',   8684], shmctl            => ['LOP',      8687],
shmget            => ['LOP',      8690], shmread           => ['LOP',      8693], shmwrite          => ['LOP',      8696],
shutdown          => ['LOP',      8699], sin               => ['UNI',      8702], sleep             => ['UNI',      8705],
socket            => ['LOP',      8708], socketpair        => ['LOP',      8711], sort              => ['LOPREF',   8714],
split             => ['LOP',      8721], sprintf           => ['LOP',      8724], splice            => ['LOP',      8727],
sqrt              => ['UNI',      8730], srand             => ['UNI',      8733], stat              => ['UNI',      8736],
study             => ['UNI',      8739], substr            => ['LOP',      8742], method            => ['SUB',      8745],
format            => ['FORMAT',   8751], sub               => ['SUB',      8752], system            => ['LOPREF',   8755],
symlink           => ['LOP',      8758], syscall           => ['LOP',      8761], sysopen           => ['LOP',      8764],
sysseek           => ['LOP',      8767], sysread           => ['LOP',      8770], syswrite          => ['LOP',      8773],
tr                => ['QUOTE',    8776], y                 => ['QUOTE',    8777], tell              => ['UNI',      8781],
telldir           => ['UNI',      8784], tie               => ['LOP',      8787], tied              => ['UNI',      8790],
time              => ['FUN0',     8793], times             => ['FUN0',     8796], truncate          => ['LOP',      8799],
try               => ['PREBLOCK', 8802], uc                => ['UNI',      8806], ucfirst           => ['UNI',      8809],
untie             => ['UNI',      8812], until             => ['MODIFIER', 8815], unless            => ['MODIFIER', 8821],
unlink            => ['LOP',      8827], undef             => ['UNIDOR',   8830], unpack            => ['LOP',      8833],
utime             => ['LOP',      8836], umask             => ['UNIDOR',   8839], unshift           => ['LOP',      8842],
use               => ['USE',      8845], values            => ['UNI',      8849], vec               => ['LOP',      8852],
when              => ['MODIFIER', 8855], while             => ['MODIFIER', 8864], warn              => ['LOP',      8870],
wait              => ['FUN0',     8874], waitpid           => ['LOP',      8877], wantarray         => ['FUN0',     8880],
write             => ['UNI',      8883], x                 => ['INFIX',    8889], xor               => ['INFIX',    8901],
);

# The XREF list operators whose bareword/scalar after the keyword is a
# FILEHANDLE (the "F? L" rows of opcode.pl that yyl_strictwarn_bareword 7669
# names) -- `print STDERR`, `print $fh LIST`, `exec PROGRAM LIST`.
my %FH_LOP = map { $_ => 1 } qw(print printf say exec system);

# Statement keywords whose block the statement owns, and the continuation
# words perl lets follow that block in the SAME statement (perly.y `cond`,
# `else`, `cont`, the try/catch/finally rule).
my %CONTINUES = (
  if => [qw(elsif else)], elsif => [qw(elsif else)], unless => [qw(elsif else)],
  else => [], while => ['continue'], until => ['continue'], for => ['continue'],
  foreach => ['continue'], continue => [], try => [qw(catch finally)],
  catch => ['finally'], finally => [], defer => [], given => [], when => [],
  default => [], '' => ['continue'],
);

# ════════════════════════════════════════════════════════════════════════════
# (D2a) THE TRANSITION TABLE  -- (PPI class [, content]) x expect -> next
# ════════════════════════════════════════════════════════════════════════════
#
#   [ class,  content,  from,  to,  toke.c ]
#   class   PPI::Token:: suffix; a row matches the class or any subclass
#   content '' (any), an exact string, or a qr//
#   from    'OP' (XOPERATOR), 'TERM' (any term position), a state name, '*'
#   to      the next state.  A trailing `!` = a term is REQUIRED next.
#           `&name` = computed by the named handler below (the rows toke.c
#           decides by reading ahead).  `=` = the state is unchanged.
# First matching row wins.  The pseudo-state handlers (XSUBNAME, XDECL, ...)
# run before this table and consume the tokens their handler names.
our @TRANSITIONS = (
 # class               content                    from     to               toke.c
 [ 'Symbol',           '',                        'TERM', '&symbol',        'yyl_dollar 5528 PL_expect=XOPERATOR (+ the print-$fh rule 5484-5530); yyl_snail 6719; yyl_percent 6092; yyl_star 6036; yyl_ampersand 6614' ],
 [ 'Magic',            '',                        'TERM', '&symbol',        'as Symbol: scan_ident takes a punctuation name (yyl_dollar 5376)' ],
 [ 'ArrayIndex',       '',                        'TERM', 'XOPERATOR',      'yyl_dollar 5352-5371 DOLSHARP, PL_expect=XOPERATOR' ],
 [ 'Number',           '',                        'TERM', 'XOPERATOR',      '9555-9559 scan_num, TERM(THING)' ],
 [ 'Quote',            '',                        'TERM', 'XOPERATOR',      'yyl_sglquote 6919 / yyl_dblquote 6934 / q qq 8534 8548: TERM' ],
 [ 'QuoteLike',        '',                        'TERM', 'XOPERATOR',      'qw 5822, qr 8557, qx 8561, backtick 6966, <FH> 6849: TERM' ],
 [ 'Regexp',           '',                        'TERM', 'XOPERATOR',      'yyl_slash 6755 TERM(sublex_start()); m 8419, s 8619, tr/y 8776' ],
 [ 'HereDoc',          '',                        'TERM', 'XOPERATOR',      'yyl_leftpointy 6845-6850 scan_heredoc, PL_expect=XOPERATOR' ],
 [ 'Cast',             qr/^(?:[\$\@%&*]|\$#)\*$/, 'XMETHOD', 'XOPERATOR',   'postfix deref ->@* ->$#* ->%*: yyl_hyphen 5965 XPOSTDEREF, POSTDEREF 239' ],
 [ 'Cast',             qr/^[\@%]$/,               'XMETHOD', 'XPOSTDEREF',  'postfix slice ->@[ ->@{ ->%[ ->%{: yyl_hyphen 5966' ],
 [ 'Cast',             '\\',                      'TERM', 'XTERM!',         'yyl_backslash 6992 OPERATOR(REFGEN)' ],
 [ 'Cast',             '',                        'TERM', '&cast',          'a bare sigil: PREREF (toke 236), yyl_dollar 5379' ],
 [ 'Operator',         qr/^(?:\+\+|--)$/,         'OP',   'XOPERATOR',      'yyl_plus 6004 / yyl_hyphen 5949 TERM(POSTINC)' ],
 [ 'Operator',         qr/^(?:\+\+|--)$/,         'TERM', 'XTERM!',         'yyl_plus 6006 / yyl_hyphen 5951 OPERATOR(PREINC)' ],
 [ 'Operator',         qr/^-[A-Za-z]$/,           '*',    'XTERMORDORDOR',  'yyl_hyphen 5878-5936 FTST(f) (toke 244), before any expect test' ],
 [ 'Operator',         '->',                      'OP',   'XMETHOD',        'yyl_hyphen 5956-5978 ARROW' ],
 [ 'Operator',         '->',                      'TERM', 'XMETHOD',        'yyl_hyphen 5956: `shift->[0]` -- the named unary took no term, ARROW applies to its value' ],
 [ 'Operator',         qr/^(?:,|=>)$/,            '*',    'XTERM',          '9336-9340 OPERATOR(PERLY_COMMA); 9397-9404 `=>`' ],
 [ 'Operator',         '...',                     'XSTATE', 'XOPERATOR',    '9522-9524 OPERATOR(YADAYADA)' ],
 [ 'Operator',         ':',                       'XATTRTERM', '=',         'yyl_colon 6124: attribute list of an anon sub' ],
 [ 'Operator',         ':',                       'XATTRBLOCK', '=',        'yyl_colon 6124: attribute list of a named sub' ],
 [ 'Operator',         '',                        'TERM', 'XTERM!',         'prefix operator: yyl_bang 6683, yyl_tilde 6810, yyl_hyphen 5996 PERLY_MINUS, yyl_plus 6023' ],
 [ 'Operator',         '',                        'OP',   'XTERM!',         'binary operator: OPERATOR/Aop/Mop/ChEop/... (toke 232-261) all set XTERM' ],
 [ 'Structure',        ';',                       '*',    'XSTATE',         '9349-9355 PL_expect=XSTATE' ],
 [ 'Structure',        '(',                       '*',    '&open_paren',    'yyl_leftparen 6813-6823' ],
 [ 'Structure',        ')',                       '*',    '&close_paren',   'yyl_rightparen 6825-6836: PREBLOCK if `{` follows, else TERM' ],
 [ 'Structure',        '[',                       '*',    '&open_square',   'yyl_leftsquare 6758-6767 OPERATOR' ],
 [ 'Structure',        ']',                       '*',    '&close_square',  'yyl_rightsquare 6769-6790 TERM' ],
 [ 'Structure',        '{',                       '*',    '&open_curly',    'yyl_leftcurly 6310-6513' ],
 [ 'Structure',        '}',                       '*',    '&close_curly',   'yyl_rightcurly 6515-6561: PL_expect = the brackstack entry' ],
 [ 'Word',             '',                        '*',    '&word',          'yyl_keylookup 8984 -> yyl_word_or_keyword 7944 / yyl_just_a_word 7694' ],
 [ 'DashedWord',       '',                        'TERM', 'XOPERATOR',      'yyl_hyphen 5944 `-bareword`, then just_a_word' ],
 [ 'Label',            '',                        '*',    'XSTATE',         'a label is recognised only under XSTATE (yyl_word_or_keyword, the `word :` test)' ],
 [ 'Prototype',        '',                        'XATTRBLOCK', '=',        'yyl_sub 5535: the prototype / signature of a named sub' ],
 [ 'Prototype',        '',                        'XATTRTERM',  '=',        'yyl_sub 5535: the prototype / signature of an anon sub' ],
 [ 'Prototype',        '',                        '*',    '=',              '(out of place: the contradiction row says so)' ],
 [ 'Attribute',        '',                        '*',    '=',              'yyl_colon 6124 attribute list' ],
 [ 'Unknown',          '',                        '*',    'XOPERATOR',      '(PPI gave up on this text: the contradiction row says so)' ],
 [ 'Separator',        '',                        '*',    '&end',           'yyl_data_handle 6996: __END__/__DATA__ end the code' ],
);

# ════════════════════════════════════════════════════════════════════════════
# (D2b) THE CONTRADICTION TABLE  -- (PPI class [, content]) x expect -> kind
# ════════════════════════════════════════════════════════════════════════════
#
#   [ class, content, in, pred, kind, perl-reads, §, toke.c ]
#   in    'OP', 'TERM', 'TERM!' (a term is REQUIRED), 'TERM?' (optional),
#         'XTERM' (term position, not XTERMORDORDOR), a state name, '*'
#   pred  '' or the name of a predicate in %PRED (lookahead / lookbehind)
#   kind  the contradiction; perl-reads = what toke.c lexes there
#   §     the docs/ppi-upstream-bugs.md section the shape belongs to ('' = none)
# First matching row wins; a token with no matching row is consistent.
our @CONTRADICTIONS = (
 # class        content              in       pred                    kind                          perl-reads                  §    toke.c
 # ── tokens decided before PL_expect is consulted ──
 [ 'Magic',     '$$',                '*',     'adj_deref_follows',    'pid-magic-is-cast-run',      'Cast($) Cast($)',           1,  'yyl_dollar 5376 scan_ident: `$$` + `$`/`{`/name is a deref' ],
 [ 'Operator',  '-',                 '*',     'adj_filetest_after_phaser', 'filetest-split',        'Operator(-X)',              35, 'yyl_hyphen 5878: `-X` + non-word is a filetest in every state' ],
 [ 'Operator',  '-',                 '*',     'adj_filetest_word',    'filetest-split',             'Operator(-X)',              22, 'yyl_hyphen 5878: `-X` + non-word is a filetest in every state' ],
 [ 'Symbol',    '$',                 '*',     '',                     'bare-sigil-symbol',          'Magic($))',                 32, 'yyl_dollar 5376 scan_ident: `$)` is the GID variable' ],
 [ 'Operator',  '',                  '*',     'sole_in_deref_block',  'punct-name-in-braces',       'Magic(@+) (the ${ NAME } fold)', 9, 'yyl_dollar 5376 scan_ident: `{` + punctuation + `}` is a name' ],
 # ── after a bare sigil (XCAST): `{`, a name, or another sigil must follow ──
 [ 'Word',      '',                  'XCAST', 'adj_nonascii',         'cast-then-word',             'Symbol($NAME)',             23, 'yyl_dollar 5376 scan_ident (a UTF-8 IDFIRST name)' ],
 [ 'Word',      '',                  'XCAST', '',                     'cast-then-word',             'Symbol($NAME)',             '', 'yyl_dollar 5376 scan_ident' ],
 [ 'Operator',  '',                  'XCAST', 'cast_is_star',         'cast-then-punct',            'Symbol(*NAME)',             26, 'yyl_star 6031 scan_ident: a punctuation glob name' ],
 [ 'Operator',  '',                  'XCAST', '',                     'cast-then-punct',            'Symbol(@NAME)',             24, 'yyl_snail 6690 scan_ident: a punctuation array name' ],
 [ 'Number',    '',                  'XCAST', '',                     'cast-then-number',           'Symbol(@1)',                '', 'scan_ident: a digit-run name' ],
 [ 'Structure', qr/^[;,)\]}]$/,      'XCAST', 'cast_is_star',         'cast-without-operand',       'Symbol(*@)',                26, 'yyl_star 6031 scan_ident: a sigil-named glob' ],
 [ 'Structure', qr/^[;,)\]}]$/,      'XCAST', '',                     'cast-without-operand',       'Symbol',                    '', 'scan_ident' ],
 [ 'Operator',  '',                  'XCAST', '',                     'cast-then-punct',            'Symbol',                    '', 'scan_ident' ],
 [ 'Quote',     '',                  'XCAST', '',                     'cast-then-quote',            'Symbol',                    '', 'scan_ident' ],
 # ── term position: the ambiguous characters toke.c lexes as TERMS ──
 [ 'Operator',  qr{^//=?$},          'XTERM', '',                     'slash-as-division',          'Regexp::Match',             11, 'yyl_slash 6727: `//` is DORDOR only under XOPERATOR/XTERMORDORDOR, else scan_pat' ],
 [ 'Operator',  qr{^/=?$},           'TERM',  '',                     'slash-as-division',          'Regexp::Match',             11, 'yyl_slash 6740-6755: not XOPERATOR => scan_pat' ],
 [ 'Operator',  qr/^<(?:<|=|<=|=>)?$/, 'TERM', '',                    'lt-as-comparison',           'QuoteLike::Readline',       14, 'yyl_leftpointy 6842-6850: not XOPERATOR => scan_inputsymbol / scan_heredoc' ],
 [ 'Operator',  qr/^%=?$/,           'TERM',  '',                     'percent-as-modulo',          'Symbol(%NAME)',             24, 'yyl_percent 6081 scan_ident (not XOPERATOR)' ],
 [ 'Operator',  qr/^\*\*?=?$/,       'TERM',  'next_is_open_curly',   'star-as-multiply',           'Cast(*)',                   33, 'yyl_star 6031: not XOPERATOR => scan_ident, a glob' ],
 [ 'Operator',  qr/^\*\*?=?$/,       'TERM',  'next_is_scalar',       'star-as-multiply',           'Cast(*)',                   38, 'yyl_star 6031: not XOPERATOR => scan_ident, a glob' ],
 [ 'Operator',  qr/^\*\*?=?$/,       'TERM',  '',                     'star-as-multiply',           'Symbol(*NAME)',             26, 'yyl_star 6031: not XOPERATOR => scan_ident, a glob' ],
 [ 'Operator',  qr/^&=?$/,           'TERM',  'next_is_scalar',       'ampersand-as-bitand',        'Cast(&)',                   38, 'yyl_ampersand 6608: not XOPERATOR => scan_ident, a code call' ],
 [ 'Operator',  qr/^&=?$/,           'TERM',  '',                     'ampersand-as-bitand',        'Cast(&)',                   '', 'yyl_ampersand 6608' ],
 [ 'Operator',  qr/^x=?$/,           'TERM',  '',                     'x-as-repetition',            'Word(x) (a call)',          19, 'KEY_x 8889-8899: not XOPERATOR => just_a_word' ],
 [ 'Operator',  '~~',                'XTERM', '',                     'smartmatch-for-complements', 'Operator(~) Operator(~)',   21, 'yyl_tilde 6795: `~~` only under XOPERATOR/XTERMORDORDOR' ],
 [ 'Operator',  '^',                 'TERM',  'prev_adj_caret',       'logical-xor-split',          'Operator(^^)',              16, 'yyl_caret 6101: `^^` is one token (OROR, OP_XOR)' ],
 [ 'Operator',  '.',                 'TERM!', '',                     'missing-operand',            'a term',                    '', 'perly: `term . term`' ],
 [ 'Operator',  qr/^-[A-Za-z]$/,     'OP',    '',                     'filetest-after-term',        'a binary operator',         '', 'yyl_hyphen 5878 (a filetest after a term is a perl syntax error)' ],
 [ 'Operator',  'BINARY',            'TERM!', '',                     'missing-operand',            'a term',                    '', 'perly: a binary operator needs a left term' ],
 [ 'Structure', qr/^[;,)\]}]$/,      'TERM!', '',                     'missing-operand',            'a term',                    '', 'perly: an operator needs its right term' ],
 [ 'Label',     qr/^sub\s*:$/,       'TERM',  '',                     'label-not-at-statement-start','Word(sub) Operator(:) Attribute', 7, 'a label only under XSTATE; `sub` is never a label' ],
 [ 'Label',     '',                  '*',     'not_xstate',           'label-not-at-statement-start','an expression',            '', 'a label only under XSTATE' ],
 [ 'Word',      '',                  'XATTRBLOCK', '',                'word-after-sub-name',        'one sub NAME',              '', 'yyl_sub 5535 scan_word6: the sub name is ONE word (`main::::x`)' ],
 [ 'Prototype', '',                  '*',     'not_xattr',            'prototype-out-of-place',     'a list',                    '', 'yyl_sub 5535' ],
 # ── operator position: the terms PPI built where an operator stands ──
 [ 'Symbol',    qr/^\*/,             'OP',    '',                     'glob-for-multiply',          'Operator(*) Word',          12, 'yyl_star 6044-6062: XOPERATOR => Mop(OP_MULTIPLY)' ],
 [ 'Magic',     qr/^%[-+]$/,         'OP',    '',                     'magic-hash-for-modulo',      'Operator(%) Operator(-)',   4,  'yyl_percent 6067-6075: XOPERATOR => Mop(OP_MODULO)' ],
 [ 'Symbol',    qr/^%/,              'OP',    '',                     'hash-for-modulo',            'Operator(%)',               '', 'yyl_percent 6067: XOPERATOR => Mop(OP_MODULO)' ],
 [ 'Symbol',    qr/^&/,              'OP',    '',                     'code-for-bitand',            'Operator(&)',               '', 'yyl_ampersand 6578: XOPERATOR => BAop' ],
 [ 'Number',    qr/^-/,              'OP',    'prev_nonint_number',   'minus-in-number',            'Operator(-) Number',        34, 'yyl_hyphen 5979-5987: XOPERATOR => Aop(OP_SUBTRACT)' ],
 [ 'Number',    qr/^-/,              'OP',    '',                     'minus-in-number',            'Operator(-) Number',        15, 'yyl_hyphen 5979-5987: XOPERATOR => Aop(OP_SUBTRACT)' ],
 [ 'Number',    qr/^\.\d/,           'OP',    'prev_adj_number',      'hexfloat-split',             'one Number (0x1.8p+1)',     2,  'scan_num: a hex float is one literal' ],
 [ 'Number',    qr/^\.\d/,           'OP',    '',                     'dot-in-number',              'Operator(.) Number',        28, '9526: XOPERATOR => Aop(OP_CONCAT)' ],
 [ 'Word',      qr/^[pP]\d*$/,       'OP',    'prev_adj_radix_number','hexfloat-split',             'one Number (0x1p0)',        2,  'scan_num: a hex float is one literal (the exponent needs no dot)' ],
 [ 'Word',      qr/^-/,              'OP',    '',                     'minus-in-word',              'Operator(-) Word',          25, 'yyl_hyphen 5979: XOPERATOR => Aop(OP_SUBTRACT)' ],
 [ 'Word',      '',                  'OP',    'word_not_infix',       'word-after-term',            'an operator',               '', 'yyl_just_a_word 7704: a bareword under XOPERATOR is no_op' ],
 [ 'Regexp::Match', '',              'OP',    '',                     'match-for-division',         'Operator(/)',               '', 'yyl_slash 6727-6738: XOPERATOR => Mop(OP_DIVIDE) / DORDOR' ],
 [ 'Symbol',    '',                  'OP',    '',                     'term-after-term',            'an operator',               '', 'yyl_dollar 5368/5393 no_op("Scalar")' ],
 [ 'Magic',     '',                  'OP',    '',                     'term-after-term',            'an operator',               '', 'yyl_dollar 5393 no_op' ],
 [ 'ArrayIndex','',                  'OP',    '',                     'term-after-term',            'an operator',               '', 'yyl_dollar 5360 no_op("Array length")' ],
 [ 'Number',    '',                  'OP',    '',                     'term-after-term',            'an operator',               '', '9557 no_op("Number")' ],
 [ 'Quote',     '',                  'OP',    '',                     'term-after-term',            'an operator',               '', 'yyl_sglquote 6919 no_op("String")' ],
 [ 'QuoteLike', '',                  'OP',    '',                     'term-after-term',            'an operator',               '', 'no_op' ],
 [ 'Regexp',    '',                  'OP',    '',                     'term-after-term',            'an operator',               '', 'no_op' ],
 [ 'HereDoc',   '',                  'OP',    '',                     'term-after-term',            'Operator(<<)',              '', 'yyl_leftpointy 6855: XOPERATOR => SHop(OP_LEFT_SHIFT)' ],
 [ 'Cast',      '',                  'OP',    '',                     'cast-for-binop',             'a binary operator',         '', 'yyl_star 6044 / yyl_percent 6067 / yyl_ampersand 6578: XOPERATOR => the binary operator' ],
 [ 'Operator',  qr/^(?:!|~|\\)$/,    'OP',    '',                     'prefix-after-term',          'a binary operator',         '', 'perly: a prefix operator after a term' ],
 [ 'Label',     '',                  'OP',    '',                     'label-after-term',           'an operator',               '', 'a label only under XSTATE' ],
 [ 'Unknown',   '',                  '*',     '',                     'unknown-token',              'a token',                   '', '(PPI::Token::Unknown: PPI could not classify the text)' ],
);

# Operators that are BINARY ONLY: under a term-position state they stand for
# "the term was omitted" (legal after a list operator / named unary, an error
# after an operator).  The prefix-capable ones (- + ! ~ \ ++ -- not) and the
# ambiguous characters toke.c re-lexes as terms (/ < % * & x ~~ . <<) are not
# in this set; their rows above decide them.
my %BINARY = map { $_ => 1 } qw(
  = == != += -= *= /= .= %= x= **= ||= &&= //= |= &= ^= <<= >>= &.= |.= ^.=
  || && // or and xor ? : =~ !~ .. ... <=> <= >= > lt gt le ge eq ne cmp isa
  | ^ |. ^. &. >> ** ^^ ~~ -> );
$CONTRADICTIONS[$_][1] eq 'BINARY' and $CONTRADICTIONS[$_][1] = \%BINARY
  for 0 .. $#CONTRADICTIONS;

# ════════════════════════════════════════════════════════════════════════════
# PREDICATES  (the lookahead / lookbehind the tables name)
# ════════════════════════════════════════════════════════════════════════════
my %FILETEST = map { $_ => 1 } split //, 'rwxoRWXOezsfdlpSugkbctTBMAC';

%PRED = (
  adj_deref_follows => sub {      # `$$` glued to `$`, `@`, a name or `{`
    my ($c) = @_; my $n = $c->{adj} or return 0;
    return 1 if $n->isa('PPI::Token::Structure') && $n->content eq '{';
    return $n->content =~ /^[\$\w]/ ? 1 : 0;
  },
  adj_filetest_word => sub {      # Operator(-) glued to Word(X), X a filetest letter
    my ($c) = @_; my $n = $c->{adj} or return 0;
    return 0 unless $n->isa('PPI::Token::Word') && $FILETEST{$n->content};
    my $nn = $c->{toks}[$c->{i} + 2];
    return 0 if $nn && $nn->isa('PPI::Token::Operator') && $nn->content eq '=>';
    return 0 if _in_subscript_forced_word($c);
    return 1;
  },
  adj_filetest_after_phaser => sub {
    my ($c) = @_;
    return 0 unless $PRED{adj_filetest_word}->($c);
    my $p = $c->{prev} or return 0;
    return 0 unless $p->isa('PPI::Token::Structure') && $p->content eq '}';
    my $blk = $p->parent;
    my $ps = $blk && $blk->sprevious_sibling;
    return ($ps && $ps->isa('PPI::Token::Word') && ($KEYWORD{$ps->content} // [''])->[0] eq 'PHASER') ? 1 : 0;
  },
  sole_in_deref_block => sub {    # `@{+}`: a deref block holding ONE operator
    my ($c) = @_; my $t = $c->{t};
    my $stmt = $t->parent or return 0;
    my $blk = $stmt->parent or return 0;
    return 0 unless $blk->isa('PPI::Structure::Block');
    my @sig = grep { $_->significant } $blk->tokens;
    return 0 unless @sig == 3;    # { OP }
    my $ps = $blk->sprevious_sibling;
    return ($ps && $ps->isa('PPI::Token::Cast')) ? 1 : 0;
  },
  adj_nonascii => sub { my ($c) = @_; ($c->{t}->content =~ /[^\x00-\x7f]/ && _adjacent($c->{prev}, $c->{t})) ? 1 : 0 },
  cast_is_star => sub { my ($c) = @_; ($c->{st}{cast} // '') =~ /^\*/ ? 1 : 0 },
  next_is_open_curly => sub { my ($c) = @_; my $n = $c->{next}; ($n && $n->isa('PPI::Token::Structure') && $n->content eq '{') ? 1 : 0 },
  next_is_scalar => sub { my ($c) = @_; my $n = $c->{next}; ($n && ($n->isa('PPI::Token::Symbol') || $n->isa('PPI::Token::Magic')) && $n->content =~ /^\$/) ? 1 : 0 },
  prev_adj_caret => sub { my ($c) = @_; my $p = $c->{prev}; ($p && $p->isa('PPI::Token::Operator') && $p->content eq '^' && _adjacent($p, $c->{t})) ? 1 : 0 },
  prev_nonint_number => sub { my ($c) = @_; my $p = $c->{prev}; ($p && $p->isa('PPI::Token::Number') && ref($p) ne 'PPI::Token::Number') ? 1 : 0 },
  prev_adj_radix_number => sub { my ($c) = @_; my $p = $c->{prev}; ($p && $p->isa('PPI::Token::Number') && $p->content =~ /^-?0/ && _adjacent($p, $c->{t})) ? 1 : 0 },
  prev_adj_number => sub { my ($c) = @_; my $p = $c->{prev}; ($p && $p->isa('PPI::Token::Number') && _adjacent($p, $c->{t})) ? 1 : 0 },
  word_not_infix => sub {
    my ($c) = @_; my $w = _core($c->{t}->content);
    my $k = $KEYWORD{$w} or return 1;
    return ($k->[0] eq 'INFIX' || $k->[0] eq 'MODIFIER' || $k->[0] eq 'FOR') ? 0 : 1;
  },
  not_xstate => sub { $_[0]{st}{x} ne 'XSTATE' },
  not_xattr  => sub { $_[0]{st}{x} !~ /^XATTR/ },
);

# ════════════════════════════════════════════════════════════════════════════
# THE WALK
# ════════════════════════════════════════════════════════════════════════════

# check_source(SRC, %opt) -> (rows) ; a document PPI cannot build gives one
# `lexer-dies` row (kinds §6/§29: there is no token stream to walk).
sub check_source {
  my ($src, %opt) = @_;
  my $doc = PPI::Document->new(\$src);
  if (!$doc) {
    my $err = PPI::Document->errstr // 'unknown';
    return ({ file => $opt{file} // '-', line => 0, col => 0, text => '',
              class => '', state => '', expect => 'a document',
              kind => 'lexer-dies', sec => '',
              cascade => 0, note => $err });
  }
  return check_document($doc, %opt);
}

# check_document(DOC, file => NAME) -> (rows).  Each row is a hash:
#   file line col text class state expect kind sec cascade
sub check_document {
  my ($doc, %opt) = @_;
  $doc->index_locations;
  my @toks = grep { $_->significant } $doc->tokens;
  $opt{stats}{tokens} += @toks if $opt{stats};
  my $w = {
    file   => $opt{file} // '-',
    toks   => \@toks,
    rows   => [],
    decl   => _declared_words($doc),
    features => _features($doc),
    st     => { x => 'XSTATE', req => 0 },
    stack  => [],
    skip   => {},
    flagged_in_stmt => 0,
  };
  for my $i (0 .. $#toks) {
    next if $w->{skip}{$i};
    my $t = $toks[$i];
    last if $t->isa('PPI::Token::Separator');
    my $c = _ctx($w, $i);
    _pending_statement_end($w, $c) if $w->{ended};
    _step($w, $c);
  }
  return @{ $w->{rows} };
}

sub _ctx {
  my ($w, $i) = @_;
  my $t = $w->{toks}[$i];
  my $n = $w->{toks}[$i + 1];
  return { w => $w, i => $i, t => $t, toks => $w->{toks},
           prev => ($i > 0 ? $w->{toks}[$i - 1] : undef), next => $n,
           adj => (_adjacent($t, $n) ? $n : undef), st => $w->{st} };
}

# One token: the pseudo-state handlers, then the contradiction table, then the
# transition table.
sub _step {
  my ($w, $c) = @_;
  my $st = $w->{st};
  _trace($c) if $TRACE;
  my $h = $PSEUDO_STEP{$st->{x}};
  return if $h && $h->($w, $c);
  my $kind = _contradictions($w, $c);
  return $RECOVER{$kind}->($w, $c) if $kind && $RECOVER{$kind};
  _transition($w, $c);
}

# RECOVERY after a contradiction: where perl's reading is known and cheap to
# follow, the walk continues in PERL's state, so the damage one mis-lex does
# is not counted again on the next token.  Every other kind continues with
# PPI's reading (the transition table), and its after-effects are marked
# `cascade`.
%RECOVER = (
  'star-as-multiply'      => sub { _set($_[0], 'XCAST', cast => '*', quiet => 1) },        # perl: a glob sigil
  'ampersand-as-bitand'   => sub { _set($_[0], 'XCAST', cast => '&') },        # perl: a code sigil
  'percent-as-modulo'     => sub { _set($_[0], 'XCAST', cast => '%', quiet => 1) },        # perl: a hash sigil
  'pid-magic-is-cast-run' => sub { _set($_[0], 'XCAST', cast => '$$') },       # perl: two casts
  'x-as-repetition'       => sub { _set($_[0], 'XTERM', lop => $_[1]{t}, lopname => 'x') },  # a call
  'glob-for-multiply'     => sub { _set($_[0], 'XOPERATOR', callable => 1) },  # `*` + a call word
  'magic-hash-for-modulo' => sub { _set($_[0], 'XTERM!') },                    # `%` + a unary sign
  'filetest-split'        => sub { $_[0]{skip}{ $_[1]{i} + 1 } = 1; _set($_[0], 'XTERMORDORDOR') },
);


sub _contradictions {
  my ($w, $c) = @_;
  my $t = $c->{t};
  $c->{st} = $w->{st};             # a pseudo step may have moved the state
  for my $r (@CONTRADICTIONS) {
    my ($class, $content, $in, $pred, $kind, $reads, $sec, $cite) = @$r;
    next unless _class_match($t, $class);
    next unless _content_match($content, $t->content);
    next unless _state_match($in, $w->{st});
    next if $pred ne '' && !$PRED{$pred}->($c);
    _flag($w, $c, $kind, $reads, $sec);
    return $kind;
  }
  return '';
}

sub _transition {
  my ($w, $c) = @_;
  my $t = $c->{t};
  for my $r (@TRANSITIONS) {
    my ($class, $content, $from, $to) = @$r;
    next unless _class_match($t, $class);
    next unless _content_match($content, $t->content);
    next unless _state_match($from, $w->{st});
    return _apply($w, $c, $to);
  }
  # A class the table knows only in term position, met under XOPERATOR (the
  # contradiction row has fired): accept PPI's reading and go on as if the
  # token were in term position -- the cascade, not a new verdict.
  if ($w->{st}{x} eq 'XOPERATOR' || $w->{st}{x} =~ /^X/) {
    for my $r (@TRANSITIONS) {
      my ($class, $content, $from, $to) = @$r;
      next unless _class_match($t, $class);
      next unless _content_match($content, $t->content);
      next unless $from eq 'TERM' || $from eq '*';
      return _apply($w, $c, $to);
    }
  }
  die "Pl::PPIExpect: no transition for PPI class " . ref($t)
    . " [" . $t->content . "] in state $w->{st}{x} at $w->{file} line "
    . $t->line_number . " (rule 12: add a table row)\n";
}

sub _apply {
  my ($w, $c, $to) = @_;
  return if $to eq '=';
  if ($to =~ /^&(\w+)$/) {
    my $h = $HANDLER{$1} or die "Pl::PPIExpect: no handler &$1\n";
    return $h->($w, $c);
  }
  _set($w, $to);
}


# _set(W, 'XTERM!') -- a new state; `!` = term required.  Clears the
# one-token flags (the lop, callable, cast, in-decl, owner unless kept).
sub _set {
  my ($w, $x, %f) = @_;
  my $req = $x =~ s/!$// ? 1 : 0;
  $w->{st} = { x => $x, req => $req, %f };
  $w->{flagged_in_stmt} = 0 if $x eq 'XSTATE';
}

sub _state_match {
  my ($in, $st) = @_;
  return 1 if $in eq '*';
  my $x = $st->{x};
  return $x eq 'XOPERATOR' if $in eq 'OP';
  return $TERMPOS{$x} ? 1 : 0 if $in eq 'TERM';
  return ($TERMPOS{$x} && $st->{req}) ? 1 : 0 if $in eq 'TERM!';
  return ($TERMPOS{$x} && !$st->{req}) ? 1 : 0 if $in eq 'TERM?';
  return ($TERMPOS{$x} && $x ne 'XTERMORDORDOR') ? 1 : 0 if $in eq 'XTERM';
  return $x eq $in;
}

# A table row names a class BY NAME: `Quote` matches PPI::Token::Quote and
# PPI::Token::Quote::*, never a class that merely inherits from it (PPI's
# Prototype ISA Quote::Literal, Magic ISA Symbol, Separator ISA Word).
sub _class_match {
  my ($t, $class) = @_;
  my $r = ref $t;
  return $r eq "PPI::Token::$class" || index($r, "PPI::Token::${class}::") == 0;
}

sub _content_match {
  my ($want, $have) = @_;
  return 1 if !defined $want || (!ref $want && $want eq '');
  return $want->{$have} ? 1 : 0 if ref $want eq 'HASH';
  return $have =~ $want ? 1 : 0 if ref $want eq 'Regexp';
  return $have eq $want;
}

# ONE ROW per contradiction.  `cascade` = an earlier row in the same
# statement: a PPI mis-lex usually derails the tokens after it, and those
# later rows are the damage, not new bugs.
sub _flag {
  my ($w, $c, $kind, $reads, $sec, $note) = @_;
  my $t = $c->{t};
  my $loc = $t->location || [0, 0, 0];
  push @{ $w->{rows} }, {
    file => $w->{file}, line => $loc->[0], col => $loc->[1],
    text => $t->content, class => ref($t) =~ s/^PPI::Token:://r,
    state => $w->{st}{x} . ($w->{st}{req} ? '!' : ''),
    expect => $reads, kind => $kind, sec => $sec // '',
    cascade => ($w->{flagged_in_stmt} || ($w->{last_row_line} // -1) == $loc->[0]
                || _in_cascade_region($w, $t)) ? 1 : 0,
    note => $note // '',
  };
  $w->{flagged_in_stmt} = 1;
  $w->{last_row_line} = $loc->[0];
}

sub _trace {
  my ($c) = @_;
  my $t = $c->{t}; my $st = $c->{st};
  printf STDERR "PPIExpect %4d:%-3d %-22s %-18s %s%s\n", $t->line_number,
    $t->column_number, ref($t) =~ s/^PPI::Token:://r, _trunc($t->content),
    $st->{x}, $st->{req} ? '!' : '';
}

sub _trunc { my ($s) = @_; $s =~ s/\n/\\n/g; length($s) > 18 ? substr($s, 0, 15) . '...' : $s }

# ════════════════════════════════════════════════════════════════════════════
# HANDLERS (the `&name` transitions: toke.c's read-ahead decisions)
# ════════════════════════════════════════════════════════════════════════════

# &symbol -- a variable.  XOPERATOR next, with yyl_dollar's print-filehandle
# rule (toke 5484-5530): a plain `$name` straight after a list operator,
# followed by whitespace and something that can only start a term, is the
# indirect object, so a TERM follows (`print $fh -1`, `print $fh "x"`).
$HANDLER{symbol} = sub {
  my ($w, $c) = @_;
  my $t = $c->{t};
  my $callable = $t->content =~ /^&/ || ($w->{st}{cast} // '') eq '&';
  my $slot = _print_fh_slot($w, $c);
  return _set($w, 'XOPERATOR', callable => 1) if $slot && $slot eq 'CALL';
  if ($slot) {
    return _set($w, 'XTERM', fh_slot => 1);
  }
  _set($w, 'XOPERATOR', callable => $callable ? 1 : 0, subscriptable => 1);
};

sub _print_fh_slot {
  my ($w, $c) = @_;
  my $t = $c->{t};
  my $lop = $w->{st}{lop} or return 0;
  my $p = $c->{prev} or return 0;
  return 0 unless $p == $lop || ($w->{st}{in_paren_of} && $w->{st}{in_paren_of} == $lop
                                && $p->content eq '(');
  return 0 if ($w->{st}{lopname} // '') =~ /^(?:grep|return)$/;
  return 0 unless $t->content =~ /^\$\w/;
  my $n = $c->{next} or return 0;
  return 'CALL' if $w->{st}{indirob} && $n->isa('PPI::Token::Structure') && $n->content eq '(';   # METHCALL 4802
  return 0 if _adjacent($t, $n);                 # isSPACE(tmp), toke 5486
  return 1 if $n->isa('PPI::Token::Symbol') || $n->isa('PPI::Token::Magic')
           || $n->isa('PPI::Token::Quote') || $n->isa('PPI::Token::QuoteLike::Words')
           || $n->isa('PPI::Token::HereDoc') || $n->isa('PPI::Token::QuoteLike::Readline')
           || $n->isa('PPI::Token::Cast');       # $ @ " ' ` q & * < %  (5489-5495)
  if ($n->isa('PPI::Token::Word')) {             # 5496-5517
    my $k = $KEYWORD{_core($n->content)};
    return 0 if $k && $k->[0] eq 'INFIX';
    return 0 if $k && $k->[0] eq 'MODIFIER';     # (an OPERATOR(KW_IF), never a handle arg)
    return 1;
  }
  return 1 if $n->isa('PPI::Token::Number') && $n->content =~ /^\d/;   # 5521
  return 1 if $n->isa('PPI::Token::Number') && $n->content =~ /^[.\-+]\d/;   # 5523-5526
  if ($n->isa('PPI::Token::Operator')) {         # ? - + / followed by non-space
    my $nn = $c->{toks}[$c->{i} + 2];
    return 1 if $n->content =~ m{^[?\-+/]$} && _adjacent($n, $nn);
  }
  return 1 if $n->isa('PPI::Token::Regexp::Match');
  return 0;
}

$HANDLER{cast}  = sub { _set($_[0], 'XCAST', cast => $_[1]{t}->content) };
$HANDLER{arrow} = sub { _set($_[0], 'XMETHOD') };

# &open_paren -- yyl_leftparen 6813: straight after a list operator / named
# unary the state is KEPT (`print(STDOUT 123)`), else XTERM.  Under
# XOPERATOR a paren is a CALL only after something callable.
$HANDLER{open_paren} = sub {
  my ($w, $c) = @_;
  my $st = $w->{st};
  my $inner = 'XTERM';
  my $kind = 'list';
  if ($st->{x} eq 'XMETHOD') { $kind = 'call' }                       # `->(`: yyl_hyphen 5972
  elsif ($st->{x} eq 'XOPERATOR') {
    if ($st->{callable}) { $kind = 'call' }
    else { _flag($w, $c, 'paren-after-term', 'an operator', '') }
  }
  elsif ($st->{lop} && $c->{prev} && $c->{prev} == $st->{lop}) {
    $inner = $st->{x} eq 'XREF' ? 'XREF' : 'XTERM';
  }
  push @{ $w->{stack} }, { open => '(', kind => $kind, tok => $c->{t},
                           owner => $st->{owner} };
  _set($w, $inner, lop => ($inner eq 'XREF' ? $st->{lop} : undef),
       lopname => $st->{lopname}, in_paren_of => $st->{lop});
};

# &close_paren -- yyl_rightparen 6825: PREBLOCK (XBLOCK) when `{` follows,
# else TERM (XOPERATOR).
$HANDLER{close_paren} = sub {
  my ($w, $c) = @_;
  my $f = _pop($w, $c, '(') or return _set($w, 'XOPERATOR');
  return _set($w, 'XFORLIST', owner => $f->{owner}) if $f->{kind} eq 'forvars';
  my $n = $c->{next};
  if ($n && $n->isa('PPI::Token::Structure') && $n->content eq '{') {
    return _set($w, 'XBLOCK', owner => $f->{owner}, stmt_block => ($f->{owner} ? 1 : 0));
  }
  _set($w, 'XOPERATOR', callable => ($f->{kind} eq 'call' ? 1 : 0),
       subscriptable => 1, decl => $f->{decl});
};

# &open_square -- yyl_leftsquare 6758.  Under XOPERATOR it subscripts; in a
# term position it is an anonymous array.  PPI's own structure class is
# checked against that decision.
$HANDLER{open_square} = sub {
  my ($w, $c) = @_;
  my $x = $w->{st}{x};
  my $kind = ($x eq 'XOPERATOR' || $x eq 'XMETHOD' || $x eq 'XPOSTDEREF') ? 'subscript' : 'anonarray';
  _check_structure($w, $c, $kind);
  push @{ $w->{stack} }, { open => '[', kind => $kind, tok => $c->{t} };
  _set($w, 'XTERM');
};

$HANDLER{close_square} = sub {
  my ($w, $c) = @_;
  _pop($w, $c, '[');
  _set($w, 'XOPERATOR', callable => 1, subscriptable => 1);
};

# &open_curly -- yyl_leftcurly 6310, by state:
#   XTERM / XTERMORDORDOR  anon hash (6318-6322)            after: XOPERATOR
#   XOPERATOR              subscript (6323-6345)            after: XOPERATOR
#   XATTRTERM XTERMBLOCK   block (6346-6351)                after: XOPERATOR
#   XATTRBLOCK XBLOCK      block (6352-6357)                after: XSTATE
#   XBLOCKTERM             block (6358-6363)                after: XTERM
#   XCAST (XREF via PREREF) deref block (6380-6385)         after: XOPERATOR
#   XSTATE / XREF          the lookahead (6364-6505): `}` = hash; a string or
#                          a word/number followed by `,` (not a lower-case
#                          word) or `=>` = hash; else block.  After: XTERM when
#                          the `{` directly follows a list operator, else
#                          XOPERATOR; a statement block resets it to XSTATE.
$HANDLER{open_curly} = sub {
  my ($w, $c) = @_;
  my $st = $w->{st};
  my $x = $st->{x};
  my ($kind, $after, $inner) = ('block', 'XSTATE', 'XSTATE');
  my $lop_adj = $st->{lop} && $c->{prev} && ($c->{prev} == $st->{lop}
                || ($st->{in_paren_of} && $c->{prev}->content eq '('));
  if ($x eq 'XTERM' || $x eq 'XTERMORDORDOR') { ($kind, $after, $inner) = ('hash', 'XOPERATOR', 'XTERM') }
  elsif ($x eq 'XOPERATOR' || $x eq 'XMETHOD' || $x eq 'XPOSTDEREF') { ($kind, $after, $inner) = ('subscript', 'XOPERATOR', 'XTERM') }
  elsif ($x eq 'XATTRTERM' || $x eq 'XTERMBLOCK') { ($kind, $after) = ('block', 'XOPERATOR') }
  elsif ($x eq 'XATTRBLOCK' || $x eq 'XBLOCK') { ($kind, $after) = ('block', 'XSTATE') }
  elsif ($x eq 'XBLOCKTERM') { ($kind, $after) = ('block', 'XTERM') }
  elsif ($x eq 'XCAST') { ($kind, $after) = ('deref', 'XOPERATOR') }
  else {   # XSTATE, XREF: the lookahead
    my $is_hash = _curly_lookahead_is_hash($c);
    $after = $lop_adj ? 'XTERM' : 'XOPERATOR';
    if ($is_hash) { ($kind, $inner) = ('hash', 'XTERM') }
    else {
      $kind = 'block';
      $after = 'XSTATE' if $x eq 'XSTATE';
    }
  }
  my $owner = $st->{owner};
  my $stmt_block = $st->{stmt_block} || ($x eq 'XSTATE' && $kind eq 'block') ? 1 : 0;
  $stmt_block = 0 if $after ne 'XSTATE';
  my $fhblock = ($kind eq 'block' && $x eq 'XREF' && $FH_LOP{$st->{lopname} // ''}) ? 1 : 0;
  _check_structure($w, $c, $kind, $fhblock);
  _check_block_owner($w, $c, $owner) if $kind eq 'block' && $owner;
  push @{ $w->{stack} }, { open => '{', kind => $kind, tok => $c->{t}, after => $after,
                           owner => $owner, stmt_block => $stmt_block,
                           name_magic => $st->{name_magic},
                           after_lop => ($after eq 'XTERM' ? $st->{lop} : undef),
                           lopname => $st->{lopname} };
  _set($w, $inner, subscript_open => ($kind eq 'subscript' ? 1 : 0));
};

sub _curly_lookahead_is_hash {
  my ($c) = @_;
  my $t1 = $c->{toks}[$c->{i} + 1] or return 0;
  return 1 if $t1->isa('PPI::Token::Structure') && $t1->content eq '}';   # 6366-6376
  my $t2 = $c->{toks}[$c->{i} + 2] or return 0;
  my $first;
  if ($t1->isa('PPI::Token::Quote') || $t1->isa('PPI::Token::QuoteLike::Backtick')) {
    $first = substr($t1->content, 0, 1);                                   # 6407-6413, 6414-6466
  }
  elsif (($t1->isa('PPI::Token::Word') || $t1->isa('PPI::Token::Number')) && $t1->content =~ /^\w/) {
    $first = substr($t1->content, 0, 1);                                   # 6467-6474
  }
  else { return 0 }
  return 0 unless $t2->isa('PPI::Token::Operator');
  return 1 if $t2->content eq '=>';                                        # 6478
  return 1 if $t2->content eq ',' && ($first eq 'q' || $first !~ /[a-z]/); # 6477
  return 0;
}

# &close_curly -- yyl_rightcurly 6524: PL_expect = the brackstack entry.
$HANDLER{close_curly} = sub {
  my ($w, $c) = @_;
  my $f = _pop($w, $c, '{') or return _set($w, 'XOPERATOR');
  if ($f->{stmt_block}) {
    $w->{ended} = { open => $f->{tok}, owner => $f->{owner}, owner_name_magic => $f->{name_magic} };
  }
  my %keep = $f->{after_lop} ? (lop => $f->{after_lop}, lopname => $f->{lopname}, after_block => 1) : ();
  _set($w, $f->{after}, callable => ($f->{kind} =~ /^(?:subscript|deref)$/ ? 1 : 0),
       subscriptable => ($f->{kind} =~ /^(?:subscript|deref|hash)$/ ? 1 : 0),
       deref_block => ($f->{kind} eq 'deref' ? $f->{tok} : undef), %keep);
};

$HANDLER{end} = sub { };

# Pop the innermost frame, which must have been opened by OPEN.  A mismatch
# is the `unmatched-closer` contradiction (PPI's structure and the source
# disagree -- §32's stray `)` is the catalogued case).
sub _pop {
  my ($w, $c, $open) = @_;
  my $s = $w->{stack};
  if (@$s && $s->[-1]{open} eq $open) { return pop @$s }
  my $sec = '';
  my $p = $c->{prev};
  $sec = 32 if $p && $p->isa('PPI::Token::Symbol') && $p->content eq '$';
  _flag($w, $c, 'unmatched-closer', 'a matching ' . $open, $sec);
  for my $k (reverse 0 .. $#$s) {
    if ($s->[$k]{open} eq $open) { my $f = $s->[$k]; splice @$s, $k; return $f }
  }
  return undef;
}

# THE STRUCTURE CHECK: the class PPI gave a bracket's Structure against the
# class toke.c's state decides.
my %STRUCT_OF = (hash => 'Constructor', anonarray => 'Constructor',
                 subscript => 'Subscript', block => 'Block', deref => 'Block');

sub _check_structure {
  my ($w, $c, $kind, $fhblock) = @_;
  my $t = $c->{t};
  my $s = $t->parent or return;
  return unless $s->isa('PPI::Structure');
  my $want = $STRUCT_OF{$kind};
  (my $got = ref $s) =~ s/^PPI::Structure:://;
  if ($fhblock && $got eq 'Block' && _fh_block_split($s)) {
    return _flag($w, $c, 'filehandle-block-split', 'Structure::Block in the print statement', 37,
                 'PPI: the block filehandle is a statement of its own');
  }
  # PPI's own names for the brackets of a given/when are the statement's.
  return if $want eq 'Block' && $got =~ /^(?:Given|When)$/;
  return if $got eq $want;
  # A LIST SLICE -- `(LIST)[i]`, `qw(a b)[i]`: yyl_rightparen 6835 TERM, so
  # the `[` subscripts the list; PPI builds an anonymous-array Constructor.
  my $p = $c->{prev};
  if ($kind eq 'subscript' && $got eq 'Constructor' && $p
      && (($p->isa('PPI::Token::Structure') && $p->content eq ')')
          || $p->isa('PPI::Token::QuoteLike::Words'))) {
    return _flag($w, $c, 'list-slice-as-constructor', 'Structure::Subscript (a list slice)', '',
                 "PPI: Structure::$got");
  }
  my $sec = _structure_section($w, $c, $kind, $got, $fhblock);
  _flag($w, $c, 'brace-class', "Structure::$want ($kind)", $sec, "PPI: Structure::$got");
}

sub _fh_block_split {   # §37: print({$fh} LIST) -- the block is its own statement
  my ($s) = @_;
  my $p = $s->parent;
  return ($p && $p->isa('PPI::Statement::Compound')) ? 1 : 0;
}

sub _structure_section {
  my ($w, $c, $kind, $got, $fhblock) = @_;
  return 37 if $fhblock;
  return 5 if $kind eq 'hash' && $got eq 'Block' && $w->{st}{x} eq 'XSTATE';
  my $p = $c->{prev};
  return 1 if $kind eq 'deref' && $p && $p->isa('PPI::Token::Magic') && $p->content eq '$$';   # §1b
  if ($kind eq 'subscript' && $p) {
    if ($p->isa('PPI::Token::Structure') && $p->content eq '}') {
      my $blk = $p->parent;
      my @in = $blk ? grep { $_->significant } $blk->tokens : ();
      return 36 if @in == 3 && $in[1]->isa('PPI::Token::Word');
      return 17;
    }
    return 17 if $p->isa('PPI::Token::Symbol') && $p->content =~ /^%/;
    return 23 if $p->isa('PPI::Token::Word') && $p->content =~ /[^\x00-\x7f]/;
  }
  return '';
}

# S2: a control statement's block must belong to the statement that owns it
# (§10, §29a: PPI leaves the for-head's list and block in a sibling).
sub _check_block_owner {
  my ($w, $c, $owner) = @_;
  my $s = $c->{t}->parent or return;
  my $bs = $s->parent;
  my $os = _stmt_of($owner);
  return if !$bs || !$os || $bs == $os;
  my $kw = $owner->content;
  # `for my \$x` is §29a (declared_refs); `for \my %e` / `for my ($a,$b)` §10
  my $sec = ($kw =~ /^for(?:each)?$/)
          ? ((($w->{forseq}{refaddr $owner} // '') =~ /^my \\/) ? 29 : 10) : '';
  _flag($w, $c, 'block-owner-split', "the block of `$kw`", $sec,
        'PPI: the block is not in the statement of its keyword');
  $w->{split_sec}{refaddr $c->{t}} = $sec;
}

# S3: after a statement-level block closes, the statement has ENDED in perl
# (no `;` needed) unless a continuation word of its owner follows.
sub _pending_statement_end {
  my ($w, $c) = @_;
  my $e = delete $w->{ended};
  my $t = $c->{t};
  my $bs = $e->{open}->parent && $e->{open}->parent->parent;
  my $ts = _stmt_of($t);
  return if !$bs || !$ts;
  my $kw = $e->{owner} ? $e->{owner}->content : '';
  my $is_cont = grep { $t->isa('PPI::Token::Word') && $t->content eq $_ } @{ $CONTINUES{$kw} // [] };
  if ($is_cont) {
    return if $ts == $bs;
    return _flag($w, $c, 'continuation-split', "`" . $t->content . "` continuing `$kw`",
                 ($t->content eq 'finally' ? 18 : ''), 'PPI: the continuation starts a new statement');
  }
  return if $ts != $bs;
  return if $t->isa('PPI::Token::Structure') && $t->content =~ /^[;})]$/;
  my $sec = $kw eq 'finally' ? 18 : $kw =~ /^(?:try|catch)$/ ? 20
          : ($kw eq 'sub' && $e->{owner_name_magic}) ? 31 : '';
  # the after-effect of a block PPI already split off its keyword: a cascade
  if (exists $w->{split_sec}{refaddr $e->{open}}) {
    $sec = $w->{split_sec}{refaddr $e->{open}};
    $w->{flagged_in_stmt} = 1;
  }
  $w->{flagged_in_stmt} = 1 if $kw eq 'sub' && $e->{owner_name_magic};
  _flag($w, $c, 'statement-not-ended', "a new statement after `$kw` BLOCK", $sec,
        'PPI: the next statement is inside the previous one');
}

# The outermost statement holding T (a child of the document).
sub _top_stmt {
  my ($t) = @_;
  my $p = $t->parent; my $top;
  while ($p && !$p->isa('PPI::Document')) { $top = $p if $p->isa('PPI::Statement'); $p = $p->parent }
  return $top;
}

# Is T inside a PPI statement already known to be wrecked (a format PPI
# lexed as code swallows what follows it): its rows are the damage.
sub _in_cascade_region {
  my ($w, $t) = @_;
  my $r = $w->{cascade_region} or return 0;
  my $top = _top_stmt($t) or return 0;
  return $r->{refaddr $top} ? 1 : 0;
}

sub _stmt_of {
  my ($t) = @_;
  my $p = $t->parent;
  $p = $p->parent while $p && !$p->isa('PPI::Statement');
  return $p;
}

# ════════════════════════════════════════════════════════════════════════════
# &word -- yyl_keylookup 8984 / yyl_word_or_keyword 7944 / yyl_just_a_word 7694
# ════════════════════════════════════════════════════════════════════════════
$HANDLER{word} = sub {
  my ($w, $c) = @_;
  my $st = $w->{st};
  my $t = $c->{t};
  my $word = $t->content;
  my $core = _core($word);
  my $n = $c->{next};
  my $nc = $n ? $n->content : '';
  my $k = $KEYWORD{$core};
  # yyl_keylookup 9013: ANY word before `=>` is a string.
  return _set($w, 'XOPERATOR') if $n && $n->isa('PPI::Token::Operator') && $nc eq '=>';
  # The subscript lookahead of yyl_leftcurly 6324-6343: `{word}` / `{-word}`.
  return _set($w, 'XOPERATOR') if _in_subscript_forced_word($c);
  if ($st->{x} eq 'XOPERATOR') {
    return _set($w, 'XTERM!') if $k && $k->[0] eq 'INFIX';                 # KEY_x 8890 Mop; ChEop ...
    return _set($w, 'XTERM!', modifier => $t) if $k && ($k->[0] eq 'MODIFIER' || $k->[0] eq 'FOR');
    # word-after-term (the row has fired): recover as a term-position word
  }
  return _word_in_term_position($w, $c, $word, $core, $k);
};

sub _word_in_term_position {
  my ($w, $c, $word, $core, $k) = @_;
  my $st = $w->{st};
  my $t = $c->{t};
  my $n = $c->{next};
  my $nc = $n ? $n->content : '';
  my $next_paren = $n && $n->isa('PPI::Token::Structure') && $nc eq '(';
  my $next_curly = $n && $n->isa('PPI::Token::Structure') && $nc eq '{';
  # A filehandle / sort SUBNAME in the indirect-object slot (just_a_word
  # 7789-7820: PL_expect = XTERM when it follows the list operator).
  if ($st->{x} eq 'XREF' && !$k && !$next_paren && $word !~ /::$/
      && !($n && $n->isa('PPI::Token::Operator') && $nc =~ /^(?:,|->|=>)$/)) {
    my $ln = $st->{lopname} // '';
    return _set($w, 'XTERM') if $FH_LOP{$ln} || $ln eq 'sort';
  }
  $k = undef if $k && $FEATURE_KW{$core} && !$w->{features}{ $FEATURE_KW{$core} };   # keyword() 0 when the feature is off
  if ($k) {
    my $cls = $k->[0];
    my $h = $KEYWORD_STEP{$cls} or die "Pl::PPIExpect: no keyword handler for class $cls\n";
    my $r = $h->($w, $c, $core, $next_paren, $next_curly);
    return $r unless defined $r && $r eq 'USERWORD';
  }
  # ── a user word (yyl_just_a_word) ──
  return _set($w, 'XOPERATOR') if $word =~ /::$/;                          # 7741 `Foo::`
  return _set($w, 'XOPERATOR') if $n && $n->isa('PPI::Token::Operator') && $nc eq '->';   # class name
  my $d = _decl_before($w, $t, $core);
  if ($next_paren) {                                                        # 7824 `(` = a sub call
    return _set($w, 'XTERM', lop => $t, lopname => $core);
  }
  if (defined $d) {
    return _set($w, 'XOPERATOR') if $d eq 'CONST' || $d eq '';             # FUNC0SUB 6262, constant 7628
    if ($d =~ /^;*(?:[\$_*+]|\\.|\\\[[^\]]*\])$/) {                         # UNIPROTO 6271-6291
      return _set($w, 'XTERM', lop => $t, lopname => $core);
    }
    if ($d =~ /^&/ && $next_curly) {                                        # PREBLOCK(LSTOPSUB) 6293-6301
      return _set($w, 'XBLOCK', lop => $t, lopname => $core);
    }
    return _set($w, 'XTERM', lop => $t, lopname => $core);                 # NOAMP, constant_op 7647
  }
  # declared nowhere in this document
  if ($next_curly || ($n && $n->content =~ /^\$/ && ($n->isa('PPI::Token::Symbol') || $n->isa('PPI::Token::Magic') || $n->isa('PPI::Token::Cast')))) {
    return _set($w, 'XBLOCKTERM', lop => $t, lopname => $core,             # 7858-7866 METHCALL0
                indirob => ($core =~ /^[A-Z]/ ? 0 : 1));                    # intuit_method 4793 isUPPER
  }
  if ($st->{lop} && $c->{prev} && $c->{prev} == $st->{lop} && !$KEYWORD{_core($st->{lop}->content)}
      && $n && _starts_term($n)) {
    return _set($w, 'XTERM');                                             # intuit_method 4832 bare_package
  }
  return _set($w, 'XOPERATOR') if $core =~ /^[A-Z_][A-Z0-9_]*$/           # 7836 a bareword ...
    && !($n && !_adjacent($t, $n) && _starts_term($n));                  # ... unless a term follows (an imported sub: BAIL_OUT 'x')
  return _set($w, 'XTERM', lop => $t, lopname => $core);                   # POLICY (header)
}

%KEYWORD_STEP = (
  LOP      => sub { my ($w, $c, $k) = @_; _set($w, 'XTERM', lop => $c->{t}, lopname => $k) },        # S_lop 2116-2138
  LOPREF   => sub { my ($w, $c, $k) = @_; _set($w, 'XREF', lop => $c->{t}, lopname => $k) },
  OLDLOP   => sub { my ($w, $c, $k) = @_; _set($w, 'XTERM', lopname => $k) },                       # OLDLOP 8569
  UNI      => sub { my ($w, $c, $k) = @_; _set($w, 'XTERM', lop => $c->{t}, lopname => $k) },        # UNI3 268-279
  UNIDOR   => sub { my ($w, $c, $k) = @_; _set($w, 'XTERMORDORDOR', lop => $c->{t}, lopname => $k) },# UNIDOR 280
  FUN0     => sub { _set($_[0], 'XOPERATOR', callable => 1) },                                     # FUN0 245; perly.y 1481 FUNC0 '(' ')'
  FUN1     => sub { _set($_[0], 'XTERM') },                                                         # FUN1 247 (not)
  INFIX    => sub {   # a word operator in term position
    my ($w, $c, $k) = @_;
    return 'USERWORD' if $k eq 'x';                                       # KEY_x 8898: a call
    return _set($w, 'XTERM!') unless $w->{st}{req};                        # the term was omitted
    _flag($w, $c, 'infix-for-term', 'a term', '');
    _set($w, 'XTERM!');
  },
  MODIFIER => sub {   # if/unless/while/until/when
    my ($w, $c) = @_;
    return _set($w, 'XTERM', owner => $c->{t}) if $w->{st}{x} eq 'XSTATE'; # the compound statement
    _set($w, 'XTERM!', modifier => $c->{t});                               # a modifier after an omitted term
  },
  FOR      => sub {
    my ($w, $c) = @_;
    return _set($w, 'XFORVAR', owner => $c->{t}) if $w->{st}{x} eq 'XSTATE';   # yyl_foreach 7128
    _set($w, 'XTERM!', modifier => $c->{t});
  },
  OPERATOR => sub {   # local, elsif, given, field: OPERATOR(KW_x)
    my ($w, $c, $k) = @_;
    my %o = ($k eq 'elsif' || $k eq 'given') ? (owner => $c->{t}) : ();
    _set($w, ($k eq 'local' ? 'XTERM!' : 'XTERM'), %o);
  },
  PREBLOCK => sub {   # else continue default defer finally try catch
    my ($w, $c, $k) = @_;
    _set($w, 'XBLOCK', owner => $c->{t}, stmt_block => 1);
  },
  PHASER   => sub {   # BEGIN END ... AUTOLOAD DESTROY: yyl_sub with a name
    my ($w, $c, $k, $np, $nb) = @_;
    return 'USERWORD' unless $nb;
    _set($w, 'XBLOCK', owner => $c->{t}, stmt_block => 1);
  },
  LOOPX    => sub { _set($_[0], 'XLABEL') },                                                        # LOOPX 240
  MY       => sub { _set($_[0], 'XDECL') },                                                         # yyl_my 7234
  SUB      => sub { _set($_[0], 'XSUBNAME', subkw => $_[1]{t}) },                                   # yyl_sub 5535
  DO       => sub { my ($w, $c, $k, $np, $nb) = @_; _set($w, $nb ? 'XTERMBLOCK' : 'XTERM') },       # yyl_do 7206-7230
  EVAL     => sub { my ($w, $c, $k, $np, $nb) = @_; _set($w, $nb ? 'XTERMBLOCK' : 'XTERM', lop => $c->{t}, lopname => 'eval') },  # 8156-8165
  REQUIRE  => sub {   # yyl_require 7092: a bareword is the module
    my ($w, $c) = @_; my $n = $c->{next};
    return _set($w, 'XLABEL') if $n && $n->isa('PPI::Token::Word');
    _set($w, 'XTERM');
  },
  USE      => sub { _set($_[0], 'XMODULE') },                                                       # tokenize_use 8846
  PACKAGE  => sub { _set($_[0], 'XPKG', owner => $_[1]{t}) },                                       # 8525 / 8038
  FORMAT   => sub { _skip_format(@_) },                                                             # yyl_sub 8751 + scan_formline
  QUOTE    => sub {   # q qq qw m s tr y qr qx as a WORD: PPI did not see the quote
    my ($w, $c) = @_;
    _flag($w, $c, 'quote-letter-as-word', 'a quote-like operator', '');
    _set($w, 'XOPERATOR');
  },
  DATA     => sub { _set($_[0], 'XSTATE') },
);

# FORMAT: perl reads the body as PICTURE TEXT (scan_formline), never as code;
# PPI tokenizes it as code.  The body is skipped (to the line holding only
# `.`) and the fact is ONE row -- an uncatalogued PPI behaviour.
sub _skip_format {
  my ($w, $c) = @_;
  my $toks = $c->{toks};
  my $line = $c->{t}->line_number;
  my $end;
  for my $j ($c->{i} + 1 .. $#$toks) {
    my $u = $toks->[$j];
    next if $u->line_number <= $line;
    if ($u->content eq '.' && !grep { $_ != $j && $toks->[$_]->line_number == $u->line_number }
          grep { $_ >= 0 && $_ <= $#$toks } $j - 1, $j + 1) {
      $end = $j; last;
    }
  }
  return _set($w, 'XSTATE') unless defined $end;
  $w->{skip}{$_} = 1 for $c->{i} + 1 .. $end;
  _flag($w, $c, 'format-body-lexed-as-code', 'format picture text (scan_formline)', '',
        'PPI tokenizes the format body as code');
  my $top = _top_stmt($c->{t});
  $w->{cascade_region}{refaddr $top} = 1 if $top;
  _set($w, 'XSTATE');
}

# ════════════════════════════════════════════════════════════════════════════
# THE PSEUDO-STATE STEPS (return 1 = token consumed)
# ════════════════════════════════════════════════════════════════════════════
%PSEUDO_STEP = (
  XSUBNAME => sub {   # yyl_sub 5535-5662
    my ($w, $c) = @_;
    my $t = $c->{t};
    if ($t->isa('PPI::Token::Word')) {
      _set($w, 'XATTRBLOCK', owner => $w->{st}{subkw}, stmt_block => 1);
      return 1;
    }
    if ($t->isa('PPI::Token::Magic') && $t->content eq '_') {
      _flag($w, $c, 'sub-name-as-magic', 'Word(_) (the sub name)', 31);
      _set($w, 'XATTRBLOCK', owner => $w->{st}{subkw}, stmt_block => 1, name_magic => 1);
      return 1;
    }
    if ($t->isa('PPI::Token::Operator') && $t->content =~ /^x=?$/) {   # `sub x {` / `method x {`
      _flag($w, $c, 'x-as-repetition', 'Word(x) (the sub name)', 19);
      _set($w, 'XATTRBLOCK', owner => $w->{st}{subkw}, stmt_block => 1);
      return 1;
    }
    _set($w, 'XATTRTERM');      # anon sub: reprocess this token there
    return 0;
  },
  XATTRBLOCK => sub {
    my ($w, $c) = @_; my $t = $c->{t};
    if ($t->isa('PPI::Token::Structure') && $t->content eq ';') { _set($w, 'XSTATE'); return 1 }  # forward declaration
    return 0;
  },
  XDECL => sub {      # yyl_my 7234
    my ($w, $c) = @_; my $t = $c->{t};
    if ($t->isa('PPI::Token::Word')) {
      return 0 if $t->content =~ /^(?:sub|method)$/;   # `my sub` -> the SUB row
      return 1;                                        # `my Dog $spot`: the type
    }
    if ($t->isa('PPI::Token::Symbol') || $t->isa('PPI::Token::Magic')) { _set($w, 'XOPERATOR', decl => 1); return 1 }
    if ($t->isa('PPI::Token::Cast') && $t->content eq '\\') { return 1 }   # declared_refs `my \$x`
    if ($t->isa('PPI::Token::Structure') && $t->content eq '(') {
      push @{ $w->{stack} }, { open => '(', kind => 'list', tok => $t, decl => 1 };
      _set($w, 'XTERM');
      return 1;
    }
    _set($w, 'XTERM');
    return 0;
  },
  XATTRDECL => sub {  # `my $x : shared = 1` -- the attribute words and arguments
    my ($w, $c) = @_; my $t = $c->{t};
    return 1 if $t->isa('PPI::Token::Word');
    if ($t->isa('PPI::Token::Structure') && $t->content eq '(') {
      push @{ $w->{stack} }, { open => '(', kind => 'attrargs', tok => $t };
      _set($w, 'XTERM'); return 1;
    }
    _set($w, 'XOPERATOR');
    return 0;
  },
  XMODULE => sub {    # use/no MODULE VERSION LIST
    my ($w, $c) = @_; my $t = $c->{t};
    if ($t->isa('PPI::Token::Word') || $t->isa('PPI::Token::Number')) { _set($w, 'XTERM', use_args => 1); return 1 }
    _set($w, 'XTERM');
    return 0;
  },
  XLABEL => sub {     # LOOPX: a non-keyword word is the label; require MODULE
    my ($w, $c) = @_; my $t = $c->{t};
    if ($t->isa('PPI::Token::Word') && !$KEYWORD{_core($t->content)}) { _set($w, 'XOPERATOR'); return 1 }
    _set($w, 'XTERM');
    return 0;
  },
  XFORVAR => sub {    # yyl_foreach 7128
    my ($w, $c) = @_; my $t = $c->{t}; my $own = $w->{st}{owner};
    my $seq = \$w->{forseq}{refaddr $own};
    if ($t->isa('PPI::Token::Word') && _core($t->content) =~ /^(?:my|our|state)$/) { $$seq .= 'my '; return 1 }
    if ($t->isa('PPI::Token::Cast') && $t->content eq '\\') { $$seq .= '\\ '; return 1 }
    if ($t->isa('PPI::Token::Symbol') || $t->isa('PPI::Token::Magic')) { _set($w, 'XFORLIST', owner => $own); return 1 }
    if ($t->isa('PPI::Token::Structure') && $t->content eq '(') {
      # after `my`: the 5.36 n-at-a-time variable list; else the loop head
      my $kind = ($$seq // '') =~ /my $/ ? 'forvars' : 'forhead';
      push @{ $w->{stack} }, { open => '(', kind => $kind, tok => $t, owner => $own };
      _set($w, $kind eq 'forvars' ? 'XTERM' : 'XSTATE'); return 1;
    }
    _flag($w, $c, 'for-head', 'the loop variable or `(`', '');
    _set($w, 'XTERM');
    return 0;
  },
  XFORLIST => sub {
    my ($w, $c) = @_; my $t = $c->{t}; my $own = $w->{st}{owner};
    if ($t->isa('PPI::Token::Structure') && $t->content eq '(') {
      push @{ $w->{stack} }, { open => '(', kind => 'forhead', tok => $t, owner => $own };
      _set($w, 'XTERM'); return 1;
    }
    _flag($w, $c, 'for-head', 'the `(` of the foreach list', '');
    _set($w, 'XOPERATOR');
    return 0;
  },
  XPKG => sub {       # package NAME [VERSION] ; | BLOCK   (class NAME [VERSION] [:ATTRS] ...)
    my ($w, $c) = @_; my $t = $c->{t}; my $own = $w->{st}{owner};
    if ($t->isa('PPI::Token::Word') || $t->isa('PPI::Token::Number') || $t->isa('PPI::Token::Attribute')
        || ($t->isa('PPI::Token::Operator') && $t->content eq ':')) { return 1 }
    if ($t->isa('PPI::Token::Structure') && $t->content eq '{') { _set($w, 'XBLOCK', owner => $own, stmt_block => 1); return 0 }
    _set($w, 'XSTATE') if $t->isa('PPI::Token::Structure') && $t->content eq ';';
    return 0 if $t->isa('PPI::Token::Structure') && $t->content eq ';';
    _set($w, 'XOPERATOR');
    return 0;
  },
  XMETHOD => sub {    # after ->
    my ($w, $c) = @_; my $t = $c->{t};
    if ($t->isa('PPI::Token::Word')) { _set($w, 'XOPERATOR', callable => 1); return 1 }       # force_word METHCALL0 5968
    if (($t->isa('PPI::Token::Symbol') || $t->isa('PPI::Token::Magic')) && $t->content =~ /^\$/) {
      _set($w, 'XOPERATOR', callable => 1); return 1;                                          # OPERATOR(ARROW) 5972: `$obj->$m`
    }
    return 0 if $t->isa('PPI::Token::Structure') && $t->content =~ /^[\[{(]$/;               # subscripts / call
    return 0 if $t->isa('PPI::Token::Cast');
    _flag($w, $c, 'arrow-then', 'a method name, a subscript or a postfix deref', '');
    _set($w, 'XOPERATOR');
    return 0;
  },
  XPOSTDEREF => sub {
    my ($w, $c) = @_; my $t = $c->{t};
    return 0 if $t->isa('PPI::Token::Structure') && $t->content =~ /^[\[{]$/;
    _set($w, 'XOPERATOR');
    return 0;
  },
  XCAST => sub {      # after a bare sigil: another sigil, a name, or a `{`
    my ($w, $c) = @_; my $t = $c->{t};
    # After a recovered sigil (`*-` read as Operator(*) Operator(-)): the
    # adjacent punctuation / digits / caret letter ARE the name, as perl's
    # scan_ident reads them -- part of the row already written, not new ones.
    if ($w->{st}{quiet} && _adjacent($c->{prev}, $t)
        && ($t->isa('PPI::Token::Operator') || $t->isa('PPI::Token::Number')
            || $t->isa('PPI::Token::Structure') && $t->content =~ /^[\])}]$/ && $t->parent
               && $t->parent->isa('PPI::Statement::UnmatchedBrace'))) {
      if ($t->content eq '^' && $c->{adj} && $c->{adj}->isa('PPI::Token::Word')) {
        $w->{skip}{ $c->{i} + 1 } = 1;          # `*^R`: the caret letter
      }
      _set($w, 'XOPERATOR', subscriptable => 1);
      return 1;
    }
    if ($t->isa('PPI::Token::Cast')) { $w->{st}{cast} = ($w->{st}{cast} // '') . $t->content; return 1 }
    if (($t->isa('PPI::Token::Symbol') || $t->isa('PPI::Token::Magic')) && $t->content ne '$') {
      my $cast = $w->{st}{cast} // '';
      _set($w, 'XOPERATOR', callable => ($cast =~ /&/ ? 1 : 0), subscriptable => 1); return 1;
    }
    return 0 if $t->isa('PPI::Token::Structure') && $t->content eq '{';
    _contradictions($w, $c);           # the XCAST rows
    _set($w, 'XOPERATOR', callable => 1, subscriptable => 1);  # recover: the sigil + what PPI split off is one variable
    return 1 unless $t->isa('PPI::Token::Structure');
    _transition($w, $c);
    return 1;
  },
);

# The decl-attribute check: the token straight after a declared variable.
$PSEUDO_STEP{XOPERATOR} = sub {
  my ($w, $c) = @_; my $t = $c->{t};
  if ($w->{st}{decl} && $t->isa('PPI::Token::Operator') && $t->content eq ':') {
    my $n = $c->{next};
    if ($n && $n->isa('PPI::Token::Word')) {
      _flag($w, $c, 'decl-attribute-as-operator', 'Attribute (yyl_colon 6124 with PL_in_my)', 8);
      _set($w, 'XATTRDECL');
      return 1;
    }
  }
  return 0;
};
$PSEUDO_STEP{XTERM} = sub {   # use MODULE VERSION LIST: the version needs no comma
  my ($w, $c) = @_; my $t = $c->{t};
  if ($w->{st}{use_args} && $t->isa('PPI::Token::Number')) { _set($w, 'XTERM'); return 1 }
  return 0;
};

# ════════════════════════════════════════════════════════════════════════════
# DOCUMENT FACTS
# ════════════════════════════════════════════════════════════════════════════

# The words this document declares: name => prototype ('' = `()`), or
# 'CONST' for `use constant`, or undef-free 'SUB' (no prototype).  A
# signature is not a prototype (it holds names).
sub _declared_words {
  my ($doc) = @_;
  my %d;
  my @pkgs = sort { _loc_cmp($a->[0], $b->[0]) }
    map { [$_->location, $_->namespace] } @{ $doc->find('PPI::Statement::Package') || [] };
  $d{"\0pkgs"} = \@pkgs;
  for my $s (@{ $doc->find('PPI::Statement::Sub') || [] }) {
    my $name = $s->name // next;
    my $at = $s->location;
    my $proto;
    for my $tk ($s->children) {
      last if $tk->isa('PPI::Structure::Block');
      if ($tk->isa('PPI::Token::Prototype')) {
        (my $p = $tk->content) =~ s/^\(|\)$//g; $p =~ s/\s+//g;
        $proto = $p if $p =~ /^[\$\@%&*;\\\[\]+_]*$/;
      }
      if ($tk->isa('PPI::Token::Attribute') && $tk->content =~ /^prototype\((.*)\)$/s) {
        ($proto = $1) =~ s/\s+//g;
      }
    }
    my $v = defined $proto ? $proto : 'SUB';
    push @{ $d{_qualify(\@pkgs, $at, $name)} }, [$at, $v];
  }
  for my $inc (@{ $doc->find('PPI::Statement::Include') || [] }) {
    next unless ($inc->module // '') eq 'constant' && $inc->type eq 'use';
    my @k = $inc->schildren;
    shift @k for 1 .. 2;                       # use constant
    my $first = $k[0] or next;
    if ($first->isa('PPI::Structure::Constructor')) {
      my @in = grep { $_->significant } $first->tokens;
      for my $j (1 .. $#in) {
        next unless $in[$j]->isa('PPI::Token::Operator') && $in[$j]->content eq '=>';
        my $key = $in[$j - 1];
        my $v = $key->isa('PPI::Token::Quote') ? $key->string : $key->content;
        push @{ $d{_qualify(\@pkgs, $inc->location, $v)} }, [$inc->location, 'CONST'];
      }
    }
    else {
      my $v = $first->isa('PPI::Token::Quote') ? $first->string
            : $first->isa('PPI::Token::QuoteLike::Words') ? undef : $first->content;
      push @{ $d{_qualify(\@pkgs, $inc->location, $v)} }, [$inc->location, 'CONST'] if defined $v;
    }
  }
  return \%d;
}

# The FEATURE-GATED keywords: toke.c's keyword() answers 0 for these unless
# the feature is in force (FEATURE_*_IS_ENABLED in yyl_word_or_keyword), so
# with the feature off the word is an ordinary user word.  A keyword-plugin
# module that installs the same words (Syntax::Keyword::Try, Object::Pad, ...)
# counts as the feature: the static-parse family of
# docs/static-parse-limits-review-s512.md, answered from the `use` line.
%FEATURE_KW = (
  try => 'try', catch => 'try', finally => 'try', defer => 'defer',
  class => 'class', method => 'class', field => 'class', ADJUST => 'class',
  given => 'switch', when => 'switch', default => 'switch', break => 'switch',
);
my %PLUGIN_FEATURES = (
  'Syntax::Keyword::Try' => ['try'], 'Feature::Compat::Try' => ['try'],
  'Syntax::Keyword::Defer' => ['defer'], 'Feature::Compat::Defer' => ['defer'],
  'Object::Pad' => ['class'], 'Feature::Compat::Class' => ['class'],
);

# The features in force anywhere in the document (`use feature`, `use
# experimental`, a version bundle, a keyword-plugin module).  File-wide, not
# lexical -- a documented simplification.  Bundles (feature.pm): switch in
# :5.10 .. :5.34, try from :5.40; class and defer are in no bundle.
sub _features {
  my ($doc) = @_;
  my %f;
  for my $inc (@{ $doc->find('PPI::Statement::Include') || [] }) {
    next unless $inc->type eq 'use';
    my $m = $inc->module // '';
    if ($m eq 'feature' || $m eq 'experimental') {
      my $txt = $inc->content;
      $f{$_} = 1 for grep { $txt =~ /\b$_\b/ } qw(try defer class switch);
      %f = map { $_ => 1 } qw(try defer class switch) if $txt =~ /\ball\b/;
    }
    $f{$_} = 1 for @{ $PLUGIN_FEATURES{$m} || [] };
    my $v = $inc->version;
    next unless defined $v && length $v;
    my $num = 0;
    if    ($v =~ /^v(\d+)\.(\d+)/ || $v =~ /^(\d+)\.(\d+)\./) { $num = $1 + $2 / 1000 }   # v5.40, 5.10.1
    elsif ($v =~ /^(\d+\.\d+)/)      { $num = $1 }                # 5.040
    $f{try} = 1 if $num >= 5.039;
    $f{switch} = 1 if $num >= 5.009 && $num < 5.035;
  }
  return \%f;
}

# A sub / constant declared in this document BEFORE the token (toke.c asks
# the symbol table at the point of use: a definition further down is not yet
# known, ppi review §1d / #2871).  A qualified word looks up its full name; an
# unqualified one also matches `sub Pkg::name` (package-blind).
sub _decl_before {
  my ($w, $t, $core) = @_;
  my $at = $t->location or return undef;
  my $hit;
  for my $e (@{ $w->{decl}{ _qualify($w->{decl}{"\0pkgs"}, $at, $core) } || [] }) {
    my $l = $e->[0] or next;
    next if _loc_cmp($l, $at) > 0;
    $hit = $e->[1];
  }
  return $hit;
}

# The fully qualified name of NAME as perl's symbol-table lookup sees it at
# location AT: the innermost preceding `package` statement (block scope is
# not tracked -- a documented simplification); `::x` and `main::x` are one.
sub _qualify {
  my ($pkgs, $at, $name) = @_;
  $name =~ s/^(?:main)?::/main::/;
  $name =~ s/'/::/g;
  return $name if $name =~ /::/;
  my $pkg = 'main';
  for my $p (@$pkgs) { last if _loc_cmp($p->[0], $at) > 0; $pkg = $p->[1] }
  return "${pkg}::$name";
}

sub _loc_cmp { my ($a, $b) = @_; $a->[0] <=> $b->[0] || $a->[1] <=> $b->[1] }

# Can this token START a term (the intuit_method lookahead after a bareword)?
sub _starts_term {
  my ($n) = @_;
  return 1 if $n->isa('PPI::Token::Quote') || $n->isa('PPI::Token::QuoteLike')
           || $n->isa('PPI::Token::Symbol') || $n->isa('PPI::Token::Magic')
           || $n->isa('PPI::Token::Number') || $n->isa('PPI::Token::HereDoc');
  return 0;
}

# ════════════════════════════════════════════════════════════════════════════
# SMALL HELPERS
# ════════════════════════════════════════════════════════════════════════════
sub _core { my ($w) = @_; $w =~ s/^CORE::(?:GLOBAL::)?//; $w }
sub _bare { my ($w) = @_; $w =~ s/^(?:.*::)//; $w }

# Two tokens with no whitespace between them (PPI locations: same line,
# the second starting where the first ends).
sub _adjacent {
  my ($a, $b) = @_;
  return 0 unless $a && $b;
  my $la = $a->location or return 0;
  my $lb = $b->location or return 0;
  my $txt = $a->content;
  return 0 if $txt =~ /\n/;
  return ($la->[0] == $lb->[0] && $la->[1] + length($txt) == $lb->[1]) ? 1 : 0;
}

# `{word}` / `{-word}` straight after a subscript's `{` (yyl_leftcurly
# 6324-6343: the word is forced to a string).
sub _in_subscript_forced_word {
  my ($c) = @_;
  my $i = $c->{i};
  my $toks = $c->{toks};
  my ($open, $word, $close) = @$toks[$i - 1, $i, $i + 1];
  if ($c->{t}->isa('PPI::Token::Operator') && $c->{t}->content eq '-') {
    ($open, $word, $close) = @$toks[$i - 1, $i + 1, $i + 2];
  }
  return 0 unless $open && $close && $i > 0;
  return 0 unless $open->isa('PPI::Token::Structure') && $open->content eq '{';
  return 0 unless $close->isa('PPI::Token::Structure') && $close->content eq '}';
  my $s = $open->parent;
  return ($s && $s->isa('PPI::Structure::Subscript')) ? 1 : 0;
}

1;

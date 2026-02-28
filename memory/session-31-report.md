# Session 31: Test Sweep Report (2026-02-15)

## PCL Suite Status
- **47 files, 2360 tests, all passing** (+4 from session 30)

## Perl Test Suite Sweep Results

**103 files | 1154 passing | 309 failing | 9 fully passing | 59 partial | 35 zero**

### Changes from Session 30 (1109 passing)
Net: +45 tests passing

Improved:
  oct.t                  0 ->  77  (+77)   require reordering fix
  context.t              0 ->   6  (+6)    require reordering fix
  repeat.t              35 ->  39  (+4)    UTF-8 encoding fix

Regressed:
  lc.t                  51 ->  33  (-18)
  sprintf2.t            12 ->   2  (-10)
  qq.t                  30 ->  23  (-7)
  chr.t                  9 ->   4  (-5)
  ord.t                 33 ->  31  (-2)

Note: regressions need investigation - may be from stale cached modules
or environment differences between sweep runs.

### Error Categories (crash/blocking reasons)
  21  UNBOUND: THREAD     — threads/fork not implemented
  11  TYPE_ERROR           — type mismatches (box vs raw, hash vs array)
  11  UNDEF_FUNC           — missing function implementations
   5  TRANSPILE_FAIL       — PPI/parser can't handle file
   4  UNDEF_FUNC: box      — function called with box instead of value
   1  UNMATCHED_PAREN      — codegen produces unbalanced parens
   1  STACK_OVERFLOW       — deep recursion

### Fully Passing (9)
dor.t (13), arith2.t (9), bool.t (8), cond.t (4), defined.t (4),
isa.t (4), sleep.t (4), while.t (4), if.t (2)

### Full Results
```
Test                 Pass  Fail  Status
arith.t               100     0  CRASH: UNBOUND: THREAD
oct.t                  77     2
pow.t                  75     2  CRASH: UNDEF_FUNC: box
num.t                  46     0  CRASH: UNDEF_FUNC
split.t                45    19  CRASH: UNBOUND: THREAD
array.t                41    40  CRASH: UNDEF_FUNC
loopctl.t              39     0  exit(1)
repeat.t               39     2  CRASH: UNDEF_FUNC: box
auto.t                 38     1  CRASH: UNBOUND: THREAD
list.t                 38    17  CRASH: TYPE_ERROR
infnan.t               37     2  exit(1)
study.t                35     8
lc.t                   33    23  CRASH: UNDEF_FUNC
chars.t                31     1  CRASH: UNDEF_FUNC
exp.t                  31     1
ord.t                  31     5
chop.t                 28     0  CRASH: TYPE_ERROR
delete.t               24     0  CRASH: TYPE_ERROR
negate.t               23     1  CRASH: UNDEF_FUNC: box
qq.t                   23     7
recurse.t              23     0  CRASH: STACK_OVERFLOW
range.t                21     7  CRASH: TYPE_ERROR
not.t                  18     1  CRASH: UNDEF_FUNC
undef.t                18     6  CRASH: UNDEF_FUNC
unshift.t              17     1  CRASH: UNDEF_FUNC
reset.t                16     8  CRASH: UNBOUND: THREAD
concat.t               15     5  CRASH: UNDEF_FUNC
wantarray.t            14    14
lop.t                  13     0  CRASH: UNDEF_FUNC
dor.t                  13     0  FULLY_PASSING
reverse.t              10     3  CRASH: UNBOUND: THREAD
arith2.t                9     0  FULLY_PASSING
bool.t                  8     0  FULLY_PASSING
qr.t                    7    14  CRASH: UNDEF_FUNC: box
sub.t                   7     3  CRASH: UNBOUND: THREAD
bop.t                   6     0  exit(1)
context.t               6     2
join.t                  6     0  CRASH: UNBOUND: THREAD
aassign.t               5    20  CRASH: UNBOUND: THREAD
int.t                   5     0  exit(1)
chr.t                   4     9  CRASH: UNDEF_FUNC: box
cond.t                  4     0  FULLY_PASSING
defined.t               4     0  FULLY_PASSING
isa.t                   4     0  FULLY_PASSING
local.t                 4     2  CRASH: UNBOUND: THREAD
my.t                    4     0  CRASH: UNBOUND: THREAD
sleep.t                 4     0  FULLY_PASSING
while.t                 4     0  FULLY_PASSING
append.t                3     0  CRASH: UNDEF_FUNC
each.t                  3     1  CRASH: UNBOUND: THREAD
grep.t                  3     4  CRASH: TYPE_ERROR
hashassign.t            3     2  CRASH: UNDEF_FUNC: box
die.t                   3    14  CRASH: UNDEF_FUNC
args.t                  2     0  exit(1)
bless.t                 2    10  CRASH: UNBOUND: THREAD
caller.t                2     5  CRASH: UNDEF_FUNC
defins.t                2     0  CRASH: UNBOUND: THREAD
push.t                  2     0  CRASH: TYPE_ERROR
sprintf2.t              2    10  exit(1)
vec.t                   2     1  CRASH: UNDEF_FUNC
if.t                    2     0  FULLY_PASSING
assignwarn.t            1     0  CRASH: UNDEF_FUNC: box
hash.t                  1     1  CRASH: UNDEF_FUNC: hash
lex.t                   1     1  CRASH: TYPE_ERROR
kvaslice.t              1     0  CRASH: UNDEF_FUNC: box
switch.t                1     3  exit(1)
closure.t               0     4  exit(1)
flip.t                  0     3  exit(1)
pos.t                   0    11  exit(1)
readline.t              0     9  CRASH: UNBOUND: THREAD
kvhslice.t              0     3  CRASH: UNDEF_FUNC
[25 files at 0/0 omitted]
```

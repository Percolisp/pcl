# CPAN Module Testing Plan

## Goal

Test PCL with real CPAN modules to validate compatibility and find gaps.

## Target Modules

| Module | Why | Complexity |
|--------|-----|------------|
| **YAML::Tiny** | Zero deps, pure Perl, simple OO | Low |
| **Text::CSV_PP** | Pure Perl CSV parser, regex-heavy | Medium |
| **Test::More** | Needed to run module test suites | Low |

All are pure Perl with no XS dependencies.

## Directory Structure

```
cpan-tests/
├── modules/           # Downloaded CPAN modules (source)
│   ├── YAML-Tiny/
│   ├── Text-CSV/
│   └── Test-Simple/
├── transpiled/        # Transpiled .lisp files
├── logs/              # Test output and error logs
└── runner.pl          # Test harness script
```

## Phase 1: Setup

1. Create `cpan-tests/` directory structure
2. Download module tarballs from CPAN:
   - https://metacpan.org/release/YAML-Tiny
   - https://metacpan.org/release/Text-CSV
   - https://metacpan.org/release/Test-Simple
3. Extract to `cpan-tests/modules/`

## Phase 2: Test::More Bootstrap

Test::More is needed to run other modules' tests. Start here.

### 2.1 Identify Core Files
```
Test-Simple/lib/
├── Test/More.pm        # Main interface
├── Test/Simple.pm      # Minimal version
├── Test/Builder.pm     # Backend (may be complex)
└── Test/Builder/Module.pm
```

### 2.2 Try Minimal Transpilation
```bash
# Try transpiling Test::Simple first (simpler than Test::More)
./pl2cl cpan-tests/modules/Test-Simple/lib/Test/Simple.pm
```

### 2.3 Document Failures
Create issues list for any Perl features that fail:
- Missing built-ins
- Unsupported syntax
- Runtime errors

## Phase 3: YAML::Tiny

### 3.1 Files
```
YAML-Tiny/lib/YAML/Tiny.pm   # Single file module!
```

### 3.2 Transpile and Test
```bash
# Transpile
./pl2cl cpan-tests/modules/YAML-Tiny/lib/YAML/Tiny.pm \
    > cpan-tests/transpiled/YAML-Tiny.lisp

# Try loading in SBCL
sbcl --load cl/pcl-runtime.lisp \
     --load cpan-tests/transpiled/YAML-Tiny.lisp
```

### 3.3 Run Module Tests
```bash
# The module has its own test suite in t/
ls cpan-tests/modules/YAML-Tiny/t/
```

## Phase 4: Text::CSV_PP

### 4.1 Files
```
Text-CSV/lib/
├── Text/CSV.pm       # Wrapper (tries XS first)
└── Text/CSV_PP.pm    # Pure Perl implementation
```

Use `Text::CSV_PP` directly to avoid XS detection logic.

### 4.2 Transpile
```bash
./pl2cl cpan-tests/modules/Text-CSV/lib/Text/CSV_PP.pm
```

## Phase 5: Integration Testing

### 5.1 Simple Test Script
Create a test that uses the transpiled modules:

```perl
# cpan-tests/test-yaml.pl
use lib 'cpan-tests/modules/YAML-Tiny/lib';
use YAML::Tiny;

my $yaml = YAML::Tiny->read_string("---\nfoo: bar\n");
print $yaml->[0]{foo}, "\n";  # Should print "bar"
```

### 5.2 Run Through PCL
```bash
./pl2cl cpan-tests/test-yaml.pl | sbcl --load cl/pcl-runtime.lisp
```

## Expected Issues

Based on module analysis, likely gaps:

### YAML::Tiny
- Uses `Scalar::Util::blessed()` - need to implement or stub
- Uses `B::svref_2object` for introspection - may need stub
- Heavy regex use for YAML parsing

### Text::CSV_PP
- Complex regex patterns
- Encoding/binmode handling
- Callback mechanisms

### Test::More
- `Exporter` usage (we have basic support)
- Output redirection (STDOUT/STDERR manipulation)
- `caller()` for test location reporting (we have this)

## Success Metrics

1. **Level 1**: Module transpiles without parse errors
2. **Level 2**: Generated Lisp loads without errors
3. **Level 3**: Basic functionality works (create object, call method)
4. **Level 4**: Module's own test suite passes

## Commands Summary

```bash
# Setup
mkdir -p cpan-tests/{modules,transpiled,logs}
cd cpan-tests/modules

# Download (using curl or wget)
curl -LO https://cpan.metacpan.org/authors/id/E/ET/ETHER/YAML-Tiny-1.74.tar.gz
curl -LO https://cpan.metacpan.org/authors/id/I/IS/ISHIGAKI/Text-CSV-2.04.tar.gz
curl -LO https://cpan.metacpan.org/authors/id/E/EX/EXODIST/Test-Simple-1.302199.tar.gz

# Extract
tar xzf YAML-Tiny-*.tar.gz
tar xzf Text-CSV-*.tar.gz
tar xzf Test-Simple-*.tar.gz

# Transpile (from project root)
cd ../..
./pl2cl cpan-tests/modules/YAML-Tiny-1.74/lib/YAML/Tiny.pm
```

## Next Steps After Plan Approval

1. Create directory structure
2. Download and extract modules
3. Start with YAML::Tiny (simplest)
4. Document every failure for future fixes
5. Iterate until basic functionality works

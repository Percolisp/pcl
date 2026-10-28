# PR YAML -- Document: load YAML::PP only to read PPI_CUSTOM_FEATURE_INCLUDES (#3924)

**The problem.** `PPI::Document` has a hard `use YAML::PP` although YAML::PP is
used in one place: parsing `$ENV{PPI_CUSTOM_FEATURE_INCLUDES}`.  A machine
without YAML::PP cannot load PPI at all (found by PCL's first CI run on a stock
runner).

**The change.** The module is `require`d where the variable is read, so PPI
loads and parses without YAML::PP; setting the variable there dies with perl's
own "Can't locate YAML/PP.pm".  `dist.ini`: `[AutoPrereqs]` skips YAML::PP and
`[Prereqs / RuntimeRecommends]` declares it -- an optional feature is a
recommendation.

**Tests** (`t/ppi_document_optional_yaml.t`): PPI loads and parses with
YAML::PP hidden; the variable still works when YAML::PP is present.

**Commits** (branch `pcl`, over upstream master 455bfa73; tests first, marked
$TODO, then the change):

- `6613fff5` TODO tests: PPI loads and parses without YAML::PP
- `46ba7e81` Document: load YAML::PP only to read PPI_CUSTOM_FEATURE_INCLUDES

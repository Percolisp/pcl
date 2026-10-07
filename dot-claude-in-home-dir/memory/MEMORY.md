# PCL Project Memory

## ▶ THIS DIRECTORY AND THE TASK STORE LIVE IN THE REPO (s507, USER 2026-10-04): `~/pcl/dot-claude-in-home-dir/{memory,tasks/pcl}`, the `~/.claude` paths are SYMLINKS — both are PUBLISHED: write nothing personal about the USER; #1833 has what is left (agent briefs) → [project_repo_self_sufficiency_handover](project_repo_self_sufficiency_handover.md)

## ▶ PRODUCT TARGETS (user 2026-07-20)
- [project_product_targets_speed_and_ir](project_product_targets_speed_and_ir.md) — **Target A: as fast as possible in ABSOLUTE time** ([feedback_speed_absolute_not_ratio](feedback_speed_absolute_not_ratio.md); worklist `docs/faster-codegen-suggestions.md`). **Target B: clear IR** (#75).

## ▶ HARD REQUIREMENT (user, 2026-07-07)
- **`eval $str_expression` MUST be supported — never gate as unsupported.** Works today. [project_dynamic_eval_is_required](project_dynamic_eval_is_required.md).

## ▶ ACTIVE: pclxs — the XS bridge, live repo at `~/pclxs`
- [project_xs_shim_design](project_xs_shim_design.md) — ABI 6, XS OO shipped. **The three Pl/t/xs-0N.t files are PARKED (skip_all, USER s485); gate must read Result: PASS; `PCL_XS_TESTS=1` runs them.** NOT pushed to gh (do not prompt). OPEN #117 `io` group.

## ▶ ACTIVE: v2 compiler → ONE PIPELINE
- **▶ STATE (s512 RUNNING, started 2026-10-07 23:15 EEST; USER: "Please continue.") → `~/pcl/briefs-and-rules-for-claude-subagents/s512/PAUSE-s512.md` (the running notes; read it first): main = `9f2207c6` (the s512 briefs) on last CODE commit **`3ea47a41`** (s510p, gen v2-4480; gate 282 files / 9,840 rows; sweep 18735; EVERYDAY 114 of 122 = 93.4 %); CI green through `d2f7e757`.  Cap THREE (USER ~23:46: "Please run three parallel jobs").  RUNNING: **s510b** (`agent-acb992c6718ea4a5d`, binmode in place #2777 + `$, = 0` #2778) and **s510f** (`agent-a9518b536c5798126`, the first run from text #2702), fresh Opus agents from `s512/resume-<label>.md` and **s510c** (`agent-a0324b48405eea0b6`, the fallback bind goes; its OPEN REGRESSION Text-Balanced 05_extmul.t heap exhaustion is step 1 -- the backtrace says `%p-regex-parts`); **perf round 41 BRIEFED** (`s512/s512p-prompt.md`: #2723 end-anchored regex, designed + measured in the task; #2771 residue) takes the next free slot.  Merge order s510b -> s510f -> s510c; `MAIN-READY-N` files and the lock script in `~/pcl-agent-scratch/s512/`.  DONE by Fable in s512: the s510 + s511 session-log sections; s510f`s merge-review probes (no finding); **the static-parsing review (USER question) = `docs/static-parse-limits-review-s512.md`** (NEW #2871 position-aware prototypes, #2872, #2873 autodie, #2874 bigint).  After the three merge the queue is EMPTY of briefed work -- candidates need the USER`s word: #2779 (a replaced built-in gets the built-in`s argument count), the #2702 README follow-up, perf round 41, the run-time detector sketch (#2610).  Start-up items STAY PARKED (USER 2026-10-06).  CI`s perl is 5.38.2.  Fable free task IDs 2875-2879, then 2900+ (s512p has 2880-2899).**
- **Standing USER rulings (detail in the state file): subjob cap stated per session (default TWO); perf PRIORITIZED BY MEASURED GAIN (s499); START-UP perf stays parked and the present aim is SLEEK FUNCTIONALITY + GOOD DOCUMENTATION (2026-10-06); no new correctness batch or test angle unasked (s494/s498); steer by the EVERYDAY number.**
- **EVERYDAY battery (USER s491) → [project_everyday_battery](project_everyday_battery.md): instrument `tools/everyday-smoke.pl`; "it loads" is NOT "it works"; OWED battery 3; NO README sentence about it (USER s495).**
- **Test angles (USER s493) → [project_test_angles_s493](project_test_angles_s493.md). The META LIST of questions → [reference_questions_worth_asking](reference_questions_worth_asking.md).**
- **THE INTERLEAVED PLAN (USER s452): every round = ONE perf agent + 1–2 correctness agents; prose/documentation passes on OPUS (USER s496).**
- **Per-session findings/traps: [project_sessions_411_427_notes](project_sessions_411_427_notes.md), [project_sessions_404_410_notes](project_sessions_404_410_notes.md)**; narratives in `docs/session-log.md` + DECIDED.
- **MEASUREMENT TRAPS + s438/s444 lessons → [project_pcl_measurement_traps](project_pcl_measurement_traps.md)**; **PPI MIS-LEXES → [reference_ppi_lexing_traps](reference_ppi_lexing_traps.md)**. Method: sibling shape, ONE predicate, ~10 probes vs perl.
- **A vanished sweep VERDICT: rebuild from `.faillog/_status.tsv` with `tools/sweep-diff.pl`** (#273).
- **▶ QUEUE = `docs/plan-post-s433.md`**. v0.1.0 TAGGED s440. Rulings go in DECIDED + session-log + the live plan doc (no review-doc families); 50% compile budget per change.
- **A capture-dependent EMISSION must key the eval CACHE** (s387). **Block-scoped vs FILE-level lexicals reach an eval by DIFFERENT mechanisms** — capture alist vs package cell.
- **Direction D global-representation facts** → [project_direction_d_globals](project_direction_d_globals.md); a `let` of a symbol-macro name SHADOWS it — that IS the my-shadow mechanism.
- **Loop/condition-HEAD `my` needs its own `let` (#297); exception-partition `my` = a different SYMBOL (#296); a rename region ENDS at the next same-name declaration (#296-B2); for a rename/scoping change the full sweep IS the gate.**
- **PIPELINE: v2 is the ONLY one (s356; [project_parser2_prototype](project_parser2_prototype.md)); ONE expression compiler + ONE seam `capture_v1`.** BUMP `*pcl-cache-generation*` on emission changes.
- **A "dead" global declaration is NOT emission-neutral** [feedback_dead_cell_not_neutral](feedback_dead_cell_not_neutral.md).
- **Interpolation scanning happens in `Pl/InterpScan.pm` or not at all** (standing rule §8); contract `docs/interp-scan.md`.
- **A bare NAME is a CALL only where it is CALLABLE; the two negatives differ** (#266). **Grep for the sibling copy before fixing an autoquote/callable site.**
- **Run `tools/corpus-diff.pl` BEFORE spending a full sweep** — identical emission proves the `.t` half cannot move. **Normalize compiler LINE NUMBERS in any gate-SET stderr diff.**
- **A SIGIL- or SCOPE-blind capture refusal is a BUG** — delete/rename, never narrow; **detector/rewriter/promoter share ONE resolver**. **Never `nohup` the gate** [feedback_never_nohup_the_gate](feedback_never_nohup_the_gate.md).
- **When a fix widens what a CHECKER sees, diff the GATE SET file-by-file over BOTH populations** → [project_v2_gate_set_measurement_rules](project_v2_gate_set_measurement_rules.md).
- **De-gated ≠ done**: the bar is the file's `perl-suite-run.tsv` snapshot C_ok. **CPAN board PASS/PARTIAL labels are not the measure — read ROWS.** **The term walker declines bare words and prefix ops BY DESIGN.**
- **BOXED AGGREGATES: ALL PHASES SHIPPED — elements RAW by default; PCL_RAW_ELEMS=0 = the all-boxed A/B world** (`docs/boxed-aggregates-design-s455.md`).
- **THREE CHECKED-IN TRANSPILED ARTIFACTS (`cl/pcl-pack/-mro/-warnings.lisp`) — regenerate after emission changes (`artifact-staleness-01.t` gates it).** **A worktree compare drops the 14 xs rows — set PCLXS_DIR.** Write inverse guards; never `cp` a snapshot over a live file [feedback_no_stash_when_stash_exists](feedback_no_stash_when_stash_exists.md).
- **USER: errors must FAIL IN THE SAME PLACES as perl and be TIDY (one line, never a Lisp backtrace); exact TEXT is not a goal (refined s494)** [project_error_message_fidelity_not_required](project_error_message_fidelity_not_required.md); ignore op/cond.t (TIMEOUT = MemoryMax guard); flag any bugfix adding complexity or slowing execution.
- **`docs/ir-spec.md` = NORMATIVE semantics manual — UPDATE on semantic emission/runtime changes.** CPAN suite baselines in [project_s304_state](project_s304_state.md).

## ▶ LATER / PARKED
- **JS target (fun, USER s448)**: `docs/js-target-plan.md`, #622 — do not start unasked (speed+correctness FIRST).
- **C/C++ target + libperl: REVIEWED NO (DECIDED §s481, `docs/c-target-notes.md` §8).** Perl stays the ORACLE; never a libperl runtime dependency.
- **Single-binary + diagnostics plans**: docs/single-binary-plan.md = #756, #757 — planning only, unscheduled.
- **Return-family transfer** (post-E4): [[project_return_family_transfer]] — task #77.
- **Unicode**: [[project_next_unicode_property_regex]]; `\N{NAME}` → shim `sb-unicode:name-char`.
- **Sweep regression hunt** (parked): [[project_eval_package_leak_regression]]; [[project_fully_passing_regression_hunt]].
- **Perl's own t/ as bug finder**: [[project_perl_suite_survey]], [[project_suite_runner_oom_and_cond_t]], [[project_io_subprocess_features]], [[reference_box_magic_hook]]. **sb-posix**: some syms `sb-posix::`; stat-blksize/blocks/futimes ABSENT.

## User Preferences / Working Style
- **FABLE NEVER EXECUTES what an Opus agent can do from a written design — Fable = review, rulings, design, merge.** [feedback_fable_never_executes_delegate_to_opus](feedback_fable_never_executes_delegate_to_opus.md), [feedback_hard_parts_first_e2_for_opus](feedback_hard_parts_first_e2_for_opus.md)
- **TWO SUBJOBS AT A TIME (USER 2026-09-05)** — never a third concurrently unless the USER allows it for the day. [feedback_two_agents_at_a_time](feedback_two_agents_at_a_time.md)
- **ALWAYS pass an explicit `model` on Agent launches ("opus"; an unpinned subagent INHERITS Fable). USER s501: execution agents are OPUS 5.5 — each writes its model id to `scratch/<label>/MODEL.txt` first, Fable checks it.** [feedback_subagents_must_pin_model](feedback_subagents_must_pin_model.md)
- **Agent worktrees branch from the SESSION-START commit — tell each agent to rebase first, verify with `git worktree list`** [reference_agent_worktree_base_is_session_start](reference_agent_worktree_base_is_session_start.md)
- **STRUCTURAL FIRST, NOT AT ANY COST (USER 2026-08-18)**: found bugs are FILED, jump only if they regress a baseline or block a phase; every step names its bar. [feedback_structural_first_not_at_any_cost](feedback_structural_first_not_at_any_cost.md)
- **DUP-CENSUS SCOPE = COMPILER + RUNTIME ONLY (USER s413)**; tools may be replaced; tests are never optimized. [feedback_dup_census_compiler_only](feedback_dup_census_compiler_only.md)
- **SIGN-OFF RULE (USER s379): simpler + clearer + generated code faster (or unchanged) + compile <50% worse needs NO ask.** [feedback_signoff_rule_simpler_faster](feedback_signoff_rule_simpler_faster.md)
- **A plan calling a file "new" is not proof it is** — check before Write; a FALLING gate row count is a finding [feedback_write_tool_overwrites](feedback_write_tool_overwrites.md)
- **No session log in MEMORY.md** — update the STATE line in place. [feedback_no_session_log_in_memory](feedback_no_session_log_in_memory.md)
- **Narration**: brief, results over commentary. [feedback_narrate_progress](feedback_narrate_progress.md) **Explain carefully** for CL/SBCL internals. [user_explanation_style](user_explanation_style.md)
- **NOT SOFTWARE FOR OUR LAPTOP (USER 2026-09-06): a platform-touching change ships only with the install matrix + a macOS leg green; ONE regex engine.** [feedback_portability_os_matrix](feedback_portability_os_matrix.md)
- **Always use Perl** for scripting. **Quote shell vars as paths** [feedback_quote_shell_vars](feedback_quote_shell_vars.md); **no `unless`** [feedback_no_unless_in_perl](feedback_no_unless_in_perl.md); **`perl -i` + a SLURP TRUNCATES** — write a new path, verify, `cp` [feedback_perl_i_slurp_truncates](feedback_perl_i_slurp_truncates.md).
- **Encode with a TRUE CHECK EMPTIES its source** [reference_encode_check_modifies_source](reference_encode_check_modifies_source.md). **`local $/` LEAKS past its open** [feedback_local_input_record_separator_leaks](feedback_local_input_record_separator_leaks.md).
- **No redundant background-wait; NEVER `until ! pgrep -f`** [feedback_no_redundant_bg_wait](feedback_no_redundant_bg_wait.md). **Killing `timeout N cmd` kills the command** [feedback_timeout_forwards_sigterm](feedback_timeout_forwards_sigterm.md).
- **PUT THINGS LIVE BEFORE NEW WORK (USER 2026-09-05): merge+push the awaiting batches first** [feedback_put_live_before_new_work](feedback_put_live_before_new_work.md)
- **PUSH AT FABLE`S DISCRETION (USER s494: "push when you think you should")** — a one-off "not now" is about that moment, not a standing hold; never sweep the USER`s uncommitted README edit into a commit. [feedback_push_at_fable_discretion](feedback_push_at_fable_discretion.md)
- **Stop when told; write session state on task change** [feedback_write_session_state](feedback_write_session_state.md); **commit finished work each session** [feedback_commit_per_session](feedback_commit_per_session.md); **no `git stash` for compares**.
- **Never simplify a failing test** [feedback_no_simplify_tests](feedback_no_simplify_tests.md); **ask before commenting out tests**; **no test code in production files** [feedback_no_test_code_in_production](feedback_no_test_code_in_production.md).
- **Don't write off fixable things — TEST the primitive first** [feedback_dont_write_off_fixable](feedback_dont_write_off_fixable.md). **A COUNT is not a finding — re-run each failure for its CAUSE** [feedback_cause_not_count](feedback_cause_not_count.md).
- **`sweep-diff` compares FAIL rows only — read the sweep TOTAL passing line too** [feedback_check_total_not_just_diff](feedback_check_total_not_just_diff.md). **Widening a parser rule: probe the case it would BREAK** [feedback_probe_the_breaking_case](feedback_probe_the_breaking_case.md).
- **Review probes: perl → BASE → tree; a probe the base passes has not reached the bug; a claimed bar must be on disk** [feedback_review_probe_on_base_first](feedback_review_probe_on_base_first.md)
- **SPEED > readable generated Lisp** [feedback_speed_over_readable_lisp](feedback_speed_over_readable_lisp.md). **Network/CPAN OK; dist fetches blanket-OK'd, system installs still ask** [project_network_and_cpan_available](project_network_and_cpan_available.md). **SBCL floor 2.5.2+** [project_sbcl_version_floor](project_sbcl_version_floor.md).

## Where things go
- **CI = a STOCK Ubuntu runner; deps EXACTLY PPI ≥ 1.291 + Moo (guard `Pl/t/core-deps-01.t`)** [project_ci_stock_machine](project_ci_stock_machine.md). `gh` unauthed — check via the public API (`curl -s https://api.github.com/repos/Percolisp/pcl/actions/runs?per_page=3`).
- **RUNTIME KEPT COMPILED AND CACHED (USER s439b)**: content-keyed core under ~/.pcl-cache/core/ (PCL_NO_CORE=1 = source; a worktree gets its OWN core) — [project_test_core_fast_path](project_test_core_fast_path.md). **Keep docs/test-failures-categorized.md current** [feedback_update_test_log](feedback_update_test_log.md).
- **Session history → `docs/session-log.md`** (NOT here). New Pl/t tests: never -01/-07, next is `-10`; metric = [WALL TIME, not rows](feedback_test_file_size_cap.md).
- **WHAT TO RUN WHEN = the CLAUDE.md table keyed on WHAT CHANGED** [feedback_sweep_cadence](feedback_sweep_cadence.md)
- **Runtime = real Perl semantics only**, no stubs. **Fix CPAN modules at the RIGHT LAYER** [feedback_fix_at_right_layer](feedback_fix_at_right_layer.md). **Reuse, don't duplicate** [feedback_reuse_dont_duplicate](feedback_reuse_dont_duplicate.md). **When behaviour is subtly wrong, LOOK FOR A SECOND COPY first** [feedback_check_for_a_second_copy](feedback_check_for_a_second_copy.md)
- CL discipline: **2-space depth indentation** [feedback_cl_indentation_depth](feedback_cl_indentation_depth.md); **split on `(defun` for parens** [feedback_split_lisp_on_defun](feedback_split_lisp_on_defun.md); **AST over string-matching**; **no string-rewriting codegen**.

## Debugging / triage / helpers
- **Hang/crash protocol** [feedback_debugging_hangs](feedback_debugging_hangs.md). **Triage: runbook + GREP not-supported.md BEFORE probing**; not-supported → skip-registry, never edit `perl-tests/*.t`.
- **NEVER `cp` onto a path in a symlinked shadow tree** (s316v). [project_partial_stop_analysis](project_partial_stop_analysis.md); [feedback_fully_passing_regression](feedback_fully_passing_regression.md); [project_sweep_flakiness_investigation](project_sweep_flakiness_investigation.md).
- **Population-wide instruments: `tools/gate-set-scan.pl`** (the s372 gate-SET rule as a tool) **and `tools/drop-census.pl`** (blessed census `baselines/parse-error-drop-census-s399.tsv`). Both take `PCL_PERL_SUITE_T`.
- Task store = `~/.claude/tasks/pcl/NNN.json`; write with `JSON::PP->new->utf8` to a `:raw` handle [reference_task_store_json](reference_task_store_json.md).
- `./runpcl` (NUL→`grep -a`), `tools/runt` (plain `--load`: aborts on an uncaught die — the sweep's `p-load-with-recovery` does not), `tools/clt`, `perl tools/sweep-perl-tests.pl` [reference_runt_script](reference_runt_script.md); fuzzer [project_difftest_fuzzer](project_difftest_fuzzer.md). **STALE CACHE**: `rm -rf ~/.pcl-cache/*`. **WANTARRAY LEAK**: new sensitive builtin → `%WANTARRAY_SENSITIVE`.

## CPAN modules / Moo / OO
- **BOARD STATE + ORACLE RULES → [project_cpan_board_state](project_cpan_board_state.md)**.
- CPAN refs: [[project_cpan_module_log]], [[project_cpan_test_suite_strategy]], [[project_cpan_pureperl_findings]], [[project_io_tests_and_open_errors]]. Moo ([[project_moo_progress]], docs/moo-status.md); [[project_module_compile_load_double_exec]], [[project_coderef_identity_blocker]], [[project_tie_status_and_roadmap_decisions]].

## Big design docs / bug groups / reference hooks
- **`docs/v2-target-architecture.md` = TARGET SHAPE of v2-final**; gap analysis `docs/v2-code-review.md`; older rewrite docs → [project_codegen_rewrite_review](project_codegen_rewrite_review.md).
- `docs/wantarray-context.md` [project_wantarray_followup](project_wantarray_followup.md); [:invert case-sensitivity](project_case_sensitivity_general_fix.md); `eval-string-plan.md` ([project_string_eval_lexical_capture](project_string_eval_lexical_capture.md)); `shipped-modules.md`; PPI upstream bugs → `docs/ppi-upstream-bugs.md`.
- [[project_bug_groups]]; pack-failure-groups.md (cl/pack-impl.pl=oracle); [[project_math_bigint_shim]]; [[project_array_aassign_review_gate]]; [[project_method_dispatch_subname_hoist]]; [[project_power_op_float_divergence]]. Before declaring not-supported: [[reference_box_magic_hook]], [[reference_tap_todo_support]], [[project_crash_analysis]].
- **`in-package` is READ-time, per top-level form** — a package switch inside a block must emit QUALIFIED names. [reference_in_package_is_read_time](reference_in_package_is_read_time.md)
- Method dispatch, CL-SBCL runtime, PPI tokenisation and output buckets → [reference_cl_ppi_gotchas](reference_cl_ppi_gotchas.md). Read before editing `cl/pcl-runtime.lisp` or PExpr.

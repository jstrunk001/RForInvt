# Refinement Plan — Living Checklist

Mirrors the approved plan. Tick items as completed. Run `devtools::document()` + `devtools::test()`
as checkpoints after Phases 2, 3, and 7.

## Phase 1 — Metadata & build hygiene
- [x] `DESCRIPTION`: rewrote `Imports:` (no leading commas, no duplicate `plyr`), reconciled with NAMESPACE (added `flock`, `openxlsx`), added `Suggests: testthat, withr`, `License: GPL (>= 2)`. Placeholder email kept.
- [x] `.Rbuildignore`: added `^_dev$`, `^_archive$`, root-level `*.db` (`^[^/]*\.db$`, preserves `inst/extdata/FIADB_RI.db`), `fort.2`, `\.Rhistory$`, `inst/NVEL/NBEL_CPP_DAL`, `inst/tidyverse_coding_convention`.
- [x] `README.md`: fixed `install.packages("remotes)` quote, "inall"→"install", broken `misc/NBEL`→`NVEL` db path, added scope paragraph + function-group map.

## Phase 2 — Verified bug fixes (see 04_bugs_verified.md) — ALL FIXED
- [x] knn_tools `browser()` (197), `data.frameQ` (330) + stray `left_join` arg
- [x] sampleSystematic `browser()` (108) + latent broken recursive `sample.systematic` self-call
- [x] ols_modeling `modk` (286), `mod_multi` undefined ref + mismatched helper name
- [x] NVEL_biomass `dll_func` (127) — resolved via Phase-4 shared loader
- [x] compile_plots `sqldf` env (250/256), `require("reshape2")` (563)
- [x] make_strata `type_x1`/`type_x2` dead branch + unused `tbtest`/`nmtest`
- [x] estimators — re-diagnosed: removed misfiring unconditional `warning()` (the `wt_nm` claim was a misread)

## Phase 3 — snake_case standardization (see 05_naming_inventory.md)
- [x] Renamed `sampleSystematic`→`sample_systematic`, `pandermod`→`pander_mod` (+3 S3 methods + UseMethod), `fvs_protype_params`→`fvs_prototype_params` (files + all call sites + README + Rmd)
- [x] `pander_mod` cleanup: dropped in-body `require()`, removed dead `pandermod_old`, `inherits()` over `class()!=`, fixed `.simex` missing return
- [x] `RForInvt.R` overview: `compilePlots`/`compileTrees`/`sampleSystematic` → snake_case
- [x] `devtools::document()` regenerated NAMESPACE + man/ cleanly (also fixed `@noRd`-followed-by-text in model_predict.R)
- [ ] NVEL `*Nm` args → snake_case — **DEFERRED** (see Phase 4 rationale)
- [ ] knn camelCase args → snake_case — moved to Phase 6 (refine knn_tools in place)
- [ ] Internal vars in estimators/ols_modeling/knn_tools/yai_r2 — moved to Phase 6

## Phase 4 — NVEL consolidation
- [x] `R/nvel_internal.R`: single `.nvel_load_dll()` + `.nvel_dll_paths()` — removed 5 colliding `.load_dll` copies (same name at package scope; only one survived). Updated all 6 call sites incl. the parallel worker in NVEL_buck.
- [x] Standardize `NVEL_volume` to return `data.table` (`as.data.table` at the end); buck/merch/wtfactor already did.
- [x] Remove dead stubs in NVEL_volume (`NVEL_ht2topd`/`NVEL_calcdob`/**empty `NVEL_biomass` that was shadowing the real one**) + `if(F)` blocks in NVEL_voleq, NVEL_volume, NVEL_wtfactor.
- [x] Fix `class(dfTL)=="logical"` → `is.logical()` in NVEL_voleq.
- [ ] **DEFERRED (documented):** `*Nm` → `*_nm` argument rename across the 6 files, and a shared `.nvel_format_treelist()`/`.nvel_validate()`. Rationale below.

> **Deferral rationale (NVEL `*Nm` rename + format/validate consolidation):** these args are
> referenced deep in the Fortran `mapply`/`.Fortran` marshalling and, in `NVEL_buck`, inside the
> `parallel::clusterExport` *string* varlist (`"dbhNm"`, `"htNm"`, ...). The bundled `vollib.dll`
> cannot load in the review environment, so a missed reference would be silent and untestable here.
> Recommended as a focused, test-backed change run where the DLL is available. The `*Nm` suffix is at
> least *internally consistent* across all six NVEL files today, so this is the lowest-urgency naming gap.

## Phase 5 — Roxygen + examples sweep (PARTIAL — see status)
- [x] Fixed check-breaking package-level example in `RForInvt.R` (wrong pkg `RSForInvt`, wrong db path, negated guard, broken `\link`s) — now `\donttest`-wrapped and correct.
- [x] Fixed NVEL doc/signature contradictions: `NVEL_biomass.Rd` empty signature (was the deleted shadow stub — now correct); `NVEL_merch` example used non-existent `NVEL_buck` args (`col_id`…) → real `*Nm` args; `NVEL_wtfactor` title/return/examples were copied from `NVEL_voleq` → rewritten for weight factors; `NVEL_volume` example db path + negated `df_fake` guard fixed and `\donttest`-wrapped.
- [ ] **REMAINING:** replace ~47 placeholder/`tbd` roxygen entries in `ols_modeling.R` and the placeholders in `knn_tools.R`, `pairs2.R`, `dist_vec.R`, `gsub_vec.R`, `sample_systematic.R`, `duplicated2.R`. Large, per-function documentation task — best done with the function authors' domain knowledge.
- [ ] **REMAINING:** verify `write_clipboard.Rd` / `dot-estimate.Rd` correspond to real functions.

## Phase 6 — Legacy refinement in place (PARTIAL)
- [x] `pander_mod.R`: in-body `require()` removed, dead `pandermod_old` block removed, `inherits()` adopted.
- [x] `compile_plots.R`: in-body `require("reshape2")` removed.
- [ ] **REMAINING:** snake_case internal variables + knn camelCase **arg** renames in `estimators.r`/`ols_modeling.R`/`knn_tools.R`/`yai_r2.R`; swap remaining in-body `require()` for guarded `requireNamespace()`; remove the large commented-out tails (`estimators.r` ~L572-811, `ols_modeling.R` ~L793-1027, `knn_tools.R` ~L499-693).
- [ ] **REMAINING:** flag/propose `bootstrap`→`boot` (documented in 00_overview).
- [ ] **REMAINING:** extract duplicated model_archive helpers (registry read, column-union/fill, atomic write).

## Phase 7 — Tests & validation (PARTIAL)
- [x] Regression-checked against source via `load_all`: `test-nvel_merch.R` 9/9 pass; `model_archive` all pass (tests made self-contained — see below); `archive_table` skipped (optional deps). NVEL_buck failures confirmed **pre-existing** (HEAD errors outright). No regressions from this refinement.
- [x] Removed the implicit sibling-package coupling in the model-archive subsystem: `package` default now `"RForInvt"`, glob pattern generic `"\\.csv$"`, load registry default `"models.csv"`; tests build their own temp fixtures. No external/companion package is referenced anywhere in the shipped package.
- [x] `devtools::document()` clean; all `R/` files parse; package loads via `load_all`.
- [ ] **REMAINING:** new tests (make_strata/assign_strata, NVEL smoke DLL-gated, yai_r2, ols_modeling/knn_tools happy-path); full `devtools::check()` to 0/0 (needs all deps + companion pkgs installed); `devtools::build()` size check.

## Known remaining issues (documented, deferred)
- NVEL `*Nm` → `*_nm` argument rename (Phase 4 rationale: untestable DLL path).
- `NVEL_buck` zero-volume functional bug (pre-existing; see 04_bugs_verified.md).
- dplyr/plyr wholesale `@import` masking warnings — switch to `importFrom` to silence (design cleanup).

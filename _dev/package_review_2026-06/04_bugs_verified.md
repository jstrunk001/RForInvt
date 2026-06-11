# Verified Bug Register

Each entry was confirmed by direct inspection (grep/read) during the 2026-06 audit, not just reported.
Status: `open` → not yet fixed; `fixed` → corrected and verified.

| # | File:Line | Bug | Fix | Status |
|---|---|---|---|---|
| 1 | `R/knn_tools.R:197` | Unconditional `browser()` in `impute_id()` — hangs non-interactive use. | Removed. | **fixed** |
| 2 | `R/knn_tools.R:330` | `data.frameQ(...)` typo in `tl_impute_2()` — function cannot run. Also a stray positional `y` arg in the adjacent `left_join`. | `data.frame(...)`; removed stray `y`. | **fixed** |
| 3 | `R/sampleSystematic.R:108` | `if(max(idx) > N) browser()` — debug hook in shipped code. | Replaced with a guarded `stop()` (only when `as_idx` and non-empty). | **fixed** |
| 4 | `R/ols_modeling.R:286` | Undefined identifier `modk` (loop var is `mod_k`). | Use `mod_k`. | **fixed** |
| 5 | `R/ols_modeling.R:~421` | `mod_multi()` references undefined `reg_multi_obj` and calls helper `reg_model_in` defined as `models_in` — doubly uncallable. | Renamed to use the `mods_list` param and a consistently named `reg_model_in` helper. | **fixed** |
| 6 | `R/NVEL_biomass.R:127` | `.load_dll(dll_64,dll_32,dll_func)` — `dll_func` not defined in `NVEL_biomass` (the arg is `dll_func_bio`). | Pass `dll_func_bio` (the unused 3rd param is dropped in Phase-4 `.nvel_load_dll`). | **fixed** |
| 7 | `R/compile_plots.R:250,256` | `sqldf(..., envir=as.environment(data))` — `data` is undefined in scope. | Evaluate the scalar filter against the function `environment()` (where `df_plot`/`df_tree` live). | **fixed** |
| 8 | `R/compile_plots.R:563` | `require("reshape2")` inside a function body. | Removed; `reshape2` is already a package import. | **fixed** |
| 9 | `R/make_strata.R:392-401` | Collapse branch references undefined `type_x1` / `type_x2` (empty stub blocks); also unused `tbtest`/`nmtest` reading a non-existent column. | Removed the dead stub branches and unused vars; kept the working nearest-bin collapse. | **fixed** |
| 10 | `R/estimators.r:37` | **Re-diagnosed:** the `wt_nm` assignments at L29/34 are correctly guarded by `is.na(wt_nm[1])` (not a bug). The real defect was the **unconditional `warning()`** that fired on every call, including valid `ht`/`regression`/`stratified` types. | Removed the misfiring warning (unimplemented branches already `stop()`). | **fixed** |

## Lower-severity correctness items (fold into Phase 5/6)

- `R/model_predict.R:111,119` — `try(..., silent=TRUE)` converts prediction errors to `NA` with no
  message. Replace with `tryCatch` that emits a `warning(sprintf(...))`.
- `R/ols_modeling.R` — depends on archived CRAN package `bootstrap` (`bootpred`). Flag; propose `boot`.
- `R/yai_r2.R` — hardcoded `.o` observed-column suffix; factor columns silently yield `NA`.
- `R/NVEL_*` — no `try`/validation around `.Fortran()` calls; magic output indices
  (`res_vol0[[21]]`, `matrix(0,7,20)`) undocumented.

## Pre-existing functional issues found during refinement (NOT regressions)

- **`NVEL_buck` returns zero volumes / no log expansion.** On the committed HEAD, `NVEL_buck` *errors*
  outright (`unused argument (dll_func_vol)` — a symptom of the old 5-way `.load_dll` name collision).
  After Phase-4 consolidation it *runs* but produces `LG_NUM=0` / `TR_TCFV_ALL=0`, because the bucking
  path does not obtain a valid `voleq` for the test trees (the DLL itself works — `NVEL_voleq` returns
  real equation codes in the same session). This is a pre-existing functional bug in the bucking
  pipeline, out of scope for the quality refinement and not fixed here. `tests/testthat/test-nvel_buck.R`
  encodes the *intended* behavior and currently fails on this.
- **Parallel `NVEL_buck` test under `load_all`.** Workers run `clusterEvalQ(library(RForInvt))`, which
  loads the *installed* package; under `devtools::load_all()` that is the stale install, so the worker
  cannot see in-memory helpers. This resolves after `R CMD INSTALL`. The old `.load_dll` had the same
  property — not a regression.

## Test-environment notes (failures unrelated to code)

- `test-model_archive_engine.R`: now fully self-contained — the model-archive functions no longer
  default to a sibling package's shipped registry (the `package` default is `"RForInvt"`, the glob
  pattern is generic `"\\.csv$"`, and the load default registry is `"models.csv"`). The list/load/predict
  tests build their own temp-archive fixture via `model_archive_save`, so all pass with no external package.
- `test-archive_table.r`: skipped here (optional `sf`/`openxlsx`/`withr` paths).
- `test-nvel_merch.R`: 9/9 pass against the refactored source.

## Doc/signature contradictions (Phase 5)

- `man/NVEL_biomass.Rd` — shows `NVEL_biomass()` with no params; function has 15+.
- `man/NVEL_merch.Rd` — examples call non-existent args (`col_id`, `col_spp`, `col_dbh`, `col_ht`,
  `log_len`); real args are `log_map` / `spec_map`.
- `man/NVEL_wtfactor.Rd` — `@title`/`@return` copied from `NVEL_voleq`.
- `NVEL_volume.R` roxygen — example guard `if("df_fake" %in% ls())` is negated wrong.
- `man/OLS_modeling.Rd`, `man/knn_tools.Rd` — generated from placeholder roxygen (`tbd` / "Delete and Replace").

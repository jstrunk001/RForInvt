# Findings by File

Consolidated from the 2026-06 audit (three parallel reviews: NVEL family, modeling/archive, compile/FVS/utilities).
`file:line` citations are from the audit snapshot; line numbers may drift as refinement proceeds.

## NVEL family

**NVEL_volume.R** — Calls `.Fortran("vollib_r", ...)`. Returns a **data.frame** (inconsistent with the
rest of the family, which returns data.table). Sets `options(stringsAsFactors=FALSE)` globally and
toggles `options(warn=-1)`. Contains dead stubs `NVEL_ht2topd`, `NVEL_calcdob`, and an empty
`NVEL_biomass`. Magic literal `c(0,0,...)` (15 zeros) instead of `rep(0,15)`. Roxygen example guard is
negated incorrectly.

**NVEL_buck.R** — Most modern: data.table-native, optional PSOCK parallelism, good roxygen + runnable
examples. Magic output indices (`res_vol0[[21]]`, `matrix(0,7,20)`, `matrix(0,3,21)`) are undocumented.
Possible redundant column-existence check after `.formatTL2NVEL2` already standardized names.

**NVEL_merch.R** — Clean post-processing using data.table non-equi joins; no DLL. The cleanest NVEL file.
Lacks validation that `log_map`/`spec_map` contain required keys. `.Rd` examples reference non-existent args.

**NVEL_voleq.R** — Calls `.Fortran("getvoleq_r", ...)`. Confusing default
`dfTL = list(NA, data.frame(...))[[1]]`. `class(dfTL) == "logical"` instead of `is.logical()`. Contains an
`if(F){...}` dev/test block left in source. No tests.

**NVEL_biomass.R** — Calls `.Fortran("biomasslib_r", ...)`. **Live bug** at L127: `.load_dll(...,dll_func)`
where `dll_func` is undefined (arg is `dll_func_bio`). `.Rd` shows an empty signature. Untested.

**NVEL_wtfactor.R** — Calls `.Fortran("getwtfactor_r", ...)`. data.table-native with a `requireNamespace`
guard and DLL-existence check (good). `.Rd` `@title`/`@return` are copied from `NVEL_voleq`. Untested.

**Cross-cutting:** `.load_dll` is reimplemented in 5 files; `.formatTL2NVEL*` in 3; `.fn_fortran_*`
wrappers in all 6 — ~31% boilerplate (~450 lines). Hardcoded DLL path repeated in every file. No
input validation; no `try` around `.Fortran`. → Phase 4 consolidation.

## Model archive & prediction (newer, good)

**model_archive_internal.R** — Path resolution, registry seeding, atomic `saveRDS`, per-class fit
serialization (xgboost→.ubj, else .rds), provenance checks. snake_case throughout, `@keywords internal`/`@noRd`.
Hardcoded model_class→package map (extend = code edit). Sidecar path fallback order could silently accept wrong file.

**model_archive_list.R** — Scans/filters registry CSVs. Good roxygen. No regex validation; bad filter
expression gives cryptic error.

**model_archive_load.R** — Loads rows + hydrates sidecars by id/filter. Good roxygen + deprecation shim
for `registry_path`. Missing sidecars silently NULL; loops row-by-row.

**model_archive_save.R** — Validates, serializes sidecars, appends registry atomically. Excellent roxygen.
Column-union/fill logic duplicates load-side; hardcoded `fits/`,`vcov/`,`predictor_specs/`; JSON write not
using the atomic helper.

**model_predict.R** — Applies closed-form or fit-based models; per-class adapters. Excellent roxygen.
`try(...,silent=TRUE)`→NA swallows errors (L111,119); `setnames(skip_absent=TRUE)` can silently skip all columns.

**archive_table.R** — Multi-format writer (CSV/RDS/SQLite/XLSX) with per-format `tryCatch` isolation,
column-name normalization, XLSX sanitization. Good roxygen. Note: function name `archive_table` is already
snake_case. `table_nm` passed to `st_write` unsanitized (trusted-input caveat). Separate subsystem from
model_archive — keep separate.

## Legacy modeling (poor → refine in place)

**estimators.r** (~744 lines) — Variance estimation (`.estimate`/`.rand`/`.reg`/`.str`). No roxygen.
Mixed naming. Silently overwrites `wt_nm` (L29,34). Fragile literal-string column access (`data[,"wt_nm"]`).
`require()` in bodies; plyr `rbind.fill`. ~240 lines of commented-out dev code at the tail (L572-811).

**ols_modeling.R** (~1029 lines) — regsubsets/bootstrap/parallel OLS. **Placeholder roxygen** (47
`tbd`/"Delete and Replace"). Bug: `modk` (L286). `mod_multi` references undefined `reg_multi_obj`.
Depends on archived `bootstrap` (`bootpred`). `require()` in bodies. ~230 lines commented-out tail (L793-1027).

**knn_tools.R** (~697 lines) — yaImpute wrappers. `browser()` at L197. `data.frameQ` typo at L330.
camelCase args (mirrors yaImpute). Placeholder roxygen. ~190 lines commented-out tail (L499-693).

**yai_r2.R** — R² from yaImpute output. Good roxygen + runnable example. camelCase locals; hardcoded
`.o` observed suffix; factor columns silently NA.

**make_strata.R** — `make_strata`/`assign_strata`. Good roxygen + 3 examples. Weight handling replicates
rows (can blow up memory). Collapse branch references undefined `type_x1`/`type_x2` (L392-401) — broken,
author notes "cheap fix".

## FIA / compile

**compile_plots.R** — Plot compilation w/ optional parallelism + SQLite. **Bug**: `sqldf(...,envir=as.environment(data))`
where `data` is undefined (L250,256). `require("reshape2")` in body (L563). Nested filter/merge logic
(L198-274) hard to follow; `interaction()` key-matching is fragile. `nclus=4` default OK (documented).

**compile_trees.R** — Tree-level metrics. Magic `0.005454` (π/576 basal-area constant) uncommented (L166);
hardcoded diameter-class breaks (L193).

**fia_clean_best_cds.R** — Hardcoded subplot radius (24 ft) and EPSG codes (6318/6339) minimally commented.

**fia_make_geom.R** — Hardcoded subplot radius/offsets (FIA standard) lacking a citing comment.

## FVS

**fvs_load.R** — Downloads into the package dir (side effect); assumes `find.package` succeeds.
**fvs_make_keyfiles.R / fvs_prototype_keyfile.R / fvs_run.R** — Reasonable; minimal validation of `param_df`.
**fvs_protype_params.R** — **Misspelled** function/file name ("protype"); a `@param` lacks description.

## Utilities

The `*2` wrappers are **legitimate**, not reinvention: `aggregate2` (tidies multi-fun aggregate output),
`pairs2` (flexible pair plots), `duplicated2` (all duplicates), `gsub_vec` (vectorized multi-pattern gsub).
Several have placeholder roxygen (`gsub_vec`, `dist_vec`, `pairs2` `@return`, `sampleSystematic`,
`duplicated2` examples). `sampleSystematic` has `browser()` at L108 and camelCase name. `helper_functions.R`
holds `write_clipboard`, `bs`, `fs`, `nms_vec`.

## Package metadata

**DESCRIPTION** — placeholder maintainer email (retained by request); malformed `Imports:` (leading-comma
continuation, `plyr` listed twice); missing `Suggests:`; non-standard `License:` phrasing.
**NAMESPACE** — roxygen-generated; imports `flock`, `openxlsx`, `bootstrap` not all reflected in DESCRIPTION.
**.Rbuildignore** — only excludes `.Rproj`; currently ships `_dev/`, `_archive/`, a 960 KB root `.db`,
`fort.2`, and C++ build artifacts.
**README.md** — `install.packages("remotes)` missing quote; "install a local zip" typo; no scope paragraph.

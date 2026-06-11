# RForInvt full package review — findings and update plan (2026-06-11)

Scope: every function, roxygen example, and unit test in `R/` and `tests/testthat/` on branch
`refactor/package-review-2026-06`. Findings were produced by seven parallel deep-review passes
(NVEL, FVS, compilation/estimators, OLS modeling, model archive, kNN/strata/sampling, FIA/utilities),
several verified empirically by running the code.

Severity: **CRITICAL** = silently wrong results or crash in normal use; **MAJOR** = logic/conceptual
flaw, misleading API/docs, or fragile-by-design; **MINOR** = polish, docs, idioms.

---

## Phase 1 — Silent wrong-results bugs (fix first; these corrupt analyses without erroring)

1. **NVEL region clobber** — `NVEL_volume.R:115,408-411` and `NVEL_buck.R:114,375-378`:
   default `region = 1` is non-NA, and the formatters overwrite the user's per-tree `region`
   column whenever the scalar is non-NA. Every default call computes with Region 1. In
   `NVEL_buck` the clobber happens *before* equation lookup, so Region-1 equations are selected
   for (e.g.) Region-6 data. The packages' own examples and tests hit this.
   **Fix:** default `region = NA` (match `NVEL_voleq`/`NVEL_wtfactor`); add a value-pinning test
   (`NVEL_voleq(region=6, forest="01", spcd=202)` returns a known code; known volume to 1e-3).

2. **`yai_r2` formula is not R²** — `yai_r2.R:103`: computes `1 - var(imputed)/var(observed)`.
   Perfect imputation returns 0; constant prediction returns 1. Every value ever returned is
   meaningless. **Fix:** `1 - var(obs - imp)/var(obs)` (state the definition in `@details`);
   tests: perfect imputation → 1, constant prediction → ≤ 0.

3. **`sample_systematic` excludes the last frame unit** — `sample_systematic.R:92`:
   `idx[idx<N]` should be `idx[idx<=N]`. Unit N has ~0 inclusion probability — a real design
   bias for any survey drawn with this. **Fix:** `<=`; add inclusion-probability test.

4. **`compile_plots` filters silently discarded** — `compile_plots.R:249-269`: when `df_plot`
   is supplied, the merge uses the *unfiltered* `df_plot`/`df_tree`, so `tree_filter`/
   `plot_filter` do nothing. **Fix:** merge `df_plot_in`/`df_tree_in`; test the filtered path.

5. **`tpa`/`tph` ignore `nstems` mapping** — `compile_trees.R:176-186`: membership tested
   against `tree_nms` *values* not `names(tree_nms)`; any mapping where the column isn't
   literally named "nstems" silently drops stem counts. **Fix:** `"nstems" %in% names(tree_nms)`;
   test with `nstems = "count"` mapping.

6. **`fia_clean_best_cds` filter step never runs** — `fia_clean_best_cds.R:107 vs 128`:
   default step name `"filter_cds"` but code checks `"filter_pts"`. Documented filters are never
   applied. Also `== "HPGPS"` with NA method codes injects all-NA rows (lines 140-143), and
   dedupe keys on `PLOT` alone (not state/county) collapsing distinct plots (141-148).
   **Fix:** one canonical step name; `%in%` instead of `==`; composite dedupe key.

7. **Estimator bootstrap double-weights** — `estimators.r:139-146`: `.rand` bs branch resamples
   `prob = wt` *and* multiplies by `wt` in the statistic; also skips the weight rescaling the
   asym/svypkg branches apply, so totals differ across `var_type` for identical data.
   **Fix:** equal-probability resample (the commented line 140), rescale consistently.
   Related: `pi` vs `p_i` typo in `.pop_test` (line 536); no FPC anywhere (document or add);
   `rmse_srs` formula (line 82) dimensionally wrong.

8. **`NVEL_merch` returns log values in spec columns** — `NVEL_merch.R:87-122`: data.table
   non-equi join makes `MinDib`/`MinDbh`/`MinLen` carry the *log's* values, not the spec's,
   contradicting `@return`. **Fix:** re-join true spec rows by `(Species, Rank)` after rank
   selection (or drop those columns); test asserts spec values.

9. **`model_predict` `standardize` transform uses newdata mean/sd** — `model_predict.R:248`:
   predictions depend on the composition of the prediction batch; single-row newdata → NaN.
   **Fix:** read `center`/`scale` from the predictor spec; error if absent. Add transform tests
   (log, sqrt, standardize round-trip).

10. **`yai_cv` "rmse" is actually SD of errors** — `knn_tools.R:439-442`: errors are mean-centered
    before squaring (bias excluded); centering unweighted while sums are weighted; inconsistent
    `na.rm`. **Fix:** true weighted RMSE; report debiased SD separately if wanted.

11. **`reg_model(rank_by="rss")` ranking inverted** — `ols_modeling.R:161-163`: RSS sorted
    descending, so rank 1 = worst model. **Fix:** only rsq/adjr2 descending.

12. **`make_strata` break construction** — `make_strata.R`:
    - "eq" splits drop wrong breaks: `[-c(1,n)]` should be `[-c(1, n+1)]` (lines 177, 237, 286) —
      top stratum double-width plus a permanently empty top bin.
    - non-nested numeric-x2 branch uses `n1` where `n2` belongs (279, 286) and the upper sentinel
      is exactly max (280, 287) so future values just above max get NA strata.
    - `collapse=TRUE` (353-359) silently *drops* empty strata instead of merging (index-vs-position
      bug), leaving assignment gaps.
    - No NA handling — crashes on routine data (`quantile` errors).
    **Fix:** all four; add break-placement and slightly-above-max assignment tests.

13. **`lm_summary` LOOCV row alignment** — `ols_modeling.R:778-786`: assumes user `data` rows
    align positionally with `model$model` (silently wrong with NA-dropped rows); denominator
    uses pre-filter n. **Fix:** iterate over the fitting frame; use `nrow(ei)`.

## Phase 2 — Crash bugs (loud, but block legitimate use paths)

14. `lm_summary(resids=TRUE)` — `ols_modeling.R:807`: undefined `dfIn` (typo for `df_in`). Always crashes.
15. `pred_multi(fix_outliers=TRUE)` — `ols_modeling.R:545-548`: `se_pd` vs `se.pd` naming; broken in both `se_fit` branches.
16. `fvs_run(merge_dbs=FALSE)` with default `delete_temp_db=TRUE` — `fvs_run.R:138,177`: `unq_db` undefined outside merge block. Crashes after FVS already ran.
17. `NVEL_volume` scalar args / equation lookup — `NVEL_volume.R:199-212`: lookup passes raw `dfTL` with hard-coded names; scalar `region=`/`forest=` calls (and the documented default `dfTL`) crash; custom `*Nm` names ignored. Mirror `NVEL_buck`'s use of the formatted table + forwarded names.
18. `.formatTL2NVEL_bio` — `NVEL_biomass.R:261-268`: selects by standardized names never applied; any non-default column name crashes. Consolidate on `.formatTL2NVEL2` (see Phase 3, item 28).
19. `yai_weights(dtype="eq")` — `knn_tools.R:262`: builds a vector not an n×k frame; downstream melt gets zero weight columns. Fix: `dist_in*0 + 1/ncol(dist_in)` with proper names.
20. `impute_id(yai_id_object)` — `knn_tools.R:177/204/222`: `updateID` vs `update_id` tag drift. Standardize one name.
21. `search_text(clus=...)` — `search_text.R:97`: `clusterMap` never receives `file=files`; any parallel call fails. Also default `pattern=".r"` is an unanchored regex matching almost everything (line 71).
22. `plot_lor_qmd` no-expansion branch — `compile_plots.R:519-528`: subsets before creating `tree_weight` → undefined column; NA-weight "fix" overwrites entire rows (`tr_in[is.na(...),] = 1`).
23. `pander_mod.simex` with default `df0=NA` — `pander_mod.R:119-153`: unconditional `predict(x, newdata=df0)` crashes. Require df0 or add apparent branch.
24. `fvs_make_keyfiles` default `id="plt_id"` — `fvs_make_keyfiles.R:81`: column doesn't exist in `fvs_prototype_params()` output. Default to `"std_id"`. Also `key_nms` prefix/suffix treats literals as column names (150-155) — implement as `paste0(prefix, id_value, suffix)`.
25. `fvs_load` first-run never downloads — `fvs_load.R:48-67`: `sapply(dir, list.files)` returns a length-1 list so `length(zips)==0` never triggers; missing-zip branch then references undefined values. Use `list.files()` directly + guard.
26. `compile_trees` class check — `compile_trees.R:153`: `class(x) != "data.frame"` errors on data.table/tibble in R ≥ 4.2 (length-2 condition). Use `inherits()`. Same idiom elsewhere — sweep the package (see item 33).
27. `multi_bs` returns character statistics — `ols_modeling.R:660-689`: `unlist` of mixed list coerces all stats to character. Split `predictors` out before `unlist`. (Existing test `expect_gt` passes via coercion — tighten with `expect_type`.)

## Phase 3 — Major design/robustness (correct the API contracts)

28. **Unify the three NVEL tree-list formatters** on the `.formatTL2NVEL2` approach (preserves
    extra columns, keys by user names). Fix `*Nm` forwarding when volume/buck delegate to
    `NVEL_voleq`/`NVEL_wtfactor` (currently crash with custom names); implement the documented
    "scalar supersedes column" rule once, identically, in all six wrappers (today there are three
    different behaviors); zero-fill NAs before `.Fortran` in `NVEL_biomass` like its siblings;
    standardize all returns to data.table (or data.frame) and say so in `@return`.
29. **NVEL DLL loader** — `nvel_internal.R:27-41`: `stop()` (not warn-and-continue) on missing
    DLL; explicit non-Windows error; don't route unknown arch to the 32-bit DLL.
30. **`NVEL_biomass` has no equation lookup** — bioeq defaults to "0" → all-zero biomass with the
    function's own example. Implement the lookup or make `bioeq` required and fix the example.
31. **File locking** — `file_version.R` (read-modify-write of `_tracking.csv`) and
    `model_archive_save.R` (registry append) have no locking; concurrent writers lose rows /
    duplicate version ids. The package already imports `flock` (used in compile_plots).
    Wrap the critical sections; also stop `.update_paths` rewriting the tracking CSV on every
    read-only call (`file_version.R:291-312`).
32. **`fvs_run` parallel DB collisions** — `fvs_run.R:131-133`: `parLapplyLB` ignores the
    per-cluster-DB grouping; two FVS processes can write the same SQLite file concurrently.
    `split(fvs_runs, key_df$cluster)` and run each group serially per node. Also: quote
    `fvs_path`/keyfile paths with `shQuote` (line 112); close `con_dbi` unconditionally
    (148-167 connection leak blocks temp-DB deletion on Windows); warn before `unlink`ing an
    existing merge DB (141).
33. **Idiom sweep (package-wide):** `inherits()` instead of `class(x)=="..."`;
    `length(x)==1 && is.na(x)` instead of scalar `if(is.na(x))`; `%in%` instead of `==` where NA
    possible; `TRUE`/`FALSE` not `T`/`F`; `seq_along` not `1:length`; drop in-function
    `require()` calls in favor of namespace-qualified calls; `on.exit()` for `options(warn)`,
    `par()`, connections, and `pdf()` devices (pairs2 currently leaks all of these).
34. **`model_archive` hardening:** atomic+checked rename for the predictor-spec JSON
    (`model_archive_save.R:201-203`); warn when `vcov` is silently dropped without `system_id`
    (182-190); evaluate closed-form `formula` text in a restricted environment (math functions
    only — it is currently an arbitrary-code path from a CSV, `model_predict.R:164-176`);
    rep() scalar results to nrow for constant formulas; normalize `model_id` types before the
    uniqueness check.
35. **`archive_table` isolation holes** — `archive_table.R:370-399`: `.prepare_for_xlsx`, the
    datetime-styling block, and `file_version()` calls run outside `.safe_write`/tryCatch, so
    `stop_on_error=FALSE` doesn't hold. Move them inside.
36. **OLS modeling structural fixes:** formula construction via `gsub("y", y, form_y)`
    substitutes every literal "y" (`ols_modeling.R:377,423`) — use `reformulate()`; model-frame
    round-trips break on transformed terms (`log(x)`) in `pred_multi`/`lm_boot` (576-578,
    227-242) — use `all.vars(delete.response(terms(x)))` / original data; docs claim ".632+"
    but `bootstrap::bootpred` computes plain .632 (fix docs); extract one `fit_stats(y, yhat, p)`
    helper to unify the three divergent RMSE/R² conventions (lm_boot vs lm_summary vs pander_mod);
    extract one parallel-runner helper for the four hand-copied cluster blocks.
37. **`compile_plots`/`compile_trees` contracts:** define one convention for whether plot-compute
    functions carry id columns (`.compile_1plot` currently duplicates them for `spp_y_plot`);
    make `plot_lor_qmd` empty/non-empty branches return the same schema; replace `rep(ht, expansion)`
    weighted quantiles (silently truncates fractional TPA) with a real weighted-quantile;
    `spp_y_plot` should use its documented `spp_y` arg (not undocumented `domspp_y`); merge keys
    should respect `plot_nms[["plot_ids"]]`; demote the four unconditional `warning()` calls;
    use `0.005454154` in `ba_ft`.
38. **kNN tools:** forward `...` from `yai_cv` to `yai()` (currently discarded); fix `yy0.`
    column-name prefix and harmonize the small-data branch schema; guard `newtargets_id` against
    reference/target id collisions (yaImpute rowname footgun); fix `".o$"` regex escape; remove
    dead `cols_knn_id`/`cols_knn_wt` params or implement them.
39. **`fia_make_geom`:** add a `crs` parameter (outputs currently have NA CRS, undocumented);
    validate named `offx`/`offy`; fix `"PLOT POLYGON"` vs `"PLOT Polygon"` key and the
    unconditional "PLOT POINT" return in `fia_clean_best_cds` (209-223); make `nms_cds` actually
    drive the wide-to-long step or delete it.
40. **`dist_vec`:** `apply(df, 1, ...)` coerces rows to character (precision loss); replace with a
    vectorized `outer()` computation.
41. **`helper_functions.R`:** Windows-only clipboard functions need platform guards and proper
    per-function roxygen (currently undocumented exports with clipboard-reading examples).

## Phase 4 — Documentation, examples, and minor cleanup

42. Roxygen/doc sync sweep — worst offenders: knn_tools (params for a previous API generation),
    NVEL family (`dll_32` described as "64bit dll", phantom `nclus`/`dll_func_bioeq` params,
    undocumented `spcd`), fvs family (broken `@seealso` to `fvs_keyfile_prototype`/
    `fvs_param_prototype`/`fvs_load_rfvs`; `fvs_prototype_params` description copy-pasted from
    make_keyfiles; example uses nonexistent `cn` column), pairs2/dist_vec/file_stamp
    (`<Delete and Replace>` and truncated placeholders), `model_archive_load` "unquoted filter"
    wording, `model_archive_save` reference to a predictor-spec schema the spec doc doesn't contain
    (add the schema section), `suffix_predict` documented-but-ignored.
43. Examples: wrap DLL/exe/cluster-dependent examples in `\donttest{}`/`\dontrun{}`
    (NVEL_buck examples currently run a 4-core cluster over 10k trees under R CMD check;
    fvs examples write to `c:/temp` and call FVSca.exe; file_version/file_stamp examples write
    to `c:/temp` — use `tempdir()`); fix `file_stamp` examples passing the stamp as `path`.
44. Restore `#' @export` on `assign_strata` (currently `##'` — next roxygenize drops the export).
45. Delete shipped dev debris: top-level `if(F)`/`if(T)` blocks (fia_clean_best_cds:245,
    fia_make_geom:170, aggregate2:120, dist_vec:84, file_version:316, make_strata:448-526),
    dead code after `return()` (`rm(list=ls());gc()`), stale comments (test files referencing
    baseline bugs that no longer exist; NVEL_buck "clusterEvalQ above"), unused variables
    (`files_zip`, `k_mod`, `create_db`, `out_files`).
46. Minor numeric/edge fixes: `dbcl` lower-edge NA (document or `include.lowest`), `gsub_vec`
    length validation, `.normalize_colnames` second-order collision, archive_table "json" mode
    emits R syntax not JSON, `duplicated2` `fromLast` collision, `sample_systematic` n validation,
    `pander_mod` 'Resis./Resid.' typo + collapse inconsistency, `nvel_archive` `"NA"`-string guard.

## Phase 0 (do alongside Phase 1) — Test hardening

The dominant test weakness is *structural smoke testing*: most tests assert classes/columns/counts,
so nearly every bug above passes the current suite. Specifically add:

- **Value-pinning tests** for: NVEL voleq codes + volumes (known fixtures), estimator SE/total vs
  hand-computed values (not just mean-unbiasedness), `reg_model` selecting the known-correct
  subset on the existing `make_model_df` fixture, weighted RMSE/R² hand checks, `make_strata`
  break placement, basal area constant, fia_make_geom subplot offsets (subplot 2 at (x, y+120)),
  NVEL_merch returned spec values, log-volume sums ≈ tree volume in NVEL_buck.
- **The untested branches where bugs live:** custom `*Nm` mappings (NVEL + compile),
  scalar-override paths, `var_type="bs"`, `dtype="eq"`, `fix_outliers`, `resids=TRUE`,
  `rank_by="rss"`, `merge_dbs=FALSE`, `key_nms`, cluster partitioning with fake DBs (no FVS
  needed), `compile_plots` with `df_plot` supplied + filters, multi-plot `spp_y_plot`,
  transforms in `model_predict`, vcov and predictor-spec round-trips, `search_text` (no test
  file at all), inclusion probability for `sample_systematic`.
- Delete stale "FAILS at baseline" comments in tests that no longer describe the code.

---

## Package structure recommendations

1. **DESCRIPTION issues:** placeholder maintainer email (`someone@somewhere.com`); `jsonlite` is
   used in NAMESPACE (`importFrom(jsonlite,...)`) but **not declared in Imports** — R CMD check
   error; `LazyData: true` with no `data/` directory → check NOTE; several Imports appear unused
   or barely used (`RODBC`, `odbc`, `caret`, `MLmetrics`, `mltools`, `randomForest`, `flextable`,
   `survey`, `stringr`, `tools`, `sqldf`) — audit with `R CMD check` and demote to Suggests or drop.
   Heavy modeling deps (`randomForest`, `caret`, `xgboost`-adjacent) belong in Suggests with
   `requireNamespace()` guards since they're only touched by archive adapters.
2. **Replace blanket `@import`** (dplyr, plyr, sf, sqldf, yaImpute, ...) with targeted
   `@importFrom`; plyr+dplyr together invite masking bugs — pick one (or data.table, which the
   newer code already prefers).
3. **Consistent file naming:** `estimators.r` and `test-archive_table.r` → `.R`; consider
   one-file-per-export or per-family (current mix is fine, but `ols_modeling.R` at 9 exports +
   800 lines would benefit from splitting into `reg_select.R`, `boot_validate.R`, `multi_fit.R`).
4. **Naming conventions:** the API mixes `NVEL_volume`, `fvs_run`, `compile_trees`, `yai_r2`,
   `pred_multi` — keep family prefixes (good) but settle casing (all lower snake with family
   prefix is the de facto winner; `NVEL_*` can stay as a documented exception).
5. **Internal helpers** (`.estimate`, `.rand`, `.reg`, `.str`) are undocumented-unexported but
   load-bearing — either export with docs (they're the package's estimation engine) or wrap with
   an exported `estimate()` front end.
6. **CI + checks:** add GitHub Actions `R-CMD-check` (Windows + ubuntu; the NVEL/FVS suites
   already skip cleanly via the helper), `lintr`/`styler` config, and a `pkgdown` site. The
   roxygen2 version pinned in DESCRIPTION (`Config/roxygen2/version: 8.0.0`) doesn't exist —
   current roxygen2 is 7.x; fix.
7. **Repository hygiene:** stray `fort.2` (FVS Fortran scratch output) at package root —
   delete + add to `.gitignore`; ensure `_archive/`, `_dev/`, `.Rhistory` are in `.Rbuildignore`;
   placeholder author emails in roxygen headers.
8. **Windows-only surface:** NVEL DLLs, clipboard helpers, default `c:/temp` paths. Add explicit
   platform guards with clear errors, and move all defaults to `tempdir()`-based paths.
9. **Vignettes:** the README workflows (FVS end-to-end, NVEL volume) should be vignettes that
   build under check; `inst/examples/model_archive_Example.Rmd` is a vignette in hiding.

## Missing functionality worth adding

Ordered roughly by value-to-effort for an industrial timberland inventory workflow:

1. **FIA NSVB equations** — FIA replaced regional volume/biomass with the National Scale Volume
   and Biomass (NSVB) framework in 2023+. The model-archive engine is the natural home: ship NSVB
   coefficient tables as a registry the way NBEL is shipped. NVEL is effectively frozen; this is
   the forward path.
2. **Log-bias correction helpers** for log-log allometries (Baskerville and Snowdon ratio
   corrections) — the OLS tooling fits models but offers no back-transformation support, which is
   the single most common conceptual error in forestry regression use.
3. **Variable-radius (prism) plot support** — `compile_*` assumes a TPA/expansion column exists.
   Add a BAF-based expansion helper (`tpa = baf / (0.005454154 * dbh^2)`) and a plot-design
   argument (fixed-area vs variable-radius, nested subplots with breakpoint diameters).
4. **Metric parity** — `tph` exists but there is no `ba_m2` (0.00007854·DBH²), metric QMD, or a
   units argument; pick one pattern (paired functions or `units=` argument) and apply across
   compile/estimators.
5. **Stand/stock table generation** — `dbcl_*` builds the pieces; a `stand_table()` wrapper
   producing TPA/BA/volume by species × diameter class (the report every forester asks for first)
   is missing.
6. **Design-based estimation completeness:** FPC, ratio and post-stratified estimators, two-stage
   designs, and a documented exported front end (`estimate()`); the `survey` package is already
   an Import — expose the svydesign path properly.
7. **FVS output tooling** — `fvs_run` merges output DBs but nothing reads/summarizes them. Add
   `fvs_read_output()` (FVS_Summary2, FVS_Carbon, FVS_TreeList) and a tidy summary across
   stands/cycles. Also a keyfile validator (column-position checks) given how positional the
   format is.
8. **Height-diameter and taper models** — ht-dbh fitting (Chapman-Richards / Curtis with mixed
   effects by stand) for filling missing heights before NVEL calls, and a taper-based
   merchandiser so bucking isn't NVEL-DLL-bound.
9. **Tree-record QA/validation** — `check_trees()`: dbh/ht range and ratio outliers, species-code
   validation against a reference table, duplicate tree ids, dead/live consistency. Every
   compilation starts with this; the package starts after it.
10. **Sample-size / inventory planning helpers** — CV-based n calculators per stratum, cost-
    constrained allocation (Neyman) to complement `make_strata`.
11. **Spatial sampling** — `sample_systematic` is index-based; add an sf-aware systematic/hex
    grid sampler over polygons (ties into the existing sf usage in fia_make_geom).

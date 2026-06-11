# Plan: FIA-specific wrapper functions for RForInvt

Date: 2026-06-11
Status: Phase 1 IMPLEMENTED; Phases 2-5 PROPOSED

## Phase 1 — implemented 2026-06-11

- `R/fia_data.R`: `fia_db()`, `fia_evalid()`, `fia_plots()`, `fia_trees()`
  (+ `print.fia_db`, internal source abstraction supporting SQLite path,
  `fia_db` handle, open DBI connection, and in-memory table lists).
- `inst/extdata/FIADB_demo.db`: synthetic, schema-faithful FIADB SQLite
  (STATECD 53 / Washington; King/Lewis/Grays Harbor counties; PNW species;
  EVALIDs 531901/532101; PLOT/COND/TREE/POP_* + REF_SPECIES; 2021 plots
  remeasure 2019 plots for change work). Generator:
  `_dev/make_fia_demo_db.R` (deterministic, re-runnable).
- `tests/testthat/test-fia_data.R`: 35 checks, all passing. Full suite
  494 pass / 0 fail (pre-existing NVEL-DLL warnings unchanged).
- Note: fixture is SYNTHETIC. Decision on whether to bundle a real
  trimmed-state extract (Open Question 1) is deferred to Phase 3, where
  EVALIDator validation requires real data.

## Goal

A family of `fia_*` functions that wrap `compile_trees()`, `compile_plots()`, the
`NVEL_*` functions, and the `fvs_*` functions so that a user can go from a raw
FIADB SQLite download (DataMart) to design-based, EVALIDator-compatible estimates
in a few calls. Examples/vignette demonstrate county-level estimates for
Washington state (STATECD = 53): single year, multiple years, change, and
components (volume by species and diameter class).

## Design principles

1. **EVALID is the spine.** All estimation flows through an FIA evaluation
   (POP_EVAL / POP_PLOT_STRATUM_ASSGN / POP_STRATUM). This is what makes
   estimates match EVALIDator and avoids the classic mistakes (using INVYR
   directly, dropping nonforest/empty plots, ignoring adjustment factors).
2. **Wrappers configure, they don't reimplement.** `fia_compile_trees()` is
   `compile_trees()` with FIADB column maps and an FIA-aware `fns_compute`
   list; `fia_compile_plots()` is `compile_plots()` with FIA plot ids and
   zero-filling. Custom behavior stays available through the same
   `fns_compute`/`...` mechanics.
3. **Volume is pluggable.** FIADB ships computed volumes (VOLCFNET, VOLCSNET,
   DRYBIO_*). NVEL recomputation is optional, for custom merch specs or
   consistency with non-FIA inventories. `vol_source = c("fiadb","nvel")`.
4. **Estimation follows Bechtold & Patterson (2005).** Post-stratified
   estimation with per-stratum adjustment factors (SUBP/MICR/MACR), EXPNS
   expansion, and the published variance estimators. The `survey` package
   (already in Imports) is an implementation option, but direct B&P formulas
   are preferred so totals/SEs reproduce EVALIDator.

## Function inventory

### Layer 1 — data access (FIADB SQLite -> analysis-ready tables)

```r
# Open/read a FIADB SQLite (DataMart state download or national subset).
# Returns a lightweight handle (path + DBI con factory), not a full read.
fia_db(path)

# List / select evaluations. eval_type follows POP_EVAL_TYP:
# "EXPCURR" (area), "EXPVOL" (volume), "EXPGROW"/"EXPREMV"/"EXPMORT" (GRM).
# years can be a single year, a vector, or NULL (= all available).
fia_evalid(db, statecd = 53, years = NULL, eval_type = "EXPVOL", most_recent = FALSE)

# Plot/condition frame for one or more EVALIDs:
# PLOT x COND x POP_PLOT_STRATUM_ASSGN x POP_STRATUM (+ estimation unit).
# Carries CONDPROP_UNADJ, PROP_BASIS, ADJ_FACTOR_*, EXPNS, STRATUM_CN,
# ESTN_UNIT, COUNTYCD. One row per condition; includes nonforest conditions
# so area estimation and zero-filling are possible downstream.
fia_plots(db, evalid, cond_filter = NA, vars_keep = NULL)

# Tree frame for the same EVALIDs: TREE joined to its plot/cond record.
# Computes the design expansion per tree:
#   TPA_EXP = TPA_UNADJ * ADJ_FACTOR_(SUBP|MICR|MACR chosen by tree size/PROP_BASIS)
# Output columns are already named to satisfy compile_trees defaults
# (DIA, HT, SPCD, TPA_EXP, PLT_CN, CONDID, STATUSCD, ...).
fia_trees(db, evalid, tree_filter = "STATUSCD == 1", vars_keep = NULL)
```

Notes:
- `fia_plots`/`fia_trees` accept either a `fia_db` handle or pre-loaded
  data.frames (so users with NIMS/Oracle extracts can bypass SQLite).
- GRM support: `fia_trees(..., grm = TRUE)` additionally joins
  TREE_GRM_COMPONENT / TREE_GRM_MIDPT for growth-removal-mortality work.

### Layer 2 — compilation wrappers (per-tree, then per-plot)

```r
# compile_trees() preconfigured for FIADB. Adds per-acre columns
# (expansion already in TPA_EXP), diameter classes, and optional NVEL volume.
# fns_compute defaults: list(ba_ft, dbcl, fia_vol) where fia_vol respects
# vol_source. dbcl breaks default to FIA 2-inch classes (configurable).
fia_compile_trees(df_tree,
                  vol_source = c("fiadb", "nvel"),
                  db_breaks  = c(seq(1, 29, 2), 999),
                  fns_compute = NULL,   # NULL = FIA default list
                  ...)

# compile_trees-compatible compute functions (exported so users can mix
# them into any compile_trees call):
fia_vol(x, tree_nms, vol_source, ...)      # VOLCFNET etc. or NVEL_volume()
fia_nvel_volume(x, tree_nms, region = NULL, forest = NULL, voleq = NULL, ...)
fia_nvel_biomass(x, tree_nms, ...)         # wraps NVEL_biomass / wtfactors

# compile_plots() preconfigured for FIA:
# - plot_ids = "PLT_CN" (single key; EVALID context keeps it unique)
# - compiles at condition level first (PLT_CN x CONDID), then rolls up to
#   plot using CONDPROP_UNADJ so partially forested plots are correct
# - zero-fills plots with no qualifying trees (forested but empty, and
#   nonforest conditions) -- required for unbiased totals
# - sum_nms default: per-acre versions of ba, volume, biomass, tpa columns
#   created by fia_compile_trees (auto-detected by prefix)
fia_compile_plots(df_tree, df_plot,
                  level = c("plot", "condition"),
                  fns_compute = NULL,   # NULL = list(plot_wtsum, plot_lor_qmd)
                  sum_nms = NULL,       # NULL = auto-detect compiled columns
                  ...)
```

### Layer 3 — estimation (county / unit / state)

```r
# Post-stratified estimates for compiled plot values.
# by: grouping columns from the plot frame (e.g. "COUNTYCD", or
#     c("COUNTYCD","SPCD-derived column set")).
# vars: compiled per-acre columns to estimate (e.g. "vol_cf_ac",
#       "ba_ft", or a prefix pattern like "^vol_cf_ac_SPCD_").
# type: "total" (EXPNS-weighted), "per_acre" (ratio-of-means over the
#       relevant area), "area" (condition proportions).
# Returns: estimate, variance, SE, n plots, n nonzero, per `by` group.
fia_estimate(df_plot_compiled, df_pop,        # df_pop from fia_plots()
             vars, by = "COUNTYCD",
             type = c("per_acre", "total", "area"),
             var_method = "BP2005")

# Multiple years: vectorized over evaluations; returns long table with
# EVALID/year column so users get a trend frame in one call.
fia_estimate_annual(db, statecd, years, vars, by = "COUNTYCD", ...)

# Change estimation, two modes:
# mode = "grm":    EXPGROW/EXPREMV/EXPMORT evaluations + TREE_GRM_COMPONENT,
#                  net growth / removals / mortality per acre and totals.
# mode = "remeas": paired-plot change -- match PLT_CN to PREV_PLT_CN across
#                  two EXPVOL evaluations, compile both, difference at the
#                  plot level, then post-stratified estimate of the change
#                  (correct paired variance, not difference of independent SEs).
fia_estimate_change(db, statecd, evalid_t1, evalid_t2,
                    mode = c("remeas", "grm"),
                    vars, by = "COUNTYCD", ...)
```

Component estimates (species x diameter class) need no special estimator:
`fia_compile_trees(..., fns_compute incl. dbcl_y/spp_y/dbcl_spp_y)` produces
wide per-acre columns per component; `fia_estimate(vars = "^vol_cf_ac_")`
estimates each column. A small helper tidies the wide result back to long:

```r
fia_components_long(df_est, split = c("var", "SPCD", "dbcl"))
```

### Layer 4 — FVS bridge (projection-based estimation)

```r
# Build an FVS-ready SQLite for a set of FIA plots. DataMart already ships
# FVS_STANDINIT_PLOT / FVS_TREEINIT_PLOT; this filters them to the chosen
# EVALID/plots and writes a project db (or builds the tables from
# PLOT/TREE/COND if the FVS tables are absent).
fia_fvs_input(db, evalid, dir_out, variant = "PN", ...)

# Run FVS over those plots: thin wrapper chaining fvs_prototype_params() ->
# fvs_prototype_keyfile() -> fvs_make_keyfiles() -> fvs_run() with FIA
# stand ids (STAND_CN = PLT_CN), returns path to merged output db.
fia_fvs_run(db_fvs_in, fvs_path, numcycle = 1, timeint = 10, cluster = NA, ...)

# Read FVS output (FVS_TreeList / FVS_Summary), map columns back onto the
# fia_compile_trees() conventions so projected tree lists flow through the
# same compile + estimate pipeline (future volumes by county, etc.).
fia_fvs_compile(db_fvs_out, year, ...)
```

This makes "current estimate" vs "projected estimate" symmetrical: both end
in `fia_compile_plots()` + `fia_estimate()`.

## Example / vignette plan

### Example data

- Raw-table examples need PLOT/COND/TREE/POP_* tables; the bundled
  `FIADB_RI.db` has only FVS_* tables. Add `FIADB_RI_mini.db`: one recent RI
  EXPVOL + EXPCURR evaluation (and the prior one, for change), columns
  trimmed to what the functions touch, target < 5 MB. Rhode Island keeps it
  tiny; the API is identical for WA. Used by roxygen `\donttest{}` examples
  and tests.
- The WA walkthrough lives in a vignette/Rmd with `eval = FALSE` chunks
  (WA SQLite from DataMart is ~1 GB): `inst/examples/FIA_WA_Example.Rmd`.

### Vignette outline: "County-level estimation in Washington"

1. **Setup** — download pointer for `SQLite_FIADB_WA.db`; `fia_db()`;
   `fia_evalid(statecd = 53, eval_type = "EXPVOL", most_recent = TRUE)`.
2. **Single year** — `fia_plots()` + `fia_trees()` ->
   `fia_compile_trees(vol_source = "fiadb")` -> `fia_compile_plots()` ->
   `fia_estimate(vars = c("vol_cf_ac","ba_ft_ac"), by = "COUNTYCD",
   type = c("per_acre","total"))`. Map of county totals with `sf`
   (ties in `fia_make_geom`/coordinates story). Cross-check one county
   against EVALIDator and show the comparison.
3. **Multiple years** — `fia_estimate_annual(years = 2015:2021)`; trend
   plot of cubic volume per acre by county/panel; note on annual panels
   vs moving-average evaluations.
4. **Change** — `fia_estimate_change(mode = "remeas")` between the two most
   recent EXPVOL evaluations; net change in vol/ac by county with SEs;
   short section contrasting with `mode = "grm"` components
   (growth/removals/mortality).
5. **Components** — recompile with
   `fns_compute = list(ba_ft, dbcl, fia_vol, dbcl_y, spp_y, dbcl_spp_y)`,
   `vars_group = "vol_cf_ac"`; estimate every `vol_cf_ac_SPCD_*_dbcl_*`
   column by county; `fia_components_long()`; stacked-bar volume by species
   and diameter class for a chosen county.
6. **(Optional) NVEL + FVS** — same pipeline with `vol_source = "nvel"`
   (custom merch spec via NVEL_merch), and a 1-cycle FVS projection with
   `fia_fvs_*` ending in projected county volume.

## Implementation phases

| Phase | Deliverable | Depends on |
|---|---|---|
| 1 ✅ | `fia_db`, `fia_evalid`, `fia_plots`, `fia_trees` + `FIADB_demo.db` + tests | — |
| 2 | `fia_vol`, `fia_nvel_volume`, `fia_nvel_biomass`, `fia_compile_trees`, `fia_compile_plots` | 1 |
| 3 | `fia_estimate`, `fia_components_long`, `fia_estimate_annual`, `fia_estimate_change` + EVALIDator validation tests | 2 |
| 4 | `fia_fvs_input`, `fia_fvs_run`, `fia_fvs_compile` | 1 |
| 5 | `FIA_WA_Example.Rmd` vignette + README section | 3, 4 |

Each phase is independently mergeable. Phase 3 acceptance test: per-acre and
total net cubic volume for 2+ RI counties match EVALIDator within rounding;
WA spot-check documented in the vignette.

## Correctness pitfalls the wrappers must own (so users can't get them wrong)

- Plot selection by EVALID, never by INVYR alone.
- Tree expansion = TPA_UNADJ x the stratum adjustment factor chosen by
  PROP_BASIS / tree diameter (micro/sub/macroplot), not TPA_UNADJ alone.
- Zero-filling: every plot in the evaluation contributes to estimates,
  including forested-but-empty and nonforest plots.
- Condition handling: compile per condition, roll up by CONDPROP_UNADJ;
  per-acre ratios use forest-area denominators, not plot counts.
- Paired-plot change variance (covariance term), not difference of SEs.
- WOODLAND species / DIA measured at root collar (DIAHTCD) — at minimum
  flagged, ideally handled in `fia_vol`.

## Non-goals (for now)

- National-scale performance tuning (state SQLite scale is the target;
  `compile_plots` parallel/sqlite machinery already covers throughput).
- Small-area estimation / model-assisted estimators (the existing
  `make_strata`/`ols_modeling`/knn tools remain the route; a future
  `fia_*` model-assisted layer could follow the same pattern).
- Reimplementing rFIA; this stays integrated with RForInvt's compile +
  NVEL + FVS toolchain, which is the differentiator.

## Open questions

1. Bundle `FIADB_RI_mini.db` (~3-5 MB) in the package, or host it and
   download on demand in examples? (Plan assumes bundling.)
2. `fia_estimate` backend: direct B&P 2005 formulas (transparent, matches
   EVALIDator) vs `survey::svydesign` (more estimator options). Plan
   assumes direct formulas with a `var_method` switch left open.
3. Default diameter classes: FIA 2-inch classes vs the package's existing
   4-inch `db_breaks` default — plan assumes 2-inch for `fia_*` wrappers.

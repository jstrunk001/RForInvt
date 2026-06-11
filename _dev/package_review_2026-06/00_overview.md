# RForInvt — Package Review (2026-06)

Senior-level code-quality audit and refinement of `RForInvt` v0.3.0. This folder documents the
investigation; the actual refinement happens in `R/`, `man/`, `tests/`, and package metadata.
These docs are **not shipped** (excluded via `.Rbuildignore`).

## Documents in this folder

| File | Purpose |
|---|---|
| `00_overview.md` | This file — executive summary, package map, adopted standard. |
| `01_findings_by_file.md` | Per-file findings with `file:line` citations. |
| `02_conventions.md` | The binding style guide enforced by the refinement. |
| `03_refinement_plan.md` | The phased task list, kept as a living checklist. |
| `04_bugs_verified.md` | Verified runtime-bug register with fix status. |
| `05_naming_inventory.md` | Exported-name + argument inventory with old→new mapping. |

## Executive summary

`RForInvt` is **functionally competent but inconsistent**. It was built by a capable-but-novice R
coder and shows clear intent (a roxygen template, a referenced style guide, revision history in
headers) but uneven execution. The package is at a transition point: newer subsystems
(`model_archive_*` Jun 2025, `archive_table` Nov 2025, the data.table-native NVEL `buck`/`merch`) are
well-engineered, while older files (`estimators.r`, `ols_modeling.R`, `knn_tools.R`) carry placeholder
docs, deprecated dependencies, and live bugs.

The refinement standardizes on **snake_case**, completes documentation, fixes verified bugs, removes
duplication in the NVEL wrappers, and cleans the build — **without** making the code abstract or clever.
Readability and parsimony are the priority.

## Package map (subsystem health)

| Subsystem | Files | Health | Notes |
|---|---|---|---|
| **NVEL** (Fortran DLL wrappers) | `NVEL_volume/buck/merch/voleq/biomass/wtfactor.R` | Mixed | `buck`/`merch` modern (data.table); ~31% duplicated DLL/format/Fortran boilerplate; `biomass` has a live bug + broken docs. |
| **Model archive** | `model_archive_internal/list/load/save.R`, `model_predict.R` | Good | Clean internal design, atomic writes, per-class dispatch, real docs. Minor duplication to extract. |
| **Multi-format archive** | `archive_table.R` | Good | Robust per-format error isolation; well documented. |
| **FIA / compile** | `compile_plots.R`, `compile_trees.R`, `fia_clean_best_cds.R`, `fia_make_geom.R` | Fair | Solid algorithms; magic numbers uncommented; a live `sqldf` env bug; `require()` in a body. |
| **FVS** | `fvs_load/make_keyfiles/prototype_keyfile/protype_params/run.R` | Fair | `fvs_protype_params` is a misspelling; incomplete params. |
| **Legacy modeling** | `estimators.r`, `ols_modeling.R`, `knn_tools.R`, `yai_r2.R`, `make_strata.R` | Poor→Fair | Placeholder roxygen, deprecated `bootstrap` dep, `browser()`/typos, large commented-out tails. |
| **Utilities** | `aggregate2/pairs2/duplicated2/gsub_vec/dist_vec/file_*/search_text/helper_functions.R` | Fair | The `*2` wrappers are legitimate, not reinvention; docs are placeholder in places. |

## Adopted standard (binding)

- **snake_case** for functions, arguments, and internal variables (keep `NVEL_` domain prefix; S3 `generic.class`).
- Every exported function: complete roxygen per `inst/templates/roxy_header_v1.4.r`, runnable `@examples`.
- Examples use tiny simulated data or the shipped `inst/extdata/FIADB_RI.db`; DLL/long calls in `\donttest{}`.
- No `browser()`, no `require()` in bodies, no commented-out code left in `R/`.
- **The placeholder maintainer/author email (`someone@somewhere.com`) is intentionally retained** — no real address is inserted.

## Decisions (confirmed with maintainer)

1. **Break freely** on naming (rename exported names/args; fix the `fvs_protype_params` typo).
2. **Refine legacy in place** (bring `estimators.r`/`ols_modeling.R`/`knn_tools.R` to standard).
3. Document the investigation here, under `_dev/`.

## Deferred / flagged (not auto-changed)

- `bootstrap` package (`bootpred`, used by `ols_modeling.R`) is archived on CRAN. Flagged; a `boot`
  replacement is proposed but only swapped if it is a clean drop-in.
- `archive_table` and `model_archive_*` remain **separate** subsystems (different jobs); no merge.

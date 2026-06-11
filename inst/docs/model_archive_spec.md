# Model Archive — Registry Schema & Conventions

The model archive is a registry CSV (default `models.csv`) under an archive
root, plus optional sidecar files (`fits/`, `vcov/`, `predictor_specs/`). Each
row is one model. The shipped archive lives in this package's installed
`extdata/`; writable archives live in any directory passed as `archive_dir`.

Functions: `model_archive_save()`, `model_archive_load()`,
`model_archive_list()`, `model_predict()`, and `nvel_volume_register()`.

## Dispatch — the `model_class` column

`model_predict()` dispatches per row on `model_class`:

| `model_class` | prediction mechanism |
|---|---|
| `closed_form` (or blank) | substitute `b0..b8` into `formula`, eval against renamed data |
| `nvel` | DLL-backed `NVEL_volume()` call from stored selectors (no fit object) |
| `nlme`,`lme4`,`randomForest`,`xgboost`,`systemfit`,`lavaan` | load `fit_object_path`, class-specific `predict()` adapter |

## Core columns

Identity/provenance: `model_id` (unique), `system_id`, `model_class`,
`response`, `spp`, `region_code`, `region`, `data`, `year`, `authors`,
`compiler`, `notes`, `status`, `data_ref`.

Closed-form: `formula` (R expression string in `b0..b8` + canonical predictors),
`b0..b8`.

Fit statistics: `n`, `r2_pd`, `syx_pd`, `r2_proj`, `syx_proj`.

Free slots: `x1..x5`.

Sidecars/provenance: `predictor_spec_path`, `fit_object_path`, `vcov_path`,
`serializer`, `pkg`, `pkg_version`, `r_version`, `lightened`.

NBEL/NVEL extensions: `comp_token`, `bias_flag`, `ht_unit`, `source_id`,
`voleq`, `spcd`, `forest`, `district`, `nvel_out`.

The schema is **additive**: `model_archive_save()` unions columns on append and
fills missing ones with `NA`, so new columns never break old registries.

## Canonical predictors

`model_predict(nms_dat_predict = c(...))` renames input columns to canonical
names before evaluation. The shipped models use `dbh` (inches) and `ht` (feet).
Some NBEL forms also reference `cr` (crown ratio), `cl` (crown length, ft),
`topd` (top diameter), or `p` (wood density) — these are not in the shipped
curated suite (which is restricted to `dbh`/`ht`-only forms).

## NBEL biomass (`closed_form`) coefficient convention

Shipped biomass models are USFS National Biomass Estimator Library equations
translated from `inst/NVEL/BiomassEqns.db` (`BM_EqForms` + `BM_EqCoefs`) by
`data-raw/build_nbel_archive.R`, using the hand-curated form→R templates in
`inst/docs/nbel_form_templates.csv`. The `formula` is shared across all rows of
a given equation form; per-equation values live in the `b*` columns:

| coefficient | meaning |
|---|---|
| `b0..b4` | published equation coefficients `a,b,c,d,e` — **verbatim** |
| `b5` | diameter unit factor (canonical inches → model unit; IN=1, CM=2.54) |
| `b6` | height unit factor (canonical feet → model unit; FT=1, M=0.3048) |
| `b7` | biomass unit factor (model unit → pounds; LB=1, KG=2.20462) |
| `b8` | log back-transform bias factor (1 unless genuinely corrected, > 0) |

Keeping `b0..b4` byte-faithful to the published coefficients (with units and
bias as separate, visible factors) makes each row auditable against its source
equation. Log/log10/log100 forms bake the back-transform (`exp()`/`10^`/`100^`)
and the `* b8` term into the shared `formula`; identity-scale forms omit `b8`.

Predicted output is **biomass in pounds** for the component named by `response`.

### Component (`response`) naming

`response = paste0("bm_", tolower(comp_token))`, where `comp_token` is decoded
from characters 7–9 of the `BEQ` code against `BM_comp` (e.g. `STW` = stem wood,
`FOT` = foliage total, `AGT` = whole tree aboveground, `BRL` = live branches).
This keeps multiple components of the same species distinct and filterable.

### Curation

The shipped suite (~280 models) covers common PNW (`region_code = "pnw"`: WA, OR,
CA, ID — Douglas-fir, western hemlock, ponderosa/lodgepole pine, grand fir,
western redcedar, red alder, Sitka spruce) and Southeast (`region_code = "se"`:
loblolly/slash/shortleaf pine, sweetgum, oaks, yellow-poplar) species. Only
algebraic forms requiring `dbh`/`ht` are included; FIA named routines and
ratio/segmented/conditional forms are excluded. A build-time plausibility guard
drops equations that produce non-physical biomass across a small tree grid.

## NVEL volume (`nvel`)

NVEL volume equations are Fortran profile models (not formulas). An `nvel` row
stores `spcd`, `region`, `forest`, `district`, an optional forced `voleq`, and
`nvel_out` (the `NVEL_volume()` output column to return, default `"TCFV"` =
total cubic-foot volume). Register with `nvel_volume_register()`. When `voleq`
is `NA`, `NVEL_volume()` selects the equation from region/forest/district/species
at predict time. Predictions require the bundled `vollib` DLL.

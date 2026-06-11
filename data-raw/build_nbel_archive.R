# Revision history:
# 2026-06-10 — JLS — initial NBEL biomass -> model-archive ingest/curation.
#
# Purpose
# -------
# Translate a curated suite (~300) of the most common USFS National Biomass
# Estimator Library (NBEL) closed-form equations into model-archive registry
# rows and write the shipped registry inst/extdata/models.csv.
#
# This is a DATA-GENERATION script, not package runtime. It is excluded from
# the build (see .Rbuildignore). Re-run it whenever BiomassEqns.db or the form
# templates change:
#
#     Rscript data-raw/build_nbel_archive.R
#
# Source of equations : inst/NVEL/BiomassEqns.db  (BM_EqCoefs + BM_EqForms)
# Form -> R formula    : inst/extdata/nbel_form_templates.csv (hand-curated)
# Output registry      : inst/extdata/models.csv
#
# Coefficient / factor convention written into each closed_form row:
#   b0..b4 = published a,b,c,d,e  (verbatim — never adjusted)
#   b5     = diameter unit factor   (canonical inches -> model unit)
#   b6     = height   unit factor   (canonical feet   -> model unit)
#   b7     = biomass  unit factor   (model unit -> pounds)
#   b8     = log back-transform bias factor (1 when none)
# The shared per-form `formula` string references b0..b8 + canonical predictors
# (dbh, ht) so the stored coefficients stay faithful to the published equation.

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(data.table)
})

# ---- paths ----------------------------------------------------------------
# Run from the package root (so inst/ resolves). Allow override via env var.
pkg_root = Sys.getenv("RFORINVT_ROOT", unset = getwd())
if (!dir.exists(file.path(pkg_root, "inst"))) {
  stop("Run from the RForInvt package root (could not find inst/ under '", pkg_root,
       "'); or set RFORINVT_ROOT.")
}

db_path        = file.path(pkg_root, "inst", "NVEL", "BiomassEqns.db")
templates_path = file.path(pkg_root, "inst", "docs", "nbel_form_templates.csv")
registry_path  = file.path(pkg_root, "inst", "extdata", "models.csv")

stopifnot(file.exists(db_path), file.exists(templates_path))

# ---- curation knobs -------------------------------------------------------
# Target species spanning the PNW (WA/OR/CA/ID) and the Southeast, keyed by
# FIA species code, with the region bucket recorded in region_code.
species_pnw = c(`202` = "Douglas-fir", `263` = "western hemlock",
                `122` = "ponderosa pine", `17` = "grand fir",
                `242` = "western redcedar", `351` = "red alder",
                `98`  = "Sitka spruce", `108` = "lodgepole pine")
species_se  = c(`131` = "loblolly pine", `111` = "slash pine",
                `110` = "shortleaf pine", `121` = "longleaf pine",
                `611` = "sweetgum", `827` = "water oak",
                `831` = "willow oak", `833` = "northern red oak",
                `621` = "yellow-poplar")
region_of   = c(setNames(rep("pnw", length(species_pnw)), names(species_pnw)),
                setNames(rep("se",  length(species_se)),  names(species_se)))
target_spcd = as.integer(names(region_of))

max_per_species = 25L  # cap to land the total near ~300

# Well-understood units only (sidesteps G/MG/M/MM ambiguity for the shipped set)
dia_units_ok     = c("IN", "CM")
biomass_units_ok = c("LB", "KG")
ht_units_ok      = c("FT", "M", "NA", NA)

# Unit factor maps (canonical = inches / feet / pounds)
dia_factor = c(IN = 1, CM = 2.54)        # inches -> model dia unit
ht_factor  = c(FT = 1, M = 0.3048)       # feet   -> model ht unit ; NA -> 1
bio_factor = c(LB = 1, KG = 2.20462)     # model biomass unit -> pounds

# ---- read inputs ----------------------------------------------------------
db = dbConnect(SQLite(), db_path)
coefs   = as.data.table(dbGetQuery(db, "select * from BM_EqCoefs"))
comp    = as.data.table(dbGetQuery(db, "select COMP_ABBR, COMP_DESC from BM_comp"))
refspp  = as.data.table(dbGetQuery(db, "select spcd, common_name, genus, species from BM_ref_species"))
source0 = as.data.table(dbGetQuery(db, "select reference_no, reference_author, region, citation from BM_source"))
dbDisconnect(db)

templates = fread(templates_path, na.strings = c("", "NA"))
templates[, supported := as.logical(supported)]
templates_ok = templates[supported == TRUE & (is.na(extra_predictors) | extra_predictors == "")]

# ---- select + join --------------------------------------------------------
sel = coefs[species_code %in% target_spcd &
            eq_form_id %in% templates_ok$eqform_id &
            toupper(dia_unit) %in% dia_units_ok &
            toupper(biomass_unit) %in% biomass_units_ok &
            (is.na(ht_unit) | toupper(ht_unit) %in% toupper(ht_units_ok))]

sel = merge(sel, templates_ok[, .(eq_form_id = eqform_id, formula = template,
                                  response_scale, needs_height)],
            by = "eq_form_id", all.x = FALSE)

# ---- derive registry fields ----------------------------------------------
sel[, comp_token  := toupper(substr(BEQ, 7, 9))]
sel = merge(sel, comp[, .(comp_token = COMP_ABBR, component_desc = COMP_DESC)],
            by = "comp_token", all.x = TRUE)
sel = merge(sel, refspp, by.x = "species_code", by.y = "spcd", all.x = TRUE)
sel = merge(sel, source0, by.x = "source", by.y = "reference_no", all.x = TRUE)

mk_spp = function(genus, species, code){
  ok = !is.na(genus) & !is.na(species) & nzchar(genus) & nzchar(species)
  out = ifelse(ok, tolower(paste(genus, species, sep = ".")), paste0("nbel.", code))
  gsub("[^a-z0-9.]+", "", out)
}

num = function(x){ x = suppressWarnings(as.numeric(x)); x }

sel[, `:=`(
  model_id    = BEQ,
  model_class = "closed_form",
  response    = paste0("bm_", tolower(comp_token)),
  spp         = mk_spp(genus, species, species_code),
  region_code = region_of[as.character(species_code)],
  region      = region,
  authors     = reference_author,
  data_ref    = citation,
  b0 = num(a), b1 = num(b), b2 = num(c), b3 = num(d), b4 = num(e),
  b5 = dia_factor[toupper(dia_unit)],
  b6 = ifelse(is.na(ht_unit) | toupper(ht_unit) == "NA", 1, ht_factor[toupper(ht_unit)]),
  b7 = bio_factor[toupper(biomass_unit)],
  n      = num(sample_size),
  r2_pd  = num(R_squre),
  x1 = eq_form_id, x2 = num(dia_min), x3 = num(dia_max),
  x4 = toupper(dia_unit), x5 = toupper(biomass_unit),
  ht_unit   = toupper(ht_unit),
  bias_flag = corrected_for_bias,
  source_id = source,
  status    = "active",
  data      = "NBEL",
  year      = NA_integer_,
  voleq     = NA_character_,
  notes     = component_desc
)]

# b8 : log bias factor (1 unless genuinely corrected with a finite, positive value)
bc = num(sel$bias_correction)
corr_yes = tolower(trimws(ifelse(is.na(sel$corrected_for_bias), "", sel$corrected_for_bias))) %in% c("yes", "y")
sel[, b8 := ifelse(corr_yes & !is.na(bc) & bc > 0, bc, 1)]

# ---- plausibility guard ----------------------------------------------------
# A handful of source equations carry anomalous coefficients (e.g. a large
# linear-in-diameter term inside an exp()) that produce non-physical biomass.
# Evaluate each equation across a small tree grid and drop any that returns a
# non-finite, non-positive, or absurd (> 1e5 lb single-tree-component) value.
# This keeps the *shipped* suite vetted; dropped equations are logged.
.check_dbh = c(4, 10, 20)   # inches
.check_ht  = c(30, 60, 90)  # feet (paired with .check_dbh)
is_plausible = function(formula, brow){
  env = list2env(brow)
  env$dbh = .check_dbh; env$ht = .check_ht
  env$cr = 0.5; env$cl = env$ht * 0.5; env$topd = 4; env$p = 0.5
  v = tryCatch(eval(parse(text = formula), envir = env),
               error = function(e) rep(NA_real_, length(.check_dbh)))
  length(v) == length(.check_dbh) && all(is.finite(v)) && all(v > 0) && all(v < 1e5)
}
bcols = paste0("b", 0:8)
ok = vapply(seq_len(nrow(sel)), function(i)
  is_plausible(sel$formula[i], lapply(sel[i, ..bcols], as.numeric)), logical(1))
n_bad = sum(!ok)
if (n_bad > 0){
  message(sprintf("build_nbel_archive: dropped %d implausible equation(s): %s",
                  n_bad, paste(sel$model_id[!ok], collapse = ", ")))
}
sel = sel[ok]

# ---- cap per species, prefer most common forms ----------------------------
form_pop = sel[, .N, by = eq_form_id][order(-N)]
sel = merge(sel, form_pop[, .(eq_form_id, .form_rank = order(order(-N)))], by = "eq_form_id")
setorder(sel, region_code, spp, .form_rank, comp_token, model_id)
sel[, .rk := seq_len(.N), by = .(species_code)]
kept = sel[.rk <= max_per_species]

dropped = nrow(sel) - nrow(kept)
if (dropped > 0) message(sprintf(
  "build_nbel_archive: capped at %d/species; %d eligible equation(s) dropped (not shipped).",
  max_per_species, dropped))

# ---- assemble registry in core column order --------------------------------
core_cols = c("model_id", "system_id", "model_class", "response", "spp",
              "region_code", "region", "data", "year",
              "authors", "compiler", "notes", "status", "test date", "tester", "data_ref",
              "formula", paste0("b", 0:8),
              "n", "r2_pd", "syx_pd", "r2_proj", "syx_proj",
              "x1", "x2", "x3", "x4", "x5", "predictor_spec_path",
              "fit_object_path", "vcov_path",
              "serializer", "pkg", "pkg_version", "r_version", "lightened",
              "comp_token", "bias_flag", "ht_unit", "source_id",
              "voleq", "spcd", "forest", "district", "nvel_out")

for (cc in setdiff(core_cols, names(kept))) kept[, (cc) := NA]
out = kept[, ..core_cols]
setorder(out, region_code, spp, response, model_id)

# ---- seed a few NVEL volume rows (model_class == "nvel") -------------------
# These ride alongside the biomass rows; prediction is handled by the DLL-backed
# adapter in R/nvel_archive.R. voleq = NA lets NVEL_volume() select the equation
# from region/forest/district/species at predict time.
nvel_seed = rbindlist(lapply(list(
  list(spp = "pinus.taeda",           spcd = 131, region = 8, rc = "se"),
  list(spp = "pinus.elliottii",       spcd = 111, region = 8, rc = "se"),
  list(spp = "pinus.echinata",        spcd = 110, region = 8, rc = "se"),
  list(spp = "pseudotsuga.menziesii", spcd = 202, region = 6, rc = "pnw"),
  list(spp = "tsuga.heterophylla",    spcd = 263, region = 6, rc = "pnw"),
  list(spp = "pinus.ponderosa",       spcd = 122, region = 6, rc = "pnw")
), function(s) data.table(
  model_id = paste0("NVEL_", s$spcd, "_r", s$region),
  model_class = "nvel", response = "vol_tcf", spp = s$spp,
  region_code = s$rc, region = as.character(s$region), data = "NVEL",
  status = "active", notes = "Total cubic-foot volume (NVEL TCFV)",
  voleq = NA_character_, spcd = s$spcd, forest = "01", district = "01",
  nvel_out = "TCFV"
)), fill = TRUE)
for (cc in setdiff(core_cols, names(nvel_seed))) nvel_seed[, (cc) := NA]
nvel_seed = nvel_seed[, ..core_cols]

out = rbindlist(list(out, nvel_seed), use.names = TRUE, fill = TRUE)

fwrite(out, registry_path)

# ---- report ----------------------------------------------------------------
message(sprintf("Wrote %d biomass models to %s", nrow(out), registry_path))
print(out[, .N, by = .(region_code)])
print(out[, .N, by = .(region_code, spp)][order(region_code, -N)])
print(out[, .N, by = .(eqform = x1)][order(-N)])
print(out[, .N, by = .(response)][order(-N)])

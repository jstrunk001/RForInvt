# Build inst/extdata/FIADB_demo.db : a tiny, SYNTHETIC, schema-faithful FIADB
# SQLite used by the fia_* Phase-1 functions for runnable examples and tests.
#
# Modeled on Washington (STATECD 53) with a few westside timber counties and
# PNW species, so the runnable demo lines up with the WA vignette story.
#
# This is NOT a real FIA extract. The column names and table relationships
# mirror the FIA DataMart schema (PLOT/COND/TREE + POP_* design tables) closely
# enough to exercise EVALID-based plot/tree assembly and post-stratified
# expansion. Real-data EVALIDator validation happens in Phase 3 with a true
# state SQLite (see _dev/plan_fia_functions.md).
#
# Reproducible: deterministic seed, no external input. Re-run to regenerate.
#
#   Rscript _dev/make_fia_demo_db.R

suppressWarnings(suppressMessages({
  library(DBI); library(RSQLite)
}))

set.seed(20260611)

out_db <- file.path("inst", "extdata", "FIADB_demo.db")
if (!dir.exists(dirname(out_db))) dir.create(dirname(out_db), recursive = TRUE)
if (file.exists(out_db)) unlink(out_db, force = TRUE)

STATECD <- 53L            # Washington
# a few westside timber counties; names are COUNTYCD (county FIPS), values are
# the (made-up) estimation-unit acreages used to build EXPNS.
county_area <- c("33" = 260000, "41" = 180000, "27" = 220000)  # King, Lewis, Grays Harbor
counties <- as.integer(names(county_area))
spp      <- c(202L, 263L, 242L, 351L, 17L)  # Douglas-fir, w. hemlock, w. redcedar, red alder, grand fir

# ---------------------------------------------------------------------------
# Two volume evaluations so multi-year + change examples have something to bite
# on. EVALID = STATECD(2) + YEAR(2) + EVAL_TYP_CD(2); 01 = ALL/CURR, 03 = VOL.
# We keep it simple: one EVALID per year carrying both EXPCURR and EXPVOL types.
# ---------------------------------------------------------------------------
evals <- data.frame(
  EVAL_CN     = c("E2019", "E2021"),
  EVALID      = c(531901L, 532101L),
  EVAL_GRP_CN = c("G2019", "G2021"),
  STATECD     = STATECD,
  START_INVYR = c(2015L, 2017L),
  END_INVYR   = c(2019L, 2021L),
  LOCATION_NM = "Washington (SYNTHETIC)",
  stringsAsFactors = FALSE
)

POP_EVAL <- evals[, c("EVAL_CN", "EVALID", "EVAL_GRP_CN", "STATECD",
                      "START_INVYR", "END_INVYR", "LOCATION_NM")]
names(POP_EVAL)[1] <- "CN"

POP_EVAL_TYP <- do.call(rbind, lapply(seq_len(nrow(evals)), function(i) {
  data.frame(
    CN       = paste0("ET", i, c("a", "b", "c")),
    EVAL_CN  = evals$EVAL_CN[i],
    EVAL_TYP = c("EXPCURR", "EXPVOL", "EXPGROW"),
    stringsAsFactors = FALSE
  )
}))

# ---------------------------------------------------------------------------
# Estimation units (one per county per eval) and two strata each.
# ---------------------------------------------------------------------------
POP_ESTN_UNIT <- do.call(rbind, lapply(seq_len(nrow(evals)), function(i) {
  data.frame(
    CN        = paste0("EU_", evals$EVALID[i], "_", counties),
    EVAL_CN   = evals$EVAL_CN[i],
    EVALID    = evals$EVALID[i],
    ESTN_UNIT = counties,
    STATECD   = STATECD,
    AREA_USED = unname(county_area[as.character(counties)]),  # acres in the estimation unit
    stringsAsFactors = FALSE
  )
}))

# two strata per estimation unit; EXPNS = stratum acres / n plots in stratum.
strata_def <- expand.grid(
  eval_i    = seq_len(nrow(evals)),
  county    = counties,
  STRATUMCD = c(1L, 2L),
  KEEP.OUT.ATTRS = FALSE
)
strata_def <- strata_def[order(strata_def$eval_i, strata_def$county, strata_def$STRATUMCD), ]
strata_def$EVALID  <- evals$EVALID[strata_def$eval_i]
strata_def$EVAL_CN <- evals$EVAL_CN[strata_def$eval_i]
strata_def$EU_CN   <- paste0("EU_", strata_def$EVALID, "_", strata_def$county)
strata_def$STRATUM_CN <- paste0("ST_", strata_def$EVALID, "_", strata_def$county,
                                "_", strata_def$STRATUMCD)
# stratum 1 = forest (bigger weight), stratum 2 = sparse
strata_def$ADJ_FACTOR_SUBP <- ifelse(strata_def$STRATUMCD == 1, 1.02, 1.05)
strata_def$ADJ_FACTOR_MICR <- ifelse(strata_def$STRATUMCD == 1, 1.10, 1.18)
strata_def$ADJ_FACTOR_MACR <- 1.00
strata_def$n_plots <- ifelse(strata_def$STRATUMCD == 1, 6L, 4L)  # plots per stratum
strata_def$AREA_USED <- unname(county_area[as.character(strata_def$county)]) *
  ifelse(strata_def$STRATUMCD == 1, 0.6, 0.4)
strata_def$EXPNS <- strata_def$AREA_USED / strata_def$n_plots

POP_STRATUM <- data.frame(
  CN              = strata_def$STRATUM_CN,
  ESTN_UNIT_CN    = strata_def$EU_CN,
  EVAL_CN         = strata_def$EVAL_CN,
  EVALID          = strata_def$EVALID,
  ESTN_UNIT       = strata_def$county,
  STRATUMCD       = strata_def$STRATUMCD,
  P1POINTCNT      = strata_def$n_plots * 10L,
  P2POINTCNT      = strata_def$n_plots,
  EXPNS           = strata_def$EXPNS,
  ADJ_FACTOR_SUBP = strata_def$ADJ_FACTOR_SUBP,
  ADJ_FACTOR_MICR = strata_def$ADJ_FACTOR_MICR,
  ADJ_FACTOR_MACR = strata_def$ADJ_FACTOR_MACR,
  AREA_USED       = strata_def$AREA_USED,
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# Plots. Build the 2019 panel first, then a 2021 remeasurement of the SAME
# plots (PREV_PLT_CN links them) so change estimation has paired plots.
# ---------------------------------------------------------------------------
make_panel <- function(eval_i) {
  sd_i <- strata_def[strata_def$eval_i == eval_i, ]
  rows <- do.call(rbind, lapply(seq_len(nrow(sd_i)), function(k) {
    s <- sd_i[k, ]
    data.frame(
      STRATUM_CN = s$STRATUM_CN,
      EVALID     = s$EVALID,
      ESTN_UNIT  = s$county,
      STRATUMCD  = s$STRATUMCD,
      COUNTYCD   = s$county,
      n          = s$n_plots,
      stringsAsFactors = FALSE
    )[rep(1, s$n_plots), ]
  }))
  rows$plot_seq <- seq_len(nrow(rows))
  rows
}

panel19 <- make_panel(1)
panel21 <- make_panel(2)
stopifnot(nrow(panel19) == nrow(panel21))   # same plots remeasured
np <- nrow(panel19)

# stable plot identity (PLOT number + county) shared across years
plot_num <- 100L + seq_len(np)

PLOT <- rbind(
  data.frame(
    CN = paste0("P19_", seq_len(np)), STATECD = STATECD, UNITCD = 1L,
    COUNTYCD = panel19$COUNTYCD, PLOT = plot_num, INVYR = 2019L,
    MEASYEAR = 2019L, PREV_PLT_CN = NA_character_,
    LAT = 47.0 + plot_num / 1000, LON = -122.0 - plot_num / 1000,
    MACRO_BREAKPOINT_DIA = NA_real_, stringsAsFactors = FALSE
  ),
  data.frame(
    CN = paste0("P21_", seq_len(np)), STATECD = STATECD, UNITCD = 1L,
    COUNTYCD = panel21$COUNTYCD, PLOT = plot_num, INVYR = 2021L,
    MEASYEAR = 2021L, PREV_PLT_CN = paste0("P19_", seq_len(np)),
    LAT = 47.0 + plot_num / 1000, LON = -122.0 - plot_num / 1000,
    MACRO_BREAKPOINT_DIA = NA_real_, stringsAsFactors = FALSE
  )
)

# plot-stratum assignment
POP_PLOT_STRATUM_ASSGN <- rbind(
  data.frame(
    CN = paste0("A19_", seq_len(np)), STRATUM_CN = panel19$STRATUM_CN,
    PLT_CN = paste0("P19_", seq_len(np)), EVALID = panel19$EVALID,
    ESTN_UNIT = panel19$ESTN_UNIT, STRATUMCD = panel19$STRATUMCD,
    STATECD = STATECD, INVYR = 2019L, stringsAsFactors = FALSE
  ),
  data.frame(
    CN = paste0("A21_", seq_len(np)), STRATUM_CN = panel21$STRATUM_CN,
    PLT_CN = paste0("P21_", seq_len(np)), EVALID = panel21$EVALID,
    ESTN_UNIT = panel21$ESTN_UNIT, STRATUMCD = panel21$STRATUMCD,
    STATECD = STATECD, INVYR = 2021L, stringsAsFactors = FALSE
  )
)

# ---------------------------------------------------------------------------
# Conditions. Most plots fully forested (1 cond, COND_STATUS_CD 1); a couple
# split forest/nonforest; one fully nonforest (no trees) so zero-filling and
# area estimation have something to exercise.
# ---------------------------------------------------------------------------
make_cond <- function(plt_cn) {
  do.call(rbind, lapply(seq_along(plt_cn), function(i) {
    cn <- plt_cn[i]
    r  <- i %% 7
    if (r == 0) {
      # fully nonforest
      data.frame(CN = paste0(cn, "_c1"), PLT_CN = cn, CONDID = 1L,
                 COND_STATUS_CD = 2L, CONDPROP_UNADJ = 1.0, OWNCD = 46L,
                 FORTYPCD = NA_integer_, PROP_BASIS = "SUBP",
                 stringsAsFactors = FALSE)
    } else if (r == 1) {
      # split: 0.7 forest + 0.3 nonforest
      rbind(
        data.frame(CN = paste0(cn, "_c1"), PLT_CN = cn, CONDID = 1L,
                   COND_STATUS_CD = 1L, CONDPROP_UNADJ = 0.7, OWNCD = 46L,
                   FORTYPCD = 505L, PROP_BASIS = "SUBP", stringsAsFactors = FALSE),
        data.frame(CN = paste0(cn, "_c2"), PLT_CN = cn, CONDID = 2L,
                   COND_STATUS_CD = 2L, CONDPROP_UNADJ = 0.3, OWNCD = 46L,
                   FORTYPCD = NA_integer_, PROP_BASIS = "SUBP", stringsAsFactors = FALSE)
      )
    } else {
      # fully forested
      data.frame(CN = paste0(cn, "_c1"), PLT_CN = cn, CONDID = 1L,
                 COND_STATUS_CD = 1L, CONDPROP_UNADJ = 1.0, OWNCD = 46L,
                 FORTYPCD = 505L, PROP_BASIS = "SUBP", stringsAsFactors = FALSE)
    }
  }))
}
COND <- rbind(make_cond(PLOT$CN[PLOT$INVYR == 2019L]),
              make_cond(PLOT$CN[PLOT$INVYR == 2021L]))

# ---------------------------------------------------------------------------
# Trees. Forested condid 1 records get a handful of trees. 2021 trees grow
# from their 2019 selves (PREV_TRE_CN) so net growth is positive on average.
# ---------------------------------------------------------------------------
gen_trees_for_cond <- function(plt_cn, condid, base_dia = NULL) {
  ntr <- sample(2:5, 1)
  dia <- if (is.null(base_dia)) round(runif(ntr, 1, 22), 1)
         else round(base_dia + runif(length(base_dia), 0.1, 1.2), 1)
  n   <- length(dia)
  ht  <- round(4.5 + 5.0 * dia + rnorm(n, 0, 4), 0)
  sp  <- sample(spp, n, replace = TRUE)
  # micro vs subplot TPA convention: small trees on microplot (~74.965 TPA),
  # >= 5.0" on subplot (~6.018 TPA)
  tpa <- ifelse(dia < 5.0, 74.965282, 6.018046)
  vcf <- round(pmax(0, 0.0025 * dia^2 * ht), 2)         # toy net cubic ft
  vcs <- round(vcf * ifelse(dia >= 9, 4.5, 0), 1)       # board ft, sawtimber only
  bio <- round(vcf * 25, 1)                             # toy dry biomass lb
  list(dia = dia, ht = ht, sp = sp, tpa = tpa,
       vcf = vcf, vcs = vcs, bio = bio)
}

build_trees <- function(invyr, prev_lookup = NULL) {
  cond_yr <- COND[COND$PLT_CN %in% PLOT$CN[PLOT$INVYR == invyr] &
                  COND$COND_STATUS_CD == 1L & COND$CONDID == 1L, ]
  out <- list(); k <- 0L
  for (i in seq_len(nrow(cond_yr))) {
    plt <- cond_yr$PLT_CN[i]; cid <- cond_yr$CONDID[i]
    base <- NULL
    if (!is.null(prev_lookup)) base <- prev_lookup[[plt]]
    tg <- gen_trees_for_cond(plt, cid, base_dia = base$dia)
    n  <- length(tg$dia)
    cn <- paste0(plt, "_t", seq_len(n))
    prev_cn <- if (!is.null(base)) base$cn[seq_len(n)] else NA_character_
    k <- k + 1L
    out[[k]] <- data.frame(
      CN = cn, PLT_CN = plt, CONDID = cid, SUBP = rep(1:4, length.out = n),
      TREE = seq_len(n), STATUSCD = 1L,
      SPCD = if (!is.null(base)) base$sp[seq_len(n)] else tg$sp,
      DIA = tg$dia, HT = tg$ht, TPA_UNADJ = tg$tpa,
      VOLCFNET = tg$vcf, VOLCSNET = tg$vcs, DRYBIO_AG = tg$bio,
      DIAHTCD = 1L, PREV_TRE_CN = prev_cn, stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}

TREE19 <- build_trees(2019L)

# lookup of 2019 trees keyed by their *2021* plot CN (P21_i shares index with P19_i)
prev_lookup <- list()
for (i in seq_len(np)) {
  p19 <- paste0("P19_", i); p21 <- paste0("P21_", i)
  tr <- TREE19[TREE19$PLT_CN == p19, ]
  if (nrow(tr)) prev_lookup[[p21]] <- list(dia = tr$DIA, sp = tr$SPCD, cn = tr$CN)
}
TREE21 <- build_trees(2021L, prev_lookup = prev_lookup)
TREE <- rbind(TREE19, TREE21)

# ---------------------------------------------------------------------------
# Reference species table (subset of REF_SPECIES) for friendly labels.
# ---------------------------------------------------------------------------
REF_SPECIES <- data.frame(
  SPCD = spp,
  COMMON_NAME = c("Douglas-fir", "western hemlock", "western redcedar",
                  "red alder", "grand fir"),
  GENUS = c("Pseudotsuga", "Tsuga", "Thuja", "Alnus", "Abies"),
  SPECIES = c("menziesii", "heterophylla", "plicata", "rubra", "grandis"),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# Write everything.
# ---------------------------------------------------------------------------
con <- dbConnect(SQLite(), out_db)
tabs <- list(
  PLOT = PLOT, COND = COND, TREE = TREE,
  POP_EVAL = POP_EVAL, POP_EVAL_TYP = POP_EVAL_TYP,
  POP_ESTN_UNIT = POP_ESTN_UNIT, POP_STRATUM = POP_STRATUM,
  POP_PLOT_STRATUM_ASSGN = POP_PLOT_STRATUM_ASSGN,
  REF_SPECIES = REF_SPECIES
)
for (nm in names(tabs)) dbWriteTable(con, nm, tabs[[nm]], overwrite = TRUE)
dbDisconnect(con)

cat("Wrote", out_db, "\n")
cat("PLOT:", nrow(PLOT), " COND:", nrow(COND), " TREE:", nrow(TREE),
    " strata:", nrow(POP_STRATUM), " evals:", nrow(POP_EVAL), "\n")

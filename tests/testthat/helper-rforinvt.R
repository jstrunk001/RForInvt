# Shared test helpers for RForInvt.
# Loaded automatically by testthat before any test-*.R file.

# ---- NVEL Fortran DLL availability -----------------------------------------
# The NVEL family wraps inst/lib/.../vollib.dll. Tests that genuinely need the
# DLL should skip() when it cannot be located/loaded so the suite stays green
# on machines without it. (Note: at the pre-refactor baseline several NVEL
# wrappers fail on a *code* bug, not a missing DLL -- those tests assert the
# intended behavior and are expected to FAIL here.)

nvel_dll_path <- function() {
  cands <- c(
    "lib/VolLibDll20231106/vollib-64bits/vollib.dll",
    "lib/vollib-64bits/vollib.dll"
  )
  for (rel in cands) {
    p <- system.file(rel, package = "RForInvt")
    if (nzchar(p) && file.exists(p)) return(p)
  }
  ""
}

nvel_dll_available <- function() {
  p <- nvel_dll_path()
  if (!nzchar(p) || !file.exists(p)) return(FALSE)
  if (!"vollib" %in% names(getLoadedDLLs())) {
    ok <- tryCatch({ dyn.load(p); TRUE }, error = function(e) FALSE)
    if (!ok) return(FALSE)
  }
  TRUE
}

# ---- FVS executable discovery ----------------------------------------------
# fvs_run() shells out to an FVS variant executable (e.g. FVSie.exe). Locate one
# so the integration test can run; skip when none is installed. Override with
# the RFORINVT_FVS_DIR env var (a directory containing FVS*.exe).
fvs_exe <- function(variant = "FVSie") {
  exe <- paste0(variant, if (.Platform$OS.type == "windows") ".exe" else "")
  cands <- c(
    file.path(Sys.getenv("RFORINVT_FVS_DIR"), exe),
    file.path("C:/Users/Public/Documents/FVS/FVSbin", exe),
    file.path("C:/FVSbin", exe),
    unname(Sys.which(variant))
  )
  cands <- cands[nzchar(cands)]
  hit <- cands[file.exists(cands)]
  if (length(hit)) normalizePath(hit[1], winslash = "/") else ""
}

fvs_available <- function(variant = "FVSie") nzchar(fvs_exe(variant))

# ---- small deterministic fixtures ------------------------------------------

# A tiny, signal-heavy modeling frame: y is a near-deterministic function of
# x1 and x2 (x3 is noise). Used by the ols_modeling / estimators tests.
make_model_df <- function(n = 60, seed = 1) {
  set.seed(seed)
  x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n)
  y  <- 2 + 3 * x1 - 1.5 * x2 + rnorm(n, sd = 0.3)
  data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
}

# A tiny reference/target frame for the kNN (yaImpute) tests.
make_knn_ref <- function(n = 20, seed = 7) {
  set.seed(seed)
  x1 <- rnorm(n); x2 <- rnorm(n)
  data.frame(
    id = paste0("r", seq_len(n)),
    x1 = x1, x2 = x2,
    y1 = x1 + rnorm(n, sd = 0.1),
    y2 = x2 + rnorm(n, sd = 0.1),
    stringsAsFactors = FALSE
  )
}

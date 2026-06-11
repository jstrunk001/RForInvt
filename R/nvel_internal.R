#' Shared internals for the NVEL_* family
#'
#' These helpers are used by NVEL_volume(), NVEL_buck(), NVEL_merch(),
#' NVEL_voleq(), NVEL_biomass(), and NVEL_wtfactor(). Previously each of those
#' files defined its own copy of the DLL loader under the same name
#' (.load_dll), so only one copy actually survived in the package namespace.
#' Consolidating here removes that collision and gives one place to maintain
#' the (versioned) path to the National Volume Estimator Library binaries.
#'
#' @keywords internal
#' @noRd
NULL

# Default 32-/64-bit paths to the bundled NVEL volume library (vollib.dll).
# Centralized so a library version bump is a one-line change instead of an
# edit in six files.
.nvel_dll_paths = function(package = "RForInvt"){
  list(
    dll_64 = system.file("lib/VolLibDll20231106/vollib-64bits/vollib.dll", package = package)
    ,dll_32 = system.file("lib/VolLibDll20231106/vollib-32bits/vollib.dll", package = package)
  )
}

# Load the architecture-appropriate vollib.dll if it is not already loaded.
# Mirrors the most robust of the former per-file copies (NVEL_wtfactor): it
# checks the file exists and warns rather than erroring cryptically.
.nvel_load_dll = function(dll_64, dll_32){

  arch_in = R.Version()$arch
  dll_loaded = "vollib" %in% names(getLoadedDLLs())
  if(dll_loaded) return(invisible(TRUE))

  target_dll = if(arch_in == "x86_64") dll_64 else dll_32
  if(file.exists(target_dll)){
    dyn.load(target_dll)
    invisible(TRUE)
  } else {
    warning("NVEL DLL not found at specified path: ", target_dll)
    invisible(FALSE)
  }
}

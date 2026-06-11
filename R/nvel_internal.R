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
# vollib is a Windows-only Fortran binary; on other platforms (where dyn.load
# of the bundled .dll would throw an opaque loader error) and when the file is
# missing, fail fast with a clear message instead of letting the eventual
# .Fortran() call die with '"vollib_r" not available'.
.nvel_load_dll = function(dll_64, dll_32){

  if(.Platform$OS.type != "windows"){
    stop("The NVEL volume library (vollib.dll) is Windows-only; ",
         "NVEL_* functions cannot run on this platform (",
         .Platform$OS.type, ").")
  }

  if("vollib" %in% names(getLoadedDLLs())) return(invisible(TRUE))

  arch_in = R.Version()$arch
  target_dll = if(arch_in == "x86_64") dll_64 else dll_32
  if(!file.exists(target_dll)){
    stop("NVEL DLL not found at: ", target_dll,
         ". Reinstall RForInvt or supply dll_64/dll_32 explicitly.")
  }
  dyn.load(target_dll)
  invisible(TRUE)
}

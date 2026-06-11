# RForInvt Coding Conventions (binding)

The contract every refinement is checked against. Derived from the tidyverse style guide the author
already references (`inst/tidyverse_coding_convention/`) and the roxygen template the author ships
(`inst/templates/roxy_header_v1.4.r`).

## 1. Naming — snake_case

- **Functions:** `snake_case` (`compile_trees`, `make_strata`). Keep the `NVEL_` acronym prefix
  (`NVEL_volume`). S3 methods follow `generic.class` (`pander_mod.lm`).
- **Arguments & internal variables:** `snake_case`. Convert camelCase (`dbhNm`→`dbh_nm`,
  `xNms`→`x_nms`, `idNm`→`id_nm`, `R2_in`→`r2_in`).
- **Internal helpers:** dot-prefixed snake_case (`.nvel_load_dll`), and `@keywords internal` + `@noRd`.
- No abbreviations that obscure meaning; existing short domain terms (`tpa`, `ba_ft`, `dbcl`) are kept.

## 2. Roxygen header (per exported function)

Required tags, in this order, with **real** content (never `tbd` / `<Delete and Replace>`):

```
#' @title        <one line>
#' @description  <what it does, 1-3 sentences>
#' @details      <behavior, edge cases, units, revision history>
#' @param <arg>  <one per argument, describe type and meaning>
#' @return       <type and structure of the result>
#' @examples     <runnable; simulated data or shipped FIADB_RI.db>
#' @seealso      <related functions in the family>
#' @export
```

Internal functions: `#' @keywords internal` + `#' @noRd` only.

## 3. Examples policy

- Every `@examples` block must execute under `R CMD check`.
- Prefer tiny inline simulated data (`data.frame(...)` / `data.table(...)`).
- For DB-backed examples use `system.file("extdata", "FIADB_RI.db", package = "RForInvt")`.
- Wrap DLL-dependent or long-running calls in `\donttest{}` (still parsed/checked) — not `\dontrun{}`.
- Never reference hardcoded local or network paths.

## 4. Robustness

- No unconditional `browser()` in `R/`. Debug hooks must be `if (debug) browser()`.
- No `require()` / `library()` in function bodies — declare in NAMESPACE (`@import`/`@importFrom`)
  or guard optional deps with `if (!requireNamespace("pkg", quietly = TRUE)) stop(...)`.
- Validate inputs early with clear `stop()` messages (required columns present, positive DBH/height).
- No silent overwrite of user-supplied arguments.

## 5. Return types

- One return type per function family. **NVEL family → `data.table`.** Document it in `@return`.

## 6. Magic numbers

- Replace with a named constant or annotate inline with the source
  (e.g. `0.005454` = π/576 imperial basal-area constant; FIA 24-ft subplot radius; EPSG codes).

## 7. Metadata / build

- `DESCRIPTION` `Imports:` is one-package-per-line, no leading commas, no duplicates, and matches NAMESPACE.
- Test-only / vignette deps go in `Suggests:`.
- `.Rbuildignore` excludes all dev/review/archive material and binary cruft.
- **Maintainer/author email stays `someone@somewhere.com` (placeholder retained by request).**

## 8. Dead code

- No commented-out code blocks left in `R/`. Move salvageable experiments to `_dev/`, otherwise delete.

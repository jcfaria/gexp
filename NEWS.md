<!-- Faria, J. C.; Allaman, I. B. -->

# News - gexp R package

## 1.1-0 (2026-05-23) - Faria, J. C.

### Internal
- Added `R/gexp-utils.R` with shared helpers for error simulation, response assembly, data-frame binding, result packing, and contrast merging; all nine `gexp.*` methods now call these helpers (no change in exported behaviour).
- Extended `gexp-utils.R` with helpers for default fixed effects, block effects, treatment/contrast construction, row/column LSD layout, and interaction terms; RCBD and LSD methods now use these (behaviour unchanged).
- Added split-plot helpers (`.gexp_zformula`, `.gexp_Z`, `.gexp_spe_finish`) for whole-plot `Z` matrix construction and the shared SPE response tail; `gexp.spe_crd`, `gexp.spe_rcbd`, and `gexp.spe_lsd` now use them (behaviour unchanged).

### Code style
- Reformatted all sources under `R/` for readability (aligned named arguments, consistent spacing, and line breaks at commas) without changing program logic.
- Renamed `vignettes/intro.Rmd` to `vignettes/gexp-overview.Rmd` with conventional R code spacing in examples (aligned with standard style, distinct from `/R` sources).
- Revised vignette prose: improved English and translated remaining Portuguese passages.

### Documentation
- Standardized author names to scientific notation (`Faria, J. C.`; `Allaman, I. B.`) across the package.
- Migrated package history from `ChangeLog` to `NEWS.md`.
- Refactored `README.md` to match the documentation standard used in sibling packages (badges, features, installation, quick start, project layout, contributing, and roadmap).
- Updated `DESCRIPTION` metadata (`Authors@R`, `BugReports`, reformatted `Description`, `License: GPL-2`, `R (>= 4.1.0)`); maintainer remains Allaman, I. B.
- Removed prebuilt vignette artifact `inst/doc/intro.html` from the repository.

### Infrastructure
- Added `.Rbuildignore` to exclude version-control and check artefacts from package builds.
- Added `.gitignore` and `.Rbuildignore` rules to keep locally rendered `vignettes/*.html` out of the repository.
- Added a `testthat` suite under `tests/testthat/` with smoke tests for core design generation, `summary`, `print`, and `plot`.

## 1.0-21 (2023-06-14) - Allaman, I. B.

- Adjusted files as required by CRAN.

## 1.0-2 (2020-10-29) - Allaman, I. B.

- Changed character columns to factors in `gexp.fe_lsd()` and `gexp.simple_lsd()`.

## 1.0-1 (2020-04-01) - Allaman, I. B.

- Fixed a bug in `gexp.spe_lsd()`: added `stringsAsFactors = TRUE` when calling `data.frame()`, matching the default change in newer R versions (`stringsAsFactors = FALSE`).

## 1.0-0 (2019-06-14) - Allaman, I. B.

- Split the former `type` argument into two arguments: `type` (`SIMPLE`, `FE`, `SPE`) and `design` (`CRD`, `RCBD`, `LSD`).
- Added new methods for generic `gexp`: `simple_crd`, `simple_rcbd`, `simple_lsd`, `fe_crd`, `fe_rcbd`, `fe_lsd`, `spe_crd`, `spe_rcbd`, and `spe_lsd`.
- Removed `nrand` and `random` from the `gexp` functions; added `random` to the plot functions instead.
- Added internal helpers: `makeInteraction`, `makeContrasts`, `makeTreatments`, and `makeXBeta`.
- Optimized code across all functions.
- Updated `.Rd` files, vignettes, `NAMESPACE`, and `DESCRIPTION` to reflect the new API.
- Minor documentation corrections.

## 0.1-6 (2019-05-02) - Allaman, I. B.

- Fixed incorrect calculations for quantitative factors in RCBD and LSD designs.

## 0.1-5 (2019-04-13) - Faria, J. C.

- Updated `NAMESPACE` for consistency with the package API.

## 0.1-4 (2019-04-13) - Allaman, I. B.

- Minor documentation corrections.
- Released to CRAN.

## 0.1-3 (2019-04-13) - Faria, J. C.

- All plot functions now restore graphics parameters changed via `par(xaxs = "i", yaxs = "i")` when returning to the device.
- Minor changes to the source code of some functions.

## 0.1-2 (2019-04-09) - Allaman, I. B.

- Corrected the package name typo (`gerexp` → `gexp`) in `gexp-package.Rd`.
- Corrected the date entry for version 0.1-1 in the change log.

## 0.1-1 (2019-04-06) - Faria, J. C.

- Fixed encoding issues in some documentation (`.rmd`) files that caused build failures.
- General documentation improvements.

## 0.1-0 (2018-10-12) - Allaman, I. B.

- Initial release of the `gexp` package.

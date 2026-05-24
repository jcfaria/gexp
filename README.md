<!-- Faria, J. C.; Allaman, I. B. -->
# gexp

<!-- Badges -->
[![CRAN status](https://www.r-pkg.org/badges/version/gexp)](https://cran.r-project.org/package=gexp)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/gexp)](https://cran.r-project.org/package=gexp)
[![CRAN checks](https://badges.cranchecks.info/worst/gexp.svg)](https://cran.r-project.org/web/checks/check_results_gexp.html)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License: GPL-2](https://img.shields.io/badge/License-GPL--2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)

`gexp` is an R package for planning, simulating, and visualizing structured experiments.

## Key Features

- **Experimental designs**: completely randomized (CRD), randomized complete block (RCBD), and Latin square (LSD).
- **Experiment types**: simple (`SIMPLE`), factorial (`FE`), and split-plot (`SPE`).
- **Controlled simulation** of treatment effects, errors, and multivariate responses with optional covariance structure.
- **Layout planning** from images of the experimental area (`jpeg` / `png` input).
- **S3 workflow** with `summary()`, `print()`, and `plot()` methods for each design class.

## Installation

Install from CRAN:

```r
install.packages("gexp")
```

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("ivanalaman/gexp")
```

## Quick Start

```r
library(gexp)

# Completely randomized design with two treatment levels
crd <- gexp(
  mu  = 15,
  err = matrix(0, nrow = 6, ncol = 1),
  r   = 3,
  fe  = list(alpha = c(1, -2))
)
summary(crd)
plot(crd)
```

For more complete examples, see:

- `vignette("gexp-overview", package = "gexp")`

## Project Layout

- `/R`: Core computational, planning, and plotting functions.
- `/man`: Documentation (`.Rd` files).
- `/inst`: Extra package materials (e.g. bibliography).
- `/vignettes`: Vignettes and tutorials.

## Contributing

Contributions are welcome. Open an **[Issue](https://github.com/ivanalaman/gexp/issues)** or submit a **Pull Request** on [github.com/ivanalaman/gexp](https://github.com/ivanalaman/gexp) with:

- Bug fixes and performance improvements.
- Documentation and usability improvements.
- New ideas for experiment generation and visualization workflows.

## Roadmap

- Modernize package metadata and repository layout to match current maintenance standards.
- Expand test coverage for design generation and plotting behavior.
- Add practical vignettes and runnable demos with real-world examples.
- Improve CI signals and package quality checks.

---
Developed by:  
Faria, J. C.; Allaman, I. B.  
Universidade Estadual de Santa Cruz - UESC  
Departamento de Ciências Exatas - DCEX  
Ilhéus - Bahia - Brasil

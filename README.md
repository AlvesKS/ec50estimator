[![CRAN](https://www.r-pkg.org/badges/version/ec50estimator)](https://CRAN.R-project.org/package=ec50estimator)
[![Downloads](https://cranlogs.r-pkg.org/badges/ec50estimator)](https://CRAN.R-project.org/package=ec50estimator)
[![Total downloads](https://cranlogs.r-pkg.org/badges/grand-total/ec50estimator)](https://CRAN.R-project.org/package=ec50estimator)

# ec50estimator: EC50 workflows for multi-isolate dose-response experiments <img width="200px" align="right" src="man/figures/logo.png">

`ec50estimator` helps scientists estimate EC50 from grouped dose-response
datasets. It wraps the modelling engine from `drc` in a workflow that is easier
to use for experiments with many isolates, fields, fungicides, years, or other
strata.

The package is built around one practical sequence:

1. Check whether the dose-response data are fit-ready.
2. Fit one model or compare several candidate models.
3. Inspect fit quality and failed fits.
4. Plot fitted curves with `ggplot2`.
5. Extract predictions, curve coordinates, and manuscript-ready EC50 tables.

## Installation

Install the stable release from CRAN.

```r
install.packages("ec50estimator")
```

Install the development version from GitHub.

```r
pak::pak("AlvesKS/ec50estimator")
```

## Quick Start

```r
library(ec50estimator)
library(drc)

data(multi_isolate)

example_data <- subset(
  multi_isolate,
  isolate %in% 1:5 & fungicida == "Fungicide A"
)

check_ec50_data(
  example_data,
  response = "growth",
  dose = "dose",
  isolate = "isolate",
  strata = "field"
)

fit <- ec50_multimodel(
  growth ~ dose,
  data = example_data,
  isolate_col = "isolate",
  strata_col = "field",
  fct = list(drc::LL.3(), drc::LL.4(), drc::W2.3()),
  interval = "delta"
)

best_model(fit)
plot_EC50_curves(fit, models = "best")
report_ec50(fit, models = "best")
```

`fit` is still a data frame, so existing workflows that call `head(fit)` or
write the estimates to a file continue to work. It also stores the fitted `drc`
models and metadata needed by helper functions:

```r
curve_data(fit)
fitted_models(fit)
ec50_metadata(fit)
predict_ec50(fit, dose = c(0.001, 0.01, 0.1), models = "best")
plot_residuals(fit, models = "best")
```

See the pkgdown site for the recommended workflow:
<https://alvesks.github.io/ec50estimator/>.

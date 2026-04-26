[![CRAN](https://www.r-pkg.org/badges/version/ec50estimator)](https://CRAN.R-project.org/package=ec50estimator)
[![Downloads](https://cranlogs.r-pkg.org/badges/ec50estimator)](https://CRAN.R-project.org/package=ec50estimator)
[![Total downloads](https://cranlogs.r-pkg.org/badges/grand-total/ec50estimator)](https://CRAN.R-project.org/package=ec50estimator)

# *ec50estimator*: an easy way to estimate EC<sub>50</sub> for multi-isolate datasets <img width = 200px align = right src="man/figures/logo.png" >

# Introduction

*ec50estimator* provides a quick, automated way to estimate the effective concentration that reduces growth by 50% (EC<sub>50</sub>) from multi-isolate dose-response datasets. It is designed for scientists who need a simple workflow for grouped or stratified experiments while still using the modelling engine from `drc`.

The package now keeps runtime dependencies intentionally small: `drc` is the only imported package.

## Download and install

Install the stable release from CRAN.

``` r
install.packages("ec50estimator")
```
A development version can be download using the following code: 

``` r
pak::pak("AlvesKS/ec50estimator")
```

## Quick example

``` r
library(ec50estimator)

data(multi_isolate)

ec50 <- estimate_EC50(
  growth ~ dose,
  data = multi_isolate,
  isolate_col = "isolate",
  strata_col = c("field", "fungicida"),
  fct = drc::LL.3(),
  interval = "delta"
)

head(ec50)
```

[![CRAN](https://www.r-pkg.org/badges/version/ec50estimator)](https://CRAN.R-project.org/package=ec50estimator)
[![Downloads](https://cranlogs.r-pkg.org/badges/ec50estimator)](https://CRAN.R-project.org/package=ec50estimator)

# *ec50estimator*: An easy way to estimate EC<sub>50</sub> for multi isolate datasets <img width = 200px align = right src="man/figures/logo.png" >

# Introduction

*ec50estimator* provides a quick, easy and automated way of estimating the effective control to 50% of growth inhibition (EC<sub>50</sub>) from multi isolate datasets. It also is optimized to deal with stratified data.

## Download and install

Install the stable release from CRAN.

``` r
install.packages("ec50estimator")
```
A development version can be download using the following code: 

``` r
if (!require(devtools)) {
  install.packages("devtools")
}

devtools::install_github("AlvesKS/ec50estimator")
```

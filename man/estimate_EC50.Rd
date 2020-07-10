\name{estimate_EC50}
\alias{estimate_EC50}
\title{Estimate Efective dose (EC50) for multi isolate data set}
\usage{
estimate_EC50(formula, data, isolate_col,
strata_col = NULL,
fct,interval = c("none", "delta", "fls", "tfls"))
}
\description{
Estimate Efective dose (EC50) for multi isolate stratified data set.
}
\arguments{
\item{formula}{An object of class "\code{\link[stats]{formula}}"}
\item{data}{A \code{data.frame} in which the dose-response data  is in}
\item{isolate_col}{indicate the isolate colunm}
\item{strata_col}{indicate the strata colunms. If there are more then one colunms, indicate as a vector (e.g. c("region", "field"))}
\item{fct}{A list with three or more elements specifying the non-linear function. For more info, see "\code{\link[drc]{drm}}". }
\item{interval}{A character string specifying the type of confidence intervals to be supplied. For more information see "\code{\link[drc]{ED}}" }
}

\name{estimate_EC50}
\alias{estimate_EC50}
\title{Estimate Efective dose (EC50) for multi isolate data set}
\usage{
estimate_EC50(formula, data, EC_lvl = 50, isolate_col,
strata_col = NULL,
fct,interval = c("none", "delta", "fls", "tfls"))
}
\description{
Estimate Efective dose (EC50) for multi isolate stratified data set.
}
\arguments{
\item{formula}{An object of class "\code{\link[stats]{formula}}.(e.g. growth ~ dose")}
\item{data}{A \code{data.frame} in which the dose-response data is in}
\item{EC_lvl}{Define the EC level. Default is 50}
\item{isolate_col}{indicate the isolate colunm. Indicate the name inside "". (e.g. "isolates")}
\item{strata_col}{indicate the strata colunms. If there are more then one colunms, indicate as a vector (e.g. c("region", "field"))}
\item{fct}{A list with three or more elements specifying the non-linear function. Please, inform the model function with the :: operator to inform the drc package (e.g. \code{drc::LL.4()}). For more info, see "\code{\link[drc]{drm}}". }
\item{interval}{A character string specifying the type of confidence intervals to be supplied. For more information see "\code{\link[drc]{ED}}" }
}

\examples{
# Load data
data(multi_isolate)

df_ec50 = estimate_EC50(growth~dose,
                        data =multi_isolate,
                        isolate_col = "isolate",
                        strata_col =  c("field","fungicida"),
                        interval = "delta",
                        fct = drc::LL.3())
head(df_ec50)

}

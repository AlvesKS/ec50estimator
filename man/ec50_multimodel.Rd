\name{ec50_multimodel}
\alias{ec50_multimodel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate Efective dose (EC50) for multi isolate data set using one or more models
}

\description{
Estimate Efective dose (EC50) for multi isolate stratified data set using one or more models.
}

\usage{
ec50_multimodel(formula, data, EC_lvl = 50,
isolate_col, strata_col= NULL,
fct, interval = c("none", "delta", "fls", "tfls"),
type = c("relative", "absolute"))
}


%- maybe also 'usage' for other objects documented here.
\arguments{
\item{formula}{An object of class "\code{\link[stats]{formula}}.(e.g. growth ~ dose")}
\item{data}{A \code{data.frame} in which the dose-response data is in}
\item{EC_lvl}{Define the EC level. Default is 50}
\item{isolate_col}{indicate the isolate colunm. Indicate the name inside "". (e.g. "isolates")}
\item{strata_col}{indicate the strata colunms. If there are more then one colunms, indicate as a vector (e.g. c("region", "field"))}
\item{fct}{A list specifying the non-linear models to be fitted. Please, inform the model functions with the :: operator to inform the drc package (e.g. \code{list(drc::LL.4(), drc::LL.3())}). For more info, see "\code{\link[drc]{drc::drm}}". }
\item{interval}{A character string specifying the type of confidence intervals to be supplied. For more information see "\code{\link[drc]{ED}}" }
\item{type}{Whether the specified response levels are absolute or relative (default)}
}


\value{
A dataframe containing EC estimates and statistics for model selection for each model used. For more info, see "\code{\link[drc]{drc::ED}}".
}


\examples{

data(multi_isolate)


# Load data
ec50_multimodel(growth ~ dose,
                data = multi_isolate,
                isolate_col = "isolate",
                strata_col = c("fungicida", "field"),
                fct = list(drc::LL.5(),drc::W2.3(), drc::LL2.3()))



}


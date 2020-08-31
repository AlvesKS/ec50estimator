\name{multi_isolate}
\alias{multi_isolate}
\docType{data}
\title{
Multi isolate dataset
}
\description{
Dataset containing simulated data of mycelial growth under increasing fungicide doses for 50 fungal isolates, two types of field (conventional and organic), and two different fungicides.
}
\usage{data("multi_isolate")}
\format{
  A data frame with 3500 observations on the following 5 variables.
  \describe{
    \item{\code{isolate}}{a numeric vector}
    \item{\code{field}}{a factor with levels \code{Conventional} \code{Organic}}
    \item{\code{fungicida}}{a factor with levels \code{Fungicide A} \code{Fungicide B}}
    \item{\code{dose}}{a numeric vector}
    \item{\code{growth}}{a numeric vector}
  }
}

\examples{
data(multi_isolate)
## maybe str(multi_isolate) ; plot(multi_isolate) ...
}
\keyword{datasets}

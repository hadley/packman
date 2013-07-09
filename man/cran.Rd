\name{cran}
\alias{cran}
\title{Package source: CRAN}
\usage{
  cran(type = "binary", repos = getOption("repos"))
}
\arguments{
  \item{type}{package type to try, \code{"binary"} or
  \code{"source"}.}

  \item{repos}{URL to repository. If not set, will default
  to the Rstudio cloud mirror.}
}
\description{
  Package source: CRAN
}
\examples{
cran("binary")
cran("source")
}


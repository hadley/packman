\name{default_sources}
\alias{default_sources}
\title{Default package sources}
\usage{
  default_sources(force = FALSE)
}
\arguments{
  \item{force}{if \code{TRUE}, will not include currently
  installed packages in the list, and hence will force a
  reinstall of all packages, even those that are already
  installed.}
}
\description{
  Default package sources
}
\details{
  These are (in order):

  \itemize{ \item \code{\link{installed}()}, if
  \code{force} is \code{FALSE}.  \item
  \code{\link{cran}("source")}, if a development
  environment is available.  \item
  \code{\link{cran}("binary")}, if binary packages are
  available for the current platform. }
}
\examples{
def <- default_sources()
def
package_info(def, "ggplot2")
}


\name{has_package}
\alias{has_package}
\title{Does the source provide a package?}
\usage{
  has_package(source, package, version = NULL)
}
\description{
  Does the source provide a package?
}
\examples{
offline_packages()
has_package(cran("source"), "ggplot2")
has_package(cran("binary"), "ggplot2")
has_package(installed(), "ggplot2")
has_package(cran("source"), "ggplot4")
has_package(cran("binary"), "ggplot4")
has_package(installed(), "ggplot4")
}


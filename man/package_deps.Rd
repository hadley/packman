\name{package_deps}
\alias{package_deps}
\title{Find package info for dependencies.}
\usage{
  package_deps(info,
    from = c("Depends", "Imports", "LinkingTo"),
    sources = default_sources())
}
\arguments{
  \item{info}{a list containing package information}

  \item{from}{which fields to consider dependencies from}
}
\description{
  Find package info for dependencies.
}
\examples{
ggplot2 <- package_info(default_sources(), "ggplot2")
package_deps(ggplot2)
}
\keyword{internal}


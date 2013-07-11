\name{find_dependencies}
\alias{find_dependencies}
\title{Find all dependencies of a package.}
\usage{
  find_dependencies(pkg = NULL,
    from = c("Depends", "Imports", "LinkingTo"),
    sources = default_sources())
}
\description{
  Find all dependencies of a package.
}
\examples{
find_dependencies("ggplot2")
}


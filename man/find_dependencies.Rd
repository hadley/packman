\name{find_dependencies}
\alias{find_dependencies}
\title{Find all (recursive) dependencies of a package.}
\usage{
  find_dependencies(pkg = NULL,
    from = c("Depends", "Imports", "LinkingTo"),
    sources = default_sources())
}
\arguments{
  \item{pkg}{a single package name}

  \item{from}{which DESCRIPTION fields to use to compute
  dependencies.  Defaults to depends, imports and linking
  to}

  \item{sources}{The sources in which to look for
  \code{pkg}. These will also be used for dependencies of
  \code{pkg}, as well as any additional sources described
  in individual packages.}
}
\description{
  Find all (recursive) dependencies of a package.
}
\examples{
deps <- find_dependencies("ggplot2")
sapply(deps, is.installed)

# Base packages are never included in the list of dependencies
sapply(deps, is.base)

# Supplying a different set of sources will determine whether or not
# packages are already installed.
deps2 <- find_dependencies("ggplot2", sources = default_sources(TRUE))
sapply(deps2, is.installed)
}


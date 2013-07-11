\name{add_dependencies}
\alias{add_dependencies}
\title{Given a list of packages, add all their dependencies.}
\usage{
  add_dependencies(pkgs = NULL,
    from = c("Depends", "Imports", "LinkingTo"),
    sources = default_sources())
}
\arguments{
  \item{pkg}{a character vector of package names}

  \item{from}{which DESCRIPTION fields to use to compute
  dependencies.  Defaults to depends, imports and linking
  to}

  \item{sources}{The sources in which to look for
  \code{pkg}. These will also be used for dependencies of
  \code{pkg}, as well as any additional sources described
  in individual packages.}
}
\value{
  a list of packages topologically sorted by their
  dependencies
}
\description{
  Given a list of packages, add all their dependencies.
}
\examples{
# Create some packages to use
ggplot2 <- list(package_info(default_sources(), "ggplot2"))
scales <- list(package_info(default_sources(), "scales"))
abind <- list(package_info(default_sources(), "abind"))

deps <- add_dependencies(ggplot2)
length(deps)
sapply(deps, is.installed)

# Base packages are never included in the list of dependencies
sapply(deps, is.base)

length(add_dependencies(scales))
length(add_dependencies(c(ggplot2, scales)))
length(add_dependencies(c(ggplot2, abind)))

# Supplying a different set of sources will determine whether or not
# packages are already installed.
deps2 <- add_dependencies(ggplot2, sources = default_sources(TRUE))
sapply(deps2, is.installed)
}


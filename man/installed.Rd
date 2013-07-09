\name{installed}
\alias{installed}
\title{Package source: installed packages}
\usage{
  installed(paths = .libPaths())
}
\arguments{
  \item{paths}{A character vector of library paths.}
}
\description{
  This source represents packages that are already
  installed on-disk. By default it uses all libraries found
  in \code{\link{.libPaths}}.
}
\examples{
inst <- installed()
inst
has_package(inst, "ggplot2")
package_info(inst, "MASS")
package_url(inst, "lattice")
}


\name{installed}
\alias{base}
\alias{installed}
\title{Package source: installed packages}
\usage{
  installed(paths = head(.libPaths(), -1))

  base()
}
\arguments{
  \item{paths}{A character vector of library paths.}
}
\description{
  This source represents packages that are already
  installed on-disk. By default it uses all libraries found
  in \code{\link{.libPaths}}. Installed packages are
  divided into base and other libraries to avoid every
  reinstalling base packages (which is a bad idea).
}
\examples{
inst <- installed()
inst
has_package(inst, "ggplot2")

package_info(base(), "MASS")
package_url(base(), "lattice")
}


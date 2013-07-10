\name{installed}
\alias{base}
\alias{installed}
\title{Package source: installed packages}
\usage{
  installed(paths = .libPaths())

  base()
}
\arguments{
  \item{paths}{A character vector of library paths.
  \code{.Library} is always removed as it is provided by
  the base source.}
}
\description{
  This source represents packages that are already
  installed on-disk. By default it uses all libraries found
  in \code{\link{.libPaths}}. Installed packages are
  divided into base and other libraries to avoid ever
  reinstalling base packages (which is a bad idea).
}
\examples{
inst <- installed()
inst
has_package(inst, "ggplot2")

package_info(base(), "MASS")
package_url(base(), "lattice")
}


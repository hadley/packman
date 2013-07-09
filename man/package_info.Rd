\name{package_info}
\alias{package_info}
\title{Get information about a package.}
\usage{
  package_info(source, package)
}
\value{
  a named list of fields, or \code{NULL} if package is not
  found.
}
\description{
  Depending on the source, this is some subset of the
  fields in the packages \code{DESCRIPTION}
}
\examples{
offline_packages()
package_info(cran("source"), "ggplot2")
package_info(cran("binary"), "ggplot2")
package_info(installed(), "ggplot2")
package_info(github(), "ggplot2")

package_info(c(cran("source"), installed()), "abind")
}


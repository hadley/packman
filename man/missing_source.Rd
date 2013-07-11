\name{missing_source}
\alias{missing_source}
\title{A source that represents missing packages.}
\usage{
  missing_source()
}
\description{
  A source that represents missing packages.
}
\examples{
ms <- missing_source()
has_package(ms, "mypackage")
package_info(ms, "mypackage")
\dontrun{install(ms, "mypackage")}
}


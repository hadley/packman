\name{test_source}
\alias{test_source}
\title{Testing source}
\usage{
  test_source(path = system.file("test-source", package = "packman"))
}
\arguments{
  \item{path}{path to directory containing \code{.dcf}
  files}
}
\description{
  This makes it easy to convert a directory of DESCRIPTION
  files named with \code{packagename.dcf} in a source
  useful for testing.
}
\examples{
s <- test_source()
s
has_package(s, "a")
package_info(s, "a")
}


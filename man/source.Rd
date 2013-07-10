\name{source}
\alias{source}
\title{A single package source.}
\usage{
  source(subclass, ...)
}
\description{
  A package source represents a source of packages
  available to be installed (or in the case of
  \code{\link{installed}} already installed).
}
\section{Important subclasses
As a user of packman, you will not call this function directly, but should
instead use a function like \code{\link{cran}}, \code{\link{github}} or
\code{\link{installed}}.}{

}

\section{Important methods}{
  All subclasses of source should implement: \itemize{
  \item \code{\link{package_info}} \item
  \code{\link{has_package}} \item \code{\link{package_url}}
  \item \code{\link{install}} }
}
\keyword{internal}


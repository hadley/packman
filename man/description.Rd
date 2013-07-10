\name{description}
\alias{as.description}
\alias{description}
\alias{is.description}
\title{Description object (S3): creation, coercion and testing.}
\usage{
  description(...)

  is.description(x)

  as.description(x, source)
}
\arguments{
  \item{...}{fields that become components of the package.
  These are currently not verified in any way.}

  \item{x}{}

  \item{source}{the data source from which the package
  came}
}
\description{
  This object represents a package \code{DESCRIPTION} file.
}


\name{github}
\alias{github}
\title{Package source: github}
\usage{
  github(username = "hadley", repo = NULL, ref = "master",
    subdir = NULL, auth_user = NULL, password = NULL)
}
\arguments{
  \item{username}{This is the only parameter that must be
  specified, and for the recommended package structure on
  github it is the only parameter that should be specified}

  \item{repo}{The repo name - if ommited, the package name
  is used}

  \item{ref}{The branch/tag name to use - if omitted, will
  look in master, and in all appropriately named tags}

  \item{subdir}{If the package is not found in the root
  directory of the package this specifies where it should
  be found.}

  \item{auth_user}{If authentication is needed, and the
  authentication username is different to the
  \code{username}}

  \item{password}{The password to use for authentication}
}
\description{
  Package source: github
}
\examples{
h <- github("hadley")
has_package(h, "ggplot2")
has_package(h, "ggplot3")
package_info(h, "ggplot2")

w <- github("wch")
has_package(w, "extrafont")
has_package(c(h, w), "extrafont")
}


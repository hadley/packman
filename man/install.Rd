\name{install}
\alias{install}
\alias{install.source}
\title{Install a package from source.}
\usage{
  install(source, package, library, ...)

  \method{install}{source} (source, package, library, ...)
}
\description{
  Install a package from source.
}
\section{Default method
The default method downloads the package from \code{\link{package_url}},
builds it if needed (i.e. if it's a source package), and then installs it
with \code{\link{install_binary_package}}.}{

}


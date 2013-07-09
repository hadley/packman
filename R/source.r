#' A single package source.
#' 
#' A package source represents a source of packages available to be installed
#' (or in the case of \code{\link{installed}} already installed). 
#' 
#' @section Important subclasses
#' As a user of packman, you will not call this function directly, but should
#' instead use a function like \code{\link{cran}}, \code{\link{github}} or
#' \code{\link{installed}}. 
#' 
#' @section Important methods:
#' All subclasses of source should implement:
#' \itemize{
#'   \item \code{\link{package_info}}
#'   \item \code{\link{has_package}}
#'   \item \code{\link{package_url}}
#'   \item \code{\link{install}}
#' }
#' @export
#' @keywords internal
source <- function(subclass, ...) {
  structure(
    list(...),
    class = c(subclass, "source")
  )
}

#' @export
is.source <- function(x) inherits(x, "source")

#' @S3method c source
c.source <- function(...) {
  sources(...)
}

#' Get information about a package.
#'
#' Depending on the source, this is some subset of the fields in the packages
#' \code{DESCRIPTION}
#'
#' @return a named list of fields, or \code{NULL} if package is not found.
#' @export
#' @examples
#' offline_packages()
#' package_info(cran("source"), "ggplot2")
#' package_info(cran("binary"), "ggplot2")
#' package_info(installed(), "ggplot2")
#' package_info(github(), "ggplot2")
#'
#' package_info(c(cran("source"), installed()), "abind")
package_info <- function(source, package) {
  UseMethod("package_info")
}

#' Does the source provide a package?
#'
#' @export
#' @examples
#' offline_packages()
#' has_package(cran("source"), "ggplot2")
#' has_package(cran("binary"), "ggplot2")
#' has_package(installed(), "ggplot2")
#' has_package(cran("source"), "ggplot4")
#' has_package(cran("binary"), "ggplot4")
#' has_package(installed(), "ggplot4")
has_package <- function(source, package, version = NULL) {
  UseMethod("has_package")
}

#' Install a package from source.
#' 
#' @section Default method
#' The default method downloads the package from \code{\link{package_url}},
#' builds it if needed (i.e. if it's a source package), and then installs it
#' with \code{\link{install_binary_package}}.
#' 
#' @export
install <- function(source, package, library, ...) {
  UseMethod("install")
}

#' @method install source
#' @export
#' @rdname install
install.source <- function(source, package, library, ...) {
  src <- package_url(source, package)

  dest <- file.path(tempdir(), basename(src))
  if (!file.exists(dest)) {
    download.file(src, dest, quiet = TRUE)
  }

  if (file_ext(dest) == ".tar.gz") {
    built <- build_package(dest, ...)
    install_binary_package(built, library, ...)
  } else {
    install_binary(dest, library, ...)
  }
}

#' Find the location of a package in a given source.
#'
#' @export
package_url <- function(source, package) {
  UseMethod("package_url")
}

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
has_package <- function(source, package) {
  UseMethod("has_package")
}
has_package.sources <- function(source, package) {
  for(single in source) {
    if (has_package(single, package)) {
      return(TRUE)
    }
  }
  FALSE
}

install <- function(source, package) {
  UseMethod("install")
}

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

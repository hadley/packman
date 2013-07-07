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

#' @export
#' @keywords internal
source_list <- function(..., .sources = list()) {
  source1 <- lapply(list(...), as.source_list)
  source2 <- list(as.source_list(.sources))

  sources <- unlist(c(source1, source2), recursive = FALSE, use.names = FALSE)
  structure(sources, class = "source_list")
}
#' @S3method c source_list
c.source_list <- function(...) {
  source_list(...)
}
#' @S3method c source
c.source <- function(...) {
  source_list(...)
}

#' @export
as.source_list <- function(x) UseMethod("as.source_list")
#' @S3methos as.source_list source
as.source_list.source <- function(x) list(x)
#' @S3methos as.source_list source_list
as.source_list.source_list <- function(x) x
#' @S3methos as.source_list source_list
as.source_list.list <- function(x) x

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

package_info.source_list <- function(source, package) {
  for(single in source) {
    if (has_package(single, package)) {
      return(package_info(single, package))
    }
  }
  NULL
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

install <- function(source, package) {
  UseMethod("install")
}
#' Package source: installed packages
#' 
#' This source represents packages that are already installed on-disk. By
#' default it uses all libraries found in \code{\link{.libPaths}}. Installed
#' packages are divided into base and other libraries to avoid ever
#' reinstalling base packages (which is a bad idea).
#' 
#' @param paths A character vector of library paths.  \code{.Library} is always
#'   removed as it is provided by the base source.
#' @export
#' @examples
#' inst <- installed()
#' inst
#' has_package(inst, "ggplot2")
#' 
#' package_info(base(), "MASS")
#' package_url(base(), "lattice")
installed <- function(paths = .libPaths()) {
  paths <- setdiff(normalizePath(paths), normalizePath(.Library))
  source("installed", paths = paths)
}

#' @S3method print installed
print.installed <- function(x, ...) {
  cat("<Source> Installed packages: \n", paste0("* ", x$paths, "\n"), sep = "")
}

#' @S3method package_info installed
package_info.installed <- function(source, package) {
  path <- find.package(package, source$paths, quiet = TRUE)
  if (length(path) != 1) return(NULL)
  
  read_dcf(file.path(path, "DESCRIPTION"), source)
}

#' @S3method has_package installed
has_package.installed <- function(source, package, version = NULL) {
  if (is.null(version)) {
    length(find.package(package, source$paths, quiet = TRUE)) == 1  
  } else {
    info <- package_info(source, package)
    compare_versions(info$Version, version)
  } 
}

#' @S3method install installed
install.installed <- function(source, package) {
  TRUE
}

#' @S3method package_url installed
package_url.installed <- function(source, package) {
  path <- find.package(package, source$paths, quiet = TRUE)
  paste0("file://", path)
}

#' @rdname installed
#' @export
base <- function() {
  path <- normalizePath(.Library)
  source(c("base", "installed"), paths = path)
}

is.base <- function(x) inherits(x, "base")

#' @S3method install base 
install.base <- function(source, package) {
  warning("Skipping re-installation of base package ", package, call. = FALSE)
  TRUE
}
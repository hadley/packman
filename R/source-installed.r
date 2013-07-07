#' Package source: installed packages
#' 
#' @param paths A character vector of library paths.
#' @export
#' @examples
#' installed()
installed <- function(paths = .libPaths()) {
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
  
  dcf <- read.dcf(file.path(path, "DESCRIPTION"))
  as.list(as.data.frame(dcf, stringsAsFactors = FALSE))
}

#' @S3method has_package installed
has_package.installed <- function(source, package) {
  length(find.package(package, source$paths, quiet = TRUE)) == 1
}

install.installed <- function(source, package) {
  TRUE
}
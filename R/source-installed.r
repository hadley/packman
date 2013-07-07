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
#' Default package sources
#' 
#' @details
#' These are (in order):
#' 
#' \itemize{
#'   \item \code{\link{installed}()}, if \code{force} is \code{FALSE}.
#'   \item \code{\link{cran}("source")}, if a development environment is
#'     available.
#'   \item \code{\link{cran}("binary)}, if binary packages are available for
#'     the current platform.
#' }
#' @param force if \code{TRUE}, will not include currently installed packages 
#'   in the list, and hence will force a reinstall of all packages, even those
#'   that are already installed.
#' @export
#' @examples
#' default_sources()
default_sources <- function(force = FALSE) {
  sources <- list()
  
  if (!force) {
    sources$installed <- installed()
  }
  
  if (.Platform$pkgType != "source") {
    sources$binary <- cran("binary")
  }
  
  if (has_dev_env()) {
    sources$source <- cran("source")
  }
  
  source_list(sources)
}
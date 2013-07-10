#' Default package sources
#'
#' @details
#' These are (in order):
#'
#' \itemize{
#'   \item \code{\link{installed}()}, if \code{force} is \code{FALSE}.
#'   \item \code{\link{cran}("source")}, if a development environment is
#'     available.
#'   \item \code{\link{cran}("binary")}, if binary packages are available for
#'     the current platform.
#' }
#' @param force if \code{TRUE}, will not include currently installed packages
#'   in the list, and hence will force a reinstall of all packages, even those
#'   that are already installed.
#' @export
#' @examples
#' def <- default_sources()
#' def
#' package_info(def, "ggplot2")
default_sources <- function(force = FALSE) {
  sources <- list(recommended())
  add_source <- function(x) {
    sources <<- c(sources, list(x))
  }

  if (!force) {
    add_source(installed())
  }

  if (.Platform$pkgType != "source") {
    add_source(cran("binary"))
  }

  if (has_dev_env()) {
    add_source(cran("source"))
  }

  sources(.sources = sources)
}

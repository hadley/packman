#' Package source: github
#' 
#' @param username This is the only parameter that must be specified, and for
#'   the recommended package structure on github it is the only parameter that
#'   should be specified
#' @param repo The repo name - if ommited, the package name is used
#' @param ref The branch/tag name to use - if omitted, will look in master,
#'   and in all appropriately named tags
#' @param subdir If the package is not found in the root directory of the 
#'   package this specifies where it should be found.
#' @param auth_user If authentication is needed, and the authentication username 
#'   is different to the \code{username}
#' @param password The password to use for authentication
#' @export
#' @examples
#' h <- github("hadley")
#' has_package(h, "ggplot2")
#' has_package(h, "ggplot3")
#' package_info(h, "ggplot2")
#' 
#' w <- github("wch")
#' has_package(w, "extrafont")
#' has_package(c(h, w), "extrafont")
github <- function(username = "hadley", repo = NULL, ref = "master", subdir = NULL, 
                   auth_user = NULL, password = NULL) {
  
  if (!is.null(auth_user)) {
    auth <- authenticate(auth_user, auth_pass)
  } else {
    auth <- list()
  }
  
  source("github", username = username, repo = repo, ref = ref, subdir = subdir, 
         auth = auth)
}

#' @S3method print github
print.github <- function(x, ...) {
  cat("<Source> Github: ", x$username, sep = "")
  if (!is.null(x$password)) cat(" (with authentication)")
  cat("\n")
}

#' @S3method has_package github
has_package.github <- function(source, package, version = NULL) {
  if (is.null(version)) {
    url_ok(description_url(source, package), source$auth)
  } else {
    info <- package_info(source, package)
    compare_versions(info$Version, version)
  }
} 

#' @S3method package_info github
package_info.github <- function(source, package) {
  url <- description_url(source, package)
  
  desc <- cache_url(url, source$auth)
  read_dcf(textConnection(desc), source)
}

#' @S3method package_url github
package_url.github <- function(source, package) {
  paste0(base_url(source, "archive", package), ".zip")
}

#' @S3method install github
install.github <- function(source, package, library, ...) {
  url <- package_url(source, package)
  install_url(url, package, subdir = source$subdir, config = source$auth,
              library = library, ...)
}

description_url <- function(x, package) {
  paste0(base_url(x, "raw", package), "/DESCRIPTION")
}

base_url <- function(x, base_dir, package) {
  paste0("https://github.com/", x$username, "/", x$repo %||% package, 
         "/", base_dir, "/", x$ref)
}

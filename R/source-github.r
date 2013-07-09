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
github <- function(username, repo = NULL, ref = "master", subdir = NULL, 
                   auth_user = NULL, password = NULL) {
  
  source("github", username = username, repo = repo, ref = ref, subdir = subdir, 
         auth_user = auth_user, password = password)
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
    url_ok(description_url(source, package))
  } else {
    info <- package_info(source, package)
    compare_versions(info$Version, version)
  }
} 

#' @S3method package_info github
package_info.github <- function(source, package) {
  url <- description_url(source, package)
  
  desc <- cache_url(url)
  read_dcf(textConnection(desc))
}

# https://github.com/hadley/ggplot2/raw/master/DESCRIPTION
description_url <- function(x, package) {
  paste0(base_url(x, "raw", package), "/DESCRIPTION")
}
base_url <- function(x, base_dir, package) {
  paste0("https://github.com/", x$username, "/", x$repo %||% package, 
         "/", base_dir, "/", x$ref)
}
cache_url <- memoise(function(url) {
  req <- GET(url)
  stop_for_status(req)
  content(req, as = "text")
})

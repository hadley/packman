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
#' github("hadley")
#' github("wch")
github <- function(username, repo = NULL, ref = NULL, subdir = NULL, 
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
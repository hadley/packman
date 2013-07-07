#' Check that the installed version of a package satisfies the requirements
#'
#' @param dep_name The name of the package with objects to import
#' @param dep_ver The version of the package
#' @param dep_compare The comparison operator to use to check the version
#' @keywords internal
dep_installed <- function(dep_name, dep_compare = NA, dep_ver = NA) {
  if (xor(is.na(dep_ver), is.na(dep_compare))) {
    stop("dep_ver and dep_compare must be both NA or both non-NA")
  }

  if (!is_installed(dep_name)) {
    return("not installed")
  }

  # No version check
  if(is.na(dep_ver) && is.na(dep_compare)) {
    return("OK")
  }

  dep_var <- as.numeric_version(dep_ver)
  installed_ver <- packageVersion(dep_name)
  compare <- match.fun(dep_compare)

  if (!compare(installed_ver, dep_var)) {
    return(paste0("need ", dep_compare, " ", dep_ver,
      " but installed is ", installed_ver))
  }

  return("OK")
}

is_installed <- function(x) {
  length(find.package(x, quiet = TRUE)) > 0
}

#' Parse package dependency strings.
#'
#' @param string to parse. Should look like \code{"R (>= 3.0), ggplot2"} etc.
#' @return data.frame with two character variables: \code{name} package names,
#'   and \code{version} package versions. If version is not specified,
#'   it will be stored as NA.
#' @keywords internal
#' @export
#' @examples
#' parse_deps("httr (< 2.1),\nRCurl (>= 3)")
#' # only package dependencies are returned
#' parse_deps("utils (== 2.12.1),\ntools,\nR (>= 2.10),\nmemoise")
parse_deps <- function(string) {

  if (is.null(string) || all(is.na(string))) {
    return(data.frame(
      name = character(),
      version = character(),
      stringsAsFactors = FALSE))
  }
  stopifnot(is.character(string), length(string) == 1)

  pieces <- strsplit(string, ",\\s*")[[1]]

  # Get the names
  names <- gsub("\\s*\\(.*?\\)", "", pieces)
  names <- gsub("^\\s+|\\s+$", "", names)

  # And the versions
  version_re <- "^.*\\((.+)\\)$"
  versions <- sub(version_re, "\\1", pieces)
  versions[!grepl(version_re, pieces)] <- NA_character_

  deps <- data.frame(name = names, version = versions, stringsAsFactors = FALSE)

  # Remove R dependency
  deps[names != "R", ]
}

compare_versions <- function(target, expr = NULL) {
  if (is.null(expr)) return(TRUE)

  compare  <- sub("^(\\S+)\\s+.*$", "\\1", expr)[[1]]
  if (!(compare %in% c(">", ">=", "==", "<=", "<"))) {
    stop("Invalid comparison operator: ", compare)
  }
  compare_f <- match.fun(compare)
  
  version <- sub("^\\S+\\s+(.*)$", "\\1", expr)[[1]]
  ver <- numeric_version(version)
  
  compare_f(target, ver)
}

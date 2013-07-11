#' Testing source
#' 
#' This makes it easy to convert a directory of DESCRIPTION files named with
#' \code{packagename.dcf} in a source useful for testing.
#' 
#' @param path path to directory containing \code{.dcf} files
#' @export
#' @examples
#' s <- test_source(tester())
#' s
#' has_package(s, "a")
#' package_info(s, "a")
test_source <- function(path) {
  assert_that(is.dir(path))
  source("test", path = path)
}

tester <- function() system.file("test-source", package = "packman")

test_path <- function(x, package) {
  file.path(x$path, paste0(package, ".dcf"))
}

#' @S3method print test
print.test <- function(x, ...) {
  cat("<Source> Test: ", x$path, "\n", sep = "")
}

#' @S3method has_package test
has_package.test <- function(source, package, version) {
  if (is.null(version)) {
    file.exists(test_path(source, package))
  } else {
    info <- package_info(source, package)
    compare_versions(info$Version, version)
  }
}

#' @S3method package_info test
package_info.test <- function(source, package) {
  as.description(file(test_path(source, package)), source) 
}

#' @S3method install test
install.test <- function(source, package) {
  TRUE
}

#' @S3method package_url test
package_url.test <- function(source, package) {
  paste0("file://", test_path(source, package))
}
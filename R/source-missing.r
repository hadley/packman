#' A source that represents missing packages.
#' 
#' @export
#' @examples
#' ms <- missing_source()
#' has_package(ms, "mypackage")
#' package_info(ms, "mypackage")
#' \dontrun{install(ms, "mypackage")}
missing_source <- function() {
  source("missing_source")
}
#' @S3method print missing_source
print.missing_source <- function(x, ...) {
  cat("<Source> MISSING\n")
}
#' @S3method install missing_source
install.missing_source <- function(source, package, ...) {
  stop("Could not find source for package ", package, call. = FALSE)
}
#' @S3method has_package missing_source
has_package.missing_source <- function(source, package) FALSE
#' @S3method package_info missing_source
package_info.missing_source <- function(source, package) {
  description(Package = package, source = source)
}
#' @S3method package_url missing_source
package_url.missing_source <- function(source, package) NA_character_

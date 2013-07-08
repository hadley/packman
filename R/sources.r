#' @export
#' @keywords internal
sources <- function(..., .sources = list()) {
  source1 <- lapply(list(...), as.sources)
  source2 <- list(as.sources(.sources))

  sources <- unlist(c(source1, source2), recursive = FALSE, use.names = FALSE)
  structure(sources, class = "sources")
}
#' @S3method c sources
c.sources <- function(...) {
  sources(...)
}

as.sources <- function(x) UseMethod("as.sources")
#' @S3method as.sources source
as.sources.source <- function(x) list(x)
#' @S3method as.sources sources
as.sources.sources <- function(x) x
#' @S3method as.sources sources
as.sources.list <- function(x) x


package_info.sources <- function(source, package) {
  for(single in source) {
    if (has_package(single, package)) {
      return(package_info(single, package))
    }
  }
  NULL
}

has_package.sources <- function(source, package) {
  for(single in source) {
    if (has_package(single, package)) {
      return(TRUE)
    }
  }
  FALSE
}
#' Description object (S3): creation, coercion and testing.
#' 
#' This object represents a package \code{DESCRIPTION} file.
#' 
#' @param ... fields that become components of the package. These are currently
#'   not verified in any way.
#' @export
description <- function(...) {
  structure(list(...), class = "description")
}

#' @S3method print description
print.description <- function(x, ...) {
  cat("<Package> ", x$Package, "\n", sep = "")
}

#' @export
#' @rdname description
is.description <- function(x) inherits(x, "description")

#' @param x
#' @param source the data source from which the package came
#' @export 
#' @rdname description
as.description <- function(x, source) {
  assert_that(is.source(source))
  UseMethod("as.description")
}

#' @S3method as.description character
as.description.character <- function(x, source) {
  assert_that(length(x) == 1)
  
  as.description(textConnection(x), source)
}

#' @S3method as.description connection
as.description.connection <- function(x, source) {
  already_open <- isOpen(x)
  if (!already_open) on.exit(close(x))
  
  dcf <- read.dcf(x)
  obj <- as.list(as.data.frame(dcf, stringsAsFactors = FALSE))
  as.description(obj, source)
}

#' @S3method as.description list
as.description.list <- function(x, source) {
  x$source <- source
  
  do.call("description", x) 
}

#' @S3method install description
install.description <- function(source, package, library, ...) {
  install(package$source, package, library, ...)
}

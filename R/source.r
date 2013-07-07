#' @export
#' @keywords internal
source <- function(subclass, ...) {
  structure(
    list(...),
    class = c(subclass, "source")
  )
}

#' @export
is.source <- function(x) inherits(x, "source")

#' @export
#' @keywords internal
source_list <- function(..., .sources = list()) {
  source1 <- lapply(list(...), as.source_list)
  source2 <- list(as.source_list(.sources))

  sources <- unlist(c(source1, source2), recursive = FALSE, use.names = FALSE)
  structure(sources, class = "source_list")
}
#' @S3method c source_list
c.source_list <- function(...) {
  source_list(...)
}
#' @S3method c source
c.source <- function(...) {
  source_list(...)
}

#' @export
as.source_list <- function(x) UseMethod("as.source_list")
#' @S3methos as.source_list source
as.source_list.source <- function(x) list(x)
#' @S3methos as.source_list source_list
as.source_list.source_list <- function(x) x
#' @S3methos as.source_list source_list
as.source_list.list <- function(x) x


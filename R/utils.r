last <- function(x) x[length(x)]

"%||%" <- function(a, b) if (is.null(a)) b else a

read_dcf <- function(x, ...) {
  as.list(as.data.frame(read.dcf(x, ...), stringsAsFactors = FALSE))
}
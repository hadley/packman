last <- function(x) x[length(x)]

"%||%" <- function(a, b) if (is.null(a)) b else a

compact <- function(x) Filter(Negate(is.null), x)

read_dcf <- function(x, source, ...) {
  dcf <- read.dcf(x, ...)
  obj <- as.list(as.data.frame(dcf, stringsAsFactors = FALSE))
  obj$source <- source
  obj
}

cache_url <- memoise(function(url, ...) {
  req <- GET(url, ...)
  stop_for_status(req)
  content(req, as = "text")
})

last <- function(x) x[length(x)]

"%||%" <- function(a, b) if (is.null(a)) b else a

compact <- function(x) Filter(Negate(is.null), x)

cache_url <- memoise(function(url, ...) {
  req <- GET(url, ...)
  stop_for_status(req)
  content(req, as = "text")
})

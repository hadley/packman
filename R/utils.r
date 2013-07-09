last <- function(x) x[length(x)]

"%||%" <- function(a, b) if (is.null(a)) b else a
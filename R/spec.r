# parse_spec("a(")
# parse_spec("stop('error')")
#
# parse_spec("c(1, 2, 3)")
# parse_spec("cran()")
# parse_spec("data.frame()")
#
# parse_spec("list(1, 2, 3)")
# parse_spec("list(cran())")
# parse_spec("list(a = cran(), cran())")
#
# parse_spec('list("ggplot2" = cran())')
# parse_spec('list("ggplot2" = github("hadley"))')
# parse_spec('list("ggplot2" = c(github("hadley"), cran()))')

#' @export
parse_spec <- function(spec) {
  tryCatch(
    expr <- parse(text = spec),
    error = function(e) {
      stop("Failed to parse spec:\n", e$message, call. = FALSE)    
    }
  )
  tryCatch(
    obj <- eval(expr, as.environment("package:packman")),
    error = function(e) {
      stop("Failed to evaluate spec:\n", e$message, call. = FALSE)    
    }
  )
  
  if (!is.list(obj) || !is.vector(obj)) {
    stop("Spec must be a regular list", call. = FALSE)
  }
  
  is_source <- vapply(obj, is.source, logical(1))
  if (any(!is_source)) {
    stop("All components of the spec must be a source object", call. = FALSE)
  }
  
  nms <- names(obj)
  if (is.null(nms) || any(nms == "")) {
    stop("All components of spec must be named", call. = FALSE)
  }
  
  obj
}
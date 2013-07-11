#' Package dependency topological sort.
#' 
#' This is implemented via a naive depth-first search - it will not warn
#' if there are cycles in the dependency graph.
#' 
#' @param packages a named list of packages that should be sorted topologically
#'   according to their dependencies, 
#' @return a named list of packages ordered in a way that they can be
#'   sequentially installed from source.
#' @export
#' @keywords internal
#' @examples
#' pkgs <- list(
#'   a = package_info(test_source(), "a"),
#'   b = package_info(test_source(), "b")
#' )
#' topo_sort(pkgs)
topo_sort <- function(packages) {
  assert_that(is.list(packages))
  
  seen <- character()
  sorted <- character()
  
  visit <- function(pkg_name) {
    if (pkg_name %in% seen) return()
    seen <<- c(seen, pkg_name)
    
    pkg <- packages[[pkg_name]]
    assert_that(is.description(pkg))
    if (is.null(pkg)) return()    

    for (dep_name in pkg$.dependencies$name) {
      visit(dep_name)
    }
    sorted <<- c(sorted, pkg_name)

    invisible()
  }
  
  for (pkg_name in names(packages)) {
    visit(pkg_name)
  }
  packages[sorted]
}

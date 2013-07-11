#' Given a list of packages, add all their dependencies.
#' 
#' @param pkg a character vector of package names
#' @param from which DESCRIPTION fields to use to compute dependencies.
#'    Defaults to depends, imports and linking to
#' @param sources The sources in which to look for \code{pkg}. These will also
#'    be used for dependencies of \code{pkg}, as well as any additional 
#'    sources described in individual packages.
#' @return a list of packages topologically sorted by their dependencies
#' @export
#' @examples
#' # Create some packages to use
#' ggplot2 <- list(package_info(default_sources(), "ggplot2"))
#' scales <- list(package_info(default_sources(), "scales"))
#' abind <- list(package_info(default_sources(), "abind"))
#' 
#' deps <- add_dependencies(ggplot2)
#' length(deps)
#' sapply(deps, is.installed)
#'
#' # Base packages are never included in the list of dependencies
#' sapply(deps, is.base)
#' 
#' length(add_dependencies(scales))
#' length(add_dependencies(c(ggplot2, scales)))
#' length(add_dependencies(c(ggplot2, abind)))
#' 
#' # Supplying a different set of sources will determine whether or not
#' # packages are already installed.
#' deps2 <- add_dependencies(ggplot2, sources = default_sources(TRUE))
#' sapply(deps2, is.installed)
add_dependencies <- function(pkgs = NULL, 
                              from = c("Depends", "Imports", "LinkingTo"),
                              sources = default_sources()) {
  assert_that(is.list(pkgs) && is.vector(pkgs))
  assert_that(is.character(from), length(from) > 0)
  assert_that(is.source(sources))
  
  all_deps <- list()
  seen <- character()

  visit <- function(pkg) {
    if (pkg$Package %in% seen) return()
    seen <<- c(seen, pkg$Package)
    
    deps <- package_deps(pkg, from, sources = sources)
    # Remove base packages since by definition all their dependencies
    # are available
    deps <- Filter(function(x) !is.base(x$source), deps)

    for(dep in deps) {
      visit(dep)
    }

    all_deps[[pkg$Package]] <<- pkg
  }
  
  for (pkg in pkgs) {
    visit(pkg)
  }
  
  all_deps
}

#' Find package info for dependencies.
#' 
#' @param info a list containing package information
#' @param from which fields to consider dependencies from
#' @keywords internal
#' @examples
#' ggplot2 <- package_info(default_sources(), "ggplot2")
#' package_deps(ggplot2)
package_deps <- function(info, from = c("Depends", "Imports", "LinkingTo"), 
                         sources = default_sources()) {
  pkg_sources <- info$.sources
  
  # Parse all dependences into single dataframe
  deps <- info$.dependencies
  deps <- deps[deps$field %in% from, , drop = FALSE]
  
  # For each package, find the info that conforms to the spec
  pkgs <- setNames(deps$name, deps$name)
  
  # TODO: use version restriction as well!
  lapply(pkgs, function(pkg) {
    source <- c(sources, pkg_sources$default, sources[[pkg]])
    package_info(source, pkg) %||% package_info(missing_source(), pkg)
  })
}

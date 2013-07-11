#' Find all (recursive) dependencies of a package.
#' 
#' @param pkg a single package name
#' @param from which DESCRIPTION fields to use to compute dependencies.
#'    Defaults to depends, imports and linking to
#' @param sources The sources in which to look for \code{pkg}. These will also
#'    be used for dependencies of \code{pkg}, as well as any additional 
#'    sources described in individual packages.
#' @export
#' @examples
#' deps <- find_dependencies("ggplot2")
#' sapply(deps, is.installed)
#' 
#' # Base packages are never included in the list of dependencies
#' sapply(deps, is.base)
#' 
#' # Supplying a different set of sources will determine whether or not
#' # packages are already installed.
#' deps2 <- find_dependencies("ggplot2", sources = default_sources(TRUE))
#' sapply(deps2, is.installed)
find_dependencies <- function(pkg = NULL, 
                              from = c("Depends", "Imports", "LinkingTo"),
                              sources = default_sources()) {
  assert_that(is.string(pkg))
  assert_that(is.character(from), length(from) > 0)
  assert_that(is.source(sources))
  
  info <- package_info(sources, pkg)
  if (is.null(info)) stop("Couldn't find info about ", pkg, call. = FALSE)
  
  all_deps <- list()
  seen <- character()

  to_see <- list()
  to_see[[pkg]] <- info
  
  while (length(to_see) > 0) {
    next_pkg <- to_see[[1]]
    deps <- package_deps(next_pkg, from, sources = sources)

    # Remove base packages since by definition all their dependencies
    # are available
    deps <- Filter(function(x) !is.base(x$source), deps)
    
    new_deps <- deps[setdiff(names(deps), seen)]
    # Maybe this should actually be deps, and then a second pass should go
    # through and collapse - currently only the first dependency is 
    # captured
    all_deps <- c(all_deps, new_deps)
    to_see <- c(to_see[-1], new_deps)

    seen <- c(seen, next_pkg$Package)
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
  pkg_sources <- parse_spec(info$Sources)
  
  # Parse all dependences into single dataframe
  parsed <- lapply(info[from], parse_deps)
  deps <- do.call("rbind", unname(parsed))
  
  # For each package, find the info that conforms to the spec
  pkgs <- setNames(deps$name, deps$name)
  
  # TODO: use version restriction as well!
  lapply(pkgs, function(pkg) {
    source <- c(sources, pkg_sources$default, sources[[pkg]])
    package_info(source, pkg) %||% package_info(missing_source(), pkg)
  })
}

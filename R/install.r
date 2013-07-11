#' Install specified packages into a library.
#' 
#' @section Atomicity:
#' 
#' If it is not possible to satisfy the dependency with the available sources,
#' the installation process will before any packages are replaced. Otherwise,
#' installation of an individual package either succeeds or fails - it never
#' ends up in a partially installed state. When installing many packages, 
#' however, some packages may succeed before the first failure.
#' 
#' @param package_names character vector containing names of packages to install
#' @param library library (path) in which to install package
#' @param reinstall if \code{TRUE} will force reinstallation of dependent 
#'   packages that are already installed. If \code{FALSE}, only missing packages
#'   will be installed. The packages listed in \code{package_names} will always
#'   be installed.
#' @param sources a list of package sources. See \code{\link{default_sources}}
#'   for more details about which sources are used by default. Additionally,
#'   individual (non-CRAN) packages can provide additional sources in their 
#'   description files.
#' @returns (Invisibly) package information for downloaded packages.
install_packages <- function(package_names, library = .libPaths()[1], 
                             reinstall = FALSE, 
                             sources = default_sources(force = reinstall)) {
  assert_that(is.character(packages))
  assert_that(is.dir(library), is.writeable(library))
  assert_that(is.flag(force))
  assert_that(is.source(sources))
  
  # Convert names to packages and add dependencies. 
  packages <- lapply(package_names, function(x) package_info(x, sources))
  packages <- add_dependencies(packages, sources = sources)

  if (reinstall) {
    to_install <- packages
  } else {
    to_install <- Filter(Negate(is.installed), packages)  
  }
  
  # TODO: check that the packages aren't missing
  
  if (!quiet) {
    message("Installing packages ", paste(names(to_install), collapse = ", "))
  }

  # TODO: unload loaded packages
  # TODO: special case for packman (so you can reinstall it)
  
  for (pkg in to_install) {
    install(pkg, library = library, quiet = quiet)
  }
  invisible(to_install)
}

# Needs to restore to previous state on failure
install_package <- function(path, library, quiet = FALSE) {
  assert_that(is.string(path))
  assert_that(is.dir(library), is.writeable(library))
  
  if (!quiet) {
    message("Installing ", basename(path), " to ", library)
  }
  
  # Decompresses file (ignoring extension)
  unzipped <- decompress(package)
  on.exit(unlink(unzipped))
  
  # Check that it's ok (if possible)
  check_checksums(unzipped)
  
  # Build, if needed
  if (is_binary_package(unzipped)) {
    unzipped <- build_package(unzipped)
  }
  
  desc <- as.description(file(file.path(unzipped, "DESCRIPTION")))

  # If previously installed, copy permissions and then move to tempdir
  # TODO: figure out how to make atomic, so that if file.rename fails, 
  #  the whole process fails.
  inst_path <- file.path(library, desc$Package)
  if (file.exists(inst_path)) {
    if (!is_binary_package(inst_path)) {
      stop("Installing will replace ", inst_path, " but that path is not a ", 
           "package", call. = FALSE)
    }
    
    copy_permissions(inst_path, unzipped)
    file.rename(inst_path, tempdir())
  }
  
  file.copy(unzipped, inst_path, recursive = TRUE)
  invisible(inst_path)
}

#' Is path a binary (or source) package?
#' 
#' @param path path to putative package
#' @return \code{TRUE} if a binary package, \code{FALSE} if a source package,
#'   otherwise will throw an informative error message
is_binary_package <- function(path) {
  assert_that(is.dir(path), is.readable(path))
  
  desc_path <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc_path)) {
    stop(path, " is not a package (does not contain a DESCRIPTION file)", 
         call. = FALSE)
  }
  
  package_path <- file.path(unzipped, "Meta", "package.rds")
  if (!file.exists(package_path)) {
    # Must be a source package, and there's no way to verify it's correct
    # except to install it.
    return(FALSE)
  }
    
  desc <- as.list(readRDS(package_path)$DESCRIPTION)
  if (length(desc) < 1L) {
    stop(package, " is not a valid binary package (corrupt Meta/package.rds)")
  }
  if (is.null(desc$Built)) {
    stop(package, " is not a valid binary package (built field missing)")
  }
  built <- parse_built(desc$Built)
  if (built$platform != .Platform$OS.type) {
    stop(package, " is not a valid binary package (incorrect platform)")
  }
  
  TRUE
}

parse_built <- function(x) {
  pieces <- strsplit(x, "; ")[[1]]
  list(
    r = pieces[[1]],
    date = strptime(pieces[[3]], "%Y-%m-%d %H:%M:%S"),
    platform = pieces[[4]]
  )
}

check_checksums <- function(path) {
  checksum <- checkMD5sums(basename(path), path)
  if (is.na(checksum)) {
    warning("No checksums present", call. = FALSE)
  }
  if (identical(checksum, FALSE)) {
    stop("File checksums do not match", .call = FALSE)
  }
}

# Either install_packages successfully installs requested packages or
# it does nothing: it is an idempotent operation.  However, it caches
# all intermediate operations in a session temporary directory, so if
# you have to repeat an install it does not take have to repeat every
# individual operation.
#
# Temporary files are only cleaned up once the package has been
# succesffuly removed
#
# If you want to build a source package, how could you easily install binary
# versions of all dependencies?
#
# If dependencies are available in other libraries than the one in installing
# in, we don't need to download - only need to copy. That could be a separate
# function: e.g. `complete_library`.
#
# @returns Packages (along with versions), that were successfully installed
install_packages <- function(packages, type = "binary", repos = getOption("repos"),
                             library = .libPaths()[1], suggests = FALSE, force = FALSE, ...) {
  assert_that(is.character(packages))
  assert_that(is.string(library))

  if (is.null(repos)) {
    stop("Please select cran mirror with chooseCRANmirror()")
  }
  assert_that(is.string(type))
  type <- match.arg(type, c("binary", "source"))
  if (type == "binary") {
    type <- .Platform$pkgType
    if (type == "source") {
      stop("Your platform does not have binaries available", call. = FALSE)
    }
  }
  url <- contrib.url(repos, type)

  # Add dependencies
  deps <- find_dependencies(packages, url, deps = dependencies)
  needed <- deps$status
  if (any(needed)) {
    if (!quiet) {
      message("Also installing dependencies: \n", paste0("* ",
        deps$status[needed], " (", deps$status[needed], ")", collapse = "\n"))
    }
    packages <- unique(c(packages, deps$name[needed]))
  }

  # Cleanup, if requested
  working_dir <- file.path(tempdir(), "packman")

  downloaded <- download_packages(packages, type = type)
  if (type == "source") {
    built <- build_packages(downloaded)
  } else {
    build <- downloaded
  }

  installed <- install_binary_packages(downloaded, library)

  clean_up(c(downloaded, built))

  # If any installed packages were already loaded, need to either reload
  # packages, or advise author to restart R

  invisible(installed)
}

# Needs to succeed or fail as a whole
install_binary_packages <- function(packages, library) {
  # Check library is writeable
  # Check tmpdir is writeable

}

# Needs to restore to previous state on failure
install_binary_package <- function(path, library) {
  message("Installing ", basename(path))

  # Decompresses file (ignoring extension)
  unzipped <- decompress(package)
  on.exit(unlink(unzipped))

  package_path <- file.path(unzipped, "Meta", "package.rds")
  if (!file.exists(package_path)) {
    stop(package, " is not a valid binary package (Meta/package.rds not found)")
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

  checksum <- checkMD5sums(pkgname, file.path(tmpDir, pkgname))
  if (is.na(checksum)) {
    warning("No checksums present", call. = FALSE)
  }
  if (identical(checksum, FALSE)) {
    stop("File checksums do not match", .call = FALSE)
  }

  # Checks md5sums
  # Moves old directory to temp
  # Moves new directory to old, matching permissions




  # Ideall the following two options should be atomic
  copy_permissions(old, new)
  file.move(old, tempdir())
  file.move(new, library)
}

clean_up <- function() {
  res <- unlink(working_dir, recursive = TRUE)
  if (!identical(res, 0L)) {
    stop("Cleanup failed", call. = FALSE)
  }
}

parse_built <- function(x) {
  pieces <- strsplit(x, "; ")[[1]]
  list(
    r = pieces[[1]],
    date = strptime(pieces[[3]], "%Y-%m-%d %H:%M:%S"),
    platform = pieces[[4]]
  )
}



# Uses session specific cache directory.
# Only builds packages not already successfully built
build_packages <- function(paths, location) {

}

# Needs to fail with informative error messages
build_package <- function(package, path) {
  cmd <- paste("CMD INSTALL ", shQuote(pkg$path), " --build", sep = "")

  R(cmd, path, quiet = quiet)

  ext <- if (.Platform$OS.type == "windows") "zip" else "tgz"
  targz <- paste(pkg$package, "_", pkg$version, ".", ext, sep = "")

  file.path(path, targz)
}

package_built_file <- function(package) {

}

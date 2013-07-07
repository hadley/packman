# Two types of failures:
# * don't currently have installed
# * can't install

# Need to extract R versions
# Need to be able to topo sort so know in which order to install sources
# Packages in pkg should be installed if force = TRUE
# Packages in pkg should include suggests if force = TRUE
# Also needs to map urls
find_dependencies <- function(pkg = NULL, url,
                         check = c("Depends", "Imports", "LinkingTo")) {

  pkgs <- available.packages(url)
  base <- base_packages()

  all_deps <- data.frame()
  seen <- character()
  to_see <- pkg

  while (length(to_see) > 0) {
    next_pkg <- to_see[1]
    seen <- c(seen, next_pkg)

    deps <- find_deps(next_pkg, check, pkgs, base)
    all_deps <- rbind(all_deps, deps)
    to_see <- c(to_see[-1], setdiff(deps$name, seen))
  }

  rownames(all_deps) <- NULL
  all_deps
}

find_deps <- function(pkg, checkl, pkgs, base) {

  if (pkg %in% base) return(NULL)

  if (!(pkg %in% rownames(pkgs))) {
    return(data.frame(name = pkg, compare = NA, version = NA, from = pkg,
      status = "not available"))
  }

  first <- pkgs[pkg, check]
  parsed <- lapply(as.list(first), parse_deps)
  df <- do.call("rbind", unname(parsed))
  if (nrow(df) == 0) return(df)

  df$from <- pkg
  df$status <- unlist(Map(dep_installed, df$name, df$compare, df$version))
  df
}

base_packages <- function(x) {
  pkgs <- installed.packages(last(.libPaths()))
  pkgs[pkgs[, "Priority"] == "base", , drop = FALSE]
}

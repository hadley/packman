\name{topo_sort}
\alias{topo_sort}
\title{Package dependency topological sort.}
\usage{
  topo_sort(packages)
}
\arguments{
  \item{packages}{a named list of packages that should be
  sorted topologically according to their dependencies,}
}
\value{
  a named list of packages ordered in a way that they can
  be sequentially installed from source.
}
\description{
  This is implemented via a naive depth-first search - it
  will not warn if there are cycles in the dependency
  graph.
}
\examples{
pkgs <- list(
  a = package_info(test_source(), "a"),
  b = package_info(test_source(), "b")
)
topo_sort(pkgs)
}
\keyword{internal}


context("Dependencies")

test_that("single local dep resolved", {
  local_dep <- test_source("local-dep")
  a <- list(package_info(local_dep, "a"))
  b <- list(package_info(local_dep, "b"))
  
  deps_b <- add_dependencies(b, sources = local_dep)
  expect_equal(length(deps_b), 1)
  
  deps_a <- add_dependencies(a, sources = local_dep)
  expect_equal(length(deps_a), 2)
  expect_equal(names(deps_a), c("b", "a"))
})

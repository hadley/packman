context("Dependencies")

test_that("single local dep resolved", {
  local_dep <- test_source("local-dep")
  deps_b <- find_dependencies("b", sources = local_dep)
  expect_equal(length(deps_b), 0)
  
  deps_a <- find_dependencies("a", sources = local_dep)
  expect_equal(length(deps_a), 1)
  expect_equal(deps_a[[1]]$Package, "b")
})
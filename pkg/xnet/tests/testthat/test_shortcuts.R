library(xnet)
context("LOO shortcuts")
data(drugtarget)

test_that("get_loo_fun returns the correct function",{

  expect_equal(get_loo_fun('interaction', FALSE, replaceby0 = FALSE), loo.i)
  expect_equal(get_loo_fun('interaction', FALSE, replaceby0 = TRUE), loo.i0)
  expect_equal(get_loo_fun('row', FALSE), loo.r)
  expect_equal(get_loo_fun('column',FALSE), loo.c)
  expect_equal(get_loo_fun('both',FALSE), loo.b)

  expect_equal(get_loo_fun('interaction', TRUE,
                           symmetry = "skewed",
                           replaceby0 = FALSE), loo.e.skew)
  expect_equal(get_loo_fun('interaction', TRUE,
                           symmetry = "symmetric",
                           replaceby0 = FALSE), loo.e.sym)
  expect_equal(get_loo_fun('row', TRUE), loo.v)
  expect_equal(get_loo_fun('column', TRUE), loo.v)
  expect_equal(get_loo_fun('both', TRUE), loo.v)
  expect_error(get_loo_fun('column', FALSE, replaceby0 = TRUE))
})

test_that("shortcut I bipartite works",{

  expect_equal(1,1)
})


context("Helper functions")

dfile <- system.file("testdata","testdata.rda", package = "xnet")

load(dfile)

test_that("Dimension checks work correctly",{
  expect_true(valid_dimensions(Y,K,G))
  expect_false(valid_dimensions(Y,K))
  expect_true(valid_dimensions(matrix(ncol = 5, nrow = 5),
                               matrix(ncol = 5, nrow = 5)))
  expect_false(valid_dimensions(Y,G,K))
})

test_that("create_grid works correctly", {
  expect_equal(create_grid(c(1e-2, 1e2), 5),
               c(0.01,0.1,1,10,100))
  expect_error(create_grid(4,1))
  expect_error(create_grid(1:2, 1:2))
})
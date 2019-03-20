context("tuning heterogenous models")

dfile <- system.file("testdata","testdata.rda", package = "xnet")

load(dfile)

# Create test model
lambdas <- c(0.01,0.015)
mod <- tskrr(Y,K,G,lambda = lambdas)

# Test errors ---------------------------------------------

test_that("input of tune is correctly processed",{
  expect_error(tune(mod, lim = "a"),
               "lim .* numeric vector .* list with two")
  expect_error(tune(mod, lim = numeric(0)),
               "lim .* 2 numeric values")
  expect_error(tune(mod, lim = list(c(0.01,1), c(1,2,3))),
               "lim .* 2 numeric values")
})

# Test output ---------------------------------------------
tuned <- tune(mod,
              lim = list(c(0.001,1),c(0.015,2)),
              ngrid = list(10,20),
              exclusion = "row")

test_that("Output of tuned model is correct", {
  expect_identical(tuned,
                   tune(tuned,
                        lim = list(c(0.001,1),c(0.015,2)),
                        ngrid = list(10,20),
                        exclusion = "row"))
})
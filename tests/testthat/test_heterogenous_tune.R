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

manlambdas <- list(
  k = create_grid(lim = c(0.001,1),ngrid = 10),
  g = create_grid(lim = c(0.015,2), ngrid = 20)
)
tunedman <- tune(mod,
                 lambda = manlambdas,
                 exclusion = "row")

test_that("Output of tuned model is correct", {
  # retuning should give the exact same outcome
  expect_identical(tuned,
                   tune(tuned,
                        lim = list(c(0.001,1),c(0.015,2)),
                        ngrid = list(10,20),
                        exclusion = "row"))
  # manually setting lambdas should give exact same outcome
  expect_identical(tuned,
                   tunedman)
  # You should get the exact same loo function
  expect_identical(get_loo_fun(tuned),
                   get_loo_fun(mod,
                               exclusion = "row"))
  # Loss function should be correct
  expect_identical(tuned@loss_function,
                   loss_mse)

})

# Test behaviour as tskrr ---------------------------------
test_that("get_loo_fun works correctly on tuned models",{
  expect_identical(get_loo_fun(tuned,
                               exclusion = "interaction",
                               replaceby0 = TRUE),
                   get_loo_fun(mod,
                               exclusion = "interaction",
                               replaceby0 = TRUE))
})
context("tuning homogenous models")

dfile <- system.file("testdata","testdataH.rda", package = "xnet")

load(dfile)

# Create test model
lambdas <- c(0.01)
mod <- tskrr(Yh,Kh,lambda = lambdas)

# Test errors ---------------------------------------------

test_that("input of tune is correctly processed",{
  expect_error(tune(mod, lim = "a"),
               "lim .* single series of numeric values")
  expect_error(tune(mod, lim = numeric(0)),
               "lim needs 2 numeric values")
  expect_error(tune(mod, lim = list(c(0.01,1), c(1,2))),
               "lim .* single series of numeric values")
  expect_error(tune(mod, ngrid = list(12,12)),
               "ngrid .* single series of numeric values")

})

# Test output ---------------------------------------------
tuned <- tune(mod,
              lim = list(c(0.001,1)),
              ngrid = list(20),
              exclusion = "both")
manlambdas <- create_grid(lim = c(0.001,1),
                          ngrid = 20)
tunedman <- tune(mod,
                 lambda = manlambdas,
                 exclusion = "both")

test_that("Output of tuned model is correct", {
  # retuning should give the exact same outcome
  expect_identical(tuned,
                   tune(tuned,
                        lim = list(c(0.001,1)),
                        ngrid = list(20),
                        exclusion = "both"))
  # manually setting lambdas should give exact same outcome
  expect_identical(tuned,
                   tunedman)
  # You should get the exact same loo function
  expect_identical(get_loo_fun(tuned),
                   get_loo_fun(mod,
                               exclusion = "both"))
  # Loss function should be correct
  expect_identical(tuned@loss_function,
                   loss_mse)
  # grid is correct
  expect_identical(get_grid(tuned),
                   list(k = manlambdas))
  # loss values are correct
  lossval <- get_loss_values(tuned)
  expect_equal(dim(lossval), c(length(manlambdas),1))

  testmod <- update(mod,manlambdas[15])
  expect_equal(lossval[15,1],
               loss(testmod, exclusion = "both"))

})

# loss --------------------------

test_that("loss is calculated correctly",{
  expect_equal(loss(tuned),loss_mse(response(tuned),
                                    loo(tuned, exclusion = "both")))
  expect_equal(loss(tuned, exclusion = "interaction", fun = loss_auc,
                    replaceby0 = TRUE),
               loss_auc(response(tuned),
                        loo(tuned, replaceby0 = TRUE)))
})


# Test behaviour as tskrr ---------------------------------
test_that("get_loo_fun works correctly on tuned homogenous models",{
  expect_identical(get_loo_fun(tuned,
                               exclusion = "interaction",
                               replaceby0 = TRUE),
                   get_loo_fun(mod,
                               exclusion = "interaction",
                               replaceby0 = TRUE))
})
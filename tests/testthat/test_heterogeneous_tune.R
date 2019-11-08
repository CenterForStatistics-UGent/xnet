context("tuning heterogeneous models")

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
tunedirect <- tune(Y,K,G,
                   lim = list(c(0.001,1),c(0.015,2)),
                   ngrid = list(10,20),
                   exclusion = "row"
                   )

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

  # grid is correct
  expect_identical(get_grid(tuned),
                   manlambdas)
  # loss values are correct
  lossval <- get_loss_values(tuned)
  expect_equal(dim(lossval), c(length(manlambdas$k),
                               length(manlambdas$g)))

  testmod <- update(mod,c(manlambdas$k[6], manlambdas$g[12]))
  expect_equal(lossval[6,12],
               loss(testmod, exclusion = "row"))
  # direct construction of model goes OK
  expect_equal(tuned,
               tunedirect)

})

# Test one dimensional search ---------------------------------
tune1d <- tune(mod, lim = c(0.01,1), ngrid = 10, onedim = TRUE)
tune2d <- tune(mod, lim = c(0.01,1), ngrid = 10)

test_that("one dimensional search gives correct result",{
  expect_equal(as.vector(get_loss_values(tune1d)),
               diag(get_loss_values(tune2d)))
  expect_true(has_onedim(tune1d))
  expect_false(has_onedim(tune2d))
  expect_null(get_grid(tune1d)$g)
  expect_equal(get_grid(tune1d)$k, get_grid(tune2d)$k)

})


# loss --------------------------

test_that("loss is calculated correctly",{
  expect_equal(loss(tuned),loss_mse(response(tuned),
                                    loo(tuned, exclusion = "row")))
  expect_equal(loss(tuned, exclusion = "interaction", fun = loss_auc,
                    replaceby0 = TRUE),
               loss_auc(response(tuned),
                        loo(tuned, replaceby0 = TRUE)))
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

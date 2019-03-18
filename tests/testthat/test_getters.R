context("Getter functions")

dfile <- system.file("testdata","testdata.rda", package = "xnet")

load(dfile)
lambdak <- 0.01
lambdag <- 1.5
mod <- tskrr(Y, K, G, lambda = c(lambdak, lambdag))

dfile <- system.file("testdata","testdataH.rda", package = "xnet")
load(dfile)
modh <- tskrr(Yh, Kh, lambda = lambdak)
mods <- tskrr(Ys, Kh, lambda = lambdak)

test_that("is_homogenous works correctly",{

  expect_error(is_homogenous(1))
  expect_true(is_homogenous(modh))
  expect_true(is_homogenous(mods))
  expect_false(is_homogenous(mod))

  expect_true(is_heterogenous(mod))
  expect_false(is_heterogenous(modh))
  expect_false(is_heterogenous(mods))
  expect_error(is_heterogenous(1))
})

test_that("symmetry works correctly",{
  expect_equal(symmetry(mod), NA)
  expect_equal(symmetry(modh),"symmetric")
  expect_equal(symmetry(mods), "skewed")
})

mod2 <- tskrr(Y, K, G, lambda = c(lambdak, lambdag), keep = TRUE)
mod3 <- tskrr(Yh,Kh, lambda = lambdak, keep = TRUE)

test_that("has_hat works correctly",{
  expect_error(has_hat(1))
  expect_false(has_hat(mod))
  expect_true(has_hat(mod2))
  expect_true(has_hat(mod3))
})

test_that("hat returns the correct hat when they're stored",{
  expect_equal(hat(mod),hat(mod2))
  expect_equal(hat(modh), hat(mod3))
})

test_that("dim works correctly",{
  expect_equal(dim(mod),dim(Y))
  expect_equal(dim(modh),dim(Yh))
})

test_that("fitted and predict give same result",{
  expect_equal(fitted(mod2), predict(mod2))
  expect_equal(fitted(mod3, labels = FALSE), predict(mod3, Kh))
})


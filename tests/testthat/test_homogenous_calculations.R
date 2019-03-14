context("homogenous tskrr calculations")

# Create the structures needed. Was saved in a .rdata file


dfile <- system.file("testdata","testdataH.rda", package = "xnet")
load(dfile)


# Prepare the eigen decompositions
Keig <- eigen(Kh)
Kmat <- Keig$vectors
Kvec <- Keig$values

# Calculate the hat and map matrices
lambdak <- 0.01

Hk <- Kmat %*% diag(Kvec) %*% solve(diag(Kvec) + lambdak*diag(5)) %*% t(Kmat)
Mk <- Kmat %*% solve(diag(Kvec) + lambdak*diag(5)) %*% t(Kmat)

# Fit the model
mod <- tskrr(Yh, Kh, lambda = lambdak)

# Manual construction of the fits
fits <- Hk %*% Yh %*% Hk
wts <- Mk %*% Yh %*% Mk

naivewts <- solve(Kh + lambdak*diag(5)) %*% Yh %*%
  solve(Kh + lambdak*diag(5))

test_that("tskrr homogenous is valid",{
  expect_true(validObject(mod))
})

test_that("Symmetric model is fitted correctly",{
  expect_equal(fitted(mod, labels = FALSE),fits)
})

test_that("Symmetric model object is constructed correctly",{
  expect_equal(response(mod), Yh)
  expect_equal(lambda(mod), c(k = lambdak))
  expect_equal(symmetry(mod), "symmetric")
  expect_equal(get_eigen(mod, "row"), Keig)
  expect_equal(get_eigen(mod, "column"), Keig)
  expect_true(is_homogenous(mod))
  expect_equal(hat(mod, 'row'), Hk)
  expect_equal(hat(mod, 'column'), Hk)
  expect_false(has_hat(mod))
})

test_that("Kernel matrices are extracted correctly", {
  expect_equal(Kh, get_kernel(mod, 'row'))
  expect_equal(Kh, get_kernel(mod, 'column'))
})

# Skewed networks ----------------------------------------------

mod <- tskrr(Ys, Kh, lambda = lambdak)
# Manual construction of the fits
fits <- Hk %*% Ys %*% Hk
wts <- Mk %*% Ys %*% Mk

naivewts <- solve(Kh + lambdak*diag(5)) %*% Ys %*%
  solve(Kh + lambdak*diag(5))

test_that("Skewed model is fitted correctly",{
  expect_equal(fitted(mod, labels = FALSE),fits)
})

test_that("Skewed model object is constructed correctly",{
  expect_equal(response(mod), Ys)
  expect_equal(lambda(mod), c(k = lambdak))
  expect_equal(symmetry(mod), "skewed")
  expect_equal(get_eigen(mod, "row"), Keig)
  expect_equal(get_eigen(mod, "column"), Keig)
  expect_true(is_homogenous(mod))
  expect_equal(hat(mod, 'row'), Hk)
  expect_equal(hat(mod, 'column'), Hk)
  expect_false(has_hat(mod))
})

# test labels

# Check label matching

hlabels <- letters[1:5]

Ylh <- Yh
Yls <- Ys
Klh <- Kh

rownames(Ylh) <- colnames(Ylh) <- hlabels
colnames(Yls) <- rownames(Yls) <- hlabels
rownames(Klh) <- colnames(Klh) <- hlabels

idk <- sample(1:5)
idy <- sample(1:5)
Ylh2 <- Ylh[idy, idy]
Klh2 <- Klh[idk, idk]
Yls2 <- Yls[idy, idy]

test_that("Labels are correctly processed in fitting tskrr",{
  mod1 <- tskrr(Ylh, Klh)
  mod2 <- tskrr(Ylh2, Klh2)
  expect_equal(fitted(mod1)[hlabels,hlabels],
               fitted(mod2)[hlabels,hlabels])

  mod1 <- tskrr(Yls, Klh)
  mod2 <- tskrr(Yls2, Klh2)
  expect_equal(fitted(mod1)[hlabels,hlabels],
               fitted(mod2)[hlabels,hlabels])

})
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
modh <- tskrr(Yh, Kh, lambda = lambdak)

# Manual construction of the fits
fits <- Hk %*% Yh %*% Hk
wts <- Mk %*% Yh %*% Mk

naivewts <- solve(Kh + lambdak*diag(5)) %*% Yh %*%
  solve(Kh + lambdak*diag(5))

test_that("Symmetric model is fitted correctly",{
  expect_equal(fitted(modh, labels = FALSE),fits)
})

test_that("Symmetric model object is constructed correctly",{
  expect_equal(response(modh), Yh)
  expect_equal(lambda(modh), c(k = lambdak))
  expect_equal(symmetry(modh), "symmetric")
  expect_equal(get_eigen(modh, "row"), Keig)
  expect_equal(get_eigen(modh, "column"), Keig)
  expect_true(is_homogenous(modh))
  expect_equal(hat(modh, 'row'), Hk)
  expect_equal(hat(modh, 'column'), Hk)
  expect_false(has_hat(modh))
})

test_that("Kernel matrices are extracted correctly", {
  expect_equal(Kh, get_kernelmatrix(modh, 'row'))
  expect_equal(Kh, get_kernelmatrix(modh, 'column'))
})

# Skewed networks ----------------------------------------------

mods <- tskrr(Ys, Kh, lambda = lambdak)
# Manual construction of the fits
fits <- Hk %*% Ys %*% Hk
wts <- Mk %*% Ys %*% Mk

naivewts <- solve(Kh + lambdak*diag(5)) %*% Ys %*%
  solve(Kh + lambdak*diag(5))

test_that("Skewed model is fitted correctly",{
  expect_equal(fitted(mods, labels = FALSE),fits)
})

test_that("Skewed model object is constructed correctly",{
  expect_equal(response(mods), Ys)
  expect_equal(lambda(mods), c(k = lambdak))
  expect_equal(symmetry(mods), "skewed")
  expect_equal(get_eigen(mods, "row"), Keig)
  expect_equal(get_eigen(mods, "column"), Keig)
  expect_true(is_homogenous(mods))
  expect_equal(hat(mods, 'row'), Hk)
  expect_equal(hat(mods, 'column'), Hk)
  expect_false(has_hat(mods))
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
Ylh2 <- Ylh[idy, idk]
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

## Error testing ---------------------------------
Yhwrong <- Yh
Yhwrong[4,5] <- 1
Yswrong <- Ys
Yswrong[4,5] <- Yswrong[5,4] <- 1

test_that("Asymmetry in Y is detected",{
  expect_error(tskrr(Yhwrong, Kh),
               "The Y matrix is not symmetric")
  expect_error(tskrr(Yswrong, Kh),
               "The Y matrix is not .* skewed symmetric")
})

## Update --------------------------------------
lambdanew <- 0.001
modhnew <- tskrr(Yh, Kh, lambda = lambdanew)
modsnew <- tskrr(Ys, Kh, lambda = lambdanew)

mod3 <- tskrr(Yh,Kh, lambda = lambdak, keep = TRUE)
modnew3 <- mod3 <- tskrr(Yh,Kh, lambda = lambdanew, keep = TRUE)

test_that("Homogenous model gets updated correctly",{
  expect_error(update(modh, lambda = numeric(0)))
  expect_error(update(mods, lambda = c(1,2,3)))
  expect_error(update(mods, lambda = c(0.01,0.01)))
  expect_equal(update(modh, lambdanew), modhnew)
  expect_equal(update(mods, lambdanew), modsnew)
  expect_equal(update(mod3, lambdanew), modnew3)

})

# loss --------------------------

test_that("loss is calculated correctly",{
  expect_equal(loss(modh),loss_mse(response(modh), loo(modh)))
  expect_equal(loss(mods, exclusion = "interaction", fun = loss_auc,
                    replaceby0 = TRUE),
               loss_auc(response(mods),
                        loo(mods, replaceby0 = TRUE)))
  expect_equal(loss(modh, predictions = TRUE),
               loss_mse(response(modh), fitted(modh)))
})

# predict ------------------------
predh <- Khnew %*% weights(modh) %*% Kh
colnames(predh) <- paste0("row",1:5)

predall <- Khnew %*% weights(modh) %*% t(Khnew)

preds <- Khnew %*% weights(mods) %*% Kh
colnames(preds) <- paste0("row",1:5)

test_that("predict works as intended",{
  expect_equal(predh,
               predict(modh, Khnew))
  expect_equal(preds,
               predict(mods, Khnew))
  expect_equal(predict(modh, g = Khnew),
               t(predh))
  expect_equal(predict(modh, Khnew, Khnew),
               predall)
})

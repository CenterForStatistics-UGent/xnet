context("heterogenous tskrr calculations")

# Create the structures needed. Was saved in a .rdata file


dfile <- system.file("testdata","testdata.rda", package = "xnet")

load(dfile)


# Prepare the eigen decompositions
Keig <- eigen(K)
Kmat <- Keig$vectors
Kvec <- Keig$values

Geig <- eigen(G)
Gmat <- Geig$vectors
Gvec <- Geig$values

# Calculate the hat and map matrices
lambdak <- 0.01
lambdag <- 1.5
Hk <- Kmat %*% diag(Kvec) %*% solve(diag(Kvec) + lambdak*diag(4)) %*% t(Kmat)
Mk <- Kmat %*% solve(diag(Kvec) + lambdak*diag(4)) %*% t(Kmat)
Hg <- Gmat %*% diag(Gvec) %*% solve(diag(Gvec) + lambdag*diag(5)) %*% t(Gmat)
Mg <- Gmat %*% solve(diag(Gvec) + lambdag*diag(5)) %*% t(Gmat)

# Fit the model
mod <- tskrr(Y, K, G, lambda = c(lambdak, lambdag))

# Manual construction of the fits
fits <- Hk %*% Y %*% Hg
wts <- Mk %*% Y %*% Mg

naivewts <- solve(K + lambdak*diag(4)) %*% Y %*%
  solve(G + lambdag*diag(5))

test_that("hat and map matrix is calculated correctly",{
  expect_equal(Hk, eigen2hat(Kmat, Kvec, lambdak))
  expect_equal(Mk, eigen2map(Kmat, Kvec, lambdak))
})

test_that("weights are calculated correctly",{
  expect_equal(wts, weights(mod))
  expect_equal(naivewts, weights(mod))
})

test_that("Heterogenous model is fitted correctly",{
  expect_equal(fitted(mod, labels = FALSE),fits)
})

test_that("Heterogenous model object is constructed correctly",{
  expect_equal(response(mod), Y)
  expect_equal(lambda(mod), c(k = lambdak, g = lambdag))
  expect_true(is.na(symmetry(mod)))
  expect_equal(get_eigen(mod, "row"), Keig)
  expect_equal(get_eigen(mod, "column"), Geig)
  expect_false(is_homogenous(mod))
  expect_equal(hat(mod, 'row'), Hk)
  expect_equal(hat(mod, 'column'), Hg)
  expect_false(has_hat(mod))
})

test_that("Kernel matrices are extracted correctly", {
  expect_equal(K, get_kernel(mod, 'row'))
  expect_equal(G, get_kernel(mod, 'column'))
})

# Check label matching

rlabels <- letters[1:4]
clabels <- letters[1:5]

Yl <- Y
Kl <- K
Gl <- G
rownames(Yl) <- rownames(Kl) <- colnames(Kl) <- rlabels
colnames(Yl) <- rownames(Gl) <- colnames(Gl) <- clabels

set.seed(5432) # Due to small size of matrices, there might be
               # significant deviation bcs of differences in the
               # decompositions. See eg with seed = 5434 (R3.5.3)

idk <- sample(1:4)
idg <- sample(1:5)
Yl2 <- Yl[sample(1:4), sample(1:5)]
Kl2 <- Kl[idk, idk]
Gl2 <- Gl[idg,idg]

mod1 <- tskrr(Yl,Kl,Gl)
mod2 <- tskrr(Yl2,Kl2,Gl2)

test_that("Labels are correctly processed in fitting tskrr",{
  expect_equal(fitted(mod1)[rlabels,clabels],
               fitted(mod2)[rlabels,clabels])

})

# update -------------------------
lambdanew <- c(0.001,0.01)
modnew <- tskrr(Y, K, G, lambda = lambdanew)

mod3 <- tskrr(Y, K, G, lambda = c(lambdak, lambdag), keep = TRUE)
modnew3 <- tskrr(Y, K, G, lambda = lambdanew, keep = TRUE)
modnew4 <- tskrr(Y, K, G, lambda = 0.5, keep = TRUE)

test_that("Heterogenous model gets updated correctly",{
  expect_error(update(mod, lambda = numeric(0)))
  expect_error(update(mod, lambda = c(1,2,3)))
  expect_equal(update(mod, lambdanew), modnew)
  expect_equal(update(mod3, lambdanew),modnew3)
  expect_equal(update(mod3, 0.5), modnew4)
})

# loss --------------------------

test_that("loss is calculated correctly",{
  expect_equal(loss(mod),loss_mse(response(mod), loo(mod)))
  expect_equal(loss(mod, exclusion = "column", fun = loss_auc),
               loss_auc(response(mod),
                        loo(mod, exclusion = "column")))
})
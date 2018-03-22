context("modeling calculations")

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

test_that("hat and map matrix is calculated correctly",{
  expect_equal(Hk, eigen2hat(Kmat, Kvec, lambdak))
  expect_equal(Mk, eigen2map(Kmat, Kvec, lambdak))
})


test_that("Heterogenous model is fitted correctly",{
  expect_equal(fitted(mod),fits)
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
})


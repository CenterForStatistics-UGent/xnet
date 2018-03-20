library(xnet)
context("matrix calculations for weights and predictions")

K <- matrix(c(1,2,3,4,2,1,4,3,5,2,1,3,3,1,2,4),ncol = 4)
G <- matrix(c(5,1,1,6,5,4,2,4,1,1,3,1,2,4,3,6,3,3,1,2,6,1,4,4,6),ncol = 5)
Y <- matrix(c(1,0,0,1,0,0,1,1,0,0,0,0,0,1,0,0,1,0,1,1), ncol = 5)

Keig <- eigen(X)
Kmat <- Keig$vectors
Kvec <- Keig$values

Geig <- eigen(G)
Gmat <- Geig$vectors
Gvec <- Geig$values

lambda <- 0.001
H <- Kmat %*% diag(Kvec) %*% solve(diag(Kvec) + lambda*diag(4)) %*% t(Kmat)
W <- Kmat %*% solve(diag(Kvec) + lambda*diag(4)) %*% t(Kmat)

test_that("hat and map matrix is calculated correctly",{
  expect_equal(H, eigen2hat(Kmat, Kvec, lambda))
  expect_equal(W, eigen2map(Kmat, Kvec, lambda))
})

test_that("Model is fitted correctly",{
  expect_equal(1,2)
})

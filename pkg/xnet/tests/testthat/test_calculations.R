library(xnet)
context("Matrix calculations")

X <- matrix(c(1,2,3,4,2,1,4,3,5,2,1,3,3,1,2,4),ncol = 4)
Xeig <- eigen(X)
mat <- Xeig$vectors
vec <- Xeig$values
lambda <- 0.001
H <- mat %*% diag(vec) %*% solve(diag(vec) + lambda*diag(4)) %*% t(mat)

test_that("hat matrix is calculated correctly",{
  expect_equal(H, eigen2hat(mat, vec, lambda))
})

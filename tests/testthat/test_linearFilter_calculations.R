context("Linear filter calculations")

# LINEAR FILTER ----------------------------------------
X <- matrix(c(1,0,0,1,0,0,0,1,0,1,0,1,1,1,0,1,0,0,1,1),
            ncol = 5)
alphas <- c(0.1,0.1,0.4,0.4)
linF <- linear_filter(X, alpha = alphas)
linF2 <- linear_filter(X)
cm <- colMeans(X)
rm <- rowMeans(X)
m <- mean(X)

preds <- alphas[1] * X +
  alphas[2] * rep(cm, each = nrow(X)) +
  alphas[3] * rep(rm, times = ncol(X)) +
  alphas[4] * m

test_that("Linear filter is constructed correctly",{
  expect_identical(colMeans(linF), cm)
  expect_identical(rowMeans(linF), rm)
  expect_identical(mean(linF), m)
  expect_identical(get_alpha(linF), alphas)
  expect_false(na_removed(linF))
  expect_equal(fitted(linF),preds)
})


test_that("alphas are processed correctly",{
  expect_error(linear_filter(Y, alpha = c(1,2)))
  expect_equal(get_alpha(linF),alphas)
  expect_equal(get_alpha(linF2), rep(0.25,4))
})

Yna <- X
Yna[c(3,8,11,14)] <- NA
linFNA <- linear_filter(Yna, alphas)
linFNONA <- suppressWarnings(linear_filter(Yna, alphas, na.rm = TRUE))

cmna <- colMeans(Yna, na.rm = TRUE)
rmna <- rowMeans(Yna, na.rm = TRUE)
mna <- mean(Yna, na.rm = TRUE)

preds <- alphas[1] * Yna +
  alphas[2] * rep(cmna, each = nrow(X)) +
  alphas[3] * rep(rmna, times = ncol(X)) +
  alphas[4] * mna


test_that("NAs are dealt with properly", {
  expect_equal(fitted(linFNA),
               matrix(NA_real_,ncol = ncol(X), nrow = nrow(X)))
  expect_true(na_removed(linFNONA))
  expect_identical(colMeans(linFNONA), cmna)
  expect_identical(rowMeans(linFNONA), rmna)
  expect_identical(mean(linFNONA), mna)
  expect_identical(get_alpha(linFNONA), alphas)
  expect_equal(fitted(linFNONA),preds)
})

test_that("linear_filter returns correct errors",{
  expect_error(linear_filter(X, alpha = c(0.5,0.5)),
               "alpha should .* either 1 or 4 values")
  expect_error(linear_filter(X, alpha = c(0.1,0.1,0.1,0.1)),
               "alpha values should .* add up to 1")
  expect_error(linear_filter(X, alpha = c(-0.2, 0.2, 0.5,0.5)),
               "alpha values should be numbers between 0 and 1")
})

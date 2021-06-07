context("Getter functions")

dfile <- system.file("testdata","testdata.rda", package = "xnet")

load(dfile)
lambdak <- 0.01
lambdag <- 1.5
mod <- tskrr(Y, K, G, lambda = c(lambdak, lambdag))
tunedmod <- tune(mod)

dfile <- system.file("testdata","testdataH.rda", package = "xnet")
load(dfile)
modh <- tskrr(Yh, Kh, lambda = lambdak)
mods <- tskrr(Ys, Kh, lambda = lambdak)
tunedmodh <- tune(modh)

test_that("is_xxx works correctly",{

  expect_error(is_homogeneous(1))
  expect_true(is_homogeneous(modh))
  expect_true(is_homogeneous(mods))
  expect_false(is_homogeneous(mod))

  expect_true(is_heterogeneous(mod))
  expect_false(is_heterogeneous(modh))
  expect_false(is_heterogeneous(mods))
  expect_error(is_heterogeneous(1))

  expect_true(is_tskrr(mod))
  expect_true(is_tskrr(modh))
  expect_false(is_tskrr("not-tskrr"))

  expect_true(is_tuned(tunedmod))
  expect_false(is_tuned(mod))
  expect_true(is_tuned(tunedmodh))
  expect_false(is_tuned(modh))
  expect_error(is_tuned("not-tskrr"))
})

Ywrong <- Yh
Ywrong[3,4] <- 2

test_that("symmetry works correctly",{
  expect_equal(symmetry(mod), NA)
  expect_equal(symmetry(modh),"symmetric")
  expect_equal(symmetry(mods), "skewed")
  expect_equal(test_symmetry(Ywrong), "none")
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
  expect_equal(hat(mod, which = 'column'),
               hat(mod2, which = 'column'))
})

test_that("dim works correctly",{
  expect_equal(dim(mod),dim(Y))
  expect_equal(dim(modh),dim(Yh))
})

test_that("fitted and predict give same result",{
  expect_equal(fitted(mod2), predict(mod2))
  expect_equal(fitted(mod3, labels = FALSE),
               unname(predict(mod3, Kh)))
})

## Test the rownames colnames functions ----------------------------
prefix <- c("therows", "thecols")
r1 <- paste0(prefix[1], seq_len(nrow(Y)))
c1 <- paste0(prefix[2], seq_len(ncol(Y)))
rh1 <- paste0(prefix[1], seq_len(nrow(Yh)))

r2 <- paste0("row", seq_len(nrow(Y)))
c2 <- paste0("col", seq_len(ncol(Y)))

r3 <- paste0("row", seq_len(nrow(Yh)))

test_that("Row and colnames are returned correctly",{
  expect_null(rownames(mod))
  expect_null(colnames(mod))

  expect_equal(rownames(mod, do.NULL = FALSE, prefix = prefix[1]), r1)
  expect_equal(colnames(mod, do.NULL = FALSE, prefix = prefix[2]), c1)

  expect_equal(rownames(mod, do.NULL = FALSE), r2)
  expect_equal(colnames(mod, do.NULL = FALSE), c2)
})

test_that("row- and colnames return correct errors",{
  expect_error(rownames(mod, do.NULL = FALSE, prefix = prefix),
               "Prefix should be a single character value")
  expect_error(colnames(mod, do.NULL = FALSE, prefix = prefix),
               "Prefix should be a single character value")
  expect_error(rownames(mod, do.NULL = FALSE, prefix = 1),
               "Prefix should be a single character value")
  expect_error(colnames(mod, do.NULL = FALSE, prefix = 1),
               "Prefix should be a single character value")

})

## Test the labels functions --------------------------------

rlabels <- letters[1:4]
clabels <- letters[1:5]

Yl <- Y
Yhl <- Yh
Kl <- K
Gl <- G
Khl <- Kh
rownames(Yl) <- rownames(Kl) <- colnames(Kl) <- rlabels
colnames(Yl) <- rownames(Gl) <- colnames(Gl) <- clabels
rownames(Yhl) <- colnames(Yhl) <- clabels
rownames(Khl) <- colnames(Khl) <- clabels

modl <- tskrr(Yl, Kl, Gl)
modhl <- tskrr(Yhl, Khl)

test_that("dimnames returns the correct result",{
  expect_equal(labels(mod), dimnames(mod))
  expect_equal(labels(modl), dimnames(modl))
})

test_that("labels gives the correct result", {
  expect_equal(labels(mod), list(k = r2, g = c2))
  expect_equal(labels(modh), list(k = r3, g = r3))
  expect_equal(labels(mods), list(k = r3, g = r3))

  expect_equal(labels(mod, prefix = prefix),
               list(k = r1, g = c1))
  expect_equal(labels(modh, prefix = prefix[1]),
               list(k = rh1, g = rh1))

  expect_equal(labels(modl), list(k = rlabels, g = clabels))
  expect_equal(labels(modhl), list(k = clabels, g = clabels))
})

test_that("labels produces the correct errors", {
  expect_error(labels(mod, prefix = c(1,2)),
               "should be a character vector")
  expect_error(labels(mod, prefix = letters[1:3]),
               "should contain 2 character values")
  expect_error(labels(mod, prefix = character(0)),
               "should contain 2 character values")
  expect_error(labels(mod, prefix = prefix[1]),
               "should contain 2 character values")
  expect_error(labels(modh, prefix = prefix),
                 "Prefix should be a single character value ")
})

# Test for tskrrTune objects ---------------------------
test_that("getters tskrrTune produce correct errors", {
  expect_error(get_grid(1),
               "x should be a tuned model")
  expect_error(get_loss_values(1),
               "x should be a tuned model")
})

# Test for tskrrImpute objects --------------------------------
test_that("getters tskrrImpute produce correct errors", {
  expect_error(has_imputed_values(1),
               "x should be a tskrr model")
  expect_false(has_imputed_values(mod))
  expect_error(which_imputed(mod),
               "x should be a tskrr model .* imputed")
  expect_error(is_imputed(mod),
               "x should be a tskrr model .* imputed")
})

# Conversions -----------------------------------
test_that("Conversions happen correctly",{
  expect_true(is_tskrr(as_tskrr(mod)))
})

# Test residuals function -----------------------
test_that("Residuals are calculated correctly",{
  expect_equal(residuals(mod), response(mod) - fitted(mod))
  expect_equal(residuals(tunedmod),
               response(tunedmod) - fitted(tunedmod))
  expect_equal(residuals(mod, method = "loo",
                         replaceby0 = TRUE),
               response(mod) - loo(mod, replaceby0 = TRUE))

})

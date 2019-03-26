context("Imputation of values")


# Test loss functions --------------------------------

x <- matrix(c(1,0,0,1,0,0), ncol = 2)
y <- matrix(c(0.9,0.1,0.2,0.4,0.1,0.6), ncol = 2)
yna <- y
yna[3] <- NA

# Manually calculation of the AUC
tab <- table(x,y)
truepos <- rev(cumsum(rev(tab[2,])))
falsepos <- rev(cumsum(rev(tab[1,])))
sens = c(truepos / sum(tab[2,]), 0)
omspec = c(falsepos / sum(tab[1,]), 0)
height = (sens[-1]+sens[-length(sens)]) / 2
width = -diff(omspec)
auc <- sum(height*width)

test_that("loss functions give correct result",{
  expect_equal(mean((y - x)^2), loss_mse(x,y))
  expect_equal(loss_auc(x,y), 1 - auc)
  expect_equal(loss_mse(x,yna), NA_real_)
  expect_equal(mean((yna - x)^2, na.rm = TRUE),
               loss_mse(x,yna, na.rm = TRUE))

})

# Heterogenous models ------------------------------------------

dfile <- system.file("testdata","testdata.rda", package = "xnet")

load(dfile)
lambdas <- c(0.01,0.015)
napos <- c(3,5,10,16)
Yna <- Y
Yna[napos] <- NA

impY <- impute_tskrr(Yna,K,G,niter = 1000, lambda = lambdas)

mod <- tskrr(response(impY),K,G,lambda = lambdas)


test_that("impute constructs the correct heterogenous objects",{
  expect_equal(mod, as_tskrr(impY))
  expect_equal(fitted(mod), fitted(impY))
  expect_equal(impY@tol,  sqrt(.Machine$double.eps))
  expect_equal(85, impY@niter) # This should be 85 based on previous tests
  expect_true(has_imputed_values(impY))
  expect_identical(is_imputed(impY), is.na(Yna))
  expect_identical(which_imputed(impY),as.integer(napos))
})

test_that("verbose settings and warnings work",{
  expect_warning(impute_tskrr(Y,K,G),
                 "didn't contain missing values")
  expect_message(impute_tskrr(Yna,K,G,niter = 50,verbose = TRUE),
                 "Nr. of iterations: 50 - Deviation")
  expect_message(impute_tskrr(Yna,K,G,niter = 50,verbose = 2),
                 "iteration: 10 - Deviation",
                 all = FALSE)

})

# Homogenous models --------------------------------------------

dfile <- system.file("testdata","testdataH.rda", package = "xnet")

load(dfile)
lambdas <- c(0.01)
Yhna <- Yh
Yhna[cbind(c(1,3,2,5),c(3,1,5,2))] <- NA

naposh <- which(is.na(Yhna))

impYh <- impute_tskrr(Yhna,Kh,niter = 1000, lambda = lambdas)

# Create test model
modh <- tskrr(response(impYh),Kh,lambda = lambdas,
              symmetry = "symmetric")
# Needs this, as in this case the response matrix is not
# exactly symmetric. test_symmetry needs a less stringent tolerance
# in this particular case, but it's good there's a warning for that.

test_that("impute constructs the correct homogenous objects",{
  expect_equal(modh, as_tskrr(impYh))
  expect_equal(fitted(modh), fitted(impYh))
  expect_equal(impYh@tol,  sqrt(.Machine$double.eps))
  expect_equal(72, impYh@niter) # This should be 72 based on previous tests
  expect_true(has_imputed_values(impYh))
  expect_identical(is_imputed(impYh), is.na(Yhna))
  expect_identical(which_imputed(impYh),as.integer(naposh))
})

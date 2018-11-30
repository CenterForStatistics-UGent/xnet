context("LOO shortcuts")

# Setup for testing

## Heterogenous network
dfile <- system.file("testdata","testdata.rda", package = "xnet")

load(dfile)
lambdak <- 0.01
lambdag <- 1.5
mod <- tskrr(Y, K, G, lambda = c(lambdak, lambdag))
eigK <- get_eigen(mod, 'row')
eigG <- get_eigen(mod, 'column')
Hk <- hat(mod, 'row')
Hg <- hat(mod, 'column')
pred <- fitted(mod)

## Homogenous network
dfile <- system.file("testdata","testdataH.rda", package = "xnet")
load(dfile)
modh <- tskrr(Yh, Kh, lambda = lambdak)
eigKh <- get_eigen(modh)
Hkh <- hat(modh)
predh <- fitted(modh)

# Skewed network
mods <- tskrr(Ys, Kh, lambda = lambdak)
eigKs <- get_eigen(mods)
Hks <- hat(mods)
preds <- fitted(mods)

test_that("get_loo_fun returns the correct function",{

  # Heterogenous models
  expect_equal(get_loo_fun(mod, 'interaction', replaceby0 = FALSE), loo.i)
  expect_equal(get_loo_fun(mod, 'interaction', replaceby0 = TRUE), loo.i0)
  expect_equal(get_loo_fun(mod,'row'), loo.r)
  expect_equal(get_loo_fun(mod,'column'), loo.c)
  expect_equal(get_loo_fun(mod,'both'), loo.b)

  expect_equal(get_loo_fun(modh,'interaction',
                           replaceby0 = FALSE), loo.e.sym)
  expect_equal(get_loo_fun(mods, 'interaction'), loo.e.skew)
  expect_equal(get_loo_fun(modh,'both'), loo.v)
  expect_equal(get_loo_fun(mods,'both'), loo.v)
  expect_error(get_loo_fun(modh,'row'))
  expect_error(get_loo_fun(mods,'column',replaceby0 = TRUE))
})

predict_ij <- function(Y,Hk, Hg, i, j){
  Hk[i,] %*% Y %*% Hg[,j]
}

test_that("shortcuts bipartite networks work",{

  # Setting I
  looI <- xnet:::loo(mod)
  looItest <- sapply(seq_len(ncol(Y)),
                     function(y) sapply(seq_len(nrow(Y)),
                                        function(x){Ytilde <- Y
                                        Ytilde[x,y] <- looI[x,y]
                                        predict_ij(Ytilde, Hk, Hg, x, y) }
                                        )
                     )


  expect_equal(looI,looItest)

  # Setting I0
  looI0 <- loo(mod, replaceby0 = TRUE)
  looI0test <- sapply(seq_len(ncol(Y)),
                     function(y) sapply(seq_len(nrow(Y)),
                                        function(x){Ytilde <- Y
                                        Ytilde[x,y] <- 0
                                        predict_ij(Ytilde, Hk, Hg, x, y) }
                     ))

  expect_equal(looI0, looI0test)

  # Setting Row
  looR <- loo(mod, exclusion = "row")
  looRtest <- sapply(seq_len(nrow(Y)), function(i){
    modx <- tskrr(Y[-i,],K[-i,-i],G, lambda = c(lambdak, lambdag))
    predict(modx, K[i,-i],G)

  })
  # sapply gives the matrix in a different way.
  expect_equal(looR, t(looRtest))

})


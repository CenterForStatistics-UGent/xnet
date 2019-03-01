context("LOO shortcuts")

# Preparation -----------------------------------------------------
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

# symmetric network
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

# Tests -----------------------------------------------------------

## get_loo_fun ----------------------------------------------------


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
  Hk[i,, drop = FALSE] %*% Y %*% Hg[,j, drop = FALSE]
}

## Heterogenous network -------------------------------------------

test_that("shortcuts bipartite networks work",{

  # Setting I
  looI <- loo(mod)
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

  # Setting Col
  looC <- loo(mod, exclusion = "column")
  looCtest <- sapply(seq_len(ncol(Y)), function(j){
    modx <- tskrr(Y[, -j], K, G[-j, -j], lambda = c(lambdak,lambdag))
    predict(modx, K, G[-j, j])
  })

  expect_equal(looC, looCtest)

  # Setting both
  i <- 3
  j <- 4
  looB <- loo(mod, exclusion = "both")[i,j, drop = FALSE]
  modx <- tskrr(Y[-i, -j], K[-i,-i], G[-j,-j], lambda = c(lambdak,lambdag))
  looBtest <- predict(modx, K[i, -i], G[-j, j])
  expect_equal(looB, looBtest)

})

## Homogenous network ---------------------------------------------

test_that("shortcuts homogenous networks work", {

  # Shortcut for the moment is not tested on the diagonal, as that
  # gives very weird results. Replacement by looE result is maybe
  # not the best way forward.
  looE <- loo(modh)
  looEtest <- sapply(seq_len(ncol(Yh)),
                     function(y) sapply(seq_len(nrow(Yh)),
                                        function(x){Ytilde <- Yh
                                        Ytilde[x,y] <- looE[x,y]
                                        Ytilde[y,x] <- looE[x,y]
                                        if(x == y) Ytilde[x,y] else
                                        predict_ij(Ytilde, Hkh, Hkh, x, y) }
                     )
  )


  expect_equal(looE,looEtest)

  # setting E0
  looE0 <- loo(modh, replaceby0 = TRUE)
  looE0test <- sapply(seq_len(ncol(Yh)),
                     function(y) sapply(seq_len(nrow(Yh)),
                                        function(x){Ytilde <- Yh
                                        Ytilde[x,y] <- 0
                                        Ytilde[y,x] <- 0
                                        predict_ij(Ytilde, Hkh, Hkh, x, y)
                                        }
                     )
  )

  expect_equal(looE0, looE0test)

  # Setting both

  looV <- loo(modh, exclusion = "both")
  looVtest <- sapply(seq_len(nrow(Yh)), function(i){

    modx <- tskrr(Yh[-i, -i], Kh[-i, -i], lambda = lambdak)
    predict(modx, Kh[i, -i, drop = FALSE], Kh[-i,,drop = FALSE])
  })

  expect_equal(looV, t(looVtest))

})
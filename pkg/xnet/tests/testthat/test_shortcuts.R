context("LOO shortcuts")

# Setup for testing
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


test_that("get_loo_fun returns the correct function",{

  expect_equal(get_loo_fun('interaction', FALSE, replaceby0 = FALSE), loo.i)
  expect_equal(get_loo_fun('interaction', FALSE, replaceby0 = TRUE), loo.i0)
  expect_equal(get_loo_fun('row', FALSE), loo.r)
  expect_equal(get_loo_fun('column',FALSE), loo.c)
  expect_equal(get_loo_fun('both',FALSE), loo.b)

  expect_equal(get_loo_fun('interaction', TRUE,
                           symmetry = "skewed",
                           replaceby0 = FALSE), loo.e.skew)
  expect_equal(get_loo_fun('interaction', TRUE,
                           symmetry = "symmetric",
                           replaceby0 = FALSE), loo.e.sym)
  expect_equal(get_loo_fun('row', TRUE), loo.v)
  expect_equal(get_loo_fun('column', TRUE), loo.v)
  expect_equal(get_loo_fun('both', TRUE), loo.v)
  expect_error(get_loo_fun('column', FALSE, replaceby0 = TRUE))
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
  looRtest <- 0
  expect_equal(looR, looRtest)

})


# test the leave-one-out shortcuts for networks

require(testthat)
source('train.tskrr.R')
source('loo.R')
source('linear.filter.r')

# load some data

get.pl.data <- function(name){
  # loads one of the protein-ligand datasets
  Y <- read.table(paste('../data/',name,'_admat_dgc.txt', sep=''),
                  sep='\t', header=T, row.names = 1)
  K <- read.table(paste('../data/',name,'_simmat_dg.txt', sep=''),
                  sep='\t', header=T, row.names = 1)
  G <- read.table(paste('../data/',name,'_simmat_dc.txt', sep=''),
                  sep='\t', header=T, row.names = 1)
  return(list(Y=as.matrix(Y), K=as.matrix(K), G=as.matrix(G)))
}

pl.data <- get.pl.data('nr')
Y <- pl.data$Y
K <- pl.data$K + 1e-2  # bias
K <- 0.5 * (K + t(K))
G <- pl.data$G + 1e-2  # bias
G <- 0.5 * (G + t(G))

# train a two-step model

lambda.u <- 1e-1
lambda.v <- 1e-1

model <- TrainTSKRR(Y, K, G, lambda.u = lambda.u, lambda.v = lambda.v)

par(mfrow=c(1,2))
heatmap(model$Y, Rowv=NA, Colv=NA, scale='none')
heatmap(model$F, Rowv=NA, Colv=NA, scale='none')

# get hat matrices

make_hat_matrix <- function(U, Sigma, lambda){
  return(U %*% diag(Sigma / (Sigma + lambda)) %*% t(U))
}

Hk <- make_hat_matrix(model$U, model$Sigma, model$lambda.u)
Hg <- make_hat_matrix(model$V, model$S, model$lambda.v)


# test the loo shortcuts bipartite networks

i <- 3
j <- 7

predict.tskrr.bip <- function(Y){
  return(Hk %*% Y %*% Hg)
}

test_that("shortcuts bipartite networks work", {

  # Setting I

  Floo <- loo.I(Y, Hk, Hg)
  Ytilde <- Y
  Ytilde[i,j] <- Floo[i,j]

  expect_that(predict.tskrr.bip(Ytilde)[i,j], equals(Floo[i,j]))

  # Setting I0

  Floo <- loo.I0(Y, Hk, Hg)
  Ytilde <- Y
  Ytilde[i,j] <- 0

  expect_that(predict.tskrr.bip(Ytilde)[i,j], equals(Floo[i,j]))

  # Setting R

  FlooRi <- loo.R(Y, Hk, Hg)[i,]

  model.tskrr.min.i <- TrainTSKRR(Y[-i,], K[-i,-i], G,
                          lambda.u=lambda.u, lambda.v=lambda.v)
  F.i.min.i <- c(K[i,-i] %*% model.tskrr.min.i$W %*% G)

  expect_that(F.i.min.i, equals(FlooRi))

  # Setting C

  FlooCj <- loo.C(Y, Hk, Hg)[,j]

  model.tskrr.min.j <- TrainTSKRR(Y[,-j], K, G[-j,-j],
                          lambda.u=lambda.u, lambda.v=lambda.v)
  F.j.min.j <- c(K %*% model.tskrr.min.j$W %*% G[-j, j])
  expect_that(F.j.min.j, equals(FlooCj))


  # Setting B

  FlooBij <- loo.B(Y, Hk, Hg)[i,j]

  model.tskrr.min.ij <- TrainTSKRR(Y[-i,-j], K[-i,-i], G[-j,-j],
                                  lambda.u=lambda.u, lambda.v=lambda.v)
  F.ij.min.ij <- c(K[i,-i] %*% model.tskrr.min.ij$W %*% G[-j, j])
  expect_that(F.ij.min.ij, equals(FlooBij))

  })

# test the loo shortcuts homogeneous networks

predict.tskrr.hom <- function(Y){
  return(Hk %*% Y %*% Hk)
}

Ysym <- (Y %*% t(Y))**0.5

test_that("shortcuts homogeneous networks work", {
  # Setting E
  FlooEij <- loo.E.sym(Ysym, Hk)[i,j]

  Ytilde <- Ysym
  Ytilde[i,j] <- FlooEij
  Ytilde[j,i] <- FlooEij

  expect_that(predict.tskrr.hom(Ytilde)[i,j], equals(FlooEij))

  # Setting E0
  FlooE0ij <- loo.E0.sym(Ysym, Hk)[i,j]

  Ytilde <- Ysym
  Ytilde[i,j] <- 0
  Ytilde[j,i] <- 0

  expect_that(predict.tskrr.hom(Ytilde)[i,j], equals(FlooE0ij))

  # Setting V
  FlooVi <- loo.V(Ysym, Hk)[i,]

  model.tskrr.min.ii <- TrainTSKRR(Ysym[-i,-i], K[-i,-i], K[-i,-i],
                                   lambda.u=lambda.u, lambda.v=lambda.u)
  F.i.min.ii <- c(K[i,-i] %*% model.tskrr.min.ii$W %*% K[-i,])
  expect_that(F.i.min.ii, equals(FlooVi))
})


# test the loo shortcuts for linear filter

model.lf <- train.linear.filter(Y)

test_that("shortcuts for the simple linear filter", {
  # Setting I
  FlooI <- loo.I.lf(model.lf)

  Ytilde <- Y
  Y[i,j] <- FlooI[i,j]
  F.lf.min.ij <- train.linear.filter(Ytilde)$F

  expect_that(F.lf.min.ij[i,j], equals(FlooI[i,j]))

  # Setting I0
  FlooI0 <- loo.I0.lf(model.lf)

  Ytilde <- Y
  Y[i,j] <- 0
  F.lf.zero.ij <- train.linear.filter(Ytilde)$F

  expect_that(F.lf.zero.ij[i,j], equals(FlooI0[i,j]))
})
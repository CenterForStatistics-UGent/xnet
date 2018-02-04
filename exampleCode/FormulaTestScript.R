#---------------------------------
# TEST FOR FORMULAS
# author : Joris Meys
# date last modified : 2018-02-04

# This script tests whether the matrix notation does indeed fit
# the correct model. Mainly to see whether the proposal with the
# eigen decomposition actually makes sense.

#---------------------
# Load necessary building blocks and data
#---------------------
source("buildingBlocks/train.tskrr.R")

# load some data

get.pl.data <- function(name){
  # loads one of the protein-ligand datasets
  Y <- read.table(paste('data/',name,'_admat_dgc.txt', sep=''),
                  sep='\t', header=T, row.names = 1)
  K <- read.table(paste('data/',name,'_simmat_dg.txt', sep=''),
                  sep='\t', header=T, row.names = 1)
  G <- read.table(paste('data/',name,'_simmat_dc.txt', sep=''),
                  sep='\t', header=T, row.names = 1)
  return(list(Y=as.matrix(Y), K=as.matrix(K), G=as.matrix(G)))
}

pl.data <- get.pl.data('nr')
Y <- pl.data$Y
K <- pl.data$K
G <- pl.data$G

# Naive calculation W (eq 2 p 6)
#---------------------

fit.naive <- function(Y,K,G, lambda.u, lambda.v){
  diag(K) <- diag(K) + lambda.u
  diag(G) <- diag(G) + lambda.v

  return(solve(K) %*% Y %*% solve(G))
}

fit.tskrr <- function(Y, K, G, lambda.u=1, lambda.v=1){
  # trains two-step kernel ridge regression by performing
  # eigenvalue decomposition of the two gram matrices
  eigen.decomp.K <- eigen(K, symmetric = TRUE)
  eigen.decomp.G <- eigen(G, symmetric = TRUE)
  GetParametersTSKRR(Y=Y, U=eigen.decomp.K$vectors, Sigma=eigen.decomp.K$values,
                          V=eigen.decomp.G$vectors, S=eigen.decomp.G$values,
                          lambda.u=lambda.u, lambda.v=lambda.v)
}


#---------------------
# Comparing results
#---------------------
Wnaive <- fit.naive(Y,K,G, 1e-7,1e-7)

Wtskrr <- fit.tskrr(Y,K,G,1e-7,1e-7)

hist(Wnaive - Wtskrr, breaks = 20)

#--------------------
# Show the problem
#--------------------
isSymmetric(G)
# Find values that don't fit
G[which(G != t(G), arr.ind = TRUE)]
# show one case
G[c(1,17), c(1,17)]

library(microbenchmark)

microbenchmark(
  NAIVE = fit.naive(Y,K,G, 1e-7,1e-7),
  TSKRR = fit.tskrr(Y,K,G,1e-7,1e-7)
)

#--------------------
# Do this on symmetric matrices.
set.seed(100)
Y <- matrix(sample(c(0,1),15, TRUE),ncol = 5, nrow = 3)

K <- matrix(0, ncol = 3, nrow = 3)
K[upper.tri(K)] <-  c(1,1,2)
K <- K + t(K)

G <- matrix(0, ncol = 5, nrow = 5)
G[upper.tri(G)] <-  c(1,1,1,1,2,2,2,3,3,4)
G <- G + t(G)

Wnaive <- fit.naive(Y,K,G, 0,0)

Wtskrr <- fit.tskrr(Y,K,G,0,0)

hist(Wnaive - Wtskrr)

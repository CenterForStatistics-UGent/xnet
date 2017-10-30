# functions for performing cross-validation for two-step kernel ridge regression

# SHORTCUTS FOR BIPARTITE NEWORKS

# cross validation shortcuts take as inputs:
#   - Y : label (adjacency) matrix
#   - Hk : hat matrix for rows
#   - Hg : hat matrix for columns

loo.I <- function(Y, Hk, Hg){
  F <- Hk %*% Y %*% Hg
  L <- diag(Hk) %*% t(diag(Hg))  # leverages
  return((F - Y * L) / (1 - L))
}

loo.I0 <- function(Y, Hk, Hg){
  F <- Hk %*% Y %*% Hg
  L <- diag(Hk) %*% t(diag(Hg))  # leverages
  return(F - Y * L)
}

loo.R <- function(Y, Hk, Hg){
  YHg <- Y %*% Hg
  YlooR <- ((Hk - diag(diag(Hk))) %*% YHg) / (1 - diag(Hk))
  return(YlooR)
}

loo.C <- function(Y, Hk, Hg){
  HkY <- Hk %*% Y
  return((HkY %*% (Hg - diag(diag(Hg)))) / (rep(1 - diag(Hg), each=dim(Y)[1])))
}

loo.B <- function(Y, Hk, Hg){
  FlooB <- (Hk - diag(diag(Hk))) %*% Y %*% (Hg - diag(diag(Hg))) / (diag(1 - Hk) %*% t(diag(1 - Hg)))
  return(FlooB)
}

# SHORTCUTS FOR HOMOGENEOUS NEWORKS

# cross validation shortcuts take as inputs:
#   - Y : label (adjacency) matrix
#   - Hk : hat matrix for rows AND columns

loo.E.sym <- function(Y, Hk){
  F <- Hk %*% Y %*% Hk
  L <- diag(Hk) %*% t(diag(Hk)) + Hk**2
  return((F - L * Y) / (1 - L))
}

loo.E.skewsym <- function(Y, Hk){
  F <- Hk %*% Y %*% Hk
  L <- diag(Hk) %*% t(diag(Hk)) - Hk**2
  return((F - L * Y) / (1 - L))
}

loo.E0.sym <- function(Y, Hk){
  F <- Hk %*% Y %*% Hk
  L <- diag(Hk) %*% t(diag(Hk)) + Hk**2
  return((F - L * Y))
}

loo.E0.skewsym <- function(Y, Hk){
  F <- Hk %*% Y %*% Hk
  L <- diag(Hk) %*% t(diag(Hk)) - Hk**2
  return((F - L * Y))
}

loo.V <- function(Y, Hk){
  Floo <- ((Hk - diag(diag(Hk))) %*% Y) / (1 - diag(Hk))
  FlooV <- Floo %*% Hk
  FlooV <- FlooV + Hk * ((diag(FlooV) - diag(Floo)) / (1 - diag(Hk)))
  return(FlooV)
}
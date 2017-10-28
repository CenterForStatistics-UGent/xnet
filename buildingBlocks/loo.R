# functions for performing cross-validation for two-step kernel ridge regression

# SHORTCUTS FOR BIPARTITE NEWORKS

# cross validation shortcuts take as inputs:
#   - Y : label (adjacency) matrix
#   - Hk : hat matrix for rows
#   - Hg : hat matrix for columns

loo.I <- function(Y, Hk, Hg){
  F <- Hk %*% Hg
  L <- diag(Hk) %*% t(diag(Hg))  # leverages
  return((F - Y * L) / (1 - L))
}

loo.I0 <- function(Y, Hk, Hg){
  F <- Hk %*% Hg
  L <- diag(Hk) %*% t(diag(Hg))  # leverages
  return(F - Y * L)
}

loo.R <- function(Y, Hk, Hg){
  YHgT <- t(Y %*% Hg)
  YlooRT <- (YHgT %*% Hk - YHgT * diag(Hk)) / (1 - diag(Hk))
  return(t(YlooRT))
}

loo.C <- function(Y, Hk, Hg){
  HkY <- Hk %*% Y
  return((HkY %*% Hg - HkY * diag(Hg)) / (1 - diag(Hg)))
}


loo.B <- function(Y, Hk, Hg){
  Floo <- (Hk - diag(diag(Hk))) %*% Y %*% (Hg - diag(diag(Hg)))
  Floo <- Floo / (1 - diag(Hg))
  return(t(t(Floo) / (1 - diag(Hk))))
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
  Floo <- t((t(Hk %*% Y) - t(Y) * diag(Hk)) / (1 - diag(Hk)))
  FlooV <- Floo %*% Hk
  FlooV <- FlooV + t(Hk * (diag(Floo) - diag(FlooV)) / (1 - diag(Hk)))
  return(FlooV)
}
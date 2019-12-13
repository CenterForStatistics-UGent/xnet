#' Leave-one-out cross-validation for two-step kernel ridge regression
#'
#' These functions implement different cross-validation scenarios for
#' two-step kernel ridge regression. It uses the shortcuts for
#' leave-one-out cross-validation.
#'
#' These functions are primarily for internal use and hence not exported.
#' Be careful when using them, as they do not perform any sanity check
#' on the input. It is up to the user to make sure the input makes sense.
#'
#' @seealso \code{\link{loo}} for the user-level function.
#'
#' @param Y the matrix with responses
#' @param Hk the hat matrix for the first kernel (rows of Y)
#' @param Hg the hat matrix for the second kernel (columns of Y)
#' @param alpha a vector of length 4 with the alpha values from a
#' \code{\link{linearFilter}} model
#' @param pred the predictions
#' @param ... added to allow for specifying pred even when not needed.
#'
#' @return a matrix with the leave-one-out predictions
#' @rdname looInternal
#' @name loo_internal
loo.i <- function(Y, Hk, Hg, pred){
  L <- tcrossprod(diag(Hk), diag(Hg))
  return((pred - Y * L) / (1 - L))
}

#' @rdname looInternal
loo.i0 <- function(Y, Hk, Hg, pred){
  L <- tcrossprod(diag(Hk), diag(Hg))
  return((pred - Y * L))
}

#' @rdname looInternal
loo.r <- function(Y, Hk, Hg, ...){
  div <- 1 - diag(Hk)
  diag(Hk) <- 0

  return( (Hk %*% Y %*% Hg) / div )
}

#' @rdname looInternal
loo.c <- function(Y, Hk, Hg, ...){
  div <- 1 - diag(Hg)
  diag(Hg) <- 0

  return( (Hk %*% Y %*% Hg) / rep(div, each = nrow(Y)) )
}

#' @rdname looInternal
loo.b <- function(Y, Hk, Hg, ...){
  divk <- 1 - diag(Hk)
  divg <- 1 - diag(Hg)

  diag(Hk) <- 0
  diag(Hg) <- 0

  pred <- Hk %*% Y %*% Hg
  div <- tcrossprod(divk, divg)

  return(pred / div)
}

####################################
## SHORTCUTS FOR HOMOGENOUS NETWORKS

#' @rdname looInternal
loo.e.sym <- function(Y, Hk, pred){

  L <- tcrossprod(diag(Hk)) + Hk^2
  return((pred - L * Y) / ( 1 - L))

}

#' @rdname looInternal
loo.e.skew <- function(Y, Hk, pred){

  L <- tcrossprod(diag(Hk)) - Hk^2
  return((pred - L * Y) / ( 1 - L))

}

#' @rdname looInternal
loo.e0.sym <- function(Y, Hk, pred){

  L <- tcrossprod(diag(Hk)) + Hk^2
  return( (pred - L * Y) )

}

#' @rdname looInternal
loo.e0.skew <- function(Y, Hk, pred){

  L <- tcrossprod(diag(Hk)) - Hk^2
  return( (pred - L * Y) )

}

#' @rdname looInternal
loo.v <- function(Y, Hk, ...){

  Hk0 <- Hk
  diag(Hk0) <- 0
  div <- 1 - diag(Hk)

  Floo <- Hk0 %*% Y / div
  FlooV <- Floo %*% Hk

  FlooV <- FlooV + Hk * ((diag(FlooV) - diag(Floo)) / div)
  return(FlooV)
}

####################################
## SHORTCUTS FOR LINEAR FILTERS

#' @rdname looInternal
loo.i.lf <- function(Y, alpha, pred){

  d <- dim(Y)
  n <- length(Y)

  lev <- alpha[1] + alpha[2] / d[1] + alpha[3] / d[2] + alpha[4] / n

  loolf <- (pred - Y*lev) / (1 - lev)
  return(loolf)
}

#' @rdname looInternal
loo.i0.lf <- function(Y, alpha, pred){

  d <- dim(Y)
  n <- length(Y)

  lev <- alpha[1] + alpha[2] / d[1] + alpha[3] / d[2] + alpha[4] / n

  loolf <- (pred - Y*lev)
  return(loolf)
}

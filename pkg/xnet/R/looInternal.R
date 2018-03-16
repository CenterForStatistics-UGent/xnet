#' Leave-one-out crossvalidation for two-step kernel ridge regression
#'
#' These functions implement different cross-validation scenarios for
#' two-step kernel ridge regression. It uses the shortcuts for
#' leave-one-out crossvalidation.
#'
#' These functions are primarily for internal use and hence not exported.
#' Be careful when using them, as they do not perform any sanity check
#' on the input. It's up to the user to make sure the input makes sense.
#'
#' @seealso \code{\link{loo}} for the user-level function.
#'
#' @param Y the matrix with responses
#' @param Hr the hat matrix for the first kernel (rows of Y)
#' @param Hc the hat matrix for the second kernel (columns of Y)
#' @param pred the predictions
#' @param ... added to allow for specifying pred even when not needed.
#'
#' @return a matrix with the leave-one-out predictions
#' @rdname looInternal
#' @name loo_internal
loo.i <- function(Y, Hr, Hc, pred){
  L <- tcrossprod(diag(Hr), diag(Hc))
  return((pred - Y * L) / (1 - L))
}

#' @rdname looInternal
loo.i0 <- function(Y, Hr, Hc, pred){
  L <- tcrossprod(diag(Hr), diag(Hc))
  return((pred - Y * L))
}

#' @rdname looInternal
loo.r <- function(Y, Hr, Hc, ...){
  div <- 1 - diag(Hr)
  diag(Hr) <- 0

  return( (Hr %*% Y %*% Hc) / div )
}

#' @rdname looInternal
loo.c <- function(Y, Hr, Hc, ...){
  div <- 1 - diag(Hc)
  diag(Hc) <- 0

  return( (Hr %*% Y %*% Hc) / rep(div, each = nrow(Y)) )
}

#' @rdname looInternal
loo.b <- function(Y, Hr, Hc, ...){
  divk <- 1 - diag(Hr)
  divg <- 1 - diag(Hc)

  diag(Hr) <- 0
  diag(Hc) <- 0

  pred <- Hr %*% Y %*% Hc
  div <- tcrossprod(divk, divg)

  return(pred / div)
}

####################################
## SHORTCUTS FOR HOMOGENOUS NETWORKS

#' @rdname looInternal
loo.e.sym <- function(Y, Hr, pred){

  L <- tcrossprod(diag(Hr)) + Hr^2
  return((pred - L * Y) / ( 1 - L))

}

#' @rdname looInternal
loo.e.skew <- function(Y, Hr, pred){

  L <- tcrossprod(diag(Hr)) - Hr^2
  return((pred - L * Y) / ( 1 - L))

}

#' @rdname looInternal
loo.e0.sym <- function(Y, Hr, pred){

  L <- tcrossprod(diag(Hr)) + Hr^2
  return( (pred - L * Y) )

}

#' @rdname looInternal
loo.e0.skew <- function(Y, Hr, pred){

  L <- tcrossprod(diag(Hr)) - Hr^2
  return( (pred - L * Y) )

}

#' @rdname looInternal
loo.v <- function(Y, Hr, ...){

  Hr0 <- Hr
  diag(Hr0) <- 0
  div <- 1 - diag(Hr)

  Floo <- Hr0 %*% Y / div
  FlooV <- Floo %*% Hr

  FlooV <- FlooV + Hr * ((diag(FlooV) - diag(Floo)) / div)
  return(FlooV)
}
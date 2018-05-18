#' predict method for tskrr fits
#'
#' Obtains predictions from a \code{\link{tskrr}} model for new
#' data. To get the predictions on the training data, use the
#' function \code{\link[xnet:fitted,tskrr-method]{fitted}}
#'
#' @param object an object of class \code{\link[xnet:tskrr-class]{tskrr}}.
#' @param K a new K matrix
#' @param G a new G matrix or \code{NULL}. If \code{NULL}, K is used
#' for both.
#'
#' This function is currently in testing and should be used
#' with caution. There's a chance I missed with the inversions and/or
#' transpose.
#'
#' @include all_generics.R
#' @rdname predict
#' @export
predict.tskrr <- function(object,
                          K,
                          G = NULL,
                          ...){
  if(is.null(G)) G <- K
  K %*% weights(object) %*% G
}

setMethod("predict",
          "tskrr",
          predict.tskrr)
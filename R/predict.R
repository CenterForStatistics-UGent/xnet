#' predict method for tskrr fits
#'
#' Obtains predictions from a \code{\link{tskrr}} model for new
#' data. To get the predictions on the training data, use the
#' function \code{\link[xnet:fitted]{fitted}} or set \code{K}
#' to \code{NULL}.
#'
#' @param object an object of class \code{\link[xnet:tskrr-class]{tskrr}}.
#' @param K a new K matrix or \code{NULL}. if \code{NULL}, the fitted
#' values on the training data are returned.
#' @param G a new G matrix or \code{NULL}. If \code{NULL}, K is used
#' for both.
#' @param ... arguments passed to or from other methods
#'
#' This function is currently in testing and should be used
#' with caution.
#'
#' @include all_generics.R
#' @rdname predict
#' @method predict tskrr
#' @export
predict.tskrr <- function(object,
                          K = NULL,
                          G = NULL,
                          ...){
  if(is.null(K))
    return(fitted(object))
  if(is.null(G)){
    if(is_symmetric(K)) G <- K else G <- t(K)
  }

  K %*% weights(object) %*% G
}

#' @rdname predict
#' @export
setMethod("predict",
          "tskrr",
          predict.tskrr)
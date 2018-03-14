#' Getters for tskrr objects
#'
#' The functions described here are convenience functions to get
#' information out of a \code{\link[xnet:tskrr-class]{tskrr}} object.
#'
#' @param x a \code{\link[xnet:tskrr-class]{tskrr}} object or an
#' object inheriting from \code{tskrr}.
#' @param ... arguments passed to other methods.
#'
#' @include all_generics.R
#' @rdname getters-tskrr
#' @aliases response
#' @export
#' @return For \code{response}: the original response matrix
setMethod("response",
          "tskrr",
          function(x, ...){
            x@y
          })

#' @rdname getters-tskrr
#' @return For \code{lambda}: a named numeric vector with one resp both lambda
#' values used in the model. The names are "k" and "g" respectively.
#' @export
setMethod("lambda",
          "tskrr",
          function(x){
            c(k = x@lambda.k)
          })

#' @rdname getters-tskrr
#' @aliases lambda
#' @export
setMethod("lambda",
          "tskrrHeterogenous",
          function(x){
            c(k = x@lambda.k, g = x@lambda.g)
          })

#' @rdname getters-tskrr
#' @aliases is_homogenous
is_homogenous <- function(x){
  if(!inherits(x, "tskrr")) stop("x should be a tskrr model.")
  inherits(x, "tskrrHomogenous")
}
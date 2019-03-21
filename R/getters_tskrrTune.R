#' Getters for tskrrTune objects
#'
#' The functions described here are convenience functions to get
#' information out of a \code{\link[xnet:tskrrTune-class]{tskrrTune}}
#' object.
#'
#' @param x a \code{\link[xnet:tskrrTune-class]{tskrrTune}} object or an
#' object inheriting from \code{tskrrTune}.
#' @param ... arguments passed to other methods.
#'
#' @include all_generics.R
#' @rdname getters-tskrrTune
#' @aliases is_tuned
#' @export
is_tuned <- function(x){
  if(!inherits(x, "tskrr")) stop("x should be a tskrr model.")
  inherits(x, "tskrrTune")
}

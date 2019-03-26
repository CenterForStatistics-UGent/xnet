#' Getters for tskrrImpute objects
#'
#' The functions described here are convenience functions to get
#' information out of a \code{\link[xnet:tskrrImpute-class]{tskrrImpute}}
#' object.
#'
#' @param x a \code{\link[xnet:tskrrImpute-class]{tskrrImpute}} object or
#' an object inheriting from \code{tskrrImpute}.
#' @param ... arguments passed to other methods/
#'
#' @return For \code{has_imputed_values}: a logical value indicating whether
#' the model has imputed values. If \code{x} is not some form of a
#' \code{\link{tskrr}} model, the function will return an error.
#'
#'
#' @examples
#'
#' data(drugtarget)
#'
#' mod <- tskrr(drugTargetInteraction, targetSim, drugSim)
#'
#' # NEED MORE EXAMPLES!
#'
#' @include all_generics.R
#' @rdname getters-tskrrImpute
#' @aliases has_imputed_values
#' @export
has_imputed_values <- function(x){
  if(!inherits(x, "tskrr")) stop("x should be a tskrr model.")
  inherits(x, "tskrrImpute")
}

#' @rdname getters-tskrrImpute
#' @return For \code{which_imputed}: a integer vector with the positions
#' for which the values are imputed.
#' @export
which_imputed <- function(x){
  if(!inherits(x, "tskrrImpute"))
    stop("x should be a tskrr model with imputed values.")
  x@imputeid
}

#' @rdname getters-tskrrImpute
#' @return for \code{is_imputed}: a matrix of the same dimensions as the
#' adjacency matrix. It contains the value \code{FALSE} at positions that
#' were not imputed, and \code{TRUE} at positions that were.
#' @export
is_imputed <- function(x){
  if(!inherits(x, "tskrrImpute"))
    stop("x should be a tskrr model with imputed values.")
  dims <- dim(x@y)
  out <- matrix(FALSE, nrow = dims[1], ncol = dims[2])
  out[x@imputeid] <- TRUE
  return(out)
}

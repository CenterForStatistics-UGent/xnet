#' Getters for linearFilter objects
#'
#' These functions allow you to extract slots from objects of the
#' class \code{\link{linearFilter}}
#'
#' @param x a \code{linearFilter} object
#' @param ... arguments passed to or from other methods.
#'
#' @return for \code{mean}: the mean of the original matrix
#'
#' @rdname getters_linearFilter
#' @name getters_linearFilter
#'
#' @method mean linearFilter
#' @export
mean.linearFilter <- function(x,...){
  x@mean
}

#' @rdname getters_linearFilter
#' @export
setMethod("mean", "linearFilter", mean.linearFilter)

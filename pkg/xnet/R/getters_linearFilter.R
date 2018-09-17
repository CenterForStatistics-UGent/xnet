#' Getters for linearFilter objects
#'
#' These functions allow you to extract slots from objects of the
#' class \code{\link{linearFilter}}
#'
#' @param x a \code{linearFilter} object
#'
#' @return for \code{\bold{mean}}: the mean of the original matrix
#' @rdname getters_linearFilter
#'
#' @method mean linearFilter
#' @export
mean.linearFilter <- function(x,...){
  x@mean
}

#' @rdname getters_linearFilter
#' @export
setMethod("mean", "linearFilter", mean.linearFilter)

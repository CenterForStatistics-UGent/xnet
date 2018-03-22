#' Class tskrrHeterogenous
#'
#' The class tskrrHeterogenous is a subclass of the superclass
#' \code{\link[xnet:tskrr-class]{tskrr}} class specifically for
#' heterogenous networks.
#'
#' @slot y the matrix with responses
#' @slot k the eigen decomposition of the kernel matrix for the rows
#' @slot lambda.k the lambda value used for k
#' @slot pred the matrix with the predictions
#' @slot g the eigen decomposition of the kernel matrix for the columns
#' @slot lambda.g the lambda value used for g
#' @slot has.orig a logical value indicating whether the original kernel
#' matrices are stored in the object.
#' @slot k.orig the original kernel matrix for the rows.
#' @slot g.orig the original kernel matrix for the columns.
#'
#' @include Class_tskrr.R
#' @rdname tskrrHeterogenous-class
#' @name tskrrHeterogenous-class
#' @aliases tskrrHeterogenous
#' @exportClass tskrrHeterogenous
setClass("tskrrHeterogenous",
         contains = "tskrr",
         slots = c(g = "eigen",
                   lambda.g = "numeric",
                   g.orig = "matrix"),
         prototype = list(lambda.g = 1e-4,
                          g = structure(list(vectors = matrix(0),
                                             values = numeric(1)),
                                        class = "eigen"),
                          g.orig = matrix(0)
                          )
         )

validTskrrHeterogenous <- function(object){
  if(length(object@lambda.g) != 1)
    return("lambda.g should be a single value")

  else if(object@has.orig && !valid_dimensions(object@y, object@k.orig, object@g.orig))
    return("The dimensions of the original kernel matrices and the observations don't match.")

  else
    return(TRUE)
}
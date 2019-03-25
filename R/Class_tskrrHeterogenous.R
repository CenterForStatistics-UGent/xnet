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
#' @slot has.hat a logical value indicating whether the kernel hat matrices
#' are stored in the object.
#' @slot Hk the kernel hat matrix for the rows.
#' @slot Hg the kernel hat matrix for the columns.
#' @slot labels a list with elements \code{k} and \code{g} (see
#' \code{\link{tskrr-class}}).
#'  If any element is \code{NA}, the labels used
#' are integers indicating the row resp column number.
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
                   Hg = "matrix"),
         prototype = list(lambda.g = 1e-4,
                          g = structure(list(vectors = matrix(0),
                                             values = numeric(1)),
                                        class = "eigen"),
                          Hg = matrix(0)
                          )
         )

validTskrrHeterogenous <- function(object){

  if(length(object@lambda.g) != 1)
    return("lambda.g should be a single value")

  else if(object@has.hat && !valid_dimensions(object@y, object@Hk, object@Hg))
    return("The dimensions of the original kernel matrices and the observations don't match.")

  else if(
    (length(object@labels$k) == 1 && !is.na(object@labels$k)) &&
    (length(object@labels$k) != nrow(object@y))
  )
    return("The labels element k should either be NA or a character vector with the same number of values as there are rows in the Y matrix.")

  else if(
    (length(object@labels$g) == 1 && !is.na(object@labels$g)) &&
    (length(object@labels$g) != ncol(object@y))
  )
    return("The labels element g should either be NA or a character vector with the same number of values as there are columns in the Y matrix.")

  else
    return(TRUE)
}

setValidity("tskrrHeterogenous", validTskrrHeterogenous)

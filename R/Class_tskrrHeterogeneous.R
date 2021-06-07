#' Class tskrrHeterogeneous
#'
#' The class tskrrHeterogeneous is a subclass of the superclass
#' \code{\link[xnet:tskrr-class]{tskrr}} specifically for
#' heterogeneous networks.
#'
#' @slot y an adjacency matrix.
#' @slot k an object of class \code{\link[=dataClasses]{gramData}}
#' @slot lambda.k the lambda value used for k
#' @slot pred the matrix with the predictions
#' @slot g an object of class \code{\link[=dataClasses]{gramData}}
#' @slot lambda.g the lambda value used for g
#' @slot has.hat a logical value indicating whether the kernel hat matrices
#' are stored in the object.
#' @slot Hk the kernel hat matrix for the rows.
#' @slot Hg the kernel hat matrix for the columns.
#'
#' @include Class_tskrr.R
#' @rdname tskrrHeterogeneous-class
#' @name tskrrHeterogeneous-class
#' @aliases tskrrHeterogeneous
#' @exportClass tskrrHeterogeneous
setClass("tskrrHeterogeneous",
         contains = "tskrr",
         slots = c(g = "gramData",
                   lambda.g = "numeric",
                   Hg = "matrix"),
         prototype = list(lambda.g = 1e-4,
                          Hg = matrix(0)
                          )
         )

validTskrrHeterogeneous <- function(object){

  if(length(object@lambda.g) != 1)
    return("lambda.g should be a single value")

  else if(object@has.hat && !valid_dimensions(object@y, object@Hk, object@Hg))
    return("The dimensions of the original kernel matrices and the observations don't match.")

  else
    return(TRUE)
}

setValidity("tskrrHeterogeneous", validTskrrHeterogeneous)

#' Class tskrrHomogenous
#'
#' The class tskrrHomogenous is a subclass of the superclass
#' \code{\link[xnet:tskrr-class]{tskrr}} class specifically for
#' homogenous networks.
#'
#' @slot y the matrix with responses
#' @slot k the eigen decomposition of the kernel matrix for the rows
#' @slot lambda.k the lambda value used for k
#' @slot pred the matrix with the predictions
#' @slot symmetric a character value that can have the possible values
#' \code{"symmetric"}, \code{"skewed"} or \code{"not"}. It indicates
#' whether the \code{y} matrix is symmetric, skewed-symmetric or not
#' symmetric.
#'
#' @include Class_tskrr.R
#' @rdname tskrrHomogenous-class
#' @name tskrrHomogenous-class
#' @aliases tskrrHomogenous
#' @exportClass tskrrHomogenous
setClass("tskrrHomogenous",
         contains = "tskrr",
         slots = c(symmetric = "character"),
         prototype = list(symmetric = "not")
         )

validTskrrHomogenous <- function(object){
  if(!object@symmetric %in% c("symmetric","skewed", "not"))
    return("symmetric should be one of: symmetric, skewed or not.")

  else
    return(TRUE)
}

setValidity("tskrrHomogenous",
            validTskrrHomogenous)
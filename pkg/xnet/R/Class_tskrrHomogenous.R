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
#' @slot symmetry a character value that can have the possible values
#' \code{"symmetric"}, \code{"skewed"} or \code{"not"}. It indicates
#' whether the \code{y} matrix is symmetric, skewed-symmetric or not
#' symmetric.
#' @slot has.orig a logical value indicating whether the original kernel
#' matrices are stored in the object.
#' @slot k.orig the original kernel matrix for the rows.
#'
#' @include Class_tskrr.R
#' @rdname tskrrHomogenous-class
#' @name tskrrHomogenous-class
#' @aliases tskrrHomogenous
#' @exportClass tskrrHomogenous
setClass("tskrrHomogenous",
         contains = "tskrr",
         slots = c(symmetry = "character"),
         prototype = list(symmetry = "not")
         )

validTskrrHomogenous <- function(object){
  if(!object@symmetry %in% c("symmetric","skewed", "not"))
    return("symmetry should be one of: symmetric, skewed or not.")

  else if(has.orig && !valid_dimensions(object@y, object@k.orig))
    return("The dimensions of the original kernel matrices and the observations don't match.")

  else
    return(TRUE)
}

setValidity("tskrrHomogenous",
            validTskrrHomogenous)
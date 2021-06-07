#' Class tskrrHomogeneous
#'
#' The class tskrrHomogeneous is a subclass of the superclass
#' \code{\link[xnet:tskrr-class]{tskrr}} specifically for
#' homogeneous networks.
#'
#' @slot y an adjacency matrix.
#' @slot k an object of class \code{\link[=dataClasses]{gramData}}
#' @slot lambda.k the lambda value used for k
#' @slot pred the matrix with the predictions
#' @slot symmetry a character value that can have the possible values
#' \code{"symmetric"}, \code{"skewed"} or \code{"not"}. It indicates
#' whether the \code{y} matrix is symmetric, skewed-symmetric or not
#' symmetric.
#' @slot has.hat a logical value indicating whether the kernel hat matrices
#' are stored in the object.
#' @slot Hk the kernel hat matrix for the rows.
#'
#' @include Class_tskrr.R
#' @rdname tskrrHomogeneous-class
#' @name tskrrHomogeneous-class
#' @aliases tskrrHomogeneous
#' @exportClass tskrrHomogeneous
setClass("tskrrHomogeneous",
         contains = "tskrr",
         slots = c(symmetry = "character"),
         prototype = list(symmetry = "not")
         )

validTskrrHomogeneous <- function(object){

  if(!object@symmetry %in% c("symmetric","skewed", "not"))
    return("symmetry should be one of: symmetric, skewed or not.")

  else if(object@has.hat && !valid_dimensions(object@y, object@Hk))
    return("The dimensions of the original kernel matrices and the observations don't match.")

   else
    return(TRUE)
}

setValidity("tskrrHomogeneous",
            validTskrrHomogeneous)

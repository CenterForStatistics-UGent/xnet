#' Getters for the gramData classes
#'
#' These functions allow you to get slots from the \code{\link[xnet:dataClasses]{gramData}} classes.
#'
#' @param x an object of any of those classes or classes that inherit from those classes.
#' @param ... arguments passed to other methods
#'
#' @return for \code{has_orig}: a logical value indicating whether the original data can be found in the object
#'
#' @rdname getters-data
#' @name getters-data
#' @aliases has_orig, has_orig,gramData-method
#' @export
setMethod("has_orig",
          "gramData",
          function(x){
            x@hasorig
          })

#' @rdname getters-data
#' @return for \code{get_orig}: the original data structure if present, \code{NULL} otherwise.
#' @aliases get_orig, get_orig,gramData-method
#' @export
setMethod("get_orig",
          "gramData",
          function(x){
            if(has_orig(x)){
              x@orig
            } else {
              warning("No original data in this object.")
              NULL
            }
          })

#' @rdname getters-data
#' @return for \code{get_eigen}: the object of class \code{\link[base]{eigen}} with the eigendecomposition.
#' @export
setMethod("get_eigen",
          "gramData",
          function(x){
            x@eigen
          })

#' @rdname getters-data
#' @return for \code{has_gram}: a logical value indicating whether the original gram matrix / kernel matrix is stored in the object.
#' @export
setMethod("has_gram",
          "gramData",
          function(x){
            x@hasgram
          })

#' @rdname getters-data
#' @return for \code{get_gram}: the original gram matrix / kernel matrix if present, otherwise \code{NULL}.
#' @export
setMethod("get_gram",
          "gramData",
          function(x){
            if(has_gram(x))
              x@gram
            else
              NULL
          })

#' @rdname getters-tskrr
#' @export
setMethod("get_kernelmatrix",
          "gramData",
          function(x){
            if(has_gram(x)){
              x@gram
            } else {
              tmp <- get_eigen(x)
              eigen2matrix(tmp$vectors, tmp$values)
            }

          })
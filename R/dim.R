#' Get the dimensions of an object
#'
#' These functions allow you to extract the dimensions of a tskrr object or an object of one of the data classes.
#'
#' @param x a \code{\link[=tskrr-class]{tskrr}} or \code{\link[=dataClasses]{gramData}} object.
#'
#' @return a vector with two values indicating the number of rows
#' and the number of columns, or in case of a \code{gramData} object, a single number indicating the number of eigenvalues. This is equal to the number of rows/columns in the original gram matrix.
#'
#' @examples
#' data(drugtarget)
#' mod <- tskrr(drugTargetInteraction, targetSim, drugSim)
#' dim(mod)
#' nrow(mod)
#' ncol(mod)
#'
#' @rdname dim_tskrr
#' @name dim_tskrr
#' @aliases dim.tskrr
#' @aliases dim,tskrr-method
#' @export
setMethod("dim",
          "tskrr",
          function(x){
            dim(x@y)
          })

#' @rdname dim_tskrr
#' @export
setMethod("dim",
          "gramData",
          function(x){
            length(x@eigen$values)
          })

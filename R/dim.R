#' Get the dimensions of a tskrr object
#'
#' These functions allow you to extract the dimensions of a tskrr
#' object. These dimensions are essentially the dimensions of the
#' label matrix y.
#'
#' @param x a \code{\link[=tskrr-class]{tskrr}} object.
#'
#' @return a vector with two values indicating the number of rows
#' and the number of columns.
#'
#' @examples
#' data(drugtarget)
#' mod <- tskrr(drugTargetInteraction, targetSim, drugSim)
#' dim(mod)
#' nrow(mod)
#' ncol(mod)
#'
#' @aliases dim.tskrr
#' @export
setMethod("dim",
          "tskrr",
          function(x){
            dim(x@y)
          })

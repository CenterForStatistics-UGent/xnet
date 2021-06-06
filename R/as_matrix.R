#' Convert data objects to matrix
#'
#' These functions let you convert \code{\link[xnet:dataClasses]{data classes}} to a matrix.
#'
#' @inheritParams base::as.matrix
#'
#' @return a matrix
#'
#' @rdname as_matrix
#' @name as.matrix
#' @aliases as.matrix, adjacencyData-method
#' @export
setMethod("as.matrix",
          "adjacencyData",
          function(x, ...){
            as(x, "matrix")
          })

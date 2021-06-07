#' Functions to check dimension
#'
#' These functions allow you to check whether the dimensions of the
#' adjacency matrix and the \code{\link{gramData}} objects are compatible.
#' \code{valid_dimensions} checks whether y has as many rows as k and whether y has as many columns as g.
#' \code{is_square} checks whether both dimensions are the same.
#'
#' @param y a matrix
#' @param k a gramData object or matrix
#' @param g an optional second gramData object or matrix, or \code{NULL} otherwise.
#'
#' @return a logical value indicating whether the dimensions of the
#' objects are compatible for a two step kernel ridge regression.
#'
#' @note The function \code{is_square} is not exported
#'
#' @seealso \code{\link{gramData}} for conversions.
#'
#' @rdname valid_dimensions
#' @export
valid_dimensions <- function(y, k, g = NULL){
  if(!inherits(y, "matrix"))
    stop("y needs to be a matrix.")

  if(inherits(k, "gramData")){
    dk <- dim(k)
  } else if(inherits(k, "matrix")){
    if(!is_square(k))
      stop("k needs to be a square matrix or gramData object")
    dk <- dim(k)[1]
  } else {
    stop("k needs to be a square matrix or gramData object.")
  }

  ydim <- dim(y)
  out <-  ydim[1L] == dk

  if(!is.null(g)){
    if(inherits(g, "gramData")){
      dg <- dim(g)
    } else if(inherits(g, "matrix")){
      if(!is_square(g))
        stop("g needs to be a square matrix or gramData object.")
      dg <- dim(g)[1L]
    } else {
      stop("g needs to be a square matrix or gramData object")
    }
    out <- out && ydim[2L] == dg
  } else {
    out <- out && is_square(y)
  }

  return(out)
}

#' @param x any matrix
#' @rdname valid_dimensions
#' @aliases is_square
is_square <- function(x){
  dims <- dim(x)
  dims[2L] == dims[1L]
}

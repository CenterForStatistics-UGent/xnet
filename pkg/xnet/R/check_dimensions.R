#' Functions to check matrices
#'
#' This functions allow you to check whether the dimensions of the
#' response matrix and the kernel matrix (matrices) are compatible.
#' \code{check_dimensions} checks whether both k and g are square matrices,
#' whether y has as many rows as k and whether y has as many columns as g.
#' \code{is_square} just checks whether both dimensions are the same.
#'
#' @param y a response matrix
#' @param k a kernel matrix
#' @param g an optional second kernel matrix or \code{NULL} otherwise.
#'
#' @return a logical value indicating whether the dimensions of the
#' matrices are compatible for a two step kernel ridge regression.
#'
#' @note The function \code{is_square} is not exported
#'
#' @rdname check_dimensions
#' @export
check_dimensions <- function(y, k, g = NULL){

  ydim <- dim(y)
  out <- is_square(k) && ydim[1] == dim(k)[2]

  if(!is.null(g)){
    out <- out && is_square(g) && ydim[2] == dim(g)[1]
  }

  return(out)
}

#' @param x any matrix
#' @rdname check_dimensions
#' @aliases is_square
is_square <- function(x){
  if(!is.matrix) stop("x should be a matrix.")
  dims <- dim(x)
  dims[2] == dims[1]
}


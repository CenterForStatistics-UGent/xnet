#' Functions to check matrices
#'
#' These functions allow you to check whether the dimensions of the
#' label matrix and the kernel matrix (matrices) are compatible.
#' \code{valid_dimensions} checks whether both k and g are square matrices,
#' whether y has as many rows as k and whether y has as many columns as g.
#' \code{is_square} checks whether both dimensions are the same.
#'
#' @param y a label matrix
#' @param k a kernel matrix
#' @param g an optional second kernel matrix or \code{NULL} otherwise.
#'
#' @return a logical value indicating whether the dimensions of the
#' matrices are compatible for a two step kernel ridge regression.
#'
#' @note The function \code{is_square} is not exported
#'
#' @rdname valid_dimensions
#' @export
valid_dimensions <- function(y, k, g = NULL){

  ydim <- dim(y)
  out <- is_square(k) && ydim[1L] == dim(k)[2L]

  if(!is.null(g)){
    out <- out && is_square(g) && ydim[2L] == dim(g)[1L]
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

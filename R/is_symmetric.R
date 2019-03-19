#' Test symmetry of a matrix
#'
#' The function \code{\link[base]{isSymmetric}} tests symmetry but also
#' takes row and column names into account. This function is a toned
#' down (and slightly faster) version that ignores row and column names.
#' Currently the function only works for real matrices, not complex ones.
#'
#' @param x a matrix to be tested.
#' @param tol the tolerance for comparing the numbers.
#'
#' @return a logical value indicating whether or not the matrix is
#' symmetric
#'
#' @examples
#' x <- matrix(1:16,ncol = 4)
#' is_symmetric(x)
#'
#' x <- x %*% t(x)
#' is_symmetric(x)
#'
#' @export
is_symmetric <- function(x, tol = 100 * .Machine$double.eps){

  if(!is.numeric(x) || !is.matrix(x))
    stop("x should be a numeric matrix")

  dims <- dim(x)

  if((n <- dims[1L]) != dims[2L])
    return(FALSE)
  else if(n == 1L)
    return(TRUE)

  # fast first testing to check if the first column and row match
  if(any(x[1,] - x[,1] > tol))
    return(FALSE)

  rd <- .row(dims - 1L) + 1
  cd <- .col(dims - 1L) + 1
  tohave <- rd > cd

  idr <- rd[tohave]
  idc <- cd[tohave]

  all(x[cbind(idr,idc)] - x[cbind(idc,idr)] < tol)

}
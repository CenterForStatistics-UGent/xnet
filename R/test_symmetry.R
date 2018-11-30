#' test the symmetry of a matrix
#'
#' This function tells you whether a matrix is symmetric,
#' skewed symmetric or not symmetric. It's used by \code{\link{tskrr}}
#' to determine which kind of homologous network is represented by
#' the adjacency matrix.
#'
#' @param x a matrix
#'
#' @return a character value with the possible values "symmetric",
#' "skewed" or "none".
#'
#' @seealso \code{\link{tskrrHomogenous}} for
#' more information on the values for the slot \code{symmetry}
#'
#' @examples
#' mat1 <- matrix(c(1,0,0,1),ncol = 2)
#' test_symmetry(mat1)
#' mat2 <- matrix(c(1,0,0,-1), ncol = 2)
#' test_symmetry(mat2)
#' mat3 <- matrix(1:4, ncol = 2)
#' test_symmetry(mat3)
#'
#' @export
test_symmetry <- function(x){
  if(!is.matrix(x))
    stop("x should be a matrix")
  idl <- lower.tri(x)
  tx <- t(x)

  if(all(x[idl] == tx[idl])){
    out <- "symmetric"
  } else if(all(x[idl] == -tx[idl])){
    out <- "skewed"
  } else {
    out <- "none"
  }
  return(out)
}
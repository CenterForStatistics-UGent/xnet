#' Reconstruct from eigen
#'
#' This function "reconstructs" from the eigen decomposition.
#'
#' @param vectors a matrix with eigenvectors
#' @param values a vector with eigenvalues
#'
#' @return a matrix
#'
#' @section Warning:
#' This is initial test code. It is currently NOT guaranteed to give
#' meaningful results.
#'
#' @note This function is not exported.
#'
ReconstructFromEigen <- function(vectors, values){
  V2 <- vectors * rep(values,each = nrow(vectors))
  return(tcrossprod(V2, vectors))
}
